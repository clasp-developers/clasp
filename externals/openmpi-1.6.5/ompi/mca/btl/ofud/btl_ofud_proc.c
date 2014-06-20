/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/runtime/ompi_module_exchange.h"

#include "btl_ofud.h"
#include "btl_ofud_proc.h"


static void mca_btl_ud_proc_construct(mca_btl_ud_proc_t* proc);
static void mca_btl_ud_proc_destruct(mca_btl_ud_proc_t* proc);

OBJ_CLASS_INSTANCE(mca_btl_ud_proc_t,
        opal_list_item_t, mca_btl_ud_proc_construct,
        mca_btl_ud_proc_destruct);

void mca_btl_ud_proc_construct(mca_btl_ud_proc_t* ud_proc)
{
    ud_proc->proc_ompi = 0;
    ud_proc->proc_addr_count = 0;
    ud_proc->proc_endpoints = 0;
    ud_proc->proc_endpoint_count = 0;
    OBJ_CONSTRUCT(&ud_proc->proc_lock, opal_mutex_t);

    /* add to list of all proc instance */
    OPAL_THREAD_LOCK(&mca_btl_ofud_component.ud_lock);
    opal_list_append(&mca_btl_ofud_component.ud_procs, &ud_proc->super);
    OPAL_THREAD_UNLOCK(&mca_btl_ofud_component.ud_lock);
}

void mca_btl_ud_proc_destruct(mca_btl_ud_proc_t* ud_proc)
{
    /* remove from list of all proc instances */
    OPAL_THREAD_LOCK(&mca_btl_ofud_component.ud_lock);
    opal_list_remove_item(&mca_btl_ofud_component.ud_procs, &ud_proc->super);
    OPAL_THREAD_UNLOCK(&mca_btl_ofud_component.ud_lock);

    /* release resources */
    if(NULL != ud_proc->proc_endpoints) {
        free(ud_proc->proc_endpoints);
    }
    OBJ_DESTRUCT(&ud_proc->proc_lock);
}


/*
 * Look for an existing IB process instance based on the associated
 * ompi_proc_t instance.
 */

mca_btl_ud_proc_t* mca_btl_ud_proc_lookup_ompi(ompi_proc_t* ompi_proc)
{
    mca_btl_ud_proc_t* ib_proc;

    OPAL_THREAD_LOCK(&mca_btl_ofud_component.ud_lock);

    for(ib_proc = (mca_btl_ud_proc_t*)
                    opal_list_get_first(&mca_btl_ofud_component.ud_procs);
            ib_proc != (mca_btl_ud_proc_t*)
                    opal_list_get_end(&mca_btl_ofud_component.ud_procs);
            ib_proc  = (mca_btl_ud_proc_t*)opal_list_get_next(ib_proc)) {
        if(ib_proc->proc_ompi == ompi_proc) {
            OPAL_THREAD_UNLOCK(&mca_btl_ofud_component.ud_lock);
            return ib_proc;
        }
    }
    OPAL_THREAD_UNLOCK(&mca_btl_ofud_component.ud_lock);
    return NULL;
}


/*
 * Create a IB process structure. There is a one-to-one correspondence
 * between a ompi_proc_t and a mca_btl_ud_proc_t instance. We cache
 * additional data (specifically the list of mca_btl_ud_endpoint_t instances,
 * and published addresses) associated w/ a given destination on this
 * datastructure.
 */

mca_btl_ud_proc_t* mca_btl_ud_proc_create(ompi_proc_t* ompi_proc)
{
    mca_btl_ud_proc_t* module_proc = NULL;
    size_t size;
    int rc;

    /* Check if we have already created a IB proc
     * structure for this ompi process */
    module_proc = mca_btl_ud_proc_lookup_ompi(ompi_proc);

    if(module_proc != NULL) {
        /* Gotcha! */
        return module_proc;
    }

    /* Oops! First time, gotta create a new IB proc out of the ompi_proc ... */
    module_proc = OBJ_NEW(mca_btl_ud_proc_t);
    /* Initialize number of peer */
    module_proc->proc_endpoint_count = 0;
    module_proc->proc_ompi = ompi_proc;

    /* build a unique identifier (of arbitrary size) to represent the proc */
    module_proc->proc_guid = ompi_proc->proc_name;


    /* query for the peer address info */
    rc = ompi_modex_recv(&mca_btl_ofud_component.super.btl_version,
                                 ompi_proc, (void*)&module_proc->proc_addrs,
                                 &size);

    if(OMPI_SUCCESS != rc) {
        opal_output(0,
                "[%s:%d] ompi_modex_recv failed for peer %s",
                __FILE__,__LINE__,ORTE_NAME_PRINT(&ompi_proc->proc_name));
        OBJ_RELEASE(module_proc);
        return NULL;
    }

    if((size % sizeof(mca_btl_ud_addr_t)) != 0) {
        opal_output(0, "[%s:%d] invalid module address for peer %s",
                __FILE__,__LINE__,ORTE_NAME_PRINT(&ompi_proc->proc_name));
        OBJ_RELEASE(module_proc);
        return NULL;
    }


    module_proc->proc_addr_count = size / sizeof(mca_btl_ud_addr_t);


    if (0 == module_proc->proc_addr_count) {
        module_proc->proc_endpoints = NULL;
    } else {
        module_proc->proc_endpoints = (mca_btl_base_endpoint_t**)
            malloc(module_proc->proc_addr_count *
                    sizeof(mca_btl_base_endpoint_t*));
    }

    if(NULL == module_proc->proc_endpoints) {
        OBJ_RELEASE(module_proc);
        return NULL;
    }
    return module_proc;
}


/*
 * Insert an endpoint into the proc array and assign it an address.
 *
 * MUST be called with the proc lock held!
 */

int mca_btl_ud_proc_insert(mca_btl_ud_proc_t* module_proc,
        mca_btl_base_endpoint_t* module_endpoint)
{
    module_endpoint->rem_addr =
            module_proc->proc_addrs[module_proc->proc_endpoint_count];
    module_proc->proc_endpoints[module_proc->proc_endpoint_count++] =
            module_endpoint;
    return OMPI_SUCCESS;
}


/*
 * Remove an endpoint from the proc array.
 */

int mca_btl_ud_proc_remove(mca_btl_ud_proc_t* proc,
        mca_btl_base_endpoint_t* endpoint)
{
    size_t i;

    OPAL_THREAD_LOCK(&proc->proc_lock);
    for(i = 0; i < proc->proc_endpoint_count; i++) {
        if(proc->proc_endpoints[i] == endpoint) {
            memmove(proc->proc_endpoints + i, proc->proc_endpoints + i + 1,
                    (proc->proc_endpoint_count -i - 1) *
                            sizeof(mca_btl_base_endpoint_t*));
            if(--proc->proc_endpoint_count == 0) {
                OPAL_THREAD_UNLOCK(&proc->proc_lock);
                OBJ_RELEASE(proc);
                return OMPI_SUCCESS;
            }

            break;
        }
    }

    OPAL_THREAD_UNLOCK(&proc->proc_lock);
    return OMPI_SUCCESS;
}

