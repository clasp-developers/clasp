/*
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/runtime/ompi_module_exchange.h"
#include "btl_elan.h"
#include "btl_elan_proc.h"

static void mca_btl_elan_proc_construct(mca_btl_elan_proc_t* proc);
static void mca_btl_elan_proc_destruct(mca_btl_elan_proc_t* proc);

OBJ_CLASS_INSTANCE(mca_btl_elan_proc_t, 
                   opal_list_item_t,mca_btl_elan_proc_construct, 
                   mca_btl_elan_proc_destruct);

void mca_btl_elan_proc_construct(mca_btl_elan_proc_t* elan_proc)
{
    elan_proc->proc_ompi = 0;
    elan_proc->proc_rail_count = 0;
    elan_proc->proc_endpoints = 0;
    elan_proc->proc_endpoint_count = 0;
    OBJ_CONSTRUCT(&elan_proc->proc_lock, opal_mutex_t);
    /* add to list of all proc instance */
    OPAL_THREAD_LOCK(&mca_btl_elan_component.elan_lock);
    opal_list_append(&mca_btl_elan_component.elan_procs, &elan_proc->super);
    OPAL_THREAD_UNLOCK(&mca_btl_elan_component.elan_lock);
}

/*
 * Cleanup elan proc instance
 */

void mca_btl_elan_proc_destruct(mca_btl_elan_proc_t* elan_proc)
{
    /* remove from list of all proc instances */
    OPAL_THREAD_LOCK(&mca_btl_elan_component.elan_lock);
    opal_list_remove_item(&mca_btl_elan_component.elan_procs, &elan_proc->super);
    OPAL_THREAD_UNLOCK(&mca_btl_elan_component.elan_lock);
    /* release resources */
    if(NULL != elan_proc->proc_endpoints) {
        free(elan_proc->proc_endpoints);
    }
    OBJ_DESTRUCT(&elan_proc->proc_lock);
}

/*
 * Look for an existing Elan process instances based on the associated
 * ompi_proc_t instance.
 */
static mca_btl_elan_proc_t* mca_btl_elan_proc_lookup_ompi(ompi_proc_t* ompi_proc)
{
    mca_btl_elan_proc_t* elan_proc;
    OPAL_THREAD_LOCK(&mca_btl_elan_component.elan_lock);
    for(elan_proc = (mca_btl_elan_proc_t*)
            opal_list_get_first(&mca_btl_elan_component.elan_procs);
        elan_proc != (mca_btl_elan_proc_t*)
            opal_list_get_end(&mca_btl_elan_component.elan_procs);
        elan_proc  = (mca_btl_elan_proc_t*)opal_list_get_next(elan_proc)) {
        if(elan_proc->proc_ompi == ompi_proc) {
            OPAL_THREAD_UNLOCK(&mca_btl_elan_component.elan_lock);
            return elan_proc;
        }
    }
    OPAL_THREAD_UNLOCK(&mca_btl_elan_component.elan_lock);
    return NULL;
}

/*
 * Create a ELAN process structure. There is a one-to-one correspondence
 * between a ompi_proc_t and a mca_btl_elan_proc_t instance. We cache
 * additional data (specifically the list of mca_btl_elan_endpoint_t instances, 
 * and published addresses) associated w/ a given destination on this
 * datastructure.
 */

mca_btl_elan_proc_t* mca_btl_elan_proc_create(ompi_proc_t* ompi_proc)
{
    int rc;
    size_t size;
    mca_btl_elan_proc_t* module_proc = NULL;
    /* Check if we have already created a Elan proc
     * structure for this ompi process */
    module_proc = mca_btl_elan_proc_lookup_ompi(ompi_proc);
    if(module_proc != NULL) {
        /* Gotcha! */
        return module_proc;
    }
    /* Oops! First time, gotta create a new Elan proc
     * out of the ompi_proc ... */
    module_proc = OBJ_NEW(mca_btl_elan_proc_t);
    if(NULL == module_proc)
        return NULL;
    /* Initialize number of peer */
    module_proc->proc_endpoint_count = 0;
    module_proc->proc_ompi = ompi_proc;

    /* build a unique identifier (of arbitrary
     * size) to represent the proc */
    module_proc->proc_guid = ompi_proc->proc_name;
    rc = ompi_modex_recv( &mca_btl_elan_component.super.btl_version,
                          ompi_proc,
                          (void**)&module_proc->position_id_array,
                          &size );
    if(rc != OMPI_SUCCESS) {
        BTL_ERROR(("mca_base_modex_recv: failed with return value=%d", rc));
        OBJ_RELEASE(module_proc);
        return NULL;
    }
    module_proc->proc_rail_count = size / sizeof(unsigned int);;
    /* XXX: Right now, there can be only 1 peer associated
     * with a proc. Needs a little bit change in 
     * mca_btl_elan_proc_t to allow on demand increasing of
     * number of endpoints for this proc 
     */

    module_proc->proc_endpoints = (mca_btl_base_endpoint_t**)
        malloc((1+module_proc->proc_rail_count )* sizeof(mca_btl_base_endpoint_t*));
    if(NULL == module_proc->proc_endpoints) {
        OBJ_RELEASE(module_proc);
        return NULL;
    }
    return module_proc;
}


/*
 * Note that this routine must be called with the lock on the process
 * already held.  Insert a btl instance into the proc array and assign 
 * it an address.
 */
int mca_btl_elan_proc_insert( mca_btl_elan_proc_t* module_proc, 
                              mca_btl_base_endpoint_t* module_endpoint )
{
    /* insert into endpoint array */
    module_proc->proc_endpoints[module_proc->proc_endpoint_count++] = module_endpoint;

    module_endpoint->endpoint_proc = module_proc;
    module_endpoint->elan_vp = module_proc->proc_ompi->proc_name.vpid;

    return OMPI_SUCCESS;
}

