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
 * Copyright (c) 2007-2008 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/util/arch.h"

#include "ompi/runtime/ompi_module_exchange.h"

#include "btl_openib.h"
#include "btl_openib_proc.h"
#include "connect/base.h"
#include "connect/connect.h"

static void mca_btl_openib_proc_construct(mca_btl_openib_proc_t* proc);
static void mca_btl_openib_proc_destruct(mca_btl_openib_proc_t* proc);

OBJ_CLASS_INSTANCE(mca_btl_openib_proc_t,
        opal_list_item_t, mca_btl_openib_proc_construct,
        mca_btl_openib_proc_destruct);

void mca_btl_openib_proc_construct(mca_btl_openib_proc_t* ib_proc)
{
    ib_proc->proc_ompi = 0;
    ib_proc->proc_ports = NULL;
    ib_proc->proc_port_count = 0;
    ib_proc->proc_endpoints = 0;
    ib_proc->proc_endpoint_count = 0;
    OBJ_CONSTRUCT(&ib_proc->proc_lock, opal_mutex_t);
    /* add to list of all proc instance */
    OPAL_THREAD_LOCK(&mca_btl_openib_component.ib_lock);
    opal_list_append(&mca_btl_openib_component.ib_procs, &ib_proc->super);
    OPAL_THREAD_UNLOCK(&mca_btl_openib_component.ib_lock);
}

/*
 * Cleanup ib proc instance
 */

void mca_btl_openib_proc_destruct(mca_btl_openib_proc_t* ib_proc)
{
    /* remove from list of all proc instances */
    OPAL_THREAD_LOCK(&mca_btl_openib_component.ib_lock);
    opal_list_remove_item(&mca_btl_openib_component.ib_procs, &ib_proc->super);
    OPAL_THREAD_UNLOCK(&mca_btl_openib_component.ib_lock);

    /* release resources */
    if(NULL != ib_proc->proc_endpoints) {
        free(ib_proc->proc_endpoints);
    }
    if (NULL != ib_proc->proc_ports) {
        int i, j;
        for (i = 0; i < ib_proc->proc_port_count; ++i) {
            for (j = 0; j < ib_proc->proc_ports[i].pm_cpc_data_count; ++j) {
                if (NULL != ib_proc->proc_ports[i].pm_cpc_data[j].cbm_modex_message) {
                    free(ib_proc->proc_ports[i].pm_cpc_data[j].cbm_modex_message);
                }
            }
        }
        free(ib_proc->proc_ports);
    }
    OBJ_DESTRUCT(&ib_proc->proc_lock);
}


/*
 * Look for an existing IB process instances based on the associated
 * ompi_proc_t instance.
 */
static mca_btl_openib_proc_t* mca_btl_openib_proc_lookup_ompi(ompi_proc_t* ompi_proc)
{
    mca_btl_openib_proc_t* ib_proc;

    OPAL_THREAD_LOCK(&mca_btl_openib_component.ib_lock);

    for(ib_proc = (mca_btl_openib_proc_t*)
            opal_list_get_first(&mca_btl_openib_component.ib_procs);
            ib_proc != (mca_btl_openib_proc_t*)
            opal_list_get_end(&mca_btl_openib_component.ib_procs);
            ib_proc  = (mca_btl_openib_proc_t*)opal_list_get_next(ib_proc)) {
        if(ib_proc->proc_ompi == ompi_proc) {
            OPAL_THREAD_UNLOCK(&mca_btl_openib_component.ib_lock);
            return ib_proc;
        }
    }
    OPAL_THREAD_UNLOCK(&mca_btl_openib_component.ib_lock);
    return NULL;
}

static void inline unpack8(char **src, uint8_t *value)
{
    /* Copy one character */
    *value = (uint8_t) **src;
    /* Most the src ahead one */
    ++*src;
}

/*
 * Create a IB process structure. There is a one-to-one correspondence
 * between a ompi_proc_t and a mca_btl_openib_proc_t instance. We
 * cache additional data (specifically the list of
 * mca_btl_openib_endpoint_t instances, and published addresses)
 * associated w/ a given destination on this datastructure.
 */

mca_btl_openib_proc_t* mca_btl_openib_proc_create(ompi_proc_t* ompi_proc)
{
    mca_btl_openib_proc_t* module_proc = NULL;
    size_t msg_size;
    uint32_t size;
    int rc, i, j;
    void *message;
    char *offset;
    int modex_message_size;
    mca_btl_openib_modex_message_t dummy;

    /* Check if we have already created a IB proc
     * structure for this ompi process */
    module_proc = mca_btl_openib_proc_lookup_ompi(ompi_proc);
    if (NULL != module_proc) {
        /* Gotcha! */
        return module_proc;
    }

    /* Oops! First time, gotta create a new IB proc
     * out of the ompi_proc ... */
    module_proc = OBJ_NEW(mca_btl_openib_proc_t);
    /* Initialize number of peer */
    module_proc->proc_endpoint_count = 0;
    module_proc->proc_ompi = ompi_proc;

    /* build a unique identifier (of arbitrary
     * size) to represent the proc */
    module_proc->proc_guid = ompi_proc->proc_name;

    /* query for the peer address info */
    rc = ompi_modex_recv(&mca_btl_openib_component.super.btl_version,
                         ompi_proc,
                         &message,
                         &msg_size);
    if (OMPI_SUCCESS != rc) {
        BTL_ERROR(("[%s:%d] ompi_modex_recv failed for peer %s",
                   __FILE__, __LINE__,
                   ORTE_NAME_PRINT(&ompi_proc->proc_name)));
        OBJ_RELEASE(module_proc);
        return NULL;
    }
    if (0 == msg_size) {
        return NULL;
    }

    /* Message was packed in btl_openib_component.c; the format is
       listed in a comment in that file */
    modex_message_size = ((char *) &(dummy.end)) - ((char*) &dummy);

    /* Unpack the number of modules in the message */
    offset = message;
    unpack8(&offset, &(module_proc->proc_port_count));
    BTL_VERBOSE(("unpack: %d btls", module_proc->proc_port_count));
    if (module_proc->proc_port_count > 0) {
        module_proc->proc_ports = (mca_btl_openib_proc_modex_t *)
            malloc(sizeof(mca_btl_openib_proc_modex_t) * 
                   module_proc->proc_port_count);
    } else {
        module_proc->proc_ports = NULL;
    }

    /* Loop over unpacking all the ports */
    for (i = 0; i < module_proc->proc_port_count; i++) {

        /* Unpack the modex comment message struct */
        size = modex_message_size;
        memcpy(&(module_proc->proc_ports[i].pm_port_info), offset, size);
#if !defined(WORDS_BIGENDIAN) && OPAL_ENABLE_HETEROGENEOUS_SUPPORT
        MCA_BTL_OPENIB_MODEX_MSG_NTOH(module_proc->proc_ports[i].pm_port_info);
#endif
        offset += size;
        BTL_VERBOSE(("unpacked btl %d: modex message, offset now %d",
                     i, (int)(offset-((char*)message))));

        /* Unpack the number of CPCs that follow */
        unpack8(&offset, &(module_proc->proc_ports[i].pm_cpc_data_count));
        BTL_VERBOSE(("unpacked btl %d: number of cpcs to follow %d (offset now %d)",
                     i, module_proc->proc_ports[i].pm_cpc_data_count, 
                     (int)(offset-((char*)message))));
        module_proc->proc_ports[i].pm_cpc_data = 
            calloc(module_proc->proc_ports[i].pm_cpc_data_count,
                   sizeof(ompi_btl_openib_connect_base_module_data_t));
        if (NULL == module_proc->proc_ports[i].pm_cpc_data) {
            return NULL;
        }

        /* Unpack the CPCs */
        for (j = 0; j < module_proc->proc_ports[i].pm_cpc_data_count; ++j) {
            uint8_t u8;
            ompi_btl_openib_connect_base_module_data_t *cpcd;
            cpcd = module_proc->proc_ports[i].pm_cpc_data + j;
            unpack8(&offset, &u8);
            BTL_VERBOSE(("unpacked btl %d: cpc %d: index %d (offset now %d)",
                         i, j, u8, (int)(offset-(char*)message)));
            cpcd->cbm_component = 
                ompi_btl_openib_connect_base_get_cpc_byindex(u8);
            BTL_VERBOSE(("unpacked btl %d: cpc %d: component %s",
                         i, j, cpcd->cbm_component->cbc_name));
            
            unpack8(&offset, &cpcd->cbm_priority);
            unpack8(&offset, &cpcd->cbm_modex_message_len);
            BTL_VERBOSE(("unpacked btl %d: cpc %d: priority %d, msg len %d (offset now %d)",
                         i, j, cpcd->cbm_priority, 
                         cpcd->cbm_modex_message_len,
                         (int)(offset-(char*)message)));
            if (cpcd->cbm_modex_message_len > 0) {
                cpcd->cbm_modex_message = malloc(cpcd->cbm_modex_message_len);
                if (NULL == cpcd->cbm_modex_message) {
                    BTL_ERROR(("Failed to malloc"));
                    return NULL;
                }
                memcpy(cpcd->cbm_modex_message, offset, 
                       cpcd->cbm_modex_message_len);
                offset += cpcd->cbm_modex_message_len;
                BTL_VERBOSE(("unpacked btl %d: cpc %d: blob unpacked %d %x (offset now %d)",
                             i, j,
                             ((uint32_t*)cpcd->cbm_modex_message)[0],
                             ((uint32_t*)cpcd->cbm_modex_message)[1],
                             (int)(offset-((char*)message))));
            }
        }
    }

    if (0 == module_proc->proc_port_count) {
        module_proc->proc_endpoints = NULL;
    } else {
        module_proc->proc_endpoints = (mca_btl_base_endpoint_t**)
            malloc(module_proc->proc_port_count * 
                   sizeof(mca_btl_base_endpoint_t*));
    }
    if (NULL == module_proc->proc_endpoints) {
        OBJ_RELEASE(module_proc);
        return NULL;
    }

    BTL_VERBOSE(("unpacking done!"));
    return module_proc;
}

int mca_btl_openib_proc_remove(ompi_proc_t *proc,
                               mca_btl_base_endpoint_t *endpoint)
{
    size_t i;
    mca_btl_openib_proc_t* ib_proc = NULL;

    /* Remove endpoint from the openib BTL version of the proc as
       well */
    ib_proc = mca_btl_openib_proc_lookup_ompi(proc);
    if (NULL != ib_proc) {
        for (i = 0; i < ib_proc->proc_endpoint_count; ++i) {
            if (ib_proc->proc_endpoints[i] == endpoint) {
                ib_proc->proc_endpoints[i] = NULL;
                if (i == ib_proc->proc_endpoint_count - 1) {
                    --ib_proc->proc_endpoint_count;
                }
                return OMPI_SUCCESS;
            }
        }
    }

    return OMPI_ERR_NOT_FOUND;
}

/*
 * Note that this routine must be called with the lock on the process
 * already held.  Insert a btl instance into the proc array and assign
 * it an address.
 */
int mca_btl_openib_proc_insert(mca_btl_openib_proc_t* module_proc,
        mca_btl_base_endpoint_t* module_endpoint)
{
    /* insert into endpoint array */


#ifndef WORDS_BIGENDIAN
    /* if we are little endian and our peer is not so lucky, then we
       need to put all information sent to him in big endian (aka
       Network Byte Order) and expect all information received to
       be in NBO.  Since big endian machines always send and receive
       in NBO, we don't care so much about that case. */
    if (module_proc->proc_ompi->proc_arch & OPAL_ARCH_ISBIGENDIAN) {
        module_endpoint->nbo = true;
    }
#endif

    /* only allow eager rdma if the peers agree on the size of a long */
    if((module_proc->proc_ompi->proc_arch & OPAL_ARCH_LONGISxx) !=
       (ompi_proc_local()->proc_arch & OPAL_ARCH_LONGISxx)) {
        module_endpoint->use_eager_rdma = false;
    }

    module_endpoint->endpoint_proc = module_proc;
    module_proc->proc_endpoints[module_proc->proc_endpoint_count++] = module_endpoint;
    return OMPI_SUCCESS;
}
