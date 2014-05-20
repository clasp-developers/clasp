/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include "opal/util/output.h"
#include <sys/time.h>
#include <time.h>
#include "ompi/types.h"
#include "mtl_mx.h"
#include "mtl_mx_types.h"
#include "mtl_mx_endpoint.h" 
#include "ompi/runtime/ompi_module_exchange.h"

/*
 * Initialize state of the endpoint instance.
 *
 */

static void mca_mtl_mx_endpoint_construct(mca_mtl_mx_endpoint_t* endpoint)
{
    endpoint->mtl_mx_module = NULL;
    endpoint->mx_peer = NULL;
}

/*
 * Destroy a endpoint
 *
 */

static void mca_mtl_mx_endpoint_destruct(mca_mtl_mx_endpoint_t* endpoint)
{
}


OBJ_CLASS_INSTANCE(
    mca_mtl_mx_endpoint_t, 
    opal_list_item_t, 
    mca_mtl_mx_endpoint_construct, 
    mca_mtl_mx_endpoint_destruct);





mca_mtl_mx_endpoint_t* mca_mtl_mx_endpoint_create(ompi_proc_t* ompi_proc) { 
    mca_mtl_mx_endpoint_t* mtl_mx_endpoint = NULL;
    int rc; 
    mca_mtl_mx_addr_t *mx_peer; 
    size_t size;
    mx_return_t mx_return;
    int num_retry = 0;
    /* get the remote proc's address (only one) */
    rc = ompi_modex_recv(&mca_mtl_mx_component.super.mtl_version, 
                                 ompi_proc, (void**)&mx_peer, &size);
    if( rc != OMPI_SUCCESS || size != sizeof(mca_mtl_mx_addr_t)) { 
        return NULL; 
    }
    
    mtl_mx_endpoint = (mca_mtl_mx_endpoint_t*) OBJ_NEW(mca_mtl_mx_endpoint_t);
    mtl_mx_endpoint->mx_peer = mx_peer;
    
 retry_connect:
    mx_return = mx_connect(ompi_mtl_mx.mx_endpoint, 
                           mx_peer->nic_id, 
                           mx_peer->endpoint_id, 
                           ompi_mtl_mx.mx_filter, 
                           ompi_mtl_mx.mx_timeout, 
                           &mtl_mx_endpoint->mx_peer_addr);
    if(MX_SUCCESS != mx_return) { 
        char peer_name[MX_MAX_HOSTNAME_LEN];
        if(MX_TIMEOUT == mx_return) { 
            if( num_retry++ < ompi_mtl_mx.mx_retries ) { 
                goto retry_connect;
            }
        }
        
        if(MX_SUCCESS != mx_nic_id_to_hostname( mx_peer->nic_id, peer_name)) { 
            sprintf( peer_name, "unknown %lx nic_id", (long)mx_peer->nic_id ); 
        }
        opal_output(ompi_mtl_base_output, 
                    "mx_connect fail for %s with key %x (error %s)\n", 
                    peer_name, ompi_mtl_mx.mx_filter, mx_strerror(mx_return) );
        return NULL;
    }
    
    
    return mtl_mx_endpoint;
    
}
