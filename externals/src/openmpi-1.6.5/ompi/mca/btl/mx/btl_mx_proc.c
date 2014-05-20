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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "orte/util/name_fns.h"
#include "ompi/runtime/ompi_module_exchange.h"

#include "btl_mx.h"
#include "btl_mx_proc.h"

static void mca_btl_mx_proc_construct(mca_btl_mx_proc_t* proc);
static void mca_btl_mx_proc_destruct(mca_btl_mx_proc_t* proc);

OBJ_CLASS_INSTANCE(mca_btl_mx_proc_t, 
        opal_list_item_t, mca_btl_mx_proc_construct, 
        mca_btl_mx_proc_destruct);

void mca_btl_mx_proc_construct(mca_btl_mx_proc_t* mx_proc)
{
    mx_proc->proc_ompi           = 0;
    mx_proc->mx_peers_count      = 0;
    mx_proc->mx_peers            = NULL;
    mx_proc->mx_routing          = NULL;
    OBJ_CONSTRUCT(&mx_proc->proc_lock, opal_mutex_t);
    /* add to list of all proc instance */
    OPAL_THREAD_LOCK(&mca_btl_mx_component.mx_lock);
    opal_list_append(&mca_btl_mx_component.mx_procs, &mx_proc->super);
    OPAL_THREAD_UNLOCK(&mca_btl_mx_component.mx_lock);
}

/*
 * Cleanup MX proc instance
 */

void mca_btl_mx_proc_destruct(mca_btl_mx_proc_t* mx_proc)
{
    /* remove from list of all proc instances */
    OPAL_THREAD_LOCK(&mca_btl_mx_component.mx_lock);
    opal_list_remove_item(&mca_btl_mx_component.mx_procs, &mx_proc->super);
    OPAL_THREAD_UNLOCK(&mca_btl_mx_component.mx_lock);

    /* release resources */
    if( NULL != mx_proc->mx_peers ) {
        free(mx_proc->mx_peers);
        mx_proc->mx_peers = NULL;
    }
    if( NULL != mx_proc->mx_routing ) {
        free(mx_proc->mx_routing);
        mx_proc->mx_routing = NULL;
    }
    OBJ_DESTRUCT(&mx_proc->proc_lock);
}


/*
 * Look for an existing MX process instances based on the associated
 * ompi_proc_t instance.
 */
static mca_btl_mx_proc_t* mca_btl_mx_proc_lookup_ompi(ompi_proc_t* ompi_proc)
{
    mca_btl_mx_proc_t* mx_proc;

    OPAL_THREAD_LOCK(&mca_btl_mx_component.mx_lock);

    for( mx_proc = (mca_btl_mx_proc_t*)opal_list_get_first(&mca_btl_mx_component.mx_procs);
         mx_proc != (mca_btl_mx_proc_t*)opal_list_get_end(&mca_btl_mx_component.mx_procs);
         mx_proc  = (mca_btl_mx_proc_t*)opal_list_get_next(mx_proc) ) {

        if(mx_proc->proc_ompi == ompi_proc) {
            OPAL_THREAD_UNLOCK(&mca_btl_mx_component.mx_lock);
            return mx_proc;
        }

    }

    OPAL_THREAD_UNLOCK(&mca_btl_mx_component.mx_lock);

    return NULL;
}

/**
 * Create a MX process structure. There is a one-to-one correspondence
 * between a ompi_proc_t and a mca_btl_mx_proc_t instance. We cache
 * additional data (specifically the list of mca_btl_mx_endpoint_t instances, 
 * and published addresses) associated w/ a given destination on this
 * datastructure.
 */
mca_btl_mx_proc_t* mca_btl_mx_proc_create(ompi_proc_t* ompi_proc)
{
    mca_btl_mx_proc_t* module_proc = NULL;
    mca_btl_mx_addr_t  *mx_peers;
    int i, j, rc, mx_peers_count, *mx_routing;
    bool at_least_one_route = false;
    size_t size;

    /* Check if we have already created a MX proc
     * structure for this ompi process */
    module_proc = mca_btl_mx_proc_lookup_ompi(ompi_proc);
    if( module_proc != NULL ) {
        return module_proc;  /* Gotcha! */
    }

    /* query for the peer address info */
    rc = ompi_modex_recv( &mca_btl_mx_component.super.btl_version,
				  ompi_proc, (void*)&mx_peers, &size );
    if( OMPI_SUCCESS != rc ) {
        opal_output( 0, "mca_pml_base_modex_recv failed for peer %s",
		     ORTE_NAME_PRINT(&ompi_proc->proc_name) );
	return NULL;
    }

    if( size < sizeof(mca_btl_mx_addr_t) ) {  /* no available connection */
        return NULL;
    }
    if( (size % sizeof(mca_btl_mx_addr_t)) != 0 ) {
        opal_output( 0, "invalid mx address for peer %s",
		     ORTE_NAME_PRINT(&ompi_proc->proc_name) );
	return NULL;
    }
    /* Let's see if we have a way to connect to the remote proc using MX.
     * Without the routing information from the mapper, it is pretty
     * to do this. Right now, we base this connection detection on the last
     * 6 digits of the mapper MAC.
     */
    mx_peers_count = size / sizeof(mca_btl_mx_addr_t);
    mx_routing = (int*)malloc( mx_peers_count * sizeof(int) );
    for( i = 0; i < mx_peers_count; mx_routing[i++] = -1 );

    for( i = 0; i < mx_peers_count; i++ ) {
        mca_btl_mx_module_t* mx_btl;
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
        BTL_MX_ADDR_NTOH(mx_peers[rc]);
#endif
	for( j = 0; j < mca_btl_mx_component.mx_num_btls; j++ ) {
	    mx_btl = mca_btl_mx_component.mx_btls[j];
            if( mx_btl->mx_unique_network_id == mx_peers[j].unique_network_id ) {
                /* There is at least one connection between these two nodes */
	        if( -1 == mx_routing[j] ) {
		    /* First connection */
		    mx_routing[j] = i;
		    at_least_one_route = true;
		    break;
		}
		/* If multiple remote endpoints match mine, we keep going. As a
		 * result we will match them in order, i.e. remote endpoint 0
		 * will be connected to local endpoint 0.
		 */
            }
        }
    }
    if( false == at_least_one_route ) {
        free(mx_routing);
	return NULL;
    }

    module_proc = OBJ_NEW(mca_btl_mx_proc_t);
    module_proc->proc_ompi      = ompi_proc;
    module_proc->mx_peers_count = mx_peers_count;
    module_proc->mx_peers       = mx_peers;
    module_proc->mx_routing     = mx_routing;
    return module_proc;
}


/**
 * Note that this routine must be called with the lock on the process
 * already held.  Insert a btl instance into the proc array and assign 
 * it an address.
 */
int mca_btl_mx_proc_insert( mca_btl_mx_proc_t* module_proc, 
                            mca_btl_mx_endpoint_t* module_endpoint )
{
    mca_btl_mx_module_t* mx_btl;
    int btl_index, peer_endpoint_index;

    for( btl_index = 0; btl_index < mca_btl_mx_component.mx_num_btls; btl_index++ ) {
        mx_btl = mca_btl_mx_component.mx_btls[btl_index];
	peer_endpoint_index = module_proc->mx_routing[btl_index];
	if( (-1 != peer_endpoint_index) && (mx_btl == module_endpoint->endpoint_btl) ) {
	    module_endpoint->mx_peer = module_proc->mx_peers + peer_endpoint_index;
	    module_endpoint->endpoint_proc = module_proc;
	    return OMPI_SUCCESS;
	}
    }
    module_proc->mx_peers_count = 0;
    /**
     * No Myrinet connectivity. Let the PML layer figure out another
     * way to communicate with the peer.
     */
    return OMPI_ERROR;
}

int mca_btl_mx_proc_connect( mca_btl_mx_endpoint_t* module_endpoint )
{
    int num_retry = 0;
    mx_return_t mx_status;
    mx_endpoint_addr_t mx_remote_addr;

    module_endpoint->status = MCA_BTL_MX_CONNECTION_PENDING;

 retry_connect:
    mx_status = mx_connect( module_endpoint->endpoint_btl->mx_endpoint,
                            module_endpoint->mx_peer->nic_id, module_endpoint->mx_peer->endpoint_id,
                            mca_btl_mx_component.mx_filter, mca_btl_mx_component.mx_timeout, &mx_remote_addr );
    if( MX_SUCCESS != mx_status ) {
        if( MX_TIMEOUT == mx_status )
            if( num_retry++ < mca_btl_mx_component.mx_connection_retries )
                goto retry_connect;
        {
            char peer_name[MX_MAX_HOSTNAME_LEN];
            
            if( MX_SUCCESS != mx_nic_id_to_hostname( module_endpoint->mx_peer->nic_id, peer_name ) )
                sprintf( peer_name, "unknown %lx nic_id", (long)module_endpoint->mx_peer->nic_id );
            
            opal_output( 0, "mx_connect fail for %s with key %x (error %s)\n\tUnique ID (local %x remote %x)\n",
                         peer_name, mca_btl_mx_component.mx_filter, mx_strerror(mx_status),
			 module_endpoint->endpoint_btl->mx_unique_network_id,
			 module_endpoint->mx_peer->unique_network_id );
        }
        module_endpoint->status = MCA_BTL_MX_NOT_REACHEABLE;
        return OMPI_ERROR;
    }
    module_endpoint->mx_peer_addr = mx_remote_addr;
    module_endpoint->status       = MCA_BTL_MX_CONNECTED;

    return OMPI_SUCCESS;
}

