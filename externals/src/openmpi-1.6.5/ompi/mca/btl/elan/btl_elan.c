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
#include "opal/class/opal_bitmap.h"
#include "opal/util/output.h"
#include "ompi/mca/btl/btl.h"
#include "btl_elan.h"
#include "btl_elan_frag.h" 
#include "btl_elan_proc.h"
#include "btl_elan_endpoint.h"

#include "opal/datatype/opal_convertor.h" 
#include "ompi/mca/btl/base/base.h" 

#include <stdio.h>
#include "elan/elan.h"
#include "opal/util/os_path.h"
#include "opal/util/opal_environ.h"
#include "orte/util/proc_info.h"

/**
 *
 */
extern char** environ;

/**
 * Reduce function that compute the MAX over an array of integers.
 */
static void __reduce_max_fn( void *vin, void* vinout, int* count, void* handle )
{
    int *in = (int*)vin, *inout = (int*)vinout;
    int i;
    
    for( i = 0; i < (*count); i++ ) {
        if( in[i] > inout[i] ) inout[i] = in[i];
    }
}

/**
 * PML->BTL notification of change in the process list.
 * 
 * @param btl (IN)
 * @param nprocs (IN)     Number of processes
 * @param procs (IN)      Set of processes
 * @param peers (OUT)     Set of (optional) peer addressing info.
 * @param peers (IN/OUT)  Set of processes that are reachable via this BTL.
 * @return     OMPI_SUCCESS or error status on failure.
 * 
 */
static int mca_btl_elan_add_procs( struct mca_btl_base_module_t* btl, 
                                   size_t nprocs, 
                                   struct ompi_proc_t **ompi_procs, 
                                   struct mca_btl_base_endpoint_t** peers, 
                                   opal_bitmap_t* reachable )
{
    mca_btl_elan_module_t* elan_btl = (mca_btl_elan_module_t*)btl;
    int i, j, rc;
    char* filename;
    FILE* file;
    ELAN_BASE* base;
    
    filename = opal_os_path( false, orte_process_info.proc_session_dir, "ELAN_ID", NULL );
    file = fopen( filename, "w" );
    fprintf( file, "%s %d\n", ompi_proc_local_proc->proc_hostname, elan_btl->elan_position );

    for(i = 0; i < (int)nprocs; i++) {
        struct ompi_proc_t* ompi_proc = ompi_procs[i];
        mca_btl_elan_proc_t* elan_proc;
        mca_btl_base_endpoint_t* elan_endpoint;
 
        /* Don't use Elan for local communications */
        if( ompi_proc_local_proc == ompi_proc )
            continue;
 
        if(NULL == (elan_proc = mca_btl_elan_proc_create(ompi_proc))) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        elan_endpoint = OBJ_NEW(mca_btl_elan_endpoint_t);
        if(NULL == elan_endpoint) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        elan_endpoint->endpoint_btl = elan_btl;
 
        OPAL_THREAD_LOCK(&elan_proc->proc_lock);
        rc = mca_btl_elan_proc_insert(elan_proc, elan_endpoint);
        OPAL_THREAD_UNLOCK(&elan_proc->proc_lock);
 
        if( OMPI_SUCCESS != rc ) {
            OBJ_RELEASE(elan_endpoint);
            OBJ_RELEASE(elan_proc);
            continue;
        }
        for( j = 0; j < (int)elan_proc->proc_rail_count; j++ ) {
            fprintf( file, "%s %d\n", ompi_proc->proc_hostname,
                     elan_proc->position_id_array[j] );
        }
        opal_bitmap_set_bit(reachable, i);
        peers[i] = elan_endpoint;
    }
    fclose(file);
    /* Set the environment before firing up the Elan library */
    opal_setenv( "LIBELAN_MACHINES_FILE", filename, true, &environ );
    opal_setenv( "MPIRUN_ELANIDMAP_FILE", mca_btl_elan_component.elanidmap_file,
                 false, &environ ); 

    base = elan_baseInit(0);  
    if( NULL == base )  
        return OMPI_ERR_OUT_OF_RESOURCE;
    if( NULL == base->state ) {  
        mca_btl_base_error_no_nics( "ELAN", "Quadrics" ); 
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    elan_btl->base      = base; 
    elan_btl->elan_vp   = base->state->vp;

    {
        unsigned int* vp_array = (unsigned int*)calloc( nprocs, sizeof(unsigned int) );
        /* Set my position in the array with the Elan vp */
        vp_array[(int)ompi_proc_local_proc->proc_name.vpid] = elan_btl->elan_vp;
        
        /* Do a reduce with the previously defined MAX function. The outcome will be
         * that at each process vpid index we will have their Elan vp. With this Elan
         * vp we can therefore communicate with the process.
         */
        elan_reduce( base->allGroup, vp_array, vp_array, sizeof(unsigned int), (int)nprocs,
                     __reduce_max_fn, NULL, 0, 0,
                     ELAN_REDUCE_COMMUTE | ELAN_RESULT_ALL | base->group_flags, 0);
        
        for(i = 0; i < (int)nprocs; i++) {
            if(NULL == peers[i])
                continue;
            peers[i]->elan_vp = vp_array[(int)ompi_procs[i]->proc_name.vpid];       
        }
        free(vp_array);
    }

    /* Create the tport global queue */
    if( (elan_btl->tport_queue = elan_gallocQueue(base, base->allGroup)) == NULL ) {  
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Create and initialize the tport */
    if( !(elan_btl->tport = elan_tportInit(base->state,
                                           elan_btl->tport_queue,
                                           mca_btl_elan_component.elan_max_posted_recv,
                                           base->tport_smallmsg,
                                           mca_btl_elan_module.super.btl_eager_limit,
                                           base->tport_stripemsg, 
                                           ELAN_POLL_EVENT,  
                                           base->retryCount, 
                                           &base->shm_key,  
                                           base->shm_fifodepth, 
                                           base->shm_fragsize,  
                                           0))) { 
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Create the receive queue */
    if( (elan_btl->global_queue = elan_gallocQueue(base, base->allGroup)) == NULL ) {  
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    mca_btl_elan_component.queue_max_size = elan_queueMaxSlotSize( base->state )
        - sizeof(mca_btl_elan_hdr_t);

    elan_btl->rx_queue =
        elan_queueRxInit( base->state,                                 /* ELAN_STATE *state */
                          elan_btl->global_queue,                      /* ELAN_QUEUE *queue */
                          mca_btl_elan_component.elan_max_posted_recv, /* int nSlots */
                          (int)mca_btl_elan_component.queue_max_size,  /* int slotSize */
                          ELAN_RAIL_ALL,                               /* int rail */
                          0 );                                         /* ELAN_FLAGS flags */

    elan_btl->tx_queue =
        elan_queueTxInit( base->state,                         /* ELAN_STATE *state */
                          elan_btl->global_queue,              /* ELAN_QUEUE *q */
                          ELAN_RAIL_ALL,                       /* int rail */
                          0 );                                 /* ELAN_FLAGS flags */

    for( i = 0; i < mca_btl_elan_component.elan_max_posted_recv; i++ ) {
        mca_btl_elan_frag_t* frag;

        MCA_BTL_ELAN_FRAG_ALLOC_EAGER(frag, rc );
        if( OPAL_UNLIKELY(NULL == frag) ) {
            return OMPI_ERROR; 
        }
        frag->segment.seg_addr.pval = (void*)(frag + 1);
        frag->base.des_dst     = &(frag->segment);
        frag->base.des_dst_cnt = 1;
        frag->base.des_src     = NULL;
        frag->base.des_src_cnt = 0;
        frag->type             = MCA_BTL_ELAN_HDR_TYPE_RECV;
        frag->elan_event = elan_tportRxStart( elan_btl->tport,
                                              ELAN_TPORT_RXBUF | ELAN_TPORT_RXANY,
                                              0, 0, 0, 0,
                                              frag->segment.seg_addr.pval,
                                              mca_btl_elan_module.super.btl_eager_limit );
        opal_list_append( &(elan_btl->recv_list), (opal_list_item_t*)frag );
    }

    /* enable the network */
    elan_enable_network( elan_btl->base->state );

    return OMPI_SUCCESS;
}

/**
 * PML->BTL notification of change in the process list.
 *
 * @param btl (IN)     BTL instance
 * @param nproc (IN)   Number of processes.
 * @param procs (IN)   Set of processes.
 * @param peers (IN)   Set of peer data structures.
 * @return             Status indicating if cleanup was successful
 *
 */
static int mca_btl_elan_del_procs( struct mca_btl_base_module_t* btl, 
                                   size_t nprocs, 
                                   struct ompi_proc_t **procs, 
                                   struct mca_btl_base_endpoint_t ** endpoints )
{
    return OMPI_SUCCESS;
}

/**
 * Allocate a descriptor with a segment of the requested size.
 * Note that the BTL layer may choose to return a smaller size
 * if it cannot support the request.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */
static mca_btl_base_descriptor_t*
mca_btl_elan_alloc( struct mca_btl_base_module_t* btl,
                    struct mca_btl_base_endpoint_t* peer,
                    uint8_t order,
                    size_t size,
                    uint32_t flags )
{
    mca_btl_elan_frag_t* frag = NULL;
    ptrdiff_t hdr_skip = 0;
    int rc;

    if( size <= btl->btl_eager_limit ) {
        MCA_BTL_ELAN_FRAG_ALLOC_EAGER(frag, rc);
        if( size <= mca_btl_elan_component.queue_max_size ) {  /* This will be going over the queue */
            hdr_skip = sizeof(mca_btl_elan_hdr_t);
        }
    } else if( size <= btl->btl_max_send_size ) {
        MCA_BTL_ELAN_FRAG_ALLOC_MAX(frag, rc);
    }
    if( OPAL_UNLIKELY(NULL == frag) ) {
        return NULL;
    }

    frag->segment.seg_addr.pval = (void*)((char*)(frag + 1) + hdr_skip);
    frag->segment.seg_len  = size;
    frag->base.des_src     = &(frag->segment);
    frag->base.des_src_cnt = 1;
    frag->base.des_dst     = NULL;
    frag->base.des_dst_cnt = 0;
    frag->base.des_flags   = flags;
    frag->btl = (mca_btl_elan_module_t*)btl;
    frag->endpoint = peer;
    frag->base.order = MCA_BTL_NO_ORDER;
    return (mca_btl_base_descriptor_t*)frag;
}


/**
 * Return a segment allocated by this BTL.
 *
 * @param btl (IN)      BTL module
 * @param descriptor (IN)  Allocated descriptor.
 */
static int mca_btl_elan_free( struct mca_btl_base_module_t* btl, 
                              mca_btl_base_descriptor_t* des ) 
{
    mca_btl_elan_frag_t* frag = (mca_btl_elan_frag_t*)des;
    MCA_BTL_ELAN_FRAG_RETURN(frag); 
    return OMPI_SUCCESS; 
}

/**
 * Prepare a descriptor for send/rdma using the supplied
 * convertor. If the convertor references data that is contigous,
 * the descriptor may simply point to the user buffer. Otherwise,
 * this routine is responsible for allocating buffer space and
 * packing if required.
 *
 * @param btl (IN)          BTL module
 * @param endpoint (IN)     BTL peer addressing
 * @param convertor (IN)    Data type convertor
 * @param reserve (IN)      Additional bytes requested by upper layer to precede user data
 * @param size (IN/OUT)     Number of bytes to prepare (IN), number of bytes actually prepared (OUT) 
 */
static mca_btl_base_descriptor_t*
mca_btl_elan_prepare_src( struct mca_btl_base_module_t* btl,
                          struct mca_btl_base_endpoint_t* endpoint,
                          struct mca_mpool_base_registration_t* registration,
                          struct opal_convertor_t* convertor,
                          uint8_t order,
                          size_t reserve,
                          size_t* size,
                          uint32_t flags)
{
    mca_btl_elan_frag_t* frag;
    struct iovec iov;
    uint32_t iov_count = 1;
    size_t max_data = *size, skip = 0;
    int rc;

    if( OPAL_UNLIKELY(max_data > UINT32_MAX) ) {  
        max_data = (size_t)UINT32_MAX;
    }
    if( 0 != reserve ) {
        if( max_data + reserve <= btl->btl_eager_limit ) {
            MCA_BTL_ELAN_FRAG_ALLOC_EAGER(frag, rc);

            if( (max_data + reserve) <= mca_btl_elan_component.queue_max_size ) {
                skip = sizeof(mca_btl_elan_hdr_t);
            }
        } else {
            MCA_BTL_ELAN_FRAG_ALLOC_MAX(frag, rc);
            
            if( (max_data + reserve) > btl->btl_max_send_size ) {
                max_data = btl->btl_max_send_size - reserve;
            }
        }
        if( OPAL_UNLIKELY(NULL == frag) ) {
            return NULL;
        }
        frag->segment.seg_addr.pval = (void*)((unsigned char*)(frag + 1) + skip);
        iov.iov_len = max_data;
        iov.iov_base = (unsigned char*)frag->segment.seg_addr.pval + reserve;
        
        rc = opal_convertor_pack(convertor, &iov, &iov_count, &max_data );
        *size  = max_data;
        if( rc < 0 ) {
            MCA_BTL_ELAN_FRAG_RETURN(frag);
            return NULL;
        }
        frag->segment.seg_len = max_data + reserve;
    } else {  /* this is a real RDMA operation */
        MCA_BTL_ELAN_FRAG_ALLOC_USER(frag, rc);
        if(OPAL_UNLIKELY(NULL == frag)) {
            return NULL;
        }
        frag->type = MCA_BTL_ELAN_HDR_TYPE_PUT;
        iov.iov_len = max_data;
        iov.iov_base = NULL;
        opal_convertor_pack(convertor, &iov, &iov_count, &max_data);
        *size = max_data;
        frag->segment.seg_addr.pval = iov.iov_base;
        frag->segment.seg_len = max_data;
    }

    frag->base.des_src = &(frag->segment);
    frag->base.des_src_cnt = 1;
    frag->base.order = MCA_BTL_NO_ORDER;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    frag->base.des_flags = flags;
    return &frag->base;

}

/**
 * Prepare a descriptor for send/rdma using the supplied
 * convertor. If the convertor references data that is contigous,
 * the descriptor may simply point to the user buffer. Otherwise,
 * this routine is responsible for allocating buffer space and
 * packing if required.
 *
 * @param btl (IN)          BTL module
 * @param endpoint (IN)     BTL peer addressing
 * @param convertor (IN)    Data type convertor
 * @param reserve (IN)      Additional bytes requested by upper layer to precede user data
 * @param size (IN/OUT)     Number of bytes to prepare (IN), number of bytes actually prepared (OUT)
 */

static mca_btl_base_descriptor_t*
mca_btl_elan_prepare_dst( struct mca_btl_base_module_t* btl,
                          struct mca_btl_base_endpoint_t* endpoint,
                          struct mca_mpool_base_registration_t* registration,
                          struct opal_convertor_t* convertor,
                          uint8_t order,
                          size_t reserve,
                          size_t* size,
                          uint32_t flags )
{
    mca_btl_elan_frag_t* frag;
    size_t origin, position = *size;
    int rc;

    if( OPAL_UNLIKELY((*size) > UINT32_MAX) ) {
        *size = (size_t)UINT32_MAX;
    }
    MCA_BTL_ELAN_FRAG_ALLOC_USER(frag, rc);
    if( OPAL_UNLIKELY(NULL == frag) ) {
        return NULL;
    }

    opal_convertor_get_current_pointer( convertor, (void**)&(frag->segment.seg_addr.pval) );
    origin = convertor->bConverted;
    position += origin;
    opal_convertor_set_position( convertor, &position );
    *size = position - origin;

    frag->segment.seg_len = *size;
    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
    frag->base.des_flags = flags;	
    frag->base.des_dst = &(frag->segment);
    frag->base.des_dst_cnt = 1;
    frag->base.order = MCA_BTL_NO_ORDER;
    return &frag->base;
}


/**
 * Initiate an asynchronous send.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transfered
 * @param tag (IN)         The tag value used to notify the peer.
 */

static int mca_btl_elan_send( struct mca_btl_base_module_t* btl,
                              struct mca_btl_base_endpoint_t* endpoint,
                              struct mca_btl_base_descriptor_t* descriptor, 
                              mca_btl_base_tag_t tag )
   
{
    mca_btl_elan_module_t* elan_btl = (mca_btl_elan_module_t*) btl;
    mca_btl_elan_frag_t* frag = (mca_btl_elan_frag_t*)descriptor; 
    mca_btl_elan_hdr_t* elan_hdr = (mca_btl_elan_hdr_t*)(frag+1);
    int send_len;

    frag->btl        = elan_btl;
    frag->endpoint   = endpoint;
    frag->tag        = tag;
    frag->type       = MCA_BTL_ELAN_HDR_TYPE_SEND;

    if( frag->segment.seg_len <= mca_btl_elan_component.queue_max_size ) {
        elan_hdr->tag    = (int)tag;
        elan_hdr->length = (int)frag->segment.seg_len;
        send_len         = frag->segment.seg_len + sizeof(mca_btl_elan_hdr_t);
        frag->elan_event = elan_queueTx( elan_btl->tx_queue,
                                         endpoint->elan_vp,
                                         (void*)elan_hdr,
                                         send_len, ELAN_RAIL_ALL );
        if( OPAL_UNLIKELY(NULL == frag->elan_event) ) {
            opal_output( 0, "elan_queueTx failed for destination %d\n", endpoint->elan_vp );
            return OMPI_ERROR;
        }
    } else {
        frag->elan_event = elan_tportTxStart( elan_btl->tport, 0, endpoint->elan_vp,
                                              elan_btl->elan_vp, frag->tag,
                                              (void*)elan_hdr, frag->segment.seg_len );
        if( OPAL_UNLIKELY(NULL == frag->elan_event) ) {
            opal_output( 0, "elan_tportTxStart failed for destination %d\n", endpoint->elan_vp );
            return OMPI_ERROR;
        }
    }
    if( elan_poll( frag->elan_event, 0 ) ) {
        int btl_ownership = (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP );

        frag->base.des_cbfunc( &(elan_btl->super), frag->endpoint,
                               &(frag->base), OMPI_SUCCESS );
        if( btl_ownership ) {
            MCA_BTL_ELAN_FRAG_RETURN(frag);
        }
        return OMPI_SUCCESS;
    }
    /* Add the fragment to the pending send list */
    OPAL_THREAD_LOCK( &elan_btl->elan_lock );
    opal_list_append( &(elan_btl->send_list), (opal_list_item_t*)frag );
    OPAL_THREAD_UNLOCK( &elan_btl->elan_lock );
    return OMPI_ERR_RESOURCE_BUSY;
}


/**
 * Initiate an asynchronous put.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */
static int mca_btl_elan_put( mca_btl_base_module_t* btl,
                             mca_btl_base_endpoint_t* endpoint,
                             mca_btl_base_descriptor_t* des )
{
    mca_btl_elan_module_t* elan_btl = (mca_btl_elan_module_t*) btl;
    mca_btl_elan_frag_t* frag = (mca_btl_elan_frag_t*) des; 
    int     peer = endpoint->elan_vp;
    mca_btl_base_segment_t* src = des->des_src;
    mca_btl_base_segment_t* dst = des->des_dst;
    unsigned char* src_addr = (unsigned char*)src->seg_addr.pval;
    size_t src_len = src->seg_len;
    unsigned char* dst_addr = (unsigned char*)ompi_ptr_ltop(dst->seg_addr.lval);

    frag->endpoint = endpoint;
    frag->btl = elan_btl;
    frag->type = MCA_BTL_ELAN_HDR_TYPE_PUT;
    frag->elan_event = elan_put(elan_btl->base->state, src_addr, dst_addr, src_len, peer);
    /* Add the fragment to the pending RDMA list */
    OPAL_THREAD_LOCK( &elan_btl->elan_lock );
    opal_list_append( &(elan_btl->rdma_list), (opal_list_item_t*)frag );
    OPAL_THREAD_UNLOCK( &elan_btl->elan_lock );
    return OMPI_SUCCESS;
}


/**
 * Initiate an asynchronous get.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 *
 */

static int mca_btl_elan_get( mca_btl_base_module_t* btl,
                             mca_btl_base_endpoint_t* endpoint,
                             mca_btl_base_descriptor_t* des )
{
    mca_btl_elan_module_t* elan_btl = (mca_btl_elan_module_t*) btl;
    mca_btl_elan_frag_t* frag = (mca_btl_elan_frag_t*) des; 
    int peer = endpoint->elan_vp;
    mca_btl_base_segment_t* src = des->des_src;
    mca_btl_base_segment_t* dst = des->des_dst;
    unsigned char* src_addr = (unsigned char*)src->seg_addr.pval;
    size_t src_len = src->seg_len;
    unsigned char* dst_addr = (unsigned char*)dst->seg_addr.lval;

    frag->endpoint = endpoint;
    frag->btl = elan_btl;
    frag->type = MCA_BTL_ELAN_HDR_TYPE_GET;
    opal_output( 0, "elan_get( remote %p, local %p, length %d, peer %d )\n",
                 (void*)src_addr, (void*)dst_addr, (int)src_len, peer );
    frag->elan_event = elan_get(elan_btl->base->state, src_addr, dst_addr, src_len, peer);
    /* Add the fragment to the pending RDMA list */
    OPAL_THREAD_LOCK( &elan_btl->elan_lock );
    opal_list_append( &(elan_btl->rdma_list), (opal_list_item_t*)frag );
    OPAL_THREAD_UNLOCK( &elan_btl->elan_lock );
    return OMPI_SUCCESS;
}

int mca_btl_elan_finalize( struct mca_btl_base_module_t* btl )
{
    mca_btl_elan_module_t* elan_btl = (mca_btl_elan_module_t*) btl; 
    int i, num_btls;

    /* First find the correct BTL in the list attached to the component */
    num_btls = mca_btl_elan_component.elan_num_btls;
    for( i = 0; i < num_btls; i++ ) {
        if( elan_btl == mca_btl_elan_component.elan_btls[i] ) {
            /* disable the network */
            elan_disable_network( elan_btl->base->state );

            /* Remove it from the list attached to the component */
            if( i == (num_btls-1) ) {
                mca_btl_elan_component.elan_btls[i] = NULL;
            } else {
                mca_btl_elan_component.elan_btls[i] =  mca_btl_elan_component.elan_btls[num_btls-1];
            }
            mca_btl_elan_component.elan_num_btls--;

            /* Cancel all posted receives */

            /* Release the internal structures */
            OBJ_DESTRUCT(&elan_btl->recv_list);
            OBJ_DESTRUCT(&elan_btl->send_list);
            OBJ_DESTRUCT(&elan_btl->rdma_list);
            OBJ_DESTRUCT(&elan_btl->elan_lock);
            /* The BTL is clean, remove it */
            free(elan_btl);

            return OMPI_SUCCESS;
        }
    }
    /* This BTL is not present in the list attached to the communicator */
    return OMPI_ERROR;
}

int mca_btl_elan_ft_event(int state)
{
    if(OPAL_CRS_CHECKPOINT == state) {
        ;
    }
    else if(OPAL_CRS_CONTINUE == state) {
        ;
    }
    else if(OPAL_CRS_RESTART == state) {
        ;
    }
    else if(OPAL_CRS_TERM == state ) {
        ;
    }
    else {
        ;
    }

    return OMPI_SUCCESS;
}

static void mca_btl_elan_dump( struct mca_btl_base_module_t* btl,
                               struct mca_btl_base_endpoint_t* endpoint,
                               int verbose )
{
}

static int
mca_btl_elan_register_error( struct mca_btl_base_module_t* btl, 
                             mca_btl_base_module_error_cb_fn_t cbfunc )
{
    return OMPI_SUCCESS;
}

mca_btl_elan_module_t mca_btl_elan_module = {
    {
        &mca_btl_elan_component.super,
        0, /* max size of first fragment */
        0, /* min send fragment size */
        0, /* max send fragment size */
        0, /* btl_rdma_pipeline_offset */
        0, /* btl_rdma_pipeline_frag_size */
        0, /* btl_min_rdma_pipeline_size */
        0, /* exclusivity */
        0, /* latency */
        0, /* bandwidth */
        0, /* flags */
        mca_btl_elan_add_procs,
        mca_btl_elan_del_procs,
        NULL,  /* btl_register */
        mca_btl_elan_finalize,
        mca_btl_elan_alloc,
        mca_btl_elan_free,
        mca_btl_elan_prepare_src,
        mca_btl_elan_prepare_dst,
        mca_btl_elan_send,
        NULL, /* send immediate */
        mca_btl_elan_put,
        mca_btl_elan_get,
        mca_btl_elan_dump,
        NULL, /* mpool */
        mca_btl_elan_register_error, /* register error cb */
        mca_btl_elan_ft_event /* mca_btl_elan_ft_event*/
    }
};
