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
#include "ompi/constants.h"
#include "opal/event/event.h"
#include "opal/util/output.h"
#include "ompi/mca/btl/btl.h"

#include "opal/mca/base/mca_base_param.h"
#include "ompi/runtime/ompi_module_exchange.h"

#include "ompi/mca/mpool/base/base.h" 

#include "btl_elan.h"
#include "btl_elan_frag.h"
#include "btl_elan_endpoint.h" 

#include "ompi/mca/btl/base/base.h" 
#include "ompi/mca/btl/base/btl_base_error.h" 

#include "elan/elan.h"

#include "opal/util/opal_environ.h"

#define ELAN_MAX_BTL  10

mca_btl_elan_component_t mca_btl_elan_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */

        {
            MCA_BTL_BASE_VERSION_2_0_0,

            "elan", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            mca_btl_elan_component_open,  /* component open */
            mca_btl_elan_component_close  /* component close */
        },
        {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },
        mca_btl_elan_component_init,  
        mca_btl_elan_component_progress,
    }
};


/*
 * utility routines for parameter registration
 */

static inline int
mca_btl_elan_param_register_int( const char* param_name, 
                                 int default_value )
{
    int id = mca_base_param_register_int("btl","elan",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}

/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */

int mca_btl_elan_component_open(void)
{
    /* initialize state */
    mca_btl_elan_component.elan_num_btls = 0;
    mca_btl_elan_component.elan_btls = NULL;

    mca_btl_elan_module.super.btl_exclusivity = MCA_BTL_EXCLUSIVITY_DEFAULT;
    mca_btl_elan_module.super.btl_eager_limit =  32*1024;
    mca_btl_elan_module.super.btl_rndv_eager_limit = mca_btl_elan_module.super.btl_eager_limit;
    mca_btl_elan_module.super.btl_max_send_size = 64*1024; /*64*1024;*/
    mca_btl_elan_module.super.btl_rdma_pipeline_send_length = 512 * 1024;
    mca_btl_elan_module.super.btl_rdma_pipeline_frag_size = 128 * 1024;
    mca_btl_elan_module.super.btl_min_rdma_pipeline_size = 128 * 1024;
    mca_btl_elan_module.super.btl_flags = MCA_BTL_FLAGS_SEND_INPLACE | MCA_BTL_FLAGS_PUT | MCA_BTL_FLAGS_SEND;
    mca_btl_elan_module.super.btl_bandwidth = 1959;
    mca_btl_elan_module.super.btl_latency = 4;
    mca_btl_base_param_register(&mca_btl_elan_component.super.btl_version,
                                &mca_btl_elan_module.super);

    mca_base_param_reg_string( (mca_base_component_t*)&mca_btl_elan_component, "elanidmap",
                               "System-wide configuration file for the Quadrics network (elanidmap)",
                               false, false, "/etc/elanidmap", &mca_btl_elan_component.elanidmap_file );
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_elan_component, "max_posted_recv",
                            "Number of received posted in advance. Increasing this number for"
                            " communication bound application can lead to visible improvement"
                            " in performances",
                            false, false, 128, &mca_btl_elan_component.elan_max_posted_recv );
 
    /* register Elan4 component parameters */
    mca_btl_elan_component.elan_free_list_num =
        mca_btl_elan_param_register_int( "free_list_num", 8 );
    mca_btl_elan_component.elan_free_list_max =
        mca_btl_elan_param_register_int( "free_list_max",
                                         (mca_btl_elan_component.elan_free_list_num +
                                          mca_btl_elan_component.elan_max_posted_recv) );
    mca_btl_elan_component.elan_free_list_inc =
        mca_btl_elan_param_register_int( "free_list_inc", 32 );

    return OMPI_SUCCESS;
}

/*
 * component cleanup - sanity checking of queue lengths
 */

int mca_btl_elan_component_close(void)
{
    if( NULL != mca_btl_elan_component.elan_btls ) {
        free( mca_btl_elan_component.elan_btls );
        mca_btl_elan_component.elan_btls = NULL;
        mca_btl_elan_component.elan_num_btls = 0;

        /* release resources */        
        OBJ_DESTRUCT(&mca_btl_elan_component.elan_procs);
        OBJ_DESTRUCT(&mca_btl_elan_component.elan_frag_eager);
        OBJ_DESTRUCT(&mca_btl_elan_component.elan_frag_user);
        OBJ_DESTRUCT(&mca_btl_elan_component.elan_frag_max);
        OBJ_DESTRUCT(&mca_btl_elan_component.elan_lock);
    }
    return OMPI_SUCCESS;
}

/*
 *  Elan4 component initialization:
 *  (1) read interface list from kernel and compare against component parameters
 *      then create a BTL instance for selected interfaces
 *  (2) setup Elan4 listen socket for incoming connection attempts
 *  (3) register BTL parameters with the MCA
 */
mca_btl_base_module_t**
mca_btl_elan_component_init( int *num_btl_modules,
                             bool enable_progress_threads,
                             bool enable_mpi_threads )
{

    mca_btl_base_module_t** btls = NULL;

    *num_btl_modules = 0;

    /* There is no support for a progress thread yet. */
    if (enable_progress_threads) { 
        ompi_modex_send(&mca_btl_elan_component.super.btl_version, NULL, 0);
        return NULL;
    }

    OBJ_CONSTRUCT (&mca_btl_elan_component.elan_lock, opal_mutex_t);
    OBJ_CONSTRUCT (&mca_btl_elan_component.elan_frag_eager, ompi_free_list_t);
    OBJ_CONSTRUCT (&mca_btl_elan_component.elan_frag_max, ompi_free_list_t);
    OBJ_CONSTRUCT (&mca_btl_elan_component.elan_frag_user, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_elan_component.elan_procs, opal_list_t);
    ompi_free_list_init_new( &mca_btl_elan_component.elan_frag_eager,
                             sizeof(mca_btl_elan_frag_t) + mca_btl_elan_module.super.btl_eager_limit,
                             opal_cache_line_size,
                             OBJ_CLASS(mca_btl_elan_frag_t),
                             0,opal_cache_line_size,
                             mca_btl_elan_component.elan_free_list_num,
                             mca_btl_elan_component.elan_free_list_max,
                             mca_btl_elan_component.elan_free_list_inc,
                             NULL ); /* use default allocator */

    ompi_free_list_init_new( &mca_btl_elan_component.elan_frag_user,
                             sizeof(mca_btl_elan_frag_t),
                             opal_cache_line_size,
                             OBJ_CLASS(mca_btl_elan_frag_t),
                             0,opal_cache_line_size,
                             mca_btl_elan_component.elan_free_list_num,
                             mca_btl_elan_component.elan_free_list_max,
                             mca_btl_elan_component.elan_free_list_inc,
                             NULL ); /* use default allocator */

    ompi_free_list_init_new( &mca_btl_elan_component.elan_frag_max,
                             sizeof(mca_btl_elan_frag_t)+mca_btl_elan_module.super.btl_max_send_size,
                             opal_cache_line_size,
                             OBJ_CLASS(mca_btl_elan_frag_t),
                             0,opal_cache_line_size,
                             mca_btl_elan_component.elan_free_list_num,
                             mca_btl_elan_component.elan_free_list_max,
                             mca_btl_elan_component.elan_free_list_inc,
                             NULL ); /* use default allocator */
    
    mca_btl_elan_component.elan_num_btls = ELAN_MAX_BTL;  /* no more than that */
    mca_btl_elan_component.elan_btls = calloc( mca_btl_elan_component.elan_num_btls,
                                               sizeof(mca_btl_base_module_t*) );
    /* Retrieve the positions of the node in the elan network */
    {
        FILE* position;
        char filename[255], file_line[255];
        int index, count = 0, positions[ELAN_MAX_BTL];
        mca_btl_elan_module_t* btl;

        for( index = 0; index < ELAN_MAX_BTL; index++ ) {
            snprintf( filename, 255, "/proc/qsnet/elan4/device%d/position", index );
            position = fopen( filename, "r" );
            if( NULL == position ) {
                continue;
            }
            if( 0 == fscanf( position, "%s%i", file_line, &positions[count] ) ) {
                opal_output( 0, "Unable to read the network position" );
                continue;
            }
            fclose(position);
            btl =  (mca_btl_elan_module_t*)malloc (sizeof (mca_btl_elan_module_t));	
            if(NULL == btl) {
                opal_output( 0, "No enough memory to allocate the Elan internal structures" );
                return NULL;
            }
            memcpy( btl, &mca_btl_elan_module, sizeof(mca_btl_elan_module_t) );
            OBJ_CONSTRUCT( &btl->elan_lock, opal_mutex_t );
            OBJ_CONSTRUCT( &btl->send_list, opal_list_t );
            OBJ_CONSTRUCT( &btl->rdma_list, opal_list_t );
            OBJ_CONSTRUCT( &btl->recv_list, opal_list_t );

            btl->expect_tport_recv = 1;
            btl->elan_position = positions[count];

            mca_btl_elan_component.elan_btls[count++] = btl;
        }
        mca_btl_elan_component.elan_num_btls = count;
        /* Publish the network positions for the current node */
        ompi_modex_send( &mca_btl_elan_component.super.btl_version, positions,
                         count * sizeof(int));
    }

    if(mca_btl_elan_component.elan_num_btls) {
        btls = (mca_btl_base_module_t**)malloc( mca_btl_elan_component.elan_num_btls *
                                               sizeof(mca_btl_base_module_t*) );
        if( NULL == btls ) {
            free( mca_btl_elan_component.elan_btls );
            mca_btl_elan_component.elan_num_btls = 0;  /* no active BTL modules */
            return NULL;
        }
        memcpy( btls,  mca_btl_elan_component.elan_btls,
               mca_btl_elan_component.elan_num_btls * sizeof(mca_btl_elan_module_t*) );
    }
    *num_btl_modules = mca_btl_elan_component.elan_num_btls;
    return btls;
}

/*
 *  Elan4 component progress.
 */
int mca_btl_elan_component_progress( void )
{
    int num_progressed = 0, i;

    for( i = 0; i < (int)mca_btl_elan_component.elan_num_btls; i++ ) {
        mca_btl_elan_module_t* elan_btl = mca_btl_elan_component.elan_btls[i];

        /* This is a fast receive over the queue */
        if( elan_queueRxPoll( elan_btl->rx_queue, 0 ) ) {
            mca_btl_active_message_callback_t* reg;
            mca_btl_elan_hdr_t* elan_hdr = NULL;
            mca_btl_elan_frag_t frag;

            elan_hdr = (mca_btl_elan_hdr_t*)elan_queueRxWait( elan_btl->rx_queue, NULL, 0 );
            frag.base.des_dst = &frag.segment;
            frag.base.des_dst->seg_addr.pval = (void*)(elan_hdr+1);
            frag.base.des_dst->seg_len = (size_t)elan_hdr->length;
            frag.base.des_dst_cnt = 1;
            frag.tag = (mca_btl_base_tag_t)elan_hdr->tag;
            frag.size = elan_hdr->length;

            reg = mca_btl_base_active_message_trigger + frag.tag;
            reg->cbfunc( &(elan_btl->super), frag.tag, &(frag.base), reg->cbdata );
            elan_queueRxComplete( elan_btl->rx_queue );
            num_progressed++;
        }
        /* This is the slower receive over the tport */
        if(elan_btl->expect_tport_recv && !OPAL_THREAD_TRYLOCK(&elan_btl->elan_lock)) {
            mca_btl_elan_frag_t* frag = (mca_btl_elan_frag_t*)opal_list_get_first( &(elan_btl->recv_list) );
            if( elan_done(frag->elan_event, 0) ) {
                int tag; 
                size_t length;
                mca_btl_active_message_callback_t* reg;
                void* recv_buf;
                recv_buf = (mca_btl_elan_hdr_t*)elan_tportRxWait( frag->elan_event,
                                                                  NULL, &tag, &length );
                num_progressed++;
                /*elan_btl->expect_tport_recv--;*/

                opal_list_remove_first( &(elan_btl->recv_list) );
                OPAL_THREAD_UNLOCK(&elan_btl->elan_lock);

                frag->base.des_dst->seg_addr.pval = (void*)recv_buf;
                frag->base.des_dst->seg_len = length;
                frag->tag = (mca_btl_base_tag_t)tag;
                reg = mca_btl_base_active_message_trigger + frag->tag;
                reg->cbfunc( &(elan_btl->super), frag->tag, &(frag->base), reg->cbdata );
                if( recv_buf != (void*)(frag+1) ) {
                    elan_tportBufFree( elan_btl->tport, recv_buf );
                    frag->base.des_dst->seg_addr.pval = (void*)(frag+1);
                }

                frag->elan_event = elan_tportRxStart( elan_btl->tport,
                                                      ELAN_TPORT_RXBUF | ELAN_TPORT_RXANY,
                                                      0, 0, 0, 0,
                                                      frag->base.des_dst->seg_addr.pval,
                                                      mca_btl_elan_module.super.btl_eager_limit );
                OPAL_THREAD_LOCK(&elan_btl->elan_lock);
                opal_list_append( &(elan_btl->recv_list), (opal_list_item_t*)frag );
            }
            OPAL_THREAD_UNLOCK(&elan_btl->elan_lock);
        }
        /* If there are any pending sends check their completion */
      recheck_send_list:
        if( !opal_list_is_empty( &(elan_btl->send_list) ) && !OPAL_THREAD_TRYLOCK(&elan_btl->elan_lock) ) {
            mca_btl_elan_frag_t* frag = (mca_btl_elan_frag_t*)opal_list_get_first( &(elan_btl->send_list) );
            if( (NULL != frag) && elan_poll(frag->elan_event, 0) ) {
                int btl_ownership = (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP );

                opal_list_remove_first( &(elan_btl->send_list) );
                OPAL_THREAD_UNLOCK(&elan_btl->elan_lock);
                num_progressed++;

                frag->base.des_cbfunc( &(elan_btl->super), frag->endpoint,
                                       &(frag->base), OMPI_SUCCESS );
                if( btl_ownership ) {
                    MCA_BTL_ELAN_FRAG_RETURN(frag);
                }
                goto recheck_send_list;
            } else {
                OPAL_THREAD_UNLOCK(&elan_btl->elan_lock);
            }
        }
      recheck_rdma_list:
        /* If any RDMA have been posted, check their status */
        if( !opal_list_is_empty( &(elan_btl->rdma_list) ) && !OPAL_THREAD_TRYLOCK(&elan_btl->elan_lock) ) {
            mca_btl_elan_frag_t* frag = (mca_btl_elan_frag_t*)opal_list_get_first( &(elan_btl->rdma_list) );
            if( (NULL != frag) && elan_poll(frag->elan_event, 0) ) {
                int btl_ownership = (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP );

                opal_list_remove_first( &(elan_btl->rdma_list) );
                OPAL_THREAD_UNLOCK(&elan_btl->elan_lock);
                num_progressed++;

                frag->base.des_cbfunc( &(elan_btl->super), frag->endpoint,
                                       &(frag->base), OMPI_SUCCESS );
                if( btl_ownership ) {
                    MCA_BTL_ELAN_FRAG_RETURN(frag);
                }
                goto recheck_rdma_list;
            } else {
                OPAL_THREAD_UNLOCK(&elan_btl->elan_lock);
            }
        }
    }

    return num_progressed;
}
