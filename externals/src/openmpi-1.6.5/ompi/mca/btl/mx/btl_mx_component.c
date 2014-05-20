/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2011 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include "opal/prefetch.h"
#include "opal/util/opal_environ.h"
#include "ompi/constants.h"

#include "opal/mca/base/mca_base_param.h"
#include "ompi/runtime/ompi_module_exchange.h"
#include "ompi/mca/btl/base/btl_base_error.h"
#include "ompi/mca/common/mx/common_mx.h"

#include "btl_mx.h"
#include "btl_mx_frag.h"
#include "btl_mx_endpoint.h" 

#if MX_HAVE_MAPPER_STATE
#include "mx_io.h"
#include "mx_internals/mx__fops.h"
#include "mx_internals/mx__driver_interface.h"
#endif  /* MX_HAVE_MAPPER_STATE */

static int mca_btl_mx_component_register(void);
static int mca_btl_mx_component_open(void);
static int mca_btl_mx_component_close(void);


mca_btl_mx_component_t mca_btl_mx_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */

        {
            MCA_BTL_BASE_VERSION_2_0_0,

            "mx", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            mca_btl_mx_component_open,  /* component open */
            mca_btl_mx_component_close,  /* component close */
            NULL, /* component query */
            mca_btl_mx_component_register, /* component register */
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        mca_btl_mx_component_init,  
        mca_btl_mx_component_progress,
    }
};


static int mca_btl_mx_component_register(void)
{
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "max_btls",
                            "Maximum number of accepted Myrinet cards",
                            false, false, 8, &mca_btl_mx_component.mx_max_btls );
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "timeout",
                            "Timeout for connections",
                            false, false, MX_INFINITE, &mca_btl_mx_component.mx_timeout );
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "retries",
                            "Number of retries for each new connection before considering the peer as unreacheable",
                            false, false, 20, &mca_btl_mx_component.mx_connection_retries );
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "filter",
                            "Unique ID for the application (used to connect to the peers)",
                            false, false, 0xdeadbeef, &mca_btl_mx_component.mx_filter );
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "self",
                            "Enable the MX support for self communications",
                            false, false, 0, &mca_btl_mx_component.mx_support_self );
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "shared_mem",
                            "Enable the MX support for shared memory",
                            false, false, 0, &mca_btl_mx_component.mx_support_sharedmem );
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "bonding",
                            "Integrate MX library bonding. Less than 0 is system default, everything else will set the MX_BONDING to the value.",
                            false, false, 1, &mca_btl_mx_component.mx_bonding );
    if( 0 >= mca_btl_mx_component.mx_bonding ) {
        char* value = getenv("MX_BONDING");
        if( NULL == value ) {
            mca_btl_mx_component.mx_bonding = 1;
        } else {
            mca_btl_mx_component.mx_bonding = atoi(value);
            if( 0 >= mca_btl_mx_component.mx_bonding )
                mca_btl_mx_component.mx_bonding = 1;
        }
    } else if( 1 != mca_btl_mx_component.mx_bonding ) {
        char value[8];
        snprintf( value, 8, "%d\n", mca_btl_mx_component.mx_bonding );
        opal_setenv( "MX_BONDING", value, true, &environ );
    }
#ifdef HAVE_MX_REGISTER_UNEXP_HANDLER
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "register_unexp",
                            "Enable the MX support for the unexpected request handler (Open MPI matching)",
			    false, false, 0, &mca_btl_mx_component.mx_use_unexpected );
#endif  /* HAVE_MX_REGISTER_UNEXP_HANDLER */
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "free_list_num",
                            "Number of allocated default request",
                            false, false, 8, &mca_btl_mx_component.mx_free_list_num );
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "free_list_inc",
                            "Number of request we allocate each time we miss some",
                            false, false, 32, &mca_btl_mx_component.mx_free_list_inc );
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "free_list_max",
                            "Maximum number of request this device is allowed to allocate",
                            false, false, 1024, &mca_btl_mx_component.mx_free_list_max );

    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "max_posted_recv",
                            "Number of received posted in advance. Increasing this number for"
			    " communication bound application can lead to visible improvement"
			    " in performances",
                            false, false, 16, &mca_btl_mx_component.mx_max_posted_recv );

#if MX_HAVE_MAPPER_STATE
    mca_base_param_reg_string( (mca_base_component_t*)&mca_btl_mx_component, "if_include",
			       "Myrinet card to use (last 6 digits from the mapper MAC)",
			       false, false, NULL, &mca_btl_mx_component.mx_if_include );
    mca_base_param_reg_string( (mca_base_component_t*)&mca_btl_mx_component, "if_exclude",
			       "Myrinet card to avoid (last 6 digits from the mapper MAC)",
			       false, false, NULL, &mca_btl_mx_component.mx_if_exclude );
#endif  /* MX_HAVE_MAPPER_STATE */

    mca_btl_mx_module.super.btl_exclusivity = MCA_BTL_EXCLUSIVITY_DEFAULT;
    if( mca_btl_mx_component.mx_use_unexpected ) {
        mca_btl_mx_module.super.btl_eager_limit = 1024;
        mca_btl_mx_module.super.btl_rndv_eager_limit = 1024;
    } else {
        mca_btl_mx_module.super.btl_eager_limit = 4*1024;
        mca_btl_mx_module.super.btl_rndv_eager_limit = 4*1024;
    }
    mca_btl_mx_module.super.btl_max_send_size = 32*1024;
    mca_btl_mx_module.super.btl_rdma_pipeline_send_length = 256*1024;
    mca_btl_mx_module.super.btl_rdma_pipeline_frag_size = 8*1024*1024;
    mca_btl_mx_module.super.btl_min_rdma_pipeline_size = 0;
    mca_btl_mx_module.super.btl_flags = (MCA_BTL_FLAGS_SEND_INPLACE |
                                         MCA_BTL_FLAGS_PUT |
                                         MCA_BTL_FLAGS_SEND |
                                         MCA_BTL_FLAGS_RDMA_MATCHED);
    mca_btl_mx_module.super.btl_bandwidth = 2000;
    mca_btl_mx_module.super.btl_latency = 5;
    mca_btl_base_param_register(&mca_btl_mx_component.super.btl_version,
                                &mca_btl_mx_module.super);

    return OMPI_SUCCESS;
}


/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */

static int mca_btl_mx_component_open(void)
{
    /* initialize state */
    mca_btl_mx_component.mx_num_btls = 0;
    mca_btl_mx_component.mx_btls = NULL;
    mca_btl_mx_component.mx_use_unexpected = 0;

    /* initialize objects */ 
    OBJ_CONSTRUCT(&mca_btl_mx_component.mx_procs, opal_list_t);
    if( 0 == mca_btl_mx_component.mx_support_sharedmem )
        opal_setenv( "MX_DISABLE_SHMEM", "1", true, &environ );
    if( 0 == mca_btl_mx_component.mx_support_self )
        opal_setenv( "MX_DISABLE_SELF", "1", true, &environ );
    /* Force the long pipeline (up to 4Kb fragments) */
    opal_setenv( "MX_PIPELINE_LOG", "0", true, &environ );

    return OMPI_SUCCESS;
}

/*
 * component cleanup - sanity checking of queue lengths
 */

static int mca_btl_mx_component_close(void)
{
   
    if(OMPI_SUCCESS != ompi_common_mx_finalize()) { 
        return OMPI_ERROR;
    }

    if( NULL == mca_btl_mx_component.mx_btls )
        return OMPI_SUCCESS;

    /* release resources */
    OBJ_DESTRUCT(&mca_btl_mx_component.mx_procs);
    OBJ_DESTRUCT(&mca_btl_mx_component.mx_send_eager_frags);
    OBJ_DESTRUCT(&mca_btl_mx_component.mx_send_user_frags);
    OBJ_DESTRUCT(&mca_btl_mx_component.mx_procs);
    OBJ_DESTRUCT(&mca_btl_mx_component.mx_lock);

#if MX_HAVE_MAPPER_STATE
    if( NULL != mca_btl_mx_component.mx_if_include ) {
        free( mca_btl_mx_component.mx_if_include );
	mca_btl_mx_component.mx_if_include = NULL;
    }
    if( NULL != mca_btl_mx_component.mx_if_exclude ) {
        free( mca_btl_mx_component.mx_if_exclude );
	mca_btl_mx_component.mx_if_exclude = NULL;
    }
#endif  /* MX_HAVE_MAPPER_STATE */
    return OMPI_SUCCESS;
}

static int __counter = 0;
#ifdef HAVE_MX_REGISTER_UNEXP_HANDLER

/**
 * In order to avoid useless memcpy, the unexpected handler will be called
 * by the MX library before doing any match in the MX internal queues. Here
 * we have a chance to match the message using our own matching logic from
 * the PML. If the match is realized, we will return MX_RECV_FINISHED (the
 * MX request will vanish in the MX library). If the match do not succeed
 * MX_RECV_CONTINUE have to be returned and the MX library will do the
 * match itself.
 */
static mx_unexp_handler_action_t
mca_btl_mx_unexpected_handler( void *context, mx_endpoint_addr_t source,
                               uint64_t match_value, uint32_t length,
                               void * data_if_available )
{
    mca_btl_mx_module_t* mx_btl = (mca_btl_mx_module_t*)context;
    mca_btl_active_message_callback_t* reg;
    mca_btl_base_tag_t tag;
    mca_btl_base_descriptor_t descriptor;
    mca_btl_base_segment_t segment;

    if( 0 == __counter ) {
        return MX_RECV_CONTINUE;
    }
    /*opal_output( 0, "Get unexpected handler context %p source %lld match_value %lld\n"
      "\tlength %d data %p\n", context, source.stuff[0], match_value, length,
      data_if_available );*/
    if( !(0x01 & match_value) ) {
        return MX_RECV_CONTINUE;
    }

    tag = (match_value >> 8) & 0xff;
    reg = mca_btl_base_active_message_trigger + tag;

    segment.seg_addr.pval = data_if_available;
    segment.seg_len = length;
    descriptor.des_dst = &segment;
    descriptor.des_dst_cnt = 1;
    reg->cbfunc( &(mx_btl->super), tag, &descriptor, reg->cbdata );

    return MX_RECV_FINISHED;
}
#endif  /* HAVE_MX_REGISTER_UNEXP_HANDLER */

/*
 * Create and initialize an MX BTL module, where each module
 * represents a specific NIC or a specific bonded set of NICS.
 */
static mca_btl_mx_module_t* mca_btl_mx_create(uint32_t board_num)
{
    mca_btl_mx_module_t* mx_btl;
    mx_endpoint_t mx_endpoint;
    mx_endpoint_addr_t mx_endpoint_addr;
    mx_return_t status;
    uint32_t endpoint_id, mx_unique_network_id = 0;
    uint64_t nic_id;

    /* open local endpoint */
    status = mx_open_endpoint( board_num, MX_ANY_ENDPOINT,
                               mca_btl_mx_component.mx_filter,
                               NULL, 0, &mx_endpoint);
    if(status != MX_SUCCESS) {
        opal_output( 0, "mca_btl_mx_init: mx_open_endpoint() failed with status %d (%s)\n",
                     status, mx_strerror(status) );
        return NULL;
    }

    /* query the endpoint address */
    if((status = mx_get_endpoint_addr( mx_endpoint,
                                       &mx_endpoint_addr)) != MX_SUCCESS) {
        opal_output( 0, "mca_btl_mx_init: mx_get_endpoint_addr() failed with status %d (%s)\n",
                     status, mx_strerror(status) );
        mx_close_endpoint(mx_endpoint);
        return NULL;
    }

    status = mx_decompose_endpoint_addr(mx_endpoint_addr, &nic_id, &endpoint_id);
    if( MX_SUCCESS != status ) {
        opal_output( 0, "mca_btl_mx_init: mx_decompose_endpoint_addr() failed with status %d (%s)\n",
                     status, mx_strerror(status) );
        mx_close_endpoint(mx_endpoint);
        return NULL;
    }
    status = mx_nic_id_to_board_number(nic_id, &board_num);
    if( MX_SUCCESS != status ) {
        opal_output( 0, "mca_btl_mx_init: mx_nic_id_to_board_number() failed with status %d (%s)\n",
                     status, mx_strerror(status) );
        mx_close_endpoint(mx_endpoint);
        return NULL;
    }

#if MX_HAVE_MAPPER_STATE
    {
        mx_endpt_handle_t endp_handle;
        mx_mapper_state_t ms;
        char mapper_mac[7], *where;

        status = mx_open_board( board_num, &endp_handle );
        if( MX_SUCCESS != status ) {
            opal_output( 0, "Unable to open board %d: %s\n", board_num, mx_strerror(status) );
            mx_close_endpoint(mx_endpoint);
            return NULL;
        }

        ms.board_number = board_num;
        ms.iport = 0;
        status = mx__get_mapper_state( endp_handle, &ms );
        if( MX_SUCCESS != status ) {
            opal_output( 0, "get_mapper_state failed for board %d: %s\n",
                         board_num, mx_strerror(status) );
            mx_close_endpoint(mx_endpoint);
            return NULL;
        }
        /* Keep the first 4 bytes for the network speed */
        mx_unique_network_id = ((ms.mapper_mac[3] << 16) +
				(ms.mapper_mac[4] << 8)  +
				(ms.mapper_mac[5]));

        /* Try to figure out if we are allowed to use this network */
        snprintf( mapper_mac, 7, "%6x", mx_unique_network_id );

        if( (NULL != mca_btl_mx_component.mx_if_exclude) &&
            (NULL != (where = strstr(mca_btl_mx_component.mx_if_exclude, mapper_mac))) ) {
            mx_close_endpoint(mx_endpoint);
            return NULL;
        }
        else if( (NULL != mca_btl_mx_component.mx_if_include) &&
                 (NULL == (where = strstr(mca_btl_mx_component.mx_if_include, mapper_mac))) ) {
            mx_close_endpoint(mx_endpoint);
            return NULL;
        }
    }
#endif  /* MX_HAVE_MAPPER_STATE */

    mx_btl = malloc(sizeof(mca_btl_mx_module_t));
    if( NULL == mx_btl ) {
        opal_output( 0, "mca_btl_mx_init: unable to allocate %lu bytes of memory\n",
                     sizeof(mca_btl_mx_module_t) );
        mx_close_endpoint(mx_endpoint);
        return NULL;
    }

    /* copy over default settings */
    memcpy( mx_btl, &mca_btl_mx_module, sizeof(mca_btl_mx_module_t) );
    OBJ_CONSTRUCT( &mx_btl->mx_peers, opal_list_t );
    OBJ_CONSTRUCT( &mx_btl->mx_lock, opal_mutex_t );
    mx_btl->mx_endpoint = mx_endpoint;
    mx_btl->mx_endpoint_addr = mx_endpoint_addr;

    mx_btl->super.btl_bandwidth = 2000;  /* whatever */
    mx_btl->super.btl_latency = 10;
#if defined(MX_HAS_NET_TYPE)
    {
        int value, board = board_num;
        if( (status = mx_get_info( mx_btl->mx_endpoint, MX_LINE_SPEED,
                                   &board, sizeof(board),
                                   &value, sizeof(int))) != MX_SUCCESS ) {
            opal_output( 0, "mx_get_info(MX_LINE_SPEED) failed with status %d (%s)\n",
                         status, mx_strerror(status) );
        } else {
            if( MX_SPEED_2G == value ) {
                mx_unique_network_id |= 0xaa000000;
                mx_btl->super.btl_bandwidth = 2000;
                mx_btl->super.btl_latency = 5;
            } else if( MX_SPEED_10G == value ) {
                mx_unique_network_id |= 0xbb000000;
                mx_btl->super.btl_bandwidth = 10000;
                mx_btl->super.btl_latency = 3;
            } else {
                mx_unique_network_id |= 0xcc000000;
            }
        }
    }
#endif  /* defined(MX_HAS_NET_TYPE) */
    mx_btl->super.btl_bandwidth *= mca_btl_mx_component.mx_bonding;
    mx_btl->mx_unique_network_id = mx_unique_network_id;

#ifdef HAVE_MX_REGISTER_UNEXP_HANDLER
    if( mca_btl_mx_component.mx_use_unexpected ) {
        status = mx_register_unexp_handler( mx_btl->mx_endpoint, mca_btl_mx_unexpected_handler,
                                            (void*)mx_btl );
        if( MX_SUCCESS != status ) {
            opal_output( 0, "mca_btl_mx_init: mx_register_unexp_handler() failed with status %d (%s)\n",
                         status, mx_strerror(status) );
            /* switch to a mode without the unexpected handler */
            mca_btl_mx_component.mx_use_unexpected = 0;
        }
    }
#endif  /* HAVE_MX_REGISTER_UNEXP_HANDLER */
    return mx_btl;
}

/*
 *  MX component initialization:
 *  - check if MX can be initialized.
 *  - and construct all static objects.
 */

mca_btl_base_module_t** mca_btl_mx_component_init(int *num_btl_modules, 
                                                  bool enable_progress_threads,
                                                  bool enable_mpi_threads)
{
    mca_btl_base_module_t** btls;
    mx_return_t status;
    uint32_t count;
    int32_t i;
    mca_btl_mx_addr_t *mx_addrs;

    *num_btl_modules = 0;

    /* First check if MX is available ... */
    if( OMPI_SUCCESS != ompi_common_mx_initialize() ) { 
        ompi_modex_send(&mca_btl_mx_component.super.btl_version, NULL, 0);
        return NULL;
    }
        
    /* get the number of card available on the system */
    if( MX_SUCCESS != (status = mx_get_info( NULL, MX_NIC_COUNT, NULL, 0,
                                             &mca_btl_mx_component.mx_num_btls,
                                             sizeof(uint32_t))) ) {
        opal_output( 0, "mca_btl_mx_component_init: mx_get_info(MX_NIC_COUNT) failed with status %d(%s)\n",
                     status, mx_strerror(status) );
        return NULL;
    }
    /* Don't forget the bonding rules ... */
    assert( mca_btl_mx_component.mx_bonding >= 1 );
    mca_btl_mx_component.mx_num_btls /= mca_btl_mx_component.mx_bonding;

    if (0 == mca_btl_mx_component.mx_num_btls) {
        mca_btl_base_error_no_nics("Myrinet/MX", "NIC");
        return NULL;
    }
    /* Limit ourselves to the number of devices requested by the users. */
    if( mca_btl_mx_component.mx_num_btls > mca_btl_mx_component.mx_max_btls ) {
        mca_btl_mx_component.mx_num_btls = mca_btl_mx_component.mx_max_btls;
    }

    /* initialize objects */
    OBJ_CONSTRUCT(&mca_btl_mx_component.mx_send_eager_frags, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_mx_component.mx_send_user_frags, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_mx_component.mx_procs, opal_list_t);
    OBJ_CONSTRUCT(&mca_btl_mx_component.mx_lock, opal_mutex_t);

    ompi_free_list_init_new( &mca_btl_mx_component.mx_send_eager_frags,
                         sizeof(mca_btl_mx_frag_t) + mca_btl_mx_module.super.btl_eager_limit,
                         opal_cache_line_size,
                         OBJ_CLASS(mca_btl_mx_frag_t),
                         0,opal_cache_line_size,
                         mca_btl_mx_component.mx_free_list_num,
                         mca_btl_mx_component.mx_free_list_max,
                         mca_btl_mx_component.mx_free_list_inc,
                         NULL ); /* use default allocator */

    ompi_free_list_init_new( &mca_btl_mx_component.mx_send_user_frags,
                         sizeof(mca_btl_mx_frag_t),
                         opal_cache_line_size,
                         OBJ_CLASS(mca_btl_mx_frag_t),
                         0,opal_cache_line_size,
                         mca_btl_mx_component.mx_free_list_num,
                         mca_btl_mx_component.mx_free_list_max,
                         mca_btl_mx_component.mx_free_list_inc,
                         NULL ); /* use default allocator */

    /* intialize process hash table */
    OBJ_CONSTRUCT( &mca_btl_mx_component.mx_procs, opal_list_t );

    /* Now we know how many NIC are available on the system. We will create a BTL
     * for each one and then give a pointer to the BTL to the upper level.
     */
    mca_btl_mx_component.mx_btls = malloc( mca_btl_mx_component.mx_num_btls *
                                           sizeof(mca_btl_base_module_t*) );
    if( NULL == mca_btl_mx_component.mx_btls ) {
        opal_output( 0, "MX BTL unable to allocate memory\n" );
        return NULL;
    }

    mx_addrs = (mca_btl_mx_addr_t*)calloc( mca_btl_mx_component.mx_num_btls,
                                           sizeof(mca_btl_mx_addr_t) );
    if( NULL == mx_addrs ) {
        opal_output( 0, "MX BTL unable to allocate memory\n" );
        free(mca_btl_mx_component.mx_btls);
        mca_btl_mx_component.mx_btls = NULL;
        return NULL;
    }

    /* create a btl for each NIC */
    for( i = count = 0; i < mca_btl_mx_component.mx_num_btls; i++ ) {
        mca_btl_mx_module_t* mx_btl = mca_btl_mx_create(MX_ANY_NIC);
        if( NULL == mx_btl ) {
            continue;
        }
        status = mx_decompose_endpoint_addr( mx_btl->mx_endpoint_addr,
                                             &(mx_addrs[count].nic_id),
                                             &(mx_addrs[count].endpoint_id) );
        if( MX_SUCCESS != status ) {
            mca_btl_mx_finalize( &mx_btl->super );
            continue;
        }
        mx_addrs[count].unique_network_id = mx_btl->mx_unique_network_id;

#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
        BTL_MX_ADDR_HTON(mx_addrs[count]);
#endif
        mca_btl_mx_component.mx_btls[count] = mx_btl;
        count++;  /* one more succesfully initialized MX interface */
    }
    mca_btl_mx_component.mx_num_btls = count;
    *num_btl_modules = count;
    if( 0 == count ) {
        /* No active BTL module */
        free(mx_addrs);
        free(mca_btl_mx_component.mx_btls);
        mca_btl_mx_component.mx_btls = NULL;
        return NULL;
    }

    /* publish the MX addresses via the MCA framework */
    ompi_modex_send(&mca_btl_mx_component.super.btl_version, mx_addrs,
                    sizeof(mca_btl_mx_addr_t) * mca_btl_mx_component.mx_num_btls);

    free( mx_addrs );

    btls = malloc( mca_btl_mx_component.mx_num_btls * sizeof(mca_btl_base_module_t*) );
    if( NULL == btls ) {
        free( mca_btl_mx_component.mx_btls );
        mca_btl_mx_component.mx_num_btls = 0;  /* no active BTL modules */
        return NULL;
    }
    memcpy( btls,  mca_btl_mx_component.mx_btls,
            mca_btl_mx_component.mx_num_btls*sizeof(mca_btl_mx_module_t*) );

    return btls;
}

/*
 *  MX component progress.
 */
int mca_btl_mx_component_progress(void)
{
    int32_t num_progressed = 0, i;
    mx_status_t mx_status;
    mx_return_t mx_return;
    mx_request_t mx_request;
    mca_btl_mx_frag_t* frag;

    ++__counter;
    for( i = 0; i < mca_btl_mx_component.mx_num_btls; i++ ) {
        mca_btl_mx_module_t* mx_btl = mca_btl_mx_component.mx_btls[i];
        uint32_t mx_result = 0;

      recheck_device:
        mx_return = mx_ipeek( mx_btl->mx_endpoint, &mx_request, &mx_result );
        if( OPAL_UNLIKELY(mx_return != MX_SUCCESS) ) {
            opal_output( 0, "mca_btl_mx_component_progress: mx_ipeek() failed with status %d (%s)\n",
                         mx_return, mx_strerror(mx_return) );
            continue;
        }
        if( OPAL_LIKELY(mx_result == 0) ) {
            continue;
        }
        
        mx_return = mx_test( mx_btl->mx_endpoint, &mx_request, &mx_status, &mx_result);
        if( OPAL_UNLIKELY(mx_return != MX_SUCCESS) ) {
            opal_output(0, "mca_btl_mx_progress: mx_test() failed with status %d (%s)\n",
                        mx_return, mx_strerror(mx_return));
            continue;
        }
        /* on the mx_status we have now the pointer attached to the request.
         * This pointer indicate which fragment we are working on. On the
         * status we have the status of the operation, so we know what we
         * are supposed to do next.
         */
        frag = mx_status.context;
        num_progressed++;
        /* If the context is NULL then we are facing a send immediate request. Therefore,
         * nothing special should be done, just keep going.
         */
        if( NULL == frag ) goto recheck_device;
        if( MCA_BTL_MX_SEND == frag->type ) {  /* it's a send */
            int btl_ownership = (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
            /* call the completion callback */
            if( MCA_BTL_DES_SEND_ALWAYS_CALLBACK & frag->base.des_flags ) {
                frag->base.des_cbfunc( &(mx_btl->super), frag->endpoint,
                                       &(frag->base), OMPI_SUCCESS );
            }
            if( btl_ownership ) {
                MCA_BTL_MX_FRAG_RETURN( mx_btl, frag );
            }
            goto recheck_device;
        } else {
            mca_btl_active_message_callback_t* reg;
            mx_segment_t mx_segment;
            uint8_t tag = (mx_status.match_info >> 8) & 0xff;
            
            reg = mca_btl_base_active_message_trigger + tag;
            frag->base.des_dst->seg_len = mx_status.msg_length;
            reg->cbfunc( &(mx_btl->super), tag, &(frag->base), reg->cbdata );
            /**
             * The upper level extract the data from the fragment.
             * Now we can register the fragment
             * again with the MX BTL.
             */
            mx_segment.segment_ptr = frag->base.des_dst->seg_addr.pval;
            mx_segment.segment_length = mca_btl_mx_module.super.btl_eager_limit;
            mx_return = mx_irecv( mx_btl->mx_endpoint, &mx_segment, 1,
                                  0x01ULL, BTL_MX_RECV_MASK,
                                  frag, &(frag->mx_request) );
            if( MX_SUCCESS != mx_return ) {
                opal_output( 0, "Fail to re-register a fragment with the MX NIC ... (%s)\n",
                             mx_strerror(mx_return) );
            }
        }
    }
    __counter--;
    return num_progressed;
}

#if 0
    {
        int counters, board, i, value, *counters_value;
        char text[MX_MAX_STR_LEN];
        char *counters_name;
        if( (status = mx_get_info( mx_btl->mx_endpoint, MX_PIO_SEND_MAX, NULL, 0,
                                   &value, sizeof(int))) != MX_SUCCESS ) {
            opal_output( 0, "mx_get_info(MX_PIO_SEND_MAX) failed with status %d (%s)\n",
                         status, mx_strerror(status) );
        }
        printf( "MX_PIO_SEND_MAX = %d\n", value );
        if( (status = mx_get_info( mx_btl->mx_endpoint, MX_COPY_SEND_MAX, NULL, 0,
                                   &value, sizeof(int))) != MX_SUCCESS ) {
            opal_output( 0, "mx_get_info(MX_COPY_SEND_MAX) failed with status %d (%s)\n",
                         status, mx_strerror(status) );
        }
        printf( "MX_COPY_SEND_MAX = %d\n", value );

        board = 0;
        if( (status = mx_get_info( mx_btl->mx_endpoint, MX_PRODUCT_CODE, &board, sizeof(int),
                                   text, MX_MAX_STR_LEN)) != MX_SUCCESS ) {
            opal_output( 0, "mx_get_info(MX_PRODUCT_CODE) failed with status %d (%s)\n",
                         status, mx_strerror(status) );
        }
        printf( "product code %s\n", text );

        if( (status = mx_get_info( mx_btl->mx_endpoint, MX_COUNTERS_COUNT, &board, sizeof(int),
                                   &counters, sizeof(int))) != MX_SUCCESS ) {
            opal_output( 0, "mx_get_info(MX_COUNTERS_COUNT) failed with status %d (%s)\n",
                         status, mx_strerror(status) );
        }
        printf( "counters = %d\n", counters );
        counters_name = (char*)malloc( counters * MX_MAX_STR_LEN );
        if( (status = mx_get_info( mx_btl->mx_endpoint, MX_COUNTERS_LABELS, &board, sizeof(int),
                                   counters_name, counters * MX_MAX_STR_LEN)) != MX_SUCCESS ) {
            opal_output( 0, "mx_get_info(MX_COUNTERS_LABELS) failed with status %d (%s)\n",
                         status, mx_strerror(status) );
        }
        counters_value = (int*)malloc( counters * sizeof(int) );
        if( (status = mx_get_info( mx_btl->mx_endpoint, MX_COUNTERS_VALUES, &board, sizeof(int),
                                   counters_value, counters * sizeof(int))) != MX_SUCCESS ) {
            opal_output( 0, "mx_get_info(MX_COUNTERS_VALUES) failed with status %d (%s)\n",
                         status, mx_strerror(status) );
        }
        for( i = 0; i < counters; i++ )
            printf( "%d -> %s = %d\n", i, counters_name + i * MX_MAX_STR_LEN,
                    counters_value[i] );
        free( counters_name );
        free( counters_value );
    }
#endif
