/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */

#include "ompi/constants.h"
#include "opal/runtime/opal.h"
#include "opal/event/event.h"
#include "opal/mca/base/mca_base_param.h"
#include "btl_self.h"
#include "btl_self_frag.h"



/*
 * Shared Memory (SELF) component instance. 
 */

mca_btl_self_component_t mca_btl_self_component = {
    {  /* super is being filled in */
        /* First, the mca_base_component_t struct containing meta information
          about the component itself */
        {
            MCA_BTL_BASE_VERSION_2_0_0,

            "self", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            mca_btl_self_component_open,  /* component open */
            mca_btl_self_component_close  /* component close */
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        mca_btl_self_component_init,  
        NULL,
    }  /* end super */
};

/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */

int mca_btl_self_component_open(void)
{
    /* register SELF component parameters */
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_self_component, "free_list_num",
                            "Number of fragments by default", false, false,
                            0, &mca_btl_self_component.free_list_num );
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_self_component, "free_list_max",
                            "Maximum number of fragments", false, false,
                            -1, &mca_btl_self_component.free_list_max );
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_self_component, "free_list_inc",
                            "Increment by this number of fragments", false, false,
                            32, &mca_btl_self_component.free_list_inc );

    mca_btl_self.btl_exclusivity = MCA_BTL_EXCLUSIVITY_HIGH;
    mca_btl_self.btl_eager_limit = 128 * 1024;
    mca_btl_self.btl_rndv_eager_limit = 128 * 1024;
    mca_btl_self.btl_max_send_size = 256 * 1024;
    mca_btl_self.btl_rdma_pipeline_send_length = INT_MAX;
    mca_btl_self.btl_rdma_pipeline_frag_size = INT_MAX;
    mca_btl_self.btl_min_rdma_pipeline_size = 0;
    mca_btl_self.btl_flags = MCA_BTL_FLAGS_PUT | MCA_BTL_FLAGS_SEND_INPLACE;
    mca_btl_self.btl_bandwidth = 100;
    mca_btl_self.btl_latency = 0;
    mca_btl_base_param_register(&mca_btl_self_component.super.btl_version,
            &mca_btl_self);

    /* initialize objects */
    OBJ_CONSTRUCT(&mca_btl_self_component.self_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_btl_self_component.self_frags_eager, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_self_component.self_frags_send, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_self_component.self_frags_rdma, ompi_free_list_t);
    return OMPI_SUCCESS;
}


/*
 * component cleanup - sanity checking of queue lengths
 */

int mca_btl_self_component_close(void)
{
    OBJ_DESTRUCT(&mca_btl_self_component.self_lock);
    OBJ_DESTRUCT(&mca_btl_self_component.self_frags_eager);
    OBJ_DESTRUCT(&mca_btl_self_component.self_frags_send);
    OBJ_DESTRUCT(&mca_btl_self_component.self_frags_rdma);
    return OMPI_SUCCESS;
}


/*
 *  SELF component initialization
 */
mca_btl_base_module_t** mca_btl_self_component_init( int *num_btls, 
                                                     bool enable_progress_threads,
                                                     bool enable_mpi_threads )
{
    mca_btl_base_module_t **btls = NULL;
    *num_btls = 0;

    /* allocate the Shared Memory PTL */
    *num_btls = 1;
    btls = (mca_btl_base_module_t**)malloc((*num_btls)*sizeof(mca_btl_base_module_t*));
    if (NULL == btls) {
        return NULL;
    }

    /* initialize free lists */
    ompi_free_list_init_new( &mca_btl_self_component.self_frags_eager, 
                         sizeof(mca_btl_self_frag_eager_t) + mca_btl_self.btl_eager_limit,
                         opal_cache_line_size,
                         OBJ_CLASS(mca_btl_self_frag_eager_t),
                         0,opal_cache_line_size,
                         mca_btl_self_component.free_list_num,
                         mca_btl_self_component.free_list_max,
                         mca_btl_self_component.free_list_inc,
                         NULL );
    ompi_free_list_init_new( &mca_btl_self_component.self_frags_send, 
                         sizeof(mca_btl_self_frag_send_t) + mca_btl_self.btl_max_send_size,
                         opal_cache_line_size,
                         OBJ_CLASS(mca_btl_self_frag_send_t),
                         0,opal_cache_line_size,
                         mca_btl_self_component.free_list_num,
                         mca_btl_self_component.free_list_max,
                         mca_btl_self_component.free_list_inc,
                         NULL );
    ompi_free_list_init_new( &mca_btl_self_component.self_frags_rdma, 
                         sizeof(mca_btl_self_frag_rdma_t),
                         opal_cache_line_size,
                         OBJ_CLASS(mca_btl_self_frag_rdma_t),
                         0,opal_cache_line_size,
                         mca_btl_self_component.free_list_num,
                         mca_btl_self_component.free_list_max,
                         mca_btl_self_component.free_list_inc,
                         NULL );

    /* get pointer to the btls */
    btls[0] = (mca_btl_base_module_t *)(&mca_btl_self);
    return btls;
}

