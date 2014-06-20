/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/event/event.h"
#include "opal/mca/base/mca_base_param.h"
#include "ompi/mca/mtl/base/base.h"
#include "ompi/mca/common/mx/common_mx.h"

#include "mtl_mx.h"
#include "mtl_mx_types.h"
#include "mtl_mx_request.h"

#include "myriexpress.h"

#define  MCA_MTL_MX_QUEUE_LENGTH_MAX 2*1024*1024
static int ompi_mtl_mx_component_open(void);
static int ompi_mtl_mx_component_close(void);
static int ompi_mtl_mx_component_initialized = 0;

static mca_mtl_base_module_t* ompi_mtl_mx_component_init( bool enable_progress_threads, 
                                                          bool enable_mpi_threads );

mca_mtl_mx_component_t mca_mtl_mx_component = {

    {
        /* First, the mca_base_component_t struct containing meta
         * information about the component itself */
        
        {
            MCA_MTL_BASE_VERSION_2_0_0,
            
            "mx", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            ompi_mtl_mx_component_open,  /* component open */
            ompi_mtl_mx_component_close  /* component close */
        },
        {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },
        
        ompi_mtl_mx_component_init  /* component init */
    }
};

    
static int
ompi_mtl_mx_component_open(void)
{
        
    
    mca_base_param_reg_int(&mca_mtl_mx_component.super.mtl_version, "filter",
                           "user assigned value used to filter incomming messages",
                           false, false, 0xaaaaffff, &ompi_mtl_mx.mx_filter);
    
    mca_base_param_reg_int(&mca_mtl_mx_component.super.mtl_version, "timeout",
                           "Timeout for connections",
                           false, false, MX_INFINITE, &ompi_mtl_mx.mx_timeout);
    
    mca_base_param_reg_int(&mca_mtl_mx_component.super.mtl_version, "retries",
                           "Number of retries for each new connection before considering the peer as unreacheable",
                           false, false, 20, &ompi_mtl_mx.mx_retries);
    
    mca_base_param_reg_int(&mca_mtl_mx_component.super.mtl_version, "shared_mem",
                           "Enable the MX support for shared memory",
                           false, true, 1, &ompi_mtl_mx.mx_support_sharedmem );
    
    mca_base_param_reg_int(&mca_mtl_mx_component.super.mtl_version, "unexpected_queue_length", 
                           "Length of MX unexpected message queue", 
                           false, false, MCA_MTL_MX_QUEUE_LENGTH_MAX, &ompi_mtl_mx.mx_unexp_queue_max);
    
    if(ompi_mtl_mx.mx_unexp_queue_max >  MCA_MTL_MX_QUEUE_LENGTH_MAX) { 
        ompi_mtl_mx.mx_unexp_queue_max =  MCA_MTL_MX_QUEUE_LENGTH_MAX; 
    }

    mca_base_param_reg_int(&mca_mtl_mx_component.super.mtl_version, "board",
                           "Which MX board number to use (<0 = any)",
                           false, false, -1, &ompi_mtl_mx.mx_board_num);
    mca_base_param_reg_int(&mca_mtl_mx_component.super.mtl_version, "endpoint",
                           "Which MX endpoint number to use (<0 = any)",
                           false, false, -1, &ompi_mtl_mx.mx_endpoint_num);
    
    return OMPI_SUCCESS;
}


static int
ompi_mtl_mx_component_close(void)
{
    --ompi_mtl_mx_component_initialized;
    if( 0 == ompi_mtl_mx_component_initialized ) {
        int ret = ompi_common_mx_finalize();
        if(OMPI_SUCCESS != ret) { 
            return NULL;
        }
     }
    return OMPI_SUCCESS;
}


static mca_mtl_base_module_t*
ompi_mtl_mx_component_init(bool enable_progress_threads,
                           bool enable_mpi_threads)
{
   int ret;
    
    ret = ompi_common_mx_initialize();
    if(OMPI_SUCCESS != ret) { 
        return NULL;
    }
    ompi_mtl_mx_component_initialized++;

    ret = ompi_mtl_mx_module_init();
    if (OMPI_SUCCESS != ret) {
        return NULL;
    }

    ompi_mtl_mx.super.mtl_request_size = 
        sizeof(mca_mtl_mx_request_t) - 
        sizeof(struct mca_mtl_request_t);
    
    return &ompi_mtl_mx.super;
}

