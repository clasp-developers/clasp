/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "opal/event/event.h"
#include "mpi.h"
#include "ompi/mca/bml/bml.h"
#include "ompi/mca/btl/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "bml_r2.h"


mca_bml_base_component_2_0_0_t mca_bml_r2_component = {

    /* First, the mca_base_component_t struct containing meta
       information about the component itself */

    {
        MCA_BML_BASE_VERSION_2_0_0,

        "r2", /* MCA component name */
        OMPI_MAJOR_VERSION,  /* MCA component major version */
        OMPI_MINOR_VERSION,  /* MCA component minor version */
        OMPI_RELEASE_VERSION,  /* MCA component release version */
        mca_bml_r2_component_open,  /* component open */
        mca_bml_r2_component_close  /* component close */
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    mca_bml_r2_component_init
};
                                                                                                                        

int mca_bml_r2_component_open(void)
{
    int tmp;

    mca_base_param_reg_int(&mca_bml_r2_component.bml_version,
                           "show_unreach_errors",
                           "Show error message when procs are unreachable",
                           false,
                           false,
                           1,
                           &tmp);
    mca_bml_r2.show_unreach_errors = OPAL_INT_TO_BOOL(tmp);


    return OMPI_SUCCESS; 
}


int mca_bml_r2_component_close(void)
{
        
    /* OBJ_DESTRUCT(&mca_bml_r2.lock); */

    return OMPI_SUCCESS;
}


mca_bml_base_module_t* mca_bml_r2_component_init( int* priority, 
                                                  bool enable_progress_threads,
                                                  bool enable_mpi_threads )
{
    /* initialize BTLs */
    
    if(OMPI_SUCCESS != mca_btl_base_select(enable_progress_threads,enable_mpi_threads))
        return NULL;
    
    *priority = 100; 
    mca_bml_r2.btls_added = false; 
    return &mca_bml_r2.super;
}
