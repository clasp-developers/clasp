/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
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
#include "opal/runtime/opal.h"
#include "opal/util/output.h"
#include "opal/event/event.h"


#include "mpi.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "ompi/mca/pml/base/pml_base_bsend.h"
#include "ompi/mca/pml/crcpw/pml_crcpw.h"
#include "ompi/mca/bml/base/base.h"


mca_pml_crcpw_component_t mca_pml_crcpw_component = {
    {
        /* First, the mca_base_component_t struct containing meta
           information about the component itself */
        
        {
            MCA_PML_BASE_VERSION_2_0_0,
    
            "crcpw", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            mca_pml_crcpw_component_open,  /* component open */
            mca_pml_crcpw_component_close  /* component close */
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
        
        mca_pml_crcpw_component_init,    /* component init */
        mca_pml_crcpw_component_finalize /* component finalize */
    },
    /* Verbosity */
    0,
    /* Priority */
    PML_SELECT_WRAPPER_PRIORITY,
    /* Are we being used as a wrapper? */
    false
};

ompi_free_list_t pml_state_list;
bool pml_crcpw_is_finalized = false;

int mca_pml_crcpw_component_open(void)
{
    opal_output_verbose( 10, mca_pml_crcpw_component.output_handle,
                         "pml:crcpw: component_open: Open");

    /*
     * Register some MCA parameters
     */
    mca_base_param_reg_int(&mca_pml_crcpw_component.super.pmlm_version,
                           "priority",
                           "Priority of the PML crcpw component",
                           false, false,
                           mca_pml_crcpw_component.priority,
                           &mca_pml_crcpw_component.priority);
    
    mca_base_param_reg_int(&mca_pml_crcpw_component.super.pmlm_version,
                           "verbose",
                           "Verbose level for the PML crcpw component",
                           false, false,
                           mca_pml_crcpw_component.verbose, 
                           &mca_pml_crcpw_component.verbose);

    mca_pml_crcpw_component.output_handle = opal_output_open(NULL);
    if ( 0 != mca_pml_crcpw_component.verbose) {
        opal_output_set_verbosity(mca_pml_crcpw_component.output_handle,
                                  mca_pml_crcpw_component.verbose);
    }
    
    /*
     * Debug Output
     */
    opal_output_verbose(10, mca_pml_crcpw_component.output_handle,
                        "pml:crcpw: open()");
    opal_output_verbose(20, mca_pml_crcpw_component.output_handle,
                        "pml:crcpw: open: priority   = %d", 
                        mca_pml_crcpw_component.priority);
    opal_output_verbose(20, mca_pml_crcpw_component.output_handle,
                        "pml:crcpw: open: verbosity  = %d", 
                        mca_pml_crcpw_component.verbose);

    return OMPI_SUCCESS;
}


int mca_pml_crcpw_component_close(void)
{
    opal_output_verbose( 20, mca_pml_crcpw_component.output_handle,
                         "pml:crcpw: component_close: Close");

    return OMPI_SUCCESS;
}


mca_pml_base_module_t* mca_pml_crcpw_component_init(int* priority, 
                                                     bool enable_progress_threads,
                                                     bool enable_mpi_threads)
{
    /* We use the PML_SELECT_WRAPPER_PRIORITY to indicate when this 
     * component should wrap around what is already selected
     * If it is not set to this seminal value, then we are doing a 
     * normal selection operation
     */
    if(*priority == PML_SELECT_WRAPPER_PRIORITY ) {
        opal_output_verbose( 20, mca_pml_crcpw_component.output_handle,
                             "pml:crcpw: component_init: Wrap the selected component %s",
                             mca_pml_base_selected_component.pmlm_version.mca_component_name);

        mca_pml_crcpw_module.wrapped_pml_component = mca_pml_base_selected_component;
        mca_pml_crcpw_module.wrapped_pml_module    = mca_pml;
        mca_pml_crcpw_component.pml_crcp_wrapped = true;

        opal_output_verbose( 20, mca_pml_crcpw_component.output_handle,
                             "pml:crcpw: component_init: Initalize Wrapper");
        
        OBJ_CONSTRUCT(&pml_state_list, ompi_free_list_t);
        ompi_free_list_init_new( &pml_state_list,
                             sizeof(ompi_crcp_base_pml_state_t),
                             opal_cache_line_size,
                             OBJ_CLASS(ompi_crcp_base_pml_state_t),
                             0,opal_cache_line_size,
                             5,  /* Initial number */
                             -1, /* Max = Unlimited */
                             64, /* Increment by */
                             NULL);
    }
    else {
        opal_output_verbose( 20, mca_pml_crcpw_component.output_handle,
                             "pml:crcpw: component_init: Priority %d",
                             mca_pml_crcpw_component.priority);
    }


    *priority = mca_pml_crcpw_component.priority;

    pml_crcpw_is_finalized = false;

    return &mca_pml_crcpw_module.super;
}

int mca_pml_crcpw_component_finalize(void)
{
    opal_output_verbose( 20, mca_pml_crcpw_component.output_handle,
                         "pml:crcpw: component_finalize: Finalize");

    OBJ_DESTRUCT(&pml_state_list);

    pml_crcpw_is_finalized = true;

    if(mca_pml_crcpw_component.pml_crcp_wrapped) {
        return mca_pml_crcpw_module.wrapped_pml_component.pmlm_finalize();
    }

    return OMPI_SUCCESS;
}

