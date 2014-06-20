/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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

#include "orte_config.h"


#include "opal/mca/base/base.h"
#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"


#include "orte/mca/rml/base/base.h"

#include "rml_ftrm.h"


static int orte_rml_ftrm_open( void);
static int orte_rml_ftrm_close(void);

/**
 * Component definition
 */
orte_rml_component_t mca_rml_ftrm_component = {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */
      {
        ORTE_RML_BASE_VERSION_2_0_0,

        "ftrm", /* MCA component name */
        ORTE_MAJOR_VERSION,    /* MCA component major version */
        ORTE_MINOR_VERSION,    /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */

        orte_rml_ftrm_open,    /* component open */
        orte_rml_ftrm_close,   /* component close */
      },
      {
          /* The component is checkpoint ready */
          MCA_BASE_METADATA_PARAM_CHECKPOINT
      },

      orte_rml_ftrm_component_init
};

orte_rml_module_t orte_rml_ftrm_module = {
    orte_rml_ftrm_module_enable_comm,
    orte_rml_ftrm_module_finalize,

    orte_rml_ftrm_get_contact_info,
    orte_rml_ftrm_set_contact_info,

    orte_rml_ftrm_get_new_name,
    orte_rml_ftrm_ping,

    orte_rml_ftrm_send,
    orte_rml_ftrm_send_nb,
    orte_rml_ftrm_send_buffer,
    orte_rml_ftrm_send_buffer_nb,

    orte_rml_ftrm_recv,
    orte_rml_ftrm_recv_nb,
    orte_rml_ftrm_recv_buffer,
    orte_rml_ftrm_recv_buffer_nb,
    orte_rml_ftrm_recv_cancel,

    orte_rml_ftrm_add_exception_handler,
    orte_rml_ftrm_del_exception_handler,

    orte_rml_ftrm_ft_event
};

int rml_ftrm_output_handle;

static int ftrm_priority = -1;

/*
 * Initalize the wrapper component
 */
orte_rml_module_t* orte_rml_ftrm_component_init(int* priority)
{
    /*
     * Asked to return a priority
     */
    if( NULL != priority ) {
        *priority = ftrm_priority;
        return &orte_rml_ftrm_module;
    }
    /*
     * Called a second time to swap module pointers
     */
    else {
        /* Copy the wrapped versions */
        orte_rml_ftrm_wrapped_module    = orte_rml;
        mca_rml_ftrm_wrapped_component  = *orte_rml_component;
        /* Replace with ourselves */
        orte_rml           = orte_rml_ftrm_module;
        orte_rml_component = &mca_rml_ftrm_component;

        opal_output_verbose(20, rml_ftrm_output_handle,
                            "orte_rml_ftrm: component_init(): Wrapped Component (%s)",
                            mca_rml_ftrm_wrapped_component.rml_version.mca_component_name);

        return NULL;
    }
}

/*
 * Initalize the structures upon opening
 */
static int orte_rml_ftrm_open(void)
{
    int value;

    mca_base_param_reg_int(&mca_rml_ftrm_component.rml_version,
                           "priority",
                           "Priority of the RML ftrm component",
                           false, false,
                           RML_SELECT_WRAPPER_PRIORITY,
                           &value);
    /* Enable this wrapper = RML_SELECT_WRAPPER_PRIORITY
     * ow = -1 or never selected
     */
#if OPAL_ENABLE_FT_CR == 1
    ftrm_priority = value;
#else
    ftrm_priority = -1;
#endif

    mca_base_param_reg_int(&mca_rml_ftrm_component.rml_version,
                           "verbose",
                           "Verbose level for the RML ftrm component",
                           false, false,
                           0, 
                           &value);
    /* If there is a custom verbose level for this component than use it
     * otherwise take our parents level and output channel
     */
    if ( 0 != value) {
        rml_ftrm_output_handle = opal_output_open(NULL);
        opal_output_set_verbosity(rml_ftrm_output_handle, value);
    } else {
        rml_ftrm_output_handle = -1;
    }

    opal_output_verbose(10, rml_ftrm_output_handle,
                        "orte_rml_ftrm: open(): Priority  = %d", ftrm_priority);
    opal_output_verbose(10, rml_ftrm_output_handle,
                        "orte_rml_ftrm: open(): Verbosity = %d", value);

    return ORTE_SUCCESS;
}

/*
 * Finalize the remaining structures upon close
 */
static int orte_rml_ftrm_close(void)
{
    return ORTE_SUCCESS;
}
