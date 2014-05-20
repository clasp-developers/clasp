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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/mca/backtrace/backtrace.h"

BEGIN_C_DECLS
    OPAL_DECLSPEC extern const opal_backtrace_base_component_2_0_0_t mca_backtrace_none_component;
END_C_DECLS

const opal_backtrace_base_component_2_0_0_t mca_backtrace_none_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {
        OPAL_BACKTRACE_BASE_VERSION_2_0_0,

        /* Component name and version */
        "none",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        NULL,
        NULL
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
};
