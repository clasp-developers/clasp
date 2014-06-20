/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2011 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/shmem/shmem.h"
#include "opal/mca/shmem/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "opal/mca/shmem/base/static-components.h"

/**
 * globals
 */
OPAL_DECLSPEC int opal_shmem_base_output = -1;
bool opal_shmem_base_components_opened_valid = false;
opal_list_t opal_shmem_base_components_opened;

/* ////////////////////////////////////////////////////////////////////////// */
/**
 * Register some shmem-wide MCA params
 */
int
opal_shmem_base_register_params(void)
{
    int value;

    /* debugging/verbose output */
    mca_base_param_reg_int_name("shmem", "base_verbose",
                                "Verbosity level of the shmem framework",
                                false, false, 0, &value);
    /* register an INTERNAL parameter used to provide a component selection
     * hint to the shmem framework.
     */
    mca_base_param_reg_string_name("shmem", "RUNTIME_QUERY_hint",
                                   "Internal OMPI parameter used to provide a "
                                   "component selection hint to the shmem "
                                   "framework.  The value of this parameter "
                                   "is the name of the component that is "
                                   "available, selectable, and meets our "
                                   "run-time behavior requirements.",
                                   true, true, NULL, NULL);
    if (0 != value) {
        opal_shmem_base_output = opal_output_open(NULL);
    }
    else {
        opal_shmem_base_output = -1;
    }

    return OPAL_SUCCESS;
}

/* ////////////////////////////////////////////////////////////////////////// */
/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int
opal_shmem_base_open(void)
{
    opal_shmem_base_components_opened_valid = false;

    /* open up all available components */
    if (OPAL_SUCCESS !=
        mca_base_components_open("shmem", opal_shmem_base_output,
                                 mca_shmem_base_static_components,
                                 &opal_shmem_base_components_opened, true)) {
        return OPAL_ERROR;
    }

    opal_shmem_base_components_opened_valid = true;

    /* all done */
    return OPAL_SUCCESS;
}

