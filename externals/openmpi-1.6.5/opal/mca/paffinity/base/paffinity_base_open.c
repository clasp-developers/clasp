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
#include "opal/mca/paffinity/paffinity.h"
#include "opal/mca/paffinity/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "opal/mca/paffinity/base/static-components.h"


/*
 * Globals
 */
OPAL_DECLSPEC int opal_paffinity_base_output = -1;
bool opal_paffinity_base_components_opened_valid = false;
opal_list_t opal_paffinity_base_components_opened;
bool opal_paffinity_alone = false;
char *opal_paffinity_base_slot_list;
bool opal_paffinity_base_bound;
char *opal_paffinity_base_applied_binding;

/*
 * Register some paffinity-wide MCA params
 */
int opal_paffinity_base_register_params(void)
{
    int value, id;
    static int been_here = 0;

    /* We may get called twice; be harmless in that case. */
    if (1 == been_here) {
        return OPAL_SUCCESS;
    }
    been_here = 1;

    /* Debugging / verbose output */

    mca_base_param_reg_int_name("paffinity", "base_verbose", 
                                "Verbosity level of the paffinity framework",
                                false, false,
                                0, &value);
    if (0 != value) {
        opal_paffinity_base_output = opal_output_open(NULL);
    } else {
        opal_paffinity_base_output = -1;
    }

    id = mca_base_param_reg_int_name("opal", "paffinity_alone", 
                                "If nonzero, assume that this job is the only (set of) process(es) running on each node and bind processes to processors, starting with processor ID 0",
                                false, false,
                                0, NULL);
    /* register the historical mpi_paffinity_alone synonym, but don't
     * declare it deprecated so we don't scare the users.
     *
     * Yes, this breaks the abstraction barrier, but as indicated
     * on the developer list....live with it. :-)
     */
    mca_base_param_reg_syn_name(id, "mpi", "paffinity_alone", false);
    mca_base_param_lookup_int(id, &value);
    opal_paffinity_alone = OPAL_INT_TO_BOOL(value);

    mca_base_param_reg_int_name("paffinity", "base_bound",
                                "Process affinity was set by an external entity",
                                true, false,
                                false, &value);
    opal_paffinity_base_bound = OPAL_INT_TO_BOOL(value);

    mca_base_param_reg_string_name("paffinity", "base_applied_binding",
                                   "Process affinity was set by an external entity",
                                   true, false,
                                   NULL, &opal_paffinity_base_applied_binding);

    return OPAL_SUCCESS;
}

/*
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int opal_paffinity_base_open(void)
{
    opal_paffinity_base_components_opened_valid = false;
        
    mca_base_param_reg_string_name("opal", "paffinity_base_slot_list",
                                   "Used to set list of processor IDs to bind MPI processes to (e.g., used in conjunction with rank files)",
                                   true, false, NULL, &opal_paffinity_base_slot_list);

    /* Open up all available components */

    if (OPAL_SUCCESS !=
        mca_base_components_open("paffinity", opal_paffinity_base_output,
                                 mca_paffinity_base_static_components,
                                 &opal_paffinity_base_components_opened, 
                                 true)) {
        return OPAL_ERROR;
    }
    opal_paffinity_base_components_opened_valid = true;

    /* All done */

    return OPAL_SUCCESS;
}
