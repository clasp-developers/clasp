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
 * Copyright (c) 2008-2009 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mpi.h"

#include "opal/class/opal_list.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"


#include "ompi/constants.h"
#include "ompi/mca/op/op.h"
#include "ompi/mca/op/base/base.h"


/*
 * Global variables
 */
bool ompi_op_base_components_available_valid = false;
opal_list_t ompi_op_base_components_available;


/*
 * Private functions
 */
static int init_query(const mca_base_component_t * ls,
                      mca_base_component_priority_list_item_t * entry,
                      bool enable_progress_threads,
                      bool enable_mpi_threads);
static int init_query_1_0_0(const mca_base_component_t * ls,
                            mca_base_component_priority_list_item_t *
                            entry, bool enable_progress_threads,
                            bool enable_mpi_threads);

/*
 * Scan down the list of successfully opened components and query each
 * of them (the opened list will be one or more components.  If the
 * user requested a specific set of components, they will be the only
 * components in the opened list).  Create and populate the available
 * list of all components who indicate that they want to be considered
 * for selection.  Close all components who do not want to be
 * considered for selection.  Finally, destroy the "opened" list,
 * because the only the "available" list is relevant now.
 */
int ompi_op_base_find_available(bool enable_progress_threads,
                                bool enable_mpi_threads)
{
    bool found = false;
    mca_base_component_priority_list_item_t *entry;
    opal_list_item_t *p;
    const mca_base_component_t *component;

    /* Initialize the list */

    OBJ_CONSTRUCT(&ompi_op_base_components_available, opal_list_t);
    ompi_op_base_components_available_valid = true;

    /* The list of components that we should check has already been
       established in ompi_op_base_open. */

    for (found = false,
         p = opal_list_remove_first(&ompi_op_base_components_opened);
         p != NULL;
         p = opal_list_remove_first(&ompi_op_base_components_opened)) {
        component = ((mca_base_component_list_item_t *) p)->cli_component;

        /* Call a subroutine to do the work, because the component may
           represent different versions of the op MCA. */

        entry = OBJ_NEW(mca_base_component_priority_list_item_t);
        entry->super.cli_component = component;
        entry->cpli_priority = 0;
        if (OMPI_SUCCESS == init_query(component, entry,
                                       enable_progress_threads,
                                       enable_mpi_threads)) {
            opal_list_append(&ompi_op_base_components_available,
                             (opal_list_item_t *) entry);
            found = true;
        } else {

            /* If the component doesn't want to run, then close it.
               It's already had its close() method invoked; now close
               it out of the DSO repository (if it's there). */

            mca_base_component_repository_release(component);
            OBJ_RELEASE(entry);
        }

        /* Free the entry from the "opened" list */

        OBJ_RELEASE(p);
    }

    /* The opened list is now no longer useful and we can free it */

    OBJ_DESTRUCT(&ompi_op_base_components_opened);
    ompi_op_base_components_opened_valid = false;

    /* It is not an error if there are no components available; we'll
       just fall back to the base functions. */

    return OMPI_SUCCESS;
}


/*
 * Query a component, see if it wants to run at all.  If it does, save
 * some information.  If it doesn't, close it.
 */
static int init_query(const mca_base_component_t * c,
                      mca_base_component_priority_list_item_t * entry,
                      bool enable_progress_threads, bool enable_mpi_threads)
{
    int ret;

    opal_output_verbose(10, ompi_op_base_output,
                        "op:find_available: querying op component %s",
                        c->mca_component_name);

    /* This component has already been successfully opened.  So now
       query it. */

    if (1 == c->mca_type_major_version &&
        0 == c->mca_type_minor_version &&
        0 == c->mca_type_release_version) {
        ret = init_query_1_0_0(c, entry, enable_progress_threads,
                               enable_mpi_threads);
    } else {
        /* Unrecognized op API version */

        opal_output_verbose(10, ompi_op_base_output,
                            "op:find_available: unrecognized op API version (%d.%d.%d, ignored)",
                            c->mca_type_major_version,
                            c->mca_type_minor_version,
                            c->mca_type_release_version);
        return OMPI_ERROR;
    }

    /* Query done -- look at the return value to see what happened */

    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(10, ompi_op_base_output,
                            "op:find_available: op component %s is not available",
                            c->mca_component_name);
        if (NULL != c->mca_close_component) {
            c->mca_close_component();
        }
    } else {
        opal_output_verbose(10, ompi_op_base_output,
                            "op:find_available: op component %s is available",
                            c->mca_component_name);
    }

    /* All done */

    return ret;
}


/*
 * Query a specific component, op v2.0.0
 */
static int init_query_1_0_0(const mca_base_component_t * component,
                            mca_base_component_priority_list_item_t * entry,
                            bool enable_progress_threads, 
                            bool enable_mpi_threads)
{
    ompi_op_base_component_1_0_0_t *op =
        (ompi_op_base_component_1_0_0_t *) component;

    return op->opc_init_query(enable_progress_threads,
                              enable_mpi_threads);
}
