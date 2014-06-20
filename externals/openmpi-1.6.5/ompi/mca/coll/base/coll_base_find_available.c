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
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
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
#include "ompi/constants.h"
#include "opal/class/opal_list.h"
#include "opal/util/output.h"
#include "orte/util/show_help.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"


/*
 * Global variables
 */
bool mca_coll_base_components_available_valid = false;
opal_list_t mca_coll_base_components_available;


/*
 * Private functions
 */
static int init_query(const mca_base_component_t * ls,
                      mca_base_component_priority_list_item_t * entry,
                      bool enable_progress_threads,
                      bool enable_mpi_threads);
static int init_query_2_0_0(const mca_base_component_t * ls,
                            mca_base_component_priority_list_item_t *
                            entry, bool enable_progress_threads,
                            bool enable_mpi_threads);

/*
 * Scan down the list of successfully opened components and query each of
 * them (the opened list will be one or more components.  If the user
 * requested a specific component, it will be the only component in the
 * opened list).  Create and populate the available list of all
 * components who indicate that they want to be considered for selection.
 * Close all components who do not want to be considered for selection,
 * and destroy the opened list.
 *
 * Also find the basic component while we're doing all of this, and save
 * it in a global variable so that we can find it easily later (e.g.,
 * during scope selection).
 */
int mca_coll_base_find_available(bool enable_progress_threads,
                                 bool enable_mpi_threads)
{
    bool found = false;
    mca_base_component_priority_list_item_t *entry;
    opal_list_item_t *p;
    const mca_base_component_t *component;

    /* Initialize the list */

    OBJ_CONSTRUCT(&mca_coll_base_components_available, opal_list_t);
    mca_coll_base_components_available_valid = true;

    /* The list of components that we should check has already been
       established in mca_coll_base_open. */

    for (found = false,
         p = opal_list_remove_first(&mca_coll_base_components_opened);
         p != NULL;
         p = opal_list_remove_first(&mca_coll_base_components_opened)) {
        component = ((mca_base_component_list_item_t *) p)->cli_component;

        /* Call a subroutine to do the work, because the component may
           represent different versions of the coll MCA. */

        entry = OBJ_NEW(mca_base_component_priority_list_item_t);
        entry->super.cli_component = component;
        entry->cpli_priority = 0;
        if (OMPI_SUCCESS == init_query(component, entry,
                                       enable_progress_threads,
                                       enable_mpi_threads)) {
            opal_list_append(&mca_coll_base_components_available,
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

    OBJ_DESTRUCT(&mca_coll_base_components_opened);
    mca_coll_base_components_opened_valid = false;

    /* If we have no collective components available, it's an error.
       Thanks for playing! */

    if (!found) {
        /* Need to free all items in the list */
        OBJ_DESTRUCT(&mca_coll_base_components_available);
        mca_coll_base_components_available_valid = false;
        opal_output_verbose(10, mca_coll_base_output,
                            "coll:find_available: no coll components available!");
        orte_show_help("help-mca-base", "find-available:none-found", true,
                       "coll");
        return OMPI_ERROR;
    }

    /* All done */

    return OMPI_SUCCESS;
}


/*
 * Query a component, see if it wants to run at all.  If it does, save
 * some information.  If it doesn't, close it.
 */
static int init_query(const mca_base_component_t * component,
                      mca_base_component_priority_list_item_t * entry,
                      bool enable_progress_threads, bool enable_mpi_threads)
{
    int ret;

    opal_output_verbose(10, mca_coll_base_output,
                        "coll:find_available: querying coll component %s",
                        component->mca_component_name);

    /* This component has already been successfully opened.  So now
       query it. */

    if (2 == component->mca_type_major_version &&
        0 == component->mca_type_minor_version &&
        0 == component->mca_type_release_version) {
        ret = init_query_2_0_0(component, entry, enable_progress_threads,
                               enable_mpi_threads);
    } else {
        /* Unrecognized coll API version */

        opal_output_verbose(10, mca_coll_base_output,
                            "coll:find_available: unrecognized coll API version (%d.%d.%d, ignored)",
                            component->mca_type_major_version,
                            component->mca_type_minor_version,
                            component->mca_type_release_version);
        return OMPI_ERROR;
    }

    /* Query done -- look at the return value to see what happened */

    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(10, mca_coll_base_output,
                            "coll:find_available: coll component %s is not available",
                            component->mca_component_name);
        if (NULL != component->mca_close_component) {
            component->mca_close_component();
        }
    } else {
        opal_output_verbose(10, mca_coll_base_output,
                            "coll:find_available: coll component %s is available",
                            component->mca_component_name);
    }

    /* All done */

    return ret;
}


/*
 * Query a specific component, coll v2.0.0
 */
static int init_query_2_0_0(const mca_base_component_t * component,
                            mca_base_component_priority_list_item_t * entry,
                            bool enable_progress_threads, 
                            bool enable_mpi_threads)
{
    mca_coll_base_component_2_0_0_t *coll =
        (mca_coll_base_component_2_0_0_t *) component;

    return coll->collm_init_query(enable_progress_threads,
                                  enable_mpi_threads);
}
