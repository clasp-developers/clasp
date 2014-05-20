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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <stdlib.h>

#include "opal/class/opal_list.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"
#include "ompi/mca/topo/topo.h"
#include "ompi/mca/topo/base/base.h"
#include "ompi/constants.h"

opal_list_t mca_topo_base_modules_available;
bool mca_topo_base_modules_available_valid = false;

static int init_query(const mca_base_component_t *m,
                      mca_base_component_priority_list_item_t *entry,
                      bool enable_progress_threads,
                      bool enable_mpi_threads);
static int init_query_2_0_0(const mca_base_component_t *component,
                            mca_base_component_priority_list_item_t *entry,
                            bool enable_progress_threads,
                            bool enable_mpi_threads);
    
int mca_topo_base_find_available(bool enable_progress_threads,
                                 bool enable_mpi_threads)
{
    bool found = false;
    mca_base_component_priority_list_item_t *entry;
    opal_list_item_t *p;

    /* Initialize the list */

    OBJ_CONSTRUCT(&mca_topo_base_components_available, opal_list_t);
    mca_topo_base_components_available_valid = true;

    /* The list of components which we should check is already present 
       in mca_topo_base_components_opened, which was established in 
       mca_topo_base_open */

     for (found = false, 
            p = opal_list_remove_first (&mca_topo_base_components_opened);
          NULL != p;
          p = opal_list_remove_first (&mca_topo_base_components_opened)) {
         entry = OBJ_NEW(mca_base_component_priority_list_item_t);
         entry->super.cli_component =
           ((mca_base_component_list_item_t *)p)->cli_component;

         /* Now for this entry, we have to determine the thread level. Call 
            a subroutine to do the job for us */

         if (OMPI_SUCCESS == init_query(entry->super.cli_component, entry,
                                        enable_progress_threads,
                                        enable_mpi_threads)) {
             /* Save the results in the list. The priority is not relvant at 
                this point in time. But we save the thread arguments so that
                the initial selection algorithm can negotiate overall thread
                level for this process */
             entry->cpli_priority = 0;
             opal_list_append (&mca_topo_base_components_available,
                               (opal_list_item_t *) entry);
             found = true;
         } else {
             /* The component does not want to run, so close it. Its close()
                has already been invoked. Close it out of the DSO repository
                (if it is there in the repository) */
             mca_base_component_repository_release(entry->super.cli_component);
             OBJ_RELEASE(entry);
         }
         /* Free entry from the "opened" list */
         OBJ_RELEASE(p);
     }

     /* The opened list is no longer necessary, so we can free it */
     OBJ_DESTRUCT (&mca_topo_base_components_opened);
     mca_topo_base_components_opened_valid = false;

     /* There should atleast be one topo component which was available */
     if (false == found) {
         /* Need to free all items in the list */
         OBJ_DESTRUCT(&mca_topo_base_components_available);
         mca_topo_base_components_available_valid = false;
         opal_output_verbose (10, mca_topo_base_output,
                              "topo:find_available: no topo components available!");
         return OMPI_ERROR;
     }

     /* All done */
     return OMPI_SUCCESS;
}
              
       
static int init_query(const mca_base_component_t *m,
                      mca_base_component_priority_list_item_t *entry,
                      bool enable_progress_threads,
                      bool enable_mpi_threads) 
{
    int ret;
    
    opal_output_verbose(10, mca_topo_base_output,
                        "topo:find_available: querying topo component %s",
                        m->mca_component_name);

    /* This component has been successfully opened, now try to query it */
    if (2 == m->mca_type_major_version &&
        0 == m->mca_type_minor_version &&
        0 == m->mca_type_release_version) {
        ret = init_query_2_0_0(m, entry, enable_progress_threads,
                               enable_mpi_threads);
    } else {
        /* unrecognised API version */
        opal_output_verbose(10, mca_topo_base_output,
                            "topo:find_available:unrecognised topo API version (%d.%d.%d)",
                            m->mca_type_major_version,
                            m->mca_type_minor_version,
                            m->mca_type_release_version);
        return OMPI_ERROR;
    }

    /* Query done -- look at return value to see what happened */
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(10, mca_topo_base_output,
                            "topo:find_available topo component %s is not available",
                            m->mca_component_name);
        if (NULL != m->mca_close_component) {
            m->mca_close_component();
        } 
    } else {
        opal_output_verbose(10, mca_topo_base_output,
                            "topo:find_avalable: topo component %s is available",
                            m->mca_component_name);

    }
    /* All done */
    return ret;
}


static int init_query_2_0_0(const mca_base_component_t *component,
                            mca_base_component_priority_list_item_t *entry,
                            bool enable_progress_threads,
                            bool enable_mpi_threads) 
{
    mca_topo_base_component_2_0_0_t *topo = (mca_topo_base_component_2_0_0_t *) component;
    
    return topo->topom_init_query(enable_progress_threads,
                                  enable_mpi_threads);
}
