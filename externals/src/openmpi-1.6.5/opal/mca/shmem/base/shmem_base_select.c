/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2011 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif /* HAVE_STRING_H */

#include "opal/constants.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/shmem/shmem.h"
#include "opal/mca/shmem/base/base.h"

/*
 * globals
 */
bool opal_shmem_base_selected = false;
const opal_shmem_base_component_2_0_0_t *opal_shmem_base_component = NULL;
const opal_shmem_base_module_2_0_0_t *opal_shmem_base_module = NULL;

/* ////////////////////////////////////////////////////////////////////////// */
char *
opal_shmem_base_best_runnable_component_name(void)
{
    mca_base_component_t *best_component = NULL;
    mca_base_module_t *best_module = NULL;

    opal_output_verbose(10, opal_shmem_base_output,
                        "shmem: base: best_runnable_component_name: "
                        "Searching for best runnable component.");
    /* select the best component so we can get its name. */
    if (OPAL_SUCCESS != opal_shmem_base_runtime_query(&best_module,
                                                      &best_component)) {
        /* fail! */
        return NULL;
    }
    else {
        if (NULL != best_component) {
            opal_output_verbose(10, opal_shmem_base_output,
                                "shmem: base: best_runnable_component_name: "
                                "Found best runnable component: (%s).",
                                best_component->mca_component_name);
            return strdup(best_component->mca_component_name);
        }
        else {
            opal_output_verbose(10, opal_shmem_base_output,
                                "shmem: base: best_runnable_component_name: "
                                "Could not find runnable component.");
            /* no component returned, so return NULL */
            return NULL;
        }
    }
}

/* ////////////////////////////////////////////////////////////////////////// */
int
opal_shmem_base_runtime_query(mca_base_module_t **best_module,
                              mca_base_component_t **best_component)
{
    mca_base_component_list_item_t *cli = NULL;
    mca_base_component_t *component = NULL;
    mca_base_module_t *module = NULL;
    opal_list_item_t *item = NULL;
    int priority = 0, best_priority = INT32_MIN;
    char *env_hint_name = NULL, *env_hint_val = NULL;

    *best_module = NULL;
    *best_component = NULL;

    opal_output_verbose(10, opal_shmem_base_output,
                        "shmem: base: runtime_query: "
                        "Auto-selecting shmem components");
    /* we are using a nonstandard name here because shmem_RUNTIME_QUERY_hint
     * is for internal use only!
     * see odls_base_default_fns.c for more details.
     */
    env_hint_name = mca_base_param_environ_variable("shmem_RUNTIME_QUERY_hint",
                                                    NULL, NULL);
    env_hint_val = getenv(env_hint_name);
    free(env_hint_name);

    /* traverse the list of available components.
     * for each call their 'run-time query' functions to determine relative
     * priority.
     */
    for (item  = opal_list_get_first(&opal_shmem_base_components_opened);
         item != opal_list_get_end(&opal_shmem_base_components_opened);
         item  = opal_list_get_next(item) ) {
        cli = (mca_base_component_list_item_t *)item;
        component = (mca_base_component_t *)cli->cli_component;

        /* if there is a run-time query function then use it. otherwise, skip
         * the component.
         */
        if (NULL == ((opal_shmem_base_component_2_0_0_t *)
                     component)->runtime_query) {
            opal_output_verbose(5, opal_shmem_base_output,
                                "shmem: base: runtime_query: "
                                "(shmem) Skipping component [%s]. It does not "
                                "implement a run-time query function",
                                component->mca_component_name);
            continue;
        }

        /* query this component for the module and priority */
        opal_output_verbose(5, opal_shmem_base_output,
                            "shmem: base: runtime_query: "
                            "(shmem) Querying component (run-time) [%s]",
                            component->mca_component_name);

        ((opal_shmem_base_component_2_0_0_t *)
         component)->runtime_query(&module, &priority, env_hint_val);

        /* if no module was returned, then skip component.
         * this probably means that the run-time test deemed the shared memory
         * backing facility unusable or unsafe.
         */
        if (NULL == module) {
            opal_output_verbose(5, opal_shmem_base_output,
                                "shmem: base: runtime_query: "
                                "(shmem) Skipping component [%s]. Run-time "
                                "Query failed to return a module",
                                component->mca_component_name);
            continue;
        }

        /* determine if this is the best module we have seen by looking the
         * priority
         */
        opal_output_verbose(5, opal_shmem_base_output,
                            "shmem: base: runtime_query: "
                            "(%5s) Query of component [%s] set priority to %d",
                            "shmem", component->mca_component_name, priority);
        if (priority > best_priority) {
            best_priority = priority;
            *best_module = module;
            *best_component = component;
        }
    }

    /* finished querying all components.
     * make sure we found something in the process.
     */
    if (NULL == *best_component) {
        opal_output_verbose(5, opal_shmem_base_output,
                            "shmem: base: runtime_query: "
                            "(%5s) No component selected!", "shmem");
        /* still close the non-selected components.
         */
        mca_base_components_close(opal_shmem_base_output,
                                  &opal_shmem_base_components_opened, 
                                  NULL, false);
        return OPAL_ERR_NOT_FOUND;
    }

    opal_output_verbose(5, opal_shmem_base_output,
                        "shmem: base: runtime_query: "
                        "(%5s) Selected component [%s]", "shmem",
                        (*best_component)->mca_component_name);
    /* close the non-selected components */
    mca_base_components_close(opal_shmem_base_output,
                              &opal_shmem_base_components_opened,
                              (mca_base_component_t *)(*best_component),
                              false);

    return OPAL_SUCCESS;
}

/* ////////////////////////////////////////////////////////////////////////// */
int
opal_shmem_base_select(void)
{
    opal_shmem_base_component_2_0_0_t *best_component = NULL;
    opal_shmem_base_module_2_0_0_t *best_module = NULL;
    /* select the best component */
    if (OPAL_SUCCESS != opal_shmem_base_runtime_query(
                                (mca_base_module_t **)&best_module,
                                (mca_base_component_t **)&best_component)) {
        /* it is NOT okay if we don't find a module because we need at
         * least one shared memory backing facility component instance.
         */
        return OPAL_ERROR;
    }

    /* save the winner */
    opal_shmem_base_component = best_component;
    opal_shmem_base_module    = best_module;
    opal_shmem_base_selected  = true;

    /* initialize the winner */
    if (NULL != opal_shmem_base_module) {
        return opal_shmem_base_module->module_init();
    }
    else {
        return OPAL_ERROR;
    }
}

