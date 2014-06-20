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

#include "orte_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "orte/constants.h"
#include "orte/types.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/class/opal_list.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"

#include "orte/util/show_help.h"
#include "opal/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/util/hostfile/hostfile.h"
#include "orte/util/dash_host/dash_host.h"
#include "orte/util/proc_info.h"
#include "orte/util/comm/comm.h"

#include "orte/mca/ras/base/ras_private.h"

/* static function to display allocation */
static void display_alloc(void)
{
    char *tmp=NULL, *tmp2, *tmp3, *pfx=NULL;
    int i;
    orte_node_t *alloc;
    
    if (orte_xml_output) {
        asprintf(&tmp, "<allocation>\n");
        pfx = "\t";
    } else {
        asprintf(&tmp, "\n======================   ALLOCATED NODES   ======================\n");
    }
    for (i=0; i < orte_node_pool->size; i++) {
        if (NULL == (alloc = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, i))) {
            continue;
        }
        opal_dss.print(&tmp2, pfx, alloc, ORTE_NODE);
        if (NULL == tmp) {
            tmp = tmp2;
        } else {
            asprintf(&tmp3, "%s%s", tmp, tmp2);
            free(tmp);
            free(tmp2);
            tmp = tmp3;
        }
    }
    if (orte_xml_output) {
        fprintf(orte_xml_fp, "%s</allocation>\n", tmp);
        fflush(orte_xml_fp);
    } else {
        opal_output(orte_clean_output, "%s\n\n=================================================================\n", tmp);
    }
    free(tmp);    
}

/*
 * Function for selecting one component from all those that are
 * available.
 */
int orte_ras_base_allocate(orte_job_t *jdata)
{
    int rc;
    opal_list_t nodes;
    orte_node_t *node;
    orte_std_cntr_t i;
    bool override_oversubscribed;
    orte_app_context_t *app;
    bool default_hostfile_used;

    OPAL_OUTPUT_VERBOSE((5, orte_ras_base.ras_output,
                         "%s ras:base:allocate",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* if we already did this, don't do it again - the pool of
     * global resources is set. 
     */
    if (orte_ras_base.allocation_read) {
        
        OPAL_OUTPUT_VERBOSE((5, orte_ras_base.ras_output,
                             "%s ras:base:allocate allocation already read",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
        /* loop through the global node pool and set the
         * number of allocated slots to the difference
         * between slots and slots_in_use. Note that
         * oversubscription will still allow procs to
         * be mapped up to slots_max
         */
        return ORTE_SUCCESS;
    }
    
    /* Otherwise, we have to create
     * the initial set of resources that will delineate all
     * further operations serviced by this HNP. This list will
     * contain ALL nodes that can be used by any subsequent job.
     *
     * In other words, if a node isn't found in this step, then
     * no job launched by this HNP will be able to utilize it.
     */
    
    /* note that the allocation has been read so we don't
     * come in here again!
     */
    orte_ras_base.allocation_read = true;

    /* construct a list to hold the results */
    OBJ_CONSTRUCT(&nodes, opal_list_t);

    /* if a component was selected, then we know we are in a managed
     * environment.  - the active module will return a list of what it found
     */
    if (NULL != orte_ras_base.active_module)  {
        /* read the allocation */
        if (ORTE_SUCCESS != (rc = orte_ras_base.active_module->allocate(&nodes))) {
            if (ORTE_ERR_SYSTEM_WILL_BOOTSTRAP == rc) {
                /* this module indicates that nodes will be discovered
                 * on a bootstrap basis, so all we do here is add our
                 * own node to the list
                 */
                goto addlocal;
            }
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&nodes);
            return rc;
        }
    } 
    /* If something came back, save it and we are done */
    if (!opal_list_is_empty(&nodes)) {
        /* store the results in the global resource pool - this removes the
        * list items
        */
        if (ORTE_SUCCESS != (rc = orte_ras_base_node_insert(&nodes, jdata))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&nodes);
            return rc;
        }
        OBJ_DESTRUCT(&nodes);
        /* flag that the allocation is managed */
        orte_managed_allocation = true;
        goto DISPLAY;
    } else if (orte_allocation_required) {
        /* if nothing was found, and an allocation is
         * required, then error out
         */
        OBJ_DESTRUCT(&nodes);
        orte_show_help("help-ras-base.txt", "ras-base:no-allocation", true);
        ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
        orte_trigger_event(&orte_exit);
        return ORTE_ERROR;
    }
    
    
    
    OPAL_OUTPUT_VERBOSE((5, orte_ras_base.ras_output,
                         "%s ras:base:allocate nothing found in module - proceeding to hostfile",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* nothing was found, or no active module was alive. Our next
     * option is to look for a hostfile and assign our global
     * pool from there.
     *
     * Individual hostfile names, if given, are included
     * in the app_contexts for this job. We therefore need to
     * retrieve the app_contexts for the job, and then cycle
     * through them to see if anything is there. The parser will
     * add the nodes found in each hostfile to our list - i.e.,
     * the resulting list contains the UNION of all nodes specified
     * in hostfiles from across all app_contexts
     *
     * Any app that has no hostfile but has a dash-host, will have
     * those nodes added to the list
     *
     * Any app that fails to have a hostfile or a dash-host will be given the
     * default hostfile, if we have it
     *
     * Note that any relative node syntax found in the hostfiles will
     * generate an error in this scenario, so only non-relative syntax
     * can be present
     */
    default_hostfile_used = false;
    for (i=0; i < jdata->apps->size; i++) {
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
            continue;
        }
        if (NULL != app->hostfile) {
            OPAL_OUTPUT_VERBOSE((5, orte_ras_base.ras_output,
                                 "%s ras:base:allocate adding hostfile %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 app->hostfile));
            
            /* hostfile was specified - parse it and add it to the list */
            if (ORTE_SUCCESS != (rc = orte_util_add_hostfile_nodes(&nodes,
                                                                   &override_oversubscribed,
                                                                   app->hostfile))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&nodes);
                return rc;
            }
        } else if (NULL != app->dash_host) {
            OPAL_OUTPUT_VERBOSE((5, orte_ras_base.ras_output,
                                 "%s ras:base:allocate adding dash_hosts",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            if (ORTE_SUCCESS != (rc = orte_util_add_dash_host_nodes(&nodes,
                                                                    &override_oversubscribed,
                                                                    app->dash_host))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&nodes);
                return rc;
            }
        } else if (!default_hostfile_used) {
            if (NULL != orte_default_hostfile) {
                OPAL_OUTPUT_VERBOSE((5, orte_ras_base.ras_output,
                                     "%s ras:base:allocate parsing default hostfile %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     orte_default_hostfile));
        
                /* a default hostfile was provided - parse it */
                if (ORTE_SUCCESS != (rc = orte_util_add_hostfile_nodes(&nodes,
                                                                       &override_oversubscribed,
                                                                       orte_default_hostfile))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_DESTRUCT(&nodes);
                    return rc;
                }
            }
            /* only look at it once */
            default_hostfile_used = true;
        }
        /* update the jdata object with override_oversubscribed flag, but don't
         * overwrite a prior setting to true
         */
        if (!jdata->oversubscribe_override) {
            jdata->oversubscribe_override = override_oversubscribed;
        }
    }
    /* if something was found in the hostfile(s), we use that as our global
     * pool - set it and we are done
     */
    if (!opal_list_is_empty(&nodes)) {
        /* store the results in the global resource pool - this removes the
         * list items
         */
        if (ORTE_SUCCESS != (rc = orte_ras_base_node_insert(&nodes, jdata))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&nodes);
            return rc;
        }
        /* cleanup */
        OBJ_DESTRUCT(&nodes);
        goto DISPLAY;
    }

    OPAL_OUTPUT_VERBOSE((5, orte_ras_base.ras_output,
                         "%s ras:base:allocate nothing found in hostfiles or dash-host - checking for rankfile",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* Our next option is to look for a rankfile - if one was provided, we
     * will use its nodes to create a default allocation pool
     */
    if (NULL != orte_rankfile) {
        /* check the rankfile for node information */
        if (ORTE_SUCCESS != (rc = orte_util_add_hostfile_nodes(&nodes,
                                                               &override_oversubscribed,
                                                               orte_rankfile))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&nodes);
            return rc;
        }
    }
    /* if something was found in rankfile, we use that as our global
     * pool - set it and we are done
     */
    if (!opal_list_is_empty(&nodes)) {
        /* store the results in the global resource pool - this removes the
         * list items
         */
        if (ORTE_SUCCESS != (rc = orte_ras_base_node_insert(&nodes, jdata))) {
            ORTE_ERROR_LOG(rc);
        }
        /* update the jdata object with override_oversubscribed flag */
        jdata->oversubscribe_override = false;
        /* cleanup */
        OBJ_DESTRUCT(&nodes);
        goto DISPLAY;
    }
    
    
    OPAL_OUTPUT_VERBOSE((5, orte_ras_base.ras_output,
                         "%s ras:base:allocate nothing found in rankfile - inserting current node",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
addlocal:
    /* if nothing was found by any of the above methods, then we have no
     * earthly idea what to do - so just add the local host
     */
    node = OBJ_NEW(orte_node_t);
    if (NULL == node) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&nodes);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    /* use the same name we got in orte_process_info so we avoid confusion in
     * the session directories
     */
    node->name = strdup(orte_process_info.nodename);
    node->state = ORTE_NODE_STATE_UP;
    node->slots_inuse = 0;
    node->slots_max = 0;
    node->slots = 1;
    /* indicate that we don't know anything about over_subscribing */
    jdata->oversubscribe_override = true;
    opal_list_append(&nodes, &node->super);
    
    /* store the results in the global resource pool - this removes the
     * list items
     */
    if (ORTE_SUCCESS != (rc = orte_ras_base_node_insert(&nodes, jdata))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&nodes);
        return rc;
    }
    OBJ_DESTRUCT(&nodes);

DISPLAY:
    /* are we to report this event? */
    if (orte_report_events) {
        if (ORTE_SUCCESS != (rc = orte_util_comm_report_event(ORTE_COMM_EVENT_ALLOCATE))) {
            ORTE_ERROR_LOG(rc);
        }
    }
    /* shall we display the results? */
    if (orte_ras_base.display_alloc) {
        display_alloc();
    }
    
    return rc;
}

int orte_ras_base_add_hosts(orte_job_t *jdata)
{
    int rc;
    opal_list_t nodes;
    bool override_oversubscribed;
    int i;
    orte_app_context_t *app;

    /* construct a list to hold the results */
    OBJ_CONSTRUCT(&nodes, opal_list_t);
    
    /* Individual add-hostfile names, if given, are included
     * in the app_contexts for this job. We therefore need to
     * retrieve the app_contexts for the job, and then cycle
     * through them to see if anything is there. The parser will
     * add the nodes found in each add-hostfile to our list - i.e.,
     * the resulting list contains the UNION of all nodes specified
     * in add-hostfiles from across all app_contexts
     *
     * Note that any relative node syntax found in the add-hostfiles will
     * generate an error in this scenario, so only non-relative syntax
     * can be present
     */
    
    for (i=0; i < jdata->apps->size; i++) {
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
            continue;
        }
        if (NULL != app->add_hostfile) {
            OPAL_OUTPUT_VERBOSE((5, orte_ras_base.ras_output,
                                 "%s ras:base:add_hosts checking add-hostfile %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 app->add_hostfile));
            
            /* hostfile was specified - parse it and add it to the list */
            if (ORTE_SUCCESS != (rc = orte_util_add_hostfile_nodes(&nodes,
                                                                   &override_oversubscribed,
                                                                   app->add_hostfile))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&nodes);
                return rc;
            }
            /* now indicate that this app is to run across it */
            app->hostfile = app->add_hostfile;
            app->add_hostfile = NULL;
        }
    }

    /* We next check for and add any add-host options. Note this is
     * a -little- different than dash-host in that (a) we add these
     * nodes to the global pool regardless of what may already be there,
     * and (b) as a result, any job and/or app_context can access them.
     *
     * Note that any relative node syntax found in the add-host lists will
     * generate an error in this scenario, so only non-relative syntax
     * can be present
     */
    for (i=0; i < jdata->apps->size; i++) {
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
            continue;
        }
        if (NULL != app->add_host) {
            if (4 < opal_output_get_verbosity(orte_ras_base.ras_output)) {
                char *fff = opal_argv_join(app->add_host, ',');
                opal_output(0, "%s ras:base:add_hosts checking add-host %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), fff);
                free(fff);
            }
            if (ORTE_SUCCESS != (rc = orte_util_add_dash_host_nodes(&nodes,
                                                                    &override_oversubscribed,
                                                                    app->add_host))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&nodes);
                return rc;
            }
            /* now indicate that this app is to run across them */
            app->dash_host = app->add_host;
            app->add_host = NULL;
        }
    }
    
    /* if something was found, we add that to our global pool */
    if (!opal_list_is_empty(&nodes)) {
        /* store the results in the global resource pool - this removes the
         * list items
         */
        if (ORTE_SUCCESS != (rc = orte_ras_base_node_insert(&nodes, jdata))) {
            ORTE_ERROR_LOG(rc);
        }
        /* update the jdata object with override_oversubscribed flag */
        jdata->oversubscribe_override = override_oversubscribed;
        /* cleanup */
        OBJ_DESTRUCT(&nodes);
    }
    
    /* shall we display the results? */
    if (orte_ras_base.display_alloc) {
        display_alloc();
    }
    
    return ORTE_SUCCESS;
}
