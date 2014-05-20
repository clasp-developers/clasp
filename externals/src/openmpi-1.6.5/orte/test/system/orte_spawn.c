/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include <stdio.h>

#include "opal/util/argv.h"

#include "orte/util/proc_info.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"

#define MY_TAG 12345

int main(int argc, char* argv[])
{
    int rc;
    orte_job_t *jdata;
    orte_app_context_t *app;
    char cwd[1024];
    orte_process_name_t name;
    struct iovec msg;
    orte_vpid_t i;
    
    if (0 > (rc = orte_init(&argc, &argv, ORTE_PROC_NON_MPI))) {
        fprintf(stderr, "couldn't init orte - error code %d\n", rc);
        return rc;
    }

    /* setup the job object */
    jdata = OBJ_NEW(orte_job_t);
    jdata->controls |= ORTE_JOB_CONTROL_NON_ORTE_JOB;

    /* create an app_context that defines the app to be run */
    app = OBJ_NEW(orte_app_context_t);
    app->app = strdup("hostname");
    opal_argv_append_nosize(&app->argv, "hostname");
    app->num_procs = 3;
    
    getcwd(cwd, sizeof(cwd));
    app->cwd = strdup(cwd);
    app->user_specified_cwd = false;
    
    /* add the app to the job data */
    opal_pointer_array_add(jdata->apps, app);
    jdata->num_apps = 1;
#if 0
    /* setup a map object */
    jdata->map = OBJ_NEW(orte_job_map_t);
    jdata->map->display_map = true;
#endif    
    /* launch the job */
    fprintf(stderr, "Parent: spawning children!\n");
    if (ORTE_SUCCESS != (rc = orte_plm.spawn(jdata))) {
        ORTE_ERROR_LOG(rc);
        orte_finalize();
        return 1;
    }
    fprintf(stderr, "Parent: children spawned!\n");

#if 0
    /* send messages to all children - this will verify that we know their contact info */
    name.jobid = jdata->jobid;
    i = 1;
    msg.iov_base = (void *) &i;
    msg.iov_len  = sizeof(i);
    for (i=0; i < app->num_procs; i++) {
        name.vpid = i;
        fprintf(stderr, "Parent: sending message to child %s\n", ORTE_NAME_PRINT(&name));
        if (0 > (rc = orte_rml.send(&name, &msg, 1, MY_TAG, 0))) {
            ORTE_ERROR_LOG(rc);
        }
    }
#endif

    /* All done */
    orte_finalize();
    return 0;
}
