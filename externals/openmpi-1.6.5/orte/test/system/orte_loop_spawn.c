/*file .c : spawned  the file Exe*/
#include <stdio.h>
#include <unistd.h>

#include "orte/constants.h"

#include "opal/util/argv.h"

#include "orte/mca/plm/plm.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_globals.h"

int main(int argc, char* argv[])
{
    int rc;
    orte_job_t *jdata;
    orte_app_context_t *app;
    char cwd[1024];
    int iter;
    
    if (0 > (rc = orte_init(&argc, &argv, ORTE_PROC_NON_MPI))) {
        fprintf(stderr, "couldn't init orte - error code %d\n", rc);
        return rc;
    }
    
    for (iter = 0; iter < 1000; ++iter) {
        /* setup the job object */
        jdata = OBJ_NEW(orte_job_t);
        jdata->controls |= ORTE_JOB_CONTROL_NON_ORTE_JOB;

        /* create an app_context that defines the app to be run */
        app = OBJ_NEW(orte_app_context_t);
        app->app = strdup("hostname");
        opal_argv_append_nosize(&app->argv, "hostname");
        app->num_procs = 1;
        
        getcwd(cwd, sizeof(cwd));
        app->cwd = strdup(cwd);
        app->user_specified_cwd = false;
    
        /* add the app to the job data */
        opal_pointer_array_add(jdata->apps, app);
        jdata->num_apps = 1;
        
        fprintf(stderr, "Parent: spawning child %d\n", iter);
        if (ORTE_SUCCESS != (rc = orte_plm.spawn(jdata))) {
            ORTE_ERROR_LOG(rc);
            exit(1);
        }
    }

    /* All done */
    orte_finalize();
    return 0;
}
