/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <signal.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include "opal/mca/installdirs/installdirs.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "orte/util/show_help.h"
#include "opal/util/opal_environ.h"
#include "opal/util/path.h"
#include "opal/util/basename.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_wait.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmaps/rmaps.h"

#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/plm/base/plm_private.h"
#include "plm_alps.h"


/*
 * Local functions
 */
static int plm_alps_init(void);
static int plm_alps_launch_job(orte_job_t *jdata);
static int plm_alps_terminate_orteds(void);
static int plm_alps_signal_job(orte_jobid_t jobid, int32_t signal);
static int plm_alps_finalize(void);

static int plm_alps_start_proc(int argc, char **argv, char **env,
                                char *prefix);


/*
 * Global variable
 */
orte_plm_base_module_t orte_plm_alps_module = {
    plm_alps_init,
    orte_plm_base_set_hnp_name,
    plm_alps_launch_job,
    NULL,
    orte_plm_base_orted_terminate_job,
    plm_alps_terminate_orteds,
    orte_plm_base_orted_kill_local_procs,
    plm_alps_signal_job,
    plm_alps_finalize
};

/*
 * Local variables
 */
static pid_t alps_pid = 0;
static orte_jobid_t active_job = ORTE_JOBID_INVALID;
static bool failed_launch;


/**
* Init the module
 */
static int plm_alps_init(void)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_plm_base_comm_start())) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}


/* When working in this function, ALWAYS jump to "cleanup" if
 * you encounter an error so that orterun will be woken up and
 * the job can cleanly terminate
 */
static int plm_alps_launch_job(orte_job_t *jdata)
{
    orte_job_map_t *map;
    char *jobid_string = NULL;
    char *param;
    char **argv = NULL;
    int argc;
    int rc;
    char *tmp;
    char** env = NULL;
    char* var;
    char *nodelist_flat;
    char **nodelist_argv;
    int nodelist_argc;
    char *vpid_string;
    char **custom_strings;
    int num_args, i;
    char *cur_prefix;
    struct timeval joblaunchstart, launchstart, launchstop;
    int proc_vpid_index;
    orte_app_context_t **apps;
    orte_node_t **nodes;
    orte_std_cntr_t nnode;
    orte_jobid_t failed_job;
    orte_job_state_t job_state = ORTE_JOB_NEVER_LAUNCHED;

    if (jdata->controls & ORTE_JOB_CONTROL_DEBUGGER_DAEMON) {
        /* debugger daemons */
        failed_job = jdata->jobid;
        goto launch_apps;
    }

    /* default to declaring the daemon launch failed */
    failed_job = ORTE_PROC_MY_NAME->jobid;
    
    if (mca_plm_alps_component.timing) {
        if (0 != gettimeofday(&joblaunchstart, NULL)) {
            opal_output(0, "plm_alps: could not obtain job start time");
        }        
    }
    
    /* indicate the state of the launch */
    failed_launch = true;
    
    /* setup the job */
    if (ORTE_SUCCESS != (rc = orte_plm_base_setup_job(jdata))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:alps: launching job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid)));
    
    /* save the active jobid */
    active_job = jdata->jobid;
    
    /* Get the map for this job */
    if (NULL == (map = orte_rmaps.get_job_map(active_job))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        rc = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }
    apps = (orte_app_context_t**)jdata->apps->addr;
    nodes = (orte_node_t**)map->nodes->addr;
    
    if (0 == map->num_new_daemons) {
        /* have all the daemons we need - launch app */
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:alps: no new daemons to launch",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        goto launch_apps;
    }
    
    /* need integer value for command line parameter */
    orte_util_convert_jobid_to_string(&jobid_string, jdata->jobid);

    /*
     * start building argv array
     */
    argv = NULL;
    argc = 0;

    /*
     * ALPS aprun  OPTIONS
     */

    /* add the aprun command */
    opal_argv_append(&argc, &argv, "aprun");

    /* Append user defined arguments to aprun */
    if ( NULL != mca_plm_alps_component.custom_args ) {
        custom_strings = opal_argv_split(mca_plm_alps_component.custom_args, ' ');
        num_args       = opal_argv_count(custom_strings);
        for (i = 0; i < num_args; ++i) {
            opal_argv_append(&argc, &argv, custom_strings[i]);
        }
        opal_argv_free(custom_strings);
    }

    /* number of processors needed */
    opal_argv_append(&argc, &argv, "-n");
    asprintf(&tmp, "%lu", (unsigned long) map->num_new_daemons);
    opal_argv_append(&argc, &argv, tmp);
    free(tmp);
    opal_argv_append(&argc, &argv, "-N");
    opal_argv_append(&argc, &argv, "1");

    /* create nodelist */
    nodelist_argv = NULL;
    nodelist_argc = 0;

    for (nnode=0; nnode < map->num_nodes; nnode++) {
        /* if the daemon already exists on this node, then
         * don't include it
         */
        if (nodes[nnode]->daemon_launched) {
            continue;
        }
        
        /* otherwise, add it to the list of nodes upon which
         * we need to launch a daemon
         */
        opal_argv_append(&nodelist_argc, &nodelist_argv, nodes[nnode]->name);
    }
    if (0 == opal_argv_count(nodelist_argv)) {
        orte_show_help("help-plm-alps.txt", "no-hosts-in-list", true);
        rc = ORTE_ERR_FAILED_TO_START;
        goto cleanup;
    }
    nodelist_flat = opal_argv_join(nodelist_argv, ',');
    opal_argv_free(nodelist_argv);
    opal_argv_append(&argc, &argv, "-L");
    asprintf(&tmp, "%s", nodelist_flat);
    opal_argv_append(&argc, &argv, tmp);
    free(tmp);


    /*
     * ORTED OPTIONS
     */

    /* add the daemon command (as specified by user) */
    orte_plm_base_setup_orted_cmd(&argc, &argv);
    
    /* Add basic orted command line options, including debug flags */
    orte_plm_base_orted_append_basic_args(&argc, &argv,
                                          "alps",
                                          &proc_vpid_index,
                                          false, nodelist_flat);
    free(nodelist_flat);

    /* tell the new daemons the base of the name list so they can compute
     * their own name on the other end
     */
    rc = orte_util_convert_vpid_to_string(&vpid_string, map->daemon_vpid_start);
    if (ORTE_SUCCESS != rc) {
        opal_output(0, "plm_alps: unable to create process name");
        goto cleanup;
    }

    free(argv[proc_vpid_index]);
    argv[proc_vpid_index] = strdup(vpid_string);
    free(vpid_string);

    if (mca_plm_alps_component.debug) {
        param = opal_argv_join(argv, ' ');
        if (NULL != param) {
            opal_output(0, "plm:alps: final top-level argv:");
            opal_output(0, "plm:alps:     %s", param);
            free(param);
        }
    }

    /* Copy the prefix-directory specified in the
       corresponding app_context.  If there are multiple,
       different prefix's in the app context, complain (i.e., only
       allow one --prefix option for the entire alps run -- we
       don't support different --prefix'es for different nodes in
       the ALPS plm) */
    cur_prefix = NULL;
    for (i=0; i < jdata->num_apps; i++) {
        char * app_prefix_dir = apps[i]->prefix_dir;
         /* Check for already set cur_prefix_dir -- if different,
           complain */
        if (NULL != app_prefix_dir) {
            if (NULL != cur_prefix &&
                0 != strcmp (cur_prefix, app_prefix_dir)) {
                orte_show_help("help-plm-alps.txt", "multiple-prefixes",
                               true, cur_prefix, app_prefix_dir);
                return ORTE_ERR_FATAL;
            }

            /* If not yet set, copy it; iff set, then it's the
               same anyway */
            if (NULL == cur_prefix) {
                cur_prefix = strdup(app_prefix_dir);
                if (mca_plm_alps_component.debug) {
                    opal_output (0, "plm:alps: Set prefix:%s",
                                 cur_prefix);
                }
            }
        }
    }

    /* setup environment */
    env = opal_argv_copy(orte_launch_environ);
    
    if (mca_plm_alps_component.timing) {
        if (0 != gettimeofday(&launchstart, NULL)) {
            opal_output(0, "plm_alps: could not obtain start time");
        }        
    }
    
    /* set the job state to indicate we attempted to launch */
    job_state = ORTE_JOB_STATE_FAILED_TO_START;

    /* exec the daemon(s) */
    if (ORTE_SUCCESS != (rc = plm_alps_start_proc(argc, argv, env, cur_prefix))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* do NOT wait for alps to complete. Alps only completes when the processes
     * it starts - in this case, the orteds - complete. Instead, we'll catch
     * any alps failures and deal with them elsewhere
     */
    
    /* wait for daemons to callback */
    if (ORTE_SUCCESS != (rc = orte_plm_base_daemon_callback(map->num_new_daemons))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
launch_apps:
    /* if we get here, then daemons launched - change to declaring apps failed */
    failed_job = active_job;
    if (ORTE_SUCCESS != (rc = orte_plm_base_launch_apps(active_job))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* declare the launch a success */
    failed_launch = false;
    
    if (mca_plm_alps_component.timing) {
        if (0 != gettimeofday(&launchstop, NULL)) {
             opal_output(0, "plm_alps: could not obtain stop time");
         } else {
             opal_output(0, "plm_alps: daemon block launch time is %ld usec",
                         (launchstop.tv_sec - launchstart.tv_sec)*1000000 + 
                         (launchstop.tv_usec - launchstart.tv_usec));
             opal_output(0, "plm_alps: total job launch time is %ld usec",
                         (launchstop.tv_sec - joblaunchstart.tv_sec)*1000000 + 
                         (launchstop.tv_usec - joblaunchstart.tv_usec));
         }
    }

    if (ORTE_SUCCESS != rc) {
        opal_output(0, "plm:alps: start_procs returned error %d", rc);
        goto cleanup;
    }

    /* JMS: should we stash the alps pid in the gpr somewhere for cleanup? */

cleanup:
    if (NULL != argv) {
        opal_argv_free(argv);
    }
    if (NULL != env) {
        opal_argv_free(env);
    }
    
    if(NULL != jobid_string) {
        free(jobid_string);
    }
    
    /* check for failed launch - if so, force terminate */
    if (failed_launch) {
        orte_plm_base_launch_failed(failed_job, -1, ORTE_ERROR_DEFAULT_EXIT_CODE, job_state);
    }
    
    return rc;
}



/**
* Terminate the orteds for a given job
 */
static int plm_alps_terminate_orteds(void)
{
    int rc;
    
    /* deregister the waitpid callback to ensure we don't make it look like
     * alps failed when it didn't. Since the alps may have already completed,
     * do NOT ERROR_LOG any return code to avoid confusing, duplicate error
     * messages
     */
    orte_wait_cb_cancel(alps_pid);
    
    /* tell them to die! */
    if (ORTE_SUCCESS != (rc = orte_plm_base_orted_exit(ORTE_DAEMON_EXIT_CMD))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}


/**
 * Signal all the processes in the child alps by sending the signal directly to it
 */
static int plm_alps_signal_job(orte_jobid_t jobid, int32_t signal)
{
    if (0 != alps_pid) {
        kill(alps_pid, (int)signal);
   }
    return ORTE_SUCCESS;
}


static int plm_alps_finalize(void)
{
    int rc;
    
    /* cleanup any pending recvs */
    if (ORTE_SUCCESS != (rc = orte_plm_base_comm_stop())) {
        ORTE_ERROR_LOG(rc);
    }
    
    return ORTE_SUCCESS;
}


static void alps_wait_cb(pid_t pid, int status, void* cbdata){
    /* According to the ALPS folks, alps always returns the highest exit
       code of our remote processes. Thus, a non-zero exit status doesn't
       necessarily mean that alps failed - it could be that an orted returned
       a non-zero exit status. Of course, that means the orted failed(!), so
       the end result is the same - the job didn't start.
    
       As a result, we really can't do much with the exit status itself - it
       could be something in errno (if alps itself failed), or it could be
       something returned by an orted, or it could be something returned by
       the OS (e.g., couldn't find the orted binary). Somebody is welcome
       to sort out all the options and pretty-print a better error message. For
       now, though, the only thing that really matters is that
       alps failed. Report the error and make sure that orterun
       wakes up - otherwise, do nothing!
    */
    
    if (0 != status) {
        if (failed_launch) {
            /* we have a problem during launch */
            opal_output(0, "ERROR: alps failed to start the required daemons.");
            opal_output(0, "ERROR: This could be due to an inability to find the orted binary (--prefix)");
            opal_output(0, "ERROR: on one or more remote nodes, compilation of the orted with dynamic libraries,");
            opal_output(0, "ERROR: lack of authority to execute on one or more specified nodes,");
            opal_output(0, "ERROR: or the inability to write startup files into /tmp (--tmpdir/orte_tmpdir_base).");
            
            /* report that the daemon has failed so we break out of the daemon
             * callback receive and exit
             */
            orte_plm_base_launch_failed(ORTE_PROC_MY_NAME->jobid, pid, status, ORTE_JOB_STATE_FAILED_TO_START);
            
        } else {
            /* an orted must have died unexpectedly after launch - report
             * that the daemon has failed so we exit
             */
            orte_plm_base_launch_failed(ORTE_PROC_MY_NAME->jobid, pid, status, ORTE_JOB_STATE_ABORTED);
        }
    }
    
}


static int plm_alps_start_proc(int argc, char **argv, char **env,
                                char *prefix)
{
    int fd;
    char *exec_argv = opal_path_findv(argv[0], 0, env, NULL);

    if (NULL == exec_argv) {
        return ORTE_ERR_NOT_FOUND;
    }

    alps_pid = fork();
    if (-1 == alps_pid) {
        ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_CHILDREN);
        return ORTE_ERR_SYS_LIMITS_CHILDREN;
    }
    
    if (0 == alps_pid) {  /* child */
        char *bin_base = NULL, *lib_base = NULL;

        /* Figure out the basenames for the libdir and bindir.  There
           is a lengthy comment about this in plm_rsh_module.c
           explaining all the rationale for how / why we're doing
           this. */

        lib_base = opal_basename(opal_install_dirs.libdir);
        bin_base = opal_basename(opal_install_dirs.bindir);

        /* If we have a prefix, then modify the PATH and
           LD_LIBRARY_PATH environment variables.  */
        if (NULL != prefix) {
            char *oldenv, *newenv;

            /* Reset PATH */
            oldenv = getenv("PATH");
            if (NULL != oldenv) {
                asprintf(&newenv, "%s/%s:%s", prefix, bin_base, oldenv);
            } else {
                asprintf(&newenv, "%s/%s", prefix, bin_base);
            }
            opal_setenv("PATH", newenv, true, &env);
            if (mca_plm_alps_component.debug) {
                opal_output(0, "plm:alps: reset PATH: %s", newenv);
            }
            free(newenv);

            /* Reset LD_LIBRARY_PATH */
            oldenv = getenv("LD_LIBRARY_PATH");
            if (NULL != oldenv) {
                asprintf(&newenv, "%s/%s:%s", prefix, lib_base, oldenv);
            } else {
                asprintf(&newenv, "%s/%s", prefix, lib_base);
            }
            opal_setenv("LD_LIBRARY_PATH", newenv, true, &env);
            if (mca_plm_alps_component.debug) {
                opal_output(0, "plm:alps: reset LD_LIBRARY_PATH: %s",
                            newenv);
            }
            free(newenv);
        }

        fd = open("/dev/null", O_CREAT|O_WRONLY|O_TRUNC, 0666);
        if(fd > 0) {
            dup2(fd, 0);
        }

        /* When not in debug mode and --debug-daemons was not passed,
         * tie stdout/stderr to dev null so we don't see messages from orted */
        if (0 == mca_plm_alps_component.debug && !orte_debug_daemons_flag) {
            if (fd >= 0) {
                if (fd != 1) {
                    dup2(fd,1);
                }
                if (fd != 2) {
                    dup2(fd,2);
                }
            }
        }

        if (fd > 2) {
            close(fd);
        }

        /* get the alps process out of orterun's process group so that
           signals sent from the shell (like those resulting from
           cntl-c) don't get sent to alps */
        setpgid(0, 0);
         
        
        execve(exec_argv, argv, env);

        opal_output(0, "plm:alps:start_proc: exec failed");
        /* don't return - need to exit - returning would be bad -
           we're not in the calling process anymore */
        exit(1);
    } else {  /* parent */
        /* just in case, make sure that the alps process is not in our
        process group any more.  Stevens says always do this on both
        sides of the fork... */
        setpgid(alps_pid, alps_pid);
        
        /* setup the waitpid so we can find out if alps succeeds! */
        orte_wait_cb(alps_pid, alps_wait_cb, NULL);
        free(exec_argv);
    }

    return ORTE_SUCCESS;
}
