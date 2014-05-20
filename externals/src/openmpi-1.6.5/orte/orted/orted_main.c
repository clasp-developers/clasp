/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2009      Institut National de Recherche en Informatique
 *                         et Automatique. All rights reserved.
 * Copyright (c) 2011      Oracle and/or its affiliates.  All rights reserved.
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

#include <stdio.h>
#include <ctype.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */


#include "opal/event/event.h"
#include "opal/mca/base/base.h"
#include "opal/util/output.h"
#include "opal/util/cmd_line.h"
#include "opal/util/opal_environ.h"
#include "opal/util/os_path.h"
#include "opal/util/printf.h"
#include "opal/util/argv.h"
#include "opal/runtime/opal.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/daemon_init.h"
#include "opal/dss/dss.h"
#include "opal/mca/sysinfo/sysinfo.h"

#include "orte/constants.h"
#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "orte/util/session_dir.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_locks.h"
#include "orte/mca/rml/base/rml_contact.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/odls/base/odls_private.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/ras/ras.h"
#include "orte/mca/routed/routed.h"

/* need access to the create_jobid fn used by plm components
* so we can set singleton name, if necessary
*/
#include "orte/mca/plm/base/plm_private.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"

#include "orte/orted/orted.h"

/*
 * Globals
 */

static opal_event_t term_handler;
static opal_event_t int_handler;
static opal_event_t pipe_handler;
static opal_event_t epipe_handler;
#ifndef __WINDOWS__
static opal_event_t sigusr1_handler;
static opal_event_t sigusr2_handler;
#endif  /* __WINDOWS__ */
char *log_path = NULL;
static opal_event_t *orted_exit_event;
static bool signals_set=false;

static void shutdown_callback(int fd, short flags, void *arg);
static void shutdown_signal(int fd, short flags, void *arg);
static void signal_callback(int fd, short event, void *arg);
static void epipe_signal_callback(int fd, short flags, void *arg);

static struct {
    bool debug;
    bool help;
    bool set_sid;
    bool hnp;
    bool daemonize;
    char* name;
    char* vpid_start;
    char* num_procs;
    int uri_pipe;
    int singleton_died_pipe;
    int fail;
    int fail_delay;
    bool abort;
    int heartbeat;
} orted_globals;

/*
 * define the orted context table for obtaining parameters
 */
opal_cmd_line_init_t orte_cmd_line_opts[] = {
    /* Various "obvious" options */
    { NULL, NULL, NULL, 'h', NULL, "help", 0,
      &orted_globals.help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },

    { "orte", "daemon_spin", NULL, 's', NULL, "spin", 0,
      &orted_spin_flag, OPAL_CMD_LINE_TYPE_BOOL,
      "Have the orted spin until we can connect a debugger to it" },

    { NULL, NULL, NULL, '\0', NULL, "debug-failure", 1,
      &orted_globals.fail, OPAL_CMD_LINE_TYPE_INT,
      "Have the specified orted fail after init for debugging purposes" },
    
    { NULL, NULL, NULL, '\0', NULL, "debug-failure-delay", 1,
      &orted_globals.fail_delay, OPAL_CMD_LINE_TYPE_INT,
      "Have the orted specified for failure delay for the provided number of seconds before failing" },
    
    { NULL, NULL, NULL, '\0', NULL, "heartbeat", 1,
      &orted_globals.heartbeat, OPAL_CMD_LINE_TYPE_INT,
      "Seconds between orted heartbeat messages to be sent to HNP (default: 0 => no heartbeat)" },
    
    { "orte", "debug", NULL, 'd', NULL, "debug", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Debug the OpenRTE" },
        
    { "orte", "daemonize", NULL, '\0', NULL, "daemonize", 0,
      &orted_globals.daemonize, OPAL_CMD_LINE_TYPE_BOOL,
      "Daemonize the orted into the background" },

    { "orte", "debug", "daemons", '\0', NULL, "debug-daemons", 0,
      &orted_globals.debug, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable debugging of OpenRTE daemons" },

    { "orte", "debug", "daemons_file", '\0', NULL, "debug-daemons-file", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable debugging of OpenRTE daemons, storing output in files" },

    { NULL, NULL, NULL, '\0', NULL, "hnp", 0,
      &orted_globals.hnp, OPAL_CMD_LINE_TYPE_BOOL,
      "Direct the orted to act as the HNP"},

    { "orte", "hnp", "uri", '\0', NULL, "hnp-uri", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "URI for the HNP"},
    
    { "orte", "parent", "uri", '\0', NULL, "parent-uri", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "URI for the parent if tree launch is enabled."},
    
    { NULL, NULL, NULL, '\0', NULL, "set-sid", 0,
      &orted_globals.set_sid, OPAL_CMD_LINE_TYPE_BOOL,
      "Direct the orted to separate from the current session"},
    
    { "tmpdir", "base", NULL, '\0', NULL, "tmpdir", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Set the root for the session directory tree" },

    { NULL, NULL, NULL, '\0', NULL, "report-uri", 1,
      &orted_globals.uri_pipe, OPAL_CMD_LINE_TYPE_INT,
      "Report this process' uri on indicated pipe"},

    { NULL, NULL, NULL, '\0', NULL, "singleton-died-pipe", 1,
      &orted_globals.singleton_died_pipe, OPAL_CMD_LINE_TYPE_INT,
      "Watch on indicated pipe for singleton termination"},
    
    { "orte", "output", "filename", '\0', "output-filename", "output-filename", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Redirect output from application processes into filename.rank" },
    
    { "orte", "xterm", NULL, '\0', "xterm", "xterm", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Create a new xterm window and display output from the specified ranks there" },

    { NULL, NULL, NULL, '\0', "launch", "launch", 1,
      &orted_launch_cmd, OPAL_CMD_LINE_TYPE_STRING,
      "A regular expression describing the job to be launched at startup" },

    { "orte", "daemon", "bootstrap", '\0', "bootstrap", "bootstrap", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Bootstrap the connection to the HNP" },
    
    { "orte", "report", "bindings", '\0', "report-bindings", "report-bindings", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to report process bindings to stderr" },

    /* End of list */
    { NULL, NULL, NULL, '\0', NULL, NULL, 0,
      NULL, OPAL_CMD_LINE_TYPE_NULL, NULL }
};

int orte_daemon(int argc, char *argv[])
{
    int ret = 0;
    int fd;
    opal_cmd_line_t *cmd_line = NULL;
    char log_file[PATH_MAX];
    char *jobidstring;
    char *rml_uri;
    int i;
    opal_buffer_t *buffer;
    char hostname[100];
    char *tmp_env_var = NULL;
    struct timeval starttime, setuptime;
    
    /* get our time for first executable */
    gettimeofday(&starttime, NULL);
    
    /* initialize the globals */
    memset(&orted_globals, 0, sizeof(orted_globals));
    /* initialize the singleton died pipe to an illegal value so we can detect it was set */
    orted_globals.singleton_died_pipe = -1;
    /* init the failure orted vpid to an invalid value */
    orted_globals.fail = ORTE_VPID_INVALID;
    
    /* setup to check common command line options that just report and die */
    cmd_line = OBJ_NEW(opal_cmd_line_t);
    opal_cmd_line_create(cmd_line, orte_cmd_line_opts);
    mca_base_cmd_line_setup(cmd_line);
    if (ORTE_SUCCESS != (ret = opal_cmd_line_parse(cmd_line, false,
                                                   argc, argv))) {
        char *args = NULL;
        args = opal_cmd_line_get_usage_msg(cmd_line);
        orte_show_help("help-orted.txt", "orted:usage", false,
                       argv[0], args);
        free(args);
        return ret;
    }
    
    /*
     * Since this process can now handle MCA/GMCA parameters, make sure to
     * process them.
     */
    mca_base_cmd_line_process_args(cmd_line, &environ, &environ);
    
    /* make sure that opal_profile is -not- set as we do not care
     * what frameworks are opened by the daemons
     */
    if (NULL != getenv("OMPI_MCA_opal_profile")) {
        putenv("OMPI_MCA_opal_profile=0");
    }
    
    /* Ensure that enough of OPAL is setup for us to be able to run */
    /*
     * NOTE: (JJH)
     *  We need to allow 'mca_base_cmd_line_process_args()' to process command
     *  line arguments *before* calling opal_init_util() since the command
     *  line could contain MCA parameters that affect the way opal_init_util()
     *  functions. AMCA parameters are one such option normally received on the
     *  command line that affect the way opal_init_util() behaves.
     *  It is "safe" to call mca_base_cmd_line_process_args() before 
     *  opal_init_util() since mca_base_cmd_line_process_args() does *not*
     *  depend upon opal_init_util() functionality.
     */
    if (OPAL_SUCCESS != opal_init_util(&argc, &argv)) {
        fprintf(stderr, "OPAL failed to initialize -- orted aborting\n");
        exit(1);
    }

    /* setup the exit triggers */
    OBJ_CONSTRUCT(&orte_exit, orte_trigger_event_t);
 
    /* save the environment for launch purposes. This MUST be
     * done so that we can pass it to any local procs we
     * spawn - otherwise, those local procs won't see any
     * non-MCA envars that were set in the enviro when the
     * orted was executed - e.g., by .csh
     */
    orte_launch_environ = opal_argv_copy(environ);
    
    
    /* if orte_daemon_debug is set, let someone know we are alive right
     * away just in case we have a problem along the way
     */
    if (orted_globals.debug) {
        gethostname(hostname, 100);
        fprintf(stderr, "Daemon was launched on %s - beginning to initialize\n", hostname);
    }
    
    /* check for help request */
    if (orted_globals.help) {
        char *args = NULL;
        args = opal_cmd_line_get_usage_msg(cmd_line);
        orte_show_help("help-orted.txt", "orted:usage", false,
                       argv[0], args);
        free(args);
        return 1;
    }
#if defined(HAVE_SETSID) && !defined(__WINDOWS__)
    /* see if we were directed to separate from current session */
    if (orted_globals.set_sid) {
        setsid();
    }
#endif  /* !defined(__WINDOWS__) */
    /* see if they want us to spin until they can connect a debugger to us */
    i=0;
    while (orted_spin_flag) {
        i++;
        if (1000 < i) i=0;        
    }

#if OPAL_ENABLE_FT_CR == 1
    /* Mark as a tool program */
    tmp_env_var = mca_base_param_env_var("opal_cr_is_tool");
    opal_setenv(tmp_env_var,
                "1",
                true, &environ);
    free(tmp_env_var);
#endif
    tmp_env_var = NULL; /* Silence compiler warning */

    /* Set the flag telling OpenRTE that I am NOT a
     * singleton, but am "infrastructure" - prevents setting
     * up incorrect infrastructure that only a singleton would
     * require.
     */
    if (orted_globals.hnp) {
        if (ORTE_SUCCESS != (ret = orte_init(&argc, &argv, ORTE_PROC_HNP))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    } else {
        if (ORTE_SUCCESS != (ret = orte_init(&argc, &argv, ORTE_PROC_DAEMON))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    }
    
    if ((int)ORTE_VPID_INVALID != orted_globals.fail) {
        orted_globals.abort=false;
        /* some vpid was ordered to fail. The value can be positive
         * or negative, depending upon the desired method for failure,
         * so need to check both here
         */
        if (0 > orted_globals.fail) {
            orted_globals.fail = -1*orted_globals.fail;
            orted_globals.abort = true;
        }
        /* are we the specified vpid? */
        if ((int)ORTE_PROC_MY_NAME->vpid == orted_globals.fail) {
            /* if the user specified we delay, then setup a timer
             * and have it kill us
             */
            if (0 < orted_globals.fail_delay) {
                ORTE_TIMER_EVENT(orted_globals.fail_delay, 0, shutdown_signal);
                
            } else {
                opal_output(0, "%s is executing clean %s", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            orted_globals.abort ? "abort" : "abnormal termination");

                /* do -not- call finalize as this will send a message to the HNP
                 * indicating clean termination! Instead, just forcibly cleanup
                 * the local session_dir tree and exit
                 */
                orte_session_dir_cleanup(ORTE_JOBID_WILDCARD);
                
                /* if we were ordered to abort, do so */
                if (orted_globals.abort) {
                    abort();
                }
                
                /* otherwise, return with non-zero status */
                ret = ORTE_ERROR_DEFAULT_EXIT_CODE;
                goto DONE;
            }
        }
    }

    /* detach from controlling terminal
     * otherwise, remain attached so output can get to us
     */
    if(!orte_debug_flag &&
       !orte_debug_daemons_flag &&
       orted_globals.daemonize) {
        opal_daemon_init(NULL);
    }
    
    /* insert our contact info into our process_info struct so we
     * have it for later use and set the local daemon field to our name
     */
    orte_process_info.my_daemon_uri = orte_rml.get_contact_info();
    ORTE_PROC_MY_DAEMON->jobid = ORTE_PROC_MY_NAME->jobid;
    ORTE_PROC_MY_DAEMON->vpid = ORTE_PROC_MY_NAME->vpid;
    
    /* if I am also the hnp, then update that contact info field too */
    if (ORTE_PROC_IS_HNP) {
        orte_process_info.my_hnp_uri = orte_rml.get_contact_info();
        ORTE_PROC_MY_HNP->jobid = ORTE_PROC_MY_NAME->jobid;
        ORTE_PROC_MY_HNP->vpid = ORTE_PROC_MY_NAME->vpid;
    }
    
    /* setup an event we can wait for to tell
     * us to terminate - both normal and abnormal
     * termination will call us here. Use the same exit
     * fd as orterun so that orte_comm can wake either of us up
     * since we share that code
     */
    if (ORTE_SUCCESS != (ret = orte_wait_event(&orted_exit_event, &orte_exit, "orted_shutdown", shutdown_callback))) {
        ORTE_ERROR_LOG(ret);
        goto DONE;
    }
    
    /* setup the primary daemon command receive function */
    ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DAEMON,
                                  ORTE_RML_NON_PERSISTENT, orte_daemon_recv, NULL);
    if (ret != ORTE_SUCCESS && ret != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(ret);
        goto DONE;
    }
    
#ifndef __WINDOWS__
    /* setup callback for SIGPIPE */
    opal_signal_set(&epipe_handler, SIGPIPE,
                    epipe_signal_callback, &epipe_handler);
    opal_signal_add(&epipe_handler, NULL);
    /* Set signal handlers to catch kill signals so we can properly clean up
     * after ourselves. 
     */
    opal_event_set(&term_handler, SIGTERM, OPAL_EV_SIGNAL,
                   shutdown_signal, NULL);
    opal_event_add(&term_handler, NULL);
    opal_event_set(&int_handler, SIGINT, OPAL_EV_SIGNAL,
                   shutdown_signal, NULL);
    opal_event_add(&int_handler, NULL);

    /** setup callbacks for signals we should ignore */
    opal_signal_set(&sigusr1_handler, SIGUSR1,
                    signal_callback, &sigusr1_handler);
    opal_signal_add(&sigusr1_handler, NULL);
    opal_signal_set(&sigusr2_handler, SIGUSR2,
                    signal_callback, &sigusr2_handler);
    opal_signal_add(&sigusr2_handler, NULL);
#endif  /* __WINDOWS__ */

    signals_set = true;
    
    /* setup stdout/stderr */
    if (orte_debug_daemons_file_flag) {
        /* if we are debugging to a file, then send stdout/stderr to
         * the orted log file
         */

        /* get my jobid */
        if (ORTE_SUCCESS != (ret = orte_util_convert_jobid_to_string(&jobidstring,
                                                                     ORTE_PROC_MY_NAME->jobid))) {
            ORTE_ERROR_LOG(ret);
            goto DONE;
        }

        /* define a log file name in the session directory */
        snprintf(log_file, PATH_MAX, "output-orted-%s-%s.log",
                 jobidstring, orte_process_info.nodename);
        log_path = opal_os_path(false,
                                orte_process_info.tmpdir_base,
                                orte_process_info.top_session_dir,
                                log_file,
                                NULL);

        fd = open(log_path, O_RDWR|O_CREAT|O_TRUNC, 0640);
        if (fd < 0) {
            /* couldn't open the file for some reason, so
             * just connect everything to /dev/null
             */
            fd = open("/dev/null", O_RDWR|O_CREAT|O_TRUNC, 0666);
        } else {
            dup2(fd, STDOUT_FILENO);
            dup2(fd, STDERR_FILENO);
            if(fd != STDOUT_FILENO && fd != STDERR_FILENO) {
                close(fd);
            }
        }
    }

    /* output a message indicating we are alive, our name, and our pid
     * for debugging purposes
     */
    if (orte_debug_daemons_flag) {
        fprintf(stderr, "Daemon %s checking in as pid %ld on host %s\n",
                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (long)orte_process_info.pid,
                orte_process_info.nodename);
    }

    /* We actually do *not* want the orted to voluntarily yield() the
       processor more than necessary.  The orted already blocks when
       it is doing nothing, so it doesn't use any more CPU cycles than
       it should; but when it *is* doing something, we do not want it
       to be unnecessarily delayed because it voluntarily yielded the
       processor in the middle of its work.

       For example: when a message arrives at the orted, we want the
       OS to wake up the orted in a timely fashion (which most OS's
       seem good about doing) and then we want the orted to process
       the message as fast as possible.  If the orted yields and lets
       aggressive MPI applications get the processor back, it may be a
       long time before the OS schedules the orted to run again
       (particularly if there is no IO event to wake it up).  Hence,
       routed OOB messages (for example) may be significantly delayed
       before being delivered to MPI processes, which can be
       problematic in some scenarios (e.g., COMM_SPAWN, BTL's that
       require OOB messages for wireup, etc.). */
    opal_progress_set_yield_when_idle(false);

    /* Change the default behavior of libevent such that we want to
       continually block rather than blocking for the default timeout
       and then looping around the progress engine again.  There
       should be nothing in the orted that cannot block in libevent
       until "something" happens (i.e., there's no need to keep
       cycling through progress because the only things that should
       happen will happen in libevent).  This is a minor optimization,
       but what the heck... :-) */
    opal_progress_set_event_flag(OPAL_EVLOOP_ONCE);

    /* if requested, obtain and report a new process name and my uri to the indicated pipe */
    if (orted_globals.uri_pipe > 0) {
        orte_job_t *jdata;
        orte_proc_t *proc;
        orte_node_t *node;
        orte_app_context_t *app;
        char *tmp, *nptr, *sysinfo;
        int rc;
        int32_t ljob;

        /* setup the singleton's job */
        jdata = OBJ_NEW(orte_job_t);
        orte_plm_base_create_jobid(jdata);
        ljob = ORTE_LOCAL_JOBID(jdata->jobid);
        opal_pointer_array_set_item(orte_job_data, ljob, jdata);
        
        /* setup an app_context for the singleton */
        app = OBJ_NEW(orte_app_context_t);
        app->app = strdup("singleton");
        app->num_procs = 1;
        opal_pointer_array_add(jdata->apps, app);
        
        /* run our local allocator to read the available
         * allocation in case this singleton decides to
         * comm_spawn other procs
         */
        if (ORTE_SUCCESS != (rc = orte_ras.allocate(jdata))) {
            ORTE_ERROR_LOG(rc);
            /* don't quit as this would cause the singleton
             * to hang!
             */
        }
        
        /* setup a map for the job - even though we won't
         * actually map it, we need a map object so the
         * nidmap (if the singleton should call comm_spawn)
         * will be complete
         */
        jdata->map = OBJ_NEW(orte_job_map_t);
        
        node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, 0);
        /* add the node to the map */
        OBJ_RETAIN(node);
        opal_pointer_array_add(jdata->map->nodes, node);
        jdata->map->num_nodes = 1;

        /* setup a proc object for the singleton - since we
         * -must- be the HNP, and therefore we stored our
         * node on the global node pool, and since the singleton
         * -must- be on the same node as us, indicate that
         */
        proc = OBJ_NEW(orte_proc_t);
        proc->name.jobid = jdata->jobid;
        proc->name.vpid = 0;
        proc->state = ORTE_PROC_STATE_RUNNING;
        proc->app_idx = 0;
        proc->node = node;
        proc->local_rank = 0;
        proc->node_rank = 0;
        OBJ_RETAIN(node);  /* keep accounting straight */
        node->num_procs = 1;
        node->slots_inuse = 1;
        opal_pointer_array_add(jdata->procs, proc);
        jdata->num_procs = 1;
        
        /* create a string that contains our uri + the singleton's name + sysinfo */
        orte_util_convert_process_name_to_string(&nptr, &proc->name);
        orte_util_convert_sysinfo_to_string(&sysinfo, orte_local_cpu_type, 
					    orte_local_cpu_model);
        asprintf(&tmp, "%s[%s][%s]", orte_process_info.my_daemon_uri, nptr, 
		 sysinfo);
        free(nptr);
	free(sysinfo);

        /* pass that info to the singleton */
#ifndef __WINDOWS__
        write(orted_globals.uri_pipe, tmp, strlen(tmp)+1); /* need to add 1 to get the NULL */
#else
        send(orted_globals.uri_pipe, tmp, strlen(tmp)+1, 0); /* need to add 1 to get the NULL */
#endif

        /* cleanup */
        free(tmp);

        /* since a singleton spawned us, we need to harvest
         * any MCA params from the local environment so
         * we can pass them along to any subsequent daemons
         * we may start as the result of a comm_spawn
         */
        for (i=0; NULL != environ[i]; i++) {
            if (0 == strncmp(environ[i], "OMPI_MCA", 8)) {
                /* make a copy to manipulate */
                sysinfo = strdup(environ[i]);
                /* find the equal sign */
                nptr = strchr(sysinfo, '=');
                *nptr = '\0';
                nptr++;
                /* add the mca param to the orted cmd line */
                opal_argv_append_nosize(&orted_cmd_line, "-mca");
                opal_argv_append_nosize(&orted_cmd_line, &sysinfo[9]);
                opal_argv_append_nosize(&orted_cmd_line, nptr);
                free(sysinfo);
            }
        }
    }

    /* if we were given a pipe to monitor for singleton termination, set that up */
    if (orted_globals.singleton_died_pipe > 0) {
        /* register shutdown handler */
        opal_event_set(&pipe_handler,
                       orted_globals.singleton_died_pipe,
                       OPAL_EV_READ|OPAL_EV_PERSIST,
                       shutdown_callback,
                       &orted_globals.singleton_died_pipe);
        opal_event_add(&pipe_handler, NULL);
    }

    /* if we are not the HNP...the only time we will be an HNP
     * is if we are launched by a singleton to provide support
     * for it
     *
     * only do this if we were not given a regexp to launch - if
     * we were given one, we won't report back our existence
     * to the HNP, but instead will report when procs are launched
     * to avoid establishing an unnecessary direct connection back
     * to the HNP
     */
    if (!ORTE_PROC_IS_HNP && NULL == orted_launch_cmd) {
        /* send the information to the orted report-back point - this function
         * will process the data, but also counts the number of
         * orteds that reported back so the launch procedure can continue.
         * We need to do this at the last possible second as the HNP
         * can turn right around and begin issuing orders to us
         */

        buffer = OBJ_NEW(opal_buffer_t);
        /* for now, always include our contact info, even if we are using
         * static ports. Eventually, this will be removed
         */
        rml_uri = orte_rml.get_contact_info();
        if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &rml_uri, 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(ret);
            OBJ_RELEASE(buffer);
            goto DONE;
        }
        if (orte_timing) {
            int64_t secs, usecs;
            /* add our start time */
            secs = starttime.tv_sec;
            if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &secs, 1, OPAL_INT64))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(buffer);
                goto DONE;
            }
            usecs = starttime.tv_usec;
            if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &usecs, 1, OPAL_INT64))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(buffer);
                goto DONE;
            }
            /* get and send our setup time */
            gettimeofday(&setuptime, NULL);
            secs = setuptime.tv_sec - starttime.tv_sec;
            if (starttime.tv_usec <= setuptime.tv_usec) {
                usecs = setuptime.tv_usec - starttime.tv_usec;
            } else {
                secs--;
                usecs = 1000000 - starttime.tv_usec + setuptime.tv_usec;
            }
            if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &secs, 1, OPAL_INT64))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(buffer);
                goto DONE;
            }
            if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &usecs, 1, OPAL_INT64))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(buffer);
                goto DONE;
            }
            /* include the actual timestamp so the HNP can figure out how
             * long it took for this message to arrive
             */
            secs = setuptime.tv_sec;
            if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &secs, 1, OPAL_INT64))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(buffer);
                goto DONE;
            }
            usecs = setuptime.tv_usec;
            if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &usecs, 1, OPAL_INT64))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(buffer);
                goto DONE;
            }
        }

        /* If I have a parent, then first let him know my URI, and only
         * after report back to the HNP.
         */
        mca_base_param_reg_string_name("orte", "parent_uri",
                                       "URI for the parent if tree launch is enabled.",
                                       true, false, NULL,  &rml_uri);
        if (NULL != rml_uri) {
            orte_process_name_t parent;

            /* set the contact info into the hash table */
            if (ORTE_SUCCESS != (ret = orte_rml.set_contact_info(rml_uri))) {
                ORTE_ERROR_LOG(ret);
                free(rml_uri);
                OBJ_RELEASE(buffer);
                goto DONE;
            }
            ret = orte_rml_base_parse_uris(rml_uri, &parent, NULL );
            if( ORTE_SUCCESS != ret ) {
                ORTE_ERROR_LOG(ret);
                free(rml_uri);
                OBJ_RELEASE(buffer);
                goto DONE;
            }
            free(rml_uri);

            if( 0 > (ret = orte_rml.send_buffer(&parent, buffer,
                                                ORTE_RML_TAG_ORTED_CALLBACK, 0)) ) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(buffer);
                goto DONE;
            }
        } else {
            opal_list_item_t *item;
            opal_sysinfo_value_t *info;
            int32_t num_values;

            /* include our node name */
            opal_dss.pack(buffer, &orte_process_info.nodename, 1, OPAL_STRING);
            
            /* add number of values to the buffer */
            num_values = opal_list_get_size(&orte_odls_globals.sysinfo);
            opal_dss.pack(buffer, &num_values, 1, OPAL_INT32);
            /* add them to the buffer */
	    for (item = opal_list_get_first(&orte_odls_globals.sysinfo);
		 item != opal_list_get_end(&orte_odls_globals.sysinfo);
		 item = opal_list_get_next(item)) {
		info = (opal_sysinfo_value_t*)item;
		opal_dss.pack(buffer, &info->key, 1, OPAL_STRING);
		opal_dss.pack(buffer, &info->type, 1, OPAL_DATA_TYPE_T);
                if (OPAL_INT64 == info->type) {
                    opal_dss.pack(buffer, &(info->data.i64), 1, OPAL_INT64);
                } else if (OPAL_STRING == info->type) {
                    opal_dss.pack(buffer, &(info->data.str), 1, OPAL_STRING);
                }
            }
        }

        if (orte_daemon_bootstrap) {
            /* send to a different callback location as the
             * HNP didn't launch us and isn't waiting for a
             * callback
             */
            if (0 > (ret = orte_rml.send_buffer(ORTE_PROC_MY_HNP, buffer,
                                                ORTE_RML_TAG_BOOTSTRAP, 0))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(buffer);
                goto DONE;
            }
        } else {
            if (0 > (ret = orte_rml.send_buffer(ORTE_PROC_MY_HNP, buffer,
                                                ORTE_RML_TAG_ORTED_CALLBACK, 0))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(buffer);
                goto DONE;
            }            
        }
        OBJ_RELEASE(buffer);  /* done with this */
    }

    if (orte_debug_daemons_flag) {
        opal_output(0, "%s orted: up and running - waiting for commands!", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
    }

    /* if we were told to do a heartbeat, then setup to do so */
    if (0 < orted_globals.heartbeat) {
        ORTE_TIMER_EVENT(orted_globals.heartbeat, 0, orte_plm_base_heartbeat);
    }
    
    /* if we were given a launch string, then process it */
    if (NULL != orted_launch_cmd) {
        opal_buffer_t launch;
        int8_t flag;
        orte_daemon_cmd_flag_t command = ORTE_DAEMON_ADD_LOCAL_PROCS;
        OBJ_CONSTRUCT(&launch, opal_buffer_t);
        opal_dss.pack(&launch, &command, 1, ORTE_DAEMON_CMD);
        flag = 1;
        opal_dss.pack(&launch, &flag, 1, OPAL_INT8);
        opal_dss.pack(&launch, &orted_launch_cmd, 1, OPAL_STRING);
        ORTE_MESSAGE_EVENT(ORTE_PROC_MY_NAME, &launch, ORTE_RML_TAG_DAEMON, orte_daemon_cmd_processor);
        OBJ_DESTRUCT(&launch);
    }

    /* wait to hear we are done */
    opal_event_dispatch();

    /* should never get here, but if we do... */
 DONE:
    if (signals_set) {
        /* Release all local signal handlers */
        opal_event_del(&term_handler);
        opal_event_del(&int_handler);
#ifndef __WINDOWS__
        opal_signal_del(&sigusr1_handler);
        opal_signal_del(&sigusr2_handler);
#endif  /* __WINDOWS__ */
    }
    
    /* cleanup any lingering session directories */
    orte_session_dir_cleanup(ORTE_JOBID_WILDCARD);
    
    /* cleanup the triggers */
    OBJ_DESTRUCT(&orte_exit);

    /* Finalize and clean up ourselves */
    orte_finalize();
    return ret;
}

static void shutdown_signal(int fd, short flags, void *arg)
{
    /* trigger the call to shutdown callback to protect
     * against race conditions - the trigger event will
     * check the one-time lock
     */
    ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
    orte_trigger_event(&orte_exit);
}

static void shutdown_callback(int fd, short flags, void *arg)
{
    int ret;
    
    if (NULL != arg) {
        /* it's the singleton pipe...  remove that handler */
        opal_event_del(&pipe_handler);
    }
    
    if (orte_debug_daemons_flag) {
        opal_output(0, "%s orted: finalizing", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
    }
    
    /* cleanup */
    if (NULL != log_path) {
        unlink(log_path);
    }
    
    /* make sure our local procs are dead - but don't update their state
     * on the HNP as this may be redundant
     */
    orte_odls.kill_local_procs(NULL, false);
    
    /* cleanup the triggers */
    OBJ_DESTRUCT(&orte_exit);

    /* if we were ordered to abort, do so */
    if (orted_globals.abort) {
        opal_output(0, "%s is executing clean abort", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        /* do -not- call finalize as this will send a message to the HNP
         * indicating clean termination! Instead, just forcibly cleanup
         * the local session_dir tree and abort
         */
        orte_session_dir_cleanup(ORTE_JOBID_WILDCARD);
        abort();
    } else if ((int)ORTE_PROC_MY_NAME->vpid == orted_globals.fail) {
        opal_output(0, "%s is executing clean abnormal termination", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        /* do -not- call finalize as this will send a message to the HNP
         * indicating clean termination! Instead, just forcibly cleanup
         * the local session_dir tree and exit
         */
        orte_session_dir_cleanup(ORTE_JOBID_WILDCARD);
        exit(ORTE_ERROR_DEFAULT_EXIT_CODE);
    }

    if (signals_set) {
        /* Release all local signal handlers */
        opal_event_del(&epipe_handler);
        opal_event_del(&term_handler);
        opal_event_del(&int_handler);
#ifndef __WINDOWS__
        opal_signal_del(&sigusr1_handler);
        opal_signal_del(&sigusr2_handler);
#endif  /* __WINDOWS__ */
    }

    /* Finalize and clean up ourselves */
    ret = orte_finalize();
    exit(orte_exit_status);
}

/**
 * Deal with sigpipe errors
 */
static void epipe_signal_callback(int fd, short flags, void *arg)
{
    /* for now, we just announce and ignore them */
    opal_output(0, "%s reports a SIGPIPE error on fd %d",
                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), fd);
    return;
}

static void signal_callback(int fd, short event, void *arg)
{
    /* just ignore these signals */
}
