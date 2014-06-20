/* -*- C -*-
 *
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
 * Copyright (c) 2006-2007 Cisco Systems, Inc. All rights reserved.
 * Copyright (c) 2007-2009 Sun Microsystems, Inc. All rights reserved.
 * Copyright (c) 2007-2012 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <errno.h>
#include <signal.h>
#include <ctype.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif  /* HAVE_SYS_WAIT_H */
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */
#include <fcntl.h>
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#include "opal/event/event.h"
#include "opal/mca/installdirs/installdirs.h"
#include "opal/mca/base/base.h"
#include "opal/mca/paffinity/base/base.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/basename.h"
#include "opal/util/cmd_line.h"
#include "opal/util/opal_environ.h"
#include "opal/util/opal_getcwd.h"
#include "orte/util/show_help.h"
#include "opal/sys/atomic.h"
#if OPAL_ENABLE_FT_CR == 1
#include "opal/runtime/opal_cr.h"
#endif

#include "opal/version.h"
#include "opal/runtime/opal.h"
#include "opal/util/os_path.h"
#include "opal/util/path.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/dss/dss.h"

#include "orte/util/proc_info.h"
#include "orte/util/pre_condition_transports.h"
#include "orte/util/session_dir.h"
#include "orte/util/hnp_contact.h"

#include "orte/mca/odls/odls.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/grpcomm/grpcomm.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_data_server.h"
#include "orte/runtime/orte_locks.h"

/* ensure I can behave like a daemon */
#include "orte/orted/orted.h"

#include "orterun.h"

/* instance the standard MPIR interfaces */
#define MPIR_MAX_PATH_LENGTH 512
#define MPIR_MAX_ARG_LENGTH 1024
struct MPIR_PROCDESC *MPIR_proctable = NULL;
int MPIR_proctable_size = 0;
volatile int MPIR_being_debugged = 0;
volatile int MPIR_debug_state = 0;
int MPIR_i_am_starter = 0;
int MPIR_partial_attach_ok = 1;
char MPIR_executable_path[MPIR_MAX_PATH_LENGTH];
char MPIR_server_arguments[MPIR_MAX_ARG_LENGTH];
volatile int MPIR_forward_output = 0;
volatile int MPIR_forward_comm = 0;
char MPIR_attach_fifo[MPIR_MAX_PATH_LENGTH];
int MPIR_force_to_main = 0;
static void orte_debugger_dump(void);
#if !defined(__WINDOWS__)
static void orte_debugger_init_before_spawn(orte_job_t *jdata);
static void orte_debugger_init_after_spawn(orte_job_t *jdata);
static void attach_debugger(int fd, short event, void *arg);
static void build_debugger_args(orte_app_context_t *debugger);
static void open_fifo (void);
#endif
ORTE_DECLSPEC void* MPIR_Breakpoint(void);

/*
 * Breakpoint function for parallel debuggers
 */
void* MPIR_Breakpoint(void)
{
    return NULL;
}

/*
 * Globals
 */
static struct opal_event term_handler;
static struct opal_event int_handler;
static struct opal_event epipe_handler;
#ifndef __WINDOWS__
static struct opal_event sigusr1_handler;
static struct opal_event sigusr2_handler;
static struct opal_event sigtstp_handler;
static struct opal_event sigcont_handler;
#endif  /* __WINDOWS__ */
static orte_job_t *jdata;
static char *orterun_basename = NULL;
static int num_aborted = 0;
static int num_killed = 0;
static int num_failed_start = 0;
static char **global_mca_env = NULL;
static bool have_zero_np = false;
static orte_std_cntr_t total_num_apps = 0;
static bool want_prefix_by_default = (bool) ORTE_WANT_ORTERUN_PREFIX_BY_DEFAULT;
static opal_event_t *orterun_event, *orteds_exit_event;
static char *ompi_server=NULL;
static opal_event_t *abort_exit_event=NULL;
static bool forcibly_die = false;
static opal_event_t *timeout_ev=NULL;
static bool profile_is_set = false;
static bool signals_set=false;

/*
 * Globals
 */
struct orterun_globals_t orterun_globals;
static bool globals_init = false;

static opal_cmd_line_init_t cmd_line_init[] = {
    /* Various "obvious" options */
    { NULL, NULL, NULL, 'h', NULL, "help", 0,
      &orterun_globals.help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },
    { NULL, NULL, NULL, 'V', NULL, "version", 0,
      &orterun_globals.version, OPAL_CMD_LINE_TYPE_BOOL,
      "Print version and exit" },
    { NULL, NULL, NULL, 'v', NULL, "verbose", 0,
      &orterun_globals.verbose, OPAL_CMD_LINE_TYPE_BOOL,
      "Be verbose" },
    { NULL, NULL, NULL, 'q', NULL, "quiet", 0,
      &orterun_globals.quiet, OPAL_CMD_LINE_TYPE_BOOL,
      "Suppress helpful messages" },
    { NULL, NULL, NULL, '\0', "report-pid", "report-pid", 1,
      &orterun_globals.report_pid, OPAL_CMD_LINE_TYPE_STRING,
      "Printout pid on stdout [-], stderr [+], or a file [anything else]" },
    { NULL, NULL, NULL, '\0', "report-uri", "report-uri", 1,
      &orterun_globals.report_uri, OPAL_CMD_LINE_TYPE_STRING,
      "Printout URI on stdout [-], stderr [+], or a file [anything else]" },
    
    
    /* hetero apps */
    { "orte", "hetero", "apps", '\0', NULL, "hetero", 0,
        NULL, OPAL_CMD_LINE_TYPE_BOOL,
    "Indicates that multiple app_contexts are being provided that are a mix of 32/64 bit binaries" },
    
    /* select XML output */
    { "orte", "xml", "output", '\0', "xml", "xml", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Provide all output in XML format" },
    { "orte", "xml", "file", '\0', "xml-file", "xml-file", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Provide all output in XML format to the specified file" },

    /* tag output */
    { "orte", "tag", "output", '\0', "tag-output", "tag-output", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Tag all output with [job,rank]" },
    { "orte", "timestamp", "output", '\0', "timestamp-output", "timestamp-output", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Timestamp all application process output" },
    { "orte", "output", "filename", '\0', "output-filename", "output-filename", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Redirect output from application processes into filename.rank" },
    { "orte", "xterm", NULL, '\0', "xterm", "xterm", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Create a new xterm window and display output from the specified ranks there" },

    /* select stdin option */
    { NULL, NULL, NULL, '\0', "stdin", "stdin", 1,
      &orterun_globals.stdin_target, OPAL_CMD_LINE_TYPE_STRING,
      "Specify procs to receive stdin [rank, all, none] (default: 0, indicating rank 0)" },
    
    /* Specify the launch agent to be used */
    { "orte", "launch", "agent", '\0', "launch-agent", "launch-agent", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Command used to start processes on remote nodes (default: orted)" },
    
    /* Preload the binary on the remote machine */
    { NULL, NULL, NULL, 's', NULL, "preload-binary", 0,
      &orterun_globals.preload_binary, OPAL_CMD_LINE_TYPE_BOOL,
      "Preload the binary on the remote machine before starting the remote process." },

    /* Preload files on the remote machine */
    { NULL, NULL, NULL, '\0', NULL, "preload-files", 1,
      &orterun_globals.preload_files, OPAL_CMD_LINE_TYPE_STRING,
      "Preload the comma separated list of files to the remote machines current working directory before starting the remote process." },

    /* Where to Preload files on the remote machine */
    { NULL, NULL, NULL, '\0', NULL, "preload-files-dest-dir", 1,
      &orterun_globals.preload_files_dest_dir, OPAL_CMD_LINE_TYPE_STRING,
      "The destination directory to use in conjunction with --preload-files. By default the absolute and relative paths provided by --preload-files are used." },

    /* Use an appfile */
    { NULL, NULL, NULL, '\0', NULL, "app", 1,
      &orterun_globals.appfile, OPAL_CMD_LINE_TYPE_STRING,
      "Provide an appfile; ignore all other command line options" },

    /* Number of processes; -c, -n, --n, -np, and --np are all
       synonyms */
    { NULL, NULL, NULL, 'c', "np", "np", 1,
      &orterun_globals.num_procs, OPAL_CMD_LINE_TYPE_INT,
      "Number of processes to run" },
    { NULL, NULL, NULL, '\0', "n", "n", 1,
      &orterun_globals.num_procs, OPAL_CMD_LINE_TYPE_INT,
      "Number of processes to run" },
    
    /* Set a hostfile */
    { NULL, NULL, NULL, '\0', "hostfile", "hostfile", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a hostfile" },
    { NULL, NULL, NULL, '\0', "machinefile", "machinefile", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a hostfile" },
    { "orte", "default", "hostfile", '\0', "default-hostfile", "default-hostfile", 1,
        NULL, OPAL_CMD_LINE_TYPE_STRING,
    "Provide a default hostfile" },
    { "opal", "if", "do_not_resolve", '\0', "do-not-resolve", "do-not-resolve", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Do not attempt to resolve interfaces" },
    
    /* uri of Open MPI server, or at least where to get it */
    { NULL, NULL, NULL, '\0', "ompi-server", "ompi-server", 1,
      &orterun_globals.ompi_server, OPAL_CMD_LINE_TYPE_STRING,
      "Specify the URI of the Open MPI server, or the name of the file (specified as file:filename) that contains that info" },
    { NULL, NULL, NULL, '\0', "wait-for-server", "wait-for-server", 0,
      &orterun_globals.wait_for_server, OPAL_CMD_LINE_TYPE_BOOL,
      "If ompi-server is not already running, wait until it is detected (default: false)" },
    { NULL, NULL, NULL, '\0', "server-wait-time", "server-wait-time", 1,
      &orterun_globals.server_wait_timeout, OPAL_CMD_LINE_TYPE_INT,
      "Time in seconds to wait for ompi-server (default: 10 sec)" },
    
    { "carto", "file", "path", '\0', "cf", "cartofile", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a cartography file" },

    { "orte", "rankfile", NULL, '\0', "rf", "rankfile", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a rankfile file" },

    /* Export environment variables; potentially used multiple times,
       so it does not make sense to set into a variable */
    { NULL, NULL, NULL, 'x', NULL, NULL, 1,
      NULL, OPAL_CMD_LINE_TYPE_NULL,
      "Export an environment variable, optionally specifying a value (e.g., \"-x foo\" exports the environment variable foo and takes its value from the current environment; \"-x foo=bar\" exports the environment variable name foo and sets its value to \"bar\" in the started processes)" },

    /* Mapping options */
    { NULL, NULL, NULL, '\0', "bynode", "bynode", 0,
      &orterun_globals.by_node, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to assign processes round-robin by node" },
    { NULL, NULL, NULL, '\0', "byslot", "byslot", 0,
      &orterun_globals.by_slot, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to assign processes round-robin by slot (the default)" },
    { NULL, NULL, NULL, '\0', "bycore", "bycore", 0,
      &orterun_globals.by_slot, OPAL_CMD_LINE_TYPE_BOOL,
      "Alias for byslot" },
    { NULL, NULL, NULL, '\0', "bysocket", "bysocket", 0,
      &orterun_globals.by_socket, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to assign processes round-robin by socket" },
    { NULL, NULL, NULL, '\0', "byboard", "byboard", 0,
      &orterun_globals.by_slot, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to assign processes round-robin by board (equivalent to bynode if only 1 board/node)" },
    { "rmaps", "base", "pernode", '\0', "pernode", "pernode", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Launch one process per available node on the specified number of nodes [no -np => use all allocated nodes]" },
    { "rmaps", "base", "n_pernode", '\0', "npernode", "npernode", 1,
        NULL, OPAL_CMD_LINE_TYPE_INT,
        "Launch n processes per node on all allocated nodes" },
    { "rmaps", "base", "slot_list", '\0', "slot-list", "slot-list", 1,
        NULL, OPAL_CMD_LINE_TYPE_STRING,
        "List of processor IDs to bind MPI processes to (e.g., used in conjunction with rank files)" },
    { "rmaps", "base", "no_oversubscribe", '\0', "nooversubscribe", "nooversubscribe", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Nodes are not to be oversubscribed, even if the system supports such operation"},
    { "rmaps", "base", "loadbalance", '\0', "loadbalance", "loadbalance", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Balance total number of procs across all allocated nodes"},
    { "rmaps", "base", "display_map", '\0', "display-map", "display-map", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Display the process map just before launch"},
    { "rmaps", "base", "display_devel_map", '\0', "display-devel-map", "display-devel-map", 0,
       NULL, OPAL_CMD_LINE_TYPE_BOOL,
       "Display a detailed process map (mostly intended for developers) just before launch"},
    { NULL, NULL, NULL, 'H', "host", "host", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "List of hosts to invoke processes on" },
    { "rmaps", "base", "no_schedule_local", '\0', "nolocal", "nolocal", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Do not run any MPI applications on the local node" },
    { "rmaps", "base", "cpus_per_rank", '\0', "cpus-per-proc", "cpus-per-proc", 1,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "Number of cpus to use for each process [default=1]" },
    { "rmaps", "base", "cpus_per_rank", '\0', "cpus-per-rank", "cpus-per-rank", 1,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "Synonym for cpus-per-proc" },
    { "rmaps", "base", "n_perboard", '\0', "nperboard", "nperboard", 1,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "Launch n processes per board on all allocated nodes" },
    { "rmaps", "base", "n_persocket", '\0', "npersocket", "npersocket", 1,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "Launch n processes per socket on all allocated nodes" },

    /* binding options */
    { NULL, NULL, NULL, '\0', "bind-to-none", "bind-to-none", 0,
      &orterun_globals.bind_to_none, OPAL_CMD_LINE_TYPE_BOOL,
      "Do not bind processes to cores or sockets (default)" },
    { NULL, NULL, NULL, '\0', "bind-to-core", "bind-to-core", 0,
      &orterun_globals.bind_to_core, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to bind processes to specific cores" },
    { NULL, NULL, NULL, '\0', "bind-to-board", "bind-to-board", 0,
      &orterun_globals.bind_to_board, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to bind processes to specific boards (meaningless on 1 board/node)" },
    { NULL, NULL, NULL, '\0', "bind-to-socket", "bind-to-socket", 0,
      &orterun_globals.bind_to_socket, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to bind processes to sockets" },
    { "rmaps", "base", "stride", '\0', "stride", "stride", 1,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "When binding multiple cores to a rank, the step size to use between cores [default: 1]" },
    { "orte", "report", "bindings", '\0', "report-bindings", "report-bindings", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to report process bindings to stderr" },

    /* Allocation options */
    { "ras", "base", "display_alloc", '\0', "display-allocation", "display-allocation", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Display the allocation being used by this job"},
    { "ras", "base", "display_devel_alloc", '\0', "display-devel-allocation", "display-devel-allocation", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Display a detailed list (mostly intended for developers) of the allocation being used by this job"},

    /* cluster hardware info */
    { "orte", "num", "boards", '\0', "num-boards", "num-boards", 1,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "Number of processor boards/node (1-256) [default: 1]"},
    { "orte", "num", "sockets", '\0', "num-sockets", "num-sockets", 1,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "Number of sockets/board (1-256) [default: 1]"},
    { "orte", "num", "cores", '\0', "num-cores", "num-cores", 1,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "Number of cores/socket (1-256) [default: 1]"},

    /* mpiexec-like arguments */
    { NULL, NULL, NULL, '\0', "wdir", "wdir", 1,
      &orterun_globals.wdir, OPAL_CMD_LINE_TYPE_STRING,
      "Set the working directory of the started processes" },
    { NULL, NULL, NULL, '\0', "wd", "wd", 1,
      &orterun_globals.wdir, OPAL_CMD_LINE_TYPE_STRING,
      "Synonym for --wdir" },
    { NULL, NULL, NULL, '\0', "path", "path", 1,
      &orterun_globals.path, OPAL_CMD_LINE_TYPE_STRING,
      "PATH to be used to look for executables to start processes" },

    /* User-level debugger arguments */
    { NULL, NULL, NULL, '\0', "tv", "tv", 0,
      &orterun_globals.debugger, OPAL_CMD_LINE_TYPE_BOOL,
      "Deprecated backwards compatibility flag; synonym for \"--debug\"" },
    { NULL, NULL, NULL, '\0', "debug", "debug", 0,
      &orterun_globals.debugger, OPAL_CMD_LINE_TYPE_BOOL,
      "Invoke the user-level debugger indicated by the orte_base_user_debugger MCA parameter" },
    { "orte", "base", "user_debugger", '\0', "debugger", "debugger", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Sequence of debuggers to search for when \"--debug\" is used" },

    /* OpenRTE arguments */
    { "orte", "debug", NULL, 'd', "debug-devel", "debug-devel", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable debugging of OpenRTE" },
    
    { "orte", "debug", "daemons", '\0', "debug-daemons", "debug-daemons", 0,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "Enable debugging of any OpenRTE daemons used by this application" },
    
    { "orte", "debug", "daemons_file", '\0', "debug-daemons-file", "debug-daemons-file", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable debugging of any OpenRTE daemons used by this application, storing output in files" },
    
    { "orte", "leave", "session_attached", '\0', "leave-session-attached", "leave-session-attached", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable debugging of OpenRTE" },

    { NULL, NULL, NULL, '\0', "tmpdir", "tmpdir", 1,
      &orte_process_info.tmpdir_base, OPAL_CMD_LINE_TYPE_STRING,
      "Set the root for the session directory tree for orterun ONLY" },

    { "orte", "do_not", "launch", '\0', "do-not-launch", "do-not-launch", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Perform all necessary operations to prepare to launch the application, but do not actually launch it" },
    
    { NULL, NULL, NULL, '\0', NULL, "prefix", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Prefix where Open MPI is installed on remote nodes" },
    { NULL, NULL, NULL, '\0', NULL, "noprefix", 0,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Disable automatic --prefix behavior" },

    { "orte", "report", "launch_progress", '\0', "show-progress", "show-progress", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Output a brief periodic report on launch progress" },

    { "orte", "use", "regexp", '\0', "use-regexp", "use-regexp", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Use regular expressions for launch" },

    { "orte", "report", "events", '\0', "report-events", "report-events", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Report events to a tool listening at the specified URI" },

    /* End of list */
    { NULL, NULL, NULL, '\0', NULL, NULL, 0,
      NULL, OPAL_CMD_LINE_TYPE_NULL, NULL }
};

/*
 * Local functions
 */
static void job_completed(int trigpipe, short event, void *arg);
static void abort_signal_callback(int fd, short flags, void *arg);
static void abort_exit_callback(int fd, short event, void *arg);
static void epipe_signal_callback(int fd, short flags, void *arg);
static void signal_forward_callback(int fd, short event, void *arg);
static int create_app(int argc, char* argv[], orte_app_context_t **app,
                      bool *made_app, char ***app_env);
static int init_globals(void);
static int parse_globals(int argc, char* argv[], opal_cmd_line_t *cmd_line);
static int parse_locals(int argc, char* argv[]);
static int parse_appfile(char *filename, char ***env);
static void dump_aborted_procs(void);
static void just_quit(int fd, short ign, void *arg);

static void run_debugger(char *basename, opal_cmd_line_t *cmd_line, 
			 int argc, char *argv[], int num_procs);

int orterun(int argc, char *argv[])
{
    int rc;
    opal_cmd_line_t cmd_line;
    char *tmp_env_var = NULL;
    char *param;
    orte_app_context_t *app;

    /* find our basename (the name of the executable) so that we can
       use it in pretty-print error messages */
    orterun_basename = opal_basename(argv[0]);

    /* Setup and parse the command line */
    init_globals();
    opal_cmd_line_create(&cmd_line, cmd_line_init);
    mca_base_cmd_line_setup(&cmd_line);
    if (ORTE_SUCCESS != (rc = opal_cmd_line_parse(&cmd_line, true,
                                                   argc, argv)) ) {
        return rc;
    }

    /*
     * Since this process can now handle MCA/GMCA parameters, make sure to
     * process them.
     */
    mca_base_cmd_line_process_args(&cmd_line, &environ, &environ);
    
    /* make sure that opal_profile is -not- set for us locally as
     * we really only want to profile MPI apps. However, if it is
     * set, remember it so we can add it to the apps environment later
     */
    if (NULL != getenv("OMPI_MCA_opal_profile")) {
        putenv("OMPI_MCA_opal_profile=0");
        profile_is_set = true;
        /* ensure that I know to turn on my profile receive! */
        putenv("OMPI_MCA_orte_grpcomm_recv_on=1");
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
    /* Need to initialize OPAL so that install_dirs are filled in */
    if (OPAL_SUCCESS != opal_init_util(&argc, &argv)) {
        exit(1);
    }
    
    /* may look strange, but the way we handle prefix is a little weird
     * and probably needs to be addressed more fully at some future point.
     * For now, we have a conflict between app_files and cmd line usage.
     * Since app_files are used by the C/R system, we will make an
     * adjustment here to avoid perturbing that system.
     *
     * We cannot just have the cmd line parser place any found value
     * in the global struct as the app_file parser would replace it.
     * So handle this specific cmd line option manually.
     */
    orterun_globals.prefix = NULL;
    orterun_globals.path_to_mpirun = NULL;
    if (opal_cmd_line_is_taken(&cmd_line, "prefix") ||
        '/' == argv[0][0] || want_prefix_by_default) {
        size_t param_len;

        if ('/' == argv[0][0]) {
            char* tmp_basename = NULL;
            /* If they specified an absolute path, strip off the
               /bin/<exec_name>" and leave just the prefix */
            orterun_globals.path_to_mpirun = opal_dirname(argv[0]);
            /* Quick sanity check to ensure we got
               something/bin/<exec_name> and that the installation
               tree is at least more or less what we expect it to
               be */
            tmp_basename = opal_basename(orterun_globals.path_to_mpirun);
            if (0 == strcmp("bin", tmp_basename)) {
                char* tmp = orterun_globals.path_to_mpirun;
                orterun_globals.path_to_mpirun = opal_dirname(tmp);
                free(tmp);
            } else {
                free(orterun_globals.path_to_mpirun);
                orterun_globals.path_to_mpirun = NULL;
            }
            free(tmp_basename);
        }
        /* if both are given, check to see if they match */
        if (opal_cmd_line_is_taken(&cmd_line, "prefix") && NULL != orterun_globals.path_to_mpirun) {
            /* if they don't match, then that merits a warning */
            param = strdup(opal_cmd_line_get_param(&cmd_line, "prefix", 0, 0));
            if (0 != strcmp(param, orterun_globals.path_to_mpirun)) {
                orte_show_help("help-orterun.txt", "orterun:double-prefix",
                               true, orterun_basename, param,
                               orterun_globals.path_to_mpirun, orterun_basename);
                /* use the prefix over the path-to-mpirun so that
                 * people can specify the backend prefix as different
                 * from the local one
                 */
                free(orterun_globals.path_to_mpirun);
                orterun_globals.path_to_mpirun = NULL;
            } else {
                /* since they match, just use param */
                free(orterun_globals.path_to_mpirun);
            }
        } else if (NULL != orterun_globals.path_to_mpirun) {
            param = orterun_globals.path_to_mpirun;
        } else if (opal_cmd_line_is_taken(&cmd_line, "prefix")){
            /* must be --prefix alone */
            param = strdup(opal_cmd_line_get_param(&cmd_line, "prefix", 0, 0));
        } else {
            /* --enable-orterun-prefix-default was given to orterun */
            param = strdup(opal_install_dirs.prefix);
        }

        if (NULL != param) {
            /* "Parse" the param, aka remove superfluous path_sep. */
            param_len = strlen(param);
            while (0 == strcmp (OPAL_PATH_SEP, &(param[param_len-1]))) {
                param[param_len-1] = '\0';
                param_len--;
                if (0 == param_len) {
                    orte_show_help("help-orterun.txt", "orterun:empty-prefix",
                                   true, orterun_basename, orterun_basename);
                    return ORTE_ERR_FATAL;
                }
            }

            orterun_globals.prefix = strdup(param);
            free(param);
        }
        want_prefix_by_default = true;
    }

    /* setup the exit triggers */
    OBJ_CONSTRUCT(&orte_exit, orte_trigger_event_t);
    OBJ_CONSTRUCT(&orteds_exit, orte_trigger_event_t);
    
    /* flag that I am the HNP - needs to be done prior to
     * registering params
     */
    orte_process_info.proc_type = ORTE_PROC_HNP;

    /* Setup MCA params */
    orte_register_params();

    /* Check for some "global" command line params */
    parse_globals(argc, argv, &cmd_line);
    OBJ_DESTRUCT(&cmd_line);

    /* create a new job object to hold the info for this one - the
     * jobid field will be filled in by the PLM when the job is
     * launched
     */
    jdata = OBJ_NEW(orte_job_t);
    if (NULL == jdata) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    /* check what user wants us to do with stdin */
    if (0 == strcmp(orterun_globals.stdin_target, "all")) {
        jdata->stdin_target = ORTE_VPID_WILDCARD;
    } else if (0 == strcmp(orterun_globals.stdin_target, "none")) {
        jdata->stdin_target = ORTE_VPID_INVALID;
    } else {
        jdata->stdin_target = strtoul(orterun_globals.stdin_target, NULL, 10);
    }
    
    /* Parse each app, adding it to the job object */
    parse_locals(argc, argv);
    
    if (0 == jdata->num_apps) {
        /* This should never happen -- this case should be caught in
        create_app(), but let's just double check... */
        orte_show_help("help-orterun.txt", "orterun:nothing-to-do",
                       true, orterun_basename);
        exit(ORTE_ERROR_DEFAULT_EXIT_CODE);
    }

    /* save the environment for launch purposes. This MUST be
     * done so that we can pass it to any local procs we
     * spawn - otherwise, those local procs won't see any
     * non-MCA envars were set in the enviro prior to calling
     * orterun
     */
    orte_launch_environ = opal_argv_copy(environ);
    
#if OPAL_ENABLE_FT_CR == 1
    /* Disable OPAL CR notifications for this tool */
    opal_cr_set_enabled(false);
    tmp_env_var = mca_base_param_env_var("opal_cr_is_tool");
    opal_setenv(tmp_env_var,
                "1",
                true, &environ);
    free(tmp_env_var);
#endif
    tmp_env_var = NULL; /* Silence compiler warning */

    /* Intialize our Open RTE environment
     * Set the flag telling orte_init that I am NOT a
     * singleton, but am "infrastructure" - prevents setting
     * up incorrect infrastructure that only a singleton would
     * require
     */
    if (ORTE_SUCCESS != (rc = orte_init(&argc, &argv, ORTE_PROC_HNP))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }    

    /* check for request to report uri */
    if (NULL != orterun_globals.report_uri) {
        FILE *fp;
        char *rml_uri;
        rml_uri = orte_rml.get_contact_info();
        if (0 == strcmp(orterun_globals.report_uri, "-")) {
            /* if '-', then output to stdout */
            printf("%s\n",  (NULL == rml_uri) ? "NULL" : rml_uri);
        } else if (0 == strcmp(orterun_globals.report_uri, "+")) {
            /* if '+', output to stderr */
            fprintf(stderr, "%s\n",  (NULL == rml_uri) ? "NULL" : rml_uri);
        } else {
            fp = fopen(orterun_globals.report_uri, "w");
            if (NULL == fp) {
                orte_show_help("help-orterun.txt", "orterun:write_file", false,
                               orterun_basename, "uri", orterun_globals.report_uri);
                exit(0);
            }
            fprintf(fp, "%s\n", (NULL == rml_uri) ? "NULL" : rml_uri);
            fclose(fp);
        }
        if (NULL != rml_uri) {
            free(rml_uri);
        }        
    }
    
    /* Change the default behavior of libevent such that we want to
     continually block rather than blocking for the default timeout
     and then looping around the progress engine again.  There
     should be nothing in the orted that cannot block in libevent
     until "something" happens (i.e., there's no need to keep
     cycling through progress because the only things that should
     happen will happen in libevent).  This is a minor optimization,
     but what the heck... :-) */
    opal_progress_set_event_flag(OPAL_EVLOOP_ONCE);
    
    /* setup an event we can wait for that will tell
     * us to terminate - both normal and abnormal
     * termination will call us here. Use the
     * same exit fd as the daemon does so that orted_comm
     * can cause either of us to exit since we share that code
     */
    if (ORTE_SUCCESS != (rc = orte_wait_event(&orterun_event, &orte_exit, "job_complete", job_completed))) {
        orte_show_help("help-orterun.txt", "orterun:event-def-failed", true,
                       orterun_basename, ORTE_ERROR_NAME(rc));
        ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
        goto DONE;
    }
    
    /* setup an event that will
     * trigger when the orteds are gone and tell the orteds that it is
     * okay to finalize and exit, we are done with them.
     * We set this up here in order to provide a way for us to
     * wakeup and terminate should the daemons themselves fail to launch,
     * and before we define signal handlers since they will call the
     * exit event trigger!
     */
    if (ORTE_SUCCESS != (rc = orte_wait_event(&orteds_exit_event, &orteds_exit, "orted_exit", just_quit))) {
        orte_show_help("help-orterun.txt", "orterun:event-def-failed", true,
                       orterun_basename, ORTE_ERROR_NAME(rc));
        goto DONE;
    }

#ifndef __WINDOWS__
    /* setup callback for SIGPIPE */
    opal_signal_set(&epipe_handler, SIGPIPE,
                    epipe_signal_callback, &epipe_handler);
    opal_signal_add(&epipe_handler, NULL);
    /** setup callbacks for abort signals - from this point
     * forward, we need to abort in a manner that allows us
     * to cleanup
     */
    opal_signal_set(&term_handler, SIGTERM,
                    abort_signal_callback, &term_handler);
    opal_signal_add(&term_handler, NULL);
    opal_signal_set(&int_handler, SIGINT,
                    abort_signal_callback, &int_handler);
    opal_signal_add(&int_handler, NULL);

    /** setup callbacks for signals we should foward */
    opal_signal_set(&sigusr1_handler, SIGUSR1,
                    signal_forward_callback, &sigusr1_handler);
    opal_signal_add(&sigusr1_handler, NULL);
    opal_signal_set(&sigusr2_handler, SIGUSR2,
                    signal_forward_callback, &sigusr2_handler);
    opal_signal_add(&sigusr2_handler, NULL);
    if (orte_forward_job_control) {
        opal_signal_set(&sigtstp_handler, SIGTSTP,
                        signal_forward_callback, &sigtstp_handler);
        opal_signal_add(&sigtstp_handler, NULL);
        opal_signal_set(&sigcont_handler, SIGCONT,
                        signal_forward_callback, &sigcont_handler);
        opal_signal_add(&sigcont_handler, NULL);
    }
#endif  /* __WINDOWS__ */
    
    signals_set = true;
    
    /* If we have a prefix, then modify the PATH and
        LD_LIBRARY_PATH environment variables in our copy. This
        will ensure that any locally-spawned children will
        have our executables and libraries in their path

        For now, default to the prefix_dir provided in the first app_context.
        Since there always MUST be at least one app_context, we are safe in
        doing this.
    */
    app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, 0);
    if (NULL != app->prefix_dir) {
        char *oldenv, *newenv, *lib_base, *bin_base;
        
        lib_base = opal_basename(opal_install_dirs.libdir);
        bin_base = opal_basename(opal_install_dirs.bindir);

        /* Reset PATH */
        newenv = opal_os_path( false, app->prefix_dir, bin_base, NULL );
        oldenv = getenv("PATH");
        if (NULL != oldenv) {
            char *temp;
            asprintf(&temp, "%s:%s", newenv, oldenv );
            free( newenv );
            newenv = temp;
        }
        opal_setenv("PATH", newenv, true, &orte_launch_environ);
        if (orte_debug_flag) {
            opal_output(0, "%s: reset PATH: %s", orterun_basename, newenv);
        }
        free(newenv);
        free(bin_base);
        
        /* Reset LD_LIBRARY_PATH */
        newenv = opal_os_path( false, app->prefix_dir, lib_base, NULL );
        oldenv = getenv("LD_LIBRARY_PATH");
        if (NULL != oldenv) {
            char* temp;
            asprintf(&temp, "%s:%s", newenv, oldenv);
            free(newenv);
            newenv = temp;
        }
        opal_setenv("LD_LIBRARY_PATH", newenv, true, &orte_launch_environ);
        if (orte_debug_flag) {
            opal_output(0, "%s: reset LD_LIBRARY_PATH: %s",
                        orterun_basename, newenv);
        }
        free(newenv);
        free(lib_base);
    }
    
    /* pre-condition any network transports that require it */
    if (ORTE_SUCCESS != (rc = orte_pre_condition_transports(jdata))) {
        ORTE_ERROR_LOG(rc);
        orte_show_help("help-orterun.txt", "orterun:precondition", false,
                       orterun_basename, NULL, NULL, rc);
        ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
        goto DONE;
    }

    /* setup to listen for commands sent specifically to me, even though I would probably
     * be the one sending them! Unfortunately, since I am a participating daemon,
     * there are times I need to send a command to "all daemons", and that means *I* have
     * to receive it too
     */
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DAEMON,
                                 ORTE_RML_NON_PERSISTENT, orte_daemon_recv, NULL);
    if (rc != ORTE_SUCCESS && rc != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(rc);
        ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
        goto DONE;
    }
    
    /* setup the data server */
    if (ORTE_SUCCESS != (rc = orte_data_server_init())) {
        ORTE_ERROR_LOG(rc);
        ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
        goto DONE;
    }
    
    /* if an uri for the ompi-server was provided, set the route */
    if (NULL != ompi_server) {
        opal_buffer_t buf;
        /* setup our route to the server */
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        opal_dss.pack(&buf, &ompi_server, 1, OPAL_STRING);
        if (ORTE_SUCCESS != (rc = orte_rml_base_update_contact_info(&buf))) {
            ORTE_ERROR_LOG(rc);
            ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
            goto DONE;
        }
        OBJ_DESTRUCT(&buf);        
        /* check if we are to wait for the server to start - resolves
         * a race condition that can occur when the server is run
         * as a background job - e.g., in scripts
         */
        if (orterun_globals.wait_for_server) {
            /* ping the server */
            struct timeval timeout;
            timeout.tv_sec = orterun_globals.server_wait_timeout;
            timeout.tv_usec = 0;
            if (ORTE_SUCCESS != (rc = orte_rml.ping(ompi_server, &timeout))) {
                /* try it one more time */
                if (ORTE_SUCCESS != (rc = orte_rml.ping(ompi_server, &timeout))) {
                    /* okay give up */
                    orte_show_help("help-orterun.txt", "orterun:server-not-found", true,
                                   orterun_basename, ompi_server,
                                   (long)orterun_globals.server_wait_timeout,
                                   ORTE_ERROR_NAME(rc));
                    ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
                    goto DONE;
                }
            }
        }
    }
    
#if !defined(__WINDOWS__)
    /* setup for debugging */
    orte_debugger_init_before_spawn(jdata);
#endif

    /* Spawn the job */
    rc = orte_plm.spawn(jdata);
    
#if !defined(__WINDOWS__)
    /* complete debugger interface */
    orte_debugger_init_after_spawn(jdata);
#endif
    
    /* now wait until the termination event fires */
    opal_event_dispatch();
    
    /* we only reach this point by jumping there due
     * to an error - so just cleanup and leave
     */
DONE:
    ORTE_UPDATE_EXIT_STATUS(orte_exit_status);
    just_quit(0,0,NULL);
    return orte_exit_status;
}

static void job_completed(int trigpipe, short event, void *arg)
{
    int rc;
    orte_job_t *daemons;
    
    /* if the abort exit event is set, delete it */
    if (NULL != abort_exit_event) {
        opal_evtimer_del(abort_exit_event);
        free(abort_exit_event);
    }
    
    /* if we never launched, just skip this part to avoid
     * meaningless error messages
     */
    if (orte_never_launched) {
        rc = orte_exit_status;
        goto DONE;
    }
    
    if (0 != orte_exit_status) {
        /* abnormal termination of some kind */
        dump_aborted_procs();
        /* If we showed more abort messages than were allowed,
        show a followup message here */
        if (num_failed_start > 1) {
            if (orte_xml_output) {
                fprintf(orte_xml_fp, "<stderr>");
            }
            fprintf(orte_xml_fp, "%d total process%s failed to start",
                   num_failed_start, ((num_failed_start > 1) ? "es" : ""));
            if (orte_xml_output) {
                fprintf(orte_xml_fp, "&#010;</stderr>");
            }
            fprintf(orte_xml_fp, "\n");
        }
        if (num_aborted > 1) {
            if (orte_xml_output) {
                fprintf(orte_xml_fp, "<stderr>");
            }
            fprintf(orte_xml_fp, "%d total process%s aborted",
                   num_aborted, ((num_aborted > 1) ? "es" : ""));
            if (orte_xml_output) {
                fprintf(orte_xml_fp, "&#010;</stderr>");
            }
            fprintf(orte_xml_fp, "\n");
        }
        if (num_killed > 1) {
            if (orte_xml_output) {
                fprintf(orte_xml_fp, "<stderr>");
            }
            fprintf(orte_xml_fp, "%d total process%s killed (some possibly by %s during cleanup)",
                   num_killed, ((num_killed > 1) ? "es" : ""), orterun_basename);
            if (orte_xml_output) {
                fprintf(orte_xml_fp, "&#010;</stderr>");
            }
            fprintf(orte_xml_fp, "\n");
        }
    }
    
    if (ORTE_SUCCESS != (rc = orte_plm.terminate_orteds())) {        
        /* since we know that the sends didn't completely go out,
         * we know that the barrier will never complete. Add a timeout so
         * that those daemons that can respond have a chance to do
         * so
         */
        /* get the orted job data object */
        if (NULL == (daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
            /* we are totally hozed */
            goto DONE;
        }
        ORTE_DETECT_TIMEOUT(&timeout_ev, daemons->num_procs,
                            orte_timeout_usec_per_proc,
                            orte_max_timeout, just_quit);
    }
    
    /* ensure all the orteds depart together */
    orte_grpcomm.onesided_barrier();

DONE:
    ORTE_UPDATE_EXIT_STATUS(rc);
    just_quit(0, 0, NULL);
}

static void just_quit(int fd, short ign, void *arg)
{
    /* if the orted exit event is set, delete it */
    if (NULL != orteds_exit_event) {
        opal_evtimer_del(orteds_exit_event);
        free(orteds_exit_event);
    }
    
    if (signals_set) {
        /* Remove the epipe handler */
        opal_signal_del(&epipe_handler);
        /* Remove the TERM and INT signal handlers */
        opal_signal_del(&term_handler);
        opal_signal_del(&int_handler);
#ifndef __WINDOWS__
        /** Remove the USR signal handlers */
        opal_signal_del(&sigusr1_handler);
        opal_signal_del(&sigusr2_handler);
        if (orte_forward_job_control) {
            opal_signal_del(&sigtstp_handler);
            opal_signal_del(&sigcont_handler);
        }
#endif  /* __WINDOWS__ */
        signals_set = false;
    }

    /* whack any lingering session directory files from our jobs */
    orte_session_dir_cleanup(ORTE_JOBID_WILDCARD);
    
    /* cleanup our data server */
    orte_data_server_finalize();
    
    /* cleanup and leave */
    orte_finalize();
    
    free(orterun_basename);
    if (orte_debug_flag) {
        fprintf(stderr, "orterun: exiting with status %d\n", orte_exit_status);
    }
    exit(orte_exit_status);
}


/*
 * On abnormal termination - dump the
 * exit status of the aborted procs.
 */

static void dump_aborted_procs(void)
{
    orte_std_cntr_t i, n;
    orte_proc_t *proc, **procs;
    orte_app_context_t **apps;
    orte_job_t **jobs, *job;
    bool found=false;
    
    /* find the job that caused the problem - be sure to start the loop
     * at 1 as the daemons are in 0 and will clearly be "running", so no
     * point in checking them
     */
    jobs = (orte_job_t**)orte_job_data->addr;
    for (n=1; n < orte_job_data->size; n++) {
        if (NULL == jobs[n]) {
            /* the array is no longer left-justified, so we have to continue */
            continue;
        }
        if (ORTE_JOB_STATE_UNDEF != jobs[n]->state &&
            ORTE_JOB_STATE_INIT != jobs[n]->state &&
            ORTE_JOB_STATE_LAUNCHED != jobs[n]->state &&
            ORTE_JOB_STATE_RUNNING != jobs[n]->state &&
            ORTE_JOB_STATE_TERMINATED != jobs[n]->state &&
            ORTE_JOB_STATE_ABORT_ORDERED != jobs[n]->state) {
            /* this is a guilty party */
            job = jobs[n];
            proc = job->aborted_proc;
            procs = (orte_proc_t**)job->procs->addr;
            apps = (orte_app_context_t**)job->apps->addr;
            /* flag that we found at least one */
            found = true;
            /* cycle through and count the number that were killed or aborted */
            for (i=0; i < job->procs->size; i++) {
                if (NULL == procs[i]) {
                    /* array is left-justfied - we are done */
                    break;
                }
                if (ORTE_PROC_STATE_FAILED_TO_START == procs[i]->state) {
                    ++num_failed_start;
                } else if (ORTE_PROC_STATE_ABORTED == procs[i]->state) {
                    ++num_aborted;
                } else if (ORTE_PROC_STATE_ABORTED_BY_SIG == procs[i]->state) {
                    ++num_killed;
                }
            }
            if (ORTE_JOB_STATE_FAILED_TO_START == job->state) {
                if (NULL == proc) {
                    orte_show_help("help-orterun.txt", "orterun:proc-failed-to-start-no-status-no-node", true,
                                   orterun_basename);
                    return;
                }
                if (ORTE_ERR_SYS_LIMITS_PIPES == proc->exit_code) {
                    orte_show_help("help-orterun.txt", "orterun:sys-limit-pipe", true,
                                   orterun_basename, proc->node->name,
                                   (unsigned long)proc->name.vpid);
                } else if (ORTE_ERR_PIPE_SETUP_FAILURE == proc->exit_code) {
                    orte_show_help("help-orterun.txt", "orterun:pipe-setup-failure", true,
                                   orterun_basename, proc->node->name,
                                   (unsigned long)proc->name.vpid);
                } else if (ORTE_ERR_SYS_LIMITS_CHILDREN == proc->exit_code) {
                    orte_show_help("help-orterun.txt", "orterun:sys-limit-children", true,
                                   orterun_basename, proc->node->name,
                                   (unsigned long)proc->name.vpid);
                } else if (ORTE_ERR_FAILED_GET_TERM_ATTRS == proc->exit_code) {
                    orte_show_help("help-orterun.txt", "orterun:failed-term-attrs", true,
                                   orterun_basename, proc->node->name,
                                   (unsigned long)proc->name.vpid);
                } else if (ORTE_ERR_WDIR_NOT_FOUND == proc->exit_code) {
                    orte_show_help("help-orterun.txt", "orterun:wdir-not-found", true,
                                   orterun_basename, apps[proc->app_idx]->cwd,
                                   proc->node->name, (unsigned long)proc->name.vpid);
                } else if (ORTE_ERR_EXE_NOT_FOUND == proc->exit_code) {
                    orte_show_help("help-orterun.txt", "orterun:exe-not-found", true,
                                   orterun_basename, apps[proc->app_idx]->app,
                                   proc->node->name, (unsigned long)proc->name.vpid);
                } else if (ORTE_ERR_EXE_NOT_ACCESSIBLE == proc->exit_code) {
                    orte_show_help("help-orterun.txt", "orterun:exe-not-accessible", true,
                                   orterun_basename, apps[proc->app_idx]->app, proc->node->name,
                                   (unsigned long)proc->name.vpid);
                } else if (ORTE_ERR_PIPE_READ_FAILURE == proc->exit_code) {
                    orte_show_help("help-orterun.txt", "orterun:pipe-read-failure", true,
                                   orterun_basename, proc->node->name, (unsigned long)proc->name.vpid);
                } else if (0 != proc->exit_code) {
                    orte_show_help("help-orterun.txt", "orterun:proc-failed-to-start", true,
                                   orterun_basename, ORTE_ERROR_NAME(proc->exit_code), proc->node->name,
                                   (unsigned long)proc->name.vpid);
                } else if (ORTE_ERR_SOCKET_NOT_AVAILABLE == proc->exit_code) {
                    orte_show_help("help-orterun.txt", "orterun:proc-socket-not-avail", true,
                                   orterun_basename, ORTE_ERROR_NAME(proc->exit_code), proc->node->name,
                                   (unsigned long)proc->name.vpid);
                } else {
                    orte_show_help("help-orterun.txt", "orterun:proc-failed-to-start-no-status", true,
                                   orterun_basename, proc->node->name);
                }
            } else if (ORTE_JOB_STATE_ABORTED == job->state) {
                if (NULL == proc) {
                    orte_show_help("help-orterun.txt", "orterun:proc-aborted-unknown", true,
                                   orterun_basename);
                } else {
                    orte_show_help("help-orterun.txt", "orterun:proc-ordered-abort", true,
                                   orterun_basename, (unsigned long)proc->name.vpid, (unsigned long)proc->pid,
                                   proc->node->name, orterun_basename);
                }
            } else if (ORTE_JOB_STATE_ABORTED_BY_SIG == job->state) {  /* aborted by signal */
                if (NULL == proc) {
                    orte_show_help("help-orterun.txt", "orterun:proc-aborted-signal-unknown", true,
                                   orterun_basename);
                } else {
#ifdef HAVE_STRSIGNAL
                    if (NULL != strsignal(WTERMSIG(proc->exit_code))) {
                        orte_show_help("help-orterun.txt", "orterun:proc-aborted-strsignal", true,
                                       orterun_basename, (unsigned long)proc->name.vpid, (unsigned long)proc->pid,
                                       proc->node->name, WTERMSIG(proc->exit_code), 
                                       strsignal(WTERMSIG(proc->exit_code)));
                    } else {
#endif
                        orte_show_help("help-orterun.txt", "orterun:proc-aborted", true,
                                       orterun_basename, (unsigned long)proc->name.vpid, (unsigned long)proc->pid,
                                       proc->node->name, WTERMSIG(proc->exit_code));
#ifdef HAVE_STRSIGNAL
                    }
#endif
                }
            } else if (ORTE_JOB_STATE_ABORTED_WO_SYNC == job->state) { /* proc exited w/o finalize */
                if (NULL == proc) {
                    orte_show_help("help-orterun.txt", "orterun:proc-exit-no-sync-unknown", true,
                                   orterun_basename, orterun_basename);
                } else {
                    orte_show_help("help-orterun.txt", "orterun:proc-exit-no-sync", true,
                                   orterun_basename, (unsigned long)proc->name.vpid, (unsigned long)proc->pid,
                                   proc->node->name, orterun_basename);
                }
            }
            return;
        }
    }
    
    /* if we got here, then we couldn't find the job that aborted -
     * report that fact and give up
     */
    orte_show_help("help-orterun.txt", "orterun:proc-aborted-unknown", true, orterun_basename);
}

static void abort_exit_callback(int fd, short ign, void *arg)
{
    int ret;

    if (!orterun_globals.quiet){
        fprintf(stderr, "%s: killing job...\n\n", orterun_basename);
    }
    
    /* since we are being terminated by a user's signal, be
     * sure to exit with a non-zero exit code - but don't
     * overwrite any error code from a proc that might have
     * failed, in case that is why the user ordered us
     * to terminate
     */
    ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);

    /* terminate the job - this will also wakeup orterun so
     * it can report to the user and kill all the orteds.
     * Check the jobid, though, just in case the user
     * hit ctrl-c before we had a chance to setup the
     * job in the system - in which case there is nothing
     * to terminate!
     */
    if (NULL != jdata &&
        jdata->jobid != ORTE_JOBID_INVALID &&
        !orte_never_launched) {
        /* terminate the orteds - they will automatically kill
         * their local procs
         */
        ret = orte_plm.terminate_orteds();
        if (ORTE_SUCCESS != ret) {
            /* If we failed the terminate_orteds() above, then we
             * need to just die
             */
            just_quit(fd, ign, arg);
        }
        /* give ourselves a time limit on how long to wait
         * for the job to die, just in case we can't make it go
         * away for some reason. Don't send us directly back
         * to job_completed, though, as that function may be
         * what has failed
         */
        ORTE_DETECT_TIMEOUT(&abort_exit_event, jdata->num_procs,
                            orte_timeout_usec_per_proc,
                            orte_max_timeout, 
                            just_quit);
        
    } else {
        /* if the jobid is invalid or we never launched,
         * there is nothing to do but just clean ourselves
         * up and exit
         */
        just_quit(fd, ign, arg);
    }
}

/*
 * Attempt to terminate the job and wait for callback indicating
 * the job has been aborted.
 */
static void abort_signal_callback(int fd, short flags, void *arg)
{
    /* if we have already ordered this once, don't keep
     * doing it to avoid race conditions
     */
    if (!opal_atomic_trylock(&orte_abort_inprogress_lock)) { /* returns 1 if already locked */
        if (forcibly_die) {
            /* kill any local procs */
            orte_odls.kill_local_procs(NULL, false);
            
            /* whack any lingering session directory files from our jobs */
            orte_session_dir_cleanup(ORTE_JOBID_WILDCARD);
            
            /* cleanup our data server */
            orte_data_server_finalize();
            
            /* exit with a non-zero status */
            exit(ORTE_ERROR_DEFAULT_EXIT_CODE);
        }
        fprintf(stderr, "%s: abort is already in progress...hit ctrl-c again to forcibly terminate\n\n", orterun_basename);
        forcibly_die = true;
        return;
    }

    /* set the global abnormal exit flag so we know not to
     * use the standard xcast for terminating orteds
     */
    orte_abnormal_term_ordered = true;
    /* ensure that the forwarding of stdin stops */
    orte_job_term_ordered = true;

    /* We are in an event handler; the job completed procedure
       will delete the signal handler that is currently running
       (which is a Bad Thing), so we can't call it directly.
       Instead, we have to exit this handler and setup to call
       job_completed() after this. */
    ORTE_TIMER_EVENT(0, 0, abort_exit_callback);
}

/**
 * Deal with sigpipe errors
 */
static int sigpipe_error_count=0;
static void epipe_signal_callback(int fd, short flags, void *arg)
{
    sigpipe_error_count++;

    if (10 < sigpipe_error_count) {
        /* time to abort */
        opal_output(0, "%s: SIGPIPE detected on fd %d - aborting", orterun_basename, fd);
        abort_exit_callback(0, 0, 0);
    }
    return;
}

/**
 * Pass user signals to the remote application processes
 */
static void  signal_forward_callback(int fd, short event, void *arg)
{
    struct opal_event *signal = (struct opal_event*)arg;
    int signum, ret;

    signum = OPAL_EVENT_SIGNAL(signal);
    if (!orterun_globals.quiet){
        fprintf(stderr, "%s: Forwarding signal %d to job\n",
                orterun_basename, signum);
    }

    /** send the signal out to the processes, including any descendants */
    if (ORTE_SUCCESS != (ret = orte_plm.signal_job(jdata->jobid, signum))) {
        fprintf(stderr, "Signal %d could not be sent to the job (returned %d)",
                signum, ret);
    }
}


static int init_globals(void)
{
    /* Only CONSTRUCT things once */
    if (!globals_init) {
        OBJ_CONSTRUCT(&orterun_globals.lock, opal_mutex_t);
        orterun_globals.env_val =     NULL;
        orterun_globals.appfile =     NULL;
        orterun_globals.wdir =        NULL;
        orterun_globals.path =        NULL;
        orterun_globals.ompi_server = NULL;
        orterun_globals.wait_for_server = false;
        orterun_globals.server_wait_timeout = 10;
        orterun_globals.stdin_target = "0";
        orterun_globals.report_pid        = NULL;
        orterun_globals.report_uri        = NULL;
    }

    /* Reset the other fields every time */

    orterun_globals.help                       = false;
    orterun_globals.version                    = false;
    orterun_globals.verbose                    = false;
    orterun_globals.quiet                      = false;
    orterun_globals.by_node                    = false;
    orterun_globals.by_slot                    = false;
    orterun_globals.by_board                   = false;
    orterun_globals.by_socket                  = false;
    orterun_globals.bind_to_core               = false;
    orterun_globals.bind_to_board              = false;
    orterun_globals.bind_to_socket             = false;
    orterun_globals.debugger                   = false;
    orterun_globals.num_procs                  =  0;
    if( NULL != orterun_globals.env_val )
        free( orterun_globals.env_val );
    orterun_globals.env_val =     NULL;
    if( NULL != orterun_globals.appfile )
        free( orterun_globals.appfile );
    orterun_globals.appfile =     NULL;
    if( NULL != orterun_globals.wdir )
        free( orterun_globals.wdir );
    orterun_globals.wdir =        NULL;
    if( NULL != orterun_globals.path )
        free( orterun_globals.path );
    orterun_globals.path =        NULL;

    orterun_globals.preload_binary = false;
    orterun_globals.preload_files  = NULL;
    orterun_globals.preload_files_dest_dir = NULL;

    /* All done */
    globals_init = true;
    return ORTE_SUCCESS;
}


static int parse_globals(int argc, char* argv[], opal_cmd_line_t *cmd_line)
{
    /* print version if requested.  Do this before check for help so
       that --version --help works as one might expect. */
    if (orterun_globals.version && 
        !(1 == argc || orterun_globals.help)) {
        char *project_name = NULL;
        if (0 == strcmp(orterun_basename, "mpirun")) {
            project_name = "Open MPI";
        } else {
            project_name = "OpenRTE";
        }
        orte_show_help("help-orterun.txt", "orterun:version", false,
                       orterun_basename, project_name, OPAL_VERSION,
                       PACKAGE_BUGREPORT);
        /* if we were the only argument, exit */
        if (2 == argc) exit(0);
    }

    /* Check for help request */
    if (1 == argc || orterun_globals.help) {
        char *args = NULL;
        char *project_name = NULL;
        if (0 == strcmp(orterun_basename, "mpirun")) {
            project_name = "Open MPI";
        } else {
            project_name = "OpenRTE";
        }
        args = opal_cmd_line_get_usage_msg(cmd_line);
        orte_show_help("help-orterun.txt", "orterun:usage", false,
                       orterun_basename, project_name, OPAL_VERSION,
                       orterun_basename, args,
                       PACKAGE_BUGREPORT);
        free(args);

        /* If someone asks for help, that should be all we do */
        exit(0);
    }

    /* check for request to report pid */
    if (NULL != orterun_globals.report_pid) {
        FILE *fp;
        if (0 == strcmp(orterun_globals.report_pid, "-")) {
            /* if '-', then output to stdout */
            printf("%d\n", (int)getpid());
        } else if (0 == strcmp(orterun_globals.report_pid, "+")) {
            /* if '+', output to stderr */
            fprintf(stderr, "%d\n", (int)getpid());
        } else {
            fp = fopen(orterun_globals.report_pid, "w");
            if (NULL == fp) {
                orte_show_help("help-orterun.txt", "orterun:write_file", false,
                               orterun_basename, "pid", orterun_globals.report_pid);
                exit(0);
            }
            fprintf(fp, "%d\n", (int)getpid());
            fclose(fp);
        }
    }
    
    /* Do we want a user-level debugger? */

    if (orterun_globals.debugger) {
        run_debugger(orterun_basename, cmd_line, argc, argv, orterun_globals.num_procs);
    }

    /* extract any rank assignment policy directives */
    if (orterun_globals.by_node) {
        ORTE_SET_MAPPING_POLICY(ORTE_MAPPING_BYNODE);
    } else if (orterun_globals.by_board) {
        ORTE_SET_MAPPING_POLICY(ORTE_MAPPING_BYBOARD);
    } else if (orterun_globals.by_socket) {
        ORTE_SET_MAPPING_POLICY(ORTE_MAPPING_BYSOCKET);
    } else if (orterun_globals.by_slot) {
        ORTE_SET_MAPPING_POLICY(ORTE_MAPPING_BYSLOT);
    }
    /* if nothing was specified, leave it as set by
     * mca param
     */
    
    /* extract any binding policy directives */
    if (orterun_globals.bind_to_socket) {
        ORTE_SET_BINDING_POLICY(ORTE_BIND_TO_SOCKET);
    } else if (orterun_globals.bind_to_board) {
        ORTE_SET_BINDING_POLICY(ORTE_BIND_TO_BOARD);
    } else if (orterun_globals.bind_to_core) {
        ORTE_SET_BINDING_POLICY(ORTE_BIND_TO_CORE);
    } else if (orterun_globals.bind_to_none) {
        ORTE_SET_BINDING_POLICY(ORTE_BIND_TO_NONE);
    }
    /* if nothing was specified, leave it as set
     * by mca param
     */
    
    return ORTE_SUCCESS;
}


static int parse_locals(int argc, char* argv[])
{
    int i, rc, app_num;
    int temp_argc;
    char **temp_argv, **env;
    orte_app_context_t *app;
    bool made_app;
    orte_std_cntr_t j, size1;

    /* if the ompi-server was given, then set it up here */
    if (NULL != orterun_globals.ompi_server) {
        /* someone could have passed us a file instead of a uri, so
         * we need to first check to see what we have - if it starts
         * with "file", then we know it is a file. Otherwise, we assume
         * it is a uri as provided by the ompi-server's output
         * of an ORTE-standard string. Note that this is NOT a standard
         * uri as it starts with the process name!
         */
        if (0 == strncmp(orterun_globals.ompi_server, "file", strlen("file")) ||
            0 == strncmp(orterun_globals.ompi_server, "FILE", strlen("FILE"))) {
            char input[1024], *filename;
            FILE *fp;
            
            /* it is a file - get the filename */
            filename = strchr(orterun_globals.ompi_server, ':');
            if (NULL == filename) {
                /* filename is not correctly formatted */
                orte_show_help("help-orterun.txt", "orterun:ompi-server-filename-bad", true,
                               orterun_basename, orterun_globals.ompi_server);
                exit(1);
            }
            ++filename; /* space past the : */
            
            if (0 >= strlen(filename)) {
                /* they forgot to give us the name! */
                orte_show_help("help-orterun.txt", "orterun:ompi-server-filename-missing", true,
                               orterun_basename, orterun_globals.ompi_server);
                exit(1);
            }
            
            /* open the file and extract the uri */
            fp = fopen(filename, "r");
            if (NULL == fp) { /* can't find or read file! */
                orte_show_help("help-orterun.txt", "orterun:ompi-server-filename-access", true,
                               orterun_basename, orterun_globals.ompi_server);
                exit(1);
            }
            if (NULL == fgets(input, 1024, fp)) {
                /* something malformed about file */
                fclose(fp);
                orte_show_help("help-orterun.txt", "orterun:ompi-server-file-bad", true,
                               orterun_basename, orterun_globals.ompi_server,
                               orterun_basename);
                exit(1);
            }
            fclose(fp);
            input[strlen(input)-1] = '\0';  /* remove newline */
            ompi_server = strdup(input);
        } else if (0 == strncmp(orterun_globals.ompi_server, "pid", strlen("pid")) ||
                   0 == strncmp(orterun_globals.ompi_server, "PID", strlen("PID"))) {
            opal_list_t hnp_list;
            opal_list_item_t *item;
            orte_hnp_contact_t *hnp;
            char *ptr;
            pid_t pid;
            
            ptr = strchr(orterun_globals.ompi_server, ':');
            if (NULL == ptr) {
                /* pid is not correctly formatted */
                orte_show_help("help-orterun.txt", "orterun:ompi-server-pid-bad", true,
                               orterun_basename, orterun_basename,
                               orterun_globals.ompi_server, orterun_basename);
                exit(1);
            }
            ++ptr; /* space past the : */
            
            if (0 >= strlen(ptr)) {
                /* they forgot to give us the pid! */
                orte_show_help("help-orterun.txt", "orterun:ompi-server-pid-bad", true,
                               orterun_basename, orterun_basename,
                               orterun_globals.ompi_server, orterun_basename);
                exit(1);
            }
            
            pid = strtoul(ptr, NULL, 10);
            
            /* to search the local mpirun's, we have to partially initialize the
             * orte_process_info structure. This won't fully be setup until orte_init,
             * but we finagle a little bit of it here
             */
            if (ORTE_SUCCESS != (rc = orte_session_dir_get_name(NULL, &orte_process_info.tmpdir_base,
                                                                &orte_process_info.top_session_dir,
                                                                NULL, NULL, NULL))) {
                orte_show_help("help-orterun.txt", "orterun:ompi-server-could-not-get-hnp-list", true,
                               orterun_basename, orterun_basename);
                exit(1);
            }
            
            OBJ_CONSTRUCT(&hnp_list, opal_list_t);
            
            /* get the list of HNPs, but do -not- setup contact info to them in the RML */
            if (ORTE_SUCCESS != (rc = orte_list_local_hnps(&hnp_list, false))) {
                orte_show_help("help-orterun.txt", "orterun:ompi-server-could-not-get-hnp-list", true,
                               orterun_basename, orterun_basename);
                exit(1);
            }
            
            /* search the list for the desired pid */
            while (NULL != (item = opal_list_remove_first(&hnp_list))) {
                hnp = (orte_hnp_contact_t*)item;
                if (pid == hnp->pid) {
                    ompi_server = strdup(hnp->rml_uri);
                    goto hnp_found;
                }
                OBJ_RELEASE(item);
            }
            /* if we got here, it wasn't found */
            orte_show_help("help-orterun.txt", "orterun:ompi-server-pid-not-found", true,
                           orterun_basename, orterun_basename, pid, orterun_globals.ompi_server,
                           orterun_basename);
            OBJ_DESTRUCT(&hnp_list);
            exit(1);
        hnp_found:
            /* cleanup rest of list */
            while (NULL != (item = opal_list_remove_first(&hnp_list))) {
                OBJ_RELEASE(item);
            }
            OBJ_DESTRUCT(&hnp_list);
        } else {
            ompi_server = strdup(orterun_globals.ompi_server);
        }
    }

    /* Make the apps */

    temp_argc = 0;
    temp_argv = NULL;
    opal_argv_append(&temp_argc, &temp_argv, argv[0]);

    /* NOTE: This bogus env variable is necessary in the calls to
       create_app(), below.  See comment immediately before the
       create_app() function for an explanation. */

    env = NULL;
    for (app_num = 0, i = 1; i < argc; ++i) {
        if (0 == strcmp(argv[i], ":")) {
            /* Make an app with this argv */
            if (opal_argv_count(temp_argv) > 1) {
                if (NULL != env) {
                    opal_argv_free(env);
                    env = NULL;
                }
                app = NULL;
                rc = create_app(temp_argc, temp_argv, &app, &made_app, &env);
                /** keep track of the number of apps - point this app_context to that index */
                if (ORTE_SUCCESS != rc) {
                    /* Assume that the error message has already been
                       printed; no need to cleanup -- we can just
                       exit */
                    exit(1);
                }
                if (made_app) {
                    app->idx = app_num;
                    ++app_num;
                    opal_pointer_array_add(jdata->apps, app);
                    ++jdata->num_apps;
                }

                /* Reset the temps */

                temp_argc = 0;
                temp_argv = NULL;
                opal_argv_append(&temp_argc, &temp_argv, argv[0]);
            }
        } else {
            opal_argv_append(&temp_argc, &temp_argv, argv[i]);
        }
    }

    if (opal_argv_count(temp_argv) > 1) {
        app = NULL;
        rc = create_app(temp_argc, temp_argv, &app, &made_app, &env);
        if (ORTE_SUCCESS != rc) {
            /* Assume that the error message has already been printed;
               no need to cleanup -- we can just exit */
            exit(1);
        }
        if (made_app) {
            app->idx = app_num;
            ++app_num;
            opal_pointer_array_add(jdata->apps, app);
            ++jdata->num_apps;
        }
    }
    if (NULL != env) {
        opal_argv_free(env);
    }
    opal_argv_free(temp_argv);

    /* Once we've created all the apps, add the global MCA params to
       each app's environment (checking for duplicates, of
       course -- yay opal_environ_merge()).  */

    if (NULL != global_mca_env) {
        size1 = (size_t)opal_pointer_array_get_size(jdata->apps);
        /* Iterate through all the apps */
        for (j = 0; j < size1; ++j) {
            app = (orte_app_context_t *)
                opal_pointer_array_get_item(jdata->apps, j);
            if (NULL != app) {
                /* Use handy utility function */
                env = opal_environ_merge(global_mca_env, app->env);
                opal_argv_free(app->env);
                app->env = env;
            }
        }
    }

    /* Now take a subset of the MCA params and set them as MCA
       overrides here in orterun (so that when we orte_init() later,
       all the components see these MCA params).  Here's how we decide
       which subset of the MCA params we set here in orterun:

       1. If any global MCA params were set, use those
       2. If no global MCA params were set and there was only one app,
          then use its app MCA params
       3. Otherwise, don't set any
    */

    env = NULL;
    if (NULL != global_mca_env) {
        env = global_mca_env;
    } else {
        if (opal_pointer_array_get_size(jdata->apps) >= 1) {
            /* Remember that pointer_array's can be padded with NULL
               entries; so only use the app's env if there is exactly
               1 non-NULL entry */
            app = (orte_app_context_t *)
                opal_pointer_array_get_item(jdata->apps, 0);
            if (NULL != app) {
                env = app->env;
                for (j = 1; j < opal_pointer_array_get_size(jdata->apps); ++j) {
                    if (NULL != opal_pointer_array_get_item(jdata->apps, j)) {
                        env = NULL;
                        break;
                    }
                }
            }
        }
    }

    if (NULL != env) {
        size1 = opal_argv_count(env);
        for (j = 0; j < size1; ++j) {
            /* Use-after-Free error possible here.  putenv does not copy
             * the string passed to it, and instead stores only the pointer.
             * env[j] may be freed later, in which case the pointer
             * in environ will now be left dangling into a deallocated
             * region.
             * So we make a copy of the variable.
             */
            char *s = strdup(env[j]);
            
            if (NULL == s) {
                return OPAL_ERR_OUT_OF_RESOURCE;
            }
            putenv(s);
        }
    }

    /* All done */

    return ORTE_SUCCESS;
}


static int capture_cmd_line_params(int argc, int start, char **argv)
{
    int i, j, k;
    bool ignore;
    char *no_dups[] = {
        "grpcomm",
        "odls",
        "rml",
        "routed",
        NULL
    };
    
    for (i = 0; i < (argc-start); ++i) {
        if (0 == strcmp("-mca",  argv[i]) ||
            0 == strcmp("--mca", argv[i]) ) {
            /* It would be nice to avoid increasing the length
             * of the orted cmd line by removing any non-ORTE
             * params. However, this raises a problem since
             * there could be OPAL directives that we really
             * -do- want the orted to see - it's only the OMPI
             * related directives we could ignore. This becomes
             * a very complicated procedure, however, since
             * the OMPI mca params are not cleanly separated - so
             * filtering them out is nearly impossible.
             *
             * see if this is already present so we at least can
             * avoid growing the cmd line with duplicates
             */
            ignore = false;
            if (NULL != orted_cmd_line) {
                for (j=0; NULL != orted_cmd_line[j]; j++) {
                    if (0 == strcmp(argv[i+1], orted_cmd_line[j])) {
                        /* already here - if the value is the same,
                         * we can quitely ignore the fact that they
                         * provide it more than once. However, some
                         * frameworks are known to have problems if the
                         * value is different. We don't have a good way
                         * to know this, but we at least make a crude
                         * attempt here to protect ourselves.
                         */
                        if (0 == strcmp(argv[i+2], orted_cmd_line[j+1])) {
                            /* values are the same */
                            ignore = true;
                            break;
                        } else {
                            /* values are different - see if this is a problem */
                            for (k=0; NULL != no_dups[k]; k++) {
                                if (0 == strcmp(no_dups[k], argv[i+1])) {
                                    /* print help message
                                     * and abort as we cannot know which one is correct
                                     */
                                    orte_show_help("help-orterun.txt", "orterun:conflicting-params",
                                                   true, orterun_basename, argv[i+1],
                                                   argv[i+2], orted_cmd_line[j+1]);
                                    return ORTE_ERR_BAD_PARAM;
                                }
                            }
                            /* this passed muster - just ignore it */
                            ignore = true;
                            break;
                        }
                    }
                }
            }
            if (!ignore) {
                opal_argv_append_nosize(&orted_cmd_line, argv[i]);
                opal_argv_append_nosize(&orted_cmd_line, argv[i+1]);
                opal_argv_append_nosize(&orted_cmd_line, argv[i+2]);
            }
            i += 2;
        }
    }
    
    return ORTE_SUCCESS;
}


/*
 * This function takes a "char ***app_env" parameter to handle the
 * specific case:
 *
 *   orterun --mca foo bar -app appfile
 *
 * That is, we'll need to keep foo=bar, but the presence of the app
 * file will cause an invocation of parse_appfile(), which will cause
 * one or more recursive calls back to create_app().  Since the
 * foo=bar value applies globally to all apps in the appfile, we need
 * to pass in the "base" environment (that contains the foo=bar value)
 * when we parse each line in the appfile.
 *
 * This is really just a special case -- when we have a simple case like:
 *
 *   orterun --mca foo bar -np 4 hostname
 *
 * Then the upper-level function (parse_locals()) calls create_app()
 * with a NULL value for app_env, meaning that there is no "base"
 * environment that the app needs to be created from.
 */
static int create_app(int argc, char* argv[], orte_app_context_t **app_ptr,
                      bool *made_app, char ***app_env)
{
    opal_cmd_line_t cmd_line;
    char cwd[OPAL_PATH_MAX];
    int i, j, count, rc;
    char *param, *value, *value2;
    orte_app_context_t *app = NULL;
    bool cmd_line_made = false;

    *made_app = false;

    /* Pre-process the command line if we are going to parse an appfile later.
     * save any mca command line args so they can be passed
     * separately to the daemons.
     * Use Case:
     *  $ cat launch.appfile
     *  -np 1 -mca aaa bbb ./my-app -mca ccc ddd
     *  -np 1 -mca aaa bbb ./my-app -mca eee fff
     *  $ mpirun -np 2 -mca foo bar --app launch.appfile
     * Only pick up '-mca foo bar' on this pass.
     */
    if (NULL != orterun_globals.appfile) {
        if (ORTE_SUCCESS != (rc = capture_cmd_line_params(argc, 0, argv))) {
            goto cleanup;
        }
    }
    
    /* Parse application command line options. */

    init_globals();
    opal_cmd_line_create(&cmd_line, cmd_line_init);
    mca_base_cmd_line_setup(&cmd_line);
    cmd_line_made = true;
    rc = opal_cmd_line_parse(&cmd_line, true, argc, argv);
    if (ORTE_SUCCESS != rc) {
        goto cleanup;
    }
    mca_base_cmd_line_process_args(&cmd_line, app_env, &global_mca_env);

    /* Is there an appfile in here? */

    if (NULL != orterun_globals.appfile) {
        OBJ_DESTRUCT(&cmd_line);
        return parse_appfile(strdup(orterun_globals.appfile), app_env);
    }

    /* Setup application context */

    app = OBJ_NEW(orte_app_context_t);
    opal_cmd_line_get_tail(&cmd_line, &count, &app->argv);

    /* See if we have anything left */

    if (0 == count) {
        orte_show_help("help-orterun.txt", "orterun:executable-not-specified",
                       true, orterun_basename, orterun_basename);
        rc = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }

    /*
     * Get mca parameters so we can pass them to the daemons.
     * Use the count determined above to make sure we do not go past
     * the executable name. Example:
     *   mpirun -np 2 -mca foo bar ./my-app -mca bip bop
     * We want to pick up '-mca foo bar' but not '-mca bip bop'
     */
    if (ORTE_SUCCESS != (rc = capture_cmd_line_params(argc, count, argv))) {
        goto cleanup;
    }
    
    /* Grab all OMPI_* environment variables */

    app->env = opal_argv_copy(*app_env);
    for (i = 0; NULL != environ[i]; ++i) {
        if (0 == strncmp("OMPI_", environ[i], 5)) {
            /* check for duplicate in app->env - this
             * would have been placed there by the
             * cmd line processor. By convention, we
             * always let the cmd line override the
             * environment
             */
            param = strdup(environ[i]);
            value = strchr(param, '=');
            *value = '\0';
            value++;
            opal_setenv(param, value, false, &app->env);
            free(param);
        }
    }
    /* if profile was set, add it back in */
    if (profile_is_set) {
        opal_setenv("OMPI_MCA_opal_profile", "1", true, &app->env);
    }
    
    /* add the ompi-server, if provided */
    if (NULL != ompi_server) {
        opal_setenv("OMPI_MCA_pubsub_orte_server", ompi_server, true, &app->env);
    }

    /* Did the user request to export any environment variables? */

    if (opal_cmd_line_is_taken(&cmd_line, "x")) {
        j = opal_cmd_line_get_ninsts(&cmd_line, "x");
        for (i = 0; i < j; ++i) {
            param = opal_cmd_line_get_param(&cmd_line, "x", i, 0);

            if (NULL != strchr(param, '=')) {
                opal_argv_append_nosize(&app->env, param);
            } else {
                value = getenv(param);
                if (NULL != value) {
                    if (NULL != strchr(value, '=')) {
                        opal_argv_append_nosize(&app->env, value);
                    } else {
                        asprintf(&value2, "%s=%s", param, value);
                        opal_argv_append_nosize(&app->env, value2);
                        free(value2);
                    }
                } else {
                    opal_output(0, "Warning: could not find environment variable \"%s\"\n", param);
                }
            }
        }
    }

    /* If the user specified --path, store it in the user's app
       environment via the OMPI_exec_path variable. */
    if (NULL != orterun_globals.path) {
        asprintf(&value, "OMPI_exec_path=%s", orterun_globals.path);
        opal_argv_append_nosize(&app->env, value);
        free(value);
    }

    /* Did the user request a specific wdir? */

    if (NULL != orterun_globals.wdir) {
        /* if this is a relative path, convert it to an absolute path */
        if (opal_path_is_absolute(orterun_globals.wdir)) {
            app->cwd = strdup(orterun_globals.wdir);
        } else {
            /* get the cwd */
            if (OPAL_SUCCESS != (rc = opal_getcwd(cwd, sizeof(cwd)))) {
                orte_show_help("help-orterun.txt", "orterun:init-failure",
                               true, "get the cwd", rc);
                goto cleanup;
            }
            /* construct the absolute path */
            app->cwd = opal_os_path(false, cwd, orterun_globals.wdir, NULL);
        }
        app->user_specified_cwd = true;
    } else {
        if (OPAL_SUCCESS != (rc = opal_getcwd(cwd, sizeof(cwd)))) {
            orte_show_help("help-orterun.txt", "orterun:init-failure",
                           true, "get the cwd", rc);
            goto cleanup;
        }
        app->cwd = strdup(cwd);
        app->user_specified_cwd = false;
    }

    /* if this is the first app_context, check for prefix directions.
     * We only do this for the first app_context because the launchers
     * only look at the first one when setting the prefix - we do NOT
     * support per-app_context prefix settings!
     */
    if (0 == total_num_apps) {
        /* Check to see if the user explicitly wanted to disable automatic
           --prefix behavior */
        
        if (opal_cmd_line_is_taken(&cmd_line, "noprefix")) {
            want_prefix_by_default = false;
        }

        /* Did the user specify a prefix, or want prefix by default? */
        if (opal_cmd_line_is_taken(&cmd_line, "prefix") || want_prefix_by_default) {
            size_t param_len;
            /* if both the prefix was given and we have a prefix
             * given above, check to see if they match
             */
            if (opal_cmd_line_is_taken(&cmd_line, "prefix") &&
                NULL != orterun_globals.prefix) {
                /* if they don't match, then that merits a warning */
                param = strdup(opal_cmd_line_get_param(&cmd_line, "prefix", 0, 0));
                if (0 != strcmp(param, orterun_globals.prefix)) {
                    orte_show_help("help-orterun.txt", "orterun:app-prefix-conflict",
                                   true, orterun_basename, orterun_globals.prefix, param);
                    /* let the global-level prefix take precedence since we
                     * know that one is being  used
                     */
                    free(param);
                    param = orterun_globals.prefix;
                } else {
                    /* since they match, just use param */
                    free(orterun_globals.prefix);
                    orterun_globals.prefix = NULL;
                }
            } else if (NULL != orterun_globals.prefix) {
                param = orterun_globals.prefix;
            } else if (opal_cmd_line_is_taken(&cmd_line, "prefix")){
                /* must be --prefix alone */
                param = strdup(opal_cmd_line_get_param(&cmd_line, "prefix", 0, 0));
            } else {
                /* --enable-orterun-prefix-default was given to orterun */
                param = strdup(opal_install_dirs.prefix);
            }
            if (NULL != param) {
                /* "Parse" the param, aka remove superfluous path_sep. */
                param_len = strlen(param);
                while (0 == strcmp (OPAL_PATH_SEP, &(param[param_len-1]))) {
                    param[param_len-1] = '\0';
                    param_len--;
                    if (0 == param_len) {
                        orte_show_help("help-orterun.txt", "orterun:empty-prefix",
                                       true, orterun_basename, orterun_basename);
                        return ORTE_ERR_FATAL;
                    }
                }
                app->prefix_dir = strdup(param);
                free(param);
            }
        }
    }

    /* Did the user specify a hostfile. Need to check for both 
     * hostfile and machine file. 
     * We can only deal with one hostfile per app context, otherwise give an error.
     */
    if (0 < (j = opal_cmd_line_get_ninsts(&cmd_line, "hostfile"))) {
        if(1 < j) {
            orte_show_help("help-orterun.txt", "orterun:multiple-hostfiles",
                           true, orterun_basename, NULL);
            return ORTE_ERR_FATAL;
        } else {
            value = opal_cmd_line_get_param(&cmd_line, "hostfile", 0, 0);
            app->hostfile = strdup(value);
        }
    }
    if (0 < (j = opal_cmd_line_get_ninsts(&cmd_line, "machinefile"))) {
        if(1 < j || NULL != app->hostfile) {
            orte_show_help("help-orterun.txt", "orterun:multiple-hostfiles",
                           true, orterun_basename, NULL);
            return ORTE_ERR_FATAL;
        } else {
            value = opal_cmd_line_get_param(&cmd_line, "machinefile", 0, 0);
            app->hostfile = strdup(value);
        }
    }
 
    /* Did the user specify any hosts? */
    if (0 < (j = opal_cmd_line_get_ninsts(&cmd_line, "host"))) {
        for (i = 0; i < j; ++i) {
            value = opal_cmd_line_get_param(&cmd_line, "host", i, 0);
            opal_argv_append_nosize(&app->dash_host, value);
        }
    }

    /* Get the numprocs */

    app->num_procs = (orte_std_cntr_t)orterun_globals.num_procs;

    /* If the user didn't specify the number of processes to run, then we
       default to launching an app process using every slot. We can't do
       anything about that here - we leave it to the RMAPS framework's
       components to note this and deal with it later.
        
       HOWEVER, we ONLY support this mode of operation if the number of
       app_contexts is equal to ONE. If the user provides multiple applications,
       we simply must have more information - in this case, generate an
       error.
    */
    if (app->num_procs == 0) {
        have_zero_np = true;  /** flag that we have a zero_np situation */
    }

    if (0 < total_num_apps && have_zero_np) {
        /** we have more than one app and a zero_np - that's no good.
         * note that we have to do this as a two step logic check since
         * the user may fail to specify num_procs for the first app, but
         * then give us another application.
         */
        orte_show_help("help-orterun.txt", "orterun:multi-apps-and-zero-np",
                       true, orterun_basename, NULL);
        return ORTE_ERR_FATAL;
    }
    
    total_num_apps++;

    /* Preserve if we are to preload the binary */
    app->preload_binary = orterun_globals.preload_binary;
    if( NULL != orterun_globals.preload_files)
        app->preload_files  = strdup(orterun_globals.preload_files);
    else 
        app->preload_files = NULL;
    if( NULL != orterun_globals.preload_files_dest_dir)
        app->preload_files_dest_dir  = strdup(orterun_globals.preload_files_dest_dir);
    else 
        app->preload_files_dest_dir = NULL;


    /* Do not try to find argv[0] here -- the starter is responsible
       for that because it may not be relevant to try to find it on
       the node where orterun is executing.  So just strdup() argv[0]
       into app. */

    app->app = strdup(app->argv[0]);
    if (NULL == app->app) {
        orte_show_help("help-orterun.txt", "orterun:call-failed",
                       true, orterun_basename, "library", "strdup returned NULL", errno);
        rc = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }

    *app_ptr = app;
    app = NULL;
    *made_app = true;

    /* All done */

 cleanup:
    if (NULL != app) {
        OBJ_RELEASE(app);
    }
    if (cmd_line_made) {
        OBJ_DESTRUCT(&cmd_line);
    }
    return rc;
}


static int parse_appfile(char *filename, char ***env)
{
    size_t i, len;
    FILE *fp;
    char line[BUFSIZ];
    int rc, argc, app_num;
    char **argv;
    orte_app_context_t *app;
    bool blank, made_app;
    char bogus[] = "bogus ";
    char **tmp_env;

    /*
     * Make sure to clear out this variable so we don't do anything odd in
     * app_create()
     */
    if( NULL != orterun_globals.appfile ) {
        free( orterun_globals.appfile );
        orterun_globals.appfile =     NULL;
    }

    /* Try to open the file */

    fp = fopen(filename, "r");
    if (NULL == fp) {
        orte_show_help("help-orterun.txt", "orterun:appfile-not-found", true,
                       filename);
        return ORTE_ERR_NOT_FOUND;
    }

    /* Read in line by line */

    line[sizeof(line) - 1] = '\0';
    app_num = 0;
    do {

        /* We need a bogus argv[0] (because when argv comes in from
           the command line, argv[0] is "orterun", so the parsing
           logic ignores it).  So create one here rather than making
           an argv and then pre-pending a new argv[0] (which would be
           rather inefficient). */

        line[0] = '\0';
        strcat(line, bogus);

        if (NULL == fgets(line + sizeof(bogus) - 1,
                          sizeof(line) - sizeof(bogus) - 1, fp)) {
            break;
        }

        /* Remove a trailing newline */

        len = strlen(line);
        if (len > 0 && '\n' == line[len - 1]) {
            line[len - 1] = '\0';
            if (len > 0) {
                --len;
            }
        }

        /* Remove comments */

        for (i = 0; i < len; ++i) {
            if ('#' == line[i]) {
                line[i] = '\0';
                break;
            } else if (i + 1 < len && '/' == line[i] && '/' == line[i + 1]) {
                line[i] = '\0';
                break;
            }
        }

        /* Is this a blank line? */

        len = strlen(line);
        for (blank = true, i = sizeof(bogus); i < len; ++i) {
            if (!isspace(line[i])) {
                blank = false;
                break;
            }
        }
        if (blank) {
            continue;
        }

        /* We got a line with *something* on it.  So process it */

        argv = opal_argv_split(line, ' ');
        argc = opal_argv_count(argv);
        if (argc > 0) {

            /* Create a temporary env to use in the recursive call --
               that is: don't disturb the original env so that we can
               have a consistent global env.  This allows for the
               case:

                   orterun --mca foo bar --appfile file

               where the "file" contains multiple apps.  In this case,
               each app in "file" will get *only* foo=bar as the base
               environment from which its specific environment is
               constructed. */

            if (NULL != *env) {
                tmp_env = opal_argv_copy(*env);
                if (NULL == tmp_env) {
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }
            } else {
                tmp_env = NULL;
            }

            rc = create_app(argc, argv, &app, &made_app, &tmp_env);
            if (ORTE_SUCCESS != rc) {
                /* Assume that the error message has already been
                   printed; no need to cleanup -- we can just exit */
                exit(1);
            }
            if (NULL != tmp_env) {
                opal_argv_free(tmp_env);
            }
            if (made_app) {
                app->idx = app_num;
                ++app_num;
                opal_pointer_array_add(jdata->apps, app);
                ++jdata->num_apps;
            }
        }
    } while (!feof(fp));
    fclose(fp);

    /* All done */

    free(filename);
    return ORTE_SUCCESS;
}
/*
 * Process one line from the orte_base_user_debugger MCA param and
 * look for that debugger in the path.  If we find it, fill in
 * new_argv.
 */
static int process(char *orig_line, char *basename, opal_cmd_line_t *cmd_line,
                   int argc, char **argv, char ***new_argv, int num_procs) 
{
    int i;
    char *line, *full_line = strdup(orig_line);
    char *user_argv, *tmp, *tmp2, **tmp_argv, **executable;
    char cwd[OPAL_PATH_MAX];
    bool used_num_procs = false;
    bool single_app = false;
    bool fail_needed_executable = false;

    line = full_line;
    if (NULL == line) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* Trim off whitespace at the beginning and ending of line */

    for (i = 0; '\0' != line[i] && isspace(line[i]); ++line) {
        continue;
    }
    for (i = strlen(line) - 2; i > 0 && isspace(line[i]); ++i) {
        line[i] = '\0';
    }
    if (strlen(line) <= 0) {
        return ORTE_ERROR;
    }

    /* Get the tail of the command line (i.e., the user executable /
       argv) */

    opal_cmd_line_get_tail(cmd_line, &i, &executable);

    /* Remove --debug, --debugger, and -tv from the user command line
       params */

    if (1 == argc) {
        user_argv = strdup("");
    } else {
        tmp_argv = opal_argv_copy(argv);
        for (i = 0; NULL != tmp_argv[i]; ++i) {
            if (0 == strcmp(tmp_argv[i], "-debug") ||
                0 == strcmp(tmp_argv[i], "--debug")) {
                free(tmp_argv[i]);
                tmp_argv[i] = strdup("");
            } else if (0 == strcmp(tmp_argv[i], "-tv") ||
		       0 == strcmp(tmp_argv[i], "--tv")) {
                free(tmp_argv[i]);
                tmp_argv[i] = strdup("");
            } else if (0 == strcmp(tmp_argv[i], "--debugger") ||
                       0 == strcmp(tmp_argv[i], "-debugger")) {
                free(tmp_argv[i]);
                tmp_argv[i] = strdup("");
                if (NULL != tmp_argv[i + 1]) {
                    ++i;
                    free(tmp_argv[i]);
                    tmp_argv[i] = strdup("");
                }
            }
        }
        user_argv = opal_argv_join(tmp_argv + 1, ' ');
        opal_argv_free(tmp_argv);
    }

    /* Replace @@ tokens - line should never realistically be bigger
       than MAX_INT, so just cast to int to remove compiler warning */

    for (i = 0; i < (int) strlen(line); ++i) {
        tmp = NULL;
        if (0 == strncmp(line + i, "@mpirun@", 8)) {
            line[i] = '\0';
            asprintf(&tmp, "%s%s%s", line, argv[0], line + i + 8);
        } else if (0 == strncmp(line + i, "@orterun@", 9)) {
            line[i] = '\0';
            asprintf(&tmp, "%s%s%s", line, argv[0], line + i + 9);
        } else if (0 == strncmp(line + i, "@mpirun_args@", 13)) {
            line[i] = '\0';
            asprintf(&tmp, "%s%s%s", line, user_argv, line + i + 13);
        } else if (0 == strncmp(line + i, "@orterun_args@", 14)) {
            line[i] = '\0';
            asprintf(&tmp, "%s%s%s", line, user_argv, line + i + 14);
        } else if (0 == strncmp(line + i, "@np@", 4)) {
            line[i] = '\0';
            asprintf(&tmp, "%s%d%s", line, num_procs,
                     line + i + 4);
            used_num_procs = true;
        } else if (0 == strncmp(line + i, "@single_app@", 12)) {
            line[i] = '\0';
            /* This token is only a flag; it is not replaced with any
               alternate text */
            asprintf(&tmp, "%s%s", line, line + i + 12);
            single_app = true;
        } else if (0 == strncmp(line + i, "@executable@", 12)) {
            line[i] = '\0';
            /* If we found the executable, paste it in.  Otherwise,
               this is a possible error. */
            if (NULL != executable) {
                asprintf(&tmp, "%s%s%s", line, executable[0], line + i + 12);
            } else {
                fail_needed_executable = true;
            }
        } else if (0 == strncmp(line + i, "@executable_argv@", 17)) {
            line[i] = '\0';
            /* If we found the tail, paste in the argv.  Otherwise,
               this is a possible error. */
            if (NULL != executable) {
                if (NULL != executable[1]) {
                    /* Put in the argv */
                    tmp2 = opal_argv_join(executable + 1, ' ');
                    asprintf(&tmp, "%s%s%s", line, tmp2, line + i + 17);
                    free(tmp2);
                } else {
                    /* There is no argv; just paste the front and back
                       together, removing the @token@ */
                    asprintf(&tmp, "%s%s", line, line + i + 17);
                }
            } else {
                fail_needed_executable = true;
            }
        }

        if (NULL != tmp) {
            free(full_line);
            full_line = line = tmp;
            --i;
        }
    }

    /* Split up into argv */

    *new_argv = opal_argv_split(line, ' ');
    free(full_line);

    /* Can we find argv[0] in the path? */

    getcwd(cwd, OPAL_PATH_MAX);
    tmp = opal_path_findv((*new_argv)[0], X_OK, environ, cwd);
    if (NULL != tmp) {
        free(tmp);

        /* Ok, we found a good debugger.  Check for some error
           conditions. */
        tmp = opal_argv_join(argv, ' ');

        /* We do not support launching a debugger that requires the
           -np value if the user did not specify -np on the command
           line. */
        if (used_num_procs && 0 == num_procs) {
            orte_show_help("help-orterun.txt", "debugger requires -np",
                           true, (*new_argv)[0], argv[0], user_argv, 
                           (*new_argv)[0]);
            /* Fall through to free / fail, below */
        } 

        /* Some debuggers do not support launching MPMD */
        else if (single_app && NULL != strchr(tmp, ':')) {
            orte_show_help("help-orterun.txt", 
                           "debugger only accepts single app", true,
                           (*new_argv)[0], (*new_argv)[0]);
            /* Fall through to free / fail, below */
        }

        /* Some debuggers do not use orterun/mpirun, and therefore
           must have an executable to run (e.g., cannot use mpirun's
           app context file feature). */
        else if (fail_needed_executable) {
            orte_show_help("help-orterun.txt", 
                           "debugger requires executable", true,
                           (*new_argv)[0], argv[0], (*new_argv)[0], argv[0],
                           (*new_argv)[0]);
            /* Fall through to free / fail, below */
        }

        /* Otherwise, we succeeded.  Return happiness. */
        else {
            free(tmp);
            return ORTE_SUCCESS;
        }
        free(tmp);
    }

    /* All done -- didn't find it */

    opal_argv_free(*new_argv);
    *new_argv = NULL;
    return ORTE_ERR_NOT_FOUND;
}

/**
 * Run a user-level debugger
 */
static void run_debugger(char *basename, opal_cmd_line_t *cmd_line,
                         int argc, char *argv[], int num_procs)
{
    int i, id;
    char **new_argv = NULL;
    char *value, **lines, *env_name;

    /* Get the orte_base_debug MCA parameter and search for a debugger
       that can run */
    
    id = mca_base_param_find("orte", NULL, "base_user_debugger");
    if (id < 0) {
        orte_show_help("help-orterun.txt", "debugger-mca-param-not-found", 
                       true);
        exit(1);
    }
    value = NULL;
    mca_base_param_lookup_string(id, &value);
    if (NULL == value) {
        orte_show_help("help-orterun.txt", "debugger-orte_base_user_debugger-empty",
                       true);
        exit(1);
    }

    /* Look through all the values in the MCA param */

    lines = opal_argv_split(value, ':');
    free(value);
    for (i = 0; NULL != lines[i]; ++i) {
        if (ORTE_SUCCESS == process(lines[i], basename, cmd_line, argc, argv, 
                                    &new_argv, num_procs)) {
            break;
        }
    }

    /* If we didn't find one, abort */

    if (NULL == lines[i]) {
        orte_show_help("help-orterun.txt", "debugger-not-found", true);
        exit(1);
    }
    opal_argv_free(lines);

    /* We found one */
    
    /* cleanup the MPIR arrays in case the debugger doesn't set them */
    memset((char*)MPIR_executable_path, 0, MPIR_MAX_PATH_LENGTH);
    memset((char*)MPIR_server_arguments, 0, MPIR_MAX_ARG_LENGTH);
    
    /* Set an MCA param so that everyone knows that they are being
       launched under a debugger; not all debuggers are consistent
       about setting MPIR_being_debugged in both the launcher and the
       MPI processes */
    env_name = mca_base_param_environ_variable("orte", 
                                               "in_parallel_debugger", NULL);
    if (NULL != env_name) {
        opal_setenv(env_name, "1", true, &environ);
        free(env_name);
    }

    /* Launch the debugger */
    execvp(new_argv[0], new_argv);
    value = opal_argv_join(new_argv, ' ');
    orte_show_help("help-orterun.txt", "debugger-exec-failed",
                   true, basename, value, new_argv[0]);
    free(value);
    opal_argv_free(new_argv);
    exit(1);
}

/****    DEBUGGER CODE ****/
/*
 * Debugger support for orterun
 *
 * We interpret the MPICH debugger interface as follows:
 *
 * a) The launcher
 *      - spawns the other processes,
 *      - fills in the table MPIR_proctable, and sets MPIR_proctable_size
 *      - sets MPIR_debug_state to MPIR_DEBUG_SPAWNED ( = 1)
 *      - calls MPIR_Breakpoint() which the debugger will have a
 *	  breakpoint on.
 *
 *  b) Applications start and then spin until MPIR_debug_gate is set
 *     non-zero by the debugger.
 *
 * This file implements (a).
 *
 **************************************************************************
 *
 * Note that we have presently tested both TotalView and DDT parallel
 * debuggers.  They both nominally subscribe to the Etnus attaching
 * interface, but there are differences between the two.
 *
 * TotalView: user launches "totalview mpirun -a ...<mpirun args>...".
 * TV launches mpirun.  mpirun launches the application and then calls
 * MPIR_Breakpoint().  This is the signal to TV that it's a parallel
 * MPI job.  TV then reads the proctable in mpirun and attaches itself
 * to all the processes (it takes care of launching itself on the
 * remote nodes).  Upon attaching to all the MPI processes, the
 * variable MPIR_being_debugged is set to 1.  When it has finished
 * attaching itself to all the MPI processes that it wants to,
 * MPIR_Breakpoint() returns.
 *
 * DDT: user launches "ddt bin -np X <mpi app name>".  DDT fork/exec's
 * mpirun to launch ddt-debugger on the back-end nodes via "mpirun -np
 * X ddt-debugger" (not the lack of other arguments -- we can't pass
 * anything to mpirun).  This app will eventually fork/exec the MPI
 * app.  DDT does not current set MPIR_being_debugged in the MPI app.
 *
 **************************************************************************
 *
 * We support two ways of waiting for attaching debuggers.  The
 * implementation spans this file and ompi/debuggers/ompi_debuggers.c.
 *
 * 1. If using orterun: MPI processes will have the
 * orte_in_parallel_debugger MCA param set to true (because not all
 * debuggers consistently set MPIR_being_debugged in both the launcher
 * and in the MPI procs).  The HNP will call MPIR_Breakpoint() and
 * then RML send a message to VPID 0 (MCW rank 0) when it returns
 * (MPIR_Breakpoint() doesn't return until the debugger has attached
 * to all relevant processes).  Meanwhile, VPID 0 blocks waiting for
 * the RML message.  All other VPIDs immediately call the grpcomm
 * barrier (and therefore block until the debugger attaches).  Once
 * VPID 0 receives the RML message, we know that the debugger has
 * attached to all processes that it cares about, and VPID 0 then
 * joins the grpcomm barrier, allowing the job to continue.  This
 * scheme has the side effect of nicely supporting partial attaches by
 * parallel debuggers (i.e., attaching to only some of the MPI
 * processes; not necessarily all of them).
 *
 * 2. If not using orterun: in this case, ORTE_DISABLE_FULL_SUPPORT
 * will be true, and we know that there will not be an RML message
 * sent to VPID 0.  So we have to look for a magic environment
 * variable from the launcher to know if the jobs will be attached by
 * a debugger (e.g., set by yod, srun, ...etc.), and if so, spin on
 * MPIR_debug_gate.  These environment variable names must be
 * hard-coded in the OMPI layer (see ompi/debuggers/ompi_debuggers.c).
 */

#if !defined(__WINDOWS__)

/* local globals and functions */
static void attach_debugger(int fd, short event, void *arg);
static void build_debugger_args(orte_app_context_t *debugger);
static void open_fifo(void);
static opal_event_t attach;
static int attach_fd = -1;
static bool fifo_active=false;
#define DUMP_INT(X) fprintf(stderr, "  %s = %d\n", # X, X);
#define FILE_MODE (S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)

struct MPIR_PROCDESC {
    char *host_name;        /* something that can be passed to inet_addr */
    char *executable_name;  /* name of binary */
    int pid;                /* process pid */
};


static void orte_debugger_dump(void)
{
    int i;

    DUMP_INT(MPIR_being_debugged);
    DUMP_INT(MPIR_debug_state);
    DUMP_INT(MPIR_partial_attach_ok);
    DUMP_INT(MPIR_i_am_starter);
    DUMP_INT(MPIR_forward_output);
    DUMP_INT(MPIR_proctable_size);
    fprintf(stderr, "  MPIR_proctable:\n");
    for (i = 0; i < MPIR_proctable_size; i++) {
        fprintf(stderr,
                "    (i, host, exe, pid) = (%d, %s, %s, %d)\n",
                i,
                MPIR_proctable[i].host_name,
                MPIR_proctable[i].executable_name,
                MPIR_proctable[i].pid);
    }
    fprintf(stderr, "MPIR_executable_path: %s\n",
            ('\0' == MPIR_executable_path[0]) ?
            "NULL" : (char*) MPIR_executable_path);
    fprintf(stderr, "MPIR_server_arguments: %s\n",
            ('\0' == MPIR_server_arguments[0]) ?
            "NULL" : (char*) MPIR_server_arguments);
}

/**
 * Initialization of data structures for running under a debugger
 * using the MPICH/TotalView parallel debugger interface.  Before the
 * spawn we need to check if we are being run under a TotalView-like
 * debugger; if so then inform applications via an MCA parameter.
 */
static void orte_debugger_init_before_spawn(orte_job_t *jdata)
{
    char *env_name;
    orte_app_context_t *app;
    int i;
    int32_t ljob;
    char *attach_fifo;

    if (!MPIR_being_debugged && !orte_in_parallel_debugger) {
        /* if we were given a test debugger, then we still want to
         * colaunch it
         */
        if (NULL != orte_debugger_test_daemon) {
            opal_output_verbose(2, orte_debug_output,
                                "%s No debugger test daemon specified",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            goto launchit;
        }
        /* if we were given an auto-detect rate, then we want to setup
         * an event so we periodically do the check
         */
        if (0 < orte_debugger_check_rate) {
            opal_output_verbose(2, orte_debug_output,
                                "%s Setting debugger attach check rate for %d seconds",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                orte_debugger_check_rate);
            ORTE_TIMER_EVENT(orte_debugger_check_rate, 0, attach_debugger);
        } else {
            /* create the attachment FIFO and setup readevent */
            /* create a FIFO name in the session dir */
            attach_fifo = opal_os_path(false, orte_process_info.job_session_dir, "debugger_attach_fifo", NULL);
            if ((mkfifo(attach_fifo, FILE_MODE) < 0) && errno != EEXIST) {
                opal_output(0, "CANNOT CREATE FIFO %s: errno %d", attach_fifo, errno);
                free(attach_fifo);
                return;
            }
            strncpy(MPIR_attach_fifo, attach_fifo, MPIR_MAX_PATH_LENGTH - 1);
	    free(attach_fifo);
	    open_fifo();
        }
        return;
    }
    
 launchit:
    opal_output_verbose(1, orte_debug_output, "Info: Spawned by a debugger");

    /* tell the procs they are being debugged */
    env_name = mca_base_param_environ_variable("orte", 
                                               "in_parallel_debugger", NULL);
    
    for (i=0; i < jdata->apps->size; i++) {
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
            continue;
        }
        opal_setenv(env_name, "1", true, &app->env);
    }
    free(env_name);

    /* check if we need to co-spawn the debugger daemons */
    if ('\0' != MPIR_executable_path[0] || NULL != orte_debugger_test_daemon) {
        /* can only have one debugger */
        if (NULL != orte_debugger_daemon) {
            opal_output(0, "-------------------------------------------\n"
                        "Only one debugger can be used on a job.\n"
                        "-------------------------------------------\n");
            ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
            return;
        }
        opal_output_verbose(2, orte_debug_output,
                            "%s Cospawning debugger daemons %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            (NULL == orte_debugger_test_daemon) ?
                            MPIR_executable_path : orte_debugger_test_daemon);
        /* add debugger info to launch message */
        orte_debugger_daemon = OBJ_NEW(orte_job_t);
        /* create a jobid for these daemons - this is done solely
         * to avoid confusing the rest of the system's bookkeeping
         */
        orte_plm_base_create_jobid(orte_debugger_daemon);
        /* flag the job as being debugger daemons */
        orte_debugger_daemon->controls |= ORTE_JOB_CONTROL_DEBUGGER_DAEMON;
        /* unless directed, we do not forward output */
        if (!MPIR_forward_output) {
            orte_debugger_daemon->controls &= ~ORTE_JOB_CONTROL_FORWARD_OUTPUT;
        }
        /* add it to the global job pool */
        ljob = ORTE_LOCAL_JOBID(orte_debugger_daemon->jobid);
        opal_pointer_array_set_item(orte_job_data, ljob, orte_debugger_daemon);
        /* create an app_context for the debugger daemon */
        app = OBJ_NEW(orte_app_context_t);
        if (NULL != orte_debugger_test_daemon) {
            app->app = strdup(orte_debugger_test_daemon);
        } else {
            app->app = strdup((char*)MPIR_executable_path);
        }
        opal_argv_append_nosize(&app->argv, app->app);
        build_debugger_args(app);
        opal_pointer_array_add(orte_debugger_daemon->apps, app);
        orte_debugger_daemon->num_apps = 1;
    }
}


/*
 * Initialization of data structures for running under a debugger
 * using the MPICH/TotalView parallel debugger interface. This stage
 * of initialization must occur after spawn
 * 
 * NOTE: We -always- perform this step to ensure that any debugger
 * that attaches to us post-launch of the application can get a
 * completed proctable
 */
static void orte_debugger_init_after_spawn(orte_job_t *jdata)
{
    orte_proc_t *proc;
    orte_app_context_t *appctx;
    orte_vpid_t i, j;
    opal_buffer_t buf;
    orte_process_name_t rank0;
    int rc;

    /* if we couldn't get thru the mapper stage, we might
     * enter here with no procs. Avoid the "zero byte malloc"
     * message by checking here
     */
    if (MPIR_proctable || 0 == jdata->num_procs) {
        /* already initialized */
        opal_output_verbose(5, orte_debug_output,
                            "%s: debugger already initialized or zero procs",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        return;
    }

    /* fill in the proc table for the application processes */
    
    opal_output_verbose(5, orte_debug_output,
                        "%s: Setting up debugger process table for applications",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
    
    MPIR_debug_state = 1;
    
    /* set the total number of processes in the job */
    MPIR_proctable_size = jdata->num_procs;
    
    /* allocate MPIR_proctable */
    MPIR_proctable = (struct MPIR_PROCDESC *)malloc(sizeof(struct MPIR_PROCDESC) *
                                                    MPIR_proctable_size);
    if (MPIR_proctable == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return;
    }
    
    if (orte_debugger_dump_proctable) {
        opal_output(orte_clean_output, "MPIR Proctable for job %s", ORTE_JOBID_PRINT(jdata->jobid));
    }

    /* initialize MPIR_proctable */
    for (j=0; j < jdata->num_procs; j++) {
        if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, j))) {
            continue;
        }
        /* store this data in the location whose index
         * corresponds to the proc's rank
         */
        i = proc->name.vpid;
        if (NULL == (appctx = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, proc->app_idx))) {
            continue;
        }
        
        MPIR_proctable[i].host_name = strdup(proc->node->name);
        if ( 0 == strncmp(appctx->app, OPAL_PATH_SEP, 1 )) { 
            MPIR_proctable[i].executable_name = 
            opal_os_path( false, appctx->app, NULL ); 
        } else {
            MPIR_proctable[i].executable_name =
            opal_os_path( false, appctx->cwd, appctx->app, NULL ); 
        } 
        MPIR_proctable[i].pid = proc->pid;
        if (orte_debugger_dump_proctable) {
            opal_output(orte_clean_output, "%s: Host %s Exe %s Pid %d",
                        ORTE_VPID_PRINT(i), MPIR_proctable[i].host_name,
                        MPIR_proctable[i].executable_name, MPIR_proctable[i].pid);
        }
    }

    if (0 < opal_output_get_verbosity(orte_debug_output)) {
        orte_debugger_dump();
    }

    /* if we are being launched under a debugger, then we must wait
     * for it to be ready to go and do some things to start the job
     */
    if (MPIR_being_debugged) {
        /* wait for all procs to have reported their contact info - this
         * ensures that (a) they are all into mpi_init, and (b) the system
         * has the contact info to successfully send a message to rank=0
         */
        ORTE_PROGRESSED_WAIT(false, jdata->num_reported, jdata->num_procs);
        
        MPIR_Breakpoint();
        
        /* send a message to rank=0 to release it */
        OBJ_CONSTRUCT(&buf, opal_buffer_t); /* don't need anything in this */
        rank0.jobid = jdata->jobid;
        rank0.vpid = 0;
        if (0 > (rc = orte_rml.send_buffer(&rank0, &buf, ORTE_RML_TAG_DEBUGGER_RELEASE, 0))) {
            opal_output(0, "Error: could not send debugger release to MPI procs - error %s", ORTE_ERROR_NAME(rc));
        }
        OBJ_DESTRUCT(&buf);
    }
}

static void open_fifo (void)
{
    if (attach_fd > 0) {
	close(attach_fd);
    }

    attach_fd = open(MPIR_attach_fifo, O_RDONLY | O_NONBLOCK, 0);
    if (attach_fd < 0) {
	opal_output(0, "%s unable to open debugger attach fifo",
		    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
	return;
    }
    opal_output_verbose(2, orte_debug_output,
			"%s Monitoring debugger attach fifo %s",
			ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
			MPIR_attach_fifo);
    opal_event_set(&attach, attach_fd, OPAL_EV_READ, attach_debugger, NULL);

    fifo_active = true;
    opal_event_add(&attach, 0);
}

static void attach_debugger(int fd, short event, void *arg)
{
    orte_app_context_t *app;
    unsigned char fifo_cmd;
    int rc;
    int32_t ljob;
    orte_job_t *jdata;
    int i;
    orte_node_t *node;
    orte_proc_t *proc;
    orte_vpid_t vpid=0;

    /* read the file descriptor to clear that event, if necessary */
    if (fifo_active) {
	opal_event_del(&attach);
	fifo_active = false;

        rc = read(attach_fd, &fifo_cmd, sizeof(fifo_cmd));
	if (!rc) {
	    /* reopen device to clear hangup */
	    open_fifo();
	    return;
	}
        if (1 != fifo_cmd) {
            /* ignore the cmd */
            goto RELEASE;
        }
    }

    if (!MPIR_being_debugged && !orte_debugger_test_attach) {
        /* false alarm */
        goto RELEASE;
    }

    opal_output_verbose(1, orte_debug_output,
                        "%s Attaching debugger %s", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        (NULL == orte_debugger_test_daemon) ? MPIR_executable_path : orte_debugger_test_daemon);

    /* a debugger has attached! All the MPIR_Proctable
     * data is already available, so we only need to
     * check to see if we should spawn any daemons
     */
    if ('\0' != MPIR_executable_path[0] || NULL != orte_debugger_test_daemon) {
        /* can only have one debugger */
        if (NULL != orte_debugger_daemon) {
            opal_output(0, "-------------------------------------------\n"
                        "Only one debugger can be used on a job.\n"
                        "-------------------------------------------\n");
            goto RELEASE;
        }
        opal_output_verbose(2, orte_debug_output,
                            "%s Spawning debugger daemons %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            (NULL == orte_debugger_test_daemon) ?
                            MPIR_executable_path : orte_debugger_test_daemon);
        /* this will be launched just like a regular job,
         * so we do not use the global orte_debugger_daemon
         * as this is reserved for co-location upon startup
         */
        jdata = OBJ_NEW(orte_job_t);
        /* create a jobid for these daemons - this is done solely
         * to avoid confusing the rest of the system's bookkeeping
         */
        orte_plm_base_create_jobid(jdata);
        /* flag the job as being debugger daemons */
        jdata->controls |= ORTE_JOB_CONTROL_DEBUGGER_DAEMON;
        /* unless directed, we do not forward output */
        if (!MPIR_forward_output) {
            jdata->controls &= ~ORTE_JOB_CONTROL_FORWARD_OUTPUT;
        }
       /* dont push stdin */
        jdata->stdin_target = ORTE_VPID_INVALID;
        /* add it to the global job pool */
        ljob = ORTE_LOCAL_JOBID(jdata->jobid);
        opal_pointer_array_set_item(orte_job_data, ljob, jdata);
        /* create an app_context for the debugger daemon */
        app = OBJ_NEW(orte_app_context_t);
        if (NULL != orte_debugger_test_daemon) {
            app->app = strdup(orte_debugger_test_daemon);
        } else {
            app->app = strdup((char*)MPIR_executable_path);
        }

	jdata->state = ORTE_JOB_STATE_INIT;

        opal_argv_append_nosize(&app->argv, app->app);
        build_debugger_args(app);
        opal_pointer_array_add(jdata->apps, app);
        jdata->num_apps = 1;

        /* create a job map */
        jdata->map = OBJ_NEW(orte_job_map_t);
        /* in building the map, we want to launch one debugger daemon
         * on each node that *already has an application process on it*.
         * We cannot just launch one debugger daemon on EVERY node because
         * the original job may not have placed procs on every node. So
         * we construct the map here by cycling across all nodes, adding
         * only those nodes where num_procs > 0.
         */
        for (i=0; i < orte_node_pool->size; i++) {
            if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, i))) {
                continue;
            }
            /* if this node isn't already occupied, ignore it */
            if (NULL == node->daemon) {
                continue;
            }
            /* if the node doesn't have any app procs on it, ignore it */
            if (node->num_procs < 1) {
                continue;
            }
            /* this node has at least one proc, so add it to our map */
            OBJ_RETAIN(node);
            opal_pointer_array_add(jdata->map->nodes, node);
            jdata->map->num_nodes++;
            /* add a debugger daemon to the node - note that the
             * debugger daemon does NOT count against our subscribed slots
             */
            proc = OBJ_NEW(orte_proc_t);
            proc->name.jobid = jdata->jobid;
            proc->name.vpid = vpid++;
	    /* set the local/node ranks - we don't actually care
	     * what these are, but the odls needs them
	     */
	    proc->local_rank = 0;
	    proc->node_rank = 0;
            /* flag the proc as ready for launch */
            proc->state = ORTE_PROC_STATE_INIT;
            proc->app_idx = 0;

            OBJ_RETAIN(node);  /* maintain accounting on object */    
            proc->node = node;
            proc->nodename = node->name;
            /* add the proc to the job */
            opal_pointer_array_set_item(jdata->procs, proc->name.vpid, proc);
            jdata->num_procs++;

            /* add the proc to the node's array */
            OBJ_RETAIN(proc);
            opal_pointer_array_add(node->procs, (void*)proc);
            node->num_procs++;
        }

        /* now go ahead and spawn this job */
        if (ORTE_SUCCESS != (rc = orte_plm.spawn(jdata))) {
            ORTE_ERROR_LOG(rc);
        }
    }
        
 RELEASE:
    /* reset the read or timer event */
    if (0 == orte_debugger_check_rate) {
        fifo_active = true;
        opal_event_add(&attach, 0);
    } else if (!MPIR_being_debugged) {
	ORTE_TIMER_EVENT(orte_debugger_check_rate, 0, attach_debugger);
    }

    /* notify the debugger that all is ready */
    MPIR_Breakpoint();
}

static void build_debugger_args(orte_app_context_t *debugger)
{
    int i, j;
    char mpir_arg[MPIR_MAX_ARG_LENGTH];

    if ('\0' != MPIR_server_arguments[0]) {
        j=0;
        memset(mpir_arg, 0, MPIR_MAX_ARG_LENGTH);
        for (i=0; i < MPIR_MAX_ARG_LENGTH; i++) {
            if (MPIR_server_arguments[i] == '\0') {
                if (0 < j) {
                    opal_argv_append_nosize(&debugger->argv, mpir_arg);
                    memset(mpir_arg, 0, MPIR_MAX_ARG_LENGTH);
                    j=0;
                }
            } else {
                mpir_arg[j] = MPIR_server_arguments[i];
                j++;
            }
        }
    }
}
#endif /* !defined(__WINDOWS__) */
