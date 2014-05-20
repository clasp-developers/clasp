/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
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


#include "opal/event/event.h"
#include "opal/mca/base/base.h"
#include "opal/util/cmd_line.h"
#include "opal/util/output.h"
#include "orte/util/show_help.h"
#include "opal/util/daemon_init.h"
#include "opal/runtime/opal.h"
#include "opal/runtime/opal_cr.h"
#include "opal/mca/base/mca_base_param.h"


#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_data_server.h"

/*
 * Globals
 */

static opal_event_t term_handler;
static opal_event_t int_handler;

static void shutdown_callback(int fd, short flags, void *arg);

static bool help=false;
static bool debug=false;
static bool no_daemonize=false;
static char *report_uri=NULL;

/*
 * define the context table for obtaining parameters
 */
opal_cmd_line_init_t ompi_server_cmd_line_opts[] = {
    /* Various "obvious" options */
    { NULL, NULL, NULL, 'h', NULL, "help", 0,
      &help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },

    { NULL, NULL, NULL, 'd', NULL, "debug", 0,
      &debug, OPAL_CMD_LINE_TYPE_BOOL,
        "Debug the Open MPI server" },
        
    { "orte", "no_daemonize", NULL, '\0', NULL, "no-daemonize", 0,
      &no_daemonize, OPAL_CMD_LINE_TYPE_BOOL,
      "Don't daemonize into the background" },
    
    { NULL, NULL, NULL, 'r', NULL, "report-uri", 1,
      &report_uri, OPAL_CMD_LINE_TYPE_STRING,
      "Report the server's uri on stdout [-], stderr [+], or a file [anything else]"},

    /* End of list */
    { NULL, NULL, NULL, '\0', NULL, NULL, 0,
      NULL, OPAL_CMD_LINE_TYPE_NULL, NULL }
};

int main(int argc, char *argv[])
{
    int ret = 0;
    opal_cmd_line_t *cmd_line = NULL;
    char *rml_uri;
    char * tmp_env_var = NULL;

    /* init enough of opal to process cmd lines */
    if (OPAL_SUCCESS != opal_init_util(&argc, &argv)) {
        fprintf(stderr, "OPAL failed to initialize -- orted aborting\n");
        exit(1);
    }
    
    /* setup to check common command line options that just report and die */
    cmd_line = OBJ_NEW(opal_cmd_line_t);
    opal_cmd_line_create(cmd_line, ompi_server_cmd_line_opts);
    mca_base_cmd_line_setup(cmd_line);
    if (OPAL_SUCCESS != (ret = opal_cmd_line_parse(cmd_line, false,
                                                   argc, argv))) {
        char *args = NULL;
        args = opal_cmd_line_get_usage_msg(cmd_line);
        orte_show_help("help-ompi-server.txt", "ompiserver:usage", false,
                       argv[0], args);
        free(args);
        return ret;
    }
    
    /* check for help request */
    if (help) {
        char *args = NULL;
        args = opal_cmd_line_get_usage_msg(cmd_line);
        orte_show_help("help-ompi-server.txt", "ompiserver:usage", false,
                       argv[0], args);
        free(args);
        return 1;
    }

    /*
     * Since this process can now handle MCA/GMCA parameters, make sure to
     * process them.
     */
    mca_base_cmd_line_process_args(cmd_line, &environ, &environ);
    
    /* if debug is set, then set orte_debug_flag so that the data server
     * code will output
     */
    if (debug) {
        putenv("OMPI_MCA_orte_debug=1");
    }
        
    /* detach from controlling terminal
     * otherwise, remain attached so output can get to us
     */
    if(debug == false &&
       no_daemonize == false) {
        opal_daemon_init(NULL);
    }

#if OPAL_ENABLE_FT_CR == 1
    /* Disable the checkpoint notification routine for this
     * tool. As we will never need to checkpoint this tool.
     * Note: This must happen before opal_init().
     */
    opal_cr_set_enabled(false);
    
    /* Select the none component, since we don't actually use a checkpointer */
    tmp_env_var = mca_base_param_env_var("crs");
    opal_setenv(tmp_env_var,
                "none",
                true, &environ);
    free(tmp_env_var);
    tmp_env_var = NULL;

    /* Mark as a tool program */
    tmp_env_var = mca_base_param_env_var("opal_cr_is_tool");
    opal_setenv(tmp_env_var,
                "1",
                true, &environ);
    free(tmp_env_var);
#else
    tmp_env_var = NULL; /* Silence compiler warning */
#endif

    /* Perform the standard init, but flag that we are a tool
     * so that we only open up the communications infrastructure. No
     * session directories will be created.
     */
    if (ORTE_SUCCESS != (ret = orte_init(&argc, &argv, ORTE_PROC_TOOL))) {
        fprintf(stderr, "ompi-server: failed to initialize -- aborting\n");
        exit(1);
    }
    
    /* report out our URI, if we were requested to do so, using syntax
     * proposed in an email thread by Jeff Squyres
     */
    if (NULL != report_uri) {
        rml_uri = orte_rml.get_contact_info();
        if (0 == strcmp(report_uri, "-")) {
            /* if '-', then output to stdout */
            printf("%s\n", rml_uri);
        } else if (0 == strcmp(report_uri, "+")) {
            /* if '+', output to stderr */
            fprintf(stderr, "%s\n", rml_uri);
        } else {
            /* treat it as a filename and output into it */
            FILE *fp;
            fp = fopen(report_uri, "w");
            if (NULL == fp) {
                fprintf(stderr, "ompi-server: failed to open designated file %s -- aborting\n", report_uri);
                orte_finalize();
                exit(1);
            }
            fprintf(fp, "%s\n", rml_uri);
            fclose(fp);
        }
        free(rml_uri);
    }
    
    /* setup the data server to listen for commands */
    if (ORTE_SUCCESS != (ret = orte_data_server_init())) {
        fprintf(stderr, "ompi-server: failed to start data server -- aborting\n");
        orte_finalize();
        exit(1);
    }
    
    /* Set signal handlers to catch kill signals so we can properly clean up
     * after ourselves. 
     */
    opal_event_set(&term_handler, SIGTERM, OPAL_EV_SIGNAL,
                   shutdown_callback, NULL);
    opal_event_add(&term_handler, NULL);
    opal_event_set(&int_handler, SIGINT, OPAL_EV_SIGNAL,
                   shutdown_callback, NULL);
    opal_event_add(&int_handler, NULL);

    /* We actually do *not* want the server to voluntarily yield() the
       processor more than necessary.  The server already blocks when
       it is doing nothing, so it doesn't use any more CPU cycles than
       it should; but when it *is* doing something, we do not want it
       to be unnecessarily delayed because it voluntarily yielded the
       processor in the middle of its work.

       For example: when a message arrives at the server, we want the
       OS to wake up the server in a timely fashion (which most OS's
       seem good about doing) and then we want the server to process
       the message as fast as possible.  If the server yields and lets
       aggressive MPI applications get the processor back, it may be a
       long time before the OS schedules the server to run again
       (particularly if there is no IO event to wake it up).  Hence,
       publish and lookup (for example) may be significantly delayed
       before being delivered to MPI processes, which can be
       problematic in some scenarios (e.g., COMM_SPAWN). */
    opal_progress_set_yield_when_idle(false);

    /* Change the default behavior of libevent such that we want to
       continually block rather than blocking for the default timeout
       and then looping around the progress engine again.  There
       should be nothing in the server that cannot block in libevent
       until "something" happens (i.e., there's no need to keep
       cycling through progress because the only things that should
       happen will happen in libevent).  This is a minor optimization,
       but what the heck... :-) */
    opal_progress_set_event_flag(OPAL_EVLOOP_ONCE);

    if (debug) {
        opal_output(0, "%s ompi-server: up and running!", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
    }

    /* wait to hear we are done */
    opal_event_dispatch();

    /* should never get here, but if we do... */
    
    /* Finalize and clean up ourselves */
    if (ORTE_SUCCESS != (ret = orte_finalize())) {
        ORTE_ERROR_LOG(ret);
    }
    return ret;
}

static void shutdown_callback(int fd, short flags, void *arg)
{
    int ret;
    
    if (debug) {
        opal_output(0, "%s ompi-server: finalizing", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
    }
    
    /* Finalize and clean up ourselves */
    if (ORTE_SUCCESS != (ret = orte_finalize())) {
        ORTE_ERROR_LOG(ret);
    }
    exit(ret);
}
