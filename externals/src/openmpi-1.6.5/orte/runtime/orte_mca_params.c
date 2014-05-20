/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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
 * Copyright (c) 2009-2011 Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c)      2011 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <stdio.h>

#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/paffinity/base/base.h"
#include "opal/mca/installdirs/installdirs.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"

#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_globals.h"

static bool passed_thru = false;

int orte_register_params(void)
{
    int value, tmp;
    char *strval, **params;
    uint16_t binding;
    
    /* only go thru this once - mpirun calls it twice, which causes
     * any error messages to show up twice
     */
    if (passed_thru) {
        return ORTE_SUCCESS;
    }
    passed_thru = true;
    
    mca_base_param_reg_int_name("orte", "base_help_aggregate",
                                "If orte_base_help_aggregate is true, duplicate help messages will be aggregated rather than displayed individually.  This can be helpful for parallel jobs that experience multiple identical failures; rather than print out the same help/failure message N times, display it once with a count of how many processes sent the same message.",
                                false, false,
                                (int) true, &value);
    orte_help_want_aggregate = OPAL_INT_TO_BOOL(value);
    
    mca_base_param_reg_string_name("orte", "tmpdir_base",
                                   "Base of the session directory tree",
                                   false, false, NULL,  &(orte_process_info.tmpdir_base));
   
    mca_base_param_reg_string_name("orte", "no_session_dirs",
                                   "Prohibited locations for session directories (multiple locations separated by ',', default=NULL)",
                                   false, false, NULL,  &orte_prohibited_session_dirs);

#if !ORTE_DISABLE_FULL_SUPPORT
    
    mca_base_param_reg_int_name("orte", "send_profile",
                                "Send profile info in launch message",
                                false, false,
                                (int) false, &value);
    orte_send_profile = OPAL_INT_TO_BOOL(value);

    mca_base_param_reg_int_name("orte", "debug",
                                "Top-level ORTE debug switch (default verbosity: 1)",
                                false, false, (int)false, &value);
    orte_debug_flag = OPAL_INT_TO_BOOL(value);
    
    mca_base_param_reg_int_name("orte", "debug_verbose",
                                "Verbosity level for ORTE debug messages (default: 1)",
                                false, false, -1, &orte_debug_verbosity);
    
   mca_base_param_reg_int_name("orte", "debug_daemons",
                                "Whether to debug the ORTE daemons or not",
                                false, false, (int)false, &value);
    orte_debug_daemons_flag = OPAL_INT_TO_BOOL(value);

    mca_base_param_reg_int_name("orte", "debug_daemons_file",
                                "Whether want stdout/stderr of daemons to go to a file or not",
                                false, false, (int)false, &value);
    orte_debug_daemons_file_flag = OPAL_INT_TO_BOOL(value);
    /* If --debug-daemons-file was specified, that also implies
        --debug-daemons */
    if (orte_debug_daemons_file_flag) {
        orte_debug_daemons_flag = true;
    }

    mca_base_param_reg_int_name("orte", "daemon_bootstrap",
                                "Bootstrap the connection to the HNP",
                                false, false, (int)false, &value);
    orte_daemon_bootstrap = OPAL_INT_TO_BOOL(value);

    /* do we want session output left open? */
    mca_base_param_reg_int_name("orte", "leave_session_attached",
                                "Whether applications and/or daemons should leave their sessions "
                                "attached so that any output can be received - this allows X forwarding "
                                "without all the attendant debugging output",
                                false, false, (int)false, &value);
    orte_leave_session_attached = OPAL_INT_TO_BOOL(value);
    
    /* See comment in orte/tools/orterun/orterun.c about this MCA
       param (this param is internal) */
    mca_base_param_reg_int_name("orte",
                                "in_parallel_debugger",
                                "Whether the application is being debugged "
                                "in a parallel debugger (default: false)",
                                true, false, 0, &value);
    orte_in_parallel_debugger = OPAL_INT_TO_BOOL(value);
    
    mca_base_param_reg_int_name("orte",
                                "output_debugger_proctable",
                                "Whether or not to output the debugger proctable after launch (default: false)",
                                false, false, 0, &value);
    orte_debugger_dump_proctable = OPAL_INT_TO_BOOL(value);

    mca_base_param_reg_string_name("orte", "debugger_test_daemon",
                                   "Name of the executable to be used to simulate a debugger colaunch (relative or absolute path)",
                                   false, false, NULL, &orte_debugger_test_daemon);

    mca_base_param_reg_int_name("orte",
                                "debugger_test_attach",
                                "Test debugger colaunch after debugger attachment",
                                false, false, 0, &value);
    orte_debugger_test_attach = OPAL_INT_TO_BOOL(value);

    mca_base_param_reg_int_name("orte",
                                "debugger_check_rate",
                                "Set rate (in secs) for auto-detect of debugger attachment (0 => do not check)",
                                false, false, 0, &orte_debugger_check_rate);

    mca_base_param_reg_int_name("orte", "do_not_launch",
                                "Perform all necessary operations to prepare to launch the application, but do not actually launch it",
                                false, false, (int)false, &value);
    orte_do_not_launch = OPAL_INT_TO_BOOL(value);
    
    mca_base_param_reg_int_name("orte", "daemon_spin",
                                "Have any orteds spin until we can connect a debugger to them",
                                false, false, (int)false, &value);
    orted_spin_flag = OPAL_INT_TO_BOOL(value);

    mca_base_param_reg_int_name("orte", "daemon_fail",
                                "Have the specified orted fail after init for debugging purposes",
                                false, false, ORTE_VPID_INVALID, &orted_debug_failure);
    
    mca_base_param_reg_int_name("orte", "daemon_fail_delay",
                                "Have the specified orted fail after specified number of seconds (default: 0 => no delay)",
                                false, false, 0, &orted_debug_failure_delay);

    mca_base_param_reg_int_name("orte", "heartbeat_rate",
                                "Seconds between checks for daemon state-of-health (default: 0 => do not check)",
                                false, false, 0, &orte_heartbeat_rate);
    
    mca_base_param_reg_int_name("orte", "startup_timeout",
                                "Milliseconds/daemon to wait for startup before declaring failed_to_start (default: 0 => do not check)",
                                false, false, 0, &orte_startup_timeout);
 
    /* check for timing requests */
    mca_base_param_reg_int_name("orte", "timing",
                                "Request that critical timing loops be measured",
                                false, false, (int)false, &value);
    orte_timing = OPAL_INT_TO_BOOL(value);

    mca_base_param_reg_int_name("orte", "timing_details",
                                "Request that detailed timing data by reported",
                                false, false, (int)false, &value);
    orte_timing_details = OPAL_INT_TO_BOOL(value);
    if (orte_timing_details) {
        /* ensure the timing flag is set too */
        orte_timing = true;
    }
    
    if (ORTE_PROC_IS_HNP) {
        char *tmp;
        mca_base_param_reg_string_name("orte", "timing_file",
                                       "Name of the file where timing data is to be written (relative or absolute path)",
                                       false, false, NULL, &tmp);
        if (orte_timing && NULL == tmp) {
            /* send the timing output to stdout */
            orte_timing_output = stdout;
        } else if (NULL != tmp) {
            /* make sure the timing flag is set */
            orte_timing = true;
            /* send the output to the indicated file */
            orte_timing_output = fopen(tmp,  "w");
            if (NULL == orte_timing_output) {
                /* couldn't be opened */
                opal_output(0, "File %s could not be opened", tmp);
                orte_timing_output = stderr;
            }
        }        
    }
    
    /* User-level debugger info string */

    mca_base_param_reg_string_name("orte", "base_user_debugger",
                                   "Sequence of user-level debuggers to search for in orterun",
                                   false, false, "totalview @mpirun@ -a @mpirun_args@ : ddt -n @np@ -start @executable@ @executable_argv@ @single_app@ : fxp @mpirun@ -a @mpirun_args@", NULL);


    mca_base_param_reg_int_name("orte", "abort_timeout",
                                "Max time to wait [in secs] before aborting an ORTE operation (default: 1sec)",
                                false, false, 1, &value);
    orte_max_timeout = 1000000.0 * value;  /* convert to usec */

    mca_base_param_reg_int_name("orte", "timeout_step",
                                "Time to wait [in usecs/proc] before aborting an ORTE operation (default: 1000 usec/proc)",
                                false, false, 1000, &orte_timeout_usec_per_proc);
    
    /* default hostfile */
    asprintf(&orte_default_hostfile, "%s/openmpi-default-hostfile", opal_install_dirs.sysconfdir);
    mca_base_param_reg_string_name("orte", "default_hostfile",
                                   "Name of the default hostfile (relative or absolute path, \"none\" to ignore environmental or default MCA param setting)",
                                   false, false, orte_default_hostfile, &orte_default_hostfile);
    if (0 == strcmp(orte_default_hostfile, "none")) {
        free(orte_default_hostfile);
        orte_default_hostfile = NULL;
    }

    /* rankfile */
    tmp = mca_base_param_reg_string_name("orte", "rankfile",
                                         "Name of the rankfile to be used for mapping processes (relative or absolute path)",
                                         false, false, NULL, NULL);
    mca_base_param_reg_syn_name(tmp, "rmaps", "rank_file_path", false);
    mca_base_param_lookup_string(tmp, &orte_rankfile);
    
#ifdef __WINDOWS__
    mca_base_param_reg_string_name("orte", "ccp_headnode",
                          "Name of the cluster head node. (For Windows CCP only.)",
                          false, false,
                          NULL, &orte_ccp_headnode);
#endif
    
    /* whether or not to keep FQDN hostnames */
    mca_base_param_reg_int_name("orte", "keep_fqdn_hostnames",
                                "Whether or not to keep FQDN hostnames [default: no]",
                                false, false, (int)false, &value);
    orte_keep_fqdn_hostnames = OPAL_INT_TO_BOOL(value);
    
    /* whether or not to use regular expressions for launch */
    mca_base_param_reg_int_name("orte", "use_regexp",
                                "Whether or not to use regular expressions for launch [default: no]",
                                false, false, (int)false, &value);
    orte_use_regexp = OPAL_INT_TO_BOOL(value);

    /* whether to tag output */
    mca_base_param_reg_int_name("orte", "tag_output",
                                "Tag all output with [job,rank] (default: false)",
                                false, false, (int) false, &value);
    orte_tag_output = OPAL_INT_TO_BOOL(value);
    /* if we requested xml output, be sure to tag the output as well */
    if (orte_xml_output) {
        orte_tag_output = true;
    }
    
    mca_base_param_reg_int_name("orte", "xml_output",
                                "Display all output in XML format (default: false)",
                                false, false, (int) false, &value);
    orte_xml_output = OPAL_INT_TO_BOOL(value);

    mca_base_param_reg_string_name("orte", "xml_file",
                                   "Provide all output in XML format to the specified file",
                                   false, false, NULL, &strval);
    if (NULL != strval) {
        if (ORTE_PROC_IS_HNP && NULL == orte_xml_fp) {
            /* only the HNP opens this file! Make sure it only happens once */
            orte_xml_fp = fopen(strval, "w");
            if (NULL == orte_xml_fp) {
                opal_output(0, "Could not open specified xml output file: %s", strval);
                return ORTE_ERROR;
            }
        }
        /* ensure we set the flags to tag output */
        orte_xml_output = true;
        orte_tag_output = true;
    } else {
        /* default to stdout */
        orte_xml_fp = stdout;
    }
        
    /* whether to timestamp output */
    mca_base_param_reg_int_name("orte", "timestamp_output",
                                "Timestamp all application process output (default: false)",
                                false, false, (int) false, &value);
    orte_timestamp_output = OPAL_INT_TO_BOOL(value);
    
    /* redirect output into files */
    mca_base_param_reg_string_name("orte", "output_filename",
                                   "Redirect output from application processes into filename.rank [default: NULL]",
                                   false, false, NULL, &orte_output_filename);
    
    mca_base_param_reg_int_name("orte", "show_resolved_nodenames",
                                "Display any node names that are resolved to a different name (default: false)",
                                false, false, (int) false, &value);
    orte_show_resolved_nodenames = OPAL_INT_TO_BOOL(value);
    
    mca_base_param_reg_int_name("orte", "hetero_apps",
                                "Indicates that multiple app_contexts are being provided that are a mix of 32/64 bit binaries (default: false)",
                                false, false, (int) false, &value);
    orte_hetero_apps = OPAL_INT_TO_BOOL(value);
    
    /* allow specification of the launch agent */
    mca_base_param_reg_string_name("orte", "launch_agent",
                                   "Command used to start processes on remote nodes (default: orted)",
                                   false, false, "orted", &orte_launch_agent);

    /* whether or not to require RM allocation */
    mca_base_param_reg_int_name("orte", "allocation_required",
                                "Whether or not an allocation by a resource manager is required [default: no]",
                                false, false, (int)false, &value);
    orte_allocation_required = OPAL_INT_TO_BOOL(value);

    /* generate new terminal windows to display output from specified ranks */
    mca_base_param_reg_string_name("orte", "xterm",
                                   "Create a new xterm window and display output from the specified ranks there [default: none]",
                                   false, false, NULL, &orte_xterm);
    if (NULL != orte_xterm) {
        /* if an xterm request is given, we have to leave any ssh
         * sessions attached so the xterm window manager can get
         * back to the controlling terminal
         */
        orte_leave_session_attached = true;
    }


    /* whether or not to forward SIGTSTP and SIGCONT signals */
    mca_base_param_reg_int_name("orte", "forward_job_control",
                                "Forward SIGTSTP (after converting to SIGSTOP) and SIGCONT signals to the application procs [default: no]",
                                false, false,
                                (int) false, &value);
    orte_forward_job_control = OPAL_INT_TO_BOOL(value);
    
    /* local rsh/ssh launch agent */
    tmp = mca_base_param_reg_string_name("orte", "rsh_agent",
                                         "The command used to launch executables on remote nodes (typically either \"ssh\" or \"rsh\")",
                                         false, false, "ssh : rsh", NULL);
    mca_base_param_reg_syn_name(tmp, "pls", "rsh_agent", true);
    mca_base_param_reg_syn_name(tmp, "plm", "rsh_agent", true);
    mca_base_param_lookup_string(tmp, &orte_rsh_agent);

    tmp = mca_base_param_reg_int_name("orte", "assume_same_shell",
                                      "If set to 1, assume that the shell on the remote node is the same as the shell on the local node.  Otherwise, probe for what the remote shell [default: 1]",
                                      false, false, 1, NULL);
    mca_base_param_reg_syn_name(tmp, "plm", "rsh_assume_same_shell", true);
    mca_base_param_lookup_int(tmp, &value);
    orte_assume_same_shell = OPAL_INT_TO_BOOL(value);
    
    /* whether or not to report launch progress */
    mca_base_param_reg_int_name("orte", "report_launch_progress",
                                "Output a brief periodic report on launch progress [default: no]",
                                false, false,
                                (int) false, &value);
    orte_report_launch_progress = OPAL_INT_TO_BOOL(value);
    if (orte_report_launch_progress) {
        /* ensure the startup timeout is set to something reasonable */
        if (0 == orte_startup_timeout) {
            orte_startup_timeout = 2000;  /* default to 2 seconds */
        }
    }
    
    /* cluster hardware info detected by orte only */
    mca_base_param_reg_string_name("orte", "cpu_type",
				   "cpu type detected in node",
				   true, false, NULL, &orte_local_cpu_type);
    mca_base_param_reg_string_name("orte", "cpu_model",
                                   "cpu model detected in node",
                                   true, false, NULL, &orte_local_cpu_model);

    /* cluster hardware info */
    mca_base_param_reg_int_name("orte", "num_boards",
                                "Number of processor boards/node (1-256) [default: 1]",
                                false, false, 1, &value);
    orte_default_num_boards = (uint8_t)value;

    mca_base_param_reg_int_name("orte", "num_sockets",
                                "Number of sockets/board (1-256)",
                                false, false, 0, &value);
    orte_default_num_sockets_per_board = (uint8_t)value;

    mca_base_param_reg_int_name("orte", "num_cores",
                                "Number of cores/socket (1-256)",
                                false, false, 0, &value);
    orte_default_num_cores_per_socket = (uint8_t)value;
    
    /* binding specification - this will be overridden by any cmd line directive, and
     * ignored unless opal_paffinity_alone is set
     */
	mca_base_param_reg_string_name("orte", "process_binding",
						     "Policy for binding processes [none | core | socket | board] (supported qualifier: if-avail)",
						     false, false, NULL, &strval);
	if (NULL != strval) {
        if (0 == strcasecmp(strval, "none")) {
            /* no binding */
            ORTE_SET_BINDING_POLICY(ORTE_BIND_TO_NONE);
        } else {
            binding = 0;
            params = opal_argv_split(strval, ':');
            if (1 < opal_argv_count(params)) {
                if (0 != strcasecmp(params[1], "if-avail")) {
                    /* unknown option */
                    opal_output(0, "Unknown qualifier to orte_process_binding: %s", strval);
                    return ORTE_ERR_BAD_PARAM;
                }
                binding = ORTE_BIND_IF_SUPPORTED;
            }
            if (0 == strcasecmp(params[0], "socket")) {
                ORTE_SET_BINDING_POLICY(ORTE_BIND_TO_SOCKET | binding);
            } else if (0 == strcasecmp(params[0], "board")) {
                ORTE_SET_BINDING_POLICY(ORTE_BIND_TO_BOARD | binding);
            } else if (0 == strcasecmp(params[0], "core")) {
                ORTE_SET_BINDING_POLICY(ORTE_BIND_TO_CORE | binding);
            }
        }
    }
    /* if nothing was set, but opal_paffinity_alone is set, then default
     * to bind-to-core
     */
    if (opal_paffinity_alone) {
        ORTE_XSET_BINDING_POLICY(ORTE_BIND_TO_CORE);
    }
    
    /* whether or not to report bindings */
    mca_base_param_reg_int_name("orte", "report_bindings",
                                "Report bindings",
                                false, false,
                                (int) false, &value);
    orte_report_bindings = OPAL_INT_TO_BOOL(value);
    
    /* tool communication controls */
    mca_base_param_reg_string_name("orte", "report_events",
                                   "URI to which events are to be reported (default: NULL)]",
                                   false, false, NULL, &orte_report_events_uri);
    if (NULL != orte_report_events_uri) {
        orte_report_events = true;
    }
    
    /* barrier control */
    mca_base_param_reg_int_name("orte", "do_not_barrier",
                                "Do not barrier in orte_init",
                                true, false,
                                (int) false, &value);
    orte_do_not_barrier = OPAL_INT_TO_BOOL(value);

#endif /* ORTE_DISABLE_FULL_SUPPORT */
    
    return ORTE_SUCCESS;
}
