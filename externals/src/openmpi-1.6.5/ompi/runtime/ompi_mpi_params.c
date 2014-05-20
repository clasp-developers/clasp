/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_TIME_H
#include <time.h>
#endif  /* HAVE_TIME_H */

#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/runtime/mpiruntime.h"
#include "ompi/runtime/params.h"
#include "orte/util/show_help.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"

/*
 * Global variables
 *
 * As a deviation from the norm, ompi_mpi_param_check is also
 * extern'ed in src/mpi/interface/c/bindings.h because it is already
 * included in all MPI function imlementation files
 *
 * The values below are the default values.
 */
bool ompi_mpi_param_check = true;
bool ompi_debug_show_handle_leaks = false;
int ompi_debug_show_mpi_alloc_mem_leaks = 0;
bool ompi_debug_no_free_handles = false;
bool ompi_mpi_show_mca_params = false;
char *ompi_mpi_show_mca_params_file = NULL;
bool ompi_mpi_abort_print_stack = false;
int ompi_mpi_abort_delay = 0;
bool ompi_mpi_keep_peer_hostnames = true;
bool ompi_mpi_keep_fqdn_hostnames = false;
int ompi_mpi_leave_pinned = -1;
bool ompi_mpi_leave_pinned_pipeline = false;
bool ompi_have_sparse_group_storage = OPAL_INT_TO_BOOL(OMPI_GROUP_SPARSE);
bool ompi_use_sparse_group_storage = OPAL_INT_TO_BOOL(OMPI_GROUP_SPARSE);
bool ompi_notify_init_finalize = true;

static bool show_default_mca_params = false;
static bool show_file_mca_params = false;
static bool show_enviro_mca_params = false;
static bool show_override_mca_params = false;

int ompi_mpi_register_params(void)
{
    int value;
    char *param;

    /* Whether we want MPI API function parameter checking or not */

    mca_base_param_reg_int_name("mpi", "param_check", 
                                "Whether you want MPI API parameters checked at run-time or not.  Possible values are 0 (no checking) and 1 (perform checking at run-time)",
                                false, false, MPI_PARAM_CHECK, &value);
    ompi_mpi_param_check = OPAL_INT_TO_BOOL(value);
    if (ompi_mpi_param_check) {
        value = 0;
        if (MPI_PARAM_CHECK) {
            value = 1;
        }
        if (0 == value) {
            orte_show_help("help-mpi-runtime.txt", 
                           "mpi-param-check-enabled-but-compiled-out",
                           true);
            ompi_mpi_param_check = false;
        }
    }
    
    /*
     * opal_progress: decide whether to yield and the event library
     * tick rate
     */
    /* JMS: Need ORTE data here -- set this to 0 when
       exactly/under-subscribed, or 1 when oversubscribed */
    mca_base_param_reg_int_name("mpi", "yield_when_idle", 
                                "Yield the processor when waiting for MPI communication (for MPI processes, will default to 1 when oversubscribing nodes)",
                                false, false, -1, NULL);
    mca_base_param_reg_int_name("mpi", "event_tick_rate", 
                                "How often to progress TCP communications (0 = never, otherwise specified in microseconds)",
                                false, false, -1, NULL);

    /* Whether or not to show MPI handle leaks */
    
    mca_base_param_reg_int_name("mpi", "show_handle_leaks",
                                "Whether MPI_FINALIZE shows all MPI handles that were not freed or not",
                                false, false, 
                                (int) ompi_debug_show_handle_leaks, &value);
    ompi_debug_show_handle_leaks = OPAL_INT_TO_BOOL(value);
    
    /* Whether or not to free MPI handles.  Useless without run-time
       param checking, so implicitly set that to true if we don't want
       to free the handles. */
    
    mca_base_param_reg_int_name("mpi", "no_free_handles", 
                                "Whether to actually free MPI objects when their handles are freed",
                                false, false, 
                                (int) ompi_debug_no_free_handles, &value);
    ompi_debug_no_free_handles = OPAL_INT_TO_BOOL(value);
    if (ompi_debug_no_free_handles) {
        ompi_mpi_param_check = true;
        value = 0;
        if (MPI_PARAM_CHECK) {
            value = 1;
        }
        if (0 == value) {
            opal_output(0, "WARNING: MCA parameter mpi_no_free_handles set to true, but MPI");
            opal_output(0, "WARNING: parameter checking has been compiled out of Open MPI.");
            opal_output(0, "WARNING: mpi_no_free_handles is therefore only partially effective!");
        }
    }

    /* Whether or not to show MPI_ALLOC_MEM leaks */

    mca_base_param_reg_int_name("mpi", "show_mpi_alloc_mem_leaks",
                                "If >0, MPI_FINALIZE will show up to this many instances of memory allocated by MPI_ALLOC_MEM that was not freed by MPI_FREE_MEM",
                                false, false, 
                                ompi_debug_show_mpi_alloc_mem_leaks,
                                &ompi_debug_show_mpi_alloc_mem_leaks);

    /* Whether or not to print all MCA parameters in MPI_INIT */
    mca_base_param_reg_string_name("mpi", "show_mca_params",
                                   "Whether to show all MCA parameter values during MPI_INIT or not (good for reproducability of MPI jobs "
                                   "for debug purposes). Accepted values are all, default, file, api, and enviro - or a comma "
                                   "delimited combination of them",
                                   false, false, NULL,  &param);
    if (NULL != param) {
        char **args;
        int i;
        
        ompi_mpi_show_mca_params = true;
        args = opal_argv_split(param, ',');
        if (NULL == args) {
            opal_output(0, "WARNING: could not parse mpi_show_mca_params request - defaulting to show \"all\"");
            show_default_mca_params = true;
            show_file_mca_params = true;
            show_enviro_mca_params = true;
            show_override_mca_params = true;
        } else {
            for (i=0; NULL != args[i]; i++) {
                if (0 == strcasecmp(args[i], "all")  || 0 == strcmp(args[i], "1")) {
                    show_default_mca_params = true;
                    show_file_mca_params = true;
                    show_enviro_mca_params = true;
                    show_override_mca_params = true;
                } else if (0 == strcasecmp(args[i], "default")) {
                    show_default_mca_params = true;
                } else if (0 == strcasecmp(args[i], "file")) {
                    show_file_mca_params = true;
                } else if (0 == strncasecmp(args[i], "env", 3)) {
                    show_enviro_mca_params = true;
                } else if (0 == strcasecmp(args[i], "api")) {
                    show_override_mca_params = true;
                }
            }
            opal_argv_free(args);
        }
        free(param);
    }

    /* File to use when dumping the parameters */
    mca_base_param_reg_string_name("mpi", "show_mca_params_file",
                                   "If mpi_show_mca_params is true, setting this string to a valid filename tells Open MPI to dump all the MCA parameter values into a file suitable for reading via the mca_param_files parameter (good for reproducability of MPI jobs)",
                                   false, false,
                                   "", &ompi_mpi_show_mca_params_file);
    
    /* User-level process pinning controls */

    /* Do we want to save hostnames for debugging messages?  This can
       eat quite a bit of memory... */

    mca_base_param_reg_int_name("mpi", "keep_peer_hostnames",
                                "If nonzero, save the string hostnames of all MPI peer processes (mostly for error / debugging output messages).  This can add quite a bit of memory usage to each MPI process.",
                                false, false, 1, &value);
    ompi_mpi_keep_peer_hostnames = OPAL_INT_TO_BOOL(value);

    /* MPI_ABORT controls */

    mca_base_param_reg_int_name("mpi", "abort_delay",
                                "If nonzero, print out an identifying message when MPI_ABORT is invoked (hostname, PID of the process that called MPI_ABORT) and delay for that many seconds before exiting (a negative delay value means to never abort).  This allows attaching of a debugger before quitting the job.",
                                false, false, 
                                ompi_mpi_abort_delay,
                                &ompi_mpi_abort_delay);
    
    mca_base_param_reg_int_name("mpi", "abort_print_stack",
                                "If nonzero, print out a stack trace when MPI_ABORT is invoked",
                                false, 
                                /* If we do not have stack trace
                                   capability, make this a read-only
                                   MCA param */
#if OPAL_WANT_PRETTY_PRINT_STACKTRACE && ! defined(__WINDOWS__) && defined(HAVE_BACKTRACE)
                                false, 
#else
                                true,
#endif
                                (int) ompi_mpi_abort_print_stack,
                                &value);
#if OPAL_WANT_PRETTY_PRINT_STACKTRACE && ! defined(__WINDOWS__) && defined(HAVE_BACKTRACE)
    /* Only take the value if we have stack trace capability */
    ompi_mpi_abort_print_stack = OPAL_INT_TO_BOOL(value);
#else
    /* If we do not have stack trace capability, ensure that this is
       hard-coded to false */
    ompi_mpi_abort_print_stack = false;
#endif

    value = mca_base_param_reg_int_name("mpi", "preconnect_mpi",
                                        "Whether to force MPI processes to fully "
                                        "wire-up the MPI connections between MPI "
                                        "processes during "
                                        "MPI_INIT (vs. making connections lazily -- "
                                        "upon the first MPI traffic between each "
                                        "process peer pair)",
                                        false, false, 0, NULL);
    mca_base_param_reg_syn_name(value, "mpi", "preconnect_all", true);
    
    /* Leave pinned parameter */

    mca_base_param_reg_int_name("mpi", "leave_pinned",
                                "Whether to use the \"leave pinned\" protocol or not.  Enabling this setting can help bandwidth performance when repeatedly sending and receiving large messages with the same buffers over RDMA-based networks (0 = do not use \"leave pinned\" protocol, 1 = use \"leave pinned\" protocol, -1 = allow network to choose at runtime).",
                                false, false,
                                ompi_mpi_leave_pinned, &value);
    ompi_mpi_leave_pinned = (value >= 1) ? true: false;

    mca_base_param_reg_int_name("mpi", "leave_pinned_pipeline",
                                "Whether to use the \"leave pinned pipeline\" protocol or not.",
                                false, false,
                                (int) ompi_mpi_leave_pinned_pipeline, &value);
    ompi_mpi_leave_pinned_pipeline = OPAL_INT_TO_BOOL(value);
    
    if (ompi_mpi_leave_pinned && ompi_mpi_leave_pinned_pipeline) {
        ompi_mpi_leave_pinned_pipeline = 0;
        orte_show_help("help-mpi-runtime.txt", 
                       "mpi-params:leave-pinned-and-pipeline-selected",
                       true);
    }

    mca_base_param_reg_int_name("mpi", "warn_on_fork",
                                "If nonzero, issue a warning if program forks under conditions that could cause system errors",
                                false, false, 
                                (int) true, &value);
    ompi_warn_on_fork = OPAL_INT_TO_BOOL(value);
    
    /* Sparse group storage support */

    mca_base_param_reg_int_name("mpi", "have_sparse_group_storage", 
                                "Whether this Open MPI installation supports storing of data in MPI groups in \"sparse\" formats (good for extremely large process count MPI jobs that create many communicators/groups)",
                                false, true, (int) OMPI_GROUP_SPARSE, NULL);
    mca_base_param_reg_int_name("mpi", "use_sparse_group_storage", 
                                "Whether to use \"sparse\" storage formats for MPI groups (only relevant if mpi_have_sparse_group_storage is 1)",
                                false, false, OMPI_GROUP_SPARSE, &value);
    ompi_use_sparse_group_storage = OPAL_INT_TO_BOOL(value);
    if (ompi_use_sparse_group_storage) {
        value = 0;
        if (OMPI_GROUP_SPARSE) {
            value = 1;
        }
        if (0 == value) {
            orte_show_help("help-mpi-runtime.txt", 
                           "sparse groups enabled but compiled out",
                           true);
            ompi_use_sparse_group_storage = false;
        }
    }

    /* Do we want notifier messages upon MPI_INIT and MPI_FINALIZE? */

    mca_base_param_reg_int_name("mpi", "notify_init_finalize",
                                "If nonzero, send two notifications during MPI_INIT: one near when MPI_INIT starts, and another right before MPI_INIT finishes, and send 2 notifications during MPI_FINALIZE: one right when MPI_FINALIZE starts, and another near when MPI_FINALIZE finishes.",
                                false, false, 
                                (int) ompi_notify_init_finalize, &value);
    ompi_notify_init_finalize = OPAL_INT_TO_BOOL(value);

    return OMPI_SUCCESS;
}

int ompi_show_all_mca_params(int32_t rank, int requested, char *nodename) {
    opal_list_t *info;
    opal_list_item_t *i;
    mca_base_param_info_t *item;
    char *value_string;
    int value_int;
    FILE *fp = NULL;
    time_t timestamp;
    mca_base_param_source_t source;
    char *src_file;
    char *src_string;
    
    if (rank != 0) {
        return OMPI_SUCCESS;
    }
    
    timestamp = time(NULL);
    
    /* Open the file if one is specified */
    if (0 != strlen(ompi_mpi_show_mca_params_file)) {
        if ( NULL == (fp = fopen(ompi_mpi_show_mca_params_file, "w")) ) {
            opal_output(0, "Unable to open file <%s> to write MCA parameters", ompi_mpi_show_mca_params_file);
            return OMPI_ERR_FILE_OPEN_FAILURE;
        }
        fprintf(fp, "#\n");
        fprintf(fp, "# This file was automatically generated on %s", ctime(&timestamp));
        fprintf(fp, "# by MPI_COMM_WORLD rank %d (out of a total of %d) on %s\n", rank, requested, nodename );
        fprintf(fp, "#\n");
    }
    
    mca_base_param_dump(&info, false);
    for (i =  opal_list_get_first(info); 
         i != opal_list_get_last(info);
         i =  opal_list_get_next(i)) {
        item = (mca_base_param_info_t*) i;

        /* If this is an internal param, don't print it */
        if (item->mbpp_internal) {
            continue;
        }
        
        /* get the source - where the param was last set */
        if (OPAL_SUCCESS != 
            mca_base_param_lookup_source(item->mbpp_index, &source, &src_file)) {
            continue;
        }
        
        /* is this a default value and we are not displaying
         * defaults, ignore this one
         */
        if (MCA_BASE_PARAM_SOURCE_DEFAULT == source && !show_default_mca_params) {
            continue;
        }
        
        /* is this a file value and we are not displaying files,
         * ignore it
         */
        if (MCA_BASE_PARAM_SOURCE_FILE == source && !show_file_mca_params) {
            continue;
        }
        
        /* is this an enviro value and we are not displaying enviros,
         * ignore it
         */
        if (MCA_BASE_PARAM_SOURCE_ENV == source && !show_enviro_mca_params) {
            continue;
        }
        
        /* is this an API value and we are not displaying APIs,
         * ignore it
         */
        if (MCA_BASE_PARAM_SOURCE_OVERRIDE == source && !show_override_mca_params) {
            continue;
        }
        
        /* Get the parameter name, and convert it to a printable string */
        if (MCA_BASE_PARAM_TYPE_STRING == item->mbpp_type) {
            mca_base_param_lookup_string(item->mbpp_index, &value_string);
            if (NULL == value_string) {
                value_string = strdup("");
            }
        } else {
            mca_base_param_lookup_int(item->mbpp_index, &value_int);
            asprintf(&value_string, "%d", value_int);
        }
        
        switch(source) {
            case MCA_BASE_PARAM_SOURCE_DEFAULT:
                src_string = "default value";
                break;
            case MCA_BASE_PARAM_SOURCE_ENV:
                src_string = "environment or cmdline";
                break;
            case MCA_BASE_PARAM_SOURCE_FILE:
                src_string = "file";
                break;
            case MCA_BASE_PARAM_SOURCE_OVERRIDE:
                src_string = "API override";
                break;
            default:
                src_string = NULL;
                break;
        }
        
        /* Print the parameter */
        if (0 != strlen(ompi_mpi_show_mca_params_file)) {
            if (NULL == src_file) {
                fprintf(fp, "%s=%s (%s)\n", item->mbpp_full_name, value_string,
                        (NULL != src_string ? src_string : "unknown"));
            } else {
                fprintf(fp, "%s=%s (%s:%s)\n", item->mbpp_full_name, value_string,
                        (NULL != src_string ? src_string : "unknown"), src_file);
            }
        } else {
            if (NULL == src_file) {
                opal_output(0, "%s=%s (%s)\n", item->mbpp_full_name, value_string,
                            (NULL != src_string ? src_string : "unknown"));
            } else {
                opal_output(0, "%s=%s (%s:%s)\n", item->mbpp_full_name, value_string,
                            (NULL != src_string ? src_string : "unknown"), src_file);
            }
        }
        
        free(value_string);
    }
    
    /* Close file, cleanup allocated memory*/
    if (0 != strlen(ompi_mpi_show_mca_params_file)) {
        fclose(fp);
    }
    mca_base_param_dump_release(info);
    
    return OMPI_SUCCESS;
}
