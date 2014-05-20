/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 * ORTE Restart Tool for restarting a previously checkpointed multiprocess job
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <stdio.h>
#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif  /*  HAVE_STDLIB_H */
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif  /* HAVE_SYS_STAT_H */
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif  /* HAVE_SYS_WAIT_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */

#include "opal/runtime/opal.h"
#include "opal/runtime/opal_cr.h"
#include "opal/util/cmd_line.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"
#include "opal/util/basename.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_cr.h"
#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"
#include "orte/mca/filem/base/base.h"
#include "opal/util/show_help.h"
#include "orte/util/proc_info.h"

/******************
 * Local Functions
 ******************/
static int initialize(int argc, char *argv[]);
static int finalize(void);
static int parse_args(int argc, char *argv[]);
static int check_file(orte_snapc_base_global_snapshot_t *snapshot);
static int create_appfile(orte_snapc_base_global_snapshot_t *snapshot);
static int spawn_children(orte_snapc_base_global_snapshot_t *snapshot, pid_t *child_pid);
static int snapshot_info(orte_snapc_base_global_snapshot_t *snapshot);
static int snapshot_sort_compare_fn(opal_list_item_t **a,
                                    opal_list_item_t **b);

/*****************************************
 * Global Vars for Command line Arguments
 *****************************************/
typedef struct {
    bool help;
    char *filename;
    char *appfile;
    bool verbose;
    bool forked;
    bool preload;
    int  seq_number;
    char *hostfile;
    int  output;
    bool info_only;
    bool app_only;
    bool showme;
    char *mpirun_opts;
} orte_restart_globals_t;

orte_restart_globals_t orte_restart_globals;

opal_cmd_line_init_t cmd_line_opts[] = {
    { NULL, NULL, NULL, 
      'h', NULL, "help", 
      0,
      &orte_restart_globals.help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },

    { NULL, NULL, NULL, 
      'v', NULL, "verbose", 
      0,
      &orte_restart_globals.verbose, OPAL_CMD_LINE_TYPE_BOOL,
      "Be Verbose" },

    { NULL, NULL, NULL, 
      'p', NULL, "preload", 
      0,
      &orte_restart_globals.preload, OPAL_CMD_LINE_TYPE_BOOL,
      "Preload the checkpoint files before restarting (Default = Disabled)" },

    { NULL, NULL, NULL, 
      '\0', NULL, "fork", 
      0,
      &orte_restart_globals.forked, OPAL_CMD_LINE_TYPE_BOOL,
      "Fork off a new process which is the restarted process instead of "
      "replacing orte_restart" },

    { NULL, NULL, NULL, 
      's', NULL, "seq", 
      1,
      &orte_restart_globals.seq_number, OPAL_CMD_LINE_TYPE_INT,
      "The sequence number of the checkpoint to start from. "
      "(Default: -1, or most recent)" },

    { NULL, NULL, NULL, 
      '\0', "hostfile", "hostfile", 
      1,
      &orte_restart_globals.hostfile, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a hostfile to use for launch" },

    { NULL, NULL, NULL, 
      '\0', "machinefile", "machinefile", 
      1,
      &orte_restart_globals.hostfile, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a hostfile to use for launch" },

    { NULL, NULL, NULL, 
      'i', NULL, "info", 
      0,
      &orte_restart_globals.info_only, OPAL_CMD_LINE_TYPE_BOOL,
      "Display information about the checkpoint" },

    { NULL, NULL, NULL, 
      'a', NULL, "apponly", 
      0,
      &orte_restart_globals.app_only, OPAL_CMD_LINE_TYPE_BOOL,
      "Only create the app context file, do not restart from it" },

    { NULL, NULL, NULL, 
      '\0', NULL, "showme", 
      0,
      &orte_restart_globals.showme, OPAL_CMD_LINE_TYPE_BOOL,
      "Display the full command line that would have been exec'ed." },

    { NULL, NULL, NULL, 
      '\0', "mpirun_opts", "mpirun_opts", 
      1,
      &orte_restart_globals.mpirun_opts, OPAL_CMD_LINE_TYPE_STRING,
      "Command line options to pass directly to mpirun (be sure to quote long strings, and escape internal quotes)" },

    /* End of list */
    { NULL, NULL, NULL, 
      '\0', NULL, NULL, 
      0,
      NULL, OPAL_CMD_LINE_TYPE_NULL,
      NULL }
};

int
main(int argc, char *argv[])
{
    int ret, exit_status = ORTE_SUCCESS;
    pid_t child_pid = 0;
    orte_snapc_base_global_snapshot_t *snapshot = NULL;
    char *tmp_str = NULL;

    /***************
     * Initialize
     ***************/
    if (ORTE_SUCCESS != (ret = initialize(argc, argv))) {
        exit_status = ret;
        goto cleanup;
    }

    snapshot = OBJ_NEW(orte_snapc_base_global_snapshot_t);
    snapshot->reference_name  = strdup(orte_restart_globals.filename);
    orte_snapc_base_get_global_snapshot_directory(&tmp_str, snapshot->reference_name);
    snapshot->local_location  = opal_dirname(tmp_str);
    free(tmp_str);
    tmp_str = NULL;

    /* 
     * Check for existence of the file
     */
    if( ORTE_SUCCESS != (ret = check_file(snapshot)) ) {
        opal_show_help("help-orte-restart.txt", "invalid_filename", true,
                       orte_restart_globals.filename);
        exit_status = ret;
        goto cleanup;
    }

    if(orte_restart_globals.info_only ) {
        if (ORTE_SUCCESS != (ret = snapshot_info(snapshot))) {
            exit_status = ret;
            goto cleanup;
        }
        exit_status = ORTE_SUCCESS;
        goto cleanup;
    }

    /******************************
     * Create the app file to use with mpirun/orterun
     ******************************/
    if( ORTE_SUCCESS != (ret = create_appfile(snapshot) ) ) {
        exit_status = ret;
        goto cleanup;
    }

    if( orte_restart_globals.app_only ) {
        printf("Created Appfile:\n\t%s\n", orte_restart_globals.appfile);
        exit_status = ORTE_SUCCESS;
        goto cleanup;
    }

    /******************************
     * Restart in this process [mpirun/orterun]
     ******************************/
    if( orte_restart_globals.verbose ) {
        opal_output_verbose(10, orte_restart_globals.output,
                            "Restarting from file (%s)",
                            orte_restart_globals.filename);
        
        if( orte_restart_globals.forked ) {
            opal_output_verbose(10, orte_restart_globals.output,
                                "\t Forking off a child");
        } else {
            opal_output_verbose(10, orte_restart_globals.output,
                                "\t Exec in self");
        }
    }

    if( ORTE_SUCCESS != (ret = spawn_children(snapshot, &child_pid)) ) {
        opal_show_help("help-orte-restart.txt", "restart_cmd_failure", true,
                       orte_restart_globals.filename, ret);
        exit_status = ret;
        goto cleanup;
    }

    opal_output_verbose(10, orte_restart_globals.output,
                        "orte_restart: Restarted Child with PID = %d\n", child_pid);

    /***************
     * Cleanup
     ***************/
 cleanup:
    if(NULL != snapshot ) {
        OBJ_RELEASE(snapshot);
        snapshot = NULL;
    }

    if (OPAL_SUCCESS != (ret = finalize())) {
        return ret;
    }

    return exit_status;
}

static int initialize(int argc, char *argv[]) {
    int ret, exit_status = ORTE_SUCCESS;
    char * tmp_env_var = NULL;

    /*
     * Make sure to init util before parse_args
     * to ensure installdirs is setup properly
     * before calling mca_base_open();
     */
    if( ORTE_SUCCESS != (ret = opal_init_util(&argc, &argv)) ) {
        return ret;
    }

    /*
     * Parse command line arguments
     */
    if (ORTE_SUCCESS != (ret = parse_args(argc, argv))) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Setup OPAL Output handle from the verbose argument
     */
    if( orte_restart_globals.verbose ) {
        orte_restart_globals.output = opal_output_open(NULL);
        opal_output_set_verbosity(orte_restart_globals.output, 10);
    } else {
        orte_restart_globals.output = 0; /* Default=STDERR */
    }

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

    /*
     * Setup any ORTE stuff we might need
     */
    if (OPAL_SUCCESS != (ret = orte_init(&argc, &argv, ORTE_PROC_TOOL))) {
        exit_status = ret;
        goto cleanup;
    }
    
    /* Unset these now that we no longer need them */
    tmp_env_var = mca_base_param_env_var("crs");
    opal_unsetenv(tmp_env_var, &environ);
    free(tmp_env_var);
    tmp_env_var = NULL;

    tmp_env_var = mca_base_param_env_var("opal_cr_is_tool");
    opal_unsetenv(tmp_env_var, &environ);
    free(tmp_env_var);
    tmp_env_var = NULL;

 cleanup:
    return exit_status;
}

static int finalize(void)
{
    int ret;

    if (OPAL_SUCCESS != (ret = orte_finalize())) {
        return ret;
    }

    return ORTE_SUCCESS;
}

static int parse_args(int argc, char *argv[])
{
    int i, ret, len;
    opal_cmd_line_t cmd_line;
    char **app_env = NULL, **global_env = NULL;
    char * tmp_env_var = NULL;
    orte_restart_globals_t tmp = { false, /* help */
                                   NULL,  /* filename */
                                   NULL,  /* appfile */
                                   false, /* verbose */
                                   false, /* forked */
                                   false, /* preload */
                                   -1,    /* seq_number */
                                   NULL,  /* hostfile */
                                   -1,    /* output*/
                                   false, /* info only */
                                   false, /* app only */
                                   false, /* showme */
                                   NULL}; /* mpirun_opts */

    orte_restart_globals = tmp;

    /* Parse the command line options */    
    opal_cmd_line_create(&cmd_line, cmd_line_opts);
    
    mca_base_open();
    mca_base_cmd_line_setup(&cmd_line);
    ret = opal_cmd_line_parse(&cmd_line, true, argc, argv);
    
    /** 
     * Put all of the MCA arguments in the environment 
     */
    mca_base_cmd_line_process_args(&cmd_line, &app_env, &global_env);
    
    len = opal_argv_count(app_env);
    for(i = 0; i < len; ++i) {
        putenv(app_env[i]);
    }

    len = opal_argv_count(global_env);
    for(i = 0; i < len; ++i) {
        putenv(global_env[i]);
    }
    
    tmp_env_var = mca_base_param_env_var("opal_cr_is_tool");
    opal_setenv(tmp_env_var,
                "1",
                true, &environ);
    free(tmp_env_var);
    tmp_env_var = NULL;

    /**
     * Now start parsing our specific arguments
     */

#if OPAL_ENABLE_FT_CR == 0
    /* Warn and exit if not configured with Checkpoint/Restart */
    {
        char *args = NULL;
        args = opal_cmd_line_get_usage_msg(&cmd_line);
        opal_show_help("help-orte-restart.txt", "usage-no-cr",
                       true, args);
        free(args);
        return ORTE_ERROR;
    }
#endif

    if (OPAL_SUCCESS != ret || 
        orte_restart_globals.help ||
        1 >= argc) {
        char *args = NULL;
        args = opal_cmd_line_get_usage_msg(&cmd_line);
        opal_show_help("help-orte-restart.txt", "usage", true,
                       args);
        free(args);
        return ORTE_ERROR;
    }

    /* get the remaining bits */
    opal_cmd_line_get_tail(&cmd_line, &argc, &argv);
    if ( 1 > argc ) {
        char *args = NULL;
        args = opal_cmd_line_get_usage_msg(&cmd_line);
        opal_show_help("help-orte-restart.txt", "usage", true,
                       args);
        free(args);
        return ORTE_ERROR;
    }

    orte_restart_globals.filename = strdup(argv[0]);
    if ( NULL == orte_restart_globals.filename || 
         0 >= strlen(orte_restart_globals.filename) ) {
        opal_show_help("help-orte-restart.txt", "invalid_filename", true,
                       orte_restart_globals.filename);
        return ORTE_ERROR;
    }

    /* If we have arguments after the command, then assume they
     * need to be grouped together.
     */
    if(argc > 1) {
        orte_restart_globals.filename = strdup(opal_argv_join(argv, ' '));
    }
    
    return ORTE_SUCCESS;
}

static int check_file(orte_snapc_base_global_snapshot_t *snapshot)
{
    int ret, exit_status = ORTE_SUCCESS;

    opal_output_verbose(10, orte_restart_globals.output,
                        "Checking for the existence of (%s)\n",
                        snapshot->local_location);

    if (0 >  (ret = access(snapshot->local_location, F_OK)) ) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

static int create_appfile(orte_snapc_base_global_snapshot_t *snapshot)
{
    int ret, exit_status = ORTE_SUCCESS;
    FILE *appfile = NULL;
    opal_list_item_t* item = NULL;

    /*
     * Extract the record information for the specified seq number.
     * Note: If the seq # passed is -1, then the largest seq # is selected,
     *       ow the seq # requested is selected if available
     */
    snapshot->seq_num = orte_restart_globals.seq_number;
    if( ORTE_SUCCESS != (ret = orte_snapc_base_extract_metadata( snapshot ) ) ) {
        opal_show_help("help-orte-restart.txt", "invalid_seq_num", true,
                       orte_restart_globals.filename,
                       (int)orte_restart_globals.seq_number);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Create the appfile
     */
    asprintf(&orte_restart_globals.appfile, "%s/%s",
             snapshot->local_location,
             strdup("restart-appfile"));

    if (NULL == (appfile = fopen(orte_restart_globals.appfile, "w")) ) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Sort the snapshots so that they are in order
     */
    opal_list_sort(&snapshot->local_snapshots, snapshot_sort_compare_fn);

    /*
     * Construct the appfile
     */
    for(item  = opal_list_get_first(&snapshot->local_snapshots);
        item != opal_list_get_end(&snapshot->local_snapshots);
        item  = opal_list_get_next(item) ) {
        orte_snapc_base_local_snapshot_t *vpid_snapshot;
        vpid_snapshot = (orte_snapc_base_local_snapshot_t*)item;
        
        fprintf(appfile, "#\n");
        fprintf(appfile, "# Old Process Name: %u.%u\n", 
                vpid_snapshot->process_name.jobid,
                vpid_snapshot->process_name.vpid);
        fprintf(appfile, "#\n");
        fprintf(appfile, "-np 1 ");
        if(orte_restart_globals.preload) {
            fprintf(appfile, "--preload-files %s/%s ", 
                    vpid_snapshot->local_location, 
                    vpid_snapshot->reference_name);
            fprintf(appfile, "--preload-files-dest-dir . ");
        }
        /* JJH: Make this match what the user originally specified on the command line */
        fprintf(appfile, "-am ft-enable-cr ");

        fprintf(appfile, " opal-restart ");

        /* JJH: Make sure this changes if ever the default location of the local file is changed,
         * currently it is safe to assume that it is in the current working directory.
         *
         * JJH: If we allow inplace restarting then this may be another directory... */
        if(orte_restart_globals.preload) {
            /* If we preloaded the files then they are in the current working
             * directory. */
            fprintf(appfile, "-mca crs_base_snapshot_dir . ");
        }
        else {
            /* If we are *not* preloading the files, the point to the original checkpoint
             * directory to access the checkpoint files. */
            fprintf(appfile, "-mca crs_base_snapshot_dir %s ", vpid_snapshot->local_location);
        }
        fprintf(appfile, "%s\n", vpid_snapshot->reference_name);
    }

 cleanup:
    if(NULL != appfile)
        fclose(appfile);
    
    return exit_status;
}

static int spawn_children(orte_snapc_base_global_snapshot_t *snapshot, pid_t *child_pid)
{
    int ret, exit_status = ORTE_SUCCESS;
    char **argv = NULL;
    int argc = 0;
    int status;
    int i;

    if( ORTE_SUCCESS != (ret = opal_argv_append(&argc, &argv, "mpirun")) ) {
        exit_status = ret;
        goto cleanup;
    }
    if( ORTE_SUCCESS != (ret = opal_argv_append(&argc, &argv, "-am")) ) {
        exit_status = ret;
        goto cleanup;
    }
    if( ORTE_SUCCESS != (ret = opal_argv_append(&argc, &argv, "ft-enable-cr")) ) {
        exit_status = ret;
        goto cleanup;
    }
    if( NULL != orte_restart_globals.hostfile ) {
        if( ORTE_SUCCESS != (ret = opal_argv_append(&argc, &argv, "--default-hostfile")) ) {
            exit_status = ret;
            goto cleanup;
        }
        if( ORTE_SUCCESS != (ret = opal_argv_append(&argc, &argv, orte_restart_globals.hostfile)) ) {
            exit_status = ret;
            goto cleanup;
        }
    }
    if( orte_restart_globals.mpirun_opts ) {
        if( ORTE_SUCCESS != (ret = opal_argv_append(&argc, &argv, orte_restart_globals.mpirun_opts)) ) {
            exit_status = ret;
            goto cleanup;
        }
    }
    if( ORTE_SUCCESS != (ret = opal_argv_append(&argc, &argv, "--app")) ) {
        exit_status = ret;
        goto cleanup;
    }
    if( ORTE_SUCCESS != (ret = opal_argv_append(&argc, &argv, orte_restart_globals.appfile)) ) {
        exit_status = ret;
        goto cleanup;
    }

    if( orte_restart_globals.showme ) {
        for(i = 0; i < argc; ++i ) {
            /*printf("%2d: (%s)\n", i, argv[i]);*/
            printf("%s ", argv[i]);
        }
        printf("\n");
        return ORTE_SUCCESS;
    }

    /* To fork off a child */
    if( orte_restart_globals.forked ) {
        *child_pid = fork();
        
        if( 0 == *child_pid) {
            /* Child Process */
            status = execvp(strdup(argv[0]), argv);
            if( 0 > status) {
                opal_output(orte_restart_globals.output,
                            "orte_restart: execv failed with status = %d\n",
                            status);
            }
            exit_status = status;
            goto cleanup;
        }
        else if(0 < *child_pid) {
            /* Parent is done once it is started */
            ;
        }
        else {
            opal_output(orte_restart_globals.output,
                        "orte_restart: fork failed: This should never happen!");
            /* Fork failed :( */
            exit_status = *child_pid;
            goto cleanup;
        }
    }
    /* ... or not to fork off a child */
    else {
        /* Make sure to finalize so we don't leave our session directory */
        orte_finalize();

        status = execvp(strdup(argv[0]), argv);
        if( 0 > status) {
            /* execv failed */
        }
        exit_status = status;
        goto cleanup;
    }

 cleanup:
    if( NULL != argv)
        opal_argv_free(argv);

    return exit_status;
}

int snapshot_info(orte_snapc_base_global_snapshot_t *snapshot)
{
    int ret, exit_status = ORTE_SUCCESS;
    int num_seqs, processes, i;
    int *snapshot_ref_seqs;
    opal_list_item_t* item = NULL;
    orte_snapc_base_local_snapshot_t *vpid_snapshot;
    
    if (orte_restart_globals.seq_number == -1) {
        if( ORTE_SUCCESS != (ret = orte_snapc_base_get_all_snapshot_ref_seqs(NULL, orte_restart_globals.filename, &num_seqs, &snapshot_ref_seqs) ) ) {
            exit_status = ret;
            goto cleanup;
        }
        opal_output(orte_restart_globals.output,
                    "Sequences: %d\n",
                    num_seqs);
    } else {
        num_seqs = 1;
        snapshot_ref_seqs = &orte_restart_globals.seq_number;
    }

    for (i=0; i<num_seqs; i++) {
        snapshot->seq_num = snapshot_ref_seqs[i];

        while (NULL != (item = opal_list_remove_first(&snapshot->local_snapshots))) {
            OBJ_RELEASE(item);
        }

        if( NULL != snapshot->start_time ) {
            free( snapshot->start_time );
            snapshot->start_time = NULL;
        }

        if( NULL != snapshot->end_time ) {
            free( snapshot->end_time );
            snapshot->end_time = NULL;
        }

        opal_output(orte_restart_globals.output,
                    "Seq: %d\n",
                    snapshot->seq_num);

        if( ORTE_SUCCESS != (ret = orte_snapc_base_extract_metadata( snapshot ) ) ) {
            exit_status = ret;
            goto cleanup;
        }

        item  = opal_list_get_first(&snapshot->local_snapshots);
        vpid_snapshot = (orte_snapc_base_local_snapshot_t*)item;

        if (NULL != snapshot->start_time ) {
            opal_output(orte_restart_globals.output,
                        "Begin Timestamp: %s\n",
                        snapshot->start_time);
        }

        if (NULL != vpid_snapshot->opal_crs ) {
            opal_output(orte_restart_globals.output,
                        "OPAL CRS Component: %s\n",
                        vpid_snapshot->opal_crs);
        }

        if (NULL != snapshot->reference_name) {
            opal_output(orte_restart_globals.output,
                        "Snapshot Reference: %s\n",
                        snapshot->reference_name);
        }

        if (NULL != snapshot->local_location) {
            opal_output(orte_restart_globals.output,
                        "Snapshot Location: %s\n",
                        snapshot->local_location);
        }

        if (NULL != snapshot->end_time ) {
            opal_output(orte_restart_globals.output,
                        "End Timestamp: %s\n",
                        snapshot->end_time);
        }

        processes = 0;
        for(item  = opal_list_get_first(&snapshot->local_snapshots);
            item != opal_list_get_end(&snapshot->local_snapshots);
            item  = opal_list_get_next(item) ) {
            processes++;
        }
        opal_output(orte_restart_globals.output,
                    "Processes: %d\n",
                    processes);

        for(item  = opal_list_get_first(&snapshot->local_snapshots);
            item != opal_list_get_end(&snapshot->local_snapshots);
            item  = opal_list_get_next(item) ) {
            vpid_snapshot = (orte_snapc_base_local_snapshot_t*)item;

            opal_output_verbose(10, orte_restart_globals.output,
                                "Process: %u.%u",
                                vpid_snapshot->process_name.jobid,
                                vpid_snapshot->process_name.vpid);
        }
    }

 cleanup:
    return exit_status;
}

static int snapshot_sort_compare_fn(opal_list_item_t **a,
                                    opal_list_item_t **b)
{
    orte_snapc_base_local_snapshot_t *snap_a, *snap_b;

    snap_a = (orte_snapc_base_local_snapshot_t*)(*a);
    snap_b = (orte_snapc_base_local_snapshot_t*)(*b);

    if( snap_a->process_name.vpid > snap_b->process_name.vpid ) {
        return 1;
    }
    else if( snap_a->process_name.vpid == snap_b->process_name.vpid ) {
        return 0;
    }
    else {
        return -1;
    }
}
