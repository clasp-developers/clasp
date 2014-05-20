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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 * OPAL Checkpoint command
 *
 * This command will initiate the checkpoint of a single 
 * process that has been compiled with OPAL support.
 */
#include "opal_config.h"

#include <stdio.h>
#include <errno.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif  /*  HAVE_STDLIB_H */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif  /* HAVE_FCNTL_H */
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>  /* for mkfifo */
#endif  /* HAVE_SYS_STAT_H */
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#include "opal/constants.h"

#include "opal/util/cmd_line.h"
#include "opal/util/argv.h"
#include "opal/util/show_help.h"
#include "opal/util/opal_environ.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "opal/runtime/opal.h"
#include "opal/runtime/opal_cr.h"

#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

/******************
 * Global Vars
 ******************/

/******************
 * Local Functions
 ******************/
static int initialize(int argc, char *argv[]);
static int finalize(void);
static int parse_args(int argc, char *argv[]);
static int notify_process_for_checkpoint(pid_t pid, char **fname, int term, 
                                         opal_crs_state_type_t *state);

/*****************************************
 * Global Vars for Command line Arguments
 *****************************************/
typedef struct {
    bool help;
    int pid;
    bool term;
    bool verbose;
    bool quiet;
    char *snapshot_name;
    char *snapshot_loc;
    int output;
} opal_checkpoint_globals_t;

opal_checkpoint_globals_t opal_checkpoint_globals;

opal_cmd_line_init_t cmd_line_opts[] = {
    { NULL, NULL, NULL, 
      'h', NULL, "help", 
      0,
      &opal_checkpoint_globals.help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },

    { NULL, NULL, NULL, 
      'v', NULL, "verbose", 
      0,
      &opal_checkpoint_globals.verbose, OPAL_CMD_LINE_TYPE_BOOL,
      "Be Verbose" },

    { NULL, NULL, NULL, 
      'q', NULL, "quiet", 
      0,
      &opal_checkpoint_globals.quiet, OPAL_CMD_LINE_TYPE_BOOL,
      "Be Super Quiet" },

    { NULL, NULL, NULL, 
      '\0', NULL, "term", 
      0,
      &opal_checkpoint_globals.term, OPAL_CMD_LINE_TYPE_BOOL,
      "Terminate the application after checkpoint" },

    { NULL, NULL, NULL, 
      'n', NULL, "name",
      1,
      &opal_checkpoint_globals.snapshot_name, OPAL_CMD_LINE_TYPE_STRING,
      "Request a specific snapshot reference." },

    { "crs", "base", "snapshot_dir", 
      'w', NULL, "where", 
      1,
      &opal_checkpoint_globals.snapshot_loc, OPAL_CMD_LINE_TYPE_STRING,
      "Where to place the checkpoint files. Note: You must remember this "
      "location to pass into opal-restart, as it may not be able to find "
      "the desired directory." },

    /* End of list */
    { NULL, NULL, NULL, '\0', NULL, NULL, 0,
      NULL, OPAL_CMD_LINE_TYPE_NULL,
      NULL }
};

int
main(int argc, char *argv[])
{
    int ret, exit_status = OPAL_SUCCESS;
    char *fname = NULL;
    opal_crs_state_type_t cr_state;

    /***************
     * Initialize
     ***************/
    if (OPAL_SUCCESS != (ret = initialize(argc, argv))) {
        exit_status = ret;
        goto cleanup;
    }

    /*******************************
     * Checkpoint the requested PID
     *******************************/
    opal_output_verbose(10, opal_checkpoint_globals.output,
                        "opal_checkpoint: Checkpointing PID %d",
                        opal_checkpoint_globals.pid);
    if( opal_checkpoint_globals.term ) {
        opal_output_verbose(10, opal_checkpoint_globals.output,
                            "\tTerminating application after checkpoint");
    }

    ret = notify_process_for_checkpoint(opal_checkpoint_globals.pid,
                                        &fname,
                                        opal_checkpoint_globals.term,
                                        &cr_state);
    if (OPAL_SUCCESS != ret ||
        cr_state == OPAL_CRS_ERROR) {
        opal_show_help("help-opal-checkpoint.txt", "ckpt_failure", true,
                       opal_checkpoint_globals.pid, ret, cr_state);
        exit_status = ret;
        goto cleanup;
    }

    if( !opal_checkpoint_globals.quiet ) {
        opal_output(opal_checkpoint_globals.output,
                    "Local Snapshot Reference = %s\n",
                    fname);
    }

 cleanup:
    /***************
     * Cleanup
     ***************/
    if (OPAL_SUCCESS != (ret = finalize())) {
        return ret;
    }

    return exit_status;
}

static int initialize(int argc, char *argv[]) {
    int ret, exit_status = OPAL_SUCCESS;
    char * tmp_env_var = NULL;

    /*
     * Make sure to init util before parse_args
     * to ensure installdirs is setup properly
     * before calling mca_base_open();
     */
    if( OPAL_SUCCESS != (ret = opal_init_util(&argc, &argv)) ) {
        return ret;
    }

    /*
     * Parse Command Line Arguments
     */
    if (OPAL_SUCCESS != (ret = parse_args(argc, argv))) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Setup OPAL Output handle from the verbose argument
     */
    if( opal_checkpoint_globals.verbose ) {
        opal_checkpoint_globals.quiet = false; /* Automaticly turn off quiet if it is set */
        opal_checkpoint_globals.output = opal_output_open(NULL);
        opal_output_set_verbosity(opal_checkpoint_globals.output, 10);
    } else {
        opal_checkpoint_globals.output = 0; /* Default=STDOUT */
    }

    /* 
     * Disable the checkpoint notification routine for this
     * tool. As we will never need to checkpoint this tool.
     * Note: This must happen before opal_init().
     */
    opal_cr_set_enabled(false);

    /* 
     * Select the 'none' CRS component, 
     * since we don't actually use a checkpointer
     */
    tmp_env_var = mca_base_param_env_var("crs");
    opal_setenv(tmp_env_var,
                "none",
                true, &environ);
    free(tmp_env_var);
    tmp_env_var = NULL;

    /*
     * Initialize OPAL
     */
    if (OPAL_SUCCESS != (ret = opal_init(&argc, &argv))) {
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

static int finalize(void) {
    int ret = OPAL_SUCCESS;

    if (OPAL_SUCCESS != (ret = opal_finalize())) {
        return ret;
    }

    return OPAL_SUCCESS;
}

static int parse_args(int argc, char *argv[]) {
    int i, ret, len;
    opal_cmd_line_t cmd_line;
    char **app_env = NULL, **global_env = NULL;
    char * tmp_env_var = NULL;

    memset(&opal_checkpoint_globals, 0, sizeof(opal_checkpoint_globals_t));

    opal_checkpoint_globals.snapshot_name = NULL;
    opal_checkpoint_globals.snapshot_loc  = NULL;
    
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
    if (OPAL_SUCCESS != ret || 
        opal_checkpoint_globals.help ||
        1 >= argc) {
        char *args = NULL;
        args = opal_cmd_line_get_usage_msg(&cmd_line);
        opal_show_help("help-opal-checkpoint.txt", "usage", true,
                       args);
        free(args);
        return OPAL_ERROR;
    }

    if( NULL == opal_checkpoint_globals.snapshot_name )
        opal_checkpoint_globals.snapshot_name = strdup("");
    if( NULL == opal_checkpoint_globals.snapshot_loc ) {
        opal_checkpoint_globals.snapshot_loc = strdup("");
    }

    /* get the remaining bits */
    opal_cmd_line_get_tail(&cmd_line, &argc, &argv);

    opal_checkpoint_globals.pid = atoi(argv[0]);
    if ( 0 >= opal_checkpoint_globals.pid ) {
        opal_show_help("help-opal-checkpoint.txt", "invalid_pid", true,
                       opal_checkpoint_globals.pid);
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static int 
notify_process_for_checkpoint(pid_t pid, char **fname, int term, opal_crs_state_type_t *cr_state)
{
    char *prog_named_pipe_r = NULL, *prog_named_pipe_w = NULL;
    int   prog_named_read_pipe_fd = -1, prog_named_write_pipe_fd = -1;
    char *loc_fname = NULL, *tmp_pid = NULL;
    unsigned char cmd;
    int len, ret;
    int exit_status = OPAL_SUCCESS;
    int s, max_wait_time = 20; /* wait time before giving up on the checkpoint */
    ssize_t tmp_size = 0;
    int value;

    /* A string copy of the pid */
    asprintf(&tmp_pid, "%d", pid);

    /* Flip the read/write files for bi-directionality */
    asprintf(&prog_named_pipe_w, "%s/%s.%s", opal_cr_pipe_dir, OPAL_CR_NAMED_PROG_R, tmp_pid);
    asprintf(&prog_named_pipe_r, "%s/%s.%s", opal_cr_pipe_dir, OPAL_CR_NAMED_PROG_W, tmp_pid);

    /*
     * Signal the application telling it that we wish to checkpoint
     */
    if( 0 != (ret = kill(pid, opal_cr_entry_point_signal) ) ) {
        exit_status = ret;
        goto cleanup;
    }

    opal_output_verbose(10, opal_checkpoint_globals.output,
                        "opal_checkpoint: Looking for Named Pipes (%s) (%s)\n",
                        prog_named_pipe_r, prog_named_pipe_w);

    for( s = 0; s < max_wait_time; ++s) {
        /*
         * See if the named pipe exists yet for the PID in question
         */
        if( 0 > (ret = access(prog_named_pipe_r, F_OK) )) {
            /* File doesn't exist yet, keep waiting */
            if( !opal_checkpoint_globals.quiet &&
                s >= max_wait_time - 5 ) {
                opal_output(0, "opal-checkpoint: File does not exist yet: <%s> rtn = %d (waited %d/%d sec)\n",
                            prog_named_pipe_r, ret, s, max_wait_time);
            }
            sleep(1);
            continue;
        }
        else if( 0 > (ret = access(prog_named_pipe_w, F_OK) )) {
            /* File doesn't exist yet, keep waiting */
            if( !opal_checkpoint_globals.quiet &&
                s >= max_wait_time - 5 ) {
                opal_output(0, "opal-checkpoint: File does not exist yet: <%s> rtn = %d (waited %d/%d sec)\n",
                            prog_named_pipe_w, ret, s, max_wait_time);
            }
            sleep(1);
            continue;
        }
        else {
            break;
        }
    }
    if( s == max_wait_time ) { 
        /* The file doesn't exist, 
         * This means that the process didn't open up a named pipe for us
         * to access their checkpoint notification routine. Therefore,
         * the application either:
         *  - Doesn't exist
         *  - Isn't checkpointable
         * In either case there is nothing we can do.
         */
        opal_show_help("help-opal-checkpoint.txt", "pid_does_not_exist", true,
                       opal_checkpoint_globals.pid, prog_named_pipe_r, prog_named_pipe_w);
        
        *cr_state = OPAL_CRS_ERROR;
        
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /* The file does exist, so let's use it */

    /*
     * Open 
     *  - prog_named_write_pipe:
     *    prog makes this file and opens Read Only
     *    this app. opens it Write Only
     *  - prog_named_read_pipe:
     *    prog makes this file and opens Write Only
     *    this app. opens it Read Only
     */
    prog_named_write_pipe_fd = open(prog_named_pipe_w, O_WRONLY);
    if(prog_named_write_pipe_fd < 0) {
        opal_output(opal_checkpoint_globals.output,
                    "opal_checkpoint: Error: Unable to open name pipe (%s). %d\n", 
                    prog_named_pipe_w, prog_named_write_pipe_fd);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    prog_named_read_pipe_fd = open(prog_named_pipe_r, O_RDWR);
    if(prog_named_read_pipe_fd < 0) {
        opal_output(opal_checkpoint_globals.output,
                    "opal_checkpoint: Error: Unable to open name pipe (%s). %d\n",
                    prog_named_pipe_r, prog_named_read_pipe_fd);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /*
     * Start the handshake
     */
    len = 0;
    if( sizeof(int) != (ret = write(prog_named_write_pipe_fd, &len, sizeof(int))) ) {
        opal_output(opal_checkpoint_globals.output,
                    "opal_checkpoint: Error: Unable to write handshake to named pipe (%s). %d\n", 
                    prog_named_pipe_w, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    if( sizeof(int) != (ret = read(prog_named_read_pipe_fd, &value, sizeof(int))) ) {
        opal_output(opal_checkpoint_globals.output,
                    "opal_checkpoint: Error: Unable to read length from named pipe (%s). %d\n", 
                    prog_named_pipe_r, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /* Check the response to make sure we can checkpoint this process */
    if( OPAL_CHECKPOINT_CMD_IN_PROGRESS == value ) {
        opal_show_help("help-opal-checkpoint.txt",
                       "ckpt:in_progress", 
                       true,
                       opal_checkpoint_globals.pid);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }
    else if( OPAL_CHECKPOINT_CMD_NULL == value ) {
        opal_show_help("help-opal-checkpoint.txt",
                       "ckpt:req_null", 
                       true,
                       opal_checkpoint_globals.pid);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }
    else if ( OPAL_CHECKPOINT_CMD_ERROR == value ) {
        opal_show_help("help-opal-checkpoint.txt",
                       "ckpt:req_error", 
                       true,
                       opal_checkpoint_globals.pid);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /*
     * Write the checkpoint request and information to the 
     *  pipe
     */
    cmd = OPAL_CR_CHECKPOINT;
    /* Send the command */
    if( sizeof(cmd) != (ret = write(prog_named_write_pipe_fd, &cmd, sizeof(cmd))) ) {
        opal_output(opal_checkpoint_globals.output,
                    "opal_checkpoint: Error: Unable to write CHECKPOINT Command to named pipe (%s). %d\n", 
                    prog_named_pipe_w, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /* Send the arguments: {pid, term} */
    if( sizeof(int) != (ret = write(prog_named_write_pipe_fd, &pid, sizeof(int))) ) {
        opal_output(opal_checkpoint_globals.output,
                    "opal_checkpoint: Error: Unable to write pid (%d) to named pipe (%s). %d\n", 
                    pid, prog_named_pipe_w, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    if( sizeof(int) != (ret = write(prog_named_write_pipe_fd, &term, sizeof(int))) ) {
        opal_output(opal_checkpoint_globals.output,
                    "opal_checkpoint: Error: Unable to write term (%d) to named pipe (%s), %d\n", 
                    term, prog_named_pipe_w, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /* Send the snapshot_name argument */
    len = strlen(opal_checkpoint_globals.snapshot_name) + 1;
    if( sizeof(int) != (ret = write(prog_named_write_pipe_fd, &len, sizeof(int))) ) {
        opal_output(opal_checkpoint_globals.output,
                    "opal_checkpoint: Error: Unable to write snapshot name len (%d) to named pipe (%s). %d\n", 
                    len, prog_named_pipe_w, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    tmp_size = sizeof(char) * len;
    if( tmp_size != (ret = write(prog_named_write_pipe_fd, (opal_checkpoint_globals.snapshot_name), (sizeof(char) * len))) ) {
        opal_output(opal_checkpoint_globals.output,
                    "opal_checkpoint: Error: Unable to write snapshot name (%s) to named pipe (%s). %d\n", 
                    opal_checkpoint_globals.snapshot_name, prog_named_pipe_w, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /* Send the snashot location argument */
    len = strlen(opal_checkpoint_globals.snapshot_loc) + 1;
    if( sizeof(int) != (ret = write(prog_named_write_pipe_fd, &len, sizeof(int))) ) {
        opal_output(opal_checkpoint_globals.output,
                    "opal_checkpoint: Error: Unable to write snapshot location len (%d) to named pipe (%s). %d\n", 
                    len, prog_named_pipe_w, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    tmp_size = sizeof(char) * len;
    if( tmp_size != (ret = write(prog_named_write_pipe_fd, (opal_checkpoint_globals.snapshot_loc), (sizeof(char) * len))) ) {
        opal_output(opal_checkpoint_globals.output,
                    "opal_checkpoint: Error: Unable to write snapshot location (%s) to named pipe (%s). %d\n", 
                    opal_checkpoint_globals.snapshot_loc, prog_named_pipe_w, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /*
     * Get the response from the notification routine on the other
     *  machine.
     */
    if( sizeof(int) != (ret = read(prog_named_read_pipe_fd, &len, sizeof(int))) ) {
        opal_output(opal_checkpoint_globals.output,
                    "opal_checkpoint: Error: Unable to read length from named pipe (%s). %d\n", 
                    prog_named_pipe_r, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    if(len > 0) {
        loc_fname = (char *) malloc(sizeof(char) * len);
        if( (ssize_t)(sizeof(char) * len) != (ret = read(prog_named_read_pipe_fd, loc_fname, (sizeof(char) * len))) ) {
            opal_output(opal_checkpoint_globals.output,
                        "opal_checkpoint: Error: Unable to read filename from named pipe (%s). %d\n", 
                        prog_named_pipe_w, ret);
            exit_status = OPAL_ERROR;
            goto cleanup;
        }
    }

    *fname = strdup(loc_fname);
    if( sizeof(int) != (ret = read(prog_named_read_pipe_fd, &cr_state, sizeof(int))) ) {
        opal_output(opal_checkpoint_globals.output,
                    "opal_checkpoint: Error: Unable to read state from named pipe (%s). %d\n",
                    prog_named_pipe_r, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

 cleanup:    
    /*
     * Close the pipes now that we are done with it
     */
    close(prog_named_write_pipe_fd);
    close(prog_named_read_pipe_fd);

    if( NULL != tmp_pid) 
        free(tmp_pid);
    if( NULL != prog_named_pipe_r) 
        free(prog_named_pipe_r);
    if( NULL != prog_named_pipe_w) 
        free(prog_named_pipe_w);

    return exit_status;
}
