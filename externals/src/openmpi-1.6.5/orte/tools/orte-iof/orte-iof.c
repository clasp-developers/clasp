/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @fie
 * ORTE PS command
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
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif  /*  HAVE_SIGNAL_H */
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
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif  /* HAVE_DIRENT_H */

#include "opal/event/event.h"
#include "opal/util/cmd_line.h"
#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/runtime/opal.h"

#include "orte/runtime/runtime.h"
#include "orte/util/hnp_contact.h"
#include "orte/util/show_help.h"
#include "orte/util/parse_options.h"
#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/iof/iof.h"


/*****************************************
 * Global Vars for Command line Arguments
 *****************************************/
static struct {
    bool help;
    int hnppid;
    char *ranks;
    bool stdout_req;
    bool stderr_req;
    bool stddiag_req;
    bool tag;
    orte_hnp_contact_t *target_hnp;
} my_globals;

opal_cmd_line_init_t cmd_line_opts[] = {
    { NULL, NULL, NULL, 
      'h', NULL, "help", 
      0,
      &my_globals.help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },

    { NULL, NULL, NULL, 
      '\0', "pid", "pid", 
      1,
     &my_globals.hnppid, OPAL_CMD_LINE_TYPE_INT,
      "The pid of the mpirun whose output you wish to see" },

    { NULL, NULL, NULL, 
      '\0', "stdout", "stdout", 
      0,
      &my_globals.stdout_req, OPAL_CMD_LINE_TYPE_BOOL,
      "Display stdout from specified process (default)" },
    
    { NULL, NULL, NULL, 
      '\0', "stderr", "stderr", 
      0,
      &my_globals.stderr_req, OPAL_CMD_LINE_TYPE_BOOL,
      "Display stderr from specified process" },
    
    { NULL, NULL, NULL, 
      '\0', "stddiag", "stddiag", 
      0,
      &my_globals.stddiag_req, OPAL_CMD_LINE_TYPE_BOOL,
      "Display stddiag from specified process" },
    
    { NULL, NULL, NULL, 
      '\0', "ranks", "ranks", 
      1,
      &my_globals.ranks, OPAL_CMD_LINE_TYPE_STRING,
     "Ranks whose output is to be displayed (Comma separated list, each element can contain range)" },
    
    { "orte", "tag", "output", 
     '\0', "tag-output", "tag-output", 
     0,
     NULL, OPAL_CMD_LINE_TYPE_BOOL,
     "Tag output with the stream and [job,rank] (default: no tags)" },
    
    /* End of list */
    { NULL, NULL, NULL, 
      '\0', NULL, NULL, 
      0,
      NULL, OPAL_CMD_LINE_TYPE_NULL,
      NULL }
};

/*
 * Local variables & functions
 */
static void abort_exit_callback(int fd, short flags, void *arg);
static struct opal_event term_handler;
static struct opal_event int_handler;
static opal_list_t hnp_list;
static orte_process_name_t target_proc;


int
main(int argc, char *argv[])
{
    int ret, i;
    opal_cmd_line_t cmd_line;
    opal_list_item_t* item = NULL;
    orte_iof_tag_t stream;
    char **ranks=NULL;
    
    /***************
     * Initialize
     ***************/

    /*
     * Make sure to init util before parse_args
     * to ensure installdirs is setup properly
     * before calling mca_base_open();
     */
    if( ORTE_SUCCESS != (ret = opal_init_util(&argc, &argv)) ) {
        return ret;
    }
    
    /* initialize the globals */
    my_globals.help = false;
    my_globals.hnppid = -1;
    my_globals.stdout_req = false;
    my_globals.stderr_req = false;
    my_globals.stddiag_req = false;
    my_globals.ranks = "0";

    /* Parse the command line options */
    opal_cmd_line_create(&cmd_line, cmd_line_opts);
    
    mca_base_open();
    mca_base_cmd_line_setup(&cmd_line);
    ret = opal_cmd_line_parse(&cmd_line, false, argc, argv);
    
    /**
     * Now start parsing our specific arguments
     */
    if (OPAL_SUCCESS != ret || my_globals.help) {
        char *args = NULL;
        args = opal_cmd_line_get_usage_msg(&cmd_line);
        orte_show_help("help-orte-iof.txt", "usage", true, args);
        free(args);
        return ORTE_ERROR;
    }
    
    /*
     * Must specify the mpirun pid
     */
    if(my_globals.hnppid < 0) {
        orte_show_help("help-orte-iof.txt", "pid-required", true);
        return ORTE_ERROR;
    }
    
    /***************************
     * We need all of OPAL and the TOOL portion of ORTE
     ***************************/
    if (ORTE_SUCCESS != orte_init(&argc, &argv, ORTE_PROC_TOOL)) {
        orte_finalize();
        return 1;
    }

    /** setup callbacks for abort signals - from this point
     * forward, we need to abort in a manner that allows us
     * to cleanup
     */
    opal_signal_set(&term_handler, SIGTERM,
                    abort_exit_callback, &term_handler);
    opal_signal_add(&term_handler, NULL);
    opal_signal_set(&int_handler, SIGINT,
                    abort_exit_callback, &int_handler);
    opal_signal_add(&int_handler, NULL);

    /*
     * Get the list of available hnp's and setup contact info
     * to them in the RML
     */
    OBJ_CONSTRUCT(&hnp_list, opal_list_t);
    if (ORTE_SUCCESS != (ret = orte_list_local_hnps(&hnp_list, true) ) ) {
        goto cleanup;
    }

    /*
     * For each hnp in the listing
     */
    while (NULL != (item  = opal_list_remove_first(&hnp_list))) {
        orte_hnp_contact_t *hnp = (orte_hnp_contact_t*)item;
        if (my_globals.hnppid == hnp->pid) {
            /* this is the one we want */
            my_globals.target_hnp = hnp;
            break;
        }
        OBJ_RELEASE(hnp);
    }
    
    /* setup the stream */
    stream = 0;
    if (my_globals.stderr_req) {
        stream |= ORTE_IOF_STDERR;
    }
    if (my_globals.stddiag_req) {
        stream |= ORTE_IOF_STDDIAG;
    }
    if (my_globals.stdout_req) {
        stream |= ORTE_IOF_STDOUT;
    }
    if (0 == stream) {
        /* default to stdout */
        stream |= ORTE_IOF_STDOUT;
    }
    
    /* parse the input ranks */
    orte_util_parse_range_options(my_globals.ranks, &ranks);
    
    /* pull the specified output streams and dump to our stdout */
    for (i=0; i < opal_argv_count(ranks); i++) {
        target_proc.jobid = my_globals.target_hnp->name.jobid + 1;
        target_proc.vpid = strtol(ranks[i], NULL, 10);
        if (ORTE_SUCCESS != (ret = orte_iof.pull(&target_proc, stream, 1))) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }
    }
    
    /* just wait until the abort is fired */
    opal_event_dispatch();

    /***************
     * Cleanup
     ***************/
 cleanup:
    while (NULL != (item  = opal_list_remove_first(&hnp_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&hnp_list);
    opal_argv_free(ranks);
    orte_finalize();

    return ret;
}

static void abort_exit_callback(int fd, short ign, void *arg)
{
    opal_list_item_t *item;
    int ret;
    
    /* Remove the TERM and INT signal handlers */
    opal_signal_del(&term_handler);
    opal_signal_del(&int_handler);

    /* close the outstanding pull */
    if (ORTE_SUCCESS != (ret = orte_iof.close(&target_proc, ORTE_IOF_STDOUT))) {
        ORTE_ERROR_LOG(ret);
    }

    while (NULL != (item  = opal_list_remove_first(&hnp_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&hnp_list);
    orte_finalize();
    exit(1);
}
