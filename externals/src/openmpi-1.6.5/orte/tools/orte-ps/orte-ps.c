/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
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

#include "opal/util/cmd_line.h"
#include "opal/util/output.h"
#include "opal/util/opal_environ.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/runtime/opal.h"
#if OPAL_ENABLE_FT_CR == 1
#include "opal/runtime/opal_cr.h"
#endif

#include "orte/runtime/runtime.h"
#include "orte/util/hnp_contact.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/comm/comm.h"
#include "orte/mca/ras/ras_types.h"
#if OPAL_ENABLE_FT_CR == 1
#include "orte/mca/snapc/base/base.h"
#endif
#include "orte/runtime/orte_globals.h"

struct orte_ps_mpirun_info_t {
    /** This is an object, so it must have a super */
    opal_list_item_t super;

    /* HNP info */
    orte_hnp_contact_t *hnp;
    
    /* array of jobs */
    orte_std_cntr_t num_jobs;
    orte_job_t **jobs;
    
    /* array of nodes */
    orte_std_cntr_t num_nodes;
    orte_node_t **nodes;
};
typedef struct orte_ps_mpirun_info_t orte_ps_mpirun_info_t;

static void orte_ps_mpirun_info_construct(orte_ps_mpirun_info_t *ptr)
{
    ptr->hnp = NULL;
    ptr->num_jobs = 0;
    ptr->jobs = NULL;
    ptr->num_nodes = 0;
    ptr->nodes = NULL;
}
static void orte_ps_mpirun_info_destruct(orte_ps_mpirun_info_t *ptr)
{
    orte_std_cntr_t i;
    
    if (NULL != ptr->hnp) OBJ_RELEASE(ptr->hnp);
    if (NULL != ptr->jobs) {
        for (i=0; i < ptr->num_jobs; i++) {
            OBJ_RELEASE(ptr->jobs[i]);
        }
        free(ptr->jobs);
    }
    if (NULL != ptr->nodes) {
        for (i=0; i < ptr->num_nodes; i++) {
            OBJ_RELEASE(ptr->nodes[i]);
        }
        free(ptr->nodes);
    }
}

OBJ_CLASS_INSTANCE(orte_ps_mpirun_info_t,
                   opal_list_item_t,
                   orte_ps_mpirun_info_construct,
                   orte_ps_mpirun_info_destruct);

/******************
 * Local Functions
 ******************/
static int orte_ps_init(int argc, char *argv[]);
static int parse_args(int argc, char *argv[]);

static int gather_information(orte_ps_mpirun_info_t *hnpinfo);
static int gather_active_jobs(orte_ps_mpirun_info_t *hnpinfo);
static int gather_nodes(orte_ps_mpirun_info_t *hnpinfo);
static int gather_vpid_info(orte_ps_mpirun_info_t *hnpinfo);

static int pretty_print(orte_ps_mpirun_info_t *hnpinfo);
static int pretty_print_nodes(orte_node_t **nodes, orte_std_cntr_t num_nodes);
static int pretty_print_jobs(orte_job_t **jobs, orte_std_cntr_t num_jobs);
static int pretty_print_vpids(orte_job_t *job);
static void pretty_print_dashed_line(int len);

static char *pretty_node_state(orte_node_state_t state);
static char *pretty_job_state(orte_job_state_t state);
static char *pretty_vpid_state(orte_proc_state_t state);

/*****************************************
 * Global Vars for Command line Arguments
 *****************************************/
typedef struct {
    bool help;
    bool verbose;
    int  jobid;
    int  vpid;
    bool nodes;
    bool daemons;
    int  output;
} orte_ps_globals_t;

orte_ps_globals_t orte_ps_globals;

opal_cmd_line_init_t cmd_line_opts[] = {
    { NULL, NULL, NULL, 
      'h', NULL, "help", 
      0,
      &orte_ps_globals.help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },

    { NULL, NULL, NULL, 
      'v', NULL, "verbose", 
      0,
      &orte_ps_globals.verbose, OPAL_CMD_LINE_TYPE_BOOL,
      "Be Verbose" },

    { NULL, NULL, NULL, 
      '\0', NULL, "daemons", 
      0,
     &orte_ps_globals.daemons, OPAL_CMD_LINE_TYPE_INT,
      "Display daemon job information" },

    { NULL, NULL, NULL, 
      'j', NULL, "jobid", 
      1,
      &orte_ps_globals.jobid, OPAL_CMD_LINE_TYPE_INT,
      "Specify a specific jobid" },
#if 0
    { NULL, NULL, NULL, 
      'p', NULL, "vpid", 
      1,
      &orte_ps_globals.vpid, OPAL_CMD_LINE_TYPE_INT,
      "Specify a specific vpid. Must specify a --jobid as well" },
#endif
    { NULL, NULL, NULL, 
      'n', NULL, "nodes", 
      0,
      &orte_ps_globals.nodes, OPAL_CMD_LINE_TYPE_INT,
      "Display Node Information" },

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
    opal_list_t hnp_list;
    opal_list_item_t* item = NULL;
    orte_ps_mpirun_info_t hnpinfo;
    
    /***************
     * Initialize
     ***************/
    OBJ_CONSTRUCT(&hnp_list, opal_list_t);

    if (ORTE_SUCCESS != (ret = orte_ps_init(argc, argv))) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Get the directory listing
     */
    opal_output_verbose(10, orte_ps_globals.output,
                        "orte_ps: Acquiring list of HNPs and setting contact info into RML...\n");

    if (ORTE_SUCCESS != (ret = orte_list_local_hnps(&hnp_list, true) ) ) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * For each hnp in the listing
     */
    while (NULL != (item  = opal_list_remove_first(&hnp_list))) {
        orte_hnp_contact_t *hnp = (orte_hnp_contact_t*)item;
        hnpinfo.hnp = hnp;

        /*
         * Gather the information
         */
        opal_output_verbose(10, orte_ps_globals.output,
                            "orte_ps: Gathering Information for HNP: %s:%d\n",
                            ORTE_NAME_PRINT(&(hnpinfo.hnp->name)),
                            hnpinfo.hnp->pid);
        
        if( ORTE_SUCCESS != (ret = gather_information(&hnpinfo)) ) {
            /* this could be due to a stale session directory - if so,
             * just skip this entry, but don't abort
             */
            if (ORTE_ERR_SILENT == ret) {
                orte_show_help("help-orte-ps.txt", "stale-hnp", true,
                               ORTE_NAME_PRINT(&(hnpinfo.hnp->name)));
                continue;
            }
            goto cleanup;
        }

        /*
         * Print the information
         */
        if(ORTE_SUCCESS != (ret = pretty_print(&hnpinfo)) ) {
            exit_status = ret;
            goto cleanup;
        }
    }

    /***************
     * Cleanup
     ***************/
 cleanup:
    orte_finalize();

    return exit_status;
}

static int parse_args(int argc, char *argv[]) {
    int ret;
    opal_cmd_line_t cmd_line;
    orte_ps_globals_t tmp = { false,                    /* help */
                              false,                    /* verbose */
                              ORTE_JOBID_WILDCARD,      /* jobid */
                              ORTE_VPID_WILDCARD,       /* vpid */
                              false,                    /* nodes */
                              false,                    /* daemons */
                              -1};                      /* output */

    orte_ps_globals = tmp;

    /* Parse the command line options */
    opal_cmd_line_create(&cmd_line, cmd_line_opts);
    
    mca_base_open();
    mca_base_cmd_line_setup(&cmd_line);
    ret = opal_cmd_line_parse(&cmd_line, true, argc, argv);
    
    /**
     * Now start parsing our specific arguments
     */
    if (OPAL_SUCCESS != ret || 
        orte_ps_globals.help) {
        char *args = NULL;
        args = opal_cmd_line_get_usage_msg(&cmd_line);
        orte_show_help("help-orte-ps.txt", "usage", true,
                       args);
        free(args);
        return ORTE_ERROR;
    }

    /*
     * If they specify a vpid, they must specify a jobid
     */
#if 0
    if( ORTE_VPID_WILDCARD != orte_ps_globals.vpid) {
        if( ORTE_JOBID_WILDCARD == orte_ps_globals.jobid) {
            orte_show_help("help-orte-ps.txt", "vpid-usage", true,
                           orte_ps_globals.vpid);
            return ORTE_ERROR;
        }
    }
#endif
    return ORTE_SUCCESS;
}

static int orte_ps_init(int argc, char *argv[]) {
    int ret;
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
     * Parse Command Line Arguments
     */
    if (ORTE_SUCCESS != (ret = parse_args(argc, argv))) {
        return ret;
    }

    /*
     * Setup OPAL Output handle from the verbose argument
     */
    if( orte_ps_globals.verbose ) {
        orte_ps_globals.output = opal_output_open(NULL);
        opal_output_set_verbosity(orte_ps_globals.output, 10);
    } else {
        orte_ps_globals.output = 0; /* Default=STDERR */
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

    tmp_env_var = mca_base_param_env_var("opal_cr_is_tool");
    opal_setenv(tmp_env_var,
                "1",
                true, &environ);
    free(tmp_env_var);
#else
    tmp_env_var = NULL; /* Silence compiler warning */
#endif

    /***************************
     * We need all of OPAL and the TOOL portion of ORTE
     ***************************/
    ret = orte_init(&argc, &argv, ORTE_PROC_TOOL);

    return ret;
}

static int pretty_print(orte_ps_mpirun_info_t *hnpinfo) {
    char *header;
    int len_hdr;
    
    /*
     * Print header and remember header length
     */
    len_hdr = asprintf(&header, "Information from mpirun %s", ORTE_JOBID_PRINT(hnpinfo->hnp->name.jobid));
    
    printf("\n\n%s\n", header);
    free(header);
    pretty_print_dashed_line(len_hdr);
    
    /*
     * Print Node Information
     */
    if( orte_ps_globals.nodes )
        pretty_print_nodes(hnpinfo->nodes, hnpinfo->num_nodes);

    /*
     * Print Job Information
     */
    pretty_print_jobs(hnpinfo->jobs, hnpinfo->num_jobs);

    return ORTE_SUCCESS;
}

static int pretty_print_nodes(orte_node_t **nodes, orte_std_cntr_t num_nodes) {
    int line_len;
    int len_name    = 0,
        len_state   = 0,
        len_slots   = 0,
        len_slots_i = 0,
        len_slots_m = 0;
    orte_node_t *node;
    orte_std_cntr_t i;

    /*
     * Caculate segment lengths
     */
    len_name    = (int) strlen("Node Name");
    len_state   = (int) strlen("State");
    len_slots   = (int) strlen("Slots");
    len_slots_i = (int) strlen("Slots In Use");
    len_slots_m = (int) strlen("Slots Max");

    for(i=0; i < num_nodes; i++) {
        node = nodes[i];
        
        if( NULL != node->name &&
            (int)strlen(node->name) > len_name)
            len_name = (int) strlen(node->name);
                
        if( (int)strlen(pretty_node_state(node->state)) > len_state )
            len_state = (int)strlen(pretty_node_state(node->state));
    }
    
    line_len = (len_name    + 3 +
                len_state   + 3 +
                len_slots   + 3 +
                len_slots_i + 3 +
                len_slots_m) + 2;

    /*
     * Print the header
     */
    printf("%*s | ", len_name,    "Node Name");
    printf("%*s | ", len_state,   "State");
    printf("%*s | ", len_slots,   "Slots");
    printf("%*s | ", len_slots_m, "Slots Max");
    printf("%*s | ", len_slots_i, "Slots In Use");
    printf("\n");

    pretty_print_dashed_line(line_len);
    
    /*
     * Print Info
     */
    for(i=0; i < num_nodes; i++) {
        node = nodes[i];
        
        printf("%*s | ", len_name,    node->name);
        printf("%*s | ", len_state,   pretty_node_state(node->state));
        printf("%*d | ", len_slots,   (uint)node->slots);
        printf("%*d | ", len_slots_m, (uint)node->slots_max);
        printf("%*d | ", len_slots_i, (uint)node->slots_inuse);
        printf("\n");
        
    }
    
    return ORTE_SUCCESS;
}

static int pretty_print_jobs(orte_job_t **jobs, orte_std_cntr_t num_jobs) {
    int len_jobid = 0,
        len_state = 0,
        len_slots = 0,
        len_vpid_r = 0,
        len_ckpt_s = 0,
        len_ckpt_r = 0,
        len_ckpt_l = 0;
    int line_len;
    orte_job_t *job;
    orte_std_cntr_t i;
    char *jobstr;
    orte_jobid_t mask=0x0000ffff;
    char * state_str = NULL;

    for(i=0; i < num_jobs; i++) {
        job = jobs[i];

        /* check the jobid to see if this is the daemons' job */
        if ((0 == (mask & job->jobid)) && !orte_ps_globals.daemons) {
            continue;
        }

        /* setup the printed name - do -not- free this! */
        jobstr = ORTE_JOBID_PRINT(job->jobid);
        
        /*
         * Caculate segment lengths
         */
        len_jobid  = strlen(jobstr);;
        len_state  = (int) (strlen(pretty_job_state(job->state)) < strlen("State") ?
                            strlen("State") :
                            strlen(pretty_job_state(job->state)));
        len_slots  = 6;
        len_vpid_r = (int) strlen("Num Procs");
#if OPAL_ENABLE_FT_CR == 1
        orte_snapc_ckpt_state_str(&state_str, job->ckpt_state);
        len_ckpt_s = (int) (strlen(state_str) < strlen("Ckpt State") ?
                            strlen("Ckpt State") :
                            strlen(state_str) );
        len_ckpt_r = (int) (NULL == job->ckpt_snapshot_ref ? strlen("Ckpt Ref") :
                            (strlen(job->ckpt_snapshot_ref) < strlen("Ckpt Ref") ?
                             strlen("Ckpt Ref") :
                             strlen(job->ckpt_snapshot_ref) ) );
        len_ckpt_l = (int) (NULL == job->ckpt_snapshot_loc ? strlen("Ckpt Loc") :
                            (strlen(job->ckpt_snapshot_loc) < strlen("Ckpt Loc") ?
                             strlen("Ckpt Loc") :
                             strlen(job->ckpt_snapshot_loc) ) );
#else
        state_str = NULL;
        len_ckpt_s = -3;
        len_ckpt_r = -3;
        len_ckpt_l = -3;
#endif
    
        line_len = (len_jobid  + 3 +
                    len_state  + 3 +
                    len_slots  + 3 +
                    len_vpid_r + 3 +
                    len_ckpt_s + 3 +
                    len_ckpt_r + 3 +
                    len_ckpt_l)
                    + 2;

        /*
         * Print Header
         */
        printf("\n");
        printf("%*s | ", len_jobid  , "JobID");
        printf("%*s | ", len_state  , "State");
        printf("%*s | ", len_slots  , "Slots");
        printf("%*s | ", len_vpid_r , "Num Procs");
#if OPAL_ENABLE_FT_CR == 1
        printf("%*s | ", len_ckpt_s , "Ckpt State");
        printf("%*s | ", len_ckpt_r , "Ckpt Ref");
        printf("%*s |",  len_ckpt_l , "Ckpt Loc");
#endif
        printf("\n");

        pretty_print_dashed_line(line_len);

        /*
         * Print Info
         */        
        printf("%*s | ",  len_jobid ,  ORTE_JOBID_PRINT(job->jobid));
        printf("%*s | ",  len_state ,  pretty_job_state(job->state));
        printf("%*d | ",  len_slots ,  (uint)job->total_slots_alloc);
        printf("%*d | ",  len_vpid_r,  job->num_procs);
#if OPAL_ENABLE_FT_CR == 1
        printf("%*s | ",  len_ckpt_s,  state_str);
        printf("%*s | ",  len_ckpt_r,  (NULL == job->ckpt_snapshot_ref ? 
                                        "" :
                                        job->ckpt_snapshot_ref) );
        printf("%*s |",   len_ckpt_l,  (NULL == job->ckpt_snapshot_loc ? 
                                        "" :
                                        job->ckpt_snapshot_loc) );
#endif
        printf("\n");


        pretty_print_vpids(job);
        printf("\n\n"); /* give a little room between job outputs */
    }
    
    return ORTE_SUCCESS;
}

static int pretty_print_vpids(orte_job_t *job) {
    int len_o_proc_name = 0, 
        len_proc_name   = 0, 
        len_rank        = 0, 
        len_pid         = 0,
        len_state       = 0,
        len_node        = 0,
        len_ckpt_s      = 0,
        len_ckpt_r      = 0,
        len_ckpt_l      = 0;
    int i, line_len;
    orte_vpid_t v;
    orte_proc_t *vpid;
    orte_app_context_t *app;
    char *o_proc_name;
    char *state_str = NULL;

    /*
     * Caculate segment lengths
     */
    len_o_proc_name = (int)strlen("ORTE Name");
    len_proc_name   = (int)strlen("Process Name");
    len_rank        = (int)strlen("Local Rank");
    len_pid         = 6;
    len_state       = 0;
    len_node        = 0;
#if OPAL_ENABLE_FT_CR == 1
    len_ckpt_s      = strlen("Ckpt State");
    len_ckpt_r      = strlen("Ckpt Ref");
    len_ckpt_l      = strlen("Ckpt Loc");
#else
    len_ckpt_s      = -3;
    len_ckpt_r      = -3;
    len_ckpt_l      = -3;
#endif

    for(v=0; v < job->num_procs; v++) {
        char *rankstr;
        vpid = (orte_proc_t*)job->procs->addr[v];
        
        /*
         * Find my app context
         */
        if( 0 >= (int)job->num_apps ) {
            if( 0 == vpid->name.vpid ) {
                if( (int)strlen("orterun") > len_proc_name)
                    len_proc_name = strlen("orterun");
            }
            else {
                if( (int)strlen("orted") > len_proc_name)
                    len_proc_name = strlen("orted");
            }
        }
        for( i = 0; i < (int)job->num_apps; ++i) {
            app = (orte_app_context_t*)job->apps->addr[i];
            if( app->idx == vpid->app_idx ) {
                if( (int)strlen(app->app) > len_proc_name) 
                    len_proc_name = strlen(app->app);
                break;
            }
        }
        
        o_proc_name = orte_util_print_name_args(&vpid->name);
        if ((int)strlen(o_proc_name) > len_o_proc_name)
            len_o_proc_name = strlen(o_proc_name);

        asprintf(&rankstr, "%u", (uint)vpid->local_rank);
        if ((int)strlen(rankstr) > len_rank)
            len_rank = strlen(rankstr);
        free(rankstr);

        if( NULL != vpid->nodename && (int)strlen(vpid->nodename) > len_node) {
            len_node = strlen(vpid->nodename);
        } else if ((int)strlen("Unknown") > len_node) {
            len_node = strlen("Unknown");
        }

        if( (int)strlen(pretty_vpid_state(vpid->state)) > len_state)
            len_state = strlen(pretty_vpid_state(vpid->state));
        
#if OPAL_ENABLE_FT_CR == 1
        orte_snapc_ckpt_state_str(&state_str, vpid->ckpt_state);
        if( (int)strlen(state_str) > len_ckpt_s)
            len_ckpt_s = strlen(state_str);
        
        if( NULL != vpid->ckpt_snapshot_ref &&
            (int)strlen(vpid->ckpt_snapshot_ref) > len_ckpt_r) 
            len_ckpt_r = strlen(vpid->ckpt_snapshot_ref);
        
        if( NULL != vpid->ckpt_snapshot_loc &&
            (int)strlen(vpid->ckpt_snapshot_loc) > len_ckpt_l) 
            len_ckpt_l = strlen(vpid->ckpt_snapshot_loc);
#else
        state_str = NULL;
#endif
    }

    line_len = (len_o_proc_name + 3 +
                len_proc_name   + 3 +
                len_rank        + 3 +
                len_pid         + 3 +
                len_state       + 3 +
                len_node        + 3 +
                len_ckpt_s      + 3 +
                len_ckpt_r      + 3 +
                len_ckpt_l)
                + 2;

    /*
     * Print Header
     */
    printf("\t");
    printf("%*s | ", len_proc_name   , "Process Name");
    printf("%*s | ", len_o_proc_name , "ORTE Name");
    printf("%*s | ", len_rank        , "Local Rank");
    printf("%*s | ", len_pid         , "PID");
    printf("%*s | ", len_node        , "Node");
    printf("%*s | ", len_state       , "State");
#if OPAL_ENABLE_FT_CR == 1
    printf("%*s | ", len_ckpt_s      , "Ckpt State");
    printf("%*s | ", len_ckpt_r      , "Ckpt Ref");
    printf("%*s |",  len_ckpt_l      , "Ckpt Loc");
#endif
    printf("\n");
    
    printf("\t");
    pretty_print_dashed_line(line_len);
    
    /*
     * Print Info
     */
    for(v=0; v < job->num_procs; v++) {
        vpid = (orte_proc_t*)job->procs->addr[v];
        
        printf("\t");

        if( 0 >= (int)job->num_apps ) {
            if( 0 == vpid->name.vpid ) {
                printf("%*s | ", len_proc_name, "orterun");
            } else {
                printf("%*s | ", len_proc_name, "orted");
            }
        }
        for( i = 0; i < (int)job->num_apps; ++i) {
            app = (orte_app_context_t*)job->apps->addr[i];
            if( app->idx == vpid->app_idx ) {
                printf("%*s | ", len_proc_name, app->app);
                break;
            }
        }
        
        o_proc_name = orte_util_print_name_args(&vpid->name);

        printf("%*s | ",  len_o_proc_name, o_proc_name);
        printf("%*u | ",  len_rank       , (uint)vpid->local_rank);
        printf("%*d | ",  len_pid        , vpid->pid);
        printf("%*s | ",  len_node       , (NULL == vpid->nodename) ? "Unknown" : vpid->nodename);
        printf("%*s | ",  len_state      , pretty_vpid_state(vpid->state));
        
#if OPAL_ENABLE_FT_CR == 1
        printf("%*s | ",  len_ckpt_s, state_str);
        printf("%*s | ",  len_ckpt_r, (NULL == vpid->ckpt_snapshot_ref ? 
                                       "" : 
                                       vpid->ckpt_snapshot_ref));
        printf("%*s |",   len_ckpt_l, (NULL == vpid->ckpt_snapshot_loc ? 
                                       "" : 
                                       vpid->ckpt_snapshot_loc));
#endif
        printf("\n");
        
    }
    
    return ORTE_SUCCESS;
}

static void pretty_print_dashed_line(int len) {
    static const char dashes[9] = "--------";

    while (len >= 8) {
        printf("%8.8s", dashes);
        len -= 8;
    }
    printf("%*.*s\n", len, len, dashes);
}

static int gather_information(orte_ps_mpirun_info_t *hnpinfo) {
    int ret;
    
    if( ORTE_SUCCESS != (ret = gather_active_jobs(hnpinfo) )) {
        goto cleanup;
    }

    if( ORTE_SUCCESS != (ret = gather_nodes(hnpinfo) )) {
        goto cleanup;
    }

    if( ORTE_SUCCESS != (ret = gather_vpid_info(hnpinfo) )) {
        goto cleanup;
    }

 cleanup:
    return ret;
}

static int gather_active_jobs(orte_ps_mpirun_info_t *hnpinfo) {
    int ret;
    
    if (ORTE_SUCCESS != (ret = orte_util_comm_query_job_info(&(hnpinfo->hnp->name), orte_ps_globals.jobid,
                                                             &hnpinfo->num_jobs, &hnpinfo->jobs))) {
        ORTE_ERROR_LOG(ret);
    }
    
    return ret;
}

static int gather_nodes(orte_ps_mpirun_info_t *hnpinfo) {
    int ret;
    
    if (ORTE_SUCCESS != (ret = orte_util_comm_query_node_info(&(hnpinfo->hnp->name), NULL,
                                                             &hnpinfo->num_nodes, &hnpinfo->nodes))) {
        ORTE_ERROR_LOG(ret);
    }
    
    return ret;
}

static int gather_vpid_info(orte_ps_mpirun_info_t *hnpinfo) {
    int ret;
    orte_std_cntr_t i;
    int cnt;
    orte_job_t *job;
    orte_proc_t **procs;

    /*
     * For each Job in the HNP
     */
    for(i=0; i < hnpinfo->num_jobs; i++) {
        job = hnpinfo->jobs[i];

        /*
         * Skip getting the vpid's for the HNP, unless asked to do so
         * The HNP is always the first in the array
         */
        if( 0 == i && !orte_ps_globals.daemons) {
            continue;
        }

        /* query the HNP for info on the procs in this job */
        if (ORTE_SUCCESS != (ret = orte_util_comm_query_proc_info(&(hnpinfo->hnp->name), job->jobid,
                                                                  orte_ps_globals.vpid, &cnt, &procs))) {
            ORTE_ERROR_LOG(ret);
        }
        job->procs->addr = (void**)procs;
        job->procs->size = cnt;
        job->num_procs = cnt;
    }

    return ORTE_SUCCESS;
}

static char *pretty_job_state(orte_job_state_t state) {
    switch(state) {
        case ORTE_JOB_STATE_UNDEF:
            return strdup("Undef");
            break;
        case ORTE_JOB_STATE_INIT:
            return strdup("Init");
            break;
        case ORTE_JOB_STATE_LAUNCHED:
            return strdup("Launched");
            break;
        case ORTE_JOB_STATE_RUNNING:
            return strdup("Running");
            break;
        case ORTE_JOB_STATE_TERMINATED:
            return strdup("Terminated");
            break;
        case ORTE_JOB_STATE_ABORTED:
            return strdup("Aborted");
            break;
        case ORTE_JOB_STATE_FAILED_TO_START:
            return strdup("Failed to start");
            break;
        case ORTE_JOB_STATE_ABORTED_BY_SIG:
            return strdup("Aborted by signal");
            break;
        case ORTE_JOB_STATE_ABORT_ORDERED:
            return strdup("Aborted ordered");
            break;
        default:
            break;
    }
    
    return strdup("");
}

static char *pretty_vpid_state(orte_proc_state_t state) {
    switch(state) {
        case ORTE_PROC_STATE_UNDEF:
            return strdup("Undef");
            break;
        case ORTE_PROC_STATE_INIT:
            return strdup("Init");
            break;
        case ORTE_PROC_STATE_LAUNCHED:
            return strdup("Launched");
            break;
        case ORTE_PROC_STATE_RUNNING:
            return strdup("Running");
            break;
        case ORTE_PROC_STATE_TERMINATED:
            return strdup("Terminated");
            break;
        case ORTE_PROC_STATE_ABORTED:
            return strdup("Aborted");
            break;
        case ORTE_PROC_STATE_FAILED_TO_START:
            return strdup("Failed to start");
            break;
        case ORTE_PROC_STATE_ABORTED_BY_SIG:
            return strdup("Aborted by signal");
            break;
        default:
            break;
    }
    
    return strdup("");
}

static char *pretty_node_state(orte_node_state_t state) {
    switch(state) {
    case ORTE_NODE_STATE_DOWN:
        return strdup("Down");
        break;
    case ORTE_NODE_STATE_UP:
        return strdup("Up");
        break;
    case ORTE_NODE_STATE_REBOOT:
        return strdup("Reboot");
        break;
    case ORTE_NODE_STATE_UNKNOWN:
    default:
        return strdup("Unknown");
        break;
    }
}
