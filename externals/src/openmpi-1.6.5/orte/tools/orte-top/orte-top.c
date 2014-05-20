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
#include <stdlib.h>

#include "opal/util/cmd_line.h"
#include "opal/util/argv.h"
#include "opal/dss/dss.h"
#include "opal/mca/base/base.h"
#include "opal/util/opal_environ.h"
#include "opal/runtime/opal.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/routed/routed.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/hnp_contact.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_wait.h"
#include "orte/mca/rml/base/rml_contact.h"

/*
 * Local variables & functions
 */
static void abort_exit_callback(int fd, short flags, void *arg);
static struct opal_event term_handler;
static struct opal_event int_handler;
static opal_list_t hnp_list;
static bool all_recvd;
static int32_t num_replies;
static int32_t num_recvd;
static opal_buffer_t cmdbuf;
static opal_event_t *my_exit_event;
static FILE *fp = NULL;
static bool help;
static char *hnppidstr;
static char *hnpuristr;
static char *ranks;
static orte_hnp_contact_t *target_hnp;
static int update_rate;
static bool timestamp;
static char *logfile;
static bool bynode;
static opal_list_t recvd_stats;
static char *sample_time;
static bool need_header = true;
static int num_lines=0;
static bool fields_set = false;
static int nodefield = 0;
static int rankfield = 0;
static int pidfield = 0;
static int cmdfield = 0;
static int timefield = 6;
static int prifield = 0;
static int thrfield = 0;
static int vsizefield = 0;
static int rssfield = 0;
static int pkvfield = 0;
static int shfield = 0;
static int pfield = 0;

/* flag what fields were actually found */
static bool pri_found = false;
static bool thr_found = false;
static bool vsize_found = false;
static bool rss_found = false;
static bool pkv_found = false;
static bool sh_found = false;
static bool p_found = false;

#define MAX_LINES 20

opal_cmd_line_init_t cmd_line_opts[] = {
    { NULL, NULL, NULL, 
      'h', NULL, "help", 
      0,
      &help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },

    { NULL, NULL, NULL, 
      '\0', "pid", "pid", 
      1,
      &hnppidstr, OPAL_CMD_LINE_TYPE_STRING,
      "The pid of the mpirun that you wish to query/monitor" },

    { NULL, NULL, NULL, 
      '\0', "uri", "uri", 
      1,
      &hnpuristr, OPAL_CMD_LINE_TYPE_STRING,
      "The uri of the mpirun that you wish to query/monitor" },
    
    { NULL, NULL, NULL, 
      '\0', "rank", "rank", 
      1,
      &ranks, OPAL_CMD_LINE_TYPE_STRING,
      "Rank whose resource usage is to be displayed/monitored" },

    { NULL, NULL, NULL, 
      '\0', "update-rate", "update-rate", 
      1,
      &update_rate, OPAL_CMD_LINE_TYPE_INT,
      "Number of seconds between updates" },
    
    { NULL, NULL, NULL, 
      '\0', "timestamp", "timestamp", 
      0,
      &timestamp, OPAL_CMD_LINE_TYPE_BOOL,
      "Time stamp each sample" },
    
    { NULL, NULL, NULL, 
      '\0', "log-file", "log-file", 
      1,
      &logfile, OPAL_CMD_LINE_TYPE_STRING,
      "Output file for returned statistics" },
 
    { NULL, NULL, NULL, 
      '\0', "bynode", "bynode", 
      0,
      &bynode, OPAL_CMD_LINE_TYPE_BOOL,
      "Group statistics by node, sorted by rank within each node" },

    /* End of list */
    { NULL, NULL, NULL, 
      '\0', NULL, NULL, 
      0,
      NULL, OPAL_CMD_LINE_TYPE_NULL,
      NULL }
};


static void recv_stats(int status, orte_process_name_t* sender,
                       opal_buffer_t *buffer, orte_rml_tag_t tag,
                       void* cbdata);

static void pretty_print(void);
static void print_headers(void);

static void send_cmd(int fd, short dummy, void *arg)
{
    int ret;
    all_recvd = false;
    num_replies = INT_MAX;
    num_recvd = 0;
    if (0 > (ret = orte_rml.send_buffer(&(target_hnp->name), &cmdbuf, ORTE_RML_TAG_DAEMON, 0))) {
        ORTE_ERROR_LOG(ret);
        orte_trigger_event(&orteds_exit);
        return;
    }
    
    ORTE_PROGRESSED_WAIT(all_recvd, 0, 1);
    
    /* flag that field sizes are set */
    fields_set = true;
    
    /* pretty-print what we got */
    pretty_print();

    /* see if we want to do it again */
    if (0 < update_rate) {
        ORTE_TIMER_EVENT(update_rate, 0, send_cmd);
    } else {
        orte_trigger_event(&orte_exit);
    }
}

int
main(int argc, char *argv[])
{
    int ret;
    opal_cmd_line_t cmd_line;
    opal_list_item_t* item = NULL;
    orte_daemon_cmd_flag_t command;
    pid_t hnppid;
    orte_process_name_t proc;
    char **r1=NULL, **r2;
    int i;
    orte_vpid_t vstart, vend;
    int vint;
    
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
    help = false;
    hnppidstr = NULL;
    ranks = NULL;
    target_hnp = NULL;
    update_rate = -1;
    timestamp = false;
    logfile = NULL;
    
    /* Parse the command line options */
    opal_cmd_line_create(&cmd_line, cmd_line_opts);
    
    mca_base_open();
    mca_base_cmd_line_setup(&cmd_line);
    ret = opal_cmd_line_parse(&cmd_line, true, argc, argv);
    
    /**
     * Now start parsing our specific arguments
     */
    if (OPAL_SUCCESS != ret || help) {
        char *args = NULL;
        args = opal_cmd_line_get_usage_msg(&cmd_line);
        orte_show_help("help-orte-top.txt", "orte-top:usage", true, "orte-top", args);
        free(args);
        return ORTE_ERROR;
    }
    
    /***************************
     * We need all of OPAL and the TOOL portion of ORTE
     ***************************/
    if (ORTE_SUCCESS != orte_init(&argc, &argv, ORTE_PROC_TOOL)) {
        orte_finalize();
        return 1;
    }
    
    OBJ_CONSTRUCT(&orte_exit, orte_trigger_event_t);
    
    if (ORTE_SUCCESS != orte_wait_event(&my_exit_event, &orte_exit, "job_complete", abort_exit_callback)) {
        orte_finalize();
        exit(1);
    }
    
    /* setup the list for recvd stats */
    OBJ_CONSTRUCT(&recvd_stats, opal_list_t);
    
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
     * Must specify the mpirun pid
     */
    if (NULL != hnppidstr) {
        if (0 == strncmp(hnppidstr, "file", strlen("file")) ||
            0 == strncmp(hnppidstr, "FILE", strlen("FILE"))) {
            char input[1024], *filename;
            FILE *fp;
            
            /* it is a file - get the filename */
            filename = strchr(hnppidstr, ':');
            if (NULL == filename) {
                /* filename is not correctly formatted */
                orte_show_help("help-orte-top.txt", "orte-top:hnp-filename-bad", true, "pid", hnppidstr);
                orte_finalize();
                exit(1);
            }
            ++filename; /* space past the : */
            
            if (0 >= strlen(filename)) {
                /* they forgot to give us the name! */
                orte_show_help("help-orte-top.txt", "orte-top:hnp-filename-bad", true, "pid", hnppidstr);
                orte_finalize();
                exit(1);
            }
            
            /* open the file and extract the pid */
            fp = fopen(filename, "r");
            if (NULL == fp) { /* can't find or read file! */
                orte_show_help("help-orte-top.txt", "orte-top:hnp-filename-access", true, filename);
                orte_finalize();
                exit(1);
            }
            if (NULL == fgets(input, 1024, fp)) {
                /* something malformed about file */
                fclose(fp);
                orte_show_help("help-orte-top.txt", "orte-top:hnp-file-bad", true, filename);
                orte_finalize();
                exit(1);
            }
            fclose(fp);
            input[strlen(input)-1] = '\0';  /* remove newline */
            /* convert the pid */
            hnppid = strtoul(input, NULL, 10);
        } else {
            /* should just be the pid itself */
            hnppid = strtoul(hnppidstr, NULL, 10);
        }
        /*
         * Get the list of available hnp's and setup contact info
         * to them in the RML
         */
        OBJ_CONSTRUCT(&hnp_list, opal_list_t);
        if (ORTE_SUCCESS != (ret = orte_list_local_hnps(&hnp_list, true) ) ) {
            orte_show_help("help-orte-top.txt", "orte-top:pid-not-found", true, hnppid);
            orte_finalize();
            exit(1);
        }
        
        /*
         * For each hnp in the listing
         */
        while (NULL != (item  = opal_list_remove_first(&hnp_list))) {
            orte_hnp_contact_t *hnp = (orte_hnp_contact_t*)item;
            if (hnppid == hnp->pid) {
                /* this is the one we want */
                target_hnp = hnp;
                /* let it continue to run so we deconstruct the list */
                continue;
            }
            OBJ_RELEASE(hnp);
        }
        OBJ_DESTRUCT(&hnp_list);
        
        /* if we get here without finding the one we wanted, then abort */
        if (NULL == target_hnp) {
            orte_show_help("help-orte-top.txt", "orte-top:pid-not-found", true, hnppid);
            orte_finalize();
            exit(1);
        }
    } else if (NULL != hnpuristr) {
        if (0 == strncmp(hnpuristr, "file", strlen("file")) ||
            0 == strncmp(hnpuristr, "FILE", strlen("FILE"))) {
            char input[1024], *filename;
            FILE *fp;
            
            /* it is a file - get the filename */
            filename = strchr(hnpuristr, ':');
            if (NULL == filename) {
                /* filename is not correctly formatted */
                orte_show_help("help-orte-top.txt", "orte-top:hnp-filename-bad", true, "uri", hnpuristr);
                orte_finalize();
                exit(1);
            }
            ++filename; /* space past the : */
            
            if (0 >= strlen(filename)) {
                /* they forgot to give us the name! */
                orte_show_help("help-orte-top.txt", "orte-top:hnp-filename-bad", true, "uri", hnpuristr);
                orte_finalize();
                exit(1);
            }
            
            /* open the file and extract the uri */
            fp = fopen(filename, "r");
            if (NULL == fp) { /* can't find or read file! */
                orte_show_help("help-orte-top.txt", "orte-top:hnp-filename-access", true, filename);
                orte_finalize();
                exit(1);
            }
            if (NULL == fgets(input, 1024, fp)) {
                /* something malformed about file */
                fclose(fp);
                orte_show_help("help-orte-top.txt", "orte-top:hnp-file-bad", true, filename);
                orte_finalize();
                exit(1);
            }
            fclose(fp);
            input[strlen(input)-1] = '\0';  /* remove newline */
            /* construct the target hnp info */
            target_hnp = OBJ_NEW(orte_hnp_contact_t);
            target_hnp->rml_uri = strdup(input);
        } else {
            /* should just be the uri itself - construct the target hnp info */
            target_hnp = OBJ_NEW(orte_hnp_contact_t);
            target_hnp->rml_uri = strdup(hnpuristr);
        }
        /* set the info in our contact table */
        if (ORTE_SUCCESS != orte_rml.set_contact_info(target_hnp->rml_uri)) {
            orte_show_help("help-orte-top.txt", "orte-top:hnp-uri-bad", true, target_hnp->rml_uri);
            orte_finalize();
            exit(1);
        }
        /* extract the name */
        if (ORTE_SUCCESS != orte_rml_base_parse_uris(target_hnp->rml_uri, &target_hnp->name, NULL)) {
            orte_show_help("help-orte-top.txt", "orte-top:hnp-uri-bad", true, target_hnp->rml_uri);
            orte_finalize();
            exit(1);
        }
        /* set the route to be direct */
        if (ORTE_SUCCESS != orte_routed.update_route(&target_hnp->name, &target_hnp->name)) {
            orte_show_help("help-orte-top.txt", "orte-top:hnp-uri-bad", true, target_hnp->rml_uri);
            orte_finalize();
            exit(1);
        }
    } else {
        orte_show_help("help-orte-top.txt", "orte-top:no-contact-given", true);
        orte_finalize();
        exit(1);
    }
    
    /* set the target hnp as our lifeline so we will terminate if it exits */
    orte_routed.set_lifeline(&target_hnp->name);
    
    /* if an output file was specified, open it */
    if (NULL != logfile) {
        fp = fopen(logfile, "w");
        if (NULL == fp) {
            orte_show_help("help-orte-top.txt", "orte-top:cant-open-logfile", true, logfile);
            orte_finalize();
            exit(1);
        }
    } else {
        fp = stdout;
    }
    
    /* setup a non-blocking recv to get answers - we don't know how
     * many daemons are going to send replies, so we just have to
     * accept whatever comes back
     */
    ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_TOOL,
                                  ORTE_RML_NON_PERSISTENT, recv_stats, NULL);
    if (ret != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }
    
    
    /* setup the command to get the resource usage */
    OBJ_CONSTRUCT(&cmdbuf, opal_buffer_t);
    command = ORTE_DAEMON_TOP_CMD;
    if (ORTE_SUCCESS != (ret = opal_dss.pack(&cmdbuf, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }
    
    proc.jobid = ORTE_PROC_MY_NAME->jobid+1;  /* only support initial launch at this time */

    /* parse the rank list - this can be a comma-separated list of ranks,
     * each element being either a single rank or a range. We also allow
     * for a -1 to indicate all ranks. If not rank is given, we assume -1
     */
    if (NULL == ranks) {
        /* take all ranks */
        proc.vpid = ORTE_VPID_WILDCARD;
        if (ORTE_SUCCESS != (ret = opal_dss.pack(&cmdbuf, &proc, 1, ORTE_NAME))) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }
        goto SEND;
    }
    
    /* split on commas */
    r1 = opal_argv_split(ranks, ',');
    /* for each resulting element, check for range */
    for (i=0; i < opal_argv_count(r1); i++) {
        r2 = opal_argv_split(r1[i], '-');
        if (1 < opal_argv_count(r2)) {
            /* given range - get start and end */
            vstart = strtol(r2[0], NULL, 10);
            vend = strtol(r2[1], NULL, 10);
        } else {
            /* check for wildcard - have to do this here because
             * the -1 would have been caught in the split
             */
            vint = strtol(r1[i], NULL, 10);
            if (-1 == vint) {
                proc.vpid = ORTE_VPID_WILDCARD;
                if (ORTE_SUCCESS != (ret = opal_dss.pack(&cmdbuf, &proc, 1, ORTE_NAME))) {
                    ORTE_ERROR_LOG(ret);
                    goto cleanup;
                }
                opal_argv_free(r2);
                goto SEND;
            }
            vstart = strtol(r2[0], NULL, 10);
            vend = vstart + 1;
        }
        for (proc.vpid = vstart; proc.vpid < vend; proc.vpid++) {
            if (ORTE_SUCCESS != (ret = opal_dss.pack(&cmdbuf, &proc, 1, ORTE_NAME))) {
                ORTE_ERROR_LOG(ret);
                goto cleanup;
            }
        }
        opal_argv_free(r2);
    }
    
SEND:
    if (NULL != r1) {
        opal_argv_free(r1);
    }
    send_cmd(0, 0, NULL);

    /* now wait until the termination event fires */
    opal_event_dispatch();

    /***************
     * Cleanup
     ***************/
cleanup:
    /* Remove the TERM and INT signal handlers */
    opal_signal_del(&term_handler);
    opal_signal_del(&int_handler);

    while (NULL != (item  = opal_list_remove_first(&recvd_stats))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&recvd_stats);
    OBJ_DESTRUCT(&cmdbuf);
    if (NULL != fp && fp != stdout) {
        fclose(fp);
    }
    orte_finalize();
    
    return ret;
}

static void abort_exit_callback(int fd, short ign, void *arg)
{
    opal_list_item_t *item;
    
    /* Remove the TERM and INT signal handlers */
    opal_signal_del(&term_handler);
    opal_signal_del(&int_handler);
    
    while (NULL != (item  = opal_list_remove_first(&recvd_stats))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&recvd_stats);
    OBJ_DESTRUCT(&cmdbuf);
    if (NULL != fp && fp != stdout) {
        fclose(fp);
    }
    orte_finalize();
    exit(1);
}

static void process_stats(int fd, short event, void *data)
{
    orte_message_event_t *mev = (orte_message_event_t*)data;
    opal_buffer_t *buffer = mev->buffer;
    orte_process_name_t *sender = &(mev->sender);
    int32_t n;
    opal_pstats_t *stats;
    orte_process_name_t proc;
    int ret;

    /* if the sender is the HNP we contacted, this message
     * contains info on the number of responses we should get
     */
    if (sender->vpid == 0) {
        n = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &num_replies, &n, OPAL_INT32))) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }
        n = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &sample_time, &n, OPAL_STRING))) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }
    }
    
    n = 1;
    while (ORTE_SUCCESS == opal_dss.unpack(buffer, &proc, &n, ORTE_NAME)) {
        n = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &stats, &n, OPAL_PSTAT))) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }
        /* if field sizes are not yet set, do so now */
        if (!fields_set) {
            int tmp;
            char *ctmp;
            
            tmp = strlen(stats->node);
            if (nodefield < tmp) {
                nodefield = tmp;
            }
            
            asprintf(&ctmp, "%d", stats->rank);
            tmp = strlen(ctmp);
            free(ctmp);
            if (rankfield < tmp) {
                rankfield = tmp;
            }
            
            asprintf(&ctmp, "%lu", (unsigned long)stats->pid);
            tmp = strlen(ctmp);
            free(ctmp);
            if (pidfield < tmp) {
                pidfield = tmp;
            }
            
            tmp = strlen(stats->cmd);
            if (cmdfield < tmp) {
                cmdfield = tmp;
            }
            
            if (0 <= stats->priority) {
                pri_found = true;
                asprintf(&ctmp, "%d", stats->priority);
                tmp = strlen(ctmp);
                free(ctmp);
                if (prifield < tmp) {
                    prifield = tmp;
                }
            }
            
            if (0 <= stats->num_threads) {
                thr_found = true;
                asprintf(&ctmp, "%d", stats->num_threads);
                tmp = strlen(ctmp);
                free(ctmp);
                if (thrfield < tmp) {
                    thrfield = tmp;
                }
            }
            
            if (0 < stats->vsize) {
                vsize_found = true;
                asprintf(&ctmp, "%lu", (unsigned long)stats->vsize);
                tmp = strlen(ctmp);
                free(ctmp);
                if (vsizefield < tmp) {
                    vsizefield = tmp;
                }
            }
            
            if (0 < stats->rss) {
                rss_found = true;
                asprintf(&ctmp, "%lu", (unsigned long)stats->rss);
                tmp = strlen(ctmp);
                free(ctmp);
                if (rssfield < tmp) {
                    rssfield = tmp;
                }
            }
            
            if (0 < stats->peak_vsize) {
                pkv_found = true;
                asprintf(&ctmp, "%lu", (unsigned long)stats->peak_vsize);
                tmp = strlen(ctmp);
                free(ctmp);
                if (pkvfield < tmp) {
                    pkvfield = tmp;
                }
            }
            
            if (0 < stats->shared_size) {
                sh_found = true;
                asprintf(&ctmp, "%lu", (unsigned long)stats->shared_size);
                tmp = strlen(ctmp);
                free(ctmp);
                if (shfield < tmp) {
                    shfield = tmp;
                }
            }
            
            if (0 <= stats->processor) {
                p_found = true;
                asprintf(&ctmp, "%d", stats->processor);
                tmp = strlen(ctmp);
                free(ctmp);
                if (pfield < tmp) {
                    pfield = tmp;
                }
            }
        }
        /* add it to the list */
        opal_list_append(&recvd_stats, &stats->super);
    }
    
cleanup:
    OBJ_RELEASE(mev);
    
    /* check for completion */
    num_recvd++;
    if (num_replies <= num_recvd) {
        all_recvd = true;
    }

    /* repost the receive */
    ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_TOOL,
                                  ORTE_RML_NON_PERSISTENT, recv_stats, NULL);
    if (ret != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(ret);
    }
}

static void recv_stats(int status, orte_process_name_t* sender,
                       opal_buffer_t *buffer, orte_rml_tag_t tag,
                       void* cbdata)
{
    /* don't process this right away - we need to get out of the recv before
     * we process the message as it may ask us to do something that involves
     * more messaging! Instead, setup an event so that the message gets processed
     * as soon as we leave the recv.
     *
     * The macro makes a copy of the buffer, which we release when processed - the incoming
     * buffer, however, is NOT released here, although its payload IS transferred
     * to the message buffer for later processing
     */
    ORTE_MESSAGE_EVENT(sender, buffer, tag, process_stats);
    
    OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                         "%s recv_stats: reissued recv",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
}

/* static values needed for printing */
static int lennode = 0;
static int lenrank = 0;
static int lenpid = 0;
static int lencmd = 0;
static int lenstate = 0;
static int lentime = 0;
static int lenpri = 0;
static int lenthr = 0;
static int lenvsize = 0;
static int lenrss = 0;
static int lenpkv = 0;
static int lensh = 0;
static int lenp = 0;

static void print_ranks(opal_list_t *statlist)
{
    opal_list_item_t *item;
    opal_pstats_t *stats, *pstats;
    int32_t minrank;
    char pretty_time[10];
    int i;

    /* sort the results by rank */
    while (0 < opal_list_get_size(statlist)) {
        minrank = INT32_MAX;
        pstats = NULL;
        for (item = opal_list_get_first(statlist);
             item != opal_list_get_end(statlist);
             item = opal_list_get_next(item)) {
            stats = (opal_pstats_t*)item;
            if (stats->rank < minrank) {
                pstats = stats;
                minrank = stats->rank;
            }
        }
        memset(pretty_time, 0, sizeof(pretty_time));
        if (pstats->time >= 3600) {
            snprintf(pretty_time, sizeof(pretty_time), "%5.1fH", 
                     (double)pstats->time / (double)(3600));
        } else {
            snprintf(pretty_time, sizeof(pretty_time), "%3ld:%02ld",
                     (unsigned long)pstats->time/60,
                     (unsigned long)pstats->time & 60);
        }
        
        if (bynode) {
            /* print blanks in the nodename field */
            for (i=0; i < lennode; i++) {
                fprintf(fp, " ");
            }
            fprintf(fp, " | ");
            /* print fields */
            fprintf(fp, "%*d | ", lenrank, pstats->rank);
        } else {
            fprintf(fp, "%*d | ", lenrank, pstats->rank);
            fprintf(fp, "%*s | ", lennode, pstats->node);
        }
        fprintf(fp, "%*s | ", lencmd, pstats->cmd);
        fprintf(fp, "%*lu | ", lenpid, (unsigned long)pstats->pid);
        fprintf(fp, "%*c | ", lenstate, pstats->state);
        fprintf(fp, "%*s | ", lentime, pretty_time);
        if (pri_found) {
            fprintf(fp, "%*d | ", lenpri, pstats->priority);
        }
        if (thr_found) {
            fprintf(fp, "%*d | ", lenthr, pstats->num_threads);
        }
        if (vsize_found) {
            fprintf(fp, "%*lu | ", lenvsize, (unsigned long)pstats->vsize);
        }
        if (rss_found) {
            fprintf(fp, "%*lu | ", lenvsize, (unsigned long)pstats->rss);
        }
        if (pkv_found) {
            fprintf(fp, "%*lu | ", lenpkv, (unsigned long)pstats->peak_vsize);
        }
        if (sh_found) {
            fprintf(fp, "%*lu | ", lensh, (unsigned long)pstats->shared_size);
        }
        if (p_found) {
            fprintf(fp, "%*d | ", lenp, pstats->processor);
        }
        fprintf(fp, "\n");
        num_lines++;
        opal_list_remove_item(statlist, &pstats->super);
        OBJ_RELEASE(pstats);
    }
}

static void pretty_print(void)
{
    opal_list_item_t *item, *next;
    opal_pstats_t *stats;
    opal_list_t tmplist;
    char *node;
    
    if (bynode) {
        if (need_header) {
            print_headers();
            need_header = false;
        }
        if (timestamp) {
            fprintf(fp, "TIMESTAMP: %s\n", sample_time);
        }
        if (NULL != sample_time) {
            free(sample_time);
            sample_time = NULL;
        }
        /* sort the results by node and then rank */
        while (NULL != (item = opal_list_remove_first(&recvd_stats))) {
            OBJ_CONSTRUCT(&tmplist, opal_list_t);
            stats = (opal_pstats_t*)item;
            node = strdup(stats->node);
            opal_list_append(&tmplist, &stats->super);
            /* cycle through the rest of the list looking
             * for matching nodes
             */
            item = opal_list_get_first(&recvd_stats);
            while (item != opal_list_get_end(&recvd_stats)) {
                stats = (opal_pstats_t*)item;
                next = opal_list_get_next(item);
                if (0 == strcmp(stats->node, node)) {
                    opal_list_remove_item(&recvd_stats, item);
                    opal_list_append(&tmplist, &stats->super);
                }
                item = next;
            }
            fprintf(fp, "%*s\n",  lennode, node);
            free(node);
            print_ranks(&tmplist);
            OBJ_DESTRUCT(&tmplist);
        }
    } else {
        if (need_header) {
            print_headers();
            need_header = false;
        }
        if (timestamp) {
            fprintf(fp, "\n\nTIMESTAMP: %s\n", sample_time);
        }
        if (NULL != sample_time) {
            free(sample_time);
            sample_time = NULL;
        }
        print_ranks(&recvd_stats);
    }
    
    /* provide some separation between iterations */
    fprintf(fp, "\n");
    
    /* if we have printed more than MAX_LINES since the last header,
     * flag that we need to print the header next time
     */
    if (MAX_LINES < num_lines) {
        need_header = true;
        num_lines = 0;
        fprintf(fp, "\n\n");
    }
}

static void print_headers(void)
{
    int num_fields = 0;
    int i;
    int linelen;
    
    lennode = strlen("Nodename");
    if (nodefield > lennode) {
        lennode = nodefield;
    }
    num_fields++;
    
    lenrank = strlen("Rank");
    if (rankfield > lenrank) {
        lenrank = rankfield;
    }
    num_fields++;

    lenpid = strlen("Pid");
    if (pidfield > lenpid) {
        lenpid = pidfield;
    }
    num_fields++;

    lencmd = strlen("Command");
    if (cmdfield > lencmd) {
        lencmd = cmdfield;
    }
    num_fields++;

    lenstate = strlen("State");
    num_fields++;

    lentime = strlen("Time");
    if (timefield > lentime) {
        lentime = timefield;
    }
    num_fields++;

    if (pri_found) {
        lenpri = strlen("Pri");
        if (prifield > lenpri) {
            lenpri = prifield;
        }
        num_fields++;
    }
    
    if (thr_found) {
        lenthr = strlen("#threads");
        if (thrfield > lenthr) {
            lenthr = thrfield;
        }
        num_fields++;
    }
    
    if (vsize_found) {
        lenvsize = strlen("Vsize");
        if (vsizefield > lenvsize) {
            lenvsize = vsizefield;
        }
        num_fields++;
    }
    
    if (rss_found) {
        lenrss = strlen("RSS");
        if (rssfield > lenrss) {
            lenrss = rssfield;
        }
        num_fields++;
    }
    
    if (pkv_found) {
        lenpkv = strlen("Peak Vsize");
        if (pkvfield > lenpkv) {
            lenpkv = pkvfield;
        }
        num_fields++;
    }

    if (sh_found) {
        lensh = strlen("Shr Size");
        if (shfield > lensh) {
            lensh = shfield;
        }
        num_fields++;
    }

    if (p_found) {
        lenp = strlen("Processor");
        if (pfield > lenp) {
            lenp = pfield;
        }
        num_fields++;
    }
    
    linelen = lennode + lenrank + lenpid + lencmd + lenstate + lentime + lenpri + lenthr + lenvsize + lenrss + lenpkv + lensh + lenp;
    /* add spacing */
    linelen += num_fields * 3;
    
    /* print the rip line */
    for(i = 0; i < linelen; ++i) {
        fprintf(fp, "=");
    }
    fprintf(fp, "\n");
    
    /* print the header */
    if (bynode) {
        fprintf(fp, "%*s | ", lennode   , "Nodename");
        fprintf(fp, "%*s | ", lenrank   , "Rank");
    } else {
        fprintf(fp, "%*s | ", lenrank   , "Rank");
        fprintf(fp, "%*s | ", lennode   , "Nodename");
    }
    fprintf(fp, "%*s | ", lencmd    , "Command");
    fprintf(fp, "%*s | ", lenpid    , "Pid");
    fprintf(fp, "%*s | ", lenstate  , "State");
    fprintf(fp, "%*s | ", lentime   , "Time");
    if (pri_found) {
        fprintf(fp, "%*s | ", lenpri   , "Pri");
    }
    if (thr_found) {
        fprintf(fp, "%*s | ", lenthr   , "#threads");
    }
    if (vsize_found) {
        fprintf(fp, "%*s | ", lenvsize   , "Vsize");
    }
    if (rss_found) {
        fprintf(fp, "%*s | ", lenrss   , "RSS");
    }
    if (pkv_found) {
        fprintf(fp, "%*s | ", lenpkv   , "Peak Vsize");
    }
    if (sh_found) {
        fprintf(fp, "%*s | ", lensh   , "Shr Size");
    }
    if (p_found) {
        fprintf(fp, "%*s | ", lenp   , "Processor");
    }
    fprintf(fp, "\n");
    
    /* print the separator */
    for(i = 0; i < linelen; ++i) {
        fprintf(fp, "-");
    }
    fprintf(fp, "\n");
    
}
