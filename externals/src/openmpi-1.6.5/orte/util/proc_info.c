/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"

#include "orte/util/proc_info.h"

ORTE_DECLSPEC orte_proc_info_t orte_process_info = {
    /*  .my_name =              */   {ORTE_JOBID_INVALID, ORTE_VPID_INVALID},
    /*  .my_daemon =            */   {ORTE_JOBID_INVALID, ORTE_VPID_INVALID},
    /*  .my_daemon_uri =        */   NULL,
    /*  .my_hnp =               */   {ORTE_JOBID_INVALID, ORTE_VPID_INVALID},
    /*  .my_hnp_uri =           */   NULL,
    /*  .hnp_pid =              */    0,
    /*  .app_num =              */   -1,
    /*  .num_procs =            */   1,
    /*  .num_nodes =            */   1,
    /*  .nodename =             */   NULL,
    /*  .pid =                  */   0,
    /*  .proc_type =            */   ORTE_PROC_TYPE_NONE,
    /*  .sync_buf =             */   NULL,
    /*  .my_port =              */   0,
    /*  .num_restarts =         */   0,
    /*  .tmpdir_base =          */   NULL,
    /*  .top_session_dir =      */   NULL,
    /*  .job_session_dir =      */   NULL,
    /*  .proc_session_dir =     */   NULL,
    /*  .sock_stdin =           */   NULL,
    /*  .sock_stdout =          */   NULL,
    /*  .sock_stderr =          */   NULL
};

static bool init=false;

int orte_proc_info(void)
{
    
    int tmp;
    char *uri, *ptr;
    char hostname[ORTE_MAX_HOSTNAME_SIZE];
    
    if (init) {
        return ORTE_SUCCESS;
    }
    init = true;
    
    mca_base_param_reg_string_name("orte", "hnp_uri",
                                   "HNP contact info",
                                   true, false, NULL,  &uri);
    if (NULL != uri) {
        /* the uri value passed to us will have quote marks around it to protect
        * the value if passed on the command line. We must remove those
        * to have a correct uri string
        */
        if ('"' == uri[0]) {
            /* if the first char is a quote, then so will the last one be */
            uri[strlen(uri)-1] = '\0';
            ptr = &uri[1];
        } else {
            ptr = &uri[0];
        }
        orte_process_info.my_hnp_uri = strdup(ptr);
        free(uri);
    }
    
    mca_base_param_reg_string_name("orte", "local_daemon_uri",
                                   "Daemon contact info",
                                   true, false, NULL,  &(uri));
    
    if (NULL != uri) {
        /* the uri value passed to us may have quote marks around it to protect
         * the value if passed on the command line. We must remove those
         * to have a correct uri string
         */
        if ('"' == uri[0]) {
            /* if the first char is a quote, then so will the last one be */
            uri[strlen(uri)-1] = '\0';
            ptr = &uri[1];
        } else {
            ptr = &uri[0];
        }
        orte_process_info.my_daemon_uri = strdup(ptr);
        free(uri);
    }
    
    mca_base_param_reg_int_name("orte", "app_num",
                                "Index of the app_context that defines this proc",
                                true, false, -1, &tmp);
    orte_process_info.app_num = tmp;
    
    /* get the process id */
    orte_process_info.pid = getpid();

    /* get the nodename */
    gethostname(hostname, ORTE_MAX_HOSTNAME_SIZE);
    orte_process_info.nodename = strdup(hostname);
    
    /* get the number of nodes in the job */
    mca_base_param_reg_int_name("orte", "num_nodes",
                                "Number of nodes in the job",
                                true, false,
                                orte_process_info.num_nodes, &tmp);
    orte_process_info.num_nodes = tmp;
    
    /* get the number of times this proc has restarted */
    mca_base_param_reg_int_name("orte", "num_restarts",
                                "Number of times this proc has restarted",
                                true, false, 0, &tmp);
    orte_process_info.num_restarts = tmp;
    
    /* setup the sync buffer */
    orte_process_info.sync_buf = OBJ_NEW(opal_buffer_t);
    
    return ORTE_SUCCESS;
}


int orte_proc_info_finalize(void)
{
    if (!init) {
        return ORTE_SUCCESS;
    }
    
    if (NULL != orte_process_info.tmpdir_base) {
        free(orte_process_info.tmpdir_base);
        orte_process_info.tmpdir_base = NULL;
    }
    
    if (NULL != orte_process_info.top_session_dir) {
        free(orte_process_info.top_session_dir);
        orte_process_info.top_session_dir = NULL;
    }
 
    if (NULL != orte_process_info.job_session_dir) {
        free(orte_process_info.job_session_dir);
        orte_process_info.job_session_dir = NULL;
    }
    
    if (NULL != orte_process_info.proc_session_dir) {
        free(orte_process_info.proc_session_dir);
        orte_process_info.proc_session_dir = NULL;
    }
    
    if (NULL != orte_process_info.nodename) {
        free(orte_process_info.nodename);
        orte_process_info.nodename = NULL;
    }

    if (NULL != orte_process_info.sock_stdin) {
        free(orte_process_info.sock_stdin);
        orte_process_info.sock_stdin = NULL;
    }
    
    if (NULL != orte_process_info.sock_stdout) {
        free(orte_process_info.sock_stdout);
        orte_process_info.sock_stdout = NULL;
    }
    
    if (NULL != orte_process_info.sock_stderr) {
        free(orte_process_info.sock_stderr);
        orte_process_info.sock_stderr = NULL;
    }

    if (NULL != orte_process_info.my_hnp_uri) {
        free(orte_process_info.my_hnp_uri);
        orte_process_info.my_hnp_uri = NULL;
    }

    if (NULL != orte_process_info.my_daemon_uri) {
        free(orte_process_info.my_daemon_uri);
        orte_process_info.my_daemon_uri = NULL;
    }

    orte_process_info.proc_type = ORTE_PROC_TYPE_NONE;
    
    OBJ_RELEASE(orte_process_info.sync_buf);
    orte_process_info.sync_buf = NULL;

    init = false;
    return ORTE_SUCCESS;
}
