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
 * Copyright (c) 2007-2009 Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009-2010 Cisco Systems, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Send an email upon notifier events.
 */

#include "orte_config.h"

#include <stdlib.h>
#include <sys/wait.h>
#include <stdio.h>
#include <string.h>
#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#include "opal/util/show_help.h"
#include "opal/util/error.h"

#include "orte/constants.h"
#include "orte/mca/ess/ess.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/notifier/base/base.h"

#include "notifier_command.h"


static void command_log(int severity, int errcode, const char *msg, ...);
static void command_help(int severity, int errcode, const char *filename, 
                       const char *topic, ...);
static void command_peer(int severity, int errcode, 
                       orte_process_name_t *peer_proc,
                       const char *msg, ...);

/* Module */
orte_notifier_base_module_t orte_notifier_command_module = {
    NULL,
    NULL,
    command_log,
    command_help,
    command_peer
};

/*
 * Back-end function to actually tell the child to fork the command
 */
static int send_command(int severity, int errcode, char *msg)
{
    /* csel = Command, Severity, Errcode, string Length */
    int rc, csel[4];
    csel[0] = CMD_EXEC;
    csel[1] = severity;
    csel[2] = errcode;
    csel[3] = strlen(msg);

    /* Write the severity, errcode, and string length */
    if (ORTE_SUCCESS != 
        (rc = orte_notifier_command_write_fd(mca_notifier_command_component.to_child[1], 
                                           sizeof(csel), csel))) {
        goto error;
    }

    /* Now write the message itself */
    if (ORTE_SUCCESS != 
        (rc = orte_notifier_command_write_fd(mca_notifier_command_component.to_child[1], 
                                           csel[3] + 1, msg))) {
        goto error;
    }

    /* Now read back the grandchild's exit status from the child:
       0 = 0/1 indicating whether the grandchild exited or not
       1 = 0/1 indicating whether the grandchild timed out/was killed or not
       2 = exit status returned by waitpid() (only relevant if exited==1) */
    if (ORTE_SUCCESS != 
        (rc = orte_notifier_command_read_fd(mca_notifier_command_component.to_parent[0], 
                                            sizeof(int) * 3, csel))) {
        goto error;
    }
    /* Did the grandchild exit? */
    if (0 == csel[0]) {
        orte_show_help("help-orte-notifier-command.txt", 
                       "grandchild did not exit", true, 
                       orte_process_info.nodename,
                       mca_notifier_command_component.cmd, 
                       mca_notifier_command_component.timeout);
        return ORTE_ERROR;
    }
    /* Did the grandchild timeout? */
    if (1 == csel[1]) {
        orte_show_help("help-orte-notifier-command.txt", 
                       "grandchild timeout", true, 
                       orte_process_info.nodename,
                       mca_notifier_command_component.cmd,
                       mca_notifier_command_component.timeout,
                       WIFEXITED(csel[0]) ? "Exit status" : "Signal",
                       WIFEXITED(csel[0]) ? WEXITSTATUS(csel[0]) : WTERMSIG(csel[0]));
        return ORTE_ERR_TIMEOUT;
    }

    /* The grandchild exited in less than the timeout -- yay.  Did it
       exit cleanly? */
    if (WIFEXITED(csel[1]) && 0 == WEXITSTATUS(csel[1])) {
        return ORTE_SUCCESS;
    }

    /* Nope -- didn't exit cleanly, so print a warning. */
    orte_show_help("help-orte-notifier-command.txt", 
                   "grandchild fail", true, orte_process_info.nodename,
                   mca_notifier_command_component.cmd,
                   WIFEXITED(csel[0]) ? "Exit status" : "Signal",
                   WIFEXITED(csel[0]) ? WEXITSTATUS(csel[0]) : WTERMSIG(csel[0]));
    return ORTE_ERROR;
    
 error:
    orte_show_help("help-orte-notifier-command.txt", 
                   "system call fail", true, orte_process_info.nodename,
                   "write", opal_strerror(rc), rc);
    return rc;
}

static void command_log(int severity, int errcode, const char *msg, ...)
{
    char *output;
    va_list arglist;

    /* is the severity value above the threshold - I know
     * this seems backward, but lower severity values are
     * considered "more severe"
     */
    if (severity > orte_notifier_threshold_severity) {
        return;
    }
    
    /* If there was a message, output it */
    va_start(arglist, msg);
    vasprintf(&output, msg, arglist);
    va_end(arglist);

    if (NULL != output) {
        send_command(severity, errcode, output);
        free(output);
    }
}

static void command_help(int severity, int errcode, const char *filename, 
                       const char *topic, ...)
{
    va_list arglist;
    char *output;
    
    /* is the severity value above the threshold - I know
     * this seems backward, but lower severity values are
     * considered "more severe"
     */
    if (severity > orte_notifier_threshold_severity) {
        return;
    }
    
    va_start(arglist, topic);
    output = opal_show_help_vstring(filename, topic, false, arglist);
    va_end(arglist);
    
    if (NULL != output) {
        send_command(severity, errcode, output);
        free(output);
    }
}

static void command_peer(int severity, int errcode, 
                       orte_process_name_t *peer_proc, const char *msg, ...)
{
    va_list arglist;
    char buf[ORTE_NOTIFIER_MAX_BUF + 1];
    char *peer_host = NULL, *peer_name = NULL;
    char *pos = buf;
    char *errstr = (char*)orte_err2str(errcode);
    int len, space = ORTE_NOTIFIER_MAX_BUF;

    /* is the severity value above the threshold - I know
     * this seems backward, but lower severity values are
     * considered "more severe"
     */
    if (severity > orte_notifier_threshold_severity) {
        return;
    }
    
    if (peer_proc) {
        peer_host = orte_ess.proc_get_hostname(peer_proc);
        peer_name = ORTE_NAME_PRINT(peer_proc);
    }

    len = snprintf(pos, space,
                   "While communicating to proc %s on node %s,"
                   " proc %s on node %s encountered an error ",
                   peer_name ? peer_name : "UNKNOWN",
                   peer_host ? peer_host : "UNKNOWN",
                   ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                   orte_process_info.nodename);
    space -= len;
    pos += len;
    
    if (0 < space) {
        if (errstr) {
            len = snprintf(pos, space, "'%s':", errstr);
        } else {
            len = snprintf(pos, space, "(%d):", errcode);
        }
        space -= len;
        pos += len;
    }

    if (0 < space) {
        va_start(arglist, msg);
        vsnprintf(pos, space, msg, arglist);
        va_end(arglist);
    }

    buf[ORTE_NOTIFIER_MAX_BUF] = '\0';
    send_command(severity, errcode, buf);
}
