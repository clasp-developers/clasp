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
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <string.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */
#ifdef HAVE_SYSLOG_H
#include <syslog.h>
#endif
#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#include "opal/util/show_help.h"

#include "orte/mca/ess/ess.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/notifier/base/base.h"
#include "notifier_syslog.h"


/* Static API's */
static int init(void);
static void finalize(void);
static void mylog(int severity, int errcode, const char *msg, ...);
static void myhelplog(int severity, int errcode, const char *filename, const char *topic, ...);
static void mypeerlog(int severity, int errcode, orte_process_name_t *peer_proc, const char *msg, ...);

/* Module def */
orte_notifier_base_module_t orte_notifier_syslog_module = {
    init,
    finalize,
    mylog,
    myhelplog,
    mypeerlog
};


static int init(void) {
    int opts;
    
    opts = LOG_CONS | LOG_PID | LOG_SYSLOG;
    openlog("Open MPI Error Report:", opts, LOG_USER);
    
    return ORTE_SUCCESS;
}

static void finalize(void) {
    closelog();
}

static void mylog(int severity, int errcode, const char *msg, ...)
{
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
    vsyslog(severity, msg, arglist);
    va_end(arglist);
}

static void myhelplog(int severity, int errcode, const char *filename, const char *topic, ...)
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
    
    /* if nothing came  back, then nothing to do */
    if (NULL == output) {
        return;
    }
    
    /* go ahead and output it */
    syslog(severity, output, NULL);
    free(output);
}

static void mypeerlog(int severity, int errcode, orte_process_name_t *peer_proc, const char *msg, ...)
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

    buf[ORTE_NOTIFIER_MAX_BUF] = '\0'; /* not needed? */
    syslog(severity, buf, NULL);
}
