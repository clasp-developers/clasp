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
 * Copyright (c) 2009      Cisco Systems, Inc. All rights reserved.
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

#include "opal/util/show_help.h"
#include "opal/util/argv.h"

#include "orte/constants.h"
#include "orte/mca/ess/ess.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/notifier/base/base.h"

#include "notifier_smtp.h"


/* Static API's */
static void mylog(int severity, int errcode, const char *msg, ...);
static void myhelplog(int severity, int errcode, const char *filename, 
                      const char *topic, ...);
static void mypeerlog(int severity, int errcode, orte_process_name_t *peer_proc,
                      const char *msg, ...);

/* Module */
orte_notifier_base_module_t orte_notifier_smtp_module = {
    NULL,
    NULL,
    mylog,
    myhelplog,
    mypeerlog
};

typedef enum {
    SENT_NONE,
    SENT_HEADER,
    SENT_BODY_PREFIX,
    SENT_BODY,
    SENT_BODY_SUFFIX,
    SENT_ALL
} sent_flag_t;

typedef struct {
    sent_flag_t sent_flag;
    char *msg;
    char *prev_string;
} message_status_t;

/*
 * Convert lone \n's to \r\n
 */
static char *crnl(char *orig)
{
    int i, j, max, count;
    char *str;
    return strdup(orig);

    /* Count how much space we need */
    count = max = strlen(orig);
    for (i = 0; i < max; ++i) {
        if (orig[i] == '\n' && i > 0 && orig[i - 1] != '\r') {
            ++count;
        }
    }

    /* Copy, changing \n to \r\n */
    str = malloc(count + 1);
    for (j = i = 0; i < max; ++i) {
        if (orig[i] == '\n' && i > 0 && orig[i - 1] != '\r') {
            str[j++] = '\n';
        }
        str[j++] = orig[i];
    }
    str[j] = '\0';
    return str;
}

/*
 * Callback function invoked via smtp_start_session()
 */
static const char *message_cb(void **buf, int *len, void *arg)
{
    message_status_t *ms = (message_status_t*) arg;

    if (NULL == *buf) {
        *buf = malloc(8192);
    }
    if (NULL == len) {
        ms->sent_flag = SENT_NONE;
        return NULL;
    }

    /* Free the previous string */
    if (NULL != ms->prev_string) {
        free(ms->prev_string);
        ms->prev_string = NULL;
    }

    switch (ms->sent_flag) {
    case SENT_NONE:
        /* Send a blank line to signify the end of the header */
        ms->sent_flag = SENT_HEADER;
        ms->prev_string = NULL;
        *len = 2;
        return "\r\n";

    case SENT_HEADER:
        if (NULL != mca_notifier_smtp_component.body_prefix) {
            ms->sent_flag = SENT_BODY_PREFIX;
            ms->prev_string = crnl(mca_notifier_smtp_component.body_prefix);
            *len = strlen(ms->prev_string);
            return ms->prev_string;
        }

    case SENT_BODY_PREFIX:
        ms->sent_flag = SENT_BODY;
        ms->prev_string = crnl(ms->msg);
        *len = strlen(ms->prev_string);
        return ms->prev_string;

    case SENT_BODY:
        if (NULL != mca_notifier_smtp_component.body_suffix) {
            ms->sent_flag = SENT_BODY_SUFFIX;
            ms->prev_string = crnl(mca_notifier_smtp_component.body_suffix);
            *len = strlen(ms->prev_string);
            return ms->prev_string;
        }

    case SENT_BODY_SUFFIX:
    case SENT_ALL:
    default:
        ms->sent_flag = SENT_ALL;
        *len = 0;
        return NULL;
    }
}

/*
 * Back-end function to actually send the email
 */
static int send_email(char *msg)
{
    int i, err = ORTE_SUCCESS;
    char *str = NULL;
    char *errmsg = NULL;
    struct sigaction sig, oldsig;
    bool set_oldsig = false;
    smtp_session_t session = NULL;
    smtp_message_t message = NULL;
    message_status_t ms;
    orte_notifier_smtp_component_t *c = &mca_notifier_smtp_component;

    if (NULL == c->to_argv) {
        c->to_argv = opal_argv_split(c->to, ',');
        if (NULL == c->to_argv ||
            NULL == c->to_argv[0]) {
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
    }

    ms.sent_flag = SENT_NONE;
    ms.prev_string = NULL;
    ms.msg = msg;

    /* Temporarily disable SIGPIPE so that if remote servers timeout
       or hang up on us, it doesn't kill this application.  We'll
       restore the original SIGPIPE handler when we're done. */
    sig.sa_handler = SIG_IGN;
    sigemptyset(&sig.sa_mask);
    sig.sa_flags = 0;
    sigaction(SIGPIPE, &sig, &oldsig); 
    set_oldsig = true;

    /* Try to get a libesmtp session.  If so, assume that libesmtp is
       happy and proceeed */
    session = smtp_create_session();
    if (NULL == session) {
        err = ORTE_ERR_NOT_SUPPORTED;
        errmsg = "stmp_create_session";
        goto error;
    }

    /* Create the message */
    message = smtp_add_message(session);
    if (NULL == message) {
        err = ORTE_ERROR;
        errmsg = "stmp_add_message";
        goto error;
    }

    /* Set the SMTP server (yes, it's a weird return status!) */
    asprintf(&str, "%s:%d", c->server, c->port);
    if (0 == smtp_set_server(session, str)) {
        err = ORTE_ERROR;
        errmsg = "stmp_set_server";
        goto error;
    }
    free(str);
    str = NULL;

    /* Add the sender */
    if (0 == smtp_set_reverse_path(message, c->from_addr)) {
        err = ORTE_ERROR;
        errmsg = "stmp_set_reverse_path";
        goto error;
    }

    /* Set the subject and some headers */
    asprintf(&str, "Open MPI SMTP Notifier v%d.%d.%d",
             c->super.base_version.mca_component_major_version,
             c->super.base_version.mca_component_minor_version,
             c->super.base_version.mca_component_release_version);
    if (0 == smtp_set_header(message, "Subject", c->subject) ||
        0 == smtp_set_header_option(message, "Subject", Hdr_OVERRIDE, 1) ||
        0 == smtp_set_header(message, "To", NULL, NULL) ||
        0 == smtp_set_header(message, "From", 
                             (NULL != c->from_name ? 
                              c->from_name : c->from_addr),
                             c->from_addr) ||
        0 == smtp_set_header(message, "X-Mailer", str) ||
        0 == smtp_set_header_option(message, "Subject", Hdr_OVERRIDE, 1)) {
        err = ORTE_ERROR;
        errmsg = "smtp_set_header";
        goto error;
    }
    free(str);
    str = NULL;

    /* Add the recipients */
    for (i = 0; NULL != c->to_argv[i]; ++i) {
        if (NULL == smtp_add_recipient(message, c->to_argv[i])) {
            err = ORTE_ERR_OUT_OF_RESOURCE;
            errmsg = "stmp_add_recipient";
            goto error;
        }
    }

    /* Set the callback to get the message */
    if (0 == smtp_set_messagecb(message, message_cb, &ms)) {
        err = ORTE_ERROR;
        errmsg = "smtp_set_messagecb";
        goto error;
    }

    /* Send it! */
    if (0 == smtp_start_session(session)) {
        err = ORTE_ERROR;
        errmsg = "smtp_start_session";
        goto error;
    }

    /* Fall through */

 error:
    if (NULL != str) {
        free(str);
    }
    if (NULL != session) {
        smtp_destroy_session(session);
    }
    /* Restore the SIGPIPE handler */
    if (set_oldsig) {
        sigaction(SIGPIPE, &oldsig, NULL);
    }
    if (ORTE_SUCCESS != err) {
        int e;
        char em[256];

        e = smtp_errno();
        smtp_strerror(e, em, sizeof(em));
        orte_show_help("help-orte-notifier-smtp.txt", 
                       "send_email failed",
                       true, "libesmtp library call failed", 
                       errmsg, em, e, msg);
    }
    return err;
}

static void mylog(int severity, int errcode, const char *msg, ...)
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
        send_email(output);
        free(output);
    }
}

static void myhelplog(int severity, int errcode, const char *filename, 
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
        send_email(output);
        free(output);
    }
}

static void mypeerlog(int severity, int errcode, 
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
    send_email(buf);
}
