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
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "orte/mca/notifier/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/notifier/base/static-components.h"

static void orte_base_log(int severity, int errcode, const char *msg, ...) __opal_attribute_format__(__printf__, 3, 4);
static void orte_log_show_help(int severity, int errcode, const char *file, const char *topic, ...);
static void orte_log_peer(int severity, int errcode, orte_process_name_t *peer_proc, const char *msg, ...) __opal_attribute_format__(__printf__, 4, 5);

/*
 * Global variables
 */
int orte_notifier_base_output = -1;
int orte_notifier_threshold_severity = ORTE_NOTIFIER_ERROR;
orte_notifier_base_module_t orte_notifier = {
    NULL,
    NULL,
    orte_base_log,
    orte_log_show_help,
    orte_log_peer
};
opal_list_t mca_notifier_base_components_available;
orte_notifier_base_component_t mca_notifier_base_selected_component;


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_notifier_base_open(void)
{
    char *level;
    
    /* Debugging / verbose output.  Always have stream open, with
       verbose set by the mca open system... */
    orte_notifier_base_output = opal_output_open(NULL);
    
    /* let the user define a base level of severity to report */
    mca_base_param_reg_string_name("notifier", "threshold_severity",
                                   "Report all events at or above this severity [default: critical]",
                                   false, false, "critical", &level);
    if (0 == strncasecmp(level, "emerg", strlen("emerg"))) {
        orte_notifier_threshold_severity = ORTE_NOTIFIER_EMERG;
    } else if (0 == strncasecmp(level, "alert", strlen("alert"))) {
        orte_notifier_threshold_severity = ORTE_NOTIFIER_ALERT;
    } else if (0 == strncasecmp(level, "crit", strlen("crit"))) {
        orte_notifier_threshold_severity = ORTE_NOTIFIER_CRIT;
    } else if (0 == strncasecmp(level, "warn", strlen("warn"))) {
        orte_notifier_threshold_severity = ORTE_NOTIFIER_WARN;
    } else if (0 == strncasecmp(level, "notice", strlen("notice"))) {
        orte_notifier_threshold_severity = ORTE_NOTIFIER_NOTICE;
    } else if (0 == strncasecmp(level, "info", strlen("info"))) {
        orte_notifier_threshold_severity = ORTE_NOTIFIER_INFO;
    } else if (0 == strncasecmp(level, "debug", strlen("debug"))) {
        orte_notifier_threshold_severity = ORTE_NOTIFIER_DEBUG;
    } else if (0 != strncasecmp(level, "error", strlen("error"))) {
        opal_output(0, "Unknown notifier level");
        return ORTE_ERROR;
    }
    
    /* Open up all available components */

    if (ORTE_SUCCESS !=
        mca_base_components_open("notifier", orte_notifier_base_output,
                                 mca_notifier_base_static_components,
                                 &mca_notifier_base_components_available, true)) {
        return ORTE_ERROR;
    }

    /* All done */

    return ORTE_SUCCESS;
}

static void orte_base_log(int severity, int errcode, const char *msg, ...)
{
    /* just do nothing - it is here just so someone calling it won't
     * segv.  Put in va_start/va_end just so that compilers won't
     * complain.
     */
    va_list ap;
    va_start(ap, msg);
    va_end(ap);
}

static void orte_log_show_help(int severity, int errcode, const char *file, const char *topic, ...)
{
    /* just do nothing - it is here just so someone calling it won't
     * segv.  Put in va_start/va_end just so that compilers won't
     * complain.
     */
    va_list ap;
    va_start(ap, topic);
    va_end(ap);
}

static void orte_log_peer(int severity, int errcode, orte_process_name_t *peer_proc, const char *msg, ...)
{
    /* just do nothing - it is here just so someone calling it won't
     * segv.  Put in va_start/va_end just so that compilers won't
     * complain.
     */
    va_list ap;
    va_start(ap, msg);
    va_end(ap);
}
