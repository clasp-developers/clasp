/* -*- C -*-
*
* Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
*                         University Research and Technology
*                         Corporation.  All rights reserved.
* Copyright (c) 2004-2005 The University of Tennessee and The University
*                         of Tennessee Research Foundation.  All rights
*                         reserved.
* Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
*                         University of Stuttgart.  All rights reserved.
* Copyright (c) 2004-2005 The Regents of the University of California.
*                         All rights reserved.
 * Copyright (c) 2009     Cisco Systems, Inc. All rights reserved.
* $COPYRIGHT$
*
* Additional copyrights may follow
*
* $HEADER$
*/

/*
 * Simple smtp notifier (using libesmtp)
 */

#include "orte_config.h"

#include "opal/mca/base/mca_base_param.h"

#include "orte/constants.h"
#include "orte/util/show_help.h"

#include "notifier_smtp.h"

static int smtp_open(void);
static int smtp_component_query(mca_base_module_t **module, int *priority);
static int smtp_close(void);
static int smtp_register(void);

/*
 * Struct of function pointers that need to be initialized
 */
orte_notifier_smtp_component_t mca_notifier_smtp_component = {
    {
        {
            ORTE_NOTIFIER_BASE_VERSION_1_0_0,
            
            "smtp",
            
            ORTE_MAJOR_VERSION,
            ORTE_MINOR_VERSION,
            ORTE_RELEASE_VERSION,
            
            smtp_open,
            smtp_close,
            smtp_component_query,
            smtp_register,
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    },

    /* SMTP server and port */
    "localhost",
    25,

    /* To, from, subject */
    NULL,
    NULL,
    "Open MPI Notifier",
    NULL,
    "Open MPI notifier",

    /* Mail body prefix, suffix */
    "The Open MPI SMTP notifier wishes to inform you of the following message:\n\n",
    "\n\nSincerely,\nOscar the Open MPI Owl",

    /* Struct hostent */
    NULL,

    /* Priority */
    10,
};

static int smtp_register(void)
{
    char version[256];

    /* Server stuff */
    mca_base_param_reg_string(&mca_notifier_smtp_component.super.base_version,
                              "server",
                              "SMTP server name or IP address",
                              false, false,
                              mca_notifier_smtp_component.server,
                              &mca_notifier_smtp_component.server);
    mca_base_param_reg_int(&mca_notifier_smtp_component.super.base_version,
                           "port",
                           "SMTP server port",
                           false, false, 
                           mca_notifier_smtp_component.port,
                           &mca_notifier_smtp_component.port);

    /* Email stuff */
    mca_base_param_reg_string(&mca_notifier_smtp_component.super.base_version,
                              "to",
                              "Comma-delimited list of email addresses to send to",
                              false, false,
                              mca_notifier_smtp_component.to,
                              &mca_notifier_smtp_component.to);
    mca_base_param_reg_string(&mca_notifier_smtp_component.super.base_version,
                              "from_addr",
                              "Email address that messages will be from",
                              false, false,
                              mca_notifier_smtp_component.from_addr,
                              &mca_notifier_smtp_component.from_addr);
    mca_base_param_reg_string(&mca_notifier_smtp_component.super.base_version,
                              "from_name",
                              "Email name that messages will be from",
                              false, false,
                              mca_notifier_smtp_component.from_name,
                              &mca_notifier_smtp_component.from_name);
    mca_base_param_reg_string(&mca_notifier_smtp_component.super.base_version,
                              "subject",
                              "Email subject",
                              false, false,
                              mca_notifier_smtp_component.subject,
                              &mca_notifier_smtp_component.subject);

    /* Mail body prefix and suffix */
    mca_base_param_reg_string(&mca_notifier_smtp_component.super.base_version,
                              "body_prefix",
                              "Text to put at the beginning of the mail message",
                              false, false,
                              mca_notifier_smtp_component.body_prefix,
                              &mca_notifier_smtp_component.body_prefix);
    mca_base_param_reg_string(&mca_notifier_smtp_component.super.base_version,
                              "body_suffix",
                              "Text to put at the beginning of the mail message",
                              false, false,
                              mca_notifier_smtp_component.body_suffix,
                              &mca_notifier_smtp_component.body_suffix);

    /* Priority */
    mca_base_param_reg_int(&mca_notifier_smtp_component.super.base_version,
                           "priority",
                           "Priority of this component",
                           false, false, 
                           mca_notifier_smtp_component.priority,
                           &mca_notifier_smtp_component.priority);

    /* Libesmtp version */
    smtp_version(version, sizeof(version), 0);
    version[sizeof(version) - 1] = '\0';
    mca_base_param_reg_string(&mca_notifier_smtp_component.super.base_version,
                              "libesmtp_version",
                              "Version of libesmtp that this component is linked against",
                              false, true, version, NULL);

    return ORTE_SUCCESS;
}

static int smtp_open(void)
{
    /* Nothing to do */
    return ORTE_SUCCESS;
}

static int smtp_close(void)
{
    if (NULL != mca_notifier_smtp_component.server) {
        free(mca_notifier_smtp_component.server);
    }

    if (NULL != mca_notifier_smtp_component.to) {
        free(mca_notifier_smtp_component.to);
    }
    if (NULL != mca_notifier_smtp_component.from_name) {
        free(mca_notifier_smtp_component.from_name);
    }
    if (NULL != mca_notifier_smtp_component.from_addr) {
        free(mca_notifier_smtp_component.from_addr);
    }
    if (NULL != mca_notifier_smtp_component.subject) {
        free(mca_notifier_smtp_component.subject);
    }
    if (NULL != mca_notifier_smtp_component.body_prefix) {
        free(mca_notifier_smtp_component.body_prefix);
    }
    if (NULL != mca_notifier_smtp_component.body_suffix) {
        free(mca_notifier_smtp_component.body_suffix);
    }

    return ORTE_SUCCESS;
}

static int smtp_component_query(mca_base_module_t **module, 
                                int *priority)
{
    *priority = 0;
    *module = NULL;

    /* If there's no to or from, there's no love */
    if (NULL == mca_notifier_smtp_component.to ||
        '\0' == mca_notifier_smtp_component.to[0] ||
        NULL == mca_notifier_smtp_component.from_addr ||
        '\0' == mca_notifier_smtp_component.from_addr[0]) {
        orte_show_help("help-orte-notifier-smtp.txt", 
                       "to/from not specified", true);
        return ORTE_ERR_NOT_FOUND;
    }

    /* Sanity checks */
    if (NULL == mca_notifier_smtp_component.server ||
        '\0' == mca_notifier_smtp_component.server[0]) {
        orte_show_help("help-orte-notifier-smtp.txt", 
                       "server not specified", true);
        return ORTE_ERR_NOT_FOUND;
    }

    /* Since we have to open a socket later, try to resolve the IP
       address of the server now.  Save the result, or abort if we
       can't resolve it. */
    mca_notifier_smtp_component.server_hostent =
        gethostbyname(mca_notifier_smtp_component.server);
    if (NULL == mca_notifier_smtp_component.server_hostent) {
        orte_show_help("help-orte-notifier-smtp.txt", 
                       "unable to resolve server",
                       true, mca_notifier_smtp_component.server);
        return ORTE_ERR_NOT_FOUND;
    }

    *priority = 10;
    *module = (mca_base_module_t *)&orte_notifier_smtp_module;
    return ORTE_SUCCESS;    
}
