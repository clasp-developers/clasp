/* -*- C -*-
 * 
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009 Cisco Systems, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */
#ifndef NOTIFIER_SMTP_H
#define NOTIFIER_SMTP_H

#include "orte_config.h"

#include <netdb.h>

#include "libesmtp.h"

#include "orte/mca/notifier/notifier.h"

BEGIN_C_DECLS

typedef struct {
    orte_notifier_base_component_t super;

    /* SMTP server name and port */
    char *server;
    int port;

    /* To, From, Subject */
    char *to, **to_argv, *from_name, *from_addr, *subject;

    /* Mail body prefix and suffix */
    char *body_prefix, *body_suffix;

    /* struct hostent from resolved SMTP server name */
    struct hostent *server_hostent;

    /* Priority of this component */
    int priority;
} orte_notifier_smtp_component_t;


/*
 * Notifier interfaces
 */
ORTE_MODULE_DECLSPEC extern orte_notifier_smtp_component_t 
    mca_notifier_smtp_component;
extern orte_notifier_base_module_t orte_notifier_smtp_module;

END_C_DECLS

#endif
