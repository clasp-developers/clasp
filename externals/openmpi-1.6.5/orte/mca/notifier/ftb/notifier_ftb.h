/* -*- C -*-
 * 
 * Copyright (c) 2004-2011 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */
#ifndef NOTIFIER_FTB_H
#define NOTIFIER_FTB_H

#include "orte_config.h"

#include "orte/mca/notifier/notifier.h"

#include "libftb.h"

BEGIN_C_DECLS

typedef struct {
    orte_notifier_base_component_t super;

    /* FTB client subscription style */
    char *subscription_style;

    /* Priority of this component */
    int priority;
} orte_notifier_ftb_component_t;

/* Notifier interfaces */

ORTE_MODULE_DECLSPEC extern orte_notifier_ftb_component_t mca_notifier_ftb_component;
extern orte_notifier_base_module_t orte_notifier_ftb_module;

/* FTB client information */
extern FTB_client_t ftb_client_info;
extern FTB_client_handle_t ftb_client_handle;

/* FTB event types */
typedef enum {
    FTB_EVENT_NORMAL   = 1,
    FTB_EVENT_RESPONSE = 2
} ftb_event_type_t;

/* Returns the FTB event name (as a string) given the event code */
#define FTB_EVENT(errnum) #errnum

END_C_DECLS

#endif
