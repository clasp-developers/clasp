/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORTE_THREAD_H
#define ORTE_THREAD_H

#include "orte_config.h"

#include "opal/class/opal_object.h"
#if OPAL_ENABLE_DEBUG
#include "opal/util/output.h"
#endif
#include "opal/util/fd.h"

#include "mutex.h"
#include "condition.h"

BEGIN_C_DECLS

typedef struct {
    opal_object_t super;
    opal_mutex_t lock;
    opal_condition_t cond;
    volatile bool active;
    volatile bool running;
    volatile bool stop;
    char *name;
} orte_thread_ctl_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_thread_ctl_t);

#define ORTE_ACQUIRE_THREAD(ctl)                                \
    do {                                                        \
        ORTE_THREAD_LOCK(&(ctl)->lock);                         \
        while ((ctl)->active) {                                 \
            ORTE_CONDITION_WAIT(&(ctl)->cond, &(ctl)->lock);    \
        }                                                       \
        (ctl)->active = true;                                   \
    } while(0);


#define ORTE_RELEASE_THREAD(ctl)                                        \
    do {                                                                \
        (ctl)->active = false;                                          \
        ORTE_CONDITION_BROADCAST(&(ctl)->cond);                         \
        ORTE_THREAD_UNLOCK(&(ctl)->lock);                               \
    } while(0);

#define ORTE_WAKEUP_THREAD(ctl)                                         \
    do {                                                                \
        ORTE_THREAD_LOCK(&(ctl)->lock);                                 \
        (ctl)->active = false;                                          \
        ORTE_CONDITION_BROADCAST(&(ctl)->cond);                         \
        ORTE_THREAD_UNLOCK(&(ctl)->lock);                               \
    } while(0);

END_C_DECLS

#endif /* ORTE_THREAD_H */
