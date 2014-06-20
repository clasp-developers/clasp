/*
 * Copyright (c) 2010      Cisco Systems, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include "threads.h"

static void constructor(orte_thread_ctl_t *ptr)
{
    OBJ_CONSTRUCT(&ptr->lock, opal_mutex_t);
    OBJ_CONSTRUCT(&ptr->cond, opal_condition_t);
    ptr->active = false;
    ptr->running = false;
    ptr->stop = false;
    ptr->name = NULL;
}
static void destructor(orte_thread_ctl_t *ptr)
{
    OBJ_DESTRUCT(&ptr->lock);
    OBJ_DESTRUCT(&ptr->cond);
    if (NULL != ptr->name) {
        free(ptr->name);
    }
}
OBJ_CLASS_INSTANCE(orte_thread_ctl_t,
                   opal_object_t,
                   constructor, destructor);
