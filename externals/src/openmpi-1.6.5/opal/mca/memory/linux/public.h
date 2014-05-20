/*
 * Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_MEMORY_LINUX_PUBLIC_H
#define OPAL_MEMORY_LINUX_PUBLIC_H

#include "opal_config.h"

#include <sys/types.h>

OPAL_DECLSPEC extern volatile uint64_t *opal_memory_linux_ummunotify_counter;
OPAL_DECLSPEC extern uint64_t opal_memory_linux_ummunotify_counter_last_value;

#define opal_memory_changed() \
    (opal_memory_linux_ummunotify_counter_last_value != \
     *opal_memory_linux_ummunotify_counter)

#endif
