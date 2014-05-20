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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_MCA_TIMER_CATAMOUNT_TIMER_CATAMOUNT_H
#define OPAL_MCA_TIMER_CATAMOUNT_TIMER_CATAMOUNT_H

#include "opal_config.h"
#include <opal/sys/timer.h>

OPAL_DECLSPEC extern opal_timer_t opal_timer_catamount_freq;

static inline opal_timer_t
opal_timer_base_get_cycles(void)
{
#if OPAL_HAVE_SYS_TIMER_GET_CYCLES
    return opal_sys_timer_get_cycles();
#else
    return 0;
#endif
}


static inline opal_timer_t
opal_timer_base_get_usec(void)
{
#if OPAL_HAVE_SYS_TIMER_GET_CYCLES
    /* freq is in Hz, so this gives usec */
    return opal_sys_timer_get_cycles() * 1000000  / opal_timer_catamount_freq;
#else
    return 0;
#endif
}    


static inline opal_timer_t
opal_timer_base_get_freq(void)
{
    return opal_timer_catamount_freq;
}


#define OPAL_TIMER_CYCLE_NATIVE OPAL_HAVE_SYS_TIMER_GET_CYCLES
#define OPAL_TIMER_CYCLE_SUPPORTED OPAL_HAVE_SYS_TIMER_GET_CYCLES
#define OPAL_TIMER_USEC_NATIVE 0
#define OPAL_TIMER_USEC_SUPPORTED OPAL_HAVE_SYS_TIMER_GET_CYCLES

#endif
