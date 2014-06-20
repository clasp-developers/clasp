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

#ifndef OPAL_MCA_TIMER_DARWIN_TIMER_DARWIN_H
#define OPAL_MCA_TIMER_DARWIN_TIMER_DARWIN_H

#include "opal_config.h"
#include <mach/mach_time.h>

typedef uint64_t opal_timer_t;

/* frequency in mhz */
OPAL_DECLSPEC extern opal_timer_t opal_timer_darwin_freq;


static inline opal_timer_t
opal_timer_base_get_cycles(void)
{
    /* this is basically a wrapper around the "right" assembly to get
       the tick counter off the PowerPC Time Base.  I believe it's
       something similar on x86 */
    return mach_absolute_time();
}


static inline opal_timer_t
opal_timer_base_get_usec(void)
{
    /* freq is in Hz, so this gives usec */
    return mach_absolute_time() * 1000000  / opal_timer_darwin_freq;
}    


static inline opal_timer_t
opal_timer_base_get_freq(void)
{
    return opal_timer_darwin_freq;
}


#define OPAL_TIMER_CYCLE_NATIVE 1
#define OPAL_TIMER_CYCLE_SUPPORTED 1
#define OPAL_TIMER_USEC_NATIVE 0
#define OPAL_TIMER_USEC_SUPPORTED 1

#endif
