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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_MCA_TIMER_AIX_TIMER_AIX_H
#define OPAL_MCA_TIMER_AIX_TIMER_AIX_H

#include <sys/time.h>

BEGIN_C_DECLS

typedef uint64_t opal_timer_t;

extern opal_timer_t opal_timer_aix_freq_mhz;
extern opal_timer_t opal_timer_aix_freq;

static inline opal_timer_t
opal_timer_base_get_usec()
{
    timebasestruct_t t;
    uint64_t retval;

    read_real_time(&t, TIMEBASE_SZ);
    time_base_to_time(&t, TIMEBASE_SZ);
    retval = (t.tb_high * 1000000) + t.tb_low / 1000;

    return retval;
}    

static inline opal_timer_t
opal_timer_base_get_cycles()
{
#ifdef HAVE_PM_CYCLES
    return opal_timer_base_get_usec() * opal_timer_aix_freq_mhz;
#else
    return 0;
#endif
}

static inline opal_timer_t
opal_timer_base_get_freq()
{
    return opal_timer_aix_freq;;
}


#ifdef HAVE_PM_CYCLES
#define OPAL_TIMER_CYCLE_NATIVE 0
#define OPAL_TIMER_CYCLE_SUPPORTED 1
#else
#define OPAL_TIMER_CYCLE_NATIVE 0
#define OPAL_TIMER_CYCLE_SUPPORTED 0
#endif
#define OPAL_TIMER_USEC_NATIVE 1
#define OPAL_TIMER_USEC_SUPPORTED 1

END_C_DECLS

#endif
