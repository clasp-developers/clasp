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

#include "opal_config.h"
#include "opal/win32/ompi_time.h"

#include<time.h>

#define EPOCHFILETIME (116444736000000000LL)

int gettimeofday(struct timeval *tv, struct timezone *tz)
{
    FILETIME file_time;
    LARGE_INTEGER place_holder;
    __int64 time;
    

    /* returns 64 bit value which is the number of 100 nanosecond
       intervals since 1601(UTC) */
    GetSystemTimeAsFileTime (&file_time);

    /* Windows recommends that we should copy the FILETIME returned 
       into a ULARGE_INTEGER and then perform the arithmetic on that */
    place_holder.LowPart = file_time.dwLowDateTime;
    place_holder.HighPart = file_time.dwHighDateTime;
    time = place_holder.QuadPart;
    time -= EPOCHFILETIME;

    /* Now we can use arithmetic operations on time which is nothing but
       a 64 bit integer holding time in 100 nanosec intervals */

    /* convert 100 nanoseconds intervals into microseconds .. divide by 10 */
    time /= 10;
    
    tv->tv_sec = (long)(time / 1000000);
    tv->tv_usec = (long)(time % 1000000);

    return 0;
}
