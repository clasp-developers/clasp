/*
 *Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                        University Research and Technology
 *                        Corporation.  All rights reserved.
 *Copyright (c) 2004-2005 The University of Tennessee and The University
 *                        of Tennessee Research Foundation.  All rights
 *                        reserved.
 *Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                        University of Stuttgart.  All rights reserved.
 *Copyright (c) 2004-2005 The Regents of the University of California.
 *                        All rights reserved.
 *$COPYRIGHT$
 *
 *Additional copyrights may follow
 *
 *$HEADER$
 */

#ifndef OMPI_TIME_H
#define OMPI_TIME_H

#include "opal_config.h"

#ifndef OMPI_WIN_COMPAT_H
#error This file is supposed to be included only from win_compat.h
#endif  /* OMPI_WIN_COMPAT_H */

#define DST_NONE    0   /* not on dst */
#define DST_USA     1   /* USA style dst */
#define DST_AUST    2   /* Australian style dst */
#define DST_WET     3   /* Western European dst */
#define DST_MET     4   /* Middle European dst */
#define DST_EET     5   /* Eastern European dst */
#define DST_CAN     6   /* Canada */

#define TIMEVAL_TO_TIMESPEC(tv, ts) \
(ts)->tv_sec = (tv)->tv_sec;    \
(ts)->tv_nsec = (tv)->tv_usec * 1000;

#define TIMESPEC_TO_TIMEVAL(tv, ts) \
(tv)->tv_sec = (ts)->tv_sec;  \
(tv)->tv_usec = (ts)->tv_nsec / 1000;


/* some more utility functions */
/* Operations on timevals. */
#ifndef timerclear
#define timerclear(tvp)     (tvp)->tv_sec = (tvp)->tv_usec = 0
#endif

#ifndef timerisset
#define timerisset(tvp)     ((tvp)->tv_sec || (tvp)->tv_usec)
#endif

#ifndef timercmp
#define timercmp(tvp, uvp, cmp)                     \
    (((tvp)->tv_sec == (uvp)->tv_sec) ?             \
    ((tvp)->tv_usec cmp (uvp)->tv_usec) :           \
    ((tvp)->tv_sec cmp (uvp)->tv_sec))
#endif

#ifndef timeradd
#define timeradd(tvp, uvp, vvp)                     \
    do {                                \
    (vvp)->tv_sec = (tvp)->tv_sec + (uvp)->tv_sec;      \
    (vvp)->tv_usec = (tvp)->tv_usec + (uvp)->tv_usec;   \
    if ((vvp)->tv_usec >= 1000000) {            \
        (vvp)->tv_sec++;                \
        (vvp)->tv_usec -= 1000000;          \
        }                           \
    } while (0)
#endif
    
#ifndef timersub
#define timersub(tvp, uvp, vvp)                     \
    do {                                \
    (vvp)->tv_sec = (tvp)->tv_sec - (uvp)->tv_sec;      \
    (vvp)->tv_usec = (tvp)->tv_usec - (uvp)->tv_usec;   \
    if ((vvp)->tv_usec < 0) {               \
    (vvp)->tv_sec--;                \
    (vvp)->tv_usec += 1000000;          \
    }                           \
    } while (0)
#endif

/* Operations on timespecs. */

#ifndef timespecclear
#define timespecclear(tsp)      (tsp)->tv_sec = (tsp)->tv_nsec = 0
#endif

#ifndef timespecisset
#define timespecisset(tsp)      ((tsp)->tv_sec || (tsp)->tv_nsec)
#endif

#ifndef timespeccmp
#define timespeccmp(tsp, usp, cmp)                  \
    (((tsp)->tv_sec == (usp)->tv_sec) ?             \
    ((tsp)->tv_nsec cmp (usp)->tv_nsec) :           \
    ((tsp)->tv_sec cmp (usp)->tv_sec))
#endif

#ifndef timespecadd
#define timespecadd(tsp, usp, vsp)                  \
    do {                                \
    (vsp)->tv_sec = (tsp)->tv_sec + (usp)->tv_sec;      \
    (vsp)->tv_nsec = (tsp)->tv_nsec + (usp)->tv_nsec;   \
    if ((vsp)->tv_nsec >= 1000000000L) {            \
    (vsp)->tv_sec++;                \
    (vsp)->tv_nsec -= 1000000000L;          \
    }                           \
    } while (0)
#endif

#ifndef timespecsub
#define timespecsub(tsp, usp, vsp)                  \
    do {                                \
    (vsp)->tv_sec = (tsp)->tv_sec - (usp)->tv_sec;      \
    (vsp)->tv_nsec = (tsp)->tv_nsec - (usp)->tv_nsec;   \
    if ((vsp)->tv_nsec < 0) {               \
    (vsp)->tv_sec--;                \
    (vsp)->tv_nsec += 1000000000L;          \
    }                           \
    } while (0)
#endif

/*
 * Names of the interval timers, and structure
  * defining a timer setting.
   */
#define ITIMER_REAL 0
#define ITIMER_VIRTUAL  1
#define ITIMER_PROF 2

struct itimerval {
  struct  timeval it_interval;    /* timer interval */
  struct  timeval it_value;   /* current value */
};

/*
 * Getkerninfo clock information structure
 */
struct clockinfo {
   int hz;     /* clock frequency */
   int tick;       /* micro-seconds per hz tick */
   int tickadj;    /* clock skew rate for adjtime() */
   int stathz;     /* statistics clock frequency */
   int profhz;     /* profiling clock frequency */
};

#define CLOCK_REALTIME  0
#define CLOCK_VIRTUAL   1
#define CLOCK_PROF  2

#define TIMER_RELTIME   0x0 /* relative timer */
#define TIMER_ABSTIME   0x1 /* absolute timer */

#ifndef OMPI_TIMESPEC
#define OMPI_TIMESPEC
struct timespec
{
  long tv_sec;
  long tv_nsec;
};
#endif


/*
NOTE: The use of timezone is obsolete even in linux and my gettimeofday
function is not going to support it either. So, please be aware of the 
fact that if you expect to pass anything here, then you are DEAD :-D */
struct timezone
{
  int tz_minuteswest;
  int tz_dsttime;
};

BEGIN_C_DECLS

OPAL_DECLSPEC int gettimeofday (struct timeval *tv, struct timezone *tz);

END_C_DECLS

#endif				/* OMPI_TIME_H */
