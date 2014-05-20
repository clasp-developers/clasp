/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2013, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _VT_MALLOCWRAP_H
#define _VT_MALLOCWRAP_H

#ifdef __cplusplus
# define EXTERN extern "C"
#else
# define EXTERN extern
#endif

#include "vt_defs.h"
#include "vt_thrd.h"
#include "vt_trc.h"

#ifdef VT_MALLOCWRAP

/* macro for temporarily suspend MALLOC tracing */
#define VT_SUSPEND_MALLOC_TRACING(tid)                                        \
  if( vt_is_alive && VT_MY_THREAD_IS_ALIVE ) {                                \
    VTThrd* _thrd =                                                           \
      ((tid) == VT_CURRENT_THREAD) ? VTTHRD_MY_VTTHRD : VTThrdv[(tid)];       \
    VTTHRD_MALLOC_TRACING_ENABLED(_thrd) = 0;                                 \
    VTTHRD_MALLOC_TRACING_SUSPEND_CNT(_thrd)++;                               \
  }

/* macro for resuming from MALLOC tracing suspension */
#define VT_RESUME_MALLOC_TRACING(tid)                                         \
  if( vt_is_alive && VT_MY_THREAD_IS_ALIVE ) {                                \
    VTThrd* _thrd =                                                           \
      ((tid) == VT_CURRENT_THREAD) ? VTTHRD_MY_VTTHRD : VTThrdv[(tid)];       \
    if( VTTHRD_MALLOC_TRACING_SUSPEND_CNT(_thrd) == 0 ||                      \
        --VTTHRD_MALLOC_TRACING_SUSPEND_CNT(_thrd) == 0 ) {                   \
      VTTHRD_MALLOC_TRACING_ENABLED(_thrd) =                                  \
        VTTHRD_MALLOC_TRACING_STATE(_thrd);                                   \
    }                                                                         \
  }

EXTERN void vt_mallocwrap_init(void);
EXTERN void vt_mallocwrap_finalize(void);

#else /* VT_MALLOCWRAP */

#define VT_SUSPEND_MALLOC_TRACING(tid)
#define VT_RESUME_MALLOC_TRACING(tid)

#endif /* VT_MALLOCWRAP */

#endif /* _VT_MALLOCWRAP_H */
