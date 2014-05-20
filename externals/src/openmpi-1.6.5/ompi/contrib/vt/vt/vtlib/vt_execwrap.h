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

#ifndef _VT_EXECWRAP_H
#define _VT_EXECWRAP_H

#ifdef __cplusplus
# define EXTERN extern "C"
#else
# define EXTERN extern
#endif

#include "vt_defs.h"
#include "vt_thrd.h"
#include "vt_trc.h"

#ifdef VT_EXECWRAP

/* macro for temporarily suspend EXEC tracing */
#define VT_SUSPEND_EXEC_TRACING(tid)                                          \
   if( vt_is_alive && VT_MY_THREAD_IS_ALIVE ) {                               \
    VTThrd* _thrd =                                                           \
      ((tid) == VT_CURRENT_THREAD) ? VTTHRD_MY_VTTHRD : VTThrdv[(tid)];       \
    VTTHRD_EXEC_TRACING_ENABLED(_thrd) = 0;                                   \
    VTTHRD_EXEC_TRACING_SUSPEND_CNT(_thrd)++;                                 \
  }

/* macro for resuming from EXEC tracing suspension */
#define VT_RESUME_EXEC_TRACING(tid)                                           \
  if( vt_is_alive && VT_MY_THREAD_IS_ALIVE ) {                                \
    VTThrd* _thrd =                                                           \
      ((tid) == VT_CURRENT_THREAD) ? VTTHRD_MY_VTTHRD : VTThrdv[(tid)];       \
    if( VTTHRD_EXEC_TRACING_SUSPEND_CNT(_thrd) == 0 ||                        \
        --VTTHRD_EXEC_TRACING_SUSPEND_CNT(_thrd) == 0 ) {                     \
      VTTHRD_EXEC_TRACING_ENABLED(_thrd) =                                    \
        VTTHRD_EXEC_TRACING_STATE(_thrd);                                     \
    }                                                                         \
  }

EXTERN void vt_execwrap_init(void);
EXTERN void vt_execwrap_finalize(void);

#else /* VT_EXECWRAP */

#define VT_SUSPEND_EXEC_TRACING(tid)
#define VT_RESUME_EXEC_TRACING(tid)

#endif /* VT_EXECWRAP */

#endif /* _VT_EXECWRAP_H */
