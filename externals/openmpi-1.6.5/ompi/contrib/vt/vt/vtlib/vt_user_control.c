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

#include "config.h"

#include "vt_defs.h"
#include "vt_env.h"
#include "vt_fbindings.h"
#include "vt_inttypes.h"
#include "vt_mallocwrap.h"
#include "vt_pform.h"
#include "vt_trc.h"
#if ((defined(VT_MPI) || defined(VT_HYB)) \
    && defined(VT_ETIMESYNC) && TIMER_IS_GLOBAL == 0)
# include "vt_esync.h"
#endif /* (VT_MPI || VT_HYB) && VT_ETIMESYNC && TIMER_IS_GLOBAL */
#define VTRACE
#undef VTRACE_NO_CONTROL
#include "vt_user.h"

#include <string.h>

static int vt_init = 1;        /* is initialization needed? */

#define VT_INIT \
  if ( vt_init ) { \
    vt_init = 0; \
    vt_open(); \
  }

int VT_User_is_trace_on__()
{
  int ret;

  VT_INIT;

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

  ret = vt_is_trace_on(VT_CURRENT_THREAD);

  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);

  return ret;
}

void VT_User_trace_on__()
{
  VT_INIT;

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

  vt_trace_on(VT_CURRENT_THREAD, 1);

  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
}

void VT_User_trace_off__()
{
  VT_INIT;

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

  vt_trace_off(VT_CURRENT_THREAD, 1, 0);

  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
}

void VT_User_buffer_flush__()
{
  VT_INIT;

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

  vt_buffer_flush(VT_CURRENT_THREAD);

  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
}

void VT_User_timesync__()
{
#if ((defined(VT_MPI) || defined(VT_HYB)) \
    && defined(VT_ETIMESYNC) && TIMER_IS_GLOBAL == 0)
  VT_INIT;

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

  if ( vt_num_traces > 1 && vt_env_etimesync() )
    vt_esync(MPI_COMM_WORLD);

  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
#endif /* (VT_MPI || VT_HYB) && VT_ETIMESYNC && TIMER_IS_GLOBAL */
}

void VT_User_update_counter__()
{
  uint64_t time;

  VT_INIT;

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

  time = vt_pform_wtime();
  vt_update_counter(VT_CURRENT_THREAD, &time);

  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
}


void VT_User_set_rewind_mark__(void)
{
  uint64_t time;

  VT_INIT;

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

  time = vt_pform_wtime();
  vt_set_rewind_mark(VT_CURRENT_THREAD, &time);

  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
}

void VT_User_rewind__(void)
{
  uint64_t time;

  VT_INIT;

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

  time = vt_pform_wtime();
  vt_rewind(VT_CURRENT_THREAD, &time);

  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
}


/*
 * Fortran version
 */

VT_DECLDEF(void VT_User_is_trace_on___f(int* ierr))
{
  *ierr = VT_User_is_trace_on__();
} VT_GENERATE_F77_BINDINGS(vt_user_is_trace_on__, VT_USER_IS_TRACE_ON__,
                           VT_User_is_trace_on___f,
                           (int* ierr), (ierr))

VT_GENERATE_F77_BINDINGS(vt_user_trace_on__, VT_USER_TRACE_ON__,
                         VT_User_trace_on__,
                         (void), ())

VT_GENERATE_F77_BINDINGS(vt_user_trace_off__, VT_USER_TRACE_OFF__,
                         VT_User_trace_off__,
                         (void), ())

VT_GENERATE_F77_BINDINGS(vt_user_buffer_flush__, VT_USER_BUFFER_FLUSH__,
                         VT_User_buffer_flush__,
                         (void), ())

VT_GENERATE_F77_BINDINGS(vt_user_timesync__, VT_USER_TIMESYNC__,
                         VT_User_timesync__,
                         (void), ())

VT_GENERATE_F77_BINDINGS(vt_user_update_counter__, VT_USER_UPDATE_COUNTER__,
                         VT_User_update_counter__,
                         (void), ())

VT_GENERATE_F77_BINDINGS(vt_user_set_rewind_mark__, VT_USER_SET_REWIND_MARK__,
                         VT_User_set_rewind_mark__,
                         (void), ())

VT_GENERATE_F77_BINDINGS(vt_user_rewind__, VT_USER_REWIND__,
                         VT_User_rewind__,
                         (void), ())
