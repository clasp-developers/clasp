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

#ifndef _VT_USER_H
# error "vt_user_control.h should only be included from vt_user.h"
#endif /* _VT_USER_H */

#ifndef _VT_USER_CONTROL_H
#define _VT_USER_CONTROL_H

#if (defined (VTRACE)) && !(defined (VTRACE_NO_CONTROL))

  __VT_EXTERN_DECL int  VT_User_is_trace_on__(void);
  __VT_EXTERN_DECL void VT_User_trace_on__(void);
  __VT_EXTERN_DECL void VT_User_trace_off__(void);
  __VT_EXTERN_DECL void VT_User_buffer_flush__(void);
  __VT_EXTERN_DECL void VT_User_timesync__(void);
  __VT_EXTERN_DECL void VT_User_update_counter__(void);
  __VT_EXTERN_DECL void VT_User_set_rewind_mark__(void);
  __VT_EXTERN_DECL void VT_User_rewind__(void);

# define VT_IS_ON() VT_User_is_trace_on__()
# define VT_ON() VT_User_trace_on__()
# define VT_OFF() VT_User_trace_off__()
# define VT_BUFFER_FLUSH() VT_User_buffer_flush__()
# define VT_TIMESYNC() VT_User_timesync__()
# define VT_UPDATE_COUNTER() VT_User_update_counter__()
# define VT_SET_REWIND_MARK() VT_User_set_rewind_mark__()
# define VT_REWIND() VT_User_rewind__()

#else /* VTRACE  && !VTRACE_NO_CONTROL */

# define VT_IS_ON() 1
# define VT_ON()
# define VT_OFF()
# define VT_BUFFER_FLUSH()
# define VT_TIMESYNC()
# define VT_UPDATE_COUNTER()
# define VT_SET_REWIND_MARK()
# define VT_REWIND()

#endif /* VTRACE && !VTRACE_NO_CONTROL */

#endif /* _VT_USER_CONTROL_H */
