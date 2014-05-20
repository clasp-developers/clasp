!**
!* VampirTrace
!* http://www.tu-dresden.de/zih/vampirtrace
!*
!* Copyright (c) 2005-2013, ZIH, TU Dresden, Federal Republic of Germany
!*
!* Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
!*                          Centre, Federal Republic of Germany
!*
!* See the file COPYING in the package base directory for details
!**

#if (defined(VTRACE)) && !(defined(VTRACE_NO_CONTROL))
# define VT_IS_ON(ierr) call VT_User_is_trace_on__(ierr)
# define VT_ON() call VT_User_trace_on__()
# define VT_OFF() call VT_User_trace_off__()
# define VT_BUFFER_FLUSH() call VT_User_buffer_flush__()
# define VT_TIMESYNC() call VT_User_timesync__()
# define VT_UPDATE_COUNTER() call VT_User_update_counter__()
# define VT_SET_REWIND_MARK() call VT_User_set_rewind_mark__()
# define VT_REWIND() call VT_User_rewind__()
#else
# define VT_IS_ON(ierr) ierr = 1
# define VT_ON()
# define VT_OFF()
# define VT_BUFFER_FLUSH()
# define VT_TIMESYNC()
# define VT_UPDATE_COUNTER()
# define VT_SET_REWIND_MARK()
# define VT_REWIND()
#endif
