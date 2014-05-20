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

#define VT_MARKER_TYPE_ERROR     1
#define VT_MARKER_TYPE_WARNING   2
#define VT_MARKER_TYPE_HINT      3

#if (defined(VTRACE)) && !(defined(VTRACE_NO_MARKER))
# define VT_MARKER_DEF(n, t, i) call VT_User_marker_def__((n), (t), i)
# define VT_MARKER(i, t) call VT_User_marker__((i), (t))
#else
# define VT_MARKER_DEF(n, t, i)
# define VT_MARKER(i, t)
#endif
