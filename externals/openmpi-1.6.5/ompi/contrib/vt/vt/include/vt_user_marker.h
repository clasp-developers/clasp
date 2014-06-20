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
# error "vt_user_marker.h should only be included from vt_user.h"
#endif /* _VT_USER_H */

#ifndef _VT_USER_MARKER_H
#define _VT_USER_MARKER_H

#define VT_MARKER_TYPE_ERROR     1
#define VT_MARKER_TYPE_WARNING   2
#define VT_MARKER_TYPE_HINT      3

#if (defined(VTRACE)) && !(defined(VTRACE_NO_MARKER))

  __VT_EXTERN_DECL unsigned int VT_User_marker_def__(const char* mname,
                                                     int mtype);
  __VT_EXTERN_DECL void VT_User_marker__(unsigned int mid, const char* mtext);

# define VT_MARKER_DEF(n, t) VT_User_marker_def__((n), (t))
# define VT_MARKER(i, t) VT_User_marker__((i), (t))

#else /* VTRACE && !VTRACE_NO_MARKER */

# define VT_MARKER_DEF(n, t) 0
# define VT_MARKER(i, t)

#endif /* VTRACE && !VTRACE_NO_MARKER */

#endif /* _VT_USER_MARKER_H && !VTRACE_NO_MARKER */
