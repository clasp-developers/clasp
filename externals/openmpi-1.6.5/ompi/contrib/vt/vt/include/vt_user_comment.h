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
# error "vt_user_comment.h should only be included from vt_user.h"
#endif /* _VT_USER_H */

#ifndef _VT_USER_COMMENT_H
#define _VT_USER_COMMENT_H

#if (defined(VTRACE)) && !(defined(VTRACE_NO_COMMENT))

  __VT_EXTERN_DECL void VT_User_comment_def__(const char* comment);
  __VT_EXTERN_DECL void VT_User_comment__(const char* comment);

# define VT_COMMENT_DEF(c) VT_User_comment_def__((c))
# define VT_COMMENT(c) VT_User_comment__((c))

#else /* VTRACE && !VTRACE_NO_COMMENT */

# define VT_COMMENT_DEF(c)
# define VT_COMMENT(c)

#endif /* VTRACE && !VTRACE_NO_COMMENT */

#endif /* _VT_USER_COMMENT_H */
