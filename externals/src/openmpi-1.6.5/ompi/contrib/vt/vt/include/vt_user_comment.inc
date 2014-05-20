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

#if (defined(VTRACE)) && !(defined(VTRACE_NO_COMMENT))
# define VT_COMMENT_DEF(c) call VT_User_comment_def__((c))
# define VT_COMMENT(c) call VT_User_comment__((c))
#else
# define VT_COMMENT_DEF(c)
# define VT_COMMENT(c)
#endif
