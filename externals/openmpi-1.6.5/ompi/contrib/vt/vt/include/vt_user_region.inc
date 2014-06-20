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

#if (defined(VTRACE)) && !(defined(VTRACE_NO_REGION))
# define VT_USER_START(n) call VT_User_start__((n), __FILE__, __LINE__)
# define VT_USER_END(n) call VT_User_end__((n))
# define VT_USER_START2(i) call VT_User_start2__((i))
# define VT_USER_END2(i) call VT_User_end2__((i))
# define VT_USER_DEF(n, g, f, l, i) call VT_User_def__((n), (g), "", 0, i)
#else
# define VT_USER_START(n)
# define VT_USER_END(n)
# define VT_USER_START2(i)
# define VT_USER_END2(i)
# define VT_USER_DEF(n, g, f, l, i)
#endif
