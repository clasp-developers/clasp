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

#define VT_MSG_DEFCOMM -1

#if (defined(VTRACE)) && !(defined(VTRACE_NO_MSG))
# define VT_MSG_COMM_DEF(n, ci) call VT_User_msg_comm_def__((n), ci)
# define VT_MSG_SEND(c, t, s) call VT_User_msg_send__((c), (t), (s))
# define VT_MSG_RECV(c, t, r) call VT_User_msg_recv__((c), (t), (r))
#else
# define VT_MSG_COMM_DEF(n, ci)
# define VT_MSG_SEND(c, t, s)
# define VT_MSG_RECV(c, t, r)
#endif

! macros for more convenient access
#define VT_COMM_WORLD VT_MSG_DEFCOMM
#define VT_COMM_DEF   VT_MSG_COMM_DEF
#define VT_SEND       VT_MSG_SEND
#define VT_RECV       VT_MSG_RECV
