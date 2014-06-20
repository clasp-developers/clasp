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
# error "vt_user_message.h should only be included from vt_user.h"
#endif /* _VT_USER_H */

#ifndef VT_USER_MESSAGE_H_
#define VT_USER_MESSAGE_H_

#define VT_MSG_DEFCOMM -1

#if (defined(VTRACE)) && !(defined(VTRACE_NO_MSG))

  __VT_EXTERN_DECL unsigned int VT_User_msg_comm_def__(const char* cname);
  __VT_EXTERN_DECL void VT_User_msg_send__(unsigned int cid, unsigned int tag,
                                           unsigned int sent);
  __VT_EXTERN_DECL void VT_User_msg_recv__(unsigned int cid, unsigned int tag,
                                           unsigned int recvd);

# define VT_MSG_COMM_DEF(n) VT_User_msg_comm_def__((n))
# define VT_MSG_SEND(c, t, s) VT_User_msg_send__((c), (t), (s))
# define VT_MSG_RECV(c, t, r) VT_User_msg_recv__((c), (t), (r))

#else /* VTRACE && !VTRACE_NO_MSG */

# define VT_MSG_COMM_DEF(n) 0
# define VT_MSG_SEND(c, t, s)
# define VT_MSG_RECV(c, t, r)

#endif /* VTRACE && !VTRACE_NO_MSG */

/* macros for more convenient access */
#define VT_COMM_WORLD VT_MSG_DEFCOMM
#define VT_COMM_DEF   VT_MSG_COMM_DEF
#define VT_SEND       VT_MSG_SEND
#define VT_RECV       VT_MSG_RECV

#endif /* VT_USER_MESSAGE_H_ */
