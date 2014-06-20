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

#include <string.h>

#include "vt_fbindings.h"
#include "vt_inttypes.h"
#include "vt_mallocwrap.h"
#include "vt_pform.h"
#include "vt_thrd.h"
#include "vt_trc.h"
#define VTRACE
#undef VTRACE_NO_MSG
#include "vt_user.h"

static int vt_init = 1;        /* is initialization needed? */
static uint32_t def_cid = 0;   /* default communicator id */

#define VT_INIT \
  if ( vt_init ) { \
    vt_init = 0; \
    vt_open(); \
  }

unsigned int VT_User_msg_comm_def__(const char* cname)
{
  uint32_t cid;

  VT_INIT;

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

#if (defined(VT_MT) || defined(VT_HYB))
  VTTHRD_LOCK_IDS();
#endif
  cid = vt_def_user_comm(VT_CURRENT_THREAD, cname);
#if (defined(VT_MT) || defined(VT_HYB))
  VTTHRD_UNLOCK_IDS();
#endif

  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);

  return cid;
}

void VT_User_msg_send__(unsigned int cid, unsigned int tag,
                        unsigned int sent)
{
  uint64_t time;

  VT_INIT;

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

  if (cid == (uint32_t)VT_MSG_DEFCOMM)
  {
    if (def_cid == 0)
      def_cid = VT_User_msg_comm_def__("User");

    cid = def_cid;
  }

  time = vt_pform_wtime();
  vt_user_send(VT_CURRENT_THREAD, &time, cid, tag, sent);

  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
}

void VT_User_msg_recv__(unsigned int cid, unsigned int tag,
                        unsigned int recvd)
{
  uint64_t time;

  VT_INIT;

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

  if (cid == (uint32_t)VT_MSG_DEFCOMM)
  {
    if (def_cid == 0)
      def_cid = VT_User_msg_comm_def__("User");

    cid = def_cid;
  }

  time = vt_pform_wtime();
  vt_user_recv(VT_CURRENT_THREAD, &time, cid, tag, recvd);

  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
}

/*
 * Fortran version
 */

VT_DECLDEF(void VT_User_msg_comm_def___f(const char* cname,
                                         unsigned int* cid, int nl))
{
  int namlen;
  char fnambuf[128];

  /* -- convert Fortran to C strings -- */
  namlen = ( nl < 128 ) ? nl : 127;
  strncpy(fnambuf, cname, namlen);
  fnambuf[namlen] = '\0';

  *cid = VT_User_msg_comm_def__(fnambuf);
} VT_GENERATE_F77_BINDINGS(vt_user_msg_comm_def__, VT_USER_MSG_COMM_DEF__,
                           VT_User_msg_comm_def___f,
                           (const char* cname, unsigned int* cid, int nl),
                           (cname, cid, nl))


VT_DECLDEF(void VT_User_msg_send___f(unsigned int* cid, unsigned int* tag,
                                     unsigned int* sent))
{
  VT_User_msg_send__(*cid, *tag, *sent);
} VT_GENERATE_F77_BINDINGS(vt_user_msg_send__,
                           VT_USER_MSG_SEND__,
                           VT_User_msg_send___f,
                           (unsigned int* cid, unsigned int* tag,
                            unsigned int* sent),
                           (cid, tag, sent))

VT_DECLDEF(void VT_User_msg_recv___f(unsigned int* cid, unsigned int* tag,
                                     unsigned int* recvd))
{
  VT_User_msg_recv__(*cid, *tag, *recvd);
} VT_GENERATE_F77_BINDINGS(vt_user_msg_recv__,
                           VT_USER_MSG_RECV__,
                           VT_User_msg_recv___f,
                           (unsigned int* cid, unsigned int* tag,
                            unsigned int* recvd),
                           (cid, tag, recvd))
