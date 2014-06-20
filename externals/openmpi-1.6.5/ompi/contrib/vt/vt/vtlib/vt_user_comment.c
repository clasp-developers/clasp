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

#include "vt_defs.h"
#include "vt_fbindings.h"
#include "vt_inttypes.h"
#include "vt_mallocwrap.h"
#include "vt_pform.h"
#include "vt_trc.h"
#define VTRACE
#undef VTRACE_NO_COMMENT
#include "vt_user.h"

static int vt_init = 1;        /* is initialization needed? */

#define VT_INIT \
  if ( vt_init ) { \
    vt_init = 0; \
    vt_open(); \
  }

void VT_User_comment_def__(const char* comment)
{
  VT_INIT;

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

  vt_def_comment(VT_CURRENT_THREAD, comment);

  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
}

void VT_User_comment__(const char* comment)
{
  uint64_t time;

  VT_INIT;

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

  time = vt_pform_wtime();
  vt_comment(VT_CURRENT_THREAD, &time, comment);

  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
}

/*
 * Fortran version
 */

VT_DECLDEF(void VT_User_comment_def___f(const char* comment, int cl))
{
  int comlen;
  char fcombuf[1024];

  /* -- convert Fortran to C strings -- */
  comlen = ( cl < 1024 ) ? cl : 1023;
  strncpy(fcombuf, comment, comlen);
  fcombuf[comlen] = '\0';

  VT_User_comment_def__(fcombuf);
} VT_GENERATE_F77_BINDINGS(vt_user_comment_def__, VT_USER_COMMENT_DEF__,
			   VT_User_comment_def___f,
			   (const char* comment, int cl),
			   (comment, cl))

VT_DECLDEF(void VT_User_comment___f(const char* comment, int cl))
{
  int comlen;
  char fcombuf[1024];

  /* -- convert Fortran to C strings -- */
  comlen = ( cl < 1024 ) ? cl : 1023;
  strncpy(fcombuf, comment, comlen);
  fcombuf[comlen] = '\0';

  VT_User_comment__(fcombuf);
} VT_GENERATE_F77_BINDINGS(vt_user_comment__, VT_USER_COMMENT__,
			   VT_User_comment___f,
			   (const char* comment, int cl),
			   (comment, cl))
