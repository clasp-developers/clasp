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

#include "vt_defs.h"
#include "vt_error.h"
#include "vt_fbindings.h"
#include "vt_inttypes.h"
#include "vt_mallocwrap.h"
#include "vt_pform.h"
#include "vt_thrd.h"
#include "vt_trc.h"
#define VTRACE
#undef VTRACE_NO_MARKER
#include "vt_user.h"

#include <string.h>

static int vt_init = 1;        /* is initialization needed? */

#define VT_INIT \
  if ( vt_init ) { \
    vt_init = 0; \
    vt_open(); \
  }

unsigned int VT_User_marker_def__(const char* mname, int mtype)
{
  uint32_t mid;
  uint32_t _mtype = VT_MARKER_UNKNOWN;

  VT_INIT;

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

  switch(mtype)
  {
    case VT_MARKER_TYPE_ERROR:
    {
      _mtype = VT_MARKER_ERROR;
      break;
    }
    case VT_MARKER_TYPE_WARNING:
    {
      _mtype = VT_MARKER_WARNING;
      break;
    }
    case VT_MARKER_TYPE_HINT:
    {
      _mtype = VT_MARKER_HINT;
      break;
    }
    default:
    {
      vt_error_msg("Unknown marker type %i", mtype);
      break;
    }
  }

#if (defined(VT_MT) || defined(VT_HYB))
  VTTHRD_LOCK_IDS();
#endif
  mid = vt_def_marker(VT_CURRENT_THREAD, mname, _mtype);
#if (defined(VT_MT) || defined(VT_HYB))
  VTTHRD_UNLOCK_IDS();
#endif

  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);

  return mid;
}

void VT_User_marker__(unsigned int mid, const char* mtext)
{
  uint64_t time;

  VT_INIT;

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

  time = vt_pform_wtime();
  vt_marker(VT_CURRENT_THREAD, &time, mid, mtext);

  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
}

/*
 * Fortran version
 */

VT_DECLDEF(void VT_User_marker_def___f(const char* mname, int* mtype,
				       unsigned int* mid, int nl))
{
  int namlen;
  char fnambuf[128];

  /* -- convert Fortran to C strings -- */
  namlen = ( nl < 128 ) ? nl : 127;
  strncpy(fnambuf, mname, namlen);
  fnambuf[namlen] = '\0';

  *mid = VT_User_marker_def__(fnambuf, *mtype);
} VT_GENERATE_F77_BINDINGS(vt_user_marker_def__, VT_USER_MARKER_DEF__,
			   VT_User_marker_def___f,
			   (const char* mname, int* mtype, unsigned int* mid, int nl),
			   (mname, mtype, mid, nl))

VT_DECLDEF(void VT_User_marker___f(unsigned int* mid, const char* mtext,
				   int tl))
{
  int texlen;
  char ftexbuf[1024];

  /* -- convert Fortran to C strings -- */
  texlen = ( tl < 1024 ) ? tl : 1023;
  strncpy(ftexbuf, mtext, texlen);
  ftexbuf[texlen] = '\0';

  VT_User_marker__(*mid, ftexbuf);
} VT_GENERATE_F77_BINDINGS(vt_user_marker__, VT_USER_MARKER__,
			   VT_User_marker___f,
			   (unsigned int* mid, const char* mtext, int tl),
			   (mid, mtext, tl))
