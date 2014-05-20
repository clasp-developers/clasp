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

#include "otf.h"

#include "vt_defs.h"
#include "vt_error.h"
#include "vt_fbindings.h"
#include "vt_inttypes.h"
#include "vt_mallocwrap.h"
#include "vt_pform.h"
#include "vt_thrd.h"
#include "vt_trc.h"
#define VTRACE
#undef VTRACE_NO_COUNT
#include "vt_user.h"

static int vt_init = 1;        /* is initialization needed? */
static uint32_t def_gid = 0;   /* default counter group id */

#define VT_INIT \
  if ( vt_init ) { \
    vt_init = 0; \
    vt_open(); \
  }

unsigned int VT_User_count_group_def__(const char* gname)
{
  uint32_t gid;

  VT_INIT;

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

#if (defined(VT_MT) || defined(VT_HYB))
  VTTHRD_LOCK_IDS();
#endif
  gid = vt_def_counter_group(VT_CURRENT_THREAD, gname);
#if (defined(VT_MT) || defined(VT_HYB))
  VTTHRD_UNLOCK_IDS();
#endif

  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);

  return gid;
}

unsigned int VT_User_count_def__(const char* cname, const char* cunit, int ctype,
				 unsigned int gid)
{
  uint32_t cid;
  uint32_t cprop = VT_CNTR_ABS | VT_CNTR_NEXT;

  VT_INIT;

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

  if (gid == (uint32_t)VT_COUNT_DEFGROUP)
  {
    if (def_gid == 0)
      def_gid = VT_User_count_group_def__("User");

    gid = def_gid;
  }

  switch(ctype)
  {
    case VT_COUNT_TYPE_SIGNED:
    case VT_COUNT_TYPE_INTEGER:
    case VT_COUNT_TYPE_INTEGER8:
    {
      cprop |= VT_CNTR_SIGNED;
      break;
    }
    case VT_COUNT_TYPE_UNSIGNED:
    {
      cprop |= VT_CNTR_UNSIGNED;
      break;
    }
    case VT_COUNT_TYPE_FLOAT:
    case VT_COUNT_TYPE_REAL:
    {
      cprop |= VT_CNTR_FLOAT;
      break;
    }
    case VT_COUNT_TYPE_DOUBLE:
    {
      cprop |= VT_CNTR_DOUBLE;
      break;
    }
    default:
    {
      vt_error_msg("Unknown counter type %i", ctype);
      break;
    }
  }

#if (defined(VT_MT) || defined(VT_HYB))
  VTTHRD_LOCK_IDS();
#endif
  cid = vt_def_counter(VT_CURRENT_THREAD, cname, cunit, cprop, gid, 0);
#if (defined(VT_MT) || defined(VT_HYB))
  VTTHRD_UNLOCK_IDS();
#endif

  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);

  return cid;
}

void VT_User_count_signed_val__(unsigned int cid, long long val)
{
  uint64_t time;
  uint64_t cval;

  VT_INIT;

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

  time = vt_pform_wtime();
  cval = OTF_Signed2Counter((int64_t)val);
  vt_count(VT_CURRENT_THREAD, &time, cid, cval);

  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
}

void VT_User_count_unsigned_val__(unsigned int cid, unsigned long long val)
{
  uint64_t time;
  uint64_t cval;

  VT_INIT;

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

  time = vt_pform_wtime();
  cval = OTF_Unsigned2Counter((uint64_t)val);
  vt_count(VT_CURRENT_THREAD, &time, cid, cval);

  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
}

void VT_User_count_float_val__(unsigned int cid, float val)
{
  uint64_t time;
  uint64_t cval;

  VT_INIT;

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

  time = vt_pform_wtime();
  cval = OTF_Float2Counter(val);
  vt_count(VT_CURRENT_THREAD, &time, cid, cval);

  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
}

void VT_User_count_double_val__(unsigned int cid, double val)
{
  uint64_t time;
  uint64_t cval;

  VT_INIT;

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

  time = vt_pform_wtime();
  cval = OTF_Double2Counter(val);
  vt_count(VT_CURRENT_THREAD, &time, cid, cval);

  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
}

/*
 * Fortran version
 */

VT_DECLDEF(void VT_User_count_group_def___f(const char* gname,
					    unsigned int* gid, int nl))
{
  int namlen;
  char fnambuf[128];

  /* -- convert Fortran to C strings -- */
  namlen = ( nl < 128 ) ? nl : 127;
  strncpy(fnambuf, gname, namlen);
  fnambuf[namlen] = '\0';

  *gid = VT_User_count_group_def__(fnambuf);
} VT_GENERATE_F77_BINDINGS(vt_user_count_group_def__, VT_USER_COUNT_GROUP_DEF__,
			   VT_User_count_group_def___f,
			   (const char* gname, unsigned int* gid, int nl),
			   (gname, gid, nl))

VT_DECLDEF(void VT_User_count_def___f(const char* cname, const char* cunit,
				      int* ctype, unsigned int* gid,
				      unsigned int* cid,
				      int nl, int ul))
{
  int namlen;
  int unilen;
  char fnambuf[128];
  char funibuf[128];

 /* -- convert Fortran to C strings -- */
  namlen = ( nl < 128 ) ? nl : 127;
  unilen = ( ul < 128 ) ? ul : 127;
  strncpy(fnambuf, cname, namlen);
  fnambuf[namlen] = '\0';
  strncpy(funibuf, cunit, unilen);
  funibuf[unilen] = '\0';

  *cid = VT_User_count_def__(fnambuf, funibuf, *ctype, *gid);
} VT_GENERATE_F77_BINDINGS(vt_user_count_def__, VT_USER_COUNT_DEF__,
			   VT_User_count_def___f,
			   (const char* cname, const char* cunit, int* ctype, unsigned int* gid, unsigned int* cid, int nl, int ul),
			   (cname, cunit, ctype, gid, cid, nl, ul))


VT_DECLDEF(void VT_User_count_integer_val___f(unsigned int* cid, int* val))
{
  uint64_t time;
  uint64_t cval;

  VT_INIT;

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

  time = vt_pform_wtime();
  cval = OTF_Signed2Counter((int64_t)(*val));
  vt_count(VT_CURRENT_THREAD, &time, *cid, cval);

  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
} VT_GENERATE_F77_BINDINGS(vt_user_count_integer_val__,
			   VT_USER_COUNT_INTEGER_VAL__,
			   VT_User_count_integer_val___f,
			   (unsigned int* cid, int* val),
			   (cid, val))

VT_DECLDEF(void VT_User_count_integer8_val___f(unsigned int* cid,
					       long long* val))
{
  uint64_t time;
  uint64_t cval;

  VT_INIT;

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

  time = vt_pform_wtime();
  cval = OTF_Signed2Counter((int64_t)(*val));
  vt_count(VT_CURRENT_THREAD, &time, *cid, cval);

  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
} VT_GENERATE_F77_BINDINGS(vt_user_count_integer8_val__,
			   VT_USER_COUNT_INTEGER8_VAL__,
			   VT_User_count_integer8_val___f,
			   (unsigned int* cid, long long* val),
			   (cid, val))

VT_DECLDEF(void VT_User_count_real_val___f(unsigned int* cid, float* val))
{
  uint64_t time;
  uint64_t cval;

  VT_INIT;

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

  time = vt_pform_wtime();
  cval = OTF_Float2Counter(*val);
  vt_count(VT_CURRENT_THREAD, &time, *cid, cval);

  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
} VT_GENERATE_F77_BINDINGS(vt_user_count_real_val__,
			   VT_USER_COUNT_real_VAL__,
			   VT_User_count_real_val___f,
			   (unsigned int* cid, float* val),
			   (cid, val))

VT_DECLDEF(void VT_User_count_double_val___f(unsigned int* cid, double* val))
{
  uint64_t time;
  uint64_t cval;

  VT_INIT;

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

  time = vt_pform_wtime();
  cval = OTF_Double2Counter(*val);
  vt_count(VT_CURRENT_THREAD, &time, *cid, cval);

  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
} VT_GENERATE_F77_BINDINGS(vt_user_count_double_val__,
			   VT_USER_COUNT_double_VAL__,
			   VT_User_count_double_val___f,
			   (unsigned int* cid, double* val),
			   (cid, val))
