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

#include "vt_pthreadreg.h"

#include "vt_defs.h"
#include "vt_trc.h"

#include <stdlib.h>

int vt_pthread_regid[VT__PTHREAD_REGID_NUM];

void vt_pthread_register()
{
  uint32_t fid;

  fid = vt_def_scl_file(VT_CURRENT_THREAD, "Pthread");

  vt_pthread_regid[VT__PTHREAD_CREATE] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_create", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_JOIN] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_join", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_EXIT] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_exit", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_ONCE] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_once", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_SELF] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_self", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_EQUAL] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_equal", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_DETACH] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_detach", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_KEY_CREATE] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_key_create", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_KEY_DELETE] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_key_delete", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_GETSPECIFIC] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_getspecific", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_SETSPECIFIC] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_setspecific", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_CANCEL] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_cancel", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_SETCANCELSTATE] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_setcancelstate", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_TESTCANCEL] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_testcancel", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_GETSCHEDPARAM] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_getschedparam", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_SETSCHEDPARAM] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_setschedparam", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_ATTR_INIT] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_attr_init", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_ATTR_DESTROY] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_attr_destroy", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_ATTR_SETDETACHSTATE] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_attr_setdetachstate", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_ATTR_GETDETACHSTATE] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_attr_getdetachstate", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_ATTR_GETSTACKSIZE] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_attr_getstacksize", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_ATTR_SETSTACKSIZE] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_attr_setstacksize", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_ATTR_GETSCHEDPARAM] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_attr_getschedparam", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_ATTR_SETSCHEDPARAM] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_attr_setschedparam", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_ATTR_GETSCHEDPOLICY] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_attr_getschedpolicy", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_ATTR_SETSCHEDPOLICY] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_attr_setschedpolicy", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_ATTR_SETINHERITSCHED] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_attr_setinheritsched", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_ATTR_GETINHERITSCHED] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_attr_getinheritsched", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_ATTR_SETSCOPE] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_attr_setscope", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_ATTR_GETSCOPE] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_attr_getscope", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_MUTEX_INIT] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_mutex_init", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_MUTEX_DESTROY] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_mutex_destroy", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_MUTEX_LOCK] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_mutex_lock", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_MUTEX_UNLOCK] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_mutex_unlock", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_MUTEX_TRYLOCK] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_mutex_trylock", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_MUTEXATTR_INIT] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_mutexattr_init", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_MUTEXATTR_DESTROY] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_mutexattr_destroy", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_MUTEXATTR_GETPSHARED] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_mutexattr_getpshared", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_MUTEXATTR_SETPSHARED] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_mutexattr_setpshared", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_COND_INIT] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_cond_init", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_COND_DESTROY] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_cond_destroy", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_COND_SIGNAL] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_cond_signal", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_COND_BROADCAST] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_cond_broadcast", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_COND_WAIT] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_cond_wait", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_COND_TIMEDWAIT] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_cond_timedwait", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_CONDATTR_INIT] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_condattr_init", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_CONDATTR_DESTROY] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_condattr_destroy", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_CONDATTR_GETPSHARED] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_condattr_getpshared", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);

  vt_pthread_regid[VT__PTHREAD_CONDATTR_SETPSHARED] =
    vt_def_region(VT_CURRENT_THREAD, "pthread_condattr_setpshared", fid, VT_NO_LNO, VT_NO_LNO, NULL,
                  VT_PTHRD_FUNCTION);
}
