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

#include "config.h"

#include <stdlib.h>

#include <pthread.h>

#include "vt_defs.h"
#include "vt_error.h"
#include "vt_pform.h"
#include "vt_pthreadreg.h"
#include "vt_thrd.h"
#include "vt_trc.h"

#define GEN_WRAPFUNC(_fidx, _fname, _rtype, _defarg, _callargs) \
VT_DECLDEF(_rtype VT_ ## _fname ## __ _defarg) {                \
   _rtype rc; uint64_t time;                                    \
   if (vt_init) { vt_init = 0; vt_open(); }                     \
   time = vt_pform_wtime();                                     \
   vt_enter(VT_CURRENT_THREAD, &time, vt_pthread_regid[_fidx]); \
   rc = _fname _callargs;                                       \
   time = vt_pform_wtime();                                     \
   vt_exit(VT_CURRENT_THREAD, &time);                           \
   return rc;                                                   \
}

#define GEN_WRAPFUNC_NORC(_fidx, _fname, _defarg, _callargs)    \
VT_DECLDEF(void VT_ ## _fname ## __ _defarg) {                  \
   uint64_t time;                                               \
   if (vt_init) { vt_init = 0; vt_open(); }                     \
   time = vt_pform_wtime();                                     \
   vt_enter(VT_CURRENT_THREAD, &time, vt_pthread_regid[_fidx]); \
   _fname _callargs;                                            \
   time = vt_pform_wtime();                                     \
   vt_exit(VT_CURRENT_THREAD, &time);                           \
}

static int vt_init = 1; /* is initialization needed? */

/*
 *-----------------------------------------------------------------------------
 *
 * Thread management
 *
 *-----------------------------------------------------------------------------
 */

/* -- pthread_create -- */

struct vt_pthread_pack_struct
{
  void*    (*start_routine)(void*); /* thread's start routine */
  void*    arg;                     /* argument for start routine */
  uint32_t ptid;                    /* parent thread-ID */
};

static void* vt_pthread_function(void* arg)
{
  void* ret;

  struct vt_pthread_pack_struct* pack = (struct vt_pthread_pack_struct*)arg;

  VTThrd_registerThread(pack->ptid);

  ret = pack->start_routine(pack->arg);

  free(arg);

  return ret;
}

VT_DECLDEF(int VT_pthread_create__(pthread_t* thread,
                                   const pthread_attr_t* attr,
                                   void *(*start_routine)(void*), void* arg))
{
  int rc;
  uint64_t time;
  struct vt_pthread_pack_struct* pack;

  if (vt_init)
  {
    vt_init = 0;
    vt_open();
  }

  time = vt_pform_wtime();
  vt_enter(VT_CURRENT_THREAD, &time, vt_pthread_regid[VT__PTHREAD_CREATE]);

  pack = (struct vt_pthread_pack_struct*)malloc(
           sizeof(struct vt_pthread_pack_struct));
  if (pack == NULL)
    vt_error();

  pack->start_routine = start_routine;
  pack->arg = arg;
  pack->ptid = VTThrd_getThreadId();

  rc = pthread_create(thread, attr, vt_pthread_function, (void*)pack);

  time = vt_pform_wtime();
  vt_exit(VT_CURRENT_THREAD, &time);

  return rc;
}

/* -- pthread_join -- */

GEN_WRAPFUNC(VT__PTHREAD_JOIN, pthread_join, int,
	     (pthread_t thread, void **value_ptr),
	     (thread, value_ptr))

/* -- pthread_exit -- */

VT_DECLDEF(void VT_pthread_exit__(void* value_ptr))
{
  uint64_t time;
  int i, stack_level;

  if (vt_init)
  {
    vt_init = 0;
    vt_open();
  }

  time = vt_pform_wtime();
  vt_enter(VT_CURRENT_THREAD, &time, vt_pthread_regid[VT__PTHREAD_EXIT]);

  /* shutdown call stack */
  stack_level = VTTHRD_STACK_LEVEL(VTThrdv[VT_MY_THREAD]);
  for(i = stack_level; i > 0; i--)
  {
    time = vt_pform_wtime();
    vt_exit(VT_CURRENT_THREAD, &time);
  }

  pthread_exit(value_ptr);
}

/* -- pthread_once -- */

GEN_WRAPFUNC(VT__PTHREAD_ONCE, pthread_once, int,
	     (pthread_once_t* once_control, void (*init_routine)(void)),
	     (once_control, init_routine))

/* -- pthread_self -- */

GEN_WRAPFUNC(VT__PTHREAD_SELF, pthread_self, pthread_t,
	     (void),
	     ())

/* -- pthread_equal -- */

GEN_WRAPFUNC(VT__PTHREAD_EQUAL, pthread_equal, int,
	     (pthread_t t1, pthread_t t2),
	     (t1, t2))

/* -- pthread_detach -- */

GEN_WRAPFUNC(VT__PTHREAD_DETACH, pthread_detach, int,
	     (pthread_t thread),
	     (thread))

/*
 *-----------------------------------------------------------------------------
 *
 * Thread-Specific Data
 *
 *-----------------------------------------------------------------------------
 */

/* -- pthread_key_create -- */

GEN_WRAPFUNC(VT__PTHREAD_KEY_CREATE, pthread_key_create, int,
	     (pthread_key_t* key, void (*destructor)(void*)),
	     (key, destructor))

/* -- pthread_key_delete -- */

GEN_WRAPFUNC(VT__PTHREAD_KEY_DELETE, pthread_key_delete, int,
	     (pthread_key_t key),
	     (key))

/* -- pthread_getspecific -- */

GEN_WRAPFUNC(VT__PTHREAD_GETSPECIFIC, pthread_getspecific, void*,
	     (pthread_key_t key),
	     (key))

/* -- pthread_setspecific -- */

GEN_WRAPFUNC(VT__PTHREAD_SETSPECIFIC, pthread_setspecific, int,
	     (pthread_key_t key, const void* value),
	     (key, value))

/*
 *-----------------------------------------------------------------------------
 *
 * Thread Cancellation
 *
 *-----------------------------------------------------------------------------
 */

/* -- pthread_cancel -- */

GEN_WRAPFUNC(VT__PTHREAD_CANCEL, pthread_cancel, int,
	     (pthread_t thread),
	     (thread))

/* -- pthread_setcancelstate -- */

GEN_WRAPFUNC(VT__PTHREAD_SETCANCELSTATE, pthread_setcancelstate, int,
	     (int state, int* oldstate),
	     (state, oldstate))

/* -- pthread_testcancel -- */

GEN_WRAPFUNC_NORC(VT__PTHREAD_TESTCANCEL, pthread_testcancel,
		  (void),
		  ())

/*
 *-----------------------------------------------------------------------------
 *
 * Thread Scheduling
 *
 *-----------------------------------------------------------------------------
 */

/* -- pthread_getschedparam -- */

GEN_WRAPFUNC(VT__PTHREAD_GETSCHEDPARAM, pthread_getschedparam, int,
	     (pthread_t thread, int* policy, struct sched_param* param),
	     (thread, policy, param))

/* -- pthread_setschedparam -- */

GEN_WRAPFUNC(VT__PTHREAD_SETSCHEDPARAM, pthread_setschedparam, int,
	     (pthread_t thread, int policy, const struct sched_param* param),
	     (thread, policy, param))

/*
 *-----------------------------------------------------------------------------
 *
 * Attributes - Basic Management
 *
 *-----------------------------------------------------------------------------
 */

/* -- pthread_attr_init -- */

GEN_WRAPFUNC(VT__PTHREAD_ATTR_INIT, pthread_attr_init, int,
	     (pthread_attr_t* attr),
	     (attr))

/* -- pthread_attr_destroy -- */

GEN_WRAPFUNC(VT__PTHREAD_ATTR_DESTROY, pthread_attr_destroy, int,
	     (pthread_attr_t* attr),
	     (attr))

/*
 *-----------------------------------------------------------------------------
 *
 * Attributes - Detachable of Joinable
 *
 *-----------------------------------------------------------------------------
 */

/* -- pthread_attr_setdetachstate -- */

GEN_WRAPFUNC(VT__PTHREAD_ATTR_SETDETACHSTATE, pthread_attr_setdetachstate, int,
	     (pthread_attr_t* attr, int detachstate),
	     (attr, detachstate))

/* -- pthread_attr_getdetachstate -- */

GEN_WRAPFUNC(VT__PTHREAD_ATTR_GETDETACHSTATE, pthread_attr_getdetachstate, int,
	     (const pthread_attr_t* attr, int* detachstate),
	     (attr, detachstate))

/*
 *-----------------------------------------------------------------------------
 *
 * Attributes - Specifying Stack Information
 *
 *-----------------------------------------------------------------------------
 */

/* -- pthread_attr_getstacksize -- */

GEN_WRAPFUNC(VT__PTHREAD_ATTR_GETSTACKSIZE, pthread_attr_getstacksize, int,
	     (const pthread_attr_t* attr, size_t* stacksize),
	     (attr, stacksize))

/* -- pthread_attr_setstacksize -- */

GEN_WRAPFUNC(VT__PTHREAD_ATTR_SETSTACKSIZE, pthread_attr_setstacksize, int,
	     (pthread_attr_t* attr, size_t stacksize),
	     (attr, stacksize))

/*
 *-----------------------------------------------------------------------------
 *
 * Attributes - Thread Scheduling
 *
 *-----------------------------------------------------------------------------
 */

/* -- pthread_attr_getschedparam -- */

GEN_WRAPFUNC(VT__PTHREAD_ATTR_GETSCHEDPARAM, pthread_attr_getschedparam, int,
	     (const pthread_attr_t* attr, struct sched_param* param),
	     (attr, param))

/* -- pthread_attr_setschedparam -- */

GEN_WRAPFUNC(VT__PTHREAD_ATTR_SETSCHEDPARAM, pthread_attr_setschedparam, int,
	     (pthread_attr_t* attr, const struct sched_param* param),
	     (attr, param))

/* -- pthread_attr_getschedpolicy -- */

GEN_WRAPFUNC(VT__PTHREAD_ATTR_GETSCHEDPOLICY, pthread_attr_getschedpolicy, int,
	     (const pthread_attr_t* attr, int* policy),
	     (attr, policy))

/* -- pthread_attr_setschedpolicy -- */

GEN_WRAPFUNC(VT__PTHREAD_ATTR_SETSCHEDPOLICY, pthread_attr_setschedpolicy, int,
	     (pthread_attr_t* attr, int policy),
	     (attr, policy))

/* -- pthread_attr_setinheritsched -- */

GEN_WRAPFUNC(VT__PTHREAD_ATTR_SETINHERITSCHED, pthread_attr_setinheritsched, int,
	     (pthread_attr_t* attr, int inheritsched),
	     (attr, inheritsched))

/* -- pthread_attr_getinheritsched -- */

GEN_WRAPFUNC(VT__PTHREAD_ATTR_GETINHERITSCHED, pthread_attr_getinheritsched, int,
	     (const pthread_attr_t* attr, int* inheritsched),
	     (attr, inheritsched))

/* -- pthread_attr_setscope -- */

GEN_WRAPFUNC(VT__PTHREAD_ATTR_SETSCOPE, pthread_attr_setscope, int,
	     (pthread_attr_t* attr, int contentionscope),
	     (attr, contentionscope))

/* -- pthread_attr_getscope -- */

GEN_WRAPFUNC(VT__PTHREAD_ATTR_GETSCOPE, pthread_attr_getscope, int,
	     (const pthread_attr_t* attr, int* contentionscope),
	     (attr, contentionscope))

/*
 *-----------------------------------------------------------------------------
 *
 * Mutex Management
 *
 *-----------------------------------------------------------------------------
 */

/* -- pthread_mutex_init -- */

GEN_WRAPFUNC(VT__PTHREAD_MUTEX_INIT, pthread_mutex_init, int,
	     (pthread_mutex_t* mutex, const pthread_mutexattr_t* attr),
	     (mutex, attr))

/* -- pthread_mutex_destroy -- */

GEN_WRAPFUNC(VT__PTHREAD_MUTEX_DESTROY, pthread_mutex_destroy, int,
	     (pthread_mutex_t* mutex),
	     (mutex))

/* -- pthread_mutex_lock -- */

GEN_WRAPFUNC(VT__PTHREAD_MUTEX_LOCK, pthread_mutex_lock, int,
	     (pthread_mutex_t* mutex),
	     (mutex))

/* -- pthread_mutex_unlock -- */

GEN_WRAPFUNC(VT__PTHREAD_MUTEX_UNLOCK, pthread_mutex_unlock, int,
	     (pthread_mutex_t* mutex),
	     (mutex))

/* -- pthread_mutex_trylock -- */

GEN_WRAPFUNC(VT__PTHREAD_MUTEX_TRYLOCK, pthread_mutex_trylock, int,
	     (pthread_mutex_t* mutex),
	     (mutex))

/*
 *-----------------------------------------------------------------------------
 *
 * Mutex - Attributes - Basic Management
 *
 *-----------------------------------------------------------------------------
 */

/* -- pthread_mutexattr_init -- */

GEN_WRAPFUNC(VT__PTHREAD_MUTEXATTR_INIT, pthread_mutexattr_init, int,
	     (pthread_mutexattr_t* attr),
	     (attr))

/* -- pthread_mutexattr_destroy -- */

GEN_WRAPFUNC(VT__PTHREAD_MUTEXATTR_DESTROY, pthread_mutexattr_destroy, int,
	     (pthread_mutexattr_t* attr),
	     (attr))

/*
 *-----------------------------------------------------------------------------
 *
 * Mutex - Attributes - Sharing
 *
 *-----------------------------------------------------------------------------
 */

#if defined(HAVE_PTHREAD_MUTEXATTR_GETPSHARED) && HAVE_PTHREAD_MUTEXATTR_GETPSHARED

/* -- pthread_mutexattr_getpshared -- */

GEN_WRAPFUNC(VT__PTHREAD_MUTEXATTR_GETPSHARED, pthread_mutexattr_getpshared, int,
	     (const pthread_mutexattr_t* attr, int* pshared),
	     (attr, pshared))

#endif /* HAVE_PTHREAD_MUTEXATTR_GETPSHARED */

#if defined(HAVE_PTHREAD_MUTEXATTR_SETPSHARED) && HAVE_PTHREAD_MUTEXATTR_SETPSHARED

/* -- pthread_mutexattr_setpshared -- */

GEN_WRAPFUNC(VT__PTHREAD_MUTEXATTR_SETPSHARED, pthread_mutexattr_setpshared, int,
	     (pthread_mutexattr_t* attr, int pshared),
	     (attr, pshared))

#endif /* HAVE_PTHREAD_MUTEXATTR_SETPSHARED */

/*
 *-----------------------------------------------------------------------------
 *
 * Condition - Basic Management
 *
 *-----------------------------------------------------------------------------
 */

/* -- pthread_cond_init -- */

GEN_WRAPFUNC(VT__PTHREAD_COND_INIT, pthread_cond_init, int,
	     (pthread_cond_t* cond, const pthread_condattr_t* attr),
	     (cond, attr))

/* -- pthread_cond_destroy -- */

GEN_WRAPFUNC(VT__PTHREAD_COND_DESTROY, pthread_cond_destroy, int,
	     (pthread_cond_t* cond),
	     (cond))

/* -- pthread_cond_signal -- */

GEN_WRAPFUNC(VT__PTHREAD_COND_SIGNAL, pthread_cond_signal, int,
	     (pthread_cond_t* cond),
	     (cond))

/* -- pthread_cond_broadcast -- */

GEN_WRAPFUNC(VT__PTHREAD_COND_BROADCAST, pthread_cond_broadcast, int,
	     (pthread_cond_t* cond),
	     (cond))

/* -- pthread_cond_wait -- */

GEN_WRAPFUNC(VT__PTHREAD_COND_WAIT, pthread_cond_wait, int,
	     (pthread_cond_t* cond,pthread_mutex_t* mutex),
	     (cond, mutex))

/* -- pthread_cond_timedwait -- */

GEN_WRAPFUNC(VT__PTHREAD_COND_TIMEDWAIT, pthread_cond_timedwait, int,
	     (pthread_cond_t* cond, pthread_mutex_t* mutex,
	      struct timespec* abstime),
	     (cond, mutex, abstime))

/*
 *-----------------------------------------------------------------------------
 *
 * Condition - Variable Attributes - Basic Management
 *
 *-----------------------------------------------------------------------------
 */

/* -- pthread_condattr_init -- */

GEN_WRAPFUNC(VT__PTHREAD_CONDATTR_INIT, pthread_condattr_init, int,
	     (pthread_condattr_t* attr),
	     (attr))

/* -- pthread_condattr_destroy -- */

GEN_WRAPFUNC(VT__PTHREAD_CONDATTR_DESTROY, pthread_condattr_destroy, int,
	     (pthread_condattr_t* attr),
	     (attr))

/*
 *-----------------------------------------------------------------------------
 *
 * Condition - Variable Attributes - Sharing
 *
 *-----------------------------------------------------------------------------
 */

#if defined(HAVE_PTHREAD_CONDATTR_GETPSHARED) && HAVE_PTHREAD_CONDATTR_GETPSHARED

/* -- pthread_condattr_getpshared -- */

GEN_WRAPFUNC(VT__PTHREAD_CONDATTR_GETPSHARED, pthread_condattr_getpshared, int,
	     (const pthread_condattr_t* attr, int* pshared),
	     (attr, pshared))

#endif /* HAVE_PTHREAD_CONDATTR_GETPSHARED */

#if defined(HAVE_PTHREAD_CONDATTR_SETPSHARED) && HAVE_PTHREAD_CONDATTR_SETPSHARED

/* -- pthread_condattr_setpshared -- */

GEN_WRAPFUNC(VT__PTHREAD_CONDATTR_SETPSHARED, pthread_condattr_setpshared, int,
	     (pthread_condattr_t* attr, int pshared),
	     (attr, pshared))

#endif /* HAVE_PTHREAD_CONDATTR_SETPSHARED */
