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

#ifndef _VT_WRAP_PTHREAD_H
#define _VT_WRAP_PTHREAD_H

#include <pthread.h>

__VT_EXTERN_DECL int VT_pthread_create__(pthread_t* thread, const pthread_attr_t* attr,
                               void *(*start_routine)(void*), void* arg);
__VT_EXTERN_DECL int VT_pthread_join__(pthread_t thread, void** value_ptr);
__VT_EXTERN_DECL int VT_pthread_exit__(void* value_ptr);
__VT_EXTERN_DECL int VT_pthread_once__(pthread_once_t* once_control,
                             void (*init_routine)(void));
__VT_EXTERN_DECL pthread_t VT_pthread_self__(void);
__VT_EXTERN_DECL int VT_pthread_equal__(pthread_t t1, pthread_t t2);
__VT_EXTERN_DECL int VT_pthread_detach__(pthread_t thread);
__VT_EXTERN_DECL int VT_pthread_key_create__(pthread_key_t* key,
                                   void (*destructor)(void*));
__VT_EXTERN_DECL int VT_pthread_key_delete__(pthread_key_t key);
__VT_EXTERN_DECL void* VT_pthread_getspecific__(pthread_key_t key);
__VT_EXTERN_DECL int VT_pthread_setspecific__(pthread_key_t key, const void* value);
__VT_EXTERN_DECL int VT_pthread_cancel__(pthread_t thread);
__VT_EXTERN_DECL int VT_pthread_setcancelstate__(int state, int* oldstate);
__VT_EXTERN_DECL void VT_pthread_testcancel__(void);
__VT_EXTERN_DECL int VT_pthread_getschedparam__(pthread_t thread, int* policy,
                                      struct sched_param* param);
__VT_EXTERN_DECL int VT_pthread_setschedparam__(pthread_t thread, int policy,
                                      const struct sched_param* param);
__VT_EXTERN_DECL int VT_pthread_attr_init__(pthread_attr_t* attr);
__VT_EXTERN_DECL int VT_pthread_attr_destroy__(pthread_attr_t* attr);
__VT_EXTERN_DECL int VT_pthread_attr_setdetachstate__(pthread_attr_t* attr,
                                            int detachstate);
__VT_EXTERN_DECL int VT_pthread_attr_getdetachstate__(const pthread_attr_t* attr,
                                            int* detachstate);
__VT_EXTERN_DECL int VT_pthread_attr_getstacksize__(const pthread_attr_t* attr,
                                          size_t* stacksize);
__VT_EXTERN_DECL int VT_pthread_attr_setstacksize__(pthread_attr_t* attr,
                                          size_t stacksize);
__VT_EXTERN_DECL int VT_pthread_attr_getschedparam__(const pthread_attr_t* attr,
                                           struct sched_param* param);
__VT_EXTERN_DECL int VT_pthread_attr_setschedparam__(pthread_attr_t* attr,
                                           const struct sched_param* param);
__VT_EXTERN_DECL int VT_pthread_attr_getschedpolicy__(const pthread_attr_t* attr,
                                            int* policy);
__VT_EXTERN_DECL int VT_pthread_attr_setschedpolicy__(pthread_attr_t* attr, int policy);
__VT_EXTERN_DECL int VT_pthread_attr_setinheritsched__(pthread_attr_t* attr,
                                             int inheritsched);
__VT_EXTERN_DECL int VT_pthread_attr_getinheritsched__(const pthread_attr_t* attr,
                                             int* inheritsched);
__VT_EXTERN_DECL int VT_pthread_attr_setscope__(pthread_attr_t* attr,
                                      int contentionscope);
__VT_EXTERN_DECL int VT_pthread_attr_getscope__(const pthread_attr_t* attr,
                                      int* contentionscope);
__VT_EXTERN_DECL int VT_pthread_mutex_init__(pthread_mutex_t* mutex,
                                   const pthread_mutexattr_t* attr);
__VT_EXTERN_DECL int VT_pthread_mutex_destroy__(pthread_mutex_t* mutex);
__VT_EXTERN_DECL int VT_pthread_mutex_lock__(pthread_mutex_t* mutex);
__VT_EXTERN_DECL int VT_pthread_mutex_unlock__(pthread_mutex_t* mutex);
__VT_EXTERN_DECL int VT_pthread_mutex_trylock__(pthread_mutex_t* mutex);
__VT_EXTERN_DECL int VT_pthread_mutexattr_init__(pthread_mutexattr_t* attr);
__VT_EXTERN_DECL int VT_pthread_mutexattr_destroy__(pthread_mutexattr_t* attr);
__VT_EXTERN_DECL int VT_pthread_mutexattr_getpshared__(const pthread_mutexattr_t* attr,
                                             int* pshared);
__VT_EXTERN_DECL int VT_pthread_mutexattr_setpshared__(pthread_mutexattr_t* attr,
                                             int pshared);
__VT_EXTERN_DECL int VT_pthread_cond_init__(pthread_cond_t* cond,
                                  const pthread_condattr_t* attr);
__VT_EXTERN_DECL int VT_pthread_cond_destroy__(pthread_cond_t* cond);
__VT_EXTERN_DECL int VT_pthread_cond_signal__(pthread_cond_t* cond);
__VT_EXTERN_DECL int VT_pthread_cond_broadcast__(pthread_cond_t* cond);
__VT_EXTERN_DECL int VT_pthread_cond_wait__(pthread_cond_t* cond,
                                  pthread_mutex_t* mutex);
__VT_EXTERN_DECL int VT_pthread_cond_timedwait__(pthread_cond_t* cond,
                                       pthread_mutex_t* mutex,
                                       const struct timespec* abstime);
__VT_EXTERN_DECL int VT_pthread_condattr_init__(pthread_condattr_t* attr);
__VT_EXTERN_DECL int VT_pthread_condattr_destroy__(pthread_condattr_t* attr);
__VT_EXTERN_DECL int VT_pthread_condattr_getpshared__(const pthread_condattr_t* attr,
                                            int* pshared);
__VT_EXTERN_DECL int VT_pthread_condattr_setpshared__(pthread_condattr_t* attr,
                                            int pshared);

#define pthread_create VT_pthread_create__
#define pthread_join VT_pthread_join__
#define pthread_exit VT_pthread_exit__
#define pthread_once VT_pthread_once__
#define pthread_self VT_pthread_self__
#define pthread_equal VT_pthread_equal__
#define pthread_detach VT_pthread_detach__
#define pthread_key_create VT_pthread_key_create__
#define pthread_key_delete VT_pthread_key_delete__
#define pthread_getspecific VT_pthread_getspecific__
#define pthread_setspecific VT_pthread_setspecific__
#define pthread_cancel VT_pthread_cancel__
#define pthread_setcancelstate VT_pthread_setcancelstate__
#define pthread_testcancel VT_pthread_testcancel__
#define pthread_getschedparam VT_pthread_getschedparam__
#define pthread_setschedparam VT_pthread_setschedparam__
#define pthread_attr_init VT_pthread_attr_init__
#define pthread_attr_destroy VT_pthread_attr_destroy__
#define pthread_attr_setdetachstate VT_pthread_attr_setdetachstate__
#define pthread_attr_getdetachstate VT_pthread_attr_getdetachstate__
#define pthread_attr_getstacksize VT_pthread_attr_getstacksize__
#define pthread_attr_setstacksize VT_pthread_attr_setstacksize__
#define pthread_attr_getschedparam VT_pthread_attr_getschedparam__
#define pthread_attr_setschedparam VT_pthread_attr_setschedparam__
#define pthread_attr_getschedpolicy VT_pthread_attr_getschedpolicy__
#define pthread_attr_setschedpolicy VT_pthread_attr_setschedpolicy__
#define pthread_attr_setinheritsched VT_pthread_attr_setinheritsched__
#define pthread_attr_getinheritsched VT_pthread_attr_getinheritsched__
#define pthread_attr_setscope VT_pthread_attr_setscope__
#define pthread_attr_getscope VT_pthread_attr_getscope__
#define pthread_mutex_init VT_pthread_mutex_init__
#define pthread_mutex_destroy VT_pthread_mutex_destroy__
#define pthread_mutex_lock VT_pthread_mutex_lock__
#define pthread_mutex_unlock VT_pthread_mutex_unlock__
#define pthread_mutex_trylock VT_pthread_mutex_trylock__
#define pthread_mutexattr_init VT_pthread_mutexattr_init__
#define pthread_mutexattr_destroy VT_pthread_mutexattr_destroy__
#define pthread_mutexattr_getpshared VT_pthread_mutexattr_getpshared__
#define pthread_mutexattr_setpshared VT_pthread_mutexattr_setpshared__
#define pthread_cond_init VT_pthread_cond_init__
#define pthread_cond_destroy VT_pthread_cond_destroy__
#define pthread_cond_signal VT_pthread_cond_signal__
#define pthread_cond_broadcast VT_pthread_cond_broadcast__
#define pthread_cond_wait VT_pthread_cond_wait__
#define pthread_cond_timedwait VT_pthread_cond_timedwait__
#define pthread_condattr_init VT_pthread_condattr_init__
#define pthread_condattr_destroy VT_pthread_condattr_destroy__
#define pthread_condattr_getpshared VT_pthread_condattr_getpshared__
#define pthread_condattr_setpshared VT_pthread_condattr_setpshared__

#endif /* _VT_WRAP_PTHREAD_H */
