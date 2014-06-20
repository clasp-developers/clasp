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

#ifndef _VT_PTHREADREG_H
#define _VT_PTHREADREG_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

#define VT__PTHREAD_CREATE                       0
#define VT__PTHREAD_JOIN                         1
#define VT__PTHREAD_EXIT                         2
#define VT__PTHREAD_ONCE                         3
#define VT__PTHREAD_SELF                         4
#define VT__PTHREAD_EQUAL                        5
#define VT__PTHREAD_DETACH                       6
#define VT__PTHREAD_KEY_CREATE                   7
#define VT__PTHREAD_KEY_DELETE                   8
#define VT__PTHREAD_GETSPECIFIC                  9
#define VT__PTHREAD_SETSPECIFIC                 10
#define VT__PTHREAD_CANCEL                      11
#define VT__PTHREAD_SETCANCELSTATE              12
#define VT__PTHREAD_TESTCANCEL                  13
#define VT__PTHREAD_GETSCHEDPARAM               14
#define VT__PTHREAD_SETSCHEDPARAM               15
#define VT__PTHREAD_ATTR_INIT                   16
#define VT__PTHREAD_ATTR_DESTROY                17
#define VT__PTHREAD_ATTR_SETDETACHSTATE         18
#define VT__PTHREAD_ATTR_GETDETACHSTATE         19
#define VT__PTHREAD_ATTR_GETSTACKSIZE           20
#define VT__PTHREAD_ATTR_SETSTACKSIZE           21
#define VT__PTHREAD_ATTR_GETSCHEDPARAM          22
#define VT__PTHREAD_ATTR_SETSCHEDPARAM          23
#define VT__PTHREAD_ATTR_GETSCHEDPOLICY         24
#define VT__PTHREAD_ATTR_SETSCHEDPOLICY         25
#define VT__PTHREAD_ATTR_SETINHERITSCHED        26
#define VT__PTHREAD_ATTR_GETINHERITSCHED        27
#define VT__PTHREAD_ATTR_SETSCOPE               28
#define VT__PTHREAD_ATTR_GETSCOPE               29
#define VT__PTHREAD_MUTEX_INIT                  30
#define VT__PTHREAD_MUTEX_DESTROY               31
#define VT__PTHREAD_MUTEX_LOCK                  32
#define VT__PTHREAD_MUTEX_UNLOCK                33
#define VT__PTHREAD_MUTEX_TRYLOCK               34
#define VT__PTHREAD_MUTEXATTR_INIT              35
#define VT__PTHREAD_MUTEXATTR_DESTROY           36
#define VT__PTHREAD_MUTEXATTR_GETPSHARED        37
#define VT__PTHREAD_MUTEXATTR_SETPSHARED        38
#define VT__PTHREAD_COND_INIT                   39
#define VT__PTHREAD_COND_DESTROY                40
#define VT__PTHREAD_COND_SIGNAL                 41
#define VT__PTHREAD_COND_BROADCAST              42
#define VT__PTHREAD_COND_WAIT                   43
#define VT__PTHREAD_COND_TIMEDWAIT              44
#define VT__PTHREAD_CONDATTR_INIT               45
#define VT__PTHREAD_CONDATTR_DESTROY            46
#define VT__PTHREAD_CONDATTR_GETPSHARED         47
#define VT__PTHREAD_CONDATTR_SETPSHARED         48
#define VT__PTHREAD_REGID_NUM                   49

extern int     vt_pthread_regid[VT__PTHREAD_REGID_NUM];

EXTERN void    vt_pthread_register(void);

#endif /* _VT_PTHREADREG_H */
