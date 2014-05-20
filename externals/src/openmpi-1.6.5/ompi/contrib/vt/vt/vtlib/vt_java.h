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

#ifndef _VT_JAVA_H
#define _VT_JAVA_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

#include "vt_jvmti.h"

#define VT_JAVA_CHECK_ERROR(jvmti, error, prefix)                             \
  if ( error != JVMTI_ERROR_NONE )                                            \
    vt_java_error((jvmti), (error), (prefix));

typedef struct
{
  jvmtiEnv*     jvmti;
  jint          jvmti_version;
  jrawMonitorID lock;
  jboolean      vm_is_started;
  jboolean      vm_is_initialized;
  jboolean      vm_is_dead;

} VTJVMAgent;

EXTERN void vt_java_get_thread_name(jvmtiEnv* jvmti, jthread thread,
                                    char* tname, int maxlen);

EXTERN void vt_java_error(jvmtiEnv* jvmti, jvmtiError error,
                          const char* prefix);

EXTERN VTJVMAgent* vt_jvmti_agent;

#endif /* _VT_JAVA_H */



















