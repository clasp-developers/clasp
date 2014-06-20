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

#define _GNU_SOURCE  /* possibly needed for execvpe */
#define _SVID_SOURCE /* possibly needed for waitid */
#define _BSD_SOURCE  /* possibly needed for wait<3|4> */

#include "config.h"

#include "vt_defs.h"
#include "vt_env.h"
#include "vt_error.h"
#include "vt_execwrap.h"
#include "vt_fork.h"
#include "vt_inttypes.h"
#include "vt_libwrap.h"
#include "vt_mallocwrap.h"
#include "vt_pform.h"
#include "vt_thrd.h"
#include "vt_trc.h"
#ifdef VT_DYNINST
# include "vt_dyninst.h"
#endif /* VT_DYNINST */

#include <dlfcn.h>
#include <errno.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

#if (defined(HAVE_WAIT3) && HAVE_WAIT3) || (defined(HAVE_WAIT4) && HAVE_WAIT4)
# include <sys/resource.h>
# include <sys/time.h>
#endif /* HAVE_WAIT3 || HAVE_WAIT4 */

#if !(defined(HAVE___WAIT_STATUS) && HAVE___WAIT_STATUS)
# define __WAIT_STATUS int*
#endif /* HAVE___WAIT_STATUS */


/* tracing fork and exec* functions is only supported for serial programs;
   using the following macro to check whether we can intercept these
   functions */
#if (!defined (VT_MPI) && !defined (VT_MT) && !defined(VT_HYB) && !defined(VT_JAVA))
# define EXECWRAP_EXEC_AND_FORK
#endif /* !VT_MPI && !VT_MT && !VT_HYB && !VT_JAVA */


/* special version of VT_LIBWRAP_FUNC_INIT w/o getting the unique function
   identifier (VT_LIBWRAP_FUNC_ID) */
#define EXECWRAP_FUNC_INIT(_func, _rettype, _argtypes)                        \
  _VT_LIBWRAP_FUNC_INIT_DECL_VARS(_func, _rettype, _argtypes);                \
  if( execwrap_lw == VT_LIBWRAP_NULL ) {                                      \
    VTLibwrap_create(&execwrap_lw, &execwrap_lw_attr);                        \
  }                                                                           \
  if( VT_LIBWRAP_FUNC_PTR == VT_LIBWRAP_NULL ) {                              \
    VTLibwrap_func_init(execwrap_lw, VT_LIBWRAP_FUNC_NAME, NULL, 0,           \
                        (void**)(&VT_LIBWRAP_FUNC_PTR), NULL);                \
  }

/* get unique function identifier
   (sets VT_LIBWRAP_FUNC_ID, must be called after EXECWRAP_FUNC_INIT!) */
#define EXECWRAP_GET_FUNC_ID()                                                \
  if( VT_LIBWRAP_FUNC_ID == VT_LIBWRAP_NOID ) {                               \
    VTLibwrap_func_init(execwrap_lw, VT_LIBWRAP_FUNC_NAME, NULL, 0,           \
      NULL, &VT_LIBWRAP_FUNC_ID);                                             \
  }

/* simplified version of VT_LIBWRAP_FUNC_CALL w/o argument for the library
   wrapper object */
#define EXECWRAP_FUNC_CALL(_args) VT_LIBWRAP_FUNC_CALL(execwrap_lw, _args)

/* check whether tracing of LIBC exec functions is currently enabled */
#define EXECWRAP_DO_TRACE()                                                   \
  ( vt_is_alive && VT_MY_THREAD_IS_ALIVE &&                                   \
    VTTHRD_EXEC_TRACING_ENABLED(VTTHRD_MY_VTTHRD) )

/* maximum number of variable arguments for the execl* functions */
#define EXECWRAP_EXECL_MAX_ARGS 1024


/* library wrapper object */
static VTLibwrap* execwrap_lw = VT_LIBWRAP_NULL;

/* library wrapper attributes */
static VTLibwrapAttr execwrap_lw_attr = {

  /* The functions to be wrapped are defined in the LIBC which is (dl)opened
     once by the underlying library wrapping infrastructure. So there is
     no need to search the actual function pointers in an additional library. */
  0,        /* shlibs_num */
  { NULL }, /* shlibs */

  /* Function group to define, finally including the recorded LIBC exec
     functions */
  "LIBC-EXEC", /* func_group */

  /* Search for the actual function pointers in the LIBC, (dl)opened once by
     the underlying library wrapping infrastructure */
  1, /* libc */

  /* Do not initialize VampirTrace when creating the library wrapper object,
     resp. when a wrapper function is entered */
  1 /* wait_for_init */
};

#ifdef EXECWRAP_EXEC_AND_FORK

#if defined(HAVE_DECL_ENVIRON) && HAVE_DECL_ENVIRON

/* pointer to global environ variable of external LIBC */
static char*** execwrap_libc_environ = NULL;

#endif /* HAVE_DECL_ENVIRON */

/* function to convert variable arguments of the execl* functions to an array
   for calling the corresponding execv* function */
static void execwrap_execl_valist_to_argv(va_list ap, const char* last,
  const char* argv[EXECWRAP_EXECL_MAX_ARGS])
{
  const char* tmp;
  uint32_t i = 0;

  argv[i++] = last;
  while((tmp = va_arg(ap, const char*)))
  {
    vt_libassert(i < EXECWRAP_EXECL_MAX_ARGS-1);
    argv[i++] = tmp;
  }
  argv[i] = NULL;
}

#endif /* EXECWRAP_EXEC_AND_FORK */

/* exec wrapper initialization/finalization functions called by
   vt_open/vt_close*/

void vt_execwrap_init()
{
#if defined(EXECWRAP_EXEC_AND_FORK) && \
    defined(HAVE_DECL_ENVIRON) && HAVE_DECL_ENVIRON
  /* get pointer to global environ variable of external LIBC */
  void* libc_handle = vt_libwrap_get_libc_handle();
  vt_libassert(libc_handle);
  execwrap_libc_environ = (char***)dlsym(libc_handle, "environ");
  vt_libassert(execwrap_libc_environ);
#endif /* EXECWRAP_EXEC_AND_FORK && HAVE_DECL_ENVIRON */
}

void vt_execwrap_finalize()
{
  /* delete library wrapper object, if necessary */
  if( execwrap_lw != VT_LIBWRAP_NULL )
    VTLibwrap_delete(execwrap_lw);
}


/* wrapper functions */

#ifdef EXECWRAP_EXEC_AND_FORK

/* -- unistd.h:execl -- */
int execl(const char* path, const char* arg, ...)
{
  int ret;

  uint32_t tid = VT_MASTER_THREAD;

  va_list ap;
  const char* argv[EXECWRAP_EXECL_MAX_ARGS];

  /* initialize this wrapper function */
  EXECWRAP_FUNC_INIT("execl", int, (const char*, const char*, ...));

  /* suspend LIBC memory (de)allocation tracing */
  VT_SUSPEND_MALLOC_TRACING(tid);

  /* record function enter event, if tracing of LIBC exec functions
     is enabled */
  if( EXECWRAP_DO_TRACE() )
  {
    uint64_t time;

    /* get calling thread id */
    /*tid = VT_MY_THREAD;*/

    /* get timestamp for the following function enter event */
    time = vt_pform_wtime();

    /* once, get unique function identifier */
    EXECWRAP_GET_FUNC_ID();

    /* record function enter event */
    (void)vt_enter(tid, &time, VT_LIBWRAP_FUNC_ID);
  }

  /* convert va_list to an array for the following execv call */
  va_start(ap, arg);
  execwrap_execl_valist_to_argv(ap, arg, argv);
  va_end(ap);

  /* resume LIBC exec tracing to prevent recording the following execv call */
  VT_SUSPEND_EXEC_TRACING(tid);

  /* call the execv wrapper function */
  ret = execv(path, (char* const*)argv);

  /* this point cannot be reached */
  return ret;
}

/* -- unistd.h:execlp -- */
int execlp(const char* file, const char* arg, ...)
{
  int ret;

  uint32_t tid = VT_MASTER_THREAD;

  va_list ap;
  const char* argv[EXECWRAP_EXECL_MAX_ARGS];

  /* initialize this wrapper function */
  EXECWRAP_FUNC_INIT("execlp", int, (const char*, const char*, ...));

  /* suspend LIBC memory (de)allocation tracing */
  VT_SUSPEND_MALLOC_TRACING(tid);

  /* record function enter event, if tracing of LIBC exec functions
     is enabled */
  if( EXECWRAP_DO_TRACE() )
  {
    uint64_t time;

    /* get calling thread id */
    /*tid = VT_MY_THREAD;*/

    /* get timestamp for the following function enter event */
    time = vt_pform_wtime();

    /* once, get unique function identifier */
    EXECWRAP_GET_FUNC_ID();

    /* record function enter event */
    (void)vt_enter(tid, &time, VT_LIBWRAP_FUNC_ID);
  }

  /* convert va_list to an array for the following execvp call */
  va_start(ap, arg);
  execwrap_execl_valist_to_argv(ap, arg, argv);
  va_end(ap);

  /* suspend LIBC exec tracing to prevent recording the following execvp call */
  VT_SUSPEND_EXEC_TRACING(tid);

  /* call the execvp wrapper function */
  ret = execvp(file, (char* const*)argv);

  /* this point cannot be reached */
  return ret;
}

/* -- unistd.h:execle -- */
int execle(const char* path, const char* arg, ...)
{
  int ret;

  uint32_t tid = VT_MASTER_THREAD;

  va_list ap;
  const char* argv[EXECWRAP_EXECL_MAX_ARGS];
  char* const* envp;

  /* initialize this wrapper function */
  EXECWRAP_FUNC_INIT("execle", int, (const char*, const char*, ...));

  /* suspend LIBC memory (de)allocation tracing */
  VT_SUSPEND_MALLOC_TRACING(tid);

  /* record function enter event, if tracing of LIBC exec functions
     is enabled */
  if( EXECWRAP_DO_TRACE() )
  {
    uint64_t time;

    /* get calling thread id */
    /*tid = VT_MY_THREAD;*/

    /* get timestamp for the following function enter event */
    time = vt_pform_wtime();

    /* once, get unique function identifier */
    EXECWRAP_GET_FUNC_ID();

    /* record function enter event */
    (void)vt_enter(tid, &time, VT_LIBWRAP_FUNC_ID);
  }

  /* convert va_list to an array for the following execve call */
  va_start(ap, arg);
  execwrap_execl_valist_to_argv(ap, arg, argv);
  envp = va_arg(ap, char* const*);
  va_end(ap);

  /* suspend LIBC exec tracing to prevent recording the following execve call */
  VT_SUSPEND_EXEC_TRACING(tid);

  /* call the execve wrapper function */
  ret = execve(path, (char* const*)argv, envp);

  /* this point cannot be reached */
  return ret;
}

/* -- unistd.h:execv -- */
int execv(const char* path, char* const argv[])
{
  int ret;

  uint32_t tid = VT_MASTER_THREAD;

  /* initialize this wrapper function */
  EXECWRAP_FUNC_INIT("execv", int, (const char*, char* const[]));

  /* suspend LIBC memory (de)allocation tracing */
  VT_SUSPEND_MALLOC_TRACING(tid);

  /* record function enter event, if tracing of LIBC exec functions
     is enabled */
  if( EXECWRAP_DO_TRACE() )
  {
    uint64_t time;

    /* get calling thread id */
    /*tid = VT_MY_THREAD;*/

    /* get timestamp for the following function enter event */
    time = vt_pform_wtime();

    /* once, get unique function identifier */
    EXECWRAP_GET_FUNC_ID();

    /* record function enter event */
    (void)vt_enter(tid, &time, VT_LIBWRAP_FUNC_ID);
  }

  /* close VT for this process, in hope that the following execv call
     succeeds */
  vt_close();

#if defined(HAVE_DECL_ENVIRON) && HAVE_DECL_ENVIRON

  /* set environ of external LIBC */
  *execwrap_libc_environ = environ;

#endif /* HAVE_DECL_ENVIRON */

  /* call the actual library function */
  ret = EXECWRAP_FUNC_CALL((path, argv));

  /* an error occurred if reaching this point; cannot continue, because VT is
     already closed */

  /* get errno from external LIBC and abort */
  errno = vt_libwrap_get_libc_errno();
  vt_error_msg("%s failed: %s", VT_LIBWRAP_FUNC_NAME, strerror(errno));

  return ret;
}

/* -- unistd.h:execvp -- */
int execvp(const char* file, char* const argv[])
{
  int ret;

  uint32_t tid = VT_MASTER_THREAD;

  /* initialize this wrapper function */
  EXECWRAP_FUNC_INIT("execvp", int, (const char*, char* const[]));

  /* suspend LIBC memory (de)allocation tracing */
  VT_SUSPEND_MALLOC_TRACING(tid);

  /* record function enter event, if tracing of LIBC exec functions
     is enabled */
  if( EXECWRAP_DO_TRACE() )
  {
    uint64_t time;

    /* get calling thread id */
    /*tid = VT_MY_THREAD;*/

    /* get timestamp for the following function enter event */
    time = vt_pform_wtime();

    /* once, get unique function identifier */
    EXECWRAP_GET_FUNC_ID();

    /* record function enter event */
    (void)vt_enter(tid, &time, VT_LIBWRAP_FUNC_ID);
  }

  /* close VT for this process, in hope that the following execvp call
     succeeds */
  vt_close();

#if defined(HAVE_DECL_ENVIRON) && HAVE_DECL_ENVIRON

  /* set environ of external LIBC */
  *execwrap_libc_environ = environ;

#endif /* HAVE_DECL_ENVIRON */

  /* call the actual library function */
  ret = EXECWRAP_FUNC_CALL((file, argv));

  /* an error occurred if reaching this point; cannot continue, because VT is
     already closed */

  /* get errno from external LIBC and abort */
  errno = vt_libwrap_get_libc_errno();
  vt_error_msg("%s failed: %s", VT_LIBWRAP_FUNC_NAME, strerror(errno));

  return ret;
}

/* -- unistd.h:execve -- */
int execve(const char* path, char* const argv[], char* const envp[])
{
  int ret;

  uint32_t tid = VT_MASTER_THREAD;

  /* initialize this wrapper function */
  EXECWRAP_FUNC_INIT("execve", int,
    (const char*, char* const[], char* const[]));

  /* suspend LIBC memory (de)allocation tracing */
  VT_SUSPEND_MALLOC_TRACING(tid);

  /* record function enter event, if tracing of LIBC exec functions
     is enabled */
  if( EXECWRAP_DO_TRACE() )
  {
    uint64_t time;

    /* get calling thread id */
    /*tid = VT_MY_THREAD;*/

    /* get timestamp for the following function enter event */
    time = vt_pform_wtime();

    /* once, get unique function identifier */
    EXECWRAP_GET_FUNC_ID();

    /* record function enter event */
    (void)vt_enter(tid, &time, VT_LIBWRAP_FUNC_ID);
  }

  /* close VT for this process, in hope that the following execve call
     succeeds */
  vt_close();

  /* call the actual library function */
  ret = EXECWRAP_FUNC_CALL((path, argv, envp));

  /* an error occurred if reaching this point; cannot continue, because VT is
     already closed */

  /* get errno from external LIBC and abort */
  errno = vt_libwrap_get_libc_errno();
  vt_error_msg("%s failed: %s", VT_LIBWRAP_FUNC_NAME, strerror(errno));

  return ret;
}

#if defined(HAVE_EXECVPE) && HAVE_EXECVPE

/* -- unistd.h:execvpe -- */
int execvpe(const char* file, char* const argv[], char* const envp[])
{
  int ret;

  uint32_t tid = VT_MASTER_THREAD;

  /* initialize this wrapper function */
  EXECWRAP_FUNC_INIT("execvpe", int,
    (const char*, char* const[], char* const[]));

  /* suspend LIBC memory (de)allocation tracing */
  VT_SUSPEND_MALLOC_TRACING(tid);

  /* record function enter event, if tracing of LIBC exec functions
     is enabled */
  if( EXECWRAP_DO_TRACE() )
  {
    uint64_t time;

    /* get calling thread id */
    /*tid = VT_MY_THREAD;*/

    /* get timestamp for the following function enter event */
    time = vt_pform_wtime();

    /* once, get unique function identifier */
    EXECWRAP_GET_FUNC_ID();

    /* record function enter event */
    (void)vt_enter(tid, &time, VT_LIBWRAP_FUNC_ID);
  }

  /* close VT for this process, in hope that the following execvpe call
     succeeds */
  vt_close();

  /* call the actual library function */
  ret = EXECWRAP_FUNC_CALL((file, argv, envp));

  /* an error occurred if reaching this point; cannot continue, because VT is
     already closed */

  /* get errno from external LIBC and abort */
  errno = vt_libwrap_get_libc_errno();
  vt_error_msg("%s failed: %s", VT_LIBWRAP_FUNC_NAME, strerror(errno));

  return ret;
}

#endif /* HAVE_EXECVPE */

#if defined(HAVE_FEXECVE) && HAVE_FEXECVE

/* -- unistd.h:fexecve -- */
int fexecve(int fd, char* const argv[], char* const envp[])
{
  int ret;

  uint32_t tid = VT_MASTER_THREAD;

  /* initialize this wrapper function */
  EXECWRAP_FUNC_INIT("fexecve", int, (int, char* const[], char* const[]));

  /* suspend LIBC memory (de)allocation tracing */
  VT_SUSPEND_MALLOC_TRACING(tid);

  /* record function enter event, if tracing of LIBC exec functions
     is enabled */
  if( EXECWRAP_DO_TRACE() )
  {
    uint64_t time;

    /* get calling thread id */
    /*tid = VT_MY_THREAD;*/

    /* get timestamp for the following function enter event */
    time = vt_pform_wtime();

    /* once, get unique function identifier */
    EXECWRAP_GET_FUNC_ID();

    /* record function enter event */
    (void)vt_enter(tid, &time, VT_LIBWRAP_FUNC_ID);
  }

  /* close VT for this process, in hope that the following fexecve call
     succeeds */
  vt_close();

  /* call the actual library function */
  ret = EXECWRAP_FUNC_CALL((fd, argv, envp));

  /* an error occurred if reaching this point; cannot continue, because VT is
     already closed */

  /* get errno from external LIBC and abort */
  errno = vt_libwrap_get_libc_errno();
  vt_error_msg("%s failed: %s", VT_LIBWRAP_FUNC_NAME, strerror(errno));

  return ret;
}

#endif /* HAVE_FEXECVE */

/* -- unistd.h:fork -- */
pid_t fork()
{
  pid_t ret;

  uint32_t tid = VT_MASTER_THREAD;
  uint64_t time;

  /* initialize this wrapper function */
  EXECWRAP_FUNC_INIT("fork", pid_t, (void));

  /* suspend LIBC memory (de)allocation tracing */
  VT_SUSPEND_MALLOC_TRACING(tid);

  /* record function enter event, if tracing of LIBC exec functions
     is enabled */
  if( EXECWRAP_DO_TRACE() )
  {
    /* get calling thread id */
    /*tid = VT_MY_THREAD;*/

    /* get timestamp for the following function enter event */
    time = vt_pform_wtime();

    /* once, get unique function identifier */
    EXECWRAP_GET_FUNC_ID();

    /* record function enter event */
    (void)vt_enter(tid, &time, VT_LIBWRAP_FUNC_ID);
  }

  /* call the actual library function */
  ret = EXECWRAP_FUNC_CALL(());

  /* handle fork, if succeeded */
#ifdef VT_DYNINST
  /* ... and we're not attaching the Dyninst mutator */
  if( !vt_dyn_attaching )
#endif /* VT_DYNINST */
  {
    if( ret != -1 )
      vt_fork(ret);
  }

  if( EXECWRAP_DO_TRACE() && ret != 0 )
  {
    /* record function exit event */
    time = vt_pform_wtime();
    vt_exit(tid, &time);
  }

  /* get errno from external LIBC */
  errno = vt_libwrap_get_libc_errno();

  /* resume LIBC memory (de)allocation tracing */
  VT_RESUME_MALLOC_TRACING(tid);

  return ret;
}

#endif /* EXECWRAP_EXEC_AND_FORK */

/* -- stdlib.h:system -- */
int system(const char* command)
{
  int ret;

  uint32_t tid = VT_CURRENT_THREAD;

  /* initialize this wrapper function */
  EXECWRAP_FUNC_INIT("system", int, (const char*));

  /* suspend LIBC memory (de)allocation tracing */
  VT_SUSPEND_MALLOC_TRACING(tid);

  if( EXECWRAP_DO_TRACE() )
  {
    uint64_t time;

    /* get calling thread id */
    tid = VT_MY_THREAD;

    /* get timestamp for the following function enter event */
    time = vt_pform_wtime();

    /* once, get unique function identifier */
    EXECWRAP_GET_FUNC_ID();

    /* record function enter event */
    (void)vt_enter(tid, &time, VT_LIBWRAP_FUNC_ID);

    /* call the actual library function */
    ret = EXECWRAP_FUNC_CALL((command));

    /* record function exit event */
    time = vt_pform_wtime();
    vt_exit(tid, &time);
  }
  else
  {
    /* call the actual library function */
    ret = EXECWRAP_FUNC_CALL((command));
  }

  /* get errno from external LIBC */
  errno = vt_libwrap_get_libc_errno();

  /* resume LIBC memory (de)allocation tracing */
  VT_RESUME_MALLOC_TRACING(tid);

  return ret;
}

/* -- sys/wait.h:wait -- */

pid_t wait(__WAIT_STATUS status)
{
  pid_t ret;

  uint32_t tid = VT_CURRENT_THREAD;

  /* initialize this wrapper function */
  EXECWRAP_FUNC_INIT("wait", pid_t, (__WAIT_STATUS));

  /* suspend LIBC memory (de)allocation tracing */
  VT_SUSPEND_MALLOC_TRACING(tid);

  if( EXECWRAP_DO_TRACE() )
  {
    uint64_t time;

    /* get calling thread id */
    tid = VT_MY_THREAD;

    /* get timestamp for the following function enter event */
    time = vt_pform_wtime();

    /* once, get unique function identifier */
    EXECWRAP_GET_FUNC_ID();

    /* record function enter event */
    (void)vt_enter(tid, &time, VT_LIBWRAP_FUNC_ID);

    /* call the actual library function */
    ret = EXECWRAP_FUNC_CALL((status));

    /* record function exit event */
    time = vt_pform_wtime();
    vt_exit(tid, &time);
  }
  else
  {
    /* call the actual library function */
    ret = EXECWRAP_FUNC_CALL((status));
  }

  /* get errno from external LIBC */
  errno = vt_libwrap_get_libc_errno();

  /* resume LIBC memory (de)allocation tracing */
  VT_RESUME_MALLOC_TRACING(tid);

  return ret;
}

/* -- sys/wait.h:waitpid -- */
pid_t waitpid(pid_t pid, int* status, int options)
{
  pid_t ret;

  uint32_t tid = VT_CURRENT_THREAD;

  /* initialize this wrapper function */
  EXECWRAP_FUNC_INIT("waitpid", pid_t, (pid_t, int*, int));

  /* suspend LIBC memory (de)allocation tracing */
  VT_SUSPEND_MALLOC_TRACING(tid);

  if( EXECWRAP_DO_TRACE() )
  {
    uint64_t time;

    /* get calling thread id */
    tid = VT_MY_THREAD;

    /* get timestamp for the following function enter event */
    time = vt_pform_wtime();

    /* once, get unique function identifier */
    EXECWRAP_GET_FUNC_ID();

    /* record function enter event */
    (void)vt_enter(tid, &time, VT_LIBWRAP_FUNC_ID);

    /* call the actual library function */
    ret = EXECWRAP_FUNC_CALL((pid, status, options));

    /* record function exit event */
    time = vt_pform_wtime();
    vt_exit(tid, &time);
  }
  else
  {
    /* call the actual library function */
    ret = EXECWRAP_FUNC_CALL((pid, status, options));
  }

  /* get errno from external LIBC */
  errno = vt_libwrap_get_libc_errno();

  /* resume LIBC memory (de)allocation tracing */
  VT_RESUME_MALLOC_TRACING(tid);

  return ret;
}

#if defined(HAVE_WAITID) && HAVE_WAITID

/* -- sys/wait.h:waitid -- */
int waitid(idtype_t idtype, id_t id, siginfo_t* infop, int options)
{
  int ret;

  uint32_t tid = VT_CURRENT_THREAD;

  /* initialize this wrapper function */
  EXECWRAP_FUNC_INIT("waitid", int, (idtype_t, id_t, siginfo_t*, int));

  /* suspend LIBC memory (de)allocation tracing */
  VT_SUSPEND_MALLOC_TRACING(tid);

  if( EXECWRAP_DO_TRACE() )
  {
    uint64_t time;

    /* get calling thread id */
    tid = VT_MY_THREAD;

    /* get timestamp for the following function enter event */
    time = vt_pform_wtime();

    /* once, get unique function identifier */
    EXECWRAP_GET_FUNC_ID();

    /* record function enter event */
    (void)vt_enter(tid, &time, VT_LIBWRAP_FUNC_ID);

    /* call the actual library function */
    ret = EXECWRAP_FUNC_CALL((idtype, id, infop, options));

    /* record function exit event */
    time = vt_pform_wtime();
    vt_exit(tid, &time);
  }
  else
  {
    /* call the actual library function */
    ret = EXECWRAP_FUNC_CALL((idtype, id, infop, options));
  }

  /* get errno from external LIBC */
  errno = vt_libwrap_get_libc_errno();

  /* resume LIBC memory (de)allocation tracing */
  VT_RESUME_MALLOC_TRACING(tid);

  return ret;
}

#endif /* HAVE_WAITID */

#if defined(HAVE_WAIT3) && HAVE_WAIT3

/* -- sys/wait.h:wait3 -- */
pid_t wait3(__WAIT_STATUS status, int options, struct rusage* rusage)
{
  pid_t ret;

  uint32_t tid = VT_CURRENT_THREAD;

  /* initialize this wrapper function */
  EXECWRAP_FUNC_INIT("wait3", pid_t, (__WAIT_STATUS, int, struct rusage*));

  /* suspend LIBC memory (de)allocation tracing */
  VT_SUSPEND_MALLOC_TRACING(tid);

  if( EXECWRAP_DO_TRACE() )
  {
    uint64_t time;

    /* get calling thread id */
    tid = VT_MY_THREAD;

    /* get timestamp for the following function enter event */
    time = vt_pform_wtime();

    /* once, get unique function identifier */
    EXECWRAP_GET_FUNC_ID();

    /* record function enter event */
    (void)vt_enter(tid, &time, VT_LIBWRAP_FUNC_ID);

    /* call the actual library function */
    ret = EXECWRAP_FUNC_CALL((status, options, rusage));

    /* record function exit event */
    time = vt_pform_wtime();
    vt_exit(tid, &time);
  }
  else
  {
    /* call the actual library function */
    ret = EXECWRAP_FUNC_CALL((status, options, rusage));
  }

  /* get errno from external LIBC */
  errno = vt_libwrap_get_libc_errno();

  /* resume LIBC memory (de)allocation tracing */
  VT_RESUME_MALLOC_TRACING(tid);

  return ret;
}

#endif /* HAVE_WAIT3 */

#if defined(HAVE_WAIT4) && HAVE_WAIT4

/* -- sys/wait.h:wait4 -- */
pid_t wait4(pid_t pid, __WAIT_STATUS status, int options, struct rusage* rusage)
{
  pid_t ret;

  uint32_t tid = VT_CURRENT_THREAD;

  /* initialize this wrapper function */
  EXECWRAP_FUNC_INIT("wait4", pid_t,
    (pid_t, __WAIT_STATUS, int, struct rusage*));

  /* suspend LIBC memory (de)allocation tracing */
  VT_SUSPEND_MALLOC_TRACING(tid);

  if( EXECWRAP_DO_TRACE() )
  {
    uint64_t time;

    /* get calling thread id */
    tid = VT_MY_THREAD;

    /* get timestamp for the following function enter event */
    time = vt_pform_wtime();

    /* once, get unique function identifier */
    EXECWRAP_GET_FUNC_ID();

    /* record function enter event */
    (void)vt_enter(tid, &time, VT_LIBWRAP_FUNC_ID);

    /* call the actual library function */
    ret = EXECWRAP_FUNC_CALL((pid, status, options, rusage));

    /* record function exit event */
    time = vt_pform_wtime();
    vt_exit(tid, &time);
  }
  else
  {
    /* call the actual library function */
    ret = EXECWRAP_FUNC_CALL((pid, status, options, rusage));
  }

  /* get errno from external LIBC */
  errno = vt_libwrap_get_libc_errno();

  /* resume LIBC memory (de)allocation tracing */
  VT_RESUME_MALLOC_TRACING(tid);

  return ret;
}

#endif /* HAVE_WAIT4 */
