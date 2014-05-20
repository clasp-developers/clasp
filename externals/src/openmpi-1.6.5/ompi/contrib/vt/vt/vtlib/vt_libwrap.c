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

#define _GNU_SOURCE

#include "config.h"

#include "vt_defs.h"
#include "vt_error.h"
#include "vt_inttypes.h"
#include "vt_libwrap.h"
#include "vt_mallocwrap.h"
#include "vt_pform.h"
#include "vt_thrd.h"
#include "vt_trc.h"

#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* maximum number of library wrapper objects */
#define MAX_LWS 16

/* maximum number of handles for shared libraries
   =VT_LIBWRAP_MAX_SHLIBS +1(LIBC's handle) [+1(RTLD_NEXT)] */
#if defined(HAVE_DECL_RTLD_NEXT) && HAVE_DECL_RTLD_NEXT
# define MAX_HANDLES (VT_LIBWRAP_MAX_SHLIBS+1+1)
#else /* HAVE_DECL_RTLD_NEXT */
# define MAX_HANDLES (VT_LIBWRAP_MAX_SHLIBS+1)
#endif /* HAVE_DECL_RTLD_NEXT */

/* Do not call dlerror, if the memory allocation wrappers are enabled.
   dlerror calls realloc which would ends up in an infinite recursion. */
#ifdef VT_MALLOCWRAP
# define dlerror no_dlerror
  static char* no_dlerror(void)
  {
    return "unknown";
  }
#endif /* VT_MALLOCWRAP */

/* data structure for library wrapper object */
struct VTLibwrap_struct
{
  VTLibwrapAttr* attr;                 /* attributes */
  void*          handlev[MAX_HANDLES]; /* vector of handles */
  uint32_t       handlen;              /* number of handles */
};

static VTLibwrap lwv[MAX_LWS]; /* vector of library wrapper objects */
static uint32_t  lwn = 0;      /* number of library wrapper objects */

/* default library wrapper attributes */
static VTLibwrapAttr default_attr = VT_LIBWRAP_ATTR_DEFAULT;

/* mutexes for locking ... */
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
/* ... creation of library wrapper objects */
static VTThrdMutex* lw_create_mutex = NULL;
/* ... getting LIBC handle */
static VTThrdMutex* lw_libc_mutex = NULL;
#endif /* VT_MT || VT_HYB || VT_JAVA */

/* get pointer to errno of external LIBC */
static int* get_libc_errno_ptr(void)
{
  /* NOTE: errno might be a macro which calls a function
     (e.g. __errno_location() (GNU), _Errno() (AIX))
     to get a per-thread value of errno */
#if defined(HAVE_DECL___ERRNO_LOCATION) && HAVE_DECL___ERRNO_LOCATION
  static int* (*libc_errno)(void) = NULL;
  const char* libc_errno_sym = "__errno_location";
#elif defined(HAVE_DECL__ERRNO) && HAVE_DECL__ERRNO
  static int* (*libc_errno)(void) = NULL;
  const char* libc_errno_sym = "_Errno";
#else /* HAVE_DECL___ERRNO_LOCATION || HAVE_DECL__ERRNO */
  static int* libc_errno = NULL;
  const char* libc_errno_sym = "errno";
#endif /* HAVE_DECL___ERRNO_LOCATION || HAVE_DECL__ERRNO */

  static void* libc_handle = NULL;

  /* get LIBC handle, if necessary */
  if( libc_handle == NULL )
    libc_handle = vt_libwrap_get_libc_handle();

  if( libc_errno == NULL )
  {
    (void)dlerror();
#if (defined(HAVE_DECL___ERRNO_LOCATION) && HAVE_DECL___ERRNO_LOCATION) || \
    (defined(HAVE_DECL__ERRNO) && HAVE_DECL__ERRNO)
    *(void**)(&libc_errno) = dlsym(libc_handle, libc_errno_sym);
#else /* HAVE_DECL___ERRNO_LOCATION || HAVE_DECL__ERRNO */
    libc_errno = (int*)dlsym(libc_handle, libc_errno_sym);
#endif /* HAVE_DECL___ERRNO_LOCATION || HAVE_DECL__ERRNO */
    if( libc_errno == NULL )
    {
#ifdef VT_IOWRAP
      /* do not use vt_error_msg() here to prevent possible recursive calls to
         this function */
      printf("VampirTrace: FATAL: dlsym(\"%s\") failed: %s\n",
             libc_errno_sym, dlerror());
      exit(EXIT_FAILURE);
#else /* VT_IOWRAP */
      vt_error_msg("dlsym(\"%s\") failed: %s\n", libc_errno_sym, dlerror());
#endif /* VT_IOWRAP */
    }
  }

#if (defined(HAVE_DECL___ERRNO_LOCATION) && HAVE_DECL___ERRNO_LOCATION) || \
    (defined(HAVE_DECL__ERRNO) && HAVE_DECL__ERRNO)
  return libc_errno();
#else /* HAVE_DECL___ERRNO_LOCATION || HAVE_DECL__ERRNO */
  return libc_errno;
#endif /* HAVE_DECL___ERRNO_LOCATION || HAVE_DECL__ERRNO */
}

void vt_libwrap_init()
{
}

void vt_libwrap_finalize()
{
  /* destroy mutexes, if necessary */
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
  if( lw_create_mutex != NULL )
    VTThrd_deleteMutex(&lw_create_mutex);
  if( lw_libc_mutex != NULL )
    VTThrd_deleteMutex(&lw_libc_mutex);
#endif /* VT_MT || VT_HYB || VT_JAVA */
}

void* vt_libwrap_get_libc_handle()
{
  static void* libc_handle = NULL;

#ifndef SHLIBC_PATHNAME
  vt_error_msg("VampirTrace is not properly configured, SHLIBC_PATHNAME is not "
               "set! Please report this incident to "PACKAGE_BUGREPORT);
#else /* SHLIBC_PATHNAME */
  if( libc_handle == NULL )
  {
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
    VTThrd_lock(&lw_libc_mutex);
    if( libc_handle == NULL )
    {
#endif /* VT_MT || VT_HYB || VT_JAVA */

    (void)dlerror();
    libc_handle = dlopen(SHLIBC_PATHNAME,
                         RTLD_LAZY | RTLD_LOCAL
#ifdef _AIX
                         | RTLD_MEMBER
#endif /* _AIX */
                        );
    if( libc_handle == NULL )
    {
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
      VTThrd_unlock(&lw_libc_mutex);
#endif /* VT_MT || VT_HYB || VT_JAVA */
#ifdef VT_IOWRAP
      /* do not use vt_error_msg() here to prevent possible recursive calls to
         this function */
      printf("VampirTrace: FATAL: dlopen(\""SHLIBC_PATHNAME"\") failed: %s\n",
             dlerror());
      exit(EXIT_FAILURE);
#else /* VT_IOWRAP */
      vt_error_msg("dlopen(\""SHLIBC_PATHNAME"\") failed: %s\n", dlerror());
#endif /* VT_IOWRAP */
    }

#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
    }
    VTThrd_unlock(&lw_libc_mutex);
#endif /* VT_MT || VT_HYB || VT_JAVA */
  }
#endif /* SHLIBC_PATHNAME */

  return libc_handle;
}

void vt_libwrap_set_libc_errno(const int value)
{
  *get_libc_errno_ptr() = value;
}

int vt_libwrap_get_libc_errno()
{
  return *get_libc_errno_ptr();
}

void VTLibwrap_create(VTLibwrap** lw, VTLibwrapAttr* lwattr)
{
  uint8_t error = 0;
  char error_msg[1024] = "";

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
  VTThrd_lock(&lw_create_mutex);
#endif /* VT_MT || VT_HYB || VT_JAVA */

  do
  {
    /* library wrapper object already exists ? */
    if( *lw != VT_LIBWRAP_NULL )
    {
      error = 1;
      break;
    }

    /* maximum number of library wrapper objects reached ? */
    if( lwn + 1 >= MAX_LWS )
    {
      error = 1;
      snprintf(error_msg, sizeof(error_msg) - 1,
               "Cannot create more than %d library wrapper objects", MAX_LWS);
      break;
    }

    /* get next unused library wrapper object from vector */
    *lw = &(lwv[lwn++]);

    /* if not attributes given, use the default attributes */
    (*lw)->attr = lwattr ? lwattr : &default_attr;

    /* call attributes initializer function, if necessary */
    if( (*lw)->attr->init_func )
      (*lw)->attr->init_func((*lw)->attr);

    (*lw)->handlen = 0;

    /* shared libraries specified ? */
    if( (*lw)->attr->shlibs_num > 0 )
    {
      int i;

      /* number of specified shared libraries to high ? */
      if( (*lw)->attr->shlibs_num > VT_LIBWRAP_MAX_SHLIBS )
      {
        error = 1;
        snprintf(error_msg, sizeof(error_msg) - 1,
                 "Number of shared libraries for searching actual library "
                 "functions exceeds VampirTrace maximum of %d",
                 VT_LIBWRAP_MAX_SHLIBS);
        break;
      }

      /* get handles for specified shared libraries */
      for( i = 0; i < (*lw)->attr->shlibs_num; i++ )
      {
        (void)dlerror();
        (*lw)->handlev[i] = dlopen((*lw)->attr->shlibs[i],
                                   RTLD_LAZY | RTLD_LOCAL
#ifdef _AIX
                                   | RTLD_MEMBER
#endif /* _AIX */
                                  );
        if( (*lw)->handlev[i] == NULL )
        {
          error = 1;
          snprintf(error_msg, sizeof(error_msg) - 1,
                   "dlopen(\"%s\") failed: %s",
                   (*lw)->attr->shlibs[i], dlerror());
          break;
        }
        (*lw)->handlen++;
      }
      if( error ) break;
    }

    /* append LIBC's handle to the vector of handles, if desired */
    if( (*lw)->attr->libc )
      (*lw)->handlev[(*lw)->handlen++] = vt_libwrap_get_libc_handle();

    /* append 'RTLD_NEXT' to the vector of handles, if possible */
#if defined(HAVE_DECL_RTLD_NEXT) && HAVE_DECL_RTLD_NEXT
    (*lw)->handlev[(*lw)->handlen++] = RTLD_NEXT;
#endif /* HAVE_DECL_RTLD_NEXT */

    if( (*lw)->handlen == 0 )
    {
      error = 1;
      snprintf(error_msg, sizeof(error_msg) - 1,
               "No shared library for searching actual library functions "
               "specified");
      break;
    }
  } while(0);

#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
  VTThrd_unlock(&lw_create_mutex);
#endif /* VT_MT || VT_HYB || VT_JAVA */

  /* error occurred ? */
  if( error )
  {
    /* abort VampirTrace, if necessary */
    if( error_msg[0] ) vt_error_msg(error_msg);
  }
  else
  {
    /* initialize VampirTrace, if necessary */
    if( !(*lw)->attr->wait_for_init && !vt_is_alive )
      vt_open();
    else
      VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
  }
}

void VTLibwrap_delete(VTLibwrap* lw)
{
  uint32_t i;

  vt_libassert(lw);

  /* close all opened handles */
  for( i = 0; i < lw->handlen; i++ )
  {
#if defined(HAVE_DECL_RTLD_NEXT) && HAVE_DECL_RTLD_NEXT
    if( lw->handlev[i] != RTLD_NEXT )
    {
#endif /* HAVE_DECL_RTLD_NEXT */
    (void)dlerror();
    if( dlclose(lw->handlev[i]) != 0 )
      vt_error_msg("dlclose(\"%s\") failed: %s",
                   lw->attr->shlibs[i], dlerror());
#if defined(HAVE_DECL_RTLD_NEXT) && HAVE_DECL_RTLD_NEXT
    }
#endif /* HAVE_DECL_RTLD_NEXT */
  }
}

void VTLibwrap_delete_all()
{
  uint32_t i;

  /* delete all library wrapper objects */
  for( i = 0; i < lwn; i++ )
    VTLibwrap_delete(&(lwv[i]));
}

void VTLibwrap_func_init(const VTLibwrap* lw, const char* func,
                         const char* file, int line,
                         void** funcptr, int* funcid)
{
  uint32_t i;

  vt_libassert(lw);

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

  if( funcptr && !(*funcptr) )
  {
    /* array for dlsym error messages */
    char dlsym_errors[MAX_HANDLES][256];

    /* search all handles for function */
    for( i = 0; i < lw->handlen && !(*funcptr); i++ )
    {
      /* get pointer to actual library function */
      (void)dlerror();
      *funcptr = dlsym(lw->handlev[i], func);

      /* function not found ? */
      if( !(*funcptr) )
      {
        char* dlsym_error_msg = dlerror();

        /* store dlsym error message, if available */
        if( dlsym_error_msg )
        {
          strncpy(dlsym_errors[i], dlsym_error_msg, sizeof(dlsym_errors[i])-1);
        }
#if defined(HAVE_DECL_RTLD_NEXT) && HAVE_DECL_RTLD_NEXT
        /* usually the dlsym error message for RTLD_NEXT may empty */
        else if( i == lw->handlen - 1 )
        {
          snprintf(dlsym_errors[i], sizeof(dlsym_errors[i])-1,
                   "RTLD_NEXT: symbol not found: %s", func);
        }
#endif /* HAVE_DECL_RTLD_NEXT */
        else
        {
          strncpy(dlsym_errors[i], "unknown error", sizeof(dlsym_errors[i])-1);
        }
      }
    }

    /* merge all dlsym error messages to one message, if
       function not found */
    if( !(*funcptr) )
    {
      char* dlsym_errors_merged;

      dlsym_errors_merged =
        (char*)calloc(lw->handlen * sizeof(dlsym_errors[0]), sizeof(char));
      if( dlsym_errors_merged == NULL )
        vt_error();

      for( i = 0; i < lw->handlen; i++ )
      {
        if( i > 0 )
          strncat(dlsym_errors_merged, "\n", 255 - strlen(dlsym_errors_merged));
        strncat(dlsym_errors_merged, dlsym_errors[i],
                255 - strlen(dlsym_errors_merged));
      }
      vt_error_msg("dlsym(\"%s\") failed:\n%s", func, dlsym_errors_merged);
    }
  }

  /* get function identifier, if necessary */
  if( funcid && *funcid == VT_LIBWRAP_NOID && vt_is_alive )
  {
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
    VTTHRD_LOCK_IDS();
    if( *funcid == VT_LIBWRAP_NOID )
    {
#endif /* VT_MT || VT_HYB || VT_JAVA */
    uint32_t fid = VT_NO_ID;
    uint32_t lno = VT_NO_LNO;

    /* register source file, if available */
    if( file != NULL && line > 0 )
    {
      fid = vt_def_scl_file(VT_CURRENT_THREAD, file);
      lno = line;
    }
    /* register function */
    *funcid = vt_def_region(VT_CURRENT_THREAD, func, fid, lno, VT_NO_LNO,
                            lw->attr->func_group, VT_FUNCTION);
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
    }
    VTTHRD_UNLOCK_IDS();
#endif /* VT_MT || VT_HYB || VT_JAVA */
  }

  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
}

void VTLibwrap_func_start(const VTLibwrap* lw, const int funcid)
{
  uint64_t time;

  vt_libassert(lw);

  if( !vt_is_alive ) return;

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

  vt_libassert(funcid != VT_LIBWRAP_NOID);

  time = vt_pform_wtime();

  (void)vt_enter(VT_CURRENT_THREAD, &time, funcid);

  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
}

void VTLibwrap_func_end(const VTLibwrap* lw, const int funcid)
{
  uint64_t time;

  vt_libassert(lw);

  if( !vt_is_alive ) return;

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);

  vt_libassert(funcid != VT_LIBWRAP_NOID);

  time = vt_pform_wtime();

  vt_exit(VT_CURRENT_THREAD, &time);

  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
}
