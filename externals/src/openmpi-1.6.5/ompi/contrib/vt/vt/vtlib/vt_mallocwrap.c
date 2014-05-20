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

#define _BSD_SOURCE /* possibly needed for valloc */
#define _XOPEN_SOURCE 600 /* possibly needed for posix_memalign */

#include "config.h"

#include "vt_defs.h"
#include "vt_env.h"
#include "vt_inttypes.h"
#include "vt_libwrap.h"
#include "vt_mallocwrap.h"
#include "vt_pform.h"
#include "vt_thrd.h"
#include "vt_trc.h"

#include <errno.h>
#include <stdlib.h>
#if (defined(HAVE_MEMALIGN) && HAVE_MEMALIGN) || \
    (defined(HAVE_VALLOC) &&  HAVE_VALLOC)
# include <malloc.h>
#endif /* HAVE_MEMALIGN || HAVE_VALLOC */


/* define the following macro to enable tracing the calloc function which
   needs a quite dirty hack to make it work (see comments in the calloc
   wrapper function below) */
#define MALLOCWRAP_CALLOC


/* special version of VT_LIBWRAP_FUNC_INIT w/o getting the actual function
   pointer (VT_LIBWRAP_FUNC_PTR) and the unique function identifier
   (VT_LIBWRAP_FUNC_ID) */
#define MALLOCWRAP_FUNC_INIT(_func, _rettype, _argtypes)                      \
  _VT_LIBWRAP_FUNC_INIT_DECL_VARS(_func, _rettype, _argtypes);                \
  if( mallocwrap_lw == VT_LIBWRAP_NULL ) {                                    \
    VTLibwrap_create(&mallocwrap_lw, &mallocwrap_lw_attr);                    \
  }

/* get pointer to actual library function
   (sets VT_LIBWRAP_FUNC_PTR, must be called after MALLOCWRAP_FUNC_INIT!) */
#define MALLOCWRAP_GET_FUNC_PTR()                                             \
  if( VT_LIBWRAP_FUNC_PTR == VT_LIBWRAP_NULL ) {                              \
    VTLibwrap_func_init(mallocwrap_lw, VT_LIBWRAP_FUNC_NAME, NULL, 0,         \
      (void**)(&VT_LIBWRAP_FUNC_PTR), NULL);                                  \
  }

/* get unique function identifier
   (sets VT_LIBWRAP_FUNC_ID, must be called after MALLOCWRAP_FUNC_INIT!) */
#define MALLOCWRAP_GET_FUNC_ID()                                              \
  if( VT_LIBWRAP_FUNC_ID == VT_LIBWRAP_NOID ) {                               \
    VTLibwrap_func_init(mallocwrap_lw, VT_LIBWRAP_FUNC_NAME, NULL, 0,         \
      NULL, &VT_LIBWRAP_FUNC_ID);                                             \
  }

/* simplified version of VT_LIBWRAP_FUNC_CALL w/o argument for the library
   wrapper object */
#define MALLOCWRAP_FUNC_CALL(_args) VT_LIBWRAP_FUNC_CALL(mallocwrap_lw, _args)

/* check whether tracing of LIBC memory (de)allocation functions is
   currently enabled */
#define MALLOCWRAP_DO_TRACE()                                                 \
  ( vt_is_alive && VT_MY_THREAD_IS_ALIVE &&                                   \
    VTTHRD_MALLOC_TRACING_ENABLED(VTTHRD_MY_VTTHRD) )

/* library wrapper object */
static VTLibwrap* mallocwrap_lw = VT_LIBWRAP_NULL;

/* library wrapper attributes */
static VTLibwrapAttr mallocwrap_lw_attr = {

  /* The functions to be wrapped are defined in the LIBC which is linked to
     the application. So there is no need to search the actual function
     pointers in an additional library. */
  0,        /* shlibs_num */
  { NULL }, /* shlibs */

  /* Function group to define, finally including the recorded LIBC memory
     (de)allocation functions */
  "LIBC-MALLOC", /* func_group */

  /* Do not search the actual function pointers in an external LIBC, because
     dlopen calls malloc which would result in an infinite recursion when
     determining the actual function pointer of malloc. Using RTLD_NEXT
     instead. */
  0, /* libc */

  /* Do not initialize VampirTrace when creating the library wrapper object,
     resp. when a wrapper function is entered */
  1 /* wait_for_init */
};

/* id of memory related counter group */
static uint32_t mallocwrap_counter_group_id = 0;

/* id of memory allocation counter */
static uint32_t mallocwrap_counter_id = 0;

/* ids of memory (de)allocation markers */
static uint32_t mallocwrap_marker_alloc_id = 0;
static uint32_t mallocwrap_marker_free_id = 0;

/* flag: write memory (de)allocation markers? (env. VT_MEMTRACE_MARKER) */
static uint32_t mallocwrap_write_markers = 0;


/* memory allocation wrapper initialization/finalization functions called
   by vt_open/vt_close */

void vt_mallocwrap_init()
{
  /* define memory related counter group */
  mallocwrap_counter_group_id =
    vt_def_counter_group(VT_CURRENT_THREAD, "Memory");

  /* define memory allocation counter */
  mallocwrap_counter_id =
    vt_def_counter(VT_CURRENT_THREAD, "Memory Allocation", "Bytes",
      VT_CNTR_ABS | VT_CNTR_NEXT, mallocwrap_counter_group_id, 0);

  /* define memory (de)allocation markers, if desired
     (env. VT_MEMTRACE_MARKER) */
  if( (mallocwrap_write_markers = vt_env_memtrace_marker()) )
  {
    mallocwrap_marker_alloc_id =
      vt_def_marker(VT_CURRENT_THREAD, "Memory Allocation", VT_MARKER_HINT);
    mallocwrap_marker_free_id =
      vt_def_marker(VT_CURRENT_THREAD, "Memory Deallocation", VT_MARKER_HINT);
  }
}

void vt_mallocwrap_finalize()
{
  /* delete library wrapper object, if necessary */
  if( mallocwrap_lw != VT_LIBWRAP_NULL )
    VTLibwrap_delete(mallocwrap_lw);
}


/* wrapper functions */

/* -- stdlib.h:malloc -- */
void* malloc(size_t size)
{
  void* ret;

  /* initialize this wrapper function */
  MALLOCWRAP_FUNC_INIT("malloc", void*, (size_t));

  /* once, get the actual function pointer */
  MALLOCWRAP_GET_FUNC_PTR();

  if( MALLOCWRAP_DO_TRACE() )
  {
    uint32_t tid;
    uint64_t time;
    uint64_t bytes;
    uint64_t* counter_val;
    uint8_t was_recorded;

    /* get calling thread id */
    tid = VT_MY_THREAD;

    /* suspend LIBC memory (de)allocation tracing */
    VT_SUSPEND_MALLOC_TRACING(tid);

    /* get timestamp for the following function enter event */
    time = vt_pform_wtime();

    /* once, get unique function identifier */
    MALLOCWRAP_GET_FUNC_ID();

    /* record function enter event */
    was_recorded = vt_enter(tid, &time, VT_LIBWRAP_FUNC_ID);

    /* call the actual library function */
    ret = MALLOCWRAP_FUNC_CALL((size));

    /* get total allocated memory */
    if( ret != NULL )
    {
/*      bytes = ( ~ (uint64_t) 3 ) & (uint64_t) *( (size_t*) ( (char*)ret - SIZEOF_VOIDP ) );*/
      bytes = (uint64_t)malloc_usable_size(ret);
    }
    else
    {
      bytes = 0;
    }

    /* get pointer to thread's memory allocation counter value and update */
    counter_val = &(VTTHRD_MALLOC_TRACING_COUNTER_VAL(VTThrdv[tid]));
    *counter_val += bytes;

    /* get timestamp for the following function exit event [+ marker] */
    time = vt_pform_wtime();

    if( was_recorded && bytes > 0 )
    {
      /* write marker, if desired */
      if( mallocwrap_write_markers )
      {
        vt_marker(tid, &time, mallocwrap_marker_alloc_id,
          "Allocated %llu Bytes", (unsigned long long)bytes);
      }

      /* write counter value */
      vt_count(tid, &time, mallocwrap_counter_id, *counter_val);
    }

    /* record function exit event */
    vt_exit(tid, &time);

    /* resume LIBC memory (de)allocation tracing */
    VT_RESUME_MALLOC_TRACING(tid);
  }
  else
  {
    /* call the actual library function */
    ret = MALLOCWRAP_FUNC_CALL((size));
  }

  /* get errno from external LIBC (not necessary if using RTLD_NEXT) */
  /*errno = vt_libwrap_get_libc_errno();*/

  return ret;
}

#ifdef MALLOCWRAP_CALLOC

/* -- stdlib.h:calloc -- */
void* calloc(size_t nmemb, size_t size)
{
  void* ret;

  /* initialize this wrapper function */
  MALLOCWRAP_FUNC_INIT("calloc", void*, (size_t, size_t));

  /* once, get the actual function pointer

     NOTE: The dlsym function which is used to determine the actual function
     pointer of calloc uses itself this function, which would ends up in an
     infinite recursion.
     In order to make it work we have to perform a quite dirty hack found on
     http://blog.bigpixel.ro/2010/09/interposing-calloc-on-linux:
     While we are trying to get the actual function pointer, we're returning
     NULL for the memory which needs to be allocated by dlsym, in hope that
     dlsym can handle this situation.
     If this workaround causes any problems, just undefine the MALLOCWRAP_CALLOC
     macro above to disable the calloc wrapper function completely. */
  if( VT_LIBWRAP_FUNC_PTR == VT_LIBWRAP_NULL )
  {
    /* flag for indicating that we are trying to get the actual function
       pointer of calloc */
    static uint8_t getting_func_ptr = 0;
    if( !getting_func_ptr )
    {
      /* before trying to get the actual function pointer of calloc, set
         an indicator in order to return NULL from the next calloc called from
         dlsym */
      getting_func_ptr = 1;
      VTLibwrap_func_init(mallocwrap_lw, VT_LIBWRAP_FUNC_NAME, NULL, 0,
        (void**)(&VT_LIBWRAP_FUNC_PTR), NULL);
      getting_func_ptr = 0;
    }
    else
    {
      /* assumed that this calloc is called from dlsym, return NULL */
      return NULL;
    }
  }

  if( MALLOCWRAP_DO_TRACE() )
  {
    uint32_t tid;
    uint64_t time;
    uint64_t bytes;
    uint64_t* counter_val;
    uint8_t was_recorded;

    /* get calling thread id */
    tid = VT_MY_THREAD;

    /* suspend LIBC memory (de)allocation tracing */
    VT_SUSPEND_MALLOC_TRACING(tid);

    /* get current timestamp for the following function enter event */
    time = vt_pform_wtime();

    /* once, get unique function identifier */
    MALLOCWRAP_GET_FUNC_ID();

    /* record function enter event */
    was_recorded = vt_enter(tid, &time, VT_LIBWRAP_FUNC_ID);

    /* call the actual library function */
    ret = MALLOCWRAP_FUNC_CALL((nmemb, size));

    /* get total allocated memory */
    if( ret != NULL )
    {
/*      bytes = ( ~ (uint64_t) 3 ) & (uint64_t) *( (size_t*) ( (char*)ret - SIZEOF_VOIDP ) );*/
      bytes = (uint64_t)malloc_usable_size(ret);
    }
    else
    {
      bytes = 0;
    }

    /* get pointer to thread's memory allocation counter value and update */
    counter_val = &(VTTHRD_MALLOC_TRACING_COUNTER_VAL(VTThrdv[tid]));
    *counter_val += bytes;

    /* get timestamp for the following function exit event [+ marker] */
    time = vt_pform_wtime();

    if( was_recorded && bytes > 0 )
    {
      /* write marker, if desired */
      if( mallocwrap_write_markers )
      {
        vt_marker(tid, &time, mallocwrap_marker_alloc_id,
          "Allocated %llu Bytes", (unsigned long long)bytes);
      }

      /* write counter value */
      vt_count(tid, &time, mallocwrap_counter_id, *counter_val);
    }

    /* record function exit event */
    vt_exit(tid, &time);

    /* resume LIBC memory (de)allocation tracing */
    VT_RESUME_MALLOC_TRACING(tid);
  }
  else
  {
    /* call the actual library function */
    ret = MALLOCWRAP_FUNC_CALL((nmemb, size));
  }

  /* get errno from external LIBC (not necessary if using RTLD_NEXT) */
  /*errno = vt_libwrap_get_libc_errno();*/

  return ret;
}

#endif /* MALLOCWRAP_CALLOC */

/* -- stdlib.h:realloc -- */
void* realloc(void* ptr, size_t size)
{
  void* ret;

  /* initialize this wrapper function */
  MALLOCWRAP_FUNC_INIT("realloc", void*, (void*, size_t));

  /* once, get the actual function pointer */
  MALLOCWRAP_GET_FUNC_PTR();

  if( MALLOCWRAP_DO_TRACE() )
  {
    uint32_t tid;
    uint64_t time;
    uint64_t bytes;
    uint64_t bytes1;
    uint64_t bytes2;
    uint64_t* counter_val;
    uint8_t was_recorded;

    /* get calling thread id */
    tid = VT_MY_THREAD;

    /* suspend LIBC memory (de)allocation tracing */
    VT_SUSPEND_MALLOC_TRACING(tid);

    /* get current timestamp for the following function enter event */
    time = vt_pform_wtime();

    /* once, get unique function identifier */
    MALLOCWRAP_GET_FUNC_ID();

    /* record function enter event */
    was_recorded = vt_enter(tid, &time, VT_LIBWRAP_FUNC_ID);

    /* get total allocated memory before realloc */
    if( ptr != NULL )
    {
/*      bytes1 = ( ~ (uint64_t) 3 ) & (uint64_t) *( (size_t*) ( (char*)ptr - SIZEOF_VOIDP ) );*/
      bytes1 = (uint64_t)malloc_usable_size(ptr);
    }
    else
    {
      bytes1 = bytes = 0;
    }

    /* call the actual library function */
    ret = MALLOCWRAP_FUNC_CALL((ptr, size));

    /* get total allocated memory after realloc */
    if( ret != NULL )
    {
/*      bytes2 = ( ~ (uint64_t) 3 ) & (uint64_t) *( (size_t*) ( (char*)ret - SIZEOF_VOIDP ) );*/
      bytes2 = (uint64_t)malloc_usable_size(ret);
      bytes = bytes2 < bytes1 ? bytes1 - bytes2 : bytes2 - bytes1;
    }
    else
    {
      bytes2 = bytes = 0;
    }

    /* get pointer to thread's memory allocation counter value and update */
    counter_val = &(VTTHRD_MALLOC_TRACING_COUNTER_VAL(VTThrdv[tid]));
    if( bytes2 < bytes1 )
    {
      if( bytes <= *counter_val )
        *counter_val -= bytes;
      else
        *counter_val = 0;
    }
    else
    {
      *counter_val += bytes;
    }

    /* get timestamp for the following function exit event [+ marker] */
    time = vt_pform_wtime();

    if( was_recorded && bytes > 0 )
    {
      /* write marker, if desired */
      if( mallocwrap_write_markers )
      {
        static const char* marker_prefix_alloced = "Allocated";
        static const char* marker_prefix_freed   = "Freed";

        uint32_t marker_id;
        const char* marker_prefix;

        if ( bytes2 < bytes1 )
        {
          marker_id = mallocwrap_marker_free_id;
          marker_prefix = marker_prefix_freed;
        }
        else
        {
          marker_id = mallocwrap_marker_alloc_id;
          marker_prefix = marker_prefix_alloced;
        }

        vt_marker(tid, &time, marker_id,
          "%s %llu Bytes", marker_prefix, (unsigned long long)bytes);
      }

      /* write counter value */
      vt_count(tid, &time, mallocwrap_counter_id, *counter_val);
    }

    /* record function exit event */
    vt_exit(tid, &time);

    /* resume LIBC memory (de)allocation tracing */
    VT_RESUME_MALLOC_TRACING(tid);
  }
  else
  {
    /* call the actual library function */
    ret = MALLOCWRAP_FUNC_CALL((ptr, size));
  }

  /* get errno from external LIBC (not necessary if using RTLD_NEXT) */
  /*errno = vt_libwrap_get_libc_errno();*/

  return ret;
}

/* -- stdlib.h:free -- */
void free(void* ptr)
{
  /* initialize this wrapper function */
  MALLOCWRAP_FUNC_INIT("free", void, (void*));

  /* once, get the actual function pointer */
  MALLOCWRAP_GET_FUNC_PTR();

  if( MALLOCWRAP_DO_TRACE() )
  {
    uint32_t tid;
    uint64_t time;
    uint64_t bytes;
    uint64_t* counter_val;
    uint8_t was_recorded;

    /* get calling thread id */
    tid = VT_MY_THREAD;

    /* suspend LIBC memory (de)allocation tracing */
    VT_SUSPEND_MALLOC_TRACING(tid);

    /* get current timestamp for the following function enter event */
    time = vt_pform_wtime();

    /* once, get unique function identifier */
    MALLOCWRAP_GET_FUNC_ID();

    /* record function enter event */
    was_recorded = vt_enter(tid, &time, VT_LIBWRAP_FUNC_ID);

    /* get total allocated memory to be freed */
    if( ptr != NULL )
    {
/*      bytes = ( ~ (uint64_t) 3 ) & (uint64_t) *( (size_t*) ( (char*)ptr - SIZEOF_VOIDP ) );*/
      bytes = (uint64_t)malloc_usable_size(ptr);
    }
    else
    {
      bytes = 0;
    }

    /* call the actual library function */
    MALLOCWRAP_FUNC_CALL((ptr));

    /* get pointer to thread's memory allocation counter value and update */
    counter_val = &(VTTHRD_MALLOC_TRACING_COUNTER_VAL(VTThrdv[tid]));
    if( bytes <= *counter_val )
      *counter_val -= bytes;
    else
      *counter_val = 0;

    /* get timestamp for the following function exit event [+ marker] */
    time = vt_pform_wtime();

    if( was_recorded && bytes > 0 )
    {
      /* write marker, if desired */
      if( mallocwrap_write_markers )
      {
        vt_marker(tid, &time, mallocwrap_marker_free_id,
          "Freed %llu Bytes", (unsigned long long)bytes);
      }

      /* write counter value */
      vt_count(tid, &time, mallocwrap_counter_id, *counter_val);
    }

    /* record function exit event */
    vt_exit(tid, &time);

    /* resume LIBC memory (de)allocation tracing */
    VT_RESUME_MALLOC_TRACING(tid);
  }
  else
  {
    /* call the actual library function */
    MALLOCWRAP_FUNC_CALL((ptr));
  }

  /* get errno from external LIBC (not necessary if using RTLD_NEXT) */
  /*errno = vt_libwrap_get_libc_errno();*/
}


#if defined(HAVE_POSIX_MEMALIGN) && HAVE_POSIX_MEMALIGN

/* -- stdlib.h:posix_memalign -- */
int posix_memalign(void** memptr, size_t alignment, size_t size)
{
  int ret;

  /* initialize this wrapper function */
  MALLOCWRAP_FUNC_INIT("posix_memalign", int, (void**, size_t, size_t));

  /* once, get the actual function pointer */
  MALLOCWRAP_GET_FUNC_PTR();

  if( MALLOCWRAP_DO_TRACE() )
  {
    uint32_t tid;
    uint64_t time;
    uint64_t bytes;
    uint64_t* counter_val;
    uint8_t was_recorded;

    /* get calling thread id */
    tid = VT_MY_THREAD;

    /* suspend LIBC memory (de)allocation tracing */
    VT_SUSPEND_MALLOC_TRACING(tid);

    /* get current timestamp for the following function enter event */
    time = vt_pform_wtime();

    /* once, get unique function identifier */
    MALLOCWRAP_GET_FUNC_ID();

    /* record function enter event */
    was_recorded = vt_enter(tid, &time, VT_LIBWRAP_FUNC_ID);

    /* call the actual library function */
    ret = MALLOCWRAP_FUNC_CALL((memptr, alignment, size));

    /* get total allocated memory */
    if( ret == 0 && memptr != NULL )
    {
/*      bytes = ( ~ (uint64_t) 3 ) & (uint64_t) *( (size_t*) ( (char*)*memptr - SIZEOF_VOIDP ) );*/
      bytes = (uint64_t)malloc_usable_size(*memptr);
    }
    else
    {
      bytes = 0;
    }

    /* get pointer to thread's memory allocation counter value and update */
    counter_val = &(VTTHRD_MALLOC_TRACING_COUNTER_VAL(VTThrdv[tid]));
    *counter_val += bytes;

    /* get timestamp for the following function exit event [+ marker] */
    time = vt_pform_wtime();

    if( was_recorded && bytes > 0 )
    {
      /* write marker, if desired */
      if( mallocwrap_write_markers )
      {
        vt_marker(tid, &time, mallocwrap_marker_alloc_id,
          "Allocated %llu Bytes", (unsigned long long)bytes);
      }

      /* write counter value */
      vt_count(tid, &time, mallocwrap_counter_id, *counter_val);
    }

    /* record function exit event */
    vt_exit(tid, &time);

    /* resume LIBC memory (de)allocation tracing */
    VT_RESUME_MALLOC_TRACING(tid);
  }
  else
  {
    /* call the actual library function */
    ret = MALLOCWRAP_FUNC_CALL((memptr, alignment, size));
  }

  return ret;
}

#endif /* HAVE_POSIX_MEMALIGN */

#if defined(HAVE_MEMALIGN) && HAVE_MEMALIGN

/* -- malloc.h:memalign -- */
void* memalign(size_t boundary, size_t size)
{
  void* ret;

  /* initialize this wrapper function */
  MALLOCWRAP_FUNC_INIT("memalign", void*, (size_t, size_t));

  /* once, get the actual function pointer */
  MALLOCWRAP_GET_FUNC_PTR();

  if( MALLOCWRAP_DO_TRACE() )
  {
    uint32_t tid;
    uint64_t time;
    uint64_t bytes;
    uint64_t* counter_val;
    uint8_t was_recorded;

    /* get calling thread id */
    tid = VT_MY_THREAD;

    /* suspend LIBC memory (de)allocation tracing */
    VT_SUSPEND_MALLOC_TRACING(tid);

    /* get current timestamp for the following function enter event */
    time = vt_pform_wtime();

    /* once, get unique function identifier */
    MALLOCWRAP_GET_FUNC_ID();

    /* record function enter event */
    was_recorded = vt_enter(tid, &time, VT_LIBWRAP_FUNC_ID);

    /* call the actual library function */
    ret = MALLOCWRAP_FUNC_CALL((boundary, size));

    /* get total allocated memory */
    if( ret != NULL )
    {
/*      bytes = ( ~ (uint64_t) 3 ) & (uint64_t) *( (size_t*) ( (char*)ret - SIZEOF_VOIDP ) );*/
      bytes = (uint64_t)malloc_usable_size(ret);
    }
    else
    {
      bytes = 0;
    }

    /* get pointer to thread's memory allocation counter value and update */
    counter_val = &(VTTHRD_MALLOC_TRACING_COUNTER_VAL(VTThrdv[tid]));
    *counter_val += bytes;

    /* get timestamp for the following function exit event [+ marker] */
    time = vt_pform_wtime();

    if( was_recorded && bytes > 0 )
    {
      /* write marker, if desired */
      if( mallocwrap_write_markers )
      {
        vt_marker(tid, &time, mallocwrap_marker_alloc_id,
          "Allocated %llu Bytes", (unsigned long long)bytes);
      }

      /* write counter value */
      vt_count(tid, &time, mallocwrap_counter_id, *counter_val);
    }

    /* record function exit event */
    vt_exit(tid, &time);

    /* resume LIBC memory (de)allocation tracing */
    VT_RESUME_MALLOC_TRACING(tid);
  }
  else
  {
    /* call the actual library function */
    ret = MALLOCWRAP_FUNC_CALL((boundary, size));
  }

  /* get errno from external LIBC (not necessary if using RTLD_NEXT) */
  /*errno = vt_libwrap_get_libc_errno();*/

  return ret;
}

#endif /* HAVE_MEMALIGN */

#if defined(HAVE_VALLOC) && HAVE_VALLOC

/* -- malloc.h:valloc -- */
void* valloc(size_t size)
{
  void* ret;

  /* initialize this wrapper function */
  MALLOCWRAP_FUNC_INIT("valloc", void*, (size_t));

  /* once, get the actual function pointer */
  MALLOCWRAP_GET_FUNC_PTR();

  if( MALLOCWRAP_DO_TRACE() )
  {
    uint32_t tid;
    uint64_t time;
    uint64_t bytes;
    uint64_t* counter_val;
    uint8_t was_recorded;

    /* get calling thread id */
    tid = VT_MY_THREAD;

    /* suspend LIBC memory (de)allocation tracing */
    VT_SUSPEND_MALLOC_TRACING(tid);

    /* get current timestamp for the following function enter event */
    time = vt_pform_wtime();

    /* once, get unique function identifier */
    MALLOCWRAP_GET_FUNC_ID();

    /* record function enter event */
    was_recorded = vt_enter(tid, &time, VT_LIBWRAP_FUNC_ID);

    /* call the actual library function */
    ret = MALLOCWRAP_FUNC_CALL((size));

    /* get total allocated memory */
    if( ret != NULL )
    {
/*      bytes = ( ~ (uint64_t) 3 ) & (uint64_t) *( (size_t*) ( (char*)ret - SIZEOF_VOIDP ) );*/
      bytes = (uint64_t)malloc_usable_size(ret);
    }
    else
    {
      bytes = 0;
    }

    /* get pointer to thread's memory allocation counter value and update */
    counter_val = &(VTTHRD_MALLOC_TRACING_COUNTER_VAL(VTThrdv[tid]));
    *counter_val += bytes;

    /* get timestamp for the following function exit event [+ marker] */
    time = vt_pform_wtime();

    if( was_recorded && bytes > 0 )
    {
      /* write marker, if desired */
      if( mallocwrap_write_markers )
      {
        vt_marker(tid, &time, mallocwrap_marker_alloc_id,
          "Allocated %llu Bytes", (unsigned long long)bytes);
      }

      /* write counter value */
      vt_count(tid, &time, mallocwrap_counter_id, *counter_val);
    }

    /* record function exit event */
    vt_exit(tid, &time);

    /* resume LIBC memory (de)allocation tracing */
    VT_RESUME_MALLOC_TRACING(tid);
  }
  else
  {
    /* call the actual library function */
    ret = MALLOCWRAP_FUNC_CALL((size));
  }

  /* get errno from external LIBC (not necessary if using RTLD_NEXT) */
  /*errno = vt_libwrap_get_libc_errno();*/

  return ret;
}

#endif /* HAVE_VALLOC */
