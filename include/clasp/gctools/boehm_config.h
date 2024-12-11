/* include/config.h.  Generated from config.h.in by configure.  */
/* include/config.h.in.  Generated from configure.ac by autoheader.  */

/* Define to recognise all pointers to the interior of objects. */
#define ALL_INTERIOR_POINTERS 1
#define REGISTER_TAG_DISPLACEMENTS 1

/* AO load, store and/or test-and-set primitives are implemented in
   libatomic_ops using locks. */
/* #undef BASE_ATOMIC_OPS_EMULATED */

/* Erroneously cleared dirty bits checking. Use only for debugging of the
   incremental collector. */
/* #undef CHECKSUMS */

/* See doc/README.macros. */
/* #undef DARWIN_DONT_PARSE_STACK */

/* Define to force debug headers on all objects. */
/* #undef DBG_HDRS_ALL */

/* Do not use user32.dll import library (Win32). */
/* #undef DONT_USE_USER32_DLL */

/* Define to enable eCos target support. */
/* #undef ECOS */

/* Wine getenv may not return NULL for missing entry. */
/* #undef EMPTY_GETENV_RESULTS */

/* Define to enable alternative finalization interface. */
#define ENABLE_DISCLAIM 1

/* Define to enable internal debug assertions. */
/* #undef GC_ASSERTIONS */

/* Define to enable atomic uncollectible allocation. */
#define GC_ATOMIC_UNCOLLECTABLE 1

/* Use C11 (GCC) atomic intrinsics instead of libatomic_ops primitives. */
#define GC_BUILTIN_ATOMIC 1

/* Define to build dynamic libraries with only API symbols exposed. */
/* #undef GC_DLL */

/* Skip the initial guess of data root sets. */
/* #undef GC_DONT_REGISTER_MAIN_STATIC_DATA */

/* Define to turn on GC_suspend_thread support. */
#define GC_ENABLE_SUSPEND_THREAD 1

/* Define to include support for gcj. */
#define GC_GCJ_SUPPORT 1

/* Define if backtrace information is supported. */
/* #undef GC_HAVE_BUILTIN_BACKTRACE */

/* Enable Win32 DllMain-based approach of threads registering. */
/* #undef GC_INSIDE_DLL */

/* Missing execinfo.h header. */
/* #undef GC_MISSING_EXECINFO_H */

/* Missing sigsetjmp function. */
/* #undef GC_NO_SIGSETJMP */

/* Disable threads discovery in GC. */
/* #undef GC_NO_THREADS_DISCOVERY */

/* Read environment variables from the GC 'env' file. */
/* #undef GC_READ_ENV_FILE */

/* Define to support platform-specific threads. */
#define GC_THREADS 1

/* Explicitly prefix exported/imported WINAPI symbols with '_'. */
/* #undef GC_UNDERSCORE_STDCALL */

/* Force the GC to use signals based on SIGRTMIN+k. */
/* #undef GC_USESIGRT_SIGNALS */

/* See doc/README.macros. */
/* #undef GC_USE_DLOPEN_WRAP */

/* The major version number of this GC release. */
#define GC_VERSION_MAJOR 8

/* The micro version number of this GC release. */
#define GC_VERSION_MICRO 0

/* The minor version number of this GC release. */
#define GC_VERSION_MINOR 2

/* Define to support pthreads-win32 or winpthreads. */
/* #undef GC_WIN32_PTHREADS */

/* Define to install pthread_atfork() handlers by default. */
#define HANDLE_FORK 1

/* Define to use 'dladdr' function. */
#define HAVE_DLADDR 1

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* Define to 1 if you have the `dl_iterate_phdr' function. */
/* #undef HAVE_DL_ITERATE_PHDR */

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* libatomic_ops AO_or primitive implementation is lock-free. */
/* #undef HAVE_LOCKFREE_AO_OR */

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to use 'pthread_setname_np(const char*)' function. */
#define HAVE_PTHREAD_SETNAME_NP_WITHOUT_TID 1

/* Define to use 'pthread_setname_np(pthread_t, const char*)' function. */
/* #undef HAVE_PTHREAD_SETNAME_NP_WITH_TID */

/* Define to use 'pthread_setname_np(pthread_t, const char*, void *)'
   function. */
/* #undef HAVE_PTHREAD_SETNAME_NP_WITH_TID_AND_ARG */

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Do not define DYNAMIC_LOADING even if supported (i.e., build the collector
   with disabled tracing of dynamic library data roots). */
/* #undef IGNORE_DYNAMIC_LOADING */

/* See doc/README.macros. */
#define JAVA_FINALIZATION 1

/* Define to save back-pointers in debugging headers. */
/* #undef KEEP_BACK_PTRS */

/* Define to optimize for large heaps or root sets. */
#define LARGE_CONFIG 1

/* Define to the sub-directory where libtool stores uninstalled libraries. */
#define LT_OBJDIR ".libs/"

/* See doc/README.macros. */
/* #undef MAKE_BACK_GRAPH */

/* Number of GC cycles to wait before unmapping an unused block. */
#define MUNMAP_THRESHOLD 6

/* Define to not use system clock (cross compiling). */
/* #undef NO_CLOCK */

/* Disable debugging, like GC_dump and its callees. */
/* #undef NO_DEBUGGING */

/* Define to make the collector not allocate executable memory by default. */
#define NO_EXECUTE_PERMISSION 1

/* Missing getcontext function. */
/* #undef NO_GETCONTEXT */

/* Prohibit installation of pthread_atfork() handlers. */
/* #undef NO_HANDLE_FORK */

/* Name of package */
#define PACKAGE "gc"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "https://github.com/ivmai/bdwgc/issues"

/* Define to the full name of this package. */
#define PACKAGE_NAME "gc"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "gc 8.2.0"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "gc"

/* Define to the home page for this package. */
#define PACKAGE_URL ""

/* Define to the version of this package. */
#define PACKAGE_VERSION "8.2.0"

/* Define to enable parallel marking. */
/* #undef PARALLEL_MARK */

/* If defined, redirect free to this function. */
/* #undef REDIRECT_FREE */

/* If defined, redirect malloc to this function. */
/* #undef REDIRECT_MALLOC */

/* If defined, redirect GC_realloc to this function. */
/* #undef REDIRECT_REALLOC */

/* The number of caller frames saved when allocating with the debugging API.
 */
/* #undef SAVE_CALL_COUNT */

/* Shorten the headers to minimize object size at the expense of checking for
   writes past the end (see doc/README.macros). */
/* #undef SHORT_DBG_HDRS */

/* Define to tune the collector for small heap sizes. */
/* #undef SMALL_CONFIG */

/* See the comment in gcconfig.h. */
/* #undef SOLARIS25_PROC_VDB_BUG_FIXED */

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Define to work around a Solaris 5.3 bug (see dyn_load.c). */
/* #undef SUNOS53_SHARED_LIB */

/* Define to enable thread-local allocation optimization. */
#define THREAD_LOCAL_ALLOC 1

/* Not exported by BOEHM? - add more freelists to each thread,
 * which lets our custom alloc kinds be done thread-locally. */
#define THREAD_FREELISTS_KINDS 8

/* Use Unicode (W) variant of Win32 API instead of ASCII (A) one. */
/* #undef UNICODE */

/* Define to use of compiler-support for thread-local variables. */
/* #undef USE_COMPILER_TLS */

/* Define to use mmap instead of sbrk to expand the heap. */
#define USE_MMAP 1

/* Define to return memory to OS with munmap calls (see doc/README.macros). */
#define USE_MUNMAP 1

/* Define to use Win32 VirtualAlloc (instead of sbrk or mmap) to expand the
   heap. */
/* #undef USE_WINALLOC */

/* Version number of package */
#define VERSION "8.2.0"

/* The POSIX feature macro. */
/* #undef _POSIX_C_SOURCE */

/* Indicates the use of pthreads (NetBSD). */
/* #undef _PTHREADS */

/* Required define if using POSIX threads. */
#define _REENTRANT 1

/* Define to `__inline__' or `__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */
#ifndef __cplusplus
/* #undef inline */
#endif
