/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2010 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * This file is included at the bottom of opal_config.h, and is
 * therefore a) after all the #define's that were output from
 * configure, and b) included in most/all files in Open MPI.
 *
 * Since this file is *only* ever included by opal_config.h, and
 * opal_config.h already has #ifndef/#endif protection, there is no
 * need to #ifndef/#endif protection here.
 */

#ifndef OPAL_CONFIG_H 
#error "opal_config_bottom.h should only be included from opal_config.h"
#endif

/*
 * If we build a static library, Visual C define the _LIB symbol. In the
 * case of a shared library _USERDLL get defined.
 *
 * OMPI_BUILDING and _LIB define how ompi_config.h
 * handles configuring all of Open MPI's "compatibility" code.  Both
 * constants will always be defined by the end of ompi_config.h.
 *
 * OMPI_BUILDING affects how much compatibility code is included by
 * ompi_config.h.  It will always be 1 or 0.  The user can set the
 * value before including either mpi.h or ompi_config.h and it will be
 * respected.  If ompi_config.h is included before mpi.h, it will
 * default to 1.  If mpi.h is included before ompi_config.h, it will
 * default to 0.
 */
#ifndef OMPI_BUILDING
#define OMPI_BUILDING 1
#endif

/*
 * Flex is trying to include the unistd.h file. As there is no configure
 * option or this, the flex generated files will try to include the file
 * even on platforms without unistd.h (such as Windows). Therefore, if we
 * know this file is not available, we can prevent flex from including it.
 */
#ifndef HAVE_UNISTD_H
#define YY_NO_UNISTD_H
#endif

/***********************************************************************
 *
 * code that should be in ompi_config_bottom.h regardless of build
 * status
 *
 **********************************************************************/

/* Do we have posix or solaris thread lib */
#define OPAL_HAVE_THREADS (OPAL_HAVE_POSIX_THREADS || OPAL_HAVE_SOLARIS_THREADS)
/* Do we have thread support? */
#define OPAL_HAVE_THREAD_SUPPORT (OPAL_ENABLE_MULTI_THREADS || OPAL_ENABLE_PROGRESS_THREADS)

/*
 * BEGIN_C_DECLS should be used at the beginning of your declarations,
 * so that C++ compilers don't mangle their names.  Use END_C_DECLS at
 * the end of C declarations.
 */
#undef BEGIN_C_DECLS
#undef END_C_DECLS
#if defined(c_plusplus) || defined(__cplusplus)
# define BEGIN_C_DECLS extern "C" {
# define END_C_DECLS }
#else
#define BEGIN_C_DECLS          /* empty */
#define END_C_DECLS            /* empty */
#endif

/**
 * The attribute definition should be included before any potential
 * usage.
 */
#if OPAL_HAVE_ATTRIBUTE_ALIGNED
#    define __opal_attribute_aligned__(a)    __attribute__((__aligned__(a)))
#    define __opal_attribute_aligned_max__   __attribute__((__aligned__))
#else
#    define __opal_attribute_aligned__(a)
#    define __opal_attribute_aligned_max__
#endif

#if OPAL_HAVE_ATTRIBUTE_ALWAYS_INLINE
#    define __opal_attribute_always_inline__ __attribute__((__always_inline__))
#else
#    define __opal_attribute_always_inline__
#endif

#if OPAL_HAVE_ATTRIBUTE_COLD
#    define __opal_attribute_cold__          __attribute__((__cold__))
#else
#    define __opal_attribute_cold__
#endif

#if OPAL_HAVE_ATTRIBUTE_CONST
#    define __opal_attribute_const__         __attribute__((__const__))
#else
#    define __opal_attribute_const__
#endif

#if OPAL_HAVE_ATTRIBUTE_DEPRECATED
#    define __opal_attribute_deprecated__    __attribute__((__deprecated__))
#else
#    define __opal_attribute_deprecated__
#endif

#if OPAL_HAVE_ATTRIBUTE_FORMAT
#    define __opal_attribute_format__(a,b,c) __attribute__((__format__(a, b, c)))
#else
#    define __opal_attribute_format__(a,b,c)
#endif

/* Use this __atribute__ on function-ptr declarations, only */
#if OPAL_HAVE_ATTRIBUTE_FORMAT_FUNCPTR
#    define __opal_attribute_format_funcptr__(a,b,c) __attribute__((__format__(a, b, c)))
#else
#    define __opal_attribute_format_funcptr__(a,b,c)
#endif

#if OPAL_HAVE_ATTRIBUTE_HOT
#    define __opal_attribute_hot__           __attribute__((__hot__))
#else
#    define __opal_attribute_hot__
#endif

#if OPAL_HAVE_ATTRIBUTE_MALLOC
#    define __opal_attribute_malloc__        __attribute__((__malloc__))
#else
#    define __opal_attribute_malloc__
#endif

#if OPAL_HAVE_ATTRIBUTE_MAY_ALIAS
#    define __opal_attribute_may_alias__     __attribute__((__may_alias__))
#else
#    define __opal_attribute_may_alias__
#endif

#if OPAL_HAVE_ATTRIBUTE_NO_INSTRUMENT_FUNCTION
#    define __opal_attribute_no_instrument_function__  __attribute__((__no_instrument_function__))
#else
#    define __opal_attribute_no_instrument_function__
#endif

#if OPAL_HAVE_ATTRIBUTE_NONNULL
#    define __opal_attribute_nonnull__(a)    __attribute__((__nonnull__(a)))
#    define __opal_attribute_nonnull_all__   __attribute__((__nonnull__))
#else
#    define __opal_attribute_nonnull__(a)
#    define __opal_attribute_nonnull_all__
#endif

#if OPAL_HAVE_ATTRIBUTE_NORETURN
#    define __opal_attribute_noreturn__      __attribute__((__noreturn__))
#else
#    define __opal_attribute_noreturn__
#endif

/* Use this __atribute__ on function-ptr declarations, only */
#if OPAL_HAVE_ATTRIBUTE_NORETURN_FUNCPTR
#    define __opal_attribute_noreturn_funcptr__  __attribute__((__noreturn__))
#else
#    define __opal_attribute_noreturn_funcptr__
#endif

#if OPAL_HAVE_ATTRIBUTE_PACKED
#    define __opal_attribute_packed__        __attribute__((__packed__))
#else
#    define __opal_attribute_packed__
#endif

#if OPAL_HAVE_ATTRIBUTE_PURE
#    define __opal_attribute_pure__          __attribute__((__pure__))
#else
#    define __opal_attribute_pure__
#endif

#if OPAL_HAVE_ATTRIBUTE_SENTINEL
#    define __opal_attribute_sentinel__      __attribute__((__sentinel__))
#else
#    define __opal_attribute_sentinel__
#endif

#if OPAL_HAVE_ATTRIBUTE_UNUSED
#    define __opal_attribute_unused__        __attribute__((__unused__))
#else
#    define __opal_attribute_unused__
#endif

#if OPAL_HAVE_ATTRIBUTE_VISIBILITY
#    define __opal_attribute_visibility__(a) __attribute__((__visibility__(a)))
#else
#    define __opal_attribute_visibility__(a)
#endif

#if OPAL_HAVE_ATTRIBUTE_WARN_UNUSED_RESULT
#    define __opal_attribute_warn_unused_result__ __attribute__((__warn_unused_result__))
#else
#    define __opal_attribute_warn_unused_result__
#endif

#if OPAL_HAVE_ATTRIBUTE_WEAK_ALIAS
#    define __opal_attribute_weak_alias__(a) __attribute__((__weak__, __alias__(a)))
#else
#    define __opal_attribute_weak_alias__(a)
#endif

/***********************************************************************
 *
 * Windows library interface declaration code
 *
 **********************************************************************/
#if !defined(__WINDOWS__)
#  if defined(_WIN32) || defined(WIN32) || defined(WIN64)
#    define __WINDOWS__
#  endif
#endif  /* !defined(__WINDOWS__) */

#if defined(__WINDOWS__)

#  if defined(_USRDLL)    /* building shared libraries (.DLL) */
#    if defined(OPAL_EXPORTS)
#      define OPAL_DECLSPEC        __declspec(dllexport)
#      define OPAL_MODULE_DECLSPEC
#    else
#      define OPAL_DECLSPEC        __declspec(dllimport)
#      if defined(OPAL_MODULE_EXPORTS)
#        define OPAL_MODULE_DECLSPEC __declspec(dllexport)
#      else
#        define OPAL_MODULE_DECLSPEC __declspec(dllimport)
#      endif  /* defined(OPAL_MODULE_EXPORTS) */
#    endif  /* defined(OPAL_EXPORTS) */
#  else          /* building static library */
#    if defined(OPAL_IMPORTS)
#      define OPAL_DECLSPEC        __declspec(dllimport)
#    else
#      define OPAL_DECLSPEC
#    endif  /* defined(OPAL_IMPORTS) */
#    define OPAL_MODULE_DECLSPEC
#  endif  /* defined(_USRDLL) */
#  include "opal/win32/win_compat.h"
#else
#  if OPAL_C_HAVE_VISIBILITY
#    define OPAL_DECLSPEC           __opal_attribute_visibility__("default")
#    define OPAL_MODULE_DECLSPEC    __opal_attribute_visibility__("default")
#  else
#    define OPAL_DECLSPEC
#    define OPAL_MODULE_DECLSPEC
#  endif
#endif  /* defined(__WINDOWS__) */

/*
 * Do we have <stdint.h>?
 */
#ifdef HAVE_STDINT_H
#if !defined(__STDC_LIMIT_MACROS) && (defined(c_plusplus) || defined (__cplusplus))
/* When using a C++ compiler, the max / min value #defines for std
   types are only included if __STDC_LIMIT_MACROS is set before
   including stdint.h */
#define __STDC_LIMIT_MACROS
#endif
#include "opal_config.h"
#include <stdint.h>
#else
#include "opal_stdint.h"
#endif

/***********************************************************************
 *
 * Code that is only for when building Open MPI or utilities that are
 * using the internals of Open MPI.  It should not be included when
 * building MPI applications
 *
 **********************************************************************/
#if OMPI_BUILDING

#ifndef HAVE_PTRDIFF_T
typedef OPAL_PTRDIFF_TYPE ptrdiff_t;
#endif

/*
 * If we're in C, we may need to bring in the bool type and true/false
 * constants.  OPAL_NEED_C_BOOL will be true if the compiler either
 * needs <stdbool.h> or doesn't define the bool type at all.
 */
#if !(defined(c_plusplus) || defined(__cplusplus))
#    if OPAL_NEED_C_BOOL
#        if OPAL_USE_STDBOOL_H
             /* If we're using <stdbool.h>, there is an implicit
                assumption that the C++ bool is the same size and has
                the same alignment.  However, configure may have
                disabled the MPI C++ bindings, so if "_Bool" exists,
                then use that sizeof. */
#            include <stdbool.h>
             /* This section exists because AC_SIZEOF(bool) may not be
                run in configure if we're not building the MPI C++
                bindings. */
#            undef SIZEOF_BOOL
#            if SIZEOF__BOOL > 0
#                define SIZEOF_BOOL SIZEOF__BOOL
#            else
                 /* If all else fails, assume it's 1 */
#                define SIZEOF_BOOL 1
#            endif
#        else
             /* We need to create a bool type and ensure that it's the
                same size / alignment as the C++ bool size /
                alignment */
#            define false 0
#            define true 1
#            if SIZEOF_BOOL == SIZEOF_CHAR && OPAL_ALIGNMENT_CXX_BOOL == OPAL_ALIGNMENT_CHAR
typedef unsigned char bool;
#            elif SIZEOF_BOOL == SIZEOF_SHORT && OPAL_ALIGNMENT_CXX_BOOL == OPAL_ALIGNMENT_SHORT
typedef short bool;
#            elif SIZEOF_BOOL == SIZEOF_INT && OPAL_ALIGNMENT_CXX_BOOL == OPAL_ALIGNMENT_INT
typedef int bool;
#            elif SIZEOF_BOOL == SIZEOF_LONG && OPAL_ALIGNMENT_CXX_BOOL == OPAL_ALIGNMENT_LONG
typedef long bool;
#            elif defined(SIZEOF_LONG_LONG) && defined(OPAL_ALIGNMENT_LONG) && SIZEOF_BOOL == SIZEOF_LONG && OPAL_ALIGNMENT_CXX_BOOL == OPAL_ALIGNMENT_LONG
typedef long long bool;
             /* If we have _Bool, use that */
#            elif SIZEOF__BOOL > 0
#                undef SIZEOF_BOOL
#                define bool _Bool
#                define SIZEOF_BOOL SIZEOF__BOOL
#            else
             /* If all else fails, just make bool be an unsigned char
                and size of 1 */
typedef unsigned char bool;
#                define SIZEOF_BOOL 1
#            endif
#        endif  /* OPAL_USE_STDBOOL_H */
#    endif  /* OPAL_NEED_C_BOOL */
#endif

/*
 * Maximum size of a filename path.
 */
#include <limits.h>
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#if defined(PATH_MAX)
#define OPAL_PATH_MAX	(PATH_MAX + 1)
#elif defined(_POSIX_PATH_MAX)
#define OPAL_PATH_MAX	(_POSIX_PATH_MAX + 1)
#else
#define OPAL_PATH_MAX	256
#endif

/*
 * Set the compile-time path-separator on this system and variable separator
 */
#ifdef __WINDOWS__
#define OPAL_PATH_SEP "\\"
#define OPAL_ENV_SEP  ';'
#else
#define OPAL_PATH_SEP "/"
#define OPAL_ENV_SEP  ':'
#endif


/*
 * Do we want memory debugging?
 *
 * A few scenarios:
 *
 * 1. In the OMPI C library: we want these defines in all cases
 * 2. In the OMPI C++ bindings: we do not want them
 * 3. In the OMPI C++ executables: we do want them
 *
 * So for 1, everyone must include <ompi_config.h> first.  For 2, the
 * C++ bindings will never include <ompi_config.h> -- they will only
 * include <mpi.h>, which includes <ompi_config.h>, but after
 * setting OMPI_BUILDING to 0  For 3, it's the same as 1 -- just include
 * <ompi_config.h> first.
 *
 * Give code that needs to include ompi_config.h but really can't have
 * this stuff enabled (like the memory manager code) a way to turn us
 * off
 */
#if OPAL_ENABLE_MEM_DEBUG && !defined(OPAL_DISABLE_ENABLE_MEM_DEBUG)

/* It is safe to include opal/util/malloc.h here because a) it will only
   happen when we are building OMPI and therefore have a full OMPI
   source tree [including headers] available, and b) we guaranteed to
   *not* to include anything else via opal/util/malloc.h, so we won't
   have Cascading Includes Of Death. */
#    include "opal/util/malloc.h"
#    if defined(malloc)
#        undef malloc
#    endif
#    define malloc(size) opal_malloc((size), __FILE__, __LINE__)
#    if defined(calloc)
#        undef calloc
#    endif
#    define calloc(nmembers, size) opal_calloc((nmembers), (size), __FILE__, __LINE__)
#    if defined(realloc)
#        undef realloc
#    endif
#    define realloc(ptr, size) opal_realloc((ptr), (size), __FILE__, __LINE__)
#    if defined(free)
#        undef free
#    endif
#    define free(ptr) opal_free((ptr), __FILE__, __LINE__)

/*
 * If we're mem debugging, make the OPAL_DEBUG_ZERO resolve to memset
 */
#    include <string.h>
#    define OPAL_DEBUG_ZERO(obj) memset(&(obj), 0, sizeof(obj))
#else
#    define OPAL_DEBUG_ZERO(obj)
#endif

/*
 * printf functions for portability (only when building Open MPI)
 */
#if !defined(HAVE_VASPRINTF) || !defined(HAVE_VSNPRINTF)
#include <stdarg.h>
#include <stdlib.h>
#endif

#if !defined(HAVE_ASPRINTF) || !defined(HAVE_SNPRINTF) || !defined(HAVE_VASPRINTF) || !defined(HAVE_VSNPRINTF)
#include "opal/util/printf.h"
#endif

#ifndef HAVE_ASPRINTF
# define asprintf opal_asprintf
#endif

#ifndef HAVE_SNPRINTF
# define snprintf opal_snprintf
#endif

#ifndef HAVE_VASPRINTF
# define vasprintf opal_vasprintf
#endif

#ifndef HAVE_VSNPRINTF
# define vsnprintf opal_vsnprintf
#endif

/*
 * Some platforms (Solaris) have a broken qsort implementation.  Work
 * around by using our own.
 */
#if OPAL_HAVE_BROKEN_QSORT
#ifdef qsort
#undef qsort
#endif

#include "opal/util/qsort.h"
#define qsort opal_qsort
#endif

/*
 * On some homogenous big-iron machines (Sandia's Red Storm), there
 * are no htonl and friends.  If that's the case, provide stubs.  I
 * would hope we never find a platform that doesn't have these macros
 * and would want to talk to the outside world... On other platforms
 * (like Windows) we fail to detect them correctly.
 */
#if !defined(__WINDOWS__) && !defined(HAVE_UNIX_BYTESWAP)
static inline uint32_t htonl(uint32_t hostvar) { return hostvar; }
static inline uint32_t ntohl(uint32_t netvar) { return netvar; }
static inline uint16_t htons(uint16_t hostvar) { return hostvar; }
static inline uint16_t ntohs(uint16_t netvar) { return netvar; }
#endif

/*
 * Define __func__-preprocessor directive if the compiler does not
 * already define it.  Define it to __FILE__ so that we at least have
 * a clue where the developer is trying to indicate where the error is
 * coming from (assuming that __func__ is typically used for
 * printf-style debugging).
 */
#if defined(HAVE_DECL___FUNC__) && !HAVE_DECL___FUNC__
#define __func__ __FILE__
#endif

/**
 * Because of the way we're using the opal_object inside Open MPI (i.e.
 * dynamic resolution at run-time to derive all objects from the basic
 * type), on Windows we have to build everything on C++ mode, simply
 * because the C mode does not support dynamic resolution in DLL. Therefore,
 * no automatic conversion is allowed. All types have to be explicitly casted
 * or the compiler generate an error. This is true even for the void* type. As
 * we use void* to silence others compilers in the resolution of the addr member
 * of the iovec structure, we have now to find a way around. The simplest solution
 * is to define a special type for this field (just for casting). It can be
 * set to void* on all platforms with the exception of windows where it has to be
 * char*.
 */
#if defined(__WINDOWS__)
#define IOVBASE_TYPE  char
#else
#define IOVBASE_TYPE  void
#endif  /* defined(__WINDOWS__) */

/**
 * If we generate our own bool type, we need a special way to cast the result
 * in such a way to keep the compilers silent. Right now, th only compiler who
 * complain about int to bool conversions is the Microsoft compiler.
 */
#if defined(__WINDOWS__)
#  define OPAL_INT_TO_BOOL(VALUE)  ((VALUE) != 0 ? true : false)
#else
#  define OPAL_INT_TO_BOOL(VALUE)  (bool)(VALUE)
#endif  /* defined(__WINDOWS__) */

/**
 * Top level define to check 2 things: a) if we want ipv6 support, and
 * b) the underlying system supports ipv6.  Having one #define for
 * this makes it simpler to check throughout the code base.
 */
#if OPAL_ENABLE_IPV6 && defined(HAVE_STRUCT_SOCKADDR_IN6)
#define OPAL_WANT_IPV6 1
#else
#define OPAL_WANT_IPV6 0
#endif

#if !defined(HAVE_STRUCT_SOCKADDR_STORAGE) && defined(HAVE_STRUCT_SOCKADDR_IN)
#define sockaddr_storage sockaddr
#define ss_family sa_family
#endif

/* Compatibility structure so that we don't have to have as many
   #if checks in the code base */
#if !defined(HAVE_STRUCT_SOCKADDR_IN6) && defined(HAVE_STRUCT_SOCKADDR_IN)
#define sockaddr_in6 sockaddr_in
#define sin6_len sin_len
#define sin6_family sin_family
#define sin6_port sin_port
#define sin6_addr sin_addr
#endif

#if !HAVE_DECL_AF_UNSPEC
#define AF_UNSPEC 0
#endif
#if !HAVE_DECL_PF_UNSPEC
#define PF_UNSPEC 0
#endif
#if !HAVE_DECL_AF_INET6
#define AF_INET6 AF_UNSPEC
#endif
#if !HAVE_DECL_PF_INET6
#define PF_INET6 PF_UNSPEC
#endif

#if defined(__APPLE__) && defined(HAVE_INTTYPES_H)
/* Prior to Mac OS X 10.3, the length modifier "ll" wasn't
   supported, but "q" was for long long.  This isn't ANSI
   C and causes a warning when using PRI?64 macros.  We
   don't support versions prior to OS X 10.3, so we dont'
   need such backward compatibility.  Instead, redefine
   the macros to be "ll", which is ANSI C and doesn't
   cause a compiler warning. */
#include <inttypes.h>
#if defined(__PRI_64_LENGTH_MODIFIER__)
#undef __PRI_64_LENGTH_MODIFIER__
#define __PRI_64_LENGTH_MODIFIER__ "ll"
#endif
#if defined(__SCN_64_LENGTH_MODIFIER__)
#undef __SCN_64_LENGTH_MODIFIER__
#define __SCN_64_LENGTH_MODIFIER__ "ll"
#endif
#endif

#ifdef MCS_VXWORKS
/* VXWorks puts some common functions in oddly named headers.  Rather
   than update all the places the functions are used, which would be a
   maintenance disatster, just update here... */
#ifdef HAVE_IOLIB_H
/* pipe(), ioctl() */
#include <ioLib.h>
#endif
#ifdef HAVE_SOCKLIB_H
/* socket() */
#include <sockLib.h>
#endif
#ifdef HAVE_HOSTLIB_H
/* gethostname() */
#include <hostLib.h>

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif
#endif

#endif

/* If we're in C++, then just undefine restrict and then define it to
   nothing.  "restrict" is not part of the C++ language, and we don't
   have a corresponding AC_CXX_RESTRICT to figure out what the C++
   compiler supports. */
#if defined(c_plusplus) || defined(__cplusplus)
#undef restrict
#define restrict
#endif

#endif /* OMPI_BUILDING */
