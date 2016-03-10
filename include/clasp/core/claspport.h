/*
    File: claspport.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
#ifndef Brcl_BRCLPORT_H
#define Brcl_BRCLPORT_H

#include "brclconfig.h" /* include for defines */

/* Some versions of HP-UX & Solaris need inttypes.h for int32_t,
   INT32_MAX, etc. */
#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#endif

#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif

/**************************************************************************
Symbols and macros to supply platform-independent interfaces to basic
C language & library operations whose spellings vary across platforms.

Please try to make documentation here as clear as possible:  by definition,
the stuff here is trying to illuminate C's darkest corners.

Config #defines referenced here:

SIGNED_RIGHT_SHIFT_ZERO_FILLS
Meaning:  To be defined iff i>>j does not extend the sign bit when i is a
          signed integral type and i < 0.
Used in:  Brcl_ARITHMETIC_RIGHT_SHIFT

Brcl_DEBUG
Meaning:  Extra checks compiled in for debug mode.
Used in:  Brcl_SAFE_DOWNCAST

HAVE_UINTPTR_T
Meaning:  The C9X type uintptr_t is supported by the compiler
Used in:  Brcl_uintptr_t

HAVE_LONG_LONG
Meaning:  The compiler supports the C type "long long"
Used in:  CLASP_LONG_LONG

**************************************************************************/

/* For backward compatibility only. Obsolete, do not use. */
#ifdef HAVE_PROTOTYPES
#define Brcl_PROTO(x) x
#else
#define Brcl_PROTO(x) ()
#endif
#ifndef Brcl_FPROTO
#define Brcl_FPROTO(x) Brcl_PROTO(x)
#endif

/* typedefs for some C9X-defined synonyms for integral types.
 *
 * The names in BridgeCommonLisp are exactly the same as the C9X names, except with a
 * Brcl_ prefix.  Until C9X is universally implemented, this is the only way
 * to ensure that BridgeCommonLisp gets reliable names that don't conflict with names
 * in non-BridgeCommonLisp code that are playing their own tricks to define the C9X
 * names.
 *
 * NOTE: don't go nuts here!  BridgeCommonLisp has no use for *most* of the C9X
 * integral synonyms.  Only define the ones we actually need.
 */

#ifdef HAVE_LONG_LONG
#ifndef CLASP_LONG_LONG
#define CLASP_LONG_LONG long long
#if defined(LLONG_MAX)
/* If LLONG_MAX is defined in limits.h, use that. */
#define CLASP_LLONG_MIN LLONG_MIN
#define CLASP_LLONG_MAX LLONG_MAX
#define CLASP_ULLONG_MAX ULLONG_MAX
#elif defined(__LONG_LONG_MAX__)
/* Otherwise, if GCC has a builtin define, use that. */
#define CLASP_LLONG_MAX __LONG_LONG_MAX__
#define CLASP_LLONG_MIN (-BRCL_LLONG_MAX - 1)
#define CLASP_ULLONG_MAX (__LONG_LONG_MAX__ * 2ULL + 1ULL)
#else
/* Otherwise, rely on two's complement. */
#define CLASP_ULLONG_MAX (~0ULL)
#define CLASP_LLONG_MAX ((long long)(BRCL_ULLONG_MAX >> 1))
#define CLASP_LLONG_MIN (-BRCL_LLONG_MAX - 1)
#endif /* LLONG_MAX */
#endif
#endif /* HAVE_LONG_LONG */

/* a build with 30-bit digits for BridgeCommonLisp long integers needs an exact-width
 * 32-bit unsigned integer type to store those digits.  (We could just use
 * type 'unsigned long', but that would be wasteful on a system where longs
 * are 64-bits.)  On Unix systems, the autoconf macro AC_TYPE_UINT32_T defines
 * uint32_t to be such a type unless stdint.h or inttypes.h defines uint32_t.
 * However, it doesn't set HAVE_UINT32_T, so we do that here.
 */
#if (defined UINT32_MAX || defined uint32_t)
#ifndef CLASP_UINT32_T
#define HAVE_UINT32_T 1
#define CLASP_UINT32_T uint32_t
#endif
#endif

/* Macros for a 64-bit unsigned integer type; used for type 'twodigits' in the
 * long integer implementation, when 30-bit digits are enabled.
 */
#if (defined UINT64_MAX || defined uint64_t)
#ifndef CLASP_UINT64_T
#define HAVE_UINT64_T 1
#define CLASP_UINT64_T uint64_t
#endif
#endif

/* Signed variants of the above */
#if (defined INT32_MAX || defined int32_t)
#ifndef CLASP_INT32_T
#define HAVE_INT32_T 1
#define CLASP_INT32_T int32_t
#endif
#endif
#if (defined INT64_MAX || defined int64_t)
#ifndef CLASP_INT64_T
#define HAVE_INT64_T 1
#define CLASP_INT64_T int64_t
#endif
#endif

/* If BRCLLONG_BITS_IN_DIGIT is not defined then we'll use 30-bit digits if all
   the necessary integer types are available, and we're on a 64-bit platform
   (as determined by SIZEOF_VOID_P); otherwise we use 15-bit digits. */

#ifndef BRCLLONG_BITS_IN_DIGIT
#if (defined HAVE_UINT64_T && defined HAVE_INT64_T && \
     defined HAVE_UINT32_T && defined HAVE_INT32_T && SIZEOF_VOID_P >= 8)
#define BRCLLONG_BITS_IN_DIGIT 30
#else
#define BRCLLONG_BITS_IN_DIGIT 15
#endif
#endif

/* uintptr_t is the C9X name for an unsigned integral type such that a
 * legitimate void* can be cast to uintptr_t and then back to void* again
 * without loss of information.  Similarly for intptr_t, wrt a signed
 * integral type.
 */
#ifdef HAVE_UINTPTR_T
typedef uintptr_t Brcl_uintptr_t;
typedef intptr_t Brcl_intptr_t;

#elif SIZEOF_VOID_P <= SIZEOF_INT
typedef unsigned int Brcl_uintptr_t;
typedef int Brcl_intptr_t;

#elif SIZEOF_VOID_P <= SIZEOF_LONG
typedef unsigned long Brcl_uintptr_t;
typedef long Brcl_intptr_t;

#elif defined(HAVE_LONG_LONG) && (SIZEOF_VOID_P <= SIZEOF_LONG_LONG)
typedef unsigned CLASP_LONG_LONG Brcl_uintptr_t;
typedef CLASP_LONG_LONG Brcl_intptr_t;

#else
#error "BridgeCommonLisp needs a typedef for Brcl_uintptr_t in pyport.h."
#endif /* HAVE_UINTPTR_T */

/* Brcl_ssize_t is a signed integral type such that sizeof(Brcl_ssize_t) ==
 * sizeof(size_t).  C99 doesn't define such a thing directly (size_t is an
 * unsigned integral type).  See PEP 353 for details.
 */
#ifdef HAVE_SSIZE_T
typedef ssize_t Brcl_ssize_t;
#elif SIZEOF_VOID_P == SIZEOF_SIZE_T
typedef Brcl_intptr_t Brcl_ssize_t;
#else
#error "BridgeCommonLisp needs a typedef for Brcl_ssize_t in pyport.h."
#endif

/* Largest possible value of size_t.
   SIZE_MAX is part of C99, so it might be defined on some
   platforms. If it is not defined, (size_t)-1 is a portable
   definition for C89, due to the way signed->unsigned
   conversion is defined. */
#ifdef SIZE_MAX
#define CLASP_SIZE_MAX SIZE_MAX
#else
#define CLASP_SIZE_MAX ((size_t)-1)
#endif

/* Largest positive value of type Brcl_ssize_t. */
#define CLASP_SSIZE_T_MAX ((Brcl_ssize_t)(((size_t)-1) >> 1))
/* Smallest negative value of type Brcl_ssize_t. */
#define CLASP_SSIZE_T_MIN (-BRCL_SSIZE_T_MAX - 1)

#if SIZEOF_PID_T > SIZEOF_LONG
#error "BridgeCommonLisp doesn't support sizeof(pid_t) > sizeof(long)"
#endif

/* CLASP_FORMAT_SIZE_T is a platform-specific modifier for use in a printf
 * format to convert an argument with the width of a size_t or Brcl_ssize_t.
 * C99 introduced "z" for this purpose, but not all platforms support that;
 * e.g., MS compilers use "I" instead.
 *
 * These "high level" BridgeCommonLisp format functions interpret "z" correctly on
 * all platforms (BridgeCommonLisp interprets the format string itself, and does whatever
 * the platform C requires to convert a size_t/Brcl_ssize_t argument):
 *
 *     PyString_FromFormat
 *     PyErr_Format
 *     PyString_FromFormatV
 *
 * Lower-level uses require that you interpolate the correct format modifier
 * yourself (e.g., calling printf, fprintf, sprintf, PyOS_snprintf); for
 * example,
 *
 *     Brcl_ssize_t index;
 *     fprintf(stderr, "index %" CLASP_FORMAT_SIZE_T "d sucks\n", index);
 *
 * That will expand to %ld, or %Id, or to something else correct for a
 * Brcl_ssize_t on the platform.
 */
#ifndef CLASP_FORMAT_SIZE_T
#if SIZEOF_SIZE_T == SIZEOF_INT && !defined(__APPLE__)
#define CLASP_FORMAT_SIZE_T ""
#elif SIZEOF_SIZE_T == SIZEOF_LONG
#define CLASP_FORMAT_SIZE_T "l"
#elif defined(MS_WINDOWS)
#define CLASP_FORMAT_SIZE_T "I"
#else
#error "This platform's pyconfig.h needs to define CLASP_FORMAT_SIZE_T"
#endif
#endif

/* CLASP_FORMAT_LONG_LONG is analogous to BRCL_FORMAT_SIZE_T above, but for
 * the long long type instead of the size_t type.  It's only available
 * when HAVE_LONG_LONG is defined. The "high level" BridgeCommonLisp format
 * functions listed above will interpret "lld" or "llu" correctly on
 * all platforms.
 */
#ifdef HAVE_LONG_LONG
#ifndef CLASP_FORMAT_LONG_LONG
#if defined(MS_WIN64) || defined(MS_WINDOWS)
#define CLASP_FORMAT_LONG_LONG "I64"
#else
#error "This platform's pyconfig.h needs to define CLASP_FORMAT_LONG_LONG"
#endif
#endif
#endif

/* Brcl_LOCAL can be used instead of static to get the fastest possible calling
 * convention for functions that are local to a given module.
 *
 * Brcl_LOCAL_INLINE does the same thing, and also explicitly requests inlining,
 * for platforms that support that.
 *
 * If CLASP_LOCAL_AGGRESSIVE is defined before python.h is included, more
 * "aggressive" inlining/optimizaion is enabled for the entire module.  This
 * may lead to code bloat, and may slow things down for those reasons.  It may
 * also lead to errors, if the code relies on pointer aliasing.  Use with
 * care.
 *
 * NOTE: You can only use this for functions that are entirely local to a
 * module; functions that are exported via method tables, callbacks, etc,
 * should keep using static.
 */

#undef USE_INLINE /* XXX - set via configure? */

#if defined(_MSC_VER)
#if defined(CLASP_LOCAL_AGGRESSIVE)
/* enable more aggressive optimization for visual studio */
#pragma optimize("agtw", on)
#endif
/* ignore warnings if the compiler decides not to inline a function */
#pragma warning(disable : 4710)
/* fastest possible local call under MSVC */
#define Brcl_LOCAL(type) static type __fastcall
#define Brcl_LOCAL_INLINE(type) static __inline type __fastcall
#elif defined(USE_INLINE)
#define Brcl_LOCAL(type) static type
#define Brcl_LOCAL_INLINE(type) static inline type
#else
#define Brcl_LOCAL(type) static type
#define Brcl_LOCAL_INLINE(type) static type
#endif

/* Brcl_MEMCPY can be used instead of memcpy in cases where the copied blocks
 * are often very short.  While most platforms have highly optimized code for
 * large transfers, the setup costs for memcpy are often quite high.  MEMCPY
 * solves this by doing short copies "in line".
 */

#if defined(_MSC_VER)
#define Brcl_MEMCPY(target, source, length) \
  do {                                      \
    size_t i_, n_ = (length);               \
    char *t_ = (void *)(target);            \
    const char *s_ = (void *)(source);      \
    if (n_ >= 16)                           \
      memcpy(t_, s_, n_);                   \
    else                                    \
      for (i_ = 0; i_ < n_; i_++)           \
        t_[i_] = s_[i_];                    \
  } while (0)
#else
#define Brcl_MEMCPY memcpy
#endif

#include <stdlib.h>

#ifdef HAVE_IEEEFP_H
#include <ieeefp.h> /* needed for 'finite' declaration on some platforms */
#endif

#include <math.h> /* Moved here from the math section, before extern "C" */

/********************************************
 * WRAPPER FOR <time.h> and/or <sys/time.h> *
 ********************************************/

#ifdef TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else /* !TIME_WITH_SYS_TIME */
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */

/******************************
 * WRAPPER FOR <sys/select.h> *
 ******************************/

/* NB caller must include <sys/types.h> */

#ifdef HAVE_SYS_SELECT_H

#include <sys/select.h>

#endif /* !HAVE_SYS_SELECT_H */

/*******************************
 * stat() and fstat() fiddling *
 *******************************/

/* We expect that stat and fstat exist on most systems.
 *  It's confirmed on Unix, Mac and Windows.
 *  If you don't have them, add
 *      #define DONT_HAVE_STAT
 * and/or
 *      #define DONT_HAVE_FSTAT
 * to your pyconfig.h. BridgeCommonLisp code beyond this should check HAVE_STAT and
 * HAVE_FSTAT instead.
 * Also
 *      #define HAVE_SYS_STAT_H
 * if <sys/stat.h> exists on your platform, and
 *      #define HAVE_STAT_H
 * if <stat.h> does.
 */
#ifndef DONT_HAVE_STAT
#define HAVE_STAT
#endif

#ifndef DONT_HAVE_FSTAT
#define HAVE_FSTAT
#endif

#ifdef RISCOS
#include <sys/types.h>
#include <unixstuff.h>
#endif

#ifdef HAVE_SYS_STAT_H
#if defined(BRCLOS_OS2) && defined(BRCLCC_GCC)
#include <sys/types.h>
#endif
#include <sys/stat.h>
#elif defined(HAVE_STAT_H)
#include <stat.h>
#endif

#if defined(BRCLCC_VACPP)
/* VisualAge C/C++ Failed to Define MountType Field in sys/stat.h */
#define S_IFMT (S_IFDIR | S_IFCHR | S_IFREG)
#endif

#ifndef S_ISREG
#define S_ISREG(x) (((x)&S_IFMT) == S_IFREG)
#endif

#ifndef S_ISDIR
#define S_ISDIR(x) (((x)&S_IFMT) == S_IFDIR)
#endif

#ifdef __cplusplus
/* Move this down here since some C++ #include's don't like to be included
   inside an extern "C" */
extern "C" {
#endif

/* Brcl_ARITHMETIC_RIGHT_SHIFT
 * C doesn't define whether a right-shift of a signed integer sign-extends
 * or zero-fills.  Here a macro to force sign extension:
 * Brcl_ARITHMETIC_RIGHT_SHIFT(TYPE, I, J)
 *    Return I >> J, forcing sign extension.  Arithmetically, return the
 *    floor of I/2**J.
 * Requirements:
 *    I should have signed integer type.  In the terminology of C99, this can
 *    be either one of the five standard signed integer types (signed char,
 *    short, int, long, long long) or an extended signed integer type.
 *    J is an integer >= 0 and strictly less than the number of bits in the
 *    type of I (because C doesn't define what happens for J outside that
 *    range either).
 *    TYPE used to specify the type of I, but is now ignored.  It's been left
 *    in for backwards compatibility with versions <= 2.6 or 3.0.
 * Caution:
 *    I may be evaluated more than once.
 */
#ifdef SIGNED_RIGHT_SHIFT_ZERO_FILLS
#define Brcl_ARITHMETIC_RIGHT_SHIFT(TYPE, I, J) \
  ((I) < 0 ? -1 - ((-1 - (I)) >> (J)) : (I) >> (J))
#else
#define Brcl_ARITHMETIC_RIGHT_SHIFT(TYPE, I, J) ((I) >> (J))
#endif

/* Brcl_FORCE_EXPANSION(X)
 * "Simply" returns its argument.  However, macro expansions within the
 * argument are evaluated.  This unfortunate trickery is needed to get
 * token-pasting to work as desired in some cases.
 */
#define Brcl_FORCE_EXPANSION(X) X

/* Brcl_SAFE_DOWNCAST(VALUE, WIDE, NARROW)
 * Cast VALUE to type NARROW from type WIDE.  In Brcl_DEBUG mode, this
 * assert-fails if any information is lost.
 * Caution:
 *    VALUE may be evaluated more than once.
 */
#ifdef Brcl_DEBUG
#define Brcl_SAFE_DOWNCAST(VALUE, WIDE, NARROW) \
  (assert((WIDE)(NARROW)(VALUE) == (VALUE)), (NARROW)(VALUE))
#else
#define Brcl_SAFE_DOWNCAST(VALUE, WIDE, NARROW) (NARROW)(VALUE)
#endif

/* Brcl_SET_ERRNO_ON_MATH_ERROR(x)
 * If a libm function did not set errno, but it looks like the result
 * overflowed or not-a-number, set errno to ERANGE or EDOM.  Set errno
 * to 0 before calling a libm function, and invoke this macro after,
 * passing the function result.
 * Caution:
 *    This isn't reliable.  See Brcl_OVERFLOWED comments.
 *    X is evaluated more than once.
 */
#if defined(__FreeBSD__) || defined(__OpenBSD__) || (defined(__hpux) && defined(__ia64))
#define _Brcl_SET_EDOM_FOR_NAN(X) \
  if (isnan(X))                   \
    errno = EDOM;
#else
#define _Brcl_SET_EDOM_FOR_NAN(X) ;
#endif
#define Brcl_SET_ERRNO_ON_MATH_ERROR(X)                  \
  do {                                                   \
    if (errno == 0) {                                    \
      if ((X) == Brcl_HUGE_VAL || (X) == -Brcl_HUGE_VAL) \
        errno = ERANGE;                                  \
      else                                               \
        _Brcl_SET_EDOM_FOR_NAN(X)                        \
    }                                                    \
  } while (0)

/* Brcl_SET_ERANGE_ON_OVERFLOW(x)
 * An alias of Brcl_SET_ERRNO_ON_MATH_ERROR for backward-compatibility.
 */
#define Brcl_SET_ERANGE_IF_OVERFLOW(X) Brcl_SET_ERRNO_ON_MATH_ERROR(X)

/* Brcl_ADJUST_ERANGE1(x)
 * Brcl_ADJUST_ERANGE2(x, y)
 * Set errno to 0 before calling a libm function, and invoke one of these
 * macros after, passing the function result(s) (Brcl_ADJUST_ERANGE2 is useful
 * for functions returning complex results).  This makes two kinds of
 * adjustments to errno:  (A) If it looks like the platform libm set
 * errno=ERANGE due to underflow, clear errno. (B) If it looks like the
 * platform libm overflowed but didn't set errno, force errno to ERANGE.  In
 * effect, we're trying to force a useful implementation of C89 errno
 * behavior.
 * Caution:
 *    This isn't reliable.  See Brcl_OVERFLOWED comments.
 *    X and Y may be evaluated more than once.
 */
#define Brcl_ADJUST_ERANGE1(X)                           \
  do {                                                   \
    if (errno == 0) {                                    \
      if ((X) == Brcl_HUGE_VAL || (X) == -Brcl_HUGE_VAL) \
        errno = ERANGE;                                  \
    } else if (errno == ERANGE && (X) == 0.0)            \
      errno = 0;                                         \
  } while (0)

#define Brcl_ADJUST_ERANGE2(X, Y)                                                                         \
  do {                                                                                                    \
    if ((X) == Brcl_HUGE_VAL || (X) == -Brcl_HUGE_VAL || (Y) == Brcl_HUGE_VAL || (Y) == -Brcl_HUGE_VAL) { \
      if (errno == 0)                                                                                     \
        errno = ERANGE;                                                                                   \
    } else if (errno == ERANGE)                                                                           \
      errno = 0;                                                                                          \
  } while (0)

/*  The functions _Brcl_dg_strtod and _Brcl_dg_dtoa in BridgeCommonLisp/dtoa.c (which are
 *  required to support the short float repr introduced in BridgeCommonLisp 3.1) require
 *  that the floating-point unit that's being used for arithmetic operations
 *  on C doubles is set to use 53-bit precision.  It also requires that the
 *  FPU rounding mode is round-half-to-even, but that's less often an issue.
 *
 *  If your FPU isn't already set to 53-bit precision/round-half-to-even, and
 *  you want to make use of _Brcl_dg_strtod and _Brcl_dg_dtoa, then you should
 *
 *     #define HAVE_CLASP_SET_53BIT_PRECISION 1
 *
 *  and also give appropriate definitions for the following three macros:
 *
 *    _CLASP_SET_53BIT_PRECISION_START : store original FPU settings, and
 *        set FPU to 53-bit precision/round-half-to-even
 *    _CLASP_SET_53BIT_PRECISION_END : restore original FPU settings
 *    _CLASP_SET_53BIT_PRECISION_HEADER : any variable declarations needed to
 *        use the two macros above.
 *
 * The macros are designed to be used within a single C function: see
 * BridgeCommonLisp/pystrtod.c for an example of their use.
 */

/* get and set x87 control word for gcc/x86 */
#ifdef HAVE_GCC_ASM_FOR_X87
#define HAVE_CLASP_SET_53BIT_PRECISION 1
/* _Brcl__get/set_387controlword functions are defined in BridgeCommonLisp/pymath.c */
#define _Brcl_SET_53BIT_PRECISION_HEADER \
  unsigned short old_387controlword, new_387controlword
#define _Brcl_SET_53BIT_PRECISION_START                           \
  do {                                                            \
    old_387controlword = _Brcl__get_387controlword();              \
    new_387controlword = (old_387controlword & ~0x0f00) | 0x0200; \
    if (new_387controlword != old_387controlword)                 \
      _Brcl__set_387controlword(new_387controlword);               \
  } while (0)
#define _Brcl_SET_53BIT_PRECISION_END           \
  if (new_387controlword != old_387controlword) \
  _Brcl__set_387controlword(old_387controlword)
#endif

/* default definitions are empty */
#ifndef HAVE_CLASP_SET_53BIT_PRECISION
#define _Brcl_SET_53BIT_PRECISION_HEADER
#define _Brcl_SET_53BIT_PRECISION_START
#define _Brcl_SET_53BIT_PRECISION_END
#endif

/* If we can't guarantee 53-bit precision, don't use the code
   in BridgeCommonLisp/dtoa.c, but fall back to standard code.  This
   means that repr of a float will be long (17 sig digits).

   Realistically, there are two things that could go wrong:

   (1) doubles aren't IEEE 754 doubles, or
   (2) we're on x86 with the rounding precision set to 64-bits
       (extended precision), and we don't know how to change
       the rounding precision.
 */

#if !defined(DOUBLE_IS_LITTLE_ENDIAN_IEEE754) && \
    !defined(DOUBLE_IS_BIG_ENDIAN_IEEE754) &&    \
    !defined(DOUBLE_IS_ARM_MIXED_ENDIAN_IEEE754)
#define CLASP_NO_SHORT_FLOAT_REPR
#endif

/* double rounding is symptomatic of use of extended precision on x86.  If
   we're seeing double rounding, and we don't have any mechanism available for
   changing the FPU rounding precision, then don't use BridgeCommonLisp/dtoa.c. */
#if defined(X87_DOUBLE_ROUNDING) && !defined(HAVE_CLASP_SET_53BIT_PRECISION)
#define CLASP_NO_SHORT_FLOAT_REPR
#endif

/* Brcl_DEPRECATED(version)
 * Declare a variable, type, or function deprecated.
 * Usage:
 *    extern int old_var Brcl_DEPRECATED(2.3);
 *    typedef int T1 Brcl_DEPRECATED(2.4);
 *    extern int x() Brcl_DEPRECATED(2.5);
 */
#if defined(__GNUC__) && ((__GNUC__ >= 4) || \
                          (__GNUC__ == 3) && (__GNUC_MINOR__ >= 1))
#define Brcl_DEPRECATED(VERSION_UNUSED) __attribute__((__deprecated__))
#else
#define Brcl_DEPRECATED(VERSION_UNUSED)
#endif

/**************************************************************************
Prototypes that are missing from the standard include files on some systems
(and possibly only some versions of such systems.)

Please be conservative with adding new ones, document them and enclose them
in platform-specific #ifdefs.
**************************************************************************/

#ifdef SOLARIS
/* Unchecked */
extern int gethostname(char *, int);
#endif

#ifdef __BEOS__
/* Unchecked */
/* It's in the libs, but not the headers... - [cjh] */
int shutdown(int, int);
#endif

#ifdef HAVE__GETPTY
#include <sys/types.h> /* we need to import mode_t */
extern char *_getpty(int *, int, mode_t, int);
#endif

/* On QNX 6, struct termio must be declared by including sys/termio.h
   if TCGETA, TCSETA, TCSETAW, or TCSETAF are used.  sys/termio.h must
   be included before termios.h or it will generate an error. */
#ifdef HAVE_SYS_TERMIO_H
#include <sys/termio.h>
#endif

#if defined(HAVE_OPENPTY) || defined(HAVE_FORKPTY)
#if !defined(HAVE_PTY_H) && !defined(HAVE_LIBUTIL_H) && !defined(HAVE_UTIL_H)
/* BSDI does not supply a prototype for the 'openpty' and 'forkpty'
   functions, even though they are included in libutil. */
#include <termios.h>
extern int openpty(int *, int *, char *, struct termios *, struct winsize *);
extern pid_t forkpty(int *, char *, struct termios *, struct winsize *);
#endif /* !defined(HAVE_PTY_H) && !defined(HAVE_LIBUTIL_H) */
#endif /* defined(HAVE_OPENPTY) || defined(HAVE_FORKPTY) */

/* These are pulled from various places. It isn't obvious on what platforms
   they are necessary, nor what the exact prototype should look like (which
   is likely to vary between platforms!) If you find you need one of these
   declarations, please move them to a platform-specific block and include
   proper prototypes. */
#if 0

/* From Modules/resource.c */
extern int getrusage();
extern int getpagesize();

/* From BridgeCommonLisp/sysmodule.c and Modules/posixmodule.c */
extern int fclose(FILE *);

/* From Modules/posixmodule.c */
extern int fdatasync(int);
#endif /* 0 */

/* On 4.4BSD-descendants, ctype functions serves the whole range of
 * wchar_t character set rather than single byte code points only.
 * This characteristic can break some operations of string object
 * including str.upper() and str.split() on UTF-8 locales.  This
 * workaround was provided by Tim Robbins of FreeBSD project.
 */

#ifdef __FreeBSD__
#include <osreldate.h>
#if __FreeBSD_version > 500039
#define _CLASP_PORT_CTYPE_UTF8_ISSUE
#endif
#endif

#if defined(__APPLE__)
#define _CLASP_PORT_CTYPE_UTF8_ISSUE
#endif

#ifdef _CLASP_PORT_CTYPE_UTF8_ISSUE
#include <ctype.h>
#include <wctype.h>
#undef isalnum
#define isalnum(c) iswalnum(btowc(c))
#undef isalpha
#define isalpha(c) iswalpha(btowc(c))
#undef islower
#define islower(c) iswlower(btowc(c))
#undef isspace
#define isspace(c) iswspace(btowc(c))
#undef isupper
#define isupper(c) iswupper(btowc(c))
#undef tolower
#define tolower(c) towlower(btowc(c))
#undef toupper
#define toupper(c) towupper(btowc(c))
#endif

/* Declarations for symbol visibility.

  PyAPI_FUNC(type): Declares a public BridgeCommonLisp API function and return type
  PyAPI_DATA(type): Declares public BridgeCommonLisp data and its type
  PyMODINIT_FUNC:   A BridgeCommonLisp module init function.  If these functions are
                    inside the BridgeCommonLisp core, they are private to the core.
                    If in an extension module, it may be declared with
                    external linkage depending on the platform.

  As a number of platforms support/require "__declspec(dllimport/dllexport)",
  we support a HAVE_DECLSPEC_DLL macro to save duplication.
*/

/*
  All windows ports, except cygwin, are handled in PC/pyconfig.h.

  BeOS and cygwin are the only other autoconf platform requiring special
  linkage handling and both of these use __declspec().
*/
#if defined(__CYGWIN__) || defined(__BEOS__)
#define HAVE_DECLSPEC_DLL
#endif

/* only get special linkage if built as shared or platform is Cygwin */
#if defined(Brcl_ENABLE_SHARED) || defined(__CYGWIN__)
#if defined(HAVE_DECLSPEC_DLL)
#ifdef Brcl_BUILD_CORE
#define PyAPI_FUNC(RTYPE) __declspec(dllexport) RTYPE
#define PyAPI_DATA(RTYPE) extern __declspec(dllexport) RTYPE
/* module init functions inside the core need no external linkage */
/* except for Cygwin to handle embedding (FIXME: BeOS too?) */
#if defined(__CYGWIN__)
#define PyMODINIT_FUNC __declspec(dllexport) void
#else /* __CYGWIN__ */
#define PyMODINIT_FUNC void
#endif /* __CYGWIN__ */
#else  /* Brcl_BUILD_CORE */
/* Building an extension module, or an embedded situation */
/* public BridgeCommonLisp functions and data are imported */
/* Under Cygwin, auto-import functions to prevent compilation */
/* failures similar to those described at the bottom of 4.1: */
/* http://docs.python.org/extending/windows.html#a-cookbook-approach */
#if !defined(__CYGWIN__)
#define PyAPI_FUNC(RTYPE) __declspec(dllimport) RTYPE
#endif /* !__CYGWIN__ */
#define PyAPI_DATA(RTYPE) extern __declspec(dllimport) RTYPE
/* module init functions outside the core must be exported */
#if defined(__cplusplus)
#define PyMODINIT_FUNC extern "C" __declspec(dllexport) void
#else /* __cplusplus */
#define PyMODINIT_FUNC __declspec(dllexport) void
#endif /* __cplusplus */
#endif /* Brcl_BUILD_CORE */
#endif /* HAVE_DECLSPEC */
#endif /* Brcl_ENABLE_SHARED */

/* If no external linkage macros defined by now, create defaults */
#ifndef PyAPI_FUNC
#define PyAPI_FUNC(RTYPE) RTYPE
#endif
#ifndef PyAPI_DATA
#define PyAPI_DATA(RTYPE) extern RTYPE
#endif
#ifndef PyMODINIT_FUNC
#if defined(__cplusplus)
#define PyMODINIT_FUNC extern "C" void
#else /* __cplusplus */
#define PyMODINIT_FUNC void
#endif /* __cplusplus */
#endif

/* Deprecated DL_IMPORT and DL_EXPORT macros */
#if defined(Brcl_ENABLE_SHARED) && defined(HAVE_DECLSPEC_DLL)
#if defined(Brcl_BUILD_CORE)
#define DL_IMPORT(RTYPE) __declspec(dllexport) RTYPE
#define DL_EXPORT(RTYPE) __declspec(dllexport) RTYPE
#else
#define DL_IMPORT(RTYPE) __declspec(dllimport) RTYPE
#define DL_EXPORT(RTYPE) __declspec(dllexport) RTYPE
#endif
#endif
#ifndef DL_EXPORT
#define DL_EXPORT(RTYPE) RTYPE
#endif
#ifndef DL_IMPORT
#define DL_IMPORT(RTYPE) RTYPE
#endif
/* End of deprecated DL_* macros */

/* If the fd manipulation macros aren't defined,
   here is a set that should do the job */

#if 0 /* disabled and probably obsolete */

#ifndef FD_SETSIZE
#define FD_SETSIZE 256
#endif

#ifndef FD_SET

typedef long fd_mask;

#define NFDBITS (sizeof(fd_mask) * NBBY) /* bits per mask */
#ifndef howmany
#define howmany(x, y) (((x) + ((y)-1)) / (y))
#endif /* howmany */

typedef struct fd_set {
    fd_mask     fds_bits[howmany(FD_SETSIZE, NFDBITS)];
} fd_set;

#define FD_SET(n, p) ((p)->fds_bits[(n) / NFDBITS] |= (1 << ((n) % NFDBITS)))
#define FD_CLR(n, p) ((p)->fds_bits[(n) / NFDBITS] &= ~(1 << ((n) % NFDBITS)))
#define FD_ISSET(n, p) ((p)->fds_bits[(n) / NFDBITS] & (1 << ((n) % NFDBITS)))
#define FD_ZERO(p) memset((char *)(p), '\0', sizeof(*(p)))

#endif /* FD_SET */

#endif /* fd manipulation macros */

/* limits.h constants that may be missing */

#ifndef INT_MAX
#define INT_MAX 2147483647
#endif

#ifndef LONG_MAX
#if SIZEOF_LONG == 4
#define LONG_MAX 0X7FFFFFFFL
#elif SIZEOF_LONG == 8
#define LONG_MAX 0X7FFFFFFFFFFFFFFFL
#else
#error "could not set LONG_MAX in pyport.h"
#endif
#endif

#ifndef LONG_MIN
#define LONG_MIN (-LONG_MAX - 1)
#endif

#ifndef LONG_BIT
#define LONG_BIT (8 * SIZEOF_LONG)
#endif

#if LONG_BIT != 8 * SIZEOF_LONG
/* 04-Oct-2000 LONG_BIT is apparently (mis)defined as 64 on some recent
 * 32-bit platforms using gcc.  We try to catch that here at compile-time
 * rather than waiting for integer multiplication to trigger bogus
 * overflows.
 */
#error "LONG_BIT definition appears wrong for platform (bad gcc/glibc config?)."
#endif

#ifdef __cplusplus
}
#endif

/*
 * Hide GCC attributes from compilers that don't support them.
 */
#if (!defined(__GNUC__) || __GNUC__ < 2 ||     \
     (__GNUC__ == 2 && __GNUC_MINOR__ < 7)) && \
    !defined(RISCOS)
#define Brcl_GCC_ATTRIBUTE(x)
#else
#define Brcl_GCC_ATTRIBUTE(x) __attribute__(x)
#endif

/*
 * Add PyArg_ParseTuple format where available.
 */
#ifdef HAVE_ATTRIBUTE_FORMAT_PARSETUPLE
#define Brcl_FORMAT_PARSETUPLE(func, p1, p2) __attribute__((format(func, p1, p2)))
#else
#define Brcl_FORMAT_PARSETUPLE(func, p1, p2)
#endif

/*
 * Specify alignment on compilers that support it.
 */
#if defined(__GNUC__) && __GNUC__ >= 3
#define Brcl_ALIGNED(x) __attribute__((aligned(x)))
#else
#define Brcl_ALIGNED(x)
#endif

/* Eliminate end-of-loop code not reached warnings from SunPro C
 * when using do{...}while(0) macros
 */
#ifdef __SUNPRO_C
#pragma error_messages(off, E_END_OF_LOOP_CODE_NOT_REACHED)
#endif

/*
 * Older Microsoft compilers don't support the C99 long long literal suffixes,
 * so these will be defined in PC/pyconfig.h for those compilers.
 */
#ifndef Brcl_LL
#define Brcl_LL(x) x##LL
#endif

#ifndef Brcl_ULL
#define Brcl_ULL(x) Brcl_LL(x##U)
#endif

#endif /* Brcl_BRCLPORT_H */
