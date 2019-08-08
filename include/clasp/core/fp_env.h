/*
Utilities for dealing with the FPU environment.
In Lisp we want floating point problems (overflow, etc) to be indicated
by conditions being signaled.
On some (many?) systems, including x86-64 POSIX (our only one right now),
we have an FPU that can be configured to trigger a hardware interrupt in
these situations, and an OS that receives the hardware interrupt and
in turn triggers a software interrupt to Lisp - e.g., SIGFPE.
Unfortunately, C and POSIX do not standardize any of this trap behavior,
so it's a bit of a mess.
TODO: Rounding mode stuff should go here too.
*/

#ifndef _core_fp_env_H
#define _core_fp_env_H

#include <cfenv>
//#ifdef _TARGET_OS_DARWIN
#include <xmmintrin.h>
//#endif

/* Our model for traps is as follows:
 * The FPU has a bit flags register.
 * Each kind of exception has some flag, and we can bitwise OR
 * flags together.
 * If a flag is set in the register, that exception is "masked"-
 * the FPU does NOT interrupt execution if it's run into, and
 * instead just returns a NaN or whatever.
 * clasp_feenableexcept UNmasks the given exceptions; exceptions
 * it doesn't flag are masked.
 */

// FIXME: Add arch-specific flags (mostly denormals I think)

// These are the flags for FPU traps.
// CLASP_FPT_SUPPORT indicates we can deal with traps at all.
#ifdef _TARGET_OS_LINUX
#define CLASP_FPT_SUPPORT
// FE_etc. are all required to exist by C++11.
#define CLASP_FPT_DIVBYZERO FE_DIVBYZERO
#define CLASP_FPT_INEXACT   FE_INEXACT
#define CLASP_FPT_INVALID   FE_INVALID
#define CLASP_FPT_OVERFLOW  FE_OVERFLOW
#define CLASP_FPT_UNDERFLOW FE_UNDERFLOW
#define clasp_feenablexcept(flags) feenableexcept(flags)
#define clasp_fegetexcept() fegetexcept()
//#elif defined(_TARGET_OS_DARWIN) // FIXME: indicate x86-64 only
#else
#define CLASP_FPT_SUPPORT
// On Darwin all floating point operations use SSE,
// so we use those flags.
// Semantics are that if a mask bit is 1, the default,
// the exception is masked.
#define CLASP_FPT_DIVBYZERO _MM_MASK_DIV_ZERO
#define CLASP_FPT_INEXACT   _MM_MASK_INEXACT
#define CLASP_FPT_INVALID   _MM_MASK_INVALID
#define CLASP_FPT_OVERFLOW  _MM_MASK_OVERFLOW
#define CLASP_FPT_UNDERFLOW _MM_MASK_UNDERFLOW
// NOTE: As documented by Intel, the _MM_MASK_MASK& shouldn't
// be necessary, but it seems to be - without it we can touch
// bits we're not allowed to, triggering a segfault.
#define clasp_feenableexcept(flags)\
  _MM_SET_EXCEPTION_MASK(_MM_MASK_MASK & ~(flags))
#define clasp_fegetexcept() _MM_GET_EXCEPTION_MASK()
#endif // no support

#ifdef CLASP_FPT_SUPPORT
// This is the initial disposition we want for Lisp.
#define CLASP_FPT_INIT_EXCEPT                 \
  (CLASP_FPT_DIVBYZERO | CLASP_FPT_INVALID    \
   | CLASP_FPT_OVERFLOW | CLASP_FPT_UNDERFLOW)
#else
#error "No FPT support"
#endif

// Function called from image initialization to set the flags to
// CLASP_FPT_INIT_EXCEPT
void init_float_traps(void);

#endif /* guard */
