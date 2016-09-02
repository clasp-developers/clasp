/*
    File: math_fenv.h
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
#ifndef core_math_fenv_H
#define core_math_fenv_H

/* -*- mode: c; c-basic-offset: 4 -*- */
/*
    math_fenv.h -- inlined versions of fenv.h
*/
/*
    Copyright (c) 2010, Juan Jose Garcia Ripoll.
    Copyright (c) 2013, Christian E. Schafmeister

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

namespace core {

/*
 * ECL admits two ways to process floating point errors
 *   - Based on hardware exceptions
 *   - Based on explicit checks
 *
 * In the hardware exception model we simply expect the floating point
 * unit to complain about a computation. In order for this to happen
 * we may need two things:
 *
 *   - Activate explicitely signaling of exceptions
 *   - Insert explicit checks for exceptions
 *
 * The first taks is achieved using feenableexcept() or an equivalent
 * function. The second task is only needed on some platforms where
 * exceptions are activated by one floating point computation but are
 * only signaled with the _next_ floating point instruction (Read x86
 * processors)
 *
 * The second model is more portable and safer and it is based on
 * using the C99 routines in fenv.h or equivalent functions. In this
 * model hardware exceptions are never triggered and instead we
 * surround the computation with explicit checks for problems.
 */

#if defined(HAVE_FEENABLEEXCEPT) && !defined(_GNU_SOURCE)
#define _GNU_SOURCE
#endif
#ifdef HAVE_FENV_H
#include <fenv.h>
#endif
#if defined(CLASP_MS_WINDOWS_HOST)
#include <ecl/impl/math_fenv_msvc.h>
#endif

#ifdef HAVE_FENV_H
#define CLASP_WITHOUT_FPE_BEGIN \
  do {                          \
    fenv_t env;                 \
    feholdexcept(&env);
#define CLASP_WITHOUT_FPE_END \
  }                           \
  while (0)
#else
#define FE_INVALID 1
#define FE_DIVBYZERO 2
#define FE_INEXACT 0
#define FE_OVERFLOW 3
#define FE_UNDERFLOW 0
#define CLASP_WITHOUT_FPE_BEGIN
#define CLASP_WITHOUT_FPE_END
#define feclearexcept(x)
#endif /* !HAVE_FENV_H */

#if defined(HAVE_FENV_H) && !defined(HAVE_FEENABLEEXCEPT) && !defined(CLASP_AVOID_FPE_H)
#define CLASP_USED_EXCEPTIONS (FE_DIVBYZERO | FE_INVALID | FE_OVERFLOW | FE_UNDERFLOW)
#define CLASP_MATHERR_CLEAR feclearexcept(FE_ALL_EXCEPT)
#define CLASP_MATHERR_TEST                          \
  do {                                              \
    int bits = fetestexcept(CLASP_USED_EXCEPTIONS); \
    unlikely_if(bits) ecl_deliver_fpe(bits);        \
  } while (0)
#else
#define CLASP_MATHERR_CLEAR
#define CLASP_MATHERR_TEST
#endif

#if 0 // __APPLE__ doesn't need feclearexcept or fetestexcept
#if defined(__APPLE__) && defined(__amd64__)
#define feclearexcept myfeclearexcept
static inline void myfeclearexcept(int flags)
{
    int aux;
    int f = ~(0x3d);
    __asm__ (
    "fnclex  \n\t"
    "stmxcsr %0\n\t"
    "andl    %1,%0\n\t"
    "ldmxcsr %0\n\t"
    : "=m"(aux) : "a"(f));
}
#define fetestexcept myfetestexcept
 static inline int myfetestexcept(gctools::Fixnum flags)
{
    gctools::Fixnum output = (flags & 0x3d);
    int sw;
    __asm__ (
    "fnstsw  %0\n\t"
    "movzwl  %0,%%eax\n\t"
    "stmxcsr %0\n\t"
    "orl     %0,%%eax\n\t"
    "and     %%rax,%1\n\t"
    : "=m"(sw), "=d"(output) : "d"(output) : "%rax");
    return output;
}
#endif /* __APPLE__ && __amd64__ */
#endif // 0  __APPLE__ doesn't need  fetestexcept  defined?

extern void ecl_deliver_fpe(int flags);
};
#endif
