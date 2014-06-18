/*
 * Copyright (c) 2003 Hewlett-Packard Development Company, L.P.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include "../read_ordered.h"

#ifndef AO_ASSUME_WINDOWS98
  /* CAS is always available */
# define AO_ASSUME_WINDOWS98
#endif
#include "common32_defs.h"
/* FIXME: Do _InterlockedOps really have a full memory barrier?         */
/* (MSDN WinCE docs say nothing about it.)                              */

#if _M_ARM >= 6
/* ARMv6 is the first architecture providing support for simple LL/SC.  */

#include "../standard_ao_double_t.h"

/* If only a single processor is used, we can define AO_UNIPROCESSOR    */
/* and do not need to access CP15 for ensuring a DMB at all.            */
#ifdef AO_UNIPROCESSOR
  AO_INLINE void AO_nop_full(void) {}
# define AO_HAVE_nop_full
#else
/* AO_nop_full() is emulated using AO_test_and_set_full().              */
#endif

#include "../test_and_set_t_is_ao_t.h"
/* AO_test_and_set() is emulated using CAS.                             */

AO_INLINE AO_t
AO_load(const volatile AO_t *addr)
{
  /* Cast away the volatile in case it adds fence semantics */
  return (*(const AO_t *)addr);
}
#define AO_HAVE_load

AO_INLINE void
AO_store_full(volatile AO_t *addr, AO_t value)
{
  /* Emulate atomic store using CAS.    */
  AO_t old = AO_load(addr);
  AO_t current;
# ifdef AO_OLD_STYLE_INTERLOCKED_COMPARE_EXCHANGE
    while ((current = (AO_t)_InterlockedCompareExchange(
                                (PVOID AO_INTERLOCKED_VOLATILE *)addr,
                                (PVOID)value, (PVOID)old)) != old)
      old = current;
# else
    while ((current = (AO_t)_InterlockedCompareExchange(
                                (LONG AO_INTERLOCKED_VOLATILE *)addr,
                                (LONG)value, (LONG)old)) != old)
      old = current;
# endif
}
#define AO_HAVE_store_full

/* FIXME: implement AO_compare_double_and_swap_double() */

#else /* _M_ARM < 6 */

/* Some slide set, if it has been red correctly, claims that Loads      */
/* followed by either a Load or a Store are ordered, but nothing        */
/* else is. It appears that SWP is the only simple memory barrier.      */
#include "../all_atomic_load_store.h"

#include "../test_and_set_t_is_ao_t.h"
/* AO_test_and_set_full() is emulated using CAS.                        */

#endif /* _M_ARM < 6 */
