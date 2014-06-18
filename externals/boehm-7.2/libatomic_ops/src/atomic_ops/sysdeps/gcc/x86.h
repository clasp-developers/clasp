/*
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1996-1999 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 1999-2003 by Hewlett-Packard Company. All rights reserved.
 *
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose,  provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 *
 * Some of the machine specific code was borrowed from our GC distribution.
 */

/* The following really assume we have a 486 or better.  Unfortunately  */
/* gcc doesn't define a suitable feature test macro based on command    */
/* line options.                                                        */
/* We should perhaps test dynamically.                                  */

#include "../all_aligned_atomic_load_store.h"

/* Real X86 implementations, except for some old WinChips, appear       */
/* to enforce ordering between memory operations, EXCEPT that a later   */
/* read can pass earlier writes, presumably due to the visible          */
/* presence of store buffers.                                           */
/* We ignore both the WinChips, and the fact that the official specs    */
/* seem to be much weaker (and arguably too weak to be usable).         */

#include "../ordered_except_wr.h"

#include "../test_and_set_t_is_char.h"

#include "../standard_ao_double_t.h"

#if defined(AO_USE_PENTIUM4_INSTRS)
AO_INLINE void
AO_nop_full(void)
{
  __asm__ __volatile__("mfence" : : : "memory");
}
#define AO_HAVE_nop_full

#else

/* We could use the cpuid instruction.  But that seems to be slower     */
/* than the default implementation based on test_and_set_full.  Thus    */
/* we omit that bit of misinformation here.                             */

#endif

/* As far as we can tell, the lfence and sfence instructions are not    */
/* currently needed or useful for cached memory accesses.               */

/* Really only works for 486 and later */
AO_INLINE AO_t
AO_fetch_and_add_full (volatile AO_t *p, AO_t incr)
{
  AO_t result;

  __asm__ __volatile__ ("lock; xaddl %0, %1" :
                        "=r" (result), "=m" (*p) : "0" (incr), "m" (*p)
                        : "memory");
  return result;
}
#define AO_HAVE_fetch_and_add_full

AO_INLINE unsigned char
AO_char_fetch_and_add_full (volatile unsigned char *p, unsigned char incr)
{
  unsigned char result;

  __asm__ __volatile__ ("lock; xaddb %0, %1" :
                        "=q" (result), "=m" (*p) : "0" (incr), "m" (*p)
                        : "memory");
  return result;
}
#define AO_HAVE_char_fetch_and_add_full

AO_INLINE unsigned short
AO_short_fetch_and_add_full (volatile unsigned short *p, unsigned short incr)
{
  unsigned short result;

  __asm__ __volatile__ ("lock; xaddw %0, %1" :
                        "=r" (result), "=m" (*p) : "0" (incr), "m" (*p)
                        : "memory");
  return result;
}
#define AO_HAVE_short_fetch_and_add_full

/* Really only works for 486 and later */
AO_INLINE void
AO_or_full (volatile AO_t *p, AO_t incr)
{
  __asm__ __volatile__ ("lock; orl %1, %0" :
                        "=m" (*p) : "r" (incr), "m" (*p) : "memory");
}
#define AO_HAVE_or_full

AO_INLINE AO_TS_VAL_t
AO_test_and_set_full(volatile AO_TS_t *addr)
{
  unsigned char oldval;
  /* Note: the "xchg" instruction does not need a "lock" prefix */
  __asm__ __volatile__("xchgb %0, %1"
                : "=q"(oldval), "=m"(*addr)
                : "0"((unsigned char)0xff), "m"(*addr) : "memory");
  return (AO_TS_VAL_t)oldval;
}
#define AO_HAVE_test_and_set_full

/* Returns nonzero if the comparison succeeded. */
AO_INLINE int
AO_compare_and_swap_full(volatile AO_t *addr, AO_t old, AO_t new_val)
{
# ifdef AO_USE_SYNC_CAS_BUILTIN
    return (int)__sync_bool_compare_and_swap(addr, old, new_val);
# else
    char result;
    __asm__ __volatile__("lock; cmpxchgl %3, %0; setz %1"
                         : "=m" (*addr), "=a" (result)
                         : "m" (*addr), "r" (new_val), "a" (old) : "memory");
    return (int)result;
# endif
}
#define AO_HAVE_compare_and_swap_full

#if !defined(__x86_64__)

/* Returns nonzero if the comparison succeeded. */
/* Really requires at least a Pentium.          */
AO_INLINE int
AO_compare_double_and_swap_double_full(volatile AO_double_t *addr,
                                       AO_t old_val1, AO_t old_val2,
                                       AO_t new_val1, AO_t new_val2)
{
  char result;
# ifdef __PIC__
    AO_t saved_ebx;

    /* If PIC is turned on, we cannot use ebx as it is reserved for the */
    /* GOT pointer.  We should save and restore ebx.  The proposed      */
    /* solution is not so efficient as the older alternatives using     */
    /* push ebx or edi as new_val1 (w/o clobbering edi and temporary    */
    /* local variable usage) but it is more portable (it works even if  */
    /* ebx is not used as GOT pointer, and it works for the buggy GCC   */
    /* releases that incorrectly evaluate memory operands offset in the */
    /* inline assembly after push).                                     */
#   ifdef __OPTIMIZE__
      __asm__ __volatile__("mov %%ebx, %2\n\t" /* save ebx */
                           "lea %0, %%edi\n\t" /* in case addr is in ebx */
                           "mov %7, %%ebx\n\t" /* load new_val1 */
                           "lock; cmpxchg8b (%%edi)\n\t"
                           "mov %2, %%ebx\n\t" /* restore ebx */
                           "setz %1"
                        : "=m" (*addr), "=a" (result), "=m" (saved_ebx)
                        : "m" (*addr), "d" (old_val2), "a" (old_val1),
                          "c" (new_val2), "m" (new_val1)
                        : "%edi", "memory");
#   else
      /* A less-efficient code manually preserving edi if GCC invoked   */
      /* with -O0 option (otherwise it fails while finding a register   */
      /* in class 'GENERAL_REGS').                                      */
      AO_t saved_edi;
      __asm__ __volatile__("mov %%edi, %3\n\t" /* save edi */
                           "mov %%ebx, %2\n\t" /* save ebx */
                           "lea %0, %%edi\n\t" /* in case addr is in ebx */
                           "mov %8, %%ebx\n\t" /* load new_val1 */
                           "lock; cmpxchg8b (%%edi)\n\t"
                           "mov %2, %%ebx\n\t" /* restore ebx */
                           "mov %3, %%edi\n\t" /* restore edi */
                           "setz %1"
                        : "=m" (*addr), "=a" (result),
                          "=m" (saved_ebx), "=m" (saved_edi)
                        : "m" (*addr), "d" (old_val2), "a" (old_val1),
                          "c" (new_val2), "m" (new_val1) : "memory");
#   endif
# else
    /* For non-PIC mode, this operation could be simplified (and be     */
    /* faster) by using ebx as new_val1 (GCC would refuse to compile    */
    /* such code for PIC mode).                                         */
    __asm__ __volatile__("lock; cmpxchg8b %0; setz %1"
                       : "=m" (*addr), "=a" (result)
                       : "m" (*addr), "d" (old_val2), "a" (old_val1),
                         "c" (new_val2), "b" (new_val1) : "memory");
# endif
  return (int) result;
}
#define AO_HAVE_compare_double_and_swap_double_full

#else /* x86_64 && ILP32 */

  /* X32 has native support for 64-bit integer operations (AO_double_t  */
  /* is a 64-bit integer and we could use 64-bit cmpxchg).              */
  /* This primitive is used by compare_double_and_swap_double_full.     */
  AO_INLINE int
  AO_double_compare_and_swap_full(volatile AO_double_t *addr,
                                  AO_double_t old_val, AO_double_t new_val)
  {
    /* It is safe to use __sync CAS built-in here.      */
    return __sync_bool_compare_and_swap(&addr->AO_whole,
                                        old_val.AO_whole, new_val.AO_whole
                                        /* empty protection list */);
  }
# define AO_HAVE_double_compare_and_swap_full

  /* TODO: Remove if generalized.       */
  AO_INLINE int
  AO_compare_double_and_swap_double_full(volatile AO_double_t *addr,
                                         AO_t old_val1, AO_t old_val2,
                                         AO_t new_val1, AO_t new_val2)
  {
    AO_double_t old_w;
    AO_double_t new_w;
    old_w.AO_val1 = old_val1;
    old_w.AO_val2 = old_val2;
    new_w.AO_val1 = new_val1;
    new_w.AO_val2 = new_val2;
    return AO_double_compare_and_swap_full(addr, old_w, new_w);
  }
# define AO_HAVE_compare_double_and_swap_double_full

#endif /* x86_64 && ILP32 */

#include "../ao_t_is_int.h"
