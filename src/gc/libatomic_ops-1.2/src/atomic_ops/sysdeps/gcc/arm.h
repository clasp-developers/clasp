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
 */

#include "../read_ordered.h"

#include "../test_and_set_t_is_ao_t.h" /* Probably suboptimal */

/* NEC LE-IT: ARMv6 is the first architecture providing support for simple LL/SC
 * A data memory barrier must be raised via CP15 command (see documentation).	
 * 																				
 * ARMv7 is compatible to ARMv6 but has a simpler command for issuing a 		
 * memory barrier (DMB). Raising it via CP15 should still work as told me by the
 * support engineers. If it turns out to be much quicker than we should implement
 * custom code for ARMv7 using the asm { dmb } command.														
 *
 * If only a single processor is used, we can define AO_UNIPROCESSOR
 * and do not need to access CP15 for ensuring a DMB  
*/

/* NEC LE-IT: gcc has no way to easily check the arm architecture
 * but defines only one of __ARM_ARCH_x__ to be true                    */
#if defined(__ARM_ARCH_6__) || defined(__ARM_ARCH_6J__) \
        || defined(__ARM_ARCH_6K__) || defined(__ARM_ARCH_6ZK__) \
        || defined(__ARM_ARCH_7__) || defined(__ARM_ARCH_7A__) \
        || defined(__ARM_ARCH_7M__) || defined(__ARM_ARCH_7R__)

#include "../standard_ao_double_t.h"

AO_INLINE void
AO_nop_full(void)
{
#ifndef AO_UNIPROCESSOR
	/* issue an data memory barrier (keeps ordering of memory transactions 	*/
	/* before and after this operation)					*/
	unsigned int dest=0;
	__asm__ __volatile__("mcr p15,0,%0,c7,c10,5" :"=&r"(dest) : : "memory");
#endif
}

#define AO_HAVE_nop_full

/* NEC LE-IT: AO_t load is simple reading */
AO_INLINE AO_t
AO_load(const volatile AO_t *addr)
{
  /* Cast away the volatile for architectures like IA64 where   */
  /* volatile adds barrier semantics.                           */
  return (*(const AO_t *)addr);
}
#define AO_HAVE_load

/* NEC LE-IT: atomic "store" - according to ARM documentation this is
 * the only safe way to set variables also used in LL/SC environment.
 * A direct write won't be recognized by the LL/SC construct on the _same_ CPU.
 * Support engineers response for behaviour of ARMv6:
 * 
   Core1        Core2          SUCCESS
   ===================================
   LDREX(x)
   STREX(x)                    Yes
   -----------------------------------
   LDREX(x)
                STR(x)
   STREX(x)                    No
   -----------------------------------
   LDREX(x)
   STR(x)
   STREX(x)                    Yes
   -----------------------------------
   
 * ARMv7 behaves similar, see documentation CortexA8 TRM, point 8.5  
 *
 * HB: I think this is only a problem if interrupt handlers do not clear
 * the reservation, as they almost certainly should.  Probably change this back
 * in a while?
*/
AO_INLINE void AO_store(volatile AO_t *addr, AO_t value)
{
        AO_t    flag;

        __asm__ __volatile__("@AO_store\n"
"1:     ldrex   %0, [%2]\n"
"       strex   %0, %3, [%2]\n"
"       teq     %0, #0\n"
"       bne     1b"
        : "=&r"(flag), "+m"(*addr)
        : "r" (addr), "r"(value)
        : "cc");
}
#define AO_HAVE_store

/* NEC LE-IT: replace the SWAP as recommended by ARM:

   "Applies to: ARM11 Cores
	Though the SWP instruction will still work with ARM V6 cores, it is
	recommended	to use the new V6 synchronization instructions. The SWP
	instruction produces ‘locked’ read and write accesses which are atomic,
	i.e. another operation cannot be done between these locked accesses which
	ties up external bus (AHB,AXI) bandwidth and can increase worst case 
	interrupt latencies. LDREX,STREX are more flexible, other instructions can
	be done between the LDREX and STREX accesses. 
   "
*/
AO_INLINE AO_TS_t
AO_test_and_set(volatile AO_TS_t *addr) {

        AO_TS_t oldval;
        unsigned long flag;

        __asm__ __volatile__("@AO_test_and_set\n"
"1:     ldrex   %0, [%3]\n"
"       strex   %1, %4, [%3]\n"
"       teq             %1, #0\n"
"       bne             1b\n"
        : "=&r"(oldval),"=&r"(flag), "+m"(*addr)
        : "r"(addr), "r"(1)
        : "cc");

        return oldval;
}

#define AO_HAVE_test_and_set

/* NEC LE-IT: fetch and add for ARMv6 */
AO_INLINE AO_t
AO_fetch_and_add(volatile AO_t *p, AO_t incr)
{
        unsigned long flag,tmp;
        AO_t result;

        __asm__ __volatile__("@AO_fetch_and_add\n"
"1:     ldrex   %0, [%5]\n"                     /* get original         */
"       add     %2, %0, %4\n"           /* sum up in incr       */
"       strex   %1, %2, [%5]\n"         /* store them           */
"       teq             %1, #0\n"
"       bne             1b\n"
        : "=&r"(result),"=&r"(flag),"=&r"(tmp),"+m"(*p) /* 0..3 */
        : "r"(incr), "r"(p)                                                             /* 4..5 */
        : "cc");

        return result;
}

#define AO_HAVE_fetch_and_add

/* NEC LE-IT: fetch and add1 for ARMv6 */
AO_INLINE AO_t
AO_fetch_and_add1(volatile AO_t *p)
{
        unsigned long flag,tmp;
        AO_t result;

        __asm__ __volatile__("@AO_fetch_and_add1\n"
"1:     ldrex   %0, [%4]\n"                     /* get original   */
"       add     %1, %0, #1\n"           /* increment */
"       strex   %2, %1, [%4]\n"         /* store them */
"       teq             %2, #0\n"
"       bne             1b\n"
        : "=&r"(result), "=&r"(tmp), "=&r"(flag), "+m"(*p)
        : "r"(p)
        : "cc");

        return result;
}

#define AO_HAVE_fetch_and_add1

/* NEC LE-IT: fetch and sub for ARMv6 */
AO_INLINE AO_t
AO_fetch_and_sub1(volatile AO_t *p)
{
        unsigned long flag,tmp;
        AO_t result;

        __asm__ __volatile__("@AO_fetch_and_sub1\n"
"1:     ldrex   %0, [%4]\n"                     /* get original   */
"       sub     %1, %0, #1\n"           /* decrement */
"       strex   %2, %1, [%4]\n"         /* store them */
"       teq             %2, #0\n"
"       bne             1b\n"
        : "=&r"(result), "=&r"(tmp), "=&r"(flag), "+m"(*p)
        : "r"(p)
        : "cc");

        return result;
}

#define AO_HAVE_fetch_and_sub1

/* NEC LE-IT: compare and swap */
/* Returns nonzero if the comparison succeeded. */
AO_INLINE int
AO_compare_and_swap(volatile AO_t *addr,
                                AO_t old_val, AO_t new_val)
{
         AO_t result,tmp;

        __asm__ __volatile__("@ AO_compare_and_swap\n"
"1:     mov             %0, #2\n"                       /* store a flag */
"       ldrex   %1, [%3]\n"                     /* get original */
"       teq             %1, %4\n"                       /* see if match */
"       strexeq %0, %5, [%3]\n"         /* store new one if matched */
"       teq             %0, #1\n"
"       beq             1b\n"                           /* if update failed, repeat */
        : "=&r"(result), "=&r"(tmp), "+m"(*addr)
        : "r"(addr), "r"(old_val), "r"(new_val)
        : "cc");

        return !(result&2);                     /* if succeded, return 1, else 0 */
}
#define AO_HAVE_compare_and_swap

AO_INLINE int
AO_compare_double_and_swap_double(volatile AO_double_t *addr,
                                                          AO_t old_val1, AO_t old_val2,
                                                          AO_t new_val1, AO_t new_val2)
{
        double_ptr_storage old_val = ((double_ptr_storage)old_val2 << 32) | old_val1;
        double_ptr_storage new_val = ((double_ptr_storage)new_val2 << 32) | new_val1;

    double_ptr_storage tmp;
        int result;

        while(1) {
                __asm__ __volatile__("@ AO_compare_and_swap_double\n"
                "       ldrexd  %0, [%1]\n"                     /* get original to r1&r2*/
                        : "=&r"(tmp)
                        : "r"(addr)
                        : "cc");
                if(tmp != old_val)      return 0;
                __asm__ __volatile__(
                "       strexd  %0, %2, [%3]\n" /* store new one if matched */
                        : "=&r"(result),"+m"(*addr)
                        : "r"(new_val), "r"(addr)
                        : "cc");
                if(!result)     return 1;
        }
}

#define AO_HAVE_compare_double_and_swap_double

#else
/* pre ARMv6 architecures ... */
 
/* I found a slide set that, if I read it correctly, claims that	*/
/* Loads followed by either a Load or Store are ordered, but nothing	*/
/* else is.								*/
/* It appears that SWP is the only simple memory barrier.		*/
#include "../all_atomic_load_store.h"

AO_INLINE AO_TS_VAL_t
AO_test_and_set_full(volatile AO_TS_t *addr) {
  AO_TS_VAL_t oldval;
  /* SWP on ARM is very similar to XCHG on x86. 		*/
  /* The first operand is the result, the second the value	*/
  /* to be stored.  Both registers must be different from addr.	*/
  /* Make the address operand an early clobber output so it     */
  /* doesn't overlap with the other operands.  The early clobber*/
  /* on oldval is neccessary to prevent the compiler allocating */
  /* them to the same register if they are both unused.  	*/
  __asm__ __volatile__("swp %0, %2, [%3]"
                        : "=&r"(oldval), "=&r"(addr)
                        : "r"(1), "1"(addr)
                        : "memory");
  return oldval;
}

#define AO_HAVE_test_and_set_full

#endif // __ARM_ARCH_x
