/* Alternate implementations of modlimb_invert to compare speeds. */

/*
Copyright 2000, 2002 Free Software Foundation, Inc.

This file is part of the GNU MP Library.

The GNU MP Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or (at your
option) any later version.

The GNU MP Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
MA 02110-1301, USA.
*/

#include <stdio.h>
#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"
#include "speed.h"


/* Like the standard version in gmp-impl.h, but with the expressions using a
   "1-" form.  This has the same number of steps, but "1-" is on the
   dependent chain, whereas the "2*" in the standard version isn't.
   Depending on the CPU this should be the same or a touch slower.  */

#if BITS_PER_MP_LIMB <= 32
#define modlimb_invert_mul1(inv,n)                              \
  do {                                                          \
    mp_limb_t  __n = (n);                                       \
    mp_limb_t  __inv;                                           \
    ASSERT ((__n & 1) == 1);                                    \
    __inv = modlimb_invert_table[(__n&0xFF)/2]; /*  8 */        \
    __inv = (1 - __n * __inv) * __inv + __inv;  /* 16 */        \
    __inv = (1 - __n * __inv) * __inv + __inv;  /* 32 */        \
    ASSERT (__inv * __n == 1);                                  \
    (inv) = __inv;                                              \
  } while (0)
#endif

#if BITS_PER_MP_LIMB > 32 && BITS_PER_MP_LIMB <= 64
#define modlimb_invert_mul1(inv,n)                              \
  do {                                                          \
    mp_limb_t  __n = (n);                                       \
    mp_limb_t  __inv;                                           \
    ASSERT ((__n & 1) == 1);                                    \
    __inv = modlimb_invert_table[(__n&0xFF)/2]; /*  8 */        \
    __inv = (1 - __n * __inv) * __inv + __inv;  /* 16 */        \
    __inv = (1 - __n * __inv) * __inv + __inv;  /* 32 */        \
    __inv = (1 - __n * __inv) * __inv + __inv;  /* 64 */        \
    ASSERT (__inv * __n == 1);                                  \
    (inv) = __inv;                                              \
  } while (0)
#endif


/* The loop based version used in GMP 3.0 and earlier.  Usually slower than
   multiplying, due to the number of steps that must be performed.  Much
   slower when the processor has a good multiply.  */

#define modlimb_invert_loop(inv,n)              \
  do {                                          \
    mp_limb_t  __v = (n);                       \
    mp_limb_t  __v_orig = __v;                  \
    mp_limb_t  __make_zero = 1;                 \
    mp_limb_t  __two_i = 1;                     \
    mp_limb_t  __v_inv = 0;                     \
                                                \
    ASSERT ((__v & 1) == 1);                    \
                                                \
    do                                          \
      {                                         \
        while ((__two_i & __make_zero) == 0)    \
          __two_i <<= 1, __v <<= 1;             \
        __v_inv += __two_i;                     \
        __make_zero -= __v;                     \
      }                                         \
    while (__make_zero);                        \
                                                \
    ASSERT (__v_orig * __v_inv == 1);           \
    (inv) = __v_inv;                            \
  } while (0)


/* Another loop based version with conditionals, but doing a fixed number of
   steps. */

#define modlimb_invert_cond(inv,n)              \
  do {                                          \
    mp_limb_t  __n = (n);                       \
    mp_limb_t  __rem = (1 - __n) >> 1;          \
    mp_limb_t  __inv = GMP_LIMB_HIGHBIT;       \
    int        __count;                         \
                                                \
    ASSERT ((__n & 1) == 1);                    \
                                                \
    __count = BITS_PER_MP_LIMB-1;               \
    do                                          \
      {                                         \
        __inv >>= 1;                            \
        if (__rem & 1)                          \
          {                                     \
            __inv |= GMP_LIMB_HIGHBIT;         \
            __rem -= __n;                       \
          }                                     \
        __rem >>= 1;                            \
      }                                         \
    while (-- __count);                         \
                                                \
    ASSERT (__inv * __n == 1);                  \
    (inv) = __inv;                              \
  } while (0)


/* Another loop based bitwise version, but purely arithmetic, no
   conditionals. */

#define modlimb_invert_arith(inv,n)                                     \
  do {                                                                  \
    mp_limb_t  __n = (n);                                               \
    mp_limb_t  __rem = (1 - __n) >> 1;                                  \
    mp_limb_t  __inv = GMP_LIMB_HIGHBIT;                               \
    mp_limb_t  __lowbit;                                                \
    int        __count;                                                 \
                                                                        \
    ASSERT ((__n & 1) == 1);                                            \
                                                                        \
    __count = BITS_PER_MP_LIMB-1;                                       \
    do                                                                  \
      {                                                                 \
        __lowbit = __rem & 1;                                           \
        __inv = (__inv >> 1) | (__lowbit << (BITS_PER_MP_LIMB-1));      \
        __rem = (__rem - (__n & -__lowbit)) >> 1;                       \
      }                                                                 \
    while (-- __count);                                                 \
                                                                        \
    ASSERT (__inv * __n == 1);                                          \
    (inv) = __inv;                                                      \
  } while (0)


double
speed_modlimb_invert_mul1 (struct speed_params *s)
{
  SPEED_ROUTINE_MODLIMB_INVERT (modlimb_invert_mul1);
}
double
speed_modlimb_invert_loop (struct speed_params *s)
{
  SPEED_ROUTINE_MODLIMB_INVERT (modlimb_invert_loop);
}
double
speed_modlimb_invert_cond (struct speed_params *s)
{
  SPEED_ROUTINE_MODLIMB_INVERT (modlimb_invert_cond);
}
double
speed_modlimb_invert_arith (struct speed_params *s)
{
  SPEED_ROUTINE_MODLIMB_INVERT (modlimb_invert_arith);
}
