/* mpn_set_str (mp_ptr res_ptr, const char *str, size_t str_len, int base) --
   Convert a STR_LEN long base BASE byte string pointed to by STR to a limb
   vector pointed to by RES_PTR.  Return the number of limbs in RES_PTR.

Copyright 1991, 1992, 1993, 1994, 1996, 2000, 2001, 2002 Free Software
Foundation, Inc.

This file is part of the GNU MP Library.

The GNU MP Library is free software; you can redistribute it and/or modify it
under the terms of the GNU Lesser General Public License as published by the
Free Software Foundation; either version 2.1 of the License, or (at your
option) any later version.

The GNU MP Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License
for more details.

You should have received a copy of the GNU Lesser General Public License along
with the GNU MP Library; see the file COPYING.LIB.  If not, write to the Free
Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA. */

#include "gmp.h"
#include "gmp-impl.h"

static mp_size_t convert_blocks __GMP_PROTO ((mp_ptr, const unsigned char *, size_t, int));

/* When to switch to sub-quadratic code.  This counts characters/digits in
   the input string, not limbs as most other *_THRESHOLD.  */
#ifndef SET_STR_THRESHOLD
#define SET_STR_THRESHOLD 4000
#endif

/* Don't define this to anything but 1 for now.  In order to make other values
   work well, either the convert_blocks function should be generalized to
   handle larger blocks than chars_per_limb, or the basecase code should be
   broken out of the main function.  Also note that this must be a power of
   2.  */
#ifndef SET_STR_BLOCK_SIZE
#define SET_STR_BLOCK_SIZE 1	/* Must be a power of 2. */
#endif


/* This check interferes with expression based values of SET_STR_THRESHOLD
   used for tuning and measuring.
#if SET_STR_BLOCK_SIZE >= SET_STR_THRESHOLD
These values are silly.
The sub-quadratic code would recurse to itself.
#endif
*/

mp_size_t
mpn_set_str (mp_ptr rp, const unsigned char *str, size_t str_len, int base)
{
  mp_size_t size;
  mp_limb_t big_base;
  int chars_per_limb;
  mp_limb_t res_digit;

  ASSERT (base >= 2);
  ASSERT (base < numberof (__mp_bases));
  ASSERT (str_len >= 1);

  big_base = __mp_bases[base].big_base;
  chars_per_limb = __mp_bases[base].chars_per_limb;

  size = 0;

  if (POW2_P (base))
    {
      /* The base is a power of 2.  Read the input string from least to most
	 significant character/digit.  */

      const unsigned char *s;
      int next_bitpos;
      int bits_per_indigit = big_base;

      res_digit = 0;
      next_bitpos = 0;

      for (s = str + str_len - 1; s >= str; s--)
	{
	  int inp_digit = *s;

	  res_digit |= ((mp_limb_t) inp_digit << next_bitpos) & GMP_NUMB_MASK;
	  next_bitpos += bits_per_indigit;
	  if (next_bitpos >= GMP_NUMB_BITS)
	    {
	      rp[size++] = res_digit;
	      next_bitpos -= GMP_NUMB_BITS;
	      res_digit = inp_digit >> (bits_per_indigit - next_bitpos);
	    }
	}

      if (res_digit != 0)
	rp[size++] = res_digit;
      return size;
    }
  else
    {
      /* General case.  The base is not a power of 2.  */

      if (str_len < SET_STR_THRESHOLD)
	{
	  size_t i;
	  int j;
	  mp_limb_t cy_limb;

	  for (i = chars_per_limb; i < str_len; i += chars_per_limb)
	    {
	      res_digit = *str++;
	      if (base == 10)
		{ /* This is a common case.
		     Help the compiler to avoid multiplication.  */
		  for (j = MP_BASES_CHARS_PER_LIMB_10 - 1; j != 0; j--)
		    res_digit = res_digit * 10 + *str++;
		}
	      else
		{
		  for (j = chars_per_limb - 1; j != 0; j--)
		    res_digit = res_digit * base + *str++;
		}

	      if (size == 0)
		{
		  if (res_digit != 0)
		    {
		      rp[0] = res_digit;
		      size = 1;
		    }
		}
	      else
		{
#if HAVE_NATIVE_mpn_mul_1c
		  cy_limb = mpn_mul_1c (rp, rp, size, big_base, res_digit);
#else
		  cy_limb = mpn_mul_1 (rp, rp, size, big_base);
		  cy_limb += mpn_add_1 (rp, rp, size, res_digit);
#endif
		  if (cy_limb != 0)
		    rp[size++] = cy_limb;
		}
	    }

	  big_base = base;
	  res_digit = *str++;
	  if (base == 10)
	    { /* This is a common case.
		 Help the compiler to avoid multiplication.  */
	      for (j = str_len - (i - MP_BASES_CHARS_PER_LIMB_10) - 1; j > 0; j--)
		{
		  res_digit = res_digit * 10 + *str++;
		  big_base *= 10;
		}
	    }
	  else
	    {
	      for (j = str_len - (i - chars_per_limb) - 1; j > 0; j--)
		{
		  res_digit = res_digit * base + *str++;
		  big_base *= base;
		}
	    }

	  if (size == 0)
	    {
	      if (res_digit != 0)
		{
		  rp[0] = res_digit;
		  size = 1;
		}
	    }
	  else
	    {
#if HAVE_NATIVE_mpn_mul_1c
	      cy_limb = mpn_mul_1c (rp, rp, size, big_base, res_digit);
#else
	      cy_limb = mpn_mul_1 (rp, rp, size, big_base);
	      cy_limb += mpn_add_1 (rp, rp, size, res_digit);
#endif
	      if (cy_limb != 0)
		rp[size++] = cy_limb;
	    }
	  return size;
	}
      else
	{
	  /* Sub-quadratic code.  */

	  mp_ptr dp;
	  mp_size_t dsize;
	  mp_ptr xp, tp;
	  mp_size_t step;
	  mp_size_t i;
	  size_t alloc;
	  mp_size_t n;
	  mp_ptr pow_mem;

	  alloc = (str_len + chars_per_limb - 1) / chars_per_limb;
	  alloc = 2 * alloc;
	  dp = __GMP_ALLOCATE_FUNC_LIMBS (alloc);

#if SET_STR_BLOCK_SIZE == 1
	  dsize = convert_blocks (dp, str, str_len, base);
#else
	  {
	    const unsigned char *s;
	    mp_ptr ddp = dp;

	    s = str + str_len;
	    while (s - str >  SET_STR_BLOCK_SIZE * chars_per_limb)
	      {
		s -= SET_STR_BLOCK_SIZE * chars_per_limb;
		mpn_set_str (ddp, s, SET_STR_BLOCK_SIZE * chars_per_limb, base);
		ddp += SET_STR_BLOCK_SIZE;
	      }
	    ddp += mpn_set_str (ddp, str, s - str, base);
	    dsize = ddp - dp;
	  }
#endif

	  /* Allocate space for powers of big_base.  Could trim this in two
	     ways:
	     1. Only really need 2^ceil(log2(dsize)) bits for the largest
		power.
	     2. Only the variable to get the largest power needs that much
		memory.  The other variable needs half as much.  We need just
		figure out which of xp and tp will hold the last one.
	     Net space savings would be in the range 1/4 to 5/8 of current
	     allocation, depending on how close dsize is to the next greater
	     power of 2.  */
	  pow_mem = __GMP_ALLOCATE_FUNC_LIMBS (2 * alloc);
	  xp = pow_mem;
	  tp = pow_mem + alloc;

	  xp[0] = big_base;
	  n = 1;
	  step = 1;
#if SET_STR_BLOCK_SIZE != 1
	  for (i = SET_STR_BLOCK_SIZE; i > 1; i >>= 1)
	    {
	      mpn_sqr_n (tp, xp, n);
	      n = 2 * n;
	      n -= tp[n - 1] == 0;

	      step = 2 * step;
	      MP_PTR_SWAP (tp, xp);
	    }
#endif

	  /* Multiply every second limb block, each `step' limbs large by the
	     base power currently in xp[], then add this to the adjacent block.
	     We thereby convert from dsize blocks in base big_base, to dsize/2
	     blocks in base big_base^2, then to dsize/4 blocks in base
	     big_base^4, etc, etc.  */

	  if (step < dsize)
	    {
	      for (;;)
		{
		  for (i = 0; i < dsize - step; i += 2 * step)
		    {
		      mp_ptr bp = dp + i;
		      mp_size_t m = dsize - i - step;
		      mp_size_t hi;
		      if (n >= m)
			{
			  mpn_mul (tp, xp, n, bp + step, m);
			  mpn_add (bp, tp, n + m, bp, n);
			  hi = i + n + m;
			  dsize = hi;
			  dsize -= dp[dsize - 1] == 0;
			}
		      else
			{
			  mpn_mul_n (tp, xp, bp + step, n);
			  mpn_add (bp, tp, n + n, bp, n);
			}
		    }

		  step = 2 * step;
		  if (! (step < dsize))
		    break;

		  mpn_sqr_n (tp, xp, n);
		  n = 2 * n;
		  n -= tp[n - 1] == 0;
		  MP_PTR_SWAP (tp, xp);
		}
	    }

	  MPN_NORMALIZE (dp, dsize);
	  MPN_COPY (rp, dp, dsize);
	  __GMP_FREE_FUNC_LIMBS (pow_mem, 2 * alloc);
	  __GMP_FREE_FUNC_LIMBS (dp, alloc);
	  return dsize;
	}
    }
}

static mp_size_t
convert_blocks (mp_ptr dp, const unsigned char *str, size_t str_len, int base)
{
  int chars_per_limb;
  mp_size_t i;
  int j;
  int ds;
  mp_size_t dsize;
  mp_limb_t res_digit;

  chars_per_limb = __mp_bases[base].chars_per_limb;

  dsize = str_len / chars_per_limb;
  ds = str_len % chars_per_limb;

  if (ds != 0)
    {
      res_digit = *str++;
      for (j = ds - 1; j != 0; j--)
	res_digit = res_digit * base + *str++;
      dp[dsize] = res_digit;
    }

  if (base == 10)
    {
      for (i = dsize - 1; i >= 0; i--)
	{
	  res_digit = *str++;
	  for (j = MP_BASES_CHARS_PER_LIMB_10 - 1; j != 0; j--)
	    res_digit = res_digit * 10 + *str++;
	  dp[i] = res_digit;
	}
    }
  else
    {
      for (i = dsize - 1; i >= 0; i--)
	{
	  res_digit = *str++;
	  for (j = chars_per_limb - 1; j != 0; j--)
	    res_digit = res_digit * base + *str++;
	  dp[i] = res_digit;
	}
    }

  return dsize + (ds != 0);
}
