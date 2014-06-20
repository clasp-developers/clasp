/* mpq_mul_2exp, mpq_div_2exp - multiply or divide by 2^N */

/*
Copyright 2000, 2002 Free Software Foundation, Inc.

This file is part of the GNU MP Library.

The GNU MP Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

The GNU MP Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the GNU MP Library.  If not, see http://www.gnu.org/licenses/.  */

#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"


/* The multiplier/divisor "n", representing 2^n, is applied by right shifting
   "r" until it's odd (if it isn't already), and left shifting "l" for the
   rest. */

static void
mord_2exp (mpz_ptr ldst, mpz_ptr rdst, mpz_srcptr lsrc, mpz_srcptr rsrc,
           mp_bitcnt_t n)
{
  mp_size_t  rsrc_size = SIZ(rsrc);
  mp_size_t  len = ABS (rsrc_size);
  mp_ptr     rsrc_ptr = PTR(rsrc);
  mp_ptr     p, rdst_ptr;
  mp_limb_t  plow;

  p = rsrc_ptr;
  plow = *p;
  while (n >= GMP_NUMB_BITS && plow == 0)
    {
      n -= GMP_NUMB_BITS;
      p++;
      plow = *p;
    }

  /* no realloc here if rsrc==rdst, so p and rsrc_ptr remain valid */
  len -= (p - rsrc_ptr);
  MPZ_REALLOC (rdst, len);
  rdst_ptr = PTR(rdst);

  if ((plow & 1) || n == 0)
    {
      /* need DECR when src==dst */
      if (p != rdst_ptr)
        MPN_COPY_DECR (rdst_ptr, p, len);
    }
  else
    {
      unsigned long  shift;
      if (plow == 0)
        shift = n;
      else
        {
          count_trailing_zeros (shift, plow);
          shift = MIN (shift, n);
        }
      mpn_rshift (rdst_ptr, p, len, shift);
      len -= (rdst_ptr[len-1] == 0);
      n -= shift;
    }
  SIZ(rdst) = (rsrc_size >= 0) ? len : -len;

  if (n)
    mpz_mul_2exp (ldst, lsrc, n);
  else if (ldst != lsrc)
    mpz_set (ldst, lsrc);
}


void
mpq_mul_2exp (mpq_ptr dst, mpq_srcptr src, mp_bitcnt_t n)
{
  mord_2exp (mpq_numref (dst), mpq_denref (dst),
             mpq_numref (src), mpq_denref (src), n);
}

void
mpq_div_2exp (mpq_ptr dst, mpq_srcptr src, mp_bitcnt_t n)
{
  if (SIZ (mpq_numref(src)) == 0)
    {
      dst->_mp_num._mp_size = 0;
      dst->_mp_den._mp_size = 1;
      dst->_mp_den._mp_d[0] = 1;
      return;
    }

  mord_2exp (mpq_denref (dst), mpq_numref (dst),
             mpq_denref (src), mpq_numref (src), n);
}
