/* mpz_fac_ui(result, n) -- Set RESULT to N!.

Copyright 1991, 1993, 1994, 1995, 2000, 2001, 2002, 2003 Free Software
Foundation, Inc.

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

#include "fac_ui.h"


static void odd_product __GMP_PROTO ((unsigned long, unsigned long, mpz_t *));
static void ap_product_small __GMP_PROTO ((mpz_t, mp_limb_t, mp_limb_t, unsigned long, unsigned long));


/* must be >=2	*/
#define APCONST	5

/* for single non-zero limb */
#define MPZ_SET_1_NZ(z,n)	\
  do {				\
    mpz_ptr  __z = (z);		\
    ASSERT ((n) != 0);		\
    PTR(__z)[0] = (n);		\
    SIZ(__z) = 1;		\
  } while (0)

/* for src>0 and n>0 */
#define MPZ_MUL_1_POS(dst,src,n)			\
  do {							\
    mpz_ptr    __dst = (dst);				\
    mpz_srcptr __src = (src);				\
    mp_size_t  __size = SIZ(__src);			\
    mp_ptr     __dst_p;					\
    mp_limb_t  __c;					\
							\
    ASSERT (__size > 0);				\
    ASSERT ((n) != 0);					\
							\
    MPZ_REALLOC (__dst, __size+1);			\
    __dst_p = PTR(__dst);				\
							\
    __c = mpn_mul_1 (__dst_p, PTR(__src), __size, n);	\
    __dst_p[__size] = __c;				\
    SIZ(__dst) = __size + (__c != 0);			\
  } while (0)


#if BITS_PER_ULONG == GMP_LIMB_BITS
#define BSWAP_ULONG(x,y)	BSWAP_LIMB(x,y)
#endif

/* We used to have a case here for limb==2*long, doing a BSWAP_LIMB followed
   by a shift down to get the high part.  But it provoked incorrect code
   from "HP aC++/ANSI C B3910B A.05.52 [Sep 05 2003]" in ILP32 mode.  This
   case would have been nice for gcc ia64 where BSWAP_LIMB is a mux1, but we
   can get that directly muxing a 4-byte ulong if it matters enough.  */

#if ! defined (BSWAP_ULONG)
#define BSWAP_ULONG(dst, src)						\
  do {									\
    unsigned long  __bswapl_src = (src);				\
    unsigned long  __bswapl_dst = 0;					\
    int	       __i;							\
    for (__i = 0; __i < sizeof(unsigned long); __i++)			\
      {									\
	__bswapl_dst = (__bswapl_dst << 8) | (__bswapl_src & 0xFF);	\
	__bswapl_src >>= 8;						\
      }									\
    (dst) = __bswapl_dst;						\
  } while (0)
#endif

/* x is bit reverse of y */
/* Note the divides below are all exact */
#define BITREV_ULONG(x,y)						   \
  do {									   \
   unsigned long __dst;							   \
   BSWAP_ULONG(__dst,y);						   \
   __dst = ((__dst>>4)&(ULONG_MAX/17)) | ((__dst<<4)&((ULONG_MAX/17)*16)); \
   __dst = ((__dst>>2)&(ULONG_MAX/5) ) | ((__dst<<2)&((ULONG_MAX/5)*4)  ); \
   __dst = ((__dst>>1)&(ULONG_MAX/3) ) | ((__dst<<1)&((ULONG_MAX/3)*2)  ); \
   (x) = __dst;								   \
  } while(0)
/* above could be improved if cpu has a nibble/bit swap/muxing instruction */
/* above code is serialized, possible to write as a big parallel expression */



void
mpz_fac_ui (mpz_ptr x, unsigned long n)
{
  unsigned long z, stt;
  int i, j;
  mpz_t t1, st[8 * sizeof (unsigned long) + 1 - APCONST];
  mp_limb_t d[4];

  static const mp_limb_t table[] = { ONE_LIMB_FACTORIAL_TABLE };

  if (n < numberof (table))
    {
      MPZ_SET_1_NZ (x, table[n]);
      return;
    }

  /*  NOTE : MUST have n>=3 here */
  ASSERT (n >= 3);
  /* for estimating the alloc sizes the calculation of these formula's is not
     exact and also the formulas are only approximations, also we ignore
     the few "side" calculations, correct allocation seems to speed up the
     small sizes better, having very little effect on the large sizes */

  /* estimate space for stack entries see below
     number of bits for n! is
     (1+log_2(2*pi)/2)-n*log_2(exp(1))+(n+1/2)*log_2(n)=
     2.325748065-n*1.442695041+(n+0.5)*log_2(n)  */
  umul_ppmm (d[1], d[0], (mp_limb_t) n, (mp_limb_t) FAC2OVERE);
  /* d[1] is 2n/e, d[0] ignored        */
  count_leading_zeros (z, d[1]);
  z = GMP_LIMB_BITS - z - 1;	/* z=floor(log_2(2n/e))   */
  umul_ppmm (d[1], d[0], (mp_limb_t) n, (mp_limb_t) z);
  /* d=n*floor(log_2(2n/e))   */
  d[0] = (d[0] >> 2) | (d[1] << (GMP_LIMB_BITS - 2));
  d[1] >>= 2;
  /* d=n*floor(log_2(2n/e))/4   */
  z = d[0] + 1;			/* have to ignore any overflow */
  /* so z is the number of bits wanted for st[0]    */


  if (n <= ((unsigned long) 1) << (APCONST))
    {
      mpz_realloc2 (x, 4 * z);
      ap_product_small (x, CNST_LIMB(2), CNST_LIMB(1), n - 1, 4L);
      return;
    }
  if (n <= ((unsigned long) 1) << (APCONST + 1))
    {				/*  use n!=odd(1,n)*(n/2)!*2^(n/2)         */
      mpz_init2 (t1, 2 * z);
      mpz_realloc2 (x, 4 * z);
      ap_product_small (x, CNST_LIMB(2), CNST_LIMB(1), n / 2 - 1, 4L);
      ap_product_small (t1, CNST_LIMB(3), CNST_LIMB(2), (n - 1) / 2, 4L);
      mpz_mul (x, x, t1);
      mpz_clear (t1);
      mpz_mul_2exp (x, x, n / 2);
      return;
    }
  if (n <= ((unsigned long) 1) << (APCONST + 2))
    {
      /* use n!=C_2(1,n/2)^2*C_2(n/2,n)*(n/4)!*2^(n/2+n/4) all int divs
	 so need (BITS_IN_N-APCONST+1)=(APCONST+3-APCONST+1)=4 stack entries */
      mpz_init2 (t1, 2 * z);
      mpz_realloc2 (x, 4 * z);
      for (i = 0; i < 4; i++)
	{
	  mpz_init2 (st[i], z);
	  z >>= 1;
	}
      odd_product (1, n / 2, st);
      mpz_set (x, st[0]);
      odd_product (n / 2, n, st);
      mpz_mul (x, x, x);
      ASSERT (n / 4 <= FACMUL4 + 6);
      ap_product_small (t1, CNST_LIMB(2), CNST_LIMB(1), n / 4 - 1, 4L);
      /* must have 2^APCONST odd numbers max */
      mpz_mul (t1, t1, st[0]);
      for (i = 0; i < 4; i++)
	mpz_clear (st[i]);
      mpz_mul (x, x, t1);
      mpz_clear (t1);
      mpz_mul_2exp (x, x, n / 2 + n / 4);
      return;
    }

  count_leading_zeros (stt, (mp_limb_t) n);
  stt = GMP_LIMB_BITS - stt + 1 - APCONST;

  for (i = 0; i < (signed long) stt; i++)
    {
      mpz_init2 (st[i], z);
      z >>= 1;
    }

  count_leading_zeros (z, (mp_limb_t) (n / 3));
  /* find z st 2^z>n/3 range for z is 1 <= z <= 8 * sizeof(unsigned long)-1 */
  z = GMP_LIMB_BITS - z;

  /*
     n! = 2^e * PRODUCT_{i=0}^{i=z-1} C_2( n/2^{i+1}, n/2^i )^{i+1}
     where 2^e || n!   3.2^z>n   C_2(a,b)=PRODUCT of odd z such that a<z<=b
   */


  mpz_init_set_ui (t1, 1);
  for (j = 8 * sizeof (unsigned long) / 2; j != 0; j >>= 1)
    {
      MPZ_SET_1_NZ (x, 1);
      for (i = 8 * sizeof (unsigned long) - j; i >= j; i -= 2 * j)
	if ((signed long) z >= i)
	  {
	    odd_product (n >> i, n >> (i - 1), st);
	    /* largest odd product when j=i=1 then we have
	       odd_product(n/2,n,st) which is approx (2n/e)^(n/4)
	       so log_base2(largest oddproduct)=n*log_base2(2n/e)/4
	       number of bits is n*log_base2(2n/e)/4+1  */
	    if (i != j)
	      mpz_pow_ui (st[0], st[0], i / j);
	    mpz_mul (x, x, st[0]);
	  }
      if ((signed long) z >= j && j != 1)
	{
	  mpz_mul (t1, t1, x);
	  mpz_mul (t1, t1, t1);
	}
    }
  for (i = 0; i < (signed long) stt; i++)
    mpz_clear (st[i]);
  mpz_mul (x, x, t1);
  mpz_clear (t1);
  popc_limb (i, (mp_limb_t) n);
  mpz_mul_2exp (x, x, n - i);
  return;
}

/* start,step are mp_limb_t although they will fit in unsigned long	*/
static void
ap_product_small (mpz_t ret, mp_limb_t start, mp_limb_t step,
		  unsigned long count, unsigned long nm)
{
  unsigned long a;
  mp_limb_t b;

  ASSERT (count <= (((unsigned long) 1) << APCONST));
/* count can never be zero ? check this and remove test below */
  if (count == 0)
    {
      MPZ_SET_1_NZ (ret, 1);
      return;
    }
  if (count == 1)
    {
      MPZ_SET_1_NZ (ret, start);
      return;
    }
  switch (nm)
    {
    case 1:
      MPZ_SET_1_NZ (ret, start);
      b = start + step;
      for (a = 0; a < count - 1; b += step, a++)
	MPZ_MUL_1_POS (ret, ret, b);
      return;
    case 2:
      MPZ_SET_1_NZ (ret, start * (start + step));
      if (count == 2)
	return;
      for (b = start + 2 * step, a = count / 2 - 1; a != 0;
	   a--, b += 2 * step)
	MPZ_MUL_1_POS (ret, ret, b * (b + step));
      if (count % 2 == 1)
	MPZ_MUL_1_POS (ret, ret, b);
      return;
    case 3:
      if (count == 2)
	{
	  MPZ_SET_1_NZ (ret, start * (start + step));
	  return;
	}
      MPZ_SET_1_NZ (ret, start * (start + step) * (start + 2 * step));
      if (count == 3)
	return;
      for (b = start + 3 * step, a = count / 3 - 1; a != 0;
	   a--, b += 3 * step)
	MPZ_MUL_1_POS (ret, ret, b * (b + step) * (b + 2 * step));
      if (count % 3 == 2)
	b = b * (b + step);
      if (count % 3 != 0)
	MPZ_MUL_1_POS (ret, ret, b);
      return;
    default:			/* ie nm=4      */
      if (count == 2)
	{
	  MPZ_SET_1_NZ (ret, start * (start + step));
	  return;
	}
      if (count == 3)
	{
	  MPZ_SET_1_NZ (ret, start * (start + step) * (start + 2 * step));
	  return;
	}
      MPZ_SET_1_NZ (ret,
		    start * (start + step) * (start + 2 * step) * (start +
								   3 * step));
      if (count == 4)
	return;
      for (b = start + 4 * step, a = count / 4 - 1; a != 0;
	   a--, b += 4 * step)
	MPZ_MUL_1_POS (ret, ret,
		       b * (b + step) * (b + 2 * step) * (b + 3 * step));
      if (count % 4 == 2)
	b = b * (b + step);
      if (count % 4 == 3)
	b = b * (b + step) * (b + 2 * step);
      if (count % 4 != 0)
	MPZ_MUL_1_POS (ret, ret, b);
      return;
    }
}

/* return value in st[0]
   odd_product(l,h)=sqrt((h/e)^h/(l/e)^l) using Stirling approx and e=exp(1)
   so st[0] needs enough bits for above, st[1] needs half these bits and
   st[2] needs 1/4 of these bits etc	*/
static void
odd_product (unsigned long low, unsigned long high, mpz_t * st)
{
  unsigned long stc = 1, stn = 0, n, y, mask, a, nm = 1;
  signed long z;

  low++;
  if (low % 2 == 0)
    low++;
  if (high == 0)
    high = 1;
  if (high % 2 == 0)
    high--;
/* must have high>=low ? check this and remove test below */
  if (high < low)
    {
      MPZ_SET_1_NZ (st[0], 1);
      return;
    }
  if (high == low)
    {
      MPZ_SET_1_NZ (st[0], low);
      return;
    }
  if (high <= FACMUL2 + 2)
    {
      nm = 2;
      if (high <= FACMUL3 + 4)
	{
	  nm = 3;
	  if (high <= FACMUL4 + 6)
	    nm = 4;
	}
    }
  high = (high - low) / 2 + 1;	/* high is now count,high<=2^(BITS_PER_ULONG-1) */
  if (high <= (((unsigned long) 1) << APCONST))
    {
      ap_product_small (st[0], (mp_limb_t) low, CNST_LIMB(2), high, nm);
      return;
    }
  count_leading_zeros (n, (mp_limb_t) high);
/* assumes clz above is LIMB based not NUMB based */
  n = GMP_LIMB_BITS - n - APCONST;
  mask = (((unsigned long) 1) << n);
  a = mask << 1;
  mask--;
/* have 2^(BITS_IN_N-APCONST) iterations so need
   (BITS_IN_N-APCONST+1) stack entries	*/
  for (z = mask; z >= 0; z--)
    {
      BITREV_ULONG (y, z);
      y >>= (BITS_PER_ULONG - n);
      ap_product_small (st[stn],
			(mp_limb_t) (low + 2 * ((~y) & mask)), (mp_limb_t) a,
			(high + y) >> n, nm);
      ASSERT (((high + y) >> n) <= (((unsigned long) 1) << APCONST));
      stn++;
      y = stc++;
      while ((y & 1) == 0)
	{
	  mpz_mul (st[stn - 2], st[stn - 2], st[stn - 1]);
	  stn--;
	  y >>= 1;
	}
    }
  ASSERT (stn == 1);
  return;
}
