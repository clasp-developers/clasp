/* mpn_sqrtrem -- square root and remainder

   Contributed to the GNU project by Paul Zimmermann (most code) and
   Torbjorn Granlund (mpn_sqrtrem1).

   THE FUNCTIONS IN THIS FILE EXCEPT mpn_sqrtrem ARE INTERNAL WITH A
   MUTABLE INTERFACE.  IT IS ONLY SAFE TO REACH THEM THROUGH DOCUMENTED
   INTERFACES.  IN FACT, IT IS ALMOST GUARANTEED THAT THEY WILL CHANGE OR
   DISAPPEAR IN A FUTURE GMP RELEASE.

Copyright 1999, 2000, 2001, 2002, 2004, 2005, 2008, 2010 Free Software
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


/* See "Karatsuba Square Root", reference in gmp.texi.  */


#include <stdio.h>
#include <stdlib.h>

#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"

static const unsigned short invsqrttab[384] =
{
  0x1ff,0x1fd,0x1fb,0x1f9,0x1f7,0x1f5,0x1f3,0x1f2, /* sqrt(1/80)..sqrt(1/87) */
  0x1f0,0x1ee,0x1ec,0x1ea,0x1e9,0x1e7,0x1e5,0x1e4, /* sqrt(1/88)..sqrt(1/8f) */
  0x1e2,0x1e0,0x1df,0x1dd,0x1db,0x1da,0x1d8,0x1d7, /* sqrt(1/90)..sqrt(1/97) */
  0x1d5,0x1d4,0x1d2,0x1d1,0x1cf,0x1ce,0x1cc,0x1cb, /* sqrt(1/98)..sqrt(1/9f) */
  0x1c9,0x1c8,0x1c6,0x1c5,0x1c4,0x1c2,0x1c1,0x1c0, /* sqrt(1/a0)..sqrt(1/a7) */
  0x1be,0x1bd,0x1bc,0x1ba,0x1b9,0x1b8,0x1b7,0x1b5, /* sqrt(1/a8)..sqrt(1/af) */
  0x1b4,0x1b3,0x1b2,0x1b0,0x1af,0x1ae,0x1ad,0x1ac, /* sqrt(1/b0)..sqrt(1/b7) */
  0x1aa,0x1a9,0x1a8,0x1a7,0x1a6,0x1a5,0x1a4,0x1a3, /* sqrt(1/b8)..sqrt(1/bf) */
  0x1a2,0x1a0,0x19f,0x19e,0x19d,0x19c,0x19b,0x19a, /* sqrt(1/c0)..sqrt(1/c7) */
  0x199,0x198,0x197,0x196,0x195,0x194,0x193,0x192, /* sqrt(1/c8)..sqrt(1/cf) */
  0x191,0x190,0x18f,0x18e,0x18d,0x18c,0x18c,0x18b, /* sqrt(1/d0)..sqrt(1/d7) */
  0x18a,0x189,0x188,0x187,0x186,0x185,0x184,0x183, /* sqrt(1/d8)..sqrt(1/df) */
  0x183,0x182,0x181,0x180,0x17f,0x17e,0x17e,0x17d, /* sqrt(1/e0)..sqrt(1/e7) */
  0x17c,0x17b,0x17a,0x179,0x179,0x178,0x177,0x176, /* sqrt(1/e8)..sqrt(1/ef) */
  0x176,0x175,0x174,0x173,0x172,0x172,0x171,0x170, /* sqrt(1/f0)..sqrt(1/f7) */
  0x16f,0x16f,0x16e,0x16d,0x16d,0x16c,0x16b,0x16a, /* sqrt(1/f8)..sqrt(1/ff) */
  0x16a,0x169,0x168,0x168,0x167,0x166,0x166,0x165, /* sqrt(1/100)..sqrt(1/107) */
  0x164,0x164,0x163,0x162,0x162,0x161,0x160,0x160, /* sqrt(1/108)..sqrt(1/10f) */
  0x15f,0x15e,0x15e,0x15d,0x15c,0x15c,0x15b,0x15a, /* sqrt(1/110)..sqrt(1/117) */
  0x15a,0x159,0x159,0x158,0x157,0x157,0x156,0x156, /* sqrt(1/118)..sqrt(1/11f) */
  0x155,0x154,0x154,0x153,0x153,0x152,0x152,0x151, /* sqrt(1/120)..sqrt(1/127) */
  0x150,0x150,0x14f,0x14f,0x14e,0x14e,0x14d,0x14d, /* sqrt(1/128)..sqrt(1/12f) */
  0x14c,0x14b,0x14b,0x14a,0x14a,0x149,0x149,0x148, /* sqrt(1/130)..sqrt(1/137) */
  0x148,0x147,0x147,0x146,0x146,0x145,0x145,0x144, /* sqrt(1/138)..sqrt(1/13f) */
  0x144,0x143,0x143,0x142,0x142,0x141,0x141,0x140, /* sqrt(1/140)..sqrt(1/147) */
  0x140,0x13f,0x13f,0x13e,0x13e,0x13d,0x13d,0x13c, /* sqrt(1/148)..sqrt(1/14f) */
  0x13c,0x13b,0x13b,0x13a,0x13a,0x139,0x139,0x139, /* sqrt(1/150)..sqrt(1/157) */
  0x138,0x138,0x137,0x137,0x136,0x136,0x135,0x135, /* sqrt(1/158)..sqrt(1/15f) */
  0x135,0x134,0x134,0x133,0x133,0x132,0x132,0x132, /* sqrt(1/160)..sqrt(1/167) */
  0x131,0x131,0x130,0x130,0x12f,0x12f,0x12f,0x12e, /* sqrt(1/168)..sqrt(1/16f) */
  0x12e,0x12d,0x12d,0x12d,0x12c,0x12c,0x12b,0x12b, /* sqrt(1/170)..sqrt(1/177) */
  0x12b,0x12a,0x12a,0x129,0x129,0x129,0x128,0x128, /* sqrt(1/178)..sqrt(1/17f) */
  0x127,0x127,0x127,0x126,0x126,0x126,0x125,0x125, /* sqrt(1/180)..sqrt(1/187) */
  0x124,0x124,0x124,0x123,0x123,0x123,0x122,0x122, /* sqrt(1/188)..sqrt(1/18f) */
  0x121,0x121,0x121,0x120,0x120,0x120,0x11f,0x11f, /* sqrt(1/190)..sqrt(1/197) */
  0x11f,0x11e,0x11e,0x11e,0x11d,0x11d,0x11d,0x11c, /* sqrt(1/198)..sqrt(1/19f) */
  0x11c,0x11b,0x11b,0x11b,0x11a,0x11a,0x11a,0x119, /* sqrt(1/1a0)..sqrt(1/1a7) */
  0x119,0x119,0x118,0x118,0x118,0x118,0x117,0x117, /* sqrt(1/1a8)..sqrt(1/1af) */
  0x117,0x116,0x116,0x116,0x115,0x115,0x115,0x114, /* sqrt(1/1b0)..sqrt(1/1b7) */
  0x114,0x114,0x113,0x113,0x113,0x112,0x112,0x112, /* sqrt(1/1b8)..sqrt(1/1bf) */
  0x112,0x111,0x111,0x111,0x110,0x110,0x110,0x10f, /* sqrt(1/1c0)..sqrt(1/1c7) */
  0x10f,0x10f,0x10f,0x10e,0x10e,0x10e,0x10d,0x10d, /* sqrt(1/1c8)..sqrt(1/1cf) */
  0x10d,0x10c,0x10c,0x10c,0x10c,0x10b,0x10b,0x10b, /* sqrt(1/1d0)..sqrt(1/1d7) */
  0x10a,0x10a,0x10a,0x10a,0x109,0x109,0x109,0x109, /* sqrt(1/1d8)..sqrt(1/1df) */
  0x108,0x108,0x108,0x107,0x107,0x107,0x107,0x106, /* sqrt(1/1e0)..sqrt(1/1e7) */
  0x106,0x106,0x106,0x105,0x105,0x105,0x104,0x104, /* sqrt(1/1e8)..sqrt(1/1ef) */
  0x104,0x104,0x103,0x103,0x103,0x103,0x102,0x102, /* sqrt(1/1f0)..sqrt(1/1f7) */
  0x102,0x102,0x101,0x101,0x101,0x101,0x100,0x100  /* sqrt(1/1f8)..sqrt(1/1ff) */
};

/* Compute s = floor(sqrt(a0)), and *rp = a0 - s^2.  */

#if GMP_NUMB_BITS > 32
#define MAGIC CNST_LIMB(0x10000000000)	/* 0xffe7debbfc < MAGIC < 0x232b1850f410 */
#else
#define MAGIC CNST_LIMB(0x100000)		/* 0xfee6f < MAGIC < 0x29cbc8 */
#endif

static mp_limb_t
mpn_sqrtrem1 (mp_ptr rp, mp_limb_t a0)
{
#if GMP_NUMB_BITS > 32
  mp_limb_t a1;
#endif
  mp_limb_t x0, t2, t, x2;
  unsigned abits;

  ASSERT_ALWAYS (GMP_NAIL_BITS == 0);
  ASSERT_ALWAYS (GMP_LIMB_BITS == 32 || GMP_LIMB_BITS == 64);
  ASSERT (a0 >= GMP_NUMB_HIGHBIT / 2);

  /* Use Newton iterations for approximating 1/sqrt(a) instead of sqrt(a),
     since we can do the former without division.  As part of the last
     iteration convert from 1/sqrt(a) to sqrt(a).  */

  abits = a0 >> (GMP_LIMB_BITS - 1 - 8);	/* extract bits for table lookup */
  x0 = invsqrttab[abits - 0x80];		/* initial 1/sqrt(a) */

  /* x0 is now an 8 bits approximation of 1/sqrt(a0) */

#if GMP_NUMB_BITS > 32
  a1 = a0 >> (GMP_LIMB_BITS - 1 - 32);
  t = (mp_limb_signed_t) (CNST_LIMB(0x2000000000000) - 0x30000  - a1 * x0 * x0) >> 16;
  x0 = (x0 << 16) + ((mp_limb_signed_t) (x0 * t) >> (16+2));

  /* x0 is now an 16 bits approximation of 1/sqrt(a0) */

  t2 = x0 * (a0 >> (32-8));
  t = t2 >> 25;
  t = ((mp_limb_signed_t) ((a0 << 14) - t * t - MAGIC) >> (32-8));
  x0 = t2 + ((mp_limb_signed_t) (x0 * t) >> 15);
  x0 >>= 32;
#else
  t2 = x0 * (a0 >> (16-8));
  t = t2 >> 13;
  t = ((mp_limb_signed_t) ((a0 << 6) - t * t - MAGIC) >> (16-8));
  x0 = t2 + ((mp_limb_signed_t) (x0 * t) >> 7);
  x0 >>= 16;
#endif

  /* x0 is now a full limb approximation of sqrt(a0) */

  x2 = x0 * x0;
  if (x2 + 2*x0 <= a0 - 1)
    {
      x2 += 2*x0 + 1;
      x0++;
    }

  *rp = a0 - x2;
  return x0;
}


#define Prec (GMP_NUMB_BITS >> 1)

/* same as mpn_sqrtrem, but for size=2 and {np, 2} normalized
   return cc such that {np, 2} = sp[0]^2 + cc*2^GMP_NUMB_BITS + rp[0] */
static mp_limb_t
mpn_sqrtrem2 (mp_ptr sp, mp_ptr rp, mp_srcptr np)
{
  mp_limb_t qhl, q, u, np0, sp0, rp0, q2;
  int cc;

  ASSERT (np[1] >= GMP_NUMB_HIGHBIT / 2);

  np0 = np[0];
  sp0 = mpn_sqrtrem1 (rp, np[1]);
  qhl = 0;
  rp0 = rp[0];
  while (rp0 >= sp0)
    {
      qhl++;
      rp0 -= sp0;
    }
  /* now rp0 < sp0 < 2^Prec */
  rp0 = (rp0 << Prec) + (np0 >> Prec);
  u = 2 * sp0;
  q = rp0 / u;
  u = rp0 - q * u;
  q += (qhl & 1) << (Prec - 1);
  qhl >>= 1; /* if qhl=1, necessary q=0 as qhl*2^Prec + q <= 2^Prec */
  /* now we have (initial rp0)<<Prec + np0>>Prec = (qhl<<Prec + q) * (2sp0) + u */
  sp0 = ((sp0 + qhl) << Prec) + q;
  cc = u >> Prec;
  rp0 = ((u << Prec) & GMP_NUMB_MASK) + (np0 & (((mp_limb_t) 1 << Prec) - 1));
  /* subtract q * q or qhl*2^(2*Prec) from rp */
  q2 = q * q;
  cc -= (rp0 < q2) + qhl;
  rp0 -= q2;
  /* now subtract 2*q*2^Prec + 2^(2*Prec) if qhl is set */
  if (cc < 0)
    {
      if (sp0 != 0)
	{
	  rp0 += sp0;
	  cc += rp0 < sp0;
	}
      else
	cc++;
      --sp0;
      rp0 += sp0;
      cc += rp0 < sp0;
    }

  rp[0] = rp0;
  sp[0] = sp0;
  return cc;
}

/* writes in {sp, n} the square root (rounded towards zero) of {np, 2n},
   and in {np, n} the low n limbs of the remainder, returns the high
   limb of the remainder (which is 0 or 1).
   Assumes {np, 2n} is normalized, i.e. np[2n-1] >= B/4
   where B=2^GMP_NUMB_BITS.  */
static mp_limb_t
mpn_dc_sqrtrem (mp_ptr sp, mp_ptr np, mp_size_t n)
{
  mp_limb_t q;			/* carry out of {sp, n} */
  int c, b;			/* carry out of remainder */
  mp_size_t l, h;

  ASSERT (np[2 * n - 1] >= GMP_NUMB_HIGHBIT / 2);

  if (n == 1)
    c = mpn_sqrtrem2 (sp, np, np);
  else
    {
      l = n / 2;
      h = n - l;
      q = mpn_dc_sqrtrem (sp + l, np + 2 * l, h);
      if (q != 0)
	mpn_sub_n (np + 2 * l, np + 2 * l, sp + l, h);
      q += mpn_divrem (sp, 0, np + l, n, sp + l, h);
      c = sp[0] & 1;
      mpn_rshift (sp, sp, l, 1);
      sp[l - 1] |= (q << (GMP_NUMB_BITS - 1)) & GMP_NUMB_MASK;
      q >>= 1;
      if (c != 0)
	c = mpn_add_n (np + l, np + l, sp + l, h);
      mpn_sqr (np + n, sp, l);
      b = q + mpn_sub_n (np, np, np + n, 2 * l);
      c -= (l == h) ? b : mpn_sub_1 (np + 2 * l, np + 2 * l, 1, (mp_limb_t) b);
      q = mpn_add_1 (sp + l, sp + l, h, q);

      if (c < 0)
	{
	  c += mpn_addmul_1 (np, sp, n, CNST_LIMB(2)) + 2 * q;
	  c -= mpn_sub_1 (np, np, n, CNST_LIMB(1));
	  q -= mpn_sub_1 (sp, sp, n, CNST_LIMB(1));
	}
    }

  return c;
}


mp_size_t
mpn_sqrtrem (mp_ptr sp, mp_ptr rp, mp_srcptr np, mp_size_t nn)
{
  mp_limb_t *tp, s0[1], cc, high, rl;
  int c;
  mp_size_t rn, tn;
  TMP_DECL;

  ASSERT (nn >= 0);
  ASSERT_MPN (np, nn);

  /* If OP is zero, both results are zero.  */
  if (nn == 0)
    return 0;

  ASSERT (np[nn - 1] != 0);
  ASSERT (rp == NULL || MPN_SAME_OR_SEPARATE_P (np, rp, nn));
  ASSERT (rp == NULL || ! MPN_OVERLAP_P (sp, (nn + 1) / 2, rp, nn));
  ASSERT (! MPN_OVERLAP_P (sp, (nn + 1) / 2, np, nn));

  high = np[nn - 1];
  if (nn == 1 && (high & GMP_NUMB_HIGHBIT))
    {
      mp_limb_t r;
      sp[0] = mpn_sqrtrem1 (&r, high);
      if (rp != NULL)
	rp[0] = r;
      return r != 0;
    }
  count_leading_zeros (c, high);
  c -= GMP_NAIL_BITS;

  c = c / 2; /* we have to shift left by 2c bits to normalize {np, nn} */
  tn = (nn + 1) / 2; /* 2*tn is the smallest even integer >= nn */

  TMP_MARK;
  if (nn % 2 != 0 || c > 0)
    {
      tp = TMP_ALLOC_LIMBS (2 * tn);
      tp[0] = 0;	     /* needed only when 2*tn > nn, but saves a test */
      if (c != 0)
	mpn_lshift (tp + 2 * tn - nn, np, nn, 2 * c);
      else
	MPN_COPY (tp + 2 * tn - nn, np, nn);
      rl = mpn_dc_sqrtrem (sp, tp, tn);
      /* We have 2^(2k)*N = S^2 + R where k = c + (2tn-nn)*GMP_NUMB_BITS/2,
	 thus 2^(2k)*N = (S-s0)^2 + 2*S*s0 - s0^2 + R where s0=S mod 2^k */
      c += (nn % 2) * GMP_NUMB_BITS / 2;		/* c now represents k */
      s0[0] = sp[0] & (((mp_limb_t) 1 << c) - 1);	/* S mod 2^k */
      rl += mpn_addmul_1 (tp, sp, tn, 2 * s0[0]);	/* R = R + 2*s0*S */
      cc = mpn_submul_1 (tp, s0, 1, s0[0]);
      rl -= (tn > 1) ? mpn_sub_1 (tp + 1, tp + 1, tn - 1, cc) : cc;
      mpn_rshift (sp, sp, tn, c);
      tp[tn] = rl;
      if (rp == NULL)
	rp = tp;
      c = c << 1;
      if (c < GMP_NUMB_BITS)
	tn++;
      else
	{
	  tp++;
	  c -= GMP_NUMB_BITS;
	}
      if (c != 0)
	mpn_rshift (rp, tp, tn, c);
      else
	MPN_COPY_INCR (rp, tp, tn);
      rn = tn;
    }
  else
    {
      if (rp == NULL)
	rp = TMP_ALLOC_LIMBS (nn);
      if (rp != np)
	MPN_COPY (rp, np, nn);
      rn = tn + (rp[tn] = mpn_dc_sqrtrem (sp, rp, tn));
    }

  MPN_NORMALIZE (rp, rn);

  TMP_FREE;
  return rn;
}
