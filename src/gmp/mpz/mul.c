/* mpz_mul -- Multiply two integers.

Copyright 1991, 1993, 1994, 1996, 2000, 2001, 2005 Free Software Foundation,
Inc.

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
MA 02110-1301, USA. */

#include <stdio.h> /* for NULL */
#include "gmp.h"
#include "gmp-impl.h"
#ifdef BERKELEY_MP
#include "mp.h"
#endif


void
#ifndef BERKELEY_MP
mpz_mul (mpz_ptr w, mpz_srcptr u, mpz_srcptr v)
#else /* BERKELEY_MP */
mult (mpz_srcptr u, mpz_srcptr v, mpz_ptr w)
#endif /* BERKELEY_MP */
{
  mp_size_t usize = u->_mp_size;
  mp_size_t vsize = v->_mp_size;
  mp_size_t wsize;
  mp_size_t sign_product;
  mp_ptr up, vp;
  mp_ptr wp;
  mp_ptr free_me;
  size_t free_me_size;
  mp_limb_t cy_limb;
  TMP_DECL;

  sign_product = usize ^ vsize;
  usize = ABS (usize);
  vsize = ABS (vsize);

  if (usize < vsize)
    {
      MPZ_SRCPTR_SWAP (u, v);
      MP_SIZE_T_SWAP (usize, vsize);
    }

  if (vsize == 0)
    {
      SIZ(w) = 0;
      return;
    }

#if HAVE_NATIVE_mpn_mul_2
  if (vsize <= 2)
    {
      MPZ_REALLOC (w, usize+vsize);
      wp = PTR(w);
      if (vsize == 1)
        cy_limb = mpn_mul_1 (wp, PTR(u), usize, PTR(v)[0]);
      else
        {
          cy_limb = mpn_mul_2 (wp, PTR(u), usize, PTR(v));
          usize++;
        }
      wp[usize] = cy_limb;
      usize += (cy_limb != 0);
      SIZ(w) = (sign_product >= 0 ? usize : -usize);
      return;
    }
#else
  if (vsize == 1)
    {
      MPZ_REALLOC (w, usize+1);
      wp = PTR(w);
      cy_limb = mpn_mul_1 (wp, PTR(u), usize, PTR(v)[0]);
      wp[usize] = cy_limb;
      usize += (cy_limb != 0);
      SIZ(w) = (sign_product >= 0 ? usize : -usize);
      return;
    }
#endif

  TMP_MARK;
  free_me = NULL;
  up = u->_mp_d;
  vp = v->_mp_d;
  wp = w->_mp_d;

  /* Ensure W has space enough to store the result.  */
  wsize = usize + vsize;
  if (w->_mp_alloc < wsize)
    {
      if (wp == up || wp == vp)
	{
	  free_me = wp;
	  free_me_size = w->_mp_alloc;
	}
      else
	(*__gmp_free_func) (wp, w->_mp_alloc * BYTES_PER_MP_LIMB);

      w->_mp_alloc = wsize;
      wp = (mp_ptr) (*__gmp_allocate_func) (wsize * BYTES_PER_MP_LIMB);
      w->_mp_d = wp;
    }
  else
    {
      /* Make U and V not overlap with W.  */
      if (wp == up)
	{
	  /* W and U are identical.  Allocate temporary space for U.  */
	  up = (mp_ptr) TMP_ALLOC (usize * BYTES_PER_MP_LIMB);
	  /* Is V identical too?  Keep it identical with U.  */
	  if (wp == vp)
	    vp = up;
	  /* Copy to the temporary space.  */
	  MPN_COPY (up, wp, usize);
	}
      else if (wp == vp)
	{
	  /* W and V are identical.  Allocate temporary space for V.  */
	  vp = (mp_ptr) TMP_ALLOC (vsize * BYTES_PER_MP_LIMB);
	  /* Copy to the temporary space.  */
	  MPN_COPY (vp, wp, vsize);
	}
    }

  cy_limb = mpn_mul (wp, up, usize, vp, vsize);
  wsize = usize + vsize;
  wsize -= cy_limb == 0;

  w->_mp_size = sign_product < 0 ? -wsize : wsize;
  if (free_me != NULL)
    (*__gmp_free_func) (free_me, free_me_size * BYTES_PER_MP_LIMB);
  TMP_FREE;
}
