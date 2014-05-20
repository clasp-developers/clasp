/* mpf_div -- Divide two floats.

Copyright 1993, 1994, 1996, 2000, 2001, 2002, 2004, 2005 Free Software
Foundation, Inc.

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

#include <stdio.h>  /* for NULL */
#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"


/* Not done:

   No attempt is made to identify an overlap u==v.  The result will be
   correct (1.0), but a full actual division is done whereas of course
   x/x==1 needs no work.  Such a call is not a sensible thing to make, and
   it's left to an application to notice and optimize if it might arise
   somehow through pointer aliasing or whatever.

   Enhancements:

   The high quotient limb is non-zero when high{up,vsize} >= {vp,vsize}.  We
   could make that comparison and use qsize==prec instead of qsize==prec+1,
   to save one limb in the division.

   If r==u but the size is enough bigger than prec that there won't be an
   overlap between quotient and dividend in mpn_tdiv_qr, then we can avoid
   copying up,usize.  This would only arise from a prec reduced with
   mpf_set_prec_raw and will be pretty unusual, but might be worthwhile if
   it could be worked into the copy_u decision cleanly.

   Future:

   If/when mpn_tdiv_qr supports its qxn parameter we can use that instead of
   padding u with zeros in temporary space.

   If/when a quotient-only division exists it can be used here immediately.
   remp is only to satisfy mpn_tdiv_qr, the remainder is not used.  */

void
mpf_div (mpf_ptr r, mpf_srcptr u, mpf_srcptr v)
{
  mp_srcptr up, vp;
  mp_ptr rp, remp, tp, new_vp;
  mp_size_t usize, vsize, rsize, prospective_rsize, tsize, zeros, copy_v_size;
  mp_size_t sign_quotient, prec, high_zero, chop;
  mp_exp_t rexp;
  int copy_u;
  TMP_DECL;

  usize = u->_mp_size;
  vsize = v->_mp_size;
  sign_quotient = usize ^ vsize;
  usize = ABS (usize);
  vsize = ABS (vsize);
  prec = r->_mp_prec;

  if (vsize == 0)
    DIVIDE_BY_ZERO;

  if (usize == 0)
    {
      r->_mp_size = 0;
      r->_mp_exp = 0;
      return;
    }

  TMP_MARK;
  rexp = u->_mp_exp - v->_mp_exp + 1;

  rp = r->_mp_d;
  up = u->_mp_d;
  vp = v->_mp_d;

  prospective_rsize = usize - vsize + 1; /* quot from using given u,v sizes */
  rsize = prec + 1;                      /* desired quot */

  zeros = rsize - prospective_rsize;     /* padding u to give rsize */
  copy_u = (zeros > 0 || rp == up);      /* copy u if overlap or padding */

  chop = MAX (-zeros, 0);                /* negative zeros means shorten u */
  up += chop;
  usize -= chop;
  zeros += chop;                         /* now zeros >= 0 */

  tsize = usize + zeros;                 /* size for possible copy of u */

  if (WANT_TMP_DEBUG)
    {
      /* separate blocks, for malloc debugging */
      remp = TMP_ALLOC_LIMBS (vsize);
      tp = (copy_u ? TMP_ALLOC_LIMBS (tsize) : NULL);
      new_vp = (rp == vp ? TMP_ALLOC_LIMBS (vsize) : NULL);
    }
  else
    {
      /* one block with conditionalized size, for efficiency */
      copy_v_size = (rp == vp ? vsize : 0);
      remp = TMP_ALLOC_LIMBS (vsize + copy_v_size + (copy_u ? tsize : 0));
      new_vp = remp + vsize;
      tp = new_vp + copy_v_size;
    }

  /* copy and possibly extend u if necessary */
  if (copy_u)
    {
      MPN_ZERO (tp, zeros);
      MPN_COPY (tp+zeros, up, usize);
      up = tp;
      usize = tsize;
    }

  /* ensure divisor doesn't overlap quotient */
  if (rp == vp)
    {
      MPN_COPY (new_vp, vp, vsize);
      vp = new_vp;
    }

  ASSERT (usize-vsize+1 == rsize);
  mpn_tdiv_qr (rp, remp, (mp_size_t) 0, up, usize, vp, vsize);

  /* strip possible zero high limb */
  high_zero = (rp[rsize-1] == 0);
  rsize -= high_zero;
  rexp -= high_zero;

  r->_mp_size = sign_quotient >= 0 ? rsize : -rsize;
  r->_mp_exp = rexp;
  TMP_FREE;
}
