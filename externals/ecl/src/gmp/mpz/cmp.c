/* mpz_cmp(u,v) -- Compare U, V.  Return positive, zero, or negative
   based on if U > V, U == V, or U < V.

Copyright 1991, 1993, 1994, 1996, 2001, 2002 Free Software Foundation, Inc.

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

#ifdef BERKELEY_MP
#include "mp.h"
#endif
#include "gmp.h"
#include "gmp-impl.h"

int
#ifdef BERKELEY_MP
mcmp (mpz_srcptr u, mpz_srcptr v)
#else
mpz_cmp (mpz_srcptr u, mpz_srcptr v)
#endif
{
  mp_size_t  usize, vsize, dsize, asize;
  mp_srcptr  up, vp;
  int        cmp;

  usize = SIZ(u);
  vsize = SIZ(v);
  dsize = usize - vsize;
  if (dsize != 0)
    return dsize;

  asize = ABS (usize);
  up = PTR(u);
  vp = PTR(v);
  MPN_CMP (cmp, up, vp, asize);
  return (usize >= 0 ? cmp : -cmp);
}
