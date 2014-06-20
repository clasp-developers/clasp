/* itom -- BSD compatible allocate and initiate a MINT.

Copyright 1991, 1994, 1995, 2000, 2001 Free Software Foundation, Inc.

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

#include "mp.h"
#include "gmp.h"
#include "gmp-impl.h"

MINT *
itom (signed short int n)
{
  MINT *x;
  mp_ptr xp;

  x = (MINT *) (*__gmp_allocate_func) (sizeof (MINT));
  x->_mp_alloc = 1;
  x->_mp_d = xp = (mp_ptr) (*__gmp_allocate_func) (BYTES_PER_MP_LIMB);
  if (n > 0)
    {
      x->_mp_size = 1;
      xp[0] = n;
    }
  else if (n < 0)
    {
      x->_mp_size = -1;
      xp[0] = (unsigned short) -n;
    }
  else
    x->_mp_size = 0;

  return x;
}
