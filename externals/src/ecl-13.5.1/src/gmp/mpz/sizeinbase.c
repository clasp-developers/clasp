/* mpz_sizeinbase(x, base) -- return an approximation to the number of
   character the integer X would have printed in base BASE.  The
   approximation is never too small.

Copyright 1991, 1993, 1994, 1995, 2001, 2002 Free Software Foundation, Inc.

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

#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"

size_t
mpz_sizeinbase (mpz_srcptr x, int base)
{
  size_t  result;
  MPN_SIZEINBASE (result, PTR(x), ABSIZ(x), base);
  return result;
}
