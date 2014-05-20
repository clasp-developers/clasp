/* mpn/generic/gcd.c accel GCD find_a() made available for measuring.  */

/*
Copyright 1999, 2000 Free Software Foundation, Inc.

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

#include "gmp.h"
#include "gmp-impl.h"

#ifdef HAVE_NATIVE_mpn_gcd_finda
#undef HAVE_NATIVE_mpn_gcd_finda
#endif
#define __gmpn_gcd  __UNWANTED_gcd

#include "mpn/generic/gcd.c"


#include "speed.h"

double
speed_find_a (struct speed_params *s)
{
  SPEED_ROUTINE_MPN_GCD_FINDA (find_a)
}
