/* mpn_set_str_basecase -- mpn_set_str forced to its basecase.

Copyright 2002 Free Software Foundation, Inc.

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

#ifndef SIZE_T_MAX
#define SIZE_T_MAX  ((size_t) ULONG_MAX)
#endif

#undef SET_STR_THRESHOLD
#define SET_STR_THRESHOLD  SIZE_T_MAX /* always */
#define __gmpn_set_str mpn_set_str_basecase

#include "mpn/generic/set_str.c"
