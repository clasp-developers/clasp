/* mp_set_memory_functions -- Set the allocate, reallocate, and free functions
   for use by the mp package.

Copyright 1991, 1993, 1994, 2000, 2001 Free Software Foundation, Inc.

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

void
mp_set_memory_functions (void *(*alloc_func) (size_t),
			 void *(*realloc_func) (void *, size_t, size_t),
			 void (*free_func) (void *, size_t))
{
  if (alloc_func == 0)
    alloc_func = __gmp_default_allocate;
  if (realloc_func == 0)
    realloc_func = __gmp_default_reallocate;
  if (free_func == 0)
    free_func = __gmp_default_free;

  __gmp_allocate_func = alloc_func;
  __gmp_reallocate_func = realloc_func;
  __gmp_free_func = free_func;
}
