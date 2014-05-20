/* Copyright (C) 2004 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Wolfram Gloger <wg@malloc.de>, 2004.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */

#include <errno.h>
#include <stdio.h>
#include "malloc.h"

static int errors = 0;

static void
merror (const char *msg)
{
  ++errors;
  printf ("Error: %s\n", msg);
}

int
main (void)
{
  void *p1, *p2;
  long i;
  mstate a;
  struct malloc_arena_info mai;
  int nfree;
  unsigned long navail;

  errno = 0;

  malloc_stats(); /* check that it works even without initialization */
  a = _int_get_arena(0);
  if (!a) {
    merror ("Can't get main arena.");
    return 1;
  }
  free (malloc (10));
  _int_get_arena_info(a, &mai);
  printf("nfree     = %d\navail     = %lu\nfastavail = %lu\ntop_size  = %lu\n",
	 mai.nbinblocks + mai.nfastblocks,
	 (unsigned long)mai.binavail,
	 (unsigned long)mai.fastavail,
	 (unsigned long)mai.top_size);
  if (mai.nfastblocks+mai.nbinblocks < 1)
    merror ("initial _int_get_arena_info() failed.");
  nfree = mai.nbinblocks + mai.nfastblocks;
  navail = mai.binavail + mai.fastavail;

  p1 = malloc (10);
  if (p1 == NULL)
    merror ("malloc (10) failed.");
  p2 = malloc (30);
  if (p2 == NULL)
    merror ("malloc (30) failed.");

  free (malloc (10));

  for (i=0; i<100; ++i)
    {
      p1 = realloc (p1, i*7 + 3);
      if (p1 == NULL)
	merror ("realloc (i*7 + 3) failed.");
    }
  free (p2);

  _int_get_arena_info(a, &mai);
  printf("nfree     = %d\navail     = %lu\nfastavail = %lu\ntop_size  = %lu\n",
	 mai.nbinblocks + mai.nfastblocks,
	 (unsigned long)mai.binavail,
	 (unsigned long)mai.fastavail,
	 (unsigned long)mai.top_size);
  /* Assume that no memory is returned to the system from these small
     chunks.  */
  if (mai.nbinblocks+mai.nfastblocks < nfree ||
      mai.binavail+mai.fastavail < navail)
    merror ("final _int_get_arena_info() failed.");
  malloc_stats();

  return errors != 0;
}

/*
 * Local variables:
 * c-basic-offset: 2
 * End:
 */
