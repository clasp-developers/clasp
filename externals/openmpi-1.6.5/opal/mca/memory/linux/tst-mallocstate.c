/* Copyright (C) 2001 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Wolfram Gloger <wg@malloc.de>, 2001.

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
  void *save_state;
  long i;

  errno = 0;

  p1 = malloc (10);
  if (p1 == NULL)
    merror ("malloc (10) failed.");

  p2 = malloc (20);
  if (p2 == NULL)
    merror ("malloc (20) failed.");

  free (malloc (10));

  for (i=0; i<100; ++i)
    {
      save_state = malloc_get_state ();
      if (save_state == NULL)
	{
	  merror ("malloc_get_state () failed.");
	  break;
	}
      /*free (malloc (10)); This could change the top chunk! */
      malloc_set_state (save_state);
      p1 = realloc (p1, i*4 + 4);
      if (p1 == NULL)
	merror ("realloc (i*4) failed.");
      free (save_state);
    }

  p1 = realloc (p1, 40);
  free (p2);
  p2 = malloc (10);
  if (p2 == NULL)
    merror ("malloc (10) failed.");
  free (p1);

  return errors != 0;
}

/*
 * Local variables:
 * c-basic-offset: 2
 * End:
 */
