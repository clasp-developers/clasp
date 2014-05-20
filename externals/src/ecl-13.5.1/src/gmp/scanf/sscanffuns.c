/* __gmp_sscanf_funs -- support for formatted input from a string.

   THE FUNCTIONS IN THIS FILE ARE FOR INTERNAL USE ONLY.  THEY'RE ALMOST
   CERTAIN TO BE SUBJECT TO INCOMPATIBLE CHANGES OR DISAPPEAR COMPLETELY IN
   FUTURE GNU MP RELEASES.

Copyright 2001, 2002 Free Software Foundation, Inc.

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

#include <stdio.h>
#include "gmp.h"
#include "gmp-impl.h"


static int
scan (const char **sp, const char *fmt, void *p1, void *p2)
{
  return sscanf (*sp, fmt, p1, p2);
}

static void
step (const char **sp, int n)
{
  ASSERT (n >= 0);

  /* shouldn't push us past the the end of the string */
#if WANT_ASSERT
  {
    int  i;
    for (i = 0; i < n; i++)
      ASSERT ((*sp)[i] != '\0');
  }
#endif

  (*sp) += n;
}

static int
get (const char **sp)
{
  const char  *s;
  int  c;
  s = *sp;
  c = (unsigned char) *s++;
  if (c == '\0')
    return EOF;
  *sp = s;
  return c;
}

static void
unget (int c, const char **sp)
{
  const char  *s;
  s = *sp;
  if (c == EOF)
    {
      ASSERT (*s == '\0');
      return;
    }
  s--;
  ASSERT ((unsigned char) *s == c);
  *sp = s;
}

const struct gmp_doscan_funs_t  __gmp_sscanf_funs = {
  (gmp_doscan_scan_t)  scan,
  (gmp_doscan_step_t)  step,
  (gmp_doscan_get_t)   get,
  (gmp_doscan_unget_t) unget,
};
