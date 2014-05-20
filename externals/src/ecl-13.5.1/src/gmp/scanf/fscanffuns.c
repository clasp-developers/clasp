/* __gmp_fscanf_funs -- support for formatted input from a FILE.

   THE FUNCTIONS IN THIS FILE ARE FOR INTERNAL USE ONLY.  THEY'RE ALMOST
   CERTAIN TO BE SUBJECT TO INCOMPATIBLE CHANGES OR DISAPPEAR COMPLETELY IN
   FUTURE GNU MP RELEASES.

Copyright 2001 Free Software Foundation, Inc.

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


/* SunOS 4 stdio.h doesn't provide prototypes for these */
#if ! HAVE_DECL_FGETC
int fgetc _PROTO ((FILE *fp));
#endif
#if ! HAVE_DECL_FSCANF
int fscanf _PROTO ((FILE *fp, const char *fmt, ...));
#endif
#if ! HAVE_DECL_UNGETC
int ungetc _PROTO ((int c, FILE *fp));
#endif


static void
step (FILE *fp, int n)
{
}

const struct gmp_doscan_funs_t  __gmp_fscanf_funs = {
  (gmp_doscan_scan_t)  fscanf,
  (gmp_doscan_step_t)  step,
  (gmp_doscan_get_t)   fgetc,
  (gmp_doscan_unget_t) ungetc,
};
