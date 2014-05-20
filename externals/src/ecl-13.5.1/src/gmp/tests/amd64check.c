/* AMD64 calling conventions checking.

Copyright 2000, 2001, 2004 Free Software Foundation, Inc.

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
#include "tests.h"


/* temporaries */
long  calling_conventions_save_rbx;
long  calling_conventions_save_rbp;
long  calling_conventions_save_r12;
long  calling_conventions_save_r13;
long  calling_conventions_save_r14;
long  calling_conventions_save_r15;
long  calling_conventions_retaddr;
long  calling_conventions_retval;

/* values to check */
struct {
  int  control;
  int  status;
  int  tag;
  int  other[4];
} calling_conventions_fenv;
long  calling_conventions_rbx;
long  calling_conventions_rbp;
long  calling_conventions_r12;
long  calling_conventions_r13;
long  calling_conventions_r14;
long  calling_conventions_r15;
long  calling_conventions_rflags;

/* expected values, as per amd64call.asm */
const long  calling_conventions_want_rbx = 0x1234567887654321L;
const long  calling_conventions_want_rbp = 0x89ABCDEFFEDCBA98L;
const long  calling_conventions_want_r12 = 0xDEADBEEFBADECAFEL;
const long  calling_conventions_want_r13 = 0xFFEEDDCCBBAA9988L;
const long  calling_conventions_want_r14 = 0x0011223344556677L;
const long  calling_conventions_want_r15 = 0x1234432156788765L;

#define DIR_BIT(rflags)   (((rflags) & (1<<10)) != 0)


/* Return 1 if ok, 0 if not */

int
calling_conventions_check (void)
{
  const char  *header = "Violated calling conventions:\n";
  int  ret = 1;

#define CHECK(callreg, regstr, value)                   \
  if (callreg != value)                                 \
    {                                                   \
      printf ("%s   %s  got 0x%016lX want 0x%016lX\n",  \
              header, regstr, callreg, value);          \
      header = "";                                      \
      ret = 0;                                          \
    }

  CHECK (calling_conventions_rbx, "rbx", calling_conventions_want_rbx);
  CHECK (calling_conventions_rbp, "rbp", calling_conventions_want_rbp);
  CHECK (calling_conventions_r12, "r12", calling_conventions_want_r12);
  CHECK (calling_conventions_r13, "r13", calling_conventions_want_r13);
  CHECK (calling_conventions_r14, "r14", calling_conventions_want_r14);
  CHECK (calling_conventions_r15, "r15", calling_conventions_want_r15);

  if (DIR_BIT (calling_conventions_rflags) != 0)
    {
      printf ("%s   rflags dir bit  got %d want 0\n",
              header, DIR_BIT (calling_conventions_rflags));
      header = "";
      ret = 0;
    }

  if ((calling_conventions_fenv.tag & 0xFFFF) != 0xFFFF)
    {
      printf ("%s   fpu tags  got 0x%lX want 0xFFFF\n",
              header, calling_conventions_fenv.tag & 0xFFFF);
      header = "";
      ret = 0;
    }

  return ret;
}
