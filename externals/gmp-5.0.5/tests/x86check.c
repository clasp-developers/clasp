/* x86 calling conventions checking. */

/*
Copyright 2000, 2001 Free Software Foundation, Inc.

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

#include <stdio.h>
#include "gmp.h"
#include "gmp-impl.h"
#include "tests.h"


/* temporaries */
int  calling_conventions_save_ebx;
int  calling_conventions_save_esi;
int  calling_conventions_save_edi;
int  calling_conventions_save_ebp;
int  calling_conventions_retaddr;
int  calling_conventions_retval;

/* values to check */
struct {
  unsigned  control;
  unsigned  status;
  unsigned  tag;
  unsigned  other[4];
} calling_conventions_fenv;
int  calling_conventions_ebx;
int  calling_conventions_esi;
int  calling_conventions_edi;
int  calling_conventions_ebp;
int  calling_conventions_eflags;

/* expected values, as per x86call.asm */
#define VALUE_EBX   0x01234567
#define VALUE_ESI   0x89ABCDEF
#define VALUE_EDI   0xFEDCBA98
#define VALUE_EBP   0x76543210

#define DIR_BIT(eflags)   (((eflags) & (1<<10)) != 0)


/* Return 1 if ok, 0 if not */

int
calling_conventions_check (void)
{
  const char  *header = "Violated calling conventions:\n";
  int  ret = 1;

#define CHECK(callreg, regstr, value)                   \
  if (callreg != value)                                 \
    {                                                   \
      printf ("%s   %s  got 0x%08X want 0x%08X\n",      \
              header, regstr, callreg, value);          \
      header = "";                                      \
      ret = 0;                                          \
    }

  CHECK (calling_conventions_ebx, "ebx", VALUE_EBX);
  CHECK (calling_conventions_esi, "esi", VALUE_ESI);
  CHECK (calling_conventions_edi, "edi", VALUE_EDI);
  CHECK (calling_conventions_ebp, "ebp", VALUE_EBP);

  if (DIR_BIT (calling_conventions_eflags) != 0)
    {
      printf ("%s   eflags dir bit  got %d want 0\n",
              header, DIR_BIT (calling_conventions_eflags));
      header = "";
      ret = 0;
    }

  if ((calling_conventions_fenv.tag & 0xFFFF) != 0xFFFF)
    {
      printf ("%s   fpu tags  got 0x%X want 0xFFFF\n",
              header, calling_conventions_fenv.tag & 0xFFFF);
      header = "";
      ret = 0;
    }

  return ret;
}
