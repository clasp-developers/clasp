divert(-1)

dnl  m4 macros for ARM assembler.

dnl  Copyright 2001 Free Software Foundation, Inc.
dnl
dnl  This file is part of the GNU MP Library.
dnl
dnl  The GNU MP Library is free software; you can redistribute it and/or
dnl  modify it under the terms of the GNU Lesser General Public License as
dnl  published by the Free Software Foundation; either version 2.1 of the
dnl  License, or (at your option) any later version.
dnl
dnl  The GNU MP Library is distributed in the hope that it will be useful,
dnl  but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl  Lesser General Public License for more details.
dnl
dnl  You should have received a copy of the GNU Lesser General Public
dnl  License along with the GNU MP Library; see the file COPYING.LIB.  If
dnl  not, write to the Free Software Foundation, Inc., 51 Franklin Street,
dnl  Fifth Floor, Boston, MA 02110-1301, USA.


dnl  Standard commenting is with @, the default m4 # is for constants and we
dnl  don't want to disable macro expansions in or after them.

changecom(@)


dnl  APCS register names.

deflit(a1,r0)
deflit(a2,r1)
deflit(a3,r2)
deflit(a4,r3)
deflit(v1,r4)
deflit(v2,r5)
deflit(v3,r6)
deflit(v4,r7)
deflit(v5,r8)
deflit(v6,r9)
deflit(sb,r9)
deflit(v7,r10)
deflit(sl,r10)
deflit(fp,r11)
deflit(ip,r12)
deflit(sp,r13)
deflit(lr,r14)
deflit(pc,r15)

divert
