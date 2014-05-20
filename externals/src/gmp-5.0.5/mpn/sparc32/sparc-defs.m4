divert(-1)

dnl  m4 macros for SPARC assembler (32 and 64 bit).


dnl  Copyright 2002, 2011 Free Software Foundation, Inc.
dnl
dnl  This file is part of the GNU MP Library.
dnl
dnl  The GNU MP Library is free software; you can redistribute it and/or
dnl  modify it under the terms of the GNU Lesser General Public License as
dnl  published by the Free Software Foundation; either version 3 of the
dnl  License, or (at your option) any later version.
dnl
dnl  The GNU MP Library is distributed in the hope that it will be useful,
dnl  but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl  Lesser General Public License for more details.
dnl
dnl  You should have received a copy of the GNU Lesser General Public License
dnl  along with the GNU MP Library.  If not, see http://www.gnu.org/licenses/.


changecom(;)	dnl cannot use default # since that's used in REGISTER decls


dnl  Usage: REGISTER(reg,attr)
dnl
dnl  Give a ".register reg,attr" directive, if the assembler supports it.
dnl  HAVE_REGISTER comes from the GMP_ASM_SPARC_REGISTER configure test.

define(REGISTER,
m4_assert_numargs(2)
m4_assert_defined(`HAVE_REGISTER')
`ifelse(HAVE_REGISTER,yes,
`.register `$1',`$2'')')


divert
