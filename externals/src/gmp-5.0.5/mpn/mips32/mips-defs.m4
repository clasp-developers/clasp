divert(-1)

dnl  m4 macros for MIPS assembly code (both 32-bit and 64-bit).


dnl  Copyright 2000, 2001, 2002 Free Software Foundation, Inc.
dnl
dnl  This file is part of the GNU MP Library.
dnl
dnl  The GNU MP Library is free software; you can redistribute it and/or modify
dnl  it under the terms of the GNU Lesser General Public License as published
dnl  by the Free Software Foundation; either version 3 of the License, or (at
dnl  your option) any later version.
dnl
dnl  The GNU MP Library is distributed in the hope that it will be useful, but
dnl  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
dnl  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
dnl  License for more details.
dnl
dnl  You should have received a copy of the GNU Lesser General Public License
dnl  along with the GNU MP Library.  If not, see http://www.gnu.org/licenses/.


dnl  Usage: ASM_START()
define(`ASM_START',
m4_assert_numargs(0)
`	.set noreorder
	.set nomacro')

dnl  Usage: X(value)
define(`X',
m4_assert_numargs(1)
`0x$1')

dnl  Called: PROLOGUE_cpu(GSYM_PREFIX`'foo)
dnl          EPILOGUE_cpu(GSYM_PREFIX`'foo)

define(`PROLOGUE_cpu',
m4_assert_numargs(1)
`	.text
	.align	4
	.globl	$1
	.ent	$1
$1:')

define(`EPILOGUE_cpu',
m4_assert_numargs(1)
`	.end	$1')


dnl  Usage: r0 ... r31
dnl         f0 ... f31
dnl
dnl  Map register names r0 to $0, and f0 to $f0, etc.
dnl
dnl  defreg() is used to protect the $ in $0 (otherwise it would represent a
dnl  macro argument).  Double quoting is used to protect the f0 in $f0
dnl  (otherwise it would be an infinite recursion).

forloop(i,0,31,`defreg(`r'i,$i)')
forloop(i,0,31,`deflit(`f'i,``$f''i)')


dnl  Usage: ASM_END()
define(`ASM_END',
m4_assert_numargs(0)
)

divert
