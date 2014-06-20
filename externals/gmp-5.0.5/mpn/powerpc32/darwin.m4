divert(-1)
dnl  m4 macros for Mac OS 32-bit assembly.

dnl  Copyright 2005, 2006 Free Software Foundation, Inc.
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

define(`ASM_START',`')

dnl  Called: PROLOGUE_cpu(GSYM_PREFIX`'foo)
dnl          EPILOGUE_cpu(GSYM_PREFIX`'foo)
dnl

define(`PROLOGUE_cpu',
m4_assert_numargs(1)
`	.text
	.globl	$1
	.align	3
$1:')

define(`EPILOGUE_cpu',
m4_assert_numargs(1))


dnl  LEA -- Load Effective Address.

define(`LEA',
m4_assert_numargs(2)
`ifdef(`PIC',
`	mflr	r0			C save return address
	bcl	20, 31, 1f
1:	mflr	$1
	addis	$1, $1, ha16($2-1b)
	la	$1, lo16($2-1b)($1)
	mtlr	r0			C restore return address
',`
	lis	$1, ha16($2)
	la	$1, lo16($2)($1)
')')

define(`LEAL',
m4_assert_numargs(2)
`LEA($1,$2)')


define(`EXTERN',
m4_assert_numargs(1)
`dnl')

define(`DEF_OBJECT',
m4_assert_numargs_range(1,2)
`	.const
	ALIGN(ifelse($#,1,2,$2))
$1:
')

define(`END_OBJECT',
m4_assert_numargs(1))

define(`ASM_END', `dnl')

ifdef(`PIC',`
define(`PIC_SLOW')')

divert
