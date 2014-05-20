divert(-1)
dnl  m4 macros for powerpc32 GNU/Linux assembly.

dnl  Copyright 2003, 2005, 2006 Free Software Foundation, Inc.
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

define(`ASM_START',`')

dnl  Called: PROLOGUE_cpu(GSYM_PREFIX`'foo)
dnl          EPILOGUE_cpu(GSYM_PREFIX`'foo)
dnl

define(`PROLOGUE_cpu',
m4_assert_numargs(1)
	`
	.section	".text"
	.align	3
	.globl	$1
	.type	$1, @function
$1:')

define(`EPILOGUE_cpu',
m4_assert_numargs(1)
`	.size	$1, .-$1')

define(`LEA',
m4_assert_numargs(2)
`ifdef(`PIC',`
	mflr	r0
	bl	_GLOBAL_OFFSET_TABLE_@local-4
	mflr	$1
	mtlr	r0
	lwz	$1, $2@got($1)
',`
	lis	$1, $2@ha
	la	$1, $2@l($1)
')')

define(`EXTERN',
m4_assert_numargs(1)
`dnl')

define(`DEF_OBJECT',
m4_assert_numargs_range(1,2)
`
	.section	.rodata
	ALIGN(ifelse($#,1,2,$2))
	.type	$1, @object
$1:
')

define(`END_OBJECT',
m4_assert_numargs(1)
`	.size	$1, .-$1')

define(`ASM_END', `dnl')

ifdef(`PIC',`
define(`PIC_SLOW')')

dnl  64-bit "long long" parameters are put in an even-odd pair, skipping an
dnl  even register if that was in turn.  I wish somebody could explain why that
dnl  is a good idea.
define(`BROKEN_LONGLONG_PARAM')

divert
