divert(-1)
dnl  m4 macros for powerpc64 GNU/Linux assembly.

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
	.globl	$1
	.globl	.$1
	.section	".opd","aw"
	.align	3
$1:
	.llong	.$1, .TOC.@tocbase, 0
	.size	$1, 24
	.type	.$1, @function
	.section	".text"
	.align	4
.$1:')

define(`EPILOGUE_cpu',
m4_assert_numargs(1)
`	.size	.$1, .-.$1')

define(`TOC_ENTRY', `')

define(`LEA',
m4_assert_numargs(2)
`define(`TOC_ENTRY',
`	.section	".toc", "aw"
..$2:	.tc	$2[TC], $2')'
	`ld	$1, ..$2@toc(2)')

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

define(`CALL',
	`bl	GSYM_PREFIX`'$1
	nop')

define(`ASM_END', `TOC_ENTRY')

divert
