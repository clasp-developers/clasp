divert(-1)
dnl  m4 macros for AIX 64-bit assembly.

dnl  Copyright 2000, 2001, 2002, 2005, 2006 Free Software Foundation, Inc.
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

define(`ASM_START',
	`.machine	"ppc64"
	.toc')

dnl  Called: PROLOGUE_cpu(GSYM_PREFIX`'foo)
dnl          EPILOGUE_cpu(GSYM_PREFIX`'foo)
dnl
dnl  Don't want ELF style .size in the epilogue.

define(`PROLOGUE_cpu',
m4_assert_numargs(1)
	`
	.globl	$1
	.globl	.$1
	.csect	[DS], 3
$1:
	.llong	.$1, TOC[tc0], 0
	.csect	[PR]
	.align	4
.$1:')

define(`EPILOGUE_cpu',
m4_assert_numargs(1)
`')

define(`TOC_ENTRY', `')

define(`LEA',
m4_assert_numargs(2)
`define(`TOC_ENTRY',
`	.toc
..$2:	.tc	$2[TC], $2')'
	`ld	$1, ..$2(2)')

define(`EXTERN',
m4_assert_numargs(1)
`	.globl	$1')

define(`DEF_OBJECT',
m4_assert_numargs_range(1,2)
`	.csect	[RO], 3
	ALIGN(ifelse($#,1,2,$2))
$1:
')

define(`END_OBJECT',
m4_assert_numargs(1))

define(`CALL',
	`bl	.$1
	nop')

define(`ASM_END', `TOC_ENTRY')

divert
