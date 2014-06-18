divert(-1)

dnl  m4 macros for alpha assembler (everywhere except unicos).


dnl  Copyright 2000, 2002, 2003, 2004 Free Software Foundation, Inc.
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


dnl  Usage: ASM_START()
define(`ASM_START',
m4_assert_numargs(0)
`	.set noreorder
	.set noat')

dnl  Usage: X(value)
define(`X',
m4_assert_numargs(1)
`0x$1')

dnl  Usage: FLOAT64(label,value)
define(`FLOAT64',
m4_assert_numargs(2)
`	.align	3
$1:	.t_floating $2')


dnl  Called: PROLOGUE_cpu(GSYM_PREFIX`'foo[,gp|noalign])
dnl          EPILOGUE_cpu(GSYM_PREFIX`'foo)

define(`PROLOGUE_cpu',
m4_assert_numargs_range(1,2)
`ifelse(`$2',gp,,
`ifelse(`$2',noalign,,
`ifelse(`$2',,,`m4_error(`Unrecognised PROLOGUE parameter
')')')')dnl
	.text
ifelse(`$2',noalign,,`	ALIGN(16)')
	.globl	$1
	.ent	$1
$1:
ifelse(`$2',gp,`	ldgp	r29,0(r27)')
	.frame r30,0,r26
	.prologue ifelse(`$2',gp,1,0)')

define(`EPILOGUE_cpu',
m4_assert_numargs(1)
`	.end	$1')


dnl  Usage: LDGP(dst,src)
dnl
dnl  Emit an "ldgp dst,src", but only if the system uses a GOT.

define(LDGP,
m4_assert_numargs(2)
`ldgp	`$1', `$2'')


dnl  Usage: EXTERN(variable_name)
define(`EXTERN',
m4_assert_numargs(1)
)

dnl  Usage: r0 ... r31
dnl         f0 ... f31
dnl
dnl  Map register names r0 to $0, and f0 to $f0, etc.
dnl  This is needed on all systems but Unicos
dnl
dnl  defreg() is used to protect the $ in $0 (otherwise it would represent a
dnl  macro argument).  Double quoting is used to protect the f0 in $f0
dnl  (otherwise it would be an infinite recursion).

forloop(i,0,31,`defreg(`r'i,$i)')
forloop(i,0,31,`deflit(`f'i,``$f''i)')


dnl  Usage: DATASTART(name)
dnl         DATAEND()

define(`DATASTART',
m4_assert_numargs(1)
`	DATA
$1:')
define(`DATAEND',
m4_assert_numargs(0)
)

dnl  Load a symbolic address into a register
define(`LEA',
m4_assert_numargs(2)
`lda   $1,  $2')

dnl  Usage: ASM_END()
define(`ASM_END',
m4_assert_numargs(0)
)

divert
