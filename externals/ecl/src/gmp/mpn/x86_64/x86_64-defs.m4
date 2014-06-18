divert(-1)

dnl  m4 macros for amd64 assembler.

dnl  Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005 Free Software
dnl  Foundation, Inc.
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


dnl  Notes:
dnl
dnl  The 32-bit mode x86/x86-defs.m4 has various 32bit-isms, like the
dnl  profiling calls, so it seems cleanest to start a fresh set of defines
dnl  for 64-bit mode.


dnl  Called: PROLOGUE_cpu(GSYM_PREFIX`'foo)
dnl
dnl  In the amd64 code we use explicit TEXT and ALIGN() calls in the code,
dnl  since different alignments are wanted in various circumstances.  So for
dnl  instance,
dnl
dnl                  TEXT
dnl                  ALIGN(16)
dnl          PROLOGUE(mpn_add_n)
dnl                  ...
dnl          EPILOGUE()

define(`PROLOGUE_cpu',
m4_assert_numargs(1)
`	GLOBL	$1
	TYPE($1,`function')
$1:
')


dnl  Usage: ASSERT([cond][,instructions])
dnl
dnl  If WANT_ASSERT is 1, output the given instructions and expect the given
dnl  flags condition to then be satisfied.  For example,
dnl
dnl         ASSERT(ne, `cmpq %rax, %rbx')
dnl
dnl  The instructions can be omitted to just assert a flags condition with
dnl  no extra calculation.  For example,
dnl
dnl         ASSERT(nc)
dnl
dnl  When `instructions' is not empty, a pushfq/popfq is added for
dnl  convenience to preserve the flags, but the instructions themselves must
dnl  preserve any registers that matter.
dnl
dnl  The condition can be omitted to just output the given instructions when
dnl  assertion checking is wanted.  In this case the pushf/popf is omitted.
dnl  For example,
dnl
dnl         ASSERT(, `movq %rax, VAR_KEEPVAL')

define(ASSERT,
m4_assert_numargs_range(1,2)
m4_assert_defined(`WANT_ASSERT')
`ifelse(WANT_ASSERT,1,
`ifelse(`$1',,
`	$2',
`ifelse(`$2',,,
`	pushfq')
	$2
	j`$1'	L(ASSERT_ok`'ASSERT_counter)
	ud2	C assertion failed
L(ASSERT_ok`'ASSERT_counter):
ifelse(`$2',,,`	popfq')
define(`ASSERT_counter',incr(ASSERT_counter))')')')

define(ASSERT_counter,1)


divert`'dnl
