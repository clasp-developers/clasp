divert(-1)

dnl  m4 macros for amd64 assembler.

dnl  Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2008, 2009 Free
dnl  Software Foundation, Inc.
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


dnl  Usage: CPUVEC_FUNCS_LIST
dnl
dnl  A list of the functions from gmp-impl.h x86 struct cpuvec_t, in the
dnl  order they appear in that structure.

define(CPUVEC_FUNCS_LIST,
``add_n',
`addmul_1',
`copyd',
`copyi',
`divexact_1',
`divexact_by3c',
`divrem_1',
`gcd_1',
`lshift',
`mod_1',
`mod_34lsub1',
`modexact_1c_odd',
`mul_1',
`mul_basecase',
`preinv_divrem_1',
`preinv_mod_1',
`rshift',
`sqr_basecase',
`sub_n',
`submul_1'')


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

define(`LEA',`
	mov	$1@GOTPCREL(%rip), $2
')


define(`DEF_OBJECT',
m4_assert_numargs_range(1,2)
`	RODATA
	ALIGN(ifelse($#,1,2,$2))
$1:
')

define(`END_OBJECT',
m4_assert_numargs(1)
`	SIZE(`$1',.-`$1')')


define(`R32',
	`ifelse($1,`%rax',`%eax',
		$1,`%rbx',`%ebx',
		$1,`%rcx',`%ecx',
		$1,`%rdx',`%edx',
		$1,`%rsi',`%esi',
		$1,`%rdi',`%edi',
		$1,`%rbp',`%ebp',
		$1,`%r8',`%r8d',
		$1,`%r9',`%r9d',
		$1,`%r10',`%r10d',
		$1,`%r11',`%r11d',
		$1,`%r12',`%r12d',
		$1,`%r13',`%r13d',
		$1,`%r14',`%r14d',
		$1,`%r15',`%r15d')')
define(`R8',
	`ifelse($1,`%rax',`%al',
		$1,`%rbx',`%bl',
		$1,`%rcx',`%cl',
		$1,`%rdx',`%dl',
		$1,`%rsi',`%sil',
		$1,`%rdi',`%dil',
		$1,`%rbp',`%bpl',
		$1,`%r8',`%r8b',
		$1,`%r9',`%r9b',
		$1,`%r10',`%r10b',
		$1,`%r11',`%r11b',
		$1,`%r12',`%r12b',
		$1,`%r13',`%r13b',
		$1,`%r14',`%r14b',
		$1,`%r15',`%r15b')')


dnl  Usage: CALL(funcname)
dnl

ifdef(`PIC',
  `define(`CALL',`call	GSYM_PREFIX`'$1@PLT')',
  `define(`CALL',`call	GSYM_PREFIX`'$1')')


define(`JUMPTABSECT', `.section	.data.rel.ro.local,"aw",@progbits')

divert`'dnl
