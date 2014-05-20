divert(-1)


dnl  Copyright 2000, 2002, 2003 Free Software Foundation, Inc.
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


dnl  ia64 assembler comments are C++ style "//" to the end of line.  gas
dnl  also accepts "#" as a comment, if it's the first non-blank on a line.
dnl
dnl  BSD m4 can't handle a multi-character comment like "//" (see notes in
dnl  mpn/asm-defs.m4).  For now the default "#" is left, but with care taken
dnl  not to put any macros after "foo#" (since of course they won't expand).


define(`ASM_START',
m4_assert_numargs(0)
`')


dnl  Called: PROLOGUE_cpu(GSYM_PREFIX`'foo)
dnl          EPILOGUE_cpu(GSYM_PREFIX`'foo)
dnl
dnl  32-byte alignment is used for the benefit of itanium-2, where the code
dnl  fetcher will only take 2 bundles from a 32-byte aligned target.  At
dnl  16mod32 it only reads 1 in the first cycle.  This might not make any
dnl  difference if the rotate buffers are full or there's other work holding
dnl  up execution, but we use 32-bytes to give the best chance of peak
dnl  throughput.
dnl
dnl  We can use .align here despite the gas bug noted in mpn/ia64/README,
dnl  since we're not expecting to execute across a PROLOGUE(), at least not
dnl  currently.

define(`PROLOGUE_cpu',
m4_assert_numargs(1)
	`
	.text
	.align	32
	.global	$1#
	.proc	$1#
$1:')

define(`EPILOGUE_cpu',
m4_assert_numargs(1)
	`
	.endp	$1#
')

define(`DATASTART',
	`dnl
	DATA
$1:')
define(`DATAEND',`dnl')

define(`ASM_END',`dnl')


dnl  Usage: ALIGN(bytes)
dnl
dnl  Emit a ".align" directive.  "bytes" is eval()ed, so can be an
dnl  expression.
dnl
dnl  This version overrides the definition in mpn/asm-defs.m4.  We suppress
dnl  any .align if the gas byte-swapped-nops bug was detected by configure
dnl  GMP_ASM_IA64_ALIGN_OK.

define(`ALIGN',
m4_assert_numargs(1)
m4_assert_defined(`IA64_ALIGN_OK')
`ifelse(IA64_ALIGN_OK,no,,
`.align	eval($1)')')


dnl  Usage: ASSERT([pr] [,code])
dnl
dnl  Require that the given predictate register is true after executing the
dnl  test code.  For example,
dnl
dnl         ASSERT(p6,
dnl         `       cmp.eq  p6,p0 = r3, r4')
dnl
dnl  If the predicate register argument is empty then nothing is tested, the
dnl  code is just executed.  This can be used for setups required by later
dnl  ASSERTs.  The code argument can be omitted to just test a predicate
dnl  with no special setup code.
dnl
dnl  For convenience, stops are inserted before and after the code emitted.

define(ASSERT,
m4_assert_numargs_range(1,2)
m4_assert_defined(`WANT_ASSERT')
`ifelse(WANT_ASSERT,1,
`	;;
ifelse(`$2',,,
`$2
	;;
')
ifelse(`$1',,,
`($1)	br	.LASSERTok`'ASSERT_label_counter ;;
	cmp.ne	p6,p6 = r0, r0	C illegal instruction
	;;
.LASSERTok`'ASSERT_label_counter:
define(`ASSERT_label_counter',eval(ASSERT_label_counter+1))
')
')')
define(`ASSERT_label_counter',1)


divert
