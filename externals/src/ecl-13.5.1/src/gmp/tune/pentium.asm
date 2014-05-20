dnl  x86 pentium time stamp counter access routine.

dnl  Copyright 1999, 2000, 2005 Free Software Foundation, Inc.
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


include(`../config.m4')


C void speed_cyclecounter (unsigned p[2]);
C
C Get the pentium rdtsc cycle counter, storing the least significant word in
C p[0] and the most significant in p[1].
C
C cpuid is used to serialize execution.  On big measurements this won't be
C significant but it may help make small single measurements more accurate.

	.text
	ALIGN(8)

defframe(PARAM_P,4)

PROLOGUE(speed_cyclecounter)
deflit(`FRAME',0)
	pushl	%ebx
FRAME_pushl()
	xorl	%eax, %eax
	cpuid
	rdtsc
	movl	PARAM_P, %ebx
	movl	%eax, (%ebx)
	movl	%edx, 4(%ebx)
	popl	%ebx
	ret
EPILOGUE()
