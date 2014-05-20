dnl  HPPA 64-bit time stamp counter access routine.

dnl  Copyright 2000, 2002, 2005 Free Software Foundation, Inc.
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

dnl void speed_cyclecounter (unsigned p[2]);
dnl
dnl Get the HPPA interval timer.

	.level 2.0w
PROLOGUE(speed_cyclecounter)
	mfctl	%cr16,%r28
	stw	%r28,0(0,%r26)		; low word
	extrd,u	%r28,31,32,%r28
	bve	(%r2)
	stw	%r28,4(0,%r26)		; high word
EPILOGUE(speed_cyclecounter)
