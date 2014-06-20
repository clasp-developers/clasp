divert(-1)
dnl  Copyright 2008 Free Software Foundation, Inc.
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

define(`DARWIN')

define(`LEA',`
	lea	$1(%rip), $2
')

dnl  Usage: CALL(funcname)
dnl
dnl  Simply override the definition in x86_64-defs.m4.

define(`CALL',`call	GSYM_PREFIX`'$1')


define(`JUMPTABSECT', `DATA')

divert`'dnl
