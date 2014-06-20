divert(-1)
dnl  Copyright 2007 Free Software Foundation, Inc.
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

dnl  Usage LEA(symbol,reg)
dnl
dnl  FIXME: Only handles one symbol per assembly file because of the
dnl  way EPILOGUE_cpu is handled.

define(`LEA',`
define(`EPILOGUE_cpu',
`	L(movl_eip_`'substr($2,1)):
	movl	(%esp), $2
	ret_internal
	.section __IMPORT,__pointers,non_lazy_symbol_pointers
L($1`'$non_lazy_ptr):
	.indirect_symbol $1
	.long	 0
')
	call	L(movl_eip_`'substr($2,1))
	movl	L($1`'$non_lazy_ptr)-.($2), $2
')

divert`'dnl
