/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    ffi_x86.c -- Nonportable component of the FFI
*/
/*
    Copyright (c) 2005, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <ecl/ecl.h>
#include <string.h>
#include <ecl/internal.h>

#error "This file is a placeholder for current development"

/*
 * Calling conventions for OS X under PowerPC/32bit architecture. The rules are
 * as follows:
 *
 * - Registers GPR3-GPR10 are used to pass 32-bit arguments. This includes
 *   integers and composed data structures which fit in the registers.
 * - Registers FPR1-FPR13 are used to pass float and double arguments.
 * - For each argument passed in a register, the same amount of memory is
 *   reserved in the stack.
 * - When the amount of registers is exhausted, the remaining arguments are
 *   passed in the stack.
 * - There is a difference between functions whose signature is known and those
 *   whose is not. In the second case, when passing float/double arguments,
 *   they are passed redundantly using a GPR, a FPR and the stack. In the
 *   former case, only the FPR or the stack is used.
 * - Since we do not allow functions with varargs (i.e "..." in C parlance), we
 *   do not care about the last case.
 *
 * Since we do not allow passing or receiving structures, we need not care
 * about it and the only rule is:
 *
 * - Returns arguments <= 32 bits are stored in GPR3
 * - Returns arguments <= 64 bits are shared between GPR3 and GPR4, for high
 *   and low bits, respectively.
 * - Floating point values are returned in FPR1.
 *
 * This information appears in "Mac OS X ABI Function Call Guide", from Apple
 * Developer's Documentation (April 2006).
 */

#define MAX_INT_REGISTERS 8
#define MAX_FP_REGISTERS 13

struct ecl_fficall_reg {
	long int registers[MAX_INT_REGISTERS];
	int int_registers_size;
	double fp_registers[MAX_FP_REGISTERS];
	int fp_registers_size;
};

struct ecl_fficall_reg *
ecl_fficall_prepare_extra(struct ecl_fficall_reg *registers)
{
	if (registers == 0) {
		registers = (struct ecl_fficall_reg *)cl_alloc_atomic(sizeof(*registers));
	}
	registers->int_registers_size = 0;
	registers->fp_registers_size = 0;
}

void
ecl_fficall_push_arg(union ecl_ffi_values *data, enum ecl_ffi_tag type)
{
	long i;
	struct ecl_fficall *fficall = cl_env.fficall;
	struct ecl_fficall_reg *registers = cl_env.fficall->registers;
	switch (type) {
	case ECL_FFI_CHAR: i = data->c;	goto INT;
	case ECL_FFI_UNSIGNED_CHAR: i = data->uc; goto INT;
	case ECL_FFI_BYTE: i = data->b; goto INT;
	case ECL_FFI_UNSIGNED_BYTE: i = data->ub; goto INT;
	case ECL_FFI_SHORT: i = data->s; goto INT;
	case ECL_FFI_UNSIGNED_SHORT: i = data->us; goto INT;
	case ECL_FFI_INT: i = data->i; goto INT;
	case ECL_FFI_UNSIGNED_INT: i = data->ui; goto INT;
	case ECL_FFI_LONG:
	case ECL_FFI_UNSIGNED_LONG:
	case ECL_FFI_POINTER_VOID:
	case ECL_FFI_CSTRING:
	case ECL_FFI_OBJECT:
		i = data->l;
	INT:
		if (registers->int_registers_size < MAX_INT_REGISTERS) {
			registers->registers[registers->int_registers_size++] = i;
		}
		ecl_fficall_align(sizeof(long));
		ecl_fficall_push_bytes(&i, sizeof(long));
		break;
	case ECL_FFI_DOUBLE:
		if (registers->fp_registers_size < MAX_FP_REGISTERS) {
			registers->fp_registers[registers->fp_registers_size++] = data->d;
			registers->int_registers_size += 2;
		}
		ecl_fficall_align(sizeof(long));
		ecl_fficall_push_bytes(&data->d, sizeof(double), sizeof(long));
		break;
	case ECL_FFI_FLOAT:
		if (registers->fp_registers_size < MAX_FP_REGISTERS) {
			registers->fp_registers[registers->fp_registers_size++] = data->f;
			registers->int_registers_size++;
		}
		ecl_fficall_align(sizeof(long));
		ecl_fficall_push_bytes(&data->f, sizeof(float), sizeof(long));
		break;
	case ECL_FFI_VOID:
		FEerror("VOID is not a valid argument type for a C function", 0);
	}
}

static void
ecl_fficall_do_execute(cl_index buf_size, void *stack, void *gpr, void *gpfr, void *f)
{
}

void
ecl_fficall_execute(void *_f_ptr, struct ecl_fficall *fficall, enum ecl_ffi_tag return_type)
{
	struct ecl_fficall_reg *registers = fficall->registers;
	long bufsize = fficall->buffer_size;
	char* buf = fficall->buffer;

	asm volatile (
	"mr	r5,%[bufsize]\n\t"	/* r5 = size of stack */
	"mr	r6,%[buf]\n\t"		/* r6 = origin of stack data */
	"mr	r17,%[registers]\n\t"	/* r17 = origin of integer registers */
	"mr	r16,%[fp_registers]\n\t"/* r16 = origin of fp registers */
	"mr	r15,%[fptr]\n\t"	/* r15 = _f_ptr */
	"mr	r29, r1\n\t"		/* r29 saves r1 */

	"subf	r13,r5,r1\n\t"
	"stwu	r13,-80(r13)\n\t"	/* r13 <- r1 - r5 - 80 */
	"mflr	r0\n\t"
	"stw	r0,8(r1)\n\t"
	"mr	r1,r13\n\t"		/* r1 <- r13 */

	"stwu	r14,24(r1)\n\t"		/* r14 <- begin of parameters */
	"cmpwi	cr0,r5,0\n\t"			/* copy r5 bytes from (r6) to (r14) */
	"ble	cr0,L3\n\t"
	"mtctr	r5\n"
"LX:	 lbz	r0,0(r6)\n\t"
	"addi	r6,r6,1\n\t"
	"stb	r0,0(r14)\n\t"
	"addi	r14,r14,1\n"
"L3:	 lfd	f1, 0(r16)\n\t"		/* load fp registers from (r16) */
	"lfd	f2, 8(r16)\n\t"
	"lfd	f3, 16(r16)\n\t"
	"lfd	f4, 24(r16)\n\t"
	"lfd	f5, 32(r16)\n\t"
	"lfd	f6, 40(r16)\n\t"
	"lfd	f7, 48(r16)\n\t"
	"lfd	f8, 56(r16)\n\t"
	"lfd	f9, 64(r16)\n\t"
	"lfd	f10, 72(r16)\n\t"
	"lfd	f11, 80(r16)\n\t"
	"lfd	f12, 88(r16)\n\t"
	"lfd	f13, 96(r16)\n\t"

	"lwz	r6, 16(r17)\n\t"	/* load int registers from (r17) */
	"lwz	r7, 20(r17)\n\t"
	"lwz	r8, 24(r17)\n\t"
	"lwz	r9, 28(r17)\n\t"
	"lwz	r10, 32(r17)\n\t"
	"lwz	r5, 8(r17)\n\t"
	"lwz	r4, 4(r17)\n\t"
	"lwz	r3, 0(r17)\n\t"

	"mtctr	r15\n\t"		/* call the function stored in r15 */
	"bctrl	\n\t"
	"mr	r1,r29\n\t"		/* restore stack and return pointer */
	"lwz	r0,8(r1)\n\t"
	"mtlr	r0\n\t"
	"stw	r3,0(r17)\n\t"		/* store function's output */
	"stw	r4,4(r17)\n\t"
	"stfd	f1,0(r16)\n\t"

        :: [bufsize] "r" (bufsize), [buf] "r" (buf), [registers] "r" (registers->registers),
	[fp_registers] "r" (registers->fp_registers), [fptr] "r" (_f_ptr)
	: "r5","r6","r17","r16","r29","r13","r14");


	void *data = registers->registers;
	if (return_type <= ECL_FFI_UNSIGNED_LONG) {
		fficall->output.i = *((unsigned long *)data);
	} else if (return_type == ECL_FFI_POINTER_VOID) {
		fficall->output.pv = *((void **)data);
	} else if (return_type == ECL_FFI_CSTRING) {
		fficall->output.pc = *((char *)data);
	} else if (return_type == ECL_FFI_OBJECT) {
		fficall->output.o = *((cl_object *)data);
	} else if (return_type == ECL_FFI_FLOAT) {
		fficall->output.f = registers->fp_registers[0];
	} else if (return_type == ECL_FFI_DOUBLE) {
		fficall->output.d = registers->fp_registers[0];
	}
}


void*
ecl_dynamic_callback_make(cl_object data, enum ecl_ffi_calling_convention cc_type)
{
	exit(0);
}
