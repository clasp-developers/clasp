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

#if !defined(HAVE_LIBFFI)

#define MAX_INT_REGISTERS 6
#define MAX_FP_REGISTERS 8

struct ecl_fficall_reg {
	long int_registers[MAX_INT_REGISTERS];
	int int_registers_size;
	double fp_registers[MAX_FP_REGISTERS];
	int fp_registers_size;
};

struct ecl_fficall_reg *
ecl_fficall_prepare_extra(struct ecl_fficall_reg *registers)
{
	if (registers == 0) {
		registers = ecl_alloc_atomic_align(sizeof(*registers), sizeof(long));
	}
	registers->int_registers_size = 0;
	registers->fp_registers_size = 0;
        return registers;
}

void
ecl_fficall_push_arg(union ecl_ffi_values *data, enum ecl_ffi_tag type)
{
	long i;
	struct ecl_fficall *fficall = cl_env.fficall;
	struct ecl_fficall_reg *registers = fficall->registers;
	switch (type) {
	case ECL_FFI_CHAR: i = data->c;	goto INT;
	case ECL_FFI_UNSIGNED_CHAR: i = data->uc; goto INT;
#ifdef ecl_uint8_t
        case ECL_FFI_INT8_T: i = data->i8; goto INT;
        case ECL_FFI_UINT8_T: i = data->u8; goto INT;
#endif
	case ECL_FFI_BYTE: i = data->b; goto INT;
	case ECL_FFI_UNSIGNED_BYTE: i = data->ub; goto INT;
#ifdef ecl_uint16_t
        case ECL_FFI_INT16_T: i = data->i16; goto INT;
        case ECL_FFI_UINT16_T: i = data->u16; goto INT;
#endif
	case ECL_FFI_SHORT: i = data->s; goto INT;
	case ECL_FFI_UNSIGNED_SHORT: i = data->us; goto INT;
#ifdef ecl_uint32_t
        case ECL_FFI_INT32_T: i = data->i32; goto INT;
        case ECL_FFI_UINT32_T: i = data->u32; goto INT;
#endif
	case ECL_FFI_INT: i = data->i; goto INT;
	case ECL_FFI_UNSIGNED_INT: i = data->ui; goto INT;
	case ECL_FFI_LONG:
	case ECL_FFI_UNSIGNED_LONG:
#ifdef ecl_uint64_t
        case ECL_FFI_INT64_T:
        case ECL_FFI_UINT64_T:
#endif
	case ECL_FFI_POINTER_VOID:
	case ECL_FFI_CSTRING:
	case ECL_FFI_OBJECT:
		i = data->l;
	INT:
		if (registers->int_registers_size < MAX_INT_REGISTERS) {
			registers->int_registers[registers->int_registers_size++] = i;
		} else {
			ecl_fficall_align(sizeof(long));
			ecl_fficall_push_bytes(&i, sizeof(long));
		}
		break;
	case ECL_FFI_DOUBLE:
		if (registers->fp_registers_size < MAX_FP_REGISTERS) {
			registers->fp_registers[registers->fp_registers_size++] = data->d;
		} else {
			ecl_fficall_align(sizeof(long));
			ecl_fficall_push_bytes(&data->d, sizeof(double));
		}
		break;
	case ECL_FFI_FLOAT:
		if (registers->fp_registers_size < MAX_FP_REGISTERS) {
			memset(&registers->fp_registers[registers->fp_registers_size], 0, sizeof(double));
			(*(float*)(&registers->fp_registers[registers->fp_registers_size++])) = (float)data->f;
		} else {
			i = 0;
			ecl_fficall_align(sizeof(long));
			ecl_fficall_push_bytes(&data->f, sizeof(float));
			ecl_fficall_push_bytes(&i, sizeof(float));
		}
		break;
	case ECL_FFI_VOID:
		FEerror("VOID is not a valid argument type for a C function", 0);
	}
}

void
ecl_fficall_execute(void *_f_ptr, struct ecl_fficall *fficall, enum ecl_ffi_tag return_type)
{
	struct ecl_fficall_reg *registers = fficall->registers;
	long bufsize = fficall->buffer_size;
	char* buf = fficall->buffer;
	char* stack_p;
	register void* f_ptr asm("r10");

	ecl_fficall_align(16);
	bufsize = fficall->buffer_size;
	f_ptr = _f_ptr;

	asm volatile (
	"mov	%%rsp, %0\n\t"
	"sub	%1, %%rsp\n\t"
	"mov	%2, %%rsi\n\t"
	"mov	%%rsp, %%rdi\n\t"
	"rep\n\t"
        "movsb\n\t"
        : "=a" (stack_p) : "c" (bufsize), "d" (buf) : "%rdi", "%rsi");

	asm volatile (
	"mov	(%%rax), %%rdi\n\t"
	"mov	0x08(%%rax), %%rsi\n\t"
	"mov	0x10(%%rax), %%rdx\n\t"
	"mov	0x18(%%rax), %%rcx\n\t"
	"mov	0x20(%%rax), %%r8\n\t"
	"mov	0x28(%%rax), %%r9\n\t"
	:: "a" (registers->int_registers));

	asm volatile (
	"movsd	(%%rax), %%xmm0\n\t"
	"movsd	0x08(%%rax), %%xmm1\n\t"
	"movsd	0x10(%%rax), %%xmm2\n\t"
	"movsd	0x18(%%rax), %%xmm3\n\t"
	"movsd	0x20(%%rax), %%xmm4\n\t"
	"movsd	0x28(%%rax), %%xmm5\n\t"
	"movsd	0x30(%%rax), %%xmm6\n\t"
	"movsd	0x38(%%rax), %%xmm7\n\t"
	:: "a" (registers->fp_registers));

	if (return_type <= ECL_FFI_UNSIGNED_LONG) {
		fficall->output.ul = ((unsigned long (*)())f_ptr)();
	} else if (return_type == ECL_FFI_POINTER_VOID) {
		fficall->output.pv = ((void * (*)())f_ptr)();
	} else if (return_type == ECL_FFI_CSTRING) {
		fficall->output.pc = ((char * (*)())f_ptr)();
	} else if (return_type == ECL_FFI_OBJECT) {
		fficall->output.o = ((cl_object (*)())f_ptr)();
	} else if (return_type == ECL_FFI_FLOAT) {
		fficall->output.f = ((float (*)())f_ptr)();
	} else if (return_type == ECL_FFI_DOUBLE) {
		fficall->output.d = ((double (*)())f_ptr)();
	}
#ifdef ecl_uint8_t
        else if (return_type == ECL_FFI_INT8_T) {
                fficall->output.i8 = ((ecl_int8_t (*)())f_ptr)();
	} else if (return_type == ECL_FFI_UINT16_T) {
                fficall->output.u8 = ((ecl_uint8_t (*)())f_ptr)();
	}
#endif
#ifdef ecl_uint16_t
        else if (return_type == ECL_FFI_INT16_T) {
                fficall->output.i16 = ((ecl_int16_t (*)())f_ptr)();
	} else if (return_type == ECL_FFI_UINT16_T) {
                fficall->output.u16 = ((ecl_uint16_t (*)())f_ptr)();
	}
#endif
#ifdef ecl_uint32_t
        else if (return_type == ECL_FFI_INT32_T) {
                fficall->output.i32 = ((ecl_int32_t (*)())f_ptr)();
	} else if (return_type == ECL_FFI_UINT32_T) {
                fficall->output.u32 = ((ecl_uint32_t (*)())f_ptr)();
	}
#endif
#ifdef ecl_uint64_t
        else if (return_type == ECL_FFI_INT64_T) {
                fficall->output.i64 = ((ecl_int64_t (*)())f_ptr)();
	} else if (return_type == ECL_FFI_UINT32_T) {
                fficall->output.u64 = ((ecl_uint64_t (*)())f_ptr)();
	}
#endif
#ifdef ecl_long_long_t
        else if (return_type == ECL_FFI_LONG_LONG) {
                fficall->output.ll = ((ecl_long_long_t (*)())f_ptr)();
	} else if (return_type == ECL_FFI_UNSIGNED_LONG_LONG) {
                fficall->output.ull = ((ecl_ulong_long_t (*)())f_ptr)();
	}
#endif
        else {
		((void (*)())f_ptr)();
	}

	asm volatile ("mov %0,%%rsp" :: "a" (stack_p));
}

static void
ecl_dynamic_callback_execute(long i1, long i2, long i3, long i4, long i5, long i6,
			     double f1, double f2, double f3, double f4,
			     double f5, double f6, double f7, double f8,
			     cl_object cbk_info, char *arg_buffer)
{
	cl_object fun, rtype, argtypes;
	cl_object result;
	cl_index i, size, i_reg_index, f_reg_index;
	union ecl_ffi_values output;
	enum ecl_ffi_tag tag;
	long i_reg[MAX_INT_REGISTERS];
	double f_reg[MAX_FP_REGISTERS];
	cl_env_ptr env = ecl_process_env();

	ECL_BUILD_STACK_FRAME(env, frame, aux);

	fun = CAR(cbk_info);
	rtype = CADR(cbk_info);
	argtypes = CADDR(cbk_info);

	i_reg_index = f_reg_index = 0;
	i_reg[0] = i1;
	i_reg[1] = i2;
	i_reg[2] = i3;
	i_reg[3] = i4;
	i_reg[4] = i5;
	i_reg[5] = i6;
	f_reg[0] = f1;
	f_reg[1] = f2;
	f_reg[2] = f3;
	f_reg[3] = f4;
	f_reg[4] = f5;
	f_reg[5] = f6;
	f_reg[6] = f7;
	f_reg[7] = f8;

	arg_buffer += 2*sizeof(void*); /* Skip return address and base pointer */
	for (i=0; !ecl_endp(argtypes); argtypes = CDR(argtypes), i++) {
		tag = ecl_foreign_type_code(CAR(argtypes));
		size = ecl_fixnum(si_size_of_foreign_elt_type(CAR(argtypes)));
		if (tag <= ECL_FFI_OBJECT) {
			if (i_reg_index < MAX_INT_REGISTERS)
				result = ecl_foreign_data_ref_elt(&i_reg[i_reg_index++], tag);
			else
				goto ARG_FROM_STACK;
		} else if (tag <= ECL_FFI_DOUBLE) {
			if (f_reg_index < MAX_FP_REGISTERS)
				result = ecl_foreign_data_ref_elt(&f_reg[f_reg_index++], tag);
			else
				goto ARG_FROM_STACK;
		} else {
ARG_FROM_STACK:
			result = ecl_foreign_data_ref_elt(arg_buffer, tag);
			{
				int mask = 7;
				int sp = (size + mask) & ~mask;
				arg_buffer += (sp);
			}
		}
		ecl_stack_frame_push(frame, result);
	}

	result = ecl_apply_from_stack_frame(frame, fun);
	ecl_stack_frame_close(frame);

	tag = ecl_foreign_type_code(rtype);
	memset(&output, 0, sizeof(output));
	ecl_foreign_data_set_elt(&output, tag, result);

	switch (tag) {
	case ECL_FFI_CHAR: i = output.c; goto INT;
	case ECL_FFI_UNSIGNED_CHAR: i = output.uc; goto INT;
	case ECL_FFI_BYTE: i = output.b; goto INT;
	case ECL_FFI_UNSIGNED_BYTE: i = output.ub; goto INT;
#ifdef ecl_uint8_t
        case ECL_FFI_INT8_T: i = output.i8; goto INT;
        case ECL_FFI_UINT8_T: i = output.u8; goto INT;
#endif
#ifdef ecl_uint16_t
        case ECL_FFI_INT16_T: i = output.i16; goto INT;
        case ECL_FFI_UINT16_T: i = output.u16; goto INT;
#endif
	case ECL_FFI_SHORT: i = output.s; goto INT;
	case ECL_FFI_UNSIGNED_SHORT: i = output.us; goto INT;
#ifdef ecl_uint32_t
        case ECL_FFI_INT32_T: i = output.i32; goto INT;
        case ECL_FFI_UINT32_T: i = output.u32; goto INT;
#endif
	case ECL_FFI_POINTER_VOID:
	case ECL_FFI_OBJECT:
	case ECL_FFI_CSTRING:
	case ECL_FFI_INT:
	case ECL_FFI_UNSIGNED_INT:
	case ECL_FFI_LONG:
	case ECL_FFI_UNSIGNED_LONG:
#ifdef ecl_uint64_t
        case ECL_FFI_INT64_T:
        case ECL_FFI_UINT64_T:
#endif
		i = output.i;
INT:
		{
		register long eax asm("rax");
		eax = i;
		}
		return;
	case ECL_FFI_DOUBLE: {
		{
		asm("movsd (%0),%%xmm0" :: "a" (&output.d));
		}
		return;
	}
	case ECL_FFI_FLOAT: {
		{
		asm("movss (%0),%%xmm0" :: "a" (&output.f));
		}
		return;
	}
	case ECL_FFI_VOID:
		return;
	}
}

void*
ecl_dynamic_callback_make(cl_object data, enum ecl_ffi_calling_convention cc_type)
{
	/*
	 *	push    %rbp                    55
	 *	push    %rsp                    54
	 *	mov     <addr64>,%rax           48 b8 <addr64>
	 *	push    %rax                    50
	 *	mov     <addr64>,%rax           48 b8 <addr64>
	 *	callq   *%rax                   48 ff d0
	 *	pop     %rcx                    59
	 *	pop     %rcx                    59
	 *	pop     %rbp                    5d
	 *	ret                             c3
	 *	nop				90
	 *	nop				90
	 */
	char *buf = (char*)ecl_alloc_atomic_align(sizeof(char)*32, 8);
	*(char*) (buf+0)  = 0x55;
	*(char*) (buf+1)  = 0x54;
	*(short*)(buf+2)  = 0xb848;
	*(long*) (buf+4)  = (long)data;
	*(char*) (buf+12) = 0x50;
	*(short*)(buf+13) = 0xb848;
	*(long*) (buf+15) = (long)ecl_dynamic_callback_execute;
	*(int*)  (buf+23) = (int)0x00d0ff48;	/* leading null byte is overwritten */
	*(char*) (buf+26) = 0x59;
	*(char*) (buf+27) = 0x59;
	*(char*) (buf+28) = 0x5d;
	*(char*) (buf+29) = 0xc3;
	*(short*)(buf+30) = 0x9090;

	return buf;
}

#endif
