/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    ffi.c -- User defined data types and foreign functions interface.
*/
/*
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <string.h>
#define ECL_INCLUDE_FFI_H
#include <ecl/ecl.h>
#include <ecl/internal.h>

static const cl_object ecl_aet_to_ffi_table[ecl_aet_bc+1] = {
	@':void', /* ecl_aet_object */
	@':float', /* ecl_aet_df */
	@':double', /* ecl_aet_df */
	@':void', /* ecl_aet_bit */
#if ECL_FIXNUM_BITS == 32 && defined(ecl_uint32_t)
        @':int32-t', /* ecl_aet_fix */
        @':uint32-t', /* ecl_aet_index */
#else
# if ECL_FIXNUM_BITS == 64 && defined(ecl_uint64_t)
        @':int64-t', /* ecl_aet_fix */
        @':uint64-t', /* ecl_aet_index */
# else
        @':void', /* ecl_aet_fix */
        @':void', /* ecl_aet_index */
# endif
#endif
	@':uint8-t', /* ecl_aet_b8 */
        @':int8-t', /* ecl_aet_i8 */
#ifdef ecl_uint16_t
        @':uint16-t', /* ecl_aet_b16 */
        @':int16-t', /* ecl_aet_i16 */
#endif
#ifdef ecl_uint32_t
        @':uint32-t', /* ecl_aet_b32 */
        @':int32-t', /* ecl_aet_i32 */
#endif
#ifdef ecl_uint64_t
        @':uint64-t', /* ecl_aet_b64 */
        @':int64-t', /* ecl_aet_i64 */
#endif
#ifdef ECL_UNICODE
# ifdef ecl_int32_t
	@':int32-t', /* ecl_aet_ch */
# else
        @':void', /* ecl_aet_ch */
# endif
#endif
	@':char' /* ecl_aet_bc */
};

#define AUX_PTR(type) \
	((struct { char a[1]; union { type c[1]; char d[sizeof(type)]; } b; } *)0)
#ifdef __GNUC__
typedef struct {
	cl_object name;
	cl_index size;
	cl_index alignment;
} ecl_foreign_type_record;
# define ALIGNMENT(tag) (ecl_foreign_type_table[tag].alignment)
# define FFI_DESC(symbol,type)			\
  {symbol, sizeof(type), (AUX_PTR(type)->b.d - AUX_PTR(type)->a)}
#else
typedef struct {
	cl_object name;
	cl_index size;
	char *d, *a;
} ecl_foreign_type_record;
#define ALIGNMENT(tag) (ecl_foreign_type_table[tag].d - ecl_foreign_type_table[tag].a)
#define AUX_PTR(type) \
	((struct { char a[1]; union { type c[1]; char d[sizeof(type)]; } b; } *)0)
#define FFI_DESC(symbol,type) \
  {symbol, sizeof(type), AUX_PTR(type)->b.d, AUX_PTR(type)->a}
#endif

static const ecl_foreign_type_record
ecl_foreign_type_table[] = {
	FFI_DESC(@':char', char),
	FFI_DESC(@':unsigned-char', unsigned char),
	FFI_DESC(@':byte', ecl_int8_t),
        FFI_DESC(@':unsigned-byte', ecl_uint8_t),
	FFI_DESC(@':short', short),
	FFI_DESC(@':unsigned-short', unsigned short),
	FFI_DESC(@':int', int),
	FFI_DESC(@':unsigned-int', unsigned int),
	FFI_DESC(@':long', long),
	FFI_DESC(@':unsigned-long', unsigned long),
#ifdef ecl_uint8_t
        FFI_DESC(@':int8-t', ecl_int8_t),
        FFI_DESC(@':uint8-t', ecl_uint8_t),
#endif
#ifdef ecl_uint16_t
        FFI_DESC(@':int16-t', ecl_int16_t),
        FFI_DESC(@':uint16-t', ecl_uint16_t),
#endif
#ifdef ecl_uint32_t
        FFI_DESC(@':int32-t', ecl_int32_t),
        FFI_DESC(@':uint32-t', ecl_uint32_t),
#endif
#ifdef ecl_uint64_t
        FFI_DESC(@':int64-t', ecl_int64_t),
        FFI_DESC(@':uint64-t', ecl_uint64_t),
#endif
#ifdef ecl_long_long_t
        FFI_DESC(@':long-long', long long),
        FFI_DESC(@':unsigned-long-long', unsigned long long),
#endif
	FFI_DESC(@':pointer-void', void *),
	FFI_DESC(@':cstring', char *),
	FFI_DESC(@':object', cl_object),
	FFI_DESC(@':float', float),
	FFI_DESC(@':double', double),
	{@':void', 0, 0}
};

#ifdef ECL_DYNAMIC_FFI
static const cl_object ecl_foreign_cc_table[] = {
	@':cdecl',
	@':stdcall'
};
#endif

#ifdef HAVE_LIBFFI
static struct {
        const cl_object symbol;
        ffi_abi abi;
} ecl_foreign_cc_table[] = {
        {@':default', FFI_DEFAULT_ABI},
#ifdef X86_WIN32
	{@':cdecl', FFI_SYSV},
	{@':sysv', FFI_SYSV},
	{@':stdcall', FFI_STDCALL},
#elif defined(X86_WIN64)
	{@':win64', FFI_WIN64},
#elif defined(X86_ANY) || defined(X86) || defined(X86_64)
	{@':cdecl', FFI_SYSV},
	{@':sysv', FFI_SYSV},
	{@':unix64', FFI_UNIX64},
#endif
};

static ffi_type *ecl_type_to_libffi_type[] = {
	&ffi_type_schar, /*@':char',*/
	&ffi_type_uchar, /*@':unsigned-char',*/
	&ffi_type_sint8, /*@':byte',*/
	&ffi_type_uint8, /*@':unsigned-byte',*/
	&ffi_type_sshort, /*@':short',*/
	&ffi_type_ushort, /*@':unsigned-short',*/
	&ffi_type_sint, /*@':int',*/
	&ffi_type_uint, /*@':unsigned-int',*/
	&ffi_type_slong, /*@':long',*/
	&ffi_type_ulong, /*@':unsigned-long',*/
#ifdef ecl_uint8_t
        &ffi_type_sint8, /*@':int8-t',*/
        &ffi_type_uint8, /*@':uint8-t',*/
#endif
#ifdef ecl_uint16_t
        &ffi_type_sint16, /*@':int16-t',*/
        &ffi_type_uint16, /*@':uint16-t',*/
#endif
#ifdef ecl_uint32_t
        &ffi_type_sint32, /*@':int32-t',*/
        &ffi_type_uint32, /*@':uint32-t',*/
#endif
#ifdef ecl_uint64_t
        &ffi_type_sint64, /*@':int64-t',*/
        &ffi_type_uint64, /*@':uint64-t',*/
#endif
#ifdef ecl_long_long_t
        &ffi_type_sint64, /*@':long-long',*/ /*FIXME! libffi does not have long long */
        &ffi_type_uint64, /*@':unsigned-long-long',*/
#endif
	&ffi_type_pointer, /*@':pointer-void',*/
	&ffi_type_pointer, /*@':cstring',*/
	&ffi_type_pointer, /*@':object',*/
	&ffi_type_float, /*@':float',*/
	&ffi_type_double, /*@':double',*/
	&ffi_type_void /*@':void'*/
};
#endif /* HAVE_LIBFFI */

cl_object
ecl_make_foreign_data(cl_object tag, cl_index size, void *data)
{
	cl_object output = ecl_alloc_object(t_foreign);
	output->foreign.tag = tag == ECL_NIL ? @':void' : tag;
	output->foreign.size = size;
	output->foreign.data = (char*)data;
	return output;
}

cl_object
ecl_allocate_foreign_data(cl_object tag, cl_index size)
{
	cl_object output = ecl_alloc_object(t_foreign);
	output->foreign.tag = tag;
	output->foreign.size = size;
	output->foreign.data = (char*)ecl_alloc_atomic(size);
	return output;
}

void *
ecl_foreign_data_pointer_safe(cl_object f)
{
	if (ecl_unlikely(ecl_t_of(f) != t_foreign)) {
                FEwrong_type_only_arg(@[si::foreign-data-pointer], f,
                                      @[si::foreign-data]);
        }
	return f->foreign.data;
}

char *
ecl_base_string_pointer_safe(cl_object f)
{
	unsigned char *s;
	/* FIXME! Is there a better function name? */
	f = ecl_check_cl_type(@'si::make-foreign-data-from-array', f, t_base_string);
	s = f->base_string.self;
	if (ecl_unlikely(ECL_ARRAY_HAS_FILL_POINTER_P(f) &&
                         s[f->base_string.fillp] != 0)) {
		FEerror("Cannot coerce a string with fill pointer to (char *)", 0);
	}
	return (char *)s;
}

cl_object
ecl_null_terminated_base_string(cl_object f)
{
	/* FIXME! Is there a better function name? */
	f = ecl_check_cl_type(@'si::make-foreign-data-from-array', f, t_base_string);
	if (ECL_ARRAY_HAS_FILL_POINTER_P(f) &&
            f->base_string.self[f->base_string.fillp] != 0) {
		return cl_copy_seq(f);
	} else {
		return f;
	}
}

cl_object
si_allocate_foreign_data(cl_object tag, cl_object size)
{
	cl_object output = ecl_alloc_object(t_foreign);
	cl_index bytes = ecl_to_size(size);
	output->foreign.tag = tag;
	output->foreign.size = bytes;
	/* FIXME! Should be atomic uncollectable or malloc, but we do not export
	 * that garbage collector interface and malloc may be overwritten
	 * by the GC library */
	output->foreign.data = bytes? ecl_alloc_uncollectable(bytes) : NULL;
	@(return output)
}

cl_object
si_free_foreign_data(cl_object f)
{
	if (ecl_unlikely(ecl_t_of(f) != t_foreign)) {
                FEwrong_type_only_arg(@[si::free-foreign-data], f,
                                      @[si::foreign-data]);
	}
	if (f->foreign.size) {
		/* See si_allocate_foreign_data() */
		ecl_free_uncollectable(f->foreign.data);
	}
	f->foreign.size = 0;
	f->foreign.data = NULL;
        @(return)
}

cl_object
si_make_foreign_data_from_array(cl_object array)
{
	cl_object tag;
	if (ecl_unlikely(ecl_t_of(array) != t_array && ecl_t_of(array) != t_vector)) {
                FEwrong_type_only_arg(@[si::make-foreign-data-from-array], array,
                                      @[array]);
	}
        tag = ecl_aet_to_ffi_table[array->array.elttype];
        if (ecl_unlikely(Null(tag))) {
		FEerror("Cannot make foreign object from array "
                        "with element type ~S.", 1,
                        ecl_elttype_to_symbol(array->array.elttype));
	}
	@(return ecl_make_foreign_data(tag, 0, array->array.self.bc));
}

cl_object
si_foreign_data_p(cl_object f)
{
        @(return (ECL_FOREIGN_DATA_P(f)? ECL_T : ECL_NIL))
}

cl_object
si_foreign_data_address(cl_object f)
{
	if (ecl_unlikely(ecl_t_of(f) != t_foreign)) {
                FEwrong_type_only_arg(@[si::foreign-data-address], f,
                                      @[si::foreign-data]);
	}
	@(return ecl_make_unsigned_integer((cl_index)f->foreign.data))
}

cl_object
si_foreign_data_tag(cl_object f)
{
	if (ecl_unlikely(ecl_t_of(f) != t_foreign)) {
                FEwrong_type_only_arg(@[si::foreign-data-tag], f,
                                      @[si::foreign-data]);
	}
	@(return f->foreign.tag);
}

cl_object
si_foreign_data_equal(cl_object f1, cl_object f2)
{
	if (ecl_unlikely(!ECL_FOREIGN_DATA_P(f1))) {
                FEwrong_type_only_arg(@[si::foreign-data-address], f1,
                                      @[si::foreign-data]);
	}
	if (ecl_unlikely(!ECL_FOREIGN_DATA_P(f2))) {
                FEwrong_type_only_arg(@[si::foreign-data-address], f2,
                                      @[si::foreign-data]);
	}
	@(return ((f1->foreign.data == f2->foreign.data)? ECL_T : ECL_NIL))
}

cl_object
si_foreign_data_pointer(cl_object f, cl_object andx, cl_object asize,
			cl_object tag)
{
	cl_index ndx = ecl_to_size(andx);
	cl_index size = ecl_to_size(asize);
	cl_object output;

	if (ecl_unlikely(ecl_t_of(f) != t_foreign)) {
                FEwrong_type_only_arg(@[si::foreign-data-pointer], f,
                                      @[si::foreign-data]);
	}
	if (ecl_unlikely(ndx >= f->foreign.size || (f->foreign.size - ndx) < size)) {
		FEerror("Out of bounds reference into foreign data type ~A.", 1, f);
	}
	output = ecl_alloc_object(t_foreign);
	output->foreign.tag = tag;
	output->foreign.size = size;
	output->foreign.data = f->foreign.data + ndx;
	@(return output)
}

cl_object
si_foreign_data_ref(cl_object f, cl_object andx, cl_object asize, cl_object tag)
{
	cl_index ndx = ecl_to_size(andx);
	cl_index size = ecl_to_size(asize);
	cl_object output;

	if (ecl_unlikely(ecl_t_of(f) != t_foreign)) {
                FEwrong_type_nth_arg(@[si::foreign-data-ref], 1, f,
                                     @[si::foreign-data]);
	}
	if (ecl_unlikely(ndx >= f->foreign.size || (f->foreign.size - ndx) < size)) {
		FEerror("Out of bounds reference into foreign data type ~A.", 1, f);
	}
	output = ecl_allocate_foreign_data(tag, size);
	memcpy(output->foreign.data, f->foreign.data + ndx, size);
	@(return output)
}

cl_object
si_foreign_data_set(cl_object f, cl_object andx, cl_object value)
{
	cl_index ndx = ecl_to_size(andx);
	cl_index size, limit;

	if (ecl_unlikely(ecl_t_of(f) != t_foreign)) {
                FEwrong_type_nth_arg(@[si::foreign-data-set], 1, f,
                                     @[si::foreign-data]);
	}
	if (ecl_unlikely(ecl_t_of(value) != t_foreign)) {
                FEwrong_type_nth_arg(@[si::foreign-data-set], 3, value,
                                     @[si::foreign-data]);
	}
	size = value->foreign.size;
	limit = f->foreign.size;
	if (ecl_unlikely(ndx >= limit || (limit - ndx) < size)) {
		FEerror("Out of bounds reference into foreign data type ~A.", 1, f);
	}
	memcpy(f->foreign.data + ndx, value->foreign.data, size);
	@(return value)
}

static int
foreign_type_code(cl_object type)
{
	int i;
	for (i = 0; i <= ECL_FFI_VOID; i++) {
		if (type == ecl_foreign_type_table[i].name)
			return i;
	}
        return -1;
}

enum ecl_ffi_tag
ecl_foreign_type_code(cl_object type)
{
	int i = foreign_type_code(type);
        if (ecl_unlikely(i < 0)) {
                FEerror("~A does not denote an elementary foreign type.", 1, type);
        }
	return (enum ecl_ffi_tag)i;
}

#ifdef HAVE_LIBFFI
ffi_abi
ecl_foreign_cc_code(cl_object cc)
{
	int i;
	for (i = 0; i <= ECL_FFI_CC_STDCALL; i++) {
		if (cc == ecl_foreign_cc_table[i].symbol)
			return ecl_foreign_cc_table[i].abi;
	}
	FEerror("~A does no denote a valid calling convention.", 1, cc);
	return ECL_FFI_CC_CDECL;
}
#endif

#ifdef ECL_DYNAMIC_FFI
enum ecl_ffi_calling_convention
ecl_foreign_cc_code(cl_object cc)
{
	int i;
	for (i = 0; i <= ECL_FFI_CC_STDCALL; i++) {
		if (cc == ecl_foreign_cc_table[i])
			return (enum ecl_ffi_calling_convention)i;
	}
	FEerror("~A does no denote a valid calling convention.", 1, cc);
	return ECL_FFI_CC_CDECL;
}
#endif

static void wrong_ffi_tag(enum ecl_ffi_tag tag) ecl_attr_noreturn;

static void
wrong_ffi_tag(enum ecl_ffi_tag tag)
{
	FEerror("Invalid ecl_ffi_tag code ~D", 1, ecl_make_integer(tag));
}

cl_object
ecl_foreign_data_ref_elt(void *p, enum ecl_ffi_tag tag)
{
	switch (tag) {
	case ECL_FFI_CHAR:
		return ECL_CODE_CHAR(*(char *)p);
	case ECL_FFI_UNSIGNED_CHAR:
		return ECL_CODE_CHAR(*(unsigned char *)p);
	case ECL_FFI_BYTE:
		return ecl_make_fixnum(*(int8_t *)p);
	case ECL_FFI_UNSIGNED_BYTE:
		return ecl_make_fixnum(*(uint8_t *)p);
	case ECL_FFI_SHORT:
		return ecl_make_fixnum(*(short *)p);
	case ECL_FFI_UNSIGNED_SHORT:
		return ecl_make_fixnum(*(unsigned short *)p);
	case ECL_FFI_INT:
		return ecl_make_integer(*(int *)p);
	case ECL_FFI_UNSIGNED_INT:
		return ecl_make_unsigned_integer(*(unsigned int *)p);
	case ECL_FFI_LONG:
		return ecl_make_integer(*(long *)p);
#ifdef ecl_uint8_t
        case ECL_FFI_INT8_T:
                return ecl_make_fixnum(*(ecl_int8_t *)p);
        case ECL_FFI_UINT8_T:
                return ecl_make_fixnum(*(ecl_uint8_t *)p);
#endif
#ifdef ecl_uint16_t
        case ECL_FFI_INT16_T:
                return ecl_make_int16_t(*(ecl_int16_t *)p);
        case ECL_FFI_UINT16_T:
                return ecl_make_uint16_t(*(ecl_uint16_t *)p);
#endif
#ifdef ecl_uint32_t
        case ECL_FFI_INT32_T:
                return ecl_make_int32_t(*(ecl_int32_t *)p);
        case ECL_FFI_UINT32_T:
                return ecl_make_uint32_t(*(ecl_uint32_t *)p);
#endif
#ifdef ecl_uint64_t
        case ECL_FFI_INT64_T:
                return ecl_make_int64_t(*(ecl_int64_t *)p);
        case ECL_FFI_UINT64_T:
                return ecl_make_uint64_t(*(ecl_uint64_t *)p);
#endif
#ifdef ecl_long_long_t
        case ECL_FFI_LONG_LONG:
                return ecl_make_long_long(*(ecl_long_long_t *)p);
        case ECL_FFI_UNSIGNED_LONG_LONG:
                return ecl_make_ulong_long(*(ecl_ulong_long_t *)p);
#endif
	case ECL_FFI_UNSIGNED_LONG:
		return ecl_make_unsigned_integer(*(unsigned long *)p);
	case ECL_FFI_POINTER_VOID:
		return ecl_make_foreign_data(@':pointer-void', 0, *(void **)p);
	case ECL_FFI_CSTRING:
		return *(char **)p ?
                        ecl_make_simple_base_string(*(char **)p, -1) : ECL_NIL;
	case ECL_FFI_OBJECT:
		return *(cl_object *)p;
	case ECL_FFI_FLOAT:
		return ecl_make_single_float(*(float *)p);
	case ECL_FFI_DOUBLE:
		return ecl_make_double_float(*(double *)p);
	case ECL_FFI_VOID:
		return ECL_NIL;
        default:
                wrong_ffi_tag(tag);
	}
}

void
ecl_foreign_data_set_elt(void *p, enum ecl_ffi_tag tag, cl_object value)
{
	switch (tag) {
	case ECL_FFI_CHAR:
		*(char *)p = (char)ecl_base_char_code(value);
		break;
	case ECL_FFI_UNSIGNED_CHAR:
		*(unsigned char*)p = (unsigned char)ecl_base_char_code(value);
		break;
	case ECL_FFI_BYTE:
		*(int8_t *)p = ecl_to_int8_t(value);
		break;
	case ECL_FFI_UNSIGNED_BYTE:
		*(uint8_t *)p = ecl_to_uint8_t(value);
		break;
	case ECL_FFI_SHORT:
		*(short *)p = ecl_to_short(value);
		break;
	case ECL_FFI_UNSIGNED_SHORT:
		*(unsigned short *)p = ecl_to_ushort(value);
		break;
	case ECL_FFI_INT:
		*(int *)p = ecl_to_int(value);
		break;
	case ECL_FFI_UNSIGNED_INT:
		*(unsigned int *)p = ecl_to_uint(value);
		break;
	case ECL_FFI_LONG:
		*(long *)p = ecl_to_long(value);
		break;
	case ECL_FFI_UNSIGNED_LONG:
		*(unsigned long *)p = ecl_to_ulong(value);
		break;
        case ECL_FFI_INT8_T:
                *(ecl_int8_t *)p = ecl_to_int8_t(value);
                break;
        case ECL_FFI_UINT8_T:
                *(ecl_uint8_t *)p = ecl_to_uint8_t(value);
                break;
#ifdef ecl_uint16_t
        case ECL_FFI_INT16_T:
                *(ecl_int16_t *)p = ecl_to_int16_t(value);
                break;
        case ECL_FFI_UINT16_T:
                *(ecl_uint16_t *)p = ecl_to_uint16_t(value);
                break;
#endif
#ifdef ecl_uint32_t
        case ECL_FFI_INT32_T:
                *(ecl_int32_t *)p = ecl_to_int32_t(value);
                break;
        case ECL_FFI_UINT32_T:
                *(ecl_uint32_t *)p = ecl_to_uint32_t(value);
                break;
#endif
#ifdef ecl_uint64_t
        case ECL_FFI_INT64_T:
                *(ecl_int64_t *)p = ecl_to_int64_t(value);
                break;
        case ECL_FFI_UINT64_T:
                *(ecl_uint64_t *)p = ecl_to_uint64_t(value);
                break;
#endif
#ifdef ecl_long_long_t
        case ECL_FFI_LONG_LONG:
                *(ecl_long_long_t *)p = ecl_to_long_long(value);
                break;
        case ECL_FFI_UNSIGNED_LONG_LONG:
                *(ecl_ulong_long_t *)p = ecl_to_ulong_long(value);
                break;
#endif
	case ECL_FFI_POINTER_VOID:
		*(void **)p = ecl_foreign_data_pointer_safe(value);
		break;
	case ECL_FFI_CSTRING:
		*(char **)p = value == ECL_NIL ? NULL : (char*)value->base_string.self;
		break;
	case ECL_FFI_OBJECT:
		*(cl_object *)p = value;
		break;
	case ECL_FFI_FLOAT:
		*(float *)p = ecl_to_float(value);
		break;
	case ECL_FFI_DOUBLE:
		*(double *)p = ecl_to_double(value);
		break;
	case ECL_FFI_VOID:
		break;
        default:
                wrong_ffi_tag(tag);
	}
}

cl_object
si_foreign_data_ref_elt(cl_object f, cl_object andx, cl_object type)
{
	cl_index ndx = ecl_to_size(andx);
	cl_index limit = f->foreign.size;
	enum ecl_ffi_tag tag = ecl_foreign_type_code(type);
	if (ecl_unlikely(ndx >= limit ||
                         (ndx + ecl_foreign_type_table[tag].size > limit))) {
		FEerror("Out of bounds reference into foreign data type ~A.", 1, f);
	}
	if (ecl_unlikely(ecl_t_of(f) != t_foreign)) {
                FEwrong_type_nth_arg(@[si::foreign-data-ref-elt], 1, f,
                                     @[si::foreign-data]);
	}
	@(return ecl_foreign_data_ref_elt((void*)(f->foreign.data + ndx), tag))
}

cl_object
si_foreign_data_set_elt(cl_object f, cl_object andx, cl_object type, cl_object value)
{
	cl_index ndx = ecl_to_size(andx);
	cl_index limit = f->foreign.size;
	enum ecl_ffi_tag tag = ecl_foreign_type_code(type);
	if (ecl_unlikely(ndx >= limit ||
                         ndx + ecl_foreign_type_table[tag].size > limit)) {
		FEerror("Out of bounds reference into foreign data type ~A.", 1, f);
	}
	if (ecl_unlikely(ecl_t_of(f) != t_foreign)) {
                FEwrong_type_nth_arg(@[si::foreign-data-set-elt], 1, f,
                                     @[si::foreign-data]);
	}
	ecl_foreign_data_set_elt((void*)(f->foreign.data + ndx), tag, value);
	@(return value)
}

cl_object
si_size_of_foreign_elt_type(cl_object type)
{
	enum ecl_ffi_tag tag = ecl_foreign_type_code(type);
	@(return ecl_make_fixnum(ecl_foreign_type_table[tag].size))
}

cl_object
si_alignment_of_foreign_elt_type(cl_object type)
{
	enum ecl_ffi_tag tag = ecl_foreign_type_code(type);
	@(return ecl_make_fixnum(ALIGNMENT(tag)))
}

cl_object
si_foreign_elt_type_p(cl_object type)
{
	@(return ((foreign_type_code(type) < 0)? ECL_NIL : ECL_T))
}

cl_object
si_null_pointer_p(cl_object f)
{
	if (ecl_unlikely(ecl_t_of(f) != t_foreign))
                FEwrong_type_only_arg(@[si::null-pointer-p], f,
                                      @[si::foreign-data]);
	@(return ((f->foreign.data == NULL)? ECL_T : ECL_NIL))
}

cl_object
si_foreign_data_recast(cl_object f, cl_object size, cl_object tag)
{
	if (ecl_unlikely(ecl_t_of(f) != t_foreign))
                FEwrong_type_nth_arg(@[si::foreign-data-recast], 1, f,
                                     @[si::foreign-data]);
	f->foreign.size = ecl_to_size(size);
	f->foreign.tag = tag;
	@(return f)
}

cl_object
si_load_foreign_module(cl_object filename)
{
#if !defined(ENABLE_DLOPEN)
	FEerror("SI:LOAD-FOREIGN-MODULE does not work when ECL is statically linked", 0);
#else
	cl_object output;

# ifdef ECL_THREADS
	mp_get_lock(1, ecl_symbol_value(@'mp::+load-compile-lock+'));
	ECL_UNWIND_PROTECT_BEGIN(ecl_process_env()) {
# endif
	output = ecl_library_open(filename, 0);
	if (output->cblock.handle == NULL) {
		cl_object aux = ecl_library_error(output);
		ecl_library_close(output);
		output = aux;
	}
# ifdef ECL_THREADS
	(void)0; /* MSVC complains about missing ';' before '}' */
	} ECL_UNWIND_PROTECT_EXIT {
	mp_giveup_lock(ecl_symbol_value(@'mp::+load-compile-lock+'));
	} ECL_UNWIND_PROTECT_END;
# endif
	if (ecl_unlikely(ecl_t_of(output) != t_codeblock)) {
		FEerror("LOAD-FOREIGN-MODULE: Could not load "
                        "foreign module ~S (Error: ~S)", 2, filename, output);
        }
        output->cblock.locked |= 1;
        @(return output)
#endif
}

cl_object
si_find_foreign_symbol(cl_object var, cl_object module, cl_object type, cl_object size)
{
#if !defined(ENABLE_DLOPEN)
	FEerror("SI:FIND-FOREIGN-SYMBOL does not work when ECL is statically linked", 0);
#else
	cl_object block;
	cl_object output = ECL_NIL;
	void *sym;

	block = (module == @':default' ? module : si_load_foreign_module(module));
	var = ecl_null_terminated_base_string(var);
	sym = ecl_library_symbol(block, (char*)var->base_string.self, 1);
	if (sym == NULL) {
		if (block != @':default')
			output = ecl_library_error(block);
		goto OUTPUT;
	}
	output = ecl_make_foreign_data(type, ecl_to_fixnum(size), sym);
OUTPUT:
	if (ecl_unlikely(ecl_t_of(output) != t_foreign))
		FEerror("FIND-FOREIGN-SYMBOL: Could not load "
                        "foreign symbol ~S from module ~S (Error: ~S)",
                        3, var, module, output);
        @(return output)
#endif
}

#ifdef ECL_DYNAMIC_FFI
static void
ecl_fficall_overflow()
{
	FEerror("Stack overflow on SI:CALL-CFUN", 0);
}

void
ecl_fficall_prepare(cl_object return_type, cl_object arg_type, cl_object cc_type)
{
	struct ecl_fficall *fficall = cl_env.fficall;
	fficall->buffer_sp = fficall->buffer;
	fficall->buffer_size = 0;
	fficall->cstring = ECL_NIL;
	fficall->cc = ecl_foreign_cc_code(cc_type);
        fficall->registers = ecl_fficall_prepare_extra(fficall->registers);
}

void
ecl_fficall_push_bytes(void *data, size_t bytes)
{
	struct ecl_fficall *fficall = cl_env.fficall;
	fficall->buffer_size += bytes;
	if (fficall->buffer_size >= ECL_FFICALL_LIMIT)
		ecl_fficall_overflow();
	memcpy(fficall->buffer_sp, (char*)data, bytes);
	fficall->buffer_sp += bytes;
}

void
ecl_fficall_push_int(int data)
{
	ecl_fficall_push_bytes(&data, sizeof(int));
}

void
ecl_fficall_align(int data)
{
	struct ecl_fficall *fficall = cl_env.fficall;
	if (data == 1)
		return;
	else {
		size_t sp = fficall->buffer_sp - fficall->buffer;
		size_t mask = data - 1;
		size_t new_sp = (sp + mask) & ~mask;
		if (new_sp >= ECL_FFICALL_LIMIT)
			ecl_fficall_overflow();
		fficall->buffer_sp = fficall->buffer + new_sp;
		fficall->buffer_size = new_sp;
	}
}

@(defun si::call-cfun (fun return_type arg_types args &optional (cc_type @':cdecl'))
	struct ecl_fficall *fficall = cl_env.fficall;
	void *cfun = ecl_foreign_data_pointer_safe(fun);
	cl_object object;
	enum ecl_ffi_tag return_type_tag = ecl_foreign_type_code(return_type);
@

	ecl_fficall_prepare(return_type, arg_types, cc_type);
	while (CONSP(arg_types)) {
		enum ecl_ffi_tag type;
		if (!CONSP(args)) {
			FEerror("In SI:CALL-CFUN, mismatch between argument types and argument list: ~A vs ~A", 0);
		}
		type = ecl_foreign_type_code(CAR(arg_types));
		if (type == ECL_FFI_CSTRING) {
			object = ecl_null_terminated_base_string(CAR(args));
			if (CAR(args) != object)
				fficall->cstring =
					CONS(object, fficall->cstring);
		} else {
			object = CAR(args);
		}
		ecl_foreign_data_set_elt(&fficall->output, type, object);
		ecl_fficall_push_arg(&fficall->output, type);
		arg_types = CDR(arg_types);
		args = CDR(args);
	}
	ecl_fficall_execute(cfun, fficall, return_type_tag);
	object = ecl_foreign_data_ref_elt(&fficall->output, return_type_tag);

	fficall->buffer_size = 0;
	fficall->buffer_sp = fficall->buffer;
	fficall->cstring = ECL_NIL;

	@(return object)
@)

@(defun si::make-dynamic-callback (fun sym rtype argtypes &optional (cctype @':cdecl'))
	cl_object data;
	cl_object cbk;
@
	data = cl_list(3, fun, rtype, argtypes);
	cbk  = ecl_make_foreign_data(@':pointer-void', 0, ecl_dynamic_callback_make(data, ecl_foreign_cc_code(cctype)));

	si_put_sysprop(sym, @':callback', CONS(cbk, data));
	@(return cbk)
@)
#endif /* ECL_DYNAMIC_FFI */


#ifdef HAVE_LIBFFI
static void
resize_call_stack(cl_env_ptr env, cl_index new_size)
{
        cl_index i;
        ffi_type **types =
                ecl_alloc_atomic((new_size + 1) * sizeof(ffi_type*));
        union ecl_ffi_values *values =
                ecl_alloc_atomic((new_size + 1) * sizeof(union ecl_ffi_values));
        union ecl_ffi_values **values_ptrs =
                ecl_alloc_atomic(new_size * sizeof(union ecl_ffi_values *));
        memcpy(types, env->ffi_types, env->ffi_args_limit * sizeof(ffi_type*));
        memcpy(values, env->ffi_values, env->ffi_args_limit *
               sizeof(union ecl_ffi_values));
        for (i = 0; i < new_size; i++) {
                values_ptrs[i] = (values + i + 1);
        }
        env->ffi_args_limit = new_size;
        ecl_dealloc(env->ffi_types);
        env->ffi_types = types;
        ecl_dealloc(env->ffi_values);
        env->ffi_values = values;
        ecl_dealloc(env->ffi_values_ptrs);
        env->ffi_values_ptrs = values_ptrs;
}

static int
prepare_cif(cl_env_ptr the_env, ffi_cif *cif, cl_object return_type,
            cl_object arg_types, cl_object args,
            cl_object cc_type, ffi_type ***output_copy)
{
        int n, ok;
        ffi_type **types;
        enum ecl_ffi_tag type = ecl_foreign_type_code(return_type);
        if (!the_env->ffi_args_limit)
                resize_call_stack(the_env, 32);
        the_env->ffi_types[0] = ecl_type_to_libffi_type[type];
        for (n=0; !Null(arg_types); ) {
                if (!LISTP(arg_types)) {
                        FEerror("In CALL-CFUN, types lists is not a proper list", 0);
                }
                if (n >= the_env->ffi_args_limit) {
                        resize_call_stack(the_env, n + 32);
                }
		type = ecl_foreign_type_code(ECL_CONS_CAR(arg_types));
                arg_types = ECL_CONS_CDR(arg_types);
                the_env->ffi_types[++n] = ecl_type_to_libffi_type[type];
                if (CONSP(args)) {
                        cl_object object = ECL_CONS_CAR(args);
                        args = ECL_CONS_CDR(args);
                        if (type == ECL_FFI_CSTRING) {
                                object = ecl_null_terminated_base_string(CAR(args));
                                if (ECL_CONS_CAR(args) != object) {
                                        ECL_STACK_PUSH(the_env, object);
                                }
                        }
                        ecl_foreign_data_set_elt(the_env->ffi_values + n, type, object);
                }
        }
        if (output_copy) {
                cl_index bytes = (n + 1) * sizeof(ffi_type*);
                *output_copy = types = (ffi_type**)ecl_alloc_atomic(bytes);
                memcpy(types, the_env->ffi_types, bytes);
        } else {
                types = the_env->ffi_types;
        }
        ok = ffi_prep_cif(cif, ecl_foreign_cc_code(cc_type), n, types[0], types + 1);
        if (ok != FFI_OK) {
                if (ok == FFI_BAD_ABI) {
                        FEerror("In CALL-CFUN, not a valid ABI: ~A", 1,
                                cc_type);
                }
                if (ok == FFI_BAD_TYPEDEF) {
                        FEerror("In CALL-CFUN, wrong or malformed argument types", 0);
                }
        }
        return n;
}

@(defun si::call-cfun (fun return_type arg_types args &optional (cc_type @':default'))
	void *cfun = ecl_foreign_data_pointer_safe(fun);
	cl_object object;
        volatile cl_index sp;
        ffi_cif cif;
@
{
	sp = ECL_STACK_INDEX(the_env);
	prepare_cif(the_env, &cif, return_type, arg_types, args, cc_type, NULL);
        ffi_call(&cif, cfun, the_env->ffi_values, (void **)the_env->ffi_values_ptrs);
	object = ecl_foreign_data_ref_elt(the_env->ffi_values,
                                          ecl_foreign_type_code(return_type));
	ECL_STACK_SET_INDEX(the_env, sp);
	@(return object)
}
@)

static void
callback_executor(ffi_cif *cif, void *result, void **args, void *userdata)
{
        cl_object data = (cl_object)userdata;
        cl_object fun = ECL_CONS_CAR(data);
        cl_object ret_type = (data = ECL_CONS_CDR(data), ECL_CONS_CAR(data));
        cl_object arg_types = (data = ECL_CONS_CDR(data), ECL_CONS_CAR(data));
        cl_env_ptr the_env = ecl_process_env();
        struct ecl_stack_frame frame_aux;
        const cl_object frame = ecl_stack_frame_open(the_env, (cl_object)&frame_aux, 0);
        cl_object x;
        while (arg_types != ECL_NIL) {
                cl_object type = ECL_CONS_CAR(arg_types);
                enum ecl_ffi_tag tag = ecl_foreign_type_code(type);
                x = ecl_foreign_data_ref_elt(*args, tag);
                ecl_stack_frame_push(frame, x);
                arg_types = ECL_CONS_CDR(arg_types);
                args++;
        }
        x = ecl_apply_from_stack_frame(frame, fun);
        ecl_stack_frame_close(frame);
        ecl_foreign_data_set_elt(result, ecl_foreign_type_code(ret_type), x);
}

cl_object
si_free_ffi_closure(cl_object closure)
{
        ffi_closure_free(ecl_foreign_data_pointer_safe(closure));
	@(return);
}

@(defun si::make-dynamic-callback (fun sym return_type arg_types
                                   &optional (cc_type @':default'))
@
{
        ffi_cif *cif = ecl_alloc(sizeof(ffi_cif));
        ffi_type **types;
        int n = prepare_cif(the_env, cif, return_type, arg_types, ECL_NIL, cc_type,
                            &types);

	/* libffi allocates executable memory for us. ffi_closure_alloc()
	 * returns a pointer to memory and a pointer to the beginning of
	 * the actual executable region (executable_closure) which is
	 * where the code resides. */
        void *executable_region;
        ffi_closure *closure = ffi_closure_alloc(sizeof(ffi_closure), &executable_region);

        cl_object closure_object = ecl_make_foreign_data(@':pointer-void',
                                                         sizeof(ffi_closure),
                                                         closure);
        si_set_finalizer(closure_object, @'si::free-ffi-closure');

        cl_object data = cl_list(6, closure_object,
                                 fun, return_type, arg_types, cc_type,
                                 ecl_make_foreign_data(@':pointer-void',
                                                       sizeof(*cif), cif),
                                 ecl_make_foreign_data(@':pointer-void',
                                                       (n + 1) * sizeof(ffi_type*),
                                                       types));
        int status = ffi_prep_closure_loc(closure, cif, callback_executor,
                                          ECL_CONS_CDR(data), executable_region);

        if (status != FFI_OK) {
                FEerror("Unable to build callback. libffi returns ~D", 1,
                        ecl_make_fixnum(status));
        }
	si_put_sysprop(sym, @':callback', data);
        @(return closure_object);
}
@)
#endif /* HAVE_LIBFFI */
