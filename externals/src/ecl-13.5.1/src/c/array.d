/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    array.c --  Array routines
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <limits.h>
#include <string.h>
#include <ecl/ecl.h>
#define ECL_DEFINE_AET_SIZE
#include <ecl/internal.h>

static const cl_object ecl_aet_name[] = {
        ECL_T,                   /* ecl_aet_object */
        @'single-float',      /* ecl_aet_sf */
        @'double-float',      /* ecl_aet_df */
        @'bit',               /* ecl_aet_bit: cannot be handled with this code */
        @'ext::cl-fixnum',    /* ecl_aet_fix */
        @'ext::cl-index',     /* ecl_aet_index */
        @'ext::byte8',        /* ecl_aet_b8 */
        @'ext::integer8',     /* ecl_aet_i8 */
#ifdef ecl_uint16_t
        @'ext::byte16',
        @'ext::integer16',
#endif
#ifdef ecl_uint32_t
        @'ext::byte32',
        @'ext::integer32',
#endif
#ifdef ecl_uint64_t
        @'ext::byte64',
        @'ext::integer64',
#endif
#ifdef ECL_UNICODE
        @'character',         /* ecl_aet_ch */
#endif
        @'base-char'          /* ecl_aet_bc */
};

static void FEbad_aet() ecl_attr_noreturn;

static void
FEbad_aet()
{
	FEerror(
"A routine from ECL got an object with a bad array element type.\n"
"If you are running a standard copy of ECL, please report this bug.\n"
"If you are embedding ECL into an application, please ensure you\n"
"passed the right value to the array creation routines.\n",0);
}

static cl_index
out_of_bounds_error(cl_index ndx, cl_object x)
{
	cl_object type = cl_list(3, @'integer', ecl_make_fixnum(0),
                                 ecl_make_fixnum(x->array.dim));
        FEwrong_type_argument(ecl_make_integer(ndx), type);
}

void
FEwrong_dimensions(cl_object a, cl_index rank)
{
        cl_object list = cl_make_list(3, ecl_make_fixnum(rank),
                                      @':initial-element', @'*');
        cl_object type = cl_list(3, @'array', @'*', list);
        FEwrong_type_argument(type, a);
}

static ECL_INLINE cl_index
checked_index(cl_object function, cl_object a, int which, cl_object index,
              cl_index nonincl_limit)
{
        cl_index output;
        unlikely_if (!ECL_FIXNUMP(index) || ecl_fixnum_minusp(index))
                FEwrong_index(function, a, which, index, nonincl_limit);
        output = ecl_fixnum(index);
        unlikely_if (output >= nonincl_limit)
                FEwrong_index(function, a, which, index, nonincl_limit);
        return output;
}

cl_index
ecl_to_index(cl_object n)
{
	switch (ecl_t_of(n)) {
	case t_fixnum: {
		cl_fixnum out = ecl_fixnum(n);
		if (out < 0 || out >= ECL_ARRAY_DIMENSION_LIMIT)
			FEtype_error_index(ECL_NIL, out);
		return out;
	}
	default:
		FEwrong_type_only_arg(@[coerce], n, @[fixnum]);
	}
}

cl_object
cl_row_major_aref(cl_object x, cl_object indx)
{
	cl_index j = ecl_to_size(indx);
	@(return ecl_aref(x, j))
}

cl_object
si_row_major_aset(cl_object x, cl_object indx, cl_object val)
{
	cl_index j = ecl_to_size(indx);
	@(return ecl_aset(x, j, val))
}

@(defun aref (x &rest indx)
@ {
	cl_index i, j;
	cl_index r = narg - 1;
	switch (ecl_t_of(x)) {
	case t_array:
		if (r != x->array.rank)
			FEerror("Wrong number of indices.", 0);
		for (i = j = 0;  i < r;  i++) {
			cl_index s = checked_index(@[aref], x, i,
                                                   ecl_va_arg(indx),
                                                   x->array.dims[i]);
			j = j*(x->array.dims[i]) + s;
		}
		break;
	case t_vector:
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_base_string:
	case t_bitvector:
		if (r != 1)
			FEerror("Wrong number of indices.", 0);
		j = checked_index(@[aref], x, -1, ecl_va_arg(indx), x->vector.dim);
		break;
	default:
                FEwrong_type_nth_arg(@[aref], 1, x, @[array]);
	}
	@(return ecl_aref_unsafe(x, j));
} @)

cl_object
ecl_aref_unsafe(cl_object x, cl_index index)
{
	switch (x->array.elttype) {
	case ecl_aet_object:
		return x->array.self.t[index];
	case ecl_aet_bc:
		return ECL_CODE_CHAR(x->base_string.self[index]);
#ifdef ECL_UNICODE
	case ecl_aet_ch:
                return ECL_CODE_CHAR(x->string.self[index]);
#endif
	case ecl_aet_bit:
		index += x->vector.offset;
		if (x->vector.self.bit[index/CHAR_BIT] & (0200>>index%CHAR_BIT))
			return(ecl_make_fixnum(1));
		else
			return(ecl_make_fixnum(0));
	case ecl_aet_fix:
		return ecl_make_integer(x->array.self.fix[index]);
	case ecl_aet_index:
		return ecl_make_unsigned_integer(x->array.self.index[index]);
	case ecl_aet_sf:
		return(ecl_make_single_float(x->array.self.sf[index]));
	case ecl_aet_df:
		return(ecl_make_double_float(x->array.self.df[index]));
	case ecl_aet_b8:
		return ecl_make_uint8_t(x->array.self.b8[index]);
	case ecl_aet_i8:
		return ecl_make_int8_t(x->array.self.i8[index]);
#ifdef ecl_uint16_t
	case ecl_aet_b16:
		return ecl_make_uint16_t(x->array.self.b16[index]);
	case ecl_aet_i16:
		return ecl_make_int16_t(x->array.self.i16[index]);
#endif
#ifdef ecl_uint32_t
	case ecl_aet_b32:
		return ecl_make_uint32_t(x->array.self.b32[index]);
	case ecl_aet_i32:
		return ecl_make_int32_t(x->array.self.i32[index]);
#endif
#ifdef ecl_uint64_t
	case ecl_aet_b64:
		return ecl_make_uint64_t(x->array.self.b64[index]);
	case ecl_aet_i64:
		return ecl_make_int64_t(x->array.self.i64[index]);
#endif
	default:
		FEbad_aet();
	}
}

cl_object
ecl_aref(cl_object x, cl_index index)
{
        if (ecl_unlikely(!ECL_ARRAYP(x))) {
                FEwrong_type_nth_arg(@[aref], 1, x, @[array]);
        }
        if (ecl_unlikely(index >= x->array.dim)) {
                FEwrong_index(@[row-major-aref], x, -1, ecl_make_fixnum(index),
                              x->array.dim);
        }
        return ecl_aref_unsafe(x, index);
}

cl_object
ecl_aref1(cl_object x, cl_index index)
{
        if (ecl_unlikely(!ECL_VECTORP(x))) {
                FEwrong_type_nth_arg(@[aref], 1, x, @[array]);
        }
        if (ecl_unlikely(index >= x->array.dim)) {
                FEwrong_index(@[aref], x, -1, ecl_make_fixnum(index),
                              x->array.dim);
        }
        return ecl_aref_unsafe(x, index);
}

void *
ecl_row_major_ptr(cl_object x, cl_index index, cl_index bytes)
{
	cl_index elt_size, offset;
	cl_elttype elt_type;

	if (ecl_unlikely(!ECL_ARRAYP(x))) {
		FEwrong_type_nth_arg(@[aref], 1, x, @[array]);
	}

	elt_type = x->array.elttype;
	if (ecl_unlikely(elt_type == ecl_aet_bit || elt_type == ecl_aet_object))
		FEerror("In ecl_row_major_ptr: Specialized array expected, element type ~S found.",
			1,ecl_elttype_to_symbol(elt_type));

	elt_size = ecl_aet_size[elt_type];
	offset = index*elt_size;

	/* don't check bounds if bytes == 0 */
        if (ecl_unlikely(bytes > 0 && offset + bytes > x->array.dim*elt_size)) {
                FEwrong_index(@[row-major-aref], x, -1, ecl_make_fixnum(index),
                              x->array.dim);
        }

	return x->array.self.b8 + offset;
}

/*
	Internal function for setting array elements:

		(si:aset value array dim0 ... dimN)
*/
@(defun si::aset (x &rest dims)
@ {
	cl_index i, j;
	cl_index r = narg - 2;
	cl_object v;
	switch (ecl_t_of(x)) {
	case t_array:
		if (ecl_unlikely(r != x->array.rank))
			FEerror("Wrong number of indices.", 0);
		for (i = j = 0;  i < r;  i++) {
			cl_index s = checked_index(@[si::aset], x, i,
                                                   ecl_va_arg(dims),
                                                   x->array.dims[i]);
			j = j*(x->array.dims[i]) + s;
		}
		break;
	case t_vector:
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_base_string:
	case t_bitvector:
		if (ecl_unlikely(r != 1))
			FEerror("Wrong number of indices.", 0);
		j = checked_index(@[si::aset], x, -1, ecl_va_arg(dims),
                                  x->vector.dim);
		break;
	default:
                FEwrong_type_nth_arg(@[si::aset], 1, x, @[array]);
	}
	v = ecl_va_arg(dims);
	@(return ecl_aset_unsafe(x, j, v))
} @)

cl_object
ecl_aset_unsafe(cl_object x, cl_index index, cl_object value)
{
	switch (x->array.elttype) {
	case ecl_aet_object:
		x->array.self.t[index] = value;
		break;
	case ecl_aet_bc:
		/* INV: ecl_char_code() checks the type of `value' */
		x->base_string.self[index] = ecl_char_code(value);
		break;
#ifdef ECL_UNICODE
	case ecl_aet_ch:
		x->string.self[index] = ecl_char_code(value);
		break;
#endif
	case ecl_aet_bit: {
		cl_fixnum i = ecl_to_bit(value);
		index += x->vector.offset;
		if (i == 0)
			x->vector.self.bit[index/CHAR_BIT] &= ~(0200>>index%CHAR_BIT);
		else
			x->vector.self.bit[index/CHAR_BIT] |= 0200>>index%CHAR_BIT;
		break;
	}
	case ecl_aet_fix:
		x->array.self.fix[index] = ecl_to_fix(value);
		break;
	case ecl_aet_index:
		x->array.self.index[index] = ecl_to_size(value);
		break;
	case ecl_aet_sf:
		x->array.self.sf[index] = ecl_to_float(value);
		break;
	case ecl_aet_df:
		x->array.self.df[index] = ecl_to_double(value);
		break;
	case ecl_aet_b8:
		x->array.self.b8[index] = ecl_to_uint8_t(value);
		break;
	case ecl_aet_i8:
		x->array.self.i8[index] = ecl_to_int8_t(value);
		break;
#ifdef ecl_uint16_t
	case ecl_aet_b16:
		x->array.self.b16[index] = ecl_to_uint16_t(value);
		break;
	case ecl_aet_i16:
		x->array.self.i16[index] = ecl_to_int16_t(value);
		break;
#endif
#ifdef ecl_uint32_t
	case ecl_aet_b32:
		x->array.self.b32[index] = ecl_to_uint32_t(value);
		break;
	case ecl_aet_i32:
		x->array.self.i32[index] = ecl_to_int32_t(value);
		break;
#endif
#ifdef ecl_uint64_t
	case ecl_aet_b64:
		x->array.self.b64[index] = ecl_to_uint64_t(value);
		break;
	case ecl_aet_i64:
		x->array.self.i64[index] = ecl_to_int64_t(value);
		break;
#endif
	}
	return(value);
}

cl_object
ecl_aset(cl_object x, cl_index index, cl_object value)
{
        if (ecl_unlikely(!ECL_ARRAYP(x))) {
                FEwrong_type_nth_arg(@[si::aset], 1, x, @[array]);
        }
        if (ecl_unlikely(index >= x->array.dim)) {
		out_of_bounds_error(index, x);
        }
        return ecl_aset_unsafe(x, index, value);
}

cl_object
ecl_aset1(cl_object x, cl_index index, cl_object value)
{
        if (ecl_unlikely(!ECL_VECTORP(x))) {
                FEwrong_type_nth_arg(@[si::aset], 1, x, @[array]);
        }
        if (ecl_unlikely(index >= x->array.dim)) {
		out_of_bounds_error(index, x);
        }
        return ecl_aset_unsafe(x, index, value);
}

/*
	Internal function for making arrays of more than one dimension:

		(si:make-pure-array dimension-list element-type adjustable
			            displaced-to displaced-index-offset)
*/
cl_object
si_make_pure_array(cl_object etype, cl_object dims, cl_object adj,
		   cl_object fillp, cl_object displ, cl_object disploff)
{
	cl_index r, s, i, j;
	cl_object x;
	if (ECL_FIXNUMP(dims)) {
		return si_make_vector(etype, dims, adj, fillp, displ, disploff);
	} else if (ecl_unlikely(!ECL_LISTP(dims))) {
                FEwrong_type_nth_arg(@[make-array], 1, dims,
                                     cl_list(3, @'or', @'list', @'fixnum'));
        }
	r = ecl_length(dims);
	if (ecl_unlikely(r >= ECL_ARRAY_RANK_LIMIT)) {
		FEerror("The array rank, ~R, is too large.", 1, ecl_make_fixnum(r));
	} else if (r == 1) {
		return si_make_vector(etype, ECL_CONS_CAR(dims), adj, fillp,
				      displ, disploff);
	} else if (ecl_unlikely(!Null(fillp))) {
		FEerror(":FILL-POINTER may not be specified for an array of rank ~D",
			1, ecl_make_fixnum(r));
	}
	x = ecl_alloc_object(t_array);
	x->array.displaced = ECL_NIL;
	x->array.self.t = NULL;		/* for GC sake */
	x->array.rank = r;
	x->array.elttype = (short)ecl_symbol_to_elttype(etype);
	x->array.flags = 0; /* no fill pointer, no adjustable */
	x->array.dims = (cl_index *)ecl_alloc_atomic_align(sizeof(cl_index)*r, sizeof(cl_index));
	for (i = 0, s = 1;  i < r;  i++, dims = ECL_CONS_CDR(dims)) {
                cl_object d = ECL_CONS_CAR(dims);
                if (ecl_unlikely(!ECL_FIXNUMP(d) ||
                                 ecl_fixnum_minusp(d) ||
                                 ecl_fixnum_greater(d, ecl_make_fixnum(ECL_ARRAY_DIMENSION_LIMIT))))
                {
                        cl_object type = ecl_make_integer_type(ecl_make_fixnum(0),
                                                               ecl_make_fixnum(ECL_ARRAY_DIMENSION_LIMIT));
                        FEwrong_type_nth_arg(@[make-array], 1, d, type);
                }
                j = ecl_fixnum(d);
		s *= (x->array.dims[i] = j);
		if (ecl_unlikely(s > ECL_ARRAY_TOTAL_LIMIT)) {
                        cl_object type = ecl_make_integer_type(ecl_make_fixnum(0),
                                                               ecl_make_fixnum(ECL_ARRAY_TOTAL_LIMIT));
                        FEwrong_type_key_arg(@[make-array], @[array-total-size],
                                             ecl_make_fixnum(s), type);
                }
	}
	x->array.dim = s;
        if (adj != ECL_NIL) {
                x->array.flags |= ECL_FLAG_ADJUSTABLE;
        }
	if (Null(displ))
		ecl_array_allocself(x);
	else
		ecl_displace(x, displ, disploff);
	@(return x);
}

/*
	Internal function for making vectors:

		(si:make-vector element-type dimension adjustable fill-pointer
				displaced-to displaced-index-offset)
*/
cl_object
si_make_vector(cl_object etype, cl_object dim, cl_object adj,
	       cl_object fillp, cl_object displ, cl_object disploff)
{
	cl_index d, f;
	cl_object x;
	cl_elttype aet;
 AGAIN:
	aet = ecl_symbol_to_elttype(etype);
        if (ecl_unlikely(!ECL_FIXNUMP(dim) || ecl_fixnum_minusp(dim) ||
                         ecl_fixnum_greater(dim, ECL_ARRAY_DIMENSION_LIMIT))) {
                cl_object type = ecl_make_integer_type(ecl_make_fixnum(0),
                                                       ecl_make_fixnum(ECL_ARRAY_DIMENSION_LIMIT));
                FEwrong_type_nth_arg(@[make-array], 1, dim, type);
        }
        d = ecl_fixnum(dim);
	if (aet == ecl_aet_bc) {
		x = ecl_alloc_object(t_base_string);
                x->base_string.elttype = (short)aet;
	} else if (aet == ecl_aet_bit) {
		x = ecl_alloc_object(t_bitvector);
                x->vector.elttype = (short)aet;
#ifdef ECL_UNICODE
	} else if (aet == ecl_aet_ch) {
		x = ecl_alloc_object(t_string);
                x->string.elttype = (short)aet;
#endif
	} else {
		x = ecl_alloc_object(t_vector);
		x->vector.elttype = (short)aet;
	}
	x->vector.self.t = NULL;		/* for GC sake */
	x->vector.displaced = ECL_NIL;
	x->vector.dim = d;
        x->vector.flags = 0;
        if (adj != ECL_NIL) {
                x->vector.flags |= ECL_FLAG_ADJUSTABLE;
        }
	if (Null(fillp)) {
		f = d;
	} else if (fillp == ECL_T) {
		x->vector.flags |= ECL_FLAG_HAS_FILL_POINTER;
		f = d;
	} else if (ECL_FIXNUMP(fillp) && ecl_fixnum_geq(fillp,ecl_make_fixnum(0)) &&
		   ((f = ecl_fixnum(fillp)) <= d)) {
		x->vector.flags |= ECL_FLAG_HAS_FILL_POINTER;
	} else {
		fillp = ecl_type_error(@'make-array',"fill pointer",fillp,
				       cl_list(3,@'or',cl_list(3,@'member',ECL_NIL,ECL_T),
					       cl_list(3,@'integer',ecl_make_fixnum(0),
						       dim)));
		goto AGAIN;
	}
	x->vector.fillp = f;

	if (Null(displ))
		ecl_array_allocself(x);
	else
		ecl_displace(x, displ, disploff);
	@(return x)
}

cl_object *
alloc_pointerfull_memory(cl_index l)
{
        cl_object *p = ecl_alloc_align(sizeof(cl_object) * l, sizeof(cl_object));
        cl_index i;
        for (i = 0; l--;)
                p[i++] = ECL_NIL;
        return p;
}

void
ecl_array_allocself(cl_object x)
{
        cl_elttype t = x->array.elttype;
	cl_index d = x->array.dim;
	switch (t) {
	/* assign self field only after it has been filled, for GC sake  */
	case ecl_aet_object:
		x->array.self.t = alloc_pointerfull_memory(d);
		return;
#ifdef ECL_UNICODE
	case ecl_aet_ch: {
		ecl_character *elts;
                d *= sizeof(ecl_character);
		elts = (ecl_character *)ecl_alloc_atomic_align(d, sizeof(ecl_character));
		x->string.self = elts;
		return;
        }
#endif
	case ecl_aet_bc: {
		cl_index elt_size = 1;
		x->vector.self.bc = (ecl_base_char *)ecl_alloc_atomic(d+1);
		/* Null terminate the string */
		x->vector.self.bc[d] = 0;
		break;
	}
        case ecl_aet_bit:
                d = (d + (CHAR_BIT-1)) / CHAR_BIT;
                x->vector.self.bit = (byte *)ecl_alloc_atomic(d);
                x->vector.offset = 0;
                break;
        default: {
                cl_index elt_size = ecl_aet_size[t];
                d *= elt_size;
                x->vector.self.bc = (ecl_base_char *)ecl_alloc_atomic_align(d, elt_size);
        }
        }
}

cl_object
ecl_alloc_simple_vector(cl_index l, cl_elttype aet)
{
	cl_object x;

	switch (aet) {
	case ecl_aet_bc:
                x = ecl_alloc_compact_object(t_base_string, l+1);
                x->base_string.self = ECL_COMPACT_OBJECT_EXTRA(x);
		x->base_string.self[l] = 0;
                break;
#ifdef ECL_UNICODE
	case ecl_aet_ch:
                {
                cl_index bytes = sizeof(ecl_character) * l;
                x = ecl_alloc_compact_object(t_string, bytes);
                x->string.self = ECL_COMPACT_OBJECT_EXTRA(x);
                }
                break;
#endif
	case ecl_aet_bit:
                {
                cl_index bytes = (l + (CHAR_BIT-1))/CHAR_BIT;
                x = ecl_alloc_compact_object(t_bitvector, bytes);
                x->vector.self.bit = ECL_COMPACT_OBJECT_EXTRA(x);
		x->vector.offset = 0;
                }
		break;
        case ecl_aet_object:
                {
		x = ecl_alloc_object(t_vector);
                x->vector.self.t = alloc_pointerfull_memory(l);
                }
                break;
	default:
		x = ecl_alloc_compact_object(t_vector, l * ecl_aet_size[aet]);
                x->vector.self.bc = ECL_COMPACT_OBJECT_EXTRA(x);
	}
        x->base_string.elttype = aet;
        x->base_string.flags = 0; /* no fill pointer, not adjustable */
        x->base_string.displaced = ECL_NIL;
        x->base_string.dim = x->base_string.fillp = l;
	return x;
}

cl_elttype
ecl_symbol_to_elttype(cl_object x)
{
 BEGIN:
	if (x == @'base-char')
		return(ecl_aet_bc);
#ifdef ECL_UNICODE
	if (x == @'character')
		return(ecl_aet_ch);
#endif
	else if (x == @'bit')
		return(ecl_aet_bit);
	else if (x == @'ext::cl-fixnum')
		return(ecl_aet_fix);
	else if (x == @'ext::cl-index')
		return(ecl_aet_index);
	else if (x == @'single-float' || x == @'short-float')
		return(ecl_aet_sf);
	else if (x == @'double-float')
		return(ecl_aet_df);
	else if (x == @'long-float') {
#ifdef ECL_LONG_FLOAT
		return(ecl_aet_object);
#else
		return(ecl_aet_df);
#endif
	} else if (x == @'ext::byte8')
		return(ecl_aet_b8);
	else if (x == @'ext::integer8')
		return(ecl_aet_i8);
#ifdef ecl_uint16_t
	else if (x == @'ext::byte16')
		return(ecl_aet_b16);
	else if (x == @'ext::integer16')
		return(ecl_aet_i16);
#endif
#ifdef ecl_uint32_t
	else if (x == @'ext::byte32')
		return(ecl_aet_b32);
	else if (x == @'ext::integer32')
		return(ecl_aet_i32);
#endif
#ifdef ecl_uint64_t
	else if (x == @'ext::byte64')
		return(ecl_aet_b64);
	else if (x == @'ext::integer64')
		return(ecl_aet_i64);
#endif
	else if (x == @'t')
		return(ecl_aet_object);
	else if (x == ECL_NIL) {
		FEerror("ECL does not support arrays with element type NIL", 0);
	}
	x = cl_upgraded_array_element_type(1, x);
	goto BEGIN;
}

cl_object
ecl_elttype_to_symbol(cl_elttype aet)
{
        return ecl_aet_name[aet];
}

cl_object
si_array_element_type_byte_size(cl_object type) {
        cl_elttype aet = ECL_ARRAYP(type) ?
                type->array.elttype :
                ecl_symbol_to_elttype(type);
	cl_object size = ecl_make_fixnum(ecl_aet_size[aet]);
	if (aet == ecl_aet_bit)
		size = ecl_make_ratio(ecl_make_fixnum(1),ecl_make_fixnum(CHAR_BIT));
	@(return size ecl_elttype_to_symbol(aet))
}

static void *
address_inc(void *address, cl_fixnum inc, cl_elttype elt_type)
{
	union ecl_array_data aux;
	aux.t = address;
	switch (elt_type) {
	case ecl_aet_object:
		return aux.t + inc;
	case ecl_aet_fix:
		return aux.fix + inc;
	case ecl_aet_index:
		return aux.fix + inc;
	case ecl_aet_sf:
		return aux.sf + inc;
	case ecl_aet_bc:
		return aux.bc + inc;
#ifdef ECL_UNICODE
	case ecl_aet_ch:
                return aux.c + inc;
#endif
	case ecl_aet_df:
		return aux.df + inc;
	case ecl_aet_b8:
	case ecl_aet_i8:
		return aux.b8 + inc;
#ifdef ecl_uint16_t
	case ecl_aet_b16:
	case ecl_aet_i16:
		return aux.b16 + inc;
#endif
#ifdef ecl_uint32_t
	case ecl_aet_b32:
	case ecl_aet_i32:
		return aux.b32 + inc;
#endif
#ifdef ecl_uint64_t
	case ecl_aet_b64:
	case ecl_aet_i64:
		return aux.b64 + inc;
#endif
	default:
		FEbad_aet();
	}
}

cl_object
cl_array_element_type(cl_object a)
{
	@(return ecl_elttype_to_symbol(ecl_array_elttype(a)))
}

/*
	Displace(from, to, offset) displaces the from-array
	to the to-array (the original array) by the specified offset.
	It changes the a_displaced field of both arrays.
	The field is a cons; the car of the from-array points to
	the to-array and the cdr of the to-array is a list of arrays
	displaced to the to-array, so the from-array is pushed to the
	cdr of the to-array's array.displaced.
*/
void
ecl_displace(cl_object from, cl_object to, cl_object offset)
{
	cl_index j;
	void *base;
	cl_elttype totype, fromtype;
	fromtype = from->array.elttype;
        if (ecl_unlikely(!ECL_FIXNUMP(offset) || ((j = ecl_fixnum(offset)) < 0))) {
                FEwrong_type_key_arg(@[adjust-array], @[:displaced-index-offset],
                                     offset, @[fixnum]);
        }
	if (ecl_t_of(to) == t_foreign) {
		if (fromtype == ecl_aet_bit || fromtype == ecl_aet_object) {
			FEerror("Cannot displace arrays with element type T or BIT onto foreign data",0);
		}
		base = to->foreign.data;
		from->array.displaced = to;
	} else {
                cl_fixnum maxdim;
		totype = to->array.elttype;
		if (totype != fromtype)
			FEerror("Cannot displace the array, "
                                "because the element types don't match.", 0);
                maxdim = to->array.dim - from->array.dim;
		if (maxdim < 0)
			FEerror("Cannot displace the array, "
                                "because the total size of the to-array"
                                "is too small.", 0);
                if (j > maxdim) {
                        cl_object type = ecl_make_integer_type(ecl_make_fixnum(0),
                                                               ecl_make_fixnum(maxdim));
                        FEwrong_type_key_arg(@[adjust-array], @[:displaced-index-offset],
                                             offset, type);
                }
		from->array.displaced = ecl_list1(to);
		/* We only need to keep track of the arrays that displace to us
		 * when this one array is adjustable */
		if (ECL_ADJUSTABLE_ARRAY_P(to)) {
			cl_object track_list = to->array.displaced;
			if (Null(track_list))
				to->array.displaced =
					track_list = ecl_list1(ECL_NIL);
			ECL_RPLACD(track_list,
				   CONS(from, ECL_CONS_CDR(track_list)));
		}
		if (fromtype == ecl_aet_bit) {
			j += to->vector.offset;
			from->vector.offset = j%CHAR_BIT;
			from->vector.self.bit = to->vector.self.bit + j/CHAR_BIT;
			return;
		}
		base = to->array.self.t;
	}
	from->array.self.t = address_inc(base, j, fromtype);
}

cl_object
si_array_raw_data(cl_object x)
{
        cl_elttype et = ecl_array_elttype(x);
        cl_index total_size = x->vector.dim * ecl_aet_size[et];
        cl_object output, to_array;
        uint8_t *data;
        if (et == ecl_aet_object) {
                FEerror("EXT:ARRAY-RAW-DATA can not get data "
                        "from an array with element type T.", 0);
        }
        data = x->vector.self.b8;
        to_array = x->array.displaced;
        if (to_array == ECL_NIL || ((to_array = ECL_CONS_CAR(to_array)) == ECL_NIL)) {
		cl_index used_size = total_size;
		int flags = 0;
		if (ECL_ARRAY_HAS_FILL_POINTER_P(x)) {
			used_size = x->vector.fillp * ecl_aet_size[et];
			flags = ECL_FLAG_HAS_FILL_POINTER;
		}
                output = ecl_alloc_object(t_vector);
                output->vector.elttype = ecl_aet_b8;
                output->vector.self.b8 = data;
                output->vector.dim = total_size;
		output->vector.fillp = used_size;
                output->vector.flags = flags;
                output->vector.displaced = ECL_NIL;
        } else {
                cl_index displ = data - to_array->vector.self.b8;
		cl_object fillp = ECL_NIL;
		if (ECL_ARRAY_HAS_FILL_POINTER_P(x)) {
			fillp = ecl_make_fixnum(x->vector.fillp * ecl_aet_size[et]);
		}
                output = si_make_vector(@'ext::byte8',
                                        ecl_make_fixnum(total_size),
                                        ECL_NIL,
                                        fillp,
                                        si_array_raw_data(to_array),
                                        ecl_make_fixnum(displ));
        }
        @(return output)
}

cl_elttype
ecl_array_elttype(cl_object x)
{
        if (ecl_unlikely(!ECL_ARRAYP(x)))
                FEwrong_type_argument(@[array], x);
        return x->array.elttype;
}

cl_object
cl_array_rank(cl_object a)
{
	@(return ecl_make_fixnum(ecl_array_rank(a)))
}

cl_index
ecl_array_rank(cl_object a)
{
	switch (ecl_t_of(a)) {
	case t_array:
		return a->array.rank;
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_base_string:
	case t_vector:
	case t_bitvector:
                return 1;
	default:
                FEwrong_type_only_arg(@[array-dimension], a, @[array]);
	}
}

cl_object
cl_array_dimension(cl_object a, cl_object index)
{
	@(return ecl_make_fixnum(ecl_array_dimension(a, ecl_to_size(index))))
}

cl_index
ecl_array_dimension(cl_object a, cl_index index)
{
	switch (ecl_t_of(a)) {
	case t_array: {
                if (ecl_unlikely(index > a->array.rank))
                        FEwrong_dimensions(a, index+1);
		return a->array.dims[index];
	}
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_base_string:
	case t_vector:
	case t_bitvector:
                if (ecl_unlikely(index))
                        FEwrong_dimensions(a, index+1);
		return a->vector.dim;
	default:
                FEwrong_type_only_arg(@[array-dimension], a, @[array]);
	}
}

cl_object
cl_array_total_size(cl_object a)
{
        if (ecl_unlikely(!ECL_ARRAYP(a)))
                FEwrong_type_only_arg(@[array-total-size], a, @[array]);
	@(return ecl_make_fixnum(a->array.dim))
}

cl_object
cl_adjustable_array_p(cl_object a)
{
        if (ecl_unlikely(!ECL_ARRAYP(a)))
                FEwrong_type_only_arg(@[adjustable-array-p], a, @[array]);
	@(return (ECL_ADJUSTABLE_ARRAY_P(a) ? ECL_T : ECL_NIL))
}

/*
	Internal function for checking if an array is displaced.
*/
cl_object
cl_array_displacement(cl_object a)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_object to_array;
	cl_index offset;

        if (ecl_unlikely(!ECL_ARRAYP(a)))
                FEwrong_type_only_arg(@[array-displacement], a, @[array]);
	to_array = a->array.displaced;
	if (Null(to_array)) {
		offset = 0;
	} else if (Null(to_array = CAR(a->array.displaced))) {
		offset = 0;
	} else {
		switch (a->array.elttype) {
		case ecl_aet_object:
			offset = a->array.self.t - to_array->array.self.t;
			break;
		case ecl_aet_bc:
			offset = a->array.self.bc - to_array->array.self.bc;
			break;
#ifdef ECL_UNICODE
		case ecl_aet_ch:
			offset = a->array.self.c - to_array->array.self.c;
			break;
#endif
		case ecl_aet_bit:
			offset = a->array.self.bit - to_array->array.self.bit;
			offset = offset * CHAR_BIT + a->array.offset
				- to_array->array.offset;
			break;
		case ecl_aet_fix:
			offset = a->array.self.fix - to_array->array.self.fix;
			break;
		case ecl_aet_index:
			offset = a->array.self.fix - to_array->array.self.fix;
			break;
		case ecl_aet_sf:
			offset = a->array.self.sf - to_array->array.self.sf;
			break;
		case ecl_aet_df:
			offset = a->array.self.df - to_array->array.self.df;
			break;
		case ecl_aet_b8:
		case ecl_aet_i8:
			offset = a->array.self.b8 - to_array->array.self.b8;
			break;
#ifdef ecl_uint16_t
		case ecl_aet_b16:
		case ecl_aet_i16:
			offset = a->array.self.b16 - to_array->array.self.b16;
			break;
#endif
#ifdef ecl_uint32_t
		case ecl_aet_b32:
		case ecl_aet_i32:
			offset = a->array.self.b32 - to_array->array.self.b32;
			break;
#endif
#ifdef ecl_uint64_t
		case ecl_aet_b64:
		case ecl_aet_i64:
			offset = a->array.self.b64 - to_array->array.self.b64;
			break;
#endif
		default:
			FEbad_aet();
		}
	}
	ecl_return2(the_env, to_array, ecl_make_fixnum(offset));
}

cl_object
cl_svref(cl_object x, cl_object index)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_index i;

	if (ecl_unlikely(ecl_t_of(x) != t_vector ||
                         (x->vector.flags & (ECL_FLAG_ADJUSTABLE | ECL_FLAG_HAS_FILL_POINTER)) ||
                         CAR(x->vector.displaced) != ECL_NIL ||
                         (cl_elttype)x->vector.elttype != ecl_aet_object))
	{
                FEwrong_type_nth_arg(@[svref],1,x,@[simple-vector]);
	}
        i = checked_index(@[svref], x, -1, index, x->vector.dim);
	ecl_return1(the_env, x->vector.self.t[i]);
}

cl_object
si_svset(cl_object x, cl_object index, cl_object v)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_index i;

	if (ecl_unlikely(ecl_t_of(x) != t_vector ||
                         (x->vector.flags & (ECL_FLAG_ADJUSTABLE | ECL_FLAG_HAS_FILL_POINTER)) ||
                         CAR(x->vector.displaced) != ECL_NIL ||
                         (cl_elttype)x->vector.elttype != ecl_aet_object))
	{
		FEwrong_type_nth_arg(@[si::svset],1,x,@[simple-vector]);
	}
        i = checked_index(@[svref], x, -1, index, x->vector.dim);
	ecl_return1(the_env, x->vector.self.t[i] = v);
}

cl_object
cl_array_has_fill_pointer_p(cl_object a)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_object r;
	switch (ecl_t_of(a)) {
	case t_array:
		r = ECL_NIL; break;
	case t_vector:
	case t_bitvector:
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_base_string:
		r = ECL_ARRAY_HAS_FILL_POINTER_P(a)? ECL_T : ECL_NIL;
		break;
	default:
                FEwrong_type_nth_arg(@[array-has-fill-pointer-p],1,a,@[array]);
	}
	ecl_return1(the_env, r);
}

cl_object
cl_fill_pointer(cl_object a)
{
	const cl_env_ptr the_env = ecl_process_env();
        if (ecl_unlikely(!ECL_VECTORP(a)))
                FEwrong_type_only_arg(@[fill-pointer], a, @[vector]);
	if (ecl_unlikely(!ECL_ARRAY_HAS_FILL_POINTER_P(a))) {
                const char *type = "(AND VECTOR (SATISFIES ARRAY-HAS-FILL-POINTER-P))";
		FEwrong_type_nth_arg(@[fill-pointer], 1, a, ecl_read_from_cstring(type));
	}
	ecl_return1(the_env, ecl_make_fixnum(a->vector.fillp));
}

/*
	Internal function for setting fill pointer.
*/
cl_object
si_fill_pointer_set(cl_object a, cl_object fp)
{
	const cl_env_ptr the_env = ecl_process_env();
        cl_fixnum i;
        if (ecl_unlikely(!ECL_VECTORP(a) || !ECL_ARRAY_HAS_FILL_POINTER_P(a))) {
                const char *type = "(AND VECTOR (SATISFIES ARRAY-HAS-FILL-POINTER-P))";
		FEwrong_type_nth_arg(@[adjust-array], 1, a,
                                     ecl_read_from_cstring(type));
        }
        if (ecl_unlikely(!ECL_FIXNUMP(fp) || ((i = ecl_fixnum(fp)) < 0) ||
                         (i > a->vector.dim))) {
                cl_object type = ecl_make_integer_type(ecl_make_fixnum(0),
                                                       ecl_make_fixnum(a->vector.dim-1));
                FEwrong_type_key_arg(@[adjust-array], @[:fill-pointer], fp, type);
        }
        a->vector.fillp = i;
	ecl_return1(the_env, fp);
}

/*
	Internal function for replacing the contents of arrays:

		(si:replace-array old-array new-array).

	Used in ADJUST-ARRAY.
*/
cl_object
si_replace_array(cl_object olda, cl_object newa)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_object dlist;
	if (ecl_t_of(olda) != ecl_t_of(newa)
	    || (ecl_t_of(olda) == t_array && olda->array.rank != newa->array.rank))
		goto CANNOT;
	if (!ECL_ADJUSTABLE_ARRAY_P(olda)) {
		/* When an array is not adjustable, we simply output the new array */
		olda = newa;
		goto OUTPUT;
	}
	for (dlist = CDR(olda->array.displaced); dlist != ECL_NIL; dlist = CDR(dlist)) {
		cl_object other_array = CAR(dlist);
		cl_object offset;
		cl_array_displacement(other_array);
		offset = ecl_nth_value(the_env, 1);
		ecl_displace(other_array, newa, offset);
	}
	switch (ecl_t_of(olda)) {
	case t_array:
	case t_vector:
	case t_bitvector:
		olda->array = newa->array;
		break;
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_base_string:
		olda->base_string = newa->base_string;
		break;
	default:
	CANNOT:
		FEerror("Cannot replace the array ~S by the array ~S.",
			2, olda, newa);
	}
 OUTPUT:
	ecl_return1(the_env, olda);
}

void
ecl_copy_subarray(cl_object dest, cl_index i0, cl_object orig,
		  cl_index i1, cl_index l)
{
	cl_elttype t = ecl_array_elttype(dest);
	if (i0 + l > dest->array.dim) {
		l = dest->array.dim - i0;
	}
	if (i1 + l > orig->array.dim) {
		l = orig->array.dim - i1;
	}
        if (t != ecl_array_elttype(orig) || t == ecl_aet_bit) {
                if (dest == orig && i0 > i1) {
                        for (i0 += l, i1 += l; l--; ) {
                                ecl_aset_unsafe(dest, --i0,
                                                ecl_aref_unsafe(orig, --i1));
                        }
                } else {
                        while (l--) {
                                ecl_aset_unsafe(dest, i0++,
                                                ecl_aref_unsafe(orig, i1++));
                        }
                }
        } else {
                /* We could have singled out also dest == orig and used memcpy
                 * but gcc-4.6 breaks this code even when i0 < i1 if the regions
                 * overlap sufficiently. */
                cl_index elt_size = ecl_aet_size[t];
                memmove(dest->array.self.bc + i0 * elt_size,
                        orig->array.self.bc + i1 * elt_size,
                        l * elt_size);
        }
}

void
ecl_reverse_subarray(cl_object x, cl_index i0, cl_index i1)
{
	cl_elttype t = ecl_array_elttype(x);
	cl_index i, j;
	if (x->array.dim == 0) {
		return;
	}
	if (i1 >= x->array.dim) {
		i1 = x->array.dim;
	}
	switch (t) {
	case ecl_aet_object:
	case ecl_aet_fix:
	case ecl_aet_index:
		for (i = i0, j = i1-1;  i < j;  i++, --j) {
			cl_object y = x->vector.self.t[i];
			x->vector.self.t[i] = x->vector.self.t[j];
			x->vector.self.t[j] = y;
		}
		break;
	case ecl_aet_sf:
		for (i = i0, j = i1-1;  i < j;  i++, --j) {
			float y = x->array.self.sf[i];
			x->array.self.sf[i] = x->array.self.sf[j];
			x->array.self.sf[j] = y;
		}
		break;
	case ecl_aet_df:
		for (i = i0, j = i1-1;  i < j;  i++, --j) {
			double y = x->array.self.df[i];
			x->array.self.df[i] = x->array.self.df[j];
			x->array.self.df[j] = y;
		}
		break;
	case ecl_aet_bc:
		for (i = i0, j = i1-1;  i < j;  i++, --j) {
			ecl_base_char y = x->array.self.bc[i];
			x->array.self.bc[i] = x->array.self.bc[j];
                        x->array.self.bc[j] = y;
		}
		break;
	case ecl_aet_b8:
        case ecl_aet_i8:
		for (i = i0, j = i1-1;  i < j;  i++, --j) {
			ecl_uint8_t y = x->array.self.b8[i];
			x->array.self.b8[i] = x->array.self.b8[j];
			x->array.self.b8[j] = y;
		}
		break;
#ifdef ecl_uint16_t
	case ecl_aet_b16:
        case ecl_aet_i16:
		for (i = i0, j = i1-1;  i < j;  i++, --j) {
			ecl_uint16_t y = x->array.self.b16[i];
			x->array.self.b16[i] = x->array.self.b16[j];
			x->array.self.b16[j] = y;
		}
		break;
#endif
#ifdef ecl_uint32_t
	case ecl_aet_b32:
        case ecl_aet_i32:
		for (i = i0, j = i1-1;  i < j;  i++, --j) {
			ecl_uint32_t y = x->array.self.b32[i];
			x->array.self.b32[i] = x->array.self.b32[j];
			x->array.self.b32[j] = y;
		}
		break;
#endif
#ifdef ecl_uint64_t
	case ecl_aet_b64:
        case ecl_aet_i64:
		for (i = i0, j = i1-1;  i < j;  i++, --j) {
			ecl_uint64_t y = x->array.self.b64[i];
			x->array.self.b64[i] = x->array.self.b64[j];
			x->array.self.b64[j] = y;
		}
		break;
#endif
#ifdef ECL_UNICODE
	case ecl_aet_ch:
		for (i = i0, j = i1-1;  i < j;  i++, --j) {
			ecl_character y = x->array.self.c[i];
			x->array.self.c[i] = x->array.self.c[j];
                        x->array.self.c[j] = y;
		}
		break;
#endif
	case ecl_aet_bit:
		for (i = i0 + x->vector.offset,
		     j = i1 + x->vector.offset - 1;
		     i < j;
		     i++, --j) {
			int k = x->array.self.bit[i/CHAR_BIT]&(0200>>i%CHAR_BIT);
			if (x->array.self.bit[j/CHAR_BIT]&(0200>>j%CHAR_BIT))
				x->array.self.bit[i/CHAR_BIT]
				|= 0200>>i%CHAR_BIT;
			else
				x->array.self.bit[i/CHAR_BIT]
				&= ~(0200>>i%CHAR_BIT);
			if (k)
				x->array.self.bit[j/CHAR_BIT]
				|= 0200>>j%CHAR_BIT;
			else
				x->array.self.bit[j/CHAR_BIT]
				&= ~(0200>>j%CHAR_BIT);
		}
		break;
	default:
		FEbad_aet();
	}
}

cl_object
si_copy_subarray(cl_object dest, cl_object start0,
                 cl_object orig, cl_object start1, cl_object length)
{
        ecl_copy_subarray(dest, ecl_to_size(start0),
                          orig, ecl_to_size(start1),
                          ecl_to_size(length));
        @(return dest)
}

cl_object
si_fill_array_with_elt(cl_object x, cl_object elt, cl_object start, cl_object end)
{
	cl_elttype t = ecl_array_elttype(x);
        cl_index first = ecl_to_size(start);
        cl_index last = Null(end)? x->array.dim : ecl_to_size(end);
        if (first >= last) {
                goto END;
        }
	switch (t) {
	case ecl_aet_object: {
                cl_object *p = x->vector.self.t + first;
		for (first = last - first; first; --first, ++p) { *p = elt; }
		break;
        }
	case ecl_aet_bc: {
                ecl_base_char e = ecl_char_code(elt);
                ecl_base_char *p = x->vector.self.bc + first;
		for (first = last - first; first; --first, ++p) { *p = e; }
		break;
        }
#ifdef ECL_UNICODE
	case ecl_aet_ch: {
                ecl_character e = ecl_char_code(elt);
                ecl_character *p = x->vector.self.c + first;
		for (first = last - first; first; --first, ++p) { *p = e; }
		break;
        }
#endif
	case ecl_aet_fix: {
                cl_fixnum e = ecl_to_fix(elt);
                cl_fixnum *p = x->vector.self.fix + first;
		for (first = last - first; first; --first, ++p) { *p = e; }
		break;
        }
	case ecl_aet_index: {
                cl_index e = ecl_to_size(elt);
                cl_index *p = x->vector.self.index + first;
		for (first = last - first; first; --first, ++p) { *p = e; }
		break;
        }
	case ecl_aet_sf: {
                float e = ecl_to_float(elt);
                float *p = x->vector.self.sf + first;
		for (first = last - first; first; --first, ++p) { *p = e; }
		break;
        }
	case ecl_aet_df: {
                double e = ecl_to_double(elt);
                double *p = x->vector.self.df + first;
		for (first = last - first; first; --first, ++p) { *p = e; }
		break;
        }
	case ecl_aet_b8: {
                uint8_t e = ecl_to_uint8_t(elt);
                uint8_t *p = x->vector.self.b8 + first;
		for (first = last - first; first; --first, ++p) { *p = e; }
		break;
        }
	case ecl_aet_i8: {
                int8_t e = ecl_to_int8_t(elt);
                int8_t *p = x->vector.self.i8 + first;
		for (first = last - first; first; --first, ++p) { *p = e; }
		break;
        }
#ifdef ecl_uint16_t
	case ecl_aet_b16: {
                ecl_uint16_t e = ecl_to_uint16_t(elt);
                ecl_uint16_t *p = x->vector.self.b16 + first;
		for (first = last - first; first; --first, ++p) { *p = e; }
		break;
        }
	case ecl_aet_i16: {
                ecl_int16_t e = ecl_to_int16_t(elt);
                ecl_int16_t *p = x->vector.self.i16 + first;
		for (first = last - first; first; --first, ++p) { *p = e; }
		break;
        }
#endif
#ifdef ecl_uint32_t
	case ecl_aet_b32: {
                ecl_uint32_t e = ecl_to_uint32_t(elt);
                ecl_uint32_t *p = x->vector.self.b32 + first;
		for (first = last - first; first; --first, ++p) { *p = e; }
		break;
        }
	case ecl_aet_i32: {
                ecl_int32_t e = ecl_to_int32_t(elt);
                ecl_int32_t *p = x->vector.self.i32 + first;
		for (first = last - first; first; --first, ++p) { *p = e; }
		break;
        }
#endif
#ifdef ecl_uint64_t
	case ecl_aet_b64: {
                ecl_uint64_t e = ecl_to_uint64_t(elt);
                ecl_uint64_t *p = x->vector.self.b64 + first;
		for (first = last - first; first; --first, ++p) { *p = e; }
		break;
        }
	case ecl_aet_i64: {
                ecl_int64_t e = ecl_to_int64_t(elt);
                ecl_int64_t *p = x->vector.self.i64 + first;
		for (first = last - first; first; --first, ++p) { *p = e; }
		break;
        }
#endif
	case ecl_aet_bit: {
                int i = ecl_to_bit(elt);
		for (last -= first, first += x->vector.offset; last; --last, ++first) {
                        int mask = 0200>>first%CHAR_BIT;
                        if (i == 0)
                                x->vector.self.bit[first/CHAR_BIT] &= ~mask;
                        else
                                x->vector.self.bit[first/CHAR_BIT] |= mask;
		}
		break;
        }
	default:
		FEbad_aet();
	}
 END:
        @(return x)
}
