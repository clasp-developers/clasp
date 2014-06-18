/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    predicate.c -- Predicates.
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

#include <string.h>
#define ECL_INCLUDE_MATH_H
#include <ecl/ecl.h>
#include <ecl/internal.h>

cl_object
cl_identity(cl_object x)
{
	@(return x)
}

cl_object
cl_null(cl_object x)
{
	@(return (Null(x) ? ECL_T : ECL_NIL))
}

cl_object
cl_symbolp(cl_object x)
{
	@(return (ECL_SYMBOLP(x) ? ECL_T : ECL_NIL))
}

cl_object
cl_atom(cl_object x)
{
	@(return (ECL_ATOM(x) ? ECL_T : ECL_NIL))
}

cl_object
cl_consp(cl_object x)
{
	@(return (CONSP(x) ? ECL_T : ECL_NIL))
}

cl_object
cl_listp(cl_object x)
{
	@(return ((Null(x) || CONSP(x)) ? ECL_T : ECL_NIL))
}

cl_object
cl_numberp(cl_object x)
{
	cl_type t = ecl_t_of(x);
	@(return (ECL_NUMBER_TYPE_P(t) ? ECL_T : ECL_NIL))
}

/*	Used in compiled code		*/
bool ecl_numberp(cl_object x)
{
        cl_type t = ecl_t_of(x);
        return ECL_NUMBER_TYPE_P(t);
}

cl_object
cl_integerp(cl_object x)
{
	cl_type t = ecl_t_of(x);
	@(return ((t == t_fixnum || t == t_bignum) ? ECL_T : ECL_NIL))
}

cl_object
cl_rationalp(cl_object x)
{
	cl_type t = ecl_t_of(x);
	@(return ((t == t_fixnum || t == t_bignum || t == t_ratio) ? ECL_T : ECL_NIL))
}

cl_object
cl_floatp(cl_object x)
{
	@(return (floatp(x)? ECL_T : ECL_NIL))
}

bool
floatp(cl_object x)
{
	cl_type t = ecl_t_of(x);
	return (t == t_singlefloat) || (t == t_doublefloat)
#ifdef ECL_LONG_FLOAT
		|| (t == t_longfloat)
#endif
		;
}

cl_object
cl_realp(cl_object x)
{
	cl_type t = ecl_t_of(x);
	@(return (ECL_REAL_TYPE_P(t) ? ECL_T : ECL_NIL))
}

bool
ecl_realp(cl_object x)
{
        cl_type t = ecl_t_of(x);
        return ECL_REAL_TYPE_P(t);
}

cl_object
cl_complexp(cl_object x)
{
	@(return (ECL_COMPLEXP(x) ? ECL_T : ECL_NIL))
}

cl_object
cl_characterp(cl_object x)
{
	@(return (ECL_CHARACTERP(x) ? ECL_T : ECL_NIL))
}

#ifdef ECL_UNICODE
cl_object
si_base_char_p(cl_object c)
{
	@(return ((ECL_CHARACTERP(c) && ECL_BASE_CHAR_P(c))? ECL_T : ECL_NIL))
}
#endif

bool
ecl_stringp(cl_object x)
{
	cl_type t = ecl_t_of(x);
#ifdef ECL_UNICODE
	return t == t_base_string || t == t_string;
#else
	return t == t_base_string;
#endif
}

cl_object
cl_stringp(cl_object x)
{
	@(return (ECL_STRINGP(x)? ECL_T : ECL_NIL))
}

cl_object
cl_bit_vector_p(cl_object x)
{
	@(return (ECL_BIT_VECTOR_P(x) ? ECL_T : ECL_NIL))
}

cl_object
cl_vectorp(cl_object x)
{
	@(return (ECL_VECTORP(x) ? ECL_T : ECL_NIL))
}

cl_object
cl_simple_string_p(cl_object x)
{
	@(return ((ECL_STRINGP(x) &&
                   !ECL_ADJUSTABLE_ARRAY_P(x) &&
                   !ECL_ARRAY_HAS_FILL_POINTER_P(x) &&
                   Null(CAR(x->base_string.displaced))) ? ECL_T : ECL_NIL))
}

#ifdef ECL_UNICODE
cl_object
si_base_string_p(cl_object x)
{
	@(return (ECL_BASE_STRING_P(x) ? ECL_T : ECL_NIL))
}
#endif

cl_object
cl_simple_bit_vector_p(cl_object x)
{
	@(return ((ECL_BIT_VECTOR_P(x) &&
                   !ECL_ADJUSTABLE_ARRAY_P(x) &&
                   !ECL_ARRAY_HAS_FILL_POINTER_P(x) &&
                   Null(CAR(x->vector.displaced))) ? ECL_T : ECL_NIL))
}

cl_object
cl_simple_vector_p(cl_object x)
{
	cl_type t = ecl_t_of(x);
	@(return ((t == t_vector &&
                   !ECL_ADJUSTABLE_ARRAY_P(x) &&
                   !ECL_ARRAY_HAS_FILL_POINTER_P(x) &&
                   Null(CAR(x->vector.displaced)) &&
                   (cl_elttype)x->vector.elttype == ecl_aet_object) ? ECL_T : ECL_NIL))
}

cl_object
cl_arrayp(cl_object x)
{
	@(return (ECL_ARRAYP(x) ? ECL_T : ECL_NIL))
}

cl_object
cl_packagep(cl_object x)
{
	@(return (ECL_PACKAGEP(x) ? ECL_T : ECL_NIL))
}

cl_object
cl_functionp(cl_object x)
{
	cl_type t;
	cl_object output;

	t = ecl_t_of(x);
	if (t == t_bytecodes || t == t_bclosure || t == t_cfun
	    || t == t_cfunfixed || t == t_cclosure
#ifdef CLOS
	    || (t == t_instance && x->instance.isgf)
#endif
	    )
		output = ECL_T;
	else
		output = ECL_NIL;
	@(return output)
}

cl_object
cl_compiled_function_p(cl_object x)
{
	cl_type t = ecl_t_of(x);
	@(return ((t == t_bytecodes || t == t_bclosure || t == t_cfun
		   || t == t_cfunfixed || t == t_cclosure) ? ECL_T : ECL_NIL))
}

cl_object
cl_eq(cl_object x, cl_object y)
{
	@(return ((x == y) ? ECL_T : ECL_NIL))
}

/*
 * EQL-comparison of floats. If we are using signed zeros and NaNs,
 * numeric comparison of floating points is not equivalent to bit-wise
 * equality. In particular every two NaNs always give false
 *	(= #1=(/ 0.0 0.0) #1#) => NIL
 * and signed zeros always compare equal
 *	(= 0 -0.0) => T
 * which is not the same as what EQL should return
 *	(EQL #1=(/ 0.0 0.0) #1#) => T
 *	(EQL 0 -0.0) => NIL
 *
 * Furthermore, we can not use bit comparisons because in some platforms
 * long double has unused bits that makes two long floats be = but not eql.
 */
#if !defined(ECL_SIGNED_ZERO) && !defined(ECL_IEEE_FP)
# define FLOAT_EQL(a,b,type) return (a) == (b)
#else
# define FLOAT_EQL(a,b,type) {                          \
        type xa = (a), xb = (b);                        \
        if (xa == xb) {                                 \
                return signbit(xa) == signbit(xb);      \
        } else if (isnan(xa) || isnan(xb)) {            \
                return !memcmp(&xa, &xb, sizeof(type)); \
        } else {                                        \
                return 0;                               \
        } }
#endif

bool
ecl_eql(cl_object x, cl_object y)
{
	if (x == y)
		return TRUE;
        if (ECL_IMMEDIATE(x) || ECL_IMMEDIATE(y))
                return FALSE;
        if (x->d.t != y->d.t)
                return FALSE;
	switch (x->d.t) {
	case t_bignum:
		return (_ecl_big_compare(x, y) == 0);
	case t_ratio:
		return (ecl_eql(x->ratio.num, y->ratio.num) &&
			ecl_eql(x->ratio.den, y->ratio.den));
	case t_singlefloat:
		FLOAT_EQL(ecl_single_float(x), ecl_single_float(y), float);
	case t_doublefloat:
		FLOAT_EQL(ecl_double_float(x), ecl_double_float(y), double);
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		FLOAT_EQL(ecl_long_float(x), ecl_long_float(y), long double);
#endif
	case t_complex:
		return (ecl_eql(x->complex.real, y->complex.real) &&
			ecl_eql(x->complex.imag, y->complex.imag));
#ifdef ECL_SSE2
	case t_sse_pack:
		return !memcmp(x->sse.data.b8, y->sse.data.b8, 16);
#endif
	default:
		return FALSE;
	}
}

cl_object
cl_eql(cl_object x, cl_object y)
{
	@(return (ecl_eql(x, y) ? ECL_T : ECL_NIL))
}

bool
ecl_equal(register cl_object x, cl_object y)
{
	cl_type tx, ty;
BEGIN:
	if (x==y)
		return(TRUE);
	tx = ecl_t_of(x);
	ty = ecl_t_of(y);
	switch (tx) {
	case t_list:
		if (Null(x) || Null(y)) {
			/* If X is NIL, then X and Y must be EQ */
			return FALSE;
		}
		if (tx != ty || !ecl_equal(CAR(x), CAR(y)))
			return FALSE;
		x = CDR(x);
		y = CDR(y);
		goto BEGIN;
	case t_symbol:
	case t_vector:
	case t_array:
	case t_fixnum:
		return FALSE;
	case t_bignum:
		return (tx == ty) && (_ecl_big_compare(x,y) == 0);
	case t_ratio:
		return (tx == ty) && ecl_eql(x->ratio.num, y->ratio.num) &&
			ecl_eql(x->ratio.den, y->ratio.den);
	case t_singlefloat: {
                if (tx != ty) return 0;
                FLOAT_EQL(ecl_single_float(x), ecl_single_float(y), float);
        }
        case t_doublefloat: {
                if (tx != ty) return 0;
                FLOAT_EQL(ecl_double_float(x), ecl_double_float(y), double);
        }
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {
                if (tx != ty) return 0;
                FLOAT_EQL(ecl_long_float(x), ecl_long_float(y), long double);
        }
#endif
	case t_complex:
		return (tx == ty) && ecl_eql(x->complex.real, y->complex.real) &&
			ecl_eql(x->complex.imag, y->complex.imag);
	case t_character:
		return (tx == ty) && (ECL_CHAR_CODE(x) == ECL_CHAR_CODE(y));
	case t_base_string:
#ifdef ECL_UNICODE
	case t_string:
		if (ty != t_base_string && ty != t_string)
			return FALSE;
#else
		if (ty != t_base_string)
			return FALSE;
#endif
		return ecl_string_eq(x, y);
	case t_bitvector: {
		cl_index i, ox, oy;
		if (ty != tx)
			return FALSE;
		if (x->vector.fillp != y->vector.fillp)
			return(FALSE);
		ox = x->vector.offset;
		oy = y->vector.offset;
		for (i = 0;  i < x->vector.fillp;  i++)
			if((x->vector.self.bit[(i+ox)/8] & (0200>>(i+ox)%8))
			 !=(y->vector.self.bit[(i+oy)/8] & (0200>>(i+oy)%8)))
				return(FALSE);
		return(TRUE);
	}
	case t_pathname:
		return ty == tx &&
		       ecl_equal(x->pathname.host, y->pathname.host) &&
		       ecl_equal(x->pathname.device, y->pathname.device) &&
		       ecl_equal(x->pathname.directory, y->pathname.directory) &&
		       ecl_equal(x->pathname.name, y->pathname.name) &&
		       ecl_equal(x->pathname.type, y->pathname.type) &&
		       ecl_equal(x->pathname.version, y->pathname.version);
	case t_foreign:
		return (tx == ty) && (x->foreign.data == y->foreign.data);
	default:
		return FALSE;
	}
}

cl_object
cl_equal(cl_object x, cl_object y)
{
	@(return (ecl_equal(x, y) ? ECL_T : ECL_NIL))
}

bool
ecl_equalp(cl_object x, cl_object y)
{
	cl_type tx, ty;
	cl_index j;
BEGIN:
        if (x == y)
                return TRUE;
	tx = ecl_t_of(x);
	ty = ecl_t_of(y);

	switch (tx) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
	case t_singlefloat:
	case t_doublefloat:
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
#endif
	case t_complex:
		return ECL_NUMBER_TYPE_P(ty) && ecl_number_equalp(x, y);
	case t_vector:
	case t_base_string:
	case t_bitvector:
#ifdef ECL_UNICODE
	case t_string:
		if (ty != t_vector && ty != t_base_string && ty != t_bitvector
		    && ty != t_string)
			return FALSE;
#else
		if (ty != t_vector && ty != t_base_string && ty != t_bitvector)
			return FALSE;
#endif
		j = x->vector.fillp;
		if (j != y->vector.fillp)
			return FALSE;
		goto ARRAY;
	case t_array:
		if (ty != t_array || x->array.rank != y->array.rank)
			return FALSE;
		if (x->array.rank > 1) {
			cl_index i = 0;
			for (i = 0; i < x->array.rank; i++)
				if (x->array.dims[i] != y->array.dims[i])
                                        return(FALSE);
		}
		if (x->array.dim != y->array.dim)
			return(FALSE);
		j=x->array.dim;
	ARRAY: {
		cl_index i;
		for (i = 0;  i < j;  i++)
			if (!ecl_equalp(ecl_aref_unsafe(x, i), ecl_aref_unsafe(y, i)))
				return(FALSE);
		return(TRUE);
                }
	case t_character:
		return (ty == tx) && ecl_char_equal(x, y);
	case t_list:
		if ((tx != ty) || Null(x) || Null(y)) {
			/* X is NIL but it is not EQ to Y */
			return FALSE;
		}
		if (!ecl_equalp(CAR(x), CAR(y)))
			return(FALSE);
		x = CDR(x);
		y = CDR(y);
		goto BEGIN;
#ifdef CLOS
	case t_instance: {
		cl_index i;
		if ((ty != tx) || (ECL_CLASS_OF(x) != ECL_CLASS_OF(y)))
			return(FALSE);
		for (i = 0;  i < x->instance.length;  i++)
			if (!ecl_equalp(x->instance.slots[i], y->instance.slots[i]))
				return(FALSE);
		return(TRUE);
	}
#else
	case t_structure: {
		cl_index i;
		if ((tx != ty) || (x->str.name != y->str.name))
			return(FALSE);
		for (i = 0;  i < x->str.length;  i++)
			if (!ecl_equalp(x->str.self[i], y->str.self[i]))
				return(FALSE);
		return(TRUE);
	}
#endif /* CLOS */
	case t_pathname:
		return (tx == ty) && ecl_equal(x, y);
	case t_hashtable: {
		if (tx != ty ||
                    x->hash.entries != y->hash.entries ||
		    x->hash.test != y->hash.test)
			return(FALSE);
		{
			cl_env_ptr env = ecl_process_env();
			cl_object iterator = si_hash_table_iterator(x);
			do {
				cl_object ndx = _ecl_funcall1(iterator);
				if (Null(ndx)) {
					return TRUE;
				} else {
					cl_object key = env->values[1];
					if (ecl_gethash_safe(key, y, OBJNULL) == OBJNULL)
						return FALSE;
				}
			} while (1);
		}
	}
	case t_random:
		return (tx == ty) && ecl_equalp(x->random.value, y->random.value);
	default:
		return ecl_eql(x,y);
	}
}

cl_object
cl_equalp(cl_object x, cl_object y)
{
	@(return (ecl_equalp(x, y) ? ECL_T : ECL_NIL))
}

cl_object
si_fixnump(cl_object x)
{
	@(return (ECL_FIXNUMP(x) ? ECL_T : ECL_NIL))
}
