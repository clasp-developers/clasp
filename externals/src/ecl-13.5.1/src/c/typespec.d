/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    typespec.c -- Type specifier routines.
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

#include <ecl/ecl.h>

void
FEtype_error_fixnum(cl_object x) {
	FEwrong_type_argument(@[fixnum], x);
}

void
FEtype_error_size(cl_object x) {
	FEwrong_type_argument(cl_list(3, @'integer', ecl_make_fixnum(0),
				      ecl_make_fixnum(MOST_POSITIVE_FIXNUM)),
			      x);
}

void
FEtype_error_cons(cl_object x) {
	FEwrong_type_argument(@[cons], x);
}

void
FEtype_error_list(cl_object x) {
	FEwrong_type_argument(@[list], x);
}

void
FEtype_error_proper_list(cl_object x) {
	cl_error(9, @'simple-type-error', @':format-control',
		    make_constant_base_string("Not a proper list ~D"),
		    @':format-arguments', cl_list(1, x),
		    @':expected-type', ecl_read_from_cstring("si::proper-list"),
		    @':datum', x);
}

void
FEcircular_list(cl_object x)
{
	/* FIXME: Is this the right way to rebind it? */
	ecl_bds_bind(ecl_process_env(), @'*print-circle*', ECL_T);
	cl_error(9, @'simple-type-error', @':format-control',
		    make_constant_base_string("Circular list ~D"),
		    @':format-arguments', cl_list(1, x),
		    @':expected-type', @'list',
		    @':datum', x);
}

void
FEtype_error_index(cl_object seq, cl_fixnum ndx)
{
        cl_object n = ecl_make_fixnum(ndx);
	cl_index l = ECL_INSTANCEP(seq)? seq->instance.length : ecl_length(seq);
	cl_error(9, @'simple-type-error', @':format-control',
		    make_constant_base_string("~S is not a valid index into the object ~S"),
		    @':format-arguments', cl_list(2, n, seq),
		    @':expected-type', cl_list(3, @'integer', ecl_make_fixnum(0), ecl_make_fixnum(l-1)),
		    @':datum', n);
}

void
FEtype_error_array(cl_object v)
{
	FEwrong_type_argument(@[array], v);
}

void
FEtype_error_vector(cl_object v)
{
	FEwrong_type_argument(@[vector], v);
}

void
FEtype_error_sequence(cl_object x) {
	FEwrong_type_argument(@[sequence], x);
}

cl_object
ecl_type_error(cl_object function, const char *place, cl_object o,
	       cl_object type)
{
	si_wrong_type_argument(4, o, type,
			       (*place? make_constant_base_string(place) : ECL_NIL),
			       function);
}

/**********************************************************************/

static cl_object
ecl_type_to_symbol(cl_type t)
{
	switch(t) {
	case t_character:
		return @'character';
	case t_fixnum:
		return @'fixnum';
	case t_bignum:
		return @'bignum';
	case t_ratio:
		return @'ratio';
	case t_singlefloat:
		return @'single-float';
	case t_doublefloat:
		return @'double-float';
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		return @'long-float';
#endif
	case t_complex:
		return @'complex';
	case t_symbol:
		return @'symbol';
	case t_package:
		return @'package';
	case t_list:
		return @'list';
	case t_hashtable:
		return @'hash-table';
	case t_array:
		return @'array';
	case t_vector:
		return @'vector';
	case t_bitvector:
		return @'bit-vector';
#ifdef ECL_UNICODE
	case t_string:
		return @'string';
#endif
	case t_base_string:
		return @'base-string';
	case t_stream:
		return @'stream';
	case t_readtable:
		return @'readtable';
	case t_pathname:
		return @'pathname';
	case t_random:
		return @'random-state';
	case t_bytecodes:
	case t_bclosure:
	case t_cfun:
	case t_cfunfixed:
	case t_cclosure:
		return @'compiled-function';
#ifdef ECL_THREADS
	case t_process:
		return @'mp::process';
	case t_lock:
		return @'mp::lock';
	case t_condition_variable:
		return @'mp::condition-variable';
	case t_semaphore:
		return @'mp::semaphore';
	case t_barrier:
		return @'mp::barrier';
	case t_mailbox:
		return @'mp::mailbox';
#endif
	case t_codeblock:
		return @'si::code-block';
	case t_foreign:
		return @'si::foreign-data';
	case t_frame:
		return @'si::frame';
	case t_weak_pointer:
		return @'ext::weak-pointer';
#ifdef ECL_SSE2
	case t_sse_pack:
		return @'ext::sse-pack';
#endif
	default:
		ecl_internal_error("not a lisp data object");
	}
}

cl_object
ecl_check_cl_type(cl_object fun, cl_object p, cl_type t)
{
	while (ecl_t_of(p) != t) {
		p = ecl_type_error(fun, "argument", p, ecl_type_to_symbol(t));
	}
	return p;
}

void
assert_type_integer(cl_object p)
{
	cl_type t = ecl_t_of(p);
	if (t != t_fixnum && t != t_bignum)
                FEwrong_type_nth_arg(@[coerce], 1, p, @[integer]);
}

void
assert_type_non_negative_integer(cl_object p)
{
	cl_type t = ecl_t_of(p);

	if (t == t_fixnum) {
		if (ecl_fixnum_plusp(p))
			return;
	} else if (t == t_bignum) {
		if (_ecl_big_sign(p) >= 0)
			return;
	}
	FEwrong_type_argument(cl_list(3,@'integer',ecl_make_fixnum(0),@'*'), p);
}

void
assert_type_proper_list(cl_object p)
{
	if (ECL_ATOM(p) && p != ECL_NIL)
		FEtype_error_list(p);
	if (cl_list_length(p) == ECL_NIL)
		FEcircular_list(p);
}

cl_object
cl_type_of(cl_object x)
{
	cl_object t;
	cl_type tx = ecl_t_of(x);
	switch (tx) {
#ifdef CLOS
        case t_instance: {
		cl_object cl = ECL_CLASS_OF(x);
		t = ECL_CLASS_NAME(cl);
		if (t == ECL_NIL || cl != cl_find_class(2, t, ECL_NIL))
			t = cl;
		break;
	}
#endif
#if 1
	case t_fixnum:
	case t_bignum:
		t = cl_list(3, @'integer', x, x); break;
#endif
	case t_character: {
		int i = ECL_CHAR_CODE(x);
		if (ecl_standard_char_p(i)) {
			t = @'standard-char';
		} else if (ecl_base_char_p(i)) {
			t = @'base-char';
		} else {
			t = @'character';
		}
		break;
	}

	case t_symbol:
		if (x == ECL_T)
			t = @'boolean';
		else if (x->symbol.hpack == cl_core.keyword_package)
			t = @'keyword';
		else
			t = @'symbol';
		break;
	case t_array:
		if (ECL_ADJUSTABLE_ARRAY_P(x) ||
		    !Null(CAR(x->array.displaced)))
			t = @'array';
		else
			t = @'simple-array';
		t = cl_list(3, t, ecl_elttype_to_symbol(ecl_array_elttype(x)),
                            cl_array_dimensions(x));
		break;
	case t_vector:
		if (ECL_ADJUSTABLE_ARRAY_P(x) ||
		    !Null(CAR(x->vector.displaced))) {
			t = cl_list(3, @'vector', ecl_elttype_to_symbol(ecl_array_elttype(x)),
				    ecl_make_fixnum(x->vector.dim));
		} else if (ECL_ARRAY_HAS_FILL_POINTER_P(x) ||
			   (cl_elttype)x->vector.elttype != ecl_aet_object) {
			t = cl_list(3, @'simple-array',
                                    ecl_elttype_to_symbol(ecl_array_elttype(x)),
				    cl_array_dimensions(x));
		} else {
			t = cl_list(2, @'simple-vector', ecl_make_fixnum(x->vector.dim));
		}
		break;
#ifdef ECL_UNICODE
	case t_string:
		if (ECL_ADJUSTABLE_ARRAY_P(x) ||
		    ECL_ARRAY_HAS_FILL_POINTER_P(x) ||
		    !Null(CAR(x->string.displaced)))
			t = @'array';
		else
			t = @'simple-array';
		t = cl_list(3, t, @'character', cl_list(1, ecl_make_fixnum(x->string.dim)));
		break;
#endif
	case t_base_string:
		if (ECL_ADJUSTABLE_ARRAY_P(x) ||
		    ECL_ARRAY_HAS_FILL_POINTER_P(x) ||
		    !Null(CAR(x->base_string.displaced)))
			t = @'array';
		else
			t = @'simple-array';
		t = cl_list(3, t, @'base-char', cl_list(1, ecl_make_fixnum(x->base_string.dim)));
		break;
	case t_bitvector:
		if (ECL_ADJUSTABLE_ARRAY_P(x) ||
		    ECL_ARRAY_HAS_FILL_POINTER_P(x) ||
		    !Null(CAR(x->vector.displaced)))
			t = @'array';
		else
			t = @'simple-array';
		t = cl_list(3, t, @'bit', cl_list(1, ecl_make_fixnum(x->vector.dim)));
		break;
#ifndef CLOS
	case t_structure:
		t = x->str.name; break;
#endif
	case t_stream:
		switch (x->stream.mode) {
		case ecl_smm_synonym:	t = @'synonym-stream'; break;
		case ecl_smm_broadcast:	t = @'broadcast-stream'; break;
		case ecl_smm_concatenated:	t = @'concatenated-stream'; break;
		case ecl_smm_two_way:	t =  @'two-way-stream'; break;
		case ecl_smm_string_input:
		case ecl_smm_string_output:	t = @'string-stream'; break;
		case ecl_smm_echo:		t = @'echo-stream'; break;
                case ecl_smm_sequence_input:
                case ecl_smm_sequence_output: t = @'ext::sequence-stream'; break;
		default:		t = @'file-stream'; break;
		}
		break;
	case t_pathname:
		t = x->pathname.logical? @'logical-pathname' : @'pathname';
		break;
	case t_list:
		t = Null(x) ? @'null' : @'cons';
		break;
#ifdef ECL_SSE2
	case t_sse_pack:
		t = @'ext::sse-pack';
		break;
#endif
	default:
		t = ecl_type_to_symbol(tx);
	}
	@(return t)
}

cl_object
ecl_make_integer_type(cl_object min, cl_object max)
{
        return cl_list(3, @'integer', min, max);
}
