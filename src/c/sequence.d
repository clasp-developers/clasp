/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    sequence.d -- Sequence routines.
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
#include <limits.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>

cl_index_pair
ecl_sequence_start_end(cl_object fun, cl_object sequence,
		       cl_object start, cl_object end)
{
        cl_index_pair p;
	cl_index l;
	p.length = l = ecl_length(sequence);
	unlikely_if (!ECL_FIXNUMP(start) || ecl_fixnum_minusp(start)) {
                FEwrong_type_key_arg(fun, @[:start], start, @[unsigned-byte]);
        }
        p.start = ecl_fixnum(start);
	if (Null(end)) {
		p.end = l;
	} else {
                unlikely_if (!ECL_FIXNUMP(end) || ecl_fixnum_minusp(end)) {
                        FEwrong_type_key_arg(fun, @[:end], end,
                                             ecl_read_from_cstring("(OR NULL UNSIGNED-BYTE)"));
                }
		p.end = ecl_fixnum(end);
		unlikely_if (p.end > l) {
                        cl_object fillp = ecl_make_fixnum(l);
                        FEwrong_type_key_arg(fun, @[:end], end,
                                             ecl_make_integer_type(start, fillp));
                }
	}
        unlikely_if (p.end < p.start) {
                FEwrong_type_key_arg(fun, @[:start], start,
                                     ecl_make_integer_type(ecl_make_fixnum(0),
							   ecl_make_fixnum(p.end)));
        }
        return p;
}

cl_object
si_sequence_start_end(cl_object fun, cl_object sequence, cl_object start, cl_object end)
{
	cl_index_pair p = ecl_sequence_start_end(fun, sequence, start, end);
	@(return ecl_make_fixnum(p.start) ecl_make_fixnum(p.end)
          ecl_make_fixnum(p.length));
}

cl_object
cl_elt(cl_object x, cl_object i)
{
	@(return ecl_elt(x, ecl_to_size(i)))
}

cl_object
ecl_elt(cl_object seq, cl_fixnum index)
{
	cl_fixnum i;
	cl_object l;

	if (index < 0)
		goto E;
	switch (ecl_t_of(seq)) {
	case t_list:
		for (i = index, l = seq;  i > 0;  --i) {
                        if (!LISTP(l)) goto E0;
                        if (Null(l)) goto E;
                        l = ECL_CONS_CDR(l);
                }
                if (!LISTP(l)) goto E0;
                if (Null(l)) goto E;
		return ECL_CONS_CAR(l);

#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_vector:
	case t_bitvector:
	case t_base_string:
                if (index >= seq->vector.fillp) goto E;
		return ecl_aref_unsafe(seq, index);
	default:
        E0:
		FEtype_error_sequence(seq);
	}
E:
	FEtype_error_index(seq, index);
}

cl_object
si_elt_set(cl_object seq, cl_object index, cl_object val)
{
	@(return ecl_elt_set(seq, ecl_to_size(index), val))
}

cl_object
ecl_elt_set(cl_object seq, cl_fixnum index, cl_object val)
{
	cl_fixnum i;
	cl_object l;

	if (index < 0)
		goto E;
	switch (ecl_t_of(seq)) {
	case t_list:
		for (i = index, l = seq;  i > 0;  --i) {
                        if (!LISTP(l)) goto E0;
                        if (Null(l)) goto E;
                        l = ECL_CONS_CDR(l);
                }
                if (!LISTP(l)) goto E0;
                if (Null(l)) goto E;
		ECL_RPLACA(l, val);
		return val;

#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_vector:
	case t_bitvector:
	case t_base_string:
                if (index >= seq->vector.fillp) goto E;
		return ecl_aset_unsafe(seq, index, val);
	default:
        E0:
		FEtype_error_sequence(seq);
	}
E:
	FEtype_error_index(seq, index);
}

cl_object
ecl_subseq(cl_object sequence, cl_index start, cl_index limit)
{
	switch (ecl_t_of(sequence)) {
	case t_list:
		if (start)
			sequence = ecl_nthcdr(start, sequence);
		{
			cl_object x = ECL_NIL;
			cl_object *z = &x;
			while (!Null(sequence) && (limit--)) {
				if (ECL_ATOM(sequence))
					FEtype_error_cons(sequence);
				z = &ECL_CONS_CDR(*z = ecl_list1(ECL_CONS_CAR(sequence)));
				sequence = ECL_CONS_CDR(sequence);
			}
			return x;
		}
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_vector:
	case t_bitvector:
	case t_base_string: {
		cl_index size;
		cl_object x;
		if (start > sequence->vector.fillp) {
			x = ecl_alloc_simple_vector(0, ecl_array_elttype(sequence));
		} else {
			size = sequence->vector.fillp - start;
			if (size > limit)
				size = limit;
			x = ecl_alloc_simple_vector(size, ecl_array_elttype(sequence));
			ecl_copy_subarray(x, 0, sequence, start, size);
		}
		return x;
	}
	default:
		FEtype_error_sequence(sequence);
	}
}

cl_object
ecl_copy_seq(cl_object sequence)
{
	return ecl_subseq(sequence, 0, MOST_POSITIVE_FIXNUM);
}

@(defun subseq (sequence start &optional end &aux x)
	cl_index_pair p;
@
	p = ecl_sequence_start_end(@[subseq], sequence, start, end);
	sequence = ecl_subseq(sequence, p.start, p.end - p.start);
	@(return sequence);
@)

cl_object
cl_copy_seq(cl_object x)
{
	@(return ecl_subseq(x, 0, MOST_POSITIVE_FIXNUM));
}

cl_object
cl_length(cl_object x)
{
	@(return ecl_make_fixnum(ecl_length(x)))
}

cl_fixnum
ecl_length(cl_object x)
{
	cl_fixnum i;

	switch (ecl_t_of(x)) {
	case t_list:
		/* INV: A list's length always fits in a fixnum */
		i = 0;
		loop_for_in(x) {
			i++;
		} end_loop_for_in;
		return(i);

#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_vector:
	case t_base_string:
	case t_bitvector:
		return(x->vector.fillp);

	default:
		FEtype_error_sequence(x);
	}
}

cl_object
cl_reverse(cl_object seq)
{
	cl_object output, x;

	switch (ecl_t_of(seq)) {
	case t_list: {
		for (x = seq, output = ECL_NIL; !Null(x); x = ECL_CONS_CDR(x)) {
                        if (!LISTP(x)) goto E;
			output = CONS(ECL_CONS_CAR(x), output);
                }
		break;
	}
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_vector:
	case t_bitvector:
	case t_base_string:
		output = ecl_alloc_simple_vector(seq->vector.fillp, ecl_array_elttype(seq));
		ecl_copy_subarray(output, 0, seq, 0, seq->vector.fillp);
		ecl_reverse_subarray(output, 0, seq->vector.fillp);
		break;
	default:
        E:
		FEtype_error_sequence(seq);
	}
	@(return output)
}

cl_object
cl_nreverse(cl_object seq)
{
	switch (ecl_t_of(seq)) {
	case t_list: {
		cl_object x, y, z;
                for (x = seq, y = ECL_NIL; !Null(x); ) {
                        if (!LISTP(x)) FEtype_error_list(x);
                        z = x;
                        x = ECL_CONS_CDR(x);
                        if (x == seq) FEcircular_list(seq);
                        ECL_RPLACD(z, y);
                        y = z;
                }
		seq = y;
		break;
	}
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_vector:
	case t_base_string:
	case t_bitvector:
		ecl_reverse_subarray(seq, 0, seq->vector.fillp);
		break;
	default:
		FEtype_error_sequence(seq);
	}
	@(return seq)
}
