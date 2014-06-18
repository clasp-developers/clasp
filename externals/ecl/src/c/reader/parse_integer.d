/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <ecl/ecl.h>
#include <ecl/number.h>
#include <ecl/internal.h>

#define basep(d) (d <= 36)

cl_object
ecl_parse_integer(cl_object str, cl_index start, cl_index end,
		  cl_index *ep, unsigned int radix)
{
	int sign, d;
	cl_object integer_part, output;
	cl_index i, c;

	if (start >= end || !basep(radix)) {
		*ep = start;
		return OBJNULL;
	}
	sign = 1;
	c = ecl_char(str, start);
	if (c == '+') {
		start++;
	} else if (c == '-') {
		sign = -1;
		start++;
	}
	integer_part = _ecl_big_register0();
        _ecl_big_set_ui(integer_part, 0);
	for (i = start; i < end; i++) {
		c = ecl_char(str, i);
		d = ecl_digitp(c, radix);
		if (d < 0) {
			break;
		}
		_ecl_big_mul_ui(integer_part, integer_part, radix);
		_ecl_big_add_ui(integer_part, integer_part, d);
	}
	if (sign < 0) {
		_ecl_big_complement(integer_part, integer_part);
	}
	output = _ecl_big_register_normalize(integer_part);
	*ep = i;
	return (i == start)? OBJNULL : output;
}

@(defun parse_integer (strng
		       &key (start ecl_make_fixnum(0))
			    end
			    (radix ecl_make_fixnum(10))
			    junk_allowed
		       &aux x)
	cl_index s, e, ep;
	cl_object rtbl = ecl_current_readtable();
@ {
        unlikely_if (!ECL_STRINGP(strng)) {
                FEwrong_type_nth_arg(@[parse-integer], 1, strng, @[string]);
        }
	unlikely_if (!ECL_FIXNUMP(radix) ||
                     ecl_fixnum_lower(radix, ecl_make_fixnum(2)) ||
                     ecl_fixnum_greater(radix, ecl_make_fixnum(36)))
        {
		FEerror("~S is an illegal radix.", 1, radix);
        }
        {
                cl_index_pair p =
                        ecl_vector_start_end(@[parse-integer], strng, start, end);
                s = p.start;
                e = p.end;
        }
	while (s < e &&
	       ecl_readtable_get(rtbl, ecl_char(strng, s), NULL) == cat_whitespace) {
		s++;
	}
	if (s >= e) {
		if (junk_allowed != ECL_NIL)
			@(return ECL_NIL ecl_make_fixnum(s))
		else
			goto CANNOT_PARSE;
	}
	x = ecl_parse_integer(strng, s, e, &ep, ecl_fixnum(radix));
	if (x == OBJNULL) {
		if (junk_allowed != ECL_NIL) {
			@(return ECL_NIL ecl_make_fixnum(ep));
		} else {
			goto CANNOT_PARSE;
		}
	}
	if (junk_allowed != ECL_NIL) {
		@(return x ecl_make_fixnum(ep));
	}
	for (s = ep; s < e; s++) {
		unlikely_if (ecl_readtable_get(rtbl, ecl_char(strng, s), NULL)
                             != cat_whitespace) 
                {
CANNOT_PARSE:		FEparse_error("Cannot parse an integer in the string ~S.",
				      ECL_NIL, 1, strng);
		}
	}
	@(return x ecl_make_fixnum(e));
} @)
