/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    Copyright (c) 2010, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#define ECL_INCLUDE_MATH_H
#include <float.h>
#include <ecl/ecl.h>
#include <ecl/internal.h>

static cl_object
bignum_to_string(cl_object buffer, cl_object x, cl_object base)
{
        cl_index str_size;
        int b;
        if (!ECL_FIXNUMP(base) || ((b = ecl_fixnum(base)) < 2) || (b > 36)) {
                FEwrong_type_nth_arg(@[si::integer-to-string], 3, base,
                                     cl_list(3, @'integer', ecl_make_fixnum(2),
                                             ecl_make_fixnum(36)));
        }
        str_size = mpz_sizeinbase(x->big.big_num, b);
        buffer = _ecl_ensure_buffer(buffer, str_size+1);
        if (str_size <= 62) {
		/* With the leading sign and the trailing null character,
		 * only 62 digits fit in this buffer. */
                char txt[64];
                mpz_get_str(txt, -b, x->big.big_num);
                _ecl_string_push_c_string(buffer, txt);
        } else {
                char *txt = ecl_alloc_atomic(str_size + 2);
                mpz_get_str(txt, -b, x->big.big_num);
                _ecl_string_push_c_string(buffer, txt);
                ecl_dealloc(txt);
        }
        return buffer;
}

static void
write_base_prefix(cl_object buffer, int base)
{
	if (base == 2) {
                _ecl_string_push_c_string(buffer, "#b");
        } else if (base == 8) {
                _ecl_string_push_c_string(buffer, "#o");
        } else if (base == 16) {
                _ecl_string_push_c_string(buffer, "#x");
        } else if (base >= 10) {
                char prefix[5] = "#00r";
                prefix[1] = base/10 + '0';
                prefix[2] = base%10 + '0';
                _ecl_string_push_c_string(buffer, prefix);
	} else {
                char prefix[4] = "#0r";
                prefix[1] = base + '0';
                _ecl_string_push_c_string(buffer, prefix);
	}
}

cl_object
si_integer_to_string(cl_object buffer, cl_object integer,
                     cl_object base, cl_object radix, cl_object decimalp)
{
        if (!Null(radix)) {
                if (Null(decimalp) || base != ecl_make_fixnum(10)) {
                        buffer = _ecl_ensure_buffer(buffer, 10);
                        write_base_prefix(buffer, ecl_fixnum(base));
                }
                buffer = si_integer_to_string(buffer, integer, base, ECL_NIL, ECL_NIL);
                if (!Null(decimalp) && base == ecl_make_fixnum(10)) {
                        _ecl_string_push_c_string(buffer, ".");
                }
                @(return buffer)
        }
        switch (ecl_t_of(integer)) {
        case t_fixnum: {
                cl_object big = _ecl_big_register0();
                _ecl_big_set_fixnum(big, ecl_fixnum(integer));
                buffer = bignum_to_string(buffer, big, base);
                _ecl_big_register_free(big);
                return buffer;
        }
        case t_bignum:
                return bignum_to_string(buffer, integer, base);
        default:
                FEwrong_type_nth_arg(@[si::integer-to-string], 2,
                                     @'integer', integer);
        }
}
