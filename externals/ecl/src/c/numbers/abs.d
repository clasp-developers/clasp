/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    abs.d  -- Absolute value.
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

#include <stdlib.h>
#define ECL_INCLUDE_MATH_H
#include <ecl/ecl.h>
#include <ecl/internal.h>
#include <ecl/impl/math_dispatch.h>

cl_object
cl_abs(cl_object x)
{
        @(return ecl_abs(x));
}

static cl_object
ecl_abs_fixnum(cl_object x)
{
        return ecl_fixnum_minusp(x)? ecl_make_integer(-ecl_fixnum(x)) : x;
}

static cl_object
ecl_abs_bignum(cl_object x)
{
        return (_ecl_big_sign(x) < 0)? _ecl_big_negate(x) : x;
}

static cl_object
ecl_abs_rational(cl_object x)
{
        return (ecl_minusp(x->ratio.num))?
                ecl_make_ratio(ecl_negate(x->ratio.num), x->ratio.den) : x;
}

static cl_object
ecl_abs_single_float(cl_object x)
{
	float f = ecl_single_float(x);
        return (f < 0)? ecl_make_single_float(-f) : x;
}

static cl_object
ecl_abs_double_float(cl_object x)
{
	double f = ecl_double_float(x);
        return (f < 0)? ecl_make_double_float(-f) : x;
}

#ifdef ECL_LONG_FLOAT
static cl_object
ecl_abs_long_float(cl_object x)
{
	long double f = ecl_long_float(x);
        return (f < 0)? ecl_make_long_float(-f) : x;
}
#endif

static cl_object
ecl_abs_complex(cl_object x)
{
        /* Compute sqrt(r*r + i*i) carefully to prevent overflow.
         * Assume |i| >= |r|. Then sqrt(i*i + r*r) = |i|*sqrt(1 +(r/i)^2).
         */
        cl_object r = ecl_abs(x->complex.real);
        cl_object i = ecl_abs(x->complex.imag);
        int comparison;
        comparison = ecl_number_compare(r, i);
        if (comparison == 0) {
                r = ecl_times(r, r);
                return ecl_sqrt(ecl_plus(r, r));
        } else {
                if (comparison > 0) {
                        cl_object aux = i;
                        i = r; r = aux;
                }
                r = ecl_divide(r, i);
                r = ecl_plus(ecl_make_fixnum(1), ecl_times(r, r));
                return ecl_times(cl_sqrt(r), i);
        }
}

MATH_DEF_DISPATCH1_NE(abs, @[abs], @[number],
                      ecl_abs_fixnum, ecl_abs_bignum, ecl_abs_rational,
                      ecl_abs_single_float, ecl_abs_double_float, ecl_abs_long_float,
                      ecl_abs_complex);
