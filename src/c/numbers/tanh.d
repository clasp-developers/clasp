/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    tanh.d  -- Trascendental functions: hyperbolic tangent
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

#define ECL_INCLUDE_MATH_H
#include <ecl/ecl.h>
#include <ecl/internal.h>
#include <ecl/impl/math_dispatch.h>

#pragma STDC FENV_ACCESS ON

cl_object
cl_tanh(cl_object x)
{
        @(return ecl_tanh(x));
}

static cl_object
ecl_tanh_rational(cl_object x)
{
        return ecl_make_single_float(tanhf(ecl_to_float(x)));
}

static cl_object
ecl_tanh_single_float(cl_object x)
{
        return ecl_make_single_float(tanhf(ecl_single_float(x)));
}

static cl_object
ecl_tanh_double_float(cl_object x)
{
        return ecl_make_double_float(tanh(ecl_double_float(x)));
}

#ifdef ECL_LONG_FLOAT
static cl_object
ecl_tanh_long_float(cl_object x)
{
        return ecl_make_long_float(tanhl(ecl_long_float(x)));
}
#endif

static cl_object
ecl_tanh_complex(cl_object x)
{
        cl_object a = ecl_sinh(x);
	cl_object b = ecl_cosh(x);
	return ecl_divide(a, b);
}

MATH_DEF_DISPATCH1(tanh, @[tanh], @[number],
                   ecl_tanh_rational, ecl_tanh_rational, ecl_tanh_rational,
                   ecl_tanh_single_float, ecl_tanh_double_float, ecl_tanh_long_float,
                   ecl_tanh_complex);
