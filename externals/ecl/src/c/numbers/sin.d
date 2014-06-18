/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    sin.d  -- Trascendental functions: sine
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
cl_sin(cl_object x)
{
        @(return ecl_sin(x));
}

static cl_object
ecl_sin_rational(cl_object x)
{
        return ecl_make_single_float(sinf(ecl_to_float(x)));
}

static cl_object
ecl_sin_single_float(cl_object x)
{
        return ecl_make_single_float(sinf(ecl_single_float(x)));
}

static cl_object
ecl_sin_double_float(cl_object x)
{
        return ecl_make_double_float(sin(ecl_double_float(x)));
}

#ifdef ECL_LONG_FLOAT
static cl_object
ecl_sin_long_float(cl_object x)
{
        return ecl_make_long_float(sinl(ecl_long_float(x)));
}
#endif

static cl_object
ecl_sin_complex(cl_object x)
{
        /*
          z = x + I y
          z = x + I y
          sin(z) = sinh(I z) = sinh(-y + I x)
        */
        cl_object dx = x->complex.real;
        cl_object dy = x->complex.imag;
        cl_object a = ecl_times(ecl_sin(dx), ecl_cosh(dy));
        cl_object b = ecl_times(ecl_cos(dx), ecl_sinh(dy));
	return ecl_make_complex(a, b);
}

MATH_DEF_DISPATCH1(sin, @[sin], @[number],
                   ecl_sin_rational, ecl_sin_rational, ecl_sin_rational,
                   ecl_sin_single_float, ecl_sin_double_float, ecl_sin_long_float,
                   ecl_sin_complex);
