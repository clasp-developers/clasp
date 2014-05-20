/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    sinh.d  -- Trascendental functions: hyperbolic sine
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
cl_sinh(cl_object x)
{
        @(return ecl_sinh(x));
}

static cl_object
ecl_sinh_rational(cl_object x)
{
        return ecl_make_single_float(sinhf(ecl_to_float(x)));
}

static cl_object
ecl_sinh_single_float(cl_object x)
{
        return ecl_make_single_float(sinhf(ecl_single_float(x)));
}

static cl_object
ecl_sinh_double_float(cl_object x)
{
        return ecl_make_double_float(sinh(ecl_double_float(x)));
}

#ifdef ECL_LONG_FLOAT
static cl_object
ecl_sinh_long_float(cl_object x)
{
        return ecl_make_long_float(sinhl(ecl_long_float(x)));
}
#endif

static cl_object
ecl_sinh_complex(cl_object x)
{
        /*
          z = x + I y
          sinh(z) = (exp(z)-exp(-z))/2
          = (exp(x)*(cos(y)+Isin(y))-exp(-x)*(cos(y)-Isin(y)))/2
          = sinh(x)*cos(y) + Icosh(x)*sin(y);
        */
        cl_object dx = x->complex.real;
        cl_object dy = x->complex.imag;
        cl_object a = ecl_times(ecl_sinh(dx), ecl_cos(dy));
        cl_object b = ecl_times(ecl_cosh(dx), ecl_sin(dy));
        return ecl_make_complex(a, b);
}

MATH_DEF_DISPATCH1(sinh, @[sinh], @[number],
                   ecl_sinh_rational, ecl_sinh_rational, ecl_sinh_rational,
                   ecl_sinh_single_float, ecl_sinh_double_float, ecl_sinh_long_float,
                   ecl_sinh_complex);
