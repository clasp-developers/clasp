/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    tan.d  -- Trascendental functions: tangent
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

/*
 * As of 2006-10-13 I found this bug in GLIBC's tanf, which overflows
 * when the argument is pi/4. It is 2008 and this has not yet been
 * solved. Not only that, but if we use tan() on float, GCC automatically
 * and stupidly forces the use of tanf(). 
 * As of 2013-03-31, this problem persists also on other platforms, such
 * as ARM, or PowerPC64. We thus extend the conditional to all GLIB copies.
 */
#if /*defined(__amd64__) && */ defined(__GLIBC__)
static double safe_tanf(double x) { return tan(x); }
#else
# define safe_tanf(x) tanf(x)
#endif

cl_object
cl_tan(cl_object x)
{
        @(return ecl_tan(x));
}

static cl_object
ecl_tan_rational(cl_object x)
{
        return ecl_make_single_float(safe_tanf(ecl_to_float(x)));
}

static cl_object
ecl_tan_single_float(cl_object x)
{
        return ecl_make_single_float(safe_tanf(ecl_single_float(x)));
}

static cl_object
ecl_tan_double_float(cl_object x)
{
        return ecl_make_double_float(tan(ecl_double_float(x)));
}

#ifdef ECL_LONG_FLOAT
static cl_object
ecl_tan_long_float(cl_object x)
{
        return ecl_make_long_float(tanl(ecl_long_float(x)));
}
#endif

static cl_object
ecl_tan_complex(cl_object x)
{
        cl_object a = ecl_sin(x);
        cl_object b = ecl_cos(x);
        return ecl_divide(a, b);
}

MATH_DEF_DISPATCH1(tan, @[tan], @[number],
                   ecl_tan_rational, ecl_tan_rational, ecl_tan_rational,
                   ecl_tan_single_float, ecl_tan_double_float, ecl_tan_long_float,
                   ecl_tan_complex);
