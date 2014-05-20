/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    conjugate.d  -- Trascendental functions: conjugateine
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
#include <ecl/impl/math_dispatch.h>

cl_object
cl_conjugate(cl_object x)
{
        @(return ecl_conjugate(x));
}

static cl_object
ecl_conjugate_real(cl_object x)
{
        return x;
}

static cl_object
ecl_conjugate_complex(cl_object x)
{
        return ecl_make_complex(x->complex.real, ecl_negate(x->complex.imag));
}

MATH_DEF_DISPATCH1_NE(conjugate, @[conjugate], @[number],
                      ecl_conjugate_real, ecl_conjugate_real, ecl_conjugate_real,
                      ecl_conjugate_real, ecl_conjugate_real,
                      ecl_conjugate_real,
                      ecl_conjugate_complex);
