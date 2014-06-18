/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    expt.d  -- Exponentiate.
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
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/impl/math_fenv.h>
#include <ecl/impl/math_dispatch.h>

#pragma STDC FENV_ACCESS ON

cl_fixnum
ecl_fixnum_expt(cl_fixnum x, cl_fixnum y)
{
	cl_fixnum z = 1;
	while (y > 0)
		if (y%2 == 0) {
			x *= x;
			y /= 2;
		} else {
			z *= x;
			--y;
		}
	return(z);
}

cl_object
cl_expt(cl_object x, cl_object y)
{
        @(return ecl_expt(x, y));
}

ecl_def_ct_single_float(singlefloat_one,1,static,const);
ecl_def_ct_double_float(doublefloat_one,1,static,const);
#ifdef ECL_LONG_FLOAT
ecl_def_ct_long_float(longfloat_one,1,static,const);
#endif

static cl_object
expt_zero(cl_object x, cl_object y)
{
	cl_type ty, tx;
	cl_object z;
        ty = ecl_t_of(y);
        tx = ecl_t_of(x);
        if (ecl_unlikely(!ECL_NUMBER_TYPE_P(tx))) {
                FEwrong_type_nth_arg(@[expt], 1, x, @[number]);
	}
        /* INV: The most specific numeric types come first. */
        switch ((ty > tx)? ty : tx) {
        case t_fixnum:
        case t_bignum:
        case t_ratio:
                return ecl_make_fixnum(1);
        case t_singlefloat:
                return singlefloat_one;
        case t_doublefloat:
                return doublefloat_one;
#ifdef ECL_LONG_FLOAT
        case t_longfloat:
                return longfloat_one;
#endif
        case t_complex:
                z = expt_zero((tx == t_complex)? x->complex.real : x,
                              (ty == t_complex)? y->complex.real : y);
                return ecl_make_complex(z, ecl_make_fixnum(0));
        default:
                /* We will never reach this */
                (void)0;
        }
}

cl_object
ecl_expt(cl_object x, cl_object y)
{
	cl_type ty, tx;
	cl_object z;
	if (ecl_unlikely(ecl_zerop(y))) {
                return expt_zero(x, y);
	}
        ty = ecl_t_of(y);
        tx = ecl_t_of(x);
        if (ecl_unlikely(!ECL_NUMBER_TYPE_P(tx))) {
                FEwrong_type_nth_arg(@[expt], 1, x, @[number]);
	}
        if (ecl_zerop(x)) {
		z = ecl_times(x, y);
		if (!ecl_plusp(ty==t_complex?y->complex.real:y))
			z = ecl_divide(ecl_make_fixnum(1), z);
	} else if (ty != t_fixnum && ty != t_bignum) {
                /* The following could be just
                   z = ecl_log1(x);
                   however, Maxima expects EXPT to have double accuracy
                   when the first argument is integer and the second
                   is double-float */
		z = ecl_log1(ecl_times(x, expt_zero(x, y)));
		z = ecl_times(z, y);
		z = ecl_exp(z);
	} else if (ecl_minusp(y)) {
		z = ecl_negate(y);
		z = ecl_expt(x, z);
		z = ecl_divide(ecl_make_fixnum(1), z);
	} else {
                ECL_MATHERR_CLEAR;
		z = ecl_make_fixnum(1);
		do {
			/* INV: ecl_integer_divide outputs an integer */
			if (!ecl_evenp(y))
				z = ecl_times(z, x);
			y = ecl_integer_divide(y, ecl_make_fixnum(2));
			if (ecl_zerop(y)) break;
			x = ecl_times(x, x);
		} while (1);
                ECL_MATHERR_TEST;
	}
	return z;
}
