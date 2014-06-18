/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    atan1.d  -- Trascendental functions: arc tangent
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
#include <ecl/impl/math_fenv.h>

#pragma STDC FENV_ACCESS ON

static double
ecl_atan2_double(double y, double x)
{
        if (signbit(x)) {
                if (signbit(y)) {
			return -ECL_PI_D + atan(-y / -x);
                } else if (y == 0) {
			return ECL_PI_D;
                } else {
			return ECL_PI_D - atan(y / -x);
		}
	} else if (x == 0) {
                if (signbit(y)) {
			return -ECL_PI2_D;
                } else if (y == 0) {
                        return x / y;  /* Produces a NaN */
		} else {
			return ECL_PI2_D;
		}
	} else {
                if (signbit(y)) {
                        return -atan(-y / x);
                } else if (y == 0) {
                        return (double)0;
                } else {
                        return atan(y / x);
		}
	}
}

#ifdef ECL_LONG_FLOAT
static long double
ecl_atan2_long_double(long double y, long double x)
{
        if (signbit(x)) {
                if (signbit(y)) {
			return -ECL_PI_L + atanl(-y / -x);
                } else if (y == 0) {
			return ECL_PI_L;
                } else {
			return ECL_PI_L - atanl(y / -x);
		}
	} else if (x == 0) {
                if (signbit(y)) {
			return -ECL_PI2_L;
                } else if (y == 0) {
                        return x / y;  /* Produces a NaN */
		} else {
			return ECL_PI2_L;
		}
	} else {
                if (signbit(y)) {
                        return -atanl(-y / x);
                } else if (y == 0) {
                        return (long double)0;
                } else {
                        return atanl(y / x);
		}
	}
}
#endif

cl_object
ecl_atan2(cl_object y, cl_object x)
{
        cl_object output;
        ECL_MATHERR_CLEAR;
        {
#ifdef ECL_LONG_FLOAT
	int tx = ecl_t_of(x);
	int ty = ecl_t_of(y);
	if (tx < ty)
		tx = ty;
	if (tx == t_longfloat) {
                long double d = ecl_atan2_long_double(ecl_to_long_double(y),
                                                      ecl_to_long_double(x));
		output = ecl_make_long_float(d);
	} else {
		double dx = ecl_to_double(x);
		double dy = ecl_to_double(y);
		double dz = ecl_atan2_double(dy, dx);
		if (tx == t_doublefloat) {
			output = ecl_make_double_float(dz);
		} else {
			output = ecl_make_single_float(dz);
		}
	}
#else
	double dy = ecl_to_double(y);
	double dx = ecl_to_double(x);
	double dz = ecl_atan2_double(dy, dx);
	if (ECL_DOUBLE_FLOAT_P(x) || ECL_DOUBLE_FLOAT_P(y)) {
		output = ecl_make_double_float(dz);
	} else {
		output = ecl_make_single_float(dz);
	}
#endif
        }
        ECL_MATHERR_TEST;
        return output;
}

cl_object
ecl_atan1(cl_object y)
{
	if (ECL_COMPLEXP(y)) {
#if 0 /* ANSI states it should be this first part */
		cl_object z = ecl_times(cl_core.imag_unit, y);
		z = ecl_plus(ecl_log1(ecl_one_plus(z)),
			     ecl_log1(ecl_minus(ecl_make_fixnum(1), z)));
		z = ecl_divide(z, ecl_times(ecl_make_fixnum(2),
					    cl_core.imag_unit));
#else
		cl_object z1, z = ecl_times(cl_core.imag_unit, y);
		z = ecl_one_plus(z);
		z1 = ecl_times(y, y);
		z1 = ecl_one_plus(z1);
		z1 = ecl_sqrt(z1);
		z = ecl_divide(z, z1);
		z = ecl_log1(z);
		z = ecl_times(cl_core.minus_imag_unit, z);
#endif /* ANSI */
		return z;
	} else {
		return ecl_atan2(y, ecl_make_fixnum(1));
	}
}

@(defun atan (x &optional (y OBJNULL))
@	/* INV: type check in ecl_atan() & ecl_atan2() */
	/* FIXME ecl_atan() and ecl_atan2() produce generic errors
	   without recovery and function information. */
	if (y == OBJNULL)
		@(return ecl_atan1(x))
	@(return ecl_atan2(x, y))
@)
