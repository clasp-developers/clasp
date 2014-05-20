/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    round.d  -- Implementation of CL:ROUND
*/
/*
    Copyright (c) 2010, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#define ECL_INCLUDE_MATH_H
#include <ecl/ecl.h>
#include <float.h>
#include <ecl/ecl.h>
#include <ecl/impl/math_dispatch2.h>
#ifndef HAVE_ISOC99
# define floorf floor
#endif
#include <ecl/internal.h>

@(defun round (x &optional (y OBJNULL))
@
	if (narg == 1)
		return ecl_round1(x);
	else
		return ecl_round2(x, y);
@)

static cl_object
number_remainder(cl_object x, cl_object y, cl_object q)
{
	cl_object z;

	z = ecl_times(q, y);
	z = ecl_minus(x, z);
	return(z);
}

static double
round_double(double d)
{
	if (d >= 0) {
		double q = floor(d += 0.5);
		if (q == d) {
			int i = (int)fmod(q, 10);
			if (i & 1) {
				return q-1;
			}
		}
		return q;
	} else {
		return -round_double(-d);
	}
}

#ifdef ECL_LONG_FLOAT
static long double
round_long_double(long double d)
{
	if (d >= 0) {
		long double q = floorl(d += 0.5);
		if (q == d) {
			int i = (int)fmodl(q, 10);
			if (i & 1) {
				return q-1;
			}
		}
		return q;
	} else {
		return -round_long_double(-d);
	}
}
#endif

static cl_object
ecl_round2_integer(const cl_env_ptr the_env, cl_object x, cl_object y, cl_object q)
{
	cl_object q1 = ecl_integer_divide(q->ratio.num, q->ratio.den);
	cl_object r = ecl_minus(q, q1);
	if (ecl_minusp(r)) {
		int c = ecl_number_compare(cl_core.minus_half, r);
		if (c > 0 || (c == 0 && ecl_oddp(q1))) {
			q1 = ecl_one_minus(q1);
		}
	} else {
		int c = ecl_number_compare(r, cl_core.plus_half);
		if (c > 0 || (c == 0 && ecl_oddp(q1))) {
			q1 = ecl_one_plus(q1);
		}
	}
	r = number_remainder(x, y, q1);
	ecl_return2(the_env, q1, r);
}

cl_object
ecl_round1(cl_object x)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_object v0, v1;
	switch (ecl_t_of(x)) {
	case t_fixnum:
	case t_bignum:
		v0 = x;
		v1 = ecl_make_fixnum(0);
		break;
	case t_ratio:
		v0 = ecl_round2_integer(the_env, x->ratio.num, x->ratio.den, x);
		v1 = ecl_make_ratio(ecl_nth_value(the_env, 1), x->ratio.den);
		break;
	case t_singlefloat: {
		float d = ecl_single_float(x);
		float q = round_double(d);
		v0 = _ecl_float_to_integer(q);
		v1 = ecl_make_single_float(d - q);
		break;
	}
	case t_doublefloat: {
		double d = ecl_double_float(x);
		double q = round_double(d);
		v0 = _ecl_double_to_integer(q);
		v1 = ecl_make_double_float(d - q);
		break;
	}
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {
		long double d = ecl_long_float(x);
		long double q = round_long_double(d);
		v0 = _ecl_long_double_to_integer(q);
		v1 = ecl_make_long_float(d - q);
		break;
	}
#endif
	default:
                FEwrong_type_nth_arg(@[round],1,x,@[real]);
	}
	ecl_return2(the_env, v0, v1);
}

cl_object
ecl_round2(cl_object x, cl_object y)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_object v0, v1;
	cl_object q;

	q = ecl_divide(x, y);
	switch (ecl_t_of(q)) {
	case t_fixnum:
	case t_bignum:
		v0 = q;
		v1 = ecl_make_fixnum(0);
		break;
	case t_ratio:
		return ecl_round2_integer(the_env, x, y, q);
	default:
		v0 = q = ecl_round1(q);
		v1 = number_remainder(x, y, q);
	}
	ecl_return2(the_env, v0, v1);
}
