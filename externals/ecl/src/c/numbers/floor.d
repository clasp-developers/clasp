/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    floor.d  -- Implementation of CL:FLOOR
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
#ifndef HAVE_ISOC99
# define floorf floor
#endif
#include <ecl/impl/math_dispatch2.h>
#include <ecl/internal.h>

@(defun floor (x &optional (y OBJNULL))
@
	if (narg == 1)
		return ecl_floor1(x);
	else
		return ecl_floor2(x, y);
@)

cl_object
ecl_floor1(cl_object x)
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
		v0 = ecl_floor2(x->ratio.num, x->ratio.den);
		v1 = ecl_make_ratio(ecl_nth_value(the_env, 1), x->ratio.den);
		break;
	case t_singlefloat: {
		float d = ecl_single_float(x);
		float y = floorf(d);
		v0 = _ecl_float_to_integer(y);
		v1 = ecl_make_single_float(d - y);
		break;
	}
	case t_doublefloat: {
		double d = ecl_double_float(x);
		double y = floor(d);
		v0 = _ecl_double_to_integer(y);
		v1 = ecl_make_double_float(d - y);
		break;
	}
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {
		long double d = ecl_long_float(x);
		long double y = floorl(d);
		v0 = _ecl_long_double_to_integer(y);
		v1 = ecl_make_long_float(d - y);
		break;
	}
#endif
	default:
                FEwrong_type_nth_arg(@[floor],1,x,@[real]);
	}
	ecl_return2(the_env, v0, v1);
}

cl_object
ecl_floor2(cl_object x, cl_object y)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_object v0, v1;
MATH_DISPATCH2_BEGIN(x,y)
{
	CASE_FIXNUM_FIXNUM {
		cl_fixnum a = ecl_fixnum(x), b = ecl_fixnum(y);
		cl_fixnum q = a / b,  r = a % b;
		if ((r^b) < 0 && r) {	/* opposite sign and some remainder*/
			v0 = ecl_make_fixnum(q-1);
			v1 = ecl_make_fixnum(r+b);
		} else {
			v0 = ecl_make_fixnum(q);
			v1 = ecl_make_fixnum(r);
		}
		break;
	}
	CASE_FIXNUM_BIGNUM {
		/* We must perform the division because there is the
		 * pathological case
		 *	x = MOST_NEGATIVE_FIXNUM
		 *    y = - MOST_NEGATIVE_FIXNUM
		 */
		ECL_WITH_TEMP_BIGNUM(bx,4);
		_ecl_big_set_fixnum(bx, ecl_fixnum(x));
		v0 = _ecl_big_floor(bx, y, &v1);
		break;
	}
	CASE_FIXNUM_RATIO {
		v0 = ecl_floor2(ecl_times(x, y->ratio.den), y->ratio.num);
		v1 = ecl_make_ratio(ecl_nth_value(the_env, 1), y->ratio.den);
		break;
	}
	CASE_FIXNUM_SINGLE_FLOAT {
		float n = ecl_single_float(y);
		float p = ecl_fixnum(x) / n;
		float q = floorf(p);
		v0 = _ecl_float_to_integer(q);
		v1 = ecl_make_single_float((p - q)*n);
		break;
	}
	CASE_FIXNUM_DOUBLE_FLOAT {
		double n = ecl_double_float(y);
		double p = ecl_fixnum(x) / n;
		double q = floor(p);
		v0 = _ecl_double_to_integer(q);
		v1 = ecl_make_double_float((p - q)*n);
		break;
	}
#ifdef ECL_LONG_FLOAT
	CASE_FIXNUM_LONG_FLOAT {	/* FIX / LF */
		long double n = ecl_long_float(y);
		long double p = ecl_fixnum(x) / n;
		long double q = floorl(p);
		v0 = _ecl_long_double_to_integer(q);
		v1 = ecl_make_long_float((p - q)*n);
		break;
	}
#endif
	CASE_BIGNUM_FIXNUM {
		ECL_WITH_TEMP_BIGNUM(by,4);
		_ecl_big_set_fixnum(by, ecl_fixnum(y));
		v0 = _ecl_big_floor(x, by, &v1);
		break;
	}
	CASE_BIGNUM_BIGNUM {
		v0 = _ecl_big_floor(x, y, &v1);
		break;
	}
	CASE_BIGNUM_RATIO {
		v0 = ecl_floor2(ecl_times(x, y->ratio.den), y->ratio.num);
		v1 = ecl_make_ratio(ecl_nth_value(the_env, 1), y->ratio.den);
		break;
	}
	CASE_BIGNUM_SINGLE_FLOAT {
		float n = ecl_single_float(y);
		float p = _ecl_big_to_double(x) / n;
		float q = floorf(p);
		v0 = _ecl_float_to_integer(q);
		v1 = ecl_make_single_float((p - q)*n);
		break;
	}
	CASE_BIGNUM_DOUBLE_FLOAT {
		double n = ecl_double_float(y);
		double p = _ecl_big_to_double(x) / n;
		double q = floor(p);
		v0 = _ecl_double_to_integer(q);
		v1 = ecl_make_double_float((p - q)*n);
		break;
	}
#ifdef ECL_LONG_FLOAT
	CASE_BIGNUM_LONG_FLOAT {
		long double n = ecl_long_float(y);
		long double p = _ecl_big_to_double(x) / n;
		long double q = floorl(p);
		v0 = _ecl_long_double_to_integer(q);
		v1 = ecl_make_long_float((p - q)*n);
		break;
	}
#endif
	CASE_RATIO_RATIO {
		v0 = ecl_floor2(ecl_times(x->ratio.num, y->ratio.den),
				ecl_times(x->ratio.den, y->ratio.num));
		v1 = ecl_make_ratio(ecl_nth_value(the_env, 1), ecl_times(x->ratio.den, y->ratio.den));
		break;
	}
	CASE_RATIO_FIXNUM;
	CASE_RATIO_BIGNUM;
	CASE_RATIO_SINGLE_FLOAT;
#ifdef ECL_LONG_FLOAT
	CASE_RATIO_LONG_FLOAT;
#endif
	CASE_RATIO_DOUBLE_FLOAT {
		v0 = ecl_floor2(x->ratio.num, ecl_times(x->ratio.den, y));
		v1 = ecl_divide(ecl_nth_value(the_env, 1), x->ratio.den);
		break;
	}

	CASE_SINGLE_FLOAT_FIXNUM;
	CASE_SINGLE_FLOAT_BIGNUM;
	CASE_SINGLE_FLOAT_RATIO;
	CASE_SINGLE_FLOAT_DOUBLE_FLOAT;
#ifdef ECL_LONG_FLOAT	
	CASE_SINGLE_FLOAT_LONG_FLOAT;
#endif
	CASE_SINGLE_FLOAT_SINGLE_FLOAT {
		float n = ecl_to_double(y);
		float p = ecl_single_float(x)/n;
		float q = floorf(p);
		v0 = _ecl_float_to_integer(q);
		/* We cannot factor these two multiplications because
		 * if we have signed zeros (1 - 1) * (-1) = -0 while
		 * 1*(-1) - 1*(-1) = +0 */
		v1 = ecl_make_single_float(p*n - q*n);
		break;
	}
	CASE_DOUBLE_FLOAT_FIXNUM;
	CASE_DOUBLE_FLOAT_BIGNUM;
	CASE_DOUBLE_FLOAT_RATIO;
	CASE_DOUBLE_FLOAT_SINGLE_FLOAT;
#ifdef ECL_LONG_FLOAT	
	CASE_DOUBLE_FLOAT_LONG_FLOAT;
#endif
	CASE_DOUBLE_FLOAT_DOUBLE_FLOAT {
		double n = ecl_to_double(y);
		double p = ecl_double_float(x)/n;
		double q = floor(p);
		v0 = _ecl_double_to_integer(q);
		v1 = ecl_make_double_float(p*n - q*n);
		break;
	}
#ifdef ECL_LONG_FLOAT
	CASE_LONG_FLOAT_FIXNUM;
	CASE_LONG_FLOAT_BIGNUM;
	CASE_LONG_FLOAT_RATIO;
	CASE_LONG_FLOAT_SINGLE_FLOAT;
	CASE_LONG_FLOAT_DOUBLE_FLOAT;
	CASE_LONG_FLOAT_LONG_FLOAT {
		long double n = ecl_to_long_double(y);
		long double p = ecl_long_float(x)/n;
		long double q = floorl(p);
		v0 = _ecl_long_double_to_integer(q);
		v1 = ecl_make_long_float(p*n - q*n);
		break;
	}
#endif
	default: DISPATCH2_ERROR: {
		if (!ecl_realp(x))
			FEwrong_type_nth_arg(@[floor], 1, x, @[real]);
		else
			FEwrong_type_nth_arg(@[floor], 2, y, @[real]);
	}
}
MATH_DISPATCH2_END;
	ecl_return2(the_env, v0, v1);
}

