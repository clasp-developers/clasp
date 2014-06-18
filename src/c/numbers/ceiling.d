/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    ceiling.d  -- Implementation of CL:CEILING
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
# define ceilf ceil
#endif
#include <ecl/internal.h>

@(defun ceiling (x &optional (y OBJNULL))
@
	if (narg == 1)
		return ecl_ceiling1(x);
	else
		return ecl_ceiling2(x, y);
@)

cl_object
ecl_ceiling1(cl_object x)
{
	cl_object v0, v1;
	switch (ecl_t_of(x)) {
	case t_fixnum:
	case t_bignum:
		v0 = x;
		v1 = ecl_make_fixnum(0);
		break;
	case t_ratio: {
		const cl_env_ptr the_env = ecl_process_env();
		v0 = ecl_ceiling2(x->ratio.num, x->ratio.den);
		v1 = ecl_make_ratio(ecl_nth_value(the_env, 1), x->ratio.den);
		break;
	}
	case t_singlefloat: {
		float d = ecl_single_float(x);
		float y = ceilf(d);
		v0 = _ecl_float_to_integer(y);
		v1 = ecl_make_single_float(d - y);
		break;
	}
	case t_doublefloat: {
		double d = ecl_double_float(x);
		double y = ceil(d);
		v0 = _ecl_double_to_integer(y);
		v1 = ecl_make_double_float(d - y);
		break;
	}
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {
		long double d = ecl_long_float(x);
		long double y = ceill(d);
		v0 = _ecl_long_double_to_integer(y);
		v1 = ecl_make_long_float(d - y);
		break;
	}
#endif
	default:
		FEwrong_type_nth_arg(@[ceiling],1,x,@[real]);
	}
	@(return v0 v1)
}

cl_object
ecl_ceiling2(cl_object x, cl_object y)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_object v0, v1;
	cl_type ty;
        ty = ecl_t_of(y);
	if (ecl_unlikely(!ECL_REAL_TYPE_P(ty))) {
		FEwrong_type_nth_arg(@[ceiling],2, y, @[real]);
	}
	switch(ecl_t_of(x)) {
	case t_fixnum:
		switch(ty) {
		case t_fixnum: {	/* FIX / FIX */
		  cl_fixnum a = ecl_fixnum(x); cl_fixnum b = ecl_fixnum(y);
		  cl_fixnum q = a / b;  cl_fixnum r = a % b;
		  if ((r^b) > 0 && r) {	/* same signs and some remainder */
		    v0 = ecl_make_fixnum(q+1);
		    v1 = ecl_make_fixnum(r-b);
		  } else {
		    v0 = ecl_make_fixnum(q);
		    v1 = ecl_make_fixnum(r);
		  }
		  break;
		}
		case t_bignum: {	/* FIX / BIG */
		  /* We must perform the division because there is the
		   * pathological case
		   *	x = MOST_NEGATIVE_FIXNUM
		   *    y = - MOST_NEGATIVE_FIXNUM
		   */
                  ECL_WITH_TEMP_BIGNUM(bx,4);
                  _ecl_big_set_fixnum(bx, ecl_fixnum(x));
                  v0 = _ecl_big_ceiling(bx, y, &v1);
		  break;
		}
		case t_ratio:		/* FIX / RAT */
		  v0 = ecl_ceiling2(ecl_times(x, y->ratio.den), y->ratio.num);
		  v1 = ecl_make_ratio(ecl_nth_value(the_env, 1), y->ratio.den);
		  break;
		case t_singlefloat: {	/* FIX / SF */
		  float n = ecl_single_float(y);
		  float p = ecl_fixnum(x)/n;
		  float q = ceilf(p);
		  v0 = _ecl_float_to_integer(q);
		  v1 = ecl_make_single_float(p*n - q*n);
		  break;
		}
		case t_doublefloat: {	/* FIX / DF */
		  double n = ecl_double_float(y);
		  double p = ecl_fixnum(x)/n;
		  double q = ceil(p);
		  v0 = _ecl_double_to_integer(q);
		  v1 = ecl_make_double_float(p*n - q*n);
		  break;
		}
#ifdef ECL_LONG_FLOAT
		case t_longfloat: {	/* FIX / LF */
		  long double n = ecl_long_float(y);
		  long double p = ecl_fixnum(x)/n;
		  long double q = ceill(p);
		  v0 = _ecl_long_double_to_integer(q);
		  v1 = ecl_make_long_float(p*n - q*n);
		  break;
		}
#endif
		default:
		  (void)0; /*Never reached */
		}
		break;
	case t_bignum:
		switch(ecl_t_of(y)) {
		case t_fixnum: {	/* BIG / FIX */
                  ECL_WITH_TEMP_BIGNUM(by,4);
                  _ecl_big_set_fixnum(by, ecl_fixnum(y));
                  v0 = _ecl_big_ceiling(x, by, &v1);
		  break;
		}
		case t_bignum: {	/* BIG / BIG */
                  v0 = _ecl_big_ceiling(x, y, &v1);
		  break;
		}
		case t_ratio:		/* BIG / RAT */
		  v0 = ecl_ceiling2(ecl_times(x, y->ratio.den), y->ratio.num);
		  v1 = ecl_make_ratio(ecl_nth_value(the_env, 1), y->ratio.den);
		  break;
		case t_singlefloat: {	/* BIG / SF */
		  float n = ecl_single_float(y);
		  float p = _ecl_big_to_double(x)/n;
		  float q = ceilf(p);
		  v0 = _ecl_float_to_integer(q);
		  v1 = ecl_make_single_float(p*n - q*n);
		  break;
		}
		case t_doublefloat: {	/* BIG / DF */
		  double n = ecl_double_float(y);
		  double p = _ecl_big_to_double(x)/n;
		  double q = ceil(p);
		  v0 = _ecl_double_to_integer(q);
		  v1 = ecl_make_double_float(p*n - q*n);
		  break;
		}
#ifdef ECL_LONG_FLOAT
		case t_longfloat: {	/* BIG / LF */
		  long double n = ecl_long_float(y);
		  long double p = _ecl_big_to_double(x)/n;
		  long double q = ceill(p);
		  v0 = _ecl_long_double_to_integer(q);
		  v1 = ecl_make_long_float(p*n - q*n);
		  break;
		}
#endif
		default:
		  (void)0; /*Never reached */
		}
		break;
	case t_ratio:
		switch(ecl_t_of(y)) {
		case t_ratio:		/* RAT / RAT */
		  v0 = ecl_ceiling2(ecl_times(x->ratio.num, y->ratio.den),
				    ecl_times(x->ratio.den, y->ratio.num));
		  v1 = ecl_make_ratio(ecl_nth_value(the_env, 1), ecl_times(x->ratio.den, y->ratio.den));
		  break;
		default:		/* RAT / ANY */
		  v0 = ecl_ceiling2(x->ratio.num, ecl_times(x->ratio.den, y));
		  v1 = ecl_divide(ecl_nth_value(the_env, 1), x->ratio.den);
		}
		break;
	case t_singlefloat: {		/* SF / ANY */
		float n = ecl_to_double(y);
		float p = ecl_single_float(x)/n;
		float q = ceilf(p);
		v0 = _ecl_float_to_integer(q);
		v1 = ecl_make_single_float(p*n - q*n);
		break;
	}
	case t_doublefloat: {		/* DF / ANY */
		double n = ecl_to_double(y);
		double p = ecl_double_float(x)/n;
		double q = ceil(p);
		v0 = _ecl_double_to_integer(q);
		v1 = ecl_make_double_float(p*n - q*n);
		break;
	}
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {		/* LF / ANY */
		long double n = ecl_to_long_double(y);
		long double p = ecl_long_float(x)/n;
		long double q = ceill(p);
		v0 = _ecl_long_double_to_integer(q);
		v1 = ecl_make_long_float(p*n - q*n);
		break;
	}
#endif
	default:
                FEwrong_type_nth_arg(@[ceiling], 1, x, @[real]);
	}
	ecl_return2(the_env, v0, v1);
}
