/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    num_co.c -- Operations on floating-point numbers.
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

/*
	IMPLEMENTATION-DEPENDENT

	This file contains those functions
	that know the representation of floating-point numbers.
*/

#define ECL_INCLUDE_MATH_H
#include <ecl/ecl.h>
#include <float.h>
#ifndef HAVE_ISOC99
# define floorf floor
# define ceilf ceil
# define fabsf fabs
# define frexpf frexp
# define ldexpf ldexp
# define cosf cos
# define coshf cosh
# define expf exp
# define logf log
# define sinf sin
# define sqrtf sqrt
# define tanf tan
# define tanhf tanh
#endif
#include <ecl/internal.h>

/* Coerce X to single-float if one arg,
   otherwise coerce to same float type as second arg */

@(defun float (x &optional (y OBJNULL))
	cl_type ty, tx;
@
	if (y != OBJNULL) {
		ty = ecl_t_of(y);
	} else {
		ty = t_singlefloat;
	}
	switch (tx = ecl_t_of(x)) {
	case t_singlefloat:
	case t_doublefloat:
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
#endif
		if (y == OBJNULL || ty == tx)
			break;
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		switch (ty) {
		case t_singlefloat:
			x = ecl_make_single_float(ecl_to_double(x)); break;
		case t_doublefloat:
			x = ecl_make_double_float(ecl_to_double(x)); break;
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			x = ecl_make_long_float(ecl_to_long_double(x)); break;
#endif
		default:
                        FEwrong_type_nth_arg(@[float],2,y,@[float]);
		}
		break;
	default:
                FEwrong_type_nth_arg(@[float],1,x,@[real]);
	}
	@(return x)
@)

cl_object
cl_numerator(cl_object x)
{
	switch (ecl_t_of(x)) {
	case t_ratio:
		x = x->ratio.num;
		break;
	case t_fixnum:
	case t_bignum:
		break;
	default:
                FEwrong_type_nth_arg(@[numerator],1,x,@[rational]);
	}
	@(return x)
}

cl_object
cl_denominator(cl_object x)
{
	switch (ecl_t_of(x)) {
	case t_ratio:
		x = x->ratio.den;
		break;
	case t_fixnum:
	case t_bignum:
		x = ecl_make_fixnum(1);
		break;
	default:
                FEwrong_type_nth_arg(@[numerator],1,x,@[rational]);
	}
	@(return x)
}

cl_object
cl_mod(cl_object x, cl_object y)
{
	const cl_env_ptr the_env = ecl_process_env();
	/* INV: #'floor always outputs two values */
	@floor(2, x, y);
	ecl_return1(the_env, the_env->values[1]);
}

cl_object
cl_rem(cl_object x, cl_object y)
{
	const cl_env_ptr the_env = ecl_process_env();
	@truncate(2, x, y);
	ecl_return1(the_env, the_env->values[1]);
}

cl_object
cl_decode_float(cl_object x)
{
	const cl_env_ptr the_env = ecl_process_env();
	int e, s;
	cl_type tx = ecl_t_of(x);
	float f;

	switch (tx) {
	case t_singlefloat: {
		f = ecl_single_float(x);
		if (f >= 0.0) {
			s = 1;
		} else {
			f = -f;
			s = 0;
		}
		f = frexpf(f, &e);
		x = ecl_make_single_float(f);
		break;
	}
	case t_doublefloat: {
		double d = ecl_double_float(x);
		if (d >= 0.0) {
			s = 1;
		} else {
			d = -d;
			s = 0;
		}
		d = frexp(d, &e);
		x = ecl_make_double_float(d);
		break;
	}
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {
		long double d = ecl_long_float(x);
		if (d >= 0.0)
			s = 1;
		else {
			d = -d;
			s = 0;
		}
		d = frexpl(d, &e);
		x = ecl_make_long_float(d);
		break;
	}
#endif
	default:
                FEwrong_type_nth_arg(@[decode-float],1,x,@[float]);
	}
	ecl_return3(the_env, x, ecl_make_fixnum(e), ecl_make_single_float(s));
}

cl_object
cl_scale_float(cl_object x, cl_object y)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_fixnum k;

	if (ECL_FIXNUMP(y)) {
		k = ecl_fixnum(y);
	} else {
		FEwrong_type_nth_arg(@[scale-float],2,y,@[fixnum]);
	}
	switch (ecl_t_of(x)) {
	case t_singlefloat:
		x = ecl_make_single_float(ldexpf(ecl_single_float(x), k));
		break;
	case t_doublefloat:
		x = ecl_make_double_float(ldexp(ecl_double_float(x), k));
		break;
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		x = ecl_make_long_float(ldexpl(ecl_long_float(x), k));
		break;
#endif
	default:
                FEwrong_type_nth_arg(@[scale-float],1,x,@[float]);
	}
	ecl_return1(the_env, x);
}

cl_object
cl_float_radix(cl_object x)
{
	const cl_env_ptr the_env = ecl_process_env();
	if (ecl_unlikely(cl_floatp(x) != ECL_T)) {
		FEwrong_type_nth_arg(@[float-radix],1,x,@[float]);
	}
	ecl_return1(the_env, ecl_make_fixnum(FLT_RADIX));
}

int
ecl_signbit(cl_object x)
{
	switch (ecl_t_of(x)) {
	case t_singlefloat:
		return signbit(ecl_single_float(x));
	case t_doublefloat:
		return signbit(ecl_double_float(x));
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		return signbit(ecl_long_float(x));
#endif
	default:
                FEwrong_type_nth_arg(@[float-sign],1,x,@[float]);
	}
}

@(defun float_sign (x &optional (y x yp))
	int negativep;
@
	if (!yp) {
		y = cl_float(2, ecl_make_fixnum(1), x);
	}
	negativep = ecl_signbit(x);
	switch (ecl_t_of(y)) {
	case t_singlefloat: {
		float f = ecl_single_float(y);
                if (signbit(f) != negativep) y = ecl_make_single_float(-f);
		break;
	}
	case t_doublefloat: {
		double f = ecl_double_float(y);
                if (signbit(f) != negativep) y = ecl_make_double_float(-f);
		break;
	}
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {
		long double f = ecl_long_float(y);
                if (signbit(f) != negativep) y = ecl_make_long_float(-f);
		break;
	}
#endif
	default:
                FEwrong_type_nth_arg(@[float-sign],2,y,@[float]);
	}
	@(return y);
@)

cl_object
cl_float_digits(cl_object x)
{
	const cl_env_ptr the_env = ecl_process_env();
	switch (ecl_t_of(x)) {
	case t_singlefloat:
		x = ecl_make_fixnum(FLT_MANT_DIG);
		break;
	case t_doublefloat:
		x = ecl_make_fixnum(DBL_MANT_DIG);
		break;
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		x = ecl_make_fixnum(LDBL_MANT_DIG);
		break;
#endif
	default:
                FEwrong_type_nth_arg(@[float-digits],1,x,@[float]);
	}
	ecl_return1(the_env, x);
}

cl_object
cl_float_precision(cl_object x)
{
	const cl_env_ptr the_env = ecl_process_env();
	int precision;
	switch (ecl_t_of(x)) {
	case t_singlefloat: {
		float f = ecl_single_float(x);
		if (f == 0.0) {
			precision = 0;
		} else {
			int exp;
			frexpf(f, &exp);
			if (exp >= FLT_MIN_EXP) {
				precision = FLT_MANT_DIG;
			} else {
				precision = FLT_MANT_DIG - (FLT_MIN_EXP - exp);
			}
		}
		break;
	}
	case t_doublefloat: {
		double f = ecl_double_float(x);
		if (f == 0.0) {
			precision = 0;
		} else {
			int exp;
			frexp(f, &exp);
			if (exp >= DBL_MIN_EXP) {
				precision = DBL_MANT_DIG;
			} else {
				precision = DBL_MANT_DIG - (DBL_MIN_EXP - exp);
			}
		}
		break;
	}
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {
		long double f = ecl_long_float(x);
		if (f == 0.0) {
			precision = 0;
		} else {
			int exp;
			frexp(f, &exp);
			if (exp >= LDBL_MIN_EXP) {
				precision = LDBL_MANT_DIG;
			} else {
				precision = LDBL_MANT_DIG - (LDBL_MIN_EXP - exp);
			}
		}
		break;
	}
#endif
	default:
		FEwrong_type_nth_arg(@[float-precision],1,x,@[float]);
	}
	ecl_return1(the_env, ecl_make_fixnum(precision));
}

cl_object
cl_integer_decode_float(cl_object x)
{
	const cl_env_ptr the_env = ecl_process_env();
	int e, s = 1;

	switch (ecl_t_of(x)) {
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {
		long double d = ecl_long_float(x);
                if (signbit(d)) {
                        s = -1;
                        d = -d;
                }
		if (d == 0.0) {
			e = 0;
			x = ecl_make_fixnum(0);
		} else {
                        d = frexpl(d, &e);
			x = _ecl_long_double_to_integer(ldexpl(d, LDBL_MANT_DIG));
			e -= LDBL_MANT_DIG;
		}
		break;
	}
#endif
	case t_doublefloat: {
		double d = ecl_double_float(x);
                if (signbit(d)) {
                        s = -1;
                        d = -d;
                }
		if (d == 0.0) {
			e = 0;
			x = ecl_make_fixnum(0);
		} else {
                        d = frexp(d, &e);
			x = _ecl_double_to_integer(ldexp(d, DBL_MANT_DIG));
			e -= DBL_MANT_DIG;
		}
		break;
	}
	case t_singlefloat: {
		float d = ecl_single_float(x);
                if (signbit(d)) {
                        s = -1;
                        d = -d;
                }
		if (d == 0.0) {
			e = 0;
			x = ecl_make_fixnum(0);
		} else {
                        d = frexpf(d, &e);
			x = _ecl_double_to_integer(ldexp(d, FLT_MANT_DIG));
			e -= FLT_MANT_DIG;
		}
		break;
	}
	default:
		FEwrong_type_nth_arg(@[integer-decode-float],1,x,@[float]);
	}
	ecl_return3(the_env, x, ecl_make_fixnum(e), ecl_make_fixnum(s));
}


@(defun complex (r &optional (i ecl_make_fixnum(0)))
@	/* INV: ecl_make_complex() checks types */
	@(return ecl_make_complex(r, i))
@)

cl_object
cl_realpart(cl_object x)
{
	switch (ecl_t_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
	case t_singlefloat:
	case t_doublefloat:
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
#endif
		break;
	case t_complex:
		x = x->complex.real;
		break;
	default:
		FEwrong_type_nth_arg(@[realpart],1,x,@[number]);
	}
	@(return x)
}

cl_object
cl_imagpart(cl_object x)
{
	switch (ecl_t_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		x = ecl_make_fixnum(0);
		break;
	case t_singlefloat:
                if (signbit(ecl_single_float(x)))
                        x = cl_core.singlefloat_minus_zero;
                else
                        x = cl_core.singlefloat_zero;
		break;
	case t_doublefloat:
                if (signbit(ecl_double_float(x)))
                        x = cl_core.doublefloat_minus_zero;
                else
                        x = cl_core.doublefloat_zero;
		break;
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
                if (signbit(ecl_long_float(x)))
                        x = cl_core.longfloat_minus_zero;
                else
                        x = cl_core.longfloat_zero;
		break;
#endif
	case t_complex:
		x = x->complex.imag;
		break;
	default:
                FEwrong_type_nth_arg(@[imagpart],1,x,@[number]);
	}
	@(return x)
}
