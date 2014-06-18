/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    log1.d  -- Trascendental functions: log(x)
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

static cl_object
ecl_log1_complex_inner(cl_object r, cl_object i)
{
	cl_object a = ecl_abs(r);
	cl_object p = ecl_abs(i);
	int rel = ecl_number_compare(a, p);
	if (rel > 0) {
		cl_object aux = p;
		p = a; a = aux;
	} else if (rel == 0) {
		/* if a == p, 
		 * log(sqrt(a^2+p^2)) = log(2a^2)/2
		 */
		a = ecl_times(a, a);
		a = ecl_divide(ecl_log1(ecl_plus(a, a)), ecl_make_fixnum(2));
		goto OUTPUT;
	}
	/* For the real part of the output we use the formula
	 *	log(sqrt(p^2 + a^2)) = log(sqrt(p^2*(1 + (a/p)^2)))
	 *			     = log(p) + log(1 + (a/p)^2)/2; */
	a = ecl_divide(a, p);
	a = ecl_plus(ecl_divide(ecl_log1p(ecl_times(a,a)), ecl_make_fixnum(2)),
		     ecl_log1(p));
 OUTPUT:
	p = ecl_atan2(i, r);
	return ecl_make_complex(a, p);
}

static cl_object
ecl_log1_bignum(cl_object x)
{
        if (ecl_minusp(x)) {
                return ecl_log1_complex_inner(x, ecl_make_fixnum(0));
        } else {
                cl_fixnum l = ecl_integer_length(x) - 1;
                cl_object r = ecl_make_ratio(x, ecl_ash(ecl_make_fixnum(1), l));
                float d = logf(ecl_to_float(r)) + l * logf(2.0);
                return ecl_make_single_float(d);
        }
}

static cl_object
ecl_log1_rational(cl_object x)
{
        float f = ecl_to_float(x);
        if (f < 0) return ecl_log1_complex_inner(x, ecl_make_fixnum(0));
        return ecl_make_single_float(logf(ecl_to_float(x)));
}

static cl_object
ecl_log1_single_float(cl_object x)
{
        float f = ecl_single_float(x);
        if (isnan(f)) return x;
        if (f < 0) return ecl_log1_complex_inner(x, ecl_make_fixnum(0));
        return ecl_make_single_float(logf(f));
}

static cl_object
ecl_log1_double_float(cl_object x)
{
        double f = ecl_double_float(x);
        if (isnan(f)) return x;
        if (f < 0) return ecl_log1_complex_inner(x, ecl_make_fixnum(0));
        return ecl_make_double_float(log(f));
}

#ifdef ECL_LONG_FLOAT
static cl_object
ecl_log1_long_float(cl_object x)
{
        long double f = ecl_long_float(x);
        if (isnan(f)) return x;
        if (f < 0) return ecl_log1_complex_inner(x, ecl_make_fixnum(0));
        return ecl_make_long_float(logl(f));
}
#endif

static cl_object
ecl_log1_complex(cl_object x)
{
        return ecl_log1_complex_inner(x->complex.real, x->complex.imag);
}

MATH_DEF_DISPATCH1(log1, @[log], @[number],
                   ecl_log1_rational, ecl_log1_bignum, ecl_log1_rational,
                   ecl_log1_single_float, ecl_log1_double_float, ecl_log1_long_float,
                   ecl_log1_complex);

cl_object
ecl_log2(cl_object x, cl_object y)
{
	return ecl_divide(ecl_log1(y), ecl_log1(x));
}

@(defun log (x &optional (y OBJNULL))
@	/* INV: type check in ecl_log1() and ecl_log2() */
	if (y == OBJNULL)
		@(return ecl_log1(x))
	@(return ecl_log2(y, x))
@)


#ifndef HAVE_LOG1P
double
log1p(double x)
{
	double u = 1.0 + x;
	if (u == 1) {
		return 0.0;
	} else {
		return (log(u) * x)/(u - 1.0);
	}
}
#endif

#ifndef HAVE_LOG1PF
float
log1pf(float x)
{
	float u = (float)1 + x;
	if (u == 1) {
		return (float)0;
	} else {
		return (logf(u) * x)/(u - (float)1);
	}
}
#endif

#if !defined(HAVE_LOG1PL) && defined(ECL_LONG_FLOAT)
long double
log1pl(long double x)
{
	long double u = (long double)1 + x;
	if (u == 1) {
		return (long double)1;
	} else {
		return (logl(u) * x)/(u - (long double)1);
	}
}
#endif

cl_object
si_log1p(cl_object x)
{
	@(return ecl_log1p(x));
}

static cl_object
ecl_log1p_simple(cl_object x)
{
        return ecl_log1_complex_inner(ecl_one_plus(x), ecl_make_fixnum(0));
}

static cl_object
ecl_log1p_rational(cl_object x)
{
        float f = ecl_to_float(x);
        if (f < -1) return ecl_log1p_simple(x);
        return ecl_make_single_float(log1pf(ecl_to_float(x)));
}

static cl_object
ecl_log1p_single_float(cl_object x)
{
        float f = ecl_single_float(x);
        if (isnan(f)) return x;
        if (f < -1) return ecl_log1p_simple(x);
        return ecl_make_single_float(log1pf(f));
}

static cl_object
ecl_log1p_double_float(cl_object x)
{
        double f = ecl_double_float(x);
        if (isnan(f)) return x;
        if (f < -1) return ecl_log1p_simple(x);
        return ecl_make_double_float(log1p(f));
}

#ifdef ECL_LONG_FLOAT
static cl_object
ecl_log1p_long_float(cl_object x)
{
        long double f = ecl_long_float(x);
        if (isnan(f)) return x;
        if (f < -1) return ecl_log1p_simple(x);
        return ecl_make_long_float(log1pl(f));
}
#endif

static cl_object
ecl_log1p_complex(cl_object x)
{
        return ecl_log1_complex_inner(ecl_one_plus(x->complex.real), x->complex.imag);
}

MATH_DEF_DISPATCH1(log1p, @[si::log1p], @[number],
                   ecl_log1p_rational, ecl_log1p_simple, ecl_log1p_rational,
                   ecl_log1p_single_float, ecl_log1p_double_float, ecl_log1p_long_float,
                   ecl_log1p_complex);
