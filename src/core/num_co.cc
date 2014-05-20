/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    num_co.c -- Operations on floating-point numbers.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2013, Christian E. Schafmeister


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

#include <float.h>

#include "foundation.h"
#include "object.h"
#include "symbolTable.h"
#include "numbers.h"
#include "num_arith.h"
#include "bignum.h"
#include "wrappers.h"

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

namespace core {

    static Number_sp
    number_remainder(Number_sp x, Number_sp y, Number_sp q)
    {
	Number_sp z;
    
	z = brcl_times(q, y);
	z = brcl_minus(x, z);
	return(z);
    }

/* Coerce X to single-float if one arg,
   otherwise coerce to same float type as second arg */



#define ARGS_cl_float "(x &optional y)"
#define DECL_cl_float ""
#define DOCS_cl_float "float"
    Float_sp cl_float(Real_sp x, Float_sp y)
    {_G();
	NumberType ty, tx;
	if (y.notnilp()) {
	    ty = y->number_type();
	} else {
	    ty = number_SingleFloat;
	}
	switch (tx = x->number_type()) {
	case number_SingleFloat:
	case number_DoubleFloat:
#ifdef BRCL_LONG_FLOAT
	case number_LongFloat:
#endif
	    if (y.nilp() || ty == tx)
		break;
	case number_Fixnum:
	case number_Bignum:
	case number_Ratio:
	    switch (ty) {
	    case number_SingleFloat:
		x = brcl_make_single_float(brcl_to_double(x)).as<Real_O>();
		break;
	    case number_DoubleFloat:
		x = brcl_make_double_float(brcl_to_double(x)).as<Real_O>();
		break;
#ifdef BRCL_LONG_FLOAT
	    case number_LongFloat:
		x = brcl_make_long_float(brcl_to_long_float(x)).as<Real_O>();
		break;
#endif
	    default:
		WRONG_TYPE_NTH_ARG(2,y,cl::_sym_float);
	    }
	    break;
	default:
	    WRONG_TYPE_NTH_ARG(1,x,cl::_sym_Real_O);
	}
	return x.as<Float_O>();
    }


    
    
    
#define ARGS_cl_numerator "(x)"
#define DECL_cl_numerator ""
#define DOCS_cl_numerator "numerator"
    Number_sp cl_numerator(Number_sp x)
    {_G();
	switch (brcl_t_of(x)) {
	case number_Ratio:
	    x = x.as<Ratio_O>()->numerator();
	    break;
	case number_Fixnum:
	case number_Bignum:
	    break;
	default:
	    WRONG_TYPE_NTH_ARG(1,x,cl::_sym_Rational_O);
	}
	return x;
    }


    
    
#define ARGS_cl_denomenator "(x)"
#define DECL_cl_denomenator ""
#define DOCS_cl_denomenator "denomenator"
    Number_sp cl_denomenator(Number_sp x)
    {_G();
	switch (brcl_t_of(x)) {
	case number_Ratio:
	    x = x.as<Ratio_O>()->denomenator();
	    break;
	case number_Fixnum:
	case number_Bignum:
	    x = brcl_make_fixnum(1);
	    break;
	default:
	    WRONG_TYPE_NTH_ARG(1,x,cl::_sym_Rational_O);
	}
	return x;
    }



    Real_mv brcl_floor1(Real_sp x)
    {
	Real_sp v0, v1;
	switch (brcl_t_of(x)) {
	case number_Fixnum:
	case number_Bignum:
	    v0 = x;
	    v1 = brcl_make_fixnum(0);
	    break;
	case number_Ratio: {
	    Ratio_sp rx(x.as<Ratio_O>());
	    Real_mv mv_v0 = brcl_floor2(rx->numerator(),rx->denomenator());
	    v0 = mv_v0;
	    Integer_sp tv1 = mv_v0.valueGet(1).as<Integer_O>();
	    v1 = brcl_make_ratio(tv1, rx->den());
	    break;
	}
	case number_SingleFloat: {
	    float d = brcl_single_float(x);
	    float y = floorf(d);
	    v0 = _brcl_float_to_integer(y);
	    v1 = brcl_make_single_float(d - y);
	    break;
	}
	case number_DoubleFloat: {
	    double d = brcl_double_float(x);
	    double y = floor(d);
	    v0 = _brcl_double_to_integer(y);
	    v1 = brcl_make_double_float(d - y);
	    break;
	}
#ifdef BRCL_LONG_FLOAT
	case number_LongFloat: {
	    LongFloat d = brcl_long_float(x);
	    LongFloat y = floorl(d);
	    v0 = _brcl_long_double_to_integer(y);
	    v1 = brcl_make_long_float(d - y);
	    break;
	}
#endif
	default:
	    WRONG_TYPE_NTH_ARG(1,x,cl::_sym_Real_O);
	}
	brcl_return2(the_env, v0, v1);
    }

    Real_mv brcl_floor2(Real_sp x, Real_sp y)
    {
//	const cl_env_ptr the_env = brcl_process_env();
	Real_sp v0, v1;
	NumberType ty;
	ty = brcl_t_of(y);
	if (brcl_unlikely(!BRCL_REAL_TYPE_P(y))) {
	    WRONG_TYPE_NTH_ARG(2,y,cl::_sym_Real_O);
	}
	switch(brcl_t_of(x)) {
	case number_Fixnum:
	    switch(ty) {
	    case number_Fixnum: {	/* FIX / FIX */
		Fixnum a = brcl_fixnum(x), b = brcl_fixnum(y);
		Fixnum q = a / b,  r = a % b;
		if ((r^b) < 0 && r) {	/* opposite sign and some remainder*/
		    v0 = brcl_make_fixnum(q-1);
		    v1 = brcl_make_fixnum(r+b);
		} else {
		    v0 = brcl_make_fixnum(q);
		    v1 = brcl_make_fixnum(r);
		}
		break;
	    }
	    case number_Bignum: {	/* FIX / BIG */
/* We must perform the division because there is the
 * pathological case
 *	x = MOST_NEGATIVE_FIXNUM
 *    y = - MOST_NEGATIVE_FIXNUM
 */
		Bignum_sp bx(Bignum_O::create(brcl_fixnum(x)));
//		BRCL_WITH_TEMP_BIGNUM(bx,4);
//		_brcl_big_set_fixnum(bx, brcl_fixnum(x));
		Bignum_sp by = y.as<Bignum_O>();
		v0 = _brcl_big_floor(bx, by, &v1);
		break;
	    }
	    case number_Ratio:		/* FIX / RAT */
	    {
		Ratio_sp ry = y.as<Ratio_O>();
		Real_mv mv_v0 = brcl_floor2(brcl_times(x, ry->den()).as<Real_O>(), ry->num());
		v0 = mv_v0;
		Integer_sp t1 = mv_v0.valueGet(1).as<Integer_O>();
//		v1 = brcl_make_ratio(brcl_nth_value(the_env, 1), y->den());
		v1 = brcl_make_ratio(t1,ry->den());
		break;
	    }
	    case number_SingleFloat: 	/* FIX / SF */
	    {
		float n = brcl_single_float(y);
		float p = brcl_fixnum(x) / n;
		float q = floorf(p);
		v0 = _brcl_float_to_integer(q);
		v1 = brcl_make_single_float((p - q)*n);
		break;
	    }
	    case number_DoubleFloat: {	/* FIX / DF */
		double n = brcl_double_float(y);
		double p = brcl_fixnum(x) / n;
		double q = floor(p);
		v0 = _brcl_double_to_integer(q);
		v1 = brcl_make_double_float((p - q)*n);
		break;
	    }
#ifdef BRCL_LONG_FLOAT
	    case number_LongFloat: {	/* FIX / LF */
		LongFloat n = brcl_long_float(y);
		LongFloat p = brcl_fixnum(x) / n;
		LongFloat q = floorl(p);
		v0 = _brcl_long_double_to_integer(q);
		v1 = brcl_make_long_float((p - q)*n);
		break;
	    }
#endif
	    default:
		(void)0; /* Never reached */
	    }
	    break;
	case number_Bignum:
	    switch(ty) {
	    case number_Fixnum: {	/* BIG / FIX */
//		INCOMPLETE(BF("Ensure that this produces the intended result"));
		Bignum_sp by(Bignum_O::create(brcl_fixnum(y)));
//		BRCL_WITH_TEMP_BIGNUM(by,4);
//		_brcl_big_set_fixnum(by, brcl_fixnum(y));
		v0 = _brcl_big_floor(x.as<Bignum_O>(), by, &v1);
		break;
	    }
	    case number_Bignum: {	/* BIG / BIG */
		v0 = _brcl_big_floor(x.as<Bignum_O>(), y.as<Bignum_O>(), &v1);
		break;
	    }
	    case number_Ratio:		/* BIG / RAT */
	    {
		Ratio_sp ry = y.as<Ratio_O>();
		Real_mv mv_v0 = brcl_floor2(brcl_times(x, ry->den()).as<Real_O>(), ry->num());
		v0 = mv_v0;
		Integer_sp tv1 = mv_v0.valueGet(1).as<Integer_O>();
		v1 = brcl_make_ratio(tv1, ry->den());
		break;
	    }
	    case number_SingleFloat: {	/* BIG / SF */
		float n = brcl_single_float(y);
		float p = _brcl_big_to_double(x.as<Bignum_O>()) / n;
		float q = floorf(p);
		v0 = _brcl_float_to_integer(q);
		v1 = brcl_make_single_float((p - q)*n);
		break;
	    }
	    case number_DoubleFloat: {	/* BIG / DF */
		double n = brcl_double_float(y);
		double p = _brcl_big_to_double(x.as<Bignum_O>()) / n;
		double q = floor(p);
		v0 = _brcl_double_to_integer(q);
		v1 = brcl_make_double_float((p - q)*n);
		break;
	    }
#ifdef BRCL_LONG_FLOAT
	    case number_LongFloat: {	/* BIG / LF */
		LongFloat n = brcl_long_float(y);
		LongFloat p = _brcl_big_to_double(x.as<Bignum_O>()) / n;
		LongFloat q = floorl(p);
		v0 = _brcl_long_double_to_integer(q);
		v1 = brcl_make_long_float((p - q)*n);
		break;
	    }
#endif
	    default:
		(void)0; /* Never reached */
	    }
	    break;
	case number_Ratio:
	    switch(ty) {
	    case number_Ratio:		/* RAT / RAT */
	    {
		Ratio_sp rx = x.as<Ratio_O>();
		Ratio_sp ry = y.as<Ratio_O>();
		Real_mv mv_v0 = brcl_floor2(brcl_times(rx->num(), ry->den()).as<Real_O>(),
					    brcl_times(rx->den(), ry->num()).as<Real_O>());
		v0 = mv_v0;
		Integer_sp tv1 = mv_v0.valueGet(1).as<Integer_O>();
		v1 = brcl_make_ratio(tv1, brcl_times(rx->den(), ry->den()).as<Integer_O>());
		break;
	    }
	    default:		/* RAT / ANY */
	    {
		Ratio_sp rx = x.as<Ratio_O>();
		Real_mv mv_v0 = brcl_floor2(rx->num(), brcl_times(rx->den(), y).as<Real_O>());
		v0 = mv_v0;
		Number_sp tv1 = mv_v0.valueGet(1).as<Number_O>();
		v1 = brcl_divide(tv1, rx->den()).as<Real_O>();
		break;
	    }
	    }
	case number_SingleFloat: {		/* SF / ANY */
	    float n = brcl_to_double(y);
	    float p = brcl_single_float(x)/n;
	    float q = floorf(p);
	    v0 = _brcl_float_to_integer(q);
	    /* We cannot factor these two multiplications because
	     * if we have signed zeros (1 - 1) * (-1) = -0 while
	     * 1*(-1) - 1*(-1) = +0 */
	    v1 = brcl_make_single_float(p*n - q*n);
	    break;
	}
	case number_DoubleFloat: {		/* DF / ANY */
	    double n = brcl_to_double(y);
	    double p = brcl_double_float(x)/n;
	    double q = floor(p);
	    v0 = _brcl_double_to_integer(q);
	    v1 = brcl_make_double_float(p*n - q*n);
	    break;
	}
#ifdef BRCL_LONG_FLOAT
	case number_LongFloat: {		/* LF / ANY */
	    LongFloat n = brcl_to_long_double(y);
	    LongFloat p = brcl_long_float(x)/n;
	    LongFloat q = floorl(p);
	    v0 = _brcl_long_double_to_integer(q);
	    v1 = brcl_make_long_float(p*n - q*n);
	    break;
	}
#endif
	default:
	    WRONG_TYPE_NTH_ARG(1,x,cl::_sym_Real_O);
	}
	return Values(v0,v1);
    }



    
    
#define ARGS_cl_floor "(x &optional y)"
#define DECL_cl_floor ""
#define DOCS_cl_floor "floor"
    Real_mv cl_floor(Real_sp x, Real_sp y)
    {_G();
	if (y.nilp())
	    return brcl_floor1(x);
	else
	    return brcl_floor2(x, y);
    }





    Real_mv brcl_ceiling1(Real_sp x)
    {
	Real_sp v0, v1;
	switch (brcl_t_of(x)) {
	case number_Fixnum:
	case number_Bignum:
	    v0 = x;
	    v1 = brcl_make_fixnum(0);
	    break;
	case number_Ratio: {
//	    const cl_env_ptr the_env = brcl_process_env();
	    Ratio_sp rx = x.as<Ratio_O>();
	    Real_mv mv_v = brcl_ceiling2(rx->num(), rx->den());
	    v0 = mv_v;
	    Integer_sp tv1 = mv_v.valueGet(1).as<Integer_O>();
	    v1 = brcl_make_ratio(tv1, rx->den());
	    break;
	}
	case number_SingleFloat: {
	    float d = brcl_single_float(x);
	    float y = ceilf(d);
	    v0 = _brcl_float_to_integer(y);
	    v1 = brcl_make_single_float(d - y);
	    break;
	}
	case number_DoubleFloat: {
	    double d = brcl_double_float(x);
	    double y = ceil(d);
	    v0 = _brcl_double_to_integer(y);
	    v1 = brcl_make_double_float(d - y);
	    break;
	}
#ifdef BRCL_LONG_FLOAT
	case number_LongFloat: {
	    LongFloat d = brcl_long_float(x);
	    LongFloat y = ceill(d);
	    v0 = _brcl_long_double_to_integer(y);
	    v1 = brcl_make_long_float(d - y);
	    break;
	}
#endif
	default:
	    WRONG_TYPE_NTH_ARG(1,x,cl::_sym_Real_O);
	}
	return Values(v0,v1);
    }

    Real_mv brcl_ceiling2(Real_sp x, Real_sp y)
    {
	Real_sp v0, v1;
	NumberType ty;
	ty = brcl_t_of(y);
	if (brcl_unlikely(!BRCL_REAL_TYPE_P(y))) {
	    WRONG_TYPE_NTH_ARG(2, y, cl::_sym_Real_O);
	}
	switch(brcl_t_of(x)) {
	case number_Fixnum: {
	    switch(ty) {
	    case number_Fixnum: {	/* FIX / FIX */
		Fixnum a = brcl_fixnum(x); 
		Fixnum b = brcl_fixnum(y);
		Fixnum q = a / b;
		Fixnum r = a % b;
		if ((r^b) > 0 && r) {	/* same signs and some remainder */
		    v0 = brcl_make_fixnum(q+1);
		    v1 = brcl_make_fixnum(r-b);
		} else {
		    v0 = brcl_make_fixnum(q);
		    v1 = brcl_make_fixnum(r);
		}
		break;
	    }
	    case number_Bignum: {	/* FIX / BIG */
		/* We must perform the division because there is the
		 * pathological case
		 *	x = MOST_NEGATIVE_FIXNUM
		 *    y = - MOST_NEGATIVE_FIXNUM
		 */
//		BRCL_WITH_TEMP_BIGNUM(bx,4);
		Bignum_sp bx(Bignum_O::create(brcl_fixnum(x)));
//		_brcl_big_set_fixnum(bx, brcl_fixnum(x));
		v0 = _brcl_big_ceiling(bx, y.as<Bignum_O>(), &v1);
		break;
	    }
	    case number_Ratio:		/* FIX / RAT */
	    {
		Ratio_sp ry = y.as<Ratio_O>();
		Real_mv mv_v = brcl_ceiling2(brcl_times(x, ry->den()).as<Real_O>(), ry->num());
		v0 = mv_v;
		Integer_sp tv1 = mv_v.valueGet(1).as<Integer_O>();
		v1 = brcl_make_ratio(tv1, ry->den());
		break;
	    }
	    case number_SingleFloat: {	/* FIX / SF */
		float n = brcl_single_float(y);
		float p = brcl_fixnum(x)/n;
		float q = ceilf(p);
		v0 = _brcl_float_to_integer(q);
		v1 = brcl_make_single_float(p*n - q*n);
		break;
	    }
	    case number_DoubleFloat: {	/* FIX / DF */
		double n = brcl_double_float(y);
		double p = brcl_fixnum(x)/n;
		double q = ceil(p);
		v0 = _brcl_double_to_integer(q);
		v1 = brcl_make_double_float(p*n - q*n);
		break;
	    }
#ifdef BRCL_LONG_FLOAT
	    case number_LongFloat: {	/* FIX / LF */
		LongFloat n = brcl_long_float(y);
		LongFloat p = brcl_fixnum(x)/n;
		LongFloat q = ceill(p);
		v0 = _brcl_long_double_to_integer(q);
		v1 = brcl_make_long_float(p*n - q*n);
		break;
	    }
#endif
	    default:
		(void)0; /*Never reached */
	    }
	    break;
	}
	case number_Bignum: {
	    switch(brcl_t_of(y)) {
	    case number_Fixnum: {	/* BIG / FIX */
//		BRCL_WITH_TEMP_BIGNUM(by,4);
		Bignum_sp by(Bignum_O::create(brcl_fixnum(y)));
//		_brcl_big_set_fixnum(by, brcl_fixnum(y));
		v0 = _brcl_big_ceiling(x.as<Bignum_O>(), by, &v1);
		break;
	    }
	    case number_Bignum: {	/* BIG / BIG */
		v0 = _brcl_big_ceiling(x.as<Bignum_O>(), y.as<Bignum_O>(), &v1);
		break;
	    }
	    case number_Ratio:		/* BIG / RAT */
	    {
		Ratio_sp ry = y.as<Ratio_O>();
		Real_mv mv_v = brcl_ceiling2(brcl_times(x, ry->den()).as<Real_O>(), ry->num());
		v0 = mv_v;
		Integer_sp tv1 = mv_v.valueGet(1).as<Integer_O>();
		v1 = brcl_make_ratio(tv1, ry->den());
		break;
	    }
	    case number_SingleFloat: {	/* BIG / SF */
		float n = brcl_single_float(y);
		float p = _brcl_big_to_double(x.as<Bignum_O>())/n;
		float q = ceilf(p);
		v0 = _brcl_float_to_integer(q);
		v1 = brcl_make_single_float(p*n - q*n);
		break;
	    }
	    case number_DoubleFloat: {	/* BIG / DF */
		double n = brcl_double_float(y);
		double p = _brcl_big_to_double(x.as<Bignum_O>())/n;
		double q = ceil(p);
		v0 = _brcl_double_to_integer(q);
		v1 = brcl_make_double_float(p*n - q*n);
		break;
	    }
#ifdef BRCL_LONG_FLOAT
	    case number_LongFloat: {	/* BIG / LF */
		LongFloat n = brcl_long_float(y);
		LongFloat p = _brcl_big_to_double(x.as<Bignum_O>())/n;
		LongFloat q = ceill(p);
		v0 = _brcl_long_double_to_integer(q);
		v1 = brcl_make_long_float(p*n - q*n);
		break;
	    }
#endif
	    default:
		(void)0; /*Never reached */
	    }
	    break;
	}
	case number_Ratio: {
	    switch(brcl_t_of(y)) {
	    case number_Ratio:		/* RAT / RAT */
	    {
		Ratio_sp rx = x.as<Ratio_O>();
		Ratio_sp ry = y.as<Ratio_O>();
		Real_mv mv_v = brcl_ceiling2(brcl_times(rx->num(), ry->den()).as<Real_O>(),
					     brcl_times(rx->den(), ry->num()).as<Real_O>());
		v0 = mv_v;
		Integer_sp tv1 = mv_v.valueGet(1).as<Integer_O>();
		v1 = brcl_make_ratio(tv1, brcl_times(rx->den(), ry->den()).as<Integer_O>());
		break;
	    }
	    default:		/* RAT / ANY */
	    {
		Ratio_sp rx = x.as<Ratio_O>();
		Real_mv mv_v = brcl_ceiling2(rx->num(), brcl_times(rx->den(), y).as<Real_O>());
		v0 = mv_v;
		Number_sp tv1 = mv_v.valueGet(1).as<Number_O>();
		v1 = brcl_divide(tv1, rx->den()).as<Real_O>();
	    }
	    }
	    break;
	}
	case number_SingleFloat: {		/* SF / ANY */
	    float n = brcl_to_double(y);
	    float p = brcl_single_float(x)/n;
	    float q = ceilf(p);
	    v0 = _brcl_float_to_integer(q);
	    v1 = brcl_make_single_float(p*n - q*n);
	    break;
	}
	case number_DoubleFloat: {		/* DF / ANY */
	    double n = brcl_to_double(y);
	    double p = brcl_double_float(x)/n;
	    double q = ceil(p);
	    v0 = _brcl_double_to_integer(q);
	    v1 = brcl_make_double_float(p*n - q*n);
	    break;
	}
#ifdef BRCL_LONG_FLOAT
	case number_LongFloat: {		/* LF / ANY */
	    LongFloat n = brcl_to_long_double(y);
	    LongFloat p = brcl_long_float(x)/n;
	    LongFloat q = ceill(p);
	    v0 = _brcl_long_double_to_integer(q);
	    v1 = brcl_make_long_float(p*n - q*n);
	    break;
	}
#endif
	default:
	    WRONG_TYPE_NTH_ARG( 1, x, cl::_sym_Real_O);
	}
	brcl_return2(the_env, v0, v1);
    }



    
    
#define ARGS_cl_ceiling "(x &optional y)"
#define DECL_cl_ceiling ""
#define DOCS_cl_ceiling "ceiling"
    Real_mv cl_ceiling(Real_sp x, Real_sp y)
    {_G();
	if (y.nilp())
	    return brcl_ceiling1(x);
	else
	    return brcl_ceiling2(x, y);
    }






    Real_mv brcl_truncate1(Real_sp x)
    {
	Real_sp v0, v1;
	switch (brcl_t_of(x)) {
	case number_Fixnum:
	case number_Bignum:
	    v0 = x;
	    v1 = brcl_make_fixnum(0);
	    break;
	case number_Ratio: {
	    Ratio_sp rx = x.as<Ratio_O>();
	    Real_mv mv_v = brcl_truncate2(rx->num(), rx->den());
	    v0 = mv_v;
	    Integer_sp tv1 = mv_v.valueGet(1).as<Integer_O>();
	    v1 = brcl_make_ratio(tv1, rx->den());
	    break;
	}
	case number_SingleFloat: {
	    float d = brcl_single_float(x);
	    float y = d > 0? floorf(d) : ceilf(d);
	    v0 = _brcl_float_to_integer(y);
	    v1 = brcl_make_single_float(d - y);
	    break;
	}
	case number_DoubleFloat: {
	    double d = brcl_double_float(x);
	    double y = d > 0? floor(d) : ceil(d);
	    v0 = _brcl_double_to_integer(y);
	    v1 = brcl_make_double_float(d - y);
	    break;
	}
#ifdef BRCL_LONG_FLOAT
	case number_LongFloat: {
	    LongFloat d = brcl_long_float(x);
	    LongFloat y = d > 0? floorl(d) : ceill(d);
	    v0 = _brcl_long_double_to_integer(y);
	    v1 = brcl_make_long_float(d - y);
	    break;
	}
#endif
	default:
	    WRONG_TYPE_NTH_ARG(1,x,cl::_sym_Real_O);
	}
	brcl_return2(the_env, v0, v1);
    }

    Real_mv brcl_truncate2(Real_sp x, Real_sp y)
    {
	if (brcl_plusp(x) != brcl_plusp(y))
	    return brcl_ceiling2(x, y);
	else
	    return brcl_floor2(x, y);
    }



    
    
#define ARGS_cl_truncate "(x &optional y)"
#define DECL_cl_truncate ""
#define DOCS_cl_truncate "truncate"
    Real_mv cl_truncate(Real_sp x, Real_sp y)
    {_G();
	if (y.nilp() == 1)
	    return brcl_truncate1(x);
	else
	    return brcl_truncate2(x, y);
    }

    static double round_double(double d)
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

#ifdef BRCL_LONG_FLOAT
    static LongFloat round_long_double(LongFloat d)
    {
	if (d >= 0) {
	    LongFloat q = floorl(d += 0.5);
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

    Real_mv brcl_round1(Real_sp x)
    {
	Real_sp v0, v1;
	switch (brcl_t_of(x)) {
	case number_Fixnum:
	case number_Bignum:
	    v0 = x;
	    v1 = brcl_make_fixnum(0);
	    break;
	case number_Ratio:
	{
	    Ratio_sp rx = x.as<Ratio_O>();
	    Real_mv mv_v = brcl_round2(rx->num(), rx->den());
	    v0 = mv_v;
	    Integer_sp tv1 = mv_v.valueGet(1).as<Integer_O>();
	    v1 = brcl_make_ratio(tv1, rx->den());
	    break;
	}
	case number_SingleFloat: {
	    float d = brcl_single_float(x);
	    float q = round_double(d);
	    v0 = _brcl_float_to_integer(q);
	    v1 = brcl_make_single_float(d - q);
	    break;
	}
	case number_DoubleFloat: {
	    double d = brcl_double_float(x);
	    double q = round_double(d);
	    v0 = _brcl_double_to_integer(q);
	    v1 = brcl_make_double_float(d - q);
	    break;
	}
#ifdef BRCL_LONG_FLOAT
	case number_LongFloat: {
	    LongFloat d = brcl_long_float(x);
	    LongFloat q = round_long_double(d);
	    v0 = _brcl_long_double_to_integer(q);
	    v1 = brcl_make_long_float(d - q);
	    break;
	}
#endif
	default:
	    WRONG_TYPE_NTH_ARG(1,x,cl::_sym_Real_O);
	}
	brcl_return2(the_env, v0, v1);
    }

    Real_mv brcl_round2(Real_sp x, Real_sp y)
    {
	Real_sp v0, v1;
	Real_sp q;

	q = brcl_divide(x, y).as<Real_O>();
	switch (brcl_t_of(q)) {
	case number_Fixnum:
	case number_Bignum:
	    v0 = q;
	    v1 = brcl_make_fixnum(0);
	    break;
	case number_Ratio: {
	    Ratio_sp rq = q.as<Ratio_O>();
	    Integer_sp q1 = brcl_integer_divide(rq->num(), rq->den());
	    Real_sp r = brcl_minus(q, q1).as<Real_O>();
	    if (brcl_minusp(r)) {
		int c = brcl_number_compare(_lisp->minusHalf(), r);
		if (c > 0 || (c == 0 && brcl_oddp(q1))) {
		    q1 = brcl_one_minus(q1).as<Integer_O>();
		}
	    } else {
		int c = brcl_number_compare(r, _lisp->plusHalf());
		if (c > 0 || (c == 0 && brcl_oddp(q1))) {
		    q1 = brcl_one_plus(q1).as<Integer_O>();
		}
	    }
	    v0 = q1;
	    v1 = number_remainder(x, y, q1).as<Real_O>();
	    break;
	}
	default:
	    v0 = q = brcl_round1(q).as<Integer_O>();
	    v1 = number_remainder(x, y, q).as<Real_O>();
	}
	brcl_return2(the_env, v0, v1);
    }



    
    
#define ARGS_cl_round "(x &optional y)"
#define DECL_cl_round ""
#define DOCS_cl_round "round"
    Number_mv cl_round(Real_sp x, Real_sp y)
    {_G();
	if (y.nilp())
	    return brcl_round1(x);
	else
	    return brcl_round2(x, y);
    }



    
    
#define ARGS_cl_mod "(x y)"
#define DECL_cl_mod ""
#define DOCS_cl_mod "mod"
    Real_sp cl_mod(Real_sp x, Real_sp y)
    {
	/* INV: #'floor always outputs two values */
	Real_mv mv_v = cl_floor(x, y);
	return mv_v.valueGet(1).as<Real_O>();
    }





    
    
#define ARGS_cl_rem "(x y)"
#define DECL_cl_rem ""
#define DOCS_cl_rem "rem"
    Real_sp cl_rem(Real_sp x, Real_sp y)
    {_G();
	Real_mv mv_v = cl_truncate(x, y);
	return mv_v.valueGet(1).as<Real_O>();
    }



    
    
#define ARGS_cl_decodeFloat "(x)"
#define DECL_cl_decodeFloat ""
#define DOCS_cl_decodeFloat "decodeFloat"
    Number_mv cl_decodeFloat(Float_sp x)
    {_G();
	int e=0, s=0;
	NumberType tx = brcl_t_of(x);
	float f;
	switch (tx) {
	case number_SingleFloat: {
	    f = brcl_single_float(x);
	    if (f >= 0.0) {
		s = 1;
	    } else {
		f = -f;
		s = 0;
	    }
	    f = frexpf(f, &e);
	    x = brcl_make_single_float(f);
	    break;
	}
	case number_DoubleFloat: {
	    double d = brcl_double_float(x);
	    if (d >= 0.0) {
		s = 1;
	    } else {
		d = -d;
		s = 0;
	    }
	    d = frexp(d, &e);
	    x = brcl_make_double_float(d);
	    break;
	}
#ifdef BRCL_LONG_FLOAT
	case number_LongFloat: {
	    LongFloat d = brcl_long_float(x);
	    if (d >= 0.0)
		s = 1;
	    else {
		d = -d;
		s = 0;
	    }
	    d = frexpl(d, &e);
	    x = brcl_make_long_float(d);
	    break;
	}
#endif
	default:
	    WRONG_TYPE_NTH_ARG(1,x,cl::_sym_float);
	}
	return Values(x, brcl_make_fixnum(e), brcl_make_single_float(s));
    }



    
    
#define ARGS_cl_scaleFloat "(x y)"
#define DECL_cl_scaleFloat ""
#define DOCS_cl_scaleFloat "scaleFloat"
    Number_sp cl_scaleFloat(Number_sp x, Number_sp y)
    {_G();
	Fixnum k;

	if (BRCL_FIXNUMP(y)) {
	    k = brcl_fixnum(y);
	} else {
	    WRONG_TYPE_NTH_ARG(2,y,cl::_sym_Fixnum_O);
	}
	switch (brcl_t_of(x)) {
	case number_SingleFloat:
	    x = brcl_make_single_float(ldexpf(brcl_single_float(x), k));
	    break;
	case number_DoubleFloat:
	    x = brcl_make_double_float(ldexp(brcl_double_float(x), k));
	    break;
#ifdef BRCL_LONG_FLOAT
	case number_LongFloat:
	    x = brcl_make_long_float(ldexpl(brcl_long_float(x), k));
	    break;
#endif
	default:
	    WRONG_TYPE_NTH_ARG(1,x,cl::_sym_float);
	}
	return x;
    }

    Integer_sp cl_float_radix(Float_sp x)
    {
	return brcl_make_fixnum(FLT_RADIX);
    }

    int
    brcl_signbit(Number_sp x)
    {
	switch (brcl_t_of(x)) {
	case number_SingleFloat:
	    return signbit(brcl_single_float(x));
	case number_DoubleFloat:
	    return signbit(brcl_double_float(x));
#ifdef BRCL_LONG_FLOAT
	case number_LongFloat:
	    return signbit(brcl_long_float(x));
#endif
	default:
	    WRONG_TYPE_NTH_ARG(1,x,cl::_sym_float);
	}
	SIMPLE_ERROR(BF("Illegal argument for brcl_signbit: %s") % _rep_(x));
    }


    
    
#define ARGS_cl_floatSign "(x &optional (y x yp))"
#define DECL_cl_floatSign ""
#define DOCS_cl_floatSign "floatSign"
    Float_sp cl_floatSign(Float_sp x, Float_sp y, T_sp yp)
    {_G();
      int negativep;
      if (yp.nilp()) {
	  y = cl_float( brcl_make_fixnum(1), x);
      }
      negativep = brcl_signbit(x);
      switch (brcl_t_of(y)) {
      case number_SingleFloat: {
	  float f = brcl_single_float(y);
	  if (signbit(f) != negativep) y = brcl_make_single_float(-f);
	  break;
      }
      case number_DoubleFloat: {
	  double f = brcl_double_float(y);
	  if (signbit(f) != negativep) y = brcl_make_double_float(-f);
	  break;
      }
#ifdef BRCL_LONG_FLOAT
      case number_LongFloat: {
	  LongFloat f = brcl_long_float(y);
	  if (signbit(f) != negativep) y = brcl_make_long_float(-f);
	  break;
      }
#endif
      default:
	  WRONG_TYPE_NTH_ARG(2,y,cl::_sym_float);
      }
      return y;
    }


    
    
    
#define ARGS_cl_floatDigits "(x)"
#define DECL_cl_floatDigits ""
#define DOCS_cl_floatDigits "floatDigits"
    Integer_sp cl_floatDigits(Float_sp x)
    {_G();
	Integer_sp ix(_Nil<Integer_O>());
	switch (brcl_t_of(x)) {
	case number_SingleFloat:
	    ix = brcl_make_fixnum(FLT_MANT_DIG);
	    break;
	case number_DoubleFloat:
	    ix = brcl_make_fixnum(DBL_MANT_DIG);
	    break;
#ifdef BRCL_LONG_FLOAT
	case number_LongFloat:
	    ix = brcl_make_fixnum(LDBL_MANT_DIG);
	    break;
#endif
	default:
	    WRONG_TYPE_NTH_ARG(1,x,cl::_sym_float);
	}
	return ix;
    }

    
    
    
#define ARGS_cl_floatPrecision "(Float_sp x)"
#define DECL_cl_floatPrecision ""
#define DOCS_cl_floatPrecision "floatPrecision"
    Integer_sp cl_floatPrecision(Float_sp x)
    {_G();
	int precision = 0;
	switch (brcl_t_of(x)) {
	case number_SingleFloat: {
	    float f = brcl_single_float(x);
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
	case number_DoubleFloat: {
	    double f = brcl_double_float(x);
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
#ifdef BRCL_LONG_FLOAT
	case number_LongFloat: {
	    LongFloat f = brcl_long_float(x);
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
	    WRONG_TYPE_NTH_ARG(1,x,cl::_sym_float);
	}
	return brcl_make_fixnum(precision);
    }


    
    
    
#define ARGS_cl_integerDecodeFloat "(x)"
#define DECL_cl_integerDecodeFloat ""
#define DOCS_cl_integerDecodeFloat "integerDecodeFloat"
    Real_mv cl_integerDecodeFloat(Float_sp x)
    {_G();
	int e = 0, s = 1;
	Real_sp rx(_Nil<Real_O>());
	switch (brcl_t_of(x)) {
#ifdef BRCL_LONG_FLOAT
	case number_LongFloat: {
	    LongFloat d = brcl_long_float(x);
	    if (signbit(d)) {
		s = -1;
		d = -d;
	    }
	    if (d == 0.0) {
		e = 0;
		rx = brcl_make_fixnum(0);
	    } else {
		d = frexpl(d, &e);
		rx = _brcl_long_double_to_integer(ldexpl(d, LDBL_MANT_DIG));
		e -= LDBL_MANT_DIG;
	    }
	    break;
	}
#endif
	case number_DoubleFloat: {
	    double d = brcl_double_float(x);
	    if (signbit(d)) {
		s = -1;
		d = -d;
	    }
	    if (d == 0.0) {
		e = 0;
		rx = brcl_make_fixnum(0);
	    } else {
		d = frexp(d, &e);
		rx = _brcl_double_to_integer(ldexp(d, DBL_MANT_DIG));
		e -= DBL_MANT_DIG;
	    }
	    break;
	}
	case number_SingleFloat: {
	    float d = brcl_single_float(x);
	    if (signbit(d)) {
		s = -1;
		d = -d;
	    }
	    if (d == 0.0) {
		e = 0;
		rx = brcl_make_fixnum(0);
	    } else {
		d = frexpf(d, &e);
		rx = _brcl_double_to_integer(ldexp(d, FLT_MANT_DIG));
		e -= FLT_MANT_DIG;
	    }
	    break;
	}
	default:
	    WRONG_TYPE_NTH_ARG(1,x,cl::_sym_float);
	}
	ASSERT(rx.notnilp());
	return Values( rx, brcl_make_fixnum(e), brcl_make_fixnum(s));
    }



    
    
#define ARGS_cl_complex "(r &optional (i 0))"
#define DECL_cl_complex ""
#define DOCS_cl_complex "complex"
    Complex_sp cl_complex(Real_sp r, Real_sp i)
    {_G();
	return Complex_O::create(r,i);
    }



    
    
#define ARGS_cl_realpart "(x)"
#define DECL_cl_realpart ""
#define DOCS_cl_realpart "realpart"
    Real_sp cl_realpart(Number_sp x)
    {_G();
	switch (brcl_t_of(x)) {
	case number_Fixnum:
	case number_Bignum:
	case number_Ratio:
	case number_SingleFloat:
	case number_DoubleFloat:
#ifdef BRCL_LONG_FLOAT
	case number_LongFloat:
#endif
	    break;
	case number_Complex: {
	    x = x.as<Complex_O>()->real();
	    break;
	}
	default:
	    WRONG_TYPE_NTH_ARG(1,x,cl::_sym_Number_O);
	}
	return x.as<Real_O>();
    }




    
    
#define ARGS_cl_imagpart "(x)"
#define DECL_cl_imagpart ""
#define DOCS_cl_imagpart "imagpart"
    Real_sp cl_imagpart(Number_sp x)
    {_G();
	switch (brcl_t_of(x)) {
	case number_Fixnum:
	case number_Bignum:
	case number_Ratio:
	    x = brcl_make_fixnum(0);
	    break;
	case number_SingleFloat:
	    if (signbit(brcl_single_float(x)))
		x = _lisp->singleFloatMinusZero();
	    else
		x = _lisp->singleFloatPlusZero();
	    break;
	case number_DoubleFloat:
	    if (signbit(brcl_double_float(x)))
		x = _lisp->doubleFloatMinusZero();
	    else
		x = _lisp->doubleFloatPlusZero();
	    break;
#ifdef BRCL_LONG_FLOAT
	case number_LongFloat:
	    if (signbit(brcl_long_float(x)))
		x = _lisp->longFloatMinusZero();
	    else
		x = _lisp->longFloatPlusZero();
	    break;
#endif
	case number_Complex:
	    x = x.as<Complex_O>()->imaginary();
	    break;
	default:
	    WRONG_TYPE_NTH_ARG(1,x,cl::_sym_Number_O);
	}
	return x.as<Real_O>();
    }



    void initialize_num_co()
    {
	SYMBOL_EXPORT_SC_(ClPkg,float);
	ClDefun(float);
	SYMBOL_EXPORT_SC_(ClPkg,numerator);
	ClDefun(numerator);
	SYMBOL_EXPORT_SC_(ClPkg,denomenator);
	ClDefun(denomenator);
	SYMBOL_EXPORT_SC_(ClPkg,floor);
	ClDefun(floor);
	SYMBOL_EXPORT_SC_(ClPkg,ceiling);
	ClDefun(ceiling);
	SYMBOL_EXPORT_SC_(ClPkg,truncate);
	ClDefun(truncate);
	SYMBOL_EXPORT_SC_(ClPkg,round);
	ClDefun(round);
	SYMBOL_EXPORT_SC_(ClPkg,mod);
	ClDefun(mod);
	SYMBOL_EXPORT_SC_(ClPkg,rem);
	ClDefun(rem);
	SYMBOL_EXPORT_SC_(ClPkg,decodeFloat);
	ClDefun(decodeFloat);
	SYMBOL_EXPORT_SC_(ClPkg,scaleFloat);
	ClDefun(scaleFloat);	
	SYMBOL_EXPORT_SC_(ClPkg,floatSign);
	ClDefun(floatSign);
	SYMBOL_EXPORT_SC_(ClPkg,floatDigits);
	ClDefun(floatDigits);
	SYMBOL_EXPORT_SC_(ClPkg,floatPrecision);
	ClDefun(floatPrecision);
	SYMBOL_EXPORT_SC_(ClPkg,integerDecodeFloat);
	ClDefun(integerDecodeFloat);
	SYMBOL_EXPORT_SC_(ClPkg,complex);
	ClDefun(complex);
	SYMBOL_EXPORT_SC_(ClPkg,realpart);
	ClDefun(realpart);
	SYMBOL_EXPORT_SC_(ClPkg,imagpart);
	ClDefun(imagpart);

    };
};

