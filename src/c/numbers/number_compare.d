/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    number_compare.c  -- number comparison.
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
#include <ecl/impl/math_dispatch2.h>
#include "numbers/float_fix_compare.d"

/*
	The value of ecl_number_compare(x, y) is

		-1	if	x < y
		0	if	x = y
		1	if	x > y.

	If x or y is not real, it fails.
*/
int
ecl_number_compare(cl_object x, cl_object y)
{
	cl_fixnum ix, iy;
	double dx, dy;
#ifdef ECL_LONG_FLOAT
	long double ldx, ldy;
#endif
	cl_type ty;
 BEGIN:
	ty = ecl_t_of(y);
	switch (ecl_t_of(x)) {
	case t_fixnum:
	  	ix = ecl_fixnum(x);
		switch (ty) {
		case t_fixnum:
		  	iy = ecl_fixnum(y);
			if (ix < iy)
				return(-1);
			else return(ix != iy);
		case t_bignum:
			/* INV: (= x y) can't be zero since fixnum != bignum */
			return _ecl_big_sign(y) < 0? 1 : -1;
		case t_ratio:
			x = ecl_times(x, y->ratio.den);
			y = y->ratio.num;
			return(ecl_number_compare(x, y));
		case t_singlefloat:
			return double_fix_compare(ix, ecl_single_float(y));
		case t_doublefloat:
			return double_fix_compare(ix, ecl_double_float(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return long_double_fix_compare(ix, ecl_long_float(y));
#endif
		default:
			FEwrong_type_nth_arg(@[<], 2, y, @[real]);
		}
	case t_bignum:
		switch (ty) {
		case t_fixnum:
			return _ecl_big_sign(x) < 0 ? -1 : 1;
		case t_bignum:
			return(_ecl_big_compare(x, y));
		case t_ratio:
			x = ecl_times(x, y->ratio.den);
			y = y->ratio.num;
			return(ecl_number_compare(x, y));
		case t_singlefloat:
		case t_doublefloat:
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
#endif
			y = cl_rational(y);
			goto BEGIN;
		default:
			FEwrong_type_nth_arg(@[<], 2, y, @[real]);
		}
	case t_ratio:
		switch (ty) {
		case t_fixnum:
		case t_bignum:
			y = ecl_times(y, x->ratio.den);
			x = x->ratio.num;
			return(ecl_number_compare(x, y));
		case t_ratio:
			return(ecl_number_compare(ecl_times(x->ratio.num,
							   y->ratio.den),
					      ecl_times(y->ratio.num,
							   x->ratio.den)));
		case t_singlefloat:
		case t_doublefloat:
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
#endif
			y = cl_rational(y);
			goto BEGIN;
		default:
			FEwrong_type_nth_arg(@[<], 2, y, @[real]);
		}
	case t_singlefloat:
		dx = (double)(ecl_single_float(x));
		goto DOUBLEFLOAT0;
	case t_doublefloat:
		dx = ecl_double_float(x);
	DOUBLEFLOAT0:
		switch (ty) {
		case t_fixnum:
			return -double_fix_compare(ecl_fixnum(y), dx);
		case t_bignum:
		case t_ratio:
			x = cl_rational(x);
			goto BEGIN;
		case t_singlefloat:
			dy = (double)(ecl_single_float(y));
			break;
		case t_doublefloat:
			dy = ecl_double_float(y);
			break;
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			ldx = dx;
			ldy = ecl_long_float(y);
			goto LONGFLOAT;
#endif
		default:
			FEwrong_type_nth_arg(@[<], 2, y, @[real]);
		}
	DOUBLEFLOAT:
		if (dx == dy)
			return(0);
		else if (dx < dy)
			return(-1);
		else
			return(1);
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		ldx = ecl_long_float(x);
		switch (ty) {
		case t_fixnum:
			return -long_double_fix_compare(ecl_fixnum(y), ldx);
		case t_bignum:
		case t_ratio:
			x = cl_rational(x);
			goto BEGIN;
		case t_singlefloat:
			ldy = ecl_single_float(y);
			break;
		case t_doublefloat:
			ldy = ecl_double_float(y);
			break;
		case t_longfloat:
			ldy = ecl_long_float(y);
			break;
		default:
			FEwrong_type_nth_arg(@[<], 2, y, @[real]);
		}
	LONGFLOAT:
		if (ldx == ldy)
			return 0;
		else if (ldx < ldy)
			return -1;
		else
			return 1;
		break;
#endif
	default:
		FEwrong_type_nth_arg(@[<], 1, x, @[real]);
	}
}

static cl_object
monotonic(int s, int t, int narg, ecl_va_list nums)
{
	cl_object c, d;

	if (narg == 0)
		FEwrong_num_arguments_anonym();
	/* INV: type check occurs in ecl_number_compare() */
	for (c = ecl_va_arg(nums); --narg; c = d) {
		d = ecl_va_arg(nums);
		if (s*ecl_number_compare(d, c) < t)
			return1(ECL_NIL);
	}
	return1(ECL_T);
}

#define MONOTONIC(i, j) (cl_narg narg, ...) \
{ ecl_va_list nums; ecl_va_start(nums, narg, narg, 0); \
  return monotonic(i, j, narg, nums); }

cl_object @<= MONOTONIC( 1, 0)
cl_object @>= MONOTONIC(-1, 0)
cl_object @<  MONOTONIC( 1, 1)
cl_object @>  MONOTONIC(-1, 1)

