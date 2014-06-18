/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    truncate.d  -- Implementation of CL:TRUNCATE
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
# define ceilf ceil
#endif
#include <ecl/internal.h>

cl_object
ecl_truncate1(cl_object x)
{
	cl_object v0, v1;
	switch (ecl_t_of(x)) {
	case t_fixnum:
	case t_bignum:
		v0 = x;
		v1 = ecl_make_fixnum(0);
		break;
	case t_ratio:
		if (ecl_plusp(x->ratio.num))
			return ecl_floor1(x);
		else
			return ecl_ceiling1(x);
	case t_singlefloat: {
		float d = ecl_single_float(x);
		float y = d > 0? floorf(d) : ceilf(d);
		v0 = _ecl_float_to_integer(y);
		v1 = ecl_make_single_float(d - y);
		break;
	}
	case t_doublefloat: {
		double d = ecl_double_float(x);
		double y = d > 0? floor(d) : ceil(d);
		v0 = _ecl_double_to_integer(y);
		v1 = ecl_make_double_float(d - y);
		break;
	}
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {
		long double d = ecl_long_float(x);
		long double y = d > 0? floorl(d) : ceill(d);
		v0 = _ecl_long_double_to_integer(y);
		v1 = ecl_make_long_float(d - y);
		break;
	}
#endif
	default:
                FEwrong_type_nth_arg(@[truncate],1,x,@[real]);
	}
	{
		const cl_env_ptr the_env = ecl_process_env();
		ecl_return2(the_env, v0, v1);
	}
}

cl_object
ecl_truncate2(cl_object x, cl_object y)
{
	if (ecl_plusp(x) != ecl_plusp(y))
		return ecl_ceiling2(x, y);
	else
		return ecl_floor2(x, y);
}

@(defun truncate (x &optional (y OBJNULL))
@
	if (narg == 1)
		return ecl_truncate1(x);
	else
		return ecl_truncate2(x, y);
@)
