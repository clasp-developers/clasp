/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    num_pred.c  -- Predicates on numbers.
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

int
ecl_oddp(cl_object x)
{
	if (ECL_FIXNUMP(x))
		return ecl_fixnum(x) & 1;
	unlikely_if (!ECL_BIGNUMP(x))
                FEwrong_type_only_arg(@[oddp], x, @[integer]);
        return _ecl_big_odd_p(x);
}

int
ecl_evenp(cl_object x)
{
	if (ECL_FIXNUMP(x))
		return ~ecl_fixnum(x) & 1;
	unlikely_if (!ECL_BIGNUMP(x))
                FEwrong_type_only_arg(@[evenp], x, @[integer]);
        return _ecl_big_even_p(x);
}

cl_object
cl_oddp(cl_object x)
{	/* INV: ecl_oddp() checks type */
	@(return (ecl_oddp(x) ? ECL_T : ECL_NIL))
}

cl_object
cl_evenp(cl_object x)
{	/* INV: ecl_evenp() checks_type */
	@(return (ecl_evenp(x) ? ECL_T : ECL_NIL))
}

cl_object
si_float_nan_p(cl_object x)
{
	@(return (ecl_float_nan_p(x)? ECL_T : ECL_NIL))
}

cl_object
si_float_infinity_p(cl_object x)
{
	@(return (ecl_float_infinity_p(x)? ECL_T : ECL_NIL))
}

bool
ecl_float_nan_p(cl_object x)
{
	return !ecl_number_equalp(x,x);
}

bool
ecl_float_infinity_p(cl_object x)
{
	switch (ecl_t_of(x)) {
	case t_singlefloat:
		return !isfinite(ecl_single_float(x));
	case t_doublefloat:
		return !isfinite(ecl_double_float(x));
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		return !isfinite(ecl_long_float(x));
#endif
	default:
		return 0;
	}
}
