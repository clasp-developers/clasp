/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    divde.d  -- Implementation of CL:/
*/
/*
    Copyright (c) 2010, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <ecl/ecl.h>
#include <ecl/impl/math_dispatch2.h>

@(defun / (num &rest nums)
@
	/* INV: type check is in ecl_divide() */
	if (narg == 0)
		FEwrong_num_arguments(@[/]);
	if (narg == 1)
		@(return ecl_divide(ecl_make_fixnum(1), num))
	while (--narg)
		num = ecl_divide(num, ecl_va_arg(nums));
	@(return num)
@)

#ifdef MATH_DISPATCH2_BEGIN

static cl_object
complex_divide(cl_object ar, cl_object ai, cl_object br, cl_object bi)
{
        /* #C(z1 z2) = #C(xr xi) * #C(yr -yi) */
        cl_object z1 = ecl_plus(ecl_times(ar, br), ecl_times(ai, bi));
        cl_object z2 = ecl_minus(ecl_times(ai, br), ecl_times(ar, bi));
        cl_object absB = ecl_plus(ecl_times(br, br), ecl_times(bi, bi));
        return ecl_make_complex(ecl_divide(z1, absB), ecl_divide(z2, absB));
}

cl_object
ecl_divide(cl_object x, cl_object y)
{
MATH_DISPATCH2_BEGIN(x,y)
{
        CASE_FIXNUM_FIXNUM;
        CASE_BIGNUM_FIXNUM {
                if (y == ecl_make_fixnum(0))
                        FEdivision_by_zero(x, y);
        }
        CASE_FIXNUM_BIGNUM;
        CASE_BIGNUM_BIGNUM {
                return ecl_make_ratio(x, y);
        }
        CASE_FIXNUM_RATIO;
        CASE_BIGNUM_RATIO {
                return ecl_make_ratio(ecl_times(x, y->ratio.den),
                                      y->ratio.num);
        }
        CASE_FIXNUM_SINGLE_FLOAT {
                return ecl_make_single_float(ecl_fixnum(x) / ecl_single_float(y));
        }
        CASE_FIXNUM_DOUBLE_FLOAT {
                return ecl_make_double_float(ecl_fixnum(x) / ecl_double_float(y));
        }
        CASE_BIGNUM_SINGLE_FLOAT;
        CASE_RATIO_SINGLE_FLOAT {
                return ecl_make_single_float(ecl_to_float(x) / ecl_single_float(y));
        }
        CASE_BIGNUM_DOUBLE_FLOAT;
        CASE_RATIO_DOUBLE_FLOAT {
                return ecl_make_double_float(ecl_to_double(x) / ecl_double_float(y));
        }
        CASE_RATIO_FIXNUM {
                if (y == ecl_make_fixnum(0)) {
                        FEdivision_by_zero(x,y);
                }
        }
        CASE_RATIO_BIGNUM {
                cl_object z = ecl_times(x->ratio.den, y);
                return ecl_make_ratio(x->ratio.num, z);
        }
        CASE_RATIO_RATIO {
                cl_object num = ecl_times(x->ratio.num,y->ratio.den);
                cl_object den = ecl_times(x->ratio.den,y->ratio.num);
                return ecl_make_ratio(num, den);
        }
        CASE_SINGLE_FLOAT_FIXNUM {
                return ecl_make_single_float(ecl_single_float(x) / ecl_fixnum(y));
        }
        CASE_SINGLE_FLOAT_BIGNUM;
        CASE_SINGLE_FLOAT_RATIO {
                return ecl_make_single_float(ecl_single_float(x) / ecl_to_float(y));
        }
        CASE_SINGLE_FLOAT_SINGLE_FLOAT {
                return ecl_make_single_float(ecl_single_float(x) / ecl_single_float(y));
        }
        CASE_SINGLE_FLOAT_DOUBLE_FLOAT {
                return ecl_make_double_float(ecl_single_float(x) / ecl_double_float(y));
        }
        CASE_DOUBLE_FLOAT_FIXNUM {
                return ecl_make_double_float(ecl_double_float(x) / ecl_fixnum(y));
        }
        CASE_DOUBLE_FLOAT_BIGNUM;
        CASE_DOUBLE_FLOAT_RATIO {
                return ecl_make_double_float(ecl_double_float(x) / ecl_to_double(y));
        }
        CASE_DOUBLE_FLOAT_SINGLE_FLOAT {
                return ecl_make_double_float(ecl_double_float(x) / ecl_single_float(y));
        }
        CASE_DOUBLE_FLOAT_DOUBLE_FLOAT {
                return ecl_make_double_float(ecl_double_float(x) / ecl_double_float(y));
        }
#ifdef ECL_LONG_FLOAT
        CASE_FIXNUM_LONG_FLOAT {
                return ecl_make_long_float(ecl_fixnum(x) / ecl_long_float(y));
        }
        CASE_BIGNUM_LONG_FLOAT;
        CASE_RATIO_LONG_FLOAT {
                return ecl_make_long_float(ecl_to_long_double(x) / ecl_long_float(y));
        }
        CASE_SINGLE_FLOAT_LONG_FLOAT {
                return ecl_make_long_float(ecl_single_float(x) / ecl_long_float(y));
        }
        CASE_DOUBLE_FLOAT_LONG_FLOAT {
                return ecl_make_long_float(ecl_double_float(x) / ecl_long_float(y));
        }
        CASE_LONG_FLOAT_FIXNUM {
                return ecl_make_long_float(ecl_long_float(x) / ecl_fixnum(y));
        }
        CASE_LONG_FLOAT_BIGNUM;
        CASE_LONG_FLOAT_RATIO {
                return ecl_make_long_float(ecl_long_float(x) / ecl_to_long_double(y));
        }
        CASE_LONG_FLOAT_SINGLE_FLOAT {
                return ecl_make_long_float(ecl_long_float(x) / ecl_single_float(y));
        }
        CASE_LONG_FLOAT_DOUBLE_FLOAT {
                return ecl_make_long_float(ecl_long_float(x) / ecl_double_float(y));
        }
        CASE_LONG_FLOAT_LONG_FLOAT {
                return ecl_make_long_float(ecl_long_float(x) / ecl_long_float(y));
        }
        CASE_LONG_FLOAT_COMPLEX {
                goto COMPLEX_Y;
        }
        CASE_COMPLEX_LONG_FLOAT;  {
                goto COMPLEX_X;
        }
#endif
        CASE_COMPLEX_FIXNUM;
        CASE_COMPLEX_BIGNUM;
        CASE_COMPLEX_RATIO;
        CASE_COMPLEX_SINGLE_FLOAT;
        CASE_COMPLEX_DOUBLE_FLOAT; COMPLEX_X: {
                return ecl_make_complex(ecl_divide(x->complex.real, y),
                                        ecl_divide(x->complex.imag, y));
        }
        CASE_BIGNUM_COMPLEX;
        CASE_RATIO_COMPLEX;
        CASE_SINGLE_FLOAT_COMPLEX;
        CASE_DOUBLE_FLOAT_COMPLEX;
        CASE_FIXNUM_COMPLEX {
        COMPLEX_Y:
                return complex_divide(x, ecl_make_fixnum(0), y->complex.real, y->complex.imag);
        }
        CASE_COMPLEX_COMPLEX {
                return complex_divide(x->complex.real, x->complex.imag,
                                      y->complex.real, y->complex.imag);
        }
        CASE_UNKNOWN(@[/],x,y,@[number]);
}
MATH_DISPATCH2_END;
}

#else

cl_object
ecl_divide(cl_object x, cl_object y)
{
	cl_object z, z1, z2;

	switch (ecl_t_of(x)) {
	case t_fixnum:
	case t_bignum:
		switch (ecl_t_of(y)) {
		case t_fixnum:
			if (y == ecl_make_fixnum(0))
				FEdivision_by_zero(x, y);
		case t_bignum:
			if (ecl_minusp(y) == TRUE) {
				x = ecl_negate(x);
				y = ecl_negate(y);
			}
			return ecl_make_ratio(x, y);
		case t_ratio:
			z = ecl_times(x, y->ratio.den);
			return ecl_make_ratio(z, y->ratio.num);
		case t_singlefloat:
			return ecl_make_single_float(ecl_to_double(x) / ecl_single_float(y));
		case t_doublefloat:
			return ecl_make_double_float(ecl_to_double(x) / ecl_double_float(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return ecl_make_long_float(ecl_to_double(x) / ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[/], 2, y, @[number]);
		}
	case t_ratio:
		switch (ecl_t_of(y)) {
		case t_fixnum:
			if (y == ecl_make_fixnum(0))
				FEdivision_by_zero(x, y);
		case t_bignum:
			z = ecl_times(x->ratio.den, y);
			return ecl_make_ratio(x->ratio.num, z);
		case t_ratio:
			z = ecl_times(x->ratio.num,y->ratio.den);
			z1 = ecl_times(x->ratio.den,y->ratio.num);
			return ecl_make_ratio(z, z1);
		case t_singlefloat:
			return ecl_make_single_float(ecl_to_double(x) / ecl_single_float(y));
		case t_doublefloat:
			return ecl_make_double_float(ecl_to_double(x) / ecl_double_float(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return ecl_make_long_float(ecl_to_double(x) / ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[/], 2, y, @[number]);
		}
	case t_singlefloat:
		switch (ecl_t_of(y)) {
		case t_fixnum:
			return ecl_make_single_float(ecl_single_float(x) / ecl_fixnum(y));
		case t_bignum:
		case t_ratio:
			return ecl_make_single_float(ecl_single_float(x) / ecl_to_double(y));
		case t_singlefloat:
			return ecl_make_single_float(ecl_single_float(x) / ecl_single_float(y));
		case t_doublefloat:
			return ecl_make_double_float(ecl_single_float(x) / ecl_double_float(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return ecl_make_long_float(ecl_single_float(x) / ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[/], 2, y, @[number]);
		}
	case t_doublefloat:
		switch (ecl_t_of(y)) {
		case t_fixnum:
			return ecl_make_double_float(ecl_double_float(x) / ecl_fixnum(y));
		case t_bignum:
		case t_ratio:
			return ecl_make_double_float(ecl_double_float(x) / ecl_to_double(y));
		case t_singlefloat:
			return ecl_make_double_float(ecl_double_float(x) / ecl_single_float(y));
		case t_doublefloat:
			return ecl_make_double_float(ecl_double_float(x) / ecl_double_float(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return ecl_make_long_float(ecl_double_float(x) / ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[/], 2, y, @[number]);
		}
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		switch (ecl_t_of(y)) {
		case t_fixnum:
			return ecl_make_long_float(ecl_long_float(x) / ecl_fixnum(y));
		case t_bignum:
		case t_ratio:
			return ecl_make_long_float(ecl_long_float(x) / ecl_to_double(y));
		case t_singlefloat:
			return ecl_make_long_float(ecl_long_float(x) / ecl_single_float(y));
		case t_doublefloat:
			return ecl_make_long_float(ecl_long_float(x) / ecl_double_float(y));
		case t_longfloat:
			return ecl_make_long_float(ecl_long_float(x) / ecl_long_float(y));
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[/], 2, y, @[number]);
		}
#endif
	case t_complex:
		if (ecl_t_of(y) != t_complex) {
			z1 = ecl_divide(x->complex.real, y);
			z2 = ecl_divide(x->complex.imag, y);
			return ecl_make_complex(z1, z2);
		} else if (1) {
			/* #C(z1 z2) = #C(xr xi) * #C(yr -yi) */
			z1 = ecl_plus(ecl_times(x->complex.real, y->complex.real),
					 ecl_times(x->complex.imag, y->complex.imag));
			z2 = ecl_minus(ecl_times(x->complex.imag, y->complex.real),
					  ecl_times(x->complex.real, y->complex.imag));
		} else {
		COMPLEX: /* INV: x is real, y is complex */
			/* #C(z1 z2) = x * #C(yr -yi) */
			z1 = ecl_times(x, y->complex.real);
			z2 = ecl_negate(ecl_times(x, y->complex.imag));
		}
		z  = ecl_plus(ecl_times(y->complex.real, y->complex.real),
				 ecl_times(y->complex.imag, y->complex.imag));
		z  = ecl_make_complex(ecl_divide(z1, z), ecl_divide(z2, z));
		return(z);
	default:
		FEwrong_type_nth_arg(@[/], 1, x, @[number]);
	}
}

#endif
