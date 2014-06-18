/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    times.d  -- Implementation of CL:*
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

@(defun * (&rest nums)
	cl_object prod = ecl_make_fixnum(1);
@
	/* INV: type check in ecl_times() */
	while (narg--)
		prod = ecl_times(prod, ecl_va_arg(nums));
	@(return prod)
@)

#ifdef MATH_DISPATCH2_BEGIN

cl_object
ecl_times(cl_object x, cl_object y)
{
MATH_DISPATCH2_BEGIN(x,y)
{
        CASE_FIXNUM_FIXNUM {
                return _ecl_fix_times_fix(ecl_fixnum(x), ecl_fixnum(y));
        }
        CASE_FIXNUM_BIGNUM {
                return _ecl_big_times_fix(y, ecl_fixnum(x));
        }
        CASE_FIXNUM_RATIO;
        CASE_BIGNUM_RATIO {
                return ecl_make_ratio(ecl_times(x, y->ratio.num),
                                      y->ratio.den);
        }
        CASE_FIXNUM_SINGLE_FLOAT {
                return ecl_make_single_float(ecl_fixnum(x) * ecl_single_float(y));
        }
        CASE_FIXNUM_DOUBLE_FLOAT {
                return ecl_make_double_float(ecl_fixnum(x) * ecl_double_float(y));
        }
        CASE_BIGNUM_FIXNUM {
                return _ecl_big_times_fix(x, ecl_fixnum(y));
        }
        CASE_BIGNUM_BIGNUM {
                return _ecl_big_times_big(x, y);
        }
        CASE_BIGNUM_SINGLE_FLOAT {
                return ecl_make_single_float(ecl_to_float(x) * ecl_single_float(y));
        }
        CASE_BIGNUM_DOUBLE_FLOAT {
                return ecl_make_double_float(ecl_to_double(x) * ecl_double_float(y));
        }
        CASE_RATIO_FIXNUM;
        CASE_RATIO_BIGNUM {
                cl_object z = ecl_times(x->ratio.num, y);
                return ecl_make_ratio(z, x->ratio.den);
        }
        CASE_RATIO_RATIO {
                cl_object num = ecl_times(x->ratio.num,y->ratio.num);
                cl_object den = ecl_times(x->ratio.den,y->ratio.den);
                return ecl_make_ratio(num, den);
        }
        CASE_RATIO_SINGLE_FLOAT {
                return ecl_make_single_float(ecl_to_float(x) * ecl_single_float(y));
        }
        CASE_RATIO_DOUBLE_FLOAT {
                return ecl_make_double_float(ecl_to_double(x) * ecl_double_float(y));
        }
        CASE_SINGLE_FLOAT_FIXNUM {
                return ecl_make_single_float(ecl_single_float(x) * ecl_fixnum(y));
        }
        CASE_SINGLE_FLOAT_BIGNUM;
        CASE_SINGLE_FLOAT_RATIO {
                return ecl_make_single_float(ecl_single_float(x) * ecl_to_float(y));
        }
        CASE_SINGLE_FLOAT_SINGLE_FLOAT {
                return ecl_make_single_float(ecl_single_float(x) * ecl_single_float(y));
        }
        CASE_SINGLE_FLOAT_DOUBLE_FLOAT {
                return ecl_make_double_float(ecl_single_float(x) * ecl_double_float(y));
        }
        CASE_DOUBLE_FLOAT_FIXNUM {
                return ecl_make_double_float(ecl_double_float(x) * ecl_fixnum(y));
        }
        CASE_DOUBLE_FLOAT_BIGNUM;
        CASE_DOUBLE_FLOAT_RATIO {
                return ecl_make_double_float(ecl_double_float(x) * ecl_to_double(y));
        }
        CASE_DOUBLE_FLOAT_SINGLE_FLOAT {
                return ecl_make_double_float(ecl_double_float(x) * ecl_single_float(y));
        }
        CASE_DOUBLE_FLOAT_DOUBLE_FLOAT {
                return ecl_make_double_float(ecl_double_float(x) * ecl_double_float(y));
        }
#ifdef ECL_LONG_FLOAT
        CASE_FIXNUM_LONG_FLOAT {
                return ecl_make_long_float(ecl_fixnum(x) * ecl_long_float(y));
        }
        CASE_BIGNUM_LONG_FLOAT;
        CASE_RATIO_LONG_FLOAT {
                return ecl_make_long_float(ecl_to_long_double(x) * ecl_long_float(y));
        }
        CASE_SINGLE_FLOAT_LONG_FLOAT {
                return ecl_make_long_float(ecl_single_float(x) * ecl_long_float(y));
        }
        CASE_DOUBLE_FLOAT_LONG_FLOAT {
                return ecl_make_long_float(ecl_double_float(x) * ecl_long_float(y));
        }
        CASE_LONG_FLOAT_FIXNUM {
                return ecl_make_long_float(ecl_long_float(x) * ecl_fixnum(y));
        }
        CASE_LONG_FLOAT_BIGNUM;
        CASE_LONG_FLOAT_RATIO {
                return ecl_make_long_float(ecl_long_float(x) * ecl_to_long_double(y));
        }
        CASE_LONG_FLOAT_SINGLE_FLOAT {
                return ecl_make_long_float(ecl_long_float(x) * ecl_single_float(y));
        }
        CASE_LONG_FLOAT_DOUBLE_FLOAT {
                return ecl_make_long_float(ecl_long_float(x) * ecl_double_float(y));
        }
        CASE_LONG_FLOAT_LONG_FLOAT {
                return ecl_make_long_float(ecl_long_float(x) * ecl_long_float(y));
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
                cl_object aux = x;
                x = y; y = aux;
                goto COMPLEX_Y;
        }
        CASE_BIGNUM_COMPLEX;
        CASE_RATIO_COMPLEX;
        CASE_SINGLE_FLOAT_COMPLEX;
        CASE_DOUBLE_FLOAT_COMPLEX;
        CASE_FIXNUM_COMPLEX {
        COMPLEX_Y:
                return ecl_make_complex(ecl_times(x, y->complex.real),
                                        ecl_times(x, y->complex.imag));
        }
        CASE_COMPLEX_COMPLEX {
		cl_object z11 = ecl_times(x->complex.real, y->complex.real);
		cl_object z12 = ecl_times(x->complex.imag, y->complex.imag);
		cl_object z21 = ecl_times(x->complex.imag, y->complex.real);
		cl_object z22 = ecl_times(x->complex.real, y->complex.imag);
		return ecl_make_complex(ecl_minus(z11, z12), ecl_plus(z21, z22));
        }
        CASE_UNKNOWN(@[*],x,y,@[number]);
}
MATH_DISPATCH2_END;
}

#else

cl_object
ecl_times(cl_object x, cl_object y)
{
	cl_object z, z1;

	switch (ecl_t_of(x)) {
	case t_fixnum:
		switch (ecl_t_of(y)) {
		case t_fixnum:
			return _ecl_fix_times_fix(ecl_fixnum(x),ecl_fixnum(y));
		case t_bignum:
			return _ecl_big_times_fix(y, ecl_fixnum(x));
		case t_ratio:
			z = ecl_times(x, y->ratio.num);
			return ecl_make_ratio(z, y->ratio.den);
		case t_singlefloat:
			return ecl_make_single_float(ecl_fixnum(x) * ecl_single_float(y));
		case t_doublefloat:
			return ecl_make_double_float(ecl_fixnum(x) * ecl_double_float(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return ecl_make_long_float(ecl_fixnum(x) * ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[*], 2, y, @[number]);
		}
	case t_bignum:
		switch (ecl_t_of(y)) {
		case t_fixnum:
			return _ecl_big_times_fix(x, ecl_fixnum(y));
		case t_bignum:
			return _ecl_big_times_big(x, y);
		case t_ratio:
			z = ecl_times(x, y->ratio.num);
			return ecl_make_ratio(z, y->ratio.den);
		case t_singlefloat:
			return ecl_make_single_float(ecl_to_double(x) * ecl_single_float(y));
		case t_doublefloat:
			return ecl_make_double_float(ecl_to_double(x) * ecl_double_float(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return ecl_make_long_float(ecl_to_double(x) * ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[*], 2, y, @[number]);
		}
	case t_ratio:
		switch (ecl_t_of(y)) {
		case t_fixnum:
		case t_bignum:
			z = ecl_times(x->ratio.num, y);
			return ecl_make_ratio(z, x->ratio.den);
		case t_ratio:
			z = ecl_times(x->ratio.num,y->ratio.num);
			z1 = ecl_times(x->ratio.den,y->ratio.den);
			return ecl_make_ratio(z, z1);
		case t_singlefloat:
			return ecl_make_single_float(ecl_to_double(x) * ecl_single_float(y));
		case t_doublefloat:
			return ecl_make_double_float(ecl_to_double(x) * ecl_double_float(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return ecl_make_long_float(ecl_to_double(x) * ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[*], 2, y, @[number]);
		}
	case t_singlefloat: {
		float fx = ecl_single_float(x);
		switch (ecl_t_of(y)) {
		case t_fixnum:
			return ecl_make_single_float(fx * ecl_fixnum(y));
		case t_bignum:
		case t_ratio:
			return ecl_make_single_float(fx * ecl_to_double(y));
		case t_singlefloat:
			return ecl_make_single_float(fx * ecl_single_float(y));
		case t_doublefloat:
			return ecl_make_double_float(fx * ecl_double_float(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return ecl_make_long_float(fx * ecl_long_float(y));
#endif
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[*], 2, y, @[number]);
		}
	}
	case t_doublefloat: {
		switch (ecl_t_of(y)) {
		case t_fixnum:
			return ecl_make_double_float(ecl_double_float(x) * ecl_fixnum(y));
		case t_bignum:
		case t_ratio:
			return ecl_make_double_float(ecl_double_float(x) * ecl_to_double(y));
		case t_singlefloat:
			return ecl_make_double_float(ecl_double_float(x) * ecl_single_float(y));
		case t_doublefloat:
			return ecl_make_double_float(ecl_double_float(x) * ecl_double_float(y));
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			return ecl_make_long_float(ecl_double_float(x) * ecl_long_float(y));
#endif
		case t_complex: {
		COMPLEX: /* INV: x is real, y is complex */
			return ecl_make_complex(ecl_times(x, y->complex.real),
                                                ecl_times(x, y->complex.imag));
		}
		default:
			FEwrong_type_nth_arg(@[*], 2, y, @[number]);
		}
	}
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {
		long double lx = ecl_long_float(x);
		switch (ecl_t_of(y)) {
		case t_fixnum:
			return ecl_make_long_float(lx * ecl_fixnum(y));
		case t_bignum:
		case t_ratio:
			return ecl_make_long_float(lx * ecl_to_double(y));
		case t_singlefloat:
			return ecl_make_long_float(lx * ecl_single_float(y));
		case t_doublefloat:
			return ecl_make_long_float(lx * ecl_double_float(y));
		case t_longfloat:
			return ecl_make_long_float(lx * ecl_long_float(y));
		case t_complex:
			goto COMPLEX;
		default:
			FEwrong_type_nth_arg(@[*], 2, y, @[number]);
		}
	}
#endif
	case t_complex:
	{
		cl_object z11, z12, z21, z22;

		if (ecl_t_of(y) != t_complex) {
			cl_object aux = x;
			x = y; y = aux;
			goto COMPLEX;
		}
		z11 = ecl_times(x->complex.real, y->complex.real);
		z12 = ecl_times(x->complex.imag, y->complex.imag);
		z21 = ecl_times(x->complex.imag, y->complex.real);
		z22 = ecl_times(x->complex.real, y->complex.imag);
		return ecl_make_complex(ecl_minus(z11, z12), ecl_plus(z21, z22));
	}
	default:
		FEwrong_type_nth_arg(@[*], 1, x, @[number]);
	}
}

#endif
