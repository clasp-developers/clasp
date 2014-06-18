/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    number.c -- constructing numbers.
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

#include <float.h>
#include <limits.h>
#include <signal.h>
#define ECL_INCLUDE_MATH_H
#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/impl/math_fenv.h>

#if defined(ECL_IEEE_FP) && defined(HAVE_FEENABLEEXCEPT)
/*
 * We are using IEEE arithmetics and can rely on FPE exceptions
 * to be raised when invalid operations are performed.
 */
# define DO_DETECT_FPE(f) ecl_detect_fpe()
#else
/*
 * Either we can not rely on C signals or we do not want IEEE NaNs and
 * infinities. The first case typically happens for instance under OS
 * X, where the status of the FPE control word is changed by
 * printf. We have two alternatives.
 */
# define DO_DETECT_FPE(f) do {						\
		unlikely_if (isnan(f)) ecl_deliver_fpe(FE_INVALID);	\
		unlikely_if (!isfinite(f)) ecl_deliver_fpe(FE_OVERFLOW); \
        } while (0)
#endif

#if !ECL_CAN_INLINE
cl_fixnum
ecl_to_fix(cl_object f)
{
	if (ecl_unlikely(!ECL_FIXNUMP(f)))
		FEtype_error_fixnum(f);
	return ecl_fixnum(f);
}

cl_index
ecl_to_size(cl_object f)
{
	cl_fixnum aux;
	if (ecl_likely(ECL_FIXNUMP(f))) {
		cl_fixnum aux = ecl_fixnum(f);
		if (ecl_likely(aux >= 0))
			return aux;
	}
	FEtype_error_size(f);
}
#endif /* !ECL_CAN_INLINE */

cl_object
ecl_make_integer(cl_fixnum l)
{
	if (l > MOST_POSITIVE_FIXNUM || l < MOST_NEGATIVE_FIXNUM) {
                cl_object z = _ecl_big_register0();
                _ecl_big_set_fixnum(z, l);
                return _ecl_big_register_copy(z);
	}
	return ecl_make_fixnum(l);
}

cl_object
ecl_make_unsigned_integer(cl_index l)
{
	if (l > MOST_POSITIVE_FIXNUM) {
                cl_object z = _ecl_big_register0();
                _ecl_big_set_index(z, l);
                return _ecl_big_register_copy(z);
	}
	return ecl_make_fixnum(l);
}

int
ecl_to_bit(cl_object x) {
        if (ecl_unlikely((x != ecl_make_fixnum(0)) && (x != ecl_make_fixnum(1))))
                FEwrong_type_nth_arg(@[coerce], 1, x, @[bit]);
        return x == ecl_make_fixnum(1);
}

ecl_uint8_t
ecl_to_uint8_t(cl_object x) {
	if (ecl_likely(ECL_FIXNUMP(x))) {
		cl_fixnum aux = ecl_fixnum(x);
		if (ecl_likely(aux >= 0 && aux <= 255))
			return (ecl_uint8_t)aux;
	}
	FEwrong_type_argument(cl_list(2, @'unsigned-byte', ecl_make_fixnum(8)),
			      x);
}

ecl_int8_t
ecl_to_int8_t(cl_object x) {
	if (ecl_likely(ECL_FIXNUMP(x))) {
		cl_fixnum aux = ecl_fixnum(x);
		if (ecl_likely(aux >= -128 && aux <= 127))
			return (ecl_uint8_t)aux;
	}
	FEwrong_type_argument(cl_list(2, @'signed-byte', ecl_make_fixnum(8)),
			      x);
}

unsigned short
ecl_to_ushort(cl_object x) {
	const unsigned short ushort_max = USHRT_MAX;
        if (ecl_likely(ECL_FIXNUMP(x))) {
		cl_fixnum y = ecl_fixnum(x);
		if (ecl_likely(y >= 0 && y <= ushort_max)) {
			return (unsigned short)y;
                }
	}
	FEwrong_type_argument(cl_list(3,@'integer',
				      ecl_make_fixnum(0),
				      ecl_make_fixnum(ushort_max)),
			      x);
}

short
ecl_to_short(cl_object x) {
	const short short_min = SHRT_MIN;
	const short short_max = SHRT_MAX;
	if (ecl_likely(ECL_FIXNUMP(x))) {
		cl_fixnum y = ecl_fixnum(x);
		if (ecl_likely(y >= short_min && y <= short_max)) {
			return (short)y;
                }
	}
	FEwrong_type_argument(cl_list(3,@'integer',
				      ecl_make_fixnum(short_min),
				      ecl_make_fixnum(short_max)),
			      x);
}

#if FIXNUM_BITS < 32
# error "Unsupported platform with cl_fixnum < ecl_uint32_t"
#endif

#ifdef ecl_uint16_t
ecl_uint16_t
ecl_to_uint16_t(cl_object x) {
	const uint16_t uint16_max = 0xFFFFL;
        if (ecl_likely(ECL_FIXNUMP(x))) {
		cl_fixnum y = ecl_fixnum(x);
		if (ecl_likely(y >= 0 && y <= uint16_max)) {
			return (ecl_uint16_t)y;
                }
	}
	FEwrong_type_argument(cl_list(3,@'integer',
				      ecl_make_fixnum(0),
				      ecl_make_fixnum(uint16_max)),
			      x);
}

ecl_int16_t
ecl_to_int16_t(cl_object x) {
	const int16_t int16_min = -0x8000;
	const int16_t int16_max =  0x7FFF;
	if (ecl_likely(ECL_FIXNUMP(x))) {
		cl_fixnum y = ecl_fixnum(x);
		if (ecl_likely(y >= int16_min && y <= int16_max)) {
			return (ecl_int16_t)y;
                }
	}
	FEwrong_type_argument(cl_list(3,@'integer',
				      ecl_make_fixnum(int16_min),
				      ecl_make_fixnum(int16_max)),
			      x);
}
#endif /* ecl_uint16_t */

#if defined(ecl_uint32_t) && (FIXNUM_BITS > 32)
ecl_uint32_t
ecl_to_uint32_t(cl_object x) {
	const uint32_t uint32_max = 0xFFFFFFFFUL;
	if (ecl_likely(ECL_FIXNUMP(x))) {
		cl_fixnum y = ecl_fixnum(x);
		if (ecl_likely(y >= 0 && y <= uint32_max)) {
			return (ecl_uint32_t)y;
                }
	}
	FEwrong_type_argument(cl_list(3,@'integer',ecl_make_fixnum(0),
				      ecl_make_unsigned_integer(uint32_max)),
			      x);
}

ecl_int32_t
ecl_to_int32_t(cl_object x) {
	const int32_t int32_min = -0x80000000L;
	const int32_t int32_max =  0x7FFFFFFFL;
	if (ecl_likely(ECL_FIXNUMP(x))) {
		cl_fixnum y = ecl_fixnum(x);
		if (ecl_likely(y >= int32_min && y <= int32_max)) {
			return (ecl_int32_t)y;
                }
	}
	FEwrong_type_argument(cl_list(3,@'integer',
				      ecl_make_integer(int32_min),
				      ecl_make_integer(int32_max)),
			      x);
}
#endif /* ecl_uint32_t */

#if defined(ecl_uint64_t) && (FIXNUM_BITS < 64)
ecl_uint64_t
ecl_to_uint64_t(cl_object x) {
	if (!ecl_minusp(x)) {
		if (ECL_FIXNUMP(x)) {
			return (ecl_uint64_t)ecl_fixnum(x);
		} else if (!ECL_BIGNUMP(x)) {
			(void)0;
		} else if (mpz_fits_ulong_p(x->big.big_num)) {
			return (ecl_uint64_t)mpz_get_ui(x->big.big_num);
		} else {
			cl_object copy = _ecl_big_register0();
			mpz_fdiv_q_2exp(copy->big.big_num, x->big.big_num, 32);
			if (mpz_fits_ulong_p(copy->big.big_num)) {
				volatile ecl_uint64_t output;
				output = (ecl_uint64_t)mpz_get_ui(copy->big.big_num);
				output = (output << 32) +
					(ecl_uint64_t)mpz_get_ui(x->big.big_num);
				return output;
			}
		}
	}
	FEwrong_type_argument(cl_list(3,@'integer',ecl_make_fixnum(0),
				      ecl_one_minus(ecl_ash(ecl_make_fixnum(1), 64))),
			      x);
}

ecl_int64_t
ecl_to_int64_t(cl_object x) {
	if (ECL_FIXNUMP(x)) {
		return (ecl_int64_t)ecl_fixnum(x);
	} else if (!ECL_BIGNUMP(x)) {
		(void)0;
	} else if (mpz_fits_slong_p(x->big.big_num)) {
		return (ecl_int64_t)mpz_get_si(x->big.big_num);
	} else {
		cl_object copy = _ecl_big_register0();
		mpz_fdiv_q_2exp(copy->big.big_num, x->big.big_num, 32);
		if (mpz_fits_slong_p(copy->big.big_num)) {
			ecl_int64_t output;
			output = (ecl_int64_t)mpz_get_si(copy->big.big_num);
			mpz_fdiv_r_2exp(copy->big.big_num, x->big.big_num, 32);
			return (output << 32) + mpz_get_ui(copy->big.big_num);
		}
	}
	FEwrong_type_argument(cl_list(3,@'integer',
				      ecl_negate(ecl_ash(ecl_make_fixnum(1), 63)),
				      ecl_one_minus(ecl_ash(ecl_make_fixnum(1), 63))),
			      x);
}

cl_object
ecl_make_uint64_t(ecl_uint64_t i)
{
        if (i <= MOST_POSITIVE_FIXNUM) {
                return ecl_make_fixnum(i);
        } else if (i <= ~(ecl_uint32_t)0) {
                return ecl_make_uint32_t(i);
        } else {
                cl_object aux = ecl_make_uint32_t(i >> 32);
                return cl_logior(2, ecl_ash(aux, 32),
                                 ecl_make_uint32_t((ecl_uint32_t)i));
        }
}

cl_object
ecl_make_int64_t(ecl_int64_t i)
{
        if (i >= MOST_NEGATIVE_FIXNUM && i <= MOST_POSITIVE_FIXNUM) {
                return ecl_make_fixnum(i);
        } else {
                cl_object aux = ecl_make_int32_t(i >> 32);
                return cl_logior(2, ecl_ash(aux, 32), ecl_make_uint32_t((ecl_uint32_t)i));
        }
}
#endif /* ecl_uint64_t */

#if defined(ecl_ulong_long_t)
# if defined(ecl_uint32_t) && ECL_LONG_LONG_BITS == 32
ecl_ulong_long_t
ecl_to_ulong_long(cl_object x) {
        return (ecl_ulong_long_t)ecl_to_uint32_t(x);
}

ecl_long_long_t
ecl_to_long_long(cl_object x) {
        return (ecl_long_long_t)ecl_to_int32_t(x);
}
cl_object
ecl_make_ulong_long(ecl_ulong_long_t i) {
        return ecl_make_uint32_t(i);
}
cl_object
ecl_make_long_long(ecl_long_long_t i) {
        return ecl_make_int32_t(i);
}
# else
#  if defined(ecl_uint64_t) && ECL_LONG_LONG_BITS == 64
ecl_ulong_long_t
ecl_to_ulong_long(cl_object x) {
        return (ecl_ulong_long_t)ecl_to_uint64_t(x);
}
ecl_long_long_t
ecl_to_long_long(cl_object x) {
        return (ecl_long_long_t)ecl_to_int64_t(x);
}
cl_object
ecl_make_ulong_long(ecl_ulong_long_t i) {
        return ecl_make_uint64_t(i);
}
cl_object
ecl_make_long_long(ecl_long_long_t i) {
        return ecl_make_int64_t(i);
}
#  else
ecl_ulong_long_t
ecl_to_ulong_long(cl_object x) {
	if (!ecl_minusp(x)) {
		if (ECL_FIXNUMP(x)) {
			return (ecl_ulong_long_t)ecl_fixnum(x);
		} else if (!ECL_BIGNUMP(x)) {
			(void)0;
		} else if (mpz_fits_ulong_p(x->big.big_num)) {
			return (ecl_ulong_long_t)mpz_get_ui(x->big.big_num);
		} else {
			cl_object copy = _ecl_big_register0();
			int i = ECL_LONG_LONG_BITS - FIXNUM_BITS;
			mpz_fdiv_q_2exp(copy->bit.big_num, x->big.big_num, i);
			if (mpz_fits_ulong_p(copy->big.big_num)) {
				volatile ecl_ulong_long_t output;
				output = mpz_get_ui(copy->big.big_num);
				for (i -= FIXNUM_BITS; i; i-= FIXNUM_BITS) {
					output = (output << FIXNUM_BITS);
					output += mpz_get_ui(x->big.big_num);
				}
				return output;
			}
		}
	}
	FEwrong_type_argument(cl_list(3,@'integer',ecl_make_fixnum(0),
				      ecl_one_minus(ecl_ash(ecl_make_fixnum(1),
							    ECL_LONG_LONG_BITS))),
			      x);
}

ecl_long_long_t
ecl_to_long_long(cl_object x)
{
	if (ECL_FIXNUMP(x)) {
		return (ecl_long_long_t)ecl_fixnum(x);
	} else if (!ECL_BIGNUMP(x)) {
		(void)0;
	} else if (mpz_fits_slong_p(x->big.big_num)) {
		return (ecl_long_long_t)mpz_get_si(x->big.big_num);
	} else {
		cl_object copy = _ecl_big_register0();
		int i = ECL_LONG_LONG_BITS - FIXNUM_BITS;
		mpz_fdiv_q_2exp(copy->bit.big_num, x->big.big_num, i);
		if (mpz_fits_ulong_p(copy->big.big_num)) {
			volatile ecl_long_long_t output;
			output = mpz_get_si(copy->big.big_num);
			for (i -= FIXNUM_BITS; i; i-= FIXNUM_BITS) {
				output = (output << FIXNUM_BITS);
				output += mpz_get_ui(x->big.big_num);
			}
			return output;
		}
	}
	FEwrong_type_argument(cl_list(3,@'integer',
				      ecl_negate(ecl_ash(ecl_make_fixnum(1), ECL_LONG_LONG_BITS-1)),
				      ecl_one_minus(ecl_ash(ecl_make_fixnum(1), ECL_LONG_LONG_BITS-1))),
			      x);
}

cl_object
ecl_make_ulong_long(ecl_ulong_long_t i)
{
        if (i <= MOST_POSITIVE_FIXNUM) {
                return ecl_make_fixnum(i);
        } else if (i <= ~(ecl_uint32_t)0) {
                return ecl_make_uint32_t(i);
        } else {
                cl_object aux = ecl_make_uint32_t(i >> 32);
                return cl_logior(2, ecl_ash(aux, 32),
                                 ecl_make_uint32_t((ecl_uint32_t)i));
        }
}

cl_object
ecl_make_long_long(ecl_long_long_t i)
{
        if (i >= MOST_NEGATIVE_FIXNUM && i <= MOST_POSITIVE_FIXNUM) {
                return ecl_make_fixnum(i);
        } else {
                cl_object aux = ecl_make_int32_t(i >> 32);
                return cl_logior(2, ecl_ash(aux, 32), ecl_make_uint32_t((ecl_uint32_t)i));
        }
}
#  endif
# endif
#endif /* ecl_ulong_long_t */

cl_object
ecl_make_ratio(cl_object num, cl_object den)
{
	cl_object g, r;

	/* INV: the arguments NUM & DEN are integers */
	if (den == ecl_make_fixnum(0))
		FEdivision_by_zero(num, den);
	if (num == ecl_make_fixnum(0) || den == ecl_make_fixnum(1))
		return(num);
	if (ecl_minusp(den)) {
		num = ecl_negate(num);
		den = ecl_negate(den);
	}
	g = ecl_gcd(num, den);
        if (g != ecl_make_fixnum(1)) {
                num = ecl_integer_divide(num, g);
                den = ecl_integer_divide(den, g);
        }
	if (den == ecl_make_fixnum(1))
		return num;
	if (den == ecl_make_fixnum(-1))
		return ecl_negate(num);
	r = ecl_alloc_object(t_ratio);
	r->ratio.num = num;
	r->ratio.den = den;
	return(r);
}

void
ecl_deliver_fpe(int status)
{
        cl_env_ptr env = ecl_process_env();
        int bits = status & env->trap_fpe_bits;
	feclearexcept(FE_ALL_EXCEPT);
        if (bits) {
                cl_object condition;
		if (bits & FE_DIVBYZERO)
			condition = @'division-by-zero';
		else if (bits & FE_INVALID)
			condition = @'floating-point-invalid-operation';
		else if (bits & FE_OVERFLOW)
			condition = @'floating-point-overflow';
		else if (bits & FE_UNDERFLOW)
			condition = @'floating-point-underflow';
		else if (bits & FE_INEXACT)
			condition = @'floating-point-inexact';
                else
                        condition = @'arithmetic-error';
		cl_error(1, condition);
        }
}

cl_object
ecl_make_single_float(float f)
{
	cl_object x;

	DO_DETECT_FPE(f);
	if (f == (float)0.0) {
#if defined(ECL_SIGNED_ZERO)
		if (signbit(f))
                        return cl_core.singlefloat_minus_zero;
#endif
                return cl_core.singlefloat_zero;
	}
	x = ecl_alloc_object(t_singlefloat);
	ecl_single_float(x) = f;
	return(x);
}

cl_object
ecl_make_double_float(double f)
{
	cl_object x;

	DO_DETECT_FPE(f);
	if (f == (double)0.0) {
#if defined(ECL_SIGNED_ZERO)
		if (signbit(f))
                        return cl_core.doublefloat_minus_zero;
#endif
                return cl_core.doublefloat_zero;
	}
	x = ecl_alloc_object(t_doublefloat);
	ecl_double_float(x) = f;
	return(x);
}

#ifdef ECL_LONG_FLOAT
cl_object
ecl_make_long_float(long double f)
{
	cl_object x;

	DO_DETECT_FPE(f);
	if (f == (long double)0.0) {
#if defined(ECL_SIGNED_ZERO)
		if (signbit(f))
                        return cl_core.longfloat_minus_zero;
#endif
                return cl_core.longfloat_zero;
	}
	x = ecl_alloc_object(t_longfloat);
	x->longfloat.value = f;
	return x;
}
#endif

cl_object
ecl_make_complex(cl_object r, cl_object i)
{
	cl_object c;
	cl_type ti;
 AGAIN:
	ti = ecl_t_of(i);
	/* Both R and I are promoted to a common type */
	switch (ecl_t_of(r)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		switch (ti) {
		case t_fixnum:
			if (i == ecl_make_fixnum(0))
				return(r);
		case t_bignum:
		case t_ratio:
			break;
		case t_singlefloat:
			r = ecl_make_single_float((float)ecl_to_double(r));
			break;
		case t_doublefloat:
			r = ecl_make_double_float(ecl_to_double(r));
			break;
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			r = ecl_make_long_float(ecl_to_double(r));
			break;
#endif
		default:
			i = ecl_type_error(@'complex',"imaginary part", i, @'real');
			goto AGAIN;
		}
		break;
	case t_singlefloat:
		switch (ti) {
		case t_fixnum:
		case t_bignum:
		case t_ratio:
			i = ecl_make_single_float((float)ecl_to_double(i));
			break;
		case t_singlefloat:
			break;
		case t_doublefloat:
			r = ecl_make_double_float((double)(ecl_single_float(r)));
			break;
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			r = ecl_make_long_float((long double)ecl_single_float(r));
			break;
#endif
		default:
			i = ecl_type_error(@'complex',"imaginary part", i, @'real');
			goto AGAIN;
		}
		break;
	case t_doublefloat:
		switch (ti) {
		case t_fixnum:
		case t_bignum:
		case t_ratio:
		case t_singlefloat:
			i = ecl_make_double_float(ecl_to_double(i));
		case t_doublefloat:
			break;
#ifdef ECL_LONG_FLOAT
		case t_longfloat:
			r = ecl_make_long_float((long double)ecl_double_float(r));
			break;
#endif
		default:
			i = ecl_type_error(@'complex',"imaginary part", i, @'real');
			goto AGAIN;
		}
		break;
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		if (ti != t_longfloat)
			i = ecl_make_long_float((long double)ecl_to_double(i));
		break;
#endif
	default:
		r = ecl_type_error(@'complex',"real part", r, @'real');
		goto AGAIN;

	}
	c = ecl_alloc_object(t_complex);
	c->complex.real = r;
	c->complex.imag = i;
	return(c);
}

static cl_object
into_bignum(cl_object bignum, cl_object integer)
{
        if (ECL_FIXNUMP(integer)) {
                _ecl_big_set_fixnum(bignum, ecl_fixnum(integer));
        } else {
                mpz_set(bignum->big.big_num, integer->big.big_num);
        }
        return bignum;
}

static cl_fixnum
remove_zeros(cl_object *integer)
{
        cl_object buffer = into_bignum(_ecl_big_register0(), *integer);
        unsigned long den_twos = mpz_scan1(buffer->big.big_num, 0);
        if (den_twos < ULONG_MAX) {
                mpz_div_2exp(buffer->big.big_num, buffer->big.big_num, den_twos);
                *integer = _ecl_big_register_normalize(buffer);
                return -den_twos;
        } else {
                _ecl_big_register_free(buffer);
                return 0;
        }
}

static cl_object
prepare_ratio_to_float(cl_object num, cl_object den, int digits, cl_fixnum *scaleout)
{
        /* We have to cook our own routine because GMP does not round.
         * The recipe is simple: we multiply the numberator by a large
         * enough number so that the division by the denominator fits
         * the floating point number. The result is scaled back by the
         * appropriate exponent.
         */
        /* Scale down the denominator, eliminating the zeros
         * so that we have smaller operands.
         */
        cl_fixnum scale = remove_zeros(&den);
        cl_fixnum num_size = ecl_integer_length(num);
        cl_fixnum delta = ecl_integer_length(den) - num_size;
        scale -= delta;
        {
                cl_fixnum adjust = digits + delta + 1;
                if (adjust > 0) {
                        num = ecl_ash(num, adjust);
                } else if (adjust < 0) {
                        den = ecl_ash(den, -adjust);
                }
        }
        do {
		const cl_env_ptr the_env = ecl_process_env();
                cl_object fraction = ecl_truncate2(num, den);
                cl_object rem = ecl_nth_value(the_env, 1);
                cl_fixnum len = ecl_integer_length(fraction);
                if ((len - digits) == 1) {
                        if (ecl_oddp(fraction)) {
                                cl_object one = ecl_minusp(num)?
                                        ecl_make_fixnum(-1) :
                                        ecl_make_fixnum(1);
                                if (rem == ecl_make_fixnum(0)) {
                                        if (cl_logbitp(ecl_make_fixnum(1), fraction)
                                            != ECL_NIL)
                                                fraction = ecl_plus(fraction, one);
                                } else {
                                        fraction = ecl_plus(fraction, one);
                                }
                        }
                        *scaleout = scale - (digits + 1);
                        return fraction;
                }
                den = ecl_ash(den, 1);
                scale++;
        } while (1);
}

#if 0 /* Unused, we do not have ecl_to_float() */
static float
ratio_to_float(cl_object num, cl_object den)
{
        cl_fixnum scale;
        cl_object bits = prepare_ratio_to_float(num, den, FLT_MANT_DIG, &scale);
#if (FIXNUM_BITS-ECL_TAG_BITS) >= FLT_MANT_DIG
        /* The output of prepare_ratio_to_float will always fit an integer */
        float output = ecl_fixnum(bits);
#else
        float output = ECL_FIXNUMP(bits)? ecl_fixnum(bits) : _ecl_big_to_double(bits);
#endif
        return ldexpf(output, scale);
}
#endif

static double
ratio_to_double(cl_object num, cl_object den)
{
        cl_fixnum scale;
        cl_object bits = prepare_ratio_to_float(num, den, DBL_MANT_DIG, &scale);
#if (FIXNUM_BITS-ECL_TAG_BITS) >= DBL_MANT_DIG
        /* The output of prepare_ratio_to_float will always fit an integer */
        double output = ecl_fixnum(bits);
#else
        double output = ECL_FIXNUMP(bits)? ecl_fixnum(bits) : _ecl_big_to_double(bits);
#endif
        return ldexp(output, scale);
}

#ifdef ECL_LONG_FLOAT
static long double
ratio_to_long_double(cl_object num, cl_object den)
{
        cl_fixnum scale;
        cl_object bits = prepare_ratio_to_float(num, den, LDBL_MANT_DIG, &scale);
#if (FIXNUM_BITS-ECL_TAG_BITS) >= LDBL_MANT_DIG
        /* The output of prepare_ratio_to_float will always fit an integer */
        long double output = ecl_fixnum(bits);
#else
        long double output = ECL_FIXNUMP(bits)?
                (long double)ecl_fixnum(bits) :
                _ecl_big_to_long_double(bits);
#endif
        return ldexpl(output, scale);
}
#endif /* ECL_LONG_FLOAT */

float
ecl_to_float(cl_object x)
{
	if (ECL_FIXNUMP(x)) return(ecl_fixnum(x));	/* Immediate fixnum */

	switch (ecl_t_of(x)) {
	case t_fixnum:
		return (float)ecl_fixnum(x);
	case t_bignum:
		return (float)ratio_to_double(x, ecl_make_fixnum(1));
	case t_ratio:
                return (float)ratio_to_double(x->ratio.num, x->ratio.den);
	case t_singlefloat:
		return ecl_single_float(x);
	case t_doublefloat:
		return (float)ecl_double_float(x);
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		return (float)ecl_long_float(x);
#endif
	default:
                FEwrong_type_nth_arg(@[coerce], 1, x, @[real]);
	}
}

double
ecl_to_double(cl_object x)
{
	switch(ecl_t_of(x)) {
	case t_fixnum:
		return((double)(ecl_fixnum(x)));
	case t_bignum:
		return ratio_to_double(x, ecl_make_fixnum(1));
	case t_ratio:
                return ratio_to_double(x->ratio.num, x->ratio.den);
	case t_singlefloat:
		return (double)ecl_single_float(x);
	case t_doublefloat:
		return(ecl_double_float(x));
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
		return (double)ecl_long_float(x);
#endif
	default:
		FEwrong_type_nth_arg(@[coerce], 1, x, @[real]);
	}
}

#ifdef ECL_LONG_FLOAT
long double
ecl_to_long_double(cl_object x)
{
	switch(ecl_t_of(x)) {
	case t_fixnum:
		return (long double)ecl_fixnum(x);
	case t_bignum:
                return ratio_to_long_double(x, ecl_make_fixnum(1));
	case t_ratio:
                return ratio_to_long_double(x->ratio.num, x->ratio.den);
	case t_singlefloat:
		return (long double)ecl_single_float(x);
	case t_doublefloat:
		return (long double)ecl_double_float(x);
	case t_longfloat:
		return ecl_long_float(x);
	default:
		FEwrong_type_nth_arg(@[coerce], 1, x, @[real]);
	}
}
#endif

cl_object
cl_rational(cl_object x)
{
	double d;
 AGAIN:
	switch (ecl_t_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		break;
	case t_singlefloat:
		d = ecl_single_float(x);
		goto GO_ON;
	case t_doublefloat:
		d = ecl_double_float(x);
	GO_ON:	if (d == 0) {
			x = ecl_make_fixnum(0);
		} else {
			int e;
			d = frexp(d, &e);
			e -= DBL_MANT_DIG;
			x = _ecl_double_to_integer(ldexp(d, DBL_MANT_DIG));
                        if (e != 0) {
                                x = ecl_times(ecl_expt(ecl_make_fixnum(FLT_RADIX),
                                                       ecl_make_fixnum(e)),
                                              x);
                        }
		}
		break;
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {
		long double d = ecl_long_float(x);
		if (d == 0) {
			x = ecl_make_fixnum(0);
		} else {
			int e;
			d = frexpl(d, &e);
			e -= LDBL_MANT_DIG;
                        d = ldexpl(d, LDBL_MANT_DIG);
			x = _ecl_long_double_to_integer(d);
			if (e != 0) {
				x = ecl_times(ecl_expt(ecl_make_fixnum(FLT_RADIX),
                                                       ecl_make_fixnum(e)),
                                              x);
			}
		}
		break;
	}
#endif
	default:
		x = ecl_type_error(@'rational',"argument",x,@'number');
		goto AGAIN;
	}
	@(return x)
}

#ifdef ECL_LONG_FLOAT
cl_object
_ecl_long_double_to_integer(long double d0)
{
        const int fb = FIXNUM_BITS - 3;
        int e;
        long double d = frexpl(d0, &e);
        if (e <= fb) {
                return ecl_make_fixnum((cl_fixnum)d0);
        } else if (e > LDBL_MANT_DIG) {
                return ecl_ash(_ecl_long_double_to_integer(ldexp(d, LDBL_MANT_DIG)),
                               e - LDBL_MANT_DIG);
        } else {
                long double d1 = floorl(d = ldexpl(d, fb));
                int newe = e - fb;
                cl_object o = ecl_ash(_ecl_long_double_to_integer(d1), newe);
                long double d2 = ldexpl(d - d1, newe);
                if (d2) o = ecl_plus(o, _ecl_long_double_to_integer(d2));
                return o;
        }
}
#endif

cl_object
_ecl_double_to_integer(double d)
{
	if (d <= MOST_POSITIVE_FIXNUM && d >= MOST_NEGATIVE_FIXNUM)
		return ecl_make_fixnum((cl_fixnum)d);
	else {
                cl_object z = _ecl_big_register0();
                _ecl_big_set_d(z, d);
                return _ecl_big_register_copy(z);
	}
}

cl_object
_ecl_float_to_integer(float d)
{
	if (d <= MOST_POSITIVE_FIXNUM && d >= MOST_NEGATIVE_FIXNUM)
		return ecl_make_fixnum((cl_fixnum)d);
	else {
                cl_object z = _ecl_big_register0();
                _ecl_big_set_d(z, d);
                return _ecl_big_register_copy(z);
	}
}
