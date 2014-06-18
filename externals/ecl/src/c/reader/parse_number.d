/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <ecl/ecl.h>

static bool
exponent_charp(cl_fixnum c)
{
        return (c == 'e') || (c == 'E') || (c == 'f') || (c == 'F') ||
                (c == 's') || (c == 'S') || (c == 'd') || (c == 'D') ||
                (c == 'l') || (c == 'L');
}

static cl_object
expt10(cl_index expt)
{
        cl_object accum = _ecl_big_register0();
        cl_object factor = _ecl_big_register1();
        _ecl_big_set_ui(accum, 1);
        _ecl_big_set_ui(factor, 10);
        for (; expt; expt >>= 1) {
                if (expt & 1) {
                        _ecl_big_mul(accum, accum, factor);
                }
                _ecl_big_mul(factor, factor, factor);
        }
        _ecl_big_register_free(factor);
        return _ecl_big_register_normalize(accum);
}

static cl_object
infinity(cl_index exp_char, int sign)
{
        cl_object var;
        switch (exp_char) {
        case 'e': case 'E':
                return infinity(ecl_current_read_default_float_format(), sign);
        case 's':  case 'S':
        case 'f':  case 'F':
                var = (sign<0)?
                        @'ext::single-float-negative-infinity' :
                        @'ext::single-float-positive-infinity';
                        break;
        case 'l':  case 'L':
#ifdef ECL_LONG_FLOAT
                var = (sign<0)?
                        @'ext::long-float-negative-infinity' :
                        @'ext::long-float-positive-infinity';
                        break;
#endif
        case 'd':  case 'D':
                var = (sign<0)?
                        @'ext::double-float-negative-infinity' :
                        @'ext::double-float-positive-infinity';
                        break;
        default:
                return OBJNULL;
        }
        return ecl_symbol_value(var);
}

static cl_object
make_float(cl_object num, cl_object exp, cl_index exp_char, int sign)
{
        if (!ECL_FIXNUMP(exp)) {
                return infinity(exp_char, sign);
        } else {
                cl_fixnum fix_exp = ecl_fixnum(exp);
                if (fix_exp > 0) {
                        num = ecl_times(num, expt10(fix_exp));
                } else if (fix_exp < 0) {
                        num = ecl_divide(num, expt10(-fix_exp));
                }
        }
 AGAIN:
        switch (exp_char) {
        case 'e': case 'E':
                exp_char = ecl_current_read_default_float_format();
                goto AGAIN;
        case 's':  case 'S':
        case 'f':  case 'F':
                return ecl_make_single_float(sign * ecl_to_double(num));
        case 'l':  case 'L':
#ifdef ECL_LONG_FLOAT
                return ecl_make_long_float(sign * ecl_to_long_double(num));
#endif
        case 'd':  case 'D': {
                return ecl_make_double_float(sign * ecl_to_double(num));
        }
        default:
                return OBJNULL;
        }
}

/*
	ecl_parse_number(str, start, end, ep, radix) parses C string str
	up to (but not including) str[end]
	using radix as the radix for the rational number.
	(For floating numbers, the radix is ignored and replaced with 10)
	When parsing succeeds,
	the index of the next character is assigned to *ep,
	and the number is returned as a lisp data object.
	If not, OBJNULL is returned.
*/
cl_object
ecl_parse_number(cl_object str, cl_index start, cl_index end,
		 cl_index *ep, unsigned int radix)
{
        int sign = -1, d;
	cl_index c, i, decimal = end;
        cl_object num = _ecl_big_register0();
        bool some_digit = 0;
	if (end <= start || radix > 36) {
		*ep = start;
		return OBJNULL;
	}
 AGAIN:
        _ecl_big_set_ui(num, 0);
        c = ecl_char(str, i = start);
        sign = 1;
        if (c == '+') {
                if (++i == end) goto NOT_A_NUMBER;
                c = ecl_char(str, i);
        } else if (c == '-') {
                sign = -1;
                if (++i == end) goto NOT_A_NUMBER;
                c = ecl_char(str, i);
        }
        if (c == '/') {
                goto NOT_A_NUMBER;
        }
        for (; i < end; i++) {
                c = ecl_char(str, i);
                if (c == '/') {
                        cl_object den;
                        if (sign < 0) _ecl_big_complement(num, num);
                        num = _ecl_big_register_normalize(num);
                        c = ecl_char(str, ++i);
                        if (ecl_digitp(c, radix) < 0)
                                goto NOT_A_NUMBER;
                        den = ecl_parse_integer(str, i, end, ep, radix);
                        if (den == OBJNULL || (*ep < end)) {
                                return OBJNULL;
                        } else if (den == ecl_make_fixnum(0)) {
                                return ECL_NIL;
                        } else {
                                return ecl_make_ratio(num, den);
                        }
                } else if (c == '.') {
                        if (decimal <= i) {
                                goto NOT_A_NUMBER;
                        }
                        if (radix != 10) {
                                radix = 10;
                                goto AGAIN;
                        }
                        /* For a number xxxx.1234...nEyyy
                         * we have stored in num the number xxxx1234...n and
                         * will get in the exponent yyy. What we do is to simply
                         * shift the exponent by -n. */
                        decimal = i+1;
                } else if ((d = ecl_digitp(c, radix)) >= 0) {
                        _ecl_big_mul_ui(num, num, radix);
                        _ecl_big_add_ui(num, num, d);
                        some_digit = 1;
                } else if (exponent_charp(c)) {
                        cl_object exp, decimals;
                        if (!some_digit)
                                goto NOT_A_NUMBER;
                        if (radix != 10) {
                                radix = 10;
                                goto AGAIN;
                        }
                        num = _ecl_big_register_normalize(num);
                        decimals = (decimal < i) ?
                                ecl_make_fixnum(decimal - i):
                                ecl_make_fixnum(0);
                        exp = ecl_parse_integer(str, ++i, end, ep, 10);
                        if (exp == OBJNULL || (*ep < end))
                                return OBJNULL;
                        return make_float(num, ecl_plus(decimals, exp),
                                          c, sign);
                } else if (radix != 10) {
                        _ecl_big_register_free(num);
                        num = ecl_parse_number(str, start, end, ep, 10);
                        if (num != OBJNULL) {
                                if (floatp(num))
                                        return num;
                                if (ECL_FIXNUMP(num) || ECL_BIGNUMP(num)) {
                                        i = *ep;
                                        if (i > start && ecl_char(str, i-1) == '.')
                                                return num;
                                }
                        }
                        return OBJNULL;
                } else {
                NOT_A_NUMBER:
                        *ep = i;
                        _ecl_big_register_free(num);
                        return OBJNULL;
                }
        }
        /* If we have reached the end without decimals (for instance
         * 1., 2, 13., etc) we return an integer */
        *ep = i;
        if (decimal < i) {
                if (!some_digit) goto NOT_A_NUMBER;
                return make_float(_ecl_big_register_normalize(num),
                                  ecl_make_fixnum(decimal - i), 'e', sign);
        } else {
                if (sign < 0) _ecl_big_complement(num, num);
                return _ecl_big_register_normalize(num);
        }
}
