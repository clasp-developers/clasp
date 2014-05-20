/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    character.d -- Character routines.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <stdio.h>
#include <ecl/ecl.h>

#include "char_ctype.d"

ecl_character
ecl_char_code(cl_object c)
{
	if (ecl_unlikely(!ECL_CHARACTERP(c)))
                FEwrong_type_only_arg(@[char-code], c, @[character]);
        return ECL_CHAR_CODE(c);
}

ecl_base_char
ecl_base_char_code(cl_object c)
{
#ifdef ECL_UNICODE
	if (ECL_CHARACTERP(c)) {
		cl_fixnum code = ECL_CHAR_CODE(c);
		if (code <= 255) {
			return (int)code;
		}
	}
	FEwrong_type_only_arg(@[char-code], c, @[base-char]);
#else
	return ecl_char_code(c);
#endif
}

cl_object
cl_standard_char_p(cl_object c)
{
	/* INV: ecl_char_code() checks the type */
	cl_fixnum i = ecl_char_code(c);
	@(return (ecl_standard_char_p(i)? ECL_T : ECL_NIL))
}

bool
ecl_standard_char_p(ecl_character code)
{
	return ((' ' <= code) && (code < '\177')) || (code == '\n');
}

bool
ecl_base_char_p(ecl_character c)
{
	return c <= 255;
}

cl_object
cl_graphic_char_p(cl_object c)
{
	/* INV: ecl_char_code() checks the type */
	@(return (ecl_graphic_char_p(ecl_char_code(c))? ECL_T : ECL_NIL))
}

cl_object
cl_alpha_char_p(cl_object c)
{
	/* INV: ecl_char_code() checks the type */
	@(return (ecl_alpha_char_p(ecl_char_code(c))? ECL_T : ECL_NIL))
}

cl_object
cl_upper_case_p(cl_object c)
{
	/* INV: ecl_char_code() checks the type */
	@(return (ecl_upper_case_p(ecl_char_code(c))? ECL_T : ECL_NIL))
}

cl_object
cl_lower_case_p(cl_object c)
{
	/* INV: ecl_char_code() checks the type */
	@(return (ecl_lower_case_p(ecl_char_code(c))? ECL_T : ECL_NIL))
}

cl_object
cl_both_case_p(cl_object c)
{
	/* INV: ecl_char_code() checks the type */
	@(return (ecl_both_case_p(ecl_char_code(c))? ECL_T : ECL_NIL))
}

int
ecl_string_case(cl_object s)
{
	int upcase;
	cl_index i;
	const ecl_base_char *text = (ecl_base_char*)s->base_string.self;
	for (i = 0, upcase = 0; i <= s->base_string.dim; i++) {
		if (ecl_upper_case_p(text[i])) {
			if (upcase < 0)
				return 0;
			upcase = +1;
		} else if (ecl_lower_case_p(text[i])) {
			if (upcase > 0)
				return 0;
			upcase = -1;
		}
	}
	return upcase;
}

@(defun digit_char_p (c &optional (radix ecl_make_fixnum(10)))
@ {
	cl_fixnum basis, value;
        if (ecl_unlikely(!ECL_FIXNUMP(radix) ||
                         ecl_fixnum_lower(radix, ecl_make_fixnum(2)) ||
                         ecl_fixnum_greater(radix, ecl_make_fixnum(36)))) {
                FEwrong_type_nth_arg(@[digit-char-p], 2, radix,
                                     ecl_make_integer_type(ecl_make_fixnum(2),
                                                           ecl_make_fixnum(36)));
        }
        basis = ecl_fixnum(radix);
	value = ecl_digitp(ecl_char_code(c), basis);
	@(return ((value < 0)? ECL_NIL: ecl_make_fixnum(value)));
} @)

/*
	Ecl_Digitp(i, r) returns the weight of code i
	as a digit of radix r, which must be 1 < r <= 36.
	If i is not a digit, -1 is returned.
*/
int
ecl_digitp(ecl_character i, int r)
{
	if (('0' <= i) && (i <= '9') && (i < '0' + r))
		return i - '0';
	if (('A' <= i) && (10 < r) && (i < 'A' + (r - 10)))
		return i - 'A' + 10;
	if (('a' <= i) && (10 < r) && (i < 'a' + (r - 10)))
		return i - 'a' + 10;
#ifdef ECL_UNICODE
	if (i > 255) {
		int number = ucd_decimal_digit(i);
		if (number < r)
			return number;
	}
#endif
	return -1;
}

cl_object
cl_alphanumericp(cl_object c)
{
	/* INV: ecl_char_code() checks type of `c' */
	cl_fixnum i = ecl_char_code(c);
	@(return (ecl_alphanumericp(i)? ECL_T : ECL_NIL))
}

@(defun char= (c &rest cs)
@
	/* INV: ecl_char_eq() checks types of `c' and `cs' */
	while (--narg)
		if (!ecl_char_eq(c, ecl_va_arg(cs)))
			@(return ECL_NIL)
	@(return ECL_T)
@)

bool
ecl_char_eq(cl_object x, cl_object y)
{
	return ecl_char_code(x) == ecl_char_code(y);
}

@(defun char/= (&rest cs)
	int i, j;
	cl_object c;
@
	/* INV: ecl_char_eq() checks types of its arguments */
	if (narg == 0)
		FEwrong_num_arguments(@[char/=]);
	c = ecl_va_arg(cs);
	for (i = 2; i<=narg; i++) {
		ecl_va_list ds;
		ecl_va_start(ds, narg, narg, 0);
		c = ecl_va_arg(cs);
		for (j = 1; j<i; j++)
			if (ecl_char_eq(ecl_va_arg(ds), c))
				@(return ECL_NIL)
	}
	@(return ECL_T)
@)

static cl_object
Lchar_cmp(cl_env_ptr env, cl_narg narg, int s, int t, ecl_va_list args)
{
	cl_object c, d;

	if (narg == 0)
		FEwrong_num_arguments_anonym();
	c = ecl_va_arg(args);
	for (; --narg; c = d) {
		d = ecl_va_arg(args);
		if (s*ecl_char_cmp(d, c) < t)
			ecl_return1(env, ECL_NIL);
	}
	ecl_return1(env, ECL_T);
}

int
ecl_char_cmp(cl_object x, cl_object y)
{
	/* ecl_char_code(x) returns an integer which is well in the range
	 * of positive fixnums. Therefore, this subtraction never
	 * oveflows. */
	return ecl_char_code(x) - ecl_char_code(y);
}

@(defun char< (&rest args)
@
	return Lchar_cmp(the_env, narg, 1, 1, args);
@)

@(defun char> (&rest args)
@
	return Lchar_cmp(the_env, narg,-1, 1, args);
@)

@(defun char<= (&rest args)
@
	return Lchar_cmp(the_env, narg, 1, 0, args);
@)

@(defun char>= (&rest args)
@
	return Lchar_cmp(the_env, narg,-1, 0, args);
@)

@(defun char_equal (c &rest cs)
	int i;
@
	/* INV: ecl_char_equal() checks the type of its arguments */
	for (narg--, i = 0;  i < narg;  i++) {
		if (!ecl_char_equal(c, ecl_va_arg(cs)))
			@(return ECL_NIL)
	}
	@(return ECL_T)
@)

#define char_equal_code(x) ecl_char_upcase(ecl_char_code(x))

bool
ecl_char_equal(cl_object x, cl_object y)
{
	return char_equal_code(x) == char_equal_code(y);
}

@(defun char-not-equal (&rest cs)
	int i, j;
	cl_object c;
@
	/* INV: ecl_char_equal() checks the type of its arguments */
	if (narg == 0)
		FEwrong_num_arguments(@[char-not-equal]);
	c = ecl_va_arg(cs);
	for (i = 2;  i<=narg;  i++) {
		ecl_va_list ds;
		ecl_va_start(ds, narg, narg, 0);
		c = ecl_va_arg(cs);
		for (j=1;  j<i;  j++)
			if (ecl_char_equal(c, ecl_va_arg(ds)))
				@(return ECL_NIL)
	}
	@(return ECL_T)
@)

static cl_object
Lchar_compare(cl_env_ptr env, cl_narg narg, int s, int t, ecl_va_list args)
{
	cl_object c, d;

	/* INV: ecl_char_compare() checks the types of its arguments */
	if (narg == 0)
		FEwrong_num_arguments_anonym();
	c = ecl_va_arg(args);
	for (; --narg; c = d) {
		d = ecl_va_arg(args);
		if (s*ecl_char_compare(d, c) < t)
			ecl_return1(env, ECL_NIL);
	}
	ecl_return1(env, ECL_T);
}

int
ecl_char_compare(cl_object x, cl_object y)
{
	cl_fixnum i = char_equal_code(x);
	cl_fixnum j = char_equal_code(y);

	if (i < j)
		return(-1);
	else if (i == j)
		return(0);
	else
		return(1);
}

@(defun char-lessp (&rest args)
@
	return Lchar_compare(the_env, narg, 1, 1, args);
@)

@(defun char-greaterp (&rest args)
@
	return Lchar_compare(the_env, narg,-1, 1, args);
@)

@(defun char-not-greaterp (&rest args)
@
	return Lchar_compare(the_env, narg, 1, 0, args);
@)

@(defun char-not-lessp (&rest args)
@
	return Lchar_compare(the_env, narg,-1, 0, args);
@)


cl_object
cl_character(cl_object x)
{
	switch (ecl_t_of(x)) {
	case t_character:
		break;
	case t_symbol:
		return cl_character(x->symbol.name);
#ifdef ECL_UNICODE
	case t_string:
		if (x->string.fillp == 1) {
			x = ECL_CODE_CHAR(x->string.self[0]);
			break;
		}
		goto ERROR;
#endif
	case t_base_string:
		if (x->base_string.fillp == 1) {
			x = ECL_CODE_CHAR(x->base_string.self[0]);
			break;
		}
	default: ERROR:
                FEwrong_type_nth_arg(@[character], 1, x, ecl_read_from_cstring("(OR CHARACTER SYMBOL (ARRAY CHARACTER (1)) (ARRAY BASE-CHAR (1)))"));
	}
	@(return x)
}

cl_object
cl_char_code(cl_object c)
{
	/* INV: ecl_char_code() checks the type of `c' */
	@(return ecl_make_fixnum(ecl_char_code(c)))
}

cl_object
cl_code_char(cl_object c)
{
	cl_fixnum fc;

	switch (ecl_t_of(c)) {
	case t_fixnum:
		fc = ecl_fixnum(c);
		if (fc < ECL_CHAR_CODE_LIMIT && fc >= 0) {
			c = ECL_CODE_CHAR(fc);
			break;
		}
	case t_bignum:
		c = ECL_NIL;
		break;
	default:
                FEwrong_type_only_arg(@[code-char], c, @[integer]);
	}
	@(return c)
}

cl_object
cl_char_upcase(cl_object c)
{
	/* INV: ecl_char_code() checks the type of `c' */
	cl_fixnum code = ecl_char_code(c);
	@(return ECL_CODE_CHAR(ecl_char_upcase(code)))
}

cl_object
cl_char_downcase(cl_object c)
{
	/* INV: ecl_char_code() checks the type of `c' */
	cl_fixnum code = ecl_char_code(c);
	@(return ECL_CODE_CHAR(ecl_char_downcase(code)))
}

@(defun digit_char (weight &optional (radix ecl_make_fixnum(10)))
@ {
        cl_fixnum basis;
        cl_object output = ECL_NIL;
        if (ecl_unlikely(!ECL_FIXNUMP(radix) ||
                         ecl_fixnum_lower(radix, ecl_make_fixnum(2)) ||
                         ecl_fixnum_greater(radix, ecl_make_fixnum(36)))) {
                FEwrong_type_nth_arg(@[digit-char], 2, radix,
                                     ecl_make_integer_type(ecl_make_fixnum(2),
                                                           ecl_make_fixnum(36)));
        }
        basis = ecl_fixnum(radix);
	switch (ecl_t_of(weight)) {
	case t_fixnum: {
		cl_fixnum value = ecl_fixnum(weight);
		if (value >= 0) {
			int dw = ecl_digit_char(value, basis);
			if (dw >= 0) {
				output = ECL_CODE_CHAR(dw);
			}
		}
		break;
	}
	case t_bignum:
		break;
	default:
                FEwrong_type_nth_arg(@[digit-char],1,weight,@[integer]);
	}
	@(return output)
} @)

short
ecl_digit_char(cl_fixnum w, cl_fixnum r)
{
	if (r < 2 || r > 36 || w < 0 || w >= r)
		return(-1);
	if (w < 10)
		return(w + '0');
	else
		return(w - 10 + 'A');
}

cl_object
cl_char_int(cl_object c)
{
	const cl_env_ptr the_env = ecl_process_env();
	/* INV: ecl_char_code() checks the type of `c' */
	ecl_return1(the_env, ecl_make_fixnum(ecl_char_code(c)));
}

/* here we give every character an implicit name of the form 'u#' where # is a hexadecimal number,
   corresponding to a unicode code point.
   #\u14ea should work, for example
*/

cl_object
cl_char_name(cl_object c)
{
	ecl_character code = ecl_char_code(c);
	cl_object output;
	if (code <= 127) {
		output = ecl_gethash_safe(ecl_make_fixnum(code), cl_core.char_names, ECL_NIL);
	}
#ifdef ECL_UNICODE_NAMES
	else if (!Null(output = _ecl_ucd_code_to_name(code))) {
		(void)0;
	}
#endif
	else {
		ecl_base_char name[8];
                ecl_base_char *start;
                name[7] = 0;
                name[6] = ecl_digit_char(code & 0xF, 16); code >>= 4;
                name[5] = ecl_digit_char(code & 0xF, 16); code >>= 4;
                name[4] = ecl_digit_char(code & 0xF, 16); code >>= 4;
                name[3] = ecl_digit_char(code & 0xF, 16); code >>= 4;
                if (code == 0) {
                        start = name + 2;
                } else {
                        name[2] = ecl_digit_char(code & 0xF, 16); code >>= 4;
                        name[1] = ecl_digit_char(code & 0xF, 16);
                        start = name;
                }
                start[0] = 'U';
		output = make_base_string_copy((const char*)start);
	}
	@(return output);
}

cl_object
cl_name_char(cl_object name)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_object c;
	cl_index l;
	name = cl_string(name);
	c = ecl_gethash_safe(name, cl_core.char_names, ECL_NIL);
        if (c != ECL_NIL) {
                ecl_return1(the_env, ECL_CODE_CHAR(ecl_fixnum(c)));
        }
#ifdef ECL_UNICODE_NAMES
	c = _ecl_ucd_name_to_code(name);
	if (c != ECL_NIL) {
		ecl_return1(the_env, cl_code_char(c));
	}
#endif
	if (ecl_stringp(name) && (l = ecl_length(name))) {
		c = cl_char(name, ecl_make_fixnum(0));
		if (l == 1) {
			(void)0;
		} else if (c != ECL_CODE_CHAR('u') && c != ECL_CODE_CHAR('U')) {
			c = ECL_NIL;
		} else {
			cl_index used_l;
			cl_index end = name->base_string.fillp;
			cl_index real_end = end;
			c = ecl_parse_integer(name, 1, end, &real_end, 16);
			used_l = real_end;
			if (!ECL_FIXNUMP(c) || (used_l == (l - 1))) {
				c = ECL_NIL;
			} else {
				c = ECL_CODE_CHAR(ecl_fixnum(c));
			}
		}
	}
	ecl_return1(the_env, c);
}
