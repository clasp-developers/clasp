/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    read.d -- Read.
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
#include <ecl/number.h>
#include <stdio.h>
#include <limits.h>
#include <float.h>
#include <string.h>
#include <stdlib.h>
#include <ecl/internal.h>
#include <ecl/ecl-inl.h>
#include <ecl/bytecodes.h>

#undef _complex

static cl_object dispatch_macro_character(cl_object table, cl_object strm, int c);

#define read_suppress (ecl_symbol_value(@'*read-suppress*') != ECL_NIL)

#ifdef ECL_UNICODE
# define TOKEN_STRING_DIM(s) ((s)->string.dim)
# define TOKEN_STRING_FILLP(s) ((s)->string.fillp)
# define TOKEN_STRING_CHAR(s,n) ((s)->string.self[n])
# define TOKEN_STRING_CHAR_SET(s,n,c) (s)->string.self[n]=(c)
# define TOKEN_STRING_CHAR_CMP(s,n,c) ((s)->string.self[n]==(c))
#else
# define TOKEN_STRING_DIM(s) ((s)->base_string.dim)
# define TOKEN_STRING_FILLP(s) ((s)->base_string.fillp)
# define TOKEN_STRING_CHAR(s,n) ((s)->base_string.self[n])
# define TOKEN_STRING_CHAR_SET(s,n,c) ((s)->base_string.self[n]=(c))
# define TOKEN_STRING_CHAR_CMP(s,n,c) ((s)->base_string.self[n]==(c))
#endif

#define ECL_READ_ONLY_TOKEN 1
#define ECL_READ_RETURN_IGNORABLE 3
#define ECL_READ_LIST_DOT 4

cl_object
si_get_buffer_string()
{
	const cl_env_ptr env = ecl_process_env();
	cl_object pool = env->string_pool;
	cl_object output;
	if (pool == ECL_NIL) {
#ifdef ECL_UNICODE
		output = ecl_alloc_adjustable_extended_string(ECL_BUFFER_STRING_SIZE);
#else
		output = ecl_alloc_adjustable_base_string(ECL_BUFFER_STRING_SIZE);
#endif
	} else {
		output = CAR(pool);
		env->string_pool = CDR(pool);
	}
	TOKEN_STRING_FILLP(output) = 0;
	@(return output)
}

cl_object
si_put_buffer_string(cl_object string)
{
	if (string != ECL_NIL) {
		const cl_env_ptr env = ecl_process_env();
		cl_object pool = env->string_pool;
		cl_index l = 0;
		if (pool != ECL_NIL) {
			/* We store the size of the pool in the string index */
			l = TOKEN_STRING_FILLP(ECL_CONS_CAR(pool));
		}
		if (l < ECL_MAX_STRING_POOL_SIZE) {
			/* Ok, by ignoring the following code, here we
			 * are doing like SBCL: we simply grow the
			 * input buffer and do not care about its
			 * size. */
#if 0
			if (TOKEN_STRING_DIM(string) > 32*ECL_BUFFER_STRING_SIZE) {
				/* String has been enlarged. Cut it. */
#ifdef ECL_UNICODE
				string = ecl_alloc_adjustable_extended_string(ECL_BUFFER_STRING_SIZE);
#else
				string = ecl_alloc_adjustable_base_string(ECL_BUFFER_STRING_SIZE);
#endif
			}
#endif
			TOKEN_STRING_FILLP(string) = l+1;
			env->string_pool = CONS(string, pool);
		}
	}
	@(return)
}

static void extra_argument (int c, cl_object stream, cl_object d);
static cl_object patch_sharp(const cl_env_ptr env, cl_object x);
static cl_object do_read_delimited_list(int d, cl_object strm, bool proper_list);

cl_object
ecl_read_object_non_recursive(cl_object in)
{
	cl_object x;
	const cl_env_ptr env = ecl_process_env();

	ecl_bds_bind(env, @'si::*sharp-eq-context*', ECL_NIL);
	ecl_bds_bind(env, @'si::*backq-level*', ecl_make_fixnum(0));
	x = ecl_read_object(in);
	x = patch_sharp(env, x);
	ecl_bds_unwind_n(env, 2);
	return x;
}

/*
 * This routine inverts the case of the characters in the buffer which
 * were not escaped. ESCAPE_LIST is a list of intevals of characters
 * that were escaped, as in ({(low-limit . high-limit)}*). The list
 * goes from the last interval to the first one, in reverse order,
 * and thus we run the buffer from the end to the beginning.
 */
static void
invert_buffer_case(cl_object x, cl_object escape_list, int sign)
{
	cl_fixnum high_limit, low_limit;
	cl_fixnum i = TOKEN_STRING_FILLP(x);
	do {
		if (escape_list != ECL_NIL) {
			cl_object escape_interval = CAR(escape_list);
			high_limit = ecl_fixnum(CAR(escape_interval));
			low_limit = ecl_fixnum(CDR(escape_interval));
			escape_list = CDR(escape_list);
		} else {
			high_limit = low_limit = -1;
		}
		for (; i > high_limit; i--) {
			/* The character is not escaped */
			int c = TOKEN_STRING_CHAR(x,i);
			if (ecl_upper_case_p(c) && (sign < 0)) {
				c = ecl_char_downcase(c);
			} else if (ecl_lower_case_p(c) && (sign > 0)) {
				c = ecl_char_upcase(c);
			}
			TOKEN_STRING_CHAR_SET(x,i,c);
		}
		for (; i > low_limit; i--) {
			/* The character is within an escaped interval */
			;
		}
	} while (i >= 0);
}

static cl_object
ecl_read_object_with_delimiter(cl_object in, int delimiter, int flags,
                               enum ecl_chattrib a)
{
	cl_object x, token;
	int c, base;
	cl_object p;
	cl_index length, i;
	int colon, intern_flag;
	bool external_symbol;
        cl_env_ptr the_env = ecl_process_env();
	cl_object rtbl = ecl_current_readtable();
	enum ecl_readtable_case read_case = rtbl->readtable.read_case;
	cl_object escape_list; /* intervals of escaped characters */
	cl_fixnum upcase; /* # uppercase characters - # downcase characters */
	cl_fixnum count; /* number of unescaped characters */
	bool suppress = read_suppress;
	if (a != cat_constituent) {
		c = 0;
		goto LOOP;
	}
BEGIN:
	do {
		c = ecl_read_char(in);
		if (c == delimiter) {
                        the_env->nvalues = 0;
			return OBJNULL;
                }
		if (c == EOF)
			FEend_of_file(in);
		a = ecl_readtable_get(rtbl, c, &x);
	} while (a == cat_whitespace);
	if ((a == cat_terminating || a == cat_non_terminating) &&
            (flags != ECL_READ_ONLY_TOKEN)) {
		cl_object o;
		if (ECL_HASH_TABLE_P(x)) {
			o = dispatch_macro_character(x, in, c);
		} else {
			o = _ecl_funcall3(x, in, ECL_CODE_CHAR(c));
		}
		if (the_env->nvalues == 0) {
                        if (flags == ECL_READ_RETURN_IGNORABLE)
                                return ECL_NIL;
                        goto BEGIN;
                }
		unlikely_if (the_env->nvalues > 1) {
                        FEerror("The readmacro ~S returned ~D values.",
                                2, x, ecl_make_fixnum(the_env->nvalues));
                }
		return o;
	}
LOOP:
	p = escape_list = ECL_NIL;
	upcase = count = length = 0;
	external_symbol = colon = 0;
	token = si_get_buffer_string();
	for (;;) {
		if (c == ':' && (flags != ECL_READ_ONLY_TOKEN) &&
                    a == cat_constituent) {
			colon++;
			goto NEXT;
		}
		if (colon > 2) {
			while (colon--) {
				ecl_string_push_extend(token, ':');
				length++;
			}
		} else if (colon) {
			external_symbol = (colon == 1);
			TOKEN_STRING_CHAR_SET(token,length,'\0');
			/* If the readtable case was :INVERT and all non-escaped characters
			 * had the same case, we revert their case. */
			if (read_case == ecl_case_invert) {
				if (upcase == count) {
					invert_buffer_case(token, escape_list, -1);
				} else if (upcase == -count) {
					invert_buffer_case(token, escape_list, +1);
				}
			}
			if (length == 0) {
				p = cl_core.keyword_package;
				external_symbol = 0;
			} else {
				p = ecl_find_package_nolock(token);
			}
			if (Null(p) && !suppress) {
				/* When loading binary files, we sometimes must create
				   symbols whose package has not yet been maked. We
				   allow it, but later on in ecl_init_module we make sure that
				   all referenced packages have been properly built.
				*/
				cl_object name = cl_copy_seq(token);
				unlikely_if (Null(the_env->packages_to_be_created_p)) {
					FEerror("There is no package with the name ~A.",
						1, name);
				}
                                p = _ecl_package_to_be_created(the_env, name);
			}
			TOKEN_STRING_FILLP(token) = length = 0;
			upcase = count = colon = 0;
			escape_list = ECL_NIL;
		}
		if (a == cat_single_escape) {
			c = ecl_read_char_noeof(in);
			a = cat_constituent;
			if (read_case == ecl_case_invert) {
				escape_list = CONS(CONS(ecl_make_fixnum(length),
							ecl_make_fixnum(length)),
						   escape_list);
			} else {
				escape_list = ECL_T;
			}
			ecl_string_push_extend(token, c);
			length++;
			goto NEXT;
		}
		if (a == cat_multiple_escape) {
			cl_index begin = length;
			for (;;) {
				c = ecl_read_char_noeof(in);
				a = ecl_readtable_get(rtbl, c, NULL);
				if (a == cat_single_escape) {
					c = ecl_read_char_noeof(in);
					a = cat_constituent;
				} else if (a == cat_multiple_escape)
					break;
				ecl_string_push_extend(token, c);
				length++;
			}
			if (read_case == ecl_case_invert) {
				escape_list = CONS(CONS(ecl_make_fixnum(begin),
							ecl_make_fixnum(length-1)),
						   escape_list);
			} else {
				escape_list = ECL_T;
			}
			goto NEXT;
		}
		if (a == cat_whitespace || a == cat_terminating) {
			ecl_unread_char(c, in);
			break;
		}
		unlikely_if (ecl_invalid_character_p(c)) {
			FEreader_error("Found invalid character ~:C", in,
                                       1, ECL_CODE_CHAR(c));
		}
		if (read_case != ecl_case_preserve) {
			if (ecl_upper_case_p(c)) {
				upcase++;
				count++;
				if (read_case == ecl_case_downcase)
					c = ecl_char_downcase(c);
			} else if (ecl_lower_case_p(c)) {
				upcase--;
				count++;
				if (read_case == ecl_case_upcase)
					c = ecl_char_upcase(c);
			}
		}
		ecl_string_push_extend(token, c);
		length++;
	NEXT:
		c = ecl_read_char(in);
		if (c == EOF)
			break;
		a = ecl_readtable_get(rtbl, c, NULL);
	}

	if (suppress) {
		x = ECL_NIL;
		goto OUTPUT;
	}

	/* If there are some escaped characters, it must be a symbol */
	if ((flags == ECL_READ_ONLY_TOKEN) || p != ECL_NIL ||
            escape_list != ECL_NIL || length == 0)
		goto SYMBOL;

	/* The case in which the buffer is full of dots has to be especial cased */
	if (length == 1 && TOKEN_STRING_CHAR_CMP(token,0,'.')) {
		if (flags == ECL_READ_LIST_DOT) {
			x = @'si::.';
			goto OUTPUT;
		} else {
			FEreader_error("Dots appeared illegally.", in, 0);
		}
	} else {
		int i;
		for (i = 0;  i < length;  i++) {
			if (!TOKEN_STRING_CHAR_CMP(token,i,'.'))
				goto MAYBE_NUMBER;
		}
		FEreader_error("Dots appeared illegally.", in, 0);
	}

 MAYBE_NUMBER:
	/* Here we try to parse a number from the content of the buffer */
	base = ecl_current_read_base();
	if ((base <= 10) && ecl_alpha_char_p(TOKEN_STRING_CHAR(token,0)))
		goto SYMBOL;
	x = ecl_parse_number(token, 0, TOKEN_STRING_FILLP(token), &i, base);
	unlikely_if (x == ECL_NIL)
		FEreader_error("Syntax error when reading number.~%Offending string: ~S.",
			       in, 1, token);
	if (x != OBJNULL && length == i)
		goto OUTPUT;
 SYMBOL:
	/*TOKEN_STRING_CHAR_SET(token,length,'\0');*/
 	/* If the readtable case was :INVERT and all non-escaped characters
	 * had the same case, we revert their case. */
	if (read_case == ecl_case_invert) {
		if (upcase == count) {
			invert_buffer_case(token, escape_list, -1);
		} else if (upcase == -count) {
			invert_buffer_case(token, escape_list, +1);
		}
	}
	if (flags == ECL_READ_ONLY_TOKEN) {
                the_env->nvalues = 1;
		return token;
	} else if (external_symbol) {
		x = ecl_find_symbol(token, p, &intern_flag);
		unlikely_if (intern_flag != ECL_EXTERNAL) {
			FEerror("Cannot find the external symbol ~A in ~S.",
				2, cl_copy_seq(token), p);
		}
	} else {
		if (p == ECL_NIL) {
			p = ecl_current_package();
		}
		/* INV: cl_make_symbol() copies the string */
		x = ecl_intern(token, p, &intern_flag);
	}
 OUTPUT:
	si_put_buffer_string(token);
        the_env->nvalues = 1;
	return x;
}

/*
	ecl_read_object(in) reads an object from stream in.
	This routine corresponds to COMMON Lisp function READ.
*/
cl_object
ecl_read_object(cl_object in)
{
	return ecl_read_object_with_delimiter(in, EOF, 0, cat_constituent);
}

cl_object
si_read_object_or_ignore(cl_object in, cl_object eof)
{
	cl_object x;
	const cl_env_ptr env = ecl_process_env();

	ecl_bds_bind(env, @'si::*sharp-eq-context*', ECL_NIL);
	ecl_bds_bind(env, @'si::*backq-level*', ecl_make_fixnum(0));
        x = ecl_read_object_with_delimiter(in, EOF, ECL_READ_RETURN_IGNORABLE, 
                                           cat_constituent);
        if (x == OBJNULL) {
                env->nvalues = 1;
                x = eof;
        } else if (env->nvalues) {
		x = patch_sharp(env, x);
        }
	ecl_bds_unwind_n(env, 2);
	return x;
}

static cl_object
right_parenthesis_reader(cl_object in, cl_object character)
{
	FEreader_error("Unmatched right parenthesis, #\\)", in, 0);
}

static cl_object
left_parenthesis_reader(cl_object in, cl_object character)
{
	const char c = ')';
	@(return do_read_delimited_list(c, in, 0))
}

/*
 * BACKQUOTE READER
 */

static
cl_object comma_reader(cl_object in, cl_object c)
{
	cl_object x, y;
	const cl_env_ptr env = ecl_process_env();
	cl_fixnum backq_level = ecl_fixnum(ECL_SYM_VAL(env, @'si::*backq-level*'));

	unlikely_if (backq_level <= 0)
		FEreader_error("A comma has appeared out of a backquote.", in, 0);
	/* Read character & complain at EOF */
	c = cl_peek_char(2,ECL_NIL,in);
	if (c == ECL_CODE_CHAR('@@')) {
		x = @'si::unquote-splice';
		ecl_read_char(in);
	} else if (c == ECL_CODE_CHAR('.')) {
		x = @'si::unquote-nsplice';
		ecl_read_char(in);
	} else {
		x = @'si::unquote';
	}
	ECL_SETQ(env, @'si::*backq-level*', ecl_make_fixnum(backq_level-1));
	y = ecl_read_object(in);
	ECL_SETQ(env, @'si::*backq-level*', ecl_make_fixnum(backq_level));
	return cl_list(2, x, y);
}

static
cl_object backquote_reader(cl_object in, cl_object c)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_fixnum backq_level = ecl_fixnum(ECL_SYM_VAL(the_env, @'si::*backq-level*'));
	ECL_SETQ(the_env, @'si::*backq-level*', ecl_make_fixnum(backq_level+1));
	in = ecl_read_object(in);
	ECL_SETQ(the_env, @'si::*backq-level*', ecl_make_fixnum(backq_level));
#if 0
	@(return cl_macroexpand_1(2, cl_list(2, @'si::quasiquote', in), ECL_NIL));
#else
	@(return cl_list(2,@'si::quasiquote',in))
#endif
}

/*
	read_constituent(in) reads a sequence of constituent characters from
	stream in and places it in token.  As a help, it returns TRUE
	or FALSE depending on the value of *READ-SUPPRESS*.
*/
static cl_object
read_constituent(cl_object in)
{
	int store = !read_suppress;
	cl_object rtbl = ecl_current_readtable();
	bool not_first = 0;
	cl_object token = si_get_buffer_string();
	do {
		int c = ecl_read_char(in);
		enum ecl_chattrib c_cat;
		if (c == EOF) {
			break;
		}
		c_cat = ecl_readtable_get(rtbl, c, NULL);
		if (c_cat == cat_constituent ||
		    ((c_cat == cat_non_terminating) && not_first))
		{
			if (store) {
				ecl_string_push_extend(token, c);
			}
		} else {
			ecl_unread_char(c, in);
			break;
		}
		not_first = 1;
	} while(1);
	return (read_suppress)? ECL_NIL : token;
}

static cl_object
double_quote_reader(cl_object in, cl_object c)
{
	int delim = ECL_CHAR_CODE(c);
	cl_object rtbl = ecl_current_readtable();
	cl_object token = si_get_buffer_string();
	cl_object output;
	for (;;) {
		int c = ecl_read_char_noeof(in);
		if (c == delim)
			break;
		else if (ecl_readtable_get(rtbl, c, NULL) == cat_single_escape)
			c = ecl_read_char_noeof(in);
		ecl_string_push_extend(token, c);
	}
#ifdef ECL_UNICODE
	if (ecl_fits_in_base_string(token))
		output = si_coerce_to_base_string(token);
	else
#endif
		output = cl_copy_seq(token);
	si_put_buffer_string(token);
	@(return output)
}

static cl_object
dispatch_reader_fun(cl_object in, cl_object dc)
{
	cl_object readtable = ecl_current_readtable();
	cl_object dispatch_table;
	int c = ecl_char_code(dc);
	ecl_readtable_get(readtable, c, &dispatch_table);
	unlikely_if (!ECL_HASH_TABLE_P(dispatch_table))
		FEreader_error("~C is not a dispatching macro character",
			       in, 1, dc);
	return dispatch_macro_character(dispatch_table, in, c);
}

static cl_object
dispatch_macro_character(cl_object table, cl_object in, int c)
{
	cl_object arg;
	int d;
	c = ecl_read_char_noeof(in);
	d = ecl_digitp(c, 10);
	if (d >= 0) {
		cl_fixnum i = 0;
		do {
			i = 10*i + d;
			c = ecl_read_char_noeof(in);
			d = ecl_digitp(c, 10);
		} while (d >= 0);
		arg = ecl_make_fixnum(i);
	} else {
		arg = ECL_NIL;
	}
	{
		cl_object dc = ECL_CODE_CHAR(c);
		cl_object fun = ecl_gethash_safe(dc, table, ECL_NIL);
		unlikely_if (Null(fun)) {
			FEreader_error("No dispatch function defined "
				       "for character ~S",
				       in, 1, dc);
		}
		return _ecl_funcall4(fun, in, dc, arg);
	}
}

static cl_object
single_quote_reader(cl_object in, cl_object c)
{
	c = ecl_read_object(in);
	unlikely_if (c == OBJNULL)
		FEend_of_file(in);
	@(return cl_list(2, @'quote', c))
}

static cl_object
void_reader(cl_object in, cl_object c)
{
	/*  no result  */
	@(return)
}

static cl_object
semicolon_reader(cl_object in, cl_object c)
{
	int auxc;

	do
		auxc = ecl_read_char(in);
	while (auxc != '\n' && auxc != EOF);
	/*  no result  */
	@(return)
}

/*
	sharpmacro routines
*/

static cl_object
sharp_C_reader(cl_object in, cl_object c, cl_object d)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_object x, real, imag;

	if (d != ECL_NIL && !read_suppress)
		extra_argument('C', in, d);
	x = ecl_read_object(in);
	unlikely_if (x == OBJNULL)
		FEend_of_file(in);
	if (read_suppress)
		@(return ECL_NIL);
	unlikely_if (!ECL_CONSP(x) || ecl_length(x) != 2)
		FEreader_error("Reader macro #C should be followed by a list",
			       in, 0);
	real = CAR(x);
	imag = CADR(x);
	/* INV: ecl_make_complex() checks its types. When reading circular
	   structures, we cannot check the types of the elements, and we
	   must build the complex number by hand. */
	if ((CONSP(real) || CONSP(imag)) &&
	    !Null(ECL_SYM_VAL(the_env, @'si::*sharp-eq-context*')))
	{
		x = ecl_alloc_object(t_complex);
		x->complex.real = real;
		x->complex.imag = imag;
	} else {
		x = ecl_make_complex(real, imag);
	}
	@(return x)
}

static cl_object
sharp_backslash_reader(cl_object in, cl_object c, cl_object d)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_object token;
	if (d != ECL_NIL && !read_suppress) {
		unlikely_if (!ECL_FIXNUMP(d) || d != ecl_make_fixnum(0)) {
			FEreader_error("~S is an illegal CHAR-FONT.", in, 1, d);
                }
        }
	token = ecl_read_object_with_delimiter(in, EOF, ECL_READ_ONLY_TOKEN,
                                               cat_single_escape);
	if (token == ECL_NIL) {
		c = ECL_NIL;
	} else if (TOKEN_STRING_FILLP(token) == 1) {
		c = ECL_CODE_CHAR(TOKEN_STRING_CHAR(token,0));
	} else if (TOKEN_STRING_FILLP(token) == 2 && TOKEN_STRING_CHAR_CMP(token,0,'^')) {
		/*	#\^x	*/
		c = ECL_CODE_CHAR(TOKEN_STRING_CHAR(token,1) & 037);
	} else {
		cl_object nc = cl_name_char(token);
		unlikely_if (Null(nc)) {
			FEreader_error("~S is an illegal character name.", in, 1, token);
		}
		c = nc;
	}
	si_put_buffer_string(token);
	ecl_return1(the_env, c);
}

static cl_object
sharp_single_quote_reader(cl_object in, cl_object c, cl_object d)
{
	bool suppress = read_suppress;
	if(d != ECL_NIL && !suppress)
		extra_argument('\'', in, d);
	c = ecl_read_object(in);
	unlikely_if (c == OBJNULL) {
		FEend_of_file(in);
	} else if (suppress) {
		c = ECL_NIL;
	} else {
		c = cl_list(2, @'function', c);
	}
	@(return c)
}

static cl_object
sharp_Y_reader(cl_object in, cl_object c, cl_object d)
{
        cl_index i;
        cl_object x, rv, nth, lex;

	if (d != ECL_NIL && !read_suppress)
		extra_argument('C', in, d);
	x = ecl_read_object(in);
	unlikely_if (x == OBJNULL) {
		FEend_of_file(in);
        }
	if (read_suppress) {
		@(return ECL_NIL);
        }
	unlikely_if (!ECL_CONSP(x) || ecl_length(x) < 5) {
		FEreader_error("Reader macro #Y should be followed by a list",
			       in, 0);
        }

	if (ecl_length(x) == 2) {
		rv = ecl_alloc_object(t_bclosure);
		rv->bclosure.code = ECL_CONS_CAR(x);
		x = ECL_CONS_CDR(x);
		rv->bclosure.lex = ECL_CONS_CAR(x);
                rv->bclosure.entry = _ecl_bclosure_dispatch_vararg;
		@(return rv);
	}

        rv = ecl_alloc_object(t_bytecodes);

        rv->bytecodes.name = ECL_CONS_CAR(x);
        x = ECL_CONS_CDR(x);

        lex = ECL_CONS_CAR(x);
        x = ECL_CONS_CDR(x);

        rv->bytecodes.definition = ECL_CONS_CAR(x);
        x = ECL_CONS_CDR(x);

        nth = ECL_CONS_CAR(x);
        x = ECL_CONS_CDR(x);
        rv->bytecodes.code_size = ecl_to_fix(cl_list_length(nth));
        rv->bytecodes.code = ecl_alloc_atomic(rv->bytecodes.code_size * sizeof(uint16_t));
        for ( i=0; !ecl_endp(nth) ; i++, nth=ECL_CONS_CDR(nth) )
             ((cl_opcode*)(rv->bytecodes.code))[i] = ecl_to_fix(ECL_CONS_CAR(nth));

        nth = ECL_CONS_CAR(x);
        x = ECL_CONS_CDR(x);
        rv->bytecodes.data = nth;

        if (ECL_ATOM(x)) {
                nth = ECL_NIL;
        } else {
                nth = ECL_CONS_CAR(x);
                x = ECL_CONS_CDR(x);
        }
        rv->bytecodes.file = nth;
        if (ECL_ATOM(x)) {
                nth = ecl_make_fixnum(0);
        } else {
                nth = ECL_CONS_CAR(x);
                x = ECL_CONS_CDR(x);
        }
        rv->bytecodes.file_position = nth;

        rv->bytecodes.entry = _ecl_bytecodes_dispatch_vararg;
        @(return rv);
}

#define	QUOTE	1
#define	EVAL	2
#define	LIST	3
#define	LISTX	4
#define	APPEND	5
#define	NCONC	6


/*
 *----------------------------------------------------------------------
 *	Stack of unknown size
 *----------------------------------------------------------------------
 */

cl_object
si_make_backq_vector(cl_object d, cl_object data, cl_object in)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_object v, last;
	cl_index dim, i;
	if (Null(d)) {
		dim = ecl_length(data);
	} else {
		dim = ecl_fixnum(d);
	}
	v = ecl_alloc_simple_vector(dim, ecl_aet_object);
	for (i = 0, last = ECL_NIL; i < dim; i++) {
		if (data == ECL_NIL) {
			/* ... we fill the vector with the last element read (or NIL). */
			for (; i < dim; i++) {
				ecl_aset_unsafe(v, i, last);
			}
			break;
		}
		ecl_aset_unsafe(v, i, last = ecl_car(data));
		data = ECL_CONS_CDR(data);
	}
	unlikely_if (data != ECL_NIL) {
		if (in != ECL_NIL) {
			FEreader_error("Vector larger than specified length,"
				       "~D.", in, 1, d);
		} else {
			FEerror("Vector larger than specified length, ~D", 1, d);
		}
	}
	ecl_return1(the_env, v);
}

static cl_object
sharp_left_parenthesis_reader(cl_object in, cl_object c, cl_object d)
{
	extern int _cl_backq_car(cl_object *);
	const cl_env_ptr the_env = ecl_process_env();
	cl_object v;
	unlikely_if (!Null(d) &&
		     (!ECL_FIXNUMP(d) || ecl_fixnum_minusp(d) ||
		      ecl_fixnum_greater(d, ecl_make_fixnum(ECL_ARRAY_DIMENSION_LIMIT))))
	{
		FEreader_error("Invalid dimension size ~D in #()", in, 1, d);
	}
	if (ecl_fixnum_plusp(ECL_SYM_VAL(the_env, @'si::*backq-level*'))) {
		/* First case: ther might be unquoted elements in the vector.
		 * Then we just create a form that generates the vector.
		 */
		cl_object x = do_read_delimited_list(')', in, 1);
		cl_index a = _cl_backq_car(&x);
		if (a != QUOTE) {
			v = cl_list(2, @'si::unquote', 
				    cl_list(4, @'si::make-backq-vector', d, x, ECL_NIL));
		} else {
			return si_make_backq_vector(d, x, in);
		}
	} else if (read_suppress) {
		/* Second case: *read-suppress* = t, we ignore the data */
		do_read_delimited_list(')', in, 1);
		v = ECL_NIL;
	} else if (Null(d)) {
		/* Third case: no dimension provided. Read a list and
		   coerce it to vector. */
		return si_make_backq_vector(d, do_read_delimited_list(')', in, 1), in);
	} else {
		/* Finally: Both dimension and data are provided. The
		   amount of data cannot exceed the length, but it may
		   be smaller, and in that case...*/
		cl_object last;
		cl_index dim = ecl_fixnum(d), i;
		v = ecl_alloc_simple_vector(dim, ecl_aet_object);
		for (i = 0, last = ECL_NIL;; i++) {
			cl_object aux = ecl_read_object_with_delimiter(in, ')', 0,
                                                                       cat_constituent);
			if (aux == OBJNULL)
				break;
			unlikely_if (i >= dim) {
				FEreader_error("Vector larger than specified length,"
                                               "~D.", in, 1, d);
			}
			ecl_aset_unsafe(v, i, last = aux);
		}
		/* ... we fill the vector with the last element read (or NIL). */
		for (; i < dim; i++) {
			ecl_aset_unsafe(v, i, last);
		}
	}
	@(return v)
}

static cl_object
sharp_asterisk_reader(cl_object in, cl_object c, cl_object d)
{
	cl_env_ptr env = ecl_process_env();
	cl_index sp = ECL_STACK_INDEX(env);
	cl_object last, elt, x;
	cl_index dim, dimcount, i;
	cl_object rtbl = ecl_current_readtable();
	enum ecl_chattrib a;

	if (read_suppress) {
		read_constituent(in);
		@(return ECL_NIL)
	}
	for (dimcount = 0 ;; dimcount++) {
	 	int x = ecl_read_char(in);
		if (x == EOF)
			break;
		a = ecl_readtable_get(rtbl, x, NULL);
		if (a == cat_terminating || a == cat_whitespace) {
			ecl_unread_char(x, in);
			break;
		}
		unlikely_if (a == cat_single_escape || a == cat_multiple_escape ||
                             (x != '0' && x != '1'))
		{
			FEreader_error("Character ~:C is not allowed after #*",
				       in, 1, ECL_CODE_CHAR(x));
		}
		ECL_STACK_PUSH(env, ecl_make_fixnum(x == '1'));
	}
	if (Null(d)) {
		dim = dimcount;
	} else {
                unlikely_if (!ECL_FIXNUMP(d) || ((dim = ecl_fixnum(d)) < 0) ||
                             (dim > ECL_ARRAY_DIMENSION_LIMIT))
                {
                        FEreader_error("Wrong vector dimension size ~D in #*.",
                                       in, 1, d);
                }
		unlikely_if (dimcount > dim)
			FEreader_error("Too many elements in #*.", in, 0);
		unlikely_if (dim && (dimcount == 0))
			FEreader_error("Cannot fill the bit-vector #*.", in, 0);
	}
	last = ECL_STACK_REF(env,-1);
	x = ecl_alloc_simple_vector(dim, ecl_aet_bit);
	for (i = 0; i < dim; i++) {
		elt = (i < dimcount) ? env->stack[sp+i] : last;
		if (elt == ecl_make_fixnum(0))
			x->vector.self.bit[i/CHAR_BIT] &= ~(0200 >> i%CHAR_BIT);
		else
			x->vector.self.bit[i/CHAR_BIT] |= 0200 >> i%CHAR_BIT;
	}
	ECL_STACK_POP_N_UNSAFE(env, dimcount);
	@(return x)
}

static cl_object
sharp_colon_reader(cl_object in, cl_object ch, cl_object d)
{
	cl_object rtbl = ecl_current_readtable();
	enum ecl_chattrib a;
	bool escape_flag;
	int c;
	cl_object output, token;

	if (d != ECL_NIL && !read_suppress)
		extra_argument(':', in, d);
	c = ecl_read_char_noeof(in);
	a = ecl_readtable_get(rtbl, c, NULL);
	escape_flag = FALSE;
	token = si_get_buffer_string();
	goto L;
	for (;;) {
		ecl_string_push_extend(token, c);
	K:
		c = ecl_read_char(in);
		if (c == EOF)
			goto M;
		a = ecl_readtable_get(rtbl, c, NULL);
	L:
		if (a == cat_single_escape) {
			c = ecl_read_char_noeof(in);
			a = cat_constituent;
			escape_flag = TRUE;
		} else if (a == cat_multiple_escape) {
			escape_flag = TRUE;
			for (;;) {
				c = ecl_read_char_noeof(in);
				a = ecl_readtable_get(rtbl, c, NULL);
				if (a == cat_single_escape) {
					c = ecl_read_char_noeof(in);
					a = cat_constituent;
				} else if (a == cat_multiple_escape)
					break;
				ecl_string_push_extend(token, c);
			}
			goto K;
		} else if (ecl_lower_case_p(c))
			c = ecl_char_upcase(c);
		if (a == cat_whitespace || a == cat_terminating)
			break;
	}
	ecl_unread_char(c, in);

M:
	if (read_suppress) {
		output = ECL_NIL;
	} else {
		output = cl_make_symbol(token);
	}
	si_put_buffer_string(token);
	@(return output)
}

static cl_object
sharp_dot_reader(cl_object in, cl_object c, cl_object d)
{
	const cl_env_ptr env = ecl_process_env();
	if (d != ECL_NIL && !read_suppress)
		extra_argument('.', in, d);
	c = ecl_read_object(in);
	unlikely_if (c == OBJNULL)
		FEend_of_file(in);
	if (read_suppress)
		@(return ECL_NIL);
	unlikely_if (ecl_symbol_value(@'*read-eval*') == ECL_NIL)
		FEreader_error("Cannot evaluate the form #.~A", in, 1, c);
        /* FIXME! We should do something here to ensure that the #.
         * only uses the #n# that have been defined */
        c = patch_sharp(env, c);
	c = si_eval_with_env(1, c);
	@(return c)
}

static cl_object
read_number(cl_object in, int radix, cl_object macro_char)
{
	cl_index i;
	cl_object x;
	cl_object token = read_constituent(in);
	if (token == ECL_NIL) {
		x = ECL_NIL;
	} else {
		x = ecl_parse_number(token, 0, TOKEN_STRING_FILLP(token), &i, radix);
		unlikely_if (x == OBJNULL || x == ECL_NIL ||
                             i != TOKEN_STRING_FILLP(token))
                {
			FEreader_error("Cannot parse the #~A readmacro.", in, 1,
				       macro_char);
		}
		unlikely_if (cl_rationalp(x) == ECL_NIL) {
			FEreader_error("The float ~S appeared after the #~A readmacro.",
				       in, 2, x, macro_char);
		}
		si_put_buffer_string(token);
	}
	return x;
}

static cl_object
sharp_B_reader(cl_object in, cl_object c, cl_object d)
{
	if(d != ECL_NIL && !read_suppress)
		extra_argument('B', in, d);
	@(return (read_number(in, 2, ECL_CODE_CHAR('B'))))
}

static cl_object
sharp_O_reader(cl_object in, cl_object c, cl_object d)
{
	if(d != ECL_NIL && !read_suppress)
		extra_argument('O', in, d);
	@(return (read_number(in, 8, ECL_CODE_CHAR('O'))))
}

static cl_object
sharp_X_reader(cl_object in, cl_object c, cl_object d)
{
	if(d != ECL_NIL && !read_suppress)
		extra_argument('X', in, d);
	@(return (read_number(in, 16, ECL_CODE_CHAR('X'))))
}

static cl_object
sharp_R_reader(cl_object in, cl_object c, cl_object d)
{
	int radix;
	if (read_suppress) {
		radix = 10;
        } else unlikely_if (!ECL_FIXNUMP(d)) {
		FEreader_error("No radix was supplied in the #R readmacro.", in, 0);
        } else {
		radix = ecl_fixnum(d);
		unlikely_if (radix > 36 || radix < 2) {
			FEreader_error("~S is an illegal radix.", in, 1, d);
                }
	}
	@(return (read_number(in, radix, ECL_CODE_CHAR('R'))))
}

#define sharp_A_reader void_reader
#define sharp_S_reader void_reader

static cl_object
sharp_eq_reader(cl_object in, cl_object c, cl_object d)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_object pair, value;
	cl_object sharp_eq_context = ECL_SYM_VAL(the_env, @'si::*sharp-eq-context*');

	if (read_suppress) @(return);
	unlikely_if (Null(d)) {
		FEreader_error("The #= readmacro requires an argument.", in, 0);
        }
	unlikely_if (ecl_assq(d, sharp_eq_context) != ECL_NIL) {
		FEreader_error("Duplicate definitions for #~D=.", in, 1, d);
        }
        pair = CONS(d, OBJNULL);
	ECL_SETQ(the_env, @'si::*sharp-eq-context*', CONS(pair, sharp_eq_context));
	value = ecl_read_object(in);
	unlikely_if (value == pair) {
		FEreader_error("#~D# is defined by itself.", in, 1, d);
        }
	ECL_RPLACD(pair, value);
	ecl_return1(the_env, value);
}

static cl_object
sharp_sharp_reader(cl_object in, cl_object c, cl_object d)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_object pair;

	if (read_suppress)
                ecl_return1(the_env, ECL_NIL);
	unlikely_if (Null(d)) {
		FEreader_error("The ## readmacro requires an argument.", in, 0);
        }
	pair = ecl_assq(d, ECL_SYM_VAL(the_env, @'si::*sharp-eq-context*'));
	unlikely_if (pair == ECL_NIL) {
                FEreader_error("#~D# is undefined.", in, 1, d);
        } else {
		cl_object value = ECL_CONS_CDR(pair);
		ecl_return1(the_env, (value == OBJNULL)? pair : value);
	}
}

static cl_object
do_patch_sharp(cl_object x, cl_object table)
#if 1
{
	/* The hash table maintains an association as follows:
	 *
	 * [1] object -> itself
	 *	The object has been processed by patch_sharp, us as it is.
	 * [2] object -> nothing
	 *	The object has to be processed by do_patch_sharp.
	 * [3] (# . object) -> object
	 *	This is the value of a #n# statement. The object migt
	 *	or might not yet be processed by do_patch_sharp().
	 */
 AGAIN:
	switch (ecl_t_of(x)) {
	case t_list: {
		cl_object y;
                if (Null(x))
                        return x;
		y = ecl_gethash_safe(x, table, table);
                if (y == table) {
			/* case [2] */
                        break;
		} else if (y == x) {
			/* case [1] */
			return x;
		} else {
			/* case [3] */
			x = y;
			goto AGAIN;
		}
	}
	case t_vector:
	case t_array:
	case t_complex:
        case t_bclosure:
        case t_bytecodes: {
                cl_object y = ecl_gethash_safe(x, table, table);
		if (y == table) {
			/* case [2] */
			break;
		}
		/* it can only be case [1] */
        }
	default:
                return x;
	}
	/* We eagerly mark the object as processed, to avoid infinite
	 * recursion. */
        _ecl_sethash(x, table, x);
	switch (ecl_t_of(x)) {
	case t_list:
                ECL_RPLACA(x, do_patch_sharp(ECL_CONS_CAR(x), table));
                ECL_RPLACD(x, do_patch_sharp(ECL_CONS_CDR(x), table));
		break;
	case t_vector:
		if (x->vector.elttype == ecl_aet_object) {
			cl_index i;
			for (i = 0;  i < x->vector.fillp;  i++)
				x->vector.self.t[i] =
                                        do_patch_sharp(x->vector.self.t[i], table);
		}
		break;
	case t_array:
		if (x->vector.elttype == ecl_aet_object) {
			cl_index i, j = x->array.dim;
			for (i = 0;  i < j;  i++)
				x->array.self.t[i] =
                                        do_patch_sharp(x->array.self.t[i], table);
		}
		break;
	case t_complex: {
		cl_object r = do_patch_sharp(x->complex.real, table);
		cl_object i = do_patch_sharp(x->complex.imag, table);
		if (r != x->complex.real || i != x->complex.imag) {
			cl_object c = ecl_make_complex(r, i);
			x->complex = c->complex;
		}
                break;
	}
        case t_bclosure: {
                x->bclosure.lex = do_patch_sharp(x->bclosure.lex, table);
                x = x->bclosure.code = do_patch_sharp(x->bclosure.code, table);
                break;
        }
        case t_bytecodes: {
                x->bytecodes.name = do_patch_sharp(x->bytecodes.name, table);
                x->bytecodes.definition = do_patch_sharp(x->bytecodes.definition, table);
		x->bytecodes.data = do_patch_sharp(x->bytecodes.data, table);
                break;
        }
	default:;
	}
        return x;
}
#else
{
	switch (ecl_t_of(x)) {
	case t_list:
                if (Null(x))
                        return x;
	case t_vector:
	case t_array:
	case t_complex:
        case t_bclosure:
        case t_bytecodes: {
                cl_object y = ecl_gethash_safe(x, table, table);
                if (y == table)
                        break;
                x = y;
        }
	default:
                return x;
	}
	switch (ecl_t_of(x)) {
	case t_list:
                ECL_RPLACA(x, do_patch_sharp(ECL_CONS_CAR(x), table));
                ECL_RPLACD(x, do_patch_sharp(ECL_CONS_CDR(x), table));
		break;
	case t_vector:
		if (x->vector.elttype == ecl_aet_object) {
			cl_index i;
			for (i = 0;  i < x->vector.fillp;  i++)
				x->vector.self.t[i] =
                                        do_patch_sharp(x->vector.self.t[i], table);
		}
		break;
	case t_array:
		if (x->vector.elttype == ecl_aet_object) {
			cl_index i, j = x->array.dim;
			for (i = 0;  i < j;  i++)
				x->array.self.t[i] =
                                        do_patch_sharp(x->array.self.t[i], table);
		}
		break;
	case t_complex: {
		cl_object r = do_patch_sharp(x->complex.real, table);
		cl_object i = do_patch_sharp(x->complex.imag, table);
		if (r != x->complex.real || i != x->complex.imag) {
			cl_object c = ecl_make_complex(r, i);
			x->complex = c->complex;
		}
                break;
	}
        case t_bclosure: {
                x->bclosure.lex = do_patch_sharp(x->bclosure.lex, table);
                x = x->bclosure.code = do_patch_sharp(x->bclosure.code, table);
                break;
        }
        case t_bytecodes: {
                x->bytecodes.name = do_patch_sharp(x->bytecodes.name, table);
                x->bytecodes.definition = do_patch_sharp(x->bytecodes.definition, table);
		x->bytecodes.data = do_patch_sharp(x->bytecodes.data, table);
                break;
        }
	default:;
	}
        _ecl_sethash(x, table, x);
        return x;
}
#endif

static cl_object
patch_sharp(const cl_env_ptr the_env, cl_object x)
{
	cl_object pairs = ECL_SYM_VAL(the_env, @'si::*sharp-eq-context*');
	if (pairs == ECL_NIL) {
		return x;
	} else {
		cl_object table = 
			cl__make_hash_table(@'eq', ecl_make_fixnum(20), /* size */
					    cl_core.rehash_size,
                                    cl_core.rehash_threshold);
		do {
			cl_object pair = ECL_CONS_CAR(pairs);
			_ecl_sethash(pair, table, ECL_CONS_CDR(pair));
			pairs = ECL_CONS_CDR(pairs);
		} while (pairs != ECL_NIL);
		return do_patch_sharp(x, table);
	}
}

#define sharp_plus_reader void_reader
#define sharp_minus_reader void_reader
#define sharp_less_than_reader void_reader
#define sharp_whitespace_reader void_reader
#define sharp_right_parenthesis_reader void_reader

static cl_object
sharp_vertical_bar_reader(cl_object in, cl_object ch, cl_object d)
{
	int c;
	int level = 0;

	if (d != ECL_NIL && !read_suppress)
		extra_argument('|', in, d);
	for (;;) {
		c = ecl_read_char_noeof(in);
	L:
		if (c == '#') {
			c = ecl_read_char_noeof(in);
			if (c == '|')
				level++;
		} else if (c == '|') {
			c = ecl_read_char_noeof(in);
			if (c == '#') {
				if (level == 0)
					break;
				else
					--level;
			} else
				goto L;
		}
	}
	@(return)
	/*  no result  */
}

static cl_object
default_dispatch_macro_fun(cl_object in, cl_object c, cl_object d)
{
	FEreader_error("No dispatch function defined for character ~s.", in, 1, c);
}

/*
	#P" ... " returns the pathname with namestring ... .
*/
static cl_object
sharp_P_reader(cl_object in, cl_object c, cl_object d)
{
	bool suppress = read_suppress;
	if (d != ECL_NIL && !suppress)
		extra_argument('P', in, d);
	d = ecl_read_object(in);
	if (suppress) {
		d = ECL_NIL;
	} else {
		d = cl_parse_namestring(3, d, ECL_NIL, ECL_NIL);
	}
	@(return d)
}

/*
	#$ fixnum returns a random-state with the fixnum
	as its content.
*/
static cl_object
sharp_dollar_reader(cl_object in, cl_object c, cl_object d)
{
	cl_object rs;
	if (d != ECL_NIL && !read_suppress)
		extra_argument('$', in, d);
	c = ecl_read_object(in);
	rs = ecl_alloc_object(t_random);
	rs->random.value = c;
	@(return rs)
}

/*
	readtable routines
*/

static void ECL_INLINE
assert_type_readtable(cl_object function, cl_narg narg, cl_object p)
{
	unlikely_if (!ECL_READTABLEP(p)) {
		FEwrong_type_nth_arg(function, narg, p, @[readtable]);
        }
}


cl_object
ecl_copy_readtable(cl_object from, cl_object to)
{
	struct ecl_readtable_entry *from_rtab, *to_rtab;
	cl_index i;
	size_t entry_bytes = sizeof(struct ecl_readtable_entry);
	size_t total_bytes = entry_bytes * RTABSIZE;
	cl_object output;

	assert_type_readtable(@[copy-readtable], 1, from);
	/* For the sake of garbage collector and thread safety we
	 * create an incomplete object and only copy to the destination
	 * at the end in a more or less "atomic" (meaning "fast") way.
	 */
	output = ecl_alloc_object(t_readtable);
        output->readtable.locked = 0;
	output->readtable.table = to_rtab = (struct ecl_readtable_entry *)
		ecl_alloc_align(total_bytes, entry_bytes);
	from_rtab = from->readtable.table;
	memcpy(to_rtab, from_rtab, total_bytes);
	for (i = 0;  i < RTABSIZE;  i++) {
		cl_object d = from_rtab[i].dispatch;
		if (ECL_HASH_TABLE_P(d)) {
			d = si_copy_hash_table(d);
		}
		to_rtab[i].dispatch = d;
	}
	output->readtable.read_case = from->readtable.read_case;
#ifdef ECL_UNICODE
	if (!Null(from->readtable.hash)) {
		output->readtable.hash = si_copy_hash_table(from->readtable.hash);
	} else {
		output->readtable.hash = ECL_NIL;
	}
#endif
	if (!Null(to)) {
                assert_type_readtable(@[copy-readtable], 2, to);
		to->readtable = output->readtable;
		output = to;
	}
	return output;
}

cl_object
ecl_current_readtable(void)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_object r;

	/* INV: *readtable* always has a value */
	r = ECL_SYM_VAL(the_env, @'*readtable*');
	unlikely_if (!ECL_READTABLEP(r)) {
		ECL_SETQ(the_env, @'*readtable*', cl_core.standard_readtable);
		FEerror("The value of *READTABLE*, ~S, was not a readtable.",
			1, r);
	}
	return r;
}

int
ecl_current_read_base(void)
{
	const cl_env_ptr the_env = ecl_process_env();
	/* INV: *READ-BASE* always has a value */
	cl_object x = ECL_SYM_VAL(the_env, @'*read-base*');
        cl_fixnum b;

        unlikely_if (!ECL_FIXNUMP(x) || ((b = ecl_fixnum(x)) < 2) || (b > 36))
        {
                ECL_SETQ(the_env, @'*read-base*', ecl_make_fixnum(10));
                FEerror("The value of *READ-BASE*~&  ~S~%"
                        "is not in the range (INTEGER 2 36)", 1, x);
        }
	return b;
}

char
ecl_current_read_default_float_format(void)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_object x;

	/* INV: *READ-DEFAULT-FLOAT-FORMAT* is always bound to something */
	x = ECL_SYM_VAL(the_env, @'*read-default-float-format*');
	if (x == @'single-float' || x == @'short-float')
		return 'F';
	if (x == @'double-float')
		return 'D';
	if (x == @'long-float') {
#ifdef ECL_LONG_FLOAT
		return 'L';
#else
		return 'D';
#endif
	}
	ECL_SETQ(the_env, @'*read-default-float-format*', @'single-float');
	FEerror("The value of *READ-DEFAULT-FLOAT-FORMAT*~& ~S~%"
                "is not one of (SINGLE-FLOAT SHORT-FLOAT DOUBLE-FLOAT LONG-FLOAT)",
		1, x);
}

static cl_object
stream_or_default_input(cl_object stream)
{
	const cl_env_ptr the_env = ecl_process_env();
	if (Null(stream))
		return ECL_SYM_VAL(the_env, @'*standard-input*');
	if (stream == ECL_T)
		return ECL_SYM_VAL(the_env, @'*terminal-io*');
	return stream;
}

@(defun read (&optional (strm ECL_NIL) (eof_errorp ECL_T) eof_value recursivep)
	cl_object x;
@
	strm = stream_or_default_input(strm);
	if (Null(recursivep)) {
		x = ecl_read_object_non_recursive(strm);
	} else {
		x = ecl_read_object(strm);
	}
	if (x == OBJNULL) {
		if (Null(eof_errorp))
			@(return eof_value)
		FEend_of_file(strm);
	}
	/* Skip whitespace characters, but stop at beginning of new line or token */
	if (Null(recursivep)) {
		cl_object rtbl = ecl_current_readtable();
		int c = ecl_read_char(strm);
		if (c != EOF && (ecl_readtable_get(rtbl, c, NULL) != cat_whitespace)) {
			ecl_unread_char(c, strm);
		}
	}
	@(return x)
@)

@(defun read_preserving_whitespace
	(&optional (strm ECL_NIL)
		   (eof_errorp ECL_T)
		   eof_value
		   recursivep)
	cl_object x;
@
	strm = stream_or_default_input(strm);
	if (Null(recursivep)) {
		x = ecl_read_object_non_recursive(strm);
	} else {
		x = ecl_read_object(strm);
	}
	if (x == OBJNULL) {
		if (Null(eof_errorp))
			@(return eof_value)
		FEend_of_file(strm);
	}
	@(return x)
@)

static cl_object
do_read_delimited_list(int d, cl_object in, bool proper_list)
{
	int after_dot = 0;
	bool suppress = read_suppress;
	cl_object x, y = ECL_NIL;
	cl_object *p = &y;
	do {
		x = ecl_read_object_with_delimiter(in, d, ECL_READ_LIST_DOT,
						   cat_constituent);
		if (x == OBJNULL) {
			/* End of the list. */
			unlikely_if (after_dot == 1) {
				/* Something like (1 . ) */
				FEreader_error("Object missing after a list dot", in, 0);
			}
			return y;
		} else if (x == @'si::.') {
			unlikely_if (proper_list) {
				FEreader_error("A dotted list was found where a proper list was expected.", in, 0);
			}
			unlikely_if (p == &y) {
				/* Something like (. 2) */
				FEreader_error("A dot appeared after a left parenthesis.", in, 0);
			}
			unlikely_if (after_dot) {
				/* Something like (1 . . 2) */
				FEreader_error("Two dots appeared consecutively.", in, 0);
			}
			after_dot = 1;
		} else if (after_dot) {
			unlikely_if (after_dot++ > 1) {
				/* Something like (1 . 2 3) */
				FEreader_error("Too many objects after a list dot", in, 0);
			}
			*p = x;
		} else if (!suppress) {
			*p = ecl_list1(x);
			p = &ECL_CONS_CDR(*p);
		}
	} while (1);
}

@(defun read_delimited_list (d &optional (strm ECL_NIL) recursivep)
	cl_object l;
	int delimiter;
@
	delimiter = ecl_char_code(d);
	strm = stream_or_default_input(strm);
	if (!Null(recursivep)) {
		l = do_read_delimited_list(delimiter, strm, 1);
	} else {
		ecl_bds_bind(the_env, @'si::*sharp-eq-context*', ECL_NIL);
		ecl_bds_bind(the_env, @'si::*backq-level*', ecl_make_fixnum(0));
		l = do_read_delimited_list(delimiter, strm, 1);
		l = patch_sharp(the_env, l);
		ecl_bds_unwind_n(the_env, 2);
	}
	@(return l)
@)

@(defun read_line (&optional (strm ECL_NIL) (eof_errorp ECL_T) eof_value recursivep)
	int c;
	cl_object token, value0, value1;
@
	strm = stream_or_default_input(strm);
#ifdef ECL_CLOS_STREAMS
        if (!ECL_ANSI_STREAM_P(strm)) {
		value0 = _ecl_funcall2(@'gray::stream-read-line', strm);
		value1 = ecl_nth_value(the_env, 1);
		if (!Null(value1)) {
			if (!Null(eof_errorp))
				FEend_of_file(strm);
			value0 = eof_value;
			value1 = ECL_T;
		}
		goto OUTPUT;
	}
#endif
	token = si_get_buffer_string();
	do {
		c = ecl_read_char(strm);
		if (c == EOF || c == '\n')
			break;
		ecl_string_push_extend(token, c);
	} while(1);
	if (c == EOF && TOKEN_STRING_FILLP(token) == 0) {
		if (!Null(eof_errorp))
			FEend_of_file(strm);
		value0 = eof_value;
		value1 = ECL_T;
	} else {
#ifdef ECL_NEWLINE_IS_CRLF	/* From \r\n, ignore \r */
		if (TOKEN_STRING_FILLP(token) > 0 &&
		    TOKEN_STRING_CHAR_CMP(token,TOKEN_STRING_FILLP(token)-1,'\r'))
			TOKEN_STRING_FILLP(token)--;
#endif
#ifdef ECL_NEWLINE_IS_LFCR	/* From \n\r, ignore \r */
		ecl_read_char(strm);
#endif
		value0 = cl_copy_seq(token);
		value1 = (c == EOF? ECL_T : ECL_NIL);
	}
	si_put_buffer_string(token);
 OUTPUT:
	@(return value0 value1)
@)

@(defun read-char (&optional (strm ECL_NIL) (eof_errorp ECL_T) eof_value recursivep)
	int c;
	cl_object output;
@
	strm = stream_or_default_input(strm);
	c = ecl_read_char(strm);
	if (c != EOF)
		output = ECL_CODE_CHAR(c);
	else if (Null(eof_errorp))
		output = eof_value;
	else
		FEend_of_file(strm);
	@(return output)
@)

@(defun unread_char (c &optional (strm ECL_NIL))
@
	/* INV: unread_char() checks the type `c' */
	strm = stream_or_default_input(strm);
	ecl_unread_char(ecl_char_code(c), strm);
	@(return ECL_NIL)
@)

@(defun peek-char (&optional peek_type (strm ECL_NIL) (eof_errorp ECL_T) eof_value recursivep)
	int c;
	cl_object rtbl = ecl_current_readtable();
@
	strm = stream_or_default_input(strm);
	c = ecl_peek_char(strm);
	if (c != EOF && !Null(peek_type)) {
		if (peek_type == ECL_T) {
			do {
				/* If the character is not a whitespace, output */
				if (ecl_readtable_get(rtbl, c, NULL) != cat_whitespace)
					break;
				/* Otherwise, read the whitespace and peek the
				 * next character */
				ecl_read_char(strm);
				c = ecl_peek_char(strm);
			} while (c != EOF);
		} else {
			do {
				/* If the character belongs to the given class,
				 * we're done. */
				if (ecl_char_eq(ECL_CODE_CHAR(c), peek_type))
					break;
				/* Otherwise, consume the character and
				 * peek the next one. */
				ecl_read_char(strm);
				c = ecl_peek_char(strm);
			} while (c != EOF);
		}
	}
	if (c != EOF) {
		eof_value = ECL_CODE_CHAR(c);
	} else if (!Null(eof_errorp)) {
		FEend_of_file(strm);
	}
	@(return eof_value)
@)

@(defun listen (&optional (strm ECL_NIL))
@
	strm = stream_or_default_input(strm);
	@(return ((ecl_listen_stream(strm) == ECL_LISTEN_AVAILABLE)? ECL_T : ECL_NIL))
@)

@(defun read_char_no_hang (&optional (strm ECL_NIL) (eof_errorp ECL_T) eof_value recursivep)
	int f;
@
	strm = stream_or_default_input(strm);
#ifdef ECL_CLOS_STREAMS
	if (!ECL_ANSI_STREAM_P(strm)) {
		cl_object output =
			_ecl_funcall2(@'gray::stream-read-char-no-hang', strm);
		if (output == @':eof')
			goto END_OF_FILE;
		@(return output);
	}
#endif
	f = ecl_listen_stream(strm);
	if (f == ECL_LISTEN_AVAILABLE) {
		int c = ecl_read_char(strm);
		if (c != EOF) {
			@(return ECL_CODE_CHAR(c));
		}
	} else if (f == ECL_LISTEN_NO_CHAR) {
		@(return @'nil');
	}
	/* We reach here if there was an EOF */
  END_OF_FILE:
	if (Null(eof_errorp))
		@(return eof_value)
	else
		FEend_of_file(strm);
@)

@(defun clear_input (&optional (strm ECL_NIL))
@
	strm = stream_or_default_input(strm);
	ecl_clear_input(strm);
	@(return ECL_NIL)
@)

@(defun read_byte (binary_input_stream &optional (eof_errorp ECL_T) eof_value)
	cl_object c;
@
	c = ecl_read_byte(binary_input_stream);
	if (c == ECL_NIL) {
		if (Null(eof_errorp))
			@(return eof_value)
		else
			FEend_of_file(binary_input_stream);
	}
	@(return c)
@)

@(defun read_sequence (sequence stream &key (start ecl_make_fixnum(0)) end)
@
#ifdef ECL_CLOS_STREAMS
	if (!ECL_ANSI_STREAM_P(stream))
		return funcall(5, @'gray::stream-read-sequence', stream, sequence, start, end);
	else
#endif
		return si_do_read_sequence(sequence, stream, start, end);
@)


@(defun copy_readtable (&o (from ecl_current_readtable()) to)
@
	if (Null(from)) {
		to = ecl_copy_readtable(cl_core.standard_readtable, to);
	} else {
		to = ecl_copy_readtable(from, to);
	}
	@(return to)
@)

cl_object
cl_readtable_case(cl_object r)
{
        assert_type_readtable(@[readtable-case], 1, r);
	switch (r->readtable.read_case) {
	case ecl_case_upcase: r = @':upcase'; break;
	case ecl_case_downcase: r = @':downcase'; break;
	case ecl_case_invert: r = @':invert'; break;
	case ecl_case_preserve: r = @':preserve';
	}
	@(return r)
}

static void
error_locked_readtable(cl_object r)
{
        cl_error(2,
		 make_constant_base_string("Cannot modify locked readtable ~A."),
                 r);
}

cl_object
si_readtable_case_set(cl_object r, cl_object mode)
{
        assert_type_readtable(@[readtable-case], 1, r);
        if (r->readtable.locked) {
                error_locked_readtable(r);
        }
	if (mode == @':upcase') {
		r->readtable.read_case = ecl_case_upcase;
	} else if (mode == @':downcase') {
		r->readtable.read_case = ecl_case_downcase;
	} else if (mode == @':preserve') {
		r->readtable.read_case = ecl_case_preserve;
	} else if (mode == @':invert') {
		r->readtable.read_case = ecl_case_invert;
	} else {
                const char *type = "(member :upcase :downcase :preserve :invert)";
		FEwrong_type_nth_arg(@[si::readtable-case-set], 2,
                                     mode, ecl_read_from_cstring(type));
	}
	@(return mode)
}

cl_object
cl_readtablep(cl_object readtable)
{
	@(return (ECL_READTABLEP(readtable) ? ECL_T : ECL_NIL))
}

int
ecl_readtable_get(cl_object readtable, int c, cl_object *macro_or_table)
{
	cl_object m;
	enum ecl_chattrib cat;
#ifdef ECL_UNICODE
	if (c >= RTABSIZE) {
		cl_object hash = readtable->readtable.hash;
		cat = cat_constituent;
		m = ECL_NIL;
		if (!Null(hash)) {
			cl_object pair = ecl_gethash_safe(ECL_CODE_CHAR(c), hash, ECL_NIL);
			if (!Null(pair)) {
				cat = ecl_fixnum(ECL_CONS_CAR(pair));
				m = ECL_CONS_CDR(pair);
			}
		}
	} else
#endif
	{
		m = readtable->readtable.table[c].dispatch;
		cat = readtable->readtable.table[c].syntax_type;
	}
	if (macro_or_table) *macro_or_table = m;
	return cat;
}

void
ecl_readtable_set(cl_object readtable, int c, enum ecl_chattrib cat,
		   cl_object macro_or_table)
{
        if (readtable->readtable.locked) {
                error_locked_readtable(readtable);
        }
#ifdef ECL_UNICODE
	if (c >= RTABSIZE) {
		cl_object hash = readtable->readtable.hash;
		if (Null(hash)) {
			hash = cl__make_hash_table(@'eql', ecl_make_fixnum(128),
                                                   cl_core.rehash_size,
                                                   cl_core.rehash_threshold);
			readtable->readtable.hash = hash;
		}
		_ecl_sethash(ECL_CODE_CHAR(c), hash,
			    CONS(ecl_make_fixnum(cat), macro_or_table));
	} else
#endif
	{
		readtable->readtable.table[c].dispatch = macro_or_table;
		readtable->readtable.table[c].syntax_type = cat;
	}
}

bool
ecl_invalid_character_p(int c)
{
	return (c <= 32) || (c == 127);
}

@(defun set_syntax_from_char (tochr fromchr
			      &o (tordtbl ecl_current_readtable())
				 fromrdtbl)
	enum ecl_chattrib cat;
	cl_object dispatch;
	cl_fixnum fc, tc;
@
        if (tordtbl->readtable.locked) {
                error_locked_readtable(tordtbl);
        }
	if (Null(fromrdtbl))
		fromrdtbl = cl_core.standard_readtable;
        assert_type_readtable(@[readtable-case], 1, tordtbl);
        assert_type_readtable(@[readtable-case], 2, fromrdtbl);
	fc = ecl_char_code(fromchr);
	tc = ecl_char_code(tochr);

	cat = ecl_readtable_get(fromrdtbl, fc, &dispatch);
	if (ECL_READTABLEP(dispatch)) {
		dispatch = si_copy_hash_table(dispatch);
	}
	ecl_readtable_set(tordtbl, tc, cat, dispatch);
	@(return ECL_T)
@)

@(defun set_macro_character (c function &optional non_terminating_p
			     (readtable ecl_current_readtable()))
@
	ecl_readtable_set(readtable, ecl_char_code(c),
			  Null(non_terminating_p)?
			  cat_terminating :
			  cat_non_terminating,
			  function);
	@(return ECL_T)
@)

@(defun get_macro_character (c &optional (readtable ecl_current_readtable()))
	enum ecl_chattrib cat;
	cl_object dispatch;
@
	if (Null(readtable))
		readtable = cl_core.standard_readtable;
	cat = ecl_readtable_get(readtable, ecl_char_code(c), &dispatch);
        if (ECL_HASH_TABLE_P(dispatch))
		dispatch = cl_core.dispatch_reader;
	@(return dispatch ((cat == cat_non_terminating)? ECL_T : ECL_NIL))
@)

@(defun make_dispatch_macro_character (chr
	&optional non_terminating_p (readtable ecl_current_readtable()))
	enum ecl_chattrib cat;
	cl_object table;
	int c;
@
        assert_type_readtable(@[make-dispatch-macro-character], 3, readtable);
	c = ecl_char_code(chr);
	cat = Null(non_terminating_p)? cat_terminating : cat_non_terminating;
	table = cl__make_hash_table(@'eql', ecl_make_fixnum(128),
                                    cl_core.rehash_size,
                                    cl_core.rehash_threshold);
	ecl_readtable_set(readtable, c, cat, table);
	@(return ECL_T)
@)

@(defun set_dispatch_macro_character (dspchr subchr fnc
	&optional (readtable ecl_current_readtable()))
	cl_object table;
	cl_fixnum subcode;
@
        assert_type_readtable(@[set-dispatch-macro-character], 4, readtable);
	ecl_readtable_get(readtable, ecl_char_code(dspchr), &table);
        unlikely_if (readtable->readtable.locked) {
                error_locked_readtable(readtable);
        }
        unlikely_if (!ECL_HASH_TABLE_P(table)) {
		FEerror("~S is not a dispatch character.", 1, dspchr);
	}
	subcode = ecl_char_code(subchr);
	if (Null(fnc)) {
		ecl_remhash(ECL_CODE_CHAR(subcode), table);
	} else {
		_ecl_sethash(ECL_CODE_CHAR(subcode), table, fnc);
	}
	if (ecl_lower_case_p(subcode)) {
		subcode = ecl_char_upcase(subcode);
	} else if (ecl_upper_case_p(subcode)) {
		subcode = ecl_char_downcase(subcode);
	}
	if (Null(fnc)) {
		ecl_remhash(ECL_CODE_CHAR(subcode), table);
	} else {
		_ecl_sethash(ECL_CODE_CHAR(subcode), table, fnc);
	}
	@(return ECL_T)
@)

@(defun get_dispatch_macro_character (dspchr subchr
	&optional (readtable ecl_current_readtable()))
	cl_object table;
	cl_fixnum c;
@
	if (Null(readtable)) {
		readtable = cl_core.standard_readtable;
	}
        assert_type_readtable(@[get-dispatch-macro-character], 3, readtable);
	c = ecl_char_code(dspchr);
	ecl_readtable_get(readtable, c, &table);
	unlikely_if (!ECL_HASH_TABLE_P(table)) {
		FEerror("~S is not a dispatch character.", 1, dspchr);
	}
	c = ecl_char_code(subchr);

	/* Since macro characters may take a number as argument, it is
	   not allowed to turn digits into dispatch macro characters */
	if (ecl_digitp(c, 10) >= 0)
		@(return ECL_NIL)
	@(return ecl_gethash_safe(subchr, table, ECL_NIL))
@)

cl_object
si_standard_readtable()
{
	@(return cl_core.standard_readtable)
}

@(defun ext::readtable-lock (r &optional yesno)
	cl_object output;
@
	assert_type_readtable(@[ext::readtable-lock], 1, r);
        output = (r->readtable.locked)? ECL_T : ECL_NIL;
	if (narg > 1) {
                r->readtable.locked = !Null(yesno);
        }
        @(return output)
@)

static void
extra_argument(int c, cl_object stream, cl_object d)
{
	FEreader_error("~S is an extra argument for the #~C readmacro.",
		       stream, 2, d, ECL_CODE_CHAR(c));
}


#define	make_cf2(f)	ecl_make_cfun((f), ECL_NIL, NULL, 2)
#define	make_cf3(f)	ecl_make_cfun((f), ECL_NIL, NULL, 3)

void
init_read(void)
{
	struct ecl_readtable_entry *rtab;
	cl_object r;
	int i;

	cl_core.standard_readtable = r = ecl_alloc_object(t_readtable);
        r->readtable.locked = 0;
	r->readtable.read_case = ecl_case_upcase;
	r->readtable.table = rtab
	= (struct ecl_readtable_entry *)
		ecl_alloc(RTABSIZE * sizeof(struct ecl_readtable_entry));
	for (i = 0;  i < RTABSIZE;  i++) {
		rtab[i].syntax_type = cat_constituent;
		rtab[i].dispatch = ECL_NIL;
	}
#ifdef ECL_UNICODE
	r->readtable.hash = ECL_NIL;
#endif

	cl_core.dispatch_reader = make_cf2(dispatch_reader_fun);

	ecl_readtable_set(r, '\t', cat_whitespace, ECL_NIL);
	ecl_readtable_set(r, '\n', cat_whitespace, ECL_NIL);
	ecl_readtable_set(r, '\f', cat_whitespace, ECL_NIL);
	ecl_readtable_set(r, '\r', cat_whitespace, ECL_NIL);
	ecl_readtable_set(r, ' ', cat_whitespace, ECL_NIL);

	ecl_readtable_set(r, '"', cat_terminating,
			  make_cf2(double_quote_reader));

	ecl_readtable_set(r, '\'', cat_terminating,
			  make_cf2(single_quote_reader));
	ecl_readtable_set(r, '(', cat_terminating,
			  make_cf2(left_parenthesis_reader));
	ecl_readtable_set(r, ')', cat_terminating,
			  make_cf2(right_parenthesis_reader));
	ecl_readtable_set(r, ',', cat_terminating,
			  make_cf2(comma_reader));
	ecl_readtable_set(r, ';', cat_terminating,
			  make_cf2(semicolon_reader));
	ecl_readtable_set(r, '\\', cat_single_escape, ECL_NIL);
	ecl_readtable_set(r, '`', cat_terminating,
			  make_cf2(backquote_reader));
	ecl_readtable_set(r, '|', cat_multiple_escape, ECL_NIL);

	cl_core.default_dispatch_macro = make_cf3(default_dispatch_macro_fun);

	cl_make_dispatch_macro_character(3, ECL_CODE_CHAR('#'),
					 ECL_T /* non terminating */, r);

	cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('C'),
					make_cf3(sharp_C_reader), r);
	cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('\\'),
					make_cf3(sharp_backslash_reader), r);
	cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('\''),
					make_cf3(sharp_single_quote_reader), r);
	cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('('),
					make_cf3(sharp_left_parenthesis_reader), r);
	cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('*'),
					make_cf3(sharp_asterisk_reader), r);
	cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR(':'),
					make_cf3(sharp_colon_reader), r);
	cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('.'),
					make_cf3(sharp_dot_reader), r);
	/*  Used for fasload only. */
	cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('B'),
					make_cf3(sharp_B_reader), r);
	cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('O'),
					make_cf3(sharp_O_reader), r);
	cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('X'),
					make_cf3(sharp_X_reader), r);
	cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('R'),
					make_cf3(sharp_R_reader), r);
	cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('A'),
					@'si::sharp-a-reader', r);
	cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('S'),
					@'si::sharp-s-reader', r);
	cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('P'),
					make_cf3(sharp_P_reader), r);

	cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('='),
					make_cf3(sharp_eq_reader), r);
	cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('#'),
					make_cf3(sharp_sharp_reader), r);
	cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('+'),
					make_cf3(sharp_plus_reader), r);
	cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('-'),
					make_cf3(sharp_minus_reader), r);
	cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('|'),
					make_cf3(sharp_vertical_bar_reader), r);
	/*  This is specific to this implementation  */
	cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('$'),
					make_cf3(sharp_dollar_reader), r);
	/*  This is specific to this implementation  */
	cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('Y'),
					make_cf3(sharp_Y_reader), r);
	/*  This is specific to this implementation: ignore BOM  */
#ifdef ECL_UNICODE
	ecl_readtable_set(r, 0xfeff, cat_whitespace, ECL_NIL);
#endif

        /* Lock the standard read table so that we do not have to make copies
         * to keep it unchanged */
        r->readtable.locked = 1;

	init_backq();

	ECL_SET(@'*readtable*',
		r=ecl_copy_readtable(cl_core.standard_readtable, ECL_NIL));
	cl_set_dispatch_macro_character(4, ECL_CODE_CHAR('#'), ECL_CODE_CHAR('!'),
					ECL_NIL, r);
	ECL_SET(@'*read-default-float-format*', @'single-float');

        {
                cl_object var, val;
                var = cl_list(24,
                              @'*print-pprint-dispatch*', /* See end of pprint.lsp */
                              @'*print-array*',
                              @'*print-base*',
                              @'*print-case*',
                              @'*print-circle*',
                              @'*print-escape*',
                              @'*print-gensym*',
                              @'*print-length*',
                              @'*print-level*',
                              @'*print-lines*',
                              @'*print-miser-width*',
                              @'*print-pretty*',
                              @'*print-radix*',
                              @'*print-readably*',
                              @'*print-right-margin*',
                              @'*read-base*',
                              @'*read-default-float-format*',
                              @'*read-eval*',
                              @'*read-suppress*',
                              @'*readtable*',
                              @'si::*print-package*',
                              @'si::*print-structure*',
                              @'si::*sharp-eq-context*',
			      @'si::*circle-counter*');
                val = cl_list(24,
                              /**pprint-dispatch-table**/ ECL_NIL,
                              /**print-array**/ ECL_T,
                              /**print-base**/ ecl_make_fixnum(10),
                              /**print-case**/ @':downcase',
                              /**print-circle**/ ECL_T,
                              /**print-escape**/ ECL_T,
                              /**print-gensym**/ ECL_T,
                              /**print-length**/ ECL_NIL,
                              /**print-level**/ ECL_NIL,
                              /**print-lines**/ ECL_NIL,
                              /**print-miser-width**/ ECL_NIL,
                              /**print-pretty**/ ECL_NIL,
                              /**print-radix**/ ECL_NIL,
                              /**print-readably**/ ECL_T,
                              /**print-right-margin**/ ECL_NIL,
                              /**read-base**/ ecl_make_fixnum(10),
                              /**read-default-float-format**/ @'single-float',
                              /**read-eval**/ ECL_T,
                              /**read-suppress**/ ECL_NIL,
                              /**readtable**/ cl_core.standard_readtable,
                              /*si::*print-package**/ cl_core.lisp_package,
                              /*si::*print-structure**/ ECL_T,
                              /*si::*sharp-eq-context**/ ECL_NIL,
			      /*si::*cicle-counter**/ ECL_NIL);
                ECL_SET(@'si::+ecl-syntax-progv-list+', CONS(var,val));
                var = cl_list(23,
                              @'*print-pprint-dispatch*', /* See end of pprint.lsp */
                              @'*print-array*',
                              @'*print-base*',
                              @'*print-case*',
                              @'*print-circle*',
                              @'*print-escape*',
                              @'*print-gensym*',
                              @'*print-length*',
                              @'*print-level*',
                              @'*print-lines*',
                              @'*print-miser-width*',
                              @'*print-pretty*',
                              @'*print-radix*',
                              @'*print-readably*',
                              @'*print-right-margin*',
                              @'*read-base*',
                              @'*read-default-float-format*',
                              @'*read-eval*',
                              @'*read-suppress*',
                              @'*readtable*',
                              @'*package*',
                              @'si::*sharp-eq-context*',
			      @'si::*circle-counter*');
                val = cl_list(23,
                              /**pprint-dispatch-table**/ ECL_NIL,
                              /**print-array**/ ECL_T,
                              /**print-base**/ ecl_make_fixnum(10),
                              /**print-case**/ @':upcase',
                              /**print-circle**/ ECL_NIL,
                              /**print-escape**/ ECL_T,
                              /**print-gensym**/ ECL_T,
                              /**print-length**/ ECL_NIL,
                              /**print-level**/ ECL_NIL,
                              /**print-lines**/ ECL_NIL,
                              /**print-miser-width**/ ECL_NIL,
                              /**print-pretty**/ ECL_NIL,
                              /**print-radix**/ ECL_NIL,
                              /**print-readably**/ ECL_T,
                              /**print-right-margin**/ ECL_NIL,
                              /**read-base**/ ecl_make_fixnum(10),
                              /**read-default-float-format**/ @'single-float',
                              /**read-eval**/ ECL_T,
                              /**read-suppress**/ ECL_NIL,
                              /**readtable**/ cl_core.standard_readtable,
                              /**package**/ cl_core.user_package,
                              /*si::*sharp-eq-context**/ ECL_NIL,
			      /*si::*cicle-counter**/ ECL_NIL);
                ECL_SET(@'si::+io-syntax-progv-list+', CONS(var,val));
        }
}

/*
 *----------------------------------------------------------------------
 *
 * ecl_init_module --
 *     reads the data vector from stream into vector VV
 *
 * Results:
 *	a vector.
 *
 *----------------------------------------------------------------------
 */
static cl_object
make_one_data_stream(const cl_object string)
{
#ifdef ECL_UNICODE
	return si_make_sequence_input_stream(3, string, @':external-format',
					     @':utf-8');
#else
	return ecl_make_string_input_stream(string, 0, ecl_length(string));
#endif
}

static cl_object
make_data_stream(const cl_object *data)
{
	if (data == 0 || data[0] == NULL) {
		return cl_core.null_stream;
	}
	if (data[1] == NULL) {
		return make_one_data_stream(data[0]);
	} else {
		cl_object stream_list = ECL_NIL;
		cl_index i;
		for (i = 0; data[i]; i++) {
			cl_object s = make_one_data_stream(data[i]);
			stream_list = ecl_cons(s, stream_list);
		}
		return cl_apply(2, @'make-concatenated-stream',
				cl_nreverse(stream_list));
	}
}

cl_object
ecl_init_module(cl_object block, void (*entry_point)(cl_object))
{
	const cl_env_ptr env = ecl_process_env();
	volatile cl_object old_eptbc = env->packages_to_be_created;
	volatile cl_object x;
	cl_index i, len, perm_len, temp_len;
	cl_object in;
	cl_object *VV, *VVtemp = 0;

	if (block == NULL)
                block = ecl_make_codeblock();
	block->cblock.entry = entry_point;

	in = OBJNULL;
	ECL_UNWIND_PROTECT_BEGIN(env) {
                cl_index bds_ndx;
                cl_object progv_list;

		ecl_bds_bind(env, @'si::*cblock*', block);
                env->packages_to_be_created_p = ECL_T;

		/* Communicate the library which Cblock we are using, and get
		 * back the amount of data to be processed.
		 */
		(*entry_point)(block);
		perm_len = block->cblock.data_size;
		temp_len = block->cblock.temp_data_size;
		len = perm_len + temp_len;

                if (block->cblock.data_text == 0) {
                        if (len) {
                                /* Code from COMPILE uses data in *compiler-constants* */
                                cl_object v = ECL_SYM_VAL(env,@'si::*compiler-constants*');
                                unlikely_if (ecl_t_of(v) != t_vector ||
                                             v->vector.dim != len ||
                                             v->vector.elttype != ecl_aet_object)
                                        FEerror("Internal error: corrupted data in "
                                                "si::*compiler-constants*", 0);
                                VV = block->cblock.data = v->vector.self.t;
                                VVtemp = block->cblock.temp_data = 0;
                        }
                        goto NO_DATA_LABEL;
                }
		if (len == 0) {
                        VV = VVtemp = 0;
                        goto NO_DATA_LABEL;
                }
#ifdef ECL_DYNAMIC_VV
		VV = block->cblock.data = perm_len? (cl_object *)ecl_alloc(perm_len * sizeof(cl_object)) : NULL;
#else
		VV = block->cblock.data;
#endif
		memset(VV, 0, perm_len * sizeof(*VV));

		VVtemp = block->cblock.temp_data = temp_len? (cl_object *)ecl_alloc(temp_len * sizeof(cl_object)) : NULL;
		memset(VVtemp, 0, temp_len * sizeof(*VVtemp));

		/* Read all data for the library */
#ifdef ECL_EXTERNALIZABLE
                {
                        cl_object v = ecl_deserialize(block->cblock.data_text);
                        unlikely_if (v->vector.dim < len)
                                FEreader_error("Not enough data while loading"
                                               "binary file", in, 0);
                        memcpy(VV, v->vector.self.t, len * sizeof(cl_object));
                }
#else
		in = make_data_stream(block->cblock.data_text);
                progv_list = ECL_SYM_VAL(env, @'si::+ecl-syntax-progv-list+');
                bds_ndx = ecl_progv(env, ECL_CONS_CAR(progv_list),
                                    ECL_CONS_CDR(progv_list));
		for (i = 0 ; i < len; i++) {
			x = ecl_read_object(in);
			if (x == OBJNULL)
				break;
			if (i < perm_len)
				VV[i] = x;
			else
				VVtemp[i-perm_len] = x;
		}
		if (!Null(ECL_SYM_VAL(env, @'si::*sharp-eq-context*'))) {
			while (i--) {
				if (i < perm_len) {
					VV[i] = patch_sharp(env, VV[i]);
				} else {
					VVtemp[i-perm_len] = patch_sharp(env, VVtemp[i-perm_len]);
				}
			}
		}
                ecl_bds_unwind(env, bds_ndx);
		unlikely_if (i < len)
			FEreader_error("Not enough data while loading"
                                       "binary file", in, 0);
                cl_close(1,in);
                in = OBJNULL;
#endif
	NO_DATA_LABEL:
                env->packages_to_be_created_p = ECL_NIL;

		for (i = 0; i < block->cblock.cfuns_size; i++) {
			const struct ecl_cfun *prototype = block->cblock.cfuns+i;
			cl_index fname_location = ecl_fixnum(prototype->block);
			cl_object fname = VV[fname_location];
			cl_index location = ecl_fixnum(prototype->name);
                        cl_object position = prototype->file_position;
			int narg = prototype->narg;
			VV[location] = narg<0?
				ecl_make_cfun_va((cl_objectfn)prototype->entry,
                                                 fname, block) :
				ecl_make_cfun((cl_objectfn_fixed)prototype->entry,
                                              fname, block, narg);
                        /* Add source file info */
                        if (position != ecl_make_fixnum(-1)) {
                                ecl_set_function_source_file_info(VV[location],
                                                                  block->cblock.source,
                                                                  position);
                        }
		}
		/* Execute top-level code */
		(*entry_point)(OBJNULL);
		x = cl_set_difference(2, env->packages_to_be_created, old_eptbc);
                old_eptbc = env->packages_to_be_created;
                unlikely_if (!Null(x)) {
                        CEerror(ECL_T,
                                Null(ECL_CONS_CDR(x))?
                                "Package ~A referenced in "
				"compiled file~&  ~A~&but has not been created":
                                "The packages~&  ~A~&were referenced in "
				"compiled file~&  ~A~&but have not been created",
				2, x, block->cblock.name);
		}
		if (VVtemp) {
			block->cblock.temp_data = NULL;
			block->cblock.temp_data_size = 0;
			ecl_dealloc(VVtemp);
		}
		ecl_bds_unwind1(env);
	} ECL_UNWIND_PROTECT_EXIT {
		if (in != OBJNULL)
			cl_close(1,in);
		env->packages_to_be_created = old_eptbc;
                env->packages_to_be_created_p = ECL_NIL;
	} ECL_UNWIND_PROTECT_END;

	return block;
}
