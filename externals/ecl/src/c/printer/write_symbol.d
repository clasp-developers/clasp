/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    write_symbol.d -- print a symbol.
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
#include <ecl/internal.h>

static bool
potential_number_p(cl_object s, int base)
{
	/* See ANSI 2.3.1.1 */
	static cl_index i, l;
        ecl_character c;
	/* A potential number must contain at least one digit */
	bool some_digit = FALSE;

	l = s->base_string.fillp;
	if (l == 0)
		return FALSE;
	c = ecl_char(s, 0);

	/* A potential number must begin with a digit, sign or
           extension character (^ _) */
	if (ecl_digitp(c,base) >= 0)
		some_digit = TRUE;
	else if (c != '+' && c != '-' && c != '^' && c != '_')
		return FALSE;

	/* A potential number cannot end with a sign */
        c = ecl_char(s, l-1);
	if (c == '+' || c == '-')
		return FALSE;

	for (i = 1;  i < l;  i++) {
		c = ecl_char(s, i);
		/* It can only contain digits, signs, ratio markers,
		 * extension characters and number markers. Number
		 * markers are letters, but two adjacent letters fail
		 * to be a number marker. */
		if (ecl_digitp(c, base) >= 0) {
			some_digit = TRUE;
		} else if (c == '+' || c == '-' ||
			   c == '/' || c == '.' || c == '^' || c == '_') {
			continue;
		} else if (ecl_alpha_char_p(c) &&
			   (((i+1) >= l) || !ecl_alpha_char_p(ecl_char(s, i+1)))) {
			continue;
		} else {
			return FALSE;
		}
	}
	return some_digit;
}

#define needs_to_be_inverted(s) (ecl_string_case(s) != 0)

static bool
all_dots(cl_object s)
{
	cl_index i;
	for (i = 0;  i < s->base_string.fillp;  i++)
		if (ecl_char(s, i) != '.')
			return 0;
	return 1;
}

static bool
needs_to_be_escaped(cl_object s, cl_object readtable, cl_object print_case)
{
	int action = readtable->readtable.read_case;
	cl_index i;
	if (potential_number_p(s, ecl_print_base()))
		return 1;
	/* The value of *PRINT-ESCAPE* is T. We need to check whether the
	 * symbol name S needs to be escaped. This will happen if it has some
	 * strange character, or if it has a lowercase character (because such
	 * a character cannot be read with the standard readtable) or if the
	 * string has to be escaped according to readtable case and the rules
	 * of 22.1.3.3.2. */
	for (i = 0; i < s->base_string.fillp;  i++) {
		int c = ecl_char(s, i);
		int syntax = ecl_readtable_get(readtable, c, 0);
		if (syntax != cat_constituent ||
                    ecl_invalid_character_p(c) ||
                    (c) == ':')
			return 1;
		if ((action == ecl_case_downcase) && ecl_upper_case_p(c))
			return 1;
		if (ecl_lower_case_p(c))
			return 1;
	}
	return 0;
}

static void
write_symbol_string(cl_object s, int action, cl_object print_case,
		    cl_object stream, bool escape)
{
	cl_index i;
	bool capitalize;
	if (action == ecl_case_invert) {
		if (!needs_to_be_inverted(s))
			action = ecl_case_preserve;
	}
	if (escape)
		ecl_write_char('|', stream);
	capitalize = 1;
	for (i = 0;  i < s->base_string.fillp;  i++) {
		int c = ecl_char(s, i);
		if (escape) {
			if (c == '|' || c == '\\') {
				ecl_write_char('\\', stream);
			}
		} else if (action != ecl_case_preserve) {
			if (ecl_upper_case_p(c)) {
				if ((action == ecl_case_invert) ||
				    ((action == ecl_case_upcase) &&
				     ((print_case == @':downcase') ||
				      ((print_case == @':capitalize') && !capitalize))))
				{
					c = ecl_char_downcase(c);
				}
				capitalize = 0;
			} else if (ecl_lower_case_p(c)) {
				if ((action == ecl_case_invert) ||
				    ((action == ecl_case_downcase) &&
				     ((print_case == @':upcase') ||
				      ((print_case == @':capitalize') && capitalize))))
				{
					c = ecl_char_upcase(c);
				}
				capitalize = 0;
			} else {
				capitalize = !ecl_alphanumericp(c);
			}
		}
		ecl_write_char(c, stream);
	}
	if (escape)
		ecl_write_char('|', stream);
}

static bool
forced_print_package(cl_object package)
{
	cl_object print_package = ecl_symbol_value(@'si::*print-package*');
	return !Null(print_package) && (print_package != package);
}

void
_ecl_write_symbol(cl_object x, cl_object stream)
{
	cl_object readtable = ecl_current_readtable();
	cl_object print_case = ecl_print_case();
	cl_object package;
	cl_object name;
	int intern_flag;
	bool print_readably = ecl_print_readably();
	bool forced_package = 0;

	if (Null(x)) {
		package = ECL_NIL_SYMBOL->symbol.hpack;
		name = ECL_NIL_SYMBOL->symbol.name;
	} else {
		package = x->symbol.hpack;
		name = x->symbol.name;
	}

	if (!print_readably && !ecl_print_escape()) {
		write_symbol_string(name, readtable->readtable.read_case,
				    print_case, stream, 0);
		return;
	}
	/* From here on, print-escape is true which means that it should
	 * be possible to recover the same symbol by reading it with
	 * the standard readtable (which has readtable-case = :UPCASE)
	 */
	if (Null(package)) {
		if (print_readably || ecl_print_gensym())
			writestr_stream("#:", stream);
	} else if (package == cl_core.keyword_package) {
		ecl_write_char(':', stream);
	} else if ((forced_package = forced_print_package(package))
		   || ecl_find_symbol(name, ecl_current_package(), &intern_flag) != x
		   || (intern_flag == 0))
	{
		cl_object name = package->pack.name;
		write_symbol_string(name, readtable->readtable.read_case,
				    print_case, stream,
				    needs_to_be_escaped(name, readtable, print_case));
		if (ecl_find_symbol(ecl_symbol_name(x), package, &intern_flag) != x)
			ecl_internal_error("can't print symbol");
		if (intern_flag == ECL_INTERNAL || forced_package) {
			writestr_stream("::", stream);
		} else if (intern_flag == ECL_EXTERNAL) {
			ecl_write_char(':', stream);
		} else {
			FEerror("Pathological symbol --- cannot print.", 0);
		}
	}
	write_symbol_string(name, readtable->readtable.read_case, print_case, stream,
			    needs_to_be_escaped(name, readtable, print_case) ||
			    all_dots(name));
}

