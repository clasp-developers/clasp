/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    write_object.d -- basic printer routine.
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
#include <ecl/bytecodes.h>

bool
_ecl_will_print_as_hash(cl_object x)
{
	cl_object circle_counter = ecl_symbol_value(@'si::*circle-counter*');
	cl_object circle_stack = ecl_symbol_value(@'si::*circle-stack*');
	cl_object code = ecl_gethash_safe(x, circle_stack, OBJNULL);
	if (ECL_FIXNUMP(circle_counter)) {
		return !(code == OBJNULL || code == ECL_NIL);
	} else if (code == OBJNULL) {
		/* Was not found before */
		_ecl_sethash(x, circle_stack, ECL_NIL);
		return 0;
	} else {
		return 1;
	}
}

/* To print circular structures, we traverse the structure by adding
   a pair <element, flag> to the interpreter stack for each element visited.
   flag is initially NIL and becomes T if the element is visited again.
   After the visit we squeeze out all the non circular elements.
   The flags is used during printing to distinguish between the first visit
   to the element.
 */

static cl_fixnum
search_print_circle(cl_object x)
{
	cl_object circle_counter = ecl_symbol_value(@'si::*circle-counter*');
	cl_object circle_stack = ecl_symbol_value(@'si::*circle-stack*');
	cl_object code;

	if (!ECL_FIXNUMP(circle_counter)) {
		code = ecl_gethash_safe(x, circle_stack, OBJNULL);
		if (code == OBJNULL) {
			/* Was not found before */
			_ecl_sethash(x, circle_stack, ECL_NIL);
			return 0;
		} else if (code == ECL_NIL) {
			/* This object is referenced twice */
			_ecl_sethash(x, circle_stack, ECL_T);
			return 1;
		} else {
			return 2;
		}
	} else {
		code = ecl_gethash_safe(x, circle_stack, OBJNULL);
		if (code == OBJNULL || code == ECL_NIL) {
			/* Is not referenced or was not found before */
			/* _ecl_sethash(x, circle_stack, ECL_NIL); */
			return 0;
		} else if (code == ECL_T) {
			/* This object is referenced twice, but has no code yet */
			cl_fixnum new_code = ecl_fixnum(circle_counter) + 1;
			circle_counter = ecl_make_fixnum(new_code);
			_ecl_sethash(x, circle_stack, circle_counter);
			ECL_SETQ(ecl_process_env(), @'si::*circle-counter*',
				 circle_counter);
			return -new_code;
		} else {
			return ecl_fixnum(code);
		}
	}
}

cl_object
si_write_object(cl_object x, cl_object stream)
{
	bool circle;
#ifdef ECL_CMU_FORMAT
	if (ecl_symbol_value(@'*print-pretty*') != ECL_NIL) {
		cl_object f = _ecl_funcall2(@'pprint-dispatch', x);
		if (VALUES(1) != ECL_NIL) {
			_ecl_funcall3(f, stream, x);
                        goto OUTPUT;
		}
	}
#endif /* ECL_CMU_FORMAT */
	circle = ecl_print_circle();
	if (circle && !Null(x) && !ECL_FIXNUMP(x) && !ECL_CHARACTERP(x) &&
	    (LISTP(x) || (x->d.t != t_symbol) || (Null(x->symbol.hpack))))
	{
		cl_object circle_counter;
		cl_fixnum code;
		circle_counter = ecl_symbol_value(@'si::*circle-counter*');
		if (circle_counter == ECL_NIL) {
			cl_env_ptr env = ecl_process_env();
			cl_object hash =
				cl__make_hash_table(@'eq',
						    ecl_make_fixnum(1024),
                                                    cl_core.rehash_size,
                                                    cl_core.rehash_threshold);
			ecl_bds_bind(env, @'si::*circle-counter*', ECL_T);
			ecl_bds_bind(env, @'si::*circle-stack*', hash);
			si_write_object(x, cl_core.null_stream);
			ECL_SETQ(env, @'si::*circle-counter*', ecl_make_fixnum(0));
			si_write_object(x, stream);
			cl_clrhash(hash);
			ecl_bds_unwind_n(env, 2);
			goto OUTPUT;
		}
		code = search_print_circle(x);
		if (!ECL_FIXNUMP(circle_counter)) {
			/* We are only inspecting the object to be printed. */
			/* Only run X if it was not referenced before */
			if (code != 0)
                                goto OUTPUT;
		} else if (code == 0) {
			/* Object is not referenced twice */
		} else if (code < 0) {
			/* Object is referenced twice. We print its definition */
			ecl_write_char('#', stream);
			_ecl_write_fixnum(-code, stream);
			ecl_write_char('=', stream);
		} else {
			/* Second reference to the object */
			ecl_write_char('#', stream);
			_ecl_write_fixnum(code, stream);
			ecl_write_char('#', stream);
			goto OUTPUT;
		}
	}
	return si_write_ugly_object(x, stream);
 OUTPUT:
        @(return x)
}
