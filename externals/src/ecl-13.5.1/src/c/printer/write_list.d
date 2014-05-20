/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    write_list.d -- ugly printer for lists
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

void
_ecl_write_list(cl_object x, cl_object stream)
{
        const cl_env_ptr env = ecl_process_env();
        bool circle;
        cl_fixnum print_level, print_length;
        cl_index i;
        cl_object y;
        if (Null(x)) {
                _ecl_write_symbol(x, stream);
                return;
        }
        if (CAR(x) == @'si::#!') {
                writestr_stream("#!", stream);
                x = CDR(x);
                si_write_object(x, stream);
                return;
        }
        if (CONSP(CDR(x)) && Null(CDDR(x))) {
                if (CAR(x) == @'quote') {
                        ecl_write_char('\'', stream);
                        x = CADR(x);
                        si_write_object(x, stream);
                        return;
                }
                if (CAR(x) == @'function') {
                        ecl_write_char('#', stream);
                        ecl_write_char('\'', stream);
                        x = CADR(x);
                        si_write_object(x, stream);
                        return;
                }
                if (CAR(x) == @'si::quasiquote') {
                        ecl_write_char('`', stream);
                        x = CADR(x);
                        si_write_object(x, stream);
                        return;
                }
                if (CAR(x) == @'si::unquote') {
                        ecl_write_char(',', stream);
                        x = CADR(x);
                        si_write_object(x, stream);
                        return;
                }
                if (CAR(x) == @'si::unquote-splice') {
                        writestr_stream(",@@", stream);
                        x = CADR(x);
                        si_write_object(x, stream);
                        return;
                }
                if (CAR(x) == @'si::unquote-nsplice') {
                        writestr_stream(",.", stream);
                        x = CADR(x);
                        si_write_object(x, stream);
                        return;
                }
        }
        circle = ecl_print_circle();
        if (ecl_print_readably()) {
                print_level = MOST_POSITIVE_FIXNUM;
                print_length = MOST_POSITIVE_FIXNUM;
        } else {
                print_level = ecl_print_level();
                print_length = ecl_print_length();
        }
        if (print_level == 0) {
                ecl_write_char('#', stream);
                return;
        }
        ecl_bds_bind(env, @'*print-level*', ecl_make_fixnum(print_level-1));
        ecl_write_char('(', stream);
        for (i = 0;  ;  i++) {
                if (i >= print_length) {
                        writestr_stream("...", stream);
                        break;
                }
                y = CAR(x);
                x = CDR(x);
                si_write_object(y, stream);
                /* FIXME! */
                if (x == OBJNULL || ECL_ATOM(x) ||
                    (circle && _ecl_will_print_as_hash(x)))
			{
				if (x != ECL_NIL) {
					ecl_write_char(' ', stream);
					writestr_stream(". ", stream);
					si_write_object(x, stream);
				}
				break;
			}
                if (i == 0 && y != OBJNULL && ecl_t_of(y) == t_symbol)
                        ecl_write_char(' ', stream);
                else
                        ecl_write_char(' ', stream);
        }
        ecl_write_char(')', stream);
        ecl_bds_unwind1(env);
}
