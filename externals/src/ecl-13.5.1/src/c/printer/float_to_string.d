/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    Copyright (c) 2010, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#define ECL_INCLUDE_MATH_H
#include <float.h>
#include <ecl/ecl.h>
#include <ecl/internal.h>

cl_object
_ecl_ensure_buffer(cl_object buffer, cl_fixnum length)
{
        if (Null(buffer)) {
                buffer = si_make_vector(@'base-char', ecl_make_fixnum(length),
                                        ECL_T /* adjustable */,
                                        ecl_make_fixnum(0) /* fill pointer */,
                                        ECL_NIL /* displacement */,
                                        ECL_NIL /* displ. offset */);
        }
        return buffer;
}

void
_ecl_string_push_c_string(cl_object s, const char *c)
{
        for (; *c; c++) {
                ecl_string_push_extend(s, *c);
        }
}

static void
insert_char(cl_object buffer, cl_index where, cl_fixnum c)
{
        cl_fixnum end = buffer->base_string.fillp;
        ecl_string_push_extend(buffer, '.');
        ecl_copy_subarray(buffer, where+1, buffer, where, end - where);
        ecl_char_set(buffer, where, c);
}

static cl_object
push_base_string(cl_object buffer, cl_object s)
{
        buffer = _ecl_ensure_buffer(buffer, s->base_string.fillp);
        _ecl_string_push_c_string(buffer, (char *)s->base_string.self);
        return buffer;
}

/**********************************************************************
 * FREE FORMAT (FIXED OR EXPONENT) OF FLOATS
 */

static void
print_float_exponent(cl_object buffer, cl_object number, cl_fixnum exp)
{
        cl_object r = ecl_symbol_value(@'*read-default-float-format*');
        cl_fixnum e;
        switch (ecl_t_of(number)) {
        case t_singlefloat:
                e = (r == @'single-float' || r == @'short-float')? 'e' : 'f';
                break;
#ifdef ECL_LONG_FLOAT
        case t_longfloat:
                e = (r == @'long-float') ? 'e' : 'l';
                break;
        case t_doublefloat:
                e = (r == @'double-float')? 'e' : 'd';
                break;
#else
        case t_doublefloat:
                e = (r == @'double-float' || r == @'long-float')? 'e' : 'd';
                break;
#endif
        }
        if (e != 'e' || exp != 0) {
                ecl_string_push_extend(buffer, e);
                si_integer_to_string(buffer, ecl_make_fixnum(exp), ecl_make_fixnum(10),
                                     ECL_NIL, ECL_NIL);
        }
}

cl_object
si_float_to_string_free(cl_object buffer_or_nil, cl_object number,
                        cl_object e_min, cl_object e_max)
{
        cl_fixnum base, e;
        cl_object exp, buffer;

        if (ecl_float_nan_p(number)) {
                cl_object s = funcall(2, @'ext::float-nan-string', number);
                @(return push_base_string(buffer_or_nil, s));
        } else if (ecl_float_infinity_p(number)) {
                cl_object s = funcall(2, @'ext::float-infinity-string', number);
                @(return push_base_string(buffer_or_nil, s));
        }
        base = ecl_length(buffer_or_nil);
        exp = si_float_to_digits(buffer_or_nil, number, ECL_NIL, ECL_NIL);
        buffer = VALUES(1);
        e = ecl_fixnum(exp);

        if (ecl_signbit(number)) {
                insert_char(buffer, base++, '-');
        }
        /* Do we have to print in exponent notation? */
        if (ecl_lowereq(exp, e_min) || ecl_lowereq(e_max, exp)) {
                insert_char(buffer, base+1, '.');
                print_float_exponent(buffer, number, e-1);
        } else if (e > 0) {
                cl_fixnum l = buffer->base_string.fillp - base;
                while (l++ <= e) {
                        ecl_string_push_extend(buffer, '0');
                }
                insert_char(buffer, base+e, '.');
                print_float_exponent(buffer, number, 0);
        } else {
                insert_char(buffer, base++, '0');
                insert_char(buffer, base++, '.');
                for (e = -e; e; e--) {
                        insert_char(buffer, base++, '0');
                }
                print_float_exponent(buffer, number, 0);
        }
        @(return buffer);
}
