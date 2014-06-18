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
#include <ecl/ecl-inl.h>

#define PRINT_BASE ecl_make_fixnum(10)
#define EXPT_RADIX(x) ecl_ash(ecl_make_fixnum(1),x)

typedef struct {
        cl_object r;
        cl_object s;
        cl_object mm;
        cl_object mp;
        bool high_ok;
        bool low_ok;
} float_approx;

static cl_object
times2(cl_object x)
{
        return ecl_plus(x, x);
}

static float_approx *
setup(cl_object number, float_approx *approx)
{
        cl_object f = cl_integer_decode_float(number);
        cl_fixnum e = ecl_fixnum(VALUES(1)), min_e;
        bool limit_f = 0;
        switch (ecl_t_of(number)) {
        case t_singlefloat:
                min_e = FLT_MIN_EXP;
                limit_f = (number->SF.SFVAL ==
                           ldexpf(FLT_RADIX, FLT_MANT_DIG-1));
                break;
        case t_doublefloat:
                min_e = DBL_MIN_EXP;
                limit_f = (number->DF.DFVAL ==
                           ldexp(FLT_RADIX, DBL_MANT_DIG-1));
                break;
#ifdef ECL_LONG_FLOAT
        case t_longfloat:
                min_e = LDBL_MIN_EXP;
                limit_f = (number->longfloat.value ==
                           ldexpl(FLT_RADIX, LDBL_MANT_DIG-1));
#endif
        }
        approx->low_ok = approx->high_ok = ecl_evenp(f);
        if (e > 0) {
                cl_object be = EXPT_RADIX(e);
                if (limit_f) {
                        cl_object be1 = ecl_times(be, ecl_make_fixnum(FLT_RADIX));
                        approx->r = times2(ecl_times(f, be1));
                        approx->s = ecl_make_fixnum(FLT_RADIX*2);
                        approx->mm = be;
                        approx->mp = be1;
                } else {
                        approx->r = times2(ecl_times(f, be));
                        approx->s = ecl_make_fixnum(2);
                        approx->mm = be;
                        approx->mp = be;
                }
        } else if (!limit_f || (e == min_e)) {
                approx->r = times2(f);
                approx->s = times2(EXPT_RADIX(-e));
                approx->mp = ecl_make_fixnum(1);
                approx->mm = ecl_make_fixnum(1);
        } else {
                approx->r = times2(ecl_make_fixnum(FLT_RADIX));
                approx->s = times2(EXPT_RADIX(1-e));
                approx->mp = ecl_make_fixnum(FLT_RADIX);
                approx->mm = ecl_make_fixnum(1);
        }
        return approx;
}

static cl_fixnum
scale(float_approx *approx)
{
        cl_fixnum k = 0;
        cl_object x = ecl_plus(approx->r, approx->mp);
        int sign;
        do {
                sign = ecl_number_compare(x, approx->s);
                if (approx->high_ok) {
                        if (sign < 0)
                                break;
                } else {
                        if (sign <= 0)
                                break;
                }
                approx->s = ecl_times(approx->s, PRINT_BASE);
                k++;
        } while(1);
        do {
                x = ecl_times(x, PRINT_BASE);
                sign = ecl_number_compare(x, approx->s);
                if (approx->high_ok) {
                        if (sign >= 0)
                                break;
                } else {
                        if (sign > 0)
                                break;
                }
                k--;
                approx->r = ecl_times(approx->r, PRINT_BASE);
                approx->mm = ecl_times(approx->mm, PRINT_BASE);
                approx->mp = ecl_times(approx->mp, PRINT_BASE);
        } while(1);
        return k;
}

static cl_object
generate(cl_object digits, float_approx *approx)
{
        cl_object d, x;
        cl_fixnum digit;
        bool tc1, tc2;
        do {
                d = ecl_truncate2(ecl_times(approx->r, PRINT_BASE), approx->s);
                approx->r = VALUES(1);
                approx->mp = ecl_times(approx->mp, PRINT_BASE);
                approx->mm = ecl_times(approx->mm, PRINT_BASE);
                tc1 = approx->low_ok?
                        ecl_lowereq(approx->r, approx->mm) :
                        ecl_lower(approx->r, approx->mm);
                x = ecl_plus(approx->r, approx->mp);
                tc2 = approx->high_ok?
                        ecl_greatereq(x, approx->s) :
                        ecl_greater(x, approx->s);
                if (tc1 || tc2) {
                        break;
                }
                ecl_string_push_extend(digits, ecl_digit_char(ecl_fixnum(d), 10));
        } while (1);
        if (tc2 && !tc1) {
                digit = ecl_fixnum(d) + 1;
        } else if (tc1 && !tc2) {
                digit = ecl_fixnum(d);
        } else if (ecl_lower(times2(approx->r), approx->s)) {
                digit = ecl_fixnum(d);
        } else {
                digit = ecl_fixnum(d) + 1;
        }
        ecl_string_push_extend(digits, ecl_digit_char(digit, 10));
        return digits;
}

static void
change_precision(float_approx *approx, cl_object position, cl_object relativep)
{
        cl_fixnum pos;
        if (Null(position))
                return;
        pos = ecl_fixnum(position);
        if (!Null(relativep)) {
                cl_object k = ecl_make_fixnum(0);
                cl_object l = ecl_make_fixnum(1);
                while (ecl_lower(ecl_times(approx->s, l),
                                 ecl_plus(approx->r, approx->mp))) {
                        k = ecl_one_plus(k);
                        l = ecl_times(l, PRINT_BASE);
                }
                position = ecl_minus(k, position);
                {
                        cl_object e1 = cl_expt(PRINT_BASE, position);
                        cl_object e2 = ecl_divide(e1, ecl_make_fixnum(2));
                        cl_object e3 = cl_expt(PRINT_BASE, k); 
                        if (ecl_greatereq(ecl_plus(approx->r, ecl_times(approx->s, e1)),
                                          ecl_times(approx->s, e2)))
                                position = ecl_one_minus(position);
                }
        }
        {
                cl_object x = ecl_times(approx->s, cl_expt(PRINT_BASE, position));
                cl_object e = ecl_divide(x, ecl_make_fixnum(2));
                cl_object low = cl_max(2, approx->mm, e);
                cl_object high = cl_max(2, approx->mp, e);
                if (ecl_lowereq(approx->mm, low)) {
                        approx->mm = low;
                        approx->low_ok = 1;
                }
                if (ecl_lowereq(approx->mp, high)) {
                        approx->mp = high;
                        approx->high_ok = 1;
                }
        }
}

cl_object
si_float_to_digits(cl_object digits, cl_object number, cl_object position,
                   cl_object relativep)
{
        cl_fixnum k;
        float_approx approx[1];
        setup(number, approx);
        change_precision(approx, position, relativep);
        k = scale(approx);
        if (Null(digits))
                digits = si_make_vector(@'base-char', ecl_make_fixnum(10),
                                        ECL_T /* adjustable */,
                                        ecl_make_fixnum(0) /* fill pointer */,
                                        ECL_NIL /* displacement */,
                                        ECL_NIL /* displ. offset */);
        generate(digits, approx);
        @(return ecl_make_fixnum(k) digits)
}
