/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    Copyright (c) 2010, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>

/*
 *  This is a port of CMUCL's FLOAT-STRING routine which converts a
 *  floating point number of arbitrary representation into a text
 *  representation which contains the least number of digits for the
 *  given precision.
 */
/*  Written by Bill Maddox
 *  Translated to C by Juan Jose Garcia Ripoll
 * 
 *  FLONUM-TO-STRING (and its subsidiary function FLOAT-STRING) does most of 
 *  the work for all printing of floating point numbers in the printer and in
 *  FORMAT.  It converts a floating point number to a string in a free or 
 *  fixed format with no exponent.  The interpretation of the arguments is as 
 *  follows:
 * 
 *      X        - The floating point number to convert, which must not be
 *                 negative.
 *      WIDTH    - The preferred field width, used to determine the number
 *                 of fraction digits to produce if the FDIGITS parameter
 *                 is unspecified or NIL.  If the non-fraction digits and the
 *                 decimal point alone exceed this width, no fraction digits
 *                 will be produced unless a non-NIL value of FDIGITS has been
 *                 specified.  Field overflow is not considerd an error at this
 *                 level.
 *      FDIGITS  - The number of fractional digits to produce. Insignificant
 *                 trailing zeroes may be introduced as needed.  May be
 *                 unspecified or NIL, in which case as many digits as possible
 *                 are generated, subject to the constraint that there are no
 *                 trailing zeroes.
 *      SCALE    - If this parameter is specified or non-NIL, then the number
 *                 printed is (* x (expt 10 scale)).  This scaling is exact,
 *                 and cannot lose precision.
 *      FMIN     - This parameter, if specified or non-NIL, is the minimum
 *                 number of fraction digits which will be produced, regardless
 *                 of the value of WIDTH or FDIGITS.  This feature is used by
 *                 the ~E format directive to prevent complete loss of
 *                 significance in the printed value due to a bogus choice of
 *                 scale factor.
 * 
 *  Most of the optional arguments are for the benefit for FORMAT and are not
 *  used by the printer.
 * 
 *  Returns:
 *  (VALUES DIGIT-STRING DIGIT-LENGTH LEADING-POINT TRAILING-POINT DECPNT)
 *  where the results have the following interpretation:
 * 
 *      DIGIT-STRING    - The decimal representation of X, with decimal point.
 *      DIGIT-LENGTH    - The length of the string DIGIT-STRING.
 *      LEADING-POINT   - True if the first character of DIGIT-STRING is the
 *                        decimal point.
 *      TRAILING-POINT  - True if the last character of DIGIT-STRING is the
 *                        decimal point.
 *      POINT-POS       - The position of the digit preceding the decimal
 *                        point.  Zero indicates point before first digit.
 * 
 *  NOTE:  FLONUM-TO-STRING goes to a lot of trouble to guarantee accuracy.
 *  Specifically, the decimal number printed is the closest possible 
 *  approximation to the true value of the binary number to be printed from 
 *  among all decimal representations  with the same number of digits.  In
 *  free-format output, i.e. with the number of digits unconstrained, it is 
 *  guaranteed that all the information is preserved, so that a properly-
 *  rounding reader can reconstruct the original binary number, bit-for-bit, 
 *  from its printed decimal representation. Furthermore, only as many digits
 *  as necessary to satisfy this condition will be printed.
 * 
 * 
 *  FLOAT-STRING actually generates the digits for positive numbers.  The
 *  algorithm is essentially that of algorithm Dragon4 in "How to Print 
 *  Floating-Point Numbers Accurately" by Steele and White.  The current 
 *  (draft) version of this paper may be found in [CMUC]<steele>tradix.press.
 *  DO NOT EVEN THINK OF ATTEMPTING TO UNDERSTAND THIS CODE WITHOUT READING 
 *  THE PAPER!
 */

static bool
large_mantissa(cl_object r, cl_object mp, cl_object s)
{
        return ecl_greatereq(ecl_plus(ecl_ash(r,1), mp),
                             ecl_ash(s, 1));
}

static cl_fixnum
assert_floating_point_width(cl_object width)
{
        if (!ECL_FIXNUMP(width) || ecl_lower(width,ecl_make_fixnum(1))) {
                FEerror("Invalid number of floating point digits."
                        "~%~A~%is not an integer within bounds",
                        1, width);
        }
        return ecl_fixnum(width);
}

static cl_object
float_string(cl_object digits_string,
             cl_object fraction, cl_object exponent, cl_object precision,
             cl_object width, cl_object fdigits, cl_object scale, cl_object fmin)
{
        cl_object r = fraction;
        cl_object s = ecl_make_fixnum(1);
        cl_object mm = s;
        cl_object mp = s;
        cl_fixnum i, k = 0, digits = 0, decpnt = 0, cutoff = 0;
        cl_object u;
        char *buffer;
        bool roundup = 0, cutoffp = 0, low = 0, high = 0;

        if (Null(digits_string)) {
                digits_string = si_make_vector(@'base-char', ecl_make_fixnum(10),
                                               ECL_T /* adjustable */,
                                               ecl_make_fixnum(0) /* fill pointer */,
                                               ECL_NIL /* displacement */,
                                               ECL_NIL /* displ. offset */);
        }
        /* Represent fraction as r/s, error bounds as m+/s and m-/s.
         * Rational arithmetic avoids loss of precision in subsequent
         * calculations.
         */
        {
                int sign = ecl_number_compare(exponent, ecl_make_fixnum(0));
                if (sign > 0) {
                        r = cl_ash(fraction, exponent);
                        mm = cl_ash(ecl_make_fixnum(1), exponent);
                        mp = mm;
                } else if (sign < 0) {
                        s = cl_ash(ecl_make_fixnum(1), ecl_negate(exponent));
                }
        }
        /* Adjust error bounds m+ and m- for unequal gaps */
        if (ecl_number_equalp(fraction, cl_ash(ecl_make_fixnum(1), precision))) {
                mp = ecl_ash(mm, 1);
                r = ecl_ash(r, 1);
                s = ecl_ash(s, 1);
        }
        /* Scale value by requested amount and update error bounds */
        if (!Null(scale)) {
                if (ecl_minusp(scale)) {
                        cl_object factor = cl_expt(ecl_make_fixnum(10),
                                                   ecl_negate(scale));
                        s = ecl_times(s, factor);
                } else {
                        cl_object factor = cl_expt(ecl_make_fixnum(10), scale);
                        r = ecl_times(r, factor);
                        mm = ecl_times(mm, factor);
                        mp = ecl_times(mp, factor);
                }
        }
        while (ecl_lower(r, ecl_ceiling2(s, ecl_make_fixnum(10)))) {
                k--;
                r = ecl_times(r, ecl_make_fixnum(10));
                mm = ecl_times(r, ecl_make_fixnum(10));
                mp = ecl_times(r, ecl_make_fixnum(10));
        }
        do {
                /* Ensure mantissa (r + m+)/s is smaller than one */
                while (large_mantissa(r, mp, s)) {
                        s = ecl_times(s, ecl_make_fixnum(10));
                        k++;
                }
                /* Determine the number of digits to generate */
                if (!Null(fdigits)) {
                        cutoffp = 1;
                        cutoff = assert_floating_point_width(width);
                } else if (!Null(width)) {
                        cutoffp = 1;
                        cutoff = assert_floating_point_width(width);
                        if (k < 0) {
                                cutoff = cutoff - 1;
                        } else {
                                cutoff = cutoff - k + 1;
                        }
                }
                /* ... and ensure it is never less than fmin */
                if (cutoffp) {
                        cl_fixnum a, i;
                        cl_object y;
                        if (!Null(fmin)) {
                                cl_fixnum f = assert_floating_point_width(fmin);
                                if (cutoff < f)
                                        cutoff = f;
                        }
                        /* If we decided to cut off digit generation before precision
                         * has been exhausted, rounding the last digit may cause a
                         * carry propagation.  We can prevent this, preserving
                         * left-to-right digit generation, with a few magical
                         * adjustments to m- and m+.  Of course, correct rounding is
                         * also preserved. */
                        a = k - cutoff;
                        y = s;
                        if (a < 0) {
                                for (i = 0, a = -a; i < a; i++) {
                                        y = ecl_ceiling2(y, ecl_make_fixnum(10));
                                } 
                        } else {
                                for (i = 0, a = -a; i < a; i++) {
                                        y = ecl_times(y, ecl_make_fixnum(10));
                                } 
                        }
                        mm = cl_max(2, y, mm);
                        mp = cl_max(2, y, mp);
                        roundup = ecl_number_equalp(mp, y);
                }
        } while (large_mantissa(r, mp, s));
        /* Zero-fill before fraction if no integer part */
        if (k < 0) {
                decpnt = digits;
                ecl_string_push_extend(digits_string, '.');
                for (i = k; i; i++) {
                        digits++;
                        ecl_string_push_extend(digits_string, '0');
                }
        }
        /* Generate least significant digits */
        do {
                int sign;
                if (--k == -1) {
                        ecl_string_push_extend(digits_string, '.');
                        decpnt = digits;
                }
                u = ecl_truncate2(ecl_times(r, ecl_make_fixnum(10)), s);
                r = VALUES(1);
                mm = ecl_times(mm, ecl_make_fixnum(10));
                mp = ecl_times(mp, ecl_make_fixnum(10));
                low = ecl_lower(ecl_ash(r,1), mm);
                sign = ecl_number_compare(ecl_ash(r,1), ecl_minus(ecl_ash(s,1),mp));
                high = roundup? (sign >= 0) : (sign > 0);
                /* stop when either precision is exhausted or we have printed as many
                 * fraction digits as permitted */
                if (low || high || (cutoffp && (k + cutoff <= 0)))
                        break;
                ecl_string_push_extend(digits_string, ecl_digit_char(ecl_fixnum(u), 10));
                digits++;
        } while(1);
        /* If cutof occured before first digit, then no digits generated at all */
        if (!cutoffp || (k + cutoff) >= 0) {
                /* Last digit may need rounding */
                int digit = ecl_fixnum(u);
                if (low && !high)
                        digit = ecl_fixnum(u);
                else if (high && !low)
                        digit = ecl_fixnum(u)+1;
                else if (ecl_lower(ecl_ash(r,1), s))
                        digit = ecl_fixnum(u);
                else
                        digit = ecl_fixnum(u) + 1;
                ecl_string_push_extend(digits_string, ecl_digit_char(digit, 10));
                digits++;
        }
        /* Zero-fill after integer part if no fraction */
        if (k >= 0) {
                for (i = 0; i < k; i++) {
                        ecl_string_push_extend(digits_string, '0');
                        digits++;
                }
                ecl_string_push_extend(digits_string, '.');
                decpnt = digits;
        }
        /* Add trailing zeroes to pad fraction if fdigits needed */
        if (!Null(fdigits)) {
                cl_fixnum f = assert_floating_point_width(fdigits) - (digits - decpnt);
                for (i = 0; i < f; i++) {
                        ecl_string_push_extend(digits_string, '0');
                        digits++;
                }
        }
        /* All done */
        @(return
          digits_string
          ecl_make_fixnum(1+digits)
          ((decpnt == 0)? ECL_T : ECL_NIL)
          ((decpnt == digits)? ECL_T : ECL_NIL)
          ecl_make_fixnum(decpnt))
}

ecl_def_ct_base_string(str_dot,".",1,static,const);

@(defun ext::float-string (string x &optional width fdigits scale fmin)
@
{
        if (ecl_zerop(x)) {
                if (Null(fdigits)) {
                        cl_object s = cl_make_string(3, ecl_one_plus(fdigits),
                                                     @':initial-element',
                                                     ECL_CODE_CHAR('0'));
                        ecl_char_set(s, 0, '.');
                        @(return s cl_length(s) ECL_T cl_zerop(fdigits) ecl_make_fixnum(0));
                } else {
                        @(return str_dot ecl_make_fixnum(1) ECL_T ECL_T ecl_make_fixnum(0));
                }
        } else {
                cl_object sig = cl_integer_decode_float(x);
                cl_object exp = VALUES(1);
                cl_object precision = cl_float_precision(x);
                cl_object digits = cl_float_digits(x);
                cl_object fudge = ecl_minus(digits, precision);
                cl_object w = Null(width)? ECL_NIL : cl_max(2, width, ecl_make_fixnum(1));
                return float_string(string, cl_ash(sig, ecl_negate(fudge)),
                                    ecl_plus(exp, fudge), precision, w,
                                    fdigits, scale, fmin);
        }
}
@)
