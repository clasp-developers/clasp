/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    num_comp.c  -- Comparisons on numbers.
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

/*
 * In Common Lisp, comparisons between floats and integers are performed
 * via an intermediate rationalization of the floating point number. In C,
 * on the other hand, the comparison is performed by converting the integer
 * into a floating point number. However, if the double type is too small
 * this may lead to a loss of precision and two numbers being told equal
 * when, by Common Lisp standards, would not.
 */
static int
double_fix_compare(cl_fixnum n, double d)
{
	if ((double)n < d) {
		return -1;
	} else if ((double)n > d) {
		return +1;
	} else if (sizeof(double) > sizeof(cl_fixnum)) {
		return 0;
	} else {
		/* When we reach here, the double type has no
		 * significant decimal part. However, as explained
		 * above, the double type is too small and integers
		 * may coerce to the same double number giving a false
		 * positive. Hence we perform the comparison in
		 * integer space. */
		cl_fixnum m = d;
		if (n == m) {
			return 0;
		} else if (n > m) {
			return +1;
		} else {
			return -1;
		}
	}
}

#ifdef ECL_LONG_FLOAT
static int
long_double_fix_compare(cl_fixnum n, long double d)
{
	if ((long double)n < d) {
		return -1;
	} else if ((long double)n > d) {
		return +1;
	} else if (sizeof(long double) > sizeof(cl_fixnum)) {
		return 0;
	} else {
		cl_fixnum m = d;
		if (n == m) {
			return 0;
		} else if (n > m) {
			return +1;
		} else {
			return -1;
		}
	}
}
#endif

