/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    minmax.c  -- number sorting.
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

@(defun max (max &rest nums)
@
	/* INV: type check occurs in ecl_number_compare() for the rest of
	   numbers, but for the first argument it happens in ecl_zerop(). */
	if (narg-- == 1) {
		ecl_zerop(max);
	} else do {
		cl_object numi = ecl_va_arg(nums);
		if (ecl_number_compare(max, numi) < 0)
			max = numi;
	} while (--narg);
	@(return max)
@)

@(defun min (min &rest nums)
@
	/* INV: type check occurs in ecl_number_compare() for the rest of
	   numbers, but for the first argument it happens in ecl_zerop(). */
	if (narg-- == 1) {
		ecl_zerop(min);
	} else do {
		cl_object numi = ecl_va_arg(nums);
		if (ecl_number_compare(min, numi) > 0)
			min = numi;
	} while (--narg);
	@(return min)
@)
