/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    mapfun.c -- Mapping.
*/
/*
    Copyright (c) 1993, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/


#include <ecl/ecl.h>
#include <ecl/internal.h>
#include <string.h>

#define PREPARE_MAP(env, list, cdrs_frame, cars_frame, narg)            \
	struct ecl_stack_frame frames_aux[2];                           \
	const cl_object cdrs_frame = (cl_object)frames_aux;             \
        const cl_object cars_frame = (cl_object)(frames_aux+1);         \
	ECL_STACK_FRAME_FROM_VA_LIST(env,cdrs_frame,list);              \
	ECL_STACK_FRAME_COPY(cars_frame, cdrs_frame);                   \
	narg = cars_frame->frame.size;                                  \
	if (ecl_unlikely(narg == 0)) {                                  \
		FEprogram_error_noreturn("MAP*: Too few arguments", 0); \
	}

@(defun mapcar (fun &rest lists)
	cl_object res, *val = &res;
@ {
	PREPARE_MAP(the_env, lists, cdrs_frame, cars_frame, narg);
	res = ECL_NIL;
	while (TRUE) {
		cl_index i;
		for (i = 0;  i < narg;  i++) {
			cl_object cdr = ECL_STACK_FRAME_REF(cdrs_frame, i);
                        if (ecl_unlikely(!LISTP(cdr)))
                                FEwrong_type_nth_arg(@[mapcar], i+2, cdr, @[list]);
			if (Null(cdr)) {
				ecl_stack_frame_close(cars_frame);
				ecl_stack_frame_close(cdrs_frame);
				@(return res)
			}
			ECL_STACK_FRAME_SET(cars_frame, i, ECL_CONS_CAR(cdr));
			ECL_STACK_FRAME_SET(cdrs_frame, i, ECL_CONS_CDR(cdr));
		}
		*val = ecl_list1(ecl_apply_from_stack_frame(cars_frame, fun));
		val = &ECL_CONS_CDR(*val);
	}
} @)

@(defun maplist (fun &rest lists)
	cl_object res, *val = &res;
@ {
	PREPARE_MAP(the_env, lists, cdrs_frame, cars_frame, narg);
	res = ECL_NIL;
	while (TRUE) {
		cl_index i;
		for (i = 0;  i < narg;  i++) {
			cl_object cdr = ECL_STACK_FRAME_REF(cdrs_frame, i);
                        if (ecl_unlikely(!LISTP(cdr)))
                                FEwrong_type_nth_arg(@[maplist], i+2, cdr, @[list]);
			if (Null(cdr)) {
				ecl_stack_frame_close(cars_frame);
				ecl_stack_frame_close(cdrs_frame);
				@(return res)
			}
			ECL_STACK_FRAME_SET(cars_frame, i, cdr);
			ECL_STACK_FRAME_SET(cdrs_frame, i, ECL_CONS_CDR(cdr));
		}
		*val = ecl_list1(ecl_apply_from_stack_frame(cars_frame, fun));
		val = &ECL_CONS_CDR(*val);
	}
} @)

@(defun mapc (fun &rest lists)
	cl_object onelist;
@ {
	PREPARE_MAP(the_env, lists, cdrs_frame, cars_frame, narg);
	onelist = ECL_STACK_FRAME_REF(cdrs_frame, 0);
	while (TRUE) {
		cl_index i;
		for (i = 0;  i < narg;  i++) {
			cl_object cdr = ECL_STACK_FRAME_REF(cdrs_frame, i);
                        if (ecl_unlikely(!LISTP(cdr)))
                                FEwrong_type_nth_arg(@[mapc], i+2, cdr, @[list]);
			if (Null(cdr)) {
				ecl_stack_frame_close(cars_frame);
				ecl_stack_frame_close(cdrs_frame);
				@(return onelist)
			}
			ECL_STACK_FRAME_SET(cars_frame, i, ECL_CONS_CAR(cdr));
			ECL_STACK_FRAME_SET(cdrs_frame, i, ECL_CONS_CDR(cdr));
		}
		ecl_apply_from_stack_frame(cars_frame, fun);
	}
} @)

@(defun mapl (fun &rest lists)
	cl_object onelist;
@ {
	PREPARE_MAP(the_env, lists, cdrs_frame, cars_frame, narg);
	onelist = ECL_STACK_FRAME_REF(cdrs_frame, 0);
	while (TRUE) {
		cl_index i;
		for (i = 0;  i < narg;  i++) {
			cl_object cdr = ECL_STACK_FRAME_REF(cdrs_frame, i);
                        if (ecl_unlikely(!LISTP(cdr)))
                                FEwrong_type_nth_arg(@[mapl], i+2, cdr, @[list]);
			if (Null(cdr)) {
				ecl_stack_frame_close(cars_frame);
				ecl_stack_frame_close(cdrs_frame);
				@(return onelist)
			}
			ECL_STACK_FRAME_SET(cars_frame, i, cdr);
			ECL_STACK_FRAME_SET(cdrs_frame, i, ECL_CONS_CDR(cdr));
		}
		ecl_apply_from_stack_frame(cars_frame, fun);
	}
} @)

@(defun mapcan (fun &rest lists)
	cl_object res, *val = &res;
@ {
	PREPARE_MAP(the_env, lists, cdrs_frame, cars_frame, narg);
	res = ECL_NIL;
	while (TRUE) {
		cl_index i;
		for (i = 0;  i < narg;  i++) {
			cl_object cdr = ECL_STACK_FRAME_REF(cdrs_frame, i);
                        if (ecl_unlikely(!LISTP(cdr)))
                                FEwrong_type_nth_arg(@[mapcan], i+2, cdr, @[list]);
			if (Null(cdr)) {
				ecl_stack_frame_close(cars_frame);
				ecl_stack_frame_close(cdrs_frame);
				@(return res)
			}
			ECL_STACK_FRAME_SET(cars_frame, i, ECL_CONS_CAR(cdr));
			ECL_STACK_FRAME_SET(cdrs_frame, i, ECL_CONS_CDR(cdr));
		}
		*val = ecl_apply_from_stack_frame(cars_frame, fun);
		while (CONSP(*val))
			val = &ECL_CONS_CDR(*val);
	}
} @)

@(defun mapcon (fun &rest lists)
	cl_object res, *val = &res;
@ {
	PREPARE_MAP(the_env, lists, cdrs_frame, cars_frame, narg);
	res = ECL_NIL;
	while (TRUE) {
		cl_index i;
		for (i = 0;  i < narg;  i++) {
			cl_object cdr = ECL_STACK_FRAME_REF(cdrs_frame, i);
                        if (ecl_unlikely(!LISTP(cdr)))
                                FEwrong_type_nth_arg(@[mapcon], i+2, cdr, @[list]);
			if (Null(cdr)) {
				ecl_stack_frame_close(cars_frame);
				ecl_stack_frame_close(cdrs_frame);
				@(return res)
			}
			ECL_STACK_FRAME_SET(cars_frame, i, cdr);
			ECL_STACK_FRAME_SET(cdrs_frame, i, ECL_CONS_CDR(cdr));
		}
		*val = ecl_apply_from_stack_frame(cars_frame, fun);
		while (CONSP(*val))
			val = &ECL_CONS_CDR(*val);
	}
} @)
