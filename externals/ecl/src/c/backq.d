/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    backq.c -- Backquote mechanism.
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

/******************************* ------- ******************************/

#define	QUOTE	1
#define	EVAL	2
#define	LIST	3
#define	LISTX	4
#define	APPEND	5
#define	NCONC	6

extern int _cl_backq_car(cl_object *px);
static cl_object backq(cl_object x);

static cl_object
kwote(cl_object x)
{
	cl_type t = ecl_t_of(x);
	if ((t == t_symbol && !Null(x) && !ecl_keywordp(x)) ||
	    t == t_list || t == t_vector)
		x = CONS(@'quote', ecl_list1(x));
	return x;
}

/*
	_cl_backq_cdr(&x) puts result into x and returns one of

		QUOTE		the form should be quoted
		EVAL		the form should be evaluated
		LIST		the form should be applied to LIST
		LISTX		the form should be applied to LIST*
		APPEND		the form should be applied to APPEND
		NCONC		the form should be applied to NCONC
*/
static int
_cl_backq_cdr(cl_object *px)
{
	cl_object x = *px, ax, dx;
	int a, d, out;

	if (ECL_ATOM(x))
		return(QUOTE);
	if (CAR(x) == @'si::unquote') {
		*px = CADR(x);
		return(EVAL);
	}
	if (CAR(x) == @'si::unquote-splice' || CAR(x) == @'si::unquote-nsplice')
		FEerror(",@@ or ,. has appeared in an illegal position.", 0);

	ax = CAR(x); dx = CDR(x);
	a = _cl_backq_car(&ax);
	d = _cl_backq_cdr(&dx);
	if (d == QUOTE) {
		switch (a) {
		case QUOTE:
			return(QUOTE);

		case EVAL:
			if (Null(dx)) {
				out = LIST;
			} else if (CONSP(dx) && Null(CDR(dx))) {
				dx = ecl_list1(kwote(CAR(dx)));
				out = LIST;
			} else {
				dx = ecl_list1(kwote(dx));
				out = LISTX;
			}
			break;
		case APPEND:
		case NCONC:
			if (Null(dx)) {
				*px = ax;
				return EVAL;
			} else {
				dx = ecl_list1(kwote(dx));
				out = a;
			}
			break;
		default:
			ecl_internal_error("backquote botch");
		}
	} else if (d == EVAL) {
		switch (a) {
		case QUOTE:
			ax = kwote(ax);
			dx = ecl_list1(dx);
			out = LISTX;
			break;
		case EVAL:
			dx = ecl_list1(dx);
			out = LISTX;
			break;
		case APPEND:
		case NCONC:
			dx = ecl_list1(dx);
			out = a;
			break;
		default:
			ecl_internal_error("backquote botch");
		}
	} else if (d == a) {
		out = d;
	} else {
		switch (d) {
		case LIST:
			if (a == QUOTE) {
				ax = kwote(ax);
				out = LIST;
				goto OUTPUT;
			} else if (a == EVAL) {
				out = LIST;
				goto OUTPUT;
			}
			dx = CONS(@'list', dx);
			break;
		case LISTX:
			if (a == QUOTE) {
				ax = kwote(ax);
				out = LISTX;
				goto OUTPUT;
			} else if (a == EVAL) {
				out = LISTX;
				goto OUTPUT;
			}
			dx = CONS(@'list*', dx);
			break;
		case APPEND:
			dx = CONS(@'append', dx);
			break;
		case NCONC:
			dx = CONS(@'nconc', dx);
			break;
		default:
			ecl_internal_error("backquote botch");
		}
		switch (a) {
		case QUOTE:
			ax = kwote(ax);
			dx = ecl_list1(dx);
			out = LISTX;
			break;
		case EVAL:
			dx = ecl_list1(dx);
			out = LISTX;
			break;
		case APPEND:
		case NCONC:
			dx = ecl_list1(dx);
			out = a;
			break;
		default:
			ecl_internal_error("backquote botch");
		}
	}
 OUTPUT:
	*px = CONS(ax, dx);
	return out;
}

/*
	_cl_backq_car(&x) puts result into x and returns one of

		QUOTE		the form should be quoted
		EVAL		the form should be evaluated
		APPEND		the form should be appended
				into the outer form
		NCONC		the form should be nconc'ed
				into the outer form
*/
int
_cl_backq_car(cl_object *px)
{
	cl_object x = *px;
	int d;
 AGAIN:
	if (ECL_ATOM(x))
		return(QUOTE);
	if (CAR(x) == @'si::quasiquote') {
		x = *px = backq(CADR(x));
		goto AGAIN;
	}
	if (CAR(x) == @'si::unquote') {
		*px = CADR(x);
		return EVAL;
	}
	if (CAR(x) == @'si::unquote-splice') {
		*px = CADR(x);
		return APPEND;
	}
	if (CAR(x) == @'si::unquote-nsplice') {
		*px = CADR(x);
		return NCONC;
	}
	d = _cl_backq_cdr(px);
	switch (d) {
	case QUOTE:
	case EVAL:
		return(d);

	case LIST:
		*px = CONS(@'list', *px);
		break;

	case LISTX:
		*px = CONS(@'list*', *px);
		break;

	case APPEND:
		*px = CONS(@'append', *px);
		break;

	case NCONC:
		*px = CONS(@'nconc', *px);
		break;

	default:
		ecl_internal_error("backquote botch");
	}
	return(EVAL);
}

static cl_object
backq(cl_object x)
{
	int a;

	a = _cl_backq_car(&x);
	if (a == APPEND || a == NCONC)
		FEerror(",@@ or ,. has appeared in an illegal position.", 0);
	if (a == QUOTE)
		return(kwote(x));
	return(x);
}

static cl_object
quasiquote_macro(cl_object whole, cl_object env)
{
	if (ecl_length(whole) != 2) {
		FEprogram_error("Syntax error: ~S.", 1, whole);
	}
	@(return backq(CADR(whole)))
}

void
init_backq(void)
{
	ecl_def_c_macro(@'si::quasiquote', quasiquote_macro, 2);
}
