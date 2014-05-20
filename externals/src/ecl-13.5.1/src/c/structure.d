/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    structure.c -- Structure interface.
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
#include <ecl/ecl-inl.h>
#include <string.h>

#ifndef CLOS
#error "Needs to define CLOS"
#endif

/******************************* ------- ******************************/

#ifdef CLOS
static bool
structure_subtypep(cl_object x, cl_object y)
{
	if (ECL_CLASS_NAME(x) == y) {
		return TRUE;
	} else {
		cl_object superiors = ECL_CLASS_SUPERIORS(x);
		loop_for_on_unsafe(superiors) {
			if (structure_subtypep(ECL_CONS_CAR(superiors), y))
				return TRUE;
		} end_loop_for_on_unsafe(superiors);
		return FALSE;
	}
}
#else
static bool
structure_subtypep(cl_object x, cl_object y)
{
	do {
		if (!ECL_SYMBOLP(x))
			return(FALSE);
		if (x == y)
			return(TRUE);
		x = si_get_sysprop(x, @'si::structure-include');
	} while (x != ECL_NIL);
	return(FALSE);
}
#endif /* CLOS */

cl_object
si_structure_subtype_p(cl_object x, cl_object y)
{
	@(return ((ecl_t_of(x) == T_STRUCTURE
		     && structure_subtypep(ECL_STRUCT_TYPE(x), y)) ? ECL_T : ECL_NIL))
}

@(defun si::make-structure (type &rest args)
	cl_object x;
	int i;
@
	x = ecl_alloc_object(T_STRUCTURE);
	ECL_STRUCT_TYPE(x) = type;
	ECL_STRUCT_SLOTS(x) = NULL;	/* for GC sake */
	ECL_STRUCT_LENGTH(x) = --narg;
	ECL_STRUCT_SLOTS(x) = (cl_object *)ecl_alloc_align(sizeof(cl_object)*narg, sizeof(cl_object));
#ifdef CLOS
        x->instance.sig = ECL_UNBOUND;
#endif
	if (narg >= ECL_SLOTS_LIMIT)
		FEerror("Limit on structure size exceeded: ~S slots requested.",
			1, ecl_make_fixnum(narg));
	for (i = 0;  i < narg;  i++)
		ECL_STRUCT_SLOT(x, i) = ecl_va_arg(args);
	@(return x)
@)

#ifdef CLOS
#define ecl_copy_structure si_copy_instance
#else
cl_object
ecl_copy_structure(cl_object x)
{
	cl_index j, size;
	cl_object y;

	if (ecl_unlikely(Null(si_structurep(x))))
		FEwrong_type_only_arg(@[copy-structure], x, @[structure]);
	y = ecl_alloc_object(T_STRUCTURE);
	ECL_STRUCT_TYPE(y) = ECL_STRUCT_TYPE(x);
	ECL_STRUCT_LENGTH(y) = j = ECL_STRUCT_LENGTH(x);
	size = sizeof(cl_object)*j;
	ECL_STRUCT_SLOTS(y) = NULL;	/* for GC sake */
	ECL_STRUCT_SLOTS(y) = (cl_object *)ecl_alloc_align(size, sizeof(cl_object));
	memcpy(ECL_STRUCT_SLOTS(y), ECL_STRUCT_SLOTS(x), size);
#ifdef CLOS
        y->instance.sig = x->instance.sig;
#endif
	@(return y)
}
#endif /* !CLOS */

cl_object
cl_copy_structure(cl_object s)
{
	switch (ecl_t_of(s)) {
	case t_instance:
		s = ecl_copy_structure(s);
		break;
	case t_list:
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_base_string:
	case t_bitvector:
	case t_vector:
		s = cl_copy_seq(s);
		break;
	default:
                FEwrong_type_only_arg(@[copy-structure], s, @[structure]);
	}
	@(return s)
}


/* Kept only for compatibility. One should use class-of or type-of. */
cl_object
si_structure_name(cl_object s)
{
	if (ecl_unlikely(Null(si_structurep(s))))
                FEwrong_type_only_arg(@[si::structure-name], s, @[structure]);
	@(return ECL_STRUCT_NAME(s))
}

cl_object
si_structure_ref(cl_object x, cl_object type, cl_object index)
{
	if (ecl_unlikely(ecl_t_of(x) != T_STRUCTURE ||
                         !structure_subtypep(ECL_STRUCT_TYPE(x), type)))
                FEwrong_type_nth_arg(@[si::structure-ref], 1, x, type);
	@(return ECL_STRUCT_SLOT(x, ecl_fixnum(index)))
}

cl_object
ecl_structure_ref(cl_object x, cl_object type, int n)
{

	if (ecl_unlikely(ecl_t_of(x) != T_STRUCTURE ||
                         !structure_subtypep(ECL_STRUCT_TYPE(x), type)))
                FEwrong_type_nth_arg(@[si::structure-ref], 1, x, type);
	return(ECL_STRUCT_SLOT(x, n));
}

cl_object
si_structure_set(cl_object x, cl_object type, cl_object index, cl_object val)
{
	if (ecl_unlikely(ecl_t_of(x) != T_STRUCTURE ||
                         !structure_subtypep(ECL_STRUCT_TYPE(x), type)))
                FEwrong_type_nth_arg(@[si::structure-set], 1, x, type);
	ECL_STRUCT_SLOT(x, ecl_fixnum(index)) = val;
	@(return val)
}

cl_object
ecl_structure_set(cl_object x, cl_object type, int n, cl_object v)
{

	if (ecl_unlikely(ecl_t_of(x) != T_STRUCTURE ||
                         !structure_subtypep(ECL_STRUCT_TYPE(x), type)))
                FEwrong_type_nth_arg(@[si::structure-set], 1, x, type);
	ECL_STRUCT_SLOT(x, n) = v;
	return(v);
}

cl_object
si_structurep(cl_object s)
{
#ifdef CLOS
	if (ECL_INSTANCEP(s) && structure_subtypep(ECL_CLASS_OF(s), @'structure-object'))
		return ECL_T;
#else
	if (ecl_t_of(s) == t_structure)
		return ECL_T;
#endif
	else
		return ECL_NIL;
}
