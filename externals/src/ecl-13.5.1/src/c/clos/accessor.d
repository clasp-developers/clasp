/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    gfun.c -- Dispatch for generic functions.
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <string.h>
#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/cache.h>

static void
no_applicable_method(cl_env_ptr env, cl_object gfun, cl_object args)
{
	env->values[0] = _ecl_funcall3(@'no-applicable-method', gfun, args);
}

static cl_object
fill_spec_vector(cl_object vector, cl_object gfun, cl_object instance)
{
	cl_object *argtype = vector->vector.self.t;
	argtype[0] = gfun;
	argtype[1] = ECL_CLASS_OF(instance);
	vector->vector.fillp = 2;
	return vector;
}

static cl_object
slot_method_name(cl_object gfun, cl_object args)
{
	cl_object methods = _ecl_funcall3(@'compute-applicable-methods',
					  gfun, args);
	unlikely_if (Null(methods)) {
		return OBJNULL;
	} else {
		cl_object first = ECL_CONS_CAR(methods);
		cl_object slotd = _ecl_funcall3(@'slot-value', first,
						@'clos::slot-definition');
		return _ecl_funcall3(@'slot-value', slotd, @'clos::name');
	}
}

static cl_object
slot_method_index(cl_object gfun, cl_object instance, cl_object args)
{
	cl_object slot_name = slot_method_name(gfun, args);
	unlikely_if (slot_name == OBJNULL)
		return OBJNULL;
	else {
		cl_object table = _ecl_funcall3(@'slot-value',
						ECL_CLASS_OF(instance),
						@'clos::location-table');
		/* The class might not be a standard class. This happens
		 * when a nonstandard class inherits from a standard class
		 * and does not add any new slot accessor.
		 */
		unlikely_if (Null(table))
			return slot_name;
		return ecl_gethash_safe(slot_name, table, OBJNULL);
	}
}

static ecl_cache_record_ptr
search_slot_index(const cl_env_ptr env, cl_object gfun, cl_object instance)
{
	ecl_cache_ptr cache = env->slot_cache;
	fill_spec_vector(cache->keys, gfun, instance);
	return ecl_search_cache(cache);
}

static ecl_cache_record_ptr
add_new_index(const cl_env_ptr env, cl_object gfun, cl_object instance, cl_object args)
{
	/* The keys and the cache may change while we compute the
	 * applicable methods. We must save the keys and recompute the
	 * cache location if it was filled. */
	cl_object index = slot_method_index(gfun, instance, args);
	unlikely_if (index == OBJNULL) {
		no_applicable_method(env, gfun, args);
		return 0;
	}
	{
		ecl_cache_record_ptr e;
		ecl_cache_ptr cache = env->slot_cache;
		fill_spec_vector(cache->keys, gfun, instance);
		e = ecl_search_cache(cache);
		e->key = cl_copy_seq(cache->keys);
		e->value = index;
		return e;
	}
}

static void
ensure_up_to_date_instance(cl_object instance)
{
	cl_object clas = ECL_CLASS_OF(instance);
	cl_object slots = ECL_CLASS_SLOTS(clas);
	unlikely_if (slots != ECL_UNBOUND && instance->instance.sig != slots) {
		_ecl_funcall2(@'clos::update-instance', instance);
	}
}

cl_object
ecl_slot_reader_dispatch(cl_narg narg, cl_object instance)
{
	const cl_env_ptr env = ecl_process_env();
	cl_object gfun = env->function;
	cl_object index, value;
	ecl_cache_record_ptr e;

	unlikely_if (narg != 1)
		FEwrong_num_arguments(gfun);
	unlikely_if (!ECL_INSTANCEP(instance)) {
		no_applicable_method(env, gfun, ecl_list1(instance));
		return env->values[0];
	}

	e = search_slot_index(env, gfun, instance);
	unlikely_if (e->key == OBJNULL) {
		cl_object args = ecl_list1(instance);
		e = add_new_index(env, gfun, instance, args);
		/* no_applicable_method() was called */
		unlikely_if (e == 0) {
			return env->values[0];
		}
	}
	ensure_up_to_date_instance(instance);
	index = e->value;
	if (ECL_FIXNUMP(index)) {
		value = instance->instance.slots[ecl_fixnum(index)];
	} else if (ecl_unlikely(!ECL_LISTP(index))) {
		value = cl_slot_value(instance, index);
	} else if (ecl_unlikely(Null(index))) {
		FEerror("Error when accessing method cache for ~A", 1, gfun);
	} else {
		value = ECL_CONS_CAR(index);
	}
	unlikely_if (value == ECL_UNBOUND) {
		cl_object slot_name = slot_method_name(gfun, ecl_list1(instance));
		value = _ecl_funcall4(@'slot-unbound',
				      ECL_CLASS_OF(instance),
				      instance,
				      slot_name);
	}
	@(return value)
}

cl_object
ecl_slot_writer_dispatch(cl_narg narg, cl_object value, cl_object instance)
{
	const cl_env_ptr env = ecl_process_env();
	cl_object gfun = env->function;
	ecl_cache_record_ptr e;
	cl_object index;

	unlikely_if (narg != 2) {
		FEwrong_num_arguments(gfun);
	}
	unlikely_if (!ECL_INSTANCEP(instance)) {
		no_applicable_method(env, gfun, cl_list(2, value, instance));
		return env->values[0];
	}
	e = search_slot_index(env, gfun, instance);
	unlikely_if (e->key == OBJNULL) {
		cl_object args = cl_list(2, value, instance);
		e = add_new_index(env, gfun, instance, args);
		/* no_applicable_method() was called */
		unlikely_if (e == 0) {
			return env->values[0];
		}
	}
	index = e->value;
	if (ECL_FIXNUMP(index)) {
		instance->instance.slots[ecl_fixnum(index)] = value;
	} else if (ecl_unlikely(!ECL_LISTP(index))) {
		clos_slot_value_set(value, instance, index);
	} else if (ecl_unlikely(Null(index))) {
		FEerror("Error when accessing method cache for ~A", 1, gfun);
	} else {
		ECL_RPLACA(index, value);
	}
	@(return value)
}
