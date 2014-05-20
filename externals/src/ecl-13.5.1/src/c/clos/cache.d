/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    cache.d -- thread-local cache for a variety of operations
*/
/*
    Copyright (c) 2011, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <ecl/ecl.h>
#include <ecl/cache.h>
#include <ecl/internal.h>
#include "newhash.h"

#define RECORD_KEY(e) ((e)[0])
#define RECORD_VALUE(e) ((e)[1])
#define RECORD_GEN(e) ecl_fixnum((e+2)[0])
#define RECORD_GEN_SET(e,v) ((e+2)[0]=ecl_make_fixnum(v))

static void
empty_cache(ecl_cache_ptr cache)
{
	cl_object table = cache->table;
	cl_index i, total_size = table->vector.dim;
	cache->generation = 0;
	for (i = 0; i < total_size; i+=3) {
		table->vector.self.t[i] = OBJNULL;
		table->vector.self.t[i+1] = OBJNULL;
		table->vector.self.fix[i+2] = 0;
	}
#ifdef ECL_THREADS
	cache->clear_list = ECL_NIL;
#endif
}

static void
clear_one_from_cache(ecl_cache_ptr cache, cl_object target)
{
	cl_object table = cache->table;
	cl_index i, total_size = table->vector.dim;
	for (i = 0; i < total_size; i+=3) {
		cl_object key = table->vector.self.t[i];
		if (key != OBJNULL) {
			if (target == key->vector.self.t[0]) {
				table->vector.self.t[i] = OBJNULL;
				table->vector.self.fix[i+2] = 0;
			}
		}
	}
}

#ifdef ECL_THREADS
static void
clear_list_from_cache(ecl_cache_ptr cache)
{
	cl_object list = ecl_atomic_get(&cache->clear_list);
	cl_object table = cache->table;
	cl_index i, total_size = table->vector.dim;
	for (i = 0; i < total_size; i+=3) {
		cl_object key = table->vector.self.t[i];
		if (key != OBJNULL) {
			if (ecl_member_eq(key->vector.self.t[0], list)) {
				table->vector.self.t[i] = OBJNULL;
				table->vector.self.fix[i+2] = 0;
			}
		}
	}
}
#endif

ecl_cache_ptr
ecl_make_cache(cl_index key_size, cl_index cache_size)
{
	ecl_cache_ptr cache = ecl_alloc(sizeof(struct ecl_cache));
	cache->keys =
		si_make_vector(ECL_T, /* element type */
			       ecl_make_fixnum(key_size), /* Maximum size */
			       ECL_T, /* adjustable */
			       ecl_make_fixnum(0), /* fill pointer */
			       ECL_NIL, /* displaced */
			       ECL_NIL);
	cache->table =
		si_make_vector(ECL_T, /* element type */
			       ecl_make_fixnum(3*cache_size), /* Maximum size */
			       ECL_NIL, /* adjustable */
			       ECL_NIL, /* fill pointer */
			       ECL_NIL, /* displaced */
			       ECL_NIL);
	empty_cache(cache);
	return cache;
}

void
ecl_cache_remove_one(ecl_cache_ptr cache, cl_object first_key)
{
#ifdef ECL_THREADS
	ecl_atomic_push(&cache->clear_list, first_key);
#else
	clear_one_from_cache(cache, first_key);
#endif
}

static cl_index
vector_hash_key(cl_object keys)
{
	cl_index c, n, a = GOLDEN_RATIO, b = GOLDEN_RATIO;
	for (c = 0, n = keys->vector.fillp; n >= 3; ) {
		c += keys->vector.self.index[--n];
		b += keys->vector.self.index[--n];
		a += keys->vector.self.index[--n];
		mix(a, b, c);
	}
	switch (n) {
	case 2:	b += keys->vector.self.index[--n];
	case 1:	a += keys->vector.self.index[--n];
		c += keys->vector.dim;
		mix(a,b,c);
	}
	return c;
}


/*
 * variation of ecl_gethash from hash.d, which takes an array of objects as key
 * It also assumes that entries are never removed except by clrhash.
 */

ecl_cache_record_ptr
ecl_search_cache(ecl_cache_ptr cache)
{
#ifdef ECL_THREADS
	if (!Null(cache->clear_list)) {
		clear_list_from_cache(cache);
	}
#endif
{
	cl_object table = cache->table;
	cl_object keys = cache->keys;
	cl_index argno = keys->vector.fillp;
	cl_index i = vector_hash_key(keys);
	cl_index total_size = table->vector.dim;
	cl_fixnum min_gen, gen;
	cl_object *min_e;
	int k;
	i = i % total_size;
	i = i - (i % 3);
	min_gen = cache->generation;
	min_e = 0;
	for (k = 20; k--; ) {
		cl_object *e = table->vector.self.t + i;
		cl_object hkey = RECORD_KEY(e);
		if (hkey == OBJNULL) {
			min_gen = -1;
			min_e = e;
			if (RECORD_VALUE(e) == OBJNULL) {
				/* This record is not only deleted but empty
				 * Hence we cannot find our method ahead */
				break;
			}
			/* Else we only know that the record has been
			 * delete, but we might find our data ahead. */
		} else if (argno == hkey->vector.fillp) {
			cl_index n;
			for (n = 0; n < argno; n++) {
				if (keys->vector.self.t[n] !=
				    hkey->vector.self.t[n])
					goto NO_MATCH;
			}
			min_e = e;
			goto FOUND;
		} else if (min_gen >= 0) {
		NO_MATCH:
			/* Unless we have found a deleted record, keep
			 * looking for the oldest record that we can
			 * overwrite with the new data. */
			gen = RECORD_GEN(e);
			if (gen < min_gen) {
				min_gen = gen;
				min_e = e;
			}
		}
		i += 3;
		if (i >= total_size) i = 0;
	}
	if (min_e == 0) {
		ecl_internal_error("search_method_hash");
	}
	RECORD_KEY(min_e) = OBJNULL;
	cache->generation++;
 FOUND:
	/*
	 * Once we have reached here, we set the new generation of
	 * this record and perform a global shift so that the total
	 * generation number does not become too large and we can
	 * expire some elements.
	 */
	gen = cache->generation;
	RECORD_GEN_SET(min_e, gen);
	if (gen >= total_size/2) {
		cl_object *e = table->vector.self.t;
		gen = 0.5*gen;
		cache->generation -= gen;
		for (i = table->vector.dim; i; i-= 3, e += 3) {
			cl_fixnum g = RECORD_GEN(e) - gen;
			if (g <= 0) {
				RECORD_KEY(e) = OBJNULL;
				RECORD_VALUE(e) = ECL_NIL;
				g = 0;
			}
			RECORD_GEN_SET(e, g);
		}
	}
	return (ecl_cache_record_ptr)min_e;
}
}

