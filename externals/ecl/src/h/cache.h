/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    cache.h -- thread-local cache for a variety of operations
*/
/*
    Copyright (c) 2011, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#ifndef ECL_CACHE_H
#define ECL_CACHE_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct ecl_cache {
	cl_object keys;
	cl_object table;
	cl_index generation;
#ifdef ECL_THREADS
	cl_object clear_list;
#endif
} *ecl_cache_ptr;

typedef struct ecl_cache_record {
	cl_object key; /* vector[ndx] */
	cl_object value; /* vector[ndx+1] */
	cl_object gen; /* vector[ndx+2] */
} *ecl_cache_record_ptr;

extern ecl_cache_ptr ecl_make_cache(cl_index key_size, cl_index cache_size);
extern ecl_cache_record_ptr ecl_search_cache(ecl_cache_ptr cache);
extern void ecl_cache_remove_one(ecl_cache_ptr cache, cl_object first_key);

#ifdef __cplusplus
}
#endif


#endif /* !ECL_CACHE_H */
