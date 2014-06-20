/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

/* These are routines for allocating and deallocating memory.
   They should be called as ADIOI_Malloc(size) and
   ADIOI_Free(ptr). In adio.h, they are macro-replaced to 
   ADIOI_Malloc(size,__LINE__,__FILE__) and 
   ADIOI_Free(ptr,__LINE__,__FILE__).

   Later on, add some tracing and error checking, similar to 
   MPID_trmalloc. */

#include "adio.h"
#include "mpi.h"
#include <stdlib.h>
#include <stdio.h>
#include "mpipr.h"

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

/* for the style checker */
/* style: allow:malloc:1 sig:0 */
/* style: allow:free:1 sig:0 */
/* style: allow:calloc:1 sig:0 */
/* style: allow:realloc:1 sig:0 */

#define FPRINTF fprintf

void *ADIOI_Malloc_fn(size_t size, int lineno, const char *fname);
void *ADIOI_Calloc_fn(size_t nelem, size_t elsize, int lineno, const char *fname);
void *ADIOI_Realloc_fn(void *ptr, size_t size, int lineno, const char *fname);
void ADIOI_Free_fn(void *ptr, int lineno, const char *fname);

void *ADIOI_Malloc_fn(size_t size, int lineno, const char *fname)
{
    void *new;

#ifdef ROMIO_XFS
    new = (void *) memalign(XFS_MEMALIGN, size);
#else
#ifdef HAVE_MPIU_FUNCS
    new = (void *) MPIU_Malloc(size);
#else
    new = (void *) malloc(size);
#endif
#endif
    if (!new) {
	FPRINTF(stderr, "Out of memory in file %s, line %d\n", fname, lineno);
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
    DBG_FPRINTF(stderr, "ADIOI_Malloc %s:<%d> %p (%#zX)\n", fname, lineno, new, size);
    return new;
}


void *ADIOI_Calloc_fn(size_t nelem, size_t elsize, int lineno, const char *fname)
{
    void *new;

#ifdef HAVE_MPIU_FUNCS
    new = (void *) MPIU_Calloc(nelem, elsize);
#else
    new = (void *) calloc(nelem, elsize);
#endif
    if (!new) {
	FPRINTF(stderr, "Out of memory in file %s, line %d\n", fname, lineno);
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
    DBG_FPRINTF(stderr, "ADIOI_Calloc %s:<%d> %p\n", fname, lineno, new);
    return new;
}


void *ADIOI_Realloc_fn(void *ptr, size_t size, int lineno, const char *fname)
{
    void *new;

#ifdef HAVE_MPIU_FUNCS
    new = (void *) MPIU_Realloc(ptr, size);
#else
    new = (void *) realloc(ptr, size);
#endif
    if (!new) {
	FPRINTF(stderr, "realloc failed in file %s, line %d\n", fname, lineno);
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
    DBG_FPRINTF(stderr, "ADIOI_Realloc %s:<%d> %p\n", fname, lineno, new);
    return new;
}


void ADIOI_Free_fn(void *ptr, int lineno, const char *fname)
{
    DBG_FPRINTF(stderr, "ADIOI_Free %s:<%d> %p\n", fname, lineno, ptr);
    if (!ptr) {
	FPRINTF(stderr, "Attempt to free null pointer in file %s, line %d\n", fname, lineno);
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

#ifdef HAVE_MPIU_FUNCS
    MPIU_Free(ptr);
#else
    free(ptr);
#endif
}


