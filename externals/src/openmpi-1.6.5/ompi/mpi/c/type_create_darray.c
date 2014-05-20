/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/memchecker.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Type_create_darray = PMPI_Type_create_darray
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Type_create_darray";

static int block(const int *array_of_gsizes, int dim, int ndims, int nprocs,
                 int rank, int darg, int order, ptrdiff_t orig_extent,
                 ompi_datatype_t *type_old, ompi_datatype_t **type_new,
                 ptrdiff_t *st_offset);
static int cyclic(const int *array_of_gsizes, int dim, int ndims, int nprocs,
                  int rank, int darg, int order, ptrdiff_t orig_extent,
                  ompi_datatype_t* type_old, ompi_datatype_t **type_new,
                  ptrdiff_t *st_offset);


int MPI_Type_create_darray(int size,
                           int rank,
                           int ndims,
                           int gsize_array[],
                           int distrib_array[],
                           int darg_array[],
                           int psize_array[],
                           int order,
                           MPI_Datatype oldtype,
                           MPI_Datatype *newtype)

{
    ompi_datatype_t *lastType;
    ptrdiff_t orig_extent, *st_offsets = NULL;
    int i, start_loop, end_loop, step;
    int *coords = NULL, rc = OMPI_SUCCESS;

    MEMCHECKER(
        memchecker_datatype(oldtype);
    );

    if (MPI_PARAM_CHECK) {
        int prod_psize = 1;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if( (rank < 0) || (size < 0) || (rank >= size) ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
        } else if( ndims < 0 ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COUNT, FUNC_NAME);
        } else if( (NULL == gsize_array) || (NULL == distrib_array) || (NULL == darg_array) || (NULL == psize_array)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
        } else if (NULL == newtype) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_TYPE, FUNC_NAME);
        } else if( !(OPAL_DATATYPE_FLAG_DATA & oldtype->super.flags) ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_TYPE, FUNC_NAME);
        } else if( (MPI_ORDER_C != order) && (MPI_ORDER_FORTRAN != order) ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
        }
        for( i = 0; i < ndims; i++ ) {
            if( (MPI_DISTRIBUTE_BLOCK != distrib_array[i]) &&
                (MPI_DISTRIBUTE_CYCLIC != distrib_array[i]) &&
                (MPI_DISTRIBUTE_NONE != distrib_array[i]) ) {
                return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
            } else if( (gsize_array[i] < 1) || (psize_array[i] < 0) ||
                       ((darg_array[i] < 0) && (MPI_DISTRIBUTE_DFLT_DARG != darg_array[i]) ) ) {
                return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
            } else if( (MPI_DISTRIBUTE_DFLT_DARG != darg_array[i]) &&
                       (MPI_DISTRIBUTE_BLOCK == distrib_array[i]) &&
                       ((darg_array[i] * psize_array[i]) < gsize_array[i]) ) {
                return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
            } else if( 1 > psize_array[i] )
                return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
            prod_psize *= psize_array[i];
        }
        if( prod_psize != size )
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
    }

    OPAL_CR_ENTER_LIBRARY();

    /* speedy corner case */
    if (ndims < 1) {
        /* Don't just return MPI_DATATYPE_NULL as that can't be
           MPI_TYPE_FREE()ed, and that seems bad */
        *newtype = ompi_datatype_create(0);
        ompi_datatype_add(*newtype, &ompi_mpi_datatype_null.dt, 0, 0, 0);
        OPAL_CR_EXIT_LIBRARY();
        return MPI_SUCCESS;
    }

    rc = ompi_datatype_type_extent(oldtype, &orig_extent);
    if (MPI_SUCCESS != rc) goto cleanup;

    /* calculate position in grid using row-major ordering */
    {
        int tmp_rank = rank, procs = size;

        coords = (int *) malloc(ndims * sizeof(int));
        for (i = 0 ; i < ndims ; i++) {
            procs = procs / psize_array[i];
            coords[i] = tmp_rank / procs;
            tmp_rank = tmp_rank % procs;
        }
    }

    st_offsets = (ptrdiff_t *) malloc(ndims * sizeof(ptrdiff_t));

    /* duplicate type to here to 1) deal with constness without
       casting and 2) eliminate need to for conditional destroy below.
       Lame, yes.  But cleaner code all around. */
    rc = ompi_datatype_duplicate(oldtype, &lastType);
    if (OMPI_SUCCESS != rc) goto cleanup;

    /* figure out ordering issues */
    if (MPI_ORDER_C == order) {
        start_loop = ndims - 1 ; step = -1; end_loop = -1;
    } else {
        start_loop = 0 ; step = 1; end_loop = ndims;
    }

    /* Build up array */
    for (i = start_loop ; i != end_loop; i += step) {
        int nprocs, tmp_rank;

        switch(distrib_array[i]) {
        case MPI_DISTRIBUTE_BLOCK:
            rc = block(gsize_array, i, ndims, psize_array[i], coords[i],
                       darg_array[i], order, orig_extent,
                       lastType, newtype, st_offsets+i);
            break;
        case MPI_DISTRIBUTE_CYCLIC:
            rc = cyclic(gsize_array, i, ndims, psize_array[i], coords[i],
                        darg_array[i], order, orig_extent,
                        lastType, newtype, st_offsets+i);
            break;
        case MPI_DISTRIBUTE_NONE:
            /* treat it as a block distribution on 1 process */
            if (order == MPI_ORDER_C) {
                nprocs = psize_array[i]; tmp_rank = coords[i];
            } else {
                nprocs = 1; tmp_rank = 0;
            }

            rc = block(gsize_array, i, ndims, nprocs, tmp_rank,
                       MPI_DISTRIBUTE_DFLT_DARG, order, orig_extent,
                       lastType, newtype, st_offsets+i);
            break;
        default:
            rc = MPI_ERR_ARG;
        }
        ompi_datatype_destroy(&lastType);
        /* need to destroy the old type even in error condition, so
           don't check return code from above until after cleanup. */
        if (MPI_SUCCESS != rc) goto cleanup;
        lastType = *newtype;
    }


    /* set displacement and UB correctly.  Use struct instead of
       resized for same reason as subarray */
    {
        ptrdiff_t displs[3];
        ompi_datatype_t *types[3];
        int tmp_size, blength[3] = { 1, 1, 1};

        displs[1] = st_offsets[start_loop];
        tmp_size = 1;
        for (i = start_loop + step ; i != end_loop ; i += step) {
            tmp_size *= gsize_array[i - step];
            displs[1] += tmp_size * st_offsets[i];
        }

        displs[0] = 0;
        displs[1] *= orig_extent;
        displs[2] = orig_extent;
        for (i = 0 ; i < ndims ; i++) {
            displs[2] *= gsize_array[i];
        }
        types[0] = MPI_LB; types[1] = lastType; types[2] = MPI_UB;

        rc = ompi_datatype_create_struct(3, blength, displs, types, newtype);
        ompi_datatype_destroy(&lastType);
        /* need to destroy the old type even in error condition, so
           don't check return code from above until after cleanup. */
        if (MPI_SUCCESS != rc) goto cleanup;
    }

    {
        int* a_i[8];

        a_i[0] = &size;
        a_i[1] = &rank;
        a_i[2] = &ndims;
        a_i[3] = gsize_array;
        a_i[4] = distrib_array;
        a_i[5] = darg_array;
        a_i[6] = psize_array;
        a_i[7] = &order;

        ompi_datatype_set_args( *newtype, 4 * ndims + 4, a_i, 0, NULL, 1, &oldtype,
                           MPI_COMBINER_DARRAY );
    }

 cleanup:
    if (NULL != st_offsets) free(st_offsets);
    if (NULL != coords) free(coords);

    OPAL_CR_EXIT_LIBRARY();

    OMPI_ERRHANDLER_RETURN(rc, MPI_COMM_WORLD, rc, FUNC_NAME);
}


static int
block(const int *gsize_array, int dim, int ndims, int nprocs,
      int rank, int darg, int order, ptrdiff_t orig_extent,
      ompi_datatype_t *type_old, ompi_datatype_t **type_new,
      ptrdiff_t *st_offset)
{
    int blksize, global_size, mysize, i, j, rc, start_loop, step;
    ptrdiff_t stride;

    global_size = gsize_array[dim];

    if (darg == MPI_DISTRIBUTE_DFLT_DARG)
        blksize = (global_size + nprocs - 1) / nprocs;
    else {
        blksize = darg;
    }

    j = global_size - blksize*rank;
    mysize = blksize < j ? blksize : j;
    if (mysize < 0) mysize = 0;

    if (MPI_ORDER_C == order) {
        start_loop = ndims - 1 ; step = -1;
    } else {
        start_loop = 0 ; step = 1;
    }

    stride = orig_extent;
    if (dim == start_loop) {
        rc = ompi_datatype_create_contiguous(mysize, type_old, type_new);
        if (OMPI_SUCCESS != rc) return rc;
    } else {
        for (i = start_loop ; i != dim ; i += step) {
            stride *= gsize_array[i];
        }
        rc = ompi_datatype_create_hvector(mysize, 1, stride, type_old, type_new);
        if (OMPI_SUCCESS != rc) return rc;
    }

    *st_offset = blksize * rank;
     /* in terms of no. of elements of type oldtype in this dimension */
    if (mysize == 0) *st_offset = 0;

    return OMPI_SUCCESS;
}


static int
cyclic(const int *gsize_array, int dim, int ndims, int nprocs,
       int rank, int darg, int order, ptrdiff_t orig_extent,
       ompi_datatype_t* type_old, ompi_datatype_t **type_new,
       ptrdiff_t *st_offset)
{
    int blksize, i, blklens[2], st_index, end_index, local_size, rem, count, rc;
    ptrdiff_t stride, disps[2];
    ompi_datatype_t *type_tmp, *types[2];

    if (darg == MPI_DISTRIBUTE_DFLT_DARG) {
        blksize = 1;
    } else {
        blksize = darg;
    }

    st_index = rank * blksize;
    end_index = gsize_array[dim] - 1;

    if (end_index < st_index) {
        local_size = 0;
    } else {
	local_size = ((end_index - st_index + 1)/(nprocs*blksize))*blksize;
	rem = (end_index - st_index + 1) % (nprocs*blksize);
	local_size += rem < blksize ? rem : blksize;
    }

    count = local_size / blksize;
    rem = local_size % blksize;

    stride = nprocs*blksize*orig_extent;
    if (order == MPI_ORDER_FORTRAN) {
	for (i=0; i<dim; i++) {
            stride *= gsize_array[i];
        }
    } else {
        for (i=ndims-1; i>dim; i--) {
            stride *= gsize_array[i];
        }
    }

    rc = ompi_datatype_create_hvector(count, blksize, stride, type_old, type_new);
    if (OMPI_SUCCESS != rc) return rc;

    if (rem) {
	/* if the last block is of size less than blksize, include
	   it separately using MPI_Type_struct */

	types[0] = *type_new;
	types[1] = type_old;
	disps[0] = 0;
	disps[1] = count*stride;
	blklens[0] = 1;
	blklens[1] = rem;

        rc = ompi_datatype_create_struct(2, blklens, disps, types, &type_tmp);
        ompi_datatype_destroy(type_new);
        /* even in error condition, need to destroy type_new, so check
           for error after destroy. */
        if (OMPI_SUCCESS != rc) return rc;
	*type_new = type_tmp;
    }

    /* need to set the UB for block-cyclic to work */
    types[0] = *type_new;
    types[1] = MPI_UB;
    disps[0] = 0;
    disps[1] = orig_extent;
    if (order == MPI_ORDER_FORTRAN) {
	for (i=0; i<=dim; i++) {
            disps[1] *= gsize_array[i];
        }
    } else {
        for (i=ndims-1; i>=dim; i--) {
            disps[1] *= gsize_array[i];
        }
    }
    blklens[0] = blklens[1] = 1;
    rc = ompi_datatype_create_struct(2, blklens, disps, types, &type_tmp);
    ompi_datatype_destroy(type_new);
        /* even in error condition, need to destroy type_new, so check
           for error after destroy. */
    if (OMPI_SUCCESS != rc) return rc;
    *type_new = type_tmp;

    *st_offset = rank * blksize;
     /* in terms of no. of elements of type oldtype in this dimension */
    if (local_size == 0) *st_offset = 0;

    return OMPI_SUCCESS;
}
