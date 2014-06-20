/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_zoidfs.h"
#include "ad_zoidfs_common.h"

/* we want to be a bit clever here:  at scale, if every client sends a
 * flush request, it will stress the file system with redundant
 * commit requests.  Instead, one process should wait for
 * everyone to catch up, do the sync, then broadcast the result.  
 */

void ADIOI_ZOIDFS_Flush(ADIO_File fd, int *error_code) 
{ 
    int ret, rank, dummy=0, dummy_in=0; 
    ADIOI_ZOIDFS_object *zoidfs_obj_ptr;
    static char myname[] = "ADIOI_ZOIDFS_FLUSH";

    *error_code = MPI_SUCCESS;

    zoidfs_obj_ptr = (ADIOI_ZOIDFS_object*)fd->fs_ptr;

    MPI_Comm_rank(fd->comm, &rank);

    /* collective call to ensure no outstanding write requests. reduce is
     * slightly less expensvie than barrier */
    MPI_Reduce(&dummy_in, &dummy, 1, MPI_INT, MPI_SUM, 
	    fd->hints->ranklist[0], fd->comm);

    if (rank == fd->hints->ranklist[0]) {
	ret = zoidfs_commit(zoidfs_obj_ptr, ZOIDFS_NO_OP_HINT);
    }
    MPI_Bcast(&ret, 1, MPI_INT, fd->hints->ranklist[0], fd->comm);

    /* --BEGIN ERROR HANDLING-- */
    if (ret != 0) {
	*error_code = MPIO_Err_create_code(MPI_SUCCESS,
					   MPIR_ERR_RECOVERABLE,
					   myname, __LINE__,
					   ADIOI_ZOIDFS_error_convert(ret),
					   "Error in zoidfs_commit", 0);
    }
    /* --END ERROR HANDLING-- */
}

/* 
 * vim: ts=8 sts=4 sw=4 noexpandtab 
 */
