/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_zoidfs.h"
#include "ad_zoidfs_common.h"

/* as with flush, implement the resize operation in a scalable
 * manner. one process does the work, then broadcasts the result to everyone
 * else.  fortunately, this operation is defined to be collective */
void ADIOI_ZOIDFS_Resize(ADIO_File fd, ADIO_Offset size, int *error_code)
{
    int ret, rank;
    ADIOI_ZOIDFS_object *zoidfs_obj_ptr;
    static char myname[] = "ADIOI_ZOIDFS_RESIZE";

    *error_code = MPI_SUCCESS;

    zoidfs_obj_ptr = (ADIOI_ZOIDFS_object *)fd->fs_ptr;

    MPI_Comm_rank(fd->comm, &rank);


    /* MPI-IO semantics treat conflicting MPI_File_set_size requests the
     * same as conflicting write requests. Thus, a resize from one
     * process does not have to be visible to the other processes until a
     * syncronization point is reached */

    if (rank == fd->hints->ranklist[0]) {
        NO_STALE(ret, fd, zoidfs_obj_ptr,
                 zoidfs_resize(zoidfs_obj_ptr, size, ZOIDFS_NO_OP_HINT));
	MPI_Bcast(&ret, 1, MPI_INT, fd->hints->ranklist[0], fd->comm);
    } else  {
	MPI_Bcast(&ret, 1, MPI_INT, fd->hints->ranklist[0], fd->comm);
    }
    /* --BEGIN ERROR HANDLING-- */
    if (ret != ZFS_OK) {
	*error_code = MPIO_Err_create_code(MPI_SUCCESS,
					   MPIR_ERR_RECOVERABLE,
					   myname, __LINE__,
					   ADIOI_ZOIDFS_error_convert(ret),
					   "Error in zoidfs_resize", 0);
	return;
    }
    /* --END ERROR HANDLING-- */
}

/*
 * vim: ts=8 sts=4 sw=4 noexpandtab 
 */
