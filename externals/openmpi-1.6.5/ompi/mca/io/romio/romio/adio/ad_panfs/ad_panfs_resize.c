/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 2004 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_panfs.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

void ADIOI_PANFS_Resize(ADIO_File fd, ADIO_Offset size, int *error_code)
{
    int err;
    int myrank;
    struct stat stat_buf;
    static char myname[] = "ADIOI_PANFS_RESIZE";

    MPI_Comm_rank(fd->comm, &myrank);
    if (!myrank)
    {
        AD_PANFS_RETRY(ftruncate(fd->fd_sys,size),err);
        MPI_Barrier(fd->comm);
    }
    else
    {
        MPI_Barrier(fd->comm);
        AD_PANFS_RETRY(fstat(fd->fd_sys,&stat_buf),err);
        if(((ADIO_Offset)stat_buf.st_size) != size)
        {
            /* This should never happen otherwise there is a coherency problem. */
            FPRINTF(stderr, "%s: Rank %d: Resize failed: requested=%llu actual=%llu.\n",myname,myrank,size,stat_buf.st_size);
            MPI_Abort(MPI_COMM_WORLD, 1);
        }
    }

    /* --BEGIN ERROR HANDLING-- */
    if (err == -1) {
	*error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					   myname, __LINE__, MPI_ERR_IO,
					   "**io", "**io %s", strerror(errno));
	return;
    }
    /* --END ERROR HANDLING-- */

    *error_code = MPI_SUCCESS;
}
