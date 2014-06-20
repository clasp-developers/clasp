/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_ntfs.h"

void ADIOI_NTFS_Flush(ADIO_File fd, int *error_code)
{
    int err;
    static char myname[] = "ADIOI_NTFS_Flush";

    err = (fd->access_mode & ADIO_RDONLY) ? TRUE :
	FlushFileBuffers(fd->fd_sys);

    /* --BEGIN ERROR HANDLING-- */
    if (err == FALSE)
    {
    char errMsg[ADIOI_NTFS_ERR_MSG_MAX];
	err = GetLastError();
    ADIOI_NTFS_Strerror(err, errMsg, ADIOI_NTFS_ERR_MSG_MAX);
	*error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					   myname, __LINE__, MPI_ERR_IO,
					   "**io",
					   "**io %s", errMsg);
	return;
    }
    /* --END ERROR HANDLING-- */
    *error_code = MPI_SUCCESS;
}
