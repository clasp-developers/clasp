/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_pvfs.h"

void ADIOI_PVFS_Close(ADIO_File fd, int *error_code)
{
    int err;
    static char myname[] = "ADIOI_PVFS_CLOSE";

#ifdef ADIOI_MPE_LOGGING
    MPE_Log_event( ADIOI_MPE_close_a, 0, NULL );
#endif
    err = pvfs_close(fd->fd_sys);
#ifdef ADIOI_MPE_LOGGING
    MPE_Log_event( ADIOI_MPE_close_b, 0, NULL );
#endif
    fd->fd_sys = -1;

    if (err == -1) {
	*error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					   myname, __LINE__, MPI_ERR_IO,
					   "**io",
					   "**io %s", strerror(errno));
    }
    else *error_code = MPI_SUCCESS;
}
