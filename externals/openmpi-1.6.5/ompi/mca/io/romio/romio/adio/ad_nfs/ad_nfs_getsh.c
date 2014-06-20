/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_nfs.h"

/* returns the current location of the shared_fp in terms of the
   no. of etypes relative to the current view, and also increments the
   shared_fp by the number of etypes to be accessed (incr) in the read
   or write following this function. */

void ADIOI_NFS_Get_shared_fp(ADIO_File fd, int incr, ADIO_Offset *shared_fp, 
			 int *error_code)
{
    ADIO_Offset new_fp;
    int err;
    MPI_Comm dupcommself;
    static char myname[] = "ADIOI_NFS_GET_SHARED_FP";

    if (fd->shared_fp_fd == ADIO_FILE_NULL) {
	MPI_Comm_dup(MPI_COMM_SELF, &dupcommself);
	fd->shared_fp_fd = ADIO_Open(MPI_COMM_SELF, dupcommself,
				     fd->shared_fp_fname, 
				     fd->file_system,
				     fd->fns,
				     ADIO_CREATE | ADIO_RDWR | ADIO_DELETE_ON_CLOSE, 
				     0, MPI_BYTE, MPI_BYTE, MPI_INFO_NULL, 
				     ADIO_PERM_NULL, error_code);
	if (*error_code != MPI_SUCCESS) return;
	*shared_fp = 0;
	ADIOI_WRITE_LOCK(fd->shared_fp_fd, 0, SEEK_SET, sizeof(ADIO_Offset));
#ifdef ADIOI_MPE_LOGGING
        MPE_Log_event( ADIOI_MPE_read_a, 0, NULL );
#endif
	err = read(fd->shared_fp_fd->fd_sys, shared_fp, sizeof(ADIO_Offset));
#ifdef ADIOI_MPE_LOGGING
        MPE_Log_event( ADIOI_MPE_read_b, 0, NULL );
#endif
        /* if the file is empty, the above read may return error
           (reading beyond end of file). In that case, shared_fp = 0, 
           set above, is the correct value. */
    }
    else {
	ADIOI_WRITE_LOCK(fd->shared_fp_fd, 0, SEEK_SET, sizeof(ADIO_Offset));

#ifdef ADIOI_MPE_LOGGING
        MPE_Log_event( ADIOI_MPE_lseek_a, 0, NULL );
#endif
	err = lseek(fd->shared_fp_fd->fd_sys, 0, SEEK_SET);
#ifdef ADIOI_MPE_LOGGING
        MPE_Log_event( ADIOI_MPE_lseek_b, 0, NULL );
#endif
	if (err == 0) {
#ifdef ADIOI_MPE_LOGGING
            MPE_Log_event( ADIOI_MPE_read_a, 0, NULL );
#endif
	    err = read(fd->shared_fp_fd->fd_sys, shared_fp,
		       sizeof(ADIO_Offset));
#ifdef ADIOI_MPE_LOGGING
            MPE_Log_event( ADIOI_MPE_read_b, 0, NULL );
#endif
	}
	if (err == -1) {
	    ADIOI_UNLOCK(fd->shared_fp_fd, 0, SEEK_SET, sizeof(ADIO_Offset));
	    *error_code = MPIO_Err_create_code(MPI_SUCCESS,
					       MPIR_ERR_RECOVERABLE, myname,
					       __LINE__, MPI_ERR_IO, "**io",
					       "**io %s", strerror(errno));
	    return;
	}
    }

    new_fp = *shared_fp + incr;

#ifdef ADIOI_MPE_LOGGING
    MPE_Log_event( ADIOI_MPE_lseek_a, 0, NULL );
#endif
    err = lseek(fd->shared_fp_fd->fd_sys, 0, SEEK_SET);
#ifdef ADIOI_MPE_LOGGING
    MPE_Log_event( ADIOI_MPE_lseek_b, 0, NULL );
#endif
    if (err == 0) {
#ifdef ADIOI_MPE_LOGGING
        MPE_Log_event( ADIOI_MPE_write_a, 0, NULL );
#endif
	err = write(fd->shared_fp_fd->fd_sys, &new_fp, sizeof(ADIO_Offset));
#ifdef ADIOI_MPE_LOGGING
        MPE_Log_event( ADIOI_MPE_write_b, 0, NULL );
#endif
    }
    ADIOI_UNLOCK(fd->shared_fp_fd, 0, SEEK_SET, sizeof(ADIO_Offset));
    if (err == -1) {
	*error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					   myname, __LINE__, MPI_ERR_IO,
					   "**io",
					   "**io %s", strerror(errno));
    }
    else *error_code = MPI_SUCCESS;
}
