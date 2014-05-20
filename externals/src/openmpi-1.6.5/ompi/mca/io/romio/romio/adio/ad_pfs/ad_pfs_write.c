/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_pfs.h"

void ADIOI_PFS_WriteContig(ADIO_File fd, void *buf, int count, 
			   MPI_Datatype datatype, int file_ptr_type,
			   ADIO_Offset offset, ADIO_Status *status,
			   int *error_code)
{
    int err=-1, datatype_size, len;
    static char myname[] = "ADIOI_PFS_WRITECONTIG";

    MPI_Type_size(datatype, &datatype_size);
    len = datatype_size * count;

    if (file_ptr_type == ADIO_EXPLICIT_OFFSET) {
        if (fd->fp_sys_posn != offset) {
            lseek(fd->fd_sys, offset, SEEK_SET);
	}
        err = _cwrite(fd->fd_sys, buf, len);
        fd->fp_sys_posn = offset + err;
         /* individual file pointer not updated */        
    }
    else { /* write from curr. location of ind. file pointer */
        if (fd->fp_sys_posn != fd->fp_ind) {
            lseek(fd->fd_sys, fd->fp_ind, SEEK_SET);
	}
        err = _cwrite(fd->fd_sys, buf, len);
        fd->fp_ind += err;
        fd->fp_sys_posn = fd->fp_ind;
    }

#ifdef HAVE_STATUS_SET_BYTES
    if (err != -1) MPIR_Status_set_bytes(status, datatype, err);
#endif

    if (err == -1) {
	*error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					   myname, __LINE__, MPI_ERR_IO,
					   "**io",
					   "**io %s", strerror(errno));
    }
    else *error_code = MPI_SUCCESS;
}
