/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_ntfs.h"

void ADIOI_NTFS_IreadContig(ADIO_File fd, void *buf, int count, 
                MPI_Datatype datatype, int file_ptr_type,
                ADIO_Offset offset, ADIO_Request *request, int *error_code)  
{
    int len, typesize;
    int err;
    static char myname[] = "ADIOI_NTFS_IreadContig";

    MPI_Type_size(datatype, &typesize);
    len = count * typesize;

    if (file_ptr_type == ADIO_INDIVIDUAL)
    {
	offset = fd->fp_ind;
    }
    err = ADIOI_NTFS_aio(fd, buf, len, offset, 0, request);
    if (file_ptr_type == ADIO_INDIVIDUAL)
    {
	fd->fp_ind += len;
    }

    /* --BEGIN ERROR HANDLING-- */
    if (err != MPI_SUCCESS)
    {
	*error_code = MPIO_Err_create_code(err, MPIR_ERR_RECOVERABLE,
					   myname, __LINE__, MPI_ERR_IO,
					   "**io", 0);
	return;
    }
    /* --END ERROR HANDLING-- */
    *error_code = MPI_SUCCESS;

    fd->fp_sys_posn = -1;   /* set it to null. */
}
