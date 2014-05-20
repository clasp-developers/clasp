/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_nfs.h"

#ifdef ROMIO_HAVE_WORKING_AIO
/* nearly identical to ADIOI_GEN_IreadContig, except we lock around I/O */
void ADIOI_NFS_IreadContig(ADIO_File fd, void *buf, int count, 
			   MPI_Datatype datatype, int file_ptr_type,
			   ADIO_Offset offset, ADIO_Request *request,
			   int *error_code)  
{
    int len, typesize;
    int aio_errno = 0;
    static char myname[] = "ADIOI_NFS_IREADCONTIG";

    MPI_Type_size(datatype, &typesize);
    len = count * typesize;

    if (file_ptr_type == ADIO_INDIVIDUAL) offset = fd->fp_ind;
    aio_errno = ADIOI_NFS_aio(fd, buf, len, offset, 0, request);
    if (file_ptr_type == ADIO_INDIVIDUAL) fd->fp_ind += len;

    fd->fp_sys_posn = -1;

    if (aio_errno != 0) {
	/* --BEGIN ERROR HANDLING-- */
	MPIO_ERR_CREATE_CODE_ERRNO(myname, aio_errno, error_code);
	return;
	/* --END ERROR HANDLING-- */
    }
    else *error_code = MPI_SUCCESS;
}
#endif
