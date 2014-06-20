/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 2004 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_AIO_H
#include <aio.h>
#endif
#ifdef HAVE_SYS_AIO_H
#include <sys/aio.h>
#endif

#include "mpiu_greq.h"

#ifdef ROMIO_HAVE_WORKING_AIO
/* ADIOI_GEN_IreadContig
 *
 * This code handles two distinct cases.  If ROMIO_HAVE_WORKING_AIO is not
 * defined, then I/O is performed in a blocking manner.  Otherwise we post
 * an asynchronous I/O operation using the appropriate aio routines. 
 *
 * In the aio case we rely on ADIOI_GEN_aio(), which is implemented in
 * common/ad_iwrite.c.
 */
void ADIOI_GEN_IreadContig(ADIO_File fd, void *buf, int count, 
			   MPI_Datatype datatype, int file_ptr_type,
			   ADIO_Offset offset, MPI_Request *request,
			   int *error_code)  
{
    int len, typesize;
    int aio_errno = 0;
    static char myname[] = "ADIOI_GEN_IREADCONTIG";

    MPI_Type_size(datatype, &typesize);
    ADIOI_Assert((count * typesize) == ((ADIO_Offset)(unsigned)count * (ADIO_Offset)typesize));
    len = count * typesize;

    if (file_ptr_type == ADIO_INDIVIDUAL) offset = fd->fp_ind;
    aio_errno = ADIOI_GEN_aio(fd, buf, len, offset, 0, request);
    if (file_ptr_type == ADIO_INDIVIDUAL) fd->fp_ind += len;

    fd->fp_sys_posn = -1;

    /* --BEGIN ERROR HANDLING-- */
    if (aio_errno != 0) {
	MPIO_ERR_CREATE_CODE_ERRNO(myname, aio_errno, error_code);
	return;
    }
    /* --END ERROR HANDLING-- */
    
    *error_code = MPI_SUCCESS;
}
#endif

/* Generic implementation of IreadStrided calls the blocking ReadStrided
 * immediately.
 */
void ADIOI_GEN_IreadStrided(ADIO_File fd, void *buf, int count, 
			    MPI_Datatype datatype, int file_ptr_type,
			    ADIO_Offset offset, ADIO_Request *request,
			    int *error_code)
{
    ADIO_Status status;
    int typesize;
    MPI_Offset nbytes=0;

    /* Call the blocking function.  It will create an error code
     * if necessary.
     */
    ADIO_ReadStrided(fd, buf, count, datatype, file_ptr_type, 
		     offset, &status, error_code);  

    if (*error_code == MPI_SUCCESS) {
	MPI_Type_size(datatype, &typesize);
	nbytes = (MPI_Offset)count*(MPI_Offset)typesize;
    }
    MPIO_Completed_request_create(&fd, nbytes, error_code, request);
}
