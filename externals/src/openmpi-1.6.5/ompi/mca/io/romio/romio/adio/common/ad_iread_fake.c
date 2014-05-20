/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 2004 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"
#include "mpiu_greq.h"

/* Generic implementation of IreadContig calls the blocking ReadContig
 * immediately.
 */
void ADIOI_FAKE_IreadContig(ADIO_File fd, void *buf, int count, 
			   MPI_Datatype datatype, int file_ptr_type,
			   ADIO_Offset offset, ADIO_Request *request,
			   int *error_code)  
{
    ADIO_Status status;
    int typesize;
    MPI_Offset len;

    MPI_Type_size(datatype, &typesize);
    len = (MPI_Offset)count * (MPI_Offset)typesize;

    /* Call the blocking function.  It will create an error code
     * if necessary.
     */
    ADIOI_Assert(len == (int) len); /* the count is an int parm */
    ADIO_ReadContig(fd, buf, (int)len, MPI_BYTE, file_ptr_type, offset, 
		    &status, error_code);  
    if (*error_code != MPI_SUCCESS) {
	    len=0;
    }
    MPIO_Completed_request_create(&fd, len, error_code, request);
}


/* Generic implementation of IreadStrided calls the blocking ReadStrided
 * immediately.
 */
void ADIOI_FAKE_IreadStrided(ADIO_File fd, void *buf, int count, 
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
