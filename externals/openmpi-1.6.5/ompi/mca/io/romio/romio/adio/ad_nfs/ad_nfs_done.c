/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_nfs.h"

int ADIOI_NFS_ReadDone(ADIO_Request *request, ADIO_Status *status,
		       int *error_code)
{
	*error_code = MPI_SUCCESS;
	return 1;
}
int ADIOI_NFS_WriteDone(ADIO_Request *request, ADIO_Status *status,
			int *error_code)
{
    return ADIOI_NFS_ReadDone(request, status, error_code);
} 
