/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_nfs.h"

void ADIOI_NFS_ReadComplete(ADIO_Request *request, ADIO_Status *status,
			    int *error_code)
{
	return;
}


void ADIOI_NFS_WriteComplete(ADIO_Request *request, ADIO_Status *status,
			     int *error_code)
{
    ADIOI_NFS_ReadComplete(request, status, error_code);
}
