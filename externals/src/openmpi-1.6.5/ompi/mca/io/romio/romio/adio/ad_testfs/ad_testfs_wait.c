/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 2001 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_testfs.h"
#include "adioi.h"

void ADIOI_TESTFS_ReadComplete(ADIO_Request *request, ADIO_Status *status, int
			       *error_code)
{
    int myrank, nprocs;

    *error_code = MPI_SUCCESS;

    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &myrank);
    FPRINTF(stdout, "[%d/%d] ADIOI_TESTFS_ReadComplete called \n", 
	    myrank, nprocs);

    /* do something with status set bytes? */
}

void ADIOI_TESTFS_WriteComplete(ADIO_Request *request, ADIO_Status *status, int
				*error_code)
{
    int myrank, nprocs;

    *error_code = MPI_SUCCESS;

    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &myrank);
    FPRINTF(stdout, "[%d/%d] ADIOI_TESTFS_WriteComplete called\n", 
	    myrank, nprocs);

    /* do something with status_set_bytes? */
}
