/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 2004 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"
#include "mpiu_greq.h"

/* In cases where nonblocking operation will carry out blocking version,
 * instantiate and complete a generalized request  */

void MPIO_Completed_request_create(MPI_File *fh, MPI_Offset bytes,
		int *error_code, MPI_Request *request)
{
	MPI_Status *status;
	status = (MPI_Status *)ADIOI_Malloc(sizeof(MPI_Status));

	status->MPI_ERROR = *error_code;
#ifdef HAVE_STATUS_SET_BYTES
	MPIR_Status_set_bytes(status, MPI_BYTE, bytes);
#endif
	/* --BEGIN ERROR HANDLING-- */
	if (*error_code != MPI_SUCCESS)
		*error_code = MPIO_Err_return_file(*fh, *error_code);
	/* --END ERROR HANDLING-- */
	MPI_Grequest_start(MPIU_Greq_query_fn, MPIU_Greq_free_fn, 
			MPIU_Greq_cancel_fn, status, request);
	MPI_Grequest_complete(*request);
}
