/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 2003 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_zoidfs.h"
#include "adio.h"

#include "ad_zoidfs_common.h"

void ADIOI_ZOIDFS_Delete(char *filename, int *error_code)
{
    int ret;
    static char myname[] = "ADIOI_ZOIDFS_DELETE";

    ADIOI_ZOIDFS_Init(0, error_code);
    /* --BEGIN ERROR HANDLING-- */
    if (*error_code != MPI_SUCCESS) 
    {
	/* ADIOI_ZOIDFS_INIT handles creating error codes itself */
	return;
    }
    /* --END ERROR HANDLING-- */

    ret = zoidfs_remove(NULL, NULL, filename, NULL, ZOIDFS_NO_OP_HINT);
    /* --BEGIN ERROR HANDLING-- */
    if (ret != ZFS_OK) {
	*error_code = MPIO_Err_create_code(MPI_SUCCESS,
					   MPIR_ERR_RECOVERABLE,
					   myname, __LINE__,
					   ADIOI_ZOIDFS_error_convert(ret),
					   "Error in zoidfs_remove", 0);
	return;
    }
    /* --END ERROR HANDLING-- */

    *error_code = MPI_SUCCESS;
    return;
}

/* 
 * vim: ts=8 sts=4 sw=4 noexpandtab 
 */
