/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_zoidfs.h"
#include "adio_extern.h"
#include "ad_zoidfs_common.h"

void ADIOI_ZOIDFS_Fcntl(ADIO_File fd, int flag, ADIO_Fcntl_t *fcntl_struct,
		       int *error_code)
{
    int ret;
    zoidfs_attr_t attr;
    ADIOI_ZOIDFS_object *zoidfs_obj_ptr;
    static char myname[] = "ADIOI_ZOIDFS_FCNTL";

    zoidfs_obj_ptr = (ADIOI_ZOIDFS_object*)fd->fs_ptr;

    switch(flag) {
    case ADIO_FCNTL_GET_FSIZE:
	attr.mask = ZOIDFS_ATTR_SIZE;
        NO_STALE(ret, fd, zoidfs_obj_ptr,
                 zoidfs_getattr(zoidfs_obj_ptr, &attr, ZOIDFS_NO_OP_HINT));
	if ( !(attr.mask & ZOIDFS_ATTR_SIZE) || (ret != ZFS_OK ) ) {
	    /* --BEGIN ERROR HANDLING-- */
	    *error_code = MPIO_Err_create_code(MPI_SUCCESS,
					       MPIR_ERR_RECOVERABLE,
					       myname, __LINE__,
					       ADIOI_ZOIDFS_error_convert(ret),
					       "Error in zoidfs_getattr", 0);
	    /* --END ERROR HANDLING-- */
	}
	else {
	    *error_code = MPI_SUCCESS;
	}
	fcntl_struct->fsize = attr.size;
	return;

    case ADIO_FCNTL_SET_DISKSPACE:
	ADIOI_GEN_Prealloc(fd, fcntl_struct->diskspace, error_code);
	break;

    /* --BEGIN ERROR HANDLING-- */
    case ADIO_FCNTL_SET_ATOMICITY:
    default:
	*error_code = MPIO_Err_create_code(MPI_SUCCESS,
					   MPIR_ERR_RECOVERABLE,
					   myname, __LINE__,
					   MPI_ERR_ARG,
					   "**flag", "**flag %d", flag);
    /* --END ERROR HANDLING-- */
    }
}

/* 
 * vim: ts=8 sts=4 sw=4 noexpandtab 
 */
