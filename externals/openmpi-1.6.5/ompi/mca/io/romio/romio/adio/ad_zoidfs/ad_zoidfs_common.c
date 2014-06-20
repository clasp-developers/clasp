/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   Copyright (C) 2003 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_zoidfs.h"
#include "ad_zoidfs_common.h"
#include <unistd.h>
#include <sys/types.h>

/* keyval hack to both tell us if we've already initialized zoidfs and also
 * close it down when mpi exits */
int ADIOI_ZOIDFS_Initialized = MPI_KEYVAL_INVALID;

void ADIOI_ZOIDFS_End(int *error_code)
{
    int ret;
    static char myname[] = "ADIOI_ZOIDFS_END";

    ret = zoidfs_finalize();

    /* --BEGIN ERROR HANDLING-- */
    if (ret != 0 ) {
	*error_code = MPIO_Err_create_code(MPI_SUCCESS,
					   MPIR_ERR_RECOVERABLE,
					   myname, __LINE__,
					   ADIOI_ZOIDFS_error_convert(ret),
					   "Error in zoidfs_finalize", 0);
	return;
    }
    /* --END ERROR HANDLING-- */

    *error_code = MPI_SUCCESS;
}

int ADIOI_ZOIDFS_End_call(MPI_Comm comm, int keyval, 
			 void *attribute_val, void *extra_state)
{
    int error_code;
    ADIOI_ZOIDFS_End(&error_code);
    MPI_Keyval_free(&keyval);
    return error_code;
}

void ADIOI_ZOIDFS_Init(int rank, int *error_code )
{
    int ret;
    static char myname[] = "ADIOI_ZOIDFS_INIT";

    /* do nothing if we've already fired up the zoidfs interface */
    if (ADIOI_ZOIDFS_Initialized != MPI_KEYVAL_INVALID) {
	*error_code = MPI_SUCCESS;
	return;
    }

    ret = zoidfs_init();
    if (ret < 0 ) {
	*error_code = MPIO_Err_create_code(MPI_SUCCESS,
					   MPIR_ERR_RECOVERABLE,
					   myname, __LINE__,
					   ADIOI_ZOIDFS_error_convert(ret),
					   "Error in zoidfs_init",
					   0);
	return;
    }
    
    MPI_Keyval_create(MPI_NULL_COPY_FN, ADIOI_ZOIDFS_End_call,
		      &ADIOI_ZOIDFS_Initialized, (void *)0); 
    /* just like romio does, we make a dummy attribute so we 
     * get cleaned up */
    MPI_Attr_put(MPI_COMM_SELF, ADIOI_ZOIDFS_Initialized, (void *)0);
}

void ADIOI_ZOIDFS_makeattribs(zoidfs_sattr_t * attribs)
{
    memset(attribs, 0, sizeof(zoidfs_sattr_t));

    attribs->mask = ZOIDFS_ATTR_MODE;
    attribs->mode = 0644;
}

int ADIOI_ZOIDFS_error_convert(int error)
{
    switch (error)
    {
	case ZFSERR_PERM: /* ??? */
	case ZFSERR_ACCES:
	    return MPI_ERR_ACCESS;
	case ZFSERR_NOENT:
	case ZFSERR_NXIO: /* ??? */
	case ZFSERR_NODEV: /* ??? */
	    return MPI_ERR_NO_SUCH_FILE;
	case ZFSERR_IO:
	    return MPI_ERR_IO;
	case ZFSERR_EXIST:
	    return MPI_ERR_FILE_EXISTS;
	case ZFSERR_NOTDIR: /* ??? */
	case ZFSERR_ISDIR: /* ??? */
	case ZFSERR_NAMETOOLONG:
	    return MPI_ERR_BAD_FILE;
	case ZFSERR_INVAL:
	case ZFSERR_STALE:
	    return MPI_ERR_FILE;
	case ZFSERR_FBIG: /* ??? */
	case ZFSERR_NOSPC:
	    return MPI_ERR_NO_SPACE;
	case ZFSERR_ROFS:
	    return MPI_ERR_READ_ONLY;
	case ZFSERR_NOTIMPL:
	    return MPI_ERR_UNSUPPORTED_OPERATION;
	case ZFSERR_DQUOT:
	    return MPI_ERR_QUOTA;
	/* case ZFSERR_NOTEMPTY: */
	/* case ZFSERR_WFLUSH: */
	/* case ZFSERR_OTHER: */
	case ZFSERR_NOMEM:
	    return MPI_ERR_INTERN;
	default:
	    return MPI_UNDEFINED;
    }
}

/* 
 * vim: ts=8 sts=4 sw=4 noexpandtab 
 */
