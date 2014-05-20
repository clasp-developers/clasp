/* -*- Mode: C; c-basic-offset:4 ; -*- 
 *     vim: ts=8 sts=4 sw=4 noexpandtab 
 * 
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"
#include "adio_extern.h"
#include "ad_pvfs2.h"
#include "ad_pvfs2_io.h"
#include "ad_pvfs2_common.h"

void ADIOI_PVFS2_ReadContig(ADIO_File fd, void *buf, int count, 
			    MPI_Datatype datatype, int file_ptr_type,
			    ADIO_Offset offset, ADIO_Status *status,
			    int *error_code)
{
    int ret, datatype_size, len;
    PVFS_Request file_req, mem_req;
    PVFS_sysresp_io resp_io;
    ADIOI_PVFS2_fs *pvfs_fs;
    static char myname[] = "ADIOI_PVFS2_READCONTIG";

    pvfs_fs = (ADIOI_PVFS2_fs*)fd->fs_ptr;

    MPI_Type_size(datatype, &datatype_size);
    len = datatype_size * count;

    ret = PVFS_Request_contiguous(len, PVFS_BYTE, &mem_req);
    /* --BEGIN ERROR HANDLING-- */
    if (ret != 0) {
	*error_code = MPIO_Err_create_code(MPI_SUCCESS,
					   MPIR_ERR_RECOVERABLE,
					   myname, __LINE__,
					   ADIOI_PVFS2_error_convert(ret),
					   "Error in pvfs_request_contig (memory)", 0);
	return;
    }
    /* --END ERROR HANDLING-- */

    ret = PVFS_Request_contiguous(len, PVFS_BYTE, &file_req);
    /* --BEGIN ERROR HANDLING-- */
    if (ret != 0) {
	*error_code = MPIO_Err_create_code(MPI_SUCCESS,
					   MPIR_ERR_RECOVERABLE,
					   myname, __LINE__,
					   ADIOI_PVFS2_error_convert(ret),
					   "Error in pvfs_request_contig (file)", 0);
	return;
    }
    /* --END ERROR HANDLING-- */

    if (file_ptr_type == ADIO_INDIVIDUAL) {
	/* copy individual file pointer into offset variable, continue */
	offset = fd->fp_ind;
    }

#ifdef ADIOI_MPE_LOGGING
    MPE_Log_event( ADIOI_MPE_read_a, 0, NULL );
#endif
    ret = PVFS_sys_read(pvfs_fs->object_ref, file_req, offset, buf, 
			mem_req, &(pvfs_fs->credentials), &resp_io);
#ifdef ADIOI_MPE_LOGGING
    MPE_Log_event( ADIOI_MPE_read_b, 0, NULL );
#endif
    /* --BEGIN ERROR HANDLING-- */
    if (ret != 0 ) {
	*error_code = MPIO_Err_create_code(MPI_SUCCESS,
					   MPIR_ERR_RECOVERABLE,
					   myname, __LINE__,
					   ADIOI_PVFS2_error_convert(ret),
					   "Error in PVFS_sys_read", 0);
	goto fn_exit;
    }
    /* --END ERROR HANDLING-- */

    if (file_ptr_type == ADIO_INDIVIDUAL) {
	fd->fp_ind += (int) resp_io.total_completed;
	/* TODO: WHY THE INT CAST? */
    }
    fd->fp_sys_posn = offset + (int)resp_io.total_completed;

#ifdef HAVE_STATUS_SET_BYTES
    MPIR_Status_set_bytes(status, datatype, (int)resp_io.total_completed);
#endif

    *error_code = MPI_SUCCESS;
fn_exit:
    PVFS_Request_free(&mem_req);
    PVFS_Request_free(&file_req);
    return;
}

static int ADIOI_PVFS2_ReadStridedListIO(ADIO_File fd, void *buf, int count,
				  MPI_Datatype datatype, int file_ptr_type,
				  ADIO_Offset offset, ADIO_Status *status,
				  int *error_code)
{
    return ADIOI_PVFS2_StridedListIO(fd, buf, count,
				     datatype, file_ptr_type,
				     offset, status,
				     error_code, READ);
}

static int ADIOI_PVFS2_ReadStridedDtypeIO(ADIO_File fd, void *buf, int count,
				   MPI_Datatype datatype, int file_ptr_type,
				   ADIO_Offset offset, ADIO_Status *status, 
				   int *error_code)
{
    return ADIOI_PVFS2_StridedDtypeIO(fd, buf, count,
				      datatype, file_ptr_type,
				      offset, status, error_code,
				      READ);
}

void ADIOI_PVFS2_ReadStrided(ADIO_File fd, void *buf, int count,
			     MPI_Datatype datatype, int file_ptr_type,
			     ADIO_Offset offset, ADIO_Status *status, int
			     *error_code)
{
    /* four ways (to date) that we can carry out strided i/o accesses:
     * - naive posix
     * - 'true' Datatype (from avery)
     * - new List I/O (from avery)
     * - classic List I/O  (the one that's always been in ROMIO)
     * I imagine we'll keep Datatype as an optional optimization, and afer a
     * release or two promote it to the default 
     */
    int ret = -1;

    if (fd->hints->fs_hints.pvfs2.posix_read == ADIOI_HINT_ENABLE) {
	ADIOI_GEN_ReadStrided(fd, buf, count, datatype, 
		file_ptr_type, offset, status, error_code);
	return;
    }
    if (fd->hints->fs_hints.pvfs2.dtype_read == ADIOI_HINT_ENABLE) {
        ret = ADIOI_PVFS2_ReadStridedDtypeIO(fd, buf, count,
                                             datatype, file_ptr_type,
                                             offset, status, error_code);

        /* Fall back to list I/O if datatype I/O didn't work */
        if (ret != 0)
        {
            fprintf(stderr,
                    "Falling back to list I/O since datatype I/O failed\n");
            ret = ADIOI_PVFS2_ReadStridedListIO(fd, buf, count,
                                                datatype, file_ptr_type,
                                                offset, status, error_code);
        }
        return;
    }
    if (fd->hints->fs_hints.pvfs2.listio_read == ADIOI_HINT_ENABLE) {
	ret = ADIOI_PVFS2_ReadStridedListIO(fd, buf, count, datatype,
		file_ptr_type, offset, status, error_code);
	return;
    }
    /* Use classic list I/O if no hints given base case */

    ADIOI_PVFS2_OldReadStrided(fd, buf, count, datatype, 
	    file_ptr_type, offset, status, error_code);
    return;
}


/*
 * vim: ts=8 sts=4 sw=4 noexpandtab 
 */
