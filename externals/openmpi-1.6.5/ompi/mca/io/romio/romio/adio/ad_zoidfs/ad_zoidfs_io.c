/* -*- Mode: C; c-basic-offset:4 ; -*- 
 * 
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"
#include "adio_extern.h"
#include "ad_zoidfs.h"

#include "ad_zoidfs_common.h"

#define ZOIDFS_READ 0
#define ZOIDFS_WRITE 1

static void ZOIDFS_IOContig(ADIO_File fd, void * buf, int count,
	    MPI_Datatype datatype, int file_ptr_type,
	    ADIO_Offset offset, ADIO_Status *status,
	    int flag, int *error_code)
{
    int ret, datatype_size;
    uint64_t file_len;
    size_t mem_len;
    ADIOI_ZOIDFS_object *zoidfs_obj_ptr;
    uint64_t file_offset = offset;
    static char myname[] = "ADIOI_ZOIDFS_IOCONTIG";

    zoidfs_obj_ptr = (ADIOI_ZOIDFS_object*)fd->fs_ptr;

    MPI_Type_size(datatype, &datatype_size);
    file_len = mem_len = datatype_size * count;

    if (file_ptr_type == ADIO_INDIVIDUAL) {
	file_offset = fd->fp_ind;
    }

    if (flag == ZOIDFS_READ) {
	NO_STALE(ret, fd, zoidfs_obj_ptr,
                 zoidfs_read(zoidfs_obj_ptr, 
                             1, &buf, &mem_len,
                             1, &file_offset, &file_len, ZOIDFS_NO_OP_HINT));
    } else {
        NO_STALE(ret, fd, zoidfs_obj_ptr,
                 zoidfs_write(zoidfs_obj_ptr, 
                              1, (const void **)&buf, &mem_len,
                              1, &file_offset, &file_len, ZOIDFS_NO_OP_HINT));
    }
    /* --BEGIN ERROR HANDLING-- */
    if (ret != ZFS_OK ) {
	*error_code = MPIO_Err_create_code(MPI_SUCCESS,
					   MPIR_ERR_RECOVERABLE,
					   myname, __LINE__,
					   ADIOI_ZOIDFS_error_convert(ret),
					   "Error in ZOIDFS I/O", 0);
	goto fn_exit;
    }
    /* --END ERROR HANDLING-- */

    if (file_ptr_type == ADIO_INDIVIDUAL) {
	fd->fp_ind += file_len;
    }
    fd->fp_sys_posn = file_offset + file_len;

#ifdef HAVE_STATUS_SET_BYTES
    MPIR_Status_set_bytes(status, datatype, file_len);
#endif

    *error_code = MPI_SUCCESS;

fn_exit:
    return;
}

void ADIOI_ZOIDFS_ReadContig(ADIO_File fd, void *buf, int count, 
			    MPI_Datatype datatype, int file_ptr_type,
			    ADIO_Offset offset, ADIO_Status *status,
			    int *error_code)
{
    ZOIDFS_IOContig(fd, buf, count, datatype, file_ptr_type, 
	    offset, status, ZOIDFS_READ, error_code);
}

void ADIOI_ZOIDFS_WriteContig(ADIO_File fd, void *buf, int count,
			   MPI_Datatype datatype, int file_ptr_type,
			   ADIO_Offset offset, ADIO_Status *status,
			   int *error_code)
{
    ZOIDFS_IOContig(fd, buf, count, datatype, file_ptr_type,
	    offset, status, ZOIDFS_WRITE, error_code);
}

    
/*
 * vim: ts=8 sts=4 sw=4 noexpandtab 
 */
