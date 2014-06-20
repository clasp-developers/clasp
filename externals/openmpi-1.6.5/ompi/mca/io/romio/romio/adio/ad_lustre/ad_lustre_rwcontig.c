/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *   Copyright (C) 1997 University of Chicago.
 *   See COPYRIGHT notice in top-level directory.
 *
 *   Copyright (C) 2007 Oak Ridge National Laboratory
 *
 *   Copyright (C) 2008 Sun Microsystems, Lustre group
 */

#define _XOPEN_SOURCE 600
#include <stdlib.h>
#include <malloc.h>
#include "ad_lustre.h"

#define LUSTRE_MEMALIGN (1<<12)  /* to use page_shift */

static void ADIOI_LUSTRE_Aligned_Mem_File_Write(ADIO_File fd, void *buf, int len, 
              ADIO_Offset offset, int *err);
static void ADIOI_LUSTRE_Aligned_Mem_File_Write(ADIO_File fd, void *buf, int len, 
              ADIO_Offset offset, int *err)
{
    int rem, size, nbytes;
    if (!(len % fd->d_miniosz) && (len >= fd->d_miniosz)) {
	*err = pwrite(fd->fd_direct, buf, len, offset);
    } else if (len < fd->d_miniosz) {
	*err = pwrite(fd->fd_sys, buf, len, offset);
    } else {
	rem = len % fd->d_miniosz;
	size = len - rem;
	nbytes = pwrite(fd->fd_direct, buf, size, offset);
	nbytes += pwrite(fd->fd_sys, ((char *)buf) + size, rem, offset+size);
	*err = nbytes;
    }
}

static void ADIOI_LUSTRE_Aligned_Mem_File_Read(ADIO_File fd, void *buf, int len, 
              ADIO_Offset offset, int *err);
static void ADIOI_LUSTRE_Aligned_Mem_File_Read(ADIO_File fd, void *buf, int len, 
              ADIO_Offset offset, int *err)
{
    int rem, size, nbytes;
    if (!(len % fd->d_miniosz) && (len >= fd->d_miniosz))
	*err = pread(fd->fd_direct, buf, len, offset);
    else if (len < fd->d_miniosz)
	*err = pread(fd->fd_sys, buf, len, offset);
    else {
	rem = len % fd->d_miniosz;
	size = len - rem;
	nbytes = pread(fd->fd_direct, buf, size, offset);
	nbytes += pread(fd->fd_sys, ((char *)buf) + size, rem, offset+size);
	*err = nbytes;
    }
}


static int ADIOI_LUSTRE_Directio(ADIO_File fd, void *buf, int len, 
			   off_t offset, int rw);
static int ADIOI_LUSTRE_Directio(ADIO_File fd, void *buf, int len, 
			   off_t offset, int rw)
{
    int err=-1, diff, size=len, nbytes = 0;
    void *newbuf;

    if (offset % fd->d_miniosz) {
	diff = fd->d_miniosz - (offset % fd->d_miniosz);
	diff = ADIOI_MIN(diff, len);
	if (rw)
	    nbytes = pwrite(fd->fd_sys, buf, diff, offset);
	else
	    nbytes = pread(fd->fd_sys, buf, diff, offset);
	buf = ((char *) buf) + diff;
	offset += diff;
	size = len - diff;
    }

    if (!size) {
	return diff;
    }

    if (rw) { /* direct I/O enabled */
	if (!(((long) buf) % fd->d_mem)) {
	    ADIOI_LUSTRE_Aligned_Mem_File_Write(fd, buf, size, offset, &err);
	    nbytes += err;
	} else {
	    newbuf = (void *) memalign(LUSTRE_MEMALIGN, size);
	    if (newbuf) {
		memcpy(newbuf, buf, size);
		ADIOI_LUSTRE_Aligned_Mem_File_Write(fd, newbuf, size, offset, &err);
		nbytes += err;
		ADIOI_Free(newbuf);
	    }
	    else nbytes += pwrite(fd->fd_sys, buf, size, offset);
	}
	err = nbytes;
    } else {       
	if (!(((long) buf) % fd->d_mem)) {
	    ADIOI_LUSTRE_Aligned_Mem_File_Read(fd, buf, size, offset, &err);
	    nbytes += err;
	} else {
	    newbuf = (void *) memalign(LUSTRE_MEMALIGN, size);
	    if (newbuf) {
		ADIOI_LUSTRE_Aligned_Mem_File_Read(fd, newbuf, size, offset, &err);
		if (err > 0) memcpy(buf, newbuf, err);
		nbytes += err;
		ADIOI_Free(newbuf);
	    }
	    else nbytes += pread(fd->fd_sys, buf, size, offset);
	}
	err = nbytes;
    }
    return err;
}

static void ADIOI_LUSTRE_IOContig(ADIO_File fd, void *buf, int count, 
                   MPI_Datatype datatype, int file_ptr_type,
	           ADIO_Offset offset, ADIO_Status *status, 
		   int io_mode, int *error_code);
static void ADIOI_LUSTRE_IOContig(ADIO_File fd, void *buf, int count, 
                   MPI_Datatype datatype, int file_ptr_type,
	           ADIO_Offset offset, ADIO_Status *status, 
		   int io_mode, int *error_code)
{
    int err=-1, datatype_size, len;
    static char myname[] = "ADIOI_LUSTRE_IOCONTIG";

    MPI_Type_size(datatype, &datatype_size);
    len = datatype_size * count;

    if (file_ptr_type == ADIO_INDIVIDUAL) {
	offset = fd->fp_ind;
    }

    if (!(fd->direct_read || fd->direct_write)) {
	if (fd->fp_sys_posn != offset) {
	    err = lseek(fd->fd_sys, offset, SEEK_SET);
	    if (err == -1) goto ioerr;
	}
	
	if (io_mode) {
#ifdef ADIOI_MPE_LOGGING
        MPE_Log_event(ADIOI_MPE_write_a, 0, NULL);
#endif
	    err = write(fd->fd_sys, buf, len);
#ifdef ADIOI_MPE_LOGGING
        MPE_Log_event(ADIOI_MPE_write_b, 0, NULL);
#endif
        } else {
#ifdef ADIOI_MPE_LOGGING
        MPE_Log_event(ADIOI_MPE_read_a, 0, NULL);
#endif
	    err = read(fd->fd_sys, buf, len);
#ifdef ADIOI_MPE_LOGGING
        MPE_Log_event(ADIOI_MPE_read_b, 0, NULL);
#endif
        }
    } else {
	err = ADIOI_LUSTRE_Directio(fd, buf, len, offset, io_mode);
    }

    if (err == -1) goto ioerr;
    fd->fp_sys_posn = offset + err;

    if (file_ptr_type == ADIO_INDIVIDUAL) {
	fd->fp_ind += err; 
    }

#ifdef HAVE_STATUS_SET_BYTES
    if (status) MPIR_Status_set_bytes(status, datatype, err);
#endif
    *error_code = MPI_SUCCESS;

ioerr:
    /* --BEGIN ERROR HANDLING-- */
    if (err == -1) {
	*error_code = MPIO_Err_create_code(MPI_SUCCESS,
					   MPIR_ERR_RECOVERABLE,
					   myname, __LINE__,
					   MPI_ERR_IO, "**io",
					   "**io %s", strerror(errno));
	fd->fp_sys_posn = -1;
	return;
    }
    /* --END ERROR HANDLING-- */
}

void ADIOI_LUSTRE_WriteContig(ADIO_File fd, void *buf, int count, 
                   MPI_Datatype datatype, int file_ptr_type,
	           ADIO_Offset offset, ADIO_Status *status, int *error_code)
{
    ADIOI_LUSTRE_IOContig(fd, buf, count, datatype, file_ptr_type,
	           offset, status, 1, error_code);
}

void ADIOI_LUSTRE_ReadContig(ADIO_File fd, void *buf, int count, 
                   MPI_Datatype datatype, int file_ptr_type,
	           ADIO_Offset offset, ADIO_Status *status, int *error_code)
{
    ADIOI_LUSTRE_IOContig(fd, buf, count, datatype, file_ptr_type,
	           offset, status, 0, error_code);
}
