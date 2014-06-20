/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_xfs.h"

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

/* style: allow:free:2 sig:0 */

static int ADIOI_XFS_Aligned_Mem_File_Write(ADIO_File fd, void *buf,
						  ADIO_Offset len, ADIO_Offset offset);

void ADIOI_XFS_WriteContig(ADIO_File fd, void *buf, int count, 
                     MPI_Datatype datatype, int file_ptr_type,
		     ADIO_Offset offset, ADIO_Status *status, int *error_code)
{
    int err=-1, datatype_size, diff, size;
    ssize_t len;
    void *newbuf;
    static char myname[] = "ADIOI_XFS_WRITECONTIG";

    MPI_Type_size(datatype, &datatype_size);
    len = datatype_size * count;

    fd->fp_sys_posn = -1; /* set it to null, since we are using pwrite */

    if (file_ptr_type == ADIO_INDIVIDUAL) offset = fd->fp_ind;

    if (!(fd->direct_write)) {    /* direct I/O not enabled */
	err = pwrite(fd->fd_sys, buf, len, offset);
	if (err < 0) {goto leaving;}
    } else {       /* direct I/O enabled */

	/* (1) if mem_aligned && file_aligned 
                    use direct I/O to write up to correct io_size
                    use buffered I/O for remaining  */

	if (!(((long) buf) % fd->d_mem) && !(offset % fd->d_miniosz)) {
	    err = ADIOI_XFS_Aligned_Mem_File_Write(fd, buf, len, offset);
	    if (err < 0) {goto leaving;}

        /* (2) if !file_aligned
                    use buffered I/O to write up to file_aligned
                    At that point, if still mem_aligned, use (1)
   		        else copy into aligned buf and then use (1) */
	} else if (offset % fd->d_miniosz) {
	    diff = fd->d_miniosz - (offset % fd->d_miniosz);
	    diff = ADIOI_MIN(diff, len);
	    err = pwrite(fd->fd_sys, buf, diff, offset);
	    if (err < 0) {goto leaving;}

	    buf = ((char *) buf) + diff;
	    offset += diff;
	    size = len - diff;
	    if (!(((long) buf) % fd->d_mem)) {
		err = ADIOI_XFS_Aligned_Mem_File_Write(fd, buf, size, offset);
		if (err < 0) {goto leaving;}
	    }
	    else {
		newbuf = (void *) memalign(XFS_MEMALIGN, size);
		if (newbuf) {
		    memcpy(newbuf, buf, size);
		    err = ADIOI_XFS_Aligned_Mem_File_Write(fd, newbuf, size, offset);
		    ADIOI_Free(newbuf);
		    if (err < 0) {goto leaving;}
		} else {
		    err = pwrite(fd->fd_sys, buf, size, offset);
		    if (err < 0) {goto leaving;}
		}
	    }
	}

        /* (3) if !mem_aligned && file_aligned
    	            copy into aligned buf, then use (1)  */
	else {
	    newbuf = (void *) memalign(XFS_MEMALIGN, len);
	    if (newbuf) {
		memcpy(newbuf, buf, len);
		err = ADIOI_XFS_Aligned_Mem_File_Write(fd, newbuf, len, offset);
		ADIOI_Free(newbuf);
	    } else {
		 err = pwrite(fd->fd_sys, buf, len, offset);
	    }

	    if (err < 0) {goto leaving;}
	}
    }

    if (file_ptr_type == ADIO_INDIVIDUAL) fd->fp_ind += len;

#ifdef HAVE_STATUS_SET_BYTES
    if (err != -1) MPIR_Status_set_bytes(status, datatype, len);
#endif
leaving:
    if (err == -1) {
	*error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					   myname, __LINE__, MPI_ERR_IO, "**io",
					   "**io %s", strerror(errno));
    }
    else *error_code = MPI_SUCCESS;
}


static int
ADIOI_XFS_Aligned_Mem_File_Write(ADIO_File fd, void *buf, ADIO_Offset len, 
              ADIO_Offset offset)
{
    unsigned write_chunk_sz = fd->hints->fs_hints.xfs.write_chunk_sz;
    ADIO_Offset nbytes, rem, newrem, size;
    int ntimes, i;

    /* memory buffer is aligned, offset in file is aligned,
       io_size may or may not be of the right size.
       use direct I/O to write up to correct io_size,
       use buffered I/O for remaining. */

    if (!(len % fd->d_miniosz) && 
	 (len >= fd->d_miniosz) && (len <= write_chunk_sz)) {
	nbytes = pwrite(fd->fd_direct, buf, len, offset);
	if (nbytes < 0) {return -1;}
    } else if (len < fd->d_miniosz) {
	nbytes = pwrite(fd->fd_sys, buf, len, offset);
	if (nbytes < 0) {return -1;}
    } else if (len > write_chunk_sz) {
	ntimes = len/(write_chunk_sz);
	rem = len - ntimes * write_chunk_sz;
	nbytes = 0;
	for (i=0; i<ntimes; i++) {
	    nbytes = pwrite(fd->fd_direct, ((char *)buf) + i * write_chunk_sz,
			 write_chunk_sz, offset);
	    offset += write_chunk_sz;
	    if (nbytes < 0) {return -1;}
	}
	if (rem) {
	    if (!(rem % fd->d_miniosz)) {
		nbytes = pwrite(fd->fd_direct, 
		             ((char *)buf) + ntimes * write_chunk_sz, rem, offset);
		if (nbytes < 0) {return -1;}
	    } else {
		newrem = rem % fd->d_miniosz;
		size = rem - newrem;
		if (size) {
		    nbytes = pwrite(fd->fd_direct, 
		            ((char *)buf) + ntimes * write_chunk_sz, size, offset);
		    offset += size;
		    if (nbytes < 0) {return -1;}
		}
		nbytes = pwrite(fd->fd_sys, 
	              ((char *)buf) + ntimes * write_chunk_sz + size, newrem, offset);
		if (nbytes < 0) {return -1;}
	    }
	}
    }
    else {
	rem = len % fd->d_miniosz;
	size = len - rem;
	nbytes = pwrite(fd->fd_direct, buf, size, offset);
	if (nbytes < 0) {return -1;}
	nbytes = pwrite(fd->fd_sys, (char *)buf + size, rem, offset+size);
	if (nbytes < 0) {return -1;}
    }

    return 0;
}
