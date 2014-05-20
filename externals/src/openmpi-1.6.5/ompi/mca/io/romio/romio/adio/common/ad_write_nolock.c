/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include <unistd.h>

#include "adio.h"
#include "adio_extern.h"


/* #define IO_DEBUG 1 */
void ADIOI_NOLOCK_WriteStrided(ADIO_File fd, void *buf, int count,
			     MPI_Datatype datatype, int file_ptr_type,
			     ADIO_Offset offset, ADIO_Status *status, int
			     *error_code)
{
/* borrowed from old-school PVFS (v1) code. A driver for file systems that
 * cannot or do not support client-side buffering
 * Does not do data sieving optimization
 * Does contain write-combining optimization for noncontig in memory, contig in
 * file 
 */

/* offset is in units of etype relative to the filetype. */

    ADIOI_Flatlist_node *flat_buf, *flat_file;
    int j, k, err=-1, st_index=0;
    ADIO_Offset fwr_size=0, bwr_size, new_bwr_size, new_fwr_size, i_offset, num;
    unsigned bufsize; 
    int n_etypes_in_filetype;
    ADIO_Offset n_filetypes, etype_in_filetype, size, sum;
    ADIO_Offset abs_off_in_filetype=0, size_in_filetype;
    int filetype_size, etype_size, buftype_size;
    MPI_Aint filetype_extent, buftype_extent, indx;
    int buf_count, buftype_is_contig, filetype_is_contig;
    ADIO_Offset off, disp;
    int flag, err_flag=0;
    static char myname[] = "ADIOI_NOLOCK_WRITESTRIDED";
#ifdef IO_DEBUG
    int rank,nprocs;
#endif

    /* --BEGIN ERROR HANDLING-- */
    if (fd->atomicity) {
	*error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					   myname, __LINE__,
					   MPI_ERR_INTERN,
					   "Atomic mode set in I/O function", 0);
	return;
    }
    /* --END ERROR HANDLING-- */

    ADIOI_Datatype_iscontig(datatype, &buftype_is_contig);
    ADIOI_Datatype_iscontig(fd->filetype, &filetype_is_contig);

    MPI_Type_size(fd->filetype, &filetype_size);
    if ( ! filetype_size ) {
	*error_code = MPI_SUCCESS; 
	return;
    }

#ifdef IO_DEBUG
    MPI_Comm_rank(fd->comm, &rank);
    MPI_Comm_size(fd->comm, &nprocs);
#endif

    MPI_Type_extent(fd->filetype, &filetype_extent);
    MPI_Type_size(datatype, &buftype_size);
    MPI_Type_extent(datatype, &buftype_extent);
    etype_size = fd->etype_size;
    
    ADIOI_Assert((buftype_size * count) == ((ADIO_Offset)(unsigned)buftype_size * (ADIO_Offset)count));
    bufsize = buftype_size * count;

    if (!buftype_is_contig && filetype_is_contig) {
	char *combine_buf, *combine_buf_ptr;
	ADIO_Offset combine_buf_remain;
/* noncontiguous in memory, contiguous in file. use writev */

	ADIOI_Flatten_datatype(datatype);
	flat_buf = ADIOI_Flatlist;
	while (flat_buf->type != datatype) flat_buf = flat_buf->next;

	/* allocate our "combine buffer" to pack data into before writing */
	combine_buf = (char *) ADIOI_Malloc(fd->hints->ind_wr_buffer_size);
	combine_buf_ptr = combine_buf;
	combine_buf_remain = fd->hints->ind_wr_buffer_size;

	/* seek to the right spot in the file */
	if (file_ptr_type == ADIO_EXPLICIT_OFFSET) {
	    off = fd->disp + etype_size * offset;
	    lseek(fd->fd_sys, off, SEEK_SET);
	}
	else off = lseek(fd->fd_sys, fd->fp_ind, SEEK_SET);

	/* loop through all the flattened pieces.  combine into buffer until
	 * no more will fit, then write.
	 *
	 * special case of a given piece being bigger than the combine buffer
	 * is also handled.
	 */
	for (j=0; j<count; j++) {
    int i;
	    for (i=0; i<flat_buf->count; i++) {
		if (flat_buf->blocklens[i] > combine_buf_remain && combine_buf != combine_buf_ptr) {
		    /* there is data in the buffer; write out the buffer so far */
#ifdef IO_DEBUG
		    printf("[%d/%d] nc mem c file (0) writing loc = %Ld sz = %Ld\n", 
				    rank, nprocs, off, 
				    fd->hints->ind_wr_buffer_size-combine_buf_remain);
#endif
#ifdef ADIOI_MPE_LOGGING
		    MPE_Log_event( ADIOI_MPE_write_a, 0, NULL );
#endif
		    err = write(fd->fd_sys,
				     combine_buf,
				     fd->hints->ind_wr_buffer_size - combine_buf_remain);
#ifdef ADIOI_MPE_LOGGING
		    MPE_Log_event( ADIOI_MPE_write_b, 0, NULL );
#endif
		    if (err == -1) err_flag = 1;

		    /* reset our buffer info */
		    combine_buf_ptr = combine_buf;
		    combine_buf_remain = fd->hints->ind_wr_buffer_size;
		}

		/* TODO: heuristic for when to not bother to use combine buffer? */
		if (flat_buf->blocklens[i] >= combine_buf_remain) {
		    /* special case: blocklen is as big as or bigger than the combine buf;
		     * write directly
		     */
#ifdef IO_DEBUG
		    printf("[%d/%d] nc mem c file (1) writing loc = %Ld sz = %d\n", 
				    rank, nprocs, off, 
				    flat_buf->blocklens[i]);
#endif
        ADIOI_Assert(flat_buf->blocklens[i] == (unsigned)flat_buf->blocklens[i]);
        ADIOI_Assert((((ADIO_Offset)(MPIR_Upint)buf) + (ADIO_Offset)j*(ADIO_Offset)buftype_extent + flat_buf->indices[i]) == (ADIO_Offset)((MPIR_Upint)buf + (ADIO_Offset)j*(ADIO_Offset)buftype_extent + flat_buf->indices[i]));
#ifdef ADIOI_MPE_LOGGING
		    MPE_Log_event( ADIOI_MPE_write_a, 0, NULL );
#endif
		    err = write(fd->fd_sys,
				     ((char *) buf) + (ADIO_Offset)j*(ADIO_Offset)buftype_extent + flat_buf->indices[i],
				     (unsigned)flat_buf->blocklens[i]);
#ifdef ADIOI_MPE_LOGGING
		    MPE_Log_event( ADIOI_MPE_write_b, 0, NULL );
#endif
		    if (err == -1) err_flag = 1;
		    off += flat_buf->blocklens[i]; /* keep up with the final file offset too */
		}
		else {
		    /* copy more data into combine buffer */
		    memcpy(combine_buf_ptr,
			   ((char *) buf) + j*buftype_extent + flat_buf->indices[i],
			   flat_buf->blocklens[i]);
		    combine_buf_ptr += flat_buf->blocklens[i];
		    combine_buf_remain -= flat_buf->blocklens[i];
		    off += flat_buf->blocklens[i]; /* keep up with the final file offset too */
		}
	    }
	}

	if (combine_buf_ptr != combine_buf) {
	    /* data left in buffer to write */
#ifdef IO_DEBUG
	    printf("[%d/%d] nc mem c file (2) writing loc = %Ld sz = %Ld\n", 
			    rank, nprocs, off, 
			     fd->hints->ind_wr_buffer_size-combine_buf_remain);
#endif
#ifdef ADIOI_MPE_LOGGING
	    MPE_Log_event( ADIOI_MPE_write_a, 0, NULL );
#endif
	    err = write(fd->fd_sys,
			     combine_buf,
			     fd->hints->ind_wr_buffer_size - combine_buf_remain);
#ifdef ADIOI_MPE_LOGGING
	    MPE_Log_event( ADIOI_MPE_write_b, 0, NULL );
#endif
	    if (err == -1) err_flag = 1;
	}

	if (file_ptr_type == ADIO_INDIVIDUAL) fd->fp_ind = off;

	ADIOI_Free(combine_buf);

	if (err_flag) {
	    *error_code = MPIO_Err_create_code(MPI_SUCCESS,
					       MPIR_ERR_RECOVERABLE, myname,
					       __LINE__, MPI_ERR_IO, "**io",
					       "**io %s", strerror(errno));
	}
	else *error_code = MPI_SUCCESS;
    } /* if (!buftype_is_contig && filetype_is_contig)  ... */

    else {  /* noncontiguous in file */

/* split up into several contiguous writes */

/* find starting location in the file */

/* filetype already flattened in ADIO_Open */
	flat_file = ADIOI_Flatlist;
	while (flat_file->type != fd->filetype) flat_file = flat_file->next;
        disp = fd->disp;

	if (file_ptr_type == ADIO_INDIVIDUAL) {
	    offset = fd->fp_ind; /* in bytes */
            n_filetypes = -1;
            flag = 0;
            while (!flag) {
                int i;
                n_filetypes++;
                for (i=0; i<flat_file->count; i++) {
                    if (disp + flat_file->indices[i] + 
                        n_filetypes*(ADIO_Offset)filetype_extent + flat_file->blocklens[i] 
                            >= offset) {
                        st_index = i;
                        fwr_size = disp + flat_file->indices[i] + 
                                n_filetypes*(ADIO_Offset)filetype_extent
                                 + flat_file->blocklens[i] - offset;
                        flag = 1;
                        break;
                    }
                }
            }
	}
	else {
            int i;
	    n_etypes_in_filetype = filetype_size/etype_size;
	    n_filetypes = offset / n_etypes_in_filetype;
	    etype_in_filetype = offset % n_etypes_in_filetype;
	    size_in_filetype = etype_in_filetype * etype_size;
 
	    sum = 0;
	    for (i=0; i<flat_file->count; i++) {
		sum += flat_file->blocklens[i];
		if (sum > size_in_filetype) {
		    st_index = i;
		    fwr_size = sum - size_in_filetype;
		    abs_off_in_filetype = flat_file->indices[i] +
			size_in_filetype - (sum - flat_file->blocklens[i]);
		    break;
		}
	    }

	    /* abs. offset in bytes in the file */
            offset = disp + n_filetypes*(ADIO_Offset)filetype_extent + abs_off_in_filetype;
	}

	if (buftype_is_contig && !filetype_is_contig) {

/* contiguous in memory, noncontiguous in file. should be the most
   common case. */

	    i_offset = 0;
	    j = st_index;
	    off = offset;
	    fwr_size = ADIOI_MIN(fwr_size, bufsize);
	    while (i_offset < bufsize) {
                if (fwr_size) { 
                    /* TYPE_UB and TYPE_LB can result in 
                       fwr_size = 0. save system call in such cases */ 
#ifdef ADIOI_MPE_LOGGING
		    MPE_Log_event(ADIOI_MPE_lseek_a, 0, NULL);
#endif
#ifdef IO_DEBUG
		    printf("[%d/%d] c mem nc file writing loc = %Ld sz = %d\n", 
			    rank, nprocs, off, fwr_size);
#endif
		    err = lseek(fd->fd_sys, off, SEEK_SET);
#ifdef ADIOI_MPE_LOGGING
		    MPE_Log_event(ADIOI_MPE_lseek_b, 0, NULL);
#endif
		    if (err == -1) err_flag = 1;
#ifdef ADIOI_MPE_LOGGING
		    MPE_Log_event(ADIOI_MPE_write_a, 0, NULL);
#endif
		    err = write(fd->fd_sys, ((char *) buf) + i_offset, fwr_size);
#ifdef ADIOI_MPE_LOGGING
		    MPE_Log_event(ADIOI_MPE_write_b, 0, NULL);
#endif
		    if (err == -1) err_flag = 1;
		}
		i_offset += fwr_size;

                if (off + fwr_size < disp + flat_file->indices[j] +
                   flat_file->blocklens[j] + n_filetypes*(ADIO_Offset)filetype_extent)
                       off += fwr_size;
                /* did not reach end of contiguous block in filetype.
                   no more I/O needed. off is incremented by fwr_size. */
                else {
		    if (j < (flat_file->count - 1)) j++;
		    else {
			j = 0;
			n_filetypes++;
		    }
		    off = disp + flat_file->indices[j] + 
                                        n_filetypes*(ADIO_Offset)filetype_extent;
		    fwr_size = ADIOI_MIN(flat_file->blocklens[j], bufsize-i_offset);
		}
	    }
	}
	else {
/* noncontiguous in memory as well as in file */

	    ADIOI_Flatten_datatype(datatype);
	    flat_buf = ADIOI_Flatlist;
	    while (flat_buf->type != datatype) flat_buf = flat_buf->next;

	    k = num = buf_count = 0;
	    indx = flat_buf->indices[0];
	    j = st_index;
	    off = offset;
	    bwr_size = flat_buf->blocklens[0];

	    while (num < bufsize) {
		size = ADIOI_MIN(fwr_size, bwr_size);
		if (size) {
#ifdef IO_DEBUG
		    printf("[%d/%d] nc mem nc file writing loc = %Ld sz = %d\n", 
				    rank, nprocs, off, size);
#endif
#ifdef ADIOI_MPE_LOGGING
		    MPE_Log_event( ADIOI_MPE_lseek_a, 0, NULL );
#endif
		    lseek(fd->fd_sys, off, SEEK_SET);
#ifdef ADIOI_MPE_LOGGING 
		    MPE_Log_event( ADIOI_MPE_lseek_b, 0, NULL );
#endif
		    if (err == -1) err_flag = 1;
#ifdef ADIOI_MPE_LOGGING 
		    MPE_Log_event( ADIOI_MPE_write_a, 0, NULL );
#endif
                    ADIOI_Assert(size == (size_t) size);
                    ADIOI_Assert(off == (off_t) off);
		    err = write(fd->fd_sys, ((char *) buf) + indx, size);
#ifdef ADIOI_MPE_LOGGING
		    MPE_Log_event( ADIOI_MPE_write_b, 0, NULL );
#endif
		    if (err == -1) err_flag = 1;
		}

		new_fwr_size = fwr_size;
		new_bwr_size = bwr_size;

		if (size == fwr_size) {
/* reached end of contiguous block in file */
                    if (j < (flat_file->count - 1)) j++;
                    else {
                        j = 0;
                        n_filetypes++;
                    }

                    off = disp + flat_file->indices[j] + 
                                   n_filetypes*(ADIO_Offset)filetype_extent;

		    new_fwr_size = flat_file->blocklens[j];
		    if (size != bwr_size) {
			indx += size;
			new_bwr_size -= size;
		    }
		}

		if (size == bwr_size) {
/* reached end of contiguous block in memory */

		    k = (k + 1)%flat_buf->count;
		    buf_count++;
		    indx = buftype_extent*(buf_count/flat_buf->count) +
			flat_buf->indices[k]; 
		    new_bwr_size = flat_buf->blocklens[k];
		    if (size != fwr_size) {
			off += size;
			new_fwr_size -= size;
		    }
		}
		num += size;
		fwr_size = new_fwr_size;
                bwr_size = new_bwr_size;
	    }
	}

        if (file_ptr_type == ADIO_INDIVIDUAL) fd->fp_ind = off;
	if (err_flag) {
	    *error_code = MPIO_Err_create_code(MPI_SUCCESS,
					       MPIR_ERR_RECOVERABLE, myname,
					       __LINE__, MPI_ERR_IO, "**io",
					       "**io %s", strerror(errno));
	}
	else *error_code = MPI_SUCCESS;
    }

    fd->fp_sys_posn = -1;   /* set it to null. */

#ifdef HAVE_STATUS_SET_BYTES
    MPIR_Status_set_bytes(status, datatype, bufsize);
/* This is a temporary way of filling in status. The right way is to 
   keep track of how much data was actually written by ADIOI_BUFFERED_WRITE. */
#endif

    if (!buftype_is_contig) ADIOI_Delete_flattened(datatype);
}
