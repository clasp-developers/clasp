/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"
#include "adio_extern.h"

#define ADIOI_BUFFERED_READ \
{ \
    if (req_off >= readbuf_off + readbuf_len) { \
	readbuf_off = req_off; \
	readbuf_len = (unsigned) (ADIOI_MIN(max_bufsize, end_offset-readbuf_off+1));\
	ADIO_ReadContig(fd, readbuf, readbuf_len, MPI_BYTE, \
              ADIO_EXPLICIT_OFFSET, readbuf_off, &status1, error_code); \
        if (*error_code != MPI_SUCCESS) return; \
    } \
    while (req_len > readbuf_off + readbuf_len - req_off) { \
  ADIOI_Assert((readbuf_off + readbuf_len - req_off) == (int) (readbuf_off + readbuf_len - req_off));\
	partial_read = (int) (readbuf_off + readbuf_len - req_off); \
	tmp_buf = (char *) ADIOI_Malloc(partial_read); \
	memcpy(tmp_buf, readbuf+readbuf_len-partial_read, partial_read); \
	ADIOI_Free(readbuf); \
	readbuf = (char *) ADIOI_Malloc(partial_read + max_bufsize); \
	memcpy(readbuf, tmp_buf, partial_read); \
	ADIOI_Free(tmp_buf); \
	readbuf_off += readbuf_len-partial_read; \
	readbuf_len = (unsigned) (partial_read + ADIOI_MIN(max_bufsize, \
				       end_offset-readbuf_off+1)); \
	ADIO_ReadContig(fd, readbuf+partial_read, readbuf_len-partial_read, \
             MPI_BYTE, ADIO_EXPLICIT_OFFSET, readbuf_off+partial_read, \
             &status1, error_code); \
        if (*error_code != MPI_SUCCESS) return; \
    } \
    ADIOI_Assert(req_len == (size_t)req_len); \
    memcpy((char *)buf + userbuf_off, readbuf+req_off-readbuf_off, req_len); \
}


void ADIOI_GEN_ReadStrided(ADIO_File fd, void *buf, int count,
                       MPI_Datatype datatype, int file_ptr_type,
                       ADIO_Offset offset, ADIO_Status *status, int
                       *error_code)
{


/* offset is in units of etype relative to the filetype. */

    ADIOI_Flatlist_node *flat_buf, *flat_file;
    ADIO_Offset i_offset, new_brd_size, brd_size, size;
    int i, j, k, st_index=0;
    unsigned num, bufsize; 
    int n_etypes_in_filetype;
    ADIO_Offset n_filetypes, etype_in_filetype, st_n_filetypes, size_in_filetype;
    ADIO_Offset abs_off_in_filetype=0, new_frd_size, frd_size=0, st_frd_size;
    int filetype_size, etype_size, buftype_size, partial_read;
    MPI_Aint filetype_extent, buftype_extent; 
    int buf_count, buftype_is_contig, filetype_is_contig;
    ADIO_Offset userbuf_off, req_len, sum;
    ADIO_Offset off, req_off, disp, end_offset=0, readbuf_off, start_off;
    char *readbuf, *tmp_buf, *value;
    int info_flag;
    unsigned max_bufsize, readbuf_len;
    ADIO_Status status1;

    if (fd->hints->ds_read == ADIOI_HINT_DISABLE) {
    	/* if user has disabled data sieving on reads, use naive
	 * approach instead.
	 */
	ADIOI_GEN_ReadStrided_naive(fd, 
				    buf,
				    count,
				    datatype,
				    file_ptr_type,
				    offset,
				    status,
				    error_code);
    	return;
    }

    *error_code = MPI_SUCCESS;  /* changed below if error */

    ADIOI_Datatype_iscontig(datatype, &buftype_is_contig);
    ADIOI_Datatype_iscontig(fd->filetype, &filetype_is_contig);

    MPI_Type_size(fd->filetype, &filetype_size);
    if ( ! filetype_size ) {
	*error_code = MPI_SUCCESS; 
	return;
    }

    MPI_Type_extent(fd->filetype, &filetype_extent);
    MPI_Type_size(datatype, &buftype_size);
    MPI_Type_extent(datatype, &buftype_extent);
    etype_size = fd->etype_size;

    ADIOI_Assert((buftype_size * count) == ((ADIO_Offset)(unsigned)buftype_size * (ADIO_Offset)count));
    bufsize = buftype_size * count;

/* get max_bufsize from the info object. */

    value = (char *) ADIOI_Malloc((MPI_MAX_INFO_VAL+1)*sizeof(char));
    ADIOI_Info_get(fd->info, "ind_rd_buffer_size", MPI_MAX_INFO_VAL, value, 
                 &info_flag);
    max_bufsize = atoi(value);
    ADIOI_Free(value);


    if (!buftype_is_contig && filetype_is_contig) {

/* noncontiguous in memory, contiguous in file. */

	ADIOI_Flatten_datatype(datatype);
	flat_buf = ADIOI_Flatlist;
	while (flat_buf->type != datatype) flat_buf = flat_buf->next;

        off = (file_ptr_type == ADIO_INDIVIDUAL) ? fd->fp_ind : 
                 fd->disp + (ADIO_Offset)etype_size * offset;

	start_off = off;
	end_offset = off + bufsize - 1;
        readbuf_off = off;
        readbuf = (char *) ADIOI_Malloc(max_bufsize);
        readbuf_len = (unsigned) (ADIOI_MIN(max_bufsize, end_offset-readbuf_off+1));

/* if atomicity is true, lock (exclusive) the region to be accessed */
        if ((fd->atomicity) && ADIO_Feature(fd, ADIO_LOCKS))
            ADIOI_WRITE_LOCK(fd, start_off, SEEK_SET, end_offset-start_off+1);

        ADIO_ReadContig(fd, readbuf, readbuf_len, MPI_BYTE, 
            ADIO_EXPLICIT_OFFSET, readbuf_off, &status1, error_code);
	if (*error_code != MPI_SUCCESS) return;

        for (j=0; j<count; j++) 
        {
              for (i=0; i<flat_buf->count; i++) {
                  userbuf_off = (ADIO_Offset)j*(ADIO_Offset)buftype_extent + flat_buf->indices[i];
      req_off = off;
      req_len = flat_buf->blocklens[i];
      ADIOI_BUFFERED_READ
                  off += flat_buf->blocklens[i];
              }
        }

        if ((fd->atomicity) && ADIO_Feature(fd, ADIO_LOCKS))
            ADIOI_UNLOCK(fd, start_off, SEEK_SET, end_offset-start_off+1);

        if (file_ptr_type == ADIO_INDIVIDUAL) fd->fp_ind = off;

	ADIOI_Free(readbuf); 
    }

    else {  /* noncontiguous in file */

/* filetype already flattened in ADIO_Open */
	flat_file = ADIOI_Flatlist;
	while (flat_file->type != fd->filetype) flat_file = flat_file->next;
	disp = fd->disp;

	if (file_ptr_type == ADIO_INDIVIDUAL) {
	    /* Wei-keng reworked type processing to be a bit more efficient */
            offset       = fd->fp_ind - disp;
            n_filetypes  = (offset - flat_file->indices[0]) / filetype_extent;
	    offset -= (ADIO_Offset)n_filetypes * filetype_extent;
	    /* now offset is local to this extent */

            /* find the block where offset is located, skip blocklens[i]==0 */
            for (i=0; i<flat_file->count; i++) {
                ADIO_Offset dist;
                if (flat_file->blocklens[i] == 0) continue;
                dist = flat_file->indices[i] + flat_file->blocklens[i] - offset;
                /* frd_size is from offset to the end of block i */
		if (dist == 0) {
		    i++;
		    offset   = flat_file->indices[i];
		    frd_size = flat_file->blocklens[i];
		    break;
		}
		if (dist > 0) {
                    frd_size = dist;
		    break;
		}
	    }
            st_index = i;  /* starting index in flat_file->indices[] */
            offset += disp + (ADIO_Offset)n_filetypes*filetype_extent;
        }
	else {
	    n_etypes_in_filetype = filetype_size/etype_size;
	    n_filetypes = offset / n_etypes_in_filetype;
	    etype_in_filetype = offset % n_etypes_in_filetype;
	    size_in_filetype = etype_in_filetype * etype_size;
 
	    sum = 0;
	    for (i=0; i<flat_file->count; i++) {
		sum += flat_file->blocklens[i];
		if (sum > size_in_filetype) {
		    st_index = i;
		    frd_size = sum - size_in_filetype;
		    abs_off_in_filetype = flat_file->indices[i] +
			size_in_filetype - (sum - flat_file->blocklens[i]);
		    break;
		}
	    }

	    /* abs. offset in bytes in the file */
	    offset = disp + (ADIO_Offset) n_filetypes*filetype_extent + 
		    abs_off_in_filetype;
	}

        start_off = offset;

	/* Wei-keng Liao: read request is within a single flat_file contig
	 * block e.g. with subarray types that actually describe the whole
	 * array */
	if (buftype_is_contig && bufsize <= frd_size) {
            ADIO_ReadContig(fd, buf, bufsize, MPI_BYTE, ADIO_EXPLICIT_OFFSET,
                             offset, status, error_code);

	    if (file_ptr_type == ADIO_INDIVIDUAL) {
                /* update MPI-IO file pointer to point to the first byte that 
		 * can be accessed in the fileview. */
		fd->fp_ind = offset + bufsize;
		if (bufsize == frd_size) {
		    do {
			st_index++;
			if (st_index == flat_file->count) {
			    st_index = 0;
			    n_filetypes++;
			}
                    } while (flat_file->blocklens[st_index] == 0);
		    fd->fp_ind = disp + flat_file->indices[st_index]
                               + n_filetypes*filetype_extent;
		}
	    }
	    fd->fp_sys_posn = -1;   /* set it to null. */ 
#ifdef HAVE_STATUS_SET_BYTES
	    MPIR_Status_set_bytes(status, datatype, bufsize);
#endif 
            return;
	}

       /* Calculate end_offset, the last byte-offset that will be accessed.
         e.g., if start_offset=0 and 100 bytes to be read, end_offset=99*/

	st_frd_size = frd_size;
	st_n_filetypes = n_filetypes;
	i_offset = 0;
	j = st_index;
	off = offset;
	frd_size = ADIOI_MIN(st_frd_size, bufsize);
	while (i_offset < bufsize) {
	    i_offset += frd_size;
	    end_offset = off + frd_size - 1;

	    j = (j+1) % flat_file->count;
            n_filetypes += (j == 0) ? 1 : 0;
            while (flat_file->blocklens[j]==0) {
		j = (j+1) % flat_file->count;
		n_filetypes += (j == 0) ? 1 : 0;
	    }
	    off = disp + flat_file->indices[j] + n_filetypes*(ADIO_Offset)filetype_extent;
	    frd_size = ADIOI_MIN(flat_file->blocklens[j], bufsize-i_offset);
	}

/* if atomicity is true, lock (exclusive) the region to be accessed */
        if ((fd->atomicity) && ADIO_Feature(fd, ADIO_LOCKS))
            ADIOI_WRITE_LOCK(fd, start_off, SEEK_SET, end_offset-start_off+1);

	readbuf_off = 0;
	readbuf_len = 0;
	readbuf = (char *) ADIOI_Malloc(max_bufsize);

	if (buftype_is_contig && !filetype_is_contig) {

/* contiguous in memory, noncontiguous in file. should be the most
   common case. */

	    i_offset = 0;
	    j = st_index;
	    off = offset;
	    n_filetypes = st_n_filetypes;
	    frd_size = ADIOI_MIN(st_frd_size, bufsize);
	    while (i_offset < bufsize) {
                if (frd_size) { 
                    /* TYPE_UB and TYPE_LB can result in 
                       frd_size = 0. save system call in such cases */ 
		    /* lseek(fd->fd_sys, off, SEEK_SET);
		    err = read(fd->fd_sys, ((char *) buf) + i, frd_size);*/

		    req_off = off;
		    req_len = frd_size;
		    userbuf_off = i_offset;
		    ADIOI_BUFFERED_READ
		}
		i_offset += frd_size;

                if (off + frd_size < disp + flat_file->indices[j] +
                   flat_file->blocklens[j] + n_filetypes*(ADIO_Offset)filetype_extent)
                       off += frd_size;
                /* did not reach end of contiguous block in filetype.
                   no more I/O needed. off is incremented by frd_size. */
                else {
                    j = (j+1) % flat_file->count;
                    n_filetypes += (j == 0) ? 1 : 0;
                    while (flat_file->blocklens[j]==0) {
                        j = (j+1) % flat_file->count;
                        n_filetypes += (j == 0) ? 1 : 0;
                    }
		    off = disp + flat_file->indices[j] + 
                                        n_filetypes*(ADIO_Offset)filetype_extent;
		    frd_size = ADIOI_MIN(flat_file->blocklens[j], bufsize-i_offset);
		}
	    }
	}
	else {
/* noncontiguous in memory as well as in file */

	    ADIOI_Flatten_datatype(datatype);
	    flat_buf = ADIOI_Flatlist;
	    while (flat_buf->type != datatype) flat_buf = flat_buf->next;

	    k = num = buf_count = 0;
	    i_offset = flat_buf->indices[0];
	    j = st_index;
	    off = offset;
	    n_filetypes = st_n_filetypes;
	    frd_size = st_frd_size;
	    brd_size = flat_buf->blocklens[0];

	    while (num < bufsize) {
		size = ADIOI_MIN(frd_size, brd_size);
		if (size) {
		    /* lseek(fd->fd_sys, off, SEEK_SET);
		    err = read(fd->fd_sys, ((char *) buf) + i, size); */

		    req_off = off;
		    req_len = size;
		    userbuf_off = i_offset;
		    ADIOI_BUFFERED_READ
		}

		new_frd_size = frd_size;
		new_brd_size = brd_size;

		if (size == frd_size) {
/* reached end of contiguous block in file */
                    j = (j+1) % flat_file->count;
                    n_filetypes += (j == 0) ? 1 : 0;
                    while (flat_file->blocklens[j]==0) {
                        j = (j+1) % flat_file->count;
                        n_filetypes += (j == 0) ? 1 : 0;
                    }
		    off = disp + flat_file->indices[j] + 
          n_filetypes*(ADIO_Offset)filetype_extent;

		    new_frd_size = flat_file->blocklens[j];
		    if (size != brd_size) {
			i_offset += size;
			new_brd_size -= size;
		    }
		}

		if (size == brd_size) {
/* reached end of contiguous block in memory */

		    k = (k + 1)%flat_buf->count;
		    buf_count++;
		    i_offset = ((ADIO_Offset)buftype_extent*(ADIO_Offset)(buf_count/flat_buf->count) +
			flat_buf->indices[k]);
		    new_brd_size = flat_buf->blocklens[k];
		    if (size != frd_size) {
			off += size;
			new_frd_size -= size;
		    }
		}
    ADIOI_Assert(((ADIO_Offset)num + size) == (unsigned)(num + size));
		num += size;
		frd_size = new_frd_size;
                brd_size = new_brd_size;
	    }
	}
	
        if ((fd->atomicity) && ADIO_Feature(fd, ADIO_LOCKS))
            ADIOI_UNLOCK(fd, start_off, SEEK_SET, end_offset-start_off+1);

	if (file_ptr_type == ADIO_INDIVIDUAL) fd->fp_ind = off;

	ADIOI_Free(readbuf); /* malloced in the buffered_read macro */
    }

    fd->fp_sys_posn = -1;   /* set it to null. */

#ifdef HAVE_STATUS_SET_BYTES
    MPIR_Status_set_bytes(status, datatype, bufsize);
/* This is a temporary way of filling in status. The right way is to 
   keep track of how much data was actually read and placed in buf 
   by ADIOI_BUFFERED_READ. */
#endif

    if (!buftype_is_contig) ADIOI_Delete_flattened(datatype);
}
