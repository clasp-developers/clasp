/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *   Copyright (C) 1997 University of Chicago.
 *   See COPYRIGHT notice in top-level directory.
 *
 *   Copyright (C) 2007 Oak Ridge National Laboratory
 *
 *   Copyright (C) 2008 Sun Microsystems, Lustre group
 */

#include "ad_lustre.h"
#include "adio_extern.h"

/* prototypes of functions used for collective writes only. */
static void ADIOI_LUSTRE_Exch_and_write(ADIO_File fd, void *buf,
					MPI_Datatype datatype, int nprocs,
					int myrank,
					ADIOI_Access *others_req,
					ADIOI_Access *my_req,
					ADIO_Offset *offset_list,
					ADIO_Offset *len_list,
					int contig_access_count,
					int *striping_info,
                                        int **buf_idx, int *error_code);
static void ADIOI_LUSTRE_Fill_send_buffer(ADIO_File fd, void *buf,
					  ADIOI_Flatlist_node *flat_buf,
					  char **send_buf,
					  ADIO_Offset *offset_list,
					  ADIO_Offset *len_list, int *send_size,
					  MPI_Request *requests,
					  int *sent_to_proc, int nprocs,
					  int myrank, int contig_access_count,
					  int *striping_info,
					  int *send_buf_idx,
                                          int *curr_to_proc,
					  int *done_to_proc, int iter,
					  MPI_Aint buftype_extent);
static void ADIOI_LUSTRE_W_Exchange_data(ADIO_File fd, void *buf,
					 char *write_buf,
					 ADIOI_Flatlist_node *flat_buf,
					 ADIO_Offset *offset_list,
					 ADIO_Offset *len_list, int *send_size,
					 int *recv_size, ADIO_Offset off,
					 int size, int *count,
					 int *start_pos, int *partial_recv,
					 int *sent_to_proc, int nprocs,
					 int myrank, int buftype_is_contig,
					 int contig_access_count,
					 int *striping_info,
					 ADIOI_Access *others_req,
					 int *send_buf_idx,
					 int *curr_to_proc,
					 int *done_to_proc, int *hole,
					 int iter, MPI_Aint buftype_extent,
					 int *buf_idx, int *error_code);
void ADIOI_Heap_merge(ADIOI_Access *others_req, int *count,
                      ADIO_Offset *srt_off, int *srt_len, int *start_pos,
                      int nprocs, int nprocs_recv, int total_elements);

void ADIOI_LUSTRE_WriteStridedColl(ADIO_File fd, void *buf, int count,
				   MPI_Datatype datatype,
				   int file_ptr_type, ADIO_Offset offset,
				   ADIO_Status *status, int *error_code)
{
    /* Uses a generalized version of the extended two-phase method described
     * in "An Extended Two-Phase Method for Accessing Sections of
     * Out-of-Core Arrays", Rajeev Thakur and Alok Choudhary,
     * Scientific Programming, (5)4:301--317, Winter 1996.
     * http://www.mcs.anl.gov/home/thakur/ext2ph.ps
     */

    ADIOI_Access *my_req;
    /* array of nprocs access structures, one for each other process has
       this process's request */

    ADIOI_Access *others_req;
    /* array of nprocs access structures, one for each other process
       whose request is written by this process. */

    int i, filetype_is_contig, nprocs, myrank, do_collect = 0;
    int contig_access_count = 0, buftype_is_contig, interleave_count = 0;
    int *count_my_req_per_proc, count_my_req_procs, count_others_req_procs;
    ADIO_Offset orig_fp, start_offset, end_offset, off;
    ADIO_Offset *offset_list = NULL, *st_offsets = NULL, *end_offsets = NULL;
    ADIO_Offset *len_list = NULL;
    int **buf_idx = NULL, *striping_info = NULL;
    int old_error, tmp_error;

    MPI_Comm_size(fd->comm, &nprocs);
    MPI_Comm_rank(fd->comm, &myrank);

    orig_fp = fd->fp_ind;

    /* IO patten identification if cb_write isn't disabled */
    if (fd->hints->cb_write != ADIOI_HINT_DISABLE) {
	/* For this process's request, calculate the list of offsets and
	   lengths in the file and determine the start and end offsets. */

	/* Note: end_offset points to the last byte-offset that will be accessed.
         * e.g., if start_offset=0 and 100 bytes to be read, end_offset=99
         */

	ADIOI_Calc_my_off_len(fd, count, datatype, file_ptr_type, offset,
	                      &offset_list, &len_list, &start_offset,
	                      &end_offset, &contig_access_count);

	/* each process communicates its start and end offsets to other
         * processes. The result is an array each of start and end offsets
         * stored in order of process rank.
         */
	st_offsets = (ADIO_Offset *) ADIOI_Malloc(nprocs * sizeof(ADIO_Offset));
	end_offsets = (ADIO_Offset *) ADIOI_Malloc(nprocs * sizeof(ADIO_Offset));
	MPI_Allgather(&start_offset, 1, ADIO_OFFSET, st_offsets, 1,
		      ADIO_OFFSET, fd->comm);
	MPI_Allgather(&end_offset, 1, ADIO_OFFSET, end_offsets, 1,
		      ADIO_OFFSET, fd->comm);
	/* are the accesses of different processes interleaved? */
	for (i = 1; i < nprocs; i++)
	    if ((st_offsets[i] < end_offsets[i-1]) &&
                (st_offsets[i] <= end_offsets[i]))
                interleave_count++;
	/* This is a rudimentary check for interleaving, but should suffice
	   for the moment. */

	/* Two typical access patterns can benefit from collective write.
         *   1) the processes are interleaved, and
         *   2) the req size is small.
         */
        if (interleave_count > 0) {
	    do_collect = 1;
        } else {
            do_collect = ADIOI_LUSTRE_Docollect(fd, contig_access_count,
			                        len_list, nprocs);
        }
    }
    ADIOI_Datatype_iscontig(datatype, &buftype_is_contig);

    /* Decide if collective I/O should be done */
    if ((!do_collect && fd->hints->cb_write == ADIOI_HINT_AUTO) ||
        fd->hints->cb_write == ADIOI_HINT_DISABLE) {

	/* use independent accesses */
	if (fd->hints->cb_write != ADIOI_HINT_DISABLE) {
	    ADIOI_Free(offset_list);
	    ADIOI_Free(len_list);
            ADIOI_Free(st_offsets);
            ADIOI_Free(end_offsets);
	}

	fd->fp_ind = orig_fp;
	ADIOI_Datatype_iscontig(fd->filetype, &filetype_is_contig);
	if (buftype_is_contig && filetype_is_contig) {
	    if (file_ptr_type == ADIO_EXPLICIT_OFFSET) {
                off = fd->disp + (ADIO_Offset)(fd->etype_size) * offset;
		ADIO_WriteContig(fd, buf, count, datatype,
				 ADIO_EXPLICIT_OFFSET,
				 off, status, error_code);
	    } else
		ADIO_WriteContig(fd, buf, count, datatype, ADIO_INDIVIDUAL,
				 0, status, error_code);
	} else {
	    ADIO_WriteStrided(fd, buf, count, datatype, file_ptr_type,
			      offset, status, error_code);
	}
	return;
    }

    /* Get Lustre hints information */
    ADIOI_LUSTRE_Get_striping_info(fd, &striping_info, 1);

    /* calculate what portions of the access requests of this process are
     * located in which process
     */
    ADIOI_LUSTRE_Calc_my_req(fd, offset_list, len_list, contig_access_count,
                             striping_info, nprocs, &count_my_req_procs,
                             &count_my_req_per_proc, &my_req,
                             &buf_idx);

    /* based on everyone's my_req, calculate what requests of other processes
     * will be accessed by this process.
     * count_others_req_procs = number of processes whose requests (including
     * this process itself) will be accessed by this process
     * count_others_req_per_proc[i] indicates how many separate contiguous
     * requests of proc. i will be accessed by this process.
     */

    ADIOI_Calc_others_req(fd, count_my_req_procs, count_my_req_per_proc,
                          my_req, nprocs, myrank, &count_others_req_procs,
                          &others_req);
    ADIOI_Free(count_my_req_per_proc);

    /* exchange data and write in sizes of no more than stripe_size. */
    ADIOI_LUSTRE_Exch_and_write(fd, buf, datatype, nprocs, myrank,
                                others_req, my_req, offset_list, len_list,
                                contig_access_count, striping_info,
                                buf_idx, error_code);

    /* If this collective write is followed by an independent write,
     * it's possible to have those subsequent writes on other processes
     * race ahead and sneak in before the read-modify-write completes.
     * We carry out a collective communication at the end here so no one
     * can start independent i/o before collective I/O completes.
     *
     * need to do some gymnastics with the error codes so that if something
     * went wrong, all processes report error, but if a process has a more
     * specific error code, we can still have that process report the
     * additional information */

    old_error = *error_code;
    if (*error_code != MPI_SUCCESS)
	*error_code = MPI_ERR_IO;

    /* optimization: if only one process performing i/o, we can perform
     * a less-expensive Bcast  */
#ifdef ADIOI_MPE_LOGGING
    MPE_Log_event(ADIOI_MPE_postwrite_a, 0, NULL);
#endif
    if (fd->hints->cb_nodes == 1)
	MPI_Bcast(error_code, 1, MPI_INT,
		  fd->hints->ranklist[0], fd->comm);
    else {
	tmp_error = *error_code;
	MPI_Allreduce(&tmp_error, error_code, 1, MPI_INT,
		      MPI_MAX, fd->comm);
    }
#ifdef ADIOI_MPE_LOGGING
    MPE_Log_event(ADIOI_MPE_postwrite_b, 0, NULL);
#endif

    if ((old_error != MPI_SUCCESS) && (old_error != MPI_ERR_IO))
	*error_code = old_error;


    if (!buftype_is_contig)
	ADIOI_Delete_flattened(datatype);

    /* free all memory allocated for collective I/O */
    /* free others_req */
    for (i = 0; i < nprocs; i++) {
	if (others_req[i].count) {
	    ADIOI_Free(others_req[i].offsets);
	    ADIOI_Free(others_req[i].lens);
	    ADIOI_Free(others_req[i].mem_ptrs);
	}
    }
    ADIOI_Free(others_req);
    /* free my_req here */
    for (i = 0; i < nprocs; i++) {
	if (my_req[i].count) {
	    ADIOI_Free(my_req[i].offsets);
	    ADIOI_Free(my_req[i].lens);
	}
    }
    ADIOI_Free(my_req);
    for (i = 0; i < nprocs; i++) {
        ADIOI_Free(buf_idx[i]);
    }
    ADIOI_Free(buf_idx);
    ADIOI_Free(offset_list);
    ADIOI_Free(len_list);
    ADIOI_Free(st_offsets);
    ADIOI_Free(end_offsets);
    ADIOI_Free(striping_info);

#ifdef HAVE_STATUS_SET_BYTES
    if (status) {
	int bufsize, size;
	/* Don't set status if it isn't needed */
	MPI_Type_size(datatype, &size);
	bufsize = size * count;
	MPIR_Status_set_bytes(status, datatype, bufsize);
    }
    /* This is a temporary way of filling in status. The right way is to
     * keep track of how much data was actually written during collective I/O.
     */
#endif

    fd->fp_sys_posn = -1;	/* set it to null. */
}

/* If successful, error_code is set to MPI_SUCCESS.  Otherwise an error
 * code is created and returned in error_code.
 */
static void ADIOI_LUSTRE_Exch_and_write(ADIO_File fd, void *buf,
					MPI_Datatype datatype, int nprocs,
					int myrank, ADIOI_Access *others_req,
                                        ADIOI_Access *my_req,
					ADIO_Offset *offset_list,
                                        ADIO_Offset *len_list, 
					int contig_access_count,
                                        int *striping_info, int **buf_idx,
                                        int *error_code)
{
    /* Send data to appropriate processes and write in sizes of no more
     * than lustre stripe_size.
     * The idea is to reduce the amount of extra memory required for
     * collective I/O. If all data were written all at once, which is much
     * easier, it would require temp space more than the size of user_buf,
     * which is often unacceptable. For example, to write a distributed
     * array to a file, where each local array is 8Mbytes, requiring
     * at least another 8Mbytes of temp space is unacceptable.
     */

    int hole, i, j, m, flag, ntimes = 1 , max_ntimes, buftype_is_contig;
    ADIO_Offset st_loc = -1, end_loc = -1, min_st_loc, max_end_loc;
    ADIO_Offset off, req_off, send_off, iter_st_off, *off_list;
    ADIO_Offset max_size, step_size = 0;
    int real_size, req_len, send_len;
    int *recv_curr_offlen_ptr, *recv_count, *recv_size;
    int *send_curr_offlen_ptr, *send_size;
    int *partial_recv, *sent_to_proc, *recv_start_pos;
    int *send_buf_idx, *curr_to_proc, *done_to_proc;
    int *this_buf_idx;
    char *write_buf = NULL;
    MPI_Status status;
    ADIOI_Flatlist_node *flat_buf = NULL;
    MPI_Aint buftype_extent;
    int stripe_size = striping_info[0], avail_cb_nodes = striping_info[2];
    int data_sieving = 0;

    *error_code = MPI_SUCCESS;	/* changed below if error */
    /* only I/O errors are currently reported */

    /* calculate the number of writes of stripe size to be done.
     * That gives the no. of communication phases as well.
     * Note:
     *   Because we redistribute data in stripe-contiguous pattern for Lustre,
     *   each process has the same no. of communication phases.
     */

    for (i = 0; i < nprocs; i++) {
	if (others_req[i].count) {
	    st_loc = others_req[i].offsets[0];
	    end_loc = others_req[i].offsets[0];
	    break;
	}
    }
    for (i = 0; i < nprocs; i++) {
	for (j = 0; j < others_req[i].count; j++) {
	    st_loc = ADIOI_MIN(st_loc, others_req[i].offsets[j]);
	    end_loc = ADIOI_MAX(end_loc, (others_req[i].offsets[j] +
                                          others_req[i].lens[j] - 1));
	}
    }
    /* this process does no writing. */
    if ((st_loc == -1) && (end_loc == -1))
	ntimes = 0;
    MPI_Allreduce(&end_loc, &max_end_loc, 1, MPI_LONG_LONG_INT, MPI_MAX, fd->comm);
    /* avoid min_st_loc be -1 */
    if (st_loc == -1)
        st_loc = max_end_loc;
    MPI_Allreduce(&st_loc, &min_st_loc, 1, MPI_LONG_LONG_INT, MPI_MIN, fd->comm);
    /* align downward */
    min_st_loc -= min_st_loc % (ADIO_Offset)stripe_size;

    /* Each time, only avail_cb_nodes number of IO clients perform IO,
     * so, step_size=avail_cb_nodes*stripe_size IO will be performed at most,
     * and ntimes=whole_file_portion/step_size
     */
    step_size = (ADIO_Offset) avail_cb_nodes * stripe_size;
    max_ntimes = (max_end_loc - min_st_loc + 1) / step_size
        + (((max_end_loc - min_st_loc + 1) % step_size) ? 1 : 0);
/*     max_ntimes = (int)((max_end_loc - min_st_loc) / step_size + 1); */
    if (ntimes)
	write_buf = (char *) ADIOI_Malloc(stripe_size);

    /* calculate the start offset for each iteration */
    off_list = (ADIO_Offset *) ADIOI_Malloc(max_ntimes * sizeof(ADIO_Offset));
    for (m = 0; m < max_ntimes; m ++)
        off_list[m] = max_end_loc;
    for (i = 0; i < nprocs; i++) {
        for (j = 0; j < others_req[i].count; j ++) {
            req_off = others_req[i].offsets[j];
            m = (int)((req_off - min_st_loc) / step_size);
            off_list[m] = ADIOI_MIN(off_list[m], req_off);
        }
    }

    recv_curr_offlen_ptr = (int *) ADIOI_Calloc(nprocs, sizeof(int));
    send_curr_offlen_ptr = (int *) ADIOI_Calloc(nprocs, sizeof(int));
    /* their use is explained below. calloc initializes to 0. */

    recv_count = (int *) ADIOI_Malloc(nprocs * sizeof(int));
    /* to store count of how many off-len pairs per proc are satisfied
       in an iteration. */

    send_size = (int *) ADIOI_Malloc(nprocs * sizeof(int));
    /* total size of data to be sent to each proc. in an iteration.
       Of size nprocs so that I can use MPI_Alltoall later. */

    recv_size = (int *) ADIOI_Malloc(nprocs * sizeof(int));
    /* total size of data to be recd. from each proc. in an iteration. */

    sent_to_proc = (int *) ADIOI_Calloc(nprocs, sizeof(int));
    /* amount of data sent to each proc so far. Used in
       ADIOI_Fill_send_buffer. initialized to 0 here. */

    send_buf_idx = (int *) ADIOI_Malloc(nprocs * sizeof(int));
    curr_to_proc = (int *) ADIOI_Malloc(nprocs * sizeof(int));
    done_to_proc = (int *) ADIOI_Malloc(nprocs * sizeof(int));
    /* Above three are used in ADIOI_Fill_send_buffer */

    this_buf_idx = (int *) ADIOI_Malloc(nprocs * sizeof(int));

    recv_start_pos = (int *) ADIOI_Malloc(nprocs * sizeof(int));
    /* used to store the starting value of recv_curr_offlen_ptr[i] in
       this iteration */

    ADIOI_Datatype_iscontig(datatype, &buftype_is_contig);
    if (!buftype_is_contig) {
	ADIOI_Flatten_datatype(datatype);
	flat_buf = ADIOI_Flatlist;
	while (flat_buf->type != datatype)
	    flat_buf = flat_buf->next;
    }
    MPI_Type_extent(datatype, &buftype_extent);
    /* I need to check if there are any outstanding nonblocking writes to
     * the file, which could potentially interfere with the writes taking
     * place in this collective write call. Since this is not likely to be
     * common, let me do the simplest thing possible here: Each process
     * completes all pending nonblocking operations before completing.
     */
    /*ADIOI_Complete_async(error_code);
    if (*error_code != MPI_SUCCESS) return;
    MPI_Barrier(fd->comm);
    */

    iter_st_off = min_st_loc;

    /* Although we have recognized the data according to OST index,
     * a read-modify-write will be done if there is a hole between the data.
     * For example: if blocksize=60, xfersize=30 and stripe_size=100,
     * then rank0 will collect data [0, 30] and [60, 90] then write. There
     * is a hole in [30, 60], which will cause a read-modify-write in [0, 90].
     *
     * To reduce its impact on the performance, we can disable data sieving
     * by hint "ds_in_coll".
     */
    /* check the hint for data sieving */
    data_sieving = fd->hints->fs_hints.lustre.ds_in_coll;

    for (m = 0; m < max_ntimes; m++) {
	/* go through all others_req and my_req to check which will be received
         * and sent in this iteration.
         */

	/* Note that MPI guarantees that displacements in filetypes are in
	   monotonically nondecreasing order and that, for writes, the
	   filetypes cannot specify overlapping regions in the file. This
	   simplifies implementation a bit compared to reads. */

	/*
           off         = start offset in the file for the data to be written in
                         this iteration
           iter_st_off = start offset of this iteration
           real_size   = size of data written (bytes) corresponding to off
           max_size    = possible maximum size of data written in this iteration
           req_off     = offset in the file for a particular contiguous request minus
                         what was satisfied in previous iteration
           send_off    = offset the request needed by other processes in this iteration
           req_len     = size corresponding to req_off
           send_len    = size corresponding to send_off
         */

	/* first calculate what should be communicated */
	for (i = 0; i < nprocs; i++)
	    recv_count[i] = recv_size[i] = send_size[i] = 0;

        off = off_list[m];
        max_size = ADIOI_MIN(step_size, max_end_loc - iter_st_off + 1);
        real_size = (int) ADIOI_MIN((off / stripe_size + 1) * stripe_size -
                                    off,
                                    end_loc - off + 1);

	for (i = 0; i < nprocs; i++) {
            if (my_req[i].count) {
                this_buf_idx[i] = buf_idx[i][send_curr_offlen_ptr[i]];
                for (j = send_curr_offlen_ptr[i]; j < my_req[i].count; j++) {
                    send_off = my_req[i].offsets[j];
                    send_len = my_req[i].lens[j];
                    if (send_off < iter_st_off + max_size) {
                        send_size[i] += send_len;
                    } else {
                        break;
                    }
                }
                send_curr_offlen_ptr[i] = j;
            }
	    if (others_req[i].count) {
		recv_start_pos[i] = recv_curr_offlen_ptr[i];
		for (j = recv_curr_offlen_ptr[i]; j < others_req[i].count; j++) {
                    req_off = others_req[i].offsets[j];
                    req_len = others_req[i].lens[j];
		    if (req_off < iter_st_off + max_size) {
			recv_count[i]++;
                        ADIOI_Assert((((ADIO_Offset)(MPIR_Upint)write_buf)+req_off-off) == (ADIO_Offset)(MPIR_Upint)(write_buf+req_off-off));
			MPI_Address(write_buf + req_off - off,
				    &(others_req[i].mem_ptrs[j]));
                        recv_size[i] += req_len;
		    } else {
			break;
                    }
		}
		recv_curr_offlen_ptr[i] = j;
	    }
	}
        /* use variable "hole" to pass data_sieving flag into W_Exchange_data */
        hole = data_sieving;
	ADIOI_LUSTRE_W_Exchange_data(fd, buf, write_buf, flat_buf, offset_list,
                                     len_list, send_size, recv_size, off, real_size,
                                     recv_count, recv_start_pos, partial_recv,
                                     sent_to_proc, nprocs, myrank,
                                     buftype_is_contig, contig_access_count,
                                     striping_info, others_req, send_buf_idx,
                                     curr_to_proc, done_to_proc, &hole, m,
                                     buftype_extent, this_buf_idx, error_code);
	if (*error_code != MPI_SUCCESS)
            goto over;

	flag = 0;
	for (i = 0; i < nprocs; i++)
	    if (recv_count[i]) {
		flag = 1;
		break;
	    }
	if (flag) {
            /* check whether to do data sieving */
            if(data_sieving == ADIOI_HINT_ENABLE) {
	        ADIO_WriteContig(fd, write_buf, real_size, MPI_BYTE,
			         ADIO_EXPLICIT_OFFSET, off, &status,
			         error_code);
            } else {
                /* if there is no hole, write data in one time;
                 * otherwise, write data in several times */
                if (!hole) {
                    ADIO_WriteContig(fd, write_buf, real_size, MPI_BYTE,
                                     ADIO_EXPLICIT_OFFSET, off, &status,
                                     error_code);
                } else {
                    for (i = 0; i < nprocs; i++) {
                        if (others_req[i].count) {
                            for (j = 0; j < others_req[i].count; j++) {
                                if (others_req[i].offsets[j] < off + real_size &&
                                    others_req[i].offsets[j] >= off) {
                                    ADIO_WriteContig(fd,
                                                     write_buf + others_req[i].offsets[j] - off,
                                                     others_req[i].lens[j],
                                                     MPI_BYTE, ADIO_EXPLICIT_OFFSET,
                                                     others_req[i].offsets[j], &status,
                                                     error_code);
	                            if (*error_code != MPI_SUCCESS)
		                        goto over;
                                }
                            }
                        }
                    }
                }
            }
	    if (*error_code != MPI_SUCCESS)
		goto over;
	}
        iter_st_off += max_size;
    }
over:
    if (ntimes)
	ADIOI_Free(write_buf);
    ADIOI_Free(recv_curr_offlen_ptr);
    ADIOI_Free(send_curr_offlen_ptr);
    ADIOI_Free(recv_count);
    ADIOI_Free(send_size);
    ADIOI_Free(recv_size);
    ADIOI_Free(sent_to_proc);
    ADIOI_Free(recv_start_pos);
    ADIOI_Free(send_buf_idx);
    ADIOI_Free(curr_to_proc);
    ADIOI_Free(done_to_proc);
    ADIOI_Free(this_buf_idx);
    ADIOI_Free(off_list);
}

/* Sets error_code to MPI_SUCCESS if successful, or creates an error code
 * in the case of error.
 */
static void ADIOI_LUSTRE_W_Exchange_data(ADIO_File fd, void *buf,
					 char *write_buf,
					 ADIOI_Flatlist_node *flat_buf,
					 ADIO_Offset *offset_list,
					 ADIO_Offset *len_list, int *send_size,
					 int *recv_size, ADIO_Offset off,
					 int size, int *count,
					 int *start_pos, int *partial_recv,
					 int *sent_to_proc, int nprocs,
					 int myrank, int buftype_is_contig,
					 int contig_access_count,
					 int *striping_info,
					 ADIOI_Access *others_req,
					 int *send_buf_idx,
					 int *curr_to_proc, int *done_to_proc,
                                         int *hole, int iter,
                                         MPI_Aint buftype_extent,
					 int *buf_idx, int *error_code)
{
    int i, j, nprocs_recv, nprocs_send, err;
    char **send_buf = NULL;
    MPI_Request *requests, *send_req;
    MPI_Datatype *recv_types;
    MPI_Status *statuses, status;
    int *srt_len, sum, sum_recv;
    ADIO_Offset *srt_off;
    int data_sieving = *hole;
    static char myname[] = "ADIOI_W_EXCHANGE_DATA";

    /* create derived datatypes for recv */
    nprocs_recv = 0;
    for (i = 0; i < nprocs; i++)
	if (recv_size[i])
	    nprocs_recv++;

    recv_types = (MPI_Datatype *) ADIOI_Malloc((nprocs_recv + 1) *
					       sizeof(MPI_Datatype));
    /* +1 to avoid a 0-size malloc */

    j = 0;
    for (i = 0; i < nprocs; i++) {
	if (recv_size[i]) {
	    MPI_Type_hindexed(count[i],
			      &(others_req[i].lens[start_pos[i]]),
			      &(others_req[i].mem_ptrs[start_pos[i]]),
			      MPI_BYTE, recv_types + j);
	    /* absolute displacements; use MPI_BOTTOM in recv */
	    MPI_Type_commit(recv_types + j);
	    j++;
	}
    }

    /* To avoid a read-modify-write,
     * check if there are holes in the data to be written.
     * For this, merge the (sorted) offset lists others_req using a heap-merge.
     */

    sum = 0;
    for (i = 0; i < nprocs; i++)
	sum += count[i];
    srt_off = (ADIO_Offset *) ADIOI_Malloc((sum + 1) * sizeof(ADIO_Offset));
    srt_len = (int *) ADIOI_Malloc((sum + 1) * sizeof(int));
    /* +1 to avoid a 0-size malloc */

    ADIOI_Heap_merge(others_req, count, srt_off, srt_len, start_pos,
		     nprocs, nprocs_recv, sum);

    /* check if there are any holes */
    *hole = 0;
    for (i = 0; i < sum - 1; i++) {
        if (srt_off[i] + srt_len[i] < srt_off[i + 1]) {
            *hole = 1;
	    break;
	}
    }
    /* In some cases (see John Bent ROMIO REQ # 835), an odd interaction
     * between aggregation, nominally contiguous regions, and cb_buffer_size
     * should be handled with a read-modify-write (otherwise we will write out
     * more data than we receive from everyone else (inclusive), so override
     * hole detection
     */
    if (*hole == 0) {
        sum_recv = 0;
        for (i = 0; i < nprocs; i++)
            sum_recv += recv_size[i];
	if (size > sum_recv)
	    *hole = 1;
    }
    /* check the hint for data sieving */
    if (data_sieving == ADIOI_HINT_ENABLE && nprocs_recv && *hole) {
        ADIO_ReadContig(fd, write_buf, size, MPI_BYTE,
                        ADIO_EXPLICIT_OFFSET, off, &status, &err);
        // --BEGIN ERROR HANDLING--
        if (err != MPI_SUCCESS) {
            *error_code = MPIO_Err_create_code(err,
                                               MPIR_ERR_RECOVERABLE,
                                               myname, __LINE__,
                                               MPI_ERR_IO,
                                               "**ioRMWrdwr", 0);
            ADIOI_Free(recv_types);
            ADIOI_Free(srt_off);
            ADIOI_Free(srt_len);
            return;
        }
        // --END ERROR HANDLING--
    }
    ADIOI_Free(srt_off);
    ADIOI_Free(srt_len);

    nprocs_send = 0;
    for (i = 0; i < nprocs; i++)
	if (send_size[i])
	    nprocs_send++;

    if (fd->atomicity) {
	/* bug fix from Wei-keng Liao and Kenin Coloma */
	requests = (MPI_Request *) ADIOI_Malloc((nprocs_send + 1) *
                                                sizeof(MPI_Request));
	send_req = requests;
    } else {
	requests = (MPI_Request *) ADIOI_Malloc((nprocs_send + nprocs_recv + 1)*
                                                sizeof(MPI_Request));
	/* +1 to avoid a 0-size malloc */

	/* post receives */
	j = 0;
	for (i = 0; i < nprocs; i++) {
	    if (recv_size[i]) {
		MPI_Irecv(MPI_BOTTOM, 1, recv_types[j], i,
			  myrank + i + 100 * iter, fd->comm, requests + j);
		j++;
	    }
	}
	send_req = requests + nprocs_recv;
    }

    /* post sends.
     * if buftype_is_contig, data can be directly sent from
     * user buf at location given by buf_idx. else use send_buf.
     */
    if (buftype_is_contig) {
	j = 0;
	for (i = 0; i < nprocs; i++)
	    if (send_size[i]) {
                ADIOI_Assert(buf_idx[i] != -1);
		MPI_Isend(((char *) buf) + buf_idx[i], send_size[i],
			  MPI_BYTE, i, myrank + i + 100 * iter, fd->comm,
			  send_req + j);
		j++;
	    }
    } else
        if (nprocs_send) {
	/* buftype is not contig */
	send_buf = (char **) ADIOI_Malloc(nprocs * sizeof(char *));
	for (i = 0; i < nprocs; i++)
	    if (send_size[i])
		send_buf[i] = (char *) ADIOI_Malloc(send_size[i]);

	ADIOI_LUSTRE_Fill_send_buffer(fd, buf, flat_buf, send_buf, offset_list,
                                      len_list, send_size, send_req,
                                      sent_to_proc, nprocs, myrank,
                                      contig_access_count, striping_info,
                                      send_buf_idx, curr_to_proc, done_to_proc,
                                      iter, buftype_extent);
	/* the send is done in ADIOI_Fill_send_buffer */
    }

	/* bug fix from Wei-keng Liao and Kenin Coloma */
    if (fd->atomicity) {
	j = 0;
	for (i = 0; i < nprocs; i++) {
	    MPI_Status wkl_status;
	    if (recv_size[i]) {
		MPI_Recv(MPI_BOTTOM, 1, recv_types[j], i,
			 myrank + i + 100 * iter, fd->comm, &wkl_status);
		j++;
	    }
	}
    }

    for (i = 0; i < nprocs_recv; i++)
	MPI_Type_free(recv_types + i);
    ADIOI_Free(recv_types);

	/* bug fix from Wei-keng Liao and Kenin Coloma */
	/* +1 to avoid a 0-size malloc */
    if (fd->atomicity) {
	statuses = (MPI_Status *) ADIOI_Malloc((nprocs_send + 1) *
					       sizeof(MPI_Status));
    } else {
	statuses = (MPI_Status *) ADIOI_Malloc((nprocs_send + nprocs_recv + 1) *
					       sizeof(MPI_Status));
    }

#ifdef NEEDS_MPI_TEST
    i = 0;
    if (fd->atomicity) {
	/* bug fix from Wei-keng Liao and Kenin Coloma */
	while (!i)
	    MPI_Testall(nprocs_send, send_req, &i, statuses);
    } else {
	while (!i)
	    MPI_Testall(nprocs_send + nprocs_recv, requests, &i, statuses);
    }
#else
	/* bug fix from Wei-keng Liao and Kenin Coloma */
    if (fd->atomicity)
	MPI_Waitall(nprocs_send, send_req, statuses);
    else
	MPI_Waitall(nprocs_send + nprocs_recv, requests, statuses);
#endif
    ADIOI_Free(statuses);
    ADIOI_Free(requests);
    if (!buftype_is_contig && nprocs_send) {
	for (i = 0; i < nprocs; i++)
	    if (send_size[i])
		ADIOI_Free(send_buf[i]);
	ADIOI_Free(send_buf);
    }
}

#define ADIOI_BUF_INCR \
{ \
    while (buf_incr) { \
        size_in_buf = ADIOI_MIN(buf_incr, flat_buf_sz); \
        user_buf_idx += size_in_buf; \
        flat_buf_sz -= size_in_buf; \
        if (!flat_buf_sz) { \
            if (flat_buf_idx < (flat_buf->count - 1)) flat_buf_idx++; \
            else { \
                flat_buf_idx = 0; \
                n_buftypes++; \
            } \
            user_buf_idx = flat_buf->indices[flat_buf_idx] + \
                (ADIO_Offset)n_buftypes*(ADIO_Offset)buftype_extent;  \
            flat_buf_sz = flat_buf->blocklens[flat_buf_idx]; \
        } \
        buf_incr -= size_in_buf; \
    } \
}


#define ADIOI_BUF_COPY \
{ \
    while (size) { \
        size_in_buf = ADIOI_MIN(size, flat_buf_sz); \
        ADIOI_Assert((((ADIO_Offset)(MPIR_Upint)buf) + user_buf_idx) == (ADIO_Offset)(MPIR_Upint)((MPIR_Upint)buf + user_buf_idx)); \
        ADIOI_Assert(size_in_buf == (size_t)size_in_buf);               \
        memcpy(&(send_buf[p][send_buf_idx[p]]), \
               ((char *) buf) + user_buf_idx, size_in_buf); \
        send_buf_idx[p] += size_in_buf; \
        user_buf_idx += size_in_buf; \
        flat_buf_sz -= size_in_buf; \
        if (!flat_buf_sz) { \
            if (flat_buf_idx < (flat_buf->count - 1)) flat_buf_idx++; \
            else { \
                flat_buf_idx = 0; \
                n_buftypes++; \
            } \
            user_buf_idx = flat_buf->indices[flat_buf_idx] + \
                (ADIO_Offset)n_buftypes*(ADIO_Offset)buftype_extent;    \
            flat_buf_sz = flat_buf->blocklens[flat_buf_idx]; \
        } \
        size -= size_in_buf; \
        buf_incr -= size_in_buf; \
    } \
    ADIOI_BUF_INCR \
}

static void ADIOI_LUSTRE_Fill_send_buffer(ADIO_File fd, void *buf,
					  ADIOI_Flatlist_node *flat_buf,
					  char **send_buf,
					  ADIO_Offset *offset_list,
					  ADIO_Offset *len_list, int *send_size,
					  MPI_Request *requests,
					  int *sent_to_proc, int nprocs,
					  int myrank,
					  int contig_access_count,
					  int *striping_info,
					  int *send_buf_idx,
					  int *curr_to_proc,
					  int *done_to_proc, int iter,
					  MPI_Aint buftype_extent)
{
    /* this function is only called if buftype is not contig */
    int i, p, flat_buf_idx, size;
    int flat_buf_sz, buf_incr, size_in_buf, jj, n_buftypes;
    ADIO_Offset off, len, rem_len, user_buf_idx;

    /* curr_to_proc[p] = amount of data sent to proc. p that has already
     * been accounted for so far
     * done_to_proc[p] = amount of data already sent to proc. p in
     * previous iterations
     * user_buf_idx = current location in user buffer
     * send_buf_idx[p] = current location in send_buf of proc. p
     */

    for (i = 0; i < nprocs; i++) {
	send_buf_idx[i] = curr_to_proc[i] = 0;
	done_to_proc[i] = sent_to_proc[i];
    }
    jj = 0;

    user_buf_idx = flat_buf->indices[0];
    flat_buf_idx = 0;
    n_buftypes = 0;
    flat_buf_sz = flat_buf->blocklens[0];

    /* flat_buf_idx = current index into flattened buftype
     * flat_buf_sz = size of current contiguous component in flattened buf
     */
    for (i = 0; i < contig_access_count; i++) {
	off = offset_list[i];
	rem_len = (ADIO_Offset) len_list[i];

	/*this request may span to more than one process */
	while (rem_len != 0) {
	    len = rem_len;
	    /* NOTE: len value is modified by ADIOI_Calc_aggregator() to be no
	     * longer than the single region that processor "p" is responsible
	     * for.
	     */
	    p = ADIOI_LUSTRE_Calc_aggregator(fd, off, &len, striping_info);

	    if (send_buf_idx[p] < send_size[p]) {
		if (curr_to_proc[p] + len > done_to_proc[p]) {
		    if (done_to_proc[p] > curr_to_proc[p]) {
			size = (int) ADIOI_MIN(curr_to_proc[p] + len -
					       done_to_proc[p],
					       send_size[p] -
					       send_buf_idx[p]);
			buf_incr = done_to_proc[p] - curr_to_proc[p];
			ADIOI_BUF_INCR
                        ADIOI_Assert((curr_to_proc[p] + len - done_to_proc[p]) == (unsigned)(curr_to_proc[p] + len - done_to_proc[p]));
			    buf_incr = (int) (curr_to_proc[p] + len -
					      done_to_proc[p]);
                        ADIOI_Assert((done_to_proc[p] + size) == (unsigned)(done_to_proc[p] + size));
			curr_to_proc[p] = done_to_proc[p] + size;
		        ADIOI_BUF_COPY
                    } else {
			size = (int) ADIOI_MIN(len, send_size[p] -
					       send_buf_idx[p]);
			buf_incr = (int) len;
                        ADIOI_Assert((curr_to_proc[p] + size) == (unsigned)((ADIO_Offset)curr_to_proc[p] + size));
			curr_to_proc[p] += size;
		        ADIOI_BUF_COPY
                    }
		    if (send_buf_idx[p] == send_size[p]) {
			MPI_Isend(send_buf[p], send_size[p], MPI_BYTE, p,
				  myrank + p + 100 * iter, fd->comm,
				  requests + jj);
			jj++;
		    }
		} else {
                    ADIOI_Assert((curr_to_proc[p] + len) == (unsigned)((ADIO_Offset)curr_to_proc[p] + len));
		    curr_to_proc[p] += (int) len;
		    buf_incr = (int) len;
		    ADIOI_BUF_INCR
                }
	    } else {
		buf_incr = (int) len;
	        ADIOI_BUF_INCR
            }
	    off += len;
	    rem_len -= len;
	}
    }
    for (i = 0; i < nprocs; i++)
	if (send_size[i])
	    sent_to_proc[i] = curr_to_proc[i];
}
