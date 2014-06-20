/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   Copyright (C) 2008 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "assert.h"
#include "adio.h"
#include "adio_extern.h"
#ifdef AGGREGATION_PROFILE
#include "mpe.h"
#endif

/* #define ALLTOALL */

/* #define DEBUG */
/* #define DEBUG2 */  /* print buffers */

#define USE_PRE_REQ

static void Exch_data_amounts (ADIO_File fd, int nprocs,
			ADIO_Offset *client_comm_sz_arr,
			ADIO_Offset *agg_comm_sz_arr,
			int *client_alltoallw_counts,
			int *agg_alltoallw_counts,
			int *aggregators_done);
static void post_aggregator_comm (MPI_Comm comm, int rw_type, int nproc,
			   void *cb_buf,
			   MPI_Datatype *client_comm_dtype_arr,
			   ADIO_Offset *client_comm_sz_arr,
			   MPI_Request **requests,
			   int *aggregators_client_count_p);

static void post_client_comm (ADIO_File fd, int rw_type, 
		       int agg_rank, void *buf,
		       MPI_Datatype agg_comm_dtype,
		       int agg_alltoallw_count,
		       MPI_Request *request);

/* Avery Ching and Kenin Columa's reworked two-phase algorithm.  Key features
 * - persistent file domains
 * - an option to use alltoall instead of point-to-point
 */
void ADIOI_IOStridedColl (ADIO_File fd, void *buf, int count, int rdwr,
			  MPI_Datatype datatype, int file_ptr_type,
			  ADIO_Offset offset, ADIO_Status *status,
			  int *error_code)
{
    ADIO_Offset min_st_offset=0, max_end_offset=0;
    ADIO_Offset st_end_offset[2];
    ADIO_Offset *all_st_end_offsets = NULL;
    int filetype_is_contig, buftype_is_contig, is_contig;
    ADIO_Offset orig_fp, off;
    int interleave_count = 0, i, nprocs, myrank, nprocs_for_coll;
    int cb_enable;
    ADIO_Offset bufsize;
    MPI_Aint extent, bufextent;
    int size;
    int agg_rank;

    ADIO_Offset agg_disp; /* aggregated file offset */
    MPI_Datatype agg_dtype; /* aggregated file datatype */

    int aggregators_done = 0;
    ADIO_Offset buffered_io_size = 0;

    int *alltoallw_disps;

    int *alltoallw_counts;
    int *client_alltoallw_counts;
    int *agg_alltoallw_counts;

    char *cb_buf = NULL;

    MPI_Datatype *client_comm_dtype_arr; /* aggregator perspective */
    MPI_Datatype *agg_comm_dtype_arr;    /* client perspective */
    ADIO_Offset *client_comm_sz_arr;     /* aggregator perspective */
    ADIO_Offset *agg_comm_sz_arr;        /* client perspective */

    /* file views for each client and aggregator */
    view_state *client_file_view_state_arr = NULL;
    view_state *agg_file_view_state_arr    = NULL;
    /* mem views for local process */
    view_state *my_mem_view_state_arr      = NULL;

    MPI_Status *agg_comm_statuses     = NULL;
    MPI_Request *agg_comm_requests    = NULL;
    MPI_Status *client_comm_statuses  = NULL;
    MPI_Request *client_comm_requests = NULL;
    int aggs_client_count = 0;
    int clients_agg_count = 0;

    MPI_Comm_size (fd->comm, &nprocs);
    MPI_Comm_rank (fd->comm, &myrank);
#ifdef DEBUG
    fprintf (stderr, "p%d: entering ADIOI_IOStridedColl\n", myrank);
#endif
#ifdef AGGREGATION_PROFILE
    if (rdwr == ADIOI_READ)
	MPE_Log_event (5010, 0, NULL);
    else
	MPE_Log_event (5012, 0, NULL);
#endif

    /* I need to check if there are any outstanding nonblocking writes
       to the file, which could potentially interfere with the writes
       taking place in this collective write call. Since this is not
       likely to be common, let me do the simplest thing possible here:
       Each process completes all pending nonblocking operations before
       completing. */

    nprocs_for_coll = fd->hints->cb_nodes;
    orig_fp = fd->fp_ind;

    if (rdwr == ADIOI_READ)
	cb_enable = fd->hints->cb_read;
    else
	cb_enable = fd->hints->cb_write;

    /* only check for interleaving if cb_read isn't disabled */
    if (cb_enable != ADIOI_HINT_DISABLE) {
	/* find the starting and ending byte of my I/O access */
	ADIOI_Calc_bounds (fd, count, datatype, file_ptr_type, offset,
			   &st_end_offset[0], &st_end_offset[1]);

	/* allocate an array of start/end pairs */
	all_st_end_offsets = (ADIO_Offset *)
	    ADIOI_Malloc (2*nprocs*sizeof(ADIO_Offset));
	MPI_Allgather (st_end_offset, 2, ADIO_OFFSET, all_st_end_offsets, 2,
		       ADIO_OFFSET, fd->comm);

	min_st_offset = all_st_end_offsets[0];
	max_end_offset = all_st_end_offsets[1];

	for (i=1; i<nprocs; i++) {
	    /* are the accesses of different processes interleaved? */
	    if ((all_st_end_offsets[i*2] < all_st_end_offsets[i*2-1]) &&
		(all_st_end_offsets[i*2] <= all_st_end_offsets[i*2+1]))
		interleave_count++;
	    /* This is a rudimentary check for interleaving, but should
	     * suffice for the moment. */
	    
	    min_st_offset = ADIOI_MIN(all_st_end_offsets[i*2],
				      min_st_offset);
	    max_end_offset = ADIOI_MAX(all_st_end_offsets[i*2+1],
				       max_end_offset);
	}
    }

    ADIOI_Datatype_iscontig (datatype, &buftype_is_contig);
    ADIOI_Datatype_iscontig (fd->filetype, &filetype_is_contig);

    if ((cb_enable == ADIOI_HINT_DISABLE
	 || (!interleave_count && (cb_enable == ADIOI_HINT_AUTO)))
	&& (fd->hints->cb_pfr != ADIOI_HINT_ENABLE)){
	if (cb_enable != ADIOI_HINT_DISABLE) {
	    ADIOI_Free (all_st_end_offsets);
	}

	if (buftype_is_contig && filetype_is_contig) {
	    if (file_ptr_type == ADIO_EXPLICIT_OFFSET) {
		off = fd->disp + (fd->etype_size) * offset;
		if (rdwr == ADIOI_READ)
		    ADIO_ReadContig(fd, buf, count, datatype,
				    ADIO_EXPLICIT_OFFSET, off, status,
				    error_code);
		else
		    ADIO_WriteContig(fd, buf, count, datatype,
				     ADIO_EXPLICIT_OFFSET, off, status,
				     error_code);
	    }
	    else {
		if (rdwr == ADIOI_READ)
		    ADIO_ReadContig(fd, buf, count, datatype, ADIO_INDIVIDUAL,
				    0, status, error_code);
		else
		    ADIO_WriteContig(fd, buf, count, datatype, ADIO_INDIVIDUAL,
				     0, status, error_code);
	    }
	}
	else {
	    if (rdwr == ADIOI_READ)
		ADIO_ReadStrided(fd, buf, count, datatype, file_ptr_type,
				 offset, status, error_code);
	    else
		ADIO_WriteStrided(fd, buf, count, datatype, file_ptr_type,
				  offset, status, error_code);
	}
	return;
    }

    MPI_Type_extent(datatype, &extent);
    bufextent = extent * count;
    MPI_Type_size(datatype, &size);
    bufsize = size * count;

    /* Calculate file realms */
    if ((fd->hints->cb_pfr != ADIOI_HINT_ENABLE) ||
	(fd->file_realm_types == NULL))
	ADIOI_Calc_file_realms (fd, min_st_offset, max_end_offset);

    my_mem_view_state_arr = (view_state *)
	ADIOI_Calloc (1, nprocs * sizeof(view_state));
    agg_file_view_state_arr = (view_state *)
	ADIOI_Calloc (1, nprocs * sizeof(view_state));
    client_comm_sz_arr = (ADIO_Offset *)
	ADIOI_Calloc (1, nprocs * sizeof(ADIO_Offset));

    if (fd->is_agg) {
	client_file_view_state_arr = (view_state *)
	    ADIOI_Calloc (1, nprocs * sizeof(view_state));
    }
    else {
	client_file_view_state_arr = NULL;
    }

    /* Alltoallw doesn't like a null array even if the counts are
     * zero.  If you do not include this code, it will fail. */
    client_comm_dtype_arr = (MPI_Datatype *)
	ADIOI_Calloc (1, nprocs * sizeof(MPI_Datatype));
    if (!fd->is_agg)
	for (i = 0; i < nprocs; i++)
	    client_comm_dtype_arr[i] = MPI_BYTE;

    ADIOI_Exch_file_views (myrank, nprocs, file_ptr_type, fd, count,
			   datatype, offset, my_mem_view_state_arr,
			   agg_file_view_state_arr,
			   client_file_view_state_arr);

    agg_comm_sz_arr = (ADIO_Offset *)
	ADIOI_Calloc (1, nprocs * sizeof(ADIO_Offset));
    agg_comm_dtype_arr = (MPI_Datatype *)
	ADIOI_Malloc (nprocs * sizeof(MPI_Datatype));
    if (fd->is_agg) {
	ADIOI_Build_agg_reqs (fd, rdwr, nprocs,
			      client_file_view_state_arr,
			      client_comm_dtype_arr,
			      client_comm_sz_arr,
			      &agg_disp,
			      &agg_dtype);
	buffered_io_size = 0;
	for (i=0; i <nprocs; i++) {
	    if (client_comm_sz_arr[i] > 0)
		buffered_io_size += client_comm_sz_arr[i];
	}
    }
#ifdef USE_PRE_REQ
    else 
    {
	/* Example use of ADIOI_Build_client_pre_req. to an
	 * appropriate section */
	
	for (i = 0; i < fd->hints->cb_nodes; i++)
	{
	    agg_rank = fd->hints->ranklist[(i+myrank)%fd->hints->cb_nodes];
#ifdef AGGREGATION_PROFILE
	    MPE_Log_event (5040, 0, NULL);
#endif
	    ADIOI_Build_client_pre_req(
		fd, agg_rank, (i+myrank)%fd->hints->cb_nodes,
		&(my_mem_view_state_arr[agg_rank]),
		&(agg_file_view_state_arr[agg_rank]),
		2*1024*1024, 
		64*1024);
#ifdef AGGREGATION_PROFILE
	    MPE_Log_event (5041, 0, NULL);
#endif
	}
    }
#endif


    if (fd->is_agg)
	cb_buf = (char *) ADIOI_Malloc (fd->hints->cb_buffer_size);
    alltoallw_disps  = (int *) ADIOI_Calloc (nprocs, sizeof(int));
    alltoallw_counts = client_alltoallw_counts = (int *)
	ADIOI_Calloc (2*nprocs, sizeof(int));
    agg_alltoallw_counts = &alltoallw_counts[nprocs];

    if (fd->hints->cb_alltoall == ADIOI_HINT_DISABLE) {
        /* aggregators pre-post all Irecv's for incoming data from clients */
        if ((fd->is_agg) && (rdwr == ADIOI_WRITE))
	    post_aggregator_comm(fd->comm, rdwr, nprocs, cb_buf,
			     client_comm_dtype_arr,
			     client_comm_sz_arr,
			     &agg_comm_requests,
			     &aggs_client_count);
    }
    /* Aggregators send amounts for data requested to clients */
    Exch_data_amounts (fd, nprocs, client_comm_sz_arr, agg_comm_sz_arr,
		       client_alltoallw_counts, agg_alltoallw_counts,
		       &aggregators_done);

#ifdef DEBUG
    fprintf (stderr, "client_alltoallw_counts[ ");
    for (i=0; i<nprocs; i++) {
	fprintf (stderr, "%d ", client_alltoallw_counts[i]);
    }
    fprintf (stderr, "]\n");
    fprintf (stderr, "agg_alltoallw_counts[ ");
    for (i=0; i<nprocs; i++) {
	fprintf (stderr,"%d ", agg_alltoallw_counts[i]);
    }
    fprintf (stderr, "]\n");
#endif

    /* keep looping while aggregators still have I/O to do */
    while (aggregators_done != nprocs_for_coll) {
	if (fd->hints->cb_alltoall == ADIOI_HINT_DISABLE) {
	/* clients should build datatypes for local memory locations
	   for data communication with aggregators and post
	   communication as the datatypes are built */

	client_comm_requests = (MPI_Request *)
	    ADIOI_Calloc (fd->hints->cb_nodes, sizeof(MPI_Request));

	for (i = 0; i < fd->hints->cb_nodes; i++)
	{
	    clients_agg_count = 0;
	    agg_rank = fd->hints->ranklist[(i+myrank)%fd->hints->cb_nodes];
	    if (agg_comm_sz_arr[agg_rank] > 0) {
	        ADIOI_Build_client_req(fd, agg_rank,
				       (i+myrank)%fd->hints->cb_nodes,
				       &(my_mem_view_state_arr[agg_rank]),
				       &(agg_file_view_state_arr[agg_rank]),
				       agg_comm_sz_arr[agg_rank], 
				       &(agg_comm_dtype_arr[agg_rank]));

#ifdef AGGREGATION_PROFILE
		if (i == 0)
		    MPE_Log_event (5038, 0, NULL);
#endif
		post_client_comm (fd, rdwr, agg_rank, buf,
				  agg_comm_dtype_arr[agg_rank],
				  agg_alltoallw_counts[agg_rank],
				  &client_comm_requests[clients_agg_count]);
		clients_agg_count++;
	    }
	}
#ifdef AGGREGATION_PROFILE
	if (!clients_agg_count)
	    MPE_Log_event(5039, 0, NULL);
#endif

	if (rdwr == ADIOI_READ) {
	    if (fd->is_agg && buffered_io_size) {
		ADIOI_IOFiletype (fd, cb_buf, buffered_io_size, MPI_BYTE,
				  ADIO_EXPLICIT_OFFSET, agg_disp, agg_dtype,
				  ADIOI_READ, status, error_code);
		if (*error_code != MPI_SUCCESS) return;
		MPI_Type_free (&agg_dtype);
	    }

#ifdef DEBUG
	    fprintf (stderr, "expecting from [agg](disp,size,cnt)=");
	    for (i=0; i < nprocs; i++) {
		MPI_Type_size (agg_comm_dtype_arr[i], &size);
		fprintf (stderr, "[%d](%d,%d,%d)", i, alltoallw_disps[i], 
			 size, agg_alltoallw_counts[i]);
		if (i != nprocs - 1)
		    fprintf(stderr, ",");
	    }
	    fprintf (stderr, "]\n");
	    if (fd->is_agg) {
		fprintf (stderr, "sending to [client](disp,size,cnt)=");
		for (i=0; i < nprocs; i++) {
		    if (fd->is_agg)
			MPI_Type_size (client_comm_dtype_arr[i], &size);
		    else
			size = -1;
		    
		    fprintf (stderr, "[%d](%d,%d,%d)", i, alltoallw_disps[i], 
			     size, client_alltoallw_counts[i]);
		    if (i != nprocs - 1)
			fprintf(stderr, ",");
		}
		fprintf (stderr,"\n");
	    }
	    fflush (NULL);
#endif
	    /* aggregators post all Isends for outgoing data to clients */
	    if (fd->is_agg)
		post_aggregator_comm(fd->comm, rdwr, nprocs, cb_buf,
				     client_comm_dtype_arr,
				     client_comm_sz_arr,
				     &agg_comm_requests,
				     &aggs_client_count);

	    if (fd->is_agg && aggs_client_count) {
		agg_comm_statuses = ADIOI_Malloc(aggs_client_count *
						 sizeof(MPI_Status));
		MPI_Waitall(aggs_client_count, agg_comm_requests,
			    agg_comm_statuses);
#ifdef AGGREGATION_PROFILE
		MPE_Log_event (5033, 0, NULL);
#endif
		ADIOI_Free (agg_comm_requests);
		ADIOI_Free (agg_comm_statuses);
	    }

	    if (clients_agg_count) {
		client_comm_statuses = ADIOI_Malloc(clients_agg_count *
						    sizeof(MPI_Status));
		MPI_Waitall(clients_agg_count, client_comm_requests,
			    client_comm_statuses);
#ifdef AGGREGATION_PROFILE
		MPE_Log_event (5039, 0, NULL);
#endif
		ADIOI_Free (client_comm_requests);
		ADIOI_Free (client_comm_statuses);
	    }

#ifdef DEBUG2
	    fprintf (stderr, "buffered_io_size = %lld\n", buffered_io_size);
	    if (fd->is_agg && buffered_io_size) {
		fprintf (stderr, "buf = [");
		for (i=0; i<bufextent; i++)
		    fprintf (stderr, "%c", ((char *) buf)[i]);
		fprintf (stderr, "]\n");
		fprintf (stderr, "cb_buf = [");
		for (i=0; i<buffered_io_size; i++)
		    fprintf (stderr, "%c", cb_buf[i]);
		fprintf (stderr, "]\n");
		fflush (NULL);
	    }
#endif
	}
	else { /* Write Case */
#ifdef DEBUG
	    fprintf (stderr, "sending to [agg](disp,size,cnt)=");
	    for (i=0; i < nprocs; i++) {
		MPI_Type_size (agg_comm_dtype_arr[i], &size);
		fprintf (stderr, "[%d](%d,%d,%d)", i, alltoallw_disps[i], 
			 size, agg_alltoallw_counts[i]);
		if (i != nprocs - 1)
		    fprintf(stderr, ",");
	    }
	    fprintf (stderr, "]\n");
	    fprintf (stderr, "expecting from [client](disp,size,cnt)=");
	    for (i=0; i < nprocs; i++) {
		if (fd->is_agg)
		    MPI_Type_size (client_comm_dtype_arr[i], &size);
		else
		    size = -1;
		
		fprintf (stderr, "[%d](%d,%d,%d)", i, alltoallw_disps[i], 
			 size, client_alltoallw_counts[i]);
		if (i != nprocs - 1)
		    fprintf(stderr, ",");
	    }
	    fprintf (stderr,"\n");
	    fflush (NULL);
#endif
#ifdef DEBUG
	    fprintf (stderr, "buffered_io_size = %lld\n", buffered_io_size);
#endif
	    
	    if (clients_agg_count) {
		client_comm_statuses = ADIOI_Malloc(clients_agg_count *
						    sizeof(MPI_Status));
		MPI_Waitall(clients_agg_count, client_comm_requests,
			    client_comm_statuses);
#ifdef AGGREGATION_PROFILE
		MPE_Log_event (5039, 0, NULL);
#endif
		ADIOI_Free(client_comm_requests);
		ADIOI_Free(client_comm_statuses);
	    }
#ifdef DEBUG2
	    if (bufextent) {
		fprintf (stderr, "buf = [");
		for (i=0; i<bufextent; i++)
		    fprintf (stderr, "%c", ((char *) buf)[i]);
		fprintf (stderr, "]\n");
	    }
#endif

	    if (fd->is_agg && buffered_io_size) {
		assert (aggs_client_count != 0);
		/* make sure we actually have the data to write out */
		agg_comm_statuses = (MPI_Status *)
		    ADIOI_Malloc (aggs_client_count*sizeof(MPI_Status));
		
		MPI_Waitall (aggs_client_count, agg_comm_requests,
			     agg_comm_statuses);
#ifdef AGGREGATION_PROFILE
		MPE_Log_event (5033, 0, NULL);
#endif
		ADIOI_Free (agg_comm_requests);
		ADIOI_Free (agg_comm_statuses);
#ifdef DEBUG2
		fprintf (stderr, "cb_buf = [");
		for (i=0; i<buffered_io_size; i++)
		    fprintf (stderr, "%c", cb_buf[i]);
		fprintf (stderr, "]\n");
		fflush (NULL);
#endif
		ADIOI_IOFiletype (fd, cb_buf, buffered_io_size, MPI_BYTE,
				  ADIO_EXPLICIT_OFFSET, agg_disp, agg_dtype,
				  ADIOI_WRITE, status, error_code);
		if (*error_code != MPI_SUCCESS) return;
		MPI_Type_free (&agg_dtype);
	    }

	}
	} else {
	/* Alltoallw version of everything */
	ADIOI_Build_client_reqs(fd, nprocs, my_mem_view_state_arr,
				agg_file_view_state_arr,
				agg_comm_sz_arr, agg_comm_dtype_arr);

	if (rdwr == ADIOI_READ) {
	    if (fd->is_agg && buffered_io_size) {
		ADIOI_IOFiletype (fd, cb_buf, buffered_io_size, MPI_BYTE,
				  ADIO_EXPLICIT_OFFSET, agg_disp, agg_dtype,
				  ADIOI_READ, status, error_code);
		if (*error_code != MPI_SUCCESS) return;
		MPI_Type_free (&agg_dtype);
	    }

#ifdef AGGREGATION_PROFILE
	    MPE_Log_event (5032, 0, NULL);
#endif
	    MPI_Alltoallw (cb_buf, client_alltoallw_counts, alltoallw_disps,
			   client_comm_dtype_arr,
			   buf, agg_alltoallw_counts , alltoallw_disps,
			   agg_comm_dtype_arr,
			   fd->comm);
#ifdef AGGREGATION_PROFILE
	    MPE_Log_event (5033, 0, NULL);
#endif
	}
	else { /* Write Case */
#ifdef AGGREGATION_PROFILE
	    MPE_Log_event (5032, 0, NULL);
#endif
	    MPI_Alltoallw (buf, agg_alltoallw_counts, alltoallw_disps,
			   agg_comm_dtype_arr,
			   cb_buf, client_alltoallw_counts, alltoallw_disps,
			   client_comm_dtype_arr,
			   fd->comm);
#ifdef AGGREGATION_PROFILE
	    MPE_Log_event (5033, 0, NULL);
#endif
	    if (fd->is_agg && buffered_io_size) {
		ADIOI_IOFiletype (fd, cb_buf, buffered_io_size, MPI_BYTE,
				  ADIO_EXPLICIT_OFFSET, agg_disp, agg_dtype,
				  ADIOI_WRITE, status, error_code);
		if (*error_code != MPI_SUCCESS) return;
		MPI_Type_free (&agg_dtype);
	    }
	}
	}

	/* Free (uncommit) datatypes for reuse */
	if (fd->is_agg) {
	    if (buffered_io_size > 0) {
		for (i=0; i<nprocs; i++) {
		    if (client_comm_sz_arr[i] > 0)
			MPI_Type_free (&client_comm_dtype_arr[i]);
		}
	    }
	}
	for (i=0; i<nprocs; i++) {
	    if (agg_comm_sz_arr[i] > 0)
		MPI_Type_free (&agg_comm_dtype_arr[i]);
	}

	/* figure out next set up requests */
	if (fd->is_agg) {
	    ADIOI_Build_agg_reqs (fd, rdwr, nprocs,
				  client_file_view_state_arr,
				  client_comm_dtype_arr,
				  client_comm_sz_arr,
				  &agg_disp,
				  &agg_dtype);
	    buffered_io_size = 0;
	    for (i=0; i <nprocs; i++) {
		if (client_comm_sz_arr[i] > 0)
		    buffered_io_size += client_comm_sz_arr[i];
	    }
	}
#ifdef USE_PRE_REQ
	else {
	    /* Example use of ADIOI_Build_client_pre_req. to an
	     * appropriate section */
	    for (i = 0; i < fd->hints->cb_nodes; i++)
	    {
		agg_rank = fd->hints->ranklist[(i+myrank)%fd->hints->cb_nodes];
#ifdef AGGREGATION_PROFILE
		MPE_Log_event (5040, 0, NULL);
#endif
		ADIOI_Build_client_pre_req(
		    fd, agg_rank, (i+myrank)%fd->hints->cb_nodes,
		    &(my_mem_view_state_arr[agg_rank]),
		    &(agg_file_view_state_arr[agg_rank]),
		    2*1024*1024, 
		    64*1024);
#ifdef AGGREGATION_PROFILE
		MPE_Log_event (5041, 0, NULL);
#endif
	    }
	}
#endif
	
	/* aggregators pre-post all Irecv's for incoming data from
	 * clients.  if nothing is needed, agg_comm_requests is not
	 * allocated */
	if (fd->hints->cb_alltoall == ADIOI_HINT_DISABLE) {
	    if ((fd->is_agg) && (rdwr == ADIOI_WRITE))
	        post_aggregator_comm(fd->comm, rdwr, nprocs, cb_buf,
				 client_comm_dtype_arr,
				 client_comm_sz_arr,
				 &agg_comm_requests,
				 &aggs_client_count);
	}

	/* Aggregators send amounts for data requested to clients */
	Exch_data_amounts (fd, nprocs, client_comm_sz_arr, agg_comm_sz_arr,
			   client_alltoallw_counts, agg_alltoallw_counts,
			   &aggregators_done);

    }

    /* Clean up */
	
    if (fd->hints->cb_pfr != ADIOI_HINT_ENABLE) {
	/* AAR, FSIZE, and User provided uniform File realms */
	if (1) {
	    ADIOI_Delete_flattened (fd->file_realm_types[0]);
	    MPI_Type_free (&fd->file_realm_types[0]);
	}
	else {
	    for (i=0; i<fd->hints->cb_nodes; i++) {
		ADIOI_Datatype_iscontig(fd->file_realm_types[i], &is_contig);
		if (!is_contig)
		    ADIOI_Delete_flattened(fd->file_realm_types[i]);
		MPI_Type_free (&fd->file_realm_types[i]);
	    }
	}
	ADIOI_Free (fd->file_realm_types);
	ADIOI_Free (fd->file_realm_st_offs);
    }

    /* This memtype must be deleted from the ADIOI_Flatlist or else it
     * will match incorrectly with other datatypes which use this
     * pointer. */
    ADIOI_Delete_flattened(datatype);
    ADIOI_Delete_flattened(fd->filetype);

    if (fd->is_agg) {
	if (buffered_io_size > 0)
	    MPI_Type_free (&agg_dtype);
	for (i=0; i<nprocs; i++) {
	    MPI_Type_free (&client_comm_dtype_arr[i]);
	    ADIOI_Free (client_file_view_state_arr[i].flat_type_p->indices);
	    ADIOI_Free (client_file_view_state_arr[i].flat_type_p->blocklens);
	    ADIOI_Free (client_file_view_state_arr[i].flat_type_p);
	}
	ADIOI_Free (client_file_view_state_arr);
	ADIOI_Free (cb_buf);
    } 
    for (i = 0; i<nprocs; i++)
	if (agg_comm_sz_arr[i] > 0)
	    MPI_Type_free (&agg_comm_dtype_arr[i]);
    
    ADIOI_Free (client_comm_sz_arr);
    ADIOI_Free (client_comm_dtype_arr);
    ADIOI_Free (my_mem_view_state_arr);
    ADIOI_Free (agg_file_view_state_arr);
    ADIOI_Free (agg_comm_sz_arr);
    ADIOI_Free (agg_comm_dtype_arr);
    ADIOI_Free (alltoallw_disps);
    ADIOI_Free (alltoallw_counts);
    ADIOI_Free (all_st_end_offsets);

#ifdef HAVE_STATUS_SET_BYTES
    MPIR_Status_set_bytes(status, datatype, bufsize);
    /* This is a temporary way of filling in status.  The right way is
     * to keep track of how much data was actually read and placed in
     * buf during collective I/O. */
#endif
    fd->fp_sys_posn = -1; /* set it to null. */
#ifdef AGGREGATION_PROFILE
    if (rdwr == ADIOI_READ)
	MPE_Log_event (5011, 0, NULL);
    else
	MPE_Log_event (5013, 0, NULL);
#endif
}


/* Some of this code is from the old Calc_my_off_len() function.
 * It calculates the 1st and last byte accessed */
void ADIOI_Calc_bounds (ADIO_File fd, int count, MPI_Datatype buftype,
			int file_ptr_type, ADIO_Offset offset,
			ADIO_Offset *st_offset, ADIO_Offset *end_offset)
{
    int filetype_size, buftype_size, etype_size;
    int i, sum;
    MPI_Aint filetype_extent;
    ADIO_Offset total_io;
    int filetype_is_contig;
    int remainder;
    ADIOI_Flatlist_node *flat_file;
    
    ADIO_Offset st_byte_off, end_byte_off;
    
#ifdef AGGREGATION_PROFILE
    MPE_Log_event (5000, 0, NULL);
#endif

    if (!count) {
	/* Max signed positive value for ADIO_Offset
	 * (arch. dependent?).  is there a better way? */
	memset (st_offset, 8, sizeof(ADIO_Offset));
	*st_offset = *st_offset / 2;
	*end_offset = -1;
	return;
    }

    ADIOI_Datatype_iscontig (fd->filetype, &filetype_is_contig);
    
    MPI_Type_size (fd->filetype, &filetype_size);
    MPI_Type_extent (fd->filetype, &filetype_extent);
    MPI_Type_size (fd->etype, &etype_size);
    MPI_Type_size (buftype, &buftype_size);
    
    total_io = buftype_size * count;

    if (filetype_is_contig) {
	if (file_ptr_type == ADIO_INDIVIDUAL)
	    st_byte_off = fd->fp_ind;
	else
	    st_byte_off = fd->disp + etype_size * offset;

	end_byte_off = st_byte_off + total_io - 1;
    }
    else {
	flat_file = ADIOI_Flatlist;
	while (flat_file->type != fd->filetype) flat_file = flat_file->next;

	/* we need to take care of some weirdness since fd->fp_ind
	   points at an accessible byte in file.  the first accessible
	   byte in the file is not necessarily the first byte, nor is
	   it necessarily the first off/len pair in the filetype. */
	if (file_ptr_type == ADIO_INDIVIDUAL) {	
	    st_byte_off = fd->fp_ind;
	    /* find end byte of I/O (may be in middle of an etype) */

	    /* calculate byte starting point of first filetype */
	    end_byte_off = (ADIO_Offset)
		((fd->fp_ind - fd->disp - flat_file->indices[0]) /
		 filetype_extent) * filetype_extent + fd->disp +
		flat_file->indices[0];
	    /* number of absolute bytes into first filetype */
	    remainder = (fd->fp_ind - fd->disp - flat_file->indices[0]) %
		filetype_extent;
	    if (remainder) {
		/* find how many file viewable bytes into first filetype */
		sum = 0;
		for (i=0; i<flat_file->count; i++) {
		    sum += flat_file->blocklens[i];
		    if ((flat_file->indices[i] - flat_file->indices[0] +
			 flat_file->blocklens[i]) >= remainder) {
			sum -= (flat_file->blocklens[i] - (sum - remainder));
			break;
		    }
		}
		total_io += sum;
	    }
	    /* byte starting point of last filetype */
	    end_byte_off += (total_io - 1) / filetype_size * filetype_extent;
	    /* number of bytes into last filetype */
	    remainder = total_io % filetype_size;
	    if (!remainder) {
		for (i=flat_file->count - 1; i>=0; i--) {
		    if (flat_file->blocklens[i]) break;
		}
		assert (i > -1);
		end_byte_off += flat_file->indices[i] +
		    flat_file->blocklens[i] - 1;
		end_byte_off -= flat_file->indices[0];
	    }
	    else {
		sum = 0;
		for (i=0; i<flat_file->count; i++) {
		    sum += flat_file->blocklens[i];
		    if (sum >= remainder) {
			end_byte_off += flat_file->indices[i] + 
			    flat_file->blocklens[i] - sum + remainder - 1;
			break;
		    }
		}
		end_byte_off -= flat_file->indices[0];
	    }
	}
	else {
	    /* find starting byte of I/O (must be aligned with an etype) */
	    /* byte starting point of starting filetype */		    
	    st_byte_off = fd->disp + ((offset * etype_size) / filetype_size) *
		filetype_extent;
	    /* number of file viewable bytes into starting filetype */
	    remainder = (etype_size * offset) % filetype_size;
	    
	    sum = 0;
	    for (i=0; i<flat_file->count; i++) {
		sum += flat_file->blocklens[i];
		if (sum >= remainder) {
		    if (sum == remainder)
			st_byte_off += flat_file->indices[i+1];
		    else
			st_byte_off += flat_file->indices[i] +
			    flat_file->blocklens[i] - sum + remainder;
		    break;
		}
	    }
	    
	    /* find end byte of I/O (may be in middle of an etype) */
	    /* byte starting point of last filetype */
	    end_byte_off = fd->disp + (offset * etype_size + total_io) /
		filetype_size * filetype_extent;
	    /* number of bytes into last filetype */
	    remainder = (offset * etype_size + total_io) % filetype_size;
	    
	    if (!remainder) {
		/* the last non-zero off/len pair */
		for (i=flat_file->count-1; i>=0; i--) {
		    if (flat_file->blocklens[i]) break;
		}
		assert (i >= 0);
		/* back up a whole filetype, and put back up to the
		 * last byte of the last non-zero offlen pair */
		/* end_byte_off = (end_byte_off - filetype_extent) +
		    flat_file->indices[i] +
		    flat_file->blocklens[i] - 1; */
		/* equivalent of above commented out equation */
		end_byte_off -= filetype_extent - flat_file->indices[i] -
		    flat_file->blocklens[i] + 1;
	    }
	    else {
		sum = 0;
		for (i=0; i<flat_file->count; i++) {
		    sum += flat_file->blocklens[i];
		    if (sum >= remainder) {
			end_byte_off += flat_file->indices[i] + 
			    flat_file->blocklens[i] - sum + remainder - 1;
			break;
		    }
		}
	    }
	}
    }
    
    *st_offset  = st_byte_off;
    *end_offset = end_byte_off;
#ifdef DEBUG
    printf ("st_offset = %lld\nend_offset = %lld\n",
	    st_byte_off, end_byte_off);
#endif
#ifdef AGGREGATION_PROFILE
    MPE_Log_event (5001, 0, NULL);
#endif
}

/* wrapper function for ADIO_WriteStrided and ADIO_ReadStrided.  Used
 * by new 2 phase code to pass an arbitrary file type directly to
 * WriteStrided call without affecting existing code.  For the new 2
 * phase code, we really only need to set a custom_ftype, and we can
 * assume that this uses MPI_BYTE for the etype, and disp is 0 */
void ADIOI_IOFiletype(ADIO_File fd, void *buf, int count,
		      MPI_Datatype datatype, int file_ptr_type,
		      ADIO_Offset offset, MPI_Datatype custom_ftype, 
		      int rdwr, ADIO_Status *status, int *error_code)
{
    MPI_Datatype user_filetype;
    MPI_Datatype user_etype;
    ADIO_Offset user_disp;
    int user_ind_wr_buffer_size;
    int user_ind_rd_buffer_size;
    int f_is_contig, m_is_contig;
    int user_ds_read, user_ds_write;
    MPI_Aint f_extent;
    int f_size;
    int f_ds_percent; /* size/extent */

#ifdef AGGREGATION_PROFILE
    if (rdwr == ADIOI_READ)
	MPE_Log_event(5006, 0, NULL);
    else
	MPE_Log_event(5008, 0, NULL);
#endif
    MPI_Type_extent(custom_ftype, &f_extent);
    MPI_Type_size(custom_ftype, &f_size);
    f_ds_percent = 100 * f_size / f_extent;

    /* temporarily store file view information */
    user_filetype           = fd->filetype;
    user_etype              = fd->etype;
    user_disp               = fd->disp;
    user_ds_read            = fd->hints->ds_read;
    user_ds_write           = fd->hints->ds_write;
    /* temporarily override the independent I/O datasieve buffer size */
    user_ind_wr_buffer_size = fd->hints->ind_wr_buffer_size;
    user_ind_rd_buffer_size = fd->hints->ind_rd_buffer_size;

    /* set new values for temporary file view */
    fd->filetype = custom_ftype;
    fd->etype    = MPI_BYTE;
    /* set new values for independent I/O datasieve buffer size */
    fd->hints->ind_wr_buffer_size = fd->hints->cb_buffer_size;
    fd->hints->ind_rd_buffer_size = fd->hints->cb_buffer_size;
    /* decide whether or not to do datasieving */
#ifdef DEBUG
    printf ("f_ds_percent = %d cb_ds_threshold = %d\n", f_ds_percent,
	    fd->hints->cb_ds_threshold);
#endif
    if (f_ds_percent >= fd->hints->cb_ds_threshold) {
	fd->hints->ds_read = ADIOI_HINT_ENABLE;
	fd->hints->ds_write = ADIOI_HINT_ENABLE;
    }
    else {
	fd->hints->ds_read = ADIOI_HINT_DISABLE;
	fd->hints->ds_write = ADIOI_HINT_DISABLE;
    }

    /* flatten the new filetype since the strided calls expect it to
     * have been flattened in set file view.  in the two phase code,
     * the datatype passed down should always be MPI_BYTE, and
     * therefore contiguous, but just for completeness sake, we'll
     * check the memory datatype anyway */
    ADIOI_Datatype_iscontig(custom_ftype, &f_is_contig);
    ADIOI_Datatype_iscontig(datatype, &m_is_contig);
    if (!f_is_contig)
	ADIOI_Flatten_datatype (custom_ftype);

    /* make appropriate Read/Write calls.  Let ROMIO figure out file
     * system specific stuff. */
    if (f_is_contig && m_is_contig) {
	fd->disp = 0;
	if (rdwr == ADIOI_READ)
	    ADIO_ReadContig(fd, buf, count, datatype, file_ptr_type, offset,
			    status, error_code);
	else
	    ADIO_WriteContig(fd, buf, count, datatype, file_ptr_type, offset,
			     status, error_code);
    }
    else {
	fd->disp = offset;
	if (rdwr == ADIOI_READ)
	    ADIO_ReadStrided(fd, buf, count, datatype, file_ptr_type, 0,
			     status, error_code);
	else
	    ADIO_WriteStrided(fd, buf, count, datatype, file_ptr_type, 0,
			      status, error_code);
    }

    /* Delete flattened temporary filetype */
    if (!f_is_contig)
	ADIOI_Delete_flattened (custom_ftype);

    /* restore the user specified file view to cover our tracks */
    fd->filetype                  = user_filetype;
    fd->etype                     = user_etype;
    fd->disp                      = user_disp;
    fd->hints->ds_read            = user_ds_read;
    fd->hints->ds_write           = user_ds_write;
    fd->hints->ind_wr_buffer_size = user_ind_wr_buffer_size;
    fd->hints->ind_rd_buffer_size = user_ind_rd_buffer_size;
#ifdef AGGREGATION_PROFILE
    if (rdwr == ADIOI_READ)
	MPE_Log_event (5007, 0, NULL);
    else
	MPE_Log_event (5009, 0, NULL);
#endif
}

static void Exch_data_amounts (ADIO_File fd, int nprocs,
			ADIO_Offset *client_comm_sz_arr,
			ADIO_Offset *agg_comm_sz_arr,
			int *client_alltoallw_counts,
			int *agg_alltoallw_counts,
			int *aggregators_done)
{
    int i;
    int recv_idx;
    MPI_Request *recv_requests;
    MPI_Request *send_requests;
    MPI_Status status;
    MPI_Status *send_statuses;
    /* Aggregators send amounts for data requested to clients */
    if (fd->hints->cb_alltoall != ADIOI_HINT_DISABLE) {
        MPI_Alltoall (client_comm_sz_arr, sizeof(ADIO_Offset), MPI_BYTE,
		  agg_comm_sz_arr, sizeof(ADIO_Offset), MPI_BYTE,
		  fd->comm);

        if (fd->is_agg) {
	    for (i=0; i<nprocs; i++)
	        if (client_comm_sz_arr[i] > 0)
		    client_alltoallw_counts[i] = 1;
	        else
		    client_alltoallw_counts[i] = 0;
        }
        *aggregators_done = 0;
        for (i=0; i<nprocs; i++) {
	    if (agg_comm_sz_arr[i] == -1)
	        *aggregators_done = *aggregators_done + 1;
	    else if (agg_comm_sz_arr[i] > 0)
	        agg_alltoallw_counts[i] = 1;
	    else
	        agg_alltoallw_counts[i] = 0;
        }
    } else {
        /* let's see if we can't reduce some communication as well as
         * overlap some communication and work */

        recv_requests = ADIOI_Malloc (fd->hints->cb_nodes * sizeof(MPI_Request));
        /* post all receives - only receive from aggregators */
        for (i = 0; i < fd->hints->cb_nodes; i++)
	    MPI_Irecv (&agg_comm_sz_arr[fd->hints->ranklist[i]],
		   sizeof(ADIO_Offset), MPI_BYTE, fd->hints->ranklist[i],
		   AMT_TAG, fd->comm, &recv_requests[i]);

        /* Barrier is needed here if we're worried about unexpected
         * messages being dropped */
        /* MPI_Barrier (fd->comm); */
        send_requests = NULL;
        if (fd->is_agg) {
	    /* only aggregators send data */
	    send_requests = ADIOI_Malloc (nprocs * sizeof(MPI_Request)); 

	    /* post all sends */
	    for (i = 0; i < nprocs; i++) {
	        MPI_Isend (&client_comm_sz_arr[i], sizeof(ADIO_Offset),
		       MPI_BYTE, i, AMT_TAG, fd->comm, &send_requests[i]);

	        if (client_comm_sz_arr[i] > 0)
		    client_alltoallw_counts[i] = 1;
	        else
		    client_alltoallw_counts[i] = 0;
	    }
        }

        *aggregators_done = 0;
        for (i=0; i < fd->hints->cb_nodes; i++) {
	    MPI_Waitany (fd->hints->cb_nodes, recv_requests, &recv_idx, &status);
	    if (agg_comm_sz_arr[fd->hints->ranklist[recv_idx]] == -1)
	        *aggregators_done = *aggregators_done + 1;
	    else if (agg_comm_sz_arr[fd->hints->ranklist[recv_idx]] > 0)
	        agg_alltoallw_counts[fd->hints->ranklist[recv_idx]] = 1;
	    else
	        agg_alltoallw_counts[fd->hints->ranklist[recv_idx]] = 0;
        }

        ADIOI_Free (recv_requests);
        if (fd->is_agg) {
	    /* wait for all sends to complete */
	    send_statuses = ADIOI_Malloc (nprocs * sizeof (MPI_Status));
	    MPI_Waitall (nprocs, send_requests, send_statuses);
	    ADIOI_Free (send_requests);
	    ADIOI_Free (send_statuses);
        }
    }
}

static void post_aggregator_comm (MPI_Comm comm, int rw_type, 
		           int nproc, void *cb_buf,
			   MPI_Datatype *client_comm_dtype_arr,
			   ADIO_Offset *client_comm_sz_arr,
			   MPI_Request **requests_p,
			   int *aggs_client_count_p)
{
    int aggs_client_count = 0;
    MPI_Request *requests;
    int i;

#ifdef DEBUG
    printf ("posting aggregator communication\n");
#endif

    for (i=0; i < nproc; i++)
	if (client_comm_sz_arr[i] > 0)
	    aggs_client_count++;
#ifdef DEBUG
    printf ("aggregator needs to talk to %d clients\n",
	aggs_client_count);
#endif
    *aggs_client_count_p = aggs_client_count;
    if (aggs_client_count) {
	requests = (MPI_Request *)
	    ADIOI_Malloc (aggs_client_count * sizeof(MPI_Request));
	aggs_client_count = 0;
#ifdef AGGREGATION_PROFILE
	MPE_Log_event (5032, 0, NULL);
#endif
	for (i=0; i < nproc; i++) {
	    if (client_comm_sz_arr[i] > 0) {
		if (rw_type == ADIOI_WRITE)
		    MPI_Irecv (cb_buf, 1, client_comm_dtype_arr[i], i,
			       DATA_TAG, comm,
			       &requests[aggs_client_count]);
		else
		    MPI_Isend (cb_buf, 1, client_comm_dtype_arr[i], i,
			       DATA_TAG, comm,
			       &requests[aggs_client_count]);

		aggs_client_count++;
	    }
	}
	*requests_p = requests;
    }
}

static void post_client_comm (ADIO_File fd, int rw_type, 
		       int agg_rank, void *buf,
		       MPI_Datatype agg_comm_dtype,
		       int agg_alltoallw_count,
		       MPI_Request *request)
{
    if (agg_alltoallw_count) {
	if (rw_type == ADIOI_READ)
	    MPI_Irecv (buf, 1, agg_comm_dtype, agg_rank, DATA_TAG, fd->comm,
		       request);
	else
	    MPI_Isend (buf, 1, agg_comm_dtype, agg_rank, DATA_TAG, fd->comm,
		       request);
    }
}



