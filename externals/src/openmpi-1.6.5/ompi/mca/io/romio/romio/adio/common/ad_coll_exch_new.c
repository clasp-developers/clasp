/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include <assert.h>
#include "adio.h"
#include "adio_extern.h"
#ifdef AGGREGATION_PROFILE
#include "mpe.h"
#endif

/*
#define DEBUG
#define DEBUG2
*/

#define COUNT_EXCH          0
#define BLOCK_LENS          1
#define INDICES             2
#define FPIND_DISP_OFF_SZ   3


typedef struct {
    int count;
    ADIO_Offset fp_ind;
    ADIO_Offset disp;
    ADIO_Offset byte_off;
    ADIO_Offset sz;
    ADIO_Offset ext;
    ADIO_Offset type_sz;
} amount_and_extra_data_t;

/* Debugging function to print out an ADIOI_Flatlist_node. */
void ADIOI_Print_flatlist_node(ADIOI_Flatlist_node *flatlist_node_p)
{
    int i;
    if (flatlist_node_p == NULL)
    {
	fprintf(stderr, "print flatlist node of NULL ptr\n");
	return;
    }
    fprintf(stderr, "print flatlist node count = %d (idx,blocklen)\n", 
	    flatlist_node_p->count);
    for (i = 0; i < flatlist_node_p->count; i++)
    {
	if (i % 5 == 0 && i != 0)
	{
	    fprintf(stderr, "%d=(%Ld,%Ld)\n", i, flatlist_node_p->indices[i],
		    flatlist_node_p->blocklens[i]);
	}
	else
	    fprintf(stderr, "%d=(%Ld,%Ld) ", i, flatlist_node_p->indices[i],
		    flatlist_node_p->blocklens[i]);
    }
    fprintf(stderr, "\n");
}

/* Since ADIOI_Flatten_datatype won't add a contig datatype to the
 * ADIOI_Flatlist, we can force it to do so with this function. */
ADIOI_Flatlist_node * ADIOI_Add_contig_flattened(MPI_Datatype contig_type)
{
    int contig_type_sz = -1;
    ADIOI_Flatlist_node *flat_node_p = ADIOI_Flatlist;
    
    /* Add contig type to the end of the list if it doesn't already
     * exist. */
    while (flat_node_p->next)
    {
	if (flat_node_p->type == contig_type)
	    return flat_node_p;
	flat_node_p = flat_node_p->next;
    }
    if (flat_node_p->type == contig_type)
	return flat_node_p;

    MPI_Type_size(contig_type, &contig_type_sz);
    if ((flat_node_p->next = (ADIOI_Flatlist_node *) ADIOI_Malloc
	 (sizeof(ADIOI_Flatlist_node))) == NULL)
    {
	fprintf(stderr, "ADIOI_Add_contig_flattened: malloc next failed\n");
    }
    flat_node_p = flat_node_p->next;
    flat_node_p->type = contig_type;
    if ((flat_node_p->blocklens = (ADIO_Offset *) ADIOI_Malloc(sizeof(ADIO_Offset))) == NULL)
    {
	fprintf(stderr, "ADIOI_Flatlist_node: malloc blocklens failed\n");
    }
    if ((flat_node_p->indices = (ADIO_Offset *) 
	 ADIOI_Malloc(sizeof(ADIO_Offset))) == NULL)
    {
	fprintf(stderr, "ADIOI_Flatlist_node: malloc indices failed\n");
    }
    flat_node_p->blocklens[0] = contig_type_sz;
    flat_node_p->indices[0] = 0;
    flat_node_p->count = 1;
    flat_node_p->next = NULL;
    return flat_node_p;
}

/* ADIOI_Exchange_file_views - Sends all the aggregators the file
 * views and file view states of the clients.  It fills in the
 * client_file_view_state_arr for the aggregators and the
 * my_mem_view_state for the client.  It also initializes the
 * agg_file_view_state for all clients, which is the view for each
 * aggregator of a client's filetype. */
void ADIOI_Exch_file_views(int myrank, int nprocs, int file_ptr_type,
			   ADIO_File fd, int count,
			   MPI_Datatype datatype, ADIO_Offset off,
			   view_state *my_mem_view_state_arr,
			   view_state *agg_file_view_state_arr,
			   view_state *client_file_view_state_arr)
{
    /* Convert my own fileview to an ADIOI_Flattened type and a
     * disp. MPI_Alltoall the count of ADIOI_Flatlist nodes.
     * MPI_Isend/Irecv the block_lens, indices of ADIOI_Flatlist node
     * to/from each of the aggregators with the rest of the file view
     * state. */

    int i = -1, j = -1;
    amount_and_extra_data_t *send_count_arr = NULL;
    amount_and_extra_data_t *recv_count_arr = NULL;
    int send_req_arr_sz = 0;
    int recv_req_arr_sz = 0;
    MPI_Request *send_req_arr = NULL, *recv_req_arr = NULL;
    MPI_Status *statuses = NULL;
    ADIO_Offset disp_off_sz_ext_typesz[6];
    MPI_Aint memtype_extent, filetype_extent;
    int ret = -1;

    /* parameters for datatypes */
    ADIOI_Flatlist_node *flat_mem_p = NULL, *flat_file_p = NULL;
    int memtype_sz = -1;
    int memtype_is_contig = -1, filetype_is_contig = -1;
    int filetype_sz = -1;

#ifdef AGGREGATION_PROFILE
    MPE_Log_event (5014, 0, NULL);
#endif
    /* The memtype will be freed after the call.  The filetype will be
     * freed in the close and should have been flattened in the file
     * view. */
    MPI_Type_size(datatype, &memtype_sz);
    MPI_Type_extent(datatype, &memtype_extent);
    if (memtype_sz == memtype_extent) {
	memtype_is_contig = 1;
	flat_mem_p = ADIOI_Add_contig_flattened(datatype);
	flat_mem_p->blocklens[0] = memtype_sz*count;
    }
    else {
	ADIOI_Flatten_datatype(datatype);
        flat_mem_p = ADIOI_Flatlist;
        while (flat_mem_p->type != datatype)
            flat_mem_p = flat_mem_p->next;
    }

    MPI_Type_extent(fd->filetype, &filetype_extent);
    MPI_Type_size(fd->filetype, &filetype_sz);
    if (filetype_extent == filetype_sz) {
	filetype_is_contig = 1;
	flat_file_p = ADIOI_Add_contig_flattened(fd->filetype);
	flat_file_p->blocklens[0] = memtype_sz*count;
	filetype_extent = memtype_sz*count;
	filetype_sz = filetype_extent;
    }
    else {
        flat_file_p = ADIOI_Flatlist;
        while (flat_file_p->type != fd->filetype)
            flat_file_p = flat_file_p->next; 
    }

    disp_off_sz_ext_typesz[0] = fd->fp_ind;
    disp_off_sz_ext_typesz[1] = fd->disp;
    disp_off_sz_ext_typesz[2] = off;
    disp_off_sz_ext_typesz[3] = memtype_sz*count;
    disp_off_sz_ext_typesz[4] = (ADIO_Offset) filetype_extent;
    disp_off_sz_ext_typesz[5] = (ADIO_Offset) filetype_sz;

    if (fd->hints->cb_alltoall != ADIOI_HINT_DISABLE) {
        recv_count_arr = ADIOI_Calloc(nprocs, sizeof(amount_and_extra_data_t));
        send_count_arr = ADIOI_Calloc(nprocs, sizeof(amount_and_extra_data_t));
    } else {
        send_count_arr = ADIOI_Calloc(fd->hints->cb_nodes,
				  sizeof(amount_and_extra_data_t));

        /* only aggregators receive data */
        if (fd->is_agg) {
	    recv_count_arr = ADIOI_Calloc(nprocs, 
			    sizeof(amount_and_extra_data_t));
	    recv_req_arr = ADIOI_Malloc (nprocs * sizeof(MPI_Request));
	    for (i=0; i < nprocs; i++)
	        MPI_Irecv (&recv_count_arr[i], sizeof(amount_and_extra_data_t),
		       MPI_BYTE, i, COUNT_EXCH, fd->comm, &recv_req_arr[i]);
        }
    
        /* only send data to aggregators */
        send_req_arr = ADIOI_Calloc (fd->hints->cb_nodes, sizeof(MPI_Request));
        for (i=0; i < fd->hints->cb_nodes; i++) {
	    send_count_arr[i].count    = flat_file_p->count;
	    send_count_arr[i].fp_ind   = disp_off_sz_ext_typesz[0];
	    send_count_arr[i].disp     = disp_off_sz_ext_typesz[1];
	    send_count_arr[i].byte_off = disp_off_sz_ext_typesz[2];
	    send_count_arr[i].sz       = disp_off_sz_ext_typesz[3];
	    send_count_arr[i].ext      = disp_off_sz_ext_typesz[4];
	    send_count_arr[i].type_sz  = disp_off_sz_ext_typesz[5];
	    MPI_Isend (&send_count_arr[i], sizeof(amount_and_extra_data_t),
		   MPI_BYTE, fd->hints->ranklist[i], COUNT_EXCH, fd->comm,
		   &send_req_arr[i]);
        }
    }

 
    /* Every client has to build mem and file view_states for each aggregator.
     * We initialize their values here.  and we also initialize
     * send_count_arr */

    if (memtype_is_contig) {
	/* if memory is contigous, we now replace memtype_sz and
	 * memtype_extent with the full access size */
	memtype_sz *= count;
	memtype_extent = memtype_sz;
    }

    for (i = 0; i < fd->hints->cb_nodes; i++)
    {
	int tmp_agg_idx = fd->hints->ranklist[i];
	memset(&(my_mem_view_state_arr[tmp_agg_idx]), 0, sizeof(view_state));
	my_mem_view_state_arr[tmp_agg_idx].sz          =
	    disp_off_sz_ext_typesz[3];
	my_mem_view_state_arr[tmp_agg_idx].ext         =
	    (ADIO_Offset) memtype_extent;
	my_mem_view_state_arr[tmp_agg_idx].type_sz     =
	    (ADIO_Offset) memtype_sz;
	my_mem_view_state_arr[tmp_agg_idx].flat_type_p = flat_mem_p;
	ADIOI_init_view_state(file_ptr_type,
			1,
			&(my_mem_view_state_arr[tmp_agg_idx]),
			TEMP_OFF);
	ADIOI_init_view_state(file_ptr_type,
			1,
			&(my_mem_view_state_arr[tmp_agg_idx]),
			REAL_OFF);
	
	memset(&(agg_file_view_state_arr[tmp_agg_idx]), 0, sizeof(view_state));
	agg_file_view_state_arr[tmp_agg_idx].fp_ind    =
	    disp_off_sz_ext_typesz[0];
	agg_file_view_state_arr[tmp_agg_idx].disp      =
	    disp_off_sz_ext_typesz[1];
	agg_file_view_state_arr[tmp_agg_idx].byte_off  =
	    disp_off_sz_ext_typesz[2];
	agg_file_view_state_arr[tmp_agg_idx].sz        =
	    disp_off_sz_ext_typesz[3];
	agg_file_view_state_arr[tmp_agg_idx].ext       =
	    disp_off_sz_ext_typesz[4];
	agg_file_view_state_arr[tmp_agg_idx].type_sz   =
	    disp_off_sz_ext_typesz[5];
	agg_file_view_state_arr[tmp_agg_idx].flat_type_p = flat_file_p;

	ADIOI_init_view_state(file_ptr_type,
			1,
			&(agg_file_view_state_arr[tmp_agg_idx]),
			TEMP_OFF);
	ADIOI_init_view_state(file_ptr_type,
			1,
			&(agg_file_view_state_arr[tmp_agg_idx]),
			REAL_OFF);

	if (fd->hints->cb_alltoall != ADIOI_HINT_DISABLE) {
	    send_count_arr[tmp_agg_idx].count    = flat_file_p->count;
	    send_count_arr[tmp_agg_idx].fp_ind   = disp_off_sz_ext_typesz[0];
	    send_count_arr[tmp_agg_idx].disp     = disp_off_sz_ext_typesz[1];
	    send_count_arr[tmp_agg_idx].byte_off = disp_off_sz_ext_typesz[2];
	    send_count_arr[tmp_agg_idx].sz       = disp_off_sz_ext_typesz[3];
	    send_count_arr[tmp_agg_idx].ext      = disp_off_sz_ext_typesz[4];
	    send_count_arr[tmp_agg_idx].type_sz  = disp_off_sz_ext_typesz[5];
	}
    }

#ifdef DEBUG2
    fprintf(stderr, "my own flattened memtype: ");
    ADIOI_Print_flatlist_node(flat_mem_p);
    fprintf(stderr, "my own flattened filetype: ");
    ADIOI_Print_flatlist_node(flat_file_p);
#endif
	
    if (fd->hints->cb_alltoall != ADIOI_HINT_DISABLE) {
        ret = MPI_Alltoall(send_count_arr, sizeof(amount_and_extra_data_t),
		       MPI_BYTE, 
		       recv_count_arr, sizeof(amount_and_extra_data_t),
		       MPI_BYTE, fd->comm);
        if (ret != MPI_SUCCESS)
        {
	    fprintf(stderr, "ADIOI_Exchange_file_views: MPI_Alltoall failed "
		"with error %d", ret);
	    return;
        }
    } else {
        statuses = (MPI_Status *) ADIOI_Malloc(1 + nprocs * sizeof(MPI_Status));
        if (fd->is_agg) {
	    MPI_Waitall(nprocs, recv_req_arr, statuses);
	    ADIOI_Free(recv_req_arr);
        }
        MPI_Waitall(fd->hints->cb_nodes, send_req_arr, statuses);
        ADIOI_Free(statuses);
        ADIOI_Free(send_req_arr);
    }
#ifdef DEBUG2
    if (fd->hints->cb_alltoall != ADIOI_HINT_DISABLE) {
        fprintf(stderr, "send_count_arr:");
        for (i = 0; i < nprocs; i++)
        {
	    fprintf(stderr, "[%d]=%d ", i, send_count_arr[i].count);
        }
        fprintf(stderr, "\n");
        fprintf(stderr, "recv_count_arr:");
        for (i = 0; i < nprocs; i++)
	{
	    fprintf(stderr, "[%d]=%d ", i, recv_count_arr[i].count);
	}
        fprintf(stderr, "\n");
    } else {
        fprintf(stderr, "send_count_arr:");
        for (i = 0; i < fd->hints->cb_nodes; i++)
        {
	    fprintf(stderr, "[%d]=%d ", i, send_count_arr[i].count);
        }
        fprintf(stderr, "\n");
        if (fd->is_agg) {
	    fprintf(stderr, "recv_count_arr:");
	    for (i = 0; i < nprocs; i++)
	    {
	        fprintf(stderr, "[%d]=%d ", i, recv_count_arr[i].count);
	    }
	    fprintf(stderr, "\n");
        }
    }
#endif

    if (fd->hints->cb_alltoall == ADIOI_HINT_DISABLE) {
        for (i=0; i < fd->hints->cb_nodes; i++)
	    if (send_count_arr[i].count > 0)
	        send_req_arr_sz++;
    }
    /* Figure out how many counts to send/recv */
    for (i = 0; i < nprocs; i++)
    {
        if (fd->hints->cb_alltoall != ADIOI_HINT_DISABLE) {
	    if (send_count_arr[i].count > 0)
	        send_req_arr_sz++;
	}
	/* Only aggregators should recv*/
	if (fd->is_agg) {
	    if (recv_count_arr[i].count > 0)
	    {
		if ((client_file_view_state_arr[i].flat_type_p = 
		     (ADIOI_Flatlist_node *) ADIOI_Malloc(
			 sizeof(ADIOI_Flatlist_node))) == NULL)
		{
		    fprintf(stderr, "ADIOI_Exchange_file_views: malloc "
			    "flat_type_p failed\n");
		}
		client_file_view_state_arr[i].flat_type_p->count = 
		    recv_count_arr[i].count;
		client_file_view_state_arr[i].flat_type_p->indices = 
		    (ADIO_Offset *) ADIOI_Calloc(recv_count_arr[i].count, 
						 sizeof(ADIO_Offset));
		client_file_view_state_arr[i].flat_type_p->blocklens =
		    (ADIO_Offset *) ADIOI_Calloc(recv_count_arr[i].count, 
				    sizeof(ADIO_Offset));
		
		/* Copy the extra data out of the stuff we Alltoall'd */
		memcpy (&client_file_view_state_arr[i].fp_ind,
			&recv_count_arr[i].fp_ind,
			6*sizeof(ADIO_Offset));

		recv_req_arr_sz++;
	    }
	}
    }

    /* Since ADIOI_Calloc may do other things we add the +1 
     * to avoid a 0-size malloc */
    send_req_arr = (MPI_Request *) ADIOI_Calloc(2*(send_req_arr_sz)+1,
						sizeof(MPI_Request));
    
    j = 0;
    if (recv_req_arr_sz > 0) {
	assert (fd->is_agg);
	recv_req_arr = (MPI_Request *) ADIOI_Calloc(2*(recv_req_arr_sz),
						    sizeof(MPI_Request));
    	for (i = 0; i < nprocs; i++) {
	    if (recv_count_arr[i].count > 0) {
		MPI_Irecv(client_file_view_state_arr[i].flat_type_p->indices,
			  recv_count_arr[i].count, ADIO_OFFSET, i, 
			  INDICES, fd->comm, &recv_req_arr[j]);
		j++;
		MPI_Irecv(client_file_view_state_arr[i].flat_type_p->blocklens,
			  recv_count_arr[i].count, MPI_INT, i, 
			  BLOCK_LENS, fd->comm, &recv_req_arr[j]);
		j++;
	    }
	}
    }

    if (fd->hints->cb_alltoall != ADIOI_HINT_DISABLE) {
        j = 0;
        for (i = 0; i < nprocs; i++) {
	    if (send_count_arr[i].count > 0) {
	        MPI_Isend(flat_file_p->indices,
		      send_count_arr[i].count, ADIO_OFFSET, i,
                      INDICES, fd->comm, &send_req_arr[j]);
	        j++;
	        MPI_Isend(flat_file_p->blocklens,         
		      send_count_arr[i].count, MPI_INT, i,
                      BLOCK_LENS, fd->comm, &send_req_arr[j]);
	        j++;
	    }
        }
    } else {
        j = 0;
        for (i = 0; i < fd->hints->cb_nodes; i++) {
	    if (send_count_arr[i].count > 0) {
	        MPI_Isend(flat_file_p->indices,
		      send_count_arr[i].count, ADIO_OFFSET,
		      fd->hints->ranklist[i], INDICES, fd->comm,
		      &send_req_arr[j]);
	        j++;
	        MPI_Isend(flat_file_p->blocklens,         
		      send_count_arr[i].count, MPI_INT,
		      fd->hints->ranklist[i], BLOCK_LENS, fd->comm,
		      &send_req_arr[j]);
	        j++;
	    }
        }
    }

    /* Since ADIOI_Malloc may do other things we add the +1 
     * to avoid a 0-size malloc */    
    statuses = (MPI_Status *) 
	ADIOI_Malloc(1 + 2 * ADIOI_MAX(send_req_arr_sz,recv_req_arr_sz)
		     * sizeof(MPI_Status));

    if (send_req_arr_sz > 0) {
	MPI_Waitall(2 * send_req_arr_sz, send_req_arr, statuses);
	ADIOI_Free(send_count_arr);
	ADIOI_Free(send_req_arr);
    }
    if (recv_req_arr_sz > 0) {
	MPI_Waitall(2 * recv_req_arr_sz, recv_req_arr, statuses);
	ADIOI_Free(recv_count_arr);
	ADIOI_Free(recv_req_arr);
    }
    ADIOI_Free(statuses);

    if (fd->is_agg == 1)
    {
	ADIOI_init_view_state(file_ptr_type,
			nprocs,
			client_file_view_state_arr,
			TEMP_OFF);
	ADIOI_init_view_state(file_ptr_type,
			nprocs,
			client_file_view_state_arr,
			REAL_OFF);
    }

#ifdef DEBUG
    if (fd->is_agg == 1)
    {
	ADIOI_Flatlist_node *fr_node_p = ADIOI_Flatlist;
	for (i = 0; i < nprocs; i++)
	{
	    fprintf(stderr, "client_file_view_state_arr[%d]=(fp_ind=%Ld,"
		    "disp=%Ld,byte_off=%Ld,sz=%Ld,ext=%Ld\n", i,
		    client_file_view_state_arr[i].fp_ind,
		    client_file_view_state_arr[i].disp,
		    client_file_view_state_arr[i].byte_off,
		    client_file_view_state_arr[i].sz,
		    client_file_view_state_arr[i].ext);
	}
	
	while (fr_node_p->type != 
	       fd->file_realm_types[fd->my_cb_nodes_index])
	    fr_node_p = fr_node_p->next;
	assert(fr_node_p != NULL);
	
	fprintf(stderr, "my file realm (idx=%d,st_off=%Ld) ", 
		fd->my_cb_nodes_index,
		fd->file_realm_st_offs[fd->my_cb_nodes_index]);
	ADIOI_Print_flatlist_node(fr_node_p);
    }
#endif
    
#ifdef DEBUG2
    if (fd->is_agg == 1)
    {
	for (i = 0; i < nprocs; i++)
	{
	    fprintf(stderr, "client_file_view_state_arr[%d]: ", i);
	    ADIOI_Print_flatlist_node(
		client_file_view_state_arr[i].flat_type_p);
	}
    }
#endif
#ifdef AGGREGATION_PROFILE
    MPE_Log_event (5015, 0, NULL);
#endif
}
