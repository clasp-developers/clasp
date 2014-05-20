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
#include "heap-sort.h"

/*
#define DEBUG1
#define DEBUG2
#define DEBUG3
*/
/* #define DEBUG_HEAP */

#define DTYPE_SKIP

#ifdef DEBUG3
static char *off_type_name[MAX_OFF_TYPE] = {"TEMP_OFFSETS",
				     "REAL_OFFSETS"};
#endif

/* Simple function to return the size of the view_state. */
static inline ADIO_Offset view_state_get_cur_sz(view_state *tmp_view_state_p,
                                                int op_type)
{
    flatten_state *tmp_state_p = NULL;
    switch(op_type)
    {   
        case TEMP_OFF:
            tmp_state_p = &(tmp_view_state_p->tmp_state);
            break;
        case REAL_OFF:
            tmp_state_p = &(tmp_view_state_p->cur_state);
            break;
        default:
            fprintf(stderr, "op_type invalid\n");
    }
    return tmp_state_p->cur_sz;
}

/* Simple function to return the len of the next piece of the view_state. */
static inline ADIO_Offset view_state_get_next_len(view_state *tmp_view_state_p,
						  int op_type)
{
    flatten_state *tmp_state_p = NULL;
    switch(op_type)
    {
	case TEMP_OFF:
	    tmp_state_p = &(tmp_view_state_p->tmp_state);
	    break;
	case REAL_OFF:
	    tmp_state_p = &(tmp_view_state_p->cur_state);
	    break;
	default:
	    fprintf(stderr, "op_type invalid\n");
    }
    return (ADIO_Offset) 
	tmp_view_state_p->flat_type_p->blocklens[tmp_state_p->idx] -
	tmp_state_p->cur_reg_off;
}

/* Add up to a region of a file view and no larger than a max size.
 * The view_state is always consistent with the abs_off and where the
 * index and cur_reg_off point to.  The regions should be coalesced if
 * possible later on. */
static inline int view_state_add_region(
    ADIO_Offset max_sz,
    view_state *tmp_view_state_p, 
    ADIO_Offset *st_reg_p,
    ADIO_Offset *tmp_reg_sz_p,
    int op_type)
{
    ADIOI_Flatlist_node *tmp_flat_type_p = NULL;
    flatten_state *tmp_state_p = NULL;
    int64_t data_sz = 0;

#ifdef AGGREGATION_PROFILE
    /* MPE_Log_event (5020, 0, NULL); */
#endif

    switch(op_type)
    {
	case TEMP_OFF:
	    tmp_state_p = &(tmp_view_state_p->tmp_state);
	    break;
	case REAL_OFF:
	    tmp_state_p = &(tmp_view_state_p->cur_state);
	    break;
	default:
	    fprintf(stderr, "op_type invalid\n");
    }

    tmp_flat_type_p = tmp_view_state_p->flat_type_p;

    *st_reg_p = tmp_state_p->abs_off;

    /* Should be looking at some data (or it's a zero len blocklens
     * (i.e. placeholder). */
    assert(tmp_state_p->cur_reg_off != 
	   tmp_flat_type_p->blocklens[tmp_state_p->idx]);
    /* Shouldn't have been called if the view_state is done. */
    assert(tmp_state_p->cur_sz != tmp_view_state_p->sz);

    /* Make sure we are not in a non-zero region in the flat_type */
    assert(tmp_flat_type_p->blocklens[tmp_state_p->idx] != 0);
    
#ifdef DEBUG3
    fprintf(stderr, "view_state:(blocklens[%Ld]=%d,cur_reg_off=%Ld,"
	    "max_sz=%Ld)\n", tmp_state_p->idx, 
	    tmp_flat_type_p->blocklens[tmp_state_p->idx], 
	    tmp_state_p->cur_reg_off, max_sz);
#endif

    /* Can it add the whole piece? */
    if (tmp_flat_type_p->blocklens[tmp_state_p->idx] - 
	tmp_state_p->cur_reg_off <= max_sz)
    {
	data_sz = tmp_flat_type_p->blocklens[tmp_state_p->idx] -
            tmp_state_p->cur_reg_off;

	tmp_state_p->cur_sz += data_sz;

	/* Advance the abs_off to the beginning of the next piece */
	if (tmp_flat_type_p->count == 1)
	{
	    assert(tmp_flat_type_p->blocklens[tmp_state_p->idx] != 0);
	    tmp_state_p->abs_off += data_sz;
#ifdef DEBUG3 
	    fprintf(stderr, "view_state_add_region: %s contig type "
		    "(old abs_off=%Ld,abs_off=%Ld,cur_sz=%Ld,reg size=%Ld)\n", 
		    off_type_name[op_type], tmp_state_p->abs_off - data_sz, 
		    tmp_state_p->abs_off, tmp_state_p->cur_sz, data_sz);
#endif
	}
	else
	{ 
	    /* Is this the last region in the datatype? */
	    if (tmp_state_p->idx == (tmp_flat_type_p->count - 1))
	    {
		tmp_state_p->abs_off += data_sz -
		    tmp_flat_type_p->indices[tmp_flat_type_p->count-1] -
		    tmp_flat_type_p->blocklens[tmp_flat_type_p->count-1] +
		    tmp_view_state_p->ext;
#ifdef DEBUG3
	    fprintf(stderr, "view_state_add_region: %s last region for type "
		    "(old abs_off=%Ld,abs_off=%Ld,cur_sz=%Ld,reg size=%Ld)\n", 
		    off_type_name[op_type], tmp_state_p->abs_off - data_sz, 
		    tmp_state_p->abs_off, tmp_state_p->cur_sz, data_sz);
#endif
	    }
	    else
	    {
		tmp_state_p->abs_off += 
		    tmp_flat_type_p->indices[tmp_state_p->idx + 1] -
		    (tmp_flat_type_p->indices[tmp_state_p->idx] +
		     tmp_state_p->cur_reg_off);
#ifdef DEBUG3
	    fprintf(stderr, "view_state_add_region: %s inner region type "
		    "(old abs_off=%Ld,abs_off=%Ld,cur_sz=%Ld,reg size=%Ld)\n", 
		    off_type_name[op_type], tmp_state_p->abs_off - 
		    (tmp_flat_type_p->indices[tmp_state_p->idx + 1] -
                    (tmp_flat_type_p->indices[tmp_state_p->idx] +
                     tmp_state_p->cur_reg_off)), tmp_state_p->abs_off, 
		    tmp_state_p->cur_sz, data_sz);
#endif
	    }
	    /* Increment idx to next non-zero region in the flat_type */
	    do {
		tmp_state_p->idx = 
		    (tmp_state_p->idx + 1) % tmp_flat_type_p->count;
	    } while (tmp_flat_type_p->blocklens[tmp_state_p->idx] == 0);
	}
	tmp_state_p->cur_reg_off = 0;
    }
    else /* Add part of the piece */
    {
	data_sz = max_sz;
	tmp_state_p->cur_reg_off += data_sz;
	tmp_state_p->abs_off += data_sz;
	tmp_state_p->cur_sz += data_sz;
#ifdef DEBUG3 
	    fprintf(stderr, "view_state_add_region: %s partial region type "
		    "(cur_reg_off=%Ld,abs_off=%Ld,cur_sz=%Ld,reg size=%Ld\n", 
		    off_type_name[op_type], tmp_state_p->cur_reg_off, 
		    tmp_state_p->abs_off, tmp_state_p->cur_sz, data_sz);
#endif
    }

    *tmp_reg_sz_p = data_sz;
#ifdef AGGREGATION_PROFILE
    /* MPE_Log_event (5021, 0, NULL); */
#endif
    return 0;
}

/* Set up the abs_off, idx, and cur_reg_off of a view_state for the
 * tmp_state or the cur_state. */
int ADIOI_init_view_state(int file_ptr_type,
		    int nprocs, 
		    view_state *view_state_arr,
		    int op_type)
{
    ADIOI_Flatlist_node *tmp_flat_type_p = NULL;
    ADIO_Offset tmp_off_used = 0, st_reg = 0, tmp_reg_sz = 0;
    int i;
    flatten_state *tmp_state_p = NULL;
    view_state *tmp_view_p = NULL;

    for (i = 0; i < nprocs; i++)
    {
	switch(op_type)
	{
	    case TEMP_OFF:
		tmp_state_p = &(view_state_arr[i].tmp_state);
		break;
	    case REAL_OFF:
		tmp_state_p = &(view_state_arr[i].cur_state);
		break;
	    default:
		fprintf(stderr, "op_type invalid\n");
	}
	
	tmp_view_p = &(view_state_arr[i]);
	tmp_flat_type_p = tmp_view_p->flat_type_p;

	if (file_ptr_type == ADIO_INDIVIDUAL)
	    tmp_state_p->abs_off = tmp_view_p->fp_ind;
	else
	    tmp_state_p->abs_off = tmp_view_p->disp;
	
	tmp_off_used = 0;

	/* initialize tmp_state idx */
	while (tmp_flat_type_p->blocklens[tmp_state_p->idx] == 0)
	    tmp_state_p->idx = (tmp_state_p->idx + 1) % tmp_flat_type_p->count;
	if (file_ptr_type == ADIO_EXPLICIT_OFFSET)
	    tmp_state_p->abs_off += tmp_flat_type_p->indices[tmp_state_p->idx];

	/* Initialize the abs_off by moving into the datatype 
	 * byte_off bytes.  Since we only do this in the beginning, we
	 * make the assumption that pieces are added whole until the last
	 * piece which MAY be partial. */
	while (tmp_off_used != tmp_view_p->byte_off)
	{
	    view_state_add_region(
		tmp_view_p->byte_off - tmp_off_used,
		&(view_state_arr[i]), &st_reg, &tmp_reg_sz, 
		op_type);
	}
	
	/* Re-initialize the cur_size so that the abs_off was set to
	 * the proper position while the actual size = 0.*/
	tmp_state_p->cur_sz = 0;
#ifdef DEBUG1
	fprintf(stderr, "init_view_state: %s (idx=%d,byte_off=%Ld,"
		"abs_off=%Ld,reg_off=%Ld,sz=%Ld)\n", off_type_name[op_type], 
		i, tmp_view_p->byte_off, tmp_state_p->abs_off,
		tmp_state_p->cur_reg_off, tmp_view_p->sz);
#endif	

    }
    return 0;
}

/* Return the next file realm offset and length for this datatype state
 * within a particular file realm. */
static inline int get_next_fr_off(ADIO_File fd,
				  ADIO_Offset off,
				  ADIO_Offset fr_st_off,
				  MPI_Datatype *fr_type_p,
				  ADIO_Offset *fr_next_off_p,
				  ADIO_Offset *fr_max_len_p) 
{
    MPI_Aint fr_extent = -1;
    ADIO_Offset tmp_off, off_rem;
    ADIOI_Flatlist_node *fr_node_p = ADIOI_Flatlist;
    int i = -1, fr_dtype_ct = 0;

    /* Should have already been flattened in calc_file_realms() */
    while (fr_node_p->type != (*fr_type_p))
	fr_node_p = fr_node_p->next;
    assert(fr_node_p != NULL);

    /* Did we get to the first region of the file realm? */
    if (off - fr_st_off < 0)
    {
	*fr_next_off_p = fr_st_off + fr_node_p->indices[0];
	*fr_max_len_p = fr_node_p->blocklens[0];
	return 0;
    }

    /* Calculate how many times to loop through the fr_type 
     * and where the next fr_off is. */
    MPI_Type_extent(*fr_type_p, &fr_extent);
    tmp_off = off - fr_st_off;
    fr_dtype_ct = tmp_off / fr_extent;
    off_rem = tmp_off % fr_extent;
    for (i = 0; i < fr_node_p->count; i++)
    {
	if (off_rem < fr_node_p->indices[i])
	{
	    *fr_next_off_p = fr_st_off +
		(fr_dtype_ct * fr_extent) + fr_node_p->indices[i];
	    *fr_max_len_p = fr_node_p->blocklens[i];
	    return 0;
	}
	else if (off_rem < fr_node_p->indices[i] + fr_node_p->blocklens[i])
	{
	    *fr_next_off_p = off;
	    *fr_max_len_p = fr_node_p->blocklens[i] - 
		(off_rem - fr_node_p->indices[i]);
	    return off;
	}
    }
    
    /* Shouldn't get here. */
    fprintf(stderr, "get_next_fr_off: Couldn't find the correct "
	    "location of the next offset for this file realm.\n");
    return -1;
}

/* Look in all the view states for the first offset within a given
 * file realm.  Report the end of a contiguous region within the file
 * realm (possibly more than the actual view state may be able to
 * process contiguously). */
static inline int find_next_off(ADIO_File fd,
				view_state *view_state_p,
				ADIO_Offset fr_st_off,
				MPI_Datatype *fr_type_p,
				int op_type,
				ADIO_Offset *cur_off_p,
				ADIO_Offset *cur_reg_max_len_p)
{
    ADIOI_Flatlist_node *tmp_flat_type_p = NULL;
    ADIO_Offset tmp_off = -1, fr_next_off = -1, fr_max_len = -1, 
	tmp_fr_max_len = -1;
    int ret = -1;
    flatten_state *tmp_state_p = NULL;
    ADIO_Offset tmp_st_off = 0, tmp_reg_sz = 0;
#ifdef DTYPE_SKIP
    int skip_type_ct;
#endif

#ifdef AGGREGATION_PROFILE
    /* MPE_Log_event (5022, 0, NULL); */
#endif

    switch(op_type)
    {
	case TEMP_OFF:
	    tmp_state_p = &(view_state_p->tmp_state);
	    break;
	case REAL_OFF:
	    tmp_state_p = &(view_state_p->cur_state);
	    break;
	default:
	    fprintf(stderr, "op_type invalid\n");
    }
	
    tmp_flat_type_p = view_state_p->flat_type_p;

    /* Can we use this proc? */
    if (tmp_state_p->cur_sz < view_state_p->sz) {
	tmp_st_off = 0;
	tmp_reg_sz = 0;
	/* If the current region is not within the file realm, advance
	 * the state until it is and calculate the end of the next file 
	 * realm in fr_max_len. */
	ret = get_next_fr_off(fd,
			      tmp_state_p->abs_off, 
			      fr_st_off,
			      fr_type_p,
			      &fr_next_off,
			      &fr_max_len);
	
	while ((tmp_state_p->abs_off < fr_next_off) &&
	       (tmp_state_p->cur_sz != view_state_p->sz))
	{
	    
	/* While this might appear to be erroneous at first,
	 * view_state_add_region can only add a single piece at a
	 * time.  Therefore, it will never overshoot the beginning
	 * of the next file realm.  When it finally does enter the
	 * next file realm it will not be able to go beyond its
	 * first piece. */
	    
#ifdef DTYPE_SKIP
	    if (tmp_flat_type_p->count > 1) {
		/* let's see if we can skip whole datatypes */
		skip_type_ct = (fr_next_off - tmp_state_p->abs_off) /
		    view_state_p->ext;
		if (skip_type_ct > 0) {
		    /* before we go on, let's check if we've actually
		     * finished up already */
		    tmp_state_p->cur_sz += skip_type_ct *
			view_state_p->type_sz;
		    if (tmp_state_p->cur_sz >= view_state_p->sz) {
			tmp_state_p->cur_sz = view_state_p->sz;
			break;
		    }
		    tmp_state_p->abs_off += skip_type_ct * view_state_p->ext;
		}
	    }
#endif
	    view_state_add_region(
		fr_next_off - tmp_state_p->abs_off,
		view_state_p,
		&tmp_st_off,
		&tmp_reg_sz,
		op_type);

	    ret = get_next_fr_off(fd,
				  tmp_state_p->abs_off, 
				  fr_st_off,
				  fr_type_p,
				  &fr_next_off,
				  &fr_max_len);
	}

	if (tmp_state_p->cur_sz != view_state_p->sz) {
	    tmp_off = tmp_state_p->abs_off;
	    /* Calculate how much of the remaining file realm there is from the
	     * current offset */
	    tmp_fr_max_len = fr_next_off + fr_max_len - tmp_off;
	}
    }

    *cur_off_p = tmp_off;
    *cur_reg_max_len_p = tmp_fr_max_len;
#ifdef AGGREGATION_PROFILE
    /* MPE_Log_event (5023, 0, NULL); */
#endif
    return 0;
}

/* Upon completion of a full collective buffer, end of a file realm
 * region (data sieving), or the end of all I/O for an aggregator, we
 * should return a list of MPI_Datatypes that correspond to client
 * communication into a collective buffer, a list of corresponding
 * sizes, and an aggregate MPI_Datatype which will be used as a
 * filetype in MPI_File_write/read on the aggregator. */ 
int ADIOI_Build_agg_reqs(ADIO_File fd, int rw_type, int nprocs,
			 view_state *client_file_view_state_arr,
			 MPI_Datatype *client_comm_dtype_arr,
			 ADIO_Offset *client_comm_sz_arr,
			 ADIO_Offset *agg_dtype_offset_p,
			 MPI_Datatype *agg_dtype_p)
{
    MPI_Aint **client_disp_arr = NULL, *agg_disp_arr = NULL;
    int **client_blk_arr = NULL, *agg_blk_arr = NULL;
    ADIO_Offset tmp_coll_buf_sz = 0, st_reg = 0, act_reg_sz = 0;
    ADIO_Offset cur_off = -1, cur_reg_max_len = -1;
    ADIO_Offset ds_fr_end = -1;
    ADIO_Offset *fr_st_off_arr = fd->file_realm_st_offs;
    MPI_Datatype *fr_type_arr = fd->file_realm_types;
    int *client_ol_ct_arr = NULL;
    int *client_ol_cur_ct_arr = NULL;
    int agg_ol_ct = 0, agg_ol_cur_ct = 0;
    int cur_off_proc = -1;
    int next_off_idx = -1;
    int i = 0, j = 0, all_done = -1;
    int agg_idx = fd->my_cb_nodes_index;
    heap_t offset_heap;
    ADIO_Offset next_off = -1, next_reg_max_len = -1;

    /* Used for coalescing ol pairs next to each other. */
    ADIO_Offset *client_comm_next_off_arr = NULL;
    ADIO_Offset agg_next_off = -1;
#ifdef AGGREGATION_PROFILE
    MPE_Log_event (5016, 0, NULL);
#endif

    memset(client_comm_sz_arr, 0, nprocs*sizeof(ADIO_Offset));

    if ((client_comm_next_off_arr = (ADIO_Offset *) 
	 ADIOI_Malloc(nprocs*sizeof(ADIO_Offset))) == NULL)
    {
	fprintf(stderr, "ADIOI_Build_agg_reqs: malloc client_next_off_arr "
		"failed\n");
	return -1;
    }
    
    if ((client_ol_ct_arr = (int *) ADIOI_Calloc(nprocs, sizeof(int))) == NULL)
    {
	fprintf(stderr, "ADIOI_Build_agg_reqs: "
		"malloc client_ol_ct_arr failed\n");
	return -1;
    }
    if ((client_ol_cur_ct_arr = 
	 (int *) ADIOI_Calloc(nprocs, sizeof(int))) == NULL)
    {
	fprintf(stderr, "ADIOI_Build_agg_reqs: "
		"malloc client_ol_cur_ct_arr failed\n");
	return -1;
    }

    /* On the first pass see how many offset-length pairs are
     * necessary for each client.  Then allocate the correct amount of
     * offset-length pairs for describing the collective buffer.  All
     * data is processed in order by the aggregator's file realm.  On
     * the second pass, set the offset-length pairs to the correct
     * values. */
    for (i = 0; i < MAX_OFF_TYPE; i++)
    {
	memset(client_comm_next_off_arr, -1, nprocs*sizeof(ADIO_Offset));
	tmp_coll_buf_sz = 0;
	ds_fr_end = -1;

	/* initialize heap */
	ADIOI_Heap_create(&offset_heap, nprocs);
	offset_heap.size = 0;
	
	for (j=0; j<nprocs; j++) {
	    find_next_off(fd, 
			  &client_file_view_state_arr[j],
			  fr_st_off_arr[agg_idx],
			  &(fr_type_arr[agg_idx]),
			  i,
			  &cur_off,
			  &cur_reg_max_len);
	    if ((cur_off != -1) && (cur_reg_max_len > 0)) {
		ADIOI_Heap_insert(&offset_heap, cur_off, j, cur_reg_max_len);
#ifdef DEBUG_HEAP
		printf ("initial: inserting offset %lld with "
			"cur_reg_max_len = %lld for p%d\n",
			cur_off, cur_reg_max_len, j);
#endif
	    }

	}
	if (!offset_heap.size)
	    ADIOI_Heap_insert(&offset_heap, -1, -1, -1);

	while (tmp_coll_buf_sz < fd->hints->cb_buffer_size)
	{
	    /* Find the next process with the next region within the
	     * file realm and the maximum amount that can be added for
	     * this particular file realm as a contiguous region. */
	    ADIOI_Heap_extract_min(&offset_heap, &cur_off, &cur_off_proc,
			     &cur_reg_max_len);
#ifdef DEBUG_HEAP
	    printf ("extracted cur_off %lld from proc %d\n",
		    cur_off, cur_off_proc);
#endif

	    if (cur_off == -1)
		break;
	    
#ifdef DEBUG3
	    fprintf(stderr, "ADIOI_Build_agg_reqs: %s proc %d start/add to"
		    " list (max_reg_fr=%Ld,tmp_coll_buf_sz=%Ld,"
		    "cb_buffer_size=%d)\n", off_type_name[i], cur_off_proc,
		    cur_reg_max_len, tmp_coll_buf_sz, 
		    fd->hints->cb_buffer_size);
#endif
	    
	    /* We process only contiguous file realm regions if we are
	     * using data sieving. Note that we only do this for
	     * writes since reads can be data sieved across each other
	     * without consistency issues. */
	    if ((fd->hints->ds_write == ADIOI_HINT_ENABLE ||
		 fd->hints->ds_write == ADIOI_HINT_AUTO) &&
		rw_type == ADIOI_WRITE && fd->hints->cb_nodes > 1)
	    {
#ifdef DEBUG2
		fprintf(stderr, "ADIOI_Build_agg_reqs: "
			"Warning - Data sieving writes on\n");
#endif
		if (ds_fr_end == -1)
		{
		    ds_fr_end = cur_off + cur_reg_max_len;
#ifdef DEBUG1
		fprintf(stderr, "ADIOI_Build_agg_reqs: "
			"cur_off=%Ld, cur_reg_max_len=%Ld\n"
			"Data sieving file realm end initialized to %Ld\n",
			cur_off,
			cur_reg_max_len,
			ds_fr_end);
#endif
		}
		else
		{
		    /* The next off switched file realms, so we will stop
		     * here. */
		    if (ds_fr_end != cur_off + cur_reg_max_len)
		    {
#ifdef DEBUG1
			fprintf(stderr, "ADIOI_Build_agg_reqs: "
				"Data sieving file realm end changed from "
				"%Ld to %Ld\n", ds_fr_end, 
				cur_off + cur_reg_max_len);
#endif
			break;
		    }
		}
	    }
	    
	    /* Add up to the end of the file realm or the collective
	     * buffer. */
	    if (cur_reg_max_len > (fd->hints->cb_buffer_size - 
				   tmp_coll_buf_sz))
		cur_reg_max_len = fd->hints->cb_buffer_size - tmp_coll_buf_sz;

	    view_state_add_region(
		cur_reg_max_len,
		&(client_file_view_state_arr[cur_off_proc]), 
		&st_reg, &act_reg_sz, i);

	    switch(i)
	    {
		case TEMP_OFF:
		    /* Increment the ol list count for each proc and
		     * the used part of the collective buffer if the
		     * next region is not adjacent to the previous
		     * region. */
		    if (client_comm_next_off_arr[cur_off_proc] != 
			tmp_coll_buf_sz)
		    {
			(client_ol_ct_arr[cur_off_proc])++;
		    }
		    client_comm_next_off_arr[cur_off_proc] = 
			tmp_coll_buf_sz + act_reg_sz;
		    
		    if (agg_next_off != st_reg)
			agg_ol_ct++;
		    agg_next_off = st_reg + act_reg_sz;
		    break;
		case REAL_OFF:
		    /* Add this region to the proper client ol list if
		     * the next region is not adjacent to the previous
		     * region. */
		    next_off_idx = client_ol_cur_ct_arr[cur_off_proc];
		    if (client_comm_next_off_arr[cur_off_proc] != 
			tmp_coll_buf_sz)
		    {
			client_disp_arr[cur_off_proc][next_off_idx] =
			    tmp_coll_buf_sz;
			client_blk_arr[cur_off_proc][next_off_idx] = 
			    act_reg_sz;
			(client_ol_cur_ct_arr[cur_off_proc])++;
		    }
		    else
		    {
			client_blk_arr[cur_off_proc][next_off_idx - 1] 
			    += act_reg_sz;
		    }
		    client_comm_sz_arr[cur_off_proc] += act_reg_sz;
		    client_comm_next_off_arr[cur_off_proc] =
			tmp_coll_buf_sz + act_reg_sz;
		    
		    /* Add to the aggregator filetype if the next
		     * region is not adjacent to the previous
		     * region. */
		    if (agg_next_off != st_reg)
		    {
			/* this will enable initial offsets much further into
			 * the file than an MPI_Aint */
			if (!agg_ol_cur_ct)
			    *agg_dtype_offset_p = st_reg;
			agg_disp_arr[agg_ol_cur_ct] = st_reg -
			    (MPI_Aint) *agg_dtype_offset_p;
			agg_blk_arr[agg_ol_cur_ct] = act_reg_sz;	
			agg_ol_cur_ct++;
		    }
		    else
		    {
			agg_blk_arr[agg_ol_cur_ct - 1] += act_reg_sz;
		    }
		    agg_next_off = st_reg + act_reg_sz;
		    
		    break;
		default:
		    fprintf(stderr, "ADIOI_Build_agg_reqs: Impossible type\n");
	    }
	    tmp_coll_buf_sz += act_reg_sz;

	    find_next_off(fd,
			  &client_file_view_state_arr[cur_off_proc],
			  fr_st_off_arr[agg_idx],
			  &(fr_type_arr[agg_idx]),
			  i,
			  &next_off,
			  &next_reg_max_len);

	    if ((next_off != -1) || (!offset_heap.size)) {
		ADIOI_Heap_insert(&offset_heap, next_off, cur_off_proc,
			    next_reg_max_len);
#ifdef DEBUG_HEAP
		printf ("inserting offset %lld for p%d\n", next_off,
			cur_off_proc);
#endif
	    }
	}
	
	if (i == TEMP_OFF)
	{
	    /* Allocate offset-length pairs for creating hindexed
	     * MPI_Datatypes for both the client and the aggregator. */
	    if ((client_disp_arr = (MPI_Aint **) 
		 ADIOI_Malloc(nprocs*sizeof(MPI_Aint *))) == NULL)
	    {
		fprintf(stderr, "ADIOI_Build_agg_reqs: malloc "
			"client_disp_arr failed\n");
		return -1;
	    }
	    if ((client_blk_arr = (int **) ADIOI_Malloc(
		     nprocs*sizeof(int *))) == NULL)
	    {
		ADIOI_Free(client_disp_arr);
		fprintf(stderr, "ADIOI_Build_agg_reqs: malloc "
			"client_blk_arr failed\n");
		return -1;
	    }    
	    for (j = 0; j < nprocs; j++)
	    {
		if ((client_disp_arr[j] = (MPI_Aint *) ADIOI_Malloc(
			 client_ol_ct_arr[j]*sizeof(MPI_Aint))) == NULL)
		{
		    fprintf(stderr, "ADIOI_Build_agg_reqs: malloc "
			    "client_disp_arr[%d] failed\n", j);
		    return -1;
		}
		if ((client_blk_arr[j] = (int *) 
		     ADIOI_Malloc(client_ol_ct_arr[j]*sizeof(int))) == NULL)
		{
		    ADIOI_Free(client_disp_arr[j]);
		    fprintf(stderr, "ADIOI_Build_agg_reqs: malloc "
			    "client_blk_arr[%d] failed\n", j);
		    return -1;
		}
	    }
	    
	    if (agg_ol_ct > 0) 
	    {
		if ((agg_disp_arr = (MPI_Aint *) ADIOI_Malloc(
			 agg_ol_ct*sizeof(MPI_Aint))) == NULL)
		{
		    fprintf(stderr, 
			    "ADIOI_Build_agg_reqs: malloc disp_arr failed\n");
		    return -1;
		}
		if ((agg_blk_arr = (int *) 
		     ADIOI_Malloc(agg_ol_ct*sizeof(int))) == NULL)
		{
		    ADIOI_Free(agg_disp_arr);
		    fprintf(stderr, 
			    "ADIOI_Build_agg_reqs: malloc blk_arr failed\n");
		    return -1;
		}
	    }
	}
	ADIOI_Heap_free(&offset_heap);
    }
    
    /* Let the clients know if this aggregator is totally finished
     * with all possible client requests. */
    all_done = 1;
    for (i = 0; i < nprocs; i++)
    {
	if ((client_file_view_state_arr[i].cur_state.cur_sz !=
            client_file_view_state_arr[i].sz) ||
            client_comm_sz_arr[i] != 0)
	{
	    all_done = 0;
	    break;
	}
    }
    if (all_done == 1)
    {
	for (i = 0; i < nprocs; i++)
	{
	    client_comm_sz_arr[i] = -1;
	}
    }

    /* Quick check to make sure we found all the ol pairs we thought
     * we did */
    for (i = 0; i < nprocs; i++)
    {
	if (client_ol_cur_ct_arr[i] != client_ol_ct_arr[i])
	{
	    fprintf(stderr, "ADIOI_Build_agg_reqs: ERROR Process %d "
		    "processed only %d out of %d ol pairs\n", i, 
		    client_ol_cur_ct_arr[i],
		    client_ol_ct_arr[i]);
	    return -1;
	}
    }
#ifdef DEBUG1
    fprintf(stderr, "ADIOI_Build_agg_reqs:(client,ol_pairs,size_req)=");
    for (i = 0; i < nprocs; i++)
    {
	fprintf(stderr, "(%d,%d,%Ld)", i, client_ol_ct_arr[i],
		client_comm_sz_arr[i]);
	if (i != nprocs - 1)
	    fprintf(stderr, ",");
    }
    fprintf(stderr, "\n");
#endif
#ifdef DEBUG1
    fprintf(stderr, "ADIOI_Build_agg_reqs: Generated %d of %d "
	    "aggregate offset-length pairs\n", agg_ol_cur_ct, agg_ol_ct);
#endif
#ifdef DEBUG2
    for (i = 0; i < nprocs; i++)
    {
	if (client_ol_ct_arr[i] > 0)
	{
	    fprintf(stderr, "ADIOI_Build_agg_reqs: p %d (off,len) = ", i);
	    for (j = 0; j < client_ol_ct_arr[i]; j++)
	    {
		fprintf(stderr, "[%d](%d,%d) ", j, 
			client_disp_arr[i][j],
			client_blk_arr[i][j]);
	    }
	    fprintf(stderr, "\n");
	}
    }    
    if (agg_ol_ct) {
	fprintf(stderr, "ADIOI_Build_agg_reqs:agg_type(off,len)=");
	for (i = 0; i < agg_ol_ct; i++)
	    {
		fprintf(stderr, "[%d](%d,%d)",
			i, agg_disp_arr[i], agg_blk_arr[i]);
		if (i != agg_ol_ct - 1)
		    fprintf(stderr, ",");
	    }
	fprintf(stderr, "\n");
    }
#endif

    assert(agg_ol_cur_ct == agg_ol_ct);

    /* Create all the client and aggregate MPI_Datatypes */
    for (i = 0; i < nprocs; i++)
    {
	if (client_comm_sz_arr[i] > 0)
	{
	    MPI_Type_hindexed(client_ol_ct_arr[i], client_blk_arr[i],
			      client_disp_arr[i], MPI_BYTE, 
			      &(client_comm_dtype_arr[i]));
	    MPI_Type_commit(&(client_comm_dtype_arr[i]));
	}
	else
	{
	    client_comm_dtype_arr[i] = MPI_BYTE;
	}
	ADIOI_Free(client_blk_arr[i]);
	ADIOI_Free(client_disp_arr[i]);
    }
    ADIOI_Free(client_blk_arr);
    ADIOI_Free(client_disp_arr);

    if (agg_ol_ct > 0) {
	if (agg_ol_ct == 1)
	    MPI_Type_contiguous (agg_blk_arr[0], MPI_BYTE, agg_dtype_p);
	else if (agg_ol_ct > 1)
	    MPI_Type_hindexed(agg_ol_ct, agg_blk_arr, agg_disp_arr, MPI_BYTE,
			      agg_dtype_p);    

	MPI_Type_commit(agg_dtype_p);

	ADIOI_Free(agg_disp_arr);
	ADIOI_Free(agg_blk_arr);
    }
    ADIOI_Free(client_ol_ct_arr);
    ADIOI_Free(client_ol_cur_ct_arr);
    ADIOI_Free(client_comm_next_off_arr);
#ifdef AGGREGATION_PROFILE
    MPE_Log_event (5017, 0, NULL);
#endif
    return 0;
}

/* All sizes from all aggregators are gathered on the clients, which
 * then call this function, which will generate the comm datatypes for
 * each aggregator (agg_comm_dtype_arr) in the upcoming
 * MPI_Alltoallw() */
int ADIOI_Build_client_reqs(ADIO_File fd, 
			    int nprocs,
			    view_state *my_mem_view_state_arr,
			    view_state *agg_file_view_state_arr,
			    ADIO_Offset *agg_comm_sz_arr,
			    MPI_Datatype *agg_comm_dtype_arr)
{
    MPI_Aint **agg_disp_arr = NULL;
    int **agg_blk_arr = NULL;
    view_state *tmp_mem_state_p = NULL, *tmp_file_state_p = NULL;
    ADIO_Offset total_agg_comm_sz = 0, cur_total_agg_comm_sz = 0;
    ADIO_Offset st_reg = 0, act_reg_sz = 0, tmp_reg_sz = 0;
    ADIO_Offset cur_off = -1, cur_reg_max_len = -1;
    ADIO_Offset tmp_cur_off = -1, tmp_cur_reg_max_len = -1;
    ADIO_Offset agg_mem_st_reg = 0, agg_mem_act_reg_sz = 0;
    ADIO_Offset *fr_st_off_arr = fd->file_realm_st_offs;
    ADIO_Offset *agg_comm_cur_sz_arr = NULL;
    MPI_Datatype *fr_type_arr = fd->file_realm_types;
    int cb_node_ct = fd->hints->cb_nodes;
    int *agg_ol_ct_arr = NULL;
    int *agg_ol_cur_ct_arr = NULL;
    int agg_fr_idx = -1, tmp_agg_fr_idx = -1;
    int cur_off_proc = -1;
    int i = 0, j = 0;
    int agg_next_off_idx = -1;
    /* Used for coalescing ol pairs next to each other. */
    ADIO_Offset *agg_mem_next_off_arr = NULL;
#ifdef AGGREGATION_PROFILE
    MPE_Log_event (5018, 0, NULL);
#endif

#ifdef DEBUG
    fprintf(stderr, "ADIOI_Build_client_reqs:(agg,size_req)=");
    for (i = 0; i < nprocs; i++)
    {
	int tmp_agg_idx = ADIOI_Agg_idx(i, fd);
        if (tmp_agg_idx >= 0)
        {
	    fprintf(stderr, "(%d,%Ld)", i, agg_comm_sz_arr[i]);
	    if (i != fd->hints->cb_nodes - 1)
		fprintf(stderr, ",");
	}
	fprintf(stderr, "\n");
    }
#endif
    
    if ((agg_mem_next_off_arr = (ADIO_Offset *) ADIOI_Malloc(
	     nprocs*sizeof(ADIO_Offset))) == NULL)
    {
	fprintf(stderr, "ADIOI_Build_client_reqs: malloc agg_mem_next_off_arr"
		"failed\n");
	return -1;
    }

    if ((agg_comm_cur_sz_arr = (ADIO_Offset *) 
	 ADIOI_Malloc(nprocs*sizeof(ADIO_Offset))) == NULL)
    {
	fprintf(stderr, "ADIOI_Build_client_reqs: malloc agg_comm_cur_sz_arr"
		" failed\n");
	return -1;
    }
    if ((agg_ol_ct_arr = (int *) ADIOI_Calloc(nprocs, sizeof(int)))
	== NULL)
    {
	fprintf(stderr, "ADIOI_Build_client_reqs: "
		"malloc agg_ol_ct_arr failed\n");
	return -1;
    }
    if ((agg_ol_cur_ct_arr = (int *) ADIOI_Calloc(nprocs, sizeof(int)))
	== NULL)
    {
	fprintf(stderr, "ADIOI_Build_client_reqs: "
		"malloc agg_ol_cur_ct_arr failed\n");
	return -1;
    }

    for (i = 0; i < nprocs; i++)
    {
	if (agg_comm_sz_arr[i] > 0)
	    total_agg_comm_sz += agg_comm_sz_arr[i];
    }
    
    /* On the first pass see how many offset-length pairs are
     * necessary for each aggregator.  Then allocate the correct
     * amount of offset-length pairs for handling each aggregator's
     * particular data size.  On the last pass, we actually create the
     * offset-length pairs. */
    for (i = 0; i < MAX_OFF_TYPE; i++)
    {
	cur_total_agg_comm_sz = 0;
	memset(agg_comm_cur_sz_arr, 0, nprocs*sizeof(ADIO_Offset));
	memset(agg_mem_next_off_arr, -1, nprocs*sizeof(ADIO_Offset));
	while (total_agg_comm_sz > cur_total_agg_comm_sz)
	{
	    /* Look for the next aggregator offset among all the
	     * aggregators and their respective file realms. */
	    cur_off = -1;
	    for (j = 0; j < nprocs; j++)
	    {
		tmp_agg_fr_idx = ADIOI_Agg_idx(j, fd);
		assert(tmp_agg_fr_idx < cb_node_ct);
		
		/* If this process is not an aggregator or we have
		 * finished all the bytes for this aggregator, move
		 * along. */
		if (tmp_agg_fr_idx < 0 || 
		    agg_comm_cur_sz_arr[j] == agg_comm_sz_arr[j])
		{
		    continue;
		}

		find_next_off(fd,
			      &(agg_file_view_state_arr[j]),
			      fr_st_off_arr[tmp_agg_fr_idx],
			      &(fr_type_arr[tmp_agg_fr_idx]),
			      i,
			      &tmp_cur_off,
			      &tmp_cur_reg_max_len);
		if (tmp_cur_off == -1)
		    continue;	       

		if ((cur_off == -1) || 
		    (cur_off > tmp_cur_off))
		{
		    agg_fr_idx = tmp_agg_fr_idx;
		    cur_off_proc = j;
		    cur_off = tmp_cur_off;
		    cur_reg_max_len = tmp_cur_reg_max_len;
		}
	    }

	    assert(cur_off_proc != -1);
	    
	    /* Add up to the end of the file realm or as many bytes
	     * are left for this particular aggregator in the client's
	     * filetype */
	    if (cur_reg_max_len > agg_comm_sz_arr[cur_off_proc] - 
		agg_comm_cur_sz_arr[cur_off_proc])
	    {
		cur_reg_max_len = agg_comm_sz_arr[cur_off_proc] - 
		    agg_comm_cur_sz_arr[cur_off_proc];
	    }
	    assert(cur_reg_max_len > 0);
	    
	    view_state_add_region(
		cur_reg_max_len,
		&(agg_file_view_state_arr[cur_off_proc]),
		&st_reg, &act_reg_sz, i);
	    
#ifdef DEBUG2
	    fprintf(stderr, "ADIOI_Build_client_reqs: %s File region"
		    " (proc=%d,off=%Ld,sz=%Ld)\n",
		    off_type_name[i], cur_off_proc,
		    cur_off, act_reg_sz);
#endif

	    /* Before translating the file regions to memory regions,
	     * we first must advance to the proper point in the
	     * mem_view_state for this aggregator to match the
	     * file_view_state. */
	    tmp_file_state_p = &(agg_file_view_state_arr[cur_off_proc]);
	    tmp_mem_state_p = &(my_mem_view_state_arr[cur_off_proc]);
	    assert(view_state_get_cur_sz(tmp_file_state_p, i) - act_reg_sz >=
		   view_state_get_cur_sz(tmp_mem_state_p, i));
	    while (view_state_get_cur_sz(tmp_file_state_p, i) - act_reg_sz != 
		   view_state_get_cur_sz(tmp_mem_state_p, i))
	    {
		ADIO_Offset fill_st_reg = -1, fill_reg_sz = -1;
		view_state_add_region(
		    view_state_get_cur_sz(tmp_file_state_p, i) - act_reg_sz -
		    view_state_get_cur_sz(tmp_mem_state_p, i),
		    tmp_mem_state_p,
		    &fill_st_reg,
		    &fill_reg_sz, i);
	    }
	    
	    /* Based on how large the act_reg_sz 1. Figure out how
	     * many memory offset-length pairs are necessary. 2. Set
	     * the offset-length pairs. */
	    tmp_reg_sz = 0;
	    while (tmp_reg_sz != act_reg_sz)
	    {
		view_state_add_region(
		    act_reg_sz - tmp_reg_sz,
		    tmp_mem_state_p,
		    &agg_mem_st_reg, &agg_mem_act_reg_sz, 
		    i);
		tmp_reg_sz += agg_mem_act_reg_sz;

#ifdef DEBUG2
		fprintf(stderr, "ADIOI_Build_client_reqs: Mem region %s"
			"(proc=%d,off=%Ld,sz=%Ld)\n",
			off_type_name[i], cur_off_proc,
			agg_mem_st_reg, agg_mem_act_reg_sz);
#endif
		agg_comm_cur_sz_arr[cur_off_proc] += agg_mem_act_reg_sz;
		cur_total_agg_comm_sz += agg_mem_act_reg_sz;	    
		switch(i)
		{
		    case TEMP_OFF:
			/* Increment the ol list count a particular
			 * aggregator if next region is not adjacent
			 * to the previous region. */
			if (agg_mem_next_off_arr[cur_off_proc] != 
			    agg_mem_st_reg)
			{
			    agg_ol_ct_arr[cur_off_proc]++;
			}
			agg_mem_next_off_arr[cur_off_proc] = 
			    agg_mem_st_reg + agg_mem_act_reg_sz;
			break;
		    case REAL_OFF:
			/* Set the ol list for the memtypes that will
			 * map to each aggregator, coaslescing if
			 * possible. */
			agg_next_off_idx = agg_ol_cur_ct_arr[cur_off_proc];
			if (agg_mem_next_off_arr[cur_off_proc] != 
			    agg_mem_st_reg)
			{
			    agg_disp_arr[cur_off_proc][agg_next_off_idx] = 
				agg_mem_st_reg;
			    agg_blk_arr[cur_off_proc][agg_next_off_idx] = 
				agg_mem_act_reg_sz;
			    (agg_ol_cur_ct_arr[cur_off_proc])++;
			}
			else
			{
			    agg_blk_arr[cur_off_proc][agg_next_off_idx - 1]
				+= agg_mem_act_reg_sz;
			}
			agg_mem_next_off_arr[cur_off_proc] = 
			    agg_mem_st_reg + agg_mem_act_reg_sz;
			break;
		    default:
			fprintf(stderr, "ADIOI_Build_client_reqs: "
				"Impossible type\n");
		}
	    }
	}
	
	/* On the first pass, allocate the memory structures for
	 * creating the MPI_hindexed type. */
	if (i == TEMP_OFF)
	{	    
	    /* Allocate offset-length pairs for creating hindexed
	     * MPI_Datatypes for each aggregator */
	    if ((agg_disp_arr = (MPI_Aint **) 
		 ADIOI_Malloc(nprocs*sizeof(MPI_Aint *))) == NULL)
	    {
		fprintf(stderr, 
			"ADIOI_Build_client_reqs: malloc agg_disp_arr failed\n");
		return -1;
	    }
	    if ((agg_blk_arr = (int **) ADIOI_Malloc(nprocs*sizeof(int *))) 
		== NULL)
	    {
		ADIOI_Free(agg_disp_arr);
		fprintf(stderr, 
			"ADIOI_Build_client_reqs: malloc agg_blk_arr failed\n");
		return -1;
	    }    
	    for (j = 0; j < nprocs; j++)
	    {
		if ((agg_disp_arr[j] = (MPI_Aint *) 
		     ADIOI_Malloc(agg_ol_ct_arr[j]*sizeof(MPI_Aint))) == NULL)
		{
		    fprintf(stderr, "ADIOI_Build_client_reqs: malloc "
			    "agg_disp_arr[%d] failed\n", j);
		    return -1;
		}
		if ((agg_blk_arr[j] = (int *) 
		     ADIOI_Malloc(agg_ol_ct_arr[j]*sizeof(int))) == NULL)
		{
		    ADIOI_Free(agg_disp_arr[j]);
		    fprintf(stderr, "ADIOI_Build_client_reqs: malloc "
			    "agg_blk_arr[%d] failed\n", j);
		    return -1;
		}
	    }
	}
    }

#ifdef DEBUG
    fprintf(stderr, "ADIOI_Build_client_reqs:(agg,cur_ol_count=ol_count)=");
    for (i = 0; i < nprocs; i++)
    {
	int tmp_agg_idx = ADIOI_Agg_idx(i, fd);
	if (tmp_agg_idx >= 0)
	{
	    fprintf(stderr, "(%d,%d=%d)", i, agg_ol_cur_ct_arr[i],
		    agg_ol_ct_arr[i]);
	    assert(agg_ol_ct_arr[i] == agg_ol_cur_ct_arr[i]);
	    if (tmp_agg_idx != fd->hints->cb_nodes - 1)
		fprintf(stderr, ",");
	}
    }
    fprintf(stderr, "\n");
#endif

#ifdef DEBUG2
    for (i = 0; i < nprocs; i++)
    {
	if (agg_ol_ct_arr[i] > 0)
	{
	    fprintf(stderr, "ADIOI_Build_client_reqs: p %d (off,len) = ", i);
	    for (j = 0; j < agg_ol_ct_arr[i]; j++)
	    {
		fprintf(stderr, "[%d](%d,%d) ", j,
			agg_disp_arr[i][j],
			agg_blk_arr[i][j]);
	    }
	    fprintf(stderr, "\n");
	}
    }
#endif

    /* Create all the aggregator MPI_Datatypes */
    for (i = 0; i < nprocs; i++)
    {
	if (agg_comm_sz_arr[i] > 0)
	{
	    MPI_Type_hindexed(agg_ol_ct_arr[i], agg_blk_arr[i],
                              agg_disp_arr[i], MPI_BYTE,
                              &(agg_comm_dtype_arr[i]));
            MPI_Type_commit(&(agg_comm_dtype_arr[i]));
	}
	else
	{
	    agg_comm_dtype_arr[i] = MPI_BYTE;
	}
	ADIOI_Free(agg_blk_arr[i]);
	ADIOI_Free(agg_disp_arr[i]);
    }
    ADIOI_Free(agg_blk_arr);
    ADIOI_Free(agg_disp_arr);

    ADIOI_Free(agg_mem_next_off_arr);
    ADIOI_Free(agg_comm_cur_sz_arr);
    ADIOI_Free(agg_ol_ct_arr);
    ADIOI_Free(agg_ol_cur_ct_arr);
#ifdef AGGREGATION_PROFILE
    MPE_Log_event (5019, 0, NULL);
#endif    
    return 0;
}
/* ADIOI_Build_client_pre_req allows a client to calculate the memtype
 * offset-length pairs up (up to a limit - max_pre_req_sz or max
 * ol_ct). It basically allows ADIOI_Build_client_req to do less work.
 * If it called and there already exist some preprocessed memtype
 * offset-length pairs, it will exit immediately if a limit has been
 * reached or if will add on the old limites to reach the new
 * limits. */

int ADIOI_Build_client_pre_req(ADIO_File fd,
			       int agg_rank, int agg_idx,
			       view_state *my_mem_view_state_p,
			       view_state *agg_file_view_state_p,
			       ADIO_Offset max_pre_req_sz,
			       int max_ol_ct)
{
    ADIO_Offset act_reg_sz = 0, tmp_reg_sz = 0;
    ADIO_Offset cur_off = -1, cur_reg_max_len = -1;
    ADIO_Offset agg_mem_st_reg = 0, agg_mem_act_reg_sz = 0;
    int agg_ol_ct = 0, agg_ol_cur_ct = 0;
    int i, agg_next_off_idx = -1;

    ADIO_Offset cur_sz = 0, max_sz = 0, agg_mem_next_off = -1;
    ADIO_Offset fill_st_reg = -1, fill_reg_sz = -1;
    ADIO_Offset *fr_st_off_arr = fd->file_realm_st_offs;
    MPI_Datatype *fr_type_arr = fd->file_realm_types;
    MPI_Aint *tmp_disp_arr = NULL;
    int *tmp_blk_arr = NULL, exit_loop = -1;
    flatten_state *tmp_mem_state_p = NULL, *tmp_file_state_p = NULL;
#ifdef DTYPE_SKIP
    int skip_type_ct;
#endif
    if (agg_idx < 0 || agg_idx >= fd->hints->cb_nodes)
    {
        fprintf(stderr, "ADIOI_Build_client_pre_req: Invalid agg_idx %d\n",
		agg_idx);
        return -1;
    }

    if (agg_file_view_state_p->cur_state.cur_sz == 
	agg_file_view_state_p->sz || max_pre_req_sz <= 0 ||
	max_ol_ct <= 0)
    {
#ifdef DEBUG1
	fprintf(stderr, 
		"ADIOI_Build_client_pre_req: Nothing to preprocess\n");
#endif
	return 0;
    }

    /* The new limits have already been surpassed by what already
     * exists.  Otherwise we will use the next restrictions */
    if ((my_mem_view_state_p->pre_sz >= max_pre_req_sz) ||
	(my_mem_view_state_p->pre_ol_ct >= max_ol_ct))
    {
#ifdef DEBUG1
	fprintf(stderr, 
		"ADIOI_Build_client_pre_req:  Old values surpass new "
		"pre_req values\n");
#endif
	return 0;
    }
    
    /* General idea is to first advance the filetype to the file realm
     * and then the memtype to the filetype.  The memtype is advanced
     * further by peeking at the filetype and then the filetype is
     * advanced. */
    for (i = 0; i < MAX_OFF_TYPE; i++)
    {
	switch(i)
	{
	    case TEMP_OFF:
		tmp_mem_state_p  = &(my_mem_view_state_p->tmp_state);
		tmp_file_state_p = &(agg_file_view_state_p->tmp_state);
		break;
	    case REAL_OFF:
		tmp_mem_state_p  = &(my_mem_view_state_p->cur_state);
		tmp_file_state_p = &(agg_file_view_state_p->cur_state);
		break;
	    default:
		fprintf(stderr, "ADIOI_Build_client_pre_req: "
			"Invalid off type %d\n", i);
	}

	if (i == TEMP_OFF && my_mem_view_state_p->pre_sz > 0)
	{
	    cur_sz = my_mem_view_state_p->pre_sz;
            agg_ol_ct = my_mem_view_state_p->pre_ol_ct;
	    /* Save the old arrays */
	    tmp_disp_arr = my_mem_view_state_p->pre_disp_arr;
	    tmp_blk_arr  = my_mem_view_state_p->pre_blk_arr;
	    my_mem_view_state_p->pre_disp_arr = NULL;
	    my_mem_view_state_p->pre_blk_arr  = NULL;
            agg_mem_next_off =
		tmp_disp_arr[agg_ol_ct - 1] + tmp_blk_arr[agg_ol_ct - 1];
	}
	else if (i == REAL_OFF && my_mem_view_state_p->pre_sz > 0)
	{
	    cur_sz = my_mem_view_state_p->pre_sz;
	    agg_ol_cur_ct = my_mem_view_state_p->pre_ol_ct;
	    
	    /* Copy the old data to the new data, freeing the old
	     * arrays */
	    memcpy(my_mem_view_state_p->pre_disp_arr, tmp_disp_arr, 
		   my_mem_view_state_p->pre_ol_ct * sizeof(MPI_Aint));
	    memcpy(my_mem_view_state_p->pre_blk_arr, tmp_blk_arr, 
		   my_mem_view_state_p->pre_ol_ct * sizeof(int));

	    ADIOI_Free(tmp_disp_arr);
	    ADIOI_Free(tmp_blk_arr);

	    agg_mem_next_off = 
		my_mem_view_state_p->pre_disp_arr[agg_ol_cur_ct - 1] +
		my_mem_view_state_p->pre_blk_arr[agg_ol_cur_ct - 1];
	}
	else
	{
	    cur_sz = 0;
	}
	
	/* Max_pre_req_sz may be larger than the amount of data left
	 * to preprocess */
	if (max_pre_req_sz - cur_sz > 
	    agg_file_view_state_p->sz - tmp_file_state_p->cur_sz)
	{
	    max_sz = cur_sz +
		agg_file_view_state_p->sz - tmp_file_state_p->cur_sz;
	}
	else
	    max_sz = max_pre_req_sz;
	
	assert(cur_sz != max_sz);
#ifdef DEBUG1
	fprintf(stderr, 
		"ADIOI_Build_client_pre_req: (cur_sz=%Ld,agg_ol_ct=%d,"
		"agg_mem_next_off=%Ld,max_sz=%Ld,max_ol_ct=%d)\n", 
		cur_sz, agg_ol_ct, agg_mem_next_off, max_sz, max_ol_ct);
#endif
	while (cur_sz < max_sz)
	{
	    find_next_off(fd, agg_file_view_state_p,
			  fr_st_off_arr[agg_rank],
			  &(fr_type_arr[agg_rank]),
			  i,
			  &cur_off,
			  &cur_reg_max_len);
	    
	    /* find_next_off may show that the file_view_state is done
	     * even if cur_sz != max_sz since find_next_off may
	     * advance the file_view_state to the end here and realize
	     * that it is done. */
	    if (cur_off == -1)
		break;

	    assert(cur_off != -1);
	    
	    /* Before translating the file regions to memory regions,
	     * we first must advance to the proper point in the
	     * mem_view_state for this aggregator to match the
	     * file_view_state. */
	    while (tmp_file_state_p->cur_sz != tmp_mem_state_p->cur_sz)
	    {
#ifdef DTYPE_SKIP
		if (my_mem_view_state_p->flat_type_p->count > 1) {
		    /* let's see if we can skip whole memory datatypes */
		    skip_type_ct =
			(tmp_file_state_p->cur_sz - tmp_mem_state_p->cur_sz) /
			my_mem_view_state_p->type_sz;
		    if (skip_type_ct > 0) {
			tmp_mem_state_p->cur_sz +=
			    skip_type_ct * my_mem_view_state_p->type_sz;
			tmp_mem_state_p->abs_off +=
			    skip_type_ct * my_mem_view_state_p->ext;
			if (tmp_mem_state_p->cur_sz ==
			    tmp_file_state_p->cur_sz)
			    break;
		    }
		}
#endif
		view_state_add_region(
		    tmp_file_state_p->cur_sz - tmp_mem_state_p->cur_sz,
		    my_mem_view_state_p,
		    &fill_st_reg,
		    &fill_reg_sz, i);
	    }

	    /* Now that the filetype and memtype are advanced to the
	     * same position, add memtype ol-pairs while we have not
	     * overstepped the min(end of the current piece in the
	     * file view, end of the file realm, data left in
	     * max_sz) */
	    
	    if (cur_reg_max_len >  
		view_state_get_next_len(agg_file_view_state_p, i))
		cur_reg_max_len =  
		    view_state_get_next_len(agg_file_view_state_p, i);

	    if (cur_reg_max_len > max_sz - cur_sz)
		cur_reg_max_len = max_sz - cur_sz;

	    assert(cur_reg_max_len > 0);

	    /* Add memtype ol pairs while we have not passed
	     * cur_reg_max_len or the max number of ol pairs
	     * allowed */
	    act_reg_sz = 0;
	    exit_loop = 0;
	    while ((act_reg_sz < cur_reg_max_len) && 
		   (exit_loop == 0))
	    {
		view_state_add_region(
		    cur_reg_max_len - act_reg_sz,
		    my_mem_view_state_p,
		    &agg_mem_st_reg, &agg_mem_act_reg_sz, 
		    i);
		act_reg_sz += agg_mem_act_reg_sz;
		
#ifdef DEBUG2
		fprintf(stderr, "ADIOI_Build_client_pre_req: %s Mem region"
			"(proc=%d,off=%Ld,sz=%Ld)\n",
			off_type_name[i], agg_rank, agg_mem_st_reg, 
			agg_mem_act_reg_sz);
#endif
		switch(i)
		{
		    case TEMP_OFF:
			/* Increment the ol list count if the next
			 * region is not adjacent to the previous
			 * region. */
			if (agg_mem_next_off != agg_mem_st_reg)
			{
			    agg_ol_ct++;
			    if (agg_ol_ct == max_ol_ct)
				exit_loop = 1;
			}
			agg_mem_next_off = 
			    agg_mem_st_reg + agg_mem_act_reg_sz;
			break;
		    case REAL_OFF:
			/* Set the ol list for the memtype that
			 * will map to our aggregator, coaslescing
			 * if possible. */
			agg_next_off_idx = agg_ol_cur_ct;
			if (agg_mem_next_off != agg_mem_st_reg)
			{
			    my_mem_view_state_p->
				pre_disp_arr[agg_next_off_idx] = 
				agg_mem_st_reg;
			    my_mem_view_state_p->
				pre_blk_arr[agg_next_off_idx] = 
				agg_mem_act_reg_sz;
			    agg_ol_cur_ct++;
			    if (agg_ol_cur_ct == agg_ol_ct)
				exit_loop = 1;
			}
			else
			{
			    my_mem_view_state_p->
				pre_blk_arr[agg_next_off_idx - 1]
				+= agg_mem_act_reg_sz;
			}
			agg_mem_next_off = 
			    agg_mem_st_reg + agg_mem_act_reg_sz;
			break;
		    default:
			fprintf(stderr, "ADIOI_Build_client_pre_req: "
				"Impossible type\n");
		}
	    }

	    /* Advance the filetype flatten state appropriately to
	     * match the data advanced in the memtype flatten state.
	     * Should only take at most a single view_state_add_region
	     * call since the memtype cannot proceed beyond the end of
	     * a contig piece in the file type. */
	    view_state_add_region(act_reg_sz - tmp_reg_sz,
				  agg_file_view_state_p,
				  &fill_st_reg, &fill_reg_sz, i);
#ifdef DEBUG2
	    fprintf(stderr, "ADIOI_Build_client_pre_req: %s File region"
		    " (proc=%d,off=%Ld,sz=%Ld)\n",
		    off_type_name[i], agg_rank, fill_st_reg, fill_reg_sz);
#endif
	    if (fill_reg_sz != act_reg_sz)
	    {
		fprintf(stderr, "ADIOI_Build_client_pre_req: "
			"view_state_add_region failed to match the memtype\n");
		return -1;
	    }
	    
	    cur_sz += act_reg_sz;
	}
	
	/* On the first pass, allocate the memory structures for
	 * storing the preprocessed information */
	if (i == TEMP_OFF)
	{
	    if ((my_mem_view_state_p->pre_disp_arr = (MPI_Aint *)
		 ADIOI_Malloc(agg_ol_ct * sizeof(MPI_Aint))) == NULL)
	    {
		fprintf(stderr, "ADIOI_Build_client_pre_req: malloc "
                        "pre_disp_arr of size %ld failed\n",
                        (long int)agg_ol_ct * sizeof(MPI_Aint));
                return -1;
	    }
	    if ((my_mem_view_state_p->pre_blk_arr = (int *) 
		 ADIOI_Malloc(agg_ol_ct * sizeof(int))) == NULL)
	    {
		ADIOI_Free(my_mem_view_state_p->pre_disp_arr);
		fprintf(stderr, "ADIOI_Build_client_pre_req: malloc "
			"agg_blk_arr of size %ld failed\n",
			(long int)agg_ol_ct * sizeof(int));
		return -1;
	    }
	}
    }

    my_mem_view_state_p->pre_sz = cur_sz;
    my_mem_view_state_p->pre_ol_ct = agg_ol_ct;

#ifdef DEBUG1
    fprintf(stderr, "ADIOI_Build_client_pre_req:(agg=%d,cur_ol_count=%d"
	    "=ol_count=%d)\n",
	    agg_rank, my_mem_view_state_p->pre_ol_ct, agg_ol_ct);
#endif

#ifdef DEBUG2
    if (agg_ol_ct > 0)
    {
	fprintf(stderr, "ADIOI_Build_client_pre_req: agg=%d,pre_sz=%Ld "
		"(off,len) = \n", agg_rank, my_mem_view_state_p->pre_sz);
	for (i = 0; i < my_mem_view_state_p->pre_ol_ct; i++)
	{
	    fprintf(stderr, "[%d](%d,%d) ", i, 
		    my_mem_view_state_p->pre_disp_arr[i], 
		    my_mem_view_state_p->pre_blk_arr[i]);
	    if (i % 5 == 0 && i != 0)
		fprintf(stderr, "\n");
	}
	fprintf(stderr, "\n");
    }
#endif

    return 0;
}

/* process_pre_req() allows ADIOI_Build_client_req to use the pre_req
 * information. */

static int process_pre_req(ADIO_File fd,
                           int agg_rank,
                           int agg_idx,
                           view_state *my_mem_view_state_p,
                           view_state *agg_file_view_state_p,
                           ADIO_Offset agg_comm_sz,
			   int off_type,
			   MPI_Aint *agg_disp_arr,
			   int *agg_blk_arr,
			   ADIO_Offset *agg_comm_pre_sz_p,
			   ADIO_Offset *agg_comm_cur_sz_p,
			   ADIO_Offset *agg_comm_sz_p,
			   int *agg_ol_cur_ct_p,
			   int *agg_ol_ct_p,
			   ADIO_Offset *agg_mem_next_off_p)
{
    int i, has_partial = 0;
    MPI_Aint partial_disp = 0;
    int partial_len = 0;
    ADIO_Offset tmp_agg_comm_pre_sz = 0;

    assert (my_mem_view_state_p->pre_sz > 0);
    switch(off_type)
    {
	case TEMP_OFF:
	    /* Use only some of the precalculated data */
	    if (my_mem_view_state_p->pre_sz > *agg_comm_sz_p)
	    {
		for (i = 0; i < my_mem_view_state_p->pre_ol_ct; i++)
		{
		    if ((my_mem_view_state_p->pre_blk_arr[i] + 
			 *agg_comm_pre_sz_p) > *agg_comm_sz_p)
		    {
			has_partial = 1;
			partial_len = *agg_comm_sz_p - *agg_comm_pre_sz_p;
			*agg_comm_pre_sz_p = *agg_comm_sz_p;
			i++;
			break;
		    }
		    else if ((my_mem_view_state_p->pre_blk_arr[i] +
			      *agg_comm_pre_sz_p) == *agg_comm_sz_p)
		    {
			*agg_comm_pre_sz_p += 
			    my_mem_view_state_p->pre_blk_arr[i];
			i++;
			break;
		    }
		    else
			*agg_comm_pre_sz_p += 
			    my_mem_view_state_p->pre_blk_arr[i];
		}
		
		if (has_partial == 1)
		{
		    *agg_mem_next_off_p = 
			my_mem_view_state_p->pre_disp_arr[i - 1] + 
			partial_len;
		}
		else
		{
		    *agg_mem_next_off_p = 
			my_mem_view_state_p->pre_disp_arr[i - 1] + 
			my_mem_view_state_p->pre_blk_arr[i - 1];
		}
		
		*agg_comm_cur_sz_p = *agg_comm_pre_sz_p;
		*agg_ol_ct_p = i;
		
	    }
	    else /* Use all the precalculated data */
	    {
		*agg_comm_pre_sz_p = my_mem_view_state_p->pre_sz;
		*agg_comm_cur_sz_p = *agg_comm_pre_sz_p;
		*agg_ol_ct_p = my_mem_view_state_p->pre_ol_ct;
		*agg_mem_next_off_p = 
		    my_mem_view_state_p->pre_disp_arr[
			my_mem_view_state_p->pre_ol_ct - 1] +
		    my_mem_view_state_p->pre_blk_arr[
			my_mem_view_state_p->pre_ol_ct - 1];
	    }
#ifdef DEBUG1
	    fprintf(stderr, "process_pre_req: TEMP_OFF "
		    "agg_comm_pre_sz=%Ld,agg_comm_cur_sz=%Ld,agg_ol_ct=%d\n",
		    *agg_comm_pre_sz_p, *agg_comm_cur_sz_p, *agg_ol_ct_p);
#endif
	    assert(*agg_comm_cur_sz_p <= *agg_comm_sz_p);
	    break;
	case REAL_OFF:
	    /* Set the ol list for the memtype that will map to our
	     * aggregator, coaslescing if possible. */
	    for (i = 0; i < my_mem_view_state_p->pre_ol_ct; i++)
	    {
		agg_disp_arr[i] = my_mem_view_state_p->pre_disp_arr[i];
		agg_blk_arr[i]  = my_mem_view_state_p->pre_blk_arr[i];
		
		if ((my_mem_view_state_p->pre_blk_arr[i] + 
		     tmp_agg_comm_pre_sz) > *agg_comm_pre_sz_p)
		{
		    has_partial = 1;
		    agg_blk_arr[i] = *agg_comm_pre_sz_p - tmp_agg_comm_pre_sz;
		    tmp_agg_comm_pre_sz = *agg_comm_pre_sz_p;
		    partial_disp = my_mem_view_state_p->pre_disp_arr[i] +
			agg_blk_arr[i];
		    partial_len  = my_mem_view_state_p->pre_blk_arr[i] - 
			agg_blk_arr[i];
		    i++;
		    break;
		}
		else if ((my_mem_view_state_p->pre_blk_arr[i] +
			  tmp_agg_comm_pre_sz) == *agg_comm_pre_sz_p)
		{
		    tmp_agg_comm_pre_sz +=  
			my_mem_view_state_p->pre_blk_arr[i];
		    i++;
		    break;
		}
		else
		    tmp_agg_comm_pre_sz +=
			my_mem_view_state_p->pre_blk_arr[i];
	    }
	    *agg_mem_next_off_p = agg_disp_arr[i - 1] + agg_blk_arr[i - 1];
	    *agg_ol_cur_ct_p = i;
	    *agg_comm_cur_sz_p = *agg_comm_pre_sz_p;
	    
	    /* Clean up the ol pairs we used */	    
	    if ((i < my_mem_view_state_p->pre_ol_ct) || (has_partial == 1))
	    {
		int remain_ol_ct = 
		    my_mem_view_state_p->pre_ol_ct - i + has_partial;
		MPI_Aint *new_pre_disp_arr = NULL;
		int *new_pre_blk_arr = NULL;
		
		if ((new_pre_disp_arr = (MPI_Aint *)
		     ADIOI_Malloc(remain_ol_ct * sizeof(MPI_Aint))) == NULL)
		{
		    fprintf(stderr, "process_pre_req: malloc "
			    "new_pre_disp_arr failed\n");
		    return -1;
		}
		if ((new_pre_blk_arr = (int *)
		     ADIOI_Malloc(remain_ol_ct * sizeof(int))) == NULL)
                {
                    fprintf(stderr, "process_pre_req: malloc "
                            "new_pre_blk_arr failed\n");
                    return -1;
                }
		
		memcpy(new_pre_disp_arr, 
		       &(my_mem_view_state_p->pre_disp_arr[i - has_partial]),
		       remain_ol_ct * sizeof(MPI_Aint));
		memcpy(new_pre_blk_arr, 
		       &(my_mem_view_state_p->pre_blk_arr[i - has_partial]),
		       remain_ol_ct * sizeof(int));
		
		/* Set the partial len of the first piece */
		if (has_partial == 1)
		{
		    /* new_pre_disp_arr[remain_ol_ct - 1] = partial_disp;
		       new_pre_blk_arr[remain_ol_ct - 1]  = partial_len; */
		    new_pre_disp_arr[0] = partial_disp;
		    new_pre_blk_arr[0]  = partial_len;
		}
		
		ADIOI_Free(my_mem_view_state_p->pre_disp_arr);
		ADIOI_Free(my_mem_view_state_p->pre_blk_arr);
		
		my_mem_view_state_p->pre_disp_arr = new_pre_disp_arr;
		my_mem_view_state_p->pre_blk_arr  = new_pre_blk_arr;
		my_mem_view_state_p->pre_ol_ct = remain_ol_ct;
		my_mem_view_state_p->pre_sz -= *agg_comm_pre_sz_p;
	    }
	    else /* Used all the precalculated ol pairs */
	    {
		ADIOI_Free(my_mem_view_state_p->pre_disp_arr);
		ADIOI_Free(my_mem_view_state_p->pre_blk_arr);
		
		my_mem_view_state_p->pre_disp_arr = NULL;
		my_mem_view_state_p->pre_blk_arr = NULL;
		my_mem_view_state_p->pre_ol_ct = 0;
		my_mem_view_state_p->pre_sz = 0;
	    }
#ifdef DEBUG1
	    fprintf(stderr, "process_pre_req: REAL_OFF "
		    "agg_comm_pre_sz=%Ld,agg_comm_cur_sz=%Ld,agg_ol_ct=%d,"
		    "agg_ol_cur_ct=%d\n",
		    *agg_comm_pre_sz_p, *agg_comm_cur_sz_p, *agg_ol_ct_p, 
		    *agg_ol_cur_ct_p);
#endif
	    break;
	default:
	    fprintf(stderr, "process_pre_req: Invalid off_type %d\n",
		    off_type);
    }
    return 0;
}

/* ADIOI_Build_client_req() creates a memory datatype to transfer data
 * to/from a particular aggregator. */

int ADIOI_Build_client_req(ADIO_File fd,
			   int agg_rank,
			   int agg_idx,
			   view_state *my_mem_view_state_p,
			   view_state *agg_file_view_state_p,
			   ADIO_Offset agg_comm_sz,
			   MPI_Datatype *agg_comm_dtype_p)
{
    MPI_Aint *agg_disp_arr = NULL;
    int *agg_blk_arr = NULL;
    ADIO_Offset st_reg = 0, act_reg_sz = 0, tmp_reg_sz = 0;
    ADIO_Offset cur_off = -1, cur_reg_max_len = -1;
    ADIO_Offset agg_mem_st_reg = 0, agg_mem_act_reg_sz = 0;
    int agg_ol_ct = 0, agg_ol_cur_ct = 0;
    int i = 0, agg_next_off_idx = -1;
    ADIO_Offset agg_mem_next_off = 0, agg_comm_cur_sz = 0, agg_comm_pre_sz = 0;
    ADIO_Offset *fr_st_off_arr = fd->file_realm_st_offs;
    MPI_Datatype *fr_type_arr = fd->file_realm_types;
    flatten_state *tmp_mem_state_p = NULL, *tmp_file_state_p = NULL;
#ifdef DTYPE_SKIP
    int skip_type_ct;
#endif

    if (agg_idx < 0 || agg_idx >= fd->hints->cb_nodes)
    {
#ifdef DEBUG1
	fprintf(stderr, "ADIOI_Build_client_req: agg_rank %d does not map "
		"to a valid node in cb_node\n", agg_rank);
#endif
	return 0;
    }

#ifdef AGGREGATION_PROFILE
    MPE_Log_event (5018, 0, NULL);
#endif

#ifdef DEBUG1
    fprintf(stderr, "ADIOI_Build_client_req:(agg=%d,size_req=%Ld)\n",
	    agg_idx, agg_comm_sz);
#endif
    
    /* On the first pass see how many offset-length pairs are
     * necessary for each aggregator.  Then allocate the correct
     * amount of offset-length pairs for handling each aggregator's
     * particular data size.  On the last pass, we actually create the
     * offset-length pairs. */
    for (i = 0; i < MAX_OFF_TYPE; i++)
    {
	switch(i)
	{
	    case TEMP_OFF:
		tmp_mem_state_p  = &(my_mem_view_state_p->tmp_state);
		tmp_file_state_p = &(agg_file_view_state_p->tmp_state);
		break;
	    case REAL_OFF:
		tmp_mem_state_p  = &(my_mem_view_state_p->cur_state);
		tmp_file_state_p = &(agg_file_view_state_p->cur_state);
		break;
	    default:
		fprintf(stderr, "ADIOI_Build_client_pre_req: "
			"Invalid off type %d\n", i);
	}

	agg_comm_cur_sz = 0;
	agg_mem_next_off = -1;

	/* First try to preprocess anything we can */
	if (my_mem_view_state_p->pre_sz > 0)
	{
	    process_pre_req(fd,
			    agg_rank,
			    agg_idx,
			    my_mem_view_state_p,
			    agg_file_view_state_p,
			    agg_comm_sz,
			    i,
			    agg_disp_arr,
			    agg_blk_arr,
			    &agg_comm_pre_sz,
			    &agg_comm_cur_sz,
			    &agg_comm_sz,
			    &agg_ol_cur_ct,
			    &agg_ol_ct,
			    &agg_mem_next_off);
	}
	
	while (agg_comm_cur_sz < agg_comm_sz)
	{	
	    find_next_off(fd, agg_file_view_state_p,
			  fr_st_off_arr[agg_idx],
			  &(fr_type_arr[agg_idx]),
			  i,
			  &cur_off,
			  &cur_reg_max_len);
	    
	    assert(cur_off != -1);
	    
	    /* Add up to the end of the file realm or as many bytes
	     * are left for this particular aggregator in the client's
	     * filetype */
	    if (cur_reg_max_len > (agg_comm_sz - agg_comm_cur_sz))
	    {
		cur_reg_max_len = agg_comm_sz - agg_comm_cur_sz;
	    }
	    assert(cur_reg_max_len > 0);
	
	    view_state_add_region(
		cur_reg_max_len,
		agg_file_view_state_p,
		&st_reg, &act_reg_sz, i);
	    
#ifdef DEBUG2
	    fprintf(stderr, "ADIOI_Build_client_req: %s File region"
		    " (proc=%d,off=%Ld,sz=%Ld)\n",
		    off_type_name[i], agg_rank, cur_off, act_reg_sz);
#endif
	    
	    /* Before translating the file regions to memory regions,
	     * we first must advance to the proper point in the
	     * mem_view_state for this aggregator to match the
	     * file_view_state. */
	    
	    assert(tmp_file_state_p->cur_sz - act_reg_sz >= 
		   tmp_mem_state_p->cur_sz);
	    
	    while (tmp_file_state_p->cur_sz - act_reg_sz != 
		   tmp_mem_state_p->cur_sz)
	    {
		ADIO_Offset fill_st_reg = -1, fill_reg_sz = -1;
#ifdef DTYPE_SKIP
		if (my_mem_view_state_p->flat_type_p->count > 1) {
		    /* let's see if we can skip whole memory datatypes */
		    skip_type_ct =
			(tmp_file_state_p->cur_sz - act_reg_sz -
			 tmp_mem_state_p->cur_sz) /
			my_mem_view_state_p->type_sz;
		    if (skip_type_ct > 0) {
			tmp_mem_state_p->cur_sz +=
			    skip_type_ct * my_mem_view_state_p->type_sz;
			tmp_mem_state_p->abs_off +=
			    skip_type_ct * my_mem_view_state_p->ext;
			if ((tmp_mem_state_p->cur_sz - act_reg_sz) ==
			    tmp_file_state_p->cur_sz)
			    break;
		    }
		}
#endif
		view_state_add_region(
		    tmp_file_state_p->cur_sz - 
		    act_reg_sz - tmp_mem_state_p->cur_sz,
		    my_mem_view_state_p,
		    &fill_st_reg,
		    &fill_reg_sz, i);
	    }
	    
	    /* Based on how large the act_reg_sz is, first figure
	     * out how many memory offset-length pairs are
	     * necessary and then set the offset-length pairs. */
	    tmp_reg_sz = 0;
	    while (tmp_reg_sz != act_reg_sz)
	    {
		view_state_add_region(
		    act_reg_sz - tmp_reg_sz,
		    my_mem_view_state_p,
		    &agg_mem_st_reg, &agg_mem_act_reg_sz, 
		    i);
		tmp_reg_sz += agg_mem_act_reg_sz;
		
#ifdef DEBUG2
		fprintf(stderr, "ADIOI_Build_client_req: %s Mem region"
			"(off=%Ld,sz=%Ld)\n",
			off_type_name[i], agg_mem_st_reg, 
			agg_mem_act_reg_sz);
#endif
		agg_comm_cur_sz += agg_mem_act_reg_sz;
		switch(i)
		{
		    case TEMP_OFF:
			/* Increment the ol list count if the next
			 * region is not adjacent to the previous
			 * region. */
			if (agg_mem_next_off != agg_mem_st_reg)
			{
			    agg_ol_ct++;
			}
			agg_mem_next_off = 
			    agg_mem_st_reg + agg_mem_act_reg_sz;
			break;
		    case REAL_OFF:
			/* Set the ol list for the memtype that
			 * will map to our aggregator, coaslescing
			 * if possible. */
			agg_next_off_idx = agg_ol_cur_ct;
			if (agg_mem_next_off != agg_mem_st_reg)
			{
			    agg_disp_arr[agg_next_off_idx] = 
				agg_mem_st_reg;
			    agg_blk_arr[agg_next_off_idx] = 
				agg_mem_act_reg_sz;
			    agg_ol_cur_ct++;
			}
			else
			{
			    agg_blk_arr[agg_next_off_idx - 1]
				+= agg_mem_act_reg_sz;
			}
			agg_mem_next_off = 
			    agg_mem_st_reg + agg_mem_act_reg_sz;
			break;
		    default:
			fprintf(stderr, "ADIOI_Build_client_req: "
				"Impossible type\n");
		}
	    }
	}
	
	/* On the first pass, allocate the memory structures for
	 * creating the MPI_hindexed type. */
	if (i == TEMP_OFF)
	{	    
	    /* Allocate offset-length pairs for creating hindexed
	     * MPI_Datatypes for each aggregator */
	    if ((agg_disp_arr = (MPI_Aint *) 
		 ADIOI_Malloc(agg_ol_ct * sizeof(MPI_Aint))) == NULL)
	    {
		fprintf(stderr, "ADIOI_Build_client_req: malloc "
			"agg_disp_arr of size %ld failed\n",
			(long int)agg_ol_ct * sizeof(MPI_Aint));
		return -1;
	    }
	    if ((agg_blk_arr = (int *) 
		 ADIOI_Malloc(agg_ol_ct * sizeof(int))) == NULL)
	    {
		ADIOI_Free(agg_disp_arr);
		fprintf(stderr, "ADIOI_Build_client_req: malloc "
			"agg_blk_arr of size %ld failed\n",
			(long int)agg_ol_ct * sizeof(int));
		return -1;
	    }
	}
    }

    assert(agg_ol_ct == agg_ol_cur_ct);
#ifdef DEBUG1
    fprintf(stderr, 
	    "ADIOI_Build_client_req:(agg=%d,cur_ol_count=%d=ol_count=%d)\n",
	    agg_rank, agg_ol_cur_ct, agg_ol_ct);
#endif

#ifdef DEBUG2
    if (agg_ol_ct > 0)
    {
	fprintf(stderr, "ADIOI_Build_client_req: p %d (off,len) = ", agg_rank);
	for (i = 0; i < agg_ol_ct; i++)
	{
	    fprintf(stderr, "[%d](%d,%d) ", i, 
		    agg_disp_arr[i], agg_blk_arr[i]);
	    if (i % 5 == 0 && i != 0)
		fprintf(stderr, "\n");
	}
	fprintf(stderr, "\n");
    }
#endif
#ifdef DEBUG1
    fprintf(stderr, 
	    "ADIOI_Build_client_req:(agg=%d,pre_ol_count=%d)\n",
	    agg_idx, my_mem_view_state_p->pre_ol_ct);
#endif

#ifdef DEBUG2
    if (my_mem_view_state_p->pre_sz > 0)
    {
	fprintf(stderr, "ADIOI_Build_client_req: p %d pre(off,len) = ", 
		agg_idx);
	for (i = 0; i < my_mem_view_state_p->pre_ol_ct; i++)
	{
	    fprintf(stderr, "[%d](%d,%d) ", i, 
		    my_mem_view_state_p->pre_disp_arr[i], 
		    my_mem_view_state_p->pre_blk_arr[i]);
	    if (i % 5 == 0 && i != 0)
		fprintf(stderr, "\n");
	}
	fprintf(stderr, "\n");
    }
#endif

    /* Create the aggregator MPI_Datatype */
    if (agg_comm_sz > 0)
    {
	MPI_Type_hindexed(agg_ol_ct, agg_blk_arr, agg_disp_arr, MPI_BYTE,
			  agg_comm_dtype_p);
	MPI_Type_commit(agg_comm_dtype_p);
    }
    else
    {
	*agg_comm_dtype_p = MPI_BYTE;
    }

    ADIOI_Free(agg_blk_arr);
    ADIOI_Free(agg_disp_arr);

#ifdef AGGREGATION_PROFILE
    MPE_Log_event (5019, 0, NULL);
#endif    
    return 0;
}


