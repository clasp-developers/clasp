/* -*- Mode: C; c-basic-offset:4 ; -*-
 * vim: ts=8 sts=4 sw=4 noexpandtab
 *
 *   Copyright (C) 2006 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include <assert.h>
#include "adio.h"
#include "adio_extern.h"
#include "ad_pvfs2.h"
#include "ad_pvfs2_io.h"
#include "ad_pvfs2_common.h"

int ADIOI_PVFS2_StridedDtypeIO(ADIO_File fd, void *buf, int count,
			       MPI_Datatype datatype, int file_ptr_type,
			       ADIO_Offset offset, ADIO_Status *status, int
			       *error_code,
			       int rw_type)
{
    int filetype_size = -1, ret = -1, filetype_is_contig = -1;
    int num_filetypes = 0, cur_flat_file_reg_off = 0;
    PVFS_Request tmp_mem_req, mem_req, tmp_file_req, file_req;
    PVFS_sysresp_io resp_io;
    ADIO_Offset off = -1, bytes_into_filetype = 0;
    MPI_Aint filetype_extent = -1;
    int etype_size = -1, i = -1;
    PVFS_size pvfs_disp = -1;
    ADIOI_Flatlist_node *flat_file_p = ADIOI_Flatlist;

    /* Use for offseting the PVFS2 filetype */
    int pvfs_blk = 1;
    ADIOI_PVFS2_fs *pvfs_fs;
    static char myname[] = "ADIOI_PVFS2_STRIDED_DTYPE";

    memset(&tmp_mem_req, 0, sizeof(PVFS_Request));
    memset(&mem_req, 0, sizeof(PVFS_Request));
    memset(&tmp_file_req, 0, sizeof(PVFS_Request));
    memset(&file_req, 0, sizeof(PVFS_Request));

    pvfs_fs = (ADIOI_PVFS2_fs*)fd->fs_ptr;

    ADIOI_Datatype_iscontig(fd->filetype, &filetype_is_contig);

    /* changed below if error */
    *error_code = MPI_SUCCESS;  

    /* datatype is the memory type 
     * fd->filetype is the file type */
    MPI_Type_size(fd->filetype, &filetype_size);
    if (filetype_size == 0) {
        *error_code = MPI_SUCCESS;
        return -1;
    }
    MPI_Type_extent(fd->filetype, &filetype_extent);
    MPI_Type_size(fd->etype, &etype_size);
    if (filetype_size == 0) {
        *error_code = MPI_SUCCESS;
        return -1;
    }

    /* offset is in units of etype relative to the filetype.  We
     * convert this to off in terms of actual data bytes (the offset
     * minus the number of bytes that are not used).  We are allowed
     * to do this since PVFS2 handles offsets with respect to a
     * file_req in bytes, otherwise we would have to convert into a
     * pure byte offset as is done in other methods.  Explicit offset
     * case is handled by using fd->disp and byte-converted off. */

    pvfs_disp = fd->disp;
    if (file_ptr_type == ADIO_INDIVIDUAL) 
    {
	if (filetype_is_contig) 
	{
	    off = fd->fp_ind - fd->disp;
	}
	else 
	{
	    int flag = 0;
	    /* Should have already been flattened in ADIO_Open*/
	    while (flat_file_p->type != fd->filetype) 
	    {
		flat_file_p = flat_file_p->next;
	    }
	    num_filetypes = -1;
	    while (!flag) 
	    {
		num_filetypes++;
		for (i = 0; i < flat_file_p->count; i++) 
		{
		    /* Start on a non zero-length region */
		    if (flat_file_p->blocklens[i]) 
		    {
			if (fd->disp + flat_file_p->indices[i] +
			    (num_filetypes * filetype_extent) +
			    flat_file_p->blocklens[i] > fd->fp_ind &&
			    fd->disp + flat_file_p->indices[i] <= 
			    fd->fp_ind) 
			{
			    cur_flat_file_reg_off = fd->fp_ind -
				(fd->disp + flat_file_p->indices[i] +
				 (num_filetypes * filetype_extent));
			    flag = 1;
			    break;
			}
			else
			    bytes_into_filetype += flat_file_p->blocklens[i];
		    }
		}
	    }
	    /* Impossible that we don't find it in this datatype */
	    assert(i != flat_file_p->count);
	    off = bytes_into_filetype + cur_flat_file_reg_off;
	}
    }
    else /* ADIO_EXPLICIT */
    { 
	off = etype_size * offset;
    }

#ifdef DEBUG_DTYPE
    fprintf(stderr, "ADIOI_PVFS2_StridedDtypeIO: (fd->fp_ind=%Ld,fd->disp=%Ld,"
	    " offset=%Ld),(pvfs_disp=%Ld,off=%Ld)\n",
	    fd->fp_ind, fd->disp, offset, pvfs_disp, off);
#endif


    /* Convert the MPI memory and file datatypes into
     * PVFS2 datatypes */
    ret = convert_mpi_pvfs2_dtype(&datatype, &tmp_mem_req);
    if (ret < 0)
    {
	goto error_state;
    }
    ret = convert_mpi_pvfs2_dtype(&(fd->filetype), &tmp_file_req);
    if (ret < 0)
    {
	goto error_state;
    }

    ret = PVFS_Request_contiguous(count, tmp_mem_req, &mem_req);
    if (ret != 0) /* TODO: convert this to MPIO error handling */
        fprintf(stderr, "ADIOI_PVFS2_stridedDtypeIO: error in final"
		" CONTIG memory type\n");
    PVFS_Request_free(&tmp_mem_req);    

    /* pvfs_disp is used to offset the filetype */
    ret = PVFS_Request_hindexed(1, &pvfs_blk, &pvfs_disp,
                                tmp_file_req, &file_req);
    if (ret != 0)
        fprintf(stderr, "ADIOI_PVFS2_StridedDtypeIO: error in final"
			" HINDEXED file type\n");
    PVFS_Request_free(&tmp_file_req);

    if (rw_type == READ)
	ret = PVFS_sys_read(pvfs_fs->object_ref, file_req, off, buf,
			    mem_req, &(pvfs_fs->credentials), &resp_io);
    else
	ret = PVFS_sys_write(pvfs_fs->object_ref, file_req, off, buf,
			     mem_req, &(pvfs_fs->credentials), &resp_io);

    if (ret != 0) {
	fprintf(stderr, "ADIOI_PVFS2_StridedDtypeIO: Warning - PVFS_sys_"
		"read/write returned %d and completed %Ld bytes.\n", 
		ret, resp_io.total_completed);
        *error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                           MPIR_ERR_RECOVERABLE,
                                           myname, __LINE__,
                                           ADIOI_PVFS2_error_convert(ret),
                                           "Error in PVFS_sys_io \n", 0);
        goto error_state;
    }

    if (file_ptr_type == ADIO_INDIVIDUAL)
    {
        fd->fp_ind = off += resp_io.total_completed;
    }
    
  error_state:
    fd->fp_sys_posn = -1;   /* set it to null. */

    PVFS_Request_free(&mem_req);
    PVFS_Request_free(&file_req);    

#ifdef DEBUG_DTYPE
    fprintf(stderr, "ADIOI_PVFS2_StridedDtypeIO: "
            "resp_io.total_completed=%Ld,ret=%d\n", 
	    resp_io.total_completed, ret);
#endif

#ifdef HAVE_STATUS_SET_BYTES
    MPIR_Status_set_bytes(status, datatype, (int)resp_io.total_completed);
    /* This is a temporary way of filling in status. The right way is to
     * keep track of how much data was actually acccessed by 
     * ADIOI_BUFFERED operations */
#endif
    return ret;
}

/* convert_mpi_pvfs2_dtype - Convert a MPI datatype into
 * a PVFS2 datatype so that we can natively use the PVFS2 
 * datatypes in the PVFS2 I/O calls instead of converting 
 * all datatypes to the hindexed method 
 * return 1  - a leaf node
 * return 0  - normal return 
 * return -1 - problems */

int convert_mpi_pvfs2_dtype(MPI_Datatype *mpi_dtype, 
			    PVFS_Request *pvfs_dtype)
{
    int num_int = -1, num_addr = -1, num_dtype = -1, 
	combiner = -1, i = -1, ret = -1, leaf = -1;
    int *arr_int = NULL, *arr_addr = NULL;
    MPI_Datatype *arr_dtype = NULL;
    PVFS_Request *old_pvfs_dtype = NULL;
    PVFS_Request *old_pvfs_dtype_arr = NULL;
    int arr_count = -1;
    PVFS_size *pvfs_arr_disp = NULL;
    int *pvfs_arr_len = NULL;

    MPI_Type_get_envelope(*mpi_dtype,
			  &num_int,
			  &num_addr,
			  &num_dtype,
			  &combiner);

    /* Depending on type of datatype do the following 
     * operations */
    
    if (combiner == MPI_COMBINER_NAMED)
    {
	convert_named(mpi_dtype, pvfs_dtype, combiner);
	return 1;
    }

    /* Allocate space for the arrays necessary for 
     * MPI_Type_get_contents */

    if ((arr_int = ADIOI_Malloc(sizeof(int)*num_int)) == NULL)
    {
	fprintf(stderr, "Failed to allocate array_int\n");
	return -1;
    }
    if ((arr_addr = ADIOI_Malloc(sizeof(int)*num_addr)) == NULL)
    {
	ADIOI_Free(arr_int);
	fprintf(stderr, "Failed to allocate array_addr\n");
	return -1;
    }
    if ((arr_dtype = ADIOI_Malloc(sizeof(MPI_Datatype)*num_dtype)) == NULL)
    {
	ADIOI_Free(arr_int);
	ADIOI_Free(arr_addr);
	fprintf(stderr, "Failed to allocate array_dtypes\n");
	return -1;
    }

    MPI_Type_get_contents(*mpi_dtype,
			  num_int,
			  num_addr,
			  num_dtype,
			  arr_int,
			  arr_addr,
			  arr_dtype);

    /* If it's not a predefined datatype, it is either a 
     * derived datatype or a structured datatype */

    if (combiner != MPI_COMBINER_STRUCT)
    {
	if ((old_pvfs_dtype = ADIOI_Malloc(sizeof(PVFS_Request))) == NULL)
	    fprintf(stderr, "convert_mpi_pvfs2_dtype: "
		    "Failed to allocate PVFS_Request\n");
	switch (combiner)
	{
	    case MPI_COMBINER_CONTIGUOUS:
		leaf = convert_mpi_pvfs2_dtype(&arr_dtype[0], old_pvfs_dtype);
		ret = PVFS_Request_contiguous(arr_int[0], 
					      *old_pvfs_dtype, pvfs_dtype);
		break;
	    case MPI_COMBINER_VECTOR:
		leaf = convert_mpi_pvfs2_dtype(&arr_dtype[0], old_pvfs_dtype);
		ret = PVFS_Request_vector(arr_int[0], arr_int[1],
					  arr_int[2], *old_pvfs_dtype, 
					  pvfs_dtype);
		break;
	    case MPI_COMBINER_HVECTOR:
		leaf = convert_mpi_pvfs2_dtype(&arr_dtype[0], old_pvfs_dtype);
		ret = PVFS_Request_hvector(arr_int[0], arr_int[1],
					   arr_addr[0], *old_pvfs_dtype, 
					   pvfs_dtype);
		break;
		/* Both INDEXED and HINDEXED types require PVFS_size 
		 * address arrays.  Therefore, we need to copy and 
		 * convert the data from MPI_get_contents() into 
		 * a PVFS_size buffer */
	    case MPI_COMBINER_INDEXED:
		leaf = convert_mpi_pvfs2_dtype(&arr_dtype[0], old_pvfs_dtype);
		if ((pvfs_arr_disp = 
			    ADIOI_Malloc(arr_int[0]*sizeof(PVFS_size))) == 0)
		{
		    fprintf(stderr, "convert_mpi_pvfs2_dtype: "
			    "Failed to allocate pvfs_arr_disp\n");
		}
		for (i = 0; i < arr_int[0]; i++)
		{
		    pvfs_arr_disp[i] = 
			(PVFS_size) arr_int[arr_int[0]+1+i];
		}
		ret = PVFS_Request_indexed(arr_int[0], &arr_int[1], 
				     pvfs_arr_disp,
				     *old_pvfs_dtype, pvfs_dtype);
		ADIOI_Free(pvfs_arr_disp);
		break;
	    case MPI_COMBINER_HINDEXED:
		leaf = convert_mpi_pvfs2_dtype(&arr_dtype[0], old_pvfs_dtype);
		if ((pvfs_arr_disp = 
			    ADIOI_Malloc(arr_int[0]*sizeof(PVFS_size))) == 0)
		{
		    fprintf(stderr, "convert_mpi_pvfs2_dtype: "
			    "Failed to allocate pvfs_arr_disp\n");
		}
		for (i = 0; i < arr_int[0]; i++)
		{
		    pvfs_arr_disp[i] = 
			(PVFS_size) arr_addr[i];
		}
		ret = PVFS_Request_hindexed(arr_int[0], &arr_int[1], 
				      (int64_t *)&arr_addr[0],
				      *old_pvfs_dtype, pvfs_dtype);
		ADIOI_Free(pvfs_arr_disp);		
		break;
	    case MPI_COMBINER_DUP:
                leaf = convert_mpi_pvfs2_dtype(&arr_dtype[0], old_pvfs_dtype);
		ret = PVFS_Request_contiguous(1, 
					      *old_pvfs_dtype, pvfs_dtype);

                break;
	    case MPI_COMBINER_INDEXED_BLOCK:
		/* No native PVFS2 support for this operation currently */
		ADIOI_Free(old_pvfs_dtype);
		fprintf(stderr, "convert_mpi_pvfs2_dtype: "
			"INDEXED_BLOCK is unsupported\n"); 
		break;
	    case MPI_COMBINER_HINDEXED_INTEGER:
		ADIOI_Free(old_pvfs_dtype);
		fprintf(stderr, "convert_mpi_pvfs2_dtype: "
			"HINDEXED_INTEGER is unsupported\n"); 
		break;
	    case MPI_COMBINER_STRUCT_INTEGER:
		ADIOI_Free(old_pvfs_dtype);
		fprintf(stderr, "convert_mpi_pvfs2_dtype: "
			"STRUCT_INTEGER is unsupported\n"); 
		break;
	    case MPI_COMBINER_SUBARRAY:
		ADIOI_Free(old_pvfs_dtype);
		fprintf(stderr, "convert_mpi_pvfs2_dtype: "
			"SUBARRAY is unsupported\n"); 
		break;
	    case MPI_COMBINER_DARRAY:
		ADIOI_Free(old_pvfs_dtype);
		fprintf(stderr, "convert_mpi_pvfs2_dtype: "
			"DARRAY is unsupported\n"); 
		break;
	    case MPI_COMBINER_F90_REAL:
		ADIOI_Free(old_pvfs_dtype);
		fprintf(stderr, "convert_mpi_pvfs2_dtype: "
			"F90_REAL is unsupported\n"); 
		break;
	    case MPI_COMBINER_F90_COMPLEX:
		ADIOI_Free(old_pvfs_dtype);
		fprintf(stderr, "convert_mpi_pvfs2_dtype: "
			"F90_COMPLEX is unsupported\n"); 
		break;
	    case MPI_COMBINER_F90_INTEGER:
		ADIOI_Free(old_pvfs_dtype);
		fprintf(stderr, "convert_mpi_pvfs2_dtype: "
			"F90_INTEGER is unsupported\n"); 
		break;
	    case MPI_COMBINER_RESIZED:
		ADIOI_Free(old_pvfs_dtype);
		fprintf(stderr, "convert_mpi_pvfs2_dtype: "
			"RESIZED is unsupported\n"); 
		break;
	    default:
		break;
	}

	if (ret != 0)
	    fprintf(stderr, "Error in PVFS_Request_* "
		    "for a derived datatype\n");

#ifdef DEBUG_DTYPE
	print_dtype_info(combiner,    
			 num_int,
			 num_addr,
			 num_dtype,
			 arr_int,
			 arr_addr,
			 arr_dtype);
#endif

	if (leaf != 1 && combiner != MPI_COMBINER_DUP)
	    MPI_Type_free(&arr_dtype[0]);

	ADIOI_Free(arr_int);
	ADIOI_Free(arr_addr);
	ADIOI_Free(arr_dtype);

	PVFS_Request_free(old_pvfs_dtype);
	ADIOI_Free(old_pvfs_dtype);
	
	return ret;
    }
    else /* MPI_COMBINER_STRUCT */
    {
	MPI_Aint mpi_lb = -1, mpi_extent = -1;
	PVFS_offset pvfs_lb = -1;
	PVFS_size pvfs_extent = -1;
	int has_lb_ub = 0;

	/* When converting into a PVFS_Request_struct, we no longer
	 * can use MPI_LB and MPI_UB.  Therfore, we have to do the
	 * following.  
	 * We simply ignore all the MPI_LB and MPI_UB types and 
	 * get the lb and extent and pass it on through a 
	 * PVFS resized_req */

	arr_count = 0;
	for (i = 0; i < arr_int[0]; i++)
	{
	    if (arr_dtype[i] != MPI_LB &&
		arr_dtype[i] != MPI_UB)
	    {
		arr_count++;
	    }
	}

	if (arr_int[0] != arr_count)
	{
	    MPI_Type_get_extent(*mpi_dtype, &mpi_lb, &mpi_extent);
	    pvfs_lb = mpi_lb;
	    pvfs_extent = mpi_extent;
	    if ((pvfs_arr_len = ADIOI_Malloc(arr_count*sizeof(int))) 
		== NULL)
	    {
		fprintf(stderr, "convert_mpi_pvfs2_dtype: "
			"Failed to allocate pvfs_arr_len\n");
	    }
	    has_lb_ub = 1;
	}

	if ((old_pvfs_dtype_arr
	     = ADIOI_Malloc(arr_count*sizeof(PVFS_Request))) == NULL)
	    fprintf(stderr, "convert_mpi_pvfs2_dtype: "
		    "Failed to allocate PVFS_Requests\n");

	if ((pvfs_arr_disp = ADIOI_Malloc(arr_count*sizeof(PVFS_size))) 
	    == NULL)
	{
	    fprintf(stderr, "convert_mpi_pvfs2_dtype: "
		    "Failed to allocate pvfs_arr_disp\n");
	}

	arr_count = 0;
	for (i = 0; i < arr_int[0]; i++)
	{
	    if (arr_dtype[i] != MPI_LB &&
		arr_dtype[i] != MPI_UB)
	    {
		leaf = convert_mpi_pvfs2_dtype(
		    &arr_dtype[i], &old_pvfs_dtype_arr[arr_count]);
		if (leaf != 1)
		    MPI_Type_free(&arr_dtype[i]); 
		pvfs_arr_disp[arr_count] = 
		    (PVFS_size) arr_addr[i];
		if (has_lb_ub)
		{
		    pvfs_arr_len[arr_count] = 
			arr_int[i+1];
		}
		arr_count++;
	    }
	}

	/* If a MPI_UB or MPI_LB did exist, we have to
	 * resize the datatype */
	if (has_lb_ub)
	{
	    PVFS_Request *tmp_pvfs_dtype = NULL;
	    if ((tmp_pvfs_dtype = ADIOI_Malloc(sizeof(PVFS_Request))) == NULL)
		fprintf(stderr, "convert_mpi_pvfs2_dtype: "
			"Failed to allocate PVFS_Request\n");
	    
	    ret = PVFS_Request_struct(arr_count, pvfs_arr_len, 
				      pvfs_arr_disp,
				      old_pvfs_dtype_arr, tmp_pvfs_dtype);
	    if (ret != 0)
		fprintf(stderr, "Error in PVFS_Request_struct\n");

	    arr_count = 0;
	    for (i = 0; i < arr_int[0]; i++)
	    {
		if (arr_dtype[i] != MPI_LB &&
		    arr_dtype[i] != MPI_UB)
		{
		    PVFS_Request_free(&old_pvfs_dtype_arr[arr_count]);
		    arr_count++;
		}
	    }
	    
#ifdef DEBUG_DTYPE
	    fprintf(stderr, "STRUCT(WITHOUT %d LB or UB)(%d,[",
		    arr_int[0] - arr_count, arr_count);
	    for (i = 0; i < arr_count; i++)
		fprintf(stderr, "(%d,%Ld) ",
			pvfs_arr_len[i],
			pvfs_arr_disp[i]);
	    fprintf(stderr, "]\n");
	    fprintf(stderr, "RESIZED(LB = %Ld, EXTENT = %Ld)\n",
		    pvfs_lb, pvfs_extent);
#endif 
	    ret = PVFS_Request_resized(*tmp_pvfs_dtype, 
				       pvfs_lb, pvfs_extent, pvfs_dtype);
	    if (ret != 0)
		fprintf(stderr, "Error in PVFS_Request_resize\n");

	    PVFS_Request_free(tmp_pvfs_dtype);
	    ADIOI_Free(tmp_pvfs_dtype);
	}
	else /* No MPI_LB or MPI_UB datatypes */
	{
	    ret = PVFS_Request_struct(arr_int[0], &arr_int[1], 
				      pvfs_arr_disp,
				      old_pvfs_dtype_arr, pvfs_dtype);
	    if (ret != 0)
		fprintf(stderr, "Error in PVFS_Request_struct\n");

	    for (i = 0; i < arr_int[0]; i++)
	    {
		if (arr_dtype[i] != MPI_LB &&
		    arr_dtype[i] != MPI_UB)
		    PVFS_Request_free(&old_pvfs_dtype_arr[i]);
	    }

#ifdef DEBUG_DTYPE
	    print_dtype_info(combiner,    
			     num_int,
			     num_addr,
			     num_dtype,
			     arr_int,
			     arr_addr,
			     arr_dtype);
#endif 
	}

	ADIOI_Free(arr_int);
	ADIOI_Free(arr_addr);
	ADIOI_Free(arr_dtype);

	ADIOI_Free(old_pvfs_dtype_arr);
	ADIOI_Free(pvfs_arr_disp);
	ADIOI_Free(pvfs_arr_len);

	return ret;
    }

    /* Shouldn't have gotten here */
    fprintf(stderr, "convert_mpi_pvfs2_dtype:  SERIOUS ERROR\n");
    return -1;
}

int convert_named(MPI_Datatype *mpi_dtype, 
		  PVFS_Request *pvfs_dtype, int combiner)
{ 
    int ret = -1;
#ifdef DEBUG_DTYPE
    fprintf(stderr, "NAMED");
#endif

    switch (*mpi_dtype)
    {
	case MPI_CHAR:
	    ret = PVFS_Request_contiguous(1, PVFS_CHAR, pvfs_dtype);
#ifdef DEBUG_DTYPE
	    fprintf(stderr, "-MPI_CHAR\n");
#endif
	    break;
	case MPI_BYTE:
	    ret = PVFS_Request_contiguous(1, PVFS_BYTE, pvfs_dtype);
#ifdef DEBUG_DTYPE
	    fprintf(stderr, "-MPI_BYTE\n");
#endif
	    break;
	case MPI_SHORT:
	    ret = PVFS_Request_contiguous(1, PVFS_SHORT, pvfs_dtype);
#ifdef DEBUG_DTYPE
	    fprintf(stderr, "-MPI_SHORT\n");
#endif
	    break;
	case MPI_INT:
	    ret = PVFS_Request_contiguous(1, PVFS_INT, pvfs_dtype);	  
#ifdef DEBUG_DTYPE
	    fprintf(stderr, "-MPI_INT\n");
#endif
	    break;
	case MPI_LONG:
	    ret = PVFS_Request_contiguous(1, PVFS_LONG, pvfs_dtype);	  
#ifdef DEBUG_DTYPE
	    fprintf(stderr, "-MPI_LONG\n");
#endif
	    break;
	case MPI_FLOAT:
	    ret = PVFS_Request_contiguous(1, PVFS_FLOAT, pvfs_dtype);	  
#ifdef DEBUG_DTYPE
	    fprintf(stderr, "-MPI_FLOAT\n");
#endif
	    break;
	case MPI_DOUBLE:
	    ret = PVFS_Request_contiguous(1, PVFS_DOUBLE, pvfs_dtype);	  
#ifdef DEBUG_DTYPE
	    fprintf(stderr, "-MPI_DOUBLE\n");
#endif
	    break;
	case MPI_UNSIGNED_CHAR:
	    ret = PVFS_Request_contiguous(1, PVFS_UNSIGNED_CHAR, pvfs_dtype);  
#ifdef DEBUG_DTYPE
	    fprintf(stderr, "-MPI_UNSIGNED_CHAR\n");
#endif
	    break;
	case MPI_UNSIGNED_SHORT:
	    ret = PVFS_Request_contiguous(1, PVFS_UNSIGNED, pvfs_dtype); 
#ifdef DEBUG_DTYPE
	    fprintf(stderr, "-MPI_UNSIGNED_SHORT\n");
#endif
	    break;
	case MPI_UNSIGNED:
	    ret = PVFS_Request_contiguous(1, PVFS_UNSIGNED, pvfs_dtype);  
#ifdef DEBUG_DTYPE
	    fprintf(stderr, "-MPI_SHORT\n");
#endif
	    break;
	case MPI_UNSIGNED_LONG:
	    ret = PVFS_Request_contiguous(1, PVFS_UNSIGNED_LONG, pvfs_dtype);  
#ifdef DEBUG_DTYPE
	    fprintf(stderr, "-MPI_UNSIGNED_LONG\n");
#endif
	    break;
	case MPI_LONG_DOUBLE:
	    ret = PVFS_Request_contiguous(1, PVFS_LONG_DOUBLE, pvfs_dtype);  
#ifdef DEBUG_DTYPE
	    fprintf(stderr, "-MPI_LONG_DOUBLE\n");
#endif
	    break;
	default:
	    fprintf(stderr, "convert_named: predefined type not found");
	    return -1;
	    break;
    }
    if (ret != 0)
	fprintf(stderr, "convert_named: Datatype creation failed\n");
    return ret;
}

void print_dtype_info(int combiner,    
		      int num_int,
		      int num_addr,
		      int num_dtype,
		      int *arr_int,
		      MPI_Aint *arr_addr,
		      MPI_Datatype *arr_dtype)
{
    int i = -1;
    switch (combiner)
    {
	case MPI_COMBINER_CONTIGUOUS:
	    fprintf(stderr, "CONTIG(%d)\n", arr_int[0]);
	    break;
	case MPI_COMBINER_VECTOR:
	    fprintf(stderr, "VECTOR(%d,%d,%d)\n", 
		    arr_int[0], arr_int[1], arr_int[2]);
	    break;
	case MPI_COMBINER_HVECTOR:
	    fprintf(stderr, "HVECTOR(%d,%d,%d)\n",
		    arr_int[0], arr_int[1],arr_addr[0]);
	    break;
	case MPI_COMBINER_INDEXED:
	    fprintf(stderr, "INDEXED(%d,[",
		    arr_int[0]);
	    for (i = 0; i < arr_int[0]; i++)
		fprintf(stderr, "(%d,%Ld) ",
			arr_int[1+i],
			(int64_t) arr_int[arr_int[0]+1+i]);
	    fprintf(stderr, "]\n");
	    break;
	case MPI_COMBINER_HINDEXED:
	    fprintf(stderr, "HINDEXED(%d,[",
		    arr_int[0]);
	    for (i = 0; i < arr_int[0]; i++)
		fprintf(stderr, "(%d,%Ld) ",
			arr_int[1+i],
			(int64_t) arr_addr[i]);
	    fprintf(stderr, "]\n");
	    break;
	case MPI_COMBINER_STRUCT:
	    fprintf(stderr, "STRUCT(%d,[",
		    arr_int[0]);
	    for (i = 0; i < arr_int[0]; i++)
		fprintf(stderr, "(%d,%Ld) ",
			arr_int[1+i],
			(int64_t) arr_addr[i]);
	    fprintf(stderr, "]\n");
	    break;
	case MPI_COMBINER_DUP:
	    fprintf(stderr, "DUP\n");
	    break;
	default:
	    fprintf(stderr, "no available information on this datatype");
    }
}
