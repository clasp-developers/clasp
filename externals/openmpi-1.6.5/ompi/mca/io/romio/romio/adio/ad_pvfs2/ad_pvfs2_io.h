/* -*- Mode: C; c-basic-offset:4 ; -*-
 * vim: ts=8 sts=4 sw=4 noexpandtab
 *
 *   Copyright (C) 2006 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

/* Contig I/O helper prototypes */

#define READ 0
#define WRITE 1

/* #define DEBUG_CONTIG */
/* #define DEBUG_LIST */
/* #define DEBUG_DTYPE */

/* Contig I/O helper prototypes */
int ADIOI_PVFS2_Contig(ADIO_File fd, void *buf, int count,
		       MPI_Datatype datatype, int file_ptr_type,
		       ADIO_Offset offset, ADIO_Status *status,
		       int *error_code, int rw_type);

/* List I/O helper prototypes */
int ADIOI_PVFS2_StridedListIO(ADIO_File fd, void *buf, int count,
			      MPI_Datatype datatype, int file_ptr_type,
			      ADIO_Offset offset, ADIO_Status *status,
			      int *error_code, int rw_type);

int gen_listio_arr(ADIOI_Flatlist_node *flat_buf,
                   int *flat_buf_index_p,
                   int64_t *cur_flat_buf_reg_off_p,
                   int flat_buf_size,
                   int flat_buf_extent,
                   ADIOI_Flatlist_node *flat_file,
                   int *flat_file_index_p,
                   int64_t *cur_flat_file_reg_off_p,
                   int flat_file_size,
                   int flat_file_extent,
                   int max_ol_count,
                   ADIO_Offset disp,
                   int bytes_into_filetype,
                   int64_t *bytes_completed,
                   int64_t total_io_size,
                   int64_t buf_off_arr[],
                   int32_t buf_len_arr[],
                   int32_t *buf_ol_count_p,
                   int64_t file_off_arr[],
                   int32_t file_len_arr[],
                   int32_t *file_ol_count_p);

void print_buf_file_ol_pairs(int64_t buf_off_arr[],
			     int32_t buf_len_arr[],
			     int32_t buf_ol_count,
			     int64_t file_off_arr[],
			     int32_t file_len_arr[],
			     int32_t file_ol_count,
			     void *buf,
			     int rw_type);

/* Datatype I/O helper prototypes */
int ADIOI_PVFS2_StridedDtypeIO(ADIO_File fd, void *buf, int count,
			       MPI_Datatype datatype, int file_ptr_type,
			       ADIO_Offset offset, ADIO_Status *status, 
			       int *error_code, int rw_type);

int convert_named(MPI_Datatype *mpi_dtype,
                  PVFS_Request *pvfs_dtype, int combiner);

void print_dtype_info(int combiner,
                      int num_int,
                      int num_addr,
                      int num_dtype,
                      int *arr_int,
                      MPI_Aint *arr_addr,
                      MPI_Datatype *arr_dtype);

int convert_mpi_pvfs2_dtype(MPI_Datatype *mpi_dtype,
                            PVFS_Request *pvfs_dtype);

