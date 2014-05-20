/* ---------------------------------------------------------------- */
/* (C)Copyright IBM Corp.  2007, 2008                               */
/* ---------------------------------------------------------------- */
/**
 * \file ad_bgl_aggrs.h
 * \brief ???
 */

/* 
 * File: ad_bgl_aggrs.h
 * 
 * Declares functions specific for BG/L - GPFS parallel I/O solution. The implemented optimizations are:
 * 	. Aligned file-domain partitioning, integrated in 7/28/2005
 * 
 * In addition, following optimizations are planned:
 * 	. Integrating multiple file-domain partitioning schemes 
 *	  (corresponding to Alok Chouhdary's persistent file domain work).
 */

#ifndef AD_BGL_AGGRS_H_
#define AD_BGL_AGGRS_H_

#include "adio.h"
#include <sys/stat.h>

#if !defined(GPFS_SUPER_MAGIC)
  #define GPFS_SUPER_MAGIC (0x47504653)
#endif

#if !defined(PVFS2_SUPER_MAGIC)
  #define PVFS2_SUPER_MAGIC (0x20030528)
#endif

    /* File system (BGL) specific information - 
         hung off of ADIOI_FileD file descriptor (fd->fs_ptr) at open */
    typedef struct ADIOI_BGL_fs_s {
      __blksize_t blksize;
      int         fsync_aggr; /* "fsync aggregation" flags (below) */
#define ADIOI_BGL_FSYNC_AGGREGATION_DISABLED  0x00
#define ADIOI_BGL_FSYNC_AGGREGATION_ENABLED   0x01
#define ADIOI_BGL_FSYNC_AGGREGATOR            0x10 /* This rank is an aggregator */
    }  ADIOI_BGL_fs;

    /* generate a list of I/O aggregators that utilizes BGL-PSET orginization. */
    int ADIOI_BGL_gen_agg_ranklist(ADIO_File fd, int n_aggrs_per_pset);

    /* overriding ADIOI_Calc_file_domains() to apply 'aligned file domain partitioning'. */
    void ADIOI_BGL_GPFS_Calc_file_domains(ADIO_Offset *st_offsets,
				          ADIO_Offset *end_offsets,
				          int          nprocs,
				          int          nprocs_for_coll,
				          ADIO_Offset *min_st_offset_ptr,
				          ADIO_Offset **fd_start_ptr,
				          ADIO_Offset **fd_end_ptr,
				          ADIO_Offset *fd_size_ptr,
                  void        *fs_ptr);

    /* a utilitiy function for debugging */
    int ADIOI_BGL_Aggrs_index(ADIO_File fd, int myrank );

    /* overriding ADIOI_Calc_aggregator() for the default implementation is specific for 
       static file domain partitioning */
    int ADIOI_BGL_Calc_aggregator(ADIO_File fd,
				  ADIO_Offset off,
				  ADIO_Offset min_off,
				  ADIO_Offset *len,
				  ADIO_Offset fd_size,
				  ADIO_Offset *fd_start,
				  ADIO_Offset *fd_end);

    /* overriding ADIOI_Calc_my_req for the default implementation is specific for 
       static file domain partitioning */
    void ADIOI_BGL_Calc_my_req ( ADIO_File fd, ADIO_Offset *offset_list, ADIO_Offset *len_list,
				 int contig_access_count, ADIO_Offset
				 min_st_offset, ADIO_Offset *fd_start,
				 ADIO_Offset *fd_end, ADIO_Offset fd_size,
				 int nprocs,
				 int *count_my_req_procs_ptr,
				 int **count_my_req_per_proc_ptr,
				 ADIOI_Access **my_req_ptr,
				 int **buf_idx_ptr);

    /*
     * ADIOI_Calc_others_req
     *
     * param[in]  count_my_req_procs        Number of processes whose file domain my
     *                                        request touches.
     * param[in]  count_my_req_per_proc     count_my_req_per_proc[i] gives the no. of 
     *                                        contig. requests of this process in 
     *                                        process i's file domain.
     * param[in]  my_req                    A structure defining my request
     * param[in]  nprocs                    Number of nodes in the block
     * param[in]  myrank                    Rank of this node
     * param[out] count_others_req_proc_ptr Number of processes whose requests lie in
     *                                        my process's file domain (including my 
     *                                        process itself)
     * param[out] others_req_ptr            Array of other process' requests that lie
     *                                        in my process's file domain
     */
     void ADIOI_BGL_Calc_others_req(ADIO_File fd, int count_my_req_procs, 
				    int *count_my_req_per_proc,
				    ADIOI_Access *my_req, 
				    int nprocs, int myrank,
				    int *count_others_req_procs_ptr,
				    ADIOI_Access **others_req_ptr);


#endif  /* AD_BGL_AGGRS_H_ */
