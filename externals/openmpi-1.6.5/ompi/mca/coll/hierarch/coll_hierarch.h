/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2008 University of Houston. All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_COLL_HIERARCH_EXPORT_H
#define MCA_COLL_HIERARCH_EXPORT_H

#define ALL_LEVELS 0
#define TWO_LEVELS 2

#include "ompi_config.h"
#include "ompi/constants.h"

#include "mpi.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/mca/mca.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/request/request.h"
#include "ompi/communicator/communicator.h"

BEGIN_C_DECLS

/*
 * Globally exported variable
 */

OMPI_MODULE_DECLSPEC extern const mca_coll_base_component_2_0_0_t mca_coll_hierarch_component;

extern int mca_coll_hierarch_priority_param;
extern int mca_coll_hierarch_verbose_param;
extern int mca_coll_hierarch_use_rdma_param;
extern int mca_coll_hierarch_ignore_sm_param;
extern int mca_coll_hierarch_detection_alg_param;
extern int mca_coll_hierarch_bcast_alg_param;
extern int mca_coll_hierarch_segsize_param;


#define COLL_HIERARCH_SEG_BCAST_ALG   0
#define COLL_HIERARCH_SEG1_BCAST_ALG  1
#define COLL_HIERARCH_SEG2_BCAST_ALG  2
#define COLL_HIERARCH_SEG3_BCAST_ALG  3
#define COLL_HIERARCH_BASIC_BCAST_ALG 4



#define HIER_DEFAULT_NUM_LLEAD 5
/*
 * Data structure for attaching data to the communicator 
 */

/* Clarifying some terminology:
 *  comm:    the input communicator, consisting of several lower level communicators.
 *  lcomm:   low level communicator, often refered to as subcommunicator
 *  lleader: local leader, a dedicated process of each low level communicator
             ATTENTION: an lleader might be the 'head' of a low level
	     communicator of size one!
 *  llcomm:  local leader communicator, grouping all local leaders of a comm.
*/

struct mca_coll_hierarch_module_t {
    mca_coll_base_module_t super;

    struct ompi_communicator_t        *hier_comm; /* link back to the attached comm */ 
    struct ompi_communicator_t       *hier_lcomm; /* low level communicator */
    opal_pointer_array_t              hier_llead; /* local leader communicator structure */
    int                        hier_num_lleaders; /* number of local leaders */
    int                               hier_level; /* level in the hierarchy. For debugging*/
    int                            hier_num_reqs; /* num. of requests */
    ompi_request_t                   **hier_reqs; /* list of requests */
    int                        hier_num_colorarr; /* size of the colorarr array */
    int                                *hier_llr; /* color array compacted (1 entry per color).
                                                     Array of size hier_num_lleaders */
    int                         *hier_max_offset; /* Number of processes for each color. 
                                                     Array of size hier_num_lleaders */
    int                           *hier_colorarr; /* array containing the color of all procs */
};
typedef struct mca_coll_hierarch_module_t mca_coll_hierarch_module_t;
OBJ_CLASS_DECLARATION(mca_coll_hierarch_module_t);

struct mca_coll_hierarch_llead_t {
    struct ompi_communicator_t    *llcomm; /* local leader communicator */
    int                         *lleaders; /* list of local leaders, ranks in comm */
    int                        my_lleader; /* rank of my lleader in lcomm */
    int                        am_lleader; /* am I an lleader? */
    int                            offset; /* Offset used for this llcomm */
};
    
    typedef struct mca_coll_hierarch_llead_t mca_coll_hierarch_llead_t;


static inline int mca_coll_hierarch_count_lleaders ( int size, int *carr)
{
    /* 
     * Determine the number of local leaders. Please note, that any process
     * with color = MPI_UNDEFINED will be counted as the head of a group of its own.
     * Please note furthermore, that every process with color=MPI_UNDEFINED will be
     * stored in this array on its own...
     */ 
    int cnt, i, j, found;
    int *llr=NULL;

    llr = (int *) malloc ( size * sizeof(int));
    if (NULL == llr ){
	return OMPI_ERR_OUT_OF_RESOURCE;
    }

    llr[0] = carr[0];
    for (cnt=1, i=1; i<size; i++ ) {
	if ( carr[i] == MPI_UNDEFINED ) {
	    llr[cnt++] = carr[i];
	    continue;
	}
	for ( found=0, j=0; j<cnt; j++ ) {
	    if ( carr[i] == llr[j] ) {
		found = 1;
		break;
	    }
	}
	if ( !found ) {
	    llr[cnt++] = carr[i];
	}
    }

    free (llr);
    return cnt;
}

static inline int mca_coll_hierarch_get_offset ( int rank, int size, int *carr) 
{
    int offset, i, color = carr[rank];

    if ( color == MPI_UNDEFINED ) {
	/* always */
	return 1;
    }

    for ( offset=0, i=0; i<=rank; i++) {
	if ( carr[i] == color ) {
	    offset++;
	}
    }

    return offset;
}



/* This function determine the parameters required in hierarchical
 * collective operations. It is called from the collective operations themselves.
 *
 * @param root (input):  rank of the root process in comm
 * @param hierarch_module (input):  module structure. Contains
 *                       all relevant, precomputed data for this set of collectives.
 *
 * @param llroot (output): rank of the root process in llcomm, MPI_UNDEFINED for all 
 *                         processes not being part of the local leader communicator.
 * @param lroot (output):  rank of the local leader in the low level communicator, 
 *                         or MPI_UNDEFINED if there is no low level communicator.
 * return value: llcomm (local leader communicator) or MPI_COMM_NULL for
 *               all processes not being part of the local leader communicator.
 */

struct ompi_communicator_t*  mca_coll_hierarch_get_llcomm (int rroot, 
							   mca_coll_hierarch_module_t *hierarch_module,
							   int* llroot, 
							   int* lleader); 

/* This function is supposed to set up all elements of the mca_coll_base_comm_t 
 * structure, including:
 *   hierarch_module->hier_num_lleaders: determine number of local leaders in the comms
 *   hierarch_module->hier_llr:          array of size hier_num_lleaders containing the colors
 *   hierarch_module->hier_max_offset:   array containing the counter for each color how often
 *                            it appears in the colorarr array. 
 */

int mca_coll_hierarch_get_llr ( mca_coll_hierarch_module_t *hierarch_module );


/* This function is supposed to set all elements of the llead structure based on the 
 * offset and the rank of the process.
 *
 * @param rank(input): rank of the calling process in comm
 * @param hierarch_module(input): structure of the hierarchical module. Contains
 *                     all relevant, precomputed data for this set of collectives.
 * @param llead(output): ptr to the mca_coll_hierarch_llead_t element which should 
 *                       be set
 * @param offset(input): offset which shall be used.
 */

int mca_coll_hierarch_get_all_lleaders ( int rank, mca_coll_hierarch_module_t *hierarch_module, 
					 struct mca_coll_hierarch_llead_t *llead, 
					 int offset );



/*
 * coll API functions
 */
int mca_coll_hierarch_init_query(bool allow_hierarch_user_threads,
				 bool have_hidden_threads);
mca_coll_base_module_t *
mca_coll_hierarch_comm_query(struct ompi_communicator_t *comm, int *priority );


int mca_coll_hierarch_module_enable( mca_coll_base_module_t *module, 
				     struct ompi_communicator_t *comm);

int mca_coll_hierarch_module_finalize(struct ompi_communicator_t *comm);

int mca_coll_hierarch_allgather_intra(void *sbuf, int scount, 
				      struct ompi_datatype_t *sdtype, 
				      void *rbuf, int rcount, 
				      struct ompi_datatype_t *rdtype, 
				      struct ompi_communicator_t *comm, 
				      mca_coll_base_module_t *module );
int mca_coll_hierarch_allgatherv_intra(void *sbuf, int scount, 
				       struct ompi_datatype_t *sdtype, 
				       void * rbuf, int *rcounts, 
				       int *disps, 
				       struct ompi_datatype_t *rdtype, 
				       struct ompi_communicator_t *comm,
				       mca_coll_base_module_t *module);
int mca_coll_hierarch_allreduce_intra(void *sbuf, void *rbuf, int count, 
				      struct ompi_datatype_t *dtype, 
				      struct ompi_op_t *op, 
				      struct ompi_communicator_t *comm, 
				      mca_coll_base_module_t *module);
int mca_coll_hierarch_alltoall_intra(void *sbuf, int scount, 
				     struct ompi_datatype_t *sdtype, 
				     void* rbuf, int rcount, 
				     struct ompi_datatype_t *rdtype, 
				     struct ompi_communicator_t *comm, 
				     mca_coll_base_module_t *module);
int mca_coll_hierarch_alltoallv_intra(void *sbuf, int *scounts, 
				      int *sdisps, 
				      struct ompi_datatype_t *sdtype, 
				      void *rbuf, int *rcounts, 
				      int *rdisps, 
				      struct ompi_datatype_t *rdtype, 
				      struct ompi_communicator_t *comm, 
				      mca_coll_base_module_t *module);
int mca_coll_hierarch_alltoallw_intra(void *sbuf, int *scounts, 
				      int *sdisps, 
				      struct ompi_datatype_t **sdtypes, 
				      void *rbuf, int *rcounts, 
				      int *rdisps, 
				      struct ompi_datatype_t **rdtypes, 
				      struct ompi_communicator_t *comm,
				      mca_coll_base_module_t *module);
int mca_coll_hierarch_barrier_intra(struct ompi_communicator_t *comm, 
				    mca_coll_base_module_t *module);
int mca_coll_hierarch_bcast_intra(void *buff, int count, 
				  struct ompi_datatype_t *datatype,
				  int root, 
				  struct ompi_communicator_t *comm, 
				  mca_coll_base_module_t *module);
int mca_coll_hierarch_exscan_intra(void *sbuf, void *rbuf, int count, 
				   struct ompi_datatype_t *dtype, 
				   struct ompi_op_t *op, 
				   struct ompi_communicator_t *comm);
int mca_coll_hierarch_gather_intra(void *sbuf, int scount, 
				   struct ompi_datatype_t *sdtype, 
				   void *rbuf, int rcount, 
				   struct ompi_datatype_t *rdtype, 
				   int root, 
				   struct ompi_communicator_t *comm, 
				   mca_coll_base_module_t *module);
int mca_coll_hierarch_gatherv_intra(void *sbuf, int scount, 
				    struct ompi_datatype_t *sdtype, 
				    void *rbuf, int *rcounts, int *disps, 
				    struct ompi_datatype_t *rdtype, 
				    int root, 
				    struct ompi_communicator_t *comm, 
				    mca_coll_base_module_t *module);
int mca_coll_hierarch_reduce_intra(void *sbuf, void* rbuf, int count, 
				   struct ompi_datatype_t *dtype, 
				   struct ompi_op_t *op, 
				   int root,
				   struct ompi_communicator_t *comm, 
				   mca_coll_base_module_t *module);
int mca_coll_hierarch_reduce_scatter_intra(void *sbuf, void *rbuf, 
					   int *rcounts, 
					   struct ompi_datatype_t *dtype, 
					   struct ompi_op_t *op, 
					   struct ompi_communicator_t *comm, 
					   mca_coll_base_module_t *module);
int mca_coll_hierarch_scan_intra(void *sbuf, void *rbuf, int count, 
				 struct ompi_datatype_t *dtype, 
				 struct ompi_op_t *op, 
				 struct ompi_communicator_t *comm, 
				 mca_coll_base_module_t *module);
int mca_coll_hierarch_scatter_intra(void *sbuf, int scount, 
				    struct ompi_datatype_t *sdtype, void *rbuf, 
				    int rcount, struct ompi_datatype_t *rdtype, 
				    int root, struct ompi_communicator_t *comm, 
				    mca_coll_base_module_t *module);
int mca_coll_hierarch_scatterv_intra(void *sbuf, int *scounts, int *disps, 
				     struct ompi_datatype_t *sdtype, 
				     void* rbuf, int rcount, 
				     struct ompi_datatype_t *rdtype, int root, 
				     struct ompi_communicator_t *comm, 
				     mca_coll_base_module_t *module);

/* 
 * These are trivial implementations of these routines used during comm_query/init,
 * since we cannot access any other collectives
 */
int mca_coll_hierarch_allgather_tmp(void *sbuf, int scount,
				    struct ompi_datatype_t *sdtype,
				    void *rbuf, int rcount,
				    struct ompi_datatype_t *rdtype,
				    struct ompi_communicator_t *comm);
int mca_coll_hierarch_allreduce_tmp(void *sbuf, void *rbuf, int count,
				    struct ompi_datatype_t *dtype,
				    struct ompi_op_t *op,
				    struct ompi_communicator_t *comm);
int mca_coll_hierarch_bcast_tmp ( void *buf, int count,  struct ompi_datatype_t *dtype,
				  int root, struct ompi_communicator_t *comm);

int mca_coll_hierarch_gather_tmp(void *sbuf, int scount,
				 struct ompi_datatype_t *sdtype,
				 void *rbuf, int rcount,
				 struct ompi_datatype_t *rdtype,
				 int root, struct ompi_communicator_t *comm);
int mca_coll_hierarch_reduce_tmp(void *sbuf, void *rbuf, int count,
				 struct ompi_datatype_t *dtype,
				 struct ompi_op_t *op,
				 int root, struct ompi_communicator_t *comm);

int mca_coll_hierarch_ft_event(int status);

END_C_DECLS

#endif /* MCA_COLL_HIERARCH_EXPORT_H */
