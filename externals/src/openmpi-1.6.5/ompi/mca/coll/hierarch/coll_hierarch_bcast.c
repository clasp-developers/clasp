/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2008 University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_hierarch.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/mca/pml/pml.h"

/*
 *	bcast_intra
 *
 *	Function:	- broadcast using hierarchical algorithm
 *	Accepts:	- same arguments as MPI_Bcast()
 *	Returns:	- MPI_SUCCESS or error code
 */




static int mca_coll_hierarch_bcast_intra_seg (void *buff, 
					      int count,
					      struct ompi_datatype_t *datatype, 
					      int root,
					      struct ompi_communicator_t *comm, 
					      mca_coll_base_module_t *module, 
                                              int segsize );


static int mca_coll_hierarch_bcast_intra_seg1 (void *buff, 
					       int count,
					       struct ompi_datatype_t *datatype, 
					       int root,
					       struct ompi_communicator_t *comm, 
					       mca_coll_base_module_t *module, 
                                               int segsize );


static int mca_coll_hierarch_bcast_intra_seg2 (void *buff, 
					       int count,
					       struct ompi_datatype_t *datatype, 
					       int root,
					       struct ompi_communicator_t *comm, 
					       mca_coll_base_module_t *module, 
                                               int segsize );

static int mca_coll_hierarch_bcast_intra_seg3 (void *buff, 
					       int count,
					       struct ompi_datatype_t *datatype, 
					       int root,
					       struct ompi_communicator_t *comm, 
					       mca_coll_base_module_t *module, 
                                               int segsize );




int mca_coll_hierarch_bcast_intra(void *buff, 
				  int count,
				  struct ompi_datatype_t *datatype, 
				  int root,
				  struct ompi_communicator_t *comm, 
				  mca_coll_base_module_t *module)
{
    int bcast_alg = mca_coll_hierarch_bcast_alg_param;
    int segsize = mca_coll_hierarch_segsize_param;
    int ret=OMPI_SUCCESS;


    /* Here is a brief description on what we try to evaluate:
       - bcast_intra_seg used the bcast of lcomm and llcomm, similarly
         to original algorithm in hierarch. However, it can segment
	 the message, such that we might get an overlap between the two
	 layers. This overlap is based on the assumption, that a process
	 might be done early with a bcast and can start the next one.
       - bcast_intra_seg1: replaces the llcomm->bcast by isend/irecvs
         to increase the overlap, keeps the lcomm->bcast however
       - bcast_intra_seg2: replaced lcomm->bcast by isend/irecvs
         to increase the overlap, keeps however llcomm->bcast
       - bcast_intra_seg3: replaced both lcomm->bcast and llcomm->bcast
         by isend/irecvs
    */

    if ( COLL_HIERARCH_SEG_BCAST_ALG == bcast_alg ) {
	ret = mca_coll_hierarch_bcast_intra_seg ( buff, count, datatype, root,
						  comm, module, segsize );
    }
    else if ( COLL_HIERARCH_SEG1_BCAST_ALG == bcast_alg ) {
	ret = mca_coll_hierarch_bcast_intra_seg1 ( buff, count, datatype, root,
						   comm, module, segsize );
    }
    else if ( COLL_HIERARCH_SEG2_BCAST_ALG == bcast_alg ) {
	ret = mca_coll_hierarch_bcast_intra_seg2 ( buff, count, datatype, root,
						   comm, module, segsize );
    }
    else if ( COLL_HIERARCH_SEG3_BCAST_ALG == bcast_alg ) {
	ret = mca_coll_hierarch_bcast_intra_seg3 ( buff, count, datatype, root,
						   comm, module, segsize );
    }
    else {
	/* Segment size of zero forces the entire message to be bcasted 
	   as a single segment. */
	ret = mca_coll_hierarch_bcast_intra_seg ( buff, count, datatype, root,
						  comm, module, 0 );
    }

    return ret;
}



static int mca_coll_hierarch_bcast_intra_seg (void *buff, 
					      int count,
					      struct ompi_datatype_t *datatype, 
					      int root,
					      struct ompi_communicator_t *comm, 
					      mca_coll_base_module_t *module, 
                                              int segsize )
{
    struct ompi_communicator_t *llcomm=NULL;
    struct ompi_communicator_t *lcomm=NULL;
    mca_coll_hierarch_module_t *hierarch_module = (mca_coll_hierarch_module_t *) module;
    int lroot=MPI_UNDEFINED, llroot=MPI_UNDEFINED;
    int rank=0, ret=OMPI_SUCCESS;
    MPI_Aint ub=0, typeext=0;
    size_t typesize=0;
    int realsegsize=0, remaining_count=0;
    int num_segments=0, segcount=0, segindex=0;
    char* tmpbuf = (char *) buff;

    rank   = ompi_comm_rank ( comm );
    lcomm  = hierarch_module->hier_lcomm;
    
    if ( mca_coll_hierarch_verbose_param ) { 
	printf("%s:%d: executing hierarchical seg bcast with cnt=%d root=%d, segsize=%d\n",
	       comm->c_name, rank, count, root, segsize );
    } 

   /*
     * This function returns the local leader communicator
     * which *always* contains the root of this operation.
     * This might involve creating a new communicator. This is
     * also the reason, that *every* process in comm has to call
     * this function
     */
    llcomm = mca_coll_hierarch_get_llcomm ( root, hierarch_module, &llroot, &lroot);


    ompi_datatype_type_size  ( datatype, &typesize);
    ompi_datatype_get_extent ( datatype, &ub, &typeext);


    /* Determine number of segments and number of elements per segment */
    if ((typesize > 0) && (segsize % typesize != 0)) {
	/* segment size must be a multiple of typesize */
	segsize = typesize * (segsize / typesize);
    }
    if ((segsize == 0) || (count == 0) || (typesize == 0)) {
	segcount = count;
	num_segments = 1;
    } 
    else {
	segcount = segsize/typesize;
	num_segments = count/segcount;
	if ( (count % segcount) != 0 ) {
	    num_segments++;
	}
	if (num_segments == 1) {
	    segcount = count;
	}
    }

    realsegsize = segcount*typeext;
    remaining_count = segcount;

    
    for (segindex = 0; segindex < num_segments; segindex++) {
	/* determine how many elements are being sent in this round */
	if( segindex == (num_segments - 1) ) {
	    remaining_count = count - segindex*segcount;
	}
		
	/* Bcast on the upper level among the local leaders */
	if ( MPI_UNDEFINED != llroot ) {
	    ret = llcomm->c_coll.coll_bcast(tmpbuf, remaining_count, 
					    datatype, llroot, llcomm, 
					    llcomm->c_coll.coll_bcast_module);
	    if ( OMPI_SUCCESS != ret ) {
		return ret;
	    }
	}
	
	/* once the local leaders got the data from the root, they can distribute
	 * it to the processes in their local, low-level communicator.
	 */
	if ( MPI_COMM_NULL != lcomm ) {
	    ret = lcomm->c_coll.coll_bcast(tmpbuf, remaining_count, 
					   datatype, lroot, lcomm, 
					   lcomm->c_coll.coll_bcast_module);
	    if ( OMPI_SUCCESS != ret ) {
		return ret;
	    }
	}
	
	tmpbuf += realsegsize;
    }
    
    return ret;
}

static int mca_coll_hierarch_bcast_intra_seg1 (void *buff, 
					       int count,
					       struct ompi_datatype_t *datatype, 
					       int root,
					       struct ompi_communicator_t *comm, 
					       mca_coll_base_module_t *module, 
                                               int segsize )
{
    struct ompi_communicator_t *llcomm=NULL;
    struct ompi_communicator_t *lcomm=NULL;
    mca_coll_hierarch_module_t *hierarch_module = (mca_coll_hierarch_module_t *) module;
    int lroot=MPI_UNDEFINED, llroot=MPI_UNDEFINED;
    int llrank=0, llsize=0, rank=0, ret=OMPI_SUCCESS;
    MPI_Aint ub=0, typeext=0;
    size_t typesize=0;
    int i, realsegsize=0, remaining_count=0;
    int num_segments=0, segcount=0, segindex=0;
    char* tmpbuf = (char *) buff;
    ompi_request_t **sreq=NULL;
    ompi_request_t *rreq=MPI_REQUEST_NULL;

    rank   = ompi_comm_rank ( comm );
    lcomm  = hierarch_module->hier_lcomm;
    
    if ( mca_coll_hierarch_verbose_param ) { 
	printf("%s:%d: executing hierarchical seg1 bcast with cnt=%d root=%d segsize=%d\n",
	       comm->c_name, rank, count, root, segsize  );
    } 

   /*
     * This function returns the local leader communicator
     * which *always* contains the root of this operation.
     * This might involve creating a new communicator. This is
     * also the reason, that *every* process in comm has to call
     * this function
     */
    llcomm = mca_coll_hierarch_get_llcomm ( root, hierarch_module, &llroot, &lroot);

    ompi_datatype_type_size ( datatype, &typesize);
    ompi_datatype_get_extent ( datatype, &ub, &typeext);
    
    /* Determine number of segments and number of elements per segment */
    if ((typesize > 0) && (segsize % typesize != 0)) {
	/* segment size must be a multiple of typesize */
	segsize = typesize * (segsize / typesize);
    }
    if ((segsize == 0) || (count == 0) || (typesize == 0)) {
	segcount = count;
	num_segments = 1;
    } 
    else {
	segcount = segsize/typesize;
	num_segments = count/segcount;
	if ( (count % segcount) != 0 ) {
	    num_segments++;
	}
	if (num_segments == 1) {
	    segcount = count;
	}
    }

    realsegsize = segcount*typeext;
    remaining_count = segcount;
    
    if ( MPI_COMM_NULL != llcomm ) {
        llrank = ompi_comm_rank ( llcomm );
        llsize = ompi_comm_size ( llcomm);
        sreq = hierarch_module->hier_reqs;
        for(i=0; i<llsize; i++) {
            sreq[i] = MPI_REQUEST_NULL;
        }
    }

    /* Broadcasting the first segment in the upper level*/
    if ( MPI_UNDEFINED != llroot ) {
        ret = llcomm->c_coll.coll_bcast(tmpbuf, remaining_count, datatype, 
                                        llroot, llcomm, 
                                        llcomm->c_coll.coll_bcast_module );
        if ( OMPI_SUCCESS != ret ) {
            return ret;
        }
    }
    
    
    /* Since the first segment has already been bcasted, this loop
       starts at 1 and not with segment 0 */
    for (segindex = 1; segindex < num_segments; segindex++) {
	/* determine how many elements are being sent in this round */
	if( segindex == (num_segments - 1) ) {
	    remaining_count = count - segindex*segcount;
	}
	tmpbuf += realsegsize;
	
	/* Broadcasting the next segment in the upper level using non blocking 
	   operations*/
	if ( MPI_COMM_NULL != llcomm ) {
	    if( llrank == llroot) {
		for( i = 0; i < llsize; i++) {
		    if( i != llroot) {
			ret = MCA_PML_CALL(isend(tmpbuf, remaining_count, datatype, i, 
					   MCA_COLL_BASE_TAG_BCAST,
					   MCA_PML_BASE_SEND_STANDARD,
					   llcomm, &(sreq[i])));
			if ( OMPI_SUCCESS != ret ) {
			    return ret;
			}
			
		    }
		}
	    }
	    else {
		ret = MCA_PML_CALL(irecv(tmpbuf, remaining_count, datatype, llroot,  
					 MCA_COLL_BASE_TAG_BCAST,
					 llcomm, &rreq ));
		if ( OMPI_SUCCESS != ret ) {
		    return ret;
		}

	    }
	}
	
	
	/* broadcasting the before segment among the lower level processes
	   using blocking operations*/
	if ( MPI_COMM_NULL != lcomm ) {
	    ret = lcomm->c_coll.coll_bcast(tmpbuf-realsegsize, segcount, 
					   datatype, lroot, lcomm, 
					   lcomm->c_coll.coll_bcast_module);
	    if ( OMPI_SUCCESS != ret ) {
		return ret;
	    }
	}
	
	if ( MPI_COMM_NULL != llcomm ) {
	    if ( llrank == llroot ) {
		ret = ompi_request_wait_all( llsize, sreq, MPI_STATUSES_IGNORE);
		if ( OMPI_SUCCESS != ret ) {
		    return ret;
		}
	    }
	    else {
		ret = ompi_request_wait_all(1, &rreq, MPI_STATUS_IGNORE);
		if ( OMPI_SUCCESS != ret ) {
		    return ret;
		}
	    }
	}
    }

    /* Bcasting the last segment among the lower level processes using blocking operations
     * once the local leaders got the data from the root, they can distribute
     * it to the processes in their local, low-level communicator.
     */
    if ( MPI_COMM_NULL != lcomm ) {
	ret = lcomm->c_coll.coll_bcast(tmpbuf, remaining_count, datatype, 
				       lroot, lcomm, 
				       lcomm->c_coll.coll_bcast_module);
    }

    
    return ret;
}

static int mca_coll_hierarch_bcast_intra_seg2 (void *buff, 
					       int count,
					       struct ompi_datatype_t *datatype, 
					       int root,
					       struct ompi_communicator_t *comm, 
					       mca_coll_base_module_t *module, 
                                               int segsize )
{
    struct ompi_communicator_t *llcomm=NULL;
    struct ompi_communicator_t *lcomm=NULL;
    mca_coll_hierarch_module_t *hierarch_module = (mca_coll_hierarch_module_t *) module;
    int lroot=MPI_UNDEFINED, llroot=MPI_UNDEFINED;
    int rank=0, ret=OMPI_SUCCESS;
    int lsize=0, lrank=0;
    MPI_Aint ub=0, typeext=0;
    size_t typesize=0;
    int i, realsegsize=0, remaining_count=0;
    int num_segments=0, segcount=0, segindex=0;
    char* tmpbuf = (char *) buff;
    ompi_request_t **sreq=NULL;
    ompi_request_t *rreq=MPI_REQUEST_NULL;

    rank   = ompi_comm_rank ( comm );
    lcomm  = hierarch_module->hier_lcomm;
    
    if ( mca_coll_hierarch_verbose_param ) { 
	printf("%s:%d: executing hierarchical seg2 bcast with cnt=%d root=%d segsize=%d\n",
	       comm->c_name, rank, count, root, segsize );
    } 

   /*
     * This function returns the local leader communicator
     * which *always* contains the root of this operation.
     * This might involve creating a new communicator. This is
     * also the reason, that *every* process in comm has to call
     * this function
     */
    llcomm = mca_coll_hierarch_get_llcomm ( root, hierarch_module, &llroot, &lroot);

    ompi_datatype_type_size ( datatype, &typesize);
    ompi_datatype_get_extent ( datatype, &ub, &typeext);
    
    /* Determine number of segments and number of elements per segment */
    if ((typesize > 0) && (segsize % typesize != 0)) {
	/* segment size must be a multiple of typesize */
	segsize = typesize * (segsize / typesize);
    }
    if ((segsize == 0) || (count == 0) || (typesize == 0)) {
	segcount = count;
	num_segments = 1;
    } 
    else {
	segcount = segsize/typesize;
	num_segments = count/segcount;
	if ( (count % segcount) != 0 ) {
	    num_segments++;
	}
	if (num_segments == 1) {
	    segcount = count;
	}
    }

    realsegsize = segcount*typeext;
    remaining_count = segcount;
    
    lsize = ompi_comm_size (lcomm);
    sreq = hierarch_module->hier_reqs;
    for(i=0; i<lsize; i++) {
	sreq[i] = MPI_REQUEST_NULL;
    }
    
    
    if ( MPI_UNDEFINED != llroot ) {
	ret = llcomm->c_coll.coll_bcast(tmpbuf, remaining_count, datatype, 
					llroot, llcomm, 
					llcomm->c_coll.coll_bcast_module);
	if ( OMPI_SUCCESS != ret ) {
	    return ret;
	}
    }
    
    if ( MPI_COMM_NULL != lcomm ) {
	lrank = ompi_comm_rank ( lcomm );
    }
    
    for (segindex = 1; segindex < num_segments; segindex++) {
	/* once the local leaders got the data from the root, they can distribute
	 * it to the processes in their local, low-level communicator.*/
	
	if ( MPI_COMM_NULL != lcomm ) {
	    if(lrank == lroot) {
		for(i = 0; i < lsize; i++) {
		    if( i != lroot) {
			ret = MCA_PML_CALL(isend(tmpbuf, remaining_count, datatype, i, 
						 MCA_COLL_BASE_TAG_BCAST,
						 MCA_PML_BASE_SEND_STANDARD,
						 lcomm, &(sreq[i])));
			if ( OMPI_SUCCESS != ret ) {
			    return ret;
			}

		    }
		}
	    }
	    else {
		ret = MCA_PML_CALL(irecv(tmpbuf, remaining_count, datatype, lroot,
					 MCA_COLL_BASE_TAG_BCAST, lcomm, &rreq));
		if ( OMPI_SUCCESS != ret ) {
		    return ret;
		}
	    }
	}
	
	/* determine how many elements are being sent in this round */
	if( segindex == (num_segments - 1) ) {
	    remaining_count = count - segindex*segcount;
	}
	tmpbuf += realsegsize;
	
	if ( MPI_UNDEFINED != llroot ) {
	    ret = llcomm->c_coll.coll_bcast(tmpbuf, remaining_count, datatype, 
					    llroot, llcomm, 
					    llcomm->c_coll.coll_bcast_module);
	    if ( OMPI_SUCCESS != ret ) {
		return ret;
	    }
	}

	if ( MPI_COMM_NULL != lcomm ) {
	    if ( lrank == lroot ) {
		ret = ompi_request_wait_all ( lsize, sreq, MPI_STATUSES_IGNORE);
		if ( OMPI_SUCCESS != ret ) {
		    return ret;
		}
	    }
	    else {
		ret = ompi_request_wait_all ( 1, &rreq, MPI_STATUS_IGNORE);
		if ( OMPI_SUCCESS != ret ) {
		    return ret;
		}
	    }
	}

    }
    
    /* Bcasting the last segment among the lower level processes
     * once the local leaders got the data from the root, they can distribute
     * it to the processes in their local, low-level communicator.
     */
    if ( MPI_COMM_NULL != lcomm ) {
	ret = lcomm->c_coll.coll_bcast(tmpbuf, remaining_count, datatype, 
				       lroot, lcomm, 
				       lcomm->c_coll.coll_bcast_module);
    }
    
    return ret;
}

static int mca_coll_hierarch_bcast_intra_seg3 (void *buff, 
					       int count,
					       struct ompi_datatype_t *datatype, 
					       int root,
					       struct ompi_communicator_t *comm, 
					       mca_coll_base_module_t *module, 
                                               int segsize )
{
    struct ompi_communicator_t *llcomm=NULL;
    struct ompi_communicator_t *lcomm=NULL;
    mca_coll_hierarch_module_t *hierarch_module = (mca_coll_hierarch_module_t *) module;
    int lroot=MPI_UNDEFINED, llroot=MPI_UNDEFINED;
    int llrank=MPI_UNDEFINED, llsize=0, rank=0, ret=OMPI_SUCCESS;
    int lsize=0, lrank=MPI_UNDEFINED;
    MPI_Aint ub=0, typeext=0;
    size_t typesize=0;
    int i, realsegsize=0, remaining_count=0;
    int num_segments=0, segcount=0, segindex=0;
    char* tmpbuf = (char *) buff;
    ompi_request_t **sreq=NULL, **sreq1=NULL;
    ompi_request_t *rreq=MPI_REQUEST_NULL, *rreq1=MPI_REQUEST_NULL;

    rank   = ompi_comm_rank ( comm );
    lcomm  = hierarch_module->hier_lcomm;
    
    if ( mca_coll_hierarch_verbose_param ) { 
	printf("%s:%d: executing hierarchical seg3 bcast with cnt=%d root=%d segsize=%d\n",
	       comm->c_name, rank, count, root, segsize );
    } 

   /*
     * This function returns the local leader communicator
     * which *always* contains the root of this operation.
     * This might involve creating a new communicator. This is
     * also the reason, that *every* process in comm has to call
     * this function
     */
    llcomm = mca_coll_hierarch_get_llcomm ( root, hierarch_module, &llroot, &lroot);

    ompi_datatype_type_size ( datatype, &typesize);
    ompi_datatype_get_extent ( datatype, &ub, &typeext);

    /* Determine number of segments and number of elements per segment */
    if ((typesize > 0) && (segsize % typesize != 0)) {
	/* segment size must be a multiple of typesize */
	segsize = typesize * (segsize / typesize);
    }
    if ((segsize == 0) || (count == 0) || (typesize == 0)) {
	segcount = count;
	num_segments = 1;
    } else {
	segcount = segsize/typesize;
	num_segments = count/segcount;
	if ( (count % segcount) != 0 ) num_segments++;
	if (num_segments == 1) segcount = count;
    }
    realsegsize = segcount*typeext;
    remaining_count = segcount;

    if ( MPI_COMM_NULL != lcomm ) {
        lsize = ompi_comm_size ( lcomm );
        lrank = ompi_comm_rank ( lcomm );	
        sreq1 = (ompi_request_t **)malloc ( lsize * sizeof(ompi_request_t *));
        if ( NULL == sreq1 ) {
	    return OMPI_ERR_OUT_OF_RESOURCE;
        }
        for(i=0; i<lsize; i++) {
	    sreq1[i] = MPI_REQUEST_NULL;
        }
    }

    if ( MPI_COMM_NULL != llcomm ) {
        llsize = ompi_comm_size (llcomm);
        llrank = ompi_comm_rank ( llcomm );
    
        sreq  = hierarch_module->hier_reqs;
	for(i=0; i<llsize; i++) {
	    sreq[i] = MPI_REQUEST_NULL;
	}
    }

    
    /* Broadcasting the first segment in the upper level*/
    if ( MPI_UNDEFINED != llroot ) {
	ret = llcomm->c_coll.coll_bcast(tmpbuf, remaining_count, datatype, 
					llroot, llcomm, 
					llcomm->c_coll.coll_bcast_module);
	if ( OMPI_SUCCESS != ret ) {
	    goto exit;
	}
    }
    
    for (segindex = 1; segindex < num_segments; segindex++) {
	/* determine how many elements are being sent in this round */
	if( segindex == (num_segments - 1) ) {
	    remaining_count = count - segindex*segcount;
	}
	tmpbuf += realsegsize;
	
	/* Broadcasting the next segment in the upper level*/
	if ( MPI_COMM_NULL != llcomm ) {
	    if(llrank == llroot) {
		for(i = 0; i < llsize; i++) {
		    if( i != llroot) {
			ret = MCA_PML_CALL(isend(tmpbuf, remaining_count, datatype, i,
						 MCA_COLL_BASE_TAG_BCAST, 
						 MCA_PML_BASE_SEND_STANDARD,
						 llcomm, (sreq+i) ));
			if ( OMPI_SUCCESS != ret ) {
			    goto exit;
			}
		    }
		}
	    }
            else {
                ret = MCA_PML_CALL(irecv(tmpbuf, remaining_count, datatype, llroot,
                                         MCA_COLL_BASE_TAG_BCAST, 
                                         llcomm, &rreq ));
                if ( OMPI_SUCCESS != ret ) {
                    goto exit;
                }
            }
        }
	
	/* broadcasting the before segment among the lower level processes
	 * once the local leaders got the data from the root, they can distribute
	 * it to the processes in their local, low-level communicator.
	 */
	if ( MPI_COMM_NULL != lcomm ) {
	    if( lrank == lroot) {
		for( i = 0; i < lsize; i++) {
		    if( i != lroot) {
			ret = MCA_PML_CALL(isend(tmpbuf-realsegsize, segcount, datatype, i,
						 MCA_COLL_BASE_TAG_BCAST,
						 MCA_PML_BASE_SEND_STANDARD,
						 lcomm, (sreq1+i) ));
			if ( OMPI_SUCCESS != ret ) {
			    goto exit;
			}
		    }
		}
	    }
	    else {
		ret = MCA_PML_CALL(irecv(tmpbuf-realsegsize, segcount, datatype, lroot, 
					 MCA_COLL_BASE_TAG_BCAST , lcomm, &rreq1 ));
		if ( OMPI_SUCCESS != ret ) {
		    goto exit;
		}
	    }
	}
	
	/* Wait for the upper level bcast to complete*/
	if ( MPI_COMM_NULL != llcomm ) {
	    if ( llrank == llroot ) {
		ret = ompi_request_wait_all(llsize, sreq, MPI_STATUSES_IGNORE);
		if ( OMPI_SUCCESS != ret ) {
		    goto exit;
		}
	    }
	    else {
		ret = ompi_request_wait_all ( 1, &rreq, MPI_STATUS_IGNORE );
		if ( OMPI_SUCCESS != ret ) {
		    goto exit;
		}
	    }
	}
	
	/*Wait for the lower level bcast to complete */
	if ( MPI_COMM_NULL != lcomm ) {
	    if ( lrank == lroot ) {
		ret = ompi_request_wait_all(lsize, sreq1, MPI_STATUSES_IGNORE);
		if ( OMPI_SUCCESS != ret ) {
		    goto exit;
		}
	    }
	    else {
		ret = ompi_request_wait_all( 1, &rreq1, MPI_STATUS_IGNORE);
		if ( OMPI_SUCCESS != ret ) {
		    goto exit;
		}
	    }
	}	
    }
    
    /*Bcasting the last segment among the lower level processes
     * once the local leaders got the data from the root, they can distribute
     * it to the processes in their local, low-level communicator.
     */
    if ( MPI_COMM_NULL != lcomm ) {
        ret = lcomm->c_coll.coll_bcast(tmpbuf, remaining_count, datatype,  
                                       lroot, lcomm, 
                                       lcomm->c_coll.coll_bcast_module);
    }
    
exit:
    if ( NULL != sreq1 ) {
	free ( sreq1 );
    }

    return ret;
}
