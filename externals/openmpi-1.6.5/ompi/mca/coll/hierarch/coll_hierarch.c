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
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_hierarch.h"

#include <stdio.h>

#include "mpi.h"
#include "ompi/communicator/communicator.h"
#include "ompi/group/group.h"
#include "ompi/proc/proc.h"

#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/coll/base/coll_tags.h"

#include "opal/class/opal_bitmap.h"
#include "ompi/mca/bml/bml.h"
#include "ompi/mca/bml/base/base.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/mca/btl/btl.h"

/* Local functions and data */
#define HIER_MAXPROTOCOL 6
#define HIER_MAX_PROTNAMELEN 7
static int mca_coll_hierarch_max_protocol=HIER_MAXPROTOCOL;

/* Commments: need to add ofud, portals and sctp into this list! */
static char hier_prot[HIER_MAXPROTOCOL][HIER_MAX_PROTNAMELEN]={"0","tcp","udapl","mx","openib","sm"};

static void mca_coll_hierarch_checkfor_component (struct ompi_communicator_t *comm,
						  int component_level,
						  char *component_name, 
						  int *key, int *ncount);
static void mca_coll_hierarch_checkfor_sm (struct ompi_communicator_t *comm,
					   int *color,
					   int *ncount);
static void mca_coll_hierarch_dump_struct ( mca_coll_hierarch_module_t *c);


/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this module to indicate what level of thread support it provides.
 */
int mca_coll_hierarch_init_query(bool allow_hierarch_user_threads,
                                 bool have_hidden_user_threads)
{
    /* Don't ask. All done */
    return OMPI_SUCCESS;
}


/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
mca_coll_base_module_t *
mca_coll_hierarch_comm_query(struct ompi_communicator_t *comm, int *priority )
{
    int size, rank;
    int color, ncount=0, maxncount;
    int level;
    int ret=OMPI_SUCCESS;
    int ignore_sm=0;
    int detection_alg=0;
    mca_coll_hierarch_module_t *hierarch_module;

    /* This module only works for intra-communicators at the moment */
    if (OMPI_COMM_IS_INTER(comm)) {
        return NULL;
    }
    
    /* Get the priority level attached to this module. If priority is less
     * than or equal to 0, then the module is unavailable. */
    *priority = mca_coll_hierarch_priority_param;
    if (0 >= mca_coll_hierarch_priority_param) {
	return NULL;
    }

    /* This module only works with ob1 pml respectively btl's. Opt out
       of we use cm/mtl's. */
    if ( strcmp ( mca_pml_base_selected_component.pmlm_version.mca_component_name, "ob1")) {
	return NULL;
    }


    size = ompi_comm_size(comm);
    if (size < 3) {
	/* No need for hierarchical collectives for 1 or 2 procs. */
	return NULL;
    }

    hierarch_module = OBJ_NEW(mca_coll_hierarch_module_t);
    if (NULL == hierarch_module) {
	return NULL;
    }

    hierarch_module->super.coll_module_enable = mca_coll_hierarch_module_enable;
    hierarch_module->super.ft_event = mca_coll_hierarch_ft_event;
    hierarch_module->super.coll_allgather  = NULL;
    hierarch_module->super.coll_allgatherv = NULL;
    hierarch_module->super.coll_allreduce  = mca_coll_hierarch_allreduce_intra;
    hierarch_module->super.coll_alltoall   = NULL;
    hierarch_module->super.coll_alltoallv  = NULL;
    hierarch_module->super.coll_alltoallw  = NULL;
    hierarch_module->super.coll_barrier    = mca_coll_hierarch_barrier_intra;
    hierarch_module->super.coll_bcast      = mca_coll_hierarch_bcast_intra;
    hierarch_module->super.coll_exscan     = NULL;
    hierarch_module->super.coll_gather     = NULL;
    hierarch_module->super.coll_gatherv    = NULL;
    hierarch_module->super.coll_reduce     = mca_coll_hierarch_reduce_intra;
    hierarch_module->super.coll_reduce_scatter = NULL;
    hierarch_module->super.coll_scan       = NULL;
    hierarch_module->super.coll_scatter    = NULL;
    hierarch_module->super.coll_scatterv   = NULL;


    /* Check whether we should ignore sm. This might be necessary to take advantage
       of the some ib or gm collectives. */
    ignore_sm = mca_coll_hierarch_ignore_sm_param;

    rank = ompi_comm_rank(comm);

    hierarch_module->hier_num_colorarr  = size;
    hierarch_module->hier_colorarr      = (int *) malloc ( sizeof(int) * size);
    if ( NULL == hierarch_module->hier_colorarr ) {
        *priority = 0;
        return NULL;
    }

    /* 
     * walk through the list of registered protocols, and check which one
     * is feasible. 
     * Later we start with level=0, and introduce the multi-cell check 
     */
    if ( ignore_sm ) {
	mca_coll_hierarch_max_protocol = HIER_MAXPROTOCOL - 1;
    }

    /* if number of levels is not specified, or if it is specified as ALL_LEVELS,
    * proceed in the usual way
    */

    detection_alg = mca_coll_hierarch_detection_alg_param;
    if( TWO_LEVELS == detection_alg ) {
	mca_coll_hierarch_max_protocol = 2;
	if ( mca_coll_hierarch_verbose_param ) {
	    printf("Using two level hierarchy detection\n");
	}
    }

    for ( level = mca_coll_hierarch_max_protocol - 1; level >0 ; level--) {
	if ( ALL_LEVELS == detection_alg ) {
	    mca_coll_hierarch_checkfor_component ( comm, 
						   level, 
						   hier_prot[level], 
						   &color, 
						   &ncount);
	}
        else if  (TWO_LEVELS == detection_alg ) {
            mca_coll_hierarch_checkfor_sm ( comm, &color, &ncount );
	}

        /* This is probably a no-no! but for the moment we agreed with Jeff,
	** that this might be the best solution. These functions emulate an 
        ** allreduce and  an allgather.
	*/
	ret = mca_coll_hierarch_allreduce_tmp (&ncount, &maxncount, 1, MPI_INT, 
					       MPI_MAX, comm );
	if ( OMPI_SUCCESS != ret ) {
	    return NULL;
	}

	if ( 0 == maxncount ) {
	    if ( mca_coll_hierarch_verbose_param ) {
		printf("%s:%d: nobody talks with %s. Continuing to next level.\n",  
		       comm->c_name, rank, hier_prot[level]);
	    }
	    continue;
	}
	else if ( maxncount == (size-1) ) {
	    /* 
	     * everybody can talk to every other process with this protocol, 
	     * no need to continue in the hierarchy tree and for the 
	     * hierarchical component.
	     * Its (size-1) because we do not count ourselves.
	     * maxncount[1] should be zero.
	     */
	    if ( mca_coll_hierarch_verbose_param ) {
		if ( ALL_LEVELS == detection_alg ) {
		    printf("%s:%d: everybody talks with %s. No need to continue\n", 
			   comm->c_name, rank, hier_prot[level]);
		}
		else if ( TWO_LEVELS == detection_alg ) {
		    printf("%s:%d: everybody talks with sm. No need to continue\n",
			   comm->c_name, rank );
		}
	    }
	    goto exit;
	}
	else {
	    if ( mca_coll_hierarch_verbose_param ) {
		printf("%s:%d: %d procs talk with %s. Use this protocol, key %d\n", 
		       comm->c_name, rank, maxncount, hier_prot[level], color);
	    }
	    
	    ret = mca_coll_hierarch_allgather_tmp (&color, 1, MPI_INT, 
						   hierarch_module->hier_colorarr, 1, 
						   MPI_INT, comm );
	    if ( OMPI_SUCCESS != ret ) {
		return NULL;
	    }
	    
	    hierarch_module->hier_level = level;
	    return &(hierarch_module->super);
	}
    }
        
 exit:
    *priority = 0;
    return NULL;
}
    

/*
 * Init module on the communicator
 */
int mca_coll_hierarch_module_enable (mca_coll_base_module_t *module,
				     struct ompi_communicator_t *comm)
{
    int color;
    int size, rank, ret=OMPI_SUCCESS;
    
    struct ompi_communicator_t *lcomm=NULL;
    struct ompi_communicator_t *llcomm=NULL;
    struct mca_coll_hierarch_llead_t *llead=NULL;
    mca_coll_hierarch_module_t *hierarch_module = (mca_coll_hierarch_module_t *) module;

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);
    
    color = hierarch_module->hier_colorarr[rank];
    
    /* Generate the subcommunicator based on the color returned by
       the previous function. */
    ret = ompi_comm_split ( comm, color, rank, &lcomm, 0 );
    if ( OMPI_SUCCESS != ret ) {
        goto exit;
    }
    if ( OMPI_COMM_CID_IS_LOWER ( lcomm, comm ) ) {
        /* Mark the communicator as 'extra retain' and increase the
           reference count by one more. See ompi_comm_activate
           for detailed comments
	*/
        OMPI_COMM_SET_EXTRA_RETAIN (lcomm);
        OBJ_RETAIN(lcomm);
    }
    
    hierarch_module->hier_comm     = comm;
    hierarch_module->hier_lcomm    = lcomm;
    hierarch_module->hier_num_reqs = 2 * size;
    hierarch_module->hier_reqs     = (ompi_request_t **) malloc (sizeof(ompi_request_t)*size*2);
    if ( NULL == hierarch_module->hier_reqs ) {
        goto exit;
    }
    
    /* allocate a certain number of the hierarch_llead structures, which store
       information about local leader and the according subcommunicators 
    */
    llead = (struct mca_coll_hierarch_llead_t * ) malloc ( 
                                                          sizeof(struct mca_coll_hierarch_llead_t));
    if ( NULL == llead ) {
        goto exit;
    }

    /* These two routines set all relevant entries in the mca_coll_base_comm_t 
     * structure. The first one makes all entries which are independent of the 
     * offset (and have to be done only once per module. The second one is 
     * depending on the offset, and has to be called therefore every time we need 
     * a new llcomm 
     */
    mca_coll_hierarch_get_llr ( hierarch_module );
    mca_coll_hierarch_get_all_lleaders ( rank, hierarch_module, llead, 1 );        
    
    /* Generate the lleader communicator assuming that all lleaders are the first
       process in the list of processes with the same color. A function generating 
       other lleader-comms will follow soon. */
    color = MPI_UNDEFINED;
    if ( llead->am_lleader ) {
	color = 1;
    }
    ret = ompi_comm_split ( comm, color, rank, &llcomm, 0);
    if ( OMPI_SUCCESS != ret ) {
        goto exit;
    }
    if ( OMPI_COMM_CID_IS_LOWER ( llcomm, comm ) ) {
        /* Mark the communicator as 'extra retain' and increase the
           reference count by one more. See ompi_comm_activate
	   for detailed explanation. 
	*/
        OMPI_COMM_SET_EXTRA_RETAIN (llcomm);
        OBJ_RETAIN(llcomm);
    }

    
    llead->llcomm = llcomm;
    
    /* Store it now on the data structure */
    OBJ_CONSTRUCT(&(hierarch_module->hier_llead), opal_pointer_array_t);
    opal_pointer_array_add ( &(hierarch_module->hier_llead), llead);
    
    if ( mca_coll_hierarch_verbose_param ) {
        mca_coll_hierarch_dump_struct (hierarch_module);
    }
    
 exit:
    if ( OMPI_SUCCESS != ret ) {
        if (NULL != llead) {
            free(llead);
        }
        ompi_comm_free ( &lcomm );
	return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}



int mca_coll_hierarch_get_all_lleaders ( int rank, mca_coll_hierarch_module_t *hierarch_module,
					 struct mca_coll_hierarch_llead_t * llead, 
					 int offset )
{
    int i, j, ret=OMPI_SUCCESS;
    int *cntarr=NULL;
    int mycolor;

    cntarr = (int *)calloc (1, sizeof (int)* hierarch_module->hier_num_lleaders );
    if ( NULL == cntarr ) {
	return OMPI_ERR_OUT_OF_RESOURCE;
    }

    llead->lleaders = (int *) malloc (sizeof(int) * hierarch_module->hier_num_lleaders);
    if ( NULL == llead->lleaders ) {
	ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }
    llead->offset = offset;

    for ( i=0; i < hierarch_module->hier_num_lleaders; i++ )  {
    	if ( MPI_UNDEFINED == hierarch_module->hier_llr[i] ) {
	    cntarr[i] = 1;
	    llead->lleaders[i] = MPI_UNDEFINED;
	}
    }

    for ( i=0; i<hierarch_module->hier_num_colorarr; i++) {
	if ( MPI_UNDEFINED == hierarch_module->hier_colorarr[i] ) {
	    continue;
	}
	for ( j=0; j<hierarch_module->hier_num_lleaders; j++) {	    
	    if ( cntarr[j] >= offset ) {
		continue;
	    }
	    if ( hierarch_module->hier_colorarr[i] == hierarch_module->hier_llr[j]) {
		cntarr[j]++;
		llead->lleaders[j] = i;
		break;
	    }
	}
    }

    mycolor = hierarch_module->hier_colorarr[rank];
    if ( MPI_UNDEFINED == mycolor ) {
	llead->am_lleader = 1;
	llead->my_lleader = MPI_UNDEFINED;
    }
    else {
	llead->am_lleader = 0;
	for ( i=0; i< hierarch_module->hier_num_lleaders; i++ ) {
	    if ( hierarch_module->hier_llr[i] == mycolor ) {
		llead->my_lleader = cntarr[i]-1;
		if ( llead->lleaders[i] == rank ) {
		    llead->am_lleader = 1; 
		}
		break;
	    }
	}
    }
 exit:
    if ( NULL != cntarr ) {
	free ( cntarr );
    }
    
    return ret;
}

int mca_coll_hierarch_get_llr ( mca_coll_hierarch_module_t *hierarch_module )
{
    int i, j, cnt, found;
    int ncount;

    ncount = mca_coll_hierarch_count_lleaders ( hierarch_module->hier_num_colorarr, 
						hierarch_module->hier_colorarr);
    hierarch_module->hier_num_lleaders = ncount;
    hierarch_module->hier_llr = (int *) malloc ( hierarch_module->hier_num_lleaders * sizeof(int));
    hierarch_module->hier_max_offset = (int *) calloc ( 1, hierarch_module->hier_num_lleaders * sizeof(int));
    if ( ( NULL == hierarch_module->hier_llr) || ( NULL == hierarch_module->hier_max_offset )) {
	return OMPI_ERR_OUT_OF_RESOURCE;
    }

    hierarch_module->hier_llr[0] = hierarch_module->hier_colorarr[0];
    hierarch_module->hier_max_offset[0]=1;
    for ( cnt=1, i=1; i<hierarch_module->hier_num_colorarr; i++ ) {
	if ( MPI_UNDEFINED == hierarch_module->hier_colorarr[i] ) { 
	    hierarch_module->hier_llr[cnt]     = hierarch_module->hier_colorarr[i];
	    hierarch_module->hier_max_offset[cnt] = 1;
	    cnt++;
	    continue;
	}
	for ( found=0, j=0; j<cnt; j++ ) {
	    if ( hierarch_module->hier_llr[j]  == hierarch_module->hier_colorarr[i]) {
		hierarch_module->hier_max_offset[j]++;
		found = 1;
		break;
	    }
	}
	if ( !found ) {
	    hierarch_module->hier_llr[cnt] = hierarch_module->hier_colorarr[i];
	    hierarch_module->hier_max_offset[cnt]++;
	    cnt++;
	}
    }

    return OMPI_SUCCESS;
}


struct ompi_communicator_t*  mca_coll_hierarch_get_llcomm (int root, 
                                                           mca_coll_hierarch_module_t *hierarch_module,
                                                           int* llroot,
                                                           int* lroot) 
{
    struct ompi_communicator_t *llcomm=NULL;
    struct ompi_group_t *llgroup=NULL;
    struct ompi_group_t *group=NULL;
    struct mca_coll_hierarch_llead_t *llead=NULL;
    int found, i, rc, num_llead, offset;
    int rank = ompi_comm_rank (hierarch_module->hier_comm);
    int color;
    
    /* determine what our offset of root is in the colorarr */
    offset = mca_coll_hierarch_get_offset ( root, 
					    hierarch_module->hier_num_colorarr, 
					    hierarch_module->hier_colorarr );
    
    num_llead = opal_pointer_array_get_size ( &(hierarch_module->hier_llead) );
    for ( found=0, i=0; i < num_llead; i++ ) {
        llead = (struct mca_coll_hierarch_llead_t *) opal_pointer_array_get_item (
                                                                                  &(hierarch_module->hier_llead), i );
	if ( NULL == llead ) {
            continue;
	}

	if (llead->offset == offset ) {
	    found = 1;
	    break;
	}
#if 0
	else if () {
            /* the offset of root = maxoffset of this color and
             * the offset on llead is larger then offset of root.
             * then we can also use this llead structure 
             */
	}
#endif
    }
    
    if ( !found ) {
	/* allocate a new llead element */
	llead = (struct mca_coll_hierarch_llead_t *) malloc (
                                                             sizeof(struct mca_coll_hierarch_llead_t));
	if ( NULL == llead ) {
	    return NULL;
	}
	
	/* generate the list of lleaders with this offset */
	mca_coll_hierarch_get_all_lleaders ( rank, hierarch_module, llead, offset );   
	color = MPI_UNDEFINED;
	if ( llead->am_lleader ) {
	    color = 1;
	}

	/* create new lleader subcommunicator */
	rc = ompi_comm_split ( hierarch_module->hier_comm, color, root, &llcomm, 0);
	if ( OMPI_SUCCESS != rc ) {
	    return NULL;
	}
	if ( OMPI_COMM_CID_IS_LOWER ( llcomm, hierarch_module->hier_comm ) ) {
            /* Mark the communicator as 'extra retain' and increase the
               reference count by one more. See ompi_comm_activate 
	       for detailed explanation. */
            OMPI_COMM_SET_EXTRA_RETAIN (llcomm);
            OBJ_RETAIN(llcomm);
        }


	llead->llcomm = llcomm;

	/* Store the new element on the hierarch_module struct */
	opal_pointer_array_add ( &(hierarch_module->hier_llead), llead);
    }

    llcomm = llead->llcomm;
    *lroot  = llead->my_lleader;
    *llroot = MPI_UNDEFINED;

    if ( MPI_COMM_NULL != llcomm ) {
	group   = hierarch_module->hier_comm->c_local_group;
	llgroup = llcomm->c_local_group;

        rc = ompi_group_translate_ranks ( group, 1, &root, llgroup, llroot);
        if ( OMPI_SUCCESS != rc ) {
            return NULL;
        }
    }
     
    return llcomm;
}


/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
static void
mca_coll_hierarch_checkfor_sm ( struct ompi_communicator_t *comm, int *color,  int *ncount )
{
    int i, size;
    int lncount=0;
    struct ompi_proc_t** procs=NULL;
    struct ompi_proc_t* my_proc=NULL;


    *color = -1;
    size = ompi_comm_size(comm);
    my_proc = ompi_proc_local();
    procs = comm->c_local_group->grp_proc_pointers;
    for ( i = 0 ; i < size ; i++) {
	if ( procs[i]->proc_name.jobid == my_proc->proc_name.jobid &&
	     ( OPAL_PROC_ON_LOCAL_NODE(procs[i]->proc_flags)) ) {
	    lncount++;
	    if ( *color == -1){
		 *color = i;
	    }
	}
    }

    /* we need to decrease ncount in order to make the other allreduce/allgather 
       operations work */
    lncount--;
    *ncount = lncount;
    return;
}


/* This function checks how many processes are using the component
   'component_name' for communication and returns this count in 
   'ncount'. Furthermore it returns a 'key', which can be used to split
   the communicator into subgroups, such that the new communicators
   will definitly have all processes communicate with this component.

   Oct 13: the algorithm has been modified such that it returns the 
   number of processes using the specified component and the number
   of processes to which an even 'faster' protocol is being used. (Faster
   specified in this context as being further up in the list of 
   hier_prot protocols specified at the beginning of this file).
*/
static void 
mca_coll_hierarch_checkfor_component ( struct ompi_communicator_t *comm,
				       int component_level,
				       char *component_name, 
				       int *key,
				       int *ncount )
{
    opal_bitmap_t reachable;
    ompi_proc_t **procs=NULL;
    struct mca_bml_base_btl_array_t *bml_btl_array=NULL;
    mca_bml_base_btl_t *bml_btl=NULL;
    mca_btl_base_component_t *btl=NULL;

    int i, size, rc;

    int counter=0;
    int firstproc=999999;
    int rank = -1;
    int use_rdma=0;

    /* default values in case an error occurs */
    *ncount=0;
    *key=MPI_UNDEFINED;

    /* Shall we check the the rdma list instead of send-list in the endpoint-structure? */
    use_rdma = mca_coll_hierarch_use_rdma_param;
    
    size = ompi_comm_size ( comm );
    rank = ompi_comm_rank ( comm );

    OBJ_CONSTRUCT(&reachable, opal_bitmap_t);
    rc = opal_bitmap_init(&reachable, size);
    if(OMPI_SUCCESS != rc) {
        return;
    }

    procs = comm->c_local_group->grp_proc_pointers;
    rc = mca_bml.bml_add_procs ( size, procs, &reachable );
    if(OMPI_SUCCESS != rc) {
        return;
    }

    for ( i=0; i<size; i++ ) {
        if ( rank ==  i ) {
            /* skip myself */
            continue;
        }
	
        if ( use_rdma ) {
            bml_btl_array = &(procs[i]->proc_bml->btl_rdma);
        }
        else {
            bml_btl_array = &(procs[i]->proc_bml->btl_send);
        }
        bml_btl = mca_bml_base_btl_array_get_index ( bml_btl_array, 0 );
        btl = bml_btl->btl->btl_component;

        /* sanity check */
        if ( strcmp(btl->btl_version.mca_type_name,"btl") ) {
            printf("Oops, got the wrong component! type_name = %s\n",
        	   btl->btl_version.mca_type_name );
        }
	    
        /* check for the required component */
        if (! strcmp (btl->btl_version.mca_component_name, component_name)){
            counter++;
	    if (i<firstproc ) {
                firstproc = i;
	    }
	    continue;
	}	    

    }

    *ncount = counter; 
    /* final decision */
    if ( counter == 0 ) {
        /* this is the section indicating, that we are not 
           using this component */
        firstproc = MPI_UNDEFINED;
    }
    else {
        if ( rank < firstproc ) {
            firstproc = rank;
        }
    }

    *key = firstproc;

    return;
}


/********************************************************************************/
/********************************************************************************/
/********************************************************************************/

static void mca_coll_hierarch_dump_struct ( mca_coll_hierarch_module_t *c)
{
    int i, j;
    int rank;
    struct mca_coll_hierarch_llead_t *current=NULL;

    rank = ompi_comm_rank ( c->hier_comm );

    printf("%d: Dump of hier-struct for  comm %s cid %u\n", 
           rank, c->hier_comm->c_name, c->hier_comm->c_contextid);

    printf("%d: No of llead communicators: %d No of lleaders: %d\n", 
	   rank, opal_pointer_array_get_size ( &(c->hier_llead)),
	   c->hier_num_lleaders );

    for ( i=0; i < opal_pointer_array_get_size(&(c->hier_llead)); i++ ) {
	current = (mca_coll_hierarch_llead_t*)opal_pointer_array_get_item (&(c->hier_llead), i);
	if ( current == NULL ) {
            continue;
	}

	printf("%d:  my_leader %d am_leader %d\n", rank,
               current->my_lleader, current->am_lleader );

        for (j=0; j<c->hier_num_lleaders; j++ ) {
            printf("%d:      lleader[%d] = %d\n", rank, j, current->lleaders[j]);
        }
    }
    
    return;
}

int mca_coll_hierarch_ft_event(int state) {
    if(OPAL_CRS_CHECKPOINT == state) {
        ;
    }
    else if(OPAL_CRS_CONTINUE == state) {
        ;
    }
    else if(OPAL_CRS_RESTART == state) {
        ;
    }
    else if(OPAL_CRS_TERM == state ) {
        ;
    }
    else {
        ;
    }

    return OMPI_SUCCESS;
}
