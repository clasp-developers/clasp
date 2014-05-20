/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2011 University of Houston. All rights reserved.
 * Copyright (c) 2007-2008 Cisco Systems, Inc. All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include <string.h>
#include <stdio.h>

#include "ompi/constants.h"

#include "opal/dss/dss.h"
#include "orte/util/name_fns.h"
#include "orte/mca/rml/rml_types.h"

#include "ompi/proc/proc.h"
#include "opal/threads/mutex.h"
#include "opal/util/bit_ops.h"
#include "opal/util/output.h"
#include "ompi/mca/topo/topo.h"
#include "ompi/mca/topo/base/base.h"
#include "ompi/mca/dpm/dpm.h"

#include "ompi/attribute/attribute.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/request/request.h"

/*
** sort-function for MPI_Comm_split 
*/
static int rankkeycompare(const void *, const void *);

/**
 * to fill the rest of the stuff for the communicator when either
 * MPI_Cart_create or MPI_Graph_create is used 
 */ 
static int ompi_comm_fill_rest (ompi_communicator_t *comm,
                                int num_procs,
                                ompi_proc_t **proc_pointers,
                                int my_rank,
                                ompi_errhandler_t *errh );
/*
** typedef for the allgather_intra required in comm_split.
** the reason for introducing this abstraction is, that
** for Comm_split for inter-coms, we do not have this
** functions, so we need to emulate it.
*/
typedef int ompi_comm_allgatherfct (void* inbuf, int incount, MPI_Datatype intype,
                                    void* outbuf, int outcount, MPI_Datatype outtype,
                                    ompi_communicator_t *comm,
                                    mca_coll_base_module_t *data);

static int ompi_comm_allgather_emulate_intra (void* inbuf, int incount, MPI_Datatype intype,
                                              void* outbuf, int outcount, 
                                              MPI_Datatype outtype, 
                                              ompi_communicator_t *comm,
                                              mca_coll_base_module_t *data);

static int ompi_comm_copy_topo (ompi_communicator_t *oldcomm, 
                                ompi_communicator_t *newcomm);

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/* 
 * This is the function setting all elements of a communicator.
 * All other routines are just used to determine these elements.
 */   

int ompi_comm_set ( ompi_communicator_t **ncomm, 
                    ompi_communicator_t *oldcomm,
                    int local_size, 
                    int *local_ranks,
                    int remote_size,
                    int *remote_ranks,
                    opal_hash_table_t *attr,
                    ompi_errhandler_t *errh,
                    mca_base_component_t *topocomponent, 
                    ompi_group_t *local_group, 
                    ompi_group_t *remote_group )

{
    ompi_communicator_t *newcomm=NULL;
    int ret;
    
    /* ompi_comm_allocate */
    newcomm = OBJ_NEW(ompi_communicator_t);
    /* fill in the inscribing hyper-cube dimensions */
    newcomm->c_cube_dim = opal_cube_dim(local_size);
    newcomm->c_id_available   = MPI_UNDEFINED;
    newcomm->c_id_start_index = MPI_UNDEFINED;

    if (NULL == local_group) {
        /* determine how the list of local_rank can be stored most
           efficiently */
        ret = ompi_group_incl(oldcomm->c_local_group, local_size, 
                              local_ranks, &newcomm->c_local_group);
    }
    else {
        newcomm->c_local_group = local_group;
        OBJ_RETAIN(newcomm->c_local_group);
        ompi_group_increment_proc_count(newcomm->c_local_group);
    }
    newcomm->c_my_rank = newcomm->c_local_group->grp_my_rank;
    
    /* Set remote group and duplicate the local comm, if applicable */
    if ( 0 < remote_size) {     
        if ( NULL == remote_group ) {
            ret = ompi_group_incl(oldcomm->c_remote_group, remote_size, 
                                  remote_ranks, &newcomm->c_remote_group);
        }
        else {
            newcomm->c_remote_group = remote_group;
            OBJ_RETAIN(newcomm->c_remote_group);
            ompi_group_increment_proc_count(newcomm->c_remote_group);
        }
        newcomm->c_flags |= OMPI_COMM_INTER;
        if ( OMPI_COMM_IS_INTRA(oldcomm) ) {
            ompi_comm_dup(oldcomm, &newcomm->c_local_comm);
        } else {
            ompi_comm_dup(oldcomm->c_local_comm, &newcomm->c_local_comm);
        }
    }
    else { 
        newcomm->c_remote_group = newcomm->c_local_group;
        OBJ_RETAIN(newcomm->c_remote_group);
    }
    
    /* Check how many different jobids are represented in this communicator.
       Necessary for the disconnect of dynamic communicators. */

    if ( 0 < local_size  ) {
	ompi_dpm.mark_dyncomm (newcomm);
    }

    /* Set error handler */
    newcomm->error_handler = errh;
    OBJ_RETAIN ( newcomm->error_handler );
    
    /* Set Topology, if required */
    
    if ( NULL != topocomponent ) {
        /*
         * This functions is never used to determine the topology
         * component. The topology component is determined only by the
         * ompi_cart_create and ompi_comm_create functions. Have to
         * see what ahppens during MPI_Comm_dup though. During this
         * the topology information has to be copied into the new
         * communicator which includes selecting a new topology
         * component and setting the information which is on that
         * communicator into this communicator. This probably is
         * another function in this file.
         */ 
 
        if (OMPI_COMM_IS_CART ( oldcomm ) ) {
            newcomm->c_flags |= OMPI_COMM_CART;
        }
        if (OMPI_COMM_IS_GRAPH ( oldcomm ) ) {
            newcomm->c_flags |= OMPI_COMM_GRAPH;
        }

        /*
         * Now I have to set the information on the topology from the previous
         * communicator
         */ 
 
        /* allocate the data for the common good */
        newcomm->c_topo_comm = (mca_topo_base_comm_t *)malloc(sizeof(mca_topo_base_comm_t));
 
        if (NULL == newcomm->c_topo_comm) {
            OBJ_RELEASE(newcomm);
            return OMPI_ERROR;
        }
 
        if (OMPI_SUCCESS != (ret = mca_topo_base_comm_select (newcomm, 
                                                              oldcomm->c_topo_component))) {
            free(newcomm->c_topo_comm); 
            OBJ_RELEASE(newcomm);
            return ret;
        }
        
        /*
         * Should copy over the information from the previous communicator
         */
        if (OMPI_SUCCESS != (ret = ompi_comm_copy_topo (oldcomm, newcomm))) {
            OBJ_RELEASE(newcomm);
            return ret;
        }
    }
      
    /* Copy attributes and call according copy functions, if
       required */
    
    if (NULL != oldcomm->c_keyhash) {
        if (NULL != attr) {
            ompi_attr_hash_init(&newcomm->c_keyhash);
            if (OMPI_SUCCESS != (ret = ompi_attr_copy_all (COMM_ATTR, oldcomm,
                                                           newcomm, attr, 
                                                           newcomm->c_keyhash))) {
                OBJ_RELEASE(newcomm);
                return ret;
            }                                    
        }
    }
      
   *ncomm = newcomm;
    return (OMPI_SUCCESS);
}


/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/*
** Counterpart to MPI_Comm_group. To be used within OMPI functions.
*/
int ompi_comm_group ( ompi_communicator_t* comm, ompi_group_t **group )
{
    /* increment reference counters for the group */
    OBJ_RETAIN(comm->c_local_group);

    /* increase also the reference counter for the procs */
    ompi_group_increment_proc_count(comm->c_local_group);

    *group = comm->c_local_group;
    return OMPI_SUCCESS;
}

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/*
** Counterpart to MPI_Comm_create. To be used within OMPI.
*/
int ompi_comm_create ( ompi_communicator_t *comm, ompi_group_t *group, 
                      ompi_communicator_t **newcomm )
{
    ompi_communicator_t *newcomp = NULL;
    int rsize , lsize;
    int mode,i,j;
    int *allranks=NULL;
    int *rranks=NULL;
    int rc = OMPI_SUCCESS;
    
    lsize = group->grp_proc_count;
 
    if ( OMPI_COMM_IS_INTER(comm) ) {
        int tsize;
 
        tsize = ompi_comm_remote_size(comm);
        allranks = (int *) malloc ( tsize * sizeof(int));
        if ( NULL == allranks ) {
            rc = OMPI_ERR_OUT_OF_RESOURCE;
            goto exit;
        }
        
        rc = comm->c_coll.coll_allgather ( &(group->grp_my_rank), 
                                           1, MPI_INT, allranks, 
                                           1, MPI_INT, comm,
                                           comm->c_coll.coll_allgather_module);
        if ( OMPI_SUCCESS != rc ) {
            goto exit;
        }

        /* Count number of procs in future remote group */
        for (rsize=0, i = 0; i < tsize; i++) {
            if ( MPI_UNDEFINED != allranks[i] ) {
                rsize++;
            }
        }

        /* If any of those groups is empty, we have to return
           MPI_COMM_NULL */
        if ( 0 == rsize || 0 == group->grp_proc_count ) {
            newcomp = MPI_COMM_NULL;
            rc = OMPI_SUCCESS;
            goto exit;
        }

        /* Set proc-pointers for remote group */
        rranks = (int *) malloc ( rsize * sizeof(int));
        if ( NULL == rranks ) {
            rc = OMPI_ERR_OUT_OF_RESOURCE;
            goto exit;
        }

        for ( j = 0, i = 0; i < tsize; i++ ) {
            if ( MPI_UNDEFINED != allranks[i] ) {
                rranks[j] = i;
                j++;
            }
        }                                           
        mode = OMPI_COMM_CID_INTER;

    } else {
        rsize  = 0;
        rranks = NULL;
        mode   = OMPI_COMM_CID_INTRA;
    }
    
    rc = ompi_comm_set ( &newcomp,                 /* new comm */
                         comm,                     /* old comm */
                         lsize,                    /* local_size */
                         NULL,                     /* local_ranks */
                         rsize,                    /* remote_size */
                         rranks,                   /* remote_ranks */
                         NULL,                     /* attrs */
                         comm->error_handler,      /* error handler */
                         NULL,                     /* topo component */
                         group,                    /* local group */
                         NULL                      /* remote group */
                         );
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    if ( NULL == newcomm ) {
        rc =  MPI_ERR_INTERN;
        goto exit;
    }

    /* Determine context id. It is identical to f_2_c_handle */
    rc = ompi_comm_nextcid ( newcomp,  /* new communicator */ 
                             comm,     /* old comm */
                             NULL,     /* bridge comm */
                             NULL,     /* local leader */
                             NULL,     /* remote_leader */
                             mode,     /* mode */
                             -1 );     /* send first */
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    /* Set name for debugging purposes */
    snprintf(newcomp->c_name, MPI_MAX_OBJECT_NAME, "MPI COMMUNICATOR %d CREATE FROM %d", 
             newcomp->c_contextid, comm->c_contextid );

    /* Activate the communicator and init coll-component */
    rc = ompi_comm_activate( &newcomp, /* new communicator */ 
	                     comm, 
	                     NULL, 
	                     NULL, 
			     NULL,
	                     mode, 
	                     -1 );  
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }


    /* Check whether we are part of the new comm.
       If not, we have to free the structure again.
       However, we could not avoid the comm_nextcid step, since
       all processes of the original comm have to participate in
       that function call. Additionally, all errhandler stuff etc.
       has to be set to make ompi_comm_free happy */
    if ( MPI_UNDEFINED == newcomp->c_local_group->grp_my_rank ) {
        ompi_comm_free ( &newcomp );
    }
    
 exit:
    if ( NULL != allranks ) {
        free ( allranks );
    }
    if ( NULL != rranks ) {
        free ( rranks );
    }

    *newcomm = newcomp;
    return ( rc );
}


/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/*
** Counterpart to MPI_Comm_split. To be used within OMPI (e.g. MPI_Cart_sub).
*/
int ompi_comm_split ( ompi_communicator_t* comm, int color, int key, 
                     ompi_communicator_t **newcomm, bool pass_on_topo )
{
    int myinfo[2];
    int size, my_size;
    int my_rsize;
    int mode;
    int rsize;
    int i, loc;
    int inter;
    int *results=NULL, *sorted=NULL; 
    int *rresults=NULL, *rsorted=NULL; 
    int rc=OMPI_SUCCESS;
    ompi_communicator_t *newcomp = NULL;
    int *lranks=NULL, *rranks=NULL;
    
    ompi_comm_allgatherfct *allgatherfct=NULL;

    /* Step 1: determine all the information for the local group */
    /* --------------------------------------------------------- */

    /* sort according to color and rank. Gather information from everyone */
    myinfo[0] = color;
    myinfo[1] = key;

    size     = ompi_comm_size ( comm );
    inter    = OMPI_COMM_IS_INTER(comm);
    if ( inter ) {
        allgatherfct = (ompi_comm_allgatherfct *)ompi_comm_allgather_emulate_intra;
    } else {
        allgatherfct = (ompi_comm_allgatherfct *)comm->c_coll.coll_allgather;
    }

    results  = (int*) malloc ( 2 * size * sizeof(int));
    if ( NULL == results ) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    rc = allgatherfct( myinfo, 2, MPI_INT, results, 2, MPI_INT, comm, comm->c_coll.coll_allgather_module );
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }
        
    /* how many have the same color like me */
    for ( my_size = 0, i=0; i < size; i++) {
        if ( results[(2*i)+0] == color) {
            my_size++;
        }
    }

    sorted = (int *) malloc ( sizeof( int ) * my_size * 2);
    if ( NULL == sorted) {
        rc =  OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }
    
    /* ok we can now fill this info */
    for( loc = 0, i = 0; i < size; i++ ) {
        if ( results[(2*i)+0] == color) {
            sorted[(2*loc)+0] = i;                 /* copy org rank */
            sorted[(2*loc)+1] = results[(2*i)+1];  /* copy key */
            loc++;
        }
    }
    
    /* the new array needs to be sorted so that it is in 'key' order */
    /* if two keys are equal then it is sorted in original rank order! */
    if(my_size>1){
        qsort ((int*)sorted, my_size, sizeof(int)*2, rankkeycompare);
    }

    /* put group elements in a list */
    lranks = (int *) malloc ( my_size * sizeof(int));
    if ( NULL == lranks ) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }
    for (i = 0; i < my_size; i++) {
        lranks[i] = sorted[i*2];
    }  
            
    /* Step 2: determine all the information for the remote group */
    /* --------------------------------------------------------- */
    if ( inter ) {
        rsize    = comm->c_remote_group->grp_proc_count;
        rresults = (int *) malloc ( rsize * 2 * sizeof(int));
        if ( NULL == rresults ) {
            rc = OMPI_ERR_OUT_OF_RESOURCE;
            goto exit;
        }

        /* this is an allgather on an inter-communicator */
        rc = comm->c_coll.coll_allgather( myinfo, 2, MPI_INT, rresults, 2, 
                                          MPI_INT, comm,
                                          comm->c_coll.coll_allgather_module);
        if ( OMPI_SUCCESS != rc ) {
            goto exit;
        }

        /* how many have the same color like me */
        for ( my_rsize = 0, i=0; i < rsize; i++) {
            if ( rresults[(2*i)+0] == color) {
                my_rsize++;
            }
        }
        rsorted = (int *) malloc ( sizeof( int ) * my_rsize * 2);
        if ( NULL == rsorted) {
            rc = OMPI_ERR_OUT_OF_RESOURCE;
            goto exit;
        }
        
        /* ok we can now fill this info */
        for( loc = 0, i = 0; i < rsize; i++ ) {
            if ( rresults[(2*i)+0] == color) {
                rsorted[(2*loc)+0] = i;                  /* org rank */
                rsorted[(2*loc)+1] = rresults[(2*i)+1];  /* key */
                loc++;
            }
        }
        
        /* the new array needs to be sorted so that it is in 'key' order */
        /* if two keys are equal then it is sorted in original rank order! */
        if(my_rsize>1) {
            qsort ((int*)rsorted, my_rsize, sizeof(int)*2, rankkeycompare);
        }

        /* put group elements in a list */
        rranks = (int *) malloc ( my_rsize * sizeof(int));
        if ( NULL ==  rranks) {
            rc = OMPI_ERR_OUT_OF_RESOURCE;
            goto exit;
        }
 
        for (i = 0; i < my_rsize; i++) {
            rranks[i] = rsorted[i*2];
        }  
        mode = OMPI_COMM_CID_INTER;
    } else {
        my_rsize  = 0;
        rranks = NULL;
        mode      = OMPI_COMM_CID_INTRA;
    }
    
    
    /* Step 3: set up the communicator                           */
    /* --------------------------------------------------------- */
    /* Create the communicator finally */

    rc = ompi_comm_set ( &newcomp,           /* new comm */
                         comm,               /* old comm */
                         my_size,            /* local_size */
                         lranks,             /* local_ranks */
                         my_rsize,           /* remote_size */
                         rranks,             /* remote_ranks */
                         NULL,               /* attrs */
                         comm->error_handler,/* error handler */
                         (pass_on_topo)?  
                         (mca_base_component_t *)comm->c_topo_component:
                         NULL,               /* topo component */
                         NULL,               /* local group */
                         NULL                /* remote group */
    );

    if ( NULL == newcomm ) {
        rc =  MPI_ERR_INTERN;
        goto exit;
    }
    if ( OMPI_SUCCESS != rc  ) {
        goto exit;
    }

    /* Determine context id. It is identical to f_2_c_handle */
    rc = ompi_comm_nextcid ( newcomp,  /* new communicator */ 
                             comm,     /* old comm */
                             NULL,     /* bridge comm */
                             NULL,     /* local leader */
                             NULL,     /* remote_leader */
                             mode,     /* mode */
                             -1 );     /* send first, doesn't matter */
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    /* Set name for debugging purposes */
    snprintf(newcomp->c_name, MPI_MAX_OBJECT_NAME, "MPI COMMUNICATOR %d SPLIT FROM %d", 
             newcomp->c_contextid, comm->c_contextid );

    /* set the rank to MPI_UNDEFINED. This prevents in comm_activate
     * the collective module selection for a communicator that will
     * be freed anyway.
     */
    if ( MPI_UNDEFINED == color ) {
	newcomp->c_local_group->grp_my_rank = MPI_UNDEFINED;
    }


    /* Activate the communicator and init coll-component */
    rc = ompi_comm_activate( &newcomp, /* new communicator */ 
			     comm, 
			     NULL,
			     NULL, 
			     NULL, 
			     mode, 
			     -1 );  
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

 exit:
    if ( NULL != results ) {
        free ( results );
    }
    if ( NULL != sorted  ) {
        free ( sorted );
    }
    if ( NULL != rresults) {
        free ( rresults );
    }
    if ( NULL != rsorted ) {
      free ( rsorted );
    }
    if ( NULL != lranks   ) {
        free ( lranks );
    }
    if ( NULL != rranks  ) {
        free ( rranks );
    }

    /* Step 4: if we are not part of the comm, free the struct   */
    /* --------------------------------------------------------- */
    if ( NULL != newcomp && MPI_UNDEFINED == color ) {
        ompi_comm_free ( &newcomp );
    }

    *newcomm = newcomp;
    return ( rc );
}
/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
int ompi_comm_dup ( ompi_communicator_t * comm, ompi_communicator_t **newcomm )
{
    ompi_communicator_t *comp=NULL;
    ompi_communicator_t *newcomp=NULL;
    int rsize, mode, rc=OMPI_SUCCESS;

    comp = (ompi_communicator_t *) comm;
    if ( OMPI_COMM_IS_INTER ( comp ) ){
        rsize  = comp->c_remote_group->grp_proc_count;
        mode   = OMPI_COMM_CID_INTER;
    } else {
        rsize  = 0;
        mode   = OMPI_COMM_CID_INTRA;
    }
    
    *newcomm = MPI_COMM_NULL;

    rc =  ompi_comm_set ( &newcomp,                               /* new comm */
                          comp,                                   /* old comm */
                          comp->c_local_group->grp_proc_count,    /* local_size */
                          NULL,                                   /* local_procs*/
                          rsize,                                  /* remote_size */
                          NULL,                                   /* remote_procs */
                          comp->c_keyhash,                        /* attrs */
                          comp->error_handler,                    /* error handler */
                          (mca_base_component_t *) comp->c_topo_component,                  
                          /* topo component */
                          comp->c_local_group,                    /* local group */
                          comp ->c_remote_group );                /* remote group */
    if ( NULL == newcomm ) {
        rc =  MPI_ERR_INTERN;
        return rc;
    }
    if ( MPI_SUCCESS != rc) { 
        return rc;
    }

    /* Determine context id. It is identical to f_2_c_handle */
    rc = ompi_comm_nextcid ( newcomp,  /* new communicator */ 
                             comp,     /* old comm */
                             NULL,     /* bridge comm */
                             NULL,     /* local leader */
                             NULL,     /* remote_leader */
                             mode,     /* mode */
                             -1 );     /* send_first */
    if ( OMPI_SUCCESS != rc ) {
        return rc;
    }

    /* Set name for debugging purposes */
    snprintf(newcomp->c_name, MPI_MAX_OBJECT_NAME, "MPI COMMUNICATOR %d DUP FROM %d", 
             newcomp->c_contextid, comm->c_contextid );

    /* activate communicator and init coll-module */
    rc = ompi_comm_activate( &newcomp, /* new communicator */ 
			     comp,
	                     NULL, 
	                     NULL, 
	                     NULL, 
	                     mode, 
	                     -1 );  
    if ( OMPI_SUCCESS != rc ) {
        return rc;
    }
    
    *newcomm = newcomp;
    return MPI_SUCCESS;
}
/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
int ompi_comm_compare(ompi_communicator_t *comm1, ompi_communicator_t *comm2, int *result) {
    /* local variables */
    ompi_communicator_t *comp1, *comp2;
    ompi_group_t *group1, *group2;
    int size1, size2, rsize1, rsize2;
    int lresult, rresult=MPI_CONGRUENT;
    int sameranks=1;
    int sameorder=1;
    int i, j;
    int found = 0;
    ompi_proc_t * proc1, * proc2;
    
    comp1 = (ompi_communicator_t *) comm1;
    comp2 = (ompi_communicator_t *) comm2;

    if ( comp1->c_contextid == comp2->c_contextid ) {
        *result = MPI_IDENT;
        return MPI_SUCCESS;
    }

    if ( MPI_COMM_NULL == comm1 || MPI_COMM_NULL == comm2 ) {
        *result = MPI_UNEQUAL;
        return MPI_SUCCESS;
    }

    /* compare sizes of local and remote groups */
    size1 = ompi_comm_size (comp1);
    size2 = ompi_comm_size (comp2);
    rsize1 = ompi_comm_remote_size (comp1);
    rsize2 = ompi_comm_remote_size (comp2);

    if ( size1 != size2 || rsize1 != rsize2 ) {
        *result = MPI_UNEQUAL;
        return MPI_SUCCESS;
    }
        
    /* Compare local groups */
    /* we need to check whether the communicators contain
       the same processes and in the same order */
    group1 = (ompi_group_t *)comp1->c_local_group;
    group2 = (ompi_group_t *)comp2->c_local_group;
    for ( i = 0; i < size1; i++ ) {
        proc1 = ompi_group_peer_lookup(group1,i);
        proc2 = ompi_group_peer_lookup(group2,i);
        if ( proc1 != proc2) {
            sameorder = 0;
            break;
        }
    }
    
    for ( i = 0; i < size1; i++ ) {
        found = 0;
        for ( j = 0; j < size2; j++ ) {
            proc1 = ompi_group_peer_lookup(group1,i);
            proc2 = ompi_group_peer_lookup(group2,j);
            if ( proc1 == proc2) {
                found = 1;
                break;
            }
        }
        if ( !found  ) {
            sameranks = 0;
            break;
        }
    }
    
    if ( sameranks && sameorder )
        lresult = MPI_CONGRUENT;
    else if ( sameranks && !sameorder )
        lresult = MPI_SIMILAR;
    else
        lresult = MPI_UNEQUAL;


    if ( rsize1 > 0 ) {        
        /* Compare remote groups for inter-communicators */
        /* we need to check whether the communicators contain
           the same processes and in the same order */
        sameranks = sameorder = 1;

        group1 = (ompi_group_t *)comp1->c_remote_group;
        group2 = (ompi_group_t *)comp2->c_remote_group;
        for ( i = 0; i < rsize1; i++ ) {
            proc1 = ompi_group_peer_lookup(group1,i);
            proc2 = ompi_group_peer_lookup(group2,i);
            if ( proc1 != proc2) {
                sameorder = 0;
                break;
            }
        }

        for ( i = 0; i < rsize1; i++ ) {
            found = 0;
            for ( j = 0; j < rsize2; j++ ) {
                proc1 = ompi_group_peer_lookup(group1,i);
                proc2 = ompi_group_peer_lookup(group2,j);
                if ( proc1 == proc2) {
                    found = 1;
                    break;
                }
            }
            if ( !found  ) {
                sameranks = 0;
                break;
            }
        }
        
        if ( sameranks && sameorder )
            rresult = MPI_CONGRUENT;
        else if ( sameranks && !sameorder )
            rresult = MPI_SIMILAR;
        else
            rresult = MPI_UNEQUAL;
    }

    /* determine final results */
    if ( MPI_CONGRUENT == rresult ) {
        *result = lresult;
    }
    else if ( MPI_SIMILAR == rresult ) {
        if ( MPI_SIMILAR == lresult || MPI_CONGRUENT == lresult ) {
            *result = MPI_SIMILAR;
        }
        else 
            *result = MPI_UNEQUAL;
    }
    else if ( MPI_UNEQUAL == rresult ) 
        *result = MPI_UNEQUAL;

    return OMPI_SUCCESS;
}
/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
int ompi_comm_set_name (ompi_communicator_t *comm, char *name )
{

#ifdef USE_MUTEX_FOR_COMMS
    OPAL_THREAD_LOCK(&(comm->c_lock));
#endif
    memset(comm->c_name, 0, MPI_MAX_OBJECT_NAME);
    strncpy(comm->c_name, name, MPI_MAX_OBJECT_NAME);
    comm->c_name[MPI_MAX_OBJECT_NAME - 1] = 0;
    comm->c_flags |= OMPI_COMM_NAMEISSET;
#ifdef USE_MUTEX_FOR_COMMS
    OPAL_THREAD_UNLOCK(&(comm->c_lock));
#endif

    return OMPI_SUCCESS;
}
/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/* 
 * Implementation of MPI_Allgather for the local_group in an inter-comm.
 * The algorithm consists of two steps: 
 * 1. an inter-gather to rank 0 in remote group
 * 2. an inter-bcast from rank 0 in remote_group.
 */

static int ompi_comm_allgather_emulate_intra( void *inbuf, int incount, 
                                              MPI_Datatype intype, void* outbuf,
                                              int outcount, MPI_Datatype outtype,
                                              ompi_communicator_t *comm,
                                              mca_coll_base_module_t *data)
{
    int rank, size, rsize, i, rc;
    int *tmpbuf=NULL;
    MPI_Request *req=NULL, sendreq;

    rsize = ompi_comm_remote_size(comm);
    size  = ompi_comm_size(comm);
    rank  = ompi_comm_rank(comm);

    /* Step 1: the gather-step */
    if ( 0 == rank ) {
        tmpbuf = (int *) malloc (rsize*outcount*sizeof(int));
        req = (MPI_Request *)malloc (rsize*outcount*sizeof(MPI_Request));
        if ( NULL == tmpbuf || NULL == req ) {
            return (OMPI_ERR_OUT_OF_RESOURCE);
        }

        for ( i=0; i<rsize; i++) {
            rc = MCA_PML_CALL(irecv( &tmpbuf[outcount*i], outcount, outtype, i,
                                     OMPI_COMM_ALLGATHER_TAG, comm, &req[i] ));
            if ( OMPI_SUCCESS != rc ) {
                goto exit;       
            }
        }
    }        
    rc = MCA_PML_CALL(isend( inbuf, incount, intype, 0, OMPI_COMM_ALLGATHER_TAG,
                             MCA_PML_BASE_SEND_STANDARD, comm, &sendreq ));
    if ( OMPI_SUCCESS != rc ) {
        goto exit;       
    }
        
    if ( 0 == rank ) {
        rc = ompi_request_wait_all (rsize, req, MPI_STATUSES_IGNORE);
        if ( OMPI_SUCCESS != rc ) {
            goto exit;       
        }
    }

    rc = ompi_request_wait_all (1, &sendreq, MPI_STATUS_IGNORE);
    if ( OMPI_SUCCESS != rc ) {
        goto exit;       
    }

    /* Step 2: the inter-bcast step */
    rc = MCA_PML_CALL(irecv (outbuf, size*outcount, outtype, 0, 
                            OMPI_COMM_ALLGATHER_TAG, comm, &sendreq));
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    if ( 0 == rank ) {
        for ( i=0; i < rsize; i++ ){
            rc = MCA_PML_CALL(send (tmpbuf, rsize*outcount, outtype, i, 
                                   OMPI_COMM_ALLGATHER_TAG, 
                                   MCA_PML_BASE_SEND_STANDARD, comm));
            if ( OMPI_SUCCESS != rc ) {
                goto exit;       
            }
        }
    }

    rc = ompi_request_wait_all (1, &sendreq, MPI_STATUS_IGNORE );

 exit:
    if ( NULL != req ) {
        free ( req );
    }
    if ( NULL != tmpbuf ) {
        free ( tmpbuf );
    }

    return (rc);
}
/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/*
** Counterpart to MPI_Comm_free. To be used within OMPI.
** The freeing of all attached objects (groups, errhandlers
** etc. ) has moved to the destructor. 
*/
int ompi_comm_free ( ompi_communicator_t **comm )
{
    int ret;
    int cid = (*comm)->c_contextid;
    int is_extra_retain = OMPI_COMM_IS_EXTRA_RETAIN(*comm);


    /* Release attributes.  We do this now instead of during the
       communicator destructor for 2 reasons:

       1. The destructor will only NOT be called immediately during
          ompi_comm_free() if the reference count is still greater
          than zero at that point, meaning that there are ongoing
          communications.  However, pending communications will never
          need attributes, so it's safe to release them directly here.

       2. Releasing attributes in ompi_comm_free() enables us to check
          the return status of the attribute delete functions.  At
          least one interpretation of the MPI standard (i.e., the one
          of the Intel test suite) is that if any of the attribute
          deletion functions fail, then MPI_COMM_FREE /
          MPI_COMM_DISCONNECT should also fail.  We can't do that if
          we delay releasing the attributes -- we need to release the
          attributes right away so that we can report the error right
          away. */
    if ( OMPI_COMM_IS_INTER(*comm) ) {
        ompi_comm_free (&(*comm)->c_local_comm);
    }

    if (NULL != (*comm)->c_keyhash) {
        ret = ompi_attr_delete_all(COMM_ATTR, *comm, (*comm)->c_keyhash);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
        OBJ_RELEASE((*comm)->c_keyhash);
    }

    /* Special case: if we are freeing the parent handle, then we need
       to set our internal handle to the parent to be equal to
       COMM_NULL.  This is according to MPI-2:88-89. */

    if (*comm == ompi_mpi_comm_parent && comm != &ompi_mpi_comm_parent) {
        ompi_mpi_comm_parent = &ompi_mpi_comm_null.comm;
    }

    /* Release the communicator */
    if ( OMPI_COMM_IS_DYNAMIC (*comm) ) {
        ompi_comm_num_dyncomm --;
    }
    OBJ_RELEASE ( (*comm) );

    if ( is_extra_retain) {
        /* This communicator has been marked as an "extra retain"
         * communicator. This can happen if a communicator creates
         * 'dependent' subcommunicators (e.g. for inter
         * communicators or when using hierarch collective
         * module *and* the cid of the dependent communicator
         * turned out to be lower than of the parent one.
         * In that case, the reference counter has been increased
         * by one more, in order to handle the scenario,
         * that the user did not free the communicator.
         * Note, that if we enter this routine, we can
         * decrease the counter by one more therefore. However,
         * in ompi_comm_finalize, we only used OBJ_RELEASE instead
         * of ompi_comm_free(), and the increased reference counter
         * makes sure that the pointer to the dependent communicator
         * still contains a valid object.
         */
        ompi_communicator_t *tmpcomm = (ompi_communicator_t *) opal_pointer_array_get_item(&ompi_mpi_communicators, cid);
        if ( NULL != tmpcomm ){
            OBJ_RELEASE (tmpcomm);
        }
    }


    *comm = MPI_COMM_NULL;
    return OMPI_SUCCESS;
}

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
ompi_proc_t **ompi_comm_get_rprocs ( ompi_communicator_t *local_comm, 
                                     ompi_communicator_t *bridge_comm, 
                                     int local_leader,
                                     int remote_leader,
                                     orte_rml_tag_t tag,
                                     int rsize)
{

    MPI_Request req;
    int rc;
    int local_rank, local_size;
    ompi_proc_t **rprocs=NULL;
    orte_std_cntr_t size_len;
    int int_len, rlen;
    opal_buffer_t *sbuf=NULL, *rbuf=NULL;
    void *sendbuf;
    char *recvbuf;
    ompi_proc_t **proc_list=NULL;
    int i;
    
    local_rank = ompi_comm_rank (local_comm);
    local_size = ompi_comm_size (local_comm);    

    if (local_rank == local_leader) {
        sbuf = OBJ_NEW(opal_buffer_t);
        if (NULL == sbuf) {
            rc = ORTE_ERROR;
            goto err_exit;
        } 
        if(OMPI_GROUP_IS_DENSE(local_comm->c_local_group)) {
            rc = ompi_proc_pack(local_comm->c_local_group->grp_proc_pointers, 
                                local_size, sbuf);
        }
        /* get the proc list for the sparse implementations */
        else { 
            proc_list = (ompi_proc_t **) calloc (local_comm->c_local_group->grp_proc_count, 
                                                 sizeof (ompi_proc_t *));
            for(i=0 ; i<local_comm->c_local_group->grp_proc_count ; i++)
                proc_list[i] = ompi_group_peer_lookup(local_comm->c_local_group,i);
            rc = ompi_proc_pack (proc_list, local_size, sbuf);
        }
        if ( OMPI_SUCCESS != rc ) {
            goto err_exit;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.unload(sbuf, &sendbuf, &size_len))) {
            goto err_exit;
        }
    
        /* send the remote_leader the length of the buffer */
        rc = MCA_PML_CALL(irecv (&rlen, 1, MPI_INT, remote_leader, tag,
                                 bridge_comm, &req ));
        if ( OMPI_SUCCESS != rc ) {
            goto err_exit;
        }
        int_len = (int)size_len;
        
        rc = MCA_PML_CALL(send (&int_len, 1, MPI_INT, remote_leader, tag, 
                                MCA_PML_BASE_SEND_STANDARD, bridge_comm ));
        if ( OMPI_SUCCESS != rc ) {
            goto err_exit;
        }
        rc = ompi_request_wait_all ( 1, &req, MPI_STATUS_IGNORE );
        if ( OMPI_SUCCESS != rc ) {
            goto err_exit;
        }
        
    }
    
    /* broadcast buffer length to all processes in local_comm */
    rc = local_comm->c_coll.coll_bcast( &rlen, 1, MPI_INT, 
                                        local_leader, local_comm,
                                        local_comm->c_coll.coll_bcast_module );
    if ( OMPI_SUCCESS != rc ) {
        goto err_exit;
    }
    
    /* Allocate temporary buffer */
    recvbuf = (char *)malloc(rlen);
    if ( NULL == recvbuf ) {
        goto err_exit;
    }

    if ( local_rank == local_leader ) {
        /* local leader exchange name lists */
        rc = MCA_PML_CALL(irecv (recvbuf, rlen, MPI_BYTE, remote_leader, tag,
                                bridge_comm, &req ));
        if ( OMPI_SUCCESS != rc ) {
            goto err_exit;
        }
        rc = MCA_PML_CALL(send(sendbuf, int_len, MPI_BYTE, remote_leader, tag, 
                               MCA_PML_BASE_SEND_STANDARD, bridge_comm ));
        if ( OMPI_SUCCESS != rc ) {
            goto err_exit;
        }
        rc = ompi_request_wait_all ( 1, &req, MPI_STATUS_IGNORE );
        if ( OMPI_SUCCESS != rc ) {
            goto err_exit;
        }

        OBJ_RELEASE(sbuf);
    }

    /* broadcast name list to all proceses in local_comm */
    rc = local_comm->c_coll.coll_bcast( recvbuf, rlen, MPI_BYTE, 
                                        local_leader, local_comm,
                                        local_comm->c_coll.coll_bcast_module);
    if ( OMPI_SUCCESS != rc ) {
        goto err_exit;
    }

    rbuf = OBJ_NEW(opal_buffer_t);
    if (NULL == rbuf) {
        rc = ORTE_ERROR;
        goto err_exit;
    }
    
    if (ORTE_SUCCESS != (rc = opal_dss.load(rbuf, recvbuf, rlen))) {
        goto err_exit;
    }
    
    /* decode the names into a proc-list -- will never add a new proc
       as the result of this operation, so no need to get the newprocs
       list or call PML add_procs(). */
    rc = ompi_proc_unpack(rbuf, rsize, &rprocs, NULL, NULL);
    OBJ_RELEASE(rbuf);
    
 err_exit:
    /* rprocs isn't freed unless we have an error, 
       since it is used in the communicator */
    if ( OMPI_SUCCESS !=rc ) {
        opal_output(0, "%d: Error in ompi_get_rprocs\n", local_rank);
        if ( NULL != rprocs ) {
            free ( rprocs );
            rprocs=NULL;
        }
    }
    /* make sure the buffers have been released */
    if (NULL != sbuf) {
        OBJ_RELEASE(sbuf);
    }
    if (NULL != rbuf) {
        OBJ_RELEASE(rbuf);
    }
    if ( NULL != proc_list ) {
        free ( proc_list );
    }
        
    return rprocs;
}
/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/**
 * This routine verifies, whether local_group and remote group are overlapping
 * in intercomm_create
 */
int ompi_comm_overlapping_groups (int size, ompi_proc_t **lprocs,
                  int rsize, ompi_proc_t ** rprocs)

{
    int rc=OMPI_SUCCESS;
    int i,j;

    for (i=0; i<size; i++) {
        for ( j=0; j<rsize; j++) {
            if ( lprocs[i] == rprocs[j] ) {
                rc = MPI_ERR_COMM;
                return rc;
            }
        }
    }

    return rc;
}
/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
int ompi_comm_determine_first ( ompi_communicator_t *intercomm, int high )
{
    int flag, rhigh;
    int rank, rsize;
    int *rcounts;
    int *rdisps;
    int scount=0;
    int rc;
    ompi_proc_t *ourproc, *theirproc;
    orte_ns_cmp_bitmask_t mask;

    rank = ompi_comm_rank        (intercomm);
    rsize= ompi_comm_remote_size (intercomm);
    
    rdisps  = (int *) calloc ( rsize, sizeof(int));
    rcounts = (int *) calloc ( rsize, sizeof(int));
    if ( NULL == rdisps || NULL == rcounts ){
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    rcounts[0] = 1;
    if ( 0 == rank ) {
        scount = 1;
    }
    
    rc = intercomm->c_coll.coll_allgatherv(&high, scount, MPI_INT,
                                           &rhigh, rcounts, rdisps,
                                           MPI_INT, intercomm,
                                           intercomm->c_coll.coll_allgatherv_module);
    if ( rc != OMPI_SUCCESS ) {
        flag = MPI_UNDEFINED;
    }

    if ( NULL != rdisps ) {
        free ( rdisps );
    }
    if ( NULL != rcounts ) {
        free ( rcounts );
    }

    /* This is the logic for determining who is first, who is second */
    if ( high && !rhigh ) {
        flag = false;
    }
    else if ( !high && rhigh ) {
        flag = true;
    }
    else {
        ourproc   = ompi_group_peer_lookup(intercomm->c_local_group,0);
        theirproc = ompi_group_peer_lookup(intercomm->c_remote_group,0);

        mask = ORTE_NS_CMP_JOBID | ORTE_NS_CMP_VPID;
        rc = orte_util_compare_name_fields(mask, &(ourproc->proc_name), &(theirproc->proc_name));
        if ( 0 > rc ) {
            flag = true;
        }
        else {
            flag = false;
        }
    }

    return flag;
}
/********************************************************************************/
/********************************************************************************/
/********************************************************************************/
int ompi_comm_dump ( ompi_communicator_t *comm )
{
    opal_output(0, "Dumping information for comm_cid %d\n", comm->c_contextid);
    opal_output(0,"  f2c index:%d cube_dim: %d\n", comm->c_f_to_c_index,  
                comm->c_cube_dim);
    opal_output(0,"  Local group: size = %d my_rank = %d\n", 
                comm->c_local_group->grp_proc_count, 
                comm->c_local_group->grp_my_rank );

    opal_output(0,"  Communicator is:");
    /* Display flags */
    if ( OMPI_COMM_IS_INTER(comm) )
        opal_output(0," inter-comm,");
    if ( OMPI_COMM_IS_CART(comm))
        opal_output(0," topo-cart,");
    if ( OMPI_COMM_IS_GRAPH(comm))
        opal_output(0," topo-graph");
    opal_output(0,"\n");

    if (OMPI_COMM_IS_INTER(comm)) {
        opal_output(0,"  Remote group size:%d\n", comm->c_remote_group->grp_proc_count);
    }
    return OMPI_SUCCESS;
}
/********************************************************************************/
/********************************************************************************/
/********************************************************************************/
/* static functions */
/* 
** rankkeygidcompare() compares a tuple of (rank,key,gid) producing 
** sorted lists that match the rules needed for a MPI_Comm_split 
*/
static int rankkeycompare (const void *p, const void *q)
{
    int *a, *b;
  
    /* ranks at [0] key at [1] */
    /* i.e. we cast and just compare the keys and then the original ranks.. */
    a = (int*)p;
    b = (int*)q;
    
    /* simple tests are those where the keys are different */
    if (a[1] < b[1]) {
        return (-1);
    }
    if (a[1] > b[1]) {
        return (1);
    }
    
    /* ok, if the keys are the same then we check the original ranks */
    if (a[1] == b[1]) {
        if (a[0] < b[0]) {
            return (-1);
        }
        if (a[0] == b[0]) {
            return (0);
        }
        if (a[0] > b[0]) {
            return (1);
        }
    }
    return ( 0 );
}


/*************************************************************************************
 * Counterpart of MPI_Cart/Graph_create. This will be called from the
 * top level MPI. The condition for INTER communicator is already
 * checked by the time this has been invoked. This function should do
 * somewhat the same things which ompi_comm_create does. It will
 * however select a component for topology and then call the
 * cart_create on that component so that it can re-arrange the proc
 * structure as required (if the reorder flag is true). It will then
 * use this proc structure to create the communicator using
 * ompi_comm_set.
 */
int ompi_topo_create (ompi_communicator_t *old_comm, 
                      int ndims_or_nnodes,
                      int *dims_or_index,
                      int *periods_or_edges,
                      bool reorder,
                      ompi_communicator_t **comm_topo,
                      int cart_or_graph){

    ompi_communicator_t *new_comm;
    int new_rank;
    ompi_proc_t **topo_procs;
    int num_procs;
    int ret;
    ompi_proc_t **proc_list=NULL;
    int i;

    /* allocate a new communicator */

    new_comm = ompi_comm_allocate(ompi_comm_size(old_comm), 0);
    if (NULL == new_comm) {
        return MPI_ERR_INTERN;
    }

    /* allocate the data for the common good */

    new_comm->c_topo_comm = (mca_topo_base_comm_t*)malloc(sizeof(mca_topo_base_comm_t));
    if (NULL == new_comm->c_topo_comm) {
        OBJ_RELEASE(new_comm);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* select the topology component on the communicator */

    if (OMPI_SUCCESS != (ret = mca_topo_base_comm_select (new_comm, NULL))) {
        /* OBJ_RELEASE also frees new_comm->c_topo_comm */
        OBJ_RELEASE(new_comm);
        return ret;
    }

    /* since the topo component has initialised, let us now initialise
     * the topo comm structure */
    new_comm->c_flags |= cart_or_graph;

    new_comm->c_topo_comm->mtc_ndims_or_nnodes = ndims_or_nnodes;
    new_comm->c_topo_comm->mtc_dims_or_index = NULL;
    new_comm->c_topo_comm->mtc_periods_or_edges = NULL;
    new_comm->c_topo_comm->mtc_reorder = reorder;
    new_comm->c_topo_comm->mtc_coords = NULL;

    /* MPI-2.1 allows 0-dimension cartesian communicators, so prevent
       a 0-byte malloc -- leave dims_or_index as NULL */
    if (!(OMPI_COMM_CART == cart_or_graph && 0 == ndims_or_nnodes)) {
        new_comm->c_topo_comm->mtc_dims_or_index = 
            (int *) malloc(sizeof(int) * ndims_or_nnodes);
        if (NULL == new_comm->c_topo_comm->mtc_dims_or_index) {
            ompi_comm_free (&new_comm);
            *comm_topo = new_comm;
            return OMPI_ERROR;
        }
        memcpy (new_comm->c_topo_comm->mtc_dims_or_index,
                dims_or_index, ndims_or_nnodes * sizeof(int));
    }

    /* Now the topology component has been selected, let the component
     * re-arrange the proc ranks if need be. This is a down-call into
     * the topo component and does not have anything to do with this
     * level */

    /* first, copy the proc structure from the previous communicator
     * over to the new one. the topology component can then work on
     * this and rearrange it as it deems fit.
     */
    num_procs = old_comm->c_local_group->grp_proc_count;
    topo_procs = (ompi_proc_t **)malloc (num_procs * sizeof(ompi_proc_t *));

    if(OMPI_GROUP_IS_DENSE(old_comm->c_local_group)) {
        memcpy (topo_procs, 
                old_comm->c_local_group->grp_proc_pointers,
                num_procs * sizeof(ompi_proc_t *));
    }
    else {
        proc_list = (ompi_proc_t **) calloc (old_comm->c_local_group->grp_proc_count, 
                                             sizeof (ompi_proc_t *));
        for(i=0 ; i<old_comm->c_local_group->grp_proc_count ; i++)
            proc_list[i] = ompi_group_peer_lookup(old_comm->c_local_group,i);
        
        memcpy (topo_procs, 
                proc_list,
                num_procs * sizeof(ompi_proc_t *));
        
    }
    if ( NULL != proc_list ) {
        free ( proc_list );
    }
    new_rank = old_comm->c_local_group->grp_my_rank;

    if (OMPI_COMM_CART == cart_or_graph) {

        /* A cartesian system has been requested. Call the right function */

        /* Note that we fill in the basic information, i.e, copy the
         * information which was provided to us over into the
         * structure. The base component functions are free to change
         * it as they deem fit */

        if (ndims_or_nnodes > 0) {
            new_comm->c_topo_comm->mtc_periods_or_edges =
                (int*) malloc(sizeof(int) * ndims_or_nnodes);
            if (NULL == new_comm->c_topo_comm->mtc_periods_or_edges) {
                ompi_comm_free (&new_comm);
                *comm_topo = new_comm;
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
            memcpy (new_comm->c_topo_comm->mtc_periods_or_edges,
                    periods_or_edges, ndims_or_nnodes * sizeof(int));

            new_comm->c_topo_comm->mtc_coords = 
                (int *) malloc(sizeof(int) * ndims_or_nnodes);
            if (NULL == new_comm->c_topo_comm->mtc_coords) {
                ompi_comm_free (&new_comm);
                *comm_topo = new_comm;
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        }

        if (OMPI_SUCCESS != 
            (ret = new_comm->c_topo->topo_cart_create (new_comm->c_topo_comm,
                                                       &num_procs,
                                                       topo_procs,
                                                       &new_rank,
                                                       ndims_or_nnodes,
                                                       dims_or_index,
                                                       periods_or_edges,
                                                       reorder))) {
            return ret;
        }

    } else if (OMPI_COMM_GRAPH == cart_or_graph) {

        /* A graph system has been requested. Call the right function */

        /* Note that we fill in the basic information, i.e, copy the
         * information which was provided to us over into the
         * structure. The base component functions are free to change
         * it as they deem fit */

        new_comm->c_topo_comm->mtc_periods_or_edges = (int *)
                malloc (sizeof(int) * dims_or_index[ndims_or_nnodes-1]);
        if (NULL == new_comm->c_topo_comm->mtc_periods_or_edges) {
            ompi_comm_free (&new_comm);
            *comm_topo = new_comm;
            return OMPI_ERROR;
        }
        memcpy (new_comm->c_topo_comm->mtc_periods_or_edges,
                periods_or_edges, dims_or_index[ndims_or_nnodes-1] * sizeof(int));

        if (OMPI_SUCCESS != 
            (ret = new_comm->c_topo->topo_graph_create (new_comm->c_topo_comm,
                                                        &num_procs,
                                                        topo_procs,
                                                        &new_rank,
                                                        ndims_or_nnodes,
                                                        dims_or_index,
                                                        periods_or_edges,
                                                        reorder))) {
            return ret;
        }

    }

    /* Determine context id. It is identical to f_2_c_handle */

    ret = ompi_comm_nextcid ( new_comm,  /* new communicator */
                              old_comm,     /* old comm */
                              NULL,     /* bridge comm */
                              NULL,     /* local leader */
                              NULL,     /* remote_leader */
                              OMPI_COMM_CID_INTRA,   /* mode */
                             -1 );     /* send first, doesn't matter */
    if (OMPI_SUCCESS != ret) {
        /* something wrong happened during setting the communicator */
        ompi_comm_free (&new_comm);
        *comm_topo = new_comm;
        return ret;
    }


    /* Now, the topology component has been selected and the group
     * which has the topology information has been created. All we
     * need to do now is to fill the rest of the information into the
     * communicator. The following steps are not just similar to
     * ompi_comm_set, but are actually the same */

    ret = ompi_comm_fill_rest(new_comm,                /* the communicator */
                              num_procs,               /* local size */
                              topo_procs,              /* process structure */
                              new_rank,                /* rank of the process */
                              old_comm->error_handler); /* error handler */

    if (OMPI_SUCCESS != ret) {
        /* something wrong happened during setting the communicator */
        ompi_comm_free (&new_comm);
        *comm_topo = new_comm;
        return ret;
    }

    ret = ompi_comm_activate( &new_comm,  /* new communicator */
                              old_comm,     /* old comm */
                              NULL,     /* bridge comm */
                              NULL,     /* local leader */
                              NULL,     /* remote_leader */
                              OMPI_COMM_CID_INTRA,   /* mode */
                             -1 );     /* send first, doesn't matter */


    if (OMPI_SUCCESS != ret) {
	/* something wrong happened during setting the communicator */
	*comm_topo = new_comm;
	return ret;
    }
    
    /* if the returned rank is -1, then this process is not in the 
     * new topology, so free everything we have allocated and return */
    if (MPI_UNDEFINED == new_rank) {
        ompi_comm_free (&new_comm);
        *comm_topo = new_comm;
    } else {
        *comm_topo = new_comm;
    }

    return OMPI_SUCCESS;
}

static int ompi_comm_fill_rest (ompi_communicator_t *comm,
                                int num_procs,
                                ompi_proc_t **proc_pointers,
                                int my_rank,
                                ompi_errhandler_t *errh )
{
    /* properly decrement the ref counts on the groups.
       We are doing this because this function is sort of a redo 
       of what is done in comm.c.. No need to decrement the ref 
       count on the proc pointers 
       This is just a quick fix, and will be looking for a 
       better solution */
    OBJ_RELEASE ( comm->c_local_group );
    OBJ_RELEASE ( comm->c_local_group );

    /* allocate a group structure for the new communicator */
    comm->c_local_group = ompi_group_allocate(num_procs);
    
    /* free the malloced  proc pointers */
    free(comm->c_local_group->grp_proc_pointers);
    
    /* set the group information */
    comm->c_local_group->grp_proc_pointers = proc_pointers;

    /* set the remote group to be the same as local group */
    comm->c_remote_group = comm->c_local_group;
    OBJ_RETAIN ( comm->c_remote_group );

    /* retain these proc pointers */
    ompi_group_increment_proc_count(comm->c_local_group);
        
    /* set the rank information */
    comm->c_local_group->grp_my_rank = my_rank;
    comm->c_my_rank = my_rank;

    /* verify whether to set the flag, that this comm
       contains process from more than one jobid. */
    ompi_dpm.mark_dyncomm (comm);

    /* set the error handler */
    comm->error_handler = errh;
    OBJ_RETAIN (comm->error_handler);

    /* set name for debugging purposes */
    /* there is no cid at this stage ... make this right and make edgars
     * code call this function and remove dupli cde
     */  
    snprintf (comm->c_name, MPI_MAX_OBJECT_NAME, "MPI_COMMUNICATOR %d",
              comm->c_contextid);

    /* determine the cube dimensions */
    comm->c_cube_dim = opal_cube_dim(comm->c_local_group->grp_proc_count);

   return OMPI_SUCCESS;
}

static int ompi_comm_copy_topo (ompi_communicator_t *oldcomm, 
                                 ompi_communicator_t *newcomm) 
{
    mca_topo_base_comm_t *oldt = oldcomm->c_topo_comm;
    mca_topo_base_comm_t *newt = newcomm->c_topo_comm;

    /* pointers for the rest of the information have been set
       up.... simply allocate enough space and copy all the
       information from the previous one.  Remember that MPI-2.1
       allows for 0-dimensional Cartesian communicators. */

    newt->mtc_ndims_or_nnodes = oldt->mtc_ndims_or_nnodes;
    newt->mtc_reorder = oldt->mtc_reorder;
    if (OMPI_COMM_IS_CART(oldcomm) && 0 == oldt->mtc_ndims_or_nnodes) {
        newt->mtc_dims_or_index = NULL;
        newt->mtc_periods_or_edges = NULL;
        newt->mtc_coords = NULL;
        return MPI_SUCCESS;
    }

    newt->mtc_dims_or_index = 
        (int *)malloc(sizeof(int) * newt->mtc_ndims_or_nnodes);
    if (NULL == newt->mtc_dims_or_index) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    memcpy (newt->mtc_dims_or_index, oldt->mtc_dims_or_index,
            newt->mtc_ndims_or_nnodes * sizeof(int));

    /* Note that the length of the mtc_periods_or_edges array is
       different depending whether it's a cartesian or graph */
    if (OMPI_COMM_IS_CART(oldcomm)) {
        newt->mtc_periods_or_edges =
            (int*) malloc (sizeof(int) * oldt->mtc_ndims_or_nnodes);
        if (NULL == newt->mtc_periods_or_edges) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        
        newt->mtc_coords =
            (int *)malloc(sizeof(int)*
                          newcomm->c_topo_comm->mtc_ndims_or_nnodes);   
        if (NULL == newt->mtc_coords) {
            return OMPI_ERROR;
        }
        memcpy(newt->mtc_periods_or_edges, oldt->mtc_periods_or_edges, 
               sizeof(int) * oldt->mtc_ndims_or_nnodes);

        memcpy (newt->mtc_coords, oldt->mtc_coords,
                sizeof(int) * newt->mtc_ndims_or_nnodes);
    } else {
        int index = oldt->mtc_dims_or_index[oldt->mtc_ndims_or_nnodes-1];
        index = (index > 0) ? index : -index;
        newt->mtc_periods_or_edges =
            (int*) malloc (sizeof(int)*index);
        if (NULL == newt->mtc_periods_or_edges) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        
        memcpy(newt->mtc_periods_or_edges, oldt->mtc_periods_or_edges, 
               sizeof(int) * index );

        newt->mtc_coords = NULL;
    }

    return OMPI_SUCCESS;
}

