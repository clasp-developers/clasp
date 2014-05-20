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
 * Copyright (c) 2006-2007 University of Houston. All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/group/group.h"
#include "ompi/constants.h"
#include "ompi/proc/proc.h"
#include "mpi.h"

#include <math.h>

int ompi_group_calc_plist ( int n , int *ranks ) { 
    return sizeof(char *) * n ;
}

int ompi_group_incl_plist(ompi_group_t* group, int n, int *ranks, 
			  ompi_group_t **new_group) 
{
    /* local variables */
    int proc,my_group_rank;
    ompi_group_t *group_pointer, *new_group_pointer;
    ompi_proc_t *my_proc_pointer;
    
    group_pointer = (ompi_group_t *)group;

    if ( 0 == n ) {
	*new_group = MPI_GROUP_EMPTY;
	OBJ_RETAIN(MPI_GROUP_EMPTY);
	return OMPI_SUCCESS;
    }

    /* get new group struct */
    new_group_pointer=ompi_group_allocate(n);
    if( NULL == new_group_pointer ) {
      return MPI_ERR_GROUP;
    }

    /* put group elements in the list */
    for (proc = 0; proc < n; proc++) {
        new_group_pointer->grp_proc_pointers[proc] = 
	  ompi_group_peer_lookup(group_pointer,ranks[proc]); 
	
    }                           /* end proc loop */

    /* increment proc reference counters */
    ompi_group_increment_proc_count(new_group_pointer);

    /* find my rank */
    my_group_rank=group_pointer->grp_my_rank;
    if (MPI_UNDEFINED != my_group_rank) {
        my_proc_pointer=ompi_group_peer_lookup (group_pointer,my_group_rank);
        ompi_set_group_rank(new_group_pointer,my_proc_pointer);
    }
    else {
        new_group_pointer->grp_my_rank = MPI_UNDEFINED;
    }

    *new_group = (MPI_Group)new_group_pointer;

    return OMPI_SUCCESS;
}

/* 
 * Group Union has to use the dense format since we don't support 
 * two parent groups in the group structure and maintain functions
 */
int ompi_group_union (ompi_group_t* group1, ompi_group_t* group2, 
		      ompi_group_t **new_group) 
{
    /* local variables */
    int new_group_size, proc1, proc2, found_in_group;
    int my_group_rank, cnt;
    ompi_group_t *group1_pointer, *group2_pointer, *new_group_pointer;
    ompi_proc_t *proc1_pointer, *proc2_pointer, *my_proc_pointer = NULL;

    group1_pointer = (ompi_group_t *) group1;
    group2_pointer = (ompi_group_t *) group2;

    /*
     * form union
     */

    /* get new group size */
    new_group_size = group1_pointer->grp_proc_count;

    /* check group2 elements to see if they need to be included in the list */
    for (proc2 = 0; proc2 < group2_pointer->grp_proc_count; proc2++) {
        proc2_pointer = ompi_group_peer_lookup(group2_pointer,proc2); 

        /* check to see if this proc2 is alread in the group */
        found_in_group = 0;
        for (proc1 = 0; proc1 < group1_pointer->grp_proc_count; proc1++) {
            proc1_pointer = ompi_group_peer_lookup(group1_pointer,proc1);

            if (proc1_pointer == proc2_pointer) {
                /* proc2 is in group1 - don't double count */
                found_in_group = 1;
                break;
            }
        }                       /* end proc1 loop */

        if (found_in_group)
            continue;

        new_group_size++;
    }                           /* end proc loop */

    if ( 0 == new_group_size ) {
	*new_group = MPI_GROUP_EMPTY;
	OBJ_RETAIN(MPI_GROUP_EMPTY);
	return MPI_SUCCESS;
    }

    /* get new group struct */
    new_group_pointer = ompi_group_allocate(new_group_size);
    if (NULL == new_group_pointer) {
        return MPI_ERR_GROUP;
    }

    /* fill in the new group list */

    /* put group1 elements in the list */
    for (proc1 = 0; proc1 < group1_pointer->grp_proc_count; proc1++) {
        new_group_pointer->grp_proc_pointers[proc1] = 
	  ompi_group_peer_lookup(group1_pointer,proc1);    
    }
    cnt = group1_pointer->grp_proc_count;

    /* check group2 elements to see if they need to be included in the list */
    for (proc2 = 0; proc2 < group2_pointer->grp_proc_count; proc2++) {
        proc2_pointer = ompi_group_peer_lookup(group2_pointer,proc2);

        /* check to see if this proc2 is alread in the group */
        found_in_group = 0;
        for (proc1 = 0; proc1 < group1_pointer->grp_proc_count; proc1++) {
            proc1_pointer = ompi_group_peer_lookup(group1_pointer,proc1);

            if (proc1_pointer == proc2_pointer) {
                /* proc2 is in group1 - don't double count */
                found_in_group = 1;
                break;
            }
        }                       /* end proc1 loop */

        if (found_in_group)
            continue;

        new_group_pointer->grp_proc_pointers[cnt] =
	  ompi_group_peer_lookup(group2_pointer,proc2);
        cnt++;
    }                           /* end proc loop */

    /* increment proc reference counters */
    ompi_group_increment_proc_count(new_group_pointer);

    /* find my rank */
    my_group_rank = group1_pointer->grp_my_rank;
    if (MPI_UNDEFINED == my_group_rank) {
        my_group_rank = group2_pointer->grp_my_rank;
	if ( MPI_UNDEFINED != my_group_rank) {
	    my_proc_pointer = ompi_group_peer_lookup(group2_pointer,my_group_rank);
	}
    } else {
        my_proc_pointer = ompi_group_peer_lookup(group1_pointer,my_group_rank);
    }

    if ( MPI_UNDEFINED == my_group_rank ) {
	new_group_pointer->grp_my_rank = MPI_UNDEFINED;
    }
    else {
	ompi_set_group_rank(new_group_pointer, my_proc_pointer);
    }

    *new_group = (MPI_Group) new_group_pointer;


    return OMPI_SUCCESS;
}

/* 
 * Group Difference has to use the dense format since we don't support 
 * two parent groups in the group structure and maintain functions
 */
int ompi_group_difference(ompi_group_t* group1, ompi_group_t* group2,
                         ompi_group_t **new_group) {

    /* local varibles */
    int new_group_size, proc1, proc2, found_in_group2, cnt;
    int my_group_rank;
    ompi_group_t *group1_pointer, *group2_pointer, *new_group_pointer;
    ompi_proc_t *proc1_pointer, *proc2_pointer, *my_proc_pointer = NULL;

   
    group1_pointer=(ompi_group_t *)group1;
    group2_pointer=(ompi_group_t *)group2;

    /*
     * form union
     */

    /* get new group size */
    new_group_size=0;

    /* loop over group1 members */
    for( proc1=0; proc1 < group1_pointer->grp_proc_count; proc1++ ) {
        proc1_pointer = ompi_group_peer_lookup(group1_pointer,proc1);
        /* check to see if this proc is in group2 */
        found_in_group2=0;
        for( proc2=0 ; proc2 < group2_pointer->grp_proc_count ; proc2++ ) {
            proc2_pointer = ompi_group_peer_lookup(group2_pointer,proc2);
            if( proc1_pointer == proc2_pointer ) {
                found_in_group2=true;
                break;
            }
        }  /* end proc1 loop */
        if(found_in_group2)
            continue;
        new_group_size++;
    }  /* end proc loop */

    if ( 0 == new_group_size ) {
	*new_group = MPI_GROUP_EMPTY;
	OBJ_RETAIN(MPI_GROUP_EMPTY);
	return MPI_SUCCESS;
    }

    /* allocate a new ompi_group_t structure */
    new_group_pointer=ompi_group_allocate(new_group_size);
    if( NULL == new_group_pointer ) {
      return MPI_ERR_GROUP;
    }

    /* fill in group list */
    cnt=0;
    /* loop over group1 members */
    for( proc1=0; proc1 < group1_pointer->grp_proc_count; proc1++ ) {
        proc1_pointer = ompi_group_peer_lookup(group1_pointer,proc1);
        /* check to see if this proc is in group2 */
        found_in_group2=0;
        for( proc2=0 ; proc2 < group2_pointer->grp_proc_count ; proc2++ ) {
            proc2_pointer = ompi_group_peer_lookup(group2_pointer,proc2);
            if( proc1_pointer == proc2_pointer ) {
                found_in_group2=true;
                break;
            }
        }  /* end proc1 loop */
        if(found_in_group2)
            continue;

        new_group_pointer->grp_proc_pointers[cnt] =
	  ompi_group_peer_lookup(group1_pointer,proc1);

        cnt++;
    }  /* end proc loop */

    /* increment proc reference counters */
    ompi_group_increment_proc_count(new_group_pointer);

    /* find my rank */
    my_group_rank=group1_pointer->grp_my_rank;
    if ( MPI_UNDEFINED != my_group_rank ) {
	my_proc_pointer = ompi_group_peer_lookup(group1_pointer,my_group_rank);
    }
    else {
	my_group_rank=group2_pointer->grp_my_rank;
	if ( MPI_UNDEFINED != my_group_rank ) {
	  my_proc_pointer = ompi_group_peer_lookup(group2_pointer,my_group_rank);
	}
    }

    if ( MPI_UNDEFINED == my_group_rank ) {
	new_group_pointer->grp_my_rank = MPI_UNDEFINED;
    }
    else {
	ompi_set_group_rank(new_group_pointer,my_proc_pointer);
    }

    *new_group = (MPI_Group)new_group_pointer;

    return OMPI_SUCCESS;
}

