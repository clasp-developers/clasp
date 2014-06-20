/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/group/group.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/communicator/communicator.h"
#include "ompi/proc/proc.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Group_compare = PMPI_Group_compare
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Group_compare";


int MPI_Group_compare(MPI_Group group1, MPI_Group group2, int *result) {

    /* local variables */
    int return_value, proc1, proc2, match;
    bool similar, identical;
    ompi_group_t *group1_pointer, *group2_pointer;
    ompi_proc_t *proc1_pointer, *proc2_pointer;

    OPAL_CR_NOOP_PROGRESS();

    /* initialization */
    return_value=MPI_SUCCESS;

    /* check for errors */
    if( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if( ( MPI_GROUP_NULL == group1 ) || ( MPI_GROUP_NULL == group2 ) ||
                (NULL == group1) || (NULL==group2) ){
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_GROUP,
                                          FUNC_NAME);
        } else if (NULL == result) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                          FUNC_NAME);
        }
    }

    /* check for same groups */
    if( group1 == group2 ) {
        *result=MPI_IDENT;
        return return_value;
    }

    /* check to see if either is MPI_GROUP_NULL or MPI_GROUP_EMPTY */
    if( ( MPI_GROUP_EMPTY == group1 ) || ( MPI_GROUP_EMPTY == group2 ) ) {
        *result=MPI_UNEQUAL;
        return return_value;
    }

    /* get group pointers */
    group1_pointer = (ompi_group_t *)group1;
    group2_pointer = (ompi_group_t *)group2;

    /* compare sizes */
    if( group1_pointer->grp_proc_count != group2_pointer->grp_proc_count ) {
        /* if not same size - return */
        *result=MPI_UNEQUAL;
        return return_value;
    }

    /* check for similarity */
    /* loop over group1 processes */
    similar=true;
    identical=true;
    for(proc1=0 ; proc1 < group1_pointer->grp_proc_count ; proc1++ ) {
        proc1_pointer= ompi_group_peer_lookup(group1_pointer,proc1);
        /* loop over group2 processes to find "match" */
        match=-1;
        for(proc2=0 ; proc2 < group2_pointer->grp_proc_count ; proc2++ ) {
            proc2_pointer=ompi_group_peer_lookup(group2_pointer,proc2);
            if( proc1_pointer == proc2_pointer ) {
                if(proc1 != proc2 ) {
                    identical=false;
                }
                match=proc2;
                break;
            }
        } /* end proc2 loop */
        if( match== -1 ) {
            similar=false;
            identical=false;
            break;
        }
    } /* end proc1 loop */

    /* set comparison result */
    if( identical ) {
        *result=MPI_IDENT;
    } else if( similar ) {
        *result=MPI_SIMILAR;
    } else {
        *result=MPI_UNEQUAL;
    }

    return return_value;
}
