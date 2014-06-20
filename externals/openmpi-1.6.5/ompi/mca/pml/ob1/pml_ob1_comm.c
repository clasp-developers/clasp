/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include <string.h>

#include "pml_ob1.h"
#include "pml_ob1_comm.h"



static void mca_pml_ob1_comm_proc_construct(mca_pml_ob1_comm_proc_t* proc)
{
    proc->expected_sequence = 1;
    proc->ompi_proc = NULL;
    proc->send_sequence = 0;
    OBJ_CONSTRUCT(&proc->frags_cant_match, opal_list_t);
    OBJ_CONSTRUCT(&proc->specific_receives, opal_list_t);
    OBJ_CONSTRUCT(&proc->unexpected_frags, opal_list_t);
}


static void mca_pml_ob1_comm_proc_destruct(mca_pml_ob1_comm_proc_t* proc)
{
    OBJ_DESTRUCT(&proc->frags_cant_match);
    OBJ_DESTRUCT(&proc->specific_receives);
    OBJ_DESTRUCT(&proc->unexpected_frags);
}


static OBJ_CLASS_INSTANCE(
    mca_pml_ob1_comm_proc_t,
    opal_object_t,
    mca_pml_ob1_comm_proc_construct,
    mca_pml_ob1_comm_proc_destruct);


static void mca_pml_ob1_comm_construct(mca_pml_ob1_comm_t* comm)
{
    OBJ_CONSTRUCT(&comm->wild_receives, opal_list_t);
    OBJ_CONSTRUCT(&comm->matching_lock, opal_mutex_t);
    comm->recv_sequence = 0;
    comm->procs = NULL;
    comm->num_procs = 0;
}


static void mca_pml_ob1_comm_destruct(mca_pml_ob1_comm_t* comm)
{
    size_t i;
    for(i=0; i<comm->num_procs; i++)
        OBJ_DESTRUCT((&comm->procs[i]));
    if(NULL != comm->procs)
        free(comm->procs);
    OBJ_DESTRUCT(&comm->wild_receives);
    OBJ_DESTRUCT(&comm->matching_lock);
}


OBJ_CLASS_INSTANCE(
    mca_pml_ob1_comm_t,
    opal_object_t,
    mca_pml_ob1_comm_construct,
    mca_pml_ob1_comm_destruct);


int mca_pml_ob1_comm_init_size(mca_pml_ob1_comm_t* comm, size_t size)
{
    size_t i;

    /* send message sequence-number support - sender side */
    comm->procs = (mca_pml_ob1_comm_proc_t*)malloc(sizeof(mca_pml_ob1_comm_proc_t)*size);
    if(NULL == comm->procs) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    for(i=0; i<size; i++) {
        OBJ_CONSTRUCT(comm->procs+i, mca_pml_ob1_comm_proc_t);
    }
    comm->num_procs = size;
    return OMPI_SUCCESS;
}


