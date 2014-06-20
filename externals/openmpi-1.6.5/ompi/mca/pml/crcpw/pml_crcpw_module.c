/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdlib.h>
#include <string.h>

#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/base/base.h"
#include "ompi/mca/pml/crcpw/pml_crcpw.h"
#include "ompi/mca/bml/base/base.h"

#include "ompi/class/ompi_free_list.h"

mca_pml_crcpw_module_t mca_pml_crcpw_module = {
    {
        mca_pml_crcpw_add_procs,
        mca_pml_crcpw_del_procs,
        mca_pml_crcpw_enable,
        mca_pml_crcpw_progress,
        mca_pml_crcpw_add_comm,
        mca_pml_crcpw_del_comm,
        mca_pml_crcpw_irecv_init,
        mca_pml_crcpw_irecv,
        mca_pml_crcpw_recv,
        mca_pml_crcpw_isend_init,
        mca_pml_crcpw_isend,
        mca_pml_crcpw_send,
        mca_pml_crcpw_iprobe,
        mca_pml_crcpw_probe,
        mca_pml_crcpw_start,
        mca_pml_crcpw_dump,
        mca_pml_crcpw_ft_event,

        32768,
        INT_MAX
    }
};

#define PML_CRCP_STATE_ALLOC(pml_state, rc)         \
do {                                                \
  if( !pml_crcpw_is_finalized ) {                   \
    ompi_free_list_item_t* item;                    \
    OMPI_FREE_LIST_WAIT(&pml_state_list, item, rc); \
    pml_state = (ompi_crcp_base_pml_state_t*)item;  \
  }                                                 \
} while(0); 

#define PML_CRCP_STATE_RETURN(pml_state)    \
do {                                        \
  if( !pml_crcpw_is_finalized ) {           \
    OMPI_FREE_LIST_RETURN(&pml_state_list,  \
    (ompi_free_list_item_t*)pml_state);     \
  }                                         \
} while(0);

int mca_pml_crcpw_enable(bool enable)
{
    int ret;
    ompi_crcp_base_pml_state_t * pml_state = NULL;

    if( OPAL_UNLIKELY(NULL == ompi_crcp.pml_enable) ) {
        return mca_pml_crcpw_module.wrapped_pml_module.pml_enable(enable);
    }

    PML_CRCP_STATE_ALLOC(pml_state, ret);

    pml_state->wrapped_pml_component = &(mca_pml_crcpw_module.wrapped_pml_component);
    pml_state->wrapped_pml_module    = &(mca_pml_crcpw_module.wrapped_pml_module);

    pml_state->state = OMPI_CRCP_PML_PRE;
    pml_state = ompi_crcp.pml_enable(enable, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    if( OMPI_CRCP_PML_SKIP != pml_state->state) {
        if( OMPI_SUCCESS != (ret = mca_pml_crcpw_module.wrapped_pml_module.pml_enable(enable) ) ) {
            PML_CRCP_STATE_RETURN(pml_state);
            return ret;
        }
    }
    
    pml_state->state = OMPI_CRCP_PML_POST;
    pml_state = ompi_crcp.pml_enable(enable, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    PML_CRCP_STATE_RETURN(pml_state);

    return OMPI_SUCCESS;
}

int mca_pml_crcpw_add_comm(ompi_communicator_t* comm)
{
    int ret;
    ompi_crcp_base_pml_state_t * pml_state = NULL;

    if( OPAL_UNLIKELY(NULL == ompi_crcp.pml_add_comm) ) {
        return mca_pml_crcpw_module.wrapped_pml_module.pml_add_comm(comm);
    }

    PML_CRCP_STATE_ALLOC(pml_state, ret);

    pml_state->wrapped_pml_component = &(mca_pml_crcpw_module.wrapped_pml_component);
    pml_state->wrapped_pml_module    = &(mca_pml_crcpw_module.wrapped_pml_module);

    pml_state->state = OMPI_CRCP_PML_PRE;
    pml_state = ompi_crcp.pml_add_comm(comm, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    if( OMPI_CRCP_PML_SKIP != pml_state->state) {
        if( OMPI_SUCCESS != (ret = mca_pml_crcpw_module.wrapped_pml_module.pml_add_comm(comm) ) ) {
            PML_CRCP_STATE_RETURN(pml_state);
            return ret;
        }
    }

    pml_state->state = OMPI_CRCP_PML_POST;
    pml_state = ompi_crcp.pml_add_comm(comm, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    PML_CRCP_STATE_RETURN(pml_state);

    return OMPI_SUCCESS;
}

int mca_pml_crcpw_del_comm(ompi_communicator_t* comm)
{
    int ret;
    ompi_crcp_base_pml_state_t * pml_state = NULL;

    if( OPAL_UNLIKELY(NULL == ompi_crcp.pml_del_comm) ) {
        return mca_pml_crcpw_module.wrapped_pml_module.pml_del_comm(comm);
    }

    PML_CRCP_STATE_ALLOC(pml_state, ret);
    if( NULL == pml_state ) {
        return mca_pml_crcpw_module.wrapped_pml_module.pml_del_comm(comm);
    }
        
    pml_state->wrapped_pml_component = &(mca_pml_crcpw_module.wrapped_pml_component);
    pml_state->wrapped_pml_module    = &(mca_pml_crcpw_module.wrapped_pml_module);

    pml_state->state = OMPI_CRCP_PML_PRE;
    pml_state = ompi_crcp.pml_del_comm(comm, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    if( OMPI_CRCP_PML_SKIP != pml_state->state) {
        if( OMPI_SUCCESS != (ret = mca_pml_crcpw_module.wrapped_pml_module.pml_del_comm(comm) ) ) {
            PML_CRCP_STATE_RETURN(pml_state);
            return ret;
        }
    }

    pml_state->state = OMPI_CRCP_PML_POST;
    pml_state = ompi_crcp.pml_del_comm(comm, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    PML_CRCP_STATE_RETURN(pml_state);

    return OMPI_SUCCESS;
}

int mca_pml_crcpw_add_procs(ompi_proc_t** procs, size_t nprocs)
{
    int ret;
    ompi_crcp_base_pml_state_t * pml_state = NULL;

    PML_CRCP_STATE_ALLOC(pml_state, ret);

    pml_state->wrapped_pml_component = &(mca_pml_crcpw_module.wrapped_pml_component);
    pml_state->wrapped_pml_module    = &(mca_pml_crcpw_module.wrapped_pml_module);

    pml_state->state = OMPI_CRCP_PML_PRE;
    pml_state = ompi_crcp.pml_add_procs(procs, nprocs, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    if( OMPI_CRCP_PML_SKIP != pml_state->state) {
        if( OMPI_SUCCESS != (ret = mca_pml_crcpw_module.wrapped_pml_module.pml_add_procs(procs, nprocs) ) ) {
            PML_CRCP_STATE_RETURN(pml_state);
            return ret;
        }
    }

    pml_state->state = OMPI_CRCP_PML_POST;
    pml_state = ompi_crcp.pml_add_procs(procs, nprocs, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    PML_CRCP_STATE_RETURN(pml_state);

    return OMPI_SUCCESS;
}

int mca_pml_crcpw_del_procs(ompi_proc_t** procs, size_t nprocs)
{
    int ret;
    ompi_crcp_base_pml_state_t * pml_state = NULL;

    PML_CRCP_STATE_ALLOC(pml_state, ret);

    pml_state->wrapped_pml_component = &(mca_pml_crcpw_module.wrapped_pml_component);
    pml_state->wrapped_pml_module    = &(mca_pml_crcpw_module.wrapped_pml_module);

    pml_state->state = OMPI_CRCP_PML_PRE;
    pml_state = ompi_crcp.pml_del_procs(procs, nprocs, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    if( OMPI_CRCP_PML_SKIP != pml_state->state) {
        if( OMPI_SUCCESS != (ret = mca_pml_crcpw_module.wrapped_pml_module.pml_del_procs(procs, nprocs) ) ) {
            PML_CRCP_STATE_RETURN(pml_state);
            return ret;
        }
    }

    pml_state->state = OMPI_CRCP_PML_POST;
    pml_state = ompi_crcp.pml_del_procs(procs, nprocs, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    PML_CRCP_STATE_RETURN(pml_state);

    return OMPI_SUCCESS;
}

int mca_pml_crcpw_iprobe(int dst, int tag, struct ompi_communicator_t* comm, int *matched, ompi_status_public_t* status )
{
    int ret;
    ompi_crcp_base_pml_state_t * pml_state = NULL;

    PML_CRCP_STATE_ALLOC(pml_state, ret);

    pml_state->wrapped_pml_component = &(mca_pml_crcpw_module.wrapped_pml_component);
    pml_state->wrapped_pml_module    = &(mca_pml_crcpw_module.wrapped_pml_module);

    pml_state->state = OMPI_CRCP_PML_PRE;
    pml_state = ompi_crcp.pml_iprobe(dst, tag, comm, matched, status, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    if( OMPI_CRCP_PML_DONE == pml_state->state) {
        goto CLEANUP;
    }

    if( OMPI_CRCP_PML_SKIP != pml_state->state) {
        if( OMPI_SUCCESS != (ret = mca_pml_crcpw_module.wrapped_pml_module.pml_iprobe(dst, tag, comm, matched, status) ) ) {
            PML_CRCP_STATE_RETURN(pml_state);
            return ret;
        }
    }

    pml_state->state = OMPI_CRCP_PML_POST;
    pml_state = ompi_crcp.pml_iprobe(dst, tag, comm, matched, status, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

 CLEANUP:
    PML_CRCP_STATE_RETURN(pml_state);

    return OMPI_SUCCESS;
}

int mca_pml_crcpw_probe( int dst, int tag, struct ompi_communicator_t* comm, ompi_status_public_t* status )
{
    int ret;
    ompi_crcp_base_pml_state_t * pml_state = NULL;

    PML_CRCP_STATE_ALLOC(pml_state, ret);

    pml_state->wrapped_pml_component = &(mca_pml_crcpw_module.wrapped_pml_component);
    pml_state->wrapped_pml_module    = &(mca_pml_crcpw_module.wrapped_pml_module);

    pml_state->state = OMPI_CRCP_PML_PRE;
    pml_state = ompi_crcp.pml_probe(dst, tag, comm, status, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    if( OMPI_CRCP_PML_DONE == pml_state->state) {
        goto CLEANUP;
    }

    if( OMPI_CRCP_PML_SKIP != pml_state->state) {
        if( OMPI_SUCCESS != (ret = mca_pml_crcpw_module.wrapped_pml_module.pml_probe(dst, tag, comm, status) ) ) {
            PML_CRCP_STATE_RETURN(pml_state);
            return ret;
        }
    }

    pml_state->state = OMPI_CRCP_PML_POST;
    pml_state = ompi_crcp.pml_probe(dst, tag, comm, status, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

 CLEANUP:
    PML_CRCP_STATE_RETURN(pml_state);

    return OMPI_SUCCESS;
}

int mca_pml_crcpw_isend_init( void *buf, size_t count, ompi_datatype_t *datatype, int dst, int tag, 
                              mca_pml_base_send_mode_t mode, struct ompi_communicator_t* comm, struct ompi_request_t **request )
{
    int ret;
    ompi_crcp_base_pml_state_t * pml_state = NULL;

    PML_CRCP_STATE_ALLOC(pml_state, ret);

    pml_state->wrapped_pml_component = &(mca_pml_crcpw_module.wrapped_pml_component);
    pml_state->wrapped_pml_module    = &(mca_pml_crcpw_module.wrapped_pml_module);

    pml_state->state = OMPI_CRCP_PML_PRE;
    pml_state = ompi_crcp.pml_isend_init(buf, count, datatype, dst, tag, mode, comm, request, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    if( OMPI_CRCP_PML_SKIP != pml_state->state) {
        if( OMPI_SUCCESS != (ret = mca_pml_crcpw_module.wrapped_pml_module.pml_isend_init(buf, count, datatype, dst, tag, mode, comm, request) ) ) {
            PML_CRCP_STATE_RETURN(pml_state);
            return ret;
        }
    }

    pml_state->state = OMPI_CRCP_PML_POST;
    pml_state = ompi_crcp.pml_isend_init(buf, count, datatype, dst, tag, mode, comm, request, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    PML_CRCP_STATE_RETURN(pml_state);

    return OMPI_SUCCESS;
}

int mca_pml_crcpw_isend( void *buf, size_t count, ompi_datatype_t *datatype, int dst, int tag,
                         mca_pml_base_send_mode_t mode, struct ompi_communicator_t* comm, struct ompi_request_t **request )
{
    int ret;
    ompi_crcp_base_pml_state_t * pml_state = NULL;

    PML_CRCP_STATE_ALLOC(pml_state, ret);

    pml_state->wrapped_pml_component = &(mca_pml_crcpw_module.wrapped_pml_component);
    pml_state->wrapped_pml_module    = &(mca_pml_crcpw_module.wrapped_pml_module);

    pml_state->state = OMPI_CRCP_PML_PRE;
    pml_state = ompi_crcp.pml_isend(buf, count, datatype, dst, tag, mode, comm, request, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    if( OMPI_CRCP_PML_SKIP != pml_state->state) {
        if( OMPI_SUCCESS != (ret = mca_pml_crcpw_module.wrapped_pml_module.pml_isend(buf, count, datatype, dst, tag, mode, comm, request) ) ) {
            PML_CRCP_STATE_RETURN(pml_state);
            return ret;
        }
    }

    pml_state->state = OMPI_CRCP_PML_POST;
    pml_state = ompi_crcp.pml_isend(buf, count, datatype, dst, tag, mode, comm, request, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    PML_CRCP_STATE_RETURN(pml_state);

    opal_cr_stall_check = false;
    OPAL_CR_TEST_CHECKPOINT_READY();

    return OMPI_SUCCESS;
}

int mca_pml_crcpw_send(  void *buf, size_t count, ompi_datatype_t *datatype, int dst, int tag,
                         mca_pml_base_send_mode_t mode, struct ompi_communicator_t* comm )
{
    int ret;
    ompi_crcp_base_pml_state_t * pml_state = NULL;

    PML_CRCP_STATE_ALLOC(pml_state, ret);

    pml_state->wrapped_pml_component = &(mca_pml_crcpw_module.wrapped_pml_component);
    pml_state->wrapped_pml_module    = &(mca_pml_crcpw_module.wrapped_pml_module);

    pml_state->state = OMPI_CRCP_PML_PRE;
    pml_state = ompi_crcp.pml_send(buf, count, datatype, dst, tag, mode, comm, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    if( OMPI_CRCP_PML_SKIP != pml_state->state) {
        if( OMPI_SUCCESS != (ret = mca_pml_crcpw_module.wrapped_pml_module.pml_send(buf, count, datatype, dst, tag, mode, comm) ) ) {
            PML_CRCP_STATE_RETURN(pml_state);
            return ret;
        }
    }

    pml_state->state = OMPI_CRCP_PML_POST;
    pml_state = ompi_crcp.pml_send(buf, count, datatype, dst, tag, mode, comm, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    PML_CRCP_STATE_RETURN(pml_state);

    opal_cr_stall_check = false;
    OPAL_CR_TEST_CHECKPOINT_READY();

    return OMPI_SUCCESS;
}

int mca_pml_crcpw_irecv_init( void *buf, size_t count, ompi_datatype_t *datatype, int src, int tag,
                              struct ompi_communicator_t* comm,  struct ompi_request_t **request)
{
    int ret;
    ompi_crcp_base_pml_state_t * pml_state = NULL;

    PML_CRCP_STATE_ALLOC(pml_state, ret);

    pml_state->wrapped_pml_component = &(mca_pml_crcpw_module.wrapped_pml_component);
    pml_state->wrapped_pml_module    = &(mca_pml_crcpw_module.wrapped_pml_module);

    pml_state->state = OMPI_CRCP_PML_PRE;
    pml_state = ompi_crcp.pml_irecv_init(buf, count, datatype, src, tag, comm, request,  pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    if( OMPI_CRCP_PML_SKIP != pml_state->state) {
        if( OMPI_SUCCESS != (ret = mca_pml_crcpw_module.wrapped_pml_module.pml_irecv_init(buf, count, datatype, src, tag, comm, request) ) ) {
            PML_CRCP_STATE_RETURN(pml_state);
            return ret;
        }
    }

    pml_state->state = OMPI_CRCP_PML_POST;
    pml_state = ompi_crcp.pml_irecv_init(buf, count, datatype, src, tag, comm, request,  pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    PML_CRCP_STATE_RETURN(pml_state);

    return OMPI_SUCCESS;
}

int mca_pml_crcpw_irecv( void *buf, size_t count, ompi_datatype_t *datatype, int src, int tag,
                         struct ompi_communicator_t* comm, struct ompi_request_t **request )
{
    int ret;
    ompi_crcp_base_pml_state_t * pml_state = NULL;

    PML_CRCP_STATE_ALLOC(pml_state, ret);

    pml_state->wrapped_pml_component = &(mca_pml_crcpw_module.wrapped_pml_component);
    pml_state->wrapped_pml_module    = &(mca_pml_crcpw_module.wrapped_pml_module);

    pml_state->state = OMPI_CRCP_PML_PRE;
    pml_state = ompi_crcp.pml_irecv(buf, count, datatype, src, tag, comm, request, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    if( OMPI_CRCP_PML_DONE == pml_state->state) {
        goto CLEANUP;
    }

    if( OMPI_CRCP_PML_SKIP != pml_state->state) {
        if( OMPI_SUCCESS != (ret = mca_pml_crcpw_module.wrapped_pml_module.pml_irecv(buf, count, datatype, src, tag, comm, request) ) ) {
            PML_CRCP_STATE_RETURN(pml_state);
            return ret;
        }
    }

    pml_state->state = OMPI_CRCP_PML_POST;
    pml_state = ompi_crcp.pml_irecv(buf, count, datatype, src, tag, comm, request, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

 CLEANUP:
    PML_CRCP_STATE_RETURN(pml_state);

    return OMPI_SUCCESS;
}

int mca_pml_crcpw_recv(  void *buf, size_t count, ompi_datatype_t *datatype, int src, int tag,
                         struct ompi_communicator_t* comm,  ompi_status_public_t* given_status)
{
    int ret = OMPI_SUCCESS, actual_ret = OMPI_SUCCESS;
    ompi_status_public_t* status = NULL;
    ompi_crcp_base_pml_state_t * pml_state = NULL;

    PML_CRCP_STATE_ALLOC(pml_state, ret);

    pml_state->wrapped_pml_component = &(mca_pml_crcpw_module.wrapped_pml_component);
    pml_state->wrapped_pml_module    = &(mca_pml_crcpw_module.wrapped_pml_module);

    if( given_status == NULL) {
        status = (ompi_status_public_t*)malloc(sizeof(ompi_status_public_t));
    }
    else {
        status = given_status;
    }

    pml_state->state = OMPI_CRCP_PML_PRE;
    pml_state = ompi_crcp.pml_recv(buf, count, datatype, src, tag, comm, status, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    if( OMPI_CRCP_PML_DONE == pml_state->state) {
        goto CLEANUP;
    }

    if( OMPI_CRCP_PML_SKIP != pml_state->state) {
        if( OMPI_SUCCESS != (actual_ret = mca_pml_crcpw_module.wrapped_pml_module.pml_recv(buf, count, datatype, src, tag, comm, status) ) ) {
            PML_CRCP_STATE_RETURN(pml_state);
            return ret;
        }
    }

    pml_state->state = OMPI_CRCP_PML_POST;
    pml_state = ompi_crcp.pml_recv(buf, count, datatype, src, tag, comm, status, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    if( given_status == NULL) {
        free(status);
    }

 CLEANUP:
    PML_CRCP_STATE_RETURN(pml_state);

    opal_cr_stall_check = false;
    OPAL_CR_TEST_CHECKPOINT_READY();

    return actual_ret;
}

int mca_pml_crcpw_dump( struct ompi_communicator_t* comm, int verbose )
{
    int ret;
    ompi_crcp_base_pml_state_t * pml_state = NULL;

    PML_CRCP_STATE_ALLOC(pml_state, ret);

    pml_state->wrapped_pml_component = &(mca_pml_crcpw_module.wrapped_pml_component);
    pml_state->wrapped_pml_module    = &(mca_pml_crcpw_module.wrapped_pml_module);

    pml_state->state = OMPI_CRCP_PML_PRE;
    pml_state = ompi_crcp.pml_dump(comm, verbose, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    if( OMPI_CRCP_PML_SKIP != pml_state->state) {
        if( OMPI_SUCCESS != (ret = mca_pml_crcpw_module.wrapped_pml_module.pml_dump(comm, verbose) ) ) {
            PML_CRCP_STATE_RETURN(pml_state);
            return ret;
        }
    }

    pml_state->state = OMPI_CRCP_PML_POST;
    pml_state = ompi_crcp.pml_dump(comm, verbose, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    PML_CRCP_STATE_RETURN(pml_state);

    return OMPI_SUCCESS;
}

int mca_pml_crcpw_progress(void)
{
    int ret;
    ompi_crcp_base_pml_state_t * pml_state = NULL;

    if( OPAL_LIKELY(NULL == ompi_crcp.pml_progress) ) {
        return mca_pml_crcpw_module.wrapped_pml_module.pml_progress();
    }

    PML_CRCP_STATE_ALLOC(pml_state, ret);

    pml_state->wrapped_pml_component = &(mca_pml_crcpw_module.wrapped_pml_component);
    pml_state->wrapped_pml_module    = &(mca_pml_crcpw_module.wrapped_pml_module);

    pml_state->state = OMPI_CRCP_PML_PRE;
    pml_state = ompi_crcp.pml_progress(pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    if( OMPI_CRCP_PML_SKIP != pml_state->state) {
        if( OMPI_SUCCESS != (ret = mca_pml_crcpw_module.wrapped_pml_module.pml_progress() ) ) {
            PML_CRCP_STATE_RETURN(pml_state);
            return ret;
        }
    }

    pml_state->state = OMPI_CRCP_PML_POST;
    pml_state = ompi_crcp.pml_progress(pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    PML_CRCP_STATE_RETURN(pml_state);

    return OMPI_SUCCESS;
}

int mca_pml_crcpw_start( size_t count, ompi_request_t** requests )
{
    int ret;
    ompi_crcp_base_pml_state_t * pml_state = NULL;

    PML_CRCP_STATE_ALLOC(pml_state, ret);

    pml_state->wrapped_pml_component = &(mca_pml_crcpw_module.wrapped_pml_component);
    pml_state->wrapped_pml_module    = &(mca_pml_crcpw_module.wrapped_pml_module);

    pml_state->state = OMPI_CRCP_PML_PRE;
    pml_state = ompi_crcp.pml_start(count, requests, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    if( OMPI_CRCP_PML_DONE == pml_state->state) {
        goto CLEANUP;
    }

    if( OMPI_CRCP_PML_SKIP != pml_state->state) {
        if( OMPI_SUCCESS != (ret = mca_pml_crcpw_module.wrapped_pml_module.pml_start(count, requests) ) ) {
            PML_CRCP_STATE_RETURN(pml_state);
            return ret;
        }
    }

    pml_state->state = OMPI_CRCP_PML_POST;
    pml_state = ompi_crcp.pml_start(count, requests, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

 CLEANUP:
    PML_CRCP_STATE_RETURN(pml_state);

    return OMPI_SUCCESS;
}

int mca_pml_crcpw_ft_event(int state)
{
    int ret;
    ompi_crcp_base_pml_state_t * pml_state = NULL;

    PML_CRCP_STATE_ALLOC(pml_state, ret);

    pml_state->wrapped_pml_component = &(mca_pml_crcpw_module.wrapped_pml_component);
    pml_state->wrapped_pml_module    = &(mca_pml_crcpw_module.wrapped_pml_module);

    pml_state->state = OMPI_CRCP_PML_PRE;
    pml_state = ompi_crcp.pml_ft_event(state, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    if( OMPI_CRCP_PML_SKIP != pml_state->state &&
        NULL != mca_pml_crcpw_module.wrapped_pml_module.pml_ft_event) {
        if( OMPI_SUCCESS != (ret = mca_pml_crcpw_module.wrapped_pml_module.pml_ft_event(state) ) ) {
            PML_CRCP_STATE_RETURN(pml_state);
            return ret;
        }
    }

    pml_state->state = OMPI_CRCP_PML_POST;
    pml_state = ompi_crcp.pml_ft_event(state, pml_state);
    if( OMPI_SUCCESS != pml_state->error_code) {
        ret =  pml_state->error_code;
        PML_CRCP_STATE_RETURN(pml_state);
        return ret;
    }

    PML_CRCP_STATE_RETURN(pml_state);

    return OMPI_SUCCESS;
}
