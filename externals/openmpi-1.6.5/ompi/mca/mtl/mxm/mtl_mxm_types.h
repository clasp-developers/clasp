/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MTL_MXM_TYPES_H_HAS_BEEN_INCLUDED
#define MTL_MXM_TYPES_H_HAS_BEEN_INCLUDED

#include "ompi_config.h"
#include "mtl_mxm.h"

#include "ompi/mca/mtl/mtl.h"
#include "ompi/mca/mtl/base/base.h"
#include "ompi/communicator/communicator.h"
#include "mtl_mxm_endpoint.h" 


BEGIN_C_DECLS

/** 
 * MTL Module Interface
 */
typedef struct mca_mtl_mxm_module_t {
    mca_mtl_base_module_t super; /**< base MTL interface */
    int                   verbose;
    int                   mxm_np;
    mxm_h                 mxm_context;
    mxm_ep_h              ep;
#if MXM_API < MXM_VERSION(1,5)
    mxm_context_opts_t    mxm_opts;
#else
    mxm_context_opts_t   *mxm_opts;
#endif
#if MXM_API >= MXM_VERSION(2,0)
    int                   using_mem_hooks;
#endif
} mca_mtl_mxm_module_t;


#if MXM_API < MXM_VERSION(2,0)
typedef struct ompi_mtl_mxm_ep_conn_info_t {
    struct sockaddr_storage  ptl_addr[MXM_PTL_LAST];
} ompi_mtl_mxm_ep_conn_info_t;
#endif

extern mca_mtl_mxm_module_t ompi_mtl_mxm;

typedef struct mca_mtl_mxm_component_t {
    mca_mtl_base_component_2_0_0_t super; /**< base MTL component */
} mca_mtl_mxm_component_t;


OMPI_DECLSPEC mca_mtl_mxm_component_t mca_mtl_mxm_component;


static inline mxm_conn_h ompi_mtl_mxm_conn_lookup(struct ompi_communicator_t* comm, int rank) {
    ompi_proc_t* ompi_proc = ompi_comm_peer_lookup(comm, rank);
    mca_mtl_mxm_endpoint_t *endpoint = (mca_mtl_mxm_endpoint_t*) ompi_proc->proc_pml;

    return endpoint->mxm_conn;
}

static inline mxm_mq_h ompi_mtl_mxm_mq_lookup(struct ompi_communicator_t* comm) {
    return (mxm_mq_h)comm->c_pml_comm;
}

static inline void ompi_mtl_mxm_to_mpi_status(mxm_error_t status, ompi_status_public_t *ompi_status) {
    switch (status) {
    case MXM_OK:
        ompi_status->MPI_ERROR = OMPI_SUCCESS;
        break;
    case MXM_ERR_CANCELED:
        ompi_status->_cancelled = true;
        break;
    case MXM_ERR_MESSAGE_TRUNCATED:
        ompi_status->MPI_ERROR = MPI_ERR_TRUNCATE;
        break;
    default:
        ompi_status->MPI_ERROR = MPI_ERR_INTERN;
        break;
    }
}

static inline void ompi_mtl_mxm_set_recv_envelope(mxm_recv_req_t *req,
                                                  struct ompi_communicator_t *comm,
                                                  int src, int tag) {
    req->base.mq    = (mxm_mq_h)comm->c_pml_comm;
    req->base.conn  = (src == MPI_ANY_SOURCE)
                           ? NULL
                           : ompi_mtl_mxm_conn_lookup(comm, src);
    if (tag == MPI_ANY_TAG) {
        req->tag      = 0;
        req->tag_mask = 0x80000000U; /* MPI_ANY_TAG should not match against negative tags */
    } else {
        req->tag      = tag;
        req->tag_mask = 0xffffffffU;
    }
}

END_C_DECLS

#endif

