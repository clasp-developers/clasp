/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_OSC_RDMA_REPLYREQ_H
#define OMPI_OSC_RDMA_REPLYREQ_H

#include "osc_rdma.h"
#include "osc_rdma_longreq.h"

#include "opal/class/opal_list.h"
#include "ompi/datatype/ompi_datatype.h"
#include "opal/datatype/opal_convertor.h"
#include "ompi/communicator/communicator.h"
#include "ompi/proc/proc.h"
#include "ompi/memchecker.h"


struct ompi_osc_rdma_replyreq_t {
    opal_list_item_t super;

    /** pointer to the module that created the replyreq */
    ompi_osc_rdma_module_t *rep_module;

    /** Datatype for the target side of the operation */
    struct ompi_datatype_t *rep_target_datatype;
    /** Convertor for the target.  Always setup for send. */
    opal_convertor_t rep_target_convertor;
    /** packed size of message on the target side */
    size_t rep_target_bytes_packed;

    /** rank in module's communicator for origin of operation */
    int rep_origin_rank;
    /** pointer to the proc structure for the origin of the operation */
    ompi_proc_t *rep_origin_proc;

    ompi_ptr_t rep_origin_sendreq;
};
typedef struct ompi_osc_rdma_replyreq_t ompi_osc_rdma_replyreq_t;
OBJ_CLASS_DECLARATION(ompi_osc_rdma_replyreq_t);


/** allocate and populate a replyreq structure.  datatype is
    RETAINed for the life of the replyreq */
int
ompi_osc_rdma_replyreq_alloc_init(ompi_osc_rdma_module_t *module,
                                int origin,
                                ompi_ptr_t origin_request,
                                OPAL_PTRDIFF_TYPE target_displacement,
                                int target_count,
                                struct ompi_datatype_t *datatype,
                                ompi_osc_rdma_replyreq_t **replyreq);


static inline int
ompi_osc_rdma_replyreq_alloc(ompi_osc_rdma_module_t *module,
                           int origin_rank,
                           ompi_osc_rdma_replyreq_t **replyreq)
{
    int ret;
    opal_free_list_item_t *item;
    ompi_proc_t *proc = ompi_comm_peer_lookup( module->m_comm, origin_rank );

    /* BWB - FIX ME - is this really the right return code? */
    if (NULL == proc) return OMPI_ERR_OUT_OF_RESOURCE;

    OPAL_FREE_LIST_GET(&mca_osc_rdma_component.c_replyreqs,
                       item, ret);
    if (OMPI_SUCCESS != ret) return ret;
    *replyreq = (ompi_osc_rdma_replyreq_t*) item;

    (*replyreq)->rep_module = module;
    (*replyreq)->rep_origin_rank = origin_rank;
    (*replyreq)->rep_origin_proc = proc;

    return OMPI_SUCCESS;
}


static inline int
ompi_osc_rdma_replyreq_init_target(ompi_osc_rdma_replyreq_t *replyreq,
                                 void *target_addr,
                                 int target_count,
                                 struct ompi_datatype_t *target_dt)
{
    OBJ_RETAIN(target_dt);
    replyreq->rep_target_datatype = target_dt;

    opal_convertor_copy_and_prepare_for_send(replyreq->rep_origin_proc->proc_convertor,
                                             &(target_dt->super),
                                             target_count,
                                             target_addr,
                                             0,
                                             &(replyreq->rep_target_convertor));
    opal_convertor_get_packed_size(&replyreq->rep_target_convertor,
                                   &replyreq->rep_target_bytes_packed);

    return OMPI_SUCCESS;
}


static inline int
ompi_osc_rdma_replyreq_init_origin(ompi_osc_rdma_replyreq_t *replyreq,
                                 ompi_ptr_t origin_request)
{
    replyreq->rep_origin_sendreq = origin_request;

    return OMPI_SUCCESS;
}


static inline int
ompi_osc_rdma_replyreq_free(ompi_osc_rdma_replyreq_t *replyreq)
{
    MEMCHECKER(
        memchecker_convertor_call(&opal_memchecker_base_mem_defined,
                                  &replyreq->rep_target_convertor);
    );
    opal_convertor_cleanup(&replyreq->rep_target_convertor);

    OBJ_RELEASE(replyreq->rep_target_datatype);

    OPAL_FREE_LIST_RETURN(&mca_osc_rdma_component.c_replyreqs,
                          (opal_list_item_t*) replyreq);
 
    return OMPI_SUCCESS;
}

#endif /* OMPI_OSC_RDMA_REPLYREQ_H */
