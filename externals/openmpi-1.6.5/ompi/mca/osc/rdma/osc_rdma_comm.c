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

#include "ompi_config.h"
#include "mpi.h"

#include <stdio.h>

#include "osc_rdma.h"
#include "osc_rdma_sendreq.h"
#include "osc_rdma_header.h"
#include "osc_rdma_data_move.h"
#include "ompi/memchecker.h"
#include "ompi/mca/osc/base/osc_base_obj_convert.h"
#include "opal_stdint.h"

static int
enqueue_sendreq(ompi_osc_rdma_module_t *module,
                ompi_osc_rdma_sendreq_t *sendreq)
{
    OPAL_THREAD_LOCK(&(module->m_lock));
    opal_list_append(&(module->m_pending_sendreqs),
                     (opal_list_item_t*) sendreq);
    module->m_num_pending_sendreqs[sendreq->req_target_rank]++;
    OPAL_THREAD_UNLOCK(&(module->m_lock));

    return OMPI_SUCCESS;
}


int
ompi_osc_rdma_module_accumulate(void *origin_addr, int origin_count,
                                 struct ompi_datatype_t *origin_dt,
                                 int target, OPAL_PTRDIFF_TYPE target_disp, 
                                 int target_count,
                                 struct ompi_datatype_t *target_dt,
                                 struct ompi_op_t *op, ompi_win_t *win)
{
    int ret;
    ompi_osc_rdma_sendreq_t *sendreq;
    ompi_osc_rdma_module_t *module = GET_MODULE(win);

    if ((OMPI_WIN_STARTED & ompi_win_get_mode(win)) &&
        (!module->m_sc_remote_active_ranks[target])) {
        return MPI_ERR_RMA_SYNC;
    }

    if (OMPI_WIN_FENCE & ompi_win_get_mode(win)) {
        /* well, we're definitely in an access epoch now */
        ompi_win_set_mode(win, OMPI_WIN_FENCE | OMPI_WIN_ACCESS_EPOCH |
                          OMPI_WIN_EXPOSE_EPOCH);
    }

    /* shortcut 0 count case */
    if (0 == origin_count || 0 == target_count) {
            return OMPI_SUCCESS;
    }

    /* create sendreq */
    ret = ompi_osc_rdma_sendreq_alloc_init(OMPI_OSC_RDMA_ACC,
                                            origin_addr,
                                            origin_count,
                                            origin_dt,
                                            target,
                                            target_disp,
                                            target_count,
                                            target_dt,
                                            module,
                                            &sendreq);
    MEMCHECKER(
        memchecker_convertor_call(&opal_memchecker_base_mem_noaccess,
                                  &sendreq->req_origin_convertor);
    );
    if (OMPI_SUCCESS != ret) return ret;

    sendreq->req_op_id = op->o_f_to_c_index;

    if (module->m_eager_send_active) {
        /* accumulate semantics require send to self, which is bloody
           expensive with the extra copies.  Put a shortcut in for the
           common case. */
        if (target == ompi_comm_rank(sendreq->req_module->m_comm) &&
            ompi_datatype_is_contiguous_memory_layout(sendreq->req_target_datatype,
                                                 sendreq->req_target_count) &&
            !opal_convertor_need_buffers(&sendreq->req_origin_convertor) &&
            0 == OPAL_THREAD_TRYLOCK(&module->m_acc_lock)) {
            void *target_buffer = (unsigned char*) module->m_win->w_baseptr + 
                ((unsigned long) target_disp * 
                 module->m_win->w_disp_unit);

            struct iovec iov;
            uint32_t iov_count = 1;
            size_t max_data = sendreq->req_origin_bytes_packed;

            iov.iov_len = max_data;
            iov.iov_base = NULL;
            ret = opal_convertor_pack(&sendreq->req_origin_convertor,
                                      &iov, &iov_count,
                                      &max_data);
            if (ret < 0) {
                OPAL_THREAD_UNLOCK(&module->m_acc_lock);
                return OMPI_ERR_FATAL;
            }

            ret = ompi_osc_base_process_op(target_buffer,
                                           iov.iov_base,
                                           max_data,
                                           target_dt,
                                           target_count,
                                           op);
            /* unlock the window for accumulates */
            OPAL_THREAD_UNLOCK(&module->m_acc_lock);
            ompi_osc_rdma_sendreq_free(sendreq);
            return ret;
        }

        OPAL_THREAD_LOCK(&module->m_lock);
        sendreq->req_module->m_num_pending_out += 1;
        module->m_num_pending_sendreqs[sendreq->req_target_rank] += 1;
        OPAL_THREAD_UNLOCK(&(module->m_lock));

        ret  = ompi_osc_rdma_sendreq_send(module, sendreq);

        if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == ret) {
            OPAL_THREAD_LOCK(&module->m_lock);
            sendreq->req_module->m_num_pending_out -= 1;
            opal_list_append(&(module->m_pending_sendreqs),
                             (opal_list_item_t*) sendreq);
            OPAL_THREAD_UNLOCK(&module->m_lock);
            ret = OMPI_SUCCESS;
        }
    } else {
        /* enqueue sendreq */
        ret = enqueue_sendreq(module, sendreq);
    }

    return ret;
}


int
ompi_osc_rdma_module_get(void *origin_addr,
                          int origin_count,
                          struct ompi_datatype_t *origin_dt,
                          int target,
                          OPAL_PTRDIFF_TYPE target_disp,
                          int target_count,
                          struct ompi_datatype_t *target_dt,
                          ompi_win_t *win)
{
    int ret;
    ompi_osc_rdma_sendreq_t *sendreq;
    ompi_osc_rdma_module_t *module = GET_MODULE(win);

    if ((OMPI_WIN_STARTED & ompi_win_get_mode(win)) &&
        (!module->m_sc_remote_active_ranks[target])) {
        return MPI_ERR_RMA_SYNC;
    }

    if (OMPI_WIN_FENCE & ompi_win_get_mode(win)) {
        /* well, we're definitely in an access epoch now */
        ompi_win_set_mode(win, OMPI_WIN_FENCE | OMPI_WIN_ACCESS_EPOCH |
                          OMPI_WIN_EXPOSE_EPOCH);
    }

    /* shortcut 0 count case */
    if (0 == origin_count || 0 == target_count) {
            return OMPI_SUCCESS;
    }

    /* create sendreq */
    ret = ompi_osc_rdma_sendreq_alloc_init(OMPI_OSC_RDMA_GET,
                                           origin_addr,
                                           origin_count,
                                           origin_dt,
                                           target,
                                           target_disp,
                                           target_count,
                                           target_dt,
                                           module,
                                           &sendreq);
    MEMCHECKER(
        memchecker_convertor_call(&opal_memchecker_base_mem_noaccess,
                                  &sendreq->req_origin_convertor);
    );
    if (OMPI_SUCCESS != ret) return ret;

    if (module->m_eager_send_active) {
        OPAL_THREAD_LOCK(&module->m_lock);
        sendreq->req_module->m_num_pending_out += 1;
        module->m_num_pending_sendreqs[sendreq->req_target_rank] += 1;
        OPAL_THREAD_UNLOCK(&(module->m_lock));

        ret  = ompi_osc_rdma_sendreq_send(module, sendreq);

        if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == ret) {
            OPAL_THREAD_LOCK(&module->m_lock);
            sendreq->req_module->m_num_pending_out -= 1;
            opal_list_append(&(module->m_pending_sendreqs),
                             (opal_list_item_t*) sendreq);
            OPAL_THREAD_UNLOCK(&module->m_lock);
            ret = OMPI_SUCCESS;
        }
    } else {
        /* enqueue sendreq */
        ret = enqueue_sendreq(module, sendreq);
    }

    return ret;
}


int
ompi_osc_rdma_module_put(void *origin_addr, int origin_count,
                          struct ompi_datatype_t *origin_dt,
                          int target, OPAL_PTRDIFF_TYPE target_disp, 
                          int target_count,
                          struct ompi_datatype_t *target_dt, ompi_win_t *win)
{
    int ret;
    ompi_osc_rdma_sendreq_t *sendreq;
    ompi_osc_rdma_module_t *module = GET_MODULE(win);

    if ((OMPI_WIN_STARTED & ompi_win_get_mode(win)) &&
        (!module->m_sc_remote_active_ranks[target])) {
        return MPI_ERR_RMA_SYNC;
    }

    if (OMPI_WIN_FENCE & ompi_win_get_mode(win)) {
        /* well, we're definitely in an access epoch now */
        ompi_win_set_mode(win, OMPI_WIN_FENCE | OMPI_WIN_ACCESS_EPOCH |
                          OMPI_WIN_EXPOSE_EPOCH);
    }

    /* shortcut 0 count case */
    if (0 == origin_count || 0 == target_count) {
            return OMPI_SUCCESS;
    }

    /* create sendreq */
    ret = ompi_osc_rdma_sendreq_alloc_init(OMPI_OSC_RDMA_PUT,
                                            origin_addr,
                                            origin_count,
                                            origin_dt,
                                            target,
                                            target_disp,
                                            target_count,
                                            target_dt,
                                            module,
                                            &sendreq);
    MEMCHECKER(
        memchecker_convertor_call(&opal_memchecker_base_mem_noaccess,
                                  &sendreq->req_origin_convertor);
    );
    if (OMPI_SUCCESS != ret) return ret;

    if (module->m_eager_send_active) {
        OPAL_THREAD_LOCK(&module->m_lock);
        sendreq->req_module->m_num_pending_out += 1;
        module->m_num_pending_sendreqs[sendreq->req_target_rank] += 1;
        OPAL_THREAD_UNLOCK(&(module->m_lock));

        ret  = ompi_osc_rdma_sendreq_send(module, sendreq);

        if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == ret) {
            OPAL_THREAD_LOCK(&module->m_lock);
            sendreq->req_module->m_num_pending_out -= 1;
            opal_list_append(&(module->m_pending_sendreqs),
                             (opal_list_item_t*) sendreq);
            OPAL_THREAD_UNLOCK(&module->m_lock);
            ret = OMPI_SUCCESS;
        }
    } else {
        /* enqueue sendreq */
        ret = enqueue_sendreq(module, sendreq);
    }

    return ret;
}
