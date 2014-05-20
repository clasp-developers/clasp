/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
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

#include "opal/class/opal_list.h"
#include "opal/threads/mutex.h"
#include "osc_pt2pt.h"
#include "osc_pt2pt_sendreq.h"
#include "osc_pt2pt_header.h"
#include "osc_pt2pt_data_move.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/op/op.h"
#include "ompi/win/win.h"
#include "ompi/memchecker.h"

static int
enqueue_sendreq(ompi_osc_pt2pt_module_t *module,
                ompi_osc_pt2pt_sendreq_t *sendreq)
{
    OPAL_THREAD_LOCK(&(module->p2p_lock));
    opal_list_append(&(module->p2p_pending_sendreqs),
                     (opal_list_item_t*) sendreq);
    module->p2p_num_pending_sendreqs[sendreq->req_target_rank]++;
    OPAL_THREAD_UNLOCK(&(module->p2p_lock));

    return OMPI_SUCCESS;
}



int
ompi_osc_pt2pt_module_accumulate(void *origin_addr, int origin_count,
                                 struct ompi_datatype_t *origin_dt,
                                 int target, OPAL_PTRDIFF_TYPE target_disp, 
                                 int target_count,
                                 struct ompi_datatype_t *target_dt,
                                 struct ompi_op_t *op, ompi_win_t *win)
{
    int ret;
    ompi_osc_pt2pt_sendreq_t *sendreq;

    if ((OMPI_WIN_STARTED & ompi_win_get_mode(win)) &&
        (!P2P_MODULE(win)->p2p_sc_remote_active_ranks[target])) {
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
    ret = ompi_osc_pt2pt_sendreq_alloc_init(OMPI_OSC_PT2PT_ACC,
                                            origin_addr,
                                            origin_count,
                                            origin_dt,
                                            target,
                                            target_disp,
                                            target_count,
                                            target_dt,
                                            P2P_MODULE(win),
                                            &sendreq);
    MEMCHECKER(
        memchecker_convertor_call(&opal_memchecker_base_mem_noaccess,
                                  &sendreq->req_origin_convertor);
    );
    if (OMPI_SUCCESS != ret) return ret;

    sendreq->req_op_id = op->o_f_to_c_index;

    /* enqueue sendreq */
    ret = enqueue_sendreq(P2P_MODULE(win), sendreq);

    return ret;
}


int
ompi_osc_pt2pt_module_get(void *origin_addr,
                          int origin_count,
                          struct ompi_datatype_t *origin_dt,
                          int target,
                          OPAL_PTRDIFF_TYPE target_disp,
                          int target_count,
                          struct ompi_datatype_t *target_dt,
                          ompi_win_t *win)
{
    int ret;
    ompi_osc_pt2pt_sendreq_t *sendreq;

    if ((OMPI_WIN_STARTED & ompi_win_get_mode(win)) &&
        (!P2P_MODULE(win)->p2p_sc_remote_active_ranks[target])) {
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
    ret = ompi_osc_pt2pt_sendreq_alloc_init(OMPI_OSC_PT2PT_GET,
                                            origin_addr,
                                            origin_count,
                                            origin_dt,
                                            target,
                                            target_disp,
                                            target_count,
                                            target_dt,
                                            P2P_MODULE(win),
                                            &sendreq);
    MEMCHECKER(
        memchecker_convertor_call(&opal_memchecker_base_mem_noaccess,
                                  &sendreq->req_origin_convertor);
    );
    if (OMPI_SUCCESS != ret) return ret;

    /* enqueue sendreq */
    ret = enqueue_sendreq(P2P_MODULE(win), sendreq);

    return ret;
}


int
ompi_osc_pt2pt_module_put(void *origin_addr, int origin_count,
                          struct ompi_datatype_t *origin_dt,
                          int target, OPAL_PTRDIFF_TYPE target_disp, 
                          int target_count,
                          struct ompi_datatype_t *target_dt, ompi_win_t *win)
{
    int ret;
    ompi_osc_pt2pt_sendreq_t *sendreq;

    if ((OMPI_WIN_STARTED & ompi_win_get_mode(win)) &&
        (!P2P_MODULE(win)->p2p_sc_remote_active_ranks[target])) {
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
    ret = ompi_osc_pt2pt_sendreq_alloc_init(OMPI_OSC_PT2PT_PUT,
                                            origin_addr,
                                            origin_count,
                                            origin_dt,
                                            target,
                                            target_disp,
                                            target_count,
                                            target_dt,
                                            P2P_MODULE(win),
                                            &sendreq);
    MEMCHECKER(
        memchecker_convertor_call(&opal_memchecker_base_mem_noaccess,
                                  &sendreq->req_origin_convertor);
    );
    if (OMPI_SUCCESS != ret) return ret;

    /* enqueue sendreq */
    ret = enqueue_sendreq(P2P_MODULE(win), sendreq);

    return ret;
}
