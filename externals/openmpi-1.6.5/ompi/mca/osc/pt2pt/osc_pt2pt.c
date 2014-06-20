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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "osc_pt2pt.h"
#include "osc_pt2pt_sendreq.h"

#include "ompi/mca/osc/base/base.h"
#include "opal/runtime/opal_progress.h"
#include "opal/threads/mutex.h"
#include "ompi/win/win.h"
#include "ompi/communicator/communicator.h"
#include "ompi/request/request.h"
#include "mpi.h"


int
ompi_osc_pt2pt_module_free(ompi_win_t *win)
{
    int ret = OMPI_SUCCESS;
    int tmp;
    ompi_osc_pt2pt_module_t *module = P2P_MODULE(win);

    opal_output_verbose(1, ompi_osc_base_output,
                        "pt2pt component destroying window with id %d",
                        ompi_comm_get_cid(module->p2p_comm));

    /* finish with a barrier */
    if (ompi_group_size(win->w_group) > 1) {
        ret = module->p2p_comm->c_coll.coll_barrier(module->p2p_comm,
                                                    module->p2p_comm->c_coll.coll_barrier_module);
    }

    /* remove from component information */
    OPAL_THREAD_LOCK(&mca_osc_pt2pt_component.p2p_c_lock);
    tmp = opal_hash_table_remove_value_uint32(&mca_osc_pt2pt_component.p2p_c_modules,
                                              ompi_comm_get_cid(module->p2p_comm));
    /* only take the output of hast_table_remove if there wasn't already an error */
    ret = (ret != OMPI_SUCCESS) ? ret : tmp;

    if (0 == opal_hash_table_get_size(&mca_osc_pt2pt_component.p2p_c_modules)) {
#if OPAL_ENABLE_PROGRESS_THREADS
        void *foo;

        mca_osc_pt2pt_component.p2p_c_thread_run = false;
        opal_condition_broadcast(&ompi_request_cond);
        opal_thread_join(&mca_osc_pt2pt_component.p2p_c_thread, &foo);
#else
        opal_progress_unregister(ompi_osc_pt2pt_component_progress);
#endif
    }
    OPAL_THREAD_UNLOCK(&mca_osc_pt2pt_component.p2p_c_lock);

    win->w_osc_module = NULL;

    OBJ_DESTRUCT(&module->p2p_unlocks_pending);
    OBJ_DESTRUCT(&module->p2p_locks_pending);
    OBJ_DESTRUCT(&module->p2p_copy_pending_sendreqs);
    OBJ_DESTRUCT(&module->p2p_pending_sendreqs);
    OBJ_DESTRUCT(&module->p2p_acc_lock);
    OBJ_DESTRUCT(&module->p2p_cond);
    OBJ_DESTRUCT(&module->p2p_lock);

    if (NULL != module->p2p_sc_remote_ranks) {
        free(module->p2p_sc_remote_ranks);
    }
    if (NULL != module->p2p_sc_remote_active_ranks) {
        free(module->p2p_sc_remote_active_ranks);
    }
    if (NULL != module->p2p_fence_coll_counts) {
        free(module->p2p_fence_coll_counts);
    }
    if (NULL != module->p2p_copy_num_pending_sendreqs) {
        free(module->p2p_copy_num_pending_sendreqs);
    }
    if (NULL != module->p2p_num_pending_sendreqs) {
        free(module->p2p_num_pending_sendreqs);
    }
    if (NULL != module->p2p_comm) ompi_comm_free(&module->p2p_comm);

#if OPAL_ENABLE_DEBUG
    memset(module, 0, sizeof(ompi_osc_base_module_t));
#endif
    if (NULL != module) free(module);

    return ret;
}
