/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "osc_pt2pt.h"
#include "osc_pt2pt_sendreq.h"
#include "osc_pt2pt_longreq.h"
#include "osc_pt2pt_header.h"
#include "osc_pt2pt_data_move.h"

#include "mpi.h"
#include "opal/runtime/opal_progress.h"
#include "opal/threads/mutex.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/osc/base/base.h"


/* Must hold module's lock before calling... */
static inline void
ompi_osc_pt2pt_flip_sendreqs(ompi_osc_pt2pt_module_t *module)
{
    unsigned int *tmp;

    tmp = module->p2p_copy_num_pending_sendreqs;
    module->p2p_copy_num_pending_sendreqs = 
        module->p2p_num_pending_sendreqs;
    module->p2p_num_pending_sendreqs = tmp;
    memset(module->p2p_num_pending_sendreqs, 0,
           sizeof(unsigned int) * ompi_comm_size(module->p2p_comm));

    /* Copy in all the pending requests */
    opal_list_join(&module->p2p_copy_pending_sendreqs,
                   opal_list_get_end(&module->p2p_copy_pending_sendreqs),
                   &module->p2p_pending_sendreqs);
}


int
ompi_osc_pt2pt_module_fence(int assert, ompi_win_t *win)
{
    unsigned int incoming_reqs;
    int ret = OMPI_SUCCESS, i;
    ompi_osc_pt2pt_module_t *module = P2P_MODULE(win);
    int num_outgoing = 0;

    if (0 != (assert & MPI_MODE_NOPRECEDE)) {
        /* check that the user didn't lie to us - since NOPRECEDED
           must be specified by all processes if it is specified by
           any process, if we see this it is safe to assume that there
           are no pending operations anywhere needed to close out this
           epoch.  No need to lock, since it's a lookup and any
           pending modification of the pending_sendreqs during this
           time is an erroneous program. */
        if (0 != opal_list_get_size(&(module->p2p_pending_sendreqs))) {
            return MPI_ERR_RMA_SYNC;
        }

    } else {
        opal_list_item_t *item;

        /* "atomically" copy all the data we're going to be modifying
           into the copy... */
        OPAL_THREAD_LOCK(&(module->p2p_lock));
        ompi_osc_pt2pt_flip_sendreqs(module);
        OPAL_THREAD_UNLOCK(&(module->p2p_lock));

        num_outgoing = opal_list_get_size(&(module->p2p_copy_pending_sendreqs));

        /* find out how much data everyone is going to send us. */
        ret = module->p2p_comm->
            c_coll.coll_reduce_scatter(module->p2p_copy_num_pending_sendreqs,
                                       &incoming_reqs,
                                       module->p2p_fence_coll_counts,
                                       MPI_UNSIGNED,
                                       MPI_SUM,
                                       module->p2p_comm,
                                       module->p2p_comm->c_coll.coll_reduce_scatter_module);

        if (OMPI_SUCCESS != ret) {
            /* put the stupid data back for the user.  This is not
               cheap, but the user lost his data if we don't. */
            OPAL_THREAD_LOCK(&(module->p2p_lock));
            opal_list_join(&module->p2p_pending_sendreqs,
                           opal_list_get_end(&module->p2p_pending_sendreqs),
                           &module->p2p_copy_pending_sendreqs);
            
            for (i = 0 ; i < ompi_comm_size(module->p2p_comm) ; ++i) {
                module->p2p_num_pending_sendreqs[i] +=
                    module->p2p_copy_num_pending_sendreqs[i];
            }

            OPAL_THREAD_UNLOCK(&(module->p2p_lock));
            return ret;
        }

        OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_output,
                             "fence: waiting on %d in and %d out",
                             module->p2p_num_pending_in,
                             module->p2p_num_pending_out));

        /* try to start all the requests.  We've copied everything we
           need out of pending_sendreqs, so don't need the lock
           here */
        while (NULL != 
               (item = opal_list_remove_first(&(module->p2p_copy_pending_sendreqs)))) {
            ompi_osc_pt2pt_sendreq_t *req = 
                (ompi_osc_pt2pt_sendreq_t*) item;

            ret = ompi_osc_pt2pt_sendreq_send(module, req);

            if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == ret ) {
                opal_output_verbose(5, ompi_osc_base_output,
                                    "complete: failure in starting sendreq (%d).  Will try later.",
                                    ret);
                opal_list_append(&(module->p2p_copy_pending_sendreqs), item);
            } else if (OMPI_SUCCESS != ret) {
                return ret;
            } 
        }

        OPAL_THREAD_LOCK(&module->p2p_lock);
        /* possible we've already received a couple in messages, so
           add however many we're going to wait for */
        module->p2p_num_pending_in += incoming_reqs;
        module->p2p_num_pending_out += num_outgoing;

        /* now we know how many things we're waiting for - wait for them... */
        while (module->p2p_num_pending_in > 0 ||
               0 != module->p2p_num_pending_out) {
            opal_condition_wait(&module->p2p_cond, &module->p2p_lock);
        }
        OPAL_THREAD_UNLOCK(&module->p2p_lock);
    }

    /* all transfers are done - back to the real world we go */
    if (0 == (assert & MPI_MODE_NOSUCCEED)) {
        ompi_win_set_mode(win, OMPI_WIN_FENCE);
    } else {
        ompi_win_set_mode(win, 0);
    }

    return OMPI_SUCCESS;
}


int
ompi_osc_pt2pt_module_start(ompi_group_t *group,
                            int assert,
                            ompi_win_t *win)
{
    int i, ret = OMPI_SUCCESS;
    ompi_osc_pt2pt_module_t *module = P2P_MODULE(win);

    OBJ_RETAIN(group);
    ompi_group_increment_proc_count(group);

    OPAL_THREAD_LOCK(&(module->p2p_lock));
    if (NULL != module->p2p_sc_group) {
        OPAL_THREAD_UNLOCK(&module->p2p_lock);
        ret = MPI_ERR_RMA_SYNC;
        goto cleanup;
    }
    module->p2p_sc_group = group;    

    /* possible we've already received a couple in messages, so
       add however many we're going to wait for */
    module->p2p_num_post_msgs += ompi_group_size(module->p2p_sc_group);
    OPAL_THREAD_UNLOCK(&(module->p2p_lock));

    memset(module->p2p_sc_remote_active_ranks, 0,
           sizeof(bool) * ompi_comm_size(module->p2p_comm));

    /* for each process in the specified group, find it's rank in our
       communicator, store those indexes, and set the true / false in
       the active ranks table */
    for (i = 0 ; i < ompi_group_size(group) ; i++) {
        int comm_rank = -1, j;
        
        /* find the rank in the communicator associated with this windows */
        for (j = 0 ; j < ompi_comm_size(module->p2p_comm) ; ++j) {
            if (ompi_group_peer_lookup(module->p2p_sc_group, i) ==
                ompi_comm_peer_lookup(module->p2p_comm, j)) {
                comm_rank = j;
                break;
            }
        }
        if (comm_rank == -1) {
            ret = MPI_ERR_RMA_SYNC;
            goto cleanup;
        }

        module->p2p_sc_remote_active_ranks[comm_rank] = true;
        module->p2p_sc_remote_ranks[i] = comm_rank;
    }

    /* Set our mode to access w/ start */
    ompi_win_remove_mode(win, OMPI_WIN_FENCE);
    ompi_win_append_mode(win, OMPI_WIN_ACCESS_EPOCH | OMPI_WIN_STARTED);

    return OMPI_SUCCESS;

 cleanup:
    ompi_group_decrement_proc_count(group);
    OBJ_RELEASE(group);
    return ret;
}


int
ompi_osc_pt2pt_module_complete(ompi_win_t *win)
{
    int i;
    int ret = OMPI_SUCCESS;
    ompi_group_t *group;
    opal_list_item_t *item;
    ompi_osc_pt2pt_module_t *module = P2P_MODULE(win);

    /* wait for all the post messages */
    OPAL_THREAD_LOCK(&module->p2p_lock);
    while (0 != module->p2p_num_post_msgs) {
        opal_condition_wait(&module->p2p_cond, &module->p2p_lock);
    }

    ompi_osc_pt2pt_flip_sendreqs(module);

    /* for each process in group, send a control message with number
       of updates coming, then start all the requests */
    for (i = 0 ; i < ompi_group_size(module->p2p_sc_group) ; ++i) {
        int comm_rank = module->p2p_sc_remote_ranks[i];

        module->p2p_num_pending_out +=
            module->p2p_copy_num_pending_sendreqs[comm_rank];
    }
    OPAL_THREAD_UNLOCK(&module->p2p_lock);

    for (i = 0 ; i < ompi_group_size(module->p2p_sc_group) ; ++i) {
        int comm_rank = module->p2p_sc_remote_ranks[i];
        ret = ompi_osc_pt2pt_control_send(module, 
                                          ompi_group_peer_lookup(module->p2p_sc_group, i),
                                          OMPI_OSC_PT2PT_HDR_COMPLETE,
                                          module->p2p_copy_num_pending_sendreqs[comm_rank],
                                          0);
        assert(ret == OMPI_SUCCESS);
    }

    /* try to start all the requests.  We've copied everything we
       need out of pending_sendreqs, so don't need the lock
       here */
    while (NULL != 
           (item = opal_list_remove_first(&(module->p2p_copy_pending_sendreqs)))) {
        ompi_osc_pt2pt_sendreq_t *req = 
            (ompi_osc_pt2pt_sendreq_t*) item;

        ret = ompi_osc_pt2pt_sendreq_send(module, req);

        if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == ret ) {
            opal_output_verbose(5, ompi_osc_base_output,
                                "complete: failure in starting sendreq (%d).  Will try later.",
                                ret);
            opal_list_append(&(module->p2p_copy_pending_sendreqs), item);
        } else if (OMPI_SUCCESS != ret) {
            return ret;
        } 
    }

    /* wait for all the requests */
    OPAL_THREAD_LOCK(&module->p2p_lock);
    while (0 != module->p2p_num_pending_out) {
        opal_condition_wait(&module->p2p_cond, &module->p2p_lock);
    }

    group = module->p2p_sc_group;
    module->p2p_sc_group = NULL;

    OPAL_THREAD_UNLOCK(&module->p2p_lock);

    /* remove WIN_POSTED from our mode */
    ompi_win_remove_mode(win, OMPI_WIN_ACCESS_EPOCH | OMPI_WIN_STARTED);

    ompi_group_decrement_proc_count(group);
    OBJ_RELEASE(group);

    return ret;
}


int
ompi_osc_pt2pt_module_post(ompi_group_t *group,
                           int assert,
                           ompi_win_t *win)
{
    int i;
    ompi_osc_pt2pt_module_t *module = P2P_MODULE(win);

    OBJ_RETAIN(group);
    ompi_group_increment_proc_count(group);

    OPAL_THREAD_LOCK(&(module->p2p_lock));
    assert(NULL == module->p2p_pw_group);
    module->p2p_pw_group = group;    

    /* Set our mode to expose w/ post */
    ompi_win_remove_mode(win, OMPI_WIN_FENCE);
    ompi_win_append_mode(win, OMPI_WIN_EXPOSE_EPOCH | OMPI_WIN_POSTED);

    /* list how many complete counters we're still waiting on */
    module->p2p_num_complete_msgs +=
        ompi_group_size(module->p2p_pw_group);
    OPAL_THREAD_UNLOCK(&(module->p2p_lock));

    /* send a hello counter to everyone in group */
    for (i = 0 ; i < ompi_group_size(module->p2p_pw_group) ; ++i) {
        ompi_osc_pt2pt_control_send(module, 
                                    ompi_group_peer_lookup(group, i),
                                    OMPI_OSC_PT2PT_HDR_POST, 1, 0);
    }

    return OMPI_SUCCESS;
}


int
ompi_osc_pt2pt_module_wait(ompi_win_t *win)
{
    ompi_group_t *group;
    ompi_osc_pt2pt_module_t *module = P2P_MODULE(win);

    OPAL_THREAD_LOCK(&module->p2p_lock);
    while (0 != (module->p2p_num_pending_in) ||
           0 != (module->p2p_num_complete_msgs)) {
        opal_condition_wait(&module->p2p_cond, &module->p2p_lock);
    }

    group = module->p2p_pw_group;
    module->p2p_pw_group = NULL;
    OPAL_THREAD_UNLOCK(&module->p2p_lock);

    ompi_win_remove_mode(win, OMPI_WIN_EXPOSE_EPOCH | OMPI_WIN_POSTED);

    ompi_group_decrement_proc_count(group);
    OBJ_RELEASE(group);

    return OMPI_SUCCESS;
}


int 
ompi_osc_pt2pt_module_test(ompi_win_t *win,
                           int *flag)
{
    ompi_group_t *group;
    ompi_osc_pt2pt_module_t *module = P2P_MODULE(win);

#if !OPAL_ENABLE_PROGRESS_THREADS
    opal_progress();
#endif

    if (0 != (module->p2p_num_pending_in) ||
        0 != (module->p2p_num_complete_msgs)) {
        *flag = 0;
        return OMPI_SUCCESS;
    }

    *flag = 1;

    ompi_win_remove_mode(win, OMPI_WIN_EXPOSE_EPOCH | OMPI_WIN_POSTED);

    OPAL_THREAD_LOCK(&(module->p2p_lock));
    group = module->p2p_pw_group;
    module->p2p_pw_group = NULL;
    OPAL_THREAD_UNLOCK(&(module->p2p_lock));

    ompi_group_decrement_proc_count(group);
    OBJ_RELEASE(group);

    return OMPI_SUCCESS;
}


struct ompi_osc_pt2pt_pending_lock_t {
    opal_list_item_t super;
    ompi_proc_t *proc;
    int32_t lock_type;
};
typedef struct ompi_osc_pt2pt_pending_lock_t ompi_osc_pt2pt_pending_lock_t;
OBJ_CLASS_INSTANCE(ompi_osc_pt2pt_pending_lock_t, opal_list_item_t,
                   NULL, NULL);


int
ompi_osc_pt2pt_module_lock(int lock_type,
                           int target,
                           int assert,
                           ompi_win_t *win)
{
    ompi_osc_pt2pt_module_t *module = P2P_MODULE(win);
    ompi_proc_t *proc = ompi_comm_peer_lookup( module->p2p_comm, target );

    assert(lock_type != 0);

    /* set our mode on the window */
    ompi_win_remove_mode(win, OMPI_WIN_FENCE);
    ompi_win_append_mode(win, OMPI_WIN_ACCESS_EPOCH | OMPI_WIN_LOCK_ACCESS);

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_output,
                         "%d: sending lock request to %d", 
                         ompi_comm_rank(module->p2p_comm),
                         target));
    /* generate a lock request */
    ompi_osc_pt2pt_control_send(module, 
                                proc,
                                OMPI_OSC_PT2PT_HDR_LOCK_REQ,
                                ompi_comm_rank(module->p2p_comm),
                                lock_type);

    /* return */
    return OMPI_SUCCESS;
}


int
ompi_osc_pt2pt_module_unlock(int target,
                             ompi_win_t *win)
{
    int32_t out_count;
    opal_list_item_t *item;
    int ret;
    ompi_osc_pt2pt_module_t *module = P2P_MODULE(win);
    ompi_proc_t *proc = ompi_comm_peer_lookup( module->p2p_comm, target );

    OPAL_THREAD_LOCK(&module->p2p_lock);
    while (0 == module->p2p_lock_received_ack) {
        opal_condition_wait(&module->p2p_cond, &module->p2p_lock);
    }

    module->p2p_lock_received_ack -= 1;

    /* start all the requests */
    ompi_osc_pt2pt_flip_sendreqs(module);

    /* try to start all the requests.  We've copied everything we need
       out of pending_sendreqs, so don't need the lock here */
    out_count = opal_list_get_size(&(module->p2p_copy_pending_sendreqs));

    /* we want to send all the requests, plus we wait for one more
       completion event for the control message ack from the unlocker
       saying we're done */
    module->p2p_num_pending_out += (out_count + 1);
    OPAL_THREAD_UNLOCK(&module->p2p_lock);

    /* send the unlock request */
    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_output,
                         "%d: sending unlock request to %d with %d requests", 
                         ompi_comm_rank(module->p2p_comm), target,
                         out_count));
    ompi_osc_pt2pt_control_send(module, 
                                proc,
                                OMPI_OSC_PT2PT_HDR_UNLOCK_REQ,
                                ompi_comm_rank(module->p2p_comm),
                                out_count);

    while (NULL != 
           (item = opal_list_remove_first(&(module->p2p_copy_pending_sendreqs)))) {
        ompi_osc_pt2pt_sendreq_t *req = 
            (ompi_osc_pt2pt_sendreq_t*) item;

        ret = ompi_osc_pt2pt_sendreq_send(module, req);

        if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == ret ) {
            opal_output_verbose(5, ompi_osc_base_output,
                                "complete: failure in starting sendreq (%d).  Will try later.",
                                ret);
            opal_list_append(&(module->p2p_copy_pending_sendreqs), item);
        } else if (OMPI_SUCCESS != ret) {
            return ret;
        } 
    }

    /* wait for all the requests */
    OPAL_THREAD_LOCK(&module->p2p_lock);
    while (0 != module->p2p_num_pending_out) {
        opal_condition_wait(&module->p2p_cond, &module->p2p_lock);
    }
    OPAL_THREAD_UNLOCK(&module->p2p_lock);

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_output,
                         "%d: finished unlock to %d",
                         ompi_comm_rank(module->p2p_comm), target));

    /* set our mode on the window */
    ompi_win_remove_mode(win, OMPI_WIN_ACCESS_EPOCH | OMPI_WIN_LOCK_ACCESS);

    return OMPI_SUCCESS;
}


int
ompi_osc_pt2pt_passive_lock(ompi_osc_pt2pt_module_t *module,
                            int32_t origin,
                            int32_t lock_type)
{
    bool send_ack = false;
    int ret = OMPI_SUCCESS;
    ompi_proc_t *proc = ompi_comm_peer_lookup( module->p2p_comm, origin );
    ompi_osc_pt2pt_pending_lock_t *new_pending;

    OPAL_THREAD_LOCK(&(module->p2p_lock));
    if (lock_type == MPI_LOCK_EXCLUSIVE) {
        if (module->p2p_lock_status == 0) {
            module->p2p_lock_status = MPI_LOCK_EXCLUSIVE;
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_output,
                                 "%d: setting lock status to EXCLUSIVE (from %d)",
                                 ompi_comm_rank(module->p2p_comm), origin));
            ompi_win_append_mode(module->p2p_win, OMPI_WIN_EXPOSE_EPOCH);
            send_ack = true;
        } else {
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_output,
                                 "%d: queuing lock request from %d (type=%d)", 
                                 ompi_comm_rank(module->p2p_comm), origin, lock_type));
            new_pending = OBJ_NEW(ompi_osc_pt2pt_pending_lock_t);
            new_pending->proc = proc;
            new_pending->lock_type = lock_type;
            opal_list_append(&(module->p2p_locks_pending), &(new_pending->super));
        }
    } else if (lock_type == MPI_LOCK_SHARED) {
        if (module->p2p_lock_status != MPI_LOCK_EXCLUSIVE) {
            module->p2p_lock_status = MPI_LOCK_SHARED;
            module->p2p_shared_count++;
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_output,
                                 "%d: setting lock status to SHARED (from %d), count %d",
                                 ompi_comm_rank(module->p2p_comm), origin, module->p2p_shared_count));
            ompi_win_append_mode(module->p2p_win, OMPI_WIN_EXPOSE_EPOCH);
            send_ack = true;
        } else {
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_output,
                                 "%d: queuing lock request from %d (type=%d)", 
                                 ompi_comm_rank(module->p2p_comm), origin, lock_type));
            new_pending = OBJ_NEW(ompi_osc_pt2pt_pending_lock_t);
            new_pending->proc = proc;
            new_pending->lock_type = lock_type;
            opal_list_append(&(module->p2p_locks_pending), &(new_pending->super));
        }
    } else {
        ret = OMPI_ERROR;
    }
    OPAL_THREAD_UNLOCK(&(module->p2p_lock));

    if (send_ack) {
        OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_output,
                             "%d: sending lock ack to %d", 
                             ompi_comm_rank(module->p2p_comm), origin));
        ompi_osc_pt2pt_control_send(module, proc,
                                    OMPI_OSC_PT2PT_HDR_LOCK_REQ,
                                    ompi_comm_rank(module->p2p_comm),
                                    OMPI_SUCCESS);
    }

    return OMPI_SUCCESS;
}


int
ompi_osc_pt2pt_passive_unlock(ompi_osc_pt2pt_module_t *module,
                              int32_t origin,
                              int32_t count)
{
    ompi_proc_t *proc = ompi_comm_peer_lookup( module->p2p_comm, origin );
    ompi_osc_pt2pt_pending_lock_t *new_pending = NULL;

    assert(module->p2p_lock_status != 0);

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_output,
                         "%d: received unlock request from %d with %d requests\n",
                         ompi_comm_rank(module->p2p_comm),
                         origin, count));

    new_pending = OBJ_NEW(ompi_osc_pt2pt_pending_lock_t);
    new_pending->proc = proc;
    new_pending->lock_type = 0;
    OPAL_THREAD_LOCK(&(module->p2p_lock));
    module->p2p_num_pending_in += count;
    opal_list_append(&module->p2p_unlocks_pending, &(new_pending->super));
    OPAL_THREAD_UNLOCK(&(module->p2p_lock));

    return ompi_osc_pt2pt_passive_unlock_complete(module);
}


int
ompi_osc_pt2pt_passive_unlock_complete(ompi_osc_pt2pt_module_t *module)
{
    ompi_osc_pt2pt_pending_lock_t *new_pending = NULL;
    opal_list_t copy_unlock_acks;

    if (module->p2p_num_pending_in != 0) return OMPI_SUCCESS;

    OPAL_THREAD_LOCK(&(module->p2p_lock)); 
    if (module->p2p_num_pending_in != 0) {
        OPAL_THREAD_UNLOCK(&module->p2p_lock);
        return OMPI_SUCCESS;
    }

    if (module->p2p_lock_status == MPI_LOCK_EXCLUSIVE) {
        ompi_win_remove_mode(module->p2p_win, OMPI_WIN_EXPOSE_EPOCH);
        module->p2p_lock_status = 0;
    } else {
        module->p2p_shared_count -= opal_list_get_size(&module->p2p_unlocks_pending);
        OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_output,
                             "%d: decrementing shared count to %d",
                             ompi_comm_rank(module->p2p_comm), 
                             module->p2p_shared_count));
        if (module->p2p_shared_count == 0) {
            ompi_win_remove_mode(module->p2p_win, OMPI_WIN_EXPOSE_EPOCH);
            module->p2p_lock_status = 0;
        }
    }

    OBJ_CONSTRUCT(&copy_unlock_acks, opal_list_t);
    /* copy over any unlocks that have been satisfied (possibly
       multiple if SHARED) */
    opal_list_join(&copy_unlock_acks,
                   opal_list_get_end(&copy_unlock_acks),
                   &module->p2p_unlocks_pending);
    OPAL_THREAD_UNLOCK(&module->p2p_lock);

    /* issue whichever unlock acks we should issue */
    while (NULL != (new_pending = (ompi_osc_pt2pt_pending_lock_t*)
                    opal_list_remove_first(&copy_unlock_acks))) {
        OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_output,
                             "%d: sending unlock ack to proc %d",
                             ompi_comm_rank(module->p2p_comm),
                             new_pending->proc->proc_name.vpid));
        ompi_osc_pt2pt_control_send(module,
                                   new_pending->proc,
                                   OMPI_OSC_PT2PT_HDR_UNLOCK_REPLY,
                                   OMPI_SUCCESS, OMPI_SUCCESS);
        OBJ_RELEASE(new_pending);
    }

    OBJ_DESTRUCT(&copy_unlock_acks);

    /* if we were really unlocked, see if we have another lock request
       we can satisfy */
    OPAL_THREAD_LOCK(&module->p2p_lock);
    if (0 == module->p2p_lock_status) {
        new_pending = (ompi_osc_pt2pt_pending_lock_t*) 
            opal_list_remove_first(&(module->p2p_locks_pending));
        if (NULL != new_pending) {
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_output,
                                 "%d: sending lock ack to proc %d",
                                 ompi_comm_rank(module->p2p_comm),
                                 new_pending->proc->proc_name.vpid));
            ompi_win_append_mode(module->p2p_win, OMPI_WIN_EXPOSE_EPOCH);
            /* set lock state and generate a lock request */
            module->p2p_lock_status = new_pending->lock_type;
            if (MPI_LOCK_SHARED == new_pending->lock_type) {
                module->p2p_shared_count++;
            }
        }
    } else {
        new_pending = NULL;
    }
    OPAL_THREAD_UNLOCK(&(module->p2p_lock));

    if (NULL != new_pending) {
        ompi_osc_pt2pt_control_send(module,
                                    new_pending->proc,
                                    OMPI_OSC_PT2PT_HDR_LOCK_REQ,
                                    ompi_comm_rank(module->p2p_comm),
                                    OMPI_SUCCESS);
        OBJ_RELEASE(new_pending);
    }

    return OMPI_SUCCESS;
}
