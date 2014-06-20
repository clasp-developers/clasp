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
 * Copyright (c) 2009      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "osc_rdma.h"
#include "osc_rdma_sendreq.h"
#include "osc_rdma_longreq.h"
#include "osc_rdma_header.h"
#include "osc_rdma_data_move.h"

#include "mpi.h"
#include "opal/runtime/opal_progress.h"
#include "opal/threads/mutex.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/osc/base/base.h"


/* Must hold module's lock before calling... */
static inline void
ompi_osc_rdma_flip_sendreqs(ompi_osc_rdma_module_t *module)
{
    unsigned int *tmp;

    tmp = module->m_copy_num_pending_sendreqs;
    module->m_copy_num_pending_sendreqs = 
        module->m_num_pending_sendreqs;
    module->m_num_pending_sendreqs = tmp;
    memset(module->m_num_pending_sendreqs, 0,
           sizeof(unsigned int) * ompi_comm_size(module->m_comm));

    /* Copy in all the pending requests */
    opal_list_join(&module->m_copy_pending_sendreqs,
                   opal_list_get_end(&module->m_copy_pending_sendreqs),
                   &module->m_pending_sendreqs);
}


int
ompi_osc_rdma_module_fence(int assert, ompi_win_t *win)
{
    unsigned int incoming_reqs;
    int ret = OMPI_SUCCESS, i, len, started_send;
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    int num_outgoing = 0;

    if (0 != (assert & MPI_MODE_NOPRECEDE)) {
        /* check that the user didn't lie to us - since NOPRECEDED
           must be specified by all processes if it is specified by
           any process, if we see this it is safe to assume that there
           are no pending operations anywhere needed to close out this
           epoch. */
        if (0 != opal_list_get_size(&(module->m_pending_sendreqs))) {
            return MPI_ERR_RMA_SYNC;
        }

    } else {
        /* "atomically" copy all the data we're going to be modifying
           into the copy... */
        OPAL_THREAD_LOCK(&module->m_lock);
        ompi_osc_rdma_flip_sendreqs(module);
        OPAL_THREAD_UNLOCK(&module->m_lock);

        num_outgoing = opal_list_get_size(&(module->m_copy_pending_sendreqs));

        /* find out how much data everyone is going to send us.  Need
           to have the lock during this period so that we have a sane
           view of the number of sendreqs */
        ret = module->m_comm->
            c_coll.coll_reduce_scatter(module->m_copy_num_pending_sendreqs,
                                       &incoming_reqs,
                                       module->m_fence_coll_counts,
                                       MPI_UNSIGNED,
                                       MPI_SUM,
                                       module->m_comm,
                                       module->m_comm->c_coll.coll_reduce_scatter_module);

        if (OMPI_SUCCESS != ret) {
            /* put the stupid data back for the user.  This is not
               cheap, but the user lost his data if we don't. */
            OPAL_THREAD_LOCK(&(module->m_lock));
            opal_list_join(&module->m_pending_sendreqs,
                           opal_list_get_end(&module->m_pending_sendreqs),
                           &module->m_copy_pending_sendreqs);
            
            for (i = 0 ; i < ompi_comm_size(module->m_comm) ; ++i) {
                module->m_num_pending_sendreqs[i] +=
                    module->m_copy_num_pending_sendreqs[i];
            }

            OPAL_THREAD_UNLOCK(&(module->m_lock));
            return ret;
        }

        /* try to start all the requests.  We've copied everything we
           need out of pending_sendreqs, so don't need the lock
           here */
        len = opal_list_get_size(&(module->m_copy_pending_sendreqs));
        started_send = 0;
        OPAL_OUTPUT_VERBOSE((40, ompi_osc_base_output,
                             "fence: trying to start %d reqs",
                             len));
        for (i = 0 ; i < len ; ++i) {
            ompi_osc_rdma_sendreq_t *req = (ompi_osc_rdma_sendreq_t*)
                opal_list_remove_first(&(module->m_copy_pending_sendreqs));

            ret = ompi_osc_rdma_sendreq_send(module, req);
            if (OMPI_SUCCESS != ret) {
                opal_list_append(&(module->m_copy_pending_sendreqs), (opal_list_item_t*)req);
            } else {
                started_send = 1;
            }
        }

        /* we need to start at least one send, so that the callback
           will restart the rest. */
        while (0 == started_send && len != 0) {
            opal_progress();
            OPAL_OUTPUT_VERBOSE((40, ompi_osc_base_output,
                                 "fence: restarting %d reqs", len));
            len = opal_list_get_size(&(module->m_copy_pending_sendreqs));
            for (i = 0 ; i < len ; ++i) {
                ompi_osc_rdma_sendreq_t *req = (ompi_osc_rdma_sendreq_t*)
                    opal_list_remove_first(&(module->m_copy_pending_sendreqs));

                ret = ompi_osc_rdma_sendreq_send(module, req);
                if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == ret) {
                    opal_list_append(&(module->m_copy_pending_sendreqs), (opal_list_item_t*)req);
                } else if (OMPI_SUCCESS != ret) {
                    return ret;
                } else {
                    started_send = 1;
                }
            }
        }
        OPAL_OUTPUT_VERBOSE((40, ompi_osc_base_output,
                             "fence: done with initial start"));

        if (module->m_use_rdma) {
            if (module->m_rdma_wait_completion) {
                OPAL_THREAD_LOCK(&module->m_lock);
                while (module->m_rdma_num_pending != 0) {
                    opal_condition_wait(&module->m_cond, &module->m_lock);
                }
                OPAL_THREAD_UNLOCK(&module->m_lock);
            }

            for (i = 0 ; i < ompi_comm_size(module->m_comm) ; ++i) {
                int j;
                for (j = 0 ; j < module->m_peer_info[i].peer_num_btls ; ++j) {
                    if (module->m_peer_info[i].peer_btls[j].num_sent > 0) {
                        ret = ompi_osc_rdma_rdma_ack_send(module,
                                                          ompi_comm_peer_lookup(module->m_comm, i),
                                                          &(module->m_peer_info[i].peer_btls[j]));
                        if (OPAL_LIKELY(OMPI_SUCCESS == ret)) {
                            module->m_peer_info[i].peer_btls[j].num_sent = 0;
                        } else {
                            return ret;
                        }
                    }
                }
            }
        }

        ompi_osc_rdma_flush(module);

        OPAL_THREAD_LOCK(&module->m_lock);
        /* if some requests couldn't be started, push into the
           "queued" list, where we will try to restart them later. */
        if (opal_list_get_size(&module->m_copy_pending_sendreqs)) {
                opal_list_join(&module->m_queued_sendreqs,
                               opal_list_get_end(&module->m_queued_sendreqs),
                               &module->m_copy_pending_sendreqs);
        }

        /* possible we've already received a couple in messages, so
           atomicall add however many we're going to wait for */
        module->m_num_pending_in += incoming_reqs;
        module->m_num_pending_out += num_outgoing;

        OPAL_OUTPUT_VERBOSE((40, ompi_osc_base_output,
                             "fence: waiting on %d in and %d out, now %d, %d",
                             incoming_reqs,
                             num_outgoing,
                             module->m_num_pending_in,
                             module->m_num_pending_out));

        /* now we know how many things we're waiting for - wait for them... */
        while (module->m_num_pending_in > 0 ||
               0 != module->m_num_pending_out) {
            opal_condition_wait(&module->m_cond, &module->m_lock);
        }
        OPAL_THREAD_UNLOCK(&module->m_lock);
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
ompi_osc_rdma_module_start(ompi_group_t *group,
                            int assert,
                            ompi_win_t *win)
{
    int i, ret = OMPI_SUCCESS;
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    int32_t count;

    OBJ_RETAIN(group);
    ompi_group_increment_proc_count(group);

    module->m_eager_send_active = false;

    OPAL_THREAD_LOCK(&module->m_lock);

    if (NULL != module->m_sc_group) {
        OPAL_THREAD_UNLOCK(&module->m_lock);
        ret = MPI_ERR_RMA_SYNC;
        goto clean;
    }
    module->m_sc_group = group;    

    /* possible we've already received a couple in messages, so
       add however many we're going to wait for */
    count = (module->m_num_post_msgs += ompi_group_size(module->m_sc_group));
    OPAL_THREAD_UNLOCK(&(module->m_lock));

    memset(module->m_sc_remote_active_ranks, 0,
           sizeof(bool) * ompi_comm_size(module->m_comm));

    /* for each process in the specified group, find it's rank in our
       communicator, store those indexes, and set the true / false in
       the active ranks table */
    for (i = 0 ; i < ompi_group_size(group) ; i++) {
        int comm_rank = -1, j;
        
        /* find the rank in the communicator associated with this windows */
        for (j = 0 ; j < ompi_comm_size(module->m_comm) ; ++j) {
            if (ompi_group_peer_lookup(module->m_sc_group, i) ==
                ompi_comm_peer_lookup(module->m_comm, j)) {
                comm_rank = j;
                break;
            }
        }
        if (comm_rank == -1) {
            ret = MPI_ERR_RMA_SYNC;
            goto clean;
        }

        module->m_sc_remote_active_ranks[comm_rank] = true;
        module->m_sc_remote_ranks[i] = comm_rank;
    }

    /* Set our mode to access w/ start */
    ompi_win_remove_mode(win, OMPI_WIN_FENCE);
    ompi_win_append_mode(win, OMPI_WIN_ACCESS_EPOCH | OMPI_WIN_STARTED);

    if (count == 0) {
        module->m_eager_send_active = module->m_eager_send_ok;
    }

    return OMPI_SUCCESS;

 clean:
    ompi_group_decrement_proc_count(group);
    OBJ_RELEASE(group);
    return ret;
}


int
ompi_osc_rdma_module_complete(ompi_win_t *win)
{
    int i, j;
    int ret = OMPI_SUCCESS;
    ompi_group_t *group;
    opal_list_item_t *item;
    ompi_osc_rdma_module_t *module = GET_MODULE(win);

    /* wait for all the post messages */
    OPAL_THREAD_LOCK(&module->m_lock);
    while (0 != module->m_num_post_msgs) {
        opal_condition_wait(&module->m_cond, &module->m_lock);
    }

    ompi_osc_rdma_flip_sendreqs(module);

    /* for each process in group, send a control message with number
       of updates coming, then start all the requests */
    module->m_num_pending_out += 
        (int32_t) opal_list_get_size(&module->m_copy_pending_sendreqs);
    OPAL_THREAD_UNLOCK(&module->m_lock);

    for (i = 0 ; i < ompi_group_size(module->m_sc_group) ; ++i) {
        int comm_rank = module->m_sc_remote_ranks[i];
        if (module->m_use_rdma) {
            if (module->m_rdma_wait_completion) {
                OPAL_THREAD_LOCK(&module->m_lock);
                while (module->m_rdma_num_pending != 0) {
                    opal_condition_wait(&module->m_cond, &module->m_lock);
                }
                OPAL_THREAD_UNLOCK(&module->m_lock);
            }

            for (j = 0 ; j < module->m_peer_info[comm_rank].peer_num_btls ; ++j) {
                if (module->m_peer_info[comm_rank].peer_btls[j].num_sent > 0) {
                    ret = ompi_osc_rdma_rdma_ack_send(module,
                                                      ompi_group_peer_lookup(module->m_sc_group, i),
                                                      &(module->m_peer_info[comm_rank].peer_btls[j]));
                    if (OPAL_LIKELY(OMPI_SUCCESS == ret)) {
                        module->m_peer_info[comm_rank].peer_btls[j].num_sent = 0;
                    } else {
                        return ret;
                    }
                }
            }
        }
        ret = ompi_osc_rdma_control_send(module, 
                                         ompi_group_peer_lookup(module->m_sc_group, i),
                                         OMPI_OSC_RDMA_HDR_COMPLETE,
                                         module->m_copy_num_pending_sendreqs[comm_rank],
                                         0);
        assert(ret == OMPI_SUCCESS);
    }

    /* try to start all the requests.  We've copied everything we
       need out of pending_sendreqs, so don't need the lock
       here */
    while (NULL != 
           (item = opal_list_remove_first(&(module->m_copy_pending_sendreqs)))) {
        ompi_osc_rdma_sendreq_t *req = 
            (ompi_osc_rdma_sendreq_t*) item;

        ret = ompi_osc_rdma_sendreq_send(module, req);
        if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == ret) {
            opal_list_append(&(module->m_copy_pending_sendreqs), item);
            break;
        } else if (OMPI_SUCCESS != ret) {
            return ret;
        }
    }

    ompi_osc_rdma_flush(module);

    OPAL_THREAD_LOCK(&module->m_lock);
    /* if some requests couldn't be started, push into the
       "queued" list, where we will try to restart them later. */
    if (opal_list_get_size(&module->m_copy_pending_sendreqs)) {
        opal_list_join(&module->m_queued_sendreqs,
                       opal_list_get_end(&module->m_queued_sendreqs),
                       &module->m_copy_pending_sendreqs);
    }

    /* wait for all the requests */
    while (0 != module->m_num_pending_out) {
        opal_condition_wait(&module->m_cond, &module->m_lock);
    }

    group = module->m_sc_group;
    module->m_sc_group = NULL;

    OPAL_THREAD_UNLOCK(&(module->m_lock));

    /* remove WIN_POSTED from our mode */
    ompi_win_remove_mode(win, OMPI_WIN_ACCESS_EPOCH | OMPI_WIN_STARTED);

    ompi_group_decrement_proc_count(group);
    OBJ_RELEASE(group);

    return ret;
}


int
ompi_osc_rdma_module_post(ompi_group_t *group,
                           int assert,
                           ompi_win_t *win)
{
    int i;
    ompi_osc_rdma_module_t *module = GET_MODULE(win);

    OBJ_RETAIN(group);
    ompi_group_increment_proc_count(group);

    OPAL_THREAD_LOCK(&(module->m_lock));
    assert(NULL == module->m_pw_group);
    module->m_pw_group = group;

    /* Set our mode to expose w/ post */
    ompi_win_remove_mode(win, OMPI_WIN_FENCE);
    ompi_win_append_mode(win, OMPI_WIN_EXPOSE_EPOCH | OMPI_WIN_POSTED);

    /* list how many complete counters we're still waiting on */
    module->m_num_complete_msgs +=
        ompi_group_size(module->m_pw_group);
    OPAL_THREAD_UNLOCK(&(module->m_lock));

    /* send a hello counter to everyone in group */
    for (i = 0 ; i < ompi_group_size(module->m_pw_group) ; ++i) {
        ompi_osc_rdma_control_send(module, 
                                   ompi_group_peer_lookup(group, i),
                                   OMPI_OSC_RDMA_HDR_POST, 1, 0);
    }    

    return OMPI_SUCCESS;
}


int
ompi_osc_rdma_module_wait(ompi_win_t *win)
{
    ompi_group_t *group;
    ompi_osc_rdma_module_t *module = GET_MODULE(win);

    OPAL_THREAD_LOCK(&module->m_lock);
    while (0 != (module->m_num_pending_in) ||
           0 != (module->m_num_complete_msgs)) {
        opal_condition_wait(&module->m_cond, &module->m_lock);
    }

    group = module->m_pw_group;
    module->m_pw_group = NULL;
    OPAL_THREAD_UNLOCK(&module->m_lock);

    ompi_win_remove_mode(win, OMPI_WIN_EXPOSE_EPOCH | OMPI_WIN_POSTED);

    ompi_group_decrement_proc_count(group);
    OBJ_RELEASE(group);

    return OMPI_SUCCESS;
}


int 
ompi_osc_rdma_module_test(ompi_win_t *win,
                           int *flag)
{
    ompi_group_t *group;
    ompi_osc_rdma_module_t *module = GET_MODULE(win);

#if !OPAL_ENABLE_PROGRESS_THREADS
    opal_progress();
#endif

    if (0 != (module->m_num_pending_in) ||
        0 != (module->m_num_complete_msgs)) {
        *flag = 0;
        return OMPI_SUCCESS;
    }

    *flag = 1;

    OPAL_THREAD_LOCK(&(module->m_lock));
    group = module->m_pw_group;
    module->m_pw_group = NULL;
    OPAL_THREAD_UNLOCK(&(module->m_lock));

    ompi_win_remove_mode(win, OMPI_WIN_EXPOSE_EPOCH | OMPI_WIN_POSTED);

    ompi_group_decrement_proc_count(group);
    OBJ_RELEASE(group);

    return OMPI_SUCCESS;
}


struct ompi_osc_rdma_pending_lock_t {
    opal_list_item_t super;
    ompi_proc_t *proc;
    int32_t lock_type;
};
typedef struct ompi_osc_rdma_pending_lock_t ompi_osc_rdma_pending_lock_t;
OBJ_CLASS_INSTANCE(ompi_osc_rdma_pending_lock_t, opal_list_item_t,
                   NULL, NULL);


int
ompi_osc_rdma_module_lock(int lock_type,
                           int target,
                           int assert,
                           ompi_win_t *win)
{
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    ompi_proc_t *proc = ompi_comm_peer_lookup( module->m_comm, target );

    assert(lock_type != 0);

    /* set our mode on the window */
    ompi_win_remove_mode(win, OMPI_WIN_FENCE);
    ompi_win_append_mode(win, OMPI_WIN_ACCESS_EPOCH | OMPI_WIN_LOCK_ACCESS);

    OPAL_OUTPUT_VERBOSE((40, ompi_osc_base_output,
                         "%d sending lock request to %d", 
                         ompi_comm_rank(module->m_comm), target));
    /* generate a lock request */
    ompi_osc_rdma_control_send(module, 
                               proc,
                               OMPI_OSC_RDMA_HDR_LOCK_REQ,
                               ompi_comm_rank(module->m_comm),
                               lock_type);

    module->m_eager_send_active = false;

    /* return */
    return OMPI_SUCCESS;
}


int
ompi_osc_rdma_module_unlock(int target,
                             ompi_win_t *win)
{
    int32_t out_count;
    opal_list_item_t *item;
    int ret;
    ompi_osc_rdma_module_t *module = GET_MODULE(win);
    ompi_proc_t *proc = ompi_comm_peer_lookup( module->m_comm, target );

    OPAL_THREAD_LOCK(&module->m_lock);
    while (0 == module->m_lock_received_ack) {
        opal_condition_wait(&module->m_cond, &module->m_lock);
    }

    module->m_lock_received_ack -= 1;

    /* start all the requests */
    ompi_osc_rdma_flip_sendreqs(module);

    /* try to start all the requests.  We've copied everything we need
       out of pending_sendreqs, so don't need the lock here */
    out_count = opal_list_get_size(&module->m_copy_pending_sendreqs);

    /* we want to send all the requests, plus we wait for one more
       completion event for the control message ack from the unlocker
       saying we're done */
    module->m_num_pending_out += (out_count + 1);
    OPAL_THREAD_UNLOCK(&module->m_lock);

    /* send the unlock request */
    OPAL_OUTPUT_VERBOSE((40, ompi_osc_base_output,
                         "%d sending unlock request to %d with %d requests", 
                         ompi_comm_rank(module->m_comm), target,
                         out_count));
    ompi_osc_rdma_control_send(module, 
                               proc,
                               OMPI_OSC_RDMA_HDR_UNLOCK_REQ,
                               ompi_comm_rank(module->m_comm),
                               out_count);

    /* try to start all the requests.  We've copied everything we
       need out of pending_sendreqs, so don't need the lock
       here */
    while (NULL != 
           (item = opal_list_remove_first(&(module->m_copy_pending_sendreqs)))) {
        ompi_osc_rdma_sendreq_t *req = 
            (ompi_osc_rdma_sendreq_t*) item;

        ret = ompi_osc_rdma_sendreq_send(module, req);
        if (OMPI_ERR_TEMP_OUT_OF_RESOURCE == ret) {
            opal_list_append(&(module->m_copy_pending_sendreqs), item);
            break;
        } else if (OMPI_SUCCESS != ret) {
            return ret;
        }
    }

    ompi_osc_rdma_flush(module);

    OPAL_THREAD_LOCK(&module->m_lock);
    /* if some requests couldn't be started, push into the
       "queued" list, where we will try to restart them later. */
    if (opal_list_get_size(&module->m_copy_pending_sendreqs)) {
        opal_list_join(&module->m_queued_sendreqs,
                       opal_list_get_end(&module->m_queued_sendreqs),
                       &module->m_copy_pending_sendreqs);
    }

    /* wait for all the requests */
    while (0 != module->m_num_pending_out) {
        opal_condition_wait(&module->m_cond, &module->m_lock);
    }
    OPAL_THREAD_UNLOCK(&module->m_lock);

    /* set our mode on the window */
    ompi_win_remove_mode(win, OMPI_WIN_ACCESS_EPOCH | OMPI_WIN_LOCK_ACCESS);

    module->m_eager_send_active = module->m_eager_send_ok;

    return OMPI_SUCCESS;
}


int
ompi_osc_rdma_passive_lock(ompi_osc_rdma_module_t *module,
                           int32_t origin,
                           int32_t lock_type)
{
    bool send_ack = false;
    int ret = OMPI_SUCCESS;
    ompi_proc_t *proc = ompi_comm_peer_lookup( module->m_comm, origin );
    ompi_osc_rdma_pending_lock_t *new_pending;

    OPAL_THREAD_LOCK(&(module->m_lock));
    if (lock_type == MPI_LOCK_EXCLUSIVE) {
        if (module->m_lock_status == 0) {
            module->m_lock_status = MPI_LOCK_EXCLUSIVE;
            ompi_win_append_mode(module->m_win, OMPI_WIN_EXPOSE_EPOCH);
            send_ack = true;
        } else {
            OPAL_OUTPUT_VERBOSE((40, ompi_osc_base_output,
                                 "%d queuing lock request from %d (%d)", 
                                 ompi_comm_rank(module->m_comm), 
                                 origin, lock_type));
            new_pending = OBJ_NEW(ompi_osc_rdma_pending_lock_t);
            new_pending->proc = proc;
            new_pending->lock_type = lock_type;
            opal_list_append(&(module->m_locks_pending), &(new_pending->super));
        }
    } else if (lock_type == MPI_LOCK_SHARED) {
        if (module->m_lock_status != MPI_LOCK_EXCLUSIVE) {
            module->m_lock_status = MPI_LOCK_SHARED;
            module->m_shared_count++;
            ompi_win_append_mode(module->m_win, OMPI_WIN_EXPOSE_EPOCH);
            send_ack = true;
        } else {
            OPAL_OUTPUT_VERBOSE((40, ompi_osc_base_output,
                                 "queuing lock request from %d (%d) lock_type:%d", 
                                 ompi_comm_rank(module->m_comm), 
                                 origin, lock_type));
            new_pending = OBJ_NEW(ompi_osc_rdma_pending_lock_t);
            new_pending->proc = proc;
            new_pending->lock_type = lock_type;
            opal_list_append(&(module->m_locks_pending), &(new_pending->super));
        }
    } else {
        ret = OMPI_ERROR;
    }
    OPAL_THREAD_UNLOCK(&(module->m_lock));

    if (send_ack) {
        OPAL_OUTPUT_VERBOSE((40, ompi_osc_base_output,
                             "%d sending lock ack to %d", 
                             ompi_comm_rank(module->m_comm), origin));
        ompi_osc_rdma_control_send(module, proc,
                                   OMPI_OSC_RDMA_HDR_LOCK_REQ,
                                   ompi_comm_rank(module->m_comm),
                                   OMPI_SUCCESS);
    }

    return OMPI_SUCCESS;
}


int
ompi_osc_rdma_passive_unlock(ompi_osc_rdma_module_t *module,
                             int32_t origin,
                             int32_t count)
{
    ompi_proc_t *proc = ompi_comm_peer_lookup( module->m_comm, origin );
    ompi_osc_rdma_pending_lock_t *new_pending = NULL;

    assert(module->m_lock_status != 0);

    OPAL_OUTPUT_VERBOSE((40, ompi_osc_base_output,
                         "received unlock request from %d with %d requests\n",
                         origin, count));

    new_pending = OBJ_NEW(ompi_osc_rdma_pending_lock_t);
    new_pending->proc = proc;
    new_pending->lock_type = 0;
    OPAL_THREAD_LOCK(&(module->m_lock));
    module->m_num_pending_in += count;
    opal_list_append(&module->m_unlocks_pending, &(new_pending->super));
    OPAL_THREAD_UNLOCK(&(module->m_lock));

    return ompi_osc_rdma_passive_unlock_complete(module);
}


int
ompi_osc_rdma_passive_unlock_complete(ompi_osc_rdma_module_t *module)
{
    ompi_osc_rdma_pending_lock_t *new_pending = NULL;
    opal_list_t copy_unlock_acks;

    if (module->m_num_pending_in != 0) return OMPI_SUCCESS;

    OPAL_THREAD_LOCK(&module->m_lock);
    if (module->m_num_pending_in != 0) {
        OPAL_THREAD_UNLOCK(&module->m_lock);
        return OMPI_SUCCESS;
    }

    if (module->m_lock_status == MPI_LOCK_EXCLUSIVE) {
        ompi_win_remove_mode(module->m_win, OMPI_WIN_EXPOSE_EPOCH);
        module->m_lock_status = 0;
    } else {
        module->m_shared_count -= opal_list_get_size(&module->m_unlocks_pending);
        if (module->m_shared_count == 0) {
            ompi_win_remove_mode(module->m_win, OMPI_WIN_EXPOSE_EPOCH);
            module->m_lock_status = 0;
        }
    }

    OBJ_CONSTRUCT(&copy_unlock_acks, opal_list_t);
    /* copy over any unlocks that have been satisfied (possibly
       multiple if SHARED) */
    opal_list_join(&copy_unlock_acks,
                   opal_list_get_end(&copy_unlock_acks),
                   &module->m_unlocks_pending);
    OPAL_THREAD_UNLOCK(&module->m_lock);

    /* issue whichever unlock acks we should issue */
    while (NULL != (new_pending = (ompi_osc_rdma_pending_lock_t*)
                    opal_list_remove_first(&copy_unlock_acks))) {
        OPAL_OUTPUT_VERBOSE((40, ompi_osc_base_output,
                             "sending unlock reply to proc"));
        ompi_osc_rdma_control_send(module,
                                   new_pending->proc,
                                   OMPI_OSC_RDMA_HDR_UNLOCK_REPLY,
                                   OMPI_SUCCESS, OMPI_SUCCESS);
        OBJ_RELEASE(new_pending);
    }

    OBJ_DESTRUCT(&copy_unlock_acks);

    /* if we were really unlocked, see if we have another lock request
       we can satisfy */
    OPAL_THREAD_LOCK(&(module->m_lock));
    if (0 == module->m_lock_status) {
        new_pending = (ompi_osc_rdma_pending_lock_t*) 
            opal_list_remove_first(&(module->m_locks_pending));
        if (NULL != new_pending) {
            ompi_win_append_mode(module->m_win, OMPI_WIN_EXPOSE_EPOCH);
            /* set lock state and generate a lock request */
            module->m_lock_status = new_pending->lock_type;
            if (MPI_LOCK_SHARED == new_pending->lock_type) {
                module->m_shared_count++;
            }
        }
    } else {
        new_pending = NULL;
    }
    OPAL_THREAD_UNLOCK(&(module->m_lock));

    if (NULL != new_pending) {
        OPAL_OUTPUT_VERBOSE((40, ompi_osc_base_output,
                             "sending lock request to proc"));
        ompi_osc_rdma_control_send(module,
                                   new_pending->proc,
                                   OMPI_OSC_RDMA_HDR_LOCK_REQ,
                                   ompi_comm_rank(module->m_comm),
                                   OMPI_SUCCESS);
        OBJ_RELEASE(new_pending);
    }

    return OMPI_SUCCESS;
}
