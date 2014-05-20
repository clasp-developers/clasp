/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2006 The Trustees of the University of Tennessee.
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

#ifndef OMPI_OSC_PT2PT_H
#define OMPI_OSC_PT2PT_H

#include "ompi_config.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_free_list.h"
#include "opal/class/opal_hash_table.h"
#include "opal/threads/threads.h"

#include "ompi/win/win.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/osc/osc.h"

BEGIN_C_DECLS

#define CONTROL_MSG_TAG (-200)

struct ompi_osc_pt2pt_component_t {
    /** Extend the basic osc component interface */
    ompi_osc_base_component_t super;

    /** store the state of progress threads for this instance of OMPI */
    bool p2p_c_have_progress_threads;

    /** lock access to datastructures in the component structure */ 
    opal_mutex_t p2p_c_lock; 

    /** List of active modules (windows) */
    opal_hash_table_t p2p_c_modules; 

    /** max size of eager message */
    size_t p2p_c_eager_size;

    /** Lock for request management */
    opal_mutex_t p2p_c_request_lock;

    /** Condition variable for request management */
    opal_condition_t p2p_c_request_cond;

    /** free list of ompi_osc_pt2pt_sendreq_t structures */
    opal_free_list_t p2p_c_sendreqs;
    /** free list of ompi_osc_pt2pt_replyreq_t structures */
    opal_free_list_t p2p_c_replyreqs;
    /** free list of ompi_osc_pt2pt_longreq_t structures */
    opal_free_list_t p2p_c_longreqs;
    /** free list for eager / control meessages */
    opal_free_list_t p2p_c_buffers;

    /** list of outstanding requests, of type ompi_osc_pt2pt_mpireq_t */
    opal_list_t p2p_c_pending_requests;

#if OPAL_ENABLE_PROGRESS_THREADS
    opal_thread_t p2p_c_thread;
    bool p2p_c_thread_run;
#endif
};
typedef struct ompi_osc_pt2pt_component_t ompi_osc_pt2pt_component_t;


struct ompi_osc_pt2pt_module_t {
    /** Extend the basic osc module interface */
    ompi_osc_base_module_t super; 

    /** lock access to data structures in the current module */
    opal_mutex_t p2p_lock;

    /** condition variable for access to current module */
    opal_condition_t p2p_cond;

    /** lock for "atomic" window updates from reductions */
    opal_mutex_t p2p_acc_lock;

    /** pointer back to window */
    ompi_win_t *p2p_win;

    /** communicator created with this window */
    ompi_communicator_t *p2p_comm;

    /** list of ompi_osc_pt2pt_sendreq_t structures, and includes all
        requests for this access epoch that have not already been
        started.  p2p_lock must be held when modifying this field. */
    opal_list_t p2p_pending_sendreqs;

    /** list of unsigned int counters for the number of requests to a
        particular rank in p2p_comm for this access epoc.  p2p_lock
        must be held when modifying this field */
    unsigned int *p2p_num_pending_sendreqs;

    /** For MPI_Fence synchronization, the number of messages to send
        in epoch.  For Start/Complete, the number of updates for this
        Complete.  For lock, the number of
        messages waiting for completion on on the origin side.  Not
        protected by p2p_lock - must use atomic counter operations. */
    int32_t p2p_num_pending_out;

    /** For MPI_Fence synchronization, the number of expected incoming
        messages.  For Post/Wait, the number of expected updates from
        complete. For lock, the number of messages on the passive side
        we are waiting for.  Not protected by p2p_lock - must use
        atomic counter operations. */
    int32_t p2p_num_pending_in;

    /** Number of "ping" messages from the remote post group we've
        received */
    int32_t p2p_num_post_msgs;

    /** Number of "count" messages from the remote complete group
        we've received */
    int32_t p2p_num_complete_msgs;

    /** cyclic counter for a unique tag for long messages.  Not
        protected by the p2p_lock - must use create_send_tag() to
        create a send tag */
    volatile int32_t p2p_tag_counter;

    opal_list_t p2p_copy_pending_sendreqs;
    unsigned int *p2p_copy_num_pending_sendreqs;

    /* ********************* FENCE data ************************ */
    /* an array of <sizeof(p2p_comm)> ints, each containing the value
       1. */
    int *p2p_fence_coll_counts;

    /* ********************* PWSC data ************************ */
    struct ompi_group_t *p2p_pw_group;
    struct ompi_group_t *p2p_sc_group;
    bool *p2p_sc_remote_active_ranks;
    int *p2p_sc_remote_ranks;

    /* ********************* LOCK data ************************ */
    int32_t p2p_lock_status; /* one of 0, MPI_LOCK_EXCLUSIVE, MPI_LOCK_SHARED */
    int32_t p2p_shared_count;
    opal_list_t p2p_locks_pending;
    opal_list_t p2p_unlocks_pending;
    int32_t p2p_lock_received_ack;
};
typedef struct ompi_osc_pt2pt_module_t ompi_osc_pt2pt_module_t;
OMPI_MODULE_DECLSPEC extern ompi_osc_pt2pt_component_t mca_osc_pt2pt_component;


/**
 * Helper macro for grabbing the module structure from a window instance
 */
#define P2P_MODULE(win) ((ompi_osc_pt2pt_module_t*) win->w_osc_module)

/*
 * Component functions 
 */

int ompi_osc_pt2pt_component_init(bool enable_progress_threads,
                                  bool enable_mpi_threads);

int ompi_osc_pt2pt_component_finalize(void);

int ompi_osc_pt2pt_component_query(struct ompi_win_t *win,
                                  struct ompi_info_t *info,
                                  struct ompi_communicator_t *comm);

int ompi_osc_pt2pt_component_select(struct ompi_win_t *win,
                                   struct ompi_info_t *info,
                                   struct ompi_communicator_t *comm);

int ompi_osc_pt2pt_component_progress(void);

/*
 * Module interface function types 
 */
int ompi_osc_pt2pt_module_free(struct ompi_win_t *win);

int ompi_osc_pt2pt_module_put(void *origin_addr,
                             int origin_count,
                             struct ompi_datatype_t *origin_dt,
                             int target,
                             OPAL_PTRDIFF_TYPE target_disp,
                             int target_count,
                             struct ompi_datatype_t *target_dt,
                             struct ompi_win_t *win);

int ompi_osc_pt2pt_module_accumulate(void *origin_addr,
                                    int origin_count,
                                    struct ompi_datatype_t *origin_dt,
                                    int target,
                                    OPAL_PTRDIFF_TYPE target_disp,
                                    int target_count,
                                    struct ompi_datatype_t *target_dt,
                                    struct ompi_op_t *op,
                                    struct ompi_win_t *win);

int ompi_osc_pt2pt_module_get(void *origin_addr,
                             int origin_count,
                             struct ompi_datatype_t *origin_dt,
                             int target,
                             OPAL_PTRDIFF_TYPE target_disp,
                             int target_count,
                             struct ompi_datatype_t *target_dt,
                             struct ompi_win_t *win);

int ompi_osc_pt2pt_module_fence(int assert, struct ompi_win_t *win);

int ompi_osc_pt2pt_module_start(struct ompi_group_t *group,
                               int assert,
                               struct ompi_win_t *win);
int ompi_osc_pt2pt_module_complete(struct ompi_win_t *win);

int ompi_osc_pt2pt_module_post(struct ompi_group_t *group,
                              int assert,
                              struct ompi_win_t *win);

int ompi_osc_pt2pt_module_wait(struct ompi_win_t *win);

int ompi_osc_pt2pt_module_test(struct ompi_win_t *win,
                              int *flag);

int ompi_osc_pt2pt_module_lock(int lock_type,
                              int target,
                              int assert,
                              struct ompi_win_t *win);

int ompi_osc_pt2pt_module_unlock(int target,
                                struct ompi_win_t *win);

/*
 * passive side sync interface functions
 */
int ompi_osc_pt2pt_passive_lock(ompi_osc_pt2pt_module_t *module,
                                int32_t origin,
                                int32_t lock_type);

int ompi_osc_pt2pt_passive_unlock(ompi_osc_pt2pt_module_t *module,
                                  int32_t origin,
                                  int32_t count);

int ompi_osc_pt2pt_passive_unlock_complete(ompi_osc_pt2pt_module_t *module);

END_C_DECLS

#endif /* OMPI_OSC_PT2PT_H */
