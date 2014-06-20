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

#ifndef OMPI_OSC_RDMA_H
#define OMPI_OSC_RDMA_H

#include "ompi_config.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_free_list.h"
#include "opal/class/opal_hash_table.h"
#include "opal/threads/threads.h"

#include "ompi/win/win.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/osc/osc.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/bml/bml.h"

BEGIN_C_DECLS

struct ompi_osc_rdma_buffer_t {
    mca_btl_base_descriptor_t* descriptor;
    size_t remain_len;
    mca_bml_base_btl_t *bml_btl;
};
typedef struct ompi_osc_rdma_buffer_t ompi_osc_rdma_buffer_t;

struct ompi_osc_rdma_component_t {
    /** Extend the basic osc component interface */
    ompi_osc_base_component_t super;

    /** store the state of progress threads for this instance of OMPI */
    bool c_have_progress_threads;

    /** lock access to datastructures in the component structure */
    opal_mutex_t c_lock;

    /** List of ompi_osc_rdma_module_ts currently in existance.
        Needed so that received fragments can be dispatched to the
        correct module */
    opal_hash_table_t c_modules;

    /** Lock for request management */
    opal_mutex_t c_request_lock;

    /** Condition variable for request management */
    opal_condition_t c_request_cond;
 
    /** free list of ompi_osc_rdma_sendreq_t structures */
    opal_free_list_t c_sendreqs;
    /** free list of ompi_osc_rdma_replyreq_t structures */
    opal_free_list_t c_replyreqs;
    /** free list of ompi_osc_rdma_longreq_t structures */
    opal_free_list_t c_longreqs;

    /** list of outstanding requests, of type ompi_osc_pt2pt_longreq_t */
    opal_list_t c_pending_requests;

#if OPAL_ENABLE_PROGRESS_THREADS
    opal_thread_t c_thread;
    bool c_thread_run;
#endif

    bool c_btl_registered;

    uint32_t c_sequence_number;
};
typedef struct ompi_osc_rdma_component_t ompi_osc_rdma_component_t;


struct ompi_osc_rdma_btl_t {
    uint64_t peer_seg_key;
    mca_bml_base_btl_t *bml_btl;
    int      rdma_order;
    int32_t  num_sent;
};
typedef struct ompi_osc_rdma_btl_t ompi_osc_rdma_btl_t;


struct ompi_osc_rdma_peer_info_t {
    uint64_t             peer_base;
    uint64_t             peer_len;

    int                  peer_num_btls;
    volatile int         peer_index_btls;
    ompi_osc_rdma_btl_t *peer_btls;

    int                             local_num_btls;
    mca_bml_base_btl_t            **local_btls;
    mca_mpool_base_registration_t **local_registrations;
    mca_btl_base_descriptor_t     **local_descriptors;
};
typedef struct ompi_osc_rdma_peer_info_t ompi_osc_rdma_peer_info_t;


struct ompi_osc_rdma_setup_info_t {
    volatile int32_t num_btls_callin;
    int32_t num_btls_expected;
    volatile int32_t num_btls_outgoing;
    opal_list_t *outstanding_btl_requests;
};
typedef struct ompi_osc_rdma_setup_info_t ompi_osc_rdma_setup_info_t;


struct ompi_osc_rdma_module_t {
    /** Extend the basic osc module interface */
    ompi_osc_base_module_t super;

    uint32_t m_sequence_number;

    /** lock access to data structures in the current module */
    opal_mutex_t m_lock;

    /** condition variable for access to current module */
    opal_condition_t m_cond;

    /** lock for "atomic" window updates from reductions */
    opal_mutex_t m_acc_lock;

    /** pointer back to window */
    ompi_win_t *m_win;

    /** communicator created with this window */
    ompi_communicator_t *m_comm;

    /** list of ompi_osc_rdma_sendreq_t structures, and includes all
        requests for this access epoch that have not already been
        started.  m_lock must be held when modifying this field. */
    opal_list_t m_pending_sendreqs;

    /** list of unsigned int counters for the number of requests to a
        particular rank in m_comm for this access epoc.  m_lock
        must be held when modifying this field */
    unsigned int *m_num_pending_sendreqs;

    /** For MPI_Fence synchronization, the number of messages to send
        in epoch.  For Start/Complete, the number of updates for this
        Complete.  For lock, the number of
        messages waiting for completion on on the origin side.  Not
        protected by m_lock - must use atomic counter operations. */
    int32_t m_num_pending_out;

    /** For MPI_Fence synchronization, the number of expected incoming
        messages.  For Post/Wait, the number of expected updates from
        complete. For lock, the number of messages on the passive side
        we are waiting for.  Not protected by m_lock - must use
        atomic counter operations. */
    int32_t m_num_pending_in;

    /** Number of "ping" messages from the remote post group we've
        received */
    int32_t m_num_post_msgs;

    /** Number of "count" messages from the remote complete group
        we've received */
    int32_t m_num_complete_msgs;

    /** cyclic counter for a unique tage for long messages.  Not
        protected by the m_lock - must use create_send_tag() to
        create a send tag */
    volatile int32_t m_tag_counter;

    opal_list_t m_copy_pending_sendreqs;
    unsigned int *m_copy_num_pending_sendreqs;

    opal_list_t m_queued_sendreqs;

    /** start sending data eagerly */
    bool m_eager_send_active;
    bool m_eager_send_ok;

    /* RDMA data */
    bool m_use_rdma;
    bool m_rdma_wait_completion;
    ompi_osc_rdma_setup_info_t *m_setup_info;
    ompi_osc_rdma_peer_info_t *m_peer_info;
    int32_t m_rdma_num_pending;

     /*** buffering ***/
     bool m_use_buffers;
     ompi_osc_rdma_buffer_t *m_pending_buffers;

    /* ********************* FENCE data ************************ */
    /* an array of <sizeof(m_comm)> ints, each containing the value
       1. */
    int *m_fence_coll_counts;

    /* ********************* PWSC data ************************ */
    struct ompi_group_t *m_pw_group;
    struct ompi_group_t *m_sc_group;
    bool *m_sc_remote_active_ranks;
    int *m_sc_remote_ranks;

    /* ********************* LOCK data ************************ */
    int32_t m_lock_status; /* one of 0, MPI_LOCK_EXCLUSIVE, MPI_LOCK_SHARED */
    int32_t m_shared_count;
    opal_list_t m_locks_pending;
    opal_list_t m_unlocks_pending;
    int32_t m_lock_received_ack;
};
typedef struct ompi_osc_rdma_module_t ompi_osc_rdma_module_t;
OMPI_MODULE_DECLSPEC extern ompi_osc_rdma_component_t mca_osc_rdma_component;


#define GET_MODULE(win) ((ompi_osc_rdma_module_t*) win->w_osc_module)

/*
 * Component functions 
 */

int ompi_osc_rdma_component_init(bool enable_progress_threads,
                                 bool enable_mpi_threads);

int ompi_osc_rdma_component_finalize(void);

int ompi_osc_rdma_component_query(struct ompi_win_t *win,
                                  struct ompi_info_t *info,
                                  struct ompi_communicator_t *comm);

int ompi_osc_rdma_component_select(struct ompi_win_t *win,
                                   struct ompi_info_t *info,
                                   struct ompi_communicator_t *comm);

int ompi_osc_rdma_component_progress(void);

int ompi_osc_rdma_peer_info_free(ompi_osc_rdma_peer_info_t *peer_info);

/*
 * Module interface function types 
 */
int ompi_osc_rdma_module_free(struct ompi_win_t *win);

int ompi_osc_rdma_module_put(void *origin_addr,
                             int origin_count,
                             struct ompi_datatype_t *origin_dt,
                             int target,
                             OPAL_PTRDIFF_TYPE target_disp,
                             int target_count,
                             struct ompi_datatype_t *target_dt,
                             struct ompi_win_t *win);

int ompi_osc_rdma_module_accumulate(void *origin_addr,
                                    int origin_count,
                                    struct ompi_datatype_t *origin_dt,
                                    int target,
                                    OPAL_PTRDIFF_TYPE target_disp,
                                    int target_count,
                                    struct ompi_datatype_t *target_dt,
                                    struct ompi_op_t *op,
                                    struct ompi_win_t *win);

int ompi_osc_rdma_module_get(void *origin_addr,
                             int origin_count,
                             struct ompi_datatype_t *origin_dt,
                             int target,
                             OPAL_PTRDIFF_TYPE target_disp,
                             int target_count,
                             struct ompi_datatype_t *target_dt,
                             struct ompi_win_t *win);

int ompi_osc_rdma_module_fence(int assert, struct ompi_win_t *win);

int ompi_osc_rdma_module_start(struct ompi_group_t *group,
                               int assert,
                               struct ompi_win_t *win);
int ompi_osc_rdma_module_complete(struct ompi_win_t *win);

int ompi_osc_rdma_module_post(struct ompi_group_t *group,
                              int assert,
                              struct ompi_win_t *win);

int ompi_osc_rdma_module_wait(struct ompi_win_t *win);

int ompi_osc_rdma_module_test(struct ompi_win_t *win,
                              int *flag);

int ompi_osc_rdma_module_lock(int lock_type,
                              int target,
                              int assert,
                              struct ompi_win_t *win);

int ompi_osc_rdma_module_unlock(int target,
                                struct ompi_win_t *win);

/*
 * passive side sync interface functions
 */
int ompi_osc_rdma_passive_lock(ompi_osc_rdma_module_t *module,
                                int32_t origin,
                                int32_t lock_type);

int ompi_osc_rdma_passive_unlock(ompi_osc_rdma_module_t *module,
                                  int32_t origin,
                                  int32_t count);

int ompi_osc_rdma_passive_unlock_complete(ompi_osc_rdma_module_t *module);


END_C_DECLS

#endif /* OMPI_OSC_RDMA_H */
