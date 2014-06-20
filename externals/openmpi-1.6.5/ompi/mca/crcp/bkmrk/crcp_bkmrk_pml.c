/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2010      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/*
 *
 */
#include "ompi_config.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNIST_H */

#include "opal/runtime/opal_cr.h"
#include "opal/event/event.h"
#include "opal/util/output.h"

#include "opal/util/opal_environ.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/rml/rml.h"

#include "ompi/request/request.h"
#include "ompi/mca/dpm/dpm.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/mca/pml/base/pml_base_request.h"
#include "ompi/mca/crcp/crcp.h"
#include "ompi/mca/crcp/base/base.h"

#include "ompi/class/ompi_free_list.h"
#include "ompi/runtime/ompi_cr.h"

#include "crcp_bkmrk.h"
#include "crcp_bkmrk_pml.h"

/************************************
 * Locally Global vars
 ************************************/
#define PROBE_ANY_SIZE  ((size_t) 0)
#define PROBE_ANY_COUNT ((size_t) 0)

#define PERSIST_MARKER ((int) -1)

#define RECV_MATCH_RESP_DONE  0
#define RECV_MATCH_RESP_MORE  1
#define RECV_MATCH_RESP_ERROR 2

#define INVALID_INT -123456789

#define FIND_MSG_TRUE     0
#define FIND_MSG_FALSE    1
#define FIND_MSG_UNKNOWN  2

/* Pointers to the 'real' PML */
static mca_pml_base_component_t  *wrapped_pml_component = NULL;
static mca_pml_base_module_t     *wrapped_pml_module    = NULL;

/* A unique ID for each message signature in the system */
static uint64_t message_seq_num = 1;
static uint64_t content_ref_seq_num = 1;

/* The current message being worked on */
static uint64_t current_msg_id = 0;
static ompi_crcp_bkmrk_pml_message_type_t current_msg_type = 0;

/* If we need to stall the C/R coordination until the current
 * operation is complete */
static bool stall_for_completion;

/*
 * State of the ft_event
 */
static int ft_event_state = OPAL_CRS_RUNNING;

/*
 * List of known peers
 */
opal_list_t ompi_crcp_bkmrk_pml_peer_refs;

/*
 * MPI_ANY_SOURCE recv lists
 */
opal_list_t unknown_recv_from_list;
opal_list_t unknown_persist_recv_list;

/*
 * List of pending drain acks
 */
opal_list_t drained_msg_ack_list;

/*
 * Free lists
 */
ompi_free_list_t coord_state_free_list;
ompi_free_list_t content_ref_free_list;
ompi_free_list_t peer_ref_free_list;
ompi_free_list_t traffic_msg_ref_free_list;
ompi_free_list_t drain_msg_ref_free_list;
ompi_free_list_t drain_ack_msg_ref_free_list;

/*
 * Quiescence requests to wait on
 */
ompi_request_t       ** quiesce_requests = NULL;
ompi_status_public_t ** quiesce_statuses = NULL;
int                     quiesce_request_count = 0;

/************************************
 * Local Funcation Decls.
 ************************************/

static int ompi_crcp_bkmrk_pml_start_isend_init(ompi_request_t **request);
static int ompi_crcp_bkmrk_pml_start_irecv_init(ompi_request_t **request);
static int ompi_crcp_bkmrk_pml_start_drain_irecv_init(ompi_request_t **request, bool *found_drain);

static int ompi_crcp_bkmrk_request_complete_isend_init(struct ompi_request_t *request, 
                                               ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                                               int src, int tag, int tmp_ddt_size);
static int ompi_crcp_bkmrk_request_complete_isend(struct ompi_request_t *request, 
                                               ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                                               int src, int tag, int tmp_ddt_size);
static int ompi_crcp_bkmrk_request_complete_irecv_init(struct ompi_request_t *request, 
                                               ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                                               int src, int tag, int tmp_ddt_size);
static int ompi_crcp_bkmrk_request_complete_irecv(struct ompi_request_t *request, 
                                               ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                                               int src, int tag, int tmp_ddt_size);

/*
 * Traffic Message: Append
 */
static int traffic_message_append(ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                                  opal_list_t * append_list,
                                  ompi_crcp_bkmrk_pml_message_type_t msg_type,
                                  size_t count,
                                  ompi_datatype_t *datatype,
                                  size_t ddt_size,
                                  int tag,
                                  int dest,
                                  struct ompi_communicator_t* comm,
                                  ompi_crcp_bkmrk_pml_traffic_message_ref_t **msg_ref);

/*
 * Traffic Message: Start a persistent send/recv
 */
static int traffic_message_start(ompi_crcp_bkmrk_pml_traffic_message_ref_t *msg_ref,
                                 ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                                 ompi_request_t **request,
                                 opal_list_t * peer_list,
                                 ompi_crcp_bkmrk_pml_message_content_ref_t ** content_ref);

/*
 * Traffic Message: Move a message from one list to another
 * - useful when moving messages from the unknown lists
 */
static int traffic_message_move(ompi_crcp_bkmrk_pml_traffic_message_ref_t *msg_ref,
                                ompi_crcp_bkmrk_pml_message_type_t msg_type,
                                ompi_crcp_bkmrk_pml_peer_ref_t *from_peer_ref,
                                opal_list_t * from_list,
                                ompi_crcp_bkmrk_pml_peer_ref_t *to_peer_ref,
                                opal_list_t * to_list,
                                ompi_crcp_bkmrk_pml_traffic_message_ref_t **new_msg_ref,
                                bool keep_active, /* If you have to create a new context, should it be initialized to active? */
                                bool remove); /* Remove the original? - false = copy() */

/*
 * Traffic Message: Strip off the first matching request
 */
static int traffic_message_grab_content(ompi_crcp_bkmrk_pml_traffic_message_ref_t *msg_ref,
                                        ompi_crcp_bkmrk_pml_message_content_ref_t ** content_ref,
                                        bool remove,
                                        bool already_drained);

/*
 * Traffic Message: Find a persistent message, and mark it approprately
 */
static int traffic_message_find_mark_persistent(ompi_crcp_bkmrk_pml_traffic_message_ref_t *msg_ref,
                                                ompi_request_t **request,
                                                bool cur_active,
                                                bool set_is_active,
                                                ompi_crcp_bkmrk_pml_message_content_ref_t **content_ref);

/*
 * Traffic Message: Find a message that matches the given signature
 */
static int traffic_message_find(opal_list_t * search_list,
                                size_t count, int tag, int peer, uint32_t comm_id,
                                size_t ddt_size,
                                ompi_crcp_bkmrk_pml_traffic_message_ref_t ** found_msg_ref,
                                int active);

/*
 * Traffic Message: Determine if we have received a message matching this signature.
 *   Return a reference to the message on all matching lists.
 */
static int traffic_message_find_recv(ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                                     int rank, uint32_t comm_id, int tag,
                                     size_t count, size_t datatype_size,
                                     ompi_crcp_bkmrk_pml_traffic_message_ref_t ** posted_recv_msg_ref,
                                     ompi_crcp_bkmrk_pml_traffic_message_ref_t ** posted_irecv_msg_ref,
                                     ompi_crcp_bkmrk_pml_traffic_message_ref_t ** posted_precv_msg_ref,
                                     ompi_crcp_bkmrk_pml_traffic_message_ref_t ** posted_unknown_recv_msg_ref,
                                     ompi_crcp_bkmrk_pml_traffic_message_ref_t ** posted_unknown_precv_msg_ref);

/*
 * Traffic Message: For all of the 'active' recvs, create a drain message
 */
static int traffic_message_create_drain_message(bool post_drain,
                                                int max_post,
                                                ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                                                ompi_crcp_bkmrk_pml_traffic_message_ref_t ** posted_msg_ref,
                                                int *num_posted);

/*
 * Drain Message: Append
 */
static int drain_message_append(ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                                ompi_crcp_bkmrk_pml_message_type_t msg_type,
                                size_t count, size_t ddt_size,
                                int tag,int dest,
                                struct ompi_communicator_t* comm,
                                ompi_crcp_bkmrk_pml_drain_message_ref_t **msg_ref);

/*
 * Drain Message: Remove
 */
static int drain_message_remove(ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                                ompi_crcp_bkmrk_pml_drain_message_ref_t *msg_ref,
                                ompi_crcp_bkmrk_pml_message_content_ref_t *content_ref);

/*
 * Drain Message: Check if this receive has been drained
 */
static int drain_message_check_recv(void **buf, size_t count,
                                    ompi_datatype_t *datatype,
                                    int *src, int *tag,
                                    struct ompi_communicator_t* comm,
                                    struct ompi_request_t **request,
                                    ompi_status_public_t** status,
                                    bool *found_drain);

/*
 * Drain Message: Find a message matching the given signature on this peer list
 */
static int drain_message_find(opal_list_t * search_list,
                              size_t count, int tag, int peer,
                              uint32_t comm_id, size_t ddt_size,
                              ompi_crcp_bkmrk_pml_drain_message_ref_t ** found_msg_ref,
                              ompi_crcp_bkmrk_pml_message_content_ref_t ** content_ref);

/*
 * Drain Message: Find a message matching the given signature on any list from any peer
 */
static int drain_message_find_any(size_t count, int tag, int peer,
                                  struct ompi_communicator_t* comm, size_t ddt_size,
                                  ompi_crcp_bkmrk_pml_drain_message_ref_t ** found_msg_ref,
                                  ompi_crcp_bkmrk_pml_message_content_ref_t ** content_ref,
                                  ompi_crcp_bkmrk_pml_peer_ref_t **peer_ref);

/*
 * Drain Message: Grab a content reference, do not remove
 */
static int drain_message_grab_content(ompi_crcp_bkmrk_pml_drain_message_ref_t *drain_msg_ref,
                                      ompi_crcp_bkmrk_pml_message_content_ref_t ** content_ref);

/*
 * Drain Message: Copy this drain message to the signature provided, remove drain message
 */
static int drain_message_copy_remove(ompi_crcp_bkmrk_pml_drain_message_ref_t *msg_ref,
                                     ompi_crcp_bkmrk_pml_message_content_ref_t * content_ref,
                                     int *src, int *tag,
                                     struct ompi_request_t **request,
                                     ompi_status_public_t **status, 
                                     ompi_datatype_t *datatype, int count, void **buf,
                                     ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref);

/*
 * Drain Message: Copy this persistent drain message to the signature provided, remove drain message
 */
static int drain_message_copy_remove_persistent(ompi_crcp_bkmrk_pml_drain_message_ref_t   *drain_msg_ref,
                                                ompi_crcp_bkmrk_pml_message_content_ref_t *drain_content_ref,
                                                ompi_crcp_bkmrk_pml_traffic_message_ref_t *traffic_msg_ref,
                                                ompi_request_t *request,
                                                ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref);

/*
 * Peer List: Find the peer reference matching the ORTE process name
 */
static ompi_crcp_bkmrk_pml_peer_ref_t* find_peer(orte_process_name_t proc);

/*
 * Peer List: Find the peer reference matching the index into the communicator
 */
static int find_peer_in_comm(struct ompi_communicator_t* comm, int proc_idx,
                             ompi_crcp_bkmrk_pml_peer_ref_t **peer_ref);

/*
 * Coordinate Peers
 *  - Quiet channels
 */
static int ft_event_coordinate_peers(void);

/*
 * Finalize the coordination of peers.
 *  - Mostly cleanup.
 */
static int ft_event_finalize_exchange(void);

/*
 * Exchange the bookmarks
 *  - Staggered All-to-All
 * LAM/MPI used a staggered all-to-all algoritm for bookmark exachange
 *    http://www.lam-mpi.org/papers/lacsi2003/
 */
static int ft_event_exchange_bookmarks(void);

/*
 * Send Bookmarks to peer
 */
static int send_bookmarks(int peer_idx);

/*
 * Recv Bookmarks from peer
 */
static int recv_bookmarks(int peer_idx);

/*
 * Callback to receive the bookmarks from a peer
 */
static void recv_bookmarks_cbfunc(int status,
                                  orte_process_name_t* sender,
                                  opal_buffer_t *buffer,
                                  orte_rml_tag_t tag,
                                  void* cbdata);
static int total_recv_bookmarks = 0;

/*
 * Now that we have all the bookmarks, check them to see if we need to 
 * drain any messages.
 */
static int ft_event_check_bookmarks(void);

/*
 * Send message details to peer
 * - matched with recv_msg_details()
 */
static int send_msg_details(ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                            int total_sent, int total_matched);

/*
 * Send a single message reference to a peer.
 * found_match = true if peer found a message to drain.
 */
static int do_send_msg_detail(ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                              ompi_crcp_bkmrk_pml_traffic_message_ref_t*msg_ref,
                              int *num_matches,
                              int *total_found,
                              bool *finished);
/*
 * Recv message details from peer
 * - matched with send_msg_details()
 */
static int recv_msg_details(ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                            int total_recv, int total_matched);

/*
 * Receive a single message reference from a peer.
 */
static int do_recv_msg_detail(ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                              int *rank, uint32_t *comm_id, int *tag,
                              size_t *count, size_t *datatype_size,
                              int *p_num_sent);

/*
 * Check the message reference to determine if:
 * - We have received this message already, or
 * - We need to post this message
 */
static int do_recv_msg_detail_check_drain(ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                                    int rank, uint32_t comm_id, int tag,
                                    size_t count, size_t datatype_size,
                                    int p_num_sent,
                                    int *num_resolved);

/*
 * Respond to peer regarding a received message detail
 */
static int do_recv_msg_detail_resp(ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                                   int resp,
                                   int num_resolv,
                                   int total_found);

/*
 * Post the Drain Message Acks
 *  - These are sent once the receiver has finished receiving
 *    all of the messages it needed to drain off the wire.
 */
static int ft_event_post_drain_acks(void);

/*
 * Callback to service drain message acks.
 */
static void drain_message_ack_cbfunc(int status,
                                     orte_process_name_t* sender,
                                     opal_buffer_t *buffer,
                                     orte_rml_tag_t tag,
                                     void* cbdata);

/*
 * Post the Drain Messages
 *  - These are irecvs to be completed in any order.
 */
static int ft_event_post_drained(void);

static int ft_event_post_drain_message(ompi_crcp_bkmrk_pml_drain_message_ref_t   *drain_msg_ref,
                                       ompi_crcp_bkmrk_pml_message_content_ref_t *content_ref);

/*
 * Wait for all drained messages and acks to complete.
 *  - Once this this finished then all channels associated
 *    with this process have been drained.
 */
static int ft_event_wait_quiesce(void);

/*
 * Wait for all the posted drain messages to finish
 */
static int wait_quiesce_drained(void);

/*
 * An optimized local version of waitall.
 * - Remove some unnecessary logic
 * - Remove logic to 'free' the request
 */
static int coord_request_wait_all( size_t count,
                                   ompi_request_t ** requests,
                                   ompi_status_public_t ** statuses);

/*
 * An optimized local version of wait.
 * - Remove some unnecessary logic
 * - Remove logic to 'free' the request
 * - Allow it to return if we need to stop waiting
 */
static int coord_request_wait( ompi_request_t * request,
                               ompi_status_public_t * status);

/*
 * Wait for all the drain ACKs to be received
 */
static int wait_quiesce_drain_ack(void);

/************************************
 * A few timing structures
 *
 * CRCP Timing Information
 * -----------------------
 *  Pi                     Pj  | Variable
 * ----                   -----+----------
 * exchange_bookmark()         | CRCP_TIMER_CKPT_EX_B
 *      ------------->         | CRCP_TIMER_CKPT_EX_PEER_S
 *      <-------------         | CRCP_TIMER_CKPT_EX_PEER_R
 *  -> wait_for_bk_done()      |
 *  --                     --  | CRCP_TIMER_CKPT_EX_WAIT
 * check_bookmarks()           |
 *  --                     --  | CRCP_TIMER_CKPT_CHECK_B
 *  -> exchange_details (*)    |
 *      ------------->         | CRCP_TIMER_CKPT_CHECK_PEER_S
 *      <-------------         | CRCP_TIMER_CKPT_CHECK_PEER_R
 * post_drain[ack]()           |
 *  --                     --  | CRCP_TIMER_CKPT_POST_DRAIN
 * wait_quiescence()           |
 *  --                     --  | CRCP_TIMER_CKPT_WAIT_QUI
 * Finish checkpoint           |   -- Total Pre-Checkpoint
 *  --                     --  | CRCP_TIMER_TOTAL_CKPT
 * finalize_exchange()         |   -- Total Continue   / Restart
 *  --                     --  | CRCP_TIMER_TOTAL_CONT / _RST
 *-----------------------------+
 * (*) If needed.
 *
 * timing_enabled:
 * < 0 : Off
 *   1 : Summary only
 *   2 : Per Peer messages + Barrier
 *   3 : Messages from all procs
 * 
 ************************************/
#define CRCP_TIMER_TOTAL_CKPT          0
#define CRCP_TIMER_CKPT_EX_B           1
#define CRCP_TIMER_CKPT_EX_PEER_S      2
#define CRCP_TIMER_CKPT_EX_PEER_R      3
#define CRCP_TIMER_CKPT_EX_WAIT        4
#define CRCP_TIMER_CKPT_CHECK_B        5
#define CRCP_TIMER_CKPT_CHECK_PEER_S   6
#define CRCP_TIMER_CKPT_CHECK_PEER_R   7
#define CRCP_TIMER_CKPT_POST_DRAIN     8
#define CRCP_TIMER_CKPT_WAIT_QUI       9
#define CRCP_TIMER_TOTAL_CONT         10
#define CRCP_TIMER_TOTAL_RST          11
#define CRCP_TIMER_MAX                12

static double get_time(void);
static void start_time(int idx);
static void end_time(int idx);
static void display_indv_timer(int idx, int proc, int msgs);
static void display_indv_timer_core(int idx, int proc, int msgs, bool direct);
static void display_all_timers(int state);
static void clear_timers(void);

double timer_start[CRCP_TIMER_MAX];
double timer_end[CRCP_TIMER_MAX];
char * timer_label[CRCP_TIMER_MAX];

#define START_TIMER(idx)                    \
  {                                         \
    if(OPAL_UNLIKELY(timing_enabled > 0)) { \
      start_time(idx);                      \
    }                                       \
  }

#define END_TIMER(idx)                      \
  {                                         \
    if(OPAL_UNLIKELY(timing_enabled > 0)) { \
      end_time(idx);                        \
    }                                       \
  }

#define DISPLAY_INDV_TIMER(idx, proc, msg)  \
  {                                         \
    if(OPAL_UNLIKELY(timing_enabled > 0)) { \
      display_indv_timer(idx, proc, msg);   \
    }                                       \
  }

#define DISPLAY_ALL_TIMERS(var)             \
  {                                         \
    if(OPAL_UNLIKELY(timing_enabled > 0)) { \
      display_all_timers(var);              \
    }                                       \
  }

/************************************
 * Additional Debuging dumps
 ************************************/
#if OPAL_ENABLE_DEBUG
static void traffic_message_dump_peer(ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref, char * msg, bool root_only);
static void traffic_message_dump_msg_list(opal_list_t *msg_list, bool is_drain);
static void traffic_message_dump_msg_indv(ompi_crcp_bkmrk_pml_traffic_message_ref_t * msg_ref, char * msg, bool vshort);
static void traffic_message_dump_msg_content_indv(ompi_crcp_bkmrk_pml_message_content_ref_t * content_ref);

static void traffic_message_dump_drain_msg_indv(ompi_crcp_bkmrk_pml_drain_message_ref_t * msg_ref, char * msg, bool vshort);

#define TRAFFIC_MSG_DUMP_PEER(lv, a) {                 \
   if( lv <= mca_crcp_bkmrk_component.super.verbose ) { \
       traffic_message_dump_peer a;                    \
   }                                                   \
}
#define TRAFFIC_MSG_DUMP_MSG_LIST(lv, a) {             \
   if( lv <= mca_crcp_bkmrk_component.super.verbose ) { \
       traffic_message_dump_msg_list a;                \
   }                                                   \
}
#define TRAFFIC_MSG_DUMP_MSG_INDV(lv, a) {             \
   if( lv <= mca_crcp_bkmrk_component.super.verbose ) { \
       traffic_message_dump_msg_indv a;                \
   }                                                   \
}
#define TRAFFIC_MSG_DUMP_MSG_CONTENT_INDV(lv, a) {     \
   if( lv <= mca_crcp_bkmrk_component.super.verbose ) { \
       traffic_message_dump_msg_content_indv a;        \
   }                                                   \
}
#define TRAFFIC_MSG_DUMP_DRAIN_MSG_INDV(lv, a) {       \
   if( lv <= mca_crcp_bkmrk_component.super.verbose ) { \
       traffic_message_dump_drain_msg_indv a;          \
   }                                                   \
}
#else
#define TRAFFIC_MSG_DUMP_PEER(lv, a)             ;
#define TRAFFIC_MSG_DUMP_MSG_LIST(lv, a)         ;
#define TRAFFIC_MSG_DUMP_MSG_INDV(lv, a)         ;
#define TRAFFIC_MSG_DUMP_MSG_CONTENT_INDV(lv, a) ;
#define TRAFFIC_MSG_DUMP_DRAIN_MSG_INDV(lv, a)   ;
#endif

#define ERROR_SHOULD_NEVER_HAPPEN(msg) {                                 \
  opal_output(0, msg                                                     \
              " ---------- This should never happen ---------- (%s:%d)", \
              __FILE__, __LINE__);                                       \
}

#define ERROR_SHOULD_NEVER_HAPPEN_ARG(msg, arg) {                        \
  opal_output(0, msg                                                     \
              " ---------- This should never happen ---------- (%s:%d)", \
              arg, __FILE__, __LINE__);                                  \
}

/************************************
 * Declare/Define Object Structures
 ************************************/
/*
 * Free List Maintenance
 */
#define HOKE_PEER_REF_ALLOC(peer_ref, rc)             \
do {                                                  \
  ompi_free_list_item_t* item;                        \
  OMPI_FREE_LIST_WAIT(&peer_ref_free_list, item, rc); \
  peer_ref = (ompi_crcp_bkmrk_pml_peer_ref_t*)item;    \
} while(0); 

#define HOKE_PEER_REF_RETURN(peer_ref)        \
do {                                          \
   OMPI_FREE_LIST_RETURN(&peer_ref_free_list, \
   (ompi_free_list_item_t*)peer_ref);         \
} while(0);


#define HOKE_CONTENT_REF_ALLOC(content_ref, rc)                  \
do {                                                             \
  ompi_free_list_item_t* item;                                   \
  OMPI_FREE_LIST_WAIT(&content_ref_free_list, item, rc);         \
  content_ref = (ompi_crcp_bkmrk_pml_message_content_ref_t*)item; \
  content_ref->msg_id = content_ref_seq_num;                     \
  content_ref_seq_num++;\
} while(0); 

#define HOKE_CONTENT_REF_RETURN(content_ref)     \
do {                                             \
   OMPI_FREE_LIST_RETURN(&content_ref_free_list, \
   (ompi_free_list_item_t*)content_ref);         \
} while(0);


#define HOKE_TRAFFIC_MSG_REF_ALLOC(msg_ref, rc)              \
do {                                                         \
  ompi_free_list_item_t* item;                               \
  OMPI_FREE_LIST_WAIT(&traffic_msg_ref_free_list, item, rc); \
  msg_ref = (ompi_crcp_bkmrk_pml_traffic_message_ref_t*)item; \
} while(0); 

#define HOKE_TRAFFIC_MSG_REF_RETURN(msg_ref)         \
do {                                                 \
   OMPI_FREE_LIST_RETURN(&traffic_msg_ref_free_list, \
   (ompi_free_list_item_t*)msg_ref);                 \
} while(0);


#define HOKE_DRAIN_MSG_REF_ALLOC(msg_ref, rc)              \
do {                                                       \
  ompi_free_list_item_t* item;                             \
  OMPI_FREE_LIST_WAIT(&drain_msg_ref_free_list, item, rc); \
  msg_ref = (ompi_crcp_bkmrk_pml_drain_message_ref_t*)item; \
} while(0); 

#define HOKE_DRAIN_MSG_REF_RETURN(msg_ref)         \
do {                                               \
   OMPI_FREE_LIST_RETURN(&drain_msg_ref_free_list, \
   (ompi_free_list_item_t*)msg_ref);               \
} while(0);


#define HOKE_DRAIN_ACK_MSG_REF_ALLOC(msg_ref, rc)              \
do {                                                           \
  ompi_free_list_item_t* item;                                 \
  OMPI_FREE_LIST_WAIT(&drain_ack_msg_ref_free_list, item, rc); \
  msg_ref = (ompi_crcp_bkmrk_pml_drain_message_ack_ref_t*)item; \
} while(0); 

#define HOKE_DRAIN_ACK_MSG_REF_RETURN(msg_ref)         \
do {                                                   \
   OMPI_FREE_LIST_RETURN(&drain_ack_msg_ref_free_list, \
   (ompi_free_list_item_t*)msg_ref);                   \
} while(0);


/*
 * Peer reference
 */
OBJ_CLASS_INSTANCE(ompi_crcp_bkmrk_pml_peer_ref_t,
                   opal_list_item_t,
                   ompi_crcp_bkmrk_pml_peer_ref_construct,
                   ompi_crcp_bkmrk_pml_peer_ref_destruct);

void ompi_crcp_bkmrk_pml_peer_ref_construct(ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref) {
    peer_ref->proc_name.jobid  = ORTE_JOBID_INVALID;
    peer_ref->proc_name.vpid   = ORTE_VPID_INVALID;

    OBJ_CONSTRUCT(&peer_ref->send_list,       opal_list_t);
    OBJ_CONSTRUCT(&peer_ref->isend_list,      opal_list_t);
    OBJ_CONSTRUCT(&peer_ref->send_init_list,  opal_list_t);

    OBJ_CONSTRUCT(&peer_ref->recv_list,       opal_list_t);
    OBJ_CONSTRUCT(&peer_ref->irecv_list,      opal_list_t);
    OBJ_CONSTRUCT(&peer_ref->recv_init_list,  opal_list_t);

    OBJ_CONSTRUCT(&peer_ref->drained_list,    opal_list_t);

    peer_ref->total_msgs_sent    = 0;
    peer_ref->matched_msgs_sent  = 0;

    peer_ref->total_msgs_recvd   = 0;
    peer_ref->matched_msgs_recvd = 0;

    peer_ref->total_drained_msgs = 0;

    peer_ref->ack_required = false;
}

void ompi_crcp_bkmrk_pml_peer_ref_destruct( ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref) {
    opal_list_item_t* item = NULL;

    peer_ref->proc_name.jobid  = ORTE_JOBID_INVALID;
    peer_ref->proc_name.vpid   = ORTE_VPID_INVALID;
    
    while( NULL != (item = opal_list_remove_first(&peer_ref->send_list)) ) {
        HOKE_TRAFFIC_MSG_REF_RETURN(item);
    }
    OBJ_DESTRUCT(&peer_ref->send_list);
    while( NULL != (item = opal_list_remove_first(&peer_ref->isend_list)) ) {
        HOKE_TRAFFIC_MSG_REF_RETURN(item);
    }
    OBJ_DESTRUCT(&peer_ref->isend_list);
    while( NULL != (item = opal_list_remove_first(&peer_ref->send_init_list)) ) {
        HOKE_TRAFFIC_MSG_REF_RETURN(item);
    }
    OBJ_DESTRUCT(&peer_ref->send_init_list);

    while( NULL != (item = opal_list_remove_first(&peer_ref->recv_list)) ) {
        HOKE_TRAFFIC_MSG_REF_RETURN(item);
    }
    OBJ_DESTRUCT(&peer_ref->recv_list);
    while( NULL != (item = opal_list_remove_first(&peer_ref->irecv_list)) ) {
        HOKE_TRAFFIC_MSG_REF_RETURN(item);
    }
    OBJ_DESTRUCT(&peer_ref->irecv_list);
    while( NULL != (item = opal_list_remove_first(&peer_ref->recv_init_list)) ) {
        HOKE_TRAFFIC_MSG_REF_RETURN(item);
    }
    OBJ_DESTRUCT(&peer_ref->recv_init_list);

    while( NULL != (item = opal_list_remove_first(&peer_ref->drained_list)) ) {
        HOKE_DRAIN_MSG_REF_RETURN(item);
    }
    OBJ_DESTRUCT(&peer_ref->drained_list);

    peer_ref->total_msgs_sent    = 0;
    peer_ref->matched_msgs_sent  = 0;

    peer_ref->total_msgs_recvd   = 0;
    peer_ref->matched_msgs_recvd = 0;

    peer_ref->total_drained_msgs = 0;

    peer_ref->ack_required = false;
}

/*
 * Message Content Structure
 */
OBJ_CLASS_INSTANCE(ompi_crcp_bkmrk_pml_message_content_ref_t,
                   opal_list_item_t,
                   ompi_crcp_bkmrk_pml_message_content_ref_construct,
                   ompi_crcp_bkmrk_pml_message_content_ref_destruct);

void ompi_crcp_bkmrk_pml_message_content_ref_construct(ompi_crcp_bkmrk_pml_message_content_ref_t *content_ref)
{
    content_ref->buffer  = NULL;
    content_ref->request = NULL;
    content_ref->active  = false;

    content_ref->done    = false;
    content_ref->active  = false;
    content_ref->already_posted  = false;
    content_ref->already_drained = false;

    content_ref->msg_id = 0;
}

void ompi_crcp_bkmrk_pml_message_content_ref_destruct( ompi_crcp_bkmrk_pml_message_content_ref_t *content_ref)
{
    if( NULL != content_ref->buffer ) {
        free(content_ref->buffer);
    }
    content_ref->buffer  = NULL;

    if( NULL != content_ref->request ) {
        OBJ_RELEASE(content_ref->request);
    }
    content_ref->request = NULL;

    content_ref->active = false;

    content_ref->done    = false;
    content_ref->active  = false;
    content_ref->already_posted  = false;
    content_ref->already_drained = false;

    content_ref->msg_id = 0;
}

/*
 * Traffic Message
 */
OBJ_CLASS_INSTANCE(ompi_crcp_bkmrk_pml_traffic_message_ref_t,
                   opal_list_item_t,
                   ompi_crcp_bkmrk_pml_traffic_message_ref_construct,
                   ompi_crcp_bkmrk_pml_traffic_message_ref_destruct);

void ompi_crcp_bkmrk_pml_traffic_message_ref_construct(ompi_crcp_bkmrk_pml_traffic_message_ref_t *msg_ref) {
    msg_ref->msg_id     = 0;
    msg_ref->msg_type   = COORD_MSG_TYPE_UNKNOWN;

    msg_ref->count      = 0;
    msg_ref->ddt_size   = 0;
    msg_ref->tag        = 0;
    msg_ref->rank       = 0;
    msg_ref->comm       = NULL;

    OBJ_CONSTRUCT(&msg_ref->msg_contents, opal_list_t);

    msg_ref->proc_name.jobid  = ORTE_JOBID_INVALID;
    msg_ref->proc_name.vpid   = ORTE_VPID_INVALID;

    msg_ref->matched        = INVALID_INT;
    msg_ref->done           = INVALID_INT;
    msg_ref->active         = INVALID_INT;
    msg_ref->posted         = INVALID_INT;
    msg_ref->active_drain   = INVALID_INT;
}

void ompi_crcp_bkmrk_pml_traffic_message_ref_destruct( ompi_crcp_bkmrk_pml_traffic_message_ref_t *msg_ref) {
    opal_list_item_t* item = NULL;

    msg_ref->msg_id     = 0;
    msg_ref->msg_type   = COORD_MSG_TYPE_UNKNOWN;

    msg_ref->count      = 0;
    msg_ref->ddt_size   = 0;
    msg_ref->tag        = 0;
    msg_ref->rank       = 0;
    msg_ref->comm       = NULL;

    while( NULL != (item = opal_list_remove_first(&(msg_ref->msg_contents)) ) ) {
        HOKE_CONTENT_REF_RETURN(item);
    }
    OBJ_DESTRUCT(&(msg_ref->msg_contents));

    msg_ref->proc_name.jobid  = ORTE_JOBID_INVALID;
    msg_ref->proc_name.vpid   = ORTE_VPID_INVALID;

    msg_ref->matched        = INVALID_INT;
    msg_ref->done           = INVALID_INT;
    msg_ref->active         = INVALID_INT;
    msg_ref->posted         = INVALID_INT;
    msg_ref->active_drain   = INVALID_INT;
}

/*
 * Drain Message
 */
OBJ_CLASS_INSTANCE(ompi_crcp_bkmrk_pml_drain_message_ref_t,
                   opal_list_item_t,
                   ompi_crcp_bkmrk_pml_drain_message_ref_construct,
                   ompi_crcp_bkmrk_pml_drain_message_ref_destruct);

void ompi_crcp_bkmrk_pml_drain_message_ref_construct(ompi_crcp_bkmrk_pml_drain_message_ref_t *msg_ref) {
    msg_ref->msg_id     = 0;
    msg_ref->msg_type   = COORD_MSG_TYPE_UNKNOWN;

    msg_ref->count      = 0;

    msg_ref->datatype   = NULL;
    msg_ref->ddt_size   = 0;

    msg_ref->tag        = 0;
    msg_ref->rank       = 0;
    msg_ref->comm       = NULL;

    OBJ_CONSTRUCT(&msg_ref->msg_contents, opal_list_t);

    msg_ref->proc_name.jobid  = ORTE_JOBID_INVALID;
    msg_ref->proc_name.vpid   = ORTE_VPID_INVALID;

    msg_ref->done           = INVALID_INT;
    msg_ref->active         = INVALID_INT;
    msg_ref->already_posted = INVALID_INT;
}

void ompi_crcp_bkmrk_pml_drain_message_ref_destruct( ompi_crcp_bkmrk_pml_drain_message_ref_t *msg_ref) {
    opal_list_item_t* item = NULL;

    msg_ref->msg_id     = 0;
    msg_ref->msg_type   = COORD_MSG_TYPE_UNKNOWN;

    msg_ref->count      = 0;

    if( NULL != msg_ref->datatype ) {
        OBJ_RELEASE(msg_ref->datatype);
        msg_ref->datatype   = NULL;
    }
    msg_ref->ddt_size   = 0;

    msg_ref->tag        = 0;
    msg_ref->rank       = 0;
    msg_ref->comm       = NULL;

    while( NULL != (item = opal_list_remove_first(&(msg_ref->msg_contents)) ) ) {
        HOKE_CONTENT_REF_RETURN(item);
    }
    OBJ_DESTRUCT(&(msg_ref->msg_contents));

    msg_ref->proc_name.jobid  = ORTE_JOBID_INVALID;
    msg_ref->proc_name.vpid   = ORTE_VPID_INVALID;

    msg_ref->done           = INVALID_INT;
    msg_ref->active         = INVALID_INT;
    msg_ref->already_posted = INVALID_INT;
}

/*
 * Drain Ack Message
 */
OBJ_CLASS_INSTANCE(ompi_crcp_bkmrk_pml_drain_message_ack_ref_t,
                   opal_list_item_t,
                   ompi_crcp_bkmrk_pml_drain_message_ack_ref_construct,
                   ompi_crcp_bkmrk_pml_drain_message_ack_ref_destruct);

void ompi_crcp_bkmrk_pml_drain_message_ack_ref_construct(ompi_crcp_bkmrk_pml_drain_message_ack_ref_t *msg_ack_ref) {
    msg_ack_ref->complete    = false;

    msg_ack_ref->peer.jobid  = ORTE_JOBID_INVALID;
    msg_ack_ref->peer.vpid   = ORTE_VPID_INVALID;
}

void ompi_crcp_bkmrk_pml_drain_message_ack_ref_destruct( ompi_crcp_bkmrk_pml_drain_message_ack_ref_t *msg_ack_ref) {
    msg_ack_ref->complete   = false;

    msg_ack_ref->peer.jobid  = ORTE_JOBID_INVALID;
    msg_ack_ref->peer.vpid   = ORTE_VPID_INVALID;
}


/*
 * PML state
 */
OBJ_CLASS_INSTANCE(ompi_crcp_bkmrk_pml_state_t,
                   ompi_crcp_base_pml_state_t,
                   NULL,
                   NULL
                   );

/************************************
 * Some Macro shortcuts
 ************************************/
#define CRCP_COORD_STATE_ALLOC(state_ref, rc)            \
do {                                                     \
  ompi_free_list_item_t* item;                           \
  OMPI_FREE_LIST_WAIT(&coord_state_free_list, item, rc); \
  state_ref = (ompi_crcp_bkmrk_pml_state_t*)item;         \
} while(0); 

#define CRCP_COORD_STATE_RETURN(state_ref)       \
do {                                             \
   OMPI_FREE_LIST_RETURN(&coord_state_free_list, \
   (ompi_free_list_item_t*)state_ref);           \
} while(0);

#define CREATE_COORD_STATE(coord_state, pml_state, v_peer_ref, v_msg_ref)         \
 {                                                                                \
   CRCP_COORD_STATE_ALLOC(coord_state, ret);                                      \
                                                                                  \
   coord_state->prev_ptr           = pml_state;                                   \
   coord_state->p_super.super      = pml_state->super;                            \
   coord_state->p_super.state      = pml_state->state;                            \
   coord_state->p_super.error_code = pml_state->error_code;                       \
   coord_state->p_super.wrapped_pml_component = pml_state->wrapped_pml_component; \
   coord_state->p_super.wrapped_pml_module    = pml_state->wrapped_pml_module;    \
                                                                                  \
   coord_state->peer_ref         = v_peer_ref;                                    \
   coord_state->msg_ref          = v_msg_ref;                                     \
 }

#define EXTRACT_COORD_STATE(pml_state, v_coord_state, v_rtn_state, v_peer_ref, v_msg_ref) \
 {                                                           \
   v_coord_state = (ompi_crcp_bkmrk_pml_state_t*)pml_state;   \
   v_rtn_state   = v_coord_state->prev_ptr;                  \
   v_peer_ref    = v_coord_state->peer_ref;                  \
   v_msg_ref     = v_coord_state->msg_ref;                   \
 }


#define CREATE_NEW_MSG(msg_ref, v_type, v_count, v_ddt_size, v_tag, v_rank, v_comm, p_jobid, p_vpid) \
 {                                                               \
   HOKE_TRAFFIC_MSG_REF_ALLOC(msg_ref, ret);                     \
                                                                 \
   msg_ref->msg_id   = message_seq_num;                          \
   message_seq_num++;                                            \
                                                                 \
   msg_ref->msg_type = v_type;                                   \
                                                                 \
   msg_ref->count    = v_count;                                  \
                                                                 \
   msg_ref->ddt_size = v_ddt_size;                               \
                                                                 \
   msg_ref->tag     = v_tag;                                     \
   msg_ref->rank    = v_rank;                                    \
   msg_ref->comm    = v_comm;                                    \
                                                                 \
   msg_ref->proc_name.jobid  = p_jobid;                          \
   msg_ref->proc_name.vpid   = p_vpid;                           \
                                                                 \
   msg_ref->matched = 0;                                         \
   msg_ref->done    = 0;                                         \
   msg_ref->active  = 0;                                         \
   msg_ref->posted  = 0;                                         \
   msg_ref->active_drain = 0;                                    \
 }

#define CREATE_NEW_DRAIN_MSG(msg_ref, v_type, v_count, v_ddt_size, v_tag, v_rank, v_comm, p_jobid, p_vpid) \
 {                                                               \
   HOKE_DRAIN_MSG_REF_ALLOC(msg_ref, ret);                       \
                                                                 \
   msg_ref->msg_id   = message_seq_num;                          \
   message_seq_num++;                                            \
                                                                 \
   msg_ref->msg_type = v_type;                                   \
                                                                 \
   msg_ref->count    = v_count;                                  \
                                                                 \
   msg_ref->datatype = NULL;                                     \
   msg_ref->ddt_size = ddt_size;                                 \
                                                                 \
   msg_ref->tag     = v_tag;                                     \
   msg_ref->rank    = v_rank;                                    \
   msg_ref->comm    = v_comm;                                    \
                                                                 \
   msg_ref->proc_name.jobid  = p_jobid;                          \
   msg_ref->proc_name.vpid   = p_vpid;                           \
 }


#define PACK_BUFFER(buffer, var, count, type, error_msg)                       \
 {                                                                             \
    if (OMPI_SUCCESS != (ret = opal_dss.pack(buffer, &(var), count, type)) ) { \
        opal_output(mca_crcp_bkmrk_component.super.output_handle,               \
                    "%s (Return %d)", error_msg, ret);                         \
        exit_status = ret;                                                     \
        goto cleanup;                                                          \
    }                                                                          \
 }

#define UNPACK_BUFFER(buffer, var, count, type, error_msg)                     \
 {                                                                             \
    orte_std_cntr_t n = count;                                                 \
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &(var), &n, type)) ) {  \
        opal_output(mca_crcp_bkmrk_component.super.output_handle,               \
                    "%s (Return %d)", error_msg, ret);                         \
        exit_status = ret;                                                     \
        goto cleanup;                                                          \
    }                                                                          \
 }

/****************
 * PML Wrapper Init/Finalize
 ****************/
int ompi_crcp_bkmrk_pml_init(void) {
    message_seq_num = 1;
    current_msg_id  = 0;
    current_msg_type = COORD_MSG_TYPE_UNKNOWN;
    stall_for_completion = false;
    ft_event_state = OPAL_CRS_RUNNING;

    OBJ_CONSTRUCT(&ompi_crcp_bkmrk_pml_peer_refs, opal_list_t);

    OBJ_CONSTRUCT(&unknown_recv_from_list, opal_list_t);
    OBJ_CONSTRUCT(&unknown_persist_recv_list, opal_list_t);

    OBJ_CONSTRUCT(&drained_msg_ack_list, opal_list_t);

    /* Create free lists for
     * - Coord State
     * - Peer Refs
     * - Traffic Message Refs
     * - Drain Message Refs
     * - Drain ACK Messsage Refs
     * - Message Contents?
     */
    OBJ_CONSTRUCT(&coord_state_free_list, ompi_free_list_t);
    ompi_free_list_init_new( &coord_state_free_list,
                             sizeof(ompi_crcp_bkmrk_pml_state_t),
                             opal_cache_line_size,
                             OBJ_CLASS(ompi_crcp_bkmrk_pml_state_t),
                             0,opal_cache_line_size,
                             4,  /* Initial number */
                             -1, /* Max = Unlimited */
                             4,  /* Increment by */
                             NULL);

    OBJ_CONSTRUCT(&content_ref_free_list, ompi_free_list_t);
    ompi_free_list_init_new( &content_ref_free_list,
                             sizeof(ompi_crcp_bkmrk_pml_message_content_ref_t),
                             opal_cache_line_size,
                             OBJ_CLASS(ompi_crcp_bkmrk_pml_message_content_ref_t),
                             0,opal_cache_line_size,
                             80, /* Initial number */
                             -1, /* Max = Unlimited */
                             32, /* Increment by */
                             NULL);

    OBJ_CONSTRUCT(&peer_ref_free_list, ompi_free_list_t);
    ompi_free_list_init_new( &peer_ref_free_list,
                             sizeof(ompi_crcp_bkmrk_pml_peer_ref_t),
                             opal_cache_line_size,
                             OBJ_CLASS(ompi_crcp_bkmrk_pml_peer_ref_t),
                             0,opal_cache_line_size,
                             16, /* Initial number */
                             -1, /* Max = Unlimited */
                             16, /* Increment by */
                             NULL);

    OBJ_CONSTRUCT(&traffic_msg_ref_free_list, ompi_free_list_t);
    ompi_free_list_init_new( &traffic_msg_ref_free_list,
                             sizeof(ompi_crcp_bkmrk_pml_traffic_message_ref_t),
                             opal_cache_line_size,
                             OBJ_CLASS(ompi_crcp_bkmrk_pml_traffic_message_ref_t),
                             0,opal_cache_line_size,
                             32, /* Initial number */
                             -1, /* Max = Unlimited */
                             64, /* Increment by */
                             NULL);

    OBJ_CONSTRUCT(&drain_msg_ref_free_list, ompi_free_list_t);
    ompi_free_list_init_new( &drain_msg_ref_free_list,
                             sizeof(ompi_crcp_bkmrk_pml_drain_message_ref_t),
                             opal_cache_line_size,
                             OBJ_CLASS(ompi_crcp_bkmrk_pml_drain_message_ref_t),
                             0,opal_cache_line_size,
                             32, /* Initial number */
                             -1, /* Max = Unlimited */
                             64, /* Increment by */
                             NULL);

    OBJ_CONSTRUCT(&drain_ack_msg_ref_free_list, ompi_free_list_t);
    ompi_free_list_init_new( &drain_ack_msg_ref_free_list,
                             sizeof(ompi_crcp_bkmrk_pml_drain_message_ack_ref_t),
                             opal_cache_line_size,
                             OBJ_CLASS(ompi_crcp_bkmrk_pml_drain_message_ack_ref_t),
                             0,opal_cache_line_size,
                             16, /* Initial number */
                             -1, /* Max = Unlimited */
                             16, /* Increment by */
                             NULL);

    clear_timers();

    if( timing_enabled > 0 ) {
        timer_label[CRCP_TIMER_TOTAL_CKPT]        = strdup("Total Ckpt.");
        timer_label[CRCP_TIMER_CKPT_EX_B]         = strdup("Exchange Bookmarks");
        timer_label[CRCP_TIMER_CKPT_EX_PEER_S]    = strdup("  Ex.Bk. Send Peer");
        timer_label[CRCP_TIMER_CKPT_EX_PEER_R]    = strdup("  Ex.Bk. Recv Peer");
        timer_label[CRCP_TIMER_CKPT_EX_WAIT]      = strdup("  Ex.Bk. Wait");

        timer_label[CRCP_TIMER_CKPT_CHECK_B]      = strdup("Check Bookmarks");
        timer_label[CRCP_TIMER_CKPT_CHECK_PEER_S] = strdup("  Ck.Bk. Send Peer");
        timer_label[CRCP_TIMER_CKPT_CHECK_PEER_R] = strdup("  Ck.Bk. Recv Peer");

        timer_label[CRCP_TIMER_CKPT_POST_DRAIN]   = strdup("Post Drain Msgs.");
        timer_label[CRCP_TIMER_CKPT_WAIT_QUI]     = strdup("Wait for Quiescence");

        timer_label[CRCP_TIMER_TOTAL_CONT]        = strdup("Total Continue");

        timer_label[CRCP_TIMER_TOTAL_RST]         = strdup("Total Restart");
    }

    return OMPI_SUCCESS;
}

int ompi_crcp_bkmrk_pml_finalize(void) {
    int i;

    current_msg_id = 0;
    current_msg_type = COORD_MSG_TYPE_UNKNOWN;
    stall_for_completion = false;
    ft_event_state = OPAL_CRS_RUNNING;

    OBJ_DESTRUCT(&ompi_crcp_bkmrk_pml_peer_refs);

    OBJ_DESTRUCT(&unknown_recv_from_list);
    OBJ_DESTRUCT(&unknown_persist_recv_list);

    OBJ_DESTRUCT(&drained_msg_ack_list);

    /* Destroy All Free Lists */
    OBJ_DESTRUCT(&peer_ref_free_list);
    OBJ_DESTRUCT(&traffic_msg_ref_free_list);
    OBJ_DESTRUCT(&drain_msg_ref_free_list);
    OBJ_DESTRUCT(&drain_ack_msg_ref_free_list);
    OBJ_DESTRUCT(&content_ref_free_list);

    if( timing_enabled > 0 ) {
        for(i = 0; i < CRCP_TIMER_MAX; ++i) {
            free(timer_label[i]);
            timer_label[i] = NULL;
        }
    }

    return OMPI_SUCCESS;
}

/****************
 * PML Wrapper
 ****************/
/**************** Enable *****************/
ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_enable(
                                  bool enable,
                                  ompi_crcp_base_pml_state_t* pml_state )
{
    /* Note: This function is not used. Set to NULL in crcp_bkmrk_module.c */
    OPAL_OUTPUT_VERBOSE((30, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: pml_enable()"));

    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

/**************** Progress *****************/
ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_progress(
                                  ompi_crcp_base_pml_state_t* pml_state)
{
    /* Note: This function is not used. Set to NULL in crcp_bkmrk_module.c */

    OPAL_OUTPUT_VERBOSE((35, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: pml_progress()"));

    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

/**************** Probe *****************/
/* JJH - Code reuse: Combine iprobe and probe logic */
ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_iprobe(
                                  int dst, int tag, 
                                  struct ompi_communicator_t* comm, 
                                  int *matched,
                                  ompi_status_public_t* status, 
                                  ompi_crcp_base_pml_state_t* pml_state )
{
    ompi_crcp_bkmrk_pml_drain_message_ref_t   *drain_msg_ref = NULL;
    ompi_crcp_bkmrk_pml_message_content_ref_t *content_ref = NULL;
    int exit_status = OMPI_SUCCESS;
    int ret;

    OPAL_OUTPUT_VERBOSE((30, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: pml_iprobe(%d, %d)", dst, tag));

    /*
     * Before PML Call
     * - Determine if this can be satisfied from the drained list
     * - Otherwise let the PML handle it
     */
    if( OMPI_CRCP_PML_PRE == pml_state->state) {
        /*
         * Check to see if this message is in the drained message list
         */
        if( OMPI_SUCCESS != (ret = drain_message_find_any(PROBE_ANY_COUNT, tag, dst,
                                                          comm, PROBE_ANY_SIZE,
                                                          &drain_msg_ref,
                                                          &content_ref,
                                                          NULL) ) ) {
            ERROR_SHOULD_NEVER_HAPPEN("crcp:bkmrk: pml_iprobe(): Failed trying to find a drained message.");
            exit_status = ret;
            goto DONE;
        }

        /*
         * If the message is a drained message
         *  - Copy of the status structure to pass back to the user
         *  - Mark the 'matched' flag as true
         */
        if( NULL != drain_msg_ref ) {
            OPAL_OUTPUT_VERBOSE((12, mca_crcp_bkmrk_component.super.output_handle,
                                 "crcp:bkmrk: pml_iprobe(): Matched a drained message..."));

            /* Copy the status information */
            if( MPI_STATUS_IGNORE != status ) {
                memcpy(status, &content_ref->status, sizeof(ompi_status_public_t)); 
            }

            /* Mark as complete */
            *matched = 1;

            /* This will identify to the wrapper that this message is complete */
            pml_state->state = OMPI_CRCP_PML_DONE;
            pml_state->error_code = OMPI_SUCCESS;
            return pml_state;
        }
        /*
         * Otherwise the message is not drained (common case), so let the PML deal with it
         */
        else {
            /* Mark as not complete */
            *matched = 0;
        }
    }

 DONE:
    pml_state->error_code = exit_status;
    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_probe(
                                  int dst, int tag, 
                                  struct ompi_communicator_t* comm, 
                                  ompi_status_public_t* status, 
                                  ompi_crcp_base_pml_state_t* pml_state )
{
    ompi_crcp_bkmrk_pml_drain_message_ref_t *drain_msg_ref = NULL;
    ompi_crcp_bkmrk_pml_message_content_ref_t *content_ref = NULL;
    int exit_status = OMPI_SUCCESS;
    int ret;

    OPAL_OUTPUT_VERBOSE((30, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: pml_probe(%d, %d)", dst, tag));

    /*
     * Before PML Call
     * - Determine if this can be satisfied from the drained list
     * - Otherwise let the PML handle it
     */
    if( OMPI_CRCP_PML_PRE == pml_state->state) {
        /*
         * Check to see if this message is in the drained message list
         */
        if( OMPI_SUCCESS != (ret = drain_message_find_any(PROBE_ANY_COUNT, tag, dst,
                                                          comm, PROBE_ANY_SIZE,
                                                          &drain_msg_ref,
                                                          &content_ref,
                                                          NULL) ) ) {
            ERROR_SHOULD_NEVER_HAPPEN("crcp:bkmrk: pml_probe(): Failed trying to find a drained message.");
            exit_status = ret;
            goto DONE;
        }

        /*
         * If the message is a drained message
         *  - Copy of the status structure to pass back to the user
         */
        if( NULL != drain_msg_ref ) {
            OPAL_OUTPUT_VERBOSE((12, mca_crcp_bkmrk_component.super.output_handle,
                                 "crcp:bkmrk: pml_iprobe(): Matched a drained message..."));

            /* Copy the status information */
            if( MPI_STATUS_IGNORE != status ) {
                memcpy(status, &content_ref->status, sizeof(ompi_status_public_t)); 
            }

            /* This will identify to the wrapper that this message is complete */
            pml_state->state = OMPI_CRCP_PML_DONE;
            pml_state->error_code = OMPI_SUCCESS;
            return pml_state;
        }
    }

 DONE:
    pml_state->error_code = exit_status;
    return pml_state;
}

/**************** Dump *****************/
ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_dump(
                                  struct ompi_communicator_t* comm,
                                  int verbose, 
                                  ompi_crcp_base_pml_state_t* pml_state )
{
    OPAL_OUTPUT_VERBOSE((30, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: pml_dump()"));

    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}


/**************** Communicator *****************/
ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_add_comm(
                                  struct ompi_communicator_t* comm, 
                                  ompi_crcp_base_pml_state_t* pml_state )
{
    /* Note: This function is not used. Set to NULL in crcp_bkmrk_module.c */

    OPAL_OUTPUT_VERBOSE((30, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: pml_add_comm()"));

    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_del_comm(
                                  struct ompi_communicator_t* comm, 
                                  ompi_crcp_base_pml_state_t* pml_state )
{
    /* Note: This function is not used. Set to NULL in crcp_bkmrk_module.c */

    OPAL_OUTPUT_VERBOSE((30, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: pml_del_comm()"));

    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

/**************** Processes *****************/
ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_add_procs(
                                   struct ompi_proc_t **procs,
                                   size_t nprocs, 
                                   ompi_crcp_base_pml_state_t* pml_state )
{
    int ret;
    ompi_crcp_bkmrk_pml_peer_ref_t *new_peer_ref;
    size_t i;

    if( OMPI_CRCP_PML_PRE != pml_state->state ){
        goto DONE;
    }

    OPAL_OUTPUT_VERBOSE((30, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: pml_add_procs()"));

    /*
     * Save pointers to the wrapped PML
     */
    wrapped_pml_component = pml_state->wrapped_pml_component;
    wrapped_pml_module    = pml_state->wrapped_pml_module;

    /*
     * Create a peer_ref for each peer added
     */
    for( i = 0; i < nprocs; ++i) {
        HOKE_PEER_REF_ALLOC(new_peer_ref, ret);

        new_peer_ref->proc_name.jobid  = procs[i]->proc_name.jobid;
        new_peer_ref->proc_name.vpid   = procs[i]->proc_name.vpid;

        opal_list_append(&ompi_crcp_bkmrk_pml_peer_refs, &(new_peer_ref->super));
    }

 DONE:
    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_del_procs(
                                  struct ompi_proc_t **procs,
                                  size_t nprocs, 
                                  ompi_crcp_base_pml_state_t* pml_state )
{
    opal_list_item_t *item = NULL;
    ompi_crcp_bkmrk_pml_peer_ref_t *old_peer_ref;
    int exit_status = OMPI_SUCCESS;
    size_t i;

    if( OMPI_CRCP_PML_PRE != pml_state->state ){
        goto DONE;
    }

    OPAL_OUTPUT_VERBOSE((30, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: pml_del_procs()"));

    for( i = 0; i < nprocs; ++i) {
        item = (opal_list_item_t*)find_peer(procs[i]->proc_name);
        if(NULL == item) {
            opal_output(mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: del_procs: Unable to find peer %s\n",
                        ORTE_NAME_PRINT(&(procs[i]->proc_name)));
            exit_status = OMPI_ERROR;
            goto DONE;
        }

        /* Remove the found peer from the list */
        opal_list_remove_item(&ompi_crcp_bkmrk_pml_peer_refs, item);
        old_peer_ref = (ompi_crcp_bkmrk_pml_peer_ref_t*)item;
        HOKE_PEER_REF_RETURN(old_peer_ref);
    }

 DONE:
    pml_state->error_code = exit_status;
    return pml_state;
}

/**************** Send *****************/
ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_isend_init(
                                  void *buf, size_t count,
                                  ompi_datatype_t *datatype,
                                  int dst, int tag, 
                                  mca_pml_base_send_mode_t mode,
                                  struct ompi_communicator_t* comm,
                                  struct ompi_request_t **request, 
                                  ompi_crcp_base_pml_state_t* pml_state )
{
    ompi_crcp_bkmrk_pml_peer_ref_t    *peer_ref    = NULL;
    ompi_crcp_bkmrk_pml_traffic_message_ref_t *msg_ref     = NULL;
    ompi_crcp_bkmrk_pml_state_t       *coord_state = NULL;
    int exit_status = OMPI_SUCCESS;
    int ret;

    OPAL_OUTPUT_VERBOSE((30, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: pml_isend_init()"));

    /*
     * Before the PML gets the message:
     *  - Setup structure to track the message
     */
    if( OMPI_CRCP_PML_PRE == pml_state->state ) {
        /*
         * Find the peer reference
         */
        if( OMPI_SUCCESS != (ret = find_peer_in_comm(comm, dst, &peer_ref) ) ){
            opal_output(mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: isend: Failed to find peer_ref\n");
            exit_status = ret;
            goto DONE;
        }

        /*
         * Archive the message Message Object
         */
        traffic_message_append(peer_ref, &(peer_ref->send_init_list),
                               COORD_MSG_TYPE_P_SEND,
                               count, datatype, 0, tag, dst, comm,
                               &msg_ref);

        /* Save the pointers */
        CREATE_COORD_STATE(coord_state, pml_state,
                           peer_ref, msg_ref);

        coord_state->p_super.error_code = OMPI_SUCCESS;
        return &coord_state->p_super;
    }
    /*
     * After PML is done, update message reference
     */
    else if( OMPI_CRCP_PML_POST == pml_state->state ) {
        ompi_crcp_base_pml_state_t *rtn_state = NULL;
        ompi_crcp_bkmrk_pml_message_content_ref_t *new_content = NULL;

        EXTRACT_COORD_STATE(pml_state, coord_state, rtn_state,
                           peer_ref,  msg_ref);

        /*
         * Update Message
         */
        HOKE_CONTENT_REF_ALLOC(new_content, ret);
        new_content->buffer  =  buf;
        new_content->request = *request;
        new_content->done    =  false;
        new_content->active  =  false;
        new_content->already_posted  = true;
        new_content->already_drained = false;
        OBJ_RETAIN(*request);
        opal_list_append(&(msg_ref->msg_contents), &(new_content->super) );

        CRCP_COORD_STATE_RETURN(coord_state);

        rtn_state->error_code = OMPI_SUCCESS;
        return rtn_state;
    }

 DONE:
    pml_state->error_code = exit_status;
    return pml_state;
}

static int ompi_crcp_bkmrk_pml_start_isend_init(ompi_request_t **request)
{
    int ret, exit_status = OMPI_SUCCESS;
    ompi_crcp_bkmrk_pml_peer_ref_t            *peer_ref = NULL;
    ompi_crcp_bkmrk_pml_traffic_message_ref_t *msg_ref  = NULL;
    ompi_crcp_bkmrk_pml_message_content_ref_t *content_ref = NULL;
    mca_pml_base_request_t *breq = NULL;
    size_t tmp_ddt_size  = 0;

    breq = (mca_pml_base_request_t *)(*request);
    ompi_datatype_type_size(breq->req_datatype, &tmp_ddt_size);

    /*
     * Find the peer reference
     */
    if( OMPI_SUCCESS != (ret = find_peer_in_comm(breq->req_comm,
                                                 breq->req_peer,
                                                 &peer_ref) ) ){
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: req_start(): Failed to find peer_ref\n");
        exit_status = ret;
        goto DONE;
    }

    /* Check the send_init list */
    if( OMPI_SUCCESS != (ret = traffic_message_find(&(peer_ref->send_init_list),
                                                    breq->req_count,
                                                    breq->req_tag,
                                                    breq->req_peer,
                                                    breq->req_comm->c_contextid,
                                                    tmp_ddt_size,
                                                    &msg_ref,
                                                    PERSIST_MARKER
                                                    ) ) ) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: pml_start(): Unable to find the proper (send_init) message ref for this recv\n");
        exit_status = ret;
        goto DONE;
    }

    if( NULL == msg_ref ) {
        ERROR_SHOULD_NEVER_HAPPEN("crcp:bkmrk: pml_start(): Could not find message ref");
        exit_status = OMPI_ERROR;
        goto DONE;
    } else {
        traffic_message_start(msg_ref,
                              peer_ref,
                              request,
                              &(peer_ref->send_init_list),
                              &content_ref);

        if( !content_ref->already_drained ) {
            /* Account for this inflight send */
            peer_ref->total_msgs_sent += 1;
        }
    }

 DONE:
    return exit_status;
}

static int ompi_crcp_bkmrk_request_complete_isend_init(struct ompi_request_t *request, 
                                               ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                                               int src, int tag, int tmp_ddt_size)
{
    int ret, exit_status = OMPI_SUCCESS;
    mca_pml_base_request_t *breq = NULL;
    ompi_crcp_bkmrk_pml_traffic_message_ref_t *msg_ref = NULL;
    ompi_crcp_bkmrk_pml_message_content_ref_t *content_ref = NULL;

    breq = (mca_pml_base_request_t *)request;

    /* Check the isend_init list */
    if( OMPI_SUCCESS != (ret = traffic_message_find(&(peer_ref->send_init_list),
                                                    breq->req_count,
                                                    tag, src,
                                                    breq->req_comm->c_contextid,
                                                    tmp_ddt_size,
                                                    &msg_ref,
                                                    FIND_MSG_TRUE
                                                    ) ) ) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: req_complete: Unable to find the proper (send_init) message ref for this complete\n");
        exit_status = ret;
        goto DONE;
    }

    if( NULL == msg_ref ) {
        /*
         * It is possible that we did not 'find' the message because 
         * we could have previously marked it as done. Due to the
         * logic in the Request Wait/Test routines we could
         * receive multiple request complete calls for the
         * same request.
         *
         * It is possible that we have 'completed' this message previously,
         *  so this case can occur during normal operation.
         * This is caused by us checking for completeness twice in ompi_request_wait_all.
         */
        OPAL_OUTPUT_VERBOSE((15, mca_crcp_bkmrk_component.super.output_handle,
                            "crcp:bkmrk: request_complete: No match found for this request :( %d, %d ): [%d/%d,%d]\n",
                             peer_ref->total_msgs_sent, peer_ref->total_msgs_recvd,
                             breq->req_peer, src, breq->req_comm->c_contextid));
        exit_status = OMPI_SUCCESS;
        goto DONE;
    }

    /* Mark request as inactive */
    traffic_message_find_mark_persistent(msg_ref, &request,
                                         true,  /* Find currently active */
                                         false, /* Mark as inactive */
                                         &content_ref);

    TRAFFIC_MSG_DUMP_PEER(15, (peer_ref, "-- Request Complete (Send_init) --", true));

    if( !content_ref->already_drained ) {
        msg_ref->done++;
        msg_ref->active--;
    } else {
        msg_ref->active_drain--;
        content_ref->already_drained = false;
    }

    OPAL_OUTPUT_VERBOSE((25, mca_crcp_bkmrk_component.super.output_handle,
                         "crcp:bkmrk: req_complete: Marked Message... ( %d, %d )\n",
                         peer_ref->total_msgs_sent, peer_ref->total_msgs_recvd));
 DONE:
    return exit_status;
}


ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_isend(
                                  void *buf, size_t count,
                                  ompi_datatype_t *datatype,
                                  int dst, int tag,
                                  mca_pml_base_send_mode_t mode,
                                  struct ompi_communicator_t* comm,
                                  struct ompi_request_t **request, 
                                  ompi_crcp_base_pml_state_t* pml_state )
{
    ompi_crcp_bkmrk_pml_peer_ref_t    *peer_ref    = NULL;
    ompi_crcp_bkmrk_pml_traffic_message_ref_t *msg_ref     = NULL;
    ompi_crcp_bkmrk_pml_state_t       *coord_state = NULL;
    int exit_status = OMPI_SUCCESS;
    int ret;

    OPAL_OUTPUT_VERBOSE((30, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: pml_isend()"));

    /*
     * Before the PML gets the message:
     *  - Setup structure to track the message
     */
    if( OMPI_CRCP_PML_PRE == pml_state->state ) {
        /*
         * Find the peer reference
         */
        if( OMPI_SUCCESS != (ret = find_peer_in_comm(comm, dst, &peer_ref) ) ){
            opal_output(mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: isend: Failed to find peer_ref\n");
            exit_status = ret;
            goto DONE;
        }

        /*
         * Archive the message Message Object
         */
        traffic_message_append(peer_ref, &(peer_ref->isend_list),
                               COORD_MSG_TYPE_I_SEND,
                               count, datatype, 0, tag, dst, comm,
                               &msg_ref);

        /*  Bookkeeping */
        peer_ref->total_msgs_sent += 1;

        /* Save the pointers */
        CREATE_COORD_STATE(coord_state, pml_state,
                           peer_ref, msg_ref);

        coord_state->p_super.error_code = OMPI_SUCCESS;
        return &coord_state->p_super;
    }
    /*
     * After PML is done, update message reference
     */
    else if( OMPI_CRCP_PML_POST == pml_state->state ) {
        ompi_crcp_base_pml_state_t *rtn_state = NULL;
        ompi_crcp_bkmrk_pml_message_content_ref_t *new_content = NULL;

        EXTRACT_COORD_STATE(pml_state, coord_state, rtn_state,
                            peer_ref,  msg_ref);

        /*
         * Update Message
         */
        HOKE_CONTENT_REF_ALLOC(new_content, ret);
        new_content->buffer  =  NULL; /* No Tracked */
        new_content->request = *request;
        new_content->done    =  false;
        new_content->active  =  true;
        new_content->already_posted  = true;
        new_content->already_drained = false;
        OBJ_RETAIN(*request);
        opal_list_append(&(msg_ref->msg_contents), &(new_content->super) );

        TRAFFIC_MSG_DUMP_PEER(15, (peer_ref, "-- Append Message (isend) --", true));

        CRCP_COORD_STATE_RETURN(coord_state);

        rtn_state->error_code = OMPI_SUCCESS;
        return rtn_state;
    }

 DONE:
    pml_state->error_code = exit_status;
    return pml_state;
}

static int ompi_crcp_bkmrk_request_complete_isend(struct ompi_request_t *request, 
                                          ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                                          int src, int tag, int tmp_ddt_size)
{
    int ret, exit_status = OMPI_SUCCESS;
    mca_pml_base_request_t *breq = NULL;
    ompi_crcp_bkmrk_pml_traffic_message_ref_t *msg_ref = NULL;
    ompi_crcp_bkmrk_pml_message_content_ref_t *content_ref = NULL;

    breq = (mca_pml_base_request_t *)request;

    /* Check the isend list */
    if( OMPI_SUCCESS != (ret = traffic_message_find(&(peer_ref->isend_list),
                                                    breq->req_count,
                                                    tag, src,
                                                    breq->req_comm->c_contextid,
                                                    tmp_ddt_size,
                                                    &msg_ref,
                                                    FIND_MSG_TRUE
                                                    ) ) ) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: req_complete: Unable to find the proper (isend) message ref for this complete\n");
        exit_status = ret;
        goto DONE;
    }

    if( NULL == msg_ref ) {
        /*
         * It is possible that we did not 'find' the message because 
         * we could have previously marked it as done. Due to the
         * logic in the Request Wait/Test routines we could
         * receive multiple request complete calls for the
         * same request.
         *
         * It is possible that we have 'completed' this message previously,
         *  so this case can occur during normal operation.
         * This is caused by us checking for completeness twice in ompi_request_wait_all.
         */
        OPAL_OUTPUT_VERBOSE((15, mca_crcp_bkmrk_component.super.output_handle,
                            "crcp:bkmrk: request_complete: No match found for this request :( %d, %d ): [%d/%d,%d]\n",
                             peer_ref->total_msgs_sent, peer_ref->total_msgs_recvd,
                             breq->req_peer, src, breq->req_comm->c_contextid));
        exit_status = OMPI_SUCCESS;
        goto DONE;
    }

    OPAL_OUTPUT_VERBOSE((15, mca_crcp_bkmrk_component.super.output_handle,
                         "crcp:bkmrk: req_complete: Matched an iSend: total = %d",
                         peer_ref->total_msgs_sent));

    /* Strip off an isend request */
    traffic_message_grab_content(msg_ref, &content_ref, true, true); /* Remove, prefer already_drained */

    if( !content_ref->already_drained ) {
        msg_ref->done++;
        msg_ref->active--;
    } else {
        msg_ref->active_drain--;
        content_ref->already_drained = false;
    }
    HOKE_CONTENT_REF_RETURN(content_ref);

    TRAFFIC_MSG_DUMP_PEER(15, (peer_ref, "-- Request Complete (iSend) --", true));

    OPAL_OUTPUT_VERBOSE((25, mca_crcp_bkmrk_component.super.output_handle,
                         "crcp:bkmrk: req_complete: Marked Message... ( %d, %d )\n",
                         peer_ref->total_msgs_sent, peer_ref->total_msgs_recvd));
 DONE:
    return exit_status;
}


ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_send(
                                  void *buf, size_t count,
                                  ompi_datatype_t *datatype,
                                  int dst, int tag,
                                  mca_pml_base_send_mode_t mode,
                                  struct ompi_communicator_t* comm, 
                                  ompi_crcp_base_pml_state_t* pml_state )
{
    ompi_crcp_bkmrk_pml_peer_ref_t    *peer_ref    = NULL;
    ompi_crcp_bkmrk_pml_traffic_message_ref_t *msg_ref     = NULL;
    ompi_crcp_bkmrk_pml_state_t       *coord_state = NULL;
    int exit_status = OMPI_SUCCESS;
    int ret;

    OPAL_OUTPUT_VERBOSE((30, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: pml_send()"));

    /*
     * Before the PML gets the message:
     *  - Setup structure to track the message
     */
    if( OMPI_CRCP_PML_PRE == pml_state->state ) {
        /*
         * Find the peer reference
         */
        if( OMPI_SUCCESS != (ret = find_peer_in_comm(comm, dst, &peer_ref) ) ){
            opal_output(mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: send: Failed to find peer_ref\n");
            exit_status = ret;
            goto DONE;
        }

        /*
         * Archive the message Message Object
         */
        traffic_message_append(peer_ref, &(peer_ref->send_list),
                               COORD_MSG_TYPE_B_SEND,
                               count, datatype, 0, tag, dst, comm,
                               &msg_ref);

        /*  Bookkeeping */
        peer_ref->total_msgs_sent += 1;
        current_msg_id = msg_ref->msg_id;
        current_msg_type = COORD_MSG_TYPE_B_SEND;

        /* Save the pointers */
        CREATE_COORD_STATE(coord_state, pml_state,
                           peer_ref, msg_ref);
        coord_state->p_super.error_code = OMPI_SUCCESS;

        return &coord_state->p_super;
    }
    /*
     * After PML is done, update message reference
     */
    else if( OMPI_CRCP_PML_POST == pml_state->state ) {
        ompi_crcp_base_pml_state_t *rtn_state = NULL;

        EXTRACT_COORD_STATE(pml_state, coord_state, rtn_state,
                            peer_ref,  msg_ref);

        /*
         * Update Message
         */
        msg_ref->done++;
        msg_ref->active--;

        current_msg_id = 0;
        current_msg_type = COORD_MSG_TYPE_UNKNOWN;

        TRAFFIC_MSG_DUMP_PEER(15, (peer_ref, "Send done", true));

        CRCP_COORD_STATE_RETURN(coord_state);
        rtn_state->error_code = OMPI_SUCCESS;

        return rtn_state;
    }

 DONE:
    pml_state->error_code = exit_status;
    return pml_state;
}

/**************** Recv *****************/
ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_irecv_init(
                                  void *buf, size_t count,
                                  ompi_datatype_t *datatype,
                                  int src, int tag,
                                  struct ompi_communicator_t* comm,
                                  struct ompi_request_t **request, 
                                  ompi_crcp_base_pml_state_t* pml_state)
{
    ompi_crcp_bkmrk_pml_peer_ref_t    *peer_ref      = NULL;
    ompi_crcp_bkmrk_pml_traffic_message_ref_t *msg_ref       = NULL;
    ompi_crcp_bkmrk_pml_state_t       *coord_state   = NULL;
    int exit_status = OMPI_SUCCESS;
    int ret;

    OPAL_OUTPUT_VERBOSE((30, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: pml_irecv_init()"));

    /*
     * Before PML Call
     * - Determine if this can be satisfied from the drained list
     * - Otherwise create a new reference to it so we can track it
     */
    if( OMPI_CRCP_PML_PRE == pml_state->state) {
        /*
         * A message will never be on the drained list for this function since
         * it does not actually receive anything, just sets up the system.
         * The receive for these reqeusts are done in the start() and wait()
         * commands.
         */

        /*
         * Find the Peer
         */
        if( MPI_ANY_SOURCE == src || src < 0) {
            /*
             * Archive the message Message Object
             */
            traffic_message_append(NULL, &(unknown_persist_recv_list),
                                   COORD_MSG_TYPE_P_RECV,
                                   count, datatype, 0, tag, src, comm,
                                   &msg_ref);

            CREATE_COORD_STATE(coord_state, pml_state,
                               NULL, msg_ref);
        }
        else {
            if( OMPI_SUCCESS != (ret = find_peer_in_comm(comm, src, &peer_ref) ) ){
                opal_output(mca_crcp_bkmrk_component.super.output_handle,
                            "crcp:bkmrk: recv: Failed to find peer_ref\n");
                exit_status = ret;
                goto DONE;
            }

            /*
             * Archive the message Message Object
             */
            traffic_message_append(peer_ref, &(peer_ref->recv_init_list),
                                   COORD_MSG_TYPE_P_RECV,
                                   count, datatype, 0, tag, src, comm,
                                   &msg_ref);

            CREATE_COORD_STATE(coord_state, pml_state,
                               peer_ref, msg_ref);
        }

        coord_state->p_super.error_code = OMPI_SUCCESS;
        return &coord_state->p_super;
    }
    /*
     * Post PML Call
     * - bookkeeping...
     */
    else if( OMPI_CRCP_PML_POST == pml_state->state) {
        ompi_crcp_base_pml_state_t *rtn_state = NULL;
        ompi_crcp_bkmrk_pml_message_content_ref_t *new_content = NULL;

        EXTRACT_COORD_STATE(pml_state, coord_state, rtn_state,
                            peer_ref,  msg_ref);

        /*
         * Do the update
         */
        HOKE_CONTENT_REF_ALLOC(new_content, ret);
        new_content->buffer  =  buf;
        new_content->request = *request;
        new_content->done    =  false;
        new_content->active  =  false;
        new_content->already_posted  = true;
        new_content->already_drained = false;
        OBJ_RETAIN(*request);
        opal_list_append(&(msg_ref->msg_contents), &(new_content->super) );

        CRCP_COORD_STATE_RETURN(coord_state);

        rtn_state->error_code = OMPI_SUCCESS;
        return rtn_state;
    }

 DONE:
    pml_state->error_code = exit_status;
    return pml_state;
}

static int ompi_crcp_bkmrk_pml_start_drain_irecv_init(ompi_request_t **request, bool *found_drain)
{
    int ret, exit_status = OMPI_SUCCESS;
    ompi_crcp_bkmrk_pml_peer_ref_t            *peer_ref = NULL;
    ompi_crcp_bkmrk_pml_traffic_message_ref_t *msg_ref  = NULL;
    ompi_crcp_bkmrk_pml_drain_message_ref_t   *drain_msg_ref = NULL;
    ompi_crcp_bkmrk_pml_message_content_ref_t *content_ref = NULL;
    mca_pml_base_request_t *breq = NULL;
    size_t tmp_ddt_size  = 0;

    *found_drain = false;

    breq = (mca_pml_base_request_t *)(*request);
    ompi_datatype_type_size(breq->req_datatype, &tmp_ddt_size);

    /*
     * If peer rank is given then find the peer reference
     */
    if( 0 <= breq->req_peer ) {
        if( OMPI_SUCCESS != (ret = find_peer_in_comm(breq->req_comm,
                                                     breq->req_peer,
                                                     &peer_ref) ) ){
            opal_output(mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: pml_start(): Failed to find peer_ref\n");
            exit_status = ret;
            goto DONE;
        }
                    
        if( OMPI_SUCCESS != (ret = traffic_message_find(&(peer_ref->recv_init_list),
                                                        breq->req_count,
                                                        breq->req_tag,
                                                        breq->req_peer,
                                                        breq->req_comm->c_contextid,
                                                        tmp_ddt_size,
                                                        &msg_ref,
                                                        PERSIST_MARKER
                                                        ) ) ) {
            opal_output(mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: pml_start(): Unable to find the proper (recv) message ref for this recv\n");
            exit_status = ret;
            goto DONE;
        }
    }
    /*
     * Otherwise peer is not known
     */
    else {
        if( OMPI_SUCCESS != (ret = traffic_message_find(&(unknown_persist_recv_list),
                                                        breq->req_count,
                                                        breq->req_tag,
                                                        INVALID_INT,
                                                        breq->req_comm->c_contextid,
                                                        tmp_ddt_size,
                                                        &msg_ref,
                                                        PERSIST_MARKER
                                                        ) ) ) {
            opal_output(mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: pml_start(): Unable to find the proper (recv) message ref for this recv\n");
            exit_status = ret;
            goto DONE;
        }
    }

    /*
     * No message found :(
     */
    if( NULL == msg_ref ) {
        ERROR_SHOULD_NEVER_HAPPEN("crcp:bkmrk: pml_start(): Could not find message ref");
        exit_status = OMPI_ERROR;
        goto DONE;
    }

    /*
     * See if this mesage was already drained.
     */
    if( NULL != peer_ref ) {
        if( OMPI_SUCCESS != (ret = drain_message_find(&(peer_ref->drained_list),
                                                      msg_ref->count, msg_ref->tag, msg_ref->rank,
                                                      msg_ref->comm->c_contextid, msg_ref->ddt_size,
                                                      &drain_msg_ref,
                                                      &content_ref) ) ) {
            ERROR_SHOULD_NEVER_HAPPEN("crcp:bkmrk: pml_start(): Failed trying to find a drained message.");
            exit_status = ret;
            goto DONE;
        }
    } else {
        if( OMPI_SUCCESS != (ret = drain_message_find_any(msg_ref->count, msg_ref->tag, msg_ref->rank,
                                                          msg_ref->comm, msg_ref->ddt_size,
                                                          &drain_msg_ref,
                                                          &content_ref,
                                                          &peer_ref) ) ) {
            ERROR_SHOULD_NEVER_HAPPEN("crcp:bkmrk: pml_start(): Failed trying to find a drained message.");
            exit_status = ret;
            goto DONE;
        }
    }

    /*
     * Found a drained message!
     */
    if( NULL != drain_msg_ref ) {
        *found_drain = true;
        OPAL_OUTPUT_VERBOSE((12, mca_crcp_bkmrk_component.super.output_handle,
                             "crcp:bkmrk: pml_start(): Matched a drained message..."));

        if( OMPI_SUCCESS != (ret = drain_message_copy_remove_persistent(drain_msg_ref,
                                                                        content_ref,
                                                                        msg_ref,
                                                                        *request,
                                                                        peer_ref) ) ) {
            opal_output( mca_crcp_bkmrk_component.super.output_handle,
                         "crcp:bkmrk: pml_start(): Datatype copy failed (%d)",
                         ret);
        }

        peer_ref->total_drained_msgs -= 1;
    }

 DONE:
    return exit_status;
}

static int ompi_crcp_bkmrk_pml_start_irecv_init(ompi_request_t **request)
{
    int ret, exit_status = OMPI_SUCCESS;
    ompi_crcp_bkmrk_pml_peer_ref_t            *peer_ref = NULL;
    ompi_crcp_bkmrk_pml_traffic_message_ref_t *msg_ref  = NULL;
    mca_pml_base_request_t *breq = NULL;
    size_t tmp_ddt_size  = 0;

    breq = (mca_pml_base_request_t *)(*request);
    ompi_datatype_type_size(breq->req_datatype, &tmp_ddt_size);

    /*
     * If peer rank is given then find the peer reference
     */
    if( 0 <= breq->req_peer ) {
        if( OMPI_SUCCESS != (ret = find_peer_in_comm(breq->req_comm,
                                                     breq->req_peer,
                                                     &peer_ref) ) ){
            opal_output(mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: pml_start(): Failed to find peer_ref\n");
            exit_status = ret;
            goto DONE;
        }

        if( OMPI_SUCCESS != (ret = traffic_message_find(&(peer_ref->recv_init_list),
                                                        breq->req_count,
                                                        breq->req_tag,
                                                        breq->req_peer,
                                                        breq->req_comm->c_contextid,
                                                        tmp_ddt_size,
                                                        &msg_ref,
                                                        PERSIST_MARKER
                                                        ) ) ) {
            opal_output(mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: pml_start(): Unable to find the proper (recv) message ref for this recv\n");
            exit_status = ret;
            goto DONE;
        }

        if( NULL != msg_ref ) {
            traffic_message_start(msg_ref,
                                  peer_ref,
                                  request,
                                  &(peer_ref->recv_init_list),
                                  NULL);
        }
    }
    /*
     * Else peer is not known
     */
    else {
        if( OMPI_SUCCESS != (ret = traffic_message_find(&(unknown_persist_recv_list),
                                                        breq->req_count,
                                                        breq->req_tag,
                                                        INVALID_INT,
                                                        breq->req_comm->c_contextid,
                                                        tmp_ddt_size,
                                                        &msg_ref,
                                                        PERSIST_MARKER
                                                        ) ) ) {
            opal_output(mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: pml_start(): Unable to find the proper (recv) message ref for this recv\n");
            exit_status = ret;
            goto DONE;
        }

        if( NULL != msg_ref ) {
            traffic_message_start(msg_ref,
                                  NULL,
                                  request,
                                  &(unknown_persist_recv_list),
                                  NULL);
        }
    }

    if( NULL == msg_ref ) {
        ERROR_SHOULD_NEVER_HAPPEN("crcp:bkmrk: pml_start(): Could not find message ref");
        exit_status = OMPI_ERROR;
        goto DONE;
    }

 DONE:
    return exit_status;
}

static int ompi_crcp_bkmrk_request_complete_irecv_init(struct ompi_request_t *request, 
                                               ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                                               int src, int tag, int tmp_ddt_size)
{
    int ret, exit_status = OMPI_SUCCESS;
    mca_pml_base_request_t *breq = NULL;
    ompi_crcp_bkmrk_pml_traffic_message_ref_t *msg_ref  = NULL, *new_msg_ref = NULL;
    ompi_crcp_bkmrk_pml_message_content_ref_t *content_ref = NULL;

    breq = (mca_pml_base_request_t *)request;

    /*
     * Check the irecv_init list
     */
    if( OMPI_SUCCESS != (ret = traffic_message_find(&(peer_ref->recv_init_list),
                                                    breq->req_count,
                                                    tag, src,
                                                    breq->req_comm->c_contextid,
                                                    tmp_ddt_size,
                                                    &msg_ref,
                                                    FIND_MSG_TRUE
                                                    ) ) ) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: req_complete: Unable to find the proper (recv_init) message ref for this complete\n");
        exit_status = ret;
        goto DONE;
    }

    /*
     * If not found, check the unknown_irecv_list
     */
    if( NULL == msg_ref ) {
        if( OMPI_SUCCESS != (ret = traffic_message_find(&(unknown_persist_recv_list),
                                                        breq->req_count,
                                                        tag,
                                                        INVALID_INT,
                                                        breq->req_comm->c_contextid,
                                                        tmp_ddt_size,
                                                        &msg_ref,
                                                        FIND_MSG_TRUE
                                                        ) ) ) {
            opal_output(mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: requ_complete: Unable to find the proper (recv_init) message ref for this complete\n");
            exit_status = ret;
            goto DONE;
        }

        if( NULL != msg_ref ) {
            traffic_message_move(msg_ref,
                                 COORD_MSG_TYPE_P_RECV,
                                 NULL, &(unknown_persist_recv_list),
                                 peer_ref, &(peer_ref->recv_init_list),
                                 &new_msg_ref,
                                 true,
                                 false);
            msg_ref = new_msg_ref;
        }
    }

    /*
     * If still not found, then we must have completed this already
     */
    if( NULL == msg_ref ) {
        /*
         * It is possible that we did not 'find' the message because 
         * we could have previously marked it as done. Due to the
         * logic in the Request Wait/Test routines we could
         * receive multiple request complete calls for the
         * same request.
         *
         * It is possible that we have 'completed' this message previously,
         *  so this case can occur during normal operation.
         * This is caused by us checking for completeness twice in ompi_request_wait_all.
         */
        OPAL_OUTPUT_VERBOSE((15, mca_crcp_bkmrk_component.super.output_handle,
                            "crcp:bkmrk: request_complete: No match found for this request :( %d, %d ): [%d/%d,%d]\n",
                             peer_ref->total_msgs_sent, peer_ref->total_msgs_recvd,
                             breq->req_peer, src, breq->req_comm->c_contextid));
        exit_status = OMPI_SUCCESS;
        goto DONE;
    }

    /*
     * Mark request as inactive
     * Only increment the total count if this was not accounted for in the last checkpoint
     */
    traffic_message_find_mark_persistent(msg_ref, &request,
                                         true,  /* Find currently active */
                                         false, /* Mark as inactive */
                                         &content_ref);
    if( NULL == content_ref ) {
        exit_status = ORTE_ERROR;
        goto DONE;
    }

    if( !content_ref->already_drained ) {
        peer_ref->total_msgs_recvd += 1;
        msg_ref->done++;
        msg_ref->active--;
    } else {
        msg_ref->active_drain--;
        content_ref->already_drained = false;
    }

    /* Do not return the content_ref, persistent sends re-use these */

    if( NULL == new_msg_ref ) {
        TRAFFIC_MSG_DUMP_PEER(15, (peer_ref, "-- Request Complete (Recv_Init) --", true));
    } else {
        TRAFFIC_MSG_DUMP_PEER(15, (peer_ref, "-- Request Complete (Recv_init - Unknown) --", true));
    }

    OPAL_OUTPUT_VERBOSE((25, mca_crcp_bkmrk_component.super.output_handle,
                         "crcp:bkmrk: req_complete: Marked Message... ( %d, %d )\n",
                         peer_ref->total_msgs_sent, peer_ref->total_msgs_recvd));
 DONE:
    return exit_status;
}

ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_irecv(
                                  void *buf, size_t count,
                                  ompi_datatype_t *datatype,
                                  int src, int tag,
                                  struct ompi_communicator_t* comm,
                                  struct ompi_request_t **request,
                                  ompi_crcp_base_pml_state_t* pml_state )
{
    int ret, exit_status = OMPI_SUCCESS;
    ompi_crcp_bkmrk_pml_peer_ref_t    *peer_ref      = NULL;
    ompi_crcp_bkmrk_pml_traffic_message_ref_t *msg_ref       = NULL;
    ompi_crcp_bkmrk_pml_state_t       *coord_state   = NULL;
    bool found_drain = false;

    OPAL_OUTPUT_VERBOSE((30, mca_crcp_bkmrk_component.super.output_handle,
                         "crcp:bkmrk: pml_irecv()"));

    /*
     * Before PML Call
     * - Determine if this can be satisfied from the drained list
     * - Otherwise create a new reference to it so we can track it
     */
    if( OMPI_CRCP_PML_PRE == pml_state->state) {
        /*
         * Check to see if this message is in the drained message list
         */
        found_drain = false;
        if( OMPI_SUCCESS != (ret = drain_message_check_recv(buf, count, datatype,
                                                            &src, &tag, comm, request, NULL,
                                                            &found_drain) ) ) {
            ERROR_SHOULD_NEVER_HAPPEN("crcp:bkmrk: pml_recv(): Failed trying to find a drained message.");
            exit_status = ret;
            goto DONE;
        }

        if( found_drain ) {
            /* Do *not* increment:
             *    peer_ref->total_msgs_recvd += 1;
             * Because we accounted for this message during the last checkpoint.
             */

            /* This will identify to the wrapper that this message is complete */
            pml_state->state = OMPI_CRCP_PML_DONE;
            pml_state->error_code = OMPI_SUCCESS;
            return pml_state;
        }
        /*
         * Otherwise the message is not drained (common case)
         */
        else {
            /*
             * Find the Peer
             */
            if( MPI_ANY_SOURCE == src || src < 0) {
                /*
                 * Archive the message Message Object
                 */
                traffic_message_append(NULL, &(unknown_recv_from_list),
                                       COORD_MSG_TYPE_I_RECV,
                                       count, datatype, 0, tag, src, comm,
                                       &msg_ref);

                CREATE_COORD_STATE(coord_state, pml_state,
                                  NULL, msg_ref);
            }
            else {
                if( OMPI_SUCCESS != (ret = find_peer_in_comm(comm, src, &peer_ref) ) ){
                    opal_output(mca_crcp_bkmrk_component.super.output_handle,
                                "crcp:bkmrk: pml_irecv(): Failed to find peer_ref\n");
                    exit_status = ret;
                    goto DONE;
                }

                /*
                 * Archive the message Message Object
                 */
                traffic_message_append(peer_ref, &(peer_ref->irecv_list),
                                       COORD_MSG_TYPE_I_RECV,
                                       count, datatype, 0, tag, src, comm,
                                       &msg_ref);

                CREATE_COORD_STATE(coord_state, pml_state,
                                   peer_ref, msg_ref);
            }

            coord_state->p_super.error_code = OMPI_SUCCESS;
            return &coord_state->p_super;
        }
    }
    /*
     * Post PML Call
     * - bookkeeping...
     */
    else if( OMPI_CRCP_PML_POST == pml_state->state) {
        ompi_crcp_base_pml_state_t *rtn_state = NULL;
        ompi_crcp_bkmrk_pml_message_content_ref_t *new_content = NULL;

        EXTRACT_COORD_STATE(pml_state, coord_state, rtn_state,
                            peer_ref,  msg_ref);

        /* 
         * Do the update
         */
        HOKE_CONTENT_REF_ALLOC(new_content, ret);
        new_content->buffer  =  NULL; /* No tracked */
        new_content->request = *request;
        new_content->done    =  false;
        new_content->active  =  true;
        new_content->already_posted  = true;
        new_content->already_drained = false;
        OBJ_RETAIN(*request);
        opal_list_append(&(msg_ref->msg_contents), &(new_content->super) );

        TRAFFIC_MSG_DUMP_PEER(15, (peer_ref, "-- Append Message (irecv) --", true));

        CRCP_COORD_STATE_RETURN(coord_state);

        rtn_state->error_code = OMPI_SUCCESS;
        return rtn_state;
    }

 DONE:
    pml_state->error_code = exit_status;
    return pml_state;
}

static int ompi_crcp_bkmrk_request_complete_irecv(struct ompi_request_t *request, 
                                          ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                                          int src, int tag, int tmp_ddt_size)
{
    int ret, exit_status = OMPI_SUCCESS;
    mca_pml_base_request_t *breq = NULL;
    ompi_crcp_bkmrk_pml_traffic_message_ref_t *msg_ref  = NULL, *new_msg_ref = NULL;
    ompi_crcp_bkmrk_pml_message_content_ref_t *content_ref = NULL;

    breq = (mca_pml_base_request_t *)request;

    /*
     * Check the irecv list
     */
    if( OMPI_SUCCESS != (ret = traffic_message_find(&(peer_ref->irecv_list),
                                                    breq->req_count,
                                                    tag, src,
                                                    breq->req_comm->c_contextid,
                                                    tmp_ddt_size,
                                                    &msg_ref,
                                                    FIND_MSG_TRUE
                                                    ) ) ) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: req_complete: Unable to find the proper (irecv) message ref for this complete\n");
        exit_status = ret;
        goto DONE;
    }

    /*
     * If not found, try the unknown_irecv_list
     */
    if( NULL == msg_ref ) {
        if( OMPI_SUCCESS != (ret = traffic_message_find(&(unknown_recv_from_list),
                                                        breq->req_count,
                                                        tag,
                                                        INVALID_INT,
                                                        breq->req_comm->c_contextid,
                                                        tmp_ddt_size,
                                                        &msg_ref,
                                                        FIND_MSG_TRUE
                                                        ) ) ) {
            opal_output(mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: req_complete: Unable to find the proper (recv_init) message ref for this complete\n");
            exit_status = ret;
            goto DONE;
        }

        if( NULL != msg_ref ) {
            traffic_message_move(msg_ref,
                                 COORD_MSG_TYPE_I_RECV,
                                 NULL, &(unknown_recv_from_list),
                                 peer_ref, &(peer_ref->irecv_list),
                                 &new_msg_ref,
                                 true,
                                 true);
            msg_ref = new_msg_ref;
        }
    }

    /*
     * If still not found, then must have completed this twice
     */
    if( NULL == msg_ref ) {
        /*
         * It is possible that we did not 'find' the message because 
         * we could have previously marked it as done. Due to the
         * logic in the Request Wait/Test routines we could
         * receive multiple request complete calls for the
         * same request.
         *
         * It is possible that we have 'completed' this message previously,
         *  so this case can occur during normal operation.
         * This is caused by us checking for completeness twice in ompi_request_wait_all.
         */
        OPAL_OUTPUT_VERBOSE((15, mca_crcp_bkmrk_component.super.output_handle,
                            "crcp:bkmrk: request_complete: No match found for this request :( %d, %d ): [%d/%d,%d]\n",
                             peer_ref->total_msgs_sent, peer_ref->total_msgs_recvd,
                             breq->req_peer, src, breq->req_comm->c_contextid));
        exit_status = OMPI_SUCCESS;
        goto DONE;
    }

    /* Strip off an irecv request
     * Only increment the total count if this was not accounted for in the last checkpoint
     */
    traffic_message_grab_content(msg_ref, &content_ref, true, true); /* Remove, prefer already_drained */

    if( !content_ref->already_drained ) {
        peer_ref->total_msgs_recvd += 1;
        msg_ref->done++;
        msg_ref->active--;
    } else {
        msg_ref->active_drain--;
        content_ref->already_drained = false;
    }

    HOKE_CONTENT_REF_RETURN(content_ref);

    if( NULL == new_msg_ref ) {
        TRAFFIC_MSG_DUMP_PEER(15, (peer_ref, "-- Request Complete (iRecv) --", true));
    } else {
        TRAFFIC_MSG_DUMP_PEER(15, (peer_ref, "-- Request Complete (iRecv - Unknown) --", true));
    }

    OPAL_OUTPUT_VERBOSE((15, mca_crcp_bkmrk_component.super.output_handle,
                         "crcp:bkmrk: req_complete: Matched an iRecv: total = %d",
                         peer_ref->total_msgs_recvd));

 DONE:
    return exit_status;
}

ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_recv(
                                  void *buf, size_t count,
                                  ompi_datatype_t *datatype,
                                  int src, int tag,
                                  struct ompi_communicator_t* comm,
                                  ompi_status_public_t* status, 
                                  ompi_crcp_base_pml_state_t* pml_state)
{
    ompi_crcp_bkmrk_pml_peer_ref_t    *peer_ref      = NULL;
    ompi_crcp_bkmrk_pml_traffic_message_ref_t *msg_ref       = NULL, *new_msg_ref = NULL;
    ompi_crcp_bkmrk_pml_state_t       *coord_state   = NULL;
    bool found_drain = false;
    int exit_status = OMPI_SUCCESS;
    int ret;

    OPAL_OUTPUT_VERBOSE((30, mca_crcp_bkmrk_component.super.output_handle,
                         "crcp:bkmrk: pml_recv()"));

    /*
     * Before PML Call
     * - Determine if this can be satisfied from the drained list
     * - Otherwise create a new reference to it so we can track it
     */
    if( OMPI_CRCP_PML_PRE == pml_state->state) {
        /*
         * Check to see if this message is in the drained message list
         */
        found_drain = false;
        if( OMPI_SUCCESS != (ret = drain_message_check_recv(buf, count, datatype,
                                                            &src, &tag, comm, NULL, &status,
                                                            &found_drain) ) ) {
            ERROR_SHOULD_NEVER_HAPPEN("crcp:bkmrk: pml_recv(): Failed trying to find a drained message.");
            exit_status = ret;
            goto DONE;
        }

        if( found_drain ) {
            /* Do *not* increment:
             *    peer_ref->total_msgs_recvd += 1;
             * Because we accounted for this message during the last checkpoint.
             */

            /* This will identify to the wrapper that this message is complete */
            pml_state->state = OMPI_CRCP_PML_DONE;
            pml_state->error_code = OMPI_SUCCESS;
            return pml_state;
        }
        /*
         * Otherwise the message is not drained (common case)
         */
        else {
            /*
             * Find the Peer
             */
            if( MPI_ANY_SOURCE == src || src < 0) {
                traffic_message_append(NULL, &(unknown_recv_from_list),
                                       COORD_MSG_TYPE_B_RECV,
                                       count, datatype, 0, tag, src, comm,
                                       &msg_ref);

                CREATE_COORD_STATE(coord_state, pml_state,
                                   NULL, msg_ref);
            }
            else {
                if( OMPI_SUCCESS != (ret = find_peer_in_comm(comm, src, &peer_ref) ) ){
                    opal_output(mca_crcp_bkmrk_component.super.output_handle,
                                "crcp:bkmrk: pml_recv(): Failed to find peer_ref\n");
                    exit_status = ret;
                    goto DONE;
                }

                traffic_message_append(peer_ref, &(peer_ref->recv_list),
                                       COORD_MSG_TYPE_B_RECV,
                                       count, datatype, 0, tag, src, comm,
                                       &msg_ref);

                CREATE_COORD_STATE(coord_state, pml_state,
                                   peer_ref, msg_ref);
            }

            /*  Bookkeeping */
            current_msg_id = msg_ref->msg_id;
            current_msg_type = COORD_MSG_TYPE_B_RECV;

            coord_state->p_super.error_code = OMPI_SUCCESS;
            return &coord_state->p_super;
        }
    }
    /*
     * Post PML Call
     * - bookkeeping...
     */
    else if( OMPI_CRCP_PML_POST == pml_state->state) {
        ompi_crcp_base_pml_state_t *rtn_state = NULL;

        EXTRACT_COORD_STATE(pml_state, coord_state, rtn_state,
                            peer_ref,  msg_ref);

        /*
         * If MPI_ANY_SOUCE, then move the message from the unknown list
         * to the list associated with the resolved process.
         */
        if( NULL == peer_ref ) {
            src = status->MPI_SOURCE;

            if( OMPI_SUCCESS != (ret = find_peer_in_comm(comm, src, &peer_ref) ) ){
                opal_output(mca_crcp_bkmrk_component.super.output_handle,
                            "crcp:bkmrk: pml_recv(): Failed to resolve peer_ref (rank %d)\n",
                            src);
                exit_status = ret;
                goto DONE;
            }

            traffic_message_move(msg_ref,
                                 COORD_MSG_TYPE_B_RECV,
                                 NULL, &(unknown_recv_from_list),
                                 peer_ref, &(peer_ref->recv_list),
                                 &new_msg_ref,
                                 false,
                                 true);
            new_msg_ref->done++;
            new_msg_ref->active--;
        } else {
            /* 
             * Do the update
             */
            msg_ref->done++;
            msg_ref->active--;
        }

        peer_ref->total_msgs_recvd += 1;
        current_msg_id = 0;
        current_msg_type = COORD_MSG_TYPE_UNKNOWN;

        TRAFFIC_MSG_DUMP_PEER(15, (peer_ref, "Recv Done", true));

        CRCP_COORD_STATE_RETURN(coord_state);

        rtn_state->error_code = OMPI_SUCCESS;
        return rtn_state;
    }

 DONE:
    pml_state->error_code = exit_status;
    return pml_state;
}


/**************** Start *****************/
/* Start is connected to irecv_start or isend_start */
static ompi_request_type_t * coord_start_req_types = NULL;

ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_start(
                                  size_t count,
                                  ompi_request_t** requests,
                                  ompi_crcp_base_pml_state_t* pml_state )
{
    int ret, exit_status = OMPI_SUCCESS;
    mca_pml_base_request_t *breq = NULL;
    size_t tmp_ddt_size  = 0;
    size_t iter_req;
    bool found_drain = false;

    OPAL_OUTPUT_VERBOSE((30, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: pml_start()"));

    /*
     * Handle all start() on send requests
     */
    if( OMPI_CRCP_PML_POST == pml_state->state ) {
        for(iter_req = 0; iter_req < count; iter_req++) {
            breq = (mca_pml_base_request_t *)requests[iter_req];
            if(breq->req_type == MCA_PML_REQUEST_SEND ) {
                if( OMPI_SUCCESS != (ret = ompi_crcp_bkmrk_pml_start_isend_init(&(requests[iter_req]))) ) {
                    exit_status = ret;
                    goto DONE;
                }
            }
        }
    }

    /*
     * Handle all start() on recv requests
     * - Pre:  Check drain queue for a match
     * - Post: Start the message, unless drained
     */
    if( OMPI_CRCP_PML_PRE == pml_state->state ) {
        /*
         * Mark all saved requests as NOOP
         */
        coord_start_req_types = (ompi_request_type_t *)malloc(sizeof(ompi_request_type_t) * count);
        for(iter_req = 0; iter_req < count; iter_req++) {
            coord_start_req_types[iter_req] = OMPI_REQUEST_NOOP;
        }

        for(iter_req = 0; iter_req < count; iter_req++) {
            breq = (mca_pml_base_request_t *)requests[iter_req];
            ompi_datatype_type_size(breq->req_datatype, &tmp_ddt_size);

            if( breq->req_type == MCA_PML_REQUEST_RECV ) {
                found_drain = false;
                if( OMPI_SUCCESS != (ret = ompi_crcp_bkmrk_pml_start_drain_irecv_init(&(requests[iter_req]), &found_drain)) ) {
                    exit_status = ret;
                    goto DONE;
                }

                if( found_drain ) {
                    coord_start_req_types[iter_req] = requests[iter_req]->req_type;
                    requests[iter_req]->req_type = OMPI_REQUEST_NOOP;
                    requests[iter_req]->req_complete = true;
                }
            }
        }
        goto DONE;
    }
    else if( OMPI_CRCP_PML_POST == pml_state->state) {
        for(iter_req = 0; iter_req < count; iter_req++) {
            breq = (mca_pml_base_request_t *)requests[iter_req];
            ompi_datatype_type_size(breq->req_datatype, &tmp_ddt_size);

            if (breq->req_type == MCA_PML_REQUEST_RECV) {
                /*
                 * If this was a drained message it will have it's type set to
                 * OMPI_REQUEST_NOOP so the PML does not try to start it again. 
                 * So we need to replace it with the original type, but can
                 * skip starting it.
                 */
                if( NULL != coord_start_req_types ) {
                    if( OMPI_REQUEST_NOOP != coord_start_req_types[iter_req] ) {
                        requests[iter_req]->req_type = coord_start_req_types[iter_req];
                        continue;
                    }
                }

                if( OMPI_SUCCESS != (ret = ompi_crcp_bkmrk_pml_start_irecv_init(&(requests[iter_req]))) ) {
                    exit_status = ret;
                    goto DONE;
                }
            }
        }

        /*
         * Clear out the temporary drain type structure.
         */
        if( NULL != coord_start_req_types ) {
            free(coord_start_req_types);
            coord_start_req_types = NULL;
        }
    }

 DONE:
    pml_state->error_code = exit_status;
    return pml_state;
}

/**************** Request Completed ********/
int ompi_crcp_bkmrk_request_complete(struct ompi_request_t *request)
{
    int ret, exit_status = OMPI_SUCCESS;
    ompi_crcp_bkmrk_pml_peer_ref_t    *peer_ref = NULL;
    mca_pml_base_request_t *breq;
    size_t tmp_ddt_size  = 0;
    int src, tag;

    OPAL_OUTPUT_VERBOSE((30, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: pml_request_complete()"));

    /*
     * Extract & check the PML version of the request
     */
    breq = (mca_pml_base_request_t *)request;

    if( (breq->req_type   != MCA_PML_REQUEST_SEND &&
         breq->req_type   != MCA_PML_REQUEST_RECV ) || /* JJH YYY -- req_state = OMPI_REQUEST_INACTIVE ??? */
        request->req_type == OMPI_REQUEST_NOOP ||
        request->req_type == OMPI_REQUEST_NULL) {
        exit_status = OMPI_SUCCESS;
        goto DONE;
    }

    /* Extract source/tag/ddt_size */
    src = breq->req_peer;
    tag = breq->req_tag;
    ompi_datatype_type_size(breq->req_datatype, &tmp_ddt_size);

    /*
     * Find the peer reference
     */
    if( MPI_ANY_SOURCE == src ) {
        if( OMPI_SUCCESS != (ret = find_peer_in_comm(breq->req_comm, request->req_status.MPI_SOURCE, &peer_ref) ) ){
            opal_output(mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: req_complete(): Failed to find peer_ref\n");
            exit_status = ret;
            goto DONE;
        }
    } else {
        if( OMPI_SUCCESS != (ret = find_peer_in_comm(breq->req_comm, src,  &peer_ref) ) ){
            opal_output(mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: req_complete(): Failed to find peer_ref\n");
            exit_status = ret;
            goto DONE;
        }
    }

    /*******************************
     * A send request is completing
     ******************************/
    if(breq->req_type == MCA_PML_REQUEST_SEND ) {
        /*
         * ISEND Case:
         */
        if( false == request->req_persistent ) {
            if( OMPI_SUCCESS != (ret = ompi_crcp_bkmrk_request_complete_isend(request, peer_ref,
                                                                             src, tag, tmp_ddt_size) ) ) {
                exit_status = ret;
                goto DONE;
            }
        }
        /*
         * SEND_INIT/START Case
         */
        else {
            if( OMPI_SUCCESS != (ret = ompi_crcp_bkmrk_request_complete_isend_init(request, peer_ref,
                                                                                  src, tag, tmp_ddt_size) ) ) {
                exit_status = ret;
                goto DONE;
            }
        }
    }
    /***********************************
     * A receive request is completing
     ***********************************/
    else if(breq->req_type == MCA_PML_REQUEST_RECV) {
        /*
         * IRECV Case:
         */
        if( false == request->req_persistent ) {
            if( OMPI_SUCCESS != (ret = ompi_crcp_bkmrk_request_complete_irecv(request, peer_ref,
                                                                             src, tag, tmp_ddt_size) ) ) {
                exit_status = ret;
                goto DONE;
            }
        }
        /*
         * IRECV_INIT/START Case:
         */
        else {
            if( OMPI_SUCCESS != (ret = ompi_crcp_bkmrk_request_complete_irecv_init(request, peer_ref,
                                                                                  src, tag, tmp_ddt_size) ) ) {
                exit_status = ret;
                goto DONE;
            }
        }
    }

 DONE:
    return exit_status;
}

/**************** FT Event *****************/
ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_ft_event(
                                  int state, 
                                  ompi_crcp_base_pml_state_t* pml_state)
{
    static int step_to_return_to = 0;
    static bool first_continue_pass = false;
    opal_list_item_t* item = NULL;
    int exit_status = OMPI_SUCCESS;
    int ret;

    ft_event_state = state;

    if( step_to_return_to == 1 ) {
        goto STEP_1;
    }

    OPAL_OUTPUT_VERBOSE((30, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: pml_ft_event()"));

    /**************************
     * Prepare for a Checkpoint
     **************************/
    if(OPAL_CRS_CHECKPOINT == state) {
        if( OMPI_CRCP_PML_PRE != pml_state->state){
            goto DONE;
        }

        if( opal_cr_timing_barrier_enabled ) {
            OPAL_CR_SET_TIMER(OPAL_CR_TIMER_CRCPBR0);
            orte_grpcomm.barrier();
        }
        OPAL_CR_SET_TIMER(OPAL_CR_TIMER_CRCP0);

        START_TIMER(CRCP_TIMER_TOTAL_CKPT);
    STEP_1:
        step_to_return_to = 0;

        /* Coordinate Peers:
         * When we return from this function we know that all of our
         * channels have been flushed.
         */
        if( OMPI_SUCCESS != (ret = ft_event_coordinate_peers()) ) {
            opal_output(mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: %s ft_event: Checkpoint Coordination Failed %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ret);
            exit_status = ret;
            goto DONE;
        }

        if( stall_for_completion ) {
            stall_for_completion = false;
            opal_cr_stall_check  = true;
            step_to_return_to    = 1;

            exit_status = OMPI_EXISTS;
            goto DONE_STALL;
        }
        END_TIMER(CRCP_TIMER_TOTAL_CKPT);

        DISPLAY_ALL_TIMERS(state);
        clear_timers();
    }
    /*****************************
     * Continue after a checkpoint
     ******************************/
    else if(OPAL_CRS_CONTINUE == state) {
        if( OMPI_CRCP_PML_POST != pml_state->state){
            goto DONE;
        }

        first_continue_pass = !first_continue_pass;

        /* Only finalize the Protocol after the PML has been rebuilt */
        if( ompi_cr_continue_like_restart && first_continue_pass ) {
            goto DONE;
        }

        START_TIMER(CRCP_TIMER_TOTAL_CONT);

        /*
         * Finish the coord protocol
         */
        if( OMPI_SUCCESS != (ret = ft_event_finalize_exchange() ) ) {
            opal_output(mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: pml_ft_event: Checkpoint Finalization Failed %d",
                        ret);
            exit_status = ret;
            goto DONE;
        }
        END_TIMER(CRCP_TIMER_TOTAL_CONT);

        DISPLAY_ALL_TIMERS(state);
        clear_timers();

        if( opal_cr_timing_barrier_enabled ) {
            OPAL_CR_SET_TIMER(OPAL_CR_TIMER_COREBR1);
            orte_grpcomm.barrier();
        }
        OPAL_CR_SET_TIMER(OPAL_CR_TIMER_CORE2);
    }
    /*****************************
     * Restart from a checkpoint
     *****************************/
    else if(OPAL_CRS_RESTART == state) {
        if( OMPI_CRCP_PML_POST != pml_state->state){
            goto DONE;
        }

        START_TIMER(CRCP_TIMER_TOTAL_RST);
        /*
         * Refresh the jobids
         */
        for(item  = opal_list_get_first(&ompi_crcp_bkmrk_pml_peer_refs);
            item != opal_list_get_end(&ompi_crcp_bkmrk_pml_peer_refs);
            item  = opal_list_get_next(item) ) {
            ompi_crcp_bkmrk_pml_peer_ref_t *cur_peer_ref;
            cur_peer_ref = (ompi_crcp_bkmrk_pml_peer_ref_t*)item;

            /* JJH - Assuming only one global jobid at the moment */
            cur_peer_ref->proc_name.jobid = ORTE_PROC_MY_NAME->jobid;
        }

        /*
         * Finish the coord protocol
         */
        if( OMPI_SUCCESS != (ret = ft_event_finalize_exchange() ) ) {
            opal_output(mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: pml_ft_event: Checkpoint Finalization Failed %d",
                        ret);
            exit_status = ret;
            goto DONE;
        }

        END_TIMER(CRCP_TIMER_TOTAL_RST);

        DISPLAY_ALL_TIMERS(state);
        clear_timers();
    }
    /*****************************
     * Terminating the process post checkpoint
     *****************************/
    else if(OPAL_CRS_TERM == state ) {
        goto DONE;
    }
    /****************************
     * Reached an error
     ****************************/
    else {
        goto DONE;
    }

 DONE:
    step_to_return_to = 0;
    ft_event_state = OPAL_CRS_RUNNING;

 DONE_STALL:
    pml_state->error_code = exit_status;
    return pml_state;
}

/******************
 * Local Utility functions
 ******************/

/************************************************
 * Traffic Message Utility Functions
 ************************************************/
static int traffic_message_append(ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                                  opal_list_t * append_list,
                                  ompi_crcp_bkmrk_pml_message_type_t msg_type,
                                  size_t count,
                                  ompi_datatype_t *datatype,
                                  size_t in_ddt_size,
                                  int tag,
                                  int dest,
                                  struct ompi_communicator_t* comm,
                                  ompi_crcp_bkmrk_pml_traffic_message_ref_t **msg_ref)
{
    int ret, exit_status = ORTE_SUCCESS;
    size_t ddt_size = 0;

    if( NULL != datatype ) {
        ompi_datatype_type_size(datatype,
                           &ddt_size);
    } else {
        ddt_size = in_ddt_size;
        /* ddt_size = 0; */
    }

    /*
     * Determine if message is currently in the list
     *  - If it is then increment the count.
     *  - ow add it to the list
     */
    if( OMPI_SUCCESS != (ret = traffic_message_find(append_list,
                                                    count, tag, dest,
                                                    comm->c_contextid,
                                                    ddt_size,
                                                    msg_ref,
                                                    FIND_MSG_UNKNOWN  /* Active?         */
                                                    ) ) ) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: traffic_message_append: Unable to find the proper message reference.\n");
        return OMPI_ERROR;
    }

    if( NULL != *msg_ref ) {
        if( msg_type == COORD_MSG_TYPE_P_SEND ||
            msg_type == COORD_MSG_TYPE_P_RECV ) {
            (*msg_ref)->posted++;
        } else {
            (*msg_ref)->active++;
        }
    } else {
        if( NULL != peer_ref ) {
            CREATE_NEW_MSG((*msg_ref), msg_type,
                           count, ddt_size, tag, dest, comm,
                           peer_ref->proc_name.jobid,
                           peer_ref->proc_name.vpid);
        } else {
            CREATE_NEW_MSG((*msg_ref), msg_type,
                           count, ddt_size, tag, dest, comm,
                           ORTE_JOBID_INVALID,
                           ORTE_VPID_INVALID);
        }

        if( msg_type == COORD_MSG_TYPE_P_SEND ||
            msg_type == COORD_MSG_TYPE_P_RECV ) {
            (*msg_ref)->matched        = 0;
            (*msg_ref)->done           = 0;
            (*msg_ref)->active         = 0;
            (*msg_ref)->posted         = 1;
        } else {
            (*msg_ref)->matched        = 0;
            (*msg_ref)->done           = 0;
            (*msg_ref)->active         = 1;
            (*msg_ref)->posted         = 0;
        }

        opal_list_append(append_list, &((*msg_ref)->super));
    }

    if( NULL != peer_ref ) {
        if( msg_type == COORD_MSG_TYPE_B_SEND ) {
            TRAFFIC_MSG_DUMP_PEER(15, (peer_ref, "-- Append Message (send)      --", true));
        }
        else if( msg_type == COORD_MSG_TYPE_P_SEND ) {
            TRAFFIC_MSG_DUMP_PEER(15, (peer_ref, "-- Append Message (send_init) --", true));
        }
        else if( msg_type == COORD_MSG_TYPE_B_RECV ) {
            TRAFFIC_MSG_DUMP_PEER(15, (peer_ref, "-- Append Message (recv)      --", true));
        }
        else if( msg_type == COORD_MSG_TYPE_P_RECV ) {
            TRAFFIC_MSG_DUMP_PEER(15, (peer_ref, "-- Append Message (recv_init) --", true));
        }
        else if( msg_type == COORD_MSG_TYPE_I_SEND || msg_type == COORD_MSG_TYPE_I_RECV ) {
            ;
        }
        else {
            TRAFFIC_MSG_DUMP_PEER(15, (peer_ref, "-- Append Message (Unknown)   --", true));
        }
    }

    return exit_status;
}

static int traffic_message_start(ompi_crcp_bkmrk_pml_traffic_message_ref_t *msg_ref,
                                 ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                                 ompi_request_t **request,
                                 opal_list_t * peer_list,
                                 ompi_crcp_bkmrk_pml_message_content_ref_t ** content_ref)
{
    /* This is only called by persistent calls.
     * This will mark the current message as having one more active member.
     * There is still only one posted message. */
    msg_ref->active++;

    traffic_message_find_mark_persistent(msg_ref, request,
                                         false,  /* Find currently not active */
                                         true, /* Mark as active */
                                         content_ref);
    return OMPI_SUCCESS;
}

static int traffic_message_move(ompi_crcp_bkmrk_pml_traffic_message_ref_t *old_msg_ref,
                                ompi_crcp_bkmrk_pml_message_type_t msg_type,
                                ompi_crcp_bkmrk_pml_peer_ref_t *from_peer_ref,
                                opal_list_t * from_list,
                                ompi_crcp_bkmrk_pml_peer_ref_t *to_peer_ref,
                                opal_list_t * to_list,
                                ompi_crcp_bkmrk_pml_traffic_message_ref_t **new_msg_ref,
                                bool keep_active,
                                bool remove)
{
    int ret, exit_status = ORTE_SUCCESS;
    ompi_crcp_bkmrk_pml_message_content_ref_t *new_content = NULL, *prev_content = NULL;
    ompi_request_t *request = NULL;
    bool loc_already_drained = false;

    /* Append to the to_peer_ref */
    if( COORD_MSG_TYPE_B_RECV != msg_type ) {
        traffic_message_grab_content(old_msg_ref, &prev_content, remove, true); /* Remove, prefer already_drained */
        request = prev_content->request;

        loc_already_drained = prev_content->already_drained;

        if( remove ) {
            prev_content->request = NULL;
            HOKE_CONTENT_REF_RETURN(prev_content);
        }
    }

    ret = traffic_message_append(to_peer_ref, to_list,
                                 old_msg_ref->msg_type,
                                 old_msg_ref->count,
                                 NULL,
                                 old_msg_ref->ddt_size,
                                 old_msg_ref->tag,
                                 old_msg_ref->rank,
                                 old_msg_ref->comm,
                                 new_msg_ref);

    if( loc_already_drained ) {
        old_msg_ref->active_drain--;
        (*new_msg_ref)->active--; /* Undo the action from _append() */
        (*new_msg_ref)->active_drain++;
    } else {
        /* 'remove' from from_peer_ref */
        old_msg_ref->active--;
    }

    if( msg_type == COORD_MSG_TYPE_P_SEND ||
        msg_type == COORD_MSG_TYPE_P_RECV ) {
        if( keep_active ) {
            (*new_msg_ref)->active++;
        }
    }

    if( COORD_MSG_TYPE_B_RECV != msg_type && NULL == request ) {
        ERROR_SHOULD_NEVER_HAPPEN("Error: Must match a non-blocking send, and there is no matching request.");
    }

    if( NULL != request ) {
        HOKE_CONTENT_REF_ALLOC(new_content, ret);
        new_content->buffer  =  NULL;
        new_content->request =  request;
        new_content->done    =  false;
        new_content->active  =  keep_active;
        new_content->already_posted  = true;
        new_content->already_drained = loc_already_drained;
        OBJ_RETAIN(request);
        opal_list_append(&((*new_msg_ref)->msg_contents), &(new_content->super) );
    }

    if( NULL == from_peer_ref && NULL != to_peer_ref ) {
        (*new_msg_ref)->proc_name.jobid = to_peer_ref->proc_name.jobid;
        (*new_msg_ref)->proc_name.vpid  = to_peer_ref->proc_name.vpid;
    }

    return exit_status;
}

static int traffic_message_find_mark_persistent(ompi_crcp_bkmrk_pml_traffic_message_ref_t *msg_ref,
                                                ompi_request_t **request,
                                                bool cur_active,
                                                bool set_is_active,
                                                ompi_crcp_bkmrk_pml_message_content_ref_t **c_ref)
{
    mca_pml_base_request_t * breq = NULL;
    opal_list_item_t* item = NULL;

    breq = (mca_pml_base_request_t *)(*request);

    for(item  = opal_list_get_first(&(msg_ref->msg_contents));
        item != opal_list_get_end(  &(msg_ref->msg_contents));
        item  = opal_list_get_next(item) ) {
        ompi_crcp_bkmrk_pml_message_content_ref_t *content_ref = NULL;
        mca_pml_base_request_t * loc_breq = NULL;

        content_ref = (ompi_crcp_bkmrk_pml_message_content_ref_t*)item;
        loc_breq    = (mca_pml_base_request_t *)(content_ref->request);

        if( content_ref->active != cur_active ) {
            continue;
        }
        else if( loc_breq->req_sequence == breq->req_sequence ) {
            OPAL_OUTPUT_VERBOSE((25, mca_crcp_bkmrk_component.super.output_handle,
                                 "%s %8s Request [%d] (%s) %d : %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 (set_is_active ? "Start" : (NULL != c_ref ? "Drain" : "Complete")),
                                 (int)msg_ref->msg_id,
                                 (content_ref->active ? "T" : "F"),
                                 (int)loc_breq->req_sequence,
                                 (int)breq->req_sequence));

            content_ref->active = set_is_active;
            if( NULL != c_ref ) {
                *c_ref = content_ref;
            }
            break;
        }
    }

    return OMPI_SUCCESS;
}

static int traffic_message_grab_content(ompi_crcp_bkmrk_pml_traffic_message_ref_t *msg_ref,
                                        ompi_crcp_bkmrk_pml_message_content_ref_t ** content_ref,
                                        bool remove,
                                        bool already_drained)
{
    ompi_crcp_bkmrk_pml_message_content_ref_t *new_content = NULL;
    ompi_crcp_bkmrk_pml_message_content_ref_t *loc_content_ref = NULL;
    opal_list_item_t* item = NULL;

    /*
     * If there is no request list, return NULL
     */
    if( 0 >= opal_list_get_size(&msg_ref->msg_contents) ) {
        return OMPI_SUCCESS;
    }

    /*
     * Otherwise look though the list, and grab something 'already_drained' if
     * possible, otherwise just get the first element.
     */
    if( already_drained ) {
        item  = opal_list_get_first(&(msg_ref->msg_contents));
        new_content = (ompi_crcp_bkmrk_pml_message_content_ref_t*)item;
    }

    for(item  = opal_list_get_first(&(msg_ref->msg_contents));
        item != opal_list_get_end(  &(msg_ref->msg_contents));
        item  = opal_list_get_next(item) ) {
        loc_content_ref = (ompi_crcp_bkmrk_pml_message_content_ref_t*)item;

        if( !already_drained ) {
            TRAFFIC_MSG_DUMP_MSG_CONTENT_INDV(10, (loc_content_ref));
        }

        if( loc_content_ref->already_drained == already_drained ) {
            new_content = (ompi_crcp_bkmrk_pml_message_content_ref_t*)item;
            break;
        }
    }

    if( remove ) {
        opal_list_remove_item(&msg_ref->msg_contents, &(new_content->super));
    }

    if( NULL != content_ref ) {
        *content_ref = new_content;
    } else if( remove && NULL != new_content ) {
        HOKE_CONTENT_REF_RETURN(new_content);
    }

    return OMPI_SUCCESS;
}

static int traffic_message_create_drain_message(bool post_drain,
                                                int max_post,
                                                ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                                                ompi_crcp_bkmrk_pml_traffic_message_ref_t ** posted_msg_ref,
                                                int *num_posted)
{
    ompi_crcp_bkmrk_pml_drain_message_ref_t *drain_msg_ref = NULL;
    ompi_crcp_bkmrk_pml_message_content_ref_t *new_content = NULL, *prev_content = NULL;
    int m_iter, m_total, ret;

    *num_posted = 0;

    /*
     * Nothing to do here
     */
    if( NULL == (*posted_msg_ref) || max_post <= 0) {
        return OMPI_SUCCESS;
    }

    /*
     * For each active message or if not active message then max_post, create a drain message
     */
    m_total = max_post;
    if( !post_drain && max_post > (*posted_msg_ref)->active ) {
        m_total = (*posted_msg_ref)->active;
    }

    OPAL_OUTPUT_VERBOSE((10, mca_crcp_bkmrk_component.super.output_handle,
                         "crcp:bkmrk: %s <-- %s "
                         " --> Create Drain Msg: %s %4d = min(%4d / %4d)",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                         (post_drain ? "Posting" : "Not Posting"),
                         m_total, (*posted_msg_ref)->active, max_post ));

    TRAFFIC_MSG_DUMP_MSG_INDV(10, ((*posted_msg_ref), "Drain", true));

    /*
     * Get a drained message reference for this signature.
     */
    drain_message_append(peer_ref,
                         COORD_MSG_TYPE_I_RECV,
                         (*posted_msg_ref)->count,
                         (*posted_msg_ref)->ddt_size,
                         (*posted_msg_ref)->tag,
                         (*posted_msg_ref)->rank,
                         (*posted_msg_ref)->comm,
                         &drain_msg_ref);

    /*
     * Create a new message content for each message to be drained.
     */
    for(m_iter = 0; m_iter < m_total; ++m_iter) {
        new_content = NULL;

        OPAL_OUTPUT_VERBOSE((10, mca_crcp_bkmrk_component.super.output_handle,
                             "crcp:bkmrk: %s <-- %s "
                             " \t--> Find Content: %s (%4d of %4d)",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                             (post_drain ? "Posting" : "Not Posting"),
                             m_iter, m_total));

        /* Grab a request if there are any
         * - if we are posting, then we created a dummy message which will not
         *   have any contents, so this is still valid.
         * - if we are not posting, and this is an iRecv, then we *must* find a content!
         */
        traffic_message_grab_content((*posted_msg_ref), &prev_content, false, false); /* Do not remove, No already drained */
        if( NULL != prev_content ) {
            prev_content->already_drained = true;
        }

        /* YYY JJH YYY - Is this needed? */
        if( !post_drain && (*posted_msg_ref)->msg_type != COORD_MSG_TYPE_B_RECV ) {
            assert( NULL != prev_content );
        }

        /* Decrementing active occurs when we stall in the Blocking Recv, do not do so here. */
        if( NULL != prev_content ) {
            (*posted_msg_ref)->active--;
        }
        (*posted_msg_ref)->active_drain++;

        /* Create a new content for the drained message */
        HOKE_CONTENT_REF_ALLOC(new_content, ret);
        new_content->buffer  = NULL;
        if( NULL == prev_content ) {
            new_content->request  = NULL;
        } else {
            new_content->request = prev_content->request;
            if( NULL != new_content->request ) {
                OBJ_RETAIN(new_content->request);
            }
        }
        opal_list_append(&(drain_msg_ref->msg_contents), &(new_content->super) );

        if( !post_drain ) {
            new_content->done            = false;
            new_content->active          = true;
            new_content->already_posted  = true;
            new_content->already_drained = true;

            drain_msg_ref->active++;
            drain_msg_ref->already_posted++;
        } else {
            new_content->done            = false;
            new_content->active          = false;
            new_content->already_posted  = false;
            new_content->already_drained = true;

            /*
             * Put the true count here so we can properly match the drain.
             * The post_drained() will properly handle the packed datatype
             * by changing the count to (count * ddt_size).
             */
            ompi_datatype_duplicate(&(ompi_mpi_packed.dt), &(drain_msg_ref->datatype));

            /* Create a buffer of the necessary type/size */
            if(drain_msg_ref->count > 0 ) {
                new_content->buffer = (void *) malloc(drain_msg_ref->count * drain_msg_ref->ddt_size);
            } else {
                new_content->buffer = (void *) malloc(1 * drain_msg_ref->ddt_size);
            }

            /* JJH - Performance Optimization? - Post drained messages right away? */
        }

        (*num_posted)++;
    }

    peer_ref->total_drained_msgs += *num_posted;

    OPAL_OUTPUT_VERBOSE((15, mca_crcp_bkmrk_component.super.output_handle,
                         "crcp:bkmrk: %s <-- %s "
                         "Added %d messages to the drained list (size = %d)",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                         (*num_posted),
                         (int)opal_list_get_size(&(peer_ref->drained_list)) ));

    return OMPI_SUCCESS;
}

static int traffic_message_find_recv(ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                             int rank, uint32_t comm_id, int tag,
                             size_t count, size_t datatype_size,
                             ompi_crcp_bkmrk_pml_traffic_message_ref_t ** posted_recv_msg_ref,
                             ompi_crcp_bkmrk_pml_traffic_message_ref_t ** posted_irecv_msg_ref,
                             ompi_crcp_bkmrk_pml_traffic_message_ref_t ** posted_precv_msg_ref,
                             ompi_crcp_bkmrk_pml_traffic_message_ref_t ** posted_unknown_recv_msg_ref,
                             ompi_crcp_bkmrk_pml_traffic_message_ref_t ** posted_unknown_precv_msg_ref)
{
    int ret;

    *posted_recv_msg_ref  = NULL;
    *posted_irecv_msg_ref = NULL;
    *posted_precv_msg_ref = NULL;
    *posted_unknown_recv_msg_ref  = NULL;
    *posted_unknown_precv_msg_ref = NULL;

    /*
     * Check the recv_list
     */
    if( OMPI_SUCCESS != (ret = traffic_message_find(&(peer_ref->recv_list),
                                                    count, tag, INVALID_INT,
                                                    comm_id, datatype_size,
                                                    posted_recv_msg_ref,
                                                    FIND_MSG_UNKNOWN) ) ) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: traffic_message_find_recv: Unable to find the proper message reference.\n");
        return OMPI_ERROR;
    }

    /*
     * Check the irecv_list
     */
    if( OMPI_SUCCESS != (ret = traffic_message_find(&(peer_ref->irecv_list),
                                                    count, tag, INVALID_INT,
                                                    comm_id, datatype_size,
                                                    posted_irecv_msg_ref,
                                                    FIND_MSG_UNKNOWN) ) ) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: traffic_message_find_recv: Unable to find the proper message reference.\n");
        return OMPI_ERROR;
    }

    /*
     * Check the recv_init_list
     */
    if( OMPI_SUCCESS != (ret = traffic_message_find(&(peer_ref->recv_init_list),
                                                    count, tag, INVALID_INT,
                                                    comm_id, datatype_size,
                                                    posted_precv_msg_ref,
                                                    FIND_MSG_UNKNOWN) ) ) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: traffic_message_find_recv: Unable to find the proper message reference.\n");
        return OMPI_ERROR;
    }

    /*
     * Check the unknown list of non-persistant
     */
    if( OMPI_SUCCESS != (ret = traffic_message_find(&(unknown_recv_from_list),
                                                    count, tag, INVALID_INT,
                                                    comm_id, datatype_size,
                                                    posted_unknown_recv_msg_ref,
                                                    FIND_MSG_UNKNOWN) ) ) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: traffic_message_find_recv: Unable to find the proper message reference.\n");
        return OMPI_ERROR;
    }

    /*
     * Check the unknown list of persistant
     */
    if( OMPI_SUCCESS != (ret = traffic_message_find(&(unknown_persist_recv_list),
                                                    count, tag, INVALID_INT,
                                                    comm_id, datatype_size,
                                                    posted_unknown_precv_msg_ref,
                                                    FIND_MSG_UNKNOWN) ) ) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: traffic_message_find_recv: Unable to find the proper message reference.\n");
        return OMPI_ERROR;
    }

    /*
     * JJH -- Should we check the drained list?
     * If we checkpoint again before dimishing the drained list, then
     * the peer could be requesting that a drained send complete...
     */

    return OMPI_SUCCESS;
}

static int traffic_message_find(opal_list_t * search_list,
                                size_t count, int tag, int peer,
                                uint32_t comm_id, size_t ddt_size,
                                ompi_crcp_bkmrk_pml_traffic_message_ref_t ** found_msg_ref,
                                int active )
{
    opal_list_item_t* item = NULL;

    *found_msg_ref = NULL;

#if OPAL_ENABLE_DEBUG == 1
    /*
     * Dummy checks:
     */
    if( NULL == search_list) {
        opal_output(0, "WARNING (Debug): Search_list NULL! (%s:%d)", __FILE__, __LINE__);
        return OMPI_ERROR;
    }
#endif

    /*
     * Check the search list
     */
    for(item  = opal_list_get_last(search_list);
        item != opal_list_get_begin(search_list);
        item  = opal_list_get_prev(item) ) {
        ompi_crcp_bkmrk_pml_traffic_message_ref_t * msg_ref;
        msg_ref = (ompi_crcp_bkmrk_pml_traffic_message_ref_t*)item;

        if( active != FIND_MSG_UNKNOWN ) {
            if( active == PERSIST_MARKER ) {
                if( 0 >= msg_ref->posted ) {
                    continue;
                }
            }
            else if( (active == FIND_MSG_TRUE  && 0 >= (msg_ref->active + msg_ref->active_drain) ) ||
                     (active == FIND_MSG_FALSE && 0 <= (msg_ref->active + msg_ref->active_drain) ) ) {
                continue;
            }
        }

        if(msg_ref->count == count  &&
           (NULL != msg_ref->comm && msg_ref->comm->c_contextid == comm_id) && 
           (msg_ref->tag  == MPI_ANY_TAG || msg_ref->tag  == tag)   &&
           (peer          == INVALID_INT || msg_ref->rank == peer)  &&
           msg_ref->ddt_size == ddt_size) {

            OPAL_OUTPUT_VERBOSE((30, mca_crcp_bkmrk_component.super.output_handle,
                                "crcp:bkmrk: traffic_message_find: Found Message -- Comm list (%d, %d)\n",
                                tag, peer));

            *found_msg_ref = msg_ref;
            return OMPI_SUCCESS;
        }
    }

    return OMPI_SUCCESS;
}


/************************************************
 * Drain Message Utility Functions
 ************************************************/
static int drain_message_append(ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                                ompi_crcp_bkmrk_pml_message_type_t msg_type,
                                size_t count, size_t ddt_size,
                                int tag,int dest,
                                struct ompi_communicator_t* comm,
                                ompi_crcp_bkmrk_pml_drain_message_ref_t **msg_ref)
{
    int ret, exit_status = ORTE_SUCCESS;
    ompi_crcp_bkmrk_pml_message_content_ref_t *content_ref = NULL;

    /*
     * Determine if message is currently in the list
     *  - If it is then increment the count.
     *  - ow add it to the list
     */
    if( OMPI_SUCCESS != (ret = drain_message_find(&(peer_ref->drained_list),
                                                  count, tag, dest,
                                                  comm->c_contextid,
                                                  ddt_size,
                                                  msg_ref,
                                                  &content_ref) ) ) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: traffic_message_append: Unable to find the proper message reference.\n");
        return OMPI_ERROR;
    }

    if( NULL == *msg_ref ) {
        CREATE_NEW_DRAIN_MSG((*msg_ref), msg_type,
                             count, NULL, tag, dest, comm,
                             peer_ref->proc_name.jobid,
                             peer_ref->proc_name.vpid);

        (*msg_ref)->done           = 0;
        (*msg_ref)->active         = 0;
        (*msg_ref)->already_posted = 0;

        opal_list_append(&(peer_ref->drained_list), &((*msg_ref)->super));
    }
    /* If message does exist then the calling function needs to handle the msg_contents and counts */

    return exit_status;
}

static int drain_message_remove(ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                                ompi_crcp_bkmrk_pml_drain_message_ref_t *msg_ref,
                                ompi_crcp_bkmrk_pml_message_content_ref_t *content_ref)
{
    /*
     * Remove the message content from the list attached to the message
     */
    opal_list_remove_item(&(msg_ref->msg_contents), &(content_ref->super));
    HOKE_CONTENT_REF_RETURN(content_ref);

    /*
     * If there are no more drained messages of this signature,
     * then remove the signature from the peers drained list.
     */
    if( 0 >= opal_list_get_size(&(msg_ref->msg_contents) ) ) {
        TRAFFIC_MSG_DUMP_DRAIN_MSG_INDV(10, (msg_ref, "D*remove", true));
        opal_list_remove_item(&(peer_ref->drained_list), &(msg_ref->super));
        HOKE_DRAIN_MSG_REF_RETURN(msg_ref);
    } else {
        TRAFFIC_MSG_DUMP_DRAIN_MSG_INDV(10, (msg_ref, "Dremove", true));
    }

    return OMPI_SUCCESS;
}

static int drain_message_check_recv(void **buf, size_t count,
                                    ompi_datatype_t *datatype,
                                    int *src, int *tag,
                                    struct ompi_communicator_t* comm,
                                    struct ompi_request_t **request,
                                    ompi_status_public_t** status,
                                    bool *found_drain)
{
    int ret, exit_status = OMPI_SUCCESS;
    ompi_crcp_bkmrk_pml_peer_ref_t    *peer_ref      = NULL;
    ompi_crcp_bkmrk_pml_drain_message_ref_t   *drain_msg_ref = NULL;
    ompi_crcp_bkmrk_pml_message_content_ref_t *content_ref = NULL;
    size_t tmp_ddt_size  = 0;

    *found_drain = false;

    ompi_datatype_type_size(datatype, &tmp_ddt_size);

    /*
     * Check to see if this message is in the drained message list
     */
    if( OMPI_SUCCESS != (ret = drain_message_find_any(count, *tag, *src,
                                                      comm, tmp_ddt_size,
                                                      &drain_msg_ref,
                                                      &content_ref,
                                                      &peer_ref) ) ) {
        ERROR_SHOULD_NEVER_HAPPEN("crcp:bkmrk: drain_check_recv(): Failed trying to find a drained message.");
        exit_status = ret;
        goto DONE;
    }

    /*
     * If the message is a drained message
     *  - Complete it right now
     *  - We do not need to increment any counters here since we already have
     *    when we originally drained the message.
     */
    if( NULL != drain_msg_ref ) {
        OPAL_OUTPUT_VERBOSE((12, mca_crcp_bkmrk_component.super.output_handle,
                             "crcp:bkmrk: drain_check_recv(): Matched a drained message..."));

        if( OMPI_SUCCESS != (ret = drain_message_copy_remove(drain_msg_ref,
                                                             content_ref,
                                                             src, tag, request, status,
                                                             datatype, count, buf,
                                                             peer_ref) ) ) {
            opal_output( mca_crcp_bkmrk_component.super.output_handle,
                         "crcp:bkmrk: drain_check_recv(): Datatype copy failed (%d)",
                         ret);
            exit_status = ret;
            goto DONE;
        }

        peer_ref->total_drained_msgs -= 1;

        *found_drain = true;
    }

 DONE:
    return exit_status;
}

static int drain_message_find_any(size_t count, int tag, int peer,
                                  struct ompi_communicator_t* comm, size_t ddt_size,
                                  ompi_crcp_bkmrk_pml_drain_message_ref_t ** found_msg_ref,
                                  ompi_crcp_bkmrk_pml_message_content_ref_t ** content_ref,
                                  ompi_crcp_bkmrk_pml_peer_ref_t **peer_ref)
{
    ompi_crcp_bkmrk_pml_peer_ref_t *cur_peer_ref = NULL;
    opal_list_item_t* item = NULL;

    *found_msg_ref = NULL;

    for(item  = opal_list_get_first(&ompi_crcp_bkmrk_pml_peer_refs);
        item != opal_list_get_end(&ompi_crcp_bkmrk_pml_peer_refs);
        item  = opal_list_get_next(item) ) {
        cur_peer_ref = (ompi_crcp_bkmrk_pml_peer_ref_t*)item;

        /*
         * If we ware not MPI_ANY_SOURCE, then extract the process name from the
         * communicator, and search only the peer that matches.
         */
        if( MPI_ANY_SOURCE != peer && peer >= 0) {
            /* Check to see if peer could possibly be in this communicator */
            if( comm->c_local_group->grp_proc_count <= peer ) {
                continue;
            }
                
            if( OPAL_EQUAL != orte_util_compare_name_fields(ORTE_NS_CMP_ALL,
                                                            &(cur_peer_ref->proc_name),
                                                            &(comm->c_local_group->grp_proc_pointers[peer]->proc_name)) ) {
                continue;
            }
        }

        drain_message_find(&(cur_peer_ref->drained_list),
                           count, tag, peer,
                           comm->c_contextid, ddt_size,
                           found_msg_ref,
                           content_ref);
        if( NULL != *found_msg_ref) {
            if( NULL != peer_ref ) {
                *peer_ref = cur_peer_ref;
            }
            return OMPI_SUCCESS;
        }
    }

    return OMPI_SUCCESS;
}

static int drain_message_find(opal_list_t * search_list,
                              size_t count, int tag, int peer,
                              uint32_t comm_id, size_t ddt_size,
                              ompi_crcp_bkmrk_pml_drain_message_ref_t ** found_msg_ref,
                              ompi_crcp_bkmrk_pml_message_content_ref_t ** content_ref)
{
    ompi_crcp_bkmrk_pml_drain_message_ref_t * drain_msg = NULL;
    opal_list_item_t* item = NULL;

    *found_msg_ref = NULL;
    *content_ref   = NULL;

    /* Dummy Check:
     * If the list is empty...
     */
    if( 0 >= opal_list_get_size(search_list) ) {
        return OMPI_SUCCESS;
    }
    
    for(item  = opal_list_get_first(search_list);
        item != opal_list_get_end(search_list);
        item  = opal_list_get_next(item) ) {
        drain_msg = (ompi_crcp_bkmrk_pml_drain_message_ref_t*)item;

        OPAL_OUTPUT_VERBOSE((15, mca_crcp_bkmrk_component.super.output_handle,
                             "crcp:bkmrk: find_drain_msg(): Compare [%d, %d, %d, %d] to [%d, %d, %d, %d]",
                             (int)ddt_size, (int)count, tag, peer,
                             (int)drain_msg->ddt_size, (int)drain_msg->count, (int)drain_msg->tag, (int)drain_msg->rank));

        /* Check the communicator for a match */
        if( NULL != drain_msg->comm ) {
            if( drain_msg->comm->c_contextid != comm_id ) {
                continue;
            }
        }

        /* If a specific tag was requested, then make sure this messages matches */
        if( MPI_ANY_TAG    != tag &&
            drain_msg->tag != tag) {
            continue;
        }

        /* If a specific rank was requested, then make sure this messages matches */
        if( INVALID_INT != peer ) {
            if( MPI_ANY_SOURCE  != peer && 
                drain_msg->rank != peer) {
                continue;
            }
        }

        /* Check the datatype size, if specified for a match */
        if( ddt_size != PROBE_ANY_SIZE &&
            count    != PROBE_ANY_COUNT) {
            /* Check the datatype size and count to make sure it matches   */
            if((drain_msg->count   ) != count   || 
               (drain_msg->ddt_size) != ddt_size) {
                continue;
            }
        }

        /* If we get here then the message matches */
        *found_msg_ref = drain_msg;
        break;
    }

    /*
     * Find a content to return
     */
    if( NULL != *found_msg_ref ) {
        drain_message_grab_content((*found_msg_ref), content_ref );

        /* If there are no contents that match, then there are no drained messages that match. */
        if( NULL == *content_ref ) {
            *found_msg_ref = NULL;
        }
    }

    return OMPI_SUCCESS;
}

static int drain_message_grab_content(ompi_crcp_bkmrk_pml_drain_message_ref_t *drain_msg_ref,
                                      ompi_crcp_bkmrk_pml_message_content_ref_t ** content_ref)
{
    ompi_crcp_bkmrk_pml_message_content_ref_t *loc_content_ref = NULL;
    opal_list_item_t* item = NULL;

    *content_ref = NULL;

    for(item  = opal_list_get_first(&(drain_msg_ref->msg_contents));
        item != opal_list_get_end(&(drain_msg_ref->msg_contents));
        item  = opal_list_get_next(item) ) {
        loc_content_ref = (ompi_crcp_bkmrk_pml_message_content_ref_t*)item;

        /* If the buffer is invalid then this is not a valid message or
         * has not been completed draining just yet */
        if(NULL != loc_content_ref->buffer) {
            *content_ref = loc_content_ref;
            break;
        }
    }

    return OMPI_SUCCESS;
}

static int drain_message_copy_remove_persistent(ompi_crcp_bkmrk_pml_drain_message_ref_t   *drain_msg_ref,
                                                ompi_crcp_bkmrk_pml_message_content_ref_t *drain_content_ref,
                                                ompi_crcp_bkmrk_pml_traffic_message_ref_t *traffic_msg_ref,
                                                ompi_request_t *request,
                                                ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref)
{
    int ret, exit_status = OMPI_SUCCESS;
    ompi_crcp_bkmrk_pml_message_content_ref_t *content_ref = NULL;

    /*
     * Find the request in the list that has been posted, but not started
     */
    traffic_message_find_mark_persistent(traffic_msg_ref, &request,
                                         false, /* Find currently not active */
                                         false, /* Keep inactive */
                                         &content_ref);

    /* These two requests should be exactly the same, so this is redundant, but here for completeness */
    content_ref->request = request;

    memcpy(&(content_ref->status), &drain_content_ref->status, sizeof(ompi_status_public_t)); 

    if( 0 != (ret = ompi_datatype_copy_content_same_ddt(drain_msg_ref->datatype,
                                                   drain_msg_ref->count,
                                                   content_ref->buffer,
                                                   drain_content_ref->buffer) ) ) {
        opal_output( mca_crcp_bkmrk_component.super.output_handle,
                     "crcp:bkmrk: drain_message_copy_remove_p(): Datatype copy failed (%d)",
                     ret);
        exit_status = ret;
    }

    /* Remove the message from the list */
    drain_content_ref->request = NULL;
    drain_message_remove(peer_ref, drain_msg_ref, drain_content_ref);

    return exit_status;
}

static int drain_message_copy_remove(ompi_crcp_bkmrk_pml_drain_message_ref_t *drain_msg_ref,
                                     ompi_crcp_bkmrk_pml_message_content_ref_t * drain_content_ref,
                                     int *src, int *tag,
                                     struct ompi_request_t **request,
                                     ompi_status_public_t **status, 
                                     ompi_datatype_t *datatype, int count, void **buf,
                                     ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref)
{
    int ret, exit_status = OMPI_SUCCESS;

    if( NULL != src ) {
        *src = drain_msg_ref->rank;
    }

    if( NULL != tag ) {
        *tag = drain_msg_ref->tag;
    }

    if( NULL != request ) {
        *request = drain_content_ref->request;
        OBJ_RETAIN(*request);
    }

    if( NULL != status && MPI_STATUS_IGNORE != *status ) {
        memcpy(*status, &drain_content_ref->status, sizeof(ompi_status_public_t)); 
    }

    /* The buffer could be NULL - More likely when doing a count=0 type of message (e.g., Barrier) */
    if( OPAL_LIKELY(NULL != buf) ) {
        if( 0 != (ret = ompi_datatype_copy_content_same_ddt(datatype, count,
                                                       (void*)buf, drain_content_ref->buffer) ) ) {
            opal_output( mca_crcp_bkmrk_component.super.output_handle,
                         "crcp:bkmrk: drain_message_copy_remove(): Datatype copy failed (%d)",
                         ret);
            exit_status = ret;
        }
    }
    else {
        OPAL_OUTPUT_VERBOSE((20, mca_crcp_bkmrk_component.super.output_handle,
                             "crcp:bkmrk: drain_message_copy_remove(): Skip copy - NULL buffer"));
    }

    /* Remove the message from the list */
    drain_content_ref->request = NULL;
    drain_message_remove(peer_ref, drain_msg_ref, drain_content_ref);

    return exit_status;
}


/************************************************
 * Peer List Utility Functions
 ************************************************/
static ompi_crcp_bkmrk_pml_peer_ref_t * find_peer(orte_process_name_t proc)
{
    opal_list_item_t* item = NULL;

    for(item  = opal_list_get_first(&ompi_crcp_bkmrk_pml_peer_refs);
        item != opal_list_get_end(&ompi_crcp_bkmrk_pml_peer_refs);
        item  = opal_list_get_next(item) ) {
        ompi_crcp_bkmrk_pml_peer_ref_t *cur_peer_ref;
        cur_peer_ref = (ompi_crcp_bkmrk_pml_peer_ref_t*)item;

        if( OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL,
                                                        &(cur_peer_ref->proc_name),
                                                        &proc) ) {
            return cur_peer_ref;
        }
    }

    return NULL;
}

static int find_peer_in_comm(struct ompi_communicator_t* comm, int proc_idx,
                             ompi_crcp_bkmrk_pml_peer_ref_t **peer_ref)
{
    *peer_ref = find_peer(comm->c_remote_group->grp_proc_pointers[proc_idx]->proc_name);

    if( NULL == *peer_ref) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: find_peer_in_comm(): Failed to find peer_ref - peer_ref is NULL\n");
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


/************************************************
 * FT Event Utility Functions
 ************************************************/
static int ft_event_coordinate_peers(void)
{
    static int step_to_return_to = 0;
    int exit_status = OMPI_SUCCESS;
    int ret;
    
    if( step_to_return_to == 1 ) {
        goto STEP_1;
    }

    /*
     * Exchange Bookmarks with peers
     */
    START_TIMER(CRCP_TIMER_CKPT_EX_B);
    if( OMPI_SUCCESS != (ret = ft_event_exchange_bookmarks() ) ) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: ft_event_coordinate_peers: Bookmark Exchange Failed %d",
                    ret);
        exit_status = ret;
        goto DONE;
    }
    END_TIMER(CRCP_TIMER_CKPT_EX_B);

    /*
     * Check exchanged bookmarks 
     */
    START_TIMER(CRCP_TIMER_CKPT_CHECK_B);
    if( OMPI_SUCCESS != (ret = ft_event_check_bookmarks() ) ) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: ft_event_coordinate_peers: Bookmark Check Failed %d",
                    ret);
        exit_status = ret;
        goto DONE;
    }
    END_TIMER(CRCP_TIMER_CKPT_CHECK_B);

    /*
     * Post Drain Acks and Msgs
     */
    START_TIMER(CRCP_TIMER_CKPT_POST_DRAIN);
    if( OMPI_SUCCESS != (ret = ft_event_post_drain_acks() ) ) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: ft_event_coordinate_peers: Bookmark Post Drain ACKS Failed %d",
                    ret);
        exit_status = ret;
        goto DONE;
    }

    if( OMPI_SUCCESS != (ret = ft_event_post_drained() ) ) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: ft_event_coordinate_peers: Bookmark Post Drain Msgs Failed %d",
                    ret);
        exit_status = ret;
        goto DONE;
    }
    END_TIMER(CRCP_TIMER_CKPT_POST_DRAIN);
    DISPLAY_INDV_TIMER(CRCP_TIMER_CKPT_POST_DRAIN, -1, 0);

    /*
     * Check if we need to stall for completion of tasks
     */
    /*
     * If we know that we are in the middle of a blocking send then we
     * need to stall the coordination algorithm while we wait for this to 
     * complete.
     */
    if( 0 < current_msg_id &&
        current_msg_type == COORD_MSG_TYPE_B_SEND) {
        stall_for_completion = true;
    }
    START_TIMER(CRCP_TIMER_CKPT_WAIT_QUI);
    if( stall_for_completion ) {
        OPAL_OUTPUT_VERBOSE((10, mca_crcp_bkmrk_component.super.output_handle,
                             "crcp:bkmrk: %s **** STALLING %s in PID %d ***",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (current_msg_type == COORD_MSG_TYPE_B_SEND ? "Send" : "Recv"),
                             getpid() ));
        step_to_return_to = 1;
        exit_status = OMPI_SUCCESS;
        goto DONE;
    }

 STEP_1:
    step_to_return_to = 0;

    /*
     * Wait for any messages that needed resolved.
     * - Outstanding Receives (to drain wire) -- Receiver
     * - Outstanding Irecvs (for drain ack)   -- Sender
     */
    if( OMPI_SUCCESS != (ret = ft_event_wait_quiesce() ) ) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: ft_event_coordinate_peers: Wait Quiesce Failed %d",
                    ret);
        exit_status = ret;
        goto DONE;
    }
    END_TIMER(CRCP_TIMER_CKPT_WAIT_QUI);

    OPAL_OUTPUT_VERBOSE((5, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: %s Coordination Finished...\n",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /*
     * Now that all our peer channels are marked as drained
     * continue with the checkpoint.
     * Note: This does not guarentee that all of the peers
     *       are at this same position, but that our
     *       checkpoint will be consistent with all of the
     *       peers once they finish the protocol.
     */

 DONE:
    return exit_status;
}

static int ft_event_finalize_exchange(void)
{
    int exit_status = OMPI_SUCCESS;
    opal_list_item_t* item = NULL, *rm_item = NULL;
    ompi_crcp_bkmrk_pml_traffic_message_ref_t * msg_ref;
    ompi_crcp_bkmrk_pml_message_content_ref_t  *content_ref = NULL;
    opal_list_item_t* cont_item = NULL;

    /*
     * Clear bookmark totals
     */
    for(item  = opal_list_get_first(&ompi_crcp_bkmrk_pml_peer_refs);
        item != opal_list_get_end(&ompi_crcp_bkmrk_pml_peer_refs);
        item  = opal_list_get_next(item) ) {
        ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref;
        peer_ref = (ompi_crcp_bkmrk_pml_peer_ref_t*)item;

        if( OPAL_EQUAL != orte_util_compare_name_fields(ORTE_NS_CMP_ALL,
                                                        (ORTE_PROC_MY_NAME),
                                                        &(peer_ref->proc_name)) ) {
            TRAFFIC_MSG_DUMP_PEER(10, (peer_ref, "finalize_exchange", false));
        }

        peer_ref->total_msgs_sent  = 0;
        peer_ref->total_msgs_recvd = 0;

        peer_ref->matched_msgs_sent  = 0;
        peer_ref->matched_msgs_recvd = 0;

        peer_ref->ack_required = false;

        /* Clear send_list */
        for(rm_item  = opal_list_get_last(&peer_ref->send_list);
            rm_item != opal_list_get_begin(&peer_ref->send_list);
            rm_item  = opal_list_get_prev(rm_item) ) {
            msg_ref = (ompi_crcp_bkmrk_pml_traffic_message_ref_t*)rm_item;
            msg_ref->matched = 0;
            msg_ref->done    = 0;
            msg_ref->active_drain  += msg_ref->active;
            msg_ref->active  = 0;

            for(cont_item  = opal_list_get_first(&(msg_ref->msg_contents));
                cont_item != opal_list_get_end(  &(msg_ref->msg_contents));
                cont_item  = opal_list_get_next(cont_item) ) {
                content_ref = (ompi_crcp_bkmrk_pml_message_content_ref_t*)cont_item;
                if( content_ref->active ) {
                    content_ref->already_drained = true;
                }
            }
        }

        /* Clear isend_list */
        for(rm_item  = opal_list_get_last(&peer_ref->isend_list);
            rm_item != opal_list_get_begin(&peer_ref->isend_list);
            rm_item  = opal_list_get_prev(rm_item) ) {
            msg_ref = (ompi_crcp_bkmrk_pml_traffic_message_ref_t*)rm_item;
            msg_ref->matched = 0;
            msg_ref->done    = 0;
            msg_ref->active_drain  += msg_ref->active;
            msg_ref->active  = 0;

            for(cont_item  = opal_list_get_first(&(msg_ref->msg_contents));
                cont_item != opal_list_get_end(  &(msg_ref->msg_contents));
                cont_item  = opal_list_get_next(cont_item) ) {
                content_ref = (ompi_crcp_bkmrk_pml_message_content_ref_t*)cont_item;
                if( content_ref->active ) {
                    content_ref->already_drained = true;
                }
            }
        }

        /* Clear send_init_list */
        for(rm_item  = opal_list_get_last(&peer_ref->send_list);
            rm_item != opal_list_get_begin(&peer_ref->send_list);
            rm_item  = opal_list_get_prev(rm_item) ) {
            msg_ref = (ompi_crcp_bkmrk_pml_traffic_message_ref_t*)rm_item;
            msg_ref->matched = 0;
            msg_ref->done    = 0;
            msg_ref->active_drain  += msg_ref->active;
            msg_ref->active  = 0;

            for(cont_item  = opal_list_get_first(&(msg_ref->msg_contents));
                cont_item != opal_list_get_end(  &(msg_ref->msg_contents));
                cont_item  = opal_list_get_next(cont_item) ) {
                content_ref = (ompi_crcp_bkmrk_pml_message_content_ref_t*)cont_item;
                if( content_ref->active ) {
                    content_ref->already_drained = true;
                }
            }
        }

        /* Clear recv_list */
        for(rm_item  = opal_list_get_last(&peer_ref->recv_list);
            rm_item != opal_list_get_begin(&peer_ref->recv_list);
            rm_item  = opal_list_get_prev(rm_item) ) {
            msg_ref = (ompi_crcp_bkmrk_pml_traffic_message_ref_t*)rm_item;
            msg_ref->matched = 0;
            msg_ref->done    = 0;
        }

        /* Clear irecv_list */
        for(rm_item  = opal_list_get_last(&peer_ref->irecv_list);
            rm_item != opal_list_get_begin(&peer_ref->irecv_list);
            rm_item  = opal_list_get_prev(rm_item) ) {
            msg_ref = (ompi_crcp_bkmrk_pml_traffic_message_ref_t*)rm_item;
            msg_ref->matched = 0;
            msg_ref->done    = 0;
        }

        /* Clear recv_init_list */
        for(rm_item  = opal_list_get_last(&peer_ref->recv_list);
            rm_item != opal_list_get_begin(&peer_ref->recv_list);
            rm_item  = opal_list_get_prev(rm_item) ) {
            msg_ref = (ompi_crcp_bkmrk_pml_traffic_message_ref_t*)rm_item;
            msg_ref->matched = 0;
            msg_ref->done    = 0;
        }
    }

    return exit_status;
}

static int ft_event_exchange_bookmarks(void)
{
    int peer_idx  = 0;
    int my_idx    = ORTE_PROC_MY_NAME->vpid;
    int iter      = 0;
    int num_peers = 0;
    
    num_peers = opal_list_get_size(&ompi_crcp_bkmrk_pml_peer_refs);

    for( peer_idx = (num_peers - my_idx - 1), iter = 0;
         iter < num_peers;
         peer_idx = (peer_idx + 1) % num_peers, ++iter) 
        {
            if(my_idx > peer_idx) {
                /* Send our bookmark status */
                send_bookmarks(peer_idx);
                /* Recv peer bookmark status */
                recv_bookmarks(peer_idx);
            }
            else if(my_idx < peer_idx) {
                /* Recv peer bookmark status */
                recv_bookmarks(peer_idx);
                /* Send our bookmark status */
                send_bookmarks(peer_idx);
            }
        }

    /* Wait for all bookmarks to arrive */
    START_TIMER(CRCP_TIMER_CKPT_EX_WAIT);
    while( total_recv_bookmarks > 0 ) {
        opal_event_loop(OPAL_EVLOOP_NONBLOCK);
    }
    total_recv_bookmarks = 0;
    END_TIMER(CRCP_TIMER_CKPT_EX_WAIT);

    return OMPI_SUCCESS;
}

static int ft_event_check_bookmarks(void)
{
    opal_list_item_t* item = NULL;
    int ret;
    int p_n_to_p_m   = 0;
    int p_n_from_p_m = 0;

    if( 10 <= mca_crcp_bkmrk_component.super.verbose ) {
        sleep(ORTE_PROC_MY_NAME->vpid);
        OPAL_OUTPUT_VERBOSE((10, mca_crcp_bkmrk_component.super.output_handle,
                             "---------------------------------------------"));
        OPAL_OUTPUT_VERBOSE((10, mca_crcp_bkmrk_component.super.output_handle,
                             "Process %s Match Table",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        OPAL_OUTPUT_VERBOSE((10, mca_crcp_bkmrk_component.super.output_handle,
                            "%s  %5s | %7s | %7s | %7s | %7s |",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            "Vpid", "T_Send", "M_Recv", "M_Send", "T_Recv"));

        for(item  = opal_list_get_first(&ompi_crcp_bkmrk_pml_peer_refs);
            item != opal_list_get_end(&ompi_crcp_bkmrk_pml_peer_refs);
            item  = opal_list_get_next(item) ) {
            ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref;
            int t_send, m_send;
            int t_recv, m_recv;
            peer_ref = (ompi_crcp_bkmrk_pml_peer_ref_t*)item;

            t_send = peer_ref->total_msgs_sent;
            m_send = peer_ref->matched_msgs_sent;
            t_recv = peer_ref->total_msgs_recvd;
            m_recv = peer_ref->matched_msgs_recvd;

            OPAL_OUTPUT_VERBOSE((10, mca_crcp_bkmrk_component.super.output_handle,
                                "%s  %5d | %7d | %7d | %7d | %7d |",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                peer_ref->proc_name.vpid,
                                t_send, m_recv, m_send, t_recv));
        }
        OPAL_OUTPUT_VERBOSE((10, mca_crcp_bkmrk_component.super.output_handle,
                             "---------------------------------------------"));
    }

    /*
     * For each peer:
     * - Check bookmarks
     * - if mis-matched then post outstanding recvs.
     */
    for(item  = opal_list_get_first(&ompi_crcp_bkmrk_pml_peer_refs);
        item != opal_list_get_end(&ompi_crcp_bkmrk_pml_peer_refs);
        item  = opal_list_get_next(item) ) {
        ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref;
        peer_ref = (ompi_crcp_bkmrk_pml_peer_ref_t*)item;

        if( OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL,
                                                        (ORTE_PROC_MY_NAME),
                                                        &(peer_ref->proc_name)) ) {
            continue;
        }

        TRAFFIC_MSG_DUMP_PEER(15, (peer_ref, "-- Bookmark Details --", false));

        /* Lowest Rank sends first */
        if( ORTE_PROC_MY_NAME->vpid < peer_ref->proc_name.vpid ) {
            /********************
             * Check P_n --> P_m
             * Has the peer received all the messages that I have put on the wire?
             ********************/
            p_n_to_p_m   = peer_ref->total_msgs_sent;
            p_n_from_p_m = peer_ref->matched_msgs_recvd;

            /* T_Send >= M_Recv */
            if( p_n_to_p_m < p_n_from_p_m ) {
                opal_output(mca_crcp_bkmrk_component.super.output_handle,
                            "crcp:bkmrk: %s --> %s "
                            "Total Sent (%4d) = Matched Recv. (%4d) => Diff (%4d). "
                            " WARNING: Peer received more than was sent. :(\n",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                            p_n_to_p_m,
                            p_n_from_p_m,
                            (p_n_to_p_m - p_n_from_p_m)
                            );
            }

            /* I've send more than my peer has received,
             * so need to coordinate with peer. */
            if( p_n_to_p_m > p_n_from_p_m) {
                OPAL_OUTPUT_VERBOSE((10, mca_crcp_bkmrk_component.super.output_handle,
                                    "crcp:bkmrk: %s --> %s "
                                    "Total Sent (%4d) = Matched Recv. (%4d). Peer needs %4d.\n",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                    ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                                    p_n_to_p_m,
                                    p_n_from_p_m,
                                    (p_n_to_p_m - p_n_from_p_m)
                                    ));
                /*
                 * Tell the peer what the outstanding messages looked like.
                 * Since we can't tell which ones they are, we need to send the
                 * information for all of the messages since the last checkpoint
                 */
                if( OMPI_SUCCESS != (ret = send_msg_details(peer_ref, p_n_to_p_m, p_n_from_p_m) ) ) {
                    opal_output(mca_crcp_bkmrk_component.super.output_handle,
                                "crcp:bkmrk: check_bookmarks: Unable to send message details to peer %s: Return %d\n",
                                ORTE_NAME_PRINT(&peer_ref->proc_name),
                                ret);
                    return ret;
                }
            }

            /********************
             * Check P_n <-- P_m
             * Have I received all the messages that my peer has put on the wire?
             ********************/
            p_n_to_p_m   = peer_ref->matched_msgs_sent;
            p_n_from_p_m = peer_ref->total_msgs_recvd;

            /* M_Send >= T_Recv */
            if( p_n_to_p_m < p_n_from_p_m ) {
                opal_output(mca_crcp_bkmrk_component.super.output_handle,
                            "crcp:bkmrk: %s --> %s "
                            "Matched Sent (%4d) = Total Recv. (%4d) => Diff (%4d). "
                            " WARNING: I received more than the peer sent. :(\n",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                            p_n_to_p_m,
                            p_n_from_p_m,
                            (p_n_to_p_m - p_n_from_p_m)
                            );
            }

            /* I've recv'ed less than my peer has sent,
             * so need to coordinate with peer. */
            if( p_n_to_p_m > p_n_from_p_m) {
                OPAL_OUTPUT_VERBOSE((10, mca_crcp_bkmrk_component.super.output_handle,
                                     "crcp:bkmrk: %s <-- %s "
                                     "Matched Sent (%4d) = Total Recv. (%4d). I need %4d.\n",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                    ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                                    p_n_to_p_m,
                                    p_n_from_p_m,
                                    (p_n_to_p_m - p_n_from_p_m)
                                    ));
                /*
                 * Receive from peer the datatypes of the outstanding sends
                 *  As we figure out what they are post Irecv's for them into a drained buffer list.
                 */
                if( OMPI_SUCCESS != (ret = recv_msg_details(peer_ref, p_n_to_p_m, p_n_from_p_m) ) ) {
                    opal_output(mca_crcp_bkmrk_component.super.output_handle,
                                "crcp:bkmrk: check_bookmarks: Unable to recv message details from peer %s: Return %d\n",
                                ORTE_NAME_PRINT(&peer_ref->proc_name),
                                ret);
                    return ret;
                }
            }
        }
        /* Highest rank recvs first */
        else {
            /********************
             * Check P_n <-- P_m
             * Have I received all the messages that my peer has put on the wire?
             ********************/
            p_n_to_p_m   = peer_ref->matched_msgs_sent;
            p_n_from_p_m = peer_ref->total_msgs_recvd;

            /* M_Send >= T_Recv */
            if( p_n_to_p_m < p_n_from_p_m ) {
                opal_output(mca_crcp_bkmrk_component.super.output_handle,
                            "crcp:bkmrk: %s --> %s "
                            "Matched Sent (%4d) = Total Recv. (%4d) => Diff (%4d). "
                            " WARNING: I received more than the peer sent. :(\n",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                            p_n_to_p_m,
                            p_n_from_p_m,
                            (p_n_to_p_m - p_n_from_p_m)
                            );
            }

            /* I've recv'ed less than my peer has sent,
             * so need to coordinate with peer. */
            if( p_n_to_p_m > p_n_from_p_m) {
                OPAL_OUTPUT_VERBOSE((10, mca_crcp_bkmrk_component.super.output_handle,
                                     "crcp:bkmrk: %s <-- %s "
                                     "Matched Sent (%4d) = Total Recv. (%4d). I need %4d.\n",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                    ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                                    p_n_to_p_m,
                                    p_n_from_p_m,
                                    (p_n_to_p_m - p_n_from_p_m)
                                    ));
                /*
                 * Receive from peer the datatypes of the outstanding sends
                 *  As we figure out what they are post Irecv's for them into a drained buffer list.
                 */
                if( OMPI_SUCCESS != (ret = recv_msg_details(peer_ref, p_n_to_p_m, p_n_from_p_m) ) ) {
                    opal_output(mca_crcp_bkmrk_component.super.output_handle,
                                "crcp:bkmrk: check_bookmarks: Unable to recv message details from peer %s: Return %d\n",
                                ORTE_NAME_PRINT(&peer_ref->proc_name),
                                ret);
                    return ret;
                }
            }

            /********************
             * Check P_n --> P_m
             * Has the peer received all the messages that I have put on the wire?
             ********************/
            p_n_to_p_m   = peer_ref->total_msgs_sent;
            p_n_from_p_m = peer_ref->matched_msgs_recvd;

            /* T_Send >= M_Recv */
            if( p_n_to_p_m < p_n_from_p_m ) {
                opal_output(mca_crcp_bkmrk_component.super.output_handle,
                            "crcp:bkmrk: %s --> %s "
                            "Total Sent (%4d) = Matched Recv. (%4d) => Diff (%4d). "
                            " WARNING: Peer received more than was sent. :(\n",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                            p_n_to_p_m,
                            p_n_from_p_m,
                            (p_n_to_p_m - p_n_from_p_m)
                            );
            }

            /* I've send more than my peer has received,
             * so need to coordinate with peer. */
            if( p_n_to_p_m > p_n_from_p_m) {
                OPAL_OUTPUT_VERBOSE((10, mca_crcp_bkmrk_component.super.output_handle,
                                    "crcp:bkmrk: %s --> %s "
                                    "Total Sent (%4d) = Matched Recv. (%4d). Peer needs %4d.\n",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                    ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                                    p_n_to_p_m,
                                    p_n_from_p_m,
                                    (p_n_to_p_m - p_n_from_p_m)
                                    ));
                /*
                 * Tell the peer what the outstanding messages looked like.
                 * Since we can't tell which ones they are, we need to send the
                 * information for all of the messages since the last checkpoint
                 */
                if( OMPI_SUCCESS != (ret = send_msg_details(peer_ref, p_n_to_p_m, p_n_from_p_m) ) ) {
                    opal_output(mca_crcp_bkmrk_component.super.output_handle,
                                "crcp:bkmrk: check_bookmarks: Unable to send message details to peer %s: Return %d\n",
                                ORTE_NAME_PRINT(&peer_ref->proc_name),
                                ret);
                    return ret;
                }
            }
        }
    }

    return OMPI_SUCCESS;
}

static int ft_event_post_drain_acks(void)
{
    ompi_crcp_bkmrk_pml_drain_message_ack_ref_t * drain_msg_ack = NULL;
    opal_list_item_t* item = NULL;
    size_t req_size;
    int ret;

    req_size  = opal_list_get_size(&drained_msg_ack_list);
    if(req_size <= 0) {
        return OMPI_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((10, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: %s Wait on %d Drain ACK Messages.\n",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        (int)req_size));

    /*
     * We have loaded our peer with the message information
     * Now wait for the ack from them
     */
    for(item  = opal_list_get_first(&drained_msg_ack_list);
        item != opal_list_get_end(&drained_msg_ack_list);
        item  = opal_list_get_next(item) ) {
        drain_msg_ack = (ompi_crcp_bkmrk_pml_drain_message_ack_ref_t*)item;

        /* Post the receive */
        if( OMPI_SUCCESS != (ret = orte_rml.recv_buffer_nb( &drain_msg_ack->peer,
                                                            OMPI_CRCP_COORD_BOOKMARK_TAG,
                                                            0,
                                                            drain_message_ack_cbfunc,
                                                            NULL) ) ) {
            opal_output(mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: %s <-- %s: Failed to post a RML receive to the peer\n",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&(drain_msg_ack->peer)));
            return ret;
        }
    }

    return OMPI_SUCCESS;
}

static void drain_message_ack_cbfunc(int status,
                                     orte_process_name_t* sender,
                                     opal_buffer_t *buffer,
                                     orte_rml_tag_t tag,
                                     void* cbdata)
{
    int ret, exit_status = OMPI_SUCCESS;
    opal_list_item_t* item = NULL;
    size_t ckpt_status;

    /*
     * Unpack the buffer
     */
    UNPACK_BUFFER(buffer, ckpt_status, 1, OPAL_SIZE, "");

    /*
     * Update the outstanding message queue
     */
    for(item  = opal_list_get_first(&drained_msg_ack_list);
        item != opal_list_get_end(&drained_msg_ack_list);
        item  = opal_list_get_next(item) ) {
        ompi_crcp_bkmrk_pml_drain_message_ack_ref_t * drain_msg_ack;
        drain_msg_ack = (ompi_crcp_bkmrk_pml_drain_message_ack_ref_t*)item;
        
        /* If this ACK has not completed yet */
        if(!drain_msg_ack->complete) {
            /* If it is the correct peer */
            if( OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL,
                                                            &(drain_msg_ack->peer),
                                                            sender) ) {
                /* We found it! */
                drain_msg_ack->complete = true;
                OPAL_OUTPUT_VERBOSE((5, mca_crcp_bkmrk_component.super.output_handle,
                                    "crcp:bkmrk: %s --> %s Received ACK of FLUSH from peer\n",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                    ORTE_NAME_PRINT(sender) ));
                return;
            }
        }
    }

    opal_output(mca_crcp_bkmrk_component.super.output_handle,
                "crcp:bkmrk: %s --> %s ERROR: Unable to match ACK to peer\n",
                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                ORTE_NAME_PRINT(sender) );

 cleanup:
    return;
}

static int ft_event_post_drained(void)
{
    int ret, exit_status = OMPI_SUCCESS;
    ompi_crcp_bkmrk_pml_peer_ref_t *cur_peer_ref = NULL;
    ompi_crcp_bkmrk_pml_drain_message_ref_t * drain_msg_ref = NULL;
    ompi_crcp_bkmrk_pml_message_content_ref_t *content_ref = NULL;
    opal_list_item_t* item = NULL, *d_item = NULL, *c_item = NULL;
    int i, total_number_to_drain = 0, peer_total = 0;

    /* First Pass just to get a count */
    for(item  = opal_list_get_first(&ompi_crcp_bkmrk_pml_peer_refs);
        item != opal_list_get_end(&ompi_crcp_bkmrk_pml_peer_refs);
        item  = opal_list_get_next(item) ) {
        cur_peer_ref = (ompi_crcp_bkmrk_pml_peer_ref_t*)item;

        if( OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL,
                                                        (ORTE_PROC_MY_NAME),
                                                        &(cur_peer_ref->proc_name)) ) {
            continue;
        }

        for(d_item  = opal_list_get_first(&(cur_peer_ref->drained_list));
            d_item != opal_list_get_end(&(cur_peer_ref->drained_list));
            d_item  = opal_list_get_next(d_item) ) {
            drain_msg_ref = (ompi_crcp_bkmrk_pml_drain_message_ref_t*)d_item;

            for(c_item  = opal_list_get_first(&(drain_msg_ref->msg_contents));
                c_item != opal_list_get_end(&(drain_msg_ref->msg_contents));
                c_item  = opal_list_get_next(c_item) ) {
                content_ref = (ompi_crcp_bkmrk_pml_message_content_ref_t*)c_item;

                if( !content_ref->done ) {
                    ++total_number_to_drain;
                }
            }
        }
    }

    /*
     * Check to make sure there is something to post
     */
    if( 0 >= total_number_to_drain ) {
        return OMPI_SUCCESS;
    }

    /* Allocate memory */
    if( NULL != quiesce_requests ) {
        free(quiesce_requests);
        quiesce_requests = NULL;
    }
    quiesce_requests = (ompi_request_t **)malloc( (total_number_to_drain) * sizeof(ompi_request_t *));
    if( NULL == quiesce_requests){
        exit_status = OMPI_ERROR;
        goto cleanup;
    }

    if( NULL != quiesce_statuses ) {
        free(quiesce_statuses);
        quiesce_statuses = NULL;
    }
    quiesce_statuses = (ompi_status_public_t **)malloc( (total_number_to_drain) * sizeof(ompi_status_public_t *));
    if( NULL == quiesce_statuses){
        exit_status = OMPI_ERROR;
        goto cleanup;
    }

    /* Initalize to invalid values */
    for(i = 0; i < (total_number_to_drain); ++i) {
        quiesce_requests[i] = &(ompi_request_null.request);
        quiesce_statuses[i] = &ompi_status_empty;
    }
    quiesce_request_count = 0;

    /* Second pass to post */
    for(item  = opal_list_get_first(&ompi_crcp_bkmrk_pml_peer_refs);
        item != opal_list_get_end(&ompi_crcp_bkmrk_pml_peer_refs);
        item  = opal_list_get_next(item) ) {
        cur_peer_ref = (ompi_crcp_bkmrk_pml_peer_ref_t*)item;
        peer_total = 0;

        if( OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL,
                                                        (ORTE_PROC_MY_NAME),
                                                        &(cur_peer_ref->proc_name)) ) {
            continue;
        }

        for(d_item  = opal_list_get_first(&(cur_peer_ref->drained_list));
            d_item != opal_list_get_end(&(cur_peer_ref->drained_list));
            d_item  = opal_list_get_next(d_item) ) {
            drain_msg_ref = (ompi_crcp_bkmrk_pml_drain_message_ref_t*)d_item;

            for(c_item  = opal_list_get_first(&(drain_msg_ref->msg_contents));
                c_item != opal_list_get_end(&(drain_msg_ref->msg_contents));
                c_item  = opal_list_get_next(c_item) ) {
                content_ref = (ompi_crcp_bkmrk_pml_message_content_ref_t*)c_item;

                if( content_ref->done ) {
                    continue;
                }

                if( OMPI_SUCCESS != (ret = ft_event_post_drain_message(drain_msg_ref, content_ref) ) ) {
                    exit_status = ret;
                    goto cleanup;
                }

                cur_peer_ref->ack_required = true;

                /* Wait on all drain requests, newly posted or not  */
                if( NULL != content_ref->request) {
                    quiesce_requests[quiesce_request_count] =  content_ref->request;
                    quiesce_statuses[quiesce_request_count] = &content_ref->status;
                    quiesce_request_count++;
                    peer_total++;
                }
                /* If a NULL request, and already_posted then this is an indicator that we need to stall */
                else if( content_ref->already_posted ) {
                    stall_for_completion = true;
                }
                else {
                    ERROR_SHOULD_NEVER_HAPPEN("crcp:bkmrk: ft_event_post_drained(): Found a drain message with a NULL request.");
                }
            }
        }

        if( peer_total > 0 || stall_for_completion ) {
            OPAL_OUTPUT_VERBOSE((10, mca_crcp_bkmrk_component.super.output_handle,
                                 "crcp:bkmrk: %s <-- %s Will be draining %4d messages from this peer. Total %4d %s\n",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&(cur_peer_ref->proc_name)),
                                 peer_total,
                                 quiesce_request_count,
                                 (stall_for_completion ? "(And Stalling)" : "") ));
        }
    }

 cleanup:
    return exit_status;
}

static int ft_event_post_drain_message(ompi_crcp_bkmrk_pml_drain_message_ref_t   *drain_msg_ref,
                                       ompi_crcp_bkmrk_pml_message_content_ref_t *content_ref)
{
    int ret;

    /*
     * This message has already been posted and drained in a previous
     * checkpoint, do not post it again.
     */
    if( content_ref->done ) {
        return OMPI_SUCCESS;
    }

    /* Do not repost those that are already posted, and 
     * we have requests for
     */
    if( content_ref->already_posted ) {
        OPAL_OUTPUT_VERBOSE((15, mca_crcp_bkmrk_component.super.output_handle,
                             "crcp:bkmrk: %s <-- %s Found a message that we do not need to post.\n",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&(drain_msg_ref->proc_name)) ));
        return OMPI_SUCCESS;
    }

    /* Match counts in traffic_message_create_drain_message() */
    content_ref->active = true;
    drain_msg_ref->active++;

    /*
     * Post a receive to drain this message
     */
    OPAL_OUTPUT_VERBOSE((20, mca_crcp_bkmrk_component.super.output_handle,
                         "crcp:bkmrk: %s <-- %s Posting a message to be drained from rank %d.\n",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&(drain_msg_ref->proc_name)),
                         drain_msg_ref->rank));
    if( OMPI_SUCCESS != (ret = wrapped_pml_module->pml_irecv(content_ref->buffer, 
                                                             (drain_msg_ref->count * drain_msg_ref->ddt_size),
                                                             drain_msg_ref->datatype, 
                                                             drain_msg_ref->rank,
                                                             drain_msg_ref->tag,
                                                             drain_msg_ref->comm,
                                                             &(content_ref->request) ) ) ) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: %s <-- %s Failed to post the Draining PML iRecv\n",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    ORTE_NAME_PRINT(&(drain_msg_ref->proc_name)) );
        return ret;
    }

    return OMPI_SUCCESS;
}

static int ft_event_wait_quiesce(void)
{
    int exit_status = OMPI_SUCCESS;
    int ret;

    /*********************************************
     * Wait for all draining receives to complete
     **********************************************/
    if( OMPI_SUCCESS != (ret = wait_quiesce_drained() ) ) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: wait_quiesce: %s Failed to quiesce drained messages\n",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME) );
        exit_status = ret;
        goto cleanup;
    }

    /*******************************************************************
     * If we are waiting for All Clear messages from peers wait on them.
     *******************************************************************/
    if( OMPI_SUCCESS != (ret = wait_quiesce_drain_ack() ) ) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: wait_quiesce: %s Failed to recv all drain ACKs\n",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME) );
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

static int wait_quiesce_drained(void)
{
    int ret, exit_status = OMPI_SUCCESS;
    ompi_crcp_bkmrk_pml_peer_ref_t *cur_peer_ref = NULL;
    ompi_crcp_bkmrk_pml_drain_message_ref_t * drain_msg_ref = NULL;
    ompi_crcp_bkmrk_pml_message_content_ref_t *content_ref = NULL;
    opal_list_item_t* item = NULL, *d_item = NULL, *d_next = NULL, *c_item = NULL, *c_next = NULL;
    bool prev_stall = false;

    /* Can we shortcut this? */

    OPAL_OUTPUT_VERBOSE((5, mca_crcp_bkmrk_component.super.output_handle,
                         "crcp:bkmrk: %s Waiting on %d messages to drain\n",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (int)quiesce_request_count));

    /*
     * Wait on all of the message to complete in any order
     * Created in ft_event_post_drained()
     */
    prev_stall = opal_cr_stall_check;
    opal_cr_stall_check = true;
    if( OMPI_SUCCESS != (ret = coord_request_wait_all(quiesce_request_count,
                                                      quiesce_requests,
                                                      quiesce_statuses) ) ) {
        exit_status = ret;
        goto cleanup;
    }
    opal_cr_stall_check = prev_stall;
    
    /*
     * Send ACKs to all peers
     *
     * Remove only the already posted members of the drained list.
     * All other elements need to be left in the list since we need
     * to match them as new receives come in.
     */
    for(item  = opal_list_get_first(&ompi_crcp_bkmrk_pml_peer_refs);
        item != opal_list_get_end(&ompi_crcp_bkmrk_pml_peer_refs);
        item  = opal_list_get_next(item) ) {
        cur_peer_ref = (ompi_crcp_bkmrk_pml_peer_ref_t*)item;

        if( OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL,
                                                        (ORTE_PROC_MY_NAME),
                                                        &(cur_peer_ref->proc_name)) ) {
            continue;
        }

        /*
         * Send ACK to peer if wanted
         */
        if( cur_peer_ref->ack_required ) {
            opal_buffer_t *buffer = NULL;
            size_t response = 1;

            OPAL_OUTPUT_VERBOSE((5, mca_crcp_bkmrk_component.super.output_handle,
                                 "crcp:bkmrk: %s --> %s Send ACKs to Peer\n",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&(cur_peer_ref->proc_name)) ));
            
            /* Send All Clear to Peer */
            if (NULL == (buffer = OBJ_NEW(opal_buffer_t))) {
                exit_status = OMPI_ERROR;
                goto cleanup;
            }

            PACK_BUFFER(buffer, response, 1, OPAL_SIZE, "");

            /* JJH - Performance Optimization? - Why not post all isends, then wait? */
            if ( 0 > ( ret = orte_rml.send_buffer(&(cur_peer_ref->proc_name), buffer, OMPI_CRCP_COORD_BOOKMARK_TAG, 0)) ) {
                exit_status = ret;
                goto cleanup;
            }
            if( NULL != buffer) {
                OBJ_RELEASE(buffer);
                buffer = NULL;
            }
        }

        cur_peer_ref->ack_required = false;

        /*
         * Remove already_posted drained items
         */
        for(d_item  = opal_list_get_first(&(cur_peer_ref->drained_list));
            d_item != opal_list_get_end(&(cur_peer_ref->drained_list));
            d_item  = d_next ) {
            drain_msg_ref = (ompi_crcp_bkmrk_pml_drain_message_ref_t*)d_item;
            d_next = opal_list_get_next(d_item);

            for(c_item  = opal_list_get_first(&(drain_msg_ref->msg_contents));
                c_item != opal_list_get_end(&(drain_msg_ref->msg_contents));
                c_item  = c_next ) {
                content_ref = (ompi_crcp_bkmrk_pml_message_content_ref_t*)c_item;
                c_next  = opal_list_get_next(c_item);

                /*
                 * This message has already been posted and drained in a previous
                 * checkpoint, do not do anything to it.
                 */
                if( content_ref->done ) {
                    continue;
                }

                if( content_ref->already_posted ) {
                    drain_message_remove(cur_peer_ref, drain_msg_ref, content_ref);

                    /* Match counts in traffic_message_create_drain_message() */
                    drain_msg_ref->active--;
                    drain_msg_ref->already_posted--;
                } else {
                    content_ref->done   = true;
                    content_ref->active = false;

                    /* Match counts in traffic_message_create_drain_message() */
                    drain_msg_ref->done++;
                    drain_msg_ref->active--;
                }
            }
        }
    }

 cleanup:
    if( NULL != quiesce_requests ) {
        free(quiesce_requests);
        quiesce_requests = NULL;
    }

    if( NULL != quiesce_statuses ) {
        free(quiesce_statuses);
        quiesce_statuses = NULL;
    }

    quiesce_request_count = 0;

    return exit_status;
}

static int coord_request_wait_all( size_t count,
                                   ompi_request_t ** requests,
                                   ompi_status_public_t ** statuses )
{
    int exit_status = OMPI_SUCCESS;
    ompi_status_public_t * status;
    ompi_request_t *req;
    size_t i;

    /*
     * Just wait on each request in order
     */
    for( i = 0; i < count; ++i) {
        req    = requests[i];
        status = statuses[i];

        coord_request_wait(req, status);

        OPAL_OUTPUT_VERBOSE((15, mca_crcp_bkmrk_component.super.output_handle,
                            "crcp:bkmrk: %s Request Wait: Done with idx %d of %d\n",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            (int)i, (int)count));
    }

    return exit_status;
}

static int coord_request_wait( ompi_request_t * req,
                               ompi_status_public_t * status)
{
    ompi_request_wait_completion(req);

    if( MPI_STATUS_IGNORE != status ) {
        status->MPI_TAG    = req->req_status.MPI_TAG;
        status->MPI_SOURCE = req->req_status.MPI_SOURCE;
        status->_cancelled = req->req_status._cancelled;
        OMPI_STATUS_SET_COUNT(&status->_ucount, &req->req_status._ucount);
    }

    return OMPI_SUCCESS;
}

static int wait_quiesce_drain_ack(void)
{
    opal_list_item_t* item = NULL;
    opal_list_item_t* next = NULL;
    ompi_crcp_bkmrk_pml_drain_message_ack_ref_t * drain_msg_ack;
    int num_outstanding;

    /* YYY JJH YYY Should we wait explicitly on the send requests pending first? */

    num_outstanding = opal_list_get_size(&drained_msg_ack_list);
    if(num_outstanding <= 0) {
        /*  Nothing to do */
        return OMPI_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((10, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: %s Waiting on %d Drain ACK messages\n",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        num_outstanding));

    while(0 < num_outstanding) {
        for(item  = opal_list_get_first(&drained_msg_ack_list);
            item != opal_list_get_end(&drained_msg_ack_list);
            item = next) {
            drain_msg_ack = (ompi_crcp_bkmrk_pml_drain_message_ack_ref_t*)item;
            next = opal_list_get_next(item);

            if(drain_msg_ack->complete) {
                num_outstanding--;
                opal_list_remove_item(&drained_msg_ack_list, &(drain_msg_ack->super) );
                HOKE_DRAIN_ACK_MSG_REF_RETURN(item);
                break;
            }
        }

        opal_event_loop(OPAL_EVLOOP_NONBLOCK);
    }
        
    /* Clear the ack queue if it isn't already clear (it should already be) */
    while (NULL != (item = opal_list_remove_first(&drained_msg_ack_list) ) ) {
        HOKE_DRAIN_ACK_MSG_REF_RETURN(item);
    }

    return OMPI_SUCCESS;
}

/* Paired with recv_bookmarks */
static int send_bookmarks(int peer_idx)
{
    ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref;
    orte_process_name_t peer_name;
    opal_buffer_t *buffer = NULL;
    int exit_status = OMPI_SUCCESS;
    int ret;

    START_TIMER(CRCP_TIMER_CKPT_EX_PEER_S);
    /*
     * Find the peer structure for this peer
     */
    peer_name.jobid  = ORTE_PROC_MY_NAME->jobid;
    peer_name.vpid   = peer_idx;

    if( NULL == (peer_ref = find_peer(peer_name))) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: send_bookmarks: Error: Could not find peer indexed %d\n",
                    peer_idx);
        exit_status = OMPI_ERROR;
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((15, mca_crcp_bkmrk_component.super.output_handle,
                         "crcp:bkmrk: %s --> %s Sending bookmark  (S[%6d] R[%6d])\n",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&peer_name),
                         peer_ref->total_msgs_sent,
                         peer_ref->total_msgs_recvd));

    /*
     * Send the bookmarks to peer
     */
    if (NULL == (buffer = OBJ_NEW(opal_buffer_t))) {
        exit_status = OMPI_ERROR;
        goto cleanup;
    }

    PACK_BUFFER(buffer, (peer_ref->total_msgs_sent),      1, OPAL_UINT32,
                "crcp:bkmrk: send_bookmarks: Unable to pack total_msgs_sent");
    PACK_BUFFER(buffer, (peer_ref->total_msgs_recvd),     1, OPAL_UINT32,
                "crcp:bkmrk: send_bookmarks: Unable to pack total_msgs_recvd");

    if ( 0 > ( ret = orte_rml.send_buffer(&peer_name, buffer, OMPI_CRCP_COORD_BOOKMARK_TAG, 0)) ) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: send_bookmarks: Failed to send bookmark to peer %s: Return %d\n",
                    ORTE_NAME_PRINT(&peer_name),
                    ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    if(NULL != buffer) {
        OBJ_RELEASE(buffer);
    }

    END_TIMER(CRCP_TIMER_CKPT_EX_PEER_S);
    DISPLAY_INDV_TIMER(CRCP_TIMER_CKPT_EX_PEER_S, peer_idx, 1);

    return exit_status;
}

/* Paired with send_bookmarks */
static int recv_bookmarks(int peer_idx)
{
    orte_process_name_t peer_name;
    int exit_status = OMPI_SUCCESS;
    int ret;

    START_TIMER(CRCP_TIMER_CKPT_EX_PEER_R);

    peer_name.jobid  = ORTE_PROC_MY_NAME->jobid;
    peer_name.vpid   = peer_idx;

    if ( 0 > (ret = orte_rml.recv_buffer_nb(&peer_name,
                                            OMPI_CRCP_COORD_BOOKMARK_TAG,
                                            0,
                                            recv_bookmarks_cbfunc,
                                            NULL) ) ) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: recv_bookmarks: Failed to post receive bookmark from peer %s: Return %d\n",
                    ORTE_NAME_PRINT(&peer_name),
                    ret);
        exit_status = ret;
        goto cleanup;
    }

    ++total_recv_bookmarks;

 cleanup:
    END_TIMER(CRCP_TIMER_CKPT_EX_PEER_R);
    /* JJH Doesn't make much sense to print this. The real bottleneck is always the send_bookmarks() */
    /*DISPLAY_INDV_TIMER(CRCP_TIMER_CKPT_EX_PEER_R, peer_idx, 1);*/

    return exit_status;
}

static void recv_bookmarks_cbfunc(int status,
                                  orte_process_name_t* sender,
                                  opal_buffer_t *buffer,
                                  orte_rml_tag_t tag,
                                  void* cbdata)
{
    ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref;
    int exit_status = OMPI_SUCCESS;
    int ret, tmp_int;
    orte_vpid_t peer_idx;

    peer_idx = sender->vpid;

    /*
     * Find the peer structure for this peer
     */
    if( NULL == (peer_ref = find_peer(*sender))) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: recv_bookmarks: Could not find peer indexed %d\n",
                    peer_idx);
        exit_status = OMPI_ERROR;
        goto cleanup;
    }

    UNPACK_BUFFER(buffer, tmp_int, 1, OPAL_UINT32,
                  "crcp:bkmrk: recv_bookmarks: Unable to unpack total_msgs_sent");
    peer_ref->matched_msgs_sent = tmp_int;

    UNPACK_BUFFER(buffer, tmp_int, 1, OPAL_UINT32,
                  "crcp:bkmrk: recv_bookmarks: Unable to unpack total_msgs_recvd");
    peer_ref->matched_msgs_recvd = tmp_int;

    OPAL_OUTPUT_VERBOSE((15, mca_crcp_bkmrk_component.super.output_handle,
                         "crcp:bkmrk: %s <-- %s Received bookmark (S[%6d] R[%6d]) vs. (S[%6d] R[%6d])\n",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender),
                         peer_ref->matched_msgs_sent,
                         peer_ref->matched_msgs_recvd,
                         peer_ref->total_msgs_sent,
                         peer_ref->total_msgs_recvd));

 cleanup:
    --total_recv_bookmarks;

    return;
}

static int send_msg_details(ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                            int total_sent, int total_matched)
{
    int ret, exit_status = OMPI_SUCCESS;
    ompi_crcp_bkmrk_pml_drain_message_ack_ref_t * d_msg_ack = NULL;
    opal_list_t *search_list = NULL;
    opal_list_item_t* msg_item  = NULL;
    bool finished;
    int pass_num = 1;
    int need, found;
    int total_details_sent = 0;
    int num_matches = 0;
    int p_total_found = 0;

    need = total_sent - total_matched;
    found = 0;
    finished = false;
    assert(need > 0);

    START_TIMER(CRCP_TIMER_CKPT_CHECK_PEER_S);

    /*
     * Check the 'send_list' for this peer
     */
    search_list = &(peer_ref->send_list);
    pass_num = 1;

 SEARCH_AGAIN:
    for(msg_item  = opal_list_get_last(search_list);
        msg_item != opal_list_get_begin(search_list);
        msg_item  = opal_list_get_prev(msg_item) ) {
        ompi_crcp_bkmrk_pml_traffic_message_ref_t * msg_ref;
        msg_ref = (ompi_crcp_bkmrk_pml_traffic_message_ref_t*)msg_item;

        num_matches = 0;

        OPAL_OUTPUT_VERBOSE((10, mca_crcp_bkmrk_component.super.output_handle,
                             "crcp:bkmrk: send_msg_details: Stage 1: [M/A/D/AD] [%3d/%3d/%3d/%3d] (%s)",
                             msg_ref->matched, msg_ref->active, msg_ref->done, msg_ref->active_drain,
                             (msg_ref->msg_type == COORD_MSG_TYPE_B_SEND ? " Send" :
                              (msg_ref->msg_type == COORD_MSG_TYPE_I_SEND ? "iSend" : "pSend"))
                             ));

        /* If this message has not seen any traffic, then skip it */
        if( 0 >= (msg_ref->active + msg_ref->done) ) {
            continue;
        }
        /* YYY JJH YYY Keep this as a sanity check?  if( msg_ref->matched >= (msg_ref->active + msg_ref->done) ) { continue; } */

        if(OMPI_SUCCESS != (ret = do_send_msg_detail(peer_ref, msg_ref, &num_matches, &p_total_found, &finished)) ) {
            opal_output(mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: send_msg_details: %s --> %s Failed to send message details to peer. Return %d\n",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                        ret);
        }

        OPAL_OUTPUT_VERBOSE((10, mca_crcp_bkmrk_component.super.output_handle,
                             "crcp:bkmrk: send_msg_details: Stage 2: [M/A/D/AD] [%3d/%3d/%3d/%3d] (%s) [%3d, %3d, %s] [%3d, %3d]",
                             msg_ref->matched, msg_ref->active, msg_ref->done, msg_ref->active_drain,
                             (msg_ref->msg_type == COORD_MSG_TYPE_B_SEND ? " Send" :
                              (msg_ref->msg_type == COORD_MSG_TYPE_I_SEND ? "iSend" : "pSend")),
                             num_matches, p_total_found, (finished ? "T" : "F"),
                             total_details_sent, found
                             ));

        total_details_sent += num_matches;
        if(0 < num_matches ) {
            found += num_matches;
        }
        if(finished) {
            goto ALL_SENT;
        }
    }

    /*
     * We tried the 'send_list' and need more,
     * so match off the 'isend_list'
     */
    if( 1 == pass_num ) {
        search_list = &(peer_ref->isend_list);
        pass_num = 2;
        goto SEARCH_AGAIN;
    }

    /*
     * We tried the 'send_list' and 'isend_list' and need more,
     * so match off the 'send_init_list'
     */
    if( 2 == pass_num ) {
        search_list = &(peer_ref->send_init_list);
        pass_num = 3;
        goto SEARCH_AGAIN;
    }

 ALL_SENT:
    if( need > found ) {
        OPAL_OUTPUT_VERBOSE((10, mca_crcp_bkmrk_component.super.output_handle,
                            "crcp:bkmrk: send_msg_details: ERROR: ****** Need (%d) vs Found (%d)",
                            need, found));
    }
    assert(need <= found);

    /* Prepare to post a Recv for the ACK All Clear signal from the peer
     * which is sent when they have finished receiving all of the 
     * inflight messages into a local buffer
     */
    HOKE_DRAIN_ACK_MSG_REF_ALLOC(d_msg_ack, ret);
    d_msg_ack->peer.jobid  = peer_ref->proc_name.jobid;
    d_msg_ack->peer.vpid   = peer_ref->proc_name.vpid;
    d_msg_ack->complete    = false;
    opal_list_append(&drained_msg_ack_list, &(d_msg_ack->super));
    OPAL_OUTPUT_VERBOSE((10, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: %s <-> %s Message Inflight! Will wait on ACK from this peer.\n",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&(peer_ref->proc_name))));

    END_TIMER(CRCP_TIMER_CKPT_CHECK_PEER_S);
    DISPLAY_INDV_TIMER(CRCP_TIMER_CKPT_CHECK_PEER_S, peer_ref->proc_name.vpid, total_details_sent);

    return exit_status;
}

static int do_send_msg_detail(ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                              ompi_crcp_bkmrk_pml_traffic_message_ref_t*msg_ref,
                              int *num_matches,
                              int *total_found,
                              bool *finished)
{
    int ret, exit_status = OMPI_SUCCESS;
    opal_buffer_t *buffer = NULL;
    int32_t recv_response = RECV_MATCH_RESP_ERROR;
    int32_t num_resolv = -1;
    int32_t p_total_found = -1;
    int comm_my_rank = -1;
    int total_sent;

    *num_matches = 0;
    *total_found = 0;;
    *finished    = false;

    if( NULL != buffer) {
        OBJ_RELEASE(buffer);
        buffer = NULL;
    }

    if (NULL == (buffer = OBJ_NEW(opal_buffer_t))) {
        exit_status = OMPI_ERROR;
        goto cleanup;
    }

    /*
     * Send:
     * - Communicator Context ID
     * - My Rank in Communicator
     */
    comm_my_rank  = ompi_comm_rank(msg_ref->comm);

    PACK_BUFFER(buffer, msg_ref->comm->c_contextid, 1, OPAL_UINT32,
                "crcp:bkmrk: send_msg_details: Unable to pack communicator ID");
    PACK_BUFFER(buffer, comm_my_rank, 1, OPAL_INT,
                "crcp:bkmrk: send_msg_details: Unable to pack comm rank ID");

    /*
     * Send:
     * - Message tag
     * - Message count
     * - Message Datatype size
     */
    PACK_BUFFER(buffer, msg_ref->tag, 1, OPAL_INT,
                "crcp:bkmrk: send_msg_details: Unable to pack tag");
    PACK_BUFFER(buffer, msg_ref->count, 1, OPAL_SIZE,
                "crcp:bkmrk: send_msg_details: Unable to pack count");
    PACK_BUFFER(buffer, msg_ref->ddt_size, 1, OPAL_SIZE,
                "crcp:bkmrk: send_msg_details: Unable to pack datatype size");

    /*
     * Send:
     * - Message done
     * - Message active
     */
    total_sent = msg_ref->done + msg_ref->active;
    PACK_BUFFER(buffer, total_sent, 1, OPAL_INT,
                "crcp:bkmrk: send_msg_details: Unable to pack done+active count");

    /*
     * Do the send...
     */
    if ( 0 > ( ret = orte_rml.send_buffer(&peer_ref->proc_name, buffer,
                                          OMPI_CRCP_COORD_BOOKMARK_TAG, 0)) ) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: do_send_msg_detail: Unable to send message details to peer %s: Return %d\n",
                    ORTE_NAME_PRINT(&peer_ref->proc_name),
                    ret);
            
        exit_status = OMPI_ERROR;
        goto cleanup;
    }
        
    if( NULL != buffer) {
        OBJ_RELEASE(buffer);
        buffer = NULL;
    }
            
    /*
     * Check return value from peer to see if we found a match.
     */
    if (NULL == (buffer = OBJ_NEW(opal_buffer_t))) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }
        
    /*
     * Recv the ACK msg
     */
    if ( 0 > (ret = orte_rml.recv_buffer(&peer_ref->proc_name, buffer,
                                         OMPI_CRCP_COORD_BOOKMARK_TAG, 0) ) ) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: do_send_msg_detail: %s --> %s Failed to receive ACK buffer from peer. Return %d\n",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                    ret);
        exit_status = ret;
        goto cleanup;
    }

    UNPACK_BUFFER(buffer, recv_response, 1, OPAL_UINT32,
                  "crcp:bkmrk: send_msg_details: Failed to unpack the ACK from peer buffer.");
    UNPACK_BUFFER(buffer, num_resolv,  1, OPAL_UINT32,
                  "crcp:bkmrk: send_msg_details: Failed to unpack the num_resolv from peer buffer.");
    UNPACK_BUFFER(buffer, p_total_found, 1, OPAL_UINT32,
                  "crcp:bkmrk: send_msg_details: Failed to unpack the total_found from peer buffer.");
    
    /* Mark message as matched */
    msg_ref->matched += num_resolv;
    *num_matches      = num_resolv;
    *total_found      = p_total_found;

    /*
     *
     */
    if( RECV_MATCH_RESP_DONE == recv_response ) {
        *finished    = true;
    }
    else if( RECV_MATCH_RESP_MORE == recv_response ) {
        *finished    = false;
    }

    OPAL_OUTPUT_VERBOSE((15, mca_crcp_bkmrk_component.super.output_handle,
                         "**************************\n"));
    OPAL_OUTPUT_VERBOSE((15, mca_crcp_bkmrk_component.super.output_handle,
                         "send_msg_details: %d, %d = %s [%d / %d]\n",
                         *num_matches, *total_found,
                         (*finished ? "Done" : "Continue..."),
                         msg_ref->done, msg_ref->active));
    TRAFFIC_MSG_DUMP_MSG_INDV(15, (msg_ref, "", false));
    OPAL_OUTPUT_VERBOSE((15, mca_crcp_bkmrk_component.super.output_handle,
                         "**************************\n"));

 cleanup:
    return exit_status;
}

/*
 * Recv message details from peer
 * - matched with send_msg_details()
 */
static int recv_msg_details(ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                            int total_recv, int total_matched)
{
    int need, found;
    int response;
    int exit_status = OMPI_SUCCESS;
    int ret;
    int total_details_recv = 0;

    need  = total_recv - total_matched;
    found = 0;

    assert( need > 0);

    START_TIMER(CRCP_TIMER_CKPT_CHECK_PEER_R);

    /*
     * While we are still looking for messages to drain
     */
    while(need > found) {
        uint32_t p_comm_id;
        size_t p_count;
        size_t p_datatype_size;
        int p_rank;
        int p_tag;
        int p_num_sent;
        int num_resolved = 0;

        /*
         * Receive the message details from peer
         */
        if( OMPI_SUCCESS != (ret = do_recv_msg_detail(peer_ref,
                                                      &p_rank, &p_comm_id,
                                                      &p_tag, &p_count,
                                                      &p_datatype_size,
                                                      &p_num_sent)) ) {
            opal_output(mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: recv_msg_details: %s <-- %s "
                        "Failed to receive message detail from peer. Return %d\n",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                        ret);
            exit_status = ret;
            goto cleanup;
        }

        /*
         * Determine if we have matched this message or not.
         * Also take approprate action.
         */
        num_resolved = 0;
        if( OMPI_SUCCESS != (ret = do_recv_msg_detail_check_drain(peer_ref,
                                                                  p_rank, p_comm_id,
                                                                  p_tag, p_count,
                                                                  p_datatype_size,
                                                                  p_num_sent,
                                                                  &num_resolved)) ) {
            opal_output(mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: recv_msg_details: %s <-- %s "
                        "Failed to check message detail from peer. Return %d\n",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                        ret);
            exit_status = ret;
            goto cleanup;
        }

        found += num_resolved;
        total_details_recv += num_resolved;

        OPAL_OUTPUT_VERBOSE((10, mca_crcp_bkmrk_component.super.output_handle,
                             "crcp:bkmrk: %s <-- %s Recv Detail: Stage --: [%3d / %3d] [%3d, %3d, %s]",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                             need, found,
                             num_resolved, total_details_recv,
                             ( need <= found ? "T" : "F") ));

        /* If we do not need any more, respond DONE  */
        if( need <= found ) {
            response = RECV_MATCH_RESP_DONE; /* All done */
        }
        /* Otherwise respond need more */
        else {
            response = RECV_MATCH_RESP_MORE;
        }

        if(OMPI_SUCCESS != (ret = do_recv_msg_detail_resp(peer_ref, response, num_resolved, found))) {
            opal_output(mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: recv_msg_details: %s <-- %s Failed to respond to peer. Return %d\n",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                        ret);
            exit_status = ret;
            goto cleanup;
        }
    }

 cleanup:

    END_TIMER(CRCP_TIMER_CKPT_CHECK_PEER_R);
    DISPLAY_INDV_TIMER(CRCP_TIMER_CKPT_CHECK_PEER_R, peer_ref->proc_name.vpid, total_details_recv);

    return exit_status;
}

static int do_recv_msg_detail(ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                              int *rank, uint32_t *comm_id, int *tag,
                              size_t *count, size_t *datatype_size,
                              int *p_num_sent)
{
    opal_buffer_t * buffer = NULL;
    int exit_status = OMPI_SUCCESS;
    int ret;

    if (NULL == (buffer = OBJ_NEW(opal_buffer_t))) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    /*
     * Recv the msg
     */
    if ( 0 > (ret = orte_rml.recv_buffer(&peer_ref->proc_name, buffer, OMPI_CRCP_COORD_BOOKMARK_TAG, 0) ) ) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: do_recv_msg_detail: %s <-- %s Failed to receive buffer from peer. Return %d\n",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                    ret);
        exit_status = ret;
        goto cleanup;
    }

    /* Pull out the communicator ID */
    UNPACK_BUFFER(buffer, (*comm_id), 1, OPAL_UINT32,
                  "crcp:bkmrk: recv_msg_details: Failed to unpack the communicator ID");
    UNPACK_BUFFER(buffer, (*rank), 1, OPAL_INT,
                  "crcp:bkmrk: recv_msg_details: Failed to unpack the communicator rank ID");
    
    /* Pull out the message details */
    UNPACK_BUFFER(buffer, (*tag), 1, OPAL_INT,
                  "crcp:bkmrk: recv_msg_details: Failed to unpack the tag");
    UNPACK_BUFFER(buffer, (*count), 1, OPAL_SIZE,
                  "crcp:bkmrk: recv_msg_details: Failed to unpack the count");
    UNPACK_BUFFER(buffer, (*datatype_size), 1, OPAL_SIZE,
                  "crcp:bkmrk: recv_msg_details: Failed to unpack the datatype size");

    /* Pull out the counts */
    UNPACK_BUFFER(buffer, (*p_num_sent), 1, OPAL_INT,
                  "crcp:bkmrk: recv_msg_details: Failed to unpack the sent count");

 cleanup:
    if( NULL != buffer) {
        OBJ_RELEASE(buffer);
        buffer = NULL;
    }

    return exit_status;
}

static int do_recv_msg_detail_check_drain(ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                                    int rank, uint32_t comm_id, int tag,
                                    size_t count, size_t datatype_size,
                                    int p_num_sent,
                                    int *num_resolved)
{
    int ret, exit_status = OMPI_SUCCESS;
    ompi_crcp_bkmrk_pml_traffic_message_ref_t *posted_tmp_msg_ref   = NULL;
    ompi_crcp_bkmrk_pml_traffic_message_ref_t *posted_recv_msg_ref  = NULL;
    ompi_crcp_bkmrk_pml_traffic_message_ref_t *posted_irecv_msg_ref = NULL;
    ompi_crcp_bkmrk_pml_traffic_message_ref_t *posted_precv_msg_ref = NULL;
    ompi_crcp_bkmrk_pml_traffic_message_ref_t *posted_unknown_recv_msg_ref  = NULL;
    ompi_crcp_bkmrk_pml_traffic_message_ref_t *posted_unknown_precv_msg_ref = NULL;
    /* Number of messages left not-matched */
    int num_left_unresolved = 0;
    /* Number of active messages need to be drained */
    int num_still_active = 0;
    /* Number of drain messages posted */
    int num_posted = 0;

    *num_resolved = 0;
    num_left_unresolved = p_num_sent;

    OPAL_OUTPUT_VERBOSE((10, mca_crcp_bkmrk_component.super.output_handle,
                         "crcp:bkmrk: %s <-- %s "
                         "Stage 0: Ck.Drain: [TR %3d/MS %3d] sent %4d, unres %4d, res %4d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                         peer_ref->total_msgs_recvd,
                         peer_ref->matched_msgs_sent,
                         p_num_sent,
                         num_left_unresolved,
                         *num_resolved));
    TRAFFIC_MSG_DUMP_PEER(15, (peer_ref, "Recv Check...", true));

    /*
     * Find all references to this message signature.
     */
    ret = traffic_message_find_recv(peer_ref,                                 /* Peer to resolve with */
                                    rank, comm_id, tag, count, datatype_size, /* Message signature */
                                    &posted_recv_msg_ref,                     /* One of 5 lists where this signature could match */
                                    &posted_irecv_msg_ref,
                                    &posted_precv_msg_ref,
                                    &posted_unknown_recv_msg_ref,
                                    &posted_unknown_precv_msg_ref);
    if( OMPI_SUCCESS != ret) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: recv_msg_detail_check: %s -- %s "
                    "Failed to determine if we have received this message. Return %d\n",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                    ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Peer sent 'p_num_sent'.
     * For each msg_ref from recv lists:
     *   Mark all as 'matched'
     *   Subtract recv->{'active' + 'done'} from 'p_num_sent'
     *   If recv->active
     *     need to make sure to drain these and possibly stall
     * If 'p_num_sent' > 0
     *   Post outstanding messages in drain queue
     */

    /*
     * First pass: Count all 'done'
     */
    if( NULL != posted_recv_msg_ref ) {
        posted_recv_msg_ref->matched += posted_recv_msg_ref->done;
        num_left_unresolved          -= posted_recv_msg_ref->done;
        TRAFFIC_MSG_DUMP_MSG_INDV(11, (posted_recv_msg_ref, "Ck.  Recv", true));
    }
    if( NULL != posted_irecv_msg_ref ) {
        posted_irecv_msg_ref->matched += posted_irecv_msg_ref->done;
        num_left_unresolved           -= posted_irecv_msg_ref->done;
        TRAFFIC_MSG_DUMP_MSG_INDV(11, (posted_irecv_msg_ref, "Ck. iRecv", true));
    }
    if( NULL != posted_precv_msg_ref ) {
        posted_precv_msg_ref->matched += posted_precv_msg_ref->done;
        num_left_unresolved           -= posted_precv_msg_ref->done;
        TRAFFIC_MSG_DUMP_MSG_INDV(11, (posted_precv_msg_ref, "Ck. pRecv", true));
    }
    if( NULL != posted_unknown_recv_msg_ref ) {
        posted_unknown_recv_msg_ref->matched += posted_unknown_recv_msg_ref->done;
        num_left_unresolved                  -= posted_unknown_recv_msg_ref->done;
        TRAFFIC_MSG_DUMP_MSG_INDV(11, (posted_unknown_recv_msg_ref, "Ck. uRecv", true));
    }
    if( NULL != posted_unknown_precv_msg_ref ) {
        posted_unknown_precv_msg_ref->matched += posted_unknown_precv_msg_ref->done;
        num_left_unresolved                   -= posted_unknown_precv_msg_ref->done;
        TRAFFIC_MSG_DUMP_MSG_INDV(11, (posted_unknown_precv_msg_ref, "Ck. upRecv", true));
    }

    OPAL_OUTPUT_VERBOSE((10, mca_crcp_bkmrk_component.super.output_handle,
                         "crcp:bkmrk: %s <-- %s "
                         "Stage 1: Ck.Drain: [TR %3d/MS %3d] sent %4d, unres %4d, res %4d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                         peer_ref->total_msgs_recvd,
                         peer_ref->matched_msgs_sent,
                         p_num_sent,
                         num_left_unresolved,
                         *num_resolved));

    /* Short cut if we have completed everything necessary
     * This should never happen since we are here because there is a message
     * that was sent that has not been started.
     */
    if( num_left_unresolved <= 0 ) {
        goto cleanup;
    }

    /*
     * Next pass: Count all 'active'
     * - if active > unresolved then match all unresolved, and jump to end
     * - if active < unresolved then match all active, and continue looking
     */
    if( NULL != posted_recv_msg_ref ) {
        if( posted_recv_msg_ref->active > num_left_unresolved ) {
            posted_recv_msg_ref->matched += num_left_unresolved;
            num_still_active             += num_left_unresolved;
            num_left_unresolved           = 0;
        } else {
            posted_recv_msg_ref->matched += posted_recv_msg_ref->active;
            num_still_active             += posted_recv_msg_ref->active;
            num_left_unresolved          -= posted_recv_msg_ref->active;
        }
    }
    if( num_left_unresolved > 0 && NULL != posted_irecv_msg_ref ) {
        if( posted_irecv_msg_ref->active > num_left_unresolved ) {
            posted_irecv_msg_ref->matched += num_left_unresolved;
            num_still_active              += num_left_unresolved;
            num_left_unresolved            = 0;
        } else {
            posted_irecv_msg_ref->matched += posted_irecv_msg_ref->active;
            num_still_active              += posted_irecv_msg_ref->active;
            num_left_unresolved           -= posted_irecv_msg_ref->active;
        }
    }
    if( num_left_unresolved > 0 && NULL != posted_precv_msg_ref ) {
        if( posted_precv_msg_ref->active > num_left_unresolved ) {
            posted_precv_msg_ref->matched += num_left_unresolved;
            num_still_active              += num_left_unresolved;
            num_left_unresolved            = 0;
        } else {
            posted_precv_msg_ref->matched += posted_precv_msg_ref->active;
            num_still_active              += posted_precv_msg_ref->active;
            num_left_unresolved           -= posted_precv_msg_ref->active;
        }
    }
    if( num_left_unresolved > 0 && NULL != posted_unknown_recv_msg_ref ) {
        if( posted_unknown_recv_msg_ref->active > num_left_unresolved ) {
            posted_unknown_recv_msg_ref->matched += num_left_unresolved;
            num_still_active                     += num_left_unresolved;
            num_left_unresolved                   = 0;
        } else {
            posted_unknown_recv_msg_ref->matched += posted_unknown_recv_msg_ref->active;
            num_still_active                     += posted_unknown_recv_msg_ref->active;
            num_left_unresolved                  -= posted_unknown_recv_msg_ref->active;
        }
    }
    if( num_left_unresolved > 0 && NULL != posted_unknown_precv_msg_ref ) {
        if( posted_unknown_precv_msg_ref->active > num_left_unresolved ) {
            posted_unknown_precv_msg_ref->matched += num_left_unresolved;
            num_still_active                      += num_left_unresolved;
            num_left_unresolved                    = 0;
        } else {
            posted_unknown_precv_msg_ref->matched += posted_unknown_precv_msg_ref->active;
            num_still_active                      += posted_unknown_precv_msg_ref->active;
            num_left_unresolved                   -= posted_unknown_precv_msg_ref->active;
        }
    }

    /*
     * If we happen to have more active Recvs than the peer has posted sends, then
     * we need to reset the number still active to reflect that only a subset
     * of the active sends should be drained.
     */
    OPAL_OUTPUT_VERBOSE((10, mca_crcp_bkmrk_component.super.output_handle,
                         "crcp:bkmrk: %s <-- %s "
                         "Stage 2: Ck.Drain: [TR %3d/MS %3d] sent %4d, unres %4d, res %4d, active %4d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                         peer_ref->total_msgs_recvd,
                         peer_ref->matched_msgs_sent,
                         p_num_sent,
                         num_left_unresolved,
                         *num_resolved,
                         num_still_active
                         ));

    /*
     * Check the math at this point, and make sure we did not mess up above.
     */
    if(num_left_unresolved < 0 ) {
        ERROR_SHOULD_NEVER_HAPPEN_ARG("crcp:bkmrk: Ck.Drain: Unresolved (%3d) < 0", num_left_unresolved);
        exit_status = OMPI_ERROR;
        goto cleanup;
    }

    /*
     * Fast Track: If there are no outstanding messages to post, and nothing 'active'
     *  If all the matched messages were found 'done' (none were 'active')
     *  -> Nothing to do.
     */
    if( num_left_unresolved <= 0 &&
        num_still_active    <= 0) {
        goto cleanup;
    }

    /*
     * Stage 3: Resolve 'active' messages by posting a drain message for each
     *  -> then we need to make sure to wait for them to complete before the checkpoint
     *  -> Create a drain message
     *  -> Point the 'request' at it
     *  -> Make sure not to post this message to be drained, but just wait on the request.
     */
    if( num_still_active > 0 ) {
        /*
         * If this is the current blocking recv, then we need to stall for it to
         * complete properly.
         * - Only applies to Blocking Recv.
         */
        if( NULL != posted_recv_msg_ref ) {
            /* Is this the signature of the current blocking recv? */
            if (current_msg_id         == posted_recv_msg_ref->msg_id  &&
                COORD_MSG_TYPE_B_RECV  == posted_recv_msg_ref->msg_type) {
                OPAL_OUTPUT_VERBOSE((10, mca_crcp_bkmrk_component.super.output_handle,
                                     "crcp:bkmrk: %s <-- %s "
                                     "Recv Check: Found a message that is 'active'! Prepare to STALL.\n",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&(peer_ref->proc_name)) ));
                stall_for_completion = true;
            }
            else {
                OPAL_OUTPUT_VERBOSE((10, mca_crcp_bkmrk_component.super.output_handle,
                                     "crcp:bkmrk: %s <-- %s "
                                     "Recv Check: Found a message that is 'active', but is not the current recv! "
                                     "No stall required [%3d, %3d, %3d, %3d].\n",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                                     (int)current_msg_id,
                                     (int)current_msg_type,
                                     (int)posted_recv_msg_ref->msg_id,
                                     (int)posted_recv_msg_ref->msg_type));
            }
        }

        /*
         * Construct a message for draining for each active message.
         * This message will *not* be posted for draining since it is already
         * posted in the system. We will simply wait for it to complete.
         * - Only applies to messages that are not Blocking Recv
         */
        traffic_message_create_drain_message(false, num_still_active,
                                             peer_ref,
                                             &posted_recv_msg_ref,
                                             &num_posted);
        num_still_active -= num_posted;
        *num_resolved    += num_posted;
        peer_ref->total_msgs_recvd += num_posted;

        traffic_message_create_drain_message(false, num_still_active,
                                             peer_ref,
                                             &posted_irecv_msg_ref,
                                             &num_posted);
        num_still_active -= num_posted;
        *num_resolved    += num_posted;
        peer_ref->total_msgs_recvd += num_posted;

        traffic_message_create_drain_message(false, num_still_active,
                                             peer_ref,
                                             &posted_precv_msg_ref,
                                             &num_posted);
        num_still_active -= num_posted;
        *num_resolved    += num_posted;
        peer_ref->total_msgs_recvd += num_posted;

        traffic_message_create_drain_message(false, num_still_active,
                                             peer_ref,
                                             &posted_unknown_recv_msg_ref,
                                             &num_posted);
        num_still_active -= num_posted;
        *num_resolved    += num_posted;
        peer_ref->total_msgs_recvd += num_posted;

        traffic_message_create_drain_message(false, num_still_active,
                                             peer_ref,
                                             &posted_unknown_precv_msg_ref,
                                             &num_posted);
        num_still_active -= num_posted;
        *num_resolved    += num_posted;
        peer_ref->total_msgs_recvd += num_posted;
    }

    OPAL_OUTPUT_VERBOSE((10, mca_crcp_bkmrk_component.super.output_handle,
                         "crcp:bkmrk: %s <-- %s "
                         "Stage 3: Ck.Drain: [TR %3d/MS %3d] sent %4d, unres %4d, res %4d, active %4d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                         peer_ref->total_msgs_recvd,
                         peer_ref->matched_msgs_sent,
                         p_num_sent,
                         num_left_unresolved,
                         *num_resolved,
                         num_still_active
                         ));

    /*
     * Post all unresolved messages to the drain queue
     *  - Create a new message to drain
     *  - Notify peer of resolution of N messages
     */
    if( num_left_unresolved > 0 ) {
        /* Create a stamp for the drained message */
        CREATE_NEW_MSG(posted_tmp_msg_ref, COORD_MSG_TYPE_I_RECV,
                       count, datatype_size, tag, rank,
                       ompi_comm_lookup(comm_id),
                       peer_ref->proc_name.jobid,
                       peer_ref->proc_name.vpid);

        traffic_message_create_drain_message(true, num_left_unresolved,
                                             peer_ref,
                                             &posted_tmp_msg_ref,
                                             &num_posted);
        num_left_unresolved  -= num_posted;
        *num_resolved        += num_posted;
        peer_ref->total_msgs_recvd += num_posted;

        HOKE_TRAFFIC_MSG_REF_RETURN(posted_tmp_msg_ref);
    }

    OPAL_OUTPUT_VERBOSE((10, mca_crcp_bkmrk_component.super.output_handle,
                         "crcp:bkmrk: %s <-- %s "
                         "Stage 4: Ck.Drain: [TR %3d/MS %3d] sent %4d, unres %4d, res %4d, active %4d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                         peer_ref->total_msgs_recvd,
                         peer_ref->matched_msgs_sent,
                         p_num_sent,
                         num_left_unresolved,
                         *num_resolved,
                         num_still_active
                         ));

    /* YYY JJH YYY Should we check for no-action? */
 cleanup:
    return exit_status;
}

static int do_recv_msg_detail_resp(ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref,
                                   int resp, int num_resolv, int total_found)
{
    opal_buffer_t * buffer = NULL;
    int exit_status = OMPI_SUCCESS;
    int ret;

    if (NULL == (buffer = OBJ_NEW(opal_buffer_t))) {
        exit_status = OMPI_ERROR;
        goto cleanup;
    }

    PACK_BUFFER(buffer, resp, 1, OPAL_UINT32,
                "crcp:bkmrk: recv_msg_details: Unable to ask peer for more messages");
    PACK_BUFFER(buffer, num_resolv, 1, OPAL_UINT32,
                "crcp:bkmrk: recv_msg_details: Unable to ask peer for more messages");
    PACK_BUFFER(buffer, total_found, 1, OPAL_UINT32,
                "crcp:bkmrk: recv_msg_details: Unable to ask peer for more messages");
        
    if ( 0 > ( ret = orte_rml.send_buffer(&peer_ref->proc_name, buffer, OMPI_CRCP_COORD_BOOKMARK_TAG, 0)) ) {
        opal_output(mca_crcp_bkmrk_component.super.output_handle,
                    "crcp:bkmrk: recv_msg_detail_resp: Unable to send message detail response to peer %s: Return %d\n",
                    ORTE_NAME_PRINT(&peer_ref->proc_name),
                    ret);
        exit_status = OMPI_ERROR;
        goto cleanup;
    }

 cleanup:
    if( NULL != buffer) {
        OBJ_RELEASE(buffer);
        buffer = NULL;
    }

    return exit_status;
}


/************************************************
 * Timer Utility Functions
 ************************************************/
static void start_time(int idx) {
    if(idx < CRCP_TIMER_MAX ) {
        timer_start[idx] = get_time();
    }
}

static void end_time(int idx) {
    if(idx < CRCP_TIMER_MAX ) {
        timer_end[idx] = get_time();
    }
}

static double get_time() {
    double wtime;

#if OPAL_TIMER_USEC_NATIVE
    wtime = (double)opal_timer_base_get_usec() / 1000000.0;
#else
    struct timeval tv;
    gettimeofday(&tv, NULL);
    wtime = tv.tv_sec;
    wtime += (double)tv.tv_usec / 1000000.0;
#endif

    return wtime;
}

static void clear_timers(void) {
    int i;
    for(i = 0; i < CRCP_TIMER_MAX; ++i) {
        timer_start[i] = 0.0;
        timer_end[i]   = 0.0;
    }
}

static void display_all_timers(int state) {
    bool report_ready = false;
    double barrier_start, barrier_stop;
    int i;

    if( 0 != ORTE_PROC_MY_NAME->vpid ) {
        if( 2 > timing_enabled ) {
            return;
        }
        else if( 2 == timing_enabled ) {
            orte_grpcomm.barrier();
            return;
        }
    }

    for( i = 0; i < CRCP_TIMER_MAX; ++i) {
        if(timer_end[i] > 0.001) {
            report_ready = true;
        }
    }
    if( !report_ready ) {
        return;
    }

    opal_output(0, "crcp:bkmrk: timing(%20s): ******************** Begin: [State = %12s]\n", "Summary", opal_crs_base_state_str(state));
    for( i = 0; i < CRCP_TIMER_MAX; ++i) {
        display_indv_timer_core(i, 0, 0, false);
    }

    if( timing_enabled >= 2) {
        barrier_start = get_time();
        orte_grpcomm.barrier();
        barrier_stop = get_time();
        opal_output(0,
                    "crcp:bkmrk: timing(%20s): %20s = %10.2f s\n",
                    "",
                    "Group Barrier",
                    (barrier_stop - barrier_start));
    }

    opal_output(0, "crcp:bkmrk: timing(%20s): ******************** End:   [State = %12s]\n", "Summary", opal_crs_base_state_str(state));
}

static void display_indv_timer(int idx, int proc, int msgs) {
    display_indv_timer_core(idx, proc, msgs, true);
}

static void display_indv_timer_core(int idx, int proc, int msgs, bool direct) {
    double diff = timer_end[idx] - timer_start[idx];
    char * str = NULL;

    if( 0 != ORTE_PROC_MY_NAME->vpid && timing_enabled < 3 ) {
        return;
    }

    /* Only display the timer if an end value was set */
    if(timer_end[idx] <= 0.001) {
        return;
    }

    switch(idx) {
    case CRCP_TIMER_CKPT_EX_PEER_S:
    case CRCP_TIMER_CKPT_EX_PEER_R:
    case CRCP_TIMER_CKPT_CHECK_PEER_S:
    case CRCP_TIMER_CKPT_CHECK_PEER_R:
        /* These timers do not mean anything in the aggregate, so only display
         * them when directly asked for */
        if( direct && timing_enabled >= 2) {
            asprintf(&str, "Proc %2d, Msg %5d", proc, msgs);
        } else {
            return;
        }
        break;
    default:
        str = strdup("");
        break;
    }

    opal_output(0,
                "crcp:bkmrk: timing(%20s): %20s = %10.2f s\n",
                str,
                timer_label[idx],
                diff);
    free(str);
    str = NULL;
}

/**************** Message Dump functionality ********************/
#if OPAL_ENABLE_DEBUG
static void traffic_message_dump_msg_content_indv(ompi_crcp_bkmrk_pml_message_content_ref_t * content_ref)
{
    OPAL_OUTPUT_VERBOSE((10, mca_crcp_bkmrk_component.super.output_handle,
                         "\t\t(%3d) Content: [A/D/P/Dr] [%s / %s / %s /%s]",
                         (int)content_ref->msg_id,
                         (content_ref->active ? "T" : "F"),
                         (content_ref->done ? "T" : "F"),
                         (content_ref->already_posted ? "T" : "F"),
                         (content_ref->already_drained ? "T" : "F")));
}

static void traffic_message_dump_msg_indv(ompi_crcp_bkmrk_pml_traffic_message_ref_t * msg_ref, char * msg, bool vshort)
{
    ompi_crcp_bkmrk_pml_message_content_ref_t  *content_ref = NULL;
    opal_list_item_t* cont_item = NULL;
    char * type_name = NULL;

    switch(msg_ref->msg_type) {
    case COORD_MSG_TYPE_B_SEND:
        type_name = strdup(" Send");
        break;
    case COORD_MSG_TYPE_I_SEND:
        type_name = strdup("iSend");
        break;
    case COORD_MSG_TYPE_P_SEND:
        type_name = strdup("pSend");
        break;
    case COORD_MSG_TYPE_B_RECV:
        type_name = strdup(" Recv");
        break;
    case COORD_MSG_TYPE_I_RECV:
        type_name = strdup("iRecv");
        break;
    case COORD_MSG_TYPE_P_RECV:
        type_name = strdup("pRecv");
        break;
    default:
        type_name = strdup("Unknown");
        break;
    }

    if( !vshort ) {
        opal_output(0, "\t%s %10s (%3d): [m %3d/d %3d/a %3d/ad %3d/p %3d] Contents %2d ... count %6d, tag %6d, rank %3d",
                    type_name,
                    msg,
                    (int)msg_ref->msg_id,
                    msg_ref->matched,
                    msg_ref->done,
                    msg_ref->active,
                    msg_ref->active_drain,
                    msg_ref->posted,
                    (int)opal_list_get_size(&msg_ref->msg_contents),
                    (int)msg_ref->count,
                    msg_ref->tag,
                    msg_ref->rank);
    } else {
        opal_output(0, "\t%s %10s (%3d): [m %3d/d %3d/a %3d/ad %3d/p %3d] Contents %2d ... count %6d",
                    type_name,
                    msg,
                    (int)msg_ref->msg_id,
                    msg_ref->matched,
                    msg_ref->done,
                    msg_ref->active,
                    msg_ref->active_drain,
                    msg_ref->posted,
                    (int)opal_list_get_size(&msg_ref->msg_contents),
                    (int)msg_ref->count);
    }

    free(type_name);

    for(cont_item  = opal_list_get_first(&(msg_ref->msg_contents));
        cont_item != opal_list_get_end(  &(msg_ref->msg_contents));
        cont_item  = opal_list_get_next(cont_item) ) {
        content_ref = (ompi_crcp_bkmrk_pml_message_content_ref_t*)cont_item;

        traffic_message_dump_msg_content_indv(content_ref);
    }
}

static void traffic_message_dump_drain_msg_indv(ompi_crcp_bkmrk_pml_drain_message_ref_t * msg_ref, char * msg, bool vshort)
{
    ompi_crcp_bkmrk_pml_message_content_ref_t  *content_ref = NULL;
    opal_list_item_t* cont_item = NULL;
    char * type_name = NULL;

    switch(msg_ref->msg_type) {
    case COORD_MSG_TYPE_B_SEND:
        type_name = strdup(" Send");
        break;
    case COORD_MSG_TYPE_I_SEND:
        type_name = strdup("iSend");
        break;
    case COORD_MSG_TYPE_P_SEND:
        type_name = strdup("pSend");
        break;
    case COORD_MSG_TYPE_B_RECV:
        type_name = strdup(" Recv");
        break;
    case COORD_MSG_TYPE_I_RECV:
        type_name = strdup("iRecv");
        break;
    case COORD_MSG_TYPE_P_RECV:
        type_name = strdup("pRecv");
        break;
    default:
        type_name = strdup("Unknown");
        break;
    }

    if( !vshort ) {
        opal_output(0, "\t%s %10s (%3d): [d %3d/a %3d] Contents %2d ... count %6d, tag %6d, rank %3d",
                    type_name,
                    msg,
                    (int)msg_ref->msg_id,
                    msg_ref->done,
                    msg_ref->active,
                    (int)opal_list_get_size(&msg_ref->msg_contents),
                    (int)msg_ref->count,
                    msg_ref->tag,
                    msg_ref->rank);
    } else {
        opal_output(0, "\t%s %10s (%3d): [d %3d/a %3d] Contents %2d ... count %6d",
                    type_name,
                    msg,
                    (int)msg_ref->msg_id,
                    msg_ref->done,
                    msg_ref->active,
                    (int)opal_list_get_size(&msg_ref->msg_contents),
                    (int)msg_ref->count);
    }

    free(type_name);

    for(cont_item  = opal_list_get_first(&(msg_ref->msg_contents));
        cont_item != opal_list_get_end(  &(msg_ref->msg_contents));
        cont_item  = opal_list_get_next(cont_item) ) {
        content_ref = (ompi_crcp_bkmrk_pml_message_content_ref_t*)cont_item;

        traffic_message_dump_msg_content_indv(content_ref);
    }
}

static void traffic_message_dump_msg_list(opal_list_t *msg_list, bool is_drain)
{
    opal_list_item_t* item = NULL;
    ompi_crcp_bkmrk_pml_traffic_message_ref_t * msg_ref = NULL;
    ompi_crcp_bkmrk_pml_drain_message_ref_t   * drain_msg_ref = NULL;

    for(item  = opal_list_get_last(msg_list);
        item != opal_list_get_begin(msg_list);
        item  = opal_list_get_prev(item) ) {
        if( !is_drain ) {
            msg_ref = (ompi_crcp_bkmrk_pml_traffic_message_ref_t*)item;
            traffic_message_dump_msg_indv(msg_ref, "", false);
        } else {
            drain_msg_ref = (ompi_crcp_bkmrk_pml_drain_message_ref_t*)item;
            traffic_message_dump_drain_msg_indv(drain_msg_ref, "Drain", false);
        }
    }
}

static void traffic_message_dump_peer(ompi_crcp_bkmrk_pml_peer_ref_t *peer_ref, char * msg, bool root_only)
{
    if( root_only && orte_process_info.my_name.vpid != 0 ) {
        return;
    } else {
        sleep(orte_process_info.my_name.vpid * 2);
    }

    opal_output(0, "------------- %s ---------------------------------", msg);
    opal_output(0, "%s <-> %s Totals Sent [ %3d / %3d ] Recv [ %3d / %3d ]",
                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                ORTE_NAME_PRINT(&(peer_ref->proc_name)),
                peer_ref->total_msgs_sent,
                peer_ref->matched_msgs_sent,
                peer_ref->total_msgs_recvd,
                peer_ref->matched_msgs_recvd);
    opal_output(0, "\n");

    traffic_message_dump_msg_list(&(peer_ref->send_list),      false);
    traffic_message_dump_msg_list(&(peer_ref->isend_list),     false);
    traffic_message_dump_msg_list(&(peer_ref->send_init_list), false);

    traffic_message_dump_msg_list(&(peer_ref->recv_list),      false);
    traffic_message_dump_msg_list(&(peer_ref->irecv_list),     false);
    traffic_message_dump_msg_list(&(peer_ref->recv_init_list), false);

    traffic_message_dump_msg_list(&(peer_ref->drained_list),   true);

    opal_output(0, "--------------------------------------------------");
    usleep(250000);
}
#endif
