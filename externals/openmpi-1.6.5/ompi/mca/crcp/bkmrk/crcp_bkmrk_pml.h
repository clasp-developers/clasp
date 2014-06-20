/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 * 
 * Hoke CRCP component
 *
 */

#ifndef MCA_CRCP_HOKE_PML_EXPORT_H
#define MCA_CRCP_HOKE_PML_EXPORT_H

#include "ompi_config.h"

#include "opal/mca/mca.h"
#include "ompi/mca/crcp/crcp.h"
#include "ompi/communicator/communicator.h"

#include "ompi/mca/crcp/bkmrk/crcp_bkmrk.h"

BEGIN_C_DECLS

    /*
     * PML Coordination functions
     */
    ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_enable
    ( bool enable, ompi_crcp_base_pml_state_t* pml_state );

    ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_add_comm
    ( struct ompi_communicator_t* comm, 
      ompi_crcp_base_pml_state_t* pml_state );
    ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_del_comm
    ( struct ompi_communicator_t* comm, 
      ompi_crcp_base_pml_state_t* pml_state );

    ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_add_procs
    ( struct ompi_proc_t **procs, size_t nprocs, 
      ompi_crcp_base_pml_state_t* pml_state );
    ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_del_procs
    ( struct ompi_proc_t **procs, size_t nprocs, 
      ompi_crcp_base_pml_state_t* pml_state );

    ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_progress
    (ompi_crcp_base_pml_state_t* pml_state);
    
    ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_iprobe
    (int dst, int tag, struct ompi_communicator_t* comm, 
     int *matched, ompi_status_public_t* status, 
     ompi_crcp_base_pml_state_t* pml_state );

    ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_probe
    ( int dst, int tag, struct ompi_communicator_t* comm, 
      ompi_status_public_t* status, 
      ompi_crcp_base_pml_state_t* pml_state );
    
    ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_isend_init
    ( void *buf, size_t count, ompi_datatype_t *datatype, 
      int dst, int tag, mca_pml_base_send_mode_t mode, 
      struct ompi_communicator_t* comm, 
      struct ompi_request_t **request, 
      ompi_crcp_base_pml_state_t* pml_state );
    
    ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_isend
    ( void *buf, size_t count, ompi_datatype_t *datatype, 
      int dst, int tag, mca_pml_base_send_mode_t mode, 
      struct ompi_communicator_t* comm, 
      struct ompi_request_t **request, 
      ompi_crcp_base_pml_state_t* pml_state );
    
    ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_send
    (  void *buf, size_t count, ompi_datatype_t *datatype, 
       int dst, int tag, mca_pml_base_send_mode_t mode, 
       struct ompi_communicator_t* comm, 
       ompi_crcp_base_pml_state_t* pml_state );
    
    ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_irecv_init
    ( void *buf, size_t count, ompi_datatype_t *datatype, 
      int src, int tag, struct ompi_communicator_t* comm,
      struct ompi_request_t **request, 
      ompi_crcp_base_pml_state_t* pml_state);
    
    ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_irecv
    ( void *buf, size_t count, ompi_datatype_t *datatype, 
      int src, int tag, struct ompi_communicator_t* comm, 
      struct ompi_request_t **request, 
      ompi_crcp_base_pml_state_t* pml_state );
    
    ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_recv
    (  void *buf, size_t count, ompi_datatype_t *datatype, 
       int src, int tag, struct ompi_communicator_t* comm,  
       ompi_status_public_t* status, 
       ompi_crcp_base_pml_state_t* pml_state);
    
    ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_dump
    ( struct ompi_communicator_t* comm, int verbose, 
      ompi_crcp_base_pml_state_t* pml_state );
    
    ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_start
    ( size_t count, ompi_request_t** requests, 
      ompi_crcp_base_pml_state_t* pml_state );
    
    ompi_crcp_base_pml_state_t* ompi_crcp_bkmrk_pml_ft_event
    (int state, ompi_crcp_base_pml_state_t* pml_state);

    /*
     * Request function
     */
    int ompi_crcp_bkmrk_request_complete(struct ompi_request_t *request);

    /***********************************
     * Globally Defined Structures
     ***********************************/
    /*
     * Types of Messages
     */
    enum ompi_crcp_bkmrk_pml_message_type_t {
        COORD_MSG_TYPE_UNKNOWN, /* 0 Unknown type      */
        COORD_MSG_TYPE_B_SEND,  /* 1 Blocking Send     */
        COORD_MSG_TYPE_I_SEND,  /* 2 Non-Blocking Send */
        COORD_MSG_TYPE_P_SEND,  /* 3 Persistent  Send  */
        COORD_MSG_TYPE_B_RECV,  /* 4 Blocking Recv     */
        COORD_MSG_TYPE_I_RECV,  /* 5 Non-Blocking Recv */
        COORD_MSG_TYPE_P_RECV   /* 6 Persistent  Recv  */
    };
    typedef enum ompi_crcp_bkmrk_pml_message_type_t ompi_crcp_bkmrk_pml_message_type_t;

    /*
     * A list structure to contain {buffer, request, status} sets
     *
     * send/recv type | Buffer | Request | Status | Active
     * ---------------+--------+---------+--------+--------
     * Blocking       |     No |      No |     No |     No
     * Non-Blocking   |     No |     Yes |    Yes |     No
     * Persistent     |    Yes |     Yes |    Yes |    Yes
     *
     * No : Does not require this field
     * Yes: Does require this field
     */
    struct ompi_crcp_bkmrk_pml_message_content_ref_t {
        /** This is a list object */
        opal_list_item_t super;

        /** Buffer for data */
        void * buffer;

        /* Request for this message */
        ompi_request_t *request;

        /** Status */
        ompi_status_public_t status;

        /** Active ? */
        bool active;

        /** Done ? - Only useful in Drain*/
        bool done;

        /** Already_posted ? - Only useful in Drain */
        bool already_posted;

        /** Drained */
        bool already_drained;

        /** JJH XXX Debug counter*/
        uint64_t msg_id;
    };
    typedef struct ompi_crcp_bkmrk_pml_message_content_ref_t ompi_crcp_bkmrk_pml_message_content_ref_t;

    OBJ_CLASS_DECLARATION(ompi_crcp_bkmrk_pml_message_content_ref_t);
    void ompi_crcp_bkmrk_pml_message_content_ref_construct(ompi_crcp_bkmrk_pml_message_content_ref_t *content_ref);
    void ompi_crcp_bkmrk_pml_message_content_ref_destruct( ompi_crcp_bkmrk_pml_message_content_ref_t *content_ref);

    /*
     * Drain Message Reference
     * - The first section of this structure should match
     *   ompi_crcp_bkmrk_pml_traffic_message_ref_t exactly.
     */
    struct ompi_crcp_bkmrk_pml_drain_message_ref_t {
        /** This is a list object */
        opal_list_item_t super;

        /** Sequence Number of this message */
        uint64_t msg_id;

        /** Type of message this references */
        ompi_crcp_bkmrk_pml_message_type_t msg_type;

        /** Count for data */
        size_t count;

        /** Datatype */
        struct ompi_datatype_t * datatype;

        /** Quick reference to the size of the datatype */
        size_t ddt_size;

        /** Message Tag */
        int tag;

        /** Peer rank to which it was sent/recv'ed if known */
        int rank;

        /** Communicator pointer */
        ompi_communicator_t* comm;

        /** Message Contents */
        opal_list_t msg_contents;

        /** Peer which we received from */
        orte_process_name_t proc_name;

        /**
         * Count of the number of completed PML messages that match this reference.
         */
        int done;

        /**
         * Count of the number of active PML messages that match this reference.
         */
        int active;

        /**
         * Count of the number of posted PML messages that match this reference.
         *   Used when trying to figure out which messages the drain protocol needs to post, and
         *   which message have already been posted for it.
         */
        int already_posted;

    };
    typedef struct ompi_crcp_bkmrk_pml_drain_message_ref_t ompi_crcp_bkmrk_pml_drain_message_ref_t;
    
    OBJ_CLASS_DECLARATION(ompi_crcp_bkmrk_pml_drain_message_ref_t);
    void ompi_crcp_bkmrk_pml_drain_message_ref_construct(ompi_crcp_bkmrk_pml_drain_message_ref_t *msg_ref);
    void ompi_crcp_bkmrk_pml_drain_message_ref_destruct( ompi_crcp_bkmrk_pml_drain_message_ref_t *msg_ref);

    /*
     * List of Pending ACKs to drained messages
     */
    struct ompi_crcp_bkmrk_pml_drain_message_ack_ref_t {
        /** This is a list object */
        opal_list_item_t super;

        /** Complete flag */
        bool complete;

        /** Peer which we received from */
        orte_process_name_t peer;
    };
    typedef struct ompi_crcp_bkmrk_pml_drain_message_ack_ref_t ompi_crcp_bkmrk_pml_drain_message_ack_ref_t;

    OBJ_CLASS_DECLARATION(ompi_crcp_bkmrk_pml_drain_message_ack_ref_t);
    void ompi_crcp_bkmrk_pml_drain_message_ack_ref_construct(ompi_crcp_bkmrk_pml_drain_message_ack_ref_t *msg_ack_ref);
    void ompi_crcp_bkmrk_pml_drain_message_ack_ref_destruct( ompi_crcp_bkmrk_pml_drain_message_ack_ref_t *msg_ack_ref);

    /*
     * Regular Traffic Message Reference
     * Tracks message signature {count, datatype_size, tag, comm, peer}
     */
    struct ompi_crcp_bkmrk_pml_traffic_message_ref_t {
        /** This is a list object */
        opal_list_item_t super;

        /** Sequence Number of this message */
        uint64_t msg_id;

        /** Type of message this references */
        ompi_crcp_bkmrk_pml_message_type_t msg_type;

        /** Count for data */
        size_t count;

        /** Quick reference to the size of the datatype */
        size_t ddt_size;

        /** Message Tag */
        int tag;

        /** Peer rank to which it was sent/recv'ed if known */
        int rank;

        /** Communicator pointer */
        ompi_communicator_t* comm;

        /** Message Contents */
        opal_list_t msg_contents;

        /** Peer which we received from */
        orte_process_name_t proc_name;

        /* Sample movement of values (mirrored for send):
         *                     Recv()   iRecv()  irecv_init()  start()  req_complete()
         *   * Pre:
         *     matched        = false   false    false         ---      ---
         *     done           = false   false    false         ---      true
         *     active         = true    true     false         true     false
         *     already_posted = true    true     true          ---      ---
         *   * Post:
         *     matched        = false   false    false         ---      ---
         *     done           = true    false    false         false    true
         *     active         = false   true     false         true     false
         *     already_posted = true    true     true          ---      ---
         *   * Drain
         *     already_posted = false -> true when posted irecv
         */
        /** Has this message been matched by the peer?
         *  - Resolved during bookmark exchange
         * true  = peer confirmed the receipt of this message
         * false = unknown if peer has received this message or not
         */
        int matched;

        /** Is this message complete WRT PML semantics?
         *  - Is it not in-flight?
         * true  = message done on this side (send or receive)
         * false = message still in process (sending or receiving)
         */
        int done;

        /** Is the message actively being worked on?
         *  - Known to be in-flight?
         * true  = Message is !done, and is in the progress cycle
         * false = Message is !done and is *not* in the progress cycle ( [send/recv]_init requests)
         */
        int active;

        /** How many times a persistent send/recv has been posted, but not activated.
         *
         */
        int posted;

        /** Actively drained
         * These are messages that are active, and being drained. So if we checkpoint while the drain
         * list is not empty then we do not try to count these messages more than once.
         */
        int active_drain;
    };
    typedef struct ompi_crcp_bkmrk_pml_traffic_message_ref_t ompi_crcp_bkmrk_pml_traffic_message_ref_t;
    
    OBJ_CLASS_DECLARATION(ompi_crcp_bkmrk_pml_traffic_message_ref_t);
    void ompi_crcp_bkmrk_pml_traffic_message_ref_construct(ompi_crcp_bkmrk_pml_traffic_message_ref_t *msg_ref);
    void ompi_crcp_bkmrk_pml_traffic_message_ref_destruct( ompi_crcp_bkmrk_pml_traffic_message_ref_t *msg_ref);

    /*
     * A structure for a single process
     * Contains:
     *  - List of sent messages to this peer
     *  - List of received message from this peer
     *  - Message totals
     */
    struct ompi_crcp_bkmrk_pml_peer_ref_t {
        /** This is a list object */
        opal_list_item_t super;

        /** Name of peer */
        orte_process_name_t proc_name;

        /** List of messages sent to this peer */
        opal_list_t send_list;      /**< pml_send       */
        opal_list_t isend_list;     /**< pml_isend      */
        opal_list_t send_init_list; /**< pml_isend_init */

        /** List of messages recved from this peer */
        opal_list_t recv_list;      /**< pml_recv       */
        opal_list_t irecv_list;     /**< pml_irecv      */
        opal_list_t recv_init_list; /**< pml_irecv_init */

        /** List of messages drained from this peer */
        opal_list_t drained_list;

        /* 
         * These are totals over all communicators provided for convenience.
         *
         * If we are P_n and this structure represent P_m then:
         *  - total_*   = P_n --> P_m
         *  - matched_* = P_n <-- P_m
         * Where P_n --> P_m means:
         *  the number of messages P_n knows that it has sent/recv to/from P_m
         * And P_n --> P_m means:
         *  the number of messages P_m told us that is has sent/recv to/from P_n
         *
         * How total* are used:
         * Send:
         *   Before put on the wire: ++total
         * Recv:
         *   Once completed: ++total
         */
        /** Total Number of messages sent */
        uint32_t  total_msgs_sent;
        uint32_t  matched_msgs_sent;
        
        /** Total Number of messages received */
        uint32_t  total_msgs_recvd;
        uint32_t  matched_msgs_recvd;

        /** Total Number of messages drained */
        uint32_t  total_drained_msgs;

        /** If peer is expecting an ACK after draining the messages */
        bool ack_required;
    };
    typedef struct ompi_crcp_bkmrk_pml_peer_ref_t ompi_crcp_bkmrk_pml_peer_ref_t;
    
    OBJ_CLASS_DECLARATION(ompi_crcp_bkmrk_pml_peer_ref_t);
    void ompi_crcp_bkmrk_pml_peer_ref_construct(ompi_crcp_bkmrk_pml_peer_ref_t *bkm_proc);
    void ompi_crcp_bkmrk_pml_peer_ref_destruct( ompi_crcp_bkmrk_pml_peer_ref_t *bkm_proc);

    /*
     * Local version of the PML state
     */
    struct ompi_crcp_bkmrk_pml_state_t {
        ompi_crcp_base_pml_state_t p_super;
        ompi_crcp_base_pml_state_t *prev_ptr;

        ompi_crcp_bkmrk_pml_peer_ref_t             *peer_ref;
        ompi_crcp_bkmrk_pml_traffic_message_ref_t  *msg_ref;
    };
    typedef struct ompi_crcp_bkmrk_pml_state_t ompi_crcp_bkmrk_pml_state_t;
    OBJ_CLASS_DECLARATION(ompi_crcp_bkmrk_pml_state_t);

    /***********************************
     * Globally Defined Variables
     ***********************************/
    /*
     * List of known peers
     */
    extern opal_list_t ompi_crcp_bkmrk_pml_peer_refs;

END_C_DECLS

#endif /* MCA_CRCP_HOKE_PML_EXPORT_H */
