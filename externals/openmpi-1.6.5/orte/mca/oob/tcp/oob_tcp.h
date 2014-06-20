/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC. 
 *                         All rights reserved.
 * Copyright (c) 2010-2011 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 *
 * Defines the functions for the tcp module.
 */

#ifndef _MCA_OOB_TCP_H_
#define _MCA_OOB_TCP_H_

#include "orte_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif

#include "orte/types.h"

#include "opal/mca/base/base.h"
#include "opal/class/opal_free_list.h"
#include "opal/class/opal_hash_table.h"
#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"
#include "opal/threads/threads.h"
#include "opal/mca/timer/base/base.h"

#include "orte/mca/oob/oob.h"
#include "orte/mca/oob/base/base.h"
#include "orte/mca/oob/tcp/oob_tcp_peer.h"
#include "orte/mca/oob/tcp/oob_tcp_msg.h"


BEGIN_C_DECLS


#define OOB_TCP_DEBUG_CONNECT_FAIL 1  /* debug connection establishment failures */
#define OOB_TCP_DEBUG_CONNECT      2  /* other connection information */
#define OOB_TCP_DEBUG_INFO         3  /* information about startup, connection establish, etc. */
#define OOB_TCP_DEBUG_ALL          4  /* everything else */

extern mca_oob_t mca_oob_tcp;

/*
 * standard component functions
 */
mca_oob_t* mca_oob_tcp_component_init(int* priority);

/**
 * Hook function to allow the selected oob components
 * to register their contact info with the registry
*/

int mca_oob_tcp_init(void);

/**
 * Cleanup resources during shutdown.
 */
int mca_oob_tcp_fini(void);

/**
* Compare two process names for equality.
*
* @param  n1  Process name 1.
* @param  n2  Process name 2.
* @return     (-1 for n1<n2 0 for equality, 1 for n1>n2)
*
* Note that the definition of < or > is somewhat arbitrary -
* just needs to be consistently applied to maintain an ordering
* when process names are used as indices.
*/
int mca_oob_tcp_process_name_compare(const orte_process_name_t* n1, const orte_process_name_t* n2);

/**
 *  Obtain contact information for this host (e.g. <ipaddress>:<port>)
 */

char* mca_oob_tcp_get_addr(void);

/**
 *  Setup cached addresses for the peers.
 */

int mca_oob_tcp_set_addr(const orte_process_name_t*, const char*);

int mca_oob_tcp_get_new_name(orte_process_name_t* name);

/**
 *  A routine to ping a given process name to determine if it is reachable.
 *
 *  @param  name  The peer name.
 *  @param  tv    The length of time to wait on a connection/response.
 *
 *  Note that this routine blocks up to the specified timeout waiting for a
 *  connection / response from the specified peer. If the peer is unavailable
 *  an error status is returned.
 */
                                                                                                       
int mca_oob_tcp_ping(const orte_process_name_t*, const char* uri, const struct timeval* tv);


/*
 * Non-blocking versions of send/recv.
 */

int mca_oob_tcp_send_nb(orte_process_name_t* target, 
                        orte_process_name_t* origin, 
                        struct iovec* msg, 
                        int count,
                        int tag,
                        int flags, 
                        orte_rml_callback_fn_t cbfunc, 
                        void* cbdata);

/**
 * Non-blocking version of mca_oob_recv().
 *
 * @param peer (IN)    Opaque name of peer process or ORTE_NAME_WILDCARD for wildcard receive.
 * @param msg (IN)     Array of iovecs describing user buffers and lengths.
 * @param count (IN)   Number of elements in iovec array.
 * @param tag (IN)     User defined tag for matching send/recv.
 * @param flags (IN)   May be MCA_OOB_PEEK to return up to size bytes of msg w/out removing it from the queue,
 * @param cbfunc (IN)  Callback function on recv completion.
 * @param cbdata (IN)  User data that is passed to callback function.
 * @return             OMPI error code (<0) on error or number of bytes actually received.
 */

int mca_oob_tcp_recv_nb(
    orte_process_name_t* peer, 
    struct iovec* msg, 
    int count, 
    int tag,
    int flags,
    orte_rml_callback_fn_t cbfunc, 
    void* cbdata);

/**
 * Cancel non-blocking receive.
 *
 * @param peer (IN)    Opaque name of peer process or ORTE_NAME_WILDCARD for wildcard receive.
 * @param tag (IN)     User defined tag for matching send/recv.
 * @return             OMPI error code (<0) on error or number of bytes actually received.
 */

int mca_oob_tcp_recv_cancel(
    orte_process_name_t* peer, 
    int tag);

/**
 * Attempt to map a peer name to its corresponding address.
 */

int mca_oob_tcp_resolve(mca_oob_tcp_peer_t*);

/**
 *  Parse a URI string into an IP address and port number.
 */
int mca_oob_tcp_parse_uri(
    const char* uri, 
    struct sockaddr* inaddr
);

/**
 *  Setup socket options
 */

void mca_oob_tcp_set_socket_options(int sd);

int mca_oob_tcp_ft_event(int state);

typedef enum { OOB_TCP_EVENT, OOB_TCP_LISTEN_THREAD } mca_oob_tcp_listen_type_t;

/**
 *  OOB TCP Component
*/
struct mca_oob_tcp_component_t {
    mca_oob_base_component_2_0_0_t super;    /**< base OOB component */
    char*              tcp_include;          /**< list of ip interfaces to include */
    char*              tcp_exclude;          /**< list of ip interfaces to exclude */
    opal_list_t        tcp_peer_list;        /**< list of peers sorted in mru order */
    opal_hash_table_t  tcp_peers;            /**< peers sorted by name */
    opal_hash_table_t  tcp_peer_names;       /**< cache of peer contact info sorted by name */
    opal_free_list_t   tcp_peer_free;        /**< free list of peers */
    int                tcp_peer_limit;       /**< max size of tcp peer cache */
    int                tcp_peer_retries;     /**< max number of retries before declaring peer gone */
    int                tcp_sndbuf;           /**< socket send buffer size */
    int                tcp_rcvbuf;           /**< socket recv buffer size */
    opal_free_list_t   tcp_msgs;             /**< free list of messages */
    opal_event_t       tcp_recv_event;       /**< event structure for IPv4 recvs */
    int                tcp_listen_sd;        /**< listen socket for incoming IPv4 connection requests */
    unsigned short     tcp_listen_port;      /**< IPv4 listen port */
    char**             tcp4_static_ports;    /**< Static ports - IPV4 */
    char**             tcp4_dyn_ports;       /**< Dynamic ports - IPV4 */
    int                disable_family;       /**< disable AF: 0-nothing, 4-IPv4, 6-IPv6 */
#if OPAL_WANT_IPV6
    opal_event_t       tcp6_recv_event;      /**< event structure for IPv6 recvs */
    int                tcp6_listen_sd;       /**< listen socket for incoming IPv6 connection requests */
    unsigned short     tcp6_listen_port;     /**< IPv6 listen port */
    char**             tcp6_static_ports;    /**< Static ports - IPV6 */
    char**             tcp6_dyn_ports;       /**< Dynamic ports - IPV6 */
#endif  /* OPAL_WANT_IPV6 */
    opal_mutex_t       tcp_lock;             /**< lock for accessing module state */
    opal_list_t        tcp_events;           /**< list of pending events (accepts) */
    opal_list_t        tcp_msg_post;         /**< list of recieves user has posted */
    opal_list_t        tcp_msg_recv;         /**< list of recieved messages */
    opal_list_t        tcp_msg_completed;    /**< list of completed messages */
    opal_mutex_t       tcp_match_lock;       /**< lock held while searching/posting messages */
    opal_condition_t   tcp_match_cond;       /**< condition variable used in finalize */
    int                tcp_match_count;      /**< number of matched recvs in progress */
    int                tcp_debug;            /**< debug level */

    bool               tcp_shutdown;
    mca_oob_tcp_listen_type_t tcp_listen_type;

    opal_list_t tcp_available_devices;

    opal_thread_t tcp_listen_thread;     /** handle to the listening thread */
    opal_list_t tcp_pending_connections; /**< List of accepted connections waiting for processing */
    opal_list_t tcp_connections_return;  /**< List of connection fragments being returned to accept thread */
    opal_mutex_t tcp_connections_lock;   /**< Lock protecting pending_connections and connections_return */
    int tcp_connections_pipe[2];
    opal_event_t tcp_listen_thread_event;

    int tcp_copy_max_size;                /**< Max size of the copy list before copying must commence */
    int tcp_listen_thread_num_sockets;    /**< Number of sockets in tcp_listen_thread_sds */
    int tcp_listen_thread_sds[2];         /**< Room for IPv4 and IPv6.  Might need to make more dynamic. */
    struct timeval tcp_listen_thread_tv;  /**< Timeout when using listen thread */

    int connect_sleep;
};

/**
 * Convenience Typedef
 */
typedef struct mca_oob_tcp_component_t mca_oob_tcp_component_t;

ORTE_MODULE_DECLSPEC extern mca_oob_tcp_component_t mca_oob_tcp_component;

extern int mca_oob_tcp_output_handle;

#if defined(__WINDOWS__)
#define CLOSE_THE_SOCKET(socket)    closesocket(socket)
#else

#define CLOSE_THE_SOCKET(socket)    \
    do {                            \
        shutdown(socket, 2);        \
        close(socket);              \
    } while(0)

#endif  /* defined(__WINDOWS__) */


struct mca_oob_tcp_pending_connection_t {
    opal_free_list_item_t super;
    int fd;
    struct sockaddr_storage addr;
};
typedef struct mca_oob_tcp_pending_connection_t mca_oob_tcp_pending_connection_t;
OBJ_CLASS_DECLARATION(mca_oob_tcp_pending_connection_t);

struct mca_oob_tcp_device_t {
    opal_list_item_t super;
    int if_index;
    bool if_local;
    struct sockaddr_storage if_addr;
};
typedef struct mca_oob_tcp_device_t mca_oob_tcp_device_t;
OBJ_CLASS_DECLARATION(mca_oob_tcp_device_t);


END_C_DECLS

#endif /* MCA_OOB_TCP_H_ */

