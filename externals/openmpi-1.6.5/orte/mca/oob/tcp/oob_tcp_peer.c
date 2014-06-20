/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * In windows, many of the socket functions return an EWOULDBLOCK
 * instead of \ things like EAGAIN, EINPROGRESS, etc. It has been
 * verified that this will \ not conflict with other error codes that
 * are returned by these functions \ under UNIX/Linux environments 
 */

#include "orte_config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <fcntl.h>
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif
#ifdef HAVE_NET_UIO_H
#include <net/uio.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include "opal/opal_socket_errno.h"
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef HAVE_NETINET_TCP_H
#include <netinet/tcp.h>
#endif

#include "opal/types.h"
#include "opal/mca/backtrace/backtrace.h"
#include "opal/util/output.h"
#include "opal/util/net.h"
#include "opal/util/error.h"
#include "opal/class/opal_hash_table.h"

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/notifier/notifier.h"
#include "orte/runtime/orte_wait.h"

#include "oob_tcp.h"
#include "oob_tcp_peer.h"

static int  mca_oob_tcp_peer_start_connect(mca_oob_tcp_peer_t* peer);
static int  mca_oob_tcp_peer_event_init(mca_oob_tcp_peer_t* peer);
static void mca_oob_tcp_peer_connected(mca_oob_tcp_peer_t* peer, int sd);
static void mca_oob_tcp_peer_construct(mca_oob_tcp_peer_t* peer);
static void mca_oob_tcp_peer_destruct(mca_oob_tcp_peer_t* peer);
static int  mca_oob_tcp_peer_send_connect_ack(mca_oob_tcp_peer_t* peer, int sd);
static int  mca_oob_tcp_peer_recv_connect_ack(mca_oob_tcp_peer_t* peer, int sd);
static int  mca_oob_tcp_peer_recv_blocking(mca_oob_tcp_peer_t* peer, int sd, void* data, size_t size);
static int  mca_oob_tcp_peer_send_blocking(mca_oob_tcp_peer_t* peer, int sd, void* data, size_t size);
static void mca_oob_tcp_peer_recv_handler(int sd, short flags, void* user);
static void mca_oob_tcp_peer_send_handler(int sd, short flags, void* user);
static void mca_oob_tcp_peer_timer_handler(int sd, short flags, void* user);


OBJ_CLASS_INSTANCE(
    mca_oob_tcp_peer_t,
    opal_free_list_item_t,
    mca_oob_tcp_peer_construct,
    mca_oob_tcp_peer_destruct);


/*
 * This is the constructor function for the mca_oob_tcp_peer
 * struct. Note that this function and OBJ_NEW should NEVER
 * be called directly. Instead, use mca_oob_tcp_add_peer
 *
 * @param peer a pointer to the mca_oob_tcp_peer_t struct to be initialized
 * @retval none
 */
static void mca_oob_tcp_peer_construct(mca_oob_tcp_peer_t* peer) 
{ 
    OBJ_CONSTRUCT(&(peer->peer_send_queue), opal_list_t);
    OBJ_CONSTRUCT(&(peer->peer_lock), opal_mutex_t);
    memset(&peer->peer_send_event, 0, sizeof(peer->peer_send_event));
    memset(&peer->peer_recv_event, 0, sizeof(peer->peer_recv_event));
    peer->peer_sd = -1;
    peer->peer_current_af = AF_UNSPEC;
    memset(&peer->peer_timer_event, 0, sizeof(peer->peer_timer_event));
    opal_evtimer_set(&peer->peer_timer_event, mca_oob_tcp_peer_timer_handler, peer);
}

/*
 * This is the destructor function for the mca_oob_tcp_peer
 * struct. Note that this function and OBJ_RELEASE should NEVER
 * be called directly. Instead, use mca_oob_tcp_del_peer
 *
 * @param peer a pointer to the mca_oob_tcp_peer_t struct to be destroyed
 * @retval none
 */
static void mca_oob_tcp_peer_destruct(mca_oob_tcp_peer_t * peer)
{
    mca_oob_tcp_peer_shutdown(peer); 
    OBJ_DESTRUCT(&(peer->peer_send_queue));
    OBJ_DESTRUCT(&(peer->peer_lock));
    if (NULL != peer->peer_addr) {
        OBJ_RELEASE(peer->peer_addr);
    }
}

/*
 * Initialize events to be used by the peer instance for TCP select/poll callbacks.
 */
static int mca_oob_tcp_peer_event_init(mca_oob_tcp_peer_t* peer)
{
    memset(&peer->peer_recv_event, 0, sizeof(peer->peer_recv_event));
    memset(&peer->peer_send_event, 0, sizeof(peer->peer_send_event));

    if (peer->peer_sd >= 0) {
        opal_event_set(
                       &peer->peer_recv_event,
                       peer->peer_sd,
                       OPAL_EV_READ|OPAL_EV_PERSIST,
                       mca_oob_tcp_peer_recv_handler,
                       peer);
        opal_event_set(
                       &peer->peer_send_event,
                       peer->peer_sd,
                       OPAL_EV_WRITE|OPAL_EV_PERSIST,
                       mca_oob_tcp_peer_send_handler,
                       peer);
    }

    return ORTE_SUCCESS;
}

/*
 *  Initiate the appropriate action based on the state of the connection
 *  to the peer.
 *
 */
int mca_oob_tcp_peer_send(mca_oob_tcp_peer_t* peer, mca_oob_tcp_msg_t* msg)
{
    int rc = ORTE_SUCCESS;
    OPAL_THREAD_LOCK(&peer->peer_lock);
    switch(peer->peer_state) {
    case MCA_OOB_TCP_CONNECTING:
    case MCA_OOB_TCP_CONNECT_ACK:
    case MCA_OOB_TCP_CLOSED:
    case MCA_OOB_TCP_RESOLVE:
        /*
         * queue the message and attempt to resolve the peer address
         */
        if(peer->peer_state == MCA_OOB_TCP_CLOSED) {
            peer->peer_state = MCA_OOB_TCP_RESOLVE;

            /* Only queue up the message if we known who the peer is.
               If we don't, return the error and let the upper layer
               figure out what to do.  Cannot hold the peer_lock when
               we call resolve(), but we do need to hold it when/if we
               append to the peer_send_queue. */
            OPAL_THREAD_UNLOCK(&peer->peer_lock);
            rc = mca_oob_tcp_resolve(peer);
            if (ORTE_ERR_ADDRESSEE_UNKNOWN != rc) {
                OPAL_THREAD_LOCK(&peer->peer_lock);
                opal_list_append(&peer->peer_send_queue,
                                 (opal_list_item_t*)msg);
                OPAL_THREAD_UNLOCK(&peer->peer_lock);
            }
            return rc;
        }
        opal_list_append(&peer->peer_send_queue, (opal_list_item_t*)msg);
        break;
    case MCA_OOB_TCP_FAILED:
        rc = ORTE_ERR_UNREACH;
        break;
    case MCA_OOB_TCP_CONNECTED:
        /*
         * start the message and queue if not completed 
         */
        if (NULL != peer->peer_send_msg) {
            opal_list_append(&peer->peer_send_queue, (opal_list_item_t*)msg);
        } else {
            /*if the send does not complete */
            if(!mca_oob_tcp_msg_send_handler(msg, peer)) {
                peer->peer_send_msg = msg;
                opal_event_add(&peer->peer_send_event, 0);
            } else {
                mca_oob_tcp_msg_complete(msg, &peer->peer_name);
            }
        }
        break;
    }
    OPAL_THREAD_UNLOCK(&peer->peer_lock);
    return rc;
}

/*
 * Lookup a peer by name, create one if it doesn't exist.
 * @param name  Peers globally unique identifier.
 * @retval      Pointer to the newly created struture or NULL on error.
 */
mca_oob_tcp_peer_t * mca_oob_tcp_peer_lookup(const orte_process_name_t* name)
{
    int rc;
    mca_oob_tcp_peer_t * peer = NULL, *old;
    if (NULL == name) { /* can't look this one up */
        return NULL;
    }
    
    OPAL_THREAD_LOCK(&mca_oob_tcp_component.tcp_lock);
    opal_hash_table_get_value_uint64(&mca_oob_tcp_component.tcp_peers,
                                     orte_util_hash_name(name), (void**)&peer);
    if (NULL != peer && 0 == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, &peer->peer_name, name)) {
        OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
        return peer;
    }

    /* allocate from free list */
    MCA_OOB_TCP_PEER_ALLOC(peer, rc);
    if(NULL == peer) {
        OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
        return NULL;
    }

    /* initialize peer state */
    peer->peer_name = *name;
    peer->peer_addr = NULL;
    peer->peer_sd = -1;
    peer->peer_current_af = AF_UNSPEC;
    peer->peer_state = MCA_OOB_TCP_CLOSED;
    peer->peer_recv_msg = NULL;
    peer->peer_send_msg = NULL;
    peer->peer_retries = 0;

    /* add to lookup table */
    if(OPAL_SUCCESS != opal_hash_table_set_value_uint64(&mca_oob_tcp_component.tcp_peers, 
                                                        orte_util_hash_name(&peer->peer_name), peer)) {
        MCA_OOB_TCP_PEER_RETURN(peer);
        OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
        return NULL;
    }

    /* if the peer list is over the maximum size, remove one unsed peer */
    opal_list_prepend(&mca_oob_tcp_component.tcp_peer_list, (opal_list_item_t *) peer);
    if(mca_oob_tcp_component.tcp_peer_limit > 0 &&
       (int)opal_list_get_size(&mca_oob_tcp_component.tcp_peer_list) > 
       mca_oob_tcp_component.tcp_peer_limit) {
        old = (mca_oob_tcp_peer_t *) 
              opal_list_get_last(&mca_oob_tcp_component.tcp_peer_list);
        while(1) {
            if(0 == opal_list_get_size(&(old->peer_send_queue)) &&
               NULL == peer->peer_recv_msg) { 
                opal_list_remove_item(&mca_oob_tcp_component.tcp_peer_list, 
                                      (opal_list_item_t *) old);
                MCA_OOB_TCP_PEER_RETURN(old);
                break;
            } else {
                old = (mca_oob_tcp_peer_t *) opal_list_get_prev(old);
                if(opal_list_get_begin(&mca_oob_tcp_component.tcp_peer_list) == (opal_list_item_t*)old) {
                    /* we tried, but we couldn't find one that was valid to get rid
                     * of. Oh well. */
                    break;
                }
            }
        }
    }
    OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock);
    return peer;
}


static int
mca_oob_tcp_peer_create_socket(mca_oob_tcp_peer_t* peer,
                               uint16_t af_family)
{
    int flags;

    if (peer->peer_current_af == af_family && peer->peer_sd > 0) {
        return ORTE_SUCCESS;
    } else if (peer->peer_sd > 0) {
        int state = peer->peer_state;
        mca_oob_tcp_peer_shutdown(peer);
        peer->peer_state = (mca_oob_tcp_state_t) state;
    }

    OPAL_OUTPUT_VERBOSE((1, mca_oob_tcp_output_handle,
                         "%s oob:tcp:peer creating socket to %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&(peer->peer_name))));
    
    peer->peer_sd = socket(af_family, SOCK_STREAM, 0);
    peer->peer_current_af = af_family;

    if (peer->peer_sd < 0) {
        opal_output(0, 
            "%s-%s mca_oob_tcp_peer_create_socket: socket() failed: %s (%d)\n",
            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
            ORTE_NAME_PRINT(&(peer->peer_name)),
            strerror(opal_socket_errno),
            opal_socket_errno);
        mca_oob_tcp_peer_shutdown(peer);
        return ORTE_ERR_UNREACH;
    }

    /* setup socket options */
    mca_oob_tcp_set_socket_options(peer->peer_sd);

    /* setup event callbacks */
    mca_oob_tcp_peer_event_init(peer);

    /* setup the socket as non-blocking */
    if (peer->peer_sd >= 0) {
        if((flags = fcntl(peer->peer_sd, F_GETFL, 0)) < 0) {
            opal_output(0, "%s-%s mca_oob_tcp_peer_connect: fcntl(F_GETFL) failed: %s (%d)\n", 
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&(peer->peer_name)),
                        strerror(opal_socket_errno),
                        opal_socket_errno);
        } else {
            flags |= O_NONBLOCK;
            if(fcntl(peer->peer_sd, F_SETFL, flags) < 0)
                opal_output(0, "%s-%s mca_oob_tcp_peer_connect: fcntl(F_SETFL) failed: %s (%d)\n", 
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&(peer->peer_name)),
                            strerror(opal_socket_errno),
                            opal_socket_errno);
        }
    }

    return ORTE_SUCCESS;
}


/*
 * Try connecting to a peer using all the addresses that peer exported.
 */
static int mca_oob_tcp_peer_try_connect(mca_oob_tcp_peer_t* peer)
{
    struct sockaddr_storage inaddr;
    int rc, retry_count;
    opal_socklen_t addrlen = 0;

    do {
        /* pick an address in round-robin fashion from the list exported by the peer */
        if(ORTE_SUCCESS != (rc = mca_oob_tcp_addr_get_next(peer->peer_addr, (struct sockaddr*) &inaddr))) {
            opal_output(0, "%s-%s mca_oob_tcp_peer_try_connect: "
                        "mca_oob_tcp_addr_get_next failed with error=%d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&(peer->peer_name)),
                        rc);
            mca_oob_tcp_peer_close(peer);
            return ORTE_ERR_UNREACH;
        }

        /* we have IPv4 disabled, so obey it and don't try to connect */
        if (AF_INET == inaddr.ss_family &&
            4 == mca_oob_tcp_component.disable_family) {
            continue;
        }

        /* we have IPv6 disabled, so obey it and don't try to connect */
        if (AF_INET6 == inaddr.ss_family &&
            6 == mca_oob_tcp_component.disable_family) {
            continue;
        }

        if(mca_oob_tcp_component.tcp_debug >= OOB_TCP_DEBUG_CONNECT) {
            opal_output(0, "%s-%s mca_oob_tcp_peer_try_connect: "
                        "connecting port %d to: %s:%d\n",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&(peer->peer_name)),
                        /* Bug, FIXME: output tcp6_listen_port for AF_INET6 */
                        ntohs(mca_oob_tcp_component.tcp_listen_port),
                        opal_net_get_hostname((struct sockaddr*) &inaddr),
                        opal_net_get_port((struct sockaddr*) &inaddr));
        }

        rc = mca_oob_tcp_peer_create_socket(peer, inaddr.ss_family);
        if (ORTE_SUCCESS != rc) {
            struct timeval tv = { 1,0 };
            opal_evtimer_add(&peer->peer_timer_event, &tv);
            return rc;
        }

        if (AF_INET == inaddr.ss_family) {
            addrlen = sizeof(struct sockaddr_in);
        } else if (AF_INET6 == inaddr.ss_family) {
            addrlen = sizeof(struct sockaddr_in6);
        }

        retry_count = 0;
    retry_connect:
        if (connect(peer->peer_sd, (struct sockaddr*)&inaddr, addrlen) < 0) {
            /* non-blocking so wait for completion */
            if(opal_socket_errno == EINPROGRESS || opal_socket_errno == EWOULDBLOCK) {
                opal_event_add(&peer->peer_send_event, 0);
                return ORTE_SUCCESS;
            }

            /* Some kernels (Linux 2.6) will automatically software
               abort a connection that was ECONNREFUSED on the last
               attempt, without even trying to establish the
               connection.  Handle that case in a semi-rational
               way. */
            if (ECONNABORTED == opal_socket_errno && ++retry_count < 2) {
                goto retry_connect;
            }

            if ((mca_oob_tcp_component.tcp_debug >= OOB_TCP_DEBUG_CONNECT) ||
                (ECONNABORTED != opal_socket_errno &&
                 ECONNREFUSED != opal_socket_errno)) {
                opal_output(0, "%s-%s mca_oob_tcp_peer_try_connect: "
                            "connect to %s:%d failed: %s (%d)",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&(peer->peer_name)),
                            opal_net_get_hostname((struct sockaddr*) &inaddr),
                            opal_net_get_port((struct sockaddr*) &inaddr),
                            strerror(opal_socket_errno),
                            opal_socket_errno);
            }
            continue;
        }        

        /* send our globally unique process identifier to the peer */
        if((rc = mca_oob_tcp_peer_send_connect_ack(peer, peer->peer_sd)) == ORTE_SUCCESS) {
            peer->peer_state = MCA_OOB_TCP_CONNECT_ACK;
            opal_event_add(&peer->peer_recv_event, 0);
            return ORTE_SUCCESS;
        } else {
            opal_output(0, 
                        "%s-%s mca_oob_tcp_peer_try_connect: "
                        "mca_oob_tcp_peer_send_connect_ack to %s:%d failed: %s (%d)",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&(peer->peer_name)),
                        opal_net_get_hostname((struct sockaddr*) &inaddr),
                        opal_net_get_port((struct sockaddr*) &inaddr),
                        opal_strerror(rc),
                        rc);
        }
    } while(peer->peer_addr->addr_next != 0);

    /* None of the interfaces worked... We'll try again for a number of
       times, so we're not done yet, hence the debug output */
    if(mca_oob_tcp_component.tcp_debug >= OOB_TCP_DEBUG_CONNECT) {
        opal_output(0, "%s-%s mca_oob_tcp_peer_try_connect: "
                    "Connection across all interfaces failed.  Likely will retry",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    ORTE_NAME_PRINT(&(peer->peer_name)));
    }
    mca_oob_tcp_peer_close(peer);
    return ORTE_ERR_UNREACH;
}


/*
 *  Start a connection to the peer. This will likely not complete,
 *  as the socket is set to non-blocking, so register for event
 *  notification of connect completion. On connection we send
 *  our globally unique process identifier to the peer and wait for
 *  the peers response.
 */

static int mca_oob_tcp_peer_start_connect(mca_oob_tcp_peer_t* peer)
{
    /* create socket */
    peer->peer_state = MCA_OOB_TCP_CONNECTING;


    /* 
     * We should parse all the IP addresses exported by the peer and
     * try to connect to each of them.
     */
    return mca_oob_tcp_peer_try_connect(peer);
}


/*
 * Check the status of the connection. If the connection failed, will retry
 * later. Otherwise, send this processes identifier to the peer on the
 * newly connected socket.
 */
static void mca_oob_tcp_peer_complete_connect(mca_oob_tcp_peer_t* peer, int sd)
{
    int so_error = 0;
    opal_socklen_t so_length = sizeof(so_error);

    /* unregister from receiving event notifications */
    opal_event_del(&peer->peer_send_event);

    /* check connect completion status */
    if(getsockopt(sd, SOL_SOCKET, SO_ERROR, (char *)&so_error, &so_length) < 0) {
        opal_output(0, "%s-%s mca_oob_tcp_peer_complete_connect: getsockopt() failed: %s (%d)\n", 
            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
            ORTE_NAME_PRINT(&(peer->peer_name)),
            strerror(opal_socket_errno),
            opal_socket_errno);
        mca_oob_tcp_peer_close(peer);
        return;
    }

    if(so_error == EINPROGRESS) {
        opal_event_add(&peer->peer_send_event, 0);
        return;
    } else if (so_error == ECONNREFUSED || so_error == ETIMEDOUT) {
        struct timeval tv = { 1,0 };
        if (mca_oob_tcp_component.tcp_debug >= OOB_TCP_DEBUG_CONNECT) {
            opal_output(0, "%s-%s mca_oob_tcp_peer_complete_connect: "
                        "connection failed: %s (%d) - retrying\n",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&(peer->peer_name)),
                        strerror(so_error),
                        so_error);
        }
        mca_oob_tcp_peer_shutdown(peer);
        opal_evtimer_add(&peer->peer_timer_event, &tv);
        return;
    } else if(so_error != 0) {
        /* No need to worry about the return code here - we return regardless
           at this point, and if an error did occur a message has already been
           printed for the user */
        mca_oob_tcp_peer_try_connect(peer);
        return;
    }

    if (mca_oob_tcp_component.tcp_debug >= OOB_TCP_DEBUG_CONNECT) {
        opal_output(0, "%s-%s mca_oob_tcp_peer_complete_connect: "
                    "sending ack, %d",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    ORTE_NAME_PRINT(&(peer->peer_name)), so_error);
    }

    if (mca_oob_tcp_peer_send_connect_ack(peer, sd) == ORTE_SUCCESS) {
        peer->peer_state = MCA_OOB_TCP_CONNECT_ACK;
        opal_event_add(&peer->peer_recv_event, 0);
    } else {
        opal_output(0, "%s-%s mca_oob_tcp_peer_complete_connect: unable to send connect ack.",
            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
            ORTE_NAME_PRINT(&(peer->peer_name)));
        mca_oob_tcp_peer_close(peer);
    }
}

/*
 *  Setup peer state to reflect that connection has been established,
 *  and start any pending sends.
 */
static void mca_oob_tcp_peer_connected(mca_oob_tcp_peer_t* peer, int sd)
{
    opal_event_del(&peer->peer_timer_event);
    peer->peer_state = MCA_OOB_TCP_CONNECTED;
    peer->peer_retries = 0;

    if(opal_list_get_size(&peer->peer_send_queue) > 0) {
        if(NULL == peer->peer_send_msg) {
            peer->peer_send_msg = (mca_oob_tcp_msg_t*)
                opal_list_remove_first(&peer->peer_send_queue);
        }
        opal_event_add(&peer->peer_send_event, 0);
    }
}

/*
 * Remove any event registrations associated with the socket
 * and update the peer state to reflect the connection has
 * been closed.
 */
void mca_oob_tcp_peer_close(mca_oob_tcp_peer_t* peer)
{
    if(mca_oob_tcp_component.tcp_debug >= OOB_TCP_DEBUG_CONNECT) {
        opal_output(0, "%s-%s mca_oob_tcp_peer_close(%p) sd %d state %d\n",
            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
            ORTE_NAME_PRINT(&(peer->peer_name)),
            (void*)peer,
            peer->peer_sd,
            peer->peer_state);
    }

    /* inform the routed framework that we have lost a connection so
     * it can decide if this is important, what to do about it, etc.
     */
    if (ORTE_SUCCESS != orte_routed.route_lost(&peer->peer_name)) {
        /* Should free the peer lock before we abort so we don't 
         * get stuck in the orte_wait_kill when receiving messages in the 
         * tcp OOB
         */
        OPAL_THREAD_UNLOCK(&peer->peer_lock);
        orte_errmgr.abort(1, NULL);        
    }
    
    mca_oob_tcp_peer_shutdown(peer);
}

void mca_oob_tcp_peer_shutdown(mca_oob_tcp_peer_t* peer)
{
    /* giving up and cleanup any pending messages */
    if(peer->peer_retries++ > mca_oob_tcp_component.tcp_peer_retries) {
        mca_oob_tcp_msg_t *msg;
        char *host;

        host = orte_ess.proc_get_hostname(&(peer->peer_name));
        opal_output(0, "%s -> %s (node: %s) oob-tcp: Number of attempts to create TCP connection has been exceeded.  Can not communicate with peer",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    ORTE_NAME_PRINT(&(peer->peer_name)),
                    (NULL == host) ? "NULL" : host);
        /* provide a notifier message */
        orte_notifier.peer(ORTE_NOTIFIER_CRIT, ORTE_ERR_COMM_FAILURE, &(peer->peer_name),
                               "OOB connection retries exceeded. Cannot communicate with peer %s.",
                               ORTE_JOBID_PRINT(peer->peer_name.jobid));
        
        /* There are cases during the initial connection setup where
           the peer_send_msg is NULL but there are things in the queue
           -- handle that case */
        if (NULL != (msg = peer->peer_send_msg)) {
            msg->msg_complete = true;
            msg->msg_rc = ORTE_ERR_UNREACH;
            mca_oob_tcp_msg_complete(msg, &peer->peer_name);
        }
        peer->peer_send_msg = NULL;
        while (NULL != 
               (msg = (mca_oob_tcp_msg_t*)opal_list_remove_first(&peer->peer_send_queue))) {
            msg->msg_complete = true;
            msg->msg_rc = ORTE_ERR_UNREACH;
            mca_oob_tcp_msg_complete(msg, &peer->peer_name);
        }

        /* We were unsuccessful in establishing a connection, and are
           not likely to suddenly become successful, so abort the
           whole thing */
        peer->peer_state = MCA_OOB_TCP_FAILED;
        
        /* since we cannot communicate, and the system obviously needed
         * to do so, let's abort so we don't just hang here
         */
        if (ORTE_PROC_IS_HNP || ORTE_PROC_IS_DAEMON) {
            /* just wake us up */
            ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
            orte_abnormal_term_ordered = true;
            orte_trigger_event(&orte_exit);
        } else {
            orte_errmgr.abort(1, NULL);
        }
    }

    if (peer->peer_sd >= 0) {
        opal_event_del(&peer->peer_recv_event);
        opal_event_del(&peer->peer_send_event);
        CLOSE_THE_SOCKET(peer->peer_sd);
        peer->peer_sd = -1;
        peer->peer_current_af = AF_UNSPEC;
    } 
      
    opal_event_del(&peer->peer_timer_event);
    peer->peer_state = MCA_OOB_TCP_CLOSED;
}

/*
 * Send the globally unique identifier for this process to a peer on
 * a newly connected socket.
 */
static int mca_oob_tcp_peer_send_connect_ack(mca_oob_tcp_peer_t* peer, int sd)
{
    /* send process identifier of self and peer - note that we may 
     * have assigned the peer a unique process name - if it came up
     * without one.
    */
    mca_oob_tcp_hdr_t hdr;
    memset(&hdr,0,sizeof(hdr));
    hdr.msg_src = *(ORTE_PROC_MY_NAME);
    hdr.msg_dst = peer->peer_name;
    hdr.msg_type = MCA_OOB_TCP_CONNECT;
    MCA_OOB_TCP_HDR_HTON(&hdr);
    if(mca_oob_tcp_peer_send_blocking(peer, sd, &hdr, sizeof(hdr)) != sizeof(hdr)) {
        return ORTE_ERR_UNREACH;
    }
    return ORTE_SUCCESS;
}

/*
 *  Receive the peers globally unique process identification from a newly
 *  connected socket and verify the expected response. If so, move the
 *  socket to a connected state.
 */
static int mca_oob_tcp_peer_recv_connect_ack(mca_oob_tcp_peer_t* peer, int sd)
{
    mca_oob_tcp_hdr_t hdr;
    if((mca_oob_tcp_peer_recv_blocking(peer, sd, &hdr, sizeof(hdr))) != sizeof(hdr)) {
        /* If the peer state is still CONNECT_ACK, that indicates that
           the error was a reset from the remote host because the
           connection was not able to be fully established.  In that
           case, Clean up the connection and give it another go.  */
        if (peer->peer_state == MCA_OOB_TCP_CONNECT_ACK) {
            struct timeval tv = { 1,0 };
            if (mca_oob_tcp_component.tcp_debug >= OOB_TCP_DEBUG_CONNECT) {
                opal_output(0,
                            "%s-%s mca_oob_tcp_peer_recv_connect_ack "
                            "connect failed during receive.  Restarting (%s).",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&(peer->peer_name)),
                            strerror(opal_socket_errno));
            }
            opal_event_del(&peer->peer_recv_event);
            mca_oob_tcp_peer_shutdown(peer);
            opal_evtimer_add(&peer->peer_timer_event, &tv);
            return ORTE_SUCCESS;
        } else {
            mca_oob_tcp_peer_close(peer);
            return ORTE_ERR_UNREACH;
        }
    }
    MCA_OOB_TCP_HDR_NTOH(&hdr);
    if(hdr.msg_type != MCA_OOB_TCP_CONNECT) {
        opal_output(0, "mca_oob_tcp_peer_recv_connect_ack: invalid header type: %d\n", 
                    hdr.msg_type);
        mca_oob_tcp_peer_close(peer);
        return ORTE_ERR_UNREACH;
    }

    /* compare the peers name to the expected value */
    if (OPAL_EQUAL != orte_util_compare_name_fields(ORTE_NS_CMP_ALL, &peer->peer_name, &hdr.msg_src)) {
        opal_output(0, "%s-%s mca_oob_tcp_peer_recv_connect_ack: "
            "received unexpected process identifier %s\n",
            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
            ORTE_NAME_PRINT(&(peer->peer_name)),
            ORTE_NAME_PRINT(&(hdr.msg_src)));
        mca_oob_tcp_peer_close(peer);
        return ORTE_ERR_UNREACH;
    }

    /* connected */
    mca_oob_tcp_peer_connected(peer, sd);
    if(mca_oob_tcp_component.tcp_debug >= OOB_TCP_DEBUG_CONNECT) {
        mca_oob_tcp_peer_dump(peer, "connected");
    }
    return ORTE_SUCCESS;
}


/*
 * A blocking recv on a non-blocking socket. Used to receive the small amount of connection
 * information that identifies the peers endpoint.
 */
static int mca_oob_tcp_peer_recv_blocking(mca_oob_tcp_peer_t* peer, int sd, void* data, size_t size)
{
    unsigned char* ptr = (unsigned char*)data;
    size_t cnt = 0;
    while(cnt < size) {
        int retval = recv(sd,(char *)ptr+cnt, size-cnt, 0);

        /* remote closed connection */
        if(retval == 0) {
            if(mca_oob_tcp_component.tcp_debug >= OOB_TCP_DEBUG_INFO) {
                opal_output(0, "%s-%s mca_oob_tcp_peer_recv_blocking: "
                    "peer closed connection: peer state %d",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    ORTE_NAME_PRINT(&(peer->peer_name)),
                    peer->peer_state);
            }
            mca_oob_tcp_peer_close(peer);
            return -1;
        }

        /* socket is non-blocking so handle errors */
        if(retval < 0) {
            if(opal_socket_errno != EINTR && 
               opal_socket_errno != EAGAIN && 
               opal_socket_errno != EWOULDBLOCK) {
                if (peer->peer_state == MCA_OOB_TCP_CONNECT_ACK) {
                    /* If we overflow the listen backlog, it's
                       possible that even though we finished the three
                       way handshake, the remote host was unable to
                       transition the connection from half connected
                       (received the initial SYN) to fully connected
                       (in the listen backlog).  We likely won't see
                       the failure until we try to receive, due to
                       timing and the like.  The first thing we'll get
                       in that case is a RST packet, which receive
                       will turn into a connection reset by peer
                       errno.  In that case, leave the socket in
                       CONNECT_ACK and propogate the error up to
                       recv_connect_ack, who will try to establish the
                       connection again */
                    return -1;
                } else {
                    opal_output(0, 
                                "%s-%s mca_oob_tcp_peer_recv_blocking: "
                                "recv() failed: %s (%d)\n",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                ORTE_NAME_PRINT(&(peer->peer_name)),
                                strerror(errno),
                                errno);
                    mca_oob_tcp_peer_close(peer);
                    return -1;
                }
            }
            continue;
        }
        cnt += retval;
    }
    return cnt;
}

/*
 * A blocking send on a non-blocking socket. Used to send the small amount of connection
 * information that identifies the peers endpoint.
 */
static int mca_oob_tcp_peer_send_blocking(mca_oob_tcp_peer_t* peer, int sd, void* data, size_t size)
{
    unsigned char* ptr = (unsigned char*)data;
    size_t cnt = 0;
    while(cnt < size) {
        int retval = send(sd, (char *)ptr+cnt, size-cnt, 0);
        if(retval < 0) {
            if(opal_socket_errno != EINTR && opal_socket_errno != EAGAIN && opal_socket_errno != EWOULDBLOCK) {
                opal_output(0, "%s-%s mca_oob_tcp_peer_send_blocking: send() failed: %s (%d)\n",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    ORTE_NAME_PRINT(&(peer->peer_name)),
                    strerror(opal_socket_errno),
                    opal_socket_errno);
                mca_oob_tcp_peer_close(peer);
                return -1;
            }
            continue;
        }
        cnt += retval;
    }
    return cnt;
}


int mca_oob_tcp_peer_send_ident(mca_oob_tcp_peer_t* peer)
{
    mca_oob_tcp_hdr_t hdr;
    if(peer->peer_state != MCA_OOB_TCP_CONNECTED)
        return ORTE_SUCCESS;
    hdr.msg_src = *ORTE_PROC_MY_NAME;
    hdr.msg_origin = *ORTE_PROC_MY_NAME;
    hdr.msg_dst = peer->peer_name;
    hdr.msg_type = MCA_OOB_TCP_IDENT;
    hdr.msg_size = 0;
    hdr.msg_tag = 0;
    MCA_OOB_TCP_HDR_HTON(&hdr);
    if(mca_oob_tcp_peer_send_blocking(peer, peer->peer_sd, &hdr, sizeof(hdr)) != sizeof(hdr)) {
        return ORTE_ERR_UNREACH;
    }
    return ORTE_SUCCESS;
}


/* static void mca_oob_tcp_peer_recv_ident(mca_oob_tcp_peer_t* peer, mca_oob_tcp_hdr_t* hdr) */
/* { */
/*     OPAL_THREAD_LOCK(&mca_oob_tcp_component.tcp_lock); */
/*     ompi_rb_tree_delete(&mca_oob_tcp_component.tcp_peer_tree, &peer->peer_name); */
/*     peer->peer_name = hdr->msg_src; */
/*     ompi_rb_tree_insert(&mca_oob_tcp_component.tcp_peer_tree, &peer->peer_name, peer); */
/*     OPAL_THREAD_UNLOCK(&mca_oob_tcp_component.tcp_lock); */
/* } */


/*
 * Dispatch to the appropriate action routine based on the state
 * of the connection with the peer.
 */

static void mca_oob_tcp_peer_recv_handler(int sd, short flags, void* user)
{
    mca_oob_tcp_peer_t* peer = (mca_oob_tcp_peer_t *)user;
    OPAL_THREAD_LOCK(&peer->peer_lock);
    switch(peer->peer_state) {
        case MCA_OOB_TCP_CONNECT_ACK: 
        {
            mca_oob_tcp_peer_recv_connect_ack(peer, sd);
            break;
        }
        case MCA_OOB_TCP_CONNECTED: 
        {
            /* allocate a new message and setup for recv */
            if(NULL == peer->peer_recv_msg) {
                int rc;
                mca_oob_tcp_msg_t* msg;
                MCA_OOB_TCP_MSG_ALLOC(msg, rc);
                if(NULL == msg) {
                    opal_output(0, "%s-%s mca_oob_tcp_peer_recv_handler: unable to allocate recv message\n",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&(peer->peer_name)));
                    return;
                }
                msg->msg_type = MCA_OOB_TCP_UNEXPECTED;
                msg->msg_rc = 0;
                msg->msg_flags = 0;
                msg->msg_peer = peer->peer_name;
                msg->msg_rwiov = mca_oob_tcp_msg_iov_alloc(msg,2);
                msg->msg_rwbuf = NULL;
                msg->msg_rwcnt = msg->msg_rwnum = 1;
                msg->msg_rwptr = msg->msg_rwiov;
                msg->msg_rwiov[0].iov_base = (ompi_iov_base_ptr_t)&msg->msg_hdr;
                msg->msg_rwiov[0].iov_len = sizeof(msg->msg_hdr);
                peer->peer_recv_msg = msg;
            }

            if (peer->peer_recv_msg && 
                mca_oob_tcp_msg_recv_handler(peer->peer_recv_msg, peer)) {
               mca_oob_tcp_msg_t* msg = peer->peer_recv_msg;
               peer->peer_recv_msg = NULL;
               OPAL_THREAD_UNLOCK(&peer->peer_lock);
               mca_oob_tcp_msg_recv_complete(msg, peer);
               return;
            }
            break;
        }
        default: 
        {
            opal_output(0, "%s-%s mca_oob_tcp_peer_recv_handler: invalid socket state(%d)", 
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    ORTE_NAME_PRINT(&(peer->peer_name)),
                    peer->peer_state);
            mca_oob_tcp_peer_close(peer);
            break;
        }
    }
    OPAL_THREAD_UNLOCK(&peer->peer_lock);
}

/*
 * A file descriptor is available/ready for send. Check the state
 * of the socket and take the appropriate action.
 */
static void mca_oob_tcp_peer_send_handler(int sd, short flags, void* user)
{
    mca_oob_tcp_peer_t* peer = (mca_oob_tcp_peer_t *)user;
    OPAL_THREAD_LOCK(&peer->peer_lock);
    switch(peer->peer_state) {
    case MCA_OOB_TCP_CONNECTING:
        mca_oob_tcp_peer_complete_connect(peer, sd);
        break;
    case MCA_OOB_TCP_CONNECTED:
        {
        while(peer->peer_send_msg != NULL) {

            /* complete the current send */
            mca_oob_tcp_msg_t* msg = peer->peer_send_msg;
            if(ntohl(msg->msg_hdr.msg_type) == MCA_OOB_TCP_PING ||
               mca_oob_tcp_msg_send_handler(msg, peer)) {
                mca_oob_tcp_msg_complete(msg, &peer->peer_name);
            } else {
                break;
            }

            /* if current completed - progress any pending sends */
            peer->peer_send_msg = (mca_oob_tcp_msg_t*)
                opal_list_remove_first(&peer->peer_send_queue);
        }
        
        /* if nothing else to do unregister for send event notifications */
        if(NULL == peer->peer_send_msg) {
            opal_event_del(&peer->peer_send_event);
        }
        break;
        }
    default:
        opal_output(0, "%s-%s mca_oob_tcp_peer_send_handler: invalid connection state (%d)",
            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
            ORTE_NAME_PRINT(&(peer->peer_name)),
            peer->peer_state);
        opal_event_del(&peer->peer_send_event);
        break;
    }
    OPAL_THREAD_UNLOCK(&peer->peer_lock);
}
                                                                                                                       

/*
 * Routine for debugging to print the connection state and socket options
 */
void mca_oob_tcp_peer_dump(mca_oob_tcp_peer_t* peer, const char* msg)
{
    char src[64];
    char dst[64];
    char buff[255];
    int sndbuf,rcvbuf,nodelay,flags;
    struct sockaddr_storage inaddr;
    opal_socklen_t addrlen = sizeof(struct sockaddr_storage);
    opal_socklen_t optlen;
                                                                                                            
    getsockname(peer->peer_sd, (struct sockaddr*)&inaddr, &addrlen);
    snprintf(src, sizeof(src), "%s", opal_net_get_hostname((struct sockaddr*) &inaddr));
    getpeername(peer->peer_sd, (struct sockaddr*)&inaddr, &addrlen);
    snprintf(dst, sizeof(dst), "%s", opal_net_get_hostname((struct sockaddr*) &inaddr));
                                                                                                            
    if((flags = fcntl(peer->peer_sd, F_GETFL, 0)) < 0) {
        opal_output(0, "mca_oob_tcp_peer_dump: fcntl(F_GETFL) failed: %s (%d)\n",
                    strerror(opal_socket_errno),
                    opal_socket_errno);
    }
                                                                                                            
#if defined(SO_SNDBUF)
    optlen = sizeof(sndbuf);
    if(getsockopt(peer->peer_sd, SOL_SOCKET, SO_SNDBUF, (char *)&sndbuf, &optlen) < 0) {
        opal_output(0, "mca_oob_tcp_peer_dump: SO_SNDBUF option: %s (%d)\n", 
                    strerror(opal_socket_errno),
                    opal_socket_errno);
    }
#else
    sndbuf = -1;
#endif
#if defined(SO_RCVBUF)
    optlen = sizeof(rcvbuf);
    if(getsockopt(peer->peer_sd, SOL_SOCKET, SO_RCVBUF, (char *)&rcvbuf, &optlen) < 0) {
        opal_output(0, "mca_oob_tcp_peer_dump: SO_RCVBUF option: %s (%d)\n", 
                    strerror(opal_socket_errno),
                    opal_socket_errno);
    }
#else
    rcvbuf = -1;
#endif
#if defined(TCP_NODELAY)
    optlen = sizeof(nodelay);
    if(getsockopt(peer->peer_sd, IPPROTO_TCP, TCP_NODELAY, (char *)&nodelay, &optlen) < 0) {
        opal_output(0, "mca_oob_tcp_peer_dump: TCP_NODELAY option: %s (%d)\n", 
                    strerror(opal_socket_errno),
                    opal_socket_errno);
    }
#else
    nodelay = 0;
#endif

    snprintf(buff, sizeof(buff), "%s-%s %s: %s - %s nodelay %d sndbuf %d rcvbuf %d flags %08x\n",
        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
        ORTE_NAME_PRINT(&(peer->peer_name)),
        msg, src, dst, nodelay, sndbuf, rcvbuf, flags);
    opal_output(0, "%s", buff);
}


/*
 * Accept incoming connection - if not already connected. We compare the name of the
 * peer to our own name using the ns.compare_fields function as we want this to be
 * a LITERAL comparison - i.e., there is no occasion when the peer's name should
 * be a wildcard value.
 *
 * To avoid competing reciprocal connection attempts, we only accept connections from
 * processes whose names are "greater" than our own.
 */

bool mca_oob_tcp_peer_accept(mca_oob_tcp_peer_t* peer, int sd)
{
    int cmpval;
    OPAL_THREAD_LOCK(&peer->peer_lock);
    cmpval = orte_util_compare_name_fields(ORTE_NS_CMP_ALL, &peer->peer_name, ORTE_PROC_MY_NAME);
    if ((peer->peer_state == MCA_OOB_TCP_CLOSED) ||
        (peer->peer_state == MCA_OOB_TCP_RESOLVE) ||
        (peer->peer_state != MCA_OOB_TCP_CONNECTED &&
         cmpval == OPAL_VALUE1_GREATER)) {

        if(peer->peer_state != MCA_OOB_TCP_CLOSED) {
            mca_oob_tcp_peer_close(peer);
        }
        peer->peer_sd = sd;
        mca_oob_tcp_peer_event_init(peer);

        if(mca_oob_tcp_peer_send_connect_ack(peer, sd) != ORTE_SUCCESS) {
            opal_output(0, "%s-%s mca_oob_tcp_peer_accept: "
                "mca_oob_tcp_peer_send_connect_ack failed\n",
                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                ORTE_NAME_PRINT(&(peer->peer_name)));
            mca_oob_tcp_peer_close(peer);
            OPAL_THREAD_UNLOCK(&peer->peer_lock);
            return false;
        }

        mca_oob_tcp_peer_connected(peer, sd);
        if (sd == peer->peer_sd) {
            opal_event_add(&peer->peer_recv_event, 0);
        }
        if(mca_oob_tcp_component.tcp_debug > 0) {
            mca_oob_tcp_peer_dump(peer, "accepted");
        }
        OPAL_THREAD_UNLOCK(&peer->peer_lock);
        return true;
    }
    OPAL_THREAD_UNLOCK(&peer->peer_lock);
    return false;
}


/*
 * resolve process name to an actual internet address.
 */

void mca_oob_tcp_peer_resolved(mca_oob_tcp_peer_t* peer, mca_oob_tcp_addr_t* addr)
{
    OPAL_THREAD_LOCK(&peer->peer_lock);
    peer->peer_addr = addr;
    if((peer->peer_state == MCA_OOB_TCP_RESOLVE) ||
       (peer->peer_state == MCA_OOB_TCP_CLOSED && opal_list_get_size(&peer->peer_send_queue))) {
        mca_oob_tcp_peer_start_connect(peer);
    }
    OPAL_THREAD_UNLOCK(&peer->peer_lock);
}

/*
 * Callback on timeout - retry connection attempt.
 */

static void mca_oob_tcp_peer_timer_handler(int sd, short flags, void* user)
{
    /* start the connection to the peer */
    mca_oob_tcp_peer_t* peer = (mca_oob_tcp_peer_t*)user;

    OPAL_THREAD_LOCK(&peer->peer_lock);
    if(peer->peer_state == MCA_OOB_TCP_CLOSED) {
        mca_oob_tcp_peer_start_connect(peer);
    } 
    OPAL_THREAD_UNLOCK(&peer->peer_lock);
}

/*
 * Remove any references to the indicated message.
 */

void mca_oob_tcp_peer_dequeue_msg(mca_oob_tcp_peer_t* peer, mca_oob_tcp_msg_t* msg)
{
    opal_list_item_t* item;
    OPAL_THREAD_LOCK(&peer->peer_lock);
    if (peer->peer_send_msg == msg)
        peer->peer_send_msg = NULL;
    if (peer->peer_recv_msg == msg)
        peer->peer_recv_msg = NULL;

    for( item =  opal_list_get_first(&peer->peer_send_queue);
         item != opal_list_get_end(&peer->peer_send_queue);
         item = opal_list_get_next(item)) {
        if(item == (opal_list_item_t*)msg) {
            opal_list_remove_item(&peer->peer_send_queue, item);
            break;
        }
    }
    OPAL_THREAD_UNLOCK(&peer->peer_lock);
}


/**
 * Set socket buffering
 */

void mca_oob_tcp_set_socket_options(int sd)
{
    int optval;
#if defined(TCP_NODELAY)
    optval = 1;
    if(setsockopt(sd, IPPROTO_TCP, TCP_NODELAY, (char *)&optval, sizeof(optval)) < 0) {
        opal_backtrace_print(stderr);
        opal_output(0, "[%s:%d] setsockopt(TCP_NODELAY) failed: %s (%d)", 
                    __FILE__, __LINE__, 
                    strerror(opal_socket_errno),
                    opal_socket_errno);
    }
#endif
#if defined(SO_SNDBUF)
    if(mca_oob_tcp_component.tcp_sndbuf > 0 &&
       setsockopt(sd, SOL_SOCKET, SO_SNDBUF, (char *)&mca_oob_tcp_component.tcp_sndbuf, sizeof(int)) < 0) {
        opal_output(0, "[%s:%d] setsockopt(SO_SNDBUF) failed: %s (%d)", 
                    __FILE__, __LINE__, 
                    strerror(opal_socket_errno),
                    opal_socket_errno);
    }
#endif
#if defined(SO_RCVBUF)
    if(mca_oob_tcp_component.tcp_rcvbuf > 0 &&
       setsockopt(sd, SOL_SOCKET, SO_RCVBUF, (char *)&mca_oob_tcp_component.tcp_rcvbuf, sizeof(int)) < 0) {
        opal_output(0, "[%s:%d] setsockopt(SO_RCVBUF) failed: %s (%d)", 
                    __FILE__, __LINE__, 
                    strerror(opal_socket_errno),
                    opal_socket_errno);
    }
#endif
}



