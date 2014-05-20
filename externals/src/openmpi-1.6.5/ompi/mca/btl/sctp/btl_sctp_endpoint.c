/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc All rights reserved.
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

#include "ompi_config.h"

#include <stdlib.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include "opal/opal_socket_errno.h"
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_NETINET_TCP_H
#include <netinet/tcp.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */
#ifdef HAVE_TIME_H
#include <time.h>
#endif  /* HAVE_TIME_H */

#include <netinet/sctp.h>

#include "ompi/types.h"
#include "ompi/mca/btl/base/btl_base_error.h"
#include "orte/util/name_fns.h"
#include "btl_sctp.h"
#include "btl_sctp_endpoint.h" 
#include "btl_sctp_proc.h"
#include "btl_sctp_frag.h"
#include "btl_sctp_addr.h"
#include "btl_sctp_utils.h"
#include "btl_sctp_recv_handler.h"

/* Pre-allocate pool of endpoints.  These are copies of endpoints
 *  since opal_list only allows an item to be a member of one list
 *  at a time.  We have a freelist of pre-allocated endpoints and
 *  also a list of endpoints that currently want to send.  Our send
 *  handler walks this list each time it's invoked.  If nobody currently
 *  wants to send (i.e., endpoint_associated_with_send is NULL), the
 *  POLLOUT for this socket is turned off.  If there are no more free
 *  slots allocated, the pool expands.
 */
#define INITIAL_NUM_FREE_POOL_SLOTS 10
static opal_list_t sending_endpoints;
static opal_list_t sending_endpoints_freelist;
mca_btl_base_endpoint_t *endpoint_associated_with_send;
OBJ_CLASS_INSTANCE(
    our_sctp_endpoint, 
    opal_list_item_t, 
    NULL, 
    NULL);
static int have_initiated_sending_endpoints_list=0;

struct sending_endpoint_chunk {
    our_sctp_endpoint *to_free;
    struct sending_endpoint_chunk *next;
};
typedef struct sending_endpoint_chunk sending_endpoint_chunk;
static sending_endpoint_chunk endpoint_chunks_to_free;
static sending_endpoint_chunk * endpoint_chunks_to_free_tail;
static int sending_endpoints_walk_count;
/*
 * Initialize state of the endpoint instance.
 *
 */

static void mca_btl_sctp_endpoint_construct(mca_btl_sctp_endpoint_t* endpoint)
{
    endpoint->endpoint_btl = NULL;
    endpoint->endpoint_proc = NULL;
    endpoint->endpoint_addr = NULL;
    endpoint->endpoint_sd = -1;
    endpoint->endpoint_send_frag = 0;
    endpoint->endpoint_recv_frag = 0;
    endpoint->endpoint_send_event.ev_flags = 0;
    endpoint->endpoint_recv_event.ev_flags = 0;
    endpoint->endpoint_state = MCA_BTL_SCTP_CLOSED;
    endpoint->endpoint_retries = 0;
    endpoint->endpoint_nbo = false;
#if MCA_BTL_SCTP_ENDPOINT_CACHE
    endpoint->endpoint_cache        = NULL;
    endpoint->endpoint_cache_pos    = NULL;
    endpoint->endpoint_cache_length = 0;
#endif  /* MCA_BTL_SCTP_ENDPOINT_CACHE */
    endpoint->endpoint_has_initialized = 0;
    endpoint->endpoint_in_list = 0;
    OBJ_CONSTRUCT(&endpoint->endpoint_frags, opal_list_t);
    OBJ_CONSTRUCT(&endpoint->endpoint_send_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&endpoint->endpoint_recv_lock, opal_mutex_t);
    if(0 == have_initiated_sending_endpoints_list) {
        int i;
        our_sctp_endpoint *free_entries;
        
        have_initiated_sending_endpoints_list++;
        OBJ_CONSTRUCT(&sending_endpoints, opal_list_t);
        OBJ_CONSTRUCT(&sending_endpoints_freelist, opal_list_t);
        endpoint_associated_with_send = NULL;

        if(NULL == (free_entries = (our_sctp_endpoint *) malloc(sizeof(our_sctp_endpoint)
                                                                * INITIAL_NUM_FREE_POOL_SLOTS)))
        {
            BTL_ERROR(("cannot allocate free poll entries."));
            return;
        }
        memset(free_entries, 0, sizeof(our_sctp_endpoint) * INITIAL_NUM_FREE_POOL_SLOTS);
        for(i=0; i < INITIAL_NUM_FREE_POOL_SLOTS; i++) {
            opal_list_append(&sending_endpoints_freelist, (opal_list_item_t *) &free_entries[i]);
        }
        endpoint_chunks_to_free.to_free = free_entries;
        endpoint_chunks_to_free.next = NULL;
        endpoint_chunks_to_free_tail = &endpoint_chunks_to_free;
    }
}

/*
 * Destroy a endpoint
 *
 */


static void mca_btl_sctp_endpoint_destruct(mca_btl_sctp_endpoint_t* endpoint)
{
    mca_btl_sctp_proc_remove(endpoint->endpoint_proc, endpoint);
    mca_btl_sctp_endpoint_close(endpoint);
    OBJ_DESTRUCT(&endpoint->endpoint_frags);
    OBJ_DESTRUCT(&endpoint->endpoint_send_lock);
    OBJ_DESTRUCT(&endpoint->endpoint_recv_lock);
    if(have_initiated_sending_endpoints_list) {
        sending_endpoint_chunk *chunkp = &endpoint_chunks_to_free, *nextp;
        
        have_initiated_sending_endpoints_list--;
        OBJ_DESTRUCT(&sending_endpoints);
        OBJ_DESTRUCT(&sending_endpoints_freelist);

        /* free up memory used in pool of sending_endpoint items */
        
        /* only to_free malloc'd in static endpoint_chunks_to_free */
        free(endpoint_chunks_to_free.to_free);
        nextp = chunkp->next;
        while(nextp) /* ...but all other chunks were dynamically allocated */
        {
            chunkp = nextp;
            nextp = chunkp->next;
            free(chunkp->to_free);
            free(chunkp);
        }
    }
}

OBJ_CLASS_INSTANCE(
    mca_btl_sctp_endpoint_t, 
    opal_list_item_t, 
    mca_btl_sctp_endpoint_construct, 
    mca_btl_sctp_endpoint_destruct);


static void mca_btl_sctp_endpoint_construct(mca_btl_base_endpoint_t* btl_endpoint);
static void mca_btl_sctp_endpoint_destruct(mca_btl_base_endpoint_t* btl_endpoint);
static int  mca_btl_sctp_endpoint_start_connect(mca_btl_base_endpoint_t*);
static void mca_btl_sctp_endpoint_connected(mca_btl_base_endpoint_t*);
static void mca_btl_sctp_endpoint_recv_handler(int sd, short flags, void* user);
static void mca_btl_sctp_endpoint_send_handler(int sd, short flags, void* user);

/*
 * Diagnostics: change this to "1" to enable the function
 * mca_btl_sctp_endpoint_dump(), below
 */
#define WANT_PEER_DUMP 0
/*
 * diagnostics
 */

#if WANT_PEER_DUMP
static void mca_btl_sctp_endpoint_dump(mca_btl_base_endpoint_t* btl_endpoint, const char* msg)
{
    char src[64];
    char dst[64];
    int sndbuf,rcvbuf,nodelay,flags;
    struct sockaddr_in inaddr;
    opal_socklen_t obtlen;
    opal_socklen_t addrlen = sizeof(struct sockaddr_in);

    getsockname(btl_endpoint->endpoint_sd, (struct sockaddr*)&inaddr, &addrlen);
    sprintf(src, "%s", inet_ntoa(inaddr.sin_addr));
    getpeername(btl_endpoint->endpoint_sd, (struct sockaddr*)&inaddr, &addrlen);
    sprintf(dst, "%s", inet_ntoa(inaddr.sin_addr));

    if((flags = fcntl(btl_endpoint->endpoint_sd, F_GETFL, 0)) < 0) {
        BTL_ERROR(("fcntl(F_GETFL) failed with errno=%d", opal_socket_errno));
    }

#if defined(SO_SNDBUF)
    obtlen = sizeof(sndbuf);
    if(getsockopt(btl_endpoint->endpoint_sd, SOL_SOCKET, SO_SNDBUF, (char *)&sndbuf, &obtlen) < 0) {
        BTL_ERROR(("SO_SNDBUF option: errno %d", opal_socket_errno));
    }
#else
    sndbuf = -1;
#endif
#if defined(SO_RCVBUF)
    obtlen = sizeof(rcvbuf);
    if(getsockopt(btl_endpoint->endpoint_sd, SOL_SOCKET, SO_RCVBUF, (char *)&rcvbuf, &obtlen) < 0) {
        BTL_ERROR(("SO_RCVBUF option: errno %d", opal_socket_errno));
    }
#else
    rcvbuf = -1;
#endif
#if defined(SCTP_NODELAY)
    obtlen = sizeof(nodelay);
    if(getsockopt(btl_endpoint->endpoint_sd, IPPROTO_SCTP, SCTP_NODELAY, (char *)&nodelay, &obtlen) < 0) {
        BTL_ERROR(("SCTP_NODELAY option: errno %d", opal_socket_errno));
    }
#else
    nodelay = 0;
#endif

    BTL_DEBUG(("%s: %s - %s nodelay %d sndbuf %d rcvbuf %d flags %08x", 
        msg, src, dst, nodelay, sndbuf, rcvbuf, flags));
}
#endif

/*
 * Initialize events to be used by the endpoint instance for TCP select/poll callbacks.
 */


static inline void mca_btl_sctp_endpoint_event_init(mca_btl_base_endpoint_t* btl_endpoint, int sd)
{
    if(mca_btl_sctp_component.sctp_if_11) {
        /* 1 to 1*/

#if MCA_BTL_SCTP_ENDPOINT_CACHE
        btl_endpoint->endpoint_cache     = (char*)malloc(mca_btl_sctp_component.sctp_endpoint_cache);
        btl_endpoint->endpoint_cache_pos = btl_endpoint->endpoint_cache;
#endif  /* MCA_BTL_SCTP_ENDPOINT_CACHE */

        opal_event_set( &btl_endpoint->endpoint_recv_event, 
                btl_endpoint->endpoint_sd, 
                OPAL_EV_READ|OPAL_EV_PERSIST, 
                mca_btl_sctp_endpoint_recv_handler,
                btl_endpoint );
        opal_event_set( &btl_endpoint->endpoint_send_event, 
                btl_endpoint->endpoint_sd, 
               OPAL_EV_WRITE|OPAL_EV_PERSIST, 
                mca_btl_sctp_endpoint_send_handler,
                btl_endpoint);
    }
    
    else {
        /* 1 to many */
        if(0 == btl_endpoint->endpoint_has_initialized) {
            btl_endpoint->endpoint_has_initialized++;
            
            btl_endpoint->endpoint_sd = mca_btl_sctp_component.sctp_listen_sd;
        
#if MCA_BTL_SCTP_ENDPOINT_CACHE
            btl_endpoint->endpoint_cache     = (char*)malloc(mca_btl_sctp_component.sctp_endpoint_cache);
            btl_endpoint->endpoint_cache_pos = btl_endpoint->endpoint_cache;
#endif  /* MCA_BTL_SCTP_ENDPOINT_CACHE */

            opal_event_set( &btl_endpoint->endpoint_recv_event, 
                            btl_endpoint->endpoint_sd, 
                            OPAL_EV_READ|OPAL_EV_PERSIST, 
                            mca_btl_sctp_recv_handler,
                            btl_endpoint );
            opal_event_set( &btl_endpoint->endpoint_send_event, 
                            btl_endpoint->endpoint_sd, 
                            OPAL_EV_WRITE|OPAL_EV_PERSIST, 
                            mca_btl_sctp_endpoint_send_handler,
                            btl_endpoint);
        }
    }
}


/*
 * Attempt to send a fragment using a given endpoint. If the endpoint is not connected,
 * queue the fragment and start the connection as required.
 *
 * Here we have our first notion of endpoint connection state. Instead of
 * assigning state, it may be benficial to maintain another table of recvrs that
 * we have sent to in the past. If that recvr is NOT found in the table, then it
 * is a new proc and we have to send them our GUID before we send anything else.
 */
 
int mca_btl_sctp_endpoint_send(mca_btl_base_endpoint_t* btl_endpoint, mca_btl_sctp_frag_t* frag)
{
    if(mca_btl_sctp_component.sctp_if_11) {
        /* 1 to 1 */
        int rc = OMPI_SUCCESS;
        OPAL_THREAD_LOCK(&btl_endpoint->endpoint_send_lock);
        switch(btl_endpoint->endpoint_state) {
            case MCA_BTL_SCTP_CONNECTING:
            case MCA_BTL_SCTP_CONNECT_ACK:
            case MCA_BTL_SCTP_CLOSED:
                opal_list_append(&btl_endpoint->endpoint_frags, (opal_list_item_t*)frag);
                if(btl_endpoint->endpoint_state == MCA_BTL_SCTP_CLOSED) {
                    rc = mca_btl_sctp_endpoint_start_connect(btl_endpoint);
                }
                break;
            case MCA_BTL_SCTP_FAILED:
                rc = OMPI_ERR_UNREACH;
                break;
            case MCA_BTL_SCTP_CONNECTED:
                if (btl_endpoint->endpoint_send_frag == NULL) {
                    if(frag->base.des_flags & MCA_BTL_DES_FLAGS_PRIORITY &&
                            mca_btl_sctp_frag_send(frag, btl_endpoint->endpoint_sd)) {
                        int btl_ownership = (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
                        OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
                        frag->base.des_cbfunc(&frag->btl->super, frag->endpoint, &frag->base, frag->rc);
                        if( btl_ownership ) {
                            MCA_BTL_SCTP_FRAG_RETURN(frag);
                        }
                        return OMPI_SUCCESS;
                    } else {
                        btl_endpoint->endpoint_send_frag = frag;
                        opal_event_add(&btl_endpoint->endpoint_send_event, 0);
                    }
                } else {
                    opal_list_append(&btl_endpoint->endpoint_frags, (opal_list_item_t*)frag);
                }
                break;
            case MCA_BTL_SCTP_SHUTDOWN:
                rc = OMPI_ERROR;
                break;
        }
        OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
        return rc;
    }

    else {
        /* 1 to many */
        int rc = OMPI_SUCCESS;
        
        /* What if there are multiple procs on this endpoint? Possible? */
        orte_vpid_t vpid = btl_endpoint->endpoint_proc->proc_name.vpid;
        OPAL_THREAD_LOCK(&btl_endpoint->endpoint_send_lock);

        if((mca_btl_sctp_proc_check_vpid(vpid, sender_proc_table)) == INVALID_ENTRY) {
            opal_list_append(&btl_endpoint->endpoint_frags, (opal_list_item_t*)frag);

            rc = mca_btl_sctp_endpoint_start_connect(btl_endpoint);

            /* add the proc to sender_proc_table somewhere here */
            mca_btl_sctp_proc_add_vpid(vpid, btl_endpoint->endpoint_proc, sender_proc_table);
        }
        else {   /* VALID_ENTRY */

            if (btl_endpoint->endpoint_send_frag == NULL) {
                if(frag->base.des_flags & MCA_BTL_DES_FLAGS_PRIORITY &&
                        mca_btl_sctp_frag_send(frag, btl_endpoint->endpoint_sd)) {
                    int btl_ownership = (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
                    OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
                    frag->base.des_cbfunc(&frag->btl->super, frag->endpoint, &frag->base, frag->rc);
                    if( btl_ownership ) {
                        MCA_BTL_SCTP_FRAG_RETURN(frag);
                    }
                    return OMPI_SUCCESS;
                } else {
                    btl_endpoint->endpoint_send_frag = frag;

                    /* TODO make this all a function since repeated below */
                    
                    /* if this endpoint is already in the sending_endpoints list; don't add duplicate */
                    /* if the associated endpoint is this one, avoid putting into the list */
                    if(btl_endpoint->endpoint_in_list ||
                       btl_endpoint == endpoint_associated_with_send) { /* pointer comparison assumes no dup endpoints */
                        ; /* leave */
                    }
                    else if(NULL != endpoint_associated_with_send) {
                        /* another endpoint associated with sending on this one-to-many socket */
                        
                        /* put this endpoint in the list of endpoints that need to send no matter what... */
                        our_sctp_endpoint *our_btl_endpoint;

                        if(0 == opal_list_get_size(&sending_endpoints_freelist))
                        {
                            /* need to expand the freelist */
                        
                            our_sctp_endpoint *free_entries;
                            int i;

                            if(NULL == (endpoint_chunks_to_free_tail->next = (sending_endpoint_chunk *)
                                        malloc(sizeof(sending_endpoint_chunk))))
                            {
                                BTL_ERROR(("cannot allocate sending endpoint chunk."));
                                return OMPI_ERR_OUT_OF_RESOURCE;
                            }

                            
                            if(NULL == (free_entries = (our_sctp_endpoint *) malloc(sizeof(our_sctp_endpoint)
                                                                * INITIAL_NUM_FREE_POOL_SLOTS)))
                            {
                                BTL_ERROR(("cannot allocate free poll entries."));
                                /* only 1 of 2 newly required allocations were successful so free the other
                                 *  and reset the value so the generic destruction function works.
                                 */
                                free(endpoint_chunks_to_free_tail->next);
                                endpoint_chunks_to_free_tail->next = NULL;
                                return OMPI_ERR_OUT_OF_RESOURCE;
                            }
                            memset(free_entries, 0, sizeof(our_sctp_endpoint) * INITIAL_NUM_FREE_POOL_SLOTS);
                            for(i=0; i < INITIAL_NUM_FREE_POOL_SLOTS; i++) {
                                opal_list_append(&sending_endpoints_freelist,
                                                 (opal_list_item_t *) &free_entries[i]);
                            }

                            /* update location of where we'll expand next (if need be) */
                            endpoint_chunks_to_free_tail = endpoint_chunks_to_free_tail->next;
                            endpoint_chunks_to_free_tail->next = NULL;
                            endpoint_chunks_to_free_tail->to_free = free_entries;

                                                        
                        }
                        our_btl_endpoint = (our_sctp_endpoint *) opal_list_remove_first(&sending_endpoints_freelist);
                        memset(our_btl_endpoint, 0, sizeof(our_sctp_endpoint));
                        our_btl_endpoint->endpoint = btl_endpoint;
                        opal_list_append(&sending_endpoints, (opal_list_item_t *) our_btl_endpoint);
                        btl_endpoint->endpoint_in_list++;
                    } else {
                        
                        /* no endpoint is currently associated with sending on this socket */
                        opal_event_add(&btl_endpoint->endpoint_send_event, 0);
                        endpoint_associated_with_send = btl_endpoint;
                    }                    
                }
            } else {
                /* this btl has pending frags to send to queue this new one */
                opal_list_append(&btl_endpoint->endpoint_frags, (opal_list_item_t*)frag);
            }
        }
        OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
        return rc;
    }
}



/*
 * A blocking send on a non-blocking socket. Used to send the small amount of connection
 * information that identifies the endpoints endpoint.
 */
static int mca_btl_sctp_endpoint_send_blocking(mca_btl_base_endpoint_t* btl_endpoint, void* data, size_t size)
{
    if(mca_btl_sctp_component.sctp_if_11) {
        /* 1 to 1 */
        unsigned char* ptr = (unsigned char*)data;
        size_t cnt = 0;
        while(cnt < size) {
            /* We don't need to pass the sockaddr and len in
             * sctp_sendmsg() as it's a TCP style 1 to 1 socket. 
             */
            int retval = sctp_sendmsg(btl_endpoint->endpoint_sd, (char *)ptr+cnt, size-cnt,
                                      0, 0, 0, 0, 0, 0, 0);
            if (retval >= 0) {
                SCTP_BTL_ERROR((" mca_btl_sctp_endpoint_send_blocking() sd=%d, retval=%d\n",
                                btl_endpoint->endpoint_sd, retval));
            }            
            
            if(retval < 0) {
                if(opal_socket_errno != EINTR && opal_socket_errno != EAGAIN &&
                   opal_socket_errno != EWOULDBLOCK) {
                    BTL_ERROR(("send() failed with errno=%d",opal_socket_errno));
                    mca_btl_sctp_endpoint_close(btl_endpoint);
                    return -1;
                }
                continue;
            }
            cnt += retval;
        }
        return cnt;
    }

    else {
        /* 1 to many */
        unsigned char* ptr = (unsigned char*)data;
        size_t cnt = 0;
        struct sockaddr_in btl_sockaddr;

        btl_sockaddr = mca_btl_sctp_utils_sockaddr_from_endpoint(btl_endpoint);
        while(cnt < size) {
            socklen_t len = sizeof(struct sockaddr_in);
            int retval = sctp_sendmsg(btl_endpoint->endpoint_sd, (char *)ptr+cnt, size-cnt,
                    (struct sockaddr *)&btl_sockaddr, len, 0, 0, 0, 0, 0);
            if (retval >= 0) {
                SCTP_BTL_ERROR((" mca_btl_sctp_endpoint_send_blocking() retval=%d\n", retval));
            }

            if(retval < 0) {
                if(opal_socket_errno != EINTR && opal_socket_errno != EAGAIN && opal_socket_errno != EWOULDBLOCK) {
                    BTL_ERROR(("send() failed with errno=%d",opal_socket_errno));
                    mca_btl_sctp_endpoint_close(btl_endpoint);
                    return -1;
                }
                continue;
            }
            cnt += retval;
        }
        return cnt;
    }
}


/*
 * Send the globally unique identifier for this process to a endpoint on 
 * a newly connected socket.
 */

static int mca_btl_sctp_endpoint_send_connect_ack(mca_btl_base_endpoint_t* btl_endpoint)
{
    /* send process identifier to remote endpoint */
    mca_btl_sctp_proc_t* btl_proc = mca_btl_sctp_proc_local();
    orte_process_name_t guid = btl_proc->proc_name;

    ORTE_PROCESS_NAME_HTON(guid);
    if(mca_btl_sctp_endpoint_send_blocking(btl_endpoint, &guid, sizeof(guid)) != 
          sizeof(guid)) {
        return OMPI_ERR_UNREACH;
    }
    return OMPI_SUCCESS;
}

/*
 * Check the state of this endpoint. If the incoming connection request matches
 * our endpoints address, check the state of our connection:
 * (1) if a connection has not been attempted, accept the connection
 * (2) if a connection has not been established, and the endpoints process identifier
 *     is less than the local process, accept the connection
 * otherwise, reject the connection and continue with the current connection 
 */

bool mca_btl_sctp_endpoint_accept(mca_btl_base_endpoint_t* btl_endpoint, struct sockaddr_in* addr, int sd)
{
    if(mca_btl_sctp_component.sctp_if_11) {
        /* 1 to 1 */
        mca_btl_sctp_addr_t* btl_addr;
        mca_btl_sctp_proc_t* this_proc = mca_btl_sctp_proc_local();
        int cmpval;

        OPAL_THREAD_LOCK(&btl_endpoint->endpoint_recv_lock);
        OPAL_THREAD_LOCK(&btl_endpoint->endpoint_send_lock);
        if(((btl_addr = btl_endpoint->endpoint_addr) != NULL) &&
           btl_addr->addr_inet.s_addr == addr->sin_addr.s_addr)
        {
            mca_btl_sctp_proc_t *endpoint_proc = btl_endpoint->endpoint_proc;
            cmpval = orte_util_compare_name_fields(ORTE_NS_CMP_ALL, 
                    &endpoint_proc->proc_ompi->proc_name,
                    &this_proc->proc_ompi->proc_name);
            if((btl_endpoint->endpoint_sd < 0) ||
                    (btl_endpoint->endpoint_state != MCA_BTL_SCTP_CONNECTED &&
                     cmpval < 0)) {
                mca_btl_sctp_endpoint_close(btl_endpoint);
                SCTP_BTL_ERROR(("endpoint_close in endpoint_accept #1\n"));
                btl_endpoint->endpoint_sd = sd;
                if(mca_btl_sctp_endpoint_send_connect_ack(btl_endpoint) != OMPI_SUCCESS) {
                    mca_btl_sctp_endpoint_close(btl_endpoint);
                    SCTP_BTL_ERROR(("endpoint_close in endpoint_accept #2\n"));
                    OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
                    OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
                    return false;
                }
                mca_btl_sctp_endpoint_event_init(btl_endpoint, sd);
                opal_event_add(&btl_endpoint->endpoint_recv_event, 0);
                mca_btl_sctp_endpoint_connected(btl_endpoint);
#if OPAL_ENABLE_DEBUG && WANT_PEER_DUMP
                mca_btl_sctp_endpoint_dump(btl_endpoint, "accepted");
#endif
                OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
                OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
                return true;
            }
        }
        OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
        OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
        return false;
    }

    else {
        /* 1 to many */
        mca_btl_sctp_addr_t* btl_addr;

        OPAL_THREAD_LOCK(&btl_endpoint->endpoint_recv_lock);
        OPAL_THREAD_LOCK(&btl_endpoint->endpoint_send_lock);
        if((btl_addr = btl_endpoint->endpoint_addr) != NULL) {

            /* conflicts can't happen with one-to-many socket */
            mca_btl_sctp_endpoint_event_init(btl_endpoint, sd);            
            opal_event_add(&btl_endpoint->endpoint_recv_event, 0);
#if OPAL_ENABLE_DEBUG && WANT_PEER_DUMP
            mca_btl_sctp_endpoint_dump(btl_endpoint, "accepted");
#endif
            OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
            OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
            return true;
        }
        OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
        OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
        return false;
    }
}


/*
 * Remove any event registrations associated with the socket
 * and update the endpoint state to reflect the connection has
 * been closed.
 */

void mca_btl_sctp_endpoint_close(mca_btl_base_endpoint_t* btl_endpoint)
{ 
    SCTP_BTL_ERROR(("inside endpoint_close (sd = %d)\n", btl_endpoint->endpoint_sd));
   
    if(btl_endpoint->endpoint_sd >= 0) {
        opal_event_del(&btl_endpoint->endpoint_recv_event);
        opal_event_del(&btl_endpoint->endpoint_send_event);
        CLOSE_THE_SOCKET(btl_endpoint->endpoint_sd);
        btl_endpoint->endpoint_sd = -1;
#if MCA_BTL_SCTP_ENDPOINT_CACHE
        free( btl_endpoint->endpoint_cache );
        btl_endpoint->endpoint_cache        = NULL;
        btl_endpoint->endpoint_cache_pos    = NULL;
        btl_endpoint->endpoint_cache_length = 0;
#endif  /* MCA_BTL_SCTP_ENDPOINT_CACHE */
    }
    btl_endpoint->endpoint_state = MCA_BTL_SCTP_CLOSED;
    btl_endpoint->endpoint_retries++;
}

void mca_btl_sctp_endpoint_shutdown(mca_btl_base_endpoint_t* btl_endpoint)
{
    OPAL_THREAD_LOCK(&btl_endpoint->endpoint_recv_lock);
    OPAL_THREAD_LOCK(&btl_endpoint->endpoint_send_lock);
    mca_btl_sctp_endpoint_close(btl_endpoint);
    btl_endpoint->endpoint_state = MCA_BTL_SCTP_SHUTDOWN;
    OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
    OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
}


/*
 *  Setup endpoint state to reflect that connection has been established,
 *  and start any pending sends.
 */

static void mca_btl_sctp_endpoint_connected(mca_btl_base_endpoint_t* btl_endpoint)
{
    if(mca_btl_sctp_component.sctp_if_11) {
        /* 1 to 1 */
        btl_endpoint->endpoint_state = MCA_BTL_SCTP_CONNECTED;
        btl_endpoint->endpoint_retries = 0;
        if(opal_list_get_size(&btl_endpoint->endpoint_frags) > 0) {
            if(NULL == btl_endpoint->endpoint_send_frag) {
                btl_endpoint->endpoint_send_frag = (mca_btl_sctp_frag_t*) 
                    opal_list_remove_first(&btl_endpoint->endpoint_frags);
            }
            opal_event_add(&btl_endpoint->endpoint_send_event, 0);
        }
    }
    else {
        /* 1 to many */
        btl_endpoint->endpoint_state = MCA_BTL_SCTP_CONNECTED;
        btl_endpoint->endpoint_retries = 0;
        if(opal_list_get_size(&btl_endpoint->endpoint_frags) > 0) {
            if(NULL == btl_endpoint->endpoint_send_frag) {
                btl_endpoint->endpoint_send_frag = (mca_btl_sctp_frag_t*)
                    opal_list_remove_first(&btl_endpoint->endpoint_frags);
            }


            /* TOOD make this all a function since repeated below */
                    
            /* if this endpoint is already in the sending_endpoints list; don't add duplicate */
            /* if the associated endpoint is this one, avoid putting into the list */
            if(btl_endpoint->endpoint_in_list ||
               btl_endpoint == endpoint_associated_with_send) { /* pointer comparison assumes no dup endpoints */
                ; /* leave */
            }
            else if(NULL != endpoint_associated_with_send) {
                /* another endpoint associated with sending on this one-to-many socket */
                        
                /* put this endpoint in the list of endpoints that need to send no matter what... */
                our_sctp_endpoint *our_btl_endpoint;

                if(0 == opal_list_get_size(&sending_endpoints_freelist))
                    {
                        /* need to expand the freelist */
                        
                        our_sctp_endpoint *free_entries;
                        int i;

                        if(NULL == (endpoint_chunks_to_free_tail->next = (sending_endpoint_chunk *)
                                    malloc(sizeof(sending_endpoint_chunk))))
                            {
                                BTL_ERROR(("cannot allocate sending endpoint chunk."));
                                return;
                            }

                            
                        if(NULL == (free_entries = (our_sctp_endpoint *) malloc(sizeof(our_sctp_endpoint)
                                                                                * INITIAL_NUM_FREE_POOL_SLOTS)))
                            {
                                BTL_ERROR(("cannot allocate free poll entries."));
                                /* only 1 of 2 newly required allocations were successful so free the other
                                 *  and reset the value so the generic destruction function works.
                                 */
                                free(endpoint_chunks_to_free_tail->next);
                                endpoint_chunks_to_free_tail->next = NULL;
                                return;
                            }
                        for(i=0; i < INITIAL_NUM_FREE_POOL_SLOTS; i++) {
                            opal_list_append(&sending_endpoints_freelist,
                                             (opal_list_item_t *) &free_entries[i]);
                        }

                        /* update location of where we'll expand next (if need be) */
                        endpoint_chunks_to_free_tail = endpoint_chunks_to_free_tail->next;
                        endpoint_chunks_to_free_tail->next = NULL;
                        endpoint_chunks_to_free_tail->to_free = free_entries;

                                                        
                    }
                our_btl_endpoint = (our_sctp_endpoint *) opal_list_remove_first(&sending_endpoints_freelist);
                memset(our_btl_endpoint, 0, sizeof(our_sctp_endpoint));
                our_btl_endpoint->endpoint = btl_endpoint;
                opal_list_append(&sending_endpoints, (opal_list_item_t *) our_btl_endpoint);
                btl_endpoint->endpoint_in_list++;
            } else {
                        
                /* no endpoint is currently associated with sending on this socket */
                opal_event_add(&btl_endpoint->endpoint_send_event, 0);
                endpoint_associated_with_send = btl_endpoint;
            }                                
        }
    }
}


/*
 * A blocking recv on a non-blocking socket. Used to receive the small amount of connection
 * information that identifies the endpoints endpoint.
 */
static int mca_btl_sctp_endpoint_recv_blocking(mca_btl_base_endpoint_t* btl_endpoint, void* data, size_t size)
{
    if(mca_btl_sctp_component.sctp_if_11) {
        /* 1 to 1 */
        unsigned char* ptr = (unsigned char*)data;
        size_t cnt = 0;
        int retval;
        int msg_flags=0;
        struct sctp_sndrcvinfo sri;
        
        while(cnt < size) {
            /* We don't need to pass the sockaddr and len in
             * sctp_recvmsg() as it's a TCP style 1 to 1 socket. 
             */
            retval = sctp_recvmsg(btl_endpoint->endpoint_sd, (char *)ptr+cnt, size-cnt, 
                                  0, 0, &sri, &msg_flags);

            /* remote closed connection */
            if((retval == -1 && (opal_socket_errno == ECONNRESET || opal_socket_errno == EBADF))
               || retval == 0) {
                mca_btl_sctp_endpoint_close(btl_endpoint);
                return -1;
            }

            /* socket is non-blocking so handle errors */
            if(retval < 0) {
                if(opal_socket_errno != EINTR && opal_socket_errno != EAGAIN && opal_socket_errno != EWOULDBLOCK) {
                    BTL_ERROR(("recv() failed with errno=%d",opal_socket_errno));
                    mca_btl_sctp_endpoint_close(btl_endpoint);
                    return -1;
                }
                continue;
            }
            SCTP_BTL_ERROR(("mca_btl_sctp_endpoint_recv_blocking() sd=%d, got %d bytes.\n", btl_endpoint->endpoint_sd, retval));
            
            cnt += retval;
        }
        return cnt;
    }

    assert(0); /* not called in 1 to many, i don't think... */
    return -1; /* never happens. avoids compiler warning. */
}



/*
 *  Receive the endpoints globally unique process identification from a newly
 *  connected socket and verify the expected response. If so, move the
 *  socket to a connected state.
 */

static int mca_btl_sctp_endpoint_recv_connect_ack(mca_btl_base_endpoint_t* btl_endpoint)
{
    orte_process_name_t guid;
    mca_btl_sctp_proc_t* btl_proc = btl_endpoint->endpoint_proc;

    if((mca_btl_sctp_endpoint_recv_blocking(btl_endpoint, &guid,
                                            sizeof(orte_process_name_t))) != sizeof(orte_process_name_t)) {
        return OMPI_ERR_UNREACH;
    }
    ORTE_PROCESS_NAME_NTOH(guid);

    /* compare this to the expected values */
    if(memcmp(&btl_proc->proc_name, &guid, sizeof(orte_process_name_t)) != 0) {
        BTL_ERROR(("received unexpected process identifier %s", 
                   ORTE_NAME_PRINT(&guid)));
        mca_btl_sctp_endpoint_close(btl_endpoint);
        return OMPI_ERR_UNREACH;
    }

    /* connected */
    mca_btl_sctp_endpoint_connected(btl_endpoint);
#if OPAL_ENABLE_DEBUG && WANT_PEER_DUMP
    mca_btl_sctp_endpoint_dump(btl_endpoint, "connected");
#endif
    return OMPI_SUCCESS;
}


int mca_btl_sctp_set_socket_options(int sd)
{
    int optval, flags;
    struct sctp_event_subscribe evnts;

    /* register io event here to see populated sndrcvinfo struct */

    memset(&evnts, 0, sizeof(evnts));
    evnts.sctp_data_io_event = 1;
    if(setsockopt(sd, IPPROTO_SCTP, SCTP_EVENTS, &evnts, sizeof(evnts)) < 0) {
        BTL_ERROR(("setsockopt(SCTP_EVENTS) failed with errno=%d", opal_socket_errno));
        return OMPI_ERROR;        
    }
    
    /*TODO set maximum number of streams */

#if defined(SCTP_NODELAY)
    optval = mca_btl_sctp_component.sctp_use_nodelay;
    if(setsockopt(sd, IPPROTO_SCTP, SCTP_NODELAY, (char *)&optval, sizeof(optval)) < 0) {
        BTL_ERROR(("setsockopt(SCTP_NODELAY) failed with errno=%d", opal_socket_errno));
        return OMPI_ERROR;
    }
#endif
#if defined(SO_SNDBUF)
    if(mca_btl_sctp_component.sctp_sndbuf > 0 &&
       setsockopt(sd, SOL_SOCKET, SO_SNDBUF, (char *)&mca_btl_sctp_component.sctp_sndbuf, sizeof(int)) < 0) {
        BTL_ERROR(("setsockopt(SO_SNDBUF) failed with errno %d", opal_socket_errno));
        return OMPI_ERROR;
    }
#endif
#if defined(SO_RCVBUF)
    if(mca_btl_sctp_component.sctp_rcvbuf > 0 &&
       setsockopt(sd, SOL_SOCKET, SO_RCVBUF, (char *)&mca_btl_sctp_component.sctp_rcvbuf, sizeof(int)) < 0) {
        BTL_ERROR(("setsockopt(SO_RCVBUF) failed with errno %d", opal_socket_errno));
        return OMPI_ERROR;
    }
#endif

    /* set socket up to be non-blocking  */
    if((flags = fcntl(sd, F_GETFL, 0)) < 0) {
        BTL_ERROR(("fcntl(F_GETFL) failed with errno=%d", opal_socket_errno));
        return OMPI_ERROR;
    } else {
        flags |= O_NONBLOCK;
        if(fcntl(sd, F_SETFL, flags) < 0) {
            BTL_ERROR(("fcntl(F_SETFL) failed with errno=%d", opal_socket_errno));
            return OMPI_ERROR;
        }
    }

    return OMPI_SUCCESS;
}



/*
 *  Start a connection to the endpoint. This will likely not complete,
 *  as the socket is set to non-blocking, so register for event
 *  notification of connect completion. On connection we send
 *  our globally unique process identifier to the endpoint and wait for
 *  the endpoints response.
 */

static int mca_btl_sctp_endpoint_start_connect(mca_btl_base_endpoint_t* btl_endpoint)
{
    if(mca_btl_sctp_component.sctp_if_11) {
        /* 1 to 1 */
        int rc;
        struct sockaddr_in endpoint_addr;

        btl_endpoint->endpoint_sd = socket(AF_INET, SOCK_STREAM, IPPROTO_SCTP);
        if (btl_endpoint->endpoint_sd < 0) {
            btl_endpoint->endpoint_retries++;
            return OMPI_ERR_UNREACH;
        }

        /* bind NIC in this btl to this newly created socket */
        if(bind(btl_endpoint->endpoint_sd, (struct sockaddr *)
                &btl_endpoint->endpoint_btl->sctp_ifaddr,
                sizeof(struct sockaddr_in)) < 0) {
                return OMPI_ERR_FATAL;
        }

        
        /* setup socket buffer sizes */
        if((rc = mca_btl_sctp_set_socket_options(btl_endpoint->endpoint_sd)) != OMPI_SUCCESS) {
            return OMPI_ERR_FATAL;
        }

        /* setup event callbacks */
        mca_btl_sctp_endpoint_event_init(btl_endpoint, btl_endpoint->endpoint_sd);
       
        /* start the connect - will likely fail with EINPROGRESS */
	memset(&endpoint_addr, 0, sizeof(endpoint_addr));
        endpoint_addr.sin_family = AF_INET;
        endpoint_addr.sin_addr = btl_endpoint->endpoint_addr->addr_inet;
        endpoint_addr.sin_port = btl_endpoint->endpoint_addr->addr_port;
#ifdef FREEBSD
        endpoint_addr.sin_len = sizeof(struct sockaddr);
#endif
        if(connect(btl_endpoint->endpoint_sd, (struct sockaddr*)&endpoint_addr,
                   sizeof(endpoint_addr)) < 0)
        {
            /* non-blocking so wait for completion */
            if(opal_socket_errno == EINPROGRESS ||
               opal_socket_errno == EWOULDBLOCK)
            {
                btl_endpoint->endpoint_state = MCA_BTL_SCTP_CONNECTING;
                opal_event_add(&btl_endpoint->endpoint_send_event, 0);
                return OMPI_SUCCESS;
            }
            SCTP_BTL_ERROR(("endpoint_close in start_connect #1\n"));
            mca_btl_sctp_endpoint_close(btl_endpoint);
            btl_endpoint->endpoint_retries++;
            return OMPI_ERR_UNREACH;
        }

        /* send our globally unique process identifier to the endpoint */
        if((rc = mca_btl_sctp_endpoint_send_connect_ack(btl_endpoint)) == OMPI_SUCCESS) {
            btl_endpoint->endpoint_state = MCA_BTL_SCTP_CONNECT_ACK;
            opal_event_add(&btl_endpoint->endpoint_recv_event, 0);
        } else {
            SCTP_BTL_ERROR(("endpoint_close in start_connect #2\n"));
            mca_btl_sctp_endpoint_close(btl_endpoint);
        }
        return rc;
    }

    else {
        /* 1 to many */
        int rc;
        struct sockaddr_in endpoint_addr;

        /* setup event callbacks */
        mca_btl_sctp_endpoint_event_init(btl_endpoint, btl_endpoint->endpoint_sd);
        if (btl_endpoint->endpoint_sd < 0) {
            btl_endpoint->endpoint_retries++;
            return OMPI_ERR_UNREACH;
        }
       
        endpoint_addr = mca_btl_sctp_utils_sockaddr_from_endpoint(btl_endpoint);

        /* send our globally unique process identifier to the endpoint */
        if((rc = mca_btl_sctp_endpoint_send_connect_ack(btl_endpoint)) == OMPI_SUCCESS) {
            /* Changing this to CONNECTED so that we bipass application level acks. */
            mca_btl_sctp_endpoint_connected(btl_endpoint);

        } else {
            SCTP_BTL_ERROR(("endpoint_close in start_connect #3\n"));            
            mca_btl_sctp_endpoint_close(btl_endpoint);
        }
        return rc;
    }
}


/*
 * Check the status of the connection. If the connection failed, will retry
 * later. Otherwise, send this processes identifier to the endpoint on the 
 * newly connected socket.
 */

static void mca_btl_sctp_endpoint_complete_connect(mca_btl_base_endpoint_t* btl_endpoint)
{
    int so_error = 0;
    opal_socklen_t so_length = sizeof(so_error);

    /* unregister from receiving event notifications */
    opal_event_del(&btl_endpoint->endpoint_send_event);

    /* check connect completion status */
    if(getsockopt(btl_endpoint->endpoint_sd, SOL_SOCKET, SO_ERROR, (char *)&so_error, &so_length) < 0) {
        BTL_ERROR(("getsockopt() failed with errno=%d", opal_socket_errno));
        mca_btl_sctp_endpoint_close(btl_endpoint);
        return;
    }
    if(so_error == EINPROGRESS || so_error == EWOULDBLOCK) {
        opal_event_add(&btl_endpoint->endpoint_send_event, 0);
        return;
    }
    if(so_error != 0) {
        BTL_ERROR(("connect() failed with errno=%d", so_error));
        mca_btl_sctp_endpoint_close(btl_endpoint);
        return;
    }

    if(mca_btl_sctp_endpoint_send_connect_ack(btl_endpoint) == OMPI_SUCCESS) {
        btl_endpoint->endpoint_state = MCA_BTL_SCTP_CONNECT_ACK;
        opal_event_add(&btl_endpoint->endpoint_recv_event, 0);
    } else {
        mca_btl_sctp_endpoint_close(btl_endpoint);
    }
}

/* used in 1-1 only */
/*
 * A file descriptor is available/ready for recv. Check the state 
 * of the socket and take the appropriate action.
 */

static void mca_btl_sctp_endpoint_recv_handler(int sd, short flags, void* user)
{
    char *foo;
    int foo_len;
    mca_btl_base_endpoint_t* btl_endpoint = (mca_btl_base_endpoint_t *)user;
    OPAL_THREAD_LOCK(&btl_endpoint->endpoint_recv_lock);
    switch(btl_endpoint->endpoint_state) {
    case MCA_BTL_SCTP_CONNECT_ACK:
        {
            mca_btl_sctp_endpoint_recv_connect_ack(btl_endpoint);
            OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
            break;
        }
    case MCA_BTL_SCTP_CONNECTED:
        {
            mca_btl_sctp_frag_t* frag;

            frag = btl_endpoint->endpoint_recv_frag;
            if(NULL == frag) {
                int rc;
                if(mca_btl_sctp_module.super.btl_max_send_size > 
                   mca_btl_sctp_module.super.btl_eager_limit) { 
                    MCA_BTL_SCTP_FRAG_ALLOC_MAX(frag, rc);
                } else { 
                    MCA_BTL_SCTP_FRAG_ALLOC_EAGER(frag, rc);
                }
                
                if(NULL == frag) {
                    OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
                    return;
                }
                MCA_BTL_SCTP_FRAG_INIT_DST(frag, btl_endpoint);
            }

#if MCA_BTL_SCTP_ENDPOINT_CACHE
            assert( 0 == btl_endpoint->endpoint_cache_length );
        data_still_pending_on_endpoint:
#endif  /* MCA_BTL_SCTP_ENDPOINT_CACHE */
            /* check for completion of non-blocking recv on the current fragment */

            /* Changed frag_recv call so adding dummy params here to allow
             * compilation. This setup does not work for 1 to 1.
             */
            if(mca_btl_sctp_frag_recv(frag, sd, foo, foo_len) == false) {
                btl_endpoint->endpoint_recv_frag = frag;
            } else {
                btl_endpoint->endpoint_recv_frag = NULL;
                switch(frag->hdr.type) {
                case MCA_BTL_SCTP_HDR_TYPE_SEND:
                    {
                        mca_btl_active_message_callback_t* reg;
                        reg = mca_btl_base_active_message_trigger + frag->hdr.base.tag;
                        reg->cbfunc(&frag->btl->super, frag->hdr.base.tag, &frag->base, reg->cbdata);
                        break;
                    }
                default:
                    break;
                }
#if MCA_BTL_SCTP_ENDPOINT_CACHE
                if( 0 != btl_endpoint->endpoint_cache_length ) {
		    /* If the cache still contain some data we can reuse the same fragment
		     * until we flush it completly.
		     */
                    MCA_BTL_SCTP_FRAG_INIT_DST(frag, btl_endpoint);
                    goto data_still_pending_on_endpoint;
                }
#endif  /* MCA_BTL_SCTP_ENDPOINT_CACHE */
                MCA_BTL_SCTP_FRAG_RETURN(frag);
            }
            OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
#if MCA_BTL_SCTP_ENDPOINT_CACHE
            assert( 0 == btl_endpoint->endpoint_cache_length );
#endif  /* MCA_BTL_SCTP_ENDPOINT_CACHE */
            break;
        }
    case MCA_BTL_SCTP_SHUTDOWN:
        OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
        break;
    default:
        OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
        BTL_ERROR(("invalid socket state(%d)", btl_endpoint->endpoint_state));
        mca_btl_sctp_endpoint_close(btl_endpoint);
        break;
    }
}


/*
 * A file descriptor is available/ready for send. Check the state
 * of the socket and take the appropriate action.
 */

static void mca_btl_sctp_endpoint_send_handler(int sd, short flags, void* user)
{
    if(mca_btl_sctp_component.sctp_if_11) {
        /* 1 to 1 */
        mca_btl_sctp_endpoint_t* btl_endpoint = (mca_btl_sctp_endpoint_t *)user;
        OPAL_THREAD_LOCK(&btl_endpoint->endpoint_send_lock);
        switch(btl_endpoint->endpoint_state) {
            case MCA_BTL_SCTP_CONNECTING:
                mca_btl_sctp_endpoint_complete_connect(btl_endpoint);
                break;
            case MCA_BTL_SCTP_CONNECTED:
                {
                    int btl_ownership;
                    /* complete the current send */
                    do {
                        mca_btl_sctp_frag_t* frag = btl_endpoint->endpoint_send_frag;
                        if(mca_btl_sctp_frag_send(frag, btl_endpoint->endpoint_sd) == false) {
                            break;
                        }
                        /* progress any pending sends */
                        btl_endpoint->endpoint_send_frag = (mca_btl_sctp_frag_t*)
                            opal_list_remove_first(&btl_endpoint->endpoint_frags);

                        btl_ownership = (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
                        /* if required - update request status and release fragment */
                        OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
                        frag->base.des_cbfunc(&frag->btl->super, frag->endpoint, &frag->base, frag->rc);
                        OPAL_THREAD_LOCK(&btl_endpoint->endpoint_send_lock);
                        if( btl_ownership ) {
                            MCA_BTL_SCTP_FRAG_RETURN(frag);
                        }
                    } while (NULL != btl_endpoint->endpoint_send_frag);

                    /* if nothing else to do unregister for send event notifications */
                    if(NULL == btl_endpoint->endpoint_send_frag) {
                        opal_event_del(&btl_endpoint->endpoint_send_event);
                    }
                    break;
                }
            default:
                BTL_ERROR(("invalid connection state (%d)",
                            btl_endpoint->endpoint_state));
                opal_event_del(&btl_endpoint->endpoint_send_event);
                break;
        }
        OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
    }

    else {
        /* 1 to many */
        mca_btl_sctp_endpoint_t* btl_endpoint = (mca_btl_sctp_endpoint_t *)user;
        our_sctp_endpoint *current_our_endpoint = NULL;
        orte_vpid_t vpid;
    send_handler_1_to_many_different_endpoint: 
        vpid = btl_endpoint->endpoint_proc->proc_name.vpid;
        OPAL_THREAD_LOCK(&btl_endpoint->endpoint_send_lock);
        if((mca_btl_sctp_proc_check_vpid(vpid, sender_proc_table)) == VALID_ENTRY) {                 int btl_ownership;

            /* complete the current send */
            do {
                mca_btl_sctp_frag_t* frag = btl_endpoint->endpoint_send_frag;
                if(NULL == frag) {
                    break;
                }
                if(mca_btl_sctp_frag_send(frag, btl_endpoint->endpoint_sd) == false) {
                    break;
                }
                /* progress any pending sends */
                btl_endpoint->endpoint_send_frag = (mca_btl_sctp_frag_t*)
                    opal_list_remove_first(&btl_endpoint->endpoint_frags);

                btl_ownership = (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
                /* if required - update request status and release fragment */
                OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
                frag->base.des_cbfunc(&frag->btl->super, frag->endpoint, &frag->base, frag->rc);
                OPAL_THREAD_LOCK(&btl_endpoint->endpoint_send_lock);
                if( btl_ownership ) {
                    MCA_BTL_SCTP_FRAG_RETURN(frag);
                }

            }while (NULL != btl_endpoint->endpoint_send_frag);

            /* if nothing else to do unregister for send event notifications */
            if(NULL == btl_endpoint->endpoint_send_frag && NULL == current_our_endpoint) {

                /* remove the send event with this endpoint */
                opal_event_del(&btl_endpoint->endpoint_send_event);
                endpoint_associated_with_send = NULL;

                /* see if there is another endpoint that wants the send event */
                if(opal_list_get_size(&sending_endpoints) > 0) {

                    /* entries in the list may be stale from walk-throughs, so prune these
                     *  while verifying the entries.  now pruned everytime the endpoint associated with
                     *  POLLOUT on the one-to-many socket completes sending all of its frags.
                     */
                    our_sctp_endpoint *our_btl_endpoint;
                    mca_btl_sctp_endpoint_t *next_endpoint;
                    
                    while(opal_list_get_size(&sending_endpoints)) {
                        our_btl_endpoint = (our_sctp_endpoint *) opal_list_remove_first(&sending_endpoints);
                        next_endpoint = our_btl_endpoint->endpoint;
                        if(NULL == next_endpoint->endpoint_send_frag)
                        {
                            /* stale entry */
                            opal_list_append(&sending_endpoints_freelist, (opal_list_item_t *) our_btl_endpoint);
                            our_btl_endpoint = NULL;
                            next_endpoint->endpoint_in_list--;
                            continue;
                        }
                        else
                        {
                            /* go with this one */
                            break;
                        }
                    }

                    if(NULL != our_btl_endpoint)
                    {
                        OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
                        OPAL_THREAD_LOCK(&next_endpoint->endpoint_send_lock);
                        btl_endpoint = next_endpoint;
                        assert(btl_endpoint->endpoint_in_list > 0);
                        btl_endpoint->endpoint_in_list--;
                        opal_event_add(&btl_endpoint->endpoint_send_event, 0);
                        opal_list_append(&sending_endpoints_freelist, (opal_list_item_t *) our_btl_endpoint);
                        endpoint_associated_with_send = btl_endpoint;
                        goto send_handler_1_to_many_different_endpoint;
                    }
                }
            } else if(opal_list_get_size(&sending_endpoints) > 0) {
                /* sending_endpoints might contain entries with different endpoints (i.e. non-duplicates) */

                /* This case happens if one association on the one-to-many socket returns EAGAIN but we
                 *  want to try another association on the same socket (in the kernel, different associations
                 *  have different buffers despite sharing the same socket).
                 */

                /* if we're in the middle of a traversal and we've completed all the frags on some endpoint,
                 *  we need to remove it from the list */
                OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
                if(NULL == current_our_endpoint)
                {
                    /* start traversing the list */
                    current_our_endpoint = (our_sctp_endpoint *) opal_list_get_first(&sending_endpoints);
                    sending_endpoints_walk_count = 0;
                }
                else
                {
                    /* continue traversing the list */
                    current_our_endpoint = (our_sctp_endpoint *) opal_list_get_next(current_our_endpoint);
                    sending_endpoints_walk_count++;
                }

                if(NULL != current_our_endpoint &&
                   sending_endpoints_walk_count < (int) opal_list_get_size(&sending_endpoints))
                {
                    btl_endpoint = current_our_endpoint->endpoint;
                    goto send_handler_1_to_many_different_endpoint;
                }
                else { /* done walking through sending_endpoints list */
                    return; /* don't want to unlock the send_lock twice */
                }
            }
        }
        else {
            BTL_ERROR(("invalid connection state (%d)",
                        btl_endpoint->endpoint_state));
            /*TODO: update del code to use sending_endpoints list */
            opal_event_del(&btl_endpoint->endpoint_send_event);
        }
        OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_send_lock);
    }
}
