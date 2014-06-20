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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <string.h>
#include <fcntl.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif

#include "ompi/constants.h"
#include "opal/event/event.h"

#include "opal/mca/base/mca_base_param.h"
#include "ompi/mca/mpool/base/base.h" 
#include "ompi/mca/btl/base/btl_base_error.h"
#include "btl_sctp.h"
#include "btl_sctp_addr.h"
#include "btl_sctp_proc.h"
#include "btl_sctp_frag.h"
#include "btl_sctp_endpoint.h" 
#include "ompi/mca/btl/base/base.h" 

#include <netinet/sctp.h>
#include "btl_sctp_recv_handler.h"
#include "btl_sctp_component.h"


/**
 * void mca_btl_sctp_print_sri(struct sctp_sndrcvinfo *sri)
 * --------------------------------------------------------
 *  Prints out sndrcvinfo data. Used only for diagnostic purposes.
 */

void mca_btl_sctp_print_sri(struct sctp_sndrcvinfo *sri) {
    SCTP_BTL_ERROR(("sri->sinfo_stream: %d\n", (int*)(sri->sinfo_stream)));
    SCTP_BTL_ERROR(("sri->sinfo_ssn: %d\n", sri->sinfo_ssn));
    SCTP_BTL_ERROR(("sri->sinfo_flags: %d\n", sri->sinfo_flags));
    SCTP_BTL_ERROR(("sri->sinfo_ppid: %d\n", sri->sinfo_ppid));
    SCTP_BTL_ERROR(("sri->sinfo_context: %d\n", sri->sinfo_context));
    SCTP_BTL_ERROR(("sri->sinfo_timetolive: %d\n", sri->sinfo_timetolive));
    SCTP_BTL_ERROR(("sri->sinfo_tsn: %d\n", sri->sinfo_tsn));
    SCTP_BTL_ERROR(("sri->sinfo_cumtsn: %d\n", sri->sinfo_cumtsn));
    SCTP_BTL_ERROR(("sri->sinfo_assoc_id: %u\n", sri->sinfo_assoc_id));
}

/* setup receive buffer */
static char * sctp_recv_buf = NULL;
int mca_btl_sctp_recv_handler_initbuf() {
    return (NULL != (sctp_recv_buf = (char *) malloc(mca_btl_sctp_component.sctp_rcvbuf + 1)));
}

/* free receive buffer */
void mca_btl_sctp_recv_handler_freebuf() {
    if(sctp_recv_buf) {
        free(sctp_recv_buf);
        sctp_recv_buf = NULL;
    }
}

/**
 * void mca_btl_sctp_recv_handler(int sd, short flags, void *user)
 * ---------------------------------------------------------------
 *  General callback function for when we have an event on our one to many SCTP
 *  socket.
 */

void mca_btl_sctp_recv_handler(int sd, short flags, void *user) {
    /* allocated this elsewhere only once per BTL to avoid repeatedly calling malloc */
    char *buf = sctp_recv_buf;

    orte_process_name_t guid;
    struct sockaddr_in their_addr;
    int retval;
    mca_btl_sctp_proc_t *btl_proc;
    /* mca_btl_sctp_event_t *event = (mca_btl_sctp_event_t *)user; */

    int msg_flags;
    socklen_t len = sizeof(struct sockaddr_in);
    struct sctp_sndrcvinfo sri;

 recv_handler_1_to_many_loop:
    /* Receive data from the socket. */
    retval = sctp_recvmsg(sd, buf, mca_btl_sctp_component.sctp_rcvbuf, 
            (struct sockaddr *)&their_addr, &len, &sri, &msg_flags);

    /* TODO move full error handling code from frag_recv to here. Can we? See next TODO... */
    if(-1 == retval && (errno == EAGAIN || errno == EINTR)) {
        return;
    } else if(-1 == retval) {
        perror("retval is -1");
        /* TODO can we determine the endpoint from the sri if
         *  this is an error? */
/*         mca_btl_sctp_endpoint_close(btl_endpoint);         */
        return;
    }
    
    SCTP_BTL_ERROR(("mca_btl_sctp_recv_handler(): got %d bytes.\n", retval));

    /* Print sndrcvinfo data. */
    mca_btl_sctp_print_sri(&sri);

    /* Check if sender is known to us. */
    
    if((mca_btl_sctp_proc_check_assoc_id(sri.sinfo_assoc_id, recvr_proc_table)) == VALID_ENTRY) {

        mca_btl_base_endpoint_t *btl_endpoint;
        mca_btl_sctp_frag_t* frag;
        
        btl_proc = mca_btl_sctp_proc_get(sri.sinfo_assoc_id, recvr_proc_table);
        btl_endpoint = btl_proc->proc_endpoints[0];
        assert(btl_proc->proc_endpoint_count == 1); /* true for 1-many */

        /* can't thread lock until after recv call since can't specify
         *  the receiver in sctp_recvmsg on a one-to-many socket... is
         *  this a problem?
         */
        OPAL_THREAD_LOCK(&btl_endpoint->endpoint_recv_lock);

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
        if(mca_btl_sctp_frag_recv(frag, sd, buf, retval) == false) {
            btl_endpoint->endpoint_recv_frag = frag;
            OPAL_THREAD_UNLOCK(&btl_endpoint->endpoint_recv_lock);
            return; /* EAGAIN occurred underneath so stop looping */
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
    }

    /* If not known then data recvd should be GUID. */
    else {

        /* TODO OPAL_THREAD_LOCK when endpoint unknown? */
        
        if(retval != sizeof(guid)) {
            BTL_ERROR(("Unexpected size of GUID.\n"));
            return;
        }

        /* Setup guid. */
        memcpy(&guid, buf, retval);
        ORTE_PROCESS_NAME_NTOH(guid);
      
        /* lookup the corresponding process */
        btl_proc = mca_btl_sctp_proc_lookup(&guid);
        if(NULL == btl_proc) {
            BTL_ERROR(("errno=%d",errno));
            CLOSE_THE_SOCKET(sd);
            return;
        }
        mca_btl_sctp_proc_add_assoc_id(sri.sinfo_assoc_id, btl_proc, recvr_proc_table);


        /* are there any existing peer instances will to accept this connection */
        if(mca_btl_sctp_proc_accept(btl_proc, &their_addr, sd) == false) {
            BTL_ERROR(("no one accepted!\n"));
            CLOSE_THE_SOCKET(sd);
        }

    }
    goto recv_handler_1_to_many_loop;

}
