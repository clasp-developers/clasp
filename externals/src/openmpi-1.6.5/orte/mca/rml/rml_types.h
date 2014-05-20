/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 *
 * Contains the typedefs for the use of the rml
 */

#ifndef MCA_RML_TYPES_H_
#define MCA_RML_TYPES_H_

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <limits.h>
#ifdef HAVE_SYS_UIO_H
/* for struct iovec */
#include <sys/uio.h>
#endif
#ifdef HAVE_NET_UIO_H
#include <net/uio.h>
#endif

#include "opal/dss/dss_types.h"
#include "opal/class/opal_list.h"

BEGIN_C_DECLS


/* ******************************************************************** */

typedef struct {
    opal_list_item_t super;
    orte_process_name_t sender;
    opal_buffer_t *buffer;
} orte_msg_packet_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_msg_packet_t);

#ifndef __WINDOWS__
#define ORTE_PROCESS_MESSAGE(rlist, lck, flg, fd, crt, sndr, buf)   \
    do {                                                            \
        orte_msg_packet_t *pkt;                                     \
        int data=1;                                                 \
        pkt = OBJ_NEW(orte_msg_packet_t);                           \
        pkt->sender.jobid = (sndr)->jobid;                          \
        pkt->sender.vpid = (sndr)->vpid;                            \
        if ((crt)) {                                                \
            pkt->buffer = OBJ_NEW(opal_buffer_t);                   \
            opal_dss.copy_payload(pkt->buffer, *(buf));             \
        } else {                                                    \
            pkt->buffer = *(buf);                                   \
            *(buf) = NULL;                                          \
        }                                                           \
        OPAL_THREAD_LOCK((lck));                                    \
        opal_list_append((rlist), &pkt->super);                     \
        if (!(flg)) {                                               \
            write((fd), &data, sizeof(data));                       \
        }                                                           \
        OPAL_THREAD_UNLOCK((lck));                                  \
    } while(0);
#else
#define ORTE_PROCESS_MESSAGE(rlist, lck, flg, fd, crt, sndr, buf)   \
    do {                                                            \
        orte_msg_packet_t *pkt;                                     \
        int data=1;                                                 \
        pkt = OBJ_NEW(orte_msg_packet_t);                           \
        pkt->sender.jobid = (sndr)->jobid;                          \
        pkt->sender.vpid = (sndr)->vpid;                            \
        if ((crt)) {                                                \
            pkt->buffer = OBJ_NEW(opal_buffer_t);                   \
            opal_dss.copy_payload(pkt->buffer, *(buf));             \
        } else {                                                    \
            pkt->buffer = *(buf);                                   \
            *(buf) = NULL;                                          \
        }                                                           \
        OPAL_THREAD_LOCK((lck));                                    \
        opal_list_append((rlist), &pkt->super);                     \
        if (!(flg)) {                                               \
            send((fd), (const char*) &data, sizeof(data), 0);       \
        }                                                           \
        OPAL_THREAD_UNLOCK((lck));                                  \
    } while(0);
#endif


/**
 * Constant tag values for well-known services
 */

#define ORTE_RML_TAG_T    OPAL_UINT32

#define ORTE_RML_TAG_INVALID                 0
#define ORTE_RML_TAG_DAEMON                  1
#define ORTE_RML_TAG_IOF_HNP                 2
#define ORTE_RML_TAG_IOF_PROXY               3
#define ORTE_RML_TAG_XCAST_BARRIER           4
#define ORTE_RML_TAG_PLM                     5
#define ORTE_RML_TAG_PLM_PROXY               6
#define ORTE_RML_TAG_ERRMGR                  7
#define ORTE_RML_TAG_WIREUP                  8
#define ORTE_RML_TAG_RML_INFO_UPDATE         9
#define ORTE_RML_TAG_ORTED_CALLBACK         10
#define ORTE_RML_TAG_APP_LAUNCH_CALLBACK    11
#define ORTE_RML_TAG_REPORT_REMOTE_LAUNCH   12

#define ORTE_RML_TAG_CKPT                   13

#define ORTE_RML_TAG_RML_ROUTE              14

#define ORTE_RML_TAG_ALLGATHER              15
#define ORTE_RML_TAG_ALLGATHER_LIST         16
#define ORTE_RML_TAG_BARRIER                17

#define ORTE_RML_TAG_INIT_ROUTES            18
#define ORTE_RML_TAG_UPDATE_ROUTE_ACK       19
#define ORTE_RML_TAG_SYNC                   20

/* For FileM Base */
#define ORTE_RML_TAG_FILEM_BASE             21
#define ORTE_RML_TAG_FILEM_BASE_RESP        22

/* For FileM RSH Component */
#define ORTE_RML_TAG_FILEM_RSH              23

/* For SnapC Framework */
#define ORTE_RML_TAG_SNAPC                  24
#define ORTE_RML_TAG_SNAPC_FULL             25

/* For tools */
#define ORTE_RML_TAG_TOOL                   26

/* support data store/lookup */
#define ORTE_RML_TAG_DATA_SERVER            27
#define ORTE_RML_TAG_DATA_CLIENT            28

/* timing related */
#define ORTE_RML_TAG_COLLECTIVE_TIMER       29

/* daemon collectives */
#define ORTE_RML_TAG_DAEMON_COLLECTIVE      30

/* show help */
#define ORTE_RML_TAG_SHOW_HELP              31

/* debugger release */
#define ORTE_RML_TAG_DEBUGGER_RELEASE       32

/* profile data */
#define ORTE_RML_TAG_GRPCOMM_PROFILE        33

/* onesided barrier */
#define ORTE_RML_TAG_ONESIDED_BARRIER       34

/* bootstrap */
#define ORTE_RML_TAG_BOOTSTRAP              35

#define ORTE_RML_TAG_MAX                   100


/** 
 * Message matching tag
 *
 * Message matching tag.  Unlike MPI, there is no wildcard receive,
 * all messages must match exactly. Tag values less than
 * ORTE_RML_TAG_DYNAMIC are reserved and may only be referenced using
 * a defined constant.
 */
typedef uint32_t orte_rml_tag_t;


/* ******************************************************************** */


/*
 * RML proxy commands
 */
typedef uint8_t orte_rml_cmd_flag_t;
#define ORTE_RML_CMD    OPAL_UINT8
#define ORTE_RML_UPDATE_CMD    1


/* ******************************************************************** */
/* Flags to send/recv */

/**
 * Non-persistent request that can be deleted when the request is
 * completed.  This is the default behavior.
 */
#define ORTE_RML_NON_PERSISTENT          0x00000000

/**
 * flag to oob_recv to allow caller to peek a portion of the next
 * available message w/out removing the message from the queue.
 */
#define ORTE_RML_PEEK                    0x00000001

/** 
 * flag to oob_recv to return the actual size of the message even if
 * the receive buffer is smaller than the number of bytes available 
 */
#define ORTE_RML_TRUNC                   0x00000002

/** 
 * flag to oob_recv to request the oob to allocate a buffer of the
 * appropriate size for the receive and return the allocated buffer
 * and size in the first element of the iovec array.
 */
#define ORTE_RML_ALLOC                   0x00000004

/**
 * posted non-blocking recv is persistent 
 */
#define ORTE_RML_PERSISTENT              0x00000008

/**
 * The request is a non-blocking request that can have its callback
 * triggered as soon as the request is completed, even if the OOB is
 * currently in the middle of another non-blocking request callback.
 */
#define ORTE_RML_FLAG_RECURSIVE_CALLBACK 0x00000010


typedef enum {
    ORTE_RML_PEER_UNREACH,
    ORTE_RML_PEER_DISCONNECTED
} orte_rml_exception_t;


END_C_DECLS


#endif  /* RML_TYPES */
