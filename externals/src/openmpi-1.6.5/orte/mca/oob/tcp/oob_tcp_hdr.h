/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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
/** @file:
 *
 *  Contains header used by tcp oob.
 */

#ifndef _MCA_OOB_TCP_HDR_H_
#define _MCA_OOB_TCP_HDR_H_

#include "orte_config.h"
#include "orte/types.h"

BEGIN_C_DECLS

#define MCA_OOB_TCP_PROBE    1
#define MCA_OOB_TCP_CONNECT  2
#define MCA_OOB_TCP_IDENT    3
#define MCA_OOB_TCP_DATA     4
#define MCA_OOB_TCP_PING     5

/**
 * Header used by tcp oob protocol.
 */
struct mca_oob_tcp_hdr_t {
    orte_process_name_t msg_origin;
    orte_process_name_t msg_src;
    orte_process_name_t msg_dst;
    uint32_t msg_type;                /**< type of message */
    uint32_t msg_size;                /**< the total size of the message body - excluding header */ 
    int32_t  msg_tag;                 /**< user provided tag */
};
typedef struct mca_oob_tcp_hdr_t mca_oob_tcp_hdr_t;

/**
 * Convert the message header to host byte order
 */
#define MCA_OOB_TCP_HDR_NTOH(h) \
    ORTE_PROCESS_NAME_NTOH((h)->msg_origin); \
    ORTE_PROCESS_NAME_NTOH((h)->msg_src); \
    ORTE_PROCESS_NAME_NTOH((h)->msg_dst); \
    (h)->msg_type = ntohl((h)->msg_type);		  \
    (h)->msg_size = ntohl((h)->msg_size);				  \
    (h)->msg_tag = ntohl((h)->msg_tag);

/**
 * Convert the message header to network byte order
 */
#define MCA_OOB_TCP_HDR_HTON(h) \
    ORTE_PROCESS_NAME_HTON((h)->msg_origin); \
    ORTE_PROCESS_NAME_HTON((h)->msg_src); \
    ORTE_PROCESS_NAME_HTON((h)->msg_dst); \
    (h)->msg_type = htonl((h)->msg_type);		  \
    (h)->msg_size = htonl((h)->msg_size);				  \
    (h)->msg_tag = htonl((h)->msg_tag);

END_C_DECLS

#endif /* _MCA_OOB_TCP_MESSAGE_H_ */

