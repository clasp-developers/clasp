/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_MCA_OSC_RDMA_HDR_H
#define OMPI_MCA_OSC_RDMA_HDR_H

#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "opal/types.h"

/* Note -- 0x05 to 0x0C are of control_hdr type */
#define OMPI_OSC_RDMA_HDR_PUT           0x01
#define OMPI_OSC_RDMA_HDR_ACC           0x02
#define OMPI_OSC_RDMA_HDR_GET           0x03
#define OMPI_OSC_RDMA_HDR_REPLY         0x04
#define OMPI_OSC_RDMA_HDR_POST          0x05
#define OMPI_OSC_RDMA_HDR_COMPLETE      0x06
#define OMPI_OSC_RDMA_HDR_LOCK_REQ      0x07
#define OMPI_OSC_RDMA_HDR_UNLOCK_REQ    0x08
#define OMPI_OSC_RDMA_HDR_UNLOCK_REPLY  0x09
#define OMPI_OSC_RDMA_HDR_RDMA_COMPLETE 0x0A
#define OMPI_OSC_RDMA_HDR_MULTI_END     0x0B
#define OMPI_OSC_RDMA_HDR_RDMA_INFO     0x0C

#define OMPI_OSC_RDMA_HDR_FLAG_ALIGN_MASK 0x0F
#define OMPI_OSC_RDMA_HDR_FLAG_NBO        0x10
#define OMPI_OSC_RDMA_HDR_FLAG_MULTI      0x20

struct ompi_osc_rdma_base_header_t {
    uint8_t hdr_type;
    /* eventually, this will include endian information */
    uint8_t hdr_flags;
};
typedef struct ompi_osc_rdma_base_header_t ompi_osc_rdma_base_header_t;

#define OMPI_OSC_RDMA_BASE_HDR_NTOH(h)
#define OMPI_OSC_RDMA_BASE_HDR_HTON(h)

struct ompi_osc_rdma_send_header_t {
    ompi_osc_rdma_base_header_t hdr_base;
    uint16_t hdr_windx;

    int32_t hdr_origin;
    ompi_ptr_t hdr_origin_sendreq;
    int32_t hdr_origin_tag;

    uint64_t hdr_target_disp;
    int32_t hdr_target_count;
    int32_t hdr_target_op;

    int32_t hdr_msg_length; /* 0 if payload is not included */
};
typedef struct ompi_osc_rdma_send_header_t ompi_osc_rdma_send_header_t;

#define OMPI_OSC_RDMA_SEND_HDR_HTON(hdr) \
    do { \
        OMPI_OSC_RDMA_BASE_HDR_HTON((hdr).hdr_base) \
        (hdr).hdr_windx = htons((hdr).hdr_windx); \
        (hdr).hdr_origin = htonl((hdr).hdr_origin); \
        (hdr).hdr_origin_tag = htonl((hdr).hdr_origin_tag); \
        (hdr).hdr_target_disp = hton64((hdr).hdr_target_disp); \
        (hdr).hdr_target_count = htonl((hdr).hdr_target_count); \
        (hdr).hdr_target_op = htonl((hdr).hdr_target_op); \
        (hdr).hdr_msg_length = htonl((hdr).hdr_msg_length); \
    } while (0)

#define OMPI_OSC_RDMA_SEND_HDR_NTOH(hdr) \
    do { \
        OMPI_OSC_RDMA_BASE_HDR_NTOH((hdr).hdr_base) \
        (hdr).hdr_windx = ntohs((hdr).hdr_windx); \
        (hdr).hdr_origin = ntohl((hdr).hdr_origin); \
        (hdr).hdr_origin_tag = ntohl((hdr).hdr_origin_tag); \
        (hdr).hdr_target_disp = ntoh64((hdr).hdr_target_disp); \
        (hdr).hdr_target_count = ntohl((hdr).hdr_target_count); \
        (hdr).hdr_target_op = ntohl((hdr).hdr_target_op); \
        (hdr).hdr_msg_length = ntohl((hdr).hdr_msg_length); \
    } while (0)


struct ompi_osc_rdma_reply_header_t {
    ompi_osc_rdma_base_header_t hdr_base;

    ompi_ptr_t hdr_origin_sendreq;

    int32_t hdr_target_tag;
    int32_t hdr_msg_length;
};
typedef struct ompi_osc_rdma_reply_header_t ompi_osc_rdma_reply_header_t;

#define OMPI_OSC_RDMA_REPLY_HDR_HTON(hdr) \
    do { \
        OMPI_OSC_RDMA_BASE_HDR_HTON((hdr).hdr_base) \
        (hdr).hdr_target_tag = htonl((hdr).hdr_target_tag); \
        (hdr).hdr_msg_length = htonl((hdr).hdr_msg_length); \
    } while (0)

#define OMPI_OSC_RDMA_REPLY_HDR_NTOH(hdr) \
    do { \
        OMPI_OSC_RDMA_BASE_HDR_NTOH((hdr).hdr_base) \
        (hdr).hdr_target_tag = ntohl((hdr).hdr_target_tag); \
        (hdr).hdr_msg_length = ntohl((hdr).hdr_msg_length); \
    } while (0)


struct ompi_osc_rdma_control_header_t {
    ompi_osc_rdma_base_header_t hdr_base;
    int16_t hdr_windx;
    int32_t hdr_value[2];
};
typedef struct ompi_osc_rdma_control_header_t ompi_osc_rdma_control_header_t;

#define OMPI_OSC_RDMA_CONTROL_HDR_HTON(hdr) \
    do { \
        OMPI_OSC_RDMA_BASE_HDR_HTON((hdr).hdr_base);    \
        (hdr).hdr_windx = htons((hdr).hdr_windx);       \
        (hdr).hdr_value[0] = htonl((hdr).hdr_value[0]); \
        (hdr).hdr_value[1] = htonl((hdr).hdr_value[1]); \
    } while (0)

#define OMPI_OSC_RDMA_CONTROL_HDR_NTOH(hdr) \
    do { \
        OMPI_OSC_RDMA_BASE_HDR_NTOH((hdr).hdr_base);    \
        (hdr).hdr_windx = ntohs((hdr).hdr_windx);       \
        (hdr).hdr_value[0] = ntohl((hdr).hdr_value[0]); \
        (hdr).hdr_value[1] = ntohl((hdr).hdr_value[1]); \
    } while (0)


struct ompi_osc_rdma_rdma_info_header_t {
    ompi_osc_rdma_base_header_t hdr_base;
    int16_t  hdr_windx;
    int32_t  hdr_origin;
    uint64_t hdr_segkey;
};
typedef struct ompi_osc_rdma_rdma_info_header_t ompi_osc_rdma_rdma_info_header_t;

#define OMPI_OSC_RDMA_RDMA_INFO_HDR_HTON(hdr)           \
    do {                                                \
        OMPI_OSC_RDMA_BASE_HDR_HTON((hdr).hdr_base);    \
        (hdr).hdr_windx = htons((hdr).hdr_windx);       \
        (hdr).hdr_origin = htonl((hdr).hdr_origin);     \
        (hdr).hdr_segkey = hton64((hdr).hdr_segkey);    \
    } while (0)

#define OMPI_OSC_RDMA_RDMA_INFO_HDR_NTOH(hdr)           \
    do {                                                \
        OMPI_OSC_RDMA_BASE_HDR_NTOH((hdr).hdr_base);    \
        (hdr).hdr_windx = ntohs((hdr).hdr_windx);       \
        (hdr).hdr_origin = ntohl((hdr).hdr_origin);     \
        (hdr).hdr_segkey = ntoh64((hdr).hdr_segkey);    \
    } while (0)


#endif /* OMPI_MCA_OSC_RDMA_HDR_H */
