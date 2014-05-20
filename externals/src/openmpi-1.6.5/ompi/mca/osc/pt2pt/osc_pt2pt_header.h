/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_MCA_OSC_PT2PT_HDR_H
#define OMPI_MCA_OSC_PT2PT_HDR_H

#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "opal/types.h"

#define OMPI_OSC_PT2PT_HDR_PUT        0x0001
#define OMPI_OSC_PT2PT_HDR_ACC        0x0002
#define OMPI_OSC_PT2PT_HDR_GET        0x0003
#define OMPI_OSC_PT2PT_HDR_REPLY      0x0004
#define OMPI_OSC_PT2PT_HDR_POST       0x0005
#define OMPI_OSC_PT2PT_HDR_COMPLETE   0x0006
#define OMPI_OSC_PT2PT_HDR_LOCK_REQ   0x0007
#define OMPI_OSC_PT2PT_HDR_UNLOCK_REQ 0x0008
#define OMPI_OSC_PT2PT_HDR_UNLOCK_REPLY 0x0009

#define OMPI_OSC_PT2PT_HDR_FLAG_NBO   0x0001

struct ompi_osc_pt2pt_base_header_t {
    uint8_t hdr_type;
    uint8_t hdr_flags;
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    uint8_t padding[2];
#endif
};
typedef struct ompi_osc_pt2pt_base_header_t ompi_osc_pt2pt_base_header_t;

#define OMPI_OSC_PT2PT_BASE_HDR_NTOH(h)
#define OMPI_OSC_PT2PT_BASE_HDR_HTON(h)

struct ompi_osc_pt2pt_send_header_t {
    ompi_osc_pt2pt_base_header_t hdr_base;

    int32_t hdr_origin;
    ompi_ptr_t hdr_origin_sendreq;
    int32_t hdr_origin_tag;

    uint64_t hdr_target_disp;
    int32_t hdr_target_count;
    int32_t hdr_target_op;

    int32_t hdr_msg_length; /* 0 if payload is not included */
};
typedef struct ompi_osc_pt2pt_send_header_t ompi_osc_pt2pt_send_header_t;

#define OMPI_OSC_PT2PT_SEND_HDR_HTON(hdr) \
    do { \
        OMPI_OSC_PT2PT_BASE_HDR_HTON((hdr).hdr_base) \
        (hdr).hdr_origin = htonl((hdr).hdr_origin); \
        (hdr).hdr_origin_tag = htonl((hdr).hdr_origin_tag); \
        (hdr).hdr_target_disp = hton64((hdr).hdr_target_disp); \
        (hdr).hdr_target_count = htonl((hdr).hdr_target_count); \
        (hdr).hdr_target_op = htonl((hdr).hdr_target_op); \
        (hdr).hdr_msg_length = htonl((hdr).hdr_msg_length); \
    } while (0)

#define OMPI_OSC_PT2PT_SEND_HDR_NTOH(hdr) \
    do { \
        OMPI_OSC_PT2PT_BASE_HDR_NTOH((hdr).hdr_base) \
        (hdr).hdr_origin = ntohl((hdr).hdr_origin); \
        (hdr).hdr_origin_tag = ntohl((hdr).hdr_origin_tag); \
        (hdr).hdr_target_disp = ntoh64((hdr).hdr_target_disp); \
        (hdr).hdr_target_count = ntohl((hdr).hdr_target_count); \
        (hdr).hdr_target_op = ntohl((hdr).hdr_target_op); \
        (hdr).hdr_msg_length = ntohl((hdr).hdr_msg_length); \
    } while (0)


struct ompi_osc_pt2pt_reply_header_t {
    ompi_osc_pt2pt_base_header_t hdr_base;
    int32_t hdr_target_tag;
    ompi_ptr_t hdr_origin_sendreq;
    int32_t hdr_msg_length;
};
typedef struct ompi_osc_pt2pt_reply_header_t ompi_osc_pt2pt_reply_header_t;

#define OMPI_OSC_PT2PT_REPLY_HDR_HTON(hdr) \
    do { \
        OMPI_OSC_PT2PT_BASE_HDR_HTON((hdr).hdr_base) \
        (hdr).hdr_target_tag = htonl((hdr).hdr_target_tag); \
        (hdr).hdr_msg_length = htonl((hdr).hdr_msg_length); \
    } while (0)

#define OMPI_OSC_PT2PT_REPLY_HDR_NTOH(hdr) \
    do { \
        OMPI_OSC_PT2PT_BASE_HDR_NTOH((hdr).hdr_base) \
        (hdr).hdr_target_tag = ntohl((hdr).hdr_target_tag); \
        (hdr).hdr_msg_length = ntohl((hdr).hdr_msg_length); \
    } while (0)


struct ompi_osc_pt2pt_control_header_t {
    ompi_osc_pt2pt_base_header_t hdr_base;
    int32_t hdr_value[2];
};
typedef struct ompi_osc_pt2pt_control_header_t ompi_osc_pt2pt_control_header_t;

#define OMPI_OSC_PT2PT_CONTROL_HDR_HTON(hdr) \
    do { \
        OMPI_OSC_PT2PT_BASE_HDR_HTON((hdr).hdr_base) \
        (hdr).hdr_value[0] = htonl((hdr).hdr_value[0]); \
        (hdr).hdr_value[1] = htonl((hdr).hdr_value[1]); \
    } while (0)

#define OMPI_OSC_PT2PT_CONTROL_HDR_NTOH(hdr) \
    do { \
        OMPI_OSC_PT2PT_BASE_HDR_NTOH((hdr).hdr_base) \
        (hdr).hdr_value[0] = ntohl((hdr).hdr_value[0]); \
        (hdr).hdr_value[1] = ntohl((hdr).hdr_value[1]); \
    } while (0)

#endif /* OMPI_MCA_OSC_PT2PT_HDR_H */
