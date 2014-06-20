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
 * Copyright (c) 2009      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PML_OB1_HEADER_H
#define MCA_PML_OB1_HEADER_H

#include "ompi_config.h"
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "opal/types.h"
#include "opal/util/arch.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/proc/proc.h"

#define MCA_PML_OB1_HDR_TYPE_MATCH     (MCA_BTL_TAG_PML + 1)
#define MCA_PML_OB1_HDR_TYPE_RNDV      (MCA_BTL_TAG_PML + 2)
#define MCA_PML_OB1_HDR_TYPE_RGET      (MCA_BTL_TAG_PML + 3)
#define MCA_PML_OB1_HDR_TYPE_ACK       (MCA_BTL_TAG_PML + 4)
#define MCA_PML_OB1_HDR_TYPE_NACK      (MCA_BTL_TAG_PML + 5)
#define MCA_PML_OB1_HDR_TYPE_FRAG      (MCA_BTL_TAG_PML + 6)
#define MCA_PML_OB1_HDR_TYPE_GET       (MCA_BTL_TAG_PML + 7)
#define MCA_PML_OB1_HDR_TYPE_PUT       (MCA_BTL_TAG_PML + 8)
#define MCA_PML_OB1_HDR_TYPE_FIN       (MCA_BTL_TAG_PML + 9)

#define MCA_PML_OB1_HDR_FLAGS_ACK     1  /* is an ack required */
#define MCA_PML_OB1_HDR_FLAGS_NBO     2  /* is the hdr in network byte order */
#define MCA_PML_OB1_HDR_FLAGS_PIN     4  /* is user buffer pinned */
#define MCA_PML_OB1_HDR_FLAGS_CONTIG  8  /* is user buffer contiguous */
#define MCA_PML_OB1_HDR_FLAGS_NORDMA  16 /* rest will be send by copy-in-out */

/**
 * Common hdr attributes - must be first element in each hdr type 
 */
struct mca_pml_ob1_common_hdr_t {
    uint8_t hdr_type;  /**< type of envelope */
    uint8_t hdr_flags; /**< flags indicating how fragment should be processed */
};
typedef struct mca_pml_ob1_common_hdr_t mca_pml_ob1_common_hdr_t;

#define MCA_PML_OB1_COMMON_HDR_NTOH(h) 
#define MCA_PML_OB1_COMMON_HDR_HTON(h) 

/**
 *  Header definition for the first fragment, contains the 
 *  attributes required to match the corresponding posted receive.
 */
struct mca_pml_ob1_match_hdr_t {
    mca_pml_ob1_common_hdr_t hdr_common;   /**< common attributes */
    uint16_t hdr_ctx;                      /**< communicator index */
    int32_t  hdr_src;                      /**< source rank */
    int32_t  hdr_tag;                      /**< user tag */
    uint16_t hdr_seq;                      /**< message sequence number */
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    uint8_t  hdr_padding[2];               /**< explicitly pad to 16 bytes.  Compilers seem to already prefer to do this, but make it explicit just in case */
#endif
};
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
#define OMPI_PML_OB1_MATCH_HDR_LEN  16
#else
#define OMPI_PML_OB1_MATCH_HDR_LEN  14
#endif 

typedef struct mca_pml_ob1_match_hdr_t mca_pml_ob1_match_hdr_t;

#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT && OPAL_ENABLE_DEBUG
#define MCA_PML_OB1_MATCH_HDR_FILL(h) \
do {                                  \
    (h).hdr_padding[0] = 0;           \
    (h).hdr_padding[1] = 0;           \
} while(0)
#else
#define MCA_PML_OB1_MATCH_HDR_FILL(h)
#endif  /* OPAL_ENABLE_HETEROGENEOUS_SUPPORT && OPAL_ENABLE_DEBUG */

#define MCA_PML_OB1_MATCH_HDR_NTOH(h) \
do { \
    MCA_PML_OB1_COMMON_HDR_NTOH((h).hdr_common); \
    (h).hdr_ctx = ntohs((h).hdr_ctx); \
    (h).hdr_src = ntohl((h).hdr_src); \
    (h).hdr_tag = ntohl((h).hdr_tag); \
    (h).hdr_seq = ntohs((h).hdr_seq); \
} while (0)

#define MCA_PML_OB1_MATCH_HDR_HTON(h) \
do { \
    MCA_PML_OB1_COMMON_HDR_HTON((h).hdr_common); \
    MCA_PML_OB1_MATCH_HDR_FILL(h);    \
    (h).hdr_ctx = htons((h).hdr_ctx); \
    (h).hdr_src = htonl((h).hdr_src); \
    (h).hdr_tag = htonl((h).hdr_tag); \
    (h).hdr_seq = htons((h).hdr_seq); \
} while (0) 

/**
 * Header definition for the first fragment when an acknowledgment
 * is required. This could be the first fragment of a large message
 * or a short message that requires an ack (synchronous).
 */
struct mca_pml_ob1_rendezvous_hdr_t {
    mca_pml_ob1_match_hdr_t hdr_match;
    uint64_t hdr_msg_length;            /**< message length */
    ompi_ptr_t hdr_src_req;             /**< pointer to source request - returned in ack */
};
typedef struct mca_pml_ob1_rendezvous_hdr_t mca_pml_ob1_rendezvous_hdr_t;

#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT && OPAL_ENABLE_DEBUG
#define MCA_PML_OB1_RNDV_HDR_FILL(h) \
    MCA_PML_OB1_MATCH_HDR_FILL((h).hdr_match)
#else
#define MCA_PML_OB1_RNDV_HDR_FILL(h)
#endif  /* OPAL_ENABLE_HETEROGENEOUS_SUPPORT && OPAL_ENABLE_DEBUG */

/* Note that hdr_src_req is not put in network byte order because it
   is never processed by the receiver, other than being copied into
   the ack header */
#define MCA_PML_OB1_RNDV_HDR_NTOH(h) \
    do { \
        MCA_PML_OB1_MATCH_HDR_NTOH((h).hdr_match); \
        (h).hdr_msg_length = ntoh64((h).hdr_msg_length); \
    } while (0)

#define MCA_PML_OB1_RNDV_HDR_HTON(h) \
    do { \
        MCA_PML_OB1_MATCH_HDR_HTON((h).hdr_match); \
        MCA_PML_OB1_RNDV_HDR_FILL(h); \
        (h).hdr_msg_length = hton64((h).hdr_msg_length); \
    } while (0) 

/**
 * Header definition for a combined rdma rendezvous/get
 */
struct mca_pml_ob1_rget_hdr_t {
    mca_pml_ob1_rendezvous_hdr_t hdr_rndv;
    uint32_t hdr_seg_cnt;                     /**< number of segments for rdma */
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    uint8_t hdr_padding[4];
#endif
    ompi_ptr_t hdr_des;                       /**< source descriptor */
    mca_btl_base_segment_t hdr_segs[1];       /**< list of segments for rdma */
};
typedef struct mca_pml_ob1_rget_hdr_t mca_pml_ob1_rget_hdr_t;

#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT && OPAL_ENABLE_DEBUG
#define MCA_PML_OB1_RGET_HDR_FILL(h)         \
do {                                         \
    MCA_PML_OB1_RNDV_HDR_FILL((h).hdr_rndv); \
    (h).hdr_padding[0] = 0;                  \
    (h).hdr_padding[1] = 0;                  \
    (h).hdr_padding[2] = 0;                  \
    (h).hdr_padding[3] = 0;                  \
} while(0)
#else
#define MCA_PML_OB1_RGET_HDR_FILL(h)
#endif  /* OPAL_ENABLE_HETEROGENEOUS_SUPPORT && OPAL_ENABLE_DEBUG */

#define MCA_PML_OB1_RGET_HDR_NTOH(h) \
    do { \
       MCA_PML_OB1_RNDV_HDR_NTOH((h).hdr_rndv); \
        (h).hdr_seg_cnt = ntohl((h).hdr_seg_cnt); \
    } while (0)

#define MCA_PML_OB1_RGET_HDR_HTON(h) \
    do { \
        MCA_PML_OB1_RNDV_HDR_HTON((h).hdr_rndv); \
        MCA_PML_OB1_RGET_HDR_FILL(h); \
        (h).hdr_seg_cnt = htonl((h).hdr_seg_cnt); \
    } while (0) 

/**
 *  Header for subsequent fragments.
 */
struct mca_pml_ob1_frag_hdr_t {
    mca_pml_ob1_common_hdr_t hdr_common;     /**< common attributes */
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    uint8_t hdr_padding[6];
#endif
    uint64_t hdr_frag_offset;                /**< offset into message */
    ompi_ptr_t hdr_src_req;                  /**< pointer to source request */
    ompi_ptr_t hdr_dst_req;                  /**< pointer to matched receive */
};
typedef struct mca_pml_ob1_frag_hdr_t mca_pml_ob1_frag_hdr_t;

#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT && OPAL_ENABLE_DEBUG
#define MCA_PML_OB1_FRAG_HDR_FILL(h) \
do {                                 \
  (h).hdr_padding[0] = 0;            \
  (h).hdr_padding[1] = 0;            \
  (h).hdr_padding[2] = 0;            \
  (h).hdr_padding[3] = 0;            \
  (h).hdr_padding[4] = 0;            \
  (h).hdr_padding[5] = 0;            \
} while(0)
#else
#define MCA_PML_OB1_FRAG_HDR_FILL(h)
#endif  /* OPAL_ENABLE_HETEROGENEOUS_SUPPORT && OPAL_ENABLE_DEBUG */

#define MCA_PML_OB1_FRAG_HDR_NTOH(h) \
    do { \
        MCA_PML_OB1_COMMON_HDR_NTOH((h).hdr_common); \
        (h).hdr_frag_offset = ntoh64((h).hdr_frag_offset); \
    } while (0)

#define MCA_PML_OB1_FRAG_HDR_HTON(h) \
    do { \
        MCA_PML_OB1_COMMON_HDR_HTON((h).hdr_common); \
        MCA_PML_OB1_FRAG_HDR_FILL(h); \
        (h).hdr_frag_offset = hton64((h).hdr_frag_offset); \
    } while (0)

/**
 *  Header used to acknowledgment outstanding fragment(s).
 */

struct mca_pml_ob1_ack_hdr_t {
    mca_pml_ob1_common_hdr_t hdr_common;      /**< common attributes */
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    uint8_t hdr_padding[6];
#endif
    ompi_ptr_t hdr_src_req;                   /**< source request */
    ompi_ptr_t hdr_dst_req;                   /**< matched receive request */
    uint64_t hdr_send_offset;                 /**< starting point of copy in/out */
};
typedef struct mca_pml_ob1_ack_hdr_t mca_pml_ob1_ack_hdr_t;

#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT && OPAL_ENABLE_DEBUG
#define MCA_PML_OB1_ACK_HDR_FILL(h) \
do {                                \
    (h).hdr_padding[0] = 0;         \
    (h).hdr_padding[1] = 0;         \
    (h).hdr_padding[2] = 0;         \
    (h).hdr_padding[3] = 0;         \
    (h).hdr_padding[4] = 0;         \
    (h).hdr_padding[5] = 0;         \
} while (0)
#else
#define MCA_PML_OB1_ACK_HDR_FILL(h)
#endif  /* OPAL_ENABLE_HETEROGENEOUS_SUPPORT && OPAL_ENABLE_DEBUG */

/* Note that the request headers are not put in NBO because the
   src_req is already in receiver's byte order and the dst_req is not
   used by the receiver for anything other than backpointers in return
   headers */
#define MCA_PML_OB1_ACK_HDR_NTOH(h) \
    do { \
        MCA_PML_OB1_COMMON_HDR_NTOH((h).hdr_common); \
        (h).hdr_send_offset = ntoh64((h).hdr_send_offset); \
    } while (0)

#define MCA_PML_OB1_ACK_HDR_HTON(h) \
    do { \
        MCA_PML_OB1_COMMON_HDR_HTON((h).hdr_common); \
        MCA_PML_OB1_ACK_HDR_FILL(h); \
        (h).hdr_send_offset = hton64((h).hdr_send_offset); \
    } while (0) 

/**
 *  Header used to initiate an RDMA operation.
 */

struct mca_pml_ob1_rdma_hdr_t {
    mca_pml_ob1_common_hdr_t hdr_common;      /**< common attributes */
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    uint8_t hdr_padding[2];                   /** two to pad out the hdr to a 4 byte alignment.  hdr_req will then be 8 byte aligned after 4 for hdr_seg_cnt */
#endif
    uint32_t hdr_seg_cnt;                     /**< number of segments for rdma */
    ompi_ptr_t hdr_req;                       /**< destination request */
    ompi_ptr_t hdr_des;                       /**< source descriptor */
    ompi_ptr_t hdr_recv_req;                  /**< receive request */
    uint64_t hdr_rdma_offset;                 /**< current offset into user buffer */ 
    mca_btl_base_segment_t hdr_segs[1];       /**< list of segments for rdma */
};
typedef struct mca_pml_ob1_rdma_hdr_t mca_pml_ob1_rdma_hdr_t;

#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT && OPAL_ENABLE_DEBUG
#define MCA_PML_OB1_RDMA_HDR_FILL(h) \
do {                                 \
    (h).hdr_padding[0] = 0;          \
    (h).hdr_padding[1] = 0;          \
} while(0)
#else
#define MCA_PML_OB1_RDMA_HDR_FILL(h)
#endif  /* OPAL_ENABLE_HETEROGENEOUS_SUPPORT && OPAL_ENABLE_DEBUG */

#define MCA_PML_OB1_RDMA_HDR_NTOH(h) \
    do { \
        MCA_PML_OB1_COMMON_HDR_NTOH((h).hdr_common); \
        (h).hdr_seg_cnt = ntohl((h).hdr_seg_cnt); \
        (h).hdr_rdma_offset = ntoh64((h).hdr_rdma_offset); \
    } while (0)

#define MCA_PML_OB1_RDMA_HDR_HTON(h) \
    do { \
        MCA_PML_OB1_COMMON_HDR_HTON((h).hdr_common); \
        MCA_PML_OB1_RDMA_HDR_FILL(h); \
        (h).hdr_seg_cnt = htonl((h).hdr_seg_cnt); \
        (h).hdr_rdma_offset = hton64((h).hdr_rdma_offset); \
    } while (0) 

/**
 *  Header used to complete an RDMA operation.
 */

struct mca_pml_ob1_fin_hdr_t {
    mca_pml_ob1_common_hdr_t hdr_common;      /**< common attributes */
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    uint8_t hdr_padding[2];
#endif
    uint32_t hdr_fail;                        /**< RDMA operation failed */
    ompi_ptr_t hdr_des;                       /**< completed descriptor */
};
typedef struct mca_pml_ob1_fin_hdr_t mca_pml_ob1_fin_hdr_t;

#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT && OPAL_ENABLE_DEBUG
#define MCA_PML_OB1_FIN_HDR_FILL(h) \
do {                                \
    (h).hdr_padding[0] = 0;         \
    (h).hdr_padding[1] = 0;         \
} while (0)
#else
#define MCA_PML_OB1_FIN_HDR_FILL(h)
#endif  /* OPAL_ENABLE_HETEROGENEOUS_SUPPORT && OPAL_ENABLE_DEBUG */

#define MCA_PML_OB1_FIN_HDR_NTOH(h) \
    do { \
        MCA_PML_OB1_COMMON_HDR_NTOH((h).hdr_common); \
    } while (0)

#define MCA_PML_OB1_FIN_HDR_HTON(h) \
    do { \
        MCA_PML_OB1_COMMON_HDR_HTON((h).hdr_common); \
        MCA_PML_OB1_FIN_HDR_FILL(h); \
    } while (0) 

/**
 * Union of defined hdr types.
 */
union mca_pml_ob1_hdr_t {
    mca_pml_ob1_common_hdr_t hdr_common;
    mca_pml_ob1_match_hdr_t hdr_match;
    mca_pml_ob1_rendezvous_hdr_t hdr_rndv;
    mca_pml_ob1_rget_hdr_t hdr_rget;
    mca_pml_ob1_frag_hdr_t hdr_frag;
    mca_pml_ob1_ack_hdr_t hdr_ack;
    mca_pml_ob1_rdma_hdr_t hdr_rdma;
    mca_pml_ob1_fin_hdr_t hdr_fin;
};
typedef union mca_pml_ob1_hdr_t mca_pml_ob1_hdr_t;

#if !defined(WORDS_BIGENDIAN) && OPAL_ENABLE_HETEROGENEOUS_SUPPORT
static inline __opal_attribute_always_inline__ void
ob1_hdr_ntoh(mca_pml_ob1_hdr_t *hdr, const uint8_t hdr_type)
{
    if(!(hdr->hdr_common.hdr_flags & MCA_PML_OB1_HDR_FLAGS_NBO))
        return;

    switch(hdr_type) {
        case MCA_PML_OB1_HDR_TYPE_MATCH:
            MCA_PML_OB1_MATCH_HDR_NTOH(hdr->hdr_match);
            break;
        case MCA_PML_OB1_HDR_TYPE_RNDV:
            MCA_PML_OB1_RNDV_HDR_NTOH(hdr->hdr_rndv);
            break;
        case MCA_PML_OB1_HDR_TYPE_RGET:
            MCA_PML_OB1_RGET_HDR_NTOH(hdr->hdr_rget);
            break;
        case MCA_PML_OB1_HDR_TYPE_ACK:
            MCA_PML_OB1_ACK_HDR_NTOH(hdr->hdr_ack);
            break;
        case MCA_PML_OB1_HDR_TYPE_FRAG:
            MCA_PML_OB1_FRAG_HDR_NTOH(hdr->hdr_frag);
            break;
        case MCA_PML_OB1_HDR_TYPE_PUT:
            MCA_PML_OB1_RDMA_HDR_NTOH(hdr->hdr_rdma);
            break;
        case MCA_PML_OB1_HDR_TYPE_FIN:
            MCA_PML_OB1_FIN_HDR_NTOH(hdr->hdr_fin);
            break;
        default:
            assert(0);
            break;
    }
}
#else
#define ob1_hdr_ntoh(h, t) do{}while(0)
#endif

#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
#define ob1_hdr_hton(h, t, p) \
    ob1_hdr_hton_intr((mca_pml_ob1_hdr_t*)h, t, p)
static inline __opal_attribute_always_inline__ void
ob1_hdr_hton_intr(mca_pml_ob1_hdr_t *hdr, const uint8_t hdr_type,
        const ompi_proc_t *proc)
{
#ifdef WORDS_BIGENDIAN
    hdr->hdr_common.hdr_flags |= MCA_PML_OB1_HDR_FLAGS_NBO;
#else

    if(!(proc->proc_arch & OPAL_ARCH_ISBIGENDIAN))
        return;

    hdr->hdr_common.hdr_flags |= MCA_PML_OB1_HDR_FLAGS_NBO;
    switch(hdr_type) {
        case MCA_PML_OB1_HDR_TYPE_MATCH:
            MCA_PML_OB1_MATCH_HDR_HTON(hdr->hdr_match);
            break;
        case MCA_PML_OB1_HDR_TYPE_RNDV:
            MCA_PML_OB1_RNDV_HDR_HTON(hdr->hdr_rndv);
            break;
        case MCA_PML_OB1_HDR_TYPE_RGET:
            MCA_PML_OB1_RGET_HDR_HTON(hdr->hdr_rget);
            break;
        case MCA_PML_OB1_HDR_TYPE_ACK:
            MCA_PML_OB1_ACK_HDR_HTON(hdr->hdr_ack);
            break;
        case MCA_PML_OB1_HDR_TYPE_FRAG:
            MCA_PML_OB1_FRAG_HDR_HTON(hdr->hdr_frag);
            break;
        case MCA_PML_OB1_HDR_TYPE_PUT:
            MCA_PML_OB1_RDMA_HDR_HTON(hdr->hdr_rdma);
            break;
        case MCA_PML_OB1_HDR_TYPE_FIN:
            MCA_PML_OB1_FIN_HDR_HTON(hdr->hdr_fin);
            break;
        default:
            assert(0);
            break;
    }
#endif
}
#else
#define ob1_hdr_hton(h, t, p) do{}while(0)
#endif
#endif
