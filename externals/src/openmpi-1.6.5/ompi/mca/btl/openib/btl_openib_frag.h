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
 * Copyright (c) 2009      IBM Corporation.  All rights reserved.
 * Copyright (c) 2006-2009 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 * Copyright (c) 2010-2012 Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BTL_IB_FRAG_H
#define MCA_BTL_IB_FRAG_H

#include "ompi_config.h"
#include "opal/align.h"

#include <infiniband/verbs.h>
#include "ompi/mca/btl/btl.h"

BEGIN_C_DECLS

struct mca_btl_openib_reg_t;

struct mca_btl_openib_header_t {
    mca_btl_base_tag_t tag;
    uint8_t cm_seen;
    uint16_t credits;
#if OMPI_OPENIB_PAD_HDR 
    uint8_t padding[4];
#endif
};
typedef struct mca_btl_openib_header_t mca_btl_openib_header_t;
#define BTL_OPENIB_RDMA_CREDITS_FLAG (1<<15)
#define BTL_OPENIB_IS_RDMA_CREDITS(I) ((I)&BTL_OPENIB_RDMA_CREDITS_FLAG)
#define BTL_OPENIB_CREDITS(I) ((I)&~BTL_OPENIB_RDMA_CREDITS_FLAG)

#define BTL_OPENIB_HEADER_HTON(h)     \
do {                                  \
    (h).credits = htons((h).credits); \
} while (0)

#define BTL_OPENIB_HEADER_NTOH(h)     \
do {                                  \
    (h).credits = ntohs((h).credits); \
} while (0)

typedef struct mca_btl_openib_header_coalesced_t {
    mca_btl_base_tag_t tag;
    uint32_t size;
    uint32_t alloc_size;
#if OMPI_OPENIB_PAD_HDR
    uint8_t padding[4];
#endif
} mca_btl_openib_header_coalesced_t;

#define BTL_OPENIB_HEADER_COALESCED_NTOH(h)     \
    do {                                        \
        (h).size = ntohl((h).size);             \
        (h).alloc_size = ntohl((h).alloc_size); \
     } while(0)

#define BTL_OPENIB_HEADER_COALESCED_HTON(h)     \
    do {                                        \
        (h).size = htonl((h).size);             \
        (h).alloc_size = htonl((h).alloc_size); \
     } while(0)

#if OMPI_OPENIB_PAD_HDR
/* BTL_OPENIB_FTR_PADDING
 * This macro is used to keep the pointer to openib
 * footers aligned for systems like SPARC64 that
 * take a big performance hit when addresses are not aligned
 * (and by default sigbus instead of coercing the type on 
 * an unaligned address).
 *
 * We assure alignment of a packet's structures when OMPI_OPENIB_PAD_HDR 
 * is set to 1.  When this is the case then several structures are padded
 * to assure alignment and the mca_btl_openib_footer_t structure itself
 * will uses the BTL_OPENIB_FTR_PADDING macro to shift the location of the 
 * pointer to assure proper alignment after the PML Header and data.
 * For example sending a 1 byte data packet the memory layout without 
 * footer alignment would look something like the following:
 * 
 * 0x00   : mca_btl_openib_coalesced_header_t (12 bytes + 4 byte pad)
 * 0x10   : mca_btl_openib_control_header_t (1 byte + 7 byte pad)
 * 0x18   : mca_btl_openib_header_t (4 bytes + 4 byte pad)
 * 0x20   : PML Header and data (16 bytes PML + 1 byte data)
 * 0x29   : mca_btl_openib_footer_t (4 bytes + 4 byte pad)
 * 0x31   : end of packet
 *
 * By applying the BTL_OPENIB_FTR_PADDING() in the progress_one_device
 * and post_send routines we adjust the pointer to mca_btl_openib_footer_t 
 * from 0x29 to 0x2C thus correctly aligning the start of the 
 * footer pointer.  This adjustment will cause the padding field of  
 * mca_btl_openib_footer_t to overlap with the neighboring memory but since
 * we never use the padding we do not end up inadvertently overwriting
 * memory that does not belong to the fragment.
 */
#define BTL_OPENIB_FTR_PADDING(size) \
    OPAL_ALIGN_PAD_AMOUNT(size, sizeof(uint64_t))

/* BTL_OPENIB_ALIGN_COALESCE_HDR
 * This macro is used in btl_openib.c, while creating a coalesce fragment, 
 * to align the coalesce headers.
 */
#define BTL_OPENIB_ALIGN_COALESCE_HDR(ptr) \
  OPAL_ALIGN_PTR(ptr, sizeof(uint32_t), unsigned char*)

/* BTL_OPENIB_COALESCE_HDR_PADDING
 * This macro is used in btl_openib_component.c, while parsing an incoming 
 * coalesce fragment, to determine the padding amount used to align the 
 * mca_btl_openib_coalesce_hdr_t.
 */
#define BTL_OPENIB_COALESCE_HDR_PADDING(ptr) \
  OPAL_ALIGN_PAD_AMOUNT(ptr, sizeof(uint32_t))
#else
#define BTL_OPENIB_FTR_PADDING(size) 0
#define BTL_OPENIB_ALIGN_COALESCE_HDR(ptr) ptr
#define BTL_OPENIB_COALESCE_HDR_PADDING(ptr) 0
#endif

struct mca_btl_openib_footer_t {
#if OPAL_ENABLE_DEBUG
    uint32_t seq;
#endif
    union {
        uint32_t size;
        uint8_t buf[4];
    } u;
#if OMPI_OPENIB_PAD_HDR
#if OPAL_ENABLE_DEBUG
    /* this footer needs to be of a 8-byte multiple so by adding the
     * seq field you throw this off and you cannot just remove the 
     * padding because the padding is needed in order to adjust the alignment
     * and not overwrite other packets.
     */
    uint8_t padding[12];
#else 
    uint8_t padding[8];
#endif
#endif
};
typedef struct mca_btl_openib_footer_t mca_btl_openib_footer_t;

#ifdef WORDS_BIGENDIAN
#define MCA_BTL_OPENIB_FTR_SIZE_REVERSE(ftr)
#else
#define MCA_BTL_OPENIB_FTR_SIZE_REVERSE(ftr)    \
    do {                                        \
        uint8_t tmp = (ftr).u.buf[0];           \
        (ftr).u.buf[0]=(ftr).u.buf[2];          \
        (ftr).u.buf[2]=tmp;                     \
    } while (0)
#endif

#if OPAL_ENABLE_DEBUG
#define BTL_OPENIB_FOOTER_SEQ_HTON(h)  ((h).seq = htonl((h).seq))
#define BTL_OPENIB_FOOTER_SEQ_NTOH(h)  ((h).seq = ntohl((h).seq))
#else
#define BTL_OPENIB_FOOTER_SEQ_HTON(h)
#define BTL_OPENIB_FOOTER_SEQ_NTOH(h)
#endif

#define BTL_OPENIB_FOOTER_HTON(h)               \
    do {                                        \
        BTL_OPENIB_FOOTER_SEQ_HTON(h);          \
        MCA_BTL_OPENIB_FTR_SIZE_REVERSE(h);     \
    } while (0)

#define BTL_OPENIB_FOOTER_NTOH(h)               \
    do {                                        \
        BTL_OPENIB_FOOTER_SEQ_NTOH(h);          \
        MCA_BTL_OPENIB_FTR_SIZE_REVERSE(h);     \
    } while (0)

#define MCA_BTL_OPENIB_CONTROL_CREDITS      0
#define MCA_BTL_OPENIB_CONTROL_RDMA         1
#define MCA_BTL_OPENIB_CONTROL_COALESCED    2
#define MCA_BTL_OPENIB_CONTROL_CTS          3
#if BTL_OPENIB_FAILOVER_ENABLED
#define MCA_BTL_OPENIB_CONTROL_EP_BROKEN    4
#define MCA_BTL_OPENIB_CONTROL_EP_EAGER_RDMA_ERROR 5
#endif

struct mca_btl_openib_control_header_t {
    uint8_t  type;
#if OMPI_OPENIB_PAD_HDR
    uint8_t  padding[7];
#endif
};
typedef struct mca_btl_openib_control_header_t mca_btl_openib_control_header_t;

struct mca_btl_openib_eager_rdma_header_t {
    mca_btl_openib_control_header_t control;
    uint32_t rkey;
    ompi_ptr_t rdma_start;
};
typedef struct mca_btl_openib_eager_rdma_header_t mca_btl_openib_eager_rdma_header_t;

#define BTL_OPENIB_EAGER_RDMA_CONTROL_HEADER_HTON(h)       \
    do {                                                   \
        (h).rkey = htonl((h).rkey);                        \
        (h).rdma_start.lval = hton64((h).rdma_start.lval); \
    } while (0)

#define BTL_OPENIB_EAGER_RDMA_CONTROL_HEADER_NTOH(h)         \
    do {                                                     \
        (h).rkey = ntohl((h).rkey);                          \
        (h).rdma_start.lval = ntoh64((h).rdma_start.lval);   \
    } while (0)


struct mca_btl_openib_rdma_credits_header_t {
    mca_btl_openib_control_header_t control;
#if OMPI_OPENIB_PAD_HDR
    uint8_t  padding[1];
#endif
    uint8_t qpn;
    uint16_t rdma_credits;
};
typedef struct mca_btl_openib_rdma_credits_header_t mca_btl_openib_rdma_credits_header_t;

#define BTL_OPENIB_RDMA_CREDITS_HEADER_HTON(h)     \
do {                                               \
    (h).rdma_credits = htons((h).rdma_credits);    \
} while (0)

#define BTL_OPENIB_RDMA_CREDITS_HEADER_NTOH(h)     \
do {                                               \
    (h).rdma_credits = ntohs((h).rdma_credits);    \
} while (0)

#if BTL_OPENIB_FAILOVER_ENABLED
struct mca_btl_openib_broken_connection_header_t {
    mca_btl_openib_control_header_t control;
    uint32_t lid;
    uint64_t subnet_id;
    uint32_t vpid;
    uint32_t index; /* for eager RDMA only */
};
typedef struct mca_btl_openib_broken_connection_header_t mca_btl_openib_broken_connection_header_t;

#define BTL_OPENIB_BROKEN_CONNECTION_HEADER_HTON(h)    \
    do {                                               \
        (h).lid = htonl((h).lid);                      \
        (h).subnet_id = hton64((h).subnet_id);         \
        (h).vpid = htonl((h).vpid);                    \
        (h).index = htonl((h).index);                  \
    } while (0)

#define BTL_OPENIB_BROKEN_CONNECTION_HEADER_NTOH(h)    \
    do {                                               \
        (h).lid = ntohl((h).lid);                      \
        (h).subnet_id = ntoh64((h).subnet_id);         \
        (h).vpid = ntohl((h).vpid);                    \
        (h).index = ntohl((h).index);                  \
    } while (0)
#endif
enum mca_btl_openib_frag_type_t {
    MCA_BTL_OPENIB_FRAG_RECV,
    MCA_BTL_OPENIB_FRAG_RECV_USER,
    MCA_BTL_OPENIB_FRAG_SEND,
    MCA_BTL_OPENIB_FRAG_SEND_USER,
    MCA_BTL_OPENIB_FRAG_EAGER_RDMA,
    MCA_BTL_OPENIB_FRAG_CONTROL,
    MCA_BTL_OPENIB_FRAG_COALESCED
};
typedef enum mca_btl_openib_frag_type_t mca_btl_openib_frag_type_t;

#define openib_frag_type(f) (to_base_frag(f)->type)
/**
 * IB fragment derived type.
 */

/* base openib frag */
typedef struct mca_btl_openib_frag_t {
    mca_btl_base_descriptor_t base;
    mca_btl_base_segment_t segment;
    mca_btl_openib_frag_type_t type;
    ompi_free_list_t* list;
} mca_btl_openib_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_openib_frag_t);

#define to_base_frag(f) ((mca_btl_openib_frag_t*)(f))

/* frag used for communication */
typedef struct mca_btl_openib_com_frag_t {
    mca_btl_openib_frag_t super;
    struct ibv_sge sg_entry;
    struct mca_btl_openib_reg_t *registration;
    struct mca_btl_base_endpoint_t *endpoint;
    /* number of unsignaled frags sent before this frag. */
    uint32_t n_wqes_inflight;
} mca_btl_openib_com_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_openib_com_frag_t);

#define to_com_frag(f) ((mca_btl_openib_com_frag_t*)(f))

typedef struct mca_btl_openib_out_frag_t {
    mca_btl_openib_com_frag_t super;
    struct ibv_send_wr sr_desc;
} mca_btl_openib_out_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_openib_out_frag_t);

#define to_out_frag(f) ((mca_btl_openib_out_frag_t*)(f))

typedef struct mca_btl_openib_com_frag_t mca_btl_openib_in_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_openib_in_frag_t);

#define to_in_frag(f) ((mca_btl_openib_in_frag_t*)(f))

typedef struct mca_btl_openib_send_frag_t {
    mca_btl_openib_out_frag_t super;
    mca_btl_openib_header_t *hdr, *chdr;
    mca_btl_openib_footer_t *ftr;
    uint8_t qp_idx;
    uint32_t coalesced_length;
    opal_list_t coalesced_frags;
} mca_btl_openib_send_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_openib_send_frag_t);

#define to_send_frag(f) ((mca_btl_openib_send_frag_t*)(f))

typedef struct mca_btl_openib_recv_frag_t {
    mca_btl_openib_in_frag_t super;
    mca_btl_openib_header_t *hdr;
    mca_btl_openib_footer_t *ftr;
    struct ibv_recv_wr rd_desc;
    uint8_t qp_idx;
} mca_btl_openib_recv_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_openib_recv_frag_t);

#define to_recv_frag(f) ((mca_btl_openib_recv_frag_t*)(f))

typedef struct mca_btl_openib_out_frag_t mca_btl_openib_put_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_openib_put_frag_t);

#define to_put_frag(f) ((mca_btl_openib_put_frag_t*)(f))

typedef struct mca_btl_openib_get_frag_t {
    mca_btl_openib_in_frag_t super;
    struct ibv_send_wr sr_desc;
} mca_btl_openib_get_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_openib_get_frag_t);

#define to_get_frag(f) ((mca_btl_openib_get_frag_t*)(f))

typedef struct mca_btl_openib_send_frag_t mca_btl_openib_send_control_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_openib_send_control_frag_t);

#define to_send_control_frag(f) ((mca_btl_openib_send_control_frag_t*)(f))

typedef struct mca_btl_openib_coalesced_frag_t {
    mca_btl_openib_frag_t super;
    mca_btl_openib_send_frag_t *send_frag;
    mca_btl_openib_header_coalesced_t *hdr;
} mca_btl_openib_coalesced_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_openib_coalesced_frag_t);

#define to_coalesced_frag(f) ((mca_btl_openib_coalesced_frag_t*)(f))

/*
 * Allocate an IB send descriptor
 *
 */

static inline mca_btl_openib_send_control_frag_t *
alloc_control_frag(mca_btl_openib_module_t *btl)
{
    int rc;
    ompi_free_list_item_t *item;

    OMPI_FREE_LIST_WAIT(&btl->device->send_free_control, item, rc);

    return to_send_control_frag(item);
}

static inline uint8_t frag_size_to_order(mca_btl_openib_module_t* btl,
        size_t size)
{
    int qp;
    for(qp = 0; qp < mca_btl_openib_component.num_qps; qp++)
        if(mca_btl_openib_component.qp_infos[qp].size >= size)
            return qp;

    return MCA_BTL_NO_ORDER;
}

static inline mca_btl_openib_com_frag_t *alloc_send_user_frag(void)
{
    int rc;
    ompi_free_list_item_t *item;

    OMPI_FREE_LIST_GET(&mca_btl_openib_component.send_user_free, item, rc);

    return to_com_frag(item);
}

static inline mca_btl_openib_com_frag_t *alloc_recv_user_frag(void)
{
    int rc;
    ompi_free_list_item_t *item;

    OMPI_FREE_LIST_GET(&mca_btl_openib_component.recv_user_free, item, rc);

    return to_com_frag(item);
}

static inline mca_btl_openib_coalesced_frag_t *alloc_coalesced_frag(void)
{
    int rc;
    ompi_free_list_item_t *item;

    OMPI_FREE_LIST_GET(&mca_btl_openib_component.send_free_coalesced, item, rc);

    return to_coalesced_frag(item);
}

#define MCA_BTL_IB_FRAG_RETURN(frag)                                    \
    do {                                                                \
        OMPI_FREE_LIST_RETURN(to_base_frag(frag)->list,                 \
                (ompi_free_list_item_t*)(frag));                        \
    } while(0);

#define MCA_BTL_OPENIB_CLEAN_PENDING_FRAGS(list)                         \
    while(!opal_list_is_empty(list)){                                    \
        opal_list_item_t *frag_item;                                     \
        frag_item = opal_list_remove_first(list);                        \
        MCA_BTL_IB_FRAG_RETURN(frag_item);                               \
    }                                                                    \

struct mca_btl_openib_module_t;

struct mca_btl_openib_frag_init_data_t {
    uint8_t order;
    ompi_free_list_t* list;
};
typedef struct mca_btl_openib_frag_init_data_t mca_btl_openib_frag_init_data_t;

void mca_btl_openib_frag_init(ompi_free_list_item_t* item, void* ctx);


END_C_DECLS
#endif
