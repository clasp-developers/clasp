/*
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BTL_OPENIB_EAGER_RDMA_BUF_H
#define MCA_BTL_OPENIB_EAGER_RDMA_BUF_H

#include "ompi_config.h"
#include "btl_openib.h"

BEGIN_C_DECLS

struct mca_btl_openib_eager_rdma_local_t {
    ompi_ptr_t base; /**< buffer for RDMAing eager messages */
    mca_btl_openib_recv_frag_t *frags;
    mca_btl_openib_reg_t *reg;
    uint16_t head; /**< RDMA buffer to poll */
    uint16_t tail; /**< Needed for credit managment */
    int32_t credits; /**< number of RDMA credits */
    int32_t rd_win;
#if OPAL_ENABLE_DEBUG
    uint32_t seq;
#endif
    opal_mutex_t lock; /**< guard access to RDMA buffer */
    int32_t rd_low;
};
typedef struct mca_btl_openib_eager_rdma_local_t mca_btl_openib_eager_rdma_local_t;

struct mca_btl_openib_eager_rdma_remote_t {
	ompi_ptr_t base; /**< address of remote buffer */
	uint32_t rkey; /**< RKey for accessing remote buffer */
	int32_t head; /**< RDMA buffer to post to */
	int32_t tokens; /**< number of rdam tokens */
#if OPAL_ENABLE_DEBUG
    uint32_t seq;
#endif
};
typedef struct mca_btl_openib_eager_rdma_remote_t mca_btl_openib_eager_rdma_remote_t;

#define MCA_BTL_OPENIB_RDMA_FRAG(F) \
    (openib_frag_type(F) == MCA_BTL_OPENIB_FRAG_EAGER_RDMA)

#define EAGER_RDMA_BUFFER_REMOTE (0)
#define EAGER_RDMA_BUFFER_LOCAL (0xff)

#ifdef WORDS_BIGENDIAN
#define MCA_BTL_OPENIB_RDMA_FRAG_GET_SIZE(F) ((F)->u.size >> 8)
#define MCA_BTL_OPENIB_RDMA_FRAG_SET_SIZE(F, S) \
                                       ((F)->u.size = (S) << 8)
#else
#define MCA_BTL_OPENIB_RDMA_FRAG_GET_SIZE(F) ((F)->u.size & 0x00ffffff)
#define MCA_BTL_OPENIB_RDMA_FRAG_SET_SIZE(F, S) \
                                       ((F)->u.size = (S) & 0x00ffffff)
#endif

#define MCA_BTL_OPENIB_RDMA_FRAG_LOCAL(F)              \
                        (((volatile uint8_t*)(F)->ftr->u.buf)[3] != EAGER_RDMA_BUFFER_REMOTE)

#define MCA_BTL_OPENIB_RDMA_FRAG_REMOTE(F) \
                        (!MCA_BTL_OPENIB_RDMA_FRAG_LOCAL(F))

#define MCA_BTL_OPENIB_RDMA_MAKE_REMOTE(F) do {                        \
                             ((volatile uint8_t*)(F)->u.buf)[3] = EAGER_RDMA_BUFFER_REMOTE; \
                            }while (0)

#define MCA_BTL_OPENIB_RDMA_MAKE_LOCAL(F) do {                        \
                             ((volatile uint8_t*)(F)->u.buf)[3] = EAGER_RDMA_BUFFER_LOCAL; \
                            }while (0)

#define MCA_BTL_OPENIB_GET_LOCAL_RDMA_FRAG(E, I)                            \
                            (&(E)->eager_rdma_local.frags[(I)])

#define MCA_BTL_OPENIB_RDMA_NEXT_INDEX(I) do {                              \
                            (I) = ((I) + 1);                                \
                            if((I) ==                                       \
                                   mca_btl_openib_component.eager_rdma_num) \
                                (I) = 0;                                    \
                        } while (0)
#define MCA_BTL_OPENIB_RDMA_MOVE_INDEX(HEAD, OLD_HEAD)                      \
    do {                                                                    \
        int32_t new_head;                                                   \
        do {                                                                \
            OLD_HEAD = HEAD;                                                \
            new_head = OLD_HEAD + 1;                                        \
            if(new_head == mca_btl_openib_component.eager_rdma_num)         \
                new_head = 0;                                               \
        } while(!OPAL_ATOMIC_CMPSET_32(&HEAD, OLD_HEAD, new_head));         \
    } while(0)


END_C_DECLS
#endif

