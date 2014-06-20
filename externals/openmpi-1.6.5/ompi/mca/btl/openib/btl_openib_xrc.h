/*
 * Copyright (c) 2007-2008 Mellanox Technologies. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * @file
 */

#ifndef MCA_BTL_OPENIB_XRC_H
#define MCA_BTL_OPENIB_XRC_H
#include "btl_openib.h"
#include "btl_openib_endpoint.h"

#if HAVE_XRC
#define MCA_BTL_XRC_ENABLED (mca_btl_openib_component.num_xrc_qps)
#else
#define MCA_BTL_XRC_ENABLED 0
#endif

typedef enum {
    MCA_BTL_IB_ADDR_CONNECTING = 100,
    MCA_BTL_IB_ADDR_CONNECTED,
    MCA_BTL_IB_ADDR_CLOSED
} mca_btl_openib_ib_addr_state_t;

struct ib_address_t {
    opal_list_item_t super;
    void *key;                             /* the key with size 80bit - [subnet(64) LID(16bit)] */
    uint64_t subnet_id;                    /* caching subnet_id  */
    uint16_t lid;                          /* caching lid */
    opal_list_t pending_ep;                /* list of endpoints that use this ib_address */
    mca_btl_openib_qp_t *qp;               /* pointer to qp that will be used
                                              for communication with the
                                              destination */
    uint32_t remote_xrc_rcv_qp_num;        /* remote xrc qp number */
    opal_mutex_t addr_lock;                /* protection */
    mca_btl_openib_ib_addr_state_t status; /* ib port status */
};
typedef struct ib_address_t ib_address_t;

int mca_btl_openib_open_xrc_domain(struct mca_btl_openib_device_t *device);
int mca_btl_openib_close_xrc_domain(struct mca_btl_openib_device_t *device);
int mca_btl_openib_ib_address_add_new (uint16_t lid, uint64_t s_id,
        orte_jobid_t ep_jobid, mca_btl_openib_endpoint_t *ep);

#endif
