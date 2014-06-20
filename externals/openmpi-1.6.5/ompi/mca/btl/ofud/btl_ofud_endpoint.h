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
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BTL_IB_ENDPOINT_H
#define MCA_BTL_IB_ENDPOINT_H

#include <infiniband/verbs.h>

#include "opal/class/opal_list.h"
#include "opal/event/event.h"

#include "btl_ofud.h"
#include "btl_ofud_frag.h"

BEGIN_C_DECLS

struct mca_btl_ud_addr_t {
    uint32_t qp_num;
    uint32_t psn;
    uint16_t lid;
    uint16_t subnet;
};
typedef struct mca_btl_ud_addr_t mca_btl_ud_addr_t;


/**
 * An abstraction that represents a connection to a endpoint process.
 * An instance of mca_btl_base_endpoint_t is associated w/ each process
 * and BTL pair and address information is exchanged at startup.
 * The UD BTL is connectionless, so no connection is ever established.
 */

struct mca_btl_base_endpoint_t {
    opal_list_item_t            super;

    mca_btl_ud_addr_t           rem_addr;
    /**< Remote address information */
    /* No lock needed, read-only past initialization */

    struct ibv_ah*              rmt_ah;
    /**< Remote address handle */
    /* No lock needed, verbs are thread-safe */
};

typedef struct mca_btl_base_endpoint_t mca_btl_base_endpoint_t;
typedef mca_btl_base_endpoint_t  mca_btl_ud_endpoint_t;
OBJ_CLASS_DECLARATION(mca_btl_ud_endpoint_t);

int mca_btl_ud_endpoint_post_send(struct mca_btl_ud_module_t* ud_btl,
                                  struct mca_btl_ud_frag_t * frag);

END_C_DECLS
#endif
