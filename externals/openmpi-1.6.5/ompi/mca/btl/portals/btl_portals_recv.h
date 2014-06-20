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

#ifndef OMPI_BTL_PORTALS_RECV_H
#define OMPI_BTL_PORTALS_RECV_H

#include "btl_portals_frag.h"

struct mca_btl_portals_recv_block_t {
    opal_list_item_t base;

    mca_btl_portals_module_t *btl;

    void *start;
    size_t length;
    ptl_handle_me_t me_h;
    ptl_handle_md_t md_h;

    volatile bool full;
    volatile int32_t pending;
};
typedef struct mca_btl_portals_recv_block_t mca_btl_portals_recv_block_t;
OBJ_CLASS_DECLARATION(mca_btl_portals_recv_block_t);


int mca_btl_portals_recv_enable(mca_btl_portals_module_t *btl);

int mca_btl_portals_recv_disable(mca_btl_portals_module_t *btl);

/**
 * Create a block of memory for receiving send messages.  Must call
 * activate_block on the returned block of memory before it will be
 * active with the POrtals library 
 *
 * Module lock must be held before calling this function
 */
mca_btl_portals_recv_block_t* 
mca_btl_portals_recv_block_init(mca_btl_portals_module_t *btl);


/**
 * Free a block of memory.  Will remove the match entry, then progress
 * Portals until the pending count is returned to 0.  Will then free
 * all resources associated with block.
 *
 * Module lock must be held before calling this function
 */
int mca_btl_portals_recv_block_free(mca_btl_portals_recv_block_t *block);


/**
 * activate a block.  Blocks that are full (have gone inactive) can be
 * re-activated with this call.  There is no need to hold the lock
 * before calling this function
 */
static inline int
mca_btl_portals_activate_block(mca_btl_portals_recv_block_t *block)
{
    int ret;
    ptl_process_id_t any_proc = { PTL_NID_ANY, PTL_PID_ANY };
    ptl_md_t md;
    uint64_t ignore_bits = ~((uint64_t) 0);

    /* if we have pending operations, something very, very, very bad
       has happened... */
    assert(block->pending == 0);

    if (NULL == block->start) return OMPI_ERROR;

    /* create match entry */
    ret = PtlMEInsert(block->btl->portals_recv_reject_me_h,
                      any_proc,
                      0, /* match bits */
                      ignore_bits, /* ignore bits */
                      PTL_UNLINK,
                      PTL_INS_BEFORE,
                      &(block->me_h));
    if (PTL_OK != ret) return OMPI_ERROR;

    /* and the memory descriptor */
    md.start = block->start;
    md.length = block->length;
    /* try to throttle incoming sends so that we don't overrun the incoming
       queue size */
    md.threshold = mca_btl_portals_module.portals_eq_sizes[OMPI_BTL_PORTALS_EQ_RECV] /
        (mca_btl_portals_module.portals_recv_mds_num * 2);
    md.max_size = block->btl->super.btl_max_send_size;
    md.options = PTL_MD_OP_PUT | PTL_MD_MAX_SIZE;
    md.user_ptr = block;
    md.eq_handle = block->btl->portals_eq_handles[OMPI_BTL_PORTALS_EQ_RECV];

    block->pending = 0;
    block->full = false;
    /* make sure that everyone sees the update on full value */
    opal_atomic_mb();

    ret = PtlMDAttach(block->me_h,
                      md,
                      PTL_UNLINK,
                      &(block->md_h));
    if (PTL_OK != ret) {
        PtlMEUnlink(block->me_h);
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


static inline void
mca_btl_portals_return_block_part(mca_btl_portals_module_t *btl,
                                  mca_btl_portals_recv_block_t *block)
{
    int ret;

    OPAL_THREAD_ADD32(&(block->pending), -1);
    if (block->full == true) {
        if (block->pending == 0) {
            ret = mca_btl_portals_activate_block(block);
            if (OMPI_SUCCESS != ret) {
                /* BWB - now what? */
            }
        }
    }    
}

#endif /* OMPI_BTL_PORTALS_RECV_H */
