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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_MTL_PORTALS_RECV_SHORT_H
#define OMPI_MTL_PORTALS_RECV_SHORT_H

struct ompi_mtl_portals_recv_short_block_t {
    opal_list_item_t base;

    mca_mtl_portals_module_t *mtl;

    void *start;
    size_t length;
    ptl_handle_me_t me_h;
    ptl_handle_md_t md_h;

    volatile bool full;
    volatile int32_t pending;
};
typedef struct ompi_mtl_portals_recv_short_block_t ompi_mtl_portals_recv_short_block_t;
OBJ_CLASS_DECLARATION(ompi_mtl_portals_recv_short_block_t);

extern int
ompi_mtl_portals_recv_short_enable(mca_mtl_portals_module_t *mtl);

extern int
ompi_mtl_portals_recv_short_disable(mca_mtl_portals_module_t *mtl);

/**
 * Create a block of memory for receiving send messages.  Must call
 * activate_block on the returned block of memory before it will be
 * active with the POrtals library 
 *
 * Module lock must be held before calling this function
 */
extern ompi_mtl_portals_recv_short_block_t* 
ompi_mtl_portals_recv_short_block_init(mca_mtl_portals_module_t *mtl);


/**
 * Free a block of memory.  Will remove the match entry, then progress
 * Portals until the pending count is returned to 0.  Will then free
 * all resources associated with block.
 *
 * Module lock must be held before calling this function
 */
extern int
ompi_mtl_portals_recv_short_block_free(ompi_mtl_portals_recv_short_block_t *block);

/**
 * activate a block.  Blocks that are full (have gone inactive) can be
 * re-activated with this call.  There is no need to hold the lock
 * before calling this function
 */
static inline int
ompi_mtl_portals_activate_block(ompi_mtl_portals_recv_short_block_t *block)
{
    int ret;
    ptl_process_id_t any_proc = { PTL_NID_ANY, PTL_PID_ANY };
    ptl_md_t md;
    uint64_t match_bits = PTL_SHORT_MSG;
    uint64_t ignore_bits = PTL_CONTEXT_MASK | PTL_SOURCE_MASK | PTL_TAG_MASK;

    /* if we have pending operations, something very, very, very bad
       has happened... */
    assert(block->pending == 0);

    if (NULL == block->start) return OMPI_ERROR;

    md.start = block->start;
    md.length = block->length;
    md.threshold = PTL_MD_THRESH_INF;
    md.max_size = block->mtl->eager_limit;
    md.options = PTL_MD_OP_PUT | PTL_MD_MAX_SIZE | PTL_MD_ACK_DISABLE;
    md.user_ptr = block;
    md.eq_handle = block->mtl->ptl_unex_eq_h;

    block->pending = 0;
    block->full = false;

    /* make sure that everyone sees the update on full value */
    opal_atomic_mb();

    ret = PtlMEMDPost(ompi_mtl_portals.ptl_ni_h,
		      ompi_mtl_portals.ptl_send_catchall_me_h,
		      any_proc,
		      match_bits,
		      ignore_bits,
		      PTL_UNLINK,
		      PTL_INS_BEFORE,
		      md,
		      PTL_UNLINK,
		      &(block->me_h),
		      &(block->md_h),
		      ompi_mtl_portals.ptl_empty_eq_h);
    if (PTL_OK != ret) return OMPI_ERROR;

    return OMPI_SUCCESS;
}


static inline void
ompi_mtl_portals_return_block_part(mca_mtl_portals_module_t *mtl,
                                  ompi_mtl_portals_recv_short_block_t *block)
{
    int ret;

    OPAL_THREAD_ADD32(&(block->pending), -1);
    if (block->full == true) {
        if (block->pending == 0) {
            ret = ompi_mtl_portals_activate_block(block);
            if (OMPI_SUCCESS != ret) {
                /* BWB - now what? */
            }
        }
    }    
}

#endif /* OMPI_MTL_PORTALS_RECV_SHORT_H */
