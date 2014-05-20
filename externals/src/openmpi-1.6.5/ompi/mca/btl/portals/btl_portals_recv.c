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


#include "ompi_config.h"

#include "ompi/constants.h"

#include "btl_portals.h"
#include "btl_portals_recv.h"
#include "btl_portals_frag.h"


OBJ_CLASS_INSTANCE(mca_btl_portals_recv_block_t,
                   opal_list_item_t,
                   NULL, NULL);

int
mca_btl_portals_recv_enable(mca_btl_portals_module_t *btl)
{
    ptl_md_t md;
    ptl_handle_md_t md_h;
    ptl_process_id_t any_proc = {PTL_NID_ANY, PTL_PID_ANY};
    int ret;
    int i;
    uint64_t ignore_bits = ~((uint64_t) 0);
    
    /* create the reject entry */
    md.start = NULL;
    md.length = 0;
    md.threshold = PTL_MD_THRESH_INF;
    md.max_size = 0;
    md.options = PTL_MD_TRUNCATE | PTL_MD_OP_PUT;
    md.user_ptr = NULL;
    md.eq_handle = PTL_EQ_NONE;

    opal_output_verbose(90, mca_btl_portals_component.portals_output,
                        "About to create reject entry");
    ret = PtlMEAttach(btl->portals_ni_h,
                      OMPI_BTL_PORTALS_SEND_TABLE_ID,
                      any_proc,
                      0, /* match */
                      ignore_bits, /* ignore */
                      PTL_RETAIN,
                      PTL_INS_BEFORE,
                      &(btl->portals_recv_reject_me_h));
    if (PTL_OK != ret) {
        opal_output(mca_btl_portals_component.portals_output,
                    "Error creating recv reject ME: %d", ret);
        return OMPI_ERROR;
    }

    ret = PtlMDAttach(btl->portals_recv_reject_me_h,
                      md,
                      PTL_RETAIN,
                      &md_h);
    if (PTL_OK != ret) {
        opal_output(mca_btl_portals_component.portals_output,
                    "Error attaching recv reject MD: %d", ret);
        mca_btl_portals_recv_disable(btl);
        return OMPI_ERROR;
    }

    /* create the recv blocks */
    for (i = 0 ; i < btl->portals_recv_mds_num ; ++i) {
        mca_btl_portals_recv_block_t *block = 
            mca_btl_portals_recv_block_init(btl);
        if (NULL == block) {
            mca_btl_portals_recv_disable(btl);
            return OMPI_ERROR;
        }
        opal_list_append(&(btl->portals_recv_blocks),
                         (opal_list_item_t*) block);
        mca_btl_portals_activate_block(block);
    }

    return OMPI_SUCCESS;
}


int
mca_btl_portals_recv_disable(mca_btl_portals_module_t *btl)
{
    opal_list_item_t *item;

    if (opal_list_get_size(&btl->portals_recv_blocks) > 0) {
        while (NULL != 
               (item = opal_list_remove_first(&btl->portals_recv_blocks))) {
            mca_btl_portals_recv_block_t *block = 
                (mca_btl_portals_recv_block_t*) item;
            mca_btl_portals_recv_block_free(block);
        }
    }

    if (PTL_INVALID_HANDLE != btl->portals_recv_reject_me_h) {
        /* destroy the reject entry */
        PtlMEUnlink(btl->portals_recv_reject_me_h);
        btl->portals_recv_reject_me_h = PTL_INVALID_HANDLE;
    }

    return OMPI_SUCCESS;
}


mca_btl_portals_recv_block_t* 
mca_btl_portals_recv_block_init(mca_btl_portals_module_t *btl)
{
    mca_btl_portals_recv_block_t *block;

    block = OBJ_NEW(mca_btl_portals_recv_block_t);
    block->btl = btl;
    block->length = btl->portals_recv_mds_size;
    block->start = malloc(block->length);
    if (block->start == NULL) return NULL;

    block->me_h = PTL_INVALID_HANDLE;
    block->md_h = PTL_INVALID_HANDLE;

    block->full = false;
    block->pending = 0;

    return block;
}


int
mca_btl_portals_recv_block_free(mca_btl_portals_recv_block_t *block)
{
    /* need to clear out the md */
    while (block->pending != 0) {
        mca_btl_portals_component_progress();
    }

    if (PTL_INVALID_HANDLE != block->md_h) {
        PtlMDUnlink(block->md_h);
        block->md_h = PTL_INVALID_HANDLE;
    }

    if (NULL != block->start) {
        free(block->start);
        block->start = NULL;
    }
    block->length = 0;
    block->full = false;

    return OMPI_SUCCESS;
}



