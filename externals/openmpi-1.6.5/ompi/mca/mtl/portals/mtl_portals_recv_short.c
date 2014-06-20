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

#include "mtl_portals.h"
#include "mtl_portals_recv_short.h"


OBJ_CLASS_INSTANCE(ompi_mtl_portals_recv_short_block_t,
                   opal_list_item_t,
                   NULL, NULL);

int
ompi_mtl_portals_recv_short_enable(mca_mtl_portals_module_t *mtl)
{
    int i;

    /* create the recv blocks */
    for (i = 0 ; i < mtl->ptl_recv_short_mds_num ; ++i) {
        ompi_mtl_portals_recv_short_block_t *block = 
            ompi_mtl_portals_recv_short_block_init(mtl);
        if (NULL == block) {
            ompi_mtl_portals_recv_short_disable(mtl);
            return OMPI_ERROR;
        }
        opal_list_append(&(mtl->ptl_recv_short_blocks),
                         (opal_list_item_t*) block);
        ompi_mtl_portals_activate_block(block);
    }

    return OMPI_SUCCESS;
}


int
ompi_mtl_portals_recv_short_disable(mca_mtl_portals_module_t *mtl)
{
    opal_list_item_t *item;

    if (opal_list_get_size(&mtl->ptl_recv_short_blocks) > 0) {
        while (NULL != 
               (item = opal_list_remove_first(&mtl->ptl_recv_short_blocks))) {
            ompi_mtl_portals_recv_short_block_t *block = 
                (ompi_mtl_portals_recv_short_block_t*) item;
            ompi_mtl_portals_recv_short_block_free(block);
        }
    }

    return OMPI_SUCCESS;
}


ompi_mtl_portals_recv_short_block_t* 
ompi_mtl_portals_recv_short_block_init(mca_mtl_portals_module_t *mtl)
{
    ompi_mtl_portals_recv_short_block_t *block;

    block = OBJ_NEW(ompi_mtl_portals_recv_short_block_t);
    block->mtl = mtl;
    block->length = mtl->ptl_recv_short_mds_size;
    block->start = malloc(block->length);
    if (block->start == NULL) return NULL;

    block->me_h = PTL_INVALID_HANDLE;
    block->md_h = PTL_INVALID_HANDLE;

    block->full = false;
    block->pending = 0;

    return block;
}


int
ompi_mtl_portals_recv_short_block_free(ompi_mtl_portals_recv_short_block_t *block)
{
    /* need to clear out the md */
    while (block->pending != 0) {
        ompi_mtl_portals_progress();
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
