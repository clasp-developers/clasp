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

#ifndef OMPI_MTL_PORTALS_SEND_SHORT_H
#define OMPI_MTL_PORTALS_SEND_SHORT_H

extern void ompi_mtl_portals_short_setup(void);
extern void ompi_mtl_portals_short_cleanup(void);

static inline int
ompi_mtl_portals_alloc_short_buf(void)
{
    int buf_num;

    while ( ompi_mtl_portals.ptl_copy_block_first_free == ompi_mtl_portals.ptl_num_copy_blocks ) {
        ompi_mtl_portals_progress();
    }

    buf_num = ompi_mtl_portals.ptl_copy_block_free_list[ompi_mtl_portals.ptl_copy_block_first_free++];

    assert((buf_num >= 0) && (buf_num < ompi_mtl_portals.ptl_num_copy_blocks));

    return buf_num;
}

static inline void
ompi_mtl_portals_free_short_buf( int offset )
{
    int buf_num;

    buf_num = offset / ompi_mtl_portals.ptl_copy_block_len;

    assert((buf_num >= 0) && (buf_num < ompi_mtl_portals.ptl_num_copy_blocks));

    ompi_mtl_portals.ptl_copy_block_first_free--;

    assert(ompi_mtl_portals.ptl_copy_block_first_free >= 0);

    ompi_mtl_portals.ptl_copy_block_free_list[ompi_mtl_portals.ptl_copy_block_first_free] = buf_num;

}


#endif /* OMPI_MTL_PORTALS_SEND_SHORT_H */
