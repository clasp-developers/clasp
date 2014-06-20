/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
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
#include "opal/util/output.h"


#include "mtl_portals.h"
#include "mtl_portals_request.h"
#include "mtl_portals_send_short.h"

static ompi_mtl_portals_request_t ptl_short_request;

/* short send callback */
static int
ompi_mtl_portals_short_callback(ptl_event_t *ev, ompi_mtl_portals_request_t *ptl_request)
{

    switch (ev->type) {

	case PTL_EVENT_SEND_END:

            ompi_mtl_portals_free_short_buf(ev->offset);
	    
	    break;

	default:
	    opal_output(fileno(stderr)," Unexpected event type %d in ompi_mtl_portals_short_callback()\n",ev->type); 
	    abort();
    }

    return OMPI_SUCCESS;

}

/* initialize short copy blocks */ 
void
ompi_mtl_portals_short_setup()
{
    int ret;
    int i;

    if ((ompi_mtl_portals.ptl_num_copy_blocks > 0) && (ompi_mtl_portals.ptl_copy_block_len > 0)) {

        ompi_mtl_portals.ptl_short_md.length = ompi_mtl_portals.ptl_num_copy_blocks * 
                                               ompi_mtl_portals.ptl_copy_block_len;

        ompi_mtl_portals.ptl_short_md.start = malloc(ompi_mtl_portals.ptl_short_md.length);
        if (NULL == ompi_mtl_portals.ptl_short_md.start ) {
            ompi_mtl_portals.ptl_num_copy_blocks = 0;
            return;
        }

        ompi_mtl_portals.ptl_short_md.threshold = PTL_MD_THRESH_INF;
        ompi_mtl_portals.ptl_short_md.max_size  = 0;
        ompi_mtl_portals.ptl_short_md.options   = PTL_MD_EVENT_START_DISABLE;
        ompi_mtl_portals.ptl_short_md.user_ptr  = &ptl_short_request;
        ompi_mtl_portals.ptl_short_md.eq_handle = ompi_mtl_portals.ptl_eq_h;

        ret = PtlMDBind(ompi_mtl_portals.ptl_ni_h,
                        ompi_mtl_portals.ptl_short_md,
                        PTL_RETAIN,
                        &ompi_mtl_portals.ptl_short_md_h);
        if (PTL_OK != ret) {
            free(ompi_mtl_portals.ptl_short_md.start);
            ompi_mtl_portals.ptl_num_copy_blocks = 0;
            return;
        }

        ptl_short_request.event_callback = ompi_mtl_portals_short_callback;

        ompi_mtl_portals.ptl_copy_block_free_list = malloc(ompi_mtl_portals.ptl_num_copy_blocks * sizeof(int));
        if (NULL == ompi_mtl_portals.ptl_copy_block_free_list) {
            free(ompi_mtl_portals.ptl_short_md.start);
            ompi_mtl_portals.ptl_num_copy_blocks = 0;
            return;
        }

        for (i=0; i<ompi_mtl_portals.ptl_num_copy_blocks; i++) {
            ompi_mtl_portals.ptl_copy_block_free_list[i] = i;
        }

        ompi_mtl_portals.ptl_copy_block_first_free = 0;

    }

}

/* free short resources */
void
ompi_mtl_portals_short_cleanup()
{
    if (ompi_mtl_portals.ptl_num_copy_blocks > 0) {
        free(ompi_mtl_portals.ptl_short_md.start);
        free(ompi_mtl_portals.ptl_copy_block_free_list);
        ompi_mtl_portals.ptl_num_copy_blocks = 0;
    }

}

