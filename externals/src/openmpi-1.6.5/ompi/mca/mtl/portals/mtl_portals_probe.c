/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2010 The University of Tennessee and The University
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
#include "ompi/communicator/communicator.h"
#include "mtl_portals.h"
#include "mtl_portals_request.h"
#include "mtl_portals_recv.h"

int
ompi_mtl_portals_iprobe(struct mca_mtl_base_module_t* mtl,
                        struct ompi_communicator_t *comm,
                        int src,
                        int tag,
                        int *flag,
                        struct ompi_status_public_t *status)
{
    ptl_match_bits_t match_bits;
    ptl_match_bits_t ignore_bits;
    ompi_mtl_portals_event_t *recv_event = NULL;

    PTL_SET_RECV_BITS(match_bits, ignore_bits, comm->c_contextid, src, tag);

    /* first, check the queue of processed unexpected messages */
    recv_event = ompi_mtl_portals_search_unex_q(match_bits, ignore_bits, true);
    if (NULL == recv_event) {
        /* check for new events */
        recv_event = ompi_mtl_portals_search_unex_events(match_bits, ignore_bits, true);
    }

    if ( NULL != recv_event ) {
        /* found it */
        *flag              = 1;
        status->MPI_SOURCE = PTL_GET_SOURCE(recv_event->ev.match_bits);
        status->MPI_TAG    = PTL_GET_TAG(recv_event->ev.match_bits);
        status->_ucount     = recv_event->ev.rlength;
        status->MPI_ERROR  = OMPI_SUCCESS;
    } else {
        *flag = 0;
    }

    return OMPI_SUCCESS;
}
