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

#ifndef OMPI_MTL_PORTALS_RECV_H
#define OMPI_MTL_PORTALS_RECV_H

extern ompi_mtl_portals_event_t*
ompi_mtl_portals_search_unex_events(ptl_match_bits_t match_bits,
                                    ptl_match_bits_t ignore_bits,
                                    bool             probe);

extern ompi_mtl_portals_event_t*
ompi_mtl_portals_search_unex_q(ptl_match_bits_t match_bits,
                               ptl_match_bits_t ignore_bits,
                               bool             probe);

#endif /* OMPI_MTL_PORTALS_RECV_SHORT_H */
