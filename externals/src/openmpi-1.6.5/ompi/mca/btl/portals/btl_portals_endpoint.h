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

#ifndef MCA_BTL_PORTALS_ENDPOINT_H
#define MCA_BTL_PORTALS_ENDPOINT_H

#include "btl_portals.h"

BEGIN_C_DECLS

/**
 * An abstraction that represents a connection to a endpoint process.
 * An instance of mca_btl_base_endpoint_t is associated w/ each process
 * and BTL pair at startup. However, connections to the endpoint
 * are established dynamically on an as-needed basis:
 */
    typedef ptl_process_id_t mca_btl_base_endpoint_t;
    typedef mca_btl_base_endpoint_t  mca_btl_portals_endpoint_t;

END_C_DECLS

#endif /* MCA_BTL_PORTALS_ENDPOINT_H */
