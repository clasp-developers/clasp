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

#ifndef OMPI_MTL_PORTALS_ENDPOINT_H
#define OMPI_MTL_PORTALS_ENDPOINT_H

struct mca_mtl_base_endpoint_t {
    ptl_process_id_t ptl_proc;
};
typedef struct mca_mtl_base_endpoint_t mca_mtl_base_endpoint_t;

#endif
