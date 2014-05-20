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

#ifndef OMPI_MTL_MX_REQUEST_H
#define OMPI_MTL_MX_REQUEST_H

#include "opal/datatype/opal_convertor.h"


typedef enum {
    OMPI_MTL_MX_ISEND,
    OMPI_MTL_MX_IRECV
} mca_mtl_mx_request_type_t;

struct mca_mtl_mx_request_t { 
    struct mca_mtl_request_t super;
    mx_request_t mx_request;
    mx_segment_t mx_segment[1];
    struct opal_convertor_t *convertor;
    bool free_after;
    mca_mtl_mx_request_type_t type;
}; 
typedef struct mca_mtl_mx_request_t mca_mtl_mx_request_t;

#endif
