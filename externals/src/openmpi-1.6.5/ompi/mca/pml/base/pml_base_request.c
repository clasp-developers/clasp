/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
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
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/pml_base_request.h"

/**
 * If you wonder why these 2 freelists are declared here read the comment
 * in the pml_base_request.h file.
 */
ompi_free_list_t mca_pml_base_send_requests = {{{0}}};
ompi_free_list_t mca_pml_base_recv_requests = {{{0}}};

static void mca_pml_base_request_construct(mca_pml_base_request_t* req)
{
    req->req_ompi.req_type = OMPI_REQUEST_PML;
}

static void mca_pml_base_request_destruct(mca_pml_base_request_t* req)
{
}

OBJ_CLASS_INSTANCE(
    mca_pml_base_request_t,
    ompi_request_t,
    mca_pml_base_request_construct,
    mca_pml_base_request_destruct
);

