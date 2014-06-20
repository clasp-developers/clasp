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
/**
 *  @file
 */
                                                                                                                                                 
#ifndef MCA_PML_CSUM_RDMAFRAG_H
#define MCA_PML_CSUM_RDMAFRAG_H

#include "ompi/mca/btl/btl.h"
#include "pml_csum_hdr.h"

BEGIN_C_DECLS

typedef enum {
    MCA_PML_CSUM_RDMA_PUT,
    MCA_PML_CSUM_RDMA_GET
} mca_pml_csum_rdma_state_t;

struct mca_pml_csum_rdma_frag_t {
    ompi_free_list_item_t super;
    mca_bml_base_btl_t* rdma_bml;
    mca_pml_csum_hdr_t rdma_hdr;
    mca_pml_csum_rdma_state_t rdma_state;
    size_t rdma_length;
    mca_btl_base_segment_t rdma_segs[MCA_BTL_DES_MAX_SEGMENTS];
    void *rdma_req;
    struct mca_bml_base_endpoint_t* rdma_ep;
    opal_convertor_t convertor;
    mca_mpool_base_registration_t* reg;
    uint32_t retries;
};
typedef struct mca_pml_csum_rdma_frag_t mca_pml_csum_rdma_frag_t;

OBJ_CLASS_DECLARATION(mca_pml_csum_rdma_frag_t);


#define MCA_PML_CSUM_RDMA_FRAG_ALLOC(frag,rc)                    \
do {                                                            \
    ompi_free_list_item_t* item;                                \
    OMPI_FREE_LIST_WAIT(&mca_pml_csum.rdma_frags, item, rc);     \
    frag = (mca_pml_csum_rdma_frag_t*)item;                      \
} while(0)

#define MCA_PML_CSUM_RDMA_FRAG_RETURN(frag)                      \
do {                                                            \
    /* return fragment */                                       \
    OMPI_FREE_LIST_RETURN(&mca_pml_csum.rdma_frags,              \
        (ompi_free_list_item_t*)frag);                          \
} while(0)


END_C_DECLS

#endif

