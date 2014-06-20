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
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "ompi_config.h"
#include "ompi/constants.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/bml/bml.h"
#include "ompi/mca/mpool/mpool.h" 
#include "pml_bfo.h"
#include "pml_bfo_rdma.h"

/* Use this registration if no registration needed for a BTL instead of NULL.
 * This will help other code to distinguish case when memory is not registered
 * from case when registration is not needed */
static mca_mpool_base_registration_t pml_bfo_dummy_reg;

/*
 * Check to see if memory is registered or can be registered. Build a 
 * set of registrations on the request.
 */

size_t mca_pml_bfo_rdma_btls(
    mca_bml_base_endpoint_t* bml_endpoint,
    unsigned char* base,
    size_t size,
    mca_pml_bfo_com_btl_t* rdma_btls)
{
    int num_btls = mca_bml_base_btl_array_get_size(&bml_endpoint->btl_rdma);
    double weight_total = 0;
    int num_btls_used = 0, n;

    /* shortcut when there are no rdma capable btls */
    if(num_btls == 0) {
        return 0;
    }

    /* check to see if memory is registered */        
    for(n = 0; n < num_btls && num_btls_used < mca_pml_bfo.max_rdma_per_request;
            n++) {
        mca_bml_base_btl_t* bml_btl =
            mca_bml_base_btl_array_get_index(&bml_endpoint->btl_rdma,
                    (bml_endpoint->btl_rdma_index + n) % num_btls); 
        mca_mpool_base_registration_t* reg = &pml_bfo_dummy_reg;
        mca_mpool_base_module_t *btl_mpool = bml_btl->btl->btl_mpool;

        if( NULL != btl_mpool ) {
            if(!mca_pml_bfo.leave_pinned) {
                /* look through existing registrations */
                btl_mpool->mpool_find(btl_mpool, base, size, &reg);
            } else {
                /* register the memory */
                btl_mpool->mpool_register(btl_mpool, base, size, 0, &reg);
            }

            if(NULL == reg)
                continue;
        }

        rdma_btls[num_btls_used].bml_btl = bml_btl;
        rdma_btls[num_btls_used].btl_reg = reg;
        weight_total += bml_btl->btl_weight;
        num_btls_used++;
    }

    /* if we don't use leave_pinned and all BTLs that already have this memory
     * registered amount to less then half of available bandwidth - fall back to
     * pipeline protocol */
    if(0 == num_btls_used || (!mca_pml_bfo.leave_pinned && weight_total < 0.5))
        return 0;

    mca_pml_bfo_calc_weighted_length(rdma_btls, num_btls_used, size,
                                     weight_total);

    bml_endpoint->btl_rdma_index = (bml_endpoint->btl_rdma_index + 1) % num_btls;
    return num_btls_used;
}

size_t mca_pml_bfo_rdma_pipeline_btls( mca_bml_base_endpoint_t* bml_endpoint,
                                       size_t size,
                                       mca_pml_bfo_com_btl_t* rdma_btls )
{
    int i, num_btls = mca_bml_base_btl_array_get_size(&bml_endpoint->btl_rdma);
    double weight_total = 0;

    for(i = 0; i < num_btls && i < mca_pml_bfo.max_rdma_per_request; i++) {
        rdma_btls[i].bml_btl =
            mca_bml_base_btl_array_get_next(&bml_endpoint->btl_rdma);
        if(NULL != rdma_btls[i].bml_btl->btl->btl_mpool)
            rdma_btls[i].btl_reg = NULL;
        else
            rdma_btls[i].btl_reg = &pml_bfo_dummy_reg;

        weight_total += rdma_btls[i].bml_btl->btl_weight;
    }

    mca_pml_bfo_calc_weighted_length(rdma_btls, i, size, weight_total);

    return i;
}
