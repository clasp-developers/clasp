/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/mca/allocator/allocator.h"
#include "ompi/constants.h"
#include "opal/mca/base/mca_base_param.h"
#include "ompi/mca/allocator/bucket/allocator_bucket_alloc.h"
#include "ompi/mca/mpool/mpool.h" 

struct mca_allocator_base_module_t* mca_allocator_bucket_module_init(
    bool enable_mpi_threads,
    mca_allocator_base_component_segment_alloc_fn_t segment_alloc,
    mca_allocator_base_component_segment_free_fn_t segment_free, 
    struct mca_mpool_base_module_t* mpool
    );

int mca_allocator_bucket_module_open(void);

int mca_allocator_bucket_module_close(void);

void * mca_allocator_bucket_alloc_wrapper(
    struct mca_allocator_base_module_t* allocator,
    size_t size, size_t align, 
    mca_mpool_base_registration_t** registration);

static int mca_allocator_num_buckets;



int mca_allocator_bucket_finalize(struct mca_allocator_base_module_t* allocator)
{
    mca_allocator_bucket_cleanup(allocator);
    free(allocator);
    return(OMPI_SUCCESS);
}

struct mca_allocator_base_module_t* mca_allocator_bucket_module_init(
    bool enable_mpi_threads,
    mca_allocator_base_component_segment_alloc_fn_t segment_alloc,
    mca_allocator_base_component_segment_free_fn_t segment_free, 
    struct mca_mpool_base_module_t* mpool)
{
    size_t alloc_size = sizeof(mca_allocator_bucket_t);
    mca_allocator_bucket_t * retval;
    mca_allocator_bucket_t * allocator = (mca_allocator_bucket_t *) malloc(alloc_size);
    if(NULL == allocator) {
        return(NULL);
    }
    retval = mca_allocator_bucket_init((mca_allocator_base_module_t *) allocator, 
        mca_allocator_num_buckets, 
        segment_alloc, 
        segment_free);
    if(NULL == retval) {
        free(allocator);
        return(NULL);
    }
    allocator->super.alc_alloc =  mca_allocator_bucket_alloc_wrapper;
    allocator->super.alc_realloc = mca_allocator_bucket_realloc;
    allocator->super.alc_free =  mca_allocator_bucket_free;
    allocator->super.alc_compact = mca_allocator_bucket_cleanup;
    allocator->super.alc_finalize = mca_allocator_bucket_finalize;
    allocator->super.alc_mpool = mpool;
    return((mca_allocator_base_module_t *) allocator);
}

int mca_allocator_bucket_module_open(void) {

    int id = mca_base_param_register_int("allocator","bucket","num_buckets", NULL,30);
    mca_base_param_lookup_int(id,&mca_allocator_num_buckets);
    return(OMPI_SUCCESS);
}

int mca_allocator_bucket_module_close(void) {
    return(OMPI_SUCCESS);
}

void * mca_allocator_bucket_alloc_wrapper(
    struct mca_allocator_base_module_t* allocator,
    size_t size, 
    size_t align, 
    mca_mpool_base_registration_t** registration)
{
    if(0 == align){
        return(mca_allocator_bucket_alloc(allocator, size, registration));
    }
    return(mca_allocator_bucket_alloc_align(allocator, size, align, registration));
}    


mca_allocator_base_component_t mca_allocator_bucket_component = { 

  /* First, the mca_base_module_t struct containing meta information
     about the module itself */

  {
    MCA_ALLOCATOR_BASE_VERSION_2_0_0,

    "bucket", /* MCA module name */
    OMPI_MAJOR_VERSION,
    OMPI_MINOR_VERSION,
    OMPI_RELEASE_VERSION,
    mca_allocator_bucket_module_open,  /* module open */
    mca_allocator_bucket_module_close  /* module close */
  },
  {
      /* The component is checkpoint ready */
      MCA_BASE_METADATA_PARAM_CHECKPOINT
  },
  mca_allocator_bucket_module_init
};

