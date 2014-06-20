/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_MPOOL_SM_H
#define MCA_MPOOL_SM_H

#include "ompi_config.h"

#include "opal/event/event.h"

#include "ompi/mca/common/sm/common_sm.h"
#include "ompi/mca/mpool/mpool.h"
#include "ompi/mca/allocator/allocator.h"

BEGIN_C_DECLS

struct mca_mpool_sm_component_t {
  mca_mpool_base_component_t super;
  /*      mca_allocator_base_module_t* sm_allocator; */
  char*  sm_allocator_name;
  int    verbose;
  /*      struct mca_mpool_sm_mmap_t *sm_mmap; */
};
typedef struct mca_mpool_sm_component_t mca_mpool_sm_component_t;

typedef struct mca_mpool_base_resources_t {
    size_t  size;
    int32_t mem_node;
} mca_mpool_base_resources_t;

OMPI_MODULE_DECLSPEC extern mca_mpool_sm_component_t mca_mpool_sm_component;

typedef struct mca_mpool_sm_module_t {
    mca_mpool_base_module_t super;
    long sm_size;
    mca_allocator_base_module_t * sm_allocator;
    struct mca_mpool_sm_mmap_t *sm_mmap;
    mca_common_sm_module_t *sm_common_module;
    int32_t mem_node;
} mca_mpool_sm_module_t;

/*
 *  Initializes the mpool module.
 */
void mca_mpool_sm_module_init(mca_mpool_sm_module_t* mpool);


/*
 *  Returns base address of shared memory mapping.
 */
void* mca_mpool_sm_base(mca_mpool_base_module_t*);

/**
  *  Allocate block of shared memory.
  */
void* mca_mpool_sm_alloc(
    mca_mpool_base_module_t* mpool,
    size_t size,
    size_t align,
    uint32_t flags,
    mca_mpool_base_registration_t** registration);

/**
  * realloc function typedef
  */
void* mca_mpool_sm_realloc(
    mca_mpool_base_module_t* mpool,
    void* addr,
    size_t size,
    mca_mpool_base_registration_t** registration);

/**
  * free function typedef
  */
void mca_mpool_sm_free(
    mca_mpool_base_module_t* mpool,
    void * addr,
    mca_mpool_base_registration_t* registration);

/**
 * Fault Tolerance Event Notification Function
 * @param state Checkpoint Stae
 * @return OMPI_SUCCESS or failure status
 */
int mca_mpool_sm_ft_event(int state);

END_C_DECLS

#endif
