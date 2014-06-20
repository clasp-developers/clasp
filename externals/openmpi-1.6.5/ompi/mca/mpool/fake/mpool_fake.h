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
 * Copyright (c) 2008      Myricom. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 */

#ifndef MCA_MPOOL_FAKE_H
#define MCA_MPOOL_FAKE_H

#include "ompi_config.h"
#include "ompi/mca/mpool/mpool.h"

BEGIN_C_DECLS

typedef struct mca_mpool_fake_component_t {
  mca_mpool_base_component_t super;
} mca_mpool_fake_component_t;

OMPI_DECLSPEC extern mca_mpool_fake_component_t mca_mpool_fake_component;

typedef struct mca_mpool_base_resources_t {
  int (*regcache_clean)(void *ptr, size_t size);
} mca_mpool_base_resources_t;

typedef struct mca_mpool_fake_module_t {
  mca_mpool_base_module_t super;
  mca_mpool_base_resources_t resources;
} mca_mpool_fake_module_t;

/*
 *  Initializes the mpool module.
 */
void mca_mpool_fake_module_init(mca_mpool_fake_module_t *mpool);

/**
 * unregister all registration covering the block of memory
 */
int mca_mpool_fake_release_memory(mca_mpool_base_module_t* mpool, void *base,
        size_t size);

/**
 * finalize mpool
 */
void mca_mpool_fake_finalize(struct mca_mpool_base_module_t *mpool);

/**
 * Fault Tolerance Event Notification Function
 * @param state Checkpoint Stae
 * @return OMPI_SUCCESS or failure status
 */
int mca_mpool_fake_ft_event(int state);

END_C_DECLS
#endif
