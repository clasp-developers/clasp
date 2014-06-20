/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Myricom.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#define OPAL_DISABLE_ENABLE_MEM_DEBUG 1
#include "ompi_config.h"
#include "mpool_fake.h"
#include <errno.h>
#include <string.h>
#include "ompi/mca/mpool/base/base.h"

/*
 *  Initializes the mpool module.
 */
void mca_mpool_fake_module_init(mca_mpool_fake_module_t* mpool)
{
    mpool->super.mpool_component = &mca_mpool_fake_component.super;
    mpool->super.mpool_base = NULL; /* no base .. */
    mpool->super.mpool_alloc = NULL;
    mpool->super.mpool_realloc = NULL;
    mpool->super.mpool_free = NULL;
    mpool->super.mpool_register = NULL;
    mpool->super.mpool_find = NULL;
    mpool->super.mpool_deregister = NULL;
    mpool->super.mpool_release_memory = mca_mpool_fake_release_memory;
    mpool->super.mpool_finalize = mca_mpool_fake_finalize;
    mpool->super.mpool_ft_event = mca_mpool_fake_ft_event;
}


int mca_mpool_fake_release_memory(struct mca_mpool_base_module_t *mpool,
        void *base, size_t size)
{
    mca_mpool_fake_module_t *mpool_module = (mca_mpool_fake_module_t*)mpool;

    mpool_module->resources.regcache_clean(base, size);

    return OMPI_SUCCESS;
}

void mca_mpool_fake_finalize(struct mca_mpool_base_module_t *mpool)
{
    ;
}

int mca_mpool_fake_ft_event(int state)
{
    return OMPI_SUCCESS;
}
