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
 * Copyright (c) 2006      Voltaire. All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      IBM Corporation.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include MCA_memory_IMPLEMENTATION_HEADER
#include "opal/mca/memory/memory.h"
#include "ompi/mca/rcache/rcache.h"
#include "rcache_vma.h"
#include "rcache_vma_tree.h"
#include "ompi/mca/mpool/base/base.h" 

/**
 * Initialize the rcache 
 */ 

void mca_rcache_vma_module_init( mca_rcache_vma_module_t* rcache ) { 
    rcache->base.rcache_find = mca_rcache_vma_find; 
    rcache->base.rcache_find_all = mca_rcache_vma_find_all;
    rcache->base.rcache_insert = mca_rcache_vma_insert; 
    rcache->base.rcache_delete = mca_rcache_vma_delete; 
    rcache->base.rcache_clean = mca_rcache_vma_clean; 
    rcache->base.rcache_finalize = mca_rcache_vma_finalize; 
    OBJ_CONSTRUCT(&rcache->base.lock, opal_mutex_t);
    mca_rcache_vma_tree_init(rcache);
}

int mca_rcache_vma_find(struct mca_rcache_base_module_t* rcache,
        void* addr, size_t size, mca_mpool_base_registration_t **reg)
{
    int rc;
    void* base_addr; 
    void* bound_addr; 

    if(size == 0) { 
        return OMPI_ERROR; 
    }

    base_addr = down_align_addr(addr, mca_mpool_base_page_size_log);
    bound_addr = up_align_addr((void*) ((unsigned long) addr + size - 1), mca_mpool_base_page_size_log);
        
    /* Check to ensure that the cache is valid */
    if (OPAL_UNLIKELY(opal_memory_changed() && 
                      NULL != opal_memory->memoryc_process &&
                      OPAL_SUCCESS != (rc = opal_memory->memoryc_process()))) {
        return rc;
    }

    *reg = mca_rcache_vma_tree_find((mca_rcache_vma_module_t*)rcache, (unsigned char*)base_addr,
            (unsigned char*)bound_addr); 

    return OMPI_SUCCESS;
}

int mca_rcache_vma_find_all(struct mca_rcache_base_module_t* rcache,
        void* addr, size_t size, mca_mpool_base_registration_t **regs,
        int reg_cnt)
{
    int rc;
    void *base_addr, *bound_addr;

    if(size == 0) {
        return OMPI_ERROR;
    }

    base_addr = down_align_addr(addr, mca_mpool_base_page_size_log);
    bound_addr = up_align_addr((void*) ((unsigned long) addr + size - 1), mca_mpool_base_page_size_log);

    /* Check to ensure that the cache is valid */
    if (OPAL_UNLIKELY(opal_memory_changed() && 
                      NULL != opal_memory->memoryc_process &&
                      OPAL_SUCCESS != (rc = opal_memory->memoryc_process()))) {
        return rc;
    }

    return mca_rcache_vma_tree_find_all((mca_rcache_vma_module_t*)rcache,
            (unsigned char*)base_addr, (unsigned char*)bound_addr, regs,
            reg_cnt);
}

int mca_rcache_vma_insert(struct mca_rcache_base_module_t* rcache,
        mca_mpool_base_registration_t* reg, size_t limit)
{
    int rc;
    size_t reg_size = reg->bound - reg->base + 1;
    mca_rcache_vma_module_t *vma_rcache = (mca_rcache_vma_module_t*)rcache;

    if(limit != 0 && reg_size > limit) {
        /* return out of resources if request is bigger than cache size
         * return temp out of resources otherwise */
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Check to ensure that the cache is valid */
    if (OPAL_UNLIKELY(opal_memory_changed() &&
                      NULL != opal_memory->memoryc_process &&
                      OPAL_SUCCESS != (rc = opal_memory->memoryc_process()))) {
        return rc;
    }

    rc = mca_rcache_vma_tree_insert(vma_rcache, reg, limit);
    if (OPAL_LIKELY(OMPI_SUCCESS == rc)) {
        /* If we successfully registered, then tell the memory manager
           to start monitoring this region */
        opal_memory->memoryc_register(reg->base, 
                                      (uint64_t) reg_size, (uint64_t) (uintptr_t) reg);
    }

    return rc;
}

int mca_rcache_vma_delete(struct mca_rcache_base_module_t* rcache,
        mca_mpool_base_registration_t* reg)
{
    mca_rcache_vma_module_t *vma_rcache = (mca_rcache_vma_module_t*)rcache;
    /* Tell the memory manager that we no longer care about this
       region */
    opal_memory->memoryc_deregister(reg->base,
                                    (uint64_t) (reg->bound - reg->base),
                                    (uint64_t) (uintptr_t) reg);
    return mca_rcache_vma_tree_delete(vma_rcache, reg);
}

int mca_rcache_vma_clean(struct mca_rcache_base_module_t* rcache)
{
    mca_rcache_vma_module_t *vma_rcache = (mca_rcache_vma_module_t*)rcache;
    mca_rcache_vma_t *vma;
    opal_list_item_t *i;

    do {
	OPAL_THREAD_LOCK(&rcache->lock);
	i = opal_list_get_first(&vma_rcache->vma_delete_list);
	if(opal_list_get_end(&vma_rcache->vma_delete_list) == i) {
	    vma = NULL;
	    OPAL_THREAD_UNLOCK(&rcache->lock);
	} else {
	    vma = (mca_rcache_vma_t *)i;
	    opal_list_remove_item(&vma_rcache->vma_delete_list, &vma->super);
	    
	    /* Need to drop the rcache lock before destroying the vma */
	    OPAL_THREAD_UNLOCK(&rcache->lock);
	    
	    mca_rcache_vma_destroy(vma);
	}
    } while (NULL != vma);
    return OMPI_SUCCESS;
}

/**
  * finalize
  */
void mca_rcache_vma_finalize(struct mca_rcache_base_module_t* rcache)
{
}
