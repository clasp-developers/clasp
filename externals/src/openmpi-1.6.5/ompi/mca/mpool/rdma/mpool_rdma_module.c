/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2006-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006      Voltaire. All rights reserved.
 * Copyright (c) 2007      Mellanox Technologies. All rights reserved.
 * Copyright (c) 2009      IBM Corporation.  All rights reserved.
 * Copyright (c) 2011      Los Alamos National Security, LLC. All rights
 *                         reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#define OPAL_DISABLE_ENABLE_MEM_DEBUG 1
#include "ompi_config.h"
#include "opal/align.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "ompi/mca/mpool/rdma/mpool_rdma.h"
#include <errno.h>
#include <string.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#include "ompi/mca/rcache/rcache.h"
#include "ompi/mca/rcache/base/base.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/runtime/params.h"

/*
 *  Initializes the mpool module.
 */
void mca_mpool_rdma_module_init(mca_mpool_rdma_module_t* mpool)
{
    mpool->super.mpool_component = &mca_mpool_rdma_component.super;
    mpool->super.mpool_base = NULL; /* no base .. */
    mpool->super.mpool_alloc = mca_mpool_rdma_alloc;
    mpool->super.mpool_realloc = mca_mpool_rdma_realloc;
    mpool->super.mpool_free = mca_mpool_rdma_free;
    mpool->super.mpool_register = mca_mpool_rdma_register;
    mpool->super.mpool_find = mca_mpool_rdma_find;
    mpool->super.mpool_deregister = mca_mpool_rdma_deregister;
    mpool->super.mpool_release_memory = mca_mpool_rdma_release_memory;
    mpool->super.mpool_finalize = mca_mpool_rdma_finalize;
    mpool->super.mpool_ft_event = mca_mpool_rdma_ft_event;
    mpool->super.rcache =
        mca_rcache_base_module_create(mca_mpool_rdma_component.rcache_name);
    mpool->super.flags = MCA_MPOOL_FLAGS_MPI_ALLOC_MEM;

    OBJ_CONSTRUCT(&mpool->reg_list, ompi_free_list_t);
    ompi_free_list_init_new(&mpool->reg_list, mpool->resources.sizeof_reg,
            opal_cache_line_size,
            OBJ_CLASS(mca_mpool_base_registration_t), 
            0,opal_cache_line_size,
            0, -1, 32, NULL);
    OBJ_CONSTRUCT(&mpool->lru_list, opal_list_t);
    OBJ_CONSTRUCT(&mpool->gc_list, opal_list_t);
    mpool->stat_cache_hit = mpool->stat_cache_miss = mpool->stat_evicted = 0;
    mpool->stat_cache_found = mpool->stat_cache_notfound = 0;

    /* Set this here (vs in component.c) because
       ompi_mpi_leave_pinned* may have been set after MCA params were
       read (e.g., by the openib btl) */
    mca_mpool_rdma_component.leave_pinned = (int) 
        (1 == ompi_mpi_leave_pinned || ompi_mpi_leave_pinned_pipeline);
}

static inline int dereg_mem(mca_mpool_base_module_t *mpool,
        mca_mpool_base_registration_t *reg)
{
    mca_mpool_rdma_module_t *mpool_rdma = (mca_mpool_rdma_module_t *)mpool;

    assert(reg->ref_count == 0);
    return mpool_rdma->resources.deregister_mem(mpool_rdma->resources.reg_data,
            reg);
}

/**
  * allocate function
  */
void* mca_mpool_rdma_alloc(mca_mpool_base_module_t *mpool, size_t size,
        size_t align, uint32_t flags, mca_mpool_base_registration_t **reg)
{
    void *base_addr, *addr;

    if(0 == align)
        align = mca_mpool_base_page_size;

#ifdef HAVE_POSIX_MEMALIGN
    if((errno = posix_memalign(&base_addr, align, size)) != 0)
        return NULL;

    addr = base_addr;
#else
    base_addr = malloc(size + align);
    if(NULL == base_addr)
        return NULL;

    addr = (void*)OPAL_ALIGN((uintptr_t)base_addr, align, uintptr_t);
#endif

    if(OMPI_SUCCESS != mca_mpool_rdma_register(mpool, addr, size, flags, reg)) {
        free(base_addr);
        return NULL;
    }
    (*reg)->alloc_base = (unsigned char *) base_addr;

    return addr;
}

/* This function must be called with the rcache lock held */
static void do_unregistration_gc(struct mca_mpool_base_module_t *mpool)
{
    mca_mpool_rdma_module_t *mpool_rdma = (mca_mpool_rdma_module_t*)mpool;
    mca_mpool_base_registration_t *reg;

    do {
        /* Remove registration from garbage collection list
           before deregistering it */
        reg = (mca_mpool_base_registration_t *)
            opal_list_remove_first(&mpool_rdma->gc_list);
        mpool->rcache->rcache_delete(mpool->rcache, reg);

        /* Drop the rcache lock before calling dereg_mem as there
           may be memory allocations */
        OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
        dereg_mem(mpool, reg);
        OPAL_THREAD_LOCK(&mpool->rcache->lock);

        OMPI_FREE_LIST_RETURN(&mpool_rdma->reg_list,
                (ompi_free_list_item_t*)reg);
    } while(!opal_list_is_empty(&mpool_rdma->gc_list));
}

static int register_cache_bypass(mca_mpool_base_module_t *mpool,
        void *addr, size_t size, uint32_t flags,
        mca_mpool_base_registration_t **reg)
{
    mca_mpool_rdma_module_t *mpool_rdma = (mca_mpool_rdma_module_t*)mpool;
    mca_mpool_base_registration_t *rdma_reg;
    ompi_free_list_item_t *item;
    unsigned char *base, *bound;
    int rc;

    base = (unsigned char *) down_align_addr(addr, mca_mpool_base_page_size_log);
    bound = (unsigned char *) up_align_addr( (void*) ((char*) addr + size - 1),
             mca_mpool_base_page_size_log);
    OMPI_FREE_LIST_GET(&mpool_rdma->reg_list, item, rc);
    if(OMPI_SUCCESS != rc) {
        return rc;
    }
    rdma_reg = (mca_mpool_base_registration_t*)item;

    rdma_reg->mpool = mpool;
    rdma_reg->base = base;
    rdma_reg->bound = bound;
    rdma_reg->flags = flags;

    rc = mpool_rdma->resources.register_mem(mpool_rdma->resources.reg_data,
            base, bound - base + 1, rdma_reg);

    if(rc != OMPI_SUCCESS) {
        OMPI_FREE_LIST_RETURN(&mpool_rdma->reg_list, item);
        return rc;
    }

    *reg = rdma_reg;
    (*reg)->ref_count++;
    return OMPI_SUCCESS;
}

static inline bool mca_mpool_rdma_deregister_lru (mca_mpool_base_module_t *mpool) {
    mca_mpool_rdma_module_t *mpool_rdma = (mca_mpool_rdma_module_t *) mpool;
    mca_mpool_base_registration_t *old_reg;
    int rc;

    /* Remove the registration from the cache and list before
       deregistering the memory */
    old_reg = (mca_mpool_base_registration_t*)
        opal_list_remove_first (&mpool_rdma->lru_list);
    if (NULL == old_reg) {
        return false;
    }

    mpool->rcache->rcache_delete(mpool->rcache, old_reg);

    /* Drop the rcache lock while we deregister the memory */
    OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
    rc = dereg_mem(mpool, old_reg);
    OPAL_THREAD_LOCK(&mpool->rcache->lock);

    /* This introduces a potential leak of registrations if
       the deregistration fails to occur as we no longer have
       a reference to it. Is this possible? */
    if (OMPI_SUCCESS != rc) {
        return false;
    }

    OMPI_FREE_LIST_RETURN(&mpool_rdma->reg_list,
                          (ompi_free_list_item_t*)old_reg);
    mpool_rdma->stat_evicted++;

    return true;
}

/*
 * register memory
 */
int mca_mpool_rdma_register(mca_mpool_base_module_t *mpool, void *addr,
                              size_t size, uint32_t flags,
                              mca_mpool_base_registration_t **reg)
{
    mca_mpool_rdma_module_t *mpool_rdma = (mca_mpool_rdma_module_t*)mpool;
    mca_mpool_base_registration_t *rdma_reg;
    ompi_free_list_item_t *item;
    unsigned char *base, *bound;
    int rc;

    /* if cache bypass is requested don't use the cache */
    if(flags & MCA_MPOOL_FLAGS_CACHE_BYPASS) {
        return register_cache_bypass(mpool, addr, size, flags, reg);
    }

    base = (unsigned char *) down_align_addr(addr, mca_mpool_base_page_size_log);
    bound = (unsigned char *) up_align_addr((void*)((char*) addr + size - 1),
             mca_mpool_base_page_size_log);
    OPAL_THREAD_LOCK(&mpool->rcache->lock);
    if(!opal_list_is_empty(&mpool_rdma->gc_list))
        do_unregistration_gc(mpool);

    /* look through existing regs if not persistent registration requested.
     * Persistent registration are always registered and placed in the cache */
    if(!(flags & MCA_MPOOL_FLAGS_PERSIST)) {
        /* check to see if memory is registered */
        mpool->rcache->rcache_find(mpool->rcache, addr, size, reg);
        if(*reg != NULL &&
                (mca_mpool_rdma_component.leave_pinned ||
                 ((*reg)->flags & MCA_MPOOL_FLAGS_PERSIST) ||
                 ((*reg)->base == base && (*reg)->bound == bound))) {
            if(0 == (*reg)->ref_count &&
                    mca_mpool_rdma_component.leave_pinned) {
                opal_list_remove_item(&mpool_rdma->lru_list,
                        (opal_list_item_t*)(*reg));
            }
            mpool_rdma->stat_cache_hit++;
            (*reg)->ref_count++;
            OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
            return OMPI_SUCCESS;
        }

        mpool_rdma->stat_cache_miss++;
        *reg = NULL; /* in case previous find found something */

        /* If no suitable registration is in cache and leave_pinned isn't
         * set and size of registration cache is unlimited don't use the cache.
         * This is optimisation in case limit is not set. If limit is set we
         * have to put registration into the cache to determine when we hit
         * memory registration limit.
         * NONE: cache is still used for persistent registrations so previous
         * find can find something */
        if(!mca_mpool_rdma_component.leave_pinned &&
                 mca_mpool_rdma_component.rcache_size_limit == 0) {
            OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
            return register_cache_bypass(mpool, addr, size, flags, reg);
        }
    }

    OMPI_FREE_LIST_GET(&mpool_rdma->reg_list, item, rc);
    if(OMPI_SUCCESS != rc) {
        OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
        return rc;
    }
    rdma_reg = (mca_mpool_base_registration_t*)item;

    rdma_reg->mpool = mpool;
    rdma_reg->base = base;
    rdma_reg->bound = bound;
    rdma_reg->flags = flags;

    while((rc = mpool->rcache->rcache_insert(mpool->rcache, rdma_reg,
             mca_mpool_rdma_component.rcache_size_limit)) ==
            OMPI_ERR_TEMP_OUT_OF_RESOURCE) {
        /* try to remove one unused reg and retry */
        if (!mca_mpool_rdma_deregister_lru (mpool)) {
            break;
        }        
    }

    if(rc != OMPI_SUCCESS) {
        OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
        OMPI_FREE_LIST_RETURN(&mpool_rdma->reg_list, item);
        return rc;
    }

    while (OMPI_ERR_OUT_OF_RESOURCE ==
           (rc = mpool_rdma->resources.register_mem(mpool_rdma->resources.reg_data,
                                                    base, bound - base + 1, rdma_reg))) {
        /* try to remove one unused reg and retry */
        if (!mca_mpool_rdma_deregister_lru (mpool)) {
            break;
        }        
    }

    if(rc != OMPI_SUCCESS) {
        mpool->rcache->rcache_delete(mpool->rcache, rdma_reg);
        OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
        OMPI_FREE_LIST_RETURN(&mpool_rdma->reg_list, item);
        return rc;
    }

    *reg = rdma_reg;
    (*reg)->ref_count++;
    OPAL_THREAD_UNLOCK(&mpool->rcache->lock);

    /* Cleanup any vmas that we have deferred deletion on */
    mpool->rcache->rcache_clean(mpool->rcache);
    return OMPI_SUCCESS;
}


/**
  * realloc function
  */
void* mca_mpool_rdma_realloc(mca_mpool_base_module_t *mpool, void *addr,
    size_t size, mca_mpool_base_registration_t **reg)
{
    mca_mpool_base_registration_t *old_reg  = *reg;
    void *new_mem = mca_mpool_rdma_alloc(mpool, size, 0, old_reg->flags, reg);
    memcpy(new_mem, addr, old_reg->bound - old_reg->base + 1);
    mca_mpool_rdma_free(mpool, addr, old_reg);

    return new_mem;
}

/**
  * free function
  */
void mca_mpool_rdma_free(mca_mpool_base_module_t *mpool, void *addr,
                         mca_mpool_base_registration_t *registration)
{
    void *alloc_base = registration->alloc_base;
    mca_mpool_rdma_deregister(mpool, registration);
    free(alloc_base);
}

int mca_mpool_rdma_find(struct mca_mpool_base_module_t *mpool, void *addr,
        size_t size, mca_mpool_base_registration_t **reg)
{
    mca_mpool_rdma_module_t *mpool_rdma = (mca_mpool_rdma_module_t*)mpool;
    int rc;
    unsigned char *base, *bound;

    base = (unsigned char *) down_align_addr(addr, mca_mpool_base_page_size_log);
    bound = (unsigned char *) up_align_addr((void*)((char*) addr + size - 1),
             mca_mpool_base_page_size_log);

    OPAL_THREAD_LOCK(&mpool->rcache->lock);
    rc = mpool->rcache->rcache_find(mpool->rcache, addr, size, reg);
    if(*reg != NULL &&
            (mca_mpool_rdma_component.leave_pinned ||
             ((*reg)->flags & MCA_MPOOL_FLAGS_PERSIST) ||
             ((*reg)->base == base && (*reg)->bound == bound))) {
        assert(((void*)(*reg)->bound) >= addr);
        if(0 == (*reg)->ref_count &&
                mca_mpool_rdma_component.leave_pinned) {
            opal_list_remove_item(&mpool_rdma->lru_list,
                    (opal_list_item_t*)(*reg));
        }
        mpool_rdma->stat_cache_found++;
        (*reg)->ref_count++;
    } else {
        mpool_rdma->stat_cache_notfound++;
    }
    OPAL_THREAD_UNLOCK(&mpool->rcache->lock);

    return rc;
}

static inline bool registration_is_cachebale(mca_mpool_base_registration_t *reg)
{
     return !(reg->flags &
             (MCA_MPOOL_FLAGS_CACHE_BYPASS |
              MCA_MPOOL_FLAGS_PERSIST |
              MCA_MPOOL_FLAGS_INVALID));
}

int mca_mpool_rdma_deregister(struct mca_mpool_base_module_t *mpool,
                            mca_mpool_base_registration_t *reg)
{
    mca_mpool_rdma_module_t *mpool_rdma = (mca_mpool_rdma_module_t*)mpool;
    int rc = OMPI_SUCCESS;
    assert(reg->ref_count > 0);

    OPAL_THREAD_LOCK(&mpool->rcache->lock);
    reg->ref_count--;
    if(reg->ref_count > 0) {
        OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
        return OMPI_SUCCESS;
    }
    if(mca_mpool_rdma_component.leave_pinned && registration_is_cachebale(reg))
    {
        /* if leave_pinned is set don't deregister memory, but put it
         * on LRU list for future use */
        opal_list_append(&mpool_rdma->lru_list, (opal_list_item_t*)reg);
    } else {
	/* Remove from rcache first */
	if(!(reg->flags & MCA_MPOOL_FLAGS_CACHE_BYPASS))
	    mpool->rcache->rcache_delete(mpool->rcache, reg);

	/* Drop the rcache lock before deregistring the memory */
	OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
        rc = dereg_mem(mpool, reg);
	OPAL_THREAD_LOCK(&mpool->rcache->lock);

        if(OMPI_SUCCESS == rc) {
            OMPI_FREE_LIST_RETURN(&mpool_rdma->reg_list,
                    (ompi_free_list_item_t*)reg);
        }
    }
    OPAL_THREAD_UNLOCK(&mpool->rcache->lock);

    /* Cleanup any vmas that we have deferred deletion on */
    mpool->rcache->rcache_clean(mpool->rcache);

    return rc;
}

#define RDMA_MPOOL_NREGS 100

int mca_mpool_rdma_release_memory(struct mca_mpool_base_module_t *mpool,
        void *base, size_t size)
{
    mca_mpool_rdma_module_t *mpool_rdma = (mca_mpool_rdma_module_t*)mpool;
    mca_mpool_base_registration_t *reg;
    mca_mpool_base_registration_t *regs[RDMA_MPOOL_NREGS];
    int reg_cnt, i, err = 0;

    OPAL_THREAD_LOCK(&mpool->rcache->lock);
    do {
        reg_cnt = mpool->rcache->rcache_find_all(mpool->rcache, base, size,
                regs, RDMA_MPOOL_NREGS);

        for(i = 0; i < reg_cnt; i++) {
            reg = regs[i];

            reg->flags |= MCA_MPOOL_FLAGS_INVALID;
            if(reg->ref_count) {
                /* memory is being freed, but there are registration in use that
                 * covers the memory. This can happen even in a correct program,
                 * but may also be an user error. We can't tell. Mark the
                 * registration as invalid. It will not be used any more and
                 * will be unregistered when ref_count will become zero */
                err++; /* tell caller that something was wrong */
                continue;
            }

            opal_list_remove_item(&mpool_rdma->lru_list,(opal_list_item_t*)reg);
            opal_list_append(&mpool_rdma->gc_list, (opal_list_item_t*)reg);
        }
    } while(reg_cnt == RDMA_MPOOL_NREGS);

    OPAL_THREAD_UNLOCK(&mpool->rcache->lock);

    return err ? OMPI_ERROR : OMPI_SUCCESS;
}

void mca_mpool_rdma_finalize(struct mca_mpool_base_module_t *mpool)
{
    mca_mpool_rdma_module_t *mpool_rdma = (mca_mpool_rdma_module_t*)mpool;
    mca_mpool_base_registration_t *reg;
    mca_mpool_base_registration_t *regs[RDMA_MPOOL_NREGS];
    int reg_cnt, i;
    int rc;

    /* Statistic */
    if(true == mca_mpool_rdma_component.print_stats) {
        opal_output(0, "%s rdma: stats "
                "(hit/miss/found/not found/evicted): %d/%d/%d/%d/%d\n",
                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                mpool_rdma->stat_cache_hit, mpool_rdma->stat_cache_miss,
                mpool_rdma->stat_cache_found, mpool_rdma->stat_cache_notfound,
                mpool_rdma->stat_evicted);
    }

    OPAL_THREAD_LOCK(&mpool->rcache->lock);
    if(!opal_list_is_empty(&mpool_rdma->gc_list))
        do_unregistration_gc(mpool);
    do {
        reg_cnt = mpool->rcache->rcache_find_all(mpool->rcache, 0, (size_t)-1,
                regs, RDMA_MPOOL_NREGS);

        for(i = 0; i < reg_cnt; i++) {
            reg = regs[i];

            if(reg->ref_count) {
                reg->ref_count = 0; /* otherway dereg will fail on assert */
            } else if (mca_mpool_rdma_component.leave_pinned) {
                opal_list_remove_item(&mpool_rdma->lru_list,
                        (opal_list_item_t*)reg);
            }

	    /* Remove from rcache first */
            mpool->rcache->rcache_delete(mpool->rcache, reg);

	    /* Drop lock before deregistering memory */
	    OPAL_THREAD_UNLOCK(&mpool->rcache->lock);
	    rc = dereg_mem(mpool, reg);
	    OPAL_THREAD_LOCK(&mpool->rcache->lock);

            if(rc != OMPI_SUCCESS) {
		/* Potentially lose track of registrations
		   do we have to put it back? */
                continue;
            }

            OMPI_FREE_LIST_RETURN(&mpool_rdma->reg_list,
                    (ompi_free_list_item_t*)reg);
        }
    } while(reg_cnt == RDMA_MPOOL_NREGS);

    OBJ_DESTRUCT(&mpool_rdma->lru_list);
    OBJ_DESTRUCT(&mpool_rdma->gc_list);
    OBJ_DESTRUCT(&mpool_rdma->reg_list);
    OPAL_THREAD_UNLOCK(&mpool->rcache->lock);

    /* Cleanup any vmas that we have deferred deletion on */
    mpool->rcache->rcache_clean(mpool->rcache);

}

int mca_mpool_rdma_ft_event(int state) {
    if(OPAL_CRS_CHECKPOINT == state) {
        ;
    }
    else if(OPAL_CRS_CONTINUE == state) {
        ;
    }
    else if(OPAL_CRS_RESTART == state ||
            OPAL_CRS_RESTART_PRE == state) {
        ;
    }
    else if(OPAL_CRS_TERM == state ) {
        ;
    }
    else {
        ;
    }

    return OMPI_SUCCESS;
}
