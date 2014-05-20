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
 * Copyright (c) 2006      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "allocator_basic.h"
#include "ompi/constants.h"



mca_allocator_base_component_t mca_allocator_basic_component = {
                                                                                                         
  /* First, the mca_base_module_t struct containing meta information
     about the module itself */
                                                                                                         
  {
    MCA_ALLOCATOR_BASE_VERSION_2_0_0,
                                                                                                         
    "basic", /* MCA module name */
    OMPI_MAJOR_VERSION,
    OMPI_MINOR_VERSION,
    OMPI_RELEASE_VERSION,
    mca_allocator_basic_component_open,  /* module open */
    mca_allocator_basic_component_close  /* module close */
  },
  {
      /* The component is checkpoint ready */
      MCA_BASE_METADATA_PARAM_CHECKPOINT
  },
  mca_allocator_basic_component_init
};


OBJ_CLASS_INSTANCE(
    mca_allocator_basic_segment_t,
    ompi_free_list_item_t,
    NULL,
    NULL);


int mca_allocator_basic_component_open(void)
{
    return OMPI_SUCCESS;
}


int mca_allocator_basic_component_close(void)
{
    return OMPI_SUCCESS;
}


/**
  *
  */

mca_allocator_base_module_t* mca_allocator_basic_component_init(
    bool enable_mpi_threads,
    mca_allocator_base_component_segment_alloc_fn_t segment_alloc,
    mca_allocator_base_component_segment_free_fn_t segment_free, 
    struct mca_mpool_base_module_t* mpool)
{
    mca_allocator_basic_module_t *module = (mca_allocator_basic_module_t *)
                                            malloc(sizeof(mca_allocator_basic_module_t));
    if (NULL == module) {
        return NULL;
    }

    module->super.alc_alloc = mca_allocator_basic_alloc;
    module->super.alc_realloc = mca_allocator_basic_realloc;
    module->super.alc_free = mca_allocator_basic_free;
    module->super.alc_compact = mca_allocator_basic_compact;
    module->super.alc_finalize = mca_allocator_basic_finalize;
    module->super.alc_mpool = mpool;
    module->seg_alloc = segment_alloc;
    module->seg_free = segment_free;
    OBJ_CONSTRUCT(&module->seg_list, opal_list_t);
    OBJ_CONSTRUCT(&module->seg_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&module->seg_descriptors, ompi_free_list_t);
 
    ompi_free_list_init_new(&module->seg_descriptors,
        sizeof(mca_allocator_basic_segment_t),
        opal_cache_line_size,
        OBJ_CLASS(mca_allocator_basic_segment_t),
        0,opal_cache_line_size,
        0,  /* initial size */
        -1, /* maximum size */
        16, /* increment to grow by */
        NULL);

    return &module->super;
}
                                                                                                         
/**
  * Combine adjacent segments together.
 */

static void mca_allocator_basic_combine_prev(
    mca_allocator_basic_module_t* module, 
    mca_allocator_basic_segment_t* seg)
{
    opal_list_item_t* item = opal_list_get_prev(seg);
    if(item != opal_list_get_begin(&module->seg_list)) {
        mca_allocator_basic_segment_t *prev = (mca_allocator_basic_segment_t*)item;
        if(prev->seg_addr + prev->seg_size == seg->seg_addr) {
            prev->seg_size += seg->seg_size;
            opal_list_remove_item(&module->seg_list, &seg->seg_item.super);
            OMPI_FREE_LIST_RETURN(&module->seg_descriptors, &seg->seg_item);
            return;
        }
    }
}

static void mca_allocator_basic_combine_next(
    mca_allocator_basic_module_t* module, 
    mca_allocator_basic_segment_t* seg)
{
    opal_list_item_t *item = opal_list_get_next(seg);
    if(item != opal_list_get_end(&module->seg_list)) {
        mca_allocator_basic_segment_t *next = (mca_allocator_basic_segment_t*)item;
        if(seg->seg_addr + seg->seg_size == next->seg_addr) {
            next->seg_addr = seg->seg_addr;
            next->seg_size += seg->seg_size;
            opal_list_remove_item(&module->seg_list, &seg->seg_item.super);
            OMPI_FREE_LIST_RETURN(&module->seg_descriptors, &seg->seg_item);
            return;
        }
    }
}

/**
  * Accepts a request for memory in a specific region defined by the
  * mca_allocator_basic_options_t struct and returns a pointer to memory in that
  * region or NULL if there was an error
  *
  * @param mem A pointer to the appropriate struct for the area of memory.
  * @param size The size of the requested area of memory
  *
  * @retval Pointer to the area of memory if the allocation was successful
  * @retval NULL if the allocation was unsuccessful
  */

void *mca_allocator_basic_alloc(
    mca_allocator_base_module_t * base, 
    size_t size,
    size_t align, 
    mca_mpool_base_registration_t** registration)
{
    mca_allocator_basic_module_t* module = (mca_allocator_basic_module_t*)base;
    mca_allocator_basic_segment_t* seg;
    ompi_free_list_item_t* item;
    unsigned char* addr;
    size_t allocated_size;
    OPAL_THREAD_LOCK(&module->seg_lock);

    /* add the size of the header into the amount we need to request */
    size += sizeof(size_t);
    /* normalize size so we don't end up with seg_addr on an odd boundary */
    size += sizeof(size_t) - (size & (sizeof(size_t) - 1));
    /* search the list for a segment of the required size */
    for(item =  (ompi_free_list_item_t*) opal_list_get_first(&module->seg_list);
        item != (ompi_free_list_item_t*) opal_list_get_end(&module->seg_list);
        item =  (ompi_free_list_item_t*) opal_list_get_next(&item->super)) {
        seg = (mca_allocator_basic_segment_t*)item;

        /* split the segment */
        if(seg->seg_size > size) {
            addr = seg->seg_addr;
            seg->seg_addr += size;
            seg->seg_size -= size;
            OPAL_THREAD_UNLOCK(&module->seg_lock);
            *(size_t*)addr = size;
            return addr+sizeof(size_t);
        } else if (seg->seg_size == size) {
            addr = seg->seg_addr;
            opal_list_remove_item(&module->seg_list, &item->super);
            OMPI_FREE_LIST_RETURN(&module->seg_descriptors, item);
            OPAL_THREAD_UNLOCK(&module->seg_lock);
            *(size_t*)addr = size;
            return addr+sizeof(size_t);
        }
    }

    /* request additional block */
    allocated_size = (unsigned char)size;
    if(NULL == (addr = (unsigned char *)module->seg_alloc(module->super.alc_mpool, &allocated_size, registration))) {
        OPAL_THREAD_UNLOCK(&module->seg_lock);
        return NULL;
    }

    /* create a segment for any extra allocation */
    if(allocated_size > size) {
        int rc;
        OMPI_FREE_LIST_GET(&module->seg_descriptors, item, rc);
        if(rc != OMPI_SUCCESS) {
            OPAL_THREAD_UNLOCK(&module->seg_lock);
            return NULL;
        }
        seg = (mca_allocator_basic_segment_t*)item;
        seg->seg_addr = addr + size;
        seg->seg_size = allocated_size - size;
        opal_list_append(&module->seg_list, &item->super);
    }

    *(size_t*)addr = size;
    OPAL_THREAD_UNLOCK(&module->seg_lock);
    return addr+sizeof(size_t);
}


/**
  * Attempts to resize the passed region of memory into a larger or a smaller
  * region. If it is unsuccessful, it will return NULL and the passed area of
  * memory will be untouched.
  *
  * @param mem A pointer to the appropriate struct for the area of
  * memory.
  * @param size The size of the requested area of memory
  * @param ptr A pointer to the region of memory to be resized
  *
  * @retval Pointer to the area of memory if the reallocation was successful
  * @retval NULL if the allocation was unsuccessful
  *
  */

void * mca_allocator_basic_realloc(
    mca_allocator_base_module_t * base, 
    void * ptr, 
    size_t size, 
    mca_mpool_base_registration_t** registration)
{
    unsigned char* addr = ((unsigned char*)ptr) - sizeof(size_t);
    size_t alloc_size = *(size_t*)addr;
    if(size <= alloc_size)
        return ptr;
    addr = (unsigned char *)mca_allocator_basic_alloc(base,size,0,registration);
    if(addr == NULL)
        return addr;
    memcpy(addr,ptr,alloc_size);
    mca_allocator_basic_free(base,ptr);
    return addr;
}


/**
  * Frees the passed region of memory
  *
  * @param mem A pointer to the appropriate struct for the area of
  * memory.
  * @param ptr A pointer to the region of memory to be freed
  *
  * @retval None
  *
  */
void mca_allocator_basic_free(
    mca_allocator_base_module_t * base,
    void * ptr)
{
    mca_allocator_basic_module_t* module = (mca_allocator_basic_module_t*)base;
    mca_allocator_basic_segment_t* seg;
    ompi_free_list_item_t *item;
    unsigned char* addr = (unsigned char*)ptr - sizeof(size_t);
    size_t size = *(size_t*)addr;
    int rc;
    OPAL_THREAD_LOCK(&module->seg_lock);

    /* maintain the free list in sorted order by address */
    for(item =  (ompi_free_list_item_t*) opal_list_get_first(&module->seg_list);
        item != (ompi_free_list_item_t*) opal_list_get_end(&module->seg_list);
        item =  (ompi_free_list_item_t*) opal_list_get_next((&item->super))) {
        seg = (mca_allocator_basic_segment_t*)item;

        if (seg->seg_addr < addr) {

            /* can we grow the current entry */
            if(seg->seg_addr + seg->seg_size == addr) {
                seg->seg_size += size;
                mca_allocator_basic_combine_next(module, seg);
                OPAL_THREAD_UNLOCK(&module->seg_lock);
                return;
            } 
            /* otherwise continue to check next larger entry */

        } else  {

            /* can this be combined with current entry */
            if(addr + size == seg->seg_addr) {
                seg->seg_addr = addr;
                seg->seg_size += size;
                mca_allocator_basic_combine_prev(module, seg);
                OPAL_THREAD_UNLOCK(&module->seg_lock);
                return;

            /* insert before larger entry */
            } else {
                mca_allocator_basic_segment_t* new_seg;
                OMPI_FREE_LIST_GET(&module->seg_descriptors, item, rc);
                if(rc != OMPI_SUCCESS) {
                    OPAL_THREAD_UNLOCK(&module->seg_lock);
                    return;
                }
                new_seg = (
mca_allocator_basic_segment_t*)item;
                new_seg->seg_addr = addr;
                new_seg->seg_size = size;
                opal_list_insert_pos(&module->seg_list, &seg->seg_item.super, &item->super);
                OPAL_THREAD_UNLOCK(&module->seg_lock);
                return;
            }
        }
    }

    /* append to the end of the list */
    OMPI_FREE_LIST_GET(&module->seg_descriptors, item, rc);
    if(rc != OMPI_SUCCESS) {
        OPAL_THREAD_UNLOCK(&module->seg_lock);
        return;
    }
    seg = (mca_allocator_basic_segment_t*)item;
    seg->seg_addr = addr;
    seg->seg_size = size;
    opal_list_append(&module->seg_list, &item->super);
    OPAL_THREAD_UNLOCK(&module->seg_lock);
}


/**
  * Frees all the memory from all the basics back to the system. Note that
  * this function only frees memory that was previously freed with
  * mca_allocator_basic_free().
  *
  * @param mem A pointer to the appropriate struct for the area of
  * memory.
  *
  * @retval None
  *
  */

int mca_allocator_basic_compact(mca_allocator_base_module_t * mem)
{
    return OMPI_SUCCESS;
}

/**
  * Cleanup all resources held by this allocator.
  *
  * @param mem A pointer to the appropriate struct for the area of
  * memory.
  *
  * @retval None
  *
  */

int mca_allocator_basic_finalize(mca_allocator_base_module_t * base)
{
    mca_allocator_basic_module_t* module = (mca_allocator_basic_module_t*)base;
    OBJ_DESTRUCT(&module->seg_list);
    OBJ_DESTRUCT(&module->seg_lock);
    OBJ_DESTRUCT(&module->seg_descriptors);
    free(module);
    return OMPI_SUCCESS;
}


