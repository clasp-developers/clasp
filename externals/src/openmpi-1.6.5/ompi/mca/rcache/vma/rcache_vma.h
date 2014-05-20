/* -*- Mode: C; c-basic-offset:4 ; -*- */
/**
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
  *
  * Copyright (c) 2006      Voltaire. All rights reserved.
  * Copyright (c) 2009      IBM Corporation.  All rights reserved.
  *
  * $COPYRIGHT$
  * 
  * Additional copyrights may follow
  * 
  * $HEADER$
  */
/**
  * @file
  * Description of the Registration Cache framework
  */
#ifndef MCA_RCACHE_VMA_H
#define MCA_RCACHE_VMA_H
#include "ompi_config.h"
#include "opal/mca/mca.h"
#include "opal/class/opal_list.h" 
#include "ompi/class/ompi_rb_tree.h"
#include "ompi/mca/rcache/rcache.h"

struct mca_rcache_vma_module_t { 
    mca_rcache_base_module_t base;
    ompi_rb_tree_t rb_tree;
    opal_list_t vma_list;
    opal_list_t vma_delete_list;
    size_t reg_cur_cache_size;
};
typedef struct mca_rcache_vma_module_t mca_rcache_vma_module_t; 


struct mca_rcache_vma_component_t { 
    mca_rcache_base_component_t super; 
};
typedef struct mca_rcache_vma_component_t mca_rcache_vma_component_t; 

OMPI_DECLSPEC extern mca_rcache_vma_component_t mca_rcache_vma_component;



void mca_rcache_vma_module_init(mca_rcache_vma_module_t* rcache);

int mca_rcache_vma_find(mca_rcache_base_module_t* rcache, void* addr,
        size_t size, mca_mpool_base_registration_t **reg);

int mca_rcache_vma_find_all(mca_rcache_base_module_t* rcache, void* addr,
         size_t size,  mca_mpool_base_registration_t **regs, int reg_cnt);

int mca_rcache_vma_insert(struct mca_rcache_base_module_t* rcache,
        mca_mpool_base_registration_t* registration, size_t limit);

int mca_rcache_vma_delete(struct mca_rcache_base_module_t* rcache,
        mca_mpool_base_registration_t* registration);

/* It is not safe to call mca_rcache_vma_clean with the rcache lock held */
int mca_rcache_vma_clean(struct mca_rcache_base_module_t* rcache);
/* Destroy vma objects which are on the deferred delete list. These were placed
   on the list earlier when the rcache lock was held and it was not safe to
   destory them. They should not be linked into any other structure anymore except
   the vma_list_delete list */

/**
  * init/finalize
  */

void mca_rcache_vma_module_init(mca_rcache_vma_module_t *rcache);

void mca_rcache_vma_finalize(struct mca_rcache_base_module_t*);

#endif /* MCA_RCACHE_VMA_H */


