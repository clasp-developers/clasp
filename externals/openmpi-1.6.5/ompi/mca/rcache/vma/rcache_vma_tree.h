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
#ifndef MCA_RCACHE_VMA_TREE_H
#define MCA_RCACHE_VMA_TREE_H
#include "opal/mca/mca.h"
#include "ompi/mca/mpool/mpool.h"
#include "rcache_vma.h"
/*
 * Data structures for the tree of allocated memory
 */

struct mca_rcache_vma_reg_list_item_t
{
    opal_list_item_t super;
    mca_mpool_base_registration_t *reg;
};
typedef struct mca_rcache_vma_reg_list_item_t mca_rcache_vma_reg_list_item_t;
OBJ_CLASS_DECLARATION(mca_rcache_vma_reg_list_item_t);

/**
 * The item in the vma_tree itself
 */
struct mca_rcache_vma_t
{
    opal_list_item_t super;          /**< the parent class */
    uintptr_t start;                 /**< the base of the memory range */
    uintptr_t end;                   /**< the bound of the memory range */
    opal_list_t reg_list;            /**< list of regs on this vma */
    opal_list_t reg_delete_list;     /**< delayed deletions list for regs on this vma */
    mca_rcache_vma_module_t *rcache; /**< pointer to rcache vma belongs to */
};
typedef struct mca_rcache_vma_t mca_rcache_vma_t;

OBJ_CLASS_DECLARATION(mca_rcache_vma_t);


/*
 * initialize the vma tree
 */
int mca_rcache_vma_tree_init(mca_rcache_vma_module_t* rcache); 

/**
 *  Returns the item in the vma tree  
 */
mca_mpool_base_registration_t* mca_rcache_vma_tree_find(
                                           mca_rcache_vma_module_t* rcache, 
                                           unsigned char* base,
                                           unsigned char *bound
                                           );
/**
 * Returns all registration that overlaps given memory region
 */
int mca_rcache_vma_tree_find_all(
        mca_rcache_vma_module_t *vma_rcache, unsigned char *base,
        unsigned char *bound, mca_mpool_base_registration_t **regs,
        int reg_cnt);

/* 
 * insert an item in the vma tree 
 */ 
int mca_rcache_vma_tree_insert(mca_rcache_vma_module_t* rcache,
        mca_mpool_base_registration_t* reg, size_t limit);

/* 
 * remove an item from the vma tree 
 */
int mca_rcache_vma_tree_delete( 
                              mca_rcache_vma_module_t* rcache, 
                              mca_mpool_base_registration_t* reg
                              ); 

/* 
 * Destroy a vma
 * Do not call this function with rcache lock as it can deadlock 
 */
void mca_rcache_vma_destroy(mca_rcache_vma_t *vma);

#endif /* MCA_RCACHE_VMA_TREE_H */

