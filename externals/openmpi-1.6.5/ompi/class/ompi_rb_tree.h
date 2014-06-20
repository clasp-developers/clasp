/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

/** @file
 *
 *     A red black tree
 */

#ifndef OMPI_RB_TREE_H
#define OMPI_RB_TREE_H

#include "ompi_config.h"
#include <stdlib.h>
#include "ompi/constants.h"
#include "opal/class/opal_object.h"
#include "ompi/class/ompi_free_list.h"

BEGIN_C_DECLS
/*
 * Data structures and datatypes
 */

/**
  * red and black enum
  */
typedef enum {RED, BLACK} ompi_rb_tree_nodecolor_t;

/**
  * node data structure
  */
struct ompi_rb_tree_node_t
{
    ompi_free_list_item_t super;             /**< the parent class */
    ompi_rb_tree_nodecolor_t color;     /**< the node color */
    struct ompi_rb_tree_node_t * parent;/**< the parent node, can be NULL */
    struct ompi_rb_tree_node_t * left;  /**< the left child - can be nill */
    struct ompi_rb_tree_node_t * right; /**< the right child - can be nill */
    void *key;                          /**< a pointer to the key */
    void *value;                        /**< a pointer to the value */
};
typedef struct ompi_rb_tree_node_t ompi_rb_tree_node_t;

/**
  * the compare function typedef. This function is used to compare 2 nodes.
  */
typedef int (*ompi_rb_tree_comp_fn_t)(void *key1, void *key2);

/**
  * the data structure that holds all the needed information about the tree.
  */
struct ompi_rb_tree_t {
    opal_object_t parent;           /**< the parent class */
    /* this root pointer doesn't actually point to the root of the tree.
     * rather, it points to a sentinal node who's left branch is the real
     * root of the tree. This is done to eliminate special cases */
    ompi_rb_tree_node_t * root_ptr;/**< a pointer to the root of the tree */
    ompi_rb_tree_node_t * nill;     /**< the nill sentinal node */
    ompi_rb_tree_comp_fn_t comp;    /**< the compare function */
    ompi_free_list_t free_list;   /**< the free list to get the memory from */
    size_t tree_size;                  /**< the size of the tree */
};
typedef struct ompi_rb_tree_t ompi_rb_tree_t;

/** declare the tree node as a class */
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_rb_tree_node_t);
/** declare the tree as a class */
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_rb_tree_t);

/* Function pointers for map traversal function */
/**
  * this function is used for the ompi_rb_tree_traverse function.
  * it is passed a pointer to the value for each node and, if it returns
  * a one, the action function is called on that node. Otherwise, the node is ignored.
  */
typedef int (*ompi_rb_tree_condition_fn_t)(void *);
/**
  * this function is uused for the user to perform any action on the passed
  * values. The first argument is the key and the second is the value.
  * note that this function SHOULD NOT modify the keys, as that would
  * mess up the tree.
  */
typedef void (*ompi_rb_tree_action_fn_t)(void *, void *);

/*
 * Public function protoypes
 */

/**
  * the construct function. creates the free list to get the nodes from
  *
  * @param object the tree that is to be used
  *
  * @retval NONE
  */
OMPI_DECLSPEC void ompi_rb_tree_construct(opal_object_t * object);

/**
  * the destruct function. tries to free the tree and destroys the free list
  *
  * @param object the tree object
  */
OMPI_DECLSPEC void ompi_rb_tree_destruct(opal_object_t * object);

/**
  * the function creates a new tree
  *
  * @param tree a pointer to an allocated area of memory for the main
  *  tree data structure.
  * @param comp a pointer to the function to use for comaparing 2 nodes
  *
  * @retval OMPI_SUCCESS if it is successful
  * @retval OMPI_ERR_TEMP_OUT_OF_RESOURCE if unsuccessful
  */
OMPI_DECLSPEC int ompi_rb_tree_init(ompi_rb_tree_t * tree, ompi_rb_tree_comp_fn_t comp);


/**
  * inserts a node into the tree
  *
  * @param tree a pointer to the tree data structure
  * @param key the key for the node
  * @param value the value for the node
  *
  * @retval OMPI_SUCCESS
  * @retval OMPI_ERR_TEMP_OUT_OF_RESOURCE if unsuccessful
  */
OMPI_DECLSPEC int ompi_rb_tree_insert(ompi_rb_tree_t *tree, void * key, void * value);

/**
  * finds a value in the tree based on the passed key using passed
  * compare function
  *
  * @param tree a pointer to the tree data structure
  * @param key a pointer to the key
  * @param compare function
  *
  * @retval pointer to the value if found
  * @retval NULL if not found
  */
OMPI_DECLSPEC void * ompi_rb_tree_find_with(ompi_rb_tree_t *tree, void *key, ompi_rb_tree_comp_fn_t compfn);

/**
  * finds a value in the tree based on the passed key
  *
  * @param tree a pointer to the tree data structure
  * @param key a pointer to the key
  *
  * @retval pointer to the value if found
  * @retval NULL if not found
  */
static inline void * ompi_rb_tree_find(ompi_rb_tree_t *tree, void *key)
{
        return ompi_rb_tree_find_with(tree, key, tree->comp);
}

/**
  * deletes a node based on its key
  *
  * @param tree a pointer to the tree data structure
  * @param key a pointer to the key
  *
  * @retval OMPI_SUCCESS if the node is found and deleted
  * @retval OMPI_ERR_NOT_FOUND if the node is not found
  */
OMPI_DECLSPEC int ompi_rb_tree_delete(ompi_rb_tree_t *tree, void *key);

/**
  * frees all the nodes on the tree
  *
  * @param tree a pointer to the tree data structure
  *
  * @retval OMPI_SUCCESS
  */
OMPI_DECLSPEC int ompi_rb_tree_destroy(ompi_rb_tree_t *tree);

/**
  * traverses the entire tree, performing the cond function on each of the
  * values and if it returns one it performs the action function on the values
  *
  * @param tree a pointer to the tree
  * @param cond a pointer to the condition function
  * @param action a pointer to the action function
  *
  * @retval OMPI_SUCCESS
  * @retval OMPI_ERROR if there is an error
  */
OMPI_DECLSPEC int ompi_rb_tree_traverse(ompi_rb_tree_t *tree,
                          ompi_rb_tree_condition_fn_t cond,
                          ompi_rb_tree_action_fn_t action);

/**
  * returns the size of the tree
  *
  * @param tree a pointer to the tree data structure
  *
  * @retval int the nuber of items on the tree
  */
OMPI_DECLSPEC int ompi_rb_tree_size(ompi_rb_tree_t *tree);

END_C_DECLS
#endif /* OMPI_RB_TREE_H */

