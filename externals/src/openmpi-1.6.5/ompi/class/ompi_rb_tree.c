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
 */
/*
 * @file
 */

#include "ompi_config.h"

#include "ompi/class/ompi_rb_tree.h"

/* declare the instance of the classes  */
OBJ_CLASS_INSTANCE(ompi_rb_tree_node_t, ompi_free_list_item_t, NULL, NULL);
OBJ_CLASS_INSTANCE(ompi_rb_tree_t, opal_object_t, ompi_rb_tree_construct,
                   ompi_rb_tree_destruct);

/* Private functions */
static void btree_insert(ompi_rb_tree_t *tree, ompi_rb_tree_node_t * node);
static void btree_delete_fixup(ompi_rb_tree_t *tree, ompi_rb_tree_node_t * x);
static ompi_rb_tree_node_t * btree_successor(ompi_rb_tree_t * tree,
                                             ompi_rb_tree_node_t * node);
static ompi_rb_tree_node_t * ompi_rb_tree_find_node(ompi_rb_tree_t *tree, void *key);
static void left_rotate(ompi_rb_tree_t *tree, ompi_rb_tree_node_t * x);
static void right_rotate(ompi_rb_tree_t *tree, ompi_rb_tree_node_t * x);
static void inorder_destroy(ompi_rb_tree_t *tree, ompi_rb_tree_node_t * node);
static void inorder_traversal(ompi_rb_tree_t *tree,
                              ompi_rb_tree_condition_fn_t cond,
                              ompi_rb_tree_action_fn_t action,
                              ompi_rb_tree_node_t * node);



/* constructor */
void ompi_rb_tree_construct(opal_object_t * object)
{
    ompi_rb_tree_t * tree = (ompi_rb_tree_t *) object;
    tree->root_ptr = NULL;
    OBJ_CONSTRUCT(&(tree->free_list), ompi_free_list_t);
    ompi_free_list_init_new(&(tree->free_list), sizeof(ompi_rb_tree_node_t),
            opal_cache_line_size, OBJ_CLASS(ompi_rb_tree_node_t),
            0,opal_cache_line_size,
            0, -1 , 128, NULL);
}

/* the destructor function */
void ompi_rb_tree_destruct(opal_object_t * object)
{
    if(NULL != ((ompi_rb_tree_t *)object)->root_ptr) {
        ompi_rb_tree_destroy((ompi_rb_tree_t *) object);
    }
    OBJ_DESTRUCT(&(((ompi_rb_tree_t *)object)->free_list));
    return;
}

/* Create the tree */
int ompi_rb_tree_init(ompi_rb_tree_t * tree,
                      ompi_rb_tree_comp_fn_t comp)
{
    int rc;

    ompi_free_list_item_t * node;
    /* we need to get memory for the root pointer from the free list */
    OMPI_FREE_LIST_GET(&(tree->free_list), node, rc);
    tree->root_ptr = (ompi_rb_tree_node_t *) node;
    if (OMPI_SUCCESS != rc) {
        return rc;
    }

    OMPI_FREE_LIST_GET(&(tree->free_list), node, rc);
    if (OMPI_SUCCESS != rc) {
        return rc;
    }
    tree->nill = (ompi_rb_tree_node_t *) node;
    /* initialize tree->nill */
    tree->nill->color = BLACK;
    tree->nill->left = tree->nill;
    tree->nill->right = tree->nill;
    tree->nill->parent = tree->nill;

    /* initialize the 'root' pointer */
    tree->root_ptr->left = tree->nill;
    tree->root_ptr->right = tree->nill;
    tree->root_ptr->parent = tree->nill;
    tree->root_ptr->color = BLACK;

    tree->comp = comp;

    /* set the tree size to zero */
    tree->tree_size = 0;

    return(OMPI_SUCCESS);
}


/* This inserts a node into the tree based on the passed values. */
int ompi_rb_tree_insert(ompi_rb_tree_t *tree, void * key, void * value)
{
    ompi_rb_tree_node_t * y;
    ompi_rb_tree_node_t * node;
    ompi_free_list_item_t * item;
    int rc;

    /* get the memory for a node */
    OMPI_FREE_LIST_GET(&(tree->free_list), item, rc);
    if (OMPI_SUCCESS != rc) {
        return rc;
    }
    node = (ompi_rb_tree_node_t *) item;
    /* insert the data into the node */
    node->key = key;
    node->value = value;

    /* insert the node into the tree */
    btree_insert(tree, node);

    /*do the rotations */
    /* usually one would have to check for NULL, but because of the sentinal,
     * we don't have to   */
    while (node->parent->color == RED) {
        if (node->parent == node->parent->parent->left) {
            y = node->parent->parent->right;
            if (y->color == RED) {
                node->parent->color = BLACK;
                y->color = BLACK;
                node->parent->parent->color = RED;
                node = node->parent->parent;
            } else {
                if (node == node->parent->right) {
                    node = node->parent;
                    left_rotate(tree, node);
                }
                node->parent->color = BLACK;
                node->parent->parent->color = RED;
                right_rotate(tree, node->parent->parent);
            }
        } else {
            y = node->parent->parent->left;
            if (y->color == RED) {
                node->parent->color = BLACK;
                y->color = BLACK;
                node->parent->parent->color = RED;
                node = node->parent->parent;
            } else {
                if (node == node->parent->left) {
                    node = node->parent;
                    right_rotate(tree, node);
                }
                node->parent->color = BLACK;
                node->parent->parent->color = RED;
                left_rotate(tree, node->parent->parent);
            }
        }
    }
    /* after the rotations the root is black */
    tree->root_ptr->left->color = BLACK;
    return OMPI_SUCCESS;
}

/* Finds the node in the tree based on the key */
void * ompi_rb_tree_find_with(ompi_rb_tree_t *tree, void *key,
        ompi_rb_tree_comp_fn_t compfn)
{
    ompi_rb_tree_node_t * node;
    int compvalue;

    node = tree->root_ptr->left;
    while (node != tree->nill) {
        compvalue = compfn(key, node->key);
        /* if the result of the comparison function is 0, we found it */
        if (compvalue == 0) {
            return(node->value);
        }
        /* else if it is less than 0, go left, else right */
        node = ((compvalue < 0) ? node->left : node->right);
    }
    /* if we didn't find anything, return NULL */
    return(NULL);
}

/* Finds the node in the tree based on the key and returns a pointer
 * to the node. This is a bit a code duplication, but this has to be fast
 * so we go ahead with the duplication */
static ompi_rb_tree_node_t * ompi_rb_tree_find_node(ompi_rb_tree_t *tree, void *key)
{
    ompi_rb_tree_node_t * node;
    int compvalue;

    node = tree->root_ptr->left;
    while (node != tree->nill) {
        compvalue = tree->comp(key, node->key);
        /* if the result of the comparison function is 0, we found it */
        if (compvalue == 0) {
            return(node);
        }
        /* else if it is less than 0, go left, else right */
        node = ((compvalue < 0) ? node->left : node->right);
    }
    /* if we didn't find anything, return NULL */
    return(NULL);
}

/* Delete a node from the tree based on the key */
int ompi_rb_tree_delete(ompi_rb_tree_t *tree, void *key)
{
    ompi_rb_tree_node_t * p;
    ompi_rb_tree_node_t * todelete;
    ompi_rb_tree_node_t * y;
    ompi_free_list_item_t * item;

    p = ompi_rb_tree_find_node(tree, key);
    if (NULL == p) {
        return(OMPI_ERR_NOT_FOUND);
    }
    if ((p->left == tree->nill) || (p->right == tree->nill)) {
        todelete = p;
    } else {
        todelete = btree_successor(tree, p);
    }

    if (todelete->left == tree->nill) {
        y = todelete->right;
    } else {
        y = todelete->left;
    }

    y->parent = todelete->parent;

    if (y->parent == tree->root_ptr) {
        tree->root_ptr->left = y;
    } else {
        if (todelete == todelete->parent->left) {
         todelete->parent->left = y;
        } else {
            todelete->parent->right = y;
        }
    }

    if (todelete != p) {
        p->key = todelete->key;
        p->value = todelete->value;
    }

    if (todelete->color == BLACK) {
        btree_delete_fixup(tree, y);
    }
    item = (ompi_free_list_item_t *) todelete;
    OMPI_FREE_LIST_RETURN(&(tree->free_list), item);
    --tree->tree_size;
    return(OMPI_SUCCESS);
}


/* Destroy the hashmap    */
int ompi_rb_tree_destroy(ompi_rb_tree_t *tree)
{
    ompi_free_list_item_t * item;
    /* Recursive inorder traversal for delete    */

    inorder_destroy(tree, tree->root_ptr);
    /* Now free the root -- root does not get free'd in the above
     * inorder destroy    */
    item = (ompi_free_list_item_t *) tree->root_ptr;
    OMPI_FREE_LIST_RETURN(&(tree->free_list), item);

    /* free the tree->nill node */
    item = (ompi_free_list_item_t *) tree->nill;
    OMPI_FREE_LIST_RETURN(&(tree->free_list), item);
    return(OMPI_SUCCESS);
}


/* Find the next inorder successor of a node    */

static ompi_rb_tree_node_t * btree_successor(ompi_rb_tree_t * tree, ompi_rb_tree_node_t * node)
{
    ompi_rb_tree_node_t * p;

    if (node->right == tree->nill) {
        p = node->parent;
        while (node == p->right) {
            node = p;
            p = p->parent;
        }
        if(p == tree->root_ptr) {
            return(tree->nill);
        }
        return p;
    }

    p = node->right;
    while(p->left != tree->nill) {
        p = p->left;
    }
    return p;
}


/* Insert an element in the normal binary search tree fashion    */
/* this function goes through the tree and finds the leaf where
 * the node will be inserted   */
static void btree_insert(ompi_rb_tree_t *tree, ompi_rb_tree_node_t * node)
{
    ompi_rb_tree_node_t * parent = tree->root_ptr;
    ompi_rb_tree_node_t * n = parent->left; /* the real root of the tree */

    /* set up initial values for the node */
    node->color = RED;
    node->parent = NULL;
    node->left = tree->nill;
    node->right = tree->nill;

    /* find the leaf where we will insert the node */
    while (n != tree->nill) {
        parent = n;
        n = ((tree->comp(node->key, n->key) <= 0) ? n->left : n->right);
    }

    /* place it on either the left or the right */
    if((parent == tree->root_ptr) || (tree->comp(node->key, parent->key) <= 0)) {
        parent->left = node;
    } else {
        parent->right = node;
    }

    /* set its parent and children */
    node->parent = parent;
    node->left = tree->nill;
    node->right = tree->nill;
    ++(tree->tree_size);
    return;
}

/* Fixup the balance of the btree after deletion    */
static void btree_delete_fixup(ompi_rb_tree_t *tree, ompi_rb_tree_node_t * x)
{
    ompi_rb_tree_node_t * w;
    ompi_rb_tree_node_t * root = tree->root_ptr->left;
    while ((x != root) && (x->color == BLACK)) {
        if (x == x->parent->left) {
            w = x->parent->right;
            if (w->color == RED) {
                w->color = BLACK;
                x->parent->color = RED;
                left_rotate(tree, x->parent);
                w = x->parent->right;
            }
            if ((w->left->color == BLACK) && (w->right->color == BLACK)) {
                w->color = RED;
                x = x->parent;
            } else {
                if (w->right->color == BLACK) {
                    w->left->color = BLACK;
                    w->color = RED;
                    right_rotate(tree, w);
                    w = x->parent->right;
                }
                w->color = x->parent->color;
                x->parent->color = BLACK;
                w->right->color = BLACK;
                left_rotate(tree, x->parent);
                x = root;
            }
        } else { /* right    */

            w = x->parent->left;
            if (w->color == RED) {
                w->color = BLACK;
                x->parent->color = RED;
                right_rotate(tree, x->parent);
                w = x->parent->left;
            }
            if ((w->right->color == BLACK) && (w->left->color == BLACK)) {
                w->color = RED;
                x = x->parent;
            } else {
                if (w->left->color == BLACK) {
                    w->right->color = BLACK;
                    w->color = RED;
                    left_rotate(tree, w);
                    w = x->parent->left;
                }
                w->color = x->parent->color;
                x->parent->color = BLACK;
                w->left->color = BLACK;
                right_rotate(tree, x->parent);
                x = root;
            }
        }
    }

    x->color = BLACK;
    return;
}


/* Free the nodes in inorder fashion    */

static void
inorder_destroy(ompi_rb_tree_t *tree, ompi_rb_tree_node_t * node)
{
    ompi_free_list_item_t * item;

    if (node == tree->nill) {
        return;
    }

    inorder_destroy(tree, node->left);

    if (node->left != tree->nill) {
        item = (ompi_free_list_item_t *) node->left;
        --tree->tree_size;
        OMPI_FREE_LIST_RETURN(&(tree->free_list), item);
    }

    inorder_destroy(tree, node->right);
    if (node->right != tree->nill) {
        item = (ompi_free_list_item_t *) node->right;
        --tree->tree_size;
        OMPI_FREE_LIST_RETURN(&(tree->free_list), item);
    }
}

/* Try to access all the elements of the hashmap conditionally */

int ompi_rb_tree_traverse(ompi_rb_tree_t *tree,
                          ompi_rb_tree_condition_fn_t cond,
                          ompi_rb_tree_action_fn_t action)
{
    if ((cond == NULL) || (action == NULL)) {
        return(OMPI_ERROR);
    }

    inorder_traversal(tree, cond, action, tree->root_ptr->left);

    return(OMPI_SUCCESS);
}


static void inorder_traversal(ompi_rb_tree_t *tree, 
                              ompi_rb_tree_condition_fn_t cond,
                              ompi_rb_tree_action_fn_t action,
                              ompi_rb_tree_node_t * node)
{
    if (node == tree->nill) {
        return;
    }

    inorder_traversal(tree, cond, action, node->left);

    if (cond(node->value)) {
        action(node->key, node->value);
    }

    inorder_traversal(tree, cond, action, node->right);
}

/* Left rotate the tree    */
/* basically what we want to do is to make x be the left child
 * of its right child    */
static void left_rotate(ompi_rb_tree_t *tree, ompi_rb_tree_node_t * x)
{
    ompi_rb_tree_node_t * y;

    y = x->right;
    /* make the left child of y's parent be x if it is not the sentinal node*/
    if (y->left != tree->nill) {
        y->left->parent=x;
    }

    /* normlly we would have to check to see if we are at the root.
     * however, the root sentinal takes care of it for us */
    if (x == x->parent->left) {
        x->parent->left = y;
    } else {
        x->parent->right = y;
    }
    /* the old parent of x is now y's parent */
    y->parent = x->parent;
    /* x's parent is y */
    x->parent = y;
    x->right = y->left;
    y->left = x;

    return;
}


/* Right rotate the tree    */
/* basically what we want to do is to make x be the right child
 * of its left child */
static void right_rotate(ompi_rb_tree_t *tree, ompi_rb_tree_node_t * x)
{
    ompi_rb_tree_node_t * y;

    y = x->left;

    if(y->right != tree->nill) {
        y->right->parent = x;
    }

    if (x == x->parent->left) {
        x->parent->left = y;
    } else {
        x->parent->right = y;
    }

    y->parent = x->parent;
    x->parent = y;
    x->left = y->right;
    y->right = x;

    return;
}


/* returns the size of the tree */
int ompi_rb_tree_size(ompi_rb_tree_t *tree)
{
        return(tree->tree_size);
}

/* below are a couple of debugging functions */
#if 0
#include <stdio.h>
static void inorder(ompi_rb_tree_t * tree, ompi_rb_tree_node_t * node);
static void print_inorder(ompi_rb_tree_t * tree);

void inorder(ompi_rb_tree_t * tree, ompi_rb_tree_node_t * node)
{
    static int level = 0;
    if (node == tree->nill) {
        printf("nill\n");
        return;
    }
    level++;
    inorder(tree, node->left);
    level--;
    printf("%d, level: %d\n", *((int *)node->value), level);
    level++;
    inorder(tree, node->right);
    level--;
}


void print_inorder(ompi_rb_tree_t *tree)
{
    inorder(tree, tree->root_ptr->left);
}

#endif
