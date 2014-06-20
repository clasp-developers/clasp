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
 * Copyright (c) 2006-2010 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <sys/_time.h>
#endif
#include <string.h>
#include "support.h"
#include "ompi/class/ompi_free_list.h"
#include "ompi/class/ompi_rb_tree.h"
#include "ompi/mca/mpool/base/base.h"

#define NUM_KEYS 10000
#define SEED  1
int keys[] = {
    0, 1, 2, 3, 4, 5, 6, 7
};

int values[] = {
    10, 11, 12, 13, 14, 15, 16, 17
};

int comp_fn(void * ele1, void * ele2);

void test1(void);

int comp_fn(void * ele1, void * ele2)
{
    if(*((int *) ele1) > *((int *) ele2)) {
        return(1);
    }
    if(*((int *) ele1) < *((int *) ele2)) {
        return(-1);
    }
    return(0);
}

struct my_key_t{
    void *base; 
    void *bound; 
}; typedef struct my_key_t my_key_t; 

struct my_val_t{
    my_key_t* key; 
    int val; 
}; typedef struct my_val_t my_val_t; 

int comp_key(void* key1, void* key2) { 
    if( ((my_key_t*) key1)->base < 
        ((my_key_t*) key2)->base) { 
        return -1;
    }
    else if ( ((my_key_t*) key1)->base > 
              ((my_key_t*) key2)->bound) {
        return 1;
    }
    else { 
        return 0; 
    }
}

void test_keys(void)
{
    ompi_rb_tree_t tree; 
    int rc, i, *result; 
    my_key_t keys[NUM_KEYS];
    my_val_t vals[NUM_KEYS];
    char buf[200];
    my_key_t *cur_key;
    my_val_t *cur_val;
    long tmp;

    OBJ_CONSTRUCT(&tree, ompi_rb_tree_t); 
    rc = ompi_rb_tree_init(&tree, comp_key); 
    srand(SEED); 
    for(i = 0; i < NUM_KEYS; i++) { 
        cur_key = &(keys[i]); 
        cur_val = &(vals[i]); 
        cur_val->key = cur_key;
        cur_val->val = i;
        tmp = (long) rand();
        cur_key->base = (void*) tmp;
        tmp += (long) rand();
        cur_key->bound = (void*) tmp;
        rc = ompi_rb_tree_insert(&tree, cur_key, cur_val); 
        if(OMPI_SUCCESS != rc) { 
            test_failure("error inserting element in the tree"); 
        }
    }
    for(i = 0; i < NUM_KEYS; i+=2) { 
        cur_key = &(keys[i]); 
        rc = ompi_rb_tree_delete(&tree, cur_key);
        if(OMPI_SUCCESS != rc) { 
            test_failure("error deleting element in the tree"); 
        }
    }
    for(i = 1; i < NUM_KEYS; i+=2) { 
        cur_key = &(keys[i]); 
        cur_val = (my_val_t*) ompi_rb_tree_find(&tree, cur_key); 
        if(cur_val == NULL) { 
            test_failure("lookup returned NULL item"); 
        }
        else if(cur_val->val != i && (cur_val->key->base > cur_key->base ||
                                      cur_val->key->bound < cur_key->base)) { 
            sprintf(buf, "lookup returned invalid item, returned %d, extected %d", 
                    cur_val->val, i); 
            test_failure(buf);
        }
        
    }
}

void test1(void)
{
    ompi_rb_tree_t tree;
    int rc;
    void * result;
 
    OBJ_CONSTRUCT(&tree, ompi_rb_tree_t);
    rc = ompi_rb_tree_init(&tree, comp_fn);
    if(!test_verify_int(OMPI_SUCCESS, rc)) {
        test_failure("failed to properly initialize the tree");
    }

    rc = ompi_rb_tree_insert(&tree, &keys[0], &values[0]);
    if(!test_verify_int(OMPI_SUCCESS, rc)) {
        test_failure("failed to properly insert a new node");
    }
    result = ompi_rb_tree_find(&tree, &keys[0]);
    if(NULL == result) {
        test_failure("lookup returned null!");
    }
    if(!test_verify_int(values[0], *((int *) result))) {
        test_failure("failed to properly insert a new node");
    }

    rc = ompi_rb_tree_insert(&tree, &keys[1], &values[1]);
    if(!test_verify_int(OMPI_SUCCESS, rc)) {
        test_failure("failed to properly insert a new node");
    }
    result = ompi_rb_tree_find(&tree, &keys[1]);
    if(NULL == result) {
        test_failure("lookup returned null!");
    }
    if(!test_verify_int(values[1], *((int *) result))) {
        test_failure("failed to properly insert a new node");
    }

    rc = ompi_rb_tree_insert(&tree, &keys[2], &values[2]);
    if(!test_verify_int(OMPI_SUCCESS, rc)) {
        test_failure("failed to properly insert a new node");
    }
    result = ompi_rb_tree_find(&tree, &keys[2]);
    if(NULL == result) {
        test_failure("lookup returned null!");
    }
    if(!test_verify_int(values[2], *((int *) result))) {
        test_failure("failed to properly insert a new node");
    }

    rc = ompi_rb_tree_insert(&tree, &keys[3], &values[3]);
    if(!test_verify_int(OMPI_SUCCESS, rc)) {
        test_failure("failed to properly insert a new node");
    }
    result = ompi_rb_tree_find(&tree, &keys[3]);
    if(NULL == result) {
        test_failure("lookup returned null!");
    }
    if(!test_verify_int(values[3], *((int *) result))) {
        test_failure("failed to properly insert a new node");
    }

    rc = ompi_rb_tree_insert(&tree, &keys[4], &values[4]);
    if(!test_verify_int(OMPI_SUCCESS, rc)) {
        test_failure("failed to properly insert a new node");
    }
    result = ompi_rb_tree_find(&tree, &keys[4]);
    if(NULL == result) {
        test_failure("lookup returned null!");
    }
    if(!test_verify_int(values[4], *((int *) result))) {
        test_failure("failed to properly insert a new node");
    }

    rc = ompi_rb_tree_insert(&tree, &keys[5], &values[5]);
    if(!test_verify_int(OMPI_SUCCESS, rc)) {
        test_failure("failed to properly insert a new node");
    }
    result = ompi_rb_tree_find(&tree, &keys[5]);
    if(NULL == result) {
        test_failure("lookup returned null!");
    }
    if(!test_verify_int(values[5], *((int *) result))) {
        test_failure("failed to properly insert a new node");
    }

    rc = ompi_rb_tree_insert(&tree, &keys[6], &values[6]);
    if(!test_verify_int(OMPI_SUCCESS, rc)) {
        test_failure("failed to properly insert a new node");
    }
    result = ompi_rb_tree_find(&tree, &keys[6]);
    if(NULL == result) {
        test_failure("lookup returned null!");
    }
    if(!test_verify_int(values[6], *((int *) result))) {
        test_failure("failed to properly insert a new node");
    }

    rc = ompi_rb_tree_insert(&tree, &keys[7], &values[7]);
    if(!test_verify_int(OMPI_SUCCESS, rc)) {
        test_failure("failed to properly insert a new node");
    }
    result = ompi_rb_tree_find(&tree, &keys[7]);
    if(NULL == result) {
        test_failure("lookup returned null!");
    }
    if(!test_verify_int(values[7], *((int *) result))) {
        test_failure("failed to properly insert a new node");
    }

    rc = ompi_rb_tree_size(&tree);
    if(!test_verify_int(8, rc)) {
        test_failure("failed to properly insert a new node");
    }

    rc = ompi_rb_tree_delete(&tree, &keys[0]);
    if(!test_verify_int(OMPI_SUCCESS, rc)) {
        test_failure("failed to properly delete a node");
    }
    result = ompi_rb_tree_find(&tree, &keys[0]);
    if(NULL != result) {
        test_failure("lookup returned a value instead of null!");
    } else {
        test_success();
    }
  
    OBJ_DESTRUCT(&tree);
}

/* the following test is based on memory lookups in the mpool */
int mem_node_compare(void * key1, void * key2);
void test2(void);

/* the maximum number of memory pools a piece of memory can be registered with */
#define MAX_REGISTRATIONS 10

/* the number of memory segments to allocate */
#define NUM_ALLOCATIONS 500 

struct ompi_test_rb_key_t
{
    void * bottom;          /* the bottom of the memory range */
    void * top;             /* the top of the memory range */
};
typedef struct ompi_test_rb_key_t ompi_test_rb_key_t;

struct ompi_test_rb_value_t
{
    ompi_free_list_item_t super; /* the parent class */
    ompi_test_rb_key_t key; /* the key which holds the memory pointers */
    mca_mpool_base_module_t* registered_mpools[MAX_REGISTRATIONS]; 
                            /* the mpools the memory is registered with */
};
typedef struct ompi_test_rb_value_t ompi_test_rb_value_t;

OBJ_CLASS_INSTANCE(ompi_test_rb_value_t, ompi_free_list_item_t, NULL, NULL);

int mem_node_compare(void * key1, void * key2)
{
    if(((ompi_test_rb_key_t *) key1)->bottom < 
       ((ompi_test_rb_key_t *) key2)->bottom)
    {
        return -1;
    }
    else if(((ompi_test_rb_key_t *) key1)->bottom > 
            ((ompi_test_rb_key_t *) key2)->top)
    {
        return 1;
    }
    return 0;
}

void test2(void)
{
    ompi_free_list_t key_list;
    ompi_free_list_item_t * new_value;
    ompi_rb_tree_t tree;
    int rc, i, size;
    void * result, * lookup;
    void * mem[NUM_ALLOCATIONS];
    ompi_free_list_item_t * key_array[NUM_ALLOCATIONS];
    struct timeval start, end;
    
    OBJ_CONSTRUCT(&key_list, ompi_free_list_t);
    ompi_free_list_init_new(&key_list, sizeof(ompi_test_rb_value_t),
            opal_cache_line_size,
            OBJ_CLASS(ompi_test_rb_value_t), 
            0,opal_cache_line_size,
            0, -1 , 128, NULL);
    
    OBJ_CONSTRUCT(&tree, ompi_rb_tree_t);
    rc = ompi_rb_tree_init(&tree, mem_node_compare);
    if(!test_verify_int(OMPI_SUCCESS, rc)) {
        test_failure("failed to properly initialize the tree");
    }
  
    size = 1;
    for(i = 0; i < NUM_ALLOCATIONS; i++)
    {
        mem[i] = malloc(size);
        if(NULL == mem[i])
        {
            test_failure("system out of memory");
            return;
        }   
        OMPI_FREE_LIST_GET(&key_list, new_value, rc);
        if(OMPI_SUCCESS != rc)
        {
            test_failure("failed to get memory from free list");
        }
        key_array[i] = new_value;
        ((ompi_test_rb_value_t *) new_value)->key.bottom = mem[i];
        ((ompi_test_rb_value_t *) new_value)->key.top = 
                                            (void *) ((size_t) mem[i] + size - 1);
        ((ompi_test_rb_value_t *) new_value)->registered_mpools[0] = (void *) i;
        rc = ompi_rb_tree_insert(&tree, &((ompi_test_rb_value_t *)new_value)->key, 
                        new_value);
        if(OMPI_SUCCESS != rc) 
        {
            test_failure("failed to properly insert a new node");
        }
        size += 1;   
    }
    
    gettimeofday(&start, NULL);
    for(i = 0; i < NUM_ALLOCATIONS; i++)
    {
        lookup = (void *) ((size_t) mem[i] + i);
        result = ompi_rb_tree_find(&tree, &lookup);
        if(NULL == result) 
        {
            test_failure("lookup returned null!");
        } else if(i != ((int) ((ompi_test_rb_value_t *) result)->registered_mpools[0]))
        {
            test_failure("lookup returned wrong node!");
        }
        result = ompi_rb_tree_find(&tree, &lookup);
        if(NULL == result) 
        {
            test_failure("lookup returned null!");
        } else if(i != ((int) ((ompi_test_rb_value_t *) result)->registered_mpools[0]))
        {
            test_failure("lookup returned wrong node!");
        }
    }

    gettimeofday(&end, NULL);

#if 0
    i = (end.tv_sec - start.tv_sec) * 1000000 + (end.tv_usec - start.tv_usec);
    printf("In a %d node tree, %d lookups took %f microseonds each\n", 
            NUM_ALLOCATIONS, NUM_ALLOCATIONS * 2, 
            (float) i / (float) (NUM_ALLOCATIONS * 2));
#endif

    for(i = 0; i < NUM_ALLOCATIONS; i++)
    {
        if(NULL != mem[i])
        {
            free(mem[i]);
        }
        OMPI_FREE_LIST_RETURN(&(key_list), key_array[i]);
    }

    OBJ_DESTRUCT(&tree);
    OBJ_DESTRUCT(&key_list);
}

int main(int argc, char **argv)
{
    test_init("ompi_rb_tree_t");
    
    test1();
    test2();
    /* test_keys(); */
    return test_finalize();
}
