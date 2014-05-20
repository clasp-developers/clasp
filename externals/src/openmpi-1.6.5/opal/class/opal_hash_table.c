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

#include "opal_config.h"

#include <string.h>
#include <stdlib.h>

#include "opal/util/output.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_hash_table.h"
#include "opal/constants.h"

/*
 * opal_hash_table_t
 */

#define HASH_MULTIPLIER 31

static void opal_hash_table_construct(opal_hash_table_t* ht);
static void opal_hash_table_destruct(opal_hash_table_t* ht);


OBJ_CLASS_INSTANCE(
    opal_hash_table_t, 
    opal_object_t,
    opal_hash_table_construct,
    opal_hash_table_destruct
);


static void opal_hash_table_construct(opal_hash_table_t* ht)
{
    OBJ_CONSTRUCT(&ht->ht_nodes, opal_list_t);
    ht->ht_table = NULL;
    ht->ht_table_size = 0;
    ht->ht_size = 0;
}


static void opal_hash_table_destruct(opal_hash_table_t* ht)
{
    size_t i;
    opal_hash_table_remove_all(ht);
    for(i=0; i<ht->ht_table_size; i++) {
        OBJ_DESTRUCT(ht->ht_table+i);
    }
    if(NULL != ht->ht_table) {
        free(ht->ht_table);
    }
    OBJ_DESTRUCT(&ht->ht_nodes);
}


int opal_hash_table_init(opal_hash_table_t* ht, size_t table_size)
{
    size_t i;
    size_t power2 = 1;
    size_t tmp = table_size;
    while(tmp) {
       tmp >>= 1;
       power2 <<= 1;
    }

    ht->ht_mask = power2-1;
    ht->ht_table = (opal_list_t *)malloc(power2 * sizeof(opal_list_t));
    if(NULL == ht->ht_table) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    for(i=ht->ht_table_size; i<power2; i++) {
        opal_list_t* list = ht->ht_table+i;
        OBJ_CONSTRUCT(list, opal_list_t);
    }
    ht->ht_table_size = power2;
    return OPAL_SUCCESS;
}

int opal_hash_table_remove_all(opal_hash_table_t* ht)
{
    size_t i;
    for(i=0; i<ht->ht_table_size; i++) {
        opal_list_t* list = ht->ht_table+i;
        while(opal_list_get_size(list)) {
            opal_list_item_t *item = opal_list_remove_first(list);
            OBJ_RELEASE(item);
        }
    }

    while(opal_list_get_size(&ht->ht_nodes)) {
        opal_list_item_t* item = opal_list_remove_first(&ht->ht_nodes);
        OBJ_RELEASE(item);
    }
    ht->ht_size = 0;
    return OPAL_SUCCESS;
}
 
/***************************************************************************/

/*
 *  opal_uint32_hash_node_t
 */

struct opal_uint32_hash_node_t
{
    opal_list_item_t super;
    uint32_t hn_key;
    void *hn_value;
};
typedef struct opal_uint32_hash_node_t opal_uint32_hash_node_t;

static OBJ_CLASS_INSTANCE(opal_uint32_hash_node_t,
                          opal_list_item_t,
                          NULL,
                          NULL);


int opal_hash_table_get_value_uint32(opal_hash_table_t* ht, uint32_t key,
				     void **ptr)
{
    opal_list_t* list = ht->ht_table + (key & ht->ht_mask);
    opal_uint32_hash_node_t *node;

#if OPAL_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        opal_output(0, "opal_hash_table_get_value_uint32:"
		   "opal_hash_table_init() has not been called");
        return OPAL_ERROR;
    }
#endif
    for(node =  (opal_uint32_hash_node_t*)opal_list_get_first(list);
        node != (opal_uint32_hash_node_t*)opal_list_get_end(list);
        node =  (opal_uint32_hash_node_t*)opal_list_get_next(node)) {
        if (node->hn_key == key) {
	    *ptr = node->hn_value;
            return OPAL_SUCCESS;
        }
    } 
    return OPAL_ERR_NOT_FOUND;
}


int opal_hash_table_set_value_uint32(opal_hash_table_t* ht,
				    uint32_t key, void* value)
{
    opal_list_t* list = ht->ht_table + (key & ht->ht_mask);
    opal_uint32_hash_node_t *node;

#if OPAL_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        opal_output(0, "opal_hash_table_set_value_uint32:"
		   "opal_hash_table_init() has not been called");
        return OPAL_ERR_BAD_PARAM;
    }
#endif
    for(node =  (opal_uint32_hash_node_t*)opal_list_get_first(list);
        node != (opal_uint32_hash_node_t*)opal_list_get_end(list);
        node =  (opal_uint32_hash_node_t*)opal_list_get_next(node)) {
        if (node->hn_key == key) {
            node->hn_value = value;
            return OPAL_SUCCESS;
        }
    } 

    node = (opal_uint32_hash_node_t*)opal_list_remove_first(&ht->ht_nodes); 
    if(NULL == node) {
        node = OBJ_NEW(opal_uint32_hash_node_t);
        if(NULL == node)
            return OPAL_ERR_OUT_OF_RESOURCE;
    }
    node->hn_key = key;
    node->hn_value = value;
    opal_list_append(list, (opal_list_item_t*)node);
    ht->ht_size++;
    return OPAL_SUCCESS;
}


int opal_hash_table_remove_value_uint32(opal_hash_table_t* ht, uint32_t key)
{
    opal_list_t* list = ht->ht_table + (key & ht->ht_mask);
    opal_uint32_hash_node_t *node;

#if OPAL_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        opal_output(0, "opal_hash_table_remove_value_uint32:"
		   "opal_hash_table_init() has not been called");
        return OPAL_ERR_BAD_PARAM;
    }
#endif
    for(node =  (opal_uint32_hash_node_t*)opal_list_get_first(list);
        node != (opal_uint32_hash_node_t*)opal_list_get_end(list);
        node =  (opal_uint32_hash_node_t*)opal_list_get_next(node)) {
        if (node->hn_key == key) {
            opal_list_remove_item(list, (opal_list_item_t*)node);
            opal_list_append(&ht->ht_nodes, (opal_list_item_t*)node);
            ht->ht_size--;
            return OPAL_SUCCESS;
        }
    } 
    return OPAL_ERR_NOT_FOUND;
}

/***************************************************************************/

/*
 *  opal_uint64_hash_node_t
 */

struct opal_uint64_hash_node_t
{
    opal_list_item_t super;
    uint64_t hn_key;
    void* hn_value;
};
typedef struct opal_uint64_hash_node_t opal_uint64_hash_node_t;

static OBJ_CLASS_INSTANCE(opal_uint64_hash_node_t,
                          opal_list_item_t,
                          NULL,
                          NULL);


int opal_hash_table_get_value_uint64(opal_hash_table_t* ht, uint64_t key,
				     void **ptr)
{
    opal_list_t* list = ht->ht_table + (key & ht->ht_mask);
    opal_uint64_hash_node_t *node;

#if OPAL_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        opal_output(0, "opal_hash_table_get_value_uint64:"
		   "opal_hash_table_init() has not been called");
        return OPAL_ERROR;
    }
#endif
    for(node =  (opal_uint64_hash_node_t*)opal_list_get_first(list);
        node != (opal_uint64_hash_node_t*)opal_list_get_end(list);
        node =  (opal_uint64_hash_node_t*)opal_list_get_next(node)) {
        if (node->hn_key == key) {
            *ptr = node->hn_value;
	    return OPAL_SUCCESS;
        }
    } 
    return OPAL_ERR_NOT_FOUND;
}


int opal_hash_table_set_value_uint64(opal_hash_table_t* ht,
				    uint64_t key, void* value)
{
    opal_list_t* list = ht->ht_table + (key & ht->ht_mask);
    opal_uint64_hash_node_t *node;

#if OPAL_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        opal_output(0, "opal_hash_table_set_value_uint64:"
		   "opal_hash_table_init() has not been called");
        return OPAL_ERR_BAD_PARAM;
    }
#endif
    for(node =  (opal_uint64_hash_node_t*)opal_list_get_first(list);
        node != (opal_uint64_hash_node_t*)opal_list_get_end(list);
        node =  (opal_uint64_hash_node_t*)opal_list_get_next(node)) {
        if (node->hn_key == key) {
            node->hn_value = value;
            return OPAL_SUCCESS;
        }
    } 

    node = (opal_uint64_hash_node_t*)opal_list_remove_first(&ht->ht_nodes); 
    if(NULL == node) {
        node = OBJ_NEW(opal_uint64_hash_node_t);
        if(NULL == node) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
    }
    node->hn_key = key;
    node->hn_value = value;
    opal_list_append(list, (opal_list_item_t*)node);
    ht->ht_size++;
    return OPAL_SUCCESS;
}


int opal_hash_table_remove_value_uint64(opal_hash_table_t* ht, uint64_t key)
{
    opal_list_t* list = ht->ht_table + (key & ht->ht_mask);
    opal_uint64_hash_node_t *node;

#if OPAL_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        opal_output(0, "opal_hash_table_remove_value_uint64:"
		   "opal_hash_table_init() has not been called");
        return OPAL_ERR_BAD_PARAM;
    }
#endif
    for(node =  (opal_uint64_hash_node_t*)opal_list_get_first(list);
        node != (opal_uint64_hash_node_t*)opal_list_get_end(list);
        node =  (opal_uint64_hash_node_t*)opal_list_get_next(node)) {
        if (node->hn_key == key) {
            opal_list_remove_item(list, (opal_list_item_t*)node);
            opal_list_append(&ht->ht_nodes, (opal_list_item_t*)node);
            ht->ht_size--;
            return OPAL_SUCCESS;
        }
    } 
    return OPAL_ERR_NOT_FOUND;
}

/***************************************************************************/

/*
 *  opal_ptr_hash_node_t
 */

struct opal_ptr_hash_node_t
{
    opal_list_item_t super;
    void*  hn_key;
    size_t hn_key_size;
    void*  hn_value;
};
typedef struct opal_ptr_hash_node_t opal_ptr_hash_node_t;

static void opal_ptr_hash_node_construct(opal_ptr_hash_node_t* hn)
{
    hn->hn_key_size = 0;
    hn->hn_key = NULL;
    hn->hn_value = NULL;
}

static void opal_ptr_hash_node_destruct(opal_ptr_hash_node_t* hn)
{
    if(NULL != hn->hn_key) {
        free(hn->hn_key);
    }
}

static OBJ_CLASS_INSTANCE(opal_ptr_hash_node_t,
                          opal_list_item_t,
                          opal_ptr_hash_node_construct,
                          opal_ptr_hash_node_destruct);


static inline uint32_t opal_hash_value(size_t mask, const void *key,
                                       size_t keysize)
{
    size_t h, i;
    const unsigned char *p;
    
    h = 0;
    p = (const unsigned char *)key;
    for (i = 0; i < keysize; i++, p++)
        h = HASH_MULTIPLIER*h + *p;
    return (uint32_t)(h & mask);
}

int opal_hash_table_get_value_ptr(opal_hash_table_t* ht, const void* key,
				  size_t key_size, void **ptr)
{
    opal_list_t* list = ht->ht_table + opal_hash_value(ht->ht_mask, key, 
                                                       key_size);
    opal_ptr_hash_node_t *node;

#if OPAL_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        opal_output(0, "opal_hash_table_get_value_ptr:"
		   "opal_hash_table_init() has not been called");
        return OPAL_ERROR;
    }
#endif
    for(node =  (opal_ptr_hash_node_t*)opal_list_get_first(list);
        node != (opal_ptr_hash_node_t*)opal_list_get_end(list);
        node =  (opal_ptr_hash_node_t*)opal_list_get_next(node)) {
        if (node->hn_key_size == key_size &&
            memcmp(node->hn_key, key, key_size) == 0) {
            *ptr = node->hn_value;
	    return OPAL_SUCCESS;
        }
    } 
    return OPAL_ERR_NOT_FOUND;
}


int opal_hash_table_set_value_ptr(opal_hash_table_t* ht, const void* key,
                                  size_t key_size, void* value)
{
    opal_list_t* list = ht->ht_table + opal_hash_value(ht->ht_mask, key,
                                                       key_size);
    opal_ptr_hash_node_t *node;

#if OPAL_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        opal_output(0, "opal_hash_table_set_value_ptr:"
		   "opal_hash_table_init() has not been called");
        return OPAL_ERR_BAD_PARAM;
    }
#endif
    for(node =  (opal_ptr_hash_node_t*)opal_list_get_first(list);
        node != (opal_ptr_hash_node_t*)opal_list_get_end(list);
        node =  (opal_ptr_hash_node_t*)opal_list_get_next(node)) {
        if (node->hn_key_size == key_size &&
            memcmp(node->hn_key, key, key_size) == 0) {
            node->hn_value = value;
            return OPAL_SUCCESS;
        }
    } 

    node = (opal_ptr_hash_node_t*)opal_list_remove_first(&ht->ht_nodes); 
    if(NULL == node) {
        node = OBJ_NEW(opal_ptr_hash_node_t);
        if(NULL == node) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
    }
    node->hn_key = malloc(key_size);
    node->hn_key_size = key_size;
    node->hn_value = value;
    memcpy(node->hn_key, key, key_size);
    opal_list_append(list, (opal_list_item_t*)node);
    ht->ht_size++;
    return OPAL_SUCCESS;
}


int opal_hash_table_remove_value_ptr(opal_hash_table_t* ht,
                                     const void* key, size_t key_size)
{
    opal_list_t* list = ht->ht_table + opal_hash_value(ht->ht_mask,
                                                       key, key_size);
    opal_ptr_hash_node_t *node;

#if OPAL_ENABLE_DEBUG
    if(ht->ht_table_size == 0) {
        opal_output(0, "opal_hash_table_remove_value_ptr: "
		   "opal_hash_table_init() has not been called");
        return OPAL_ERR_BAD_PARAM;
    }
#endif
    for(node =  (opal_ptr_hash_node_t*)opal_list_get_first(list);
        node != (opal_ptr_hash_node_t*)opal_list_get_end(list);
        node =  (opal_ptr_hash_node_t*)opal_list_get_next(node)) {
        if (node->hn_key_size == key_size &&
            memcmp(node->hn_key, key, key_size) == 0) {
            free(node->hn_key);
            node->hn_key = NULL;
            node->hn_key_size = 0;
            opal_list_remove_item(list, (opal_list_item_t*)node);
            opal_list_append(&ht->ht_nodes, (opal_list_item_t*)node);
            ht->ht_size--;
            return OPAL_SUCCESS;
        }
    } 
 return OPAL_ERR_NOT_FOUND;
}


int 
opal_hash_table_get_first_key_uint32(opal_hash_table_t *ht, uint32_t *key, 
                                     void **value, void **node)
{
    size_t i;
    opal_uint32_hash_node_t *list_node;

    /* Go through all the lists and return the first element off the
       first non-empty list */
    
    for (i = 0; i < ht->ht_table_size; ++i) {
        if (opal_list_get_size(ht->ht_table + i) > 0) {
            list_node = (opal_uint32_hash_node_t*)
                opal_list_get_first(ht->ht_table + i);
            *node = list_node;
            *key = list_node->hn_key;
            *value = list_node->hn_value;
            return OPAL_SUCCESS;
        }
    }

    /* The hash table is empty */

    return OPAL_ERROR;
}


int 
opal_hash_table_get_next_key_uint32(opal_hash_table_t *ht, uint32_t *key,
                                    void **value, void *in_node, 
                                    void **out_node)
{
    size_t i;
    opal_list_t *list;
    opal_list_item_t *item;
    opal_uint32_hash_node_t *next;

    /* Try to simply get the next value in the list.  If there isn't
       one, find the next non-empty list and take the first value */

    next = (opal_uint32_hash_node_t*) in_node;
    list = ht->ht_table + (next->hn_key & ht->ht_mask);
    item = opal_list_get_next(next);
    if (opal_list_get_end(list) == item) {
        item = NULL;
        for (i = (list - ht->ht_table) + 1; i < ht->ht_table_size; ++i) {
            if (opal_list_get_size(ht->ht_table + i) > 0) {
                item = opal_list_get_first(ht->ht_table + i);
                break;
            }
        }

        /* If we didn't find another non-empty list after this one,
           then we're at the end of the hash table */

        if (NULL == item) {
            return OPAL_ERROR;
        }
    }

    /* We found it.  Save the values (use "next" to avoid some
       typecasting) */

    *out_node = (void *) item;
    next = (opal_uint32_hash_node_t *) *out_node;
    *key = next->hn_key;
    *value = next->hn_value;

    return OPAL_SUCCESS;
}


int 
opal_hash_table_get_first_key_uint64(opal_hash_table_t *ht, uint64_t *key,
                                     void **value, void **node)
{
    size_t i;
    opal_uint64_hash_node_t *list_node;

    /* Go through all the lists and return the first element off the
       first non-empty list */
    
    for (i = 0; i < ht->ht_table_size; ++i) {
        if (opal_list_get_size(ht->ht_table + i) > 0) {
            list_node = (opal_uint64_hash_node_t*)
                opal_list_get_first(ht->ht_table + i);
            *node = list_node;
            *key = list_node->hn_key;
            *value = list_node->hn_value;
            return OPAL_SUCCESS;
        }
    }

    /* The hash table is empty */

    return OPAL_ERROR;
}


int 
opal_hash_table_get_next_key_uint64(opal_hash_table_t *ht, uint64_t *key,
                                    void **value, void *in_node, 
                                    void **out_node)
{
    size_t i;
    opal_list_t *list;
    opal_list_item_t *item;
    opal_uint64_hash_node_t *next;

    /* Try to simply get the next value in the list.  If there isn't
       one, find the next non-empty list and take the first value */

    next = (opal_uint64_hash_node_t*) in_node;
    list = ht->ht_table + (next->hn_key & ht->ht_mask);
    item = opal_list_get_next(next);
    if (opal_list_get_end(list) == item) {
        item = NULL;
        for (i = (list - ht->ht_table) + 1; i < ht->ht_table_size; ++i) {
            if (opal_list_get_size(ht->ht_table + i) > 0) {
                item = opal_list_get_first(ht->ht_table + i);
                break;
            }
        }

        /* If we didn't find another non-empty list after this one,
           then we're at the end of the hash table */

        if (NULL == item) {
            return OPAL_ERROR;
        }
    }

    /* We found it.  Save the values (use "next" to avoid some
       typecasting) */

    *out_node = (void *) item;
    next = (opal_uint64_hash_node_t *) *out_node;
    *key = next->hn_key;
    *value = next->hn_value;

    return OPAL_SUCCESS;
}
