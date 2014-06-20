/* -*- Mode: C; c-basic-offset:4 ; -*- */
/**
  * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
  *                         University Research and Technology
  *                         Corporation.  All rights reserved.
  * Copyright (c) 2004-2007 The University of Tennessee and The University
  *                         of Tennessee Research Foundation.  All rights
  *                         reserved.
  * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
  *                         University of Stuttgart.  All rights reserved.
  * Copyright (c) 2004-2005 The Regents of the University of California.
  *                         All rights reserved.
  * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
  * $COPYRIGHT$
  * 
  * Additional copyrights may follow
  * 
  * $HEADER$
  */
/**
  * @file
  * Description of the Memory Pool framework
  */
#ifndef MCA_MPOOL_H
#define MCA_MPOOL_H
#include "ompi_config.h"
#include "opal/mca/mca.h"
#include "ompi/info/info.h"
#include "ompi/class/ompi_free_list.h" 

#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

#define MCA_MPOOL_FLAGS_CACHE_BYPASS 0x1
#define MCA_MPOOL_FLAGS_PERSIST 0x2 
#define MCA_MPOOL_FLAGS_MPI_ALLOC_MEM 0x4
#define MCA_MPOOL_FLAGS_INVALID 0x8
#define MCA_MPOOL_FLAGS_SO_MEM 0x10

struct mca_mpool_base_resources_t;

struct mca_mpool_base_registration_t { 
    ompi_free_list_item_t super; 
    struct mca_mpool_base_module_t *mpool; 
    unsigned char* base;
    unsigned char* bound; 
    unsigned char* alloc_base;
    int32_t ref_count; 
    uint32_t flags;
};  

typedef struct mca_mpool_base_registration_t mca_mpool_base_registration_t; 

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_mpool_base_registration_t); 

/**
 * component initialize
 */
typedef struct mca_mpool_base_module_t* (*mca_mpool_base_component_init_fn_t)(
    struct mca_mpool_base_resources_t*);

/**
  * allocate function typedef
  */
typedef void* (*mca_mpool_base_module_alloc_fn_t)(
    struct mca_mpool_base_module_t* mpool,
    size_t size,
    size_t align,
    uint32_t flags, 
    mca_mpool_base_registration_t** registration);
                                                                                                                                   
/**
  * realloc function typedef
  */
typedef void* (*mca_mpool_base_module_realloc_fn_t)(
    struct mca_mpool_base_module_t* mpool,
    void* addr,
    size_t size,
    mca_mpool_base_registration_t** registration);
                                                                                                                                   
/**
  * free function typedef
  */
typedef void (*mca_mpool_base_module_free_fn_t)(
    struct mca_mpool_base_module_t* mpool,
    void *addr, 
    mca_mpool_base_registration_t* registration);
                                                                                                                                   
/**
  * register memory
  */
typedef int (*mca_mpool_base_module_register_fn_t)(
    struct mca_mpool_base_module_t* mpool,
    void * addr,
    size_t size,
    uint32_t flags, 
    mca_mpool_base_registration_t** registration);
    
/**
  * deregister memory
  */
typedef int (*mca_mpool_base_module_deregister_fn_t)(
    struct mca_mpool_base_module_t* mpool,
    mca_mpool_base_registration_t* registration);

/**
 * find registration in this memory pool
 */ 

typedef int (*mca_mpool_base_module_find_fn_t) (
        struct mca_mpool_base_module_t* mpool, void* addr, size_t size,
        mca_mpool_base_registration_t **reg);

/** 
 * release registration
 */ 

typedef int (*mca_mpool_base_module_release_fn_t) ( 
                                            struct mca_mpool_base_module_t* mpool, 
                                            mca_mpool_base_registration_t* registration); 

                                            
/**
 * release memory region
 */
typedef int (*mca_mpool_base_module_release_memory_fn_t) (
        struct mca_mpool_base_module_t* mpool, void *base, size_t size);

/**
  * if appropriate - returns base address of memory pool
  */
typedef void* (*mca_mpool_base_module_address_fn_t)(struct mca_mpool_base_module_t* mpool);

/**
  * finalize
  */
typedef void (*mca_mpool_base_module_finalize_fn_t)(struct mca_mpool_base_module_t*);


/**
 * Fault Tolerance Event Notification Function
 * @param state Checkpoint Stae
 * @return OMPI_SUCCESS or failure status
 */
typedef int (*mca_mpool_base_module_ft_event_fn_t)(int state);


/**
 * mpool component descriptor. Contains component version information
 * and open/close/init functions.
 */
struct mca_mpool_base_component_2_0_0_t {
  mca_base_component_t mpool_version;        /**< version */
  mca_base_component_data_t mpool_data;/**< metadata */

  mca_mpool_base_component_init_fn_t mpool_init;    /**< init function */
};
/**
 * Convenience typedef.
 */
typedef struct mca_mpool_base_component_2_0_0_t mca_mpool_base_component_2_0_0_t;
/**
  * Convenience typedef
  */
typedef struct mca_mpool_base_component_2_0_0_t mca_mpool_base_component_t;

/**
 *  mpool module descriptor. Contains the interface functions exported
 *  by the component.  This does not expose memory management
 *  details.
 */
struct mca_mpool_base_module_t {
    mca_mpool_base_component_t *mpool_component;  /**< component stuct */
    mca_mpool_base_module_address_fn_t mpool_base;       /**< returns the base address */
    mca_mpool_base_module_alloc_fn_t mpool_alloc;        /**< allocate function */
    mca_mpool_base_module_realloc_fn_t mpool_realloc;    /**< reallocate function */
    mca_mpool_base_module_free_fn_t mpool_free;          /**< free function */
    mca_mpool_base_module_register_fn_t mpool_register;  /**< register memory */
    mca_mpool_base_module_deregister_fn_t mpool_deregister; /**< deregister memory */
    mca_mpool_base_module_find_fn_t mpool_find; /**< find regisrations in the cache */
    mca_mpool_base_module_release_fn_t mpool_release; /**< release a registration from the cache */ 
    mca_mpool_base_module_release_memory_fn_t mpool_release_memory; /**< release memor region from the cache  */
    mca_mpool_base_module_finalize_fn_t mpool_finalize;  /**< finalize */
    mca_mpool_base_module_ft_event_fn_t mpool_ft_event;  /**< ft_event */
    struct mca_rcache_base_module_t *rcache; /* the rcache associated with this mpool */ 
    uint32_t flags; /**< mpool flags */
};
/**
 * Convenience typedef
 */
typedef struct mca_mpool_base_module_t mca_mpool_base_module_t;


/**
 * Function to allocate special memory according to what the user requests in
 * the info object.
 *
 * If the user passes in a valid info structure then the function will
 * try to allocate the memory and register it with every mpool that there is a
 * key for it in the info struct. If it fails at registering the memory with
 * one of the requested mpools, an error will be returned. Also, if there is a
 * key in info that does not match any mpool, an error will be returned.
 *
 * If the info parameter is MPI_INFO_NULL, then this function will try to allocate
 * the memory and register it wih as many mpools as possible. However,
 * if any of the registratons fail the mpool will simply be ignored.
 *
 * @param size the size of the memory area to allocate
 * @param info an info object which tells us what kind of memory to allocate
 *
 * @retval pointer to the allocated memory
 * @retval NULL on failure
 */
OMPI_DECLSPEC void * mca_mpool_base_alloc(size_t size, struct ompi_info_t * info);

/**
 * Function to free memory previously allocated by mca_mpool_base_alloc
 *
 * @param base pointer to the memory to free
 *
 * @retval OMPI_SUCCESS
 * @retval OMPI_ERR_BAD_PARAM if the passed base pointer was invalid
 */
OMPI_DECLSPEC int mca_mpool_base_free(void * base); 

/**
 * Function for the red black tree to compare 2 keys
 *
 * @param key1 a pointer to the 1st key
 * @param key2 a pointer to the second key
 *
 * @retval -1 if key1 is below key2
 * @retval 1 if key 1 is above key2
 * @retval 0 if the keys are the same
 */
OMPI_DECLSPEC int mca_mpool_base_tree_node_compare(void * key1, void * key2);


OMPI_DECLSPEC int mca_mpool_base_insert(
    void * addr, 
    size_t size, 
    mca_mpool_base_module_t* mpool, 
    void* user_in, 
    mca_mpool_base_registration_t* registration); 

OMPI_DECLSPEC int mca_mpool_base_remove(void * base); 

/**
 * Macro for use in components that are of type mpool
 */
#define MCA_MPOOL_BASE_VERSION_2_0_0 \
  MCA_BASE_VERSION_2_0_0, \
  "mpool", 2, 0, 0

#endif /* MCA_MPOOL_H */

