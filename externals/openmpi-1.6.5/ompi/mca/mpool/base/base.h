/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2009 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_MEM_BASE_H
#define MCA_MEM_BASE_H

#include "ompi_config.h"

#include "opal/class/opal_list.h"
#include "opal/mca/mca.h"
#include "ompi/mca/mpool/mpool.h"

BEGIN_C_DECLS

static inline unsigned int my_log2(unsigned long val) { 
    unsigned int count = 0;
    while(val > 0) { 
        val = val >> 1; 
        count++;
    }
    return count > 0 ? count-1: 0;
}
static inline void *down_align_addr(void* addr, unsigned int shift) {
    return (void*) (((intptr_t) addr) & (~(intptr_t) 0) << shift); 
}

static inline void *up_align_addr(void*addr, unsigned int shift) { 
    return (void*) ((((intptr_t) addr) | ~((~(intptr_t) 0) << shift))); 
}

struct mca_mpool_base_selected_module_t {
    opal_list_item_t super;
    mca_mpool_base_component_t *mpool_component;
    mca_mpool_base_module_t *mpool_module;
    void* user_data; 
    struct mca_mpool_base_resources_t *mpool_resources;
};
typedef struct mca_mpool_base_selected_module_t mca_mpool_base_selected_module_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_mpool_base_selected_module_t);

/*
 * Data structures for the tree of allocated memory
 */

/*
 * Global functions for MCA: overall mpool open and close
 */

OMPI_DECLSPEC int mca_mpool_base_open(void);
OMPI_DECLSPEC int mca_mpool_base_init(bool enable_progress_threads, bool enable_mpi_threads);
OMPI_DECLSPEC int mca_mpool_base_close(void);
OMPI_DECLSPEC mca_mpool_base_component_t* mca_mpool_base_component_lookup(const char* name);
OMPI_DECLSPEC mca_mpool_base_module_t* mca_mpool_base_module_create(
    const char* name, 
    void* user_data,
    struct mca_mpool_base_resources_t* mpool_resources);
OMPI_DECLSPEC mca_mpool_base_module_t* mca_mpool_base_module_lookup(const char* name);
OMPI_DECLSPEC int mca_mpool_base_module_destroy(mca_mpool_base_module_t *module);
 
/*
 * Globals
 */
OMPI_DECLSPEC extern int mca_mpool_base_output;
OMPI_DECLSPEC extern opal_list_t mca_mpool_base_components;
extern opal_list_t mca_mpool_base_modules;
OMPI_DECLSPEC extern uint32_t mca_mpool_base_page_size;
OMPI_DECLSPEC extern uint32_t mca_mpool_base_page_size_log;

/* only used within base -- no need to DECLSPEC */
extern int mca_mpool_base_used_mem_hooks;
    
END_C_DECLS

#endif /* MCA_MEM_BASE_H */
