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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_ALLOCATOR_BASE_H
#define MCA_ALLOCATOR_BASE_H

#include "ompi_config.h"

#include "opal/class/opal_list.h"
#include "opal/mca/mca.h"
#include "ompi/mca/allocator/allocator.h"

BEGIN_C_DECLS
/**
 * Structure which describes a selected module.
 */
struct mca_allocator_base_selected_module_t {
  opal_list_item_t super;    
  /**< Makes this an object of type opal_list_item */
  mca_allocator_base_component_t *allocator_component; 
  /**< Info about the module */
  mca_allocator_base_module_t *allocator_module; 
  /**< The function pointers for all the module's functions. */
};
/**
 * Convenience typedef.
 */
typedef struct mca_allocator_base_selected_module_t mca_allocator_base_selected_module_t;

/**
 * Declaces mca_mpool_base_selected_module_t as a class.
 */
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_allocator_base_selected_module_t);
                                                                                                     

/*
 * Global functions for MCA: overall allocator open and close
 */

OMPI_DECLSPEC int mca_allocator_base_open(void);
OMPI_DECLSPEC int mca_allocator_base_close(void);
OMPI_DECLSPEC mca_allocator_base_component_t* mca_allocator_component_lookup(const char* name);


/*
 * Globals
 */
/**
 * The list of all the selected components.
 */
OMPI_DECLSPEC extern opal_list_t mca_allocator_base_components;

END_C_DECLS

#endif /* MCA_ALLOCATOR_BASE_H */
