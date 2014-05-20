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
#ifndef MCA_RCACHE_BASE_H
#define MCA_RCACHE_BASE_H

#include "ompi_config.h"

#include "opal/class/opal_list.h"
#include "opal/mca/mca.h"
#include "ompi/mca/rcache/rcache.h"

BEGIN_C_DECLS

/* 
 * create a module by name
 */
OMPI_DECLSPEC mca_rcache_base_module_t* mca_rcache_base_module_create(const char* name); 

/*
 * opens all rcache components
 */ 
OMPI_DECLSPEC int mca_rcache_base_open(void); 

/* 
 * close all rcache components and call finalize on any open modules 
 */ 
OMPI_DECLSPEC int mca_rcache_base_close(void); 


struct mca_rcache_base_selected_module_t {
    opal_list_item_t super;
    mca_rcache_base_component_t *rcache_component;
    mca_rcache_base_module_t *rcache_module;
};
typedef struct mca_rcache_base_selected_module_t mca_rcache_base_selected_module_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_rcache_base_selected_module_t);

OMPI_DECLSPEC mca_rcache_base_component_t* mca_rcache_base_component_lookup(const char* name);
OMPI_DECLSPEC mca_rcache_base_module_t* mca_rcache_base_module_lookup(const char* name);
 
/*
 * Globals
 */
OMPI_DECLSPEC extern int mca_rcache_base_output;
OMPI_DECLSPEC extern opal_list_t mca_rcache_base_components;
OMPI_DECLSPEC extern opal_list_t mca_rcache_base_modules;

END_C_DECLS

#endif /* MCA_RCACHE_BASE_H */
