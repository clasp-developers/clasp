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
 * Copyright (c) 2007-2009 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#define OPAL_DISABLE_ENABLE_MEM_DEBUG 1
#include "ompi_config.h"

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h> 
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/memoryhooks/memory.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/constants.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "ompi/mca/mpool/base/static-components.h"

#include "mpool_base_tree.h"
/*
 * Global variables
 */
int mca_mpool_base_output = -1;

/* whether we actually used the mem hooks or not */
int mca_mpool_base_used_mem_hooks = 0;

uint32_t mca_mpool_base_page_size; 
uint32_t mca_mpool_base_page_size_log;

opal_list_t mca_mpool_base_components;
opal_list_t mca_mpool_base_modules;

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_mpool_base_open(void)
{
    /* Open up all available components - and populate the
       mca_mpool_base_components list */
    
    if (OMPI_SUCCESS != 
        mca_base_components_open("mpool", 0, mca_mpool_base_static_components, 
                               &mca_mpool_base_components, true)) {
        return OMPI_ERROR;
    }
  
     /* Initialize the list so that in mca_mpool_base_close(), we can
        iterate over it (even if it's empty, as in the case of ompi_info) */

    OBJ_CONSTRUCT(&mca_mpool_base_modules, opal_list_t);
  
    /* get the page size for this architecture*/ 
    mca_mpool_base_page_size = sysconf(_SC_PAGESIZE); 
    mca_mpool_base_page_size_log = my_log2(mca_mpool_base_page_size); 

    /* setup tree for tracking MPI_Alloc_mem */ 
    mca_mpool_base_tree_init();
    
    return OMPI_SUCCESS;
}

