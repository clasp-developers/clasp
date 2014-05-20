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


#include "ompi_config.h"
#include <stdio.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "ompi/mca/rcache/rcache.h"
#include "ompi/mca/rcache/base/base.h"
#include "ompi/constants.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#ifdef __WINDOWS__
    const mca_base_component_t *mca_rcache_base_static_components[] = {NULL};
#else 
#include "ompi/mca/rcache/base/static-components.h"
#endif


/*
 * Global variables
 */
int mca_rcache_base_output = -1;
opal_list_t mca_rcache_base_components;
opal_list_t mca_rcache_base_modules;


OBJ_CLASS_INSTANCE(mca_rcache_base_selected_module_t, opal_list_item_t, NULL, NULL);

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_rcache_base_open(void)
{
  /* Open up all available components - and populate the
     mca_rcache_base_components list */

  if (OMPI_SUCCESS != 
      mca_base_components_open("rcache", 0, mca_rcache_base_static_components, 
                               &mca_rcache_base_components, true)) {
    return OMPI_ERROR;
  }

  /* Initialize the list so that in mca_rcache_base_close(), we can
     iterate over it (even if it's empty, as in the case of the ompi_info-tool) */

  OBJ_CONSTRUCT(&mca_rcache_base_modules, opal_list_t);

  /* All done */

  return OMPI_SUCCESS;
}
