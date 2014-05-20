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
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "ompi/mca/allocator/allocator.h"
#include "ompi/mca/allocator/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "ompi/mca/allocator/base/static-components.h"

/*
 * Global variables
 */
opal_list_t mca_allocator_base_components;
int mca_allocator_base_output = -1;

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_allocator_base_open(void)
{
  /* Open up all available components */

  return mca_base_components_open("allocator", 0, 
                                  mca_allocator_base_static_components, 
                                  &mca_allocator_base_components, true);
}

/**
 * Traverses through the list of available components, calling their init
 * functions until it finds the component that has the specified name. It
 * then returns the found component.
 *
 * @param name the name of the component that is being searched for.
 * @retval mca_allocator_base_component_t* pointer to the requested component
 * @retval NULL if the requested component is not found
 */
mca_allocator_base_component_t* mca_allocator_component_lookup(const char* name)
{
    /* Traverse the list of available components; call their init functions. */
    opal_list_item_t* item;
    for (item = opal_list_get_first(&mca_allocator_base_components);
         item != opal_list_get_end(&mca_allocator_base_components);
         item = opal_list_get_next(item)) {
         mca_base_component_list_item_t *cli = (mca_base_component_list_item_t *) item;
         mca_allocator_base_component_t* component = (mca_allocator_base_component_t *) cli->cli_component;
         if(strcmp(component->allocator_version.mca_component_name,
                   name) == 0) {
             return component;
         }
    }
    return NULL;
}


