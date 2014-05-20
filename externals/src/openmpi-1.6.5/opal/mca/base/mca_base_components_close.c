/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/class/opal_list.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"
#include "opal/constants.h"

int mca_base_components_close(int output_id, 
                              opal_list_t *components_available, 
                              const mca_base_component_t *skip,
                              bool close_stream)
{
  opal_list_item_t *item;
  mca_base_component_priority_list_item_t *pcli, *skipped_pcli = NULL;
  const mca_base_component_t *component;

  /* Close and unload all components in the available list, except the
     "skip" item.  This is handy to close out all non-selected
     components.  It's easier to simply remove the entire list and
     then simply re-add the skip entry when done. */

  for (item = opal_list_remove_first(components_available);
       NULL != item; 
       item = opal_list_remove_first(components_available)) {
    pcli = (mca_base_component_priority_list_item_t *) item;
    component = pcli->super.cli_component;

    if (component != skip) {

      /* Close */


      if (NULL != component->mca_close_component) {
        component->mca_close_component();
        opal_output_verbose(10, output_id, 
                            "mca: base: close: component %s closed",
                           component->mca_component_name);
      }

      /* Unload */

      opal_output_verbose(10, output_id, 
                          "mca: base: close: unloading component %s",
                         component->mca_component_name);
      mca_base_component_repository_release((mca_base_component_t *) component);
      free(pcli);
    } else {
      skipped_pcli = pcli;
    }
  }

  /* If we found it, re-add the skipped component to the available
     list (see above comment) */

  if (NULL != skipped_pcli) {
    opal_list_append(components_available, (opal_list_item_t *) skipped_pcli);
  }

  /*
   * If we are not the verbose output stream, and we shouldn't skip
   * any components, close the output stream.  If there's a skip
   * component, this is a 'choose one' framework and we're closing the
   * unchoosen components, but will still be using the framework.  Or,
   * if the caller told us to close the stream, then close it.
   */
  if (output_id > 0 && 
      (close_stream || NULL == skip)) {
      opal_output_close (output_id);
  }
  /* All done */
  return OPAL_SUCCESS;
}
