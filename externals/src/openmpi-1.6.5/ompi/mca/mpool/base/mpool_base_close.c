/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2013 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "ompi/constants.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "ompi/mca/mpool/base/base.h"
#include "mpool_base_mem_cb.h"


int mca_mpool_base_close(void)
{
  opal_list_item_t *item;
  mca_mpool_base_selected_module_t *sm;
  int32_t modules_length;

  /* Need the initial length in order to know if some of the initializations
   * are done in the open function.
   */
  modules_length = opal_list_get_size(&mca_mpool_base_modules);

  /* Finalize all the mpool components and free their list items */

  for (item = opal_list_remove_first(&mca_mpool_base_modules);
       NULL != item; 
       item = opal_list_remove_first(&mca_mpool_base_modules)) {
    sm = (mca_mpool_base_selected_module_t *) item;

    /* Blatently ignore the return code (what would we do to recover,
       anyway?  This component is going away, so errors don't matter
       anymore).  Note that it's legal for the module to have NULL for
       the finalize function. */

    if (NULL != sm->mpool_module->mpool_finalize) {
        sm->mpool_module->mpool_finalize(sm->mpool_module);
    }
    OBJ_RELEASE(sm);
  }

  /* Close all remaining available components (may be one if this is a
     OMPI RTE program, or [possibly] multiple if this is ompi_info) */

  mca_base_components_close(mca_mpool_base_output, 
                            &mca_mpool_base_components, NULL, true);

  /* deregister memory free callback */
  if( (modules_length > 0) && mca_mpool_base_used_mem_hooks && 
     0 != (OPAL_MEMORY_FREE_SUPPORT & opal_mem_hooks_support_level())) {
      opal_mem_hooks_unregister_release(mca_mpool_base_mem_cb);
  }
  /* All done */

  return OMPI_SUCCESS;
}
