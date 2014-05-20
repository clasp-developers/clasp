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
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
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
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/mca/pml/base/pml_base_request.h"
#include "opal/runtime/opal_progress.h"


int mca_pml_base_finalize(void) {
  if (NULL != mca_pml_base_selected_component.pmlm_finalize) {
    return mca_pml_base_selected_component.pmlm_finalize();
  }
  return OMPI_SUCCESS;
}

     
int mca_pml_base_close(void)
{
    /* turn off the progress code for the pml */
    if( NULL != mca_pml.pml_progress ) {
        opal_progress_unregister(mca_pml.pml_progress);
    }

    /* Blatently ignore the return code (what would we do to recover,
       anyway?  This module is going away, so errors don't matter
       anymore) */

    /**
     * Destruct the send and receive queues. The ompi_free_list_t destructor
     * will return the memory to the mpool, so this has to be done before the
     * mpool get released by the PML close function.
     */
    OBJ_DESTRUCT(&mca_pml_base_send_requests);
    OBJ_DESTRUCT(&mca_pml_base_recv_requests);

    mca_pml.pml_progress = mca_pml_base_progress;
    
    OBJ_DESTRUCT(&mca_pml_base_pml);

    /* Close all remaining available modules (may be one if this is a
       OMPI RTE program, or [possibly] multiple if this is ompi_info) */

    mca_base_components_close(mca_pml_base_output, 
                              &mca_pml_base_components_available, NULL, true);

    /* All done */

    return OMPI_SUCCESS;
}

