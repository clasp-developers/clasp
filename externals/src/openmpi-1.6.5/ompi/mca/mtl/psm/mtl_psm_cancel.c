/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      QLogic Corporation. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "mtl_psm.h"
#include "mtl_psm_request.h"

int ompi_mtl_psm_cancel(struct mca_mtl_base_module_t* mtl, 
                       struct mca_mtl_request_t *mtl_request, 
                       int flag) {

  psm_error_t err;
  psm_mq_status_t status;
  
  mca_mtl_psm_request_t *mtl_psm_request = 
    (mca_mtl_psm_request_t*) mtl_request; 
  
  err = psm_mq_cancel(&mtl_psm_request->psm_request); 
  if(PSM_OK == err) { 
    err = psm_mq_test(&mtl_psm_request->psm_request, &status);
    if(PSM_OK == err) { 
      mtl_request->ompi_req->req_status._cancelled = true;
      mtl_psm_request->super.completion_callback(&mtl_psm_request->super);
      return OMPI_SUCCESS;
    } else { 
      return OMPI_ERROR;
    }
  } else if(PSM_MQ_INCOMPLETE == err) { 
    return OMPI_SUCCESS; 
  } 
  
  return OMPI_ERROR;
}
