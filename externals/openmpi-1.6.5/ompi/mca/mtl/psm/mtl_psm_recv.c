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
#include "ompi/communicator/communicator.h"
#include "opal/datatype/opal_convertor.h"
#include "ompi/mca/mtl/base/mtl_base_datatype.h"

#include "orte/util/show_help.h"

#include "mtl_psm.h"
#include "mtl_psm_types.h"
#include "mtl_psm_request.h"

int
ompi_mtl_psm_irecv(struct mca_mtl_base_module_t* mtl,
                  struct ompi_communicator_t *comm,
                  int src,
                  int tag,
                  struct opal_convertor_t *convertor,
                  struct mca_mtl_request_t *mtl_request)
{
    int ret;    
    psm_error_t err;
    mca_mtl_psm_request_t * mtl_psm_request = (mca_mtl_psm_request_t*) mtl_request;
    uint64_t mqtag;
    uint64_t tagsel;
    size_t length;
    
    ret = ompi_mtl_datatype_recv_buf(convertor,
                                     &mtl_psm_request->buf,
                                     &length, 
                                     &mtl_psm_request->free_after);
    
    if (OMPI_SUCCESS != ret) return ret;

    mtl_psm_request->length = length;
    mtl_psm_request->convertor = convertor;
    mtl_psm_request->type = OMPI_MTL_PSM_IRECV;

    PSM_MAKE_TAGSEL(src, tag, comm->c_contextid, mqtag, tagsel);

#if 0
    printf("recv bits:   0x%016llx 0x%016llx\n", mqtag, tagsel);
#endif
    err = psm_mq_irecv(ompi_mtl_psm.mq, 
		       mqtag,
		       tagsel,
		       0,
		       mtl_psm_request->buf,
		       length,
		       mtl_psm_request,
		       &mtl_psm_request->psm_request);
    
    if (err) {
      orte_show_help("help-mtl-psm.txt",
		     "error posting receive", true,
		     psm_error_get_string(err),
		     mtl_psm_request->buf, length);
      return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

