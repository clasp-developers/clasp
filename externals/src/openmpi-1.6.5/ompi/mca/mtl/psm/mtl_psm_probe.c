/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2010 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      QLogic Corporation. All rights reserved.
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "mtl_psm.h"
#include "mtl_psm_types.h"
#include "psm.h"
#include "ompi/communicator/communicator.h"


int ompi_mtl_psm_iprobe(struct mca_mtl_base_module_t* mtl, 
                              struct ompi_communicator_t *comm,
                              int src,
                              int tag,
                              int *flag,
                              struct ompi_status_public_t *status)
{
    uint64_t mqtag, tagsel;
    psm_mq_status_t mqstat;
    psm_error_t err;

    PSM_MAKE_TAGSEL(src, tag, comm->c_contextid, mqtag, tagsel);

    err = psm_mq_iprobe(ompi_mtl_psm.mq, mqtag, tagsel, &mqstat);
    if (err == PSM_OK) {
	*flag = 1;
	if(MPI_STATUS_IGNORE != status) { 
            status->MPI_SOURCE = PSM_GET_MQRANK(mqstat.msg_tag);
            status->MPI_TAG = PSM_GET_MQUTAG(mqstat.msg_tag);
            status->_ucount = mqstat.nbytes;

            switch (mqstat.error_code) {
	    case PSM_OK:
		status->MPI_ERROR = OMPI_SUCCESS;
		break;
	    case PSM_MQ_TRUNCATION:
		status->MPI_ERROR = MPI_ERR_TRUNCATE;
		break;
	    default:
		status->MPI_ERROR = MPI_ERR_INTERN;
            }
        }
        
        return OMPI_SUCCESS;
    }
    else if (err == PSM_MQ_INCOMPLETE) {
	*flag = 0;
	return OMPI_SUCCESS;
    }
    else
	return OMPI_ERROR;
}
