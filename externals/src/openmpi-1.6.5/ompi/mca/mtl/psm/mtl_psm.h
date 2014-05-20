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

#ifndef MTL_PSM_H_HAS_BEEN_INCLUDED
#define MTL_PSM_H_HAS_BEEN_INCLUDED

#include "ompi/mca/pml/pml.h"
#include "ompi/mca/mtl/mtl.h"
#include "ompi/mca/mtl/base/base.h"
#include "opal/datatype/opal_convertor.h"
#include <psm.h>
#include <psm_mq.h>

BEGIN_C_DECLS


/* MTL interface functions */
extern int ompi_mtl_psm_add_procs(struct mca_mtl_base_module_t* mtl, 
                          size_t nprocs,
                          struct ompi_proc_t** procs, 
                          struct mca_mtl_base_endpoint_t **mtl_peer_data);
    
extern int ompi_mtl_psm_del_procs(struct mca_mtl_base_module_t* mtl, 
                                 size_t nprocs,
                                 struct ompi_proc_t** procs, 
                                 struct mca_mtl_base_endpoint_t **mtl_peer_data);

int
ompi_mtl_psm_send(struct mca_mtl_base_module_t* mtl, 
                 struct ompi_communicator_t* comm,
                 int dest,
                 int tag,
                 struct opal_convertor_t *convertor,
                 mca_pml_base_send_mode_t mode);

extern int ompi_mtl_psm_isend(struct mca_mtl_base_module_t* mtl, 
                             struct ompi_communicator_t* comm,
                             int dest,
                             int tag,
                             struct opal_convertor_t *convertor,
                             mca_pml_base_send_mode_t mode,
                             bool blocking,
                             mca_mtl_request_t * mtl_request);

extern int ompi_mtl_psm_irecv(struct mca_mtl_base_module_t* mtl,
                             struct ompi_communicator_t *comm,
                             int src,
                             int tag,
                             struct opal_convertor_t *convertor,
                             struct mca_mtl_request_t *mtl_request);
    
    
extern int ompi_mtl_psm_iprobe(struct mca_mtl_base_module_t* mtl, 
                              struct ompi_communicator_t *comm,
                              int src,
                              int tag,
                              int *flag,
                              struct ompi_status_public_t *status);

extern int ompi_mtl_psm_cancel(struct mca_mtl_base_module_t* mtl,
                              struct mca_mtl_request_t *mtl_request, 
                              int flag);
    
extern int ompi_mtl_psm_finalize(struct mca_mtl_base_module_t* mtl);

int ompi_mtl_psm_module_init(int local_rank, int num_local_procs);
    

   
END_C_DECLS

#endif  /* MTL_PSM_H_HAS_BEEN_INCLUDED */

