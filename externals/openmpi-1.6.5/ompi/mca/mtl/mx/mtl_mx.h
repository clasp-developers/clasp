/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MTL_MX_H_HAS_BEEN_INCLUDED
#define MTL_MX_H_HAS_BEEN_INCLUDED

#include "ompi/mca/mtl/mtl.h"
#include "ompi/mca/mtl/base/base.h"
#include "opal/datatype/opal_convertor.h"


BEGIN_C_DECLS

int
ompi_mtl_mx_send(struct mca_mtl_base_module_t* mtl, 
                 struct ompi_communicator_t* comm,
                 int dest,
                 int tag,
                 struct opal_convertor_t *convertor,
                 mca_pml_base_send_mode_t mode);

extern int ompi_mtl_mx_isend(struct mca_mtl_base_module_t* mtl, 
                             struct ompi_communicator_t* comm,
                             int dest,
                             int tag,
                             struct opal_convertor_t *convertor,
                             mca_pml_base_send_mode_t mode,
                             bool blocking,
                             mca_mtl_request_t * mtl_request);

extern int ompi_mtl_mx_irecv(struct mca_mtl_base_module_t* mtl,
                             struct ompi_communicator_t *comm,
                             int src,
                             int tag,
                             struct opal_convertor_t *convertor,
                             struct mca_mtl_request_t *mtl_request);
    
    
extern int ompi_mtl_mx_iprobe(struct mca_mtl_base_module_t* mtl, 
                              struct ompi_communicator_t *comm,
                              int src,
                              int tag,
                              int *flag,
                              struct ompi_status_public_t *status);

extern int ompi_mtl_mx_cancel(struct mca_mtl_base_module_t* mtl,
                              struct mca_mtl_request_t *mtl_request, 
                              int flag);
    
extern int ompi_mtl_mx_finalize(struct mca_mtl_base_module_t* mtl);

int ompi_mtl_mx_module_init(void);
    

   
END_C_DECLS

#endif  /* MTL_MX_H_HAS_BEEN_INCLUDED */

