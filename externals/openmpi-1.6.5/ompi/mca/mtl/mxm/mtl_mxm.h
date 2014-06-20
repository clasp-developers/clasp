/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MTL_MXM_H_HAS_BEEN_INCLUDED
#define MTL_MXM_H_HAS_BEEN_INCLUDED

#include <stdint.h>
#include <sys/types.h>
#include <unistd.h>

#include <mxm/api/mxm_api.h>
#ifndef MXM_VERSION
#define MXM_VERSION(major, minor) (((major)<<MXM_MAJOR_BIT)|((minor)<<MXM_MINOR_BIT))
#endif
#if MXM_API < MXM_VERSION(2, 0)
#include <mxm/api/mxm_addr.h>
#endif

#include "ompi/mca/pml/pml.h"
#include "ompi/mca/mtl/mtl.h"
#include "ompi/mca/mtl/base/base.h"
#include "opal/datatype/opal_convertor.h"

#include "mtl_mxm_debug.h"

BEGIN_C_DECLS

/* MTL interface functions */
extern int ompi_mtl_mxm_add_procs(struct mca_mtl_base_module_t* mtl,
                                  size_t nprocs, struct ompi_proc_t** procs,
                                  struct mca_mtl_base_endpoint_t **mtl_peer_data);

extern int ompi_mtl_mxm_del_procs(struct mca_mtl_base_module_t* mtl,
                                  size_t nprocs, struct ompi_proc_t** procs,
                                  struct mca_mtl_base_endpoint_t **mtl_peer_data);

extern int ompi_mtl_mxm_send(struct mca_mtl_base_module_t* mtl,
                             struct ompi_communicator_t* comm, int dest, int tag,
                             struct opal_convertor_t *convertor,
                             mca_pml_base_send_mode_t mode);

extern int ompi_mtl_mxm_isend(struct mca_mtl_base_module_t* mtl,
                              struct ompi_communicator_t* comm, int dest,
                              int tag, struct opal_convertor_t *convertor,
                              mca_pml_base_send_mode_t mode, bool blocking,
                              mca_mtl_request_t * mtl_request);

extern int ompi_mtl_mxm_irecv(struct mca_mtl_base_module_t* mtl,
                              struct ompi_communicator_t *comm, int src,
                              int tag, struct opal_convertor_t *convertor,
                              struct mca_mtl_request_t *mtl_request);

extern int ompi_mtl_mxm_iprobe(struct mca_mtl_base_module_t* mtl,
                               struct ompi_communicator_t *comm, int src,
                               int tag, int *flag,
                               struct ompi_status_public_t *status);

extern int ompi_mtl_mxm_cancel(struct mca_mtl_base_module_t* mtl,
                               struct mca_mtl_request_t *mtl_request, int flag);

extern int ompi_mtl_mxm_add_comm(struct mca_mtl_base_module_t *mtl,
                                 struct ompi_communicator_t *comm);

extern int ompi_mtl_mxm_del_comm(struct mca_mtl_base_module_t *mtl,
                                 struct ompi_communicator_t *comm);

extern int ompi_mtl_mxm_finalize(struct mca_mtl_base_module_t* mtl);

int ompi_mtl_mxm_module_init(void);

END_C_DECLS

#endif

