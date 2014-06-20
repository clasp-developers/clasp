/*
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PML_CM_H
#define PML_CM_H

#include "ompi_config.h"
#include "ompi/request/request.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/request/request.h"
#include "ompi/mca/mtl/mtl.h"

BEGIN_C_DECLS

OMPI_MODULE_DECLSPEC extern mca_pml_base_component_2_0_0_t mca_pml_cm_component;

struct mca_mtl_request_t;

/* Array of send completion callback - one per send type  
 * These are called internally by the library when the send 
 * is completed from its perspective. 
 */
extern void (*send_completion_callbacks[])    
    (struct mca_mtl_request_t *mtl_request);

struct ompi_pml_cm_t {
    mca_pml_base_module_t super;
    int                   free_list_num;
    int                   free_list_max;
    int                   free_list_inc;
    int                   default_priority;
};
typedef struct ompi_pml_cm_t ompi_pml_cm_t;
extern ompi_pml_cm_t ompi_pml_cm;

/* PML interface functions */
extern int mca_pml_cm_add_procs(struct ompi_proc_t **procs, size_t nprocs);
extern int mca_pml_cm_del_procs(struct ompi_proc_t **procs, size_t nprocs);

extern int mca_pml_cm_enable(bool enable);
extern int mca_pml_cm_progress(void);

extern int mca_pml_cm_add_comm(struct ompi_communicator_t* comm);
extern int mca_pml_cm_del_comm(struct ompi_communicator_t* comm);

extern int mca_pml_cm_irecv_init(void *buf,
                                 size_t count,
                                 ompi_datatype_t *datatype,
                                 int src,
                                 int tag,
                                 struct ompi_communicator_t* comm,
                                 struct ompi_request_t **request);

extern int mca_pml_cm_irecv(void *buf,
                            size_t count,
                            ompi_datatype_t *datatype,
                            int src,
                            int tag,
                            struct ompi_communicator_t* comm,
                            struct ompi_request_t **request);

extern int mca_pml_cm_recv(void *buf,
                           size_t count,
                           ompi_datatype_t *datatype,
                           int src,
                           int tag,
                           struct ompi_communicator_t* comm,
                           ompi_status_public_t* status );

extern int mca_pml_cm_isend_init(void *buf,
                                 size_t count,
                                 ompi_datatype_t *datatype,
                                 int dst,
                                 int tag,
                                 mca_pml_base_send_mode_t mode,
                                 struct ompi_communicator_t* comm,
                                 struct ompi_request_t **request);

extern int mca_pml_cm_isend(void *buf,
                            size_t count,
                            ompi_datatype_t *datatype,
                            int dst,
                            int tag,
                            mca_pml_base_send_mode_t mode,
                            struct ompi_communicator_t* comm,
                            struct ompi_request_t **request);

extern int mca_pml_cm_send(void *buf,
                           size_t count,
                           ompi_datatype_t *datatype,
                           int dst,
                           int tag,
                           mca_pml_base_send_mode_t mode,
                           struct ompi_communicator_t* comm);

extern int mca_pml_cm_iprobe(int dst,
                             int tag,
                             struct ompi_communicator_t* comm,
                             int *matched,
                             ompi_status_public_t* status);

extern int mca_pml_cm_probe(int dst,
                            int tag,
                            struct ompi_communicator_t* comm,
                            ompi_status_public_t* status);

extern int mca_pml_cm_start(size_t count, ompi_request_t** requests);


extern int mca_pml_cm_dump(struct ompi_communicator_t* comm,
                           int verbose);

extern int mca_pml_cm_cancel(struct ompi_request_t *request, int flag);

END_C_DECLS

#endif  /* PML_CM_H_HAS_BEEN_INCLUDED */
