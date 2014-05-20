/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_COLL_BASIC_EXPORT_H
#define MCA_COLL_BASIC_EXPORT_H

#include "ompi_config.h"

#include "mpi.h"
#include "opal/mca/mca.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/request/request.h"
#include "ompi/communicator/communicator.h"

BEGIN_C_DECLS

    /* Globally exported variables */

    OMPI_MODULE_DECLSPEC extern const mca_coll_base_component_2_0_0_t
        mca_coll_basic_component;
    extern int mca_coll_basic_priority;
    extern int mca_coll_basic_crossover;

    /* API functions */

    int mca_coll_basic_init_query(bool enable_progress_threads,
                                  bool enable_mpi_threads);
    mca_coll_base_module_t
        *mca_coll_basic_comm_query(struct ompi_communicator_t *comm,
                                   int *priority);

    int mca_coll_basic_module_enable(mca_coll_base_module_t *module,
                                     struct ompi_communicator_t *comm);

    int mca_coll_basic_allgather_intra(void *sbuf, int scount,
                                       struct ompi_datatype_t *sdtype,
                                       void *rbuf, int rcount,
                                       struct ompi_datatype_t *rdtype,
                                       struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t *module);
    int mca_coll_basic_allgather_inter(void *sbuf, int scount,
                                       struct ompi_datatype_t *sdtype,
                                       void *rbuf, int rcount,
                                       struct ompi_datatype_t *rdtype,
                                       struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t *module);

    int mca_coll_basic_allgatherv_intra(void *sbuf, int scount,
                                        struct ompi_datatype_t *sdtype,
                                        void *rbuf, int *rcounts,
                                        int *disps,
                                        struct ompi_datatype_t *rdtype,
                                        struct ompi_communicator_t *comm,
                                        mca_coll_base_module_t *module);
    int mca_coll_basic_allgatherv_inter(void *sbuf, int scount,
                                        struct ompi_datatype_t *sdtype,
                                        void *rbuf, int *rcounts,
                                        int *disps,
                                        struct ompi_datatype_t *rdtype,
                                        struct ompi_communicator_t *comm,
                                        mca_coll_base_module_t *module);

    int mca_coll_basic_allreduce_intra(void *sbuf, void *rbuf, int count,
                                       struct ompi_datatype_t *dtype,
                                       struct ompi_op_t *op,
                                       struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t *module);
    int mca_coll_basic_allreduce_inter(void *sbuf, void *rbuf, int count,
                                       struct ompi_datatype_t *dtype,
                                       struct ompi_op_t *op,
                                       struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t *module);

    int mca_coll_basic_alltoall_intra(void *sbuf, int scount,
                                      struct ompi_datatype_t *sdtype,
                                      void *rbuf, int rcount,
                                      struct ompi_datatype_t *rdtype,
                                      struct ompi_communicator_t *comm,
                                      mca_coll_base_module_t *module);
    int mca_coll_basic_alltoall_inter(void *sbuf, int scount,
                                      struct ompi_datatype_t *sdtype,
                                      void *rbuf, int rcount,
                                      struct ompi_datatype_t *rdtype,
                                      struct ompi_communicator_t *comm,
                                      mca_coll_base_module_t *module);

    int mca_coll_basic_alltoallv_intra(void *sbuf, int *scounts,
                                       int *sdisps,
                                       struct ompi_datatype_t *sdtype,
                                       void *rbuf, int *rcounts,
                                       int *rdisps,
                                       struct ompi_datatype_t *rdtype,
                                       struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t *module);
    int mca_coll_basic_alltoallv_inter(void *sbuf, int *scounts,
                                       int *sdisps,
                                       struct ompi_datatype_t *sdtype,
                                       void *rbuf, int *rcounts,
                                       int *rdisps,
                                       struct ompi_datatype_t *rdtype,
                                       struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t *module);

    int mca_coll_basic_alltoallw_intra(void *sbuf, int *scounts,
                                       int *sdisps,
                                       struct ompi_datatype_t **sdtypes,
                                       void *rbuf, int *rcounts,
                                       int *rdisps,
                                       struct ompi_datatype_t **rdtypes,
                                       struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t *module);
    int mca_coll_basic_alltoallw_inter(void *sbuf, int *scounts,
                                       int *sdisps,
                                       struct ompi_datatype_t **sdtypes,
                                       void *rbuf, int *rcounts,
                                       int *rdisps,
                                       struct ompi_datatype_t **rdtypes,
                                       struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t *module);

    int mca_coll_basic_barrier_intra_lin(struct ompi_communicator_t *comm,
                                         mca_coll_base_module_t *module);

    int mca_coll_basic_barrier_inter_lin(struct ompi_communicator_t *comm,
                                         mca_coll_base_module_t *module);

    int mca_coll_basic_barrier_intra_log(struct ompi_communicator_t *comm,
                                         mca_coll_base_module_t *module);

    int mca_coll_basic_bcast_lin_intra(void *buff, int count,
                                       struct ompi_datatype_t *datatype,
                                       int root,
                                       struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t *module);

    int mca_coll_basic_bcast_lin_inter(void *buff, int count,
                                       struct ompi_datatype_t *datatype,
                                       int root,
                                       struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t *module);

    int mca_coll_basic_bcast_log_intra(void *buff, int count,
                                       struct ompi_datatype_t *datatype,
                                       int root,
                                       struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t *module);

    int mca_coll_basic_bcast_log_inter(void *buff, int count,
                                       struct ompi_datatype_t *datatype,
                                       int root,
                                       struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t *module);

    int mca_coll_basic_exscan_intra(void *sbuf, void *rbuf, int count,
                                    struct ompi_datatype_t *dtype,
                                    struct ompi_op_t *op,
                                    struct ompi_communicator_t *comm,
                                    mca_coll_base_module_t *module);

    int mca_coll_basic_exscan_inter(void *sbuf, void *rbuf, int count,
                                    struct ompi_datatype_t *dtype,
                                    struct ompi_op_t *op,
                                    struct ompi_communicator_t *comm,
                                    mca_coll_base_module_t *module);

    int mca_coll_basic_gather_intra(void *sbuf, int scount,
                                    struct ompi_datatype_t *sdtype,
                                    void *rbuf, int rcount,
                                    struct ompi_datatype_t *rdtype,
                                    int root,
                                    struct ompi_communicator_t *comm,
                                    mca_coll_base_module_t *module);
    int mca_coll_basic_gather_inter(void *sbuf, int scount,
                                    struct ompi_datatype_t *sdtype,
                                    void *rbuf, int rcount,
                                    struct ompi_datatype_t *rdtype,
                                    int root,
                                    struct ompi_communicator_t *comm,
                                    mca_coll_base_module_t *module);

    int mca_coll_basic_gatherv_intra(void *sbuf, int scount,
                                     struct ompi_datatype_t *sdtype,
                                     void *rbuf, int *rcounts, int *disps,
                                     struct ompi_datatype_t *rdtype,
                                     int root,
                                     struct ompi_communicator_t *comm,
                                     mca_coll_base_module_t *module);

    int mca_coll_basic_gatherv_inter(void *sbuf, int scount,
                                     struct ompi_datatype_t *sdtype,
                                     void *rbuf, int *rcounts, int *disps,
                                     struct ompi_datatype_t *rdtype,
                                     int root,
                                     struct ompi_communicator_t *comm,
                                     mca_coll_base_module_t *module);

    int mca_coll_basic_reduce_lin_intra(void *sbuf, void *rbuf, int count,
                                        struct ompi_datatype_t *dtype,
                                        struct ompi_op_t *op,
                                        int root,
                                        struct ompi_communicator_t *comm,
                                        mca_coll_base_module_t *module);
    int mca_coll_basic_reduce_lin_inter(void *sbuf, void *rbuf, int count,
                                        struct ompi_datatype_t *dtype,
                                        struct ompi_op_t *op,
                                        int root,
                                        struct ompi_communicator_t *comm,
                                        mca_coll_base_module_t *module);

    int mca_coll_basic_reduce_log_intra(void *sbuf, void *rbuf, int count,
                                        struct ompi_datatype_t *dtype,
                                        struct ompi_op_t *op,
                                        int root,
                                        struct ompi_communicator_t *comm,
                                        mca_coll_base_module_t *module);
    int mca_coll_basic_reduce_log_inter(void *sbuf, void *rbuf, int count,
                                        struct ompi_datatype_t *dtype,
                                        struct ompi_op_t *op,
                                        int root,
                                        struct ompi_communicator_t *comm,
                                        mca_coll_base_module_t *module);

    int mca_coll_basic_reduce_scatter_intra(void *sbuf, void *rbuf,
                                            int *rcounts,
                                            struct ompi_datatype_t *dtype,
                                            struct ompi_op_t *op,
                                            struct ompi_communicator_t *comm,
                                            mca_coll_base_module_t *module);
    int mca_coll_basic_reduce_scatter_inter(void *sbuf, void *rbuf,
                                            int *rcounts,
                                            struct ompi_datatype_t *dtype,
                                            struct ompi_op_t *op,
                                            struct ompi_communicator_t *comm,
                                            mca_coll_base_module_t *module);

    int mca_coll_basic_scan_intra(void *sbuf, void *rbuf, int count,
                                  struct ompi_datatype_t *dtype,
                                  struct ompi_op_t *op,
                                  struct ompi_communicator_t *comm,
                                  mca_coll_base_module_t *module);
    int mca_coll_basic_scan_inter(void *sbuf, void *rbuf, int count,
                                  struct ompi_datatype_t *dtype,
                                  struct ompi_op_t *op,
                                  struct ompi_communicator_t *comm,
                                  mca_coll_base_module_t *module);

    int mca_coll_basic_scatter_intra(void *sbuf, int scount,
                                     struct ompi_datatype_t *sdtype,
                                     void *rbuf, int rcount,
                                     struct ompi_datatype_t *rdtype,
                                     int root,
                                     struct ompi_communicator_t *comm,
                                     mca_coll_base_module_t *module);
    int mca_coll_basic_scatter_inter(void *sbuf, int scount,
                                     struct ompi_datatype_t *sdtype,
                                     void *rbuf, int rcount,
                                     struct ompi_datatype_t *rdtype,
                                     int root,
                                     struct ompi_communicator_t *comm,
                                     mca_coll_base_module_t *module);

    int mca_coll_basic_scatterv_intra(void *sbuf, int *scounts, int *disps,
                                      struct ompi_datatype_t *sdtype,
                                      void *rbuf, int rcount,
                                      struct ompi_datatype_t *rdtype,
                                      int root,
                                      struct ompi_communicator_t *comm,
                                      mca_coll_base_module_t *module);
    int mca_coll_basic_scatterv_inter(void *sbuf, int *scounts, int *disps,
                                      struct ompi_datatype_t *sdtype,
                                      void *rbuf, int rcount,
                                      struct ompi_datatype_t *rdtype,
                                      int root,
                                      struct ompi_communicator_t *comm,
                                      mca_coll_base_module_t *module);

    int mca_coll_basic_ft_event(int status);


/* Utility functions */

    static inline void mca_coll_basic_free_reqs(ompi_request_t ** reqs,
                                                int count)
    {
        int i;
        for (i = 0; i < count; ++i)
             ompi_request_free(&reqs[i]);
    }


struct mca_coll_basic_module_t {
    mca_coll_base_module_t super;

    ompi_request_t **mccb_reqs;
    int mccb_num_reqs;
};
typedef struct mca_coll_basic_module_t mca_coll_basic_module_t;
OBJ_CLASS_DECLARATION(mca_coll_basic_module_t);

END_C_DECLS

#endif /* MCA_COLL_BASIC_EXPORT_H */
