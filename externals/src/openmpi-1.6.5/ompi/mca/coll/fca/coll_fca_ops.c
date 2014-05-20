/**
  Copyright (c) 2011 Mellanox Technologies. All rights reserved.
  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"
#include "coll_fca.h"
#include "coll_fca_convertor.h"


static mca_coll_fca_dtype_info_t* mca_coll_fca_get_dtype(ompi_datatype_t *dtype)
{
    mca_coll_fca_dtype_info_t *dtype_info;
    ptrdiff_t lb, extent;
    int id = dtype->id;
    int fca_dtype;

    if (id < 0 || id >= FCA_DT_MAX_PREDEFINED) {
        return NULL;
    }

    /* Different dtype structures may have the same id. In that case, we assume
     * they are aliases.
     */
    dtype_info = &mca_coll_fca_component.fca_dtypes[id];
    if (dtype_info->mpi_dtype->id == id) {
        return dtype_info;
    }

    /* assert we don't overwrite another datatype */
    assert(dtype_info->mpi_dtype == MPI_DATATYPE_NULL);

    fca_dtype = fca_translate_mpi_dtype(dtype->name);
    if (fca_dtype < 0) {
        return NULL;
    }

    FCA_DT_GET_TRUE_EXTENT(dtype, &lb, &extent);
    dtype_info->mpi_dtype = dtype;
    dtype_info->mpi_dtype_extent = extent;
    dtype_info->fca_dtype = fca_dtype;
    dtype_info->fca_dtype_extent = fca_get_dtype_size(fca_dtype);
    FCA_VERBOSE(2, "Added new dtype[%d]: %s fca id: %d, mpi size: %lu, fca size: %lu",
                id, dtype->name, dtype_info->fca_dtype, dtype_info->mpi_dtype_extent,
                dtype_info->fca_dtype_extent);
    return dtype_info;
}

static mca_coll_fca_op_info_t *mca_coll_fca_get_op(ompi_op_t *op)
{
    mca_coll_fca_op_info_t *op_info;
    int i, fca_op;

    /*
     * Find 'op' in the array by exhaustive search. We assume all valid ops are
     * in the beginning. If we stumble on a MPI_OP_NULL, we try to resolve the FCA
     * operation code and store it in the array.
     */
    op_info = mca_coll_fca_component.fca_reduce_ops;
    for (i = 0; i < FCA_MAX_OPS; ++i, ++op_info) {
        if (op_info->mpi_op == op) {
            return op_info;
        } else if (op_info->mpi_op == MPI_OP_NULL) {
            fca_op = fca_translate_mpi_op(op->o_name);
            if (fca_op < 0)
                return NULL;
            op_info->mpi_op = op;
            op_info->fca_op = fca_op;
            FCA_VERBOSE(2, "Added new op[%d]: %s fca id: %d", i, op->o_name, fca_op);
            return op_info;
        }
    }
    /* assert the array does not overflow */
    /*assert(mca_coll_fca_component.fca_reduce_ops[FCA_MAX_OPS - 1] == MPI_OP_NULL);*/
    return NULL;
}

/**
 * If "datatype" is contiguous when it appears "count" times, return 1 and
 * set "*size" to the total buffer size, and *gap to the gap before the data.
 * Otherwise return 0.
 */
static inline int mca_coll_fca_array_size(ompi_datatype_t *dtype, int count,
                                          size_t *gap, size_t *size)
{
    ptrdiff_t true_lb, true_extent;

    if (FCA_DT_IS_CONTIGUOUS_MEMORY_LAYOUT(dtype, count)) {
        FCA_DT_GET_TRUE_EXTENT(dtype, &true_lb, &true_extent);
        *gap = true_lb;
        *size = true_extent * count;
        return 1;
    } else {
        return 0;
    }
}

static int mca_coll_fca_fill_reduce_spec(int count, ompi_datatype_t *dtype,
                                         ompi_op_t *op, fca_reduce_spec_t *spec,
                                         int max_fca_payload)
{
    mca_coll_fca_dtype_info_t *dtype_info;
    mca_coll_fca_op_info_t *op_info;

    /* Check dtype */
    dtype_info = mca_coll_fca_get_dtype(dtype);
    if (!dtype_info) {
        FCA_VERBOSE(10, "Unsupported dtype: %s", dtype->name);
        return OMPI_ERROR;
    }

    /* Check FCA size */
    if ((int)(dtype_info->fca_dtype_extent * count) > max_fca_payload) {
        FCA_VERBOSE(10, "Unsupported buffer size: %lu", dtype_info->fca_dtype_extent * count);
        return OMPI_ERROR;
    }

    /* Check operation */
    op_info = mca_coll_fca_get_op(op);
    if (!op_info) {
        FCA_VERBOSE(10, "Unsupported op: %s", op->o_name);
        return OMPI_ERROR;
    }

    /* Fill spec */
    spec->dtype = dtype_info->fca_dtype;
    spec->op = op_info->fca_op;
    spec->length = count;
    spec->buf_size = dtype_info->mpi_dtype_extent * count;
    if (MPI_IN_PLACE == spec->sbuf) {
        FCA_VERBOSE(10, "Using MPI_IN_PLACE for sbuf");
        spec->sbuf = spec->rbuf;
    } else if (MPI_IN_PLACE == spec->rbuf) {
        FCA_VERBOSE(10, "Using MPI_IN_PLACE for rbuf");
        spec->rbuf = spec->sbuf;
    }
    return OMPI_SUCCESS;
}

/*
 *  Function:   - barrier
 *  Returns:    - MPI_SUCCESS or error code
 */
int mca_coll_fca_barrier(struct ompi_communicator_t *comm,
                         mca_coll_base_module_t *module)
{
    mca_coll_fca_module_t *fca_module = (mca_coll_fca_module_t*)module;
    int ret;

    FCA_VERBOSE(5,"Using FCA Barrier");
    ret = fca_do_barrier(fca_module->fca_comm);
    if (ret < 0) {
        if (ret == -EUSEMPI) {
            goto orig_barrier;
        }
        FCA_ERROR("Barrier failed: %s", fca_strerror(ret));
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;

orig_barrier:
    return fca_module->previous_barrier(comm, fca_module->previous_barrier_module);

}

/*
 *  Function:   - broadcast
 *  Accepts:    - same arguments as MPI_Bcast()
 *  Returns:    - MPI_SUCCESS or error code
 */
int mca_coll_fca_bcast(void *buff, int count, struct ompi_datatype_t *datatype,
                       int root, struct ompi_communicator_t *comm,
                       mca_coll_base_module_t *module)
{
    mca_coll_fca_module_t *fca_module = (mca_coll_fca_module_t*)module;
    MCA_COLL_FCA_DECLARE_CONVERTOR(conv);
    fca_bcast_spec_t spec;
    size_t gap, size;
    int ret;

    FCA_VERBOSE(5, "[%d] Calling mca_coll_fca_bcast, root=%d, count=%d",
                ompi_comm_rank(comm), root, count);

    /* Setup exchange buffer */
    spec.root = root;
    if (mca_coll_fca_array_size(datatype, count, &gap, &size)) {
        spec.buf = (char*)buff + gap;
    } else {
        mca_coll_fca_convertor_create(&conv, datatype, count, buff,
                                      (root == fca_module->rank)
                                                    ? MCA_COLL_FCA_CONV_SEND
                                                    : MCA_COLL_FCA_CONV_RECV,
                                      &spec.buf, &size);
    }

    /* Check that operation size does not exceed limit */
    spec.size = size;
    if (spec.size > fca_module->fca_comm_caps.max_payload) {
         FCA_VERBOSE(5, "Unsupported bcast operation size %d, using fallback",
                     spec.size);
         if (spec.buf != buff) {
             mca_coll_fca_convertor_destroy(&conv);
         }
         goto orig_bcast;
    }

    /* Sender may pack data */
    if (spec.buf != buff && root == fca_module->rank) {
        mca_coll_fca_convertor_process(&conv, 0);
    }

    /* Call FCA Bcast */
    FCA_VERBOSE(5, "Using FCA Bcast");
    ret = fca_do_bcast(fca_module->fca_comm, &spec);

    /* Destroy convertor if operation failed */
    if (ret < 0) {
        mca_coll_fca_convertor_destroy(&conv);
        if (ret == -EUSEMPI) {
            goto orig_bcast;
        }
        FCA_ERROR("Bcast failed: %s", fca_strerror(ret));
        return OMPI_ERROR;
    }

    /* Unpack data and clean up convertor */
    if (mca_coll_fca_convertor_valid(&conv)) {
        if (root != fca_module->rank) {
            mca_coll_fca_convertor_process(&conv, 0);
        }
        mca_coll_fca_convertor_destroy(&conv);
    }
    return OMPI_SUCCESS;

orig_bcast:
    return fca_module->previous_bcast(buff, count, datatype, root, comm,
                                      fca_module->previous_bcast_module);
}

/*
 *  Reduce
 *
 *  Function:   - reduce
 *  Accepts:    - same as MPI_Reduce()
 *  Returns:    - MPI_SUCCESS or error code
 */
int mca_coll_fca_reduce(void *sbuf, void *rbuf, int count,
                        struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                        int root, struct ompi_communicator_t *comm,
                        mca_coll_base_module_t *module)
{
    mca_coll_fca_module_t *fca_module = (mca_coll_fca_module_t*)module;
    fca_reduce_spec_t spec;
    int ret;

    mca_coll_fca_get_reduce_root(root, fca_module->rank, &spec);
    spec.sbuf = sbuf;
    spec.rbuf = rbuf;
    if (mca_coll_fca_fill_reduce_spec(count, dtype, op, &spec,
                                      fca_module->fca_comm_caps.max_payload)
            != OMPI_SUCCESS) {
        FCA_VERBOSE(5, "Unsupported reduce operation %s, using fallback\n", op->o_name);
        goto orig_reduce;
    }

    FCA_VERBOSE(5,"Using FCA Reduce");
    ret = fca_do_reduce(fca_module->fca_comm, &spec);
    if (ret < 0) {
        if (ret == -EUSEMPI) {
            goto orig_reduce;
        }
        FCA_ERROR("Reduce failed: %s", fca_strerror(ret));
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;

orig_reduce:
    return fca_module->previous_reduce(sbuf, rbuf, count, dtype, op, root,
                                       comm, fca_module->previous_reduce_module);
}

/*
 *  Allreduce
 *
 *  Function:   - allreduce
 *  Accepts:    - same as MPI_Allreduce()
 *  Returns:    - MPI_SUCCESS or error code
 */
int mca_coll_fca_allreduce(void *sbuf, void *rbuf, int count,
                           struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                           struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module)
{
    mca_coll_fca_module_t *fca_module = (mca_coll_fca_module_t*)module;
    fca_reduce_spec_t spec;
    int ret;

    spec.sbuf = sbuf;
    spec.rbuf = rbuf;
    if (mca_coll_fca_fill_reduce_spec(count, dtype, op, &spec,
                                      fca_module->fca_comm_caps.max_payload)
            != OMPI_SUCCESS) {
        FCA_VERBOSE(5, "Unsupported allreduce operation %s, using fallback\n", op->o_name);
        goto orig_allreduce;
    }

    FCA_VERBOSE(5,"Using FCA Allreduce");
    ret = fca_do_all_reduce(fca_module->fca_comm, &spec);
    if (ret < 0) {
        if (ret == -EUSEMPI) {
            goto orig_allreduce;
        }
        FCA_ERROR("Allreduce failed: %s", fca_strerror(ret));
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;

orig_allreduce:
    return fca_module->previous_allreduce(sbuf, rbuf, count, dtype, op, comm,
                                          fca_module->previous_allreduce_module);
}

/*
 * Prepare a send buffer for allgather/allgatherv, handle packing and MPI_IN_PLACE.
 */
static size_t __setup_gather_sendbuf(void *sbuf, int scount,
                                     struct ompi_datatype_t *sdtype,
                                     struct mca_coll_fca_convertor *sconv,
                                     void **real_sendbuf)
{
    size_t gap, ssize;

    if (mca_coll_fca_array_size(sdtype, scount, &gap, &ssize)) {
        *real_sendbuf = (char*) sbuf + gap;
    } else {
        FCA_VERBOSE(5, "Packing send buffer");
        mca_coll_fca_convertor_create(sconv, sdtype, scount, sbuf,
                                      MCA_COLL_FCA_CONV_SEND, real_sendbuf,
                                      &ssize);
        mca_coll_fca_convertor_process(sconv, 0);
    }
    return ssize;
}

static size_t __setup_gather_sendbuf_inplace(void *inplace_sbuf, int rcount,
                                             struct ompi_datatype_t *rdtype,
                                             struct mca_coll_fca_convertor *sconv,
                                             void **real_sendbuf)
{
    size_t gap, ssize;

    if (mca_coll_fca_array_size(rdtype, rcount, &gap, &ssize)) {
        *real_sendbuf = (char*) inplace_sbuf + gap;
    } else {
        FCA_VERBOSE(5, "Packing send buffer");
        mca_coll_fca_convertor_create(sconv, rdtype, rcount, inplace_sbuf,
                                      MCA_COLL_FCA_CONV_SEND, real_sendbuf,
                                      &ssize);
        mca_coll_fca_convertor_process(sconv, 0);
    }
    return ssize;
}

/*
 *  Allgather
 *
 *  Function:   - allgather
 *  Accepts:    - same as MPI_Allgather()
 *  Returns:    - MPI_SUCCESS or error code
 */
int mca_coll_fca_allgather(void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                           void *rbuf, int rcount, struct ompi_datatype_t *rdtype,
                           struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module)
{
    mca_coll_fca_module_t *fca_module = (mca_coll_fca_module_t*)module;
#if OMPI_FCA_ALLGATHER == 1
    MCA_COLL_FCA_DECLARE_CONVERTOR(sconv);
    MCA_COLL_FCA_DECLARE_CONVERTOR(rconv);
    fca_gather_spec_t spec = {0,};
    size_t rgap, rsize;
    ptrdiff_t rdtype_extent;
    ssize_t total_rcount;
    int ret;

    /* Setup send buffer */
    if(sbuf == MPI_IN_PLACE ) {
        FCA_DT_EXTENT(rdtype, &rdtype_extent);
        spec.size = __setup_gather_sendbuf_inplace(
                        (char *)rbuf + rcount * fca_module->rank * rdtype_extent,
                        rcount, rdtype, &sconv, &spec.sbuf);
    } else {
        spec.size = __setup_gather_sendbuf(sbuf, scount, sdtype, &sconv, &spec.sbuf);
    }

    /* Setup recv buffer */
    total_rcount = ompi_comm_size(comm) * rcount;
    if (mca_coll_fca_array_size(rdtype, total_rcount, &rgap, &rsize) && rgap == 0) {
        spec.rbuf = rbuf;
    } else {
        mca_coll_fca_convertor_create(&rconv, rdtype, total_rcount, rbuf,
                                      MCA_COLL_FCA_CONV_RECV, &spec.rbuf, &rsize);
    }


    /* Call FCA Allgather */
    FCA_VERBOSE(5,"Using FCA Allgather size");
    ret = fca_do_allgather(fca_module->fca_comm, &spec);

    /* Destroy convertors if operation failed */
    if (ret < 0) {
        mca_coll_fca_convertor_destroy(&sconv);
        mca_coll_fca_convertor_destroy(&rconv);
        if (ret == -EUSEMPI) {
            goto orig_allgather;
        }
        FCA_ERROR("Allgather failed: %s", fca_strerror(ret));
        return OMPI_ERROR;
    }

    /* Unpack data and clean up convertor */
    mca_coll_fca_convertor_destroy(&sconv);
    if (mca_coll_fca_convertor_valid(&rconv)) {
        FCA_VERBOSE(5, "Unpacking Allgather receive buffer");
        mca_coll_fca_convertor_process(&rconv, 0);
        mca_coll_fca_convertor_destroy(&rconv);
    }
    return OMPI_SUCCESS;

orig_allgather:
#endif
    return fca_module->previous_allgather(sbuf, scount, sdtype, rbuf, rcount, rdtype,
                                          comm, fca_module->previous_allgather_module);
}

int mca_coll_fca_allgatherv(void *sbuf, int scount,
                           struct ompi_datatype_t *sdtype,
                           void *rbuf, int *rcounts, int *disps,
                           struct ompi_datatype_t *rdtype,
                           struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module)
{
    mca_coll_fca_module_t *fca_module = (mca_coll_fca_module_t*)module;
#if OMPI_FCA_ALLGATHER == 1
    MCA_COLL_FCA_DECLARE_CONVERTOR(sconv);
    MCA_COLL_FCA_DECLARE_CONVERTOR(rconv);
    fca_gatherv_spec_t spec;
    size_t rgap, rsize;
    int sum_rcounts;
    ptrdiff_t rdtype_extent;
    int comm_size;
    int relemsize;
    size_t displ;
    int i, ret;

    comm_size = ompi_comm_size(fca_module->comm);
    FCA_DT_EXTENT(rdtype, &rdtype_extent);

    /* Setup send buffer */
    if(sbuf == MPI_IN_PLACE ) {
        spec.sendsize = __setup_gather_sendbuf_inplace(
                                (char *)rbuf + disps[fca_module->rank] * rdtype_extent,
                                rcounts[fca_module->rank], rdtype, &sconv, &spec.sbuf);
    } else {
        spec.sendsize = __setup_gather_sendbuf(sbuf, scount, sdtype, &sconv, &spec.sbuf);
    }

    /* Allocate alternative recvsizes/displs on the stack, which will be in bytes */
    spec.recvsizes = alloca(sizeof *spec.recvsizes * comm_size);
    spec.displs = alloca(sizeof *spec.displs * comm_size);

    /* Calculate the size of receive buffer */
    sum_rcounts = 0;
    for (i = 0; i < comm_size; ++i) {
        sum_rcounts += rcounts[i];
    }

    /* convert MPI counts which depend on dtype) to FCA sizes (which are in bytes) */
    if (mca_coll_fca_array_size(rdtype, sum_rcounts, &rgap, &rsize) && rgap == 0) {
        spec.rbuf = rbuf;
        for (i = 0; i < comm_size; ++i) {
            spec.recvsizes[i] = rcounts[i] * rdtype_extent;
            spec.displs[i] = disps[i] * rdtype_extent;
        }
    } else {
        /*
         * Reorder and remove gaps in displs - we want to allocate as little memory
         * as possible, and we should unpack one-by-one anyway.
         */
        FCA_VERBOSE(5, "Reordering AllgatherV displacements");
        mca_coll_fca_convertor_create(&rconv, rdtype, sum_rcounts, rbuf,
                                      MCA_COLL_FCA_CONV_RECV, &spec.rbuf, &rsize);
        assert(rsize % sum_rcounts == 0);
        relemsize = rsize / sum_rcounts;

        displ = 0;
        for (i = 0; i < comm_size; ++i) {
            spec.recvsizes[i] = rcounts[i] * relemsize;
            spec.displs[i] = displ;
            displ += spec.recvsizes[i];
        }
        assert(displ == rsize);
    }

    /* Call FCA AllgatherV */
    FCA_VERBOSE(5,"Using FCA Allgatherv");
    ret = fca_do_allgatherv(fca_module->fca_comm, &spec);

    /* Destroy convertors if operation failed */
    if (ret < 0) {
        mca_coll_fca_convertor_destroy(&sconv);
        mca_coll_fca_convertor_destroy(&rconv);
        if (ret == -EUSEMPI) {
            goto orig_allgatherv;
        }
        FCA_ERROR("Allgatherv failed: %s", fca_strerror(ret));
        return OMPI_ERROR;
    }

    /* Unpack data and clean up convertor */
    mca_coll_fca_convertor_destroy(&sconv);
    if (mca_coll_fca_convertor_valid(&rconv)) {
        FCA_VERBOSE(5, "Unpacking AllgatherV receive buffer rdtype_extent=%ld",
                    rdtype_extent);
        for (i = 0; i < comm_size; ++i) {
            mca_coll_fca_convertor_set(&rconv, rdtype,
                                       (char*)rbuf + disps[i] * rdtype_extent,
                                       rcounts[i]);
            mca_coll_fca_convertor_process(&rconv, spec.displs[i]);
        }
        mca_coll_fca_convertor_destroy(&rconv);
    }
    return OMPI_SUCCESS;

orig_allgatherv:
#endif
    return fca_module->previous_allgatherv(sbuf, scount, sdtype, rbuf, rcounts,
                                           disps, rdtype, comm,
                                           fca_module->previous_allgatherv_module);
}

int mca_coll_fca_alltoall(void *sbuf, int scount,
                            struct ompi_datatype_t *sdtype,
                            void *rbuf, int rcount,
                            struct ompi_datatype_t *rdtype,
                            struct ompi_communicator_t *comm,
                            mca_coll_base_module_t *module)
{
    mca_coll_fca_module_t *fca_module = (mca_coll_fca_module_t*)module;
    /* not implemented yet */
    return fca_module->previous_alltoall(sbuf, scount, sdtype, rbuf, rcount, rdtype,
                                         comm, fca_module->previous_alltoall_module);
}

int mca_coll_fca_alltoallv(void *sbuf, int *scounts, int *sdisps,
                           struct ompi_datatype_t *sdtype,
                           void *rbuf, int *rcounts, int *rdisps,
                           struct ompi_datatype_t *rdtype,
                           struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module)
{
    mca_coll_fca_module_t *fca_module = (mca_coll_fca_module_t*)module;
    /* not implemented yet */
    return fca_module->previous_alltoallv(sbuf, scounts, sdisps, sdtype, rbuf, rcounts, rdisps, rdtype,
                                          comm, fca_module->previous_alltoallv_module);
}

int mca_coll_fca_alltoallw(void *sbuf, int *scounts, int *sdisps,
                           struct ompi_datatype_t **sdtypes,
                           void *rbuf, int *rcounts, int *rdisps,
                           struct ompi_datatype_t **rdtypes,
                           struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module)
{
    mca_coll_fca_module_t *fca_module = (mca_coll_fca_module_t*)module;
    /* not implemented yet */
    return fca_module->previous_alltoallw(sbuf, scounts, sdisps, sdtypes, rbuf, rcounts, rdisps, rdtypes,
                                          comm, fca_module->previous_alltoallw_module);
}

int mca_coll_fca_gather(void *sbuf, int scount,
                        struct ompi_datatype_t *sdtype,
                        void *rbuf, int rcount,
                        struct ompi_datatype_t *rdtype,
                        int root, struct ompi_communicator_t *comm,
                        mca_coll_base_module_t *module)
{
    mca_coll_fca_module_t *fca_module = (mca_coll_fca_module_t*)module;
    /* not implemented yet */
    return fca_module->previous_gather(sbuf, scount, sdtype, rbuf, rcount, rdtype, root,
                                          comm, fca_module->previous_gather_module);
}

int mca_coll_fca_gatherv(void *sbuf, int scount,
                         struct ompi_datatype_t *sdtype,
                         void *rbuf, int *rcounts, int *disps,
                         struct ompi_datatype_t *rdtype, int root,
                         struct ompi_communicator_t *comm,
                         mca_coll_base_module_t *module)
{
    mca_coll_fca_module_t *fca_module = (mca_coll_fca_module_t*)module;
    /* not implemented yet */
    return fca_module->previous_gatherv(sbuf, scount, sdtype, rbuf, rcounts, disps, rdtype, root,
                                          comm, fca_module->previous_gatherv_module);
}

int mca_coll_fca_reduce_scatter(void *sbuf, void *rbuf, int *rcounts,
                                struct ompi_datatype_t *dtype,
                                struct ompi_op_t *op,
                                struct ompi_communicator_t *comm,
                                mca_coll_base_module_t *module)
{
    mca_coll_fca_module_t *fca_module = (mca_coll_fca_module_t*)module;
    /* not implemented yet */
    return fca_module->previous_reduce_scatter(sbuf, rbuf, rcounts, dtype, op,
                                          comm, fca_module->previous_reduce_scatter_module);
}
