#ifndef MCA_COLL_FCA_CONVERTOR_H
#define MCA_COLL_FCA_CONVERTOR_H




enum {
    MCA_COLL_CONVERTOR_NULL = 0,
    MCA_COLL_FCA_CONV_SEND,
    MCA_COLL_FCA_CONV_RECV
};


struct mca_coll_fca_convertor {
    int               type;
    FCA_CONVERTOR_T  ompic;
    size_t            size;
    void              *buf;
};

#define MCA_COLL_FCA_DECLARE_CONVERTOR(__name) \
    struct mca_coll_fca_convertor __name = {MCA_COLL_CONVERTOR_NULL}


static inline void mca_coll_fca_convertor_set(struct mca_coll_fca_convertor *conv,
                                              struct ompi_datatype_t *datatype,
                                              void *buffer, int count)
{
    if (conv->type == MCA_COLL_FCA_CONV_SEND) {
        FCA_CONVERTOR_COPY_AND_PREPARE_FOR_SEND(ompi_mpi_local_convertor,
                                                 &datatype->super, count,
                                                 buffer, 0, &conv->ompic);
    } else if (conv->type == MCA_COLL_FCA_CONV_RECV) {
        FCA_CONVERTOR_COPY_AND_PREPARE_FOR_RECV(ompi_mpi_local_convertor,
                                                 &datatype->super, count,
                                                 buffer, 0, &conv->ompic);
    }
}

static inline void mca_coll_fca_convertor_create(struct mca_coll_fca_convertor *conv,
                                                 struct ompi_datatype_t *datatype,
                                                 int count, void *buffer, int type,
                                                 void **tmpbuf, size_t *size)
{
    OBJ_CONSTRUCT(&conv->ompic, FCA_CONVERTOR_T);
    conv->type = type;
    mca_coll_fca_convertor_set(conv, datatype, buffer, count);
    FCA_CONVERTOR_CONVERTOR_GET_PACKED_SIZE(&conv->ompic, &conv->size);
    conv->buf = malloc(conv->size);
    *tmpbuf = conv->buf;
    *size = conv->size;
}

static inline int mca_coll_fca_convertor_valid(struct mca_coll_fca_convertor *conv)
{
    return conv->type != MCA_COLL_CONVERTOR_NULL;
}

static inline void mca_coll_fca_convertor_destroy(struct mca_coll_fca_convertor *conv)
{
    if (mca_coll_fca_convertor_valid(conv)) {
        free(conv->buf);
        OBJ_DESTRUCT(&conv->ompic);
    }
}

static inline int32_t mca_coll_fca_convertor_process(struct mca_coll_fca_convertor *conv,
                                                     size_t offset)
{
    struct iovec invec;
    unsigned iov_count;
    size_t size;

    iov_count = 1;
    invec.iov_base = (char*)conv->buf + offset;
    invec.iov_len = conv->size;
    size = conv->size;

    if (conv->type == MCA_COLL_FCA_CONV_SEND) {
        return FCA_CONVERTOR_CONVERTOR_PACK(&conv->ompic, &invec, &iov_count, &size);
    } else if (conv->type == MCA_COLL_FCA_CONV_RECV) {
        return FCA_CONVERTOR_CONVERTOR_UNPACK(&conv->ompic, &invec, &iov_count, &size);
    }
    return 0;
}
#endif
