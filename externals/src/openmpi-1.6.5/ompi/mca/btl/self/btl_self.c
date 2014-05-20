/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

#include "opal/class/opal_bitmap.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/sys/atomic.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/mpool/base/base.h"
#include "btl_self.h"
#include "btl_self_frag.h"
#include "ompi/proc/proc.h"

mca_btl_base_module_t mca_btl_self = {
    &mca_btl_self_component.super,
    0, /* btl_eager_limit */
    0, /* btl_rndv_eager_limit */
    0, /* btl_max_send_size */
    0, /* btl_rdma_pipeline_send_length */
    0, /* btl_rdma_pipeline_frag_size */
    0, /* btl_min_rdma_pipeline_size */
    0, /* btl_exclusivity */
    0, /* btl_latency */
    0, /* btl_bandwidth */
    0, /* btl flags */
    mca_btl_self_add_procs,
    mca_btl_self_del_procs,
    NULL,
    mca_btl_self_finalize,
    mca_btl_self_alloc,
    mca_btl_self_free,
    mca_btl_self_prepare_src,
    mca_btl_self_prepare_dst,
    mca_btl_self_send,
    NULL, /* send immediate */
    mca_btl_self_rdma,  /* put */
    mca_btl_self_rdma,  /* get */
    mca_btl_base_dump,
    NULL, /* mpool */
    NULL, /* register error cb */
    mca_btl_self_ft_event
};


int mca_btl_self_add_procs( struct mca_btl_base_module_t* btl, 
                            size_t nprocs, 
                            struct ompi_proc_t **procs, 
                            struct mca_btl_base_endpoint_t **peers,
                            opal_bitmap_t* reachability )
{
    int i;

    for( i = 0; i < (int)nprocs; i++ ) {
        if( procs[i] == ompi_proc_local_proc ) {
            opal_bitmap_set_bit( reachability, i );
            break;  /* there will always be only one ... */
        }
    }
    return OMPI_SUCCESS;
}


int mca_btl_self_del_procs( struct mca_btl_base_module_t* btl, 
                            size_t nprocs,
                            struct ompi_proc_t **procs, 
                            struct mca_btl_base_endpoint_t **peers )
{
    return OMPI_SUCCESS;
}


/**
 * MCA->BTL Clean up any resources held by BTL module
 * before the module is unloaded.
 *
 * @param btl (IN)   BTL module.
 *
 * Prior to unloading a BTL module, the MCA framework will call
 * the BTL finalize method of the module. Any resources held by
 * the BTL should be released and if required the memory corresponding
 * to the BTL module freed.
 *
 */

int mca_btl_self_finalize(struct mca_btl_base_module_t* btl)
{
    return OMPI_SUCCESS;
}


/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */
mca_btl_base_descriptor_t* mca_btl_self_alloc(
        struct mca_btl_base_module_t* btl,
        struct mca_btl_base_endpoint_t* endpoint,
        uint8_t order,
        size_t size,
        uint32_t flags)
{
    mca_btl_self_frag_t* frag = NULL;
    int rc;
    if(size <= mca_btl_self.btl_eager_limit) {
        MCA_BTL_SELF_FRAG_ALLOC_EAGER(frag,rc);
    } else if (size <= btl->btl_max_send_size) {
        MCA_BTL_SELF_FRAG_ALLOC_SEND(frag,rc);
    }
    if( OPAL_UNLIKELY(NULL == frag) ) {
        return NULL; 
    }
    
    frag->segment.seg_len = size;
    frag->base.des_flags   = flags;
    frag->base.des_src     = &(frag->segment);
    frag->base.des_src_cnt = 1;
    return (mca_btl_base_descriptor_t*)frag;
}
                                                                                                                   
/**
 * Return a segment allocated by this BTL.
 *
 * @param btl (IN)      BTL module
 * @param segment (IN)  Allocated segment.
 */
int mca_btl_self_free( struct mca_btl_base_module_t* btl,
                       mca_btl_base_descriptor_t* des )
{
    mca_btl_self_frag_t* frag = (mca_btl_self_frag_t*)des;

    frag->base.des_src     = NULL;
    frag->base.des_src_cnt = 0;
    frag->base.des_dst     = NULL;
    frag->base.des_dst_cnt = 0;

    if(frag->size == mca_btl_self.btl_eager_limit) {
        MCA_BTL_SELF_FRAG_RETURN_EAGER(frag);
    } else if (frag->size == mca_btl_self.btl_max_send_size) {
        MCA_BTL_SELF_FRAG_RETURN_SEND(frag);
    } else {
        MCA_BTL_SELF_FRAG_RETURN_RDMA(frag);
    }
    return OMPI_SUCCESS;
}


/**
 * Prepare data for send/put
 *
 * @param btl (IN)      BTL module
 */
struct mca_btl_base_descriptor_t*
mca_btl_self_prepare_src( struct mca_btl_base_module_t* btl,
                          struct mca_btl_base_endpoint_t* endpoint,
                          mca_mpool_base_registration_t* registration,
                          struct opal_convertor_t* convertor,
                          uint8_t order,
                          size_t reserve,
                          size_t* size,
                          uint32_t flags )
{
    mca_btl_self_frag_t* frag;
    struct iovec iov;
    uint32_t iov_count = 1;
    size_t max_data = *size;
    int rc;

    /* non-contigous data */
    if( opal_convertor_need_buffers(convertor) ||
        max_data < mca_btl_self.btl_max_send_size ||
        reserve != 0 ) {

        MCA_BTL_SELF_FRAG_ALLOC_SEND(frag, rc);
        if(OPAL_UNLIKELY(NULL == frag)) {
            return NULL;
        }

        if(reserve + max_data > frag->size) {
            max_data = frag->size - reserve;
        } 
        iov.iov_len = max_data;
        iov.iov_base = (IOVBASE_TYPE*)((unsigned char*)(frag+1) + reserve);

        rc = opal_convertor_pack(convertor, &iov, &iov_count, &max_data );
        if(rc < 0) {
            MCA_BTL_SELF_FRAG_RETURN_SEND(frag);
            return NULL;
        }
        frag->segment.seg_addr.pval = frag+1;
        frag->segment.seg_len = reserve + max_data;
        *size = max_data;
    } else {
        MCA_BTL_SELF_FRAG_ALLOC_RDMA(frag, rc);
        if(OPAL_UNLIKELY(NULL == frag)) {
            return NULL;
        }
        iov.iov_len = max_data;
        iov.iov_base = NULL;

        /* convertor should return offset into users buffer */
        rc = opal_convertor_pack(convertor, &iov, &iov_count, &max_data );
        if(rc < 0) {
            MCA_BTL_SELF_FRAG_RETURN_RDMA(frag);
            return NULL;
        }
        frag->segment.seg_addr.pval = iov.iov_base;
        frag->segment.seg_len = max_data;
        *size = max_data;
    }
    frag->base.des_flags = flags;
    frag->base.des_src          = &frag->segment;
    frag->base.des_src_cnt      = 1;
    frag->segment.seg_key.key64 = (uint64_t)(intptr_t)convertor;
    return &frag->base;
}

/**
 * Prepare data for receive.
 */
struct mca_btl_base_descriptor_t*
mca_btl_self_prepare_dst( struct mca_btl_base_module_t* btl,
                          struct mca_btl_base_endpoint_t* endpoint,
                          mca_mpool_base_registration_t* registration,
                          struct opal_convertor_t* convertor,
                          uint8_t order,
                          size_t reserve,
                          size_t* size,
                          uint32_t flags )
{
    mca_btl_self_frag_t* frag;
    size_t max_data = *size;
    int rc;

    MCA_BTL_SELF_FRAG_ALLOC_RDMA(frag, rc);
    if(OPAL_UNLIKELY(NULL == frag)) {
        return NULL;
    }

    /* setup descriptor to point directly to user buffer */
    opal_convertor_get_current_pointer( convertor, (void**)&(frag->segment.seg_addr.pval) );
    frag->segment.seg_len = reserve + max_data;
    frag->segment.seg_key.key64 = (uint64_t)(intptr_t)convertor;
    frag->base.des_dst = &frag->segment;
    frag->base.des_dst_cnt = 1;
    frag->base.des_flags = flags;
    return &frag->base;
}
 
/**
 * Initiate a send to the peer.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */

int mca_btl_self_send( struct mca_btl_base_module_t* btl,
                       struct mca_btl_base_endpoint_t* endpoint,
                       struct mca_btl_base_descriptor_t* des,
                       mca_btl_base_tag_t tag )
{
    mca_btl_active_message_callback_t* reg;
    int btl_ownership = (des->des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);

    /**
     * We have to set the dst before the call to the function and reset them
     * after.
     */
    des->des_dst     = des->des_src;
    des->des_dst_cnt = des->des_src_cnt;
    /* upcall */
    reg = mca_btl_base_active_message_trigger + tag;
    reg->cbfunc( btl, tag, des, reg->cbdata );
    des->des_dst     = NULL;
    des->des_dst_cnt = 0;
    /* send completion */
    if( des->des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK ) {
        des->des_cbfunc( btl, endpoint, des, OMPI_SUCCESS );
    }
    if( btl_ownership ) {
        mca_btl_self_free( btl, des );
    }
    return 1;
}

/**
 * Initiate a put to the peer.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
                                                                                                                           
int mca_btl_self_rdma( struct mca_btl_base_module_t* btl,
                       struct mca_btl_base_endpoint_t* endpoint,
                       struct mca_btl_base_descriptor_t* des )
{
    mca_btl_base_segment_t* src = des->des_src;
    mca_btl_base_segment_t* dst = des->des_dst;
    size_t src_cnt = des->des_src_cnt;
    size_t dst_cnt = des->des_dst_cnt;
    unsigned char* src_addr = (unsigned char*)src->seg_addr.pval;
    size_t src_len = src->seg_len;
    unsigned char* dst_addr = (unsigned char*)ompi_ptr_ltop(dst->seg_addr.lval);
    size_t dst_len = dst->seg_len;
    int btl_ownership = (des->des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);

    while(src_len && dst_len) {

        if(src_len == dst_len) {
            memcpy(dst_addr, src_addr, src_len);

            /* advance src */
            if(--src_cnt != 0) {
                src++;
                src_addr = (unsigned char*)src->seg_addr.pval;
                src_len = src->seg_len;
            } else {
                src_len = 0;
            }

            /* advance dst */
            if(--dst_cnt != 0) {
                dst++;
                dst_addr = (unsigned char*)dst->seg_addr.pval;
                dst_len = dst->seg_len;
            } else {
                dst_len = 0;
            }
                
        } else {
            size_t bytes = src_len < dst_len ? src_len : dst_len;
            memcpy(dst_addr, src_addr, bytes);

            /* advance src */
            src_len -= bytes;
            if(src_len == 0) {
                if(--src_cnt != 0) {
                    src++;
                    src_addr = (unsigned char*)src->seg_addr.pval;
                    src_len = src->seg_len;
                }
            } else {
                src_addr += bytes;
            }

            /* advance dst */
            dst_len -= bytes;
            if(dst_len == 0) {
                if(--dst_cnt != 0) {
                    dst++;
                    dst_addr = (unsigned char*)src->seg_addr.pval;
                    dst_len = src->seg_len;
                }
            } else {
                dst_addr += bytes;
            }
        }
    }

    /* rdma completion */
    des->des_cbfunc( btl, endpoint, des, OMPI_SUCCESS );
    if( btl_ownership ) {
        mca_btl_self_free( btl, des );
    }
    return OMPI_SUCCESS;
}

int mca_btl_self_ft_event(int state) {
    if(OPAL_CRS_CHECKPOINT == state) {
        ;
    }
    else if(OPAL_CRS_CONTINUE == state) {
        ;
    }
    else if(OPAL_CRS_RESTART == state) {
        ;
    }
    else if(OPAL_CRS_TERM == state ) {
        ;
    }
    else {
        ;
    }

    return OMPI_SUCCESS;
}
