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
 * Copyright (c) 2008      UT-Battelle, LLC. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <inttypes.h>

#include "opal/class/opal_bitmap.h"
#include "ompi/constants.h"
#include "ompi/mca/btl/btl.h"
#include "opal/datatype/opal_convertor.h"

#include "btl_portals.h"
#include "btl_portals_endpoint.h"
#include "btl_portals_recv.h"
#include "btl_portals_frag.h"

mca_btl_portals_module_t mca_btl_portals_module = {
    {
        &mca_btl_portals_component.super,

        /* NOTE: All these default values are set in
           component_open() */

        0,   /* max size of first frag */
        0,   /* min send size */
        0,   /* max send size */
        0,   /* btl_rdma_pipeline_send_length */
        0,   /* btl_rdma_pipeline_frag_size */
        0,   /* btl_min_rdma_pipeline_size */
        0,   /* exclusivity - higher than sm, lower than self */
        0,   /* latency */
        0,   /* bandwidth */
        0,   /* btl flags */

        mca_btl_portals_add_procs,
        mca_btl_portals_del_procs,
        NULL,
        mca_btl_portals_finalize,

        mca_btl_portals_alloc,
        mca_btl_portals_free,
        mca_btl_portals_prepare_src,
        mca_btl_portals_prepare_dst,
        mca_btl_portals_send,
        NULL, /* mca_btl_portals_sendi, */
        mca_btl_portals_put,
        mca_btl_portals_get,
        mca_btl_base_dump,
        NULL, /* mpool */
        NULL, /* register error */
        NULL
    },
};



int
mca_btl_portals_add_procs(struct mca_btl_base_module_t* btl_base,
                          size_t nprocs, struct ompi_proc_t **procs,
                          struct mca_btl_base_endpoint_t** peers,
                          opal_bitmap_t* reachable)
{
    int ret;
    struct ompi_proc_t *curr_proc = NULL;
    ptl_process_id_t *portals_procs = NULL;
    size_t i;
    unsigned long distance;
    bool need_activate = false;
    bool accel;

    assert(&mca_btl_portals_module == (mca_btl_portals_module_t*) btl_base);
    opal_output_verbose(50, mca_btl_portals_component.portals_output,
                        "Adding %d procs (%d)", (int) nprocs,
                        (int) mca_btl_portals_module.portals_num_procs);

    /* if we havne't already, get our network handle */
    if (mca_btl_portals_module.portals_ni_h == PTL_INVALID_HANDLE) {
        ret = ompi_common_portals_ni_initialize(&mca_btl_portals_module.portals_ni_h, &accel);
        if (OMPI_SUCCESS != ret) return ret;
    }

    portals_procs = malloc(nprocs * sizeof(ptl_process_id_t));
    ret = ompi_common_portals_get_procs(nprocs, procs, portals_procs);
    if (OMPI_SUCCESS != ret) return ret;

    if (0 == mca_btl_portals_module.portals_num_procs) {
        need_activate = true;
    }

    /* loop through all procs, setting our reachable flag */
    for (i= 0; i < nprocs ; ++i) {
        curr_proc = procs[i];

        /* portals doesn't support heterogeneous yet... */
        if (ompi_proc_local()->proc_arch != curr_proc->proc_arch) {
            continue;
        }

        peers[i] = malloc(sizeof(mca_btl_base_endpoint_t));
        if (NULL == peers[i]) return OMPI_ERROR;
        *((mca_btl_base_endpoint_t*) peers[i]) = portals_procs[i];

        /* accelerated doesn't support PtlNIDist() */
        if (accel == false) {
            /* make sure we can reach the process - this is supposed to be
               a cheap-ish operation */
            ret = PtlNIDist(mca_btl_portals_module.portals_ni_h,
                            portals_procs[i],
                            &distance);
            if (ret != PTL_OK) {
                opal_output_verbose(10, mca_btl_portals_component.portals_output,
                                    "Could not find distance to process %d", (int) i);
                continue;
            }
        }

        OPAL_THREAD_ADD32(&mca_btl_portals_module.portals_num_procs, 1);
        /* and here we can reach */
        opal_bitmap_set_bit(reachable, i);
    }

    if (NULL != portals_procs) free(portals_procs);

    if (need_activate && mca_btl_portals_module.portals_num_procs > 0) {
        /* create eqs */
        int i;

        opal_output_verbose(50, mca_btl_portals_component.portals_output,
                            "Enabling progress");

        for (i = 0 ; i < OMPI_BTL_PORTALS_EQ_SIZE ; ++i) {
            int ptl_ret = PtlEQAlloc(mca_btl_portals_module.portals_ni_h,
                                     mca_btl_portals_module.portals_eq_sizes[i],
                                     PTL_EQ_HANDLER_NONE,
                                     &(mca_btl_portals_module.portals_eq_handles[i]));
            if (PTL_OK != ptl_ret) {
                opal_output(mca_btl_portals_component.portals_output,
                            "Error creating EQ %d: %d", i, ptl_ret);
                /* BWB - better error code? */
                return OMPI_ERROR;
            }
        }

        ret = mca_btl_portals_recv_enable(&mca_btl_portals_module);

        /* fill in send memory descriptor */
        mca_btl_portals_module.md_send.start = NULL;
        mca_btl_portals_module.md_send.length = 0;
        mca_btl_portals_module.md_send.threshold = PTL_MD_THRESH_INF;
        mca_btl_portals_module.md_send.max_size = 0;
        mca_btl_portals_module.md_send.options = PTL_MD_EVENT_START_DISABLE;
        mca_btl_portals_module.md_send.user_ptr = NULL;
        mca_btl_portals_module.md_send.eq_handle = 
            mca_btl_portals_module.portals_eq_handles[OMPI_BTL_PORTALS_EQ_SEND];
    } else {
        ret = OMPI_SUCCESS;
    }

    return ret;
}


int
mca_btl_portals_del_procs(struct mca_btl_base_module_t *btl_base,
			  size_t nprocs,
			  struct ompi_proc_t **procs,
			  struct mca_btl_base_endpoint_t **peers)
{
    size_t i = 0;
    int ret = OMPI_SUCCESS;

    assert(&mca_btl_portals_module == (mca_btl_portals_module_t*) btl_base);
    opal_output_verbose(50, mca_btl_portals_component.portals_output,
                        "Removing %d procs (%d)", (int) nprocs,
                        (int) mca_btl_portals_module.portals_num_procs);

    for (i = 0 ; i < nprocs ; ++i) {
        free(peers[i]);
        OPAL_THREAD_ADD32(&mca_btl_portals_module.portals_num_procs, -1);
    }

    if (0 == mca_btl_portals_module.portals_num_procs) {
        int i;

        opal_output_verbose(50, mca_btl_portals_component.portals_output,
                            "Disabling progress");

        ret = mca_btl_portals_recv_disable(&mca_btl_portals_module);

        /* destroy eqs */
        for (i = 0 ; i < OMPI_BTL_PORTALS_EQ_SIZE ; ++i) {
            int ptl_ret = PtlEQFree(mca_btl_portals_module.portals_eq_handles[i]);
            if (PTL_OK != ptl_ret) {
                opal_output(mca_btl_portals_component.portals_output,
                            "Error freeing EQ %d: %d", i, ptl_ret);
            }
        }

    } else {
        ret = OMPI_SUCCESS;
    }

    return ret;
}


mca_btl_base_descriptor_t*
mca_btl_portals_alloc(struct mca_btl_base_module_t* btl_base,
                      struct mca_btl_base_endpoint_t* endpoint,
                      uint8_t order,
                      size_t size,
                      uint32_t flags)
{
    int rc;
    mca_btl_portals_frag_t* frag;

    assert(&mca_btl_portals_module == (mca_btl_portals_module_t*) btl_base);
    
    if (size <= mca_btl_portals_module.super.btl_eager_limit) { 
        OMPI_BTL_PORTALS_FRAG_ALLOC_EAGER(&mca_btl_portals_module, frag, rc); 
        if (OMPI_SUCCESS != rc) return NULL;
        frag->segments[0].seg_len = size;
    } else { 
        OMPI_BTL_PORTALS_FRAG_ALLOC_MAX(&mca_btl_portals_module, frag, rc); 
        if (OMPI_SUCCESS != rc) return NULL;
        frag->segments[0].seg_len = 
            size <= mca_btl_portals_module.super.btl_max_send_size ? 
            size : mca_btl_portals_module.super.btl_max_send_size ; 
    }
    
    frag->base.des_src_cnt = 1;
    frag->base.des_flags = flags | MCA_BTL_DES_SEND_ALWAYS_CALLBACK; 
    frag->base.order = MCA_BTL_NO_ORDER;

    return &frag->base;
}


int
mca_btl_portals_free(struct mca_btl_base_module_t* btl_base, 
                      mca_btl_base_descriptor_t* des) 
{
    mca_btl_portals_frag_t* frag = (mca_btl_portals_frag_t*) des; 

    assert(&mca_btl_portals_module == (mca_btl_portals_module_t*) btl_base);

    if (BTL_PORTALS_FRAG_TYPE_EAGER == frag->type) {
        /* don't ever unlink eager frags */
        OMPI_BTL_PORTALS_FRAG_RETURN_EAGER(&mca_btl_portals_module.super, frag); 

    } else if (BTL_PORTALS_FRAG_TYPE_MAX == frag->type) {
        if (frag->md_h != PTL_INVALID_HANDLE) {
            PtlMDUnlink(frag->md_h);
            frag->md_h = PTL_INVALID_HANDLE;
        }
        OMPI_BTL_PORTALS_FRAG_RETURN_MAX(&mca_btl_portals_module.super, frag); 

    } else if (BTL_PORTALS_FRAG_TYPE_USER == frag->type) {
        if (frag->md_h != PTL_INVALID_HANDLE) {
            PtlMDUnlink(frag->md_h);
            frag->md_h = PTL_INVALID_HANDLE;
        }
        OPAL_THREAD_ADD32(&mca_btl_portals_module.portals_outstanding_ops, -1);
        OMPI_BTL_PORTALS_FRAG_RETURN_USER(&mca_btl_portals_module.super, frag); 
    } else {
        return OMPI_ERR_BAD_PARAM;
    }

    return OMPI_SUCCESS; 
}


mca_btl_base_descriptor_t* 
mca_btl_portals_prepare_src(struct mca_btl_base_module_t* btl_base,
                            struct mca_btl_base_endpoint_t* peer,
                            mca_mpool_base_registration_t* registration, 
                            struct opal_convertor_t* convertor,
                            uint8_t order,
                            size_t reserve,
                            size_t* size,
                            uint32_t flags)
{
    mca_btl_portals_frag_t* frag;
    size_t max_data = *size;
    struct iovec iov;
    uint32_t iov_count = 1;
    int ret;

    assert(&mca_btl_portals_module == (mca_btl_portals_module_t*) btl_base);

    if (0 != reserve || 0 != opal_convertor_need_buffers(convertor)) {
        frag = (mca_btl_portals_frag_t*) 
            mca_btl_portals_alloc(btl_base, peer, MCA_BTL_NO_ORDER, max_data + reserve, flags);
        if (NULL == frag)  {
            return NULL;
        }

        if (max_data + reserve > frag->size) {
            max_data = frag->size - reserve;
        }
        
        iov.iov_len = max_data;
        iov.iov_base = (unsigned char*) frag->segments[0].seg_addr.pval + reserve;
        ret = opal_convertor_pack(convertor, &iov, &iov_count, 
                                  &max_data );
        *size  = max_data;
        if ( ret < 0 ) {
            return NULL;
        }

        frag->segments[0].seg_len = max_data + reserve;
        frag->base.des_src_cnt = 1;

    } else {
        /* no need to pack - rdma operation out of user's buffer */
        ptl_md_t md;
        ptl_handle_me_t me_h;

        /* reserve space in the event queue for rdma operations immediately */
        while (OPAL_THREAD_ADD32(&mca_btl_portals_module.portals_outstanding_ops, 1) >
               mca_btl_portals_module.portals_max_outstanding_ops) {
            OPAL_THREAD_ADD32(&mca_btl_portals_module.portals_outstanding_ops, -1);
            mca_btl_portals_component_progress();
        }

        OMPI_BTL_PORTALS_FRAG_ALLOC_USER(&mca_btl_portals_module.super, frag, ret);
        if(NULL == frag){
            OPAL_THREAD_ADD32(&mca_btl_portals_module.portals_outstanding_ops, -1);
            return NULL;
        }
        iov.iov_len = max_data;
        iov.iov_base = NULL;

        opal_convertor_pack(convertor, &iov, &iov_count, &max_data );

        frag->segments[0].seg_len = max_data;
        frag->segments[0].seg_addr.pval = iov.iov_base;
        frag->segments[0].seg_key.key64 = 
            OPAL_THREAD_ADD64(&(mca_btl_portals_module.portals_rdma_key), 1);
        frag->base.des_src_cnt = 1;

        /* either a put or get.  figure out which later */
        OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                             "rdma src posted for frag 0x%lx, callback 0x%lx, bits %"PRIu64", flags say %d" ,
                             (unsigned long) frag, 
                             (unsigned long) frag->base.des_cbfunc,
                             frag->segments[0].seg_key.key64, flags));

        /* create a match entry */
        ret = PtlMEAttach(mca_btl_portals_module.portals_ni_h,
                          OMPI_BTL_PORTALS_RDMA_TABLE_ID,
                          *((mca_btl_base_endpoint_t*) peer),
                          frag->segments[0].seg_key.key64, /* match */
                          0, /* ignore */
                          PTL_UNLINK,
                          PTL_INS_AFTER,
                          &me_h);
        if (PTL_OK != ret) {
            opal_output(mca_btl_portals_component.portals_output,
                        "Error creating rdma src ME: %d", ret);
            OMPI_BTL_PORTALS_FRAG_RETURN_USER(&mca_btl_portals_module.super, frag);
            OPAL_THREAD_ADD32(&mca_btl_portals_module.portals_outstanding_ops, -1);
            return NULL;
        }

        /* setup the memory descriptor */
        md.start = frag->segments[0].seg_addr.pval;
        md.length = frag->segments[0].seg_len;
        md.threshold = PTL_MD_THRESH_INF;
        md.max_size = 0;
        md.options = PTL_MD_OP_PUT | PTL_MD_OP_GET | PTL_MD_EVENT_START_DISABLE;
        md.user_ptr = frag; /* keep a pointer to ourselves */
        md.eq_handle = mca_btl_portals_module.portals_eq_handles[OMPI_BTL_PORTALS_EQ_SEND];

        ret = PtlMDAttach(me_h, 
                          md,
                          PTL_UNLINK,
                          &(frag->md_h));
        if (PTL_OK != ret) {
            opal_output(mca_btl_portals_component.portals_output,
                        "Error creating rdma src MD: %d", ret);
            PtlMEUnlink(me_h);
            OMPI_BTL_PORTALS_FRAG_RETURN_USER(&mca_btl_portals_module.super, frag);
            OPAL_THREAD_ADD32(&mca_btl_portals_module.portals_outstanding_ops, -1);
            return NULL;
        }
    }

    frag->base.des_src = frag->segments;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    frag->base.des_flags = flags | MCA_BTL_DES_SEND_ALWAYS_CALLBACK; 
    frag->base.order = MCA_BTL_NO_ORDER;
    return &frag->base;
}


mca_btl_base_descriptor_t* 
mca_btl_portals_prepare_dst(struct mca_btl_base_module_t* btl_base, 
                            struct mca_btl_base_endpoint_t* peer,
                            mca_mpool_base_registration_t* registration, 
                            struct opal_convertor_t* convertor,
                            uint8_t order,
                            size_t reserve,
                            size_t* size,
                            uint32_t flags)
{
    mca_btl_portals_frag_t* frag;
    ptl_md_t md;
    ptl_handle_me_t me_h;
    int ret;

    assert(&mca_btl_portals_module == (mca_btl_portals_module_t*) btl_base);

    /* reserve space in the event queue for rdma operations immediately */
    while (OPAL_THREAD_ADD32(&mca_btl_portals_module.portals_outstanding_ops, 1) >
           mca_btl_portals_module.portals_max_outstanding_ops) {
        OPAL_THREAD_ADD32(&mca_btl_portals_module.portals_outstanding_ops, -1);
        mca_btl_portals_component_progress();
    }

    OMPI_BTL_PORTALS_FRAG_ALLOC_USER(&mca_btl_portals_module.super, frag, ret);
    if(NULL == frag) {
        OPAL_THREAD_ADD32(&mca_btl_portals_module.portals_outstanding_ops, -1);
        return NULL;
    }

    frag->segments[0].seg_len = *size;
    opal_convertor_get_current_pointer( convertor, (void**)&(frag->segments[0].seg_addr.pval) );
    frag->segments[0].seg_key.key64 = 
        OPAL_THREAD_ADD64(&(mca_btl_portals_module.portals_rdma_key), 1);
    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
    frag->base.des_dst = frag->segments;
    frag->base.des_dst_cnt = 1;
    frag->base.des_flags = flags | MCA_BTL_DES_SEND_ALWAYS_CALLBACK; 
    
    OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                         "rdma dest posted for frag 0x%lx, callback 0x%lx, bits %" PRIu64 " flags %d",
                         (unsigned long) frag,
                         (unsigned long) frag->base.des_cbfunc,
                         frag->segments[0].seg_key.key64,
                         flags));

    /* create a match entry */
    ret = PtlMEAttach(mca_btl_portals_module.portals_ni_h,
                      OMPI_BTL_PORTALS_RDMA_TABLE_ID,
                      *((mca_btl_base_endpoint_t*) peer),
                      frag->segments[0].seg_key.key64, /* match */
                      0, /* ignore */
                      PTL_UNLINK,
                      PTL_INS_AFTER,
                      &me_h);
    if (PTL_OK != ret) {
        opal_output(mca_btl_portals_component.portals_output,
                    "Error creating rdma dest ME: %d", ret);
        OPAL_THREAD_ADD32(&mca_btl_portals_module.portals_outstanding_ops, -1);
        OMPI_BTL_PORTALS_FRAG_RETURN_USER(&mca_btl_portals_module.super, frag);
        return NULL;
    }

    /* setup the memory descriptor. */
    md.start = frag->segments[0].seg_addr.pval;
    md.length = frag->segments[0].seg_len;
    md.threshold = PTL_MD_THRESH_INF;
    md.max_size = 0;
    md.options = PTL_MD_OP_PUT | PTL_MD_OP_GET | PTL_MD_EVENT_START_DISABLE;
    md.user_ptr = frag; /* keep a pointer to ourselves */
    md.eq_handle = mca_btl_portals_module.portals_eq_handles[OMPI_BTL_PORTALS_EQ_SEND];

    ret = PtlMDAttach(me_h, 
                      md,
                      PTL_UNLINK,
                      &(frag->md_h));
    if (PTL_OK != ret) {
        opal_output(mca_btl_portals_component.portals_output,
                    "Error creating rdma dest MD: %d", ret);
        PtlMEUnlink(me_h);
        OPAL_THREAD_ADD32(&mca_btl_portals_module.portals_outstanding_ops, -1);
        OMPI_BTL_PORTALS_FRAG_RETURN_USER(&mca_btl_portals_module.super, frag);
        return NULL;
    }
    frag->base.order = MCA_BTL_NO_ORDER;
    return &frag->base;
}


int
mca_btl_portals_finalize(struct mca_btl_base_module_t *btl_base)
{
    int ret;

    assert(&mca_btl_portals_module == (mca_btl_portals_module_t*) btl_base);
    OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                            "in mca_btl_portals_finalize"));

    /* sanity check */
    assert(mca_btl_portals_module.portals_outstanding_ops  >= 0);
    
    /* finalize all communication */
    while (mca_btl_portals_module.portals_outstanding_ops > 0) {
        OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                            "portals_outstanding_ops: %d", 
                            mca_btl_portals_module.portals_outstanding_ops));
        
        mca_btl_portals_component_progress();
    }
    
    
    if (mca_btl_portals_module.portals_num_procs != 0) {
        int i;

        ret = mca_btl_portals_recv_disable(&mca_btl_portals_module);

        /* destroy eqs */
        for (i = 0 ; i < OMPI_BTL_PORTALS_EQ_SIZE ; ++i) {
            int ptl_ret = PtlEQFree(mca_btl_portals_module.portals_eq_handles[i]);
            if (PTL_OK != ptl_ret) {
#if (OMPI_PORTALS_CRAYXT3 || OMPI_PORTALS_CRAYXT3_MODEX)
                if (i != OMPI_BTL_PORTALS_EQ_SEND && PTL_EQ_IN_USE != ptl_ret) {
                    /* The PML isn't great about cleaning up after itself.
                       Ignore related errors. */
#endif
                    opal_output(mca_btl_portals_component.portals_output,
                            "Error freeing EQ %d: %d", i, ptl_ret);
#if (OMPI_PORTALS_CRAYXT3 || OMPI_PORTALS_CRAYXT3_MODEX)
                }
#endif
            }
        }

    } 

    OBJ_DESTRUCT(&mca_btl_portals_module.portals_recv_blocks);
    OBJ_DESTRUCT(&mca_btl_portals_module.portals_recv_frag);
    OBJ_DESTRUCT(&mca_btl_portals_module.portals_frag_eager);
    OBJ_DESTRUCT(&mca_btl_portals_module.portals_frag_max);
    OBJ_DESTRUCT(&mca_btl_portals_module.portals_frag_user);
    
    ompi_common_portals_ni_finalize();
    ompi_common_portals_finalize();

    opal_output_verbose(20, mca_btl_portals_component.portals_output,
                        "successfully finalized module");

    return OMPI_SUCCESS;
}
