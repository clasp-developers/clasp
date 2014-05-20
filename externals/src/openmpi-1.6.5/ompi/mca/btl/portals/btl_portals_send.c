/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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

#include <assert.h>

#include "ompi/constants.h"
#include "opal/datatype/opal_convertor.h"

#include "btl_portals.h"
#include "btl_portals_send.h"


int
mca_btl_portals_send(struct mca_btl_base_module_t* btl_base,
                     struct mca_btl_base_endpoint_t* endpoint,
                     struct mca_btl_base_descriptor_t* descriptor, 
                     mca_btl_base_tag_t tag)
{
    mca_btl_portals_frag_t *frag = (mca_btl_portals_frag_t*) descriptor;
    int ret;
    unsigned char hdr_data[8];
    
    assert(&mca_btl_portals_module == (mca_btl_portals_module_t*) btl_base);

    frag->endpoint = endpoint;
    hdr_data[7] = frag->hdr.tag = tag;

    OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                         "PtlPut (send) fragment %lx",
                         (unsigned long) frag));

    if (OPAL_THREAD_ADD32(&mca_btl_portals_module.portals_outstanding_ops, 1) >
        mca_btl_portals_module.portals_max_outstanding_ops) {
        /* no space - queue and continute */
        opal_output_verbose(50, mca_btl_portals_component.portals_output,
                            "no space for message 0x%lx.  Adding to back of queue",
                            (unsigned long) frag);
        OPAL_THREAD_ADD32(&mca_btl_portals_module.portals_outstanding_ops, -1);
        opal_list_append(&(mca_btl_portals_module.portals_queued_sends),
                         (opal_list_item_t*) frag);
    }

    if (frag->md_h == PTL_INVALID_HANDLE) {
        /* setup the send - always describe entire fragment */
        mca_btl_portals_module.md_send.start = frag->segments[0].seg_addr.pval;
        mca_btl_portals_module.md_send.length = 
            0 == frag->size ? frag->segments[0].seg_len : frag->size;
#if OPAL_ENABLE_DEBUG 
        mca_btl_portals_module.md_send.options = 
            PTL_MD_EVENT_START_DISABLE;
#else 
        /* optimized build, we can get rid of the END event */
        /*  if we are using portals ACK's for completion */
        mca_btl_portals_module.md_send.options = 
            (PTL_MD_EVENT_START_DISABLE | 
             (mca_btl_portals_component.portals_need_ack ? PTL_MD_EVENT_END_DISABLE : 0));
#endif
        mca_btl_portals_module.md_send.user_ptr = frag; /* keep a pointer to ourselves */

        /* make a free-floater */
        ret = PtlMDBind(mca_btl_portals_module.portals_ni_h,
                        mca_btl_portals_module.md_send,
                        PTL_UNLINK,
                        &frag->md_h);
        if (ret != PTL_OK) {
            opal_output(mca_btl_portals_component.portals_output,
                        "PtlMDBind failed with error %d", ret);
            return OMPI_ERROR;
        }
    } 

    OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                         "fragment info:\n"
                         "\tstart: 0x%lx\n"
                         "\tlen: %d",
                         (unsigned long) frag->segments[0].seg_addr.pval,
                         frag->segments[0].seg_len)); 
    
    ret = PtlPutRegion(frag->md_h,                /* memory descriptor */
                       0,                         /* fragment offset */
                       frag->segments[0].seg_len, /* fragment length */
                       (mca_btl_portals_component.portals_need_ack ? PTL_ACK_REQ : PTL_NO_ACK_REQ),
                       *((mca_btl_base_endpoint_t*) endpoint),
                       OMPI_BTL_PORTALS_SEND_TABLE_ID,
                       0,                         /* ac_index - not used */
                       0,                         /* match bits */
                       0,                         /* remote offset - not used */
                       *((ptl_hdr_data_t*) hdr_data));            /* hdr_data: tag */
    if (ret != PTL_OK) {
        opal_output(mca_btl_portals_component.portals_output,
                    "send: PtlPut failed with error %d", ret);
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}



int mca_btl_portals_sendi(struct mca_btl_base_module_t* btl_base,
                          struct mca_btl_base_endpoint_t* endpoint,
                          struct opal_convertor_t* convertor,
                          void* header,
                          size_t header_size,
                          size_t payload_size,
                          uint8_t order,
                          uint32_t flags,
                          mca_btl_base_tag_t tag, 
                          mca_btl_base_descriptor_t** des)
{
    mca_btl_portals_frag_t *frag = NULL;
    struct iovec iov;
    unsigned int iov_count;
    unsigned char match_bits[8];
    unsigned char hdr_data[8];
    int ret;
    size_t max_data;
    
    opal_output(0, "mca_btl_portals_sendi status is incomplete");
    abort();
    assert(&mca_btl_portals_module == (mca_btl_portals_module_t*) btl_base);
    *des = NULL; 
    if (OPAL_THREAD_ADD32(&mca_btl_portals_module.portals_outstanding_ops, 1) >
        mca_btl_portals_module.portals_max_outstanding_ops) {
        /* no space - queue and continue */
        opal_output_verbose(50, mca_btl_portals_component.portals_output,
                            "no resources left for send inline");
        
        OPAL_THREAD_ADD32(&mca_btl_portals_module.portals_outstanding_ops, -1);
        *des = mca_btl_portals_alloc(btl_base, endpoint, order, 
                                     payload_size + header_size, flags);
        return OMPI_ERR_RESOURCE_BUSY;
        
    } else if(14 < header_size) { 
        /* header is too big */
        *des = mca_btl_portals_alloc(btl_base, endpoint, order, 
                                     payload_size + header_size, flags);
        OPAL_THREAD_ADD32(&mca_btl_portals_module.portals_outstanding_ops, -1);
        return OMPI_ERR_RESOURCE_BUSY;
    }
    
    assert (payload_size <= mca_btl_portals_module.super.btl_eager_limit);
    
    OMPI_BTL_PORTALS_FRAG_ALLOC_EAGER(&mca_btl_portals_module, frag, ret); 
    if (OMPI_SUCCESS != ret) { 
        OPAL_THREAD_ADD32(&mca_btl_portals_module.portals_outstanding_ops, -1);
        return OMPI_ERR_RESOURCE_BUSY;
    }
    frag->segments[0].seg_len = payload_size;
    frag->base.des_src_cnt = 1;
    frag->base.des_flags = flags; 
    frag->base.order = MCA_BTL_NO_ORDER;
    frag->endpoint = endpoint;

    if(payload_size) { 
        /* pack the data into the supplied buffer */
        iov.iov_base = (IOVBASE_TYPE*)((unsigned char*)frag->segments[0].seg_addr.pval);
        iov.iov_len  = max_data = payload_size;
        iov_count    = 1;
        
        (void)opal_convertor_pack( convertor,
                                   &iov, &iov_count, &max_data);
        
        assert(max_data == payload_size);
    }
    
    if(header_size) { 
        memcpy(match_bits, header, header_size > 8 ? 8 : header_size);
    }
    if(header_size > 8 ) { 
        memcpy(hdr_data, ((unsigned char*) header) + 8, header_size - 8);
    }
    hdr_data[6] = header_size;
    hdr_data[7] = frag->hdr.tag = tag;
    
    
    OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                         "PtlPut (send) fragment %lx",
                         (unsigned long) frag));
    


    if (frag->md_h == PTL_INVALID_HANDLE) {
        /* setup the send - always describe entire fragment */
        mca_btl_portals_module.md_send.start = frag->segments[0].seg_addr.pval;
        mca_btl_portals_module.md_send.length = 
            0 == frag->size ? frag->segments[0].seg_len : frag->size;
#if OPAL_ENABLE_DEBUG 
        mca_btl_portals_module.md_send.options = 
            PTL_MD_EVENT_START_DISABLE;
#else 
        /* optimized build, we can get rid of the END event */
        /*  if we are using portals ACK's for completion */
        mca_btl_portals_module.md_send.options = 
            (PTL_MD_EVENT_START_DISABLE | 
             (mca_btl_portals_component.portals_need_ack ? PTL_MD_EVENT_END_DISABLE : 0));
#endif
        mca_btl_portals_module.md_send.user_ptr = frag; /* keep a pointer to ourselves */

        /* make a free-floater */
        ret = PtlMDBind(mca_btl_portals_module.portals_ni_h,
                        mca_btl_portals_module.md_send,
                        PTL_UNLINK,
                        &frag->md_h);
        if (ret != PTL_OK) {
            opal_output(mca_btl_portals_component.portals_output,
                        "PtlMDBind failed with error %d", ret);
            return OMPI_ERROR;
        }
    } 

    OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                         "fragment info:\n"
                         "\tstart: 0x%lx\n"
                         "\tlen: %d",
                         (unsigned long) frag->segments[0].seg_addr.pval,
                         frag->segments[0].seg_len)); 
    
    ret = PtlPutRegion(frag->md_h,                /* memory descriptor */
                       0,                         /* fragment offset */
                       frag->segments[0].seg_len, /* fragment length */
                       (mca_btl_portals_component.portals_need_ack ? PTL_ACK_REQ : PTL_NO_ACK_REQ),
                       *((mca_btl_base_endpoint_t*) endpoint),
                       OMPI_BTL_PORTALS_SEND_TABLE_ID,
                       0,                         /* ac_index - not used */
                       *((ptl_match_bits_t*) match_bits),                /* match bits */
                       0,                         /* remote offset - not used */
                       *((ptl_hdr_data_t*) hdr_data));            /* hdr_data: tag */
    
    if (ret != PTL_OK) {
        opal_output(mca_btl_portals_component.portals_output,
                    "send: PtlPut failed with error %d", ret);
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}
