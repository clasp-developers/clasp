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
 * Copyright (c) 2006-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2008-2011 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2009      IBM Corporation.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/dss/dss.h"
#include "opal_stdint.h"
#include "orte/util/show_help.h"
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "ompi/mca/dpm/dpm.h"

#include "btl_openib.h"
#include "btl_openib_endpoint.h" 
#include "btl_openib_proc.h"
#include "connect/connect.h"
#include "orte/util/show_help.h"

#if (ENABLE_DYNAMIC_SL)
#include "connect/btl_openib_connect_sl.h"
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
typedef enum {
    ENDPOINT_CONNECT_REQUEST,
    ENDPOINT_CONNECT_RESPONSE,
    ENDPOINT_CONNECT_ACK
} connect_message_type_t;

static int oob_priority = 50;
static bool rml_recv_posted = false;

static void oob_component_register(void);
static int oob_component_query(mca_btl_openib_module_t *openib_btl, 
                               ompi_btl_openib_connect_base_module_t **cpc);
static int oob_component_finalize(void);

static int oob_module_start_connect(ompi_btl_openib_connect_base_module_t *cpc,
                                    mca_btl_base_endpoint_t *endpoint);
static int reply_start_connect(mca_btl_openib_endpoint_t *endpoint,
                               mca_btl_openib_rem_info_t *rem_info);
static int set_remote_info(mca_btl_base_endpoint_t* endpoint,
                           mca_btl_openib_rem_info_t* rem_info);
static int qp_connect_all(mca_btl_base_endpoint_t* endpoint);
static int qp_create_all(mca_btl_base_endpoint_t* endpoint);
static int qp_create_one(mca_btl_base_endpoint_t* endpoint, int qp,
        struct ibv_srq *srq, uint32_t max_recv_wr, uint32_t max_send_wr);
static int send_connect_data(mca_btl_base_endpoint_t* endpoint, 
                             uint8_t message_type);

static void rml_send_cb(int status, orte_process_name_t* endpoint, 
                        opal_buffer_t* buffer, orte_rml_tag_t tag, 
                        void* cbdata);
static void rml_recv_cb(int status, orte_process_name_t* process_name, 
                        opal_buffer_t* buffer, orte_rml_tag_t tag, 
                        void* cbdata);

/*
 * The "component" struct -- the top-level function pointers for the
 * oob connection scheme.
 */
ompi_btl_openib_connect_base_component_t ompi_btl_openib_connect_oob = {
    "oob",
    /* Register */
    oob_component_register,
    /* Init */
    NULL,
    /* Query */
    oob_component_query,
    /* Finalize */
    oob_component_finalize,
};

/* Open - this functions sets up any oob specific commandline params */
static void oob_component_register(void)
{
    mca_base_param_reg_int(&mca_btl_openib_component.super.btl_version,
                           "connect_oob_priority",
                           "The selection method priority for oob",
                           false, false, oob_priority, &oob_priority);

    if (oob_priority > 100) {
        oob_priority = 100;
    } else if (oob_priority < -1) {
        oob_priority = -1;
    }
}

/*
 * Init function.  Post non-blocking RML receive to accept incoming
 * connection requests.
 */
static int oob_component_query(mca_btl_openib_module_t *btl, 
                               ompi_btl_openib_connect_base_module_t **cpc)
{
    int rc;

    /* If we have the transport_type member, check to ensure we're on
       IB (this CPC will not work with iWarp).  If we do not have the
       transport_type member, then we must be < OFED v1.2, and
       therefore we must be IB. */   
#if defined(HAVE_STRUCT_IBV_DEVICE_TRANSPORT_TYPE)
    if (BTL_OPENIB_CONNECT_BASE_CHECK_IF_NOT_IB(btl)) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: oob CPC only supported on InfiniBand; skipped on  %s:%d",
                            ibv_get_device_name(btl->device->ib_dev),
                            btl->port_num);
        return OMPI_ERR_NOT_SUPPORTED;
    }
#endif

    if (mca_btl_openib_component.num_xrc_qps > 0) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: oob CPC not supported with XRC receive queues, please try xoob CPC; skipped on %s:%d",
                            ibv_get_device_name(btl->device->ib_dev),
                            btl->port_num);
        return OMPI_ERR_NOT_SUPPORTED;
    }
    /* If this btl supports OOB, then post the RML message.  But
       ensure to only post it *once*, because another btl may have
       come in before this and already posted it. */
    if (!rml_recv_posted) {
        rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, 
                                     OMPI_RML_TAG_OPENIB,
                                     ORTE_RML_PERSISTENT,
                                     rml_recv_cb,
                                     NULL);
        if (ORTE_SUCCESS != rc) {
            opal_output_verbose(5, mca_btl_base_output,
                                "openib BTL: oob CPC system error %d (%s)",
                                rc, opal_strerror(rc));
            return rc;
        }
        rml_recv_posted = true;
    }

    *cpc = malloc(sizeof(ompi_btl_openib_connect_base_module_t));
    if (NULL == *cpc) {
        orte_rml.recv_cancel(ORTE_NAME_WILDCARD, OMPI_RML_TAG_OPENIB);
        rml_recv_posted = false;
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: oob CPC system error (malloc failed)");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    (*cpc)->data.cbm_component = &ompi_btl_openib_connect_oob;
    (*cpc)->data.cbm_priority = oob_priority;
    (*cpc)->data.cbm_modex_message = NULL;
    (*cpc)->data.cbm_modex_message_len = 0;

    (*cpc)->cbm_endpoint_init = NULL;
    (*cpc)->cbm_start_connect = oob_module_start_connect;
    (*cpc)->cbm_endpoint_finalize = NULL;
    (*cpc)->cbm_finalize = NULL;
    (*cpc)->cbm_uses_cts = false;

    opal_output_verbose(5, mca_btl_base_output,
                        "openib BTL: oob CPC available for use on %s:%d",
                        ibv_get_device_name(btl->device->ib_dev),
                        btl->port_num);
    return OMPI_SUCCESS;
}

/*
 * Connect function.  Start initiation of connections to a remote
 * peer.  We send our Queue Pair information over the RML/OOB
 * communication mechanism.  On completion of our send, a send
 * completion handler is called.
 */
static int oob_module_start_connect(ompi_btl_openib_connect_base_module_t *cpc,
                                    mca_btl_base_endpoint_t *endpoint)
{
    int rc;

    if (OMPI_SUCCESS != (rc = qp_create_all(endpoint))) {
        return rc;
    }

    /* Send connection info over to remote endpoint */
    endpoint->endpoint_state = MCA_BTL_IB_CONNECTING;
    if (OMPI_SUCCESS !=
        (rc = send_connect_data(endpoint, ENDPOINT_CONNECT_REQUEST))) {
        BTL_ERROR(("error sending connect request, error code %d", rc)); 
        return rc;
    }

    return OMPI_SUCCESS;
}

/*
 * Component finalize function.  Cleanup RML non-blocking receive.
 */
static int oob_component_finalize(void)
{
    if (rml_recv_posted) {
        orte_rml.recv_cancel(ORTE_NAME_WILDCARD, OMPI_RML_TAG_OPENIB);
        rml_recv_posted = false;
    }
#if (ENABLE_DYNAMIC_SL)
    btl_openib_connect_sl_finalize();
#endif
    return OMPI_SUCCESS;
}

/**************************************************************************/

/*
 * Reply to a `start - connect' message
 */
static int reply_start_connect(mca_btl_openib_endpoint_t *endpoint,
                               mca_btl_openib_rem_info_t *rem_info)
{
    int rc;

    BTL_VERBOSE(("Initialized QPs, LID = %d",
                 ((mca_btl_openib_module_t*)endpoint->endpoint_btl)->lid));

    /* Create local QP's and post receive resources */
    if (OMPI_SUCCESS != (rc = qp_create_all(endpoint))) {
        return rc;
    }

    /* Set the remote side info */
    set_remote_info(endpoint, rem_info);
    
    /* Connect to remote endpoint qp's */
    if (OMPI_SUCCESS != (rc = qp_connect_all(endpoint))) {
        return rc;
    }

    /* Send connection info over to remote endpoint */
    endpoint->endpoint_state = MCA_BTL_IB_CONNECT_ACK;
    if (OMPI_SUCCESS !=
        (rc = send_connect_data(endpoint, ENDPOINT_CONNECT_RESPONSE))) {
        BTL_ERROR(("error in endpoint send connect request error code is %d",
                   rc));
        return rc;
    }
    return OMPI_SUCCESS;
}


static int set_remote_info(mca_btl_base_endpoint_t* endpoint,
                           mca_btl_openib_rem_info_t* rem_info)
{
    /* Free up the memory pointed to by rem_qps before overwriting the pointer
       in the following memcpy */
    free(endpoint->rem_info.rem_qps);

    /* copy the rem_info stuff */
    memcpy(&((mca_btl_openib_endpoint_t*) endpoint)->rem_info, 
           rem_info, sizeof(mca_btl_openib_rem_info_t)); 
    
    BTL_VERBOSE(("Setting QP info,  LID = %d", endpoint->rem_info.rem_lid));
    return ORTE_SUCCESS;

}


/*
 * Connect the local ends of all qp's to the remote side
 */
static int qp_connect_all(mca_btl_openib_endpoint_t *endpoint)
{
    int i;
    mca_btl_openib_module_t* openib_btl =
        (mca_btl_openib_module_t*)endpoint->endpoint_btl;

    for (i = 0; i < mca_btl_openib_component.num_qps; i++) {
        struct ibv_qp_attr attr;
        struct ibv_qp* qp = endpoint->qps[i].qp->lcl_qp;
        enum ibv_mtu mtu = (openib_btl->device->mtu < endpoint->rem_info.rem_mtu) ?
            openib_btl->device->mtu : endpoint->rem_info.rem_mtu;

        memset(&attr, 0, sizeof(attr));
        attr.qp_state           = IBV_QPS_RTR;
        attr.path_mtu           = mtu;
        attr.dest_qp_num        = endpoint->rem_info.rem_qps[i].rem_qp_num;
        attr.rq_psn             = endpoint->rem_info.rem_qps[i].rem_psn;
        attr.max_dest_rd_atomic = mca_btl_openib_component.ib_max_rdma_dst_ops;
        attr.min_rnr_timer  = mca_btl_openib_component.ib_min_rnr_timer;
        attr.ah_attr.is_global     = 0;
        attr.ah_attr.dlid          = endpoint->rem_info.rem_lid;
        attr.ah_attr.src_path_bits = openib_btl->src_path_bits;
        attr.ah_attr.port_num      = openib_btl->port_num;
        attr.ah_attr.sl            = mca_btl_openib_component.ib_service_level;

#if (ENABLE_DYNAMIC_SL)
        /* if user enabled dynamic SL, get it from PathRecord */
        if (0 != mca_btl_openib_component.ib_path_record_service_level) {
            int rc = btl_openib_connect_get_pathrecord_sl(qp->context,
                                                          attr.ah_attr.port_num,
                                                          openib_btl->lid,
                                                          attr.ah_attr.dlid);
            if (OMPI_ERROR == rc) {
                return OMPI_ERROR;
            }
            attr.ah_attr.sl = rc;
        }
#endif

        /* JMS to be filled in later dynamically */
        attr.ah_attr.static_rate   = 0;

        if (mca_btl_openib_component.verbose) {
            BTL_OUTPUT(("Set MTU to IBV value %d (%s bytes)", mtu,
                        (mtu == IBV_MTU_256) ? "256" :
                        (mtu == IBV_MTU_512) ? "512" :
                        (mtu == IBV_MTU_1024) ? "1024" :
                        (mtu == IBV_MTU_2048) ? "2048" :
                        (mtu == IBV_MTU_4096) ? "4096" :
                        "unknown (!)"));
        }

        if (ibv_modify_qp(qp, &attr,
                          IBV_QP_STATE              |
                          IBV_QP_AV                 |
                          IBV_QP_PATH_MTU           |
                          IBV_QP_DEST_QPN           |
                          IBV_QP_RQ_PSN             |
                          IBV_QP_MAX_DEST_RD_ATOMIC |
                          IBV_QP_MIN_RNR_TIMER)) {
            BTL_ERROR(("error modifing QP to RTR errno says %s",
                       strerror(errno)));
            return OMPI_ERROR; 
        }
        attr.qp_state       = IBV_QPS_RTS;
        attr.timeout        = mca_btl_openib_component.ib_timeout;
        attr.retry_cnt      = mca_btl_openib_component.ib_retry_count;
        /* On PP QPs we have SW flow control, no need for rnr retries. Setting
         * it to zero helps to catch bugs */
        attr.rnr_retry      = BTL_OPENIB_QP_TYPE_PP(i) ? 0 :
            mca_btl_openib_component.ib_rnr_retry;
        attr.sq_psn         = endpoint->qps[i].qp->lcl_psn;
        attr.max_rd_atomic  = mca_btl_openib_component.ib_max_rdma_dst_ops;
        if (ibv_modify_qp(qp, &attr,
                          IBV_QP_STATE              |
                          IBV_QP_TIMEOUT            |
                          IBV_QP_RETRY_CNT          |
                          IBV_QP_RNR_RETRY          |
                          IBV_QP_SQ_PSN             |
                          IBV_QP_MAX_QP_RD_ATOMIC)) {
            BTL_ERROR(("error modifying QP to RTS errno says %s",
                       strerror(errno)));
            return OMPI_ERROR;
        }
    }

    return OMPI_SUCCESS;
}


/*
 * Create the local side of all the qp's.  The remote sides will be
 * connected later.
 */
static int qp_create_all(mca_btl_base_endpoint_t* endpoint)
{
    int qp, rc, pp_qp_num = 0;
    int32_t rd_rsv_total = 0;

    for (qp = 0; qp < mca_btl_openib_component.num_qps; ++qp)
        if(BTL_OPENIB_QP_TYPE_PP(qp)) {
            rd_rsv_total +=
                mca_btl_openib_component.qp_infos[qp].u.pp_qp.rd_rsv;
            pp_qp_num++;
        }

    /* if there is no pp QPs we still need reserved WQE for eager rdma flow
     * control */
    if(0 == pp_qp_num && true == endpoint->use_eager_rdma)
        pp_qp_num = 1;

    for (qp = 0; qp < mca_btl_openib_component.num_qps; ++qp) { 
        struct ibv_srq *srq = NULL;
        uint32_t max_recv_wr, max_send_wr;
        int32_t rd_rsv, rd_num_credits;

        /* QP used for SW flow control need some additional recourses */
        if(qp == mca_btl_openib_component.credits_qp) {
            rd_rsv = rd_rsv_total;
            rd_num_credits = pp_qp_num;
        } else {
            rd_rsv = rd_num_credits = 0;
        }

        if(BTL_OPENIB_QP_TYPE_PP(qp)) {
            max_recv_wr = mca_btl_openib_component.qp_infos[qp].rd_num + rd_rsv;
            max_send_wr = mca_btl_openib_component.qp_infos[qp].rd_num +
                rd_num_credits;
        } else {
            srq = endpoint->endpoint_btl->qps[qp].u.srq_qp.srq;
            /* no receives are posted to SRQ qp */
            max_recv_wr = 0;
            max_send_wr = mca_btl_openib_component.qp_infos[qp].u.srq_qp.sd_max
                + rd_num_credits;
        }

        rc = qp_create_one(endpoint, qp, srq, max_recv_wr, max_send_wr);
        if (OMPI_SUCCESS != rc) {
            return rc;
        }
    }

    /* Now that all the qp's are created locally, post some receive
       buffers, setup credits, etc. */
    return mca_btl_openib_endpoint_post_recvs(endpoint);
}


/* Returns max inlne size for qp #N */
static uint32_t max_inline_size(int qp, mca_btl_openib_device_t *device)
{
    if (mca_btl_openib_component.qp_infos[qp].size <= device->max_inline_data) {
        /* If qp message size is smaller than max_inline_data,
         * we should enable inline messages */
        return mca_btl_openib_component.qp_infos[qp].size;
    } else if (mca_btl_openib_component.rdma_qp == qp || 0 == qp) {
        /* If qp message size is bigger that max_inline_data, we
         * should enable inline messages only for RDMA QP (for PUT/GET
         * fin messages) and for the first qp */
        return device->max_inline_data;
    }
    /* Otherway it is no reason for inline */
    return 0;
}

/*
 * Create the local side of one qp.  The remote side will be connected
 * later.
 */
static int qp_create_one(mca_btl_base_endpoint_t* endpoint, int qp, 
        struct ibv_srq *srq, uint32_t max_recv_wr, uint32_t max_send_wr)
{
    mca_btl_openib_module_t *openib_btl = endpoint->endpoint_btl;
    struct ibv_qp *my_qp;
    struct ibv_qp_init_attr init_attr;
    struct ibv_qp_attr attr;
    size_t req_inline;

    memset(&init_attr, 0, sizeof(init_attr));
    memset(&attr, 0, sizeof(attr));

    init_attr.qp_type = IBV_QPT_RC;
    init_attr.send_cq = openib_btl->device->ib_cq[BTL_OPENIB_LP_CQ];
    init_attr.recv_cq = openib_btl->device->ib_cq[qp_cq_prio(qp)];
    init_attr.srq     = srq;
    init_attr.cap.max_inline_data = req_inline = 
        max_inline_size(qp, openib_btl->device);
    init_attr.cap.max_send_sge = 1;
    init_attr.cap.max_recv_sge = 1; /* we do not use SG list */
    if(BTL_OPENIB_QP_TYPE_PP(qp)) {
        init_attr.cap.max_recv_wr  = max_recv_wr;
    } else {
        init_attr.cap.max_recv_wr  = 0;
    }
    init_attr.cap.max_send_wr  = max_send_wr;

    my_qp = ibv_create_qp(openib_btl->device->ib_pd, &init_attr);

    if (NULL == my_qp) {
	orte_show_help("help-mpi-btl-openib-cpc-base.txt",
		       "ibv_create_qp failed", true,
		       orte_process_info.nodename,
		       ibv_get_device_name(openib_btl->device->ib_dev),
		       "Reliable connected (RC)");
        return OMPI_ERROR;
    }
    endpoint->qps[qp].qp->lcl_qp = my_qp;

    if (init_attr.cap.max_inline_data < req_inline) {
        endpoint->qps[qp].ib_inline_max = init_attr.cap.max_inline_data;
        orte_show_help("help-mpi-btl-openib-cpc-base.txt",
                       "inline truncated", true, orte_process_info.nodename,
                       ibv_get_device_name(openib_btl->device->ib_dev),
                       openib_btl->port_num,
                       req_inline, init_attr.cap.max_inline_data);
    } else {
        endpoint->qps[qp].ib_inline_max = req_inline;
    }
    
    attr.qp_state        = IBV_QPS_INIT;
    attr.pkey_index      = openib_btl->pkey_index;
    attr.port_num        = openib_btl->port_num;
    attr.qp_access_flags = IBV_ACCESS_REMOTE_WRITE | IBV_ACCESS_REMOTE_READ;

    if (ibv_modify_qp(endpoint->qps[qp].qp->lcl_qp, 
                      &attr, 
                      IBV_QP_STATE | 
                      IBV_QP_PKEY_INDEX | 
                      IBV_QP_PORT | 
                      IBV_QP_ACCESS_FLAGS )) { 
        BTL_ERROR(("error modifying qp to INIT errno says %s", strerror(errno))); 
        return OMPI_ERROR; 
    } 

    /* Setup meta data on the endpoint */
    endpoint->qps[qp].qp->lcl_psn = lrand48() & 0xffffff;
    endpoint->qps[qp].credit_frag = NULL;

    return OMPI_SUCCESS;
}


/*
 * RML send connect information to remote endpoint
 */
static int send_connect_data(mca_btl_base_endpoint_t* endpoint, 
                             uint8_t message_type)
{
    opal_buffer_t* buffer = OBJ_NEW(opal_buffer_t);
    int rc;
    
    if (NULL == buffer) {
         ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
         return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* pack the info in the send buffer */ 
    BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT8));
    rc = opal_dss.pack(buffer, &message_type, 1, OPAL_UINT8);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT64));
    rc = opal_dss.pack(buffer, &endpoint->subnet_id, 1, OPAL_UINT64);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (message_type != ENDPOINT_CONNECT_REQUEST) {
        /* send the QP connect request info we respond to */
        BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
        rc = opal_dss.pack(buffer,
                           &endpoint->rem_info.rem_qps[0].rem_qp_num, 1,
                           OPAL_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT16));
        rc = opal_dss.pack(buffer, &endpoint->rem_info.rem_lid, 1, OPAL_UINT16);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    if (message_type != ENDPOINT_CONNECT_ACK) {
        int qp;
        /* stuff all the QP info into the buffer */
        for (qp = 0; qp < mca_btl_openib_component.num_qps; qp++) { 
            BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
            rc = opal_dss.pack(buffer, &endpoint->qps[qp].qp->lcl_qp->qp_num,
                               1, OPAL_UINT32);
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
            rc = opal_dss.pack(buffer, &endpoint->qps[qp].qp->lcl_psn, 1,
                               OPAL_UINT32); 
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
        
        BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT16));
        rc = opal_dss.pack(buffer, &endpoint->endpoint_btl->lid, 1, OPAL_UINT16);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
        rc = opal_dss.pack(buffer, &endpoint->endpoint_btl->device->mtu, 1,
                OPAL_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
        rc = opal_dss.pack(buffer, &endpoint->index, 1, OPAL_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    /* send to remote endpoint */
    rc = orte_rml.send_buffer_nb(&endpoint->endpoint_proc->proc_guid, 
                                 buffer, OMPI_RML_TAG_OPENIB, 0,
                                 rml_send_cb, NULL);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    BTL_VERBOSE(("Sent QP Info, LID = %d, SUBNET = %" PRIx64 "\n",
                 endpoint->endpoint_btl->lid, 
                 endpoint->subnet_id));

    return OMPI_SUCCESS;
}


/*
 * Callback when we have finished RML sending the connect data to a
 * remote peer
 */
static void rml_send_cb(int status, orte_process_name_t* endpoint, 
                        opal_buffer_t* buffer, orte_rml_tag_t tag, 
                        void* cbdata)
{
    OBJ_RELEASE(buffer);
}


/*
 * Non blocking RML recv callback.  Read incoming QP and other info,
 * and if this endpoint is trying to connect, reply with our QP info,
 * otherwise try to modify QP's and establish reliable connection
 */
static void rml_recv_cb(int status, orte_process_name_t* process_name, 
                        opal_buffer_t* buffer, orte_rml_tag_t tag, 
                        void* cbdata)
{
    mca_btl_openib_proc_t *ib_proc;
    mca_btl_openib_endpoint_t *ib_endpoint = NULL;
    int endpoint_state;
    int rc;
    uint32_t i, lcl_qp = 0;
    uint16_t lcl_lid = 0;
    int32_t cnt = 1;
    mca_btl_openib_rem_info_t rem_info;
    uint8_t message_type;
    bool master;

    /* We later memcpy this whole structure. Make sure
       that all the parameters are initialized, especially
       the pointers */
    memset(&rem_info,0, sizeof(rem_info));

    /* start by unpacking data first so we know who is knocking at 
       our door */ 
    BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT8));
    rc = opal_dss.unpack(buffer, &message_type, &cnt, OPAL_UINT8);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        mca_btl_openib_endpoint_invoke_error(NULL);
        return;
    }
    
    BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT64));
    rc = opal_dss.unpack(buffer, &rem_info.rem_subnet_id, &cnt, OPAL_UINT64);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        mca_btl_openib_endpoint_invoke_error(NULL);
        return;
    }
    
    if (ENDPOINT_CONNECT_REQUEST != message_type) {
        BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
        rc = opal_dss.unpack(buffer, &lcl_qp, &cnt, OPAL_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            mca_btl_openib_endpoint_invoke_error(NULL);
            return;
        }
        BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT16));
        rc = opal_dss.unpack(buffer, &lcl_lid, &cnt, OPAL_UINT16);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            mca_btl_openib_endpoint_invoke_error(NULL);
            return;
        }
    }
    if (ENDPOINT_CONNECT_ACK != message_type) {
        int qp; 
        /* get ready for the data */
        rem_info.rem_qps = 
            (mca_btl_openib_rem_qp_info_t*) malloc(sizeof(mca_btl_openib_rem_qp_info_t) * 
                                                   mca_btl_openib_component.num_qps);
        
        /* unpack all the qp info */
        for (qp = 0; qp < mca_btl_openib_component.num_qps; ++qp) { 
            BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
            rc = opal_dss.unpack(buffer, &rem_info.rem_qps[qp].rem_qp_num, &cnt,
                                 OPAL_UINT32);
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                mca_btl_openib_endpoint_invoke_error(NULL);
                return;
            }
            BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
            rc = opal_dss.unpack(buffer, &rem_info.rem_qps[qp].rem_psn, &cnt,
                                 OPAL_UINT32);
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                mca_btl_openib_endpoint_invoke_error(NULL);
                return;
            }
        }
        
        BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT16));
        rc = opal_dss.unpack(buffer, &rem_info.rem_lid, &cnt, OPAL_UINT16);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            mca_btl_openib_endpoint_invoke_error(NULL);
            return;
        }
        BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
        rc = opal_dss.unpack(buffer, &rem_info.rem_mtu, &cnt, OPAL_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            mca_btl_openib_endpoint_invoke_error(NULL);
            return;
        }
        BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
        rc = opal_dss.unpack(buffer, &rem_info.rem_index, &cnt, OPAL_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            mca_btl_openib_endpoint_invoke_error(NULL);
            return;
        }
    }
    
    BTL_VERBOSE(("Received QP Info,  LID = %d, SUBNET = %" PRIx64 "\n",
                 rem_info.rem_lid, 
                 rem_info.rem_subnet_id));
    
    master = orte_util_compare_name_fields(ORTE_NS_CMP_ALL, ORTE_PROC_MY_NAME,
                                    process_name) > 0 ? true : false;
    
    /* Need to protect the ib_procs list */
    OPAL_THREAD_LOCK(&mca_btl_openib_component.ib_lock);

    for (ib_proc = (mca_btl_openib_proc_t*)
            opal_list_get_first(&mca_btl_openib_component.ib_procs);
        ib_proc != (mca_btl_openib_proc_t*)
            opal_list_get_end(&mca_btl_openib_component.ib_procs);
        ib_proc  = (mca_btl_openib_proc_t*)opal_list_get_next(ib_proc)) {
        bool found = false;
        
        if (orte_util_compare_name_fields(ORTE_NS_CMP_ALL,
                                   &ib_proc->proc_guid, process_name) != OPAL_EQUAL) {
            continue;
        }
        
        if (ENDPOINT_CONNECT_REQUEST != message_type) {
            /* This is a reply message. Try to get the endpoint
               instance the reply belongs to */
            for (i = 0; i < ib_proc->proc_endpoint_count; i++) { 
                ib_endpoint = ib_proc->proc_endpoints[i];
                if (ib_endpoint->qps[0].qp->lcl_qp != NULL &&
                    lcl_lid == ib_endpoint->endpoint_btl->lid &&
                    lcl_qp == ib_endpoint->qps[0].qp->lcl_qp->qp_num &&
                    rem_info.rem_subnet_id == ib_endpoint->subnet_id) {
                    found = true;
                    break;
                }
            }
        } else {
            /* This is new connection request. If this is master try
               to find endpoint in a connecting state. If this is
               slave try to find  endpoint in closed state and
               initiate connection back */
            mca_btl_openib_endpoint_t *ib_endpoint_found = NULL;
            int master_first_closed = -1;

            for (i = 0; i < ib_proc->proc_endpoint_count; i++) { 
                ib_endpoint = ib_proc->proc_endpoints[i];
                if (ib_endpoint->subnet_id != rem_info.rem_subnet_id ||
                   (ib_endpoint->endpoint_state != MCA_BTL_IB_CONNECTING
                    && ib_endpoint->endpoint_state != MCA_BTL_IB_CLOSED))
                    continue;
                found = true;
                ib_endpoint_found = ib_endpoint;

                if (master && -1 == master_first_closed &&
                    MCA_BTL_IB_CLOSED == ib_endpoint->endpoint_state ) {
                    /* capture in case no endpoint in connecting state */
                    master_first_closed = i;
                }

                if ((master &&
                     MCA_BTL_IB_CONNECTING == ib_endpoint->endpoint_state) ||
                    (!master &&
                     MCA_BTL_IB_CLOSED == ib_endpoint->endpoint_state))
                    break; /* Found one. No point to continue */
            }
            ib_endpoint = ib_endpoint_found;
            
            if (found && master &&
                MCA_BTL_IB_CLOSED == ib_endpoint->endpoint_state ) {
                /* since this is master and no endpoints found in
                 * connecting state use the first endpoint found
                 * in closed state */
                ib_endpoint = ib_proc->proc_endpoints[master_first_closed];
            }

            /* if this is slave and there is no endpoints in closed
               state then all connection are already in progress so
               just ignore this connection request */
            if (found && !master &&
                MCA_BTL_IB_CLOSED != ib_endpoint->endpoint_state) {
                OPAL_THREAD_UNLOCK(&mca_btl_openib_component.ib_lock);
                return;
            }
        }
        
        if (!found) {
            BTL_ERROR(("can't find suitable endpoint for this peer\n")); 
            mca_btl_openib_endpoint_invoke_error(NULL);
            OPAL_THREAD_UNLOCK(&mca_btl_openib_component.ib_lock);
            return; 
        }
        
        OPAL_THREAD_LOCK(&ib_endpoint->endpoint_lock);
        endpoint_state = ib_endpoint->endpoint_state;
        
        /* Update status */
        switch (endpoint_state) {
        case MCA_BTL_IB_CLOSED :
            /* We had this connection closed before.  The endpoint is
               trying to connect. Move the status of this connection
               to CONNECTING, and then reply with our QP
               information */
            if (master) {
                assert(rem_info.rem_qps != NULL);
                rc = reply_start_connect(ib_endpoint, &rem_info);
            } else {
                rc = oob_module_start_connect(ib_endpoint->endpoint_local_cpc, 
                                              ib_endpoint);
            }
            
            if (OMPI_SUCCESS != rc) {
                BTL_ERROR(("error in endpoint reply start connect"));
                mca_btl_openib_endpoint_invoke_error(ib_endpoint);
                OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
                break;
            }
            
            /* As long as we expect a message from the peer (in order
               to setup the connection) let the event engine pool the
               RML events. Note: we increment it once peer active
               connection. */
            opal_progress_event_users_increment();
            OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
            break;
             
        case MCA_BTL_IB_CONNECTING :
            assert(rem_info.rem_qps != NULL);
            set_remote_info(ib_endpoint, &rem_info);
            if (OMPI_SUCCESS != (rc = qp_connect_all(ib_endpoint))) {
                BTL_ERROR(("endpoint connect error: %d", rc)); 
                mca_btl_openib_endpoint_invoke_error(ib_endpoint);
                OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
                break;
            }
           
            if (master) {
                ib_endpoint->endpoint_state = MCA_BTL_IB_WAITING_ACK;

                /* Send him an ACK */
                send_connect_data(ib_endpoint, ENDPOINT_CONNECT_RESPONSE);
                OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
            } else {
                send_connect_data(ib_endpoint, ENDPOINT_CONNECT_ACK);
                /* Tell main BTL that we're done */
                mca_btl_openib_endpoint_cpc_complete(ib_endpoint);
                /* cpc complete unlock the endpoint */
             }
            break;
            
        case MCA_BTL_IB_WAITING_ACK:
            /* Tell main BTL that we're done */
            mca_btl_openib_endpoint_cpc_complete(ib_endpoint);
            /* cpc complete unlock the endpoint */
            break;
            
        case MCA_BTL_IB_CONNECT_ACK:
            send_connect_data(ib_endpoint, ENDPOINT_CONNECT_ACK);
            /* Tell main BTL that we're done */
            mca_btl_openib_endpoint_cpc_complete(ib_endpoint);
            /* cpc complete unlock the endpoint */
            break;
            
        case MCA_BTL_IB_CONNECTED:
            OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
            break;

        default :
            BTL_ERROR(("Invalid endpoint state %d", endpoint_state));
            mca_btl_openib_endpoint_invoke_error(ib_endpoint);
            OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
        }
        break;
    }
    OPAL_THREAD_UNLOCK(&mca_btl_openib_component.ib_lock);
}
