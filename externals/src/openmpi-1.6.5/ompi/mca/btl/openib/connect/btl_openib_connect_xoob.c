/*
 * Copyright (c) 2007-2011 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      IBM Corporation.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal_stdint.h"
#include "opal/dss/dss.h"
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "orte/util/show_help.h"
#include "orte/util/name_fns.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/errmgr/errmgr.h"
#include "ompi/mca/dpm/dpm.h"

#include "btl_openib.h"
#include "btl_openib_endpoint.h"
#include "btl_openib_proc.h"
#include "btl_openib_xrc.h"
#include "btl_openib_async.h"
#include "connect/connect.h"
#include "orte/util/show_help.h"
#if (ENABLE_DYNAMIC_SL)
#include "connect/btl_openib_connect_sl.h"
#endif

static void xoob_component_register(void);
static int xoob_component_query(mca_btl_openib_module_t *openib_btl, 
                                ompi_btl_openib_connect_base_module_t **cpc);
static int xoob_component_finalize(void);

static int xoob_module_start_connect(ompi_btl_openib_connect_base_module_t *cpc,
                                     mca_btl_base_endpoint_t *endpoint);

/*
 * The "component" struct -- the top-level function pointers for the
 * xoob connection scheme.
 */
ompi_btl_openib_connect_base_component_t ompi_btl_openib_connect_xoob = {
    "xoob",
    /* Register */
    xoob_component_register,
    /* Init */
    NULL,
    /* Query */
    xoob_component_query,
    /* Finalize */
    xoob_component_finalize,
};

typedef enum {
    ENDPOINT_XOOB_CONNECT_REQUEST,
    ENDPOINT_XOOB_CONNECT_RESPONSE,
    ENDPOINT_XOOB_CONNECT_XRC_REQUEST,
    ENDPOINT_XOOB_CONNECT_XRC_RESPONSE,
    ENDPOINT_XOOB_CONNECT_XRC_NR_RESPONSE /* The xrc recv qp already was destroyed */
} connect_message_type_t;

static bool rml_recv_posted = false;

#define XOOB_SET_REMOTE_INFO(EP, INFO)                                    \
do {                                                                      \
    /* copy the rem_info stuff */                                         \
    EP.rem_lid       = INFO.rem_lid;                                      \
    EP.rem_subnet_id = INFO.rem_subnet_id;                                \
    EP.rem_mtu       = INFO.rem_mtu;                                      \
    EP.rem_index     = INFO.rem_index;                                    \
    memcpy((void*)EP.rem_qps, (void*)INFO.rem_qps,                        \
            sizeof(mca_btl_openib_rem_qp_info_t));                        \
    /* copy the rem_info stuff */                                         \
    memcpy((void*)EP.rem_srqs, (void*)INFO.rem_srqs,                      \
            sizeof(mca_btl_openib_rem_srq_info_t) *                       \
            mca_btl_openib_component.num_xrc_qps);                        \
} while (0)

static int xoob_priority = 60;

/*
 * Callback when we have finished RML sending the connect data to a
 * remote peer
 */
static void xoob_rml_send_cb(int status, orte_process_name_t* endpoint,
                        opal_buffer_t* buffer, orte_rml_tag_t tag,
                        void* cbdata)
{
    OBJ_RELEASE(buffer);
}

/* Receive connect information to remote endpoint */
static int xoob_receive_connect_data(mca_btl_openib_rem_info_t *info, uint16_t *lid,
        uint8_t *message_type, opal_buffer_t* buffer)
{
    int cnt = 1, rc, srq;

    /* Recv standart header */
    BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT8));
    rc = opal_dss.unpack(buffer, message_type, &cnt, OPAL_UINT8);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return OMPI_ERROR;
    }
    BTL_VERBOSE(("Recv unpack Message type  = %d\n", *message_type));

    BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT64));
    rc = opal_dss.unpack(buffer, &info->rem_subnet_id, &cnt, OPAL_UINT64);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return OMPI_ERROR;
    }
    BTL_VERBOSE(("Recv unpack sid  = %" PRIx64 "\n", info->rem_subnet_id));

    BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT16));
    rc = opal_dss.unpack(buffer, &info->rem_lid, &cnt, OPAL_UINT16);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return OMPI_ERROR;
    }
    BTL_VERBOSE(("Recv unpack lid  = %d", info->rem_lid));

    /* Till now we got the standart header, now we continue to recieve data for
     * different packet types
     */
    if (ENDPOINT_XOOB_CONNECT_REQUEST == *message_type ||
            ENDPOINT_XOOB_CONNECT_RESPONSE == *message_type) {
        BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
        rc = opal_dss.unpack(buffer, &info->rem_qps->rem_qp_num, &cnt,
                OPAL_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return OMPI_ERROR;
        }
        BTL_VERBOSE(("Recv unpack remote qp  = %x", info->rem_qps->rem_qp_num));

        BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
        rc = opal_dss.unpack(buffer, &info->rem_qps->rem_psn, &cnt,
                OPAL_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return OMPI_ERROR;
        }
        BTL_VERBOSE(("Recv unpack remote psn = %d", info->rem_qps->rem_psn));

        BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
        rc = opal_dss.unpack(buffer, &info->rem_mtu, &cnt, OPAL_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return OMPI_ERROR;
        }
        BTL_VERBOSE(("Recv unpack remote mtu = %d", info->rem_mtu));
    }

    if (ENDPOINT_XOOB_CONNECT_REQUEST == *message_type ||
            ENDPOINT_XOOB_CONNECT_XRC_REQUEST == *message_type) {
        /* unpack requested lid info */
        BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT16));
        rc = opal_dss.unpack(buffer, lid, &cnt, OPAL_UINT16);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return OMPI_ERROR;
        }
        BTL_VERBOSE(("Recv unpack requested lid = %d", *lid));
    }

    /* Unpack requested recv qp number */
    if (ENDPOINT_XOOB_CONNECT_XRC_REQUEST == *message_type) {
        BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
        /* In XRC request case we will use rem_qp_num as container for requested qp number */
        rc = opal_dss.unpack(buffer, &info->rem_qps->rem_qp_num, &cnt,
                OPAL_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        BTL_VERBOSE(("Recv unpack requested qp = %x", info->rem_qps->rem_qp_num));
    }

    if (ENDPOINT_XOOB_CONNECT_RESPONSE == *message_type ||
            ENDPOINT_XOOB_CONNECT_XRC_RESPONSE == *message_type) {
        BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
        rc = opal_dss.unpack(buffer, &info->rem_index, &cnt, OPAL_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return OMPI_ERROR;
        }
        BTL_VERBOSE(("Recv unpack remote index = %d", info->rem_index));

        for (srq = 0; srq < mca_btl_openib_component.num_xrc_qps; srq++) {
            BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
            rc = opal_dss.unpack(buffer, &info->rem_srqs[srq].rem_srq_num, &cnt, OPAL_UINT32);
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                return OMPI_ERROR;
            }
            BTL_VERBOSE(("Recv unpack remote index srq num[%d]= %d", srq, info->rem_srqs[srq].rem_srq_num));
        }
    }
    return OMPI_SUCCESS;
}

/*
 * send connect information to remote endpoint
 */
static int xoob_send_connect_data(mca_btl_base_endpoint_t* endpoint,
        uint8_t message_type)
{
    opal_buffer_t* buffer = OBJ_NEW(opal_buffer_t);
    int rc, srq;

    if (NULL == buffer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* Bulding standart header that we use in all messages:
     * - Message type,
     * - Our subnet id
     * - Our LID
     */
    /* pack the info in the send buffer */
    BTL_VERBOSE(("Send pack Message type = %d", message_type));
    BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT8));
    rc = opal_dss.pack(buffer, &message_type, 1, OPAL_UINT8);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    BTL_VERBOSE(("Send pack sid = %" PRIx64 "\n", endpoint->subnet_id));
    BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT64));
    rc = opal_dss.pack(buffer, &endpoint->subnet_id, 1, OPAL_UINT64);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    BTL_VERBOSE(("Send pack lid = %d", endpoint->endpoint_btl->lid));
    BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT16));
    rc = opal_dss.pack(buffer, &endpoint->endpoint_btl->lid, 1, OPAL_UINT16);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* Now we append to standart header additional information
     * that is required for full (open qp,etc..) connect request and response:
     * - qp_num of first qp
     * - psn of first qp
     * - MTU
     */
    if (ENDPOINT_XOOB_CONNECT_REQUEST == message_type ||
            ENDPOINT_XOOB_CONNECT_RESPONSE == message_type) {
        uint32_t psn, qp_num;

        if (ENDPOINT_XOOB_CONNECT_REQUEST == message_type) {
            qp_num = endpoint->qps[0].qp->lcl_qp->qp_num;
            psn = endpoint->qps[0].qp->lcl_psn;
        } else {
            qp_num = endpoint->xrc_recv_qp_num;
            psn = endpoint->xrc_recv_psn;
        }
        /* stuff all the QP info into the buffer */
        /* we need to send only one QP */
        BTL_VERBOSE(("Send pack qp num = %x", qp_num));
        BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
        rc = opal_dss.pack(buffer, &qp_num, 1, OPAL_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        BTL_VERBOSE(("Send pack lpsn = %d", psn));
        BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
        rc = opal_dss.pack(buffer, &psn, 1, OPAL_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        BTL_VERBOSE(("Send pack mtu = %d", endpoint->endpoint_btl->device->mtu));
        BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
        rc = opal_dss.pack(buffer, &endpoint->endpoint_btl->device->mtu, 1,
                OPAL_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    /* We append to header above additional information
     * that is required for full & XRC connect request:
     * - The lid ob btl on remote site that we want to connect
     */
    if (ENDPOINT_XOOB_CONNECT_REQUEST == message_type ||
            ENDPOINT_XOOB_CONNECT_XRC_REQUEST == message_type) {
        /* when we are sending request we add remote lid that we want to connect */

        BTL_VERBOSE(("Send pack remote lid = %d", endpoint->ib_addr->lid));
        BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT16));
        rc = opal_dss.pack(buffer, &endpoint->ib_addr->lid, 1, OPAL_UINT16);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    /* when we are sending xrc request we add remote
     * recv qp number that we want to connect. */
    if (ENDPOINT_XOOB_CONNECT_XRC_REQUEST == message_type) {
        BTL_VERBOSE(("Send pack remote qp = %x", endpoint->ib_addr->remote_xrc_rcv_qp_num));
        BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
        rc = opal_dss.pack(buffer, &endpoint->ib_addr->remote_xrc_rcv_qp_num,
                1, OPAL_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    /* We append to header above additional information
     * that is required for full & XRC connect response:
     * - index of our endpoint
     * - array of xrc-srq numbers
     */
    if (ENDPOINT_XOOB_CONNECT_RESPONSE == message_type ||
            ENDPOINT_XOOB_CONNECT_XRC_RESPONSE == message_type) {
        /* we need to send the endpoint index for immidate send */
        BTL_VERBOSE(("Send pack index = %d", endpoint->index));
        BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
        rc = opal_dss.pack(buffer, &endpoint->index, 1, OPAL_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* on response we add all SRQ numbers */
        for (srq = 0; srq < mca_btl_openib_component.num_xrc_qps; srq++) {
            BTL_VERBOSE(("Send pack srq[%d] num  = %d", srq, endpoint->endpoint_btl->qps[srq].u.srq_qp.srq->xrc_srq_num));
            BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
            rc = opal_dss.pack(buffer, &endpoint->endpoint_btl->qps[srq].u.srq_qp.srq->xrc_srq_num,
                    1, OPAL_UINT32);
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }

    /* send to remote endpoint */
    rc = orte_rml.send_buffer_nb(&endpoint->endpoint_proc->proc_guid,
            buffer, OMPI_RML_TAG_XOPENIB, 0,
            xoob_rml_send_cb, NULL);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    BTL_VERBOSE(("Send QP Info, LID = %d, SUBNET = %" PRIx64 ", Message type = %d",
                endpoint->endpoint_btl->lid,
                endpoint->subnet_id,
                message_type));

    return OMPI_SUCCESS;
}

/* Create XRC send qp */
static int xoob_send_qp_create (mca_btl_base_endpoint_t* endpoint)
{
    int prio = BTL_OPENIB_LP_CQ; /* all send completions go to low prio CQ */
    uint32_t send_wr;
    struct ibv_qp **qp;
    uint32_t *psn;
    struct ibv_qp_init_attr qp_init_attr;
    struct ibv_qp_attr attr;
    int ret;
    size_t req_inline;

    mca_btl_openib_module_t *openib_btl =
        (mca_btl_openib_module_t*)endpoint->endpoint_btl;

    /* Prepare QP structs */
    BTL_VERBOSE(("Creating Send QP\n"));
    qp = &endpoint->qps[0].qp->lcl_qp;
    psn = &endpoint->qps[0].qp->lcl_psn;
    /* reserve additional wr for eager rdma credit management */
    send_wr = endpoint->ib_addr->qp->sd_wqe +
        (mca_btl_openib_component.use_eager_rdma ?
         mca_btl_openib_component.max_eager_rdma : 0);
    memset(&qp_init_attr, 0, sizeof(struct ibv_qp_init_attr));
    memset(&attr, 0, sizeof(struct ibv_qp_attr));

    qp_init_attr.send_cq = qp_init_attr.recv_cq = openib_btl->device->ib_cq[prio];

    /* no need recv queue; receives are posted to srq */
    qp_init_attr.cap.max_recv_wr = 0;
    qp_init_attr.cap.max_send_wr = send_wr;
    qp_init_attr.cap.max_inline_data = req_inline =
        openib_btl->device->max_inline_data;
    qp_init_attr.cap.max_send_sge = 1;
    /* this one is ignored by driver */
    qp_init_attr.cap.max_recv_sge = 1; /* we do not use SG list */
    qp_init_attr.qp_type = IBV_QPT_XRC;
    qp_init_attr.xrc_domain = openib_btl->device->xrc_domain;
    *qp = ibv_create_qp(openib_btl->device->ib_pd, &qp_init_attr);
    if (NULL == *qp) {
	orte_show_help("help-mpi-btl-openib-cpc-base.txt",
		       "ibv_create_qp failed", true,
		       orte_process_info.nodename,
		       ibv_get_device_name(openib_btl->device->ib_dev),
		       "Reliable connected (XRC)");
        return OMPI_ERROR;
    }

    if (qp_init_attr.cap.max_inline_data < req_inline) {
        endpoint->qps[0].ib_inline_max = qp_init_attr.cap.max_inline_data;
        orte_show_help("help-mpi-btl-openib-cpc-base.txt",
                       "inline truncated", orte_process_info.nodename,
                       ibv_get_device_name(openib_btl->device->ib_dev),
                       openib_btl->port_num,
                       req_inline, qp_init_attr.cap.max_inline_data);
    } else {
        endpoint->qps[0].ib_inline_max = req_inline;
    }

    attr.qp_state = IBV_QPS_INIT;
    attr.pkey_index = openib_btl->pkey_index;
    attr.port_num = openib_btl->port_num;
    attr.qp_access_flags = IBV_ACCESS_REMOTE_WRITE | IBV_ACCESS_REMOTE_READ;
    ret = ibv_modify_qp(*qp, &attr,
                      IBV_QP_STATE |
                      IBV_QP_PKEY_INDEX |
                      IBV_QP_PORT |
                      IBV_QP_ACCESS_FLAGS );
    if (ret) {
        BTL_ERROR(("Error modifying QP[%x] to IBV_QPS_INIT errno says: %s [%d]",
                    (*qp)->qp_num, strerror(ret), ret));
        return OMPI_ERROR;
    }

    /* Setup meta data on the endpoint */
    *psn = lrand48() & 0xffffff;

    /* Now that all the qp's are created locally, post some receive
       buffers, setup credits, etc. */
    return mca_btl_openib_endpoint_post_recvs(endpoint);
}

/* Send qp connect */
static int xoob_send_qp_connect(mca_btl_openib_endpoint_t *endpoint, mca_btl_openib_rem_info_t *rem_info)
{
    struct ibv_qp* qp;
    struct ibv_qp_attr attr;
    uint32_t psn;
    int ret;

    mca_btl_openib_module_t* openib_btl =
        (mca_btl_openib_module_t*)endpoint->endpoint_btl;

    BTL_VERBOSE(("Connecting Send QP\n"));
    assert(NULL != endpoint->qps);
    qp = endpoint->qps[0].qp->lcl_qp;
    psn = endpoint->qps[0].qp->lcl_psn;

    memset(&attr, 0, sizeof(attr));
    attr.qp_state           = IBV_QPS_RTR;
    attr.path_mtu = (openib_btl->device->mtu < endpoint->rem_info.rem_mtu) ?
        openib_btl->device->mtu : rem_info->rem_mtu;
    attr.dest_qp_num        = rem_info->rem_qps->rem_qp_num;
    attr.rq_psn             = rem_info->rem_qps->rem_psn;
    attr.max_dest_rd_atomic = mca_btl_openib_component.ib_max_rdma_dst_ops;
    attr.min_rnr_timer  = mca_btl_openib_component.ib_min_rnr_timer;
    attr.ah_attr.is_global     = 0;
    attr.ah_attr.dlid          = rem_info->rem_lid;
    attr.ah_attr.src_path_bits = openib_btl->src_path_bits;
    attr.ah_attr.port_num      = openib_btl->port_num;
    attr.ah_attr.static_rate   = 0;
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

    if (mca_btl_openib_component.verbose) {
        BTL_VERBOSE(("Set MTU to IBV value %d (%s bytes)", attr.path_mtu,
                    (attr.path_mtu == IBV_MTU_256) ? "256" :
                    (attr.path_mtu == IBV_MTU_512) ? "512" :
                    (attr.path_mtu == IBV_MTU_1024) ? "1024" :
                    (attr.path_mtu == IBV_MTU_2048) ? "2048" :
                    (attr.path_mtu == IBV_MTU_4096) ? "4096" :
                    "unknown (!)"));
    }
    ret = ibv_modify_qp(qp, &attr,
                IBV_QP_STATE              |
                IBV_QP_AV                 |
                IBV_QP_PATH_MTU           |
                IBV_QP_DEST_QPN           |
                IBV_QP_RQ_PSN             |
                IBV_QP_MAX_DEST_RD_ATOMIC |
                IBV_QP_MIN_RNR_TIMER);
    if (ret) {
        BTL_ERROR(("Error modifying QP[%x] to IBV_QPS_RTR errno says: %s [%d]",
                    qp->qp_num, strerror(ret), ret));
        return OMPI_ERROR;
    }

    attr.qp_state       = IBV_QPS_RTS;
    attr.timeout        = mca_btl_openib_component.ib_timeout;
    attr.retry_cnt      = mca_btl_openib_component.ib_retry_count;
    attr.rnr_retry      = mca_btl_openib_component.ib_rnr_retry;
    attr.sq_psn         = psn;
    attr.max_rd_atomic  = mca_btl_openib_component.ib_max_rdma_dst_ops;
    ret = ibv_modify_qp(qp, &attr,
            IBV_QP_STATE              |
            IBV_QP_TIMEOUT            |
            IBV_QP_RETRY_CNT          |
            IBV_QP_RNR_RETRY          |
            IBV_QP_SQ_PSN             |
            IBV_QP_MAX_QP_RD_ATOMIC);
    if (ret) {
        BTL_ERROR(("Error modifying QP[%x] to IBV_QPS_RTS errno says: %s [%d]",
                    qp->qp_num, strerror(ret), ret));
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

/* Recv qp create */
static int xoob_recv_qp_create(mca_btl_openib_endpoint_t *endpoint, mca_btl_openib_rem_info_t *rem_info)
{
    struct ibv_qp_init_attr qp_init_attr;
    struct ibv_qp_attr attr;
    int ret;

    mca_btl_openib_module_t* openib_btl =
        (mca_btl_openib_module_t*)endpoint->endpoint_btl;

    BTL_VERBOSE(("Connecting Recv QP\n"));

    memset(&qp_init_attr, 0, sizeof(struct ibv_qp_init_attr));
    /* Only xrc_domain is required, all other are ignored */
    qp_init_attr.xrc_domain = openib_btl->device->xrc_domain;
    ret = ibv_create_xrc_rcv_qp(&qp_init_attr, &endpoint->xrc_recv_qp_num);
    if (ret) {
        BTL_ERROR(("Error creating XRC recv QP[%x], errno says: %s [%d]",
                    endpoint->xrc_recv_qp_num, strerror(ret), ret));
        return OMPI_ERROR;
    }

    memset(&attr, 0, sizeof(struct ibv_qp_attr));
    attr.qp_state = IBV_QPS_INIT;
    attr.pkey_index = openib_btl->pkey_index;
    attr.port_num = openib_btl->port_num;
    attr.qp_access_flags = IBV_ACCESS_REMOTE_WRITE | IBV_ACCESS_REMOTE_READ;
    ret = ibv_modify_xrc_rcv_qp(openib_btl->device->xrc_domain,
            endpoint->xrc_recv_qp_num,
            &attr,
            IBV_QP_STATE|
            IBV_QP_PKEY_INDEX|
            IBV_QP_PORT|
            IBV_QP_ACCESS_FLAGS);
    if (ret) {
        BTL_ERROR(("Error modifying XRC recv QP[%x] to IBV_QPS_INIT, errno says: %s [%d]",
                     endpoint->xrc_recv_qp_num, strerror(ret), ret));
        while(1);
        return OMPI_ERROR;
    }

    memset(&attr, 0, sizeof(struct ibv_qp_attr));
    attr.qp_state           = IBV_QPS_RTR;
    attr.path_mtu = (openib_btl->device->mtu < endpoint->rem_info.rem_mtu) ?
        openib_btl->device->mtu : rem_info->rem_mtu;
    attr.dest_qp_num        = rem_info->rem_qps->rem_qp_num;
    attr.rq_psn             = rem_info->rem_qps->rem_psn;
    attr.max_dest_rd_atomic = mca_btl_openib_component.ib_max_rdma_dst_ops;
    attr.min_rnr_timer  = mca_btl_openib_component.ib_min_rnr_timer;
    attr.ah_attr.is_global     = 0;
    attr.ah_attr.dlid          = rem_info->rem_lid;
    attr.ah_attr.src_path_bits = openib_btl->src_path_bits;
    attr.ah_attr.port_num      = openib_btl->port_num;
    attr.ah_attr.static_rate   = 0;
    attr.ah_attr.sl            = mca_btl_openib_component.ib_service_level;

#if (ENABLE_DYNAMIC_SL)
    /* if user enabled dynamic SL, get it from PathRecord */
    if (0 != mca_btl_openib_component.ib_path_record_service_level) {
        int rc = btl_openib_connect_get_pathrecord_sl(
                                openib_btl->device->xrc_domain->context,
                                attr.ah_attr.port_num,
                                openib_btl->lid,
                                attr.ah_attr.dlid);
        if (OMPI_ERROR == rc) {
            return OMPI_ERROR;
        }
        attr.ah_attr.sl = rc;
    }
#endif

    ret = ibv_modify_xrc_rcv_qp(openib_btl->device->xrc_domain,
            endpoint->xrc_recv_qp_num,
            &attr,
            IBV_QP_STATE|
            IBV_QP_AV|
            IBV_QP_PATH_MTU|
            IBV_QP_DEST_QPN|
            IBV_QP_RQ_PSN|
            IBV_QP_MAX_DEST_RD_ATOMIC|
            IBV_QP_MIN_RNR_TIMER);
    if (ret) {
        BTL_ERROR(("Error modifying XRC recv QP[%x] to IBV_QPS_RTR, errno says: %s [%d]",
                    endpoint->xrc_recv_qp_num, strerror(ret), ret));
        return OMPI_ERROR;
    }
#if OPAL_HAVE_THREADS
    if (APM_ENABLED) {
        mca_btl_openib_load_apm_xrc_rcv(endpoint->xrc_recv_qp_num, endpoint);
    }
#endif

    return OMPI_SUCCESS;
}

/* Recv qp connect */
static int xoob_recv_qp_connect(mca_btl_openib_endpoint_t *endpoint, mca_btl_openib_rem_info_t *rem_info)
{
    int ret;

    mca_btl_openib_module_t* openib_btl =
        (mca_btl_openib_module_t*)endpoint->endpoint_btl;

    BTL_VERBOSE(("Connecting Recv QP\n"));
    ret = ibv_reg_xrc_rcv_qp(openib_btl->device->xrc_domain, rem_info->rem_qps->rem_qp_num);
    if (ret) { /* failed to regester the qp, so it is already die and we should create new one */
       /* Return NOT READY !!!*/
        BTL_ERROR(("Failed to register qp_num: %d , get error: %s (%d)\n. Replying with RNR",
                    rem_info->rem_qps->rem_qp_num, strerror(ret), ret));
        return OMPI_ERROR;
    } else {
        /* save the qp number for unregister */
        endpoint->xrc_recv_qp_num = rem_info->rem_qps->rem_qp_num;
        return OMPI_SUCCESS;
    }
}

/*
 * Reply to a `start - connect' message
 */
static int xoob_reply_first_connect(mca_btl_openib_endpoint_t *endpoint,
                               mca_btl_openib_rem_info_t *rem_info)
{
    int rc;

    BTL_VERBOSE(("Initialized QPs, LID = %d",
                 ((mca_btl_openib_module_t*)endpoint->endpoint_btl)->lid));

    /* Create local QP's and post receive resources */
    if (OMPI_SUCCESS != (rc = xoob_recv_qp_create(endpoint, rem_info))) {
        return rc;
    }

    if (OMPI_SUCCESS !=
        (rc = xoob_send_connect_data(endpoint, ENDPOINT_XOOB_CONNECT_RESPONSE))) {
        BTL_ERROR(("error in endpoint send connect request error code is %d",
                   rc));
        return rc;
    }
    return OMPI_SUCCESS;
}

/* Find endpoint for specific subnet/lid/message */
static mca_btl_openib_endpoint_t* xoob_find_endpoint(orte_process_name_t* process_name,
        uint64_t subnet_id, uint16_t lid, uint8_t message_type)
{
    size_t i;
    mca_btl_openib_proc_t *ib_proc;
    mca_btl_openib_endpoint_t *ib_endpoint = NULL;
    bool found = false;

    BTL_VERBOSE(("Searching for ep and proc with follow parameters:"
                "jobid %d, vpid %d, sid %" PRIx64 ", lid %d",
                process_name->jobid, process_name->vpid, subnet_id, lid));
    /* find ibproc */
    OPAL_THREAD_LOCK(&mca_btl_openib_component.ib_lock);
    for (ib_proc = (mca_btl_openib_proc_t*)
            opal_list_get_first(&mca_btl_openib_component.ib_procs);
            ib_proc != (mca_btl_openib_proc_t*)
            opal_list_get_end(&mca_btl_openib_component.ib_procs);
            ib_proc  = (mca_btl_openib_proc_t*)opal_list_get_next(ib_proc)) {
        if (orte_util_compare_name_fields(ORTE_NS_CMP_ALL,
                    &ib_proc->proc_guid, process_name) == OPAL_EQUAL) {
            found = true;
            break;
        }
    }
    /* we found our ib_proc, lets find endpoint now */
    if (found) {
        for (i = 0; i < ib_proc->proc_endpoint_count; i++) {
            ib_endpoint = ib_proc->proc_endpoints[i];
            /* we need to check different
             * lid for different message type */
            if (ENDPOINT_XOOB_CONNECT_RESPONSE == message_type ||
                    ENDPOINT_XOOB_CONNECT_XRC_RESPONSE == message_type) {
                /* response message */
                if (ib_endpoint->subnet_id == subnet_id &&
                        ib_endpoint->ib_addr->lid == lid) {
                    break; /* Found one */
                }
            } else {
                /* request message */
                if (ib_endpoint->subnet_id == subnet_id &&
                        ib_endpoint->endpoint_btl->lid == lid) {
                    break; /* Found one */
                }
            }
        }
        if (NULL == ib_endpoint) {
                BTL_ERROR(("can't find suitable endpoint for this peer\n"));
        }
    } else {
            BTL_ERROR(("can't find suitable endpoint for this peer\n"));
    }
    OPAL_THREAD_UNLOCK(&mca_btl_openib_component.ib_lock);
    return ib_endpoint;
}

/* In case if XRC recv qp was closed and sender still don't know about it
 * we need close the qp, reset the ib_adrr status to CLOSED and start everything
 * from scratch.
 */
static void xoob_restart_connect(mca_btl_base_endpoint_t *endpoint)
{
    BTL_VERBOSE(("Restarting the connection for the endpoint"));
    OPAL_THREAD_LOCK(&endpoint->ib_addr->addr_lock);
    switch (endpoint->ib_addr->status) {
        case MCA_BTL_IB_ADDR_CONNECTED:
            /* so we have the send qp, we just need the recive site.
             * Send request for SRQ numbers */
            BTL_VERBOSE(("Restart The IB addr: sid %" PRIx64 " lid %d"
                         "in MCA_BTL_IB_ADDR_CONNECTED status,"
                         " Changing to MCA_BTL_IB_ADDR_CLOSED and starting from scratch\n",
                         endpoint->ib_addr->subnet_id,endpoint->ib_addr->lid));
            /* Switching back to closed and starting from scratch */
            endpoint->ib_addr->status = MCA_BTL_IB_ADDR_CLOSED;
            /* destroy the qp */
            /* the reciver site was alredy closed so all pending list must be clean ! */
            assert (opal_list_is_empty(&endpoint->qps->no_wqe_pending_frags[0]));
            assert (opal_list_is_empty(&endpoint->qps->no_wqe_pending_frags[1]));
            if(ibv_destroy_qp(endpoint->qps[0].qp->lcl_qp))
                BTL_ERROR(("Failed to destroy QP"));
        case MCA_BTL_IB_ADDR_CLOSED:
        case MCA_BTL_IB_ADDR_CONNECTING:
            BTL_VERBOSE(("Restart The IB addr: sid %" PRIx64 " lid %d"
                         "in MCA_BTL_IB_ADDR_CONNECTING or MCA_BTL_IB_ADDR_CLOSED status,"
                         " starting from scratch\n",
                         endpoint->ib_addr->subnet_id,endpoint->ib_addr->lid));
            OPAL_THREAD_UNLOCK(&endpoint->ib_addr->addr_lock);
            /* xoob_module_start_connect() should automaticly handle all other cases */
            if (OMPI_SUCCESS != xoob_module_start_connect(NULL, endpoint))
                BTL_ERROR(("Failed to restart connection from MCA_BTL_IB_ADDR_CONNECTING/CLOSED"));
            break;
        default :
            BTL_ERROR(("Invalid endpoint status %d", endpoint->ib_addr->status));
            OPAL_THREAD_UNLOCK(&endpoint->ib_addr->addr_lock);
    }
}

/* Init remote information structs */
static int init_rem_info(mca_btl_openib_rem_info_t *rem_info)
{
    rem_info->rem_qps = (mca_btl_openib_rem_qp_info_t*)malloc(sizeof(mca_btl_openib_rem_qp_info_t));
    if (NULL == rem_info->rem_qps) {
        BTL_ERROR(("Failed to allocate memory for remote QP data\n"));
        return OMPI_ERROR;
    }
    rem_info->rem_srqs = (mca_btl_openib_rem_srq_info_t*)malloc(sizeof(mca_btl_openib_rem_srq_info_t) *
            mca_btl_openib_component.num_xrc_qps);
    if (NULL == rem_info->rem_srqs) {
        BTL_ERROR(("Failed to allocate memory for remote SRQ data\n"));
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}

/* Free remote information structs */
static void free_rem_info(mca_btl_openib_rem_info_t *rem_info)
{
    if (NULL != rem_info->rem_qps) {
        free(rem_info->rem_qps);
    }
    if (NULL != rem_info->rem_srqs) {
        free(rem_info->rem_srqs);
    }
}

/*
 * Non blocking RML recv callback.  Read incoming QP and other info,
 * and if this endpoint is trying to connect, reply with our QP info,
 * otherwise try to modify QP's and establish reliable connection
 */
static void xoob_rml_recv_cb(int status, orte_process_name_t* process_name,
                        opal_buffer_t* buffer, orte_rml_tag_t tag,
                        void* cbdata)
{
    int rc;
    uint8_t message_type;
    uint16_t requested_lid = 0;
    mca_btl_openib_rem_info_t rem_info;
    mca_btl_openib_endpoint_t *ib_endpoint = NULL;

    if ( OMPI_SUCCESS != init_rem_info(&rem_info)) {
        return;
    }

    /* Get data. */
    if ( OMPI_SUCCESS != xoob_receive_connect_data(&rem_info, &requested_lid, &message_type, buffer)) {
        BTL_ERROR(("Failed to read data\n"));
        mca_btl_openib_endpoint_invoke_error(NULL);
        return;
    }

    /* Processing message */
    switch (message_type) {
        case ENDPOINT_XOOB_CONNECT_REQUEST:
            BTL_VERBOSE(("Received ENDPOINT_XOOB_CONNECT_REQUEST: lid %d, sid %" PRIx64 ", rlid %d\n",
                        rem_info.rem_lid,
                        rem_info.rem_subnet_id,
                        requested_lid));
            ib_endpoint = xoob_find_endpoint(process_name,rem_info.rem_subnet_id,
                    requested_lid, message_type);
            if ( NULL == ib_endpoint) {
                BTL_ERROR(("Got ENDPOINT_XOOB_CONNECT_REQUEST."
                           " Failed to find endpoint with subnet %" PRIx64 
                           " and LID %d",
                           rem_info.rem_subnet_id,requested_lid));
                mca_btl_openib_endpoint_invoke_error(NULL);
                return;
            }
            OPAL_THREAD_LOCK(&ib_endpoint->endpoint_lock);
            /* prepost data on receiver site */
            if (OMPI_SUCCESS != mca_btl_openib_endpoint_post_recvs(ib_endpoint)) {
                BTL_ERROR(("Failed to post on XRC SRQs"));
                mca_btl_openib_endpoint_invoke_error(NULL);
                OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
                return;
            }
            /* we should create qp and send the info + srq to requestor */
            rc = xoob_reply_first_connect(ib_endpoint, &rem_info);
            if (OMPI_SUCCESS != rc) {
                BTL_ERROR(("error in endpoint reply start connect"));
                mca_btl_openib_endpoint_invoke_error(NULL);
                OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
                return;
            }
            /* enable pooling for this btl */
            OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
            break;
        case ENDPOINT_XOOB_CONNECT_XRC_REQUEST:
            /* pasha we don't need the remote lid here ??*/
            BTL_VERBOSE(("Received ENDPOINT_XOOB_CONNECT_XRC_REQUEST: lid %d, sid %" PRIx64 "\n",
                        rem_info.rem_lid,
                        rem_info.rem_subnet_id));
            ib_endpoint = xoob_find_endpoint(process_name,rem_info.rem_subnet_id,
                    requested_lid, message_type);
            if ( NULL == ib_endpoint) {
                BTL_ERROR(("Got ENDPOINT_XOOB_CONNECT_XRC_REQUEST."
                            " Failed to find endpoint with subnet %" PRIx64 " and LID %d",
                            rem_info.rem_subnet_id,requested_lid));
                mca_btl_openib_endpoint_invoke_error(NULL);
                return;
            }
            if (OMPI_SUCCESS == xoob_recv_qp_connect(ib_endpoint, &rem_info)) {
                if (OMPI_SUCCESS != mca_btl_openib_endpoint_post_recvs(ib_endpoint)) {
                    BTL_ERROR(("Failed to post on XRC SRQs"));
                    mca_btl_openib_endpoint_invoke_error(ib_endpoint);
                    return;
                }
                OPAL_THREAD_LOCK(&ib_endpoint->endpoint_lock);
                rc = xoob_send_connect_data(ib_endpoint, ENDPOINT_XOOB_CONNECT_XRC_RESPONSE);
                if (OMPI_SUCCESS != rc) {
                    BTL_ERROR(("error in endpoint reply start connect"));
                    mca_btl_openib_endpoint_invoke_error(ib_endpoint);
                    OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
                    return;
                }
                OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
            } else {
                /* The XRC recv qp was destroyed */
                OPAL_THREAD_LOCK(&ib_endpoint->endpoint_lock);
                rc = xoob_send_connect_data(ib_endpoint, ENDPOINT_XOOB_CONNECT_XRC_NR_RESPONSE);
                if (OMPI_SUCCESS != rc) {
                    BTL_ERROR(("error in endpoint reply start connect"));
                    mca_btl_openib_endpoint_invoke_error(ib_endpoint);
                    OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
                    return;
                }
                OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
            }
            /* enable pooling for this btl */
            break;
        case ENDPOINT_XOOB_CONNECT_RESPONSE:
            BTL_VERBOSE(("Received ENDPOINT_XOOB_CONNECT_RESPONSE: lid %d, sid %" PRIx64 "\n",
                        rem_info.rem_lid,
                        rem_info.rem_subnet_id));
            ib_endpoint = xoob_find_endpoint(process_name, rem_info.rem_subnet_id,
                    rem_info.rem_lid, message_type);
            if ( NULL == ib_endpoint) {
                BTL_ERROR(("Got ENDPOINT_XOOB_CONNECT_RESPONSE."
                            " Failed to find endpoint with subnet %" PRIx64 " and LID %d",
                            rem_info.rem_subnet_id,rem_info.rem_lid));
                mca_btl_openib_endpoint_invoke_error(NULL);
                return;
            }
            OPAL_THREAD_LOCK(&ib_endpoint->endpoint_lock);
            /* we got all the data srq. switch the endpoint to connect mode */
            XOOB_SET_REMOTE_INFO(ib_endpoint->rem_info, rem_info);
            /* update ib_addr with remote qp number */
            ib_endpoint->ib_addr->remote_xrc_rcv_qp_num =
                ib_endpoint->rem_info.rem_qps->rem_qp_num;
            BTL_VERBOSE(("rem_info: lid %d, sid %" PRIx64 
                         " ep %d %" PRIx64 "\n",
                         rem_info.rem_lid,
                         rem_info.rem_subnet_id,
                         ib_endpoint->rem_info.rem_lid,
                         ib_endpoint->rem_info.rem_subnet_id));
            if (OMPI_SUCCESS != xoob_send_qp_connect(ib_endpoint, &rem_info)) {
                BTL_ERROR(("Failed to connect  endpoint\n"));
                mca_btl_openib_endpoint_invoke_error(NULL);
                OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
                return;
            }
            mca_btl_openib_endpoint_cpc_complete(ib_endpoint);
            /* cpc complete unlock the endpoint */
            break;
        case ENDPOINT_XOOB_CONNECT_XRC_RESPONSE:
            BTL_VERBOSE(("Received ENDPOINT_XOOB_CONNECT_XRC_RESPONSE: lid %d, sid %" PRIx64 "\n",
                        rem_info.rem_lid,
                        rem_info.rem_subnet_id));
            ib_endpoint = xoob_find_endpoint(process_name, rem_info.rem_subnet_id,
                    rem_info.rem_lid, message_type);
            if ( NULL == ib_endpoint) {
                BTL_ERROR(("Got ENDPOINT_XOOB_CONNECT_XRC_RESPONSE."
                            " Failed to find endpoint with subnet %" PRIx64 " and LID %d",
                            rem_info.rem_subnet_id,rem_info.rem_lid));
                mca_btl_openib_endpoint_invoke_error(NULL);
                return;
            }
            OPAL_THREAD_LOCK(&ib_endpoint->endpoint_lock);
            /* we got srq numbers on our request */
            XOOB_SET_REMOTE_INFO(ib_endpoint->rem_info, rem_info);
            mca_btl_openib_endpoint_cpc_complete(ib_endpoint);
            /* cpc complete unlock the endpoint */
            break;
        case ENDPOINT_XOOB_CONNECT_XRC_NR_RESPONSE:
            /* The XRC recv site already was destroyed so we need
             * start to bringup the connection from scratch  */
            BTL_VERBOSE(("Received ENDPOINT_XOOB_CONNECT_XRC_NR_RESPONSE: lid %d, sid %" PRIx64 "\n",
                        rem_info.rem_lid,
                        rem_info.rem_subnet_id));
            ib_endpoint = xoob_find_endpoint(process_name, rem_info.rem_subnet_id,
                    rem_info.rem_lid, message_type);
            if ( NULL == ib_endpoint) {
                BTL_ERROR(("Got ENDPOINT_XOOB_CONNECT_XRC_NR_RESPONSE."
                            " Failed to find endpoint with subnet %" PRIx64 " and LID %d",
                            rem_info.rem_subnet_id,rem_info.rem_lid));
                mca_btl_openib_endpoint_invoke_error(NULL);
                return;
            }
            xoob_restart_connect(ib_endpoint);
            break;
        default :
            BTL_ERROR(("Invalid message type %d", message_type));
    }

    free_rem_info(&rem_info);
}

/*
 * XOOB interface functions
 */

/* Quere for the XOOB priority - will be highest in XRC case */
static int xoob_component_query(mca_btl_openib_module_t *openib_btl, 
        ompi_btl_openib_connect_base_module_t **cpc)
{
    int rc;

    if (mca_btl_openib_component.num_xrc_qps <= 0) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: xoob CPC only supported with XRC receive queues; skipped on %s:%d",
                            ibv_get_device_name(openib_btl->device->ib_dev),
                            openib_btl->port_num);
        return OMPI_ERR_NOT_SUPPORTED;
    }

    *cpc = malloc(sizeof(ompi_btl_openib_connect_base_module_t));
    if (NULL == *cpc) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: xoob CPC system error (malloc failed)");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* If this btl supports XOOB, then post the RML message.  But
       ensure to only post it *once*, because another btl may have
       come in before this and already posted it. */
    if (!rml_recv_posted) {
        rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, 
                                     OMPI_RML_TAG_XOPENIB,
                                     ORTE_RML_PERSISTENT,
                                     xoob_rml_recv_cb,
                                     NULL);
        if (ORTE_SUCCESS != rc) {
            opal_output_verbose(5, mca_btl_base_output,
                                "openib BTL: xoob CPC system error %d (%s)",
                                rc, opal_strerror(rc));
            return rc;
        }
        rml_recv_posted = true;
    }
        
    (*cpc)->data.cbm_component = &ompi_btl_openib_connect_xoob;
    (*cpc)->data.cbm_priority = xoob_priority;
    (*cpc)->data.cbm_modex_message = NULL;
    (*cpc)->data.cbm_modex_message_len = 0;

    (*cpc)->cbm_endpoint_init = NULL;
    (*cpc)->cbm_start_connect = xoob_module_start_connect;
    (*cpc)->cbm_endpoint_finalize = NULL;
    (*cpc)->cbm_finalize = NULL;
    (*cpc)->cbm_uses_cts = false;

    opal_output_verbose(5, mca_btl_base_output,
                        "openib BTL: xoob CPC available for use on %s:%d",
                        ibv_get_device_name(openib_btl->device->ib_dev),
                        openib_btl->port_num);
    return OMPI_SUCCESS;
}

/* Open - this functions sets up any xoob specific commandline params */
static void xoob_component_register(void)
{
    mca_base_param_reg_int(&mca_btl_openib_component.super.btl_version,
                           "connect_xoob_priority",
                           "The selection method priority for xoob",
                           false, false, xoob_priority, &xoob_priority);

    if (xoob_priority > 100) {
        xoob_priority = 100;
    } else if (xoob_priority < -1) {
        xoob_priority = -1;
    }
}

/*
 * Connect function.  Start initiation of connections to a remote
 * peer.  We send our Queue Pair information over the RML/OOB
 * communication mechanism.  On completion of our send, a send
 * completion handler is called.
 */
static int xoob_module_start_connect(ompi_btl_openib_connect_base_module_t *cpc,
                                     mca_btl_base_endpoint_t *endpoint)
{
    int rc = OMPI_SUCCESS;

    OPAL_THREAD_LOCK(&endpoint->ib_addr->addr_lock);
    switch (endpoint->ib_addr->status) {
        case MCA_BTL_IB_ADDR_CLOSED:
            BTL_VERBOSE(("The IB addr: sid %" PRIx64 " lid %d"
                        "in MCA_BTL_IB_ADDR_CLOSED status,"
                        " sending ENDPOINT_XOOB_CONNECT_REQUEST\n",
                        endpoint->ib_addr->subnet_id,endpoint->ib_addr->lid));
            if (OMPI_SUCCESS != (rc = xoob_send_qp_create(endpoint))) {
                break;
            }

            /* Send connection info over to remote endpoint */
            endpoint->endpoint_state = MCA_BTL_IB_CONNECTING;
            endpoint->ib_addr->status = MCA_BTL_IB_ADDR_CONNECTING;
            if (OMPI_SUCCESS !=
                    (rc = xoob_send_connect_data(endpoint, ENDPOINT_XOOB_CONNECT_REQUEST))) {
                BTL_ERROR(("Error sending connect request, error code %d", rc));
            }
            break;
        case MCA_BTL_IB_ADDR_CONNECTING:
            BTL_VERBOSE(("The IB addr: sid %" PRIx64 " lid %d"
                        "in MCA_BTL_IB_ADDR_CONNECTING status,"
                        " Subscribing to this address\n",
                        endpoint->ib_addr->subnet_id,endpoint->ib_addr->lid));
            /* some body already connectng to this machine, lets wait */
            opal_list_append(&endpoint->ib_addr->pending_ep, &(endpoint->super));
            endpoint->endpoint_state = MCA_BTL_IB_CONNECTING;
            break;
        case MCA_BTL_IB_ADDR_CONNECTED:
            /* so we have the send qp, we just need the recive site.
             * Send request for SRQ numbers */
            BTL_VERBOSE(("The IB addr: sid %" PRIx64 " lid %d"
                        "in MCA_BTL_IB_ADDR_CONNECTED status,"
                        " sending ENDPOINT_XOOB_CONNECT_XRC_REQUEST\n",
                        endpoint->ib_addr->subnet_id,endpoint->ib_addr->lid));
            endpoint->endpoint_state = MCA_BTL_IB_CONNECTING;
            if (OMPI_SUCCESS !=
                    (rc = xoob_send_connect_data(endpoint, ENDPOINT_XOOB_CONNECT_XRC_REQUEST))) {
                BTL_ERROR(("error sending xrc connect request, error code %d", rc));
            }
            break;
        default :
            BTL_ERROR(("Invalid endpoint status %d", endpoint->ib_addr->status));
    }
    OPAL_THREAD_UNLOCK(&endpoint->ib_addr->addr_lock);
    return rc;
}


/*
 * Finalize function.  Cleanup RML non-blocking receive.
 */
static int xoob_component_finalize(void)
{
    if (rml_recv_posted) {
        orte_rml.recv_cancel(ORTE_NAME_WILDCARD, OMPI_RML_TAG_XOPENIB);
        rml_recv_posted = false;
    }
#if (ENABLE_DYNAMIC_SL)
    btl_openib_connect_sl_finalize();
#endif
    return OMPI_SUCCESS;
}
