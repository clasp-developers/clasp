/*
 * Copyright (c) 2011      Mellanox Technologies.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_openib.h"
#include "orte/util/show_help.h"

#include "connect/btl_openib_connect_sl.h"
#include <infiniband/iba/ib_types.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#define SL_NOT_PRESENT                0xFF
#define MAX_GET_SL_REC_RETRIES        20
#define GET_SL_REC_RETRIES_TIMEOUT_MS 2000000

static struct mca_btl_openib_sa_qp_cache {
    /* There will be a MR with the one send and receive buffer together */
    /* The send buffer is first, the receive buffer is second */
    /* The receive buffer in a UD queue pair needs room for the 40 byte GRH */
    /* The buffers are first in the structure for page alignment */
    char     send_recv_buffer[MAD_BLOCK_SIZE * 2 + 40];
    struct   mca_btl_openib_sa_qp_cache *next;
    struct   ibv_context *context;
    char     *device_name;
    uint32_t port_num;
    struct   ibv_qp *qp;
    struct   ibv_ah *ah;
    struct   ibv_cq *cq;
    struct   ibv_mr *mr;
    struct   ibv_pd *pd;
    struct   ibv_recv_wr rwr;
    struct   ibv_sge rsge;
    uint8_t  sl_values[65536]; /* 64K */
} *sa_qp_cache = 0;

static int init_ud_qp(
                struct ibv_context *context_arg,
                struct mca_btl_openib_sa_qp_cache *cache);

static void init_sa_mad(
                struct mca_btl_openib_sa_qp_cache *cache,
                ib_sa_mad_t *sa_mad,
                struct ibv_send_wr *swr,
                struct ibv_sge *ssge,
                uint16_t lid,
                uint16_t rem_lid);

static int get_pathrecord_info(
                struct mca_btl_openib_sa_qp_cache *cache,
                ib_sa_mad_t *sa_mad,
                ib_sa_mad_t *sar,
                struct ibv_send_wr *swr,
                uint16_t lid,
                uint16_t rem_lid);

static int init_device(
                struct ibv_context *context_arg,
                struct mca_btl_openib_sa_qp_cache *cache,
                uint32_t port_num);

/*=================================================================*/

static void free_sa_qp_cache(void)
{
    struct mca_btl_openib_sa_qp_cache *cache, *tmp;

    cache = sa_qp_cache;
    while (NULL != cache) {
        /* free cache data */
        if (cache->device_name)
            free(cache->device_name);
        if (NULL != cache->qp)
            ibv_destroy_qp(cache->qp);
        if (NULL != cache->ah)
            ibv_destroy_ah(cache->ah);
        if (NULL != cache->cq)
            ibv_destroy_cq(cache->cq);
        if (NULL != cache->mr)
            ibv_dereg_mr(cache->mr);
        if (NULL != cache->pd)
            ibv_dealloc_pd(cache->pd);
        tmp = cache->next;
        free(cache);
        cache = tmp;
    }
    sa_qp_cache = NULL;
}

/*=================================================================*/

static int init_ud_qp(struct ibv_context *context_arg,
                      struct mca_btl_openib_sa_qp_cache *cache)
{
    struct ibv_qp_init_attr iattr;
    struct ibv_qp_attr mattr;
    int rc;

    /* create cq */
    cache->cq = ibv_create_cq(cache->context, 4, NULL, NULL, 0);
    if (NULL == cache->cq) {
        BTL_ERROR(("error creating cq, errno says %s", strerror(errno)));
        orte_show_help("help-mpi-btl-openib.txt", "init-fail-create-q",
                true, orte_process_info.nodename,
                __FILE__, __LINE__, "ibv_create_cq",
                strerror(errno), errno,
                ibv_get_device_name(context_arg->device));
        return OMPI_ERROR;
    }

    /* create qp */
    memset(&iattr, 0, sizeof(iattr));
    iattr.send_cq = cache->cq;
    iattr.recv_cq = cache->cq;
    iattr.cap.max_send_wr = 2;
    iattr.cap.max_recv_wr = 2;
    iattr.cap.max_send_sge = 1;
    iattr.cap.max_recv_sge = 1;
    iattr.qp_type = IBV_QPT_UD;
    cache->qp = ibv_create_qp(cache->pd, &iattr);
    if (NULL == cache->qp) {
        BTL_ERROR(("error creating qp %s (%d)", strerror(errno), errno));
        return OMPI_ERROR;
    }

    /* modify qp to IBV_QPS_INIT */
    memset(&mattr, 0, sizeof(mattr));
    mattr.qp_state = IBV_QPS_INIT;
    mattr.port_num = cache->port_num;
    mattr.qkey = ntohl(IB_QP1_WELL_KNOWN_Q_KEY);
    rc = ibv_modify_qp(cache->qp, &mattr,
            IBV_QP_STATE              |
            IBV_QP_PKEY_INDEX         |
            IBV_QP_PORT               |
            IBV_QP_QKEY);
    if (rc) {
        BTL_ERROR(("Error modifying QP[%x] to IBV_QPS_INIT errno says: %s [%d]",
                    cache->qp->qp_num, strerror(errno), errno));
        return OMPI_ERROR;
    }

    /* modify qp to IBV_QPS_RTR */
    memset(&mattr, 0, sizeof(mattr));
    mattr.qp_state = IBV_QPS_RTR;
    rc = ibv_modify_qp(cache->qp, &mattr, IBV_QP_STATE);
    if (rc) {
        BTL_ERROR(("Error modifying QP[%x] to IBV_QPS_RTR errno says: %s [%d]",
                    cache->qp->qp_num, strerror(errno), errno));
        return OMPI_ERROR;
    }

    /* modify qp to IBV_QPS_RTS */
    mattr.qp_state = IBV_QPS_RTS;
    rc = ibv_modify_qp(cache->qp, &mattr, IBV_QP_STATE | IBV_QP_SQ_PSN);
    if (rc) {
        BTL_ERROR(("Error modifying QP[%x] to IBV_QPS_RTR errno says: %s [%d]",
                    cache->qp->qp_num, strerror(errno), errno));
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

/*=================================================================*/

static void init_sa_mad(struct mca_btl_openib_sa_qp_cache *cache,
                       ib_sa_mad_t *sa_mad,
                       struct ibv_send_wr *swr,
                       struct ibv_sge *ssge,
                       uint16_t lid,
                       uint16_t rem_lid)
{
    ib_path_rec_t *path_record = (ib_path_rec_t*)sa_mad->data;

    memset(swr, 0, sizeof(*swr));
    memset(ssge, 0, sizeof(*ssge));

    /* Initialize the standard MAD header. */
    memset(sa_mad, 0, MAD_BLOCK_SIZE);
    ib_mad_init_new((ib_mad_t *)sa_mad,          /* mad header pointer */
                    IB_MCLASS_SUBN_ADM,          /* management class */
                    (uint8_t) 2,                 /* version */
                    IB_MAD_METHOD_GET,           /* method */
                    hton64((uint64_t)lid << 48 | /* transaction ID */
                           (uint64_t)rem_lid << 32 |
                           (uint64_t)cache->qp->qp_num << 8),
                    IB_MAD_ATTR_PATH_RECORD,     /* attribute ID */
                    0);                          /* attribute modifier */

    sa_mad->comp_mask = IB_PR_COMPMASK_DLID | IB_PR_COMPMASK_SLID;
    path_record->dlid = htons(rem_lid);
    path_record->slid = htons(lid);

    swr->sg_list = ssge;
    swr->num_sge = 1;
    swr->opcode = IBV_WR_SEND;
    swr->wr.ud.ah = cache->ah;
    swr->wr.ud.remote_qpn = ntohl(IB_QP1);
    swr->wr.ud.remote_qkey = ntohl(IB_QP1_WELL_KNOWN_Q_KEY);
    swr->send_flags = IBV_SEND_SIGNALED | IBV_SEND_SOLICITED;

    ssge->addr = (uint64_t)(void *)sa_mad;
    ssge->length = MAD_BLOCK_SIZE;
    ssge->lkey = cache->mr->lkey;
}

/*=================================================================*/

static int get_pathrecord_info(struct mca_btl_openib_sa_qp_cache *cache,
                             ib_sa_mad_t *req_mad,
                             ib_sa_mad_t *resp_mad,
                             struct ibv_send_wr *swr,
                             uint16_t lid,
                             uint16_t rem_lid)
{
    struct ibv_send_wr *bswr;
    struct ibv_wc wc;
    struct timeval get_sl_rec_last_sent, get_sl_rec_last_poll;
    struct ibv_recv_wr *brwr;
    int got_sl_value, get_sl_rec_retries, rc, ne, i;
    ib_path_rec_t *req_path_record = ib_sa_mad_get_payload_ptr(req_mad);
    ib_path_rec_t *resp_path_record = ib_sa_mad_get_payload_ptr(resp_mad);

    got_sl_value = 0;
    get_sl_rec_retries = 0;

    rc = ibv_post_recv(cache->qp, &(cache->rwr), &brwr);
    if (0 != rc) {
        BTL_ERROR(("error posting receive on QP [0x%x] errno says: %s [%d]",
                   cache->qp->qp_num, strerror(errno), errno));
        return OMPI_ERROR;
    }

    while (0 == got_sl_value) {
        rc = ibv_post_send(cache->qp, swr, &bswr);
        if (0 != rc) {
            BTL_ERROR(("error posting send on QP [0x%x] errno says: %s [%d]",
                       cache->qp->qp_num, strerror(errno), errno));
            return OMPI_ERROR;
        }
        gettimeofday(&get_sl_rec_last_sent, NULL);

        while (0 == got_sl_value) {
            ne = ibv_poll_cq(cache->cq, 1, &wc);
            if (ne > 0 &&
                IBV_WC_SUCCESS == wc.status &&
                IBV_WC_RECV == wc.opcode &&
                wc.byte_len >= MAD_BLOCK_SIZE &&
                resp_mad->trans_id == req_mad->trans_id) {
                if (0 == resp_mad->status &&
                    req_path_record->slid == htons(lid) &&
                    req_path_record->dlid == htons(rem_lid)) {
                    /* Everything matches, so we have the desired SL */
                    cache->sl_values[rem_lid] = ib_path_rec_sl(resp_path_record);
                    got_sl_value = 1; /* still must repost recieve buf */
                } else {
                    /* Probably bad status, unlikely bad lid match. We will */
                    /* ignore response and let it time out so that we do a  */
                    /* retry, but after a delay. We must make a new TID so  */
                    /* the SM doesn't see it as the same request.           */
                    req_mad->trans_id += hton64(1);
                }
                rc = ibv_post_recv(cache->qp, &(cache->rwr), &brwr);
                if (0 != rc) {
                    BTL_ERROR(("error posing receive on QP[%x] errno says: %s [%d]",
                               cache->qp->qp_num, strerror(errno), errno));
                    return OMPI_ERROR;
                }
            } else if (0 == ne) {    /* poll did not find anything */
                gettimeofday(&get_sl_rec_last_poll, NULL);
                i = get_sl_rec_last_poll.tv_sec - get_sl_rec_last_sent.tv_sec;
                i = (i * 1000000) +
                    get_sl_rec_last_poll.tv_usec - get_sl_rec_last_sent.tv_usec;
                if (i > GET_SL_REC_RETRIES_TIMEOUT_MS) {
                    get_sl_rec_retries++;
                    BTL_VERBOSE(("[%d/%d] retries to get PathRecord",
                            get_sl_rec_retries, MAX_GET_SL_REC_RETRIES));
                    if (get_sl_rec_retries > MAX_GET_SL_REC_RETRIES) {
                        BTL_ERROR(("No response from SA after %d retries",
                                MAX_GET_SL_REC_RETRIES));
                        return OMPI_ERROR;
                    }
                    break;  /* retransmit request */
                }
                usleep(100);  /* otherwise pause before polling again */
            } else if (ne < 0) {
                BTL_ERROR(("error polling CQ with %d: %s\n",
                    ne, strerror(errno)));
                return OMPI_ERROR;
            }
        }
    }
    return 0;
}

/*=================================================================*/

static int init_device(struct ibv_context *context_arg,
                       struct mca_btl_openib_sa_qp_cache *cache,
                       uint32_t port_num)
{
    struct ibv_ah_attr aattr;
    struct ibv_port_attr pattr;
    int rc;

    cache->context = ibv_open_device(context_arg->device);
    if (NULL == cache->context) {
        BTL_ERROR(("error obtaining device context for %s errno says %s",
                    ibv_get_device_name(context_arg->device), strerror(errno)));
        return OMPI_ERROR;
    }
    cache->device_name = strdup(ibv_get_device_name(cache->context->device));
    cache->port_num = port_num;

    /* init all sl_values to be SL_NOT_PRESENT */
    memset(&cache->sl_values, SL_NOT_PRESENT, sizeof(cache->sl_values));

    cache->next = sa_qp_cache;
    sa_qp_cache = cache;

    /* allocate the protection domain for the device */
    cache->pd = ibv_alloc_pd(cache->context);
    if (NULL == cache->pd) {
        BTL_ERROR(("error allocating protection domain for %s errno says %s",
                    ibv_get_device_name(context_arg->device), strerror(errno)));
        return OMPI_ERROR;
    }

    /* register memory region */
    cache->mr = ibv_reg_mr(cache->pd, cache->send_recv_buffer,
            sizeof(cache->send_recv_buffer),
            IBV_ACCESS_REMOTE_WRITE | IBV_ACCESS_LOCAL_WRITE);
    if (NULL == cache->mr) {
        BTL_ERROR(("error registering memory region, errno says %s", strerror(errno)));
        return OMPI_ERROR;
    }

    /* init the ud qp */
    rc = init_ud_qp(context_arg, cache);
    if (OMPI_ERROR == rc) {
        return OMPI_ERROR;
    }

    rc = ibv_query_port(cache->context, cache->port_num, &pattr);
    if (rc) {
        BTL_ERROR(("error getting port attributes for device %s "
                    "port number %d errno says %s",
                    ibv_get_device_name(context_arg->device),
                    cache->port_num, strerror(errno)));
        return OMPI_ERROR;
    }

    /* create address handle  */
    memset(&aattr, 0, sizeof(aattr));
    aattr.dlid = pattr.sm_lid;
    aattr.sl = pattr.sm_sl;
    aattr.port_num = cache->port_num;
    cache->ah = ibv_create_ah(cache->pd, &aattr);
    if (NULL == cache->ah) {
        BTL_ERROR(("error creating address handle: %s", strerror(errno)));
        return OMPI_ERROR;
    }

    memset(&(cache->rwr), 0, sizeof(cache->rwr));
    cache->rwr.num_sge = 1;
    cache->rwr.sg_list = &(cache->rsge);
    memset(&(cache->rsge), 0, sizeof(cache->rsge));
    cache->rsge.addr = (uint64_t)(void *)
        (cache->send_recv_buffer + MAD_BLOCK_SIZE);
    cache->rsge.length = MAD_BLOCK_SIZE + 40;
    cache->rsge.lkey = cache->mr->lkey;

    return 0;
}

/*=================================================================*/

static int get_pathrecord_sl(struct ibv_context *context_arg,
                      uint32_t port_num,
                      uint16_t lid,
                      uint16_t rem_lid)
{
    struct ibv_send_wr swr;
    ib_sa_mad_t *req_mad, *resp_mad;
    struct ibv_sge ssge;
    struct mca_btl_openib_sa_qp_cache *cache;
    long page_size = sysconf(_SC_PAGESIZE);
    int rc;

    /* search for a cached item */
    for (cache = sa_qp_cache; cache; cache = cache->next) {
        if (0 == strcmp(cache->device_name,
                    ibv_get_device_name(context_arg->device))
                && cache->port_num == port_num) {
            break;
        }
    }

    if (NULL == cache) {
        /* init new cache */
        if (posix_memalign((void **)(&cache), page_size,
                    sizeof(struct mca_btl_openib_sa_qp_cache))) {
            BTL_ERROR(("error in posix_memalign SA cache"));
            return OMPI_ERROR;
        }
        /* one time setup for each device/port combination */
        rc = init_device(context_arg, cache, port_num);
        if (0 != rc) {
            return rc;
        }
    }

    /* if the destination lid SL value is not in the cache, go get it */
    if (SL_NOT_PRESENT == cache->sl_values[rem_lid]) {
        /* sa_mad is first buffer, where we build the SA Get request to send */
        req_mad = (ib_sa_mad_t *)(cache->send_recv_buffer);

        init_sa_mad(cache, req_mad, &swr, &ssge, lid, rem_lid);

        /* resp_mad is the receive buffer (40 byte offset is for GRH) */
        resp_mad = (ib_sa_mad_t *)(cache->send_recv_buffer + MAD_BLOCK_SIZE + 40);

        rc = get_pathrecord_info(cache, req_mad, resp_mad, &swr, lid, rem_lid);
        if (0 != rc) {
            return rc;
        }
    }

    /* now all we do is send back the value laying around */
    return cache->sl_values[rem_lid];
}

/*=================================================================*/

int btl_openib_connect_get_pathrecord_sl(struct ibv_context *context_arg,
                                     uint32_t port_num,
                                     uint16_t lid,
                                     uint16_t rem_lid)
{
    int rc = get_pathrecord_sl(context_arg, port_num, lid, rem_lid);
    if (OMPI_ERROR == rc) {
            free_sa_qp_cache();
    }
    return rc;
}

/*=================================================================*/

void btl_openib_connect_sl_finalize()
{
        free_sa_qp_cache();
}
