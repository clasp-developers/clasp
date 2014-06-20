/*
 * Copyright (c) 2007-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007-2008 Chelsio, Inc. All rights reserved.
 * Copyright (c) 2008      Mellanox Technologies. All rights reserved.
 * Copyright (c) 2009      Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <rdma/rdma_cma.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <net/if.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <dirent.h>
#include <stddef.h>

#include "opal/util/output.h"
#include "opal/util/error.h"
#include "orte/util/show_help.h"

#include "btl_openib_fd.h"
#include "btl_openib_proc.h"
#include "btl_openib_endpoint.h"
#include "connect/connect.h"
#include "btl_openib_ip.h"
#include "btl_openib_ini.h"

/* JMS to be removed: see #1264 */
#undef event

#define mymin(a, b) ((a) < (b) ? (a) : (b))

static void rdmacm_component_register(void);
static int rdmacm_component_init(void);
static int rdmacm_component_query(mca_btl_openib_module_t *openib_btl,
                                  ompi_btl_openib_connect_base_module_t **cpc);
static int rdmacm_component_finalize(void);

ompi_btl_openib_connect_base_component_t ompi_btl_openib_connect_rdmacm = {
    "rdmacm",
    rdmacm_component_register,
    rdmacm_component_init,
    rdmacm_component_query,
    rdmacm_component_finalize
};

/*
 * A single instance of this data structure is shared between one
 * id_context_t for each BSRQ qp on an endpoint.
 */
typedef struct {
    opal_list_item_t super;
    mca_btl_openib_endpoint_t *endpoint;
    mca_btl_openib_module_t *openib_btl;
    /* Dummy QP only used when we expect the connection to be
       rejected */
    struct ibv_cq *dummy_cq;
    uint32_t ipaddr;
    uint16_t tcp_port;
    /* server==false means that this proc initiated the connection;
       server==true means that this proc accepted the incoming
       connection.  Note that this may be different than the "one way"
       / i_initiate() direction -- it is possible for server==false
       and i_initiate() to return false; it means that this proc
       initially initiated the connection, but we expect it to be
       rejected. */
    bool server;

    /* Whether this contents struct has been saved on the client list
       or not */
    bool on_client_list;

    /* A list of all the id_context_t's that are using this
       rdmacm_contents_t */
    opal_list_t ids;
} rdmacm_contents_t;

static void rdmacm_contents_constructor(rdmacm_contents_t *contents);
static void rdmacm_contents_destructor(rdmacm_contents_t *contents);
OBJ_CLASS_INSTANCE(rdmacm_contents_t, opal_list_item_t, 
                   rdmacm_contents_constructor,
                   rdmacm_contents_destructor);

typedef struct {
    int device_max_qp_rd_atom;
    int device_max_qp_init_rd_atom;
    uint32_t ipaddr;
    uint16_t tcp_port;
    uint8_t end;
} modex_message_t;

typedef struct {
    int rdmacm_counter;
} rdmacm_endpoint_local_cpc_data_t;

/*
 * There are one of these for each RDMA CM ID.  Because of BSRQ, there
 * can be multiple of these for one endpoint, so all the
 * id_context_t's on a single endpoing share a single
 * rdmacm_contents_t.
 */
typedef struct {
    opal_list_item_t super;
    rdmacm_contents_t *contents;
    mca_btl_openib_endpoint_t *endpoint;
    uint8_t qpnum;
    bool already_disconnected;
    uint16_t route_retry_count;
    struct rdma_cm_id *id;
} id_context_t;

static void id_context_constructor(id_context_t *context);
static void id_context_destructor(id_context_t *context);
OBJ_CLASS_INSTANCE(id_context_t, opal_list_item_t, 
                   id_context_constructor,
                   id_context_destructor);

typedef struct {
    uint32_t rem_index;
    uint16_t rem_port;
    uint8_t qpnum;
} private_data_t;

/* Used to send a specific show_help message from the service_thread
   to the main thread (because we can't call show_help from the
   service_thread) */
typedef struct {
    char device_name[32];
    uint32_t peer_ip_addr;
    uint32_t peer_tcp_port;
} cant_find_endpoint_context_t;

static opal_list_t server_listener_list;
static opal_list_t client_list;
static opal_mutex_t client_list_lock;
static struct rdma_event_channel *event_channel = NULL;
static int rdmacm_priority = 30;
static uint16_t rdmacm_port = 0;
static uint32_t rdmacm_addr = 0;
static int rdmacm_resolve_timeout = 30000;
static int rdmacm_resolve_max_retry_count = 20;
static bool rdmacm_reject_causes_connect_error = false;
static volatile int disconnect_callbacks = 0;
static bool rdmacm_component_initialized = false;

/* Calculate the *real* length of the message (not aligned/rounded
   up) */
static int message_len = offsetof(modex_message_t, end);

/* Rejection reasons */
typedef enum {
    REJECT_WRONG_DIRECTION,
    REJECT_TRY_AGAIN
} reject_reason_t;

static void id_context_constructor(id_context_t *context)
{
    context->already_disconnected = false;
    context->id = NULL;
    context->contents = NULL;
    context->endpoint = NULL;
    context->qpnum = 255;
    context->route_retry_count = 0;
}

static void id_context_destructor(id_context_t *context)
{
    if (NULL != context->id) {
        rdma_destroy_id(context->id);
        context->id = NULL;
    }
    if (NULL != context->contents) {
        OBJ_RELEASE(context->contents);
    }
}

static void rdmacm_contents_constructor(rdmacm_contents_t *contents)
{
    contents->endpoint = NULL;
    contents->openib_btl = NULL;
    contents->dummy_cq = NULL;
    contents->ipaddr = 0;
    contents->tcp_port = 0;
    contents->server = false;
    contents->on_client_list = false;
    OBJ_CONSTRUCT(&(contents->ids), opal_list_t);
}

static void rdmacm_contents_destructor(rdmacm_contents_t *contents)
{
    OBJ_DESTRUCT(&(contents->ids));
}

/*
 * Invoked by main thread
 *
 * Sets up any rdma_cm specific commandline params 
 */
static void rdmacm_component_register(void)
{
    int value;

    mca_base_param_reg_int(&mca_btl_openib_component.super.btl_version,
                           "connect_rdmacm_priority",
                           "The selection method priority for rdma_cm",
                           false, false, rdmacm_priority, &rdmacm_priority);

    if (rdmacm_priority > 100) {
        rdmacm_priority = 100;
    } else if (rdmacm_priority < 0) {
        rdmacm_priority = 0;
    }

    mca_base_param_reg_int(&mca_btl_openib_component.super.btl_version,
                           "connect_rdmacm_port",
                           "The selection method port for rdma_cm",
                           false, false, rdmacm_port, &value);
    if (value >= 0 && value < 65536) {
        rdmacm_port = (uint16_t) value;
    } else {
        orte_show_help("help-mpi-btl-openib-cpc-rdmacm.txt",
                       "illegal tcp port", true, value);
    }

    mca_base_param_reg_int(&mca_btl_openib_component.super.btl_version,
                           "connect_rdmacm_resolve_timeout",
                           "The timeout (in miliseconds) for address and route resolution",
                           false, false, rdmacm_resolve_timeout, &value);
    if (value > 0) {
        rdmacm_resolve_timeout = value;
    } else {
        orte_show_help("help-mpi-btl-openib-cpc-rdmacm.txt",
                       "illegal timeout", true, value);
    }

    mca_base_param_reg_int(&mca_btl_openib_component.super.btl_version,
                           "connect_rdmacm_retry_count",
                           "Maximum number of times rdmacm will retry route resolution",
                           false, false, rdmacm_resolve_max_retry_count, &value);
    if (value > 0) {
        rdmacm_resolve_max_retry_count = value;
    } else {
        orte_show_help("help-mpi-btl-openib-cpc-rdmacm.txt",
                       "illegal retry count", true, value);
    }

    mca_base_param_reg_int(&mca_btl_openib_component.super.btl_version,
                           "connect_rdmacm_reject_causes_connect_error",
                           "The drivers for some devices are buggy such that an RDMA REJECT action may result in a CONNECT_ERROR event instead of a REJECTED event.  Setting this MCA parameter to true tells Open MPI to treat CONNECT_ERROR events on connections where a REJECT is expected as a REJECT (default: false)",
                           false, false, 0, &value);
    rdmacm_reject_causes_connect_error = (bool) (value != 0);
}

/*
 * Helper function for when we are debugging
 */
static char *stringify(uint32_t addr)
{
    char *line = malloc(64);
    asprintf(&line, "%d.%d.%d.%d (0x%x)", 
#if defined(WORDS_BIGENDIAN)
             (addr >> 24),
             (addr >> 16) & 0xff,
             (addr >> 8) & 0xff,
             addr & 0xff,
#else
             addr & 0xff,
             (addr >> 8) & 0xff,
             (addr >> 16) & 0xff,
             (addr >> 24),
#endif
             addr);
    return line;
}

/*
 * Invoked by service thread
 * 
 * This function traverses the list of endpoints associated with the
 * device and determines which of them the remote side is attempting
 * to connect to.  This is determined based on the local endpoint's
 * modex message recevied and the IP address and port associated with
 * the rdma_cm event id
 */
static mca_btl_openib_endpoint_t *rdmacm_find_endpoint(rdmacm_contents_t *contents,
                                                       struct rdma_cm_id *id,
                                                       uint16_t rem_port)
{
    int i;
    mca_btl_openib_endpoint_t *ep = NULL;
    opal_pointer_array_t *endpoints = contents->openib_btl->device->endpoints;
    struct sockaddr *peeraddr = rdma_get_peer_addr(id);
    uint32_t peeripaddr = ((struct sockaddr_in *)peeraddr)->sin_addr.s_addr;
#if OPAL_ENABLE_DEBUG
    char *a;
#endif

    OPAL_OUTPUT((-1, "remote peer requesting connection: %s port %d",
                 a = stringify(peeripaddr), rem_port));
#if OPAL_ENABLE_DEBUG
    free(a);
#endif
    for (i = 0; i < opal_pointer_array_get_size(endpoints); i++) {
        mca_btl_openib_endpoint_t *endpoint;
        modex_message_t *message;

        endpoint = opal_pointer_array_get_item(endpoints, i);
        if (NULL == endpoint) {
            continue;
        }

        message = endpoint->endpoint_remote_cpc_data->cbm_modex_message;
        OPAL_OUTPUT((-1, "message ipaddr = %s port %d",
                     a = stringify(message->ipaddr), message->tcp_port));
#if OPAL_ENABLE_DEBUG
        free(a);
#endif
        if (message->ipaddr == peeripaddr && message->tcp_port == rem_port) {
            ep = endpoint;
            break;
        }
    }

    if (NULL == ep) {
        BTL_ERROR(("can't find suitable endpoint for this peer"));
    }

    return ep;
}

/*
 * Returns max inlne size for qp #N 
 */
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
    /* Otherwise it is no reason for inline */
    return 0;
}


/*
 * Invoked by both main and service threads
 */
static int rdmacm_setup_qp(rdmacm_contents_t *contents,
                           mca_btl_openib_endpoint_t *endpoint,
                           struct rdma_cm_id *id,
                           int qpnum)
{
    struct ibv_qp_init_attr attr;
    struct ibv_qp *qp;
    struct ibv_srq *srq = NULL;
    int credits = 0, reserved = 0, max_recv_wr, max_send_wr;
    size_t req_inline;

    if (qpnum == mca_btl_openib_component.credits_qp) {
        int qp;

        for (qp = 0; qp < mca_btl_openib_component.num_qps; qp++) {
            if(BTL_OPENIB_QP_TYPE_PP(qp)) {
                reserved += mca_btl_openib_component.qp_infos[qp].u.pp_qp.rd_rsv;
            }
        }
        credits = mca_btl_openib_component.num_qps;
    }

    if (BTL_OPENIB_QP_TYPE_PP(qpnum)) {
        max_recv_wr = mca_btl_openib_component.qp_infos[qpnum].rd_num + reserved;
        max_send_wr = mca_btl_openib_component.qp_infos[qpnum].rd_num + credits;
    } else {
        srq = endpoint->endpoint_btl->qps[qpnum].u.srq_qp.srq;
        max_recv_wr = reserved;
        max_send_wr = mca_btl_openib_component.qp_infos[qpnum].u.srq_qp.sd_max + credits;
    }

    memset(&attr, 0, sizeof(attr));
    attr.qp_type = IBV_QPT_RC;
    attr.send_cq = contents->openib_btl->device->ib_cq[BTL_OPENIB_LP_CQ];
    attr.recv_cq = contents->openib_btl->device->ib_cq[qp_cq_prio(qpnum)];
    attr.srq = srq;
    if(BTL_OPENIB_QP_TYPE_PP(qpnum)) {
        /* Add one for the CTS receive frag that will be posted */
        attr.cap.max_recv_wr = max_recv_wr + 1;
    } else {
        attr.cap.max_recv_wr = 0;
    }
    attr.cap.max_send_wr = max_send_wr;
    attr.cap.max_inline_data = req_inline = 
        max_inline_size(qpnum, contents->openib_btl->device);
    attr.cap.max_send_sge = 1;
    attr.cap.max_recv_sge = 1; /* we do not use SG list */

    {
        /* JMS Temprary gross hack: we *must* use rdma_create_cp()
           (vs. ibv_create_qp()) because strange things happen on IB
           if we don't.  However, rdma_create_cp() wants us to use
           rdma_get_devices() (and therefore the pd that they have
           allocated).  In order to get v1.3 out the door, we're
           bypassing this functionality - we're temporarily overriding
           the device context cached on the ID with our own, so that
           our pd will match.  We need to fix this to properly get the
           pd from the RDMA CM and use that, etc. */
        struct ibv_context *temp = id->verbs;
        id->verbs = contents->openib_btl->device->ib_pd->context;
        if (0 != rdma_create_qp(id, contents->openib_btl->device->ib_pd,
                                &attr)) {
            BTL_ERROR(("Failed to create qp with %d", qpnum));
            goto out;
        }
        qp = id->qp;
        id->verbs = temp;
    }

    endpoint->qps[qpnum].qp->lcl_qp = qp;
    endpoint->qps[qpnum].credit_frag = NULL;
    if (attr.cap.max_inline_data < req_inline) {
        endpoint->qps[qpnum].ib_inline_max = attr.cap.max_inline_data;
        orte_show_help("help-mpi-btl-openib-cpc-base.txt",
                       "inline truncated", true,
                       orte_process_info.nodename,
                       ibv_get_device_name(contents->openib_btl->device->ib_dev),
                       contents->openib_btl->port_num,
                       req_inline, attr.cap.max_inline_data);
    } else {
        endpoint->qps[qpnum].ib_inline_max = req_inline;
    }
    id->qp = qp;

    return OMPI_SUCCESS;

out:
    return OMPI_ERROR;
}


/* 
 * Invoked by both main and service threads
 *
 * To avoid all kinds of nasty race conditions, we only allow
 * connections to be made in one direction.  So use a simple
 * (arbitrary) test to decide which direction is allowed to initiate
 * the connection: the process with the lower IP address wins.  If the
 * IP addresses are the same (i.e., the MPI procs are on the same
 * node), then the process with the lower TCP port wins.
 */
static bool i_initiate(uint32_t local_ipaddr, uint16_t local_port,
                       uint32_t remote_ipaddr, uint16_t remote_port)
{
#if OPAL_ENABLE_DEBUG
    char *a = stringify(local_ipaddr);
    char *b = stringify(remote_ipaddr);
#endif
    
    if (local_ipaddr > remote_ipaddr ||
        (local_ipaddr == remote_ipaddr && local_port < remote_port)) {
        OPAL_OUTPUT((-1, "i_initiate (I WIN): local ipaddr %s, remote ipaddr %s",
                     a, b));
#if OPAL_ENABLE_DEBUG
        free(a);
        free(b);
#endif
        return true;
    } else {
        OPAL_OUTPUT((-1, "i_initiate (I lose): local ipaddr %s, remote ipaddr %s",
                     a, b));
#if OPAL_ENABLE_DEBUG
        free(a);
        free(b);
#endif
        return false;
    }
}

/*
 * Invoked by main thread
 */
static int rdmacm_client_connect_one(rdmacm_contents_t *contents,
                                     modex_message_t *message,
                                     int num)
{
    struct sockaddr_in src_in, dest_in;
    id_context_t *context;
    int rc;
#if OPAL_ENABLE_DEBUG
    char *a, *b;
#endif

    /* We'll need to access some data in the event handler.  We can
     * encapsulate it in this data struct and attach it to the id being
     * created below.  The event->id will contain this same pointer.
     */
    context = OBJ_NEW(id_context_t);
    if (NULL == context) {
        BTL_ERROR(("malloc error"));
        goto out;
    }

    context->contents = contents;
    OBJ_RETAIN(contents);
    context->qpnum = num;
    context->endpoint = contents->endpoint;

    rc = rdma_create_id(event_channel, &(context->id),
                        context, RDMA_PS_TCP);
    if (0 != rc) {
        BTL_ERROR(("Failed to create a rdma id with %d", rc));
        goto out1;
    }

    /* Source address (we must specify this to ensure that the traffic
       goes out on the device+port that we expect it go out). */
    memset(&src_in, 0, sizeof(src_in));
    src_in.sin_family = AF_INET;
    src_in.sin_addr.s_addr = contents->ipaddr;
    src_in.sin_port = 0;

    /* Destination address */
    memset(&dest_in, 0, sizeof(dest_in));
    dest_in.sin_family = AF_INET;
    dest_in.sin_addr.s_addr = message->ipaddr;
    dest_in.sin_port = message->tcp_port;

    /* Once the route to the remote system is discovered, a
     * RDMA_CM_EVENT_ADDR_RESOLVED event will occur on the local event
     * handler.
     */
    OPAL_OUTPUT((-1, "MAIN Resolving id: from IP %s:%d to IP %s:%d", 
                 a = stringify(contents->ipaddr), 
                 contents->tcp_port,
                 b = stringify(message->ipaddr), 
                 message->tcp_port));
#if OPAL_ENABLE_DEBUG
    free(a);
    free(b);
#endif

    /* This is odd an worth explaining: when we place the context on
       the ids list, we need to add an extra RETAIN to the context.
       The reason is because of a race condition.  Let's explain
       through a few cases:

       1. Normal termination: client side endpoint_finalize removes
          the context from the ids list, has its service thread call
          rdma_disconnect(), and then RELEASE.  A DISCONNECT event
          will occur on both sides; the client DISCONNECT will invoke
          RELEASE again on the context.  Note that the DISCONNECT
          event may occur *very* quickly on the client side, so the
          order of these two RELEASEs is not known.  The destructor
          will invoke rdma_destroy_id() -- we obviously can't have
          this happen before both actions complete.  Hence,
          refcounting (and the additional RETAIN) saves us.

          Note that the server side never had the context on the ids
          list, so it never had an extra RETAIN.  So the DISCONNECT on
          the server side will only invoke one RELEASE.

       2. Abnormal termination: if the server side terminates
          improperly (e.g., user's app segv's), then the kernel from
          the server side will send a DISCONNECT event to the client
          before the item has been removed from the ids list.  This
          will cause an assertion failure in debug builds (because
          we'll be trying to RELEASE an opal_list_item_t that is still
          on a list), and possibly other badness in optimized builds
          because we'll be traversing a freed opal_list_item_t in
          endpoint_finalize.  So the extra RETAIN here right when we
          put the item on the list prevents it from actually being
          released in the client until BOTH the endpoint_finalize
          occurs *and* the DISCONNECT event arrives.

       Asynchronous programming is fun!
     */
    OBJ_RETAIN(context);
    opal_list_append(&(contents->ids), &(context->super));

    rc = rdma_resolve_addr(context->id,
                           (struct sockaddr *) &src_in,
                           (struct sockaddr *) &dest_in,
                           rdmacm_resolve_timeout);
    if (0 != rc) {
        BTL_ERROR(("Failed to resolve the remote address with %d", rc));
        goto out1;
    }

    return OMPI_SUCCESS;

out1:
    OBJ_RELEASE(context);
out:
    return OMPI_ERROR;
}

/* 
 * Invoked by main thread
 *
 * Connect method called by the upper layers to connect the local
 * endpoint to the remote endpoint by creating QP(s) to connect the two.
 * Already holding endpoint lock when this function is called.
 */
static int rdmacm_module_start_connect(ompi_btl_openib_connect_base_module_t *cpc,
                                       mca_btl_base_endpoint_t *endpoint)
{
    rdmacm_contents_t *contents;
    modex_message_t *message, *local_message;
    int rc, qp;
    opal_list_item_t *item;
#if OPAL_ENABLE_DEBUG
    char *a, *b;
#endif

    /* Don't use the CPC to get the message, because this function is
       invoked from the event_handler (to intitiate connections in the
       Right direction), where we don't have the CPC, so it'll be
       NULL. */
    local_message = 
        (modex_message_t *) endpoint->endpoint_local_cpc->data.cbm_modex_message;
    message = (modex_message_t *)
        endpoint->endpoint_remote_cpc_data->cbm_modex_message;

    OPAL_OUTPUT((-1, "Connecting from IP %s:%d to remote IP %s:%d  ep state = %d",
                 a = stringify(local_message->ipaddr), local_message->tcp_port,
                 b = stringify(message->ipaddr), message->tcp_port, endpoint->endpoint_state));
#if OPAL_ENABLE_DEBUG
    free(a);
    free(b);
#endif
    BTL_VERBOSE(("Connecting to remote ip addr = %x, port = %d  ep state = %d",
                 message->ipaddr, message->tcp_port, endpoint->endpoint_state));

    if (MCA_BTL_IB_CONNECTED == endpoint->endpoint_state ||
        MCA_BTL_IB_CONNECTING == endpoint->endpoint_state ||
        MCA_BTL_IB_CONNECT_ACK == endpoint->endpoint_state) {
        return OMPI_SUCCESS;
    }

    /* Set the endpoint state to "connecting" (this function runs in
       the main MPI thread; not the service thread, so we can set the
       endpoint_state here). */
    endpoint->endpoint_state = MCA_BTL_IB_CONNECTING;

    contents = OBJ_NEW(rdmacm_contents_t);
    if (NULL == contents) {
        BTL_ERROR(("malloc of contents failed"));
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto out;
    }

    contents->openib_btl = endpoint->endpoint_btl;
    contents->endpoint = endpoint;
    contents->server = false;
    /* Populate the port information with the local port the server is
     * listening on instead of the ephemerial port this client is
     * connecting with.  This port is used to determine which endpoint
     * is being connected from, in the case where there are multiple
     * listeners on the local system.
     */
    contents->ipaddr = local_message->ipaddr;
    contents->tcp_port = local_message->tcp_port;

    /* Are we the initiator?  Or do we expect this connect request to
       be rejected? */
    endpoint->endpoint_initiator = 
        i_initiate(contents->ipaddr, contents->tcp_port, 
                   message->ipaddr, message->tcp_port);
    OPAL_OUTPUT((-1, "MAIN Start connect; ep=%p (%p), I %s the initiator to %s",
                 (void*) endpoint,
                 (void*) endpoint->endpoint_local_cpc,
                 endpoint->endpoint_initiator ? "am" : "am NOT",
                 endpoint->endpoint_proc->proc_ompi->proc_hostname));

    /* If we're the initiator, then open all the QPs */
    if (contents->endpoint->endpoint_initiator) {
        /* Initiator needs a CTS frag (non-initiator will have a CTS
           frag allocated later) */
        if (OMPI_SUCCESS != 
            (rc = ompi_btl_openib_connect_base_alloc_cts(contents->endpoint))) {
            BTL_ERROR(("Failed to alloc CTS frag"));
            goto out;
        }

        for (qp = 0; qp < mca_btl_openib_component.num_qps; qp++) {
            rc = rdmacm_client_connect_one(contents, message, qp);
            if (OMPI_SUCCESS != rc) {
                BTL_ERROR(("rdmacm_client_connect_one error (real QP %d)", 
                           qp));
                goto out;
            }
        }
    }
    /* Otherwise, only open 1 QP that we expect to be rejected */
    else {
        rc = rdmacm_client_connect_one(contents, message, 0);
        if (OMPI_SUCCESS != rc) {
            BTL_ERROR(("rdmacm_client_connect_one error (bogus QP)"));
            goto out;
        }
    }

    return OMPI_SUCCESS;

out:
    for (item = opal_list_remove_first(&(contents->ids));
         NULL != item;
         item = opal_list_remove_first(&(contents->ids))) {
        OBJ_RELEASE(item);
   }

   return rc;
}

static void *show_help_cant_find_endpoint(void *context)
{
    char *msg;
    cant_find_endpoint_context_t *c = 
        (cant_find_endpoint_context_t*) context;

    if (NULL != c) {
        msg = stringify(c->peer_ip_addr);
        orte_show_help("help-mpi-btl-openib-cpc-rdmacm.txt",
                       "could not find matching endpoint", true,
                       orte_process_info.nodename,
                       c->device_name,
                       c->peer_tcp_port);
        free(msg);
    } else {
        orte_show_help("help-mpi-btl-openib-cpc-rdmacm.txt",
                       "could not find matching endpoint", true,
                       orte_process_info.nodename,
                       "<unknown>", "<unknown>", -1);
    }
    free(context);

    /* Now kill it */
    mca_btl_openib_endpoint_invoke_error(NULL);
    return NULL;
}

/* 
 * Invoked by service thread
 *
 * The server thread will handle the incoming connection requests and
 * allow them or reject them based on a unidirectional connection
 * method.  The choonections are allowed based on the IP address and
 * port values.  This determination is arbitrary, but is uniform in
 * allowing the connections only in 1 direction.  If the connection in
 * the requestion is disallowed by this rule, then the server will
 * reject the connection and make its own in the proper direction.
 */
static int handle_connect_request(struct rdma_cm_event *event)
{
    id_context_t *listener_context = (id_context_t*) event->id->context;
    id_context_t *new_context = NULL;
    rdmacm_contents_t *contents = listener_context->contents;
    mca_btl_openib_endpoint_t *endpoint;
    struct rdma_conn_param conn_param;
    modex_message_t *message;
    private_data_t msg;
    int rc = -1, qpnum;
    uint32_t rem_index;
    uint16_t rem_port;

    qpnum = ((private_data_t *)event->param.conn.private_data)->qpnum;
    rem_port = ((private_data_t *)event->param.conn.private_data)->rem_port;
    rem_index = ((private_data_t *)event->param.conn.private_data)->rem_index;

    /* Determine which endpoint the remote side is trying to connect
       to; use the listener's context->contents to figure it out */
    endpoint = rdmacm_find_endpoint(contents, event->id, rem_port);
    if (NULL == endpoint) {
        struct sockaddr *peeraddr = rdma_get_peer_addr(event->id);
        cant_find_endpoint_context_t *c = calloc(1, sizeof(*c));
        if (NULL != c) {
            snprintf(c->device_name, sizeof(c->device_name) - 1,
                     "%s:%d",
                     ibv_get_device_name(contents->openib_btl->device->ib_dev),
                     contents->openib_btl->port_num);
            c->peer_ip_addr =
                ((struct sockaddr_in *)peeraddr)->sin_addr.s_addr;
            c->peer_tcp_port = rdma_get_dst_port(event->id);
        }
        ompi_btl_openib_fd_run_in_main(show_help_cant_find_endpoint, c);
        goto out;
    }

    message = endpoint->endpoint_remote_cpc_data->cbm_modex_message;
    endpoint->endpoint_initiator = 
        i_initiate(contents->ipaddr, contents->tcp_port,
                   message->ipaddr, rem_port);

    BTL_VERBOSE(("ep state = %d, local ipaddr = %x, remote ipaddr = %x, local port = %d, remote port = %d",
                 endpoint->endpoint_state, contents->ipaddr, message->ipaddr, 
                 contents->tcp_port, rem_port));

    OPAL_OUTPUT((-1, "SERVICE in handle_connect_request; ep=%p (%p), I still %s the initiator to %s",
                 (void*) endpoint,
                 (void*) endpoint->endpoint_local_cpc,
                 endpoint->endpoint_initiator ? "am" : "am NOT",
                 endpoint->endpoint_proc->proc_ompi->proc_hostname));
    if (endpoint->endpoint_initiator) {
        reject_reason_t reason = REJECT_WRONG_DIRECTION;

        OPAL_OUTPUT((-1, "SERVICE Received a connect request from an endpoint in the wrong direction"));

        /* This will cause a event on the remote system.  By passing in
         * a value in the second arg of rdma_reject, the remote side
         * can check for this to know if it was an intentional reject or
         * a reject based on an error.
         */
        rc = rdma_reject(event->id, &reason, sizeof(reject_reason_t));
        if (0 != rc) {
            BTL_ERROR(("rdma_reject failed %d", rc));
            goto out;
        }

        OPAL_OUTPUT((-1, "SERVICE Starting connection in other direction"));
        rdmacm_module_start_connect(NULL, endpoint);

        return OMPI_SUCCESS;
    }

    /* Set the endpoint_state to "CONNECTING".  This is running
       in the service thread, so we need to do a write barrier. */
    endpoint->endpoint_state = MCA_BTL_IB_CONNECTING;
    opal_atomic_wmb();

    endpoint->rem_info.rem_index = rem_index;

    /* Setup QP for new connection */
    BTL_VERBOSE(("ACCEPTING src port = %d, dst port = %d, qpnum = %d",
                 rdma_get_src_port(event->id), rdma_get_dst_port(event->id), qpnum));

    rc = rdmacm_setup_qp(contents, endpoint, event->id, qpnum);
    if (0 != rc) {
        BTL_ERROR(("rdmacm_setup_qp error %d", rc));
        goto out;
    }

    /* Post a single receive buffer on the smallest QP for the CTS
       protocol */
    if (mca_btl_openib_component.credits_qp == qpnum) {
        struct ibv_recv_wr *bad_wr, *wr;

        if (OMPI_SUCCESS != 
            ompi_btl_openib_connect_base_alloc_cts(endpoint)) {
            BTL_ERROR(("Failed to alloc CTS frag"));
            goto out1;
        }
        wr = &(endpoint->endpoint_cts_frag.rd_desc);
        assert(NULL != wr);
        wr->next = NULL;

        if (0 != ibv_post_recv(endpoint->qps[qpnum].qp->lcl_qp, 
                               wr, &bad_wr)) {
            BTL_ERROR(("failed to post CTS recv buffer"));
            goto out1;
        }
        OPAL_OUTPUT((-1, "Posted CTS receiver buffer (%p) for peer %s, qp index %d (QP num %d), WR ID %p, SG addr %p, len %d, lkey %d",
                     (void*) wr->sg_list[0].addr,
                     endpoint->endpoint_proc->proc_ompi->proc_hostname,
                     qpnum,
                     endpoint->qps[qpnum].qp->lcl_qp->qp_num,
                     (void*) wr->wr_id,
                     (void*) wr->sg_list[0].addr,
                     wr->sg_list[0].length,
                     wr->sg_list[0].lkey));
    }

    /* Since the event id is already created (since we're the server),
       the context that was passed to us was the listen server's
       context -- which is no longer useful to us.  So allocate a new
       context and populate it just for this connection. */
    event->id->context = new_context = OBJ_NEW(id_context_t);
    if (NULL == new_context) {
        BTL_ERROR(("malloc error"));
        goto out1;
    }

    new_context->contents = contents;
    OBJ_RETAIN(contents);
    new_context->qpnum = qpnum;
    new_context->endpoint = endpoint;

    memset(&conn_param, 0, sizeof(conn_param));
    /* See rdma_connect(3) for a description of these 2 values.  We
       ensure to pass these values around via the modex so that we can
       compute the values properly. */
    conn_param.responder_resources = 
        mymin(contents->openib_btl->device->ib_dev_attr.max_qp_rd_atom,
              message->device_max_qp_init_rd_atom);
    conn_param.initiator_depth = 
        mymin(contents->openib_btl->device->ib_dev_attr.max_qp_init_rd_atom,
              message->device_max_qp_rd_atom);
    conn_param.retry_count = mca_btl_openib_component.ib_retry_count;
    conn_param.rnr_retry_count = BTL_OPENIB_QP_TYPE_PP(qpnum) ? 0 :
        mca_btl_openib_component.ib_rnr_retry;
    conn_param.srq = BTL_OPENIB_QP_TYPE_SRQ(qpnum);
    conn_param.private_data = &msg;
    conn_param.private_data_len = sizeof(private_data_t);

    /* Fill the private data being sent to the other side */
    msg.qpnum = qpnum;
    msg.rem_index = endpoint->index;

    /* Accepting the connection will result in a
       RDMA_CM_EVENT_ESTABLISHED event on both the client and server
       side. */
    rc = rdma_accept(event->id, &conn_param);
    if (0 != rc) {
        BTL_ERROR(("rdma_accept error %d", rc));
        goto out2;
    }

    return OMPI_SUCCESS;

out2:
    OBJ_RELEASE(new_context);
out1:
    ibv_destroy_qp(endpoint->qps[qpnum].qp->lcl_qp);
out:
    return OMPI_ERROR;
}

/*
 * Invoked by service thread
 */
static void *rdmacm_unmonitor(int fd, int flags, void *context)
{
    volatile int *barrier = (volatile int *) context;

    OPAL_OUTPUT((-1, "SERVICE rdmacm unlocking main thread"));
    *barrier = 1;

    return NULL;
}

/*
 * Runs in service thread
 *
 * We call rdma_disconnect() here in the service thread so that there
 * is zero chance that the DISCONNECT event is delivered and executed
 * in the service thread while rdma_disconnect() is still running in
 * the main thread (which causes all manner of Bad Things to occur).
 */
static void *call_disconnect_callback(void *v)
{
    void *tmp = NULL;
    id_context_t *context = (id_context_t*) v;
    OPAL_OUTPUT((-1, "SERVICE Service thread calling disconnect on ID %p",
                 (void*) context->id));

    if (!context->already_disconnected) {
        tmp = context->id;
        rdma_disconnect(context->id);
        context->already_disconnected = true;
    }
    OBJ_RELEASE(context);

    /* Tell the main thread that we're done */
    opal_atomic_add(&disconnect_callbacks, 1);
    OPAL_OUTPUT((-1, "SERVICE Service thread disconnect on ID %p done; count=%d",
                 (void*) tmp, disconnect_callbacks));
    return NULL;
}

/* 
 * Invoked by main thread
 * 
 * Runs *while* the progress thread is running.  We can't stop the
 * progress thread because this function may be invoked to kill a
 * specific endpoint that was the result of MPI-2 dynamics (i.e., this
 * is not during MPI_FINALIZE).
 */
static int rdmacm_endpoint_finalize(struct mca_btl_base_endpoint_t *endpoint)
{
    int num_to_wait_for;
    opal_list_item_t *item, *item2;

    BTL_VERBOSE(("Start disconnecting..."));
    OPAL_OUTPUT((-1, "MAIN Endpoint finalizing"));

    if (NULL == endpoint) {
        BTL_ERROR(("Attempting to shutdown a NULL endpoint"));
        return OMPI_SUCCESS;
    }

    /* Determine which rdmacm_contents_t correlates to the endpoint
     * we are shutting down.  By disconnecting instead of simply
     * destroying the QPs, we are shutting down in a more graceful way
     * thus preventing errors on the line.
     *
     * Need to lock because the client_list is accessed in both the
     * main thread and service thread.
     */
    opal_mutex_lock(&client_list_lock);
    num_to_wait_for = disconnect_callbacks = 0;
    for (item = opal_list_get_first(&client_list);
         item != opal_list_get_end(&client_list); 
         item = opal_list_get_next(item)) {
        rdmacm_contents_t *contents = (rdmacm_contents_t *) item;

        if (endpoint == contents->endpoint) {
            while (NULL != 
                   (item2 = opal_list_remove_first(&(contents->ids)))) {
                /* Fun race condition: we cannot call
                   rdma_disconnect() here in the main thread, because
                   if we do, there is a nonzero chance that the
                   DISCONNECT event will be delivered and get executed
                   in the service thread immediately.  If this all
                   happens before rdma_disconnect() returns, all
                   manner of Bad Things can/will occur.  So just
                   invoke rdma_disconnect() in the service thread
                   where we guarantee that we won't be processing an
                   event when it is called. */
                OPAL_OUTPUT((-1, "MAIN Main thread calling disconnect on ID %p",
                             (void*) ((id_context_t*) item2)->id));
                ++num_to_wait_for;
                ompi_btl_openib_fd_run_in_service(call_disconnect_callback,
                                                  item2);
            }
	    /* remove_item returns the item before the item removed,
	       meaning that the for list is still safe */
            item = opal_list_remove_item(&client_list, item);
            contents->on_client_list = false;
            break;
        }
    }

    /* Flush writes to ensure we sync across threads */
    opal_atomic_wmb();
    opal_mutex_unlock(&client_list_lock);

    /* Now wait for all the disconnect callbacks to occur */
    while (num_to_wait_for != disconnect_callbacks) {
        ompi_btl_openib_fd_main_thread_drain();
        sched_yield();
    }

    OPAL_OUTPUT((-1, "MAIN Endpoint finished finalizing"));
    return OMPI_SUCCESS;
}

/*
 * Callback (from main thread) when the endpoint has been connected
 */
static void *local_endpoint_cpc_complete(void *context)
{
    mca_btl_openib_endpoint_t *endpoint = (mca_btl_openib_endpoint_t *)context;

    OPAL_OUTPUT((-1, "MAIN local_endpoint_cpc_complete to %s",
                 endpoint->endpoint_proc->proc_ompi->proc_hostname));
    mca_btl_openib_endpoint_cpc_complete(endpoint);

    return NULL;
}

/*
 * Runs in service thread
 */
static int rdmacm_connect_endpoint(id_context_t *context,
                                   struct rdma_cm_event *event)
{
    rdmacm_contents_t *contents = context->contents;
    rdmacm_endpoint_local_cpc_data_t *data;
    mca_btl_openib_endpoint_t *endpoint;
    modex_message_t *message;

    if (contents->server) {
        endpoint = context->endpoint;
        OPAL_OUTPUT((-1, "SERVICE Server CPC complete to %s",
                     endpoint->endpoint_proc->proc_ompi->proc_hostname));
    } else {
        endpoint = contents->endpoint;
        endpoint->rem_info.rem_index =
            ((private_data_t *)event->param.conn.private_data)->rem_index;

        if (!contents->on_client_list) {
            opal_mutex_lock(&client_list_lock);
            opal_list_append(&client_list, &(contents->super));
            /* Flush writes to ensure we sync across threads */
            opal_atomic_wmb();
            opal_mutex_unlock(&client_list_lock);
            contents->on_client_list = true;
        }
        OPAL_OUTPUT((-1, "SERVICE Client CPC complete to %s",
                     endpoint->endpoint_proc->proc_ompi->proc_hostname));
    }
    if (NULL == endpoint) {
        BTL_ERROR(("Can't find endpoint"));
        return OMPI_ERR_NOT_FOUND;
    }
    data = 
        (rdmacm_endpoint_local_cpc_data_t *)endpoint->endpoint_local_cpc_data;

    /* Only notify the upper layers after the last QP has been
       connected */
    if (++data->rdmacm_counter < mca_btl_openib_component.num_qps) {
        BTL_VERBOSE(("%s to peer %s, count == %d", contents->server?"server":"client", endpoint->endpoint_proc->proc_ompi->proc_hostname, data->rdmacm_counter));
        OPAL_OUTPUT((-1, "%s to peer %s, count == %d", contents->server?"server":"client", endpoint->endpoint_proc->proc_ompi->proc_hostname, data->rdmacm_counter));
        return OMPI_SUCCESS;
    }

    message = endpoint->endpoint_remote_cpc_data->cbm_modex_message;
    BTL_VERBOSE(("%s connected!!! local %x remote %x state = %d",
                 contents->server?"server":"client",
                 contents->ipaddr,
                 message->ipaddr,
                 endpoint->endpoint_state));

    /* Ensure that all the writes back to the endpoint and associated
       data structures have completed */
    opal_atomic_wmb();
    ompi_btl_openib_fd_run_in_main(local_endpoint_cpc_complete, endpoint);

    return OMPI_SUCCESS;
}

/*
 * Runs in service thread
 */
static int rdmacm_disconnected(id_context_t *context)
{
    /* If this was a client thread, then it *may* still be listed in a
       contents->ids list. */

    context->already_disconnected = true;
    if (NULL != context) {
        OPAL_OUTPUT((-1, "SERVICE Releasing context because of DISCONNECT: context %p, id %p",
                     (void*) context, (void*) context->id));
        OBJ_RELEASE(context);
    }
    return OMPI_SUCCESS;
}

/*
 * Runs in service thread
 */
static int rdmacm_destroy_dummy_qp(id_context_t *context)
{
    /* We need to check id pointer because of retransmitions.
       Maybe the reject was already done. */

    if (NULL != context->id) {
	    if (NULL != context->id->qp) {
           ibv_destroy_qp(context->id->qp);
           context->id->qp = NULL;
        }
    }

    if (NULL != context->contents->dummy_cq) {
        ibv_destroy_cq(context->contents->dummy_cq);
    }
    /* This item was appended to the contents->ids list (the list will
       only have just this one item), so remove it before RELEASEing
       the item */
    opal_list_remove_first(&(context->contents->ids));
    OBJ_RELEASE(context);

    return OMPI_SUCCESS;
}

/*
 * Runs in service thread
 */
static int rdmacm_rejected(id_context_t *context, struct rdma_cm_event *event)
{
    if (NULL != event->param.conn.private_data) {
        /* Why were we rejected? */
        switch (*((reject_reason_t*) event->param.conn.private_data)) {
        case REJECT_WRONG_DIRECTION:
            OPAL_OUTPUT((-1, "SERVICE A good reject! for qp %d, id 0x%p", 
                         context->qpnum, (void*) context->id));
            rdmacm_destroy_dummy_qp(context);
            break;

        default:
            /* Just so compilers won't complain */
            break;
        }
    }

    return OMPI_SUCCESS;
}

/*
 * Runs in service thread
 */
static int resolve_route(id_context_t *context)
{
    int rc;

    /* Resolve the route to the remote system.  Once established, the
     * local system will get a RDMA_CM_EVENT_ROUTE_RESOLVED event.
     */
    rc = rdma_resolve_route(context->id, rdmacm_resolve_timeout);
    if (0 != rc) {
        BTL_ERROR(("Failed to resolve the route with %d", rc));
        goto out;
    }

#if OPAL_ENABLE_DEBUG
    {
        char *a, *b;
        OPAL_OUTPUT((-1, "Resolved route ID %p (local addr %s, remote addr %s)",
                     (void*) context->id,
                     a = stringify(((struct sockaddr_in*) rdma_get_local_addr(context->id))->sin_addr.s_addr),
                     b = stringify(((struct sockaddr_in*) rdma_get_peer_addr(context->id))->sin_addr.s_addr)));
        free(a);
        free(b);
    }
#endif

    return OMPI_SUCCESS;

out:
    return OMPI_ERROR;
}

/*
 * Runs in service thread
 */
static int create_dummy_cq(rdmacm_contents_t *contents, 
                           mca_btl_openib_module_t *openib_btl)
{
    contents->dummy_cq = 
        ibv_create_cq(openib_btl->device->ib_dev_context, 1, NULL, NULL, 0);
    if (NULL == contents->dummy_cq) {
        BTL_ERROR(("dummy_cq not created"));
        goto out;
    }

    return OMPI_SUCCESS;
out:
    return OMPI_ERROR;
}

/*
 * Runs in service thread
 */
static int create_dummy_qp(rdmacm_contents_t *contents, 
                           struct rdma_cm_id *id, int qpnum)
{
    struct ibv_qp_init_attr attr;

    memset(&attr, 0, sizeof(attr));
    attr.qp_type = IBV_QPT_RC;
    attr.send_cq = contents->dummy_cq;
    attr.recv_cq = contents->dummy_cq;
    attr.cap.max_recv_wr = 1;
    attr.cap.max_send_wr = 1;
    attr.cap.max_send_sge = 1;
    attr.cap.max_recv_sge = 1;

    {
        /* JMS Temprary gross hack: we *must* use rdma_create_cp()
           (vs. ibv_create_qp()) because strange things happen on IB
           if we don't.  However, rdma_create_cp() wants us to use
           rdma_get_devices() (and therefore the pd that they have
           allocated).  In order to get v1.3 out the door, we're
           bypassing this functionality - we're temporarily overriding
           the device context cached on the ID with our own, so that
           our pd will match.  We need to fix this to properly get the
           pd from the RDMA CM and use that, etc. */
        struct ibv_context *temp = id->verbs;
        id->verbs = contents->openib_btl->device->ib_pd->context;
        if (0 != rdma_create_qp(id, contents->openib_btl->device->ib_pd,
                                &attr)) {
            BTL_ERROR(("Failed to create qp with %d", qpnum));
            goto out;
        }
        id->verbs = temp;
    }
    BTL_VERBOSE(("dummy qp created %d", qpnum));

    return OMPI_SUCCESS;

out:
    return OMPI_ERROR;
}

/*
 * Runs in service thread
 */
static int finish_connect(id_context_t *context)
{
    rdmacm_contents_t *contents = context->contents;
    struct rdma_conn_param conn_param;
    private_data_t msg;
    int rc;
    struct sockaddr *peeraddr, *localaddr;
    uint32_t localipaddr, remoteipaddr;
    uint16_t remoteport;
    modex_message_t *message;

    remoteport = rdma_get_dst_port(context->id);
    peeraddr = rdma_get_peer_addr(context->id);
    remoteipaddr = ((struct sockaddr_in *)peeraddr)->sin_addr.s_addr;

    localaddr = rdma_get_local_addr(context->id);
    localipaddr = ((struct sockaddr_in *)localaddr)->sin_addr.s_addr;

    message = (modex_message_t *)
        context->endpoint->endpoint_remote_cpc_data->cbm_modex_message;

    /* If we're the initiator, then setup the QP's and post the CTS
       message buffer */
    if (contents->endpoint->endpoint_initiator) {
        rc = rdmacm_setup_qp(contents, contents->endpoint, 
                             context->id, context->qpnum);
        if (0 != rc) {
            BTL_ERROR(("rdmacm_setup_qp error %d", rc));
            goto out;
        }

        if (mca_btl_openib_component.credits_qp == context->qpnum) {
            /* Post a single receive buffer on the smallest QP for the CTS
               protocol */
            
            struct ibv_recv_wr *bad_wr, *wr;
            assert(NULL != contents->endpoint->endpoint_cts_frag.super.super.base.super.ptr);
            wr = &(contents->endpoint->endpoint_cts_frag.rd_desc);
            assert(NULL != wr);
            wr->next = NULL;
            
            if (0 != ibv_post_recv(contents->endpoint->qps[context->qpnum].qp->lcl_qp, 
                                   wr, &bad_wr)) {
                BTL_ERROR(("failed to post CTS recv buffer"));
                goto out1;
            }
            OPAL_OUTPUT((-1, "Posted initiator CTS buffer (%p, length %d) for peer %s, qp index %d (QP num %d)",
                         (void*) wr->sg_list[0].addr,
                         wr->sg_list[0].length,
                         contents->endpoint->endpoint_proc->proc_ompi->proc_hostname,
                         context->qpnum,
                         contents->endpoint->qps[context->qpnum].qp->lcl_qp->qp_num));
        }
    } else {
        /* If we are establishing a connection in the "wrong" direction,
         * setup a dummy CQ and QP and do NOT post any recvs on them.
         * Otherwise this will screwup the recv accounting and will
         * result in not posting recvs when you really really wanted to.
         * All of the dummy cq and qps will be cleaned up on the reject
         * event.
         */
        rc = create_dummy_cq(contents, contents->openib_btl);
        if (0 != rc) {
            BTL_ERROR(("create_dummy_cq error %d", rc));
            goto out;
        }

        rc = create_dummy_qp(contents, context->id, context->qpnum);
        if (0 != rc) {
            BTL_ERROR(("create_dummy_qp error %d", rc));
            goto out;
        }
    }

    memset(&conn_param, 0, sizeof(conn_param));
    /* See above comment about rdma_connect(3) and these two values. */
    conn_param.responder_resources = 
        mymin(contents->openib_btl->device->ib_dev_attr.max_qp_rd_atom,
              message->device_max_qp_init_rd_atom);
    conn_param.initiator_depth = 
        mymin(contents->openib_btl->device->ib_dev_attr.max_qp_init_rd_atom,
              message->device_max_qp_rd_atom);
    conn_param.flow_control = 0;
    conn_param.retry_count = mca_btl_openib_component.ib_retry_count;
    conn_param.rnr_retry_count = BTL_OPENIB_QP_TYPE_PP(context->qpnum) ? 0 :
        mca_btl_openib_component.ib_rnr_retry;
    conn_param.srq = BTL_OPENIB_QP_TYPE_SRQ(context->qpnum);
    conn_param.private_data = &msg;
    conn_param.private_data_len = sizeof(private_data_t);

    msg.qpnum = context->qpnum;
    msg.rem_index = contents->endpoint->index;
    msg.rem_port = contents->tcp_port;
    if (contents->endpoint->endpoint_initiator) {
#if OPAL_ENABLE_DEBUG
        char *a;
#endif
        OPAL_OUTPUT((-1, "Finish connect (I am initiator): sending from %s:%d, TCP port %d, qp index %d (num %d) to IP %s:%d",
                     ibv_get_device_name(contents->openib_btl->device->ib_dev),
                     contents->openib_btl->port_num,
                     contents->tcp_port,
                     context->qpnum,
                     contents->endpoint->qps[context->qpnum].qp->lcl_qp->qp_num,
                     a = stringify(remoteipaddr), remoteport));
#if OPAL_ENABLE_DEBUG
        free(a);
#endif
    }

    /* Now all of the local setup has been done.  The remote system
       should now get a RDMA_CM_EVENT_CONNECT_REQUEST event to further
       the setup of the QP. */
    OPAL_OUTPUT((-1, "SERVICE in finish_connect; ep=%p (%p), I still %s the initiator to %s",
                 (void*) contents->endpoint,
                 (void*) contents->endpoint->endpoint_local_cpc,
                 contents->endpoint->endpoint_initiator ? "am" : "am NOT",
                 contents->endpoint->endpoint_proc->proc_ompi->proc_hostname));
    rc = rdma_connect(context->id, &conn_param);
    if (0 != rc) {
        BTL_ERROR(("rdma_connect Failed with %d", rc));
        goto out1;
    }

    return OMPI_SUCCESS;

out1:
    ibv_destroy_qp(context->id->qp);
out:
    OBJ_RELEASE(contents);

    return OMPI_ERROR;
}

/* 
 * Runs in main thread
 */
static void *show_help_rdmacm_event_error(void *c)
{
    struct rdma_cm_event *event = (struct rdma_cm_event*) c;
    id_context_t *context = (id_context_t*) event->id->context;

    if (RDMA_CM_EVENT_DEVICE_REMOVAL == event->event) {
        orte_show_help("help-mpi-btl-openib-cpc-rdmacm.txt",
                       "rdma cm device removal", true,
                       orte_process_info.nodename,
                       ibv_get_device_name(event->id->verbs->device));
    } else {
        const char *device = "Unknown";
        if (NULL != event->id &&
            NULL != event->id->verbs &&
            NULL != event->id->verbs->device) {
            device = ibv_get_device_name(event->id->verbs->device);
        }
        orte_show_help("help-mpi-btl-openib-cpc-rdmacm.txt",
                       "rdma cm event error", true,
                       orte_process_info.nodename,
                       device,
                       rdma_event_str(event->event),
                       context->endpoint->endpoint_proc->proc_ompi->proc_hostname);
    }

    return NULL;
}

/*
 * Runs in service thread
 */
static int event_handler(struct rdma_cm_event *event)
{
    id_context_t *context = (id_context_t*) event->id->context;
    rdmacm_contents_t *contents;
    struct sockaddr *peeraddr, *localaddr;
    uint32_t peeripaddr, localipaddr;
    int rc = -1, qpnum;
    ompi_btl_openib_ini_values_t ini;
    bool found;

    if (NULL == context) {
        return rc;
    }

    contents = context->contents;
    qpnum = context->qpnum;
    localaddr = rdma_get_local_addr(event->id);
    peeraddr = rdma_get_peer_addr(event->id);
    localipaddr = ((struct sockaddr_in *)localaddr)->sin_addr.s_addr;
    peeripaddr = ((struct sockaddr_in *)peeraddr)->sin_addr.s_addr;

    BTL_VERBOSE(("%s event_handler -- %s, status = %d to %x",
                contents->server?"server":"client",
                rdma_event_str(event->event),
                event->status,
                peeripaddr));

    switch (event->event) {
    case RDMA_CM_EVENT_ADDR_RESOLVED:
        OPAL_OUTPUT((-1, "SERVICE Got ADDR_RESOLVED: ID %p", (void*) context->id));
        rc = resolve_route(context);
        break;

    case RDMA_CM_EVENT_ROUTE_RESOLVED:
        OPAL_OUTPUT((-1, "SERVICE Got ROUTE_RESOLVED: ID %p", (void*) context->id));
        contents->ipaddr = localipaddr;
        rc = finish_connect(context);
        break;

    case RDMA_CM_EVENT_CONNECT_REQUEST:
        OPAL_OUTPUT((-1, "SERVICE Got CONNECT_REQUEST: ID %p, context %p",
                     (void*) event->id, (void*) context));
        rc = handle_connect_request(event);
        break;

    case RDMA_CM_EVENT_ESTABLISHED:
        OPAL_OUTPUT((-1, "SERVICE Got ESTABLISHED: %p", (void*) event->id));
        rc = rdmacm_connect_endpoint(context, event);
        break;

    case RDMA_CM_EVENT_DISCONNECTED:
        OPAL_OUTPUT((-1, "SERVICE Got DISCONNECTED: %p", (void*) event->id));
        rc = rdmacm_disconnected(context);
        break;

    case RDMA_CM_EVENT_REJECTED:
        OPAL_OUTPUT((-1, "SERVICE Got REJECTED: %p", (void*) event->id));
        rc = rdmacm_rejected(context, event);
        break;

    case RDMA_CM_EVENT_CONNECT_ERROR:
        /* Some adapters have broken REJECT behavior; the recipient
           gets a CONNECT_ERROR event instead of the expected REJECTED
           event.  So if we get a CONNECT_ERROR, see if it's on a
           connection that we're expecting a REJECT (i.e., we have a
           dummy_cq setup).  If it is, and if a) the MCA param
           btl_openib_connect_rdmacm_reject_causes_connect_error is
           true, or b) if rdmacm_reject_causes_connect_error set on
           the device INI values, then just treat this CONNECT_ERROR
           as if it were the REJECT. */
        if (NULL != context->contents->dummy_cq) {
            struct ibv_device_attr *attr =
                &(context->endpoint->endpoint_btl->device->ib_dev_attr);
            found = false;
            if (OMPI_SUCCESS == ompi_btl_openib_ini_query(attr->vendor_id,
                                                          attr->vendor_part_id,
                                                          &ini) && 
                ini.rdmacm_reject_causes_connect_error) {
                found = true;
            }
            if (rdmacm_reject_causes_connect_error) {
                found = true;
            }
            
            if (found) {
                OPAL_OUTPUT((-1, "SERVICE Got CONNECT_ERROR, but ignored: %p", (void*) event->id));
                rc = rdmacm_destroy_dummy_qp(context);
                break;
            }
        }

        /* Otherwise, fall through and handle the error as normal */

    case RDMA_CM_EVENT_UNREACHABLE:
    case RDMA_CM_EVENT_CONNECT_RESPONSE:
    case RDMA_CM_EVENT_ADDR_ERROR:
    case RDMA_CM_EVENT_DEVICE_REMOVAL:
        ompi_btl_openib_fd_run_in_main(show_help_rdmacm_event_error, event);
        rc = OMPI_ERROR;
        break;

    case RDMA_CM_EVENT_ROUTE_ERROR:
        /* Route lookup does not necessarily handle retries, and there
           appear to be cases where the subnet manager node can no
           longer handle incoming requests.  The rdma connection
           manager and lower level code doesn't handle retries, so we
           have to. */
	if (context->route_retry_count < rdmacm_resolve_max_retry_count) {
		context->route_retry_count++;
		rc = resolve_route(context);
		break;
	}
        ompi_btl_openib_fd_run_in_main(show_help_rdmacm_event_error, event);
        rc = OMPI_ERROR;
        break;

    default:
        /* Unknown error */
        BTL_ERROR(("Unknown RDMA CM error event_handler: %s, status = %d",
                   rdma_event_str(event->event), event->status));
        rc = OMPI_ERROR;
        break;
    }

    return rc;
}

/*
 * Runs in service thread
 */
static inline void rdmamcm_event_error(struct rdma_cm_event *event)
{
    mca_btl_base_endpoint_t *endpoint = NULL;

    if (event->id->context) {
        endpoint = ((id_context_t *)event->id->context)->contents->endpoint;
    }

    ompi_btl_openib_fd_run_in_main(mca_btl_openib_endpoint_invoke_error, 
                                   endpoint);
}

/*
 * Runs in service thread
 */
static void *rdmacm_event_dispatch(int fd, int flags, void *context)
{
    struct rdma_cm_event *event, ecopy;
    void *data = NULL;
    int rc;

    /* blocks until next cm_event */
    rc = rdma_get_cm_event(event_channel, &event);
    if (0 != rc) {
        BTL_ERROR(("rdma_get_cm_event error %d", rc));
        return NULL;
    }

    /* If the incoming event is not acked in a sufficient amount of
     * time, there will be a timeout error and the connection will be
     * torndown.  Also, the act of acking the event destroys the
     * included data in the event.  In certain circumstances, the time
     * it takes to handle a incoming event could approach or exceed
     * this time.  To prevent this from happening, we will copy the
     * event and all of its data, ack the event, and process the copy
     * of the event.
     */
    memcpy(&ecopy, event, sizeof(struct rdma_cm_event));
    if (event->param.conn.private_data_len > 0) {
        data = malloc(event->param.conn.private_data_len);
        if (NULL == data) {
           BTL_ERROR(("error mallocing memory"));
           return NULL;
        }
        memcpy(data, event->param.conn.private_data, event->param.conn.private_data_len);
        ecopy.param.conn.private_data = data;
    }
    rdma_ack_cm_event(event);

    rc = event_handler(&ecopy);
    if (OMPI_SUCCESS != rc) {
        rdmamcm_event_error(&ecopy);
    }

    if (NULL != data) {
        free(data);
    }

    return NULL;
}

/*
 * Runs in main thread
 *
 * CPC init function - Setup all globals here 
 */
static int rdmacm_init(mca_btl_openib_endpoint_t *endpoint)
{
    void *data;

    data = calloc(1, sizeof(rdmacm_endpoint_local_cpc_data_t));
    if (NULL == data) {
        BTL_ERROR(("malloc failed"));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    endpoint->endpoint_local_cpc_data = data;

    return OMPI_SUCCESS;
}

static int ipaddrcheck(id_context_t *context,
                       mca_btl_openib_module_t *openib_btl)
{
    rdmacm_contents_t *server = context->contents;
    uint32_t ipaddr;
    bool already_exists = false;
    opal_list_item_t *item;
    int server_tcp_port = rdma_get_src_port(context->id);
    char *str;

    /* Look up the IP address of this device/port.  This call should not be
     * necessary, as rdma_get_local_addr would be more correct in returning the
     * IP address given the cm_id (and not necessitate having to do a list look
     * up).  Unfortunately, the subnet and IP address look up needs to match or
     * there could be a mismatch if IP Aliases are being used.  For more
     * information on this, please read comment above
     * mca_btl_openib_get_ip_subnet_id in btl_openib_ip.c 
     */
    ipaddr = 
        mca_btl_openib_rdma_get_ipv4addr(openib_btl->device->ib_dev_context, 
                                         openib_btl->port_num);
    if (0 == ipaddr) {
        BTL_VERBOSE(("*** Could not find IP address for %s:%d -- is there an IP address configured for this device?",
                     ibv_get_device_name(openib_btl->device->ib_dev), 
                     openib_btl->port_num));
        return OMPI_ERR_NOT_FOUND;
    }
    str = stringify(ipaddr);
    BTL_VERBOSE(("Found device %s:%d = IP address %s:%d",
                 ibv_get_device_name(openib_btl->device->ib_dev),
                 openib_btl->port_num, str, server_tcp_port));
    free(str);

    /* Ok, we found the IP address of this device/port.  Have we
       already see this IP address/TCP port before? */
    for (item = opal_list_get_first(&server_listener_list); 
         item != opal_list_get_end(&server_listener_list); 
         item = opal_list_get_next(item)) {
        rdmacm_contents_t *contents = (rdmacm_contents_t *)item;
        BTL_VERBOSE(("paddr = %x, ipaddr addr = %x", 
                     contents->ipaddr, ipaddr));
        if (contents->ipaddr == ipaddr &&
            contents->tcp_port == server_tcp_port) {
            str = stringify(ipaddr);
            BTL_VERBOSE(("server already listening on %s:%d", 
                         str, server_tcp_port));
            free(str);
            already_exists = true;
            break;
        }
    }

    /* If we haven't seen it before, save it */
    if (!already_exists) {
        str = stringify(ipaddr);
        BTL_VERBOSE(("creating new server to listen on %s:%d", 
                     str, server_tcp_port));
        free(str);
        server->ipaddr = ipaddr;
        server->tcp_port = server_tcp_port;
    }

    return already_exists ? OMPI_ERROR : OMPI_SUCCESS;
}

static int create_message(rdmacm_contents_t *server, 
                          mca_btl_openib_module_t *openib_btl, 
                          ompi_btl_openib_connect_base_module_data_t *data)
{
    modex_message_t *message;
#if OPAL_ENABLE_DEBUG
    char *a;
#endif

    message = malloc(sizeof(modex_message_t));
    if (NULL == message) {
        BTL_ERROR(("malloc failed"));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    message->device_max_qp_rd_atom = 
        openib_btl->device->ib_dev_attr.max_qp_rd_atom;
    message->device_max_qp_init_rd_atom = 
        openib_btl->device->ib_dev_attr.max_qp_init_rd_atom;
    message->ipaddr = server->ipaddr;
    message->tcp_port = server->tcp_port;

    OPAL_OUTPUT((-1, "Message IP address is %s, port %d", 
                 a = stringify(message->ipaddr), message->tcp_port));
#if OPAL_ENABLE_DEBUG
    free(a);
#endif
    data->cbm_modex_message = message;
    data->cbm_modex_message_len = message_len;

    return OMPI_SUCCESS;
}

/* 
 * Runs in main thread
 *
 * This function determines if the RDMACM is a possible cpc method and
 * sets it up accordingly.
 */
static int rdmacm_component_query(mca_btl_openib_module_t *openib_btl, ompi_btl_openib_connect_base_module_t **cpc)
{
    rdmacm_contents_t *server = NULL;
    struct sockaddr_in sin;
    id_context_t *context;
    int rc;

    /* RDMACM is not supported if we have any XRC QPs */
    if (mca_btl_openib_component.num_xrc_qps > 0) {
        BTL_VERBOSE(("rdmacm CPC not supported with XRC receive queues, please try xoob CPC; skipped on %s:%d",
                     ibv_get_device_name(openib_btl->device->ib_dev),
                     openib_btl->port_num));
        rc = OMPI_ERR_NOT_SUPPORTED;
        goto out;
    }

    BTL_VERBOSE(("rdmacm_component_query"));

    *cpc = malloc(sizeof(ompi_btl_openib_connect_base_module_t));
    if (NULL == *cpc) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto out;
    }

    (*cpc)->data.cbm_component = &ompi_btl_openib_connect_rdmacm;
    (*cpc)->data.cbm_priority = rdmacm_priority;
    (*cpc)->cbm_endpoint_init = rdmacm_init;
    (*cpc)->cbm_start_connect = rdmacm_module_start_connect;
    (*cpc)->cbm_endpoint_finalize = rdmacm_endpoint_finalize;
    (*cpc)->cbm_finalize = NULL;
    /* Setting uses_cts=true also guarantees that we'll only be
       selected if QP 0 is PP */
    (*cpc)->cbm_uses_cts = true;

    server = OBJ_NEW(rdmacm_contents_t);
    if (NULL == server) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto out1;
    }
    server->server = true;
    server->openib_btl = openib_btl;

    context = OBJ_NEW(id_context_t);
    OPAL_OUTPUT((-1, "MAIN Server context: %p", (void*) context));
    if (NULL == context) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC system error (malloc failed)");
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto out3;
    }
    context->contents = server;
    OBJ_RETAIN(context->contents);
    opal_list_append(&(server->ids), &(context->super));
    context->qpnum = 0;

    rc = rdma_create_id(event_channel, &(context->id), context, RDMA_PS_TCP);
    if (0 != rc) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC failed to create ID");
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto out4;
    }

    memset(&sin, 0, sizeof(sin));
    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = rdmacm_addr;
    sin.sin_port = rdmacm_port;

    /* Bind the rdmacm server to the local IP address and an ephemerial
     * port or one specified by a comand arg.
     */
    rc = rdma_bind_addr(context->id, (struct sockaddr *)&sin);
    if (0 != rc) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC unable to bind to address");
        rc = OMPI_ERR_UNREACH;
        goto out5;
    }

    /* Verify that the device has a valid IP address on it, or we
       cannot use the cpc */
    rc = ipaddrcheck(context, openib_btl);
    if (OMPI_SUCCESS != rc) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm IP address not found on port");
        rc = OMPI_ERR_NOT_SUPPORTED;
        goto out5;
    }

    /* Listen on the specified address/port with the rdmacm, limit the
       amount of incoming connections to 1024 */
    /* FIXME - 1024 should be (num of connectors *
       mca_btl_openib_component.num_qps) */
    rc = rdma_listen(context->id, 1024);
    if (0 != rc) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC unable to listen");
        rc = OMPI_ERR_UNREACH;
        goto out5;
    }

    rc = create_message(server, openib_btl, &(*cpc)->data);
    if (0 != rc) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC unable to create message");
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto out5;
    }

    opal_list_append(&server_listener_list, &(server->super));

    opal_output_verbose(5, mca_btl_base_output,
                        "openib BTL: rdmacm CPC available for use on %s:%d",
                        ibv_get_device_name(openib_btl->device->ib_dev),
                        openib_btl->port_num);
    return OMPI_SUCCESS;

out5:
    /*
     * Since rdma_create_id() succeeded, we need "rdma_destroy_id(context->id)".
     * But don't do it here since it's part of out4:OBJ_RELEASE(context),
     * and we don't want to do it twice.
     */
out4:
    opal_list_remove_first(&(server->ids));
    OBJ_RELEASE(context);
out3:
    OBJ_RELEASE(server);
out1:
    free(*cpc);
out:
    if (OMPI_ERR_NOT_SUPPORTED == rc) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC unavailable for use on %s:%d; skipped",
                            ibv_get_device_name(openib_btl->device->ib_dev),
                            openib_btl->port_num);
    } else {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rmacm CPC unavailable for use on %s:%d; fatal error %d (%s)",
                            ibv_get_device_name(openib_btl->device->ib_dev), 
                            openib_btl->port_num, rc,
                            opal_strerror(rc));
    }
    return rc;
}

/*
 * Invoked by main thread
 *
 * Shutting down the whole thing.
 */
static int rdmacm_component_finalize(void)
{
    volatile int barrier = 0;
    opal_list_item_t *item, *item2;
    int rc;

    BTL_VERBOSE(("rdmacm_component_finalize"));

    /* If we're just trolling through ompi_info, don't bother doing
       anything */
    if (!rdmacm_component_initialized) {
        return OMPI_SUCCESS;
    }

    if (NULL != event_channel) {
        rc = ompi_btl_openib_fd_unmonitor(event_channel->fd, 
                                          rdmacm_unmonitor, (void*) &barrier);
        if (OMPI_SUCCESS != rc) {
            BTL_ERROR(("Error disabling fd monitor"));
        }

        /* Wait for the service thread to stop monitoring the fd */
        OPAL_OUTPUT((-1, "MAIN rdmacm_component_finalize: waiting for thread to finish"));
        while (0 == barrier) {
            sched_yield();
        }
        OPAL_OUTPUT((-1, "MAIN rdmacm_component_finalize: thread finished"));
    }

    /* The service thread is no longer running; no need to lock access
       to the client_list */
    for (item = opal_list_remove_first(&client_list);
         NULL != item;
         item = opal_list_remove_first(&client_list)) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&client_list);

    /* For each of the items in the server list, there's only one item
       in the "ids" list -- the server listener.  So explicitly
       destroy its RDMA ID context. */
    for (item = opal_list_remove_first(&server_listener_list);
         NULL != item;
         item = opal_list_remove_first(&server_listener_list)) {
        rdmacm_contents_t *contents = (rdmacm_contents_t*) item;
        item2 = opal_list_remove_first(&(contents->ids));
        OBJ_RELEASE(item2);
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&server_listener_list);

    /* Now we're all done -- destroy the event channel */
    if (NULL != event_channel) {
        rdma_destroy_event_channel(event_channel);
        event_channel = NULL;
    }

    mca_btl_openib_free_rdma_addr_list();

    return OMPI_SUCCESS;
}

static int rdmacm_component_init(void)
{
    int rc;

    OBJ_CONSTRUCT(&server_listener_list, opal_list_t);
    OBJ_CONSTRUCT(&client_list, opal_list_t);
    OBJ_CONSTRUCT(&client_list_lock, opal_mutex_t);

    rc = mca_btl_openib_build_rdma_addr_list();
    if (OMPI_SUCCESS != rc) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC unable to find any valid IP address");
        return OMPI_ERR_NOT_SUPPORTED;
    }

    event_channel = rdma_create_event_channel();
    if (NULL == event_channel) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC failed to create channel");
        return OMPI_ERR_UNREACH;
    }

    /* Start monitoring the fd associated with the cm_device */
    ompi_btl_openib_fd_monitor(event_channel->fd, OPAL_EV_READ,
                               rdmacm_event_dispatch, NULL);
    
    rdmacm_component_initialized = true;
    return OMPI_SUCCESS;
}
