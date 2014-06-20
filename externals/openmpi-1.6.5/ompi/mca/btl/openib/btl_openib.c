/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2009 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 * Copyright (c) 2008-2012 Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2009      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include <string.h>
#include <inttypes.h>
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/base/base.h"
#include "opal/class/opal_bitmap.h"
#include "opal/util/output.h"
#include "opal/util/arch.h"

#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/btl_base_error.h"

#if OPAL_ENABLE_FT_CR == 1
#include "ompi/runtime/ompi_cr.h"
#endif

#include "btl_openib_ini.h"

#include "btl_openib.h"
#include "btl_openib_frag.h"
#include "btl_openib_proc.h"
#include "btl_openib_endpoint.h"
#include "btl_openib_xrc.h"
#include "opal/datatype/opal_convertor.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/mpool/mpool.h"
#include "ompi/mca/mpool/rdma/mpool_rdma.h"
#include "orte/util/proc_info.h"
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <math.h>
#include <inttypes.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif
#include <unistd.h>
#ifdef OPAL_HAVE_HWLOC
#include "opal/mca/hwloc/hwloc.h"
#endif

#ifndef MIN
#define MIN(a,b) ((a)<(b)?(a):(b))
#endif

mca_btl_openib_module_t mca_btl_openib_module = {
    {
        &mca_btl_openib_component.super,
        0, /* max size of first fragment */
        0, /* min send fragment size */
        0, /* max send fragment size */
        0, /* btl_rdma_pipeline_send_length */
        0, /* btl_rdma_pipeline_frag_size */
        0, /* btl_min_rdma_pipeline_size */
        0, /* exclusivity */
        0, /* latency */
        0, /* bandwidth */
        0, /* TODO this should be PUT btl flags */
        mca_btl_openib_add_procs,
        mca_btl_openib_del_procs,
        NULL,
        mca_btl_openib_finalize,
        /* we need alloc free, pack */
        mca_btl_openib_alloc,
        mca_btl_openib_free,
        mca_btl_openib_prepare_src,
        mca_btl_openib_prepare_dst,
        mca_btl_openib_send,
        mca_btl_openib_sendi, /* send immediate */
        mca_btl_openib_put,
        mca_btl_openib_get,
        mca_btl_base_dump,
        NULL, /* mpool */
        mca_btl_openib_register_error_cb, /* error call back registration */
        mca_btl_openib_ft_event
    }
};

char* const mca_btl_openib_transport_name_strings[MCA_BTL_OPENIB_TRANSPORT_SIZE] = {
    "MCA_BTL_OPENIB_TRANSPORT_IB",
    "MCA_BTL_OPENIB_TRANSPORT_IWARP",
    "MCA_BTL_OPENIB_TRANSPORT_RDMAOE",
    "MCA_BTL_OPENIB_TRANSPORT_UNKNOWN"
};

static int mca_btl_openib_finalize_resources(struct mca_btl_base_module_t* btl);

void mca_btl_openib_show_init_error(const char *file, int line,
                                    const char *func, const char *dev)
{
    if (ENOMEM == errno) {
        int ret;
        struct rlimit limit;
        char *str_limit = NULL;

#if HAVE_DECL_RLIMIT_MEMLOCK
        ret = getrlimit(RLIMIT_MEMLOCK, &limit);
#else
        ret = -1;
#endif
        if (0 != ret) {
            asprintf(&str_limit, "Unknown");
        } else if (limit.rlim_cur == RLIM_INFINITY) {
            asprintf(&str_limit, "unlimited");
        } else {
            asprintf(&str_limit, "%ld", (long)limit.rlim_cur);
        }

        orte_show_help("help-mpi-btl-openib.txt", "init-fail-no-mem",
                       true, orte_process_info.nodename,
                       file, line, func, dev, str_limit);

        if (NULL != str_limit) free(str_limit);
    } else {
        orte_show_help("help-mpi-btl-openib.txt", "init-fail-create-q",
                       true, orte_process_info.nodename,
                       file, line, func, strerror(errno), errno, dev);
    }
}

static inline struct ibv_cq *ibv_create_cq_compat(struct ibv_context *context,
        int cqe, void *cq_context, struct ibv_comp_channel *channel,
        int comp_vector)
{
#if OMPI_IBV_CREATE_CQ_ARGS == 3
    return ibv_create_cq(context, cqe, channel);
#else
    return ibv_create_cq(context, cqe, cq_context, channel, comp_vector);
#endif
}

static int adjust_cq(mca_btl_openib_device_t *device, const int cq)
{
    uint32_t cq_size = device->cq_size[cq];

    /* make sure we don't exceed the maximum CQ size and that we
     * don't size the queue smaller than otherwise requested
     */
     if(cq_size < mca_btl_openib_component.ib_cq_size[cq])
        cq_size = mca_btl_openib_component.ib_cq_size[cq];

    if(cq_size > (uint32_t)device->ib_dev_attr.max_cqe)
        cq_size = device->ib_dev_attr.max_cqe;

    if(NULL == device->ib_cq[cq]) {
        device->ib_cq[cq] = ibv_create_cq_compat(device->ib_dev_context, cq_size,
#if OPAL_ENABLE_PROGRESS_THREADS == 1
                device, device->ib_channel,
#else
                NULL, NULL,
#endif
                0);

        if (NULL == device->ib_cq[cq]) {
            mca_btl_openib_show_init_error(__FILE__, __LINE__, "ibv_create_cq",
                                           ibv_get_device_name(device->ib_dev));
            return OMPI_ERROR;
        }

#if OPAL_ENABLE_PROGRESS_THREADS == 1
        if(ibv_req_notify_cq(device->ib_cq[cq], 0)) {
            mca_btl_openib_show_init_error(__FILE__, __LINE__,
                                           "ibv_req_notify_cq",
                                           ibv_get_device_name(device->ib_dev));
            return OMPI_ERROR;
        }

        OPAL_THREAD_LOCK(&device->device_lock);
        if (!device->progress) {
            int rc;
            device->progress = true;
            if(OPAL_SUCCESS != (rc = opal_thread_start(&device->thread))) {
                BTL_ERROR(("Unable to create progress thread, retval=%d", rc));
                return rc;
            }
        }
        OPAL_THREAD_UNLOCK(&device->device_lock);
#endif
    }
#ifdef HAVE_IBV_RESIZE_CQ
    else if (cq_size > mca_btl_openib_component.ib_cq_size[cq]){
        int rc;
        rc = ibv_resize_cq(device->ib_cq[cq], cq_size);
        /* For ConnectX the resize CQ is not implemented and verbs returns -ENOSYS
         * but should return ENOSYS. So it is reason for abs */
        if(rc && ENOSYS != abs(rc)) {
            BTL_ERROR(("cannot resize completion queue, error: %d", rc));
            return OMPI_ERROR;
        }
    }
#endif

    return OMPI_SUCCESS;
}


/* In this function we check if the device supports srq limit
   event. We create the temporary srq, post some receive buffers - in
   order to prevent srq limit event immediately and call the
   "ibv_modify_srq" function. If a return value of the function not
   success => our decision that the device doesn't support this
   capability. */
static int check_if_device_support_modify_srq(mca_btl_openib_module_t *openib_btl)
{
    char buff;
    int rc = OMPI_SUCCESS;

    struct ibv_srq* dummy_srq = NULL;
    struct ibv_srq_attr modify_attr;

    struct ibv_sge sge_elem;
    struct ibv_recv_wr wr1, wr2, *bad_wr;

    struct ibv_srq_init_attr init_attr;
    memset(&init_attr, 0, sizeof(struct ibv_srq_init_attr));

    init_attr.attr.max_wr = 3;
    init_attr.attr.max_sge = 1;

    dummy_srq = ibv_create_srq(openib_btl->device->ib_pd, &init_attr);
    if(NULL == dummy_srq) {
        rc = OMPI_ERROR;
        return rc;
    }

    sge_elem.addr = (uint64_t) &buff;
    sge_elem.length = sizeof(buff);

    wr1.num_sge = wr2.num_sge = 1;
    wr1.sg_list = wr2.sg_list = &sge_elem;

    wr1.next = &wr2;
    wr2.next = NULL;

    if(ibv_post_srq_recv(dummy_srq, &wr1, &bad_wr)) {
        rc = OMPI_ERROR;
        goto destroy_dummy_srq;
    }

    modify_attr.max_wr = 2;
    modify_attr.max_sge = 1;
    modify_attr.srq_limit = 1;

    if(ibv_modify_srq(dummy_srq, &modify_attr, IBV_SRQ_LIMIT)) {
        rc = OMPI_ERR_NOT_SUPPORTED;
        goto destroy_dummy_srq;
    }

destroy_dummy_srq:
    if(ibv_destroy_srq(dummy_srq)) {
        rc = OMPI_ERROR;
    }

    return rc;
}

/*
 * create both the high and low priority completion queues
 * and the shared receive queue (if requested)
 */
static int create_srq(mca_btl_openib_module_t *openib_btl)
{
    int qp, rc = 0;
    int32_t rd_num, rd_curr_num;

    bool device_support_modify_srq = true;

    /* Check if our device supports modify srq ability */
    rc = check_if_device_support_modify_srq(openib_btl);
    if(OMPI_ERR_NOT_SUPPORTED == rc) {
        device_support_modify_srq = false;
    } else if(OMPI_SUCCESS != rc) {
        mca_btl_openib_show_init_error(__FILE__, __LINE__,
                    "ibv_create_srq",
                    ibv_get_device_name(openib_btl->device->ib_dev));
        return rc;
    }

    /* create the SRQ's */
    for(qp = 0; qp < mca_btl_openib_component.num_qps; qp++) {
        struct ibv_srq_init_attr attr;
        memset(&attr, 0, sizeof(struct ibv_srq_init_attr));

        if(!BTL_OPENIB_QP_TYPE_PP(qp)) {
            attr.attr.max_wr = mca_btl_openib_component.qp_infos[qp].rd_num +
                mca_btl_openib_component.qp_infos[qp].u.srq_qp.sd_max;
            attr.attr.max_sge = 1;
            openib_btl->qps[qp].u.srq_qp.rd_posted = 0;
#if HAVE_XRC
            if(BTL_OPENIB_QP_TYPE_XRC(qp)) {
                openib_btl->qps[qp].u.srq_qp.srq =
                    ibv_create_xrc_srq(openib_btl->device->ib_pd,
                            openib_btl->device->xrc_domain,
                            openib_btl->device->ib_cq[qp_cq_prio(qp)], &attr);
            } else
#endif
            {
               openib_btl->qps[qp].u.srq_qp.srq =
                   ibv_create_srq(openib_btl->device->ib_pd, &attr);
            }
            if (NULL == openib_btl->qps[qp].u.srq_qp.srq) {
                mca_btl_openib_show_init_error(__FILE__, __LINE__,
                                               "ibv_create_srq",
                                               ibv_get_device_name(openib_btl->device->ib_dev));
                return OMPI_ERROR;
            }

#if OPAL_HAVE_THREADS
            {
                opal_mutex_t *lock = &mca_btl_openib_component.srq_manager.lock;
                opal_hash_table_t *srq_addr_table = &mca_btl_openib_component.srq_manager.srq_addr_table;

                opal_mutex_lock(lock);
                if (OPAL_SUCCESS != opal_hash_table_set_value_ptr(
                                srq_addr_table, &openib_btl->qps[qp].u.srq_qp.srq,
                                sizeof(struct ibv_srq*), (void*) openib_btl)) {
                    BTL_ERROR(("SRQ Internal error."
                            " Failed to add element to mca_btl_openib_component.srq_manager.srq_addr_table\n"));

                    opal_mutex_unlock(lock);
                    return OMPI_ERROR;
                }
                opal_mutex_unlock(lock);
            }
#endif
            rd_num = mca_btl_openib_component.qp_infos[qp].rd_num;
            rd_curr_num = openib_btl->qps[qp].u.srq_qp.rd_curr_num = mca_btl_openib_component.qp_infos[qp].u.srq_qp.rd_init;

            if(true == mca_btl_openib_component.enable_srq_resize &&
                                    true == device_support_modify_srq) {
                if(0 == rd_curr_num) {
                    openib_btl->qps[qp].u.srq_qp.rd_curr_num = 1;
                }

                openib_btl->qps[qp].u.srq_qp.rd_low_local = rd_curr_num - (rd_curr_num >> 2);
                openib_btl->qps[qp].u.srq_qp.srq_limit_event_flag = true;
            } else {
                openib_btl->qps[qp].u.srq_qp.rd_curr_num = rd_num;
                openib_btl->qps[qp].u.srq_qp.rd_low_local = mca_btl_openib_component.qp_infos[qp].rd_low;
                /* Not used in this case, but we don't need a garbage */
                mca_btl_openib_component.qp_infos[qp].u.srq_qp.srq_limit = 0;
                openib_btl->qps[qp].u.srq_qp.srq_limit_event_flag = false;
            }
        }
    }

    return OMPI_SUCCESS;
}

static int mca_btl_openib_size_queues(struct mca_btl_openib_module_t* openib_btl, size_t nprocs)
{
    uint32_t send_cqes, recv_cqes;
    int rc = OMPI_SUCCESS, qp;
    mca_btl_openib_device_t *device = openib_btl->device;

    /* figure out reasonable sizes for completion queues */
    for(qp = 0; qp < mca_btl_openib_component.num_qps; qp++) {
        if(BTL_OPENIB_QP_TYPE_SRQ(qp)) {
            send_cqes = mca_btl_openib_component.qp_infos[qp].u.srq_qp.sd_max;
            recv_cqes = mca_btl_openib_component.qp_infos[qp].rd_num;
        } else {
            send_cqes = (mca_btl_openib_component.qp_infos[qp].rd_num +
                mca_btl_openib_component.qp_infos[qp].u.pp_qp.rd_rsv) * nprocs;
            recv_cqes = send_cqes;
        }
        openib_btl->device->cq_size[qp_cq_prio(qp)] += recv_cqes;
        openib_btl->device->cq_size[BTL_OPENIB_LP_CQ] += send_cqes;
    }

    rc = adjust_cq(device, BTL_OPENIB_HP_CQ);
    if (OMPI_SUCCESS != rc) {
        goto out;
    }

    rc = adjust_cq(device, BTL_OPENIB_LP_CQ);
    if (OMPI_SUCCESS != rc) {
        goto out;
    }

    if (0 == openib_btl->num_peers && 
            (mca_btl_openib_component.num_srq_qps > 0 || 
             mca_btl_openib_component.num_xrc_qps > 0)) {
        rc = create_srq(openib_btl);
    }

    openib_btl->num_peers += nprocs;
out:
    return rc;
}

mca_btl_openib_transport_type_t mca_btl_openib_get_transport_type(mca_btl_openib_module_t* openib_btl)
{
/* If we have a driver with RDMAoE supporting as the device struct contains the same type (IB) for 
   IBV_LINK_LAYER_INFINIBAND and IBV_LINK_LAYER_ETHERNET link layers and the single way
   to detect this fact is to check their link_layer fields in a port_attr struct.
   If our driver doesn't support this feature => the checking of transport type in device struct will be enough.
   If the driver doesn't support completely transport types =>
   our assumption that it is very old driver - that supports IB devices only */

#ifdef HAVE_STRUCT_IBV_DEVICE_TRANSPORT_TYPE
    switch(openib_btl->device->ib_dev->transport_type) {
        case IBV_TRANSPORT_IB:
#ifdef OMPI_HAVE_RDMAOE
            switch(openib_btl->ib_port_attr.link_layer) {
                case IBV_LINK_LAYER_ETHERNET:
                    return MCA_BTL_OPENIB_TRANSPORT_RDMAOE;

                case IBV_LINK_LAYER_INFINIBAND:
                    return MCA_BTL_OPENIB_TRANSPORT_IB;
            /* It is not possible that a device struct contains
               IB transport and port was configured to IBV_LINK_LAYER_UNSPECIFIED */
                case IBV_LINK_LAYER_UNSPECIFIED:
                default:
                    return MCA_BTL_OPENIB_TRANSPORT_UNKNOWN;
            }
#endif
            return MCA_BTL_OPENIB_TRANSPORT_IB;

        case IBV_TRANSPORT_IWARP:
            return MCA_BTL_OPENIB_TRANSPORT_IWARP;

        case IBV_TRANSPORT_UNKNOWN:		 
        default:
            return MCA_BTL_OPENIB_TRANSPORT_UNKNOWN;
    }
#else
    return MCA_BTL_OPENIB_TRANSPORT_IB;
#endif
}

static int mca_btl_openib_tune_endpoint(mca_btl_openib_module_t* openib_btl, 
                                            mca_btl_base_endpoint_t* endpoint)
{
    int ret = OMPI_SUCCESS;

    char* recv_qps = NULL;

    ompi_btl_openib_ini_values_t values;

    if(mca_btl_openib_get_transport_type(openib_btl) != endpoint->rem_info.rem_transport_type) {
        orte_show_help("help-mpi-btl-openib.txt",
                "conflicting transport types", true,
                orte_process_info.nodename,
                        ibv_get_device_name(openib_btl->device->ib_dev),
                        (openib_btl->device->ib_dev_attr).vendor_id,
                        (openib_btl->device->ib_dev_attr).vendor_part_id,
                        mca_btl_openib_transport_name_strings[mca_btl_openib_get_transport_type(openib_btl)],
                        endpoint->endpoint_proc->proc_ompi->proc_hostname,
                        endpoint->rem_info.rem_vendor_id,
                        endpoint->rem_info.rem_vendor_part_id,
                        mca_btl_openib_transport_name_strings[endpoint->rem_info.rem_transport_type]);
    
        return OMPI_ERROR;
    }

    memset(&values, 0, sizeof(ompi_btl_openib_ini_values_t));
    ret = ompi_btl_openib_ini_query(endpoint->rem_info.rem_vendor_id,
                          endpoint->rem_info.rem_vendor_part_id, &values);

    if (OMPI_SUCCESS != ret && OMPI_ERR_NOT_FOUND != ret) {
        orte_show_help("help-mpi-btl-openib.txt",
                       "error in device init", true,
                       orte_process_info.nodename,
                       ibv_get_device_name(openib_btl->device->ib_dev));
        return ret;
    }

    if(openib_btl->device->mtu < endpoint->rem_info.rem_mtu) {
        endpoint->rem_info.rem_mtu = openib_btl->device->mtu; 
    }

    endpoint->use_eager_rdma = openib_btl->device->use_eager_rdma &
                               endpoint->use_eager_rdma;

    /* Receive queues checking */

    /* In this check we assume that the command line or INI file parameters are the same
       for all processes on all machines. The assumption is correct for 99.9999% of users,
       if a user distributes different INI files or parameters for different node/procs,
       it is on his own responsibility */
    switch(mca_btl_openib_component.receive_queues_source) {
        case BTL_OPENIB_RQ_SOURCE_MCA:
        case BTL_OPENIB_RQ_SOURCE_MAX:
            break;

        /* If the queues configuration was set from command line 
           (with --mca btl_openib_receive_queues parameter) => both sides have a same configuration */

        /* In this case the local queues configuration was gotten from INI file =>
           not possible that remote side got its queues configuration from command line => 
           (by prio) the configuration was set from INI file or (if not configure)
           by default queues configuration */
        case BTL_OPENIB_RQ_SOURCE_DEVICE_INI:
            if(NULL != values.receive_queues) {
                recv_qps = values.receive_queues;
            } else {
                recv_qps = mca_btl_openib_component.default_recv_qps;
            }

            if(0 != strcmp(mca_btl_openib_component.receive_queues,
                                                         recv_qps)) {
                orte_show_help("help-mpi-btl-openib.txt",
                               "unsupported queues configuration", true,
                               orte_process_info.nodename,
                               ibv_get_device_name(openib_btl->device->ib_dev),
                               (openib_btl->device->ib_dev_attr).vendor_id,
                               (openib_btl->device->ib_dev_attr).vendor_part_id,
                               mca_btl_openib_component.receive_queues,
                               endpoint->endpoint_proc->proc_ompi->proc_hostname,
                               endpoint->rem_info.rem_vendor_id,
                               endpoint->rem_info.rem_vendor_part_id,
                               recv_qps);

                return OMPI_ERROR;
            }
            break;

        /* If the local queues configuration was set 
           by default queues => check all possible cases for remote side and compare */
        case  BTL_OPENIB_RQ_SOURCE_DEFAULT:
            if(NULL != values.receive_queues) {
                if(0 != strcmp(mca_btl_openib_component.receive_queues,
                                                values.receive_queues)) {
                     orte_show_help("help-mpi-btl-openib.txt",
                               "unsupported queues configuration", true,
                               orte_process_info.nodename,
                               ibv_get_device_name(openib_btl->device->ib_dev),
                               (openib_btl->device->ib_dev_attr).vendor_id,
                               (openib_btl->device->ib_dev_attr).vendor_part_id,
                               mca_btl_openib_component.receive_queues,
                               endpoint->endpoint_proc->proc_ompi->proc_hostname,
                               endpoint->rem_info.rem_vendor_id,
                               endpoint->rem_info.rem_vendor_part_id,
                               values.receive_queues);

                    return OMPI_ERROR;
                }
            }
            break;
    }

    return OMPI_SUCCESS;
}

/* read a single integer from a linux module parameters file */
static uint64_t read_module_param(char *file, uint64_t value)
{
    int fd = open(file, O_RDONLY);
    char buffer[64];
    uint64_t ret;

    if (0 > fd) {
        return value;
    }

    read (fd, buffer, 64);

    close (fd);

    errno = 0;
    ret = strtoull(buffer, NULL, 10);

    return (0 == errno) ? ret : value;
}

/* calculate memory registation limits */
static uint64_t calculate_total_mem (void)
{
#if OPAL_HAVE_HWLOC
    hwloc_obj_t machine;

    machine = hwloc_get_next_obj_by_type (opal_hwloc_topology, HWLOC_OBJ_MACHINE, NULL);
    if (NULL == machine) {
        return 0;
    }
    
    return machine->memory.total_memory;
#else
    return 0;
#endif
}

static uint64_t calculate_max_reg (void) 
{
    struct stat statinfo;
    uint64_t mtts_per_seg = 1;
    uint64_t num_mtt = 1 << 19;
    uint64_t reserved_mtt = 0;
    uint64_t max_reg, mem_total;

    mem_total = calculate_total_mem ();

    if (0 == stat("/sys/module/mlx5_core", &statinfo)) {
        /* mlx5 means that we have ofed 2.0 and it can always register 2xmem_total for any mlx hca */
        max_reg = 2 * mem_total;
    }
    else if (0 == stat("/sys/module/mlx4_core/parameters", &statinfo)) {
        mtts_per_seg = 1 << read_module_param("/sys/module/mlx4_core/parameters/log_mtts_per_seg", 1);
        num_mtt = 1 << read_module_param("/sys/module/mlx4_core/parameters/log_num_mtt", 20);
        if (1 == num_mtt) {
            /* NTH: is 19 a minimum? when log_num_mtt is set to 0 use 19 */
            num_mtt = 1 << 20;
        }

        max_reg = (num_mtt - reserved_mtt) * getpagesize () * mtts_per_seg;
    } else if (0 == stat("/sys/module/ib_mthca/parameters", &statinfo)) {
        mtts_per_seg = 1 << read_module_param("/sys/module/ib_mthca/parameters/log_mtts_per_seg", 1);
        num_mtt = read_module_param("/sys/module/ib_mthca/parameters/num_mtt", 1 << 20);
        reserved_mtt = read_module_param("/sys/module/ib_mthca/parameters/fmr_reserved_mtts", 0);

        max_reg = (num_mtt - reserved_mtt) * getpagesize () * mtts_per_seg;
    } else {
        /* Need to update to determine the registration limit for this
           configuration */
        max_reg = mem_total;
    }

    /* Print a warning if we can't register more than 75% of physical
       memory.  Abort if the abort_not_enough_reg_mem MCA param was
       set. */
    if (max_reg < mem_total * 3 / 4) {
        char *action;

        if (mca_btl_openib_component.abort_not_enough_reg_mem) {
            action = "Your MPI job will now abort.";
        } else {
            action = "Your MPI job will continue, but may be behave poorly and/or hang.";
        }
        orte_show_help("help-mpi-btl-openib.txt", "reg mem limit low", true,
                       orte_process_info.nodename, (unsigned long)(max_reg >> 20),
                       (unsigned long)(mem_total >> 20), action);
        if (mca_btl_openib_component.abort_not_enough_reg_mem) {
            orte_errmgr.abort(1, NULL);
        }
    }

    /* Limit us to 87.5% of the registered memory (some fluff for QPs,
       file systems, etc) */
    return (max_reg * 7) >> 3;
}


/*
 *  add a proc to this btl module
 *    creates an endpoint that is setup on the
 *    first send to the endpoint
 */
int mca_btl_openib_add_procs(
    struct mca_btl_base_module_t* btl,
    size_t nprocs,
    struct ompi_proc_t **ompi_procs,
    struct mca_btl_base_endpoint_t** peers,
    opal_bitmap_t* reachable)
{
    mca_btl_openib_module_t* openib_btl = (mca_btl_openib_module_t*)btl;
    int i,j, rc, local_procs;
    int rem_subnet_id_port_cnt;
    int lcl_subnet_id_port_cnt = 0;
    int btl_rank = 0;
    mca_btl_base_endpoint_t* endpoint;
    ompi_btl_openib_connect_base_module_t *local_cpc;
    ompi_btl_openib_connect_base_module_data_t *remote_cpc_data;

    for(j=0; j < mca_btl_openib_component.ib_num_btls; j++){
        if(mca_btl_openib_component.openib_btls[j]->port_info.subnet_id
           == openib_btl->port_info.subnet_id) {
            if(openib_btl == mca_btl_openib_component.openib_btls[j]) {
                btl_rank = lcl_subnet_id_port_cnt;
            }
            lcl_subnet_id_port_cnt++;
        }
    }

#if HAVE_XRC
    if(MCA_BTL_XRC_ENABLED &&
            NULL == mca_btl_openib_component.ib_addr_table.ht_table) {
        if(OPAL_SUCCESS != opal_hash_table_init(
                    &mca_btl_openib_component.ib_addr_table, nprocs)) {
            BTL_ERROR(("XRC internal error. Failed to allocate ib_table"));
            return OMPI_ERROR;
        }
    }
#endif

    for (i = 0, local_procs = 0 ; i < (int) nprocs; i++) {
        struct ompi_proc_t* ompi_proc = ompi_procs[i];
        mca_btl_openib_proc_t* ib_proc;
        int remote_matching_port;

        opal_output(-1, "add procs: adding proc %d", i);

        if (OPAL_PROC_ON_LOCAL_NODE(ompi_proc->proc_flags)) {
            local_procs ++;
        }

        /* OOB, XOOB, and RDMACM do not support SELF comunication, so
         * mark the proc as unreachable by openib btl  */
        if (OPAL_EQUAL == orte_util_compare_name_fields
                (ORTE_NS_CMP_ALL, ORTE_PROC_MY_NAME, &ompi_proc->proc_name)) {
            continue;
        }
#if defined(HAVE_STRUCT_IBV_DEVICE_TRANSPORT_TYPE)
        /* Most current iWARP adapters (June 2008) cannot handle
           talking to other processes on the same host (!) -- so mark
           them as unreachable (need to use sm).  So for the moment,
           we'll just mark any local peer on an iWARP NIC as
           unreachable.  See trac ticket #1352. */
        if (IBV_TRANSPORT_IWARP == openib_btl->device->ib_dev->transport_type &&
            OPAL_PROC_ON_LOCAL_NODE(ompi_proc->proc_flags)) {
            continue;
        }
#endif

        if(NULL == (ib_proc = mca_btl_openib_proc_create(ompi_proc))) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        /* check if the remote proc has any ports that:
           - on the same subnet as the local proc, and
           - on that subnet, has a CPC in common with the local proc
        */
        remote_matching_port = -1;
        rem_subnet_id_port_cnt = 0;
        BTL_VERBOSE(("got %d port_infos ", ib_proc->proc_port_count));
        for (j = 0; j < (int) ib_proc->proc_port_count; j++){
            BTL_VERBOSE(("got a subnet %016" PRIx64,
                         ib_proc->proc_ports[j].pm_port_info.subnet_id));
            if (ib_proc->proc_ports[j].pm_port_info.subnet_id ==
                openib_btl->port_info.subnet_id) {
                BTL_VERBOSE(("Got a matching subnet!"));
                if (rem_subnet_id_port_cnt == btl_rank) {
                    remote_matching_port = j;
                }
                rem_subnet_id_port_cnt++;
            }
        }

        if (0 == rem_subnet_id_port_cnt) {
            /* no use trying to communicate with this endpoint */
            BTL_VERBOSE(("No matching subnet id/CPC was found, moving on.. "));
            continue;
        }

        /* If this process has multiple ports on a single subnet ID,
           and the report proc also has multiple ports on this same
           subnet ID, the default connection pattern is:

                      LOCAL                   REMOTE PEER
                 1st port on subnet X <--> 1st port on subnet X
                 2nd port on subnet X <--> 2nd port on subnet X
                 3nd port on subnet X <--> 3nd port on subnet X
                 ...etc.

           Note that the port numbers may not be contiguous, and they
           may not be the same on either side.  Hence the "1st", "2nd",
           "3rd, etc. notation, above.

           Hence, if the local "rank" of this module's port on the
           subnet ID is greater than the total number of ports on the
           peer on this same subnet, then we have no match.  So skip
           this connection.  */
        if (rem_subnet_id_port_cnt < lcl_subnet_id_port_cnt &&
            btl_rank >= rem_subnet_id_port_cnt) {
            BTL_VERBOSE(("Not enough remote ports on this subnet id, moving on.. "));
            continue;
        }

        /* Now that we have verified that we're on the same subnet and
           the remote peer has enough ports, see if that specific port
           on the peer has a matching CPC. */
        assert(btl_rank <= ib_proc->proc_port_count);
        assert(remote_matching_port != -1);
        if (OMPI_SUCCESS != 
            ompi_btl_openib_connect_base_find_match(openib_btl,
                                                    &(ib_proc->proc_ports[remote_matching_port]),
                                                    &local_cpc,
                                                    &remote_cpc_data)) {
            continue;
        }

        OPAL_THREAD_LOCK(&ib_proc->proc_lock);

        /* The btl_proc datastructure is shared by all IB BTL
         * instances that are trying to reach this destination.
         * Cache the peer instance on the btl_proc.
         */
        endpoint = OBJ_NEW(mca_btl_openib_endpoint_t);
        assert(((opal_object_t*)endpoint)->obj_reference_count == 1);
        if(NULL == endpoint) {
            OPAL_THREAD_UNLOCK(&ib_proc->proc_lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

#if HAVE_XRC
        if (MCA_BTL_XRC_ENABLED) {
            int rem_port_cnt = 0;
            for(j = 0; j < (int) ib_proc->proc_port_count; j++) {
                if(ib_proc->proc_ports[j].pm_port_info.subnet_id ==
                        openib_btl->port_info.subnet_id) {
                    if (rem_port_cnt == btl_rank)
                        break;
                    else
                        rem_port_cnt ++;
                }
            }

            assert(rem_port_cnt == btl_rank);
            /* Push the subnet/lid/jobid to xrc hash */
            rc = mca_btl_openib_ib_address_add_new(
                    ib_proc->proc_ports[j].pm_port_info.lid,
                    ib_proc->proc_ports[j].pm_port_info.subnet_id,
                    ompi_proc->proc_name.jobid, endpoint);
            if (OMPI_SUCCESS != rc ) {
                OPAL_THREAD_UNLOCK(&ib_proc->proc_lock);
                return OMPI_ERROR;
            }
        }
#endif
        mca_btl_openib_endpoint_init(openib_btl, endpoint, 
                                     local_cpc, 
                                     &(ib_proc->proc_ports[remote_matching_port]),
                                     remote_cpc_data);

        rc = mca_btl_openib_proc_insert(ib_proc, endpoint);
        if (OMPI_SUCCESS != rc) {
            OBJ_RELEASE(endpoint);
            OPAL_THREAD_UNLOCK(&ib_proc->proc_lock);
            continue;
        }

         if(OMPI_SUCCESS != mca_btl_openib_tune_endpoint(openib_btl, endpoint)) {
            OBJ_RELEASE(endpoint);
            OPAL_THREAD_UNLOCK(&ib_proc->proc_lock);
            return OMPI_ERROR;
        }

        endpoint->index = opal_pointer_array_add(openib_btl->device->endpoints, (void*)endpoint);
        if( 0 > endpoint->index ) {
            OBJ_RELEASE(endpoint);
            OPAL_THREAD_UNLOCK(&ib_proc->proc_lock);
            continue;
        }

        /* Tell the selected CPC that it won.  NOTE: This call is
           outside of / separate from mca_btl_openib_endpoint_init()
           because this function likely needs the endpoint->index. */
        if (NULL != local_cpc->cbm_endpoint_init) {
            rc = local_cpc->cbm_endpoint_init(endpoint);
            if (OMPI_SUCCESS != rc) {
                OBJ_RELEASE(endpoint);
                OPAL_THREAD_UNLOCK(&ib_proc->proc_lock);
                continue;
            }
        }

        opal_bitmap_set_bit(reachable, i);
        OPAL_THREAD_UNLOCK(&ib_proc->proc_lock);

        peers[i] = endpoint;
    }

    openib_btl->local_procs += local_procs;
    openib_btl->device->mem_reg_max = calculate_max_reg () / openib_btl->local_procs;

    return mca_btl_openib_size_queues(openib_btl, nprocs);
}

/*
 * delete the proc as reachable from this btl module
 */
int mca_btl_openib_del_procs(struct mca_btl_base_module_t* btl,
        size_t nprocs,
        struct ompi_proc_t **procs,
        struct mca_btl_base_endpoint_t ** peers)
{
    int i,ep_index;
    mca_btl_openib_module_t* openib_btl = (mca_btl_openib_module_t*) btl;
    mca_btl_openib_endpoint_t* endpoint;

    for (i=0 ; i < (int) nprocs ; i++) {
        mca_btl_base_endpoint_t* del_endpoint = peers[i];
        for(ep_index=0;
            ep_index < opal_pointer_array_get_size(openib_btl->device->endpoints);
            ep_index++) {
            endpoint =
                opal_pointer_array_get_item(openib_btl->device->endpoints,
                        ep_index);
            if(!endpoint || endpoint->endpoint_btl != openib_btl) {
                continue;
            }
            if (endpoint == del_endpoint) {
                BTL_VERBOSE(("in del_procs %d, setting another endpoint to null",
                             ep_index));
                opal_pointer_array_set_item(openib_btl->device->endpoints,
                        ep_index, NULL);
                assert(((opal_object_t*)endpoint)->obj_reference_count == 1);
                mca_btl_openib_proc_remove(procs[i], endpoint);
                OBJ_RELEASE(endpoint);
            }
        }
    }

    return OMPI_SUCCESS;
}

/*
 *Register callback function for error handling..
 */
int mca_btl_openib_register_error_cb(
                        struct mca_btl_base_module_t* btl,
                        mca_btl_base_module_error_cb_fn_t cbfunc)
{

    mca_btl_openib_module_t* openib_btl = (mca_btl_openib_module_t*) btl;
    openib_btl->error_cb = cbfunc; /* stash for later */
    return OMPI_SUCCESS;
}

static inline mca_btl_base_descriptor_t *
ib_frag_alloc(mca_btl_openib_module_t *btl, size_t size, uint8_t order,
        uint32_t flags)
{
    int qp, rc;
    ompi_free_list_item_t* item = NULL;

    for(qp = 0; qp < mca_btl_openib_component.num_qps; qp++) {
         if(mca_btl_openib_component.qp_infos[qp].size >= size) {
             OMPI_FREE_LIST_GET(&btl->device->qps[qp].send_free, item, rc);
             if(item)
                 break;
         }
    }
    if(NULL == item)
        return NULL;

    /* not all upper layer users set this */
    to_base_frag(item)->segment.seg_len = size;
    to_base_frag(item)->base.order = order;
    to_base_frag(item)->base.des_flags = flags;

    assert(to_send_frag(item)->qp_idx <= order);
    return &to_base_frag(item)->base;
}

/* check if pending fragment has enough space for coalescing */
static mca_btl_openib_send_frag_t *check_coalescing(opal_list_t *frag_list,
        opal_mutex_t *lock, mca_btl_base_endpoint_t *ep, size_t size)
{
    mca_btl_openib_send_frag_t *frag = NULL;

    if(opal_list_is_empty(frag_list))
        return NULL;

    OPAL_THREAD_LOCK(lock);
    if(!opal_list_is_empty(frag_list)) {
        int qp;
        size_t total_length;
        opal_list_item_t *i = opal_list_get_first(frag_list);
        frag = to_send_frag(i);
        if(to_com_frag(frag)->endpoint != ep ||
                MCA_BTL_OPENIB_FRAG_CONTROL == openib_frag_type(frag)) {
            OPAL_THREAD_UNLOCK(lock);
            return NULL;
        }

        total_length = size + frag->coalesced_length +
            to_base_frag(frag)->segment.seg_len +
            sizeof(mca_btl_openib_header_coalesced_t);

        qp = to_base_frag(frag)->base.order;

        if(total_length <= mca_btl_openib_component.qp_infos[qp].size)
            opal_list_remove_first(frag_list);
        else
            frag = NULL;
    }
    OPAL_THREAD_UNLOCK(lock);

    return frag;
}

/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 * @param size (IN) Size of segment to allocate
 *
 * When allocating a segment we pull a pre-alllocated segment
 * from one of two free lists, an eager list and a max list
 */
mca_btl_base_descriptor_t* mca_btl_openib_alloc(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    uint8_t order,
    size_t size,
    uint32_t flags)
{
    mca_btl_openib_module_t *obtl = (mca_btl_openib_module_t*)btl;
    int qp = frag_size_to_order(obtl, size);
    mca_btl_openib_send_frag_t *sfrag = NULL;
    mca_btl_openib_coalesced_frag_t *cfrag;

    assert(qp != MCA_BTL_NO_ORDER);

    if(mca_btl_openib_component.use_message_coalescing &&
       (flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP)) {
        int prio = !(flags & MCA_BTL_DES_FLAGS_PRIORITY);

        sfrag = check_coalescing(&ep->qps[qp].no_wqe_pending_frags[prio],
                &ep->endpoint_lock, ep, size);

        if(NULL == sfrag) {
            if(BTL_OPENIB_QP_TYPE_PP(qp)) {
                sfrag = check_coalescing(&ep->qps[qp].no_credits_pending_frags[prio],
                        &ep->endpoint_lock, ep, size);
            } else {
                sfrag = check_coalescing(
                        &obtl->qps[qp].u.srq_qp.pending_frags[prio],
                        &obtl->ib_lock, ep, size);
            }
        }
    }

    if(NULL == sfrag)
        return ib_frag_alloc((mca_btl_openib_module_t*)btl, size, order, flags);

    /* begin coalescing message */
    cfrag = alloc_coalesced_frag();
    cfrag->send_frag = sfrag;

    /* fix up new coalescing header if this is the first coalesced frag */
    if(sfrag->hdr != sfrag->chdr) {
        mca_btl_openib_control_header_t *ctrl_hdr;
        mca_btl_openib_header_coalesced_t *clsc_hdr;
        uint8_t org_tag;

        org_tag = sfrag->hdr->tag;
        sfrag->hdr = sfrag->chdr;
        ctrl_hdr = (mca_btl_openib_control_header_t*)(sfrag->hdr + 1);
        clsc_hdr = (mca_btl_openib_header_coalesced_t*)(ctrl_hdr + 1);
        sfrag->hdr->tag = MCA_BTL_TAG_BTL;
        ctrl_hdr->type = MCA_BTL_OPENIB_CONTROL_COALESCED;
        clsc_hdr->tag = org_tag;
        clsc_hdr->size = to_base_frag(sfrag)->segment.seg_len;
        clsc_hdr->alloc_size = to_base_frag(sfrag)->segment.seg_len;
        if(ep->nbo)
            BTL_OPENIB_HEADER_COALESCED_HTON(*clsc_hdr);
        sfrag->coalesced_length = sizeof(mca_btl_openib_control_header_t) +
            sizeof(mca_btl_openib_header_coalesced_t);
        to_com_frag(sfrag)->sg_entry.addr = (uint64_t)(uintptr_t)sfrag->hdr;
    }

    cfrag->hdr = (mca_btl_openib_header_coalesced_t*)((unsigned char*)(sfrag->hdr + 1) + 
                  sfrag->coalesced_length +
                  to_base_frag(sfrag)->segment.seg_len);
    cfrag->hdr = (mca_btl_openib_header_coalesced_t*)(unsigned char*)(BTL_OPENIB_ALIGN_COALESCE_HDR((unsigned char*)(cfrag->hdr)));
    cfrag->hdr->alloc_size = size;

    /* point coalesced frag pointer into a data buffer */
    to_base_frag(cfrag)->segment.seg_addr.pval = cfrag->hdr + 1;
    to_base_frag(cfrag)->segment.seg_len = size;

    /* save coalesced fragment on a main fragment; we will need it after send
     * completion to free it and to call upper layer callback */
    opal_list_append(&sfrag->coalesced_frags, (opal_list_item_t*)cfrag);
    sfrag->coalesced_length += (size+sizeof(mca_btl_openib_header_coalesced_t));

    to_base_frag(cfrag)->base.des_flags = flags;

    return &to_base_frag(cfrag)->base;
}

/**
 * Return a segment
 *
 * Return the segment to the appropriate
 *  preallocated segment list
 */
int mca_btl_openib_free(
                    struct mca_btl_base_module_t* btl,
                    mca_btl_base_descriptor_t* des)
{
    /* is this fragment pointing at user memory? */
    if(MCA_BTL_OPENIB_FRAG_SEND_USER == openib_frag_type(des) ||
            MCA_BTL_OPENIB_FRAG_RECV_USER == openib_frag_type(des)) {
        mca_btl_openib_com_frag_t* frag = to_com_frag(des);

        if(frag->registration != NULL) {
            btl->btl_mpool->mpool_deregister(btl->btl_mpool,
                    (mca_mpool_base_registration_t*)frag->registration);
            frag->registration = NULL;
        }
    }

    /* reset those field on free so we will not have to do it on alloc */
    to_base_frag(des)->base.des_flags = 0;
    switch(openib_frag_type(des)) {
        case MCA_BTL_OPENIB_FRAG_RECV:
        case MCA_BTL_OPENIB_FRAG_RECV_USER:
            to_base_frag(des)->base.des_src = NULL;
            to_base_frag(des)->base.des_src_cnt = 0;
            break;
        case MCA_BTL_OPENIB_FRAG_SEND:
            to_send_frag(des)->hdr = (mca_btl_openib_header_t*)
                (((unsigned char*)to_send_frag(des)->chdr) +
                sizeof(mca_btl_openib_header_coalesced_t) +
                sizeof(mca_btl_openib_control_header_t));
            to_com_frag(des)->sg_entry.addr =
                (uint64_t)(uintptr_t)to_send_frag(des)->hdr;
            to_send_frag(des)->coalesced_length = 0;
            assert(!opal_list_get_size(&to_send_frag(des)->coalesced_frags));
            /* fall throug */
        case MCA_BTL_OPENIB_FRAG_SEND_USER:
            to_base_frag(des)->base.des_dst = NULL;
            to_base_frag(des)->base.des_dst_cnt = 0;
            break;
        default:
            break;
    }
    MCA_BTL_IB_FRAG_RETURN(des);

    return OMPI_SUCCESS;
}

/**
 * register user buffer or pack
 * data into pre-registered buffer and return a
 * descriptor that can be
 * used for send/put.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 *
 * prepare source's behavior depends on the following:
 * Has a valid memory registration been passed to prepare_src?
 *    if so we attempt to use the pre-registered user-buffer, if the memory registration
 *    is too small (only a portion of the user buffer) then we must reregister the user buffer
 * Has the user requested the memory to be left pinned?
 *    if so we insert the memory registration into a memory tree for later lookup, we
 *    may also remove a previous registration if a MRU (most recently used) list of
 *    registrations is full, this prevents resources from being exhausted.
 * Is the requested size larger than the btl's max send size?
 *    if so and we aren't asked to leave the registration pinned, then we register the memory if
 *    the users buffer is contiguous
 * Otherwise we choose from two free lists of pre-registered memory in which to pack the data into.
 *
 */
mca_btl_base_descriptor_t* mca_btl_openib_prepare_src(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    mca_mpool_base_registration_t* registration,
    struct opal_convertor_t* convertor,
    uint8_t order,
    size_t reserve,
    size_t* size,
    uint32_t flags)
{
    mca_btl_openib_module_t *openib_btl;
    mca_btl_openib_reg_t *openib_reg;
    mca_btl_openib_com_frag_t *frag = NULL;
    struct iovec iov;
    uint32_t iov_count = 1;
    size_t max_data = *size;
    int rc;

    openib_btl = (mca_btl_openib_module_t*)btl;

    if(opal_convertor_need_buffers(convertor) == false && 0 == reserve) {
        /* GMS  bloody HACK! */
        if(registration != NULL || max_data > btl->btl_max_send_size) {
            frag = alloc_send_user_frag();
            if(NULL == frag) {
                return NULL;
            }

            iov.iov_len = max_data;
            iov.iov_base = NULL;

            opal_convertor_pack(convertor, &iov, &iov_count, &max_data);

            *size = max_data;

            if(NULL == registration) {
                rc = btl->btl_mpool->mpool_register(btl->btl_mpool,
                        iov.iov_base, max_data, 0, &registration);
                if(OMPI_SUCCESS != rc || NULL == registration) {
                    MCA_BTL_IB_FRAG_RETURN(frag);
                    return NULL;
                }
                /* keep track of the registration we did */
                to_com_frag(frag)->registration =
                    (mca_btl_openib_reg_t*)registration;
            }
            openib_reg = (mca_btl_openib_reg_t*)registration;

            frag->sg_entry.length = max_data;
            frag->sg_entry.lkey = openib_reg->mr->lkey;
            frag->sg_entry.addr = (uint64_t)(uintptr_t)iov.iov_base;

            to_base_frag(frag)->base.order = order;
            to_base_frag(frag)->base.des_flags = flags;
            to_base_frag(frag)->segment.seg_len = max_data;
            to_base_frag(frag)->segment.seg_addr.pval = iov.iov_base;
            to_base_frag(frag)->segment.seg_key.key32[0] =
                (uint32_t)frag->sg_entry.lkey;

            assert(MCA_BTL_NO_ORDER == order);

            BTL_VERBOSE(("frag->sg_entry.lkey = %" PRIu32 " .addr = %" PRIx64
                         " frag->segment.seg_key.key32[0] = %" PRIu32,
                         frag->sg_entry.lkey, 
                         frag->sg_entry.addr,
                         frag->sg_entry.lkey));

            return &to_base_frag(frag)->base;
        }
    }

    assert(MCA_BTL_NO_ORDER == order);

    if(max_data + reserve > btl->btl_max_send_size) {
        max_data = btl->btl_max_send_size - reserve;
    }

    frag = (mca_btl_openib_com_frag_t*)(reserve ?
            mca_btl_openib_alloc(btl, endpoint, order, max_data + reserve,
                flags) :
            ib_frag_alloc(openib_btl, max_data, order, flags));

    if(NULL == frag)
        return NULL;

    iov.iov_len = max_data;
    iov.iov_base = (unsigned char*)to_base_frag(frag)->segment.seg_addr.pval +
        reserve;
    rc = opal_convertor_pack(convertor, &iov, &iov_count, &max_data);

    *size = max_data;

    /* not all upper layer users set this */
    to_base_frag(frag)->segment.seg_len = max_data + reserve;

    return &to_base_frag(frag)->base;
}

/**
 * Prepare the dst buffer
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 * prepare dest's behavior depends on the following:
 * Has a valid memory registration been passed to prepare_src?
 *    if so we attempt to use the pre-registered user-buffer, if the memory registration
 *    is to small (only a portion of the user buffer) then we must reregister the user buffer
 * Has the user requested the memory to be left pinned?
 *    if so we insert the memory registration into a memory tree for later lookup, we
 *    may also remove a previous registration if a MRU (most recently used) list of
 *    registrations is full, this prevents resources from being exhausted.
 */
mca_btl_base_descriptor_t* mca_btl_openib_prepare_dst(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    mca_mpool_base_registration_t* registration,
    struct opal_convertor_t* convertor,
    uint8_t order,
    size_t reserve,
    size_t* size,
    uint32_t flags)
{
    mca_btl_openib_module_t *openib_btl;
    mca_btl_openib_com_frag_t *frag;
    mca_btl_openib_reg_t *openib_reg;
    int rc;
    void *buffer;

    openib_btl = (mca_btl_openib_module_t*)btl;

    frag = alloc_recv_user_frag();
    if(NULL == frag) {
        return NULL;
    }

    opal_convertor_get_current_pointer(convertor, &buffer);

    if(NULL == registration){
        /* we didn't get a memory registration passed in, so we have to
         * register the region ourselves
         */
        rc = btl->btl_mpool->mpool_register(btl->btl_mpool, buffer, *size, 0,
                &registration);
        if(OMPI_SUCCESS != rc || NULL == registration) {
            MCA_BTL_IB_FRAG_RETURN(frag);
            return NULL;
        }
        /* keep track of the registration we did */
        frag->registration = (mca_btl_openib_reg_t*)registration;
    }
    openib_reg = (mca_btl_openib_reg_t*)registration;

    frag->sg_entry.length = *size;
    frag->sg_entry.lkey = openib_reg->mr->lkey;
    frag->sg_entry.addr = (uint64_t)(uintptr_t)buffer;

    to_base_frag(frag)->segment.seg_addr.pval = buffer;
    to_base_frag(frag)->segment.seg_len = *size;
    to_base_frag(frag)->segment.seg_key.key32[0] = openib_reg->mr->rkey;
    to_base_frag(frag)->base.order = order;
    to_base_frag(frag)->base.des_flags = flags;

    BTL_VERBOSE(("frag->sg_entry.lkey = %" PRIu32 " .addr = %" PRIx64 " "
                 "frag->segment.seg_key.key32[0] = %" PRIu32,
                 frag->sg_entry.lkey, 
                 frag->sg_entry.addr,
                 openib_reg->mr->rkey));

    return &to_base_frag(frag)->base;
}

static int mca_btl_openib_finalize_resources(struct mca_btl_base_module_t* btl) {
    mca_btl_openib_module_t* openib_btl;
    mca_btl_openib_endpoint_t* endpoint;
    int ep_index, i;
    int qp, rc = OMPI_SUCCESS;

    openib_btl = (mca_btl_openib_module_t*) btl;

    /* Sanity check */
    if( mca_btl_openib_component.ib_num_btls <= 0 ) {
        return OMPI_SUCCESS;
    }

    /* Release all QPs */
    if (NULL != openib_btl->device->endpoints) {
        for (ep_index=0;
             ep_index < opal_pointer_array_get_size(openib_btl->device->endpoints);
             ep_index++) {
            endpoint=opal_pointer_array_get_item(openib_btl->device->endpoints,
                                                 ep_index);
            if(!endpoint) {
                BTL_VERBOSE(("In finalize, got another null endpoint"));
                continue;
            }
            if(endpoint->endpoint_btl != openib_btl) {
                continue;
            }
            for(i = 0; i < openib_btl->device->eager_rdma_buffers_count; i++) {
                if(openib_btl->device->eager_rdma_buffers[i] == endpoint) {
                    openib_btl->device->eager_rdma_buffers[i] = NULL;
                    OBJ_RELEASE(endpoint);
                }
            }
            OBJ_RELEASE(endpoint);
        }
    }

    /* Release SRQ resources */
    for(qp = 0; qp < mca_btl_openib_component.num_qps; qp++) {
        if(!BTL_OPENIB_QP_TYPE_PP(qp)) {
            MCA_BTL_OPENIB_CLEAN_PENDING_FRAGS(
                    &openib_btl->qps[qp].u.srq_qp.pending_frags[0]);
            MCA_BTL_OPENIB_CLEAN_PENDING_FRAGS(
                    &openib_btl->qps[qp].u.srq_qp.pending_frags[1]);
            if (NULL != openib_btl->qps[qp].u.srq_qp.srq) {
#if OPAL_HAVE_THREADS
                opal_mutex_t *lock =
                             &mca_btl_openib_component.srq_manager.lock;

                opal_hash_table_t *srq_addr_table =
                            &mca_btl_openib_component.srq_manager.srq_addr_table;

                opal_mutex_lock(lock);		
                if (OPAL_SUCCESS !=
                        opal_hash_table_remove_value_ptr(srq_addr_table,
                                    &openib_btl->qps[qp].u.srq_qp.srq,
                                    sizeof(struct ibv_srq *))) {
                    BTL_VERBOSE(("Failed to remove SRQ  %d entry from hash table.", qp));
                    rc = OMPI_ERROR;
                }
                opal_mutex_unlock(lock);
#endif
                if (0 != ibv_destroy_srq(openib_btl->qps[qp].u.srq_qp.srq)) {
                    BTL_VERBOSE(("Failed to close SRQ %d", qp));
                    rc = OMPI_ERROR;
                }
            }

            OBJ_DESTRUCT(&openib_btl->qps[qp].u.srq_qp.pending_frags[0]);
            OBJ_DESTRUCT(&openib_btl->qps[qp].u.srq_qp.pending_frags[1]);
        }
    }

    /* Finalize the CPC modules on this openib module */
    for (i = 0; i < openib_btl->num_cpcs; ++i) {
        if (NULL != openib_btl->cpcs[i]->cbm_finalize) {
            openib_btl->cpcs[i]->cbm_finalize(openib_btl, openib_btl->cpcs[i]);
        }
        free(openib_btl->cpcs[i]);
    }
    free(openib_btl->cpcs);

    /* Release device if there are no more users */
    if(!(--openib_btl->device->btls)) {
        OBJ_RELEASE(openib_btl->device);
    }

    if (NULL != openib_btl->qps) {
        free(openib_btl->qps); 
    }

    return rc;
}


int mca_btl_openib_finalize(struct mca_btl_base_module_t* btl)
{
    mca_btl_openib_module_t* openib_btl;
    int i, rc = OMPI_SUCCESS;

    openib_btl = (mca_btl_openib_module_t*) btl;

    /* Sanity check */
    if( mca_btl_openib_component.ib_num_btls <= 0 ) {
        return 0;
    }

    if( OMPI_SUCCESS != (rc = mca_btl_openib_finalize_resources(btl) ) ) {
        BTL_VERBOSE(("Failed to finalize resources"));
    }

    /* Remove the btl from component list */
    if ( mca_btl_openib_component.ib_num_btls > 1 ) {
        for(i = 0; i < mca_btl_openib_component.ib_num_btls; i++){
            if (mca_btl_openib_component.openib_btls[i] == openib_btl){
                mca_btl_openib_component.openib_btls[i] =
                    mca_btl_openib_component.openib_btls[mca_btl_openib_component.ib_num_btls-1];
                break;
            }
        }
    }

    mca_btl_openib_component.ib_num_btls--;

    OBJ_DESTRUCT(&openib_btl->ib_lock);
    free(openib_btl);

    BTL_VERBOSE(("Success in closing BTL resources"));

    return rc;
}

/*
 *  Send immediate - Minimum function calls minimum checks, send the data ASAP.
 *  If BTL can't to send the messages imidiate, it creates messages descriptor 
 *  returns it to PML.
 */
int mca_btl_openib_sendi( struct mca_btl_base_module_t* btl,
        struct mca_btl_base_endpoint_t* ep,
        struct opal_convertor_t* convertor,
        void* header,
        size_t header_size,
        size_t payload_size,
        uint8_t order,
        uint32_t flags,
        mca_btl_base_tag_t tag,
        mca_btl_base_descriptor_t** descriptor) 
{
    mca_btl_openib_module_t *obtl = (mca_btl_openib_module_t*)btl;
    size_t size = payload_size + header_size;
    size_t eager_limit;
    int rc,  
        qp = frag_size_to_order(obtl, size), 
        prio = !(flags & MCA_BTL_DES_FLAGS_PRIORITY), 
        ib_rc;
    int32_t cm_return;
    bool do_rdma = false;
    ompi_free_list_item_t* item = NULL;
    mca_btl_openib_frag_t *frag;
    mca_btl_openib_header_t *hdr;
    int send_signaled;

    OPAL_THREAD_LOCK(&ep->endpoint_lock);

    if (OPAL_UNLIKELY(MCA_BTL_IB_CONNECTED != ep->endpoint_state)) {
        goto cant_send;
    }

    /* If it is pending messages on the qp - we can not send */
    if(OPAL_UNLIKELY(!opal_list_is_empty(&ep->qps[qp].no_wqe_pending_frags[prio]))) {
        goto cant_send;
    }

    /* Allocate WQE */
    if(OPAL_UNLIKELY(qp_get_wqe(ep, qp) < 0)) {
        goto no_credits_or_wqe;
    }

    /* eager rdma or send ? Check eager rdma credits */
    /* Note: Maybe we want to implement isend only for eager rdma ?*/
    eager_limit = mca_btl_openib_component.eager_limit +
        sizeof(mca_btl_openib_header_coalesced_t) +
        sizeof(mca_btl_openib_control_header_t);

    if(OPAL_LIKELY(size <= eager_limit)) {
        if(acquire_eager_rdma_send_credit(ep) == OMPI_SUCCESS) {
            do_rdma = true;
        }
    }

    /* if(!do_rdma && acquire_send_credit(ep, frag) != OMPI_SUCCESS) { */
    /* Check send credits if it is no rdma */
    if(!do_rdma) {
        if(BTL_OPENIB_QP_TYPE_PP(qp)) {
            if(OPAL_UNLIKELY(OPAL_THREAD_ADD32(&ep->qps[qp].u.pp_qp.sd_credits, -1) < 0)){
                OPAL_THREAD_ADD32(&ep->qps[qp].u.pp_qp.sd_credits, 1);
                goto no_credits_or_wqe;
            }
        } else {
            if(OPAL_UNLIKELY(OPAL_THREAD_ADD32(&obtl->qps[qp].u.srq_qp.sd_credits, -1) < 0)){
                OPAL_THREAD_ADD32(&obtl->qps[qp].u.srq_qp.sd_credits, 1);
                goto no_credits_or_wqe;
            }
        }
    }

    /* Allocate fragment */
    OMPI_FREE_LIST_GET(&obtl->device->qps[qp].send_free, item, rc);
    if(OPAL_UNLIKELY(NULL == item)) {
        /* we don't return NULL because maybe later we will try to coalesce */
        goto no_frags;
    }
    frag = to_base_frag(item);
    hdr = to_send_frag(item)->hdr;
    frag->segment.seg_len = size;
    frag->base.order = qp;
    frag->base.des_flags = flags;
    hdr->tag = tag;
    to_com_frag(item)->endpoint = ep;

    /* put match header */
    memcpy(frag->segment.seg_addr.pval, header, header_size);

    /* Pack data */
    if(payload_size) {
        size_t max_data;
        struct iovec iov;
        uint32_t iov_count;
        /* pack the data into the supplied buffer */
        iov.iov_base = (IOVBASE_TYPE*)((unsigned char*)frag->segment.seg_addr.pval + header_size);
        iov.iov_len  = max_data = payload_size;
        iov_count    = 1;

        (void)opal_convertor_pack( convertor, &iov, &iov_count, &max_data);

        assert(max_data == payload_size);
    }

    /* Set all credits */
    BTL_OPENIB_GET_CREDITS(ep->eager_rdma_local.credits, hdr->credits);
    if(hdr->credits)
        hdr->credits |= BTL_OPENIB_RDMA_CREDITS_FLAG;

    if(!do_rdma) {
        if(BTL_OPENIB_QP_TYPE_PP(qp) && 0 == hdr->credits) {
            BTL_OPENIB_GET_CREDITS(ep->qps[qp].u.pp_qp.rd_credits, hdr->credits);
        }
    } else {
        hdr->credits |= (qp << 11);
    }

    BTL_OPENIB_GET_CREDITS(ep->qps[qp].u.pp_qp.cm_return, cm_return);
    /* cm_seen is only 8 bytes, but cm_return is 32 bytes */
    if(cm_return > 255) {
        hdr->cm_seen = 255;
        cm_return -= 255;
        OPAL_THREAD_ADD32(&ep->qps[qp].u.pp_qp.cm_return, cm_return);
    } else {
        hdr->cm_seen = cm_return;
    }

#if BTL_OPENIB_FAILOVER_ENABLED
    send_signaled = 1;
#else
    send_signaled = qp_need_signal(ep, qp, payload_size + header_size, do_rdma);
#endif
    ib_rc = post_send(ep, to_send_frag(item), do_rdma, send_signaled);

    if(!ib_rc) {
        if (0 == send_signaled) {
            MCA_BTL_IB_FRAG_RETURN(frag);
        }
#if BTL_OPENIB_FAILOVER_ENABLED
        else {
            /* Return up in case needed for failover */
            *descriptor = (struct mca_btl_base_descriptor_t *) frag;
        }
#endif
        OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
        return OMPI_SUCCESS;
    }

    /* Failed to send, do clean up all allocated resources */
    if(ep->nbo) {
        BTL_OPENIB_HEADER_NTOH(*hdr);
    }
    if(BTL_OPENIB_IS_RDMA_CREDITS(hdr->credits)) {
        OPAL_THREAD_ADD32(&ep->eager_rdma_local.credits,
                BTL_OPENIB_CREDITS(hdr->credits));
    }
    if (!do_rdma && BTL_OPENIB_QP_TYPE_PP(qp)) {
        OPAL_THREAD_ADD32(&ep->qps[qp].u.pp_qp.rd_credits,
                hdr->credits);
    }
no_frags:
    if(do_rdma) {
        OPAL_THREAD_ADD32(&ep->eager_rdma_remote.tokens, 1);
    } else {
        if(BTL_OPENIB_QP_TYPE_PP(qp)) {
            OPAL_THREAD_ADD32(&ep->qps[qp].u.pp_qp.sd_credits, 1);
        } else if BTL_OPENIB_QP_TYPE_SRQ(qp){
            OPAL_THREAD_ADD32(&obtl->qps[qp].u.srq_qp.sd_credits, 1);
        }
    }
no_credits_or_wqe:
    qp_put_wqe(ep, qp);
cant_send:
    OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
    /* We can not send the data directly, so we just return descriptor */
    *descriptor = mca_btl_openib_alloc(btl, ep, order, size, flags);
    return OMPI_ERR_RESOURCE_BUSY;
}
/*
 *  Initiate a send.
 */

int mca_btl_openib_send(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* des,
    mca_btl_base_tag_t tag)

{
    mca_btl_openib_send_frag_t *frag;

    assert(openib_frag_type(des) == MCA_BTL_OPENIB_FRAG_SEND ||
            openib_frag_type(des) == MCA_BTL_OPENIB_FRAG_COALESCED);

    if(openib_frag_type(des) == MCA_BTL_OPENIB_FRAG_COALESCED) {
        to_coalesced_frag(des)->hdr->tag = tag;
        to_coalesced_frag(des)->hdr->size = des->des_src->seg_len;
        if(ep->nbo)
            BTL_OPENIB_HEADER_COALESCED_HTON(*to_coalesced_frag(des)->hdr);
        frag = to_coalesced_frag(des)->send_frag;
    } else {
        frag = to_send_frag(des);
        to_com_frag(des)->endpoint = ep;
        frag->hdr->tag = tag;
    }

    des->des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;

    return mca_btl_openib_endpoint_send(ep, frag);
}

/*
 * RDMA WRITE local buffer to remote buffer address.
 */

int mca_btl_openib_put( mca_btl_base_module_t* btl,
                    mca_btl_base_endpoint_t* ep,
                    mca_btl_base_descriptor_t* descriptor)
{
    struct ibv_send_wr* bad_wr;
    mca_btl_openib_out_frag_t* frag = to_out_frag(descriptor);
    int qp = descriptor->order;
    uint64_t rem_addr = descriptor->des_dst->seg_addr.lval;
    uint32_t rkey = descriptor->des_dst->seg_key.key32[0];

    assert(openib_frag_type(frag) == MCA_BTL_OPENIB_FRAG_SEND_USER ||
            openib_frag_type(frag) == MCA_BTL_OPENIB_FRAG_SEND);

    descriptor->des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;

    if(ep->endpoint_state != MCA_BTL_IB_CONNECTED) {
        int rc;
        OPAL_THREAD_LOCK(&ep->endpoint_lock);
        rc = check_endpoint_state(ep, descriptor, &ep->pending_put_frags);
        OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
        if(OMPI_ERR_RESOURCE_BUSY == rc)
            return OMPI_SUCCESS;
        if(OMPI_SUCCESS != rc)
            return rc;
    }

    if(MCA_BTL_NO_ORDER == qp)
        qp = mca_btl_openib_component.rdma_qp;

    /* check for a send wqe */
    if (qp_get_wqe(ep, qp) < 0) {
        qp_put_wqe(ep, qp);
        OPAL_THREAD_LOCK(&ep->endpoint_lock);
        opal_list_append(&ep->pending_put_frags, (opal_list_item_t*)frag);
        OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
        return OMPI_SUCCESS;
    }
    /* post descriptor */
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    if((ep->endpoint_proc->proc_ompi->proc_arch & OPAL_ARCH_ISBIGENDIAN)
            != (ompi_proc_local()->proc_arch & OPAL_ARCH_ISBIGENDIAN)) {
        rem_addr = opal_swap_bytes8(rem_addr);
        rkey = opal_swap_bytes4(rkey);
    }
#endif
    frag->sr_desc.wr.rdma.remote_addr = rem_addr;
    frag->sr_desc.wr.rdma.rkey = rkey;

    to_com_frag(frag)->sg_entry.addr =
        (uint64_t)(uintptr_t)descriptor->des_src->seg_addr.pval;
    to_com_frag(frag)->sg_entry.length = descriptor->des_src->seg_len;
    to_com_frag(frag)->endpoint = ep;
#if HAVE_XRC
    if (MCA_BTL_XRC_ENABLED && BTL_OPENIB_QP_TYPE_XRC(qp))
        frag->sr_desc.xrc_remote_srq_num=ep->rem_info.rem_srqs[qp].rem_srq_num;
#endif

    descriptor->order = qp;
    /* Setting opcode on a frag constructor isn't enough since prepare_src
     * may return send_frag instead of put_frag */
    frag->sr_desc.opcode = IBV_WR_RDMA_WRITE;
    frag->sr_desc.send_flags = ib_send_flags(descriptor->des_src->seg_len, &(ep->qps[qp]), 1);
    qp_inflight_wqe_to_frag(ep, qp, to_com_frag(frag));
    qp_reset_signal_count(ep, qp);
    if(ibv_post_send(ep->qps[qp].qp->lcl_qp, &frag->sr_desc, &bad_wr))
        return OMPI_ERROR;

    return OMPI_SUCCESS;
}

/*
 * RDMA READ remote buffer to local buffer address.
 */

int mca_btl_openib_get(mca_btl_base_module_t* btl,
                    mca_btl_base_endpoint_t* ep,
                    mca_btl_base_descriptor_t* descriptor)
{
    struct ibv_send_wr* bad_wr;
    mca_btl_openib_get_frag_t* frag = to_get_frag(descriptor);
    int qp = descriptor->order;
    uint64_t rem_addr = descriptor->des_src->seg_addr.lval;
    uint32_t rkey = descriptor->des_src->seg_key.key32[0];

    assert(openib_frag_type(frag) == MCA_BTL_OPENIB_FRAG_RECV_USER);

    descriptor->des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;

    if(ep->endpoint_state != MCA_BTL_IB_CONNECTED) {
        int rc;
        OPAL_THREAD_LOCK(&ep->endpoint_lock);
        rc = check_endpoint_state(ep, descriptor, &ep->pending_get_frags);
        OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
        if(OMPI_ERR_RESOURCE_BUSY == rc)
            return OMPI_SUCCESS;
        if(OMPI_SUCCESS != rc)
            return rc;
    }

    if(MCA_BTL_NO_ORDER == qp)
        qp = mca_btl_openib_component.rdma_qp;

    /* check for a send wqe */
    if (qp_get_wqe(ep, qp) < 0) {
        qp_put_wqe(ep, qp);
        OPAL_THREAD_LOCK(&ep->endpoint_lock);
        opal_list_append(&ep->pending_get_frags, (opal_list_item_t*)frag);
        OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
        return OMPI_SUCCESS;
    }

    /* check for a get token */
    if(OPAL_THREAD_ADD32(&ep->get_tokens,-1) < 0) {
        qp_put_wqe(ep, qp);
        OPAL_THREAD_ADD32(&ep->get_tokens,1);
        OPAL_THREAD_LOCK(&ep->endpoint_lock);
        opal_list_append(&ep->pending_get_frags, (opal_list_item_t*)frag);
        OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
        return OMPI_SUCCESS;
    }

#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    if((ep->endpoint_proc->proc_ompi->proc_arch & OPAL_ARCH_ISBIGENDIAN)
            != (ompi_proc_local()->proc_arch & OPAL_ARCH_ISBIGENDIAN)) {
        rem_addr = opal_swap_bytes8(rem_addr);
        rkey = opal_swap_bytes4(rkey);
    }
#endif
    frag->sr_desc.wr.rdma.remote_addr = rem_addr;
    frag->sr_desc.wr.rdma.rkey = rkey;

    to_com_frag(frag)->sg_entry.addr =
        (uint64_t)(uintptr_t)descriptor->des_dst->seg_addr.pval;
    to_com_frag(frag)->sg_entry.length  = descriptor->des_dst->seg_len;
    to_com_frag(frag)->endpoint = ep;

#if HAVE_XRC
    if (MCA_BTL_XRC_ENABLED && BTL_OPENIB_QP_TYPE_XRC(qp))
        frag->sr_desc.xrc_remote_srq_num=ep->rem_info.rem_srqs[qp].rem_srq_num;
#endif
    descriptor->order = qp;

    qp_inflight_wqe_to_frag(ep, qp, to_com_frag(frag));
    qp_reset_signal_count(ep, qp);

    if(ibv_post_send(ep->qps[qp].qp->lcl_qp, &frag->sr_desc, &bad_wr))
        return OMPI_ERROR;

    return OMPI_SUCCESS;
}

#if OPAL_ENABLE_FT_CR == 0
int mca_btl_openib_ft_event(int state) {
    return OMPI_SUCCESS;
}
#else
int mca_btl_openib_ft_event(int state) {
    int i;

    if(OPAL_CRS_CHECKPOINT == state) {
        /* Continue must reconstruct the routes (including modex), since we
         * have to tear down the devices completely. */
        ompi_cr_continue_like_restart = true;

        /*
         * To keep the node from crashing we need to call ibv_close_device
         * before the checkpoint is taken. To do this we need to tear
         * everything down, and rebuild it all on continue/restart. :(
         */

        /* Shutdown all modules
         * - Do this backwards since the openib_finalize function also loops
         *   over this variable.
         */
        for (i = 0; i < mca_btl_openib_component.ib_num_btls; ++i ) {
            mca_btl_openib_finalize_resources( &(mca_btl_openib_component.openib_btls[i])->super);
        }

        mca_btl_openib_component.devices_count = 0;
        mca_btl_openib_component.ib_num_btls = 0;
        OBJ_DESTRUCT(&mca_btl_openib_component.ib_procs);

        ompi_btl_openib_connect_base_finalize();
    }
    else if(OPAL_CRS_CONTINUE == state) {
        ; /* Cleared by forcing the modex, no work needed */
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

#endif /* OPAL_ENABLE_FT_CR */
