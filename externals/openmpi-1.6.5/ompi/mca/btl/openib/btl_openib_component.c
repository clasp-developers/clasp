/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2006-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2009 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2006-2012 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 * Copyright (c) 2009-2012 Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <infiniband/verbs.h>
/* This is crummy, but <infiniband/driver.h> doesn't work on all
   platforms with all compilers.  Specifically, trying to include it
   on RHEL4U3 with the PGI 32 bit compiler will cause problems because
   certain 64 bit types are not defined.  Per advice from Roland D.,
   just include the one prototype that we need in this case
   (ibv_get_sysfs_path()). */
#ifdef HAVE_INFINIBAND_DRIVER_H
#include <infiniband/driver.h>
#else
const char *ibv_get_sysfs_path(void);
#endif
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <stddef.h>
#if BTL_OPENIB_MALLOC_HOOKS_ENABLED
/*
 * The include of malloc.h below breaks abstractions in OMPI (by
 * directly including a header file from another component), but has
 * been ruled "ok" because the openib component is only supported on
 * Linux.  
 *
 * The malloc hooks in newer glibc were deprecated, including stock
 * malloc.h causes compilation warnings.  Instead, we use the internal
 * linux component malloc.h which does not cause these warnings.
 * Internally, OMPI uses the built-in ptmalloc from the linux memory
 * component anyway.
 */
#include "opal/mca/memory/linux/malloc.h"
#endif

#include "opal/event/event.h"
#include "opal/align.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/sys/atomic.h"
#include "opal/util/argv.h"
#include "opal/memoryhooks/memory.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/carto/carto.h"
#include "opal/mca/carto/base/base.h"
#include "opal/mca/paffinity/base/base.h"
#include "opal/mca/installdirs/installdirs.h"
#include "opal/mca/timer/base/base.h"
#include "opal_stdint.h"

#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/notifier/notifier.h"

#include "ompi/constants.h"
#include "ompi/proc/proc.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/mpool/rdma/mpool_rdma.h"
#include "ompi/mca/btl/base/base.h"
#include "ompi/mca/mpool/mpool.h"
#include "ompi/runtime/ompi_module_exchange.h"
#include "ompi/runtime/mpiruntime.h"

#include "btl_openib.h"
#include "btl_openib_frag.h"
#include "btl_openib_endpoint.h"
#include "btl_openib_eager_rdma.h"
#include "btl_openib_proc.h"
#include "btl_openib_ini.h"
#include "btl_openib_mca.h"
#include "btl_openib_xrc.h"
#include "btl_openib_fd.h"
#if BTL_OPENIB_FAILOVER_ENABLED
#include "btl_openib_failover.h"
#endif
#if OPAL_HAVE_THREADS
#include "btl_openib_async.h"
#endif
#include "connect/base.h"
#include "btl_openib_ip.h"
#include "ompi/runtime/params.h"

/*
 * Local functions
 */
static int btl_openib_component_register(void);
static int btl_openib_component_open(void);
static int btl_openib_component_close(void);
static mca_btl_base_module_t **btl_openib_component_init(int*, bool, bool);
static int btl_openib_component_progress(void);

/*
 * Local variables
 */
static mca_btl_openib_device_t *receive_queues_device = NULL;
static bool malloc_hook_set = false;
static int num_devices_intentionally_ignored = 0;

mca_btl_openib_component_t mca_btl_openib_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */

        {
            MCA_BTL_BASE_VERSION_2_0_0,

            "openib", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            btl_openib_component_open,  /* component open */
            btl_openib_component_close,  /* component close */
            NULL, /* component query */
            btl_openib_component_register, /* component register */
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        btl_openib_component_init,
        btl_openib_component_progress,
    }
};

#if BTL_OPENIB_MALLOC_HOOKS_ENABLED
/* This is a memory allocator hook. The purpose of this is to make
 * every malloc aligned since this speeds up IB HCA work.
 * There two basic cases here:
 * 1. Memory manager for Open MPI is enabled. Then memalign below will be
 * overridden by __memalign_hook which is set to opal_memory_linux_memalign_hook.
 * Thus, _malloc_hook is going to use opal_memory_linux_memalign_hook.
 * 2. No memory manager support. The memalign below is just regular glibc
 * memalign which will be called through __malloc_hook instead of malloc.
 */
static void *btl_openib_malloc_hook(size_t sz, const void* caller)
{
    if (sz < mca_btl_openib_component.memalign_threshold) {
        return mca_btl_openib_component.previous_malloc_hook(sz, caller);
    } else {
        return memalign(mca_btl_openib_component.use_memalign, sz);
    }
}
#endif

static int btl_openib_component_register(void)
{
    int ret;

    /* register IB component parameters */
    ret = btl_openib_register_mca_params();

    mca_btl_openib_component.max_send_size =
        mca_btl_openib_module.super.btl_max_send_size;
    mca_btl_openib_component.eager_limit =
        mca_btl_openib_module.super.btl_eager_limit;

    /* if_include and if_exclude need to be mutually exclusive */
    if (OPAL_SUCCESS != 
        mca_base_param_check_exclusive_string(
        mca_btl_openib_component.super.btl_version.mca_type_name,
        mca_btl_openib_component.super.btl_version.mca_component_name,
        "if_include",
        mca_btl_openib_component.super.btl_version.mca_type_name,
        mca_btl_openib_component.super.btl_version.mca_component_name,
        "if_exclude")) {
        /* Return ERR_NOT_AVAILABLE so that a warning message about
           "open" failing is not printed */
        return OMPI_ERR_NOT_AVAILABLE;
    }
    return OMPI_SUCCESS;
}

/*
 *  Called by MCA framework to open the component
 */
static int btl_openib_component_open(void)
{
#if OPAL_HAVE_THREADS
    opal_mutex_t *lock = &mca_btl_openib_component.srq_manager.lock;
    opal_hash_table_t *srq_addr_table = &mca_btl_openib_component.srq_manager.srq_addr_table;

    /* Construct hash table that stores pointers to SRQs */
    OBJ_CONSTRUCT(lock, opal_mutex_t);
    OBJ_CONSTRUCT(srq_addr_table, opal_hash_table_t);
#endif

    /* initialize state */
    mca_btl_openib_component.ib_num_btls = 0;
    mca_btl_openib_component.openib_btls = NULL;
    OBJ_CONSTRUCT(&mca_btl_openib_component.devices, opal_pointer_array_t);
    mca_btl_openib_component.devices_count = 0;
    mca_btl_openib_component.cpc_explicitly_defined = false;
    mca_btl_openib_component.default_recv_qps = NULL;

    /* initialize objects */
    OBJ_CONSTRUCT(&mca_btl_openib_component.ib_procs, opal_list_t);

    srand48(getpid() * time(NULL));
    return OMPI_SUCCESS;
}

/*
 * component cleanup - sanity checking of queue lengths
 */

static int btl_openib_component_close(void)
{
    int rc = OMPI_SUCCESS;

#if OPAL_HAVE_THREADS
    /* Tell the async thread to shutdown */
    if (mca_btl_openib_component.use_async_event_thread &&
        0 != mca_btl_openib_component.async_thread) {
        int async_command = 0;
        if (write(mca_btl_openib_component.async_pipe[1], &async_command,
                  sizeof(int)) < 0) {
            BTL_ERROR(("Failed to communicate with async event thread"));
            rc = OMPI_ERROR;
        } else {
            if (pthread_join(mca_btl_openib_component.async_thread, NULL)) {
                BTL_ERROR(("Failed to stop OpenIB async event thread"));
                rc = OMPI_ERROR;
            }
        }
        close(mca_btl_openib_component.async_pipe[0]);
        close(mca_btl_openib_component.async_pipe[1]);
        close(mca_btl_openib_component.async_comp_pipe[0]);
        close(mca_btl_openib_component.async_comp_pipe[1]);
    }

    OBJ_DESTRUCT(&mca_btl_openib_component.srq_manager.lock);
    OBJ_DESTRUCT(&mca_btl_openib_component.srq_manager.srq_addr_table);
#endif

    ompi_btl_openib_connect_base_finalize();
    ompi_btl_openib_fd_finalize();
    ompi_btl_openib_ini_finalize();
    if (NULL != mca_btl_openib_component.receive_queues) {
        free(mca_btl_openib_component.receive_queues);
    }

    if (NULL != mca_btl_openib_component.default_recv_qps) {
        free(mca_btl_openib_component.default_recv_qps);
    }
    
#if BTL_OPENIB_MALLOC_HOOKS_ENABLED
    /* Must check to see whether the malloc hook was set before 
       assigning it back because ompi_info will call _register() and 
       then _close() (which won't set the hook) */ 
    if (malloc_hook_set) { 
        __malloc_hook = mca_btl_openib_component.previous_malloc_hook; 
    } 
#endif

    return rc;
}

static bool check_basics(void)
{
    int rc;
    char *file;
    struct stat s;

#if defined(__linux__)
    /* Check to see if $sysfsdir/class/infiniband/ exists */
    asprintf(&file, "%s/class/infiniband", ibv_get_sysfs_path());
    if (NULL == file) {
        return false;
    }
    rc = stat(file, &s);
    free(file);
    if (0 != rc || !S_ISDIR(s.st_mode)) {
        return false;
    }
#endif

    /* It exists and is a directory -- good enough */
    return true;
}

static void inline pack8(char **dest, uint8_t value)
{
    /* Copy one character */
    **dest = (char) value;
    /* Most the dest ahead one */
    ++*dest;
}

/*
 *  Register local openib port information with the modex so that it
 *  can be shared with all other peers.
 */
static int btl_openib_modex_send(void)
{
    int rc, i, j;
    int modex_message_size;
    char *message, *offset;
    size_t size, msg_size;
    ompi_btl_openib_connect_base_module_t *cpc;

    opal_output(-1, "Starting to modex send");
    if (0 == mca_btl_openib_component.ib_num_btls) {
        return 0;
    }
    modex_message_size = offsetof(mca_btl_openib_modex_message_t, end);

    /* The message is packed into multiple parts:
     * 1. a uint8_t indicating the number of modules (ports) in the message
     * 2. for each module:
     *    a. the common module data
     *    b. a uint8_t indicating how many CPCs follow
     *    c. for each CPC:
     *       a. a uint8_t indicating the index of the CPC in the all[]
     *          array in btl_openib_connect_base.c
     *       b. a uint8_t indicating the priority of this CPC
     *       c. a uint8_t indicating the length of the blob to follow
     *       d. a blob that is only meaningful to that CPC
     */
    msg_size = 
        /* uint8_t for number of modules in the message */
        1 +
        /* For each module: */
        mca_btl_openib_component.ib_num_btls * 
        (
         /* Common module data */
         modex_message_size + 
         /* uint8_t for how many CPCs follow */
         1
         );
    /* For each module, add in the size of the per-CPC data */
    for (i = 0; i < mca_btl_openib_component.ib_num_btls; i++) {
        for (j = 0; 
             j < mca_btl_openib_component.openib_btls[i]->num_cpcs;
             ++j) {
            msg_size += 
                /* uint8_t for the index of the CPC */
                1 +
                /* uint8_t for the CPC's priority */
                1 + 
                /* uint8_t for the blob length */
                1 +
                /* blob length */
                mca_btl_openib_component.openib_btls[i]->cpcs[j]->data.cbm_modex_message_len;
        }
    }
    message = malloc(msg_size);
    if (NULL == message) {
        BTL_ERROR(("Failed malloc"));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Pack the number of modules */
    offset = message;
    pack8(&offset, mca_btl_openib_component.ib_num_btls);
    opal_output(-1, "modex sending %d btls (packed: %d, offset now at %d)", mca_btl_openib_component.ib_num_btls, *((uint8_t*) message), (int) (offset - message));

    /* Pack each of the modules */
    for (i = 0; i < mca_btl_openib_component.ib_num_btls; i++) {

        /* Pack the modex common message struct.  */
        size = modex_message_size;

        (mca_btl_openib_component.openib_btls[i]->port_info).vendor_id =
            (mca_btl_openib_component.openib_btls[i]->device->ib_dev_attr).vendor_id;

        (mca_btl_openib_component.openib_btls[i]->port_info).vendor_part_id =
            (mca_btl_openib_component.openib_btls[i]->device->ib_dev_attr).vendor_part_id;

        (mca_btl_openib_component.openib_btls[i]->port_info).transport_type =
            mca_btl_openib_get_transport_type(mca_btl_openib_component.openib_btls[i]);

        memcpy(offset, 
               &(mca_btl_openib_component.openib_btls[i]->port_info), 
               size);
        opal_output(-1, "modex packed btl port modex message: 0x%" PRIx64 ", %d, %d (size: %d)",
                    mca_btl_openib_component.openib_btls[i]->port_info.subnet_id,
                    mca_btl_openib_component.openib_btls[i]->port_info.mtu,
                    mca_btl_openib_component.openib_btls[i]->port_info.lid,
                    (int) size);
                    
#if !defined(WORDS_BIGENDIAN) && OPAL_ENABLE_HETEROGENEOUS_SUPPORT
        MCA_BTL_OPENIB_MODEX_MSG_HTON(*(mca_btl_openib_modex_message_t *)offset);
#endif
        offset += size;
        opal_output(-1, "modex packed btl %d: modex message, offset now %d",
                    i, (int) (offset -message));

        /* Pack the number of CPCs that follow */
        pack8(&offset, 
              mca_btl_openib_component.openib_btls[i]->num_cpcs);
        opal_output(-1, "modex packed btl %d: to pack %d cpcs (packed: %d, offset now %d)",
                    i, mca_btl_openib_component.openib_btls[i]->num_cpcs,
                    *((uint8_t*) (offset - 1)), (int) (offset-message));

        /* Pack each CPC */
        for (j = 0; 
             j < mca_btl_openib_component.openib_btls[i]->num_cpcs;
             ++j) {
            uint8_t u8;

            cpc = mca_btl_openib_component.openib_btls[i]->cpcs[j];
            opal_output(-1, "modex packed btl %d: packing cpc %s", 
                        i, cpc->data.cbm_component->cbc_name);
            /* Pack the CPC index */
            u8 = ompi_btl_openib_connect_base_get_cpc_index(cpc->data.cbm_component);
            pack8(&offset, u8);
            opal_output(-1, "packing btl %d: cpc %d: index %d (packed %d, offset now %d)",
                        i, j, u8, *((uint8_t*) (offset-1)), (int)(offset-message));
            /* Pack the CPC priority */
            pack8(&offset, cpc->data.cbm_priority);
            opal_output(-1, "packing btl %d: cpc %d: priority %d (packed %d, offset now %d)",
                        i, j, cpc->data.cbm_priority, *((uint8_t*) (offset-1)), (int)(offset-message));
            /* Pack the blob length */
            u8 = cpc->data.cbm_modex_message_len;
            pack8(&offset, u8);
            opal_output(-1, "packing btl %d: cpc %d: message len %d (packed %d, offset now %d)",
                        i, j, u8, *((uint8_t*) (offset-1)), (int)(offset-message));
            /* If the blob length is > 0, pack the blob */
            if (u8 > 0) {
                memcpy(offset, cpc->data.cbm_modex_message, u8);
                offset += u8;
                opal_output(-1, "packing btl %d: cpc %d: blob packed %d %x (offset now %d)",
                            i, j,
                            ((uint32_t*)cpc->data.cbm_modex_message)[0],
                            ((uint32_t*)cpc->data.cbm_modex_message)[1],
                            (int)(offset-message));
            }

            /* Sanity check */
            assert((size_t) (offset - message) <= msg_size);
        }
    }

    /* All done -- send it! */
    rc = ompi_modex_send(&mca_btl_openib_component.super.btl_version,
                         message, msg_size);
    free(message);
    opal_output(-1, "Modex sent!  %d calculated, %d actual\n", (int) msg_size, (int) (offset - message));

    return rc;
}

/*
 * Active Message Callback function on control message.
 */

static void btl_openib_control(mca_btl_base_module_t* btl,
        mca_btl_base_tag_t tag, mca_btl_base_descriptor_t* des,
        void* cbdata)
{
    /* don't return credits used for control messages */
    mca_btl_openib_module_t *obtl = (mca_btl_openib_module_t*)btl;
    mca_btl_openib_endpoint_t* ep = to_com_frag(des)->endpoint;
    mca_btl_openib_control_header_t *ctl_hdr =
        to_base_frag(des)->segment.seg_addr.pval;
    mca_btl_openib_eager_rdma_header_t *rdma_hdr;
    mca_btl_openib_header_coalesced_t *clsc_hdr =
        (mca_btl_openib_header_coalesced_t*)(ctl_hdr + 1);
    mca_btl_active_message_callback_t* reg;
    size_t len = des->des_dst->seg_len - sizeof(*ctl_hdr);

    switch (ctl_hdr->type) {
    case MCA_BTL_OPENIB_CONTROL_CREDITS:
        assert(0); /* Credit message is handled elsewhere */
        break;
    case MCA_BTL_OPENIB_CONTROL_RDMA:
       rdma_hdr = (mca_btl_openib_eager_rdma_header_t*)ctl_hdr;

       BTL_VERBOSE(("prior to NTOH received  rkey %" PRIu32 
                    ", rdma_start.lval %" PRIx64 ", pval %p, ival %" PRIu32,
                    rdma_hdr->rkey,
                    rdma_hdr->rdma_start.lval,
                    rdma_hdr->rdma_start.pval,
                    rdma_hdr->rdma_start.ival
                  ));

       if(ep->nbo) {
           BTL_OPENIB_EAGER_RDMA_CONTROL_HEADER_NTOH(*rdma_hdr);
       }

       BTL_VERBOSE(("received  rkey %" PRIu32 
                    ", rdma_start.lval %" PRIx64 ", pval %p,"
                    " ival %" PRIu32, rdma_hdr->rkey,
                    rdma_hdr->rdma_start.lval,
                    rdma_hdr->rdma_start.pval, rdma_hdr->rdma_start.ival));

       if (ep->eager_rdma_remote.base.pval) {
           BTL_ERROR(("Got RDMA connect twice!"));
           return;
       }
       ep->eager_rdma_remote.rkey = rdma_hdr->rkey;
       ep->eager_rdma_remote.base.lval = rdma_hdr->rdma_start.lval;
       ep->eager_rdma_remote.tokens=mca_btl_openib_component.eager_rdma_num - 1;
       break;
    case MCA_BTL_OPENIB_CONTROL_COALESCED:
        {
            size_t pad = 0;
            while(len > 0) {
                size_t skip;
                mca_btl_openib_header_coalesced_t* unalign_hdr = 0;
                mca_btl_base_descriptor_t tmp_des;
                mca_btl_base_segment_t tmp_seg;

                assert(len >= sizeof(*clsc_hdr));

                if(ep->nbo)
                    BTL_OPENIB_HEADER_COALESCED_NTOH(*clsc_hdr);

                skip = (sizeof(*clsc_hdr) + clsc_hdr->alloc_size - pad);

                tmp_des.des_dst = &tmp_seg;
                tmp_des.des_dst_cnt = 1;
                tmp_seg.seg_addr.pval = clsc_hdr + 1;
                tmp_seg.seg_len = clsc_hdr->size;

                /* call registered callback */
                reg = mca_btl_base_active_message_trigger + clsc_hdr->tag;
                reg->cbfunc( &obtl->super, clsc_hdr->tag, &tmp_des, reg->cbdata );
                len -= (skip + pad);
                unalign_hdr = (mca_btl_openib_header_coalesced_t*)
                    ((unsigned char*)clsc_hdr + skip);
                pad = (size_t)BTL_OPENIB_COALESCE_HDR_PADDING(unalign_hdr);
                clsc_hdr = (mca_btl_openib_header_coalesced_t*)((unsigned char*)unalign_hdr +
                                                                pad);
            }
        }
       break;
    case MCA_BTL_OPENIB_CONTROL_CTS:
        OPAL_OUTPUT((-1, "received CTS from %s (buffer %p): posted recvs %d, sent cts %d",
                     ep->endpoint_proc->proc_ompi->proc_hostname,
                     (void*) ctl_hdr,
                     ep->endpoint_posted_recvs, ep->endpoint_cts_sent));
        ep->endpoint_cts_received = true;

        /* Only send the CTS back and mark connected if:
           - we have posted our receives (it's possible that we can
             get this CTS before this side's CPC has called
             cpc_complete())
           - we have not yet sent our CTS

           We don't even want to mark the endpoint connected() until
           we have posted our receives because otherwise we will
           trigger credit management (because the rd_credits will
           still be negative), and Bad Things will happen. */
        if (ep->endpoint_posted_recvs) {
            if (!ep->endpoint_cts_sent) {
                mca_btl_openib_endpoint_send_cts(ep);
            }
            mca_btl_openib_endpoint_connected(ep);
        }
        break;
#if BTL_OPENIB_FAILOVER_ENABLED
    case MCA_BTL_OPENIB_CONTROL_EP_BROKEN:
    case MCA_BTL_OPENIB_CONTROL_EP_EAGER_RDMA_ERROR:
        btl_openib_handle_failover_control_messages(ctl_hdr, ep);
        break;
#endif
    default:
        BTL_ERROR(("Unknown message type received by BTL"));
       break;
    }
}

static int openib_reg_mr(void *reg_data, void *base, size_t size,
        mca_mpool_base_registration_t *reg)
{
    mca_btl_openib_device_t *device = (mca_btl_openib_device_t*)reg_data;
    mca_btl_openib_reg_t *openib_reg = (mca_btl_openib_reg_t*)reg;

    enum ibv_access_flags access_flag = IBV_ACCESS_LOCAL_WRITE |
        IBV_ACCESS_REMOTE_WRITE | IBV_ACCESS_REMOTE_READ;

#if HAVE_DECL_IBV_ACCESS_SO
    if (reg->flags & MCA_MPOOL_FLAGS_SO_MEM) {
        access_flag |= IBV_ACCESS_SO;
    }
#endif

    if (device->mem_reg_max &&
        device->mem_reg_max < (device->mem_reg_active + size)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    device->mem_reg_active += size;

    openib_reg->mr = ibv_reg_mr(device->ib_pd, base, size, access_flag);
 
    if (NULL == openib_reg->mr) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    return OMPI_SUCCESS;
}

static int openib_dereg_mr(void *reg_data, mca_mpool_base_registration_t *reg)
{
    mca_btl_openib_device_t *device = (mca_btl_openib_device_t*)reg_data;
    mca_btl_openib_reg_t *openib_reg = (mca_btl_openib_reg_t*)reg;

    if(openib_reg->mr != NULL) {
        if(ibv_dereg_mr(openib_reg->mr)) {
            BTL_ERROR(("%s: error unpinning openib memory errno says %s",
                       __func__, strerror(errno)));
            return OMPI_ERROR;
        }
    }

    device->mem_reg_active -= (uint64_t) (reg->bound - reg->base + 1);

    openib_reg->mr = NULL;
    return OMPI_SUCCESS;
}
static inline int param_register_int(const char* param_name, int default_value)
{
    int param_value = default_value;
    int id = mca_base_param_register_int("btl", "openib", param_name, NULL,
            default_value);
    mca_base_param_lookup_int(id, &param_value);
    return param_value;
}

#if OPAL_HAVE_THREADS
static int start_async_event_thread(void)
{
    /* Set the error counter to zero */
    mca_btl_openib_component.error_counter = 0;

    /* Create pipe for communication with async event thread */
    if(pipe(mca_btl_openib_component.async_pipe)) {
        BTL_ERROR(("Failed to create pipe for communication with "
                    "async event thread"));
        return OMPI_ERROR;
    }

    if(pipe(mca_btl_openib_component.async_comp_pipe)) {
        BTL_ERROR(("Failed to create comp pipe for communication with "
                    "main thread"));
        return OMPI_ERROR;
    }

    /* Starting async event thread for the component */
    if(pthread_create(&mca_btl_openib_component.async_thread, NULL,
                (void*(*)(void*))btl_openib_async_thread, NULL)) {
        BTL_ERROR(("Failed to create async event thread"));
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}
#endif

static int init_one_port(opal_list_t *btl_list, mca_btl_openib_device_t *device,
                         uint8_t port_num, uint16_t pkey_index,
                         struct ibv_port_attr *ib_port_attr)
{
    uint16_t lid, i, lmc, lmc_step;
    mca_btl_openib_module_t *openib_btl;
    mca_btl_base_selected_module_t *ib_selected;
    union ibv_gid gid;
    uint64_t subnet_id;

    /* Ensure that the requested GID index (via the
       btl_openib_gid_index MCA param) is within the GID table
       size. */
    if (mca_btl_openib_component.gid_index >
        ib_port_attr->gid_tbl_len) {
        orte_show_help("help-mpi-btl-openib.txt", "gid index too large",
                       true, orte_process_info.nodename,
                       ibv_get_device_name(device->ib_dev), port_num,
                       mca_btl_openib_component.gid_index,
                       ib_port_attr->gid_tbl_len);
        return OMPI_ERR_NOT_FOUND;
    }
    BTL_VERBOSE(("looking for %s:%d GID index %d",
                 ibv_get_device_name(device->ib_dev), port_num,
                 mca_btl_openib_component.gid_index));

    /* If we have struct ibv_device.transport_type, then we're >= OFED
       v1.2, and the transport could be iWarp or IB.  If we don't have
       that member, then we're < OFED v1.2, and it can only be IB. */
#if defined(HAVE_STRUCT_IBV_DEVICE_TRANSPORT_TYPE)
    if (IBV_TRANSPORT_IWARP == device->ib_dev->transport_type) {
        subnet_id = mca_btl_openib_get_ip_subnet_id(device->ib_dev, port_num);
        BTL_VERBOSE(("my iWARP subnet_id is %016" PRIx64, subnet_id));
    } else {
        memset(&gid, 0, sizeof(gid));
        if (0 != ibv_query_gid(device->ib_dev_context, port_num, 
                               mca_btl_openib_component.gid_index, &gid)) {
            BTL_ERROR(("ibv_query_gid failed (%s:%d, %d)\n",
                       ibv_get_device_name(device->ib_dev), port_num,
                       mca_btl_openib_component.gid_index));
            return OMPI_ERR_NOT_FOUND;
        }

#ifdef OMPI_HAVE_RDMAOE
        if (IBV_LINK_LAYER_ETHERNET == ib_port_attr->link_layer) {
            subnet_id = mca_btl_openib_get_ip_subnet_id(device->ib_dev, 
                                                           port_num);
        } else {
            subnet_id = ntoh64(gid.global.subnet_prefix);
        }
#else
        subnet_id = ntoh64(gid.global.subnet_prefix);
#endif

        BTL_VERBOSE(("my IB subnet_id for HCA %s port %d is %016" PRIx64, 
                     ibv_get_device_name(device->ib_dev), port_num, subnet_id));
    }
#else
    if (0 != ibv_query_gid(device->ib_dev_context, port_num, 
                           mca_btl_openib_component.gid_index, &gid)) {
        BTL_ERROR(("ibv_query_gid failed (%s:%d, %d)\n",
                   ibv_get_device_name(device->ib_dev), port_num,
                   mca_btl_openib_component.gid_index));
        return OMPI_ERR_NOT_FOUND;
    }
    subnet_id = ntoh64(gid.global.subnet_prefix);
    BTL_VERBOSE(("my IB-only subnet_id for HCA %s port %d is %016" PRIx64, 
                 ibv_get_device_name(device->ib_dev), port_num, subnet_id));
#endif

    if(mca_btl_openib_component.ib_num_btls > 0 &&
            IB_DEFAULT_GID_PREFIX == subnet_id &&
            mca_btl_openib_component.warn_default_gid_prefix) {
        orte_show_help("help-mpi-btl-openib.txt", "default subnet prefix",
                true, orte_process_info.nodename);
    }

    lmc = (1 << ib_port_attr->lmc);
    lmc_step = 1;

    if (0 != mca_btl_openib_component.max_lmc &&
        mca_btl_openib_component.max_lmc < lmc) {
        lmc = mca_btl_openib_component.max_lmc;
    }

#if OPAL_HAVE_THREADS
    /* APM support -- only meaningful if async event support is
       enabled.  If async events are not enabled, then there's nothing
       to listen for the APM event to load the new path, so it's not
       worth enabling APM.  */
    if (lmc > 1){
        if (-1 == mca_btl_openib_component.apm_lmc) {
            lmc_step = lmc;
            mca_btl_openib_component.apm_lmc = lmc - 1;
        } else if (0 == lmc % (mca_btl_openib_component.apm_lmc + 1)) {
            lmc_step = mca_btl_openib_component.apm_lmc + 1;
        } else {
            orte_show_help("help-mpi-btl-openib.txt", "apm with wrong lmc",true,
                    mca_btl_openib_component.apm_lmc, lmc);
            return OMPI_ERROR;
        }
    } else {
        if (mca_btl_openib_component.apm_lmc) {
            /* Disable apm and report warning */
            mca_btl_openib_component.apm_lmc = 0;
            orte_show_help("help-mpi-btl-openib.txt", "apm without lmc",true);
        }
    }
#endif

    for(lid = ib_port_attr->lid;
            lid < ib_port_attr->lid + lmc; lid += lmc_step){
        for(i = 0; i < mca_btl_openib_component.btls_per_lid; i++){
            char param[40];

            openib_btl = calloc(1, sizeof(mca_btl_openib_module_t));
            if(NULL == openib_btl) {
                BTL_ERROR(("Failed malloc: %s:%d", __FILE__, __LINE__));
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
            memcpy(openib_btl, &mca_btl_openib_module,
                    sizeof(mca_btl_openib_module));
            memcpy(&openib_btl->ib_port_attr, ib_port_attr,
                    sizeof(struct ibv_port_attr));
            ib_selected = OBJ_NEW(mca_btl_base_selected_module_t);
            ib_selected->btl_module = (mca_btl_base_module_t*) openib_btl;
            openib_btl->device = device;
            openib_btl->port_num = (uint8_t) port_num;
            openib_btl->pkey_index = pkey_index;
            openib_btl->lid = lid;
            openib_btl->apm_port = 0;
            openib_btl->src_path_bits = lid - ib_port_attr->lid;

            openib_btl->port_info.subnet_id = subnet_id;
            openib_btl->port_info.mtu = device->mtu;
            openib_btl->port_info.lid = lid;

            openib_btl->cpcs = NULL;
            openib_btl->num_cpcs = 0;
            openib_btl->local_procs = 0;

            mca_btl_base_active_message_trigger[MCA_BTL_TAG_IB].cbfunc = btl_openib_control;
            mca_btl_base_active_message_trigger[MCA_BTL_TAG_IB].cbdata = NULL;

            /* Check bandwidth configured for this device */
            sprintf(param, "bandwidth_%s", ibv_get_device_name(device->ib_dev));
            openib_btl->super.btl_bandwidth =
                param_register_int(param, openib_btl->super.btl_bandwidth);

            /* Check bandwidth configured for this device/port */
            sprintf(param, "bandwidth_%s:%d", ibv_get_device_name(device->ib_dev),
                    port_num);
            openib_btl->super.btl_bandwidth =
                param_register_int(param, openib_btl->super.btl_bandwidth);

            /* Check bandwidth configured for this device/port/LID */
            sprintf(param, "bandwidth_%s:%d:%d",
                    ibv_get_device_name(device->ib_dev), port_num, lid);
            openib_btl->super.btl_bandwidth =
                param_register_int(param, openib_btl->super.btl_bandwidth);

            /* Check latency configured for this device */
            sprintf(param, "latency_%s", ibv_get_device_name(device->ib_dev));
            openib_btl->super.btl_latency =
                param_register_int(param, openib_btl->super.btl_latency);

            /* Check latency configured for this device/port */
            sprintf(param, "latency_%s:%d", ibv_get_device_name(device->ib_dev),
                    port_num);
            openib_btl->super.btl_latency =
                param_register_int(param, openib_btl->super.btl_latency);

            /* Check latency configured for this device/port/LID */
            sprintf(param, "latency_%s:%d:%d", ibv_get_device_name(device->ib_dev),
                    port_num, lid);
            openib_btl->super.btl_latency =
                param_register_int(param, openib_btl->super.btl_latency);

            /* Auto-detect the port bandwidth */
            if (0 == openib_btl->super.btl_bandwidth) {
                /* To calculate the bandwidth available on this port,
                   we have to look up the values corresponding to
                   port->active_speed and port->active_width.  These
                   are enums corresponding to the IB spec.  Overall
                   forumula to get the true link speed is 8/10 or
                   64/66 of the reported speed (depends on the coding
                   that is being used for the particular speed) times
                   the number of links. */
                switch (ib_port_attr->active_speed) {
                case 1:
                    /* SDR: 2.5 Gbps * 0.8, in megabits */
                    openib_btl->super.btl_bandwidth = 2000;
                    break;
                case 2:
                    /* DDR: 5 Gbps * 0.8, in megabits */
                    openib_btl->super.btl_bandwidth = 4000;
                    break;
                case 4:
                    /* QDR: 10 Gbps * 0.8, in megabits */
                    openib_btl->super.btl_bandwidth = 8000;
                    break;
                case 8:
                    /* FDR10: 10 Gbps * 64/66, in megabits */
                    openib_btl->super.btl_bandwidth = 9700;
                    break;
                case 16:
                    /* FDR: 14 Gbps * 64/66, in megabits */
                    openib_btl->super.btl_bandwidth = 13600;
                    break;
                case 32:
                    /* EDR: 25 Gbps * 64/66, in megabits */
                    openib_btl->super.btl_bandwidth = 24200;
                    break;
                default:
                    /* Who knows?  Declare this port unreachable (do
                       *not* return ERR_VALUE_OF_OUT_OF_BOUNDS; that
                       is reserved for when we exceed the number of
                       allowable BTLs). */
                    return OMPI_ERR_UNREACH;
                }
                switch (ib_port_attr->active_width) {
                case 1:
                    /* 1x */
                    /* unity */
                    break;
                case 2:
                    /* 4x */
                    openib_btl->super.btl_bandwidth *= 4;
                    break;
                case 4:
                    /* 8x */
                    openib_btl->super.btl_bandwidth *= 8;
                    break;
                case 8:
                    /* 12x */
                    openib_btl->super.btl_bandwidth *= 12;
                    break;
                default:
                    /* Who knows?  Declare this port unreachable (do
                       *not* return ERR_VALUE_OF_OUT_OF_BOUNDS; that
                       is reserved for when we exceed the number of
                       allowable BTLs). */
                    return OMPI_ERR_UNREACH;
                }
            }
            opal_list_append(btl_list, (opal_list_item_t*) ib_selected);
            opal_pointer_array_add(device->device_btls, (void*) openib_btl);
            ++device->btls;
            ++mca_btl_openib_component.ib_num_btls;
            if (-1 != mca_btl_openib_component.ib_max_btls &&
                mca_btl_openib_component.ib_num_btls >=
                mca_btl_openib_component.ib_max_btls) {
                return OMPI_ERR_VALUE_OUT_OF_BOUNDS;
            }
        }
    }

    return OMPI_SUCCESS;
}

static void device_construct(mca_btl_openib_device_t *device)
{
    device->ib_dev = NULL;
    device->ib_dev_context = NULL;
    device->ib_pd = NULL;
    device->mpool = NULL;
#if OPAL_ENABLE_PROGRESS_THREADS
    device->ib_channel = NULL;
#endif
    device->btls = 0;
    device->endpoints = NULL;
    device->device_btls = NULL;
    device->ib_cq[BTL_OPENIB_HP_CQ] = NULL;
    device->ib_cq[BTL_OPENIB_LP_CQ] = NULL;
    device->cq_size[BTL_OPENIB_HP_CQ] = 0;
    device->cq_size[BTL_OPENIB_LP_CQ] = 0;
    device->non_eager_rdma_endpoints = 0;
    device->hp_cq_polls = mca_btl_openib_component.cq_poll_ratio;
    device->eager_rdma_polls = mca_btl_openib_component.eager_rdma_poll_ratio;
    device->pollme = true;
    device->eager_rdma_buffers_count = 0;
    device->eager_rdma_buffers = NULL;
#if HAVE_XRC
    device->xrc_fd = -1;
#endif
    device->qps = NULL;
#if OPAL_HAVE_THREADS
    mca_btl_openib_component.async_pipe[0] = 
        mca_btl_openib_component.async_pipe[1] = -1;
    mca_btl_openib_component.async_comp_pipe[0] = 
        mca_btl_openib_component.async_comp_pipe[1] = -1;
#endif
    OBJ_CONSTRUCT(&device->device_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&device->send_free_control, ompi_free_list_t);
    device->max_inline_data = 0;
}

static void device_destruct(mca_btl_openib_device_t *device)
{
    int i;

#if OPAL_HAVE_THREADS
#if OPAL_ENABLE_PROGRESS_THREADS
    if(device->progress) {
        device->progress = false;
        if (pthread_cancel(device->thread.t_handle)) {
            BTL_ERROR(("Failed to cancel OpenIB progress thread"));
            goto device_error;
        }
        opal_thread_join(&device->thread, NULL);
    }
    if (ibv_destroy_comp_channel(device->ib_channel)) {
        BTL_VERBOSE(("Failed to close comp_channel"));
        goto device_error;
    }
#endif
    /* signaling to async_tread to stop poll for this device */
    if (mca_btl_openib_component.use_async_event_thread &&
        -1 != mca_btl_openib_component.async_pipe[1]) {
        int device_to_remove;
        device_to_remove = -(device->ib_dev_context->async_fd);
        if (write(mca_btl_openib_component.async_pipe[1], &device_to_remove,
                    sizeof(int)) < 0){
            BTL_ERROR(("Failed to write to pipe"));
            goto device_error;
        }
        /* wait for ok from thread */
        if (OMPI_SUCCESS != btl_openib_async_command_done(device_to_remove)){
            goto device_error;
        } 
    }
#endif

    if(device->eager_rdma_buffers) {
        int i;
        for(i = 0; i < device->eager_rdma_buffers_count; i++)
            if(device->eager_rdma_buffers[i])
                OBJ_RELEASE(device->eager_rdma_buffers[i]);
        free(device->eager_rdma_buffers);
    }

    if (NULL != device->qps) {
        for (i = 0; i < mca_btl_openib_component.num_qps; i++) {
            OBJ_DESTRUCT(&device->qps[i].send_free);
            OBJ_DESTRUCT(&device->qps[i].recv_free);
        }
        free(device->qps);
    }

    OBJ_DESTRUCT(&device->send_free_control);

    /* Release CQs */
    if(device->ib_cq[BTL_OPENIB_HP_CQ] != NULL) {
        if (ibv_destroy_cq(device->ib_cq[BTL_OPENIB_HP_CQ])) {
            BTL_VERBOSE(("Failed to close HP CQ"));
            goto device_error;
        }
    }

    if(device->ib_cq[BTL_OPENIB_LP_CQ] != NULL) {
        if (ibv_destroy_cq(device->ib_cq[BTL_OPENIB_LP_CQ])) {
            BTL_VERBOSE(("Failed to close LP CQ"));
            goto device_error;
        }
    }

    if (OMPI_SUCCESS != mca_mpool_base_module_destroy(device->mpool)) {
        BTL_VERBOSE(("Failed to release mpool"));
        goto device_error;
    }

#if HAVE_XRC
    if (MCA_BTL_XRC_ENABLED) {
        if (OMPI_SUCCESS != mca_btl_openib_close_xrc_domain(device)) {
            BTL_VERBOSE(("XRC Internal error. Failed to close xrc domain"));
            goto device_error;
        }
    }
#endif

    if (ibv_dealloc_pd(device->ib_pd)) {
        BTL_VERBOSE(("Warning! Failed to release PD"));
        goto device_error;
    }

    OBJ_DESTRUCT(&device->device_lock);

    if (ibv_close_device(device->ib_dev_context)) {
        if (1 == ompi_mpi_leave_pinned || ompi_mpi_leave_pinned_pipeline) {
            BTL_VERBOSE(("Warning! Failed to close device"));
            goto device_error;
        } else {
            BTL_ERROR(("Error! Failed to close device"));
            goto device_error;
        }
    }
    BTL_VERBOSE(("device was successfully released"));
    return;
device_error:
    BTL_VERBOSE(("Failed to destroy device resources"));
}

OBJ_CLASS_INSTANCE(mca_btl_openib_device_t, opal_object_t, device_construct,
        device_destruct);

static int prepare_device_for_use(mca_btl_openib_device_t *device)
{
    mca_btl_openib_frag_init_data_t *init_data;
    int rc, qp, length;

#if OPAL_HAVE_THREADS
    if(mca_btl_openib_component.use_async_event_thread) {
        if(0 == mca_btl_openib_component.async_thread) {
            /* async thread is not yet started, so start it here */
            if(start_async_event_thread() != OMPI_SUCCESS)
                return OMPI_ERROR;
        }
        device->got_fatal_event = false;
        device->got_port_event = false;
        if (write(mca_btl_openib_component.async_pipe[1],
                    &device->ib_dev_context->async_fd, sizeof(int))<0){
            BTL_ERROR(("Failed to write to pipe [%d]",errno));
            return OMPI_ERROR;
        }
        /* wait for ok from thread */
        if (OMPI_SUCCESS != 
            btl_openib_async_command_done(device->ib_dev_context->async_fd)) {
            return OMPI_ERROR;
        } 
    }
#if OPAL_ENABLE_PROGRESS_THREADS == 1
    /* Prepare data for thread, but not starting it */
    OBJ_CONSTRUCT(&device->thread, opal_thread_t);
    device->thread.t_run = mca_btl_openib_progress_thread;
    device->thread.t_arg = device;
    device->progress = false;
#endif
#endif

#if HAVE_XRC
    /* if user configured to run with XRC qp and the device doesn't
     * support it - we should ignore this device. Maybe we have another
     * one that has XRC support
     */
    if (!(device->ib_dev_attr.device_cap_flags & IBV_DEVICE_XRC) &&
            MCA_BTL_XRC_ENABLED) {
        orte_show_help("help-mpi-btl-openib.txt",
                "XRC on device without XRC support", true,
                mca_btl_openib_component.num_xrc_qps,
                ibv_get_device_name(device->ib_dev),
                orte_process_info.nodename);
        return OMPI_ERROR;
    }

    if (MCA_BTL_XRC_ENABLED) {
        if (OMPI_SUCCESS != mca_btl_openib_open_xrc_domain(device)) {
            BTL_ERROR(("XRC Internal error. Failed to open xrc domain"));
            return OMPI_ERROR;
        }
    }
#endif

    device->endpoints = OBJ_NEW(opal_pointer_array_t);
    opal_pointer_array_init(device->endpoints, 10, INT_MAX, 10);
    opal_pointer_array_add(&mca_btl_openib_component.devices, device);
    if (mca_btl_openib_component.max_eager_rdma > 0 &&
        device->use_eager_rdma) {
        device->eager_rdma_buffers =
            calloc(mca_btl_openib_component.max_eager_rdma * device->btls,
                    sizeof(mca_btl_openib_endpoint_t*));
        if(NULL == device->eager_rdma_buffers) {
            BTL_ERROR(("Memory allocation fails"));
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
    }

    init_data = malloc(sizeof(mca_btl_openib_frag_init_data_t));
    length = sizeof(mca_btl_openib_header_t) +
        sizeof(mca_btl_openib_footer_t) +
        sizeof(mca_btl_openib_eager_rdma_header_t);

    init_data->order = MCA_BTL_NO_ORDER;
    init_data->list = &device->send_free_control;

    rc = ompi_free_list_init_ex_new(&device->send_free_control,
                sizeof(mca_btl_openib_send_control_frag_t), opal_cache_line_size,
                OBJ_CLASS(mca_btl_openib_send_control_frag_t), length,
                mca_btl_openib_component.buffer_alignment,
                mca_btl_openib_component.ib_free_list_num, -1,
                mca_btl_openib_component.ib_free_list_inc,
                device->mpool, mca_btl_openib_frag_init,
                init_data);
    if (OMPI_SUCCESS != rc) {
        /* If we're "out of memory", this usually means that we ran
           out of registered memory, so show that error message */
        if (OMPI_ERR_OUT_OF_RESOURCE == rc ||
            OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc) {
            errno = ENOMEM;
            mca_btl_openib_show_init_error(__FILE__, __LINE__,
                                           "ompi_free_list_init_ex_new",
                                           ibv_get_device_name(device->ib_dev));
        }
        return rc;
    }

    /* setup all the qps */
    for(qp = 0; qp < mca_btl_openib_component.num_qps; qp++) {
        init_data = malloc(sizeof(mca_btl_openib_frag_init_data_t));
        /* Initialize pool of send fragments */
        length = sizeof(mca_btl_openib_header_t) +
            sizeof(mca_btl_openib_header_coalesced_t) +
            sizeof(mca_btl_openib_control_header_t) +
            sizeof(mca_btl_openib_footer_t) +
            mca_btl_openib_component.qp_infos[qp].size;

        init_data->order = qp;
        init_data->list = &device->qps[qp].send_free;

        rc = ompi_free_list_init_ex_new(init_data->list,
                    sizeof(mca_btl_openib_send_frag_t), opal_cache_line_size,
                    OBJ_CLASS(mca_btl_openib_send_frag_t), length,
                    mca_btl_openib_component.buffer_alignment,
                    mca_btl_openib_component.ib_free_list_num,
                    mca_btl_openib_component.ib_free_list_max,
                    mca_btl_openib_component.ib_free_list_inc,
                    device->mpool, mca_btl_openib_frag_init,
                                        init_data);
        if (OMPI_SUCCESS != rc) {
            /* If we're "out of memory", this usually means that we
               ran out of registered memory, so show that error
               message */
            if (OMPI_ERR_OUT_OF_RESOURCE == rc ||
                OMPI_ERR_TEMP_OUT_OF_RESOURCE == rc) {
                errno = ENOMEM;
                mca_btl_openib_show_init_error(__FILE__, __LINE__,
                                               "ompi_free_list_init_ex_new",
                                               ibv_get_device_name(device->ib_dev));
            }
            return OMPI_ERROR;
        }

        init_data = malloc(sizeof(mca_btl_openib_frag_init_data_t));
        length = sizeof(mca_btl_openib_header_t) +
            sizeof(mca_btl_openib_header_coalesced_t) +
            sizeof(mca_btl_openib_control_header_t) +
            sizeof(mca_btl_openib_footer_t) +
            mca_btl_openib_component.qp_infos[qp].size;

        init_data->order = qp;
        init_data->list = &device->qps[qp].recv_free;

        if(OMPI_SUCCESS != ompi_free_list_init_ex_new(init_data->list,
                    sizeof(mca_btl_openib_recv_frag_t), opal_cache_line_size,
                    OBJ_CLASS(mca_btl_openib_recv_frag_t),
                    length, mca_btl_openib_component.buffer_alignment,
                    mca_btl_openib_component.ib_free_list_num,
                    mca_btl_openib_component.ib_free_list_max,
                    mca_btl_openib_component.ib_free_list_inc,
                    device->mpool, mca_btl_openib_frag_init,
                    init_data)) {
            return OMPI_ERROR;
        }
    }

    return OMPI_SUCCESS;
}

static int
get_port_list(mca_btl_openib_device_t *device, int *allowed_ports)
{
    int i, j, k, num_ports = 0;
    const char *dev_name;
    char *name;

    dev_name = ibv_get_device_name(device->ib_dev);
    name = (char*) malloc(strlen(dev_name) + 4);
    if (NULL == name) {
        return 0;
    }

    /* Assume that all ports are allowed.  num_ports will be adjusted
       below to reflect whether this is true or not. */
    for (i = 1; i <= device->ib_dev_attr.phys_port_cnt; ++i) {
        allowed_ports[num_ports++] = i;
    }
    num_ports = 0;
    if (NULL != mca_btl_openib_component.if_include_list) {
        /* If only the device name is given (eg. mtdevice0,mtdevice1) use all
           ports */
        i = 0;
        while (mca_btl_openib_component.if_include_list[i]) {
            if (0 == strcmp(dev_name,
                            mca_btl_openib_component.if_include_list[i])) {
                num_ports = device->ib_dev_attr.phys_port_cnt;
                goto done;
            }
            ++i;
        }
        /* Include only requested ports on the device */
        for (i = 1; i <= device->ib_dev_attr.phys_port_cnt; ++i) {
            sprintf(name,"%s:%d",dev_name,i);
            for (j = 0;
                 NULL != mca_btl_openib_component.if_include_list[j]; ++j) {
                if (0 == strcmp(name,
                                mca_btl_openib_component.if_include_list[j])) {
                    allowed_ports[num_ports++] = i;
                    break;
                }
            }
        }
    } else if (NULL != mca_btl_openib_component.if_exclude_list) {
        /* If only the device name is given (eg. mtdevice0,mtdevice1) exclude
           all ports */
        i = 0;
        while (mca_btl_openib_component.if_exclude_list[i]) {
            if (0 == strcmp(dev_name,
                            mca_btl_openib_component.if_exclude_list[i])) {
                num_ports = 0;
                goto done;
            }
            ++i;
        }
        /* Exclude the specified ports on this device */
        for (i = 1; i <= device->ib_dev_attr.phys_port_cnt; ++i) {
            sprintf(name,"%s:%d",dev_name,i);
            for (j = 0;
                 NULL != mca_btl_openib_component.if_exclude_list[j]; ++j) {
                if (0 == strcmp(name,
                                mca_btl_openib_component.if_exclude_list[j])) {
                    /* If found, set a sentinel value */
                    j = -1;
                    break;
                }
            }
            /* If we didn't find it, it's ok to include in the list */
            if (-1 != j) {
                allowed_ports[num_ports++] = i;
            }
        }
    } else {
        num_ports = device->ib_dev_attr.phys_port_cnt;
    }

done:

    /* Remove the following from the error-checking if_list:
       - bare device name
       - device name suffixed with port number */
    if (NULL != mca_btl_openib_component.if_list) {
        for (i = 0; NULL != mca_btl_openib_component.if_list[i]; ++i) {

            /* Look for raw device name */
            if (0 == strcmp(mca_btl_openib_component.if_list[i], dev_name)) {
                j = opal_argv_count(mca_btl_openib_component.if_list);
                opal_argv_delete(&j, &(mca_btl_openib_component.if_list),
                                 i, 1);
                --i;
            }
        }
        for (i = 1; i <= device->ib_dev_attr.phys_port_cnt; ++i) {
            sprintf(name, "%s:%d", dev_name, i);
            for (j = 0; NULL != mca_btl_openib_component.if_list[j]; ++j) {
                if (0 == strcmp(mca_btl_openib_component.if_list[j], name)) {
                    k = opal_argv_count(mca_btl_openib_component.if_list);
                    opal_argv_delete(&k, &(mca_btl_openib_component.if_list),
                                     j, 1);
                    --j;
                    break;
                }
            }
        }
    }

    free(name);

    return num_ports;
}

/*
 * Prefer values that are already in the target
 */
static void merge_values(ompi_btl_openib_ini_values_t *target,
                         ompi_btl_openib_ini_values_t *src)
{
    if (!target->mtu_set && src->mtu_set) {
        target->mtu = src->mtu;
        target->mtu_set = true;
    }

    if (!target->use_eager_rdma_set && src->use_eager_rdma_set) {
        target->use_eager_rdma = src->use_eager_rdma;
        target->use_eager_rdma_set = true;
    }

    if (NULL == target->receive_queues && NULL != src->receive_queues) {
        target->receive_queues = strdup(src->receive_queues);
    }

    if (!target->max_inline_data_set && src->max_inline_data_set) {
        target->max_inline_data = src->max_inline_data;
        target->max_inline_data_set = true;
    }
}

static bool inline is_credit_message(const mca_btl_openib_recv_frag_t *frag)
{
    mca_btl_openib_control_header_t* chdr =
        to_base_frag(frag)->segment.seg_addr.pval;
    return (MCA_BTL_TAG_BTL == frag->hdr->tag) &&
        (MCA_BTL_OPENIB_CONTROL_CREDITS == chdr->type);
}

static bool inline is_cts_message(const mca_btl_openib_recv_frag_t *frag)
{
    mca_btl_openib_control_header_t* chdr =
        to_base_frag(frag)->segment.seg_addr.pval;
    return (MCA_BTL_TAG_BTL == frag->hdr->tag) &&
        (MCA_BTL_OPENIB_CONTROL_CTS == chdr->type);
}

static int32_t atoi_param(char *param, int32_t dflt)
{
    if (NULL == param || '\0' == param[0]) {
        return dflt ? dflt : 1;
    }

    return atoi(param);
}

static void init_apm_port(mca_btl_openib_device_t *device, int port, uint16_t lid)
{
    int index;
    struct mca_btl_openib_module_t *btl;
    for(index = 0; index < device->btls; index++) {
        btl = opal_pointer_array_get_item(device->device_btls, index);
        /* Ok, we already have btl for the fist port,
         * second one will be used for APM */
        btl->apm_port = port;
        btl->port_info.apm_lid = lid + btl->src_path_bits;
        mca_btl_openib_component.apm_ports++;
        BTL_VERBOSE(("APM-PORT: Setting alternative port - %d, lid - %d"
                    ,port ,lid));
    }
}

static int setup_qps(void)
{
    char **queues, **params = NULL;
    int num_xrc_qps = 0, num_pp_qps = 0, num_srq_qps = 0, qp = 0;
    uint32_t max_qp_size, max_size_needed;
    int32_t min_freelist_size = 0;
    int smallest_pp_qp = 0, ret = OMPI_ERROR;

    queues = opal_argv_split(mca_btl_openib_component.receive_queues, ':');
    if (0 == opal_argv_count(queues)) {
        orte_show_help("help-mpi-btl-openib.txt",
                       "no qps in receive_queues", true,
                       orte_process_info.nodename, 
                       mca_btl_openib_component.receive_queues);
        ret = OMPI_ERROR;
        goto error;
    }

    while (queues[qp] != NULL) {
        if (0 == strncmp("P,", queues[qp], 2)) {
            num_pp_qps++;
            if (smallest_pp_qp > qp) {
                smallest_pp_qp = qp;
            }
        } else if (0 == strncmp("S,", queues[qp], 2)) {
            num_srq_qps++;
        } else if (0 == strncmp("X,", queues[qp], 2)) {
#if HAVE_XRC
            num_xrc_qps++;
#else
            orte_show_help("help-mpi-btl-openib.txt", "No XRC support", true,
                           orte_process_info.nodename, 
                           mca_btl_openib_component.receive_queues);
            ret = OMPI_ERR_NOT_AVAILABLE;
            goto error;
#endif
        } else {
            orte_show_help("help-mpi-btl-openib.txt",
                           "invalid qp type in receive_queues", true,
                           orte_process_info.nodename, 
                           mca_btl_openib_component.receive_queues,
                           queues[qp]);
            ret = OMPI_ERR_BAD_PARAM;
            goto error;
        }
        qp++;
    }
    /* Current XRC implementation can't used with other QP types - PP
       and SRQ */
    if (num_xrc_qps > 0 && (num_pp_qps > 0 || num_srq_qps > 0)) {
        orte_show_help("help-mpi-btl-openib.txt", "XRC with PP or SRQ", true,
                       orte_process_info.nodename, 
                       mca_btl_openib_component.receive_queues);
        ret = OMPI_ERR_BAD_PARAM;
        goto error;
    }

    /* Current XRC implementation can't used with btls_per_lid > 1 */
    if (num_xrc_qps > 0 && mca_btl_openib_component.btls_per_lid > 1) {
        orte_show_help("help-mpi-btl-openib.txt", "XRC with BTLs per LID", 
                       true, orte_process_info.nodename, 
                       mca_btl_openib_component.receive_queues, num_xrc_qps);
        ret = OMPI_ERR_BAD_PARAM;
        goto error;
    }
    mca_btl_openib_component.num_pp_qps = num_pp_qps;
    mca_btl_openib_component.num_srq_qps = num_srq_qps;
    mca_btl_openib_component.num_xrc_qps = num_xrc_qps;
    mca_btl_openib_component.num_qps = num_pp_qps + num_srq_qps + num_xrc_qps;

    mca_btl_openib_component.qp_infos = (mca_btl_openib_qp_info_t*)
        malloc(sizeof(mca_btl_openib_qp_info_t) *
                mca_btl_openib_component.num_qps);

    qp = 0;
#define P(N) (((N) > count) ? NULL : params[(N)])
    while (queues[qp] != NULL) {
        int count;
        int32_t rd_low, rd_num;
        params = opal_argv_split_with_empty(queues[qp], ',');
        count = opal_argv_count(params);

        if ('P' == params[0][0]) {
            int32_t rd_win, rd_rsv;
            if (count < 3 || count > 6) {
                orte_show_help("help-mpi-btl-openib.txt",
                               "invalid pp qp specification", true,
                               orte_process_info.nodename, queues[qp]);
                ret = OMPI_ERR_BAD_PARAM;
                goto error;
            }
            mca_btl_openib_component.qp_infos[qp].type = MCA_BTL_OPENIB_PP_QP;
            mca_btl_openib_component.qp_infos[qp].size = atoi_param(P(1), 0);
            rd_num = atoi_param(P(2), 256);
            /* by default set rd_low to be 3/4 of rd_num */
            rd_low = atoi_param(P(3), rd_num - (rd_num / 4));
            rd_win = atoi_param(P(4), (rd_num - rd_low) * 2);
            rd_rsv = atoi_param(P(5), (rd_num * 2) / rd_win);

            BTL_VERBOSE(("pp: rd_num is %d rd_low is %d rd_win %d rd_rsv %d",
                         rd_num, rd_low, rd_win, rd_rsv));

            /* Calculate the smallest freelist size that can be allowed */
            if (rd_num + rd_rsv > min_freelist_size) {
                min_freelist_size = rd_num + rd_rsv;
            }

            mca_btl_openib_component.qp_infos[qp].u.pp_qp.rd_win = rd_win;
            mca_btl_openib_component.qp_infos[qp].u.pp_qp.rd_rsv = rd_rsv;
            if ((rd_num - rd_low) > rd_win) {
                orte_show_help("help-mpi-btl-openib.txt", "non optimal rd_win",
                        true, rd_win, rd_num - rd_low);
            }
        } else {
            int32_t sd_max, rd_init, srq_limit;
            if (count < 3 || count > 7) {
                orte_show_help("help-mpi-btl-openib.txt",
                               "invalid srq specification", true,
                               orte_process_info.nodename, queues[qp]);
                ret = OMPI_ERR_BAD_PARAM;
                goto error;
            }
            mca_btl_openib_component.qp_infos[qp].type = (params[0][0] =='X') ?
                MCA_BTL_OPENIB_XRC_QP : MCA_BTL_OPENIB_SRQ_QP;
            mca_btl_openib_component.qp_infos[qp].size = atoi_param(P(1), 0);
            rd_num = atoi_param(P(2), 256);
            /* by default set rd_low to be 3/4 of rd_num */
            rd_low = atoi_param(P(3), rd_num - (rd_num / 4));
            sd_max = atoi_param(P(4), rd_low / 4);
            /* rd_init is initial value for rd_curr_num of all SRQs, 1/4 of rd_num by default */
            rd_init = atoi_param(P(5), rd_num / 4);
            /* by default set srq_limit to be 3/16 of rd_init (it's 1/4 of rd_low_local,
               the value of rd_low_local we calculate in create_srq function) */
            srq_limit = atoi_param(P(6), (rd_init - (rd_init / 4)) / 4);

            /* If we set srq_limit less or greater than rd_init
               (init value for rd_curr_num) => we receive the IBV_EVENT_SRQ_LIMIT_REACHED
               event immediately and the value of rd_curr_num will be increased */

            /* If we set srq_limit to zero, but size of SRQ greater than 1 => set it to be 1 */
            if((0 == srq_limit) && (1 < rd_num)) {
                srq_limit = 1;
            }

            BTL_VERBOSE(("srq: rd_num is %d rd_low is %d sd_max is %d rd_max is %d srq_limit is %d",
                         rd_num, rd_low, sd_max, rd_init, srq_limit));

            /* Calculate the smallest freelist size that can be allowed */
            if (rd_num > min_freelist_size) {
                min_freelist_size = rd_num;
            }

            if (rd_num < rd_init) {
                orte_show_help("help-mpi-btl-openib.txt", "rd_num must be >= rd_init",
                        true, orte_process_info.nodename, queues[qp]);
                ret = OMPI_ERR_BAD_PARAM;
                goto error;
            }

            if (rd_num < srq_limit) {
                orte_show_help("help-mpi-btl-openib.txt", "srq_limit must be > rd_num",
                        true, orte_process_info.nodename, queues[qp]);
                ret = OMPI_ERR_BAD_PARAM;
                goto error;
            }

            mca_btl_openib_component.qp_infos[qp].u.srq_qp.sd_max = sd_max;
            mca_btl_openib_component.qp_infos[qp].u.srq_qp.rd_init = rd_init;
            mca_btl_openib_component.qp_infos[qp].u.srq_qp.srq_limit = srq_limit;
        }

        if (rd_num <= rd_low) {
            orte_show_help("help-mpi-btl-openib.txt", "rd_num must be > rd_low",
                    true, orte_process_info.nodename, queues[qp]);
            ret = OMPI_ERR_BAD_PARAM;
            goto error;
        }
        mca_btl_openib_component.qp_infos[qp].rd_num = rd_num;
        mca_btl_openib_component.qp_infos[qp].rd_low = rd_low;
        opal_argv_free(params);
        qp++;
    }
    params = NULL;

    /* Sanity check some sizes */

    max_qp_size = mca_btl_openib_component.qp_infos[mca_btl_openib_component.num_qps - 1].size;
    max_size_needed = (mca_btl_openib_module.super.btl_eager_limit >
                       mca_btl_openib_module.super.btl_max_send_size) ?
        mca_btl_openib_module.super.btl_eager_limit :
        mca_btl_openib_module.super.btl_max_send_size;
    if (max_qp_size < max_size_needed) {
        orte_show_help("help-mpi-btl-openib.txt",
                       "biggest qp size is too small", true,
                       orte_process_info.nodename, max_qp_size,
                       max_size_needed);
        ret = OMPI_ERR_BAD_PARAM;
        goto error;
    } else if (max_qp_size > max_size_needed) {
        orte_show_help("help-mpi-btl-openib.txt",
                       "biggest qp size is too big", true,
                       orte_process_info.nodename, max_qp_size,
                       max_size_needed);
    }

    if (mca_btl_openib_component.ib_free_list_max > 0 &&
        min_freelist_size > mca_btl_openib_component.ib_free_list_max) {
        orte_show_help("help-mpi-btl-openib.txt", "freelist too small", true,
                       orte_process_info.nodename,
                       mca_btl_openib_component.ib_free_list_max,
                       min_freelist_size);
        ret = OMPI_ERR_BAD_PARAM;
        goto error;
    }

    mca_btl_openib_component.rdma_qp = mca_btl_openib_component.num_qps - 1;
    mca_btl_openib_component.credits_qp = smallest_pp_qp;

    ret = OMPI_SUCCESS;
error:
    if (NULL != params) {
        opal_argv_free(params);
    }

    if (NULL != queues) {
        opal_argv_free(queues);
    }

    return ret;
}

static int init_one_device(opal_list_t *btl_list, struct ibv_device* ib_dev)
{
    struct mca_mpool_base_resources_t mpool_resources;
    mca_btl_openib_device_t *device;
    uint8_t i, k = 0;
    int ret = -1, port_cnt;
    ompi_btl_openib_ini_values_t values, default_values;
    int *allowed_ports = NULL;
    bool need_search;

    device = OBJ_NEW(mca_btl_openib_device_t);
    if(NULL == device){
        BTL_ERROR(("Failed malloc: %s:%d", __FILE__, __LINE__));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    device->mem_reg_active = 0;
    /* NTH: set some high default until we know how many local peers we have */
    device->mem_reg_max    = 1ull << 48;

    device->ib_dev = ib_dev;
    device->ib_dev_context = ibv_open_device(ib_dev);
    device->ib_pd = NULL;
    device->device_btls = OBJ_NEW(opal_pointer_array_t);
    if (OPAL_SUCCESS != opal_pointer_array_init(device->device_btls, 2, INT_MAX, 2)) {
        BTL_ERROR(("Failed to initialize device_btls array: %s:%d", __FILE__, __LINE__));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    if(NULL == device->ib_dev_context){
        BTL_ERROR(("error obtaining device context for %s errno says %s",
                    ibv_get_device_name(device->ib_dev), strerror(errno)));
        goto error;
    }

    if(ibv_query_device(device->ib_dev_context, &device->ib_dev_attr)){
        BTL_ERROR(("error obtaining device attributes for %s errno says %s",
                    ibv_get_device_name(device->ib_dev), strerror(errno)));
        goto error;
    }
    /* If mca_btl_if_include/exclude were specified, get usable ports */
    allowed_ports = (int*)malloc(device->ib_dev_attr.phys_port_cnt * sizeof(int));
    port_cnt = get_port_list(device, allowed_ports);
    if (0 == port_cnt) {
        free(allowed_ports);
        ret = OMPI_SUCCESS;
        ++num_devices_intentionally_ignored;
        goto error;
    }

    /* Load in vendor/part-specific device parameters.  Note that even if
       we don't find values for this vendor/part, "values" will be set
       indicating that it does not have good values */
    ret = ompi_btl_openib_ini_query(device->ib_dev_attr.vendor_id,
                                    device->ib_dev_attr.vendor_part_id,
                                    &values);
    if (OMPI_SUCCESS != ret && OMPI_ERR_NOT_FOUND != ret) {
        /* If we get a serious error, propagate it upwards */
        goto error;
    }
    if (OMPI_ERR_NOT_FOUND == ret) {
        /* If we didn't find a matching device in the INI files, output a
           warning that we're using default values (unless overridden
           that we don't want to see these warnings) */
        if (mca_btl_openib_component.warn_no_device_params_found) {
            orte_show_help("help-mpi-btl-openib.txt",
                           "no device params found", true,
                           orte_process_info.nodename,
                           ibv_get_device_name(device->ib_dev),
                           device->ib_dev_attr.vendor_id,
                           device->ib_dev_attr.vendor_part_id);
        }
    }

    /* If we're supposed to ignore devices of this vendor/part ID,
       then do so */
    if (values.ignore_device_set && values.ignore_device) {
        BTL_VERBOSE(("device %s skipped; ignore_device=1",
                     ibv_get_device_name(device->ib_dev)));
        ret = OMPI_SUCCESS;
        ++num_devices_intentionally_ignored;
        goto error;
    }

    /* Note that even if we don't find default values, "values" will
       be set indicating that it does not have good values */
    ret = ompi_btl_openib_ini_query(0, 0, &default_values);
    if (OMPI_SUCCESS != ret && OMPI_ERR_NOT_FOUND != ret) {
        /* If we get a serious error, propagate it upwards */
        goto error;
    }

    /* If we did find values for this device (or in the defaults
       section), handle them */
    merge_values(&values, &default_values);
    if (values.mtu_set) {
        switch (values.mtu) {
        case 256:
            device->mtu = IBV_MTU_256;
            break;
        case 512:
            device->mtu = IBV_MTU_512;
            break;
        case 1024:
            device->mtu = IBV_MTU_1024;
            break;
        case 2048:
            device->mtu = IBV_MTU_2048;
            break;
        case 4096:
            device->mtu = IBV_MTU_4096;
            break;
        default:
            BTL_ERROR(("invalid MTU value specified in INI file (%d); ignored", values.mtu));
            device->mtu = mca_btl_openib_component.ib_mtu;
            break;
        }
    } else {
        device->mtu = mca_btl_openib_component.ib_mtu;
    }

    /* Allocate the protection domain for the device */
    device->ib_pd = ibv_alloc_pd(device->ib_dev_context);
    if(NULL == device->ib_pd){
        BTL_ERROR(("error allocating protection domain for %s errno says %s",
                    ibv_get_device_name(device->ib_dev), strerror(errno)));
        goto error;
    }

    /* Figure out what the max_inline_data value should be for all
       ports and QPs on this device */
    need_search = false;
    if(-2 != mca_btl_openib_component.ib_max_inline_data) {
        /* User has explicitly set btl_openib_max_inline_data MCA parameter 
           Per setup in _mca.c, we know that the MCA param value is guaranteed
           to be >= -1 */
        if (-1 == mca_btl_openib_component.ib_max_inline_data) {
            need_search = true;
        } else {
            device->max_inline_data = (uint32_t) 
                mca_btl_openib_component.ib_max_inline_data;
        }
    } else if (values.max_inline_data_set) {
        if (-1 == values.max_inline_data) {
            need_search = true;
        } else if (values.max_inline_data >= 0) {
            device->max_inline_data = (uint32_t) values.max_inline_data;
        } else {
            if(default_values.max_inline_data_set && 
               default_values.max_inline_data >= -1) {
                BTL_ERROR(("Invalid max_inline_data value specified "
                           "in INI file (%d); using default value (%d)", 
                            values.max_inline_data, 
                            default_values.max_inline_data));
                device->max_inline_data = (uint32_t) 
                    default_values.max_inline_data;
            } else {
                BTL_ERROR(("Invalid max_inline_data value specified "
                           "in INI file (%d)", values.max_inline_data));
                ret = OMPI_ERR_BAD_PARAM;
                goto error;
            } 
        }
    }
    /* Horrible.  :-( Per the thread starting here:
       http://lists.openfabrics.org/pipermail/general/2008-June/051822.html,
       we can't rely on the value reported by the device to determine
       the maximum max_inline_data value.  So we have to search by
       looping over max_inline_data values and trying to make dummy
       QPs.  Yuck! */
    if (need_search) {
        struct ibv_qp *qp;
        struct ibv_cq *cq;
        struct ibv_qp_init_attr init_attr;
        uint32_t max_inline_data;

        /* Make a dummy CQ */
#if OMPI_IBV_CREATE_CQ_ARGS == 3
        cq = ibv_create_cq(device->ib_dev_context, 1, NULL);
#else
        cq = ibv_create_cq(device->ib_dev_context, 1, NULL, NULL, 0);
#endif
        if (NULL == cq) {
            orte_show_help("help-mpi-btl-openib.txt", "init-fail-create-q",
                           true, orte_process_info.nodename,
                           __FILE__, __LINE__, "ibv_create_cq",
                           strerror(errno), errno, 
                           ibv_get_device_name(device->ib_dev));
            ret = OMPI_ERR_NOT_AVAILABLE;
            goto error;
        }

        /* Setup the QP attributes */
        memset(&init_attr, 0, sizeof(init_attr));
        init_attr.qp_type = IBV_QPT_RC;
        init_attr.send_cq = cq;
        init_attr.recv_cq = cq;
        init_attr.srq = 0;
        init_attr.cap.max_send_sge = 1;
        init_attr.cap.max_recv_sge = 1;
        init_attr.cap.max_recv_wr = 1;

        /* Loop over max_inline_data values; just check powers of 2 --
           that's good enough */
        init_attr.cap.max_inline_data = max_inline_data = 1 << 20;
        while (max_inline_data > 0) {
            qp = ibv_create_qp(device->ib_pd, &init_attr); 
            if (NULL != qp) {
                break;
            }
            max_inline_data >>= 1;
            init_attr.cap.max_inline_data = max_inline_data;
        }

        /* Did we find it? */
        if (NULL != qp) {
            device->max_inline_data = max_inline_data;
            ibv_destroy_qp(qp);
        } else {
            device->max_inline_data = 0;
        }

        /* Destroy the temp CQ */
        ibv_destroy_cq(cq);
    }

    /* Should we use RDMA for short / eager messages?  First check MCA
       param, then check INI file values. */
    if (mca_btl_openib_component.use_eager_rdma >= 0) {
        device->use_eager_rdma = mca_btl_openib_component.use_eager_rdma;
    } else if (values.use_eager_rdma_set) {
        device->use_eager_rdma = values.use_eager_rdma;
    }
    /* Eager RDMA is not currently supported with progress threads */
    if (device->use_eager_rdma && OPAL_ENABLE_PROGRESS_THREADS) {
        device->use_eager_rdma = 0;
        orte_show_help("help-mpi-btl-openib.txt", 
                       "eager RDMA and progress threads", true);
    }

    mpool_resources.reg_data = (void*)device;
    mpool_resources.sizeof_reg = sizeof(mca_btl_openib_reg_t);
    mpool_resources.register_mem = openib_reg_mr;
    mpool_resources.deregister_mem = openib_dereg_mr;
    device->mpool =
        mca_mpool_base_module_create(mca_btl_openib_component.ib_mpool_name,
                device, &mpool_resources);
    if(NULL == device->mpool){
        /* Don't print an error message here -- we'll get one from
           mpool_create anyway (OPAL_SOS would be good here...) */
         goto error;
    }

#if OPAL_ENABLE_PROGRESS_THREADS
    device->ib_channel = ibv_create_comp_channel(device->ib_dev_context);
    if (NULL == device->ib_channel) {
        BTL_ERROR(("error creating channel for %s errno says %s",
                    ibv_get_device_name(device->ib_dev),
                    strerror(errno)));
        goto error;
    }
#endif

    ret = OMPI_SUCCESS;

    /* Note ports are 1 based (i >= 1) */
    for(k = 0; k < port_cnt; k++){
        struct ibv_port_attr ib_port_attr;
        i = allowed_ports[k];
        if(ibv_query_port(device->ib_dev_context, i, &ib_port_attr)){
            BTL_ERROR(("error getting port attributes for device %s "
                        "port number %d errno says %s",
                        ibv_get_device_name(device->ib_dev), i, strerror(errno)));
            break;
        }
        if(IBV_PORT_ACTIVE == ib_port_attr.state) {
            /* Select the lower of the HCA and port active speed. With QLogic
               HCAs that are capable of 4K MTU we had an issue when connected
               to switches with 2K MTU. This fix is valid for other IB vendors
               as well. */
            if (ib_port_attr.active_mtu < device->mtu){
                device->mtu = ib_port_attr.active_mtu;
            }
            if (mca_btl_openib_component.apm_ports && device->btls > 0) {
                init_apm_port(device, i, ib_port_attr.lid);
                break;
            }
            if (0 == mca_btl_openib_component.ib_pkey_val) {
                ret = init_one_port(btl_list, device, i, 0, &ib_port_attr);
            } else {
                uint16_t pkey,j;
                for (j = 0; j < device->ib_dev_attr.max_pkeys; j++) {
                    if(ibv_query_pkey(device->ib_dev_context, i, j, &pkey)){
                        BTL_ERROR(("error getting pkey for index %d, device %s "
                                    "port number %d errno says %s",
                                    j, ibv_get_device_name(device->ib_dev), i, strerror(errno)));
                    }
                    pkey = ntohs(pkey) & MCA_BTL_IB_PKEY_MASK;
                    if(pkey == mca_btl_openib_component.ib_pkey_val){
                        ret = init_one_port(btl_list, device, i, j, &ib_port_attr);
                        break;
                    }
                }
            }
            if (OMPI_SUCCESS != ret) {
                /* Out of bounds error indicates that we hit max btl number
                 * don't propagate the error to the caller */
                if (OMPI_ERR_VALUE_OUT_OF_BOUNDS == ret) {
                    ret = OMPI_SUCCESS;
                }
                break;
            }
        }
    }
    free(allowed_ports);

    /* If we made a BTL, check APM status and return.  Otherwise, fall
       through and destroy everything */
    if (device->btls > 0) {
        /* if apm was enabled it should be > 1 */
        if (1 == mca_btl_openib_component.apm_ports) {
            orte_show_help("help-mpi-btl-openib.txt",
                           "apm not enough ports", true);
            mca_btl_openib_component.apm_ports = 0;
        }

        /* Check to ensure that all devices used in this process have
           compatible receive_queues values (we check elsewhere to see
           if all devices used in other processes in this job have
           compatible receive_queues values).

           Not only is the check complex, but the reasons behind what
           it does (and does not do) are complex.  Before explaining
           the code below, here's some notes:

           1. The openib BTL component only supports 1 value of the
              receive_queues between all of its modules.

              --> This could be changed to allow every module to have
                  its own receive_queues.  But that would be a big
                  deal; no one has time to code this up right now.

           2. The receive_queues value can be specified either as an
              MCA parameter or in the INI file.  Specifying the value
              as an MCA parameter overrides all INI file values
              (meaning: that MCA param value will be used for all
              openib BTL modules in the process).

           Effectively, the first device through init_one_device()
           gets to decide what the receive_queues will be for the all
           modules in this process.  This is an unfortunate artifact
           of the openib BTL startup sequence (see below for more
           details).  The first device will choose the receive_queues
           value from: (in priority order): 

           1. If the btl_openib_receive_queues MCA param was
              specified, use that.
           2. If this device has a receive_queues value specified in
              the INI file, use that.
           3. Otherwise, use the default MCA param value for
              btl_openib_receive_queues.

           If any successive device has a different value specified in
           the INI file, we show_help and return up the stack that
           this device failed.

           In the case that the user does not specify a
           mca_btl_openib_receive_queues value, the short description
           of what is allowed is that either a) no devices specify a
           receive_queues value in the INI file (in which case we use
           the default MCA param value), b) all devices specify the
           same receive_queues value in the INI value, or c) some/all
           devices specify the same receive_queues value in the INI
           value as the default MCA param value.

           Let's take some sample cases to explain this more clearly...

           THESE ARE THE "GOOD" CASES
           --------------------------

           Case 1: no INI values
           - MCA parameter: not specified
           - default receive_queues: value A
           - device 0: no receive_queues in INI file
           - device 1: no receive_queues in INI file
           - device 2: no receive_queues in INI file
           --> use receive_queues value A with all devices

           Case 2: all INI values the same (same as default)
           - MCA parameter: not specified
           - default receive_queues: value A
           - device 0: receive_queues value A in the INI file
           - device 1: receive_queues value A in the INI file
           - device 2: receive_queues value A in the INI file
           --> use receive_queues value A with all devices

           Case 3: all INI values the same (but different than default)
           - MCA parameter: not specified
           - default receive_queues: value A
           - device 0: receive_queues value B in the INI file
           - device 1: receive_queues value B in the INI file
           - device 2: receive_queues value B in the INI file
           --> use receive_queues value B with all devices

           Case 4: some INI unspecified, but rest same as default
           - MCA parameter: not specified
           - default receive_queues: value A
           - device 0: receive_queues value A in the INI file
           - device 1: no receive_queues in INI file
           - device 2: receive_queues value A in the INI file
           --> use receive_queues value A with all devices

           Case 5: some INI unspecified (including device 0), but rest same as default
           - MCA parameter: not specified
           - default receive_queues: value A
           - device 0: no receive_queues in INI file
           - device 1: no receive_queues in INI file
           - device 2: receive_queues value A in the INI file
           --> use receive_queues value A with all devices

           Case 6: different default/INI values, but MCA param is specified
           - MCA parameter: value D
           - default receive_queues: value A
           - device 0: no receive_queues in INI file
           - device 1: receive_queues value B in INI file
           - device 2: receive_queues value C in INI file
           --> use receive_queues value D with all devices

           What this means is that this selection process is
           unfortunately tied to the order of devices.  :-( Device 0
           effectively sets what the receive_queues value will be for
           that process.  If any later device disagrees, that's
           problematic and we have to error/abort.

           ALL REMAINING CASES WILL FAIL
           -----------------------------

           Case 7: one INI value (different than default)
           - MCA parameter: not specified
           - default receive_queues: value A
           - device 0: receive_queues value B in INI file
           - device 1: no receive_queues in INI file
           - device 2: no receive_queues in INI file
           --> Jeff thinks that it would be great to use
               receive_queues value B with all devices.  However, it
               shares one of the problems cited in case 8, below.  So
               we need to fail this scenario; print an error and
               abort.
           
           Case 8: one INI value, different than default
           - MCA parameter: not specified
           - default receive_queues: value A
           - device 0: no receive_queues in INI file
           - device 1: receive_queues value B in INI file
           - device 2: no receive_queues in INI file

           --> Jeff thinks that it would be great to use
               receive_queues value B with all devices.  However, it
               has (at least) 2 problems:

               1. The check for local receive_queue compatibility is
                  done here in init_one_device().  By the time we call
                  init_one_device() for device 1, we have already
                  called init_one_device() for device 0, meaning that
                  device 0's QPs have already been created and setup
                  using the MCA parameter's default receive_queues
                  value.  So if device 1 *changes* the
                  component.receive_queues value, then device 0 and
                  device 1 now have different receive_queue sets (more
                  specifically: the QPs setup for device 0 are now
                  effectively lost).  This is Bad.

                  It would be great if we didn't have this restriction
                  -- either by letting each module have its own
                  receive_queues value or by scanning all devices and
                  figuring out a final receive_queues value *before*
                  actually setting up any QPs.  But that's not the
                  current flow of the code (patches would be greatly
                  appreciated here, of course!).  Unfortunately, no
                  one has time to code this up right now, so we're
                  leaving this as explicitly documented for some
                  future implementer...

               2. Conside a scenario with server 1 having HCA A/subnet
                  X, and server 2 having HCA B/subnet X and HCA
                  C/subnet Y.  And let's assume:

                  Server 1:
                  HCA A: no receive_queues in INI file

                  Server 2:
                  HCA B: no receive_queues in INI file
                  HCA C: receive_queues specified in INI file
                
                  A will therefore use the default receive_queues
                  value.  B and C will use C's INI receive_queues.
                  But note that modex [currently] only sends around
                  vendor/part IDs for OpenFabrics devices -- not the
                  actual receive_queues value (it was felt that
                  including the final receive_queues string value in
                  the modex would dramatically increase the size of
                  the modex).  So processes on server 1 will get the
                  vendor/part ID for HCA B, look it up in the INI
                  file, see that it has no receive_queues value
                  specified, and then assume that it uses the default
                  receive_queues value.  Hence, procs on server 1 will
                  try to connect HCA A-->HCA B with the wrong
                  receive_queues value.  Bad.  Further, the error
                  won't be discovered by checks like this because A
                  won't check D's receive_queues because D is on a
                  different subnet.

                  This could be fixed, of course; either by a) send
                  the final receive_queues value in the modex (perhaps
                  compressing or encoding it so that it can be much
                  shorter than the string -- the current vendor/part
                  ID stuff takes 8 bytes for each device), or b)
                  replicating the determination process of each host
                  in each process (i.e., procs on server 1 would see
                  both B and C, and use them both to figure out what
                  the "final" receive_queues value is for B).
                  Unfortunately, no one has time to code this up right
                  now, so we're leaving this as explicitly documented
                  for some future implementer...

               Because of both of these problems, this case is
               problematic and must fail with a show_help error.

           Case 9: two devices with same INI value (different than default)
           - MCA parameter: not specified
           - default receive_queues: value A
           - device 0: no receive_queues in INI file
           - device 1: receive_queues value B in INI file
           - device 2: receive_queues value B in INI file
           --> per case 8, fail with a show_help message.
           
           Case 10: two devices with different INI values
           - MCA parameter: not specified
           - default receive_queues: value A
           - device 0: no receive_queues in INI file
           - device 1: receive_queues value B in INI file
           - device 2: receive_queues value C in INI file
           --> per case 8, fail with a show_help message.

        */

        /* If the MCA param was specified, skip all the checks */
        if (BTL_OPENIB_RQ_SOURCE_MCA ==
            mca_btl_openib_component.receive_queues_source) {
            goto good;
        }

        /* If we're the first device and we have a receive_queues
           value from the INI file *that is different than the
           already-existing default value*, then set the component to
           use that. */
        if (0 == mca_btl_openib_component.devices_count) {
            if (NULL != values.receive_queues &&
                0 != strcmp(values.receive_queues,
                            mca_btl_openib_component.receive_queues)) {
                if (NULL != mca_btl_openib_component.receive_queues) {
                    free(mca_btl_openib_component.receive_queues);
                }
                mca_btl_openib_component.receive_queues =
                    strdup(values.receive_queues);
                mca_btl_openib_component.receive_queues_source =
                    BTL_OPENIB_RQ_SOURCE_DEVICE_INI;
            }
        }

        /* If we're not the first device, then we have to conform to
           either the default value if the first device didn't set
           anything, or to whatever the first device decided. */
        else {
            /* In all cases, if this device has a receive_queues value
               in the INI, then it must agree with
               component.receive_queues. */
            if (NULL != values.receive_queues) {
                if (0 != strcmp(values.receive_queues, 
                                mca_btl_openib_component.receive_queues)) {
                    orte_show_help("help-mpi-btl-openib.txt",
                                   "locally conflicting receive_queues", true,
                                   opal_install_dirs.pkgdatadir,
                                   orte_process_info.nodename,
                                   ibv_get_device_name(receive_queues_device->ib_dev),
                                   receive_queues_device->ib_dev_attr.vendor_id,
                                   receive_queues_device->ib_dev_attr.vendor_part_id,
                                   mca_btl_openib_component.receive_queues,
                                   ibv_get_device_name(device->ib_dev),
                                   device->ib_dev_attr.vendor_id,
                                   device->ib_dev_attr.vendor_part_id,
                                   values.receive_queues);
                    ret = OMPI_ERR_RESOURCE_BUSY;
                    goto error;
                }
            }

            /* If this device doesn't have an INI receive_queues
               value, then if the component.receive_queues value came
               from the default, we're ok.  But if the
               component.receive_queues value came from the 1st
               device's INI file, we must error. */
            else if (BTL_OPENIB_RQ_SOURCE_DEVICE_INI ==
                mca_btl_openib_component.receive_queues_source) {
                orte_show_help("help-mpi-btl-openib.txt",
                               "locally conflicting receive_queues", true,
                               opal_install_dirs.pkgdatadir,
                               orte_process_info.nodename,
                               ibv_get_device_name(receive_queues_device->ib_dev),
                               receive_queues_device->ib_dev_attr.vendor_id,
                               receive_queues_device->ib_dev_attr.vendor_part_id,
                               mca_btl_openib_component.receive_queues,
                               ibv_get_device_name(device->ib_dev),
                               device->ib_dev_attr.vendor_id,
                               device->ib_dev_attr.vendor_part_id,
                               mca_btl_openib_component.default_recv_qps);
                ret = OMPI_ERR_RESOURCE_BUSY;
                goto error;
            }
        }

        receive_queues_device = device;

    good:
        mca_btl_openib_component.devices_count++;
        return OMPI_SUCCESS;
    }

error:
#if OPAL_ENABLE_PROGRESS_THREADS
    if (device->ib_channel) {
        ibv_destroy_comp_channel(device->ib_channel);
    }
#endif
    if (device->mpool) {
        mca_mpool_base_module_destroy(device->mpool);
    }

    if (device->ib_pd) {
        ibv_dealloc_pd(device->ib_pd);
    }

    if (OMPI_SUCCESS != ret) {
        orte_show_help("help-mpi-btl-openib.txt",
                       "error in device init", true, 
                       orte_process_info.nodename,
                       ibv_get_device_name(device->ib_dev));
    }

    if (device->ib_dev_context) {
        ibv_close_device(device->ib_dev_context);
    }
    OBJ_RELEASE(device);
    return ret;
}

static int finish_btl_init(mca_btl_openib_module_t *openib_btl)
{
    int qp;
    openib_btl->num_peers = 0;

    /* Initialize module state */
    OBJ_CONSTRUCT(&openib_btl->ib_lock, opal_mutex_t);

    /* setup the qp structure */
    openib_btl->qps = (mca_btl_openib_module_qp_t*)
        calloc(mca_btl_openib_component.num_qps,
                sizeof(mca_btl_openib_module_qp_t));

    /* setup all the qps */
    for (qp = 0; qp < mca_btl_openib_component.num_qps; qp++) {
        if (!BTL_OPENIB_QP_TYPE_PP(qp)) {
            OBJ_CONSTRUCT(&openib_btl->qps[qp].u.srq_qp.pending_frags[0],
                    opal_list_t);
            OBJ_CONSTRUCT(&openib_btl->qps[qp].u.srq_qp.pending_frags[1],
                    opal_list_t);
            openib_btl->qps[qp].u.srq_qp.sd_credits =
                mca_btl_openib_component.qp_infos[qp].u.srq_qp.sd_max;
            openib_btl->qps[qp].u.srq_qp.srq = NULL;
        }
    }

    /* initialize the memory pool using the device */
    openib_btl->super.btl_mpool = openib_btl->device->mpool;

    openib_btl->eager_rdma_channels = 0;

    openib_btl->eager_rdma_frag_size = OPAL_ALIGN(
            sizeof(mca_btl_openib_header_t) +
            sizeof(mca_btl_openib_header_coalesced_t) +
            sizeof(mca_btl_openib_control_header_t) +
            sizeof(mca_btl_openib_footer_t) +
            openib_btl->super.btl_eager_limit,
            mca_btl_openib_component.buffer_alignment, size_t);

    return OMPI_SUCCESS;
}

static struct ibv_device **ibv_get_device_list_compat(int *num_devs)
{
    struct ibv_device **ib_devs;

#ifdef HAVE_IBV_GET_DEVICE_LIST
    ib_devs = ibv_get_device_list(num_devs);
#else
    struct dlist *dev_list;
    struct ibv_device *ib_dev;
    *num_devs = 0;

    /* Determine the number of device's available on the host */
    dev_list = ibv_get_devices();
    if (NULL == dev_list)
        return NULL;

    dlist_start(dev_list);

    dlist_for_each_data(dev_list, ib_dev, struct ibv_device)
        (*num_devs)++;

    /* Allocate space for the ib devices */
    ib_devs = (struct ibv_device**)malloc(*num_devs * sizeof(struct ibv_dev*));
    if(NULL == ib_devs) {
        *num_devs = 0;
        BTL_ERROR(("Failed malloc: %s:%d", __FILE__, __LINE__));
        return NULL;
    }

    dlist_start(dev_list);

    dlist_for_each_data(dev_list, ib_dev, struct ibv_device)
        *(++ib_devs) =  ib_dev;
#endif

    return ib_devs;
}

static void ibv_free_device_list_compat(struct ibv_device **ib_devs)
{
#ifdef HAVE_IBV_GET_DEVICE_LIST
    ibv_free_device_list(ib_devs);
#else
    free(ib_devs);
#endif
}

static opal_carto_graph_t *host_topo;

static int get_ib_dev_distance(struct ibv_device *dev)
{
    opal_paffinity_base_cpu_set_t cpus;
    opal_carto_base_node_t *device_node;
    int min_distance = -1, i, num_processors;
    const char *device = ibv_get_device_name(dev);

    if(opal_paffinity_base_get_processor_info(&num_processors) != OMPI_SUCCESS) {
        num_processors = 100; /* Choose something big enough */
    }

    device_node = opal_carto_base_find_node(host_topo, device);

    /* no topology info for device found. Assume that it is close */
    if(NULL == device_node)
        return 0;

    OPAL_PAFFINITY_CPU_ZERO(cpus);
    opal_paffinity_base_get(&cpus);

    for (i = 0; i < num_processors; i++) {
        opal_carto_base_node_t *slot_node;
        int distance, socket, core;
        char *slot;

        if(!OPAL_PAFFINITY_CPU_ISSET(i, cpus))
            continue;

        opal_paffinity_base_get_map_to_socket_core(i, &socket, &core);
        asprintf(&slot, "slot%d", socket);

        slot_node = opal_carto_base_find_node(host_topo, slot);

        free(slot);

        if(NULL == slot_node)
            return 0;

        distance = opal_carto_base_spf(host_topo, slot_node, device_node);

        if(distance < 0)
            return 0;

        if(min_distance < 0 || min_distance > distance)
            min_distance = distance;
    }

    return min_distance;
}

struct dev_distance {
    struct ibv_device *ib_dev;
    int distance;
};

static int compare_distance(const void *p1, const void *p2)
{
    const struct dev_distance *d1 = p1;
    const struct dev_distance *d2 = p2;

    return d1->distance - d2->distance;
}

static struct dev_distance *
sort_devs_by_distance(struct ibv_device **ib_devs, int count)
{
    int i;
    struct dev_distance *devs = malloc(count * sizeof(struct dev_distance));

    opal_carto_base_get_host_graph(&host_topo, "Infiniband");

    for (i = 0; i < count; i++) {
        devs[i].ib_dev = ib_devs[i];
        devs[i].distance = get_ib_dev_distance(ib_devs[i]);
    }

    qsort(devs, count, sizeof(struct dev_distance), compare_distance);

    opal_carto_base_free_graph(host_topo);

    return devs;
}


/*
 *  IB component initialization:
 *  (1) read interface list from kernel and compare against component parameters
 *      then create a BTL instance for selected interfaces
 *  (2) setup IB listen socket for incoming connection attempts
 *  (3) register BTL parameters with the MCA
 */

static mca_btl_base_module_t**
btl_openib_component_init(int *num_btl_modules,
                          bool enable_progress_threads,
                          bool enable_mpi_threads)
{
    struct ibv_device **ib_devs;
    mca_btl_base_module_t** btls;
    int i, ret, num_devs, length;
    opal_list_t btl_list;
    mca_btl_openib_module_t * openib_btl;
    mca_btl_base_selected_module_t* ib_selected;
    opal_list_item_t* item;
    unsigned short seedv[3];
    mca_btl_openib_frag_init_data_t *init_data;
    struct dev_distance *dev_sorted;
    int distance;
    int index, value;
    bool found;
    mca_base_param_source_t source;
    int list_count = 0;

    /* initialization */
    *num_btl_modules = 0;
    num_devs = 0;
#if BTL_OPENIB_MALLOC_HOOKS_ENABLED 
    /* If we got this far, then setup the memory alloc hook (because 
       we're most likely going to be using this component). The hook is to be set up 
       as early as possible in this function since we want most of the allocated resources 
       be aligned.*/ 
    if (mca_btl_openib_component.use_memalign > 0) { 
        mca_btl_openib_component.previous_malloc_hook = __malloc_hook; 
        __malloc_hook = btl_openib_malloc_hook;  
        malloc_hook_set = true; 
    } 
#endif 
    /* Currently refuse to run if MPI_THREAD_MULTIPLE is enabled */
    if (ompi_mpi_thread_multiple && !mca_btl_base_thread_multiple_override) {
        BTL_VERBOSE(("openib BTL disqualifying itself because MPI_THREAD_MULTIPLE is being used"));
        goto no_btls;
    }

    /* Per https://svn.open-mpi.org/trac/ompi/ticket/1305, check to
       see if $sysfsdir/class/infiniband exists.  If it does not,
       assume that the RDMA hardware drivers are not loaded, and
       therefore we don't want OpenFabrics verbs support in this OMPI
       job.  No need to print a warning. */
    if (!check_basics()) {
        goto no_btls;
    }

    seedv[0] = ORTE_PROC_MY_NAME->vpid;
    seedv[1] = opal_timer_base_get_cycles();
    seedv[2] = opal_timer_base_get_cycles();
    seed48(seedv);

    /* Read in INI files with device-specific parameters */
    if (OMPI_SUCCESS != (ret = ompi_btl_openib_ini_init())) {
        goto no_btls;
    }

    /* Initialize FD listening */
    if (OMPI_SUCCESS != ompi_btl_openib_fd_init()) {
        goto no_btls;
    }

    /* Init CPC components */
    if (OMPI_SUCCESS != (ret = ompi_btl_openib_connect_base_init())) {
        goto no_btls;
    }

    /* If we are using ptmalloc2 and there are no posix threads
       available, this will cause memory corruption.  Refuse to run.
       Right now, ptmalloc2 is the only memory manager that we have on
       OS's that support OpenFabrics that provide both FREE and MUNMAP
       support, so the following test is [currently] good enough... */
    value = opal_mem_hooks_support_level();
#if !OPAL_HAVE_THREADS
    if ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT) == 
        ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT) & value)) {
        orte_show_help("help-mpi-btl-openib.txt",
                       "ptmalloc2 with no threads", true,
                       orte_process_info.nodename);
        goto no_btls;
    }
#endif

    /* If we have a memory manager available, and
       mpi_leave_pinned==-1, then unless the user explicitly set
       mpi_leave_pinned_pipeline==0, then set mpi_leave_pinned to 1.

       We have a memory manager if we have both FREE and MUNMAP
       support */
    if ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT) == 
        ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT) & value)) {
        ret = 0;
        index = mca_base_param_find("mpi", NULL, "leave_pinned");
        if (index >= 0) {
            if (OPAL_SUCCESS == mca_base_param_lookup_int(index, &value) &&
                -1 == value) {
                ++ret;
            }
        }
        index = mca_base_param_find("mpi", NULL, "leave_pinned_pipeline");
        if (index >= 0) {
            if (OPAL_SUCCESS == mca_base_param_lookup_int(index, &value) &&
                OPAL_SUCCESS == mca_base_param_lookup_source(index, &source, 
                                                             NULL)) {
                if (0 == value && MCA_BASE_PARAM_SOURCE_DEFAULT == source) {
                    ++ret;
                }
            }
        }
        /* If we were good on both parameters, then set leave_pinned=1 */
        if (2 == ret) {
            ompi_mpi_leave_pinned = 1;
            ompi_mpi_leave_pinned_pipeline = 0;
        }
    }
    index = mca_base_param_find("btl", "openib", "max_inline_data");
    if (index >= 0) {
        if (OPAL_SUCCESS == mca_base_param_lookup_source(index, &source,
                                                         NULL)) {
            if (-1 == mca_btl_openib_component.ib_max_inline_data  && 
                MCA_BASE_PARAM_SOURCE_DEFAULT == source) {
                /* If the user has not explicitly set this MCA parameter
                   use max_inline_data value specified in the 
                   device-specific parameters INI file */
                mca_btl_openib_component.ib_max_inline_data = -2;
            }
        }
    }

    OBJ_CONSTRUCT(&mca_btl_openib_component.send_free_coalesced, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_openib_component.send_user_free, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_openib_component.recv_user_free, ompi_free_list_t);

    init_data = malloc(sizeof(mca_btl_openib_frag_init_data_t));

    init_data->order = mca_btl_openib_component.rdma_qp;
    init_data->list = &mca_btl_openib_component.send_user_free;

    /* Align fragments on 8-byte boundaries (instead of 2) to fix bus errors that
       occur on some 32-bit platforms. Depending on the size of the fragment this
       will waste 2-6 bytes of space per frag. In most cases this shouldn't waste
       any space. */
    if (OMPI_SUCCESS != ompi_free_list_init_ex_new(
                &mca_btl_openib_component.send_user_free,
                sizeof(mca_btl_openib_put_frag_t), 8,
                OBJ_CLASS(mca_btl_openib_put_frag_t),
                0, 0,
                mca_btl_openib_component.ib_free_list_num,
                mca_btl_openib_component.ib_free_list_max,
                mca_btl_openib_component.ib_free_list_inc,
                NULL, mca_btl_openib_frag_init, init_data)) {
        goto no_btls;
    }

    init_data = malloc(sizeof(mca_btl_openib_frag_init_data_t));

    init_data->order = mca_btl_openib_component.rdma_qp;
    init_data->list = &mca_btl_openib_component.recv_user_free;

    if(OMPI_SUCCESS != ompi_free_list_init_ex_new(
                &mca_btl_openib_component.recv_user_free,
                sizeof(mca_btl_openib_get_frag_t), 8,
                OBJ_CLASS(mca_btl_openib_get_frag_t),
                0, 0,
                mca_btl_openib_component.ib_free_list_num,
                mca_btl_openib_component.ib_free_list_max,
                mca_btl_openib_component.ib_free_list_inc,
                NULL, mca_btl_openib_frag_init, init_data)) {
        goto no_btls;
    }

    init_data = malloc(sizeof(mca_btl_openib_frag_init_data_t));
    length = sizeof(mca_btl_openib_coalesced_frag_t);

    init_data->list = &mca_btl_openib_component.send_free_coalesced;

    if(OMPI_SUCCESS != ompi_free_list_init_ex(
                &mca_btl_openib_component.send_free_coalesced,
                length, 8, OBJ_CLASS(mca_btl_openib_coalesced_frag_t),
                mca_btl_openib_component.ib_free_list_num,
                mca_btl_openib_component.ib_free_list_max,
                mca_btl_openib_component.ib_free_list_inc,
                NULL, mca_btl_openib_frag_init, init_data)) {
        goto no_btls;
    }

    /* If we want fork support, try to enable it */
#ifdef HAVE_IBV_FORK_INIT
    if (0 != mca_btl_openib_component.want_fork_support) {
        if (0 != ibv_fork_init()) {
            /* If the want_fork_support MCA parameter is >0, then the
               user was specifically asking for fork support and we
               couldn't provide it.  So print an error and deactivate
               this BTL. */
            if (mca_btl_openib_component.want_fork_support > 0) {
                orte_show_help("help-mpi-btl-openib.txt",
                               "ibv_fork_init fail", true,
                               orte_process_info.nodename);
                goto no_btls;
            }
        }
    }
#endif

    /* Parse the include and exclude lists, checking for errors */
    mca_btl_openib_component.if_include_list =
        mca_btl_openib_component.if_exclude_list =
        mca_btl_openib_component.if_list = NULL;

    if (NULL != mca_btl_openib_component.if_include)
      list_count++;
    if (NULL != mca_btl_openib_component.if_exclude)
      list_count++;
    if (NULL != mca_btl_openib_component.ipaddr_include)
      list_count++;
    if (NULL != mca_btl_openib_component.ipaddr_exclude)
      list_count++;

    if (list_count > 1) {
        orte_show_help("help-mpi-btl-openib.txt",
                       "specified include and exclude", true,
                       NULL == mca_btl_openib_component.if_include ?
                        "<not specified>" : mca_btl_openib_component.if_include,
                       NULL == mca_btl_openib_component.if_exclude ?
                        "<not specified>" : mca_btl_openib_component.if_exclude,
                       NULL == mca_btl_openib_component.ipaddr_include ?
                        "<not specified>" :mca_btl_openib_component.ipaddr_include,
                       NULL == mca_btl_openib_component.ipaddr_exclude ?
                         "<not specified>" :mca_btl_openib_component.ipaddr_exclude,
                       NULL);
        goto no_btls;
    } else if (NULL != mca_btl_openib_component.if_include) {
        mca_btl_openib_component.if_include_list =
            opal_argv_split(mca_btl_openib_component.if_include, ',');
        mca_btl_openib_component.if_list =
            opal_argv_copy(mca_btl_openib_component.if_include_list);
    } else if (NULL != mca_btl_openib_component.if_exclude) {
        mca_btl_openib_component.if_exclude_list =
            opal_argv_split(mca_btl_openib_component.if_exclude, ',');
        mca_btl_openib_component.if_list =
            opal_argv_copy(mca_btl_openib_component.if_exclude_list);
    }

    ib_devs = ibv_get_device_list_compat(&num_devs);

    if(0 == num_devs || NULL == ib_devs) {
        mca_btl_base_error_no_nics("OpenFabrics (openib)", "device");
        goto no_btls;
    }

    dev_sorted = sort_devs_by_distance(ib_devs, num_devs);

    OBJ_CONSTRUCT(&btl_list, opal_list_t);
    OBJ_CONSTRUCT(&mca_btl_openib_component.ib_lock, opal_mutex_t);
#if OPAL_HAVE_THREADS
    mca_btl_openib_component.async_thread = 0;
#endif
    distance = dev_sorted[0].distance;
    for (found = false, i = 0; 
         i < num_devs && (-1 == mca_btl_openib_component.ib_max_btls ||
                mca_btl_openib_component.ib_num_btls <
                mca_btl_openib_component.ib_max_btls); i++) {
        if (distance != dev_sorted[i].distance) {
            break;
        }

        /* Only take devices that match the type specified by
           btl_openib_device_type */
        switch (mca_btl_openib_component.device_type) {
        case BTL_OPENIB_DT_IB:
#if defined(HAVE_STRUCT_IBV_DEVICE_TRANSPORT_TYPE)
            if (IBV_TRANSPORT_IWARP == dev_sorted[i].ib_dev->transport_type) {
                BTL_VERBOSE(("openib: only taking infiniband devices -- skipping %s",
                             ibv_get_device_name(dev_sorted[i].ib_dev)));
                continue;
            }
#endif
            break;

        case BTL_OPENIB_DT_IWARP:
#if defined(HAVE_STRUCT_IBV_DEVICE_TRANSPORT_TYPE)
            if (IBV_TRANSPORT_IB == dev_sorted[i].ib_dev->transport_type) {
                BTL_VERBOSE(("openib: only taking iwarp devices -- skipping %s",
                             ibv_get_device_name(dev_sorted[i].ib_dev)));
                continue;
            }
#else
            orte_show_help("help-mpi-btl-openib.txt", "no iwarp support",
                           true);
#endif
            break;

        case BTL_OPENIB_DT_ALL:
            break;
        }

        found = true;
        ret = init_one_device(&btl_list, dev_sorted[i].ib_dev);
        if (OMPI_SUCCESS != ret && OMPI_ERR_NOT_SUPPORTED != ret) {
            free(dev_sorted);
            goto no_btls;
        }
    }
    free(dev_sorted);
    if (!found) {
        orte_show_help("help-mpi-btl-openib.txt", "no devices right type",
                       true, orte_process_info.nodename,
                       ((BTL_OPENIB_DT_IB == mca_btl_openib_component.device_type) ?
                        "InfiniBand" :
                        (BTL_OPENIB_DT_IWARP == mca_btl_openib_component.device_type) ?
                        "iWARP" : "<any>"));
        goto no_btls;
    }

    /* If we got back from checking all the devices and find that
       there are still items in the component.if_list, that means that
       they didn't exist.  Show an appropriate warning if the warning
       was not disabled. */

    if (0 != opal_argv_count(mca_btl_openib_component.if_list) &&
        mca_btl_openib_component.warn_nonexistent_if) {
        char *str = opal_argv_join(mca_btl_openib_component.if_list, ',');
        orte_show_help("help-mpi-btl-openib.txt", "nonexistent port",
                       true, orte_process_info.nodename,
                       ((NULL != mca_btl_openib_component.if_include) ?
                        "in" : "ex"), str);
        free(str);
    }

    if(0 == mca_btl_openib_component.ib_num_btls) {
        /* If there were unusable devices that weren't specifically
           ignored, warn about it */
        if (num_devices_intentionally_ignored < num_devs) {
            orte_show_help("help-mpi-btl-openib.txt",
                           "no active ports found", true, 
                           orte_process_info.nodename);
        }
        goto no_btls;
    }

    /* Setup the BSRQ QP's based on the final value of
       mca_btl_openib_component.receive_queues. */
    if (OMPI_SUCCESS != setup_qps()) {
        goto no_btls;
    }
#if OPAL_HAVE_THREADS
    if (mca_btl_openib_component.num_srq_qps > 0 ||
                     mca_btl_openib_component.num_xrc_qps > 0) {
        opal_hash_table_t *srq_addr_table = &mca_btl_openib_component.srq_manager.srq_addr_table;
        if(OPAL_SUCCESS != opal_hash_table_init(
                srq_addr_table, (mca_btl_openib_component.num_srq_qps +
                                 mca_btl_openib_component.num_xrc_qps) *
                                 mca_btl_openib_component.ib_num_btls)) {
            BTL_ERROR(("SRQ internal error. Failed to allocate SRQ addr hash table"));
            goto no_btls;
        }
    }
#endif

    /* For XRC: 
     * from this point we know if MCA_BTL_XRC_ENABLED it true or false */

    /* Init XRC IB Addr hash table */
    if (MCA_BTL_XRC_ENABLED) {
        OBJ_CONSTRUCT(&mca_btl_openib_component.ib_addr_table,
                opal_hash_table_t);
    }

    /* Allocate space for btl modules */
    mca_btl_openib_component.openib_btls =
        malloc(sizeof(mca_btl_openib_module_t*) *
                mca_btl_openib_component.ib_num_btls);
    if(NULL == mca_btl_openib_component.openib_btls) {
        BTL_ERROR(("Failed malloc: %s:%d", __FILE__, __LINE__));
        goto no_btls;
    }
    btls = (struct mca_btl_base_module_t **)
        malloc(mca_btl_openib_component.ib_num_btls *
               sizeof(struct mca_btl_base_module_t*));
    if(NULL == btls) {
        BTL_ERROR(("Failed malloc: %s:%d", __FILE__, __LINE__));
        goto no_btls;
    }

    /* Copy the btl module structs into a contiguous array and fully
       initialize them */
    i = 0;
    while (NULL != (item = opal_list_remove_first(&btl_list))) {
        mca_btl_openib_device_t *device;
        int qp_index;

        ib_selected = (mca_btl_base_selected_module_t*)item;
        openib_btl = (mca_btl_openib_module_t*)ib_selected->btl_module;
        device = openib_btl->device;
        OBJ_RELEASE(ib_selected);

        /* Search for a CPC that can handle this port */
        ret = ompi_btl_openib_connect_base_select_for_local_port(openib_btl);
        if ((OMPI_SUCCESS != ret) && (OMPI_ERR_NOT_SUPPORTED != ret)) {
            /* All errors except NOT_SUPPORTED are fatal. Note that we already
               did a show_help in the lower layer */
            goto no_btls;
        }

        /* Finish BTL initialization */
        if (finish_btl_init(openib_btl) != OMPI_SUCCESS) {
            goto no_btls;
        }

        /* If we get NOT_SUPPORTED, then no CPC was found for this
            port.  But that's not a fatal error -- destroy this openib module
            and keep going; let's see if we find any usable openib modules or not. */
        if (OMPI_ERR_NOT_SUPPORTED == ret) {
            mca_btl_openib_finalize(&openib_btl->super);
            continue;
        } else /* OMPI_SUCCESS == ret */ {
            mca_btl_openib_component.openib_btls[i] = openib_btl;
            btls[i] = &openib_btl->super;
            ++i;
        }

        /* For each btl module that we made - find every
           base device that doesn't have device->qps setup on it yet (remember
           that some modules may share the same device, so when going through
           to loop, we may hit a device that was already setup earlier in
           the loop). 
           
           We may to call for prepare_device_for_use() only after adding the btl
           to mca_btl_openib_component.openib_btls, since the prepare_device_for_use 
           adds device to async thread that require access to 
           mca_btl_openib_component.openib_btls.
        */

        if (NULL == device->qps) {
            /* Setup the device qps info */
            device->qps = (mca_btl_openib_device_qp_t*)
                calloc(mca_btl_openib_component.num_qps,
                       sizeof(mca_btl_openib_device_qp_t));
            for (qp_index = 0; qp_index < mca_btl_openib_component.num_qps; qp_index++) {
                OBJ_CONSTRUCT(&device->qps[qp_index].send_free, ompi_free_list_t);
                OBJ_CONSTRUCT(&device->qps[qp_index].recv_free, ompi_free_list_t);
            }

            /* Do finial init on device */
            ret = prepare_device_for_use(device);
            if (OMPI_SUCCESS != ret) {
                orte_show_help("help-mpi-btl-openib.txt",
                               "error in device init", true, 
                               orte_process_info.nodename,
                               ibv_get_device_name(device->ib_dev));
                goto no_btls;
            }
        }
    }
    /* If we got nothing, then error out */
    if (0 == i) {
        goto no_btls;
    }
    /* Otherwise reset to the number of openib modules that we
       actually got */
    mca_btl_openib_component.ib_num_btls = i;

    btl_openib_modex_send();

    *num_btl_modules = mca_btl_openib_component.ib_num_btls;
    ibv_free_device_list_compat(ib_devs);
    if (NULL != mca_btl_openib_component.if_include_list) {
        opal_argv_free(mca_btl_openib_component.if_include_list);
        mca_btl_openib_component.if_include_list = NULL;
    }
    if (NULL != mca_btl_openib_component.if_exclude_list) {
        opal_argv_free(mca_btl_openib_component.if_exclude_list);
        mca_btl_openib_component.if_exclude_list = NULL;
    }
    
    /* setup the fork warning message as we are sensitive
     * to memory corruption issues when fork is called
     */
    ompi_warn_fork();
    
    return btls;

 no_btls:
    /* If we fail early enough in the setup, we just modex around that
       there are no openib BTL's in this process and return NULL. */

    mca_btl_openib_component.ib_num_btls = 0;
    btl_openib_modex_send();
#if BTL_OPENIB_MALLOC_HOOKS_ENABLED 
    /*Unset malloc hook since the component won't start*/ 
    if (malloc_hook_set) { 
        __malloc_hook = mca_btl_openib_component.previous_malloc_hook; 
    } 
#endif
    return NULL;
}

/*
 * Progress the no_credits_pending_frags lists on all qp's
 */
static int progress_no_credits_pending_frags(mca_btl_base_endpoint_t *ep)
{
    int qp, pri, rc, len;
    opal_list_item_t *frag;

    OPAL_THREAD_LOCK(&ep->endpoint_lock);

    /* Traverse all QPs and all priorities */
    for (qp = 0; qp < mca_btl_openib_component.num_qps; ++qp) {
        for (pri = 0; pri < 2; ++pri) {
            /* Note that entries in the no_credits_pending_frags list
               may be eager RDMA or send fragments.  So be sure to
               check that we have at least 1 RDMA or send credit.

               This loop needs a little explaining.  :-\

               In the body of the loop, we call _endpoint_post_send().
               The frag will either be successfully sent, or it will
               be [re]added to the no_credit_pending_frags list.  So
               if we keep trying to drain the no_credits_pending_frag
               list, we could end up in an infinite loop.  So instead,
               we get the initial length of the list and ensure to run
               through every entry at least once.  This attempts to
               send *every* frag once and catches the case where a
               frag may be on the RDMA list, but because of
               coalescing, is now too big for RDMA and defaults over
               to sending -- but then we're out of send credits, so it
               doesn't go.  But if we *do* still have some RDMA
               credits and there are RDMA frags on the list behind
               this now-too-big frag, they'll get a chance to go.

               Specifically, the condition in this for loop is as follows:

               - len > 0: ensure to go through all entries in the list once
               - the 2nd part of the conditional checks to see if we
                 have any credits at all.  Specifically, do we have
                 any RDMA credits or any send credits, *or* are we on
                 an SRQ, in which case we define that we *always* have
                 credits (because the hardware will continually
                 retransmit for us).
            */
            for (len = opal_list_get_size(&ep->qps[qp].no_credits_pending_frags[pri]);
                 len > 0 && 
                     (ep->eager_rdma_remote.tokens > 0 ||
                      ep->qps[qp].u.pp_qp.sd_credits > 0 ||
                      !BTL_OPENIB_QP_TYPE_PP(qp)); --len) {
                frag = opal_list_remove_first(&ep->qps[qp].no_credits_pending_frags[pri]);

                /* If _endpoint_post_send() fails because of
                   RESOURCE_BUSY, then the frag was re-added to the
                   no_credits_pending list.  Specifically: either the
                   frag was initially an RDMA frag, but there were no
                   RDMA credits so it fell through the trying to send,
                   but we had no send credits and therefore re-added
                   the frag to the no_credits list, or the frag was a
                   send frag initially (and the same sequence
                   occurred, starting at the send frag out-of-credits
                   scenario).  In this case, just continue and try the
                   rest of the frags in the list.

                   If it fails because of another error, return the
                   error upward. */
                rc = mca_btl_openib_endpoint_post_send(ep, to_send_frag(frag));
                if (OPAL_UNLIKELY(OMPI_SUCCESS != rc && 
                                  OMPI_ERR_RESOURCE_BUSY != rc)) {
                    OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
                    return rc;
                }
            }
        }
    }

    OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
    return OMPI_SUCCESS;
}

void mca_btl_openib_frag_progress_pending_put_get(mca_btl_base_endpoint_t *ep,
        const int qp)
{
    mca_btl_openib_module_t* openib_btl = ep->endpoint_btl;
    opal_list_item_t *frag;
    size_t i, len = opal_list_get_size(&ep->pending_get_frags);

    for(i = 0; i < len && ep->qps[qp].qp->sd_wqe > 0 && ep->get_tokens > 0; i++)
    {
        OPAL_THREAD_LOCK(&ep->endpoint_lock);
        frag = opal_list_remove_first(&(ep->pending_get_frags));
        OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
        if(NULL == frag)
            break;
        if(mca_btl_openib_get((mca_btl_base_module_t *)openib_btl, ep,
                    &to_base_frag(frag)->base) == OMPI_ERR_OUT_OF_RESOURCE)
            break;
    }

    len = opal_list_get_size(&ep->pending_put_frags);
    for(i = 0; i < len && ep->qps[qp].qp->sd_wqe > 0; i++) {
        OPAL_THREAD_LOCK(&ep->endpoint_lock);
        frag = opal_list_remove_first(&(ep->pending_put_frags));
        OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
        if(NULL == frag)
            break;
        if(mca_btl_openib_put((mca_btl_base_module_t*)openib_btl, ep,
                    &to_base_frag(frag)->base) == OMPI_ERR_OUT_OF_RESOURCE)
            break;
    }
}

static int btl_openib_handle_incoming(mca_btl_openib_module_t *openib_btl,
                                         mca_btl_openib_endpoint_t *ep,
                                         mca_btl_openib_recv_frag_t *frag,
                                         size_t byte_len)
{
    mca_btl_base_descriptor_t *des = &to_base_frag(frag)->base;
    mca_btl_openib_header_t *hdr = frag->hdr;
    int rqp = to_base_frag(frag)->base.order, cqp;
    uint16_t rcredits = 0, credits;
    bool is_credit_msg;

    if(ep->nbo) {
        BTL_OPENIB_HEADER_NTOH(*hdr);
    }

    /* advance the segment address past the header and subtract from the
     * length.*/
    des->des_dst->seg_len = byte_len - sizeof(mca_btl_openib_header_t);

    if(OPAL_LIKELY(!(is_credit_msg = is_credit_message(frag)))) {
        /* call registered callback */
        mca_btl_active_message_callback_t* reg;
        reg = mca_btl_base_active_message_trigger + hdr->tag;
        reg->cbfunc( &openib_btl->super, hdr->tag, des, reg->cbdata );
        if(MCA_BTL_OPENIB_RDMA_FRAG(frag)) {
            cqp = (hdr->credits >> 11) & 0x0f;
            hdr->credits &= 0x87ff;
        } else {
            cqp = rqp;
        }
        if(BTL_OPENIB_IS_RDMA_CREDITS(hdr->credits)) {
            rcredits = BTL_OPENIB_CREDITS(hdr->credits);
            hdr->credits = 0;
        }
    } else {
        mca_btl_openib_rdma_credits_header_t *chdr=des->des_dst->seg_addr.pval;
        if(ep->nbo) {
            BTL_OPENIB_RDMA_CREDITS_HEADER_NTOH(*chdr);
        }
        cqp = chdr->qpn;
        rcredits = chdr->rdma_credits;
    }

    credits = hdr->credits;

    if(hdr->cm_seen)
         OPAL_THREAD_ADD32(&ep->qps[cqp].u.pp_qp.cm_sent, -hdr->cm_seen);

    /* Now return fragment. Don't touch hdr after this point! */
    if(MCA_BTL_OPENIB_RDMA_FRAG(frag)) {
        mca_btl_openib_eager_rdma_local_t *erl = &ep->eager_rdma_local;
        OPAL_THREAD_LOCK(&erl->lock);
        MCA_BTL_OPENIB_RDMA_MAKE_REMOTE(frag->ftr);
        while(erl->tail != erl->head) {
            mca_btl_openib_recv_frag_t *tf;
            tf = MCA_BTL_OPENIB_GET_LOCAL_RDMA_FRAG(ep, erl->tail);
            if(MCA_BTL_OPENIB_RDMA_FRAG_LOCAL(tf))
                break;
            OPAL_THREAD_ADD32(&erl->credits, 1);
            MCA_BTL_OPENIB_RDMA_NEXT_INDEX(erl->tail);
        }
        OPAL_THREAD_UNLOCK(&erl->lock);
    } else {
        if (is_cts_message(frag)) {
            /* If this was a CTS, free it here (it was
               malloc'ed+ibv_reg_mr'ed -- so it should *not* be
               FRAG_RETURN'ed). */
            int rc = ompi_btl_openib_connect_base_free_cts(ep);
            if (OMPI_SUCCESS != rc) {
                return rc;
            }
        } else {
            /* Otherwise, FRAG_RETURN it and repost if necessary */
            MCA_BTL_IB_FRAG_RETURN(frag);
            if (BTL_OPENIB_QP_TYPE_PP(rqp)) {
                if (OPAL_UNLIKELY(is_credit_msg)) {
                    OPAL_THREAD_ADD32(&ep->qps[cqp].u.pp_qp.cm_received, 1);
                } else {
                    OPAL_THREAD_ADD32(&ep->qps[rqp].u.pp_qp.rd_posted, -1);
                }
                mca_btl_openib_endpoint_post_rr(ep, cqp);
            } else {
                mca_btl_openib_module_t *btl = ep->endpoint_btl;
                OPAL_THREAD_ADD32(&btl->qps[rqp].u.srq_qp.rd_posted, -1);
                mca_btl_openib_post_srr(btl, rqp);
            }
        }
    }

    assert((cqp != MCA_BTL_NO_ORDER && BTL_OPENIB_QP_TYPE_PP(cqp)) || !credits);

    /* If we got any credits (RDMA or send), then try to progress all
       the no_credits_pending_frags lists */
    if (rcredits > 0) {
        OPAL_THREAD_ADD32(&ep->eager_rdma_remote.tokens, rcredits);
    }
    if (credits > 0) {
        OPAL_THREAD_ADD32(&ep->qps[cqp].u.pp_qp.sd_credits, credits);
    }
    if (rcredits + credits > 0) {
        int rc;

        if (OMPI_SUCCESS != 
            (rc = progress_no_credits_pending_frags(ep))) {
            return rc;
        }
    }

    send_credits(ep, cqp);

    return OMPI_SUCCESS;
}

static char* btl_openib_component_status_to_string(enum ibv_wc_status status)
{
    switch(status) {
    case IBV_WC_SUCCESS:
        return "SUCCESS";
        break;
    case IBV_WC_LOC_LEN_ERR:
        return "LOCAL LENGTH ERROR";
        break;
    case IBV_WC_LOC_QP_OP_ERR:
        return "LOCAL QP OPERATION ERROR";
        break;
    case IBV_WC_LOC_EEC_OP_ERR:
        return "LOCAL EEC OPERATION ERROR";
        break;
    case IBV_WC_LOC_PROT_ERR:
        return "LOCAL PROTOCOL ERROR";
        break;
    case IBV_WC_WR_FLUSH_ERR:
        return "WORK REQUEST FLUSHED ERROR";
        break;
    case IBV_WC_MW_BIND_ERR:
        return "MEMORY WINDOW BIND ERROR";
        break;
    case IBV_WC_BAD_RESP_ERR:
        return "BAD RESPONSE ERROR";
        break;
    case IBV_WC_LOC_ACCESS_ERR:
        return "LOCAL ACCESS ERROR";
        break;
    case IBV_WC_REM_INV_REQ_ERR:
        return "INVALID REQUEST ERROR";
        break;
    case IBV_WC_REM_ACCESS_ERR:
        return "REMOTE ACCESS ERROR";
        break;
    case IBV_WC_REM_OP_ERR:
        return "REMOTE OPERATION ERROR";
        break;
    case IBV_WC_RETRY_EXC_ERR:
        return "RETRY EXCEEDED ERROR";
        break;
    case IBV_WC_RNR_RETRY_EXC_ERR:
        return "RECEIVER NOT READY RETRY EXCEEDED ERROR";
        break;
    case IBV_WC_LOC_RDD_VIOL_ERR:
        return "LOCAL RDD VIOLATION ERROR";
        break;
    case IBV_WC_REM_INV_RD_REQ_ERR:
        return "INVALID READ REQUEST ERROR";
        break;
    case IBV_WC_REM_ABORT_ERR:
        return "REMOTE ABORT ERROR";
        break;
    case IBV_WC_INV_EECN_ERR:
        return "INVALID EECN ERROR";
        break;
    case IBV_WC_INV_EEC_STATE_ERR:
        return "INVALID EEC STATE ERROR";
        break;
    case IBV_WC_FATAL_ERR:
        return "FATAL ERROR";
        break;
    case IBV_WC_RESP_TIMEOUT_ERR:
        return "RESPONSE TIMEOUT ERROR";
        break;
    case IBV_WC_GENERAL_ERR:
        return "GENERAL ERROR";
        break;
    default:
        return "STATUS UNDEFINED";
        break;
    }
}

static void
progress_pending_frags_wqe(mca_btl_base_endpoint_t *ep, const int qpn)
{
    int i;
    opal_list_item_t *frag;
    mca_btl_openib_qp_t *qp = ep->qps[qpn].qp;

    OPAL_THREAD_LOCK(&ep->endpoint_lock);
    for(i = 0; i < 2; i++) {
       while(qp->sd_wqe > 0) {
            mca_btl_base_endpoint_t *tmp_ep;
            frag = opal_list_remove_first(&ep->qps[qpn].no_wqe_pending_frags[i]);
            if(NULL == frag)
                break;
            tmp_ep = to_com_frag(frag)->endpoint;
            mca_btl_openib_endpoint_post_send(tmp_ep, to_send_frag(frag));
       }
    }
    OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
}

static void progress_pending_frags_srq(mca_btl_openib_module_t* openib_btl,
        const int qp)
{
    opal_list_item_t *frag;
    int i;

    assert(BTL_OPENIB_QP_TYPE_SRQ(qp) || BTL_OPENIB_QP_TYPE_XRC(qp));

    for(i = 0; i < 2; i++) {
        while(openib_btl->qps[qp].u.srq_qp.sd_credits > 0) {
            OPAL_THREAD_LOCK(&openib_btl->ib_lock);
            frag = opal_list_remove_first(
                    &openib_btl->qps[qp].u.srq_qp.pending_frags[i]);
            OPAL_THREAD_UNLOCK(&openib_btl->ib_lock);

            if(NULL == frag)
                break;

            mca_btl_openib_endpoint_send(to_com_frag(frag)->endpoint,
                    to_send_frag(frag));
        }
    }
}

static char *cq_name[] = {"HP CQ", "LP CQ"};
static void handle_wc(mca_btl_openib_device_t* device, const uint32_t cq,
        struct ibv_wc *wc)
{
    static int flush_err_printed[] = {0, 0};
    mca_btl_openib_com_frag_t* frag;
    mca_btl_base_descriptor_t *des;
    mca_btl_openib_endpoint_t* endpoint;
    mca_btl_openib_module_t *openib_btl = NULL;
    ompi_proc_t* remote_proc = NULL;
    int qp, btl_ownership;
    int n;

    des = (mca_btl_base_descriptor_t*)(uintptr_t)wc->wr_id;
    frag = to_com_frag(des);

    /* For receive fragments "order" contains QP idx the fragment was posted
     * to. For send fragments "order" contains QP idx the fragment was send
     * through */
    qp = des->order;
    endpoint = frag->endpoint;

    if(endpoint)
        openib_btl = endpoint->endpoint_btl;

    if(wc->status != IBV_WC_SUCCESS) {
        OPAL_OUTPUT((-1, "Got WC: ERROR"));
        goto error;
    }

    /* Handle work completions */
    switch(wc->opcode) {
        case IBV_WC_RDMA_READ:
            OPAL_OUTPUT((-1, "Got WC: RDMA_READ"));
            OPAL_THREAD_ADD32(&endpoint->get_tokens, 1);
            /* fall through */

        case IBV_WC_RDMA_WRITE:
        case IBV_WC_SEND:
            OPAL_OUTPUT((-1, "Got WC: RDMA_WRITE or SEND"));
            if(openib_frag_type(des) == MCA_BTL_OPENIB_FRAG_SEND) {
                opal_list_item_t *i;
                while((i = opal_list_remove_first(&to_send_frag(des)->coalesced_frags))) {
                    btl_ownership = (to_base_frag(i)->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
#if BTL_OPENIB_FAILOVER_ENABLED
                    /* The check for the callback flag is only needed when running
                     * with the failover case because there is a chance that a fragment
                     * generated from a sendi call (which does not set the flag) gets
                     * coalesced.  In normal operation, this cannot happen as the sendi
                     * call will never queue up a fragment which could potentially become
                     * a coalesced fragment.  It will revert to a regular send. */
                    if (to_base_frag(i)->base.des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK) {
#endif
                        to_base_frag(i)->base.des_cbfunc(&openib_btl->super, endpoint,
                                &to_base_frag(i)->base, OMPI_SUCCESS);
#if BTL_OPENIB_FAILOVER_ENABLED
                    }
#endif
                    if( btl_ownership ) {
                        mca_btl_openib_free(&openib_btl->super, &to_base_frag(i)->base);
                    }
                }
            }
            /* Process a completed send/put/get */
            btl_ownership = (des->des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
            if (des->des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK) {
                des->des_cbfunc(&openib_btl->super, endpoint, des,OMPI_SUCCESS);
            }
            if( btl_ownership ) {
                mca_btl_openib_free(&openib_btl->super, des);
            }

            /* return send wqe */
            qp_put_wqe(endpoint, qp);

            /* return wqes that were sent before this frag */
            n = qp_frag_to_wqe(endpoint, qp, to_com_frag(des));

            if(IBV_WC_SEND == wc->opcode && !BTL_OPENIB_QP_TYPE_PP(qp)) {
                OPAL_THREAD_ADD32(&openib_btl->qps[qp].u.srq_qp.sd_credits, 1+n);

                /* new SRQ credit available. Try to progress pending frags*/
                progress_pending_frags_srq(openib_btl, qp);
            }
            /* new wqe or/and get token available. Try to progress pending frags */
            progress_pending_frags_wqe(endpoint, qp);
            mca_btl_openib_frag_progress_pending_put_get(endpoint, qp);
            break;
        case IBV_WC_RECV:
            OPAL_OUTPUT((-1, "Got WC: RDMA_RECV, qp %d, src qp %d, WR ID %" PRIx64,
                         wc->qp_num, wc->src_qp, wc->wr_id));

#if !defined(WORDS_BIGENDIAN) && OPAL_ENABLE_HETEROGENEOUS_SUPPORT
            wc->imm_data = ntohl(wc->imm_data);
#endif
            if(wc->wc_flags & IBV_WC_WITH_IMM) {
                endpoint = (mca_btl_openib_endpoint_t*)
                    opal_pointer_array_get_item(device->endpoints, wc->imm_data);
                frag->endpoint = endpoint;
                openib_btl = endpoint->endpoint_btl;
            }

            /* Process a RECV */
            if(btl_openib_handle_incoming(openib_btl, endpoint, to_recv_frag(frag),
                        wc->byte_len) != OMPI_SUCCESS) {
                openib_btl->error_cb(&openib_btl->super, MCA_BTL_ERROR_FLAGS_FATAL,
                                     NULL, NULL);
                break;
            }

            /* decide if it is time to setup an eager rdma channel */
            if(!endpoint->eager_rdma_local.base.pval && endpoint->use_eager_rdma &&
                    wc->byte_len < mca_btl_openib_component.eager_limit &&
                    openib_btl->eager_rdma_channels <
                    mca_btl_openib_component.max_eager_rdma &&
                    OPAL_THREAD_ADD32(&endpoint->eager_recv_count, 1) ==
                    mca_btl_openib_component.eager_rdma_threshold) {
                mca_btl_openib_endpoint_connect_eager_rdma(endpoint);
            }
            break;
        default:
            BTL_ERROR(("Unhandled work completion opcode is %d", wc->opcode));
            if(openib_btl)
                openib_btl->error_cb(&openib_btl->super, MCA_BTL_ERROR_FLAGS_FATAL,
                                     NULL, NULL);
            break;
    }

    return;

error:
    if(endpoint && endpoint->endpoint_proc && endpoint->endpoint_proc->proc_ompi)
        remote_proc = endpoint->endpoint_proc->proc_ompi;

    /* For iWARP, the TCP connection is tied to the QP once the QP is
     * in RTS.  And destroying the QP is thus tied to connection
     * teardown for iWARP.  To destroy the connection in iWARP you
     * must move the QP out of RTS, either into CLOSING for a nice
     * graceful close (e.g., via rdma_disconnect()), or to ERROR if
     * you want to be rude (e.g., just destroying the QP without
     * disconnecting first).  In both cases, all pending non-completed
     * SQ and RQ WRs will automatically be flushed.
     */
#if defined(HAVE_STRUCT_IBV_DEVICE_TRANSPORT_TYPE)
    if (IBV_WC_WR_FLUSH_ERR == wc->status && 
        IBV_TRANSPORT_IWARP == device->ib_dev->transport_type) {
        return;
    }
#endif

    if(IBV_WC_WR_FLUSH_ERR != wc->status || !flush_err_printed[cq]++) {
        BTL_PEER_ERROR(remote_proc, ("error polling %s with status %s "
                    "status number %d for wr_id %" PRIx64 " opcode %d  vendor error %d qp_idx %d",
                    cq_name[cq], btl_openib_component_status_to_string(wc->status),
                    wc->status, wc->wr_id, 
                    wc->opcode, wc->vendor_err, qp));
        orte_notifier.peer(ORTE_NOTIFIER_CRIT, ORTE_ERR_COMM_FAILURE,
                           remote_proc ? &remote_proc->proc_name : NULL,
                           "\n\tIB polling %s with status %s "
                           "status number %d for wr_id %" PRIx64 " opcode %d vendor error %d qp_idx %d",
                           cq_name[cq],
                           btl_openib_component_status_to_string(wc->status),
                           wc->status, wc->wr_id, 
                             wc->opcode, wc->vendor_err, qp);
    }

    if (IBV_WC_RNR_RETRY_EXC_ERR == wc->status ||
        IBV_WC_RETRY_EXC_ERR == wc->status) {
        char *peer_hostname = 
            (NULL != endpoint->endpoint_proc->proc_ompi->proc_hostname) ?
            endpoint->endpoint_proc->proc_ompi->proc_hostname : 
            "<unknown -- please run with mpi_keep_peer_hostnames=1>";
        const char *device_name = 
            ibv_get_device_name(endpoint->qps[qp].qp->lcl_qp->context->device);

        if (IBV_WC_RNR_RETRY_EXC_ERR == wc->status) {
            orte_show_help("help-mpi-btl-openib.txt",
                           BTL_OPENIB_QP_TYPE_PP(qp) ? 
                           "pp rnr retry exceeded" : 
                           "srq rnr retry exceeded", true,
                           orte_process_info.nodename, device_name,
                           peer_hostname);
            orte_notifier.help(ORTE_NOTIFIER_CRIT, ORTE_ERR_COMM_FAILURE,
                                    "help-mpi-btl-openib.txt",
                                    BTL_OPENIB_QP_TYPE_PP(qp) ? 
                                    "pp rnr retry exceeded" : 
                                    "srq rnr retry exceeded",
                                    orte_process_info.nodename, device_name,
                                    peer_hostname);
        } else if (IBV_WC_RETRY_EXC_ERR == wc->status) {
            orte_show_help("help-mpi-btl-openib.txt", 
                           "pp retry exceeded", true,
                           orte_process_info.nodename,
                           device_name, peer_hostname);
            orte_notifier.help(ORTE_NOTIFIER_CRIT, ORTE_ERR_COMM_FAILURE,
                                    "help-mpi-btl-openib.txt", 
                                    "pp retry exceeded",
                                    orte_process_info.nodename,
                                    device_name, peer_hostname);
        }
    }

#if BTL_OPENIB_FAILOVER_ENABLED
    mca_btl_openib_handle_endpoint_error(openib_btl, des, qp,
                                         remote_proc, endpoint);
#else
    if(openib_btl)
        openib_btl->error_cb(&openib_btl->super, MCA_BTL_ERROR_FLAGS_FATAL,
                             remote_proc, NULL);
#endif
}

static int poll_device(mca_btl_openib_device_t* device, int count)
{
    int ne = 0, cq;
    uint32_t hp_iter = 0;
    struct ibv_wc wc[MCA_BTL_OPENIB_CQ_POLL_BATCH_DEFAULT];
    int i;

    device->pollme = false;
    for(cq = 0; cq < 2 && hp_iter < mca_btl_openib_component.cq_poll_progress;)
    {
        ne = ibv_poll_cq(device->ib_cq[cq], mca_btl_openib_component.cq_poll_batch, wc);
        if(0 == ne) {
            /* don't check low prio cq if there was something in high prio cq,
             * but for each cq_poll_ratio hp cq polls poll lp cq once */
            if(count && device->hp_cq_polls)
                break;
            cq++;
            device->hp_cq_polls = mca_btl_openib_component.cq_poll_ratio;
            continue;
        }

        if(ne < 0)
            goto error;

        count++;

        if(BTL_OPENIB_HP_CQ == cq) {
            device->pollme = true;
            hp_iter++;
            device->hp_cq_polls--;
        }

        for (i = 0; i < ne; i++)
            handle_wc(device, cq, &wc[i]);
    }

    return count;
error:
    BTL_ERROR(("error polling %s with %d errno says %s", cq_name[cq], ne,
                strerror(errno)));
    return count;
}

#if OPAL_ENABLE_PROGRESS_THREADS
void* mca_btl_openib_progress_thread(opal_object_t* arg)
{
    opal_thread_t* thread = (opal_thread_t*)arg;
    mca_btl_openib_device_t* device = thread->t_arg;
    struct ibv_cq *ev_cq;
    void *ev_ctx;

    /* This thread enter in a cancel enabled state */
    pthread_setcancelstate( PTHREAD_CANCEL_ENABLE, NULL );
    pthread_setcanceltype( PTHREAD_CANCEL_ASYNCHRONOUS, NULL );

    opal_output(-1, "WARNING: the openib btl progress thread code *does not yet work*.  Your run is likely to hang, crash, break the kitchen sink, and/or eat your cat.  You have been warned.");

    while (device->progress) {
        while(opal_progress_threads()) {
            while(opal_progress_threads())
                sched_yield();
            usleep(100); /* give app a chance to re-enter library */
        }

        if(ibv_get_cq_event(device->ib_channel, &ev_cq, &ev_ctx))
            BTL_ERROR(("Failed to get CQ event with error %s",
                        strerror(errno)));
        if(ibv_req_notify_cq(ev_cq, 0)) {
            BTL_ERROR(("Couldn't request CQ notification with error %s",
                        strerror(errno)));
        }

        ibv_ack_cq_events(ev_cq, 1);

        while(poll_device(device, 0));
    }

    return PTHREAD_CANCELED;
}
#endif

static int progress_one_device(mca_btl_openib_device_t *device)
{
    int i, c, count = 0, ret;
    mca_btl_openib_recv_frag_t* frag;
    mca_btl_openib_endpoint_t* endpoint;
    uint32_t non_eager_rdma_endpoints = 0;

    c = device->eager_rdma_buffers_count;
    non_eager_rdma_endpoints += (device->non_eager_rdma_endpoints + device->pollme);

    for(i = 0; i < c; i++) {
        endpoint = device->eager_rdma_buffers[i];

        if(!endpoint)
            continue;

        OPAL_THREAD_LOCK(&endpoint->eager_rdma_local.lock);
        frag = MCA_BTL_OPENIB_GET_LOCAL_RDMA_FRAG(endpoint,
                endpoint->eager_rdma_local.head);

        if(MCA_BTL_OPENIB_RDMA_FRAG_LOCAL(frag)) {
            uint32_t size;
            mca_btl_openib_module_t *btl = endpoint->endpoint_btl;

            opal_atomic_mb();

            if(endpoint->nbo) {
                BTL_OPENIB_FOOTER_NTOH(*frag->ftr);
            }
            size = MCA_BTL_OPENIB_RDMA_FRAG_GET_SIZE(frag->ftr);
#if OPAL_ENABLE_DEBUG
            if (frag->ftr->seq != endpoint->eager_rdma_local.seq)
                BTL_ERROR(("Eager RDMA wrong SEQ: received %d expected %d",
                           frag->ftr->seq,
                           endpoint->eager_rdma_local.seq));
            endpoint->eager_rdma_local.seq++;
#endif
            MCA_BTL_OPENIB_RDMA_NEXT_INDEX(endpoint->eager_rdma_local.head);

            OPAL_THREAD_UNLOCK(&endpoint->eager_rdma_local.lock);
            frag->hdr = (mca_btl_openib_header_t*)(((char*)frag->ftr) -
                size - BTL_OPENIB_FTR_PADDING(size) + sizeof(mca_btl_openib_footer_t));
            to_base_frag(frag)->segment.seg_addr.pval =
                ((unsigned char* )frag->hdr) + sizeof(mca_btl_openib_header_t);

            ret = btl_openib_handle_incoming(btl, to_com_frag(frag)->endpoint,
                    frag, size - sizeof(mca_btl_openib_footer_t));
            if (ret != OMPI_SUCCESS) {
                btl->error_cb(&btl->super, MCA_BTL_ERROR_FLAGS_FATAL, NULL, NULL);
                return 0;
            }

            count++;
        } else
            OPAL_THREAD_UNLOCK(&endpoint->eager_rdma_local.lock);
    }

    device->eager_rdma_polls--;

    if(0 == count || non_eager_rdma_endpoints != 0 || !device->eager_rdma_polls) {
        count += poll_device(device, count);
        device->eager_rdma_polls = mca_btl_openib_component.eager_rdma_poll_ratio;
    }

    return count;
}

/*
 *  IB component progress.
 */
static int btl_openib_component_progress(void)
{
    int i;
    int count = 0;

#if OPAL_HAVE_THREADS
    if(OPAL_UNLIKELY(mca_btl_openib_component.use_async_event_thread &&
            mca_btl_openib_component.error_counter)) {
        goto error;
    }
#endif

    for(i = 0; i < mca_btl_openib_component.devices_count; i++) {
        mca_btl_openib_device_t *device =
            opal_pointer_array_get_item(&mca_btl_openib_component.devices, i);
        count += progress_one_device(device);
    }

    return count;

#if OPAL_HAVE_THREADS
error:
    /* Set the fatal counter to zero */
    mca_btl_openib_component.error_counter = 0;
    /* Lets find all error events */
    for(i = 0; i < mca_btl_openib_component.ib_num_btls; i++) {
        mca_btl_openib_module_t* openib_btl =
            mca_btl_openib_component.openib_btls[i];
        if(openib_btl->device->got_fatal_event) {
            openib_btl->error_cb(&openib_btl->super, MCA_BTL_ERROR_FLAGS_FATAL,
                                 NULL, NULL);
        }
        if(openib_btl->device->got_port_event) {
            /* These are non-fatal so just ignore it. */
            openib_btl->device->got_port_event = false;
#if BTL_OPENIB_FAILOVER_ENABLED
            mca_btl_openib_handle_btl_error(openib_btl);
#endif
        }
    }
    return count;
#endif
}

int mca_btl_openib_post_srr(mca_btl_openib_module_t* openib_btl, const int qp)
{
    int rd_low_local = openib_btl->qps[qp].u.srq_qp.rd_low_local;
    int rd_curr_num = openib_btl->qps[qp].u.srq_qp.rd_curr_num;
    int num_post, i, rc;
    struct ibv_recv_wr *bad_wr, *wr_list = NULL, *wr = NULL;

    assert(!BTL_OPENIB_QP_TYPE_PP(qp));

    OPAL_THREAD_LOCK(&openib_btl->ib_lock);
    if(openib_btl->qps[qp].u.srq_qp.rd_posted > rd_low_local) {
        OPAL_THREAD_UNLOCK(&openib_btl->ib_lock);
        return OMPI_SUCCESS;
    }
    num_post = rd_curr_num - openib_btl->qps[qp].u.srq_qp.rd_posted;

    if (0 == num_post) {
        OPAL_THREAD_UNLOCK(&openib_btl->ib_lock);
        return OMPI_SUCCESS;
    }

    for(i = 0; i < num_post; i++) {
        ompi_free_list_item_t* item;
        OMPI_FREE_LIST_WAIT(&openib_btl->device->qps[qp].recv_free, item, rc);
        to_base_frag(item)->base.order = qp;
        to_com_frag(item)->endpoint = NULL;
        if(NULL == wr)
            wr = wr_list = &to_recv_frag(item)->rd_desc;
        else
            wr = wr->next = &to_recv_frag(item)->rd_desc;
    }

    wr->next = NULL;

    rc = ibv_post_srq_recv(openib_btl->qps[qp].u.srq_qp.srq, wr_list, &bad_wr);
    if(OPAL_LIKELY(0 == rc)) {
        struct ibv_srq_attr srq_attr;

        OPAL_THREAD_ADD32(&openib_btl->qps[qp].u.srq_qp.rd_posted, num_post);

        if(true == openib_btl->qps[qp].u.srq_qp.srq_limit_event_flag) {
            srq_attr.max_wr = openib_btl->qps[qp].u.srq_qp.rd_curr_num;
            srq_attr.max_sge = 1;
            srq_attr.srq_limit = mca_btl_openib_component.qp_infos[qp].u.srq_qp.srq_limit;

            openib_btl->qps[qp].u.srq_qp.srq_limit_event_flag = false;
            if(ibv_modify_srq(openib_btl->qps[qp].u.srq_qp.srq, &srq_attr, IBV_SRQ_LIMIT)) {
                BTL_ERROR(("Failed to request limit event for srq on  %s.  "
                   "Fatal error, stoping asynch event thread",
                   ibv_get_device_name(openib_btl->device->ib_dev)));

                OPAL_THREAD_UNLOCK(&openib_btl->ib_lock);
                return OMPI_ERROR;
            }
        }

        OPAL_THREAD_UNLOCK(&openib_btl->ib_lock);
        return OMPI_SUCCESS;
    }

    for(i = 0; wr_list && wr_list != bad_wr; i++, wr_list = wr_list->next);

    BTL_ERROR(("error posting receive descriptors to shared receive "
                "queue %d (%d from %d)", qp, i, num_post));

    OPAL_THREAD_UNLOCK(&openib_btl->ib_lock);
    return OMPI_ERROR;
}

