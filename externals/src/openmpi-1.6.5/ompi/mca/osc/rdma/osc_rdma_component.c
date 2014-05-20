/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2006-2008 University of Houston.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>

#include "osc_rdma.h"
#include "osc_rdma_sendreq.h"
#include "osc_rdma_replyreq.h"
#include "osc_rdma_header.h"
#include "osc_rdma_data_move.h"
#include "osc_rdma_obj_convert.h"

#include "opal/threads/condition.h"
#include "opal/threads/mutex.h"
#include "opal/util/arch.h"
#include "opal/align.h"

#include "ompi/info/info.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/osc/osc.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/osc/base/osc_base_obj_convert.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/bml/bml.h"
#include "ompi/mca/bml/base/base.h"

static int component_open(void);
static void component_fragment_cb(struct mca_btl_base_module_t *btl,
                                  mca_btl_base_tag_t tag,
                                  mca_btl_base_descriptor_t *descriptor,
                                  void *cbdata);
#if OPAL_ENABLE_PROGRESS_THREADS
static void* component_thread_fn(opal_object_t *obj);
#endif
static int setup_rdma(ompi_osc_rdma_module_t *module);

ompi_osc_rdma_component_t mca_osc_rdma_component = {
    { /* ompi_osc_base_component_t */
        { /* ompi_base_component_t */
            OMPI_OSC_BASE_VERSION_2_0_0,
            "rdma",
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            component_open,
            NULL
        },
        { /* mca_base_component_data */
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },
        ompi_osc_rdma_component_init,
        ompi_osc_rdma_component_query,
        ompi_osc_rdma_component_select,
        ompi_osc_rdma_component_finalize
    }
};


ompi_osc_rdma_module_t ompi_osc_rdma_module_template = {
    {
        ompi_osc_rdma_module_free,

        ompi_osc_rdma_module_put,
        ompi_osc_rdma_module_get,
        ompi_osc_rdma_module_accumulate,

        ompi_osc_rdma_module_fence,

        ompi_osc_rdma_module_start,
        ompi_osc_rdma_module_complete,
        ompi_osc_rdma_module_post,
        ompi_osc_rdma_module_wait,
        ompi_osc_rdma_module_test,

        ompi_osc_rdma_module_lock,
        ompi_osc_rdma_module_unlock,
    }
};


/* look up parameters for configuring this window.  The code first
   looks in the info structure passed by the user, then through mca
   parameters. */
static bool
check_config_value_bool(char *key, ompi_info_t *info)
{
    char *value_string;
    int value_len, ret, flag, param;
    bool result;

    ret = ompi_info_get_valuelen(info, key, &value_len, &flag);
    if (OMPI_SUCCESS != ret) goto info_not_found;
    if (flag == 0) goto info_not_found;
    value_len++;

    value_string = (char*)malloc(sizeof(char) * value_len + 1); /* Should malloc 1 char for NUL-termination */
    if (NULL == value_string) goto info_not_found;

    ret = ompi_info_get(info, key, value_len, value_string, &flag);
    if (OMPI_SUCCESS != ret) {
        free(value_string);
        goto info_not_found;
    }
    assert(flag != 0);
    ret = ompi_info_value_to_bool(value_string, &result);
    free(value_string);
    if (OMPI_SUCCESS != ret) goto info_not_found;
    return result;

 info_not_found:
    param = mca_base_param_find("osc", "rdma", key);
    if (param == OPAL_ERROR) return false;

    ret = mca_base_param_lookup_int(param, &flag);
    if (OMPI_SUCCESS != ret) return false;

    return OPAL_INT_TO_BOOL(flag);
}


static int
component_open(void)
{
    mca_base_param_reg_int(&mca_osc_rdma_component.super.osc_version,
                           "eager_send",
                           "Attempt to start data movement during communication "
                           "call, instead of at synchrnoization time.  "
                           "Info key of same name overrides this value.",
                           false, false, 1, NULL);

     mca_base_param_reg_int(&mca_osc_rdma_component.super.osc_version,
                            "use_buffers",
                            "Coalesce messages during an epoch to reduce "
                            "network utilization.  Info key of same name "
                            "overrides this value.",
                            false, false, 1, NULL);

    mca_base_param_reg_int(&mca_osc_rdma_component.super.osc_version,
                           "use_rdma",
                           "Use real RDMA operations to transfer data.  "
                           "Info key of same name overrides this value.",
                           false, false, 0, NULL);

     mca_base_param_reg_int(&mca_osc_rdma_component.super.osc_version,
                            "rdma_completion_wait",
                            "Wait for all completion of rdma events before "
                            "sending acknowledgment.  Info key of same name "
                            "overrides this value.",
                            false, false, 1, NULL);

    mca_base_param_reg_int(&mca_osc_rdma_component.super.osc_version,
                           "no_locks",
                           "Enable optimizations available only if MPI_LOCK is "
                           "not used.  "
                           "Info key of same name overrides this value.",
                           false, false, 0, NULL);

    return OMPI_SUCCESS;
}


int
ompi_osc_rdma_component_init(bool enable_progress_threads,
                             bool enable_mpi_threads)
{
    if (!mca_bml_base_inited()) return OMPI_ERROR;

    /* we can run with either threads or not threads (may not be able
       to do win locks)... */
    mca_osc_rdma_component.c_have_progress_threads = 
        enable_progress_threads;

    OBJ_CONSTRUCT(&mca_osc_rdma_component.c_lock, opal_mutex_t);

    OBJ_CONSTRUCT(&mca_osc_rdma_component.c_modules,
                  opal_hash_table_t);
    opal_hash_table_init(&mca_osc_rdma_component.c_modules, 2);

    OBJ_CONSTRUCT(&mca_osc_rdma_component.c_request_lock, 
                  opal_mutex_t);
    OBJ_CONSTRUCT(&mca_osc_rdma_component.c_request_cond, 
                  opal_condition_t);

    OBJ_CONSTRUCT(&mca_osc_rdma_component.c_sendreqs, opal_free_list_t);
    opal_free_list_init(&mca_osc_rdma_component.c_sendreqs,
                        sizeof(ompi_osc_rdma_sendreq_t),
                        OBJ_CLASS(ompi_osc_rdma_sendreq_t),
                        1, -1, 1);

    OBJ_CONSTRUCT(&mca_osc_rdma_component.c_replyreqs, opal_free_list_t);
    opal_free_list_init(&mca_osc_rdma_component.c_replyreqs,
                        sizeof(ompi_osc_rdma_replyreq_t),
                        OBJ_CLASS(ompi_osc_rdma_replyreq_t),
                        1, -1, 1);

    OBJ_CONSTRUCT(&mca_osc_rdma_component.c_longreqs, opal_free_list_t);
    opal_free_list_init(&mca_osc_rdma_component.c_longreqs,
                        sizeof(ompi_osc_rdma_longreq_t),
                        OBJ_CLASS(ompi_osc_rdma_longreq_t),
                        1, -1, 1);

    OBJ_CONSTRUCT(&mca_osc_rdma_component.c_pending_requests,
                  opal_list_t);

#if OPAL_ENABLE_PROGRESS_THREADS
    OBJ_CONSTRUCT(&mca_osc_rdma_component.c_thread, opal_thread_t);
    mca_osc_rdma_component.c_thread_run = false;
#endif

    mca_osc_rdma_component.c_btl_registered = false;

    mca_osc_rdma_component.c_sequence_number = 0;

    return OMPI_SUCCESS;
}


int 
ompi_osc_rdma_component_finalize(void)
{
    size_t num_modules;

    if (0 != 
        (num_modules = opal_hash_table_get_size(&mca_osc_rdma_component.c_modules))) {
        opal_output(ompi_osc_base_output,
                    "WARNING: There were %d Windows created but not freed.",
                    (int) num_modules);
#if OPAL_ENABLE_PROGRESS_THREADS
        mca_osc_rdma_component.c_thread_run = false;
        opal_condition_broadcast(&ompi_request_cond);
        {
            void* ret;
            opal_thread_join(&mca_osc_rdma_component.c_thread, &ret);
        }
#else
        opal_progress_unregister(ompi_osc_rdma_component_progress);
#endif
    }

    mca_bml.bml_register(MCA_BTL_TAG_OSC_RDMA, NULL, NULL);

#if OPAL_ENABLE_PROGRESS_THREADS
    OBJ_DESTRUCT(&mca_osc_rdma_component.c_thread);
#endif
    OBJ_DESTRUCT(&mca_osc_rdma_component.c_pending_requests);
    OBJ_DESTRUCT(&mca_osc_rdma_component.c_longreqs);
    OBJ_DESTRUCT(&mca_osc_rdma_component.c_replyreqs);
    OBJ_DESTRUCT(&mca_osc_rdma_component.c_sendreqs);
    OBJ_DESTRUCT(&mca_osc_rdma_component.c_request_cond);
    OBJ_DESTRUCT(&mca_osc_rdma_component.c_request_lock);
    OBJ_DESTRUCT(&mca_osc_rdma_component.c_modules);
    OBJ_DESTRUCT(&mca_osc_rdma_component.c_lock);

    return OMPI_SUCCESS;
}


int
ompi_osc_rdma_component_query(ompi_win_t *win,
                              ompi_info_t *info,
                              ompi_communicator_t *comm)
{
    /* if we inited, then the BMLs are available and we have a path to
       each peer.  Return slightly higher priority than the
       point-to-point code */
    return 10;
}


int 
ompi_osc_rdma_component_select(ompi_win_t *win,
                               ompi_info_t *info,
                               ompi_communicator_t *comm)
{
    ompi_osc_rdma_module_t *module = NULL;
    int ret, i;
    char *tmp;

    /* create module structure */
    module = (ompi_osc_rdma_module_t*)
        calloc(1, sizeof(ompi_osc_rdma_module_t));
    if (NULL == module) return OMPI_ERR_TEMP_OUT_OF_RESOURCE;

    /* fill in the function pointer part */
    memcpy(module, &ompi_osc_rdma_module_template, 
           sizeof(ompi_osc_base_module_t));

    /* initialize the module part */
    OBJ_CONSTRUCT(&module->m_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&module->m_cond, opal_condition_t);
    OBJ_CONSTRUCT(&module->m_acc_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&module->m_pending_sendreqs, opal_list_t);
    OBJ_CONSTRUCT(&module->m_copy_pending_sendreqs, opal_list_t);
    OBJ_CONSTRUCT(&module->m_queued_sendreqs, opal_list_t);
    OBJ_CONSTRUCT(&module->m_locks_pending, opal_list_t);
    OBJ_CONSTRUCT(&module->m_unlocks_pending, opal_list_t);

    module->m_win = win;

    OPAL_THREAD_LOCK(&mca_osc_rdma_component.c_lock);
    module->m_sequence_number = (mca_osc_rdma_component.c_sequence_number++);
    OPAL_THREAD_UNLOCK(&mca_osc_rdma_component.c_lock);

    ret = ompi_comm_dup(comm, &module->m_comm);
    if (ret != OMPI_SUCCESS) goto cleanup;

    opal_output_verbose(1, ompi_osc_base_output,
                        "rdma component creating window with id %d",
                        ompi_comm_get_cid(module->m_comm));

    asprintf(&tmp, "%d", ompi_comm_get_cid(module->m_comm));
    ompi_win_set_name(win, tmp);
    free(tmp);

    module->m_num_pending_sendreqs = (unsigned int*)
        malloc(sizeof(unsigned int) * ompi_comm_size(module->m_comm));
    if (NULL == module->m_num_pending_sendreqs) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }
    memset(module->m_num_pending_sendreqs, 0, 
           sizeof(unsigned int) * ompi_comm_size(module->m_comm));

    module->m_num_pending_out = 0;
    module->m_num_pending_in = 0;
    module->m_num_post_msgs = 0;
    module->m_num_complete_msgs = 0;
    module->m_tag_counter = 0;

    module->m_copy_num_pending_sendreqs = (unsigned int*)
        malloc(sizeof(unsigned int) * ompi_comm_size(module->m_comm));
    if (NULL == module->m_copy_num_pending_sendreqs) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }
    memset(module->m_num_pending_sendreqs, 0, 
           sizeof(unsigned int) * ompi_comm_size(module->m_comm));

    module->m_eager_send_ok = check_config_value_bool("eager_send", info);
    /* initially, we're in that pseudo-fence state, so we allow eager
       sends (yay for Fence).  Other protocols will disable before
       they start their epochs, so this isn't a problem. */
    module->m_eager_send_active = module->m_eager_send_ok;

    /* allocate space for rdma information */
    module->m_use_rdma = check_config_value_bool("use_rdma", info);
    module->m_rdma_wait_completion = check_config_value_bool("rdma_completion_wait", info);
    module->m_setup_info = NULL;
    module->m_peer_info = NULL;

    /* buffer setup */
    module->m_use_buffers = check_config_value_bool("use_buffers", info);
    module->m_pending_buffers = (ompi_osc_rdma_buffer_t *) malloc(sizeof(ompi_osc_rdma_buffer_t) *
                                       ompi_comm_size(module->m_comm));
    memset(module->m_pending_buffers, 0, 
           sizeof(ompi_osc_rdma_buffer_t) * ompi_comm_size(module->m_comm));

    /* fence data */
    module->m_fence_coll_counts = (int*)
        malloc(sizeof(int) * ompi_comm_size(module->m_comm));
    if (NULL == module->m_fence_coll_counts) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }
    for (i = 0 ; i < ompi_comm_size(module->m_comm) ; ++i) {
        module->m_fence_coll_counts[i] = 1;
    }

    /* pwsc data */
    module->m_pw_group = NULL;
    module->m_sc_group = NULL;
    module->m_sc_remote_active_ranks = (bool*)
        malloc(sizeof(bool) * ompi_comm_size(module->m_comm));
    if (NULL == module->m_sc_remote_active_ranks) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }
    module->m_sc_remote_ranks = (int*)
        malloc(sizeof(int) * ompi_comm_size(module->m_comm));
    if (NULL == module->m_sc_remote_ranks) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /* lock data */
    module->m_lock_status = 0;
    module->m_shared_count = 0;
    module->m_lock_received_ack = 0;

    /* update component data */
    OPAL_THREAD_LOCK(&mca_osc_rdma_component.c_lock);
    opal_hash_table_set_value_uint32(&mca_osc_rdma_component.c_modules,
                                     ompi_comm_get_cid(module->m_comm),
                                     module);
    ret = opal_hash_table_get_size(&mca_osc_rdma_component.c_modules);
    if (ret == 1) {
#if OPAL_ENABLE_PROGRESS_THREADS
        mca_osc_rdma_component.c_thread_run = true;
        mca_osc_rdma_component.c_thread.t_run = component_thread_fn;
        mca_osc_rdma_component.c_thread.t_arg = NULL;
        ret = opal_thread_start(&mca_osc_rdma_component.c_thread);
#else
        ret = opal_progress_register(ompi_osc_rdma_component_progress);
#endif
    } else {
        ret = OMPI_SUCCESS;
    }
    OPAL_THREAD_UNLOCK(&mca_osc_rdma_component.c_lock);
    if (OMPI_SUCCESS != ret) goto cleanup;

    /* fill in window information */
    win->w_osc_module = (ompi_osc_base_module_t*) module;
    if (check_config_value_bool("no_locks", info)) {
        win->w_flags |= OMPI_WIN_NO_LOCKS;
    }

    /* register to receive fragment callbacks, if not already done */
    OPAL_THREAD_LOCK(&mca_osc_rdma_component.c_lock);
    if (!mca_osc_rdma_component.c_btl_registered) {
        mca_osc_rdma_component.c_btl_registered = true;
        ret = mca_bml.bml_register(MCA_BTL_TAG_OSC_RDMA,
                                   component_fragment_cb,
                                   NULL);
    }
    OPAL_THREAD_UNLOCK(&mca_osc_rdma_component.c_lock);
    if (OMPI_SUCCESS != ret) goto cleanup;

    /* sync memory - make sure all initialization completed */
    opal_atomic_mb();

    if (module->m_use_rdma) {
        /* fill in rdma information - involves barrier semantics */
        ret = setup_rdma(module);
    } else {
        /* barrier to prevent arrival of lock requests before we're
           fully created */
        ret = module->m_comm->c_coll.coll_barrier(module->m_comm,
                                                  module->m_comm->c_coll.coll_barrier_module);
    }
    if (OMPI_SUCCESS != ret) goto cleanup;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_output,
                         "done creating window %d", ompi_comm_get_cid(module->m_comm)));

    return OMPI_SUCCESS;

 cleanup:
    OBJ_DESTRUCT(&module->m_unlocks_pending);
    OBJ_DESTRUCT(&module->m_locks_pending);
    OBJ_DESTRUCT(&module->m_queued_sendreqs);
    OBJ_DESTRUCT(&module->m_copy_pending_sendreqs);
    OBJ_DESTRUCT(&module->m_pending_sendreqs);
    OBJ_DESTRUCT(&module->m_acc_lock);
    OBJ_DESTRUCT(&module->m_cond);
    OBJ_DESTRUCT(&module->m_lock);

    if (NULL != module->m_sc_remote_ranks) {
        free(module->m_sc_remote_ranks);
    }
    if (NULL != module->m_sc_remote_active_ranks) {
        free(module->m_sc_remote_active_ranks);
    }
    if (NULL != module->m_fence_coll_counts) {
        free(module->m_fence_coll_counts);
    }
    if (NULL != module->m_copy_num_pending_sendreqs) {
        free(module->m_copy_num_pending_sendreqs);
    }
    if (NULL != module->m_num_pending_sendreqs) {
        free(module->m_num_pending_sendreqs);
    }
    if (NULL != module->m_peer_info) {
        for (i = 0 ; i < ompi_comm_size(module->m_comm) ; ++i) {
            ompi_osc_rdma_peer_info_free(&module->m_peer_info[i]);
        }
        free(module->m_peer_info);
    }
    if (NULL != module->m_comm) ompi_comm_free(&module->m_comm);
    if (NULL != module) free(module);

    return ret;
}


/* dispatch for callback on message completion */
static void
component_fragment_cb(struct mca_btl_base_module_t *btl,
                      mca_btl_base_tag_t tag,
                      mca_btl_base_descriptor_t *descriptor,
                      void *cbdata)
{
    int ret;
    ompi_osc_rdma_module_t *module;
    void *payload;
    bool done = false;
    ompi_osc_rdma_base_header_t *base_header = 
        (ompi_osc_rdma_base_header_t*) descriptor->des_dst[0].seg_addr.pval;

    assert(descriptor->des_dst[0].seg_len >= 
           sizeof(ompi_osc_rdma_base_header_t));

    /* handle message */
    while (!done) {
        switch (base_header->hdr_type) {
        case OMPI_OSC_RDMA_HDR_PUT:
            {
                ompi_osc_rdma_send_header_t *header;

                /* get our header and payload */
                header = (ompi_osc_rdma_send_header_t*) base_header;
                payload = (void*) (header + 1);

#if !defined(WORDS_BIGENDIAN) && OPAL_ENABLE_HETEROGENEOUS_SUPPORT
                if (header->hdr_base.hdr_flags & OMPI_OSC_RDMA_HDR_FLAG_NBO) {
                    OMPI_OSC_RDMA_SEND_HDR_NTOH(*header);
                }
#endif

                /* get our module pointer */
                module = ompi_osc_rdma_windx_to_module(header->hdr_windx);
                if (NULL == module) return;

                if (!ompi_win_exposure_epoch(module->m_win)) {
                    if (OMPI_WIN_FENCE & ompi_win_get_mode(module->m_win)) {
                        /* well, we're definitely in an access epoch now */
                        ompi_win_set_mode(module->m_win, 
                                          OMPI_WIN_FENCE | 
                                          OMPI_WIN_ACCESS_EPOCH |
                                          OMPI_WIN_EXPOSE_EPOCH);
                    }
                }

                ret = ompi_osc_rdma_sendreq_recv_put(module, header, &payload);
            }
            break;

        case OMPI_OSC_RDMA_HDR_ACC: 
            {
                ompi_osc_rdma_send_header_t *header;

                /* get our header and payload */
                header = (ompi_osc_rdma_send_header_t*) base_header;
                payload = (void*) (header + 1);

#if !defined(WORDS_BIGENDIAN) && OPAL_ENABLE_HETEROGENEOUS_SUPPORT
                if (header->hdr_base.hdr_flags & OMPI_OSC_RDMA_HDR_FLAG_NBO) {
                    OMPI_OSC_RDMA_SEND_HDR_NTOH(*header);
                }
#endif

                /* get our module pointer */
                module = ompi_osc_rdma_windx_to_module(header->hdr_windx);
                if (NULL == module) return;

                if (!ompi_win_exposure_epoch(module->m_win)) {
                    if (OMPI_WIN_FENCE & ompi_win_get_mode(module->m_win)) {
                        /* well, we're definitely in an access epoch now */
                        ompi_win_set_mode(module->m_win, 
                                          OMPI_WIN_FENCE | 
                                          OMPI_WIN_ACCESS_EPOCH |
                                          OMPI_WIN_EXPOSE_EPOCH);
                    }
                }

                /* receive into temporary buffer */
                ret = ompi_osc_rdma_sendreq_recv_accum(module, header, &payload);
            }
            break;

        case OMPI_OSC_RDMA_HDR_GET:
            {
                ompi_datatype_t *datatype;
                ompi_osc_rdma_send_header_t *header;
                ompi_osc_rdma_replyreq_t *replyreq;
                ompi_proc_t *proc;

                /* get our header and payload */
                header = (ompi_osc_rdma_send_header_t*) base_header;
                payload = (void*) (header + 1);

#if !defined(WORDS_BIGENDIAN) && OPAL_ENABLE_HETEROGENEOUS_SUPPORT
                if (header->hdr_base.hdr_flags & OMPI_OSC_RDMA_HDR_FLAG_NBO) {
                    OMPI_OSC_RDMA_SEND_HDR_NTOH(*header);
                }
#endif

                /* get our module pointer */
                module = ompi_osc_rdma_windx_to_module(header->hdr_windx);
                if (NULL == module) return;

                if (!ompi_win_exposure_epoch(module->m_win)) {
                    if (OMPI_WIN_FENCE & ompi_win_get_mode(module->m_win)) {
                        /* well, we're definitely in an access epoch now */
                        ompi_win_set_mode(module->m_win, 
                                          OMPI_WIN_FENCE | 
                                          OMPI_WIN_ACCESS_EPOCH |
                                          OMPI_WIN_EXPOSE_EPOCH);
                    }
                }

                /* create or get a pointer to our datatype */
                proc = ompi_comm_peer_lookup( module->m_comm, header->hdr_origin );
                datatype = ompi_osc_base_datatype_create(proc, &payload);

                if (NULL == datatype) {
                    opal_output(ompi_osc_base_output,
                                "Error recreating datatype.  Aborting.");
                    ompi_mpi_abort(module->m_comm, 1, false);
                }

                /* create replyreq sendreq */
                ret = ompi_osc_rdma_replyreq_alloc_init(module,
                                                        header->hdr_origin,
                                                        header->hdr_origin_sendreq,
                                                        header->hdr_target_disp,
                                                        header->hdr_target_count,
                                                        datatype,
                                                        &replyreq);

                /* send replyreq */
                ompi_osc_rdma_replyreq_send(module, replyreq);

                /* sendreq does the right retain, so we can release safely */
                OBJ_RELEASE(datatype);
            }
            break;

        case OMPI_OSC_RDMA_HDR_REPLY:
            {
                ompi_osc_rdma_reply_header_t *header;
                ompi_osc_rdma_sendreq_t *sendreq;

                /* get our header and payload */
                header = (ompi_osc_rdma_reply_header_t*) base_header;
                payload = (void*) (header + 1);

#if !defined(WORDS_BIGENDIAN) && OPAL_ENABLE_HETEROGENEOUS_SUPPORT
                if (header->hdr_base.hdr_flags & OMPI_OSC_RDMA_HDR_FLAG_NBO) {
                    OMPI_OSC_RDMA_REPLY_HDR_NTOH(*header);
                }
#endif

                /* get original sendreq pointer */
                sendreq = (ompi_osc_rdma_sendreq_t*) header->hdr_origin_sendreq.pval;
                module = sendreq->req_module;

                /* receive data */
                ompi_osc_rdma_replyreq_recv(module, sendreq, header, &payload);
            }
            break;
        case OMPI_OSC_RDMA_HDR_POST:
            {
                ompi_osc_rdma_control_header_t *header = 
                    (ompi_osc_rdma_control_header_t*) base_header;
                int32_t count;
                payload = (void*) (header + 1);

#if !defined(WORDS_BIGENDIAN) && OPAL_ENABLE_HETEROGENEOUS_SUPPORT
                if (header->hdr_base.hdr_flags & OMPI_OSC_RDMA_HDR_FLAG_NBO) {
                    OMPI_OSC_RDMA_CONTROL_HDR_NTOH(*header);
                }
#endif

                /* get our module pointer */
                module = ompi_osc_rdma_windx_to_module(header->hdr_windx);
                if (NULL == module) return;

                OPAL_THREAD_LOCK(&module->m_lock);
                count = (module->m_num_post_msgs -= 1);
                OPAL_THREAD_UNLOCK(&module->m_lock);
                if (count == 0) {
                    module->m_eager_send_active = module->m_eager_send_ok;

                    while (module->m_eager_send_active && 
                           opal_list_get_size(&module->m_pending_sendreqs)) {
                        ompi_osc_rdma_sendreq_t *sendreq;

                        OPAL_THREAD_LOCK(&module->m_lock);
                        sendreq = (ompi_osc_rdma_sendreq_t*) 
                            opal_list_remove_first(&module->m_pending_sendreqs);

                        if (NULL == sendreq) {
                            OPAL_THREAD_UNLOCK(&module->m_lock);
                            break;
                        }

                        sendreq->req_module->m_num_pending_out += 1;
                        OPAL_THREAD_UNLOCK(&module->m_lock);

                        ret = ompi_osc_rdma_sendreq_send(module, sendreq);

                        if (OMPI_SUCCESS != ret) {
                            OPAL_THREAD_LOCK(&module->m_lock);
                            sendreq->req_module->m_num_pending_out -= 1;
                            opal_list_append(&(module->m_pending_sendreqs),
                                             (opal_list_item_t*) sendreq);
                            OPAL_THREAD_UNLOCK(&module->m_lock);
                            break;
                        }
                    }

                    opal_condition_broadcast(&module->m_cond);
                }
            }
            break;

        case OMPI_OSC_RDMA_HDR_COMPLETE:
            {
                ompi_osc_rdma_control_header_t *header = 
                    (ompi_osc_rdma_control_header_t*) base_header;
                int32_t count;
                payload = (void*) (header + 1);

#if !defined(WORDS_BIGENDIAN) && OPAL_ENABLE_HETEROGENEOUS_SUPPORT
                if (header->hdr_base.hdr_flags & OMPI_OSC_RDMA_HDR_FLAG_NBO) {
                    OMPI_OSC_RDMA_CONTROL_HDR_NTOH(*header);
                }
#endif

                /* get our module pointer */
                module = ompi_osc_rdma_windx_to_module(header->hdr_windx);
                if (NULL == module) return;

                /* we've heard from one more place, and have value reqs to
                   process */
                OPAL_THREAD_LOCK(&module->m_lock);
                count = (module->m_num_complete_msgs -= 1);
                count += (module->m_num_pending_in += header->hdr_value[0]);
                OPAL_THREAD_UNLOCK(&module->m_lock);

                if (count == 0) opal_condition_broadcast(&module->m_cond);
            }
            break;

        case OMPI_OSC_RDMA_HDR_LOCK_REQ:
            {
                ompi_osc_rdma_control_header_t *header = 
                    (ompi_osc_rdma_control_header_t*) base_header;
                int32_t count;
                payload = (void*) (header + 1);

#if !defined(WORDS_BIGENDIAN) && OPAL_ENABLE_HETEROGENEOUS_SUPPORT
                if (header->hdr_base.hdr_flags & OMPI_OSC_RDMA_HDR_FLAG_NBO) {
                    OMPI_OSC_RDMA_CONTROL_HDR_NTOH(*header);
                }
#endif

                /* get our module pointer */
                module = ompi_osc_rdma_windx_to_module(header->hdr_windx);
                if (NULL == module) return;

                if (header->hdr_value[1] > 0) {
                    ompi_osc_rdma_passive_lock(module, header->hdr_value[0], 
                                               header->hdr_value[1]);
                } else {
                    OPAL_THREAD_LOCK(&module->m_lock);
                    count = (module->m_lock_received_ack += 1);
                    OPAL_THREAD_UNLOCK(&module->m_lock);

                    if (count != 0) opal_condition_broadcast(&module->m_cond);
                }
            }
            break;

        case OMPI_OSC_RDMA_HDR_UNLOCK_REQ:
            {
                ompi_osc_rdma_control_header_t *header = 
                    (ompi_osc_rdma_control_header_t*) base_header;
                payload = (void*) (header + 1);

#if !defined(WORDS_BIGENDIAN) && OPAL_ENABLE_HETEROGENEOUS_SUPPORT
                if (header->hdr_base.hdr_flags & OMPI_OSC_RDMA_HDR_FLAG_NBO) {
                    OMPI_OSC_RDMA_CONTROL_HDR_NTOH(*header);
                }
#endif

                /* get our module pointer */
                module = ompi_osc_rdma_windx_to_module(header->hdr_windx);
                if (NULL == module) return;

                ompi_osc_rdma_passive_unlock(module, header->hdr_value[0],
                                             header->hdr_value[1]);
            }
            break;

        case OMPI_OSC_RDMA_HDR_UNLOCK_REPLY:
            {
                ompi_osc_rdma_control_header_t *header = 
                    (ompi_osc_rdma_control_header_t*) base_header;
                int32_t count;
                payload = (void*) (header + 1);

#if !defined(WORDS_BIGENDIAN) && OPAL_ENABLE_HETEROGENEOUS_SUPPORT
                if (header->hdr_base.hdr_flags & OMPI_OSC_RDMA_HDR_FLAG_NBO) {
                    OMPI_OSC_RDMA_CONTROL_HDR_NTOH(*header);
                }
#endif

                /* get our module pointer */
                module = ompi_osc_rdma_windx_to_module(header->hdr_windx);
                if (NULL == module) return;

                OPAL_THREAD_LOCK(&module->m_lock);
                count = (module->m_num_pending_out -= 1);
                OPAL_THREAD_UNLOCK(&module->m_lock);
                if (count == 0) opal_condition_broadcast(&module->m_cond);
            }
            break;

        case OMPI_OSC_RDMA_HDR_RDMA_COMPLETE:
            {
                ompi_osc_rdma_control_header_t *header = 
                    (ompi_osc_rdma_control_header_t*) base_header;
                int32_t count;
                payload = (void*) (header + 1);

#if !defined(WORDS_BIGENDIAN) && OPAL_ENABLE_HETEROGENEOUS_SUPPORT
                if (header->hdr_base.hdr_flags & OMPI_OSC_RDMA_HDR_FLAG_NBO) {
                    OMPI_OSC_RDMA_CONTROL_HDR_NTOH(*header);
                }
#endif

                /* get our module pointer */
                module = ompi_osc_rdma_windx_to_module(header->hdr_windx);
                if (NULL == module) return;

                OPAL_THREAD_LOCK(&module->m_lock);
                count = (module->m_num_pending_in -= header->hdr_value[0]);
                OPAL_THREAD_UNLOCK(&module->m_lock);
                if (count == 0) opal_condition_broadcast(&module->m_cond);
            }
            break;

        case OMPI_OSC_RDMA_HDR_RDMA_INFO:
            {
                ompi_osc_rdma_rdma_info_header_t *header = 
                    (ompi_osc_rdma_rdma_info_header_t*) base_header;
                ompi_proc_t *proc = NULL;
                mca_bml_base_endpoint_t *endpoint = NULL;
                mca_bml_base_btl_t *bml_btl;
                ompi_osc_rdma_btl_t *rdma_btl;
                int origin, index;
                payload = (void*) (header + 1);

#if !defined(WORDS_BIGENDIAN) && OPAL_ENABLE_HETEROGENEOUS_SUPPORT
                if (header->hdr_base.hdr_flags & OMPI_OSC_RDMA_HDR_FLAG_NBO) {
                    OMPI_OSC_RDMA_RDMA_INFO_HDR_NTOH(*header);
                }
#endif

                /* get our module pointer */
                module = ompi_osc_rdma_windx_to_module(header->hdr_windx);
                if (NULL == module) return;

                origin = header->hdr_origin;

                /* find the bml_btl */
                proc = ompi_comm_peer_lookup(module->m_comm, origin);
                endpoint = (mca_bml_base_endpoint_t*) proc->proc_bml;
                bml_btl = mca_bml_base_btl_array_find(&endpoint->btl_rdma, btl);
                if (NULL == bml_btl) {
                    opal_output(ompi_osc_base_output,
                                "received rdma info for unknown btl from rank %d",
                                origin);
                    return;
                } else {
                    OPAL_OUTPUT_VERBOSE((1, ompi_osc_base_output,
                                         "received rdma info from rank %d for BTL %s",
                                         origin,
                                         bml_btl->btl->
                                         btl_component->btl_version.
                                         mca_component_name));
                }

                OPAL_THREAD_LOCK(&module->m_lock);
                index = module->m_peer_info[origin].peer_num_btls++;
                rdma_btl = &(module->m_peer_info[origin].peer_btls[index]);

                rdma_btl->peer_seg_key = header->hdr_segkey;
                rdma_btl->bml_btl = bml_btl;
                rdma_btl->rdma_order = MCA_BTL_NO_ORDER;
                rdma_btl->num_sent = 0;

                module->m_setup_info->num_btls_callin++;
                OPAL_THREAD_UNLOCK(&module->m_lock);
            
                opal_condition_broadcast(&module->m_cond);
            }
            break;

        case OMPI_OSC_RDMA_HDR_MULTI_END:
            payload = base_header;
            done = true;
            break;

        default:
            /* BWB - FIX ME - this sucks */
            opal_output(ompi_osc_base_output,
                        "received packet for Window with unknown type");
        }

        if ((base_header->hdr_flags & OMPI_OSC_RDMA_HDR_FLAG_MULTI) != 0) {
            /* The next header starts at the next aligned address in
             * the buffer.  Therefore, check the hdr_flags to see if
             * any extra alignment is necessary, and if so, pull value
             * from the flags. */
            if (base_header->hdr_flags & OMPI_OSC_RDMA_HDR_FLAG_ALIGN_MASK) {
                payload = (char *)payload + (base_header->hdr_flags & OMPI_OSC_RDMA_HDR_FLAG_ALIGN_MASK);
            }
            base_header = (ompi_osc_rdma_base_header_t*) payload;
        } else {
            done = true;
        }
    }
}

int
ompi_osc_rdma_component_progress(void)
{
    opal_list_item_t *item;
    int ret, done = 0;

#if OPAL_ENABLE_PROGRESS_THREADS
    OPAL_THREAD_LOCK(&mca_osc_rdma_component.c_lock);
#else
    ret = OPAL_THREAD_TRYLOCK(&mca_osc_rdma_component.c_lock);
    if (ret != 0) return 0;
#endif

    for (item = opal_list_get_first(&mca_osc_rdma_component.c_pending_requests) ;
         item != opal_list_get_end(&mca_osc_rdma_component.c_pending_requests) ;
         item = opal_list_get_next(item)) {
        ompi_osc_rdma_longreq_t *longreq = 
            (ompi_osc_rdma_longreq_t*) item;

        /* BWB - FIX ME */
#if OPAL_ENABLE_PROGRESS_THREADS == 0
        if (longreq->request->req_state == OMPI_REQUEST_INACTIVE ||
            longreq->request->req_complete) {
            ret = ompi_request_test(&longreq->request,
                                    &done,
                                    0);
        } else {
            done = 0;
            ret = OMPI_SUCCESS;
        }
#else
        ret = ompi_request_test(&longreq->request,
                                &done,
                                0);
#endif
        if (OMPI_SUCCESS == ret && 0 != done) {
            opal_list_remove_item(&mca_osc_rdma_component.c_pending_requests,
                                  item);
            OPAL_THREAD_UNLOCK(&mca_osc_rdma_component.c_lock);
            longreq->cbfunc(longreq);
            OPAL_THREAD_LOCK(&mca_osc_rdma_component.c_lock);
            break;
        }
    }
        
    OPAL_THREAD_UNLOCK(&mca_osc_rdma_component.c_lock);

    return done;
}


#if OPAL_ENABLE_PROGRESS_THREADS
static void*
component_thread_fn(opal_object_t *obj)
{
    struct timespec waittime;

    while (mca_osc_rdma_component.c_thread_run) {
        /* wake up whenever a request completes, to make sure it's not
           for us */
        waittime.tv_sec = 1;
        waittime.tv_nsec = 0;
        OPAL_THREAD_LOCK(&ompi_request_lock);
        opal_condition_timedwait(&ompi_request_cond, &ompi_request_lock, &waittime);
        OPAL_THREAD_UNLOCK(&ompi_request_lock);
        ompi_osc_rdma_component_progress();
    }

    return NULL;
}
#endif


/*********** RDMA setup stuff ***********/


struct peer_rdma_send_info_t{
    opal_list_item_t super;
    ompi_osc_rdma_module_t *module;
    ompi_proc_t *proc;
    mca_bml_base_btl_t *bml_btl;
    uint64_t seg_key;
};
typedef struct peer_rdma_send_info_t peer_rdma_send_info_t;
OBJ_CLASS_INSTANCE(peer_rdma_send_info_t, opal_list_item_t, NULL, NULL);


static void
rdma_send_info_send_complete(struct mca_btl_base_module_t* btl, 
                             struct mca_btl_base_endpoint_t *endpoint,
                             struct mca_btl_base_descriptor_t* descriptor,
                             int status)
{
    peer_rdma_send_info_t *peer_send_info = 
        (peer_rdma_send_info_t*) descriptor->des_cbdata;

    if (OMPI_SUCCESS == status) {
        btl->btl_free(btl, descriptor);

        OPAL_THREAD_LOCK(&peer_send_info->module->m_lock);
        peer_send_info->module->m_setup_info->num_btls_outgoing--;
        OPAL_THREAD_UNLOCK(&peer_send_info->module->m_lock);

        opal_condition_broadcast(&(peer_send_info->module->m_cond));

        OBJ_RELEASE(peer_send_info);
    } else {
        /* BWB - fix me */
        abort();
    }
}

static int
rdma_send_info_send(ompi_osc_rdma_module_t *module,
                    peer_rdma_send_info_t *peer_send_info)
{
    int ret = OMPI_SUCCESS;
    mca_bml_base_btl_t *bml_btl = NULL;
    mca_btl_base_descriptor_t *descriptor = NULL;
    ompi_osc_rdma_rdma_info_header_t *header = NULL;
        
    bml_btl = peer_send_info->bml_btl;
    mca_bml_base_alloc(bml_btl, &descriptor, MCA_BTL_NO_ORDER,
                       sizeof(ompi_osc_rdma_rdma_info_header_t),
                       MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_SEND_ALWAYS_CALLBACK);
    if (NULL == descriptor) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /* verify at least enough space for header */
    if (descriptor->des_src[0].seg_len < sizeof(ompi_osc_rdma_rdma_info_header_t)) {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /* setup descriptor */
    descriptor->des_cbfunc = rdma_send_info_send_complete;
    descriptor->des_cbdata = peer_send_info;
    descriptor->des_src[0].seg_len = sizeof(ompi_osc_rdma_rdma_info_header_t);

    /* pack header */
    header = (ompi_osc_rdma_rdma_info_header_t*) descriptor->des_src[0].seg_addr.pval;
    header->hdr_base.hdr_type = OMPI_OSC_RDMA_HDR_RDMA_INFO;
    header->hdr_base.hdr_flags = 0;
    header->hdr_segkey = peer_send_info->seg_key;
    header->hdr_origin = ompi_comm_rank(module->m_comm);
    header->hdr_windx = ompi_comm_get_cid(module->m_comm);

#ifdef WORDS_BIGENDIAN
    header->hdr_base.hdr_flags |= OMPI_OSC_RDMA_HDR_FLAG_NBO;
#elif OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    if (peer_send_info->proc->proc_arch & OPAL_ARCH_ISBIGENDIAN) {
        header->hdr_base.hdr_flags |= OMPI_OSC_RDMA_HDR_FLAG_NBO;
        OMPI_OSC_RDMA_RDMA_INFO_HDR_HTON(*header);
    }
#endif

    /* send fragment */
    ret = mca_bml_base_send(bml_btl, descriptor, MCA_BTL_TAG_OSC_RDMA);
    if (1 == ret) ret = OMPI_SUCCESS;
    goto done;

 cleanup:
    if (descriptor != NULL) {
        mca_bml_base_free(bml_btl, descriptor);
    }

 done:
    return ret;
}


static bool
is_valid_rdma(mca_bml_base_btl_t *bml_btl)
{
    if ((bml_btl->btl->btl_put != NULL) &&
        (bml_btl->btl->btl_get != NULL) &&
        ((bml_btl->btl_flags & MCA_BTL_FLAGS_RDMA_MATCHED) == 0)) {
        return true;
    }

    return false;
}


static int
setup_rdma(ompi_osc_rdma_module_t *module)
{

    uint64_t local;
    uint64_t *remote = NULL;
    MPI_Datatype ui64_type;
    int ret = OMPI_SUCCESS;
    int i;
    
#if SIZEOF_LONG == 8
    ui64_type = MPI_LONG;
#else
    ui64_type = MPI_LONG_LONG;
#endif

    /* create a setup info structure */
    module->m_setup_info = (ompi_osc_rdma_setup_info_t *) malloc(sizeof(ompi_osc_rdma_setup_info_t));
    if (NULL == module->m_setup_info) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }
    module->m_setup_info->num_btls_callin = 0;
    module->m_setup_info->num_btls_expected = -1;
    module->m_setup_info->num_btls_outgoing = 0;
    module->m_setup_info->outstanding_btl_requests = 
        (opal_list_t *) malloc(sizeof(opal_list_t) * ompi_comm_size(module->m_comm));
    if (NULL == module->m_setup_info->outstanding_btl_requests) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }
    for (i = 0 ; i < ompi_comm_size(module->m_comm) ; ++i) {
        OBJ_CONSTRUCT(&(module->m_setup_info->outstanding_btl_requests[i]),
                      opal_list_t);
    }
    
    /* create peer info array */
    module->m_peer_info = (ompi_osc_rdma_peer_info_t*)
        malloc(sizeof(ompi_osc_rdma_peer_info_t) * 
               ompi_comm_size(module->m_comm));
    if (NULL == module->m_peer_info) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }
    memset(module->m_peer_info, 0, 
           sizeof(ompi_osc_rdma_peer_info_t) * ompi_comm_size(module->m_comm));

    /* get number of btls to each peer, descriptors for the window for
       each peer */
    for (i = 0 ; i < ompi_comm_size(module->m_comm) ; ++i) {
        ompi_proc_t *proc = ompi_comm_peer_lookup(module->m_comm, i);
        ompi_osc_rdma_peer_info_t *peer_info = &module->m_peer_info[i];
        mca_bml_base_endpoint_t *endpoint = 
            (mca_bml_base_endpoint_t*) proc->proc_bml;
        int num_avail =
            mca_bml_base_btl_array_get_size(&endpoint->btl_rdma);
        size_t j, size;
        opal_convertor_t convertor;
        
        /* skip peer if heterogeneous */
        if (ompi_proc_local()->proc_arch != proc->proc_arch) {
            continue;
        }

        /* get a rough estimation of how many BTLs we'll be able to
           use, and exit if the answer is none */
        for (j = 0 ; 
             j < mca_bml_base_btl_array_get_size(&endpoint->btl_rdma) ;
             ++j) {
            mca_bml_base_btl_t *bml_btl =
                mca_bml_base_btl_array_get_index(&endpoint->btl_rdma, j);
            if (!is_valid_rdma(bml_btl)) num_avail--;
        }
        if (0 == num_avail) continue;

        /* Allocate space for all the useable BTLs.  They might not
           all end up useable, if we can't pin memory for the btl or
           the like.  But the number of elements to start with should
           be small and the number that fail the pin test should be
           approximately 0, so this isn't too big of a waste */
        peer_info->peer_btls = (ompi_osc_rdma_btl_t*)
            malloc(sizeof(ompi_osc_rdma_btl_t) * num_avail);
        peer_info->local_btls = (mca_bml_base_btl_t**)
            malloc(sizeof(mca_bml_base_btl_t*) * num_avail);
        peer_info->local_registrations = (mca_mpool_base_registration_t**)
            malloc(sizeof(mca_mpool_base_registration_t*) * num_avail);
        peer_info->local_descriptors = (mca_btl_base_descriptor_t**)
            malloc(sizeof(mca_btl_base_descriptor_t*) * num_avail);
        if (NULL == peer_info->peer_btls ||
            NULL == peer_info->local_btls ||
            NULL == peer_info->local_registrations ||
            NULL == peer_info->local_descriptors) {
            ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
            goto cleanup;
        }
        memset(peer_info->peer_btls, 0,
               sizeof(ompi_osc_rdma_btl_t) * num_avail);
        memset(peer_info->local_registrations, 0,
               sizeof(mca_mpool_base_registration_t*) * num_avail);
        memset(peer_info->local_descriptors, 0,
               sizeof(mca_btl_base_descriptor_t*) * num_avail);

        OBJ_CONSTRUCT(&convertor, opal_convertor_t);

        /* Find all useable btls, try to do the descriptor thing for
           them, and store all that information */
        for (j = 0 ;
             j < mca_bml_base_btl_array_get_size(&endpoint->btl_rdma) ;
             ++j) {
            mca_bml_base_btl_t *bml_btl =
                mca_bml_base_btl_array_get_index(&endpoint->btl_rdma, j);
            mca_mpool_base_module_t *btl_mpool = bml_btl->btl->btl_mpool;
            int index = peer_info->local_num_btls;

            if (!is_valid_rdma(bml_btl)) continue;

            if (NULL != btl_mpool) {
                ret = btl_mpool->mpool_register(btl_mpool, module->m_win->w_baseptr,
                                                module->m_win->w_size, 0,
                                                &(peer_info->local_registrations[index]));
                if (OMPI_SUCCESS != ret) continue;
            } else {
                peer_info->local_registrations[index] = NULL;
            }

            size = module->m_win->w_size;

            opal_convertor_copy_and_prepare_for_send(proc->proc_convertor,
                                                     &(ompi_mpi_byte.dt.super),
                                                     module->m_win->w_size,
                                                     module->m_win->w_baseptr,
                                                     0,
                                                     &convertor);

            mca_bml_base_prepare_dst(bml_btl,
                    peer_info->local_registrations[index],
                    &convertor, MCA_BTL_NO_ORDER, 0, &size, 0,
                    &peer_info->local_descriptors[index]);

            if (NULL == peer_info->local_descriptors[index]) {
                if (NULL != peer_info->local_registrations[index]) {
                    btl_mpool->mpool_deregister(btl_mpool,
                                                peer_info->local_registrations[index]);
                }
                opal_convertor_cleanup(&convertor);
                continue;
            }

            peer_info->local_btls[index] = bml_btl;

            opal_convertor_cleanup(&convertor);

            peer_info->local_num_btls++;
            module->m_setup_info->num_btls_outgoing++;
        }

        OBJ_DESTRUCT(&convertor);
    }

    /* fill in information about remote peers */
    remote = (uint64_t *) malloc(sizeof(uint64_t) * ompi_comm_size(module->m_comm));
    if (NULL == remote) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }

    local = ompi_ptr_ptol(module->m_win->w_baseptr);
    ret = module->m_comm->c_coll.coll_allgather(&local, 1, ui64_type,
                                                remote, 1, ui64_type,
                                                module->m_comm,
                                                module->m_comm->c_coll.coll_allgather_module);
    if (OMPI_SUCCESS != ret) goto cleanup;
    for (i = 0 ; i < ompi_comm_size(module->m_comm) ; ++i) {
        module->m_peer_info[i].peer_base = remote[i];
    }

    local = module->m_win->w_size;
    ret = module->m_comm->c_coll.coll_allgather(&local, 1, ui64_type,
                                                remote, 1, ui64_type,
                                                module->m_comm,
                                                module->m_comm->c_coll.coll_allgather_module);
    if (OMPI_SUCCESS != ret) goto cleanup;
    for (i = 0 ; i < ompi_comm_size(module->m_comm) ; ++i) {
        module->m_peer_info[i].peer_len = remote[i];
    }

    /* get number of btls we're expecting from everyone */
    for (i = 0 ; i < ompi_comm_size(module->m_comm) ; ++i) {
        remote[i] = module->m_peer_info[i].local_num_btls;
    }
    ret = module->m_comm->c_coll.coll_reduce_scatter(remote,
                                                     &local,
                                                     module->m_fence_coll_counts,
                                                     ui64_type,
                                                     MPI_SUM,
                                                     module->m_comm,
                                                     module->m_comm->c_coll.coll_reduce_scatter_module);
    if (OMPI_SUCCESS != ret) goto cleanup;
    module->m_setup_info->num_btls_expected = (int32_t)local;
    /* end fill in information about remote peers */

    /* send our contact info to everyone... */
    for (i = 0 ; i < ompi_comm_size(module->m_comm) ; ++i) {
        ompi_osc_rdma_peer_info_t *peer_info = &module->m_peer_info[i];
        int j;

        for (j = 0 ; j < peer_info->local_num_btls ; ++j) {
            peer_rdma_send_info_t *peer_send_info = 
                OBJ_NEW(peer_rdma_send_info_t);
            peer_send_info->module = module;
            peer_send_info->proc = ompi_comm_peer_lookup(module->m_comm, i);
            peer_send_info->bml_btl = peer_info->local_btls[j];
            peer_send_info->seg_key = 
                peer_info->local_descriptors[j]->des_dst[0].seg_key.key64;

            ret = rdma_send_info_send(module, peer_send_info);
            if (OMPI_SUCCESS != ret) {
                opal_list_append(&(module->m_setup_info->outstanding_btl_requests[i]),
                                 &peer_send_info->super);
            }
        }
    }

    OPAL_THREAD_LOCK(&module->m_lock);
    while ((module->m_setup_info->num_btls_outgoing != 0) ||
           (module->m_setup_info->num_btls_expected !=
            module->m_setup_info->num_btls_callin)) {
        for (i = 0 ; i < ompi_comm_size(module->m_comm) ; ++i) {
            peer_rdma_send_info_t *peer_send_info = 
                (peer_rdma_send_info_t*) opal_list_remove_first(&module->m_setup_info->outstanding_btl_requests[i]);
            if (NULL != peer_send_info) {
                ret = rdma_send_info_send(module, peer_send_info);
                if (OMPI_SUCCESS != ret) {
                    opal_list_append(&(module->m_setup_info->outstanding_btl_requests[i]),
                                     &peer_send_info->super);
                }
            }
        }
        opal_condition_wait(&module->m_cond, &module->m_lock);
    }
    OPAL_THREAD_UNLOCK(&module->m_lock);
 
    ret = OMPI_SUCCESS;

 cleanup:
    if (NULL != module->m_setup_info) {
        if (NULL != module->m_setup_info->outstanding_btl_requests) {
            for (i = 0 ; i < ompi_comm_size(module->m_comm) ; ++i) {
                OBJ_DESTRUCT(&(module->m_setup_info->outstanding_btl_requests[i]));
            }
            free(module->m_setup_info->outstanding_btl_requests);
        }
        free(module->m_setup_info);
    }
    if (NULL != remote) free(remote);

    return ret;
}

