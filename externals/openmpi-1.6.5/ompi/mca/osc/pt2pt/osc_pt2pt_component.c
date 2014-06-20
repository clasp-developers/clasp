/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2010 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2006-2008 University of Houston.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>

#include "osc_pt2pt.h"
#include "osc_pt2pt_sendreq.h"
#include "osc_pt2pt_replyreq.h"
#include "osc_pt2pt_header.h"
#include "osc_pt2pt_data_move.h"
#include "osc_pt2pt_buffer.h"

#include "opal/threads/mutex.h"

#include "ompi/info/info.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/osc/osc.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/osc/base/osc_base_obj_convert.h"
#include "ompi/mca/pml/pml.h"

static int component_open(void);
static void component_fragment_cb(ompi_osc_pt2pt_mpireq_t *mpireq);
#if OPAL_ENABLE_PROGRESS_THREADS
static void* component_thread_fn(opal_object_t *obj);
#endif

ompi_osc_pt2pt_component_t mca_osc_pt2pt_component = {
    { /* ompi_osc_base_component_t */
        { /* ompi_base_component_t */
            OMPI_OSC_BASE_VERSION_2_0_0,
            "pt2pt",
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            component_open,
            NULL
        },
        { /* mca_base_component_data */
            /* The component is checkpoint ready - JJH Double check this... */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
        ompi_osc_pt2pt_component_init,
        ompi_osc_pt2pt_component_query,
        ompi_osc_pt2pt_component_select,
        ompi_osc_pt2pt_component_finalize
    }
};


ompi_osc_pt2pt_module_t ompi_osc_pt2pt_module_template = {
    {
        ompi_osc_pt2pt_module_free,

        ompi_osc_pt2pt_module_put,
        ompi_osc_pt2pt_module_get,
        ompi_osc_pt2pt_module_accumulate,

        ompi_osc_pt2pt_module_fence,

        ompi_osc_pt2pt_module_start,
        ompi_osc_pt2pt_module_complete,
        ompi_osc_pt2pt_module_post,
        ompi_osc_pt2pt_module_wait,
        ompi_osc_pt2pt_module_test,

        ompi_osc_pt2pt_module_lock,
        ompi_osc_pt2pt_module_unlock,
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
    param = mca_base_param_find("osc", "pt2pt", key);
    if (param == OPAL_ERROR) return false;

    ret = mca_base_param_lookup_int(param, &flag);
    if (OMPI_SUCCESS != ret) return false;

    return OPAL_INT_TO_BOOL(flag);
}


static int
component_open(void)
{
    int tmp;

    mca_base_param_reg_int(&mca_osc_pt2pt_component.super.osc_version,
                           "no_locks",
                           "Enable optimizations available only if MPI_LOCK is not used.",
                           false, false, 0, NULL);

    mca_base_param_reg_int(&mca_osc_pt2pt_component.super.osc_version,
                           "eager_limit",
                           "Max size of eagerly sent data",
                           false, false, 16 * 1024, 
                           &tmp);

    mca_osc_pt2pt_component.p2p_c_eager_size = tmp;

    return OMPI_SUCCESS;
}


int
ompi_osc_pt2pt_component_init(bool enable_progress_threads,
                             bool enable_mpi_threads)
{
    size_t aligned_size;

    /* we can run with either threads or not threads (may not be able
       to do win locks)... */
    mca_osc_pt2pt_component.p2p_c_have_progress_threads = 
        enable_progress_threads;

    OBJ_CONSTRUCT(&mca_osc_pt2pt_component.p2p_c_lock, opal_mutex_t);

    OBJ_CONSTRUCT(&mca_osc_pt2pt_component.p2p_c_modules,
                  opal_hash_table_t);
    opal_hash_table_init(&mca_osc_pt2pt_component.p2p_c_modules, 2);

    OBJ_CONSTRUCT(&mca_osc_pt2pt_component.p2p_c_request_lock,
                  opal_mutex_t);
    OBJ_CONSTRUCT(&mca_osc_pt2pt_component.p2p_c_request_cond,
                  opal_condition_t);

    OBJ_CONSTRUCT(&mca_osc_pt2pt_component.p2p_c_sendreqs, opal_free_list_t);
    opal_free_list_init(&mca_osc_pt2pt_component.p2p_c_sendreqs,
                        sizeof(ompi_osc_pt2pt_sendreq_t),
                        OBJ_CLASS(ompi_osc_pt2pt_sendreq_t),
                        1, -1, 1);

    OBJ_CONSTRUCT(&mca_osc_pt2pt_component.p2p_c_replyreqs, opal_free_list_t);
    opal_free_list_init(&mca_osc_pt2pt_component.p2p_c_replyreqs,
                        sizeof(ompi_osc_pt2pt_replyreq_t),
                        OBJ_CLASS(ompi_osc_pt2pt_replyreq_t),
                        1, -1, 1);

    OBJ_CONSTRUCT(&mca_osc_pt2pt_component.p2p_c_longreqs, opal_free_list_t);
    opal_free_list_init(&mca_osc_pt2pt_component.p2p_c_longreqs,
                        sizeof(ompi_osc_pt2pt_longreq_t),
                        OBJ_CLASS(ompi_osc_pt2pt_longreq_t),
                        1, -1, 1);

    /* adjust size to be multiple of ompi_ptr_t to avoid alignment issues*/
    aligned_size = sizeof(ompi_osc_pt2pt_buffer_t) + 
        (sizeof(ompi_osc_pt2pt_buffer_t) % sizeof(ompi_ptr_t)) + 
        mca_osc_pt2pt_component.p2p_c_eager_size;
    OBJ_CONSTRUCT(&mca_osc_pt2pt_component.p2p_c_buffers, opal_free_list_t);
    opal_free_list_init(&mca_osc_pt2pt_component.p2p_c_buffers,
                        aligned_size,
                        OBJ_CLASS(ompi_osc_pt2pt_buffer_t),
                        1, -1, 1);

    OBJ_CONSTRUCT(&mca_osc_pt2pt_component.p2p_c_pending_requests,
                  opal_list_t);

#if OPAL_ENABLE_PROGRESS_THREADS
    OBJ_CONSTRUCT(&mca_osc_pt2pt_component.p2p_c_thread, opal_thread_t);
    mca_osc_pt2pt_component.p2p_c_thread_run = false;
#endif

    return OMPI_SUCCESS;
}


int 
ompi_osc_pt2pt_component_finalize(void)
{
    size_t num_modules;
#if OPAL_ENABLE_PROGRESS_THREADS
    void* ret;
#endif

    if (0 !=
        (num_modules = opal_hash_table_get_size(&mca_osc_pt2pt_component.p2p_c_modules))) {
        opal_output(ompi_osc_base_output,
                    "WARNING: There were %d Windows created but not freed.",
                    (int) num_modules); 
#if OPAL_ENABLE_PROGRESS_THREADS
        mca_osc_pt2pt_component.p2p_c_thread_run = false;
        opal_condition_broadcast(&ompi_request_cond);
        opal_thread_join(&mca_osc_pt2pt_component.p2p_c_thread, &ret);
#else
        opal_progress_unregister(ompi_osc_pt2pt_component_progress);
#endif
    }

    OBJ_DESTRUCT(&mca_osc_pt2pt_component.p2p_c_pending_requests);
    OBJ_DESTRUCT(&mca_osc_pt2pt_component.p2p_c_buffers);
    OBJ_DESTRUCT(&mca_osc_pt2pt_component.p2p_c_longreqs);
    OBJ_DESTRUCT(&mca_osc_pt2pt_component.p2p_c_replyreqs);
    OBJ_DESTRUCT(&mca_osc_pt2pt_component.p2p_c_sendreqs);
    OBJ_DESTRUCT(&mca_osc_pt2pt_component.p2p_c_request_lock);
    OBJ_DESTRUCT(&mca_osc_pt2pt_component.p2p_c_request_cond);

    OBJ_DESTRUCT(&mca_osc_pt2pt_component.p2p_c_modules);
    OBJ_DESTRUCT(&mca_osc_pt2pt_component.p2p_c_lock);

    return OMPI_SUCCESS;
}


int
ompi_osc_pt2pt_component_query(ompi_win_t *win,
                              ompi_info_t *info,
                              ompi_communicator_t *comm)
{
    /* we can always run - return a low priority */
    return 5;
}


int 
ompi_osc_pt2pt_component_select(ompi_win_t *win,
                               ompi_info_t *info,
                               ompi_communicator_t *comm)
{
    ompi_osc_pt2pt_module_t *module = NULL;
    int ret, i;
    ompi_osc_pt2pt_buffer_t *buffer = NULL;
    opal_free_list_item_t *item = NULL;
    char *tmp = NULL;

    /* create module structure */
    module = (ompi_osc_pt2pt_module_t*)
        calloc(1, sizeof(ompi_osc_pt2pt_module_t));
    if (NULL == module) return OMPI_ERR_TEMP_OUT_OF_RESOURCE;

    /* fill in the function pointer part */
    memcpy(module, &ompi_osc_pt2pt_module_template, 
           sizeof(ompi_osc_base_module_t));

    /* initialize the p2p part */
    OBJ_CONSTRUCT(&(module->p2p_lock), opal_mutex_t);
    OBJ_CONSTRUCT(&(module->p2p_cond), opal_condition_t);
    OBJ_CONSTRUCT(&(module->p2p_acc_lock), opal_mutex_t);
    OBJ_CONSTRUCT(&module->p2p_pending_sendreqs, opal_list_t);
    OBJ_CONSTRUCT(&(module->p2p_copy_pending_sendreqs), opal_list_t);
    OBJ_CONSTRUCT(&(module->p2p_locks_pending), opal_list_t);
    OBJ_CONSTRUCT(&(module->p2p_unlocks_pending), opal_list_t);

    module->p2p_win = win;

    ret = ompi_comm_dup(comm, &(module->p2p_comm));
    if (ret != OMPI_SUCCESS) goto cleanup;

    opal_output_verbose(1, ompi_osc_base_output,
                        "pt2pt component creating window with id %d",
                        ompi_comm_get_cid(module->p2p_comm));

    asprintf(&tmp, "%d", ompi_comm_get_cid(module->p2p_comm));
    ompi_win_set_name(win, tmp);
    free(tmp);

    module->p2p_num_pending_sendreqs = (unsigned int*)
        malloc(sizeof(unsigned int) * ompi_comm_size(module->p2p_comm));
    if (NULL == module->p2p_num_pending_sendreqs) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }
    memset(module->p2p_num_pending_sendreqs, 0, 
           sizeof(unsigned int) * ompi_comm_size(module->p2p_comm));

    module->p2p_num_pending_out = 0;
    module->p2p_num_pending_in = 0;
    module->p2p_num_post_msgs = 0;
    module->p2p_num_complete_msgs = 0;
    module->p2p_tag_counter = 0;

    module->p2p_copy_num_pending_sendreqs = (unsigned int*)
        malloc(sizeof(unsigned int) * ompi_comm_size(module->p2p_comm));
    if (NULL == module->p2p_copy_num_pending_sendreqs) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }
    memset(module->p2p_num_pending_sendreqs, 0, 
           sizeof(unsigned int) * ompi_comm_size(module->p2p_comm));

    /* fence data */
    module->p2p_fence_coll_counts = (int*)
        malloc(sizeof(int) * ompi_comm_size(module->p2p_comm));
    if (NULL == module->p2p_fence_coll_counts) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }
    for (i = 0 ; i < ompi_comm_size(module->p2p_comm) ; ++i) {
        module->p2p_fence_coll_counts[i] = 1;
    }

    /* pwsc data */
    module->p2p_pw_group = NULL;
    module->p2p_sc_group = NULL;
    module->p2p_sc_remote_active_ranks = (bool*)
        malloc(sizeof(bool) * ompi_comm_size(module->p2p_comm));
    if (NULL == module->p2p_sc_remote_active_ranks) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }

    module->p2p_sc_remote_ranks = (int*)
        malloc(sizeof(int) * ompi_comm_size(module->p2p_comm));
    if (NULL == module->p2p_sc_remote_ranks) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /* lock data */
    module->p2p_lock_status = 0;
    module->p2p_shared_count = 0;
    module->p2p_lock_received_ack = 0;

    /* update component data */
    OPAL_THREAD_LOCK(&mca_osc_pt2pt_component.p2p_c_lock);
    opal_hash_table_set_value_uint32(&mca_osc_pt2pt_component.p2p_c_modules,
                                     ompi_comm_get_cid(module->p2p_comm),
                                     module);
    ret = opal_hash_table_get_size(&mca_osc_pt2pt_component.p2p_c_modules);
    if (ret == 1) {
#if OPAL_ENABLE_PROGRESS_THREADS
        mca_osc_pt2pt_component.p2p_c_thread_run = true;
        mca_osc_pt2pt_component.p2p_c_thread.t_run = component_thread_fn;
        mca_osc_pt2pt_component.p2p_c_thread.t_arg = NULL;
        ret = opal_thread_start(&mca_osc_pt2pt_component.p2p_c_thread);
#else
        ret = opal_progress_register(ompi_osc_pt2pt_component_progress);
#endif
    } else {
        ret = OMPI_SUCCESS;
    }
    OPAL_THREAD_UNLOCK(&mca_osc_pt2pt_component.p2p_c_lock); 
    if (OMPI_SUCCESS != ret) goto cleanup;

    /* fill in window information */
    win->w_osc_module = (ompi_osc_base_module_t*) module;
    if (check_config_value_bool("no_locks", info)) {
        win->w_flags |= OMPI_WIN_NO_LOCKS;
    }

    /* sync memory - make sure all initialization completed */
    opal_atomic_mb();

    /* start up receive for protocol headers */
    OPAL_FREE_LIST_WAIT(&mca_osc_pt2pt_component.p2p_c_buffers,
                        item, ret);
    if (OMPI_SUCCESS != ret) goto cleanup;

    buffer = (ompi_osc_pt2pt_buffer_t*) item;
    buffer->mpireq.cbfunc = component_fragment_cb;
    buffer->mpireq.cbdata = (void*) module;
    
    ret = MCA_PML_CALL(irecv(buffer->payload,
                             mca_osc_pt2pt_component.p2p_c_eager_size,
                             MPI_BYTE,
                             MPI_ANY_SOURCE,
                             CONTROL_MSG_TAG,
                             module->p2p_comm,
                             &buffer->mpireq.request));
    if (OMPI_SUCCESS != ret) goto cleanup;

    OPAL_THREAD_LOCK(&mca_osc_pt2pt_component.p2p_c_lock);
    opal_list_append(&mca_osc_pt2pt_component.p2p_c_pending_requests,
                     &buffer->mpireq.super.super);
    OPAL_THREAD_UNLOCK(&mca_osc_pt2pt_component.p2p_c_lock);

    return OMPI_SUCCESS;

 cleanup:
    OBJ_DESTRUCT(&module->p2p_unlocks_pending);
    OBJ_DESTRUCT(&module->p2p_locks_pending);
    OBJ_DESTRUCT(&module->p2p_copy_pending_sendreqs);
    OBJ_DESTRUCT(&module->p2p_pending_sendreqs);
    OBJ_DESTRUCT(&module->p2p_acc_lock);
    OBJ_DESTRUCT(&module->p2p_cond);
    OBJ_DESTRUCT(&module->p2p_lock);

    if (NULL != buffer) {
        OPAL_FREE_LIST_RETURN(&mca_osc_pt2pt_component.p2p_c_buffers, item);
    }
    if (NULL != module->p2p_sc_remote_ranks) {
        free(module->p2p_sc_remote_ranks);
    }
    if (NULL != module->p2p_sc_remote_active_ranks) {
        free(module->p2p_sc_remote_active_ranks);
    }
    if (NULL != module->p2p_fence_coll_counts) {
        free(module->p2p_fence_coll_counts);
    }
    if (NULL != module->p2p_copy_num_pending_sendreqs) {
        free(module->p2p_copy_num_pending_sendreqs);
    }
    if (NULL != module->p2p_num_pending_sendreqs) {
        free(module->p2p_num_pending_sendreqs);
    }
    if (NULL != module->p2p_comm) ompi_comm_free(&module->p2p_comm);

#if OPAL_ENABLE_DEBUG
    memset(module, 0, sizeof(ompi_osc_base_module_t));
#endif
    if (NULL != module) free(module);

    return ret;
}


/* dispatch for callback on message completion */
static void
component_fragment_cb(ompi_osc_pt2pt_mpireq_t *mpireq)
{
    int ret;
    ompi_osc_pt2pt_buffer_t *buffer =
        (ompi_osc_pt2pt_buffer_t*) mpireq;
    ompi_osc_pt2pt_module_t *module = 
        (ompi_osc_pt2pt_module_t*) mpireq->cbdata;


    assert(mpireq->status._ucount >= sizeof(ompi_osc_pt2pt_base_header_t));

    /* handle message */
    switch (((ompi_osc_pt2pt_base_header_t*) buffer->payload)->hdr_type) {
    case OMPI_OSC_PT2PT_HDR_PUT:
        {
            /* get our header and payload */
            ompi_osc_pt2pt_send_header_t *header =
                (ompi_osc_pt2pt_send_header_t*) buffer->payload;
            void *payload = (void*) (header + 1);

#if !defined(WORDS_BIGENDIAN) && OPAL_ENABLE_HETEROGENEOUS_SUPPORT
            if (header->hdr_base.hdr_flags & OMPI_OSC_PT2PT_HDR_FLAG_NBO) {
                OMPI_OSC_PT2PT_SEND_HDR_NTOH(*header);
            }
#endif

            if (!ompi_win_exposure_epoch(module->p2p_win)) {
                if (OMPI_WIN_FENCE & ompi_win_get_mode(module->p2p_win)) {
                    ompi_win_set_mode(module->p2p_win, 
                                      OMPI_WIN_FENCE | 
                                      OMPI_WIN_ACCESS_EPOCH |
                                      OMPI_WIN_EXPOSE_EPOCH);
                }
            }

            ret = ompi_osc_pt2pt_sendreq_recv_put(module, header, payload);
        }
        break;

    case OMPI_OSC_PT2PT_HDR_ACC: 
        {
            /* get our header and payload */
            ompi_osc_pt2pt_send_header_t *header =
                (ompi_osc_pt2pt_send_header_t*) buffer->payload;
            void *payload = (void*) (header + 1);

#if !defined(WORDS_BIGENDIAN) && OPAL_ENABLE_HETEROGENEOUS_SUPPORT
            if (header->hdr_base.hdr_flags & OMPI_OSC_PT2PT_HDR_FLAG_NBO) {
                OMPI_OSC_PT2PT_SEND_HDR_NTOH(*header);
            }
#endif

            if (!ompi_win_exposure_epoch(module->p2p_win)) {
                if (OMPI_WIN_FENCE & ompi_win_get_mode(module->p2p_win)) {
                    ompi_win_set_mode(module->p2p_win, 
                                      OMPI_WIN_FENCE | 
                                      OMPI_WIN_ACCESS_EPOCH |
                                      OMPI_WIN_EXPOSE_EPOCH);
                }
            }

            /* receive into temporary buffer */
            ret = ompi_osc_pt2pt_sendreq_recv_accum(module, header, payload);
        }
        break;

    case OMPI_OSC_PT2PT_HDR_GET:
        {
            /* get our header and payload */
            ompi_osc_pt2pt_send_header_t *header =
                (ompi_osc_pt2pt_send_header_t*) buffer->payload;
            void *payload = (void*) (header + 1);
            ompi_datatype_t *datatype;
            ompi_osc_pt2pt_replyreq_t *replyreq;
            ompi_proc_t *proc;

#if !defined(WORDS_BIGENDIAN) && OPAL_ENABLE_HETEROGENEOUS_SUPPORT
            if (header->hdr_base.hdr_flags & OMPI_OSC_PT2PT_HDR_FLAG_NBO) {
                OMPI_OSC_PT2PT_SEND_HDR_NTOH(*header);
            }
#endif

            if (!ompi_win_exposure_epoch(module->p2p_win)) {
                if (OMPI_WIN_FENCE & ompi_win_get_mode(module->p2p_win)) {
                    ompi_win_set_mode(module->p2p_win, 
                                      OMPI_WIN_FENCE | 
                                      OMPI_WIN_ACCESS_EPOCH |
                                      OMPI_WIN_EXPOSE_EPOCH);
                }
            }

            /* create or get a pointer to our datatype */
            proc = ompi_comm_peer_lookup( module->p2p_comm, header->hdr_origin );
            datatype = ompi_osc_base_datatype_create(proc, &payload);

            if (NULL == datatype) {
                opal_output(ompi_osc_base_output,
                            "Error recreating datatype.  Aborting.");
                ompi_mpi_abort(module->p2p_comm, 1, false);
            }

            /* create replyreq sendreq */
            ret = ompi_osc_pt2pt_replyreq_alloc_init(module,
                                                  header->hdr_origin,
                                                  header->hdr_origin_sendreq,
                                                  header->hdr_target_disp,
                                                  header->hdr_target_count,
                                                  datatype,
                                                  &replyreq);

            /* send replyreq */
            ompi_osc_pt2pt_replyreq_send(module, replyreq);

            /* sendreq does the right retain, so we can release safely */
            OBJ_RELEASE(datatype);
        }
        break;

    case OMPI_OSC_PT2PT_HDR_REPLY:
        {
            ompi_osc_pt2pt_reply_header_t *header = 
                (ompi_osc_pt2pt_reply_header_t*) buffer->payload;
            void *payload = (void*) (header + 1);
            ompi_osc_pt2pt_sendreq_t *sendreq;

#if !defined(WORDS_BIGENDIAN) && OPAL_ENABLE_HETEROGENEOUS_SUPPORT
            if (header->hdr_base.hdr_flags & OMPI_OSC_PT2PT_HDR_FLAG_NBO) {
                OMPI_OSC_PT2PT_REPLY_HDR_NTOH(*header);
            }
#endif

            /* get original sendreq pointer */
            sendreq = (ompi_osc_pt2pt_sendreq_t*) header->hdr_origin_sendreq.pval;
            module = sendreq->req_module;

            /* receive data */
            ompi_osc_pt2pt_replyreq_recv(module, sendreq, header, payload);
        }
        break;

    case OMPI_OSC_PT2PT_HDR_POST:
        {
            int32_t count;
            OPAL_THREAD_LOCK(&module->p2p_lock);
            count = (module->p2p_num_post_msgs -= 1);
            OPAL_THREAD_UNLOCK(&module->p2p_lock);
            if (count == 0) opal_condition_broadcast(&module->p2p_cond);
        }
        break;

    case OMPI_OSC_PT2PT_HDR_COMPLETE:
        {
            ompi_osc_pt2pt_control_header_t *header = 
                (ompi_osc_pt2pt_control_header_t*) buffer->payload;
            int32_t count;

#if !defined(WORDS_BIGENDIAN) && OPAL_ENABLE_HETEROGENEOUS_SUPPORT
            if (header->hdr_base.hdr_flags & OMPI_OSC_PT2PT_HDR_FLAG_NBO) {
                OMPI_OSC_PT2PT_CONTROL_HDR_NTOH(*header);
            }
#endif

            /* we've heard from one more place, and have value reqs to
               process */
            OPAL_THREAD_LOCK(&module->p2p_lock);
            count = (module->p2p_num_complete_msgs -= 1);
            count += (module->p2p_num_pending_in += header->hdr_value[0]);
            OPAL_THREAD_UNLOCK(&module->p2p_lock);

            if (count == 0) opal_condition_broadcast(&module->p2p_cond);
        }
        break;

    case OMPI_OSC_PT2PT_HDR_LOCK_REQ:
        {
            ompi_osc_pt2pt_control_header_t *header = 
                (ompi_osc_pt2pt_control_header_t*) buffer->payload;
            int32_t count;

#if !defined(WORDS_BIGENDIAN) && OPAL_ENABLE_HETEROGENEOUS_SUPPORT
            if (header->hdr_base.hdr_flags & OMPI_OSC_PT2PT_HDR_FLAG_NBO) {
                OMPI_OSC_PT2PT_CONTROL_HDR_NTOH(*header);
            }
#endif

            if (header->hdr_value[1] > 0) {
                ompi_osc_pt2pt_passive_lock(module, header->hdr_value[0], 
                                            header->hdr_value[1]);
            } else {
                OPAL_THREAD_LOCK(&module->p2p_lock);
                count = (module->p2p_lock_received_ack += 1);
                OPAL_THREAD_UNLOCK(&module->p2p_lock);

                if (count != 0) opal_condition_broadcast(&module->p2p_cond);
            }
        }
        break;

    case OMPI_OSC_PT2PT_HDR_UNLOCK_REQ:
        {
            ompi_osc_pt2pt_control_header_t *header = 
                (ompi_osc_pt2pt_control_header_t*) buffer->payload;

#if !defined(WORDS_BIGENDIAN) && OPAL_ENABLE_HETEROGENEOUS_SUPPORT
            if (header->hdr_base.hdr_flags & OMPI_OSC_PT2PT_HDR_FLAG_NBO) {
                OMPI_OSC_PT2PT_CONTROL_HDR_NTOH(*header);
            }
#endif

            ompi_osc_pt2pt_passive_unlock(module, header->hdr_value[0],
                                          header->hdr_value[1]);
        }
        break;

    case OMPI_OSC_PT2PT_HDR_UNLOCK_REPLY:
        {
            int32_t count;

            OPAL_THREAD_LOCK(&module->p2p_lock);
            count = (module->p2p_num_pending_out -= 1);
            OPAL_THREAD_UNLOCK(&module->p2p_lock);
            if (count == 0) opal_condition_broadcast(&module->p2p_cond);
        }
        break;

    default:
        opal_output_verbose(5, ompi_osc_base_output,
                            "received one-sided packet for with unknown type");
    }
    
    ret = MCA_PML_CALL(irecv(buffer->payload,
                             mca_osc_pt2pt_component.p2p_c_eager_size,
                             MPI_BYTE,
                             MPI_ANY_SOURCE,
                             CONTROL_MSG_TAG,
                             module->p2p_comm,
                             &buffer->mpireq.request));
    /* BWB -- FIX ME -- handle irecv errors */
    OPAL_THREAD_LOCK(&mca_osc_pt2pt_component.p2p_c_lock);
    opal_list_append(&mca_osc_pt2pt_component.p2p_c_pending_requests,
                     &buffer->mpireq.super.super);
    OPAL_THREAD_UNLOCK(&mca_osc_pt2pt_component.p2p_c_lock);
}


int
ompi_osc_pt2pt_component_progress(void)
{
    opal_list_item_t *item;
    int ret, done = 0;

#if OPAL_ENABLE_PROGRESS_THREADS
    OPAL_THREAD_LOCK(&mca_osc_pt2pt_component.p2p_c_lock);
#else
    ret = OPAL_THREAD_TRYLOCK(&mca_osc_pt2pt_component.p2p_c_lock);
    if (ret != 0) return 0;
#endif

    for (item = opal_list_get_first(&mca_osc_pt2pt_component.p2p_c_pending_requests) ;
         item != opal_list_get_end(&mca_osc_pt2pt_component.p2p_c_pending_requests) ;
         item = opal_list_get_next(item)) {
        ompi_osc_pt2pt_mpireq_t *buffer = 
            (ompi_osc_pt2pt_mpireq_t*) item;

        /* BWB - FIX ME */
#if OPAL_ENABLE_PROGRESS_THREADS == 0
        if (buffer->request->req_state == OMPI_REQUEST_INACTIVE ||
            buffer->request->req_complete) {
            ret = ompi_request_test(&buffer->request,
                                    &done,
                                    &buffer->status);
        } else {
            done = 0;
            ret = OMPI_SUCCESS;
        }
#else
        ret = ompi_request_test(&buffer->request,
                                &done,
                                &buffer->status);
#endif
        if (OMPI_SUCCESS == ret && 0 != done) {
            opal_list_remove_item(&mca_osc_pt2pt_component.p2p_c_pending_requests,
                                  item);
            OPAL_THREAD_UNLOCK(&mca_osc_pt2pt_component.p2p_c_lock);
            buffer->cbfunc(buffer);
            OPAL_THREAD_LOCK(&mca_osc_pt2pt_component.p2p_c_lock);
            break;
        }
    }
        
    OPAL_THREAD_UNLOCK(&mca_osc_pt2pt_component.p2p_c_lock);

    return done;
}

#if OPAL_ENABLE_PROGRESS_THREADS
static void*
component_thread_fn(opal_object_t *obj)
{
    struct timespec waittime;

    while (mca_osc_pt2pt_component.p2p_c_thread_run) {
        /* wake up whenever a request completes, to make sure it's not
           for us */
        waittime.tv_sec = 1;
        waittime.tv_nsec = 0;
        OPAL_THREAD_LOCK(&ompi_request_lock);
        opal_condition_timedwait(&ompi_request_cond, &ompi_request_lock, &waittime);
        OPAL_THREAD_UNLOCK(&ompi_request_lock);
        ompi_osc_pt2pt_component_progress();
    }

    return NULL;
}
#endif

