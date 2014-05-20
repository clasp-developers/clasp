/*
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "opal/types.h"

#include "orte/mca/rml/base/base.h"
#include "orte/mca/rml/rml_types.h"
#include "opal/dss/dss.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "rml_oob.h"


static void
orte_rml_recv_msg_callback(int status,
                           struct orte_process_name_t* peer,
                           struct iovec* iov,
                           int count,
                           orte_rml_tag_t tag,
                           void* cbdata)
{
    orte_rml_oob_msg_t *msg = (orte_rml_oob_msg_t*) cbdata;
    orte_rml_oob_msg_header_t *hdr =
        (orte_rml_oob_msg_header_t*) iov[0].iov_base;
    ORTE_RML_OOB_MSG_HEADER_NTOH(*hdr);

    OPAL_OUTPUT_VERBOSE((1, orte_rml_base_output,
                         "%s recv from %s for %s (tag %d)",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&hdr->origin),
                         ORTE_NAME_PRINT(&hdr->destination),
                         hdr->tag));

    if (msg->msg_type == ORTE_RML_BLOCKING_RECV) {
        /* blocking send */
        msg->msg_status = status;
        msg->msg_complete = true;
        opal_condition_broadcast(&msg->msg_cond);
    } else if (msg->msg_type == ORTE_RML_NONBLOCKING_IOV_RECV) {
        /* non-blocking iovec send */
        if (status > 0) {
            status -= sizeof(orte_rml_oob_msg_header_t);
        }

        msg->msg_cbfunc.iov(status, &hdr->origin, iov + 1, count - 1, 
                            hdr->tag, msg->msg_cbdata);
        if (!msg->msg_persistent) OBJ_RELEASE(msg);

    } else if (msg->msg_type == ORTE_RML_NONBLOCKING_BUFFER_RECV) {
        /* non-blocking buffer send */
        status = opal_dss.load(&msg->msg_recv_buffer, 
                               iov[1].iov_base, 
                               iov[1].iov_len);

        msg->msg_cbfunc.buffer(status, &hdr->origin, &msg->msg_recv_buffer, 
                               hdr->tag, msg->msg_cbdata);

        if (!msg->msg_persistent) OBJ_RELEASE(msg);
    } else {
        abort();
    }
}


int
orte_rml_oob_recv(orte_process_name_t* peer,
                  struct iovec *iov,
                  int count,
                  orte_rml_tag_t tag,
                  int flags)
{
    orte_rml_oob_msg_t *msg = OBJ_NEW(orte_rml_oob_msg_t);
    int ret;
    int i;

    msg->msg_type = ORTE_RML_BLOCKING_RECV;
    flags |= ORTE_RML_FLAG_RECURSIVE_CALLBACK;

    msg->msg_data = (struct iovec *) malloc(sizeof(struct iovec) * (count + 1));
    msg->msg_data[0].iov_base = (ompi_iov_base_ptr_t)&msg->msg_header;
    msg->msg_data[0].iov_len = sizeof(orte_rml_oob_msg_header_t);
    for (i = 0 ; i < count ; ++i) {
        msg->msg_data[i + 1].iov_base = iov[i].iov_base;
        msg->msg_data[i + 1].iov_len = iov[i].iov_len;
    }

    ret = orte_rml_oob_module.active_oob->oob_recv_nb(peer, msg->msg_data,
                                                      count + 1, tag, flags,
                                                      orte_rml_recv_msg_callback,
                                                      msg);
    if (ret < 0) goto cleanup;

    OPAL_THREAD_LOCK(&msg->msg_lock);
    while (!msg->msg_complete) {
        opal_condition_wait(&msg->msg_cond, &msg->msg_lock);
    }
    ret = msg->msg_status;
    OPAL_THREAD_UNLOCK(&msg->msg_lock);

 cleanup:
    OBJ_RELEASE(msg);

    if (ret > 0) {
        ret -= sizeof(struct orte_rml_oob_msg_header_t);
    }
    return ret;
}


int
orte_rml_oob_recv_nb(orte_process_name_t* peer,
                     struct iovec* iov,
                     int count,
                     orte_rml_tag_t tag,
                     int flags,
                     orte_rml_callback_fn_t cbfunc,
                     void* cbdata)
{
    orte_rml_oob_msg_t *msg = OBJ_NEW(orte_rml_oob_msg_t);
    int ret;
    int i;

    msg->msg_type = ORTE_RML_NONBLOCKING_IOV_RECV;
    msg->msg_persistent = (flags & ORTE_RML_PERSISTENT) ? true : false;
    msg->msg_cbfunc.iov = cbfunc;
    msg->msg_cbdata = cbdata;

    msg->msg_data = (struct iovec *) malloc(sizeof(struct iovec) * (count + 1));
    msg->msg_data[0].iov_base = (ompi_iov_base_ptr_t)&msg->msg_header;
    msg->msg_data[0].iov_len = sizeof(orte_rml_oob_msg_header_t);
    for (i = 0 ; i < count ; ++i) {
        msg->msg_data[i + 1].iov_base = iov[i].iov_base;
        msg->msg_data[i + 1].iov_len = iov[i].iov_len;
    }

    ret = orte_rml_oob_module.active_oob->oob_recv_nb(peer, msg->msg_data,
                                                      count + 1, tag, flags,
                                                      orte_rml_recv_msg_callback,
                                                      msg);
    if (ret < 0) OBJ_RELEASE(msg);

    return ret;
}


int
orte_rml_oob_recv_buffer(orte_process_name_t* peer,
                         opal_buffer_t *buf,
                         orte_rml_tag_t tag,
                         int flags)
{
    orte_rml_oob_msg_t *msg = OBJ_NEW(orte_rml_oob_msg_t);
    int ret;

    msg->msg_type = ORTE_RML_BLOCKING_RECV;
    flags |= (ORTE_RML_FLAG_RECURSIVE_CALLBACK | ORTE_RML_ALLOC);

    msg->msg_data = (struct iovec *) malloc(sizeof(struct iovec) * 2);

    msg->msg_data[0].iov_base = (ompi_iov_base_ptr_t)&msg->msg_header;
    msg->msg_data[0].iov_len = sizeof(orte_rml_oob_msg_header_t);

    msg->msg_data[1].iov_base = NULL;
    msg->msg_data[1].iov_len = 0;

    ret = orte_rml_oob_module.active_oob->oob_recv_nb(peer, msg->msg_data,
                                                      2, tag, flags,
                                                      orte_rml_recv_msg_callback,
                                                      msg);
    if (ret < 0) goto cleanup;

    OPAL_THREAD_LOCK(&msg->msg_lock);
    while (!msg->msg_complete) {
        opal_condition_wait(&msg->msg_cond, &msg->msg_lock);
    }
    ret = msg->msg_status;
    OPAL_THREAD_UNLOCK(&msg->msg_lock);

    if (ret > 0) {
        ret = opal_dss.load(buf, 
                            msg->msg_data[1].iov_base,
                            msg->msg_data[1].iov_len);
    }

 cleanup:
    OBJ_RELEASE(msg);


    return ret;
}


int
orte_rml_oob_recv_buffer_nb(orte_process_name_t* peer,
                            orte_rml_tag_t tag,
                            int flags,
                            orte_rml_buffer_callback_fn_t cbfunc,
                            void* cbdata)
{
    orte_rml_oob_msg_t *msg = OBJ_NEW(orte_rml_oob_msg_t);
    int ret;

    msg->msg_data = (struct iovec *) malloc(sizeof(struct iovec) * 2);

    msg->msg_data[0].iov_base = (ompi_iov_base_ptr_t)&msg->msg_header;
    msg->msg_data[0].iov_len = sizeof(orte_rml_oob_msg_header_t);

    msg->msg_data[1].iov_base = NULL;
    msg->msg_data[1].iov_len = 0;

    msg->msg_type = ORTE_RML_NONBLOCKING_BUFFER_RECV;
    msg->msg_persistent = (flags & ORTE_RML_PERSISTENT) ? true : false;
    msg->msg_cbfunc.buffer = cbfunc;
    msg->msg_cbdata = cbdata;
    flags |= ORTE_RML_ALLOC;

    ret = orte_rml_oob_module.active_oob->oob_recv_nb(peer, 
                                                      msg->msg_data,
                                                      2,
                                                      tag, flags,
                                                      orte_rml_recv_msg_callback,
                                                      msg);
    if (ret < 0) OBJ_RELEASE(msg);

    return ret;
}


int
orte_rml_oob_recv_cancel(orte_process_name_t* peer, 
                         orte_rml_tag_t tag)
{
    return orte_rml_oob_module.active_oob->oob_recv_cancel(peer, tag);
}
