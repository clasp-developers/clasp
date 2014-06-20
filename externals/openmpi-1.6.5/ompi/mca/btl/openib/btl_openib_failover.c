/*
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 * Functions specific to implementing failover support.
 *
 * This file is conditionally copiled into the BTL when one configures
 * it in with --enable-openib-failover.  When this file is compiled
 * in, the multi-BTL configurations can handle errors.  The
 * requirement is that there needs to be more than one openib BTL in
 * use so that all the traffic can move to the other BTL.  This does
 * not support failing over to a different BTL like TCP.
 */

#include "ompi_config.h"
#include "opal_stdint.h"

#include "btl_openib.h"
#include "btl_openib_endpoint.h"
#include "btl_openib_proc.h"
#include "btl_openib_failover.h"

static void error_out_all_pending_frags(mca_btl_base_endpoint_t *ep,
                                        struct mca_btl_base_module_t* module,
                                        bool errout);
static void mca_btl_openib_endpoint_notify(mca_btl_openib_endpoint_t *endpoint,
                                           uint8_t type, int index);

/* debug functions that are normally not needed */
void mca_btl_openib_dump_all_local_rdma_frags(mca_btl_openib_device_t *device);
void mca_btl_openib_dump_all_internal_queues(bool errout);
static void dump_local_rdma_frags(mca_btl_openib_endpoint_t * endpoint);

/**
 * This function is called when we get an error on the completion
 * event of a fragment.  We check to see what type of fragment it is
 * and act accordingly.  In most cases, we first call up into the PML
 * and have it map out this connection for any future communication.
 * In addition, this function will possibly send some control messages
 * over the other openib BTL.  The first control message will tell the
 * remote side to also map out this connection.  The second control
 * message makes sure the eager RDMA connection remains in a sane
 * state.  See that function for more details.
 * @param openib_btl Pointer to BTL that had the error
 * @param des Pointer to descriptor that had the error
 * @param qp Queue pair that had the error
 * @param remote_proc Pointer to process that had the error
 * @param endpoint Pointer to endpoint that had the error
 */ 
void mca_btl_openib_handle_endpoint_error(mca_btl_openib_module_t *openib_btl,
                                          mca_btl_base_descriptor_t *des,
                                          int qp,
                                          ompi_proc_t* remote_proc,
                                          mca_btl_openib_endpoint_t* endpoint)
{
    char *btlname = NULL;
    int btl_ownership;
    /* Since this BTL supports failover, it will call the PML error handler
     * function with the NONFATAL flag.  If the PML is running with failover
     * support, then it will map out the endpoint for further communication
     * and return control here.  If the PML does not have failover support,
     * it will abort the job and control will not return here. */

    /* Note: At this point, what needs to be done is based on the type
     * of openib fragment that got the error.  Also note that in the wc
     * struct, when wc->status != IBV_WC_SUCCESS, these are the only
     * valid fields: wc->wr_id, wc->status, wc->vendor_err, wc->qp_num.
     * This means that one cannot key off of the wc->opcode to see what
     * operation was done.  The important information needs to be read
     * from the fragment. */

    /* Cannot issue callback to SRQ errors because the shared receive
     * queue is shared and is not specific to a connection.  There is no
     * way to figure out what type of message created the error because
     * we need the information in the wc->imm_data field which does not
     * exist when we have an error.  So, nothing to do here but return. */
    if ((openib_frag_type(des) == MCA_BTL_OPENIB_FRAG_RECV) && 
        !BTL_OPENIB_QP_TYPE_PP(qp)) {
        opal_output_verbose(20, mca_btl_openib_component.verbose_failover,
                            "SRQ RECV type=%d", openib_frag_type(des));
        /* Need to think about returning any shared resources of the
         * SRQ.  For now, we do nothing as we rarely see an error on
         * the SRQ. */
        return;
    }
    assert(NULL != remote_proc);

    /* Create a nice string to help with debug */
    if (NULL != openib_btl) {
        asprintf(&btlname, "lid=%d:name=%s",
                 openib_btl->lid, openib_btl->device->ib_dev->name);
    }

    /* The next set of errors are associated with an endpoint, but not
     * with a PML descriptor.  They are not associated with a PML
     * descriptor because:
     *    A. It was a receive
     *    B. It was some type of openib specific control message.
     * Therefore, just drop the fragments and call up into the PML to
     * disable this endpoint for future communication. */
    if (((openib_frag_type(des) == MCA_BTL_OPENIB_FRAG_RECV) && 
         (BTL_OPENIB_QP_TYPE_PP(qp))) ||
         (openib_frag_type(des) == MCA_BTL_OPENIB_FRAG_CONTROL) ||
         (openib_frag_type(des) == MCA_BTL_OPENIB_FRAG_EAGER_RDMA)) {
        openib_btl->error_cb(&openib_btl->super, MCA_BTL_ERROR_FLAGS_NONFATAL,
                              remote_proc, btlname);
        /* Now that this connection has been mapped out at the PML layer,
         * we change the state in the BTL layer.  The change in the PML
         * layer should prevent that we ever try to send on this BTL
         * again.  If we do, then this is an error case.  */
        if (MCA_BTL_IB_FAILED != endpoint->endpoint_state) {
            endpoint->endpoint_state = MCA_BTL_IB_FAILED;
            mca_btl_openib_endpoint_notify(endpoint, MCA_BTL_OPENIB_CONTROL_EP_BROKEN, 0);
            error_out_all_pending_frags(endpoint, &openib_btl->super, true);
        }
        opal_output_verbose(60, mca_btl_openib_component.verbose_failover,
                            "MCA_BTL_OPENIG_FRAG=%d, "
                            "dropping since connection is broken (des=%lx)",
                            openib_frag_type(des), (long unsigned int) des);
        if (NULL != btlname) free(btlname);
        return;
    }

    /* These are RDMA read type fragments.  Just continue with processing */
    if (openib_frag_type(des) == MCA_BTL_OPENIB_FRAG_RECV_USER) {
        OPAL_THREAD_ADD32(&endpoint->get_tokens, 1);
        opal_output_verbose(20, mca_btl_openib_component.verbose_failover,
                            "OPENIB_FRAG_RECV_USER fragment, "
                            "btl=%lx, continue with callbacks",
                            (long unsigned int) &openib_btl->super);
    }

    /* If we are at this point, we have completed a send, RDMA read or
     * RDMA write.  Call the PML callback function to map out this
     * btl for further sending.  We just call this every time we get an
     * error even though it is not necessary.  Subsequent calls with
     * the same remote_proc argument will not actually map anything out. */
    openib_btl->error_cb(&openib_btl->super, MCA_BTL_ERROR_FLAGS_NONFATAL,
                         remote_proc, btlname);
    if (NULL != btlname) free(btlname);

    /* Since we believe we have done a send, read or write, then the
     * des_src fields should have valid data. */
    assert(des->des_src != NULL);

    /* If the endpoint is not yet in the MCA_BTL_IB_CLOSED state, then
     * change the status.  Since this connection was mapped out in the
     * PML layer, no more attempts should be made to send on it.  In
     * addition, send a message to other end of the connection letting
     * it know that this side is now broken.  This is needed in the case
     * of a spurious error which may not cause the remote side to detect
     * the error.  */
    if (MCA_BTL_IB_FAILED != endpoint->endpoint_state) {
        endpoint->endpoint_state = MCA_BTL_IB_FAILED;
        mca_btl_openib_endpoint_notify(endpoint, MCA_BTL_OPENIB_CONTROL_EP_BROKEN, 0);
    }

    /* Now, call the callback function associated with the fragment.
     * In case the fragments were coalesced we need to pull them apart
     * and call the callback function for each one. */
    if(openib_frag_type(des) == MCA_BTL_OPENIB_FRAG_SEND) {
        opal_list_item_t *i;
        while((i = opal_list_remove_first(&to_send_frag(des)->coalesced_frags))) {
            btl_ownership = (to_base_frag(i)->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
            to_base_frag(i)->base.des_cbfunc(&openib_btl->super, endpoint,
                                             &to_base_frag(i)->base, OMPI_ERROR);
            if( btl_ownership ) {
                mca_btl_openib_free(&openib_btl->super, &to_base_frag(i)->base);
            }
        }
    }

    /* This must be a MCA_BTL_OPENIB_FRAG_SEND, MCA_BTL_OPENIB_FRAG_SEND_USER
     * or MCA_BTL_OPENIB_FRAG_RECV_USER. */
    btl_ownership = (des->des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
    des->des_cbfunc(&openib_btl->super, endpoint, des, OMPI_ERROR);
    if( btl_ownership ) {
        mca_btl_openib_free(&openib_btl->super, des);
    }

    /* Here we send another control message to notify the remote side
     * we had an error on a eager fragment.  A non-zero value for the
     * ftr variable indicates that this was an eager RDMA fragment.
     * We need to do this in case the eager RDMA fragment after this
     * one actually made it successfully. */
    if (0 != to_send_frag(des)->ftr) {
        mca_btl_openib_endpoint_notify(endpoint,
                                       MCA_BTL_OPENIB_CONTROL_EP_EAGER_RDMA_ERROR,
                                       (long)to_send_frag(des)->ftr - 1);
    }

    /* We know we have completed a send so return some resources even
     * though connection is broken.  With SRQ, the resources are shared
     * so if we do not return the credits we may not be allowed to send
     * anymore. */
    qp_put_wqe(endpoint, qp);
    if((openib_frag_type(des) == MCA_BTL_OPENIB_FRAG_SEND) && !BTL_OPENIB_QP_TYPE_PP(qp)) {
        OPAL_THREAD_ADD32(&openib_btl->qps[qp].u.srq_qp.sd_credits, 1);
    }

    /* There are several queues associated with an endpoint that may
     * have some unsent fragments sitting in them.  Remove them and
     * call the callback functions with an error so the PML can send
     * them down a different path.  This really only needs to be called
     * once on an endpoint, but for now, just call it a bunch of times.
     * The first time through will remove the unsent fragments so
     * subsequent calls are no-ops. */
    if (endpoint) {
        error_out_all_pending_frags(endpoint, &openib_btl->super, true);
    }
}

/**
 * This functions allows an error to map out the entire BTL.  First a
 * call is made up to the PML to map out all connections from this BTL.
 * Then a message is sent to all the endpoints connected to this BTL.
 * This function is enabled by the btl_openib_port_error_failover
 * MCA parameter.  If that parameter is not set, then this function
 * does not do anything.
 * @param openib_btl Pointer to BTL that had the error
 */
void mca_btl_openib_handle_btl_error(mca_btl_openib_module_t* openib_btl) {
    mca_btl_base_endpoint_t* endpoint;
    int i;

    /* Check to see that the flag is set for the entire map out. */
    if(mca_btl_openib_component.port_error_failover) {
        /* Since we are not specifying a specific connection to bring down,
         * the PML layer will may out the entire BTL for future communication. */
        char *btlname = NULL;
        asprintf(&btlname, "lid=%d:name=%s",
                 openib_btl->lid, openib_btl->device->ib_dev->name);
        openib_btl->error_cb(&openib_btl->super, MCA_BTL_ERROR_FLAGS_NONFATAL,
                             NULL, btlname);
        if (NULL != btlname) free(btlname);

        /* Now send out messages to all endpoints that we are disconnecting.  
         * Only do this to endpoints that are connected.  Otherwise, the
         * remote side does not yet have the information on this endpoint.  */
        for (i = 0; i < opal_pointer_array_get_size(openib_btl->device->endpoints); i++) {
            endpoint = (mca_btl_openib_endpoint_t*)
                opal_pointer_array_get_item(openib_btl->device->endpoints, i);
            if (NULL == endpoint) {
                continue;
            }
            if (MCA_BTL_IB_CONNECTED == endpoint->endpoint_state) {
                mca_btl_openib_endpoint_notify(endpoint, MCA_BTL_OPENIB_CONTROL_EP_BROKEN, 0);
                endpoint->endpoint_state = MCA_BTL_IB_FAILED;
                error_out_all_pending_frags(endpoint, &openib_btl->super, true);
            }
        }
    }
}

/**
 * This function gets called when a control message is received that
 * is one of the following types:
 *   MCA_BTL_OPENIB_CONTROL_EP_BROKEN
 *   MCA_BTL_OPENIB_CONTROL_EP_EAGER_RDMA_ERROR message
 * Note that we are using the working connection to send information
 * about the broken connection.  That is why we have to look at the
 * various information in the control message to figure out which
 * endpoint is broken.  It is (obviously) not the one the message was
 * received on, because we would not have received the message in that
 * case.  In the case of the BROKEN message, that means the remote
 * side is notifying us that it has brought down its half of the
 * connection.  Therefore, we need to bring out half down.  This is
 * done because it has been observed that there are cases where only
 * one side of the connection actually sees the error.  This means we
 * can be left in a state where one side believes it has two BTLs, but
 * the other side believes it only has one.  This can cause problems.
 * In the case of the EAGER_RDMA_ERROR, see elsewhere in the code what
 * we are doing.
 * @param ctl_hdr Pointer control header that was received
 */
void btl_openib_handle_failover_control_messages(mca_btl_openib_control_header_t *ctl_hdr,
                                                 mca_btl_openib_endpoint_t* ep)
{
    mca_btl_openib_broken_connection_header_t *bc_hdr =
        (mca_btl_openib_broken_connection_header_t*)ctl_hdr;
    int i;
    int found = false;

    if(ep->nbo) {
        BTL_OPENIB_BROKEN_CONNECTION_HEADER_NTOH((*bc_hdr));
    }

    opal_output_verbose(30, mca_btl_openib_component.verbose_failover,
                        "IB: Control message received from %d: lid=%d,subnet=0x%" PRIx64 "",
                        bc_hdr->vpid, bc_hdr->lid, bc_hdr->subnet_id);

    /* Now we walk through all the endpoints on all the BTLs to
     * find out which one to map out.  */
    for(i = 0; i < mca_btl_openib_component.ib_num_btls; i++) {
        mca_btl_openib_module_t* newbtl;
        int j;

        newbtl = mca_btl_openib_component.openib_btls[i];
        /* Now, find the endpoint associated with it */
        for (j = 0; j < opal_pointer_array_get_size(newbtl->device->endpoints); j++) {
            mca_btl_base_endpoint_t* newep;
            newep = (mca_btl_openib_endpoint_t*)
                opal_pointer_array_get_item(newbtl->device->endpoints, j);
            if (NULL == newep) {
                continue;
            }
            /* Now compare the LID, subnet ID, and the vpid we received
             * from the remote side and try to match it to an endpoint. */
            if ((bc_hdr->lid == newep->rem_info.rem_lid) &&
                (bc_hdr->subnet_id == newep->rem_info.rem_subnet_id) &&
                (bc_hdr->vpid == newep->endpoint_proc->proc_guid.vpid)) {
                opal_output_verbose(30, mca_btl_openib_component.verbose_failover,
                                    "IB: Control message received from %d: "
                                    "found match: lid=%d,"
                                    "subnet=0x%" PRIx64 ",endpoint_state=%d",
                                    newep->endpoint_proc->proc_guid.vpid,
                                    newep->rem_info.rem_lid,
                                    newep->rem_info.rem_subnet_id,
                                    newep->endpoint_state);
                found = true;
                /* At this point, we have found the endpoint.  Now decode the
                 * message type and do the appropriate action. */
                if (MCA_BTL_OPENIB_CONTROL_EP_BROKEN == ctl_hdr->type) {
                    /* Now that we found a match, check the state of the
                     * endpoint to see it is already in a failed state.
                     * If not, then notify the upper layer and error out
                     * any pending fragments. */
                    if (MCA_BTL_IB_FAILED == newep->endpoint_state) {
                        return;
                    } else {
                        char *btlname = NULL;
                        ompi_proc_t* remote_proc = NULL;

                        asprintf(&btlname, "lid=%d:name=%s",
                                 newbtl->lid, newbtl->device->ib_dev->name);

                        remote_proc = newep->endpoint_proc->proc_ompi;

                        opal_output_verbose(10, mca_btl_openib_component.verbose_failover,
                                            "IB: Control message received from %d: "
                                            "bringing down connection,lid=%d,"
                                            "subnet=0x%" PRIx64 ",endpoint_state=%d",
                                            newep->endpoint_proc->proc_guid.vpid,
                                            newep->rem_info.rem_lid,
                                            newep->rem_info.rem_subnet_id,
                                            newep->endpoint_state);
                        newbtl->error_cb(&newbtl->super, MCA_BTL_ERROR_FLAGS_NONFATAL,
                                         remote_proc, btlname);
                        if (NULL != btlname) free(btlname);

                        error_out_all_pending_frags(newep, &newbtl->super, true);
                        newep->endpoint_state = MCA_BTL_IB_FAILED;
                        return;
                    }
                } else { /* MCA_BTL_OPENIB_CONTROL_EP_EAGER_RDMA_ERROR message */
                    /* If we are still pointing at the location where
                     * we detected an error on the remote side, then
                     * bump the index by one. */
                    if (newep->eager_rdma_local.head == (uint16_t)bc_hdr->index) {
                        /* Adjust the local head by one just in case */
                        MCA_BTL_OPENIB_RDMA_NEXT_INDEX(newep->eager_rdma_local.head);
                        opal_output_verbose(20, mca_btl_openib_component.verbose_failover,
                                            "IB: rank=%d, control message (remote=%d), "
                                            "moved local head by one (new=%d)",
                                            ORTE_PROC_MY_NAME->vpid,    
                                            newep->endpoint_proc->proc_guid.vpid,
                                            newep->eager_rdma_local.head);
                    } else {
                        opal_output_verbose(20, mca_btl_openib_component.verbose_failover,
                                            "IB: rank=%d, control message (remote=%d), "
                                            "did not move local head by one (still=%d)",
                                            ORTE_PROC_MY_NAME->vpid,
                                            newep->endpoint_proc->proc_guid.vpid,
                                            newep->eager_rdma_local.head);
                    }
                }
                break; /* since we found the endpoint */
            }
        }
    }
    if (false == found) {
        opal_output_verbose(30, mca_btl_openib_component.verbose_failover,
                            "IB: Control message: no match found");
    }
}

/**
 * This function will find all the pending fragments on an endpoint
 * and call the callback function with OMPI_ERROR.  It walks through
 * each qp with each priority and looks for both no_credits_pending_frags
 * and no_wqe_pending_frags.  It then looks for any pending_lazy_frags,
 * pending_put_frags, and pending_get_frags.  This function is only 
 * called when running with failover support enabled.  Note that
 * the errout parameter allows the function to also be used as a 
 * debugging tool to see if there are any fragments on any of the
 * queues.
 * @param ep Pointer to endpoint that had error
 * @param module Pointer to module that had error
 * @param errout Boolean which says whether to error them out or not
 */
static void error_out_all_pending_frags(mca_btl_base_endpoint_t *ep,
                                        struct mca_btl_base_module_t* module,
                                        bool errout)
{
    int qp, pri, len, total, btl_ownership;

    opal_list_item_t *item;
    mca_btl_openib_com_frag_t* frag;
    mca_btl_base_descriptor_t *des;
    int verbose = 10;  /* Verbosity level unless debugging */

    /* If debugging, drop verbosity level so we can see the output
     * regardless of the level the program was run with. */
    if (false == errout) {
	verbose = 0;
    }

    total = 0;
    /* Traverse all QPs and all priorities and move to other endpoint */
    for (qp = 0; qp < mca_btl_openib_component.num_qps; ++qp) {
        for (pri = 0; pri < 2; ++pri) {
            /* All types of qp's have a no_wqe_pending_frags list */
            len = opal_list_get_size(&ep->qps[qp].no_wqe_pending_frags[pri]);
            if (len > 0) {
                total += len;
                opal_output_verbose(verbose, mca_btl_openib_component.verbose_failover,
                                    "IB: Checking for no_wqe_pending_frags qp=%d, "
                                    "pri=%d, list size=%d",
                                    qp, pri, len);
                if (true == errout) {
                    while (NULL != (item = opal_list_remove_first(&ep->qps[qp].
                                                                  no_wqe_pending_frags[pri]))) {
                        frag = (mca_btl_openib_com_frag_t *) item;
                        des = (mca_btl_base_descriptor_t *)frag;

                        /* Error out any coalesced frags if they exist */
                        if(openib_frag_type(des) == MCA_BTL_OPENIB_FRAG_SEND) {
                            opal_list_item_t *i;
                            while((i = opal_list_remove_first(&to_send_frag(des)->coalesced_frags))) {
                                opal_output_verbose(verbose, mca_btl_openib_component.verbose_failover,
                                                    "IB: Found coalesced frag in no_wqe_pending_frags");
                                btl_ownership = (to_base_frag(i)->base.des_flags &
                                                 MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
                                to_base_frag(i)->base.des_cbfunc(module, ep,
                                                                 &to_base_frag(i)->base, OMPI_ERROR);
                                if( btl_ownership ) {
                                    mca_btl_openib_free(module, &to_base_frag(i)->base);
                                }
                            }
                        }
                        btl_ownership = (des->des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
                        des->des_cbfunc(module, ep, des, OMPI_ERROR);
                        if( btl_ownership ) {
                            mca_btl_openib_free(module, des);
                        }
                    }
                }
            }
            if (BTL_OPENIB_QP_TYPE_PP(qp)) {
                len = opal_list_get_size(&ep->qps[qp].no_credits_pending_frags[pri]);
                if (len > 0) {
                    total += len;
                    opal_output_verbose(verbose, mca_btl_openib_component.verbose_failover,
                                        "IB: Checking for no_credits_pending_frags qp=%d, "
                                        "pri=%d, list size=%d",
                                        qp, pri, len);
                    if (true == errout) {
                        while (NULL != (item = opal_list_remove_first(&ep->qps[qp].
                                                                      no_credits_pending_frags[pri]))) {
                            frag = (mca_btl_openib_com_frag_t *) item;
                            des = (mca_btl_base_descriptor_t *)frag;

                            /* Error out any coalesced frags if they exist */
                            if(openib_frag_type(des) == MCA_BTL_OPENIB_FRAG_SEND) {
                                opal_list_item_t *i;
                                while((i = opal_list_remove_first(&to_send_frag(des)->coalesced_frags))) {
                                    opal_output_verbose(verbose, mca_btl_openib_component.verbose_failover,
                                                        "IB: Found coalesced frag in "
                                                        "no_credits_pending_frags");
                                    btl_ownership = (to_base_frag(i)->base.des_flags &
                                                     MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
                                    to_base_frag(i)->base.des_cbfunc(module, ep,
                                                                     &to_base_frag(i)->base, OMPI_ERROR);
                                    if( btl_ownership ) {
                                        mca_btl_openib_free(module, &to_base_frag(i)->base);
                                    }
                                }
                            }
                            btl_ownership = (des->des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
                            des->des_cbfunc(module, ep, des, OMPI_ERROR);
                            if( btl_ownership ) {
                                mca_btl_openib_free(module, des);
                            }
                        }
                    }
                }

            } else if (BTL_OPENIB_QP_TYPE_SRQ(qp)) {
                len = opal_list_get_size(&ep->endpoint_btl->qps[qp].u.srq_qp.pending_frags[pri]);
                if (len > 0) {
                    total += len;
                    opal_output_verbose(verbose, mca_btl_openib_component.verbose_failover,
                                        "IB: Checking for srq pending_frags qp=%d, pri=%d, "
                                        "list size=%d",
                                        qp, pri, len);
                    if (true == errout) {
                        while (NULL != (item = opal_list_remove_first(&ep->endpoint_btl->qps[qp].
                                                                      u.srq_qp.pending_frags[pri]))) {
                            frag = (mca_btl_openib_com_frag_t *) item;
                            des = (mca_btl_base_descriptor_t *)frag;

                            /* Error out any coalesced frags if they exist */
                            if(openib_frag_type(des) == MCA_BTL_OPENIB_FRAG_SEND) {
                                opal_list_item_t *i;
                                while((i = opal_list_remove_first(&to_send_frag(des)->coalesced_frags))) {
                                    opal_output_verbose(verbose, mca_btl_openib_component.verbose_failover,
                                                        "IB: Found coalesced frag in SRQ pending_frags");
                                    btl_ownership = (to_base_frag(i)->base.des_flags &
                                                     MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
                                    to_base_frag(i)->base.des_cbfunc(module, ep,
                                                                     &to_base_frag(i)->base, OMPI_ERROR);
                                    if( btl_ownership ) {
                                        mca_btl_openib_free(module, &to_base_frag(i)->base);
                                    }
                                }
                            }
                            btl_ownership = (des->des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
                            des->des_cbfunc(module, ep, des, OMPI_ERROR);
                            if( btl_ownership ) {
                                mca_btl_openib_free(module, des);
                            }
                        }
                    }
                }
            }
        }
    }

    /* Check for any frags from a connection that was never made.  Not sure if this
     * can actually happen. */
    len = opal_list_get_size(&ep->pending_lazy_frags);

    if (len > 0) {
        total += len;
        opal_output_verbose(verbose, mca_btl_openib_component.verbose_failover,
                            "IB: Checking for pending_lazy_frags, list size=%d", len);
        if (true == errout) {
            while  (NULL != (item = opal_list_remove_first(&(ep->pending_lazy_frags)))) {
                frag = (mca_btl_openib_com_frag_t *) item;
                des = (mca_btl_base_descriptor_t *)frag;
                des->des_cbfunc(module, ep, des, OMPI_ERROR);
            }
        }
    }

    len = opal_list_get_size(&ep->pending_put_frags);
    if (len > 0) {
        total += len;
        opal_output_verbose(verbose, mca_btl_openib_component.verbose_failover,
                            "IB: Checking for pending_put_frags, list size=%d", len);
        if (true == errout) {
            while (NULL != (item = opal_list_remove_first(&(ep->pending_put_frags)))) {
                frag = (mca_btl_openib_com_frag_t *) item;
                des = (mca_btl_base_descriptor_t *)frag;
                des->des_cbfunc(module, ep, des, OMPI_ERROR);
            }
        }
    }

    len = opal_list_get_size(&ep->pending_get_frags);
    if (len > 0) {
        total += len;
        opal_output_verbose(verbose, mca_btl_openib_component.verbose_failover,
                            "IB: Checking for pending_get_frags, list size=%d", len);
        if (true == errout) {
            while (NULL != (item = opal_list_remove_first(&(ep->pending_put_frags)))) {
                frag = (mca_btl_openib_com_frag_t *) item;
                des = (mca_btl_base_descriptor_t *)frag;
                des->des_cbfunc(module, ep, des, OMPI_ERROR);
            }
        }
    }

    opal_output_verbose(verbose + 30, mca_btl_openib_component.verbose_failover,
                        "IB: Finished checking for pending_frags, total moved=%d",
                        total);
}

/* local callback function for completion of a failover control message */
static void mca_btl_openib_endpoint_notify_cb(mca_btl_base_module_t* btl,
                                              struct mca_btl_base_endpoint_t* endpoint,
                                              struct mca_btl_base_descriptor_t* descriptor,
                                              int status)
{
    MCA_BTL_IB_FRAG_RETURN(descriptor);
}

/**
 * This function is used to send a message to the remote side
 * indicating the endpoint is broken and telling the remote side to
 * brings its endpoint down as well.  This is needed because there are
 * cases where only one side of the connection determines that the
 * there was a problem.
 * @param endpoint Pointer to endpoint with error
 * @param type Type of message to be sent, can be one of two types
 * @param index When sending RDMA error message, index is non zero
 */
static void mca_btl_openib_endpoint_notify(mca_btl_base_endpoint_t* endpoint, uint8_t type, int index)
{
    mca_btl_openib_module_t* openib_btl = endpoint->endpoint_btl;
    mca_btl_openib_module_t* newbtl = NULL;
    bool found = false;
    mca_btl_openib_broken_connection_header_t *bc_hdr;
    mca_btl_openib_send_control_frag_t* frag;
    mca_btl_base_endpoint_t* newep;
    int i, rc;
    ompi_proc_t* remote_proc = endpoint->endpoint_proc->proc_ompi;

    /* First, find a different BTL than this one that got the
     * error to send the message over. */
    for(i = 0; i < mca_btl_openib_component.ib_num_btls; i++) {
        if (mca_btl_openib_component.openib_btls[i] != openib_btl) {
            newbtl = mca_btl_openib_component.openib_btls[i];
            break;
        }
    }
    if (NULL == newbtl) {
        opal_output_verbose(20, mca_btl_openib_component.verbose_failover,
                            "IB: Endpoint Notify: No BTL found");
        /* If we cannot find one, then just return. */
        return;
    }

    /* Now, find the endpoint associated with it.  The device
     * associated with the BTL has the list of all the
     * endpoints. */
    for (i = 0; i < opal_pointer_array_get_size(newbtl->device->endpoints); i++) {
        newep = (mca_btl_openib_endpoint_t*)
            opal_pointer_array_get_item(newbtl->device->endpoints, i);
        if (NULL == newep) {
            continue;
        }
        if (newep->endpoint_proc->proc_ompi == remote_proc) {
            found = true;
            break;
        }
    }
    if (false == found) {
        opal_output_verbose(20, mca_btl_openib_component.verbose_failover,
                            "IB: Endpoint Notify: No endpoint found");
        /* If we cannot find a match, then just return. */
        return;
    }

    frag = alloc_control_frag(newbtl);
    if(NULL == frag) {
        opal_output_verbose(20, mca_btl_openib_component.verbose_failover,
                            "IB: Endpoint Notify: No frag space");
        /* If no frag available, then just return. */
        return;
    }

    to_base_frag(frag)->base.des_cbfunc =
        mca_btl_openib_endpoint_notify_cb;
    to_base_frag(frag)->base.des_cbdata = NULL;
    to_base_frag(frag)->base.des_flags |= MCA_BTL_DES_FLAGS_PRIORITY|MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
    to_base_frag(frag)->base.order = mca_btl_openib_component.credits_qp;
    to_base_frag(frag)->segment.seg_len =
        sizeof(mca_btl_openib_broken_connection_header_t);
    to_com_frag(frag)->endpoint = newep;

    frag->hdr->tag = MCA_BTL_TAG_BTL;
    bc_hdr = (mca_btl_openib_broken_connection_header_t*)to_base_frag(frag)->segment.seg_addr.pval;
    bc_hdr->control.type = type;
    bc_hdr->lid = endpoint->endpoint_btl->port_info.lid;
    bc_hdr->subnet_id = endpoint->endpoint_btl->port_info.subnet_id;
    bc_hdr->vpid = ORTE_PROC_MY_NAME->vpid;
    bc_hdr->index = index;

    if(newep->nbo) {
        BTL_OPENIB_BROKEN_CONNECTION_HEADER_HTON((*bc_hdr));
    }
    rc = mca_btl_openib_endpoint_send(newep, frag);
    if (OMPI_SUCCESS == rc ||OMPI_ERR_RESOURCE_BUSY == rc) {
        return;
    }

    MCA_BTL_IB_FRAG_RETURN(frag);
    BTL_ERROR(("Error sending BROKEN CONNECTION buffer (%s)", strerror(errno)));
    return;
}

/*
 * Function used for debugging problems in eager rdma.
 */
static void dump_local_rdma_frags(mca_btl_openib_endpoint_t * endpoint) {
    mca_btl_openib_recv_frag_t *headers_buf = endpoint->eager_rdma_local.frags;
    mca_btl_openib_recv_frag_t * frag;
    mca_btl_openib_control_header_t* chdr;
    int i, size;

    opal_output(0, "Head = %d", endpoint->eager_rdma_local.head);

    for (i = 0; i < mca_btl_openib_component.eager_rdma_num; i++) {
        frag = &headers_buf[i];
        size = MCA_BTL_OPENIB_RDMA_FRAG_GET_SIZE(frag->ftr);

        frag->hdr = (mca_btl_openib_header_t*)(((char*)frag->ftr) -
               size + sizeof(mca_btl_openib_footer_t));
        to_base_frag(frag)->segment.seg_addr.pval =
               ((unsigned char* )frag->hdr) + sizeof(mca_btl_openib_header_t);

        chdr = to_base_frag(frag)->segment.seg_addr.pval;
        if ((MCA_BTL_TAG_BTL == frag->hdr->tag) &&
            (MCA_BTL_OPENIB_CONTROL_CREDITS == chdr->type)) {
            opal_output(0, "tag[%d] is credit message", i);
        } else {
            opal_output(0, "frag[%d] size=%d,tag=%d,ftr->u.buf=%d", i, size, frag->hdr->tag,
                        frag->ftr->u.buf[3]);
        }
    }
}

/*
 * Function used for debugging problems in eager rdma.
 */
void mca_btl_openib_dump_all_local_rdma_frags(mca_btl_openib_device_t *device) {
    int i, c;
    mca_btl_openib_endpoint_t* endpoint;

    c = device->eager_rdma_buffers_count;
    opal_output(0, "rank=%d, device=%s", ORTE_PROC_MY_NAME->vpid, device->ib_dev->name);

    for(i = 0; i < c; i++) {
        endpoint = device->eager_rdma_buffers[i];

        if(!endpoint)
            continue;

        dump_local_rdma_frags(endpoint);
    }
}

/**
 * This function is a debugging tool.  If you notify a hang, you can
 * call this function from a debugger and see if there are any 
 * messages stuck in any of the queues.  If you call it with
 * errout=true, then it will error them out.  Otherwise, it will
 * just print out the size of the queues with data in them.
 */
void mca_btl_openib_dump_all_internal_queues(bool errout) {
    int i, j, num_eps;
    mca_btl_openib_module_t* btl;
    int total;
    mca_btl_base_endpoint_t* ep;
    struct mca_btl_base_module_t* module;

    for(i = 0; i < mca_btl_openib_component.ib_num_btls; i++) {
        btl = mca_btl_openib_component.openib_btls[i];
        module = &btl->super;
        num_eps = opal_pointer_array_get_size(btl->device->endpoints);
        
        /* Now, find the endpoint associated with it */
        for (j = 0; j < num_eps; j++) {
            ep = (mca_btl_openib_endpoint_t*)
                opal_pointer_array_get_item(btl->device->endpoints, j);
            if (NULL == ep) {
                continue;
            }

            total = 0;
            error_out_all_pending_frags(ep, module, errout);
        }
    }
}

