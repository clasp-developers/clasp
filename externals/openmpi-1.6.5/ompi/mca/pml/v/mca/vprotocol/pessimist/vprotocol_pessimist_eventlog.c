/*
 * Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "vprotocol_pessimist_eventlog.h"

#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/errmgr/errmgr.h"
#include "ompi/mca/dpm/dpm.h"
#include "ompi/mca/pubsub/pubsub.h"

int vprotocol_pessimist_event_logger_connect(int el_rank, ompi_communicator_t **el_comm)
{
    int rc;
    opal_buffer_t buffer;
    char *port;
    orte_process_name_t el_proc;
    char *hnp_uri, *rml_uri;
    orte_rml_tag_t el_tag;
    char name[MPI_MAX_PORT_NAME];
    int rank;
    vprotocol_pessimist_clock_t connect_info[2];
    
    snprintf(name, MPI_MAX_PORT_NAME, VPROTOCOL_EVENT_LOGGER_NAME_FMT, el_rank);
    port = ompi_pubsub.lookup(name, MPI_INFO_NULL);
    if(NULL == port)
    {
        return ORTE_ERR_NOT_FOUND;
    }
    V_OUTPUT_VERBOSE(45, "Found port < %s >", port);
    
    /* separate the string into the HNP and RML URI and tag */
    if (ORTE_SUCCESS != (rc = ompi_dpm.parse_port(port, &hnp_uri, &rml_uri, &el_tag))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* extract the originating proc's name */
    if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(rml_uri, &el_proc, NULL))) {
        ORTE_ERROR_LOG(rc);
        free(rml_uri); free(hnp_uri);
        return rc;
    }
    /* make sure we can route rml messages to the destination */
    if (ORTE_SUCCESS != (rc = ompi_dpm.route_to_port(hnp_uri, &el_proc))) {
        ORTE_ERROR_LOG(rc);
        free(rml_uri); free(hnp_uri);
        return rc;
    }
    free(rml_uri); free(hnp_uri);
    
    /* Send an rml message to tell the remote end to wake up and jump into 
     * connect/accept */
    OBJ_CONSTRUCT(&buffer, opal_buffer_t);
    rc = orte_rml.send_buffer(&el_proc, &buffer, el_tag+1, 0);
    if(OMPI_SUCCESS > rc) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buffer);        
        return rc;
    }
    OBJ_DESTRUCT(&buffer);

    rc = ompi_dpm.connect_accept(MPI_COMM_SELF, 0, port, true, el_comm);
    if(OMPI_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }
    
    /* Send Rank, receive max buffer size and max_clock back */
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    rc = mca_pml_v.host_pml.pml_send(&rank, 1, MPI_INTEGER, 0, 
                                     VPROTOCOL_PESSIMIST_EVENTLOG_NEW_CLIENT_CMD,
                                     MCA_PML_BASE_SEND_STANDARD, 
                                     mca_vprotocol_pessimist.el_comm);
    if(OPAL_UNLIKELY(MPI_SUCCESS != rc))
        OMPI_ERRHANDLER_INVOKE(mca_vprotocol_pessimist.el_comm, rc,
                               __FILE__ ": failed sending event logger handshake");
    rc = mca_pml_v.host_pml.pml_recv(&connect_info, 2, MPI_UNSIGNED_LONG_LONG, 
                                     0, VPROTOCOL_PESSIMIST_EVENTLOG_NEW_CLIENT_CMD,
                                     mca_vprotocol_pessimist.el_comm, MPI_STATUS_IGNORE);
    if(OPAL_UNLIKELY(MPI_SUCCESS != rc))                                  \
        OMPI_ERRHANDLER_INVOKE(mca_vprotocol_pessimist.el_comm, rc,       \
                               __FILE__ ": failed receiving event logger handshake");   
    
    return rc;
}

int vprotocol_pessimist_event_logger_disconnect(ompi_communicator_t *el_comm)
{
    ompi_dpm.disconnect(el_comm);
    return OMPI_SUCCESS;
}

void vprotocol_pessimist_matching_replay(int *src) {
#if OPAL_ENABLE_DEBUG
    vprotocol_pessimist_clock_t max = 0;
#endif
    mca_vprotocol_pessimist_event_t *event;

    /* searching this request in the event list */
    for(event = (mca_vprotocol_pessimist_event_t *) opal_list_get_first(&mca_vprotocol_pessimist.replay_events);
        event != (mca_vprotocol_pessimist_event_t *) opal_list_get_end(&mca_vprotocol_pessimist.replay_events);
        event = (mca_vprotocol_pessimist_event_t *) opal_list_get_next(event))
    {
        vprotocol_pessimist_matching_event_t *mevent;
        
        if(VPROTOCOL_PESSIMIST_EVENT_TYPE_MATCHING != event->type) continue;        
        mevent = &(event->u_event.e_matching);
        if(mevent->reqid == mca_vprotocol_pessimist.clock)
        {
            /* this is the event to replay */
            V_OUTPUT_VERBOSE(70, "pessimist: replay\tmatch\t%"PRIpclock"\trecv is forced from %d", mevent->reqid, mevent->src);
            (*src) = mevent->src;
            opal_list_remove_item(&mca_vprotocol_pessimist.replay_events, 
                                  (opal_list_item_t *) event);
            VPESSIMIST_EVENT_RETURN(event);
        }   
#if OPAL_ENABLE_DEBUG
        else if(mevent->reqid > max) 
            max = mevent->reqid;                         
    }
    /* not forcing a ANY SOURCE event whose recieve clock is lower than max
     * is a bug indicating we have missed an event during logging ! */
    assert(((*src) != MPI_ANY_SOURCE) || (mca_vprotocol_pessimist.clock > max));
#else
    }
#endif
}

void vprotocol_pessimist_delivery_replay(size_t n, ompi_request_t **reqs,
                                         int *outcount, int *index, 
                                         ompi_status_public_t *status) {
    mca_vprotocol_pessimist_event_t *event;

    for(event = (mca_vprotocol_pessimist_event_t *) opal_list_get_first(&mca_vprotocol_pessimist.replay_events);
        event != (mca_vprotocol_pessimist_event_t *) opal_list_get_end(&mca_vprotocol_pessimist.replay_events);
        event = (mca_vprotocol_pessimist_event_t *) opal_list_get_next(event))
    {
        vprotocol_pessimist_delivery_event_t *devent; 

        if(VPROTOCOL_PESSIMIST_EVENT_TYPE_DELIVERY != event->type) continue;
        devent = &(event->u_event.e_delivery);
        if(devent->probeid < mca_vprotocol_pessimist.clock)
        {
            /* this particular test have to return no request completed yet */
            V_OUTPUT_VERBOSE(70, "pessimist:\treplay\tdeliver\t%"PRIpclock"\tnone", mca_vprotocol_pessimist.clock);
            *index = MPI_UNDEFINED;
            *outcount = 0;
            mca_vprotocol_pessimist.clock++;
            /* This request have to stay in the queue until probeid matches */
            return;
        }
        else if(devent->probeid == mca_vprotocol_pessimist.clock)
        {
            int i;
            for(i = 0; i < (int) n; i++)
            {
                if(VPESSIMIST_FTREQ(reqs[i])->reqid == devent->reqid)
                {
                    V_OUTPUT_VERBOSE(70, "pessimist:\treplay\tdeliver\t%"PRIpclock"\t%"PRIpclock, devent->probeid, devent->reqid);
                    opal_list_remove_item(&mca_vprotocol_pessimist.replay_events,
                                          (opal_list_item_t *) event);
                    VPESSIMIST_EVENT_RETURN(event);
                    *index = i;
                    *outcount = 1;
                    mca_vprotocol_pessimist.clock++;
                    ompi_request_wait(&reqs[i], status);
                    return;
                }
            }
            V_OUTPUT_VERBOSE(70, "pessimist:\treplay\tdeliver\t%"PRIpclock"\tnone", mca_vprotocol_pessimist.clock);
            assert(devent->reqid == 0); /* make sure we don't missed a request */
            *index = MPI_UNDEFINED;
            *outcount = 0;
            mca_vprotocol_pessimist.clock++;
            opal_list_remove_item(&mca_vprotocol_pessimist.replay_events,
                                  (opal_list_item_t *) event);
            VPESSIMIST_EVENT_RETURN(event);
            return;
        }
    }
    V_OUTPUT_VERBOSE(50, "pessimist:\treplay\tdeliver\t%"PRIpclock"\tnot forced", mca_vprotocol_pessimist.clock);
}
