/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "pml_bfo.h"
#include "pml_bfo_sendreq.h"
#include "ompi/mca/bml/base/base.h" 

int mca_pml_bfo_progress(void)
{
    int i, queue_length = opal_list_get_size(&mca_pml_bfo.send_pending);
    int j, completed_requests = 0;
    bool send_succedded;

    if( OPAL_LIKELY(0 == queue_length) )
        return 0;

    for( i = 0; i < queue_length; i++ ) {
        mca_pml_bfo_send_pending_t pending_type = MCA_PML_BFO_SEND_PENDING_NONE;
        mca_pml_bfo_send_request_t* sendreq;
        mca_bml_base_endpoint_t* endpoint;

        sendreq = get_request_from_send_pending(&pending_type);
        if(OPAL_UNLIKELY(NULL == sendreq))
            break;

        switch(pending_type) {
        case MCA_PML_BFO_SEND_PENDING_NONE:
            assert(0);
            return 0;
        case MCA_PML_BFO_SEND_PENDING_SCHEDULE:
            if( mca_pml_bfo_send_request_schedule_exclusive(sendreq) ==
                OMPI_ERR_OUT_OF_RESOURCE ) {
                return 0;
            }
            completed_requests++;
            break;
        case MCA_PML_BFO_SEND_PENDING_START:
            endpoint = sendreq->req_endpoint;
            send_succedded = false;
            for(j = 0; j < (int)mca_bml_base_btl_array_get_size(&endpoint->btl_eager); j++) {
                mca_bml_base_btl_t* bml_btl;
                int rc;
                
                /* select a btl */
                bml_btl = mca_bml_base_btl_array_get_next(&endpoint->btl_eager);
                rc = mca_pml_bfo_send_request_start_btl(sendreq, bml_btl);
                if( OPAL_LIKELY(OMPI_SUCCESS == rc) ) {
                    send_succedded = true;
                    completed_requests++;
                    break;
                }
            }
            if( false == send_succedded ) {
                add_request_to_send_pending(sendreq, MCA_PML_BFO_SEND_PENDING_START, true);
            }
        }
    }
    return completed_requests;
}

