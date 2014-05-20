/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2010 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2008 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"
#include "ompi/request/request.h"
#include "ompi/request/request_default.h"
#include "ompi/request/grequest.h"

#include "opal/runtime/opal_cr.h"
#include "ompi/mca/crcp/crcp.h"
#include "ompi/mca/pml/base/pml_base_request.h"


int ompi_request_default_wait(
    ompi_request_t ** req_ptr,
    ompi_status_public_t * status)
{
    ompi_request_t *req = *req_ptr;

    ompi_request_wait_completion(req);

#if OPAL_ENABLE_FT_CR == 1
    OMPI_CRCP_REQUEST_COMPLETE(req);
#endif
        
    /* return status.  If it's a generalized request, we *have* to
       invoke the query_fn, even if the user procided STATUS_IGNORE.
       MPI-2:8.2. */
    if (OMPI_REQUEST_GEN == req->req_type) {
        ompi_grequest_invoke_query(req, &req->req_status);
    }
    if( MPI_STATUS_IGNORE != status ) {
        /* Do *NOT* set status->MPI_ERROR here!  See MPI-1.1 doc, sec
           3.2.5, p.22 */
        status->MPI_TAG    = req->req_status.MPI_TAG;
        status->MPI_SOURCE = req->req_status.MPI_SOURCE;
        OMPI_STATUS_SET_COUNT(&status->_ucount, &req->req_status._ucount);
        status->_cancelled = req->req_status._cancelled;
    }
    if( req->req_persistent ) {
        if( req->req_state == OMPI_REQUEST_INACTIVE ) {
            return OMPI_SUCCESS;
        }
        req->req_state = OMPI_REQUEST_INACTIVE;
        return req->req_status.MPI_ERROR;
    }

    /* If there was an error, don't free the request -- just return
       the single error. */
    if (MPI_SUCCESS != req->req_status.MPI_ERROR) {
        return req->req_status.MPI_ERROR;
    }

    /* If there's an error while freeing the request, assume that the
       request is still there.  Otherwise, Bad Things will happen
       later! */
    return ompi_request_free(req_ptr);
}


int ompi_request_default_wait_any(
    size_t count,
    ompi_request_t ** requests,
    int *index,
    ompi_status_public_t * status)
{
#if OPAL_ENABLE_PROGRESS_THREADS
    int c;
#endif
    size_t i=0, num_requests_null_inactive=0;
    int rc = OMPI_SUCCESS;
    int completed = -1;
    ompi_request_t **rptr=NULL;
    ompi_request_t *request=NULL;

#if OPAL_ENABLE_PROGRESS_THREADS
    /* poll for completion */
    OPAL_THREAD_ADD32(&opal_progress_thread_count,1);
    for (c = 0; completed < 0 && c < opal_progress_spin_count; c++) {
        rptr = requests;
        num_requests_null_inactive = 0;
        for (i = 0; i < count; i++, rptr++) {
            request = *rptr;
            /*
             * Check for null or completed persistent request.
             * For MPI_REQUEST_NULL, the req_state is always OMPI_REQUEST_INACTIVE
             */
            if( request->req_state == OMPI_REQUEST_INACTIVE ) {
                num_requests_null_inactive++;
                continue;
            }
            if (true == request->req_complete) {
                completed = i;
                OPAL_THREAD_ADD32(&opal_progress_thread_count,-1);
                goto finished;
            }
        }
        if( num_requests_null_inactive == count ) {
            OPAL_THREAD_ADD32(&opal_progress_thread_count,-1);
            goto finished;
        }
        opal_progress();
    }
    OPAL_THREAD_ADD32(&opal_progress_thread_count,-1);
#endif

    /* give up and sleep until completion */
    OPAL_THREAD_LOCK(&ompi_request_lock);
    ompi_request_waiting++;
    do {
        rptr = requests;
        num_requests_null_inactive = 0;
        for (i = 0; i < count; i++, rptr++) {
            request = *rptr;

            /* Sanity test */
            if( NULL == request) {
                continue;
            }

            /*
             * Check for null or completed persistent request.
             * For MPI_REQUEST_NULL, the req_state is always OMPI_REQUEST_INACTIVE.
             */
            if( request->req_state == OMPI_REQUEST_INACTIVE ) {
                num_requests_null_inactive++;
                continue;
            }
            if (request->req_complete == true) {
                completed = i;
                break;
            }
        }
        if(num_requests_null_inactive == count)
            break;
        if (completed < 0) {
            opal_condition_wait(&ompi_request_cond, &ompi_request_lock);
        }
    } while (completed < 0);
    ompi_request_waiting--;
    OPAL_THREAD_UNLOCK(&ompi_request_lock);

#if OPAL_ENABLE_PROGRESS_THREADS
finished:
#endif  /* OPAL_ENABLE_PROGRESS_THREADS */

    if(num_requests_null_inactive == count) {
        *index = MPI_UNDEFINED;
        if (MPI_STATUS_IGNORE != status) {
            OMPI_STATUS_SET(status, &ompi_status_empty);
        }
    } else {
        assert( true == request->req_complete );
        /* Per note above, we have to call gen request query_fn even
           if STATUS_IGNORE was provided */
        if (OMPI_REQUEST_GEN == request->req_type) {
            rc = ompi_grequest_invoke_query(request, &request->req_status);
        }
        if (MPI_STATUS_IGNORE != status) {
            /* Do *NOT* set status->MPI_ERROR here!  See MPI-1.1 doc,
               sec 3.2.5, p.22 */
            int old_error = status->MPI_ERROR;
            OMPI_STATUS_SET(status, &request->req_status);
            status->MPI_ERROR = old_error;
        }
        rc = request->req_status.MPI_ERROR;
        if( request->req_persistent ) {
            request->req_state = OMPI_REQUEST_INACTIVE;
        } else if (MPI_SUCCESS == rc) {
            /* Only free the request if there is no error on it */
            /* If there's an error while freeing the request,
               assume that the request is still there.  Otherwise,
               Bad Things will happen later! */
            rc = ompi_request_free(rptr);
        }
        *index = completed;
    }

#if OPAL_ENABLE_FT_CR == 1
    if( opal_cr_is_enabled) {
        rptr = requests;
        for (i = 0; i < count; i++, rptr++) {
            request = *rptr;
            if( true == request->req_complete) {
                OMPI_CRCP_REQUEST_COMPLETE(request);
            }
        }
    }
#endif

    return rc;
}


int ompi_request_default_wait_all( size_t count,
                           ompi_request_t ** requests,
                           ompi_status_public_t * statuses )
{
    size_t completed = 0, i;
    ompi_request_t **rptr;
    ompi_request_t *request;
    int mpi_error = OMPI_SUCCESS;

    rptr = requests;
    for (i = 0; i < count; i++) {
        request = *rptr++;
        if (request->req_complete == true) {
            completed++;
        }
    }

    /* if all requests have not completed -- defer acquiring lock
     * unless required
     */
    if (completed != count) {
        /*
         * acquire lock and test for completion - if all requests are
         * not completed pend on condition variable until a request
         * completes
         */
        OPAL_THREAD_LOCK(&ompi_request_lock);
        ompi_request_waiting++;
#if OPAL_HAVE_THREAD_SUPPORT
        /*
         * confirm the status of the pending requests. We have to do it before
         * taking the condition or otherwise we can miss some requests completion (the
         * one that happpens between our initial test and the aquisition of the lock).
         */
        rptr = requests;
        for( completed = i = 0; i < count; i++ ) {
            request = *rptr++;
            if (request->req_complete == true) {
                completed++;
            }
        }
#endif  /* OPAL_HAVE_THREAD_SUPPORT */
        while( completed != count ) {
            /* check number of pending requests */
            size_t start = ompi_request_completed;
            size_t pending = count - completed;
            /*
             * wait until at least pending requests complete
             */
            while (pending > ompi_request_completed - start) {
                opal_condition_wait(&ompi_request_cond, &ompi_request_lock);
            }
            /*
             * confirm that all pending operations have completed.
             */
            rptr = requests;
            for( completed = i = 0; i < count; i++ ) {
                request = *rptr++;
                if (request->req_complete == true) {
                    completed++;
                }
            }
        }
        ompi_request_waiting--;
        OPAL_THREAD_UNLOCK(&ompi_request_lock);
    }

#if OPAL_ENABLE_FT_CR == 1
    if( opal_cr_is_enabled) {
        rptr = requests;
        for (i = 0; i < count; i++, rptr++) {
            request = *rptr;
            if( true == request->req_complete) {
                OMPI_CRCP_REQUEST_COMPLETE(request);
            }
        }
    }
#endif

    rptr = requests;
    if (MPI_STATUSES_IGNORE != statuses) {
        /* fill out status and free request if required */
        for( i = 0; i < count; i++, rptr++ ) {
            request = *rptr;
            assert( true == request->req_complete );
            if (OMPI_REQUEST_GEN == request->req_type) {
                ompi_grequest_invoke_query(request, &request->req_status);
            }

            OMPI_STATUS_SET(&statuses[i], &request->req_status);

            if( request->req_persistent ) {
                request->req_state = OMPI_REQUEST_INACTIVE;
            } else {
                /* Only free the request if there is no error on it */
                if (MPI_SUCCESS == request->req_status.MPI_ERROR) {
                    /* If there's an error while freeing the request,
                       assume that the request is still there.
                       Otherwise, Bad Things will happen later! */
                    int tmp = ompi_request_free(rptr);
                    if (OMPI_SUCCESS != tmp) {
                        mpi_error = tmp;
                    }
                }
            }
            if( statuses[i].MPI_ERROR != OMPI_SUCCESS) {
                mpi_error = MPI_ERR_IN_STATUS;
            }
        }
    } else {
        /* free request if required */
        for( i = 0; i < count; i++, rptr++ ) {
            int rc;
            request = *rptr;

            assert( true == request->req_complete );
            /* Per note above, we have to call gen request query_fn
               even if STATUSES_IGNORE was provided */
            if (OMPI_REQUEST_GEN == request->req_type) {
                rc = ompi_grequest_invoke_query(request, &request->req_status);
            }
            if( request->req_state == OMPI_REQUEST_INACTIVE ) {
                rc = ompi_status_empty.MPI_ERROR;
            } else {
	        rc = request->req_status.MPI_ERROR;
            }
            if( request->req_persistent ) {
                request->req_state = OMPI_REQUEST_INACTIVE;
            } else if (MPI_SUCCESS == rc) {
                /* Only free the request if there is no error on it */
	        int tmp = ompi_request_free(rptr);
                if (OMPI_SUCCESS != tmp) {
                    mpi_error = tmp;
                }
            }
            if( rc != OMPI_SUCCESS) {
	        mpi_error = rc;
            }
	}
    }
    return mpi_error;
}


int ompi_request_default_wait_some(
    size_t count,
    ompi_request_t ** requests,
    int * outcount,
    int * indices,
    ompi_status_public_t * statuses)
{
#if OPAL_ENABLE_PROGRESS_THREADS
    int c;
#endif
    size_t i, num_requests_null_inactive=0, num_requests_done=0;
    int rc = MPI_SUCCESS;
    ompi_request_t **rptr=NULL;
    ompi_request_t *request=NULL;

    *outcount = 0;
    for (i = 0; i < count; i++){
        indices[i] = 0;
    }

#if OPAL_ENABLE_PROGRESS_THREADS
    /* poll for completion */
    OPAL_THREAD_ADD32(&opal_progress_thread_count,1);
    for (c = 0; c < opal_progress_spin_count; c++) {
        rptr = requests;
        num_requests_null_inactive = 0;
        num_requests_done = 0;
        for (i = 0; i < count; i++, rptr++) {
            request = *rptr;
            /*
             * Check for null or completed persistent request.
             * For MPI_REQUEST_NULL, the req_state is always OMPI_REQUEST_INACTIVE
             */
            if (request->req_state == OMPI_REQUEST_INACTIVE ) {
                num_requests_null_inactive++;
                continue;
            }
            if (true == request->req_complete) {
                indices[i] = 1;
                num_requests_done++;
            }
        }
        if (num_requests_null_inactive == count ||
            num_requests_done > 0) {
            OPAL_THREAD_ADD32(&opal_progress_thread_count,-1);
            goto finished;
        }
        opal_progress();
    }
    OPAL_THREAD_ADD32(&opal_progress_thread_count,-1);
#endif

    /*
     * We only get here when outcount still is 0.
     * give up and sleep until completion
     */
    OPAL_THREAD_LOCK(&ompi_request_lock);
    ompi_request_waiting++;
    do {
        rptr = requests;
        num_requests_null_inactive = 0;
        num_requests_done = 0;
        for (i = 0; i < count; i++, rptr++) {
            request = *rptr;
            /*
             * Check for null or completed persistent request.
             * For MPI_REQUEST_NULL, the req_state is always OMPI_REQUEST_INACTIVE.
             */
            if( request->req_state == OMPI_REQUEST_INACTIVE ) {
                num_requests_null_inactive++;
                continue;
            }
            if (request->req_complete == true) {
                indices[i] = 1;
                num_requests_done++;
            }
        }
        if (num_requests_null_inactive == count ||
            num_requests_done > 0)
            break;
        opal_condition_wait(&ompi_request_cond, &ompi_request_lock);
    } while (1);
    ompi_request_waiting--;
    OPAL_THREAD_UNLOCK(&ompi_request_lock);

#if OPAL_ENABLE_PROGRESS_THREADS
finished:
#endif  /* OPAL_ENABLE_PROGRESS_THREADS */

#if OPAL_ENABLE_FT_CR == 1
    if( opal_cr_is_enabled) {
        rptr = requests;
        for (i = 0; i < count; i++, rptr++) {
            request = *rptr;
            if( true == request->req_complete) {
                OMPI_CRCP_REQUEST_COMPLETE(request);
            }
        }
    }
#endif

    if(num_requests_null_inactive == count) {
        *outcount = MPI_UNDEFINED;
    } else {
        /*
         * Compress the index array.
         */
        for (i = 0, num_requests_done = 0; i < count; i++) {
            if (0 != indices[i]) {
                indices[num_requests_done++] = i;
            }
        }

        *outcount = num_requests_done;

        for (i = 0; i < num_requests_done; i++) {
            request = requests[indices[i]];
            assert( true == request->req_complete );
            /* return status */
            /* Per note above, we have to call gen request query_fn even
               if STATUS_IGNORE was provided */
            if (OMPI_REQUEST_GEN == request->req_type) {
                ompi_grequest_invoke_query(request, &request->req_status);
            }
            if (MPI_STATUSES_IGNORE != statuses) {
                OMPI_STATUS_SET(&statuses[i], &request->req_status);
            }

            if (MPI_SUCCESS != request->req_status.MPI_ERROR) {
                rc = MPI_ERR_IN_STATUS;
            }

            if( request->req_persistent ) {
                request->req_state = OMPI_REQUEST_INACTIVE;
            } else {
                /* Only free the request if there was no error */
                if (MPI_SUCCESS == request->req_status.MPI_ERROR) {
                    int tmp;
                    tmp = ompi_request_free(&(requests[indices[i]]));
                    if (OMPI_SUCCESS != tmp) {
                        return tmp;
                    }
                }
            }
        }
    }
    return rc;
}
