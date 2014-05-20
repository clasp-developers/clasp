/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009-2010 Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 *
 * Top-level description of requests
 */

#ifndef OMPI_REQUEST_H
#define OMPI_REQUEST_H

#include "ompi_config.h"
#include "mpi.h"
#include "ompi/class/ompi_free_list.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/threads/condition.h"
#include "ompi/constants.h"

BEGIN_C_DECLS

/**
 * Request class
 */
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_request_t);

/*
 * The following include pulls in shared typedefs with debugger plugins.
 * For more information on why we do this see the Notice to developers 
 * comment at the top of the ompi_msgq_dll.c file.
 */

#include "request_dbg.h"

struct ompi_request_t;

/*
 * Required function to free the request and any associated resources.
 */
typedef int (*ompi_request_free_fn_t)(struct ompi_request_t** rptr);

/*
 * Optional function to cancel a pending request.
 */
typedef int (*ompi_request_cancel_fn_t)(struct ompi_request_t* request, int flag); 

/*
 * Optional function called when the request is completed from the MPI
 * library perspective. This function is not allowed to release any
 * ressources related to the request.
 */
typedef int (*ompi_request_complete_fn_t)(struct ompi_request_t* request);

/**
 * Forward declaration
 */
struct ompi_communicator_t;

/**
 * Forward declaration
 */
struct ompi_win_t;

/**
 * Forward declaration
 */
struct ompi_file_t;

/**
 * Union for holding several different MPI pointer types on the request
 */
typedef union ompi_mpi_object_t {
    struct ompi_communicator_t *comm;
    struct ompi_file_t *file;
    struct ompi_win_t *win;
} ompi_mpi_object_t;

/**
 * Main top-level request struct definition 
 */
struct ompi_request_t {
    ompi_free_list_item_t super;                /**< Base type */
    ompi_request_type_t req_type;               /**< Enum indicating the type of the request */
    ompi_status_public_t req_status;            /**< Completion status */
    volatile bool req_complete;                 /**< Flag indicating wether request has completed */
    volatile ompi_request_state_t req_state;    /**< enum indicate state of the request */
    bool req_persistent;                        /**< flag indicating if the this is a persistent request */
    int req_f_to_c_index;                       /**< Index in Fortran <-> C translation array */
    ompi_request_free_fn_t req_free;            /**< Called by free */
    ompi_request_cancel_fn_t req_cancel;        /**< Optional function to cancel the request */
    ompi_request_complete_fn_t req_complete_cb; /**< Called when the request is MPI completed */
    ompi_mpi_object_t req_mpi_object;           /**< Pointer to MPI object that created this request */
};

/**
 * Convenience typedef
 */
typedef struct ompi_request_t ompi_request_t;

/**
 * Padded struct to maintain back compatibiltiy.
 * See ompi/communicator/communicator.h comments with struct ompi_communicator_t
 * for full explanation why we chose the following padding construct for predefines.
 */
#define PREDEFINED_REQUEST_PAD (sizeof(void*) * 32)

struct ompi_predefined_request_t {
    struct ompi_request_t request;
    char padding[PREDEFINED_REQUEST_PAD - sizeof(ompi_request_t)];
};

typedef struct ompi_predefined_request_t ompi_predefined_request_t;

/**
 * Initialize a request.  This is a macro to avoid function call
 * overhead, since this is typically invoked in the critical
 * performance path (since requests may be re-used, it is possible
 * that we will have to initialize a request multiple times).
 */
#define OMPI_REQUEST_INIT(request, persistent)        \
    do {                                              \
        (request)->req_complete = false;              \
        (request)->req_state = OMPI_REQUEST_INACTIVE; \
        (request)->req_persistent = (persistent);     \
    } while (0); 

/**
 * Finalize a request.  This is a macro to avoid function call
 * overhead, since this is typically invoked in the critical
 * performance path (since requests may be re-used, it is possible
 * that we will have to finalize a request multiple times).
 *
 * When finalizing a request, if MPI_Request_f2c() was previously
 * invoked on that request, then this request was added to the f2c
 * table, and we need to remove it 
 *
 * This function should be called only from the MPI layer. It should
 * never be called from the PML. It take care of the upper level clean-up.
 * When the user call MPI_Request_free we should release all MPI level
 * ressources, so we have to call this function too.
 */
#define OMPI_REQUEST_FINI(request)                                      \
do {                                                                    \
    (request)->req_state = OMPI_REQUEST_INVALID;                        \
    if (MPI_UNDEFINED != (request)->req_f_to_c_index) {                 \
        opal_pointer_array_set_item(&ompi_request_f_to_c_table,         \
                                    (request)->req_f_to_c_index, NULL); \
        (request)->req_f_to_c_index = MPI_UNDEFINED;                    \
    }                                                                   \
} while (0); 

/**
 * Non-blocking test for request completion.
 *
 * @param request (IN)   Array of requests
 * @param complete (OUT) Flag indicating if index is valid (a request completed).
 * @param status (OUT)   Status of completed request.
 * @return               OMPI_SUCCESS or failure status.
 *
 * Note that upon completion, the request is freed, and the
 * request handle at index set to NULL.
 */
typedef int (*ompi_request_test_fn_t)(ompi_request_t ** rptr,
                                      int *completed,
                                      ompi_status_public_t * status );
/**
 * Non-blocking test for request completion.
 *
 * @param count (IN)     Number of requests
 * @param request (IN)   Array of requests
 * @param index (OUT)    Index of first completed request.
 * @param complete (OUT) Flag indicating if index is valid (a request completed).
 * @param status (OUT)   Status of completed request.
 * @return               OMPI_SUCCESS or failure status.
 *
 * Note that upon completion, the request is freed, and the
 * request handle at index set to NULL.
 */
typedef int (*ompi_request_test_any_fn_t)(size_t count,
                                          ompi_request_t ** requests,
                                          int *index,
                                          int *completed,
                                          ompi_status_public_t * status);
/**
 * Non-blocking test for request completion.
 *
 * @param count (IN)      Number of requests
 * @param requests (IN)   Array of requests
 * @param completed (OUT) Flag indicating wether all requests completed.
 * @param statuses (OUT)  Array of completion statuses.
 * @return                OMPI_SUCCESS or failure status.
 *
 * This routine returns completed==true if all requests have completed.
 * The statuses parameter is only updated if all requests completed. Likewise,
 * the requests array is not modified (no requests freed), unless all requests
 * have completed.
 */
typedef int (*ompi_request_test_all_fn_t)(size_t count,
                                          ompi_request_t ** requests,
                                          int *completed,
                                          ompi_status_public_t * statuses);
/**
 * Non-blocking test for some of N requests to complete.
 *
 * @param count (IN)        Number of requests
 * @param requests (INOUT)  Array of requests
 * @param outcount (OUT)    Number of finished requests
 * @param indices (OUT)     Indices of the finished requests
 * @param statuses (OUT)    Array of completion statuses.
 * @return                  OMPI_SUCCESS, OMPI_ERR_IN_STATUS or failure status.
 *
 */
typedef int (*ompi_request_test_some_fn_t)(size_t count,
                                           ompi_request_t ** requests,
                                           int * outcount,
                                           int * indices,
                                           ompi_status_public_t * statuses);
/**
 * Wait (blocking-mode) for one requests to complete.
 *
 * @param request (IN)    Pointer to request.
 * @param status (OUT)    Status of completed request.
 * @return                OMPI_SUCCESS or failure status.
 *
 */
typedef int (*ompi_request_wait_fn_t)(ompi_request_t ** req_ptr,
                                      ompi_status_public_t * status);
/**
 * Wait (blocking-mode) for one of N requests to complete.
 *
 * @param count (IN)      Number of requests
 * @param requests (IN)   Array of requests
 * @param index (OUT)     Index into request array of completed request.
 * @param status (OUT)    Status of completed request.
 * @return                OMPI_SUCCESS or failure status.
 *
 */
typedef int (*ompi_request_wait_any_fn_t)(size_t count,
                                          ompi_request_t ** requests,
                                          int *index,
                                          ompi_status_public_t * status);
/**
 * Wait (blocking-mode) for all of N requests to complete.
 *
 * @param count (IN)      Number of requests
 * @param requests (IN)   Array of requests
 * @param statuses (OUT)  Array of completion statuses.
 * @return                OMPI_SUCCESS or failure status.
 *
 */
typedef int (*ompi_request_wait_all_fn_t)(size_t count,
                                          ompi_request_t ** requests,
                                          ompi_status_public_t * statuses);
/**
 * Wait (blocking-mode) for some of N requests to complete.
 *
 * @param count (IN)        Number of requests
 * @param requests (INOUT)  Array of requests
 * @param outcount (OUT)    Number of finished requests
 * @param indices (OUT)     Indices of the finished requests
 * @param statuses (OUT)    Array of completion statuses.
 * @return                  OMPI_SUCCESS, OMPI_ERR_IN_STATUS or failure status.
 *
 */
typedef int (*ompi_request_wait_some_fn_t)(size_t count,
                                           ompi_request_t ** requests,
                                           int * outcount,
                                           int * indices,
                                           ompi_status_public_t * statuses);

/**
 * Replaceable request functions
 */
typedef struct ompi_request_fns_t {
    ompi_request_test_fn_t      req_test;
    ompi_request_test_any_fn_t  req_test_any;
    ompi_request_test_all_fn_t  req_test_all;
    ompi_request_test_some_fn_t req_test_some;
    ompi_request_wait_fn_t      req_wait;
    ompi_request_wait_any_fn_t  req_wait_any;
    ompi_request_wait_all_fn_t  req_wait_all;
    ompi_request_wait_some_fn_t req_wait_some;
} ompi_request_fns_t;

/**
 * Globals used for tracking requests and request completion.
 */
OMPI_DECLSPEC extern opal_pointer_array_t  ompi_request_f_to_c_table;
OMPI_DECLSPEC extern size_t                ompi_request_waiting;
OMPI_DECLSPEC extern size_t                ompi_request_completed;
OMPI_DECLSPEC extern int32_t               ompi_request_poll;
OMPI_DECLSPEC extern opal_mutex_t          ompi_request_lock;
OMPI_DECLSPEC extern opal_condition_t      ompi_request_cond;
OMPI_DECLSPEC extern ompi_predefined_request_t        ompi_request_null;
OMPI_DECLSPEC extern ompi_request_t        ompi_request_empty;
OMPI_DECLSPEC extern ompi_status_public_t  ompi_status_empty;
OMPI_DECLSPEC extern ompi_request_fns_t    ompi_request_functions;

/**
 * Initialize the MPI_Request subsystem; invoked during MPI_INIT.
 */
int ompi_request_init(void);

/**
 * Free a persistent request to a MPI_PROC_NULL peer (there's no
 * freelist to put it back to, so we have to actually OBJ_RELEASE it).
 */
OMPI_DECLSPEC int ompi_request_persistent_proc_null_free(ompi_request_t **request);

/**
 * Shut down the MPI_Request subsystem; invoked during MPI_FINALIZE.
 */
int ompi_request_finalize(void);

/**
 * Cancel a pending request.
 */
static inline int ompi_request_cancel(ompi_request_t* request)
{
    if (request->req_cancel != NULL) {
        return request->req_cancel(request, true);
    }
    return OMPI_SUCCESS;
}

/**
 * Free a request.
 *
 * @param request (INOUT)   Pointer to request.
 */
static inline int ompi_request_free(ompi_request_t** request)
{
    return (*request)->req_free(request);
}

#define ompi_request_test       (ompi_request_functions.req_test)
#define ompi_request_test_any   (ompi_request_functions.req_test_any)
#define ompi_request_test_all   (ompi_request_functions.req_test_all)
#define ompi_request_test_some  (ompi_request_functions.req_test_some)
#define ompi_request_wait       (ompi_request_functions.req_wait)
#define ompi_request_wait_any   (ompi_request_functions.req_wait_any)
#define ompi_request_wait_all   (ompi_request_functions.req_wait_all)
#define ompi_request_wait_some  (ompi_request_functions.req_wait_some)


/**
 * Wait a particular request for completion
 */
static inline void ompi_request_wait_completion(ompi_request_t *req)
{
    if(false == req->req_complete) {
#if OPAL_ENABLE_PROGRESS_THREADS
        if(opal_progress_spin(&req->req_complete)) {
            return;
        }
#endif
        OPAL_THREAD_LOCK(&ompi_request_lock);
        ompi_request_waiting++;
        while(false == req->req_complete) {
            opal_condition_wait(&ompi_request_cond, &ompi_request_lock);
        }
        ompi_request_waiting--;
        OPAL_THREAD_UNLOCK(&ompi_request_lock);
    }
}

/**
 *  Signal or mark a request as complete. If with_signal is true this will
 *  wake any thread pending on the request and ompi_request_lock should be
 *  held while calling this function. If with_signal is false, there will
 *  signal generated, and no lock required. This is a special case when
 *  the function is called from the critical path for small messages, where
 *  we know the current execution flow created the request, and is still
 *  in the _START macro.
 */
static inline int ompi_request_complete(ompi_request_t* request, bool with_signal)
{
    if( NULL != request->req_complete_cb ) {
        request->req_complete_cb( request );
    }
    ompi_request_completed++;
    request->req_complete = true;
    if(with_signal && ompi_request_waiting) {
        /* Broadcast the condition, otherwise if there is already a thread
         * waiting on another request it can use all signals.
         */
        opal_condition_broadcast(&ompi_request_cond);
    }
    return OMPI_SUCCESS;
}

/* In a 64-bit library with strict alignment requirements (like 64-bit
 * SPARC), the _ucount field of a C status is a long and requires 8
 * byte alignment.  Unfortunately a Fortran status is an array of 6
 * integers which only requires 4 byte alignment.  When storing the
 * length into a status we don't know whether it is a C or Fortran
 * status.  Therefore, we just copy the entire status as an integer
 * array to avoid any issues.  We supply one macro for doing the entire
 * status and another for just the _ucount field.  Note that these
 * macros are enabled on 64-bit SPARC platforms only.  This is because
 * an investigation into performance effects showed that keeping the
 * structure assignment code wherever possible resulted in the best
 * performance.  Details of the investigation into this issue are at
 * https://svn.open-mpi.org/trac/ompi/ticket/2526
 */
#if defined(__sparc) && SIZEOF_SIZE_T == 8
#define OMPI_STATUS_SET(outstat, instat)                                          \
    do {                                                                          \
        if (((ulong)(outstat)) & 0x7) {                                           \
            int _i;                                                               \
            for(_i=0; _i<(int)(sizeof(ompi_status_public_t)/sizeof(int)); _i++) { \
                ((int *)(outstat))[_i] = ((int *)(instat))[_i];                   \
            }                                                                     \
        } else {                                                                  \
            *(outstat) = *(instat);                                               \
        }                                                                         \
    } while(0)
#define OMPI_STATUS_SET_COUNT(outcount, incount)             \
    do {                                                     \
        if (((ulong)(outcount)) & 0x7) {                     \
            ((int *)(outcount))[0] = ((int *)(incount))[0];  \
            ((int *)(outcount))[1] = ((int *)(incount))[1];  \
        } else {                                             \
            *(outcount) = *(incount);                        \
        }                                                    \
    } while(0)
#else
#define OMPI_STATUS_SET(outstat, instat) (*(outstat) = *(instat))
#define OMPI_STATUS_SET_COUNT(outcount, incount) (*(outcount) = *(incount))
#endif

END_C_DECLS

#endif
