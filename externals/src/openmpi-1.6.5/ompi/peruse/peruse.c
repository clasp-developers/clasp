/*
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#ifdef HAVE_STRING_H
#  include <string.h>
#endif
#include "mpi.h"
#include "ompi/peruse/peruse.h"
#include "ompi/peruse/peruse-internal.h"
#include "ompi/communicator/communicator.h"
#include "ompi/runtime/params.h"

/*
 * Data
 */

typedef struct {
    const char* name;
    const int   id;
} peruse_event_associations_t;

/**
 * The associations between the peruse event name and id. This array
 * should be ended by the tuple {NULL, PERUSE_CUSTOM_EVENT}.
 */
static const peruse_event_associations_t PERUSE_events[] = {
    /* Point-to-point request events */
    { "PERUSE_COMM_REQ_ACTIVATE", PERUSE_COMM_REQ_ACTIVATE },
    { "PERUSE_COMM_REQ_MATCH_UNEX", PERUSE_COMM_REQ_MATCH_UNEX },
    { "PERUSE_COMM_REQ_INSERT_IN_POSTED_Q", PERUSE_COMM_REQ_INSERT_IN_POSTED_Q },
    { "PERUSE_COMM_REQ_REMOVE_FROM_POSTED_Q", PERUSE_COMM_REQ_REMOVE_FROM_POSTED_Q },
    { "PERUSE_COMM_REQ_XFER_BEGIN", PERUSE_COMM_REQ_XFER_BEGIN },
    { "PERUSE_COMM_REQ_XFER_CONTINUE", PERUSE_COMM_REQ_XFER_CONTINUE },
    { "PERUSE_COMM_REQ_XFER_END", PERUSE_COMM_REQ_XFER_END },
    { "PERUSE_COMM_REQ_COMPLETE", PERUSE_COMM_REQ_COMPLETE },
    { "PERUSE_COMM_REQ_NOTIFY", PERUSE_COMM_REQ_NOTIFY },
    { "PERUSE_COMM_MSG_ARRIVED", PERUSE_COMM_MSG_ARRIVED },
    { "PERUSE_COMM_MSG_INSERT_IN_UNEX_Q", PERUSE_COMM_MSG_INSERT_IN_UNEX_Q },
    { "PERUSE_COMM_MSG_REMOVE_FROM_UNEX_Q", PERUSE_COMM_MSG_REMOVE_FROM_UNEX_Q },
    { "PERUSE_COMM_MSG_MATCH_POSTED_REQ", PERUSE_COMM_MSG_MATCH_POSTED_REQ },

    /* Queue events*/
    { "PERUSE_COMM_SEARCH_POSTED_Q_BEGIN", PERUSE_COMM_SEARCH_POSTED_Q_BEGIN },
    { "PERUSE_COMM_SEARCH_POSTED_Q_END", PERUSE_COMM_SEARCH_POSTED_Q_END },
    { "PERUSE_COMM_SEARCH_UNEX_Q_BEGIN", PERUSE_COMM_SEARCH_UNEX_Q_BEGIN },
    { "PERUSE_COMM_SEARCH_UNEX_Q_END", PERUSE_COMM_SEARCH_UNEX_Q_END },
    { "PERUSE_CUSTOM_EVENT", PERUSE_CUSTOM_EVENT }
};

const int PERUSE_num_events = (sizeof(PERUSE_events) / sizeof(peruse_event_associations_t));

/*
 * PERUSE user-callable function
 */
int PERUSE_Init (void)
{
    if (MPI_PARAM_CHECK) {
        if (!ompi_mpi_initialized || ompi_mpi_finalized)
            return PERUSE_ERR_INIT;
    }
    ompi_peruse_init ();
    return PERUSE_SUCCESS;
}


/* Query all implemented events */
int PERUSE_Query_supported_events (int* num_supported,
                                   char*** event_names,
                                   int** events)
{
    int i;
    *num_supported = PERUSE_num_events;

    *event_names = (char**) malloc (PERUSE_num_events * sizeof (char *));
    *events = (int*) malloc (PERUSE_num_events * sizeof (int));

    for (i = 0; i < PERUSE_num_events; i++) {
        (*event_names)[i] = strdup (PERUSE_events[i].name);
        (*events)[i] = PERUSE_events[i].id;
    }

    return PERUSE_SUCCESS;
}


/* Query supported events */
int PERUSE_Query_event (const char* event_name, int* event)
{
    int i;

    for( i = 0; i < PERUSE_num_events; i++ ) {
        if( !strcmp (event_name, PERUSE_events[i].name) ) {
            *event = PERUSE_events[i].id;
            return PERUSE_SUCCESS;
        }
    }
    return PERUSE_ERR_EVENT;
}


/* Query event name */
int PERUSE_Query_event_name (int event, char** event_name)
{
    if (event < 0 || event > PERUSE_num_events ||
        NULL == PERUSE_events[event].name)
        return PERUSE_EVENT_INVALID;

    *event_name = strdup(PERUSE_events[event].name);
    return PERUSE_SUCCESS;
}


/* Get environment variables that affect MPI library behavior */
int PERUSE_Query_environment (int * env_size, char *** env)
{
    /* XXX tbd */
    return PERUSE_SUCCESS;
}

/* Query the scope of queue metrics - global or per communicator */
int PERUSE_Query_queue_event_scope (int * scope)
{
    *scope = PERUSE_PER_COMM;

    return PERUSE_SUCCESS;
}


/*
 * II. Events, objects initialization and manipulation
 */
/* Initialize event associated with an MPI communicator */
int PERUSE_Event_comm_register (int                       event,
                                MPI_Comm                  comm,
                                peruse_comm_callback_f *  callback_fn,
                                void *                    param,
                                peruse_event_h *          event_h)
{
    ompi_peruse_handle_t * handle;
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_PERUSE_INIT_FINALIZE;

        if( (event < 0) || (event > PERUSE_num_events) ||
            (NULL == PERUSE_events[event].name) )
            return PERUSE_ERR_EVENT;

        if( (MPI_COMM_NULL == comm) || ompi_comm_invalid (comm) )
            return PERUSE_ERR_COMM;

        if (NULL == callback_fn)
            return PERUSE_ERR_GENERIC;

        if (NULL == event_h)
            return PERUSE_ERR_EVENT_HANDLE;
    }

    handle = OBJ_NEW (ompi_peruse_handle_t);

    /*
     * Initialize the newly created handle to the default inactive state.
     */
    handle->active = 0;
    handle->event = event;
    handle->type = PERUSE_TYPE_COMM;
    handle->comm = comm;
    handle->fn = (ompi_peruse_callback_f*) callback_fn;
    handle->param = param;

    /*
     * Update the information on the handle on the communicator
     */
    OPAL_THREAD_LOCK (&comm->c_lock);
    if( NULL == comm->c_peruse_handles ) {
        comm->c_peruse_handles = (ompi_peruse_handle_t**)calloc( PERUSE_num_events, sizeof(ompi_peruse_handle_t*) );
    }
    OPAL_THREAD_UNLOCK (&comm->c_lock);
    comm->c_peruse_handles[event] = handle;

    *event_h = handle;
    return PERUSE_SUCCESS;
}


/* Start collecting data (activate event) */
int PERUSE_Event_activate (peruse_event_h event_h)
{
    ompi_peruse_handle_t* handle = (ompi_peruse_handle_t*)event_h;

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_PERUSE_INIT_FINALIZE;

        if (PERUSE_EVENT_HANDLE_NULL == event_h)
            return PERUSE_ERR_EVENT_HANDLE;
    }

    OPAL_THREAD_LOCK (&handle->lock);
    handle->active = 1;
    OPAL_THREAD_UNLOCK (&handle->lock);
    return PERUSE_SUCCESS;
}


/* Stop collecting data (deactivate event) */
int PERUSE_Event_deactivate (peruse_event_h event_h)
{
    ompi_peruse_handle_t* handle = (ompi_peruse_handle_t*)event_h;

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_PERUSE_INIT_FINALIZE;

        if (PERUSE_EVENT_HANDLE_NULL == event_h)
            return PERUSE_ERR_EVENT_HANDLE;
    }

    OPAL_THREAD_LOCK (&handle->lock);
    handle->active = 0;
    OPAL_THREAD_UNLOCK (&handle->lock);
    return PERUSE_SUCCESS;
}


/* Free event handle */
int PERUSE_Event_release (peruse_event_h * event_h)
{
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_PERUSE_INIT_FINALIZE;

        if (PERUSE_EVENT_HANDLE_NULL == event_h)
            return PERUSE_ERR_EVENT_HANDLE;
    }
    /*
     * XXX
     */
    *event_h = PERUSE_EVENT_HANDLE_NULL;
    return PERUSE_SUCCESS;
}

#define PERUSE_MPI_PARAM_CHECK(obj_upper,obj_lower )                  \
    if (MPI_PARAM_CHECK) {                                            \
        OMPI_ERR_PERUSE_INIT_FINALIZE;                                \
                                                                      \
        if (PERUSE_EVENT_HANDLE_NULL == event_h ||                    \
            ((ompi_peruse_handle_t*)event_h)->active ||               \
            ((ompi_peruse_handle_t*)event_h)->type !=                 \
                PERUSE_TYPE_ ## obj_upper)                            \
            return PERUSE_ERR_EVENT_HANDLE;                           \
                                                                      \
        if (NULL == callback_fn)                                      \
            return PERUSE_ERR_PARAMETER;                              \
        /*                                                            \
         * XXX whether the underlying MPI-object has been freed!??    \
        if (ompi_ ## obj_lower ## _invalid (                          \
              ((ompi_peruse_handle_t*)event_h)->obj_lower))           \
            return PERUSE_ERR_MPI_OBJECT;                             \
         */                                                           \
    }                                                                 \

/* Set a new comm callback */
int PERUSE_Event_comm_callback_set (peruse_event_h           event_h,
                                    peruse_comm_callback_f*  callback_fn,
                                    void*                    param)
{
    ompi_peruse_handle_t* handle = (ompi_peruse_handle_t*)event_h;

    PERUSE_MPI_PARAM_CHECK (COMM, comm);

    OPAL_THREAD_LOCK (&handle->lock);
    handle->fn = (ompi_peruse_callback_f*) callback_fn;
    handle->param = param;
    OPAL_THREAD_UNLOCK (&handle->lock);

    return PERUSE_SUCCESS;
}

/* Get the current comm callback */
int PERUSE_Event_comm_callback_get (peruse_event_h           event_h,
                                    peruse_comm_callback_f** callback_fn,
                                    void**                   param)
{
    ompi_peruse_handle_t* handle = (ompi_peruse_handle_t*)event_h;

    PERUSE_MPI_PARAM_CHECK (COMM, comm);

    OPAL_THREAD_LOCK (&handle->lock);
    *callback_fn = (peruse_comm_callback_f*) handle->fn;
    *param = handle->param;
    OPAL_THREAD_UNLOCK (&handle->lock);

    return PERUSE_SUCCESS;
}

/* Obtain event descriptor from an event handle (reverse lookup) */
int PERUSE_Event_get (peruse_event_h event_h, int* event)
{
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_PERUSE_INIT_FINALIZE;

        if (NULL == event_h)
            return PERUSE_ERR_EVENT_HANDLE;

        if (NULL == event)
            return PERUSE_ERR_PARAMETER;
    }

    *event = ((ompi_peruse_handle_t*)event_h)->event;
    return PERUSE_SUCCESS;
}


/* Obtain MPI object associated with event handle */
int PERUSE_Event_object_get (peruse_event_h event_h, void** mpi_object)
{
    ompi_peruse_handle_t* p = (ompi_peruse_handle_t*)event_h;

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_PERUSE_INIT_FINALIZE;

        if (NULL == event_h)
            return PERUSE_ERR_EVENT_HANDLE;

        if (NULL == mpi_object)
            return PERUSE_ERR_PARAMETER;
    }

    switch (p->type) {
        case PERUSE_TYPE_COMM:
            *mpi_object = p->comm;
            return PERUSE_SUCCESS;
    }
    return PERUSE_ERR_GENERIC;
}


/* Propagation mode */
int PERUSE_Event_propagate (peruse_event_h event_h, int mode)
{
    return PERUSE_SUCCESS;
}
