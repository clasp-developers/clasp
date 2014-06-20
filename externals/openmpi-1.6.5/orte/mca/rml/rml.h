/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** 
 * @file
 *
 * Runtime Messaging Layer (RML) Communication Interface
 *
 * The Runtime Messaging Layer (RML) provices basic point-to-point
 * communication between ORTE processes.  The system is available for
 * most architectures, with some exceptions (the Cray XT3/XT4, for example).
 */


#ifndef ORTE_MCA_RML_RML_H_
#define ORTE_MCA_RML_RML_H_

#include "orte_config.h"
#include "orte/types.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

#include "orte/mca/rml/rml_types.h"

BEGIN_C_DECLS


/* ******************************************************************** */


struct opal_buffer_t;
struct orte_process_name_t;
struct orte_rml_module_t;


/* ******************************************************************** */


/**
 * RML component initialization 
 *
 * Create an instance (module) of the given RML component.  Upon
 * returning, the module data structure should be fully populated and
 * all functions should be usable.  Non-blocking receive calls may be
 * posted upon return from this function, although communication need
 * not be enabled until enable_comm() call is called on the module.
 *
 * @return Exactly one module created by the call to the component's
 * initialization function should be returned.  The module structure
 * should be fully populated, and the priority should be set to a
 * reasonable value.
 *
 * @param[out] priority Selection priority for the given component
 *
 * @retval NULL An error occurred and initialization did not occur
 * @retval non-NULL The module was successfully initialized
 */
typedef struct orte_rml_module_t* (*orte_rml_component_init_fn_t)(int  *priority);


/**
 * RML component interface
 *
 * Component interface for the RML framework.  A public instance of
 * this structure, called mca_rml_[component name]_component, must
 * exist in any RML component.
 */
struct orte_rml_component_2_0_0_t {
    /* Base component description */
    mca_base_component_t rml_version;
    /* Base component data block */
    mca_base_component_data_t rml_data;
    /* Component intialization function */
    orte_rml_component_init_fn_t rml_init;
};
/** Convienence typedef */
typedef struct orte_rml_component_2_0_0_t orte_rml_component_t;


/* ******************************************************************** */


/**
 * Funtion prototype for callback from non-blocking iovec send and receive
 *
 * Funtion prototype for callback from non-blocking iovec send and
 * receive.  The iovec pointer will be the same pointer passed to
 * either send_nb and recv_nb.  The peer memory location may not be
 * the same, and is owned by the RML, not the calling process.
 *
 * @note The parameter in/out parameters are relative to the user's callback
 * function.
 *
 * @param[in] status  Completion status - equivalent to the return value
 *                    from blocking send/recv
 * @param[in] peer    Opaque name of peer process
 * @param[in] msg     Array of iovecs describing user buffers and lengths
 * @param[in] count   Number of elements in iovec array
 * @param[in] tag     User defined tag for matching send/recv
 * @param[in] cbdata  User data passed to send_nb() or recv_nb()
 */
typedef void (*orte_rml_callback_fn_t)(int status,
                                       struct orte_process_name_t* peer,
                                       struct iovec* msg,
                                       int count,
                                       orte_rml_tag_t tag,
                                       void* cbdata);


/**
 * Funtion prototype for callback from non-blocking buffer send and receive
 *
 * Funtion prototype for callback from non-blocking buffer send and
 * receive.  The buffer may not be the same pointer passed to either
 * send_buffer_nb and recv_buffer_nb.  The peer memory location may
 * not be the same, and is owned by the RML, not the calling process.
 *
 * @note The parameter in/out parameters are relative to the user's callback
 * function.
 *
 * @param[in] status  Completion status - equivalent to the return value
 *                    from blocking send/recv
 * @param[in] peer    Name of peer process
 * @param[in] buffer  Message buffer
 * @param[in] tag     User defined tag for matching send/recv
 * @param[in] cbdata  User data passed to send_nb() or recv_nb()
 */
typedef void (*orte_rml_buffer_callback_fn_t)(int status,
                                              struct orte_process_name_t* peer,
                                              struct opal_buffer_t* buffer,
                                              orte_rml_tag_t tag,
                                              void* cbdata);


/**
 * Function prototype for exception callback
 *
 * Function prototype for callback triggered when a communication error is detected.
 *
 * @note The parameter in/out parameters are relative to the user's callback
 * function.
 *
 * @param[in] peer      Name of peer process
 * @param[in] exception Description of the error causing the exception
 */
typedef void (*orte_rml_exception_callback_t)(const orte_process_name_t* peer,
                                              orte_rml_exception_t exception);


/* ******************************************************************** */


/**
 * Enable communication using the RML module
 *
 * Enable communication using the RML module.  Before this call, only
 * the non-blocking receive and ping interfaces may be used.  After
 * this call returns, the module must be fully functional, capable of
 * sending and receiving data.  This function will be called after the
 * process has been assigned a proces identifier.
 *
 * @note While the ping interface may be used between the call to the
 * component's initialization function and this call, care must be
 * taken when doing so.  The remote process must have already called
 * enable_comm() or the remote process will not reply to the ping.
 * As the ping interface is generally used by MPI processes to find a
 * daemon to contact, this should not be a major limitation.
 *
 * @retval ORTE_SUCCESS Communications successfully enabled
 * @retval ORTE_ERROR   An unspecified error occurred
 */
typedef int (*orte_rml_module_enable_comm_fn_t)(void);


/**
 * Finalize the RML module
 *
 * Finalize the RML module, ending all communication and cleaning up
 * all resources associated with the module.  After the finalize
 * function is called, all interface functions (and the module
 * structure itself) are not available for use.
 *
 * @note Whether or not the finalize function returns successfully,
 * the module should not be used once this function is called.
 *
 * @retval ORTE_SUCCESS Success
 * @retval ORTE_ERROR   An unspecified error occurred
 */
typedef int (*orte_rml_module_finalize_fn_t)(void);


/**
 * Get a "contact info" string for the local process
 *
 * Get a "contact info" string that can be used by other processes to
 * share the contact information for the given process.  The "contact
 * info" string includes the process identifier for the given process
 * and uses only basic ascii characters.  It should be quoted when
 * evaluated by a shell, although no special escaping is necessary.
 *
 * @note The function may return a contact info string which contains
 * multiple addresses.
 *
 * @retval non-NULL The contact information for this process
 * @retval NULL     An error occurred when trying to get the current
 *                  process contact info
 */
typedef char* (*orte_rml_module_get_contact_info_fn_t)(void);


/**
 * Update the RML with a remote process's contact info
 *
 * Update the RML with a remote process's contact information, as
 * returned from the get_contact_info() function on the remote
 * process.  Before a send can be initiated to a remote process,
 * either this function must be called for that process or that
 * process must have already established a connection to the local
 * process.
 *
 * @note The user may not always explicitly call this function
 * directly, but may instead cause it to be called through one of the
 * contact setup functions available in
 * orte/mca/rml/base/rml_contact.h.
 *
 * @param[in] contact_info The contact information string of a peer
 *
 * @retval ORTE_SUCCESS The contact information was successfully updated
 * @retval ORTE_ERROR   An unspecified error occurred during the update
 */
typedef int (*orte_rml_module_set_contact_info_fn_t)(const char *contact_info);


/**
 * Request a new name from the HNP, if one is not already assigned
 *
 * Request a new name from the HNP, if one is not already assigned.
 * This function should be avoided at all costs, but is unavoidable in
 * some instances (like when trying to get a name from a singleton or 
 * when trying to contact a persistent daemon from an orte tool.
 *
 * @param[out] name The new name of the process
 *
 * @retval ORTE_SUCCESS A name was successfully acquired
 * @retcal ORTE_ERR_NOT_SUPPORTED A name is already assigned to the
 *                      current process
 * @retval ORTE_ERROR   An unspecified error occurred
 */
typedef int (*orte_rml_module_get_new_name_fn_t)(orte_process_name_t *name);


/**
 * "Ping" another process to determine availability
 *
 * Ping another process to determine if it is available.  This
 * function only verifies that the process is alive and will allow a
 * connection to the local process.  It does *not* qualify as
 * establishing communication with the remote process, as required by
 * the note for set_contact_info().
 *
 * @param[in] contact_info The contact info string for the remote process
 * @param[in] tv           Timeout after which the ping should be failed
 *
 * @retval ORTE_SUCESS The process is available and will allow connections 
 *                     from the local process
 * @retval ORTE_ERROR  An unspecified error occurred during the update
 */
typedef int (*orte_rml_module_ping_fn_t)(const char* contact_info,
                                         const struct timeval* tv);


/**
 * Send an iovec blocking message
 *
 * Send an iovec blocking message to the specified peer.  The call
 * will return when the buffer may be modified.  The return of a call
 * to send() does not give any indication of remote completion.
 *
 * @param[in] peer   Name of receiving process
 * @param[in] msg    iovec array describing send buffer
 * @param[in] count  Length of iovec array
 * @param[in] tag    User defined tag for matching send/recv
 * @param[in] flags  Currently unused.
 *
 * @retval >0        Number of bytes successfully sent (will be 
 *                   length of all iovecs)
 * @retval ORTE_ERR_BAD_PARAM One of the parameters was invalid
 * @retval ORTE_ERR_ADDRESSEE_UNKNOWN Contact information for the
 *                    receiving process is not available
 * @retval ORTE_ERROR  An unspecified error occurred
 */
typedef int (*orte_rml_module_send_fn_t)(struct orte_process_name_t* peer,
                                         struct iovec *msg,
                                         int count,
                                         int tag,
                                         int flags);


/**
 * Send a buffer blocking message
 *
 * Send a buffer blocking message to the specified peer.  The call
 * will return when the buffer may be modified.  The return of a call
 * to send() does not give any indication of remote completion.
 *
 * @param[in] peer   Name of receiving process
 * @param[in] buffer send buffer
 * @param[in] tag    User defined tag for matching send/recv
 * @param[in] flags  Currently unused.
 *
 * @retval >0        Number of bytes successfully sent (will be 
 *                   length of buffer)
 * @retval ORTE_ERR_BAD_PARAM One of the parameters was invalid
 * @retval ORTE_ERR_ADDRESSEE_UNKNOWN Contact information for the
 *                    receiving process is not available
 * @retval ORTE_ERROR  An unspecified error occurred
 */
typedef int (*orte_rml_module_send_buffer_fn_t)(struct orte_process_name_t* peer,
                                                struct opal_buffer_t* buffer,
                                                orte_rml_tag_t tag,
                                                int flags);


/**
 * Send an iovec non-blocking message
 *
 * Send an iovec blocking message to the specified peer.  The call
 * will return immediately, although the iovec may not be modified
 * until the completion callback is triggered.  The iovec *may* be
 * passed to another call to send_nb before the completion callback is
 * triggered.  The callback being triggered does not give any
 * indication of remote completion.
 *
 * @param[in] peer   Name of receiving process
 * @param[in] msg    iovec array describing send buffer
 * @param[in] count  Length of iovec array
 * @param[in] tag    User defined tag for matching send/recv
 * @param[in] flags  Currently unused
 * @param[in] cbfunc Callback function on message comlpetion
 * @param[in] cbdata User data to provide during completion callback
 *
 * @retval ORTE_SUCCESS The message was successfully started
 * @retval ORTE_ERR_BAD_PARAM One of the parameters was invalid
 * @retval ORTE_ERR_ADDRESSEE_UNKNOWN Contact information for the
 *                    receiving process is not available
 * @retval ORTE_ERROR  An unspecified error occurred
 */
typedef int (*orte_rml_module_send_nb_fn_t)(struct orte_process_name_t* peer,
                                            struct iovec* msg,
                                            int count,
                                            orte_rml_tag_t tag,
                                            int flags,
                                            orte_rml_callback_fn_t cbfunc,
                                            void* cbdata);


/**
 * Send an buffer non-blocking message
 *
 * Send an buffer blocking message to the specified peer.  The call
 * will return immediately, although the buffer may not be modified
 * until the completion callback is triggered.  The buffer *may* be
 * passed to another call to send_nb before the completion callback is
 * triggered.  The callback being triggered does not give any
 * indication of remote completion.
 *
 * @param[in] peer   Name of receiving process
 * @param[in] buffer Buffer array describing send buffer
 * @param[in] tag    User defined tag for matching send/recv
 * @param[in] flags  Currently unused
 * @param[in] cbfunc Callback function on message comlpetion
 * @param[in] cbdata User data to provide during completion callback
 *
 * @retval ORTE_SUCCESS The message was successfully started
 * @retval ORTE_ERR_BAD_PARAM One of the parameters was invalid
 * @retval ORTE_ERR_ADDRESSEE_UNKNOWN Contact information for the
 *                    receiving process is not available
 * @retval ORTE_ERROR  An unspecified error occurred
 */
typedef int (*orte_rml_module_send_buffer_nb_fn_t)(struct orte_process_name_t* peer,
                                                   struct opal_buffer_t* buffer,
                                                   orte_rml_tag_t tag,
                                                   int flags,
                                                   orte_rml_buffer_callback_fn_t cbfunc,
                                                   void* cbdata);

/**
 * Receive an iovec blocking message
 *
 * Receive a message into a user-provided iovec.  The call will not
 * return until the buffer has been received.  The remote process does
 * not need to be connected to the current process to post the
 * receive, so it is possible to post a receive with no sending
 * process (ie, it will never complete).  The buffer must not be
 * currently in use with any other RML communication function during a
 * receive call.
 *
 * @param[in]  peer    Peer process or ORTE_NAME_WILDCARD for wildcard receive
 * @param[out] msg     iovec array of receive buffer space
 * @param[in]  count   Number of iovec entries in the array
 * @param[in]  tag     User defined tag for matching send/recv
 * @param[in]  flags   May be ORTE_RML_PEEK to return up to the number
 *                     of bytes provided in the iovec array without
 *                     removing the message from the queue.
 *
 * @retval >0          Number of bytes successfully received (may be smaller
 *                     than the posted buffer size
 * @retval ORTE_ERR_BAD_PARAM One of the parameters was invalid
 * @retval ORTE_ERROR  An unspecified error occurred
 */
typedef int (*orte_rml_module_recv_fn_t)(struct orte_process_name_t* peer,
                                         struct iovec *msg,
                                         int count,
                                         orte_rml_tag_t tag,
                                         int flags);


/**
 * Receive a buffer blocking message
 *
 * Receive a message into a user-provided buffer.  The call will not
 * return until the buffer has been received.  The remote process does
 * not need to be connected to the current process to post the
 * receive, so it is possible to post a receive with no sending
 * process (ie, it will never complete).  The buffer must not be
 * currently in use with any other RML communication function during a
 * receive call.
 *
 * @param[in]  peer    Peer process or ORTE_NAME_WILDCARD for wildcard receive
 * @param[out] buffer  A dss buffer to update with the received data
 * @param[in]  tag     User defined tag for matching send/recv
 * @param[in]  flags   Flags modifying receive behavior
 *
 * @retval >0          Number of bytes successfully received (may be smaller
 *                     than the posted buffer size
 * @retval ORTE_ERR_BAD_PARAM One of the parameters was invalid
 * @retval ORTE_ERROR  An unspecified error occurred
 */
typedef int (*orte_rml_module_recv_buffer_fn_t) (struct orte_process_name_t* peer,
                                                 struct opal_buffer_t *buf,
                                                 orte_rml_tag_t tag,
                                                 int flags);


/**
 * Receive an iovec non-blocking message
 *
 * Receive a message into a user-provided iovec.  The call will not
 * return until the buffer has been received.  The remote process does
 * not need to be connected to the current process to post the
 * receive, so it is possible to post a receive with no sending
 * process (ie, it will never complete).  The buffer must not be
 * currently in use with any other RML communication function during a
 * receive call.
 *
 * @param[in]  peer    Peer process or ORTE_NAME_WILDCARD for wildcard receive
 * @param[out] msg     iovec array of receive buffer space
 * @param[in]  count   Number of iovec entries in the array
 * @param[in]  tag     User defined tag for matching send/recv
 * @param[in]  flags   May be ORTE_RML_PEEK to return up to the number
 *                     of bytes provided in the iovec array without
 *                     removing the message from the queue.
 * @param[in] cbfunc   Callback function on message comlpetion
 * @param[in] cbdata   User data to provide during completion callback
 *
 * @retval ORTE_SUCCESS The message was successfully started
 * @retval ORTE_ERR_BAD_PARAM One of the parameters was invalid
 * @retval ORTE_ERR_ADDRESSEE_UNKNOWN Contact information for the
 *                    receiving process is not available
 * @retval ORTE_ERROR  An unspecified error occurred
 */
typedef int (*orte_rml_module_recv_nb_fn_t)(struct orte_process_name_t* peer,
                                            struct iovec* msg,
                                            int count,
                                            orte_rml_tag_t tag,
                                            int flags,
                                            orte_rml_callback_fn_t cbfunc,
                                            void* cbdata);


/**
 * Receive a buffer non-blocking message
 *
 * Receive a message into a user-provided buffer.  The call will not
 * return until the buffer has been received.  The remote process does
 * not need to be connected to the current process to post the
 * receive, so it is possible to post a receive with no sending
 * process (ie, it will never complete).  The buffer must not be
 * currently in use with any other RML communication function during a
 * receive call.
 *
 * @param[in]  peer    Peer process or ORTE_NAME_WILDCARD for wildcard receive
 * @param[in]  tag     User defined tag for matching send/recv
 * @param[in]  flags   May be ORTE_RML_PEEK to return up to the number
 *                     of bytes provided in the iovec array without
 *                     removing the message from the queue.
 * @param[in] cbfunc   Callback function on message comlpetion
 * @param[in] cbdata   User data to provide during completion callback
 *
 * @retval ORTE_SUCCESS The message was successfully started
 * @retval ORTE_ERR_BAD_PARAM One of the parameters was invalid
 * @retval ORTE_ERR_ADDRESSEE_UNKNOWN Contact information for the
 *                    receiving process is not available
 * @retval ORTE_ERROR  An unspecified error occurred
 */
typedef int (*orte_rml_module_recv_buffer_nb_fn_t)(struct orte_process_name_t* peer,
                                                   orte_rml_tag_t tag,
                                                   int flags,
                                                   orte_rml_buffer_callback_fn_t cbfunc,
                                                   void* cbdata);


/**
 * Cancel a posted non-blocking receive
 *
 * Attempt to cancel a posten non-blocking receive.
 *
 * @param[in] peer    Peer process or ORTE_NAME_WILDCARD, exactly as passed 
 *                    to the non-blocking receive call
 * @param[in] tag     Posted receive tag
 *
 * @retval ORTE_SUCCESS        The receive was successfully cancelled
 * @retval ORTE_ERR_BAD_PARAM  One of the parameters was invalid
 * @retval ORTE_ERR_NOT_FOUND  A matching receive was not found
 * @retval ORTE_ERROR          An unspecified error occurred
 */
typedef int (*orte_rml_module_recv_cancel_fn_t)(orte_process_name_t* peer,
                                                orte_rml_tag_t tag);


/**
 * Register or deregister an exception callback function
 *
 * Register or deregister a callback when an asynchronous
 * communication exception occurs.
 *
 * @param[in] cbfunc  User callback
 *
 * @retval ORTE_SUCCESS The operation completed successfully
 * @retval ORTE_ERROR   An unspecifed error occurred
 */
typedef int (*orte_rml_module_exception_fn_t)(orte_rml_exception_callback_t cbfunc);


/**
 * Handle fault tolerance updates
 *
 * Handle fault tolerance updates
 *
 * @param[in] state Fault tolerance state update
 *
 * @retval ORTE_SUCCESS The operation completed successfully
 * @retval ORTE_ERROR   An unspecifed error occurred
 */
typedef int  (*orte_rml_module_ft_event_fn_t)(int state);


/* ******************************************************************** */


/**
 * RML module interface
 *
 * Module interface to the RML communication system.  A global
 * instance of this module, orte_rml, provices an interface into the
 * active RML interface.
 */
struct orte_rml_module_t {
    /** Enable communication once a process name has been assigned */
    orte_rml_module_enable_comm_fn_t             enable_comm;
    /** Shutdown the communication system and clean up resources */
    orte_rml_module_finalize_fn_t                finalize;

    /** Get contact information for local process */
    orte_rml_module_get_contact_info_fn_t        get_contact_info;
    /** Set contact information for remote process */
    orte_rml_module_set_contact_info_fn_t        set_contact_info;

    /** Get a name from the remote process */
    orte_rml_module_get_new_name_fn_t            get_new_name;
    /** Ping process for connectivity check */
    orte_rml_module_ping_fn_t                    ping;

    /** Send blocking iovec message */
    orte_rml_module_send_fn_t                    send;
    /** Send blocking buffer message */
    orte_rml_module_send_nb_fn_t                 send_nb;
    /** Send non-blocking iovec message */
    orte_rml_module_send_buffer_fn_t             send_buffer;
    /** Send non-blocking buffer message */
    orte_rml_module_send_buffer_nb_fn_t          send_buffer_nb;

    /** Receive blocking iovec message */
    orte_rml_module_recv_fn_t                    recv;
    /** Receive blocking buffer message */
    orte_rml_module_recv_nb_fn_t                 recv_nb;
    /** Receive non-blocking iovec message */
    orte_rml_module_recv_buffer_fn_t             recv_buffer;
    /** Receive non-blocking buffer message */
    orte_rml_module_recv_buffer_nb_fn_t          recv_buffer_nb;
    /** Cancel posted non-blocking receive */
    orte_rml_module_recv_cancel_fn_t             recv_cancel;

    /** Add callback for communication exception */
    orte_rml_module_exception_fn_t               add_exception_handler;
    /** Delete callback for communication exception */
    orte_rml_module_exception_fn_t               del_exception_handler;

    /** Fault tolerance handler */
    orte_rml_module_ft_event_fn_t                ft_event;
};
/** Convienence typedef */
typedef struct orte_rml_module_t orte_rml_module_t;

/** Interface for RML communication */
ORTE_DECLSPEC extern orte_rml_module_t orte_rml;


/* ******************************************************************** */


/** Macro for use in components that are of type rml */
#define ORTE_RML_BASE_VERSION_2_0_0 \
  MCA_BASE_VERSION_2_0_0, \
  "rml", 2, 0, 0


/* ******************************************************************** */


END_C_DECLS

#endif
