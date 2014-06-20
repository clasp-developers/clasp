/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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
/** @file:
 *
 * Contains the internal functions and typedefs for the use of the oob
 */

#ifndef MCA_OOB_H_
#define MCA_OOB_H_

#include "orte_config.h"
#include "orte/types.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/mca/mca.h"

#include "orte/mca/rml/rml.h"

#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"


BEGIN_C_DECLS


struct mca_oob_1_0_0_t;
typedef struct mca_oob_1_0_0_t mca_oob_1_0_0_t;
typedef struct mca_oob_1_0_0_t mca_oob_t;


typedef mca_oob_t* (*mca_oob_base_component_init_fn_t)(int  *priority);

struct mca_oob_base_component_2_0_0_t {
   mca_base_component_t oob_base;
   mca_base_component_data_t oob_data;
   mca_oob_base_component_init_fn_t oob_init;
};
typedef struct mca_oob_base_component_2_0_0_t mca_oob_base_component_2_0_0_t;
typedef mca_oob_base_component_2_0_0_t mca_oob_base_component_t;



typedef char* (*mca_oob_base_module_get_addr_fn_t)(void);

typedef int (*mca_oob_base_module_set_addr_fn_t)(const orte_process_name_t* peer, 
                                                 const char* uri);


typedef int (*mca_oob_base_module_get_new_name_fn_t)(orte_process_name_t*);

/**
*  Implementation of mca_oob_ping().
*   
*  @param peer (IN)   Opaque name of peer process.
*  @param tv (IN)     Timeout to wait in connection response.
*  @return            OMPI error code (<0) or ORTE_SUCCESS
*/
typedef int (*mca_oob_base_module_ping_fn_t)(const orte_process_name_t*, 
                                             const char* uri, 
                                             const struct timeval* tv);


/**
 * Send an oob message
 * 
 * Send an oob message.  All oob sends are non-blocking, and cbfunc
 * will be called when the message has been sent.  When cbfunc is
 * called, message has been injected into the network but no guarantee
 * is made about whether the target has received the message.
 *
 * @param[in] target   Destination process name
 * @param[in] origin   Origin process for the message, for the purposes
 *                     of message matching.  This can be different from
 *                     the process calling send().
 * @param[in] msg      Array of iovecs describing user buffers and lengths.
 * @param[in] count    Number of elements in iovec array.
 * @param[in] tag      User defined tag for matching send/recv.
 * @param[in] flags    Currently unused.
 * @param[in] cbfunc   Callback function on send completion.
 * @param[in] cbdata   User data that is passed to callback function.
 *
 * @return             OMPI error code (<0) on error number of bytes actually sent.
 */
typedef int (*mca_oob_base_module_send_nb_fn_t)(
    orte_process_name_t* target,
    orte_process_name_t* origin,
    struct iovec* msg,
    int count,
    int tag,
    int flags,
    orte_rml_callback_fn_t cbfunc,
    void* cbdata);

/**
* Implementation of mca_oob_recv_nb().
*
* @param peer (IN)    Opaque name of peer process or ORTE_NAME_WILDCARD for wildcard receive.
* @param msg (IN)     Array of iovecs describing user buffers and lengths.
* @param count (IN)   Number of elements in iovec array.
* @param tag (IN)     User defined tag for matching send/recv.
* @param flags (IN)   May be MCA_OOB_PEEK to return up to size bytes of msg w/out removing it from the queue,
* @param cbfunc (IN)  Callback function on recv completion.
* @param cbdata (IN)  User data that is passed to callback function.
* @return             OMPI error code (<0) on error or number of bytes actually received.
*/

typedef int (*mca_oob_base_module_recv_nb_fn_t)(
    orte_process_name_t* peer,
    struct iovec* msg,
    int count,
    int tag,
    int flags,
    orte_rml_callback_fn_t cbfunc,
    void* cbdata);

/**
* Implementation of mca_oob_recv_cancel().
*
* @param peer (IN)    Opaque name of peer process or ORTE_NAME_WILDCARD for wildcard receive.
* @param tag (IN)     User defined tag for matching send/recv.
* @return             OMPI error code (<0) on error or number of bytes actually received.
*/

typedef int (*mca_oob_base_module_recv_cancel_fn_t)(orte_process_name_t* peer, int tag);

/**
 * Hook function called by mca_oob_base_register to allow
 * the oob component a chance to register contact information
 */
typedef int (*mca_oob_base_module_init_fn_t)(void);

/**
 * Cleanup during finalize.
 */
typedef int (*mca_oob_base_module_fini_fn_t)(void);

/* ft event */
typedef int (*mca_oob_base_module_ft_event_fn_t)( int state );


/**
 * OOB Module
 */
struct mca_oob_1_0_0_t {
    mca_oob_base_module_init_fn_t                oob_init;
    mca_oob_base_module_fini_fn_t                oob_fini;

    mca_oob_base_module_get_addr_fn_t            oob_get_addr;
    mca_oob_base_module_set_addr_fn_t            oob_set_addr;

    mca_oob_base_module_get_new_name_fn_t        oob_get_new_name;
    mca_oob_base_module_ping_fn_t                oob_ping;

    mca_oob_base_module_send_nb_fn_t             oob_send_nb;

    mca_oob_base_module_recv_nb_fn_t             oob_recv_nb;
    mca_oob_base_module_recv_cancel_fn_t         oob_recv_cancel;

    mca_oob_base_module_ft_event_fn_t            oob_ft_event;

    orte_rml_exception_callback_t                oob_exception_callback;
};

/**
 * Macro for use in components that are of type oob
 */
#define MCA_OOB_BASE_VERSION_2_0_0 \
  MCA_BASE_VERSION_2_0_0, \
  "oob", 2, 0, 0

/*
 * BWB - FIX ME - This is the first module on the list. This is here
 * temporarily to make things work
 */

ORTE_DECLSPEC extern mca_oob_t mca_oob;

END_C_DECLS

#endif
