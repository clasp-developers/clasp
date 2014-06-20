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

#include "orte_config.h"

#include "orte/util/name_fns.h"

#include "opal/mca/base/base.h"
#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/rml/base/base.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/rml/rml.h"

#include "rml_ftrm.h"

orte_rml_component_t mca_rml_ftrm_wrapped_component;
orte_rml_module_t   orte_rml_ftrm_wrapped_module;

/*
 * Init (Module)
 */

int orte_rml_ftrm_module_enable_comm(void)
{
    int ret;

    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: module_init(): Normal...");

    if( NULL != orte_rml_ftrm_wrapped_module.enable_comm ) {
        if( ORTE_SUCCESS != (ret = orte_rml_ftrm_wrapped_module.enable_comm() ) ) {
            return ret;
        }
    }

    return ORTE_SUCCESS;
}
    
/*
 * Finalize (Module)
 */
int orte_rml_ftrm_module_finalize(void)
{
    int ret;

    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: module_finalize()");


    if( NULL != orte_rml_ftrm_wrapped_module.finalize ) {
        if( ORTE_SUCCESS != (ret = orte_rml_ftrm_wrapped_module.finalize() ) ) {
            return ret;
        }
    }

    return ORTE_SUCCESS;
}


int orte_rml_ftrm_get_new_name(orte_process_name_t *name)
{
    int ret;

    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: get_new_name()");

    if( NULL != orte_rml_ftrm_wrapped_module.get_new_name ) {
        if( ORTE_SUCCESS != (ret = orte_rml_ftrm_wrapped_module.get_new_name(name) ) ) {
            return ret;
        }
    }

    return ORTE_SUCCESS;
}


/*
 * Get URI
 */
char * orte_rml_ftrm_get_contact_info(void)
{
    char * rtn_val = NULL;

    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: get_uri()");


    if( NULL != orte_rml_ftrm_wrapped_module.get_contact_info ) {
        rtn_val = orte_rml_ftrm_wrapped_module.get_contact_info();
    }

    return rtn_val;
}

/*
 * Set CONTACT_INFO
 */
int orte_rml_ftrm_set_contact_info(const char* contact_info)
{
    int ret;

    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: set_contact_info()");

    if( NULL != orte_rml_ftrm_wrapped_module.set_contact_info ) {
        if( ORTE_SUCCESS != (ret = orte_rml_ftrm_wrapped_module.set_contact_info(contact_info) ) ) {
            return ret;
        }
    }

    return ORTE_SUCCESS;
}


/*
 * Ping
 */
int orte_rml_ftrm_ping(const char* uri, const struct timeval* tv)
{
    int ret;

    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: ping()");

    if( NULL != orte_rml_ftrm_wrapped_module.ping ) {
        if( ORTE_SUCCESS != (ret = orte_rml_ftrm_wrapped_module.ping(uri, tv) ) ) {
            return ret;
        }
    }

    return ORTE_SUCCESS;
}


/*
 * Send
 */
int orte_rml_ftrm_send(orte_process_name_t* peer,
                       struct iovec *msg,
                       int count,
                       int tag,
                       int flags)
{
    int ret;

    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: send(%s, %d, %d, %d )",
                        ORTE_NAME_PRINT(peer), count, tag, flags);

    if( NULL != orte_rml_ftrm_wrapped_module.send ) {
        if( ORTE_SUCCESS != (ret = orte_rml_ftrm_wrapped_module.send(peer, msg, count, tag, flags) ) ) {
            return ret;
        }
    }

    return ORTE_SUCCESS;
}

/*
 * Send Non-blocking
 */
int orte_rml_ftrm_send_nb(orte_process_name_t* peer,
                          struct iovec* msg,
                          int count,
                          orte_rml_tag_t tag,
                          int flags,
                          orte_rml_callback_fn_t cbfunc,
                          void* cbdata)
{
    int ret;

    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: send_nb(%s, %d, %d, %d )",
                        ORTE_NAME_PRINT(peer), count, tag, flags);

    if( NULL != orte_rml_ftrm_wrapped_module.send_nb ) {
        if( ORTE_SUCCESS != (ret = orte_rml_ftrm_wrapped_module.send_nb(peer, msg, count, tag, flags, cbfunc, cbdata) ) ) {
            return ret;
        }
    }

    return ORTE_SUCCESS;
}

/*
 * Send Buffer
 */
int orte_rml_ftrm_send_buffer(orte_process_name_t* peer,
                              opal_buffer_t* buffer,
                              orte_rml_tag_t tag,
                              int flags)
{
    int ret;

    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: send_buffer(%s, %d, %d )",
                        ORTE_NAME_PRINT(peer), tag, flags);

    if( NULL != orte_rml_ftrm_wrapped_module.send_buffer ) {
        if( ORTE_SUCCESS != (ret = orte_rml_ftrm_wrapped_module.send_buffer(peer, buffer, tag, flags) ) ) {
            return ret;
        }
    }

    return ORTE_SUCCESS;
}

/*
 * Send Buffer Non-blocking
 */
int orte_rml_ftrm_send_buffer_nb(orte_process_name_t* peer,
                                 opal_buffer_t* buffer,
                                 orte_rml_tag_t tag,
                                 int flags,
                                 orte_rml_buffer_callback_fn_t cbfunc,
                                 void* cbdata)
{
    int ret;

    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: send_buffer_nb(%s, %d, %d )",
                        ORTE_NAME_PRINT(peer), tag, flags);

    if( NULL != orte_rml_ftrm_wrapped_module.send_buffer_nb ) {
        if( ORTE_SUCCESS != (ret = orte_rml_ftrm_wrapped_module.send_buffer_nb(peer, buffer, tag, flags, cbfunc, cbdata) ) ) {
            return ret;
        }
    }

    return ORTE_SUCCESS;
}


/*
 * Recv
 */
int orte_rml_ftrm_recv(orte_process_name_t* peer,
                       struct iovec *msg,
                       int count,
                       orte_rml_tag_t tag,
                       int flags)
{
    int ret;

    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: recv(%s, %d, %d, %d )",
                        ORTE_NAME_PRINT(peer), count, tag, flags);

    if( NULL != orte_rml_ftrm_wrapped_module.recv ) {
        if( ORTE_SUCCESS != (ret = orte_rml_ftrm_wrapped_module.recv(peer, msg, count, tag, flags) ) ) {
            return ret;
        }
    }

    return ORTE_SUCCESS;
}

/*
 * Recv Non-blocking
 */
int orte_rml_ftrm_recv_nb(orte_process_name_t* peer,
                          struct iovec* msg,
                          int count,
                          orte_rml_tag_t tag,
                          int flags,
                          orte_rml_callback_fn_t cbfunc,
                          void* cbdata)
{
    int ret;

    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: recv_nb(%s, %d, %d, %d )",
                        ORTE_NAME_PRINT(peer), count, tag, flags);

    if( NULL != orte_rml_ftrm_wrapped_module.recv_nb ) {
        if( ORTE_SUCCESS != (ret = orte_rml_ftrm_wrapped_module.recv_nb(peer, msg, count, tag, flags, cbfunc, cbdata) ) ) {
            return ret;
        }
    }

    return ORTE_SUCCESS;
}

/*
 * Recv Buffer
 */
int orte_rml_ftrm_recv_buffer(orte_process_name_t* peer,
                              opal_buffer_t *buf,
                              orte_rml_tag_t tag,
                              int flags)
{
    int ret;

    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: recv_buffer(%s, %d )",
                        ORTE_NAME_PRINT(peer), tag);

    if( NULL != orte_rml_ftrm_wrapped_module.recv_buffer ) {
        if( ORTE_SUCCESS != (ret = orte_rml_ftrm_wrapped_module.recv_buffer(peer, buf, tag, flags) ) ) {
            return ret;
        }
    }

    return ORTE_SUCCESS;
}

/*
 * Recv Buffer Non-blocking
 */
int orte_rml_ftrm_recv_buffer_nb(orte_process_name_t* peer,
                                 orte_rml_tag_t tag,
                                 int flags,
                                 orte_rml_buffer_callback_fn_t cbfunc,
                                 void* cbdata)
{
    int ret;

    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: recv_buffer_nb(%s, %d, %d)",
                        ORTE_NAME_PRINT(peer), tag, flags);

    if( NULL != orte_rml_ftrm_wrapped_module.recv_buffer_nb ) {
        if( ORTE_SUCCESS != (ret = orte_rml_ftrm_wrapped_module.recv_buffer_nb(peer, tag, flags, cbfunc, cbdata) ) ) {
            return ret;
        }
    }

    return ORTE_SUCCESS;
}

/*
 * Recv Cancel
 */
int orte_rml_ftrm_recv_cancel(orte_process_name_t* peer, orte_rml_tag_t tag)
{
    int ret;

    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: recv_cancel()");

    if( NULL != orte_rml_ftrm_wrapped_module.recv_cancel ) {
        if( ORTE_SUCCESS != (ret = orte_rml_ftrm_wrapped_module.recv_cancel(peer, tag) ) ) {
            return ret;
        }
    }

    return ORTE_SUCCESS;
}


/*
 * Register a callback on loss of connection
 */
int orte_rml_ftrm_add_exception_handler(orte_rml_exception_callback_t cbfunc)
{
    int ret;

    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: add_exception_handler()");

    if( NULL != orte_rml_ftrm_wrapped_module.add_exception_handler ) {
        if( ORTE_SUCCESS != (ret = orte_rml_ftrm_wrapped_module.add_exception_handler(cbfunc) ) ) {
            return ret;
        }
    }

    return ORTE_SUCCESS;
}

int orte_rml_ftrm_del_exception_handler(orte_rml_exception_callback_t cbfunc)
{
    int ret;

    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: del_exception_handler()");

    if( NULL != orte_rml_ftrm_wrapped_module.del_exception_handler ) {
        if( ORTE_SUCCESS != (ret = orte_rml_ftrm_wrapped_module.del_exception_handler(cbfunc) ) ) {
            return ret;
        }
    }

    return ORTE_SUCCESS;
}

/*
 * FT Event
 */
int orte_rml_ftrm_ft_event(int state)
{
    int ret;

    opal_output_verbose(20, rml_ftrm_output_handle,
                        "orte_rml_ftrm: ft_event()");

    if(OPAL_CRS_CHECKPOINT == state) {
        ;
    }
    else if(OPAL_CRS_CONTINUE == state) {
        ;
    }
    else if(OPAL_CRS_RESTART == state) {
        ;
    }
    else if(OPAL_CRS_TERM == state ) {
        ;
    }
    else {
        ;
    }

    /*
     * The wrapped component is responsible for calling the OOB modules
     */
    if( NULL != orte_rml_ftrm_wrapped_module.ft_event ) {
        if( ORTE_SUCCESS != (ret = orte_rml_ftrm_wrapped_module.ft_event(state))) {
            return ret;
        }
    }

    if(OPAL_CRS_CHECKPOINT == state) {
        ;
    }
    else if(OPAL_CRS_CONTINUE == state) {
        ;
    }
    else if(OPAL_CRS_RESTART == state) {
        ;
    }
    else if(OPAL_CRS_TERM == state ) {
        ;
    }
    else {
        ;
    }

    return ORTE_SUCCESS;
}

