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
/** @file */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include "orte/mca/rml/base/base.h"


int orte_rml_base_null_send(struct orte_process_name_t* peer,
                            struct iovec *msg,
                            int count,
                            int tag,
                            int flags)
{
    return ORTE_ERR_NOT_SUPPORTED;
}

int orte_rml_base_null_send_nb(struct orte_process_name_t* peer,
                               struct iovec* msg,
                               int count,
                               orte_rml_tag_t tag,
                               int flags,
                               orte_rml_callback_fn_t cbfunc,
                               void* cbdata)
{
    return ORTE_ERR_NOT_SUPPORTED;
}

int orte_rml_base_null_send_buffer(struct orte_process_name_t* peer,
                                   struct opal_buffer_t* buffer,
                                   orte_rml_tag_t tag,
                                   int flags)
{
    return ORTE_ERR_NOT_SUPPORTED;
}

int orte_rml_base_null_send_buffer_nb(struct orte_process_name_t* peer,
                                      struct opal_buffer_t* buffer,
                                      orte_rml_tag_t tag,
                                      int flags,
                                      orte_rml_buffer_callback_fn_t cbfunc,
                                      void* cbdata)
{
    return ORTE_ERR_NOT_SUPPORTED;
}

int orte_rml_base_null_recv(struct orte_process_name_t* peer,
                            struct iovec *msg,
                            int count,
                            orte_rml_tag_t tag,
                            int flags)
{
    return ORTE_ERR_NOT_SUPPORTED;
}

int orte_rml_base_null_recv_nb(struct orte_process_name_t* peer,
                               struct iovec* msg,
                               int count,
                               orte_rml_tag_t tag,
                               int flags,
                               orte_rml_callback_fn_t cbfunc,
                               void* cbdata)
{
    return ORTE_ERR_NOT_SUPPORTED;
}

int orte_rml_base_null_recv_buffer(struct orte_process_name_t* peer,
                                   struct opal_buffer_t *buf,
                                   orte_rml_tag_t tag,
                                   int flags)
{
    return ORTE_ERR_NOT_SUPPORTED;
}

int orte_rml_base_null_recv_buffer_nb(struct orte_process_name_t* peer,
                                      orte_rml_tag_t tag,
                                      int flags,
                                      orte_rml_buffer_callback_fn_t cbfunc,
                                      void* cbdata)
{
    return ORTE_ERR_NOT_SUPPORTED;
}

int orte_rml_base_null_recv_cancel(orte_process_name_t* peer,
                                   orte_rml_tag_t tag)
{
    return ORTE_ERR_NOT_SUPPORTED;
}

