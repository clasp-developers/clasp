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

#ifndef ORTED_H
#define ORTED_H

#include "orte_config.h"
#include "orte/types.h"

#ifdef HAVE_TIME_H
#include <time.h>
#endif

#include "opal/dss/dss_types.h"
#include "orte/mca/rml/rml_types.h"

BEGIN_C_DECLS

/* main orted routine */
ORTE_DECLSPEC int orte_daemon(int argc, char *argv[]);

/* orted communication functions */
ORTE_DECLSPEC void orte_daemon_recv(int status, orte_process_name_t* sender,
                      opal_buffer_t *buffer, orte_rml_tag_t tag,
                      void* cbdata);

/* direct cmd processing entry point - used by HNP */
ORTE_DECLSPEC void orte_daemon_cmd_processor(int fd, short event, void *data);

/* a time flag that needs to be visible elsewhere */
ORTE_DECLSPEC extern struct timeval orte_daemon_msg_recvd;

END_C_DECLS

#endif /* ORTED_H */
