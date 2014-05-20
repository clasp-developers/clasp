/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include <stdio.h>
#include <stdarg.h>

#include "base.h"
#include "btl_base_error.h"

#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "orte/types.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

int mca_btl_base_verbose = -1;

int mca_btl_base_err(const char* fmt, ...)
{
    va_list list;
    int ret;

    va_start(list, fmt);
    ret = vfprintf(stderr, fmt, list);
    va_end(list);
    return ret;
}


int mca_btl_base_out(const char* fmt, ...)
{
    va_list list;
    int ret;

    va_start(list, fmt);
    ret = vfprintf(stdout, fmt, list);
    va_end(list);
    return ret;
}


void mca_btl_base_error_no_nics(const char* transport, 
                                const char* nic_name)
{
    char *procid;
    if (mca_btl_base_warn_component_unused) {
        /* print out no-nic warning if user told us to */
        asprintf(&procid, "%s", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

        orte_show_help("help-mpi-btl-base.txt", "btl:no-nics",
                       true, procid, transport, orte_process_info.nodename,
                       nic_name);
        free(procid);
    }
}


void mca_btl_base_dump(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    int verbose)
{
}
