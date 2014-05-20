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
 * Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007-2008 Sun Microsystems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file **/

#include "orte_config.h"
#include "orte/constants.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/runtime/opal.h"

#include "orte/util/show_help.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/proc_info.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_locks.h"

/*
 * Whether we have completed orte_init or we are in orte_finalize
 */
bool orte_initialized = false;
bool orte_finalizing = false;
bool orte_debug_flag = false;
int orte_debug_verbosity;
char *orte_prohibited_session_dirs = NULL;

orte_process_name_t orte_name_wildcard = {ORTE_JOBID_WILDCARD, ORTE_VPID_WILDCARD};
orte_process_name_t orte_name_invalid = {ORTE_JOBID_INVALID, ORTE_VPID_INVALID}; 


#if OPAL_CC_USE_PRAGMA_IDENT
#pragma ident ORTE_IDENT_STRING
#elif OPAL_CC_USE_IDENT
#ident ORTE_IDENT_STRING
#endif
const char orte_version_string[] = ORTE_IDENT_STRING;

int orte_init(int* pargc, char*** pargv, orte_proc_type_t flags)
{
    int ret;
    char *error = NULL;

    if (orte_initialized) {
        return ORTE_SUCCESS;
    }

    /* initialize the opal layer */
    if (ORTE_SUCCESS != (ret = opal_init(pargc, pargv))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    /* ensure we know the type of proc for when we finalize */
    orte_process_info.proc_type = flags;

    /* setup the locks */
    if (ORTE_SUCCESS != (ret = orte_locks_init())) {
        error = "orte_locks_init";
        goto error;
    }
    
    /* Register all MCA Params */
    if (ORTE_SUCCESS != (ret = orte_register_params())) {
        error = "orte_register_params";
        goto error;
    }
    
    /* setup the orte_show_help system */
    if (ORTE_SUCCESS != (ret = orte_show_help_init())) {
        ORTE_ERROR_LOG(ret);
        error = "opal_output_init";
        goto error;
    }
    
    /* register handler for errnum -> string conversion */
    opal_error_register("ORTE", ORTE_ERR_BASE, ORTE_ERR_MAX, orte_err2str);

    /* Ensure the rest of the process info structure is initialized */
    if (ORTE_SUCCESS != (ret = orte_proc_info())) {
        error = "orte_proc_info";
        goto error;
    }

    /* open the ESS and select the correct module for this environment */
    if (ORTE_SUCCESS != (ret = orte_ess_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ess_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_ess_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ess_base_select";
        goto error;
    }
    
    /* initialize the RTE for this environment */
    if (ORTE_SUCCESS != (ret = orte_ess.init())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ess_set_name";
        goto error;
    }
    
    /* All done */
    orte_initialized = true;
    return ORTE_SUCCESS;
    
error:
    if (ORTE_ERR_SILENT != ret) {
        orte_show_help("help-orte-runtime",
                       "orte_init:startup:internal-failure",
                       true, error, ORTE_ERROR_NAME(ret), ret);
    }

    return ret;
}

