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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 */

#ifndef ORTE_MCA_ERRMGR_PRIVATE_H
#define ORTE_MCA_ERRMGR_PRIVATE_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include "opal/dss/dss_types.h"
#include "orte/mca/plm/plm_types.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/errmgr/errmgr.h"


/*
 * Functions for use solely within the ERRMGR framework
 */
BEGIN_C_DECLS

/* Define the ERRMGR command flag */
typedef uint8_t orte_errmgr_cmd_flag_t;
#define ORTE_ERRMGR_CMD	OPAL_UINT8
    
/* define some commands */
#define ORTE_ERRMGR_ABORT_PROCS_REQUEST_CMD     0x01
#define ORTE_ERRMGR_REGISTER_CALLBACK_CMD       0x02
 
/* provide access to verbose output channel */
ORTE_DECLSPEC extern int orte_errmgr_base_output;

    
/*
 * Base functions
 */

ORTE_DECLSPEC    void orte_errmgr_base_log(int error_code, char *filename, int line);

ORTE_DECLSPEC    void orte_errmgr_base_proc_aborted_not_avail(orte_process_name_t *name, int exit_code);

ORTE_DECLSPEC    void orte_errmgr_base_incomplete_start_not_avail(orte_jobid_t job, int exit_code);

ORTE_DECLSPEC    void orte_errmgr_base_error_abort(int error_code, char *fmt, ...) __opal_attribute_format__(__printf__, 2, 3) __opal_attribute_noreturn__;

ORTE_DECLSPEC    int orte_errmgr_base_register_cb_not_avail(orte_jobid_t job,
                                                            orte_job_state_t state,
                                                            orte_err_cb_fn_t cbfunc,
                                                            void *cbdata);

/*
 * external API functions will be documented in the mca/errmgr/errmgr.h file
 */

END_C_DECLS
#endif
