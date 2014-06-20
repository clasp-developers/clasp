/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 * The Open RTE Error Manager
 *
 */

#ifndef ORTE_MCA_ERRMGR_H
#define ORTE_MCA_ERRMGR_H

/*
 * includes
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include "opal/mca/mca.h"
#include "opal/util/error.h"

#include "orte/runtime/orte_globals.h"
#include "orte/mca/plm/plm_types.h"

BEGIN_C_DECLS

/*
 * Macro definitions
 */
/*
 * Thess macros and associated error name array are used to output intelligible error
 * messages.
 */

#define ORTE_ERROR_NAME(n)  opal_strerror(n)
#define ORTE_ERROR_LOG(n) \
    orte_errmgr_base_log(n, __FILE__, __LINE__)

#if WANT_PMI_SUPPORT
#define ORTE_PMI_ERROR(pmi_err, pmi_func)                               \
    do {                                                                \
        opal_output(0, "%s[%s:%d:%s] %s: %s\n",                         \
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),                 \
                    __FILE__, __LINE__, __func__,                       \
                    pmi_func, orte_errmgr_base_pmi_error(pmi_err));     \
    } while(0);
OPAL_DECLSPEC char* orte_errmgr_base_pmi_error(int pmi_err);
#endif

/**
 * This is not part of any
 * module so it can be used at any time!
 */
ORTE_DECLSPEC extern void orte_errmgr_base_log(int error_code, char *filename, int line);



/*
 * Component functions - all MUST be provided!
 */

/**
 * Alert - process aborted
 * This function is called by the PLM when a remote process aborts during execution. Actions taken
 * in response to the abnormal termination of a remote application process will vary across
 * the various errmgr components.
 *
 * NOTE: Local process errors should always be reported through the error_detected interface and
 * NOT here.
 *
 * @param *name Pointer to the name of the proc that aborted
 *
 * @retval ORTE_SUCCESS Whatever action that was taken was successful
 * @retval ORTE_ERROR Appropriate error code
 */
typedef void (*orte_errmgr_base_module_proc_aborted_fn_t)(orte_process_name_t *name, int exit_code);

/**
 * Alert - incomplete start of a job
 * This function is called by the PLM when an attempted launch of a job encounters failure of
 * one or more processes to start. The strategy for dealing
 * with this "incomplete start" situation varies across the various errmgr components.
 *
 * This function is only called by the respective process launcher, which is responsible
 * for detecting incomplete starts. If on a daemon, the function simply updates the
 * process state to indicate failure to launch - this initiates a trigger that goes to
 * the respective HNP for response.
 *
 * NOTE: Errmgr components on non-HNP and non-daemon processes are expressly forbidden
 * from taking any action to this function call. Instead, they are restricted to simply
 * returning.
 *
 * @param job Job that failed to start
 *
 * @retval ORTE_SUCCESS Whatever action that was taken was successful
 * @retval ORTE_ERROR Appropriate error code
 */
typedef void (*orte_errmgr_base_module_incomplete_start_fn_t)(orte_jobid_t job, int exit_code);

/*
 * Register a job with the error manager
 * When a job is launched, this function is called so the error manager can register
 * subscriptions on the job segment so that the error manager will be notified when
 * problems occur - i.e., when process status entries change to abnormal termination
 * values. Process status entries are changed by the appropriate state monitor
 * and/or the process launcher, depending upon the stage at which the problem occurs.
 *
 * Monitoring of the job begins once the job has reached the "executing" stage. Prior
 * to that time, failure of processes to start are the responsibility of the respective
 * process launcher - which is expected to call the error manager via the "incomplete
 * start" interface to report any problems prior to the job beginning "execution".
 *
 * NOTE: ONLY HNPs are allowed to register for trigger reports. All other components
 * MUST do nothing but return ORTE_SUCCESS.
 */
typedef int (*orte_errmgr_base_module_register_cb_fn_t)(orte_jobid_t job,
                                                        orte_proc_state_t state,
                                                        orte_err_cb_fn_t cbfunc,
                                                        void *cbdata);

/**
 * Alert - self aborting
 * This function is called when a process is aborting due to some internal error.
 * It will finalize the process
 * itself, and then exit - it takes no other actions. The intent here is to provide
 * a last-ditch exit procedure that attempts to clean up a little.
 */
typedef void (*orte_errmgr_base_module_abort_fn_t)(int error_code, char *fmt, ...) __opal_attribute_noreturn__
    __opal_attribute_format_funcptr__(__printf__, 2, 3);

/*
 * 
 */
struct orte_errmgr_base_module_2_3_0_t {
    orte_errmgr_base_module_proc_aborted_fn_t           proc_aborted;
    orte_errmgr_base_module_incomplete_start_fn_t       incomplete_start;
    orte_errmgr_base_module_register_cb_fn_t            register_callback;
    orte_errmgr_base_module_abort_fn_t                  abort;
};

typedef struct orte_errmgr_base_module_2_3_0_t orte_errmgr_base_module_2_3_0_t;
typedef orte_errmgr_base_module_2_3_0_t orte_errmgr_base_module_t;

/*
 * ERRMGR Component
 * the standard component data structure
 */
struct mca_errmgr_base_component_2_0_0_t {
    mca_base_component_t base_version;
    mca_base_component_data_t base_data;
};
typedef struct mca_errmgr_base_component_2_0_0_t mca_errmgr_base_component_2_0_0_t;
typedef mca_errmgr_base_component_2_0_0_t mca_errmgr_base_component_t;



/*
 * Macro for use in components that are of type errmgr
 */
#define ORTE_ERRMGR_BASE_VERSION_2_0_0 \
  MCA_BASE_VERSION_2_0_0, \
  "errmgr", 2, 0, 0

/* Global structure for accessing error manager functions
 */
ORTE_DECLSPEC extern orte_errmgr_base_module_t orte_errmgr;  /* holds selected module's function pointers */

END_C_DECLS

#endif
