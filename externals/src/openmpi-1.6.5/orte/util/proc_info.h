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
 *
 * Populates global structure with process-specific information.
 *
 *
 */

#ifndef _ORTE_PROC_INFO_H_
#define _ORTE_PROC_INFO_H_

#include "orte_config.h"

#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "orte/types.h"
#include "opal/dss/dss_types.h"

BEGIN_C_DECLS

#define ORTE_MAX_HOSTNAME_SIZE  512

typedef uint32_t orte_proc_type_t;
#define ORTE_PROC_TYPE_NONE     0x0000
#define ORTE_PROC_SINGLETON     0x0001
#define ORTE_PROC_DAEMON        0x0002
#define ORTE_PROC_HNP           0x0004
#define ORTE_PROC_TOOL          0x0008
#define ORTE_PROC_NON_MPI       0x0010
#define ORTE_PROC_MPI           0x0020
#define ORTE_PROC_APP           0x0030
#define ORTE_PROC_CM            0x0040
#define ORTE_PROC_CM_APP        0x0080

#define ORTE_PROC_IS_SINGLETON      (ORTE_PROC_SINGLETON & orte_process_info.proc_type)
#define ORTE_PROC_IS_DAEMON         (ORTE_PROC_DAEMON & orte_process_info.proc_type)
#define ORTE_PROC_IS_HNP            (ORTE_PROC_HNP & orte_process_info.proc_type)
#define ORTE_PROC_IS_TOOL           (ORTE_PROC_TOOL & orte_process_info.proc_type)
#define ORTE_PROC_IS_NON_MPI        (ORTE_PROC_NON_MPI & orte_process_info.proc_type)
#define ORTE_PROC_IS_MPI            (ORTE_PROC_MPI & orte_process_info.proc_type)
#define ORTE_PROC_IS_APP            (ORTE_PROC_APP & orte_process_info.proc_type)
#define ORTE_PROC_IS_CM             (ORTE_PROC_CM & orte_process_info.proc_type)
#define ORTE_PROC_IS_CM_APP         (ORTE_PROC_CM_APP & orte_process_info.proc_type)


/**
 * Process information structure
 *
 * The orte_proc_info() function fills the pid field and obtains the
 * process name, storing that information in the global structure. The
 * structure also holds path names to the universe, job, and process
 * session directories, and to the stdin, stdout, and stderr temp
 * files - however, these are all initialized elsewhere.
 */
struct orte_proc_info_t {
    orte_process_name_t my_name;        /**< My official process name */
    orte_process_name_t my_daemon;      /**< Name of my local daemon */
    char *my_daemon_uri;                /**< Contact info to local daemon */
    orte_process_name_t my_hnp;         /**< Name of my hnp */
    char *my_hnp_uri;                   /**< Contact info for my hnp */
    pid_t hnp_pid;                      /**< hnp pid - used if singleton */
    int32_t app_num;                    /**< our index into the app_context array */
    orte_vpid_t num_procs;              /**< number of processes in this job */
    int num_nodes;                      /**< number of nodes in the job */
    char *nodename;                     /**< string name for this node */
    pid_t pid;                          /**< Local process ID for this process */
    orte_proc_type_t proc_type;         /**< Type of process */
    opal_buffer_t *sync_buf;            /**< buffer to store sync response */
    uint16_t my_port;                   /**< TCP port for out-of-band comm */
    int32_t num_restarts;               /**< number of times this proc has restarted */
    /* The session directory has the form
     * <prefix>/<openmpi-sessions-user>/<jobid>/<procid>, where the prefix
     * can either be provided by the user via the
     * --tmpdir command-line flag, the use of one of several
     * environmental variables, or else a default location.
     */
    char *tmpdir_base;                  /**< Base directory of the session dir tree */
    char *top_session_dir;              /**< Top-most directory of the session tree */
    char *job_session_dir;              /**< Session directory for job */
    char *proc_session_dir;             /**< Session directory for the process */

    char *sock_stdin;                   /**< Path name to temp file for stdin. */
    char *sock_stdout;                  /**< Path name to temp file for stdout. */
    char *sock_stderr;                  /**< Path name to temp file for stderr. */
};
typedef struct orte_proc_info_t orte_proc_info_t;


/**
 *
 * Global process info descriptor.  Initialized to almost no
 * meaningful information - data is provided by calling \c
 * orte_rte_init() (which calls \c orte_proc_info() to fill in the
 * structure).
 *
 * The exception to this rule is the \c orte_process_info.seed field,
 * which will be initialized to \c false, but should be set to \c true
 * before calling \c orte_rte_info() if the caller is a seed daemon.
 */
ORTE_DECLSPEC extern orte_proc_info_t orte_process_info;


/**
 * \internal
 *
 * Global structure to store a wide range of information about the
 * process.  orte_proc_info populates a global variable with
 * information about the process being executing. This function should
 * be called only once, from orte_rte_init().
 *
 * @param None.
 *
 * @retval ORTE_SUCCESS Successfully initialized the various fields.
 * @retval OMPI_ERROR Failed to initialize one or more fields.
 */

ORTE_DECLSPEC int orte_proc_info(void);

ORTE_DECLSPEC int orte_proc_info_finalize(void);

END_C_DECLS

#endif
