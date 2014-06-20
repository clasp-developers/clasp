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

#ifndef ORTE_TYPES_H
#define ORTE_TYPES_H

#include "orte_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include "opal/dss/dss_types.h"

/**
 * Supported datatypes for messaging and storage operations.
 */

typedef int32_t orte_std_cntr_t;  /** standard counters used in ORTE */
#define ORTE_STD_CNTR_T         OPAL_INT32
#define ORTE_STD_CNTR_MAX       INT32_MAX
#define ORTE_STD_CNTR_MIN       INT32_MIN
#define ORTE_STD_CNTR_INVALID   -1

/** rank on node, used for both local and node rank. We
 * don't send these around on their own, so don't create
 * dedicated type support for them - we are defining them
 * here solely for readability in the code and so we have
 * one place where any future changes can be made
 */
typedef uint16_t orte_local_rank_t;
typedef uint16_t orte_node_rank_t;
#define ORTE_LOCAL_RANK         OPAL_UINT16
#define ORTE_NODE_RANK          OPAL_UINT16
#define ORTE_LOCAL_RANK_MAX     UINT16_MAX-1
#define ORTE_NODE_RANK_MAX      UINT16_MAX-1
#define ORTE_LOCAL_RANK_INVALID UINT16_MAX
#define ORTE_NODE_RANK_INVALID  UINT16_MAX


/*
 * general typedefs & structures
 */
/** Set the allowed range for ids in each space
 *
 * NOTE: Be sure to update the ORTE_NAME_ARGS #define (above) and all
 * uses of it if these types change to be larger than (long)!  The
 * HTON and NTOH macros below must be updated, as well as the MIN /
 * MAX macros below and the datatype packing representations in
 * orte/mca/plm/base/plm_private.h
 *
 * NOTE: Be sure to keep the jobid and vpid types the same size! Due
 * to padding rules, it won't save anything to have one larger than
 * the other, and it will cause problems in the communication subsystems
 */

typedef uint32_t orte_jobid_t;
#define ORTE_JOBID_T        OPAL_UINT32
#define ORTE_JOBID_MAX      UINT32_MAX-2
#define ORTE_JOBID_MIN      0
typedef uint32_t orte_vpid_t;
#define ORTE_VPID_T         OPAL_UINT32
#define ORTE_VPID_MAX       UINT32_MAX-2
#define ORTE_VPID_MIN       0

#define ORTE_PROCESS_NAME_HTON(n)       \
do {                                    \
    n.jobid = htonl(n.jobid);           \
    n.vpid = htonl(n.vpid);             \
} while (0)

#define ORTE_PROCESS_NAME_NTOH(n)       \
do {                                    \
    n.jobid = ntohl(n.jobid);           \
    n.vpid = ntohl(n.vpid);             \
} while (0)

#define ORTE_NAME_ARGS(n) \
    (unsigned long) ((NULL == n) ? (unsigned long)ORTE_JOBID_INVALID : (unsigned long)(n)->jobid), \
    (unsigned long) ((NULL == n) ? (unsigned long)ORTE_VPID_INVALID : (unsigned long)(n)->vpid)

/*
 * define invalid values
 */
#define ORTE_JOBID_INVALID      (ORTE_JOBID_MAX + 2)
#define ORTE_VPID_INVALID       (ORTE_VPID_MAX + 2)

/*
 * define wildcard values
 */
#define ORTE_JOBID_WILDCARD      (ORTE_JOBID_MAX + 1)
#define ORTE_VPID_WILDCARD       (ORTE_VPID_MAX + 1)

/*
 * define the process name structure
 */
struct orte_process_name_t {
    orte_jobid_t jobid;     /**< Job number */
    orte_vpid_t vpid;       /**< Process id - equivalent to rank */
};
typedef struct orte_process_name_t orte_process_name_t;


/**
 * handle differences in iovec
 */

#if defined(__APPLE__) || defined(__WINDOWS__)
typedef char* orte_iov_base_ptr_t;
#else
typedef void* orte_iov_base_ptr_t;
#endif


/* General ORTE types - support handled within DSS */
#define    ORTE_STD_CNTR            (OPAL_DSS_ID_DYNAMIC + 1)  /**< standard counter type */
/* PLM types */
    /* Name-related types */
#define    ORTE_NAME                (OPAL_DSS_ID_DYNAMIC + 2)  /**< an orte_process_name_t */
#define    ORTE_VPID                (OPAL_DSS_ID_DYNAMIC + 3)  /**< a vpid */
#define    ORTE_JOBID               (OPAL_DSS_ID_DYNAMIC + 4)  /**< a jobid */

#if !ORTE_DISABLE_FULL_SUPPORT
    /* State-related types */
#define    ORTE_NODE_STATE          (OPAL_DSS_ID_DYNAMIC + 5)  /**< node status flag */
#define    ORTE_PROC_STATE          (OPAL_DSS_ID_DYNAMIC + 6)  /**< process/resource status */
#define    ORTE_JOB_STATE           (OPAL_DSS_ID_DYNAMIC + 7)  /**< job status flag */
#define    ORTE_EXIT_CODE           (OPAL_DSS_ID_DYNAMIC + 8)  /**< process exit code */
    /* Data-passing types */
#define    ORTE_VALUE               (OPAL_DSS_ID_DYNAMIC + 9)  /**< registry return value */
    /* Resource types */
#define    ORTE_APP_CONTEXT         (OPAL_DSS_ID_DYNAMIC + 10) /**< argv and enviro arrays */
#define    ORTE_NODE_DESC           (OPAL_DSS_ID_DYNAMIC + 11) /**< describes capabilities of nodes */
#define    ORTE_SLOT_DESC           (OPAL_DSS_ID_DYNAMIC + 12) /**< describes slot allocations/reservations */
#define    ORTE_JOB                 (OPAL_DSS_ID_DYNAMIC + 13) /**< job information */
#define    ORTE_NODE                (OPAL_DSS_ID_DYNAMIC + 14) /**< node information */
#define    ORTE_PROC                (OPAL_DSS_ID_DYNAMIC + 15) /**< process information */
#define    ORTE_JOB_MAP             (OPAL_DSS_ID_DYNAMIC + 16) /**< map of process locations */

/* RML types */
#define    ORTE_RML_TAG             (OPAL_DSS_ID_DYNAMIC + 17) /**< tag for sending/receiving messages */

/* DAEMON command type */
#define    ORTE_DAEMON_CMD          (OPAL_DSS_ID_DYNAMIC + 18) /**< command flag for communicating with the daemon */

/* GRPCOMM types */
#define    ORTE_GRPCOMM_MODE        (OPAL_DSS_ID_DYNAMIC + 19) 

/* IOF types */
#define    ORTE_IOF_TAG             (OPAL_DSS_ID_DYNAMIC + 20)

#endif /* !ORTE_DISABLE_FULL_SUPPORT */

#endif
