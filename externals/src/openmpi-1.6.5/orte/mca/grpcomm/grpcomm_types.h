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
/** @file:
 *
 * The OpenRTE Group Communications
 *
 * The OpenRTE Group Comm framework provides communication services that
 * span entire jobs or collections of processes. It is not intended to be
 * used for point-to-point communications (the RML does that), nor should
 * it be viewed as a high-performance communication channel for large-scale
 * data transfers.
 */

#ifndef MCA_GRPCOMM_TYPES_H
#define MCA_GRPCOMM_TYPES_H

/*
 * includes
 */

#include "orte_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

BEGIN_C_DECLS

/*
 * Define routing modes
 */
typedef uint8_t orte_grpcomm_mode_t;
#define ORTE_GRPCOMM_MODE_T     OPAL_UINT8

/* daemon N relays message to daemon N+1 */
#define ORTE_GRPCOMM_CHAIN      (orte_grpcomm_mode_t) 1
/* binomial tree */
#define ORTE_GRPCOMM_BINOMIAL   (orte_grpcomm_mode_t) 2
/* linear - HNP sends direct to all daemons */
#define ORTE_GRPCOMM_LINEAR     (orte_grpcomm_mode_t) 3

/*
 * Define collective types
 */
typedef uint8_t orte_grpcomm_coll_t;
#define ORTE_GRPCOMM_COLL_T     OPAL_UINT8

#define ORTE_GRPCOMM_COLL_NONE  0x00
#define ORTE_GRPCOMM_BARRIER    0x01
#define ORTE_GRPCOMM_ALLGATHER  0x02

END_C_DECLS

#endif
