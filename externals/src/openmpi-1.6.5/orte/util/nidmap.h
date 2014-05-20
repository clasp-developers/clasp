/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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
 * Populates global structure with system-specific information.
 *
 * Notes: add limits.h, compute size of integer and other types via sizeof(type)*CHAR_BIT
 *
 */

#ifndef _ORTE_NIDMAP_H_
#define _ORTE_NIDMAP_H_

#include "orte_config.h"
#include "orte/types.h"

#include "opal/dss/dss_types.h"

#include "orte/runtime/orte_globals.h"

BEGIN_C_DECLS

#define ORTE_MAX_NODE_PREFIX        50
#define ORTE_CONTIG_NODE_CMD        0x01
#define ORTE_NON_CONTIG_NODE_CMD    0x02

ORTE_DECLSPEC int orte_util_nidmap_init(opal_buffer_t *buffer);
ORTE_DECLSPEC void orte_util_nidmap_finalize(void);
ORTE_DECLSPEC int orte_util_setup_local_nidmap_entries(void);

ORTE_DECLSPEC orte_jmap_t* orte_util_lookup_jmap(orte_jobid_t job);
ORTE_DECLSPEC orte_pmap_t* orte_util_lookup_pmap(orte_process_name_t *proc);
ORTE_DECLSPEC orte_nid_t* orte_util_lookup_nid(orte_process_name_t *proc);

ORTE_DECLSPEC int orte_util_encode_nodemap(opal_byte_object_t *boptr);
ORTE_DECLSPEC int orte_util_decode_nodemap(opal_byte_object_t *boptr);

ORTE_DECLSPEC int orte_util_encode_pidmap(opal_byte_object_t *boptr);
ORTE_DECLSPEC int orte_util_decode_pidmap(opal_byte_object_t *boptr);

ORTE_DECLSPEC int orte_util_build_daemon_nidmap(char **nodes);

ORTE_DECLSPEC void orte_nidmap_dump(void);
ORTE_DECLSPEC void orte_jmap_dump(orte_jmap_t *jmap);
ORTE_DECLSPEC void orte_jobmap_dump(void);

END_C_DECLS
#endif
