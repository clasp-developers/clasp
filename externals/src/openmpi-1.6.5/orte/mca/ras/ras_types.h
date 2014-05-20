/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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

#ifndef ORTE_MCA_RAS_TYPES_H
#define ORTE_MCA_RAS_TYPES_H

#include "orte_config.h"
#include "orte/types.h"

#include "opal/class/opal_list.h"

BEGIN_C_DECLS

struct orte_ras_proc_t{
    /** Base object */
    opal_list_item_t super;
    /** String node name */
    char *node_name;
    /* the rank of this proc (default -1) */
    orte_std_cntr_t rank;
    /* cpu list as defined (if defined) in the hostfile */
    char *cpu_list;
    /* whther or not this process is allocated to a map */
};

typedef struct orte_ras_proc_t orte_ras_proc_t;

ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_ras_proc_t);


END_C_DECLS

#endif
