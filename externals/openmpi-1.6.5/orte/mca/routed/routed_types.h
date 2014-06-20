/*
 * Copyright (c) 2008      Los Alamos National Security, LLC.
 *                         All rights reserved. 
 * Copyright (c) 2004-2008 The Trustees of Indiana University.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** 
 * @file
 *
 * Type definitions to support routed framework
 */


#ifndef ORTE_MCA_ROUTED_TYPES_H_
#define ORTE_MCA_ROUTED_TYPES_H_

#include "orte_config.h"
#include "orte/types.h"

#include "opal/class/opal_bitmap.h"
#include "opal/class/opal_list.h"

BEGIN_C_DECLS

#if !ORTE_DISABLE_FULL_SUPPORT

/* struct for tracking routing trees */
typedef struct {
    opal_list_item_t super;
    orte_vpid_t vpid;
    opal_bitmap_t relatives;
} orte_routed_tree_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_routed_tree_t);

#endif

END_C_DECLS

#endif
