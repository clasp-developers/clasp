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


#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/maffinity/maffinity.h"
#include "opal/mca/maffinity/base/base.h"

int opal_maffinity_base_set(opal_maffinity_base_segment_t *segments,
                            size_t num_segments)
{
    if (!opal_maffinity_base_selected) {
        return OPAL_ERR_NOT_FOUND;
    }
    return opal_maffinity_base_module->maff_module_set(segments, num_segments);
}

int opal_maffinity_base_node_name_to_id(char *node_name, int *node_id)
{
    if (!opal_maffinity_base_selected) {
        return OPAL_ERR_NOT_FOUND;
    }

    if (!opal_maffinity_base_module->maff_module_name_to_id) {
        *node_id = 0;
        return OPAL_ERR_NOT_IMPLEMENTED;
    }

    return opal_maffinity_base_module->maff_module_name_to_id(node_name,
            node_id);
}

int opal_maffinity_base_bind(opal_maffinity_base_segment_t *segments,
        size_t num_segments, int node_id)
{
    if (!opal_maffinity_base_selected) {
        return OPAL_ERR_NOT_FOUND;
    }

    if (!opal_maffinity_base_module->maff_module_bind) {
        return OPAL_ERR_NOT_IMPLEMENTED;
    }

    return opal_maffinity_base_module->maff_module_bind(segments, num_segments,
            node_id);
}
