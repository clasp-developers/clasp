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

#include "opal_config.h"

#include <string.h>
#include <stddef.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/constants.h"
#include "opal/mca/maffinity/maffinity.h"
#include "opal/mca/maffinity/base/base.h"
#include "maffinity_first_use.h"

/*
 * Local functions
 */
static int first_use_module_init(void);
static int first_use_module_set(opal_maffinity_base_segment_t *segments,
                                size_t num_segments);

/*
 * First_Use maffinity module
 */
static const opal_maffinity_base_module_1_0_0_t loc_module = {
    /* Initialization function */
    first_use_module_init,

    /* Module function pointers */
    first_use_module_set,
    NULL,
    NULL
};

int opal_maffinity_first_use_component_query(mca_base_module_t **module, int *priority)
{
    int param;

    param = mca_base_param_find("maffinity", "first_use", "priority");
    mca_base_param_lookup_int(param, priority);

    *module = (mca_base_module_t *)&loc_module;

    return OPAL_SUCCESS;
}


static int first_use_module_init(void)
{
    /* Nothing to do */

    return OPAL_SUCCESS;
}


static int first_use_module_set(opal_maffinity_base_segment_t *segments,
                                size_t num_segments)
{
    size_t i;
    uintptr_t pagesize = (uintptr_t)sysconf(_SC_PAGESIZE);
    volatile char useless; 

    for (i = 0; i < num_segments; ++i) {
        char* ptr = (char*)segments[i].mbs_start_addr;
        char* end_ptr = ptr + segments[i].mbs_len;

        /* Let's touch the first byte of the segment. If this is the
         * first byte on the memory page, good. If not, at least it
         * will not overwrite anything important.
         */
        useless = ptr[0];
        /* Compute the address of the first byte on the next page */
        ptr = (char*)((uintptr_t)(ptr + pagesize) & ~pagesize);
        while( ptr <= end_ptr) {
            useless += ptr[0];
            ptr += pagesize;
        };
    }

    return OPAL_SUCCESS;
}
