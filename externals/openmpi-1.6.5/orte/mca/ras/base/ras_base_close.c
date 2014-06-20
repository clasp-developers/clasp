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
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <stdio.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/mca/ras/base/base.h"

#include "orte/mca/ras/base/ras_private.h"

int orte_ras_base_finalize(void)
{
    if (NULL != orte_ras_base.active_module) {
        orte_ras_base.active_module->finalize();
    }
    
    return ORTE_SUCCESS;
}


int orte_ras_base_close(void)
{
        /* Close all remaining available components (may be one if this is a
        Open RTE program, or [possibly] multiple if this is ompi_info) */

        mca_base_components_close(orte_ras_base.ras_output, 
                                  &orte_ras_base.ras_opened, NULL, true);
  
    return ORTE_SUCCESS;
}
