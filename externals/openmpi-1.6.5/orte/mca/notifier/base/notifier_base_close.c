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

#include <stdio.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "orte/mca/notifier/base/base.h"


int orte_notifier_base_close(void)
{
    /* If we have a selected component and module, then finalize it */
    
    if (NULL != orte_notifier.finalize) {
        orte_notifier.finalize();
    }
    
    /* Close all remaining available components (may be one if this is a
     OpenRTE program, or [possibly] multiple if this is ompi_info) */
    
    mca_base_components_close(orte_notifier_base_output, 
                              &mca_notifier_base_components_available, 
                              NULL, true);
    
    /* All done */
    
    return ORTE_SUCCESS;
}
