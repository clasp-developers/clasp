/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/filem/filem.h"
#include "orte/mca/filem/base/base.h"

int orte_filem_base_close(void)
{
    /* Close the selected component */
    if( NULL != orte_filem.filem_finalize ) {
        orte_filem.filem_finalize();
    }

    orte_filem_base_is_active = false;

    /* Close all available modules that are open */
    mca_base_components_close(orte_filem_base_output,
                              &orte_filem_base_components_available,
                              NULL, true);
    
    return ORTE_SUCCESS;
}
