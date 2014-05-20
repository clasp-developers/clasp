/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University.
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

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/constants.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

int opal_crs_base_close(void)
{
    /* Call the component's finalize routine */
    if( NULL != opal_crs.crs_finalize ) {
        opal_crs.crs_finalize();
    }

    /* Close all available modules that are open */
    mca_base_components_close(opal_crs_base_output,
                              &opal_crs_base_components_available,
                              NULL, true);
    
    return OPAL_SUCCESS;
}
