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

#include "ompi_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "opal/mca/base/mca_base_param.h"

#include "ompi/mca/crcp/crcp.h"
#include "ompi/mca/crcp/base/base.h"

int ompi_crcp_base_close(void)
{
    /* Close the selected component */
    if( NULL != ompi_crcp.crcp_finalize ) {
        ompi_crcp.crcp_finalize();
    }

    /* Close all available modules that are open */
    mca_base_components_close(ompi_crcp_base_output,
                              &ompi_crcp_base_components_available,
                              NULL, true);
    
    return OMPI_SUCCESS;
}
