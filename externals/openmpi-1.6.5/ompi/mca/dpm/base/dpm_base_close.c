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

#include "ompi/mca/dpm/dpm.h"
#include "ompi/mca/dpm/base/base.h"

int ompi_dpm_base_close(void)
{
    /* Close the selected component */
    if( NULL != ompi_dpm.finalize ) {
        ompi_dpm.finalize();
    }

    /* Close all available modules that are open */
    mca_base_components_close(ompi_dpm_base_output,
                              &ompi_dpm_base_components_available,
                              NULL, true);
    
    return OMPI_SUCCESS;
}
