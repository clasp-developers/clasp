/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/mca/btl/base/base.h" 
#include "ompi/mca/bml/base/base.h" 

int mca_bml_base_close( void )
{
    if (mca_bml_base_already_opened <= 0) {
        return OMPI_ERROR;
    } else if (--mca_bml_base_already_opened > 0) {
        return OMPI_SUCCESS;
    }

    return mca_btl_base_close();
}

