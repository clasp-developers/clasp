/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_MCA_COMMON_MX_H
#define OMPI_MCA_COMMON_MX_H

#include "ompi_config.h"
#include "myriexpress.h"


/**
 * Initialize mx library
 * *
 * @retval OMPI_SUCCESS MX successfully initialized
 * @retval OMPI_ERR_NOT_AVAILABLE MX could not be initialized
 */
OMPI_DECLSPEC int ompi_common_mx_initialize(void);


/**
 * Shut down mx library 
 *
 */
OMPI_DECLSPEC int ompi_common_mx_finalize(void);



#endif /* OMPI_MCA_COMMON_PORTALS_H */
