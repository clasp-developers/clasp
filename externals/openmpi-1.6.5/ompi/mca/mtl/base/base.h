/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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

#ifndef MCA_MTL_BASE_H
#define MCA_MTL_BASE_H

#include "ompi_config.h"

#include "opal/mca/mca.h"
#include "ompi/mca/mtl/mtl.h"


/*
 * Global functions for the MTL
 */

BEGIN_C_DECLS

OMPI_DECLSPEC extern mca_mtl_base_component_t* ompi_mtl_base_selected_component;
    
OMPI_DECLSPEC int ompi_mtl_base_open(void);
OMPI_DECLSPEC int ompi_mtl_base_select(bool enable_progress_threads,
                                       bool enable_mpi_threads);
OMPI_DECLSPEC int ompi_mtl_base_close(void);


OMPI_DECLSPEC extern opal_list_t ompi_mtl_base_components_opened;
OMPI_DECLSPEC extern int ompi_mtl_base_output;

END_C_DECLS
#endif /* MCA_MTL_BASE_H */
