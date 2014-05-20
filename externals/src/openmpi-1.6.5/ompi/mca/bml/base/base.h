/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#ifndef MCA_BML_BASE_H
#define MCA_BML_BASE_H

#include "ompi_config.h"

#include "opal/mca/mca.h"
#include "ompi/mca/bml/bml.h"


/*
 * Global functions for the BML
 */

BEGIN_C_DECLS


struct mca_bml_base_selected_module_t {
    opal_list_item_t super;
    mca_bml_base_component_t *bml_component;
    mca_bml_base_module_t *bml_module;
};
typedef struct mca_bml_base_selected_module_t mca_bml_base_selected_module_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_bml_base_selected_module_t); 

/*
 * Global functions for MCA: overall BTL open and close
 */

OMPI_DECLSPEC  int mca_bml_base_open(void);
OMPI_DECLSPEC  int mca_bml_base_init(bool enable_progress_threads, 
                                     bool enable_mpi_threads);
OMPI_DECLSPEC  int mca_bml_base_close(void);
OMPI_DECLSPEC  bool mca_bml_base_inited(void);

OMPI_DECLSPEC  int mca_bml_base_ft_event(int state);


/*
 * Globals
 */
extern int mca_bml_base_already_opened;
OMPI_DECLSPEC extern int mca_bml_base_output;
OMPI_DECLSPEC extern mca_bml_base_component_t mca_bml_component;
OMPI_DECLSPEC extern opal_list_t mca_bml_base_components_available; 
OMPI_DECLSPEC extern mca_bml_base_module_t mca_bml; 

END_C_DECLS
#endif /* MCA_BML_BASE_H */
