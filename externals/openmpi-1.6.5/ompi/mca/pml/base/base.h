/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_PML_BASE_H
#define MCA_PML_BASE_H

#include "ompi_config.h"

#include "opal/mca/mca.h"
#include "ompi/mca/pml/pml.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_pointer_array.h"

/*
 * Global functions for the PML
 */

BEGIN_C_DECLS

/*
 * This is the base priority for a PML wrapper component
 * If there exists more than one then it is undefined 
 * which one is picked.
 */
#define PML_SELECT_WRAPPER_PRIORITY -128

OMPI_DECLSPEC  int mca_pml_base_open(void);
OMPI_DECLSPEC  int mca_pml_base_progress(void);
OMPI_DECLSPEC  int mca_pml_base_select(bool enable_progress_threads,
                                       bool enable_mpi_threads);
    /* share in modex the name of the selected component */
OMPI_DECLSPEC int mca_pml_base_pml_selected(const char *name);
    /* verify that all new procs are using the currently selected component */
OMPI_DECLSPEC int mca_pml_base_pml_check_selected(const char *my_pml,
                                                  struct ompi_proc_t **procs,
                                                  size_t nprocs);

OMPI_DECLSPEC int mca_pml_base_finalize(void);

OMPI_DECLSPEC int mca_pml_base_close(void);

OMPI_DECLSPEC int mca_pml_base_ft_event(int state);

/*
 * Globals
 */
OMPI_DECLSPEC extern int mca_pml_base_output;
OMPI_DECLSPEC extern opal_list_t mca_pml_base_components_available;
OMPI_DECLSPEC extern mca_pml_base_component_t mca_pml_base_selected_component;
OMPI_DECLSPEC extern mca_pml_base_module_t mca_pml;
OMPI_DECLSPEC extern opal_pointer_array_t mca_pml_base_pml;

END_C_DECLS

#endif /* MCA_PML_BASE_H */
