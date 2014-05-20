/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

/**
 * @file
 * 
 * Hoke CRCP component
 *
 */

#ifndef MCA_CRCP_HOKE_EXPORT_H
#define MCA_CRCP_HOKE_EXPORT_H

#include "ompi_config.h"

#include "opal/mca/mca.h"
#include "ompi/mca/crcp/crcp.h"
#include MCA_timer_IMPLEMENTATION_HEADER


BEGIN_C_DECLS

    /*
     * Local Component structures
     */
    struct ompi_crcp_bkmrk_component_t {
        ompi_crcp_base_component_t super;  /** Base CRCP component */
    };
    typedef struct ompi_crcp_bkmrk_component_t ompi_crcp_bkmrk_component_t;
    OMPI_MODULE_DECLSPEC extern ompi_crcp_bkmrk_component_t mca_crcp_bkmrk_component;

    /*
     * Local variables
     */
    extern int timing_enabled;

    /*
     * Module functions
     */
    int ompi_crcp_bkmrk_component_query(mca_base_module_t **module, int *priority);
    int ompi_crcp_bkmrk_module_init(void);
    int ompi_crcp_bkmrk_module_finalize(void);

    int ompi_crcp_bkmrk_pml_init(void);
    int ompi_crcp_bkmrk_pml_finalize(void);

END_C_DECLS

#endif /* MCA_CRCP_HOKE_EXPORT_H */
