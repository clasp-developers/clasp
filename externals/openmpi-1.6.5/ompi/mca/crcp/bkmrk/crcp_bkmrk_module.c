/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University.
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

#include "ompi_config.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNIST_H */

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"

#include "opal/util/opal_environ.h"

#include "ompi/mca/crcp/crcp.h"
#include "ompi/mca/crcp/base/base.h"

#include "crcp_bkmrk.h"
#include "crcp_bkmrk_pml.h"

/*
 * Coord module
 */
static ompi_crcp_base_module_t loc_module = {
    /** Initialization Function */
    ompi_crcp_bkmrk_module_init,
    /** Finalization Function */
    ompi_crcp_bkmrk_module_finalize,

    /** PML Wrapper */
    NULL, /* ompi_crcp_bkmrk_pml_enable, */

    NULL, /* ompi_crcp_bkmrk_pml_add_comm, */
    NULL, /* ompi_crcp_bkmrk_pml_del_comm, */

    ompi_crcp_bkmrk_pml_add_procs,
    ompi_crcp_bkmrk_pml_del_procs,

    NULL, /* ompi_crcp_bkmrk_pml_progress, */

    ompi_crcp_bkmrk_pml_iprobe,
    ompi_crcp_bkmrk_pml_probe,

    ompi_crcp_bkmrk_pml_isend_init,
    ompi_crcp_bkmrk_pml_isend,
    ompi_crcp_bkmrk_pml_send,

    ompi_crcp_bkmrk_pml_irecv_init,
    ompi_crcp_bkmrk_pml_irecv,
    ompi_crcp_bkmrk_pml_recv,

    ompi_crcp_bkmrk_pml_dump,
    ompi_crcp_bkmrk_pml_start,

    ompi_crcp_bkmrk_pml_ft_event,

    /* Request Functions */
    ompi_crcp_bkmrk_request_complete,

    /* BTL Wrapper Functions */
    NULL, /* btl_add_procs */
    NULL, /* btl_del_procs */
    NULL, /* btl_register */
    NULL, /* btl_finalize */
    NULL, /* btl_alloc */
    NULL, /* btl_free */
    NULL, /* btl_prepare_src */
    NULL, /* btl_prepare_dst */
    NULL, /* btl_send */
    NULL, /* btl_put */
    NULL, /* btl_get */
    NULL, /* btl_dump */
    NULL /* btl_ft_event */
};

/************************************
 * Locally Global vars & functions :)
 ************************************/

/************************
 * Function Definitions
 ************************/
/*
 * MCA Functions
 */
int ompi_crcp_bkmrk_component_query(mca_base_module_t **module, int *priority)
{
    opal_output_verbose(10, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: component_query()");

    *priority = mca_crcp_bkmrk_component.super.priority;
    *module = (mca_base_module_t *)&loc_module;

    return ORTE_SUCCESS;
}

int ompi_crcp_bkmrk_module_init(void)
{
    opal_output_verbose(10, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: module_init()");

    ompi_crcp_bkmrk_pml_init();

    return OMPI_SUCCESS;
}

int ompi_crcp_bkmrk_module_finalize(void)
{
    opal_output_verbose(10, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: module_finalize()");

    ompi_crcp_bkmrk_pml_finalize();

    return OMPI_SUCCESS;
}

/******************
 * Local functions
 ******************/
