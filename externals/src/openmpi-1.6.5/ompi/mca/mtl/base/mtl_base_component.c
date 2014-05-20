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
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"


#include "ompi/constants.h"
#include "ompi/mca/mtl/mtl.h"
#include "ompi/mca/mtl/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "ompi/mca/mtl/base/static-components.h"


int ompi_mtl_base_output = 0;
opal_list_t ompi_mtl_base_components_opened;
mca_mtl_base_component_t *ompi_mtl_base_selected_component = NULL;
mca_mtl_base_module_t *ompi_mtl;


/*
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int
ompi_mtl_base_open(void)
{
    /* setup the output stream */
    ompi_mtl_base_output = opal_output_open(NULL);

    /* Open up all available components */
    if (OMPI_SUCCESS != 
        mca_base_components_open("mtl", ompi_mtl_base_output,
                                 mca_mtl_base_static_components, 
                                 &ompi_mtl_base_components_opened,
                                 !MCA_mtl_DIRECT_CALL)) {
        return OMPI_ERROR;
    }


    /* Set a sentinel in case we don't select any components (e.g.,
       ompi_info) */
    ompi_mtl = NULL;

    return OMPI_SUCCESS;
}


/*
 * Function for selecting one component from all those that are
 * available.
 *
 * For now, we take the first component that says it can run.  Might
 * need to reexamine this at a later time.
 */
int
ompi_mtl_base_select(bool enable_progress_threads,
                     bool enable_mpi_threads)
{
    opal_list_item_t *item = NULL;
    mca_base_component_list_item_t *cli = NULL;
    mca_mtl_base_component_t *component = NULL;
    mca_mtl_base_module_t *module = NULL;

    /* Traverse the list of available components; call their init
       functions. */
    for (item = opal_list_get_first(&ompi_mtl_base_components_opened);
         opal_list_get_end(&ompi_mtl_base_components_opened) != item;
         item = opal_list_get_next(item) ) {
        cli = (mca_base_component_list_item_t *) item;
        component = (mca_mtl_base_component_t *) cli->cli_component;

        if (NULL == component->mtl_init) {
            opal_output_verbose( 10, ompi_mtl_base_output,
                                 "select: no init function; ignoring component %s",
                                 component->mtl_version.mca_component_name );
            continue;
        }
        opal_output_verbose( 10, ompi_mtl_base_output, 
                             "select: initializing %s component %s",
                             component->mtl_version.mca_type_name,
                             component->mtl_version.mca_component_name );
        module = component->mtl_init(enable_progress_threads,
                                     enable_mpi_threads);
        if (NULL == module) {
            opal_output_verbose( 10, ompi_mtl_base_output,
                                 "select: init returned failure for component %s",
                                 component->mtl_version.mca_component_name );
            continue;
        }
        opal_output_verbose( 10, ompi_mtl_base_output,
                             "select: init returned success");

        ompi_mtl_base_selected_component = component;
        ompi_mtl = module;
    }

    /* This base function closes, unloads, and removes from the
       available list all unselected components.  The available list will
       contain only the selected component. */
    mca_base_components_close(ompi_mtl_base_output, 
                              &ompi_mtl_base_components_opened, 
                              (mca_base_component_t *) ompi_mtl_base_selected_component,
                              false);

    /* All done */
    if (NULL == module) {
        opal_output_verbose( 10, ompi_mtl_base_output, 
                             "select: no component selected");
        return OMPI_ERR_NOT_FOUND;
    } else {
        opal_output_verbose( 10, ompi_mtl_base_output, 
                             "select: component %s selected",
                             ompi_mtl_base_selected_component->
                             mtl_version.mca_component_name );
        return OMPI_SUCCESS;
    }
}


int
ompi_mtl_base_close(void)
{
    /* Close all remaining available modules (may be one if this is a
       OMPI RTE program, or [possibly] multiple if this is ompi_info) */
    mca_base_components_close(ompi_mtl_base_output, 
                              &ompi_mtl_base_components_opened, NULL, true);

    /* All done */
    return OMPI_SUCCESS;
}
