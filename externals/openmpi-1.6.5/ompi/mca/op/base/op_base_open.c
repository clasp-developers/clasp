/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2009 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "ompi/constants.h"
#include "ompi/mca/op/op.h"
#include "ompi/mca/op/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "ompi/mca/op/base/static-components.h"


/*
 * Globals
 */
int ompi_op_base_output = -1;
bool ompi_op_base_components_opened_valid = false;
opal_list_t ompi_op_base_components_opened;

static void module_constructor(ompi_op_base_module_t *m)
{
    m->opm_enable = NULL;
    m->opm_op = NULL;
    memset(&(m->opm_fns), 0, sizeof(m->opm_fns));
    memset(&(m->opm_3buff_fns), 0, sizeof(m->opm_3buff_fns));
}

static void module_constructor_1_0_0(ompi_op_base_module_1_0_0_t *m)
{
    m->opm_enable = NULL;
    m->opm_op = NULL;
    memset(&(m->opm_fns), 0, sizeof(m->opm_fns));
    memset(&(m->opm_3buff_fns), 0, sizeof(m->opm_3buff_fns));
}

OBJ_CLASS_INSTANCE(ompi_op_base_module_t, opal_object_t, 
                   module_constructor, NULL);
OBJ_CLASS_INSTANCE(ompi_op_base_module_1_0_0_t, opal_object_t, 
                   module_constructor_1_0_0, NULL);

/*
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int ompi_op_base_open(void)
{
    int value;

    /* Debugging / verbose output */

    mca_base_param_reg_int_name("op", "base_verbose",
                                "Verbosity level of the op framework",
                                false, false, 0, &value);
    if (0 != value) {
        ompi_op_base_output = opal_output_open(NULL);
    } else {
        ompi_op_base_output = -1;
    }

    /* Open up all available components */

    if (OPAL_SUCCESS !=
        mca_base_components_open("op", ompi_op_base_output,
                                 mca_op_base_static_components,
                                 &ompi_op_base_components_opened, true)) {
        return OPAL_ERROR;
    }
    ompi_op_base_components_opened_valid = true;

    /* All done */

    return OPAL_SUCCESS;
}
