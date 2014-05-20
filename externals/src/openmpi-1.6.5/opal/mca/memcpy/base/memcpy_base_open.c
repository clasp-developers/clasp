/*
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/memcpy/memcpy.h"
#include "opal/mca/memcpy/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "opal/mca/memcpy/base/static-components.h"

/*
 * Globals
 */
opal_list_t opal_memcpy_base_components_opened;

/*
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int opal_memcpy_base_open(void)
{
    OBJ_CONSTRUCT( &opal_memcpy_base_components_opened, opal_list_t );

    /* Open up all available components */
    if (OPAL_SUCCESS !=
        mca_base_components_open("memcpy", 0,
                                 mca_memcpy_base_static_components,
                                 &opal_memcpy_base_components_opened, 
                                 true)) {
        return OPAL_ERROR;
    }

    /* All done */
    return OPAL_SUCCESS;
}
