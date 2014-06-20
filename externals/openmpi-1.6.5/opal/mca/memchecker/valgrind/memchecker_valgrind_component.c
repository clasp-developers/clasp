/*
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2008 The Trustees of Indiana University.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/memchecker/memchecker.h"
#include "memchecker_valgrind.h"

/*
 * Public string showing the memchecker ompi_linux component version number
 */
const char *opal_memchecker_valgrind_component_version_string =
    "OPAL valgrind memchecker MCA component version " OPAL_VERSION;

/*
 * Local function
 */
static int valgrind_open(void);
static int valgrind_close(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

const opal_memchecker_base_component_2_0_0_t mca_memchecker_valgrind_component = {

    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {
        OPAL_MEMCHECKER_BASE_VERSION_2_0_0,

        /* Component name and version */
        "valgrind",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        valgrind_open,
        valgrind_close,
        opal_memchecker_valgrind_component_query

    },
    {
        /* Valgrind does not offer functionality to save the state  */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};


static int valgrind_open(void)
{
    /*
     * Any initialization of valgrind upon starting of the component
     * should be done here.
     *
     * Possibilities are, that we need to set special stuff, when
     * valgrind is not being run / actually is being run.
     */
    return OPAL_SUCCESS;
}


static int valgrind_close(void)
{
    /*
     * Any closing of valgrind upon starting of the component
     * should be done here.
     *
     * Possibilities are, that we need to set special stuff, when
     * valgrind is not being run / actually is being run.
     */
    return OPAL_SUCCESS;
}

