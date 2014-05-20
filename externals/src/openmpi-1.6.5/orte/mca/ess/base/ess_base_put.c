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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdlib.h>
#include <errno.h>

#include "opal/util/opal_environ.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/ess/base/base.h"

int orte_ess_env_put(orte_std_cntr_t num_procs,
                        orte_std_cntr_t num_local_procs,
                        char ***env)
{
    char* param;
    char* value;

    /* tell the ESS to select the env component */
    if(NULL == (param = mca_base_param_environ_variable("ess",NULL,NULL))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    opal_setenv(param, "env", true, env);
    free(param);

    /* since we want to pass the name as separate components, make sure
     * that the "name" environmental variable is cleared!
     */
    if(NULL == (param = mca_base_param_environ_variable("orte","ess","name"))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    opal_unsetenv(param, env);
    free(param);

    asprintf(&value, "%ld", (long) num_procs);
    if(NULL == (param = mca_base_param_environ_variable("orte","ess","num_procs"))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    opal_setenv(param, value, true, env);
    free(param);
    /* although the num_procs is the comm_world size, users
     * would appreciate being given a public environmental variable
     * that also represents this value - something MPI specific - so
     * do that here.
     *
     * AND YES - THIS BREAKS THE ABSTRACTION BARRIER TO SOME EXTENT.
     * We know - just live with it
     */
    opal_setenv("OMPI_COMM_WORLD_SIZE", value, true, env);
    free(value);

    /* users would appreciate being given a public environmental variable
     * that also represents this value - something MPI specific - so
     * do that here.
     *
     * AND YES - THIS BREAKS THE ABSTRACTION BARRIER TO SOME EXTENT.
     * We know - just live with it
     */
    asprintf(&value, "%ld", (long) num_local_procs);
    opal_setenv("OMPI_COMM_WORLD_LOCAL_SIZE", value, true, env);
    free(value);
    
    return ORTE_SUCCESS;
}
