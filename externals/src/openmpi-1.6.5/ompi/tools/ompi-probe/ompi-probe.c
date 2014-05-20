/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"
#include "mpi.h"

#include <stdio.h>
#include <ctype.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#include <stdlib.h>

#include "opal/mca/base/base.h"
#include "opal/util/opal_environ.h"
#include "opal/runtime/opal.h"
#include "opal/runtime/opal_cr.h"


/*
 * Globals
 */

int main(int argc, char *argv[])
{
    char * tmp_env_var = NULL;

    /* init enough of opal to use a few utilities */
    if (OPAL_SUCCESS != opal_init_util(&argc, &argv)) {
        fprintf(stderr, "OPAL failed to initialize -- ompi-probe aborting\n");
        exit(1);
    }
    
#if OPAL_ENABLE_FT_CR == 1
    /* Disable the checkpoint notification routine for this
     * tool. As we will never need to checkpoint this tool.
     * Note: This must happen before opal_init().
     */
    opal_cr_set_enabled(false);
    
    /* Select the none component, since we don't actually use a checkpointer */
    tmp_env_var = mca_base_param_env_var("crs");
    opal_setenv(tmp_env_var,
                "none",
                true, &environ);
    free(tmp_env_var);
    tmp_env_var = NULL;

    /* Mark as a tool program */
    tmp_env_var = mca_base_param_env_var("opal_cr_is_tool");
    opal_setenv(tmp_env_var,
                "1",
                true, &environ);
    free(tmp_env_var);
#else
    tmp_env_var = NULL; /* Silence compiler warning */
#endif

    /* open up and select all the frameworks - this will generate the
     * profiled output
     */
    MPI_Init(NULL, NULL);
    
    /* Finalize and clean up ourselves */
    MPI_Finalize();
    
    return 0;
}
