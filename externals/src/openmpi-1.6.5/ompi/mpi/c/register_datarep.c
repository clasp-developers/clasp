/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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

#include "ompi_config.h"
#include <stdio.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/mca/io/base/base.h"
#include "ompi/file/file.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Register_datarep = PMPI_Register_datarep
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Register_datarep";


int MPI_Register_datarep(char *datarep,
                       MPI_Datarep_conversion_function *read_conversion_fn,
                       MPI_Datarep_conversion_function *write_conversion_fn,
                       MPI_Datarep_extent_function *dtype_file_extent_fn,
                       void *extra_state) 
{
    int rc;

    if (MPI_PARAM_CHECK) {
        rc = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (NULL == datarep) {
            rc = MPI_ERR_ARG;
        }
        OMPI_ERRHANDLER_CHECK(rc, MPI_FILE_NULL, rc, FUNC_NAME);
    }

    /* The io framework is only initialized lazily.  If it hasn't
       already been initialized, do so now (note that MPI_FILE_OPEN
       and MPI_FILE_DELETE are the only two places that it will be
       initialized). */

    if (!(mca_io_base_components_opened_valid ||
          mca_io_base_components_available_valid)) {
        if (OMPI_SUCCESS != (rc = mca_io_base_open())) {
            return OMPI_ERRHANDLER_INVOKE(MPI_FILE_NULL, rc, FUNC_NAME);
        }
        if (OMPI_SUCCESS != 
            (rc = mca_io_base_find_available(OPAL_ENABLE_PROGRESS_THREADS,
                                             OMPI_ENABLE_THREAD_MULTIPLE))) {
            return OMPI_ERRHANDLER_INVOKE(MPI_FILE_NULL, rc, FUNC_NAME);
        }
    }

    OPAL_CR_ENTER_LIBRARY();

    /* Call the back-end io component function */
    rc = mca_io_base_register_datarep(datarep, read_conversion_fn,
                                      write_conversion_fn,
                                      dtype_file_extent_fn,
                                      extra_state);


    /* All done */
    
    OMPI_ERRHANDLER_RETURN(rc, MPI_FILE_NULL, rc, FUNC_NAME);
}
