/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_get_info = PMPI_File_get_info
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_get_info MPI_File_get_info
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_get_info as PMPI_File_get_info
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*@
    MPI_File_get_info - Returns the hints for a file that are actually being used by MPI

Input Parameters:
. fh - file handle (handle)

Output Parameters:
. info_used - info object (handle)

.N fortran
@*/
int MPI_File_get_info(MPI_File mpi_fh, MPI_Info *info_used)
{
    int error_code;
    ADIO_File fh;
    static char myname[] = "MPI_FILE_GET_INFO";

    MPIU_THREAD_CS_ENTER(ALLFUNC,);

    fh = MPIO_File_resolve(mpi_fh);

    /* --BEGIN ERROR HANDLING-- */
    MPIO_CHECK_FILE_HANDLE(fh, myname, error_code);
    /* --END ERROR HANDLING-- */

    error_code = MPI_Info_dup(fh->info, info_used);
    /* --BEGIN ERROR HANDLING-- */
    if (error_code != MPI_SUCCESS)
	error_code = MPIO_Err_return_file(fh, error_code);
    /* --END ERROR HANDLING-- */

fn_exit:
    MPIU_THREAD_CS_EXIT(ALLFUNC,);
    return  error_code;
}
