/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_delete = PMPI_File_delete
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_delete MPI_File_delete
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_delete as PMPI_File_delete
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/*@
    MPI_File_delete - Deletes a file

Input Parameters:
. filename - name of file to delete (string)
. info - info object (handle)

.N fortran
@*/
int MPI_File_delete(char *filename, MPI_Info info)
{
    int error_code, file_system;
    char *tmp;
    ADIOI_Fns *fsops;
#ifdef MPI_hpux
    int fl_xmpi;
  
    HPMP_IO_START(fl_xmpi, BLKMPIFILEDELETE, TRDTBLOCK,
                MPI_FILE_NULL, MPI_DATATYPE_NULL, -1);
#endif /* MPI_hpux */

    MPIU_UNREFERENCED_ARG(info);

    MPIU_THREAD_CS_ENTER(ALLFUNC,);

    MPIR_MPIOInit(&error_code);
    if (error_code != MPI_SUCCESS) goto fn_exit;

    /* resolve file system type from file name; this is a collective call */
    ADIO_ResolveFileType(MPI_COMM_SELF, filename, &file_system, &fsops, 
			 &error_code);

    /* --BEGIN ERROR HANDLING-- */
    if (error_code != MPI_SUCCESS)
    {
	/* ADIO_ResolveFileType() will print as informative a message as it
	 * possibly can or call MPIR_Err_setmsg.  We just need to propagate 
	 * the error up.  In the PRINT_ERR_MSG case MPI_Abort has already
	 * been called as well, so we probably didn't even make it this far.
	 */
	error_code = MPIO_Err_return_file(MPI_FILE_NULL, error_code);
	goto fn_exit;
    }
    /* --END ERROR HANDLING-- */

    /* skip prefixes on file names if they have more than one character;
     * single-character prefixes are assumed to be windows drive
     * specifications (e.g. c:\foo) and are left alone.
     */
    tmp = strchr(filename, ':');
    if (tmp > filename + 1)
	filename = tmp + 1;

    /* call the fs-specific delete function */
    (fsops->ADIOI_xxx_Delete)(filename, &error_code);
    /* --BEGIN ERROR HANDLING-- */
    if (error_code != MPI_SUCCESS)
	error_code = MPIO_Err_return_file(MPI_FILE_NULL, error_code);
    /* --END ERROR HANDLING-- */
	
#ifdef MPI_hpux
    HPMP_IO_END(fl_xmpi, MPI_FILE_NULL, MPI_DATATYPE_NULL, -1);
#endif /* MPI_hpux */

fn_exit:
    MPIU_THREAD_CS_EXIT(ALLFUNC,);
    return error_code;
}
