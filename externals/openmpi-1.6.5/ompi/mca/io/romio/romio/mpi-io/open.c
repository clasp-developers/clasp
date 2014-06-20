/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_open = PMPI_File_open
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_open MPI_File_open
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_open as PMPI_File_open
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

/* for user-definde reduce operator */
#include "adio_extern.h"


extern int ADIO_Init_keyval;

/*@
    MPI_File_open - Opens a file

Input Parameters:
. comm - communicator (handle)
. filename - name of file to open (string)
. amode - file access mode (integer)
. info - info object (handle)

Output Parameters:
. fh - file handle (handle)

.N fortran
@*/
int MPI_File_open(MPI_Comm comm, char *filename, int amode, 
                  MPI_Info info, MPI_File *fh)
{
    int error_code, file_system, flag, tmp_amode=0, rank;
    char *tmp;
    MPI_Comm dupcomm;
    ADIOI_Fns *fsops;
    static char myname[] = "MPI_FILE_OPEN";
#ifdef MPI_hpux
    int fl_xmpi;

    HPMP_IO_OPEN_START(fl_xmpi, comm);
#endif /* MPI_hpux */

    MPIU_THREAD_CS_ENTER(ALLFUNC,);

    /* --BEGIN ERROR HANDLING-- */
    if (comm == MPI_COMM_NULL)
    {
	error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					  myname, __LINE__, MPI_ERR_COMM,
					  "**comm", 0);
	goto fn_fail;
    }
    /* --END ERROR HANDLING-- */

    MPI_Comm_test_inter(comm, &flag);
    /* --BEGIN ERROR HANDLING-- */
    if (flag)
    {
	error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					  myname, __LINE__, MPI_ERR_COMM, 
					  "**commnotintra", 0);
	goto fn_fail;
    }

    if ( ((amode&MPI_MODE_RDONLY)?1:0) + ((amode&MPI_MODE_RDWR)?1:0) +
	 ((amode&MPI_MODE_WRONLY)?1:0) != 1 )
    {
	error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					  myname, __LINE__, MPI_ERR_AMODE, 
					  "**fileamodeone", 0);
	goto fn_fail;
    }

    if ((amode & MPI_MODE_RDONLY) && 
            ((amode & MPI_MODE_CREATE) || (amode & MPI_MODE_EXCL)))
    {
	error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					  myname, __LINE__, MPI_ERR_AMODE, 
					  "**fileamoderead", 0);
	goto fn_fail;
    }

    if ((amode & MPI_MODE_RDWR) && (amode & MPI_MODE_SEQUENTIAL))
    {
	error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					  myname, __LINE__, MPI_ERR_AMODE, 
					  "**fileamodeseq", 0);
	goto fn_fail;
    }

    MPI_Comm_dup(comm, &dupcomm);

/* check if ADIO has been initialized. If not, initialize it */
    MPIR_MPIOInit(&error_code);
    if (error_code != MPI_SUCCESS) goto fn_fail;

/* check if amode is the same on all processes */
    MPI_Allreduce(&amode, &tmp_amode, 1, MPI_INT, ADIO_same_amode, dupcomm);

    if (tmp_amode == ADIO_AMODE_NOMATCH) {
	error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
			myname, __LINE__, MPI_ERR_AMODE,
			"**fileamodediff", 0);
	goto fn_fail;
    }
    /* --END ERROR HANDLING-- */

    file_system = -1;

    /* resolve file system type from file name; this is a collective call */
    ADIO_ResolveFileType(dupcomm, filename, &file_system, &fsops, &error_code);
    /* --BEGIN ERROR HANDLING-- */
    if (error_code != MPI_SUCCESS)
    {
	/* ADIO_ResolveFileType() will print as informative a message as it
	 * possibly can or call MPIO_Err_setmsg.  We just need to propagate 
	 * the error up.
	 */
	goto fn_fail;
    }

    /* --END ERROR HANDLING-- */

    /* strip off prefix if there is one, but only skip prefixes
     * if they are greater than length one to allow for windows
     * drive specifications (e.g. c:\...) */

    tmp = strchr(filename, ':');
    if (tmp > filename + 1) {
	filename = tmp + 1;
    }

/* use default values for disp, etype, filetype */    

    *fh = ADIO_Open(comm, dupcomm, filename, file_system, fsops, amode, 0,
		    MPI_BYTE, MPI_BYTE, info, ADIO_PERM_NULL, &error_code);

    /* --BEGIN ERROR HANDLING-- */
    if (error_code != MPI_SUCCESS) {
        MPI_Comm_free(&dupcomm);
	goto fn_fail;
    }
    /* --END ERROR HANDLING-- */

    /* if MPI_MODE_SEQUENTIAL requested, file systems cannot do explicit offset
     * or independent file pointer accesses, leaving not much else aside from
     * shared file pointer accesses. */
    if ( !ADIO_Feature((*fh), ADIO_SHARED_FP) && (amode & MPI_MODE_SEQUENTIAL)) 
    {
        error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE, 
			                  myname, __LINE__, 
					  MPI_ERR_UNSUPPORTED_OPERATION,
					  "**iosequnsupported", 0);
	ADIO_Close(*fh, &error_code);
	goto fn_fail;
    }

    /* determine name of file that will hold the shared file pointer */
    /* can't support shared file pointers on a file system that doesn't
       support file locking. */
    if ((error_code == MPI_SUCCESS) && 
		    ADIO_Feature((*fh), ADIO_SHARED_FP)) {
	MPI_Comm_rank(dupcomm, &rank);
	ADIOI_Shfp_fname(*fh, rank);

        /* if MPI_MODE_APPEND, set the shared file pointer to end of file.
           indiv. file pointer already set to end of file in ADIO_Open. 
           Here file view is just bytes. */
	if ((*fh)->access_mode & MPI_MODE_APPEND) {
	    if (rank == (*fh)->hints->ranklist[0])  /* only one person need set the sharedfp */
		    ADIO_Set_shared_fp(*fh, (*fh)->fp_ind, &error_code);
	    MPI_Barrier(dupcomm);
	}
    }

#ifdef MPI_hpux
    HPMP_IO_OPEN_END(fl_xmpi, *fh, comm);
#endif /* MPI_hpux */

fn_exit:
    MPIU_THREAD_CS_EXIT(ALLFUNC,);
    return error_code;
fn_fail:
    /* --BEGIN ERROR HANDLING-- */
    error_code = MPIO_Err_return_file(MPI_FILE_NULL, error_code);
    goto fn_exit;
    /* --END ERROR HANDLING-- */
}
