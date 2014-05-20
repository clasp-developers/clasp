/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *  (C) 2009 UChicago/Argonne LLC
 *      See COPYRIGHT in top-level directory.
 */
#include <string.h>
#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS
/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

extern int ADIO_Init_keyval;

/* common code to stuff an attribute on a communicator for the purpose of
 * cleaning up in MPI_Finalize() */

void MPIR_MPIOInit(int * error_code) {

    int flag;
    char myname[] = "MPIR_MPIOInit";

    /* first check if ADIO has been initialized. If not, initialize it */
    if (ADIO_Init_keyval == MPI_KEYVAL_INVALID) {
        MPI_Initialized(&flag);

	/* --BEGIN ERROR HANDLING-- */
        if (!flag) {
	    *error_code = MPIO_Err_create_code(MPI_SUCCESS, 
		    MPIR_ERR_RECOVERABLE, myname, __LINE__, 
		    MPI_ERR_OTHER, "**initialized", 0);
	    *error_code = MPIO_Err_return_file(MPI_FILE_NULL, *error_code);
	    return;
	}
	/* --END ERROR HANDLING-- */

        MPI_Keyval_create(MPI_NULL_COPY_FN, ADIOI_End_call, &ADIO_Init_keyval,
                          (void *) 0);  

	/* put a dummy attribute on MPI_COMM_SELF, because we want the delete
	   function to be called when MPI_COMM_SELF is freed. Clarified
	   in MPI-2 section 4.8, the standard mandates that attributes on
	   MPI_COMM_SELF get cleaned up early in MPI_Finalize */

        MPI_Attr_put(MPI_COMM_SELF, ADIO_Init_keyval, (void *) 0);

	/* initialize ADIO */
        ADIO_Init( (int *)0, (char ***)0, error_code);
    }
    *error_code = MPI_SUCCESS;
}
/* 
 * vim: ts=8 sts=4 sw=4 noexpandtab 
 */
