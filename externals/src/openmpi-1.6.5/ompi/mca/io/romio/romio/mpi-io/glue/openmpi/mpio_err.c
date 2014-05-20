/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   Copyright (C) 2004 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include <stdarg.h>
#include <stdio.h>

#include "mpioimpl.h"
#include "adio_extern.h"

/* Default error handling implementation.
 *
 * Note that only MPI_ERRORS_ARE_FATAL and MPI_ERRORS_RETURN are
 * handled correctly; other handlers cause an abort.
 */

int MPIO_Err_create_code(int lastcode, int fatal, const char fcname[],
			 int line, int error_class, const char generic_msg[],
			 const char specific_msg[], ... )
{
    va_list Argp;
    int idx = 0;
    char *buf;

    buf = (char *) ADIOI_Malloc(1024);
    if (buf != NULL) {
	idx += ADIOI_Snprintf(buf, 1023, "%s (line %d): ", fcname, line);
	if (specific_msg == NULL) {
	    ADIOI_Snprintf(&buf[idx], 1023 - idx, "%s\n", generic_msg);
	}
	else {
	    va_start(Argp, specific_msg);
	    vsnprintf(&buf[idx], 1023 - idx, specific_msg, Argp);
	    va_end(Argp);
	}
	ADIOI_Free(buf);
    }

    return error_class;
}

int MPIO_Err_return_file(MPI_File mpi_fh, int error_code)
{
    return error_code;
}

int MPIO_Err_return_comm(MPI_Comm mpi_comm, int error_code)
{
    return error_code;
}
