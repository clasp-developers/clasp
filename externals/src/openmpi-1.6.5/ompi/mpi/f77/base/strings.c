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
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "ompi/constants.h"
#include "opal/util/argv.h"
#include "ompi/mpi/f77/f77_strings.h"


/*
 * creates a C string from an F77 string
 */
int ompi_fortran_string_f2c(char *fstr, int len, char **cstr)
{
    char *end;
    int i;

    /* Leading and trailing blanks are discarded. */

    end = fstr + len - 1;

    for (i = 0; (i < len) && (' ' == *fstr); ++i, ++fstr) {
        continue;
    }

    if (i >= len) {
        len = 0;
    } else {
        for (; (end > fstr) && (' ' == *end); --end) {
            continue;
        }

        len = end - fstr + 1;
    }

    /* Allocate space for the C string. */

    if (NULL == (*cstr = (char *) malloc(len + 1))) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Copy F77 string into C string and NULL terminate it. */

    if (len > 0) {
        strncpy(*cstr, fstr, len);
    }
    (*cstr)[len] = '\0';

    return OMPI_SUCCESS;
}


/*
 * Copy a C string into a Fortran string.  Note that when Fortran
 * copies strings, even if it operates on subsets of the strings, it
 * is expected to zero out the rest of the string with spaces.  Hence,
 * when calling this function, the "len" parameter should be the
 * compiler-passed length of the entire string, even if you're copying
 * over less than the full string.  Specifically:
 *
 * http://www.ibiblio.org/pub/languages/fortran/ch2-13.html
 *
 * "Whole operations 'using' only 'part' of it, e.g. assignment of a
 * shorter string, or reading a shorter record, automatically pads the
 * rest of the string with blanks."
 */
int ompi_fortran_string_c2f(char *cstr, char *fstr, int len)
{
    int i;

    strncpy(fstr, cstr, len);
    for (i = strlen(cstr); i < len; ++i) {
        fstr[i] = ' ';
    }

    return OMPI_SUCCESS;
}


/*
 * creates a C argument vector from an F77 array of strings
 * (terminated by a blank string)
 */
int ompi_fortran_argv_f2c(char *array, int string_len, int advance, 
                          char ***argv)
{
    int err, argc = 0;
    char *cstr;

    /* Fortran lines up strings in memory, each delimited by \0.  So
       just convert them until we hit an extra \0. */

    *argv = NULL;
    while (1) {
	if (OMPI_SUCCESS != (err = ompi_fortran_string_f2c(array, string_len, 
                                                           &cstr))) {
	    opal_argv_free(*argv);
	    return err;
	}

	if ('\0' == *cstr) {
	    break;
	}

	if (OMPI_SUCCESS != (err = opal_argv_append(&argc, argv, cstr))) {
	    opal_argv_free(*argv);
	    return err;
	}

	free(cstr);
	array += advance;
    }

    return OMPI_SUCCESS;
}


/*
 * Creates a set of C argv arrays from an F77 array of argv's.  The
 * returned arrays need to be freed by the caller.
 */
int ompi_fortran_multiple_argvs_f2c(int num_argv_arrays, char *array, 
                                    int string_len, char ****argv)
{
    char ***argv_array;
    int i;
    char *current_array = array;
    int ret;

    argv_array = (char ***) malloc (num_argv_arrays * sizeof(char **));

    for (i = 0; i < num_argv_arrays; ++i) {
        ret = ompi_fortran_argv_f2c(current_array, string_len, 
                                    string_len * num_argv_arrays,
                                    &argv_array[i]);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
        current_array += string_len;
    }
    *argv = argv_array;
    return OMPI_SUCCESS;
}
