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
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_F77_STRINGS_H
#define OMPI_F77_STRINGS_H

#include "ompi_config.h"

BEGIN_C_DECLS
    /**
     * Convert a fortran string to a C string.
     *
     * @param fstr Fortran string
     * @param len Fortran string length
     * @param cstr Pointer to C string that will be created and returned 
     *
     * @retval OMPI_SUCCESS upon success
     * @retval OMPI_ERROR upon error
     *
     * This function is intended to be used in the MPI F77 bindings to
     * convert fortran strings to C strings before invoking a back-end
     * MPI C binding function.  It will create a new C string and
     * assign it to the cstr to return.  The caller is responsible for
     * eventually freeing the C string.
     */
    OMPI_DECLSPEC int ompi_fortran_string_f2c(char *fstr, int len, char **cstr);

    /**
     * Convert a C string to a fortran string.
     *
     * @param cstr C string
     * @param fstr Fortran string (must already exist and be allocated)
     * @param len Fortran string length
     *
     * @retval OMPI_SUCCESS upon success
     * @retval OMPI_ERROR upon error
     *
     * This function is intended to be used in the MPI F77 bindings to
     * convert C strings to fortran strings.  It is assumed that the
     * fortran string is already allocated and has a length of len.
     */
    OMPI_DECLSPEC int ompi_fortran_string_c2f(char *cstr, char *fstr, int len);

    /**
     * Convert an array of Fortran strings to an argv-style array of C
     * strings.
     *
     * @param farray Array of fortran strings
     * @param string_len Length of each fortran string in the array
     * @param advance Number of bytes to advance to get to the next string
     * @param cargv Returned argv-style array of C strings
     *
     * @retval OMPI_SUCCESS upon success
     * @retval OMPI_ERROR upon error
     *
     * This function is intented to be used in the MPI F77 bindings to
     * convert arrays of fortran strings to argv-style arrays of C
     * strings.  The argv array will be allocated and returned; it is
     * the caller's responsibility to invoke opal_argv_free() to free
     * it later (or equivalent).
     *
     * For 1D Fortran string arrays, advance will == string_len.
     *
     * However, when this function is used (indirectly) for
     * MPI_COMM_SPAWN_MULTIPLE, a 2D array of Fortran strings is
     * converted to individual C-style argv vectors.  In this case,
     * Fortran will intertwine the strings of the different argv
     * vectors in memory; the displacement between the beginning of 2
     * strings in a single argv vector is (string_len *
     * number_of_argv_arrays).  Hence, the advance parameter is used
     * to specify this displacement.
     */
    OMPI_DECLSPEC int ompi_fortran_argv_f2c(char *farray, int string_len, 
                                            int advancex, char ***cargv);

    /**
     * Convert an array of argvs to a C style array of argvs
     * @param count Dimension of the array of argvs
     * @param array Array of fortran argv
     * @param len Length of Fortran array
     * @param argv Returned C arrray of argvs
     *
     * This function is intented to be used in the MPI F77 bindings to
     * convert arrays of fortran strings to argv-style arrays of C
     * strings.  The argv array will be allocated and returned; it is
     * the caller's responsibility to invoke opal_argv_free() to free
     * each content of argv array and call free to deallocate the argv
     * array itself
     */
    OMPI_DECLSPEC int ompi_fortran_multiple_argvs_f2c(int count, char *array, int len,
                                                      char ****argv);

END_C_DECLS


#endif /* OMPI_F77_STRINGS_H */
