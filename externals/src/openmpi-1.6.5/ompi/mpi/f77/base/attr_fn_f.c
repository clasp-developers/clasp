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
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/f77/bindings.h"
#include "ompi/mpi/f77/fint_2_int.h"

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_NULL_COPY_FN   = mpi_type_null_copy_fn_f
#pragma weak mpi_type_null_copy_fn   = mpi_type_null_copy_fn_f
#pragma weak mpi_type_null_copy_fn_  = mpi_type_null_copy_fn_f
#pragma weak mpi_type_null_copy_fn__ = mpi_type_null_copy_fn_f
#pragma weak MPI_TYPE_NULL_DELETE_FN   = mpi_type_null_delete_fn_f
#pragma weak mpi_type_null_delete_fn   = mpi_type_null_delete_fn_f
#pragma weak mpi_type_null_delete_fn_  = mpi_type_null_delete_fn_f
#pragma weak mpi_type_null_delete_fn__ = mpi_type_null_delete_fn_f
#pragma weak MPI_TYPE_DUP_FN   = mpi_type_dup_fn_f
#pragma weak mpi_type_dup_fn   = mpi_type_dup_fn_f
#pragma weak mpi_type_dup_fn_  = mpi_type_dup_fn_f
#pragma weak mpi_type_dup_fn__ = mpi_type_dup_fn_f
#pragma weak MPI_COMM_NULL_DELETE_FN   = mpi_comm_null_delete_fn_f
#pragma weak mpi_comm_null_delete_fn   = mpi_comm_null_delete_fn_f
#pragma weak mpi_comm_null_delete_fn_  = mpi_comm_null_delete_fn_f
#pragma weak mpi_comm_null_delete_fn__ = mpi_comm_null_delete_fn_f
#pragma weak MPI_COMM_DUP_FN   = mpi_comm_dup_fn_f
#pragma weak mpi_comm_dup_fn   = mpi_comm_dup_fn_f
#pragma weak mpi_comm_dup_fn_  = mpi_comm_dup_fn_f
#pragma weak mpi_comm_dup_fn__ = mpi_comm_dup_fn_f
#pragma weak MPI_COMM_NULL_COPY_FN   = mpi_comm_null_copy_fn_f
#pragma weak mpi_comm_null_copy_fn   = mpi_comm_null_copy_fn_f
#pragma weak mpi_comm_null_copy_fn_  = mpi_comm_null_copy_fn_f
#pragma weak mpi_comm_null_copy_fn__ = mpi_comm_null_copy_fn_f
#pragma weak MPI_WIN_NULL_DELETE_FN   = mpi_win_null_delete_fn_f
#pragma weak mpi_win_null_delete_fn   = mpi_win_null_delete_fn_f
#pragma weak mpi_win_null_delete_fn_  = mpi_win_null_delete_fn_f
#pragma weak mpi_win_null_delete_fn__ = mpi_win_null_delete_fn_f
#pragma weak MPI_WIN_NULL_COPY_FN   = mpi_win_null_copy_fn_f
#pragma weak mpi_win_null_copy_fn   = mpi_win_null_copy_fn_f
#pragma weak mpi_win_null_copy_fn_  = mpi_win_null_copy_fn_f
#pragma weak mpi_win_null_copy_fn__ = mpi_win_null_copy_fn_f
#pragma weak MPI_WIN_DUP_FN   = mpi_win_dup_fn_f
#pragma weak mpi_win_dup_fn   = mpi_win_dup_fn_f
#pragma weak mpi_win_dup_fn_  = mpi_win_dup_fn_f
#pragma weak mpi_win_dup_fn__ = mpi_win_dup_fn_f
#pragma weak MPI_NULL_COPY_FN   = mpi_null_copy_fn_f
#pragma weak mpi_null_copy_fn   = mpi_null_copy_fn_f
#pragma weak mpi_null_copy_fn_  = mpi_null_copy_fn_f
#pragma weak mpi_null_copy_fn__ = mpi_null_copy_fn_f
#pragma weak MPI_NULL_DELETE_FN = mpi_null_delete_fn_f
#pragma weak mpi_null_delete_fn = mpi_null_delete_fn_f
#pragma weak mpi_null_delete_fn_ = mpi_null_delete_fn_f
#pragma weak mpi_null_delete_fn__ = mpi_null_delete_fn_f
#pragma weak MPI_DUP_FN   = mpi_dup_fn_f
#pragma weak mpi_dup_fn   = mpi_dup_fn_f
#pragma weak mpi_dup_fn_  = mpi_dup_fn_f
#pragma weak mpi_dup_fn__ = mpi_dup_fn_f

#else
OMPI_GENERATE_F77_BINDINGS( MPI_TYPE_NULL_DELETE_FN,
                            mpi_type_null_delete_fn,
                            mpi_type_null_delete_fn_,
                            mpi_type_null_delete_fn__,
                            mpi_type_null_delete_fn_f,
                            (MPI_Fint* type, MPI_Fint* type_keyval, MPI_Aint* attribute_val_out, MPI_Aint* extra_state, MPI_Fint* ierr),
                            (type, type_keyval, attribute_val_out, extra_state, ierr) )
OMPI_GENERATE_F77_BINDINGS( MPI_TYPE_NULL_COPY_FN,
                            mpi_type_null_copy_fn,
                            mpi_type_null_copy_fn_,
                            mpi_type_null_copy_fn__,
                            mpi_type_null_copy_fn_f,
                            (MPI_Fint* type, MPI_Fint* type_keyval, MPI_Aint* extra_state, MPI_Aint* attribute_val_in, MPI_Aint* attribute_val_out, ompi_fortran_logical_t* flag, MPI_Fint* ierr),
                            (type, type_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierr) )
OMPI_GENERATE_F77_BINDINGS( MPI_TYPE_DUP_FN,
                            mpi_type_dup_fn,
                            mpi_type_dup_fn_,
                            mpi_type_dup_fn__,
                            mpi_type_dup_fn_f,
                            (MPI_Fint* type, MPI_Fint* type_keyval, MPI_Aint* extra_state, MPI_Aint* attribute_val_in, MPI_Aint* attribute_val_out, ompi_fortran_logical_t* flag, MPI_Fint* ierr),
                            (type, type_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierr) )
OMPI_GENERATE_F77_BINDINGS( MPI_COMM_NULL_DELETE_FN,
                            mpi_comm_null_delete_fn,
                            mpi_comm_null_delete_fn_,
                            mpi_comm_null_delete_fn__,
                            mpi_comm_null_delete_fn_f,
                            (MPI_Fint* comm, MPI_Fint* comm_keyval,MPI_Aint* attribute_val_out, MPI_Aint* extra_state, MPI_Fint* ierr ),
                            (comm, comm_keyval, attribute_val_out, extra_state, ierr) )
OMPI_GENERATE_F77_BINDINGS( MPI_COMM_NULL_COPY_FN,
                            mpi_comm_null_copy_fn,
                            mpi_comm_null_copy_fn_,
                            mpi_comm_null_copy_fn__,
                            mpi_comm_null_copy_fn_f,
                            (MPI_Fint* comm, MPI_Fint* comm_keyval, MPI_Aint* extra_state, MPI_Aint* attribute_val_in, MPI_Aint* attribute_val_out, ompi_fortran_logical_t* flag, MPI_Fint* ierr),
                            (comm, comm_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierr) )
OMPI_GENERATE_F77_BINDINGS( MPI_COMM_DUP_FN,
                            mpi_comm_dup_fn,
                            mpi_comm_dup_fn_,
                            mpi_comm_dup_fn__,
                            mpi_comm_dup_fn_f,
                            (MPI_Fint* comm, MPI_Fint* comm_keyval, MPI_Aint* extra_state, MPI_Aint* attribute_val_in, MPI_Aint* attribute_val_out, ompi_fortran_logical_t* flag, MPI_Fint* ierr),
                            (comm, comm_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierr) )
OMPI_GENERATE_F77_BINDINGS( MPI_NULL_DELETE_FN,
                            mpi_null_delete_fn,
                            mpi_null_delete_fn_,
                            mpi_null_delete_fn__,
                            mpi_null_delete_fn_f,
                            (MPI_Fint* comm, MPI_Fint* comm_keyval,MPI_Fint* attribute_val_out, MPI_Fint* extra_state, MPI_Fint* ierr ),
                            (comm, comm_keyval, attribute_val_out, extra_state, ierr) )
OMPI_GENERATE_F77_BINDINGS( MPI_NULL_COPY_FN,
                            mpi_null_copy_fn,
                            mpi_null_copy_fn_,
                            mpi_null_copy_fn__,
                            mpi_null_copy_fn_f,
                            (MPI_Fint* comm, MPI_Fint* comm_keyval, MPI_Fint* extra_state, MPI_Fint* attribute_val_in, MPI_Fint* attribute_val_out, ompi_fortran_logical_t* flag, MPI_Fint* ierr),
                            (comm, comm_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierr) )
OMPI_GENERATE_F77_BINDINGS( MPI_DUP_FN,
                            mpi_dup_fn,
                            mpi_dup_fn_,
                            mpi_dup_fn__,
                            mpi_dup_fn_f,
                            (MPI_Fint* comm, MPI_Fint* comm_keyval, MPI_Fint* extra_state, MPI_Fint* attribute_val_in, MPI_Fint* attribute_val_out, ompi_fortran_logical_t* flag, MPI_Fint* ierr),
                            (comm, comm_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierr) )

OMPI_GENERATE_F77_BINDINGS( MPI_WIN_NULL_DELETE_FN,
                            mpi_win_null_delete_fn,
                            mpi_win_null_delete_fn_,
                            mpi_win_null_delete_fn__,
                            mpi_win_null_delete_fn_f,
                            (MPI_Fint* type, MPI_Fint* type_keyval,MPI_Aint* attribute_val_out, MPI_Aint* extra_state, MPI_Fint* ierr ),
                            (type, type_keyval, attribute_val_out, extra_state, ierr) )
OMPI_GENERATE_F77_BINDINGS( MPI_WIN_NULL_COPY_FN,
                            mpi_win_null_copy_fn,
                            mpi_win_null_copy_fn_,
                            mpi_win_null_copy_fn__,
                            mpi_win_null_copy_fn_f,
                            (MPI_Fint* window, MPI_Fint* win_keyval, MPI_Aint* extra_state, MPI_Aint* attribute_val_in, MPI_Aint* attribute_val_out, ompi_fortran_logical_t* flag, MPI_Fint* ierr),
                            (window, win_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierr) )
OMPI_GENERATE_F77_BINDINGS( MPI_WIN_DUP_FN,
                            mpi_win_dup_fn,
                            mpi_win_dup_fn_,
                            mpi_win_dup_fn__,
                            mpi_win_dup_fn_f,
                            (MPI_Fint* window, MPI_Fint* win_keyval, MPI_Aint* extra_state, MPI_Aint* attribute_val_in, MPI_Aint* attribute_val_out, ompi_fortran_logical_t* flag, MPI_Fint* ierr),
                            (window, win_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierr) )
#endif

/*
 * Note that in this file, we invoke OMPI_C_<function> rather than
 * <function>, where <function> is MPI_DUP_FN (and all the rest).
 * Specifically:
 * 
 *   MPI_NULL_DELETE_FN -> OMPI_C_MPI_NULL_DELETE_FN
 *   MPI_NULL_COPY_FN -> OMPI_C_MPI_NULL_COPY_FN
 *   MPI_DUP_FN -> OMPI_C_MPI_DUP_FN
 *
 *   MPI_TYPE_NULL_DELETE_FN -> OMPI_C_MPI_TYPE_NULL_DELETE_FN
 *   MPI_TYPE_NULL_COPY_FN -> OMPI_C_MPI_TYPE_NULL_COPY_FN
 *   MPI_TYPE_DUP_FN -> OMPI_C_MPI_TYPE_DUP_FN
 *
 *   MPI_COMM_NULL_DELETE_FN -> OMPI_C_MPI_COMM_NULL_DELETE_FN
 *   MPI_COMM_NULL_COPY_FN -> OMPI_C_MPI_COMM_NULL_COPY_FN
 *   MPI_COMM_DUP_FN -> OMPI_C_MPI_COMM_DUP_FN
 *
 *   MPI_WIN_NULL_DELETE_FN -> OMPI_C_MPI_WIN_NULL_DELETE_FN
 *   MPI_WIN_NULL_COPY_FN -> OMPI_C_MPI_WIN_NULL_COPY_FN
 *   MPI_WIN_DUP_FN -> OMPI_C_MPI_WIN_DUP_FN
 *
 * The reason why is discussed in a lengthy comment in mpi.h.
 */
void mpi_type_null_delete_fn_f(MPI_Fint* type, MPI_Fint* type_keyval,
                               MPI_Aint* attribute_val_out, 
                               MPI_Aint* extra_state, MPI_Fint* ierr)
{
    *ierr = OMPI_INT_2_FINT(MPI_SUCCESS);
}

void mpi_type_null_copy_fn_f(MPI_Fint* type, MPI_Fint* type_keyval, 
                             MPI_Aint* extra_state,
                             MPI_Aint* attribute_val_in, 
                             MPI_Aint* attribute_val_out,
                             ompi_fortran_logical_t* flag, MPI_Fint* ierr)
{
    *flag = (ompi_fortran_logical_t) 0;
    *ierr = OMPI_INT_2_FINT(MPI_SUCCESS);
}

void mpi_type_dup_fn_f(MPI_Fint* type, MPI_Fint* type_keyval, 
                       MPI_Aint* extra_state,
                       MPI_Aint* attribute_val_in, 
                       MPI_Aint* attribute_val_out, 
                       ompi_fortran_logical_t* flag, MPI_Fint* ierr )
{
    *flag = (ompi_fortran_logical_t) 1;
    *attribute_val_out = *attribute_val_in;
    *ierr = OMPI_INT_2_FINT(MPI_SUCCESS);
}

void mpi_comm_null_delete_fn_f( MPI_Fint* comm, MPI_Fint* comm_keyval,
                                MPI_Aint* attribute_val_out, 
                                MPI_Aint* extra_state, MPI_Fint* ierr )
{
    *ierr = OMPI_INT_2_FINT(MPI_SUCCESS);
}

void mpi_comm_null_copy_fn_f( MPI_Fint* comm, MPI_Fint* comm_keyval, 
                              MPI_Aint* extra_state,
                              MPI_Aint* attribute_val_in, 
                              MPI_Aint* attribute_val_out, 
                              ompi_fortran_logical_t* flag, MPI_Fint* ierr )
{
    *flag = (ompi_fortran_logical_t) 0;
    *ierr = OMPI_INT_2_FINT(MPI_SUCCESS);
}

void mpi_comm_dup_fn_f( MPI_Fint* comm, MPI_Fint* comm_keyval, 
                        MPI_Aint* extra_state,
                        MPI_Aint* attribute_val_in, 
                        MPI_Aint* attribute_val_out, 
                        ompi_fortran_logical_t* flag, MPI_Fint* ierr )
{
    *flag = (ompi_fortran_logical_t) 1;
    *attribute_val_out = *attribute_val_in;
    *ierr = OMPI_INT_2_FINT(MPI_SUCCESS);
}

void mpi_null_delete_fn_f( MPI_Fint* comm, MPI_Fint* comm_keyval,
                           MPI_Fint* attribute_val_out, 
                           MPI_Fint* extra_state, MPI_Fint* ierr )
{
    *ierr = OMPI_INT_2_FINT(MPI_SUCCESS);
}

void mpi_null_copy_fn_f( MPI_Fint* comm, MPI_Fint* comm_keyval, 
                         MPI_Fint* extra_state,
                         MPI_Fint* attribute_val_in, 
                         MPI_Fint* attribute_val_out, 
                         ompi_fortran_logical_t* flag, MPI_Fint* ierr )
{
    *flag = (ompi_fortran_logical_t) 0;
    *ierr = OMPI_INT_2_FINT(MPI_SUCCESS);
}


void mpi_dup_fn_f( MPI_Fint* comm, MPI_Fint* comm_keyval, 
                   MPI_Fint* extra_state,
                   MPI_Fint* attribute_val_in, 
                   MPI_Fint* attribute_val_out, 
                   ompi_fortran_logical_t* flag, MPI_Fint* ierr )
{
    *flag = (ompi_fortran_logical_t) 1;
    *attribute_val_out = *attribute_val_in;
    *ierr = OMPI_INT_2_FINT(MPI_SUCCESS);
}

void mpi_win_null_delete_fn_f( MPI_Fint* window, MPI_Fint* win_keyval,
                               MPI_Aint* attribute_val_out, 
                               MPI_Aint* extra_state, MPI_Fint* ierr )
{
    *ierr = OMPI_INT_2_FINT(MPI_SUCCESS);
}

void mpi_win_null_copy_fn_f( MPI_Fint* window, MPI_Fint* win_keyval, 
                             MPI_Aint* extra_state,
                             MPI_Aint* attribute_val_in, 
                             MPI_Aint* attribute_val_out, 
                             ompi_fortran_logical_t* flag, MPI_Fint* ierr )
{
    *flag = (ompi_fortran_logical_t) 0;
    *ierr = OMPI_INT_2_FINT(MPI_SUCCESS);
}

void mpi_win_dup_fn_f( MPI_Fint* window, MPI_Fint* win_keyval, 
                       MPI_Aint* extra_state,
                       MPI_Aint* attribute_val_in, 
                       MPI_Aint* attribute_val_out, 
                       ompi_fortran_logical_t* flag, MPI_Fint* ierr )
{
    *flag = (ompi_fortran_logical_t) 1;
    *attribute_val_out = *attribute_val_in;
    *ierr = OMPI_INT_2_FINT(MPI_SUCCESS);
}
