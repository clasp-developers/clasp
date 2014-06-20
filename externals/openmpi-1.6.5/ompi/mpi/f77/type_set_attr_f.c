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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/f77/bindings.h"
#include "ompi/attribute/attribute.h"
#include "ompi/datatype/ompi_datatype.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_SET_ATTR = mpi_type_set_attr_f
#pragma weak pmpi_type_set_attr = mpi_type_set_attr_f
#pragma weak pmpi_type_set_attr_ = mpi_type_set_attr_f
#pragma weak pmpi_type_set_attr__ = mpi_type_set_attr_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_SET_ATTR,
                           pmpi_type_set_attr,
                           pmpi_type_set_attr_,
                           pmpi_type_set_attr__,
                           pmpi_type_set_attr_f,
                           (MPI_Fint *type, MPI_Fint *type_keyval, MPI_Aint *attr_val, MPI_Fint *ierr),
                           (type, type_keyval, attr_val, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_SET_ATTR = mpi_type_set_attr_f
#pragma weak mpi_type_set_attr = mpi_type_set_attr_f
#pragma weak mpi_type_set_attr_ = mpi_type_set_attr_f
#pragma weak mpi_type_set_attr__ = mpi_type_set_attr_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_SET_ATTR,
                           mpi_type_set_attr,
                           mpi_type_set_attr_,
                           mpi_type_set_attr__,
                           mpi_type_set_attr_f,
                           (MPI_Fint *type, MPI_Fint *type_keyval, MPI_Aint *attr_val, MPI_Fint *ierr),
                           (type, type_keyval, attr_val, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_type_set_attr_f(MPI_Fint *type, MPI_Fint *type_keyval, MPI_Aint *attribute_val, MPI_Fint *ierr)
{
    int c_err;
    MPI_Datatype c_type = MPI_Type_f2c(*type);

    /* This stuff is very confusing.  Be sure to see the comment at
       the top of src/attributes/attributes.c. */

    c_err = ompi_attr_set_fortran_mpi2(TYPE_ATTR,
                                       c_type,
                                       &c_type->d_keyhash,
                                       OMPI_FINT_2_INT(*type_keyval), 
                                       *attribute_val,
                                       false);
    *ierr = OMPI_INT_2_FINT(c_err);
}
