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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
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
#pragma weak PMPI_TYPE_GET_ATTR = mpi_type_get_attr_f
#pragma weak pmpi_type_get_attr = mpi_type_get_attr_f
#pragma weak pmpi_type_get_attr_ = mpi_type_get_attr_f
#pragma weak pmpi_type_get_attr__ = mpi_type_get_attr_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_GET_ATTR,
                           pmpi_type_get_attr,
                           pmpi_type_get_attr_,
                           pmpi_type_get_attr__,
                           pmpi_type_get_attr_f,
                           (MPI_Fint *type, MPI_Fint *type_keyval, MPI_Aint *attribute_val, ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                           (type, type_keyval, attribute_val, flag, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_GET_ATTR = mpi_type_get_attr_f
#pragma weak mpi_type_get_attr = mpi_type_get_attr_f
#pragma weak mpi_type_get_attr_ = mpi_type_get_attr_f
#pragma weak mpi_type_get_attr__ = mpi_type_get_attr_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_GET_ATTR,
                           mpi_type_get_attr,
                           mpi_type_get_attr_,
                           mpi_type_get_attr__,
                           mpi_type_get_attr_f,
                           (MPI_Fint *type, MPI_Fint *type_keyval, MPI_Aint *attribute_val, ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                           (type, type_keyval, attribute_val, flag, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_type_get_attr_f(MPI_Fint *type, MPI_Fint *type_keyval,
                         MPI_Aint *attribute_val, ompi_fortran_logical_t *flag,
                         MPI_Fint *ierr)
{
    int c_err;
    MPI_Datatype c_type = MPI_Type_f2c(*type);
    OMPI_LOGICAL_NAME_DECL(flag);

    /* This stuff is very confusing.  Be sure to see the comment at
       the top of src/attributes/attributes.c. */

    c_err = ompi_attr_get_fortran_mpi2(c_type->d_keyhash,
                                       OMPI_FINT_2_INT(*type_keyval),
                                       attribute_val,
                                       OMPI_LOGICAL_SINGLE_NAME_CONVERT(flag));
    *ierr = OMPI_INT_2_FINT(c_err);
    OMPI_SINGLE_INT_2_LOGICAL(flag);
}
