/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2012, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _VT_UNIFY_MPI_H_
#define _VT_UNIFY_MPI_H_

#include "config.h"

#include "vt_defs.h" /* to get VT_MPI_INT */

/* use MPI's profile interface for the library version of the unifier */
#ifdef VT_LIB
#  define CALL_MPI( call ) P##call
#else /* VT_LIB */
#  define CALL_MPI( call ) call
#endif /* VT_LIB */

#include "mpi.h"

#if defined(HAVE_MPI_GET_ADDRESS) && HAVE_MPI_GET_ADDRESS
#  define MPI_Address MPI_Get_address
#  define PMPI_Address PMPI_Get_address
#endif /* HAVE_MPI_GET_ADDRESS */

#if defined(HAVE_MPI_TYPE_CREATE_STRUCT) && HAVE_MPI_TYPE_CREATE_STRUCT
#  define MPI_Type_struct MPI_Type_create_struct
#  define PMPI_Type_struct PMPI_Type_create_struct
#endif /* HAVE_MPI_TYPE_CREATE_STRUCT */

#endif /* _VT_UNIFY_MPI_H_ */
