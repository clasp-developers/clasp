/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2013, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _VT_FMPICONST_H
#define _VT_FMPICONST_H

#ifdef __cplusplus
#   define EXTERN extern "C"
#else
#   define EXTERN extern
#endif

#include "config.h"

#include "mpi.h"

#define VT_MPI_F_BOTTOM          vt_mpi_f_bottom_addr
#if (defined(HAVE_DECL_MPI_IN_PLACE) && HAVE_DECL_MPI_IN_PLACE)
# define VT_MPI_F_IN_PLACE       vt_mpi_f_in_place_addr
#endif /* HAVE_DECL_MPI_IN_PLACE */
#define VT_MPI_F_STATUS_IGNORE   vt_mpi_f_status_ignore_addr
#define VT_MPI_F_STATUSES_IGNORE vt_mpi_f_statuses_ignore_addr
#if (defined(HAVE_DECL_MPI_STATUS_SIZE) && HAVE_DECL_MPI_STATUS_SIZE)
# define VT_MPI_STATUS_SIZE      MPI_STATUS_SIZE
#else /* HAVE_DECL_MPI_STATUS_SIZE */
# define VT_MPI_STATUS_SIZE      vt_mpi_status_size
#endif /* HAVE_DECL_MPI_STATUS_SIZE */

#define VT_MPI_BOTTOM_F2C(addr) \
  ((addr == VT_MPI_F_BOTTOM) ? MPI_BOTTOM : (addr))
#if (defined(HAVE_DECL_MPI_IN_PLACE) && HAVE_DECL_MPI_IN_PLACE)
# define VT_MPI_IN_PLACE_F2C(addr) \
    ((addr == VT_MPI_F_IN_PLACE) ? MPI_IN_PLACE : (addr))
#endif /* HAVE_DECL_MPI_IN_PLACE */

EXTERN MPI_Fint* vt_mpi_f_bottom_addr;
#if (defined(HAVE_DECL_MPI_IN_PLACE) && HAVE_DECL_MPI_IN_PLACE)
  EXTERN MPI_Fint* vt_mpi_f_in_place_addr;
#endif /* HAVE_DECL_MPI_IN_PLACE */
EXTERN MPI_Fint* vt_mpi_f_status_ignore_addr;
EXTERN MPI_Fint* vt_mpi_f_statuses_ignore_addr;
#if !(defined(HAVE_DECL_MPI_STATUS_SIZE) && HAVE_DECL_MPI_STATUS_SIZE)
  EXTERN MPI_Fint vt_mpi_status_size;
#endif /* HAVE_DECL_MPI_STATUS_SIZE */

EXTERN void vt_fmpiconst_init(void);

#endif /* _VT_FMPICONST_H */
