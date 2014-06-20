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

#include "config.h"

#include "vt_defs.h"
#include "vt_error.h"
#include "vt_fbindings.h"
#include "vt_fmpiconst.h"

#include <stdlib.h>

/* External Fortran subroutines to get MPI Fortran constants. */
extern void vt_get_mpi_f_bottom___(void);
#if (defined(HAVE_DECL_MPI_IN_PLACE) && HAVE_DECL_MPI_IN_PLACE)
  extern void vt_get_mpi_f_in_place___(void);
#endif /* HAVE_DECL_MPI_IN_PLACE */
extern void vt_get_mpi_f_statuses_ignore___(void);
#if !(defined(HAVE_DECL_MPI_STATUS_SIZE) && HAVE_DECL_MPI_STATUS_SIZE)
  extern void vt_get_mpi_status_size___(int* size);
#endif /* HAVE_DECL_MPI_STATUS_SIZE */

/* Variables that hold the MPI Fortran constants. */
MPI_Fint*   vt_mpi_f_bottom_addr          = NULL;
#if (defined(HAVE_DECL_MPI_IN_PLACE) && HAVE_DECL_MPI_IN_PLACE)
  MPI_Fint* vt_mpi_f_in_place_addr        = NULL;
#endif /* HAVE_DECL_MPI_IN_PLACE */
  MPI_Fint* vt_mpi_f_status_ignore_addr   = NULL;
  MPI_Fint* vt_mpi_f_statuses_ignore_addr = NULL;
#if !(defined(HAVE_DECL_MPI_STATUS_SIZE) && HAVE_DECL_MPI_STATUS_SIZE)
  int       vt_mpi_status_size            = 0;
#endif /* HAVE_DECL_MPI_STATUS_SIZE */

/* Callback functions for receiving the addresses of MPI Fortran constants.
   This function will be called from the Fortran subroutines
  "vt_get_mpi_f_*. */

/* -- MPI_BOTTOM -- */

VT_DECLDEF(void vt_get_mpi_f_bottom_cb_f(void* addr)) {
  vt_mpi_f_bottom_addr = (MPI_Fint*)addr;
} VT_GENERATE_F77_BINDINGS(vt_get_mpi_f_bottom_cb, VT_GET_MPI_F_BOTTOM_CB,
                           vt_get_mpi_f_bottom_cb_f,
                           (void* addr), (addr))

#if (defined(HAVE_DECL_MPI_IN_PLACE) && HAVE_DECL_MPI_IN_PLACE)

/* -- MPI_IN_PLACE -- */

VT_DECLDEF(void vt_get_mpi_f_in_place_cb_f(void* addr)) {
  vt_mpi_f_in_place_addr = (MPI_Fint*)addr;
} VT_GENERATE_F77_BINDINGS(vt_get_mpi_f_in_place_cb, VT_GET_MPI_F_IN_PLACE_CB,
                           vt_get_mpi_f_in_place_cb_f,
                           (void* addr), (addr))

#endif /* HAVE_DECL_MPI_IN_PLACE */

/* -- MPI_F_STATUS[ES]_IGNORE -- */

VT_DECLDEF(void vt_get_mpi_f_statuses_ignore_cb_f(void* addr1, void* addr2)) {
  vt_mpi_f_status_ignore_addr = (MPI_Fint*)addr1;
  vt_mpi_f_statuses_ignore_addr = (MPI_Fint*)addr2;
} VT_GENERATE_F77_BINDINGS(vt_get_mpi_f_statuses_ignore_cb,
                           VT_GET_MPI_F_STATUSES_IGNORE_CB,
                           vt_get_mpi_f_statuses_ignore_cb_f,
                           (void* addr1, void* addr2), (addr1, addr2))

void vt_fmpiconst_init()
{
  vt_get_mpi_f_bottom___();
  vt_libassert(vt_mpi_f_bottom_addr != NULL);
  vt_cntl_msg(2, "Detected address of MPI_BOTTOM=%x",
              vt_mpi_f_bottom_addr);
#if (defined(HAVE_DECL_MPI_IN_PLACE) && HAVE_DECL_MPI_IN_PLACE)
  vt_get_mpi_f_in_place___();
  vt_libassert(vt_mpi_f_in_place_addr != NULL);
  vt_cntl_msg(2, "Detected address of MPI_IN_PLACE=%x",
              vt_mpi_f_in_place_addr);
#endif /* HAVE_DECL_MPI_IN_PLACE */
  vt_get_mpi_f_statuses_ignore___();
  vt_libassert(vt_mpi_f_status_ignore_addr != NULL);
  vt_cntl_msg(2, "Detected address of MPI_F_STATUS_IGNORE=%x",
              vt_mpi_f_status_ignore_addr);
  vt_libassert(vt_mpi_f_statuses_ignore_addr != NULL);
  vt_cntl_msg(2, "Detected address of MPI_F_STATUSES_IGNORE=%x",
              vt_mpi_f_statuses_ignore_addr);
#if !(defined(HAVE_DECL_MPI_STATUS_SIZE) && HAVE_DECL_MPI_STATUS_SIZE)
  vt_get_mpi_status_size___(&vt_mpi_status_size);
  vt_libassert(vt_mpi_status_size > 0);
  vt_cntl_msg(2, "Detected value of MPI_STATUS_SIZE=%d",
              vt_mpi_status_size);
#endif /* HAVE_DECL_MPI_STATUS_SIZE */
}
