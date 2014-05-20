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

#include <stdlib.h>
#include <string.h>

#include "vt_defs.h"
#include "vt_env.h"
#include "vt_fmpiconst.h"
#include "vt_fbindings.h"
#include "vt_unimci.h"

#include "mpi.h"

static void fmpiwrap_init(void)
{
  /* get MPI Fortran constants */
  vt_fmpiconst_init();
}

/* -- MPI_Init -- */

VT_DECLDEF(void vt_mpi_init_f(MPI_Fint* ierr))
{
  fmpiwrap_init();

  VT_UNIMCI_SET_BINDING_LANGUAGE_FORTRAN();
  *ierr = MPI_Init(0, (char***)0);
  VT_UNIMCI_SET_BINDING_LANGUAGE_C();

} VT_GENERATE_F77_BINDINGS(mpi_init, MPI_INIT, vt_mpi_init_f,
  (MPI_Fint* ierr),
  (ierr))

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD

/* -- MPI_Init_thread -- */

VT_DECLDEF(void vt_mpi_init_thread_f(MPI_Fint* required, MPI_Fint* provided,
                                     MPI_Fint* ierr))
{
  fmpiwrap_init();

  VT_UNIMCI_SET_BINDING_LANGUAGE_FORTRAN();
  *ierr = MPI_Init_thread(0, (char***)0, *required, provided);
  VT_UNIMCI_SET_BINDING_LANGUAGE_C();

} VT_GENERATE_F77_BINDINGS(mpi_init_thread, MPI_INIT_THREAD, vt_mpi_init_thread_f,
  (MPI_Fint* required, MPI_Fint* provided, MPI_Fint* ierr),
  (required, provided, ierr))

#endif /* HAVE_MPI2_THREAD */

/* include generated wrapper functions */
#include "vt_fmpiwrap.gen.c"

