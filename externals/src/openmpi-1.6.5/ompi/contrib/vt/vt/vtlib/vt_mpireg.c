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
#include "vt_mpireg.h"
#include "vt_trc.h"

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

int vt_mpi_regid[VT__MPI_REGID_NUM];

static uint32_t mpi_fid = (uint32_t)-1;

void vt_mpi_register()
{
  mpi_fid = vt_def_scl_file(VT_CURRENT_THREAD, "MPI");

  vt_mpi_regid[VT__MPI_INIT] =
    vt_def_region(VT_CURRENT_THREAD, "MPI_Init", mpi_fid, VT_NO_LNO, VT_NO_LNO,
                  NULL, VT_MPI_FUNCTION);

  vt_mpi_regid[VT__MPI_INITIALIZED] =
      vt_def_region(VT_CURRENT_THREAD, "MPI_Initialized", mpi_fid, VT_NO_LNO,
                    VT_NO_LNO, NULL, VT_MPI_FUNCTION);

#if defined(HAVE_MPI2_THREAD) && HAVE_MPI2_THREAD

  vt_mpi_regid[VT__MPI_INIT_THREAD] =
    vt_def_region(VT_CURRENT_THREAD, "MPI_Init_thread", mpi_fid, VT_NO_LNO,
                  VT_NO_LNO, NULL, VT_MPI_FUNCTION);

#endif /* HAVE_MPI2_THREAD */
}

void vt_mpi_register_remain()
{
  vt_libassert( mpi_fid != (uint32_t)-1 );

  /* include generated function registry */
# include "vt_mpireg.gen.c"
}
