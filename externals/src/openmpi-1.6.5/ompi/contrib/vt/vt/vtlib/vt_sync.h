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

#ifndef _VT_MPISYNC_H
#define _VT_MPISYNC_H

#ifdef __cplusplus
# define EXTERN extern "C" 
#else
# define EXTERN extern 
#endif

#include "vt_inttypes.h"

#include "mpi.h"

EXTERN void vt_sync(MPI_Comm comm, uint64_t* ltime, int64_t* offset);

#endif









