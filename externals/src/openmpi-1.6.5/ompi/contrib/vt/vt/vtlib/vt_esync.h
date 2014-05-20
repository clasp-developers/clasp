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

#ifndef _VT_ESYNC_H
#define _VT_ESYNC_H

#ifdef __cplusplus
# define EXTERN extern "C" 
#else
# define EXTERN extern 
#endif

#include <stdio.h>

#include "vt_inttypes.h"

#include "mpi.h"

EXTERN void vt_esync_init(void);

EXTERN void vt_esync_finalize(void);

EXTERN void vt_esync(MPI_Comm comm);

EXTERN void vt_esync_app_uctl_data(char** data);

EXTERN uint64_t vt_esync_next(void);

#endif /* _VT_ESYNC_H */









