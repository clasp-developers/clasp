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

#ifndef _VT_MPIREG_H
#define _VT_MPIREG_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

/* include generated function registry */
#include "vt_mpireg.gen.h"

extern int vt_mpi_regid[VT__MPI_REGID_NUM];

EXTERN void vt_mpi_register(void);

EXTERN void vt_mpi_register_remain(void);

#endif /* _VT_MPIREG_H */

