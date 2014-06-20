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

#ifndef _VT_MPIWRAP_H
#define _VT_MPIWRAP_H

#ifdef __cplusplus
# define EXTERN extern "C"
#else
# define EXTERN extern
#endif

/* initialize/finalize the MPI wrappers */
EXTERN void vt_mpiwrap_init(void);
EXTERN void vt_mpiwrap_finalize(void);

#endif /* _VT_MPIWRAP_H */

