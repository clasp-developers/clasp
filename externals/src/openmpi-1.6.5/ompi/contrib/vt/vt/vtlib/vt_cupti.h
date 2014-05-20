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

#ifndef VT_CUPTI_H
#define	VT_CUPTI_H

#ifdef __cplusplus
# define EXTERN extern "C"
#else
# define EXTERN extern
#endif

/* Disable all compiler warnings before including the actual
   CUPTI header file. */
#ifdef __GNUC__
# pragma GCC system_header
#endif /* __GNUC__ */
#include "cupti.h"

#endif	/* VT_CUPTI_H */

