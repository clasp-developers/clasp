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

#ifndef VT_CUPTI_CALLBACK_H
#define	VT_CUPTI_CALLBACK_H

#ifdef __cplusplus
# define EXTERN extern "C"
#else
# define EXTERN extern
#endif

EXTERN void vt_cupti_callback_init(void);
EXTERN void vt_cupti_callback_finalize(void);

#endif	/* VT_CUPTI_CALLBACK_H */

