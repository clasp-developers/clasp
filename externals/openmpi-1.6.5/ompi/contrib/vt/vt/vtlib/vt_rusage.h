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

#ifndef _VT_RUSAGE_H
#define _VT_RUSAGE_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

#include "vt_inttypes.h"

struct vt_rusage;

EXTERN uint32_t          vt_rusage_open(void);
EXTERN void              vt_rusage_close(void);

EXTERN struct vt_rusage* vt_rusage_create(void);
EXTERN void              vt_rusage_free(struct vt_rusage* rusage);

EXTERN void              vt_rusage_init(void);

EXTERN void              vt_rusage_read(struct vt_rusage* rusage,
					uint64_t* values, uint32_t* changed);

EXTERN uint32_t          vt_rusage_num(void);

/* vector of counter ids */
EXTERN uint32_t* vt_rusage_cidv;

/* read interval */
EXTERN uint64_t  vt_rusage_intv;

#endif /* _VT_RUSAGE_H */
