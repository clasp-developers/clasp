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

#ifndef _VT_GETCPU_H
#define _VT_GETCPU_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

#include "vt_inttypes.h"

EXTERN void vt_getcpu_init(void);
EXTERN void vt_getcpu_finalize(void);

EXTERN void vt_getcpu_read(uint32_t* value, uint8_t* changed);

/* counter id */
EXTERN uint32_t vt_getcpu_cid;

#endif /* _VT_GETCPU_H */
