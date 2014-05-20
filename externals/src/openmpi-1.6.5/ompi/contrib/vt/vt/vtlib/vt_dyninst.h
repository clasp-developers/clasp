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

#ifndef _VT_DYNINST_H
#define _VT_DYNINST_H

#ifdef __cplusplus
# define EXTERN extern "C"
#else
# define EXTERN extern
#endif

#include "vt_inttypes.h"

/* attach Dyninst mutator (vtdyn) to the current process
   (called during initialization of libvt-dynatt) */
EXTERN void vt_dyn_attach(void);

/* function to be called at the entry of each function or loop */
EXTERN void vt_dyn_start(uint32_t index, const char* name, const char* fname,
                         uint32_t lno, uint32_t loop);

/* function to be called at the exit of each function or loop */
EXTERN void vt_dyn_end(uint32_t index);

/* flag: attaching the Dyninst mutator to the current process? */
EXTERN uint8_t vt_dyn_attaching;

#endif /* _VT_DYNINST_H */

