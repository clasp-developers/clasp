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

#ifndef _VT_IOFSL_H
#define _VT_IOFSL_H

#ifdef __cplusplus
#   define EXTERN extern "C"
#else
#   define EXTERN extern
#endif

#include "vt_inttypes.h"

/* IOFSL mode initialization */
EXTERN void vt_iofsl_init(void);

/* IOFSL mode finalization */
EXTERN void vt_iofsl_finalize(void);

/* indicator for enabled/disabled IOFSL mode */
EXTERN uint8_t vt_iofsl_enabled;

/* IOFSL mode
   (either VT_IOFSL_MODE_MULTIFILE or VT_IOFSL_MODE_MULTIFILE_SPLIT) */
EXTERN uint32_t vt_iofsl_mode;

/* IOFSL flags bitmask */
EXTERN uint32_t vt_iofsl_flags;

/* number of IOFSL servers */
EXTERN uint32_t vt_iofsl_servers_num;

/* IOFSL server addresses */
EXTERN char** vt_iofsl_servers_list;

#endif /* _VT_IOFSL_H */
