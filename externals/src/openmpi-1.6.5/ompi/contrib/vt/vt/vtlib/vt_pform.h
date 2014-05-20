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

#ifndef _VT_PFORM_H
#define _VT_PFORM_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

#include "vt_inttypes.h"

/* platform specific initialization */
EXTERN void   vt_pform_init(void);

/* directory of global file system  */
EXTERN char*  vt_pform_gdir(void);

/* directory of local file system  */
EXTERN char*  vt_pform_ldir(void);

/* full path of executable  */
EXTERN char*  vt_pform_exec(void);

/* clock resolution */
EXTERN uint64_t vt_pform_clockres(void);

/* local or global wall-clock time */
EXTERN uint64_t vt_pform_wtime(void);

/* unique numeric SMP-node identifier */
EXTERN long   vt_pform_node_id(void);

/* unique string SMP-node identifier */
EXTERN char*  vt_pform_node_name(void);

/* number of CPUs */
EXTERN int    vt_pform_num_cpus(void);

#endif





