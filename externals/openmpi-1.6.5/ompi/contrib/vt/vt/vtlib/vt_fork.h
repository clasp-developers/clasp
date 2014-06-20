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

#ifndef _VT_FORK_H
#define _VT_FORK_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

#include "config.h"

#if (!defined (VT_MPI) && !defined (VT_MT) && !defined(VT_HYB) && !defined(VT_JAVA))

#define VT_FORK

#include "vt_inttypes.h"
#include <unistd.h>
#include <sys/types.h>

EXTERN void     vt_fork_init(void);
EXTERN void     vt_fork_finalize(void);

EXTERN void     vt_fork(pid_t pid);
EXTERN void     vt_fork_waitchilds(void);
EXTERN uint32_t vt_fork_get_num_childs(void);
EXTERN uint32_t vt_fork_get_num_childs_tot(void);
EXTERN char*    vt_fork_get_trcid_filename(void);

#endif /* !VT_MPI && !VT_MT && !VT_HYB */

#endif /* _VT_FORK_H */
