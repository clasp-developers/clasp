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

#ifndef _VT_USER_H
#define _VT_USER_H

#ifdef __VT_EXTERN_DECL
# error The macro __VT_EXTERN_DECL is used by VampirTrace internally and must not be defined by user code!
#endif /* __VT_EXTERN_DECL */

#ifdef __cplusplus
# define __VT_EXTERN_DECL extern "C" 
#else /* __cplusplus */
# define __VT_EXTERN_DECL extern 
#endif /* __cplusplus */

#ifdef __VT_NOINST_ATTR
# error The macro __VT_NOINST_ATTR is used by VampirTrace internally and must not be defined by user code!
#endif /* __VT_NOINST_ATTR */

#ifdef __GNUC__
# define __VT_NOINST_ATTR __attribute__ ((no_instrument_function))
#else /* __GNUC__ */
# define __VT_NOINST_ATTR
#endif /* __GNUC__ */

#include "vt_user_control.h"
#include "vt_user_comment.h"
#include "vt_user_count.h"
#include "vt_user_marker.h"
#include "vt_user_message.h"
#include "vt_user_region.h"

#ifdef VTRACE_PTHREAD
# include "vt_wrap_pthread.h"
#endif /* VTRACE_PTHREAD */

#include <stdlib.h>

#endif /* _VT_USER_H */
