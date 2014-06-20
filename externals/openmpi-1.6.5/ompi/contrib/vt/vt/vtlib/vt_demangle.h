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

#ifndef _VT_DEMANGLE_H
#define _VT_DEMANGLE_H

#ifdef __cplusplus
# define EXTERN extern "C"
#else
# define EXTERN extern
#endif

#include "config.h"

#if defined(HAVE_DEMANGLE_H)
# include <demangle.h>
#else /* HAVE_DEMANGLE_H */
  EXTERN char* cplus_demangle(const char* mangled, int options);
#endif /* HAVE_DEMANGLE_H */

#endif /* _VT_DEMANGLE_H */
