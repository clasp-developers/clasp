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

#ifndef _VT_FILTER_CONFIG_H_
#define _VT_FILTER_CONFIG_H_

#include "config.h"

#if defined(HAVE_OMP) && HAVE_OMP
  // disable OpenMP under the following circumstances:

  // on NEC SX platforms (causes "internal compiler error")
# if defined(_SX)
#   undef HAVE_OMP

  // on MacOS X using GCC < v4.5
  // causes "undefined reference to ___builtin_expect()"
  // (induced by assert()'s within OpenMP-parallel regions)
# elif (defined(__APPLE__) && defined(__MACH__) && defined(__GNUC__) && \
       (__GNUC__ < 4 || (__GNUC__ == 4 &&  __GNUC_MINOR__ < 5)))
#   undef HAVE_OMP

  // using Open64 < v4.2.4 (causes "internal compiler error")
# elif defined(__OPEN64__)
#   if !defined(__OPENCC__) || !defined(__OPENCC_MINOR__) || !defined(__OPENCC_PATCHLEVEL__)
      // unknown compiler version; disable OpenMP to be on the safe side
#     undef HAVE_OMP
#   else
      // __OPENCC_PATCHLEVEL__ can be empty; redefine it to 0
#     if !(__OPENCC_PATCHLEVEL__ + 0)
#       undef __OPENCC_PATCHLEVEL__
#       define __OPENCC_PATCHLEVEL__ 0
#     endif
      // disable OpenMP, if compiler version is less than 4.2.4
#     if __OPENCC__ < 4 || (__OPENCC__ == 4 && (__OPENCC_MINOR__ < 2 || (__OPENCC_MINOR__ == 2 && __OPENCC_PATCHLEVEL__ < 4)))
#       undef HAVE_OMP
#     endif
#   endif
# endif // __OPEN64__
#endif // HAVE_OMP

#endif // _VT_FILTER_CONFIG_H_
