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

#ifndef _VT_FBINDINGS_H
#define _VT_FBINDINGS_H

#ifdef __cplusplus
#   define EXTERN extern "C"
#else
#   define EXTERN extern
#endif

/* define all possible Fortran bindings for a function */
#define VT_GENERATE_F77_BINDINGS(lower_case, \
                                 upper_case, \
                                 wrapper_function, \
                                 signature, \
                                 params) \
  void lower_case signature; \
  void lower_case signature { wrapper_function params; } \
  void lower_case##_ signature; \
  void lower_case##_ signature { wrapper_function params; } \
  void lower_case##__ signature; \
  void lower_case##__ signature { wrapper_function params; } \
  void upper_case signature; \
  void upper_case signature { wrapper_function params; }

/* create a C string from an F77 string (allocates *cstr if NULL) */
EXTERN void vt_string_f2c(const char* fstr, int len, char** cstr);

/* create a F77 string from a C string */
EXTERN void vt_string_c2f(const char* cstr, char* fstr, int len);

#endif /* _VT_FBINDINGS_H */
