/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2004-2005, The Trustees of Indiana University and Indiana
 *                          University Research and Technology
 *
 * Copyright (c) 2004-2006, The University of Tennessee and The University
 *                          of Tennessee Research Foundation
 *
 * Copyright (c) 2004-2005, High Performance Computing Center Stuttgart,
 *                          University of Stuttgart
 *
 * Copyright (c) 2004-2005, The Regents of the University of California
 *
 * Copyright (c) 2007,      Cisco Systems, Inc.
 *
 * Copyright (c) 2005-2013, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _UTIL_H
#define _UTIL_H

#ifdef __cplusplus
# define EXTERN extern "C"
#else
# define EXTERN extern
#endif

#include <stdarg.h>
#include <stdlib.h>

EXTERN int vt_asprintf(char** ptr, const char* fmt, ...);
EXTERN int vt_snprintf(char* str, size_t size, const char* fmt, ...);
EXTERN int vt_vasprintf(char** ptr, const char* fmt, va_list ap);
EXTERN int vt_vsnprintf(char* str, size_t size, const char* fmt, va_list ap);
EXTERN char* vt_strdup(const char* s);
EXTERN char* vt_strtrim(char* s);
EXTERN void* vt_memmove(void* dest, const void* src, size_t n);

/* own implementation of assert() which isn't affected by NDEBUG */
#define vt_assert(expr) if( !(expr) ) vt_assert_fail(#expr, __FILE__, __LINE__)
EXTERN void vt_assert_fail(const char* expr, const char* file, int line);

#endif /* _UTIL_H */
