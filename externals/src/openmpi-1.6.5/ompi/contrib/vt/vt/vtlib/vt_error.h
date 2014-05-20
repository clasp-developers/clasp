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

#ifndef _VT_ERROR_H
#define _VT_ERROR_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

#include <stdarg.h>

/* set process id/rank for messages */
EXTERN void vt_error_pid(const int pid);

/* abort if assertion is false */
#define vt_libassert(expr) if(!(expr)) vt_libassert_fail(__FILE__, __LINE__, #expr)
EXTERN void vt_libassert_fail(const char* f, int l, const char* expr);

/* abort and system error message */
#define vt_error() vt_error_impl(__FILE__, __LINE__)
EXTERN void vt_error_impl(const char* f, int l);                          

/* abort and user error message */
EXTERN void vt_error_msg(const char* fmt, ...);

/* user warning message without abort */
EXTERN void vt_warning(const char* fmt, ...);

/* user control message without abort (printed only if VT_VERBOSE is set) */
EXTERN void vt_cntl_msg(int level, const char* fmt, ...);

#endif






