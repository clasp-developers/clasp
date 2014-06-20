/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/** 
 *  @file OTF_Platform_unix.h
 *
 *  @brief Deals with platform dependend issues.
 *
 *  \ingroup internal
 */


#ifndef OTF_PLATFORM_UNIX_H
#define OTF_PLATFORM_UNIX_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#include <stdarg.h>
#include <stdlib.h>


#include <limits.h>
#if defined(HAVE_SYS_PARAM_H) && HAVE_SYS_PARAM_H
#	include <sys/param.h>
#endif
#if defined(PATH_MAX)
#	define OTF_PATH_MAX (PATH_MAX + 1)
#elif defined(_POSIX_PATH_MAX)
#	define OTF_PATH_MAX (_POSIX_PATH_MAX + 1)
#else
#	define OTF_PATH_MAX 256
#endif

#if !(defined(HAVE_FSEEKO) && HAVE_FSEEKO)
#	undef fseeko
#	undef ftello
#	define fseeko fseek
#	define ftello ftell
#endif /* HAVE_FSEEKO */

#if !(defined(HAVE_ASPRINTF) && HAVE_ASPRINTF)
#	undef asprintf
#	define asprintf OTF_asprintf
#endif /* HAVE_ASPRINTF */

#if !(defined(HAVE_SNPRINTF) && HAVE_SNPRINTF)
#	undef snprintf
#	define snprintf OTF_snprintf
#endif /* HAVE_SNPRINTF */

#if !(defined(HAVE_VASPRINTF) && HAVE_VASPRINTF)
#	undef vasprintf
#	define vasprintf OTF_vasprintf
#endif /* HAVE_VASPRINTF */

#if !(defined(HAVE_VSNPRINTF) && HAVE_VSNPRINTF)
#	undef vsnprintf
#	define vsnprintf OTF_vsnprintf
#endif /* HAVE_VSNPRINTF */

#if !(defined(HAVE_STRDUP) && HAVE_STRDUP)
#	undef strdup
#	define strdup OTF_strdup
#endif /* HAVE_STRDUP */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

int OTF_asprintf( char** ptr, const char* fmt, ... );
int OTF_snprintf( char* str, size_t size, const char* fmt, ... );
int OTF_vasprintf( char** ptr, const char* fmt, va_list ap );
int OTF_vsnprintf( char* str, size_t size, const char* fmt, va_list ap );
char* OTF_strdup( const char* s );

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTF_PLATFORM_UNIX_H */
