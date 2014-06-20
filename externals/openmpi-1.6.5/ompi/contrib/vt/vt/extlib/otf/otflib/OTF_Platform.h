/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/** 
 *  @file OTF_Platform.h
 *
 *  @brief Deals with platform dependend issues.
 *
 *  \ingroup internal
 */


/* THIS HEADER SHOULD NEVER BE INCLUDED INTO OTHER HEADERS OF THE OTF LIBRARY
   AND HAS TO BE INCLUDED INTO ALL .C-FILES OF THE OTF LIBARY */


#ifndef OTF_PLATFORM_H
#define OTF_PLATFORM_H

#if defined(_WIN32) /* windows */
#	include "OTF_Platform_win.h"
#else /* unix */
#	include "OTF_Platform_unix.h"
#endif /* windows/unix */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* if you know (for sure) of more compilers supporting __FUNCTION__,
   then add them here */
#if defined(__GNUC__) /* gnu */
#elif defined(_MSC_VER) /* ms vs */
#elif defined(__xlC__) /* ibm xlc */
#else

	/* set __FUNCTION__ to a dummy for compilers not supporting this macro */
#	define __FUNCTION__ "<unknown function>"

#endif

#ifndef __FILE__
#	define __FILE__ "<unknown file>"
#endif

#ifndef __LINE__
#	define __LINE__ 0
#endif

char* OTF_basename( char* path );

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTF_PLATFORM_H */
