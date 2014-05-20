/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/** 
 *  @file OTF_Platform_win.h
 *
 *  @brief Deals with platform dependend issues.
 *
 *  \ingroup internal
 */


#ifndef OTF_PLATFORM_WIN_H
#define OTF_PLATFORM_WIN_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#if defined(_MSC_VER) /* visual studio */

#	include <limits.h>
#	include <winsock2.h>
#	include <process.h>

#	define HAVE_IO_H
#	define HAVE_ZLIB

#	ifndef PATH_MAX
#		define PATH_MAX 255
#	endif

#	define OTF_PATH_MAX PATH_MAX

#	undef ftello
#	define ftello (uint64_t) _ftelli64
#	undef fseeko
#	define fseeko(f,off,orig) _fseeki64(f,(__int64)off,orig)

#	undef snprintf
#	define snprintf _snprintf

#	undef getpid
#	define getpid() _getpid()

#	pragma warning (disable : 4996) /* disable insecurity/deprication warnings */

	int gettimeofday(struct timeval* tv, void* dummytimezone);

	long int nrand48 (unsigned short int xsubi[3]);

#	include <fcntl.h>
#	ifndef _S_IREAD
#		define _S_IREAD 256
#	endif
#	ifndef _S_IWRITE
#		define _S_IWRITE 128
#	endif

	int mkstemp(char *tmpl);

#else

#	error "You are using an unsupported compiler on windows."

#endif /* _MSC_VER */

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTF_PLATFORM_WIN_H */
