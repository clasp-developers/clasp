/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
 
 Copyright (c) 2004-2005, The Trustees of Indiana University and Indiana
                          University Research and Technology
 Copyright (c) 2004-2006, The University of Tennessee and The University
                          of Tennessee Research Foundation
 Copyright (c) 2004-2005, High Performance Computing Center Stuttgart,
                          University of Stuttgart
 Copyright (c) 2004-2005, The Regents of the University of California
 Copyright (c) 2007,      Cisco Systems, Inc.
*/

#include "OTF_Platform.h"

#include <stdlib.h>
#include <string.h>

#if defined(_WIN32) /* windows */

#include <Windows.h>
#include <io.h>

#include "OTF_inttypes_win.h"


int gettimeofday(struct timeval* tv, void* dummytimezone) {
	union {
		long long ns100;
		FILETIME ft;
	} now;
 
	GetSystemTimeAsFileTime (&now.ft);
	tv->tv_usec = (long) ((now.ns100 / 10LL) % 1000000LL);
	tv->tv_sec = (long) ((now.ns100 - 116444736000000000LL) / 10000000LL);

	return 0;
}

/* Taken from: http://www.mail-archive.com/pan-devel@nongnu.org/msg00294.html */
int mkstemp(char *tmpl)
{
	int ret = -1;

	mktemp(tmpl);
	ret = open(tmpl, O_RDWR|O_BINARY|O_CREAT|O_EXCL|_O_SHORT_LIVED, _S_IREAD|_S_IWRITE);

	return ret;
}


/* Taken from: http://gnuwin32.sourceforge.net/packages/libgw32c.htm */
long int nrand48 (unsigned short int xsubi[3])
{
    uint64_t x = (uint64_t) xsubi[2] << 32 | (uint32_t) xsubi[1] << 16 | xsubi[0];
    x = x * 0x5deece66dull + 0xb;

    xsubi[0] = x & 0xffff;
    xsubi[1] = (x >> 16) & 0xffff;
    xsubi[2] = (x >> 32) & 0xffff;

    if (sizeof (unsigned short int) == 2) {
        return xsubi[2] << 15 | xsubi[1] >> 1;
    } else {
        return xsubi[2] >> 1;
    }
}

#else /* unix */

#include <errno.h>
#include <stdio.h>

static int guess_strlen(const char* fmt, va_list ap) {
	char* sarg;
	double darg;
	float farg;
	size_t i;
	int iarg;
	int len;
	long larg;

	/* Start off with a fudge factor of 128 to handle the % escapes that
	 *      we aren't calculating here */

	len = (int)strlen(fmt) + 128;
	for (i = 0; i < strlen(fmt); ++i) {
		if ('%' == fmt[i] && i + 1 < strlen(fmt) && '%' != fmt[i + 1]) {
			++i;
			switch (fmt[i]) {
			case 'c':
				(void)va_arg(ap, int);
				len += 1;  /* let's suppose it's a printable char */
				break;
			case 's':
				sarg = va_arg(ap, char*);

				/* If there's an arg, get the strlen, otherwise we'll
				 * use (null) */

				if (NULL != sarg) {
					len += (int)strlen(sarg);
				} else {
					len += 5;
				}
				break;
			case 'd':
			case 'i':
				iarg = va_arg(ap, int);
				/* Alloc for minus sign */
				if (iarg < 0) ++len;
				/* Now get the log10 */
				do {
					++len;
					iarg /= 10;
				} while (0 != iarg);
				break;
			case 'x':
			case 'X':
				iarg = va_arg(ap, int);
				/* Now get the log16 */
				do {
					++len;
					iarg /= 16;
				} while (0 != iarg);
				break;
			case 'f':
				farg = (float)va_arg(ap, int);
				/* Alloc for minus sign */
				if (farg < 0) {
					++len;
					farg = -farg;
				}
				/* Alloc for 3 decimal places + '.' */
				len += 4;
				/* Now get the log10 */
				do {
					++len;
					farg /= 10.0;
				} while (0 != farg);
				break;
			case 'g':
				darg = va_arg(ap, int);
				/* Alloc for minus sign */
				if (darg < 0) {
					++len;
					darg = -darg;
				}
				/* Alloc for 3 decimal places + '.' */
				len += 4;
				/* Now get the log10 */
				do {
					++len;
					darg /= 10.0;
				} while (0 != darg);
				break;
			case 'l':
				/* Get %ld %lx %lX %lf */
				if (i + 1 < strlen(fmt)) {
					++i;
					switch (fmt[i]) {
					case 'x':
					case 'X':
						larg = va_arg(ap, int);
						/* Now get the log16 */
						do {
							++len;
							larg /= 16;
						} while (0 != larg);
						break;
					case 'f':
						darg = va_arg(ap, int);
						/* Alloc for minus sign */
						if (darg < 0) {
							++len;
							darg = -darg;
						}
						/* Alloc for 3 decimal places + '.' */
						len += 4;
						/* Now get the log10 */
						do {
							++len;
							darg /= 10.0;
						} while (0 != darg);
						break;
					case 'd':
					default:
						larg = va_arg(ap, int);
						/* Now get the log10 */
						do {
							++len;
							larg /= 10;
						} while (0 != larg);
						break;
					}
				}
			default:
				break;
			}
		}
	}

	return len;
}

int OTF_asprintf(char** ptr, const char* fmt, ...) {
	int length;
	va_list ap;

	va_start(ap, fmt);
	length = OTF_vasprintf(ptr, fmt, ap);
	va_end(ap);

	return length;
}

int OTF_vasprintf(char** ptr, const char* fmt, va_list ap) {
	int length;
	va_list ap2;

	/* va_list might have pointer to internal state and using
	 * it twice is a bad idea.  So make a copy for the second
	 * use.  Copy order taken from Autoconf docs. */
#if defined(HAVE_VA_COPY) && HAVE_VA_COPY
	va_copy(ap2, ap);
#elif defined(HAVE_UNDERSCORE_VA_COPY) && HAVE_UNDERSCORE_VA_COPY
	__va_copy(ap2, ap);
#else
	memcpy (&ap2, &ap, sizeof(va_list));
#endif

	/* guess the size */
	length = guess_strlen(fmt, ap);

	/* allocate a buffer */
	*ptr = (char*) malloc((size_t) length + 1);
	if (NULL == *ptr) {
		errno = ENOMEM;
		va_end(ap2);
		return -1;
	}

	/* fill the buffer */
	length = vsprintf(*ptr, fmt, ap2);
#if (defined(HAVE_VA_COPY) && HAVE_VA_COPY) || (defined(HAVE_UNDERSCORE_VA_COPY) && HAVE_UNDERSCORE_VA_COPY)
	va_end(ap2);
#endif /* HAVE_VA_COPY || HAVE_UNDERSCORE_VA_COPY */

	/* realloc */
	*ptr = (char*)realloc(*ptr, (size_t)length + 1);
	if (NULL == *ptr) {
		errno = ENOMEM;
		return -1;
	}

	return length;
}

int OTF_snprintf(char* str, size_t size, const char* fmt, ...) {
	int length;
	va_list ap;

	va_start(ap, fmt);
	length = OTF_vsnprintf(str, size, fmt, ap);
	va_end(ap);

	return length;
}

int OTF_vsnprintf(char* str, size_t size, const char* fmt, va_list ap) {
	int length;
	char* buf;

	length = OTF_vasprintf(&buf, fmt, ap);
	if (length < 0) {
		return length;
	}

	/* return the length when given a null buffer (C99) */
	if (str) {
		if ((size_t) length < size) {
			strcpy(str, buf);
		} else {
			memcpy(str, buf, size - 1);
			str[size] = '\0';
		}
	}

	/* free allocated buffer */
	free(buf);
	buf = NULL;

	return length;
}

char* OTF_strdup(const char* s) {
	char* c;

	if (s == NULL || (c = (char*)malloc(strlen(s)+1)) == NULL) {
		errno = ENOMEM;
		return NULL;
	}

	strcpy(c, s) ;

	return c;
}

#endif /* windows/unix */

char* OTF_basename(char* path) {
	char *ret;
#if defined(_WIN32)
	const char* s = "\\";
#else
	const char* s = "/";
#endif
	
	if( path == NULL || strlen( path ) == 0 ) {
		ret = strdup( "." );
	} else if( path[strlen(path)-1] == *s ) {
		ret = strdup( s );
	} else {
		char* tmp;
		if( ( tmp = strrchr( path, *s ) ) != NULL )
			ret = strdup( tmp+1 );
		else
			ret = strdup( path );
	}
	
	return ret;
}

