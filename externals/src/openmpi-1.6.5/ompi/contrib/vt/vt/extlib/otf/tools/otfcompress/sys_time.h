/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifndef CUSTOM_SYS_TIME_H
#define CUSTOM_SYS_TIME_H


#ifdef WIN32

#include "OTF_Platform.h"

/* Based on timeval.h by Wu Yongwei */


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#define EPOCHFILETIME (116444736000000000LL)


#include <time.h>
#include <windows.h>


typedef struct {
  long tv_sec;
  long tv_usec;
} timeval;

struct timezone {
    int tz_minuteswest; /* minutes W of Greenwich */
    int tz_dsttime;     /* type of dst correction */
};


static int gettimeofday(struct timeval *tv, struct timezone *tz)
{
    FILETIME        ft;
    LARGE_INTEGER   li;
    __int64         t;
    static int      tzflag;

	if (tv) {
		GetSystemTimeAsFileTime(&ft);
		li.LowPart  = ft.dwLowDateTime;
		li.HighPart = ft.dwHighDateTime;
		t  = li.QuadPart;       /* In 100-nanosecond intervals */
		t -= EPOCHFILETIME;     /* Offset to the Epoch time */
		t /= 10;                /* In microseconds */
		tv->tv_sec  = (long)(t / 1000000);
		tv->tv_usec = (long)(t % 1000000);
    }

    if (tz) {
		if (!tzflag) {
			_tzset();
			tzflag++;
		}
		tz->tz_minuteswest = _timezone / 60;
		tz->tz_dsttime = _daylight;
    }

    return 0;
}


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* WIN32 */


#endif /* CUSTOM_SYS_TIME_H */
