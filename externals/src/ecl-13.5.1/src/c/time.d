/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    time.c -- Time routines.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <limits.h>
#include <time.h>
#ifndef _MSC_VER
# include <unistd.h>
# include <errno.h>
#endif
#if defined(_MSC_VER) || defined(__MINGW32__)
# include <windows.h>
# include <winsock.h>
#endif

#define ECL_INCLUDE_MATH_H
#include <ecl/ecl.h>
#include <ecl/internal.h>
#include <ecl/number.h>
#ifdef HAVE_TIMES
# include <sys/times.h>
#endif
#ifdef HAVE_GETRUSAGE
# include <sys/time.h>
# include <sys/resource.h>
#endif
#ifdef HAVE_GETTIMEOFDAY
# include <sys/time.h>
#endif
#include <ecl/impl/math_fenv.h>

static struct ecl_timeval beginning;

void
ecl_get_internal_real_time(struct ecl_timeval *tv)
{
#if defined(HAVE_GETTIMEOFDAY) && !defined(ECL_MS_WINDOWS_HOST)
	struct timezone tz;
	struct timeval aux;
	gettimeofday(&aux, &tz);
	tv->tv_usec = aux.tv_usec;
	tv->tv_sec = aux.tv_sec;
#else
# if defined(ECL_MS_WINDOWS_HOST)
	union {
		FILETIME filetime;
		DWORDLONG hundred_ns;
	} system_time;
	GetSystemTimeAsFileTime(&system_time.filetime);
	system_time.hundred_ns /= 10000;
	tv->tv_sec = system_time.hundred_ns / 1000;
	tv->tv_usec = (system_time.hundred_ns % 1000) * 1000;
# else
	time_t = time(0);
	tv->tv_sec = time_t;
	tv->tv_usec = 0;
# endif
#endif
}

void
ecl_get_internal_run_time(struct ecl_timeval *tv)
{
#ifdef HAVE_GETRUSAGE
	struct rusage r;
	getrusage(RUSAGE_SELF, &r);
	tv->tv_usec = r.ru_utime.tv_usec;
	tv->tv_sec = r.ru_utime.tv_sec;
#else
# ifdef HAVE_TIMES
	struct tms buf;
	times(&buf);
	tv->tv_sec = buf.tms_utime / CLK_TCK;
	tv->tv_usec = (buf.tms_utime % CLK_TCK) * 1000000;
# else
#  if defined(ECL_MS_WINDOWS_HOST)
	union {
		FILETIME filetime;
		DWORDLONG hundred_ns;
	} kernel_time, user_time, creation_time, exit_time;
	if (!GetProcessTimes(GetCurrentProcess(),
	                     &creation_time.filetime,
	                     &exit_time.filetime,
	                     &kernel_time.filetime,
	                     &user_time.filetime))
	    FEwin32_error("GetProcessTimes() failed", 0);
	kernel_time.hundred_ns += user_time.hundred_ns;
	kernel_time.hundred_ns /= 10000;
	tv->tv_sec = kernel_time.hundred_ns / 1000;
	tv->tv_usec = (kernel_time.hundred_ns % 1000) * 1000;
#  else
	ecl_get_internal_real_time(tv);
#  endif
# endif
#endif
}

void
ecl_musleep(double time, bool alertable)
{
#ifdef HAVE_NANOSLEEP
	struct timespec tm;
	int code;
	tm.tv_sec = (time_t)floor(time);
	tm.tv_nsec = (long)((time - floor(time)) * 1e9);
 AGAIN:
	code = nanosleep(&tm, &tm);
	{
		int old_errno = errno;
		if (code < 0 && old_errno == EINTR && !alertable) {
			goto AGAIN;
		}
	}
#else
#if defined (ECL_MS_WINDOWS_HOST)
	/* Maximum waiting time that fits in SleepEx. This is the
	 * largest integer that fits safely in DWORD in milliseconds
	 * and has to be converted to 100ns (1e-3 / 100e-9 = 1e4) */
	const DWORDLONG maxtime = (DWORDLONG)0xfffffff * (DWORDLONG)10000;
	DWORDLONG wait = time * 1e7;
	union {
		FILETIME filetime;
		DWORDLONG hundred_ns;
	} end, now;
	if (alertable) {
		GetSystemTimeAsFileTime(&end.filetime);
		end.hundred_ns += wait;
	}
	do {
		DWORDLONG interval;
		if (wait > maxtime) {
			interval = maxtime;
			wait -= maxtime;
		} else {
			interval = wait;
			wait = 0;
		}
		if (SleepEx(interval/10000, alertable) != 0) {
			if (alertable) {
				break;
			} else {
				GetSystemTimeAsFileTime(&now.filetime);
				if (now.hundred_ns >= end.hundred_ns)
					break;
				else
					wait = end.hundred_ns - now.hundred_ns;
			}
		}
	} while (wait);
#else
	int t = (int)time;
	for (t = (time + 0.5); t > 1000; t -= 1000)
		sleep(1000);
	sleep(t);
#endif
#endif
}

cl_fixnum
ecl_runtime(void)
{
	struct ecl_timeval tv;
	ecl_get_internal_run_time(&tv);
	return tv.tv_sec * 1000 + tv.tv_usec / 1000;
}

cl_object
cl_sleep(cl_object z)
{
        double time;
	/* INV: ecl_minusp() makes sure `z' is real */
	if (ecl_minusp(z))
		cl_error(9, @'simple-type-error', @':format-control',
			    make_constant_base_string("Not a non-negative number ~S"),
			    @':format-arguments', cl_list(1, z),
			    @':expected-type', @'real', @':datum', z);
        /* Compute time without overflows */
        ECL_WITHOUT_FPE_BEGIN {
                time = ecl_to_double(z);
                if (isnan(time) || !isfinite(time) || (time > INT_MAX)) {
                        time = INT_MAX;
                } else if (time < 1e-9) {
                        time = 1e-9;
                }
        } ECL_WITHOUT_FPE_END;
	ecl_musleep(time, 0);
	@(return ECL_NIL)
}

static cl_object
timeval_to_time(long sec, long usec)
{
	cl_object milliseconds = ecl_plus(ecl_times(ecl_make_integer(sec),
						    ecl_make_fixnum(1000)),
					  ecl_make_integer(usec / 1000));
	@(return milliseconds);
}

cl_object
cl_get_internal_run_time()
{
	struct ecl_timeval tv;
	ecl_get_internal_run_time(&tv);
	return timeval_to_time(tv.tv_sec, tv.tv_usec);
}

cl_object
cl_get_internal_real_time()
{
	struct ecl_timeval tv;
	ecl_get_internal_real_time(&tv);
	return timeval_to_time(tv.tv_sec - beginning.tv_sec,
			       tv.tv_usec - beginning.tv_usec);
}

cl_object
cl_get_universal_time()
{
	cl_object utc = ecl_make_integer(time(0));
	@(return ecl_plus(utc, cl_core.Jan1st1970UT))
}

void
init_unixtime(void)
{
	ecl_get_internal_real_time(&beginning);

	ECL_SET(@'internal-time-units-per-second', ecl_make_fixnum(1000));

	cl_core.Jan1st1970UT =
	    ecl_times(ecl_make_fixnum(24 * 60 * 60),
			 ecl_make_fixnum(17 + 365 * 70));
}
