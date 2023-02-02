/*
    File: posixTime.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
//#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>

#include <time.h>
#include <sys/time.h>

#include <chrono>
#ifdef _TARGET_OS_LINUX
#include <sys/resource.h>
#endif

#ifdef __MACH__
#include <mach/clock.h>
#include <mach/mach.h>
#endif

#include <clasp/core/posixTime.h>
#include <clasp/core/symbolTable.h>
#include <clasp/gctools/interrupt.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/lisp.h>
#
// last include is wrappers.h
#include <clasp/core/wrappers.h>

namespace core {

struct timespec global__start_end_time;
struct timespec global__end_end_time;


/* Return the time in nanoseconds form the system defined starting time */
void systemReadClock(struct timespec &ts) {
#ifdef __MACH__ // OS X does not have clock_gettime, use clock_get_time
  clock_serv_t cclock;
  mach_timespec_t mts;
  host_get_clock_service(mach_host_self(), SYSTEM_CLOCK, &cclock);
  clock_get_time(cclock, &mts);
  mach_port_deallocate(mach_task_self(), cclock);
  ts.tv_sec = mts.tv_sec;
  ts.tv_nsec = mts.tv_nsec;
#else
  clock_gettime(CLOCK_REALTIME, &ts);
#endif
}

void first_exit() {
  if (strcmp(getenv("CLASP_TIME_EXIT"),"wait-start")==0 ||
      strcmp(getenv("CLASP_TIME_EXIT"),"wait-start-end")==0 ) {
    gctools::wait_for_user_signal("About to exit");
  }
  printf("%s:%d:%s About to exit\n", __FILE__, __LINE__, __FUNCTION__ );
  systemReadClock(global__start_end_time);
}

void last_exit() {
  systemReadClock(global__end_end_time);
  size_t seconds = global__end_end_time.tv_sec - global__start_end_time.tv_sec;
  if (strcmp(getenv("CLASP_TIME_EXIT"),"wait-")==0 ||
      strcmp(getenv("CLASP_TIME_EXIT"),"wait-start-end")==0 ) {
    gctools::wait_for_user_signal("About to exit");
  }
  if (getenv("CLASP_TIME_EXIT")) {
    printf("%s:%d:%s Number of seconds between first_exit and last_exit: %lu\n",
           __FILE__, __LINE__, __FUNCTION__, seconds );
  }
}


CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(getInternalRealTime)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__get_internal_real_time() {
  auto now = std::chrono::system_clock::now();
  auto d = now.time_since_epoch();
  auto us = std::chrono::duration_cast<std::chrono::microseconds>(d);
  return Integer_O::create(us.count() * (CLASP_INTERNAL_TIME_UNITS_PER_SECOND / 1000000 ));
};

/* Converts the two-tier time structure into one big number */
Bignum convertClockToNs(struct timespec &ts) {
  Bignum mpz_ns = ts.tv_sec;
  mpz_ns = mpz_ns * 1000000000; /* 1e9 ns/s */
  mpz_ns = mpz_ns + ts.tv_nsec;
  return mpz_ns;
}

Bignum systemTimeNs() {
  struct timespec ts;
  systemReadClock(ts);
  Bignum mpz_ns = convertClockToNs(ts);
  return mpz_ns;
}

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(clock_gettime_nanoseconds)dx");
DOCGROUP(clasp);
CL_DEFUN core::Integer_sp core__clock_gettime_nanoseconds() {
  Bignum ns = systemTimeNs();
  core::Integer_sp bn = core::Integer_O::create(ns);
  return bn;
};
  
CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(getInternalRunTime)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__get_internal_run_time() {
  return core__clock_gettime_nanoseconds();
#if 0
  struct rusage r;
  getrusage(RUSAGE_SELF, &r);
  size_t usec = r.ru_utime.tv_usec;
  size_t sec = r.ru_utime.tv_sec;
  mpz_class bn(sec);
  bn = bn * CLASP_INTERNAL_TIME_UNITS_PER_SECOND;
  bn = bn + usec * ( CLASP_INTERNAL_TIME_UNITS_PER_SECOND / 1000);
  return Integer_O::create(bn);
#endif
};

};
