/*
    File: profile.cc
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
#define DEBUG_LEVEL_FULL

#include <time.h>
#include <sys/time.h>

#ifdef __MACH__
#include <mach/clock.h>
#include <mach/mach.h>
#endif

//#include "clasp_gmpxx.h"

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/bignum.h>
#include <clasp/core/profile.h>
#include <clasp/core/wrappers.h>
namespace core {

Profiler _globalProfiler;
ProfilerFunctionInfo *_LinkedListOfProfilerFunctionInfos = NULL;

/*! Return the time in nanoseconds from the system defined starting time */
void profilerReadClock(struct timespec &ts) {
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

Bignum profilerConvertClockToNs(struct timespec &ts) {
  Bignum mpz_ns = ts.tv_sec;
  mpz_ns = mpz_ns * 1000000000 /* 1e9 ns*/;
  mpz_ns = mpz_ns + ts.tv_nsec;
  return mpz_ns;
}

/*! Return the time in nanoseconds from the system defined starting time */
Bignum profilerTimeNs() {
  struct timespec ts;
  profilerReadClock(ts);
  Bignum mpz_ns = profilerConvertClockToNs(ts);
  return mpz_ns;
}

#define ARGS_af_clock_gettime_nanoseconds "()"
#define DECL_af_clock_gettime_nanoseconds ""
#define DOCS_af_clock_gettime_nanoseconds "clock_gettime_nanoseconds"
core::Bignum_mv af_clock_gettime_nanoseconds() {
  _G();
  Bignum ns = profilerTimeNs();
  core::Bignum_sp bn = core::Bignum_O::create(ns);
  return (Values(bn));
};

#define ARGS_af_testProfileTimer "(delay)"
#define DECL_af_testProfileTimer ""
#define DOCS_af_testProfileTimer "testProfileTimer"
core::Bignum_mv af_testProfileTimer(uint delay) {
  _G();
  struct timespec start, stop;
  profilerReadClock(start);
  uint z = 0;
  for (uint i = 0; i < delay; i++) {
    z += i;
  }
  profilerReadClock(stop);
  Bignum mpz_start = profilerConvertClockToNs(start);
  Bignum mpz_stop = profilerConvertClockToNs(stop);
  core::Bignum_sp bn = core::Bignum_O::create(mpz_stop - mpz_start);
  return (Values(bn));
};

void ProfilerFunctionInfo::dumpProfileInfo() {
  double d_inclusive = this->_InclusiveTime.get_d() / PROFILER_NANOSECONDS_PER_SECOND;
  double d_self = this->_SelfTime.get_d() / PROFILER_NANOSECONDS_PER_SECOND;
  stringstream sinclusive, sself;
  profiler_print(BF("%5d %7.2f %7.2f == %s/%s/%d\n") % this->_TimesInvoked % d_inclusive % d_self % this->_FunctionName % this->_SourceFileName % this->_LineNumber);
}

ProfilerFunctionInfo::~ProfilerFunctionInfo() {
}

ProfilerFunctionTimer::ProfilerFunctionTimer(ProfilerFunctionInfo *funcInfo) : _ProfilerFunctionInfo(funcInfo),
                                                                               _CalleeElapsed(0) {}

ProfilerFunctionTimer::~ProfilerFunctionTimer() {}

void ProfilerFunctionTimer::startFunctionTimer() {
  this->_StartTime = profilerTimeNs();
  this->_CalleeElapsed = 0;
}

void ProfilerFunctionTimer::stopFunctionTimer(ProfilerFunctionTimer *caller) {
  Bignum endTime = profilerTimeNs();
  Bignum inclusiveElapsed = endTime - this->_StartTime;
  if (this->_ProfilerFunctionInfo != NULL) {
    ++(this->_ProfilerFunctionInfo->_TimesInvoked);
    this->_ProfilerFunctionInfo->_SelfTime += inclusiveElapsed - this->_CalleeElapsed;
    this->_ProfilerFunctionInfo->_InclusiveTime += inclusiveElapsed;
  }
  if (caller != NULL) {
    caller->_CalleeElapsed += inclusiveElapsed;
  }
}

void Profiler::start() {
  this->_ProgramStartTime = profilerTimeNs();
}

void Profiler::end() {
  Bignum endTime = profilerTimeNs();
  Bignum elapsed = endTime - this->_ProgramStartTime;
  double seconds = elapsed.get_d() / PROFILER_NANOSECONDS_PER_SECOND;
  profiler_print(BF("\n-----------------------------\n%lf TOTAL-ELAPSED-TIME(seconds)\n") % seconds);
  dump_profile();
}

void Profiler::enterFunction(ProfilerFunctionInfo &funcProfiler) {
  ProfilerFunctionTimer timer(&funcProfiler);
  timer.startFunctionTimer();
  this->_Stack.push_back(timer);
};

void Profiler::exitFunction() {
  ProfilerFunctionTimer &top = this->_Stack.back();
  ProfilerFunctionTimer *caller = NULL;
  if (this->_Stack.size() > 1) {
    caller = &(this->_Stack[this->_Stack.size() - 2]);
  }
  top.stopFunctionTimer(caller);
  this->_Stack.pop_back();
}

void profiler_print(boost::format &fmt) {
  TRY_BOOST_FORMAT_STRING(fmt, fmt_str);
  printf("+CPROF+ %s", fmt_str.c_str());
}

void initialize_profile() {
  _G();
  SYMBOL_SC_(CorePkg, clock_gettime_nanoseconds);
  Defun(clock_gettime_nanoseconds);
  SYMBOL_SC_(CorePkg, testProfileTimer);
  Defun(testProfileTimer);
}

void restart_profile() {
  _globalProfiler.start();
  /* First reset all of the timers currently on the Profiler stack */
  for (vector<ProfilerFunctionTimer>::iterator it = _globalProfiler._Stack.begin();
       it != _globalProfiler._Stack.end(); it++) {
    (*it).startFunctionTimer();
  }
  /* Now wipe out any accumulated time in all of the functions being profiled */
  ProfilerFunctionInfo *cur = _LinkedListOfProfilerFunctionInfos;
  for (; cur != NULL; cur = cur->_Next) {
    cur->_TimesInvoked = 0;
    cur->_InclusiveTime = 0;
    cur->_SelfTime = 0;
  }
}

#if 0
class DumpFindClassCount : public KeyValueMapper
{
public:
    virtual bool mapKeyValue(T_sp className, T_sp omc)
    {_G();
	Symbol_sp sym = className.as<Symbol_O>();
	Class_sp mc = omc.as<Class_O>();
	printf( "+PROFILE-FIND-CLASS-COUNT+ %6d %20s\n", mc->findClassCount(), sym->__repr__().c_str() );
	return true;
    }
};
#endif

void dump_profile() {
  ProfilerFunctionInfo *cur = _LinkedListOfProfilerFunctionInfos;
  for (; cur != NULL; cur = cur->_Next) {
    cur->dumpProfileInfo();
  }
  //	DumpFindClassCount dumpCount;
  //	_lisp->mapClassNamesAndClasses(&dumpCount);
}

simple_timer::simple_timer(const std::string &msg) {
  this->_Message = msg;
  this->_StartTime = profilerTimeNs();
};

simple_timer::~simple_timer() {
  Bignum endTime = profilerTimeNs();
  Bignum delta = endTime - this->_StartTime;
  double ddelta = delta.get_d();
  double seconds_delta = ddelta / 1000000000.0;
  printf("%s %8.3lf seconds\n", this->_Message.c_str(), seconds_delta);
}
};
