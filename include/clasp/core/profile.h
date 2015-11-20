/*
    File: profile.h
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
#ifndef _core_profile_H
#define _core_profile_H

#include <clasp/core/clasp_gmpxx.h>

namespace core {

#define PROFILER_NANOSECONDS_PER_SECOND 1000000000.0

struct ProfilerFunctionInfo;

extern ProfilerFunctionInfo *_LinkedListOfProfilerFunctionInfos;

/*! Class to profile a single function */
struct ProfilerFunctionInfo {
  ProfilerFunctionInfo *_Next;
  const char *_SourceFileName;
  const char *_FunctionName;
  uint _LineNumber;
  uint _InitializeSensor;
  uint _TimesInvoked;
  Bignum _InclusiveTime;
  Bignum _SelfTime;

  ProfilerFunctionInfo(const char *fileName, const char *functionName, uint lineNumber) : _Next(NULL),
                                                                                          _SourceFileName(fileName),
                                                                                          _FunctionName(functionName),
                                                                                          _LineNumber(lineNumber),
                                                                                          _InitializeSensor(0),
                                                                                          _TimesInvoked(0),
                                                                                          _InclusiveTime(0),
                                                                                          _SelfTime(0) {
    this->_Next = _LinkedListOfProfilerFunctionInfos;
    _LinkedListOfProfilerFunctionInfos = this;
  };

  void dumpProfileInfo();

  /*! Dump the info for this function */
  virtual ~ProfilerFunctionInfo();
};

/*! Class to keep track of timers on profile stack */
struct ProfilerFunctionTimer {
  ProfilerFunctionInfo *_ProfilerFunctionInfo;
  //	ProfilerFunctionTimer* 	_CallerFunctionTimer;
  Bignum _StartTime;
  Bignum _CalleeElapsed;

  ProfilerFunctionTimer(ProfilerFunctionInfo *funcInfo);
  void startFunctionTimer();
  void stopFunctionTimer(ProfilerFunctionTimer *callerFunctionTimer);

  virtual ~ProfilerFunctionTimer();
};

class Profiler {
  friend void restart_profile();

private:
  vector<ProfilerFunctionTimer> _Stack;
  Bignum _ProgramStartTime;

public:
  void print(boost::format &fmt);
  void start();
  void end();

  void enterFunction(ProfilerFunctionInfo &funcInfo);
  void exitFunction();
};

void profiler_print(boost::format &fmt);

class ProfilerGuard {
private:
  Profiler *profilerP;

public:
  ProfilerGuard(Profiler &profiler, ProfilerFunctionInfo *funcInfoP) {
    this->profilerP = &profiler;
    profilerP->enterFunction(*funcInfoP);
  }

  virtual ~ProfilerGuard() {
    profilerP->exitFunction();
  }
};

#if ENABLE_PROFILING
#define _PROFILE_FUNCTION()                                                                      \
  static core::ProfilerFunctionInfo *___profilerFunctionInfoP = NULL;                            \
  if (___profilerFunctionInfoP == NULL) {                                                        \
    ___profilerFunctionInfoP = new core::ProfilerFunctionInfo(__FILE__, __FUNCTION__, __LINE__); \
  }                                                                                              \
  core::ProfilerGuard ___profilerGuard(core::_globalProfiler, ___profilerFunctionInfoP);
#define DUMP_PROFILING() core::dump_profile_info();
#define RESTART_PROFILING() core::restart_profile();
#else
#define _PROFILE_FUNCTION() \
  {}
#define DUMP_PROFILING() \
  {}
#define RESTART_PROFILING() \
  {}
#endif

extern Profiler _globalProfiler;

/*! Provide the time in nanoseconds since a system defined epoch */
Bignum profilerTimeNs();

void initialize_profile();

/*! Reset the clocks on all functions to start profiling from the current time */
void restart_profile();

/*! Dump all profiling information */
void dump_profile();

/*! Dump all profiling information and shutdown profiling */
void shutdown_profile();

/*! Class to keep track of timers on profile stack */
struct simple_timer {
  //	ProfilerFunctionTimer* 	_CallerFunctionTimer;
  std::string _Message;
  Bignum _StartTime;

  simple_timer(const std::string &message);
  virtual ~simple_timer();
};
};
#endif // _core_profile_H
