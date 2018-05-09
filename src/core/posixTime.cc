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
#undef USEBOOSTPYTHON
//#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>

#ifdef _TARGET_OS_LINUX
#include <sys/time.h>
#include <sys/resource.h>
#endif
#include <clasp/core/profile.h>
#include <clasp/core/posixTime.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/lisp.h>
#
// last include is wrappers.h
#include <clasp/core/wrappers.h>

namespace core {

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("getInternalRealTime");
CL_DEFUN T_sp cl__get_internal_real_time() {
  PosixTime_sp now = PosixTime_O::createNow();
  PosixTimeDuration_sp diff = now->sub(gc::As<PosixTime_sp>(_sym_STARstartRunTimeSTAR->symbolValue()));
  return Integer_O::create(diff->totalMilliseconds() * (CLASP_INTERNAL_TIME_UNITS_PER_SECOND / 1000 ));
};


CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("clock_gettime_nanoseconds");
CL_DEFUN core::Integer_sp core__clock_gettime_nanoseconds() {
  Bignum ns = profilerTimeNs();
  core::Integer_sp bn = core::Integer_O::create(ns);
  return bn;
};
  
CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("getInternalRunTime");
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

PosixTime_sp PosixTime_O::createNow() {
  PosixTime_sp now = PosixTime_O::create();
  now->setToLocalTime();
  return now;
}

void PosixTime_O::initialize() {
  this->Base::initialize();
}

#if defined(XML_ARCHIVE)
void PosixTime_O::archiveBase(ArchiveP node) {
  this->Base::archiveBase(node);
  if (node->loading()) {
    string iso;
    node->attribute("iso", iso);
    this->_Time = boost::posix_time::from_iso_string(iso);
  } else {
    string iso;
    iso = boost::posix_time::to_iso_string(this->_Time);
    node->attribute("iso", iso);
  }
}
#endif // defined(XML_ARCHIVE)

CL_LISPIFY_NAME("setToLocalTime");
CL_DEFMETHOD PosixTime_sp PosixTime_O::setToLocalTime() {
  this->_Time = boost::posix_time::microsec_clock::local_time();
  return this->sharedThis<PosixTime_O>();
}

string PosixTime_O::toSimpleString() {
  stringstream ss;
  ss << to_simple_string(this->_Time);
  return ss.str();
}

CL_LISPIFY_NAME("sub");
CL_DEFMETHOD PosixTimeDuration_sp PosixTime_O::sub(PosixTime_sp tt) {
  PosixTimeDuration_sp result = PosixTimeDuration_O::create();
  result->_Duration = this->_Time - tt->_Time;
  return result;
}

#if 0
    PosixTimeDuration_sp PosixTimeDuration_O::make(uint hours, uint minutes, uint seconds, uint milliseconds )
    {
	LongLongInt hours = env->lookup(CorePkg,"hours")->object().as<Rational_O>()->as_int();
    LongLongInt minutes = env->lookup(CorePkg,"minutes")->object().as<Rational_O>()->as_int();
    LongLongInt seconds = env->lookup(CorePkg,"seconds")->object().as<Rational_O>()->as_int();
    LongLongInt milliseconds = env->lookup(CorePkg,"milliseconds")->object().as<Rational_O>()->as_int();
    this->_Duration = boost::posix_time::hours(hours)
			+ boost::posix_time::minutes(minutes)
			+ boost::posix_time::seconds(seconds)
			+ boost::posix_time::milliseconds(milliseconds);
    return _Nil<T_O>();
}
#endif
PosixTimeDuration_sp PosixTimeDuration_O::createDurationSince(PosixTime_sp past) {
  PosixTime_sp now = PosixTime_O::createNow();
  PosixTimeDuration_sp delta = now->sub(past);
  return delta;
}
void PosixTimeDuration_O::initialize() {
  this->Base::initialize();
}

#if defined(XML_ARCHIVE)
void PosixTimeDuration_O::archiveBase(ArchiveP node) {
  this->Base::archiveBase(node);
  IMPLEMENT_ME();
}
#endif // defined(XML_ARCHIVE)

CL_LISPIFY_NAME("sub");
CL_DEFMETHOD PosixTimeDuration_sp PosixTimeDuration_O::sub(PosixTimeDuration_sp tt) {
  PosixTimeDuration_sp result = PosixTimeDuration_O::create();
  result->_Duration = this->_Duration - tt->_Duration;
  return result;
}

CL_LISPIFY_NAME("totalSeconds");
CL_DEFMETHOD mpz_class PosixTimeDuration_O::totalSeconds() {
  static_assert(sizeof(long long) == sizeof(signed long int),"The size of long long does not match the size of signed long int");
  return mpz_class((signed long int)this->_Duration.total_seconds());
}

CL_LISPIFY_NAME("totalMilliseconds");
CL_DEFMETHOD mpz_class PosixTimeDuration_O::totalMilliseconds() {
  _OF();
  stringstream ss;
  ss << this->_Duration.total_milliseconds();
  return mpz_class(ss.str());
}

mpz_class PosixTimeDuration_O::totalMicroseconds() {
  _OF();
  stringstream ss;
  ss << this->_Duration.total_microseconds();
  return mpz_class(ss.str());
}

mpz_class PosixTimeDuration_O::fractionalSeconds() {
  _OF();
  stringstream ss;
  ss << this->_Duration.fractional_seconds();
  return mpz_class(ss.str());
}

CL_LISPIFY_NAME("posix-time-duration-seconds");
CL_DEFMETHOD uint PosixTimeDuration_O::seconds() {
  return this->_Duration.seconds();
}

CL_LISPIFY_NAME("minutes");
CL_DEFMETHOD uint PosixTimeDuration_O::minutes() {
  return this->_Duration.minutes();
}

CL_LISPIFY_NAME("hours");
CL_DEFMETHOD uint PosixTimeDuration_O::hours() {
  return this->_Duration.hours();
}

CL_LISPIFY_NAME("toSimpleString");
CL_DEFMETHOD string PosixTimeDuration_O::toSimpleString() {
  return boost::posix_time::to_simple_string(this->_Duration);
}

CL_LISPIFY_NAME("toIsoString");
CL_DEFMETHOD string PosixTimeDuration_O::toIsoString() {
  return boost::posix_time::to_iso_string(this->_Duration);
}









};
