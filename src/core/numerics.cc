/*
    File: numerics.cc
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

#define DEBUG_LEVEL_NONE
#include <float.h>
#include <math.h>

#include <clasp/core/foundation.h>
#include <clasp/core/lisp.h>
#include <clasp/core/numerics.h>
#ifdef darwin
#include <stdint.h>
#include <mach/mach_time.h>
#else
#include <time.h>
#endif
#include <clasp/core/bignum.h>
#include <boost/random.hpp>
#include <clasp/core/symbolTable.h>
#include <clasp/core/wrappers.h>

namespace core {

Bignum mixedBaseDigitsToBignum(const vector<int> &bases, const vector<int> &digits) {
  Bignum index;
  vector<int>::const_iterator bi, di;
  ASSERT(bases.size() == digits.size());
  ASSERT(bases.size() >= 1);
  ASSERT(digits[0] < bases[0]);
  index = digits[0];
  for (bi = bases.begin() + 1, di = digits.begin() + 1;
       bi != bases.end(); bi++, di++) {
    index = index * (*bi) + (*di);
    if (index < 0)
      break;
  }
  return index;
}

Bignum numberOfIndicesForMixedBase(const vector<int> &bases) {
  vector<int>::const_iterator bi;
  Bignum numSeq;
  ASSERT(bases.size() >= 1);
  numSeq = 1;
  for (bi = bases.begin(); bi != bases.end(); bi++) {
    numSeq = numSeq * (*bi);
    if (numSeq < 0)
      break;
  }
  return numSeq;
}

/*! Convert a collection of positive mixed-base digits to a LongLongInt index.
 * If the index can not be stored in a LongLongInt then return -1
 */
vector<int> bignumToMixedBaseDigits(const Bignum &index, const vector<int> &bases) {
  Bignum curIndex;
  vector<int> digits;
  vector<int>::const_reverse_iterator bi;
  vector<int>::reverse_iterator di;
  int digitIdx;
  curIndex = index;
  LOG(BF("*starting index=%20lld") % curIndex);
  ASSERT(bases.size() >= 1);
  digits.resize(bases.size());
  digitIdx = digits.size() - 1;
  for (bi = bases.rbegin(), di = digits.rbegin(); digitIdx >= 0; bi++, di++, digitIdx--) {
    Bignum bb = (curIndex % (*bi));
    *di = bb.get_si();
    curIndex /= *bi;
    LOG(BF("*di=%d  *bi=%d curIndex=%20lld") % *di % *bi % curIndex);
  }
  LOG(BF("digits[0] = %d") % digits[0]);
  return digits;
}

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("getUniversalTime");
CL_DEFUN Integer_sp cl__get_universal_time() {
  time_t current_time;
  time(&current_time);
  Integer_sp offset = Integer_O::create(static_cast<Fixnum>(2208988800));
  Integer_sp unix_time = Integer_O::create(static_cast<Fixnum>(current_time));
  Integer_sp utime = gc::As_unsafe<Integer_sp>(contagen_add(unix_time, offset));
  return utime;
}

boost::mt11213b globalRealRandom01Producer;
boost::uniform_real<> globalRealRandom01Distribution(0, 1);
boost::variate_generator<boost::mt11213b &, boost::uniform_real<>>
    globalRandomReal01Generator(globalRealRandom01Producer,
                                globalRealRandom01Distribution);
boost::mt11213b globalRealRandomNormal01Producer;
boost::normal_distribution<double> globalNormal01Distribution(0, 1);
boost::variate_generator<boost::mt11213b &, boost::normal_distribution<double>>
    globalRandomRealNormal01Generator(globalRealRandomNormal01Producer, globalNormal01Distribution);

CL_LISPIFY_NAME(seedRandomNumberGenerators);
CL_DEFUN void seedRandomNumberGenerators(uint i) {
  globalRealRandom01Producer.seed(static_cast<uint>(i));
  globalRealRandomNormal01Producer.seed(static_cast<uint>(i));
}

CL_LISPIFY_NAME(seedRandomNumberGeneratorsUsingTime);
CL_DEFUN void seedRandomNumberGeneratorsUsingTime() {
  clock_t currentTime;
  int tt;
#ifdef darwin
  currentTime = mach_absolute_time();
#else
  currentTime = clock();
#endif
  tt = currentTime % 32768;
  LOG(BF("seedRandomNumberGeneratorsUsingTime using value(%d)") % tt);
  seedRandomNumberGenerators(tt);
}

CL_LISPIFY_NAME(randomNumber01);
CL_DEFUN double randomNumber01() {
  return globalRandomReal01Generator();
}

CL_LISPIFY_NAME(randomNumberNormal01);
CL_DEFUN double randomNumberNormal01() {
  return globalRandomRealNormal01Generator();
}

bool almostEqualAbsoluteOrRelative(double va, double vb,
                                   double absEpsilon,
                                   double relEpsilon) {
  if (fabs(va - vb) < absEpsilon)
    return true;
  if (fabs(va) > fabs(vb)) {
    if (fabs(va - vb) < vb * relEpsilon)
      return true;
  } else {
    if (fabs(va - vb) < va * relEpsilon)
      return true;
  }
  return false;
}

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("asin");
CL_DEFUN double core__num_op_asin(double x) {
  return asin(x);
}

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("num-op-acos");
CL_DEFUN double core__num_op_acos(double x) {
  return acos(x);
}

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("asinh");
CL_DEFUN double core__num_op_asinh(double x) {
  return log(x + sqrt(1.0 + x * x));
}

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("num_op_acosh");
CL_DEFUN double core__num_op_acosh(double x) {
  return log(x + sqrt((x - 1) * (x + 1)));
}

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("atanh");
CL_DEFUN double core__num_op_atanh(double x) {
  return log((1 + x) / (1 - x)) / 2;
}
};

namespace core {

SYMBOL_EXPORT_SC_(ClPkg, getUniversalTime);
SYMBOL_EXPORT_SC_(ClPkg, leastPositiveSingleFloat);
SYMBOL_EXPORT_SC_(ClPkg, leastNegativeSingleFloat);
SYMBOL_EXPORT_SC_(ClPkg, mostPositiveSingleFloat);
SYMBOL_EXPORT_SC_(ClPkg, mostNegativeSingleFloat);
SYMBOL_EXPORT_SC_(ClPkg, leastPositiveShortFloat);
SYMBOL_EXPORT_SC_(ClPkg, leastNegativeShortFloat);
SYMBOL_EXPORT_SC_(ClPkg, mostPositiveShortFloat);
SYMBOL_EXPORT_SC_(ClPkg, mostNegativeShortFloat);
SYMBOL_EXPORT_SC_(ClPkg, leastPositiveDoubleFloat);
SYMBOL_EXPORT_SC_(ClPkg, leastNegativeDoubleFloat);
SYMBOL_EXPORT_SC_(ClPkg, mostPositiveDoubleFloat);
SYMBOL_EXPORT_SC_(ClPkg, mostNegativeDoubleFloat);
SYMBOL_EXPORT_SC_(ClPkg, leastPositiveLongFloat);
SYMBOL_EXPORT_SC_(ClPkg, leastNegativeLongFloat);
SYMBOL_EXPORT_SC_(ClPkg, mostPositiveLongFloat);
SYMBOL_EXPORT_SC_(ClPkg, mostNegativeLongFloat);
SYMBOL_EXPORT_SC_(ClPkg, leastNegativeNormalizedSingleFloat);
SYMBOL_EXPORT_SC_(ClPkg, leastNegativeNormalizedShortFloat);
SYMBOL_EXPORT_SC_(ClPkg, leastNegativeNormalizedDoubleFloat);
SYMBOL_EXPORT_SC_(ClPkg, leastNegativeNormalizedLongFloat);
SYMBOL_EXPORT_SC_(ClPkg, leastPositiveNormalizedSingleFloat);
SYMBOL_EXPORT_SC_(ClPkg, leastPositiveNormalizedShortFloat);
SYMBOL_EXPORT_SC_(ClPkg, leastPositiveNormalizedDoubleFloat);
SYMBOL_EXPORT_SC_(ClPkg, leastPositiveNormalizedLongFloat);
SYMBOL_EXPORT_SC_(ExtPkg, singleFloatPositiveInfinity);
SYMBOL_EXPORT_SC_(ExtPkg, singleFloatNegativeInfinity);
SYMBOL_EXPORT_SC_(ExtPkg, shortFloatPositiveInfinity);
SYMBOL_EXPORT_SC_(ExtPkg, shortFloatNegativeInfinity);
SYMBOL_EXPORT_SC_(ExtPkg, doubleFloatPositiveInfinity);
SYMBOL_EXPORT_SC_(ExtPkg, doubleFloatNegativeInfinity);
SYMBOL_EXPORT_SC_(ExtPkg, longFloatPositiveInfinity);
SYMBOL_EXPORT_SC_(ExtPkg, longFloatNegativeInfinity);
SYMBOL_EXPORT_SC_(ClPkg, pi);

void exposeCando_Numerics() {
  cl::_sym_mostPositiveSingleFloat->defconstant(clasp_make_single_float(FLT_MAX));
  cl::_sym_mostNegativeSingleFloat->defconstant(clasp_make_single_float(-FLT_MAX));
  cl::_sym_leastPositiveSingleFloat->defconstant(clasp_make_single_float(FLT_MIN));
  cl::_sym_leastNegativeSingleFloat->defconstant(clasp_make_single_float(-FLT_MIN));
  cl::_sym_mostPositiveShortFloat->defconstant(clasp_make_single_float(FLT_MAX));
  cl::_sym_mostNegativeShortFloat->defconstant(clasp_make_single_float(-FLT_MAX));
  cl::_sym_leastPositiveShortFloat->defconstant(clasp_make_single_float(FLT_MIN));
  cl::_sym_leastNegativeShortFloat->defconstant(clasp_make_single_float(-FLT_MIN));
  cl::_sym_mostPositiveDoubleFloat->defconstant(DoubleFloat_O::create(DBL_MAX));
  cl::_sym_mostNegativeDoubleFloat->defconstant(DoubleFloat_O::create(-DBL_MAX));
  cl::_sym_leastPositiveDoubleFloat->defconstant(DoubleFloat_O::create(DBL_MIN));
  cl::_sym_leastNegativeDoubleFloat->defconstant(DoubleFloat_O::create(-DBL_MIN));
  cl::_sym_mostPositiveLongFloat->defconstant(DoubleFloat_O::create(DBL_MAX));
  cl::_sym_mostNegativeLongFloat->defconstant(DoubleFloat_O::create(-DBL_MAX));
  cl::_sym_leastPositiveLongFloat->defconstant(DoubleFloat_O::create(DBL_MIN));
  cl::_sym_leastNegativeLongFloat->defconstant(DoubleFloat_O::create(-DBL_MIN));
  cl::_sym_leastNegativeNormalizedSingleFloat->defconstant(clasp_make_single_float(-std::numeric_limits<float>::denorm_min()));
  cl::_sym_leastNegativeNormalizedShortFloat->defconstant(clasp_make_single_float(-std::numeric_limits<float>::denorm_min()));
  cl::_sym_leastNegativeNormalizedDoubleFloat->defconstant(DoubleFloat_O::create(-std::numeric_limits<double>::denorm_min()));
  cl::_sym_leastNegativeNormalizedLongFloat->defconstant(LongFloat_O::create(-std::numeric_limits<LongFloat>::denorm_min()));
  // the following must be positive, not negative, fixes #434
  cl::_sym_leastPositiveNormalizedSingleFloat->defconstant(clasp_make_single_float(std::numeric_limits<float>::denorm_min()));
  cl::_sym_leastPositiveNormalizedShortFloat->defconstant(clasp_make_single_float(std::numeric_limits<float>::denorm_min()));
  cl::_sym_leastPositiveNormalizedDoubleFloat->defconstant(DoubleFloat_O::create(std::numeric_limits<double>::denorm_min()));
  cl::_sym_leastPositiveNormalizedLongFloat->defconstant(LongFloat_O::create(std::numeric_limits<LongFloat>::denorm_min()));
  cl::_sym_pi->defconstant(DoubleFloat_O::create(3.14159265358979323846264338));
  // extensions
  ext::_sym_singleFloatPositiveInfinity->defconstant(clasp_make_single_float(std::numeric_limits<float>::infinity()));
  ext::_sym_singleFloatNegativeInfinity->defconstant(clasp_make_single_float(-std::numeric_limits<float>::infinity()));
  ext::_sym_shortFloatPositiveInfinity->defconstant(clasp_make_single_float(std::numeric_limits<float>::infinity()));
  ext::_sym_shortFloatNegativeInfinity->defconstant(clasp_make_single_float(-std::numeric_limits<float>::infinity()));
  ext::_sym_doubleFloatPositiveInfinity->defconstant(DoubleFloat_O::create(std::numeric_limits<double>::infinity()));
  ext::_sym_doubleFloatNegativeInfinity->defconstant(DoubleFloat_O::create(-std::numeric_limits<double>::infinity()));
  ext::_sym_longFloatPositiveInfinity->defconstant(DoubleFloat_O::create(std::numeric_limits<double>::infinity()));
  ext::_sym_longFloatNegativeInfinity->defconstant(DoubleFloat_O::create(-std::numeric_limits<double>::infinity()));
}

};
