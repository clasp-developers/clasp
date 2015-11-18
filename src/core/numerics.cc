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
  _G();
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
  _G();
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
  _G();
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

#define ARGS_af_getUniversalTime "()"
#define DECL_af_getUniversalTime ""
#define DOCS_af_getUniversalTime "getUniversalTime"
Integer_sp af_getUniversalTime() {
  _G();
  time_t current_time;
  time(&current_time);
  Integer_sp offset = Integer_O::create(2208988800);
  Integer_sp unix_time = Integer_O::create(current_time);
  Integer_sp utime = contagen_add(unix_time, offset);
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

void seedRandomNumberGenerators(uint i) {
  _G();
  globalRealRandom01Producer.seed(static_cast<uint>(i));
  globalRealRandomNormal01Producer.seed(static_cast<uint>(i));
}

void seedRandomNumberGeneratorsUsingTime() {
  _G();
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

double randomNumber01() {
  return globalRandomReal01Generator();
}

double randomNumberNormal01() {
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

#if 0

#define ARGS_af_ceiling1 "(x)"
#define DECL_af_ceiling1 ""
#define DOCS_af_ceiling1 "ceiling1"
    Number_sp af_ceiling1(Number_sp x)
    {_G();
	Number_sp v0, v1;
	Number_mv mv_v1;
	switch (clasp_t_of(x)) {
	case number_Fixnum:
	case number_Bignum:
	    v0 = x;
	    v1 = make_fixnum(0);
	    break;
	case number_Ratio: {
//		const cl_env_ptr the_env = ecl_process_env();
	    Ratio_sp ratio_x = x.as<Ratio_O>();
	    mv_v1 = af_ceiling2(ratio_x->numerator(),ratio_x->denominator());
	    v1 = Ratio_O::create(mv_v1.getValue(1).as<Integer_O>(), ratio_x->denominator());
	    break;
	}
	case number_SingleFloat: {
	    float d = x.as<SingleFloat_O>()->get();
	    float y = ceilf(d);
	    v0 = Integer_O::create(y);
	    v1 = clasp_make_single_float(d - y);
	    break;
	}
	case number_DoubleFloat: {
	    double d = x.as<DoubleFloat_O>()->get();
	    double y = ceil(d);
	    v0 = Integer_O::create(y);
	    v1 = DoubleFloat_O::create(d - y);
	    break;
	}
#ifdef CLASP_LONG_FLOAT
	case number_LongFloat: {
	    LongFloat d = x.as<LongFloat_O>()->get();
	    LongFloat y = ceill(d);
	    v0 = Integer_O::create(y);
	    v1 = LongFLoat_O::create(d - y);
	    break;
	}
#endif
	default:
	    QERROR_WRONG_TYPE_NTH_ARG(_sym_ceiling, 1, x, _sym_Real_O);
	}
	return Values(v0,v1);
    }

    Number_sp
    ecl_ceiling2(Number_sp x, Number_sp y)
    {
	const cl_env_ptr the_env = ecl_process_env();
	Number_sp v0, v1;
	cl_type ty;
        ty = clasp_t_of(y);
	if (ecl_unlikely(!ECL_REAL_TYPE_P(ty))) {
            FEwrong_type_nth_arg(@[ceiling],2, y, @[real]);
	}
	switch(clasp_t_of(x)) {
	case number_Fixnum:
            switch(ty) {
            case number_Fixnum: {	/* FIX / FIX */
                gctools::Fixnum a = ecl_fixnum(x); cl_fixnum b = ecl_fixnum(y);
                gctools::Fixnum q = a / b;  cl_fixnum r = a % b;
                if ((r^b) > 0 && r) {	/* same signs and some remainder */
		    v0 = make_fixnum(q+1);
		    v1 = make_fixnum(r-b);
                } else {
		    v0 = make_fixnum(q);
		    v1 = make_fixnum(r);
                }
                break;
            }
            case number_Bignum: {	/* FIX / BIG */
                /* We must perform the division because there is the
                 * pathological case
                 *	x = MOST_NEGATIVE_FIXNUM
                 *    y = - MOST_NEGATIVE_FIXNUM
                 */
                ECL_WITH_TEMP_BIGNUM(bx,4);
                _ecl_big_senumber_Fixnum(bx, ecl_fixnum(x));
                v0 = _ecl_big_ceiling(bx, y, &v1);
                break;
            }
            case number_Ratio:		/* FIX / RAT */
                v0 = ecl_ceiling2(ecl_times(x, y->ratio.den), y->ratio.num);
                v1 = ecl_make_ratio(ecl_nth_value(the_env, 1), y->ratio.den);
                break;
            case number_SingleFloat: {	/* FIX / SF */
                float n = ecl_single_float(y);
                float p = ecl_fixnum(x)/n;
                float q = ceilf(p);
                v0 = _ecl_float_to_integer(q);
                v1 = ecl_make_single_float(p*n - q*n);
                break;
            }
            case number_DoubleFloat: {	/* FIX / DF */
                double n = ecl_double_float(y);
                double p = ecl_fixnum(x)/n;
                double q = ceil(p);
                v0 = _ecl_double_to_integer(q);
                v1 = ecl_make_double_float(p*n - q*n);
                break;
            }
#ifdef CLASP_LONG_FLOAT
            case number_LongFloat: {	/* FIX / LF */
                LongFloat n = ecl_long_float(y);
                LongFloat p = ecl_fixnum(x)/n;
                LongFloat q = ceill(p);
                v0 = _ecl_long_double_to_integer(q);
                v1 = ecl_make_long_float(p*n - q*n);
                break;
            }
#endif
            default:
                (void)0; /*Never reached */
            }
            break;
	case number_Bignum:
	    switch(clasp_t_of(y)) {
            case number_Fixnum: {	/* BIG / FIX */
                ECL_WITH_TEMP_BIGNUM(by,4);
                _ecl_big_senumber_Fixnum(by, ecl_fixnum(y));
                v0 = _ecl_big_ceiling(x, by, &v1);
                break;
            }
            case number_Bignum: {	/* BIG / BIG */
                v0 = _ecl_big_ceiling(x, y, &v1);
                break;
            }
            case number_Ratio:		/* BIG / RAT */
                v0 = ecl_ceiling2(ecl_times(x, y->ratio.den), y->ratio.num);
                v1 = ecl_make_ratio(ecl_nth_value(the_env, 1), y->ratio.den);
                break;
            case number_SingleFloat: {	/* BIG / SF */
                float n = ecl_single_float(y);
                float p = _ecl_big_to_double(x)/n;
                float q = ceilf(p);
                v0 = _ecl_float_to_integer(q);
                v1 = ecl_make_single_float(p*n - q*n);
                break;
            }
            case number_DoubleFloat: {	/* BIG / DF */
                double n = ecl_double_float(y);
                double p = _ecl_big_to_double(x)/n;
                double q = ceil(p);
                v0 = _ecl_double_to_integer(q);
                v1 = ecl_make_double_float(p*n - q*n);
                break;
            }
#ifdef CLASP_LONG_FLOAT
            case number_LongFloat: {	/* BIG / LF */
                LongFloat n = ecl_long_float(y);
                LongFloat p = _ecl_big_to_double(x)/n;
                LongFloat q = ceill(p);
                v0 = _ecl_long_double_to_integer(q);
                v1 = ecl_make_long_float(p*n - q*n);
                break;
            }
#endif
            default:
                (void)0; /*Never reached */
            }
            break;
	case number_Ratio:
	    switch(clasp_t_of(y)) {
            case number_Ratio:		/* RAT / RAT */
                v0 = ecl_ceiling2(ecl_times(x->ratio.num, y->ratio.den),
                                  ecl_times(x->ratio.den, y->ratio.num));
                v1 = ecl_make_ratio(ecl_nth_value(the_env, 1), ecl_times(x->ratio.den, y->ratio.den));
                break;
            default:		/* RAT / ANY */
                v0 = ecl_ceiling2(x->ratio.num, ecl_times(x->ratio.den, y));
                v1 = ecl_divide(ecl_nth_value(the_env, 1), x->ratio.den);
            }
            break;
	case number_SingleFloat: {		/* SF / ANY */
            float n = ecl_to_double(y);
            float p = ecl_single_float(x)/n;
            float q = ceilf(p);
            v0 = _ecl_float_to_integer(q);
            v1 = ecl_make_single_float(p*n - q*n);
            break;
	}
	case number_DoubleFloat: {		/* DF / ANY */
            double n = ecl_to_double(y);
            double p = ecl_double_float(x)/n;
            double q = ceil(p);
            v0 = _ecl_double_to_integer(q);
            v1 = ecl_make_double_float(p*n - q*n);
            break;
	}
#ifdef CLASP_LONG_FLOAT
	case number_LongFloat: {		/* LF / ANY */
            LongFloat n = ecl_to_long_double(y);
            LongFloat p = ecl_long_float(x)/n;
            LongFloat q = ceill(p);
            v0 = _ecl_long_double_to_integer(q);
            v1 = ecl_make_long_float(p*n - q*n);
            break;
	}
#endif
	default:
            FEwrong_type_nth_arg(@[ceiling], 1, x, @[real]);
	}
	ecl_return2(the_env, v0, v1);
    }

#define ARGS_af_ceiling "(x &optional y)"
#define DECL_af_ceiling ""
#define DOCS_af_ceiling "ceiling"
    Number_sp af_ceiling(Number_sp x, Number_sp y)
    {_G();
	if ( y.nilp() ) return af_ceiling1(x);
	return af_ceiling2(x,y);
    }

#endif

#if 0

    static cl_object
    number_remainder(cl_object x, cl_object y, cl_object q)
    {
	cl_object z;

	z = ecl_times(q, y);
	z = ecl_minus(x, z);
	return(z);
    }

/* Coerce X to single-float if one arg,
   otherwise coerce to same float type as second arg */

    @(defun float (x &optional (y OBJNULL))
      cl_type ty, tx;
      @
      if (y != OBJNULL) {
          ty = clasp_t_of(y);
      } else {
          ty = number_SingleFloat;
      }
      switch (tx = clasp_t_of(x)) {
      case number_SingleFloat:
      case number_DoubleFloat:
#ifdef CLASP_LONG_FLOAT
      case number_LongFloat:
#endif
          if (y == OBJNULL || ty == tx)
              break;
      case number_Fixnum:
      case number_Bignum:
      case number_Ratio:
          switch (ty) {
          case number_SingleFloat:
              x = ecl_make_single_float(ecl_to_double(x)); break;
          case number_DoubleFloat:
              x = ecl_make_double_float(ecl_to_double(x)); break;
#ifdef CLASP_LONG_FLOAT
          case number_LongFloat:
              x = ecl_make_long_float(ecl_to_long_double(x)); break;
#endif
          default:
              FEwrong_type_nth_arg(@[float],2,y,@[float]);
          }
          break;
      default:
          FEwrong_type_nth_arg(@[float],1,x,@[real]);
      }
      @(return x)
      @)

    cl_object
    cl_numerator(cl_object x)
    {
        switch (clasp_t_of(x)) {
	case number_Ratio:
            x = x->ratio.num;
            break;
	case number_Fixnum:
	case number_Bignum:
            break;
	default:
            FEwrong_type_nth_arg(@[numerator],1,x,@[rational]);
	}
	@(return x)
            }

    cl_object
    cl_denominator(cl_object x)
    {
        switch (clasp_t_of(x)) {
	case number_Ratio:
            x = x->ratio.den;
            break;
	case number_Fixnum:
	case number_Bignum:
            x = make_fixnum(1);
            break;
	default:
            FEwrong_type_nth_arg(@[numerator],1,x,@[rational]);
	}
	@(return x)
            }

    cl_object
    ecl_floor1(cl_object x)
    {
	const cl_env_ptr the_env = ecl_process_env();
	cl_object v0, v1;
	switch (clasp_t_of(x)) {
	case number_Fixnum:
	case number_Bignum:
            v0 = x;
            v1 = make_fixnum(0);
            break;
	case number_Ratio:
            v0 = ecl_floor2(x->ratio.num, x->ratio.den);
            v1 = ecl_make_ratio(ecl_nth_value(the_env, 1), x->ratio.den);
            break;
	case number_SingleFloat: {
            float d = ecl_single_float(x);
            float y = floorf(d);
            v0 = _ecl_float_to_integer(y);
            v1 = ecl_make_single_float(d - y);
            break;
	}
	case number_DoubleFloat: {
            double d = ecl_double_float(x);
            double y = floor(d);
            v0 = _ecl_double_to_integer(y);
            v1 = ecl_make_double_float(d - y);
            break;
	}
#ifdef CLASP_LONG_FLOAT
	case number_LongFloat: {
            LongFloat d = ecl_long_float(x);
            LongFloat y = floorl(d);
            v0 = _ecl_long_double_to_integer(y);
            v1 = ecl_make_long_float(d - y);
            break;
	}
#endif
	default:
            FEwrong_type_nth_arg(@[floor],1,x,@[real]);
	}
	ecl_return2(the_env, v0, v1);
    }

    cl_object
    ecl_floor2(cl_object x, cl_object y)
    {
	const cl_env_ptr the_env = ecl_process_env();
	cl_object v0, v1;
	cl_type ty;
        ty = clasp_t_of(y);
	if (ecl_unlikely(!ECL_REAL_TYPE_P(ty))) {
            FEwrong_type_nth_arg(@[floor],2,y,@[real]);
	}
	switch(clasp_t_of(x)) {
	case number_Fixnum:
            switch(ty) {
            case number_Fixnum: {	/* FIX / FIX */
                gctools::Fixnum a = ecl_fixnum(x), b = ecl_fixnum(y);
                gctools::Fixnum q = a / b,  r = a % b;
                if ((r^b) < 0 && r) {	/* opposite sign and some remainder*/
		    v0 = make_fixnum(q-1);
		    v1 = make_fixnum(r+b);
                } else {
		    v0 = make_fixnum(q);
		    v1 = make_fixnum(r);
                }
                break;
            }
            case number_Bignum: {	/* FIX / BIG */
                /* We must perform the division because there is the
                 * pathological case
                 *	x = MOST_NEGATIVE_FIXNUM
                 *    y = - MOST_NEGATIVE_FIXNUM
                 */
                ECL_WITH_TEMP_BIGNUM(bx,4);
                _ecl_big_senumber_Fixnum(bx, ecl_fixnum(x));
                v0 = _ecl_big_floor(bx, y, &v1);
                break;
            }
            case number_Ratio:		/* FIX / RAT */
                v0 = ecl_floor2(ecl_times(x, y->ratio.den), y->ratio.num);
                v1 = ecl_make_ratio(ecl_nth_value(the_env, 1), y->ratio.den);
                break;
            case number_SingleFloat: {	/* FIX / SF */
                float n = ecl_single_float(y);
                float p = ecl_fixnum(x) / n;
                float q = floorf(p);
                v0 = _ecl_float_to_integer(q);
                v1 = ecl_make_single_float((p - q)*n);
                break;
            }
            case number_DoubleFloat: {	/* FIX / DF */
                double n = ecl_double_float(y);
                double p = ecl_fixnum(x) / n;
                double q = floor(p);
                v0 = _ecl_double_to_integer(q);
                v1 = ecl_make_double_float((p - q)*n);
                break;
            }
#ifdef CLASP_LONG_FLOAT
            case number_LongFloat: {	/* FIX / LF */
                LongFloat n = ecl_long_float(y);
                LongFloat p = ecl_fixnum(x) / n;
                LongFloat q = floorl(p);
                v0 = _ecl_long_double_to_integer(q);
                v1 = ecl_make_long_float((p - q)*n);
                break;
            }
#endif
            default:
                (void)0; /* Never reached */
            }
            break;
	case number_Bignum:
            switch(ty) {
            case number_Fixnum: {	/* BIG / FIX */
                ECL_WITH_TEMP_BIGNUM(by,4);
                _ecl_big_senumber_Fixnum(by, ecl_fixnum(y));
                v0 = _ecl_big_floor(x, by, &v1);
                break;
            }
            case number_Bignum: {	/* BIG / BIG */
                v0 = _ecl_big_floor(x, y, &v1);
                break;
            }
            case number_Ratio:		/* BIG / RAT */
                v0 = ecl_floor2(ecl_times(x, y->ratio.den), y->ratio.num);
                v1 = ecl_make_ratio(ecl_nth_value(the_env, 1), y->ratio.den);
                break;
            case number_SingleFloat: {	/* BIG / SF */
                float n = ecl_single_float(y);
                float p = _ecl_big_to_double(x) / n;
                float q = floorf(p);
                v0 = _ecl_float_to_integer(q);
                v1 = ecl_make_single_float((p - q)*n);
                break;
            }
            case number_DoubleFloat: {	/* BIG / DF */
                double n = ecl_double_float(y);
                double p = _ecl_big_to_double(x) / n;
                double q = floor(p);
                v0 = _ecl_double_to_integer(q);
                v1 = ecl_make_double_float((p - q)*n);
                break;
            }
#ifdef CLASP_LONG_FLOAT
            case number_LongFloat: {	/* BIG / LF */
                LongFloat n = ecl_long_float(y);
                LongFloat p = _ecl_big_to_double(x) / n;
                LongFloat q = floorl(p);
                v0 = _ecl_long_double_to_integer(q);
                v1 = ecl_make_long_float((p - q)*n);
                break;
            }
#endif
            default:
                (void)0; /* Never reached */
            }
            break;
	case number_Ratio:
            switch(ty) {
            case number_Ratio:		/* RAT / RAT */
                v0 = ecl_floor2(ecl_times(x->ratio.num, y->ratio.den),
                                ecl_times(x->ratio.den, y->ratio.num));
                v1 = ecl_make_ratio(ecl_nth_value(the_env, 1), ecl_times(x->ratio.den, y->ratio.den));
                break;
            default:		/* RAT / ANY */
                v0 = ecl_floor2(x->ratio.num, ecl_times(x->ratio.den, y));
                v1 = ecl_divide(ecl_nth_value(the_env, 1), x->ratio.den);
            }
            break;
	case number_SingleFloat: {		/* SF / ANY */
            float n = ecl_to_double(y);
            float p = ecl_single_float(x)/n;
            float q = floorf(p);
            v0 = _ecl_float_to_integer(q);
            /* We cannot factor these two multiplications because
             * if we have signed zeros (1 - 1) * (-1) = -0 while
             * 1*(-1) - 1*(-1) = +0 */
            v1 = ecl_make_single_float(p*n - q*n);
            break;
	}
	case number_DoubleFloat: {		/* DF / ANY */
            double n = ecl_to_double(y);
            double p = ecl_double_float(x)/n;
            double q = floor(p);
            v0 = _ecl_double_to_integer(q);
            v1 = ecl_make_double_float(p*n - q*n);
            break;
	}
#ifdef CLASP_LONG_FLOAT
	case number_LongFloat: {		/* LF / ANY */
            LongFloat n = ecl_to_long_double(y);
            LongFloat p = ecl_long_float(x)/n;
            LongFloat q = floorl(p);
            v0 = _ecl_long_double_to_integer(q);
            v1 = ecl_make_long_float(p*n - q*n);
            break;
	}
#endif
	default:
            FEwrong_type_nth_arg(@[floor], 1, x, @[real]);
	}
	ecl_return2(the_env, v0, v1);
    }

    @(defun floor (x &optional (y OBJNULL))
      @
      if (narg == 1)
      return ecl_floor1(x);
      else
          return ecl_floor2(x, y);
      @)


    cl_object
    ecl_truncate1(cl_object x)
    {
	const cl_env_ptr the_env = ecl_process_env();
	cl_object v0, v1;
	switch (clasp_t_of(x)) {
	case number_Fixnum:
	case number_Bignum:
            v0 = x;
            v1 = make_fixnum(0);
            break;
	case number_Ratio:
            v0 = ecl_truncate2(x->ratio.num, x->ratio.den);
            v1 = ecl_make_ratio(ecl_nth_value(the_env, 1), x->ratio.den);
            break;
	case number_SingleFloat: {
            float d = ecl_single_float(x);
            float y = d > 0? floorf(d) : ceilf(d);
            v0 = _ecl_float_to_integer(y);
            v1 = ecl_make_single_float(d - y);
            break;
	}
	case number_DoubleFloat: {
            double d = ecl_double_float(x);
            double y = d > 0? floor(d) : ceil(d);
            v0 = _ecl_double_to_integer(y);
            v1 = ecl_make_double_float(d - y);
            break;
	}
#ifdef CLASP_LONG_FLOAT
	case number_LongFloat: {
            LongFloat d = ecl_long_float(x);
            LongFloat y = d > 0? floorl(d) : ceill(d);
            v0 = _ecl_long_double_to_integer(y);
            v1 = ecl_make_long_float(d - y);
            break;
	}
#endif
	default:
            FEwrong_type_nth_arg(@[truncate],1,x,@[real]);
	}
	ecl_return2(the_env, v0, v1);
    }

    cl_object
    ecl_truncate2(cl_object x, cl_object y)
    {
	if (ecl_plusp(x) != ecl_plusp(y))
            return ecl_ceiling2(x, y);
	else
            return ecl_floor2(x, y);
    }

    @(defun truncate (x &optional (y OBJNULL))
      @
      if (narg == 1)
      return ecl_truncate1(x);
      else
          return ecl_truncate2(x, y);
      @)

    static double
    round_double(double d)
    {
	if (d >= 0) {
            double q = floor(d += 0.5);
            if (q == d) {
                int i = (int)fmod(q, 10);
                if (i & 1) {
                    return q-1;
                }
            }
            return q;
	} else {
            return -round_double(-d);
	}
    }

#ifdef CLASP_LONG_FLOAT
    static LongFloat
    round_long_double(LongFloat d)
    {
	if (d >= 0) {
            LongFloat q = floorl(d += 0.5);
            if (q == d) {
                int i = (int)fmodl(q, 10);
                if (i & 1) {
                    return q-1;
                }
            }
            return q;
	} else {
            return -round_long_double(-d);
	}
    }
#endif

    cl_object
    ecl_round1(cl_object x)
    {
	const cl_env_ptr the_env = ecl_process_env();
	cl_object v0, v1;
	switch (clasp_t_of(x)) {
	case number_Fixnum:
	case number_Bignum:
            v0 = x;
            v1 = make_fixnum(0);
            break;
	case number_Ratio:
            v0 = ecl_round2(x->ratio.num, x->ratio.den);
            v1 = ecl_make_ratio(ecl_nth_value(the_env, 1), x->ratio.den);
            break;
	case number_SingleFloat: {
            float d = ecl_single_float(x);
            float q = round_double(d);
            v0 = _ecl_float_to_integer(q);
            v1 = ecl_make_single_float(d - q);
            break;
	}
	case number_DoubleFloat: {
            double d = ecl_double_float(x);
            double q = round_double(d);
            v0 = _ecl_double_to_integer(q);
            v1 = ecl_make_double_float(d - q);
            break;
	}
#ifdef CLASP_LONG_FLOAT
	case number_LongFloat: {
            LongFloat d = ecl_long_float(x);
            LongFloat q = round_long_double(d);
            v0 = _ecl_long_double_to_integer(q);
            v1 = ecl_make_long_float(d - q);
            break;
	}
#endif
	default:
            FEwrong_type_nth_arg(@[round],1,x,@[real]);
	}
	ecl_return2(the_env, v0, v1);
    }

    cl_object
    ecl_round2(cl_object x, cl_object y)
    {
	const cl_env_ptr the_env = ecl_process_env();
	cl_object v0, v1;
	cl_object q;

	q = ecl_divide(x, y);
	switch (clasp_t_of(q)) {
	case number_Fixnum:
	case number_Bignum:
            v0 = q;
            v1 = make_fixnum(0);
            break;
	case number_Ratio: {
            cl_object q1 = ecl_integer_divide(q->ratio.num, q->ratio.den);
            cl_object r = ecl_minus(q, q1);
            if (ecl_minusp(r)) {
                int c = ecl_number_compare(cl_core.minus_half, r);
                if (c > 0 || (c == 0 && ecl_oddp(q1))) {
                    q1 = ecl_one_minus(q1);
                }
            } else {
                int c = ecl_number_compare(r, cl_core.plus_half);
                if (c > 0 || (c == 0 && ecl_oddp(q1))) {
                    q1 = ecl_one_plus(q1);
                }
            }
            v0 = q1;
            v1 = number_remainder(x, y, q1);
            break;
	}
	default:
            v0 = q = ecl_round1(q);
            v1 = number_remainder(x, y, q);
	}
	ecl_return2(the_env, v0, v1);
    }

    @(defun round (x &optional (y OBJNULL))
      @
      if (narg == 1)
      return ecl_round1(x);
      else
          return ecl_round2(x, y);
      @)


    cl_object
    cl_mod(cl_object x, cl_object y)
    {
	const cl_env_ptr the_env = ecl_process_env();
	/* INV: #'floor always outputs two values */
	@floor(2, x, y);
	ecl_return1(the_env, the_env->values[1]);
    }

    cl_object
    cl_rem(cl_object x, cl_object y)
    {
	const cl_env_ptr the_env = ecl_process_env();
	@truncate(2, x, y);
	ecl_return1(the_env, the_env->values[1]);
    }

    cl_object
    cl_decode_float(cl_object x)
    {
	const cl_env_ptr the_env = ecl_process_env();
	int e, s;
	cl_type tx = clasp_t_of(x);
	float f;

	switch (tx) {
	case number_SingleFloat: {
            f = ecl_single_float(x);
            if (f >= 0.0) {
                s = 1;
            } else {
                f = -f;
                s = 0;
            }
            f = frexpf(f, &e);
            x = ecl_make_single_float(f);
            break;
	}
	case number_DoubleFloat: {
            double d = ecl_double_float(x);
            if (d >= 0.0) {
                s = 1;
            } else {
                d = -d;
                s = 0;
            }
            d = frexp(d, &e);
            x = ecl_make_double_float(d);
            break;
	}
#ifdef CLASP_LONG_FLOAT
	case number_LongFloat: {
            LongFloat d = ecl_long_float(x);
            if (d >= 0.0)
                s = 1;
            else {
                d = -d;
                s = 0;
            }
            d = frexpl(d, &e);
            x = ecl_make_long_float(d);
            break;
	}
#endif
	default:
            FEwrong_type_nth_arg(@[decode-float],1,x,@[float]);
	}
	ecl_return3(the_env, x, make_fixnum(e), ecl_make_single_float(s));
    }

    cl_object
    cl_scale_float(cl_object x, cl_object y)
    {
	const cl_env_ptr the_env = ecl_process_env();
	gctools::Fixnum k;

	if (ECL_FIXNUMP(y)) {
            k = ecl_fixnum(y);
	} else {
            FEwrong_type_nth_arg(@[scale-float],2,y,@[fixnum]);
	}
	switch (clasp_t_of(x)) {
	case number_SingleFloat:
            x = ecl_make_single_float(ldexpf(ecl_single_float(x), k));
            break;
	case number_DoubleFloat:
            x = ecl_make_double_float(ldexp(ecl_double_float(x), k));
            break;
#ifdef CLASP_LONG_FLOAT
	case number_LongFloat:
            x = ecl_make_long_float(ldexpl(ecl_long_float(x), k));
            break;
#endif
	default:
            FEwrong_type_nth_arg(@[scale-float],1,x,@[float]);
	}
	ecl_return1(the_env, x);
    }

    cl_object
    cl_float_radix(cl_object x)
    {
	const cl_env_ptr the_env = ecl_process_env();
	if (ecl_unlikely(cl_floatp(x) != ECL_T)) {
            FEwrong_type_nth_arg(@[float-radix],1,x,@[float]);
	}
	ecl_return1(the_env, make_fixnum(FLT_RADIX));
    }

    int
    ecl_signbit(cl_object x)
    {
        switch (clasp_t_of(x)) {
	case number_SingleFloat:
            return signbit(ecl_single_float(x));
	case number_DoubleFloat:
            return signbit(ecl_double_float(x));
#ifdef CLASP_LONG_FLOAT
	case number_LongFloat:
            return signbit(ecl_long_float(x));
#endif
	default:
            FEwrong_type_nth_arg(@[float-sign],1,x,@[float]);
	}
    }

    @(defun float_sign (x &optional (y x yp))
      int negativep;
      @
      if (!yp) {
          y = cl_float(2, make_fixnum(1), x);
      }
      negativep = ecl_signbit(x);
      switch (clasp_t_of(y)) {
      case number_SingleFloat: {
          float f = ecl_single_float(y);
          if (signbit(f) != negativep) y = ecl_make_single_float(-f);
          break;
      }
      case number_DoubleFloat: {
          double f = ecl_double_float(y);
          if (signbit(f) != negativep) y = ecl_make_double_float(-f);
          break;
      }
#ifdef CLASP_LONG_FLOAT
      case number_LongFloat: {
          LongFloat f = ecl_long_float(y);
          if (signbit(f) != negativep) y = ecl_make_long_float(-f);
          break;
      }
#endif
      default:
          FEwrong_type_nth_arg(@[float-sign],2,y,@[float]);
      }
      @(return y);
      @)

    cl_object
    cl_float_digits(cl_object x)
    {
	const cl_env_ptr the_env = ecl_process_env();
	switch (clasp_t_of(x)) {
	case number_SingleFloat:
            x = make_fixnum(FLT_MANT_DIG);
            break;
	case number_DoubleFloat:
            x = make_fixnum(DBL_MANT_DIG);
            break;
#ifdef CLASP_LONG_FLOAT
	case number_LongFloat:
            x = make_fixnum(LDBL_MANT_DIG);
            break;
#endif
	default:
            FEwrong_type_nth_arg(@[float-digits],1,x,@[float]);
	}
	ecl_return1(the_env, x);
    }

    cl_object
    cl_float_precision(cl_object x)
    {
	const cl_env_ptr the_env = ecl_process_env();
	int precision;
	switch (clasp_t_of(x)) {
	case number_SingleFloat: {
            float f = ecl_single_float(x);
            if (f == 0.0) {
                precision = 0;
            } else {
                int exp;
                frexpf(f, &exp);
                if (exp >= FLT_MIN_EXP) {
                    precision = FLT_MANT_DIG;
                } else {
                    precision = FLT_MANT_DIG - (FLT_MIN_EXP - exp);
                }
            }
            break;
	}
	case number_DoubleFloat: {
            double f = ecl_double_float(x);
            if (f == 0.0) {
                precision = 0;
            } else {
                int exp;
                frexp(f, &exp);
                if (exp >= DBL_MIN_EXP) {
                    precision = DBL_MANT_DIG;
                } else {
                    precision = DBL_MANT_DIG - (DBL_MIN_EXP - exp);
                }
            }
            break;
	}
#ifdef CLASP_LONG_FLOAT
	case number_LongFloat: {
            LongFloat f = ecl_long_float(x);
            if (f == 0.0) {
                precision = 0;
            } else {
                int exp;
                frexp(f, &exp);
                if (exp >= LDBL_MIN_EXP) {
                    precision = LDBL_MANT_DIG;
                } else {
                    precision = LDBL_MANT_DIG - (LDBL_MIN_EXP - exp);
                }
            }
            break;
	}
#endif
	default:
            FEwrong_type_nth_arg(@[float-precision],1,x,@[float]);
	}
	ecl_return1(the_env, make_fixnum(precision));
    }



    @(defun complex (r &optional (i make_fixnum(0)))
      @	/* INV: ecl_make_complex() checks types */
      @(return ecl_make_complex(r, i))
      @)

    cl_object
    cl_realpart(cl_object x)
    {
        switch (clasp_t_of(x)) {
	case number_Fixnum:
	case number_Bignum:
	case number_Ratio:
	case number_SingleFloat:
	case number_DoubleFloat:
#ifdef CLASP_LONG_FLOAT
	case number_LongFloat:
#endif
            break;
	case number_Complex:
            x = x->complex.real;
            break;
	default:
            FEwrong_type_nth_arg(@[realpart],1,x,@[number]);
	}
	@(return x)
            }

    cl_object
    cl_imagpart(cl_object x)
    {
        switch (clasp_t_of(x)) {
	case number_Fixnum:
	case number_Bignum:
	case number_Ratio:
            x = make_fixnum(0);
            break;
	case number_SingleFloat:
            if (signbit(ecl_single_float(x)))
                x = cl_core.singlefloat_minus_zero;
            else
                x = cl_core.singlefloat_zero;
            break;
	case number_DoubleFloat:
            if (signbit(ecl_double_float(x)))
                x = cl_core.doublefloat_minus_zero;
            else
                x = cl_core.doublefloat_zero;
            break;
#ifdef CLASP_LONG_FLOAT
	case number_LongFloat:
            if (signbit(ecl_long_float(x)))
                x = cl_core.longfloat_minus_zero;
            else
                x = cl_core.longfloat_zero;
            break;
#endif
	case number_Complex:
            x = x->complex.imag;
            break;
	default:
            FEwrong_type_nth_arg(@[imagpart],1,x,@[number]);
	}
	@(return x)
            }

#endif

#if 0
T_sp
cl_integer_decode_float(T_sp x)
{
  const cl_env_ptr the_env = ecl_process_env();
  int e, s = 1;

  switch (clasp_t_of(x)) {
#ifdef CLASP_LONG_FLOAT
  case number_LongFloat: {
    LongFloat d = ecl_long_float(x);
    if (signbit(d)) {
      s = -1;
      d = -d;
    }
    if (d == 0.0) {
      e = 0;
      x = make_fixnum(0);
    } else {
      d = frexpl(d, &e);
      x = _ecl_long_double_to_integer(ldexpl(d, LDBL_MANT_DIG));
      e -= LDBL_MANT_DIG;
    }
    break;
  }
#endif
  case number_DoubleFloat: {
    double d = clasp_to_double_float(x);
    if (signbit(d)) {
      s = -1;
      d = -d;
    }
    if (d == 0.0) {
      e = 0;
      x = make_fixnum(0);
    } else {
      d = frexp(d, &e);
      x = _clasp_double_to_integer(ldexp(d, DBL_MANT_DIG));
      e -= DBL_MANT_DIG;
    }
    break;
  }
  case number_SingleFloat: {
    float d = clasp_to_single_float(x);
    if (signbit(d)) {
      s = -1;
      d = -d;
    }
    if (d == 0.0) {
      e = 0;
      x = make_fixnum(0);
    } else {
      d = frexpf(d, &e);
      x = _ecl_double_to_integer(ldexp(d, FLT_MANT_DIG));
      e -= FLT_MANT_DIG;
    }
    break;
  }
  default:
      ERROR_WRONG_TYPE_NTH_ARG(FEwrong_type_nth_arg(@[integer-decode-float],1,x,@[float]);
  }
  ecl_return3(the_env, x, make_fixnum(e), make_fixnum(s));
}
#endif

#define ARGS_core_asin "(arg)"
#define DECL_core_asin ""
#define DOCS_core_asin "asinh"
double core_asin(double x) {
  _G();
  return asin(x);
}

#define ARGS_core_acos "(arg)"
#define DECL_core_acos ""
#define DOCS_core_acos "acosh"
double core_acos(double x) {
  _G();
  return acos(x);
}

#define ARGS_core_asinh "(arg)"
#define DECL_core_asinh ""
#define DOCS_core_asinh "asinh"
double core_asinh(double x) {
  _G();
  return log(x + sqrt(1.0 + x * x));
}

#define ARGS_core_acosh "(arg)"
#define DECL_core_acosh ""
#define DOCS_core_acosh "acosh"
double core_acosh(double x) {
  _G();
  return log(x + sqrt((x - 1) * (x + 1)));
}

#define ARGS_core_atanh "(arg)"
#define DECL_core_atanh ""
#define DOCS_core_atanh "atanh"
double core_atanh(double x) {
  _G();
  return log((1 + x) / (1 - x)) / 2;
}
};

namespace core {

void exposeCando_Numerics() {
  _G();
  LOG(BF("Initializing numerics random"));
  af_def(CorePkg, "seedRandomNumberGenerators", &seedRandomNumberGenerators);
  af_def(CorePkg, "seedRandomNumberGeneratorsUsingTime", &seedRandomNumberGeneratorsUsingTime);
  af_def(CorePkg, "randomNumber01", &randomNumber01);
  af_def(CorePkg, "randomNumberNormal01", &randomNumberNormal01);
  SYMBOL_EXPORT_SC_(ClPkg, getUniversalTime);
  Defun(getUniversalTime);
  CoreDefun(asin);
  CoreDefun(acos);
  CoreDefun(asinh);
  CoreDefun(acosh);
  CoreDefun(atanh);

  SYMBOL_EXPORT_SC_(ClPkg, leastPositiveSingleFloat);
  SYMBOL_EXPORT_SC_(ClPkg, leastNegativeSingleFloat);
  SYMBOL_EXPORT_SC_(ClPkg, mostPositiveSingleFloat);
  SYMBOL_EXPORT_SC_(ClPkg, mostNegativeSingleFloat);
  cl::_sym_mostPositiveSingleFloat->defconstant(clasp_make_single_float(FLT_MAX));
  cl::_sym_mostNegativeSingleFloat->defconstant(clasp_make_single_float(-FLT_MAX));
  cl::_sym_leastPositiveSingleFloat->defconstant(clasp_make_single_float(FLT_MIN));
  cl::_sym_leastNegativeSingleFloat->defconstant(clasp_make_single_float(-FLT_MIN));

  SYMBOL_EXPORT_SC_(ClPkg, leastPositiveShortFloat);
  SYMBOL_EXPORT_SC_(ClPkg, leastNegativeShortFloat);
  SYMBOL_EXPORT_SC_(ClPkg, mostPositiveShortFloat);
  SYMBOL_EXPORT_SC_(ClPkg, mostNegativeShortFloat);
  cl::_sym_mostPositiveShortFloat->defconstant(ShortFloat_O::create(FLT_MAX));
  cl::_sym_mostNegativeShortFloat->defconstant(ShortFloat_O::create(-FLT_MAX));
  cl::_sym_leastPositiveShortFloat->defconstant(ShortFloat_O::create(FLT_MIN));
  cl::_sym_leastNegativeShortFloat->defconstant(ShortFloat_O::create(-FLT_MIN));

  SYMBOL_EXPORT_SC_(ClPkg, leastPositiveDoubleFloat);
  SYMBOL_EXPORT_SC_(ClPkg, leastNegativeDoubleFloat);
  SYMBOL_EXPORT_SC_(ClPkg, mostPositiveDoubleFloat);
  SYMBOL_EXPORT_SC_(ClPkg, mostNegativeDoubleFloat);
  cl::_sym_mostPositiveDoubleFloat->defconstant(DoubleFloat_O::create(DBL_MAX));
  cl::_sym_mostNegativeDoubleFloat->defconstant(DoubleFloat_O::create(-DBL_MAX));
  cl::_sym_leastPositiveDoubleFloat->defconstant(DoubleFloat_O::create(DBL_MIN));
  cl::_sym_leastNegativeDoubleFloat->defconstant(DoubleFloat_O::create(-DBL_MIN));

  SYMBOL_EXPORT_SC_(ClPkg, leastPositiveLongFloat);
  SYMBOL_EXPORT_SC_(ClPkg, leastNegativeLongFloat);
  SYMBOL_EXPORT_SC_(ClPkg, mostPositiveLongFloat);
  SYMBOL_EXPORT_SC_(ClPkg, mostNegativeLongFloat);
  cl::_sym_mostPositiveLongFloat->defconstant(DoubleFloat_O::create(DBL_MAX));
  cl::_sym_mostNegativeLongFloat->defconstant(DoubleFloat_O::create(-DBL_MAX));
  cl::_sym_leastPositiveLongFloat->defconstant(DoubleFloat_O::create(DBL_MIN));
  cl::_sym_leastNegativeLongFloat->defconstant(DoubleFloat_O::create(-DBL_MIN));

  SYMBOL_EXPORT_SC_(ClPkg, leastNegativeNormalizedSingleFloat);
  SYMBOL_EXPORT_SC_(ClPkg, leastNegativeNormalizedShortFloat);
  SYMBOL_EXPORT_SC_(ClPkg, leastNegativeNormalizedDoubleFloat);
  SYMBOL_EXPORT_SC_(ClPkg, leastNegativeNormalizedLongFloat);
  cl::_sym_leastNegativeNormalizedSingleFloat->defconstant(clasp_make_single_float(-std::numeric_limits<float>::denorm_min()));
  cl::_sym_leastNegativeNormalizedShortFloat->defconstant(ShortFloat_O::create(-std::numeric_limits<float>::denorm_min()));
  cl::_sym_leastNegativeNormalizedDoubleFloat->defconstant(DoubleFloat_O::create(-std::numeric_limits<double>::denorm_min()));
  cl::_sym_leastNegativeNormalizedLongFloat->defconstant(LongFloat_O::create(-std::numeric_limits<LongFloat>::denorm_min()));

  SYMBOL_EXPORT_SC_(ClPkg, leastPositiveNormalizedSingleFloat);
  SYMBOL_EXPORT_SC_(ClPkg, leastPositiveNormalizedShortFloat);
  SYMBOL_EXPORT_SC_(ClPkg, leastPositiveNormalizedDoubleFloat);
  SYMBOL_EXPORT_SC_(ClPkg, leastPositiveNormalizedLongFloat);
  cl::_sym_leastPositiveNormalizedSingleFloat->defconstant(clasp_make_single_float(-std::numeric_limits<float>::denorm_min()));
  cl::_sym_leastPositiveNormalizedShortFloat->defconstant(ShortFloat_O::create(-std::numeric_limits<float>::denorm_min()));
  cl::_sym_leastPositiveNormalizedDoubleFloat->defconstant(DoubleFloat_O::create(-std::numeric_limits<double>::denorm_min()));
  cl::_sym_leastPositiveNormalizedLongFloat->defconstant(LongFloat_O::create(-std::numeric_limits<LongFloat>::denorm_min()));

  SYMBOL_EXPORT_SC_(ClPkg, pi);
  cl::_sym_pi->defconstant(DoubleFloat_O::create(3.14159265358979323846264338));
}

#ifdef USEBOOSTPYTHON

void exposePython_Numerics() {
  boost::python::def("mixedBaseDigitsToBignum", &mixedBaseDigitsToBignum);
  boost::python::def("bignumToMixedBaseDigits", &bignumToMixedBaseDigits);
  boost::python::def("numberOfIndicesForMixedBase", &numberOfIndicesForMixedBase);
  boost::python::def("seedRandomNumberGenerators", &seedRandomNumberGenerators);
  boost::python::def("seedRandomNumberGeneratorsUsingTime", &seedRandomNumberGeneratorsUsingTime);
  boost::python::def("randomNumber01", &randomNumber01);
  boost::python::def("randomNumberNormal01", &randomNumberNormal01);
  boost::python::def("almostEqualAbsoluteOrRelative", &almostEqualAbsoluteOrRelative);
}

#endif
};
