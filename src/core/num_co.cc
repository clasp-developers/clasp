/*
    File: num_co.cc
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
/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    num_co.c -- Operations on floating-point numbers.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2013, Christian E. Schafmeister


    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

/*
        IMPLEMENTATION-DEPENDENT

        This file contains those functions
        that know the representation of floating-point numbers.
*/

#include <float.h>

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/numbers.h>
#include <clasp/core/num_arith.h>
#include <clasp/core/bignum.h>
#include <clasp/core/wrappers.h>
#include <clasp/core/mathDispatch.h>

#ifndef HAVE_ISOC99
#define floorf floor
#define ceilf ceil
#define fabsf fabs
#define frexpf frexp
#define ldexpf ldexp
#define cosf cos
#define coshf cosh
#define expf exp
#define logf log
#define sinf sin
#define sqrtf sqrt
#define tanf tan
#define tanhf tanh
#endif

namespace core {

/* Coerce X to single-float if one arg,
   otherwise coerce to same float type as second arg */

CL_LAMBDA(x &optional y);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(float)dx");
DOCGROUP(clasp);
CL_DEFUN Float_sp cl__float(Real_sp x, T_sp y) {
  NumberType ty, tx;
  if (y.notnilp()) {
    ty = clasp_t_of(gc::As<Float_sp>(y));
  } else {
    ty = NumberType::SingleFloat;
  }
  switch (tx = clasp_t_of(x)) {
  case NumberType::SingleFloat:
  case NumberType::DoubleFloat:
#ifdef CLASP_LONG_FLOAT
  case NumberType::LongFloat:
#endif
    if (y.nilp() || ty == tx)
      return gc::As_unsafe<Float_sp>(x);
    // otherwise, fall through
  case NumberType::Fixnum:
  case NumberType::Bignum:
  case NumberType::Ratio:
    switch (ty) {
    case NumberType::SingleFloat:
      return clasp_make_single_float(clasp_to_float(x));
    case NumberType::DoubleFloat:
      return clasp_make_double_float(clasp_to_double(x));
#ifdef CLASP_LONG_FLOAT
    case NumberType::LongFloat:
      return clasp_make_long_float(clasp_to_long_float(x)).as<Real_O>();
#endif
    default:
      ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_float, 2, y, cl::_sym_float);
    }
  default:
    ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_float, 1, x, cl::_sym_Real_O);
  }
}

// Simpler versions used by the compiler.
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN SingleFloat_sp core__to_single_float(Real_sp x) {
  if (x.single_floatp())
    return gc::As_unsafe<SingleFloat_sp>(x);
  else
    return clasp_make_single_float(clasp_to_double(x));
}

CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN DoubleFloat_sp core__to_double_float(Real_sp x) {
  if (gc::IsA<DoubleFloat_sp>(x))
    return gc::As_unsafe<DoubleFloat_sp>(x);
  else
    return clasp_make_double_float(clasp_to_double(x));
}

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(numerator)dx");
DOCGROUP(clasp);
CL_DEFUN Integer_sp cl__numerator(Rational_sp x) {
  switch (clasp_t_of(x)) {
  case NumberType::Ratio:
    return gc::As_unsafe<Ratio_sp>(x)->numerator();
  case NumberType::Fixnum:
  case NumberType::Bignum:
    return gc::As_unsafe<Integer_sp>(x);
  default:
    ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_numerator, 1, x, cl::_sym_Rational_O);
  }
}

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(denominator)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp cl__denominator(Rational_sp x) {
  switch (clasp_t_of(x)) {
  case NumberType::Ratio:
    return gc::As_unsafe<Ratio_sp>(x)->denominator();
  case NumberType::Fixnum:
  case NumberType::Bignum:
    return clasp_make_fixnum(1);
  default:
    ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_denominator, 1, x, cl::_sym_Rational_O);
  }
}

// Stores the result in quotient, remainder.
static void clasp_truncate(Real_sp dividend, Real_sp divisor, Integer_sp& quotient, Real_sp& remainder) {
  // The CL standard is a bit ambiguous about the type of the remainder.
  // We treat it as a contagion thing: If either argument is a float, the
  // remainder is a float of the largest format among the arguments.
  MATH_DISPATCH_BEGIN(dividend, divisor) {
  case_Fixnum_v_Fixnum : {
    Fixnum a = dividend.unsafe_fixnum();
    Fixnum b = divisor.unsafe_fixnum();
    // Uniquely, (truncate most-negative-fixnum -1) is a bignum, so
    // we can't just use clasp_make_fixnum for the quotient.
    quotient = Integer_O::create(a / b);
    remainder = clasp_make_fixnum(a % b);
    return;
  }
  case_Fixnum_v_Bignum : {
    // This is always a zero quotient, except when
    // we have MOST_NEGATIVE_FIXNUM / - MOST_NEGATIVE_FIXNUM.
    Fixnum a = dividend.unsafe_fixnum();
    Bignum_sp b = gc::As_unsafe<Bignum_sp>(divisor);
    if ((a == gc::most_negative_fixnum) && (b->length() == 1) && ((b->limbs())[0] == -gc::most_negative_fixnum)) {
      quotient = clasp_make_fixnum(-1);
      remainder = clasp_make_fixnum(0);
    } else {
      quotient = clasp_make_fixnum(0);
      remainder = dividend;
    }
    return;
  }
  case_Fixnum_v_Ratio:
  case_Bignum_v_Ratio : {
    Ratio_sp ry = gc::As<Ratio_sp>(divisor);
    Real_sp subr;
    Number_sp product = clasp_times(dividend, ry->denominator());
    clasp_truncate(gc::As_unsafe<Real_sp>(product), ry->numerator(), quotient, subr);
    remainder = Rational_O::create(gc::As_unsafe<Integer_sp>(subr), ry->denominator());
    return;
  }
  case_Fixnum_v_SingleFloat : {
    float n = divisor.unsafe_single_float();
    float p = dividend.unsafe_fixnum() / n;
    float q = std::trunc(p);
    quotient = _clasp_float_to_integer(q);
    remainder = clasp_make_single_float(p * n - q * n);
    return;
  }
  case_Fixnum_v_DoubleFloat : {
    double n = gc::As_unsafe<DoubleFloat_sp>(divisor)->get();
    double p = dividend.unsafe_fixnum() / n;
    double q = std::trunc(p);
    quotient = _clasp_double_to_integer(q);
    remainder = clasp_make_double_float(p * n - q * n);
    return;
  }
#ifdef CLASP_LONG_FLOAT
  case_Fixnum_v_LongFloat : {
    LongFloat n = clasp_long_float(divisor);
    LongFloat p = dividend.unsafe_fixnum() / n;
    LongFloat q = std::trunc(p);
    quotient = _clasp_long_double_to_integer(q);
    remainder = clasp_make_long_float(p * n - q * n);
    return;
  }
#endif
  case_Bignum_v_Fixnum : {
    Bignum_sp bdividend = gc::As_unsafe<Bignum_sp>(dividend);
    Fixnum fdivisor = divisor.unsafe_fixnum();
    T_mv rmv = core__next_ftruncate(bdividend, fdivisor);
    quotient = gc::As_unsafe<Integer_sp>(rmv);
    MultipleValues& mvn = core::lisp_multipleValues();
    remainder = gc::As_unsafe<Integer_sp>(mvn.valueGet(1, rmv.number_of_values()));
    return;
  }
  case_Bignum_v_Bignum : {
    Bignum_sp bdividend = gc::As_unsafe<Bignum_sp>(dividend);
    Bignum_sp bdivisor = gc::As_unsafe<Bignum_sp>(divisor);
    T_mv mvr = core__next_truncate(bdividend, bdivisor);
    quotient = gc::As_unsafe<Integer_sp>(mvr);
    MultipleValues& mvn = core::lisp_multipleValues();
    remainder = gc::As_unsafe<Integer_sp>(mvn.valueGet(1, mvr.number_of_values()));
    return;
  }
  // case_Bignum_v_Ratio: above
  case_Bignum_v_SingleFloat : {
    float n = divisor.unsafe_single_float();
    float p = gc::As_unsafe<Bignum_sp>(dividend)->as_float_() / n;
    float q = std::trunc(p);
    quotient = _clasp_float_to_integer(q);
    remainder = clasp_make_single_float(p * n - q * n);
    return;
  }
  case_Bignum_v_DoubleFloat : {
    double n = gc::As_unsafe<DoubleFloat_sp>(divisor)->get();
    double p = gc::As_unsafe<Bignum_sp>(dividend)->as_double_() / n;
    double q = std::trunc(p);
    quotient = _clasp_double_to_integer(q);
    remainder = clasp_make_double_float(p * n - q * n);
    return;
  }
#ifdef CLASP_LONG_FLOAT
  case_Bignum_v_LongFloat : {
    LongFloat n = clasp_long_float(divisor);
    LongFloat p = gc::As_unsafe<Bignum_sp>(dividend)->as_long_float_() / n;
    LongFloat q = std::trunc(p);
    quotient = _clasp_long_double_to_integer(q);
    remainder = clasp_make_long_float(p * n - q * n);
    return;
  }
#endif
  case_Ratio_v_Ratio : {
    Ratio_sp rx = gc::As_unsafe<Ratio_sp>(dividend);
    Ratio_sp ry = gc::As_unsafe<Ratio_sp>(divisor);
    Real_sp subr;
    Real_sp c1 = gc::As_unsafe<Real_sp>(clasp_times(rx->numerator(), ry->denominator()));
    Real_sp c2 = gc::As_unsafe<Real_sp>(clasp_times(ry->numerator(), rx->denominator()));
    Real_sp nd = gc::As_unsafe<Real_sp>(clasp_times(rx->denominator(), ry->denominator()));
    clasp_truncate(c1, c2, quotient, subr);
    remainder = Rational_O::create(gc::As_unsafe<Integer_sp>(subr), gc::As_unsafe<Integer_sp>(nd));
    return;
  }
  case_Ratio_v_Fixnum:
  case_Ratio_v_Bignum:
#ifdef CLASP_LONG_FLOAT
  case_Ratio_v_LongFloat:
#endif
  case_Ratio_v_SingleFloat:
  case_Ratio_v_DoubleFloat : {
    // Given (truncate x (* y z)) = q, r,
    // (truncate x/y z) = q, r/y.
    Ratio_sp rx = gc::As_unsafe<Ratio_sp>(dividend);
    Integer_sp den = rx->denominator();
    Real_sp ndiv = gc::As_unsafe<Real_sp>(clasp_times(den, divisor));
    Real_sp subr;
    clasp_truncate(rx->numerator(), ndiv, quotient, subr);
    remainder = gc::As_unsafe<Real_sp>(clasp_divide(subr, den));
    return;
  }
  case_SingleFloat_v_Fixnum:
  case_SingleFloat_v_Bignum:
  case_SingleFloat_v_SingleFloat:
  case_SingleFloat_v_Ratio : {
    float n = clasp_to_float(divisor);
    float p = dividend.unsafe_single_float() / n;
    float q = std::trunc(p);
    quotient = _clasp_float_to_integer(q);
    remainder = clasp_make_single_float(p * n - q * n);
    return;
  }
  case_DoubleFloat_v_Fixnum:
  case_DoubleFloat_v_Bignum:
  case_SingleFloat_v_DoubleFloat:
  case_DoubleFloat_v_SingleFloat:
  case_DoubleFloat_v_DoubleFloat:
#ifdef CLASP_LONG_FLOAT
  case_DoubleFloat_v_LongFloat:
  case_SingleFloat_v_LongFloat:
#endif
  case_DoubleFloat_v_Ratio : {
    double n = clasp_to_double(divisor);
    double p = clasp_to_double(dividend) / n;
    double q = std::trunc(p);
    quotient = _clasp_double_to_integer(q);
    remainder = clasp_make_double_float(p * n - q * n);
    return;
  }
#ifdef CLASP_LONG_FLOAT
  case_LongFloat_v_Fixnum:
  case_LongFloat_v_Bignum:
  case_LongFloat_v_SingleFloat:
  case_LongFloat_v_DoubleFloat:
  case_LongFloat_v_LongFloat : {
    LongFloat n = clasp_to_long_double(divisor);
    LongFloat p = clasp_long_float(dividend) / n;
    LongFloat q = std::trunc(p);
    quotient = _clasp_long_double_to_integer(q);
    remainder = clasp_make_long_float(p * n - q * n);
    return;
  }
#endif
  default:
    UNREACHABLE();
  };
  MATH_DISPATCH_END();
}

static void clasp_floor(Real_sp dividend, Real_sp divisor, Integer_sp& quotient, Real_sp& remainder) {
  Integer_sp t0;
  Real_sp t1;
  clasp_truncate(dividend, divisor, t0, t1);
  if (!(clasp_zerop(t1)) && (clasp_minusp(divisor) ? clasp_plusp(dividend) : clasp_minusp(dividend))) {
    quotient = gc::As_unsafe<Integer_sp>(clasp_one_minus(t0));
    remainder = gc::As_unsafe<Real_sp>(clasp_plus(t1, divisor));
  } else {
    quotient = t0;
    remainder = t1;
  }
}

Real_mv clasp_floor1(Real_sp x) {
  switch (clasp_t_of(x)) {
  case NumberType::Fixnum:
  case NumberType::Bignum:
    return Values(x, clasp_make_fixnum(0));
  case NumberType::Ratio: {
    Ratio_sp rx(gc::As_unsafe<Ratio_sp>(x));
    Integer_sp v0;
    Real_sp tv1;
    clasp_floor(rx->numerator(), rx->denominator(), v0, tv1);
    return Values(v0, Ratio_O::create(gc::As_unsafe<Integer_sp>(tv1), rx->denominator()));
  }
  case NumberType::SingleFloat: {
    float d = x.unsafe_single_float();
    if (std::isnan(d))
      return Values(x, clasp_make_fixnum(0));
    else {
      float y = floorf(d);
      return Values(_clasp_float_to_integer(y), clasp_make_single_float(d - y));
    }
  }
  case NumberType::DoubleFloat: {
    double d = gc::As<DoubleFloat_sp>(x)->get();
    if (std::isnan(d))
      return Values(x, clasp_make_fixnum(0));
    else {
      double y = floor(d);
      return Values(_clasp_double_to_integer(y), clasp_make_double_float(d - y));
    }
  }
#ifdef CLASP_LONG_FLOAT
  case NumberType::LongFloat: {
    LongFloat d = clasp_long_float(x);
    if (std::isnan(d))
      return Values(x, clasp_make_fixnum(0));
    else {
      LongFloat y = floorl(d);
      return Values(_clasp_long_double_to_integer(y), v1 = clasp_make_long_float(d - y));
    }
  }
#endif
  default:
    ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_floor, 1, x, cl::_sym_Real_O);
  }
}

Real_mv clasp_floor2(Real_sp dividend, Real_sp divisor) {
  Integer_sp v0;
  Real_sp v1;
  clasp_floor(dividend, divisor, v0, v1);
  return Values(v0, v1);
}

CL_LAMBDA(x &optional y);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(floor)dx");
DOCGROUP(clasp);
CL_DEFUN Real_mv cl__floor(Real_sp x, T_sp y) {
  if (y.nilp())
    return clasp_floor1(x);
  else
    return clasp_floor2(x, gc::As<Real_sp>(y));
}

static void clasp_ceiling(Real_sp dividend, Real_sp divisor, Integer_sp& quotient, Real_sp& remainder) {
  Integer_sp t0;
  Real_sp t1;
  clasp_truncate(dividend, divisor, t0, t1);
  if (!(clasp_zerop(t1)) && (clasp_minusp(divisor) ? clasp_minusp(dividend) : clasp_plusp(dividend))) {
    quotient = gc::As_unsafe<Integer_sp>(clasp_one_plus(t0));
    remainder = gc::As_unsafe<Real_sp>(clasp_minus(t1, divisor));
  } else {
    quotient = t0;
    remainder = t1;
  }
}

Real_mv clasp_ceiling1(Real_sp x) {
  switch (clasp_t_of(x)) {
  case NumberType::Fixnum:
  case NumberType::Bignum:
    return Values(x, clasp_make_fixnum(0));
  case NumberType::Ratio: {
    Integer_sp t0;
    Real_sp t1;
    Ratio_sp rx = gc::As_unsafe<Ratio_sp>(x);
    clasp_ceiling(rx->numerator(), rx->denominator(), t0, t1);
    return Values(t0, Ratio_O::create(gc::As_unsafe<Integer_sp>(t1), rx->denominator()));
  }
  case NumberType::SingleFloat: {
    float d = x.unsafe_single_float();
    float y = ceilf(d);
    return Values(_clasp_float_to_integer(y), clasp_make_single_float(d - y));
  }
  case NumberType::DoubleFloat: {
    double d = gc::As_unsafe<DoubleFloat_sp>(x)->get();
    double y = ceil(d);
    return Values(_clasp_double_to_integer(y), clasp_make_double_float(d - y));
  }
#ifdef CLASP_LONG_FLOAT
  case NumberType::LongFloat: {
    LongFloat d = clasp_long_float(x);
    LongFloat y = ceill(d);
    return Values(_clasp_long_double_to_integer(y), clasp_make_long_float(d - y));
  }
#endif
  default:
    ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_ceiling, 1, x, cl::_sym_Real_O);
  }
}

Real_mv clasp_ceiling2(Real_sp dividend, Real_sp divisor) {
  Integer_sp v0;
  Real_sp v1;
  clasp_ceiling(dividend, divisor, v0, v1);
  return Values(v0, v1);
}

CL_LAMBDA(dividend &optional divisor);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(ceiling)dx");
DOCGROUP(clasp);
CL_DEFUN Real_mv cl__ceiling(Real_sp dividend, T_sp divisor) {
  if (divisor.nilp())
    return clasp_ceiling1(dividend);
  else
    return clasp_ceiling2(dividend, gc::As<Real_sp>(divisor));
}

Real_mv clasp_truncate1(Real_sp x) {
  switch (clasp_t_of(x)) {
  case NumberType::Fixnum:
  case NumberType::Bignum:
    return Values(x, clasp_make_fixnum(0));
  case NumberType::Ratio: {
    Ratio_sp rx = gc::As<Ratio_sp>(x);
    Integer_sp v0;
    Real_sp v1;
    clasp_truncate(rx->numerator(), rx->denominator(), v0, v1);
    return Values(v0, Ratio_O::create(gc::As_unsafe<Integer_sp>(v1), rx->denominator()));
  }
  case NumberType::SingleFloat: {
    float d = x.unsafe_single_float();
    float y = d > 0 ? floorf(d) : ceilf(d);
    return Values(_clasp_float_to_integer(y), clasp_make_single_float(d - y));
  }
  case NumberType::DoubleFloat: {
    double d = gc::As_unsafe<DoubleFloat_sp>(x)->get();
    double y = d > 0 ? floor(d) : ceil(d);
    return Values(_clasp_double_to_integer(y), clasp_make_double_float(d - y));
  }
#ifdef CLASP_LONG_FLOAT
  case NumberType::LongFloat: {
    LongFloat d = clasp_long_float(x);
    LongFloat y = d > 0 ? floorl(d) : ceill(d);
    return Values(_clasp_long_double_to_integer(y), clasp_make_long_float(d - y));
  }
#endif
  default:
    ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_truncate, 1, x, cl::_sym_Real_O);
  }
}

Real_mv clasp_truncate2(Real_sp x, Real_sp y) {
  Integer_sp v0;
  Real_sp v1;
  clasp_truncate(x, y, v0, v1);
  return Values(v0, v1);
}

CL_LAMBDA(dividend &optional divisor);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(truncate)dx");
DOCGROUP(clasp);
CL_DEFUN Real_mv cl__truncate(Real_sp dividend, T_sp divisor) {
  if (divisor.nilp())
    return clasp_truncate1(dividend);
  else
    return clasp_truncate2(dividend, gc::As<Real_sp>(divisor));
}

static double round_double(double d) {
  if (d >= 0) {
    double q = floor(d += 0.5);
    if (q == d) {
      int i = (int)fmod(q, 10);
      if (i & 1) {
        return q - 1;
      }
    }
    return q;
  } else {
    return -round_double(-d);
  }
}

#ifdef CLASP_LONG_FLOAT
static LongFloat round_long_double(LongFloat d) {
  if (d >= 0) {
    LongFloat q = floorl(d += 0.5);
    if (q == d) {
      int i = (int)fmodl(q, 10);
      if (i & 1) {
        return q - 1;
      }
    }
    return q;
  } else {
    return -round_long_double(-d);
  }
}
#endif

static void clasp_round(Real_sp dividend, Real_sp divisor, Integer_sp& quotient, Real_sp& remainder) {
  Integer_sp tru;
  Real_sp rem;
  clasp_truncate(dividend, divisor, tru, rem);

  // If they divide, no need to round
  if (clasp_zerop(rem)) {
    quotient = tru;
    remainder = rem;
    return;
  }

  Real_sp threshold = gc::As_unsafe<Real_sp>(clasp_divide(clasp_abs(divisor), clasp_make_fixnum(2)));
  int c = clasp_number_compare(rem, threshold);
  if (c > 0 || (c == 0 && clasp_oddp(tru))) {
    if (clasp_minusp(divisor)) {
      quotient = gc::As_unsafe<Integer_sp>(contagion_sub(tru, clasp_make_fixnum(1)));
      remainder = gc::As_unsafe<Real_sp>(contagion_add(rem, divisor));
    } else {
      quotient = gc::As_unsafe<Integer_sp>(clasp_one_plus(tru));
      remainder = gc::As_unsafe<Real_sp>(contagion_sub(rem, divisor));
    }
    return;
  }
  threshold = gc::As_unsafe<Real_sp>(clasp_negate(threshold));
  c = clasp_number_compare(rem, threshold);
  if (c < 0 || (c == 0 && clasp_oddp(tru))) {
    if (clasp_minusp(divisor)) {
      quotient = gc::As_unsafe<Integer_sp>(clasp_one_plus(tru));
      remainder = gc::As_unsafe<Real_sp>(contagion_sub(rem, divisor));
    } else {
      quotient = gc::As_unsafe<Integer_sp>(contagion_sub(tru, clasp_make_fixnum(1)));
      remainder = gc::As_unsafe<Real_sp>(contagion_add(rem, divisor));
    }
    return;
  }
  // not rounding
  quotient = tru;
  remainder = rem;
}

Real_mv clasp_round1(Real_sp x) {
  switch (clasp_t_of(x)) {
  case NumberType::Fixnum:
  case NumberType::Bignum:
    return Values(x, clasp_make_fixnum(0));
  case NumberType::Ratio: {
    Ratio_sp rx = gc::As<Ratio_sp>(x);
    Integer_sp tv0;
    Real_sp tv1;
    clasp_round(rx->numerator(), rx->denominator(), tv0, tv1);
    return Values(tv0, Ratio_O::create(gc::As_unsafe<Integer_sp>(tv1), rx->denominator()));
  }
  case NumberType::SingleFloat: {
    float d = x.unsafe_single_float();
    float q = round_double(d);
    return Values(_clasp_float_to_integer(q), clasp_make_single_float(d - q));
  }
  case NumberType::DoubleFloat: {
    double d = gc::As_unsafe<DoubleFloat_sp>(x)->get();
    double q = round_double(d);
    return Values(_clasp_double_to_integer(q), clasp_make_double_float(d - q));
  }
#ifdef CLASP_LONG_FLOAT
  case NumberType::LongFloat: {
    LongFloat d = clasp_long_float(x);
    LongFloat q = round_long_double(d);
    return Values(_clasp_long_double_to_integer(q), clasp_make_long_float(d - q));
  }
#endif
  default:
    ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_round, 1, x, cl::_sym_Real_O);
  }
}

Real_mv clasp_round2(Real_sp dividend, Real_sp divisor) {
  Integer_sp v0;
  Real_sp v1;
  clasp_round(dividend, divisor, v0, v1);
  return Values(v0, v1);
}

CL_LAMBDA(dividend &optional divisor);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(round)dx");
DOCGROUP(clasp);
CL_DEFUN Number_mv cl__round(Real_sp x, T_sp y) {
  if (y.nilp())
    return clasp_round1(x);
  else
    return clasp_round2(x, gc::As<Real_sp>(y));
}

CL_LAMBDA(x y);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(mod)dx");
DOCGROUP(clasp);
CL_DEFUN Real_sp cl__mod(Real_sp dividend, Real_sp divisor) {
  Integer_sp q;
  Real_sp mod;
  clasp_floor(dividend, divisor, q, mod);
  return mod;
}

CL_LAMBDA(x y);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(rem)dx");
DOCGROUP(clasp);
CL_DEFUN Real_sp cl__rem(Real_sp dividend, Real_sp divisor) {
  Integer_sp q;
  Real_sp rem;
  clasp_truncate(dividend, divisor, q, rem);
  return rem;
}

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(decodeFloat)dx");
DOCGROUP(clasp);
CL_DEFUN Number_mv cl__decode_float(Float_sp x) {
  int e = 0, s = 0;
  NumberType tx = clasp_t_of(x);
  float f;
  switch (tx) {
  case NumberType::SingleFloat: {
    f = x.unsafe_single_float();
    if (std::isfinite(f)) {
      if (f >= 0.0) {
        s = 1;
      } else {
        f = -f;
        s = 0;
      }
      f = frexpf(f, &e);
      x = clasp_make_single_float(f);
    } else {
      SIMPLE_ERROR("Can't decode NaN or infinity {}", _rep_(x));
    }
    break;
  }
  case NumberType::DoubleFloat: {
    double d = gc::As_unsafe<DoubleFloat_sp>(x)->get();
    if (std::isfinite(d)) {
      if (d >= 0.0) {
        s = 1;
      } else {
        d = -d;
        s = 0;
      }
      d = frexp(d, &e);
      x = clasp_make_double_float(d);
    } else {
      SIMPLE_ERROR("Can't decode NaN or infinity {}", _rep_(x));
    }
    break;
  }
#ifdef CLASP_LONG_FLOAT
  case NumberType::LongFloat: {
    LongFloat d = clasp_long_float(x);
    if (std::isfinite(d)) {
      if (d >= 0.0)
        s = 1;
      else {
        d = -d;
        s = 0;
      }
      d = frexpl(d, &e);
      x = clasp_make_long_float(d);
    } else {
      SIMPLE_ERROR("Can't decode NaN or infinity {}", _rep_(x));
    }
    break;
  }
#endif
  default:
    ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_decodeFloat, 1, x, cl::_sym_float);
  }
  return Values(x, clasp_make_fixnum(e), clasp_make_single_float(s));
}

CL_LAMBDA(x y);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(scaleFloat)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp cl__scale_float(Number_sp x, Number_sp y) {
  Fixnum k;
  if (CLASP_FIXNUMP(y)) {
    k = y.unsafe_fixnum();
  } else {
    ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_scaleFloat, 2, y, cl::_sym_fixnum);
  }
  switch (clasp_t_of(x)) {
  case NumberType::SingleFloat:
    x = clasp_make_single_float(std::ldexp(x.unsafe_single_float(), k));
    break;
  case NumberType::DoubleFloat:
    x = clasp_make_double_float(std::ldexp(gc::As_unsafe<DoubleFloat_sp>(x)->get(), k));
    break;
#ifdef CLASP_LONG_FLOAT
  case NumberType::LongFloat:
    x = clasp_make_long_float(std::ldexp(clasp_long_float(x), k));
    break;
#endif
  default:
    ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_scaleFloat, 1, x, cl::_sym_float);
  }
  return x;
}

Integer_sp cl__float_radix(Float_sp x) { return clasp_make_fixnum(FLT_RADIX); }

int clasp_signbit(Number_sp x) {
  switch (clasp_t_of(x)) {
  case NumberType::SingleFloat:
    return std::signbit(x.unsafe_single_float());
  case NumberType::DoubleFloat:
    return std::signbit(gc::As_unsafe<DoubleFloat_sp>(x)->get());
#ifdef CLASP_LONG_FLOAT
  case NumberType::LongFloat:
    return signbit(clasp_long_float(x));
#endif
  default:
    ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_floatSign, 1, x, cl::_sym_float);
  }
}

CL_LAMBDA(x &optional (y nil yp));
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(floatSign)dx");
DOCGROUP(clasp);
CL_DEFUN Float_sp cl__float_sign(Float_sp x, T_sp oy, T_sp yp) {
  Float_sp y = yp.nilp() ? cl__float(clasp_make_fixnum(1), x) : gc::As<Float_sp>(oy);
  int negativep = clasp_signbit(x);
  switch (clasp_t_of(y)) {
  case NumberType::SingleFloat: {
    float f = y.unsafe_single_float();
    if (std::signbit(f) != negativep)
      y = clasp_make_single_float(-f);
    break;
  }
  case NumberType::DoubleFloat: {
    double f = gc::As_unsafe<DoubleFloat_sp>(y)->get();
    if (std::signbit(f) != negativep)
      y = clasp_make_double_float(-f);
    break;
  }
#ifdef CLASP_LONG_FLOAT
  case NumberType::LongFloat: {
    LongFloat f = clasp_long_float(y);
    if (std::signbit(f) != negativep)
      y = clasp_make_long_float(-f);
    break;
  }
#endif
  default:
    ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_floatSign, 2, y, cl::_sym_float);
  }
  return y;
}

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(floatDigits)dx");
DOCGROUP(clasp);
CL_DEFUN Integer_sp cl__float_digits(Float_sp x) {
  Integer_sp ix(nil<Integer_O>());
  switch (clasp_t_of(x)) {
  case NumberType::SingleFloat:
    ix = clasp_make_fixnum(FLT_MANT_DIG);
    break;
  case NumberType::DoubleFloat:
    ix = clasp_make_fixnum(DBL_MANT_DIG);
    break;
#ifdef CLASP_LONG_FLOAT
  case NumberType::LongFloat:
    ix = clasp_make_fixnum(LDBL_MANT_DIG);
    break;
#endif
  default:
    ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_floatDigits, 1, x, cl::_sym_float);
  }
  return ix;
}

CL_LAMBDA(value);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(floatPrecision)dx");
DOCGROUP(clasp);
CL_DEFUN Integer_sp cl__float_precision(Float_sp x) {
  int precision = 0;
  switch (clasp_t_of(x)) {
  case NumberType::SingleFloat: {
    float f = x.unsafe_single_float();
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
  case NumberType::DoubleFloat: {
    double f = gc::As_unsafe<DoubleFloat_sp>(x)->get();
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
  case NumberType::LongFloat: {
    LongFloat f = clasp_long_float(x);
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
    ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_floatPrecision, 1, x, cl::_sym_float);
  }
  return clasp_make_fixnum(precision);
}

template <typename Float> inline Real_mv integer_decode_float(Float f) {
  struct float_convert<Float>::quadruple q;

  // IEEE-754-2019 7.2.j specifies that NaN or infinity should result in FE_INVALID for integer producing operations.
  switch (std::fpclassify(f)) {
  case FP_INFINITE:
    feraiseexcept(FE_INVALID);
    q = float_convert<Float>::to_quadruple(std::signbit(f) ? std::numeric_limits<Float>::min() : std::numeric_limits<Float>::max());
    break;
  case FP_NAN:
    feraiseexcept(FE_INVALID);
    q = float_convert<Float>::to_quadruple(std::signbit(f) ? Float{-0.0} : Float{0.0});
    break;
  default:
    q = float_convert<Float>::to_quadruple(f);
    break;
  }

  return Values(Integer_O::create(q.significand), clasp_make_fixnum(q.exponent), clasp_make_fixnum(q.sign));
}

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(integer_decode_float)dx");
DOCGROUP(clasp);
CL_DEFUN Real_mv cl__integer_decode_float(Float_sp x) {
  switch (clasp_t_of(x)) {
#ifdef CLASP_LONG_FLOAT
  case NumberType::LongFloat:
    return integer_decode_float(gc::As_unsafe<LongFloat_sp>(x)->get());
#endif
  case NumberType::DoubleFloat:
    return integer_decode_float(gc::As_unsafe<DoubleFloat_sp>(x)->get());
  case NumberType::SingleFloat:
    return integer_decode_float(x.unsafe_single_float());
  default:
    ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_integer_decode_float, 1, x, cl::_sym_float);
  }
}

CL_LAMBDA(r &optional (i 0));
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(complex)dx");
DOCGROUP(clasp);
CL_DEFUN Complex_sp cl__complex(Real_sp r, Real_sp i) { return gc::As_unsafe<Complex_sp>(clasp_make_complex(r, i)); }

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(realpart)dx");
DOCGROUP(clasp);
CL_DEFUN Real_sp cl__realpart(Number_sp x) {
  switch (clasp_t_of(x)) {
  case NumberType::Fixnum:
  case NumberType::Bignum:
  case NumberType::Ratio:
  case NumberType::SingleFloat:
  case NumberType::DoubleFloat:
#ifdef CLASP_LONG_FLOAT
  case NumberType::LongFloat:
#endif
    return gc::As_unsafe<Real_sp>(x);
  case NumberType::Complex:
    return gc::As_unsafe<Complex_sp>(x)->real();
  default:
    ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_realpart, 1, x, cl::_sym_Number_O);
  }
}

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(imagpart)dx");
DOCGROUP(clasp);
CL_DEFUN Real_sp cl__imagpart(Number_sp x) {
  switch (clasp_t_of(x)) {
  case NumberType::Fixnum:
  case NumberType::Bignum:
  case NumberType::Ratio:
    return clasp_make_fixnum(0);
  case NumberType::SingleFloat:
    return clasp_make_single_float((float)0 * x.unsafe_single_float());
  case NumberType::DoubleFloat:
    return DoubleFloat_O::create((float)0 * gc::As_unsafe<DoubleFloat_sp>(x)->get());
#ifdef CLASP_LONG_FLOAT
  case NumberType::LongFloat:
    return LongFloat_O::create((float)0 * clasp_long_float(x));
#endif
  case NumberType::Complex:
    return gc::As_unsafe<Complex_sp>(x)->imaginary();
  default:
    ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_imagpart, 1, x, cl::_sym_Number_O);
  }
}

SYMBOL_EXPORT_SC_(ClPkg, float);
SYMBOL_EXPORT_SC_(ClPkg, numerator);
SYMBOL_EXPORT_SC_(ClPkg, denominator);
SYMBOL_EXPORT_SC_(ClPkg, floor);
SYMBOL_EXPORT_SC_(ClPkg, ceiling);
SYMBOL_EXPORT_SC_(ClPkg, truncate);
SYMBOL_EXPORT_SC_(ClPkg, round);
SYMBOL_EXPORT_SC_(ClPkg, mod);
SYMBOL_EXPORT_SC_(ClPkg, rem);
SYMBOL_EXPORT_SC_(ClPkg, decodeFloat);
SYMBOL_EXPORT_SC_(ClPkg, scaleFloat);
SYMBOL_EXPORT_SC_(ClPkg, floatSign);
SYMBOL_EXPORT_SC_(ClPkg, floatDigits);
SYMBOL_EXPORT_SC_(ClPkg, floatPrecision);
SYMBOL_EXPORT_SC_(ClPkg, integer_decode_float);
SYMBOL_EXPORT_SC_(ClPkg, complex);
SYMBOL_EXPORT_SC_(ClPkg, realpart);
SYMBOL_EXPORT_SC_(ClPkg, imagpart);

}; // namespace core
