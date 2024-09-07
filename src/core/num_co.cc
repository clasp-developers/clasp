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

CL_LAMBDA(x &optional (y nil yp));
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(float)dx");
DOCGROUP(clasp);
CL_DEFUN Float_sp cl__float(Real_sp x, T_sp y, bool yp) {
  if (!yp) {
    if (x.isA<Float_O>())
      return x;
    return SingleFloat_dummy_O::coerce(x);
  }

#ifdef CLASP_LONG_FLOAT
  if (y.isA<LongFloat_O>())
    return LongFloat_O::coerce(x);
#endif

  if (y.isA<DoubleFloat_O>())
    return DoubleFloat_O::coerce(x);

  if (y.single_floatp())
    return SingleFloat_dummy_O::coerce(x);

#ifdef CLASP_SHORT_FLOAT
  if (y.short_floatp())
    return ShortFloat_O::coerce(x);
#endif

  ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_float, 2, y, cl::_sym_float);
}

// Simpler versions used by the compiler.
#ifdef CLASP_SHORT_FLOAT
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN ShortFloat_sp core__to_short_float(Real_sp x) { return ShortFloat_O::coerce(x); }
#endif

CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN SingleFloat_sp core__to_single_float(Real_sp x) { return SingleFloat_dummy_O::coerce(x); }

CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN DoubleFloat_sp core__to_double_float(Real_sp x) { return DoubleFloat_O::coerce(x); }

#ifdef CLASP_LONG_FLOAT
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN LongFloat_sp core__to_long_float(Real_sp x) { return LongFloat_O::coerce(x); }
#endif

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(numerator)dx");
DOCGROUP(clasp);
CL_DEFUN Integer_sp cl__numerator(Rational_sp x) {
  Ratio_sp rx = x.asOrNull<Ratio_O>();
  if (rx)
    return rx->numerator();
  if (x.fixnump())
    return x;

  return x.as<Integer_O>();
}

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(denominator)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp cl__denominator(Rational_sp x) {
  Ratio_sp rx = x.asOrNull<Ratio_O>();
  if (rx)
    return rx->denominator();

  return clasp_make_fixnum(1);
}

template <std::floating_point Float> void float_trunc(Float dividend, Float divisor, Integer_sp& quotient, Real_sp& remainder) {
  Float p = dividend / divisor;
  Float q = std::trunc(p);
  quotient = Integer_O::create(q);
  remainder = Number_O::make_float(p * divisor - q * divisor);
}

// Stores the result in quotient, remainder.
static void clasp_truncate(Real_sp dividend, Real_sp divisor, Integer_sp& quotient, Real_sp& remainder) {
  // The CL standard is a bit ambiguous about the type of the remainder.
  // We treat it as a contagion thing: If either argument is a float, the
  // remainder is a float of the largest format among the arguments.
  Ratio_sp rdividend = dividend.asOrNull<Ratio_O>(), rdivisor = divisor.asOrNull<Ratio_O>();
  if (rdividend && rdivisor) {
    Real_sp subr;
    clasp_truncate(rdividend->numerator() * rdivisor->denominator(), rdivisor->numerator() * rdividend->denominator(), quotient,
                   subr);
    remainder = Rational_O::create(subr, rdividend->denominator() * rdivisor->denominator());
    return;
  }
  if (rdividend) {
    Real_sp subr;
    clasp_truncate(rdividend->numerator(), rdividend->denominator() * divisor, quotient, subr);
    remainder = subr / rdividend->denominator();
    return;
  }
  if (rdivisor && dividend.isA<Integer_O>()) {
    Real_sp subr;
    clasp_truncate(dividend * rdivisor->denominator(), rdivisor->numerator(), quotient, subr);
    remainder = Rational_O::create(subr, rdivisor->denominator());
    return;
  }

#ifdef CLASP_LONG_FLOAT
  if (dividend.isA<LongFloat_O>() || divisor.isA<LongFloat_O>()) {
    float_trunc(Number_O::as_long_float(dividend), Number_O::as_long_float(divisor), quotient, remainder);
    return;
  }
#endif

  if (dividend.isA<DoubleFloat_O>() || divisor.isA<DoubleFloat_O>()) {
    float_trunc(Number_O::as_double_float(dividend), Number_O::as_double_float(divisor), quotient, remainder);
    return;
  }

  if (dividend.single_floatp() || divisor.single_floatp()) {
    float_trunc(Number_O::as_single_float(dividend), Number_O::as_single_float(divisor), quotient, remainder);
    return;
  }

#ifdef CLASP_SHORT_FLOAT
  if (dividend.short_floatp() || divisor.short_floatp()) {
    float_trunc(Number_O::as_short_float(dividend), Number_O::as_short_float(divisor), quotient, remainder);
    return;
  }
#endif

  Bignum_sp bdividend = dividend.asOrNull<Bignum_O>(), bdivisor = divisor.asOrNull<Bignum_O>();
  if (bdividend && bdivisor) {
    T_mv mvr = core__next_truncate(bdividend, bdivisor);
    quotient = mvr;
    MultipleValues& mvn = core::lisp_multipleValues();
    remainder = mvn.valueGet(1, mvr.number_of_values());
    return;
  }
  if (bdividend) {
    T_mv rmv = core__next_ftruncate(bdividend, divisor.unsafe_fixnum());
    quotient = gc::As_unsafe<Integer_sp>(rmv);
    MultipleValues& mvn = core::lisp_multipleValues();
    remainder = gc::As_unsafe<Integer_sp>(mvn.valueGet(1, rmv.number_of_values()));
    return;
  }
  if (bdivisor) {
    Fixnum a = dividend.unsafe_fixnum();
    if ((a == gc::most_negative_fixnum) && (bdivisor->length() == 1) && ((bdivisor->limbs())[0] == -gc::most_negative_fixnum)) {
      quotient = clasp_make_fixnum(-1);
      remainder = clasp_make_fixnum(0);
    } else {
      quotient = clasp_make_fixnum(0);
      remainder = dividend;
    }
    return;
  }

  Fixnum a = dividend.unsafe_fixnum();
  Fixnum b = divisor.unsafe_fixnum();
  // Uniquely, (truncate most-negative-fixnum -1) is a bignum, so
  // we can't just use clasp_make_fixnum for the quotient.
  quotient = Integer_O::create(a / b);
  remainder = clasp_make_fixnum(a % b);
}

static void clasp_floor(Real_sp dividend, Real_sp divisor, Integer_sp& quotient, Real_sp& remainder) {
  Integer_sp t0;
  Real_sp t1;
  clasp_truncate(dividend, divisor, t0, t1);
  if (!(Number_O::zerop(t1)) && (Real_O::minusp(divisor) ? Real_O::plusp(dividend) : Real_O::minusp(dividend))) {
    quotient = gc::As_unsafe<Integer_sp>(clasp_one_minus(t0));
    remainder = gc::As_unsafe<Real_sp>(t1 + divisor);
  } else {
    quotient = t0;
    remainder = t1;
  }
}

template <std::floating_point Float> Real_mv _floor1(Float x) {
  switch (std::fpclassify(x)) {
  case FP_NAN:
    feraiseexcept(FE_INVALID);
    return Values(clasp_make_fixnum(0), Number_O::make_float(x));
  case FP_INFINITE:
    return Values(clasp_make_fixnum(0), Number_O::make_float(x));
  default: {
    Float f = std::floor(x);
    return Values(Integer_O::create(f), Number_O::make_float(x - f));
  }
  }
}

Real_mv clasp_floor1(Real_sp x) {
  if (x.fixnump() || x.isA<Integer_O>())
    return Values(x, clasp_make_fixnum(0));

  Ratio_sp rx = x.asOrNull<Ratio_O>();
  if (rx) {
    Integer_sp v0;
    Real_sp tv1;
    clasp_floor(rx->numerator(), rx->denominator(), v0, tv1);
    return Values(v0, Ratio_O::create(tv1, rx->denominator()));
  }

#ifdef CLASP_LONG_FLOAT
  if (x.isA<LongFloat_O>())
    return _floor1(x.as_unsafe<LongFloat_O>()->get());
#endif

  if (x.isA<DoubleFloat_O>())
    return _floor1(x.as_unsafe<DoubleFloat_O>()->get());

  if (x.single_floatp())
    return _floor1(x.unsafe_single_float());

#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp())
    return _floor1(x.unsafe_short_float());
#endif

  ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_floor, 1, x, cl::_sym_Real_O);
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
  if (!(Number_O::zerop(t1)) && (Real_O::minusp(divisor) ? Real_O::minusp(dividend) : Real_O::plusp(dividend))) {
    quotient = gc::As_unsafe<Integer_sp>(clasp_one_plus(t0));
    remainder = gc::As_unsafe<Real_sp>(t1 - divisor);
  } else {
    quotient = t0;
    remainder = t1;
  }
}

template <std::floating_point Float> Real_mv _ceiling1(Float x) {
  switch (std::fpclassify(x)) {
  case FP_NAN:
    feraiseexcept(FE_INVALID);
    return Values(clasp_make_fixnum(0), Number_O::make_float(x));
  case FP_INFINITE:
    return Values(clasp_make_fixnum(0), Number_O::make_float(x));
  default: {
    Float f = std::ceil(x);
    return Values(Integer_O::create(f), Number_O::make_float(x - f));
  }
  }
}

Real_mv clasp_ceiling1(Real_sp x) {
  if (x.fixnump() || x.isA<Integer_O>())
    return Values(x, clasp_make_fixnum(0));

  Ratio_sp rx = x.asOrNull<Ratio_O>();
  if (rx) {
    Integer_sp v0;
    Real_sp tv1;
    clasp_ceiling(rx->numerator(), rx->denominator(), v0, tv1);
    return Values(v0, Ratio_O::create(tv1, rx->denominator()));
  }

#ifdef CLASP_LONG_FLOAT
  if (x.isA<LongFloat_O>())
    return _ceiling1(x.as_unsafe<LongFloat_O>()->get());
#endif

  if (x.isA<DoubleFloat_O>())
    return _ceiling1(x.as_unsafe<DoubleFloat_O>()->get());

  if (x.single_floatp())
    return _ceiling1(x.unsafe_single_float());

#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp())
    return _ceiling1(x.unsafe_short_float());
#endif

  ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_floor, 1, x, cl::_sym_Real_O);
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

template <std::floating_point Float> Real_mv _truncate1(Float x) {
  switch (std::fpclassify(x)) {
  case FP_NAN:
    feraiseexcept(FE_INVALID);
    return Values(clasp_make_fixnum(0), Number_O::make_float(x));
  case FP_INFINITE:
    return Values(clasp_make_fixnum(0), Number_O::make_float(x));
  default: {
    Float f = std::signbit(x) ? std::ceil(x) : std::floor(x);
    return Values(Integer_O::create(f), Number_O::make_float(x - f));
  }
  }
}

Real_mv clasp_truncate1(Real_sp x) {
  if (x.fixnump() || x.isA<Integer_O>())
    return Values(x, clasp_make_fixnum(0));

  Ratio_sp rx = x.asOrNull<Ratio_O>();
  if (rx) {
    Integer_sp v0;
    Real_sp tv1;
    clasp_truncate(rx->numerator(), rx->denominator(), v0, tv1);
    return Values(v0, Ratio_O::create(tv1, rx->denominator()));
  }

#ifdef CLASP_LONG_FLOAT
  if (x.isA<LongFloat_O>())
    return _truncate1(x.as_unsafe<LongFloat_O>()->get());
#endif

  if (x.isA<DoubleFloat_O>())
    return _truncate1(x.as_unsafe<DoubleFloat_O>()->get());

  if (x.single_floatp())
    return _truncate1(x.unsafe_single_float());

#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp())
    return _truncate1(x.unsafe_short_float());
#endif

  ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_floor, 1, x, cl::_sym_Real_O);
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

static void clasp_round(Real_sp dividend, Real_sp divisor, Integer_sp& quotient, Real_sp& remainder) {
  Integer_sp tru;
  Real_sp rem;
  clasp_truncate(dividend, divisor, tru, rem);

  // If they divide, no need to round
  if (Number_O::zerop(rem)) {
    quotient = tru;
    remainder = rem;
    return;
  }

  Real_sp threshold = gc::As_unsafe<Real_sp>(Number_O::abs(divisor) / clasp_make_fixnum(2));
  int c = Number_O::compare(rem, threshold);
  if (c > 0 || (c == 0 && Integer_O::oddp(tru))) {
    if (Real_O::minusp(divisor)) {
      quotient = gc::As_unsafe<Integer_sp>(tru - clasp_make_fixnum(1));
      remainder = rem + divisor;
    } else {
      quotient = gc::As_unsafe<Integer_sp>(clasp_one_plus(tru));
      remainder = gc::As_unsafe<Real_sp>(rem - divisor);
    }
    return;
  }
  threshold = gc::As_unsafe<Real_sp>(clasp_negate(threshold));
  c = Number_O::compare(rem, threshold);
  if (c < 0 || (c == 0 && Integer_O::oddp(tru))) {
    if (Real_O::minusp(divisor)) {
      quotient = gc::As_unsafe<Integer_sp>(clasp_one_plus(tru));
      remainder = gc::As_unsafe<Real_sp>(rem - divisor);
    } else {
      quotient = gc::As_unsafe<Integer_sp>(tru - clasp_make_fixnum(1));
      remainder = rem + divisor;
    }
    return;
  }
  // not rounding
  quotient = tru;
  remainder = rem;
}

template <std::floating_point Float> Real_mv _round1(Float x) {
  switch (std::fpclassify(x)) {
  case FP_NAN:
    feraiseexcept(FE_INVALID);
    return Values(clasp_make_fixnum(0), Number_O::make_float(x));
  case FP_INFINITE:
    return Values(clasp_make_fixnum(0), Number_O::make_float(x));
  default: {
    auto r = std::fegetround();
    std::fesetround(FE_TONEAREST);
    Float f = std::rint(x);
    std::fesetround(r);
    return Values(Integer_O::create(f), Number_O::make_float(x - f));
  }
  }
}

Real_mv clasp_round1(Real_sp x) {
  if (x.fixnump() || x.isA<Integer_O>())
    return Values(x, clasp_make_fixnum(0));

  Ratio_sp rx = x.asOrNull<Ratio_O>();
  if (rx) {
    Integer_sp v0;
    Real_sp tv1;
    clasp_round(rx->numerator(), rx->denominator(), v0, tv1);
    return Values(v0, Ratio_O::create(tv1, rx->denominator()));
  }

#ifdef CLASP_LONG_FLOAT
  if (x.isA<LongFloat_O>())
    return _round1(x.as_unsafe<LongFloat_O>()->get());
#endif

  if (x.isA<DoubleFloat_O>())
    return _round1(x.as_unsafe<DoubleFloat_O>()->get());

  if (x.single_floatp())
    return _round1(x.unsafe_single_float());

#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp())
    return _round1(x.unsafe_short_float());
#endif

  ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_floor, 1, x, cl::_sym_Real_O);
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

template <std::floating_point Float> Number_mv decode_float(Float x) {
  if (std::isfinite(x)) {
    int e = 0;
    Float s = std::copysign(Float{1}, x);
    x = std::frexp(std::abs(x), &e);
    return Values(Number_O::make_float(x), clasp_make_fixnum(e), Number_O::make_float(s));
  }
  SIMPLE_ERROR("Can't decode NaN or infinity {}", x);
}

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(decodeFloat)dx");
DOCGROUP(clasp);
CL_DEFUN Number_mv cl__decode_float(Float_sp x) {
#ifdef CLASP_LONG_FLOAT
  if (x.isA<LongFloat_O>())
    return decode_float(x.as_unsafe<LongFloat_O>()->get());
#endif

  if (x.isA<DoubleFloat_O>())
    return decode_float(x.as_unsafe<DoubleFloat_O>()->get());

  if (x.single_floatp())
    return decode_float(x.unsafe_single_float());

#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp())
    return decode_float(x.unsafe_short_float());
#endif

  ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_decodeFloat, 1, x, cl::_sym_float);
}

CL_LAMBDA(x y);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(scaleFloat)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp cl__scale_float(Number_sp x, Fixnum y) {
#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp())
    return ShortFloat_O::create(std::ldexp(x.unsafe_short_float(), y));
#endif
  if (x.single_floatp())
    return SingleFloat_dummy_O::create(std::ldexp(x.unsafe_single_float(), y));
  if (x.isA<DoubleFloat_O>())
    return DoubleFloat_O::create(std::ldexp(x.as_unsafe<DoubleFloat_O>()->get(), y));
#ifdef CLASP_LONG_FLOAT
  if (x.isA<LongFloat_O>())
    return LongFloat_O::create(std::ldexp(x.as_unsafe<LongFloat_O>()->get(), y));
#endif
  ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_scaleFloat, 1, x, cl::_sym_float);
}

Integer_sp cl__float_radix(Float_sp x) { return clasp_make_fixnum(FLT_RADIX); }

bool clasp_signbit(Number_sp x) {
#ifdef CLASP_LONG_FLOAT
  if (x.isA<LongFloat_O>())
    return std::signbit(x.as_unsafe<LongFloat_O>()->get());
#endif

  if (x.isA<DoubleFloat_O>())
    return std::signbit(x.as_unsafe<DoubleFloat_O>()->get());

  if (x.single_floatp())
    return std::signbit(x.unsafe_single_float());

#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp())
    return std::signbit(x.unsafe_short_float());
#endif

  ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_floatSign, 1, x, cl::_sym_float);
}

CL_LAMBDA(x &optional (y nil yp));
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(floatSign)dx");
DOCGROUP(clasp);
CL_DEFUN Float_sp cl__float_sign(Float_sp x, T_sp oy, bool yp) {
  if (!yp) {
#ifdef CLASP_LONG_FLOAT
    if (x.isA<LongFloat_O>())
      return Number_O::make_float(std::copysign(long_float_t{1}, x.as_unsafe<LongFloat_O>()->get()));
#endif
    if (x.isA<DoubleFloat_O>())
      return Number_O::make_float(std::copysign(double_float_t{1}, x.as_unsafe<DoubleFloat_O>()->get()));
    if (x.single_floatp())
      return Number_O::make_float(std::copysign(single_float_t{1}, x.unsafe_single_float()));
#ifdef CLASP_SHORT_FLOAT
    if (x.short_floatp())
      return Number_O::make_float(std::copysign(short_float_t{1}, x.unsafe_short_float()));
#endif
  }

  int sign = clasp_signbit(x) ? -1 : 1;

  if (oy.single_floatp())
    return Number_O::make_float(std::copysign(oy.unsafe_single_float(), sign));

#ifdef CLASP_SHORT_FLOAT
  if (oy.short_floatp())
    return Number_O::make_float(std::copysign(oy.unsafe_short_float(), sign));
#endif

  if (oy.isA<DoubleFloat_O>())
    return Number_O::make_float(std::copysign(oy.as_unsafe<DoubleFloat_O>()->get(), sign));

#ifdef CLASP_LONG_FLOAT
  if (oy.isA<LongFloat_O>())
    return Number_O::make_float(std::copysign(oy.as_unsafe<LongFloat_O>()->get(), sign));
#endif

  ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_floatSign, 2, oy, cl::_sym_float);
}

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(floatDigits)dx");
DOCGROUP(clasp);
CL_DEFUN Integer_sp cl__float_digits(Float_sp x) {
#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp())
    return clasp_make_fixnum(std::numeric_limits<short_float_t>::digits);
#endif
  if (x.single_floatp())
    return clasp_make_fixnum(std::numeric_limits<single_float_t>::digits);
  if (x.isA<DoubleFloat_O>())
    return clasp_make_fixnum(std::numeric_limits<double_float_t>::digits);
#ifdef CLASP_LONG_FLOAT
  if (x.isA<LongFloat_O>())
    return clasp_make_fixnum(std::numeric_limits<long_float_t>::digits);
#endif
  ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_floatPrecision, 1, x, cl::_sym_float);
}

template <std::floating_point Float> size_t float_precision(Float f) {
  switch (std::fpclassify(f)) {
  case FP_ZERO:
    return 0;
  case FP_SUBNORMAL:
    return 1 - std::numeric_limits<Float>::min_exponent + std::ilogb(f) + std::numeric_limits<Float>::digits;
  default:
    return std::numeric_limits<Float>::digits;
  }
}

CL_LAMBDA(value);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(floatPrecision)dx");
DOCGROUP(clasp);
CL_DEFUN Integer_sp cl__float_precision(Float_sp x) {
#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp())
    return clasp_make_fixnum(float_precision(x.unsafe_short_float()));
#endif
  if (x.single_floatp())
    return clasp_make_fixnum(float_precision(x.unsafe_single_float()));
  if (x.isA<DoubleFloat_O>())
    return clasp_make_fixnum(float_precision(x.as_unsafe<DoubleFloat_O>()->get()));
#ifdef CLASP_LONG_FLOAT
  if (x.isA<LongFloat_O>())
    return clasp_make_fixnum(float_precision(x.as_unsafe<LongFloat_O>()->get()));
#endif
  ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_floatPrecision, 1, x, cl::_sym_float);
}

template <typename Float> inline Real_mv integer_decode_float(Float f) {
  struct float_convert<Float>::quadruple q;

  // IEEE-754-2019 7.2.j specifies that NaN or infinity should result in FE_INVALID for integer producing operations.
  switch (std::fpclassify(f)) {
  case FP_INFINITE:
    feraiseexcept(FE_INVALID);
    q = float_convert<Float>::float_to_quadruple(std::signbit(f) ? std::numeric_limits<Float>::min()
                                                                 : std::numeric_limits<Float>::max());
    break;
  case FP_NAN:
    feraiseexcept(FE_INVALID);
    q = float_convert<Float>::float_to_quadruple(std::signbit(f) ? Float{-0.0} : Float{0.0});
    break;
  default:
    q = float_convert<Float>::float_to_quadruple(f);
    break;
  }

  return Values(Integer_O::create((typename float_convert<Float>::uint_t)q.significand), clasp_make_fixnum(q.exponent),
                clasp_make_fixnum(q.sign));
}

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(integer_decode_float)dx");
DOCGROUP(clasp);
CL_DEFUN Real_mv cl__integer_decode_float(Float_sp x) {
#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp())
    return integer_decode_float(x.unsafe_short_float());
#endif
  if (x.single_floatp())
    return integer_decode_float(x.unsafe_single_float());
  if (x.isA<DoubleFloat_O>())
    return integer_decode_float(x.as_unsafe<DoubleFloat_O>()->get());
#ifdef CLASP_LONG_FLOAT
  if (x.isA<LongFloat_O>())
    return integer_decode_float(x.as_unsafe<LongFloat_O>()->get());
#endif
  ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_integer_decode_float, 1, x, cl::_sym_float);
}

CL_LAMBDA(r &optional (i 0));
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(complex)dx");
DOCGROUP(clasp);
CL_DEFUN Complex_sp cl__complex(Real_sp r, Real_sp i) { return gc::As_unsafe<Complex_sp>(clasp_make_complex(r, i)); }

Number_sp DoubleFloat_O::imagpart_() const { return create(std::copysign(double_float_t{0.0}, _Value)); }

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::imagpart_() const { return create(std::copysign(long_float_t{0.0}, _Value)); }
#endif

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(realpart)dx");
DOCGROUP(clasp);
CL_DEFUN Real_sp cl__realpart(Number_sp x) { return Number_O::realpart(x); }

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(imagpart)dx");
DOCGROUP(clasp);
CL_DEFUN Real_sp cl__imagpart(Number_sp x) { return Number_O::imagpart(x); }

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
