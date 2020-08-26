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

static Number_sp
number_remainder(Number_sp x, Number_sp y, Number_sp q) {
  Number_sp z;

  z = clasp_times(q, y);
  z = clasp_minus(x, z);
  return (z);
}

/* Coerce X to single-float if one arg,
   otherwise coerce to same float type as second arg */

CL_LAMBDA(x &optional y);
CL_DECLARE();
CL_DOCSTRING("float");
CL_DEFUN Float_sp cl__float(Real_sp x, T_sp y) {
  NumberType ty, tx;
  if (y.notnilp()) {
    ty = clasp_t_of(gc::As<Float_sp>(y));
  } else {
    ty = number_SingleFloat;
  }
  switch (tx = clasp_t_of(x)) {
  case number_SingleFloat:
  case number_DoubleFloat:
#ifdef CLASP_LONG_FLOAT
  case number_LongFloat:
#endif
    if (y.nilp() || ty == tx)
      break;
  case number_Fixnum:
  case number_Bignum:
  case number_Ratio:
    switch (ty) {
    case number_SingleFloat:
      x = gc::As<Real_sp>(clasp_make_single_float(clasp_to_double(x)));
      break;
    case number_DoubleFloat:
      x = gc::As<Real_sp>(clasp_make_double_float(clasp_to_double(x)));
      break;
#ifdef CLASP_LONG_FLOAT
    case number_LongFloat:
      x = clasp_make_long_float(clasp_to_long_float(x)).as<Real_O>();
      break;
#endif
    default:
      QERROR_WRONG_TYPE_NTH_ARG(2, y, cl::_sym_float);
    }
    break;
  default:
    QERROR_WRONG_TYPE_NTH_ARG(1, x, cl::_sym_Real_O);
  }
  return gc::As<Float_sp>(x);
}

CL_LAMBDA(x);
CL_DECLARE();
CL_DOCSTRING("numerator");
CL_DEFUN Number_sp cl__numerator(Number_sp x) {
  switch (clasp_t_of(x)) {
  case number_Ratio:
    x = gc::As<Ratio_sp>(x)->numerator();
    break;
  case number_Fixnum:
  case number_Bignum:
    break;
  default:
    QERROR_WRONG_TYPE_NTH_ARG(1, x, cl::_sym_Rational_O);
  }
  return x;
}

CL_LAMBDA(x);
CL_DECLARE();
CL_DOCSTRING("denominator");
CL_DEFUN Number_sp cl__denominator(Number_sp x) {
  switch (clasp_t_of(x)) {
  case number_Ratio:
    x = gc::As<Ratio_sp>(x)->denominator();
    break;
  case number_Fixnum:
  case number_Bignum:
    x = clasp_make_fixnum(1);
    break;
  default:
    QERROR_WRONG_TYPE_NTH_ARG(1, x, cl::_sym_Rational_O);
  }
  return x;
}

// Stores the result in quotient, remainder.
void clasp_truncate(Real_sp dividend, Real_sp divisor,
                    Real_sp& quotient, Real_sp& remainder) {
  MATH_DISPATCH_BEGIN(dividend, divisor) {
  case_Fixnum_v_Fixnum: {
      Fixnum a = dividend.unsafe_fixnum();
      Fixnum b = divisor.unsafe_fixnum();
      quotient = clasp_make_fixnum(a / b);
      remainder = clasp_make_fixnum(a % b);
      break;
    }
  case_Fixnum_v_Bignum: {
      // This is always a zero quotient, except when
      // we have MOST_NEGATIVE_FIXNUM / - MOST_NEGATIVE_FIXNUM.
      Fixnum a = dividend.unsafe_fixnum();
      Bignum_sp b = gc::As_unsafe<Bignum_sp>(divisor);
      if ((a == gc::most_negative_fixnum)
          && (b->length() == 1)
          && ((b->limbs())[0] == -gc::most_negative_fixnum)) {
        quotient = clasp_make_fixnum(-1);
        remainder = clasp_make_fixnum(0);
      } else {
        quotient = clasp_make_fixnum(0);
        remainder = dividend;
      }
      break;
    }
  case_Fixnum_v_Ratio:
  case_Bignum_v_Ratio: {
      Ratio_sp ry = gc::As<Ratio_sp>(divisor);
      Real_sp subr;
      Number_sp product = clasp_times(dividend, ry->denominator());
      clasp_truncate(gc::As_unsafe<Real_sp>(product), ry->numerator(),
                     quotient, subr);
      remainder = Rational_O::create(gc::As_unsafe<Integer_sp>(subr),
                                     ry->denominator());
      break;
    }
  case_Fixnum_v_SingleFloat: {
      float n = divisor.unsafe_single_float();
      float p = dividend.unsafe_fixnum() / n;
      float q = std::trunc(p);
      quotient = _clasp_float_to_integer(q);
      remainder = clasp_make_single_float(p * n - q * n);
      break;
    }
  case_Fixnum_v_DoubleFloat: {
      double n = gc::As_unsafe<DoubleFloat_sp>(divisor)->get();
      double p = dividend.unsafe_fixnum() / n;
      double q = std::trunc(p);
      quotient = _clasp_double_to_integer(q);
      remainder = clasp_make_double_float(p * n - q * n);
      break;
    }
#ifdef CLASP_LONG_FLOAT
  case_Fixnum_v_LongFloat: {
      LongFloat n = clasp_long_float(divisor);
      LongFloat p = dividend.unsafe_fixnum() / n;
      LongFloat q = std::trunc(p);
      quotient = _clasp_long_double_to_integer(q);
      remainder = clasp_make_long_float(p * n - q * n);
      break;
    }
#endif
  case_Bignum_v_Fixnum: {
      Bignum_sp bdividend = gc::As_unsafe<Bignum_sp>(dividend);
      Fixnum fdivisor = divisor.unsafe_fixnum();
      T_mv mv = core__next_ftruncate(bdividend, fdivisor);
      quotient = gc::As_unsafe<Integer_sp>(mv);
      remainder = gc::As_unsafe<Integer_sp>(mv.valueGet_(1));
      break;
    }
  case_Bignum_v_Bignum: {
      Bignum_sp bdividend = gc::As_unsafe<Bignum_sp>(dividend);
      Bignum_sp bdivisor = gc::As_unsafe<Bignum_sp>(divisor);
      T_mv mv = core__next_truncate(bdividend, bdivisor);
      quotient = gc::As_unsafe<Integer_sp>(mv);
      remainder = gc::As_unsafe<Integer_sp>(mv.valueGet_(1));
      break;
    }
  // case_Bignum_v_Ratio: above
  case_Bignum_v_SingleFloat: {
      float n = divisor.unsafe_single_float();
      float p = gc::As_unsafe<Bignum_sp>(dividend)->as_float_() / n;
      float q = std::trunc(p);
      quotient = _clasp_float_to_integer(q);
      remainder = clasp_make_single_float(p * n - q * n);
    }
  case_Bignum_v_DoubleFloat: {
      double n = gc::As_unsafe<DoubleFloat_sp>(divisor)->get();
      double p = gc::As_unsafe<Bignum_sp>(dividend)->as_double_() / n;
      double q = std::trunc(p);
      quotient = _clasp_double_to_integer(q);
      remainder = clasp_make_double_float(p * n - q * n);
      break;
    }
#ifdef CLASP_LONG_FLOAT
  case_Bignum_v_LongFloat: {
      LongFloat n = clasp_long_float(divisor);
      LongFloat p = gc::As_unsafe<Bignum_sp>(dividend)->as_long_float_() / n;
      LongFloat q = std::trunc(p);
      quotient = _clasp_long_double_to_integer(q);
      remainder = clasp_make_long_float(p * n - q * n);
      break;
    }
#endif
  case_Ratio_v_Ratio: {
      Ratio_sp rx = gc::As_unsafe<Ratio_sp>(dividend);
      Ratio_sp ry = gc::As_unsafe<Ratio_sp>(divisor);
      Real_sp subr;
      Real_sp c1 = gc::As_unsafe<Real_sp>(clasp_times(rx->numerator(),
                                                      ry->denominator()));
      Real_sp c2 = gc::As_unsafe<Real_sp>(clasp_times(ry->numerator(),
                                                      rx->denominator()));
      Real_sp nd = gc::As_unsafe<Real_sp>(clasp_times(rx->denominator(),
                                                      ry->denominator()));
      clasp_truncate(c1, c2, quotient, subr);
      remainder = Rational_O::create(gc::As_unsafe<Integer_sp>(subr),
                                     gc::As_unsafe<Integer_sp>(nd));
      break;
    }
  case_Ratio_v_Fixnum:
  case_Ratio_v_Bignum:
#ifdef CLASP_LONG_FLOAT
  case_Ratio_v_LongFloat:
#endif
  case_Ratio_v_SingleFloat:
  case_Ratio_v_DoubleFloat: {
      // Given (truncate x (* y z)) = q, r,
      // (truncate x/y z) = q, r/y.
      Ratio_sp rx = gc::As_unsafe<Ratio_sp>(dividend);
      Integer_sp den = rx->denominator();
      Real_sp ndiv = gc::As_unsafe<Real_sp>(clasp_times(den, divisor));
      Real_sp subr;
      clasp_truncate(rx->numerator(), ndiv, quotient, subr);
      remainder = gc::As_unsafe<Real_sp>(clasp_divide(subr, den));
      break;
    }
  case_SingleFloat_v_Fixnum:
  case_SingleFloat_v_Bignum:
  case_SingleFloat_v_SingleFloat:
  case_SingleFloat_v_DoubleFloat:
#ifdef CLASP_LONG_FLOAT
  case_SingleFloat_v_LongFloat:
#endif
  case_SingleFloat_v_Ratio: {
      float n = clasp_to_double(divisor);
      float p = dividend.unsafe_single_float() / n;
      float q = std::trunc(p);
      quotient = _clasp_float_to_integer(q);
      remainder = clasp_make_single_float(p * n - q * n);
      break;
    }
  case_DoubleFloat_v_Fixnum:
  case_DoubleFloat_v_Bignum:
  case_DoubleFloat_v_SingleFloat:
  case_DoubleFloat_v_DoubleFloat:
#ifdef CLASP_LONG_FLOAT
  case_DoubleFloat_v_LongFloat:
#endif
  case_DoubleFloat_v_Ratio: {
      double n = clasp_to_double(divisor);
      double p = gc::As_unsafe<DoubleFloat_sp>(dividend)->get() / n;
      double q = std::trunc(p);
      quotient = _clasp_double_to_integer(q);
      remainder = clasp_make_double_float(p * n - q * n);
      break;
    }
#ifdef CLASP_LONG_FLOAT
  case_LongFloat_v_Fixnum:
  case_LongFloat_v_Bignum:
  case_LongFloat_v_SingleFloat:
  case_LongFloat_v_DoubleFloat:
  case_LongFloat_v_LongFloat: {
      LongFloat n = clasp_to_long_double(divisor);
      LongFloat p = clasp_long_float(dividend) / n;
      LongFloat q = std::trunc(p);
      quotient = _clasp_long_double_to_integer(q);
      remainder = clasp_make_long_float(p * n - q * n);
      break;
    }
#endif
    default: UNREACHABLE();
  };
  MATH_DISPATCH_END();
}

CL_DEFUN Real_mv core__test_truncate(Real_sp dividend, Real_sp divisor) {
  Real_sp quotient, remainder;
  clasp_truncate(dividend, divisor, quotient, remainder);
  return Values(quotient, remainder);
}

Real_mv clasp_floor1(Real_sp x) {
  Real_sp v0, v1;
  switch (clasp_t_of(x)) {
  case number_Fixnum:
  case number_Bignum:
    v0 = x;
    v1 = clasp_make_fixnum(0);
    break;
  case number_Ratio: {
    Ratio_sp rx(gc::As<Ratio_sp>(x));
    Real_mv mv_v0 = clasp_floor2(rx->numerator(), rx->denominator());
    v0 = mv_v0;
    Integer_sp tv1 = gc::As<Integer_sp>(mv_v0.valueGet_(1));
    v1 = clasp_make_ratio(tv1, rx->denominator());
    break;
  }
  case number_SingleFloat: {
    float d = x.unsafe_single_float();
    if (std::isnan(d)) {
      v0 = x;
      v1 = clasp_make_fixnum(0);
    } else {
      float y = floorf(d);
      v0 = _clasp_float_to_integer(y);
      v1 = clasp_make_single_float(d - y);
    }
    break;
  }
  case number_DoubleFloat: {
    double d = gc::As<DoubleFloat_sp>(x)->get();
    if (std::isnan(d)) {
      v0 = x;
      v1 = clasp_make_fixnum(0);
    } else {
      double y = floor(d);
      v0 = _clasp_double_to_integer(y);
      v1 = clasp_make_double_float(d - y);
    }
    break;
  }
#ifdef CLASP_LONG_FLOAT
  case number_LongFloat: {
    LongFloat d = clasp_long_float(x);
    if (std::isnan(d)) {
      v0 = x;
      v1 = clasp_make_fixnum(0);
    } else {
    LongFloat y = floorl(d);
    v0 = _clasp_long_double_to_integer(y);
    v1 = clasp_make_long_float(d - y);
    }
    break;
  }
#endif
  default:
    QERROR_WRONG_TYPE_NTH_ARG(1, x, cl::_sym_Real_O);
  }
  clasp_return2(the_env, v0, v1);
}

Real_mv clasp_floor2(Real_sp x, Real_sp y) {
  Real_sp v0, v1;
  clasp_truncate(x, y, v0, v1);
  if (!(clasp_zerop(v1))
      && (clasp_minusp(y) ? clasp_plusp(x) : clasp_minusp(x)))
    return Values(clasp_one_minus(v0), clasp_plus(v1, y));
  else return Values(v0, v1);
}

CL_LAMBDA(x &optional y);
CL_DECLARE();
CL_DOCSTRING("floor");
CL_DEFUN Real_mv cl__floor(Real_sp x, T_sp y) {
  if (y.nilp())
    return clasp_floor1(x);
  else
    return clasp_floor2(x, gc::As<Real_sp>(y));
}

Real_mv clasp_ceiling1(Real_sp x) {
  Real_sp v0, v1;
  switch (clasp_t_of(x)) {
  case number_Fixnum:
  case number_Bignum:
    v0 = x;
    v1 = clasp_make_fixnum(0);
    break;
  case number_Ratio: {
    //	    const cl_env_ptr the_env = clasp_process_env();
    Ratio_sp rx = gc::As<Ratio_sp>(x);
    Real_mv mv_v = clasp_ceiling2(rx->numerator(), rx->denominator());
    v0 = mv_v;
    Integer_sp tv1 = gc::As<Integer_sp>(mv_v.valueGet_(1));
    v1 = clasp_make_ratio(tv1, rx->denominator());
    break;
  }
  case number_SingleFloat: {
    float d = x.unsafe_single_float();
    float y = ceilf(d);
    v0 = _clasp_float_to_integer(y);
    v1 = clasp_make_single_float(d - y);
    break;
  }
  case number_DoubleFloat: {
    double d = gc::As_unsafe<DoubleFloat_sp>(x)->get();
    double y = ceil(d);
    v0 = _clasp_double_to_integer(y);
    v1 = clasp_make_double_float(d - y);
    break;
  }
#ifdef CLASP_LONG_FLOAT
  case number_LongFloat: {
    LongFloat d = clasp_long_float(x);
    LongFloat y = ceill(d);
    v0 = _clasp_long_double_to_integer(y);
    v1 = clasp_make_long_float(d - y);
    break;
  }
#endif
  default:
    QERROR_WRONG_TYPE_NTH_ARG(1, x, cl::_sym_Real_O);
  }
  return Values(v0, v1);
}

Real_mv clasp_ceiling2(Real_sp x, Real_sp y) {
  Real_sp v0, v1;
  clasp_truncate(x, y, v0, v1);
  if (!(clasp_zerop(v1))
      && (clasp_minusp(y) ? clasp_minusp(x) : clasp_plusp(x)))
    return Values(clasp_one_plus(v0), clasp_minus(v1, y));
  else return Values(v0, v1);
}

CL_LAMBDA(x &optional y);
CL_DECLARE();
CL_DOCSTRING("ceiling");
CL_DEFUN Real_mv cl__ceiling(Real_sp x, T_sp y) {
  if (y.nilp())
    return clasp_ceiling1(x);
  else
    return clasp_ceiling2(x, gc::As<Real_sp>(y));
}

Real_mv clasp_truncate1(Real_sp x) {
  Real_sp v0, v1;
  switch (clasp_t_of(x)) {
  case number_Fixnum:
  case number_Bignum:
    v0 = x;
    v1 = clasp_make_fixnum(0);
    break;
  case number_Ratio: {
    Ratio_sp rx = gc::As<Ratio_sp>(x);
    Real_mv mv_v = clasp_truncate2(rx->numerator(), rx->denominator());
    v0 = mv_v;
    Integer_sp tv1 = gc::As<Integer_sp>(mv_v.valueGet_(1));
    v1 = clasp_make_ratio(tv1, rx->denominator());
    break;
  }
  case number_SingleFloat: {
    float d = x.unsafe_single_float();
    float y = d > 0 ? floorf(d) : ceilf(d);
    v0 = _clasp_float_to_integer(y);
    v1 = clasp_make_single_float(d - y);
    break;
  }
  case number_DoubleFloat: {
    double d = gc::As_unsafe<DoubleFloat_sp>(x)->get();
    double y = d > 0 ? floor(d) : ceil(d);
    v0 = _clasp_double_to_integer(y);
    v1 = clasp_make_double_float(d - y);
    break;
  }
#ifdef CLASP_LONG_FLOAT
  case number_LongFloat: {
    LongFloat d = clasp_long_float(x);
    LongFloat y = d > 0 ? floorl(d) : ceill(d);
    v0 = _clasp_long_double_to_integer(y);
    v1 = clasp_make_long_float(d - y);
    break;
  }
#endif
  default:
    QERROR_WRONG_TYPE_NTH_ARG(1, x, cl::_sym_Real_O);
  }
  clasp_return2(the_env, v0, v1);
}

Real_mv clasp_truncate2(Real_sp x, Real_sp y) {
  Real_sp v0, v1;
  clasp_truncate(x, y, v0, v1);
  return Values(v0, v1);
}

CL_LAMBDA(x &optional y);
CL_DECLARE();
CL_DOCSTRING("truncate");
CL_DEFUN Real_mv cl__truncate(Real_sp x, T_sp y) {
  if (y.nilp())
    return clasp_truncate1(x);
  else
    return clasp_truncate2(x, gc::As<Real_sp>(y));
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

Real_mv clasp_round1(Real_sp x) {
  Real_sp v0, v1;
  switch (clasp_t_of(x)) {
  case number_Fixnum:
  case number_Bignum:
    v0 = x;
    v1 = clasp_make_fixnum(0);
    break;
  case number_Ratio: {
    Ratio_sp rx = gc::As<Ratio_sp>(x);
    Real_mv mv_v = clasp_round2(rx->numerator(), rx->denominator());
    v0 = mv_v;
    Integer_sp tv1 = gc::As<Integer_sp>(mv_v.valueGet_(1));
    v1 = clasp_make_ratio(tv1, rx->denominator());
    break;
  }
  case number_SingleFloat: {
    float d = x.unsafe_single_float();
    float q = round_double(d);
    v0 = _clasp_float_to_integer(q);
    v1 = clasp_make_single_float(d - q);
    break;
  }
  case number_DoubleFloat: {
    double d = gc::As_unsafe<DoubleFloat_sp>(x)->get();
    double q = round_double(d);
    v0 = _clasp_double_to_integer(q);
    v1 = clasp_make_double_float(d - q);
    break;
  }
#ifdef CLASP_LONG_FLOAT
  case number_LongFloat: {
    LongFloat d = clasp_long_float(x);
    LongFloat q = round_long_double(d);
    v0 = _clasp_long_double_to_integer(q);
    v1 = clasp_make_long_float(d - q);
    break;
  }
#endif
  default:
    QERROR_WRONG_TYPE_NTH_ARG(1, x, cl::_sym_Real_O);
  }
  clasp_return2(the_env, v0, v1);
}

Real_mv clasp_round2(Real_sp x, Real_sp y) {
  Real_sp v0, v1;
  Real_sp q;

  q = gc::As<Real_sp>(clasp_divide(x, y));
  switch (clasp_t_of(q)) {
  case number_Fixnum:
  case number_Bignum:
    v0 = q;
    v1 = clasp_make_fixnum(0);
    break;
  case number_Ratio: {
    Ratio_sp rq = gc::As<Ratio_sp>(q);
    Integer_sp q1 = clasp_integer_divide(rq->numerator(), rq->denominator());
    Real_sp r = gc::As<Real_sp>(clasp_minus(q, q1));
    if (clasp_minusp(r)) {
      int c = clasp_number_compare(_lisp->minusHalf(), r);
      if (c > 0 || (c == 0 && clasp_oddp(q1))) {
        q1 = gc::As<Integer_sp>(clasp_one_minus(q1));
      }
    } else {
      int c = clasp_number_compare(r, _lisp->plusHalf());
      if (c > 0 || (c == 0 && clasp_oddp(q1))) {
        q1 = gc::As<Integer_sp>(clasp_one_plus(q1));
      }
    }
    v0 = q1;
    v1 = gc::As<Real_sp>(number_remainder(x, y, q1));
    break;
  }
  default:
    v0 = q = gc::As<Integer_sp>(clasp_round1(q));
    v1 = gc::As<Real_sp>(number_remainder(x, y, q));
  }
  clasp_return2(the_env, v0, v1);
}

CL_LAMBDA(x &optional y);
CL_DECLARE();
CL_DOCSTRING("round");
CL_DEFUN Number_mv cl__round(Real_sp x, T_sp y) {
  if (y.nilp())
    return clasp_round1(x);
  else
    return clasp_round2(x, gc::As<Real_sp>(y));
}

CL_LAMBDA(x y);
CL_DECLARE();
CL_DOCSTRING("mod");
CL_DEFUN Real_sp cl__mod(Real_sp x, Real_sp y) {
  /* INV: #'floor always outputs two values */
  Real_mv mv_v = cl__floor(x, y);
  return gc::As<Real_sp>(mv_v.valueGet_(1));
}

CL_LAMBDA(x y);
CL_DECLARE();
CL_DOCSTRING("rem");
CL_DEFUN Real_sp cl__rem(Real_sp x, Real_sp y) {
  Real_mv mv_v = cl__truncate(x, y);
  return gc::As<Real_sp>(mv_v.valueGet_(1));
}

CL_LAMBDA(x);
CL_DECLARE();
CL_DOCSTRING("decodeFloat");
CL_DEFUN Number_mv cl__decode_float(Float_sp x) {
  int e = 0, s = 0;
  NumberType tx = clasp_t_of(x);
  float f;
  switch (tx) {
  case number_SingleFloat: {
    f = x.unsafe_single_float();
    if (f >= 0.0) {
      s = 1;
    } else {
      f = -f;
      s = 0;
    }
    f = frexpf(f, &e);
    x = clasp_make_single_float(f);
    break;
  }
  case number_DoubleFloat: {
    double d = gc::As_unsafe<DoubleFloat_sp>(x)->get();
    if (d >= 0.0) {
      s = 1;
    } else {
      d = -d;
      s = 0;
    }
    d = frexp(d, &e);
    x = clasp_make_double_float(d);
    break;
  }
#ifdef CLASP_LONG_FLOAT
  case number_LongFloat: {
    LongFloat d = clasp_long_float(x);
    if (d >= 0.0)
      s = 1;
    else {
      d = -d;
      s = 0;
    }
    d = frexpl(d, &e);
    x = clasp_make_long_float(d);
    break;
  }
#endif
  default:
    QERROR_WRONG_TYPE_NTH_ARG(1, x, cl::_sym_float);
  }
  return Values(x, clasp_make_fixnum(e), clasp_make_single_float(s));
}

CL_LAMBDA(x y);
CL_DECLARE();
CL_DOCSTRING("scaleFloat");
CL_DEFUN Number_sp cl__scale_float(Number_sp x, Number_sp y) {
  Fixnum k;
  if (CLASP_FIXNUMP(y)) {
    k = y.unsafe_fixnum();
  } else {
    QERROR_WRONG_TYPE_NTH_ARG(2, y, cl::_sym_fixnum);
  }
  switch (clasp_t_of(x)) {
  case number_SingleFloat:
    x = clasp_make_single_float(ldexpf(x.unsafe_single_float(), k));
    break;
  case number_DoubleFloat:
      x = clasp_make_double_float(ldexp(gc::As_unsafe<DoubleFloat_sp>(x)->get(), k));
    break;
#ifdef CLASP_LONG_FLOAT
  case number_LongFloat:
    x = clasp_make_long_float(ldexpl(clasp_long_float(x), k));
    break;
#endif
  default:
    QERROR_WRONG_TYPE_NTH_ARG(1, x, cl::_sym_float);
  }
  return x;
}

Integer_sp cl__float_radix(Float_sp x) {
  return clasp_make_fixnum(FLT_RADIX);
}

int clasp_signbit(Number_sp x) {
  switch (clasp_t_of(x)) {
  case number_SingleFloat:
    return std::signbit(x.unsafe_single_float());
  case number_DoubleFloat:
      return std::signbit(gc::As_unsafe<DoubleFloat_sp>(x)->get());
#ifdef CLASP_LONG_FLOAT
  case number_LongFloat:
    return signbit(clasp_long_float(x));
#endif
  default:
    QERROR_WRONG_TYPE_NTH_ARG(1, x, cl::_sym_float);
  }
  SIMPLE_ERROR(BF("Illegal argument for clasp_signbit: %s") % _rep_(x));
}

CL_LAMBDA(x &optional (y x yp));
CL_DECLARE();
CL_DOCSTRING("floatSign");
CL_DEFUN Float_sp cl__float_sign(Float_sp x, Float_sp y, T_sp yp) {
  int negativep;
  if (yp.nilp()) {
    y = cl__float(clasp_make_fixnum(1), x);
  }
  negativep = clasp_signbit(x);
  switch (clasp_t_of(y)) {
  case number_SingleFloat: {
    float f = y.unsafe_single_float();
    if (std::signbit(f) != negativep)
      y = clasp_make_single_float(-f);
    break;
  }
  case number_DoubleFloat: {
    double f = gc::As_unsafe<DoubleFloat_sp>(y)->get();
    if (std::signbit(f) != negativep)
      y = clasp_make_double_float(-f);
    break;
  }
#ifdef CLASP_LONG_FLOAT
  case number_LongFloat: {
    LongFloat f = clasp_long_float(y);
    if (std::signbit(f) != negativep)
      y = clasp_make_long_float(-f);
    break;
  }
#endif
  default:
    QERROR_WRONG_TYPE_NTH_ARG(2, y, cl::_sym_float);
  }
  return y;
}

CL_LAMBDA(x);
CL_DECLARE();
CL_DOCSTRING("floatDigits");
CL_DEFUN Integer_sp cl__float_digits(Float_sp x) {
  Integer_sp ix(_Nil<Integer_O>());
  switch (clasp_t_of(x)) {
  case number_SingleFloat:
    ix = clasp_make_fixnum(FLT_MANT_DIG);
    break;
  case number_DoubleFloat:
    ix = clasp_make_fixnum(DBL_MANT_DIG);
    break;
#ifdef CLASP_LONG_FLOAT
  case number_LongFloat:
    ix = clasp_make_fixnum(LDBL_MANT_DIG);
    break;
#endif
  default:
    QERROR_WRONG_TYPE_NTH_ARG(1, x, cl::_sym_float);
  }
  return ix;
}

CL_LAMBDA(value);
CL_DECLARE();
CL_DOCSTRING("floatPrecision");
CL_DEFUN Integer_sp cl__float_precision(Float_sp x) {
  int precision = 0;
  switch (clasp_t_of(x)) {
  case number_SingleFloat: {
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
  case number_DoubleFloat: {
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
  case number_LongFloat: {
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
    QERROR_WRONG_TYPE_NTH_ARG(1, x, cl::_sym_float);
  }
  return clasp_make_fixnum(precision);
}

CL_LAMBDA(x);
CL_DECLARE();
CL_DOCSTRING("integer_decode_float");
CL_DEFUN Real_mv cl__integer_decode_float(Float_sp x) {
  int e = 0, s = 1;
  Real_sp rx(_Nil<Real_O>());
  switch (clasp_t_of(x)) {
#ifdef CLASP_LONG_FLOAT
  case number_LongFloat: {
    LongFloat d = clasp_long_float(x);
    if (std::signbit(d)) {
      s = -1;
      d = -d;
    }
    if (d == 0.0) {
      e = 0;
      rx = clasp_make_fixnum(0);
    } else {
      d = frexpl(d, &e);
      rx = _clasp_long_double_to_integer(ldexpl(d, LDBL_MANT_DIG));
      e -= LDBL_MANT_DIG;
    }
    break;
  }
#endif
  case number_DoubleFloat: {
    double d = gc::As_unsafe<DoubleFloat_sp>(x)->get();
    if (std::signbit(d)) {
      s = -1;
      d = -d;
    }
    if (d == 0.0) {
      e = 0;
      rx = clasp_make_fixnum(0);
    } else {
      d = frexp(d, &e);
      rx = _clasp_double_to_integer(ldexp(d, DBL_MANT_DIG));
      e -= DBL_MANT_DIG;
    }
    break;
  }
  case number_SingleFloat: {
    float d = x.unsafe_single_float();
    if (std::signbit(d)) {
      s = -1;
      d = -d;
    }
    if (d == 0.0) {
      e = 0;
      rx = clasp_make_fixnum(0);
    } else {
      d = frexpf(d, &e);
      rx = _clasp_double_to_integer(ldexp(d, FLT_MANT_DIG));
      e -= FLT_MANT_DIG;
    }
    break;
  }
  default:
    QERROR_WRONG_TYPE_NTH_ARG(1, x, cl::_sym_float);
  }
  ASSERT(rx.notnilp());
  return Values(rx, clasp_make_fixnum(e), clasp_make_fixnum(s));
}

CL_LAMBDA(r &optional (i 0));
CL_DECLARE();
CL_DOCSTRING("complex");
CL_DEFUN Complex_sp cl__complex(Real_sp r, Real_sp i) {
  return gc::As_unsafe<Complex_sp>(clasp_make_complex(r, i));
}

CL_LAMBDA(x);
CL_DECLARE();
CL_DOCSTRING("realpart");
CL_DEFUN Real_sp cl__realpart(Number_sp x) {
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
  case number_Complex: {
    x = gc::As<Complex_sp>(x)->real();
    break;
  }
  default:
    QERROR_WRONG_TYPE_NTH_ARG(1, x, cl::_sym_Number_O);
  }
  return gc::As<Real_sp>(x);
}

CL_LAMBDA(x);
CL_DECLARE();
CL_DOCSTRING("imagpart");
CL_DEFUN Real_sp cl__imagpart(Number_sp x) {
  switch (clasp_t_of(x)) {
  case number_Fixnum:
  case number_Bignum:
  case number_Ratio:
    x = clasp_make_fixnum(0);
    break;
  case number_SingleFloat:
    if (std::signbit(x.unsafe_single_float()))
      x = _lisp->singleFloatMinusZero();
    else
      x = _lisp->singleFloatPlusZero();
    break;
  case number_DoubleFloat:
      if (std::signbit(gc::As_unsafe<DoubleFloat_sp>(x)->get()))
      x = _lisp->doubleFloatMinusZero();
    else
      x = _lisp->doubleFloatPlusZero();
    break;
#ifdef CLASP_LONG_FLOAT
  case number_LongFloat:
    if (std::signbit(clasp_long_float(x)))
      x = _lisp->longFloatMinusZero();
    else
      x = _lisp->longFloatPlusZero();
    break;
#endif
  case number_Complex:
    x = gc::As<Complex_sp>(x)->imaginary();
    break;
  default:
    QERROR_WRONG_TYPE_NTH_ARG(1, x, cl::_sym_Number_O);
  }
  return gc::As<Real_sp>(x);
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

};
