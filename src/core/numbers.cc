/*
    File: numbers.cc
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
// #define DEBUG_LEVEL_FULL

#define FAST_FIXNUM_ARITH
// #include "clasp_gmpxx.h"
#include <cstdint>

#include <clasp/core/foundation.h>
#include <clasp/core/primitives.h> // core__list_from_vaslist
#include <clasp/core/common.h>
#include <clasp/core/numbers.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/symbol.h>
#include <clasp/core/bignum.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/cleavirPrimopsPackage.fwd.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/num_arith.h>
#include <clasp/gctools/pointer_tagging.h>
#include <clasp/core/wrappers.h>
#include <clasp/core/num_co.h>

namespace core {

core::Fixnum not_fixnum_error(core::T_sp o) { TYPE_ERROR(o, cl::_sym_fixnum); }

[[noreturn]] void not_comparable_error(Number_sp na, Number_sp nb) {
  string naclass, nbclass;
  if (na.fixnump())
    naclass = "FIXNUM";
  else if (na.single_floatp())
    naclass = "SINGLE-FLOAT";
  else
    naclass = na->_instanceClass()->_classNameAsString();
  if (nb.fixnump())
    nbclass = "FIXNUM";
  else if (nb.single_floatp())
    nbclass = "SINGLE-FLOAT";
  else
    nbclass = nb->_instanceClass()->_classNameAsString();

  SIMPLE_ERROR("Numbers of class {} and {} are not commensurable, or operation is unimplemented", naclass, nbclass);
}

void clasp_report_divide_by_zero(Number_sp x) { ERROR_DIVISION_BY_ZERO(clasp_make_fixnum(1), x); }

Number_sp clasp_make_complex(Real_sp r, Real_sp i) {
  if (i.fixnump() && cl__rationalp(r)) {
    if (i.unsafe_fixnum() == 0)
      return r;
    return Complex_O::create(r, i);
  }

  if (!cl__floatp(r) && !cl__floatp(i))
    return Complex_O::create(r, i);

  if (r.single_floatp()) {
    if (i.isA<DoubleFloat_O>())
      return Complex_O::create(DoubleFloat_O::coerce(r), i);
    if (i.isA<LongFloat_O>())
      return Complex_O::create(LongFloat_O::coerce(r), i);
    return Complex_O::create(r, SingleFloat_dummy_O::coerce(i));
  }

  if (r.isA<DoubleFloat_O>()) {
    if (i.isA<LongFloat_O>())
      return Complex_O::create(LongFloat_O::coerce(r), i);
    return Complex_O::create(r, DoubleFloat_O::coerce(i));
  }

  if (r.isA<LongFloat_O>()) {
    return Complex_O::create(r, LongFloat_O::coerce(i));
  }

  if (i.single_floatp())
    return Complex_O::create(SingleFloat_dummy_O::coerce(r), i);
  if (i.isA<DoubleFloat_O>())
    return Complex_O::create(DoubleFloat_O::coerce(r), i);
  if (i.isA<LongFloat_O>())
    return Complex_O::create(LongFloat_O::coerce(r), i);

  return Complex_O::create(r, i);
}

CL_LAMBDA(num);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(zerop)dx");
DOCGROUP(clasp);
CL_DEFUN bool cl__zerop(Number_sp num) { return Number_O::zerop(num); }

CL_LAMBDA(num);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(minusp)dx");
DOCGROUP(clasp);
CL_DEFUN bool cl__minusp(Real_sp num) { return Real_O::minusp(num); }

CL_LAMBDA(num);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(plusp)dx");
DOCGROUP(clasp);
CL_DEFUN bool cl__plusp(Real_sp num) { return Real_O::plusp(num); }

CL_LAMBDA(num);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(evenp)dx");
DOCGROUP(clasp);
CL_DEFUN bool cl__evenp(Integer_sp num) { return Integer_O::evenp(num); }

CL_LAMBDA(num);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(oddp)dx");
DOCGROUP(clasp);
CL_DEFUN bool cl__oddp(Integer_sp num) { return Integer_O::oddp(num); }

CL_LAMBDA();
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(fixnum_number_of_bits)dx");
DOCGROUP(clasp);
CL_DEFUN Fixnum_sp core__fixnum_number_of_bits() {
  int num = gc::fixnum_bits;
  return make_fixnum(num);
};

Real_sp clasp_max2(Real_sp x, Real_sp y) {
  Real_sp max = x;
  if (Number_O::compare(max, y) < 0)
    max = y;
  return max;
}

Real_sp clasp_min2(Real_sp x, Real_sp y) {
  Real_sp min = x;
  if (Number_O::compare(min, y) > 0)
    min = y;
  return min;
}

CL_LAMBDA(min &rest nums);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(min)dx");
DOCGROUP(clasp);
CL_DEFUN Real_sp cl__min(Real_sp min, List_sp nums) {
  /* INV: type check occurs in Number_O::compare() for the rest of
           numbers, but for the first argument it's due to the Real_sp decl
           above. */
  for (auto cur : nums) {
    Real_sp numi = gc::As<Real_sp>(oCar(cur));
    min = clasp_min2(min, numi);
  }
  return min;
}

CL_LAMBDA(max &rest nums);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(max)dx");
DOCGROUP(clasp);
CL_DEFUN Real_sp cl__max(Real_sp max, List_sp nums) {
  /* INV: type checks the same as cl__min. */
  for (auto cur : nums) {
    Real_sp numi = gc::As<Real_sp>(oCar(cur));
    max = clasp_max2(max, numi);
  }
  return max;
}

CL_NAME("TWO-ARG-+-FIXNUM-FIXNUM");
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN Number_sp two_arg__PLUS_FF(Fixnum fa, Fixnum fb) { return Integer_O::create(static_cast<gc::Fixnum>(fa + fb)); }

CL_NAME("TWO-ARG-+");
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN Number_sp Number_O::add_nn(Number_sp na, Number_sp nb) {
  Complex_sp ca = na.asOrNull<Complex_O>(), cb = nb.asOrNull<Complex_O>();
  if (ca && cb)
    return clasp_make_complex(ca->real() + cb->real(), ca->imaginary() + cb->imaginary());
  if (ca)
    return clasp_make_complex(ca->real() + nb, ca->imaginary());
  if (cb)
    return clasp_make_complex(na + cb->real(), cb->imaginary());

#ifdef CLASP_LONG_FLOAT
  if (na.isA<LongFloat_O>() || nb.isA<LongFloat_O>())
    return LongFloat_O::create(as_long_float(na) + as_long_float(nb));
#endif

  if (na.isA<DoubleFloat_O>() || nb.isA<DoubleFloat_O>())
    return DoubleFloat_O::create(as_double_float(na) + as_double_float(nb));

  if (na.single_floatp() || nb.single_floatp())
    return SingleFloat_dummy_O::create(as_single_float(na) + as_single_float(nb));

#ifdef CLASP_SHORT_FLOAT
  if (na.short_floatp() || nb.short_floatp())
    return ShortFloat_O::create(as_short_float(na) + as_short_float(nb));
#endif

  Ratio_sp ra = na.asOrNull<Ratio_O>(), rb = nb.asOrNull<Ratio_O>();
  if (ra && rb)
    return Rational_O::create(ra->numerator() * rb->denominator() + ra->denominator() * rb->numerator(),
                              ra->denominator() * rb->denominator());
  if (ra)
    return Ratio_O::create(ra->numerator() + nb * ra->denominator(), ra->denominator());
  if (rb)
    return Ratio_O::create(na * rb->denominator() + rb->numerator(), rb->denominator());

  Bignum_sp ba = na.asOrNull<Bignum_O>(), bb = nb.asOrNull<Bignum_O>();
  if (ba && bb)
    return add_bb(ba, bb);
  if (ba)
    return add_bx(ba, nb.unsafe_fixnum());
  if (bb)
    return add_bx(bb, na.unsafe_fixnum());

  return Integer_O::create(na.unsafe_fixnum() + nb.unsafe_fixnum());
};

CL_NAME("TWO-ARG--");
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN Number_sp Number_O::sub_nn(Number_sp na, Number_sp nb) {
  Complex_sp ca = na.asOrNull<Complex_O>(), cb = nb.asOrNull<Complex_O>();
  if (ca && cb)
    return clasp_make_complex(ca->real() - cb->real(), ca->imaginary() - cb->imaginary());
  if (ca)
    return clasp_make_complex(ca->real() - nb, ca->imaginary());
  if (cb)
    return clasp_make_complex(na - cb->real(), -cb->imaginary());

#ifdef CLASP_LONG_FLOAT
  if (na.isA<LongFloat_O>() || nb.isA<LongFloat_O>())
    return LongFloat_O::create(as_long_float(na) - as_long_float(nb));
#endif

  if (na.isA<DoubleFloat_O>() || nb.isA<DoubleFloat_O>())
    return DoubleFloat_O::create(as_double_float(na) - as_double_float(nb));

  if (na.single_floatp() || nb.single_floatp())
    return SingleFloat_dummy_O::create(as_single_float(na) - as_single_float(nb));

#ifdef CLASP_SHORT_FLOAT
  if (na.short_floatp() || nb.short_floatp())
    return ShortFloat_O::create(as_short_float(na) - as_short_float(nb));
#endif

  Ratio_sp ra = na.asOrNull<Ratio_O>(), rb = nb.asOrNull<Ratio_O>();
  if (ra && rb)
    return Rational_O::create(ra->numerator() * rb->denominator() - ra->denominator() * rb->numerator(),
                              ra->denominator() * rb->denominator());
  if (ra)
    return Ratio_O::create(ra->numerator() - nb * ra->denominator(), ra->denominator());
  if (rb)
    return Ratio_O::create(na * rb->denominator() - rb->numerator(), rb->denominator());

  Bignum_sp ba = na.asOrNull<Bignum_O>(), bb = nb.asOrNull<Bignum_O>();
  if (ba && bb)
    return sub_bb(ba, bb);
  if (ba)
    return add_bx(ba, -nb.unsafe_fixnum());
  if (bb)
    return sub_xb(na.unsafe_fixnum(), bb);

  return Integer_O::create(na.unsafe_fixnum() - nb.unsafe_fixnum());
}

CL_NAME("TWO-ARG-*");
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN Number_sp Number_O::mul_nn(Number_sp na, Number_sp nb) {
  Complex_sp ca = na.asOrNull<Complex_O>(), cb = nb.asOrNull<Complex_O>();
  if (ca && cb)
    return clasp_make_complex(ca->real() * cb->real() - ca->imaginary() * cb->imaginary(),
                              ca->real() * cb->imaginary() + ca->imaginary() * cb->real());
  if (ca)
    return clasp_make_complex(ca->real() * nb, ca->imaginary() * nb);
  if (cb)
    return clasp_make_complex(na * cb->real(), na * cb->imaginary());

#ifdef CLASP_LONG_FLOAT
  if (na.isA<LongFloat_O>() || nb.isA<LongFloat_O>())
    return LongFloat_O::create(as_long_float(na) * as_long_float(nb));
#endif

  if (na.isA<DoubleFloat_O>() || nb.isA<DoubleFloat_O>())
    return DoubleFloat_O::create(as_double_float(na) * as_double_float(nb));

  if (na.single_floatp() || nb.single_floatp())
    return SingleFloat_dummy_O::create(as_single_float(na) * as_single_float(nb));

#ifdef CLASP_SHORT_FLOAT
  if (na.short_floatp() || nb.short_floatp())
    return ShortFloat_O::create(as_short_float(na) * as_short_float(nb));
#endif

  Ratio_sp ra = na.asOrNull<Ratio_O>(), rb = nb.asOrNull<Ratio_O>();
  if (ra && rb)
    return Rational_O::create(ra->numerator() * rb->numerator(), ra->denominator() * rb->denominator());
  if (ra)
    return Rational_O::create(ra->numerator() * nb, ra->denominator());
  if (rb)
    return Rational_O::create(na * rb->numerator(), rb->denominator());

  Bignum_sp ba = na.asOrNull<Bignum_O>(), bb = nb.asOrNull<Bignum_O>();
  if (ba && bb)
    return mul_bb(ba, bb);
  if (ba)
    return mul_bx(ba, nb.unsafe_fixnum());
  if (bb)
    return mul_bx(bb, na.unsafe_fixnum());

  // We want to detect when Fixnum * Fixnum multiplication will overflow and only then use bignum arithmetic.
  // But C++ doesn't give us a way to do that - so we use the __builtin_mul_overflow clang builtin.
  // It will return false if there is no overflow and the multiplication result will be in fr.
  // The return value fr may over
  // If it doesn't overflow - then this will be faster than always using bignum arithmetic.
  Fixnum fa = na.unsafe_fixnum(), fb = nb.unsafe_fixnum(), fr;
  bool overflow = __builtin_mul_overflow(fa, fb, &fr);
  if (!overflow)
    return Integer_O::create(fr);
  return core__mul_fixnums(fa, fb);
}

CL_NAME("TWO-ARG-/");
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN Number_sp Number_O::div_nn(Number_sp na, Number_sp nb) {
  Complex_sp ca = na.asOrNull<Complex_O>(), cb = nb.asOrNull<Complex_O>();
  if (ca && cb) {
    Number_sp den = cb->real() * cb->real() + cb->imaginary() * cb->imaginary();
    return clasp_make_complex((ca->real() * cb->real() + ca->imaginary() * cb->imaginary()) / den,
                              (ca->imaginary() * cb->real() - ca->real() * cb->imaginary()) / den);
  }
  if (ca)
    return clasp_make_complex(ca->real() / nb, ca->imaginary() / nb);
  if (cb) {
    Number_sp den = cb->real() * cb->real() + cb->imaginary() * cb->imaginary();
    return clasp_make_complex((na * cb->real()) / den, -(na * cb->imaginary()) / den);
  }

#ifdef CLASP_LONG_FLOAT
  if (na.isA<LongFloat_O>() || nb.isA<LongFloat_O>())
    return LongFloat_O::create(as_long_float(na) / as_long_float(nb));
#endif

  if (na.isA<DoubleFloat_O>() || nb.isA<DoubleFloat_O>())
    return DoubleFloat_O::create(as_double_float(na) / as_double_float(nb));

  if (na.single_floatp() || nb.single_floatp())
    return SingleFloat_dummy_O::create(as_single_float(na) / as_single_float(nb));

#ifdef CLASP_SHORT_FLOAT
  if (na.short_floatp() || nb.short_floatp())
    return ShortFloat_O::create(as_short_float(na) / as_short_float(nb));
#endif

  Ratio_sp ra = na.asOrNull<Ratio_O>(), rb = nb.asOrNull<Ratio_O>();
  if (ra && rb)
    return Rational_O::create(ra->numerator() * rb->denominator(), ra->denominator() * rb->numerator());
  if (ra)
    return Rational_O::create(ra->numerator(), ra->denominator() * nb);
  if (rb)
    return Rational_O::create(na * rb->denominator(), rb->numerator());

  return Rational_O::create(na, nb);
}

CL_LAMBDA(&rest numbers);
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN Number_sp cl___PLUS_(List_sp numbers) {
  if (!numbers.consp())
    return make_fixnum(0);
  Number_sp result = gc::As<Number_sp>(oCar(numbers));
  for (auto cur : (List_sp)oCdr(numbers)) {
    result += gc::As<Number_sp>(oCar(cur));
  }
  return result;
}

CL_LAMBDA(&rest numbers);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(See CLHS: *)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp cl___TIMES_(List_sp numbers) {
  if (!numbers.consp())
    return make_fixnum(1);
  Number_sp result = gc::As<Number_sp>(oCar(numbers));
  for (auto cur : (List_sp)oCdr(numbers)) {
    result *= gc::As<Number_sp>(oCar(cur));
  }
  return result;
}

CL_LAMBDA(num &rest numbers);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(See CLHS: +)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp cl___MINUS_(Number_sp num, List_sp numbers) {
  if (!numbers.consp()) {
    return clasp_negate(num);
  }
  Number_sp result = num;
  for (auto cur : (List_sp)(numbers)) {
    result -= gc::As<Number_sp>(oCar(cur));
  }
  return result;
}

CL_LAMBDA(num &rest numbers);
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN Number_sp cl___DIVIDE_(Number_sp num, List_sp numbers) {
  if (!numbers.consp()) {
    return clasp_reciprocal(num);
  }
  Number_sp result = num;
  for (auto cur : (List_sp)(numbers)) {
    result /= gc::As<Number_sp>(oCar(cur));
  }
  return result;
}

/* ----------------------------------------------------------------------

   fix_compare

*/
/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    num_comp.c  -- Comparisons on numbers.
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

/* ----------------------------------------------------------------------

   Number_O::compare

*/

template <std::floating_point Float> int compare_bignum_float(Bignum_sp x, Float y) {
  constexpr size_t limb_width = 8 * sizeof(mp_limb_t);
  auto q = float_convert<Float>::float_to_quadruple(y);

  if (q.category != float_convert<Float>::category::finite)
    return q.sign;

  bool negative = Real_O::minusp(x);

  if (negative && (q.significand == 0 || q.sign > 0))
    return -1;

  if (!negative && (q.significand == 0 || q.sign < 0))
    return 1;

  int64_t xlen = clasp_integer_length(x);
  int64_t ylen = std::bit_width(q.significand) + q.exponent;

  if (xlen < ylen)
    return -q.sign;

  if (xlen > ylen)
    return q.sign;

  const mp_limb_t* limbs = x->limbs();

  size_t width = std::bit_width(q.significand);
  typename float_convert<Float>::uint_t xsig = 0;
  bool first = true;

  for (mp_size_t i = std::abs(x->length()) - 1; i > -1; i--) {
    mp_limb_t z = limbs[i];

    if (width > 0) {
      auto w = first ? std::bit_width(z) : limb_width;
      auto shift = std::min(w, width);
      xsig = (xsig << shift) | (z >> (w - shift));
      z &= (mp_limb_t{1} << (w - shift)) - mp_limb_t{1};
      width -= shift;
      first = false;

      // if (width == 0)
      //   fmt::print("{} {}\n", xsig, q.significand);

      if (width == 0 && xsig < q.significand)
        return -q.sign;
      if (width == 0 && xsig > q.significand)
        return q.sign;
    }

    if (width == 0 && z != 0)
      return q.sign;
  }

  return 0;
}

template <typename T> inline int compare_pod(T x, T y) {
  if (x < y)
    return -1;
  if (x > y)
    return 1;
  return 0;
}

template <std::floating_point Float> inline int compare_fixnum_float(Fixnum a, Float b) {
  // We can't use C's comparison because it promotes ints to floats,
  // which is the opposite of how CL is defined.
  // If b is out of range, this is easy. Also covers infinities.
  // We do this instead of the more obvious most_positive_fixnum
  // comparison mpf, being not-a-power-of-two, cannot be exactly
  // represented and clang whines about that.
  if (b > ((gc::Fixnum)1 << gc::fixnum_bits))
    return -1;
  else if (b < -((gc::Fixnum)1 << gc::fixnum_bits))
    return 1;

  gctools::Fixnum ib = b; // per C std, truncates (towards zero).
  if (a < ib)
    return -1;
  else if (a > ib)
    return 1;
  // a == trunc(b), so we have to check on b's frac part.
  // examples: 3.2 3, -3.2 -3, -0.2 0, 0.2 0, 0.0 0
  else if (b > ib)
    return -1;
  else if (b < ib)
    return 1;
  else
    return 0;
}

template <std::floating_point Float> inline int compare_rational_float(Ratio_sp x, Float y) {
  if (std::isinf(y))
    return std::signbit(y) ? 1 : -1;
  return Number_O::compare(x, float_to_rational(y));
}

/*! Return -1 if a<b
      0 if a == b
      +1 if a > b
    */
int Number_O::compare(const Real_sp na, const Real_sp nb) {
  Ratio_sp ra = na.asOrNull<Ratio_O>(), rb = nb.asOrNull<Ratio_O>();
  if (ra && rb) {
    // First, divide through the ratios and compare those.
    // That can give us an answer not requiring consing larger numbers.
    // Failing that, use a/b <=> c/d is equivalent to ad <=> bc.
    Integer_sp ta = clasp_integer_divide(ra->numerator(), ra->denominator());
    Integer_sp tb = clasp_integer_divide(rb->numerator(), rb->denominator());
    int res = compare(ta, tb);
    if (res != 0)
      return res;
    else {
      return compare(ra->numerator() * rb->denominator(), rb->numerator() * ra->denominator());
    }
  }
  if (ra) {
#ifdef CLASP_LONG_FLOAT
    if (nb.isA<LongFloat_O>())
      return compare_rational_float(ra, nb.as_unsafe<LongFloat_O>()->get());
#endif
    if (nb.isA<DoubleFloat_O>())
      return compare_rational_float(ra, nb.as_unsafe<DoubleFloat_O>()->get());
    if (nb.single_floatp())
      return compare_rational_float(ra, nb.unsafe_single_float());
#ifdef CLASP_SHORT_FLOAT
    if (nb.short_floatp())
      return compare_rational_float(ra, nb.unsafe_short_float());
#endif
    Integer_sp trunc = clasp_integer_divide(ra->numerator(), ra->denominator());
    int res = compare(trunc, nb);
    if (res == 0)
      return (Real_O::minusp(ra) ? -1 : 1);
    else
      return res;
  }
  if (rb) {
#ifdef CLASP_LONG_FLOAT
    if (na.isA<LongFloat_O>())
      return -compare_rational_float(rb, na.as_unsafe<LongFloat_O>()->get());
#endif
    if (na.isA<DoubleFloat_O>())
      return -compare_rational_float(rb, na.as_unsafe<DoubleFloat_O>()->get());
    if (na.single_floatp())
      return -compare_rational_float(rb, na.unsafe_single_float());
#ifdef CLASP_SHORT_FLOAT
    if (na.short_floatp())
      return -compare_rational_float(rb, na.unsafe_short_float());
#endif
    Integer_sp trunc = clasp_integer_divide(rb->numerator(), rb->denominator());
    int res = compare(na, trunc);
    if (res == 0)
      return (Real_O::minusp(rb) ? 1 : -1);
    else
      return res;
  }

  Bignum_sp ba = na.asOrNull<Bignum_O>(), bb = nb.asOrNull<Bignum_O>();
  if (ba && bb)
    return core__next_compare(ba, bb);
  if (ba) {
#ifdef CLASP_LONG_FLOAT
    if (nb.isA<LongFloat_O>())
      return compare_bignum_float(ba, nb.as_unsafe<LongFloat_O>()->get());
#endif
    if (nb.isA<DoubleFloat_O>())
      return compare_bignum_float(ba, nb.as_unsafe<DoubleFloat_O>()->get());
    if (nb.single_floatp())
      return compare_bignum_float(ba, nb.unsafe_single_float());
#ifdef CLASP_SHORT_FLOAT
    if (nb.short_floatp())
      return compare_bignum_float(ba, nb.unsafe_short_float());
#endif
    if (ba->plusp_())
      return 1;
    else
      return -1;
  }
  if (bb) {
#ifdef CLASP_LONG_FLOAT
    if (na.isA<LongFloat_O>())
      return -compare_bignum_float(bb, na.as_unsafe<LongFloat_O>()->get());
#endif
    if (na.isA<DoubleFloat_O>())
      return -compare_bignum_float(bb, na.as_unsafe<DoubleFloat_O>()->get());
    if (na.single_floatp())
      return -compare_bignum_float(bb, na.unsafe_single_float());
#ifdef CLASP_SHORT_FLOAT
    if (na.short_floatp())
      return -compare_bignum_float(bb, na.unsafe_short_float());
#endif
    if (bb->minusp_())
      return 1;
    else
      return -1;
  }

  if (na.fixnump() && nb.fixnump())
    return compare_pod(na.unsafe_fixnum(), nb.unsafe_fixnum());
  if (na.fixnump()) {
#ifdef CLASP_LONG_FLOAT
    if (nb.isA<LongFloat_O>())
      return compare_fixnum_float(na.unsafe_fixnum(), nb.as_unsafe<LongFloat_O>()->get());
#endif
    if (nb.isA<DoubleFloat_O>())
      return compare_fixnum_float(na.unsafe_fixnum(), nb.as_unsafe<DoubleFloat_O>()->get());
    if (nb.single_floatp())
      return compare_fixnum_float(na.unsafe_fixnum(), nb.unsafe_single_float());
#ifdef CLASP_SHORT_FLOAT
    if (nb.short_floatp())
      return compare_fixnum_float(na.unsafe_fixnum(), nb.unsafe_short_float());
#endif
    not_comparable_error(na, nb);
  }
  if (nb.fixnump()) {
#ifdef CLASP_LONG_FLOAT
    if (na.isA<LongFloat_O>())
      return -compare_fixnum_float(nb.unsafe_fixnum(), na.as_unsafe<LongFloat_O>()->get());
#endif
    if (na.isA<DoubleFloat_O>())
      return -compare_fixnum_float(nb.unsafe_fixnum(), na.as_unsafe<DoubleFloat_O>()->get());
    if (na.single_floatp())
      return -compare_fixnum_float(nb.unsafe_fixnum(), na.unsafe_single_float());
#ifdef CLASP_SHORT_FLOAT
    if (na.short_floatp())
      return -compare_fixnum_float(nb.unsafe_fixnum(), na.unsafe_short_float());
#endif
    not_comparable_error(na, nb);
  }

#ifdef CLASP_LONG_FLOAT
  if (na.isA<LongFloat_O>() || nb.isA<LongFloat_O>())
    return compare_pod(Number_O::as_long_float(na), Number_O::as_long_float(nb));
#endif
  if (na.isA<DoubleFloat_O>() || nb.isA<DoubleFloat_O>())
    return compare_pod(Number_O::as_double_float(na), Number_O::as_double_float(nb));
  if (na.single_floatp() || nb.single_floatp())
    return compare_pod(Number_O::as_single_float(na), Number_O::as_single_float(nb));
#ifdef CLASP_SHORT_FLOAT
  if (na.short_floatp() || nb.short_floatp())
    return compare_pod(Number_O::as_short_float(na), Number_O::as_short_float(nb));
#endif

  not_comparable_error(na, nb);
}

T_sp numbers_monotonic_vaslist(int s, int t, Vaslist_sp args) {
  Real_sp c = gc::As<Real_sp>(args->next_arg());
  Real_sp d;
  int dir;
  while (args->nargs() > 0) {
    d = gc::As<Real_sp>(args->next_arg());
    dir = s * Number_O::compare(c, d);
    if (dir < t)
      return _lisp->_false();
    c = d;
  }
  return _lisp->_true();
};

CL_NAME("TWO-ARG-<");
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN bool two_arg__LT_(Number_sp x, Number_sp y) { return Number_O::compare(x, y) == -1; }

CL_NAME("TWO-ARG-<=");
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN bool two_arg__LE_(Number_sp x, Number_sp y) { return Number_O::compare(x, y) != 1; }

CL_NAME("TWO-ARG->");
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN bool two_arg__GT_(Number_sp x, Number_sp y) { return Number_O::compare(x, y) == 1; }

CL_NAME("TWO-ARG->=");
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN bool two_arg__GE_(Number_sp x, Number_sp y) { return Number_O::compare(x, y) != -1; }

CL_LAMBDA(core:&va-rest args);
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN T_sp cl___LT_(Vaslist_sp args) {
  if (args->nargs_zero()) {
    SIMPLE_ERROR("< needs at least one argument");
  }
  return numbers_monotonic_vaslist(-1, 1, args);
};

CL_LAMBDA(core:&va-rest args);
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN T_sp cl___GT_(Vaslist_sp args) {
  if (args->nargs_zero()) {
    SIMPLE_ERROR("> needs at least one argument");
  }
  return numbers_monotonic_vaslist(1, 1, args);
};

CL_LAMBDA(core:&va-rest args);
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN T_sp cl___LE_(Vaslist_sp args) {
  if (args->nargs_zero()) {
    SIMPLE_ERROR("> needs at least one argument");
  }
  return numbers_monotonic_vaslist(-1, 0, args);
};

CL_LAMBDA(core:&va-rest args);
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN T_sp cl___GE_(Vaslist_sp args) {
  if (args->nargs_zero()) {
    SIMPLE_ERROR(">= needs at least one argument");
  }
  return numbers_monotonic_vaslist(1, 0, args);
};

bool basic_equalp(Number_sp na, Number_sp nb);

template <std::floating_point Float> bool equalp_ratio_float(Ratio_sp x, Float y) {
  auto q = float_convert<Float>::float_to_quadruple(y);

  if (q.category != float_convert<Float>::category::finite || q.significand == 0 || q.exponent >= 0 ||
      (q.sign > 0 && Real_O::minusp(x)) || (q.sign < 0 && Real_O::plusp(x)))
    return false;

  return basic_equalp(x, float_to_rational(y));
}

/*! Return true if two numbers are equal otherwise false */
bool basic_equalp(Number_sp na, Number_sp nb) {
  Complex_sp ca = na.asOrNull<Complex_O>(), cb = nb.asOrNull<Complex_O>();
  if (ca && cb)
    return basic_equalp(ca->real(), cb->real()) && basic_equalp(ca->imaginary(), cb->imaginary());
  if (ca)
    return Number_O::zerop(ca->imaginary()) && basic_equalp(ca->real(), nb);
  if (cb)
    return Number_O::zerop(cb->imaginary()) && basic_equalp(cb->real(), na);

  Ratio_sp ra = na.asOrNull<Ratio_O>(), rb = nb.asOrNull<Ratio_O>();
  if (ra && rb)
    return basic_equalp(ra->numerator(), rb->numerator()) && basic_equalp(ra->denominator(), rb->denominator());
  if (ra) {
#ifdef CLASP_LONG_FLOAT
    if (nb.isA<LongFloat_O>())
      return equalp_ratio_float(ra, nb.as_unsafe<LongFloat_O>()->get());
#endif
    if (nb.isA<DoubleFloat_O>())
      return equalp_ratio_float(ra, nb.as_unsafe<DoubleFloat_O>()->get());
    if (nb.single_floatp())
      return equalp_ratio_float(ra, nb.unsafe_single_float());
#ifdef CLASP_SHORT_FLOAT
    if (nb.short_floatp())
      return equalp_ratio_float(ra, nb.unsafe_short_float());
#endif
    // Normalized ratios are never integers.
    return false;
  }
  if (rb) {
#ifdef CLASP_LONG_FLOAT
    if (na.isA<LongFloat_O>())
      return equalp_ratio_float(rb, na.as_unsafe<LongFloat_O>()->get());
#endif
    if (na.isA<DoubleFloat_O>())
      return equalp_ratio_float(rb, na.as_unsafe<DoubleFloat_O>()->get());
    if (na.single_floatp())
      return equalp_ratio_float(rb, na.unsafe_single_float());
#ifdef CLASP_SHORT_FLOAT
    if (na.short_floatp())
      return equalp_ratio_float(rb, na.unsafe_short_float());
#endif
    // Normalized ratios are never integers.
    return false;
  }

  Bignum_sp ba = na.asOrNull<Bignum_O>(), bb = nb.asOrNull<Bignum_O>();
  if (ba && bb)
    return core__next_compare(ba, bb) == 0;
  if (ba) {
#ifdef CLASP_LONG_FLOAT
    if (nb.isA<LongFloat_O>())
      return compare_bignum_float(ba, nb.as_unsafe<LongFloat_O>()->get()) == 0;
#endif
    if (nb.isA<DoubleFloat_O>())
      return compare_bignum_float(ba, nb.as_unsafe<DoubleFloat_O>()->get()) == 0;
    if (nb.single_floatp())
      return compare_bignum_float(ba, nb.unsafe_single_float()) == 0;
#ifdef CLASP_SHORT_FLOAT
    if (nb.short_floatp())
      return compare_bignum_float(ba, nb.unsafe_short_float()) == 0;
#endif
    // bignums are never in fixnum range.
    return false;
  }
  if (bb) {
#ifdef CLASP_LONG_FLOAT
    if (na.isA<LongFloat_O>())
      return compare_bignum_float(bb, na.as_unsafe<LongFloat_O>()->get()) == 0;
#endif
    if (na.isA<DoubleFloat_O>())
      return compare_bignum_float(bb, na.as_unsafe<DoubleFloat_O>()->get()) == 0;
    if (na.single_floatp())
      return compare_bignum_float(bb, na.unsafe_single_float()) == 0;
#ifdef CLASP_SHORT_FLOAT
    if (na.short_floatp())
      return compare_bignum_float(bb, na.unsafe_short_float()) == 0;
#endif
    // bignums are never in fixnum range.
    return false;
  }

#ifdef CLASP_LONG_FLOAT
  if (na.isA<LongFloat_O>())
    return nb.fixnump() ? compare_fixnum_float(nb.unsafe_fixnum(), na.as_unsafe<LongFloat_O>()->get()) == 0
                        : na.as_unsafe<LongFloat_O>()->get() == Number_O::as_long_float(nb);
  if (nb.isA<LongFloat_O>())
    return na.fixnump() ? compare_fixnum_float(na.unsafe_fixnum(), nb.as_unsafe<LongFloat_O>()->get()) == 0
                        : nb.as_unsafe<LongFloat_O>()->get() == Number_O::as_long_float(na);
#endif

  if (na.isA<DoubleFloat_O>())
    return nb.fixnump() ? compare_fixnum_float(nb.unsafe_fixnum(), na.as_unsafe<DoubleFloat_O>()->get()) == 0
                        : na.as_unsafe<DoubleFloat_O>()->get() == Number_O::as_double_float(nb);
  if (nb.isA<DoubleFloat_O>())
    return na.fixnump() ? compare_fixnum_float(na.unsafe_fixnum(), nb.as_unsafe<DoubleFloat_O>()->get()) == 0
                        : nb.as_unsafe<DoubleFloat_O>()->get() == Number_O::as_double_float(na);

  if (na.single_floatp())
    return nb.fixnump() ? compare_fixnum_float(nb.unsafe_fixnum(), na.unsafe_single_float()) == 0
                        : na.unsafe_single_float() == Number_O::as_single_float(nb);
  if (nb.single_floatp())
    return na.fixnump() ? compare_fixnum_float(na.unsafe_fixnum(), nb.unsafe_single_float()) == 0
                        : nb.unsafe_single_float() == Number_O::as_single_float(na);

#ifdef CLASP_SHORT_FLOAT
  if (na.short_floatp())
    return nb.fixnump() ? compare_fixnum_float(nb.unsafe_fixnum(), na.unsafe_short_float()) == 0
                        : na.unsafe_short_float() == Number_O::as_short_float(nb);
  if (nb.short_floatp())
    return na.fixnump() ? compare_fixnum_float(na.unsafe_fixnum(), nb.unsafe_short_float()) == 0
                        : nb.unsafe_short_float() == Number_O::as_short_float(na);
#endif

  return na.unsafe_fixnum() == nb.unsafe_fixnum();
}

CL_NAME("TWO-ARG-=");
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN bool two_arg__EQ_(Number_sp x, Number_sp y) { return basic_equalp(x, y); }

CL_LAMBDA(core:&va-rest args);
CL_DECLARE();
CL_UNWIND_COOP(True);
CL_DOCSTRING(R"dx(NE)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl___NE_(Vaslist_sp args) {
  /* Unlike variable-argument =, this takes a quadratic number of
   * comparisons, as every pair must be unequal. */
  switch (args->nargs()) {
    /* I expect the order of likelihood is 2, 3, 1, >3, 0.
     * I don't think the compiler takes the order in a switch
     * very seriously, though, so it's just in order. */
  case 0:
    SIMPLE_PROGRAM_ERROR("/= needs at least 1 argument", nil<T_O>());
  case 1: {
    // the first arg needs to be a number - check that
    gc::As<Number_sp>(args->next_arg());
    return _lisp->_true();
  }
  case 2: {
    Number_sp a = gc::As<Number_sp>(args->next_arg());
    Number_sp b = gc::As<Number_sp>(args->next_arg());
    if (basic_equalp(a, b))
      return nil<T_O>();
    else
      return _lisp->_true();
  }
  case 3: {
    Number_sp a = gc::As<Number_sp>(args->next_arg());
    Number_sp b = gc::As<Number_sp>(args->next_arg());
    Number_sp c = gc::As<Number_sp>(args->next_arg());
    if (basic_equalp(a, b))
      return nil<T_O>();
    if (basic_equalp(a, c))
      return nil<T_O>();
    if (basic_equalp(b, c))
      return nil<T_O>();
    return _lisp->_true();
  }
  default: {
    /* General case is a nested loop.
     * We're going to iterate over the arguments several times,
     * so a valist isn't going to cut it. */
    List_sp largs = core__list_from_vaslist(args);
    while (largs.consp()) {
      Number_sp n1 = gc::As<Number_sp>(oCar(largs));
      for (List_sp cur = oCdr(largs); cur.notnilp(); cur = oCdr(cur)) {
        Number_sp n2 = gc::As<Number_sp>(oCar(cur));
        if (basic_equalp(n1, n2))
          return nil<T_O>();
      }
      largs = oCdr(largs);
    }
    return _lisp->_true();
  }
  }
}

CL_LAMBDA(&rest args);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(_EQ_)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl___EQ_(List_sp args) {
  if (args.nilp())
    SIMPLE_PROGRAM_ERROR("= needs at least 1 argument", nil<T_O>());
  Number_sp a = gc::As<Number_sp>(oCar(args));
  Number_sp b;
  for (auto cur : (List_sp)oCdr(args)) {
    b = gc::As<Number_sp>(oCar(cur));
    if (!basic_equalp(a, b))
      return nil<T_O>();
  }
  return _lisp->_true();
};

SYMBOL_EXPORT_SC_(ClPkg, max);
SYMBOL_EXPORT_SC_(ClPkg, min);
SYMBOL_EXPORT_SC_(ClPkg, zerop);
SYMBOL_SC_(CorePkg, fixnum_number_of_bits);
SYMBOL_EXPORT_SC_(ClPkg, _LT_);
SYMBOL_EXPORT_SC_(ClPkg, _GT_);
SYMBOL_EXPORT_SC_(ClPkg, _LE_);
SYMBOL_EXPORT_SC_(ClPkg, _GE_);
SYMBOL_EXPORT_SC_(ClPkg, _EQ_);
SYMBOL_EXPORT_SC_(ClPkg, _NE_);
SYMBOL_EXPORT_SC_(ClPkg, _PLUS_);
SYMBOL_EXPORT_SC_(ClPkg, _TIMES_);
SYMBOL_EXPORT_SC_(ClPkg, _MINUS_);
SYMBOL_EXPORT_SC_(ClPkg, _DIVIDE_);
SYMBOL_EXPORT_SC_(CorePkg, logand_2op);
SYMBOL_EXPORT_SC_(CorePkg, logxor_2op);
SYMBOL_EXPORT_SC_(CorePkg, logior_2op);
SYMBOL_EXPORT_SC_(CorePkg, logeqv_2op);

bool Number_O::equal(T_sp obj) const {
  if (this->eq(obj))
    return true;
  return cl__eql(this->asSmartPtr(), obj);
}

bool Number_O::equalp(T_sp obj) const {
  if (gc::IsA<Number_sp>(obj))
    return basic_equalp(this->asSmartPtr(), gc::As_unsafe<Number_sp>(obj));
  else
    return false;
}

Rational_sp Rational_O::create(mpz_class const& num, mpz_class const& denom) {
  mpz_class q, r;
  if (denom == 0)
    ERROR_DIVISION_BY_ZERO(Integer_O::create(num), Integer_O::create(denom));
  else if (denom == 1)
    return Integer_O::create(num);
  else {
    mpz_tdiv_qr(q.get_mpz_t(), r.get_mpz_t(), num.get_mpz_t(), denom.get_mpz_t());
    if (r == 0)
      return Integer_O::create(q);
    else
      return Ratio_O::create(num, denom);
  }
}

Rational_sp Rational_O::create(Integer_sp num, Integer_sp denom) {
  if (num.fixnump()) {
    Fixnum fnum = num.unsafe_fixnum();
    if (denom.fixnump()) {
      Fixnum fdenom = denom.unsafe_fixnum();
      switch (fdenom) {
      case 0:
        ERROR_DIVISION_BY_ZERO(num, denom);
      case 1:
        return num;
      case -1:
        return gc::As_unsafe<Integer_sp>(clasp_negate(num));
      default: {
        // check if they divide.
        // (Note that the case of fnum == 0 is covered here.)
        if ((fnum % fdenom) == 0)
          return clasp_make_fixnum(fnum / fdenom);
        else
          return Ratio_O::create(num, denom); // no
      }
      }
    } else {
      // Fixnum divided by a bignum.
      // This will never be exact except in the exceptional case
      // that num = most-negative-fixnum, denom = -num,
      // or if fnum == 0.
      if (fnum == 0)
        return clasp_make_fixnum(0);
      else if (fnum == gc::most_negative_fixnum) {
        Number_sp ndenom = clasp_negate(denom);
        if (ndenom.fixnump() && (ndenom.unsafe_fixnum() == gc::most_negative_fixnum))
          return clasp_make_fixnum(-1);
      }
      return Ratio_O::create(num, denom);
    }
  } else
    return num->ratdivide(denom);
}

CL_DOCSTRING(R"dx(Return a number that is NAN)dx");
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN DoubleFloat_sp core__nan() {
  DoubleFloat_sp rnan = DoubleFloat_O::create(NAN);
  return (rnan);
}

// --------------------------------------------------------------------------------

T_sp Integer_O::makeIntegerType(gc::Fixnum low, gc::Fixnum hi) {
  return Cons_O::createList(cl::_sym_Integer_O, Integer_O::create(low), Integer_O::create(hi));
}

// Integer_O::create is not defined in numbers.h because it cannot
// include bignum.h for Bignum_O::create (circular reference).
// Therefore we need to instantiate the template functions.

Integer_sp Integer_O::create(std::signed_integral auto v) {
  if (v >= gc::most_negative_fixnum && v <= gc::most_positive_fixnum)
    return clasp_make_fixnum(v);
  else
    return Bignum_O::create(v);
}

Integer_sp Integer_O::create(std::unsigned_integral auto v) {
  if (v <= gc::most_positive_fixnum)
    return clasp_make_fixnum(v);
  else
    return Bignum_O::create(v);
}

// gc::Fixnum is handled by one of the below definitions or instantiations.

Integer_sp Integer_O::create(int8_t v) { return clasp_make_fixnum(v); }
Integer_sp Integer_O::create(uint8_t v) { return clasp_make_fixnum(v); }
Integer_sp Integer_O::create(int16_t v) { return clasp_make_fixnum(v); }
Integer_sp Integer_O::create(uint16_t v) { return clasp_make_fixnum(v); }
#ifdef CLASP_FIXNUM_IS_INT64
Integer_sp Integer_O::create(int32_t v) { return clasp_make_fixnum(v); }
Integer_sp Integer_O::create(uint32_t v) { return clasp_make_fixnum(v); }
#else
template Integer_sp Integer_O::create(int32_t v);
template Integer_sp Integer_O::create(uint32_t v);
#endif
template Integer_sp Integer_O::create(int64_t v);
template Integer_sp Integer_O::create(uint64_t v);

#ifndef CLASP_LONG_LONG_IS_INT64
template Integer_sp Integer_O::create(long long v);
#endif
#ifndef CLASP_UNSIGNED_LONG_LONG_IS_UINT64
template Integer_sp Integer_O::create(unsigned long long v);
#endif

#if !defined(_TARGET_OS_LINUX) && !defined(_TARGET_OS_FREEBSD)
Integer_sp Integer_O::create(unsigned long v) {
  if (v <= gc::most_positive_fixnum)
    return clasp_make_fixnum(static_cast<Fixnum>(v));
  else
    return Bignum_O::create(static_cast<uint64_t>(v));
}
#endif

/*Integer_sp Integer_O::create(std::floating_point auto v) {
  // Why >= and <? Because most-negative-fixnum is a negative power of
  // two, exactly representable by a float. most-positive-fixnum is
  // slightly less than a positive power of two. So (double)mpf is a
  // double that, cast to an integer, will be (1+ mpf). We want a bignum
  // out of that.
  if (v >= static_cast<decltype(v)>(gc::most_negative_fixnum) && v < static_cast<decltype(v)>(gc::most_positive_fixnum))
    return clasp_make_fixnum(v);
  else
    return Bignum_O::create(v);
}*/

template Integer_sp Integer_O::create(float v);
template Integer_sp Integer_O::create(double v);

Integer_sp Integer_O::create(const mpz_class& v) {
  if (v >= gc::most_negative_fixnum && v <= gc::most_positive_fixnum)
    return clasp_make_fixnum(v.get_si());
  else
    return Bignum_O::create(v);
}

}; // namespace core

SYMBOL_EXPORT_SC_(ClPkg, logand);
SYMBOL_EXPORT_SC_(ClPkg, logior);
SYMBOL_EXPORT_SC_(ClPkg, logandc1);
SYMBOL_EXPORT_SC_(ClPkg, logandc2);
SYMBOL_EXPORT_SC_(ClPkg, logeqv);
SYMBOL_EXPORT_SC_(ClPkg, lognand);
SYMBOL_EXPORT_SC_(ClPkg, lognor);
SYMBOL_EXPORT_SC_(ClPkg, lognot);
SYMBOL_EXPORT_SC_(ClPkg, logorc1);
SYMBOL_EXPORT_SC_(ClPkg, logorc2);
SYMBOL_EXPORT_SC_(ClPkg, logxor);

namespace core {

//--------------------------------------------------

Number_sp DoubleFloat_O::reciprocal_() const { return DoubleFloat_O::create(1.0 / this->_Value); }

short_float_t DoubleFloat_O::as_short_float_() const { return (short_float_t)this->_Value; }

single_float_t DoubleFloat_O::as_single_float_() const { return (float)this->_Value; }

double_float_t DoubleFloat_O::as_double_float_() const { return (double)this->_Value; }

long_float_t DoubleFloat_O::as_long_float_() const { return (long_float_t)this->_Value; }

CL_LISPIFY_NAME("core:castToInteger");
CL_DEFMETHOD Integer_sp DoubleFloat_O::castToInteger() const {
  TESTING();
  if (this->_Value < 0) {
    double f = -this->_Value;
    long long int cf = *(long long int*)&f;
    return gc::As<Integer_sp>(clasp_negate(Integer_O::create((gctools::Fixnum)cf)));
  }
  long long int cf = *(long long int*)&this->_Value;
  return Integer_O::create((gctools::Fixnum)cf);
}

Number_sp DoubleFloat_O::signum_() const { return create(_signum(_Value)); }

void DoubleFloat_O::sxhash_(HashGenerator& hg) const {
  hg.addValue((std::fpclassify(this->_Value) == FP_ZERO) ? 0u : float_convert<double>::float_to_bits(this->_Value));
}

bool DoubleFloat_O::eql_(T_sp obj) const {
  if (this->eq(obj))
    return true;
  if (DoubleFloat_sp other = obj.asOrNull<DoubleFloat_O>())
    return _Value == other->get() && std::signbit(_Value) == std::signbit(other->get());
  return false;
}

string DoubleFloat_O::__repr__() const {
  stringstream ss;
  ss << this->_Value;
  return ss.str();
}

// ---------------------------------------------
//
// LongFloat stuff

#ifdef CLASP_LONG_FLOAT
short_float_t LongFloat_O::as_short_float_() const { return (short_float_t)this->_Value; }

single_float_t LongFloat_O::as_single_float_() const { return (float)this->_Value; }

double_float_t LongFloat_O::as_double_float_() const { return (double)this->_Value; }

long_float_t LongFloat_O::as_long_float_() const { return (long_float_t)this->_Value; }

CL_LISPIFY_NAME("core:castToInteger");
CL_DEFMETHOD Integer_sp LongFloat_O::castToInteger() const {
  IMPLEMENT_MEF("How do I cast the value to a bignum?");
#if 0
  if (this->_Value < 0) {
    double f = -this->_Value;
    long long int cf = *(long long int*)&f;
    return Integer_O::create(cf)->negate().as<Integer_O>();
  }
  long long int cf = *(long long int*)&this->_Value;
  return Integer_O::create(cf);
#endif
}

Number_sp LongFloat_O::reciprocal_() const { return LongFloat_O::create(long_float_t{1.0} / this->_Value); }

void LongFloat_O::sxhash_(HashGenerator& hg) const {
  hg.addValue((std::fpclassify(this->_Value) == FP_ZERO) ? 0u : float_convert<long_float_t>::float_to_bits(this->_Value));
}

bool LongFloat_O::eql_(T_sp obj) const {
  if (this->eq(obj))
    return true;
  if (LongFloat_sp other = obj.asOrNull<LongFloat_O>())
    return _Value == other->get() && std::signbit(_Value) == std::signbit(other->get());
  return false;
}

Number_sp LongFloat_O::signum_() const { return create(_signum(_Value)); }

string LongFloat_O::__repr__() const {
  stringstream ss;
  ss << this->_Value;
  return ss.str();
}
#endif

// --------------------------------------------------------------------------------

// translated from https://gitlab.com/embeddable-common-lisp/ecl/blob/develop/src/c/number.d#L663
static Integer_sp mantissa_and_exponent_from_ratio(Integer_sp num, Integer_sp den, int digits, gc::Fixnum* exponent) {
  /* We have to cook our own routine because GMP does not round. The
   * recipe is simple: we multiply the numerator by a large enough
   * number so that the integer length of the division by the
   * denominator is equal to the number of digits of the mantissa of
   * the floating point number. The result is scaled back by the
   * appropriate exponent.
   */
  bool negative = false;
  if (Real_O::minusp(num)) {
    negative = true;
    num = gc::As_unsafe<Integer_sp>(clasp_negate(num));
  }
  gc::Fixnum num_digits = clasp_integer_length(num);
  gc::Fixnum den_digits = clasp_integer_length(den);
  gc::Fixnum scale = digits + 1 - (num_digits - den_digits);
  /* Scale the numerator in the correct range so that the quotient
   * truncated to an integer has a length of digits+1 or digits+2. If
   * scale is negative, we simply shift out unnecessary digits of num,
   * which don't affect the quotient. */
  num = clasp_ash(num, scale);
  Integer_sp quotient = clasp_integer_divide(num, den);
  if (clasp_integer_length(quotient) > digits + 1) {
    /* quotient is too large, shift out an unnecessary digit */
    scale--;
    quotient = clasp_ash(quotient, -1);
  }
  /* round quotient */
  if (Integer_O::oddp(quotient)) {
    quotient = gc::As_unsafe<Integer_sp>(clasp_one_plus(quotient));
  }
  /* shift out the remaining unnecessary digit of quotient */
  quotient = clasp_ash(quotient, -1);
  /* fix the sign */
  if (negative) {
    quotient = gc::As<Integer_sp>(clasp_negate(quotient));
  }
  *exponent = 1 - scale;
  return quotient;
}

template <typename Float> inline Float ratio_to_float(Integer_sp num, Integer_sp den) {
  struct float_convert<Float>::quadruple q = {
    .sign = 1
  };

  if (Real_O::minusp(num)) {
    q.sign = -1;
    num = gc::As_unsafe<Integer_sp>(clasp_negate(num));
  }

  q.exponent = clasp_integer_length(num) - clasp_integer_length(den) - float_convert<Float>::traits::significand_width - 1;
  q.significand = clasp_to_integral<typename float_convert<Float>::uint_t>(clasp_integer_divide(clasp_ash(num, -q.exponent), den));

  return float_convert<Float>::quadruple_to_float(q);
}

short_float_t Ratio_O::as_short_float_() const { return ratio_to_float<short_float_t>(this->_numerator, this->_denominator); }

single_float_t Ratio_O::as_single_float_() const { return ratio_to_float<float>(this->_numerator, this->_denominator); }

double_float_t Ratio_O::as_double_float_() const { return ratio_to_float<double>(this->_numerator, this->_denominator); }

long_float_t Ratio_O::as_long_float_() const { return ratio_to_float<long_float_t>(this->_numerator, this->_denominator); }

string Ratio_O::__repr__() const {
  stringstream ss;
  ss << _rep_(this->_numerator) << "/" << _rep_(this->_denominator);
  return ss.str();
}

Number_sp Ratio_O::abs_() const {
  return Ratio_O::create_primitive(gc::As_unsafe<Integer_sp>(Number_O::abs(gc::As<Integer_sp>(this->_numerator))),
                                   this->_denominator);
}

bool Ratio_O::eql_(T_sp obj) const {
  if (this->eq(obj))
    return true;
  if (Ratio_sp other = obj.asOrNull<Ratio_O>()) {
    if (!cl__eql(this->_numerator, other->_numerator))
      return false;
    if (!cl__eql(this->_denominator, other->_denominator))
      return false;
    return true;
  }
  return false;
}
void Ratio_O::sxhash_(HashGenerator& hg) const {
  if (hg.isFilling())
    hg.hashObject(this->_numerator);
  if (hg.isFilling())
    hg.hashObject(this->_denominator);
}

Number_sp Ratio_O::signum_() const {
  ASSERT(Real_O::plusp(this->_denominator));
  return signum(this->_numerator);
}

CL_PKG_NAME(ClPkg, signum);
DOCGROUP(clasp)
CL_DEFUN Number_sp cl__signum(Number_sp num) { return Number_O::signum(num); }

Number_sp Ratio_O::sqrt_() const { return float_sqrt(this->as_single_float_()); }

Number_sp Ratio_O::reciprocal_() const {
  Integer_sp num = this->_numerator, denom = this->_denominator;
  if (num.fixnump()) {
    switch (num.unsafe_fixnum()) {
    case 1:
      return denom;
    case -1:
      return clasp_negate(denom);
    }
  }
  if (Real_O::minusp(num)) {
    Integer_sp indenom = gc::As_unsafe<Integer_sp>(clasp_negate(denom));
    Integer_sp innum = gc::As_unsafe<Integer_sp>(clasp_negate(num));
    return Ratio_O::create_primitive(indenom, innum);
  } else
    return Ratio_O::create_primitive(denom, num);
}

void Ratio_O::setf_numerator_denominator(Integer_sp inum, Integer_sp idenom) {
  Integer_sp gcd = clasp_gcd(inum, idenom);
  Integer_sp num = inum;
  Integer_sp denom = idenom;
  if (!(gcd.fixnump() && gcd.unsafe_fixnum() == 1)) {
    num = clasp_integer_divide(inum, gcd);
    denom = clasp_integer_divide(idenom, gcd);
  }
  if (num.fixnump() && denom.fixnump()) {
    if (denom.unsafe_fixnum() < 0) {
      this->_numerator = clasp_make_fixnum(-num.unsafe_fixnum());
      this->_denominator = clasp_make_fixnum(-denom.unsafe_fixnum());
    } else {
      this->_numerator = num;
      this->_denominator = denom;
    }
    return;
  }
  if (Real_O::minusp(idenom)) {
    this->_numerator = gc::As<Integer_sp>(clasp_negate(num));
    this->_denominator = gc::As<Integer_sp>(clasp_negate(denom));
  } else {
    this->_numerator = num;
    this->_denominator = denom;
  }
}

// --------------------------------------------------------------------------------

Number_sp Complex_O::signum_() const {
  if (this->zerop_())
    return this->asSmartPtr();
  else
    return this->asSmartPtr() / this->abs_();
}

string Complex_O::__repr__() const {
  stringstream ss;
  ss << "#C(" << _rep_(this->_real) << " " << _rep_(this->_imaginary) << ")";
  return ss.str();
}

void Complex_O::sxhash_(HashGenerator& hg) const {
  if (hg.isFilling())
    hg.hashObject(this->_real);
  if (hg.isFilling())
    hg.hashObject(this->_imaginary);
}

bool Complex_O::eql_(T_sp o) const {
  if (this->eq(o))
    return true;
  if (Complex_sp other = o.asOrNull<Complex_O>()) {
    if (!cl__eql(this->_real, other->_real))
      return false;
    if (!cl__eql(this->_imaginary, other->_imaginary))
      return false;
    return true;
  }
  return false;
}

Number_sp Complex_O::abs_() const { return Number_O::sqrt(this->_real * this->_real + this->_imaginary * this->_imaginary); }

Number_sp Complex_O::reciprocal_() const {
  // 1/(a+bi) = (a-bi)/(a^2+b^2) by basic algebra.
  // alternately we could just clasp_divide. I dunno if reciprocal_ is terribly necessary.
  Real_sp square_modulus = gc::As_unsafe<Real_sp>(this->_real * this->_real + this->_imaginary * this->_imaginary);
  return Complex_O::create(gc::As_unsafe<Real_sp>(this->_real / square_modulus),
                           gc::As_unsafe<Real_sp>(clasp_negate(this->_imaginary) / square_modulus));
}

/* ----------------------------------------------------------------------

   sqrt

*/

/*
    sqrt.d  -- Square root.
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

Number_sp DoubleFloat_O::sqrt_() const {
  if (_Value < 0.0)
    return clasp_make_complex(DoubleFloat_O::create(0.0), DoubleFloat_O::create(std::sqrt(-_Value)));

  return DoubleFloat_O::create(std::sqrt(this->_Value));
}

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::sqrt_() const {
  if (_Value < long_float_t{0.0})
    return clasp_make_complex(LongFloat_O::create(long_float_t{0.0}), LongFloat_O::create(std::sqrt(-_Value)));

  return LongFloat_O::create(std::sqrt(_Value));
}
#endif

Number_sp Complex_O::sqrt_() const { return cl__expt(this->asSmartPtr(), _lisp->plusHalf()); }
Number_sp Bignum_O::sqrt_() const {
  // Could move the <0 logic out to another function, to share
  // hypothetically we could use mpn_sqrtrem instead, but i imagine it's slower.
  // We convert to a double for maximum range, but return a single as required
  // by CLHS.
  double z = this->as_double_float_();
  if (z < 0)
    return clasp_make_complex(clasp_make_single_float(0.0), clasp_make_single_float(std::sqrt(-z)));
  else
    return clasp_make_single_float(std::sqrt(z));
}

Number_sp Bignum_O::reciprocal_() const {
  if (this->minusp_())
    return Ratio_O::create_primitive(clasp_make_fixnum(-1), gc::As_unsafe<Integer_sp>(this->negate_()));
  else
    return Ratio_O::create_primitive(clasp_make_fixnum(1), this->asSmartPtr());
}

CL_LAMBDA(arg);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(sqrt)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp cl__sqrt(Number_sp x) { return Number_O::sqrt(x); };

CL_LAMBDA(arg);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(abs)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp cl__abs(Number_sp x) { return Number_O::abs(x); };

/* ----------------------------------------------------------------------

   sin

*/

/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    sin.d  -- Trascendental functions: sine
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

Number_sp Rational_O::sin_() const { return clasp_make_single_float(std::sin(this->as_single_float_())); }

Number_sp DoubleFloat_O::sin_() const { return DoubleFloat_O::create(std::sin(this->_Value)); }

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::sin_() const { return LongFloat_O::create(std::sin(this->_Value)); }
#endif

Number_sp Complex_O::sin_() const {
  /*
          z = x + I y
          z = x + I y
          sin(z) = sinh(I z) = sinh(-y + I x)
        */
  Number_sp dx = this->_real;
  Number_sp dy = this->_imaginary;
  Number_sp a = Number_O::sin(dx) * Number_O::cosh(dy); // clasp_sin(dx), clasp_cosh(dy));
  Number_sp b = Number_O::cos(dx) * Number_O::sinh(dy); // clasp_cos(dx), clasp_sinh(dy));
  return clasp_make_complex(gc::As<Real_sp>(a), gc::As<Real_sp>(b));
}

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(sin)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp cl__sin(Number_sp x) { return Number_O::sin(x); }

Number_sp Rational_O::asin_() const { return _asin(this->as_single_float_()); }

Number_sp DoubleFloat_O::asin_() const { return _asin(this->_Value); }

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::asin_() const { return _asin(this->_Value); }
#endif

template <std::floating_point Float> inline std::complex<Float> _asin2(Float real, Float imag) {
#ifdef _TARGET_OS_DARWIN2
  return (std::fpclassify(real) == FP_ZERO) ? -std::log(-imag + std::sqrt(std::complex<Float>(Float{1} + imag, Float{0})))
                                            : std::asin(std::complex<Float>(real, imag));
#else
  return std::asin(std::complex<Float>(real, imag));
#endif
}

Number_sp Complex_O::asin_() const {
#ifdef CLASP_LONG_FLOAT
  if (_real.isA<LongFloat_O>())
    return make_complex(_asin2(_real->as_long_float_(), _imaginary->as_long_float_()));
#endif
  if (_real.isA<DoubleFloat_O>())
    return make_complex(_asin2(_real->as_double_float_(), _imaginary->as_double_float_()));
#ifdef CLASP_SHORT_FLOAT
  if (_real.short_floatp())
    return make_complex(_asin2(_real.unsafe_short_float(), _imaginary.unsafe_short_float()));
#endif
  return make_complex((std::complex<single_float_t>)_asin2<double_float_t>(clasp_to_float(_real), clasp_to_float(_imaginary)));
}

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(asin)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp cl__asin(Number_sp x) { return Number_O::asin(x); }

Number_sp Rational_O::acos_() const { return _acos(this->as_single_float_()); }

Number_sp DoubleFloat_O::acos_() const { return _acos(this->_Value); }

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::acos_() const { return _acos(this->_Value); }
#endif

template <std::floating_point Float> inline std::complex<Float> _acos2(Float real, Float imag) {
#ifdef _TARGET_OS_DARWIN2
  return (std::fpclassify(real) == FP_ZERO)
             ? std::numbers::pi_v<Float> / 2 + std::log(-imag + std::sqrt(std::complex<Float>(Float{1} + imag, Float{0})))
             : std::acos(std::complex<Float>(real, imag));
#else
  return std::acos(std::complex<Float>(real, imag));
#endif
}

Number_sp Complex_O::acos_() const {
#ifdef CLASP_LONG_FLOAT
  if (_real.isA<LongFloat_O>())
    return make_complex(_acos2(_real->as_long_float_(), _imaginary->as_long_float_()));
#endif
  if (_real.isA<DoubleFloat_O>())
    return make_complex(_acos2(_real->as_double_float_(), _imaginary->as_double_float_()));
#ifdef CLASP_SHORT_FLOAT
  if (_real.short_floatp())
    return make_complex(_acos2(_real.unsafe_short_float(), _imaginary.unsafe_short_float()));
#endif
#ifdef _TARGET_OS_DARWIN
  return make_complex((std::complex<single_float_t>)_acos2<double_float_t>(clasp_to_float(_real), clasp_to_float(_imaginary)));
#else  
  return make_complex(_acos2(clasp_to_float(_real), clasp_to_float(_imaginary)));
#endif
}

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(acos)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp cl__acos(Number_sp x) { return Number_O::acos(x); }

/* ----------------------------------------------------------------------

   cos

*/
/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    cos.d  -- Trascendental functions: cosine
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

Number_sp Rational_O::cos_() const { return clasp_make_single_float(std::cos(this->as_single_float_())); }

Number_sp DoubleFloat_O::cos_() const { return DoubleFloat_O::create(std::cos(this->_Value)); }

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::cos_() const { return LongFloat_O::create(std::cos(this->_Value)); }
#endif

Number_sp Complex_O::cos_() const {
  /* z = x + I y
           cos(z) = cosh(I z) = cosh(-y + I x)
        */
  Number_sp dx = this->_real;
  Number_sp dy = this->_imaginary;
  Number_sp a = Number_O::cos(dx) * Number_O::cosh(dy);               // clasp_cos(dx), clasp_cosh(dy));
  Number_sp b = clasp_negate(Number_O::sin(dx)) * Number_O::sinh(dy); // clasp_negate(clasp_sin(dx)), clasp_sinh(dy));
  return clasp_make_complex(gc::As<Real_sp>(a), gc::As<Real_sp>(b));  // clasp_make_complex(a, b);
}

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(cos)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp cl__cos(Number_sp x) { return Number_O::cos(x); }

/* ----------------------------------------------------------------------

   tan

*/
/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    tan.d  -- Trascendental functions: tangent
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2013, Christian E. Schafmeister

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
Number_sp DoubleFloat_O::sin_() const

*/

/*
 * As of 2006-10-13 I found this bug in GLIBC's tanf, which overflows
 * when the argument is pi/4. It is 2008 and this has not yet been
 * solved. Not only that, but if we use tan() on float, GCC automatically
 * and stupidly forces the use of tanf().
 */
#if defined(__amd64__) && defined(__GLIBC__)
static double safe_tanf(double x) { return tan(x); }
#else
#define safe_tanf(x) tanf(x)
#endif

Number_sp Rational_O::tan_() const { return clasp_make_single_float(safe_tanf(this->as_single_float_())); }

Number_sp DoubleFloat_O::tan_() const { return DoubleFloat_O::create(std::tan(this->_Value)); }

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::tan_() const { return LongFloat_O::create(std::tan(this->_Value)); }
#endif

Number_sp Complex_O::tan_() const {
  Number_sp a = this->sin_();
  Number_sp b = this->cos_();
  return a / b;
}

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(tan)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp cl__tan(Number_sp x) { return Number_O::tan(x); }

/* ----------------------------------------------------------------------

   sinh
*/
/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    sinh.d  -- Trascendental functions: hyperbolic sine
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

Number_sp Rational_O::sinh_() const { return clasp_make_single_float(std::sinh(this->as_single_float_())); }

Number_sp DoubleFloat_O::sinh_() const { return DoubleFloat_O::create(std::sinh(this->_Value)); }

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::sinh_() const { return LongFloat_O::create(std::sinh(this->_Value)); }
#endif

Number_sp Complex_O::sinh_() const {
  /*
          z = x + I y
          sinh(z) = (exp(z)-exp(-z))/2
          = (exp(x)*(cos(y)+Isin(y))-exp(-x)*(cos(y)-Isin(y)))/2
          = sinh(x)*cos(y) + Icosh(x)*sin(y);
        */
  Number_sp dx = this->_real;
  Number_sp dy = this->_imaginary;
  Number_sp a = Number_O::sinh(dx) * Number_O::cos(dy);
  Number_sp b = Number_O::cosh(dx) * Number_O::sin(dy);
  return clasp_make_complex(gc::As<Real_sp>(a), gc::As<Real_sp>(b));
}

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(sinh)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp cl__sinh(Number_sp x) { return Number_O::sinh(x); }

/* ----------------------------------------------------------------------

   cosh
*/
/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    cosh.d  -- Trascendental functions: hyperbolic cosine
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

Number_sp Rational_O::cosh_() const { return clasp_make_single_float(std::cosh(this->as_single_float_())); }

Number_sp DoubleFloat_O::cosh_() const { return DoubleFloat_O::create(std::cosh(this->_Value)); }

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::cosh_() const { return LongFloat_O::create(std::cosh(this->_Value)); }
#endif

Number_sp Complex_O::cosh_() const {
  /*
          z = x + I y
          cosh(z) = (exp(z)+exp(-z))/2
          = (exp(x)*(cos(y)+Isin(y))+exp(-x)*(cos(y)-Isin(y)))/2
          = cosh(x)*cos(y) + Isinh(x)*sin(y);
        */
  Number_sp dx = this->_real;
  Number_sp dy = this->_imaginary;
  Number_sp a = Number_O::cosh(dx) * Number_O::cos(dy);              // Number_O::cosh(dx), Number_O::cos(dy));
  Number_sp b = Number_O::sinh(dx) * Number_O::sin(dy);              // Number_O::sinh(dx), Number_O::sin(dy));
  return clasp_make_complex(gc::As<Real_sp>(a), gc::As<Real_sp>(b)); // clasp_make_complex(a, b);
}

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(cosh)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp cl__cosh(Number_sp x) { return Number_O::cosh(x); }

/* ----------------------------------------------------------------------

   tanh
*/
/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    tanh.d  -- Trascendental functions: hyperbolic tangent
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

Number_sp Rational_O::tanh_() const { return clasp_make_single_float(std::tanh(this->as_single_float_())); }

Number_sp DoubleFloat_O::tanh_() const { return DoubleFloat_O::create(std::tanh(this->_Value)); }

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::tanh_() const { return LongFloat_O::create(std::tanh(this->_Value)); }
#endif

Number_sp Complex_O::tanh_() const {
  Number_sp a = this->sinh_();
  Number_sp b = this->cosh_();
  return a / b;
}

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(tanh)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp cl__tanh(Number_sp x) { return Number_O::tanh(x); }

/* ----------------------------------------------------------------------

   complex-conjugate
*/
/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    conjugate.d  -- Trascendental functions: conjugateine
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

Complex_sp Complex_O::conjugate() const {
  return Complex_O::create(this->_real, gc::As_unsafe<Real_sp>(clasp_negate(this->_imaginary)));
}

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(conjugate)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp cl__conjugate(Number_sp x) { return clasp_conjugate(x); }

/* ----------------------------------------------------------------------

   exp

*/

/* -*- mode: c; c-basic-offset: 8 -*- */
/*
  exp.d  -- Trascendental functions: exponential
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

Number_sp Rational_O::exp_() const { return clasp_make_single_float(std::exp(this->as_single_float_())); }

Number_sp DoubleFloat_O::exp_() const { return DoubleFloat_O::create(std::exp(this->_Value)); }

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::exp_() const { return LongFloat_O::create(std::exp(this->_Value)); }
#endif

Number_sp Complex_O::exp_() const {
  Real_sp y, y1;
  y = this->_imaginary;
  Real_sp x = gc::As<Real_sp>(clasp_exp(this->_real));
  y1 = gc::As<Real_sp>(Number_O::cos(y)); // Number_O::cos(y);
  y = gc::As<Real_sp>(Number_O::sin(y));  // Number_O::sin(y);
  Complex_sp cy = gc::As_unsafe<Complex_sp>(clasp_make_complex(y1, y));
  return x * cy;
}

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(exp)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp cl__exp(Number_sp x) { return clasp_exp(x); }

/* ----------------------------------------------------------------------

   expt

*/

/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    expt.d  -- Exponentiate.
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

Fixnum clasp_fixnum_expt(Fixnum x, Fixnum y) {
  Fixnum z = 1;
  while (y > 0)
    if (y % 2 == 0) {
      x *= x;
      y /= 2;
    } else {
      z *= x;
      --y;
    }
  return (z);
}

static Number_sp expt_zero(Number_sp x, Number_sp y) {
  Complex_sp cx = x.asOrNull<Complex_O>(), cy = y.asOrNull<Complex_O>();
  if (cx && cy)
    return clasp_make_complex(expt_zero(cx->real(), cy->real()), clasp_make_fixnum(0));
  if (cx)
    return clasp_make_complex(expt_zero(cx->real(), y), clasp_make_fixnum(0));
  if (cy)
    return clasp_make_complex(expt_zero(x, cy->real()), clasp_make_fixnum(0));

#ifdef CLASP_LONG_FLOAT
  if (x.isA<LongFloat_O>() || y.isA<LongFloat_O>())
    return LongFloat_O::create(long_float_t{1});
#endif

  if (x.isA<DoubleFloat_O>() || y.isA<DoubleFloat_O>())
    return DoubleFloat_O::create(double_float_t{1});

  if (x.single_floatp() || y.single_floatp())
    return SingleFloat_dummy_O::create(single_float_t{1});

#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp() || y.short_floatp())
    return ShortFloat_O::create(short_float_t{1});
#endif

  return clasp_make_fixnum(1);
}

Number_sp clasp_expt(Number_sp x, Number_sp y) {
  if (clasp_unlikely(Number_O::zerop(y))) {
    return expt_zero(x, y);
  }
  Number_sp z;
  if (Number_O::zerop(x)) {
    z = x * y;
    if (!Real_O::plusp(y.isA<Complex_O>() ? y.as_unsafe<Complex_O>()->real() : y.as<Real_O>()))
      z = clasp_make_fixnum(1) / z;
  } else if (!y.isA<Integer_O>()) {
    // Use the general definition, a^b = exp(b log(a))
    /* The following could be just
           z = clasp_log1(x);
           however, Maxima expects EXPT to have double accuracy
           when the first argument is integer and the second
           is double-float */
    z = clasp_log1(x * expt_zero(x, y));
    z = z * y;
    z = cl__exp(z);
  } else if (Real_O::minusp(y)) {
    z = clasp_negate(y);
    z = clasp_expt(x, z);
    z = clasp_reciprocal(z);
  } else {
    z = clasp_make_fixnum(1);
    Integer_sp iy = gc::As<Integer_sp>(y);
    do {
      // Exponentiation by squaring.
      if (!Integer_O::evenp(iy))
        z = z * x;
      iy = clasp_shift_right(iy, 1); // divide by two
      if (Number_O::zerop(iy))
        break;
      x = x * x;
    } while (1);
  }
  return z;
}

CL_LAMBDA(x y);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(expt)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp cl__expt(Number_sp x, Number_sp y) { return clasp_expt(x, y); }

/* ----------------------------------------------------------------------

   atan
*/
/*    atan1.d  -- Trascendental functions: arc tangent
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

Number_sp Number_O::atan2(Real_sp y, Real_sp x) {
#ifdef CLASP_LONG_FLOAT
  if (x.isA<LongFloat_O>() || y.isA<LongFloat_O>())
    return LongFloat_O::create(std::atan2(as_long_float(y), as_long_float(x)));
#endif

  if (x.isA<DoubleFloat_O>() || y.isA<DoubleFloat_O>())
    return DoubleFloat_O::create(std::atan2(as_double_float(y), as_double_float(x)));

#ifdef CLASP_SHORT_FLOAT
  if ((na.short_floatp() && !nb.single_floatp()) || (!na.single_floatp() && nb.short_floatp()))
    return ShortFloat_O::create(std::atan2(as_short_float(y), as_short_float(x)));
#endif

#ifdef _TARGET_OS_DARWIN
  return SingleFloat_dummy_O::create(std::atan2(as_double_float(y), as_double_float(x)));
#else
  return SingleFloat_dummy_O::create(std::atan2(as_single_float(y), as_single_float(x)));
#endif
}

Number_sp Rational_O::atan_() const {
#ifdef _TARGET_OS_DARWIN
  return SingleFloat_dummy_O::create(std::atan(as_double_float_()));
#else
  return SingleFloat_dummy_O::create(std::atan(as_single_float_()));
#endif
}

Number_sp DoubleFloat_O::atan_() const { return DoubleFloat_O::create(std::atan(_Value)); }

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::atan_() const { return LongFloat_O::create(std::atan(_Value)); }
#endif

Number_sp Complex_O::atan_() const {
  Number_sp z = _lisp->imaginaryUnit() * asSmartPtr();
  Number_sp z1;
  z = clasp_one_plus(z);
  z1 = asSmartPtr() * asSmartPtr();
  z1 = clasp_one_plus(z1);
  z1 = Number_O::sqrt(z1);
  z = z / z1;
  z = clasp_log1(z);
  z = _lisp->imaginaryUnitNegative() * z;
  return z;
}

CL_LAMBDA(x &optional (y nil yp));
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(atan)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp cl__atan(Number_sp x, T_sp y, bool yp) {
  if (!yp)
    return Number_O::atan(x);

  if (gctools::IsA<Number_sp>(y))
    return Number_O::atan2(x, y.as_unsafe<Number_O>());

  TYPE_ERROR(y, cl::_sym_Number_O);
}

/* ----------------------------------------------------------------------

   log1

   Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
   Copyright (c) 1990, Giuseppe Attardi.
   Copyright (c) 2001, Juan Jose Garcia Ripoll.
   Copyright (c) 2013, Christian E. Schafmeister

   ECL is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   See file '../Copyright' for full details.

   This code is modified from ecl/src/numbers/log.d
*/

Number_sp clasp_log1_complex_inner(Number_sp r, Number_sp i) {
  Real_sp a = gc::As<Real_sp>(Number_O::abs(r));
  Real_sp p = gc::As<Real_sp>(Number_O::abs(i));
  int rel = Number_O::compare(a, p);
  if (rel > 0) {
    Real_sp aux = p;
    p = a;
    a = aux;
  } else if (rel == 0) {
    /* if a == p,
     * log(sqrt(a^2+p^2)) = log(2a^2)/2
     */
    a = gc::As<Real_sp>(a * a);
    a = gc::As<Real_sp>(clasp_log1(a + a) / make_fixnum(2));
    goto OUTPUT;
  }
  /* For the real part of the output we use the formula
   *	log(sqrt(p^2 + a^2)) = log(sqrt(p^2*(1 + (a/p)^2)))
   *			     = log(p) + log(1 + (a/p)^2)/2; */
  a = gc::As<Real_sp>(a / p);
  a = gc::As<Real_sp>(clasp_log1p(a * a) / make_fixnum(2) + clasp_log1(p));
OUTPUT:
  p = gc::As<Real_sp>(Number_O::atan2(i, r));
  return clasp_make_complex(a, p);
}

Number_sp Bignum_O::log1_() const {
  Bignum_sp bignum = this->asSmartPtr();
  if (this->minusp_())
    return clasp_log1_complex_inner(bignum, clasp_make_fixnum(0));
  else {
    // In order to avoid floating point overflow,
    // we take the log of (x/2^n), n being the number of bits in the bignum,
    // and add back to the result.
    // FIXME: This can probably be accomplished more efficiently, though.
    // For example, take the most significant two words of the bignum, as we
    // do when converting bignums to floats (as of this writing), but don't
    // shift the result.
    Fixnum length = clasp_integer_length(bignum) - 1;
    Integer_sp ash = clasp_ash(make_fixnum(1), length);
    Rational_sp rational = Rational_O::create(bignum, ash);
    float d = logf(clasp_to_float(rational)) + length * logf(2.0);
    return clasp_make_single_float(d);
  }
}

Number_sp Rational_O::log1_() const {
  float f = this->as_single_float_();
  if (f < 0)
    return clasp_log1_complex_inner(this->asSmartPtr(), clasp_make_fixnum(0));
  return clasp_make_single_float(logf(this->as_single_float_()));
}

Number_sp DoubleFloat_O::log1_() const {
  if (std::isnan(_Value))
    return this->asSmartPtr();
  if (_Value < 0)
    return clasp_log1_complex_inner(this->asSmartPtr(), clasp_make_fixnum(0));
  return clasp_make_double_float(std::log(_Value));
}

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::log1_() const {
  if (std::isnan(_Value))
    return this->asSmartPtr();
  if (_Value < 0)
    return clasp_log1_complex_inner(this->asSmartPtr(), clasp_make_fixnum(0));
  return clasp_make_long_float(std::log(_Value));
}
#endif

Number_sp Complex_O::log1_() const { return clasp_log1_complex_inner(this->real(), this->imaginary()); }

Number_sp Number_O::log1p_() const { return clasp_log1_complex_inner(clasp_one_plus(this->asSmartPtr()), clasp_make_fixnum(0)); }

Number_sp Rational_O::log1p_() const {
  float f = this->as_single_float_();
  if (f < -1)
    return this->Base::log1p_();
  return clasp_make_single_float(_log1p(f));
}

Rational_sp DoubleFloat_O::as_rational_() const { return float_to_rational(_Value); }

#ifdef CLASP_LONG_FLOAT
Rational_sp LongFloat_O::as_rational_() const { return float_to_rational(_Value); }
#endif

Number_sp DoubleFloat_O::log1p_() const {
  if (std::isnan(_Value))
    return this->asSmartPtr();
  if (_Value < -1)
    return clasp_log1_complex_inner(clasp_one_plus(this->asSmartPtr()), clasp_make_fixnum(0));
  return clasp_make_double_float(_log1p(_Value));
}

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::log1p_() const {
  if (std::isnan(_Value))
    return this->asSmartPtr();
  if (_Value < -1)
    return clasp_log1_complex_inner(clasp_one_plus(this->asSmartPtr()), clasp_make_fixnum(0));
  return clasp_make_long_float(_log1p(_Value));
}
#endif

Number_sp clasp_log2(Number_sp x, Number_sp y) { return clasp_log1(y) / clasp_log1(x); }

Number_sp Complex_O::log1p_() const { return clasp_log1_complex_inner(clasp_one_plus(this->real()), this->imaginary()); }

CL_LAMBDA(number &optional base);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(Calculate the log of (number) to base (base).)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp cl__log(Number_sp number, T_sp base) {
  if (base.nilp())
    return clasp_log1(number);
  return clasp_log2(gc::As<Number_sp>(base), number);
}

CL_LAMBDA(arg);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(log1p)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp core__log1p(Number_sp arg) { return clasp_log1p(arg); };

Integer_sp clasp_ash(Integer_sp x, int bits) {
  if (bits > 0)
    return clasp_shift_left(x, bits);
  else if (bits < 0)
    return clasp_shift_right(x, -bits);
  else
    return x;
};

CL_LAMBDA(i);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(integerLength)dx");
DOCGROUP(clasp);
CL_DEFUN gc::Fixnum cl__integer_length(Integer_sp i) { return clasp_integer_length(i); };

CL_LAMBDA(i);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(Return the number of bits in the 2's complement representation"
             "of I that are 'on', i.e. distinct from the sign bit.)dx")
DOCGROUP(clasp);
CL_DEFUN gc::Fixnum cl__logcount(Integer_sp i) {
  // Builtins aren't very helpful for negative numbers, so we use the
  // (logcount x) = (logcount (lognot x)) identity.
  if (i.fixnump()) {
    gc::Fixnum x(i.unsafe_fixnum());
    if (x < 0)
      x = ~x;
    return fixnum_popcount(x);
  } else { // bignum
    return i->popcount();
  }
}

CL_LAMBDA(i);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(doc(float-nan-p)dx");
DOCGROUP(clasp);
CL_DEFUN bool ext__float_nan_p(Float_sp i) { return Float_O::isnan(i); };

CL_LAMBDA(i);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(float-infinity-p)dx");
DOCGROUP(clasp);
CL_DEFUN bool ext__float_infinity_p(Float_sp i) { return Float_O::isinf(i); };

SYMBOL_EXPORT_SC_(ClPkg, sqrt);
SYMBOL_EXPORT_SC_(ClPkg, sin);
SYMBOL_EXPORT_SC_(ClPkg, cos);
SYMBOL_EXPORT_SC_(ClPkg, tan);
SYMBOL_EXPORT_SC_(ClPkg, sinh);
SYMBOL_EXPORT_SC_(ClPkg, cosh);
SYMBOL_EXPORT_SC_(ClPkg, tanh);
SYMBOL_EXPORT_SC_(ClPkg, conjugate);
SYMBOL_EXPORT_SC_(ClPkg, log);
SYMBOL_EXPORT_SC_(CorePkg, log1p);
SYMBOL_EXPORT_SC_(ClPkg, expt);
SYMBOL_EXPORT_SC_(ClPkg, exp);

// === CLASP_TO- TRANSLATORS ===

Fixnum clasp_to_fixnum(T_sp x) { return clasp_to_integral<Fixnum>(x); }
short clasp_to_short(T_sp x) { return clasp_to_integral<short>(x); }
unsigned short clasp_to_ushort(T_sp x) { return clasp_to_integral<unsigned short>(x); }
int clasp_to_int(T_sp x) { return clasp_to_integral<int>(x); }
unsigned int clasp_to_uint(T_sp x) { return clasp_to_integral<unsigned int>(x); }
long clasp_to_long(T_sp x) { return clasp_to_integral<long>(x); }
unsigned long clasp_to_ulong(T_sp x) { return clasp_to_integral<unsigned long>(x); }
long long clasp_to_longlong(T_sp x) { return clasp_to_integral<long long>(x); }
unsigned long long clasp_to_ulonglong(T_sp x) { return clasp_to_integral<unsigned long long>(x); }
int8_t clasp_to_int8_t(T_sp x) { return clasp_to_integral<int8_t>(x); }
uint8_t clasp_to_uint8_t(T_sp x) { return clasp_to_integral<uint8_t>(x); }
int16_t clasp_to_int16_t(T_sp x) { return clasp_to_integral<int16_t>(x); }
uint16_t clasp_to_uint16_t(T_sp x) { return clasp_to_integral<uint16_t>(x); }
int32_t clasp_to_int32_t(T_sp x) { return clasp_to_integral<int32_t>(x); }
uint32_t clasp_to_uint32_t(T_sp x) { return clasp_to_integral<uint32_t>(x); }
int64_t clasp_to_int64_t(T_sp x) { return clasp_to_integral<int64_t>(x); }
uint64_t clasp_to_uint64_t(T_sp x) { return clasp_to_integral<uint64_t>(x); }
intptr_t clasp_to_intptr_t(T_sp x) { return clasp_to_integral<intptr_t>(x); }
uintptr_t clasp_to_uintptr_t(T_sp x) { return clasp_to_integral<uintptr_t>(x); }
ptrdiff_t clasp_to_ptrdiff_t(T_sp x) { return clasp_to_integral<ptrdiff_t>(x); }
size_t clasp_to_size_t(T_sp x) { return clasp_to_integral<size_t>(x); }
// FIXME: Replace all uses with clasp_to_size_t
size_t clasp_to_size(T_sp x) { return clasp_to_integral<size_t>(x); }
ssize_t clasp_to_ssize_t(T_sp x) { return clasp_to_integral<ssize_t>(x); }
ssize_t clasp_to_ssize(T_sp x) { return clasp_to_integral<ssize_t>(x); }

mpz_class clasp_to_mpz(core::T_sp x) {
  if (x.fixnump()) {
    Fixnum fn = x.unsafe_fixnum();
    mpz_class z = GMP_LONG(fn);
    return z;
  }
  return (gc::As<Integer_sp>(x))->mpz();
}

float clasp_to_float(core::Number_sp x) {
  if (x.fixnump()) {
    return (float)x.unsafe_fixnum();
  }
  if (x.single_floatp()) {
    return (float)x.unsafe_single_float();
  }
  return x->as_single_float_();
}

double clasp_to_double(core::Number_sp x) {
  if (x.fixnump()) {
    double d = x.unsafe_fixnum();
    return d;
  } else if (x.single_floatp()) {
    double d = x.unsafe_single_float();
    return d;
  }
  return x->as_double_float_();
};

double clasp_to_double(core::Integer_sp x) {
  if (x.fixnump()) {
    double d = x.unsafe_fixnum();
    return d;
  }
  return x->as_double_float_();
};

double clasp_to_double(core::T_sp x) {
  if (x.fixnump()) {
    double d = x.unsafe_fixnum();
    return d;
  } else if (x.single_floatp()) {
    double d = x.unsafe_single_float();
    return d;
  } else if (gc::IsA<Number_sp>(x)) {
    return gc::As_unsafe<Number_sp>(x)->as_double_float_();
  }
  TYPE_ERROR(x, cl::_sym_Number_O);
}

double clasp_to_double(core::Real_sp x) {
  if (x.fixnump()) {
    double d = x.unsafe_fixnum();
    return d;
  } else if (x.single_floatp()) {
    double d = x.unsafe_single_float();
    return d;
  } else if (gc::IsA<Number_sp>(x)) {
    return gc::As_unsafe<Number_sp>(x)->as_double_float_();
  }
  TYPE_ERROR(x, Cons_O::createList(cl::_sym_Real_O));
}

double clasp_to_double(core::General_sp x) {
  if (gc::IsA<Number_sp>(x)) {
    return gc::As_unsafe<Number_sp>(x)->as_double_float_();
  }
  TYPE_ERROR(x, cl::_sym_Number_O);
};

double clasp_to_double(core::DoubleFloat_sp x) { return x->get(); };

long_float_t clasp_to_long_float(Number_sp x) {
  if (x.fixnump())
    return (long_float_t)x.unsafe_fixnum();

  if (x.single_floatp())
    return (long_float_t)x.unsafe_single_float();

  if (x.isA<Real_O>())
    return x->as_long_float_();

  TYPE_ERROR(x, cl::_sym_Real_O);
};

// --- END OF TRANSLATORS ---

#ifdef CLASP_SHORT_FLOAT
CL_LAMBDA(float);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(Return the bit representation of a short float as an integer.)dx");
DOCGROUP(clasp);
CL_DEFUN Integer_sp ext__short_float_to_bits(ShortFloat_sp x) {
  return Integer_O::create(float_convert<short_float_t>::float_to_bits(x.unsafe_short_float()));
}

CL_LAMBDA(bit-representation);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(Convert a bit representation, an integer, to a short float.)dx");
DOCGROUP(clasp);
CL_DEFUN LongFloat_sp ext__bits_to_short_float(Integer_sp integer) {
  return ShortFloat_O::create(float_convert<short_float_t>::bits_to_float(clasp_to_uint16_t(integer)));
}
#endif

CL_LAMBDA(singleFloat);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(Return the IEEE754 binary32 (single) representation of a single float, as an integer.)dx");
DOCGROUP(clasp);
CL_DEFUN Integer_sp ext__single_float_to_bits(SingleFloat_sp singleFloat) {
  return Integer_O::create(float_convert<float>::float_to_bits(unbox_single_float(singleFloat)));
}

CL_LAMBDA(bit-representation);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(Convert an IEEE754 binary32 (single) representation, an integer, to a single float.)dx");
DOCGROUP(clasp);
CL_DEFUN SingleFloat_sp ext__bits_to_single_float(Integer_sp integer) {
  return make_single_float(float_convert<float>::bits_to_float(clasp_to_uint32_t(integer)));
};

CL_LAMBDA(doubleFloat);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(Return the IEEE754 binary64 (double) bit representation of a double float as an integer.)dx");
DOCGROUP(clasp);
CL_DEFUN Integer_sp ext__double_float_to_bits(DoubleFloat_sp doubleFloat) {
  return Integer_O::create(float_convert<double>::float_to_bits(doubleFloat->get()));
}

CL_LAMBDA(bit-representation);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(Convert an IEEE754 binary64 (double) representation, an integer, to a double float.)dx");
DOCGROUP(clasp);
CL_DEFUN DoubleFloat_sp ext__bits_to_double_float(Integer_sp integer) {
  return clasp_make_double_float(float_convert<double>::bits_to_float(clasp_to_uint64_t(integer)));
}

#ifdef CLASP_LONG_FLOAT
CL_LAMBDA(longFloat);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(Return the bit representation of a long float as an integer.)dx");
DOCGROUP(clasp);
CL_DEFUN Integer_sp ext__long_float_to_bits(LongFloat_sp longFloat) {
  return Integer_O::create(float_convert<long_float_t>::float_to_bits(longFloat->get()));
}

CL_LAMBDA(bit-representation);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(Convert a bit representation, an integer, to a long float.)dx");
DOCGROUP(clasp);
CL_DEFUN LongFloat_sp ext__bits_to_long_float(Integer_sp integer) {
  return clasp_make_long_float(float_convert<long_float_t>::bits_to_float(clasp_to_integral<__uint128_t>(integer)));
}
#endif

}; // namespace core

namespace core {

CL_UNWIND_COOP(true);
DOCGROUP(clasp);
 CL_DEFUN Rational_sp cl__rational(Real_sp num) { return Rational_O::coerce(num); }

CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN Rational_sp cl__rationalize(Real_sp num) { return Rational_O::coerce(num); };

Integer_sp clasp_make_integer(size_t s) { return Integer_O::create((uint64_t)s); }

#ifdef CLASP_SHORT_FLOAT
ShortFloat_sp ShortFloat_dummy_O::coerce(Number_sp x) {
  if (x.fixnump())
    return create(x.unsafe_fixnum());
  if (x.short_floatp())
    return x;
  if (x.single_floatp())
    return create((short_float_t)x.unsafe_single_float());
  if (x.isA<Real_O>())
    return create(x->as_short_float_());
  TYPE_ERROR(x, cl::_sym_Real_O);
}
#endif

SingleFloat_sp SingleFloat_dummy_O::coerce(Number_sp x) {
  if (x.fixnump())
    return create(x.unsafe_fixnum());
#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp())
    return create((single_float_t)x.unsafe_short_float());
#endif
  if (x.single_floatp())
    return x;
  if (x.isA<Real_O>())
    return create(x->as_single_float_());
  TYPE_ERROR(x, cl::_sym_Real_O);
}

DoubleFloat_sp DoubleFloat_O::coerce(Number_sp x) {
  if (x.fixnump())
    return create(x.unsafe_fixnum());
#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp())
    return create((double_float_t)x.unsafe_short_float());
#endif
  if (x.single_floatp())
    return create(x.unsafe_single_float());
  if (x.isA<DoubleFloat_O>())
    return x;
  if (x.isA<Real_O>())
    return create(x->as_double_float_());
  TYPE_ERROR(x, cl::_sym_Real_O);
}

#ifdef CLASP_LONG_FLOAT
LongFloat_sp LongFloat_O::coerce(Number_sp x) {
  if (x.fixnump())
    return create(x.unsafe_fixnum());
#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp())
    return create((long_float_t)x.unsafe_short_float());
#endif
  if (x.single_floatp())
    return create(x.unsafe_single_float());
  if (x.isA<LongFloat_O>())
    return x;
  if (x.isA<Real_O>())
    return create(x->as_long_float_());
  TYPE_ERROR(x, cl::_sym_Real_O);
}
#endif

}; // namespace core
