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
#include <clasp/core/mathDispatch.h>
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
  // need to check whether i is 0
  // A bignum better not be 0
  // if realpart is a rational and imagpart is the rational number zero, the result of complex is realpart, a rational.
  if (i.fixnump() && cl__rationalp(r)) {
    Fixnum fn = i.unsafe_fixnum();
    if (fn == 0)
      return r;
    else
      return Complex_O::create(r, i);
  }
  // If imagpart is not supplied, the imaginary part is a zero of the same type as realpart;
  // perhaps need to distinguish better whether i is supplied or not
  if (cl__floatp(r) && i.fixnump() && clasp_zerop(i)) {
    if (r.single_floatp())
      i = clasp_make_single_float(0.0);
    else if (core__double_float_p(r))
      i = DoubleFloat_O::create(0.0);
    else if (core__long_float_p(r))
      i = LongFloat_O::create(0.0l);
    // short floats are not really implemented
  }
  // If either realpart or imagpart is a float, the non-float is converted to a float before the complex is created.
  // does that mean, I need to distinguish single and double-float? PDietz seem to assume so
  else if (cl__floatp(r) && !cl__floatp(i)) {
    if (r.single_floatp())
      i = cl__float(i, clasp_make_single_float(1.0));
    else
      i = cl__float(i, DoubleFloat_O::create(1.0));
  } else if (cl__floatp(i) && !cl__floatp(r)) {
    if (i.single_floatp())
      r = cl__float(r, clasp_make_single_float(1.0));
    else
      r = cl__float(r, DoubleFloat_O::create(1.0));
  } else if (cl__floatp(i) && cl__floatp(r)) {
    // the highest type of both wins single -> double -> long
    if (r.single_floatp()) {
      if (!(i.single_floatp())) {
        // r should be of type of i
        if (core__double_float_p(i))
          r = DoubleFloat_O::create((double)r.unsafe_single_float());
        else
          r = LongFloat_O::create((long)r.unsafe_single_float());
      }
    } else if (core__double_float_p(r)) {
      if (!(core__double_float_p(i))) {
        if (core__long_float_p(i))
          r = LongFloat_O::create(clasp_to_long_float(r));
        else
          i = DoubleFloat_O::create((double)i.unsafe_single_float());
      }
    } else if (core__long_float_p(r))
      if (!(core__long_float_p(i))) {
        if (i.single_floatp())
          i = DoubleFloat_O::create((double)i.unsafe_single_float());
        else
          i = LongFloat_O::create(clasp_to_long_float(i));
      }
  }
  return Complex_O::create(r, i);
}

CL_LAMBDA(num);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(zerop)dx");
DOCGROUP(clasp);
CL_DEFUN bool cl__zerop(Number_sp num) { return clasp_zerop(num); }

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
  if (clasp_number_compare(max, y) < 0)
    max = y;
  return max;
}

Real_sp clasp_min2(Real_sp x, Real_sp y) {
  Real_sp min = x;
  if (clasp_number_compare(min, y) > 0)
    min = y;
  return min;
}

CL_LAMBDA(min &rest nums);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(min)dx");
DOCGROUP(clasp);
CL_DEFUN Real_sp cl__min(Real_sp min, List_sp nums) {
  /* INV: type check occurs in clasp_number_compare() for the rest of
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
CL_DEFUN Number_sp contagion_add(Number_sp na, Number_sp nb) {
  MATH_DISPATCH_BEGIN(na, nb) {
  case_Fixnum_v_Fixnum:
    return two_arg__PLUS_FF(na.unsafe_fixnum(), nb.unsafe_fixnum());
  case_Fixnum_v_Bignum:
    return core__next_fadd(gc::As_unsafe<Bignum_sp>(nb), na.unsafe_fixnum());
  case_Fixnum_v_Ratio:
  case_Bignum_v_Ratio : {
    // NOTE: All of the numbers are either fixnums or bignums,
    // so this could be more efficient maybe.
    Ratio_sp rat = gc::As_unsafe<Ratio_sp>(nb);
    Integer_sp den = rat->denominator();
    Integer_sp new_num = gc::As_unsafe<Integer_sp>(contagion_add(rat->numerator(), contagion_mul(na, den)));
    // result is a ratio, not an integer.
    return Ratio_O::create(new_num, den);
  }
  case_Fixnum_v_SingleFloat : { return clasp_make_single_float(clasp_to_float(na) + clasp_to_float(nb)); }
  case_Fixnum_v_DoubleFloat : { return DoubleFloat_O::create(clasp_to_double(na) + clasp_to_double(nb)); }
  case_Bignum_v_Fixnum:
    return core__next_fadd(gc::As_unsafe<Bignum_sp>(na), nb.unsafe_fixnum());
  case_Bignum_v_Bignum:
    return core__next_add(gc::As_unsafe<Bignum_sp>(na), gc::As_unsafe<Bignum_sp>(nb));
  case_Bignum_v_SingleFloat:
  case_Ratio_v_SingleFloat : { return clasp_make_single_float(clasp_to_float(na) + clasp_to_float(nb)); }
  case_Bignum_v_DoubleFloat:
  case_Ratio_v_DoubleFloat : { return DoubleFloat_O::create(clasp_to_double(na) + clasp_to_double(nb)); }
  case_Ratio_v_Fixnum:
  case_Ratio_v_Bignum : {
    Ratio_sp rat = gc::As_unsafe<Ratio_sp>(na);
    Integer_sp den = rat->denominator();
    Integer_sp new_num = gc::As_unsafe<Integer_sp>(contagion_add(rat->numerator(), contagion_mul(nb, den)));
    // result is a ratio, not an integer.
    return Ratio_O::create(new_num, den);
  }
  case_Ratio_v_Ratio : {
    Ratio_sp ra = gc::As<Ratio_sp>(na);
    Ratio_sp rb = gc::As<Ratio_sp>(nb);
    // ra.num/ra.den + rb.num/rb.den = (ra.num*rb.den+rb.num*ra.den)/ra.den*rb.den
    Number_sp n1 = contagion_mul(ra->numerator(), rb->denominator());
    Number_sp n2 = contagion_mul(ra->denominator(), rb->numerator());
    Number_sp d = contagion_mul(ra->denominator(), rb->denominator());
    Number_sp n = contagion_add(n1, n2);
    return Rational_O::create(gc::As_unsafe<Integer_sp>(n), gc::As_unsafe<Integer_sp>(d));
  }
  case_SingleFloat_v_Fixnum:
  case_SingleFloat_v_Bignum:
  case_SingleFloat_v_Ratio:
    return clasp_make_single_float(clasp_to_float(na) + clasp_to_float(nb));
  case_SingleFloat_v_SingleFloat:
    return clasp_make_single_float(clasp_to_float(na) + clasp_to_float(nb));
  case_SingleFloat_v_DoubleFloat:
  case_DoubleFloat_v_Fixnum:
  case_DoubleFloat_v_Bignum:
  case_DoubleFloat_v_Ratio:
  case_DoubleFloat_v_SingleFloat:
  case_DoubleFloat_v_DoubleFloat:
    return DoubleFloat_O::create(clasp_to_double(na) + clasp_to_double(nb));
#ifdef CLASP_LONG_FLOAT
  case_Fixnum_v_LongFloat:
  case_Ratio_v_LongFloat:
  case_SingleFloat_v_LongFloat:
  case_DoubleFloat_v_LongFloat:
  case_LongFloat_v_Fixnum:
  case_LongFloat_v_Ratio:
  case_LongFloat_v_SingleFloat:
  case_LongFloat_v_DoubleFloat:
  case_LongFloat_v_LongFloat:
    return LongFloat_O::create(na->as_long_float() + nb->as_long_float());
  case_Complex_v_LongFloat:
#endif // CLASP_LONG_FLOAT
  case_Complex_v_Fixnum:
  case_Complex_v_Ratio:
  case_Complex_v_Bignum:
  case_Complex_v_SingleFloat:
  case_Complex_v_DoubleFloat : {
    Number_sp aux = na;
    na = nb;
    nb = aux;
    goto Complex_v_Y;
  }
  case_Fixnum_v_Complex:
  case_Bignum_v_Complex:
  case_Ratio_v_Complex:
  case_SingleFloat_v_Complex:
  case_DoubleFloat_v_Complex:
#ifdef CLASP_LONG_FLOAT
  case_LongFloat_v_Complex:
#endif
  Complex_v_Y:
    return clasp_make_complex(gc::As<Real_sp>(contagion_add(na, gc::As<Complex_sp>(nb)->real())),
                              gc::As<Complex_sp>(nb)->imaginary());
  case_Complex_v_Complex : {
    Real_sp r = gc::As<Real_sp>(contagion_add(gc::As<Complex_sp>(na)->real(), gc::As<Complex_sp>(nb)->real()));
    Real_sp i = gc::As<Real_sp>(contagion_add(gc::As<Complex_sp>(na)->imaginary(), gc::As<Complex_sp>(nb)->imaginary()));
    return clasp_make_complex(r, i);
  } break;
  default:
    not_comparable_error(na, nb);
  };
  MATH_DISPATCH_END();
};

CL_NAME("TWO-ARG--");
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN Number_sp contagion_sub(Number_sp na, Number_sp nb) {
  MATH_DISPATCH_BEGIN(na, nb) {
  case_Fixnum_v_Fixnum : {
    Fixnum fa = na.unsafe_fixnum();
    Fixnum fb = nb.unsafe_fixnum();
    return Integer_O::create(static_cast<gc::Fixnum>(fa - fb));
  }
  case_Fixnum_v_Ratio:
  case_Bignum_v_Ratio : {
    // x - a/b = xb/b - a/b = (xb-a)/b
    Ratio_sp rb = gc::As<Ratio_sp>(nb);
    Number_sp n1 = contagion_mul(na, rb->denominator());
    Number_sp n = contagion_sub(n1, rb->numerator());
    return Ratio_O::create(gc::As_unsafe<Integer_sp>(n), rb->denominator());
  }
  case_Fixnum_v_SingleFloat : { return clasp_make_single_float(clasp_to_float(na) - clasp_to_float(nb)); }
  case_Fixnum_v_DoubleFloat : { return DoubleFloat_O::create(clasp_to_double(na) - clasp_to_double(nb)); }
  case_Ratio_v_SingleFloat:
  case_Bignum_v_SingleFloat : { return clasp_make_single_float(clasp_to_float(na) - clasp_to_float(nb)); }
  case_Ratio_v_DoubleFloat:
  case_Bignum_v_DoubleFloat : { return DoubleFloat_O::create(clasp_to_double(na) - clasp_to_double(nb)); }
  case_Fixnum_v_Bignum:
    return core__next_fsub(na.unsafe_fixnum(), gc::As_unsafe<Bignum_sp>(nb));
  case_Bignum_v_Fixnum:
    return core__next_fadd(gc::As_unsafe<Bignum_sp>(na), -(nb.unsafe_fixnum()));
  case_Bignum_v_Bignum:
    return core__next_sub(gc::As_unsafe<Bignum_sp>(na), gc::As_unsafe<Bignum_sp>(nb));
  case_Ratio_v_Fixnum:
  case_Ratio_v_Bignum : {
    // a/b - x = a/b - xb/b = (a-xb)/b
    Ratio_sp ra = gc::As<Ratio_sp>(na);
    Number_sp n2 = contagion_mul(nb, ra->denominator());
    Number_sp n = contagion_sub(ra->numerator(), n2);
    return Ratio_O::create(gc::As_unsafe<Integer_sp>(n), ra->denominator());
  }
  case_Ratio_v_Ratio : {
    // a/b - c/d = (ad-bc)/bd
    Ratio_sp ra = gc::As<Ratio_sp>(na);
    Ratio_sp rb = gc::As<Ratio_sp>(nb);
    Number_sp n1 = contagion_mul(ra->numerator(), rb->denominator());
    Number_sp n2 = contagion_mul(ra->denominator(), rb->numerator());
    Number_sp n = contagion_sub(n1, n2);
    Number_sp d = contagion_mul(ra->denominator(), rb->denominator());
    return Rational_O::create(gc::As_unsafe<Integer_sp>(n), gc::As_unsafe<Integer_sp>(d));
  }
  case_SingleFloat_v_Fixnum:
  case_SingleFloat_v_Bignum:
  case_SingleFloat_v_Ratio:
    return clasp_make_single_float(clasp_to_float(na) - clasp_to_float(nb));
  case_SingleFloat_v_SingleFloat:
    return clasp_make_single_float(clasp_to_float(na) - clasp_to_float(nb));
  case_SingleFloat_v_DoubleFloat:
  case_DoubleFloat_v_Fixnum:
  case_DoubleFloat_v_Bignum:
  case_DoubleFloat_v_Ratio:
  case_DoubleFloat_v_SingleFloat:
  case_DoubleFloat_v_DoubleFloat:
    return DoubleFloat_O::create(clasp_to_double(na) - clasp_to_double(nb));
#ifdef CLASP_LONG_FLOAT
  case_Fixnum_v_LongFloat:
  case_Ratio_v_LongFloat:
  case_SingleFloat_v_LongFloat:
  case_DoubleFloat_v_LongFloat:
  case_LongFloat_v_Fixnum:
  case_LongFloat_v_Ratio:
  case_LongFloat_v_SingleFloat:
  case_LongFloat_v_DoubleFloat:
  case_LongFloat_v_LongFloat:
    return LongFloat_O::create(na->as_long_float() - nb->as_long_float());
#endif
  case_Complex_v_LongFloat:
  case_Complex_v_Fixnum:
  case_Complex_v_Ratio:
  case_Complex_v_Bignum:
  case_Complex_v_SingleFloat:
  case_Complex_v_DoubleFloat : {
    return clasp_make_complex(gc::As<Real_sp>(contagion_sub(gc::As<Complex_sp>(na)->real(), nb)),
                              gc::As<Complex_sp>(na)->imaginary());
  }
  case_Fixnum_v_Complex:
  case_Bignum_v_Complex:
  case_Ratio_v_Complex:
  case_SingleFloat_v_Complex:
  case_DoubleFloat_v_Complex:
  case_LongFloat_v_Complex:
    return clasp_make_complex(gc::As<Real_sp>(contagion_sub(na, gc::As<Complex_sp>(nb)->real())),
                              gc::As<Real_sp>(clasp_negate(gc::As<Complex_sp>(nb)->imaginary())));
  case_Complex_v_Complex : {
    Real_sp r = gc::As<Real_sp>(contagion_sub(gc::As<Complex_sp>(na)->real(), gc::As<Complex_sp>(nb)->real()));
    Real_sp i = gc::As<Real_sp>(contagion_sub(gc::As<Complex_sp>(na)->imaginary(), gc::As<Complex_sp>(nb)->imaginary()));
    return clasp_make_complex(r, i);
  } break;
  default:
    not_comparable_error(na, nb);
  };
  MATH_DISPATCH_END();
}

CL_NAME("TWO-ARG-*");
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN Number_sp contagion_mul(Number_sp na, Number_sp nb) {
  MATH_DISPATCH_BEGIN(na, nb) {
  case_Fixnum_v_Fixnum : {
    // We want to detect when Fixnum * Fixnum multiplication will overflow and only then use bignum arithmetic.
    // But C++ doesn't give us a way to do that - so we use the __builtin_mul_overflow clang builtin.
    // It will return false if there is no overflow and the multiplication result will be in fr.
    // The return value fr may over
    // If it doesn't overflow - then this will be faster than always using bignum arithmetic.
    Fixnum fa = na.unsafe_fixnum();
    Fixnum fb = nb.unsafe_fixnum();
    Fixnum fr;
    bool overflow = __builtin_mul_overflow(fa, fb, &fr);
    if (!overflow)
      return Integer_O::create(fr);
    return core__mul_fixnums(fa, fb);
  }
  case_Fixnum_v_Bignum:
    return core__next_fmul(gc::As_unsafe<Bignum_sp>(nb), na.unsafe_fixnum());
  case_Fixnum_v_Ratio:
  case_Bignum_v_Ratio : {
    Ratio_sp rat = gc::As_unsafe<Ratio_sp>(nb);
    Integer_sp new_num = gc::As_unsafe<Integer_sp>(contagion_mul(na, rat->numerator()));
    return Rational_O::create(new_num, rat->denominator());
  }
  case_Fixnum_v_SingleFloat : { return clasp_make_single_float(clasp_to_float(na) * clasp_to_float(nb)); }
  case_Fixnum_v_DoubleFloat : { return DoubleFloat_O::create(clasp_to_double(na) * clasp_to_double(nb)); }
  case_Bignum_v_Fixnum:
    return core__next_fmul(gc::As_unsafe<Bignum_sp>(na), nb.unsafe_fixnum());
  case_Bignum_v_Bignum:
    return core__next_mul(gc::As_unsafe<Bignum_sp>(na), gc::As_unsafe<Bignum_sp>(nb));
  case_Bignum_v_SingleFloat:
  case_Ratio_v_SingleFloat : { return clasp_make_single_float(clasp_to_float(na) * clasp_to_float(nb)); }
  case_Bignum_v_DoubleFloat:
  case_Ratio_v_DoubleFloat : { return DoubleFloat_O::create(clasp_to_double(na) * clasp_to_double(nb)); }
  case_Ratio_v_Fixnum:
  case_Ratio_v_Bignum : {
    Ratio_sp rat = gc::As_unsafe<Ratio_sp>(na);
    Integer_sp new_num = gc::As_unsafe<Integer_sp>(contagion_mul(nb, rat->numerator()));
    return Rational_O::create(new_num, rat->denominator());
  }
  case_Ratio_v_Ratio : {
    Ratio_sp ra = gc::As<Ratio_sp>(na);
    Ratio_sp rb = gc::As<Ratio_sp>(nb);
    Number_sp num = contagion_mul(ra->numerator(), rb->numerator());
    Number_sp den = contagion_mul(ra->denominator(), rb->denominator());
    return Rational_O::create(gc::As_unsafe<Integer_sp>(num), gc::As_unsafe<Integer_sp>(den));
  }
  case_SingleFloat_v_Fixnum:
  case_SingleFloat_v_Bignum:
  case_SingleFloat_v_Ratio:
  case_SingleFloat_v_SingleFloat:
    return clasp_make_single_float(clasp_to_float(na) * clasp_to_float(nb));
  case_SingleFloat_v_DoubleFloat:
  case_DoubleFloat_v_Fixnum:
  case_DoubleFloat_v_Bignum:
  case_DoubleFloat_v_Ratio:
  case_DoubleFloat_v_SingleFloat:
  case_DoubleFloat_v_DoubleFloat:
    return DoubleFloat_O::create(clasp_to_double(na) * clasp_to_double(nb));
#ifdef CLASP_LONG_FLOAT
  case_Fixnum_v_LongFloat:
  case_Bignum_v_LongFloat:
  case_Ratio_v_LongFloat:
  case_SingleFloat_v_LongFloat:
  case_DoubleFloat_v_LongFloat:
  case_LongFloat_v_Fixnum:
  case_LongFloat_v_Ratio:
  case_LongFloat_v_SingleFloat:
  case_LongFloat_v_DoubleFloat:
  case_LongFloat_v_LongFloat:
    return LongFloat_O::create(na->as_long_float() * nb->as_long_float());
#endif
  case_Complex_v_LongFloat:
  case_Complex_v_Fixnum:
  case_Complex_v_Bignum:
  case_Complex_v_Ratio:
  case_Complex_v_SingleFloat:
  case_Complex_v_DoubleFloat : {
    Number_sp aux = na;
    na = nb;
    nb = aux;
    goto Complex_v_Y;
  }
  case_Fixnum_v_Complex:
  case_Bignum_v_Complex:
  case_Ratio_v_Complex:
  case_SingleFloat_v_Complex:
  case_DoubleFloat_v_Complex:
  case_LongFloat_v_Complex:
  Complex_v_Y:
    return clasp_make_complex(gc::As<Real_sp>(contagion_mul(na, gc::As<Complex_sp>(nb)->real())),
                              gc::As<Real_sp>(contagion_mul(na, gc::As<Complex_sp>(nb)->imaginary())));
  case_Complex_v_Complex : {
    Complex_sp ca = gc::As<Complex_sp>(na);
    Complex_sp cb = gc::As<Complex_sp>(nb);
    Real_sp x = ca->real();
    Real_sp y = ca->imaginary();
    Real_sp u = cb->real();
    Real_sp v = cb->imaginary();
    // (x + yi)(u + vi) = (xu - yv) + (xv + yu)i.
    return clasp_make_complex(gc::As<Real_sp>(contagion_sub(contagion_mul(x, u), contagion_mul(y, v))),
                              gc::As<Real_sp>(contagion_add(contagion_mul(x, v), contagion_mul(y, u))));
  } break;
  default:
    not_comparable_error(na, nb);
  };
  MATH_DISPATCH_END();
}

// Forward declaration for contagion_div
Number_sp contagion_div(Number_sp na, Number_sp nb);

Number_sp complex_divide(Real_sp ar, Real_sp ai, Real_sp br, Real_sp bi) {
  // Compute (ar+ai*i)/(br+bi*i).
  // Just multiply the numerator and denominator by (br - bi*i)
  // to end up with ar*br+ai*bi/z real and ai*br-ar*bi imaginary,
  // where z is br^2+bi*2.
#define realmul(A, B) gc::As_unsafe<Real_sp>(contagion_mul((A), (B)))
#define realadd(A, B) gc::As_unsafe<Real_sp>(contagion_add((A), (B)))
#define realsub(A, B) gc::As_unsafe<Real_sp>(contagion_sub((A), (B)))
#define realdiv(A, B) gc::As_unsafe<Real_sp>(contagion_div((A), (B)))
  Real_sp absB2 = realadd(realmul(br, br), realmul(bi, bi));
  Real_sp rnum = realadd(realmul(ar, br), realmul(ai, bi));
  Real_sp inum = realsub(realmul(ai, br), realmul(ar, bi));
  Real_sp realpart = realdiv(rnum, absB2);
  // note: could save a bit of time by checking if inum is zero,
  // and if so not bothering to compute imagpart
  Real_sp imagpart = realdiv(inum, absB2);
  return clasp_make_complex(realpart, imagpart);
#undef realmul
#undef realadd
#undef realsub
#undef realdiv
}

CL_NAME("TWO-ARG-/");
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN Number_sp contagion_div(Number_sp na, Number_sp nb) {
  MATH_DISPATCH_BEGIN(na, nb) {
  case_Fixnum_v_Fixnum:
  case_Bignum_v_Fixnum:
  case_Fixnum_v_Bignum:
  case_Bignum_v_Bignum:
    return Rational_O::create(gc::As_unsafe<Integer_sp>(na), gc::As_unsafe<Integer_sp>(nb));
  case_Fixnum_v_Ratio:
  case_Bignum_v_Ratio:
    return Rational_O::create(gc::As<Integer_sp>(contagion_mul(na, gc::As<Ratio_sp>(nb)->denominator())),
                              gc::As<Ratio_sp>(nb)->numerator());
  case_Fixnum_v_SingleFloat:
    return clasp_make_single_float(clasp_to_float(na) / clasp_to_float(nb));
  case_Fixnum_v_DoubleFloat:
    return DoubleFloat_O::create(clasp_to_double(na) / clasp_to_double(nb));
  case_Bignum_v_SingleFloat:
  case_Ratio_v_SingleFloat:
    return clasp_make_single_float(clasp_to_float(na) / clasp_to_float(nb));
  case_Bignum_v_DoubleFloat:
  case_Ratio_v_DoubleFloat:
    return DoubleFloat_O::create(clasp_to_double(na) / clasp_to_double(nb));
  case_Ratio_v_Fixnum:
  case_Ratio_v_Bignum : {
    Integer_sp z = gc::As<Integer_sp>(contagion_mul(gc::As<Ratio_sp>(na)->denominator(), nb));
    return Rational_O::create(gc::As<Ratio_sp>(na)->numerator(), z);
  }
  case_Ratio_v_Ratio : {
    Ratio_sp ra = gc::As<Ratio_sp>(na);
    Ratio_sp rb = gc::As<Ratio_sp>(nb);
    Integer_sp num = gc::As<Integer_sp>(contagion_mul(ra->numerator(), rb->denominator()));
    Integer_sp denom = gc::As<Integer_sp>(contagion_mul(ra->denominator(), rb->numerator()));
    return Rational_O::create(num, denom);
  }
  case_SingleFloat_v_Fixnum:
  case_SingleFloat_v_Ratio:
  case_SingleFloat_v_Bignum:
  case_SingleFloat_v_SingleFloat:
    return clasp_make_single_float(clasp_to_float(na) / clasp_to_float(nb));
  case_SingleFloat_v_DoubleFloat:
  case_DoubleFloat_v_Fixnum:
  case_DoubleFloat_v_Ratio:
  case_DoubleFloat_v_Bignum:
  case_DoubleFloat_v_SingleFloat:
  case_DoubleFloat_v_DoubleFloat:
    return DoubleFloat_O::create(clasp_to_double(na) / clasp_to_double(nb));
#ifdef CLASP_LONG_FLOAT
  case_Fixnum_v_LongFloat:
  case_Ratio_v_LongFloat:
  case_SingleFloat_v_LongFloat:
  case_DoubleFloat_v_LongFloat:
  case_LongFloat_v_Fixnum:
  case_LongFloat_v_Ratio:
  case_LongFloat_v_SingleFloat:
  case_LongFloat_v_DoubleFloat:
  case_LongFloat_v_LongFloat:
    return LongFloat_O::create(na->as_long_float() / nb->as_long_float());
#endif
  case_Complex_v_Fixnum:
  case_Complex_v_Bignum:
  case_Complex_v_Ratio:
  case_Complex_v_SingleFloat:
  case_Complex_v_DoubleFloat:
  case_Complex_v_LongFloat : {
    Complex_sp ca = gc::As<Complex_sp>(na);
    return clasp_make_complex(gc::As<Real_sp>(contagion_div(ca->real(), nb)), gc::As<Real_sp>(contagion_div(ca->imaginary(), nb)));
  }
  case_Fixnum_v_Complex:
  case_Bignum_v_Complex:
  case_Ratio_v_Complex:
  case_SingleFloat_v_Complex:
  case_DoubleFloat_v_Complex:
  case_LongFloat_v_Complex : {
    Complex_sp cb = gc::As_unsafe<Complex_sp>(nb);
    return complex_divide(gc::As_unsafe<Real_sp>(na), clasp_make_fixnum(0), cb->real(), cb->imaginary());
  }
  case_Complex_v_Complex : {
    Complex_sp ca = gc::As<Complex_sp>(na);
    Complex_sp cb = gc::As<Complex_sp>(nb);
    return complex_divide(ca->real(), ca->imaginary(), cb->real(), cb->imaginary());
  }
  }
  MATH_DISPATCH_END();
  not_comparable_error(na, nb);
}

CL_LAMBDA(&rest numbers);
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN Number_sp cl___PLUS_(List_sp numbers) {
  if (!numbers.consp())
    return make_fixnum(0);
  Number_sp result = gc::As<Number_sp>(oCar(numbers));
  for (auto cur : (List_sp)oCdr(numbers)) {
    result = contagion_add(result, gc::As<Number_sp>(oCar(cur)));
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
    result = contagion_mul(result, gc::As<Number_sp>(oCar(cur)));
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
    result = contagion_sub(result, gc::As<Number_sp>(oCar(cur)));
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
    result = contagion_div(result, gc::As<Number_sp>(oCar(cur)));
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

/*
 * In Common Lisp, comparisons between floats and integers are performed
 * via an intermediate rationalization of the floating point number. In C,
 * on the other hand, the comparison is performed by converting the integer
 * into a floating point number. However, if the double type is too small
 * this may lead to a loss of precision and two numbers being told equal
 * when, by Common Lisp standards, would not.
 */
static int double_fix_compare(Fixnum n, double d) {
  if ((double)n < d) {
    return -1;
  } else if ((double)n > d) {
    return +1;
  } else if (sizeof(double) > sizeof(Fixnum)) {
    return 0;
  } else {
    /* When we reach here, the double type has no
     * significant decimal part. However, as explained
     * above, the double type is too small and integers
     * may coerce to the same double number giving a false
     * positive. Hence we perform the comparison in
     * integer space. */
    Fixnum m = d;
    if (n == m) {
      return 0;
    } else if (n > m) {
      return +1;
    } else {
      return -1;
    }
  }
}

#ifdef CLASP_LONG_FLOAT
static int long_double_fix_compare(Fixnum n, LongFloat d) {
  if ((LongFloat)n < d) {
    return -1;
  } else if ((LongFloat)n > d) {
    return +1;
  } else if (sizeof(LongFloat) > sizeof(Fixnum)) {
    return 0;
  } else {
    Fixnum m = d;
    if (n == m) {
      return 0;
    } else if (n > m) {
      return +1;
    } else {
      return -1;
    }
  }
}
#endif

/* ----------------------------------------------------------------------

   basic_compare

*/

/*! Return -1 if a<b
      0 if a == b
      +1 if a > b
    */
int basic_compare(Number_sp na, Number_sp nb) {
  MATH_DISPATCH_BEGIN(na, nb) {
  case_Fixnum_v_Fixnum : {
    gctools::Fixnum fa = unbox_fixnum(gc::As<Fixnum_sp>(na));
    gctools::Fixnum fb = unbox_fixnum(gc::As<Fixnum_sp>(nb));
    if (fa < fb)
      return -1;
    else if (fa > fb)
      return 1;
    else
      return 0;
  }
  case_Fixnum_v_Bignum : {
    // Bignums are outside the range of fixnums, so this is easy.
    // That is, negative bignums are < all fixnums,
    // and positive bignums are > all fixnums.
    Bignum_sp bb = gc::As_unsafe<Bignum_sp>(nb);
    if (bb->minusp_())
      return 1;
    else
      return -1;
  }
  case_Fixnum_v_Ratio:
  case_Bignum_v_Ratio : {
    Ratio_sp rb = gc::As<Ratio_sp>(nb);
    Integer_sp trunc = clasp_integer_divide(rb->numerator(), rb->denominator());
    int res = basic_compare(na, trunc);
    if (res == 0)
      return (clasp_minusp(rb) ? 1 : -1);
    else
      return res;
  }
  // We can't use C's comparison because it promotes ints to floats,
  // which is the opposite of how CL is defined.
  case_Fixnum_v_SingleFloat : {
    float b = nb.unsafe_single_float();
    // If b is out of range, this is easy. Also covers infinities.
    // We do this instead of the more obvious most_positive_fixnum
    // comparison mpf, being not-a-power-of-two, cannot be exactly
    // represented and clang whines about that.
    if (b > ((gc::Fixnum)1 << gc::fixnum_bits))
      return -1;
    else if (b < -((gc::Fixnum)1 << gc::fixnum_bits))
      return 1;

    gctools::Fixnum a = na.unsafe_fixnum();
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
  // FIXME: Efficiency
  case_Bignum_v_SingleFloat:
  case_Ratio_v_SingleFloat : {
    float s = nb.unsafe_single_float();
    if (std::isinf(s)) {
      if (s > 0.0f)
        return -1;
      else
        return 1;
    } else
      return basic_compare(na, DoubleFloat_O::rational(s));
  }
  case_Fixnum_v_DoubleFloat : {
    double b = clasp_to_double(nb);
    if (b > ((gc::Fixnum)1 << gc::fixnum_bits))
      return -1;
    else if (b < -((gc::Fixnum)1 << gc::fixnum_bits))
      return 1;

    gctools::Fixnum a = na.unsafe_fixnum();
    gctools::Fixnum ib = b;
    if (a < ib)
      return -1;
    else if (a > ib)
      return 1;
    else if (b > ib)
      return -1;
    else if (b < ib)
      return 1;
    else
      return 0;
  }
  case_Bignum_v_DoubleFloat:
  case_Ratio_v_DoubleFloat : {
    DoubleFloat_sp d = gc::As_unsafe<DoubleFloat_sp>(nb);
    if (d->isinf_()) {
      if (d->plusp_())
        return -1;
      else
        return 1;
    } else
      return basic_compare(na, d->rational_());
  }
  case_Bignum_v_Fixnum : {
    Bignum_sp ba = gc::As_unsafe<Bignum_sp>(na);
    if (ba->plusp_())
      return 1;
    else
      return -1;
  }
  case_Bignum_v_Bignum:
    return core__next_compare(gc::As_unsafe<Bignum_sp>(na), gc::As_unsafe<Bignum_sp>(nb));
  case_Ratio_v_Fixnum:
  case_Ratio_v_Bignum : {
    Ratio_sp ra = gc::As<Ratio_sp>(na);
    Integer_sp trunc = clasp_integer_divide(ra->numerator(), ra->denominator());
    int res = basic_compare(trunc, nb);
    if (res == 0)
      return (clasp_minusp(ra) ? -1 : 1);
    else
      return res;
  }
  case_Ratio_v_Ratio : {
    // First, divide through the ratios and compare those.
    // That can give us an answer not requiring consing larger numbers.
    // Failing that, use a/b <=> c/d is equivalent to ad <=> bc.
    Ratio_sp ra = gc::As<Ratio_sp>(na);
    Ratio_sp rb = gc::As<Ratio_sp>(nb);
    Integer_sp ta = clasp_integer_divide(ra->numerator(), ra->denominator());
    Integer_sp tb = clasp_integer_divide(rb->numerator(), rb->denominator());
    int res = basic_compare(ta, tb);
    if (res != 0)
      return res;
    else {
      Number_sp left = contagion_mul(ra->numerator(), rb->denominator());
      Number_sp right = contagion_mul(rb->numerator(), ra->denominator());
      return basic_compare(left, right);
    }
  }
  case_SingleFloat_v_Fixnum : {
    float a = na.unsafe_single_float();
    if (a > ((gc::Fixnum)1 << gc::fixnum_bits))
      return 1;
    else if (a < -((gc::Fixnum)1 << gc::fixnum_bits))
      return -1;

    gctools::Fixnum b = nb.unsafe_fixnum();
    gctools::Fixnum ia = a;
    if (ia < b)
      return -1;
    else if (ia > b)
      return 1;
    else if (a > ia)
      return 1;
    else if (a < ia)
      return -1;
    else
      return 0;
  }
  case_SingleFloat_v_Bignum:
  case_SingleFloat_v_Ratio : {
    float s = na.unsafe_single_float();
    if (std::isinf(s)) {
      if (s > 0.0f)
        return 1;
      else
        return -1;
    } else
      return basic_compare(DoubleFloat_O::rational(s), nb);
  }
  case_SingleFloat_v_SingleFloat : {
    float a = na.unsafe_single_float();
    float b = nb.unsafe_single_float();
    if (a < b)
      return -1;
    else if (a > b)
      return 1;
    else
      return 0;
  }
  case_DoubleFloat_v_Fixnum : {
    double a = clasp_to_double(na);
    if (a > ((gc::Fixnum)1 << gc::fixnum_bits))
      return 1;
    else if (a < -((gc::Fixnum)1 << gc::fixnum_bits))
      return -1;

    gctools::Fixnum b = nb.unsafe_fixnum();
    gctools::Fixnum ia = a;
    if (ia < b)
      return -1;
    else if (a > b)
      return 1;
    else if (a > ia)
      return 1;
    else if (a < ia)
      return -1;
    else
      return 0;
  }
  case_DoubleFloat_v_Bignum:
  case_DoubleFloat_v_Ratio : {
    DoubleFloat_sp d = gc::As_unsafe<DoubleFloat_sp>(na);
    if (d->isinf_()) {
      if (d->plusp_())
        return 1;
      else
        return -1;
    } else
      return basic_compare(d->rational_(), nb);
  }
  case_SingleFloat_v_DoubleFloat:
  case_DoubleFloat_v_SingleFloat:
  case_DoubleFloat_v_DoubleFloat : {
    double a = clasp_to_double(na);
    double b = clasp_to_double(nb);
    if (a < b)
      return -1;
    else if (a > b)
      return 1;
    else
      return 0;
  }
#ifdef CLASP_LONG_FLOAT
  case_Fixnum_v_LongFloat:
    return long_double_fix_compare(gc::As<Fixnum_sp>(na)->get(), nb.as<LongFloat_O>()->get());
    break;
  case_LongFloat_v_Fixnum:
    return -long_double_fix_compare(gc::As<Fixnum_sp>(nb)->get(), na.as<LongFloat_O>()->get());
    break;
  case_Bignum_v_LongFloat:
  case_Ratio_v_LongFloat:
  case_SingleFloat_v_LongFloat:
  case_DoubleFloat_v_LongFloat:
  case_LongFloat_v_Ratio:
  case_LongFloat_v_SingleFloat:
  case_LongFloat_v_DoubleFloat:
  case_LongFloat_v_LongFloat : {
    LongFloat a = na->as_long_float();
    LongFloat b = nb->as_long_float();
    if (a < b)
      return -1;
    if (a == b)
      return 0;
    return 1;
  }
#endif
  default:
    not_comparable_error(na, nb);
  };
  MATH_DISPATCH_END();
}

/// this is used for comparison of reals, not for complex, so use Real_sp
T_sp numbers_monotonic(int s, int t, List_sp args) {
  Real_sp c = gc::As<Real_sp>(oCar(args));
  Real_sp d;
  int dir;
  args = oCdr(args);
  while (args.notnilp()) {
    d = gc::As<Real_sp>(oCar(args));
    dir = s * basic_compare(c, d);
    if (dir < t)
      return _lisp->_false();
    c = d;
    args = oCdr(args);
  }
  return _lisp->_true();
};

T_sp numbers_monotonic_vaslist(int s, int t, Vaslist_sp args) {
  Real_sp c = gc::As<Real_sp>(args->next_arg());
  Real_sp d;
  int dir;
  while (args->nargs() > 0) {
    d = gc::As<Real_sp>(args->next_arg());
    dir = s * basic_compare(c, d);
    if (dir < t)
      return _lisp->_false();
    c = d;
  }
  return _lisp->_true();
};

CL_NAME("TWO-ARG-<");
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN bool two_arg__LT_(Number_sp x, Number_sp y) { return basic_compare(x, y) == -1; }

CL_NAME("TWO-ARG-<=");
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN bool two_arg__LE_(Number_sp x, Number_sp y) { return basic_compare(x, y) != 1; }

CL_NAME("TWO-ARG->");
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN bool two_arg__GT_(Number_sp x, Number_sp y) { return basic_compare(x, y) == 1; }

CL_NAME("TWO-ARG->=");
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN bool two_arg__GE_(Number_sp x, Number_sp y) { return basic_compare(x, y) != -1; }

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

/*! Return true if two numbers are equal otherwise false */
bool basic_equalp(Number_sp na, Number_sp nb) {
  MATH_DISPATCH_BEGIN(na, nb) {
  case_Fixnum_v_Fixnum : {
    gctools::Fixnum fa = unbox_fixnum(gc::As<Fixnum_sp>(na));
    gctools::Fixnum fb = unbox_fixnum(gc::As<Fixnum_sp>(nb));
    return fa == fb;
  }
  case_Fixnum_v_Bignum:
  case_Bignum_v_Fixnum:
    // bignums are never in fixnum range.
    return false;
  case_Fixnum_v_Ratio:
  case_Ratio_v_Fixnum:
  case_Bignum_v_Ratio:
  case_Ratio_v_Bignum:
    // Normalized ratios are never integers.
    return false;
  case_Fixnum_v_SingleFloat:
  case_Bignum_v_SingleFloat:
  case_Ratio_v_SingleFloat : {
    float s = nb.unsafe_single_float();
    if (std::isinf(s))
      return false;
    else
      return basic_equalp(na, DoubleFloat_O::rational(s));
  }
  case_Fixnum_v_DoubleFloat:
  case_Bignum_v_DoubleFloat:
  case_Ratio_v_DoubleFloat : {
    DoubleFloat_sp d = gc::As_unsafe<DoubleFloat_sp>(nb);
    if (d->isinf_())
      return false;
    else
      return basic_equalp(na, d->rational_());
  }
  case_Bignum_v_Bignum : { return (core__next_compare(gc::As_unsafe<Bignum_sp>(na), gc::As_unsafe<Bignum_sp>(nb)) == 0); }
  case_Ratio_v_Ratio : {
    // ratios are normalized
    Ratio_sp ra = gc::As<Ratio_sp>(na);
    Ratio_sp rb = gc::As<Ratio_sp>(nb);
    return (basic_equalp(ra->numerator(), rb->numerator()) && basic_equalp(ra->denominator(), rb->denominator()));
  }
  case_SingleFloat_v_Fixnum:
  case_SingleFloat_v_Bignum:
  case_SingleFloat_v_Ratio : {
    float s = na.unsafe_single_float();
    if (std::isinf(s))
      return false;
    else
      return basic_equalp(DoubleFloat_O::rational(s), nb);
  }
  case_SingleFloat_v_SingleFloat : {
    float a = clasp_to_float(na);
    float b = clasp_to_float(nb);
    return a == b;
  }
  case_DoubleFloat_v_Fixnum:
  case_DoubleFloat_v_Bignum:
  case_DoubleFloat_v_Ratio : {
    DoubleFloat_sp d = gc::As_unsafe<DoubleFloat_sp>(na);
    if (d->isinf_())
      return false;
    else
      return basic_equalp(d->rational_(), nb);
  }
  case_SingleFloat_v_DoubleFloat:
  case_DoubleFloat_v_SingleFloat:
  case_DoubleFloat_v_DoubleFloat : {
    double a = clasp_to_double(na);
    double b = clasp_to_double(nb);
    return a == b;
  }
  case_Fixnum_v_LongFloat:
  case_Bignum_v_LongFloat:
  case_Ratio_v_LongFloat:
  case_SingleFloat_v_LongFloat:
  case_DoubleFloat_v_LongFloat:
  case_LongFloat_v_Fixnum:
  case_LongFloat_v_Ratio:
  case_LongFloat_v_SingleFloat:
  case_LongFloat_v_DoubleFloat:
  case_LongFloat_v_LongFloat : {
    LongFloat a = clasp_to_long_float(na);
    LongFloat b = clasp_to_long_float(nb);
    return a == b;
  }
  case_Complex_v_LongFloat:
  case_Complex_v_Fixnum:
  case_Complex_v_Bignum:
  case_Complex_v_Ratio:
  case_Complex_v_SingleFloat:
  case_Complex_v_DoubleFloat : {
    Number_sp aux = na;
    na = nb;
    nb = aux;
    goto Complex_v_Y;
  }
  case_Fixnum_v_Complex:
  case_Bignum_v_Complex:
  case_Ratio_v_Complex:
  case_SingleFloat_v_Complex:
  case_DoubleFloat_v_Complex:
  case_LongFloat_v_Complex:
  Complex_v_Y:
    return (clasp_zerop(gc::As<Complex_sp>(nb)->imaginary()) && basic_equalp(na, gc::As<Complex_sp>(nb)->real()));
  case_Complex_v_Complex:
    return (basic_equalp(gc::As<Complex_sp>(na)->real(), gc::As<Complex_sp>(nb)->real()) &&
            basic_equalp(gc::As<Complex_sp>(na)->imaginary(), gc::As<Complex_sp>(nb)->imaginary()));
  default:
    not_comparable_error(na, nb);
  };
  MATH_DISPATCH_END();
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

Number_sp Number_O::create(double val) { return DoubleFloat_O::create(val); }

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

Integer_sp Integer_O::create(std::floating_point auto v) {
  // Why >= and <? Because most-negative-fixnum is a negative power of
  // two, exactly representable by a float. most-positive-fixnum is
  // slightly less than a positive power of two. So (double)mpf is a
  // double that, cast to an integer, will be (1+ mpf). We want a bignum
  // out of that.
  if (v >= static_cast<decltype(v)>(gc::most_negative_fixnum) &&
      v < static_cast<decltype(v)>(gc::most_positive_fixnum))
    return clasp_make_fixnum(v);
  else
    return Bignum_O::create(v);
}

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

// ------------------------------------------------------------------------

Number_sp ShortFloat_O::reciprocal_() const { return ShortFloat_O::create(1.0 / this->_Value); }

Number_sp ShortFloat_O::signum_() const { return ShortFloat_O::create(this->_Value > 0.0 ? 1 : (this->_Value < 0.0 ? -1 : 0)); }

float ShortFloat_O::as_float_() const { return (float)this->_Value; }

double ShortFloat_O::as_double_() const { return (double)this->_Value; }

LongFloat ShortFloat_O::as_long_float_() const { return (LongFloat)this->_Value; }

CL_LISPIFY_NAME("core:castToInteger");
CL_DEFMETHOD Integer_sp ShortFloat_O::castToInteger() const {
  if (this->_Value < 0) {
    float f = -this->_Value;
    int cf = *(int*)&f;
    return gc::As<Integer_sp>(clasp_negate(Integer_O::create((gc::Fixnum)cf)));
  }
  int cf = *(int*)&this->_Value;
  return Integer_O::create((gc::Fixnum)cf);
}

Number_sp ShortFloat_O::abs_() const { return ShortFloat_O::create(fabs(this->_Value)); }

void ShortFloat_O::sxhash_(HashGenerator& hg) const { hg.addValue(std::abs(::floor(this->_Value))); }

bool ShortFloat_O::eql_(T_sp obj) const {
  if (this->eq(obj))
    return true;
  if (gc::IsA<Number_sp>(obj)) {
    Number_sp num = gc::As<Number_sp>(obj);
    return this->get() == clasp_to_double(num);
  }
  return false;
}

string ShortFloat_O::__repr__() const {
  stringstream ss;
  ss << this->_Value;
  return ss.str();
}

//--------------------------------------------------

Number_sp DoubleFloat_O::reciprocal_() const { return DoubleFloat_O::create(1.0 / this->_Value); }

float DoubleFloat_O::as_float_() const { return (float)this->_Value; }

double DoubleFloat_O::as_double_() const { return (double)this->_Value; }

LongFloat DoubleFloat_O::as_long_float_() const { return (LongFloat)this->_Value; }

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

Number_sp DoubleFloat_O::signum_() const { return DoubleFloat_O::create(this->_Value > 0.0 ? 1 : (this->_Value < 0.0 ? -1 : 0)); }

void DoubleFloat_O::sxhash_(HashGenerator& hg) const { hg.addValue(std::abs(::floor(this->_Value))); }

bool DoubleFloat_O::eql_(T_sp obj) const {
  if (this->eq(obj))
    return true;
  if (DoubleFloat_sp other = obj.asOrNull<DoubleFloat_O>()) {
    ASSERT(sizeof(this->_Value) == sizeof(int64_t));
    int64_t me = *(int64_t*)(&this->_Value);
    int64_t them = *(int64_t*)(&other->_Value);
    return me == them;
  }
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
float LongFloat_O::as_float() const { return (float)this->_Value; }

double LongFloat_O::as_double() const { return (double)this->_Value; }

LongFloat LongFloat_O::as_long_float() const { return (LongFloat)this->_Value; }

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

Number_sp LongFloat_O::copy() const { return LongFloat_O::create(this->_Value); }

Number_sp LongFloat_O::reciprocal_() const { return LongFloat_O::create(1.0 / this->_Value); }

string LongFloat_O::valueAsString() const {
  stringstream ss;
  ss << this->_Value;
  return ss.str();
}

Number_sp LongFloat_O::abs() const { return LongFloat_O::create(fabs(this->_Value)); }

void LongFloat_O::sxhash(HashGenerator& hg) const { hg.addValue(std::abs(::floor(this->_Value))); }

bool LongFloat_O::eql(T_sp obj) const {
  if (this->eq(obj))
    return true;
  if (gc::IsA<Number_sp>(obj)) {
    Number_sp num = obj.as<Number_O>();
    return this->get() == num->as_double();
  }
  return false;
}

bool LongFloat_O::eqn(T_sp obj) const {
  if (core__long_float_p(obj)) {
    LongFloat_sp t = obj.as<LongFloat_O>();
    return this->get() == t->get();
  } else if (core__fixnump(obj)) {
    Fixnum_sp t = gc::As<Fixnum_sp>(obj);
    return this->get() == t->get();
  }
  ASSERT(!cl__numberp(obj));
  return false;
}

Number_sp LongFloat_O::signum() const { return LongFloat_O::create(this->_Value > 0.0 ? 1 : (this->_Value < 0.0 ? -1 : 0)); }

string LongFloat_O::__repr__() const {
  stringstream ss;
  ss << this->_Value;
  return ss.str();
}
#endif

// --------------------------------------------------------------------------------

float Ratio_O::as_float_() const {
  double d = this->as_double_();
  return d;
}

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
  if (clasp_minusp(num)) {
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
  if (clasp_oddp(quotient)) {
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

double Ratio_O::as_double_() const {
  if ((this->_numerator).fixnump() && (this->_denominator).fixnump()) {
    double d = clasp_to_double(this->_numerator);
    d /= clasp_to_double(this->_denominator);
    return d;
  } else {
    gc::Fixnum exponent;
    Integer_sp mantissa = mantissa_and_exponent_from_ratio(this->_numerator, this->_denominator, DBL_MANT_DIG, &exponent);
    double output;
    if (mantissa.fixnump())
      output = mantissa.unsafe_fixnum();
    else
      output = mantissa->as_double_();
    return ldexp(output, exponent);
  }
}

LongFloat Ratio_O::as_long_float_() const {
  double d = this->as_double_();
  return d;
}

string Ratio_O::__repr__() const {
  stringstream ss;
  ss << _rep_(this->_numerator) << "/" << _rep_(this->_denominator);
  return ss.str();
}

Number_sp Ratio_O::abs_() const {
  return Ratio_O::create_primitive(gc::As_unsafe<Integer_sp>(clasp_abs(gc::As<Integer_sp>(this->_numerator))), this->_denominator);
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
  ASSERT(clasp_plusp(this->_denominator));
  return clasp_signum(this->_numerator);
}

Number_sp Ratio_O::sqrt_() const { return float_sqrt(this->as_float_()); }

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
  if (clasp_minusp(num)) {
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
  if (clasp_minusp(idenom)) {
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
    return clasp_divide(this->asSmartPtr(), this->abs_());
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

Number_sp Complex_O::abs_() const {
  return clasp_sqrt(clasp_plus(clasp_times(this->_real, this->_real), clasp_times(this->_imaginary, this->_imaginary)));
}

Number_sp Complex_O::reciprocal_() const {
  // 1/(a+bi) = (a-bi)/(a^2+b^2) by basic algebra.
  // alternately we could just clasp_divide. I dunno if reciprocal_ is terribly necessary.
  Real_sp square_modulus =
      gc::As_unsafe<Real_sp>(clasp_plus(clasp_times(this->_real, this->_real), clasp_times(this->_imaginary, this->_imaginary)));
  return Complex_O::create(gc::As_unsafe<Real_sp>(clasp_divide(this->_real, square_modulus)),
                           gc::As_unsafe<Real_sp>(clasp_divide(clasp_negate(this->_imaginary), square_modulus)));
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
  if (clasp_minusp(this->asSmartPtr())) {
    Number_sp x = clasp_sqrt(clasp_negate(this->asSmartPtr()));
    return clasp_make_complex(DoubleFloat_O::create(0.0), gc::As<Real_sp>(x));
  } else {
    return DoubleFloat_O::create(::sqrt(this->_Value));
  }
}

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::sqrt_() const {
  if (this->minusp()) {
    Number_sp x = this->negate()->sqrt();
    return clasp_make_complex(LongFloat_O::create(0.0), x.as<Real_O>());
  } else {
    return LongFloat_O::create(sqrtl(this->_Value));
  }
}
#endif

Number_sp Complex_O::sqrt_() const { return cl__expt(this->asSmartPtr(), _lisp->plusHalf()); }
Number_sp Bignum_O::sqrt_() const {
  // Could move the <0 logic out to another function, to share
  // hypothetically we could use mpn_sqrtrem instead, but i imagine it's slower.
  // We convert to a double for maximum range, but return a single as required
  // by CLHS.
  double z = this->as_double_();
  if (z < 0)
    return clasp_make_complex(clasp_make_single_float(0.0), clasp_make_single_float(sqrt(-z)));
  else
    return clasp_make_single_float(sqrt(z));
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
CL_DEFUN Number_sp cl__sqrt(Number_sp x) { return clasp_sqrt(x); };

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

Number_sp Rational_O::sin_() const { return clasp_make_single_float(sinf(this->as_float_())); }

Number_sp DoubleFloat_O::sin_() const { return DoubleFloat_O::create(::sin(this->_Value)); }

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::sin_() const { return LongFloat_O::create(sinl(this->_Value)); }
#endif

Number_sp Complex_O::sin_() const {
  /*
          z = x + I y
          z = x + I y
          sin(z) = sinh(I z) = sinh(-y + I x)
        */
  Number_sp dx = this->_real;
  Number_sp dy = this->_imaginary;
  Number_sp a = clasp_times(clasp_sin(dx), clasp_cosh(dy)); // clasp_sin(dx), clasp_cosh(dy));
  Number_sp b = clasp_times(clasp_cos(dx), clasp_sinh(dy)); // clasp_cos(dx), clasp_sinh(dy));
  return clasp_make_complex(gc::As<Real_sp>(a), gc::As<Real_sp>(b));
}

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(sin)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp cl__sin(Number_sp x) { return clasp_sin(x); }

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

Number_sp Rational_O::cos_() const { return clasp_make_single_float(cosf(this->as_float_())); }

Number_sp DoubleFloat_O::cos_() const { return DoubleFloat_O::create(::cos(this->_Value)); }

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::cos_() const { return LongFloat_O::create(cosl(this->_Value)); }
#endif

Number_sp Complex_O::cos_() const {
  /* z = x + I y
           cos(z) = cosh(I z) = cosh(-y + I x)
        */
  Number_sp dx = this->_real;
  Number_sp dy = this->_imaginary;
  Number_sp a = clasp_times(clasp_cos(dx), clasp_cosh(dy));               // clasp_cos(dx), clasp_cosh(dy));
  Number_sp b = clasp_times(clasp_negate(clasp_sin(dx)), clasp_sinh(dy)); // clasp_negate(clasp_sin(dx)), clasp_sinh(dy));
  return clasp_make_complex(gc::As<Real_sp>(a), gc::As<Real_sp>(b));      // clasp_make_complex(a, b);
}

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(cos)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp cl__cos(Number_sp x) { return clasp_cos(x); }

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

Number_sp Rational_O::tan_() const { return clasp_make_single_float(safe_tanf(this->as_float_())); }

Number_sp DoubleFloat_O::tan_() const { return DoubleFloat_O::create(::tan(this->_Value)); }

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::tan_() const { return LongFloat_O::create(tanl(this->_Value)); }
#endif

Number_sp Complex_O::tan_() const {
  Number_sp a = this->sin_();
  Number_sp b = this->cos_();
  return clasp_divide(a, b);
}

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(tan)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp cl__tan(Number_sp x) { return clasp_tan(x); }

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

Number_sp Rational_O::sinh_() const { return clasp_make_single_float(sinhf(this->as_float_())); }

Number_sp DoubleFloat_O::sinh_() const { return DoubleFloat_O::create(::sinh(this->_Value)); }

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::sinh_() const { return LongFloat_O::create(sinhl(this->_Value)); }
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
  Number_sp a = clasp_times(clasp_sinh(dx), clasp_cos(dy));
  Number_sp b = clasp_times(clasp_cosh(dx), clasp_sin(dy));
  return clasp_make_complex(gc::As<Real_sp>(a), gc::As<Real_sp>(b));
}

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(sinh)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp cl__sinh(Number_sp x) { return clasp_sinh(x); }

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

Number_sp Rational_O::cosh_() const { return clasp_make_single_float(coshf(this->as_float_())); }

Number_sp DoubleFloat_O::cosh_() const { return DoubleFloat_O::create(::cosh(this->_Value)); }

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::cosh_() const { return LongFloat_O::create(coshl(this->_Value)); }
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
  Number_sp a = clasp_times(clasp_cosh(dx), clasp_cos(dy));          // clasp_cosh(dx), clasp_cos(dy));
  Number_sp b = clasp_times(clasp_sinh(dx), clasp_sin(dy));          // clasp_sinh(dx), clasp_sin(dy));
  return clasp_make_complex(gc::As<Real_sp>(a), gc::As<Real_sp>(b)); // clasp_make_complex(a, b);
}

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(cosh)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp cl__cosh(Number_sp x) { return clasp_cosh(x); }

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

Number_sp Rational_O::tanh_() const { return clasp_make_single_float(tanhf(this->as_float_())); }

Number_sp DoubleFloat_O::tanh_() const { return DoubleFloat_O::create(::tanh(this->_Value)); }

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::tanh_() const { return LongFloat_O::create(tanhl(this->_Value)); }
#endif

Number_sp Complex_O::tanh_() const {
  Number_sp a = this->sinh_();
  Number_sp b = this->cosh_();
  return clasp_divide(a, b);
}

CL_LAMBDA(x);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(tanh)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp cl__tanh(Number_sp x) { return clasp_tanh(x); }

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

Number_sp Rational_O::exp_() const { return clasp_make_single_float(expf(this->as_float_())); }

Number_sp DoubleFloat_O::exp_() const { return DoubleFloat_O::create(::exp(this->_Value)); }

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::exp_() const { return LongFloat_O::create(expl(this->_Value)); }
#endif

Number_sp Complex_O::exp_() const {
  Real_sp y, y1;
  y = this->_imaginary;
  Real_sp x = gc::As<Real_sp>(clasp_exp(this->_real));
  y1 = gc::As<Real_sp>(clasp_cos(y)); // clasp_cos(y);
  y = gc::As<Real_sp>(clasp_sin(y));  // clasp_sin(y);
  Complex_sp cy = gc::As_unsafe<Complex_sp>(clasp_make_complex(y1, y));
  return clasp_times(x, cy);
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
  NumberType ty, tx;
  Number_sp z;
  ty = clasp_t_of(y);
  tx = clasp_t_of(x);
  /* INV: The most specific numeric types come first. */
  switch ((ty > tx) ? ty : tx) {
  case number_Fixnum:
  case number_Bignum:
  case number_Ratio:
    return clasp_make_fixnum(1);
  case number_SingleFloat:
    return clasp_make_single_float(1.0f);
  case number_DoubleFloat:
    return DoubleFloat_O::create(1.0);
#ifdef CLASP_LONG_FLOAT
  case number_LongFloat:
    return LongFloat_O::create(1.0);
#endif
  case number_Complex:
    z = expt_zero((tx == number_Complex) ? gc::As<Number_sp>(gc::As<Complex_sp>(x)->real()) : x,
                  (ty == number_Complex) ? gc::As<Number_sp>(gc::As<Complex_sp>(y)->real()) : y);
    return clasp_make_complex(gc::As<Real_sp>(z), clasp_make_fixnum(0));
  default:
      UNREACHABLE();
  }
}

Number_sp clasp_expt(Number_sp x, Number_sp y) {
  NumberType ty, tx;
  Number_sp z;
  if (clasp_unlikely(clasp_zerop(y))) {
    return expt_zero(x, y);
  }
  ty = clasp_t_of(y);
  tx = clasp_t_of(x);
  if (clasp_zerop(x)) {
    z = clasp_times(x, y);
    if (!clasp_plusp((ty == number_Complex) ? gc::As<Complex_sp>(y)->real() : gc::As<Real_sp>(y)))
      z = clasp_divide(clasp_make_fixnum(1), z);
  } else if (ty != number_Fixnum && ty != number_Bignum) {
    // Use the general definition, a^b = exp(b log(a))
    /* The following could be just
           z = clasp_log1(x);
           however, Maxima expects EXPT to have double accuracy
           when the first argument is integer and the second
           is double-float */
    z = clasp_log1(clasp_times(x, expt_zero(x, y)));
    z = clasp_times(z, y);
    z = cl__exp(z);
  } else if (clasp_minusp(gc::As<Real_sp>(y))) {
    z = clasp_negate(y);
    z = clasp_expt(x, z);
    z = clasp_reciprocal(z);
  } else {
    z = clasp_make_fixnum(1);
    Integer_sp iy = gc::As<Integer_sp>(y);
    do {
      // Exponentiation by squaring.
      if (!clasp_evenp(iy))
        z = clasp_times(z, x);
      iy = clasp_shift_right(iy, 1); // divide by two
      if (clasp_zerop(iy))
        break;
      x = clasp_times(x, x);
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

static double clasp_atan2_double(double y, double x) {
  if (std::signbit(x)) {
    if (std::signbit(y)) {
      return -CLASP_PI_D + atan(-y / -x);
    } else if (y == 0) {
      return CLASP_PI_D;
    } else {
      return CLASP_PI_D - atan(y / -x);
    }
  } else if (x == 0) {
    if (std::signbit(y)) {
      return -CLASP_PI2_D;
    } else if (y == 0) {
      return x / y; /* Produces a NaN */
    } else {
      return CLASP_PI2_D;
    }
  } else {
    if (std::signbit(y)) {
      return -atan(-y / x);
    } else if (y == 0) {
      return (double)0;
    } else {
      return atan(y / x);
    }
  }
}

#ifdef CLASP_LONG_FLOAT
static LongFloat clasp_atan2_LongFloat(LongFloat y, LongFloat x) {
  if (signbit(x)) {
    if (signbit(y)) {
      return -CLASP_PI_L + atanl(-y / -x);
    } else if (y == 0) {
      return CLASP_PI_L;
    } else {
      return CLASP_PI_L - atanl(y / -x);
    }
  } else if (x == 0) {
    if (signbit(y)) {
      return -CLASP_PI2_L;
    } else if (y == 0) {
      return x / y; /* Produces a NaN */
    } else {
      return CLASP_PI2_L;
    }
  } else {
    if (signbit(y)) {
      return -atanl(-y / x);
    } else if (y == 0) {
      return (LongFloat)0;
    } else {
      return atanl(y / x);
    }
  }
}
#endif

Number_sp clasp_atan2(Number_sp y, Number_sp x) {
  Number_sp output;
  {
#ifdef CLASP_LONG_FLOAT
    NumberType tx = clasp_t_of(x);
    NumberType ty = clasp_t_of(y);
    if (tx < ty)
      tx = ty;
    if (tx == number_LongFloat) {
      LongFloat d = clasp_atan2_LongFloat(y->as_long_float(), x->as_long_float());
      output = clasp_make_long_float(d);
    } else {
      double dx = x->as_double();
      double dy = y->as_double();
      double dz = clasp_atan2_double(dy, dx);
      if (tx == number_DoubleFloat) {
        output = clasp_make_double_float(dz);
      } else {
        output = clasp_make_single_float(dz);
      }
    }
#else
    double dy = clasp_to_double(y);
    double dx = clasp_to_double(x);
    double dz = clasp_atan2_double(dy, dx);
    if (clasp_t_of(x) == number_DoubleFloat || clasp_t_of(y) == number_DoubleFloat) {
      output = clasp_make_double_float(dz);
    } else {
      output = clasp_make_single_float(dz);
    }
#endif
  }
  return output;
}

Number_sp clasp_atan1(Number_sp y) {
  if (clasp_t_of(y) == number_Complex) {
#if 0 /* ANSI states it should be this first part */
    Number_sp z = clasp_times(cl_core.imag_unit, y);
    z = clasp_plus(clasp_log1(clasp_one_plus(z)),
                   clasp_log1(clasp_minus(clasp_make_fixnum(1), z)));
    z = clasp_divide(z, clasp_times(clasp_make_fixnum(2),
                                    cl_core.imag_unit));
#else
    Number_sp z1, z = clasp_times(_lisp->imaginaryUnit(), y);
    z = clasp_one_plus(z);
    z1 = clasp_times(y, y);
    z1 = clasp_one_plus(z1);
    z1 = clasp_sqrt(z1);
    z = clasp_divide(z, z1);
    z = clasp_log1(z);
    z = clasp_times(_lisp->imaginaryUnitNegative(), z);
#endif /* ANSI */
    return z;
  } else {
    return clasp_atan2(y, clasp_make_fixnum(1));
  }
}

CL_LAMBDA(x &optional y);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(atan)dx");
DOCGROUP(clasp);
CL_DEFUN Number_sp cl__atan(Number_sp x, T_sp y) {
  /* INV: type check in clasp_atan() & clasp_atan2() */
  /* FIXME clasp_atan() and clasp_atan2() produce generic errors
           without recovery and function information. */
  if (y.nilp())
    return clasp_atan1(x);
  return clasp_atan2(x, gc::As<Number_sp>(y));
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
  Real_sp a = gc::As<Real_sp>(clasp_abs(r));
  Real_sp p = gc::As<Real_sp>(clasp_abs(i));
  int rel = clasp_number_compare(a, p);
  if (rel > 0) {
    Real_sp aux = p;
    p = a;
    a = aux;
  } else if (rel == 0) {
    /* if a == p,
     * log(sqrt(a^2+p^2)) = log(2a^2)/2
     */
    a = gc::As<Real_sp>(clasp_times(a, a));
    a = gc::As<Real_sp>(clasp_divide(clasp_log1(clasp_plus(a, a)), make_fixnum(2)));
    goto OUTPUT;
  }
  /* For the real part of the output we use the formula
   *	log(sqrt(p^2 + a^2)) = log(sqrt(p^2*(1 + (a/p)^2)))
   *			     = log(p) + log(1 + (a/p)^2)/2; */
  a = gc::As<Real_sp>(clasp_divide(a, p));
  a = gc::As<Real_sp>(clasp_plus(clasp_divide(clasp_log1p(clasp_times(a, a)), make_fixnum(2)), clasp_log1(p)));
OUTPUT:
  p = gc::As<Real_sp>(clasp_atan2(i, r));
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
  float f = this->as_float_();
  if (f < 0)
    return clasp_log1_complex_inner(this->asSmartPtr(), clasp_make_fixnum(0));
  return clasp_make_single_float(logf(this->as_float_()));
}

Number_sp DoubleFloat_O::log1_() const {
  double f = this->as_double_();
  if (std::isnan(f))
    return this->asSmartPtr();
  if (f < 0)
    return clasp_log1_complex_inner(this->asSmartPtr(), clasp_make_fixnum(0));
  return clasp_make_double_float(log(f));
}

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::log1_() const {
  LongFloat f = this->as_long_float();
  if (std::isnan(f))
    return this->asSmartPtr();
  if (f < 0)
    return clasp_log1_complex_inner(this->asSmartPtr(), clasp_make_fixnum(0));
  return clasp_make_long_float(logl(f));
}
#endif

Number_sp Complex_O::log1_() const { return clasp_log1_complex_inner(this->real(), this->imaginary()); }

Number_sp Number_O::log1p_() const { return clasp_log1_complex_inner(clasp_one_plus(this->asSmartPtr()), clasp_make_fixnum(0)); }

Number_sp Rational_O::log1p_() const {
  float f = this->as_float_();
  if (f < -1)
    return this->Base::log1p_();
  return clasp_make_single_float(_log1p(f));
}

// translated from ECL cl_rational
Rational_sp DoubleFloat_O::rational(double d) {
  if (d == 0) {
    return clasp_make_fixnum(0);
  }
  int e;
  d = frexp(d, &e);
  e -= DBL_MANT_DIG;
  Integer_sp x = _clasp_double_to_integer(ldexp(d, DBL_MANT_DIG));
#if 0 //(FLT_RADIX == 2) // runtime is sane (or at least IEEE 754)
  if (e > 0)
    return clasp_shift_left(x, e);
  else if (e < 0)
    // Efficiency note: This could be done faster by exploiting the fact
    // that the denominator is a power of two. Rather than take the full
    // gcd, you can just shift out any shared less significant zero bits.
    return Rational_O::create(x, clasp_shift_left(clasp_make_fixnum(1), -e));
  else return x;
#else
  Number_sp radixexp = clasp_expt(clasp_make_fixnum(FLT_RADIX), clasp_make_fixnum(e));
  return gc::As_unsafe<Integer_sp>(clasp_times(x, radixexp));
#endif
}

Number_sp DoubleFloat_O::log1p_() const {
  double f = this->as_double_();
  if (std::isnan(f))
    return this->asSmartPtr();
  if (f < -1)
    return clasp_log1_complex_inner(clasp_one_plus(this->asSmartPtr()), clasp_make_fixnum(0));
  return clasp_make_double_float(_log1p(f));
}

#ifdef CLASP_LONG_FLOAT
Number_sp LongFloat_O::log1p() const {
  LongFloat f = this->as_long_float();
  if (std::isnan(f))
    return this->asSmartPtr();
  if (f < -1)
    return clasp_log1_complex_inner(clasp_one_plus(this->asSmartPtr()), clasp_make_fixnum(0));
  return clasp_make_long_float(_log1p(f));
}
#endif

Number_sp clasp_log2(Number_sp x, Number_sp y) { return clasp_divide(clasp_log1(y), clasp_log1(x)); }

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
CL_DEFUN bool ext__float_nan_p(Float_sp i) {
  return clasp_float_nan_p(i);
};

CL_LAMBDA(i);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(float-infinity-p)dx");
DOCGROUP(clasp);
CL_DEFUN bool ext__float_infinity_p(Float_sp i) { return clasp_float_infinity_p(i); };

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
  return x->as_float_();
}

double clasp_to_double(core::Number_sp x) {
  if (x.fixnump()) {
    double d = x.unsafe_fixnum();
    return d;
  } else if (x.single_floatp()) {
    double d = x.unsafe_single_float();
    return d;
  }
  return x->as_double_();
};

double clasp_to_double(core::Integer_sp x) {
  if (x.fixnump()) {
    double d = x.unsafe_fixnum();
    return d;
  }
  return x->as_double_();
};

double clasp_to_double(core::T_sp x) {
  if (x.fixnump()) {
    double d = x.unsafe_fixnum();
    return d;
  } else if (x.single_floatp()) {
    double d = x.unsafe_single_float();
    return d;
  } else if (gc::IsA<Number_sp>(x)) {
    return gc::As_unsafe<Number_sp>(x)->as_double_();
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
    return gc::As_unsafe<Number_sp>(x)->as_double_();
  }
  TYPE_ERROR(x, Cons_O::createList(cl::_sym_Real_O));
}

double clasp_to_double(core::General_sp x) {
  if (gc::IsA<Number_sp>(x)) {
    return gc::As_unsafe<Number_sp>(x)->as_double_();
  }
  TYPE_ERROR(x, cl::_sym_Number_O);
};

double clasp_to_double(core::DoubleFloat_sp x) { return x->get(); };

LongFloat clasp_to_long_float(Number_sp x) { return x->as_long_float_(); };

LongFloat clasp_to_long_double(Number_sp x) { return x->as_long_float_(); };

// --- END OF TRANSLATORS ---

CL_LAMBDA(singleFloat);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(Return the IEEE754 binary32 (single) representation of a single float, as an integer.)dx");
DOCGROUP(clasp);
CL_DEFUN Integer_sp ext__single_float_to_bits(SingleFloat_sp singleFloat) {
  // NOTE: This and the later ones are probably undefined behavior,
  // though Clang seems to support them fine.
  // I don't know a conforming way to do this other than converting to
  // bytes, but that's a pretty annoying way to go about this.
  union {
    float f;
    uint32_t i;
  } converter;
  converter.f = unbox_single_float(singleFloat);
  return Integer_O::create(converter.i);
}

CL_LAMBDA(bit-representation);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(Convert an IEEE754 binary32 (single) representation, an integer, to a single float.)dx");
DOCGROUP(clasp);
CL_DEFUN SingleFloat_sp ext__bits_to_single_float(Fixnum_sp fixnum) {
  union {
    float f;
    uint32_t i;
  } converter;
  converter.i = unbox_fixnum(fixnum);
  return make_single_float(converter.f);
};

CL_LAMBDA(doubleFloat);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(Return the IEEE754 binary64 (double) bit representation of a double float as an integer.)dx");
DOCGROUP(clasp);
CL_DEFUN Integer_sp ext__double_float_to_bits(DoubleFloat_sp doubleFloat) {
  union {
    double d;
    uint64_t i;
  } converter;
  converter.d = doubleFloat->get();
  return Integer_O::create(converter.i);
}

CL_LAMBDA(bit-representation);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(Convert an IEEE754 binary64 (double) representation, an integer, to a double float.)dx");
DOCGROUP(clasp);
CL_DEFUN DoubleFloat_sp ext__bits_to_double_float(Integer_sp integer) {
  union {
    double d;
    uint64_t i;
  } converter;
  converter.i = clasp_to_uint64_t(integer);
  return clasp_make_double_float(converter.d);
}

}; // namespace core

namespace core {

CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN Number_sp cl__rational(Real_sp num) {
  if (num.fixnump())
    return num;
  if (num.single_floatp())
    return DoubleFloat_O::rational(num.unsafe_single_float());
  if (gc::IsA<Number_sp>(num))
    return gc::As_unsafe<Number_sp>(num)->rational_();
  TYPE_ERROR(num, cl::_sym_Real_O);
};

CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN Number_sp cl__rationalize(Real_sp num) { return cl__rational(num); };

Integer_sp clasp_make_integer(size_t s) { return Integer_O::create((uint64_t)s); }

}; // namespace core
