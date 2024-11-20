#pragma once
/*
  File: numbers.h
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

#include <clasp/core/clasp_gmpxx.h>
#include <cmath>
#include <math.h>
#include <limits.h>
#include <cfenv>
#include <complex>
#include <numbers>
#pragma GCC diagnostic push
// #pragma GCC diagnostic ignored "-Wunused-local-typedef"
#pragma GCC diagnostic pop

#include <clasp/core/object.h>
#include <clasp/core/numerics.h>
#include <clasp/core/bignum.fwd.h>
#include <clasp/core/numbers.fwd.h>

// Class Hierarchy
// Number_0 [n]
//   Real_O
//     Rational_O
//       Integer_O
//         Fixnum_dummy_O (immediate) [x]
//         Bignum_O [b]
//       Ratio_O
//     Float_O
//       ShortFloat_O [s]
//       SingleFloat_dummy_O (immediate) [f]
//       DoubleFloat_O [d]
//       LongFloat_O [l]
//   Complex_O

#ifdef _TARGET_OS_DARWIN

#if defined(__aarch64__)
#define __FE_EXCEPT_SHIFT 8
#elif defined(__amd64__)
#define __FE_EXCEPT_SHIFT 7
#else
#error "Don't know how to provide FPE for this platform."
#endif

#define __FE_ALL_EXCEPT FE_ALL_EXCEPT

inline int feenableexcept(int excepts) {
  static fenv_t fenv;
  unsigned int new_excepts = excepts & __FE_ALL_EXCEPT;
  unsigned int old_excepts = 0;

  if (fegetenv(&fenv)) {
    return -1;
  }

#ifdef __aarch64__
  old_excepts = (fenv.__fpcr >> __FE_EXCEPT_SHIFT) & __FE_ALL_EXCEPT;
  fenv.__fpcr |= new_excepts << __FE_EXCEPT_SHIFT;
#else
  old_excepts = ~fenv.__control & __FE_ALL_EXCEPT;
  fenv.__control &= ~new_excepts;
  fenv.__mxcsr &= ~(new_excepts << __FE_EXCEPT_SHIFT);
#endif

  return fesetenv(&fenv) ? -1 : old_excepts;
}

inline int fedisableexcept(int excepts) {
  static fenv_t fenv;
  unsigned int new_excepts = excepts & __FE_ALL_EXCEPT;
  unsigned int old_excepts = 0;

  if (fegetenv(&fenv)) {
    return -1;
  }

#ifdef __aarch64__
  old_excepts = (fenv.__fpcr >> __FE_EXCEPT_SHIFT) & __FE_ALL_EXCEPT;
  fenv.__fpcr &= ~(new_excepts << __FE_EXCEPT_SHIFT);
#else
  old_excepts = ~fenv.__control & __FE_ALL_EXCEPT;
  fenv.__control |= new_excepts;
  fenv.__mxcsr |= new_excepts << __FE_EXCEPT_SHIFT;
#endif

  return fesetenv(&fenv) ? -1 : old_excepts;
}

inline int fegetexcept() {
  static fenv_t fenv;
  if (fegetenv(&fenv))
    return -1;
#ifdef __aarch64__
  return (fenv.__fpcr >> __FE_EXCEPT_SHIFT) & __FE_ALL_EXCEPT;
#else
  return ~fenv.__control & __FE_ALL_EXCEPT;
#endif
}

#endif

namespace cl {
extern core::Symbol_sp& _sym_Integer_O; // CL:INTEGER
extern core::Symbol_sp& _sym_Real_O;    // CL:INTEGER
}; // namespace cl

namespace core {
// TYPE ERRORS

core::Fixnum not_fixnum_error(core::T_sp o);

// TYPE TEMPLATES

template <typename T> gc::smart_ptr<T> immediate_fixnum(Fixnum f) { return gc::make_tagged_fixnum<core::Fixnum_I>(f); };
template <typename T> gc::smart_ptr<T> immediate_single_float(float f) {
  return gc::make_tagged_single_float<core::SingleFloat_I>(f);
};

template <typename Float> inline Float _log1p(Float x) {
  Float u = Float{1} + x;
  if (u == Float{1}) {
    return Float{0};
  }
  return (std::log(u) * x) / (u - Float{1});
}

Number_sp clasp_one_plus(Number_sp num);
Number_sp clasp_one_minus(Number_sp num);
Number_sp clasp_negate(Number_sp num);
Integer_sp clasp_shift_left(Integer_sp num, Fixnum nbits);
Integer_sp clasp_shift_right(Integer_sp num, Fixnum nbits);
gc::Fixnum clasp_integer_length(Integer_sp x);

Fixnum_sp clasp_make_fixnum(gc::Fixnum i);
SingleFloat_sp clasp_make_single_float(float d);
DoubleFloat_sp clasp_make_double_float(double d);
Number_sp clasp_log1_complex_inner(Number_sp r, Number_sp i);
void clasp_report_divide_by_zero(Number_sp x);
}; // namespace core

namespace core {

SMART(Number);
class Number_O : public General_O {
  LISP_ABSTRACT_CLASS(core, ClPkg, Number_O, "number", General_O);

public:
  template <std::floating_point Float> inline static Float_sp make_float(Float x);
  template <std::floating_point Float> inline static Complex_sp make_complex(const std::complex<Float> x);

  virtual Number_sp signum_() const { SUBCLASS_MUST_IMPLEMENT(); };
  virtual Number_sp reciprocal_() const { SUBCLASS_MUST_IMPLEMENT(); };
  virtual Number_sp abs_() const { SUBCLASS_MUST_IMPLEMENT(); };
  virtual bool equal(T_sp obj) const override;
  virtual bool equalp(T_sp obj) const override;

  virtual Real_sp realpart_() const { SUBIMP(); };
  virtual Real_sp imagpart_() const { SUBIMP(); };

  // log(x) (i.e. natural log)
  virtual Number_sp log1_() const { SUBIMP(); };
  // log(x+1)
  virtual Number_sp log1p_() const;

  virtual Number_sp sqrt_() const { SUBIMP(); };
  virtual Rational_sp as_rational_() const = 0;
  /*! Add one to the number */
  virtual Number_sp onePlus_() const { SUBIMP(); };
  /*! Subtrace one from the number */
  virtual Number_sp oneMinus_() const { SUBIMP(); };

  // math routines shared by all numbers
  virtual bool zerop_() const { SUBIMP(); }
  virtual Number_sp negate_() const { SUBIMP(); };

  virtual Number_sp exp_() const { SUBIMP(); };

  virtual uint as_uint_() const { SUBIMP(); }
  virtual LongLongInt as_LongLongInt_() const { SUBIMP(); };
  virtual short_float_t as_short_float_() const { SUBIMP(); };
  virtual single_float_t as_single_float_() const { SUBIMP(); };
  virtual double_float_t as_double_float_() const { SUBIMP(); }
  virtual long_float_t as_long_float_() const { SUBIMP(); };

  virtual Number_sp sin_() const { SUBIMP(); };
  virtual Number_sp asin_() const { SUBIMP(); };
  virtual Number_sp cos_() const { SUBIMP(); };
  virtual Number_sp acos_() const { SUBIMP(); };
  virtual Number_sp tan_() const { SUBIMP(); };
  virtual Number_sp atan_() const { SUBIMP(); };
  virtual Number_sp sinh_() const { SUBIMP(); };
  virtual Number_sp cosh_() const { SUBIMP(); };
  virtual Number_sp tanh_() const { SUBIMP(); };

  virtual void sxhash_(HashGenerator& hg) const override { SUBIMP(); };
  Number_O() {};

  inline static bool zerop(Number_sp x) {
    if (x.fixnump())
      return x.unsafe_fixnum() == 0;
#ifdef CLASP_SHORT_FLOAT
    if (x.short_floatp())
      return std::fpclassify(x.unsafe_short_float()) == FP_ZERO;
#endif
    if (x.single_floatp())
      return std::fpclassify(x.unsafe_single_float()) == FP_ZERO;
    return x->zerop_();
  }

  inline static short_float_t as_short_float(const Number_sp x) {
    if (x.fixnump())
      return (short_float_t)x.unsafe_fixnum();
#ifdef CLASP_SHORT_FLOAT
    if (x.short_floatp())
      return (short_float_t)x.unsafe_short_float();
#endif
    if (x.single_floatp())
      return (short_float_t)x.unsafe_single_float();
    return x->as_short_float_();
  }

  inline static single_float_t as_single_float(const Number_sp x) {
    if (x.fixnump())
      return (single_float_t)x.unsafe_fixnum();
#ifdef CLASP_SHORT_FLOAT
    if (x.short_floatp())
      return (single_float_t)x.unsafe_short_float();
#endif
    if (x.single_floatp())
      return (single_float_t)x.unsafe_single_float();
    return x->as_single_float_();
  }

  inline static double_float_t as_double_float(const Number_sp x) {
    if (x.fixnump())
      return (double_float_t)x.unsafe_fixnum();
#ifdef CLASP_SHORT_FLOAT
    if (x.short_floatp())
      return (double_float_t)x.unsafe_short_float();
#endif
    if (x.single_floatp())
      return (double_float_t)x.unsafe_single_float();
    return x->as_double_float_();
  }

  inline static long_float_t as_long_float(const Number_sp x) {
    if (x.fixnump())
      return (long_float_t)x.unsafe_fixnum();
#ifdef CLASP_SHORT_FLOAT
    if (x.short_floatp())
      return (long_float_t)x.unsafe_short_float();
#endif
    if (x.single_floatp())
      return (long_float_t)x.unsafe_single_float();
    return x->as_long_float_();
  }

  inline static Real_sp realpart(const Number_sp x);
  inline static Real_sp imagpart(const Number_sp x);

  inline static Number_sp negate(const Number_sp x);
  inline static Number_sp signum(const Number_sp num);

  inline static Number_sp abs(const Number_sp x);
  inline static Number_sp sqrt(const Number_sp x);

  inline static Number_sp sin(const Number_sp x);
  inline static Number_sp asin(const Number_sp x);
  inline static Number_sp cos(const Number_sp x);
  inline static Number_sp acos(const Number_sp x);
  inline static Number_sp tan(const Number_sp x);
  static Number_sp atan2(Real_sp y, Real_sp x);
  inline static Number_sp atan(Number_sp y);
  inline static Number_sp sinh(const Number_sp x);
  inline static Number_sp cosh(const Number_sp x);
  inline static Number_sp tanh(const Number_sp x);

  static Number_sp add_nn(Number_sp x, Number_sp y);
  static Integer_sp add_bb(Bignum_sp x, Bignum_sp y);
  static Integer_sp add_bx(Bignum_sp x, Fixnum y);

  static Number_sp sub_nn(Number_sp x, Number_sp y);
  static Integer_sp sub_bb(Bignum_sp x, Bignum_sp y);
  static Integer_sp sub_xb(Fixnum x, Bignum_sp y);

  static Number_sp mul_nn(Number_sp x, Number_sp y);
  static Integer_sp mul_bx(Bignum_sp x, Fixnum y);
  static Bignum_sp mul_bb(Bignum_sp x, Bignum_sp y);

  static Number_sp div_nn(Number_sp x, Number_sp y);

  static int compare(const Real_sp na, const Real_sp nb);
};

inline Number_sp operator-(const Number_sp x) { return Number_O::negate(x); }
inline Number_sp operator+(const Number_sp x, const Number_sp y) { return Number_O::add_nn(x, y); }
inline Number_sp operator-(const Number_sp x, const Number_sp y) { return Number_O::sub_nn(x, y); }
inline Number_sp operator*(const Number_sp x, const Number_sp y) { return Number_O::mul_nn(x, y); }
inline Number_sp operator/(const Number_sp x, const Number_sp y) { return Number_O::div_nn(x, y); }

inline Number_sp operator+=(Number_sp& x, const Number_sp y) { return x = Number_O::add_nn(x, y); };
inline Number_sp operator-=(Number_sp& x, const Number_sp y) { return x = Number_O::sub_nn(x, y); };
inline Number_sp operator*=(Number_sp& x, const Number_sp y) { return x = Number_O::mul_nn(x, y); };
inline Number_sp operator/=(Number_sp& x, const Number_sp y) { return x = Number_O::div_nn(x, y); };

SMART(Real);
class Real_O : public Number_O {
  LISP_ABSTRACT_CLASS(core, ClPkg, Real_O, "real", Number_O);

public:
  Real_sp realpart_() const override { return asSmartPtr(); }
  Real_sp imagpart_() const override { return clasp_make_fixnum(0); }

  virtual double_float_t as_double_float_() const override { SUBIMP(); };

  // functions shared by all Real
  virtual bool plusp_() const { SUBIMP(); };
  virtual bool minusp_() const { SUBIMP(); };

  Real_O() {};

  inline static bool plusp(Real_sp x) {
    if (x.fixnump())
      return x.unsafe_fixnum() > 0;
#ifdef CLASP_SHORT_FLOAT
    if (x.short_floatp())
      return x.unsafe_short_float() > short_float_t{0.0};
#endif
    if (x.single_floatp())
      return x.unsafe_single_float() > single_float_t{0.0};
    return x->plusp_();
  }

  inline static bool minusp(Real_sp x) {
    if (x.fixnump())
      return x.unsafe_fixnum() < 0;
#ifdef CLASP_SHORT_FLOAT
    if (x.short_floatp())
      return x.unsafe_short_float() < short_float_t{0.0};
#endif
    if (x.single_floatp())
      return x.unsafe_single_float() < single_float_t{0.0};
    return x->minusp_();
  }
};

inline Real_sp operator-(const Real_sp x) { return gc::As_unsafe<Real_sp>(Number_O::negate(x)); }
inline Real_sp operator+(const Real_sp x, const Real_sp y) { return gc::As_unsafe<Real_sp>(Number_O::add_nn(x, y)); }
inline Real_sp operator-(const Real_sp x, const Real_sp y) { return gc::As_unsafe<Real_sp>(Number_O::sub_nn(x, y)); }
inline Real_sp operator*(const Real_sp x, const Real_sp y) { return gc::As_unsafe<Real_sp>(Number_O::mul_nn(x, y)); }
inline Real_sp operator/(const Real_sp x, const Real_sp y) { return gc::As_unsafe<Real_sp>(Number_O::div_nn(x, y)); }

inline Real_sp operator+=(Real_sp& x, const Real_sp y) { return x = gc::As_unsafe<Real_sp>(Number_O::add_nn(x, y)); };
inline Real_sp operator-=(Real_sp& x, const Real_sp y) { return x = gc::As_unsafe<Real_sp>(Number_O::sub_nn(x, y)); };
inline Real_sp operator*=(Real_sp& x, const Real_sp y) { return x = gc::As_unsafe<Real_sp>(Number_O::mul_nn(x, y)); };

SMART(Rational);
class Rational_O : public Real_O {
  LISP_ABSTRACT_CLASS(core, ClPkg, Rational_O, "rational", Real_O);

public:
  static Rational_sp create(mpz_class const& num, mpz_class const& denom);
  static Rational_sp create(Integer_sp num, Integer_sp denom);

public:
  virtual Number_sp log1_() const override;
  virtual Number_sp log1p_() const override;
  //	virtual Number_sp sqrt_() const;
  virtual Number_sp exp_() const override;

  virtual Number_sp sin_() const override;
  virtual Number_sp asin_() const override;
  virtual Number_sp cos_() const override;
  virtual Number_sp acos_() const override;
  virtual Number_sp tan_() const override;
  virtual Number_sp atan_() const override;
  virtual Number_sp sinh_() const override;
  virtual Number_sp cosh_() const override;
  virtual Number_sp tanh_() const override;

  Rational_O() {};

  inline static Rational_sp coerce(const Real_sp x);
};

SMART(Integer);
class Integer_O : public Rational_O {
  LISP_ABSTRACT_CLASS(core, ClPkg, Integer_O, "integer", Rational_O);

public:
  /*! Return a Cons (integer low high) */
  static T_sp makeIntegerType(gc::Fixnum low, gc::Fixnum high);

  static Integer_sp create(std::signed_integral auto v);
  static Integer_sp create(std::unsigned_integral auto v);

  template <std::floating_point Float> static Integer_sp create(Float v);

  static Integer_sp create(int8_t v);
  static Integer_sp create(uint8_t v);
  static Integer_sp create(int16_t v);
  static Integer_sp create(uint16_t v);
#ifdef CLASP_FIXNUM_IS_INT64
  static Integer_sp create(int32_t v);
  static Integer_sp create(uint32_t v);
#endif
#if !defined(_TARGET_OS_LINUX) && !defined(_TARGET_OS_FREEBSD)
  static Integer_sp create(long v) { return Integer_O::create(static_cast<Fixnum>(v)); };
  static Integer_sp create(unsigned long v);
#endif

  static Integer_sp create(const mpz_class& v);
  static Integer_sp create(const string& v, int base = 0) { return create(v.c_str(), base); };
  static Integer_sp create(const char* v, int base = 0) {
    if (v[0] == '+')
      v = &v[1];
    return create(mpz_class{v, base});
  };

public:
  virtual mpz_class mpz() const { SUBIMP(); };
  virtual bool evenp_() const { SUBIMP(); };
  virtual bool oddp_() const { SUBIMP(); };

  virtual gc::Fixnum bit_length_() const { SUBIMP(); };
  virtual gc::Fixnum popcount() const { SUBIMP(); };

  // Divide this integer by another,
  // returning an exact, reduced rational.
  virtual Rational_sp ratdivide(Integer_sp) const { SUBIMP(); };

  // Return the value shifted by BITS bits left.
  virtual Integer_sp shift_left(gc::Fixnum nbits) const { SUBIMP(); };
  // and right.
  virtual Integer_sp shift_right(gc::Fixnum nbits) const { SUBIMP(); };

  virtual void __write__(T_sp strm) const override;
  Integer_O() {};

  inline static bool evenp(const Integer_sp x) {
    if (x.fixnump())
      return (x.unsafe_fixnum() % 2) == 0;
    return x->evenp_();
  }

  inline static bool oddp(const Integer_sp x) {
    if (x.fixnump())
      return (x.unsafe_fixnum() % 2) != 0;
    return x->oddp_();
  }
};

inline Integer_sp operator-(const Integer_sp x) { return gc::As_unsafe<Integer_sp>(Number_O::negate(x)); }
inline Integer_sp operator+(const Integer_sp x, const Integer_sp y) { return gc::As_unsafe<Integer_sp>(Number_O::add_nn(x, y)); }
inline Integer_sp operator-(const Integer_sp x, const Integer_sp y) { return gc::As_unsafe<Integer_sp>(Number_O::sub_nn(x, y)); }
inline Integer_sp operator*(const Integer_sp x, const Integer_sp y) { return gc::As_unsafe<Integer_sp>(Number_O::mul_nn(x, y)); }
inline Rational_sp operator/(const Integer_sp x, const Integer_sp y) { return gc::As_unsafe<Rational_sp>(Number_O::div_nn(x, y)); }

inline Integer_sp operator+=(Integer_sp& x, const Integer_sp y) { return x = gc::As_unsafe<Integer_sp>(Number_O::add_nn(x, y)); };
inline Integer_sp operator-=(Integer_sp& x, const Integer_sp y) { return x = gc::As_unsafe<Integer_sp>(Number_O::sub_nn(x, y)); };
inline Integer_sp operator*=(Integer_sp& x, const Integer_sp y) { return x = gc::As_unsafe<Integer_sp>(Number_O::mul_nn(x, y)); };

}; // namespace core

namespace core {

Fixnum_sp make_fixnum(gc::Fixnum x);

class Fixnum_dummy_O : public Integer_O {
  LISP_ABSTRACT_CLASS(core, ClPkg, Fixnum_dummy_O, "fixnum", Integer_O);
};
inline Fixnum_sp make_fixnum(gc::Fixnum x) { return gc::make_tagged_fixnum<core::Fixnum_I>(x); };
inline gc::Fixnum unbox_fixnum(Fixnum_sp x) { return x.unsafe_fixnum(); };
}; // namespace core

namespace core {

SMART(Float);
class Float_O : public Real_O {
  LISP_ABSTRACT_CLASS(core, ClPkg, Float_O, "float", Real_O);

public:
  virtual Integer_sp castToInteger() const { SUBIMP(); };

  virtual bool isnan_() const { SUBIMP(); };
  virtual bool isinf_() const { SUBIMP(); };
  virtual int fpclassify_() const { SUBIMP(); };

  Float_O() {};

  inline static bool isnan(Float_sp x) {
#ifdef CLASP_SHORT_FLOAT
    if (x.short_floatp())
      return std::isnan(x.unsafe_short_float());
#endif
    if (x.single_floatp())
      return std::isnan(x.unsafe_single_float());
    return x->isnan_();
  }

  inline static bool isinf(Float_sp x) {
#ifdef CLASP_SHORT_FLOAT
    if (x.short_floatp())
      return std::isinf(x.unsafe_short_float());
#endif
    if (x.single_floatp())
      return std::isinf(x.unsafe_single_float());
    return x->isinf_();
  }

  inline static int fpclassify(Float_sp x) {
#ifdef CLASP_SHORT_FLOAT
    if (x.short_floatp())
      return std::fpclassify(x.unsafe_short_float());
#endif
    if (x.single_floatp())
      return std::fpclassify(x.unsafe_single_float());
    return x->fpclassify_();
  }
};

class SingleFloat_dummy_O : public Float_O {
  LISP_ABSTRACT_CLASS(core, ClPkg, SingleFloat_dummy_O, "SingleFloat", Float_O);

  inline static SingleFloat_sp create(float x) { return gc::make_tagged_single_float<core::SingleFloat_I>(x); };

  static SingleFloat_sp coerce(Number_sp x);
};

#ifdef CLASP_SHORT_FLOAT
template <> inline Float_sp Number_O::make_float(short_float_t x) { return ShortFloat_O::create(x); }
#endif

class ShortFloat_O : public Float_O {
  LISP_ABSTRACT_CLASS(core, ClPkg, ShortFloat_O, "ShortFloat", Float_O);

#ifdef CLASP_SHORT_FLOAT
  inline static ShortFloat_sp create(float x) { return gc::make_tagged_short_float<core::SingleFloat_I>(x); };

  static ShortFloat_sp coerce(Number_sp x);
#else
  inline static SingleFloat_sp create(float x) { return gc::make_tagged_single_float<core::SingleFloat_I>(x); };

  inline static SingleFloat_sp coerce(Number_sp x) { return SingleFloat_dummy_O::coerce(x); }
#endif
};

inline SingleFloat_sp make_single_float(float x) { return gc::make_tagged_single_float<core::SingleFloat_I>(x); };
inline float unbox_single_float(SingleFloat_sp x) { return x.unsafe_single_float(); };

template <> inline Float_sp Number_O::make_float(single_float_t x) { return SingleFloat_dummy_O::create(x); }

}; // namespace core

template <> struct gctools::GCInfo<core::DoubleFloat_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = atomic;
};

namespace core {
SMART(DoubleFloat);
class DoubleFloat_O : public Float_O {
  LISP_CLASS(core, ClPkg, DoubleFloat_O, "double-float", Float_O);

private:
  double _Value;

public:
  static DoubleFloat_sp create(double nm) {
    auto v = gctools::GC<DoubleFloat_O>::allocate_with_default_constructor();
    v->set(nm);
    return v;
  };

  static DoubleFloat_sp coerce(Number_sp x);

  void sxhash_(HashGenerator& hg) const override;
  //	virtual Number_sp copy() const;
  string __repr__() const override;
  void set(double val) { this->_Value = val; };
  double get() const { return this->_Value; };

  Real_sp imagpart_() const override;

  Number_sp signum_() const override;
  Number_sp abs_() const override { return DoubleFloat_O::create(std::abs(this->_Value)); };
  bool isnan_() const override { return std::isnan(this->_Value); }; // NaN is supposed to be the only value that != itself!!!!
  bool isinf_() const override { return std::isinf(this->_Value); };
  int fpclassify_() const override { return std::fpclassify(_Value); }

public:
  virtual bool eql_(T_sp obj) const override;

  // math routines shared by all numbers
  bool zerop_() const override { return std::fpclassify(this->_Value) == FP_ZERO; };
  virtual Number_sp negate_() const override { return DoubleFloat_O::create(-this->_Value); };

  // Shared by real
  bool plusp_() const override { return this->_Value > 0.0; };
  bool minusp_() const override { return this->_Value < 0.0; };

  virtual Number_sp reciprocal_() const override;
  virtual Number_sp sqrt_() const override;

  virtual Number_sp onePlus_() const override { return create(this->_Value + 1.0); };
  virtual Number_sp oneMinus_() const override { return create(this->_Value - 1.0); };

  virtual Number_sp log1_() const override;
  virtual Number_sp log1p_() const override;

  virtual short_float_t as_short_float_() const override;
  virtual single_float_t as_single_float_() const override;
  virtual double_float_t as_double_float_() const override;
  virtual long_float_t as_long_float_() const override;

  Integer_sp castToInteger() const override;

  virtual Number_sp exp_() const override;

  virtual Number_sp sin_() const override;
  virtual Number_sp asin_() const override;
  virtual Number_sp cos_() const override;
  virtual Number_sp acos_() const override;
  virtual Number_sp tan_() const override;
  virtual Number_sp atan_() const override;
  virtual Number_sp sinh_() const override;
  virtual Number_sp cosh_() const override;
  virtual Number_sp tanh_() const override;
  virtual Rational_sp as_rational_() const override;
  DoubleFloat_O() : _Value(0.0) {};
};

template <> inline Float_sp Number_O::make_float(double_float_t x) { return DoubleFloat_O::create(x); }

}; // namespace core

#ifdef CLASP_LONG_FLOAT
template <> struct gctools::GCInfo<core::LongFloat_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = atomic;
};
#endif

namespace core {
SMART(LongFloat);
class LongFloat_O : public Float_O {
#ifdef CLASP_LONG_FLOAT
  LISP_CLASS(core, ClPkg, LongFloat_O, "long-float", Float_O);
#else
  LISP_ABSTRACT_CLASS(core, ClPkg, LongFloat_O, "long-float", Float_O);
#endif

private:
  long_float_t _Value;

public:
#ifdef CLASP_LONG_FLOAT
  static LongFloat_sp create(long_float_t nm) {
    auto v = gctools::GC<LongFloat_O>::allocate_with_default_constructor();
    v->set(nm);
    return v;
  };

  static LongFloat_sp coerce(Number_sp x);

  void sxhash_(HashGenerator& hg) const override;
  string __repr__() const override;
  void set(long_float_t val) { this->_Value = val; };
  long_float_t get() const { return this->_Value; };

  Real_sp imagpart_() const override;

  Number_sp signum_() const override;
  Number_sp abs_() const override { return LongFloat_O::create(std::abs(this->_Value)); };
  bool isnan_() const override { return std::isnan(this->_Value); }; // NaN is supposed to be the only value that != itself!!!!
  bool isinf_() const override { return std::isinf(this->_Value); };
  int fpclassify_() const override { return std::fpclassify(_Value); }

  virtual bool eql_(T_sp obj) const override;

  // math routines shared by all numbers
  bool zerop_() const override { return std::fpclassify(this->_Value) == FP_ZERO; };
  virtual Number_sp negate_() const override { return LongFloat_O::create(-this->_Value); };

  // Shared by real
  bool plusp_() const override { return this->_Value > long_float_t{0.0}; };
  bool minusp_() const override { return this->_Value < long_float_t{0.0}; };

  virtual Number_sp reciprocal_() const override;
  virtual Number_sp sqrt_() const override;

  virtual Number_sp onePlus_() const override { return create(this->_Value + long_float_t{1.0}); };
  virtual Number_sp oneMinus_() const override { return create(this->_Value - long_float_t{1.0}); };

  virtual Number_sp log1_() const override;
  virtual Number_sp log1p_() const override;

  virtual short_float_t as_short_float_() const override;
  virtual single_float_t as_single_float_() const override;
  virtual double_float_t as_double_float_() const override;
  virtual long_float_t as_long_float_() const override;

  Integer_sp castToInteger() const override;

  virtual Number_sp exp_() const override;

  virtual Number_sp sin_() const override;
  virtual Number_sp asin_() const override;
  virtual Number_sp cos_() const override;
  virtual Number_sp acos_() const override;
  virtual Number_sp tan_() const override;
  virtual Number_sp atan_() const override;
  virtual Number_sp sinh_() const override;
  virtual Number_sp cosh_() const override;
  virtual Number_sp tanh_() const override;
  virtual Rational_sp as_rational_() const override;
  LongFloat_O() : _Value(long_float_t{0.0}) {};
#else
  inline static DoubleFloat_sp create(long_float_t nm) { return DoubleFloat_O::create(nm); }

  inline static DoubleFloat_sp coerce(Number_sp x) { return DoubleFloat_O::coerce(x); }

  DEFAULT_CTOR_DTOR(LongFloat_O);
#endif
};

#ifdef CLASP_LONG_FLOAT
template <> inline Float_sp Number_O::make_float(long_float_t x) { return LongFloat_O::create(x); }
#endif

SMART(Complex);
class Complex_O : public Number_O {
  LISP_CLASS(core, ClPkg, Complex_O, "complex", Number_O);

public:
  GCPRIVATE : Real_sp _real;
  Real_sp _imaginary;

public:
  static Complex_sp create(double r, double i) {
    DoubleFloat_sp dfr = DoubleFloat_O::create(r);
    DoubleFloat_sp dfi = DoubleFloat_O::create(i);
    auto v = gctools::GC<Complex_O>::allocate(dfr, dfi);
    return v;
  };
  static Complex_sp create(Real_sp r, Real_sp i) {
    auto v = gctools::GC<Complex_O>::allocate(r, i);
    return v;
  }

public:
  Real_sp real() const { return this->_real; };
  Real_sp imaginary() const { return this->_imaginary; };

  void sxhash_(HashGenerator& hg) const override;
  //	virtual Number_sp copy() const;
  string __repr__() const override;

  Real_sp realpart_() const override { return _real; }
  Real_sp imagpart_() const override { return _imaginary; }

  Number_sp signum_() const override;
  Number_sp abs_() const override;
  Rational_sp as_rational_() const override { TYPE_ERROR(this->asSmartPtr(), cl::_sym_Real_O); };

public:
  //	virtual	bool	eqn(T_sp obj) const;
  virtual bool eql_(T_sp obj) const override;

  // math routines shared by all numbers
  bool zerop_() const override { return (zerop(this->_real) && zerop(this->_imaginary)); };
  virtual Number_sp negate_() const override {
    return Complex_O::create(gc::As<Real_sp>(clasp_negate(this->_real)), gc::As<Real_sp>(clasp_negate(this->_imaginary)));
  };

  virtual Number_sp log1_() const override;
  virtual Number_sp log1p_() const override;

  virtual Number_sp onePlus_() const override { return create(gc::As<Real_sp>(clasp_one_plus(this->_real)), this->_imaginary); };
  virtual Number_sp oneMinus_() const override { return create(gc::As<Real_sp>(clasp_one_minus(this->_real)), this->_imaginary); };

  Number_sp sqrt_() const override;

  virtual Number_sp exp_() const override;

  virtual Number_sp sin_() const override;
  virtual Number_sp asin_() const override;
  virtual Number_sp cos_() const override;
  virtual Number_sp acos_() const override;
  virtual Number_sp tan_() const override;
  virtual Number_sp atan_() const override;
  virtual Number_sp sinh_() const override;
  virtual Number_sp cosh_() const override;
  virtual Number_sp tanh_() const override;

  virtual Number_sp reciprocal_() const override;
  Complex_sp conjugate() const;

  virtual void __write__(T_sp strm) const override;

  Complex_O(Real_sp r, Real_sp i) : _real(r), _imaginary(i) {};
  Complex_O() : _real(clasp_make_single_float(0.0)), _imaginary(clasp_make_single_float(0.0)) {};
};

template <std::floating_point Float> inline Complex_sp Number_O::make_complex(const std::complex<Float> x) {
  return Complex_O::create(make_float(x.real()), make_float(x.imag()));
}

SMART(Ratio);
class Ratio_O : public Rational_O {
  LISP_CLASS(core, ClPkg, Ratio_O, "ratio", Rational_O);

public:
  GCPRIVATE : Integer_sp _numerator;
  Integer_sp _denominator;

public:
  static Ratio_sp create(Integer_sp num, Integer_sp denom) {
    auto v = gctools::GC<Ratio_O>::allocate_with_default_constructor();
    v->setf_numerator_denominator(num, denom);
    return v;
  };
  static Ratio_sp create(mpz_class const& num, mpz_class const& denom) {
    return Ratio_O::create(Integer_O::create(num), Integer_O::create(denom));
  }
  // For when it is known that the ratio is reduced already.
  static Ratio_sp create_primitive(Integer_sp num, Integer_sp denom) {
    auto v = gctools::GC<Ratio_O>::allocate_with_default_constructor();
    v->_numerator = num;
    v->_denominator = denom;
    return v;
  }

public:
  // Only useful for creating Ratio in fasl files.
  void setf_numerator_denominator(core::Integer_sp num, core::Integer_sp denom);

public:
  virtual bool zerop_() const override { return zerop(this->_numerator); };
  virtual Number_sp negate_() const override {
    return Ratio_O::create_primitive(gc::As<Integer_sp>(clasp_negate(this->_numerator)), gc::As<Integer_sp>(this->_denominator));
  };
  Integer_sp numerator() const { return this->_numerator; };
  Integer_sp denominator() const { return this->_denominator; };

  void sxhash_(HashGenerator& hg) const override;
  //	virtual Number_sp copy() const;
  string __repr__() const override;
  Number_sp signum_() const override;
  Number_sp abs_() const override;
  Number_sp sqrt_() const override;
  Number_sp reciprocal_() const override;
  Rational_sp as_rational_() const override { return this->asSmartPtr(); };
  bool isnan_() const;

public:
  virtual bool eql_(T_sp obj) const override;

  Number_sp onePlus_() const override { return create(_numerator + _denominator, _denominator); };

  Number_sp oneMinus_() const override {
    return create(gc::As<Integer_sp>(this->_numerator - this->_denominator), this->_denominator);
  };

  virtual short_float_t as_short_float_() const override;
  virtual single_float_t as_single_float_() const override;
  virtual double_float_t as_double_float_() const override;
  virtual long_float_t as_long_float_() const override;

  // functions shared by all Real

  bool plusp_() const override { return plusp(this->_numerator); }

  bool minusp_() const override { return minusp(this->_numerator); }

  virtual void __write__(T_sp strm) const override;

  Ratio_O() : _numerator(clasp_make_fixnum(0)), _denominator(clasp_make_fixnum(1)) {};
};

inline Number_sp float_sqrt(float f) {
  if (f < 0.0) {
    return Complex_O::create(clasp_make_single_float(0.0), clasp_make_single_float(sqrtf(-f)));
  } else {
    return clasp_make_single_float(sqrtf(f));
  }
}

inline Number_sp clasp_log1(Number_sp x) {
  if (x.fixnump()) {
    float f = x.unsafe_fixnum();
    if (f < 0)
      return clasp_log1_complex_inner(x, clasp_make_fixnum(0));
    return clasp_make_single_float(logf(f));
  }
#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp()) {
    short_float_t f = x.unsafe_short_float();
    if (std::isnan(f))
      return x;
    if (f < 0)
      return clasp_log1_complex_inner(x, clasp_make_fixnum(0));
    return ShortFloat_O::create(logf(f));
  }
#endif
  if (x.single_floatp()) {
    float f = x.unsafe_single_float();
    if (std::isnan(f))
      return x;
    if (f < 0)
      return clasp_log1_complex_inner(x, clasp_make_fixnum(0));
    return clasp_make_single_float(logf(f));
  }
  return x->log1_();
}

inline Number_sp clasp_log1p(Number_sp x) {
  if (x.fixnump()) {
    float f = x.unsafe_fixnum();
    if (f < -1)
      return clasp_log1_complex_inner(clasp_one_plus(x), clasp_make_fixnum(0));
    return clasp_make_single_float(_log1p(f));
  }
#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp()) {
    short_float_t f = x.unsafe_short_float();
    if (std::isnan(f))
      return x;
    if (f < -1)
      return clasp_log1_complex_inner(clasp_one_plus(x), clasp_make_fixnum(0));
    return ShortFloat_O::create(_log1p(f));
  }
#endif
  if (x.single_floatp()) {
    float f = x.unsafe_single_float();
    if (std::isnan(f))
      return x;
    if (f < -1)
      return clasp_log1_complex_inner(clasp_one_plus(x), clasp_make_fixnum(0));
    return clasp_make_single_float(_log1p(f));
  }
  return x->log1p_();
};

template <std::floating_point Float> inline Rational_sp float_to_rational(Float f) {
  auto q = float_convert<long_float_t>::float_to_quadruple(f);

  Number_sp n = Integer_O::create(q.significand);

  if (q.exponent < 0) {
    n /= clasp_ash(clasp_make_fixnum(1), -q.exponent);
  } else if (q.exponent > 0) {
    n = clasp_ash(n.as_unsafe<Integer_O>(), q.exponent);
  }

  if (q.sign < 0)
    return clasp_negate(n).as_unsafe<Rational_O>();

  return n.as_unsafe<Rational_O>();
}

inline Rational_sp Rational_O::coerce(const Real_sp x) {
  if (x.fixnump())
    return x.as_unsafe<Rational_O>();
#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp())
    return float_to_rational(x.unsafe_short_float());
#endif
  if (x.single_floatp())
    return float_to_rational(x.unsafe_single_float());
  return gc::As_unsafe<Number_sp>(x)->as_rational_();
}

Number_sp cl__expt(Number_sp x, Number_sp y);
Real_sp cl__mod(Real_sp, Real_sp);

Integer_sp clasp_ash(Integer_sp x, int bits);

inline gctools::Fixnum clasp_safe_fixnum(Number_sp x) { return gc::As<Fixnum_sp>(x).unsafe_fixnum(); }

Number_sp clasp_make_complex(Real_sp r, Real_sp i);

inline Fixnum_sp clasp_make_fixnum(gc::Fixnum i) { return make_fixnum(i); }

inline Integer_sp _clasp_float_to_integer(float d) { return Integer_O::create(d); }

inline Integer_sp _clasp_double_to_integer(double d) { return Integer_O::create(d); }

inline Integer_sp _clasp_long_float_to_integer(long_float_t d) { return Integer_O::create(d); }

inline Integer_sp _clasp_long_double_to_integer(long_float_t d) { return Integer_O::create(d); }

inline SingleFloat_sp clasp_make_single_float(float d) { return gc::make_tagged_single_float<core::SingleFloat_I>(d); }

inline DoubleFloat_sp clasp_make_double_float(double d) { return DoubleFloat_O::create(d); }

#ifdef CLASP_LONG_FLOAT
inline LongFloat_sp clasp_make_long_float(long_float_t d) { return LongFloat_O::create(d); }
#endif

#define CLASP_REAL_TYPE_P(y) (gc::IsA<Real_sp>(y))

#define CLASP_FIXNUMP(n) (gc::IsA<Fixnum_sp>(n))

/*! In num_co.cc */
Real_mv clasp_floor1(Real_sp x);
Real_mv clasp_floor2(Real_sp x, Real_sp y);
Real_mv clasp_ceiling1(Real_sp x);
Real_mv clasp_ceiling2(Real_sp x, Real_sp y);

Real_mv clasp_truncate1(Real_sp x);
Real_mv clasp_truncate2(Real_sp x, Real_sp y);

Real_mv clasp_round1(Real_sp x);
Real_mv clasp_round2(Real_sp x, Real_sp y);

Real_sp clasp_max2(Real_sp x, Real_sp y);
Real_sp clasp_min2(Real_sp x, Real_sp y);

#define clasp_lowereq(x, y) (Number_O::compare((x), (y)) <= 0)
#define clasp_greatereq(x, y) (Number_O::compare((x), (y)) >= 0)
#define clasp_lower(x, y) (Number_O::compare((x), (y)) < 0)
#define clasp_greater(x, y) (Number_O::compare((x), (y)) > 0)

template <std::floating_point Float> inline Float _signum(Float x) {
  return (std::fpclassify(x) == FP_ZERO) ? x : std::copysign(Float{1}, x);
}

inline Number_sp Number_O::signum(const Number_sp num) {
  if (num.fixnump()) {
    Fixnum fn = num.unsafe_fixnum();
    if (fn == 0)
      return immediate_fixnum<Number_O>(0);
    if (fn > 0)
      return immediate_fixnum<Number_O>(1);
    return immediate_fixnum<Number_O>(-1);
  }
#ifdef CLASP_SHORT_FLOAT
  if (num.short_floatp())
    return ShortFloat_O::create(_signum(num.unsafe_short_float()));
#endif
  if (num.single_floatp())
    return SingleFloat_dummy_O::create(_signum(num.unsafe_single_float()));
  return num->signum_();
}

inline Number_sp clasp_one_plus(Number_sp num) {
  if (num.fixnump()) {
    gc::Fixnum fixnum = num.unsafe_fixnum();
    if (fixnum == MOST_POSITIVE_FIXNUM) {
      // will overflow to a bignum
      fixnum = (MOST_POSITIVE_FIXNUM + 1);
      return Integer_O::create(fixnum);
    } else
      return immediate_fixnum<Number_O>(fixnum + 1);
  }
#ifdef CLASP_SHORT_FLOAT
  if (num.short_floatp())
    return ShortFloat_O::create(num.unsafe_short_float() + short_float_t{1});
#endif
  if (num.single_floatp())
    return SingleFloat_dummy_O::create(num.unsafe_single_float() + single_float_t{1});
  return num->onePlus_();
}

inline Number_sp clasp_one_minus(Number_sp num) {
  if (num.fixnump()) {
    gc::Fixnum fixnum = num.unsafe_fixnum();
    if (fixnum == MOST_NEGATIVE_FIXNUM) {
      // will overflow to a bignum
      fixnum = (MOST_NEGATIVE_FIXNUM - 1);
      return Integer_O::create(fixnum);
    } else
      return immediate_fixnum<Number_O>(fixnum - 1);
  }
#ifdef CLASP_SHORT_FLOAT
  if (num.short_floatp())
    return ShortFloat_O::create(num.unsafe_short_float() - short_float_t{1});
#endif
  if (num.single_floatp())
    return SingleFloat_dummy_O::create(num.unsafe_single_float() - single_float_t{1});
  return num->oneMinus_();
}

CL_LISPIFY_NAME(negate);
DOCGROUP(clasp)
CL_DEFUN inline Number_sp clasp_negate(Number_sp num) {
  if (num.fixnump()) {
    gc::Fixnum fixnum = num.unsafe_fixnum();
    if (fixnum == MOST_NEGATIVE_FIXNUM) {
      // will overflow to a bignum when negated
      fixnum = (MOST_POSITIVE_FIXNUM + 1);
      return Integer_O::create(fixnum);
    } else
      return immediate_fixnum<Number_O>(-fixnum);
  }
#ifdef CLASP_SHORT_FLOAT
  if (num.short_floatp())
    return ShortFloat_O::create(-num.unsafe_short_float());
#endif
  if (num.single_floatp())
    return SingleFloat_dummy_O::create(-num.unsafe_single_float());
  return num->negate_();
}

inline Integer_sp clasp_shift_left(Integer_sp n, Fixnum bits) {
  if (n.fixnump()) {
    Fixnum y = (uint64_t)n.raw_();
    if (y > 0) {
      int clz = fixnum_clz(y);
      if (clz > bits) {
        y <<= bits;
        Integer_sp result((gctools::Tagged)y);
        return result;
      }
    } else if (y < 0) {
      int clrsb = fixnum_clrsb(y);
      if (clrsb > bits) {
        y <<= bits;
        Integer_sp result((gctools::Tagged)y);
        return result;
      }
    } else if (y == 0) {
      Integer_sp result((gctools::Tagged)0);
      return result;
    }
    Bignum val(static_cast<signed long>(n.unsafe_fixnum()));
    Bignum res;
    mpz_mul_2exp(res.get_mpz_t(), val.get_mpz_t(), bits);
    return Integer_O::create(res);
  } else
    return n->shift_left(bits);
}

inline Integer_sp clasp_shift_right(Integer_sp n, Fixnum nbits) {
  if (n.fixnump()) {
    Fixnum y = n.unsafe_fixnum();
    if (nbits >= gc::fixnum_bits)
      y = (y < 0) ? -1 : 0;
    else
      y >>= nbits;
    return immediate_fixnum<Integer_O>(y);
  } else {
    return n->shift_right(nbits);
  }
}

inline gc::Fixnum clasp_integer_length(Integer_sp x) {
  if (x.fixnump()) {
    Fixnum i(x.unsafe_fixnum());
    // clrsb counts _redundant_ sign bits, so the highest
    // value it can return is [number of bits in a word] - 1.
    // unsafe_fixnum untags, so nothing to worry about there.
    return sizeof(Fixnum) * CHAR_BIT - 1 - fixnum_clrsb(i);
  }
  return x->bit_length_();
}

// CLASP_TO_... FUNCTIONS

Fixnum clasp_to_fixnum(core::T_sp);
short clasp_to_short(core::T_sp);
unsigned short clasp_to_ushort(core::T_sp);
int clasp_to_int(core::T_sp);
unsigned int clasp_to_uint(core::T_sp);
long clasp_to_long(core::T_sp);
unsigned long clasp_to_ulong(core::T_sp);
long long clasp_to_longlong(core::T_sp);
unsigned long long clasp_to_ulonglong(core::T_sp);
int8_t clasp_to_int8_t(core::T_sp);
uint8_t clasp_to_uint8_t(core::T_sp);
int16_t clasp_to_int16_t(core::T_sp);
uint16_t clasp_to_uint16_t(core::T_sp);
int32_t clasp_to_int32_t(core::T_sp);
uint32_t clasp_to_uint32_t(core::T_sp);
int64_t clasp_to_int64_t(core::T_sp);
uint64_t clasp_to_uint64_t(core::T_sp);
uintptr_t clasp_to_uintptr_t(core::T_sp);
intptr_t clasp_to_intptr_t(core::T_sp);
ssize_t clasp_to_ssize_t(core::T_sp);
mpz_class clasp_to_mpz(core::T_sp);

float clasp_to_float(core::Number_sp);
double clasp_to_double(core::Number_sp);
long_float_t clasp_to_long_float(core::Number_sp);

// END OF CLASP_TO_... FUNCTIONS

CL_LISPIFY_NAME(reciprocal);
DOCGROUP(clasp)
CL_DEFUN inline Number_sp clasp_reciprocal(Number_sp x) {
  if (x.fixnump()) {
    Integer_sp ix = gc::As_unsafe<Integer_sp>(x);
    Fixnum fx = x.unsafe_fixnum();
    switch (fx) {
    case 1:
      return x;
    case -1:
      return x;
    case 0:
      clasp_report_divide_by_zero(x);
    default:
      if (fx > 0)
        return Ratio_O::create_primitive(clasp_make_fixnum(1), ix);
      else
        return Ratio_O::create_primitive(clasp_make_fixnum(-1), clasp_make_fixnum(-fx));
    }
  }
#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp())
    return ShortFloat_O::create(short_float_t{1.0} / x.unsafe_short_float());
#endif
  if (x.single_floatp())
    return clasp_make_single_float(single_float_t{1.0} / x.unsafe_single_float());
  return x->reciprocal_();
}

inline Number_sp clasp_exp(Number_sp x) {
  if (x.fixnump())
    return clasp_make_single_float(std::exp((single_float_t)x.unsafe_fixnum()));
#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp())
    return ShortFloat_O::create(std::exp(x.unsafe_short_float()));
#endif
  if (x.single_floatp())
    return clasp_make_single_float(std::exp(x.unsafe_single_float()));
  return x->exp_();
}

inline Number_sp clasp_conjugate(Number_sp x) {
  if (gc::IsA<Complex_sp>(x))
    return gc::As_unsafe<Complex_sp>(x)->conjugate();
  else
    return x;
}

inline Number_sp Number_O::abs(Number_sp x) {
  if (x.fixnump()) {
    gc::Fixnum fixnum = x.unsafe_fixnum();
    if (fixnum == MOST_NEGATIVE_FIXNUM)
      return Integer_O::create(MOST_POSITIVE_FIXNUM + 1);
    return clasp_make_fixnum(std::abs(fixnum));
  }
#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp())
    return make_float(std::abs(x.unsafe_short_float()));
#endif
  if (x.single_floatp())
    return make_float(std::abs(x.unsafe_single_float()));
  return x->abs_();
}

inline Number_sp Number_O::sqrt(const Number_sp x) {
  if (x.fixnump())
    return float_sqrt((single_float_t)x.unsafe_fixnum());
#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp())
    return make_float(std::sqrt(x.unsafe_short_float()));
#endif
  if (x.single_floatp())
    return float_sqrt(x.unsafe_single_float());
  return x->sqrt_();
}

inline Number_sp Number_O::sin(const Number_sp x) {
  if (x.fixnump())
    return SingleFloat_dummy_O::create(std::sin((single_float_t)x.unsafe_fixnum()));
#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp())
    return make_float(std::sin(x.unsafe_short_float()));
#endif
  if (x.single_floatp())
    return SingleFloat_dummy_O::create(std::sin(x.unsafe_single_float()));
  return x->sin_();
}

template <std::floating_point Float, std::floating_point Float2 = Float> Number_sp _asin(Float z) {
  if (z >= Float{-1} && z <= Float{1})
    return Number_O::make_float((Float)std::asin((Float2)z));

  return Complex_O::create(Number_O::make_float(std::copysign(std::numbers::pi_v<Float> * Float{0.5}, z)),
                           Number_O::make_float(std::asinh(std::copysign(std::sqrt(z * z - Float{1.0}), -z))));
}

inline Number_sp Number_O::asin(const Number_sp x) {
  if (x.fixnump())
#ifdef _TARGET_OS_DARWIN
    return _asin<single_float_t, double_float_t>(x.unsafe_fixnum());
#else
    return _asin((single_float_t)x.unsafe_fixnum());
#endif
#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp())
    return _asin(x.unsafe_short_float());
#endif
  if (x.single_floatp())
#ifdef _TARGET_OS_DARWIN
    return _asin<single_float_t, double_float_t>(x.unsafe_single_float());
#else
    return _asin(x.unsafe_single_float());
#endif
  return x->asin_();
}

inline Number_sp Number_O::cos(Number_sp x) {
  if (x.fixnump())
    return SingleFloat_dummy_O::create(std::cos((single_float_t)x.unsafe_fixnum()));
#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp())
    return ShortFloat_O::create(std::cos(x.unsafe_short_float()));
#endif
  if (x.single_floatp())
    return SingleFloat_dummy_O::create(std::cos(x.unsafe_single_float()));
  return x->cos_();
}

template <std::floating_point Float, std::floating_point Float2 = Float> Number_sp _acos(Float z) {
  if (z >= Float{-1} && z <= Float{1})
    return Number_O::make_float((Float)std::acos((Float2)z));

  return Complex_O::create(Number_O::make_float((z > Float{0.0}) ? Float{0.0} : std::numbers::pi_v<Float>),
                           Number_O::make_float(std::asinh(std::copysign(std::sqrt(z * z - Float{1.0}), z))));
}

inline Number_sp Number_O::acos(const Number_sp x) {
  if (x.fixnump())
    return _acos<single_float_t, double_float_t>((single_float_t)x.unsafe_fixnum());
#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp())
    return _acos(x.unsafe_short_float());
#endif
  if (x.single_floatp())
#ifdef _TARGET_OS_DARWIN
    return _acos<single_float_t, double_float_t>(x.unsafe_single_float());
#else
    return _acos(x.unsafe_single_float());
#endif
  return x->acos_();
}

inline Number_sp Number_O::tan(Number_sp x) {
  if (x.fixnump())
    return SingleFloat_dummy_O::create(std::tan((single_float_t)x.unsafe_fixnum()));
#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp())
    return ShortFloat_O::create(std::tan(x.unsafe_short_float()));
#endif
  if (x.single_floatp())
    return SingleFloat_dummy_O::create(std::tan(x.unsafe_single_float()));
  return x->tan_();
}

inline Number_sp Number_O::atan(Number_sp x) {
  if (x.fixnump())
#ifdef _TARGET_OS_DARWIN
    return SingleFloat_dummy_O::create(std::atan((double_float_t)x.unsafe_fixnum()));
#else
    return SingleFloat_dummy_O::create(std::atan((single_float_t)x.unsafe_fixnum()));
#endif
#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp())
    return ShortFloat_O::create(std::atan(x.unsafe_short_float()));
#endif
  if (x.single_floatp())
#ifdef _TARGET_OS_DARWIN
    return SingleFloat_dummy_O::create(std::atan((double_float_t)x.unsafe_single_float()));
#else
    return SingleFloat_dummy_O::create(std::atan(x.unsafe_single_float()));
#endif
  return x->atan_();
}

inline Number_sp Number_O::sinh(Number_sp x) {
  if (x.fixnump())
    return SingleFloat_dummy_O::create(std::sinh((single_float_t)x.unsafe_fixnum()));
#ifdef CLASP_SHORT_FLOAT
  if (x.single_floatp())
    return ShortFloat_O::create(std::sinh(x.unsafe_short_float()));
#endif
  if (x.single_floatp())
    return SingleFloat_dummy_O::create(std::sinh(x.unsafe_single_float()));
  return x->sinh_();
}

inline Number_sp Number_O::cosh(Number_sp x) {
  if (x.fixnump())
    return SingleFloat_dummy_O::create(std::cosh((single_float_t)x.unsafe_fixnum()));
#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp())
    return ShortFloat_O::create(std::cosh(x.unsafe_short_float()));
#endif
  if (x.single_floatp())
    return SingleFloat_dummy_O::create(std::cosh(x.unsafe_single_float()));
  return x->cosh_();
}

inline Number_sp Number_O::tanh(Number_sp x) {
  if (x.fixnump())
    return SingleFloat_dummy_O::create(std::tanh((single_float_t)x.unsafe_fixnum()));
#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp())
    return ShortFloat_O::create(std::tanh(x.unsafe_short_float()));
#endif
  if (x.single_floatp())
    return SingleFloat_dummy_O::create(std::tanh(x.unsafe_single_float()));
  return x->tanh_();
}

inline Real_sp Number_O::realpart(const Number_sp x) {
#ifdef CLASP_SHORT_FLOAT
  if (x.fixnump() || x.single_floatp() || x.short_floatp())
    return x.as_unsafe<Real_O>();
#else
  if (x.fixnump() || x.single_floatp())
    return x.as_unsafe<Real_O>();
#endif
  return x->realpart_();
}

inline Real_sp Number_O::imagpart(const Number_sp x) {
  if (x.fixnump())
    return clasp_make_fixnum(0);
#ifdef CLASP_SHORT_FLOAT
  if (x.short_floatp())
    return make_float(std::copysign(short_float_t{0.0}, x.unsafe_short_float()));
#endif
  if (x.single_floatp())
    return make_float(std::copysign(single_float_t{0.0}, x.unsafe_single_float()));
  return x->imagpart_();
}

inline Number_sp Number_O::negate(const Number_sp num) {
  if (num.fixnump()) {
    gc::Fixnum fixnum = num.unsafe_fixnum();
    if (fixnum == MOST_NEGATIVE_FIXNUM) {
      // will overflow to a bignum when negated
      fixnum = (MOST_POSITIVE_FIXNUM + 1);
      return Integer_O::create(fixnum);
    } else
      return immediate_fixnum<Number_O>(-fixnum);
  }
#ifdef CLASP_SHORT_FLOAT
  if (num.short_floatp())
    return ShortFloat_O::create(-num.unsafe_short_float());
#endif
  if (num.single_floatp())
    return SingleFloat_dummy_O::create(-num.unsafe_single_float());
  return num->negate_();
}

}; // namespace core
