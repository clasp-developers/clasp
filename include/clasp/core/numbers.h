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
#pragma GCC diagnostic push
// #pragma GCC diagnostic ignored "-Wunused-local-typedef"
#pragma GCC diagnostic pop

#include <clasp/core/object.h>
#include <clasp/core/numerics.h>
#include <clasp/core/bignum.fwd.h>
#include <clasp/core/numbers.fwd.h>

// Class Hierarchy
// Number_0
//   Real_O
//     Rational_O
//       Integer_O
//         Fixnum_dummy_O (immediate)
//         Bignum_O
//       Ratio_O
//     Float_O
//       ShortFloat_O
//       SingleFloat_dummy_O (immediate)
//       DoubleFloat_O
//       LongFloat_O
//   Complex_O

#define CLASP_PI_D 3.14159265358979323846264338327950288
#define CLASP_PI_L 3.14159265358979323846264338327950288l
#define CLASP_PI2_D 1.57079632679489661923132169163975144
#define CLASP_PI2_L 1.57079632679489661923132169163975144l

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
};                                      // namespace cl

namespace core {
// TYPE ERRORS

core::Fixnum not_fixnum_error(core::T_sp o);

// TYPE IDS

enum class NumberType : uint8_t {
  Fixnum = 0,
  Bignum = 1,
  Ratio = 2,
  ShortFloat = 3,
  SingleFloat = 4,
  DoubleFloat = 5,
  LongFloat = 6,
  Complex = 7,
  NUM = 8
};

inline NumberType operator|(NumberType x, NumberType y) {
  return static_cast<NumberType>(std::max(static_cast<uint8_t>(x), static_cast<uint8_t>(y)));
}

bool clasp_zerop(Number_sp num);
bool clasp_plusp(Real_sp num);
bool clasp_minusp(Real_sp num);
bool clasp_evenp(Integer_sp num);
bool clasp_oddp(Integer_sp num);
Number_sp clasp_abs(Number_sp num);
Number_sp clasp_signum(Number_sp num);
Number_sp clasp_one_plus(Number_sp num);
Number_sp clasp_one_minus(Number_sp num);
Number_sp clasp_negate(Number_sp num);
bool clasp_float_nan_p(Float_sp num);
bool clasp_float_infinity_p(Float_sp num);
NumberType clasp_t_of(Number_sp num);
Integer_sp clasp_shift_left(Integer_sp num, Fixnum nbits);
Integer_sp clasp_shift_right(Integer_sp num, Fixnum nbits);
gc::Fixnum clasp_integer_length(Integer_sp x);

Fixnum_sp clasp_make_fixnum(gc::Fixnum i);
SingleFloat_sp clasp_make_single_float(float d);
DoubleFloat_sp clasp_make_double_float(double d);
Number_sp clasp_log1_complex_inner(Number_sp r, Number_sp i);
void clasp_report_divide_by_zero(Number_sp x);

template <typename T> inline Number_sp make_number(T x);

template <> inline Number_sp make_number(float x) { return gc::make_tagged_single_float<core::SingleFloat_I>(x); }

typedef double LongFloat;

template <typename Float> inline Float _log1p(Float x) {
  Float u = Float{1} + x;
  return (u == Float{1}) ? Float{0} : ((std::log(u) * x) / (u - Float{1}));
}

Number_sp contagion_add(Number_sp na, Number_sp nb);
Number_sp contagion_sub(Number_sp na, Number_sp nb);
Number_sp contagion_mul(Number_sp na, Number_sp nb);
Number_sp contagion_div(Number_sp na, Number_sp nb);
int basic_compare(Number_sp na, Number_sp nb);

SMART(Number);
class Number_O : public General_O {
  LISP_ABSTRACT_CLASS(core, ClPkg, Number_O, "number", General_O);

public:
  static Number_sp create(double val);
  static Number_sp create(gc::Fixnum val);
  //	static Number_sp create(size_t val);
public:
  virtual Number_sp signum_() const { SUBCLASS_MUST_IMPLEMENT(); };
  virtual Number_sp reciprocal_() const { SUBCLASS_MUST_IMPLEMENT(); };
  virtual Number_sp abs_() const { SUBCLASS_MUST_IMPLEMENT(); };
  virtual bool equal(T_sp obj) const override;
  virtual bool equalp(T_sp obj) const override;

  // log(x) (i.e. natural log)
  virtual Number_sp log1_() const { SUBIMP(); };
  // log(x+1)
  virtual Number_sp log1p_() const;

  virtual Number_sp sqrt_() const { SUBIMP(); };
  virtual Rational_sp rational_() const = 0;
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
  virtual float as_float_() const { SUBIMP(); };
  virtual double as_double_() const { SUBIMP(); }
  virtual LongFloat as_long_float_() const { SUBIMP(); };

  virtual Number_sp sin_() const { SUBIMP(); };
  virtual Number_sp cos_() const { SUBIMP(); };
  virtual Number_sp tan_() const { SUBIMP(); };
  virtual Number_sp sinh_() const { SUBIMP(); };
  virtual Number_sp cosh_() const { SUBIMP(); };
  virtual Number_sp tanh_() const { SUBIMP(); };

  virtual void sxhash_(HashGenerator& hg) const override { SUBIMP(); };
  Number_O(){};
  virtual ~Number_O(){};

  inline static NumberType number_type(Number_sp n) {
    if (n.fixnump())
      return NumberType::Fixnum;
    else if (n.single_floatp())
      return NumberType::SingleFloat;
    else if (gc::IsA<Bignum_sp>(n))
      return NumberType::Bignum;
    else if (gc::IsA<Ratio_sp>(n))
      return NumberType::Ratio;
    else if (gc::IsA<ShortFloat_sp>(n))
      return NumberType::ShortFloat;
    else if (gc::IsA<DoubleFloat_sp>(n))
      return NumberType::DoubleFloat;
#ifdef CLASP_LONG_FLOAT
    else if (gc::IsA<LongFloat_sp>(n))
      return NumberType::LongFloat;
#endif
    else if (gc::IsA<Complex_sp>(n))
      return NumberType::Complex;
    UNREACHABLE();
  }

  static Number_sp atan(Number_sp num);
  static Number_sp atan2(Number_sp y, Number_sp x);
};

SMART(Real);
class Real_O : public Number_O {
  LISP_ABSTRACT_CLASS(core, ClPkg, Real_O, "real", Number_O);

public:
  virtual double as_double_() const override { SUBIMP(); };

  // functions shared by all Real
  virtual bool plusp_() const { SUBIMP(); };
  virtual bool minusp_() const { SUBIMP(); };

  Real_O(){};
  virtual ~Real_O(){};
};

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
  virtual Number_sp cos_() const override;
  virtual Number_sp tan_() const override;
  virtual Number_sp sinh_() const override;
  virtual Number_sp cosh_() const override;
  virtual Number_sp tanh_() const override;

  Rational_O(){};
  virtual ~Rational_O(){};
};

SMART(Integer);
class Integer_O : public Rational_O {
  LISP_ABSTRACT_CLASS(core, ClPkg, Integer_O, "integer", Rational_O);

public:
  /*! Return a Cons (integer low high) */
  static T_sp makeIntegerType(gc::Fixnum low, gc::Fixnum high);

  static Integer_sp create(std::signed_integral auto v);
  static Integer_sp create(std::unsigned_integral auto v);
  static Integer_sp create(std::floating_point auto v);

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
  Integer_O(){};
  virtual ~Integer_O(){};
};
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

  Float_O(){};
  virtual ~Float_O(){};
};

SMART(ShortFloat);
class ShortFloat_O : public Float_O {
  LISP_ABSTRACT_CLASS(core, ClPkg, ShortFloat_O, "ShortFloat", Float_O);

private:
  float _Value;

public:
  static ShortFloat_sp create(float nm) {
    auto sf = gctools::GC<ShortFloat_O>::allocate_with_default_constructor();
    sf->_Value = nm;
    return sf;
  };

public:
  float get() const { return this->_Value; };
  void sxhash_(HashGenerator& hg) const override;
  //	virtual Number_sp copy() const;
  Number_sp signum_() const override;
  string __repr__() const override;
  Number_sp abs_() const override;
  bool isnan_() const override { return std::isnan(this->_Value); }; // NaN is supposed to be the only value that != itself!
  bool isinf_() const override { return std::isinf(this->_Value); };

public:
  //	virtual	bool	eqn(T_sp obj) const;
  virtual bool eql_(T_sp obj) const override;
  virtual Number_sp reciprocal_() const override;

  // math routines shared by all numbers
  virtual bool zerop_() const override { return this->_Value == 0.0; };
  virtual Number_sp negate_() const override { return ShortFloat_O::create(-this->_Value); };

  virtual Number_sp onePlus_() const override { return ShortFloat_O::create(this->_Value + 1.0); };
  virtual Number_sp oneMinus_() const override { return ShortFloat_O::create(this->_Value - 1.0); };

  // shared by real
  virtual bool plusp_() const override { return this->_Value > 0.0; };
  virtual bool minusp_() const override { return this->_Value < 0.0; };

  virtual float as_float_() const override;
  virtual double as_double_() const override;
  virtual LongFloat as_long_float_() const override;

  Integer_sp castToInteger() const override;

  DEFAULT_CTOR_DTOR(ShortFloat_O);
};

class SingleFloat_dummy_O : public Float_O {
  LISP_ABSTRACT_CLASS(core, ClPkg, SingleFloat_dummy_O, "SingleFloat", Float_O);
};

inline SingleFloat_sp make_single_float(float x) { return gc::make_tagged_single_float<core::SingleFloat_I>(x); };
inline float unbox_single_float(SingleFloat_sp x) { return x.unsafe_single_float(); };
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

public:
private:
  double _Value;

public:
  static DoubleFloat_sp create(double nm) {
    auto v = gctools::GC<DoubleFloat_O>::allocate_with_default_constructor();
    v->set(nm);
    return v;
  };

public:
  static Rational_sp rational(double val);

public:
  void sxhash_(HashGenerator& hg) const override;
  //	virtual Number_sp copy() const;
  string __repr__() const override;
  void set(double val) { this->_Value = val; };
  double get() const { return this->_Value; };
  Number_sp signum_() const override;
  Number_sp abs_() const override {
    return DoubleFloat_O::create(fabs(this->_Value));
  };
  bool isnan_() const override { return std::isnan(this->_Value); }; // NaN is supposed to be the only value that != itself!!!!
  bool isinf_() const override { return std::isinf(this->_Value); };

public:
  virtual bool eql_(T_sp obj) const override;

  // math routines shared by all numbers
  bool zerop_() const override { return this->_Value == 0.0; };
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

  virtual float as_float_() const override;
  virtual double as_double_() const override;
  virtual LongFloat as_long_float_() const override;

  Integer_sp castToInteger() const override;

  virtual Number_sp exp_() const override;

  virtual Number_sp sin_() const override;
  virtual Number_sp cos_() const override;
  virtual Number_sp tan_() const override;
  virtual Number_sp sinh_() const override;
  virtual Number_sp cosh_() const override;
  virtual Number_sp tanh_() const override;
  virtual Rational_sp rational_() const final { return DoubleFloat_O::rational(this->_Value); };
  DoubleFloat_O() : _Value(0.0){};
  virtual ~DoubleFloat_O(){};
};

template <> inline Number_sp make_number(double x) { return DoubleFloat_O::create(x); }

}; // namespace core

namespace core {
SMART(LongFloat);
class LongFloat_O : public Float_O {
  LISP_ABSTRACT_CLASS(core, ClPkg, LongFloat_O, "LongFloat", Float_O);

public:
private:
  //  LongFloat _Value;
public:
  static DoubleFloat_sp create(LongFloat nm) { return DoubleFloat_O::create(nm); };

public:
  //    virtual Rational_sp rational_() const final { return DoubleFloat_O::rational(this->_Value); };

  DEFAULT_CTOR_DTOR(LongFloat_O);
};
}; // namespace core

namespace core {
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
  Number_sp signum_() const override;
  Number_sp abs_() const override;
  Rational_sp rational_() const override { TYPE_ERROR(this->asSmartPtr(), cl::_sym_Real_O); };

public:
  //	virtual	bool	eqn(T_sp obj) const;
  virtual bool eql_(T_sp obj) const override;

  // math routines shared by all numbers
  bool zerop_() const override { return (clasp_zerop(this->_real) && clasp_zerop(this->_imaginary)); };
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
  virtual Number_sp cos_() const override;
  virtual Number_sp tan_() const override;
  virtual Number_sp sinh_() const override;
  virtual Number_sp cosh_() const override;
  virtual Number_sp tanh_() const override;

  virtual Number_sp reciprocal_() const override;
  Complex_sp conjugate() const;

  virtual void __write__(T_sp strm) const override;

  Complex_O(Real_sp r, Real_sp i) : _real(r), _imaginary(i){};
  Complex_O() : _real(clasp_make_single_float(0.0)), _imaginary(clasp_make_single_float(0.0)){};
  virtual ~Complex_O(){};
};

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
  virtual bool zerop_() const override { return clasp_zerop(this->_numerator); };
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
  Rational_sp rational_() const final { return this->asSmartPtr(); };
  bool isnan_() const;

public:
  virtual bool eql_(T_sp obj) const override;

  Number_sp onePlus_() const override {
    return create(gc::As<Integer_sp>(contagion_add(this->_numerator, this->_denominator)), this->_denominator);
  };
  Number_sp oneMinus_() const override {
    return create(gc::As<Integer_sp>(contagion_sub(this->_numerator, this->_denominator)), this->_denominator);
  };

  virtual float as_float_() const override;
  virtual double as_double_() const override;
  virtual LongFloat as_long_float_() const override;

  // functions shared by all Real

  bool plusp_() const override { return clasp_plusp(this->_numerator); }

  bool minusp_() const override { return clasp_minusp(this->_numerator); }

  virtual void __write__(T_sp strm) const override;

  Ratio_O() : _numerator(clasp_make_fixnum(0)), _denominator(clasp_make_fixnum(1)){};
  virtual ~Ratio_O(){};
};

template <std::integral T> inline Number_sp make_number(T x) {
  if (x <= gc::most_positive_fixnum && x >= gc::most_negative_fixnum)
    return gc::make_tagged_fixnum<core::Fixnum_I>(x);

  return Integer_O::create(x);
}

inline Number_sp clasp_plus(Number_sp na, Number_sp nb) { return contagion_add(na, nb); };
inline Number_sp clasp_minus(Number_sp na, Number_sp nb) { return contagion_sub(na, nb); };
inline Number_sp clasp_times(Number_sp na, Number_sp nb) { return contagion_mul(na, nb); };
inline Number_sp clasp_divide(Number_sp na, Number_sp nb) { return contagion_div(na, nb); };

inline int clasp_number_compare(Number_sp x, Number_sp y) { return basic_compare(x, y); };

template<typename Float> inline Number_sp _sqrt(Float f) {
  if (std::signbit(f))
    return Complex_O::create(make_number(Float{0.0}), make_number(std::sqrt(-f)));

  return make_number(std::sqrt(f));
}

inline Number_sp clasp_log1(Number_sp x) {
  if (x.fixnump()) {
    float f = x.unsafe_fixnum();
    if (f < 0)
      return clasp_log1_complex_inner(x, clasp_make_fixnum(0));
    return clasp_make_single_float(logf(f));
  } else if (x.single_floatp()) {
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
  } else if (x.single_floatp()) {
    float f = x.unsafe_single_float();
    if (std::isnan(f))
      return x;
    if (f < -1)
      return clasp_log1_complex_inner(clasp_one_plus(x), clasp_make_fixnum(0));
    return clasp_make_single_float(_log1p(f));
  }
  return x->log1p_();
};

Number_sp cl__expt(Number_sp x, Number_sp y);
Real_sp cl__mod(Real_sp, Real_sp);

Integer_sp clasp_ash(Integer_sp x, int bits);

inline gctools::Fixnum clasp_safe_fixnum(Number_sp x) { return gc::As<Fixnum_sp>(x).unsafe_fixnum(); }

#if 0
  inline gctools::Fixnum clasp_fixnum(Number_sp x) {
    return unbox_fixnum(Fixnum_sp(x));
  }

  inline float clasp_single_float(Number_sp x) {
    if (x.single_floatp()) {
      return unbox_single_float(x);
    }
  }

  inline double clasp_double_float(Number_sp x) {
    return gc::As<DoubleFloat_sp>(x)->get();
  }
#endif

#ifdef CLASP_LONG_FLOAT
inline LongFloat clasp_long_float(Number_sp x) { return x.as<LongFloat_O>()->get(); }
#endif

Number_sp clasp_make_complex(Real_sp r, Real_sp i);

inline Fixnum_sp clasp_make_fixnum(gc::Fixnum i) { return make_fixnum(i); }

inline Integer_sp _clasp_float_to_integer(float d) { return Integer_O::create(d); }

inline Integer_sp _clasp_double_to_integer(double d) { return Integer_O::create(d); }

inline Integer_sp _clasp_long_float_to_integer(LongFloat d) { return Integer_O::create(d); }

inline Integer_sp _clasp_long_double_to_integer(LongFloat d) { return Integer_O::create(d); }

inline SingleFloat_sp clasp_make_single_float(float d) { return gc::make_tagged_single_float<core::SingleFloat_I>(d); }

inline DoubleFloat_sp clasp_make_double_float(double d) { return DoubleFloat_O::create(d); }

#ifdef CLASP_LONG_FLOAT
inline LongFloat_sp clasp_make_long_float(LongFloat d) { return LongFloat_O::create(d); }
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

#define clasp_lowereq(x, y) (clasp_number_compare((x), (y)) <= 0)
#define clasp_greatereq(x, y) (clasp_number_compare((x), (y)) >= 0)
#define clasp_lower(x, y) (clasp_number_compare((x), (y)) < 0)
#define clasp_greater(x, y) (clasp_number_compare((x), (y)) > 0)

}; // namespace core

template <typename Char> struct fmt::formatter<core::NumberType, Char> : fmt::formatter<fmt::basic_string_view<Char>> {
  template <typename FormatContext>
  auto format(const core::NumberType& o, FormatContext& ctx) const -> typename FormatContext::iterator {
    fmt::basic_string_view<Char> name = "unknown";
    switch (o) {
    case core::NumberType::Fixnum:
      name = "Fixnum";
      break;
    case core::NumberType::Bignum:
      name = "Bignum";
      break;
    case core::NumberType::Ratio:
      name = "Ratio";
      break;
    case core::NumberType::ShortFloat:
      name = "ShortFloat";
      break;
    case core::NumberType::SingleFloat:
      name = "SingleFloat";
      break;
    case core::NumberType::DoubleFloat:
      name = "DoubleFloat";
      break;
    case core::NumberType::LongFloat:
      name = "LongFloat";
      break;
    case core::NumberType::Complex:
      name = "Complex";
      break;
    case core::NumberType::NUM:
      name = "NUM";
      break;
    }
    return fmt::formatter<fmt::basic_string_view<Char>>::format(name, ctx);
  }
};

namespace core {

CL_PKG_NAME(ClPkg, plusp);
CL_DEFUN inline bool clasp_plusp(Real_sp num) {
  if (num.fixnump()) {
    return num.unsafe_fixnum() > 0;
  } else if (num.single_floatp()) {
    return num.unsafe_single_float() > 0.0;
  }
  return num->plusp_();
}

CL_PKG_NAME(ClPkg, minusp);
CL_DEFUN inline bool clasp_minusp(Real_sp num) {
  if (num.fixnump()) {
    return num.unsafe_fixnum() < 0;
  } else if (num.single_floatp()) {
    return num.unsafe_single_float() < 0.0;
  }
  return num->minusp_();
}

CL_PKG_NAME(ClPkg, evenp);
CL_DEFUN inline bool clasp_evenp(Integer_sp num) {
  if (num.fixnump()) {
    return (num.unsafe_fixnum() % 2) == 0;
  }
  return num->evenp_();
}

CL_PKG_NAME(ClPkg, oddp);
DOCGROUP(clasp)
CL_DEFUN inline bool clasp_oddp(Integer_sp num) {
  // for negative numbers num % 2 == 1 does not work, since -1 is returned
  if (num.fixnump()) {
    return (num.unsafe_fixnum() % 2) != 0;
  }
  // now num must be a bignum, works fine
  return num->oddp_();
}

CL_PKG_NAME(ClPkg, abs);
DOCGROUP(clasp)
CL_DEFUN inline Number_sp clasp_abs(Number_sp num) {
  if (num.fixnump()) {
    gc::Fixnum fixnum = num.unsafe_fixnum();
    if (fixnum == MOST_NEGATIVE_FIXNUM) {
      // will overflow to a bignum
      fixnum = (MOST_POSITIVE_FIXNUM + 1);
      return Integer_O::create(fixnum);
    } else
      return make_number(std::abs(fixnum));
  } else if (num.single_floatp()) {
    return make_number(std::fabs(num.unsafe_single_float()));
  }
  return num->abs_();
}

CL_PKG_NAME(ClPkg, signum);
DOCGROUP(clasp)
CL_DEFUN inline Number_sp clasp_signum(Number_sp num) {
  if (num.fixnump()) {
    Fixnum fn = num.unsafe_fixnum();
    if (fn == 0)
      return make_number(0);
    if (fn > 0)
      return make_number(1);
    return make_number(-1);
  } else if (num.single_floatp()) {
    float fl = num.unsafe_single_float();
    if (fl == 0.0f)
      return make_number(0.0f);
    if (fl < 0.0f)
      return make_number(-1.0f);
    return make_number(1.0f);
  }
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
      return make_number(fixnum + 1);
  } else if (num.single_floatp()) {
    return make_number(num.unsafe_single_float() + 1.0f);
  }
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
      return make_number(fixnum - 1);
  } else if (num.single_floatp()) {
    return make_number(num.unsafe_single_float() - 1.0f);
  }
  return num->oneMinus_();
}

inline bool clasp_zerop(Number_sp num) {
  if (num.fixnump()) {
    return num.unsafe_fixnum() == 0;
  } else if (num.single_floatp()) {
    float fl = num.unsafe_single_float();
    return fl == 0.0;
  }
  return num->zerop_();
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
      return make_number(-fixnum);
  } else if (num.single_floatp()) {
    float fl = num.unsafe_single_float();
    fl = -fl;
    return make_number(fl);
  }
  return num->negate_();
}

inline NumberType clasp_t_of(Number_sp n) {
  if (n.fixnump())
    return NumberType::Fixnum;
  else if (n.single_floatp())
    return NumberType::SingleFloat;
  else if (gc::IsA<Bignum_sp>(n))
    return NumberType::Bignum;
  else if (gc::IsA<Ratio_sp>(n))
    return NumberType::Ratio;
  else if (gc::IsA<ShortFloat_sp>(n))
    return NumberType::ShortFloat;
  else if (gc::IsA<DoubleFloat_sp>(n))
    return NumberType::DoubleFloat;
#ifdef CLASP_LONG_FLOAT
  else if (gc::IsA<LongFloat_sp>(n))
    return NumberType::LongFloat;
#endif
  else if (gc::IsA<Complex_sp>(n))
    return NumberType::Complex;
  UNREACHABLE();
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
    return make_number(y);
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
LongFloat clasp_to_long_float(core::Number_sp);
LongFloat clasp_to_long_double(core::Number_sp);

// END OF CLASP_TO_... FUNCTIONS

inline Number_sp clasp_sqrt(Number_sp z) {
  if (z.fixnump())
    return _sqrt((float)z.unsafe_fixnum());

  if (z.single_floatp())
    return _sqrt(z.unsafe_single_float());

  return z->sqrt_();
}

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
  } else if (x.single_floatp()) {
    float f = x.unsafe_single_float();
    return clasp_make_single_float(1.0 / f);
  }
  return x->reciprocal_();
}

inline Number_sp clasp_exp(Number_sp x) {
  if (x.fixnump()) {
    float f = x.unsafe_fixnum();
    return clasp_make_single_float(expf(f));
  } else if (x.single_floatp()) {
    float f = x.unsafe_single_float();
    return clasp_make_single_float(expf(f));
  }
  return x->exp_();
}

inline Number_sp clasp_sin(Number_sp x) {
  if (x.fixnump()) {
    float f = x.unsafe_fixnum();
    return clasp_make_single_float(sinf(f));
  } else if (x.single_floatp())
    return clasp_make_single_float(sinf(x.unsafe_single_float()));
  return x->sin_();
}
inline Number_sp clasp_cos(Number_sp x) {
  if (x.fixnump()) {
    float f = x.unsafe_fixnum();
    return clasp_make_single_float(cosf(f));
  } else if (x.single_floatp())
    return clasp_make_single_float(cosf(x.unsafe_single_float()));
  return x->cos_();
}
inline Number_sp clasp_tan(Number_sp x) {
  if (x.fixnump()) {
    float f = x.unsafe_fixnum();
    return clasp_make_single_float(tanf(f));
  } else if (x.single_floatp())
    return clasp_make_single_float(tanf(x.unsafe_single_float()));
  return x->tan_();
}

inline Number_sp clasp_sinh(Number_sp x) {
  if (x.fixnump()) {
    float f = x.unsafe_fixnum();
    return clasp_make_single_float(sinhf(f));
  } else if (x.single_floatp())
    return clasp_make_single_float(sinhf(x.unsafe_single_float()));
  return x->sinh_();
}
inline Number_sp clasp_cosh(Number_sp x) {
  if (x.fixnump()) {
    float f = x.unsafe_fixnum();
    return clasp_make_single_float(coshf(f));
  } else if (x.single_floatp())
    return clasp_make_single_float(coshf(x.unsafe_single_float()));
  return x->cosh_();
}
inline Number_sp clasp_tanh(Number_sp x) {
  if (x.fixnump()) {
    float f = x.unsafe_fixnum();
    return clasp_make_single_float(tanhf(f));
  } else if (x.single_floatp())
    return clasp_make_single_float(tanhf(x.unsafe_single_float()));
  return x->tanh_();
}

inline Number_sp clasp_conjugate(Number_sp x) {
  if (gc::IsA<Complex_sp>(x))
    return gc::As_unsafe<Complex_sp>(x)->conjugate();
  else
    return x;
}

inline bool clasp_float_nan_p(Float_sp num) {
  if (num.single_floatp()) {
    float f = num.unsafe_single_float();
    return std::isnan(f);
  }
  return num->isnan_();
}

inline bool clasp_float_infinity_p(Float_sp num) {
  if (num.single_floatp()) {
    float f = num.unsafe_single_float();
    return std::isinf(f);
  }
  // test for isinf not for isnan, good old friend copy paste
  return num->isinf_();
}

}; // namespace core
