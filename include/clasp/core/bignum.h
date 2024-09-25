#pragma once
/*
    File: bignum.h
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
#include <clasp/core/object.h>
#include <clasp/core/numbers.h>
#include <clasp/core/bignum.fwd.h>
#include <concepts> // integral

namespace core {

template <class T>
concept unsigned_limb = std::is_integral<T>::value && std::is_unsigned<T>::value && sizeof(T) <= sizeof(mp_limb_t);

template <class T>
concept unsigned_limbs = std::is_integral<T>::value && std::is_unsigned<T>::value && sizeof(T) > sizeof(mp_limb_t);

template <class T>
concept signed_limb = std::is_integral<T>::value && std::is_signed<T>::value && sizeof(T) <= sizeof(mp_limb_t);

template <class T>
concept signed_limbs = std::is_integral<T>::value && std::is_signed<T>::value && sizeof(T) > sizeof(mp_limb_t);

class Bignum_O;
};

template <> struct gctools::GCInfo<core::Bignum_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = atomic;
};

namespace core {
class Bignum_O : public Integer_O {
  LISP_CLASS(core, ClPkg, Bignum_O, "Bignum", Integer_O);

public:
  Bignum_O(int64_t signed_length, mp_limb_t initialElement = 0, bool initialElementSupplied = false, size_t initialContentsSize = 0,
           const mp_limb_t* initialContents = NULL)
      : _limbs(signed_length, initialElement, initialElementSupplied, initialContentsSize, initialContents) {}

public:
  typedef mp_limb_t value_type;

public: // instance variables here
  gctools::GCArraySignedLength_moveable<mp_limb_t> _limbs;

public: // Functions here
  // NOTE: Not just create since (a) it's the lowest level, and
  // (b) it would cause ambiguity with create(fixnum).
  static Bignum_sp create_from_limbs(int64_t signed_number_of_limbs, mp_limb_t initialElement = 0,
                                     bool initialElementSupplied = false, size_t initialContentsSize = 0,
                                     const mp_limb_t* initialContents = NULL) {
    auto b = gctools::GC<Bignum_O>::allocate_container<gctools::RuntimeStage>(false /*static_vector_p*/, signed_number_of_limbs,
                                                                              initialElement, initialElementSupplied,
                                                                              initialContentsSize, initialContents);
    return b;
  };
  static Bignum_sp create(const mpz_class&);

  template <unsigned_limb T> static Bignum_sp create(T v) {
    Bignum_sp b = create_from_limbs(1);
    b->_limbs[0] = v;
    return b;
  }

  template <signed_limb T> static Bignum_sp create(T v) {
    Bignum_sp b = create_from_limbs((v < 0) ? -1 : 1);
    b->_limbs[0] = std::abs(v);
    return b;
  }

  template <unsigned_limbs T> static Bignum_sp create(T v) {
    constexpr size_t limb_width = CHAR_BIT * sizeof(mp_limb_t);
    size_t len = (std::bit_width(v) + limb_width - 1) / limb_width;
    Bignum_sp b = create_from_limbs(len);

    for (size_t i = 0; i < len; i++) {
      b->_limbs[i] = static_cast<mp_limb_t>(v);
      v = v >> limb_width;
    }

    return b;
  }

  template <signed_limbs T> static Bignum_sp create(T v) {
    constexpr size_t limb_width = CHAR_BIT * sizeof(mp_limb_t);
    using UT = typename std::make_unsigned<T>::type;
    bool negative = v < 0;
    UT w = std::abs(v);
    size_t len = (std::bit_width(w) + limb_width - 1) / limb_width;
    Bignum_sp b = create_from_limbs(negative ? -len : len);

    for (size_t i = 0; i < len; i++) {
      b->_limbs[i] = static_cast<mp_limb_t>(w);
      w = w >> limb_width;
    }

    return b;
  }

  template <std::floating_point Float> static Bignum_sp create(Float v) {
    auto q = float_convert<Float>::float_to_quadruple(v);
    Bignum_sp b = gc::As_unsafe<Bignum_sp>(clasp_ash(create(q.significand), q.exponent))                      ;
    if (q.sign < 0)
      return gc::As_unsafe<Bignum_sp>(clasp_negate(b));

    return b;
  }

  static Bignum_sp make(const string& value_in_string);

  mp_size_t length() const { return _limbs.signedLength(); }
  const mp_limb_t* limbs() const { return &(_limbs._Data[0]); }

  void sxhash_(HashGenerator& hg) const;

  mpz_class mpz() const;

  string __repr__() const override;

  Number_sp signum_() const override;
  Number_sp abs_() const override;
  Number_sp negate_() const override;

  Number_sp oneMinus_() const override;
  Number_sp onePlus_() const override;

  Number_sp log1_() const override;
  Number_sp sqrt_() const override;
  Number_sp reciprocal_() const override;
  Rational_sp as_rational_() const override { return this->asSmartPtr(); };
  virtual short_float_t as_short_float_() const override;
  virtual single_float_t as_single_float_() const override;
  virtual double as_double_float_() const override;
  virtual long_float_t as_long_float_() const override;

  virtual bool zerop_() const override { return false; }
  virtual bool plusp_() const override { return (this->length() > 0); }
  virtual bool minusp_() const override { return (this->length() < 0); }
  gc::Fixnum popcount() const;
  Rational_sp ratdivide(Integer_sp) const override;
  virtual gc::Fixnum bit_length_() const override;
  Integer_sp shift_left(gc::Fixnum) const override;
  Integer_sp shift_right(gc::Fixnum) const override;

  virtual bool eql_(T_sp obj) const override;
  virtual bool evenp_() const override { return !((this->limbs())[0] & 1); }
  virtual bool oddp_() const override { return (this->limbs())[0] & 1; }

  template <std::unsigned_integral integral> integral to_integral() const {
    constexpr auto limb_width = sizeof(mp_limb_t) * 8;
    constexpr auto integral_width = std::bit_width(std::numeric_limits<integral>::max());
    constexpr auto max_limb_count = 1 + ((sizeof(integral) - 1) / sizeof(mp_limb_t));

    const mp_limb_t* limbs = this->limbs();
    mp_size_t len = this->length();

    if ((len > -1) && ((limb_width * (len - 1) + std::bit_width(limbs[len - 1])) <= integral_width)) {
      integral value = 0;
      for (mp_size_t i = 0; i < length(); i++)
        value |= (integral)limbs[i] << (i * limb_width);
      return value;
    }

    TYPE_ERROR(this->asSmartPtr(), Cons_O::createList(cl::_sym_Integer_O, Integer_O::create(std::numeric_limits<integral>::min()),
                                                      Integer_O::create(std::numeric_limits<integral>::max())));
  }

  template <std::signed_integral integral> integral to_integral() const {
    integral mn = std::numeric_limits<integral>::min();
    integral mx = std::numeric_limits<integral>::max();
    // First, if integral can only hold fixnums, conversion will always fail.
    if (!((mn >= gc::most_negative_fixnum) && (mx <= gc::most_positive_fixnum))) {
      // integral is big enough to hold some bignums.
      // The actual conversion is a KLUDGE right now.
      // We assume the type is exactly big enough to fit one mp_limb_t.
      mp_size_t len = this->length();
      const mp_limb_t* limbs = this->limbs();
      if (std::is_signed<integral>::value) {
        if (len == 1)
          return limbs[0];
        else if (len == -1)
          return -(limbs[0]);
      } else if (len == 1)
        return limbs[0];
    }
    // Fell through: Bignum is out of range
    TYPE_ERROR(this->asSmartPtr(), Cons_O::createList(cl::_sym_Integer_O, Integer_O::create(mn), Integer_O::create(mx)));
  };

}; // Bignum class

// Remove any high limbs that are equal to zero,
// starting from the right (most significant)
// Can result in zero limbs.
#define BIGNUM_NORMALIZE(NLIMBS, LIMBS)                                                                                            \
  while ((NLIMBS) > 0) {                                                                                                           \
    if ((LIMBS)[(NLIMBS)-1] != 0)                                                                                                  \
      break;                                                                                                                       \
    (NLIMBS)--;                                                                                                                    \
  }

Bignum_sp core__next_from_fixnum(Fixnum);
Integer_sp bignum_result(mp_size_t, const mp_limb_t*);
Bignum_sp core__mul_fixnums(Fixnum, Fixnum);
Bignum_sp core__next_lshift(Bignum_sp, Fixnum);
Integer_sp core__next_rshift(Bignum_sp, Fixnum);
Number_mv core__next_truncate(Bignum_sp, Bignum_sp);
Integer_sp fix_divided_by_next(Fixnum, Bignum_sp);
Number_mv core__next_ftruncate(Bignum_sp, Fixnum);
Integer_sp core__next_gcd(Bignum_sp, Bignum_sp);
Integer_sp core__next_fgcd(Bignum_sp, Fixnum);
int core__next_compare(Bignum_sp, Bignum_sp);

template <std::integral integral> integral clasp_to_integral(T_sp obj) {
  integral mn = std::numeric_limits<integral>::min();
  integral mx = std::numeric_limits<integral>::max();
  if (obj.fixnump()) {
    gc::Fixnum f = obj.unsafe_fixnum();
    if ((mn <= f) && (f <= mx))
      return f;
    else
      TYPE_ERROR(obj, Cons_O::createList(cl::_sym_Integer_O, Integer_O::create(mn), Integer_O::create(mx)));
  } else if (gc::IsA<Bignum_sp>(obj))
    return (gc::As_unsafe<Bignum_sp>(obj))->template to_integral<integral>();
  else if (gc::IsA<Bignum_sp>(obj))
    return (gc::As_unsafe<Bignum_sp>(obj))->template to_integral<integral>();
  else
    TYPE_ERROR(obj, Cons_O::createList(cl::_sym_Integer_O, Integer_O::create(mn), Integer_O::create(mx)));
};

template <std::floating_point Float> Integer_sp Integer_O::create(Float v) {
  if (v >= static_cast<Float>(gc::most_negative_fixnum) && v < static_cast<Float>(gc::most_positive_fixnum))
    return clasp_make_fixnum(v);
  else
    return Bignum_O::create(v);
}

}; // namespace core
