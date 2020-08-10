/*
    File: bignum.cc
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
//#define DEBUG_LEVEL_FULL

#include <boost/format.hpp>
#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/numbers.h>
#include <clasp/core/symbol.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/bignum.h>
#include <clasp/core/wrappers.h>

namespace core {

CL_PKG_NAME(CorePkg,make-bignum);
CL_DEFUN Bignum_sp Bignum_O::make(const string &value_in_string) {
  GC_ALLOCATE(Bignum_O, bn);
  bn->_value = value_in_string;
  return ((bn));
};

LongLongInt Bignum_O::as_LongLongInt_() const {
  LIKELY_if (this->_value.fits_sint_p()) {
    return ((this->_value.get_si()));
  }
  SIMPLE_ERROR(BF("Cannot convert Bignum %s to long long") % this->__repr__());
}

unsigned long long Bignum_O::as_unsigned_long_long_() const {
  if (sizeof(unsigned long long) == sizeof(uint64_t)) {
    return this->as_uint64_();
  }
  SIMPLE_ERROR(BF("Handle unsigned long long != uint64_t"));
  //	TYPE_ERROR(this->asSmartPtr(),Cons_O::createList(cl::_sym_Integer_O,make_fixnum(0),Integer_O::create(gc::most_positive_unsigned_long_long)));
}

void Bignum_O::sxhash_(HashGenerator &hg) const {
  hg.addValue(this->_value);
}

gc::Fixnum Bignum_O::as_int_() const {
  IMPLEMENT_MEF("Implement conversion of Bignum to Fixnum");
  if (this->_value.fits_sint_p()) {
    return ((this->_value.get_si()));
  }
  TYPE_ERROR(this->asSmartPtr(), Cons_O::createList(cl::_sym_Integer_O, make_fixnum(gc::most_negative_int), make_fixnum(gc::most_positive_int)));
}

int64_t Bignum_O::as_int64_() const
{
  size_t sizeinbase2 = mpz_sizeinbase(this->_value.get_mpz_t(),2);

  if ( sizeinbase2 > 64 )
  {
    goto BAD;
  }
  else
  {
    int64_t   val   = 0;
    size_t    count = 0;
    int       sign  = 0;

    int64_t * valP  = (int64_t *)::mpz_export( &val,
                                               &count,
                                               _lisp->integer_ordering()._mpz_import_word_order,
                                               sizeof(int64_t),
                                               _lisp->integer_ordering()._mpz_import_endian,
                                               0,
                                               this->_value.get_mpz_t() );

    sign = mpz_sgn(this->_value.get_mpz_t());
    if ( sign < 0 )
    {
      val = -val;
    }

    return val;
  }

 BAD:

  SIMPLE_ERROR(BF("The value %s won't fit into an int64_t") % _rep_(this->asSmartPtr()));

}

uint64_t Bignum_O::as_uint64_() const
{
  size_t sizeinbase2 = mpz_sizeinbase( this->_value.get_mpz_t(), 2 );

  if ( sizeinbase2 > 64 )
  {
    goto BAD;
  }
  else
  {
    uint64_t   val    = 0;
    size_t     count  = 0;

    uint64_t * valP   = (uint64_t *)::mpz_export( &val,
                                                  &count,
                                                  _lisp->integer_ordering()._mpz_import_word_order,
                                                  sizeof(uint64_t),
                                                  _lisp->integer_ordering()._mpz_import_endian,
                                                  0,
                                                  this->_value.get_mpz_t() );
    return val;
  }

 BAD:

  SIMPLE_ERROR(BF("The value %s won't fit into an uint64_t") % _rep_(this->asSmartPtr()));

}

/*! This helps us debug the as_uint64 function by returning a string representation of the uint64 */
CL_LISPIFY_NAME("core:asUint64String");
CL_DEFMETHOD string Bignum_O::as_uint64_string() const {
  uint64_t ui64 = clasp_to_uint64(this->asSmartPtr());
  stringstream ss;
  ss << ui64;
  return ((ss.str()));
}

CL_LISPIFY_NAME("core:fitsSintP");
CL_DEFMETHOD bool Bignum_O::fits_sint_p() {
  return ((this->_value.fits_sint_p()));
}

// --- TRANSLATION METHODS ---

// -- SHORT --

inline short Bignum_O::as_short() const {
  return static_cast<short>(this->mpz().get_si());
}

inline unsigned short Bignum_O::as_ushort() const {
  return static_cast<unsigned short>(this->mpz().get_ui());
}

// -- INT --

inline int Bignum_O::as_int() const {
  return static_cast<int>(this->mpz().get_si());
}

inline unsigned int Bignum_O::as_uint() const {
  return static_cast<unsigned int>(this->mpz().get_ui());
}

// --  LONG --

inline long Bignum_O::as_long() const {
  return static_cast<long>(this->mpz().get_si());
}

inline unsigned long Bignum_O::as_ulong() const {
  return static_cast<unsigned long>(this->mpz().get_ui());
}

// -- LONG LONG --

inline long long Bignum_O::as_longlong() const {
#ifdef CLASP_MS_WINDOWS_HOST
#error "Add support for windows and long long bignum conversions"
#endif
  return this->as_int64_();
}

inline unsigned long long Bignum_O::as_ulonglong() const {
#ifdef CLASP_MS_WINDOWS_HOST
#error "Add support for windows and unsigned long long bignum conversions"
#endif
  return this->as_uint64_();
}

// -- INT8 --

inline int8_t Bignum_O::as_int8_t() const {
  return static_cast<int8_t>(this->mpz().get_si());
}

inline uint8_t Bignum_O::as_uint8_t() const {
  return static_cast<uint8_t>(this->mpz().get_ui());
}

// -- INT16 --

inline int16_t Bignum_O::as_int16_t() const {
  return static_cast<int16_t>(this->mpz().get_si());
}

inline uint16_t Bignum_O::as_uint16_t() const {
  return static_cast<uint16_t>(this->mpz().get_ui());
}

// -- INT32 --

inline int32_t Bignum_O::as_int32_t() const {
  return static_cast<int32_t>(this->mpz().get_si());
}

inline uint32_t Bignum_O::as_uint32_t() const {
  return static_cast<uint32_t>(this->mpz().get_ui());
}

// -- INT64 --

inline int64_t Bignum_O::as_int64_t() const {
  return static_cast<int64_t>( this->as_int64_() );
}

inline uint64_t Bignum_O::as_uint64_t() const {
  return static_cast<uint64_t>( this->as_uint64_() );
}

// -- UINTPTR_T --

inline uintptr_t Bignum_O::as_uintptr_t() const
{
  if( this->mpz().get_si() >= 0 )
  {
    return static_cast<uintptr_t>( this->mpz().get_si() );
  }

  SIMPLE_ERROR(BF("Value %llud out of range for type UINTPTR_T .") % (unsigned long long) this->mpz().get_si() );
}

// -- PTRDIFF_T --

inline ptrdiff_t Bignum_O::as_ptrdiff_t() const {
  if( this->mpz().get_si() >= 0 ) {
    return static_cast<ptrdiff_t>(  this->mpz().get_si() );
  }
  SIMPLE_ERROR(BF("Value %lld out of range for type PTRDIFF_T .") % (long long) this->mpz().get_si() );
}

// -- SIZE_T --

inline size_t Bignum_O::as_size_t() const {
  if(( this->mpz().get_si() >= gc::most_negative_size ) && ( this->mpz().get_si() <= gc::most_positive_size )) {
    return static_cast<size_t>( this->mpz().get_si() );
  }

  SIMPLE_ERROR(BF("Value %lld out of range for integer type SIZE_T .") % (long long) this->mpz().get_si() );
}

// -- SSIZE_T --

inline ssize_t Bignum_O::as_ssize_t() const {
  if(( this->mpz().get_si() >= gc::most_negative_ssize ) && ( this->mpz().get_si() <= gc::most_positive_ssize )) {
    return static_cast<ssize_t>( this->mpz().get_si() );
  }

  SIMPLE_ERROR(BF("Value %lld out of range for integer type SSIZE_T .") % (long long) this->mpz().get_si() );
}

// --- ---

float Bignum_O::as_float_() const {
  return static_cast<float_t>( (this->_value.get_d()) );
}

double Bignum_O::as_double_() const {
  return static_cast<double>( (this->_value.get_d()) );
}

LongFloat Bignum_O::as_long_float_() const {
  return static_cast<LongFloat>( (this->_value.get_d()) );
}

// --- END OF TRANSLATION METHODS ---

void Bignum_O::setFromString(const string &strVal) {
  this->_value = strVal;
}

gc::Fixnum Bignum_O::bit_length_() const {
  Bignum x = this->_value;
  if (this->minusp_()) {
    // issue #536
    // from ECL: logxor(2,x,ecl_make_fixnum(-1)); before calling mpz_sizeinbase on x
    mpz_class temp;
    mpz_xor(temp.get_mpz_t(),clasp_to_mpz(clasp_make_fixnum(2)).get_mpz_t(), x.get_mpz_t());
    mpz_class temp1;
    mpz_xor(temp1.get_mpz_t(), temp.get_mpz_t(), clasp_to_mpz(clasp_make_fixnum(-1)).get_mpz_t());
    return mpz_sizeinbase(temp1.get_mpz_t(), 2);
  } else {
    return mpz_sizeinbase(x.get_mpz_t(), 2);
  }
}

gc::Fixnum Bignum_O::popcount() const {
  Bignum x = this->_value;
  if (this->minusp_()) {
    // mpz_popcount is defined to return useless results on negative numbers,
    // so use (logcount x) = (logcount (lognot x))
    mpz_class temp;
    mpz_com(temp.get_mpz_t(), x.get_mpz_t());
    return mpz_popcount(temp.get_mpz_t());
  } else
    return mpz_popcount(x.get_mpz_t());
}

Rational_sp Bignum_O::ratdivide(Integer_sp divisor) const {
  return Rational_O::create(this->mpz(), clasp_to_mpz(divisor));
}

/*! Return the value shifted by BITS bits.
      If BITS < 0 shift right, if BITS >0 shift left. */
Integer_sp Bignum_O::shift_(gc::Fixnum bits) const {
  if (bits == 0)
    return this->asSmartPtr();
  Bignum res;
  if (bits < 0) {
    mpz_div_2exp(res.get_mpz_t(), this->_value.get_mpz_t(), -bits);
  } else {
    mpz_mul_2exp(res.get_mpz_t(), this->_value.get_mpz_t(), bits);
  }
  return Integer_O::create(res);
}

string Bignum_O::__repr__() const {
  stringstream ss;
  ss << this->_value;
  return ((ss.str()));
}

Number_sp Bignum_O::signum_() const {
  if (this->zerop_())
    return immediate_fixnum<Number_O>(0);
  else if (this->plusp_())
    return immediate_fixnum<Number_O>(1);
  else
    return immediate_fixnum<Number_O>(-1);
}

Number_sp Bignum_O::abs_() const {
  GC_ALLOCATE(Bignum_O, cp);
  cp->_value = this->_value * ::sgn(this->_value);
  return ((cp));
}

bool Bignum_O::eql_(T_sp o) const {
  if (o.fixnump()) {
    return (this->_value == clasp_to_mpz(gc::As<Fixnum_sp>(o)));
  } else if (Integer_sp oi = o.asOrNull<Integer_O>()) {
    return (this->_value == clasp_to_mpz(oi));
  }
  return false;
}

Integer_mv big_ceiling(Bignum_sp a, Bignum_sp b) {
  Bignum mpzq, mpzr;
  mpz_cdiv_qr(mpzq.get_mpz_t(),
              mpzr.get_mpz_t(),
              a->mpz().get_mpz_t(),
              b->mpz().get_mpz_t());
  return Values(Integer_O::create(mpzq), Integer_O::create(mpzr));
}

Integer_mv big_truncate(Bignum_sp a, Bignum_sp b) {
  Bignum mpzq, mpzr;
  mpz_tdiv_qr(mpzq.get_mpz_t(),
              mpzr.get_mpz_t(),
              a->mpz().get_mpz_t(),
              b->mpz().get_mpz_t());
  return Values(Integer_O::create(mpzq), Integer_O::create(mpzr));
}

Integer_mv big_floor(Bignum_sp a, Bignum_sp b) {
  Bignum_sp q = my_thread->bigRegister0();
  Bignum_sp r = my_thread->bigRegister1();
  mpz_fdiv_qr(q->mpz_ref().get_mpz_t(), r->mpz_ref().get_mpz_t(),
              a->mpz().get_mpz_t(), b->mpz().get_mpz_t());
  return Values(Integer_O::create(q->mpz()), Integer_O::create(r->mpz()));
}

Integer_sp _clasp_big_gcd(Bignum_sp x, Bignum_sp y) {
  Bignum zz;
  mpz_gcd(zz.get_mpz_t(), x->mpz().get_mpz_t(), y->mpz().get_mpz_t());
  return Bignum_O::create(zz);
}

Integer_sp bignum_divide(const Bignum &a, const Bignum &b) {
  size_t size_a = CLASP_BIGNUM_ABS_SIZE(a.get_mpz_t());
  size_t size_b = CLASP_BIGNUM_ABS_SIZE(b.get_mpz_t());
  Fixnum size_z = size_a - size_b + 1;
  if (size_z <= 0)
    size_z = 1;
  Bignum z;
  mpz_tdiv_q(z.get_mpz_t(), a.get_mpz_t(), b.get_mpz_t());
  return Integer_O::create(z);
}

Integer_sp _clasp_big_divided_by_big(const Bignum_sp a, const Bignum_sp b) {
  return bignum_divide(a->mpz_ref(), b->mpz_ref());
}

Integer_sp _clasp_big_divided_by_fix(const Bignum_sp x, const Fixnum y) {
  Bignum by(GMP_LONG(y));
  return bignum_divide(x->mpz_ref(), by);
}

Integer_sp _clasp_fix_divided_by_big(const Fixnum x, const Bignum_sp y) {
  Bignum bx(GMP_LONG(x));
  return bignum_divide(bx, y->mpz_ref());
}

void clasp_big_register_free(Bignum_sp b) {
  // ECL just returns but we
  // could clear out the bignum register if it's too big
  return;
}

CL_DEFUN TheNextBignum_sp core__next_from_fixnum(Fixnum fix) {
  return TheNextBignum_O::create((fix < 0) ? -1 : 1, std::abs(fix), true);
}

void TheNextBignum_O::sxhash_(HashGenerator &hg) const {
  mp_size_t len = this->length();
  if (!(hg.addValue(len))) return;
  mp_size_t size = std::abs(len);
  const mp_limb_t* limbs = this->limbs();
  for (mp_size_t i = 0; i < size; ++i)
    if (!(hg.addValue(limbs[i]))) return;
}

// Remove any high limbs that are equal to zero,
// starting from the right (most significant)
// Can result in zero limbs.
#define BIGNUM_NORMALIZE(NLIMBS, LIMBS)\
  while((NLIMBS) > 0) {\
    if ((LIMBS)[(NLIMBS) - 1] != 0) break;\
    (NLIMBS)--;\
  }

// Given bignum parts, return a fixnum if that fits, or else a bignum.
Integer_sp bignum_result(mp_size_t len, const mp_limb_t* limbs) {
  switch (len) {
  case 0: return clasp_make_fixnum(0);
  case 1:
      if (limbs[0] <= gc::most_positive_fixnum)
        return clasp_make_fixnum(limbs[0]);
      else break;
  case -1:
      if (-(limbs[0]) >= gc::most_negative_fixnum)
        return clasp_make_fixnum(-(limbs[0]));
      else break;
  }
  return TheNextBignum_O::create(len, 0, false,
                                 std::abs(len), limbs);
}

string TheNextBignum_O::__repr__() const {
  stringstream ss;
  const char* num_to_text = "0123456789abcdefghijklmnopqrstuvwxyz";
  mp_size_t len = this->length();
  const mp_limb_t *limbs = this->limbs();
  unsigned char raw[16*std::abs(len)+1];
  // mpn_get_str doesn't actually alter the limbs unless the base is not
  // a power of two, but C++ does not understand such subtleties.
  ss << "#<NEXT-BIGNUM length " << len << " ";
  for (size_t i = 0; i < std::abs(len); ++i)
    ss << limbs[i] << " ";
  ss << "#x";
  mp_size_t strlen = mpn_get_str(raw, 16, const_cast<mp_limb_t*>(limbs), len);
  for (size_t i = 0; i < strlen; ++i) ss << num_to_text[raw[i]];
  ss << ">";
  return ss.str();
}

Number_sp TheNextBignum_O::signum_() const {
  // There are no zero bignums, so this is easy.
  if (this->length() < 0)
    return clasp_make_fixnum(-1);
  else return clasp_make_fixnum(1);
}

Number_sp TheNextBignum_O::abs_() const {
  // NOTE: We assume that abs of a bignum is never a fixnum.
  // This will be true for two's complement systems.
  mp_size_t length = this->length();
  if (length < 0) // negative
    return TheNextBignum_O::create(-length, 0, false,
                                   -length, this->limbs());
  else return this->asSmartPtr();
}

bool TheNextBignum_O::eql_(T_sp obj) const {
  if (gc::IsA<TheNextBignum_sp>(obj)) {
    TheNextBignum_sp other = gc::As_unsafe<TheNextBignum_sp>(obj);
    mp_size_t len = this->length();
    if (len != other->length()) return false;
    // Maybe faster than a word-by-word comparison?
    return (mpn_cmp(this->limbs(), other->limbs(), std::abs(len)) == 0);
  }
  // Non bignums never eql bignums; in particular bignums in the
  // fixnum size range don't exist.
  else return false;
}

gc::Fixnum TheNextBignum_O::popcount() const {
  mp_size_t length = this->length();
  const mp_limb_t* limbs = this->limbs();
  if (length > 0)
    return mpn_popcount(limbs, length);
  else {
    // (Note that the size of a bignum is never zero - that's a fixnum.)
    // The signed magnitude representation used by mpn functions
    // makes this a little annoying.
    // (= (logcount x) (logcount (- (+ x 1)))) implies
    // (= (logcount (- y)) (logcount (- y 1))).
    mp_size_t size = -length;
    mp_limb_t sublimbs[size];
    mpn_sub_1(sublimbs, limbs, size, (mp_limb_t)1); // sublimbs = (-this)-1
    // The most significant limb may be zero, but that won't affect
    // the popcount obviously. Borrow is zero since -this > 0.
    return mpn_popcount(sublimbs, size);
  }
}

gc::Fixnum TheNextBignum_O::bit_length_() const {
  mp_size_t length = this->length();
  const mp_limb_t* limbs = this->limbs();
  if (length > 0)
    return mpn_sizeinbase(limbs, length, 2);
  else {
    // Since mpn_sizeinbase doesn't know about negative numbers,
    // we have to do as in the above, taking advantage of the
    // facts that (integer-length (lognot x)) = (integer-length x)
    // and (lognot x) = (- (+ x 1)).
    mp_size_t size = -length;
    mp_limb_t sublimbs[size];
    mpn_sub_1(sublimbs, limbs, size, (mp_limb_t)1);
    // mpn_sizeinbase does require the msl is not zero though.
    if (sublimbs[size-1] == 0) --size;
    return mpn_sizeinbase(sublimbs, size, 2);
  }
}

CL_DEFUN string core__next_primitive_string(TheNextBignum_sp num) {
  stringstream ss;
  mp_size_t len = num->length();
  const mp_limb_t *limbs = num->limbs();
  ss << "#<NEXT-BIGNUM";
  for (size_t i = 0; i < std::abs(len); ++i) ss << " " << limbs[i];
  ss << ">";
  return ss.str();
}

CL_DEFUN Integer_sp core__next_fmul(TheNextBignum_sp left, Fixnum right) {
  if (right == 0) return clasp_make_fixnum(0);
  mp_size_t llen = left->length();
  mp_size_t size = std::abs(llen);
  const mp_limb_t *llimbs = left->limbs();
  mp_size_t result_len;
  mp_limb_t result_limbs[size+1];
  mp_limb_t carry;
  // NOTE that std::abs will be undefined if the result isn't representable,
  // which will happen if right is INT_MIN or whatever. So this will break if
  // most_negative_fixnum is that negative (which it isn't hopefully).
  carry = mpn_mul_1(result_limbs, llimbs, size, std::abs(right));
  if (carry != 0) {
    result_limbs[size] = carry;
    ++size;
  }
  if ((llen < 0) ^ (right < 0))
    // Signs don't match, so we negate.
    result_len = -size;
  else
    result_len = size;
  // NOTE: This can only be a fixnum if right = -1 and left = m-p-fixnum + 1,
  // I think. Could be smarter maybe.
  return bignum_result(result_len, result_limbs);
}

CL_DEFUN TheNextBignum_sp core__next_lshift(TheNextBignum_sp num, Fixnum shift) {
  ASSERT(shift >= 0);
  mp_size_t len = num->length();
  size_t size = std::abs(len);
  const mp_limb_t* limbs = num->limbs();
  unsigned int nlimbs = shift / mp_bits_per_limb;
  unsigned int nbits = shift % mp_bits_per_limb;
  size_t result_size = size + nlimbs + 1;
  mp_limb_t result_limbs[result_size];
  mp_limb_t carry;
  // mpn shifts don't work when shift = 0, so we special case that.
  if (nbits == 0) {
    carry = 0;
    // do the "carry" ourselves by copying memory
    // FIXME: memcpy? std::copy?
    for (size_t i = 0; i < size; ++i) result_limbs[nlimbs+i] = limbs[i];
  }
  else carry = mpn_lshift(&(result_limbs[nlimbs]), limbs, size, nbits);
  if (carry == 0) --result_size;
  else result_limbs[result_size-1] = carry;
  for (size_t i = 0; i < nlimbs; ++i) result_limbs[i] = 0;
  // Since we start with a bignum, and we're making it bigger, we have a bignum.
  return TheNextBignum_O::create((len < 0) ? -result_size : result_size, 0, false,
                                 result_size, result_limbs);
}

CL_DEFUN Integer_sp core__next_rshift(TheNextBignum_sp num, Fixnum shift) {
  ASSERT(shift >= 0);
  mp_size_t len = num->length();
  size_t size = std::abs(len);
  const mp_limb_t* limbs = num->limbs();
  unsigned int nlimbs = shift / mp_bits_per_limb;
  unsigned int nbits = shift % mp_bits_per_limb;
  size_t result_size = size - nlimbs;
  mp_limb_t result_limbs[result_size];
  if (nbits == 0) {
    // FIXME: memcpy? std::copy?
    for (size_t i = 0; i < result_size; ++i) result_limbs[i] = limbs[nlimbs+i];
    // input bignum is normalized, so high limb is not zero
  } else {
    mpn_rshift(result_limbs, &(limbs[nlimbs]), size, nbits); // ignore outshifted bits
    if (result_limbs[result_size-1] == 0) --result_size;
  }
  return bignum_result((len < 0) ? -result_size : result_size, result_limbs);
}

CL_DEFUN TheNextBignum_sp core__next_mul(TheNextBignum_sp left, TheNextBignum_sp right) {
  // NOTE: The mpz_ functions detect when left = right (analogously) and use
  // mpn_sqr instead. I don't _think_ this is required, given they're untouched anyway.
  mp_size_t llen = left->length(), rlen = right->length();
  mp_size_t lsize = std::abs(llen), rsize = std::abs(rlen);
  const mp_limb_t *llimbs = left->limbs(), *rlimbs = right->limbs();
  mp_size_t result_size = lsize + rsize;
  mp_limb_t result_limbs[result_size];
  mp_limb_t msl;
  // "This function requires that s1n is greater than or equal to s2n."
  if (rsize > lsize)
    msl = mpn_mul(result_limbs, rlimbs, rsize, llimbs, lsize);
  else msl = mpn_mul(result_limbs, llimbs, lsize, rlimbs, rsize);
  if (msl == 0) --result_size;
  // Should always be a bignum.
  return TheNextBignum_O::create(((llen < 0) ^ (rlen < 0)) ? -result_size : result_size,
                                 0, false,
                                 result_size, result_limbs);
}

CL_DEFUN T_mv core__next_truncate(TheNextBignum_sp dividend,
                                  TheNextBignum_sp divisor) {
  ASSERT(dividend != divisor); // "No overlap is permitted between arguments"
  mp_size_t dividend_length = dividend->length();
  mp_size_t divisor_length = divisor->length();
  mp_size_t dividend_size = std::abs(dividend_length);
  mp_size_t divisor_size = std::abs(divisor_length);
  const mp_limb_t *dividend_limbs = dividend->limbs();
  const mp_limb_t *divisor_limbs = divisor->limbs();
  mp_size_t quotient_size = dividend_size - divisor_size + 1;
  mp_size_t remainder_size = divisor_size;
  mp_limb_t quotient_limbs[quotient_size];
  mp_limb_t remainder_limbs[remainder_size];
  mpn_tdiv_qr(quotient_limbs, remainder_limbs, 0L,
              dividend_limbs, dividend_size,
              divisor_limbs, divisor_size);
  // MSL of the quotient may be zero
  if (quotient_limbs[quotient_size-1] == 0) --quotient_size;
  // Remainder could be any size less than the divisor
  BIGNUM_NORMALIZE(remainder_size, remainder_limbs);
  // The quotient has the same sign as the mathematical quotient.
  Integer_sp quotient = bignum_result(((dividend_length < 0) ^ (divisor_length < 0))
                                      ? -quotient_size : quotient_size,
                                      quotient_limbs);
  // The remainder has the same sign as the dividend.
  Integer_sp remainder = bignum_result((dividend_length < 0)
                                       ? -remainder_size : remainder_size,
                                       remainder_limbs);
  return Values(quotient, remainder);
}

// Truncating a fixnum by a bignum will always get you zero
// so there's no function for that.
CL_DEFUN T_mv core__next_ftruncate(TheNextBignum_sp dividend,
                                   Fixnum divisor) {
  if (divisor == 0)
    ERROR_DIVISION_BY_ZERO(dividend, clasp_make_fixnum(divisor));
  mp_limb_t positive_divisor = std::abs(divisor);
  mp_size_t len = dividend->length();
  mp_size_t size = std::abs(len);
  const mp_limb_t* limbs = dividend->limbs();
  mp_size_t quotient_size = size;
  mp_limb_t quotient_limbs[quotient_size];
  // GMP docs don't mark the third argument const but it seems to be.
  mp_limb_t remainder = mpn_divrem_1(quotient_limbs, (mp_size_t)0,
                                     limbs, size, positive_divisor);
  BIGNUM_NORMALIZE(quotient_size, quotient_limbs);
  return Values(bignum_result(((len < 0) ^ (divisor < 0))
                              ? -quotient_size : quotient_size,
                              quotient_limbs),
                // the remainder is at most the divisor,
                // but with abs(m-n-fixnum) != m-p-fixnum I wanna be careful.
                bignum_result((len < 0) ? -1 : 1, &remainder));
}

Integer_sp fix_divided_by_next(Fixnum dividend, TheNextBignum_sp divisor) {
  // Assuming two's complement representation, the magnitude of a fixnum
  // is always less than that of a bignum, except for one case:
  // when the fixnum is most-negative-fixnum and the bignum is
  // -most-negative-fixnum.
  if ((dividend == gc::most_negative_fixnum)
      && (divisor->length() == 1)
      && ((divisor->limbs())[0] == -gc::most_negative_fixnum))
    return clasp_make_fixnum(-1);
  else return clasp_make_fixnum(0);
}

Integer_sp next_gcd(const mp_limb_t* llimbs, mp_size_t lsize,
                    const mp_limb_t* rlimbs, mp_size_t rsize) {
  // This one is rather nontrivial due to two properties of mpn_gcd:
  // 1) Both source operands are destroyed.
  //    So we have to copy the operands, as Lisp bignums are immutable.
  // 2) At least one operand must be odd.
  //    So we shift out any low zero bits,
  //    using the properties that gcd(x*2^n, y*2^n) = gcd(x,y)*2^n
  //    and gcd(2u,v) = gcd(u,2v) = gcd(u,v) for odd u and v.
  // Also note that we can ignore sign due to Lisp's always-positive
  // GCD result.

  // Shift zeroes out of the left.
  // (Also, remember that bignums are never zero, so this terminates fine.)
  mp_size_t n_left_zero_limbs = 0, n_left_zero_bits;
  while (llimbs[0] == 0) {
    ++n_left_zero_limbs; --lsize;
    ++llimbs; // llimbs = &(llimbs[1]); essentially
  }
  // NOTE: We assume mp_limb_t = long long. Unfortunate.
  n_left_zero_bits = __builtin_ctzll(llimbs[0]);
  mp_limb_t llimbs_copy[lsize];
  if (n_left_zero_bits == 0) {
    // FIXME: Could memcpy/whatever.
    for (mp_size_t i = 0; i < lsize; ++i) llimbs_copy[i] = llimbs[i];
  } else {
    mpn_rshift(llimbs_copy, llimbs, lsize, n_left_zero_bits);
    if (llimbs_copy[lsize-1] == 0) --lsize;
  }

  // Ditto for the right.
  mp_size_t n_right_zero_limbs = 0, n_right_zero_bits;
  while (rlimbs[0] == 0) {
    ++n_right_zero_limbs; --rsize;
    ++rlimbs;
  }
  n_right_zero_bits = __builtin_ctzll(rlimbs[0]);
  mp_limb_t rlimbs_copy[rsize];
  if (n_right_zero_bits == 0) {
    for (mp_size_t i = 0; i < rsize; ++i) rlimbs_copy[i] = rlimbs[i];
  } else {
    mpn_rshift(rlimbs_copy, rlimbs, rsize, n_right_zero_bits);
    if (rlimbs_copy[rsize-1] == 0) --rsize;
  }

  // Now figure out how many zero bits in the result (the min of both)
  mp_size_t n_result_zero_limbs, n_result_zero_bits;
  if (n_left_zero_limbs > n_right_zero_limbs) {
    n_result_zero_limbs = n_right_zero_limbs;
    n_result_zero_bits = n_right_zero_bits;
  } else if (n_left_zero_limbs < n_right_zero_limbs) {
    n_result_zero_limbs = n_left_zero_limbs;
    n_result_zero_bits = n_right_zero_bits;
  } else { // equal
    n_result_zero_limbs = n_left_zero_limbs;
    n_result_zero_bits = std::min(n_left_zero_bits, n_right_zero_bits);
  }

  // Now we are ready to do the actual GCD.
  // The left must have at least as many limbs as the right.
  // Though not in the documentation, the mpz_gcd code suggests
  // that the left must be actually bigger than the right, so we
  // check the most significant limb as well.
  // Also, we reuse one of the operands as the destination, which
  // is apparently allowed.
  mp_size_t gcd_size;
  mp_limb_t* gcd_limbs = rlimbs_copy;
  if ((lsize < rsize)
      || ((lsize == rsize)
          && (llimbs_copy[lsize-1] < rlimbs_copy[rsize-1])))
    gcd_size = mpn_gcd(gcd_limbs, rlimbs_copy, rsize, llimbs_copy, lsize);
  else
    gcd_size = mpn_gcd(gcd_limbs, llimbs_copy, lsize, rlimbs_copy, rsize);

  // Now we need to shift the shared bits back in to the result
  // and otherwise construct it.
  mp_limb_t result_size = gcd_size + n_result_zero_limbs;
  mp_limb_t result_limbs[result_size+1]; // +1 for space to shift into.
  for (mp_size_t i = 0; i < n_result_zero_limbs; ++i)
    result_limbs[i] = 0;
  if (n_result_zero_bits == 0) {
    // FIXME blabla memcpy
    for (mp_size_t i = 0; i < gcd_size; ++i)
      result_limbs[i+n_result_zero_limbs] = gcd_limbs[i];
  } else {
    mp_limb_t carry;
    carry = mpn_lshift(&(result_limbs[n_result_zero_limbs]),
                       gcd_limbs, gcd_size, n_result_zero_bits);
    if (carry != 0) {
      result_limbs[result_size] = carry;
      ++result_size;
    }
  }

  // Finally, construct the result.
  return bignum_result(result_size, result_limbs);
}

CL_DEFUN Integer_sp core__next_gcd(TheNextBignum_sp left, TheNextBignum_sp right) {
  return next_gcd(left->limbs(), std::abs(left->length()),
                  right->limbs(), std::abs(right->length()));
}

CL_DEFUN Integer_sp core__next_fgcd(TheNextBignum_sp big, Fixnum small) {
  if (small == 0) return big;
  // Don't think mpn_gcd_1 understands negatives.
  if (small < 0) small = -small;
  return clasp_make_fixnum(mpn_gcd_1(big->limbs(), std::abs(big->length()),
                                     small));
}

Rational_sp TheNextBignum_O::ratdivide(Integer_sp divisor) const {
  // FIXME?: Some of the bignum_results in here might always
  // end up with bignums and so could be direct create calls instead.
  mp_size_t len = this->length();
  mp_size_t size = std::abs(len);
  const mp_limb_t* limbs = this->limbs();
  
  if (divisor.fixnump()) {
    Fixnum fdivisor = divisor.unsafe_fixnum();
    if (fdivisor == 0) ERROR_DIVISION_BY_ZERO(this->asSmartPtr(), divisor);
    Fixnum result_sign = ((len < 0) ^ (fdivisor < 0)) ? -1 : 1;
    Fixnum adivisor = std::abs(fdivisor);
    mp_limb_t gcd = mpn_gcd_1(limbs, size, adivisor);
    if (gcd == adivisor) { // exact division
      if (result_sign == 1)
        return this->asSmartPtr();
      else return bignum_result(-len, limbs);
    } else { // need a ratio
      Fixnum adenom = adivisor/gcd;
      mp_limb_t num[size];
      mpn_divexact_1(num, limbs, size, gcd);
      // quick normalize
      mp_limb_t num_length;
      if (num[size-1] == 0)
        num_length = result_sign * (size-1);
      else num_length = result_sign * size;
      return Ratio_O::create(bignum_result(num_length, num),
                             clasp_make_fixnum(adenom));
    }
  } else {
    // divisor is a bignum.
    // TODO: Switch to As_unsafe once old bignums are gone.
    TheNextBignum_sp bdivisor = gc::As<TheNextBignum_sp>(divisor);
    mp_size_t divlen = bdivisor->length();
    mp_size_t divsize = std::abs(divlen);
    const mp_limb_t* divlimbs = bdivisor->limbs();
    Fixnum result_sign = ((len < 0) ^ (divlen < 0)) ? -1 : 1;

    // FIXME?: We could save some memory by not consing up an
    // actual bignum for the gcd and just working with limbs
    // stored on the stack.
    Integer_sp gcd = next_gcd(limbs, size, divlimbs, divsize);

    if (gcd.fixnump()) {
      Fixnum fgcd = gcd.unsafe_fixnum();

      // Pick off the specialest case
      if (fgcd == 1) {
        Integer_sp num;
        if (divlen < 0) num = gc::As_unsafe<Integer_sp>(this->negate_());
        else num = this->asSmartPtr();
        return Ratio_O::create(num, divisor);
      }

      // Nope, have to do some divisions.
      // Compute the numerator
      mp_limb_t numsize = size;
      mp_limb_t numlimbs[numsize];
      mpn_divexact_1(numlimbs, limbs, size, fgcd);
      if (numlimbs[numsize-1] == 0) --numsize;
      Integer_sp numerator = bignum_result(result_sign * numsize,
                                           numlimbs);

      // Compute the denominator
      mp_limb_t densize = divsize;
      mp_limb_t denlimbs[densize];
      mpn_divexact_1(denlimbs, divlimbs, divsize, fgcd);
      if (denlimbs[densize-1] == 0) --densize;
      // If the denominator is 1, return the numerator
      if ((densize == 1) && (denlimbs[0] == 1)) return numerator;
      // Nope, making a fraction.
      // Denominator is always positive.
      Integer_sp denominator = bignum_result(densize, denlimbs);

      return Ratio_O::create(numerator, denominator);
    } else {
      // GCD is a bignum.
      // NOTE: If MPN had a divexact we could use it,
      // but it doesn't seem to, so we just use tdiv_qr
      // and ignore the remainder.
      TheNextBignum_sp bgcd = gc::As_unsafe<TheNextBignum_sp>(gcd);
      mp_size_t gcd_size = bgcd->length(); // necessarily positive
      const mp_limb_t* gcd_limbs = bgcd->limbs();
      
      mp_limb_t remainder_limbs[gcd_size];

      // Compute the numerator
      mp_size_t numsize = size - gcd_size + 1;
      mp_limb_t numlimbs[numsize];
      mpn_tdiv_qr(numlimbs, remainder_limbs, (mp_size_t)0,
                  limbs, size, gcd_limbs, gcd_size);
      // Numerator MSL may be zero
      if (numlimbs[numsize-1] == 0) --numsize;
      Integer_sp numerator = bignum_result(result_sign * numsize, numlimbs);

      // Compute the denominator
      mp_size_t densize = divsize - gcd_size + 1;
      mp_limb_t denlimbs[densize];
      mpn_tdiv_qr(denlimbs, remainder_limbs, (mp_size_t)0,
                  divlimbs, divsize, gcd_limbs, gcd_size);
      // Denominator MSL may be zero
      if (denlimbs[densize-1] == 0) --densize;

      // If the denominator is one, just return the numerator
      if ((densize == 1) && (denlimbs[0] == 1))
        return numerator;

      // Nope, make a fraction
      // Denominator still always positive
      Integer_sp denominator = bignum_result(densize, denlimbs);
      return Ratio_O::create(numerator, denominator);
    }
  }
}

CL_DEFUN Integer_sp core__next_add(TheNextBignum_sp left, TheNextBignum_sp right) {
  mp_size_t llen = left->length(), rlen = right->length();
  mp_size_t absllen = std::abs(llen), absrlen = std::abs(rlen);
  const mp_limb_t *llimbs = left->limbs(), *rlimbs = right->limbs();
  
  // Keep the larger number in the left.
  if (absllen < absrlen) {
    std::swap(llen, rlen);
    std::swap(absllen, absrlen);
    std::swap(llimbs, rlimbs);
  }

  mp_size_t result_len;
  mp_limb_t result_limbs[1+absllen];
  
  if ((llen ^ rlen) < 0) {
    // the lengths (and therefore the numbers) have different sign.
    result_len = absllen;
    if (absllen != absrlen) {
      // left is larger, so we fear no carry.
      mpn_sub(result_limbs, llimbs, absllen, rlimbs, absrlen);
      BIGNUM_NORMALIZE(result_len, result_limbs);
      if (llen < 0) result_len = -result_len;
    } // They have the same size, so we have to do a real comparison.
    else if (mpn_cmp(llimbs, rlimbs, absllen) < 0) {
      // left has less magnitude, so it's the subtrahend
      mpn_sub_n(result_limbs, rlimbs, llimbs, absllen);
      BIGNUM_NORMALIZE(result_len, result_limbs);
      if (rlen < 0) result_len = -result_len;
    } else {
      // right has less magnitude
      mpn_sub_n(result_limbs, llimbs, rlimbs, absllen);
      BIGNUM_NORMALIZE(result_len, result_limbs);
      if (llen < 0) result_len = -result_len;
    }
  } else {
    // The numbers have the same sign, so just add them
    mp_limb_t carry = mpn_add(result_limbs, llimbs, absllen, rlimbs, absrlen);
    result_limbs[absllen] = carry;
    result_len = absllen + carry; // carry is either 0 or 1
    if (llen < 0) result_len = -result_len;
  }
  // NOTE: If the signs match we definitely have a bignum, so hypothetically
  // the tests in bignum_result could be skipped.
  return bignum_result(result_len, result_limbs);
}

CL_DEFUN Integer_sp core__next_fadd(TheNextBignum_sp left, Fixnum right) {
  // Easier than above since we know abs(right) < abs(left) and the result size.
  mp_size_t len = left->length();
  mp_size_t size = std::abs(len);
  const mp_limb_t* limbs = left->limbs();

  mp_limb_t result_len = size;
  mp_limb_t result_limbs[size+1];

  if ((len < 0) ^ (right < 0)) {
    // Different signs - subtract
    mpn_sub_1(result_limbs, limbs, size, right);
    // Quick normalize
    if (result_limbs[size-1] == 0) --result_len;
  } else {
    // signs match
    mp_limb_t carry = mpn_add_1(result_limbs, limbs, size, right);
    if (carry != 0) {
      ++result_len;
      result_limbs[size] = carry;
    }
  }
  // Result has the same sign as the bigger number (so, the bignum)
  if (len < 0) result_len = -result_len;
  return bignum_result(result_len, result_limbs);
}

double next_to_double(mp_size_t len, const mp_limb_t* limbs) {
  // MPN does not seem to export a float conversion function,
  // so we roll our own. FIXME: Could get bad with float rounding
  // or otherwise as I'm making this up as I go.
  mp_size_t size = std::abs(len);

  // There are no length zero bignums.
  // If the bignum is only one long, we just convert directly.
  if (size == 1)
    return static_cast<double>(limbs[0]);

  // Otherwise, we just use the most significant two limbs.
  // Assuming the float format has at most 128 bits of significand,
  // the lower limbs ought to be irrelevant.
  // (In fact one 64-bit number would probably be enough,
  //  but it's possible the high bits of this number are all zero.)
  double ultimate = static_cast<double>(limbs[size-1]);
  double penultimate = static_cast<double>(limbs[size-2]);
  // Compiler is smart enough to make exp2(constant) constant, hopefully.
  // In C++17 we could use a hexadecimal float literal
  // without any loss of precision.
  double soon = penultimate + ultimate * std::exp2(64);
  return soon * std::exp2(64*(size-2)) * ((len < 0) ? -1e0 : 1e0);
}

float TheNextBignum_O::as_float_() const {
  return static_cast<float>(next_to_double(this->length(), this->limbs()));
}

double TheNextBignum_O::as_double_() const {
  return next_to_double(this->length(), this->limbs());
}

LongFloat TheNextBignum_O::as_long_float_() const {
  return static_cast<LongFloat>(next_to_double(this->length(), this->limbs()));
}

CL_DEFUN int core__next_compare(TheNextBignum_sp left,
                                TheNextBignum_sp right) {
  mp_size_t llen = left->length(), rlen = right->length();
  const mp_limb_t *llimbs = left->limbs(), *rlimbs = right->limbs();

  if (llen < rlen) return -1;
  else if (llen > rlen) return 1;
  else { // actual comparison
    // mpn_cmp is only defined to return "a negative value" etc.,
    // so we normalize it to -1, 0, or 1.
    int cmp = mpn_cmp(llimbs, rlimbs, std::abs(llen));
    if (cmp < 0) {
      if (llen < 0) return 1; else return -1;
    } else if (cmp > 0) {
      if (llen < 0) return -1; else return 1;
    } else return 0;
  }
}

}; // namespace core
