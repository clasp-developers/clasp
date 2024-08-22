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
// #define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/numbers.h>
#include <clasp/core/symbol.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/bignum.h>
#include <clasp/core/wrappers.h>

namespace core {

DOCGROUP(clasp);
CL_DEFUN Bignum_sp core__next_from_fixnum(Fixnum fix) { return Bignum_O::create(fix); }

Bignum_sp Bignum_O::create(const mpz_class& c) {
  const mp_size_t limbsize = sizeof(mp_limb_t);
  const size_t nails = GMP_NAIL_BITS;
  // copied from gmp docs
  size_t numb = 8 * limbsize - nails;
  size_t count = (mpz_sizeinbase(c.get_mpz_t(), 2) + numb - 1) / numb;
  mp_limb_t dest[count];
  // These parameters are chosen in order to hopefully
  // just let GMP copy its limbs directly.
  mpz_export(dest, &count, -1, limbsize, 0, nails, c.get_mpz_t());
  mp_size_t len = count * mpz_sgn(c.get_mpz_t());
  return create_from_limbs(len, 0, false, count, dest);
}

void Bignum_O::sxhash_(HashGenerator& hg) const {
  mp_size_t len = this->length();
  if (!(hg.addValue(len)))
    return;
  mp_size_t size = std::abs(len);
  const mp_limb_t* limbs = this->limbs();
  for (mp_size_t i = 0; i < size; ++i)
    if (!(hg.addValue(limbs[i])))
      return;
}

// Given bignum parts, return a fixnum if that fits, or else a bignum.
Integer_sp bignum_result(mp_size_t len, const mp_limb_t* limbs) {
  switch (len) {
  case 0:
    return clasp_make_fixnum(0);
  case 1:
    if (limbs[0] <= gc::most_positive_fixnum)
      return clasp_make_fixnum(limbs[0]);
    else
      break;
  case -1:
    if (-(limbs[0]) >= gc::most_negative_fixnum)
      return clasp_make_fixnum(-(limbs[0]));
    else
      break;
  }
  return Bignum_O::create_from_limbs(len, 0, false, std::abs(len), limbs);
}

mpz_class Bignum_O::mpz() const {
  mp_size_t len = this->length();
  mpz_class m;
  mpz_import(m.get_mpz_t(), std::abs(len), -1, sizeof(mp_limb_t), 0, GMP_NAIL_BITS, this->limbs());
  if (len < 0) {
    mpz_class r;
    mpz_neg(r.get_mpz_t(), m.get_mpz_t());
    return r;
  } else
    return m;
}

string Bignum_O::__repr__() const {
  stringstream ss;
  mp_size_t len = this->length();
  mp_size_t size = std::abs(len);
  const mp_limb_t* limbs = this->limbs();
  mp_limb_t copylimbs[size];
  for (mp_size_t i = 0; i < size; ++i)
    copylimbs[i] = limbs[i];
  size_t prestrsize = mpn_sizeinbase(limbs, size, 10);
  unsigned char raw[prestrsize + 1];
  mp_size_t strsize = mpn_get_str(raw, 10, copylimbs, size);
  // Now write
  if (len < 0)
    ss << '-';
  for (size_t i = 0; i < strsize; ++i)
    ss << (unsigned char)(raw[i] + '0');
  return ss.str();
}

Bignum_sp Bignum_O::make(const string& str) {
  const char* cstr = str.c_str();
  size_t strsize = str.size();
  bool negative = false;
  if (str[0] == '-') {
    negative = true;
    --strsize;
    cstr = &(cstr[1]);
  }
  unsigned char s[strsize];
  for (size_t i = 0; i < strsize; ++i)
    s[i] = cstr[i] - '0';
  // Number of GMP limbs per decimal digit, approximately.
  // i.e. (/ (log 10 2) (log (expt 2 bits-per-limb) 2))
  double convert = log2(10) / mp_bits_per_limb;
  mp_size_t nlimbs = std::ceil(strsize * convert) + 2;
  mp_limb_t limbs[nlimbs];
  nlimbs = mpn_set_str(limbs, s, strsize, 10);
  return Bignum_O::create_from_limbs(negative ? -nlimbs : nlimbs, 0, false, nlimbs, limbs);
}

DOCGROUP(clasp);
CL_DEFUN Bignum_sp core__next_from_string(const string& str) { return Bignum_O::make(str); }

Number_sp Bignum_O::signum_() const {
  // There are no zero bignums, so this is easy.
  if (this->length() < 0)
    return clasp_make_fixnum(-1);
  else
    return clasp_make_fixnum(1);
}

Number_sp Bignum_O::abs_() const {
  // NOTE: We assume that abs of a bignum is never a fixnum.
  // This will be true for two's complement systems.
  mp_size_t length = this->length();
  if (length < 0) // negative
    return Bignum_O::create_from_limbs(-length, 0, false, -length, this->limbs());
  else
    return this->asSmartPtr();
}

Number_sp Bignum_O::negate_() const {
  mp_size_t len = this->length();
  const mp_limb_t* limbs = this->limbs();
  // This can be a fixnum, if we are -most_negative_fixnum.
  if ((len == 1) && (limbs[0] == -gc::most_negative_fixnum))
    return clasp_make_fixnum(gc::most_negative_fixnum);
  else
    return Bignum_O::create_from_limbs(-len, 0, false, std::abs(len), limbs);
}

bool Bignum_O::eql_(T_sp obj) const {
  if (gc::IsA<Bignum_sp>(obj)) {
    Bignum_sp other = gc::As_unsafe<Bignum_sp>(obj);
    mp_size_t len = this->length();
    if (len != other->length())
      return false;
    // Maybe faster than a word-by-word comparison?
    return (mpn_cmp(this->limbs(), other->limbs(), std::abs(len)) == 0);
  }
  // Non bignums never eql bignums; in particular bignums in the
  // fixnum size range don't exist.
  else
    return false;
}

gc::Fixnum Bignum_O::popcount() const {
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

gc::Fixnum Bignum_O::bit_length_() const {
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
    if (sublimbs[size - 1] == 0)
      --size;
    return mpn_sizeinbase(sublimbs, size, 2);
  }
}

DOCGROUP(clasp);
CL_DEFUN string core__next_primitive_string(Bignum_sp num) {
  stringstream ss;
  mp_size_t len = num->length();
  const mp_limb_t* limbs = num->limbs();
  ss << "#<NEXT-BIGNUM (size " << len << ")";
  for (size_t i = 0; i < std::abs(len); ++i)
    ss << " " << limbs[i];
  ss << ">";
  return ss.str();
}

DOCGROUP(clasp);
CL_DEFUN Integer_sp core__next_fmul(Bignum_sp left, Fixnum right) {
  if (right == 0)
    return clasp_make_fixnum(0);
  mp_size_t llen = left->length();
  mp_size_t size = std::abs(llen);
  const mp_limb_t* llimbs = left->limbs();
  mp_size_t result_len;
  mp_limb_t result_limbs[size + 1];
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

DOCGROUP(clasp);
CL_DEFUN Bignum_sp core__next_lshift(Bignum_sp num, Fixnum shift) {
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
    for (size_t i = 0; i < size; ++i)
      result_limbs[nlimbs + i] = limbs[i];
  } else
    carry = mpn_lshift(&(result_limbs[nlimbs]), limbs, size, nbits);
  if (carry == 0)
    --result_size;
  else
    result_limbs[result_size - 1] = carry;
  for (size_t i = 0; i < nlimbs; ++i)
    result_limbs[i] = 0;
  // Since we start with a bignum, and we're making it bigger, we have a bignum.
  return Bignum_O::create_from_limbs((len < 0) ? -result_size : result_size, 0, false, result_size, result_limbs);
}

DOCGROUP(clasp);
CL_DEFUN Integer_sp core__next_rshift(Bignum_sp num, Fixnum shift) {
  ASSERT(shift >= 0);
  mp_size_t len = num->length();
  size_t size = std::abs(len);
  const mp_limb_t* limbs = num->limbs();
  unsigned int nlimbs = shift / mp_bits_per_limb;
  unsigned int nbits = shift % mp_bits_per_limb;
  if (len < 0) {
    // -(-x >> a) = -(~(x-1) >> a) = ~(~(x-1) >> a) + 1 = ((x-1) >> a) + 1 i think.
    if (nlimbs >= size)
      return clasp_make_fixnum(-1);
    mp_size_t result_size = size - nlimbs;
    mp_limb_t result_limbs[result_size];
    mp_limb_t copy[size];
    mpn_sub_1(copy, limbs, size, 1);
    if (nbits == 0) {
      // FIXME: memcpy? std::copy?
      for (size_t i = 0; i < result_size; ++i)
        result_limbs[i] = copy[nlimbs + i];
      mpn_add_1(result_limbs, result_limbs, result_size, 1);
    } else {
      mpn_rshift(result_limbs, &(copy[nlimbs]), result_size, nbits);
      // Since we just shifted, I think there will be no carry.
      mpn_add_1(result_limbs, result_limbs, result_size, 1);
      if (result_limbs[result_size - 1] == 0)
        --result_size;
    }
    return bignum_result(-result_size, result_limbs);
  } else {
    if (nlimbs >= size)
      return clasp_make_fixnum(0);
    size_t result_size = size - nlimbs;
    mp_limb_t result_limbs[result_size];
    if (nbits == 0) {
      // FIXME: memcpy? std::copy?
      for (size_t i = 0; i < result_size; ++i)
        result_limbs[i] = limbs[nlimbs + i];
      // input bignum is normalized, so high limb is not zero
    } else {
      // we don't need outshifted bits, so we ignore mpn_rshift's return value
      mpn_rshift(result_limbs, &(limbs[nlimbs]), result_size, nbits);
      if (result_limbs[result_size - 1] == 0)
        --result_size;
    }
    return bignum_result(result_size, result_limbs);
  }
}

Integer_sp Bignum_O::shift_left(Fixnum shift) const {
  if (shift > 0)
    return core__next_lshift(this->asSmartPtr(), shift);
  else
    return this->asSmartPtr();
}
Integer_sp Bignum_O::shift_right(Fixnum shift) const {
  if (shift > 0)
    return core__next_rshift(this->asSmartPtr(), shift);
  else
    return this->asSmartPtr();
}

DOCGROUP(clasp);
CL_DEFUN Bignum_sp core__next_mul(Bignum_sp left, Bignum_sp right) {
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
  else
    msl = mpn_mul(result_limbs, llimbs, lsize, rlimbs, rsize);
  if (msl == 0)
    --result_size;
  // Should always be a bignum.
  return Bignum_O::create_from_limbs(((llen < 0) ^ (rlen < 0)) ? -result_size : result_size, 0, false, result_size, result_limbs);
}

DOCGROUP(clasp);
CL_DEFUN Bignum_sp core__mul_fixnums(Fixnum left, Fixnum right) {
  mp_size_t result_size = 2;
  mp_limb_t limbs[2];
  mp_size_t llen;
  if (left < 0) {
    llen = -1;
    limbs[0] = -left;
  } else {
    llen = 1;
    limbs[0] = left;
  }
  // Reusing the storage here is ok, according to docs,
  // provided that result_limbs <= input_limbs (as here).
  mp_limb_t msl = mpn_mul_1(limbs, limbs, 1, std::abs(right));
  if (msl == 0)
    --result_size;
  else
    limbs[1] = msl;
  // We unconditionally return a bignum because this is only called
  // from contagion_mul in the case that multiplication is known to
  // overflow.
  // TODO: Because of that, msl might always be nonzero? Double check,
  // we could save a branch.
  return Bignum_O::create_from_limbs(((llen < 0) ^ (right < 0)) ? -result_size : result_size, 0, false, result_size, limbs);
}

DOCGROUP(clasp);
CL_DEFUN T_mv core__next_truncate(Bignum_sp dividend, Bignum_sp divisor) {
  ASSERT(dividend != divisor); // "No overlap is permitted between arguments"
  mp_size_t dividend_length = dividend->length();
  mp_size_t divisor_length = divisor->length();
  mp_size_t dividend_size = std::abs(dividend_length);
  mp_size_t divisor_size = std::abs(divisor_length);
  if (divisor_size > dividend_size)
    return Values(clasp_make_fixnum(0), dividend);
  const mp_limb_t* dividend_limbs = dividend->limbs();
  const mp_limb_t* divisor_limbs = divisor->limbs();
  mp_size_t quotient_size = dividend_size - divisor_size + 1;
  mp_size_t remainder_size = divisor_size;
  mp_limb_t quotient_limbs[quotient_size];
  mp_limb_t remainder_limbs[remainder_size];
  mpn_tdiv_qr(quotient_limbs, remainder_limbs, 0L, dividend_limbs, dividend_size, divisor_limbs, divisor_size);
  // MSL of the quotient may be zero
  if (quotient_limbs[quotient_size - 1] == 0)
    --quotient_size;
  // Remainder could be any size less than the divisor
  BIGNUM_NORMALIZE(remainder_size, remainder_limbs);
  // The quotient has the same sign as the mathematical quotient.
  Integer_sp quotient =
      bignum_result(((dividend_length < 0) ^ (divisor_length < 0)) ? -quotient_size : quotient_size, quotient_limbs);
  // The remainder has the same sign as the dividend.
  Integer_sp remainder = bignum_result((dividend_length < 0) ? -remainder_size : remainder_size, remainder_limbs);
  return Values(quotient, remainder);
}

// Truncating a fixnum by a bignum will always get you zero
// so there's no function for that.
DOCGROUP(clasp);
CL_DEFUN T_mv core__next_ftruncate(Bignum_sp dividend, Fixnum divisor) {
  if (divisor == 0)
    ERROR_DIVISION_BY_ZERO(dividend, clasp_make_fixnum(divisor));
  mp_limb_t positive_divisor = std::abs(divisor);
  mp_size_t len = dividend->length();
  mp_size_t size = std::abs(len);
  const mp_limb_t* limbs = dividend->limbs();
  mp_size_t quotient_size = size;
  mp_limb_t quotient_limbs[quotient_size];
  // GMP docs don't mark the third argument const but it seems to be.
  mp_limb_t remainder = mpn_divrem_1(quotient_limbs, (mp_size_t)0, limbs, size, positive_divisor);
  BIGNUM_NORMALIZE(quotient_size, quotient_limbs);
  return Values(bignum_result(((len < 0) ^ (divisor < 0)) ? -quotient_size : quotient_size, quotient_limbs),
                // the remainder is at most the divisor,
                // but with abs(m-n-fixnum) != m-p-fixnum I wanna be careful.
                bignum_result((remainder == 0 || len > 0) ? 1 : -1, &remainder));
}

Integer_sp fix_divided_by_next(Fixnum dividend, Bignum_sp divisor) {
  // Assuming two's complement representation, the magnitude of a fixnum
  // is always less than that of a bignum, except for one case:
  // when the fixnum is most-negative-fixnum and the bignum is
  // -most-negative-fixnum.
  if ((dividend == gc::most_negative_fixnum) && (divisor->length() == 1) && ((divisor->limbs())[0] == -gc::most_negative_fixnum))
    return clasp_make_fixnum(-1);
  else
    return clasp_make_fixnum(0);
}

Integer_sp next_gcd(const mp_limb_t* llimbs, mp_size_t lsize, const mp_limb_t* rlimbs, mp_size_t rsize) {
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
    ++n_left_zero_limbs;
    --lsize;
    ++llimbs; // llimbs = &(llimbs[1]); essentially
  }
  // NOTE: We assume mp_limb_t = long long. Unfortunate.
  n_left_zero_bits = __builtin_ctzll(llimbs[0]);
  mp_limb_t llimbs_copy[lsize];
  if (n_left_zero_bits == 0) {
    // FIXME: Could memcpy/whatever.
    for (mp_size_t i = 0; i < lsize; ++i)
      llimbs_copy[i] = llimbs[i];
  } else {
    mpn_rshift(llimbs_copy, llimbs, lsize, n_left_zero_bits);
    if (llimbs_copy[lsize - 1] == 0)
      --lsize;
  }

  // Ditto for the right.
  mp_size_t n_right_zero_limbs = 0, n_right_zero_bits;
  while (rlimbs[0] == 0) {
    ++n_right_zero_limbs;
    --rsize;
    ++rlimbs;
  }
  n_right_zero_bits = __builtin_ctzll(rlimbs[0]);
  mp_limb_t rlimbs_copy[rsize];
  if (n_right_zero_bits == 0) {
    for (mp_size_t i = 0; i < rsize; ++i)
      rlimbs_copy[i] = rlimbs[i];
  } else {
    mpn_rshift(rlimbs_copy, rlimbs, rsize, n_right_zero_bits);
    if (rlimbs_copy[rsize - 1] == 0)
      --rsize;
  }

  // Now figure out how many zero bits in the result (the min of both)
  mp_size_t n_result_zero_limbs, n_result_zero_bits;
  if (n_left_zero_limbs > n_right_zero_limbs) {
    n_result_zero_limbs = n_right_zero_limbs;
    n_result_zero_bits = n_right_zero_bits;
  } else if (n_left_zero_limbs < n_right_zero_limbs) {
    n_result_zero_limbs = n_left_zero_limbs;
    n_result_zero_bits = n_left_zero_bits;
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
  if ((lsize < rsize) || ((lsize == rsize) && (llimbs_copy[lsize - 1] < rlimbs_copy[rsize - 1])))
    gcd_size = mpn_gcd(gcd_limbs, rlimbs_copy, rsize, llimbs_copy, lsize);
  else
    gcd_size = mpn_gcd(gcd_limbs, llimbs_copy, lsize, rlimbs_copy, rsize);

  // Now we need to shift the shared bits back in to the result
  // and otherwise construct it.
  mp_limb_t result_size = gcd_size + n_result_zero_limbs;
  mp_limb_t result_limbs[result_size + 1]; // +1 for space to shift into.
  for (mp_size_t i = 0; i < n_result_zero_limbs; ++i)
    result_limbs[i] = 0;
  if (n_result_zero_bits == 0) {
    // FIXME blabla memcpy
    for (mp_size_t i = 0; i < gcd_size; ++i)
      result_limbs[i + n_result_zero_limbs] = gcd_limbs[i];
  } else {
    mp_limb_t carry;
    carry = mpn_lshift(&(result_limbs[n_result_zero_limbs]), gcd_limbs, gcd_size, n_result_zero_bits);
    if (carry != 0) {
      result_limbs[result_size] = carry;
      ++result_size;
    }
  }

  // Finally, construct the result.
  return bignum_result(result_size, result_limbs);
}

DOCGROUP(clasp);
CL_DEFUN Integer_sp core__next_gcd(Bignum_sp left, Bignum_sp right) {
  return next_gcd(left->limbs(), std::abs(left->length()), right->limbs(), std::abs(right->length()));
}

DOCGROUP(clasp);
CL_DEFUN Integer_sp core__next_fgcd(Bignum_sp big, Fixnum small) {
  if (small == 0)
    return big;
  // Don't think mpn_gcd_1 understands negatives.
  if (small < 0)
    small = -small;
  return clasp_make_fixnum(mpn_gcd_1(big->limbs(), std::abs(big->length()), small));
}

Rational_sp Bignum_O::ratdivide(Integer_sp divisor) const {
  // FIXME?: Some of the bignum_results in here might always
  // end up with bignums and so could be direct create calls instead.
  mp_size_t len = this->length();
  mp_size_t size = std::abs(len);
  const mp_limb_t* limbs = this->limbs();

  if (divisor.fixnump()) {
    Fixnum fdivisor = divisor.unsafe_fixnum();
    if (fdivisor == 0)
      ERROR_DIVISION_BY_ZERO(this->asSmartPtr(), divisor);
    Fixnum result_sign = ((len < 0) ^ (fdivisor < 0)) ? -1 : 1;
    Fixnum adivisor = std::abs(fdivisor);
    mp_limb_t gcd = mpn_gcd_1(limbs, size, adivisor);
    mp_limb_t num[size];
    mpn_divexact_1(num, limbs, size, gcd);
    // quick normalize
    mp_limb_t num_length;
    if (num[size - 1] == 0)
      num_length = result_sign * (size - 1);
    else
      num_length = result_sign * size;
    Integer_sp bnum = bignum_result(num_length, num);
    if (gcd == adivisor) // exact division
      return bnum;
    else { // need a ratio
      Fixnum adenom = adivisor / gcd;
      return Ratio_O::create_primitive(bnum, clasp_make_fixnum(adenom));
    }
  } else {
    // divisor is a bignum.
    // TODO: Switch to As_unsafe once old bignums are gone.
    Bignum_sp bdivisor = gc::As<Bignum_sp>(divisor);
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
        if (divlen < 0)
          return Ratio_O::create_primitive(gc::As_unsafe<Integer_sp>(this->negate_()),
                                           gc::As_unsafe<Integer_sp>(divisor->negate_()));
        else
          return Ratio_O::create_primitive(this->asSmartPtr(), bdivisor);
      }

      // Nope, have to do some divisions.
      // Compute the numerator
      mp_limb_t numsize = size;
      mp_limb_t numlimbs[numsize];
      mpn_divexact_1(numlimbs, limbs, size, fgcd);
      if (numlimbs[numsize - 1] == 0)
        --numsize;
      Integer_sp numerator = bignum_result(result_sign * numsize, numlimbs);

      // Compute the denominator
      mp_limb_t densize = divsize;
      mp_limb_t denlimbs[densize];
      mpn_divexact_1(denlimbs, divlimbs, divsize, fgcd);
      if (denlimbs[densize - 1] == 0)
        --densize;
      // If the denominator is 1, return the numerator
      if ((densize == 1) && (denlimbs[0] == 1))
        return numerator;
      // Nope, making a fraction.
      // Denominator is always positive.
      Integer_sp denominator = bignum_result(densize, denlimbs);

      return Ratio_O::create_primitive(numerator, denominator);
    } else {
      // GCD is a bignum.
      // NOTE: If MPN had a divexact we could use it,
      // but it doesn't seem to, so we just use tdiv_qr
      // and ignore the remainder.
      // (eta) Actually there's an mpz_divexact that uses an
      // mpn_divexact function, but this latter isn't documented.
      // Might be worth looking into - apparently the exact division
      // algorithm is a few times faster than the usual one.
      Bignum_sp bgcd = gc::As_unsafe<Bignum_sp>(gcd);
      mp_size_t gcd_size = bgcd->length(); // necessarily positive
      const mp_limb_t* gcd_limbs = bgcd->limbs();

      mp_limb_t remainder_limbs[gcd_size];

      // Compute the numerator
      mp_size_t numsize = size - gcd_size + 1;
      mp_limb_t numlimbs[numsize];
      mpn_tdiv_qr(numlimbs, remainder_limbs, (mp_size_t)0, limbs, size, gcd_limbs, gcd_size);
      // Numerator MSL may be zero
      if (numlimbs[numsize - 1] == 0)
        --numsize;
      Integer_sp numerator = bignum_result(result_sign * numsize, numlimbs);

      // Compute the denominator
      mp_size_t densize = divsize - gcd_size + 1;
      mp_limb_t denlimbs[densize];
      mpn_tdiv_qr(denlimbs, remainder_limbs, (mp_size_t)0, divlimbs, divsize, gcd_limbs, gcd_size);
      // Denominator MSL may be zero
      if (denlimbs[densize - 1] == 0)
        --densize;

      // If the denominator is one, just return the numerator
      if ((densize == 1) && (denlimbs[0] == 1))
        return numerator;

      // Nope, make a fraction
      // Denominator still always positive
      Integer_sp denominator = bignum_result(densize, denlimbs);
      return Ratio_O::create_primitive(numerator, denominator);
    }
  }
}

Integer_sp next_add(const mp_limb_t* llimbs, mp_size_t llen, const mp_limb_t* rlimbs, mp_size_t rlen) {
  mp_size_t absllen = std::abs(llen), absrlen = std::abs(rlen);

  // Keep the larger number in the left.
  if (absllen < absrlen) {
    std::swap(llen, rlen);
    std::swap(absllen, absrlen);
    std::swap(llimbs, rlimbs);
  }

  mp_size_t result_len;
  mp_limb_t result_limbs[1 + absllen];

  if ((llen ^ rlen) < 0) {
    // the lengths (and therefore the numbers) have different sign.
    result_len = absllen;
    if (absllen != absrlen) {
      // left is larger, so we fear no carry.
      mpn_sub(result_limbs, llimbs, absllen, rlimbs, absrlen);
      BIGNUM_NORMALIZE(result_len, result_limbs);
      if (llen < 0)
        result_len = -result_len;
    } // They have the same size, so we have to do a real comparison.
    else if (mpn_cmp(llimbs, rlimbs, absllen) < 0) {
      // left has less magnitude, so it's the subtrahend
      mpn_sub_n(result_limbs, rlimbs, llimbs, absllen);
      BIGNUM_NORMALIZE(result_len, result_limbs);
      if (rlen < 0)
        result_len = -result_len;
    } else {
      // right has less magnitude
      mpn_sub_n(result_limbs, llimbs, rlimbs, absllen);
      BIGNUM_NORMALIZE(result_len, result_limbs);
      if (llen < 0)
        result_len = -result_len;
    }
  } else {
    // The numbers have the same sign, so just add them
    mp_limb_t carry = mpn_add(result_limbs, llimbs, absllen, rlimbs, absrlen);
    result_limbs[absllen] = carry;
    result_len = absllen + carry; // carry is either 0 or 1
    if (llen < 0)
      result_len = -result_len;
  }
  // NOTE: If the signs match we definitely have a bignum, so hypothetically
  // the tests in bignum_result could be skipped.
  return bignum_result(result_len, result_limbs);
}

DOCGROUP(clasp);
CL_DEFUN Integer_sp core__next_add(Bignum_sp left, Bignum_sp right) {
  return next_add(left->limbs(), left->length(), right->limbs(), right->length());
}

DOCGROUP(clasp);
CL_DEFUN Integer_sp core__next_sub(Bignum_sp left, Bignum_sp right) {
  return next_add(left->limbs(), left->length(), right->limbs(), -(right->length()));
}

// Easier than above since we know abs(right) < abs(left) and the result size.
Integer_sp next_fadd(const mp_limb_t* limbs, mp_size_t len, Fixnum right) {
  Fixnum aright = std::abs(right);
  mp_size_t size = std::abs(len);

  mp_size_t result_len = size;
  mp_limb_t result_limbs[size + 1];

  if ((len < 0) ^ (right < 0)) {
    // Different signs - subtract
    mpn_sub_1(result_limbs, limbs, size, aright);
    // Quick normalize
    if (result_limbs[size - 1] == 0)
      --result_len;
  } else {
    // signs match
    mp_limb_t carry = mpn_add_1(result_limbs, limbs, size, aright);
    if (carry != 0) {
      ++result_len;
      result_limbs[size] = carry;
    }
  }
  // Result has the same sign as the bigger number (so, the bignum)
  if (len < 0)
    result_len = -result_len;
  return bignum_result(result_len, result_limbs);
}

DOCGROUP(clasp);
CL_DEFUN Integer_sp core__next_fadd(Bignum_sp left, Fixnum right) { return next_fadd(left->limbs(), left->length(), right); }

// bignum - fixnum is trivially bignum +-fixnum, but fixnum - bignum
// is very slightly trickier
DOCGROUP(clasp);
CL_DEFUN Integer_sp core__next_fsub(Fixnum left, Bignum_sp right) { return next_fadd(right->limbs(), -(right->length()), left); }

Number_sp Bignum_O::oneMinus_() const { return next_fadd(this->limbs(), this->length(), -1); }

Number_sp Bignum_O::onePlus_() const { return next_fadd(this->limbs(), this->length(), 1); }

double next_to_double(mp_size_t len, const mp_limb_t* limbs) {
  // MPN does not seem to export a float conversion function,
  // so we roll our own. FIXME: Could get bad with float rounding
  // or otherwise as I'm making this up as I go.
  mp_size_t size = std::abs(len);

  // There are no length zero bignums.
  // If the bignum is only one long, we just convert directly.
  if (len == 1)
    return static_cast<double>(limbs[0]);
  else if (len == -1)
    return -static_cast<double>(limbs[0]);

  // Otherwise, we just use the most significant two limbs.
  // Assuming the float format has at most 128 bits of significand,
  // the lower limbs ought to be irrelevant.
  // (In fact one 64-bit number would probably be enough, but
  //  it's possible the high bits of this number are almost all zero.)
  double ultimate = static_cast<double>(limbs[size - 1]);
  double penultimate = static_cast<double>(limbs[size - 2]);
  double soon = penultimate + std::ldexp(ultimate, 64);
  return std::ldexp(((len < 0) ? -soon : soon), 64 * (size - 2));
}

float Bignum_O::as_float_() const { return static_cast<float>(mpz_get_d(this->mpz().get_mpz_t())); }

double Bignum_O::as_double_() const { return mpz_get_d(this->mpz().get_mpz_t()); }

LongFloat Bignum_O::as_long_float_() const { return static_cast<LongFloat>(next_to_double(this->length(), this->limbs())); }

DOCGROUP(clasp);
CL_DEFUN int core__next_compare(Bignum_sp left, Bignum_sp right) {
  mp_size_t llen = left->length(), rlen = right->length();
  const mp_limb_t *llimbs = left->limbs(), *rlimbs = right->limbs();

  if (llen < rlen)
    return -1;
  else if (llen > rlen)
    return 1;
  else { // actual comparison
    // mpn_cmp is only defined to return "a negative value" etc.,
    // so we normalize it to -1, 0, or 1.
    int cmp = mpn_cmp(llimbs, rlimbs, std::abs(llen));
    if (cmp < 0) {
      if (llen < 0)
        return 1;
      else
        return -1;
    } else if (cmp > 0) {
      if (llen < 0)
        return -1;
      else
        return 1;
    } else
      return 0;
  }
}

CL_DEFUN Bignum_sp core__dump_bignum(Bignum_sp bignum) {
  int64_t len = bignum->_limbs.signedLength();
  fmt::print("{}:{}:{} len = {}\n", __FILE__, __LINE__, __FUNCTION__, len);
  for (size_t ii = 0; ii < bignum->_limbs.size(); ii++) {
    fmt::print("{}:{}:{}  limb[{}] = {:x}\n", __FILE__, __LINE__, __FUNCTION__, ii, bignum->_limbs[ii]);
  }
  return bignum;
}

CL_DEFUN Bignum_sp core__bignum_from_fixnum_add_over(int64_t add_over) {
  fmt::print("{}:{}:{}       add_over = {:x}\n", __FILE__, __LINE__, __FUNCTION__, add_over);
  mp_limb_t limb;
  int64_t len;
  if (add_over < 0) { // positive
    len = 1;
    limb = ((uint64_t)add_over) >> 2;
  }
  if (add_over > 0) { // negative
    len = -1;
    limb = ((uint64_t)((~add_over) + 1)) >> 2;
  } else { // add_over == 0
    len = -1;
    limb = 0x4000000000000000;
  }
  fmt::print("{}:{}:{}  len = {:2d} limb = {:x}\n", __FILE__, __LINE__, __FUNCTION__, len, limb);
  return Bignum_O::create_from_limbs(len, limb, true);
}

CL_DEFUN Bignum_sp core__bignum_do_fixnum_add_over(T_sp x, T_sp y) {
  fmt::print("{}:{}:{} x = {} y = {}\n", __FILE__, __LINE__, __FUNCTION__, (void*)x.raw_(), (void*)y.raw_());
  int64_t add_over = (int64_t)x.raw_() + (int64_t)y.raw_();
  return core__bignum_from_fixnum_add_over(add_over);
}

}; // namespace core
