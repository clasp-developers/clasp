#include <clasp/core/foundation.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/bformat.h>
#include <clasp/core/array.h>
#include <clasp/core/bits.h>
#include <clasp/core/wrappers.h>
#include <clasp/core/bignum.h>

/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    bits.cc  -- Logical operations on numbers.
If you're looking for bit array stuff, try array_bit.cc.
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

namespace core {

// Add 1 to in and stuff it in result. Return normalized size of result.
mp_size_t next_addone_aux(mp_limb_t* result, const mp_limb_t* in, mp_size_t size) {
  if (size == 0) {
    // zero size argument to mpn_add_1 is invalid, so we special case
    result[0] = (mp_limb_t)1;
    return 1;
  }
  mp_limb_t carry = mpn_add_1(result, in, size, (mp_limb_t)1);
  if (carry != 0) {
    result[size] = carry;
    ++size;
  } else
    BIGNUM_NORMALIZE(size, result);
  return size;
}

// AND two positive bignums together and return size of the result.
mp_size_t next_and_aux(mp_limb_t* result, const mp_limb_t* s1, mp_size_t size1, const mp_limb_t* s2, mp_size_t size2) {
  mp_size_t result_size = std::min(size1, size2);
  if (result_size == 0)
    return 0;
  mpn_and_n(result, s1, s2, result_size);
  BIGNUM_NORMALIZE(result_size, result);
  return result_size;
}

// OR two positive bignums together and return size of the result.
mp_size_t next_ior_aux(mp_limb_t* result, const mp_limb_t* s1, mp_size_t size1, const mp_limb_t* s2, mp_size_t size2) {
  if (size1 < size2) {
    if (size1 != 0)
      mpn_ior_n(result, s1, s2, size1);
    // FIXME memcpy sth
    for (size_t i = size1; i < size2; ++i)
      result[i] = s2[i];
    return size2;
  } else {
    if (size2 != 0)
      mpn_ior_n(result, s1, s2, size2);
    // FIXME memcpy sth
    for (size_t i = size2; i < size1; ++i)
      result[i] = s1[i];
    return size1;
  }
}

// XOR two positive bignums together and return size of the result.
mp_size_t next_xor_aux(mp_limb_t* result, const mp_limb_t* s1, mp_size_t size1, const mp_limb_t* s2, mp_size_t size2) {
  // x^0 = x so we have to copy over the high limbs.
  if (size1 < size2) {
    if (size1 != 0)
      mpn_xor_n(result, s1, s2, size1);
    for (mp_size_t i = size1; i < size2; ++i) // FIXME memcpy sth
      result[i] = s2[i];
    BIGNUM_NORMALIZE(size2, result);
    return size2;
  } else {
    if (size2 != 0)
      mpn_xor_n(result, s1, s2, size2);
    for (mp_size_t i = size2; i < size1; ++i) // FIXME memcpy sth
      result[i] = s1[i];
    BIGNUM_NORMALIZE(size1, result);
    return size1;
  }
}

// Compute s1 & ~s2, both operands being positive, and return size of result.
mp_size_t next_andc2_aux(mp_limb_t* result, const mp_limb_t* s1, mp_size_t size1, const mp_limb_t* s2, mp_size_t size2) {
  if (size1 < size2) {
    // For the high limbs s1 is zero, so high limbs of s2 are discarded.
    if (size1 != 0)
      mpn_andn_n(result, s1, s2, size1);
    BIGNUM_NORMALIZE(size1, result);
    return size1;
  } else {
    // For the high limbs s1 is something and ~s2 = -1, so copy s1 in
    if (size2 != 0)
      mpn_andn_n(result, s1, s2, size2);
    for (mp_size_t i = size2; i < size1; ++i) // FIXME memcpy
      result[i] = s1[i];
    BIGNUM_NORMALIZE(size1, result);
    return size1;
  }
}

mp_size_t next_clr(mp_limb_t* result, const mp_limb_t* s1, mp_size_t len1, const mp_limb_t* s2, mp_size_t len2) { return 0; }

// Return bignum parts for -1.
mp_size_t next_set_aux(mp_limb_t* result) {
  result[0] = 1;
  return -1;
}

mp_size_t next_set(mp_limb_t* result, const mp_limb_t* s1, mp_size_t len1, const mp_limb_t* s2, mp_size_t len2) {
  return next_set_aux(result);
}

// next_add where t1 is negative, t2 is positive.
mp_size_t next_addneg(mp_limb_t* result, const mp_limb_t* t1, mp_size_t size1, const mp_limb_t* t2, mp_size_t size2) {
  // t1 is positive, t2 is negative.
  // result will be positive (think of the infinite left bits)
  // x & (-y) = x & ~(y-1)
  // t1 controls the high limbs, since its high limbs are always zero
  // and zero & anything = 0.
  mp_limb_t temp[size2];
  mpn_sub_1(temp, t2, size2, (mp_limb_t)1);
  return next_andc2_aux(result, t1, size1, temp, size2);
}

mp_size_t next_and(mp_limb_t* result, const mp_limb_t* s1, mp_size_t len1, const mp_limb_t* s2, mp_size_t len2) {
  if (len1 > 0) {
    if (len2 > 0)
      // Both operands are positive so this is simple.
      return next_and_aux(result, s1, len1, s2, len2);
    else if (len2 < 0)
      return next_addneg(result, s1, len1, s2, -len2);
    else
      return 0; // s2 = 0
  } else if (len1 < 0) {
    if (len2 > 0)
      return next_addneg(result, s2, len2, s1, -len1);
    else if (len2 < 0) {
      // Both are negative.
      // -((-x) & (-y)) = -(~(x-1) & ~(y-1))
      // = ~(~(x-1) & ~(y-1)) + 1
      // = ((x-1) | (y-1)) + 1 [de morgan]
      // Note that this can expand, e.g. (logand -3 -2) = -4
      // and 4 needs one more bit to represent.
      mp_size_t size1 = -len1, size2 = -len2;
      mp_limb_t temp1[size1];
      mpn_sub_1(temp1, s1, size1, (mp_limb_t)1);
      mp_limb_t temp2[size2];
      mpn_sub_1(temp2, s2, size2, (mp_limb_t)1);

      mp_size_t result_size = next_ior_aux(result, temp1, size1, temp2, size2);
      BIGNUM_NORMALIZE(result_size, result); // FIXME: Not sure if needed
      return -next_addone_aux(result, result, result_size);
    } else
      return 0; // s2 = 0
  } else
    return 0; // s1 = 0
}

mp_size_t next_andc2(mp_limb_t* result, const mp_limb_t* s1, mp_size_t len1, const mp_limb_t* s2, mp_size_t len2) {
  if (len1 > 0) {
    if (len2 > 0)
      return next_andc2_aux(result, s1, len1, s2, len2);
    else if (len2 < 0) {
      // x & ~(-y) = x & ~(~(y-1)) = x & (y-1)
      mp_size_t size2 = -len2;
      mp_limb_t temp[size2];
      mpn_sub_1(temp, s2, size2, (mp_limb_t)1);
      return next_and_aux(result, s1, len1, temp, size2);
    } else { // len2 = 0, and a&~0 = a
      mpn_copyi(result, s1, len1);
      return len1;
    }
  } else if (len1 < 0) {
    if (len2 > 0) {
      // Negative result.
      // -(-x & ~y) = -(~(x-1) & ~y) = ~(~(x-1) & ~y) + 1 = ((x-1) | y) + 1
      mp_size_t size1 = -len1;
      mp_limb_t temp[size1];
      mpn_sub_1(temp, s1, size1, (mp_limb_t)1);
      mp_size_t result_size = next_ior_aux(result, temp, size1, s2, len2);
      return -next_addone_aux(result, result, result_size);
    } else if (len2 < 0) {
      // Positive
      // -x & ~(-y) = ~(x-1) & ~(~(y-1)) = ~(x-1) & (y-1)
      mp_size_t size1 = -len1;
      mp_limb_t temp1[size1];
      mpn_sub_1(temp1, s1, size1, (mp_limb_t)1);
      mp_size_t size2 = -len2;
      mp_limb_t temp2[size2];
      mpn_sub_1(temp2, s2, size2, (mp_limb_t)1);
      return next_andc2_aux(result, temp2, size2, temp1, size1);
    } else { // len2 = 0, len1 negative
      mpn_copyi(result, s1, -len1);
      return len1;
    }
  } else
    return 0; // len1 = 0, so result is zero
}

mp_size_t next_1(mp_limb_t* result, const mp_limb_t* s1, mp_size_t len1, const mp_limb_t* s2, mp_size_t len2) {
  // mpn gives us the ability to copy increasingly or decreasingly,
  // but I don't think it matters which we do here.
  if (len1 != 0)
    mpn_copyi(result, s1, std::abs(len1));
  return len1;
}

mp_size_t next_andc1(mp_limb_t* result, const mp_limb_t* s1, mp_size_t len1, const mp_limb_t* s2, mp_size_t len2) {
  return next_andc2(result, s2, len2, s1, len1);
}

mp_size_t next_2(mp_limb_t* result, const mp_limb_t* s1, mp_size_t len1, const mp_limb_t* s2, mp_size_t len2) {
  return next_1(result, s2, len2, s1, len1);
}

mp_size_t next_xorneg(mp_limb_t* result, const mp_limb_t* t1, mp_size_t size1, const mp_limb_t* t2, mp_size_t size2) {
  // t1 positive, t2 negative.
  // result negative because we have infinite 0^-1 on the left.
  // -(a ^ (-b)) = -(a ^ ~(b-1))
  // = ~(a ^ ~(b-1)) + 1
  // = (a ^ (b-1)) + 1
  mp_limb_t temp[size2];
  mpn_sub_1(temp, t2, size2, (mp_limb_t)1);
  mp_size_t result_size = next_xor_aux(result, t1, size1, temp, size2);
  return -next_addone_aux(result, result, result_size);
}

mp_size_t next_xor(mp_limb_t* result, const mp_limb_t* s1, mp_size_t len1, const mp_limb_t* s2, mp_size_t len2) {
  if (len1 > 0) {
    if (len2 > 0)
      return next_xor_aux(result, s1, len1, s2, len2);
    else if (len2 < 0)
      return next_xorneg(result, s1, len1, s2, -len2);
    else {                         // s2 = 0, so s1 ^ s2 = s1
      mpn_copyi(result, s1, len1); // remember len1 is positive & nonzero
      return len1;
    }
  } else if (len1 < 0) {
    if (len2 < 0) {
      // Both negative. Result is positive.
      // (-x) ^ (-y) = ~(x-1) ^ ~(y-1) = (x-1) ^ (y-1)
      mp_size_t size1 = -len1, size2 = -len2;
      mp_limb_t temp1[size1];
      mpn_sub_1(temp1, s1, size1, (mp_limb_t)1);
      mp_limb_t temp2[size2];
      mpn_sub_1(temp2, s2, size2, (mp_limb_t)1);
      return next_xor_aux(result, temp1, size1, temp2, size2);
    } else if (len2 > 0)
      return next_xorneg(result, s2, len2, s1, -len1);
    else { // len2 = 0 (and len1 is negative & nonzero)
      mpn_copyi(result, s1, -len1);
      return len1;
    }
  } else { // len1 = 0
    if (len2 != 0)
      mpn_copyi(result, s2, std::abs(len2));
    return len2;
  }
}

mp_size_t next_iorneg(mp_limb_t* result, const mp_limb_t* t1, mp_size_t size1, const mp_limb_t* t2, mp_size_t size2) {
  // Result is negative since we have 0|-1 = -1 on the left.
  // -(x | -y) = -(x | ~(y-1)) = ~(x | ~(y-1)) + 1
  // = ~x & (y-1) + 1
  mp_limb_t temp[size2];
  mpn_sub_1(temp, t2, size2, (mp_limb_t)1);
  mp_size_t result_size = next_andc2_aux(result, temp, size2, t1, size1);
  return -next_addone_aux(result, result, result_size);
}

mp_size_t next_ior(mp_limb_t* result, const mp_limb_t* s1, mp_size_t len1, const mp_limb_t* s2, mp_size_t len2) {
  if (len1 > 0) {
    if (len2 > 0)
      return next_ior_aux(result, s1, len1, s2, len2);
    else if (len2 < 0)
      return next_iorneg(result, s1, len1, s2, -len2);
    else { // len2 = 0
      mpn_copyi(result, s1, len1);
      return len1;
    }
  } else if (len1 < 0) {
    if (len2 > 0)
      return next_iorneg(result, s2, len2, s1, -len1);
    else if (len2 < 0) {
      // Both negative. Result is also negative due to -1|-1 on left.
      // -(-x | -y) = -(~(x-1) | ~(y-1)) = ~(~(x-1) | ~(y-1)) + 1
      // = ((x-1) & (y-1)) + 1
      mp_size_t size1 = -len1, size2 = -len2;
      mp_limb_t temp1[size1];
      mpn_sub_1(temp1, s1, size1, (mp_limb_t)1);
      mp_limb_t temp2[size2];
      mpn_sub_1(temp2, s2, size2, (mp_limb_t)1);
      mp_size_t result_size = next_and_aux(result, temp1, size1, temp2, size2);
      return -next_addone_aux(result, result, result_size);
    } else { // len2 = 0, len1 negative
      mpn_copyi(result, s1, -len1);
      return len1;
    }
  } else { // len1 = 0
    if (len2 != 0)
      mpn_copyi(result, s2, std::abs(len2));
    return len2;
  }
}

mp_size_t next_com(mp_limb_t* result, const mp_limb_t* s, mp_size_t size) {
  if (size > 0)
    // -~x = ~~x + 1 = x + 1
    return -next_addone_aux(result, s, size);
  else if (size < 0) {
    // ~-x = ~~(x - 1) = x - 1
    mp_size_t result_size = -size;
    mpn_sub_1(result, s, result_size, (mp_limb_t)1);
    BIGNUM_NORMALIZE(result_size, result);
    return result_size;
  } else
    return next_set_aux(result);
}

mp_size_t next_norneg(mp_limb_t* result, const mp_limb_t* t1, mp_size_t size1, const mp_limb_t* t2, mp_size_t size2) {
  // ~(-1|0) = 0 so the result is positive
  // ~(x | -y) = ~(x | ~(y-1)) = (~x & (y-1))
  mp_limb_t temp[size2];
  mpn_sub_1(temp, t2, size2, (mp_limb_t)1);
  return next_andc2_aux(result, temp, size2, t1, size1);
}

mp_size_t next_nor(mp_limb_t* result, const mp_limb_t* s1, mp_size_t len1, const mp_limb_t* s2, mp_size_t len2) {
  if (len1 > 0) {
    if (len2 > 0) {
      // Negation of high limbs means the result is negative.
      // -~(x | y) = ~~(x | y) + 1 = (x | y) + 1
      mp_size_t result_size = next_ior_aux(result, s1, len1, s2, len2);
      return -next_addone_aux(result, result, result_size);
    } else if (len2 < 0)
      return next_norneg(result, s1, len1, s2, -len2);
    else
      return next_com(result, s1, len1);
  } else if (len1 < 0) {
    if (len2 > 0)
      return next_norneg(result, s2, len2, s1, -len1);
    else if (len2 < 0) {
      // ~(-x | -y) = ~(~(x-1) | ~(y-1)) = (x-1) & (y-1)
      mp_size_t size1 = -len1, size2 = -len2;
      mp_limb_t temp1[size1];
      mpn_sub_1(temp1, s1, size1, (mp_limb_t)1);
      mp_limb_t temp2[size2];
      mpn_sub_1(temp2, s2, size2, (mp_limb_t)1);
      return next_and_aux(result, temp1, size1, temp2, size2);
    } else
      return next_com(result, s1, len1);
  } else
    return next_com(result, s2, len2); // s1 = 0
}

mp_size_t next_eqvneg(mp_limb_t* result, const mp_limb_t* t1, mp_size_t size1, const mp_limb_t* t2, mp_size_t size2) {
  // ~(0^-1) = 0 so the result is positive.
  // ~(x ^ -y) = ~(x ^ ~(y - 1)) = x ^ (y - 1)
  mp_limb_t temp[size2];
  mpn_sub_1(temp, t2, size2, (mp_limb_t)1);
  return next_xor_aux(result, t1, size1, temp, size2);
}

// eqv is xnor if that wasn't clear
mp_size_t next_eqv(mp_limb_t* result, const mp_limb_t* s1, mp_size_t len1, const mp_limb_t* s2, mp_size_t len2) {
  if (len1 > 0) {
    if (len2 > 0) {
      // Both positive. Result is negative because ~(0^0) = -1.
      // -~(x ^ y) = ~~(x ^ y) + 1 = (x ^ y) + 1
      mp_size_t result_size = next_xor_aux(result, s1, len1, s2, len2);
      return -next_addone_aux(result, result, result_size);
    } else if (len2 < 0)
      return next_eqvneg(result, s1, len1, s2, -len2);
    else
      return next_com(result, s1, len1);
  } else if (len1 < 0) {
    if (len2 > 0)
      return next_eqvneg(result, s2, len2, s1, -len1);
    else if (len2 < 0) {
      // Both negative. ~(-1^-1) = -1 so result is negative.
      // -~(-x ^ -y) = ~~(-x ^ -y) + 1 = (-x ^ -y) + 1
      // = (~(x-1) ^ ~(y-1)) + 1 = ((x-1) ^ (y-1)) + 1
      mp_size_t size1 = -len1;
      mp_limb_t temp1[size1];
      mpn_sub_1(temp1, s1, size1, (mp_limb_t)1);
      mp_size_t size2 = -len2;
      mp_limb_t temp2[size2];
      mpn_sub_1(temp2, s2, size2, (mp_limb_t)1);
      mp_size_t result_size = next_xor_aux(result, temp1, size1, temp2, size2);
      return -next_addone_aux(result, result, result_size);
    } else
      return next_com(result, s1, len1);
  } else
    return next_com(result, s2, len2);
}

mp_size_t next_c2(mp_limb_t* result, const mp_limb_t* s1, mp_size_t len1, const mp_limb_t* s2, mp_size_t len2) {
  return next_com(result, s2, len2);
}

mp_size_t next_orc2(mp_limb_t* result, const mp_limb_t* s1, mp_size_t len1, const mp_limb_t* s2, mp_size_t len2) {
  if (len1 > 0) {
    if (len2 > 0) {
      // 0|~0 = -1, negative result
      // -(x | ~y) = ~(x | ~y) + 1 = (~x & y) + 1
      mp_size_t result_size = next_andc2_aux(result, s2, len2, s1, len1);
      return -next_addone_aux(result, result, result_size);
    } else if (len2 < 0) {
      // 0|~-1 = 0, positive result
      // x | ~-y = x | ~(~(y-1)) = x | (y-1)
      mp_size_t size2 = -len2;
      mp_limb_t temp[size2];
      mpn_sub_1(temp, s2, size2, (mp_limb_t)1);
      mp_size_t result_size = next_ior_aux(result, s1, len1, temp, size2);
      BIGNUM_NORMALIZE(result_size, result); // FIXME not sure if necessary
      return result_size;
    } else
      return next_set_aux(result); // s2 = 0. x|~0 = ~0
  } else if (len1 < 0) {
    if (len2 > 0) {
      // -1|~0 = -1, negative result
      // -(-x | ~y) = -(~(x-1) | ~y) = ~(~(x-1) | ~y) + 1
      // = ((x-1) & y) + 1
      mp_size_t size1 = -len1;
      mp_limb_t temp[size1];
      mpn_sub_1(temp, s1, size1, (mp_limb_t)1);
      mp_size_t result_size = next_and_aux(result, temp, size1, s2, len2);
      return -next_addone_aux(result, result, result_size);
    } else if (len2 < 0) {
      // 1|~1 = 1, negative result
      // -(-x | ~-y) = -(~(x-1) | ~(~(y-1))) = -(~(x-1) | (y-1))
      // = ~(~(x-1) | (y-1)) + 1 = ((x-1) & ~(y-1)) + 1
      mp_size_t size1 = -len1, size2 = -len2;
      mp_limb_t temp1[size1];
      mpn_sub_1(temp1, s1, size1, (mp_limb_t)1);
      mp_limb_t temp2[size2];
      mpn_sub_1(temp2, s2, size2, (mp_limb_t)1);
      mp_size_t result_size = next_andc2_aux(result, temp1, size1, temp2, size2);
      return -next_addone_aux(result, result, result_size);
    } else
      return next_set_aux(result); // x|~0 = ~0
  } else
    return next_com(result, s2, len2);
}

mp_size_t next_c1(mp_limb_t* result, const mp_limb_t* s1, mp_size_t len1, const mp_limb_t* s2, mp_size_t len2) {
  return next_com(result, s1, len1);
}

mp_size_t next_orc1(mp_limb_t* result, const mp_limb_t* s1, mp_size_t len1, const mp_limb_t* s2, mp_size_t len2) {
  return next_orc2(result, s2, len2, s1, len1);
}

mp_size_t next_nandneg(mp_limb_t* result, const mp_limb_t* t1, mp_size_t size1, const mp_limb_t* t2, mp_size_t size2) {
  // ~(0&-1) = ~0 = -1, negative result
  // -(~(x & -y)) = ~(~(x & -y)) + 1 = (x & -y) + 1
  // = (x & ~(y-1)) + 1
  mp_limb_t temp[size2];
  mpn_sub_1(temp, t2, size2, (mp_limb_t)1);
  mp_size_t result_size = next_andc2_aux(result, t1, size1, temp, size2);
  return -next_addone_aux(result, result, result_size);
}

mp_size_t next_nand(mp_limb_t* result, const mp_limb_t* s1, mp_size_t len1, const mp_limb_t* s2, mp_size_t len2) {
  if (len1 > 0) {
    if (len2 > 0) {
      // ~(0&0) = ~0 = -1, negative result
      // -(~(x & y)) = ~(~(x & y)) + 1 = (x & y) + 1
      mp_size_t result_size = next_and_aux(result, s1, len1, s2, len2);
      return -next_addone_aux(result, result, result_size);
    } else if (len2 < 0)
      return next_nandneg(result, s1, len1, s2, -len2);
    else
      return next_set_aux(result); // ~(x&0) = ~0 = -1
  } else if (len1 < 0) {
    if (len2 > 0)
      return next_nandneg(result, s2, len2, s1, -len1);
    else if (len2 < 0) {
      // ~(-1&-1) = ~(-1) = 0, positive result
      // ~(-x & -y) = ~(~(x-1) & ~(y-1)) = (x-1) | (y-1)
      mp_size_t size1 = -len1, size2 = -len2;
      mp_limb_t temp1[size1];
      mpn_sub_1(temp1, s1, size1, (mp_limb_t)1);
      mp_limb_t temp2[size2];
      mpn_sub_1(temp2, s2, size2, (mp_limb_t)1);
      mp_size_t result_size = next_ior_aux(result, temp1, size1, temp2, size2);
      // May be necessary since we subtracted. e.g. for (lognand x x) where
      // x = (- (ash 1 64))
      BIGNUM_NORMALIZE(result_size, result);
      return result_size;
    } else
      return next_set_aux(result);
  } else
    return next_set_aux(result);
}

typedef mp_size_t (*next_bit_operator)(mp_limb_t*, const mp_limb_t*, mp_size_t, const mp_limb_t*, mp_size_t);

static next_bit_operator next_operations[boolOpsMax] = {next_clr, next_and,  next_andc2, next_1,   next_andc1, next_2,
                                                        next_xor, next_ior,  next_nor,   next_eqv, next_c2,    next_orc2,
                                                        next_c1,  next_orc1, next_nand,  next_set};

// ----------------------------------------------------------------------

/*
 * BIT OPERATIONS FOR FIXNUMS
 */

static gctools::Fixnum ior_op(gctools::Fixnum i, gctools::Fixnum j) { return (i | j); }

static gctools::Fixnum xor_op(gctools::Fixnum i, gctools::Fixnum j) { return (i ^ j); }

static gctools::Fixnum and_op(gctools::Fixnum i, gctools::Fixnum j) { return (i & j); }

static gctools::Fixnum eqv_op(gctools::Fixnum i, gctools::Fixnum j) { return (~(i ^ j)); }

static gctools::Fixnum nand_op(gctools::Fixnum i, gctools::Fixnum j) { return (~(i & j)); }

static gctools::Fixnum nor_op(gctools::Fixnum i, gctools::Fixnum j) { return (~(i | j)); }

static gctools::Fixnum andc1_op(gctools::Fixnum i, gctools::Fixnum j) { return ((~i) & j); }

static gctools::Fixnum andc2_op(gctools::Fixnum i, gctools::Fixnum j) { return (i & (~j)); }

static gctools::Fixnum orc1_op(gctools::Fixnum i, gctools::Fixnum j) { return ((~i) | j); }

static gctools::Fixnum orc2_op(gctools::Fixnum i, gctools::Fixnum j) { return (i | (~j)); }

static gctools::Fixnum b_clr_op(gctools::Fixnum i, gctools::Fixnum j) { return (0); }

static gctools::Fixnum b_set_op(gctools::Fixnum i, gctools::Fixnum j) { return (-1); }

static gctools::Fixnum b_1_op(gctools::Fixnum i, gctools::Fixnum j) { return (i); }

static gctools::Fixnum b_2_op(gctools::Fixnum i, gctools::Fixnum j) { return (j); }

static gctools::Fixnum b_c1_op(gctools::Fixnum i, gctools::Fixnum j) { return (~i); }

static gctools::Fixnum b_c2_op(gctools::Fixnum i, gctools::Fixnum j) { return (~j); }

typedef gctools::Fixnum (*bit_operator)(gctools::Fixnum, gctools::Fixnum);

static bit_operator fixnum_operations[boolOpsMax] = {b_clr_op, and_op, andc2_op, b_1_op,  andc1_op, b_2_op,  xor_op,  ior_op,
                                                     nor_op,   eqv_op, b_c2_op,  orc2_op, b_c1_op,  orc1_op, nand_op, b_set_op};

// ----------------------------------------------------------------------

Integer_sp next_operation_rest(boole_ops op, List_sp integers) {
  // integers must not be nil.
  mp_size_t max_size = 0; // 0 here means fixnum.
  for (auto cur : integers) {
    Integer_sp e = gc::As<Integer_sp>(oCar(cur));
    if (!(e.fixnump())) {
      // FIXME: switch to As_unsafe once old bignums are gone.
      Bignum_sp big = gc::As<Bignum_sp>(e);
      max_size = std::max(max_size, std::abs(big->length()));
    }
  }
  // Now we know the sizes of all the integers,
  // and incidentally, that they are integers (because of As).
  if (max_size == 0) { // all fixnums
    Fixnum result = oCar(integers).unsafe_fixnum();
    bit_operator bop = fixnum_operations[op];
    for (auto cur : (List_sp)oCdr(integers))
      result = bop(result, oCar(cur).unsafe_fixnum());
    return clasp_make_fixnum(result);
  } else { // at least one bignum, so in general the result is a bignum
    // Because of the sign-magnitude representation of bignums, these
    // operations can sometimes require more space in the result than
    // in the operands, because e.g. (logand -2 -3) = -4 and 4 has
    // one more bit than 2 or 3.
    mp_limb_t result_limbs[max_size + 1];
    mp_size_t result_len;
    // Now stick in the first integer
    Integer_sp first = gc::As_unsafe<Integer_sp>(oCar(integers));
    if (first.fixnump()) {
      Fixnum ffirst = first.unsafe_fixnum();
      if (ffirst < 0) {
        result_len = -1;
        result_limbs[0] = -ffirst;
      } else {
        result_len = 1;
        result_limbs[0] = ffirst;
      }
    } else {
      // FIXME: switch to As_unsafe once old bignums are gone
      Bignum_sp bfirst = gc::As<Bignum_sp>(first);
      mp_size_t first_len = bfirst->length();
      mp_size_t first_size = std::abs(first_len);
      const mp_limb_t* first_limbs = bfirst->limbs();
      result_len = first_len;
      // FIXME: use memcpy or something
      for (mp_size_t i = 0; i < first_size; ++i)
        result_limbs[i] = first_limbs[i];
    }
    // Now loop.
    next_bit_operator bop = next_operations[op];
    for (auto cur : (List_sp)oCdr(integers)) {
      Integer_sp icur = gc::As_unsafe<Integer_sp>(oCar(cur));
      if (icur.fixnump()) {
        Fixnum fcur = icur.unsafe_fixnum();
        if (fcur < 0) {
          mp_limb_t acur = -fcur;
          result_len = bop(result_limbs, result_limbs, result_len, &acur, -1);
        } else {
          mp_limb_t acur = fcur;
          result_len = bop(result_limbs, result_limbs, result_len, &acur, 1);
        }
      } else {
        // FIXME switch to As_unsafe once etc
        Bignum_sp bcur = gc::As<Bignum_sp>(icur);
        result_len = bop(result_limbs, result_limbs, result_len, bcur->limbs(), bcur->length());
      }
    }
    // Loop over, return result.
    return bignum_result(result_len, result_limbs);
  }
}

Integer_sp next_boole_mixed1(next_bit_operator op, Bignum_sp big, Fixnum small) {
  // FIXME: to As_unsafe once old bignums are gone
  mp_size_t len = big->length();
  mp_limb_t result[std::abs(len) + 1];
  mp_limb_t flimb;
  mp_size_t rlen;
  if (small < 0) {
    flimb = -small;
    rlen = op(result, big->limbs(), len, &flimb, -1);
  } else if (small > 0) {
    flimb = small;
    rlen = op(result, big->limbs(), len, &flimb, 1);
  } else {
    flimb = 0;
    rlen = op(result, big->limbs(), len, &flimb, 0);
  }
  return bignum_result(rlen, result);
}

Integer_sp next_boole_mixed2(next_bit_operator op, Fixnum small, Bignum_sp big) {
  // Ditto FIXME
  mp_size_t len = big->length();
  mp_limb_t result[std::abs(len) + 1];
  mp_limb_t flimb;
  mp_size_t rlen;
  if (small < 0) {
    flimb = -small;
    rlen = op(result, &flimb, -1, big->limbs(), len);
  } else if (small > 0) {
    flimb = small;
    rlen = op(result, &flimb, 1, big->limbs(), len);
  } else {
    flimb = 0;
    rlen = op(result, &flimb, 0, big->limbs(), len);
  }
  return bignum_result(rlen, result);
}

Integer_sp clasp_boole(Fixnum op, Integer_sp i1, Integer_sp i2) {
  if (i1.fixnump()) {
    if (i2.fixnump()) {
      bit_operator bop = fixnum_operations[op];
      return clasp_make_fixnum(bop(i1.unsafe_fixnum(), i2.unsafe_fixnum()));
    } else {
      return next_boole_mixed2(next_operations[op],
                               // FIXME: to As_unsafe once old bignums gone
                               i1.unsafe_fixnum(), gc::As<Bignum_sp>(i2));
    }
  } else {
    if (i2.fixnump()) {
      return next_boole_mixed1(next_operations[op], gc::As<Bignum_sp>(i1), i2.unsafe_fixnum());
    } else {
      Bignum_sp left = gc::As<Bignum_sp>(i1);
      Bignum_sp right = gc::As<Bignum_sp>(i2);
      mp_size_t llen = left->length(), rlen = right->length();
      mp_limb_t result[std::max(std::abs(llen), std::abs(rlen))];
      next_bit_operator bop = next_operations[op];
      mp_size_t res_len = bop(result, left->limbs(), llen, right->limbs(), rlen);
      return bignum_result(res_len, result);
    }
  }
}

/*! Copied from ECL */
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN bool cl__logbitp(Integer_sp index, Integer_sp i) {
  if (index.fixnump()) {
    gc::Fixnum n = index.unsafe_fixnum();
    if (n < 0)
      goto NEGINDEX;
    if (i.fixnump()) {
      gc::Fixnum fi = i.unsafe_fixnum();
      if (n >= FIXNUM_BITS)
        return (fi < 0);
      else
        return ((fi >> n) & 1);
    } else { // TODO: switch to GC_unsafe after bignum switch
      Bignum_sp bi = gc::As<Bignum_sp>(i);
      mp_size_t len = bi->length();
      mp_size_t size = std::abs(len);
      const mp_limb_t* limbs = bi->limbs();
      mp_size_t limb_index = n / mp_bits_per_limb;
      if (limb_index < size) {
        const mp_limb_t* limbptr = &(limbs[limb_index]);
        mp_limb_t limb = *limbptr;
        if (len < 0) {
          limb = -limb; // two's complement
          while (limbptr != limbs) {
            limbptr--;
            if (*limbptr != 0) {
              limb--; // one's complement instead
              break;
            }
          }
        }
        return ((limb >> (n % mp_bits_per_limb)) & 1);
      } else // limb_index >= size: index out of range
        return (len < 0);
    }
  } else {
    if (Real_O::minusp(index))
      goto NEGINDEX;
    // Index is a bignum.
    // We don't support bignums with that many bits, so we're out of range.
    if (Real_O::minusp(i))
      return true;
    else
      return false;
  }
NEGINDEX:
  TYPE_ERROR(index, cl::_sym_UnsignedByte); // (integer 0)
}

CL_LAMBDA(op arg1 arg2);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(boole)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__boole(Fixnum op, Integer_sp arg1, Integer_sp arg2) {
  if ((op < 0) || (op >= boolOpsMax))
    TYPE_ERROR(clasp_make_fixnum(op),
               Cons_O::createList(cl::_sym_Integer_O, clasp_make_fixnum(0), clasp_make_fixnum(boolOpsMax - 1)));
  return clasp_boole(op, arg1, arg2);
};

SYMBOL_EXPORT_SC_(ClPkg, boole_1);
SYMBOL_EXPORT_SC_(ClPkg, boole_2);
SYMBOL_EXPORT_SC_(ClPkg, boole_and);
SYMBOL_EXPORT_SC_(ClPkg, boole_andc1);
SYMBOL_EXPORT_SC_(ClPkg, boole_andc2);
SYMBOL_EXPORT_SC_(ClPkg, boole_c1);
SYMBOL_EXPORT_SC_(ClPkg, boole_c2);
SYMBOL_EXPORT_SC_(ClPkg, boole_clr);
SYMBOL_EXPORT_SC_(ClPkg, boole_eqv);
SYMBOL_EXPORT_SC_(ClPkg, boole_ior);
SYMBOL_EXPORT_SC_(ClPkg, boole_nand);
SYMBOL_EXPORT_SC_(ClPkg, boole_nor);
SYMBOL_EXPORT_SC_(ClPkg, boole_orc1);
SYMBOL_EXPORT_SC_(ClPkg, boole_orc2);
SYMBOL_EXPORT_SC_(ClPkg, boole_set);
SYMBOL_EXPORT_SC_(ClPkg, boole_xor);

void initialize_bits() {
  cl::_sym_boole_1->defconstant(make_fixnum(boole_1));
  cl::_sym_boole_2->defconstant(make_fixnum(boole_2));
  cl::_sym_boole_and->defconstant(make_fixnum(boole_and));
  cl::_sym_boole_andc1->defconstant(make_fixnum(boole_andc1));
  cl::_sym_boole_andc2->defconstant(make_fixnum(boole_andc2));
  cl::_sym_boole_c1->defconstant(make_fixnum(boole_c1));
  cl::_sym_boole_c2->defconstant(make_fixnum(boole_c2));
  cl::_sym_boole_clr->defconstant(make_fixnum(boole_clr));
  cl::_sym_boole_eqv->defconstant(make_fixnum(boole_eqv));
  cl::_sym_boole_ior->defconstant(make_fixnum(boole_ior));
  cl::_sym_boole_nand->defconstant(make_fixnum(boole_nand));
  cl::_sym_boole_nor->defconstant(make_fixnum(boole_nor));
  cl::_sym_boole_orc1->defconstant(make_fixnum(boole_orc1));
  cl::_sym_boole_orc2->defconstant(make_fixnum(boole_orc2));
  cl::_sym_boole_set->defconstant(make_fixnum(boole_set));
  cl::_sym_boole_xor->defconstant(make_fixnum(boole_xor));

  //  af_def(ClPkg, "logbitp", &cl_logbitp);
};

CL_LAMBDA(first second);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(logand_2op)dx");
DOCGROUP(clasp);
CL_DEFUN Integer_sp core__logand_2op(Integer_sp first, Integer_sp second) { return clasp_boole(boole_and, first, second); }

CL_LAMBDA(first second);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(logxor_2op)dx");
DOCGROUP(clasp);
CL_DEFUN Integer_sp core__logxor_2op(Integer_sp first, Integer_sp second) { return clasp_boole(boole_xor, first, second); }

CL_LAMBDA(first second);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(logior_2op)dx");
DOCGROUP(clasp);
CL_DEFUN Integer_sp core__logior_2op(Integer_sp first, Integer_sp second) { return clasp_boole(boole_ior, first, second); }

CL_LAMBDA(first second);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(logeqv_2op)dx");
DOCGROUP(clasp);
CL_DEFUN Integer_sp core__logeqv_2op(Integer_sp first, Integer_sp second) { return clasp_boole(boole_eqv, first, second); }

CL_LAMBDA(&rest integers);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(logand)dx");
DOCGROUP(clasp);
CL_DEFUN Integer_sp cl__logand(List_sp integers) {
  // if the arguments are all fixnum, don't convert everything to mpz, but stay in fixnums
  if (integers.nilp())
    return clasp_make_fixnum(-1);
  else
    return next_operation_rest(boole_and, integers);
};

CL_LAMBDA(&rest integers);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(logior)dx");
DOCGROUP(clasp);
CL_DEFUN Integer_sp cl__logior(List_sp integers) {
  if (integers.nilp())
    return clasp_make_fixnum(0);
  else
    return next_operation_rest(boole_ior, integers);
};

CL_LAMBDA(&rest integers);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(logxor)dx");
DOCGROUP(clasp);
CL_DEFUN Integer_sp cl__logxor(List_sp integers) {
  if (integers.nilp())
    return clasp_make_fixnum(0);
  else
    return next_operation_rest(boole_xor, integers);
};

CL_LAMBDA(&rest integers);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(logeqv)dx");
DOCGROUP(clasp);
CL_DEFUN Integer_sp cl__logeqv(List_sp integers) {
  if (integers.nilp())
    return Integer_O::create((gc::Fixnum)-1);
  else
    return next_operation_rest(boole_eqv, integers);
};

CL_LAMBDA(a b);
CL_DECLARE();
CL_DOCSTRING(R"dx(logandc1)dx");
CL_UNWIND_COOP(true);
DOCGROUP(clasp);
CL_DEFUN Integer_sp cl__logandc1(Integer_sp a, Integer_sp b) { return clasp_boole(boole_andc1, a, b); };

CL_LAMBDA(a b);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(logandc2)dx");
DOCGROUP(clasp);
CL_DEFUN Integer_sp cl__logandc2(Integer_sp a, Integer_sp b) { return clasp_boole(boole_andc2, a, b); };

CL_LAMBDA(a b);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(logorc1)dx");
DOCGROUP(clasp);
CL_DEFUN Integer_sp cl__logorc1(Integer_sp a, Integer_sp b) { return clasp_boole(boole_orc1, a, b); };

CL_LAMBDA(a b);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(logorc2)dx");
DOCGROUP(clasp);
CL_DEFUN Integer_sp cl__logorc2(Integer_sp a, Integer_sp b) { return clasp_boole(boole_orc2, a, b); };

CL_LAMBDA(a);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(lognot)dx");
DOCGROUP(clasp);
CL_DEFUN Integer_sp cl__lognot(Integer_sp a) {
  // KLUDGE
  if (a.fixnump())
    return clasp_make_fixnum(a.unsafe_fixnum() ^ -1);
  else {
    Bignum_sp big = gc::As_unsafe<Bignum_sp>(a);
    mp_size_t len = big->length();
    mp_size_t size = std::abs(len);
    const mp_limb_t* limbs = big->limbs();
    mp_limb_t rlimbs[size + 1];
    mp_size_t result_len = next_com(rlimbs, limbs, len);
    return bignum_result(result_len, rlimbs);
  }
};

CL_LAMBDA(a b);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(lognand)dx");
DOCGROUP(clasp);
CL_DEFUN Integer_sp cl__lognand(Integer_sp a, Integer_sp b) { return clasp_boole(boole_nand, a, b); };

CL_LAMBDA(a b);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(lognor)dx");
DOCGROUP(clasp);
CL_DEFUN Integer_sp cl__lognor(Integer_sp a, Integer_sp b) { return clasp_boole(boole_nor, a, b); };

}; // namespace core
