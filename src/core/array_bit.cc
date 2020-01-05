/*
    File: array_bit.cc
*/

// Functions specific to bit arrays.
// NOTE: creation through make-array is still in array.cc.

#include <clasp/core/foundation.h>
#include <clasp/core/array.h>

namespace core {
void bitVectorDoesntSupportError() {
  SIMPLE_ERROR(BF("You tried to invoke a method that bit-vector doesn't support on a bit-vector"));
}

bool ranged_bit_vector_EQ_(const SimpleBitVector_O& bvx,const SimpleBitVector_O& bvy, size_t startx, size_t endx, size_t starty, size_t endy) {
  size_t lenx = endx - startx;
  size_t leny = endy - starty;
  if (lenx!=leny) return false;
  for (size_t ix(startx), iy(starty); ix<endx; ++ix, ++iy ) {
    if (bvx[ix] != bvy[iy]) return false;
  }
  return true;
}

SimpleBitVector_sp SimpleBitVector_O::make(const string& bv) {
  size_t dim = bv.size()-2;
  SimpleBitVector_sp x = SimpleBitVector_O::make(dim);
  for (int i = 0; i<dim; i++) {
    char elt = bv[i+2];
    (*x)[i] = elt - '0';
  }
  return x;
}

// Used by Cando.
SimpleBitVector_sp SimpleBitVector_copy(SimpleBitVector_sp orig_sbv)
{ return orig_sbv->copy(orig_sbv->length(), SimpleBitVector_O::default_initial_element(), false);
}

bool SimpleBitVector_O::equal(T_sp other) const {
  if (this == &*other) return true;
  if (SimpleBitVector_sp sbv = other.asOrNull<SimpleBitVector_O>()) {
    if (this->length()!=sbv->length()) return false;
    return ranged_bit_vector_EQ_(*this,*sbv,0,this->length(),0,sbv->length());
  } else if (BitVectorNs_sp bvns = other.asOrNull<BitVectorNs_O>()) {
    if (this->length()!=bvns->length()) return false;
    AbstractSimpleVector_sp sv;
    size_t start, end;
    bvns->asAbstractSimpleVectorRange(sv,start,end);
    SimpleBitVector_O& sbv = *gc::As<SimpleBitVector_sp>(sv);
    return ranged_bit_vector_EQ_(*this,sbv,0,this->length(),start,end);
  }
  return false;
}

// The division is length/BIT_ARRAY_WORD_BITS, but rounding up.
#define DEF_SBV_BIT_OP(name, form)\
  CL_DEFUN SimpleBitVector_sp core__sbv_bit_##name(SimpleBitVector_sp a, SimpleBitVector_sp b,\
                                                   SimpleBitVector_sp r, size_t length) { \
    bit_array_word *ab, *bb, *rb;\
    ab = a->bytes(); bb = b->bytes(); rb = r->bytes();\
    size_t nwords = length/BIT_ARRAY_WORD_BITS + ((length % BIT_ARRAY_WORD_BITS == 0) ? 0 : 1);\
    for (size_t i = 0; i < nwords; ++i) rb[i] = form;\
    return r;\
  }
DEF_SBV_BIT_OP(and, ab[i] & bb[i])
DEF_SBV_BIT_OP(ior, ab[i] | bb[i])
DEF_SBV_BIT_OP(xor, ab[i] ^ bb[i])
DEF_SBV_BIT_OP(nand, ~(ab[i] & bb[i]))
DEF_SBV_BIT_OP(nor, ~(ab[i] | bb[i]))
DEF_SBV_BIT_OP(eqv, ~(ab[i] ^ bb[i]))
DEF_SBV_BIT_OP(andc1, ~(ab[i]) & bb[i])
DEF_SBV_BIT_OP(andc2, ab[i] & ~(bb[i]))
DEF_SBV_BIT_OP(orc1, ~(ab[i]) | bb[i])
DEF_SBV_BIT_OP(orc2, ab[i] | ~(bb[i]))

CL_DEFUN SimpleBitVector_sp core__sbv_bit_not(SimpleBitVector_sp vec, SimpleBitVector_sp res,
                                              size_t length) {
  bit_array_word *vecb, *resb;
  vecb = vec->bytes(); resb = res->bytes();
  size_t nwords = length/BIT_ARRAY_WORD_BITS + ((length % BIT_ARRAY_WORD_BITS == 0) ? 0 : 1);
  for (size_t i = 0; i < nwords; ++i) resb[i] = ~(vecb[i]);
  return res;
}

/* This macro handles iterating over a single bit vector efficiently.
 * The first argument is a SimpleBitVector_sp. The second and third are variables, and
 * the fourth is zero or more statements. The statements will be executed in such that
 * the first variable is bound to the successive bit_array_words of the bit vector, and
 * the second is bound to the index of the word in the bit vector's words.
 */
#define DO_BIT_ARRAY_WORDS(vec, word, i, statement)                                    \
  do {                                                                                 \
    bit_array_word* bytes = vec->bytes();                                              \
    bit_array_word word;                                                               \
    size_t len = vec->length();                                                        \
    size_t nwords = len / BIT_ARRAY_WORD_BITS;                                         \
    size_t leftover = len % BIT_ARRAY_WORD_BITS;                                       \
    size_t i;                                                                          \
    for (i = 0; i < nwords; ++i) { word = bytes[i]; statement}                         \
    i = nwords;                                                                        \
    if (leftover != 0) {                                                               \
      bit_array_word mask = ((1 << leftover) - 1) << (BIT_ARRAY_WORD_BITS - leftover); \
      word = bytes[i] & mask;                                                          \
      statement}                                                                       \
  } while (0);

// Population count for simple bit vector.
CL_DEFUN Integer_sp core__sbv_popcnt(SimpleBitVector_sp vec) {
  ASSERT(sizeof(bit_array_word) == sizeof(unsigned long long)); // for popcount. FIXME
  gctools::Fixnum result = 0;
  DO_BIT_ARRAY_WORDS(vec, word, i, result += bit_array_word_popcount(word););
  return make_fixnum(result);
}

CL_DEFUN bool core__sbv_zerop(SimpleBitVector_sp vec) {
  DO_BIT_ARRAY_WORDS(vec, word, i, if (word != 0) return false;);
  return true;
}

// Returns the index of the first 1 in the bit vector, or NIL.
CL_DEFUN T_sp core__sbv_position_one(SimpleBitVector_sp v) {
  DO_BIT_ARRAY_WORDS(v, w, i,
                     if (w != 0)
                       return make_fixnum(i*BIT_ARRAY_WORD_BITS + bit_array_word_clz(w)););
  return _Nil<T_O>();
}

// The following SimpleBitVector_ functions are used in Cando.
// TODO: Rethink API - some are now redundant wrt the above sbv_* functions.

void SimpleBitVector_inPlaceOr(SimpleBitVector_sp x, SimpleBitVector_sp y) {
  size_t len = x->length();
  if (len != y->length())
    SIMPLE_ERROR(BF("BitVectors aren't the same length for in place or - lengths are %d and %d")
                 % len % y->length());
  core__sbv_bit_ior(x, y, x, len);
}

void SimpleBitVector_inPlaceAnd(SimpleBitVector_sp x, SimpleBitVector_sp y) {
  size_t len = x->length();
  if (len != y->length())
    SIMPLE_ERROR(BF("BitVectors aren't the same length for operation"));
  core__sbv_bit_and(x, y, x, len);
}

void SimpleBitVector_inPlaceXor(SimpleBitVector_sp x, SimpleBitVector_sp y) {
  size_t len = x->length();
  if (len != y->length())
    SIMPLE_ERROR(BF("BitVectors aren't the same length for operation"));
  core__sbv_bit_xor(x, y, x, len);
}

size_t SimpleBitVector_lowestIndex(SimpleBitVector_sp x) { return core__sbv_position_one(x); }

void SimpleBitVector_getOnIndices(SimpleBitVector_sp x, vector<size_t> &res) {
  size_t i;
  res.clear();
  i = SimpleBitVector_lowestIndex(x);
  for (; i != x->length(); i++) {
    if ((*x)[i]) {
      res.push_back(i);
    }
  }
}

bool SimpleBitVector_isZero(SimpleBitVector_sp vec) { return core__sbv_zerop(vec); }

bool BitVectorNs_O::equal(T_sp other) const {
  if (&*other==this) return true;
  if (!other.generalp()) return false;
  if (BitVectorNs_sp strns = other.asOrNull<BitVectorNs_O>()) {
    AbstractSimpleVector_sp bme;
    size_t mstart, mend;
    this->asAbstractSimpleVectorRange(bme,mstart,mend);
    simple_type* me = reinterpret_cast<simple_type*>(&*bme);
    AbstractSimpleVector_sp bso;
    size_t ostart, oend;
    strns->asAbstractSimpleVectorRange(bso,ostart,oend);
    simple_type* so = reinterpret_cast<simple_type*>(&*bso);
    return ranged_bit_vector_EQ_(*me,*so,mstart,mend,ostart,oend);
  } else if (SimpleBitVector_sp ss = other.asOrNull<SimpleBitVector_O>()) {
    AbstractSimpleVector_sp bme;
    size_t mstart, mend;
    this->asAbstractSimpleVectorRange(bme,mstart,mend);
    simple_type* me = reinterpret_cast<simple_type*>(&*bme);
    return ranged_bit_vector_EQ_(*me,*ss,mstart,mend,0,ss->length());
  }
  return false;
};

}; // namespace core
