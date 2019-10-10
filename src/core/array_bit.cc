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
    if (bvx.testBit(ix) != bvy.testBit(iy)) return false;
  }
  return true;
}

Array_sp ranged_bit_vector_reverse(SimpleBitVector_sp sv, size_t start, size_t end) {
  gctools::smart_ptr<SimpleBitVector_O> rev = SimpleBitVector_O::make(end-start);
  for (size_t i(0),iEnd(end-start); i<iEnd; ++i ) {
    rev->setBit(i,sv->testBit(end-1-i));
  }
  return rev;
}

void ranged_bit_vector_nreverse(SimpleBitVector_sp sv, size_t start, size_t end) {
  size_t middle = (end-start)/2;
  for (size_t i(start),iEnd(start+end-1),iOff(0); i<middle; ++i,--iEnd ) {
    uint x = sv->testBit(i);
    uint y = sv->testBit(iEnd);
    sv->setBit(iEnd,x);
    sv->setBit(i,y);
  }
}

SimpleBitVector_sp SimpleBitVector_O::make(const string& bv) {
  size_t dim = bv.size()-2;
  SimpleBitVector_sp x = SimpleBitVector_O::make(dim);
  for (int i = 0; i<dim; i++) {
    char elt = bv[i+2];
    x->setBit(i,elt-'0');
  }
  return x;
}

// Restored by drmeister because Cando uses this function
//  we can remove it if there is a problem with it or it is redundant.
SimpleBitVector_sp SimpleBitVector_copy(SimpleBitVector_sp orig_sbv)
{
  size_t value_type_size = core::SimpleBitVector_O::bitunit_array_type::sizeof_for_length(orig_sbv->length())/sizeof(core::SimpleBitVector_O::value_type);
  core::SimpleBitVector_sp sbv = core::SimpleBitVector_O::make(orig_sbv->length(),0,true,value_type_size,&orig_sbv->_Data[0]);
  return sbv;
}

Array_sp SimpleBitVector_O::unsafe_subseq(size_t start, size_t end) const {
  BOUNDS_ASSERT(0<=start&&start<end&&end<=this->length());
  SimpleBitVector_sp sbv = SimpleBitVector_O::make(end-start);
  for (size_t i(0),iEnd(end-start);i<iEnd;++i) {
    sbv->setBit(i,this->testBit(start+i));
  }
  return sbv;
}

Array_sp SimpleBitVector_O::unsafe_setf_subseq(size_t start, size_t end, Array_sp other) {
  BOUNDS_ASSERT(0<=start&&start<end&&end<=this->length());
  if (SimpleBitVector_sp sbv = other.asOrNull<SimpleBitVector_O>()) {
      // TODO: Write specialized versions of this to speed it up
    for ( size_t i(start),ni(0); i<end; ++i,++ni ) {
      this->setBit(i,sbv->testBit(ni));
    }
    return this->asSmartPtr();
  } else if (BitVectorNs_sp bv = other.asOrNull<BitVectorNs_O>()) {
    AbstractSimpleVector_sp bsv;
    size_t ostart, oend;
    bv->asAbstractSimpleVectorRange(bsv,ostart,oend);
    SimpleBitVector_sp sbv = gc::As_unsafe<SimpleBitVector_sp>(bsv);
    for ( size_t i(start),io(ostart); i<end; ++i, ++io) {
      this->setBit(i,sbv->testBit(io));
    }
    return this->asSmartPtr();
  }
  TYPE_ERROR(other,cl::_sym_bit_vector);
}

// This does not work properly
/*
void SimpleBitVector_O::unsafe_fillArrayWithElt(T_sp initialElement, size_t start, size_t end)
{
  value_type initBlockValue = (initialElement.nilp()) ? 0 : ~0;
  // round up start and round down end
  size_t blockStart = ((start+BitWidth-1)/BitWidth);
  size_t blockEnd = (end+BitWidth-1)/BitWidth;
  for (size_t i(start), iEnd(blockStart*BitWidth); i<iEnd; ++i ) {
    this->setBit(i,initBlockValue);
  }
  for (size_t i(blockStart),iEnd(blockEnd); i<iEnd; ++i ) {
    this->_Data[i] = initBlockValue;
  }
  for (size_t i(blockEnd*BitWidth), iEnd(end); i<iEnd; ++i ) {
    this->setBit(i,initBlockValue);
  }
};
*/

void SimpleBitVector_O::unsafe_fillArrayWithElt(T_sp initialElement, size_t start, size_t end) {
  if (CLASP_FIXNUMP (initialElement))
  {
    Fixnum zero_or_one = gc::As<core::Fixnum_sp>(initialElement).unsafe_fixnum();
    if ((zero_or_one == 0) || (zero_or_one == 1)) {
      for (size_t i(start),iEnd(end); i<iEnd; ++i) {
        this->setBit(i, zero_or_one);
      }
    }
    else TYPE_ERROR(initialElement, cl::_sym_bit);
  }
  else TYPE_ERROR(initialElement, cl::_sym_bit);
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

Array_sp SimpleBitVector_O::reverse() const {
  return ranged_bit_vector_reverse(this->asSmartPtr(),0,this->length());
}

Array_sp SimpleBitVector_O::nreverse() {
  ranged_bit_vector_nreverse(this->asSmartPtr(),0,this->length());
  return this->asSmartPtr();
}

void SimpleBitVector_inPlaceOr(SimpleBitVector_sp x, SimpleBitVector_sp y) {
  size_t i;
  if (x->length() != y->length()) SIMPLE_ERROR(BF("BitVectors aren't the same length for in place or - lengths are %d and %d") % x->length() % y->length());
  for (size_t i = 0; i<x->_Data.number_of_words(); ++i ) {
    (*x)._Data[i] |= (*y)._Data[i];
  }
}

void SimpleBitVector_inPlaceAnd(SimpleBitVector_sp x, SimpleBitVector_sp y) {
  size_t i;
  if (x->length() != y->length()) SIMPLE_ERROR(BF("BitVectors aren't the same length for operation"));
  for (size_t i = 0; i<x->_Data.number_of_words(); ++i ) {
    (*x)._Data[i] &= (*y)._Data[i];
  }
}

void SimpleBitVector_inPlaceXor(SimpleBitVector_sp x, SimpleBitVector_sp y) {
  size_t i;
  if (x->length() != y->length()) SIMPLE_ERROR(BF("BitVectors aren't the same length for operation"));
  for (size_t i = 0; i<x->_Data.number_of_words(); ++i ) {
    (*x)._Data[i] ^= (*y)._Data[i];
  }
}

size_t SimpleBitVector_lowestIndex(SimpleBitVector_sp x) {
  size_t word_length = x->length()/SimpleBitVector_O::BitWidth;
  size_t ib = 0;
  size_t iw = 0;
  while (iw<word_length && x->_Data[iw]==0) {
    ++iw;
    ib = ib + SimpleBitVector_O::BitWidth;
  }
  for ( ; ib < x->length(); ib++) {
    if (x->testBit(ib)) {
      return ib;
    }
  }
  return ib;
}

void SimpleBitVector_getOnIndices(SimpleBitVector_sp x, vector<size_t> &res) {
  size_t i;
  res.clear();
  i = SimpleBitVector_lowestIndex(x);
  for (; i != x->length(); i++) {
    if (x->testBit(i)) {
      res.push_back(i);
    }
  }
}

// FIXME: This is redundant with respect to core__sbv_zerop (bits.cc)
bool SimpleBitVector_isZero(SimpleBitVector_sp vec) {
  bit_array_word* bytes = vec->bytes();
  size_t len = vec->length();
  size_t nwords = len / BIT_ARRAY_WORD_BITS;
  size_t leftover = len % BIT_ARRAY_WORD_BITS;
  for (size_t i = 0; i < nwords; ++i) if (bytes[i] != 0) return false;
  if (leftover != 0) {
    bit_array_word unshifted_mask = (1 << leftover) - 1;
    bit_array_word mask = unshifted_mask << (BIT_ARRAY_WORD_BITS - leftover);
    if ((bytes[nwords] & mask) != 0) return false;
  }
  return true;
}

Fixnum_sp BitVectorNs_O::vectorPushExtend(T_sp newElement, size_t extension) {
  unlikely_if (!this->_Flags.fillPointerP()) noFillPointerError(cl::_sym_vectorPushExtend,this->asSmartPtr());
  cl_index idx = this->_FillPointerOrLengthOrDummy;
  unlikely_if (idx >= this->_ArrayTotalSize) {
    unlikely_if (this->displacedToP()) {
    // The array needs to be resized because it's displaced
      if (extension <= 0) extension = calculate_extension(this->_ArrayTotalSize);
      cl_index new_size = this->_ArrayTotalSize+extension;
      this->internalAdjustSize_(new_size);
    } else {
      size_t bytes_for_ArrayTotalSizeP1 = SimpleBitVector_O::bitunit_array_type::sizeof_for_length(this->_ArrayTotalSize+1);
      size_t bytes_for_ArrayTotalSize = SimpleBitVector_O::bitunit_array_type::sizeof_for_length(this->_ArrayTotalSize);
      if (bytes_for_ArrayTotalSizeP1 > bytes_for_ArrayTotalSize) { // or it needs more words to store the bits
    // The array needs to be resized because there aren't enough bits to hold the next bit
        if (extension <= 0) extension = calculate_extension(this->_ArrayTotalSize);
        cl_index new_size = this->_ArrayTotalSize+extension;
        this->internalAdjustSize_(new_size);
      } else {
    // There were enough bits to handle the extend
        this->_ArrayTotalSize = idx+1;
        this->_Dimensions[0] = idx+1;
      }
    }
  }
  this->_Data->rowMajorAset(idx+this->_DisplacedIndexOffset,newElement);
  ++this->_FillPointerOrLengthOrDummy;
  return make_fixnum(idx);
}

void BitVectorNs_O::internalAdjustSize_(size_t size, T_sp initElement, bool initElementSupplied) {
  if (size == this->_ArrayTotalSize) return;
  AbstractSimpleVector_sp basesv;
  size_t start, end;
  this->asAbstractSimpleVectorRange(basesv,start,end);
  gctools::smart_ptr<simple_type> sv = gc::As_unsafe<gctools::smart_ptr<simple_type>>(basesv);
  size_t initialContentsSize = MIN(this->length(),size);
  gc::smart_ptr<simple_type> newData = simple_type::make(size,0,true);
  for (size_t i(0),iEnd(initialContentsSize); i<iEnd; ++i ) {
    newData->setBit(i,sv->testBit(i+start));
  }
  this->set_data(newData);
  this->_ArrayTotalSize = size;
  this->_Dimensions[0] = size;
  if (!this->_Flags.fillPointerP()) this->_FillPointerOrLengthOrDummy = size;
  this->_DisplacedIndexOffset = 0;
  this->_Flags.set_displacedToP(false);
}

Array_sp BitVectorNs_O::reverse() const {
  AbstractSimpleVector_sp basesv;
  size_t start, end;
  this->asAbstractSimpleVectorRange(basesv,start,end);
  gctools::smart_ptr<simple_type> sv = gc::As_unsafe<gctools::smart_ptr<simple_type>>(basesv);
  return ranged_bit_vector_reverse(sv,start,end);
}

Array_sp BitVectorNs_O::nreverse() {
  AbstractSimpleVector_sp basesv;
  size_t start, end;
  this->asAbstractSimpleVectorRange(basesv,start,end);
  gctools::smart_ptr<simple_type> sv = gc::As_unsafe<gctools::smart_ptr<simple_type>>(basesv);
  ranged_bit_vector_nreverse(sv,start,end);
  return this->asSmartPtr();
}

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
