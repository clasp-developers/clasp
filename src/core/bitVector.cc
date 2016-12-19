/*
    File: bitVector.cc
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
#define DEBUG_LEVEL_NONE

#include <clasp/core/common.h>
#include <clasp/core/bitVector.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/wrappers.h>

namespace core {

BitVector_O::BitVector_O(size_t dimension, bool init_value, T_sp fillPointer, BitVector_sp displacedTo, cl_index displacedIndexOffset)
  : Base(),
    _Dimension(dimension),
    _FillPointer(fillPointer),
    _Bits(),
    _DisplacedTo(displacedTo),
    _DisplacedIndexOffset(displacedIndexOffset) {
  if (displacedTo.nilp()) {
    // allocate data
    cl_index capacity = BitVectorWords(dimension);
    BitBlockType init_bits = init_value ? ~0 : 0;
    this->_Bits.allocate(init_bits,capacity);
  } else {
    this->_DisplacedTo = gc::As<BitVector_sp>(displacedTo);
  }
};

void BitVector_O::rowMajorAset(cl_index idx, T_sp value) {
  ASSERTF(idx < this->length(), BF("Index %d is out of range (<%d)") % idx % this->length());
  Fixnum_sp fn = gc::As<Fixnum_sp>(value);
  this->setBit(idx, fn.unsafe_fixnum());
}

T_sp BitVector_O::rowMajorAref(cl_index idx) const {
  ASSERTF(idx < this->length(), BF("Index %d is out of range (<%d)") % idx % this->length());
  uint val = this->testBit(idx);
  return (val != 0) ? clasp_make_fixnum(1) : clasp_make_fixnum(0);
}

void BitVector_O::getOnIndices(vector<cl_index> &res) {
  cl_index i;
  res.clear();
  for (i = 0; i != this->length(); i++) {
    if (this->testBit(i)) {
      res.push_back(i);
    }
  }
}

bool BitVector_O::equal(T_sp obv) const {
  uint i;
  if (!gc::IsA<BitVector_sp>(obv)) return false;
  BitVector_sp bv = gc::As<BitVector_sp>(obv);
  if (this->length() != bv->length()) return false;
  for (i = 0; i < this->length(); ++i) {
    if (this->testBit(i) != bv->testBit(i)) return false;
  }
  return true;
}

void BitVector_O::erase() {
  for (cl_index i(0), iEnd(this->length()); i<iEnd; ++i ) {
    this->setBit(i,0);
  }
}

CL_LISPIFY_NAME("core:setBit");
CL_DEFMETHOD void BitVector_O::setBit(cl_index i, uint v) {
  if (this->_DisplacedTo) {
    IMPLEMENT_MEF(BF("Handle setting bits in displaced bit-vectors"));
  }
  cl_index block;
  cl_index offset;
  BitBlockType packedVal;
  BitBlockType mask;
  BitBlockType omask;
  if (i >= this->dimension()) {
    SIMPLE_ERROR(BF("BitVector index overflow"));
  }
  cl_index idx = this->_DisplacedIndexOffset + i;
  block = idx / BitWidth;
  offset = idx % BitWidth;
  omask = ~0;
  mask = (1 << offset) ^ omask;
  packedVal = v << offset;
  this->_Bits[block] = (this->_Bits[block] & mask) | packedVal;
}

CL_LISPIFY_NAME("core:testBit");
CL_DEFMETHOD uint BitVector_O::testBit(cl_index i) const {
  if (this->_DisplacedTo) {
    IMPLEMENT_MEF(BF("Handle test bits in displaced bit-vectors"));
  }
  cl_index block;
  cl_index offset;
  BitBlockType mask;
  cl_index idx = this->_DisplacedIndexOffset + i;
  block = idx / BitWidth;
  offset = idx % BitWidth;
  mask = (1 << offset);
  LOG(BF("testBit i=%u BitWidth=%d block=%d offset=%d") % (i) % BitWidth % (block) % (offset));
  LOG(BF("      mask = |%lx|") % mask);
  LOG(BF("bits[%04d] = |%lx|") % block % this->bits[block]);
  BitBlockType result = (this->_Bits[block] & mask);
  LOG(BF("    result = |%lx|") % result);
  return ((result ? 1 : 0));
}

CL_LISPIFY_NAME("core:inPlaceOr");
CL_DEFMETHOD void BitVector_O::inPlaceOr(BitVector_sp bv) {
  if (this->length() != bv->length()) {
    SIMPLE_ERROR(BF("BitVectors aren't the same length for inPlaceOr"));
  }
  if (!this->displacedp() && !bv->displacedp()) {
    for (cl_index i = 0; i != this->_Bits.size(); i++) {
      this->_Bits[i] |= bv->_Bits[i];
    }
  } else {
    // One or both of the BitVector_O's are displaced - do this bit by bit.
    for (cl_index i=0; i<this->length(); ++i ) {
      this->setBit(i,this->testBit(i) | bv->testBit(i));
    }
  }
}

CL_LISPIFY_NAME("core:inPlaceAnd");
CL_DEFMETHOD void BitVector_O::inPlaceAnd(BitVector_sp bv) {
  if (this->length() != bv->length()) {
    SIMPLE_ERROR(BF("BitVectors aren't the same length for inPlaceOr"));
  }
  if (!this->_DisplacedTo && !bv->_DisplacedTo) {
    cl_index i;
    for (i = 0; i != this->_Bits.size(); i++) {
      this->_Bits[i] &= bv->_Bits[i];
    }
  } else {
    // One or both of the BitVector_O's are displaced - do this bit by bit.
    for (cl_index i=0; i<this->length(); ++i ) {
      this->setBit(i,this->testBit(i) & bv->testBit(i));
    }
  }
}

CL_LISPIFY_NAME("core:inPlaceXor");
CL_DEFMETHOD void BitVector_O::inPlaceXor(BitVector_sp bv) {
  if (this->length() != bv->length()) {
    SIMPLE_ERROR(BF("BitVectors aren't the same length for inPlaceOr"));
  }
  if (!this->_DisplacedTo && !bv->_DisplacedTo) {
    cl_index i;
    for (i = 0; i != this->_Bits.size(); i++) {
      this->_Bits[i] ^= bv->_Bits[i];
    }
  } else {
    // One or both of the BitVector_O's are displaced - do this bit by bit.
    for (cl_index i=0; i<this->length(); ++i ) {
      this->setBit(i,this->testBit(i) ^ bv->testBit(i));
    }
  }
}

CL_LISPIFY_NAME("core:bitOr");
CL_DEFMETHOD BitVector_sp BitVector_O::bitOr(BitVector_sp bv) {
  BitVector_sp res = gc::As<BitVector_sp>(this->deepCopy());
  res->inPlaceOr(bv);
  return ((res));
}

CL_LISPIFY_NAME("core:bitAnd");
CL_DEFMETHOD BitVector_sp BitVector_O::bitAnd(BitVector_sp bv) {
  BitVector_sp res = gc::As<BitVector_sp>(this->deepCopy());
  res->inPlaceAnd(bv);
  return ((res));
}

CL_LISPIFY_NAME("core:bitXor");
CL_DEFMETHOD BitVector_sp BitVector_O::bitXor(BitVector_sp bv) {
  BitVector_sp res = gc::As<BitVector_sp>(this->deepCopy());
  res->inPlaceXor(bv);
  return ((res));
}

CL_LISPIFY_NAME("core:countSet");
CL_DEFMETHOD cl_index BitVector_O::countSet() {
  cl_index c;
  c = 0;
  for (cl_index i = 0; i < this->length(); ++i) {
    if (this->testBit(i)) ++c;
  }
  return c;
}

CL_LISPIFY_NAME("core:BitVector-asString");
CL_DEFMETHOD string BitVector_O::asString() {
  stringstream s;
  for (cl_index i = 0; i < this->length(); ++i) {
    s << this->testBit(i);
  }
  return s.str();
}

T_sp BitVector_O::deepCopy() const {
  IMPLEMENT_MEF(BF("Make sure BitVector_O::deepCopy makes a copy of the bits if its not displaced!!!"));
  GC_COPY(BitVector_O, n, *this);
  if (!n->_DisplacedTo) {
    cl_index capacity = BitVectorWords(n->dimension());
    BitBlockType init_bits = 0;
    n->_Bits.allocate(init_bits,capacity);
    memcpy(&n->_Bits[0],&this->_Bits[0],capacity*sizeof(BitBlockType));
  }
  return (n);
}


//
//	dumpToStream
//
//	Dump the BitVector to a stream
//
std::ostream &BitVector_O::dumpToStream(std::ostream &out) {
  cl_index i;
  for (i = 0; i < this->vector_length(); i++) {
    out << this->testBit(i);
  }
  out << std::endl;
  return out;
}

//
//	dump
//
//	Dump the BitVector to a stream
//
CL_LISPIFY_NAME("core:dump");
CL_DEFMETHOD void BitVector_O::dump() {
  this->dumpToStream(std::cout);
}

void BitVector_O::sxhash_(HashGenerator &hg) const {
  Bignum bn;
  for (cl_index i = 0; i < this->length(); ++i) {
    if (this->testBit(i)) {
      mpz_setbit(bn.get_mpz_t(), i);
    }
  }
  hg.addPart(bn);
}

CL_LISPIFY_NAME("core:lowestIndex");
CL_DEFMETHOD cl_index BitVector_O::lowestIndex() {
  cl_index i;
  for ( i = 0; i < this->length(); i++) {
    if (this->testBit(i)) {
      return i;
    }
  }
  return i;
}

void BitVector_O::fillPointerSet(T_sp fp)
{
  if (fp.fixnump()) {
    cl_index ifp = fp.unsafe_fixnum();
    if (ifp >= 0 && ifp <= this->dimension()) {
      this->_FillPointer =fp;
      return;
    }
  } else if (fp.nilp()) {
    this->_FillPointer = fp;
    return;
  }
  SIMPLE_ERROR(BF("Illegal fill-pointer %d - must be less than %d or nil") % fp % this->length());
}


void BitVector_O::adjust(bool init_bit, size_t dimension )
{
  ASSERT(!this->displacedp());
  cl_index capacity = BitVectorWords(dimension);
  BitBlockType init_val = 0;
  if (init_bit) {
    init_val = ~0;
  }
  LOG(BF("Resizing BitVector_O to %lu") % capacity);
  this->_Bits.resize(capacity,init_val);
}


T_sp BitVector_O::vectorPush(T_sp newElement) {
  unlikely_if (!newElement.fixnump()) {
    TYPE_ERROR(newElement, cl::_sym_bit);
  }
  if (!this->_FillPointer.fixnump()) noFillPointerError();
  cl_index idx = this->_FillPointer.unsafe_fixnum();
  if (idx < this->_Dimension) {
    this->setBit(idx,newElement.unsafe_fixnum());
    this->_FillPointer = clasp_make_fixnum(idx+1);
    return clasp_make_fixnum(idx);
  }
  return _Nil<T_O>();
}

Fixnum_sp BitVector_O::vectorPushExtend(T_sp newElement, cl_index extension) {
  unlikely_if (!newElement.fixnump()) {
    TYPE_ERROR(newElement, cl::_sym_bit);
  }
  unlikely_if (!this->_FillPointer.fixnump()) noFillPointerError();
  cl_index idx = this->_FillPointer.unsafe_fixnum();
  unlikely_if (idx >= this->_Dimension) {
    if (extension <= 0) extension = 64;
  }
  cl_index new_size = this->_Dimension+extension;
  unlikely_if (!cl::_sym_adjust_array->boundP()) {
    this->adjust(false,new_size);
  } else {
    eval::funcall(cl::_sym_adjust_array,this->asSmartPtr(),clasp_make_fixnum(new_size),cl::_sym_fill_pointer, this->_FillPointer);
  }
  this->setBit(idx,newElement.unsafe_fixnum());
  this->_FillPointer = clasp_make_fixnum(idx+1);
  return make_fixnum(idx);
}


BitVector_sp make_bit_vector(bool init_value,
                             size_t dimension,
                             bool adjustable,
                             T_sp fill_pointer,
                             T_sp displaced_to,
                             cl_index displaced_index_offset)
{
  // Allocate different kinds of bit-vectors depending on arguments.
  // currently adjustable
  GC_ALLOCATE_VARIADIC(BitVector_O,bv,dimension,init_value,fill_pointer,displaced_to,displaced_index_offset);
  return bv;
}










// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
//
// Depreciated code
//
#if 0
void BitVector_O::do_subseq(BaseBitVector_sp result, cl_index start, cl_index iend) const {
  cl_index idest = 0;
  for (cl_index i(start); i < iend; ++i) {
    result->setBit(idest++, this->testBit(i));
  }
}


CL_PKG_NAME(CorePkg,make-simple-bit-vector);
CL_DEFUN SimpleBitVector_sp SimpleBitVector_O::make(size_t size,int init_bit) {
  GC_ALLOCATE_VARIADIC(SimpleBitVector_O, sbv, size,init_bit);
  return sbv;
}

CL_PKG_NAME(CorePkg,make-bit-vector-with-fill-ptr);
CL_DEFUN BitVectorWithFillPtr_sp BitVectorWithFillPtr_O::make(size_t size, size_t fill_ptr, bool adjust,int init_bit) {
  GC_ALLOCATE_VARIADIC(BitVectorWithFillPtr_O, sbv, size, fill_ptr, adjust, init_bit);
  return sbv;
}


Fixnum_sp BitVectorWithFillPtr_O::vectorPushExtend(T_sp newElement, cl_index extension) {
  if (!this->_adjustable) {
    SIMPLE_ERROR(BF("This bit-vector is not extensible"));
  }
  if (!newElement.fixnump()) {
    TYPE_ERROR(newElement, cl::_sym_bit);
  }
  Fixnum b = newElement.unsafe_fixnum();
  if (b != 0 && b != 1) {
    TYPE_ERROR(newElement, cl::_sym_bit);
  }
  this->setBit(this->_fill_ptr, b);
  ++this->_fill_ptr;
  if (this->_fill_ptr > this->BitVector_O::dimension()) {
    this->bits.resize((this->_fill_ptr - 1 + extension) / BitWidth + 1, 0);
  }
  return clasp_make_fixnum(this->_fill_ptr - 1);
}

#endif



#if 0
// ------------------------------------------------------------
// ------------------------------------------------------------
// ------------------------------------------------------------
// ------------------------------------------------------------
// ------------------------------------------------------------
// ------------------------------------------------------------
//
//  Move into BitVector
//

T_sp SimpleBitVector_O::subseq(cl_index start, T_sp end) const {
  if (start < 0) {
    SIMPLE_ERROR(BF("Illegal start %d for subseq") % start);
  }
  cl_index iend;
  if (end.nilp()) {
    iend = this->dimension();
  } else {
    iend = unbox_fixnum(gc::As<Fixnum_sp>(end));
  }
  if (iend < start) {
    SIMPLE_ERROR(BF("The limits %d and %d are bad for a string of %d characters") % start % iend % this->dimension());
  }
  if (iend > this->dimension()) {
    iend = this->dimension();
  }
  cl_index ilen = iend - start;
  SimpleBitVector_sp result = SimpleBitVector_O::make(ilen);
  this->do_subseq(result, start, iend);
  return result;
}

T_sp BitVectorWithFillPtr_O::subseq(cl_index start, T_sp end) const {
  if (start < 0) {
    SIMPLE_ERROR(BF("Illegal start %d for subseq") % start);
  }
  cl_index iend;
  if (end.nilp()) {
    iend = this->dimension();
  } else {
    iend = unbox_fixnum(gc::As<Fixnum_sp>(end));
  }
  if (iend < start) {
    SIMPLE_ERROR(BF("The limits %d and %d are bad for a string of %d characters") % start % iend % this->dimension());
  }
  if (iend > this->dimension()) {
    iend = this->dimension();
  }
  cl_index ilen = iend - start;
  BitVectorWithFillPtr_sp result = BitVectorWithFillPtr_O::make(ilen, ilen, this->_adjustable);
  this->do_subseq(result, start, iend);
  return result;
}
#endif

};
