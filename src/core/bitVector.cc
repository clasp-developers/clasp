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
#include <clasp/core/hashTable.h>
#include <clasp/core/wrappers.h>

namespace core {

//
// Constructor
//
BitVector_O::BitVector_O(size_t sz) {
  this->bits.resize(((sz - 1) / CHAR_BIT) + 1, 0);
}

BitVector_O::BitVector_O(const BitVector_O &bv) {
  uint i;
  this->bits.resize(bv.bits.size());
  for (i = 0; i < this->bits.size(); i++) {
    this->bits[i] = bv.bits[i];
  }
}

//
// Destructor
//

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

void BitVector_O::do_subseq(BitVector_sp result, int start, int iend) const {
  int idest = 0;
  for (int i(start); i < iend; ++i) {
    result->setBit(idest++, this->testBit(i));
  }
}

T_sp SimpleBitVector_O::subseq(int start, T_sp end) const {
  if (start < 0) {
    SIMPLE_ERROR(BF("Illegal start %d for subseq") % start);
  }
  int iend;
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
  int ilen = iend - start;
  SimpleBitVector_sp result = SimpleBitVector_O::make(ilen);
  this->do_subseq(result, start, iend);
  return result;
}

T_sp BitVectorWithFillPtr_O::subseq(int start, T_sp end) const {
  if (start < 0) {
    SIMPLE_ERROR(BF("Illegal start %d for subseq") % start);
  }
  int iend;
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
  int ilen = iend - start;
  BitVectorWithFillPtr_sp result = BitVectorWithFillPtr_O::make(ilen, ilen, this->_adjustable);
  this->do_subseq(result, start, iend);
  return result;
}

void BitVector_O::getOnIndices(vector<uint> &res) {
  uint i;
  res.clear();
  for (i = 0; i != this->vector_length(); i++) {
    if (this->testBit(i)) {
      res.push_back(i);
    }
  }
}

bool BitVector_O::equal(T_sp obv) const {
  uint i;
  if (!gc::IsA<BitVector_sp>(obv))
    return ((false));
  BitVector_sp bv = gc::As<BitVector_sp>(obv);
  if (this->vector_length() != bv->vector_length())
    return ((false));
  for (i = 0; i < this->bits.size(); i++) {
    if (this->bits[i] != bv->bits[i]) {
      return ((false));
    }
  }
  return ((true));
}

void BitVector_O::erase() {
  vector<BitBlockType>::iterator vi;
  for (vi = this->bits.begin(); vi != this->bits.end(); vi++) {
    (*vi) = 0;
  }
}

CL_LISPIFY_NAME("core:setBit");
CL_DEFMETHOD void BitVector_O::setBit(uint i, uint v) {
  _OF();
  uint block;
  uint offset;
  BitBlockType packedVal;
  BitBlockType mask;
  BitBlockType omask;
  if (i >= this->vector_length()) {
    SIMPLE_ERROR(BF("BitVector index overflow"));
  }
  block = i / CHAR_BIT;
  offset = i % CHAR_BIT;
  omask = ~0;
  mask = (1 << offset) ^ omask;
  packedVal = v << offset;
  this->bits[block] = (this->bits[block] & mask) | packedVal;
}

CL_LISPIFY_NAME("core:testBit");
CL_DEFMETHOD uint BitVector_O::testBit(uint i) const {
  _OF();
  uint block;
  uint offset;
  BitBlockType mask;
  block = i / CHAR_BIT;
  offset = i % CHAR_BIT;
  mask = (1 << offset);

  LOG(BF("testBit i=%u CHAR_BIT=%d block=%d offset=%d") % (i) % (CHAR_BIT) % (block) % (offset));
  LOG(BF("      mask = |%lx|") % mask);
  LOG(BF("bits[%04d] = |%lx|") % block % this->bits[block]);
  BitBlockType result = (this->bits[block] & mask);
  LOG(BF("    result = |%lx|") % result);
  return ((result ? 1 : 0));
}

CL_LISPIFY_NAME("core:inPlaceOr");
CL_DEFMETHOD void BitVector_O::inPlaceOr(BitVector_sp bv) {
  _OF();
  uint i;
  if (this->vector_length() != bv->vector_length()) {
    SIMPLE_ERROR(BF("BitVectors aren't the same length for operation"));
  }
  for (i = 0; i != this->bits.size(); i++) {
    this->bits[i] |= bv->bits[i];
  }
}

CL_LISPIFY_NAME("core:inPlaceAnd");
CL_DEFMETHOD void BitVector_O::inPlaceAnd(BitVector_sp bv) {
  _OF();
  uint i;
  if (this->vector_length() != bv->vector_length()) {
    SIMPLE_ERROR(BF("BitVectors aren't the same length for operation"));
  }
  for (i = 0; i != this->bits.size(); i++) {
    this->bits[i] &= bv->bits[i];
  }
}

CL_LISPIFY_NAME("core:inPlaceXor");
CL_DEFMETHOD void BitVector_O::inPlaceXor(BitVector_sp bv) {
  _OF();
  uint i;
  if (this->vector_length() != bv->vector_length()) {
    SIMPLE_ERROR(BF("BitVectors aren't the same length for operation"));
  }
  for (i = 0; i != this->bits.size(); i++) {
    this->bits[i] ^= bv->bits[i];
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
CL_DEFMETHOD uint BitVector_O::countSet() {
  uint i;
  uint c;
  c = 0;
  for (i = 0; i < this->vector_length(); i++) {
    if (this->testBit(i))
      c++;
  }
  return ((c));
}

CL_LISPIFY_NAME("core:BitVector-asString");
CL_DEFMETHOD string BitVector_O::asString() {
  uint i;
  stringstream s;

  s.str("");
  for (i = 0; i < this->vector_length(); i++) {
    s << this->testBit(i);
  }
  return ((s.str()));
}

T_sp SimpleBitVector_O::deepCopy() const {
  GC_COPY(SimpleBitVector_O, n, *this);
  return (n);
}

T_sp BitVectorWithFillPtr_O::deepCopy() const {
  GC_COPY(BitVectorWithFillPtr_O, n, *this);
  return (n);
}

//
//	dumpToStream
//
//	Dump the BitVector to a stream
//
std::ostream &BitVector_O::dumpToStream(std::ostream &out) {
  uint i;
  for (i = 0; i < this->vector_length(); i++) {
    out << this->testBit(i);
  }
  out << std::endl;
  RET_POD((out));
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
  for (int i = 0; i < this->vector_length(); i++) {
    if (this->testBit(i)) {
      mpz_setbit(bn.get_mpz_t(), i);
    }
  }
  hg.addPart(bn);
}

CL_LISPIFY_NAME("core:lowestIndex");
CL_DEFMETHOD uint BitVector_O::lowestIndex() {
  uint i;
  for (i = 0; i < this->vector_length(); i++) {
    if (this->testBit(i)) {
      return ((i));
    }
  }
  return ((i));
}





CL_PKG_NAME(CorePkg,make-simple-bit-vector);
CL_DEFUN SimpleBitVector_sp SimpleBitVector_O::make(size_t size) {
  GC_ALLOCATE_VARIADIC(SimpleBitVector_O, sbv, size);
  return sbv;
}





CL_PKG_NAME(CorePkg,make-bit-vector-with-fill-ptr);
CL_DEFUN BitVectorWithFillPtr_sp BitVectorWithFillPtr_O::make(size_t size, size_t fill_ptr, bool adjust) {
  GC_ALLOCATE_VARIADIC(BitVectorWithFillPtr_O, sbv, size, fill_ptr, adjust);
  return sbv;
}

T_sp BitVectorWithFillPtr_O::vectorPush(T_sp newElement) {
  if (!newElement.fixnump()) {
    TYPE_ERROR(newElement, cl::_sym_bit);
  }
  Fixnum b = newElement.unsafe_fixnum();
  if (b != 0 && b != 1) {
    TYPE_ERROR(newElement, cl::_sym_bit);
  }
  if (!this->_adjustable) {
    if (this->_fill_ptr >= this->BitVector_O::dimension()) {
      return _Nil<T_O>();
    }
  }
  this->setBit(this->_fill_ptr, b);
  ++this->_fill_ptr;
  return clasp_make_fixnum(this->_fill_ptr - 1);
}

void BitVectorWithFillPtr_O::setFillPointer(size_t fp)
{
  if ( fp < this->dimension() ) {
    this->_fill_ptr = fp;
    return;
  }
  TYPE_ERROR_INDEX(this->asSmartPtr(),fp);
}

Fixnum_sp BitVectorWithFillPtr_O::vectorPushExtend(T_sp newElement, int extension) {
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
    this->bits.resize((this->_fill_ptr - 1 + extension) / CHAR_BIT + 1, 0);
  }
  return clasp_make_fixnum(this->_fill_ptr - 1);
}




};
