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
  _G();
  ASSERTF(idx < this->length(), BF("Index %d is out of range (<%d)") % idx % this->length());
  Fixnum_sp fn = gc::As<Fixnum_sp>(value);
  this->setBit(idx, fn.unsafe_fixnum());
}

T_sp BitVector_O::rowMajorAref(cl_index idx) const {
  _G();
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
  SimpleBitVector_sp result = SimpleBitVector_O::create(ilen);
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
  BitVectorWithFillPtr_sp result = BitVectorWithFillPtr_O::create(ilen, ilen, this->_adjustable);
  this->do_subseq(result, start, iend);
  return result;
}

void BitVector_O::getOnIndices(vector<uint> &res) {
  _G();
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

void BitVector_O::setBit(uint i, uint v) {
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

uint BitVector_O::testBit(uint i) const {
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

void BitVector_O::inPlaceOr(BitVector_sp bv) {
  _OF();
  uint i;
  if (this->vector_length() != bv->vector_length()) {
    SIMPLE_ERROR(BF("BitVectors aren't the same length for operation"));
  }
  for (i = 0; i != this->bits.size(); i++) {
    this->bits[i] |= bv->bits[i];
  }
}

void BitVector_O::inPlaceAnd(BitVector_sp bv) {
  _OF();
  uint i;
  if (this->vector_length() != bv->vector_length()) {
    SIMPLE_ERROR(BF("BitVectors aren't the same length for operation"));
  }
  for (i = 0; i != this->bits.size(); i++) {
    this->bits[i] &= bv->bits[i];
  }
}

void BitVector_O::inPlaceXor(BitVector_sp bv) {
  _OF();
  uint i;
  if (this->vector_length() != bv->vector_length()) {
    SIMPLE_ERROR(BF("BitVectors aren't the same length for operation"));
  }
  for (i = 0; i != this->bits.size(); i++) {
    this->bits[i] ^= bv->bits[i];
  }
}

BitVector_sp BitVector_O::bitOr(BitVector_sp bv) {
  BitVector_sp res = gc::As<BitVector_sp>(this->deepCopy());
  res->inPlaceOr(bv);
  return ((res));
}

BitVector_sp BitVector_O::bitAnd(BitVector_sp bv) {
  BitVector_sp res = gc::As<BitVector_sp>(this->deepCopy());
  res->inPlaceAnd(bv);
  return ((res));
}

BitVector_sp BitVector_O::bitXor(BitVector_sp bv) {
  BitVector_sp res = gc::As<BitVector_sp>(this->deepCopy());
  res->inPlaceXor(bv);
  return ((res));
}

uint BitVector_O::countSet() {
  uint i;
  uint c;
  c = 0;
  for (i = 0; i < this->vector_length(); i++) {
    if (this->testBit(i))
      c++;
  }
  return ((c));
}

string BitVector_O::asString() {
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
void BitVector_O::dump() {
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

uint BitVector_O::lowestIndex() {
  uint i;
  for (i = 0; i < this->vector_length(); i++) {
    if (this->testBit(i)) {
      return ((i));
    }
  }
  return ((i));
}

void BitVector_O::exposeCando(Lisp_sp lisp) {
  class_<BitVector_O>()
      //	.def("equal",&BitVector_O::equal)
      .def("core:setBit", &BitVector_O::setBit)
      .def("core:testBit", &BitVector_O::testBit)
      .def("core:inPlaceOr", &BitVector_O::inPlaceOr)
      .def("core:inPlaceAnd", &BitVector_O::inPlaceAnd)
      .def("core:inPlaceXor", &BitVector_O::inPlaceXor)
      .def("core:bitOr", &BitVector_O::bitOr)
      .def("core:bitAnd", &BitVector_O::bitAnd)
      .def("core:bitXor", &BitVector_O::bitXor)
      .def("core:countSet", &BitVector_O::countSet)
      .def("core:isZero", &BitVector_O::isZero)
      .def("core:lowestIndex", &BitVector_O::lowestIndex)
      .def("core:dump", &BitVector_O::dump)
      .def("core:BitVector-asString", &BitVector_O::asString);
}
void BitVector_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, BitVector, "", "", _lisp)
      //	.def("equal",&BitVector_O::equal)
      .def("setBit", &BitVector_O::setBit)
      .def("testBit", &BitVector_O::testBit)
      .def("inPlaceOr", &BitVector_O::inPlaceOr)
      .def("inPlaceAnd", &BitVector_O::inPlaceAnd)
      .def("inPlaceXor", &BitVector_O::inPlaceXor)
      .def("bitOr", &BitVector_O::bitOr)
      .def("bitAnd", &BitVector_O::bitAnd)
      .def("bitXor", &BitVector_O::bitXor)
      .def("countSet", &BitVector_O::countSet)
      .def("isZero", &BitVector_O::isZero)
      .def("lowestIndex", &BitVector_O::lowestIndex)
      .def("dump", &BitVector_O::dump)
      .def("asString", &BitVector_O::asString);
//    boost::python::def("create_BitVector",&BitVector_O::create);
#endif
}

EXPOSE_CLASS(core, BitVector_O);

SimpleBitVector_sp SimpleBitVector_O::create(size_t size) {
  GC_ALLOCATE_VARIADIC(SimpleBitVector_O, sbv, size);
  return sbv;
}

void SimpleBitVector_O::exposeCando(Lisp_sp lisp) {
  class_<SimpleBitVector_O>();
  af_def(CorePkg, "make-simple-bit-vector", (SimpleBitVector_sp (*)(size_t)) & SimpleBitVector_O::create);
}
void SimpleBitVector_O::exposePython(Lisp_sp lisp) {
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, BitVector, "", "", _lisp);
#endif
}

EXPOSE_CLASS(core, SimpleBitVector_O);

BitVectorWithFillPtr_sp BitVectorWithFillPtr_O::create(size_t size, size_t fill_ptr, bool adjust) {
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

void BitVectorWithFillPtr_O::exposeCando(Lisp_sp lisp) {
  class_<BitVectorWithFillPtr_O>();
  af_def(CorePkg, "make-bit-vector-with-fill-ptr", (BitVectorWithFillPtr_sp (*)(size_t, size_t, bool)) & BitVectorWithFillPtr_O::create);
}
void BitVectorWithFillPtr_O::exposePython(Lisp_sp lisp) {
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, BitVector, "", "", _lisp);
#endif
}

EXPOSE_CLASS(core, BitVectorWithFillPtr_O);
};
