/*
    File: bitVector.h
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
#ifndef core_BitVector_H
#define core_BitVector_H
#include <stdio.h>
#include <limits>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lispVector.h>

namespace core {

typedef unsigned char BitBlockType;

SMART(BitVector);
class BitVector_O : public Vector_O {
  LISP_BASE1(Vector_O);
  LISP_CLASS(core, ClPkg, BitVector_O, "bit-vector");

public:
  bool bitVectorP() const { return true; };
  virtual int offset() const { return 0; }; // displaced arrays?  used in bits.cc
  virtual void __write__(T_sp strm) const;
  virtual uint testBit(uint i) const {SUBIMP();};

  explicit BitVector_O() : Vector_O(){};
  virtual ~BitVector_O(){};
};

SMART(SimpleBitVector);
class SimpleBitVector_O : public BitVector_O {
  friend T_sp core_bitArrayOp(T_sp o, T_sp x, T_sp y, T_sp r);
  LISP_BASE1(BitVector_O);
  LISP_CLASS(core, ClPkg, SimpleBitVector_O, "simple-bit-vector");

private:
  uint _Length;
  vector<unsigned char> bits;

public:
  static SimpleBitVector_sp create(int size);

public:
  gc::Fixnum dimension() const { return this->_Length; };
  unsigned char *bytes() const { return const_cast<unsigned char *>(this->bits.data()); };

  void getOnIndices(vector<uint> &vals);
  void setOnIndices(const vector<uint> &indices);


  bool equal(T_sp bv) const;

  void setBit(uint i, uint v);
  uint testBit(uint i) const;
  void erase();

  void sxhash_(HashGenerator &hg) const;
  uint lowestIndex();

  //! Calculate the "or" of bv with this BitVector
  void inPlaceOr(SimpleBitVector_sp bv);
  //! Calculate the "and" of bv with this BitVector
  void inPlaceAnd(SimpleBitVector_sp bv);
  //! Calculate the "xor" of bv with this BitVector
  void inPlaceXor(SimpleBitVector_sp bv);

  //! Return a BitVector "or"ed with this
  SimpleBitVector_sp bitOr(SimpleBitVector_sp bv);
  //! Return a BitVector "and"ed with this
  SimpleBitVector_sp bitAnd(SimpleBitVector_sp bv);
  //! Return a BitVector "xor"ed with this
  SimpleBitVector_sp bitXor(SimpleBitVector_sp bv);

  //! Return the number of set bits
  uint countSet();
  //! Return true if the BitVector contains only 0's
  bool isZero() { return (this->countSet() == 0); };

  string asString();

  std::ostream &dumpToStream(std::ostream &out);
  void dump();

  T_sp deepCopy() const;

#if 0
    virtual T_sp& operator[](uint index);

    virtual T_sp getElementObject(uint index) const;

    virtual T_sp setElementObject(uint index, T_sp val);
#endif

  virtual void rowMajorAset(cl_index idx, T_sp value);
  virtual T_sp rowMajorAref(cl_index idx) const;

  virtual T_sp elementType() const { return cl::_sym_bit;};
  explicit SimpleBitVector_O() : BitVector_O(){};
  SimpleBitVector_O(const SimpleBitVector_O &bv);
  virtual ~SimpleBitVector_O(){};
};
};
TRANSLATE(core::BitVector_O);
TRANSLATE(core::SimpleBitVector_O);
#endif
