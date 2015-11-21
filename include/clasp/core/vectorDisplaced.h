/*
    File: vectorDisplaced.h
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
#ifndef _core_VectorDisplaced_H
#define _core_VectorDisplaced_H

#include <clasp/core/object.h>
#include <clasp/core/lispVector.h>
#include <clasp/core/corePackage.fwd.h>

namespace core {

FORWARD(VectorDisplaced);
class VectorDisplaced_O : public Vector_O {
  //  friend void(::sp_copyLoadTimeValue(T_sp *resultP, LoadTimeValues_O **ltvPP, int index));
  LISP_BASE1(Vector_O);
  LISP_CLASS(core, CorePkg, VectorDisplaced_O, "VectorDisplaced");

public:
  VectorDisplaced_O(){};
  virtual ~VectorDisplaced_O(){};

public:
  typedef gctools::Vec0<T_sp> vector_type;
  typedef gctools::Vec0<T_sp>::iterator iterator;

GCPROTECTED: // instance variables here -- REMEMBER to update swap(...) if you add/remove variables
  T_sp _ElementType;
  Vector_sp _Vector;
  size_t _Size;
  size_t _DisplacedIndexOffset;

public:
  //  void fillInitialContents(T_sp ic);

public:
  //  iterator begin() { return this->_Values.begin(); };
  //  iterator end() { return this->_Values.begin() + this->length(); };

public:
public:
  void setElementType(T_sp elementType) { this->_ElementType = elementType; };
  T_sp elementType() const { return this->_ElementType; };

public: // Functions here
  virtual T_sp aset_unsafe(int j, T_sp val) { (*this->_Vector)[j + this->_DisplacedIndexOffset] = val; };
  virtual T_sp aref_unsafe(cl_index index) const { return (*this->_Vector)[index + this->_DisplacedIndexOffset]; };

  virtual std::vector<cl_index> dimensions() const {
    std::vector<cl_index> dims;
    dims.push_back(this->_Size);
    return dims;
  };
  virtual gc::Fixnum dimension() const { return this->_Size; };
  virtual void rowMajorAset(cl_index idx, T_sp value);
  virtual T_sp rowMajorAref(cl_index idx) const;
  //  virtual gc::Fixnum arrayRowMajorIndex(List_sp indices) const;

  T_sp &operator[](uint index) { return (*this->_Vector)[index + this->_DisplacedIndexOffset]; }
  const T_sp &operator[](uint index) const { return (*this->_Vector)[index + this->_DisplacedIndexOffset]; }

  virtual void swapElements(uint i1, uint i2) {
    T_sp t = (*this)[i2];
    (*this)[i2] = (*this)[i1];
    (*this)[i1] = t;
  }

  /*! Swap the contents of the VectorDisplaced */
  void swap(VectorDisplaced_sp vec);

  virtual T_sp aref(List_sp indices) const;
  virtual T_sp setf_aref(List_sp indices_val);

  virtual T_sp elt(int index) const;
  virtual T_sp setf_elt(int index, T_sp value);

  virtual T_sp svref(int index) const { return this->elt(index); };
  virtual T_sp setf_svref(int index, T_sp value) { return this->setf_elt(index, value); };

  //  virtual void fillArrayWithElt(T_sp element, Fixnum_sp start, T_sp end);
  virtual gc::Fixnum length() const { return this->_Size; };
  string __repr__() const;

  //  virtual T_sp subseq(int start, T_sp end) const;
  virtual T_sp setf_subseq(int start, T_sp end, T_sp new_subseq) {
    _G();
    IMPLEMENT_ME();
  };

  VectorDisplaced_sp makeVectorDisplaced(T_sp dim, T_sp elementType, T_sp displacedTo, size_t displacedOffset);
};

}; /* core */
TRANSLATE(core::VectorDisplaced_O);
template <>
struct gctools::GCInfo<core::VectorDisplaced_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

#endif /* _core_VectorDisplaced_H */
