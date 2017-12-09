/*
    File: newVectorObjects.h
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
#ifndef _core_VectorObjects_H
#define _core_VectorObjects_H

#include <clasp/core/object.h>
#include <clasp/core/vectorCore.h>
#include <clasp/core/lispVector.h>
#include <clasp/core/corePackage.fwd.h>

namespace core {

FORWARD(VectorObjects);
class VectorObjects_O : public Vector_O {
  LISP_CLASS(core, CorePkg, VectorObjects_O, "VectorObjects",Vector_O);

  void archiveBase(SNode_sp node);

public:
  VectorObjects_O();
  virtual ~VectorObjects_O(){};

public:
  void initialize();

protected: // instance variables here -- REMEMBER to update swap(...) if you add/remove variables
  typedef vec::MutableVector<T_sp>::iterator iterator;
  T_sp _ElementType;
  bool _Adjustable;
  vec::MutableVector<T_sp> *_Values;

public:
  void fillInitialContents(T_sp ic);

public:
  iterator begin() { return this->_Values->begin(); };
  iterator end() { return this->_Values->begin() + this->length(); };

public:
  static VectorObjects_sp create(T_sp initial_element, int dimension, T_sp elementType);
  static VectorObjects_sp make(T_sp initial_element, T_sp initialContents, int dimension, bool adjustable);

public:
  void setup(T_sp initial_element, T_sp initialContents, int dimension, bool adjustable);
  void adjust(T_sp initial_element, T_sp initialContents, int dimension);

  void setElementType(T_sp elementType) { this->_ElementType = elementType; };
  T_sp elementType() const { return this->_ElementType; };

public: // Functions here
  bool adjustableArrayP() const { return this->_Adjustable; };

  uint dimension() const { return this->_Values->size(); };
  virtual void rowMajorAset(cl_index idx, T_sp value);
  virtual T_sp rowMajorAref(cl_index idx) const;
  virtual cl_index arrayRowMajorIndex(List_sp indices) const;

  T_sp &operator[](uint index) { ASSERT(i<this->_Vector.size()); return (*this->_Values)[index]; }

  virtual void swapElements(uint i1, uint i2) {
    T_sp t = (*this->_Values)[i2];
    (*this->_Values)[i2] = (*this->_Values)[i1];
    (*this->_Values)[i1] = t;
  }

  /*! Swap the contents of the VectorObjects */
  void swap(VectorObjects_sp vec);

  virtual T_sp aref(List_sp indices) const;
  virtual T_sp setf_aref(List_sp indices_val);

  virtual T_sp elt(int index) const;
  virtual T_sp setf_elt(int index, T_sp value);

  virtual T_sp svref(int index) const { return this->elt(index); };
  virtual T_sp setf_svref(int index, T_sp value) { return this->setf_elt(index, value); };

  virtual void fillArrayWithElt(T_sp element, Fixnum_sp start, T_sp end);
  uint length() const { return this->_Values->size(); };
  string __repr__() const;

  virtual T_sp subseq(int start, T_sp end) const;
  virtual T_sp setf_subseq(int start, T_sp end, T_sp new_subseq) {
    _G();
    HARD_IMPLEMENT_ME();
  };
};

}; /* core */


#endif /* _core_VectorObjects_H */
