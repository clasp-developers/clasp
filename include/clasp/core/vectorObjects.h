/*
    File: vectorObjects.h
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
#include <clasp/core/lispVector.h>
#include <clasp/core/corePackage.fwd.h>

extern "C" {
extern void sp_copyLoadTimeValue(core::T_sp *resultP, core::LoadTimeValues_O **ltvPP, int index);
};

namespace core {

FORWARD(VectorObjects);
class VectorObjects_O : public Vector_O {
  friend void(::sp_copyLoadTimeValue(T_sp *resultP, LoadTimeValues_O **ltvPP, int index));
  LISP_BASE1(Vector_O);
  LISP_CLASS(core, CorePkg, VectorObjects_O, "VectorObjects");
  DECLARE_INIT();

  void archiveBase(SNode_sp node);

public:
  VectorObjects_O();
  virtual ~VectorObjects_O(){};

public:
  typedef gctools::Vec0<T_sp> vector_type;
  typedef gctools::Vec0<T_sp>::iterator iterator;

GCPROTECTED: // instance variables here -- REMEMBER to update swap(...) if you add/remove variables
  T_sp _ElementType;
  bool _Adjustable;
  vector_type _Values;

public:
  void fillInitialContents(T_sp ic);

public:
  iterator begin() { return this->_Values.begin(); };
  iterator end() { return this->_Values.begin() + this->length(); };

public:
  static VectorObjects_sp create(T_sp initial_element, int dimension, T_sp elementType);
  static VectorObjects_sp make(T_sp initial_element, T_sp initialContents, int dimension, bool adjustable, T_sp elementType);
  static VectorObjects_sp create(const gctools::Vec0<T_sp> &objs);

public:
  void setup(T_sp initial_element, T_sp initialContents, int dimension, bool adjustable, T_sp elementType);
  void adjust(T_sp initial_element, T_sp initialContents, int dimension);

  void setElementType(T_sp elementType) { this->_ElementType = elementType; };
  T_sp elementType() const { return this->_ElementType; };

public: // Functions here
  bool adjustableArrayP() const { return this->_Adjustable; };

  virtual T_sp aset_unsafe(int j, T_sp val);
  virtual T_sp aref_unsafe(cl_index index) const { return this->_Values[index]; };

  virtual std::vector<cl_index> dimensions() const {
    std::vector<cl_index> dims;
    dims.push_back(this->length());
    return dims;
  };
  virtual gc::Fixnum dimension() const { return this->_Values.size(); };
  virtual void rowMajorAset(cl_index idx, T_sp value);
  virtual T_sp rowMajorAref(cl_index idx) const;
  virtual gc::Fixnum arrayRowMajorIndex(List_sp indices) const;

  T_sp &operator[](uint index) { return this->_Values[index]; }

  virtual void swapElements(uint i1, uint i2) {
    T_sp t = this->_Values[i2];
    this->_Values[i2] = this->_Values[i1];
    this->_Values[i1] = t;
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
  virtual gc::Fixnum length() const { return this->_Values.size(); };
  string __repr__() const;

  virtual T_sp subseq(int start, T_sp end) const;
  virtual T_sp setf_subseq(int start, T_sp end, T_sp new_subseq) {
    _G();
    IMPLEMENT_ME();
  };
};

}; /* core */
TRANSLATE(core::VectorObjects_O);
template <>
struct gctools::GCInfo<core::VectorObjects_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

#endif /* _core_VectorObjects_H */
