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
extern void sp_copyLoadTimeValue(core::T_sp *resultP, core::LoadTimeValues_O **ltvPP, cl_index index);
};

template <>
struct gctools::GCInfo<core::VectorObjects_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {

FORWARD(VectorObjects);
class VectorObjects_O : public Vector_O {
  friend void(::sp_copyLoadTimeValue(T_sp *resultP, LoadTimeValues_O **ltvPP, cl_index index));
  LISP_CLASS(core, CorePkg, VectorObjects_O, "VectorObjects",Vector_O);
public:
 VectorObjects_O() : Base(), _Dimension(0), _FillPointer(_Nil<core::T_O>()), _ElementType(cl::_sym_T_O), _DisplacedTo(VectorObjects_sp()), _DisplacedIndexOffset(0) {};
public:
  typedef gctools::Vec0<T_sp> vector_type;
  typedef gctools::Vec0<T_sp>::iterator iterator;

public: // instance variables here -- REMEMBER to update swap(...) if you add/remove variables
  size_t       _Dimension;
  T_sp         _FillPointer;
  T_sp         _ElementType;
  vector_type  _Values;
  /*! Displaced vector stuff */
  VectorObjects_sp _DisplacedTo;
  gc::Fixnum       _DisplacedIndexOffset;
  
 public: // Creators
  static VectorObjects_sp make(T_sp initial_element, size_t dimension, T_sp elementType, T_sp fillPointer= _Nil<core::T_O>(), T_sp displacedTo=_Nil<core::T_O>(), Fixnum displacedIndexOffset=0 );
  static VectorObjects_sp create(T_sp initial_element, size_t dimension, T_sp elementType);
  static VectorObjects_sp create(const gctools::Vec0<T_sp> &objs);
public: // Functions here
public:
  void fillInitialContents(T_sp ic);
public:
  iterator begin() { return this->_Values.begin()+this->_DisplacedIndexOffset; };
  iterator end() { return this->begin() + this->length(); };
  bool adjustableArrayP() const { return true; };
public:
  void setup(T_sp initial_element, size_t dimension, T_sp elementType, T_sp fillPointer, T_sp displacedTo, Fixnum displacedIndexOffset );
  void adjust(T_sp initial_element, size_t dimension);

  void setElementType(T_sp elementType) { this->_ElementType = elementType; };
  T_sp elementType() const { return this->_ElementType; };

  void clear() { if ( this->_FillPointer.fixnump()) this->_FillPointer = clasp_make_fixnum(0); };
  virtual bool arrayHasFillPointerP() const { return this->_FillPointer.fixnump(); };
  T_sp fillPointer() const { return this->_FillPointer; };
  void unsafe_setf_fill_pointer(T_sp fp) { this->_FillPointer = fp; };
  void fillPointerSet(T_sp fp);
  
  virtual T_sp aset_unsafe(cl_index j, T_sp val) { (*this)[j] = val; return val; };
  virtual T_sp aref_unsafe(cl_index index) const { return (*this)[index]; };

  virtual std::vector<cl_index> dimensions() const {
    std::vector<cl_index> dims;
    dims.push_back(this->length());
    return dims;
  };
  virtual cl_index dimension() const { return this->_Dimension; };
  virtual T_sp displaced_to() const {
    if (this->_DisplacedTo) return this->_DisplacedTo;
    return _Nil<T_O>();
  }
  virtual cl_index displaced_index_offset() const {
    return this->_DisplacedIndexOffset;
  }

  T_sp replace_array(T_sp other) {
    *this = *gc::As<VectorObjects_sp>(other);
    return this->asSmartPtr();
  }
  virtual void swapElements(cl_index i1, cl_index i2) {
    T_sp t = (*this)[i2];
    (*this)[i2] = (*this)[i1];
    (*this)[i1] = t;
  }

  /*! Swap the contents of the VectorObjects */
  void swap(VectorObjects_sp vec);

  T_sp &operator[](cl_index index) { return this->_Values[this->_DisplacedIndexOffset+index]; }
  const T_sp &operator[](cl_index index) const { return this->_Values[this->_DisplacedIndexOffset+index]; }

  virtual void rowMajorAset(cl_index idx, T_sp value) { (*this)[idx] = value; };
  virtual T_sp rowMajorAref(cl_index idx) const { return (*this)[idx]; };
  virtual cl_index arrayRowMajorIndex(List_sp indices) const;

  virtual T_sp aref(VaList_sp indices) const;
  virtual T_sp setf_aref(List_sp indices_val);

  virtual T_sp elt(cl_index index) const { return (*this)[index]; };
  virtual T_sp setf_elt(cl_index index, T_sp value) { (*this)[index] = value; return value;};

  virtual T_sp svref(cl_index index) const { return (*this)[index]; };
  virtual T_sp setf_svref(cl_index index, T_sp value) { return (*this)[index] = value; return value; };

  virtual void fillArrayWithElt(T_sp element, Fixnum_sp start, T_sp end);
  virtual cl_index length() const { return (this->_FillPointer.fixnump()) ? this->_FillPointer.unsafe_fixnum() : this->dimension(); };
  string __repr__() const;

  T_sp vectorPush(T_sp newElement);
  Fixnum_sp vectorPushExtend(T_sp newElement, cl_index extension = 16);


  virtual T_sp subseq(cl_index start, T_sp end) const;
  virtual T_sp setf_subseq(cl_index start, T_sp end, T_sp new_subseq) {
    IMPLEMENT_ME();
  };
};

 Vector_sp make_vector_objects(T_sp element_type,
                               size_t dimension,
                               T_sp initial_element,
                               bool initial_element_supplied_p,
                               bool adjustable,
                               T_sp fill_pointer,
                               T_sp displaced_to,
                               cl_index displaced_index_offset);


}; /* core */

#endif /* _core_VectorObjects_H */
