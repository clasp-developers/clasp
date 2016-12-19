/*
    File: lispVector.h
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
#ifndef _core_Vector_H
#define _core_Vector_H

#include <clasp/core/object.h>
#include <clasp/core/array.h>
#include <clasp/core/sequence.h>
#include <clasp/core/corePackage.fwd.h>

namespace core {

FORWARD(Vector);

/*! A one dimensional vector of objects */
class Vector_O : public Array_O {
  LISP_CLASS(core, ClPkg, Vector_O, "vector",Array_O);
public:
  explicit Vector_O() : Array_O(){};
private: // instance variables here
public:  // Functions here
  bool equalp(T_sp o) const;
CL_LISPIFY_NAME("adjustableArrayP");
CL_DEFMETHOD   bool adjustableArrayP() const { return false; };
  cl_index vector_length() const { return this->dimension(); };
  virtual cl_index dimension() const { SUBIMP(); };

  virtual void swapElements(cl_index idx1, cl_index idx2) { SUBIMP(); };

  /*! For write_array */
  virtual std::vector<cl_index> dimensions() const { SUBIMP(); };

  virtual size_t elementSizeInBytes() const { SUBIMP(); }
  virtual T_sp elementType() const { SUBIMP(); }
  virtual gc::Fixnum rank() const { return 1; };
  virtual gc::Fixnum arrayDimension(gc::Fixnum axisNumber) const;
  virtual List_sp arrayDimensions() const;
  virtual cl_index arrayTotalSize() const { return this->length(); };
  INHERIT_SEQUENCE virtual cl_index length() const { SUBIMP(); };

  virtual T_sp vectorPush(T_sp newElement) { SUBIMP(); };
  virtual Fixnum_sp vectorPushExtend(T_sp newElement, cl_index extension = 1) { SUBIMP(); };

  virtual T_sp aset_unsafe(cl_index j, T_sp val) { SUBIMP(); };
  virtual T_sp aref_unsafe(cl_index index) const { SUBIMP(); };

  CL_NAME("CL:FILL-POINTER");
  CL_DEFMETHOD virtual T_sp fillPointer() const { SUBIMP(); };

  virtual void fillPointerSet(T_sp idx);
  virtual void *addressOfBuffer() const { SUBIMP(); };

  virtual T_sp aref(VaList_sp indices) const;
  virtual T_sp setf_aref(List_sp indices_val);

  virtual T_sp rowMajorAref(cl_index idx) const { return this->elt(idx); };
  virtual void rowMajorAset(cl_index idx, T_sp value) { this->setf_elt(idx, value); };

  virtual void __write__(T_sp strm) const;

  INHERIT_SEQUENCE virtual T_sp reverse();
  INHERIT_SEQUENCE virtual T_sp nreverse();


  INHERIT_SEQUENCE virtual T_sp elt(cl_index index) const = 0;
  INHERIT_SEQUENCE virtual T_sp setf_elt(cl_index index, T_sp value) = 0;
  INHERIT_SEQUENCE virtual void swap(cl_index i1, cl_index i2) const = 0;
  INHERIT_SEQUENCE virtual T_sp subseq(cl_index start, T_sp end) const = 0;
  INHERIT_SEQUENCE virtual T_sp setf_subseq(cl_index start, T_sp end, T_sp newSubseq) = 0;

  virtual T_sp unsafe_subseq(cl_index start, cl_index end);
  virtual T_sp unsafe_setf_subseq(cl_index start, cl_index end, T_sp new_subseq);
  virtual void unsafe_fillArrayWithElt(T_sp element, cl_index start, cl_index end);
  virtual void unsafe_fillInitialContents(cl_index start, cl_index end, T_sp contents);
  virtual void unsafe_sxhash_(cl_index start, cl_index end, HashGenerator &hg);

}; /* core */
};


namespace core {
  class BaseSimpleVector_O : public Vector_O {
    LISP_CLASS(core, ClPkg, BaseSimpleVector_O, "base-simple-vector",Vector_O);
  public:
    INHERIT_SEQUENCE virtual cl_index length() const = 0;
    INHERIT_SEQUENCE virtual T_sp elt(cl_index index) const { return this->svref(index); };
    INHERIT_SEQUENCE virtual T_sp setf_elt(cl_index index, T_sp value) { return this->setf_svref(index,value); };
    virtual T_sp svref(cl_index index) const = 0;
    virtual T_sp setf_svref(cl_index index, T_sp value) = 0;
    INHERIT_SEQUENCE virtual void swap(cl_index i1, cl_index i2) const = 0;
    INHERIT_SEQUENCE virtual T_sp subseq(cl_index start, T_sp end) const = 0;
    INHERIT_SEQUENCE virtual T_sp setf_subseq(cl_index start, T_sp end, T_sp newSubseq) = 0;
  };
};

namespace core { class SimpleString8_O; };
template <>
struct gctools::GCInfo<core::SimpleString8_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = atomic;
};
namespace core {
  class SimpleString8_O : public BaseSimpleVector_O {
    LISP_CLASS(core, ClPkg, SimpleString8_O, "simple-string8",BaseSimpleVector_O);
  public:
    typedef char value_type;
    typedef gctools::GCArray_moveable<value_type> vector_type;
    typedef value_type* iterator;
    typedef const value_type* const_iterator;
  public:
    vector_type _Data;
  SimpleString8_O(value_type initial_element, size_t total_size) : Base(), _Data(initial_element,total_size) {};
  public:
    virtual cl_index length() const { return this->_Data._Capacity; };
  public:
    value_type& operator[](size_t index) { return this->_Data[index];};
    const value_type& operator[](size_t index) const { return this->_Data[index];};
    iterator begin() { return &this->_Data[0];};
    iterator end() { return &this->_Data[this->_Data._Capacity]; }
    const_iterator begin() const { return &this->_Data[0];};
    const_iterator end() const { return &this->_Data[this->_Data._Capacity]; }
  public:
    virtual void __write__(T_sp strm) const;
  };
};


namespace core { class SimpleBitVector_O; };
template <>
struct gctools::GCInfo<core::SimpleBitVector_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = atomic;
};
namespace core {
  class SimpleBitVector_O : public BaseSimpleVector_O {
    LISP_CLASS(core, ClPkg, SimpleBitVector_O, "simple-bit-vector",BaseSimpleVector_O);
  public:
    typedef uintptr_t value_type;
    static const size_t BitWidth = sizeof(value_type)/8;
    typedef gctools::GCArray_moveable<value_type> vector_type;
    typedef value_type* iterator;
    typedef const value_type* const_iterator;
  public:
    vector_type _Data;
  SimpleBitVector_O(value_type initial_element, size_t total_size) : Base(), _Data(initial_element,total_size) {};
  public:
    virtual cl_index length() const { return this->_Data._Capacity; };
  public:
    void setBit(cl_index i, uint v) {
      cl_index block;
      cl_index offset;
      value_type packedVal;
      value_type mask;
      value_type omask;
      block = i / BitWidth;
      offset = i % BitWidth;
      omask = ~0;
      mask = (1 << offset) ^ omask;
      packedVal = v << offset;
      this->_Data[block] = (this->_Data[block] & mask) | packedVal;
    }

    uint testBit(cl_index idx) const {
      cl_index block;
      cl_index offset;
      value_type mask;
      block = idx / BitWidth;
      offset = idx % BitWidth;
      mask = (1 << offset);
      LOG(BF("testBit i=%u BitWidth=%d block=%d offset=%d") % (idx) % BitWidth % (block) % (offset));
      LOG(BF("      mask = |%lx|") % mask);
      LOG(BF("bits[%04d] = |%lx|") % block % this->_Data[block]);
      value_type result = (this->_Data[block] & mask);
      LOG(BF("    result = |%lx|") % result);
      return ((result ? 1 : 0));
    }
  };
};

namespace core { class SimpleVector_O; };
template <>
struct gctools::GCInfo<core::SimpleVector_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};
namespace core {
  class SimpleVector_O : public BaseSimpleVector_O {
    LISP_CLASS(core, ClPkg, SimpleVector_O, "simple-vector",BaseSimpleVector_O);
  public:
    typedef T_sp value_type;
    typedef gctools::GCArray_moveable<value_type> vector_type;
    typedef value_type* iterator;
    typedef const value_type* const_iterator;
  public:
    vector_type _Data;
  SimpleVector_O(value_type initial_element, size_t total_size) : Base(), _Data(initial_element,total_size) {};
  public:
    virtual cl_index length() const { return this->_Data._Capacity; };
  public:
    value_type& operator[](size_t index) { return this->_Data[index];};
    const value_type& operator[](size_t index) const { return this->_Data[index];};
    iterator begin() { return &this->_Data[0];};
    iterator end() { return &this->_Data[this->_Data._Capacity]; }
    const_iterator begin() const { return &this->_Data[0];};
    const_iterator end() const { return &this->_Data[this->_Data._Capacity]; }
  public:
  };
};


namespace core {
Vector_sp core__make_vector(T_sp element_type,
                            size_t dimension,
                            bool adjustable = false,
                            T_sp fill_pointer = cl::_sym_T_O,
                            T_sp displaced_to = _Nil<T_O>(),
                            cl_index displaced_index_offset = 0,
                            T_sp initial_element = _Nil<T_O>(),
                            bool initial_element_supplied_p = false);
};


namespace core {

class NonSimpleVector_O : public Vector_O {
  LISP_CLASS(core, ClPkg, NonSimpleVector_O, "non-simple-vector",Vector_O);
public:
  explicit NonSimpleVector_O(cl_index totalArraySize, T_sp element_type, T_sp initial_element, T_sp fillPointer, T_sp displacedTo, cl_index displacedIndexOffset) : Base(), _TotalArraySize(totalArraySize), _ElementType(element_type), _Vec(), _DisplacedIndexOffset(displacedIndexOffset) {
    if (fillPointer.fixnump()) {
      this->_FillPointerOrLength = fillPointer.unsafe_fixnum();
      this->_FillPointerP = true;
    } else {
      this->_FillPointerOrLength = totalArraySize;
      this->_FillPointerP = false;
    }
    this->_TotalArraySize = totalArraySize;
    if ( displacedTo.notnilp() ) {
      this->_Vec = gc::As<Vector_sp>(displacedTo);
      this->_DisplacedIndexOffset = displacedIndexOffset;
      this->_DisplacedToP = true;
    } else {
      this->_Vec = core__make_vector(element_type, totalArraySize, false,_Nil<T_O>(),_Nil<T_O>(),0,initial_element,true);
      this->_DisplacedIndexOffset = 0;
      this->_DisplacedToP = false;
    }
  }
 public:
  size_t     _FillPointerOrLength;
  size_t     _TotalArraySize;
  T_sp       _ElementType;
  Vector_sp  _Vec;
  size_t     _DisplacedIndexOffset;
  bool       _FillPointerP;
  bool       _DisplacedToP;
 public:
  virtual bool adjustableArrayP() const { return true; }
public:
  //! dimension() ignores the fill pointer
  virtual T_sp displaced_to() const {
    if (this->_DisplacedToP) return this->_Vec;
    return _Nil<T_O>();
  }
  virtual cl_index displaced_index_offset() const {
    return this->_DisplacedIndexOffset;
  }
  virtual bool arrayHasFillPointerP() const { return this->_FillPointerP; };
  virtual void fillPointerSet(T_sp idx);
  T_sp fillPointer() const { if (this->_FillPointerP) return clasp_make_fixnum(this->_FillPointerOrLength); else return _Nil<T_O>(); };
  
  virtual void swapElements(cl_index i1, cl_index i2) {
    this->_Vec->swapElements(i1+this->_DisplacedIndexOffset,i2+this->_DisplacedIndexOffset);
  }
  virtual T_sp elementType() const { return this->_ElementType; };

  virtual void __write__(T_sp strm) const {this->_Vec->__write__(strm); };
    
  virtual T_sp elt(cl_index index) const { return this->_Vec->elt(index+this->_DisplacedIndexOffset); }
  virtual T_sp setf_elt(cl_index index, T_sp value) { return this->_Vec->setf_elt(index+this->_DisplacedIndexOffset,value); }

  cl_index checkBounds(cl_index start, T_sp end, cl_index length) const;

  virtual T_sp subseq(cl_index start, T_sp end) const
  {
    cl_index iend = this->checkBounds(start,end,this->length());
    return this->_Vec->unsafe_subseq(start+this->_DisplacedIndexOffset,iend+this->_DisplacedIndexOffset);
  }
  virtual T_sp setf_subseq(cl_index start, T_sp end, T_sp new_subseq)
  {
    cl_index iend = this->checkBounds(start,end,this->length());
    return this->_Vec->unsafe_setf_subseq(start+this->_DisplacedIndexOffset,iend+this->_DisplacedIndexOffset,new_subseq);
  }
  virtual void fillArrayWithElt(T_sp element, cl_index start, T_sp end)
  {
    cl_index iend = this->checkBounds(start,end,this->length());
    this->_Vec->unsafe_fillArrayWithElt(element,start+this->_DisplacedIndexOffset,iend+this->_DisplacedIndexOffset);
  }
  virtual void fillInitialContents(T_sp initialContents)
  {
    this->_Vec->unsafe_fillInitialContents(this->_DisplacedIndexOffset,this->length()+this->_DisplacedIndexOffset, initialContents );
  }

  virtual void sxhash_(HashGenerator &hg) const
  {
    this->_Vec->unsafe_sxhash_(this->_DisplacedIndexOffset,this->length()+this->_DisplacedIndexOffset, hg);
  }

  void ensureSpaceAfterFillPointer(cl_index size);
  T_sp vectorPush(T_sp newElement);
  Fixnum_sp vectorPushExtend(T_sp newElement, cl_index extension = 0);

};
}



namespace core {

  class StrNs_O : public NonSimpleVector_O {
    LISP_CLASS(core, ClPkg, StrNs_O, "non-simple-string-base-char",NonSimpleVector_O);
  public:
    // move all the constructors into here
    
  };
};




















namespace cl {
  extern core::Symbol_sp& _sym_General_O;
};

namespace core {
// Like ecl__vector_start_end
T_mv clasp_vectorStartEnd(Symbol_sp fn, T_sp thing, Fixnum_sp start, Fixnum_sp end);

 void noFillPointerError();
};

#endif /* _core_Vector_H */
