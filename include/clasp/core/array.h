/*
    File: array.h
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
#ifndef _core_Array_H
#define _core_Array_H

#include <clasp/core/clasp_gmpxx.h>
#include <clasp/core/foundation.h>
#include <clasp/core/numbers.h>
#include <clasp/core/corePackage.fwd.h>

namespace core {

FORWARD(Array);
class Array_O : public General_O {
  friend class ArrayObjects_O;
  LISP_CLASS(core, ClPkg, Array_O, "array",General_O);
#if defined(XML_ARCHIVE)
  DECLARE_ARCHIVE();
#endif // defined(XML_ARCHIVE)
#if defined(OLD_SERIALIZE)
  DECLARE_SERIALIZE();
#endif // defined(OLD_SERIALIZE)
public:
  explicit Array_O(){};
  virtual ~Array_O(){};

private: // instance variables here
public:
  /*! Check an index of the array to make sure it is in bounds */
  static int checkedIndex(const string &filename, int lineno, const string &function, Array_sp array, int which, T_sp index, int nonincl_index);

public: // Functions here
  virtual bool equalp(T_sp other) const { SUBIMP(); };
  virtual T_sp aset_unsafe(cl_index j, T_sp val) { SUBIMP(); };
CL_LISPIFY_NAME("cl:arrayHasFillPointerP");
CL_DEFMETHOD   virtual bool arrayHasFillPointerP() const { return false; };
  virtual gc::Fixnum arrayTotalSize() const;
  virtual T_sp aref_unsafe(cl_index index) const { SUBIMP(); };
  /*! Return the total capacity of the array */
  virtual gc::Fixnum dimension() const {SUBIMP();};
  virtual std::vector<cl_index> dimensions() const { SUBIMP(); };
CL_LISPIFY_NAME("core:rowMajorAset");
CL_DEFMETHOD   virtual void rowMajorAset(cl_index idx, T_sp value) { SUBIMP(); };
 
CL_LISPIFY_NAME("cl:rowMajorAref");
CL_DEFMETHOD   virtual T_sp rowMajorAref(cl_index idx) const { SUBIMP(); };
 
  virtual cl_index arrayRowMajorIndex(VaList_sp indices) const;

  //! Don't support adjustable arrays yet
  bool adjustable_array_p() const { return false; };
  //! Don't support displaced arrays yet
  bool _displaced_array_p() const { return false; }
  //! Don't support fill pointers yet
  bool array_has_fill_pointer_p() const { return false; }

  virtual LongLongInt setDimensions(List_sp dimensions, T_sp initialElement) { SUBIMP(); };

  /*! Return the rank of the array */
CL_LISPIFY_NAME("cl:array-rank");
CL_DEFMETHOD   virtual size_t rank() const { SUBIMP(); };

  /*! Return the offset into a one-dimensional vector for the multidimensional index
      in the vector<int>s.  This is in rowMajor order.*/
  cl_index index_vector_int(const vector<int> &indices) const;

  /*! Return the offset into a one-dimensional vector for a multidimensional index
	 If last_value_is_val == true then don't use the last value in the indices list
	*/
  cl_index index_val_(List_sp indices, bool last_value_is_val, T_sp &last_val) const;
  cl_index index_val_(VaList_sp indices, bool last_value_is_val, T_sp &last_val) const;

  /*! Return the offset into a one-dimensional vector for a multidimensional index
	*/
  inline gc::Fixnum index_(List_sp indices) const {
    T_sp dummy;
    return this->index_val_(indices,false,dummy);
  };
  inline gc::Fixnum index_(VaList_sp indices) const {
    T_sp dummy;
    return this->index_val_(indices,false,dummy);
  };

  /*! Return the type returned by this array */
CL_LISPIFY_NAME("cl:array-elementType");
 CL_DEFMETHOD   virtual T_sp elementType() const {SUBIMP();}

  /*! This replicates ECL ecl__elttype_to_symbol in array.d */

  Symbol_sp element_type_as_symbol() const;

  /*! Return the array dimension along the axis-number */
CL_LISPIFY_NAME("cl:array-dimension");
CL_DEFMETHOD   virtual size_t arrayDimension(gc::Fixnum axisNumber) const { SUBIMP(); };

  /*! Return the array dimensions as a list of integers */
  virtual List_sp arrayDimensions() const;

  /*! Return the value at the indices */
  virtual T_sp aref(VaList_sp indices) const;

  /*! Setf the value at the indices - the val is at the end of the list of indices */
  virtual T_sp setf_aref(List_sp indices_val);

CL_LISPIFY_NAME("cl:svref");
CL_DEFMETHOD   virtual T_sp svref(cl_index idx) const { SUBIMP(); };
CL_LISPIFY_NAME("core:setf-svref");
CL_DEFMETHOD   virtual T_sp setf_svref(cl_index idx, T_sp val) { SUBIMP(); };

  /*! Return the value at the indices */
CL_LISPIFY_NAME("core:array-fill");
 CL_DEFMETHOD   virtual void arrayFill(T_sp val) {SUBIMP();}


  /*! Fill the range of elements of the array,
     if end is nil then fill to the end of the array*/
CL_LISPIFY_NAME("core:fill-array-with-elt");
CL_DEFMETHOD   virtual void fillArrayWithElt(T_sp element, Fixnum_sp start, T_sp end) {
    _OF();
    SUBCLASS_MUST_IMPLEMENT();
  };

CL_LISPIFY_NAME("core:replace-array");
CL_DEFMETHOD   virtual T_sp replace_array(T_sp other) {
    SUBCLASS_MUST_IMPLEMENT();
  };

  virtual void __write__(T_sp strm) const;

  virtual string __repr__() const;
};


 
}; /* core */


namespace core {
class MDArray_o : public Array_o {
  LISP_CLASS(core, ClPkg, MDArray_o, "mdarray",Array_o);
 public:
//! same offset as _fillpointerorlength in nonsimplevector
  size_t      _FillPointerOrLengthOrDummy;
  size_t      _TotalArraySize;
  Array_sp    _Data;
  size_t      _DisplacedIndexOffset;
  bool        _DisplacedTop;
  bool        _FillPointerP;
  gctools::GCArray_moveable<size_t> _Dimensions;
MDArray_o(size_t rank,
          List_sp dimensions,
          T_sp elementType,
          T_sp fillPointer,
          Array_sp displacedTo,
          size_t displacedIndexOffset,
          T_sp initial_element,
          bool initial_element_supplied_p );
 public:
  virtual bool adjustableArrayP() const { return true; }
 public:
  //! dimension() ignores the fill pointer
  virtual T_sp displaced_to() const {
    if (this->_displacedToP) return this->_Data;
    return _Nil<T_o>();
  }
  virtual cl_index displaced_index_offset() const {
    return this->_displacedIndexOffset;
  }
  virtual bool arrayHasFillPointerP() const { return this->_fillPointerP; };
  virtual void fillPointerSet(size_t idx) {
    if (this->_fillPointerP) {
      this->_fillPointerOrLength = idx;
    };
  };
  T_sp fillPointer() const {
    if (this->_fillPointerP) return clasp_make_fixnum(this->_fillPointerOrLength);
    return _Nil<T_o>();
  };
  virtual T_sp elementType() const { return this->_Data->elementType();};
  virtual void __write__(T_sp strm) const;
    
};

};

// ----------------------------------------------------------------------
//
// here go specialized mdarrays
//
namespace core {
  class MDArrayNs_o : public MDArray_o {
    LISP_CLASS(core, ClPkg, MDArrayNs_o, "mdarrayns",MDArray_o);
  public:
  };
};




// ----------------------------------------------------------------------
//
// here go Vector and Simple vectors
//
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

  class VectorNs_o : public MDArray_o {
    LISP_CLASS(core, ClPkg, VectorNs_o, "non-simple-vector",MDArray_o);
  public:
  VectorNs_o(List_sp dimensions,
             T_sp elementType,
             T_sp fillPointer,
             array_sp displacedTo,
             size_t displacedIndexOffset,
             T_sp initial_element,
             bool initial_element_supplied_p ) : Base(1,dimensions,elementType,fillPointer,displacedTo,displacedIndexOffset,initial_element,initial_element_supplied_p) {};
  public:
    virtual bool arrayHasFillPointerP() const { return this->_fillPointerP; };
    virtual void fillPointerSet(T_sp idx);
    T_sp fillPointer() const { if (this->_fillPointerP) return clasp_make_fixnum(this->_fillPointerOrLengthOrDummy); else return _Nil<T_o>(); };
  
    virtual void swapElements(cl_index i1, cl_index i2) {
      this->_Data->swapElements(i1+this->_displacedIndexOffset,i2+this->_displacedIndexOffset);
    }
    virtual T_sp elementType() const { return this->_Data->elementType(); };

    virtual T_sp elt(cl_index index) const { return this->_Data->rowMajorAref(index+this->_displacedIndexOffset); }
    virtual T_sp setf_elt(cl_index index, T_sp value) { return this->_Data->rowMajorAset(index+this->_displacedIndexOffset,value); }

    cl_index checkBounds(cl_index start, T_sp end, cl_index length) const;

    virtual T_sp subseq(cl_index start, T_sp end) const
    {
      cl_index iend = this->checkBounds(start,end,this->length());
      return this->_array->unsafe_subseq(start+this->_displacedIndexOffset,iend+this->_displacedIndexOffset);
    }
    virtual T_sp setf_subseq(cl_index start, T_sp end, T_sp new_subseq)
    {
      cl_index iend = this->checkBounds(start,end,this->length());
      return this->_array->unsafe_setf_subseq(start+this->_displacedIndexOffset,iend+this->_displacedIndexOffset,new_subseq);
    }
    virtual void fillArrayWithElt(T_sp element, cl_index start, T_sp end)
    {
      cl_index iend = this->checkBounds(start,end,this->length());
      this->_array->unsafe_fillArrayWithElt(element,start+this->_displacedIndexOffset,iend+this->_displacedIndexOffset);
    }
    virtual void fillInitialContents(T_sp initialContents)
    {
      this->_array->unsafe_fillInitialContents(this->_displacedIndexOffset,this->length()+this->_displacedIndexOffset, initialContents );
    }

    virtual void sxhash_(HashGenerator &hg) const
    {
      this->_vec->unsafe_sxhash_(this->_displacedIndexOffset,this->length()+this->_displacedIndexOffset, hg);
    }

    void ensurespaceAfterFillPointer(cl_index size);
    T_sp vectorPush(T_sp newElement);
    Fixnum_sp vectorPushExtend(T_sp newElement, cl_index extension = 0);

  };
}

namespace core {

  class Str8Ns_O : public VectorNs_O {
    LISP_CLASS(core, ClPkg, Str8Ns_O, "non-simple-string-base-char",VectorNs_O);
  public:
    // move all the constructors into here
    static Str8Ns_sp create_with_fill_pointer(char initial_element, size_t dimension, cl_index fill_pointer, bool adjustable);
  static Str8Ns_sp createBufferString(size_t bufferSize = BUFFER_STRING_SIZE) {
    return Str8Ns_O::create_with_fill_pointer(' ', bufferSize, 0, true);
  };
  public:
  static Str8Ns_sp create(cl_index dim, Str8Ns_sp displacedTo, cl_index displacedIndexOffset );
  static Str8Ns_sp create(const string &nm);
  static Str8Ns_sp create(const boost::format &nm);
  static Str8Ns_sp create(const char *nm);
  static Str8Ns_sp create(size_t numChars);
  static Str8Ns_sp create(const char *nm, cl_index numChars);
  static Str8Ns_sp create(claspChar initial_element, cl_index dimension);
  static Str8Ns_sp create(Str8Ns_sp orig);
    
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



// ----------------------------------------------------------------------
//
// String functions
//

namespace core {

  String_sp cl__string(T_sp str);
  String_sp cl__string_upcase(T_sp arg);
  String_sp cl__string_downcase(T_sp arg);
  String_sp cl__nstring_upcase(String_sp arg);
  String_sp cl__nstring_downcase(String_sp arg);
  claspChar cl__char(String_sp str, cl_index idx);

  bool clasp_memberChar(claspChar c, T_sp charBag);

  String_sp cl__string_trim(T_sp charbag, T_sp str);
  String_sp cl__string_left_trim(T_sp charbag, T_sp str);
  String_sp cl__string_right_trim(T_sp charbag, T_sp str);

  T_mv cl__parse_integer(Str_sp str, Fixnum start = 0, T_sp end = _Nil<T_O>(), uint radix = 10, T_sp junkAllowed = _Nil<T_O>());

  T_sp cl__string_equal(T_sp strdes1, T_sp strdes2, Fixnum_sp start1 = make_fixnum(0), T_sp end1 = _Nil<T_O>(), Fixnum_sp start2 = make_fixnum(0), T_sp end2 = _Nil<T_O>());


  String_sp make_string(T_sp element_type,
                        size_t dimension,
                        bool adjustable,
                        T_sp fill_pointer,
                        T_sp displaced_to,
                        cl_index displaced_index_offset,
                        T_sp initial_element,
                        T_sp initial_element_supplied_p);

};






#endif /* _core_Array_H */
