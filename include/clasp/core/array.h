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

namespace cl {
  extern core::Symbol_sp& _sym_sequence;
  extern core::Symbol_sp& _sym_array;
  extern core::Symbol_sp& _sym_vector;
  extern core::Symbol_sp& _sym_simple_base_string;
  extern core::Symbol_sp& _sym_simple_bit_vector;
  extern core::Symbol_sp& _sym_simple_vector;
  extern core::Symbol_sp& _sym_base_string;
};

namespace core {

  // ------------------------------------------------------------
  // Utility

  void noFillPointerError();
  void notVectorError();


  
#define INHERIT_SEQUENCE

class Array_O : public General_O {
  LISP_CLASS(core, ClPkg, Array_O, "array",General_O);
 public:
  /*! A very hackish way to get at the first element of all subclasses
      - which will be the FillPointer or the Length for vectors */
  size_t       _Length[0];  // A very hackish way to get at the first element of all subclasses
 public: // Functions here
  virtual T_sp type_as_symbol() = 0;
  size_t length() const { return this->_Length[0]; };
  virtual bool equalp(T_sp other) const { SUBIMP(); };
  
  CL_LISPIFY_NAME("cl:array-has-fill-pointer-p");
  CL_DEFMETHOD   virtual bool arrayHasFillPointerP() const { return false; };

  CL_LISPIFY_NAME("cl:array-total-size");
  CL_DEFMETHOD virtual size_t arrayTotalSize() const = 0;

  virtual T_sp elt(cl_index index) const = 0;
  virtual T_sp setf_elt(cl_index index, T_sp value) = 0;

  /*! This is to reach through to the simple vectors that contain everything
      and get a pointer to the element at index.   It follows the
      chain of displacements. */
  virtual void* addressOfElement(size_t index) const = 0;
  virtual size_t elementSizeInBytes() const = 0;
  
  virtual T_sp aset_unsafe(cl_index j, T_sp val) = 0;
  virtual T_sp aref_unsafe(cl_index index) const = 0;
  /*! Return the total capacity of the array */
  virtual std::vector<cl_index> dimensions() const { SUBIMP(); };

  CL_LISPIFY_NAME("core:rowMajorAset");
  CL_DEFMETHOD   virtual void rowMajorAset(cl_index idx, T_sp value) { SUBIMP(); };
 
  CL_LISPIFY_NAME("cl:rowMajorAref");
  CL_DEFMETHOD   virtual T_sp rowMajorAref(cl_index idx) const { SUBIMP(); };
 
  virtual cl_index arrayRowMajorIndex(VaList_sp indices) const = 0;

  //! Don't support adjustable arrays yet
  bool adjustable_array_p() const { return false; };
  //! Don't support displaced arrays yet
  bool _displaced_array_p() const { return false; }
  //! Don't support fill pointers yet
  bool array_has_fill_pointer_p() const { return false; }

  virtual LongLongInt setDimensions(List_sp dimensions, T_sp initialElement) { SUBIMP(); };
  virtual void swapElements(cl_index i1, cl_index i2) = 0;

  /*! Return the rank of the array */
  CL_LISPIFY_NAME("cl:array-rank");
  CL_DEFMETHOD   virtual size_t rank() const = 0;

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
  inline cl_index index_(List_sp indices) const {
    T_sp dummy;
    return this->index_val_(indices,false,dummy);
  };
  inline cl_index index_(VaList_sp indices) const {
    T_sp dummy;
    return this->index_val_(indices,false,dummy);
  };

  /*! Return the type returned by this array */
CL_LISPIFY_NAME("cl:array-element-type");
 CL_DEFMETHOD   virtual T_sp arrayElementType() = 0;

  /*! This replicates ECL ecl__elttype_to_symbol in array.d */

  Symbol_sp element_type_as_symbol() const;

  /*! Return the array dimension along the axis-number */
  CL_LISPIFY_NAME("cl:array-dimension");
 CL_DEFMETHOD   virtual size_t arrayDimension(size_t axisNumber) const = 0;

  /*! Return the array dimensions as a list of integers */
  virtual List_sp arrayDimensions() const = 0;

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
CL_DEFMETHOD void fillArrayWithElt(T_sp element, cl_index start, T_sp end) {
    cl_index iend = this->checkBounds(start,end,this->arrayTotalSize());
    this->unsafe_fillArrayWithElt(element,start,iend);
  }

CL_LISPIFY_NAME("core:replace-array");
CL_DEFMETHOD   virtual T_sp replace_array(T_sp other) {
    SUBCLASS_MUST_IMPLEMENT();
  };

 virtual T_sp elementType() const = 0;

  virtual void __write__(T_sp strm) const;

  virtual string __repr__() const;

  // ------------------------------------------------------------
  //
  // String functions
  virtual claspCharacter schar(cl_index index) const;
  virtual claspCharacter scharSet(cl_index index, claspCharacter c);
  virtual std::string get_std_string() const;
  std::string get() const {/*DEPRECIATE for get_std_string*/return this->get_std_string(); };

  // ------------------------------------------------------------
  //
  // Vector functions
  //
  virtual T_sp vectorPush(T_sp newElement) = 0;
  virtual Fixnum_sp vectorPushExtend(T_sp newElement, cl_index extension = 0) = 0;

  /*! The safe version - check if its a vector and
      check the arguments */
  T_sp subseq(cl_index start, T_sp end) const {
    if (this->rank()!=1) notVectorError();
    cl_index iend = this->checkBounds(start,end,this->length());
    return this->unsafe_subseq(start,iend);
  }
  
  virtual T_sp setf_subseq(cl_index start, T_sp end, T_sp new_subseq) {
    if (this->rank()!=1) notVectorError();
    cl_index iend = this->checkBounds(start,end,this->length());
    return this->unsafe_setf_subseq(start,iend,new_subseq);
  }
  virtual void fillInitialContents(T_sp initialContents) {
    this->unsafe_fillInitialContents(0,this->arrayTotalSize(),initialContents);
  }
  virtual void sxhash_(HashGenerator& hg) = 0;
  
  // --------------------------------------------------
  // Ranged operations with explicit limits
  virtual Array_sp unsafe_subseq(cl_index start, cl_index end) const = 0;
  virtual Array_sp unsafe_setf_subseq(cl_index start, cl_index end, T_sp newSubseq) = 0;
  virtual void unsafe_fillInitialContents(cl_index start, cl_index end,T_sp initialContents) = 0;
  virtual void unsafe_fillArrayWithElt(T_sp initial_element, cl_index start, cl_index end) = 0;
  
  /*! Signal an error if the value is out of bounds */
  cl_index checkBounds(cl_index start, T_sp end, cl_index length) const;

};


 
}; /* core */


namespace core {
  class MDArray_O : public Array_O {
    LISP_CLASS(core, ClPkg, MDArray_O, "mdarray",Array_O);
  public:
//! same offset as _fillpointerorlength in nonsimplevector
    size_t      _FillPointerOrLengthOrDummy;
    size_t      _ArrayTotalSize;
    Array_sp    _Data;
    size_t      _DisplacedIndexOffset;
    bool        _DisplacedToP;
    bool        _FillPointerP;
    gctools::GCArray_moveable<size_t> _Dimensions;
    MDArray_O(size_t rank,
              List_sp dimensions,
              T_sp elementType,
              T_sp fillPointer,
              Array_sp displacedTo,
              size_t displacedIndexOffset,
              T_sp initial_element,
              bool initial_element_supplied_p );
  public:
    virtual Array_sp data() { return this->_Data; };
    virtual bool adjustableArrayP() const { return true; }
  public:
    virtual size_t arrayTotalSize() const { return this->_ArrayTotalSize; };
    virtual T_sp displaced_to() const {
      if (this->_DisplacedToP) return this->_Data;
      return _Nil<T_O>();
    };
    virtual cl_index displaced_index_offset() const {return this->_DisplacedIndexOffset;}
    virtual bool arrayHasFillPointerP() const { return this->_FillPointerP; };
    virtual T_sp elt(cl_index index) const { TYPE_ERROR(this->asSmartPtr(),cl::_sym_sequence);};
    virtual T_sp setf_elt(cl_index index, T_sp value) { TYPE_ERROR(this->asSmartPtr(),cl::_sym_sequence);};
    virtual void fillPointerSet(size_t idx) {
      unlikely_if (!this->_FillPointerP) noFillPointerError();
      this->_FillPointerOrLengthOrDummy = idx;
    };
    size_t fillPointer() const {
      unlikely_if (this->_FillPointerP) noFillPointerError();
      return this->_FillPointerOrLengthOrDummy;
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
  class MDArrayNs_O : public MDArray_O {
    LISP_CLASS(core, ClPkg, MDArrayNs_O, "mdarrayns",MDArray_O);
  public:
    virtual T_sp type_as_symbol() { return cl::_sym_array; };
  };
};


namespace core {
  class ArrayTNs_O : public MDArrayNs_O {
    LISP_CLASS(core, ClPkg, ArrayTNs_O, "arraytns",MDArrayNs_O);
  public:
    static ArrayTNs_sp make(T_sp dim, T_sp elementType, T_sp initialElement, T_sp initialElementSuppliedP, T_sp displacedTo, Fixnum displacedIndexOffset );
    
  };
};




// ----------------------------------------------------------------------
//
// here go Vector and Simple vectors
//
namespace core {

  class BaseSimpleVector_O : public Array_O {
    LISP_CLASS(core, ClPkg, BaseSimpleVector_O, "base-simple-vector",Array_O);
  public:
    virtual Array_sp data() { return this->asSmartPtr(); };
    virtual size_t arrayTotalSize() const { return this->length(); };
    INHERIT_SEQUENCE virtual T_sp elt(cl_index index) const { return this->svref(index); };
    INHERIT_SEQUENCE virtual T_sp setf_elt(cl_index index, T_sp value) { return this->setf_svref(index,value); };
    virtual T_sp svref(cl_index index) const = 0;
    virtual T_sp setf_svref(cl_index index, T_sp value) = 0;
    INHERIT_SEQUENCE virtual void swap(cl_index i1, cl_index i2) const = 0;
  };
};

namespace core { class SimpleBaseString_O; };
template <>
struct gctools::GCInfo<core::SimpleBaseString_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = atomic;
};
namespace core {
  class SimpleBaseString_O : public BaseSimpleVector_O {
    LISP_CLASS(core, ClPkg, SimpleBaseString_O, "simple-base-string",BaseSimpleVector_O);
  public:
    typedef unsigned char value_type;
    typedef gctools::GCArray_moveable<value_type> vector_type;
    typedef value_type* iterator;
    typedef const value_type* const_iterator;
  public:
    vector_type _Data;
    // Always leave space for \0 at end
  SimpleBaseString_O(value_type initial_element, size_t total_size) : Base(), _Data(initial_element,total_size) {};
  //SimpleBaseString_O(size_t total_size) : Base(), _Data('\0',total_size+1) {};
  public:
    virtual T_sp type_as_symbol() { return cl::_sym_simple_base_string; };
    virtual cl_index length() const { return this->_Data._Capacity; };
  public:
    virtual void* addressOfElement(size_t index) const { return (void*)&((*this)[index]); };
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
    virtual T_sp type_as_symbol() { return cl::_sym_simple_vector; };
    virtual cl_index length() const { return this->_Data._Capacity; };
  public:
    virtual void* addressOfElement(size_t index) const { return (void*)&((*this)[index]); };
    value_type& operator[](size_t index) { return this->_Data[index];};
    const value_type& operator[](size_t index) const { return this->_Data[index];};
    iterator begin() { return &this->_Data[0];};
    iterator end() { return &this->_Data[this->_Data._Capacity]; }
    const_iterator begin() const { return &this->_Data[0];};
    const_iterator end() const { return &this->_Data[this->_Data._Capacity]; }
  public:
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
    virtual T_sp type_as_symbol() { return cl::_sym_simple_bit_vector; };
    virtual cl_index length() const { return this->_Data._Capacity; };
  public:
    virtual void* addressOfElement(size_t index) const { SIMPLE_ERROR(BF("bit-vector cannot support addressOfElement")); };
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


namespace core {
Vector_sp core__make_vector(T_sp element_type,
                            size_t dimension,
                            bool adjustable = false,
                            T_sp fill_pointer = _Nil<T_O>(),
                            T_sp displaced_to = _Nil<T_O>(),
                            cl_index displaced_index_offset = 0,
                            T_sp initial_element = _Nil<T_O>(),
                            bool initial_element_supplied_p = false);
};


namespace core {

  class VectorNs_O : public MDArray_O {
    LISP_CLASS(core, ClPkg, VectorNs_O, "non-simple-vector",MDArray_O);
  public:
  VectorNs_O(List_sp dimensions,
             T_sp elementType,
             T_sp fillPointer,
             Array_sp displacedTo,
             size_t displacedIndexOffset,
             T_sp initial_element,
             bool initial_element_supplied_p ) : Base(1,dimensions,elementType,fillPointer,displacedTo,displacedIndexOffset,initial_element,initial_element_supplied_p) {};
  public:
    virtual T_sp type_as_symbol() { return cl::_sym_vector; };
    virtual void swapElements(cl_index i1, cl_index i2) {
      this->_Data->swapElements(i1+this->_DisplacedIndexOffset,i2+this->_DisplacedIndexOffset);
    }

    virtual T_sp elt(cl_index index) const { return this->_Data->rowMajorAref(index+this->_DisplacedIndexOffset); }
    virtual T_sp setf_elt(cl_index index, T_sp value) { this->_Data->rowMajorAset(index+this->_DisplacedIndexOffset,value);
      return value;
    }

    virtual Array_sp unsafe_subseq(cl_index start, cl_index iend) const
    {
      return this->_Data->unsafe_subseq(start+this->_DisplacedIndexOffset,iend+this->_DisplacedIndexOffset);
    }
    virtual Array_sp unsafe_setf_subseq(cl_index start, cl_index iend, T_sp new_subseq)
    {
      return this->_Data->unsafe_setf_subseq(start+this->_DisplacedIndexOffset,iend+this->_DisplacedIndexOffset,new_subseq);
    }
    virtual void unsafe_fillArrayWithElt(T_sp element, cl_index start, cl_index end)
    {
      this->_Data->unsafe_fillArrayWithElt(element,start+this->_DisplacedIndexOffset,end+this->_DisplacedIndexOffset);
    }
    virtual void sxhash_(HashGenerator &hg) const;

    void ensureSpaceAfterFillPointer(cl_index size);
    T_sp vectorPush(T_sp newElement);
    Fixnum_sp vectorPushExtend(T_sp newElement, cl_index extension = 0);

    // Old functions
    void adjust(T_sp initial_element, size_t dimension);
  };
}

namespace core {
  class Str8Ns_O : public VectorNs_O {
    LISP_CLASS(core, ClPkg, Str8Ns_O, "base-string",VectorNs_O);
  public:
    // The types that define what this class does
    typedef Str8Ns_O my_type;
    typedef SimpleBaseString_O my_simple_type;
    typedef unsigned char value_type;
    typedef gctools::smart_ptr<my_type> smart_ptr_type;
  public:
      virtual void* addressOfElement(size_t index) const {
      unlikely_if (gc::IsA<smart_ptr_type>(this->_Data)) {
        my_type& vecns = *reinterpret_cast<my_type*>(&*this->_Data);
        // It's another VectorTNs - we need to deal with multiple indirection
        return (void*)&vecns[this->_DisplacedIndexOffset+index];
      }
      // It's a SimpleVector_O - so we can index into it directly
      my_simple_type& simple_vec = *reinterpret_cast<my_simple_type*>(&*this->_Data);
      return &simple_vec[this->_DisplacedIndexOffset+index];
    }
    value_type& operator[](size_t index) {
      // FIXME:   This is where I get fancy and dangerous
      // Get a pointer to the data element
      void* address = this->addressOfElement(index);
      return *reinterpret_cast<value_type*>(address);
    }
    const value_type& operator[](size_t index) const {
      // FIXME:   This is where I get fancy and dangerous
      // Get a pointer to the data element
      void* address = this->addressOfElement(index);
      return *reinterpret_cast<value_type*>(address);
    }
  public:
    // move all the constructors into here
    static Str8Ns_sp create_with_fill_pointer(value_type initial_element, size_t dimension, cl_index fill_pointer, bool adjustable);
  static Str8Ns_sp createBufferString(size_t bufferSize = BUFFER_STRING_SIZE) {
    return Str8Ns_O::create_with_fill_pointer(value_type()/*' '*/, bufferSize, 0, true);
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
  public:
    virtual T_sp type_as_symbol() { return cl::_sym_base_string; };
  public:
  // Return a pointer to the zero terminated string - do not use this for
  // anything but legacy calls.
  const char* c_str() const;
  std::string get() { /*DEPRECIATE for get_std_string */ return this->get_std_string(); };
  void* addressOfBuffer();
  };
};







namespace core {
  class VectorTNs_O : public VectorNs_O {
    LISP_CLASS(core, ClPkg, VectorTNs_O, "non-simple-vector-t",VectorNs_O);
  public:
    // The types that define what this class does
    typedef VectorTNs_O my_type;
    typedef SimpleVector_O my_simple_type;
    typedef gctools::smart_ptr<my_type> smart_ptr_type;
    typedef T_sp value_type;
  public:
    static VectorObjects_sp make(T_sp initial_element, size_t dimension, T_sp elementType, T_sp fillPointer= _Nil<core::T_O>(), T_sp displacedTo=_Nil<core::T_O>(), Fixnum displacedIndexOffset=0 );
    static VectorTNs_sp create(T_sp initial_element, size_t dimension, T_sp elementType);
    static VectorTNs_sp create(const gctools::Vec0<T_sp> &objs);
  public:
    virtual void* addressOfElement(size_t index) const {
      unlikely_if (gc::IsA<smart_ptr_type>(this->_Data)) {
        my_type& vecns = *reinterpret_cast<my_type*>(&*this->_Data);
        // It's another VectorTNs - we need to deal with multiple indirection
        return (void*)&vecns[this->_DisplacedIndexOffset+index];
      }
      // It's a SimpleVector_O - so we can index into it directly
      my_simple_type& simple_vec = *reinterpret_cast<my_simple_type*>(&*this->_Data);
      return &simple_vec[this->_DisplacedIndexOffset+index];
    }
    value_type& operator[](size_t index) {
      // FIXME:   This is where I get fancy and dangerous
      // Get a pointer to the data element
      void* address = this->addressOfElement(index);
      return *reinterpret_cast<value_type*>(address);
    }
    const value_type& operator[](size_t index) const {
      // FIXME:   This is where I get fancy and dangerous
      // Get a pointer to the data element
      void* address = this->addressOfElement(index);
      return *reinterpret_cast<value_type*>(address);
    }
  public:
    value_type* begin() { return &(*this)[0]; };
    value_type* end() { return &(*this)[this->length()]; };
    const value_type* begin() const { return &(*this)[0]; };
    const value_type* end() const { return &(*this)[this->length()]; };
  };
};

namespace core {
  class BitVectorNs_O : public VectorNs_O {
    LISP_CLASS(core, ClPkg, BitVectorNs_O, "bit-vector",VectorNs_O);
  public:
  static BitVectorNs_sp create(T_sp initial_element, size_t dimension);
  public:
    virtual void* addressOfElement(size_t index) const { SIMPLE_ERROR(BF("bit-vector cannot support addressOfElement")); };
    virtual void __write__(T_sp strm) const;
    uint testBit(cl_index idx) const;
    void setBit(cl_index i, uint v);
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

  /*! Push a c-style string worth of characters into the buffer */
  void StringPushStringCharStar(String_sp buffer, const char* cp);
  // Was Str_O::pushStringSubstring
  void StringPushSubString(String_sp buffer, String_sp other, cl_index start, cl_index end);
  // Was Str_O::pushString
  void StringPushString(String_sp buffer, String_sp other);
  /*! Search outer for the first occurrence of inner starting at start of outer
      This function should be depreciated - look into it */
  T_sp StrFind(Str_sp outer, Str_sp inner, size_t start);

  Vector_sp VectorReverse(Vector_sp vec);
  Vector_sp VectorNReverse(Vector_sp vec);


  BitVector_sp make_bit_vector(uint initial_element, size_t arrayTotalSize);
  
};




#endif /* _core_Array_H */
