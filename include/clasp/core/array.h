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
#include <clasp/core/character.fwd.h>
#include <clasp/core/sequence.fwd.h>
#include <clasp/core/corePackage.fwd.h>

namespace cl {
  extern core::Symbol_sp& _sym_General_O;
  extern core::Symbol_sp& _sym_sequence;
  extern core::Symbol_sp& _sym_array;
  extern core::Symbol_sp& _sym_vector;
  extern core::Symbol_sp& _sym_vectorPush;
  extern core::Symbol_sp& _sym_vectorPushExtend;
  extern core::Symbol_sp& _sym_simple_base_string;
  extern core::Symbol_sp& _sym_simple_bit_vector;
  extern core::Symbol_sp& _sym_simple_vector;
  extern core::Symbol_sp& _sym_base_string;
  extern core::Symbol_sp& _sym_subseq;
  extern core::Symbol_sp& _sym_string;
  extern core::Symbol_sp& _sym_fillPointer;
  extern core::Symbol_sp& _sym_arrayDimension;
  extern core::Symbol_sp& _sym_reverse;
  extern core::Symbol_sp& _sym_nreverse;
  extern core::Symbol_sp& _sym_base_char;
  extern core::Symbol_sp& _sym_character;
  extern core::Symbol_sp& _sym_bit_vector;
  extern core::Symbol_sp& _sym_bit;
  extern core::Symbol_sp& _sym_float;
  extern core::Symbol_sp& _sym_double_float;
  extern core::Symbol_sp& _sym_single_float;
  extern core::Symbol_sp& _sym_T_O;
  extern core::Symbol_sp& _sym_simple_string;
};
namespace core {
  extern core::Symbol_sp& _sym_replaceArray;
  extern core::Symbol_sp& _sym_swapElements;
  extern core::Symbol_sp& _sym_fillPointerSet;
  extern core::Symbol_sp& _sym_fillArrayWithElt;
  extern core::Symbol_sp& _sym_setf_subseq;
  extern core::Symbol_sp& _sym_simple_double_vector;
  extern void clasp_write_string(const string &str, T_sp strm );
  extern claspCharacter clasp_write_char(claspCharacter c, T_sp strm);
};
  

namespace core {

  // ------------------------------------------------------------
  // Utility

  [[noreturn]] void bitVectorDoesntSupportError();
  [[noreturn]] void missingValueListError(List_sp indices);
  [[noreturn]] void tooManyIndicesListError(List_sp indices);
  [[noreturn]] void missingValueVaListError(VaList_sp indices);
  [[noreturn]] void tooManyIndicesVaListError(VaList_sp indices);
  [[noreturn]] void badAxisNumberError(Symbol_sp fn_name, size_t rank, size_t axisNumber);
  [[noreturn]] void badIndexError(size_t oneIndex, size_t curDimension);
  [[noreturn]] void indexNotFixnumError(T_sp index);
  [[noreturn]] void insufficientIndexListError(List_sp indices);
  [[noreturn]] void insufficientIndexVaListError(VaList_sp indices);
  [[noreturn]] void notStringError(T_sp obj);
  [[noreturn]] void notSequenceError(T_sp obj);
  [[noreturn]] void noFillPointerError(Symbol_sp fn_name, T_sp array);
  [[noreturn]] void vectorNotArrayError(Symbol_sp fn_name, T_sp array);
  [[noreturn]] void notAdjustableError(Symbol_sp fn_name, T_sp array);
  [[noreturn]] void notVectorError(T_sp array);
  bool ranged_bit_vector_EQ_(const SimpleBitVector_O& x, const SimpleBitVector_O& y, size_t startx, size_t endx, size_t starty, size_t endy );

  template <typename T1,typename T2>
    bool template_string_EQ_equal(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2)
  {
    const typename T1::value_type* cp1(&string1[start1]);
    const typename T2::value_type* cp2(&string2[start2]);
    size_t num1 = end1 - start1;
    size_t num2 = end2 - start2;
//    printf("%s:%d:%s string1@%p string2=@%p sizeof(*cp1)=%lu sizeof(*cp2)=%lu cp1=%p cp2=%p start1=%lu end1=%lu start2=%lu end2=%lu num1=%lu num2=%lu\n", __FILE__, __LINE__, __FUNCTION__, (void*)&string1, (void*)&string2, sizeof(*cp1), sizeof(*cp2), (void*)cp1, (void*)cp2, start1, end1, start2, end2, num1, num2);
    while (1) {
      if (num1 == 0)
        goto END_STRING1;
      if (num2 == 0)
        goto END_STRING2;
      if ((static_cast<claspCharacter>(*cp1) != static_cast<claspCharacter>(*cp2)))
        goto RETURN_FALSE;
      --num1;
      --num2;
      ++cp1;
      ++cp2;
    }
  END_STRING1:
    if (num2 == 0)
      goto RETURN_TRUE;
    goto RETURN_FALSE;
  END_STRING2:
  RETURN_FALSE:
    return false;
  RETURN_TRUE:
    return true;
  }

  template <class SimpleType>
    Array_sp templated_ranged_reverse(const SimpleType& me, size_t start, size_t end) {
    size_t meLength = end - start;
    size_t lastElement = meLength - 1;
    gctools::smart_ptr<SimpleType> newVec = SimpleType::make(meLength);
    for (size_t i = start; i < meLength; ++i) {
      size_t ri = lastElement-i;
      (*newVec)[ri] = me[i];
    }
    return newVec;
  }
  template <class T>
    void templated_swapElements(T& x, size_t xi, size_t yi) {
    typename T::value_type u = x[xi];
    x[xi] = x[yi];
    x[yi] = u;
  }
  template <class MaybeTemplatedSimpleType>
    Array_sp templated_ranged_nreverse(MaybeTemplatedSimpleType& me,size_t start, size_t end) {
    size_t meLength = end-start;
    size_t halfLength = meLength/2;
    size_t lastElement = meLength - 1;
    for (size_t i = start; i < halfLength; ++i) {
      size_t ri = lastElement-i;
      templated_swapElements(me,i,ri);
    }
    return me.asSmartPtr();
  }

  template <class T>
    Array_sp templated_reverse_VectorNs(T& me) {
    BaseSimpleVector_sp bsv;
    size_t start, end;
    me.asBaseSimpleVectorRange(bsv,start,end);
    auto sv = gc::As_unsafe<gctools::smart_ptr<typename T::simple_type>>(bsv);
    return templated_ranged_reverse<typename T::simple_type>(*sv,start,end);
  }

  template <class T>
    Array_sp templated_nreverse_VectorNs(T& me) {
    BaseSimpleVector_sp bsv;
    size_t start, end;
    me.asBaseSimpleVectorRange(bsv,start,end);
    auto sv = gc::As_unsafe<gctools::smart_ptr<typename T::simple_type>>(bsv);
    return templated_ranged_nreverse(*sv,start,end);
  }

};

namespace core {
class Array_O : public General_O {
  LISP_CLASS(core, ClPkg, Array_O, "array",General_O);
 public:
  /*! A hackish (clever?) way to get at the first element of all subclasses
      - which will be the FillPointer/Length for vectors and a Dummy for Arrays.
      - The first field of every subclass needs to be a size_t length/fillPointer.
 */
  size_t       _Length[0];
 public:
  // Low level functions for access to contents
  virtual size_t elementSizeInBytes() const = 0;
  virtual void* rowMajorAddressOfElement_(size_t index) const = 0;
  virtual void asBaseSimpleVectorRange(BaseSimpleVector_sp& sv, size_t& start, size_t& end) const = 0;
 public: // Functions here
  virtual T_sp type_as_symbol() const = 0;
  /*! length() doesn't dispatch - it reaches into the subclass
      through the _Length[0] array to read the first size_t element
      which is the Length/FillPointer for vectors and a Dummy value for arrays */
  size_t length() const { return this->_Length[0]; };
  virtual bool equal(T_sp other) const = 0;
  virtual bool equalp(T_sp other) const = 0;
  virtual size_t arrayTotalSize() const = 0;
  virtual void rowMajorAset(size_t idx, T_sp value) = 0;
  virtual T_sp rowMajorAref(size_t idx) const = 0;
  virtual bool adjustableArrayP() const { return false; };
  virtual bool displacedToP() const { return false; };
  size_t arrayRowMajorIndex(VaList_sp indices) const {
    return this->index_(indices);
  }
  virtual LongLongInt setDimensions(List_sp dimensions, T_sp initialElement) { SUBIMP(); };
  virtual Array_sp reverse() const = 0;
  virtual Array_sp nreverse() = 0;
  virtual size_t rank() const = 0;
  /*! Return the offset into a one-dimensional vector for the multidimensional index
      in the vector<int>s.  This is in rowMajor order.*/
  size_t index_vector_int(const vector<int> &indices) const;
  /*! Return the offset into a one-dimensional vector for a multidimensional index
	 If last_value_is_val == true then don't use the last value in the indices list */
  size_t index_val_(List_sp indices, bool last_value_is_val, T_sp &last_val) const;
  size_t index_val_(VaList_sp indices, bool last_value_is_val, T_sp &last_val) const;
  /*! Return the offset into a one-dimensional vector for a multidimensional index */
  inline size_t index_(List_sp indices) const {T_sp dummy; return this->index_val_(indices,false,dummy);};
  inline size_t index_(VaList_sp indices) const {T_sp dummy; return this->index_val_(indices,false,dummy);};
  /*! Return the type returned by this array */
  virtual T_sp arrayElementType() const = 0;
  virtual bool arrayHasFillPointerP() const { return false; };
  virtual void fillPointerSet(size_t f) {noFillPointerError(cl::_sym_fillPointer,this->asSmartPtr());};
  virtual size_t fillPointer() const {noFillPointerError(cl::_sym_fillPointer,this->asSmartPtr());};
  /*! This replicates ECL ecl__elttype_to_symbol in array.d */
  virtual Symbol_sp elementTypeAsSymbol() const = 0;
  virtual size_t displacedIndexOffset() const = 0;
  /*! Return the array dimension along the axis-number */
  virtual size_t arrayDimension(size_t axisNumber) const = 0;
  /*! Return the value at the indices */
  virtual T_sp replaceArray(T_sp other) = 0;
  virtual void __write__(T_sp strm) const = 0;
  virtual string __repr__() const;
  // ------------------------------------------------------------
  //
  // String functions
  virtual std::string get_std_string() const = 0;
  std::string get() const {/*DEPRECIATE for get_std_string*/return this->get_std_string(); };
  virtual vector<size_t> arrayDimensionsAsVector() const = 0;
  // ------------------------------------------------------------
  //
  // Vector functions
  //
  virtual T_sp vectorPush(T_sp newElement) = 0;
  virtual Fixnum_sp vectorPushExtend(T_sp newElement, size_t extension = 0) = 0;
  /*! The safe version - check if its a vector and
      check the arguments */
  T_sp subseq(size_t start, T_sp end) const {
    if (this->rank()!=1) notVectorError(this->asSmartPtr());
    size_t_pair p = sequenceStartEnd(cl::_sym_subseq,this->length(),start,end);
    return this->unsafe_subseq(p.start,p.end);
  }
  virtual T_sp setf_subseq(size_t start, T_sp end, T_sp new_subseq) {
    if (this->rank()!=1) notVectorError(this->asSmartPtr());
    size_t_pair p = sequenceStartEnd(core::_sym_setf_subseq,this->length(),start,end);
    Array_sp newVec = gc::As<Array_sp>(new_subseq);
    return this->unsafe_setf_subseq(p.start,p.end,newVec);
  }
  void fillInitialContents(T_sp initialContents);
  virtual void sxhash_(HashGenerator& hg) const = 0;
  // --------------------------------------------------
  // Ranged operations with explicit limits
  virtual Array_sp unsafe_subseq(size_t start, size_t end) const = 0;
  virtual Array_sp unsafe_setf_subseq(size_t start, size_t end, Array_sp newSubseq) = 0;
  virtual void unsafe_fillArrayWithElt(T_sp initial_element, size_t start, size_t end) = 0;
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
              T_sp fillPointer,
              T_sp displacedTo,
              size_t displacedIndexOffset);
  public:
    virtual Array_sp data() const { return this->_Data;};
    void set_data(Array_sp a);
  public:
    virtual size_t elementSizeInBytes() const override { return this->_Data->elementSizeInBytes(); };
    virtual void* rowMajorAddressOfElement_(size_t index) const override { return this->_Data->rowMajorAddressOfElement_(index+this->_DisplacedIndexOffset); };
    virtual bool adjustableArrayP() const { return true; }
    virtual bool displacedToP() const { return this->_DisplacedToP; };
  public:
    virtual size_t arrayTotalSize() const { return this->_ArrayTotalSize; };
    virtual T_sp displacedTo() const {
      if (this->_DisplacedToP) return this->_Data;
      return _Nil<T_O>();
    };

    virtual size_t rank() const override { return this->_Dimensions._Length; };
    virtual Symbol_sp elementTypeAsSymbol() const override { return this->_Data->elementTypeAsSymbol(); };
    virtual size_t arrayDimension(size_t axisNumber) const override {
      LIKELY_if (axisNumber<this->_Dimensions._Length) return this->_Dimensions[axisNumber];
      badAxisNumberError(cl::_sym_arrayDimension,this->_Dimensions._Length,axisNumber);
      
    };
    virtual size_t displacedIndexOffset() const override {return this->_DisplacedIndexOffset;}
    virtual T_sp arrayElementType() const override { return this->_Data->arrayElementType();};
    virtual bool arrayHasFillPointerP() const override { return this->_FillPointerP; };
    virtual T_sp replaceArray(T_sp other) override { this->set_data(gc::As<Array_sp>(other)); return this->asSmartPtr(); };
    void fillPointerSet(size_t idx) {
      this->_FillPointerOrLengthOrDummy = idx;
    };
    size_t fillPointer() const {
      return this->_FillPointerOrLengthOrDummy;
    };
    virtual void __write__(T_sp strm) const override;
    virtual std::string get_std_string() const {notStringError(this->asSmartPtr()); }
    virtual vector<size_t> arrayDimensionsAsVector() const {
      vector<size_t> dims;
      for (size_t i; i<this->_Dimensions._Length; ++i ) {
        dims.push_back(this->_Dimensions[i]);
      }
      return dims;
    }
    virtual void internalAdjustSize_(size_t size, T_sp init_element=_Nil<T_O>(), bool initElementSupplied=false ) = 0;
    virtual void unsafe_fillArrayWithElt(T_sp element, size_t start, size_t end) final
    {
      this->_Data->unsafe_fillArrayWithElt(element,start+this->_DisplacedIndexOffset,end+this->_DisplacedIndexOffset);
    }
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
  MDArrayNs_O(size_t rank,
              List_sp dimensions,
              T_sp fillPointer,
              T_sp displacedTo,
              size_t displacedIndexOffset) : Base(rank,dimensions,fillPointer,displacedTo,displacedIndexOffset) {};
  public:
    virtual Array_sp reverse() const final {notSequenceError(this->asSmartPtr());};
    virtual Array_sp nreverse() final {notSequenceError(this->asSmartPtr());};
    virtual T_sp vectorPush(T_sp newElement)  override {notVectorError(this->asSmartPtr());};;
    virtual Fixnum_sp vectorPushExtend(T_sp newElement, size_t extension = 0)  override {notVectorError(this->asSmartPtr());};
    virtual Array_sp unsafe_subseq(size_t start, size_t end) const override {notVectorError(this->asSmartPtr());};
    virtual Array_sp unsafe_setf_subseq(size_t start, size_t end, Array_sp newSubseq) override {notVectorError(this->asSmartPtr());};
  };
};

namespace core {
  class SpecializedMDArrayNs_O : public MDArrayNs_O {
    LISP_CLASS(core, ClPkg, SpecializedMDArrayNs_O, "specialized-mdarrayns",MDArrayNs_O);
  public:
  SpecializedMDArrayNs_O(size_t rank,
                         List_sp dimensions,
                         T_sp fillPointer,
                         T_sp displacedTo,
                         size_t displacedIndexOffset) : Base(rank,dimensions,fillPointer,displacedTo,displacedIndexOffset) {};
  public:
    virtual T_sp type_as_symbol() const final { return cl::_sym_array; };
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
    virtual void rowMajorAset(size_t idx, T_sp value) = 0;
    virtual T_sp rowMajorAref(size_t idx) const = 0;
    virtual size_t rank() const override { return 1; };
    virtual size_t displacedIndexOffset() const override { return 0; };
    virtual size_t arrayDimension(size_t axisNumber) const override {
      unlikely_if (axisNumber!=0) { vectorNotArrayError(cl::_sym_arrayDimension,this->asSmartPtr()); }
      return this->length();
    }
    virtual T_sp vectorPush(T_sp newElement) override {noFillPointerError(cl::_sym_vectorPush,this->asSmartPtr());  };
    virtual Fixnum_sp vectorPushExtend(T_sp newElement, size_t extension) override {noFillPointerError(cl::_sym_vectorPushExtend,this->asSmartPtr());  };
    virtual T_sp replaceArray(T_sp other) override {notAdjustableError(core::_sym_replaceArray,this->asSmartPtr());  };
    virtual void ranged_sxhash(HashGenerator& hg, size_t start, size_t end) const = 0;
    virtual std::string get_std_string() const override {notStringError(this->asSmartPtr());};

  };
};

namespace core {
  FORWARD(SpecializedSimpleVector);
  class SpecializedSimpleVector_O : public BaseSimpleVector_O {
    LISP_CLASS(core, ClPkg, SpecializedSimpleVector_O, "specialized-simple-vector",BaseSimpleVector_O);
  public:
     // ranged_sxhash is only appropriate for string and bit-vector
     virtual void ranged_sxhash(HashGenerator& hg, size_t start, size_t end) const final
     {TYPE_ERROR(this->asSmartPtr(),Cons_O::createList(cl::_sym_string,cl::_sym_bit_vector));};
     virtual void sxhash_(HashGenerator& hg) const final {this->General_O::sxhash_(hg);}
     virtual bool equalp(T_sp other) const;
  };
};



namespace core {
  template <typename MyLeafType, typename ValueType, typename MyParentType >
    class abstract_SimpleVector : public MyParentType {
  public:
    // The types that define what this class does
    typedef MyParentType Base;
    typedef MyLeafType /* eg: SimpleVector_O*/ leaf_type;
    typedef ValueType  /*eg: T_sp*/ value_type;
    typedef gctools::smart_ptr<leaf_type> leaf_smart_ptr_type;
    typedef gctools::GCArray_moveable<value_type> vector_type;
    typedef value_type* iterator;
    typedef const value_type* const_iterator;
  public:
    vector_type _Data;
  public:
  abstract_SimpleVector(size_t length, value_type initialElement=value_type(), bool initialElementSupplied=false, size_t initialContentsSize=0, const value_type* initialContents=NULL)
    : Base(), _Data(length,initialElement,initialElementSupplied,initialContentsSize,initialContents) {};
  public:
    static void never_invoke_allocator() {gctools::GCAbstractAllocator<abstract_SimpleVector>::never_invoke_allocator();};
  public:
    value_type& operator[](size_t index) { return this->_Data[index];};
    const value_type& operator[](size_t index) const { return this->_Data[index];};
    iterator begin() { return &this->_Data[0];};
    iterator end() { return &this->_Data[this->_Data._Capacity]; }
    const_iterator begin() const { return &this->_Data[0];};
    const_iterator end() const { return &this->_Data[this->_Data._Length]; }
    virtual size_t elementSizeInBytes() const override {return sizeof(value_type); };
    virtual void* rowMajorAddressOfElement_(size_t i) const override {return (void*)&(*this)[i];};
    void asBaseSimpleVectorRange(BaseSimpleVector_sp& sv, size_t& start, size_t& end) const override {
      sv = this->asSmartPtr();
      start = 0;
      end = this->length();
    }
    virtual vector<size_t> arrayDimensionsAsVector() const {
      vector<size_t> dims;
      dims.push_back(this->length());
      return dims;
    }
    virtual void unsafe_fillArrayWithElt(T_sp initialElement, size_t start, size_t end) override {
      for (size_t i(start); i<end; ++i ) {
        (*this)[i] = leaf_type::from_object(initialElement);
      }
    };
    virtual Array_sp reverse() const final { return templated_ranged_reverse<leaf_type>(*reinterpret_cast<const leaf_type*>(this),0,this->length()); };
    virtual Array_sp nreverse() final { return templated_ranged_nreverse(*this,0,this->length()); };
    CL_METHOD_OVERLOAD virtual void rowMajorAset(size_t idx, T_sp value) final {(*this)[idx] = leaf_type::from_object(value);}
    CL_METHOD_OVERLOAD virtual T_sp rowMajorAref(size_t idx) const final {return leaf_type::to_object((*this)[idx]);}
    virtual Array_sp unsafe_subseq(size_t start, size_t end) const final {
      return leaf_type::make(end-start,value_type(),true,end-start,&(*this)[start]);
    }
    virtual Array_sp unsafe_setf_subseq(size_t start, size_t end, Array_sp newSubseq) final {
      // TODO: Write specialized versions of this to speed it up
      for ( size_t i(start),ni(0); i<end; ++i,++ni ) {
        (*this)[i] = leaf_type::from_object(newSubseq->rowMajorAref(ni));
      }
      return newSubseq;
    }
    virtual void __write__(T_sp strm) const override {
      clasp_write_string("#<",strm);
      clasp_write_string(this->className(),strm);
      clasp_write_char('>',strm);
    };
  };
};
namespace core { class SimpleString_O; };
template <>
struct gctools::GCInfo<core::SimpleString_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = atomic;
};
namespace core {
  class SimpleString_O : public BaseSimpleVector_O {
    LISP_CLASS(core, ClPkg, SimpleString_O, "simple-string",BaseSimpleVector_O);
  };
};

namespace core { class SimpleBaseCharString_O; };
template <>
struct gctools::GCInfo<core::SimpleBaseCharString_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = atomic;
};
namespace core {
  class SimpleBaseCharString_O;
  typedef abstract_SimpleVector<SimpleBaseCharString_O,claspChar,SimpleString_O> specialized_SimpleBaseCharString;
  class SimpleBaseCharString_O : public specialized_SimpleBaseCharString {
    LISP_CLASS(core, ClPkg, SimpleBaseCharString_O, "SimpleBaseCharString",SimpleString_O);
  public:
    typedef specialized_SimpleBaseCharString TemplatedBase;
    typedef typename TemplatedBase::leaf_type leaf_type;
    typedef typename TemplatedBase::value_type value_type;
    typedef typename TemplatedBase::vector_type vector_type;
    typedef typename TemplatedBase::iterator iterator;
    typedef typename TemplatedBase::const_iterator const_iterator;
    typedef value_type container_value_type;
  public:
    static value_type initial_element_from_object(T_sp obj, bool supplied);
    static value_type from_object(T_sp obj) {return obj.unsafe_character();}
    static T_sp to_object(const value_type& v) { return clasp_make_character(v); };
  public:
    // Always leave space for \0 at end
  SimpleBaseCharString_O(size_t length, value_type initialElement=value_type(), bool initialElementSupplied=false, size_t initialContentsSize=0, const value_type* initialContents=NULL) : TemplatedBase(length,initialElement,initialElementSupplied,initialContentsSize,initialContents) {};
    static SimpleBaseCharString_sp make(size_t length, value_type initialElement='\0', bool initialElementSupplied=false, size_t initialContentsSize=0, const value_type* initialContents=NULL) {
      // For C/C++ interop make SimpleBaseCharString 1 character longer and append a \0
      auto bs = gctools::GC<SimpleBaseCharString_O>::allocate_container(gctools::GCStamp<SimpleBaseCharString_O>::TheStamp,
                                                                        length+1,length,initialElement,initialElementSupplied,initialContentsSize,initialContents);
      (*bs)[length] = '\0';
      return bs;
    }
    static SimpleBaseCharString_sp make(const std::string& str) {
      return SimpleBaseCharString_O::make(str.size(),'\0',true,str.size(),(const claspChar*)str.c_str());
    }
  //SimpleBaseCharString_O(size_t total_size) : Base(), _Data('\0',total_size+1) {};
  public:
    virtual T_sp type_as_symbol() const final { return cl::_sym_simple_base_string; };
    virtual T_sp arrayElementType() const final { return cl::_sym_base_char; };
    virtual Symbol_sp elementTypeAsSymbol() const final { return cl::_sym_base_char; };
    virtual bool equal(T_sp other) const final;
    virtual bool equalp(T_sp other) const final;
    virtual void __write__(T_sp strm) const final; // implemented in write_array.cc
    virtual std::string get_std_string() const final { return string((char*)&(*this)[0],this->length());};
    virtual std::string __repr__() const { return this->get_std_string(); };
    virtual void sxhash_(HashGenerator& hg) const final {this->ranged_sxhash(hg,0,this->length());}
    virtual void ranged_sxhash(HashGenerator& hg, size_t start, size_t end) const final {
      if (hg.isFilling()) {
        Fixnum hash = 5381;
        Fixnum c;
        for ( size_t i(start); i<end; ++i ) {
          const value_type& c = (*this)[i];
          hash = ((hash << 5) + hash) + c;
        }
        hg.addPart(hash);
      }
    }
  };
};

namespace core { class SimpleCharacterString_O; };
template <>
struct gctools::GCInfo<core::SimpleCharacterString_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = atomic;
};
namespace core {
  class SimpleCharacterString_O;
  typedef abstract_SimpleVector<SimpleCharacterString_O,claspCharacter,SimpleString_O> specialized_SimpleCharacterString;
  class SimpleCharacterString_O : public specialized_SimpleCharacterString {
    LISP_CLASS(core, ClPkg, SimpleCharacterString_O, "simple-character-string",SimpleString_O);
  public:
    typedef specialized_SimpleCharacterString TemplatedBase;
    typedef typename TemplatedBase::leaf_type leaf_type;
    typedef typename TemplatedBase::value_type value_type;
    typedef typename TemplatedBase::vector_type vector_type;
    typedef typename TemplatedBase::iterator iterator;
    typedef typename TemplatedBase::const_iterator const_iterator;
    typedef value_type container_value_type;
  public:
    static value_type initial_element_from_object(T_sp obj, bool supplied);
    static value_type from_object(T_sp obj) {return obj.unsafe_character();}
    static T_sp to_object(const value_type& v) { return clasp_make_character(v); };
  public:
    // Always leave space for \0 at end
  SimpleCharacterString_O(size_t length, value_type initialElement=value_type(), bool initialElementSupplied=false, size_t initialContentsSize=0, const value_type* initialContents=NULL) : TemplatedBase(length,initialElement,initialElementSupplied,initialContentsSize,initialContents) {};
    static SimpleCharacterString_sp make(size_t length, value_type initialElement='\0', bool initialElementSupplied=false, size_t initialContentsSize=0, const value_type* initialContents=NULL) {
      auto bs = gctools::GC<SimpleCharacterString_O>::allocate_container(gctools::GCStamp<SimpleCharacterString_O>::TheStamp,
                                                                         length,length,initialElement,initialElementSupplied,initialContentsSize,initialContents);
      return bs;
    }
    static SimpleCharacterString_sp make(const std::string& str) {
      auto bs = SimpleCharacterString_O::make(str.size(),'\0');
      for ( size_t i(0); i<str.size(); ++i ) {
        (*bs)[i] = str[i];
      }
      return bs;
    }
  //SimpleCharacterString_O(size_t total_size) : Base(), _Data('\0',total_size+1) {};
  public:
    virtual T_sp type_as_symbol() const final { return cl::_sym_simple_string; };
    virtual T_sp arrayElementType() const override { return cl::_sym_character; };
    virtual Symbol_sp elementTypeAsSymbol() const override { return cl::_sym_character; };
  public:
    // Implement these methods for simple vectors - some are implemented in parent classes
    // for convenience if not speed
    virtual void __write__(T_sp strm) const final;
    virtual bool equal(T_sp other) const final;
    virtual bool equalp(T_sp other) const final;
    virtual std::string get_std_string() const final;
    virtual std::string __repr__() const final;
  public:
    virtual void sxhash_(HashGenerator& hg) const override {this->ranged_sxhash(hg,0,this->length());}
    virtual void ranged_sxhash(HashGenerator& hg, size_t start, size_t end) const override {
      if (hg.isFilling()) {
        Fixnum hash = 5381;
        Fixnum c;
        for ( size_t i(start); i<end; ++i ) {
          const value_type& c = (*this)[i];
          hash = ((hash << 5) + hash) + c;
        }
        hg.addPart(hash);
      }
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
  class SimpleVector_O;
  typedef abstract_SimpleVector<SimpleVector_O,T_sp,SpecializedSimpleVector_O> specialized_SimpleVector;
  class SimpleVector_O : public specialized_SimpleVector {
    LISP_CLASS(core, ClPkg, SimpleVector_O, "simple-vector",SpecializedSimpleVector_O);
  public:
    typedef specialized_SimpleVector TemplatedBase;
    typedef typename TemplatedBase::leaf_type leaf_type;
    typedef typename TemplatedBase::value_type value_type;
    typedef typename TemplatedBase::vector_type vector_type;
    typedef typename TemplatedBase::iterator iterator;
    typedef typename TemplatedBase::const_iterator const_iterator;
    typedef value_type container_value_type;
  public:
    static value_type initial_element_from_object(T_sp obj, bool supplied) {return supplied ? obj : _Nil<T_O>();};
    static value_type from_object(T_sp obj) {return obj; };
    static T_sp to_object(const value_type& v) { return v; };
  public:
  SimpleVector_O(size_t length, value_type initialElement=value_type(), bool initialElementSupplied=false, size_t initialContentsSize=0, const value_type* initialContents=NULL) : TemplatedBase(length,initialElement,initialElementSupplied,initialContentsSize,initialContents) {};
    static SimpleVector_sp make(size_t length, T_sp initialElement=_Nil<T_O>(), bool initialElementSupplied=false, size_t initialContentsSize=0, const T_sp* initialContents=NULL) {
      auto bs = gctools::GC<SimpleVector_O>::allocate_container(gctools::GCStamp<SimpleVector_O>::TheStamp,
                                                                length,length,initialElement,initialElementSupplied,initialContentsSize,initialContents);
      return bs;
    }
  public:
    // Specific to SimpleVector_O
    virtual void __write__(T_sp stream) const final;
  public:
    virtual T_sp type_as_symbol() const final { return cl::_sym_simple_vector; };
    virtual T_sp arrayElementType() const override { return cl::_sym_T_O; };
    virtual Symbol_sp elementTypeAsSymbol() const override { return cl::_sym_T_O; };
  public:
    // Implement these methods for simple vectors - some are implemented in parent classes
    // for convenience if not speed
    virtual bool equal(T_sp other) const override { return this->eq(other);};
    virtual bool equalp(T_sp other) const override;
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
    typedef gctools::GCBitUnitArray_moveable<1> bitunit_array_type;
    typedef typename bitunit_array_type::word_type value_type;
    static const size_t BitWidth = bitunit_array_type::number_of_bit_units_in_word;
  public:
    bitunit_array_type _Data;
  SimpleBitVector_O(size_t length,
                    value_type initialElement,
                    bool initialElementSupplied)
    : Base(), _Data(length,initialElement,initialElementSupplied) {};
    static SimpleBitVector_sp make( size_t length,
                                    value_type initialElement=0,
                                    bool initialElementSupplied=false) {
      auto sbv = gctools::GC<SimpleBitVector_O>::allocate_bitunit_container(gctools::GCStamp<SimpleBitVector_O>::TheStamp,length,initialElement,initialElementSupplied);
      return sbv;
    }
  public:
    static value_type initial_element_from_object(T_sp initialElement, bool initialElementSupplied) {
      if (initialElementSupplied) {
        if (initialElement.fixnump()) {
          value_type i = initialElement.unsafe_fixnum();
          if (i==0||i==1) return i;
        }
        TYPE_ERROR(initialElement,cl::_sym_bit);
      }
      return 0;
    }
  public:
    virtual T_sp type_as_symbol() const final { return cl::_sym_simple_bit_vector; };
    virtual T_sp arrayElementType() const override { return cl::_sym_bit; };
    virtual Symbol_sp elementTypeAsSymbol() const override { return cl::_sym_bit; };
  public:
    void asBaseSimpleVectorRange(BaseSimpleVector_sp& sv, size_t& start, size_t& end) const override {
      sv = this->asSmartPtr();
      start = 0;
      end = this->length();
    }
    void setBit(size_t idx, uint v) {this->_Data.unsignedSetBitUnit(idx,v);}
    uint testBit(size_t idx) const {return this->_Data.unsignedBitUnit(idx);};
  public:
    // Implement these methods for simple vectors - some are implemented in parent classes
    // for convenience if not speed
    virtual void __write__(T_sp strm) const final;
    virtual size_t elementSizeInBytes() const override {bitVectorDoesntSupportError();};
    virtual void* rowMajorAddressOfElement_(size_t i) const override {bitVectorDoesntSupportError();};
    virtual Array_sp reverse() const final;
    virtual Array_sp nreverse() final;
    virtual bool equal(T_sp other) const final;
    virtual bool equalp(T_sp other) const final {return this->equal(other);};
    virtual Array_sp unsafe_subseq(size_t start, size_t end) const final;
    virtual Array_sp unsafe_setf_subseq(size_t start, size_t end, Array_sp newSubseq) override;
    virtual vector<size_t> arrayDimensionsAsVector() const final {
      vector<size_t> dims;
      dims.push_back(this->length());
      return dims;
    }
    virtual void unsafe_fillArrayWithElt(T_sp initialElement, size_t start, size_t end) final;
  public:
    CL_METHOD_OVERLOAD virtual void rowMajorAset(size_t idx, T_sp value) final {this->setBit(idx,value.unsafe_fixnum());};
    CL_METHOD_OVERLOAD virtual T_sp rowMajorAref(size_t idx) const final {return clasp_make_fixnum(this->testBit(idx)); };
    virtual void sxhash_(HashGenerator& hg) const final {this->ranged_sxhash(hg,0,this->length());}
    virtual void ranged_sxhash(HashGenerator& hg, size_t start, size_t end) const final {
      if (hg.isFilling()) {
        Fixnum hash = 5381;
        Fixnum c;
        for ( size_t i(start); i<end; ++i ) {
          uint c = this->testBit(i);
          hash = ((hash << 5) + hash) + c;
        }
        hg.addPart(hash);
      }
    }
  };
};

SYMBOL_EXPORT_SC_(CorePkg,simple_double_vector);

namespace core {
  FORWARD(SimpleDoubleVector);
};
template <>
struct gctools::GCInfo<core::SimpleDoubleVector_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = atomic;
};
namespace core {
  class SimpleDoubleVector_O;
  typedef abstract_SimpleVector<SimpleDoubleVector_O,double,SpecializedSimpleVector_O> specialized_SimpleDoubleVector;
  class SimpleDoubleVector_O : public specialized_SimpleDoubleVector {
    LISP_CLASS(core, CorePkg, SimpleDoubleVector_O, "SimpleDoubleVector",SpecializedSimpleVector_O);
  public:
    typedef specialized_SimpleDoubleVector TemplatedBase;
    typedef typename TemplatedBase::leaf_type leaf_type;
    typedef typename TemplatedBase::value_type value_type;
    typedef typename TemplatedBase::vector_type vector_type;
    typedef typename TemplatedBase::iterator iterator;
    typedef typename TemplatedBase::const_iterator const_iterator;
    typedef value_type container_value_type;
  public:
    static value_type initial_element_from_object(T_sp obj, bool supplied) {
      if (supplied) {
        if (obj.single_floatp()) {
          return obj.unsafe_single_float();
        } else if (gc::IsA<DoubleFloat_sp>(obj)) {
          return gc::As_unsafe<DoubleFloat_sp>(obj)->get();
        }
        TYPE_ERROR(obj,cl::_sym_float);
      }
      return 0.0;
    }
    static value_type from_object(T_sp obj) { if (gc::IsA<DoubleFloat_sp>(obj)) return gc::As_unsafe<DoubleFloat_sp>(obj)->get(); TYPE_ERROR(obj,cl::_sym_double_float); };
    static T_sp to_object(const value_type& v) { return DoubleFloat_O::create(v); };
  public:
  SimpleDoubleVector_O(size_t length, value_type initialElement=value_type(), bool initialElementSupplied=false, size_t initialContentsSize=0, const value_type* initialContents=NULL) : TemplatedBase(length,initialElement,initialElementSupplied,initialContentsSize,initialContents) {};
    static SimpleDoubleVector_sp make(size_t length, value_type initialElement=value_type(), bool initialElementSupplied=false, size_t initialContentsSize=0, const value_type* initialContents=NULL) {
      auto bs = gctools::GC<SimpleDoubleVector_O>::allocate_container(gctools::GCStamp<SimpleDoubleVector_O>::TheStamp,
                                                                      length,length,initialElement,initialElementSupplied,initialContentsSize,initialContents);
      return bs;
    }
  public:
    // Specific to SimpleDoubleVector_O
//    virtual void __write__(T_sp stream) const final;
  public:
    virtual T_sp type_as_symbol() const final { return core::_sym_simple_double_vector; };
    virtual T_sp arrayElementType() const override { return cl::_sym_double_float; };
    virtual Symbol_sp elementTypeAsSymbol() const override { return gc::As_unsafe<Symbol_sp>(this->arrayElementType()); };
  public:
    // Implement these methods for simple vectors - some are implemented in parent classes
    // for convenience if not speed
    virtual bool equal(T_sp other) const override { return this->eq(other);};
//    virtual bool equalp(T_sp other) const override;
  };
};













namespace core {
Vector_sp core__make_vector(T_sp element_type,
                            size_t dimension,
                            bool adjustable = false,
                            T_sp fill_pointer = _Nil<T_O>(),
                            T_sp displaced_to = _Nil<T_O>(),
                            T_sp displacedIndexOffset = _Nil<T_O>(),
                            T_sp initial_element = _Nil<T_O>(),
                            bool initial_element_supplied_p = false);
};


namespace core {
  template <typename MyArrayType, typename MySimpleType, typename MyParentType >
    class abstract_DisplacementHandlingVector : public MyParentType {
  public:
    // The types that define what this class does
    typedef MyParentType Base;
    typedef MyArrayType /*eg: VectorTNs_O*/ my_array_type;
    typedef MySimpleType /*eg: SimpleVector_O*/ simple_type;
    typedef typename simple_type::value_type /*eg: T_sp*/ value_type;
    typedef gctools::smart_ptr<my_array_type> my_smart_ptr_type;
  public:
    // Pass constructor arguments up
  abstract_DisplacementHandlingVector(size_t rank,
                                      List_sp dimensions,
                                      T_sp fillPointer,
                                      T_sp displacedTo,
                                      size_t displacedIndexOffset)
    : Base(rank,dimensions,fillPointer,displacedTo,displacedIndexOffset) {};
  public:
    static void never_invoke_allocator() {gctools::GCAbstractAllocator<abstract_DisplacementHandlingVector>::never_invoke_allocator();};
  public:
    // Primary functions/operators for operator[] that handle displacement
    // There's a non-const and a const version of each
    value_type& unsafe_indirectReference(size_t index) {
      my_array_type& vecns = *reinterpret_cast<my_array_type*>(&*this->_Data);
      return vecns[this->_DisplacedIndexOffset+index];
    }
    value_type& operator[](size_t index) {
      unlikely_if (gc::IsA<my_smart_ptr_type>(this->_Data)) return this->unsafe_indirectReference(index);
      return (*reinterpret_cast<simple_type*>(&*(this->_Data)))[this->_DisplacedIndexOffset+index];
    }
    const value_type& unsafe_indirectReference(size_t index) const {
      my_array_type& vecns = *reinterpret_cast<my_array_type*>(&*this->_Data);
      return vecns[this->_DisplacedIndexOffset+index];
    }
    const value_type& operator[](size_t index) const {
      unlikely_if (gc::IsA<my_smart_ptr_type>(this->_Data)) return this->unsafe_indirectReference(index);
      return (*reinterpret_cast<simple_type*>(&*(this->_Data)))[this->_DisplacedIndexOffset+index];
    }
  public:
    // Iterators
    value_type* begin() { return &(*this)[0]; };
    value_type* end() { return &(*this)[this->length()]; };
    const value_type* begin() const { return &(*this)[0]; };
    const value_type* end() const { return &(*this)[this->length()]; };
  public:
    void this_asBaseSimpleVectorRange(BaseSimpleVector_sp& sv, size_t& start, size_t& end) const  {
      unlikely_if (gc::IsA<my_smart_ptr_type>(this->_Data)) {
        this->asBaseSimpleVectorRange(sv,start,end);
        start += this->_DisplacedIndexOffset;
        end = this->length()+this->_DisplacedIndexOffset;
        return;
      }
      sv = gc::As<BaseSimpleVector_sp>(this->_Data);
      start = this->_DisplacedIndexOffset;
      end = this->length()+this->_DisplacedIndexOffset;
    }
    void asBaseSimpleVectorRange(BaseSimpleVector_sp& sv, size_t& start, size_t& end) const final {
      this->this_asBaseSimpleVectorRange(sv,start,end);
    }
    virtual Array_sp reverse() const final { return templated_reverse_VectorNs(*this); };
    virtual Array_sp nreverse() final { return templated_nreverse_VectorNs(*this); };
    virtual void internalAdjustSize_(size_t size, T_sp initElement=_Nil<T_O>(), bool initElementSupplied=false ) final {
      if (size == this->_ArrayTotalSize) return;
      BaseSimpleVector_sp basesv;
      size_t start, end;
      this->this_asBaseSimpleVectorRange(basesv,start,end);
      gctools::smart_ptr<simple_type> sv = gc::As_unsafe<gctools::smart_ptr<simple_type>>(basesv);
      size_t initialContentsSize = MIN(this->length(),size);
      my_smart_ptr_type newData = simple_type::make(size,simple_type::from_object(initElement),true,initialContentsSize,&(*sv)[start]);
      this->set_data(newData);
//      printf("%s:%d:%s  original size=%lu new size=%lu  copied %lu elements\n", __FILE__, __LINE__, __FUNCTION__, this->_ArrayTotalSize, size, initialContentsSize );
      this->_ArrayTotalSize = size;
      if (!this->_FillPointerP) this->_FillPointerOrLengthOrDummy = size;
      this->_DisplacedIndexOffset = 0;
      this->_DisplacedToP = false;
    }
    bool equalp(T_sp o) const {
      if (&*o == this) return true;
      if (my_smart_ptr_type other = o.asOrNull<my_array_type>()) {
        if (this->rank() != other->rank() ) return false;
        for (size_t i(0); i<this->rank(); ++i ) {
          if (this->_Dimensions[i] != other->_Dimensions[i]) return false;
        }
        for (size_t i(0),iEnd(this->arrayTotalSize()); i<iEnd; ++i) {
          if (!cl__equalp(this->rowMajorAref(i), other->rowMajorAref(i))) return false;
        }
        return true;
      }
      return false;
    }
    CL_METHOD_OVERLOAD virtual void rowMajorAset(size_t idx, T_sp value) final {(*this)[idx] = simple_type::from_object(value);}
    CL_METHOD_OVERLOAD virtual T_sp rowMajorAref(size_t idx) const final {return simple_type::to_object((*this)[idx]);}
    bool equal(T_sp obj) const override { return this->eq(obj); };
  };
};


namespace core {
  class VectorNs_O : public MDArray_O {
    LISP_CLASS(core, ClPkg, VectorNs_O, "non-simple-vector",MDArray_O);
  public:
  VectorNs_O(size_t rank,
             List_sp dimensions,
             T_sp fillPointer,
             T_sp displacedTo,
             size_t displacedIndexOffset)
    : Base(rank,dimensions,fillPointer,displacedTo,displacedIndexOffset) {};
  public:
    virtual Array_sp unsafe_subseq(size_t start, size_t iend) const
    {
      return this->_Data->unsafe_subseq(start+this->_DisplacedIndexOffset,iend+this->_DisplacedIndexOffset);
    }
    virtual Array_sp unsafe_setf_subseq(size_t start, size_t iend, Array_sp new_subseq)
    {
      return this->_Data->unsafe_setf_subseq(start+this->_DisplacedIndexOffset,iend+this->_DisplacedIndexOffset,new_subseq);
    }

    void ensureSpaceAfterFillPointer(T_sp init_element, size_t size);
    T_sp vectorPush(T_sp newElement);
    Fixnum_sp vectorPushExtend(T_sp newElement, size_t extension = 0);

    // The class needs to provide something to resize the
    // vector until adjust-array is available
    // Was setSize
    virtual void sxhash_(HashGenerator& hg) const override {this->General_O::sxhash_(hg);}
    virtual bool equalp(T_sp other) const;
  };
}

namespace core {
  class StrNs_O : public VectorNs_O {
    LISP_CLASS(core, ClPkg, StrNs_O, "StrNs",VectorNs_O);
  public:
  StrNs_O(size_t rank,
          List_sp dimensions,
          T_sp fillPointer,
          T_sp displacedTo,
          size_t displacedIndexOffset)
    : Base(rank,dimensions,fillPointer,displacedTo,displacedIndexOffset) {};
  public:
    virtual T_sp type_as_symbol() const = 0;
    virtual void sxhash_(HashGenerator& hg) const final {
      BaseSimpleVector_sp svec;
      size_t start,end;
      this->asBaseSimpleVectorRange(svec,start,end);
      svec->ranged_sxhash(hg,start,end);
    }
    virtual SimpleString_sp asMinimalSimpleString() const = 0;
  };
};

namespace core {
  class Str8Ns_O : public abstract_DisplacementHandlingVector<Str8Ns_O,SimpleBaseCharString_O,StrNs_O> {
    LISP_CLASS(core, ClPkg, Str8Ns_O, "base-string",StrNs_O);
  public:
    // The types that define what this class does
    typedef abstract_DisplacementHandlingVector<Str8Ns_O,SimpleBaseCharString_O,StrNs_O> TemplatedBase;
    typedef typename TemplatedBase::value_type value_type;
    typedef typename TemplatedBase::simple_type simple_type;
    typedef value_type* iterator;
    typedef const value_type* const_iterator;
  public:
  Str8Ns_O(size_t dimension,
           T_sp fillPointer,
           T_sp displacedTo,
           size_t displacedIndexOffset)
    : TemplatedBase(1,Cons_O::createList(clasp_make_fixnum(dimension)),fillPointer,displacedTo,displacedIndexOffset) {};
    static Str8Ns_sp make(size_t dimension, claspChar initElement='\0', bool initialElementSuppliedP = false, T_sp fillPointer=_Nil<T_O>(), T_sp displacedTo=_Nil<T_O>(), size_t displacedIndexOffset=0 ) {
      GC_ALLOCATE_VARIADIC(Str8Ns_O,s,dimension,fillPointer,displacedTo,displacedIndexOffset);
      if (LIKELY(displacedTo.nilp())) {
        SimpleBaseCharString_sp sb = SimpleBaseCharString_O::make(dimension,initElement,initialElementSuppliedP);
        s->set_data(sb);
      }
      return s;
    }
    static Str8Ns_sp make(const string& nm) {
      auto ss = SimpleBaseCharString_O::make(nm);
      auto result = Str8Ns_O::make(nm.size());
      result->set_data(ss);
      return result;
    }
  public:
    // move all the constructors into here
  static Str8Ns_sp createBufferString(size_t bufferSize = BUFFER_STRING_SIZE) {
    return Str8Ns_O::make(bufferSize, value_type()/*' '*/, true, clasp_make_fixnum(0));
  };
  public:
  static Str8Ns_sp create(const string &nm);
//  static Str8Ns_sp create(const boost::format &nm);
  static Str8Ns_sp create(const char *nm, size_t numChars);
  static Str8Ns_sp create(const char *nm);
  static Str8Ns_sp create(size_t numChars);
  static Str8Ns_sp create(Str8Ns_sp orig);
  public:
    virtual T_sp type_as_symbol() const final { return cl::_sym_base_string; };
  public:
    virtual bool equal(T_sp other) const final;
  public:
    iterator begin() { return &(*this)[0]; };
    iterator end() { return &(*this)[this->length()]; };
    const_iterator begin() const { return &(*this)[0]; };
    const_iterator end() const { return &(*this)[this->length()]; };
  public:
    virtual void __write__(T_sp strm) const final;
    virtual std::string get_std_string() const final { return std::string((const char*)this->begin(),this->length());};
    virtual std::string __repr__() const final { return this->get_std_string(); };
  public: // Str8Ns specific functions
    void vectorPushExtend_claspChar(claspChar c, size_t extension=32);
  std::string get() { /*DEPRECIATE for get_std_string */ return this->get_std_string(); };
  virtual SimpleString_sp asMinimalSimpleString() const final;
  };
};

namespace core {
  class StrWNs_O : public abstract_DisplacementHandlingVector<StrWNs_O,SimpleCharacterString_O,StrNs_O> {
    LISP_CLASS(core, ClPkg, StrWNs_O, "wide-string",StrNs_O);
  public:
    // The types that define what this class does
    typedef abstract_DisplacementHandlingVector<StrWNs_O,SimpleCharacterString_O,StrNs_O> TemplatedBase;
    typedef typename TemplatedBase::value_type value_type;
    typedef typename TemplatedBase::simple_type simple_type;
    typedef value_type* iterator;
    typedef const value_type* const_iterator;
  public:
  StrWNs_O(size_t dimension,
          T_sp fillPointer,
          T_sp displacedTo,
          size_t displacedIndexOffset)
    : TemplatedBase(1,Cons_O::createList(clasp_make_fixnum(dimension)),fillPointer,displacedTo,displacedIndexOffset) {};
    static StrWNs_sp make(size_t dimension, claspCharacter initElement='\0', bool initialElementSuppliedP=false, T_sp fillPointer=_Nil<T_O>(), T_sp displacedTo=_Nil<T_O>(), size_t displacedIndexOffset=0 ) {
      GC_ALLOCATE_VARIADIC(StrWNs_O,s,dimension,fillPointer,displacedTo,displacedIndexOffset);
      LIKELY_if (displacedTo.nilp()) {
        SimpleCharacterString_sp sb = SimpleCharacterString_O::make(dimension,initElement,initialElementSuppliedP);
        s->set_data(sb);
      }
      return s;
    }
    static StrWNs_sp make(const string& nm) {
      auto ss = SimpleCharacterString_O::make(nm);
      auto result = StrWNs_O::make(nm.size());
      result->set_data(ss);
      return result;
    }
    static StrWNs_sp createBufferString(size_t bufferSize = BUFFER_STRING_SIZE) {
      return StrWNs_O::make(bufferSize, value_type()/*' '*/, true, clasp_make_fixnum(0));
    };
  public:
    virtual T_sp type_as_symbol() const final{ return cl::_sym_string; };
  public:
    virtual bool equal(T_sp other) const final;
  public:
    iterator begin() { return &(*this)[0]; };
    iterator end() { return &(*this)[this->length()]; };
    const_iterator begin() const { return &(*this)[0]; };
    const_iterator end() const { return &(*this)[this->length()]; };
  public:
    virtual void __write__(T_sp strm) const final;
    virtual std::string get_std_string() const final;
    virtual std::string __repr__() const final;
  public: // StrWNs specific functions
    void vectorPushExtend_claspCharacter(claspCharacter c, size_t extension=32);
    /*! Return true if all characters are base characters and the string 
        can be downgraded to a base-char string */
    bool all_base_char_p() const;
    /*! Return the smallest character simple-string that can hold this */
    SimpleString_sp asMinimalSimpleString() const final;
  };
};


namespace core {
  class SpecializedVectorNs_O : public VectorNs_O {
    LISP_CLASS(core, ClPkg, SpecializedVectorNs_O, "specialized-non-simple-vector",VectorNs_O);
  public:
  SpecializedVectorNs_O(size_t rank,
                        List_sp dimensions,
                        T_sp fillPointer,
                        T_sp displacedTo,
                        size_t displacedIndexOffset)
    : Base(rank,dimensions,fillPointer,displacedTo,displacedIndexOffset) {};
  public:
    virtual T_sp type_as_symbol() const final { return cl::_sym_vector; };
    virtual void sxhash_(HashGenerator& hg) const final {this->General_O::sxhash_(hg);}
  };
}

namespace core {
  class VectorTNs_O : public abstract_DisplacementHandlingVector<VectorTNs_O,SimpleVector_O,SpecializedVectorNs_O> {
    LISP_CLASS(core, ClPkg, VectorTNs_O, "non-simple-vector-t",SpecializedVectorNs_O);
  public:
    typedef abstract_DisplacementHandlingVector<VectorTNs_O,SimpleVector_O,SpecializedVectorNs_O> TemplatedBase;
    typedef typename TemplatedBase::value_type value_type;
    typedef typename TemplatedBase::simple_type simple_type;
  public:
  VectorTNs_O(size_t dimension,
              T_sp fillPointer,
              T_sp displacedTo,
              size_t displacedIndexOffset)
    : TemplatedBase(1,Cons_O::createList(clasp_make_fixnum(dimension)),fillPointer,displacedTo,displacedIndexOffset) {};
    static VectorTNs_sp make(size_t dimension, T_sp initElement=_Nil<T_O>(), T_sp fillPointer=_Nil<T_O>(), T_sp displacedTo=_Nil<T_O>(), size_t displacedIndexOffset=0 );
    static VectorTNs_sp create(const gctools::Vec0<T_sp> &objs);
    virtual bool equalp(T_sp o) const final;
  };
};

namespace core {
  // I can't use the abstract_DisplacementHandlingVector here because of bitwise access
  class BitVectorNs_O : public VectorNs_O {
    LISP_CLASS(core, ClPkg, BitVectorNs_O, "bit-vector",VectorNs_O);
  public:
    typedef SimpleBitVector_O simple_type;
  BitVectorNs_O(size_t dimension,
                T_sp fillPointer,
                T_sp displacedTo,
                size_t displacedIndexOffset)
    : Base(1,Cons_O::createList(clasp_make_fixnum(dimension)),fillPointer,displacedTo,displacedIndexOffset) {};
    static BitVectorNs_sp make(size_t length, SimpleBitVector_O::value_type initialElement, bool initialElementSuppliedP, T_sp fillPointer, T_sp displacedTo, size_t displacedIndexOffset ) {
      GC_ALLOCATE_VARIADIC(BitVectorNs_O, bv, length, fillPointer, displacedTo, displacedIndexOffset );
      LIKELY_if (displacedTo.nilp()) {
        SimpleBitVector_sp sbv = SimpleBitVector_O::make(length,initialElement,initialElementSuppliedP);
        bv->set_data(sbv);
      }
      return bv;
    }
  public:
    virtual void __write__(T_sp strm) const;
    uint testBit(size_t idx) const {
        BaseSimpleVector_sp bme;
        size_t mstart, mend;
        this->asBaseSimpleVectorRange(bme,mstart,mend);
        simple_type* me = reinterpret_cast<simple_type*>(&*bme);
        return me->testBit(idx+this->_DisplacedIndexOffset);
    }
    void setBit(size_t idx, uint v)  {
        BaseSimpleVector_sp bme;
        size_t mstart, mend;
        this->asBaseSimpleVectorRange(bme,mstart,mend);
        simple_type* me = reinterpret_cast<simple_type*>(&*bme);
        me->setBit(idx+this->_DisplacedIndexOffset,v);
    }
    void asBaseSimpleVectorRange(BaseSimpleVector_sp& sv, size_t& start, size_t& end) const final {
      unlikely_if (gc::IsA<smart_ptr_type>(this->_Data)) {
        this->asBaseSimpleVectorRange(sv,start,end);
        start += this->_DisplacedIndexOffset;
        end = this->length()+this->_DisplacedIndexOffset;
        return;
      }
      sv = gc::As<SimpleBitVector_sp>(this->_Data);
      start = this->_DisplacedIndexOffset;
      end = this->length()+this->_DisplacedIndexOffset;
    }
  public:
    virtual T_sp type_as_symbol() const final { return cl::_sym_bit_vector; };
    virtual bool equal(T_sp other) const final;
    virtual bool equalp(T_sp other) const final { return this->equal(other);};
    virtual void internalAdjustSize_(size_t size, T_sp init_element=_Nil<T_O>(), bool initElementSupplied=false ) override;
    virtual Array_sp reverse() const override;
    virtual Array_sp nreverse() override;
  public:
    CL_METHOD_OVERLOAD virtual void rowMajorAset(size_t idx, T_sp value) override {this->setBit(idx,value.unsafe_fixnum());};
    CL_METHOD_OVERLOAD virtual T_sp rowMajorAref(size_t idx) const override {return clasp_make_fixnum(this->testBit(idx)); };
    virtual void sxhash_(HashGenerator& hg) const final {
      if (hg.isFilling()) {
        BaseSimpleVector_sp svec;
        size_t start,end;
        this->asBaseSimpleVectorRange(svec,start,end);
        svec->ranged_sxhash(hg,start,end);
      }
    }
  };
};


namespace core {
  template <typename MyArrayType, typename MySimpleType, typename MyParentType >
    class abstract_DisplacementHandlingArray : public MyParentType {
  public:
    // The types that define what this class does
    typedef MyParentType Base;
    typedef MyArrayType /*eg: VectorTNs_O*/ my_array_type;
    typedef MySimpleType /*eg: SimpleVector_O*/ simple_type;
    typedef typename simple_type::value_type /*eg: T_sp*/ value_type;
    typedef gctools::smart_ptr<my_array_type> my_smart_ptr_type;
  public:
    // Pass constructor arguments up
  abstract_DisplacementHandlingArray(size_t rank,
                                     List_sp dimensions,
                                     T_sp fillPointer,
                                     T_sp displacedTo,
                                     size_t displacedIndexOffset)
    : Base(rank,dimensions,fillPointer,displacedTo,displacedIndexOffset) {};
  public:
    static void never_invoke_allocator() {gctools::GCAbstractAllocator<abstract_DisplacementHandlingArray>::never_invoke_allocator();};
  public:
    // Primary functions/operators for operator[] that handle displacement
    // There's a non-const and a const version of each
    value_type& unsafe_indirectReference(size_t index) {
      my_array_type& vecns = *reinterpret_cast<my_array_type*>(&*this->_Data);
      return vecns[this->_DisplacedIndexOffset+index];
    }
    value_type& operator[](size_t index) {
      unlikely_if (gc::IsA<my_smart_ptr_type>(this->_Data)) return this->unsafe_indirectReference(index);
      return (*reinterpret_cast<simple_type*>(&*(this->_Data)))[this->_DisplacedIndexOffset+index];
    }
    const value_type& unsafe_indirectReference(size_t index) const {
      my_array_type& vecns = *reinterpret_cast<my_array_type*>(&*this->_Data);
      return vecns[this->_DisplacedIndexOffset+index];
    }
    const value_type& operator[](size_t index) const {
      unlikely_if (gc::IsA<my_smart_ptr_type>(this->_Data)) return this->unsafe_indirectReference(index);
      return (*reinterpret_cast<simple_type*>(&*(this->_Data)))[this->_DisplacedIndexOffset+index];
    }
  public:
    // Iterators
    value_type* begin() { return &(*this)[0]; };
    value_type* end() { return &(*this)[this->arrayTotalSize()]; };
    const value_type* begin() const { return &(*this)[0]; };
    const value_type* end() const { return &(*this)[this->arrayTotalSize()]; };
  public:
    void asBaseSimpleVectorRange(BaseSimpleVector_sp& sv, size_t& start, size_t& end) const final {
      unlikely_if (gc::IsA<my_smart_ptr_type>(this->_Data)) {
        this->asBaseSimpleVectorRange(sv,start,end);
        start += this->_DisplacedIndexOffset;
        end = this->arrayTotalSize()+this->_DisplacedIndexOffset;
        return;
      }
      sv = gc::As<SimpleVector_sp>(this->_Data);
      start = this->_DisplacedIndexOffset;
      end = this->arrayTotalSize()+this->_DisplacedIndexOffset;
    }
    virtual void sxhash_(HashGenerator& hg) const final {this->General_O::sxhash_(hg);}
    virtual bool equal(T_sp other) const override { return this->eq(other);};
    bool equalp(T_sp o) const final {
      if (&*o == this) return true;
      if (my_smart_ptr_type other = o.asOrNull<my_array_type>()) {
        if (this->rank() != other->rank() ) return false;
        for (size_t i(0); i<this->rank(); ++i ) {
          if (this->_Dimensions[i] != other->_Dimensions[i]) return false;
        }
        for (size_t i(0),iEnd(this->arrayTotalSize()); i<iEnd; ++i) {
          if (!cl__equalp((*this)[i], (*other)[i])) return false;
        }
        return true;
      }
      return false;
    }
  public:
    CL_METHOD_OVERLOAD virtual void rowMajorAset(size_t idx, T_sp value) final {(*this)[idx] = simple_type::from_object(value);}
    CL_METHOD_OVERLOAD virtual T_sp rowMajorAref(size_t idx) const final {return simple_type::to_object((*this)[idx]);}

  };
}

namespace core {
  class ArrayTNs_O : public abstract_DisplacementHandlingArray<ArrayTNs_O,SimpleVector_O,SpecializedMDArrayNs_O>  {
    LISP_CLASS(core, ClPkg, ArrayTNs_O, "arraytns",SpecializedMDArrayNs_O);
  public:
    typedef abstract_DisplacementHandlingArray<ArrayTNs_O,SimpleVector_O,SpecializedMDArrayNs_O> TemplatedBase;
  ArrayTNs_O(size_t rank,
             List_sp dimensions,
             T_sp fillPointer,
             T_sp displacedTo,
             size_t displacedIndexOffset) : TemplatedBase(rank,dimensions,fillPointer,displacedTo,displacedIndexOffset) {};
  public:
    static ArrayTNs_sp make(T_sp dim_desig, T_sp initialElement, T_sp fillPointer, T_sp displacedTo, size_t displacedIndexOffset);
  public:
    virtual void internalAdjustSize_(size_t size, T_sp initElement=_Nil<T_O>(), bool initElementSupplied=false ) final {
      IMPLEMENT_MEF(BF("Add support for internalAdjustSize for ArrayTNs when you see this"));
    };
  };

};




namespace core {
// Like ecl__vector_start_end
T_mv clasp_vectorStartEnd(Symbol_sp fn, T_sp thing, Fixnum_sp start, Fixnum_sp end);

};



// ----------------------------------------------------------------------
//
// Array and String functions
//

namespace core {

  List_sp cl__arrayDimensions(Array_sp array);


  String_sp cl__string(T_sp str);
  SimpleString_sp cl__string_upcase(T_sp arg);
  SimpleString_sp cl__string_downcase(T_sp arg);
  String_sp cl__nstring_upcase(String_sp arg);
  String_sp cl__nstring_downcase(String_sp arg);
  Character_sp cl__char(String_sp str, size_t idx);

  bool clasp_memberChar(claspChar c, String_sp charBag);

  String_sp cl__string_trim(T_sp charbag, T_sp str);
  String_sp cl__string_left_trim(T_sp charbag, T_sp str);
  String_sp cl__string_right_trim(T_sp charbag, T_sp str);

  T_mv cl__parse_integer(String_sp str, Fixnum start = 0, T_sp end = _Nil<T_O>(), uint radix = 10, T_sp junkAllowed = _Nil<T_O>());

  T_sp cl__string_equal(T_sp strdes1, T_sp strdes2, Fixnum_sp start1 = make_fixnum(0), T_sp end1 = _Nil<T_O>(), Fixnum_sp start2 = make_fixnum(0), T_sp end2 = _Nil<T_O>());

  /*! Push a c-style string worth of characters into the buffer */
  void StringPushStringCharStar(String_sp buffer, const char* cp);
  void StringPushSubString(String_sp buffer, String_sp other, size_t start, size_t end);
  void StringPushString(String_sp buffer, String_sp other);

  T_sp cl__string_EQ_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1=clasp_make_fixnum(0), T_sp end1=_Nil<T_O>(), Fixnum_sp start2=clasp_make_fixnum(0), T_sp end2=_Nil<T_O>());

  T_sp core__search_string(String_sp sub, size_t sub_start, T_sp sub_end, String_sp outer, size_t outer_start, T_sp outer_end );
};


#include <clasp/core/string.h>



#endif /* _core_Array_H */
