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
#include <clasp/core/object.h>
#include <clasp/core/numbers.h> // need full definitions for to_object.
#include <clasp/core/character.fwd.h>
#include <clasp/core/array.fwd.h>
#include <clasp/core/sequence.fwd.h>
#include <clasp/core/corePackage.fwd.h>


namespace core {
  size_t calculate_extension(size_t arrayTotalSize);
};

SYMBOL_EXPORT_SC_(ExtPkg,cl_index);
SYMBOL_EXPORT_SC_(ExtPkg,byte8);
SYMBOL_EXPORT_SC_(ExtPkg,byte16);
SYMBOL_EXPORT_SC_(ExtPkg,byte32);
SYMBOL_EXPORT_SC_(ExtPkg,byte64);
SYMBOL_EXPORT_SC_(ExtPkg,integer8);
SYMBOL_EXPORT_SC_(ExtPkg,integer16);
SYMBOL_EXPORT_SC_(ExtPkg,integer32);
SYMBOL_EXPORT_SC_(ExtPkg,integer64);

CL_VALUE_ENUM(cl::_sym_fixnum,      clasp_aet_fix);
CL_VALUE_ENUM(ext::_sym_cl_index,      clasp_aet_size_t);
CL_VALUE_ENUM(ext::_sym_byte64,      clasp_aet_byte64_t);
CL_VALUE_ENUM(ext::_sym_integer64,      clasp_aet_int64_t);
CL_VALUE_ENUM(ext::_sym_byte32,      clasp_aet_byte32_t);
CL_VALUE_ENUM(ext::_sym_integer32,      clasp_aet_int32_t);
CL_VALUE_ENUM(ext::_sym_byte16,      clasp_aet_byte16_t);
CL_VALUE_ENUM(ext::_sym_integer16,      clasp_aet_int16_t);
CL_VALUE_ENUM(ext::_sym_byte8,      clasp_aet_byte8_t);
CL_VALUE_ENUM(ext::_sym_integer8,      clasp_aet_int8_t);

namespace cl {
    extern core::Symbol_sp& _sym_fixnum;
};

namespace ext {
  extern core::Symbol_sp& _sym_cl_index;
  extern core::Symbol_sp& _sym_byte64;
  extern core::Symbol_sp& _sym_byte32;
  extern core::Symbol_sp& _sym_byte16;
  extern core::Symbol_sp& _sym_byte8;
  extern core::Symbol_sp& _sym_integer64;
  extern core::Symbol_sp& _sym_integer32;
  extern core::Symbol_sp& _sym_integer16;
  extern core::Symbol_sp& _sym_integer8;
  
};


namespace cl {
  extern core::Symbol_sp& _sym_General_O;
  extern core::Symbol_sp& _sym_adjust_array;
  extern core::Symbol_sp& _sym_sequence;
  extern core::Symbol_sp& _sym_array;
  extern core::Symbol_sp& _sym_or;
  extern core::Symbol_sp& _sym_nil;
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
  extern core::Symbol_sp& _sym_UnsignedByte;
  extern core::Symbol_sp& _sym_T_O;
  extern core::Symbol_sp& _sym_simple_string;
  extern core::Symbol_sp& _sym_simple_array;
};

namespace core {
  extern core::Symbol_sp& _sym_replaceArray;
  extern core::Symbol_sp& _sym_swapElements;
  extern core::Symbol_sp& _sym_fillPointerSet;
  extern core::Symbol_sp& _sym_fillArrayWithElt;
  extern core::Symbol_sp& _sym_setf_subseq;
  extern void clasp_write_string(const string &str, T_sp strm );
  extern claspCharacter clasp_write_char(claspCharacter c, T_sp strm);
};

namespace core {

  List_sp cl__arrayDimensions(Array_sp array);

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
  [[noreturn]] void badIndexError(T_sp arr, size_t axis, size_t oneIndex, size_t curDimension);
  [[noreturn]] void indexNotFixnumError(T_sp index);
  [[noreturn]] void insufficientIndexListError(List_sp indices);
  [[noreturn]] void insufficientIndexVaListError(VaList_sp indices);
  [[noreturn]] void notStringError(T_sp obj);
  [[noreturn]] void cannotAdjustSizeOfSimpleArrays(T_sp obj);
  [[noreturn]] void notSequenceError(T_sp obj);
  [[noreturn]] void noFillPointerError(Symbol_sp fn_name, T_sp array);
  [[noreturn]] void noFillPointerSpecializedArrayError(T_sp array);
  [[noreturn]] void notAdjustableError(Symbol_sp fn_name, T_sp array);
  [[noreturn]] void notVectorError(T_sp array);

  template <class SimpleType>
    Array_sp templated_ranged_reverse(const SimpleType& me, size_t start, size_t end) {
    size_t new_length = end - start;
    size_t last_index = end-1;
    gctools::smart_ptr<SimpleType> newVec = SimpleType::make(new_length);
    for (size_t i = 0; i < new_length; ++i) {
      (*newVec)[i] = me[last_index - i];
    }
    return newVec;
  }
  template <class T>
    void templated_swapElements(T& x, size_t xi, size_t yi) {
    typename T::simple_element_type u = x[xi];
    x[xi] = x[yi];
    x[yi] = u;
  }
  template <class MaybeTemplatedSimpleType>
    Array_sp templated_ranged_nreverse(MaybeTemplatedSimpleType& me, size_t start, size_t end) {
    size_t length = end - start;
    size_t halfpoint = length/2;
    size_t lastElement = end - 1;
    for (size_t i = 0; i < halfpoint; ++i) {
      templated_swapElements(me, start + i, lastElement - i);
    }
    return me.asSmartPtr();
  }

  template <class T>
    Array_sp templated_reverse_VectorNs(T& me) {
    AbstractSimpleVector_sp bsv;
    size_t start, end;
    me.asAbstractSimpleVectorRange(bsv,start,end);
    auto sv = gc::As_unsafe<gctools::smart_ptr<typename T::simple_type>>(bsv);
    return templated_ranged_reverse<typename T::simple_type>(*sv,start,end);
  }

  template <class T>
    void templated_nreverse_VectorNs(T& me) {
    AbstractSimpleVector_sp bsv;
    size_t start, end;
    me.asAbstractSimpleVectorRange(bsv,start,end);
    auto sv = gc::As_unsafe<gctools::smart_ptr<typename T::simple_type>>(bsv);
    templated_ranged_nreverse(*sv,start,end);
  }

};

namespace core {
  class Array_O : public General_O {
    LISP_CLASS(core, ClPkg, Array_O, "array",General_O);
    virtual ~Array_O() {};
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
    virtual void asAbstractSimpleVectorRange(AbstractSimpleVector_sp& sv, size_t& start, size_t& end) const = 0;
  public: // Functions here
    virtual T_sp type_of() const { return Cons_O::createList(this->array_type(),this->element_type(),cl__arrayDimensions(this->asSmartPtr()));};
    virtual T_sp array_type() const = 0;
  /*! This replicates ECL ecl__elttype_to_symbol in array.d */
    virtual T_sp element_type() const = 0;
  /*! length() doesn't dispatch - it reaches into the subclass
      through the _Length[0] array to read the first size_t element
      which is the Length/FillPointer for vectors and a Dummy value for arrays */
    size_t length() const { return this->_Length[0]; };
    virtual bool equal(T_sp other) const = 0;
    virtual bool equalp(T_sp other) const = 0;
    virtual size_t arrayTotalSize() const = 0;
    virtual void rowMajorAset(size_t idx, T_sp value) = 0;
    virtual T_sp rowMajorAref(size_t idx) const = 0;
    virtual bool adjustableArrayP() const =0;
    virtual bool displacedToP() const { return false; };
  /*! As the CL function: Return the offset into a one-dimensional vector for the
      multidimensional indices in the (Va)List, using row-major ordering. */
    size_t arrayRowMajorIndex(VaList_sp indices) const;
    size_t arrayRowMajorIndex(List_sp indices) const;
    virtual Array_sp reverse() const = 0;
    virtual Array_sp nreverse() = 0;
    virtual size_t rank() const = 0;
  /*! Return the offset into a one-dimensional vector for the multidimensional index
      in the vector<int>s.  This is in rowMajor order.
      Separate from arrayRowMajorIndex because it's internal and does less error checking. */
    size_t index_vector_int(const vector<int> &indices) const;
    virtual bool arrayHasFillPointerP() const { return false; };
    virtual void fillPointerSet(size_t f) {noFillPointerError(cl::_sym_fillPointer,this->asSmartPtr());};
    virtual size_t fillPointer() const {noFillPointerError(cl::_sym_fillPointer,this->asSmartPtr());};
    virtual size_t displacedIndexOffset() const = 0;
  /*! Return the array dimension along the axis-number */
    virtual size_t arrayDimension(size_t axisNumber) const = 0;
  /*! Return the value at the indices */
    virtual T_sp replaceArray(T_sp other) = 0;
    virtual void __write__(T_sp strm) const;
    virtual string __repr__() const;
  // ------------------------------------------------------------
  //
  // String functions
    virtual std::string get_std_string() const = 0;
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
    virtual void sxhash_equalp(HashGenerator &hg,LocationDependencyPtrT ptr) const;
  // --------------------------------------------------
  // Ranged operations with explicit limits
    virtual Array_sp unsafe_subseq(size_t start, size_t end) const = 0;
    virtual Array_sp unsafe_setf_subseq(size_t start, size_t end, Array_sp newSubseq) = 0;
    virtual void unsafe_fillArrayWithElt(T_sp initial_element, size_t start, size_t end) = 0;
  };


}; /* core */

namespace core {
  struct Rank1 {};
  class MDArray_O : public Array_O {
    LISP_CLASS(core, CorePkg, MDArray_O, "mdarray",Array_O);
    virtual ~MDArray_O() {};
  public:
    typedef size_t value_type; // this is container - needs value_type
    typedef gctools::GCArray_moveable<value_type> vector_type;
    struct Flags {
      size_t     _Flags;
      static const size_t fillPointerFlag         = 0x000001;
      static const size_t displacedToFlag         = 0x000100;
    Flags(bool fillPointerP, bool displacedToP)
    : _Flags( (fillPointerP ? fillPointerFlag : 0)
              | (displacedToP ? displacedToFlag : 0)) {};
      bool fillPointerP() const { return this->_Flags&fillPointerFlag;};
      bool displacedToP() const { return this->_Flags&displacedToFlag;};
      void set_displacedToP(bool f) { if (f) this->_Flags |= displacedToFlag; else this->_Flags &= ~displacedToFlag; };
      bool simpleP() const { return this->_Flags==0; }
    };
  public:
//! same offset as _fillpointerorlength in nonsimplevector
    size_t      _FillPointerOrLengthOrDummy;
    size_t      _ArrayTotalSize;
    Array_sp    _Data;
    size_t      _DisplacedIndexOffset;
    Flags _Flags;
    vector_type _Dimensions;
    // One dimension
    MDArray_O(Rank1 dummy_rank,
                  size_t dimension,
                  T_sp fillPointer,
                  Array_sp data,
                  bool displacedToP,
                  Fixnum_sp displacedIndexOffset);
    // multiple dimensions
    MDArray_O(size_t rank,
              List_sp dimensions,
              Array_sp data,
              bool displacedToP,
              Fixnum_sp displacedIndexOffset);
  public:
    virtual T_sp array_type() const override { return cl::_sym_array; };
    virtual T_sp element_type() const override { return this->_Data->element_type(); };
  public:
    virtual Array_sp data() const { return this->_Data;};
    void set_data(Array_sp a);
  public:
    virtual size_t elementSizeInBytes() const override { return this->_Data->elementSizeInBytes(); };
    virtual void* rowMajorAddressOfElement_(size_t index) const override { return this->_Data->rowMajorAddressOfElement_(index+this->_DisplacedIndexOffset); };
    virtual bool adjustableArrayP() const { return true; };
    virtual bool displacedToP() const { return this->_Flags.displacedToP(); };
  public:
    virtual size_t arrayTotalSize() const { return this->_ArrayTotalSize; };
    virtual T_sp displacedTo() const {
      if (this->_Flags.displacedToP()) return this->_Data;
      return _Nil<T_O>();
    };
    // like the above but ignores the flag
    virtual T_sp realDisplacedTo() const {
      return this->_Data;
    }

    virtual size_t rank() const override { return this->_Dimensions._Length; };
    virtual size_t arrayDimension(size_t axisNumber) const override {
      LIKELY_if (axisNumber<this->_Dimensions._Length) return this->_Dimensions[axisNumber];
      badAxisNumberError(cl::_sym_arrayDimension,this->_Dimensions._Length,axisNumber);

    };
    virtual size_t displacedIndexOffset() const override {return this->_DisplacedIndexOffset;}
    virtual bool arrayHasFillPointerP() const override { return this->_Flags.fillPointerP(); };
    virtual T_sp replaceArray(T_sp other) override;
    virtual void sxhash_(HashGenerator& hg) const;
    void fillPointerSet(size_t idx) {
      this->_FillPointerOrLengthOrDummy = idx;
    };
    size_t fillPointer() const {
      return this->_FillPointerOrLengthOrDummy;
    };
    virtual bool equalp(T_sp other) const override;
    virtual std::string get_std_string() const {notStringError(this->asSmartPtr()); }
    virtual vector<size_t> arrayDimensionsAsVector() const {
      vector<size_t> dims;
      for (size_t i(0); i<this->_Dimensions._Length; ++i ) {
        dims.push_back(this->_Dimensions[i]);
      }
      return dims;
    }
    virtual void internalAdjustSize_(size_t size, T_sp init_element=_Nil<T_O>(), bool initElementSupplied=false ) = 0;
    virtual void unsafe_fillArrayWithElt(T_sp element, size_t start, size_t end) final
    {
      this->_Data->unsafe_fillArrayWithElt(element,start+this->_DisplacedIndexOffset,end+this->_DisplacedIndexOffset);
    }
    void ensureSpaceAfterFillPointer(T_sp init_element, size_t size);
    virtual T_sp vectorPush(T_sp newElement) final;
    virtual Fixnum_sp vectorPushExtend(T_sp newElement, size_t extension = 0) override;
    virtual Array_sp unsafe_subseq(size_t start, size_t end) const override;
    virtual Array_sp unsafe_setf_subseq(size_t start, size_t end, Array_sp newSubseq) override;
  };
};

namespace core {
FORWARD(ComplexVector);
class ComplexVector_O : public MDArray_O {
  LISP_CLASS(core, CorePkg, ComplexVector_O, "ComplexVector",MDArray_O);
    // One dimension vector
 ComplexVector_O(Rank1 dummy,
                 size_t dimension,
                 T_sp fillPointer,
                 Array_sp data,
                 bool displacedToP,
                 Fixnum_sp displacedIndexOffset) : MDArray_O(Rank1(),dimension,fillPointer,data,displacedToP,displacedIndexOffset) {};
 public:
    virtual void __write__(T_sp strm) const;
};
};


namespace core {
  class SimpleMDArray_O : public MDArray_O {
    LISP_CLASS(core, CorePkg, SimpleMDArray_O, "simple-mdarray",MDArray_O);
    virtual ~SimpleMDArray_O() {};
  public:
    SimpleMDArray_O(size_t dimension,
                    Array_sp data) : MDArray_O(Rank1(),dimension,_Nil<T_O>(),data,false,clasp_make_fixnum(0)) {};
    // multiple dimensions
    SimpleMDArray_O(size_t rank,
                    List_sp dimensions,
                    Array_sp data) : MDArray_O(rank,dimensions,data,false,clasp_make_fixnum(0)) {};
  public:
    virtual T_sp array_type() const override {return cl::_sym_simple_array;};
    virtual bool adjustableArrayP() const { return false; };
    virtual bool fillPointerP() const { return false; };
    virtual bool displacedToP() const { return false; };
  };
};




// ----------------------------------------------------------------------
//
// here go Simple vectors
//
namespace core {

  class AbstractSimpleVector_O : public Array_O {
    LISP_CLASS(core, CorePkg, AbstractSimpleVector_O, "AbstractSimpleVector",Array_O);
    virtual ~AbstractSimpleVector_O() {};
  public:
    virtual T_sp array_type() const final override { return cl::_sym_simple_array; };
  public:
    virtual Array_sp data() { return gc::As_unsafe<Array_sp>(this->asSmartPtr()); };
    virtual size_t arrayTotalSize() const { return this->length(); };
    virtual void rowMajorAset(size_t idx, T_sp value) = 0;
    virtual T_sp rowMajorAref(size_t idx) const = 0;
    virtual void vset(size_t idx, T_sp value) = 0;
    virtual T_sp vref(size_t idx) const = 0;
    virtual size_t rank() const override { return 1; };
    virtual bool adjustableArrayP() const final {return false;};
    virtual size_t displacedIndexOffset() const override { return 0; };
    virtual size_t arrayDimension(size_t axisNumber) const override {
      unlikely_if (axisNumber!=0) {
        badAxisNumberError(cl::_sym_arrayDimension,1,axisNumber);
      }
      return this->length();
    }
    virtual T_sp vectorPush(T_sp newElement) override {noFillPointerError(cl::_sym_vectorPush,this->asSmartPtr());  };
    virtual Fixnum_sp vectorPushExtend(T_sp newElement, size_t extension) override {noFillPointerError(cl::_sym_vectorPushExtend,this->asSmartPtr());  };
    virtual T_sp replaceArray(T_sp other) override {notAdjustableError(core::_sym_replaceArray,this->asSmartPtr());  };
    virtual std::string get_std_string() const override {notStringError(this->asSmartPtr());};
    virtual void ranged_sxhash(HashGenerator& hg, size_t start, size_t end) const
    {
      TYPE_ERROR(this->asSmartPtr(),Cons_O::createList(cl::_sym_string,cl::_sym_bit_vector));
    };
    virtual void sxhash_(HashGenerator& hg) const override {this->General_O::sxhash_(hg);}
    virtual bool equal(T_sp other) const override { return this->eq(other);};
    virtual bool equalp(T_sp other) const;
    void asAbstractSimpleVectorRange(AbstractSimpleVector_sp& sv, size_t& start, size_t& end) const override {
      sv = this->asSmartPtr();
      start = 0;
      end = this->length();
    }
    virtual vector<size_t> arrayDimensionsAsVector() const {
      vector<size_t> dims;
      dims.push_back(this->length());
      return dims;
    }
  };
};



namespace core {
  template <typename MyLeafType, typename ValueType, typename MyParentType >
    class template_SimpleVector : public MyParentType {
  public:
    // The types that define what this class does
    typedef MyParentType Base;
    typedef MyLeafType /* eg: SimpleVector_O*/ leaf_type;
    typedef ValueType  /*eg: T_sp*/ value_type;
    typedef ValueType  /*eg: T_sp*/ simple_element_type;
    typedef gctools::smart_ptr<leaf_type> leaf_smart_ptr_type;
    typedef gctools::GCArray_moveable<value_type> vector_type;
    typedef value_type* iterator;
    typedef const value_type* const_iterator;
  public:
    vector_type _Data;
  public:
  template_SimpleVector(size_t length, value_type initialElement=value_type(), bool initialElementSupplied=false, size_t initialContentsSize=0, const value_type* initialContents=NULL)
    : Base(), _Data(length,initialElement,initialElementSupplied,initialContentsSize,initialContents) {};
  public:
    static void never_invoke_allocator() {gctools::GCAbstractAllocator<template_SimpleVector>::never_invoke_allocator();};
  public:
    // NULL terminated strings use this - so the ASSERT needs to accept it
    value_type& operator[](size_t index) { BOUNDS_ASSERT_LT(index,this->length());return this->_Data[index];};
    const value_type& operator[](size_t index) const { BOUNDS_ASSERT_LT(index,this->length());return this->_Data[index];};
    iterator begin() { return &this->_Data[0];};
    iterator end() { return &this->_Data[this->_Data._Length]; }
    const_iterator begin() const { return &this->_Data[0];};
    const_iterator end() const { return &this->_Data[this->_Data._Length]; }
    virtual size_t elementSizeInBytes() const override {return sizeof(value_type); };
    virtual void* rowMajorAddressOfElement_(size_t i) const override {return (void*)&(this->_Data[i]);};
    virtual void unsafe_fillArrayWithElt(T_sp initialElement, size_t start, size_t end) override {
      for (size_t i(start); i<end; ++i ) {
        (*this)[i] = leaf_type::from_object(initialElement);
      }
    };
    virtual Array_sp reverse() const final { return templated_ranged_reverse<leaf_type>(*reinterpret_cast<const leaf_type*>(this),0,this->length()); };
    virtual Array_sp nreverse() final { return templated_ranged_nreverse(*this,0,this->length()); };
    CL_METHOD_OVERLOAD virtual void rowMajorAset(size_t idx, T_sp value) final {(*this)[idx] = leaf_type::from_object(value);}
    CL_METHOD_OVERLOAD virtual T_sp rowMajorAref(size_t idx) const final {return leaf_type::to_object((*this)[idx]);}
    CL_METHOD_OVERLOAD virtual void vset(size_t idx, T_sp value) final {(*this)[idx] = leaf_type::from_object(value);}
    CL_METHOD_OVERLOAD virtual T_sp vref(size_t idx) const final {return leaf_type::to_object((*this)[idx]);}
    virtual Array_sp unsafe_subseq(size_t start, size_t end) const final {
      BOUNDS_ASSERT(start<=end&&end<=this->length());
      return leaf_type::make(end-start,value_type(),true,end-start,(value_type*)this->rowMajorAddressOfElement_(start));
    }
    virtual Array_sp unsafe_setf_subseq(size_t start, size_t end, Array_sp newSubseq) final {
      // TODO: Write specialized versions of this to speed it up
      BOUNDS_ASSERT(start<=end&&end<=this->length());
      for ( size_t i(start),ni(0); i<end; ++i,++ni ) {
        (*this)[i] = leaf_type::from_object(newSubseq->rowMajorAref(ni));
      }
      return newSubseq;
    }
  };
};

namespace core {
  template <typename MyLeafType, size_t BitUnitBitWidth, typename MyParentType>
    class template_SimpleBitUnitVector : public MyParentType {
  public:
    typedef MyParentType Base;
    typedef MyLeafType leaf_type;
    typedef unsigned char value_type;
    typedef value_type simple_element_type;
    typedef gctools::smart_ptr<leaf_type> leaf_smart_ptr_type;
    typedef gctools::GCBitUnitArray_moveable<BitUnitBitWidth> bitunit_array_type;
  public:
    bitunit_array_type _Data;
  template_SimpleBitUnitVector(size_t length, bit_array_word initialElement, bool initialElementSupplied,
                               size_t initialContentsSize = 0, const bit_array_word* initialContents = NULL)
    : Base(), _Data(length,initialElement,initialElementSupplied,initialContentsSize,initialContents) {};
  public:
    typename bitunit_array_type::reference operator[](size_t index) {
      BOUNDS_ASSERT_LT(index,this->length());
      return this->_Data.ref(index);
    }
    value_type operator[](size_t index) const {
      BOUNDS_ASSERT_LT(index, this->length());
      return this->_Data.unsignedBitUnit(index);
    }
    // Given an initial element, replicate it into a bit_array_word. E.g. 01 becomes 01010101...01
    // Hopefully the compiler unrolls the loop.
    static bit_array_word initialFillValue(value_type initialValue) {
      bit_array_word result = initialValue;
      for (size_t i = BitUnitBitWidth; i < BIT_ARRAY_WORD_BITS; i *= 2) result |= result << i;
      return result;
    }
    // Since we know the element type, we can define these here instead of in the child.
    static value_type from_object(T_sp object) {
      if (object.fixnump()) {
        Fixnum i = object.unsafe_fixnum();
        if ((0 <= i) && (i < (1 << BitUnitBitWidth))) return i;
      }
      TYPE_ERROR(object, Cons_O::createList(cl::_sym_UnsignedByte, clasp_make_fixnum(BitUnitBitWidth)));
    }
    static T_sp to_object(value_type v) { return clasp_make_integer(v); }
    virtual void unsafe_fillArrayWithElt(T_sp initialElement, size_t start, size_t end) override {
      // FIXME: Could be done more efficiently by writing whole words, but be careful about the ends.
      for (size_t i = start; i < end; ++i) (*this)[i] = from_object(initialElement);
    }
    virtual size_t elementSizeInBytes() const override {bitVectorDoesntSupportError();}
    virtual void* rowMajorAddressOfElement_(size_t i) const override {bitVectorDoesntSupportError();}
    static value_type default_initial_element(void) { return 0; }
    virtual T_sp element_type() const override {
      return Cons_O::createList(cl::_sym_UnsignedByte, clasp_make_fixnum(BitUnitBitWidth));
    }
    virtual Array_sp reverse() const final { return templated_ranged_reverse<leaf_type>(*reinterpret_cast<const leaf_type*>(this),0,this->length()); }
    virtual Array_sp nreverse() final { return templated_ranged_nreverse(*this,0,this->length()); }
    CL_METHOD_OVERLOAD virtual void rowMajorAset(size_t idx, T_sp value) final {(*this)[idx] = from_object(value);}
    CL_METHOD_OVERLOAD virtual T_sp rowMajorAref(size_t idx) const final {return to_object((*this)[idx]);}
    CL_METHOD_OVERLOAD virtual void vset(size_t idx, T_sp value) final {(*this)[idx] = from_object(value);}
    CL_METHOD_OVERLOAD virtual T_sp vref(size_t idx) const final {return to_object((*this)[idx]);}
    Array_sp unsafe_subseq(size_t start, size_t end) const {
      BOUNDS_ASSERT(0<=start&&start<end&&end<=this->length());
      leaf_smart_ptr_type sbv = leaf_type::make(end-start);
      for (size_t i(0),iEnd(end-start);i<iEnd;++i)
        (*sbv)[i] = (*this)[start+i];
      return sbv;
    }
    Array_sp unsafe_setf_subseq(size_t start, size_t end, Array_sp other) {
      BOUNDS_ASSERT(0<=start&&start<end&&end<=this->length());
      for (size_t i = start, ni = 0; i < end; ++i, ++ni)
        (*this)[i] = from_object(other->rowMajorAref(ni));
      return other;
    }
  };
}; // namespace core

namespace core {
  Vector_sp core__make_vector(T_sp element_type,
                              size_t dimension,
                              bool adjustable = false,
                              T_sp fill_pointer = _Nil<T_O>(),
                              T_sp displaced_to = _Nil<T_O>(),
                              Fixnum_sp displacedIndexOffset = clasp_make_fixnum(0),
                              T_sp initial_element = _Nil<T_O>(),
                              bool initial_element_supplied_p = false);

Vector_sp core__make_static_vector(T_sp element_type,
                              size_t dimension,
                              T_sp initial_element = _Nil<T_O>(),
                              bool initial_element_supplied_p = false);

MDArray_sp core__make_mdarray(List_sp dimensions,
                              T_sp element_type,
                              bool adjustable = false,
                              T_sp displacedTo = _Nil<T_O>(),
                              Fixnum_sp displacedIndexOffset = clasp_make_fixnum(0),
                              T_sp initialElement = _Nil<T_O>(),
                              bool initialElementSuppliedP = false);

};

namespace core {
  template <typename MyArrayType, typename MySimpleArrayType, typename MySimpleType, typename MyParentType >
    class template_Array : public MyParentType {
  public:
    // The types that define what this class does
    typedef MyParentType Base;
    typedef MyArrayType /*eg: ComplexVector_T_O*/ my_array_type;
    typedef MySimpleArrayType /*eg: ComplexVector_T_O*/ my_simple_array_type;
    typedef MySimpleType /*eg: SimpleVector_O*/ simple_type;
    typedef typename simple_type::simple_element_type /*eg: T_sp*/ simple_element_type;
    typedef gctools::smart_ptr<my_array_type> my_smart_ptr_type;
    typedef gctools::smart_ptr<my_simple_array_type> my_simple_smart_ptr_type;
    typedef gctools::GCArray_moveable<simple_element_type> simple_vector_type;
    typedef typename MDArray_O::value_type dimension_element_type;
  public:
    // vector
  template_Array(Rank1 dummy,
                 size_t dimension,
                 T_sp fillPointer,
                 Array_sp data,
                 bool displacedToP,
                 Fixnum_sp displacedIndexOffset)
    : Base(dummy,dimension,fillPointer,data,displacedToP,displacedIndexOffset) {};
    // multidimensional array
  template_Array(size_t rank,
                 List_sp dimensions,
                 Array_sp data,
                 bool displacedToP,
                 Fixnum_sp displacedIndexOffset)
    : Base(rank,dimensions,data,displacedToP,displacedIndexOffset) {};
  public:
    // Primary functions/operators for operator[] that handle displacement
    // There's a non-const and a const version of each
    template <typename array_type>
    simple_element_type& unsafe_indirectReference(size_t index) {
      array_type& vecns = *reinterpret_cast<array_type*>(&*this->_Data);
      return vecns[this->_DisplacedIndexOffset+index];
    }
    simple_element_type& operator[](size_t index) {
      BOUNDS_ASSERT(index<this->arrayTotalSize());
      LIKELY_if (gc::IsA<gc::smart_ptr<simple_type>>(this->_Data)) {
        return (*reinterpret_cast<simple_type*>(&*(this->_Data)))[this->_DisplacedIndexOffset+index];
      }
      if (gc::IsA<my_smart_ptr_type>(this->_Data)) return this->unsafe_indirectReference<my_array_type>(index);
      return this->unsafe_indirectReference<my_simple_array_type>(index);
    }
    template <typename array_type>
    const simple_element_type& unsafe_indirectReference(size_t index) const {
      array_type& vecns = *reinterpret_cast<array_type*>(&*this->_Data);
      return vecns[this->_DisplacedIndexOffset+index];
    }
    const simple_element_type& operator[](size_t index) const {
      BOUNDS_ASSERT(index<this->arrayTotalSize());
      LIKELY_if (gc::IsA<gc::smart_ptr<simple_type>>(this->_Data)) {
        return (*reinterpret_cast<simple_type*>(&*(this->_Data)))[this->_DisplacedIndexOffset+index];
      }
      if (gc::IsA<my_smart_ptr_type>(this->_Data)) return this->unsafe_indirectReference<my_array_type>(index);
      return this->unsafe_indirectReference<my_simple_array_type>(index);
    }
  public:
    // Iterators
    simple_element_type* begin() { return &(*this)[0]; };
    simple_element_type* end() { return &(*this)[this->length()]; };
    const simple_element_type* begin() const { return &(*this)[0]; };
    const simple_element_type* end() const { return &(*this)[this->length()]; };
  public:
    void this_asAbstractSimpleVectorRange(AbstractSimpleVector_sp& sv, size_t& start, size_t& end) const  {
      unlikely_if (gc::IsA<my_smart_ptr_type>(this->_Data)) {
        this->_Data->asAbstractSimpleVectorRange(sv,start,end);
        start += this->_DisplacedIndexOffset;
        end = this->length()+this->_DisplacedIndexOffset;
        return;
      }
      sv = gc::As<AbstractSimpleVector_sp>(this->_Data);
      start = this->_DisplacedIndexOffset;
      end = this->length()+this->_DisplacedIndexOffset;
    }
    void asAbstractSimpleVectorRange(AbstractSimpleVector_sp& sv, size_t& start, size_t& end) const final {
      this->this_asAbstractSimpleVectorRange(sv,start,end);
    }
    virtual Array_sp reverse() const final { return templated_reverse_VectorNs(*this); };
    virtual Array_sp nreverse() final { templated_nreverse_VectorNs(*this); return this->asSmartPtr(); };
    virtual void internalAdjustSize_(size_t size, T_sp initElement=_Nil<T_O>(), bool initElementSupplied=false ) final {
      if (size == this->_ArrayTotalSize) return;
      AbstractSimpleVector_sp basesv;
      size_t start, end;
      this->this_asAbstractSimpleVectorRange(basesv,start,end);
      gctools::smart_ptr<simple_type> sv = gc::As_unsafe<gctools::smart_ptr<simple_type>>(basesv);
      size_t initialContentsSize = MIN(this->length(),size);
      gctools::smart_ptr<simple_type> newData = simple_type::make(size,initElementSupplied ? simple_type::from_object(initElement) : simple_type::default_initial_element(),initElementSupplied,initialContentsSize,&(*sv)[start]);
      this->set_data(newData);
//      printf("%s:%d:%s  original size=%lu new size=%lu  copied %lu elements\n", __FILE__, __LINE__, __FUNCTION__, this->_ArrayTotalSize, size, initialContentsSize );
      this->_ArrayTotalSize = size;
      this->_Dimensions[0] = size;
      if (!this->_Flags.fillPointerP()) this->_FillPointerOrLengthOrDummy = size;
      this->_DisplacedIndexOffset = 0;
      this->_Flags.set_displacedToP(false);
    }
    CL_METHOD_OVERLOAD virtual void rowMajorAset(size_t idx, T_sp value) final {(*this)[idx] = simple_type::from_object(value);}
    CL_METHOD_OVERLOAD virtual T_sp rowMajorAref(size_t idx) const final {return simple_type::to_object((*this)[idx]);}
    bool equal(T_sp obj) const override { return this->eq(obj); };
  };
};

namespace core {
  template <typename MyArrayType, typename MySimpleType, typename MyParentType >
    class template_Vector : public MyParentType {
  public:
    // The types that define what this class does
    typedef MyParentType Base;
    typedef MyArrayType /*eg: ComplexVector_T_O*/ my_array_type;
    typedef MySimpleType /*eg: SimpleVector_O*/ simple_type;
    typedef typename simple_type::simple_element_type /*eg: T_sp*/ simple_element_type;
    typedef gctools::smart_ptr<my_array_type> my_smart_ptr_type;
    typedef gctools::GCArray_moveable<simple_element_type> simple_vector_type;
    typedef typename MDArray_O::value_type dimension_element_type;
  public:
    // vector
  template_Vector(Rank1 dummy,
                 size_t dimension,
                 T_sp fillPointer,
                 Array_sp data,
                 bool displacedToP,
                 Fixnum_sp displacedIndexOffset)
    : Base(dummy,dimension,fillPointer,data,displacedToP,displacedIndexOffset) {};
  public:
    // Primary functions/operators for operator[] that handle displacement
    // There's a non-const and a const version of each
    simple_element_type& unsafe_indirectReference(size_t index) {
      my_array_type& vecns = *reinterpret_cast<my_array_type*>(&*this->_Data);
      return vecns[this->_DisplacedIndexOffset+index];
    }
    simple_element_type& operator[](size_t index) {
      BOUNDS_ASSERT(index<this->arrayTotalSize());
      // FIXME: This is sketchy - we treat everything but a simple vector as being a complex vector.
      // ASSUMING ComplexVector_O and MDArray_O have the same data layout, this should work.
      // The definition in template_Array is similarly sketchy in the case of an mdarray displaced
      // to a complex vector.
      LIKELY_if (gc::IsA<gc::smart_ptr<simple_type>>(this->_Data))
        return (*reinterpret_cast<simple_type*>(&*(this->_Data)))[this->_DisplacedIndexOffset+index];
      else return this->unsafe_indirectReference(index);
    }
    const simple_element_type& unsafe_indirectReference(size_t index) const {
      my_array_type& vecns = *reinterpret_cast<my_array_type*>(&*this->_Data);
      return vecns[this->_DisplacedIndexOffset+index];
    }
    const simple_element_type& operator[](size_t index) const {
      BOUNDS_ASSERT(index<this->arrayTotalSize());
      LIKELY_if (gc::IsA<gc::smart_ptr<simple_type>>(this->_Data))
        return (*reinterpret_cast<simple_type*>(&*(this->_Data)))[this->_DisplacedIndexOffset+index];
      else return this->unsafe_indirectReference(index);
    }
  public:
    // Iterators
    simple_element_type* begin() { return &(*this)[0]; };
    simple_element_type* end() { return &(*this)[this->length()]; };
    const simple_element_type* begin() const { return &(*this)[0]; };
    const simple_element_type* end() const { return &(*this)[this->length()]; };
  public:
    void this_asAbstractSimpleVectorRange(AbstractSimpleVector_sp& sv, size_t& start, size_t& end) const  {
      unlikely_if (gc::IsA<my_smart_ptr_type>(this->_Data)) {
        this->_Data->asAbstractSimpleVectorRange(sv, start, end);
        start += this->_DisplacedIndexOffset;
        end = this->length()+this->_DisplacedIndexOffset;
        return;
      }
      sv = gc::As<AbstractSimpleVector_sp>(this->_Data);
      start = this->_DisplacedIndexOffset;
      end = this->length()+this->_DisplacedIndexOffset;
    }
    void asAbstractSimpleVectorRange(AbstractSimpleVector_sp& sv, size_t& start, size_t& end) const final {
      this->this_asAbstractSimpleVectorRange(sv,start,end);
    }
    virtual Array_sp reverse() const final { return templated_reverse_VectorNs(*this); };
    virtual Array_sp nreverse() final { templated_nreverse_VectorNs(*this); return this->asSmartPtr(); };
    virtual void internalAdjustSize_(size_t size, T_sp initElement=_Nil<T_O>(), bool initElementSupplied=false ) final {
      if (size == this->_ArrayTotalSize) return;
      AbstractSimpleVector_sp basesv;
      size_t start, end;
      this->this_asAbstractSimpleVectorRange(basesv,start,end);
      gctools::smart_ptr<simple_type> sv = gc::As_unsafe<gctools::smart_ptr<simple_type>>(basesv);
      size_t initialContentsSize = MIN(this->length(),size);
      gctools::smart_ptr<simple_type> newData = simple_type::make(size,initElementSupplied ? simple_type::from_object(initElement) : simple_type::default_initial_element(),initElementSupplied,initialContentsSize,&(*sv)[start]);
      this->set_data(newData);
//      printf("%s:%d:%s  original size=%lu new size=%lu  copied %lu elements\n", __FILE__, __LINE__, __FUNCTION__, this->_ArrayTotalSize, size, initialContentsSize );
      this->_ArrayTotalSize = size;
      this->_Dimensions[0] = size;
      if (!this->_Flags.fillPointerP()) this->_FillPointerOrLengthOrDummy = size;
      this->_DisplacedIndexOffset = 0;
      this->_Flags.set_displacedToP(false);
    }
    CL_METHOD_OVERLOAD virtual void rowMajorAset(size_t idx, T_sp value) final {(*this)[idx] = simple_type::from_object(value);}
    CL_METHOD_OVERLOAD virtual T_sp rowMajorAref(size_t idx) const final {return simple_type::to_object((*this)[idx]);}
    bool equal(T_sp obj) const override { return this->eq(obj); };
    // NOTE: For only ComplexVector_T_O this will override the ComplexVector_O function,
    // since simple_element_type = T_sp, but that's harmless since they have the same effect.
    Fixnum_sp vectorPushExtend(simple_element_type newElement, size_t extension = 0) {
      unlikely_if (!this->_Flags.fillPointerP()) noFillPointerSpecializedArrayError(this->asSmartPtr());
      cl_index idx = this->_FillPointerOrLengthOrDummy;
      unlikely_if (idx >= this->_ArrayTotalSize) {
        if (extension <= 0) extension = calculate_extension(this->_ArrayTotalSize);
        cl_index new_size = this->_ArrayTotalSize+extension;
        unlikely_if (!cl::_sym_adjust_array || !lisp_boundp(cl::_sym_adjust_array)) {
          this->internalAdjustSize_(new_size);
        } else {
          lisp_adjust_array(this->asSmartPtr(),clasp_make_fixnum(new_size),clasp_make_fixnum(this->_FillPointerOrLengthOrDummy));
        }
      }
      (*this)[idx] = newElement;
      ++this->_FillPointerOrLengthOrDummy;
      return make_fixnum(idx);
    }
  };
};

namespace core {
  template <typename MyArrayType, typename MySimpleType, typename MyParentType >
    class template_SimpleArray : public MyParentType {
  public:
    // The types that define what this class does
    typedef MyParentType Base;
    typedef MyArrayType /*eg: ComplexVector_T_O*/ my_array_type;
    typedef MySimpleType /*eg: SimpleVector_O*/ simple_type;
    typedef typename simple_type::simple_element_type /*eg: T_sp*/ simple_element_type;
    typedef gctools::smart_ptr<my_array_type> my_smart_ptr_type;
    typedef gctools::GCArray_moveable<simple_element_type> simple_vector_type;
    typedef typename MDArray_O::value_type dimension_element_type;
  public:
    // vector
  template_SimpleArray(size_t dimension, Array_sp data) : Base(dimension,data) {};
  template_SimpleArray(size_t rank, List_sp dimensions, Array_sp data)
    : Base(rank,dimensions,data) {};
  public:
    // Primary functions/operators for operator[] that handle displacement
    // There's a non-const and a const version of each
    simple_element_type& operator[](size_t index) {
      BOUNDS_ASSERT(index<this->arrayTotalSize());
      return (*reinterpret_cast<simple_type*>(&*(this->_Data)))[index];
    }
    const simple_element_type& operator[](size_t index) const {
      BOUNDS_ASSERT(index<this->arrayTotalSize());
      return (*reinterpret_cast<simple_type*>(&*(this->_Data)))[index];
    }
  public:
    // Iterators
    simple_element_type* begin() { return &(*this)[0]; };
    simple_element_type* end() { return &(*this)[this->length()]; };
    const simple_element_type* begin() const { return &(*this)[0]; };
    const simple_element_type* end() const { return &(*this)[this->length()]; };
  public:
    virtual Array_sp reverse() const final { return templated_reverse_VectorNs(*this); };
    virtual Array_sp nreverse() final { templated_nreverse_VectorNs(*this); return this->asSmartPtr(); };
    virtual void internalAdjustSize_(size_t size, T_sp initElement=_Nil<T_O>(), bool initElementSupplied=false ) final {cannotAdjustSizeOfSimpleArrays(this->asSmartPtr());};
public:
    void this_asAbstractSimpleVectorRange(AbstractSimpleVector_sp& sv, size_t& start, size_t& end) const  {
      ASSERT(gc::IsA<AbstractSimpleVector_sp>(this->_Data));
      sv = gc::As_unsafe<AbstractSimpleVector_sp>(this->_Data);
      start = this->_DisplacedIndexOffset;
      end = this->length()+this->_DisplacedIndexOffset;
    }
    void asAbstractSimpleVectorRange(AbstractSimpleVector_sp& sv, size_t& start, size_t& end) const final {
      this->this_asAbstractSimpleVectorRange(sv,start,end);
    }
    CL_METHOD_OVERLOAD virtual void rowMajorAset(size_t idx, T_sp value) final {(*this)[idx] = simple_type::from_object(value);}
    CL_METHOD_OVERLOAD virtual T_sp rowMajorAref(size_t idx) const final {return simple_type::to_object((*this)[idx]);}
    bool equal(T_sp obj) const override { return this->eq(obj); };
  };
};

namespace core {
  size_t calculateArrayTotalSizeAndValidateDimensions(List_sp dim_desig, size_t& rank);
};

// It's easier to manage the duplicate code if its isolated in separate files

#include <clasp/core/array_t.h>
#include <clasp/core/string.h>
#include <clasp/core/array_double.h>
#include <clasp/core/array_float.h>
#include <clasp/core/array_size_t.h>
#include <clasp/core/array_fixnum.h>
#include <clasp/core/array_int64.h>
#include <clasp/core/array_int32.h>
#include <clasp/core/array_int16.h>
#include <clasp/core/array_int8.h>
#include <clasp/core/array_int4.h>
#include <clasp/core/array_bit.h>

// ----------------------------------------------------------------------
//
// functions
//


namespace core {
// Like ecl__vector_start_end
  T_mv clasp_vectorStartEnd(Symbol_sp fn, T_sp thing, Fixnum_sp start, Fixnum_sp end);

};



// ----------------------------------------------------------------------
//
// Array and String functions
//

namespace core {

CL_LAMBDA(dest destStart orig origStart len);
CL_DECLARE();
CL_DOCSTRING("copy_subarray");
 CL_DEFUN void core__copy_subarray(Array_sp dest, Fixnum_sp destStart, Array_sp orig, Fixnum_sp origStart, Fixnum_sp len);  
}; // namespace core

#endif /* _core_Array_H */
