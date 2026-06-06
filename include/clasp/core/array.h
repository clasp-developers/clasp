#pragma once
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

#include <algorithm> // range algorithms
#include <limits>    // numeric_limits for casting
#include <type_traits> // is_integral etc.
#include <concepts> // beware
#include <clasp/core/array.fwd.h>
#include <clasp/core/clasp_gmpxx.h>
#include <clasp/core/object.h>
#include <clasp/core/numbers.h> // need full definitions for to_object.
#include <clasp/core/character.fwd.h>
#include <clasp/core/sequence.fwd.h>
#include <clasp/core/corePackage.fwd.h>
#include <clasp/core/exceptions.h>
#include <clasp/core/foundation.h>

namespace core {
size_t calculate_extension(size_t arrayTotalSize);
};

SYMBOL_EXPORT_SC_(ExtPkg, cl_index);
SYMBOL_EXPORT_SC_(ExtPkg, byte2);
SYMBOL_EXPORT_SC_(ExtPkg, byte4);
SYMBOL_EXPORT_SC_(ExtPkg, byte8);
SYMBOL_EXPORT_SC_(ExtPkg, byte16);
SYMBOL_EXPORT_SC_(ExtPkg, byte32);
SYMBOL_EXPORT_SC_(ExtPkg, byte64);
SYMBOL_EXPORT_SC_(ExtPkg, integer2);
SYMBOL_EXPORT_SC_(ExtPkg, integer4);
SYMBOL_EXPORT_SC_(ExtPkg, integer8);
SYMBOL_EXPORT_SC_(ExtPkg, integer16);
SYMBOL_EXPORT_SC_(ExtPkg, integer32);
SYMBOL_EXPORT_SC_(ExtPkg, integer64);

namespace cl {
extern core::Symbol_sp& _sym_fixnum;
};

namespace ext {
extern core::Symbol_sp& _sym_cl_index;
extern core::Symbol_sp& _sym_byte64;
extern core::Symbol_sp& _sym_byte32;
extern core::Symbol_sp& _sym_byte16;
extern core::Symbol_sp& _sym_byte8;
extern core::Symbol_sp& _sym_byte4;
extern core::Symbol_sp& _sym_byte2;
extern core::Symbol_sp& _sym_integer64;
extern core::Symbol_sp& _sym_integer32;
extern core::Symbol_sp& _sym_integer16;
extern core::Symbol_sp& _sym_integer8;
extern core::Symbol_sp& _sym_integer4;
extern core::Symbol_sp& _sym_integer2;
}; // namespace ext

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
extern core::Symbol_sp& _sym_short_float;
extern core::Symbol_sp& _sym_long_float;
extern core::Symbol_sp& _sym_UnsignedByte;
extern core::Symbol_sp& _sym_T_O;
extern core::Symbol_sp& _sym_simple_string;
extern core::Symbol_sp& _sym_simple_array;
}; // namespace cl

namespace core {
extern core::Symbol_sp& _sym_replaceArray;
extern core::Symbol_sp& _sym_swapElements;
extern core::Symbol_sp& _sym_fillPointerSet;
extern core::Symbol_sp& _sym_fillArrayWithElt;
extern core::Symbol_sp& _sym_setf_subseq;
extern void clasp_write_string(const string& str, T_sp strm);
extern claspCharacter stream_write_char(T_sp strm, claspCharacter c);
}; // namespace core

namespace core {

List_sp cl__arrayDimensions(Array_sp array);

};

namespace core {

// ------------------------------------------------------------
// Utility

[[noreturn]] void bitVectorDoesntSupportError();
[[noreturn]] void missingValueListError(List_sp indices);
[[noreturn]] void tooManyIndicesListError(List_sp indices);
[[noreturn]] void missingValueVaslistError(Vaslist_sp indices);
[[noreturn]] void tooManyIndicesVaslistError(Vaslist_sp indices);
[[noreturn]] void badAxisNumberError(Symbol_sp fn_name, size_t rank, size_t axisNumber);
[[noreturn]] void badIndexError(T_sp arr, size_t axis, size_t oneIndex, size_t curDimension);
[[noreturn]] void badRMIndexError(T_sp arr, size_t oneIndex, size_t totalSize);
[[noreturn]] void indexNotFixnumError(T_sp index);
[[noreturn]] void insufficientIndexListError(List_sp indices);
[[noreturn]] void insufficientIndexVaslistError(Vaslist_sp indices);
[[noreturn]] void notStringError(T_sp obj);
[[noreturn]] void cannotAdjustSizeOfSimpleArrays(T_sp obj);
[[noreturn]] void notSequenceError(T_sp obj);
[[noreturn]] void noFillPointerError(Symbol_sp fn_name, T_sp array);
[[noreturn]] void noFillPointerSpecializedArrayError(T_sp array);
[[noreturn]] void notAdjustableError(Symbol_sp fn_name, T_sp array);
[[noreturn]] void notVectorError(T_sp array);

size_t calculateArrayTotalSizeAndValidateDimensions(List_sp dim_desig, size_t& rank);
}; // namespace core

namespace core {
class Array_O : public General_O {
  LISP_ABSTRACT_CLASS(core, ClPkg, Array_O, "array", General_O);

public:
  /*! A hackish (clever?) way to get at the first element of all subclasses
      - which will be the FillPointer/Length for vectors and a Dummy for Arrays.
      - The first field of every subclass needs to be a size_t length/fillPointer.
 */
  size_t _Length[0];

public:
  // Low level functions for access to contents

  virtual size_t elementSizeInBytes() const = 0;
  virtual void* rowMajorAddressOfElement_(size_t index) const = 0;
  virtual void asAbstractSimpleVectorRange(AbstractSimpleVector_sp& sv, size_t& start, size_t& end) const = 0;

public: // Functions here
  virtual T_sp type_of() const {
    return Cons_O::createList(this->array_type(), this->element_type(), cl__arrayDimensions(this->asSmartPtr()));
  };
  virtual T_sp array_type() const = 0;
  /*! This replicates ECL ecl__elttype_to_symbol in array.d */
  virtual T_sp element_type() const = 0;
  /*! length() doesn't dispatch - it reaches into the subclass
      through the _Length[0] array to read the first size_t element
      which is the Length/FillPointer for vectors and a Dummy value for arrays */
  size_t length() const { return this->_Length[0]; };
  virtual bool equal(T_sp other) const override = 0;
  virtual bool equalp(T_sp other) const override = 0;
  virtual size_t arrayTotalSize() const = 0;
  virtual void rowMajorAset(size_t idx, T_sp value) = 0;
  virtual T_sp rowMajorAref(size_t idx) const = 0;
  virtual bool adjustableArrayP() const = 0;
  virtual bool displacedToP() const { return false; };
  /*! As the CL function: Return the offset into a one-dimensional vector for the
      multidimensional indices in the (Va)List, using row-major ordering. */
  size_t arrayRowMajorIndex(Vaslist_sp indices) const;
  size_t arrayRowMajorIndex(List_sp indices) const;
  virtual Array_sp reverse() const = 0;
  virtual Array_sp nreverse() = 0;
  virtual size_t rank() const = 0;
  /*! Return the offset into a one-dimensional vector for the multidimensional index
      in the vector<int>s.  This is in rowMajor order.
      Separate from arrayRowMajorIndex because it's internal and does less error checking. */
  size_t index_vector_int(const vector<int>& indices) const;
  virtual bool arrayHasFillPointerP() const { return false; };
  virtual void fillPointerSet(size_t f) { noFillPointerError(cl::_sym_fillPointer, this->asSmartPtr()); };
  virtual size_t fillPointer() const { noFillPointerError(cl::_sym_fillPointer, this->asSmartPtr()); };
  virtual size_t displacedIndexOffset() const = 0;
  /*! Return the array dimension along the axis-number */
  virtual size_t arrayDimension(size_t axisNumber) const = 0;
  /*! Return the value at the indices */
  virtual T_sp replaceArray(T_sp other) = 0;
  virtual void __write__(T_sp strm) const override;
  virtual void __writeString(size_t istart, size_t iend, T_sp stream) const;
  virtual string __repr__() const override;
  // ------------------------------------------------------------
  //
  // String functions
  virtual std::string get_std_string() const = 0;
  // Get a string usable as a path. Has to take care of encoding.
  virtual std::string get_path_string() const = 0;
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
    if (this->rank() != 1)
      notVectorError(this->asSmartPtr());
    size_t_pair p = sequenceStartEnd(cl::_sym_subseq, this->length(), start, end);
    return this->unsafe_subseq(p.start, p.end);
  }
  virtual T_sp setf_subseq(size_t start, T_sp end, T_sp new_subseq) {
    if (this->rank() != 1)
      notVectorError(this->asSmartPtr());
    size_t_pair p = sequenceStartEnd(core::_sym_setf_subseq, this->length(), start, end);
    Array_sp newVec = gc::As<Array_sp>(new_subseq);
    this->copy_nd(newVec, p.start, 0, p.end - p.start);
    return newVec;
  }
  void fillInitialContents(T_sp initialContents);
  virtual void sxhash_equalp(HashGenerator& hg) const override;
  // --------------------------------------------------
  // Unsafe ranged operations with explicit limits
  virtual Array_sp unsafe_subseq(size_t start, size_t end) const = 0;
  virtual void unsafe_fillArrayWithElt(T_sp initial_element, size_t start, size_t end) = 0;
  // copy_n (below) but dispatching on the dest.
  // does not fully dispatch on the source's element type, but at least handles
  // the common case of dest and source having the same element type,
  // and unwrapping MDArrays.
  virtual void copy_nd(Array_sp source, size_t dest_start, size_t source_start, size_t len) = 0;
};

}; // namespace core

namespace core {
struct Rank1 {};
class MDArray_O : public Array_O {
  LISP_ABSTRACT_CLASS(core, CorePkg, MDArray_O, "mdarray", Array_O);

public:
  typedef AbstractSimpleVector_O simple_type;
  typedef size_t value_type; // this is container - needs value_type
  typedef gctools::GCArray_moveable<value_type> vector_type;
  struct Flags {
    size_t _Flags;
    static const size_t fillPointerFlag = 0x000001;
    static const size_t displacedToFlag = 0x000100;
    Flags() = default;
    Flags(bool fillPointerP, bool displacedToP)
        : _Flags((fillPointerP ? fillPointerFlag : 0) | (displacedToP ? displacedToFlag : 0)){};
    bool fillPointerP() const { return this->_Flags & fillPointerFlag; };
    bool displacedToP() const { return this->_Flags & displacedToFlag; };
    void set_displacedToP(bool f) {
      if (f)
        this->_Flags |= displacedToFlag;
      else
        this->_Flags &= ~displacedToFlag;
    };
    bool simpleP() const { return this->_Flags == 0; }
  };

public:
  //! same offset as _fillpointerorlength in nonsimplevector
  size_t _FillPointerOrLengthOrDummy;
  size_t _ArrayTotalSize;
  Array_sp _Data;
  size_t _DisplacedIndexOffset;
  Flags _Flags;
  vector_type _Dimensions;
  // One dimension
  MDArray_O(Rank1 dummy_rank, size_t dimension, T_sp fillPointer, Array_sp data, bool displacedToP, Fixnum_sp displacedIndexOffset);
  // multiple dimensions
  MDArray_O(size_t rank, List_sp dimensions, Array_sp data, bool displacedToP, Fixnum_sp displacedIndexOffset);

public:
  virtual T_sp array_type() const override { return cl::_sym_array; };
  virtual T_sp element_type() const override { return this->_Data->element_type(); };

public:
  virtual Array_sp data() const { return this->_Data; };
  void set_data(Array_sp a);

public:
  virtual size_t elementSizeInBytes() const override { return this->_Data->elementSizeInBytes(); };
  virtual void* rowMajorAddressOfElement_(size_t index) const override {
    return this->_Data->rowMajorAddressOfElement_(index + this->_DisplacedIndexOffset);
  };
  virtual bool adjustableArrayP() const override { return true; };
  virtual bool displacedToP() const override { return this->_Flags.displacedToP(); };

public:
  virtual size_t arrayTotalSize() const override { return this->_ArrayTotalSize; };
  virtual T_sp displacedTo() const {
    if (this->_Flags.displacedToP())
      return this->_Data;
    return nil<T_O>();
  };
  // like the above but ignores the flag
  virtual T_sp realDisplacedTo() const { return this->_Data; }

  virtual size_t rank() const override { return this->_Dimensions.length(); };
  virtual size_t arrayDimension(size_t axisNumber) const override {
    LIKELY_if(axisNumber < this->_Dimensions.length()) return this->_Dimensions[axisNumber];
    badAxisNumberError(cl::_sym_arrayDimension, this->_Dimensions.length(), axisNumber);
  };
  virtual size_t displacedIndexOffset() const override { return this->_DisplacedIndexOffset; }
  virtual bool arrayHasFillPointerP() const override { return this->_Flags.fillPointerP(); };
  virtual T_sp replaceArray(T_sp other) override;
  void fillPointerSet(size_t idx) override {
    // This better not be bigger than the vector size (must be a vector)
    if (idx > this->_ArrayTotalSize)
      SIMPLE_ERROR(("Attempt to set fill-pointer %d past vector size %d"), idx, this->_ArrayTotalSize);
    this->_FillPointerOrLengthOrDummy = idx;
  };
  size_t fillPointer() const override { return this->_FillPointerOrLengthOrDummy; };
  virtual bool equalp(T_sp other) const override;
  virtual std::string get_std_string() const override { notStringError(this->asSmartPtr()); }
  virtual std::string get_path_string() const override { notStringError(this->asSmartPtr()); }
  virtual vector<size_t> arrayDimensionsAsVector() const override {
    return vector<size_t>(_Dimensions.begin(), _Dimensions.end());
  }
  virtual void resize(size_t size, T_sp init_element = nil<T_O>(), bool initElementSupplied = false) = 0;
  virtual void unsafe_fillArrayWithElt(T_sp element, size_t start, size_t end) final {
    this->_Data->unsafe_fillArrayWithElt(element, start + this->_DisplacedIndexOffset, end + this->_DisplacedIndexOffset);
  }
  virtual T_sp vectorPush(T_sp newElement) final;
  virtual Fixnum_sp vectorPushExtend(T_sp newElement, size_t extension = 0) override;
  virtual Array_sp unsafe_subseq(size_t start, size_t end) const override;
};
}; // namespace core

namespace core {
FORWARD(ComplexVector);
class ComplexVector_O : public MDArray_O {
  LISP_ABSTRACT_CLASS(core, CorePkg, ComplexVector_O, "ComplexVector", MDArray_O);
  // One dimension vector
public:
  ComplexVector_O(size_t dimension, T_sp fillPointer, Array_sp data, bool displacedToP, Fixnum_sp displacedIndexOffset)
      : MDArray_O(Rank1(), dimension, fillPointer, data, displacedToP, displacedIndexOffset){};

public:
  virtual void __write__(T_sp strm) const override;
};
}; // namespace core

namespace core {
class SimpleMDArray_O : public MDArray_O {
  LISP_ABSTRACT_CLASS(core, CorePkg, SimpleMDArray_O, "simple-mdarray", MDArray_O);

public:
  // multiple dimensions
  SimpleMDArray_O(size_t rank, List_sp dimensions, Array_sp data)
      : MDArray_O(rank, dimensions, data, false, clasp_make_fixnum(0)){};

public:
  virtual T_sp array_type() const override { return cl::_sym_simple_array; };
  virtual bool adjustableArrayP() const override { return false; };
  virtual bool fillPointerP() const { return false; };
  virtual bool displacedToP() const override { return false; };
};
}; // namespace core

// ----------------------------------------------------------------------
//
// here go Simple vectors
//
namespace core {

class AbstractSimpleVector_O : public Array_O {
  LISP_ABSTRACT_CLASS(core, CorePkg, AbstractSimpleVector_O, "AbstractSimpleVector", Array_O);

public:
  virtual T_sp array_type() const override { return cl::_sym_simple_array; };

public:
  virtual Array_sp data() { return gc::As_unsafe<Array_sp>(this->asSmartPtr()); };
  virtual size_t arrayTotalSize() const override { return this->length(); };
  virtual void rowMajorAset(size_t idx, T_sp value) override = 0;
  virtual T_sp rowMajorAref(size_t idx) const override = 0;
  virtual void vset(size_t idx, T_sp value) = 0;
  virtual T_sp vref(size_t idx) const = 0;
  virtual size_t rank() const override { return 1; };
  virtual bool adjustableArrayP() const final { return false; };
  virtual size_t displacedIndexOffset() const override { return 0; };
  virtual size_t arrayDimension(size_t axisNumber) const override {
    unlikely_if(axisNumber != 0) { badAxisNumberError(cl::_sym_arrayDimension, 1, axisNumber); }
    return this->length();
  }
  virtual T_sp vectorPush(T_sp newElement) override { noFillPointerError(cl::_sym_vectorPush, this->asSmartPtr()); };
  virtual Fixnum_sp vectorPushExtend(T_sp newElement, size_t extension) {
    noFillPointerError(cl::_sym_vectorPushExtend, this->asSmartPtr());
  };
  virtual T_sp replaceArray(T_sp other) override { notAdjustableError(core::_sym_replaceArray, this->asSmartPtr()); };
  virtual std::string get_std_string() const override { notStringError(this->asSmartPtr()); };
  virtual std::string get_path_string() const override { notStringError(this->asSmartPtr()); };
  virtual void ranged_sxhash(HashGenerator& hg, size_t start, size_t end) const {
    TYPE_ERROR(this->asSmartPtr(), Cons_O::createList(cl::_sym_string, cl::_sym_bit_vector));
  };
  virtual bool equal(T_sp other) const override { return this->eq(other); };
  virtual bool equalp(T_sp other) const override;
  void asAbstractSimpleVectorRange(AbstractSimpleVector_sp& sv, size_t& start, size_t& end) const override {
    sv = this->asSmartPtr();
    start = 0;
    end = this->length();
  }
  virtual vector<size_t> arrayDimensionsAsVector() const override {
    return vector<size_t>(1, length());
  }
};
}; // namespace core

namespace core {
template <typename MyLeafType, typename ValueType, typename MyParentType> class template_SimpleVector : public MyParentType {
public:
  // The types that define what this class does
  typedef MyParentType Base;
  typedef MyLeafType /* eg: SimpleVector_O*/ leaf_type;
  typedef ValueType /*eg: T_sp*/ value_type;
  typedef ValueType /*eg: T_sp*/ simple_element_type;
  typedef gctools::smart_ptr<leaf_type> leaf_smart_ptr_type;
  typedef gctools::GCArray_moveable<value_type> vector_type;
  typedef typename vector_type::iterator iterator;
  typedef typename vector_type::const_iterator const_iterator;
  /* These two are necessary because of bit unit vectors, see below */
  typedef value_type& reference_type;
  typedef const value_type& const_reference_type;

public:
  vector_type _Data;

public:
  template_SimpleVector(size_t length, value_type initialElement = value_type(), bool initialElementSupplied = false,
                        size_t initialContentsSize = 0, const value_type* initialContents = NULL)
      : Base(), _Data(length, initialElement, initialElementSupplied, initialContentsSize, initialContents){};
  template_SimpleVector(size_t length, const value_type& initialElement)
    : Base(), _Data(length, initialElement) {};
  template <std::ranges::sized_range R>
  template_SimpleVector(R&& initialContents)
    : Base(), _Data(initialContents) {};
  template <std::input_iterator I, std::sized_sentinel_for<I> S>
  template_SimpleVector(I first, S last)
    : Base(), _Data(first, last) {};

public:
  leaf_smart_ptr_type copy(size_t length, value_type initialElement, bool initialElementSupplied) {
    return leaf_type::make(length, initialElement, initialElementSupplied, MIN(length, this->length()), this->begin());
  }

public:
  // NULL terminated strings use this - so the ASSERT needs to accept it
  reference_type operator[](size_t index) {
    BOUNDS_ASSERT_LT(index, this->length());
    return this->_Data[index];
  };
  const_reference_type operator[](size_t index) const {
    BOUNDS_ASSERT_LT(index, this->length());
    return this->_Data[index];
  };
  iterator begin() { return _Data.begin(); }
  iterator end() { return _Data.end(); }
  const_iterator begin() const { return _Data.begin(); }
  const_iterator end() const { return _Data.end(); }
  virtual size_t elementSizeInBytes() const override { return sizeof(value_type); };
  virtual void* rowMajorAddressOfElement_(size_t i) const override { return (void*)&(this->_Data[i]); };
  virtual void unsafe_fillArrayWithElt(T_sp initialElement, size_t start, size_t end) override {
    std::fill(begin() + start, begin() + end,
              leaf_type::from_object(initialElement));
  };
  virtual Array_sp reverse() const final {
    auto result = leaf_type::make(this->length());
    std::ranges::reverse_copy(*this, result.begin());
    return result;
  };
  virtual Array_sp nreverse() final {
    std::ranges::reverse(*this);
    return this->asSmartPtr();
  }
  CL_METHOD_OVERLOAD virtual void rowMajorAset(size_t idx, T_sp value) final { (*this)[idx] = leaf_type::from_object(value); }
  CL_METHOD_OVERLOAD virtual T_sp rowMajorAref(size_t idx) const final { return leaf_type::to_object((*this)[idx]); }
  CL_METHOD_OVERLOAD virtual void vset(size_t idx, T_sp value) final { (*this)[idx] = leaf_type::from_object(value); }
  CL_METHOD_OVERLOAD virtual T_sp vref(size_t idx) const final { return leaf_type::to_object((*this)[idx]); }
  virtual Array_sp unsafe_subseq(size_t start, size_t end) const final {
    BOUNDS_ASSERT(start <= end && end <= this->length());
    return leaf_type::make(end - start, value_type(), true, end - start, (value_type*)this->rowMajorAddressOfElement_(start));
  }

  // copy_n is the base function for copying data between arrays.
  // If you pass it a source array of known leaf type it will copy without
  // dispatching, and when possible without boxing.
  // If you pass it an array of generic type it will do a lot of virtual dispatch
  // and will box, except that the case of the underlying element type being
  // identical between source and dest is dispatched on to not box.
  template <typename SourceArray_O>
  void copy_n(gctools::smart_ptr<SourceArray_O> source,
              size_t dest_start, size_t source_start, size_t n);

  // Best case: copying from an array with the same element type.
  // This does raise the possibility of aliasing though, so watch out for that.
  template <>
  void copy_n(leaf_smart_ptr_type source,
              size_t dest_start, size_t source_start, size_t n) {
    if (dest_start <= source_start) {
      std::copy(source->begin() + source_start,
                source->begin() + source_start + n,
                begin() + dest_start);
    } else {
      std::copy_backward(source->begin() + source_start,
                         source->begin() + source_start + n,
                         begin() + dest_start + n);
    }
  }
  // Worst specific case: box and unbox,
  // but we can at least avoid virtual rowMajorAref.
  // If the element types are integral we try to copy them without boxing,
  // but this may need type checks for every element if the destination's
  // integers are smaller than the source's.
  // We'd hit the specific expansion if SourceArray_O = leaf_type,
  // so we don't need to worry about aliasing.
  using Base::element_type;
  template <typename SourceArray_O>
  requires (std::derived_from<SourceArray_O, AbstractSimpleVector_O>
            && !std::same_as<SourceArray_O, leaf_type>
            // needs its own specialization since we need virtual rowMajorAref
            && !std::same_as<SourceArray_O, AbstractSimpleVector_O>)
    void copy_n(gctools::smart_ptr<SourceArray_O> source,
                size_t dest_start, size_t source_start, size_t n) {
    if constexpr(std::is_integral_v<typename leaf_type::simple_element_type>
                 && std::is_integral_v<typename SourceArray_O::simple_element_type>
                 // make sure not to copy strings into bytevectors or vice versa
                 && ((std::is_base_of_v<SimpleString_O, leaf_type>
                      && std::is_base_of_v<SimpleString_O, SourceArray_O>)
                     || (!std::is_base_of_v<SimpleString_O, leaf_type>
                         && !std::is_base_of_v<SimpleString_O, SourceArray_O>))) {
      // Specialish case so we can copy integers without boxing.
      // if constexpr is because constraint normalization is kind of dumb,
      // making it difficult to keep the templates unambiguous.
      // actual copy loop
      for (size_t i = 0; i < n; ++i) {
        auto elt = (*source)[source_start + i];
        // clang can probably optimize this test out when the source type
        // always fits
        if (elt < std::numeric_limits<typename leaf_type::simple_element_type>::min()
            || elt > std::numeric_limits<typename leaf_type::simple_element_type>::max()) {
          if constexpr(std::is_base_of_v<SimpleString_O, leaf_type>)
            // very special case to get an actual lisp character into the
            // type error. (plus there's no Integer_O::create for claspCharacter
            // as i write this, since it's char32_t)
            TYPE_ERROR(clasp_make_character(elt), element_type());
          else
            TYPE_ERROR(Integer_O::create(elt), element_type());
        } else (*this)[dest_start + i] = elt;
      }
    } else {
      // note: no std::transform since if the unary_op throws, the program is
      // std::terminate'd. I think.
      for (size_t i = 0; i < n; ++i) {
        T_sp elt = SourceArray_O::to_object((*source)[source_start + i]);
        (*this)[dest_start + i] = leaf_type::from_object(elt);
      }
    }
  }
  // default specialization when we know source is simple but nothing else
  template <>
  void copy_n(AbstractSimpleVector_sp source,
              size_t dest_start, size_t source_start, size_t n) {
    // Dynamic dispatch the common self case for speed, and also to avoid
    // any aliasing problems.
    if (source.isA<leaf_type>())
      // KLUDGE: apparently the generic smart_ptr template does not define
      // as_unsafe, so source.as_unsafe gets you a base_ptr, which does not
      // fit the templates. WTF?
      copy_n(gc::As_unsafe<leaf_smart_ptr_type>(source), dest_start, source_start, n);
    else
      for (size_t i = 0; i < n; ++i)
        // virtual dispatch is required unless we want to hardcode
        // a test for each type of array, which no
        (*this)[dest_start + i] = leaf_type::from_object(source->rowMajorAref(source_start + i));
  }
  // For MDArrays, get the underlying simple array of the source
  // and copy from that.
  // This also covers MDArray_O itself with simple_type = AbstractSimpleVector_O
  template <typename SourceArray_O>
  requires std::derived_from<SourceArray_O, MDArray_O>
  void copy_n(gctools::smart_ptr<SourceArray_O> source,
              size_t dest_start, size_t source_start, size_t n) {
    AbstractSimpleVector_sp sv;
    size_t sstart, send;
    source->asAbstractSimpleVectorRange(sv, sstart, send);
    gctools::smart_ptr<typename SourceArray_O::simple_type> rsv = sv.as_unsafe<typename SourceArray_O::simple_type>();
    copy_n(rsv, dest_start, source_start + sstart, n);
  }
  // default specialization when source array type is not known at all
  template <>
  void copy_n(Array_sp source, size_t dest_start, size_t source_start, size_t n) {
    AbstractSimpleVector_sp sv;
    size_t sstart, send;
    source->asAbstractSimpleVectorRange(sv, sstart, send);
    copy_n(sv, dest_start, source_start + sstart, n);
  }

  void copy_nd(Array_sp source,
               size_t dest_start, size_t source_start, size_t n) final {
    copy_n(source, dest_start, source_start, n);
  }
};
}; // namespace core

// FIXME: Iterators are probably very broken for complex bit unit arrays.
namespace core {
template <typename MyLeafType, int BitUnitBitWidth, int Signedp>
class template_SimpleBitUnitVector : public AbstractSimpleVector_O {

public:
  typedef AbstractSimpleVector_O Base;
  typedef MyLeafType leaf_type;
  typedef gctools::smart_ptr<leaf_type> leaf_smart_ptr_type;
  typedef gctools::GCBitUnitArray_moveable<BitUnitBitWidth, Signedp> bitunit_array_type;
  typedef typename bitunit_array_type::value_type value_type;
  typedef value_type simple_element_type;
  /* See GCBitUnitArray_moveable - short version is, we don't have pointers into
   * sub-byte arrays for obvious reasons, so we use proxies. */
  typedef typename bitunit_array_type::reference reference_type;
  typedef value_type const_reference_type;
  typedef typename bitunit_array_type::iterator iterator;
  typedef typename bitunit_array_type::const_iterator const_iterator;
  static_assert(std::random_access_iterator<iterator>);
  static_assert(std::random_access_iterator<const_iterator>);
  static_assert(std::indirectly_copyable<const_iterator, iterator>);
public:
  // E.g., for three bits, we range from -4 to 3 signed, or 0 to 7 unsigned.
  static const value_type min_value = Signedp ? -(1 << (BitUnitBitWidth - 1)) : 0;
  static const value_type max_value = (1 << (BitUnitBitWidth - (Signedp ? 1 : 0))) - 1;

public:
  bitunit_array_type _Data;
  template_SimpleBitUnitVector(size_t length, bit_array_word initialElement, bool initialElementSupplied,
                               size_t initialContentsSize = 0, const bit_array_word* initialContents = NULL)
      : Base(), _Data(length, initialElement, initialElementSupplied, initialContentsSize, initialContents){};

public:
  leaf_smart_ptr_type copy(size_t length, value_type initialElement, bool initialElementSupplied) {
    bit_array_word init = initialFillValue(initialElement);
    return leaf_type::make(length, init, initialElementSupplied, MIN(bitunit_array_type::nwords_for_length(length), byteslen()),
                           bytes());
  }

public:
  reference_type operator[](size_t index) {
    BOUNDS_ASSERT_LT(index, this->length());
    return this->_Data.ref(index);
  }
  const_reference_type operator[](size_t index) const {
    BOUNDS_ASSERT_LT(index, this->length());
    return this->_Data.ref(index);
  }
  iterator begin() { return this->_Data.begin(); }
  iterator end() { return this->_Data.end(); }
  const_iterator begin() const { return this->_Data.begin(); }
  const_iterator end() const { return this->_Data.end(); }
  bit_array_word* bytes() { return &this->_Data[0]; }
  size_t byteslen() { return bitunit_array_type::nwords_for_length(this->length()); }
  // Given an initial element, replicate it into a bit_array_word. E.g. 01 becomes 01010101...01
  static bit_array_word initialFillValue(value_type initialValue) { return bitunit_array_type::initialFillValue(initialValue); }
  // Since we know the element type, we can define these here instead of in the child.
  static value_type from_object(T_sp object) {
    if (object.fixnump()) {
      Fixnum i = object.unsafe_fixnum();
      if ((min_value <= i) && (i <= max_value))
        return i;
    }
    TYPE_ERROR(object, leaf_type::static_element_type());
  }
  static T_sp to_object(value_type v) { return Integer_O::create(v); }
  virtual void unsafe_fillArrayWithElt(T_sp initialElement, size_t start, size_t end) override {
    // FIXME: Could be done more efficiently by writing whole words, but be careful about the ends.
    std::fill(begin() + start, begin() + end, from_object(initialElement));
  }
  virtual size_t elementSizeInBytes() const override { bitVectorDoesntSupportError(); }
  virtual void* rowMajorAddressOfElement_(size_t i) const override { bitVectorDoesntSupportError(); }
  static value_type default_initial_element(void) { return 0; }
  virtual T_sp element_type() const override final { return leaf_type::static_element_type(); }
  virtual Array_sp reverse() const final {
    auto result = leaf_type::make(this->length());
    std::ranges::reverse_copy(*this, result.begin());
    return result;
  }
  virtual Array_sp nreverse() final {
    std::ranges::reverse(*this);
    return this->asSmartPtr();
  }
  CL_METHOD_OVERLOAD virtual void rowMajorAset(size_t idx, T_sp value) final { (*this)[idx] = from_object(value); }
  CL_METHOD_OVERLOAD virtual T_sp rowMajorAref(size_t idx) const final { return to_object((*this)[idx]); }
  CL_METHOD_OVERLOAD virtual void vset(size_t idx, T_sp value) final { (*this)[idx] = from_object(value); }
  CL_METHOD_OVERLOAD virtual T_sp vref(size_t idx) const final { return to_object((*this)[idx]); }
  Array_sp unsafe_subseq(size_t start, size_t end) const override {
    BOUNDS_ASSERT(0 <= start && start < end && end <= this->length());
    leaf_smart_ptr_type sbv = leaf_type::make(end - start);
    std::copy(begin() + start, begin() + end, sbv->begin());
    return sbv;
  }

  // see definitions in template_SimpleVector::copy_n for more comments
  template <typename SourceArray_O>
  void copy_n(gctools::smart_ptr<SourceArray_O> source,
              size_t dest_start, size_t source_start, size_t n);

  template <>
  void copy_n(leaf_smart_ptr_type source,
              size_t dest_start, size_t source_start, size_t n) {
    // TODO: Could be done much faster without boxing by copying whole bit
    // array words and masking appropriately.
    if (dest_start <= source_start) {
      std::copy(source->begin() + source_start,
                source->begin() + source_start + n,
                begin() + dest_start);
    } else {
      std::copy_backward(source->begin() + source_start,
                         source->begin() + source_start + n,
                         begin() + dest_start + n);
    }
  }
  using Base::element_type;
  template <typename SourceArray_O>
  requires (std::derived_from<SourceArray_O, AbstractSimpleVector_O>
            && !std::same_as<SourceArray_O, leaf_type>
            && !std::same_as<SourceArray_O, AbstractSimpleVector_O>)
  void copy_n(gctools::smart_ptr<SourceArray_O> source,
              size_t dest_start, size_t source_start, size_t n) {
    if constexpr(std::is_integral_v<typename SourceArray_O::simple_element_type>
                 && !std::is_base_of_v<SimpleString_O, SourceArray_O>) {
      static_assert(std::is_integral_v<typename leaf_type::simple_element_type>);
      for (size_t i = 0; i < n; ++i) {
        auto elt = (*source)[source_start + i];
        if (elt < std::numeric_limits<typename leaf_type::simple_element_type>::min()
            || elt > std::numeric_limits<typename leaf_type::simple_element_type>::max())
          TYPE_ERROR(Integer_O::create(elt), element_type());
        else (*this)[dest_start + i] = elt;
      }
    } else {
      for (size_t i = 0; i < n; ++i) {
        T_sp elt = SourceArray_O::to_object((*source)[source_start + i]);
        (*this)[dest_start + i] = leaf_type::from_object(elt);
      }
    }
  }
  template <>
  void copy_n(AbstractSimpleVector_sp source,
              size_t dest_start, size_t source_start, size_t n) {
    if (source.isA<leaf_type>())
      copy_n(gc::As_unsafe<leaf_smart_ptr_type>(source), dest_start, source_start, n);
    else
      for (size_t i = 0; i < n; ++i)
        (*this)[dest_start + i] = leaf_type::from_object(source->rowMajorAref(source_start + i));
  }
  template <typename SourceArray_O>
  requires std::derived_from<SourceArray_O, MDArray_O>
  void copy_n(gctools::smart_ptr<SourceArray_O> source,
              size_t dest_start, size_t source_start, size_t n) {
    AbstractSimpleVector_sp sv;
    size_t sstart, send;
    source->asAbstractSimpleVectorRange(sv, sstart, send);
    gctools::smart_ptr<typename SourceArray_O::simple_type> rsv = sv.as_unsafe<typename SourceArray_O::simple_type>();
    copy_n(rsv, dest_start, source_start + sstart, n);
  }
  template <>
  void copy_n(Array_sp source, size_t dest_start, size_t source_start, size_t n) {
    AbstractSimpleVector_sp sv;
    size_t sstart, send;
    source->asAbstractSimpleVectorRange(sv, sstart, send);
    copy_n(sv, dest_start, source_start + sstart, n);
  }

  void copy_nd(Array_sp source, size_t dest_start, size_t source_start, size_t n) final {
    copy_n(source, dest_start, source_start, n);
  }
};
}; // namespace core

namespace core {
Vector_sp core__make_vector(T_sp element_type, size_t dimension, bool adjustable = false, T_sp fill_pointer = nil<T_O>(),
                            T_sp displaced_to = nil<T_O>(), Fixnum_sp displacedIndexOffset = clasp_make_fixnum(0),
                            T_sp initial_element = nil<T_O>(), bool initial_element_supplied_p = false);

Vector_sp core__make_static_vector(T_sp element_type, size_t dimension, T_sp initial_element = nil<T_O>(),
                                   bool initial_element_supplied_p = false);

MDArray_sp core__make_mdarray(List_sp dimensions, T_sp element_type, bool adjustable = false, T_sp displacedTo = nil<T_O>(),
                              Fixnum_sp displacedIndexOffset = clasp_make_fixnum(0), T_sp initialElement = nil<T_O>(),
                              bool initialElementSuppliedP = false);

}; // namespace core

namespace core {
template <typename MyArrayType, typename MySimpleArrayType, typename MySimpleType, typename MyParentType>
class template_Array : public MyParentType {
public:
  // The types that define what this class does
  typedef MyParentType Base; /* e.g. MDArray_O */
  typedef MyArrayType /*eg: MDArrayT_O */ my_array_type;
  typedef MySimpleArrayType /*eg: SimpleMDArrayT_O */ my_simple_array_type;
  typedef MySimpleType /*eg: SimpleVector_O */ simple_type;
  typedef typename simple_type::simple_element_type /*eg: T_sp*/ simple_element_type;
  typedef typename simple_type::reference_type /* e.g. T_sp& */ reference_type;
  typedef typename simple_type::const_reference_type /* e.g. const T_sp & */ const_reference_type;
  typedef typename simple_type::iterator iterator;
  typedef typename simple_type::const_iterator const_iterator;
  typedef gctools::smart_ptr<my_array_type> my_smart_ptr_type;
  typedef gctools::smart_ptr<my_simple_array_type> my_simple_smart_ptr_type;
  typedef gctools::GCArray_moveable<simple_element_type> simple_vector_type;
  typedef typename MDArray_O::value_type dimension_element_type;

public:
  // multidimensional array
  template_Array(size_t rank, List_sp dimensions, Array_sp data, bool displacedToP, Fixnum_sp displacedIndexOffset)
      : Base(rank, dimensions, data, displacedToP, displacedIndexOffset){};

public: // make array
  static my_smart_ptr_type make_multi_dimensional(List_sp dim_desig, simple_element_type initialElement, T_sp dataOrDisplacedTo,
                                                  bool displacedToP, Fixnum_sp displacedIndexOffset) {
    ASSERT(dim_desig.consp() || dim_desig.nilp());
    size_t rank;
    size_t arrayTotalSize = calculateArrayTotalSizeAndValidateDimensions(dim_desig, rank);
    LIKELY_if(dataOrDisplacedTo.nilp()) dataOrDisplacedTo = simple_type::make(arrayTotalSize, initialElement, true);
    return gctools::GC<my_array_type>::template allocate_container<gctools::RuntimeStage>(
        false, rank, dim_desig, gc::As<Array_sp>(dataOrDisplacedTo), displacedToP, displacedIndexOffset);
  }

public:
  // Iterators
  iterator begin() {
    Array_sp data = this->_Data;
    size_t offset = this->_DisplacedIndexOffset;
    while (!data.isA<simple_type>()) [[unlikely]] {
      // if it's not simple, it's either a complex vector, a simple mdarray, or an
      // mdarray, and all of those are subclasses of mdarray.
      offset += data.as_unsafe<Base>()->_DisplacedIndexOffset;
      data = data.as_unsafe<Base>()->_Data;
    }
    return data.as_unsafe<simple_type>().begin() + offset;
  };
  iterator end() { return begin() + this->arrayTotalSize(); };
  const_iterator begin() const {
    Array_sp data = this->_Data;
    size_t offset = this->_DisplacedIndexOffset;
    while (!data.isA<simple_type>()) [[unlikely]] {
      offset += data.as_unsafe<Base>()->_DisplacedIndexOffset;
      data = data.as_unsafe<Base>()->_Data;
    }
    const gc::smart_ptr<simple_type> s = data.as_unsafe<simple_type>();
    return s.begin() + offset;
  }
  const_iterator end() const { return begin() + this->arrayTotalSize(); };
  reference_type operator[](size_t index) {
    BOUNDS_ASSERT(index < this->arrayTotalSize());
    return this->begin()[index];
  }
  const_reference_type operator[](size_t index) const {
    BOUNDS_ASSERT(index < this->arrayTotalSize());
    return this->begin()[index];
  }

public:
  void asAbstractSimpleVectorRange(AbstractSimpleVector_sp& sv, size_t& start, size_t& end) const final {
    if (this->_Data.template isA<simple_type>()) [[likely]] {
      sv = gc::As_unsafe<gctools::smart_ptr<simple_type>>(this->_Data);
      start = this->_DisplacedIndexOffset;
      end = start + this->arrayTotalSize();
    }
    else {
      this->_Data->asAbstractSimpleVectorRange(sv, start, end);
      start += this->_DisplacedIndexOffset;
      end = start + this->arrayTotalSize();
    }
  }
  virtual Array_sp reverse() const final {
    auto result = simple_type::make(this->length());
    std::ranges::reverse_copy(*this, result.begin());
    return result;
  }
  virtual Array_sp nreverse() final {
    std::ranges::reverse(*this);
    return this->asSmartPtr();
  };
  virtual void resize(size_t size, T_sp initElement = nil<T_O>(), bool initElementSupplied = false) final {
    if (size == this->_ArrayTotalSize)
      return;
    AbstractSimpleVector_sp basesv;
    size_t start, end;
    this->asAbstractSimpleVectorRange(basesv, start, end);
    gctools::smart_ptr<simple_type> sv = gc::As_unsafe<gctools::smart_ptr<simple_type>>(basesv);
    gctools::smart_ptr<simple_type> newData =
        sv->copy(size, initElementSupplied ? simple_type::from_object(initElement) : simple_type::default_initial_element(),
                 initElementSupplied);
    this->set_data(newData);
    //      printf("%s:%d:%s  original size=%lu new size=%lu  copied %lu elements\n", __FILE__, __LINE__, __FUNCTION__,
    //      this->_ArrayTotalSize, size, initialContentsSize );
    this->_ArrayTotalSize = size;
    this->_Dimensions[0] = size;
    if (!this->_Flags.fillPointerP())
      this->_FillPointerOrLengthOrDummy = size;
    this->_DisplacedIndexOffset = 0;
    this->_Flags.set_displacedToP(false);
  }
  CL_METHOD_OVERLOAD virtual void rowMajorAset(size_t idx, T_sp value) final { (*this)[idx] = simple_type::from_object(value); }
  CL_METHOD_OVERLOAD virtual T_sp rowMajorAref(size_t idx) const final { return simple_type::to_object((*this)[idx]); }
  bool equal(T_sp obj) const override { return this->eq(obj); };

  template <typename SourceArray_O>
  void copy_n(gctools::smart_ptr<SourceArray_O> source,
              size_t dest_start, size_t source_start, size_t n) {
    AbstractSimpleVector_sp dest;
    size_t dstart, dend;
    asAbstractSimpleVectorRange(dest, dstart, dend);
    dest.as_unsafe<simple_type>()->copy_n(source, dest_start + dstart,
                                          source_start, n);
  }

  void copy_nd(Array_sp source, size_t dest_start, size_t source_start, size_t n) final {
    copy_n(source, dest_start, source_start, n);
  }
};
}; // namespace core

namespace core {
template <typename MyArrayType, typename MySimpleType, typename MyParentType> class template_Vector : public MyParentType {
public:
  // The types that define what this class does
  typedef MyParentType Base;
  typedef MyArrayType /*eg: ComplexVector_T_O*/ my_array_type;
  typedef MySimpleType /*eg: SimpleVector_O*/ simple_type;
  typedef typename simple_type::simple_element_type /*eg: T_sp*/ simple_element_type;
  typedef typename simple_type::reference_type /* e.g. T_sp& */ reference_type;
  typedef typename simple_type::const_reference_type /* e.g. const T_sp & */ const_reference_type;
  typedef typename simple_type::iterator iterator;
  typedef typename simple_type::const_iterator const_iterator;
  typedef gctools::smart_ptr<my_array_type> my_smart_ptr_type;
  typedef gctools::GCArray_moveable<simple_element_type> simple_vector_type;
  typedef typename MDArray_O::value_type dimension_element_type;
  static_assert(std::random_access_iterator<iterator>);
  static_assert(std::random_access_iterator<const_iterator>);

public:
  // vector
  template_Vector(size_t dimension, T_sp fillPointer, Array_sp data, bool displacedToP, Fixnum_sp displacedIndexOffset)
      : Base(dimension, fillPointer, data, displacedToP, displacedIndexOffset){};

public:
  // Iterators
  iterator begin() {
    Array_sp data = this->_Data;
    size_t offset = this->_DisplacedIndexOffset;
    while (!data.isA<simple_type>()) [[unlikely]] {
      // if it's not simple, it's either a complex vector, a simple mdarray, or an
      // mdarray, and all of those are subclasses of mdarray.
      offset += data.as_unsafe<Base>()->_DisplacedIndexOffset;
      data = data.as_unsafe<Base>()->_Data;
    }
    return data.as_unsafe<simple_type>().begin() + offset;
  };
  iterator end() { return begin() + this->length(); };
  const_iterator begin() const {
    Array_sp data = this->_Data;
    size_t offset = this->_DisplacedIndexOffset;
    while (!data.isA<simple_type>()) [[unlikely]] {
      offset += data.as_unsafe<Base>()->_DisplacedIndexOffset;
      data = data.as_unsafe<Base>()->_Data;
    }
    const gc::smart_ptr<simple_type> s = data.as_unsafe<simple_type>();
    return s.begin() + offset;
  };
  const_iterator end() const { return begin() + this->length(); };
  reference_type operator[](size_t index) {
    BOUNDS_ASSERT(index < this->arrayTotalSize());
    return this->begin()[index];
  }
  const_reference_type operator[](size_t index) const {
    BOUNDS_ASSERT(index < this->arrayTotalSize());
    return this->begin()[index];
  }

public:
  void asAbstractSimpleVectorRange(AbstractSimpleVector_sp& sv, size_t& start, size_t& end) const final {
    LIKELY_if(gc::IsA<gc::smart_ptr<simple_type>>(this->_Data)) {
      sv = gc::As<AbstractSimpleVector_sp>(this->_Data);
      start = this->_DisplacedIndexOffset;
      end = start + this->length();
    }
    else {
      this->_Data->asAbstractSimpleVectorRange(sv, start, end);
      start += this->_DisplacedIndexOffset;
      end = start + this->length();
      return;
    }
  }
  virtual Array_sp reverse() const final {
    auto result = simple_type::make(this->length());
    std::ranges::reverse_copy(*this, result.begin());
    return result;
  }
  virtual Array_sp nreverse() final {
    std::ranges::reverse(*this);
    return this->asSmartPtr();
  };
  virtual void resize(size_t size, T_sp initElement = nil<T_O>(), bool initElementSupplied = false) final {
    if (size == this->_ArrayTotalSize)
      return;
    AbstractSimpleVector_sp basesv;
    size_t start, end;
    this->asAbstractSimpleVectorRange(basesv, start, end);
    gctools::smart_ptr<simple_type> sv = gc::As_unsafe<gctools::smart_ptr<simple_type>>(basesv);
    gctools::smart_ptr<simple_type> newData =
        sv->copy(size, initElementSupplied ? simple_type::from_object(initElement) : simple_type::default_initial_element(),
                 initElementSupplied);
    this->set_data(newData);
    //      printf("%s:%d:%s  original size=%lu new size=%lu  copied %lu elements\n", __FILE__, __LINE__, __FUNCTION__,
    //      this->_ArrayTotalSize, size, initialContentsSize );
    this->_ArrayTotalSize = size;
    this->_Dimensions[0] = size;
    if (!this->_Flags.fillPointerP())
      this->_FillPointerOrLengthOrDummy = size;
    this->_DisplacedIndexOffset = 0;
    this->_Flags.set_displacedToP(false);
  }
  CL_METHOD_OVERLOAD virtual void rowMajorAset(size_t idx, T_sp value) final { (*this)[idx] = simple_type::from_object(value); }
  CL_METHOD_OVERLOAD virtual T_sp rowMajorAref(size_t idx) const final { return simple_type::to_object((*this)[idx]); }
  bool equal(T_sp obj) const override { return this->eq(obj); };
  // NOTE: For only ComplexVector_T_O this will override the ComplexVector_O function,
  // since simple_element_type = T_sp, but that's harmless since they have the same effect.
  // FIXME: This will be mildly inefficient for bit vectors, as the array total size is not
  // always the number of elements for which space is allocated. Probably add a capacity()
  // non virtual function or something, to get the actual size.
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Winconsistent-missing-override"
  virtual Fixnum_sp vectorPushExtend(simple_element_type newElement, size_t extension = 0) {
    unlikely_if(!this->_Flags.fillPointerP()) noFillPointerSpecializedArrayError(this->asSmartPtr());
    cl_index idx = this->_FillPointerOrLengthOrDummy;
    unlikely_if(idx >= this->_ArrayTotalSize) {
      if (extension <= 0)
        extension = calculate_extension(this->_ArrayTotalSize);
      cl_index new_size = this->_ArrayTotalSize + extension;
      unlikely_if(!cl::_sym_adjust_array || !lisp_boundp(cl::_sym_adjust_array)) { this->resize(new_size); }
      else {
        lisp_adjust_array(this->asSmartPtr(), clasp_make_fixnum(new_size), clasp_make_fixnum(this->_FillPointerOrLengthOrDummy));
      }
    }
    (*this)[idx] = newElement;
    ++this->_FillPointerOrLengthOrDummy;
    return make_fixnum(idx);
  }
#pragma clang diagnostic pop
  template <typename SourceArray_O>
  void copy_n(gctools::smart_ptr<SourceArray_O> source,
              size_t dest_start, size_t source_start, size_t n) {
    AbstractSimpleVector_sp dest;
    size_t dstart, dend;
    asAbstractSimpleVectorRange(dest, dstart, dend);
    dest.as_unsafe<simple_type>()->copy_n(source, dest_start + dstart,
                                          source_start, n);
  }
  void copy_nd(Array_sp source,
               size_t dest_start, size_t source_start, size_t n) final {
    copy_n(source, dest_start, source_start, n);
  }
};
}; // namespace core

namespace core {
template <typename MyArrayType, typename MySimpleType, typename MyParentType> class template_SimpleArray : public MyParentType {
public:
  // The types that define what this class does
  typedef MyParentType Base;
  typedef MyArrayType /*eg: ComplexVector_T_O*/ my_array_type;
  typedef MySimpleType /*eg: SimpleVector_O*/ simple_type;
  typedef typename simple_type::simple_element_type /*eg: T_sp*/ simple_element_type;
  typedef typename simple_type::reference_type /* e.g. T_sp& */ reference_type;
  typedef typename simple_type::const_reference_type /* e.g. const T_sp & */ const_reference_type;
  typedef typename simple_type::iterator iterator;
  typedef typename simple_type::const_iterator const_iterator;
  typedef gctools::smart_ptr<my_array_type> my_smart_ptr_type;
  typedef gctools::GCArray_moveable<simple_element_type> simple_vector_type;
  typedef typename MDArray_O::value_type dimension_element_type;

public:
  // vector
  template_SimpleArray(size_t dimension, Array_sp data) : Base(dimension, data){};
  template_SimpleArray(size_t rank, List_sp dimensions, Array_sp data) : Base(rank, dimensions, data){};

public: // make array
  static my_smart_ptr_type make_multi_dimensional(List_sp dim_desig, simple_element_type initialElement, T_sp data) {
    ASSERT(dim_desig.consp() || dim_desig.nilp());
    size_t rank;
    size_t arrayTotalSize = calculateArrayTotalSizeAndValidateDimensions(dim_desig, rank);
    LIKELY_if(data.nilp()) data = simple_type::make(arrayTotalSize, initialElement, true);
    return gctools::GC<my_array_type>::template allocate_container<gctools::RuntimeStage>(false, rank, dim_desig,
                                                                                          gc::As<Array_sp>(data));
  }

public:
  // Iterators
  iterator begin() { return this->_Data.template as_assert<simple_type>().begin(); }
  iterator end() { return begin() + this->arrayTotalSize(); };
  const_iterator begin() const {
    // FIXME: const overloading for as_assert would make this easier
    // but require const correctness in a lot of other places.
    const auto dat = this->_Data.template as_assert<simple_type>();
    return dat.begin();
  }
  const_iterator end() const { return begin() + this->arrayTotalSize(); };
  reference_type operator[](size_t index) {
    BOUNDS_ASSERT(index < this->arrayTotalSize());
    return this->begin()[index];
  }
  const_reference_type operator[](size_t index) const {
    BOUNDS_ASSERT(index < this->arrayTotalSize());
    return this->begin()[index];
  }

public:
  virtual Array_sp reverse() const final {
    auto result = simple_type::make(this->length());
    std::ranges::reverse_copy(*this, result.begin());
    return result;
  }
  virtual Array_sp nreverse() final {
    std::ranges::reverse(*this);
    return this->asSmartPtr();
  };
  virtual void resize(size_t size, T_sp initElement = nil<T_O>(), bool initElementSupplied = false) final {
    cannotAdjustSizeOfSimpleArrays(this->asSmartPtr());
  };

public:
  void asAbstractSimpleVectorRange(AbstractSimpleVector_sp& sv, size_t& start, size_t& end) const final {
    ASSERT(gc::IsA<AbstractSimpleVector_sp>(this->_Data));
    sv = gc::As_unsafe<AbstractSimpleVector_sp>(this->_Data);
    start = this->_DisplacedIndexOffset;
    end = this->length() + this->_DisplacedIndexOffset;
  }
  CL_METHOD_OVERLOAD virtual void rowMajorAset(size_t idx, T_sp value) final { (*this)[idx] = simple_type::from_object(value); }
  CL_METHOD_OVERLOAD virtual T_sp rowMajorAref(size_t idx) const final { return simple_type::to_object((*this)[idx]); }
  bool equal(T_sp obj) const override { return this->eq(obj); };

  template <typename SourceArray_O>
  void copy_n(gctools::smart_ptr<SourceArray_O> source,
              size_t dest_start, size_t source_start, size_t n) {
    AbstractSimpleVector_sp dest;
    size_t dstart, dend;
    asAbstractSimpleVectorRange(dest, dstart, dend);
    dest.as_unsafe<simple_type>()->copy_n(source, dest_start + dstart,
                                          source_start, n);
  }
  void copy_nd(Array_sp source,
               size_t dest_start, size_t source_start, size_t n) final {
    copy_n(source, dest_start, source_start, n);
  }
};
}; // namespace core

// It's easier to manage the duplicate code if its isolated in separate files

#include <clasp/core/array_t.h>
#include <clasp/core/string.h>
#include <clasp/core/array_short_float.h>
#include <clasp/core/array_float.h>
#include <clasp/core/array_double.h>
#include <clasp/core/array_long_float.h>
#include <clasp/core/array_size_t.h>
#include <clasp/core/array_fixnum.h>
#include <clasp/core/array_int64.h>
#include <clasp/core/array_int32.h>
#include <clasp/core/array_int16.h>
#include <clasp/core/array_int8.h>
#include <clasp/core/array_int4.h>
#include <clasp/core/array_int2.h>
#include <clasp/core/array_bit.h>

// ----------------------------------------------------------------------
//
// functions
//

namespace core {
// Like ecl__vector_start_end
T_mv clasp_vectorStartEnd(Symbol_sp fn, T_sp thing, Fixnum_sp start, Fixnum_sp end);

}; // namespace core

// ----------------------------------------------------------------------
//
// Array and String functions
//

namespace core {

void core__copy_subarray(Array_sp dest, size_t destStart, Array_sp orig, size_t origStart, size_t len);

T_sp core__rowMajorAset(T_sp, Array_sp, gc::Fixnum);
T_sp cl__rowMajorAref(Array_sp, gc::Fixnum);

}; // namespace core
