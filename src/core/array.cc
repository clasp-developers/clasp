/*
    File: array.cc
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
// #define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/bformat.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/compiler.h>
#include <clasp/core/numbers.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/primitives.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/array.h>
#include <clasp/core/fli.h>
#include <clasp/core/wrappers.h>

/*! Adding a new specialized array

Look at the specialized array for uint32_t as an example.

1.  For specialized arrays with type that is a multiple of bytes sized...
    Copy the template code in array_specialized_template.h for to a new header file
    and change SPECIALIZE_ME to the new specialized type.
    For anything else - (nibbles, 2bit words, odd sized packed types) you are going to have to
    figure it out.
2.  Add the #include <clasp/core/array-<new-type>.h> to the body of array.h after the rest of them
3.  Add an enum to clasp_elttype for the specialized type
4.  Add a CL_VALUE_ENUM entry to array.h  after CL_BEGIN_ENUM(clasp_elttype,_sym_clasp_elttype,"clasp_elttype");
5.  Add a new deftype definition to predlib.lisp
6.  Edit arraylib.lisp and add the type to +upgraded-array-element-types+
    Make sure you add it in the right place - if it's an integer type
    it needs to go before any larger integer types so that the smallest
    necessary upgraded-array-element-type is chosen every time.
7.  Add the type test and make_xxxx calls to array.cc make_vector and make_mdarray
8.  Add the class to hierarchy.lisp

Check the following...
1.  Maybe change these methods in array_<new-type>.h
    virtual T_sp array_type() const final { return cl::_sym_simple_array; };
    virtual T_sp element_type() const override { return core::_sym_size_t;};
    virtual T_sp arrayElementType() const override { return core::_sym_size_t; };
    virtual clasp_elttype elttype() const { return clasp_aet_size_t; };

*/

namespace core {
void missingValueListError(List_sp indices) { SIMPLE_ERROR("The value was missing after the indices {}", _rep_(indices)); }
void tooManyIndicesListError(List_sp indices) { SIMPLE_ERROR("Too many indices {}", _rep_(indices)); }
void badAxisNumberError(Symbol_sp fn_name, size_t rank, size_t axisNumber) {
  SIMPLE_ERROR("In {} illegal axis number {} must be less than rank {}", _rep_(fn_name), axisNumber, rank);
}
void badIndexError(T_sp array, size_t axis, gc::Fixnum index, size_t dimension) {
  ERROR(core::_sym_array_out_of_bounds,
        core::lisp_createList(
            kw::_sym_expected_type,
            // `(integer 0 (,dimension))
            core::lisp_createList(cl::_sym_integer, clasp_make_fixnum(0), core::lisp_createList(clasp_make_fixnum(dimension))),
            kw::_sym_datum, clasp_make_fixnum(index), kw::_sym_object, array, kw::_sym_axis, clasp_make_fixnum(axis)));
}
void badRMIndexError(T_sp array, gc::Fixnum index, size_t totalSize) {
  ERROR(core::_sym_row_major_out_of_bounds,
        core::lisp_createList(
            kw::_sym_expected_type,
            core::lisp_createList(cl::_sym_integer, clasp_make_fixnum(0), core::lisp_createList(clasp_make_fixnum(totalSize))),
            kw::_sym_datum, clasp_make_fixnum(index), kw::_sym_object, array));
}
void indexNotFixnumError(T_sp index) { TYPE_ERROR(index, cl::_sym_fixnum); }
void insufficientIndexListError(List_sp indices) { SIMPLE_ERROR("Insufficient indices {}", _rep_(indices)); }
void notStringError(T_sp obj) { TYPE_ERROR(obj, cl::_sym_string); }
void cannotAdjustSizeOfSimpleArrays(T_sp obj) { SIMPLE_ERROR("Cannot adjust the size of a simple array {}", _rep_(obj)); }
void notSequenceError(T_sp obj) { TYPE_ERROR(obj, cl::_sym_string); }
void notAdjustableError(Symbol_sp fn_name, T_sp array) { SIMPLE_ERROR("In {} - array is not adjustable", _rep_(fn_name)); }
void notVectorError(T_sp array) { TYPE_ERROR(array, cl::_sym_vector); }
void noFillPointerError(Symbol_sp fn_name, T_sp thing) {
  ERROR(cl::_sym_simpleTypeError,
        core::lisp_createList(kw::_sym_format_control,
                              core::lisp_createStr("When calling ~S the argument ~S is not an array with a fill pointer."),
                              kw::_sym_format_arguments, core::lisp_createList(fn_name, thing), kw::_sym_expected_type,
                              core::lisp_createList(cl::_sym_and, cl::_sym_vector,
                                                    core::lisp_createList(cl::_sym_satisfies, cl::_sym_array_has_fill_pointer_p)),
                              kw::_sym_datum, thing));
}
void noFillPointerSpecializedArrayError(T_sp thing) {
  Array_sp athing = gc::As<Array_sp>(thing);
  ERROR(cl::_sym_simpleTypeError,
        core::lisp_createList(
            kw::_sym_format_control,
            core::lisp_createStr(
                "When calling vectorPushExtend for a ~S specialized array ~S the argument ~S is not an array with a fill pointer."),
            kw::_sym_format_arguments, core::lisp_createList(athing->element_type(), thing), kw::_sym_expected_type,
            core::lisp_createList(cl::_sym_and, cl::_sym_vector,
                                  core::lisp_createList(cl::_sym_satisfies, cl::_sym_array_has_fill_pointer_p)),
            kw::_sym_datum, thing));
}

}; // namespace core

namespace core {

// ------------------------------------------------------------
//
// Array_O
//
//

void Array_O::sxhash_equalp(HashGenerator& hg) const {
  // TODO:  Write optimized versions for different array types
  for (size_t i = 0; i < this->length(); ++i) {
    if (!hg.isFilling())
      break;
    T_sp obj = this->rowMajorAref(i);
    HashTable_O::sxhash_equalp(hg, obj);
  }
}

void Array_O::fillInitialContents(T_sp ic) {
  unlikely_if(gc::IsA<MDArray_sp>(ic)) {
    if (this->rank() != 1) {
      notVectorError(ic);
      UNREACHABLE();
    }
  }
  if (cl__length(ic) != this->arrayTotalSize())
    SIMPLE_ERROR("The number of elements {} in :INITIAL-CONTENTS does not match the size of the vector {}", cl__length(ic),
                 this->arrayTotalSize());
  if (ic.nilp()) {
    // do nothing
  } else if (Cons_sp ccInitialContents = ic.asOrNull<Cons_O>()) {
    List_sp cInitialContents = ccInitialContents;
    size_t i = 0;
    for (auto cur : cInitialContents) {
      T_sp obj = oCar(cur);
      this->rowMajorAset(i, obj);
      ++i;
    }
  } else if (Array_sp vic = ic.asOrNull<Array_O>()) {
    for (size_t i = 0; i < vic->length(); ++i) {
      T_sp obj = vic->rowMajorAref(i);
      this->rowMajorAset(i, obj);
    }
  } else {
    SIMPLE_ERROR("Illegal :INITIAL-CONTENTS");
  }
}

// Like ARRAY-ROW-MAJOR-INDEX, but with a vector<int> instead of a list.
// Used only in the printer, below.
size_t Array_O::index_vector_int(const vector<int>& indices) const {
  size_t rank = this->rank();
  size_t offset = 0;
  size_t oneIndex = 0;
  size_t idx = 0;
  for (idx = 0; idx < rank; ++idx) {
    if (idx > 0)
      offset *= this->arrayDimension(idx);
    oneIndex = indices[idx];
    offset += oneIndex;
  }
  return offset;
}

size_t Array_O::arrayRowMajorIndex(List_sp indices) const {
  size_t rank = this->rank();
  size_t offset = 0;
  size_t idx = 0;
  List_sp cur = indices;
  for (; idx < rank; ++idx) {
    size_t curDimension = this->arrayDimension(idx);
    LIKELY_if(cur.consp()) {
      T_sp index = oCar(cur);
      LIKELY_if(index.fixnump()) {
        gc::Fixnum oneIndex = index.unsafe_fixnum();
        unlikely_if(oneIndex < 0 || oneIndex >= curDimension) { badIndexError(this->asSmartPtr(), idx, oneIndex, curDimension); }
        offset = offset * curDimension + oneIndex;
      }
      else {
        indexNotFixnumError(index);
      }
    }
    else {
      // cur is nil, and so
      insufficientIndexListError(indices);
    }
    cur = oCdr(cur);
  }
  unlikely_if(cur.consp()) { tooManyIndicesListError(indices); }
  return offset;
}

size_t Array_O::arrayRowMajorIndex(Vaslist_sp indices) const {
  size_t rank = this->rank();
  size_t indices_passed = indices->nargs();
  unlikely_if(indices_passed < rank) { insufficientIndexListError(core__list_from_vaslist(indices)); }
  else {
    unlikely_if(indices_passed > rank) { tooManyIndicesListError(core__list_from_vaslist(indices)); }
  }
  size_t offset = 0;
  size_t idx = 0;
  for (; idx < rank; ++idx) {
    core::T_sp one((gctools::Tagged)(*indices)[idx]);
    size_t curDimension = this->arrayDimension(idx);
    LIKELY_if(one.fixnump()) {
      gc::Fixnum oneIndex = one.unsafe_fixnum();
      unlikely_if(oneIndex < 0 || oneIndex >= curDimension) { badIndexError(this->asSmartPtr(), idx, oneIndex, curDimension); }
      offset = offset * curDimension + oneIndex;
    }
    else {
      indexNotFixnumError(one);
    }
  }
  return offset;
}

CL_LISPIFY_NAME("cl:array-dimensions");
DOCGROUP(clasp);
CL_DEFUN List_sp cl__arrayDimensions(Array_sp array) {
  List_sp indices = nil<T_O>();
  for (cl_index i = array->rank() - 1; i >= 0; i--) {
    indices = Cons_O::create(make_fixnum(array->arrayDimension(i)), indices);
  }
  return indices;
}

CL_LISPIFY_NAME("cl:adjustable-array-p");
DOCGROUP(clasp);
CL_DEFUN bool cl__adjustable_array_p(Array_sp array) { return array->adjustableArrayP(); }

CL_LISPIFY_NAME("cl:array-dimension");
DOCGROUP(clasp);
CL_DEFUN size_t cl__arrayDimension(Array_sp array, size_t idx) { return array->arrayDimension(idx); }

CL_LISPIFY_NAME("core:data-vector-p");
DOCGROUP(clasp);
CL_DEFUN bool core__data_vector_p(T_sp obj) { return gc::IsA<AbstractSimpleVector_sp>(obj); }

CL_LISPIFY_NAME("core:check-rank");
DOCGROUP(clasp);
CL_DEFUN T_mv core__check_rank(Array_sp array, size_t vs_rank) {
  size_t rank = array->rank();
  if (rank != vs_rank)
    SIMPLE_ERROR("Wrong number of subscripts, {}, for an array of rank {}.", vs_rank, rank);
  return Values0<T_O>();
}

CL_LISPIFY_NAME("core:check-index");
DOCGROUP(clasp);
CL_DEFUN T_mv core__check_index(size_t index, size_t max, size_t axis) {
  if (!((index >= 0) && (index < max)))
    SIMPLE_ERROR("Invalid index {} for axis {} of array: expected 0-{}", index, axis, (max - 1));
  return Values0<T_O>();
}

// ------------------------------------------------------------
//
// MDArray_O
//
//

// One dimension constructor
MDArray_O::MDArray_O(Rank1 dummy, size_t dimension, T_sp fillPointer, Array_sp data, bool displacedToP,
                     Fixnum_sp displacedIndexOffset)
    : _FillPointerOrLengthOrDummy(dimension), _Data(data), // It better be an array
      _DisplacedIndexOffset(displacedIndexOffset.unsafe_fixnum()), _Flags(fillPointer.notnilp(), displacedToP),
      _Dimensions(1, dimension, true) {
  size_t arrayTotalSize = dimension;
  this->_ArrayTotalSize = arrayTotalSize;
  if (fillPointer.fixnump()) {
    this->_FillPointerOrLengthOrDummy = fillPointer.unsafe_fixnum();
  } else {
    this->_FillPointerOrLengthOrDummy = arrayTotalSize;
  }
}

// Multi-dimensional constructor
MDArray_O::MDArray_O(size_t rank, List_sp dimensions, Array_sp data, bool displacedToP, Fixnum_sp displacedIndexOffset)
    : _FillPointerOrLengthOrDummy(0xDEADBEEF01234567), _Data(data), // It better be an array
      _DisplacedIndexOffset(displacedIndexOffset.unsafe_fixnum()), _Flags(false, displacedToP), _Dimensions(rank, 0, true) {
  size_t arrayTotalSize = 1;
  size_t irank = 0;
  // dimensions has to be right
  // This code cannot call error handlers
  // we are in an allocator here
  for (auto cur : dimensions) {
    T_sp tdim = oCar(cur);
    size_t dim = tdim.unsafe_fixnum();
    this->_Dimensions[irank++] = dim;
    arrayTotalSize *= dim;
  }
  if (irank != rank) {
    SIMPLE_ERROR("Mismatch in the number of arguments rank = {} indices = {}", rank, _rep_(dimensions));
  }
  this->_ArrayTotalSize = arrayTotalSize;
}

T_sp MDArray_O::replaceArray(T_sp other) {
  MDArray_sp mdo = gc::As<MDArray_sp>(other);
  *this = *mdo;
  for (size_t i(0); i < mdo->_Dimensions.length(); ++i) {
    this->_Dimensions[i] = mdo->_Dimensions[i];
  }
  return this->asSmartPtr();
}

void MDArray_O::set_data(Array_sp a) { this->_Data = a; }

void MDArray_O::sxhash_(HashGenerator& hg) const {
  // Just to get it working. FIXME
  this->General_O::sxhash_(hg);
}

Array_sp MDArray_O::unsafe_subseq(size_t start, size_t iend) const {
  return this->_Data->unsafe_subseq(start + this->_DisplacedIndexOffset, iend + this->_DisplacedIndexOffset);
}
Array_sp MDArray_O::unsafe_setf_subseq(size_t start, size_t iend, Array_sp new_subseq) {
  return this->_Data->unsafe_setf_subseq(start + this->_DisplacedIndexOffset, iend + this->_DisplacedIndexOffset, new_subseq);
}

bool MDArray_O::equalp(T_sp other) const {
  if (&*other == this)
    return true;
  if (!other.generalp())
    return false;
  if (!gc::IsA<Array_sp>(other))
    return false;
  Array_sp aother = gc::As_unsafe<Array_sp>(other);
  if (this->rank() != aother->rank())
    return false;
  // For vectors we need to respect fill pointers, so don't use _Dimensions.
  // FIXME?: Could move this into ComplexVector_O or something
  if (this->rank() == 1) {
    if (aother->length() != this->length())
      return false;
    for (size_t i(0), iEnd(this->length()); i < iEnd; ++i) {
      if (!cl__equalp(this->rowMajorAref(i), aother->rowMajorAref(i)))
        return false;
    }
    return true;
  }
  MDArray_sp mdaother = gc::As_unsafe<MDArray_sp>(other);
  for (size_t d = 0; d < this->rank(); ++d) {
    if (this->_Dimensions[d] != mdaother->_Dimensions[d])
      return true;
  }
  for (size_t i(0), iEnd(this->arrayTotalSize()); i < iEnd; ++i) {
    if (!cl__equalp(this->rowMajorAref(i), mdaother->rowMajorAref(i)))
      return false;
  }
  return true;
}

T_sp MDArray_O::vectorPush(T_sp newElement) {
  unlikely_if(!this->_Flags.fillPointerP()) noFillPointerError(cl::_sym_vectorPush, this->asSmartPtr());
  cl_index idx = this->_FillPointerOrLengthOrDummy;
  unlikely_if(idx >= this->_ArrayTotalSize) { return nil<T_O>(); }
  this->_Data->rowMajorAset(idx + this->_DisplacedIndexOffset, newElement);
  ++this->_FillPointerOrLengthOrDummy;
  return clasp_make_fixnum(idx);
}

size_t calculate_extension(size_t arrayTotalSize) {
  size_t new_size = arrayTotalSize;
  if (new_size < 8)
    new_size = 8;
  return new_size;
}

SYMBOL_EXPORT_SC_(ClPkg, vectorPushExtend);
Fixnum_sp MDArray_O::vectorPushExtend(T_sp newElement, size_t extension) {
  unlikely_if(!this->_Flags.fillPointerP()) noFillPointerError(cl::_sym_vectorPushExtend, this->asSmartPtr());
  cl_index idx = this->_FillPointerOrLengthOrDummy;
  unlikely_if(idx >= this->_ArrayTotalSize) {
    if (extension <= 0)
      extension = calculate_extension(this->_ArrayTotalSize);
    cl_index new_size = this->_ArrayTotalSize + extension;
    this->resize(new_size);
  }
  this->_Data->rowMajorAset(idx + this->_DisplacedIndexOffset, newElement);
  ++this->_FillPointerOrLengthOrDummy;
  return make_fixnum(idx);
}

// ----------------------------------------------------------------------
//

CL_LISPIFY_NAME("cl:array-element-type");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__arrayElementType(Array_sp array) { return array->element_type(); }

CL_LAMBDA(array core:&va-rest core::indices);
CL_LISPIFY_NAME("cl:arrayRowMajorIndex");
DOCGROUP(clasp);
CL_DEFUN size_t cl__arrayRowMajorIndex(Array_sp array, Vaslist_sp indices) { return array->arrayRowMajorIndex(indices); }

CL_LISPIFY_NAME("cl:RowMajorAref");
DOCGROUP(clasp);
CL_DEFUN_SETF T_sp core__rowMajorAset(T_sp value, Array_sp array, gc::Fixnum idx) {
  // bounds check
  size_t max = array->arrayTotalSize();
  unlikely_if((idx < 0) || (idx >= max)) badRMIndexError(array, idx, max);
  // set
  array->rowMajorAset(idx, value);
  return value;
}

CL_LISPIFY_NAME("cl:rowMajorAref");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__rowMajorAref(Array_sp array, gc::Fixnum idx) {
  // bounds check
  size_t max = array->arrayTotalSize();
  unlikely_if((idx < 0) || (idx >= max)) badRMIndexError(array, idx, max);
  // actual ref
  return array->rowMajorAref(idx);
}

// Note for the curious: this is kind of a sham function.
// In cclasp we use it when we move bounds checks out from the aref callee
// to the caller, but in that case it should always be transformed into a
// call to cc_checkBound. See cleavir/bir-to-bmir.lisp for that.
CL_LISPIFY_NAME("core:checkBound");
DOCGROUP(clasp);
CL_DEFUN Fixnum core__check_bound(AbstractSimpleVector_sp vec, size_t len, gc::Fixnum idx) {
  unlikely_if((idx < 0) || (idx >= len)) badIndexError(vec, 0, idx, len);
  return idx;
}

CL_LISPIFY_NAME("core:vref");
DOCGROUP(clasp);
CL_DEFUN T_sp core__vref(AbstractSimpleVector_sp vec, size_t idx) { return vec->vref(idx); }

CL_LISPIFY_NAME("CORE:vref");
DOCGROUP(clasp);
CL_DEFUN_SETF T_sp core__vset(T_sp value, AbstractSimpleVector_sp vec, size_t idx) {
  vec->vset(idx, value);
  return value;
}

DOCGROUP(clasp);
CL_DEFUN size_t core__arrayFlags(Array_sp a) {
  if (gc::IsA<AbstractSimpleVector_sp>(a)) {
    return 0;
  } else if (gc::IsA<MDArray_sp>(a)) {
    MDArray_sp mda = gc::As_unsafe<MDArray_sp>(a);
    return mda->_Flags._Flags;
  }
  SIMPLE_ERROR("Cannot get array-flags of {}", _rep_(a));
}

DOCGROUP(clasp);
CL_DEFUN void core__mdarray_dump(Array_sp a) {
  MDArray_sp mda = gc::As<MDArray_sp>(a);
  clasp_write_string(fmt::format("MDArray address = {}\n", (void*)&*mda));
  clasp_write_string(fmt::format("MDArray _ArrayTotalSize = {}\n", mda->_ArrayTotalSize));
  clasp_write_string(fmt::format("MDArray _Data = {}\n", (void*)&*(mda->_Data)));
  clasp_write_string(fmt::format("MDArray _DisplacedIndexOffset = {}\n", mda->_DisplacedIndexOffset));
  clasp_write_string(fmt::format("MDArray _Flags = {}\n", mda->_Flags._Flags));
  clasp_write_string(fmt::format("MDArray _Dimensions.length() = {}\n", mda->_Dimensions.length()));
  for (size_t i(0); i < mda->_Dimensions.length(); ++i) {
    clasp_write_string(fmt::format("MDArray _Dimensions[{} = {}\n", i, mda->_Dimensions[i]));
  }
}

CL_NAME("FILL-POINTER");
DOCGROUP(clasp);
CL_DEFUN_SETF size_t core__fillPointerSet(size_t idx, Array_sp vector)
{
  unlikely_if (!vector->arrayHasFillPointerP()) noFillPointerError(core::_sym_fillPointerSet,vector);
  sequenceIndexInBounds(core::_sym_fillPointerSet,vector->arrayTotalSize(),idx);
  vector->fillPointerSet(idx);
  return idx;
}

DOCGROUP(clasp);
CL_DEFUN size_t cl__fillPointer(Array_sp vector) {
  unlikely_if(!vector->arrayHasFillPointerP()) noFillPointerError(cl::_sym_fillPointer, vector);
  return vector->fillPointer();
}

DOCGROUP(clasp);
CL_DEFUN bool cl__arrayHasFillPointerP(Array_sp array) { return array->arrayHasFillPointerP(); }

CL_LISPIFY_NAME("cl:array-total-size");
DOCGROUP(clasp);
CL_DEFUN size_t cl__arrayTotalSize(Array_sp array) { return array->arrayTotalSize(); }

CL_LISPIFY_NAME("cl:array-rank");
DOCGROUP(clasp);
CL_DEFUN size_t cl__array_rank(Array_sp array) { return array->rank(); }

CL_LAMBDA(core::array);
CL_DECLARE();
CL_DOCSTRING(R"dx(arrayDisplacement)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv cl__array_displacement(Array_sp array) {
  unlikely_if(!gc::IsA<MDArray_sp>(array)) { return Values(nil<T_O>(), clasp_make_fixnum(0)); }
  MDArray_O* mdarray = reinterpret_cast<MDArray_O*>(&*array);
  return Values(mdarray->displacedTo(), clasp_make_fixnum(mdarray->displacedIndexOffset()));
}

// Next two used internally. In the compiler they'll be inlined.
CL_LISPIFY_NAME("core:%displacement");
DOCGROUP(clasp);
CL_DEFUN T_sp core__PERCENTdisplacement(MDArray_sp array) { return array->realDisplacedTo(); }
CL_LISPIFY_NAME("core:%displaced-index-offset");
DOCGROUP(clasp);
CL_DEFUN T_sp core__PERCENTdisplaced_index_offset(MDArray_sp array) { return clasp_make_fixnum(array->displacedIndexOffset()); }

void core__copy_subarray(Array_sp dest, Fixnum_sp destStart, Array_sp orig, Fixnum_sp origStart, Fixnum_sp len) {
  // TODO: THIS NEEDS TO BE OPTIMIZED FOR DIFFERENT TYPES OF ARRAYS!!!!!!!
  //       Currently this is very inefficient
  size_t iLen = unbox_fixnum(len);
  if (iLen == 0)
    return;
  size_t iDestStart = unbox_fixnum(destStart);
  size_t iOrigStart = unbox_fixnum(origStart);
  if ((iLen + iDestStart) >= dest->arrayTotalSize())
    iLen = dest->arrayTotalSize() - iDestStart;
  if ((iLen + iOrigStart) >= orig->arrayTotalSize())
    iLen = orig->arrayTotalSize() - iOrigStart;
  if (iDestStart < iOrigStart) {
    for (size_t i = 0; i < iLen; ++i) {
      dest->rowMajorAset(iDestStart, orig->rowMajorAref(iOrigStart));
      ++iDestStart;
      ++iOrigStart;
    }
  } else {
    iDestStart += iLen;
    iOrigStart += iLen;
    for (size_t i = 0; i < iLen; ++i) {
      --iDestStart;
      --iOrigStart;
      dest->rowMajorAset(iDestStart, orig->rowMajorAref(iOrigStart));
    }
  }
}

CL_LISPIFY_NAME("cl:aref"); // SETF symbol
CL_LAMBDA(value array core:&va-rest indices);
CL_DECLARE();
CL_DOCSTRING(R"dx(aset)dx");
DOCGROUP(clasp);
CL_DEFUN_SETF T_sp core__aset(T_sp value, Array_sp array, Vaslist_sp indices) {
  cl_index rowMajorIndex = array->arrayRowMajorIndex(indices);
  array->rowMajorAset(rowMajorIndex, value);
  return value;
};

CL_LISPIFY_NAME("cl:aref");
CL_LAMBDA(array core:&va-rest core::indices);
DOCGROUP(clasp);
CL_DEFUN T_sp cl__aref(Array_sp array, Vaslist_sp vargs) {
  cl_index rowMajorIndex = array->arrayRowMajorIndex(vargs);
  return array->rowMajorAref(rowMajorIndex);
}

// Big evil FIXME: These are functions are basically backup for when the
// compiler can't inline an atomic access. BUT: They aren't actually atomic.
// This is difficult to fix as the underlying GCArrays are actually not atomic.
// Perhaps C++20's atomic_ref could help in the future?
// And a bit of a KLUDGE: These ignore the atomic order, since figuring that
// out at runtime seems a little silly.
CL_LISPIFY_NAME("core:atomic-aref");
CL_LAMBDA(order array core:&va-rest core::indices);
DOCGROUP(clasp);
CL_DEFUN T_sp core__atomic_aref(T_sp order, Array_sp array, Vaslist_sp vargs) {
  (void)order; // ignore
  return cl__aref(array, vargs);
}

CL_LISPIFY_NAME("core:atomic-aref");
CL_LAMBDA(value order array core:&va-rest core::indices);
DOCGROUP(clasp);
CL_DEFUN_SETF T_sp core__atomic_aset(T_sp value, T_sp order, Array_sp array, Vaslist_sp indices) {
  (void)order; // ignore
  return core__aset(value, array, indices);
}

CL_LAMBDA(order old value array core:&va-rest core::indices);
DOCGROUP(clasp);
CL_DEFUN T_sp core__acas(T_sp order, T_sp cmp, T_sp nvalue, Array_sp array, Vaslist_sp indices) {
  (void)order; // ignore
  T_sp old = cl__aref(array, indices);
  if (cmp == old)
    core__aset(nvalue, array, indices);
  return old;
}

CL_LAMBDA(array core:&va-rest indices);
CL_LISPIFY_NAME("core:index");
DOCGROUP(clasp);
CL_DEFUN gc::Fixnum core__index(Array_sp array, Vaslist_sp indices) { return array->arrayRowMajorIndex(indices); }

CL_LISPIFY_NAME("cl:svref");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__svref(SimpleVector_sp simple_vector, size_t idx) { return simple_vector->rowMajorAref(idx); }

CL_LISPIFY_NAME("cl:svref");
DOCGROUP(clasp);
CL_DEFUN_SETF T_sp core__setf_svref(T_sp val, SimpleVector_sp simple_vector, cl_index idx) {
  simple_vector->rowMajorAset(idx, val);
  return val;
}

/*! Fill the range of elements of the array,
   if end is nil then fill to the end of the array*/
CL_LISPIFY_NAME("core:fill-array-with-elt");
DOCGROUP(clasp);
CL_DEFUN void core__fillArrayWithElt(Array_sp array, T_sp element, cl_index start, T_sp end) {
  // dimensions probably already checked with core__sequence_start_end
  // this is probably a redundant check
  size_t_pair p = sequenceStartEnd(core::_sym_fillArrayWithElt, array->arrayTotalSize(), start, end);
  array->unsafe_fillArrayWithElt(element, p.start, p.end);
}

CL_LISPIFY_NAME("core:replace-array");
DOCGROUP(clasp);
CL_DEFUN T_sp core__replace_array(Array_sp array, Array_sp other) { return array->replaceArray(other); }

struct RecursivePrint {
  Array_sp me;
  int depth;
  vector<int> indices;
  stringstream ss;

  RecursivePrint(const Array_sp& array) {
    this->me = array;
    this->indices.resize(array->rank(), 0);
    this->depth = array->rank() - 1;
  }
  void recurse(int level) {
    while (1) {
      if (level < depth) {
        ss << "(";
        recurse(level + 1);
        ss << ")";
      } else {
        ss << _rep_(this->me->rowMajorAref(this->me->index_vector_int(this->indices))) << " ";
      }
      if (!this->advanceIndices(level))
        break;
    }
  }

  bool advanceIndices(int level) {
    // if this is a nil sized array, indices is empty, so already return false, otherwise we get EXC_BAD_ACCESS
    unlikely_if(this->indices.empty()) return false;
    this->indices[level]++;
    if (this->indices[level] < this->me->arrayDimension(level))
      return ((true));
    this->indices[level] = 0;
    return ((false));
  }
};

string Array_O::__repr__() const {
  RecursivePrint rp(this->asSmartPtr());
  unlikely_if(this->rank() == 0) {
    rp.ss << "#0A" << this->rowMajorAref(0);
    return rp.ss.str();
  }
  rp.ss << "#" << this->rank() << "A(";
  rp.recurse(0);
  rp.ss << ")";
  return ((rp.ss.str()));
}

SYMBOL_SC_(CorePkg, copy_subarray);
SYMBOL_SC_(CorePkg, aset);

}; // namespace core

// ------------------------------------------------------------
//
// Class AbstractSimpleVector_O
//

namespace core {
bool AbstractSimpleVector_O::equalp(T_sp other) const {
  if (&*other == this)
    return true;
  if (!other.generalp())
    return false;
  if (gc::IsA<AbstractSimpleVector_sp>(other)) {
    AbstractSimpleVector_sp svother = gc::As_unsafe<AbstractSimpleVector_sp>(other);
    if (svother->length() != this->length())
      return false;
    for (size_t i(0), iEnd(this->length()); i < iEnd; ++i) {
      if (!cl__equalp(this->rowMajorAref(i), svother->rowMajorAref(i)))
        return false;
    }
    return true;
  } else if (gc::IsA<MDArray_sp>(other)) {
    MDArray_sp mdother = gc::As_unsafe<MDArray_sp>(other);
    if (mdother->rank() != 1)
      return false;
    if (mdother->length() != this->length())
      return false;
    for (size_t i(0), iEnd(this->length()); i < iEnd; ++i) {
      if (!cl__equalp(this->rowMajorAref(i), mdother->rowMajorAref(i)))
        return false;
    }
    return true;
  }
  return false;
}
}; // namespace core

// ------------------------------------------------------------
//
// MDArrayT

namespace core {

size_t calculateArrayTotalSizeAndValidateDimensions(List_sp dim_desig, size_t& rank) {
  size_t arrayTotalSize = 1;
  rank = 0;
  // dimensions has to be right
  // This code cannot call error handlers
  // we are in an allocator here
  for (auto cur : dim_desig) {
    T_sp tdim = oCar(cur);
    if (!tdim.fixnump())
      // Implicitely tests cl::_sym_arrayTotalSizeLimit, since this is most_positive_fixnum
      TYPE_ERROR(tdim, cl::_sym_UnsignedByte);
    Fixnum fdim = tdim.unsafe_fixnum();
    if (fdim < 0)
      TYPE_ERROR(tdim, cl::_sym_UnsignedByte);
    size_t dim = fdim;
    arrayTotalSize *= dim;
    ++rank;
  }
  return arrayTotalSize;
}
}; // namespace core

////////////////////////////////////////////////////////////

namespace core {
CL_LAMBDA(&rest args);
CL_DECLARE();
CL_DOCSTRING(R"dx(vector)dx");
DOCGROUP(clasp);
CL_DEFUN Vector_sp cl__vector(List_sp args) {
  SimpleVector_sp vec = SimpleVector_O::make(cl__length(args));
  vec->fillInitialContents(args);
  return vec;
};
SYMBOL_EXPORT_SC_(ClPkg, subtypep);
CL_LAMBDA(dimension initial_element initial_element_supplied_p);
CL_DECLARE();
CL_DOCSTRING(R"dx(Make a (simple-vector t))dx");
DOCGROUP(clasp);
CL_DEFUN SimpleVector_sp core__make_simple_vector_t(size_t dimension, T_sp initialElement, bool initialElementSuppliedP) {
  (void)initialElementSuppliedP; // ignore
  return SimpleVector_O::make(dimension, initialElement, true);
}

#define DEFMAKESIMPLEVECTOR(TYPE, OBJECT, SMART)                                                                                   \
  CL_DOCSTRING(R"dx(Make a (simple-vector " #TYPE "))dx")                                                                          \
  DOCGROUP(clasp)                                                                                                                  \
  CL_LAMBDA(dimension initial_element initial_element_supplied_p);                                                                 \
  CL_DEFUN SMART core__make_simple_vector_##TYPE(size_t dimension, T_sp initialElement, bool initialElementSuppliedP) {            \
    OBJECT::value_type init = initialElementSuppliedP ? OBJECT::from_object(initialElement) : OBJECT::default_initial_element();   \
    return OBJECT::make(dimension, init, initialElementSuppliedP);                                                                 \
  }
DEFMAKESIMPLEVECTOR(bit, SimpleBitVector_O, SimpleBitVector_sp);
DEFMAKESIMPLEVECTOR(base_char, SimpleBaseString_O, SimpleBaseString_sp);
DEFMAKESIMPLEVECTOR(character, SimpleCharacterString_O, SimpleCharacterString_sp);
DEFMAKESIMPLEVECTOR(single_float, SimpleVector_float_O, SimpleVector_float_sp);
DEFMAKESIMPLEVECTOR(double_float, SimpleVector_double_O, SimpleVector_double_sp);
#ifdef CLASP_SHORT_FLOAT
DEFMAKESIMPLEVECTOR(short_float, SimpleVector_short_float_O, SimpleVector_short_float_sp);
#else
DEFMAKESIMPLEVECTOR(short_float, SimpleVector_float_O, SimpleVector_float_sp);
#endif
#ifdef CLASP_LONG_FLOAT
DEFMAKESIMPLEVECTOR(long_float, SimpleVector_long_float_O, SimpleVector_long_float_sp);
#else
DEFMAKESIMPLEVECTOR(long_float, SimpleVector_double_O, SimpleVector_double_sp);
#endif
DEFMAKESIMPLEVECTOR(int2, SimpleVector_int2_t_O, SimpleVector_int2_t_sp);
DEFMAKESIMPLEVECTOR(byte2, SimpleVector_byte2_t_O, SimpleVector_byte2_t_sp);
DEFMAKESIMPLEVECTOR(int4, SimpleVector_int4_t_O, SimpleVector_int4_t_sp);
DEFMAKESIMPLEVECTOR(byte4, SimpleVector_byte4_t_O, SimpleVector_byte4_t_sp);
DEFMAKESIMPLEVECTOR(int8, SimpleVector_int8_t_O, SimpleVector_int8_t_sp);
DEFMAKESIMPLEVECTOR(byte8, SimpleVector_byte8_t_O, SimpleVector_byte8_t_sp);
DEFMAKESIMPLEVECTOR(int16, SimpleVector_int16_t_O, SimpleVector_int16_t_sp);
DEFMAKESIMPLEVECTOR(byte16, SimpleVector_byte16_t_O, SimpleVector_byte16_t_sp);
DEFMAKESIMPLEVECTOR(int32, SimpleVector_int32_t_O, SimpleVector_int32_t_sp);
DEFMAKESIMPLEVECTOR(byte32, SimpleVector_byte32_t_O, SimpleVector_byte32_t_sp);
DEFMAKESIMPLEVECTOR(int64, SimpleVector_int64_t_O, SimpleVector_int64_t_sp);
DEFMAKESIMPLEVECTOR(byte64, SimpleVector_byte64_t_O, SimpleVector_byte64_t_sp);
DEFMAKESIMPLEVECTOR(fixnum, SimpleVector_fixnum_O, SimpleVector_fixnum_sp);
DEFMAKESIMPLEVECTOR(size_t, SimpleVector_size_t_O, SimpleVector_size_t_sp);

CL_LAMBDA(dimensions initial_element initial_element_supplied_p);
CL_DECLARE();
CL_DOCSTRING(R"dx(Make a (simple-array t) that is not a vector)dx");
DOCGROUP(clasp);
CL_DEFUN SimpleMDArrayT_sp core__make_simple_mdarray_t(List_sp dimensions, T_sp initialElement, bool initialElementSuppliedP) {
  (void)initialElementSuppliedP;
  return SimpleMDArrayT_O::make_multi_dimensional(dimensions, initialElement, nil<T_O>());
}

#define DEFMAKESIMPLEMDARRAY(TYPE, OBJECT, SMART, SIMPLE)                                                                          \
  CL_DOCSTRING(R"dx(Make a (simple-array " #TYPE ") that is not a vector)dx")                                                      \
  DOCGROUP(clasp)                                                                                                                  \
  CL_LAMBDA(dimension initial_element initial_element_supplied_p);                                                                 \
  CL_DEFUN SMART core__make_simple_mdarray_##TYPE(List_sp dimensions, T_sp initialElement, bool initialElementSuppliedP) {         \
    SIMPLE::value_type init = initialElementSuppliedP ? SIMPLE::from_object(initialElement) : SIMPLE::default_initial_element();   \
    return OBJECT::make_multi_dimensional(dimensions, init, nil<T_O>());                                                           \
  }

DEFMAKESIMPLEMDARRAY(bit, SimpleMDArrayBit_O, SimpleMDArrayBit_sp, SimpleBitVector_O);
DEFMAKESIMPLEMDARRAY(base_char, SimpleMDArrayBaseChar_O, SimpleMDArrayBaseChar_sp, SimpleBaseString_O);
DEFMAKESIMPLEMDARRAY(character, SimpleMDArrayCharacter_O, SimpleMDArrayCharacter_sp, SimpleCharacterString_O);
#ifdef CLASP_SHORT_FLOAT
DEFMAKESIMPLEMDARRAY(short_float, SimpleMDArray_short_float_O, SimpleMDArray_short_float_sp, SimpleVector_short_float_O);
#else
DEFMAKESIMPLEMDARRAY(short_float, SimpleMDArray_float_O, SimpleMDArray_float_sp, SimpleVector_float_O);
#endif
DEFMAKESIMPLEMDARRAY(single_float, SimpleMDArray_float_O, SimpleMDArray_float_sp, SimpleVector_float_O);
DEFMAKESIMPLEMDARRAY(double_float, SimpleMDArray_double_O, SimpleMDArray_double_sp, SimpleVector_double_O);
#ifdef CLASP_LONG_FLOAT
DEFMAKESIMPLEMDARRAY(long_float, SimpleMDArray_long_float_O, SimpleMDArray_long_float_sp, SimpleVector_long_float_O);
#else
DEFMAKESIMPLEMDARRAY(long_float, SimpleMDArray_double_O, SimpleMDArray_double_sp, SimpleVector_double_O);
#endif
DEFMAKESIMPLEMDARRAY(int2, SimpleMDArray_int2_t_O, SimpleMDArray_int2_t_sp, SimpleVector_int2_t_O);
DEFMAKESIMPLEMDARRAY(byte2, SimpleMDArray_byte2_t_O, SimpleMDArray_byte2_t_sp, SimpleVector_byte2_t_O);
DEFMAKESIMPLEMDARRAY(int4, SimpleMDArray_int4_t_O, SimpleMDArray_int4_t_sp, SimpleVector_int4_t_O);
DEFMAKESIMPLEMDARRAY(byte4, SimpleMDArray_byte4_t_O, SimpleMDArray_byte4_t_sp, SimpleVector_byte4_t_O);
DEFMAKESIMPLEMDARRAY(int8, SimpleMDArray_int8_t_O, SimpleMDArray_int8_t_sp, SimpleVector_int8_t_O);
DEFMAKESIMPLEMDARRAY(byte8, SimpleMDArray_byte8_t_O, SimpleMDArray_byte8_t_sp, SimpleVector_byte8_t_O);
DEFMAKESIMPLEMDARRAY(int16, SimpleMDArray_int16_t_O, SimpleMDArray_int16_t_sp, SimpleVector_int16_t_O);
DEFMAKESIMPLEMDARRAY(byte16, SimpleMDArray_byte16_t_O, SimpleMDArray_byte16_t_sp, SimpleVector_byte16_t_O);
DEFMAKESIMPLEMDARRAY(int32, SimpleMDArray_int32_t_O, SimpleMDArray_int32_t_sp, SimpleVector_int32_t_O);
DEFMAKESIMPLEMDARRAY(byte32, SimpleMDArray_byte32_t_O, SimpleMDArray_byte32_t_sp, SimpleVector_byte32_t_O);
DEFMAKESIMPLEMDARRAY(int64, SimpleMDArray_int64_t_O, SimpleMDArray_int64_t_sp, SimpleVector_int64_t_O);
DEFMAKESIMPLEMDARRAY(byte64, SimpleMDArray_byte64_t_O, SimpleMDArray_byte64_t_sp, SimpleVector_byte64_t_O);
DEFMAKESIMPLEMDARRAY(fixnum, SimpleMDArray_fixnum_O, SimpleMDArray_fixnum_sp, SimpleVector_fixnum_O);
DEFMAKESIMPLEMDARRAY(size_t, SimpleMDArray_size_t_O, SimpleMDArray_size_t_sp, SimpleVector_size_t_O);

CL_LAMBDA(element_type dimension &optional adjustable fill_pointer displaced_to (displaced_index_offset 0) initial_element initial_element_supplied_p);
CL_DECLARE();
CL_DOCSTRING(R"dx(Makes a vector based on the arguments. See si_make_vector in ecl>>array.d)dx");
DOCGROUP(clasp);
CL_DEFUN Vector_sp core__make_vector(T_sp element_type, size_t dimension, bool adjustable, T_sp fillPointer, T_sp displacedTo,
                                     Fixnum_sp displacedIndexOffset, T_sp initialElement, bool initialElementSuppliedP) {
  if (fillPointer == cl::_sym_T_O)
    fillPointer = clasp_make_fixnum(dimension);
  if (fillPointer.notnilp() || displacedTo.notnilp())
    adjustable = true;
#define MAKE(simple, complex)                                                                                                      \
  simple::value_type init = initialElementSuppliedP ? simple::from_object(initialElement) : simple::default_initial_element();     \
  if (adjustable)                                                                                                                  \
    return complex::make(dimension, init, initialElementSuppliedP, fillPointer, displacedTo, displacedTo.notnilp(),                \
                         displacedIndexOffset);                                                                                    \
  else                                                                                                                             \
    return simple::make(dimension, init, initialElementSuppliedP);
  // macro over
  if (element_type == cl::_sym_T_O) {
    MAKE(SimpleVector_O, ComplexVector_T_O)
  } else if (element_type == cl::_sym_bit) {
    MAKE(SimpleBitVector_O, BitVectorNs_O)
  } else if (element_type == cl::_sym_base_char) {
    MAKE(SimpleBaseString_O, Str8Ns_O)
  } else if (element_type == cl::_sym_character) {
    MAKE(SimpleCharacterString_O, StrWNs_O)
  } else if (element_type == cl::_sym_double_float) {
    MAKE(SimpleVector_double_O, ComplexVector_double_O)
#ifdef CLASP_SHORT_FLOAT
  } else if (element_type == cl::_sym_short_float) {
    MAKE(SimpleVector_short_float_O, ComplexVector_short_float_O)
#endif
#ifdef CLASP_LONG_FLOAT
  } else if (element_type == cl::_sym_long_float) {
    MAKE(SimpleVector_long_float_O, ComplexVector_long_float_O)
#endif
  } else if (element_type == cl::_sym_single_float) {
    MAKE(SimpleVector_float_O, ComplexVector_float_O)
  } else if (element_type == ext::_sym_integer2) {
    MAKE(SimpleVector_int2_t_O, ComplexVector_int2_t_O)
  } else if (element_type == ext::_sym_byte2) {
    MAKE(SimpleVector_byte2_t_O, ComplexVector_byte2_t_O)
  } else if (element_type == ext::_sym_integer4) {
    MAKE(SimpleVector_int4_t_O, ComplexVector_int4_t_O)
  } else if (element_type == ext::_sym_byte4) {
    MAKE(SimpleVector_byte4_t_O, ComplexVector_byte4_t_O)
  } else if (element_type == ext::_sym_integer8) {
    MAKE(SimpleVector_int8_t_O, ComplexVector_int8_t_O)
  } else if (element_type == ext::_sym_byte8) {
    MAKE(SimpleVector_byte8_t_O, ComplexVector_byte8_t_O)
  } else if (element_type == ext::_sym_integer16) {
    MAKE(SimpleVector_int16_t_O, ComplexVector_int16_t_O)
  } else if (element_type == ext::_sym_byte16) {
    MAKE(SimpleVector_byte16_t_O, ComplexVector_byte16_t_O)
  } else if (element_type == ext::_sym_integer32) {
    MAKE(SimpleVector_int32_t_O, ComplexVector_int32_t_O)
  } else if (element_type == ext::_sym_byte32) {
    MAKE(SimpleVector_byte32_t_O, ComplexVector_byte32_t_O)
  } else if (element_type == ext::_sym_integer64) {
    MAKE(SimpleVector_int64_t_O, ComplexVector_int64_t_O)
  } else if (element_type == ext::_sym_byte64) {
    MAKE(SimpleVector_byte64_t_O, ComplexVector_byte64_t_O)
  } else if (element_type == _sym_size_t) {
    MAKE(SimpleVector_size_t_O, ComplexVector_size_t_O)
  } else if (element_type == cl::_sym_fixnum) {
    MAKE(SimpleVector_fixnum_O, ComplexVector_fixnum_O)
  }
#undef MAKE
  else
    SIMPLE_ERROR("Handle make-vector :element-type {}", _rep_(element_type));
};

CL_LAMBDA(element_type dimension &optional initial_element initial_element_supplied_p);
CL_DECLARE();
CL_DOCSTRING(R"dx(Makes a static vector based on the arguments. See si_make_vector in ecl>>array.d)dx");
DOCGROUP(clasp);
CL_DEFUN Vector_sp core__make_static_vector(T_sp element_type, size_t dimension, T_sp initialElement,
                                            bool initialElementSuppliedP) {
#define MAKE(simple)                                                                                                               \
  simple::value_type init = initialElementSuppliedP ? simple::from_object(initialElement) : simple::default_initial_element();     \
  return simple::make(dimension, init, initialElementSuppliedP, 0, NULL, true);
  // macro over
  if (element_type == cl::_sym_base_char) {
    MAKE(SimpleBaseString_O)
  } else if (element_type == cl::_sym_character) {
    MAKE(SimpleCharacterString_O)
  } else if (element_type == cl::_sym_double_float) {
    MAKE(SimpleVector_double_O)
#ifdef CLASP_SHORT_FLOAT
  } else if (element_type == cl::_sym_short_float) {
    MAKE(SimpleVector_short_float_O)
#endif
#ifdef CLASP_LONG_FLOAT
  } else if (element_type == cl::_sym_long_float) {
    MAKE(SimpleVector_long_float_O)
#endif
  } else if (element_type == cl::_sym_single_float) {
    MAKE(SimpleVector_float_O)
  } else if (element_type == cl::_sym_bit) {
    MAKE(SimpleBitVector_O)
  } else if (element_type == ext::_sym_integer2) {
    MAKE(SimpleVector_int2_t_O)
  } else if (element_type == ext::_sym_byte2) {
    MAKE(SimpleVector_byte2_t_O)
  } else if (element_type == ext::_sym_integer4) {
    MAKE(SimpleVector_int4_t_O)
  } else if (element_type == ext::_sym_byte4) {
    MAKE(SimpleVector_byte4_t_O)
  } else if (element_type == ext::_sym_integer8) {
    MAKE(SimpleVector_int8_t_O)
  } else if (element_type == ext::_sym_byte8) {
    MAKE(SimpleVector_byte8_t_O)
  } else if (element_type == ext::_sym_integer16) {
    MAKE(SimpleVector_int16_t_O)
  } else if (element_type == ext::_sym_byte16) {
    MAKE(SimpleVector_byte16_t_O)
  } else if (element_type == ext::_sym_integer32) {
    MAKE(SimpleVector_int32_t_O)
  } else if (element_type == ext::_sym_byte32) {
    MAKE(SimpleVector_byte32_t_O)
  } else if (element_type == ext::_sym_integer64) {
    MAKE(SimpleVector_int64_t_O)
  } else if (element_type == ext::_sym_byte64) {
    MAKE(SimpleVector_byte64_t_O)
  } else if (element_type == _sym_size_t) {
    MAKE(SimpleVector_size_t_O)
  } else if (element_type == cl::_sym_fixnum) {
    MAKE(SimpleVector_fixnum_O)
  }
#undef MAKE
  else
    SIMPLE_ERROR("Handle make-static-vector :element-type {}", _rep_(element_type));
};

CL_LAMBDA(dimensions element_type adjustable displaced_to displaced_index_offset initial_element initial_element_supplied_p);
CL_DECLARE();
CL_DOCSTRING(R"dx(Makes a multidimensional array based on the arguments.)dx");
DOCGROUP(clasp);
CL_DEFUN MDArray_sp core__make_mdarray(List_sp dimensions, T_sp element_type, bool adjustable, T_sp displacedTo,
                                       Fixnum_sp displacedIndexOffset, T_sp initialElement, bool initialElementSuppliedP) {
  if (displacedTo.notnilp())
    adjustable = true;
#define MAKE(multi, simple)                                                                                                        \
  simple::value_type init = initialElementSuppliedP ? simple::from_object(initialElement) : simple::default_initial_element();     \
  if (adjustable)                                                                                                                  \
    return multi::make_multi_dimensional(dimensions, init, displacedTo, displacedTo.notnilp(), displacedIndexOffset);              \
  else                                                                                                                             \
    return Simple##multi::make_multi_dimensional(dimensions, init, nil<T_O>());
  // macro over
  if (element_type == cl::_sym_T_O) {
    MAKE(MDArrayT_O, SimpleVector_O)
  } else if (element_type == cl::_sym_double_float) {
    MAKE(MDArray_double_O, SimpleVector_double_O)
#ifdef CLASP_SHORT_FLOAT
  } else if (element_type == cl::_sym_short_float) {
    MAKE(MDArray_short_O, SimpleVector_short_float_O)
#endif
#ifdef CLASP_LONG_FLOAT
  } else if (element_type == cl::_sym_long_float) {
    MAKE(MDArray_long_float_O, SimpleVector_long_float_O)
#endif
  } else if (element_type == cl::_sym_single_float) {
    MAKE(MDArray_float_O, SimpleVector_float_O)
  } else if (element_type == cl::_sym_bit) {
    MAKE(MDArrayBit_O, SimpleBitVector_O)
  } else if (element_type == cl::_sym_base_char) {
    MAKE(MDArrayBaseChar_O, SimpleBaseString_O)
  } else if (element_type == cl::_sym_character) {
    MAKE(MDArrayCharacter_O, SimpleCharacterString_O)
  } else if (element_type == ext::_sym_integer2) {
    MAKE(MDArray_int2_t_O, SimpleVector_int2_t_O)
  } else if (element_type == ext::_sym_byte2) {
    MAKE(MDArray_byte2_t_O, SimpleVector_byte2_t_O)
  } else if (element_type == ext::_sym_integer4) {
    MAKE(MDArray_int4_t_O, SimpleVector_int4_t_O)
  } else if (element_type == ext::_sym_byte4) {
    MAKE(MDArray_byte4_t_O, SimpleVector_byte4_t_O)
  } else if (element_type == ext::_sym_integer8) {
    MAKE(MDArray_int8_t_O, SimpleVector_int8_t_O)
  } else if (element_type == ext::_sym_byte8) {
    MAKE(MDArray_byte8_t_O, SimpleVector_byte8_t_O)
  } else if (element_type == ext::_sym_integer16) {
    MAKE(MDArray_int16_t_O, SimpleVector_int16_t_O)
  } else if (element_type == ext::_sym_byte16) {
    MAKE(MDArray_byte16_t_O, SimpleVector_byte16_t_O)
  } else if (element_type == ext::_sym_integer32) {
    MAKE(MDArray_int32_t_O, SimpleVector_int32_t_O)
  } else if (element_type == ext::_sym_byte32) {
    MAKE(MDArray_byte32_t_O, SimpleVector_byte32_t_O)
  } else if (element_type == ext::_sym_integer64) {
    MAKE(MDArray_int64_t_O, SimpleVector_int64_t_O)
  } else if (element_type == ext::_sym_byte64) {
    MAKE(MDArray_byte64_t_O, SimpleVector_byte64_t_O)
  } else if (element_type == _sym_size_t) {
    MAKE(MDArray_size_t_O, SimpleVector_size_t_O)
  } else if (element_type == cl::_sym_fixnum) {
    MAKE(MDArray_fixnum_O, SimpleVector_fixnum_O)
  }
#undef MAKE
  else
    SIMPLE_ERROR("Handle creation of multi-dimensional array of type {}", _rep_(element_type));
};

// ------------------------------------------------------------
// ------------------------------------------------------------

CL_LAMBDA(newElement vector);
CL_DECLARE();
CL_DOCSTRING(R"dx(vectorPush)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__vector_push(T_sp newElement, Vector_sp vec) { return vec->vectorPush(newElement); };

CL_LAMBDA(newElement vector &optional (extension 0));
CL_DECLARE();
CL_DOCSTRING(R"dx(vectorPushExtend)dx");
DOCGROUP(clasp);
CL_DEFUN Fixnum_sp cl__vector_push_extend(T_sp newElement, Vector_sp vec, size_t extension) {
  return vec->vectorPushExtend(newElement, extension);
}

DOCGROUP(clasp);
CL_DEFUN void core__verify_simple_vector_layout(size_t length_offset, size_t data_offset) {
  size_t cxx_length_offset = offsetof(SimpleVector_O, _Data._MaybeSignedLength);
  size_t cxx_data_offset = offsetof(SimpleVector_O, _Data._Data);
  if (length_offset != cxx_length_offset)
    SIMPLE_ERROR("length_offset {} does not match cxx_length_offset {}", length_offset, cxx_length_offset);
  if (data_offset != cxx_data_offset)
    SIMPLE_ERROR("data_offset {} does not match cxx_data_offset {}", data_offset, cxx_data_offset);
}

SYMBOL_EXPORT_SC_(KeywordPkg, vtable);
SYMBOL_EXPORT_SC_(KeywordPkg, FillPointerOrLengthOrDummy);
SYMBOL_EXPORT_SC_(KeywordPkg, ArrayTotalSize);
SYMBOL_EXPORT_SC_(KeywordPkg, Data);
SYMBOL_EXPORT_SC_(KeywordPkg, DisplacedIndexOffset);
SYMBOL_EXPORT_SC_(KeywordPkg, Flags);
SYMBOL_EXPORT_SC_(KeywordPkg, Rank);
SYMBOL_EXPORT_SC_(KeywordPkg, Dimensions);

DOCGROUP(clasp);
CL_DEFUN void core__verify_mdarray_layout(T_sp alist) {
  expect_offset(kw::_sym_FillPointerOrLengthOrDummy, alist,
                offsetof(MDArray_O, _FillPointerOrLengthOrDummy) - gctools::general_tag);
  expect_offset(kw::_sym_ArrayTotalSize, alist, offsetof(MDArray_O, _ArrayTotalSize) - gctools::general_tag);
  expect_offset(kw::_sym_Data, alist, offsetof(MDArray_O, _Data) - gctools::general_tag);
  expect_offset(kw::_sym_DisplacedIndexOffset, alist, offsetof(MDArray_O, _DisplacedIndexOffset) - gctools::general_tag);
  expect_offset(kw::_sym_Flags, alist, offsetof(MDArray_O, _Flags) - gctools::general_tag);
  expect_offset(kw::_sym_Rank, alist, offsetof(MDArray_O, _Dimensions._MaybeSignedLength) - gctools::general_tag);
  expect_offset(kw::_sym_Dimensions, alist, offsetof(MDArray_O, _Dimensions._Data) - gctools::general_tag);
}

SYMBOL_SC_(CorePkg, make_vector);
SYMBOL_EXPORT_SC_(ClPkg, vectorPush);
SYMBOL_EXPORT_SC_(ClPkg, vectorPushExtend);

// Create a byte8 simple vector from any array
DOCGROUP(clasp);
CL_DEFUN Array_sp core__coerce_to_byte8_vector(T_sp object) {
  if (gc::IsA<Array_sp>(object)) {
    Array_sp source = gc::As_unsafe<Array_sp>(object);
    T_sp element_type = source->element_type();
    if (element_type == cl::_sym_single_float) {
      AbstractSimpleVector_sp asv;
      size_t start, end;
      source->asAbstractSimpleVectorRange(asv, start, end);
      const unsigned char* memory_start = reinterpret_cast<const unsigned char*>(asv->rowMajorAddressOfElement_(start));
      const unsigned char* memory_end = reinterpret_cast<const unsigned char*>(asv->rowMajorAddressOfElement_(end));
      SimpleVector_byte8_t_sp result =
          SimpleVector_byte8_t_O::make((memory_end - memory_start), 0, false, (memory_end - memory_start), memory_start);
      return result;
    } else if (element_type == cl::_sym_base_char) {
      AbstractSimpleVector_sp asv;
      size_t start, end;
      source->asAbstractSimpleVectorRange(asv, start, end);
      const unsigned char* memory_start = reinterpret_cast<const unsigned char*>(asv->rowMajorAddressOfElement_(start));
      const unsigned char* memory_end = reinterpret_cast<const unsigned char*>(asv->rowMajorAddressOfElement_(end));
      SimpleVector_byte8_t_sp result =
          SimpleVector_byte8_t_O::make((memory_end - memory_start), 0, false, (memory_end - memory_start), memory_start);
      return result;
    }
    SIMPLE_ERROR("Add support for coercing {} to a byte8 vector", _rep_(source));
  } else if (gc::IsA<clasp_ffi::ForeignData_sp>(object)) {
    clasp_ffi::ForeignData_sp source = gc::As_unsafe<clasp_ffi::ForeignData_sp>(object);
    SimpleVector_byte8_t_sp result = SimpleVector_byte8_t_O::make(
        source->foreign_data_size(), 0, false, source->foreign_data_size(), (const unsigned char*)source->orig_data_ptr());
    return result;
  }
  SIMPLE_ERROR("Add support for coercing {} to a byte8 vector", _rep_(object));
}

// Create a base-char simple vector from any array
DOCGROUP(clasp);
CL_DEFUN clasp_ffi::ForeignData_sp core__coerce_memory_to_foreign_data(Array_sp source) {
  T_sp element_type = source->element_type();
  if (element_type == cl::_sym_single_float) {
    AbstractSimpleVector_sp asv;
    size_t start, end;
    source->asAbstractSimpleVectorRange(asv, start, end);
    const unsigned char* memory_start = reinterpret_cast<const unsigned char*>(asv->rowMajorAddressOfElement_(start));
    const unsigned char* memory_end = reinterpret_cast<const unsigned char*>(asv->rowMajorAddressOfElement_(end));
    auto data = gctools::GC<clasp_ffi::ForeignData_O>::allocate_with_default_constructor();
    data->allocate(ext::_sym_byte8, DeleteOnDtor, memory_end - memory_start);
    memcpy(const_cast<void*>(data->orig_data_ptr()), (void*)memory_start, memory_end - memory_start);
    return data;
  }
  SIMPLE_ERROR("Add support for coercing {} to a simple-base-string", _rep_(source));
}

DOCGROUP(clasp);
CL_DEFUN clasp_ffi::ForeignData_sp core__static_vector_address(Array_sp source) {
  return clasp_ffi::ForeignData_O::create(source->rowMajorAddressOfElement_(0));
};

DOCGROUP(clasp);
CL_DEFUN clasp_ffi::ForeignData_sp core__static_vector_pointer(Array_sp source, size_t offset) {
  return clasp_ffi::ForeignData_O::create((char*)source->rowMajorAddressOfElement_(0) + offset);
}

CL_DOCSTRING(R"dx(Return the simple-vector that stores the data for this array - this is like sbcl sb-ext:array-storage-vector)dx");
DOCGROUP(clasp);
CL_DEFUN Array_sp ext__array_storage_vector(Array_sp source) {
  if (source->displacedToP()) {
    SIMPLE_ERROR("array-storage-vector cannot be used with displaced arrays");
  }
  AbstractSimpleVector_sp bsv;
  size_t ostart = 0, oend;
  ASSERT(ostart == 0);
  source->asAbstractSimpleVectorRange(bsv, ostart, oend);
  return bsv;
}

CL_DOCSTRING(R"dx(Return a pointer to the data in the array source)dx");
DOCGROUP(clasp);
CL_DEFUN clasp_ffi::ForeignData_sp ext__array_pointer(Array_sp source) {
  return clasp_ffi::ForeignData_O::create(source->rowMajorAddressOfElement_(0));
}

CL_DOCSTRING(R"dx(Pin the objects in the list in memory and then call the thunk)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv ext__pinned_objects_funcall(List_sp objects, T_sp thunk) {
  size_t num = cl__length(objects);
  T_O* pointerArray[num];
  size_t idx = 0;
  for (auto cur : objects) {
    T_sp obj = CONS_CAR(cur);
    pointerArray[idx] = obj.raw_();
    idx++;
  }
  return eval::funcall(thunk);
}

CL_DEFUN bool ext__array_no_nans_p(Array_sp array) {
  if (gc::IsA<SimpleVector_float_sp>(array)) {
    auto sa = gc::As_unsafe<SimpleVector_float_sp>(array);
    for (auto const& e : sa) if (std::isnan(e)) return false;
  } else if (gc::IsA<MDArray_float_sp>(array)) {
    auto sa = gc::As_unsafe<MDArray_float_sp>(array);
    for (auto const& e : sa) if (std::isnan(e)) return false;
  } else if (gc::IsA<SimpleMDArray_float_sp>(array)) {
    auto sa = gc::As_unsafe<SimpleMDArray_float_sp>(array);
    for (auto const& e : sa) if (std::isnan(e)) return false;
  } else if (gc::IsA<ComplexVector_float_sp>(array)) {
    auto sa = gc::As_unsafe<ComplexVector_float_sp>(array);
    for (auto const& e : sa) if (std::isnan(e)) return false;
  } else if (gc::IsA<SimpleVector_double_sp>(array)) {
    auto sa = gc::As_unsafe<SimpleVector_double_sp>(array);
    for (auto const& e : sa) if (std::isnan(e)) return false;
  } else if (gc::IsA<MDArray_double_sp>(array)) {
    auto sa = gc::As_unsafe<MDArray_double_sp>(array);
    for (auto const& e : sa) if (std::isnan(e)) return false;
  } else if (gc::IsA<SimpleMDArray_double_sp>(array)) {
    auto sa = gc::As_unsafe<SimpleMDArray_double_sp>(array);
    for (auto const& e : sa) if (std::isnan(e)) return false;
  } else if (gc::IsA<ComplexVector_double_sp>(array)) {
    auto sa = gc::As_unsafe<ComplexVector_double_sp>(array);
    for (auto const& e : sa) if (std::isnan(e)) return false;
#ifdef CLASP_SHORT_FLOAT
  } else if (gc::IsA<SimpleVector_short_float_sp>(array)) {
    auto sa = gc::As_unsafe<SimpleVector_short_float_sp>(array);
    for (auto const& e : sa) if (std::isnan(e)) return false;
  } else if (gc::IsA<MDArray_short_float_sp>(array)) {
    auto sa = gc::As_unsafe<MDArray_short_float_sp>(array);
    for (auto const& e : sa) if (std::isnan(e)) return false;
  } else if (gc::IsA<SimpleMDArray_short_float_sp>(array)) {
    auto sa = gc::As_unsafe<SimpleMDArray_short_float_sp>(array);
    for (auto const& e : sa) if (std::isnan(e)) return false;
  } else if (gc::IsA<ComplexVector_short_float_sp>(array)) {
    auto sa = gc::As_unsafe<ComplexVector_short_float_sp>(array);
    for (auto const& e : sa) if (std::isnan(e)) return false;
#endif
#ifdef CLASP_LONG_FLOAT
  } else if (gc::IsA<SimpleVector_long_float_sp>(array)) {
    auto sa = gc::As_unsafe<SimpleVector_long_float_sp>(array);
    for (auto const& e : sa) if (std::isnan(e)) return false;
  } else if (gc::IsA<MDArray_long_float_sp>(array)) {
    auto sa = gc::As_unsafe<MDArray_long_float_sp>(array);
    for (auto const& e : sa) if (std::isnan(e)) return false;
  } else if (gc::IsA<SimpleMDArray_long_float_sp>(array)) {
    auto sa = gc::As_unsafe<SimpleMDArray_long_float_sp>(array);
    for (auto const& e : sa) if (std::isnan(e)) return false;
  } else if (gc::IsA<ComplexVector_long_float_sp>(array)) {
    auto sa = gc::As_unsafe<ComplexVector_long_float_sp>(array);
    for (auto const& e : sa) if (std::isnan(e)) return false;
#endif
  }
  return true;
}

}; // namespace core
