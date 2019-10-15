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
//#define DEBUG_LEVEL_FULL

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
5.  Add a new deftype definition to predlib.lsp 
6.  Edit arraylib.lsp and add the type to +upgraded-array-element-types+
    Make sure you add it in the right place - if it's an integer type
    it needs to go before any larger integer types so that the smallest
    necessary upgraded-array-element-type is chosen every time.
7.  Add the type test and make_xxxx calls to array.cc make_vector and make_mdarray
8.  Add the class to hierarchy.lsp

Check the following...
1.  Maybe change these methods in array_<new-type>.h
    virtual T_sp array_type() const final { return cl::_sym_simple_array; };
    virtual T_sp element_type() const override { return core::_sym_size_t;};
    virtual T_sp arrayElementType() const override { return core::_sym_size_t; };
    virtual clasp_elttype elttype() const { return clasp_aet_size_t; };

*/

namespace core {
void missingValueListError(List_sp indices) {
  SIMPLE_ERROR(BF("The value was missing after the indices %s") % _rep_(indices));
}
void tooManyIndicesListError(List_sp indices) {
  SIMPLE_ERROR(BF("Too many indices %s") % _rep_(indices));
}
void badAxisNumberError(Symbol_sp fn_name, size_t rank, size_t axisNumber) {
  SIMPLE_ERROR(BF("In %s illegal axis number %d must be less than rank %d") % _rep_(fn_name) % axisNumber % rank );
}
void badIndexError(T_sp array, size_t axis, gc::Fixnum index, size_t dimension) {
  ERROR(core::_sym_array_out_of_bounds,
        core::lisp_createList(kw::_sym_expected_type,
                              // `(integer 0 (,dimension))
                              core::lisp_createList(cl::_sym_integer, clasp_make_fixnum(0),
                                                    core::lisp_createList(clasp_make_fixnum(dimension))),
                              kw::_sym_datum, clasp_make_fixnum(index),
                              kw::_sym_object, array,
                              kw::_sym_axis, clasp_make_fixnum(axis)));
}
void indexNotFixnumError(T_sp index) {
  TYPE_ERROR(index,cl::_sym_fixnum);
}
void insufficientIndexListError(List_sp indices) {
  SIMPLE_ERROR(BF("Insufficient indices %s") % _rep_(indices));
}
void notStringError(T_sp obj) {
  TYPE_ERROR(obj,cl::_sym_string);
}
void cannotAdjustSizeOfSimpleArrays(T_sp obj) {
  SIMPLE_ERROR(BF("Cannot adjust the size of a simple array %s") % _rep_(obj));
}
void notSequenceError(T_sp obj) {
  TYPE_ERROR(obj,cl::_sym_string);
}
void notAdjustableError(Symbol_sp fn_name, T_sp array) {
  SIMPLE_ERROR(BF("In %s - array is not adjustable") % _rep_(fn_name));
}
void notVectorError(T_sp array) {
  TYPE_ERROR(array,cl::_sym_vector);
}
void noFillPointerError(Symbol_sp fn_name, T_sp thing) {
  ERROR(cl::_sym_simpleTypeError,
        core::lisp_createList(kw::_sym_format_control, core::lisp_createStr("When calling ~S the argument ~S is not an array with a fill pointer."),
                              kw::_sym_format_arguments, core::lisp_createList(fn_name, thing),
                              kw::_sym_expected_type, core::lisp_createList(cl::_sym_and,cl::_sym_vector,core::lisp_createList(cl::_sym_satisfies,cl::_sym_array_has_fill_pointer_p)),
                              kw::_sym_datum, thing));
}
void noFillPointerSpecializedArrayError(T_sp thing) {
  Array_sp athing = gc::As<Array_sp>(thing);
  ERROR(cl::_sym_simpleTypeError,
        core::lisp_createList(kw::_sym_format_control, core::lisp_createStr("When calling vectorPushExtend for a ~S specialized array ~S the argument ~S is not an array with a fill pointer."),
                              kw::_sym_format_arguments, core::lisp_createList(athing->element_type(), thing),
                              kw::_sym_expected_type, core::lisp_createList(cl::_sym_and,cl::_sym_vector,core::lisp_createList(cl::_sym_satisfies,cl::_sym_array_has_fill_pointer_p)),
                              kw::_sym_datum, thing));
}


};


namespace core {

// ------------------------------------------------------------
//
// Array_O
//
//

void Array_O::sxhash_equalp(HashGenerator &hg,LocationDependencyPtrT ld) const {
  // TODO:  Write optimized versions for different array types
  for (size_t i = 0; i < this->length(); ++i) {
    if (!hg.isFilling()) break;
    T_sp obj = this->rowMajorAref(i);
    HashTable_O::sxhash_equalp(hg,obj,ld);
  }
}

void Array_O::fillInitialContents(T_sp ic) {
  unlikely_if (gc::IsA<MDArray_sp>(ic)) {
    if (this->rank()!=1) {
      notVectorError(ic);
      UNREACHABLE();
    }
  }
  if (cl__length(ic) != this->arrayTotalSize())
    SIMPLE_ERROR(BF("The number of elements %d in :INITIAL-CONTENTS does not match the size of the vector %d") % cl__length(ic) % this->arrayTotalSize());
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
    SIMPLE_ERROR(BF("Illegal :INITIAL-CONTENTS"));
  }
}

// Like ARRAY-ROW-MAJOR-INDEX, but with a vector<int> instead of a list.
// Used only in the printer, below.
size_t Array_O::index_vector_int(const vector<int> &indices) const {
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
  for ( ; idx<rank; ++idx ) {
    size_t curDimension = this->arrayDimension(idx);
    LIKELY_if (cur.consp()) {
      T_sp index = oCar(cur);
      LIKELY_if (index.fixnump()) {
        gc::Fixnum oneIndex = index.unsafe_fixnum();
        unlikely_if (oneIndex < 0 || oneIndex >= curDimension) {
          badIndexError(this->asSmartPtr(), idx, oneIndex, curDimension);
        }
        offset = offset * curDimension + oneIndex;
      } else {
        indexNotFixnumError(index);
      }
    } else {
      // cur is nil, and so
      insufficientIndexListError(indices);
    }
    cur = oCdr(cur);
  }
  unlikely_if (cur.consp()) {
    tooManyIndicesListError(indices);
  }
  return offset;
}

size_t Array_O::arrayRowMajorIndex(VaList_sp indices) const {
  size_t rank = this->rank();
  size_t indices_passed = indices->remaining_nargs();

  unlikely_if (indices_passed < rank) {
    insufficientIndexListError(core__list_from_va_list(indices));
  } else {
    unlikely_if (indices_passed > rank) {
      tooManyIndicesListError(core__list_from_va_list(indices));
    }
  }
  
  size_t offset = 0;
  size_t idx = 0;
  size_t idxEnd(indices_passed);
  for ( ; idx<rank; ++idx ) {
    core::T_sp one = indices->next_arg();
    size_t curDimension = this->arrayDimension(idx);
    LIKELY_if (one.fixnump()) {
      gc::Fixnum oneIndex = one.unsafe_fixnum();
      unlikely_if (oneIndex < 0 || oneIndex >= curDimension) {
        badIndexError(this->asSmartPtr(), idx, oneIndex, curDimension);
      }
      offset = offset * curDimension + oneIndex;
    } else {
      indexNotFixnumError(one);
    }
  }
  return offset;
}

CL_LISPIFY_NAME("cl:array-dimensions");
CL_DEFUN List_sp cl__arrayDimensions(Array_sp array)
{
  List_sp indices = _Nil<T_O>();
  for (cl_index i = array->rank() - 1; i >= 0; i--) {
    indices = Cons_O::create(make_fixnum(array->arrayDimension(i)), indices);
  }
  return indices;
}

CL_LISPIFY_NAME("cl:adjustable-array-p");
CL_DEFUN bool cl__adjustable_array_p(Array_sp array)
{
  return array->adjustableArrayP();
}


CL_LISPIFY_NAME("cl:array-dimension");
CL_DEFUN size_t cl__arrayDimension(Array_sp array, size_t idx)
{
  return array->arrayDimension(idx);
}

CL_LISPIFY_NAME("core:data-vector-p");
CL_DEFUN bool core__data_vector_p(T_sp obj)
{
  return gc::IsA<AbstractSimpleVector_sp>(obj);
}

CL_LISPIFY_NAME("core:check-rank");
CL_DEFUN T_mv core__check_rank(Array_sp array, size_t vs_rank) {
  size_t rank = array->rank();
  if (rank != vs_rank)
    SIMPLE_ERROR(BF("Wrong number of subscripts, %d, for an array of rank %d.")
                 % vs_rank % rank);
  return Values0<T_O>();
}

CL_LISPIFY_NAME("core:check-index");
CL_DEFUN T_mv core__check_index (size_t index, size_t max, size_t axis) {
  if (!((index >= 0) && (index < max)))
    SIMPLE_ERROR(BF("Invalid index %d for axis %d of array: expected 0-%d")
                 % index % axis % max);
  return Values0<T_O>();
}

// ------------------------------------------------------------
//
// MDArray_O
//
//

// One dimension constructor
MDArray_O::MDArray_O(Rank1 dummy,
                     size_t dimension,
                     T_sp fillPointer,
                     Array_sp data,
                     bool displacedToP,
                     Fixnum_sp displacedIndexOffset )
  : _FillPointerOrLengthOrDummy(dimension),
    _Data(data), // It better be an array
    _DisplacedIndexOffset(displacedIndexOffset.unsafe_fixnum()),
    _Flags(fillPointer.notnilp(),displacedToP),
    _Dimensions(1,dimension,true) {
  size_t arrayTotalSize = dimension;
  this->_ArrayTotalSize = arrayTotalSize;
  if ( fillPointer.fixnump() ) {
    this->_FillPointerOrLengthOrDummy = fillPointer.unsafe_fixnum();
  } else {
    this->_FillPointerOrLengthOrDummy = arrayTotalSize;
  }
}

// Multi-dimensional constructor
MDArray_O::MDArray_O(size_t rank,
                     List_sp dimensions,
                     Array_sp data,
                     bool displacedToP,
                     Fixnum_sp displacedIndexOffset )
  : _FillPointerOrLengthOrDummy(0xDEADBEEF01234567),
    _Data(data), // It better be an array
    _DisplacedIndexOffset(displacedIndexOffset.unsafe_fixnum()),
    _Flags(false,displacedToP),
    _Dimensions(rank,0,true) {
  size_t arrayTotalSize = 1;
  size_t irank = 0;
   // dimensions has to be right
   // This code cannot call error handlers
   // we are in an allocator here
  for ( auto cur : dimensions ) {
    T_sp tdim = oCar(cur);
    size_t dim = tdim.unsafe_fixnum();
    this->_Dimensions[irank++] = dim;
    arrayTotalSize *= dim;
  }
  if (irank!=rank) {
    SIMPLE_ERROR(BF("Mismatch in the number of arguments rank = %d indices = %s") % rank % _rep_(dimensions));
  }
  this->_ArrayTotalSize = arrayTotalSize;
}

T_sp MDArray_O::replaceArray(T_sp other)
{
  MDArray_sp mdo = gc::As<MDArray_sp>(other);
  *this = *mdo;
  for ( size_t i(0); i<mdo->_Dimensions._Length; ++i ) {
    this->_Dimensions[i] = mdo->_Dimensions[i];
  }
  return this->asSmartPtr();
}

void MDArray_O::set_data(Array_sp a) {
  this->_Data = a;
}

void MDArray_O::sxhash_(HashGenerator& hg) const {
  // Just to get it working. FIXME
  this->General_O::sxhash_(hg);
}

Array_sp MDArray_O::unsafe_subseq(size_t start, size_t iend) const
{
  return this->_Data->unsafe_subseq(start+this->_DisplacedIndexOffset,iend+this->_DisplacedIndexOffset);
}
Array_sp MDArray_O::unsafe_setf_subseq(size_t start, size_t iend, Array_sp new_subseq)
{
  return this->_Data->unsafe_setf_subseq(start+this->_DisplacedIndexOffset,iend+this->_DisplacedIndexOffset,new_subseq);
}

void MDArray_O::ensureSpaceAfterFillPointer(T_sp init_element, size_t size) {
  ASSERT(this->arrayHasFillPointerP());
  cl_index left = this->arrayTotalSize() - this->fillPointer();
  if (left < size) {
    this->internalAdjustSize_((size-left)+this->_ArrayTotalSize,init_element,true);
  }
}

bool MDArray_O::equalp(T_sp other) const {
  if (&*other==this) return true;
  if (!other.generalp()) return false;
  if (!gc::IsA<Array_sp>(other)) return false;
  Array_sp aother = gc::As_unsafe<Array_sp>(other);
  if (this->rank()!=aother->rank()) return false;
  if (this->rank() == 1 ) {
    if (aother->length()!=this->length()) return false;
    for (size_t i(0),iEnd(this->length()); i<iEnd; ++i ) {
      if (!cl__equalp(this->rowMajorAref(i),aother->rowMajorAref(i))) return false;
    }
    return true;
  }
  MDArray_sp mdaother = gc::As_unsafe<MDArray_sp>(other);
  for (size_t d=0; d<this->rank(); ++d) {
    if (this->_Dimensions[d] != mdaother->_Dimensions[d]) return true;
  }
  for (size_t i(0),iEnd(this->arrayTotalSize()); i<iEnd; ++i ) {
    if (!cl__equalp(this->rowMajorAref(i),mdaother->rowMajorAref(i))) return false;
  }
  return true;
}



T_sp MDArray_O::vectorPush(T_sp newElement) {
  unlikely_if (!this->_Flags.fillPointerP()) noFillPointerError(cl::_sym_vectorPush,this->asSmartPtr());
  cl_index idx = this->_FillPointerOrLengthOrDummy;
  unlikely_if (idx >= this->_ArrayTotalSize) {
    return _Nil<T_O>();
  }
  this->_Data->rowMajorAset(idx+this->_DisplacedIndexOffset,newElement);
  ++this->_FillPointerOrLengthOrDummy;
  return clasp_make_fixnum(idx);
}

size_t calculate_extension(size_t arrayTotalSize)
{
  size_t new_size = arrayTotalSize;
  if (new_size < 8) new_size = 8;
  return new_size;
}

SYMBOL_EXPORT_SC_(ClPkg,vectorPushExtend);
Fixnum_sp MDArray_O::vectorPushExtend(T_sp newElement, size_t extension) {
  unlikely_if (!this->_Flags.fillPointerP()) noFillPointerError(cl::_sym_vectorPushExtend,this->asSmartPtr());
  cl_index idx = this->_FillPointerOrLengthOrDummy;
  unlikely_if (idx >= this->_ArrayTotalSize) {
    if (extension <= 0) extension = calculate_extension(this->_ArrayTotalSize);
    cl_index new_size = this->_ArrayTotalSize+extension;
    this->internalAdjustSize_(new_size);
  }
  this->_Data->rowMajorAset(idx+this->_DisplacedIndexOffset,newElement);
  ++this->_FillPointerOrLengthOrDummy;
  return make_fixnum(idx);
}

// ------------------------------------------------------------
//
// ComplexVector



// ----------------------------------------------------------------------
//

CL_LISPIFY_NAME("cl:array-element-type");
CL_DEFUN  T_sp cl__arrayElementType(Array_sp array)
{
  return array->element_type();
}

CL_LAMBDA(array core:&va-rest core::indices);
CL_LISPIFY_NAME("cl:arrayRowMajorIndex");
CL_DEFUN size_t cl__arrayRowMajorIndex(Array_sp array, VaList_sp indices) {
  return array->arrayRowMajorIndex(indices);
}


CL_LISPIFY_NAME("core:rowMajorAset");
CL_DEFUN T_sp cl__rowMajorAset(Array_sp array, size_t idx, T_sp value)
{
  array->rowMajorAset(idx,value);
  return value;
}

CL_LISPIFY_NAME("cl:rowMajorAref");
CL_DEFUN  T_sp cl__rowMajorAref(Array_sp array, size_t idx)
{
  return array->rowMajorAref(idx);
}

CL_LISPIFY_NAME("core:vref");
CL_DEFUN T_sp core__vref(AbstractSimpleVector_sp vec, size_t idx)
{
  return vec->vref(idx);
}

CL_LISPIFY_NAME("CORE:vref");
CL_DEFUN_SETF T_sp core__vset(T_sp value, AbstractSimpleVector_sp vec, size_t idx)
{
  vec->vset(idx, value);
  return value;
}

CL_DEFUN size_t core__arrayFlags(Array_sp a)
{
  if (gc::IsA<AbstractSimpleVector_sp>(a)) {
    return 0;
  } else if (gc::IsA<MDArray_sp>(a)) {
    MDArray_sp mda = gc::As_unsafe<MDArray_sp>(a);
    return mda->_Flags._Flags;
  }
  SIMPLE_ERROR(BF("Cannot get array-flags of %s") % _rep_(a));
}

CL_DEFUN void core__mdarray_dump(Array_sp a)
{
  MDArray_sp mda = gc::As<MDArray_sp>(a);
  write_bf_stream(BF("MDArray address = %p\n") % (void*)&*mda);
  write_bf_stream(BF("MDArray _ArrayTotalSize = %d\n") % mda->_ArrayTotalSize);
  write_bf_stream(BF("MDArray _Data = %p\n") % (void*)&*(mda->_Data));
  write_bf_stream(BF("MDArray _DisplacedIndexOffset = %d\n") % mda->_DisplacedIndexOffset);
  write_bf_stream(BF("MDArray _Flags = %d\n") % mda->_Flags._Flags);
  write_bf_stream(BF("MDArray _Dimensions._Length = %d\n") % mda->_Dimensions._Length);
  for ( size_t i(0); i<mda->_Dimensions._Length; ++i ) {
    write_bf_stream(BF("MDArray _Dimensions[%d = %d\n") % i % mda->_Dimensions[i]);
  }
}

CL_NAME("FILL-POINTER-SET");
CL_DEFUN size_t core__fillPointerSet(Array_sp vector, size_t idx)
{
  unlikely_if (!vector->arrayHasFillPointerP()) noFillPointerError(core::_sym_fillPointerSet,vector);
  sequenceIndexInBounds(core::_sym_fillPointerSet,vector->arrayTotalSize(),idx);
  vector->fillPointerSet(idx);
  return idx;
}

CL_NAME("CL:FILL-POINTER");
CL_DEFUN size_t cl__fillPointer(Array_sp vector)
{
  unlikely_if (!vector->arrayHasFillPointerP()) noFillPointerError(cl::_sym_fillPointer,vector);
  return vector->fillPointer();
}


CL_LISPIFY_NAME("cl:array-has-fill-pointer-p");
CL_DEFUN bool cl__arrayHasFillPointerP(Array_sp array) {
  return array->arrayHasFillPointerP();
}

CL_LISPIFY_NAME("cl:array-total-size");
CL_DEFUN size_t cl__arrayTotalSize(Array_sp array) {
  return array->arrayTotalSize();
}

CL_LISPIFY_NAME("cl:array-rank");
CL_DEFUN  size_t cl__array_rank(Array_sp array)
{
  return array->rank();
}

CL_LAMBDA(core::array);
CL_DECLARE();
CL_DOCSTRING("arrayDisplacement");
CL_DEFUN T_mv cl__array_displacement(Array_sp array) {
  unlikely_if (!gc::IsA<MDArray_sp>(array)) {
    return Values(_Nil<T_O>(),clasp_make_fixnum(0));
  }
  MDArray_O* mdarray = reinterpret_cast<MDArray_O*>(&*array);
  return Values(mdarray->displacedTo(),clasp_make_fixnum(mdarray->displacedIndexOffset()));
}

// Next two used internally. In the compiler they'll be inlined.
CL_LISPIFY_NAME("core:%displacement");
CL_DEFUN T_sp core__PERCENTdisplacement(MDArray_sp array) {
  return array->realDisplacedTo();
}
CL_LISPIFY_NAME("core:%displaced-index-offset");
CL_DEFUN T_sp core__PERCENTdisplaced_index_offset(MDArray_sp array) {
  return clasp_make_fixnum(array->displacedIndexOffset());
}

void core__copy_subarray(Array_sp dest, Fixnum_sp destStart, Array_sp orig, Fixnum_sp origStart, Fixnum_sp len) {
  // TODO: THIS NEEDS TO BE OPTIMIZED FOR DIFFERENT TYPES OF ARRAYS!!!!!!!
  //       Currently this is very inefficient
  size_t iLen = unbox_fixnum(len);
  if (iLen == 0)
    return;
  ASSERTF(dest->rank() == 1, BF("dest array must be rank 1 - instead it is %d") % dest->rank());
  ASSERTF(orig->rank() == 1, BF("orig array must be rank 1 - instead it is %d") % orig->rank());
  size_t iDestStart = unbox_fixnum(destStart);
  size_t iOrigStart = unbox_fixnum(origStart);
  if ((iLen + iDestStart) >= dest->arrayTotalSize()) iLen = dest->arrayTotalSize()-iDestStart;
  if ((iLen + iOrigStart) >= orig->arrayTotalSize()) iLen = orig->arrayTotalSize()-iOrigStart;
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

CL_LISPIFY_NAME("CL:aref");
CL_LAMBDA(value array core:&va-rest indices);
CL_DECLARE();
CL_DOCSTRING("aset");
CL_DEFUN_SETF T_sp core__aset(T_sp value, Array_sp array, VaList_sp indices) {
  cl_index rowMajorIndex = array->arrayRowMajorIndex(indices);
  array->rowMajorAset(rowMajorIndex,value);
  return value;
};

CL_LISPIFY_NAME("cl:aref");
CL_LAMBDA(array core:&va-rest core::indices);
CL_DEFUN T_sp cl__aref(Array_sp array, VaList_sp vargs)
{
  cl_index rowMajorIndex = array->arrayRowMajorIndex(vargs);
  return array->rowMajorAref(rowMajorIndex);
}

CL_LAMBDA(array core:&va-rest indices);
CL_LISPIFY_NAME("core:index");
CL_DEFUN gc::Fixnum core__index(Array_sp array, VaList_sp indices) {
  return array->arrayRowMajorIndex(indices);
}


CL_LISPIFY_NAME("cl:svref");
CL_DEFUN T_sp cl__svref(SimpleVector_sp simple_vector, size_t idx)
{
  return simple_vector->rowMajorAref(idx);
}

CL_LISPIFY_NAME("CL:svref");
CL_DEFUN_SETF T_sp core__setf_svref(T_sp val, SimpleVector_sp simple_vector, cl_index idx)
{
  simple_vector->rowMajorAset(idx,val);
  return val;
}

/*! Fill the range of elements of the array,
   if end is nil then fill to the end of the array*/
CL_LISPIFY_NAME("core:fill-array-with-elt");
CL_DEFUN void core__fillArrayWithElt(Array_sp array, T_sp element, cl_index start, T_sp end) {
    // dimensions probably already checked with core__sequence_start_end
    // this is probably a redundant check
    size_t_pair p = sequenceStartEnd(core::_sym_fillArrayWithElt,
                                     array->arrayTotalSize(),start,end);
    array->unsafe_fillArrayWithElt(element,p.start,p.end);
  }

CL_LISPIFY_NAME("core:replace-array");
CL_DEFUN  T_sp core__replace_array(Array_sp array, Array_sp other)
{
  return array->replaceArray(other);
}


struct RecursivePrint {
  Array_sp me;
  int depth;
  vector<int> indices;
  stringstream ss;

  RecursivePrint(const Array_sp &array) {
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
    this->indices[level]++;
    if (this->indices[level] < this->me->arrayDimension(level))
      return ((true));
    this->indices[level] = 0;
    return ((false));
  }
};

string Array_O::__repr__() const {
  RecursivePrint rp(this->asSmartPtr());
  rp.ss << "#" << this->rank() << "A(";
  rp.recurse(0);
  rp.ss << ")";
  return ((rp.ss.str()));
}

SYMBOL_SC_(CorePkg, copy_subarray);
SYMBOL_SC_(CorePkg, aset);

}; /* core */

// ------------------------------------------------------------
//
// Class AbstractSimpleVector_O
//

namespace core {
bool AbstractSimpleVector_O::equalp(T_sp other) const {
  if (&*other==this) return true;
  if (!other.generalp()) return false;
  if (gc::IsA<AbstractSimpleVector_sp>(other)) {
    AbstractSimpleVector_sp svother = gc::As_unsafe<AbstractSimpleVector_sp>(other);
    if (svother->length()!=this->length()) return false;
    for (size_t i(0),iEnd(this->length()); i<iEnd; ++i ) {
      if (!cl__equalp(this->rowMajorAref(i),svother->rowMajorAref(i))) return false;
    }
    return true;
  } else if (gc::IsA<MDArray_sp>(other)) {
    MDArray_sp mdother = gc::As_unsafe<MDArray_sp>(other);
    if (mdother->rank()!=1) return false;
    if (mdother->length()!=this->length()) return false;
    for (size_t i(0),iEnd(this->length()); i<iEnd; ++i ) {
      if (!cl__equalp(this->rowMajorAref(i),mdother->rowMajorAref(i))) return false;
    }
    return true;
  }
  return false;
}
};

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
  for ( auto cur : dim_desig ) {
    T_sp tdim = oCar(cur);
    Fixnum fdim = tdim.unsafe_fixnum();
    if (fdim<0) SIMPLE_ERROR(BF("Array dimensions %d must be positive") % fdim);
    size_t dim = fdim;
    arrayTotalSize *= dim;
    ++rank;
  }
  return arrayTotalSize;
}
};

// ----------------------------------------------------------------------
//
// ArrayT functions
//

namespace core {

MDArrayT_sp MDArrayT_O::create(const gc::Vec0<T_sp>& objs) {
  size_t len = objs.size();
  SimpleVector_sp ss;
  if (len==0) {
    ss = SimpleVector_O::make(len,_Nil<T_O>(),true);
  } else {
    ss = SimpleVector_O::make(len,_Nil<T_O>(),true,len,&(objs[0]));
  }
  MDArrayT_sp result = MDArrayT_O::make(len,_Nil<T_O>(),_Nil<T_O>(),ss,false,clasp_make_fixnum(0));
  return result;
}

ComplexVector_T_sp ComplexVector_T_O::create(const gc::Vec0<T_sp>& objs) {
  size_t len = objs.size();
  SimpleVector_sp ss;
  if (len==0) {
    ss = SimpleVector_O::make(len,_Nil<T_O>(),true);
  } else {
    ss = SimpleVector_O::make(len,_Nil<T_O>(),true,len,&(objs[0]));
  }
  ComplexVector_T_sp result = ComplexVector_T_O::make(len,_Nil<T_O>(),_Nil<T_O>(),ss,false,clasp_make_fixnum(0));
  return result;
}

};

#if 0
namespace core {
SYMBOL_EXPORT_SC_(CorePkg,vectorPushExtend_size_t);
void MDArray_size_t_O::vectorPushExtend_size_t(size_t newElement, size_t extension) {
  unlikely_if (!this->_Flags.fillPointerP()) noFillPointerError(_sym_vectorPushExtend_size_t,this->asSmartPtr());
  cl_index idx = this->_FillPointerOrLengthOrDummy;
  unlikely_if (idx >= this->_ArrayTotalSize) {
    if (extension <= 0) extension = calculate_extension(this->_ArrayTotalSize);
    cl_index new_size = this->_ArrayTotalSize+extension;
    this->internalAdjustSize_(new_size);
  }
  (*this)[idx] = newElement;
  ++this->_FillPointerOrLengthOrDummy;
}
};
#endif

////////////////////////////////////////////////////////////

namespace core {

CL_LAMBDA(&rest args);
CL_DECLARE();
CL_DOCSTRING("vector");
CL_DEFUN Vector_sp cl__vector(List_sp args) {
  SimpleVector_sp vec = SimpleVector_O::make(cl__length(args));
  vec->fillInitialContents(args);
  return vec;
};
SYMBOL_EXPORT_SC_(ClPkg, subtypep);

CL_LAMBDA(dimension initial_element initial_element_supplied_p);
CL_DECLARE();
CL_DOCSTRING("Make a (simple-vector t)");
CL_DEFUN SimpleVector_sp core__make_simple_vector_t(size_t dimension,
                                                    T_sp initialElement, bool initialElementSuppliedP) {
  (void)initialElementSuppliedP; // ignore
  return SimpleVector_O::make(dimension, initialElement, true);
}

#define DEFMAKESIMPLEVECTOR(TYPE, OBJECT, SMART)\
  CL_LAMBDA(dimension initial_element initial_element_supplied_p);\
  CL_DECLARE();\
  CL_DOCSTRING("Make a (simple-vector " #TYPE ")");\
  CL_DEFUN SMART core__make_simple_vector_##TYPE(size_t dimension, T_sp initialElement, bool initialElementSuppliedP) {\
    OBJECT::value_type init = OBJECT::initial_element_from_object(initialElement, initialElementSuppliedP);\
    return OBJECT::make(dimension, init, initialElementSuppliedP);\
  }

DEFMAKESIMPLEVECTOR(bit, SimpleBitVector_O, SimpleBitVector_sp);
DEFMAKESIMPLEVECTOR(base_char, SimpleBaseString_O, SimpleBaseString_sp);
DEFMAKESIMPLEVECTOR(character, SimpleCharacterString_O, SimpleCharacterString_sp);
DEFMAKESIMPLEVECTOR(single_float, SimpleVector_float_O, SimpleVector_float_sp);
DEFMAKESIMPLEVECTOR(double_float, SimpleVector_double_O, SimpleVector_double_sp);
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
CL_DOCSTRING("Make a (simple-array t) that is not a vector");
CL_DEFUN SimpleMDArrayT_sp core__make_simple_mdarray_t(List_sp dimensions,
                                                       T_sp initialElement, bool initialElementSuppliedP) {
  (void)initialElementSuppliedP;
  return SimpleMDArrayT_O::make_multi_dimensional(dimensions,initialElement,_Nil<T_O>());
}

#define DEFMAKESIMPLEMDARRAY(TYPE, OBJECT, SMART, SIMPLE)\
  CL_LAMBDA(dimension initial_element initial_element_supplied_p);\
  CL_DECLARE();\
  CL_DOCSTRING("Make a (simple-array " #TYPE ") that is not a vector");\
  CL_DEFUN SMART core__make_simple_mdarray_##TYPE(List_sp dimensions, T_sp initialElement, bool initialElementSuppliedP) {\
    SIMPLE::value_type init = SIMPLE::initial_element_from_object(initialElement, initialElementSuppliedP);\
    return OBJECT::make_multi_dimensional(dimensions, init, _Nil<T_O>());\
  }

DEFMAKESIMPLEMDARRAY(bit, SimpleMDArrayBit_O, SimpleMDArrayBit_sp, SimpleBitVector_O);
DEFMAKESIMPLEMDARRAY(base_char, SimpleMDArrayBaseChar_O, SimpleMDArrayBaseChar_sp, SimpleBaseString_O);
DEFMAKESIMPLEMDARRAY(character, SimpleMDArrayCharacter_O, SimpleMDArrayCharacter_sp, SimpleCharacterString_O);
DEFMAKESIMPLEMDARRAY(single_float, SimpleMDArray_float_O, SimpleMDArray_float_sp, SimpleVector_float_O);
DEFMAKESIMPLEMDARRAY(double_float, SimpleMDArray_double_O, SimpleMDArray_double_sp, SimpleVector_double_O);
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
CL_DOCSTRING("Makes a vector based on the arguments. See si_make_vector in ecl>>array.d");
CL_DEFUN Vector_sp core__make_vector(T_sp element_type,
                                     size_t dimension,
                                     bool adjustable,
                                     T_sp fillPointer,
                                     T_sp displacedTo,
                                     Fixnum_sp displacedIndexOffset,
                                     T_sp initialElement,
                                     bool initialElementSuppliedP) {
  if (fillPointer == cl::_sym_T_O) fillPointer = clasp_make_fixnum(dimension);
  if ( fillPointer.notnilp() || displacedTo.notnilp()) adjustable = true;
  if ( element_type == cl::_sym_T_O ) {
    if (adjustable) return ComplexVector_T_O::make(dimension,initialElement,fillPointer,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    // true because we MUST initialize vectors including pointers, such as simple vectors.
    return SimpleVector_O::make(dimension,initialElement,true);
  } else if (element_type == cl::_sym_bit) {
    SimpleBitVector_O::value_type init_bit = SimpleBitVector_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return BitVectorNs_O::make(dimension,init_bit,initialElementSuppliedP,fillPointer,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    return SimpleBitVector_O::make(dimension,init_bit,initialElementSuppliedP);
  } else if (element_type == cl::_sym_base_char
             || element_type == cl::_sym_character) {
    unlikely_if (element_type == cl::_sym_character) {
      claspCharacter initialCharacter = SimpleCharacterString_O::initial_element_from_object(initialElement,initialElementSuppliedP);
      // can't understand why displaced was set constantly to true, do as in the other cases with displacedTo.notnilp()
      if (adjustable) return StrWNs_O::make(dimension,initialCharacter,initialElementSuppliedP,fillPointer,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
      else return SimpleCharacterString_O::make(dimension,initialCharacter,initialElementSuppliedP,0,NULL,false);
    }
    claspChar initialChar = SimpleBaseString_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    // can't understand why displaced was set constantly to true, do as in the other cases with displacedTo.notnilp()
    if (adjustable) return Str8Ns_O::make(dimension,initialChar,initialElementSuppliedP,fillPointer,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    else return SimpleBaseString_O::make(dimension,initialChar,initialElementSuppliedP);
  } else if ( element_type == cl::_sym_double_float ) {
    double initialValue = SimpleVector_double_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return ComplexVector_double_O::make_vector(dimension,initialValue,fillPointer,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    return SimpleVector_double_O::make(dimension,initialValue,initialElementSuppliedP,0,NULL,false);
  } else if ( element_type == cl::_sym_single_float ) {
    float initialValue = SimpleVector_float_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return ComplexVector_float_O::make_vector(dimension,initialValue,fillPointer,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    return SimpleVector_float_O::make(dimension,initialValue,initialElementSuppliedP,0,NULL,false);

  } else if ( element_type == ext::_sym_integer8 ) {
    int8_t initialValue = SimpleVector_int8_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return ComplexVector_int8_t_O::make_vector(dimension,initialValue,fillPointer,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    return SimpleVector_int8_t_O::make(dimension,initialValue,initialElementSuppliedP,0,NULL,false);
  } else if ( element_type == ext::_sym_byte8 ) {
    byte8_t initialValue = SimpleVector_byte8_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return ComplexVector_byte8_t_O::make_vector(dimension,initialValue,fillPointer,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    return SimpleVector_byte8_t_O::make(dimension,initialValue,initialElementSuppliedP,0,NULL,false);

  } else if ( element_type == ext::_sym_integer16 ) {
    int16_t initialValue = SimpleVector_int16_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return ComplexVector_int16_t_O::make_vector(dimension,initialValue,fillPointer,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    return SimpleVector_int16_t_O::make(dimension,initialValue,initialElementSuppliedP,0,NULL,false);
  } else if ( element_type == ext::_sym_byte16 ) {
    byte16_t initialValue = SimpleVector_byte16_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return ComplexVector_byte16_t_O::make_vector(dimension,initialValue,fillPointer,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    return SimpleVector_byte16_t_O::make(dimension,initialValue,initialElementSuppliedP,0,NULL,false);

  } else if ( element_type == ext::_sym_integer32 ) {
    int32_t initialValue = SimpleVector_int32_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return ComplexVector_int32_t_O::make_vector(dimension,initialValue,fillPointer,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    return SimpleVector_int32_t_O::make(dimension,initialValue,initialElementSuppliedP,0,NULL,false);
  } else if ( element_type == ext::_sym_byte32 ) {
    byte32_t initialValue = SimpleVector_byte32_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return ComplexVector_byte32_t_O::make_vector(dimension,initialValue,fillPointer,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    return SimpleVector_byte32_t_O::make(dimension,initialValue,initialElementSuppliedP,0,NULL,false);

  } else if ( element_type == ext::_sym_integer64 ) {
    int64_t initialValue = SimpleVector_int64_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return ComplexVector_int64_t_O::make_vector(dimension,initialValue,fillPointer,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    return SimpleVector_int64_t_O::make(dimension,initialValue,initialElementSuppliedP,0,NULL,false);
  } else if ( element_type == ext::_sym_byte64 ) {
    byte64_t initialValue = SimpleVector_byte64_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return ComplexVector_byte64_t_O::make_vector(dimension,initialValue,fillPointer,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    return SimpleVector_byte64_t_O::make(dimension,initialValue,initialElementSuppliedP,0,NULL,false);

#if 0
  } else if ( element_type == ext::_sym_integer64 ) {
    int64_t initialValue = SimpleVector_int64_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return MDArray_int64_t_O::make_vector(dimension,initialValue,fillPointer,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    return SimpleVector_int64_t_O::make(dimension,initialValue,initialElementSuppliedP,0,NULL,false);
  } else if ( element_type == ext::_sym_byte64 ) {
    byte64_t initialValue = SimpleVector_byte64_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return MDArray_byte64_t_O::make_vector(dimension,initialValue,fillPointer,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    return SimpleVector_byte64_t_O::make(dimension,initialValue,initialElementSuppliedP,0,NULL,false);
#endif
    
  } else if ( element_type == _sym_size_t ) {
    size_t initialValue = SimpleVector_size_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return ComplexVector_size_t_O::make_vector(dimension,initialValue,fillPointer,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    return SimpleVector_size_t_O::make(dimension,initialValue,initialElementSuppliedP,0,NULL,false);

  } else if ( element_type == cl::_sym_fixnum ) {
    Fixnum initialValue = SimpleVector_fixnum_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return ComplexVector_fixnum_O::make_vector(dimension,initialValue,fillPointer,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    return SimpleVector_fixnum_O::make(dimension,initialValue,initialElementSuppliedP,0,NULL,false);

  }
  SIMPLE_ERROR(BF("Handle make-vector :element-type %s") % _rep_(element_type));
};


CL_LAMBDA(element_type dimension &optional initial_element initial_element_supplied_p);
CL_DECLARE();
CL_DOCSTRING("Makes a static vector based on the arguments. See si_make_vector in ecl>>array.d");
CL_DEFUN Vector_sp core__make_static_vector(T_sp element_type,
                                            size_t dimension,
                                            T_sp initialElement,
                                            bool initialElementSuppliedP) {
  if (element_type == cl::_sym_base_char
      || element_type == cl::_sym_character) {
    unlikely_if (element_type == cl::_sym_character) {
      claspCharacter initialCharacter = SimpleCharacterString_O::initial_element_from_object(initialElement,initialElementSuppliedP);
      return SimpleCharacterString_O::make(dimension,initialCharacter,initialElementSuppliedP,0,NULL,true);
    }
    claspChar initialChar = SimpleBaseString_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    return SimpleBaseString_O::make(dimension,initialChar,initialElementSuppliedP,0,NULL,true);
  } else if ( element_type == cl::_sym_double_float ) {
    double initialValue = SimpleVector_double_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    return SimpleVector_double_O::make(dimension,initialValue,initialElementSuppliedP,0,NULL,true);
  } else if ( element_type == cl::_sym_single_float ) {
    float initialValue = SimpleVector_float_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    return SimpleVector_float_O::make(dimension,initialValue,initialElementSuppliedP,0,NULL,true);

  } else if ( element_type == ext::_sym_integer8 ) {
    int8_t initialValue = SimpleVector_int8_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    return SimpleVector_int8_t_O::make(dimension,initialValue,initialElementSuppliedP,0,NULL,true);
  } else if ( element_type == ext::_sym_byte8 ) {
    byte8_t initialValue = SimpleVector_byte8_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    return SimpleVector_byte8_t_O::make(dimension,initialValue,initialElementSuppliedP,0,NULL,true);

  } else if ( element_type == ext::_sym_integer16 ) {
    int16_t initialValue = SimpleVector_int16_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    return SimpleVector_int16_t_O::make(dimension,initialValue,initialElementSuppliedP,0,NULL,true);
  } else if ( element_type == ext::_sym_byte16 ) {
    byte16_t initialValue = SimpleVector_byte16_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    return SimpleVector_byte16_t_O::make(dimension,initialValue,initialElementSuppliedP,0,NULL,true);

  } else if ( element_type == ext::_sym_integer32 ) {
    int32_t initialValue = SimpleVector_int32_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    return SimpleVector_int32_t_O::make(dimension,initialValue,initialElementSuppliedP,0,NULL,true);
  } else if ( element_type == ext::_sym_byte32 ) {
    byte32_t initialValue = SimpleVector_byte32_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    return SimpleVector_byte32_t_O::make(dimension,initialValue,initialElementSuppliedP,0,NULL,true);

  } else if ( element_type == ext::_sym_integer64 ) {
    int64_t initialValue = SimpleVector_int64_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    return SimpleVector_int64_t_O::make(dimension,initialValue,initialElementSuppliedP,0,NULL,true);
  } else if ( element_type == ext::_sym_byte64 ) {
    byte64_t initialValue = SimpleVector_byte64_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    return SimpleVector_byte64_t_O::make(dimension,initialValue,initialElementSuppliedP,0,NULL,true);

  } else if ( element_type == ext::_sym_integer64 ) {
    int64_t initialValue = SimpleVector_int64_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    return SimpleVector_int64_t_O::make(dimension,initialValue,initialElementSuppliedP,0,NULL,true);
  } else if ( element_type == ext::_sym_byte64 ) {
    byte64_t initialValue = SimpleVector_byte64_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    return SimpleVector_byte64_t_O::make(dimension,initialValue,initialElementSuppliedP,0,NULL,true);

  } else if ( element_type == _sym_size_t ) {
    size_t initialValue = SimpleVector_size_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    return SimpleVector_size_t_O::make(dimension,initialValue,initialElementSuppliedP,0,NULL,true);

  } else if ( element_type == cl::_sym_fixnum ) {
    Fixnum initialValue = SimpleVector_fixnum_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    return SimpleVector_fixnum_O::make(dimension,initialValue,initialElementSuppliedP,0,NULL,true);

  }
  SIMPLE_ERROR(BF("Handle make-static-vector :element-type %s") % _rep_(element_type));
};

CL_LAMBDA(dimensions element_type adjustable displaced_to displaced_index_offset initial_element initial_element_supplied_p);
CL_DECLARE();
CL_DOCSTRING("Makes a multidimensional array based on the arguments.");
CL_DEFUN MDArray_sp core__make_mdarray(List_sp dimensions,
                                       T_sp element_type,
                                       bool adjustable,
                                       T_sp displacedTo,
                                       Fixnum_sp displacedIndexOffset,
                                       T_sp initialElement,
                                       bool initialElementSuppliedP) {
  if (displacedTo.notnilp()) adjustable = true;
#define MAKE(multi, simple)\
  simple::value_type init = simple::initial_element_from_object(initialElement,initialElementSuppliedP);\
  if (adjustable) return multi::make_multi_dimensional(dimensions,initialElement,displacedTo,displacedTo.notnilp(),displacedIndexOffset);\
  else return Simple##multi::make_multi_dimensional(dimensions,initialElement,_Nil<T_O>());
  // macro over
  if (element_type == cl::_sym_T_O) { MAKE(MDArrayT_O, SimpleVector_O) }
  else if (element_type == cl::_sym_double_float) { MAKE(MDArray_double_O, SimpleVector_double_O) }
  else if (element_type == cl::_sym_single_float) { MAKE(MDArray_float_O, SimpleVector_float_O) }
  else if (element_type == cl::_sym_bit) { MAKE(MDArrayBit_O, SimpleBitVector_O) }
  else if (element_type == cl::_sym_base_char) { MAKE(MDArrayBaseChar_O, SimpleBaseString_O) }
  else if (element_type == cl::_sym_character) { MAKE(MDArrayCharacter_O, SimpleCharacterString_O) }
  else if (element_type == ext::_sym_integer8) { MAKE(MDArray_int8_t_O, SimpleVector_int8_t_O) }
  else if (element_type == ext::_sym_byte8) { MAKE(MDArray_byte8_t_O, SimpleVector_byte8_t_O) }
  else if (element_type == ext::_sym_integer16) { MAKE(MDArray_int16_t_O, SimpleVector_int16_t_O) }
  else if (element_type == ext::_sym_byte16) { MAKE(MDArray_byte16_t_O, SimpleVector_byte16_t_O) }
  else if (element_type == ext::_sym_integer32) { MAKE(MDArray_int32_t_O, SimpleVector_int32_t_O) }
  else if (element_type == ext::_sym_byte32) { MAKE(MDArray_byte32_t_O, SimpleVector_byte32_t_O) }
  else if (element_type == ext::_sym_integer64) { MAKE(MDArray_int8_t_O, SimpleVector_int64_t_O) }
  else if (element_type == ext::_sym_byte64) { MAKE(MDArray_byte64_t_O, SimpleVector_byte64_t_O) }
  else if (element_type == _sym_size_t) { MAKE(MDArray_size_t_O, SimpleVector_size_t_O) }
  else if (element_type == cl::_sym_fixnum) { MAKE(MDArray_fixnum_O, SimpleVector_fixnum_O) }
#undef MAKE
  else SIMPLE_ERROR(BF("Handle creation of multi-dimensional array of type %s") % _rep_(element_type));
};

// ------------------------------------------------------------
// ------------------------------------------------------------

CL_LAMBDA(newElement vector);
CL_DECLARE();
CL_DOCSTRING("vectorPush");
CL_DEFUN T_sp cl__vector_push(T_sp newElement, Vector_sp vec) {
  return vec->vectorPush(newElement);
};

CL_LAMBDA(newElement vector &optional (exension 0));
CL_DECLARE();
CL_DOCSTRING("vectorPushExtend");
CL_DEFUN Fixnum_sp cl__vector_push_extend(T_sp newElement, Vector_sp vec, size_t extension) {
  return vec->vectorPushExtend(newElement, extension);
}

CL_DEFUN void core__verify_simple_vector_layout(size_t length_offset, size_t data_offset)
{
  size_t cxx_length_offset = offsetof(SimpleVector_O,_Data._Length);
  size_t cxx_data_offset = offsetof(SimpleVector_O,_Data._Data);
  if (length_offset!=cxx_length_offset)
    SIMPLE_ERROR(BF("length_offset %lu does not match cxx_length_offset %lu") % length_offset % cxx_length_offset );
  if (data_offset!=cxx_data_offset)
    SIMPLE_ERROR(BF("data_offset %lu does not match cxx_data_offset %lu") % data_offset % cxx_data_offset );
}

SYMBOL_EXPORT_SC_(KeywordPkg,vtable);
SYMBOL_EXPORT_SC_(KeywordPkg,FillPointerOrLengthOrDummy);
SYMBOL_EXPORT_SC_(KeywordPkg,ArrayTotalSize);
SYMBOL_EXPORT_SC_(KeywordPkg,Data);
SYMBOL_EXPORT_SC_(KeywordPkg,DisplacedIndexOffset);
SYMBOL_EXPORT_SC_(KeywordPkg,Flags);
SYMBOL_EXPORT_SC_(KeywordPkg,Rank);
SYMBOL_EXPORT_SC_(KeywordPkg,Dimensions);

CL_DEFUN void core__verify_mdarray_layout(T_sp alist)
{
  expect_offset(kw::_sym_FillPointerOrLengthOrDummy,alist,offsetof(MDArray_O,_FillPointerOrLengthOrDummy)-gctools::general_tag);
  expect_offset(kw::_sym_ArrayTotalSize,alist,offsetof(MDArray_O,_ArrayTotalSize)-gctools::general_tag);
  expect_offset(kw::_sym_Data,alist,offsetof(MDArray_O,_Data)-gctools::general_tag);
  expect_offset(kw::_sym_DisplacedIndexOffset,alist,offsetof(MDArray_O,_DisplacedIndexOffset)-gctools::general_tag);
  expect_offset(kw::_sym_Flags,alist,offsetof(MDArray_O,_Flags)-gctools::general_tag);
  expect_offset(kw::_sym_Rank,alist,offsetof(MDArray_O,_Dimensions._Length)-gctools::general_tag);
  expect_offset(kw::_sym_Dimensions,alist,offsetof(MDArray_O,_Dimensions._Data)-gctools::general_tag);
}

SYMBOL_SC_(CorePkg, make_vector);
SYMBOL_EXPORT_SC_(CorePkg, adjustVector);
SYMBOL_EXPORT_SC_(ClPkg, vectorPush);
SYMBOL_EXPORT_SC_(ClPkg, vectorPushExtend);

// Create a byte8 simple vector from any array
CL_DEFUN Array_sp core__coerce_to_byte8_vector(T_sp object)
{
  if (gc::IsA<Array_sp>(object)) {
    Array_sp source = gc::As_unsafe<Array_sp>(object);
    T_sp element_type = source->element_type();
    if (element_type == cl::_sym_single_float) {
      AbstractSimpleVector_sp asv;
      size_t start, end;
      source->asAbstractSimpleVectorRange(asv,start,end);
      const unsigned char* memory_start = reinterpret_cast<const unsigned char*>(asv->rowMajorAddressOfElement_(start));
      const unsigned char* memory_end = reinterpret_cast<const unsigned char*>(asv->rowMajorAddressOfElement_(end));
      SimpleVector_byte8_t_sp result = SimpleVector_byte8_t_O::make((memory_end-memory_start),0,false,(memory_end-memory_start),memory_start);
      return result;
    } else if (element_type == cl::_sym_base_char) {
      AbstractSimpleVector_sp asv;
      size_t start, end;
      source->asAbstractSimpleVectorRange(asv,start,end);
      const unsigned char* memory_start = reinterpret_cast<const unsigned char*>(asv->rowMajorAddressOfElement_(start));
      const unsigned char* memory_end = reinterpret_cast<const unsigned char*>(asv->rowMajorAddressOfElement_(end));
      SimpleVector_byte8_t_sp result = SimpleVector_byte8_t_O::make((memory_end-memory_start),0,false,(memory_end-memory_start),memory_start);
      return result;
    }
    SIMPLE_ERROR(BF("Add support for coercing %s to a byte8 vector") % _rep_(source));
  } else if (gc::IsA<clasp_ffi::ForeignData_sp>(object)) {
    clasp_ffi::ForeignData_sp source = gc::As_unsafe<clasp_ffi::ForeignData_sp>(object);
    SimpleVector_byte8_t_sp result = SimpleVector_byte8_t_O::make(source->foreign_data_size(),0,false,source->foreign_data_size(),(const unsigned char*)source->orig_data_ptr());
    return result;
  }
  SIMPLE_ERROR(BF("Add support for coercing %s to a byte8 vector") % _rep_(object));
}

// Create a base-char simple vector from any array
CL_DEFUN clasp_ffi::ForeignData_sp core__coerce_memory_to_foreign_data(Array_sp source)
{
  T_sp element_type = source->element_type();
  if (element_type == cl::_sym_single_float) {
    AbstractSimpleVector_sp asv;
    size_t start, end;
    source->asAbstractSimpleVectorRange(asv,start,end);
    const unsigned char* memory_start = reinterpret_cast<const unsigned char*>(asv->rowMajorAddressOfElement_(start));
    const unsigned char* memory_end = reinterpret_cast<const unsigned char*>(asv->rowMajorAddressOfElement_(end));
    GC_ALLOCATE(clasp_ffi::ForeignData_O,data);
    data->allocate(ext::_sym_byte8,DeleteOnDtor,memory_end-memory_start);
    memcpy(const_cast<void*>(data->orig_data_ptr()),(void*)memory_start,memory_end-memory_start);
    return data;
  }
  SIMPLE_ERROR(BF("Add support for coercing %s to a simple-base-string") % _rep_(source));
}

CL_DEFUN clasp_ffi::ForeignData_sp core__static_vector_address(Array_sp source)
{
  return clasp_ffi::ForeignData_O::create(source->rowMajorAddressOfElement_(0));
};

CL_DEFUN clasp_ffi::ForeignData_sp core__static_vector_pointer(Array_sp source, size_t offset )
{
  return clasp_ffi::ForeignData_O::create((char*)source->rowMajorAddressOfElement_(0)+offset);
}

CL_DOCSTRING("Return the simple-vector that stores the data for this array - this is like sbcl sb-ext:array-storage-vector");
CL_DEFUN Array_sp ext__array_storage_vector(Array_sp source )
{
  if (source->displacedToP()) {
    SIMPLE_ERROR(BF("array-storage-vector cannot be used with displaced arrays"));
  }
  AbstractSimpleVector_sp bsv;
  size_t ostart, oend;
  ASSERT(ostart==0);
  source->asAbstractSimpleVectorRange(bsv,ostart,oend);
  return bsv;
}

CL_DOCSTRING("Return a pointer to the data in the array source");
CL_DEFUN clasp_ffi::ForeignData_sp ext__array_pointer(Array_sp source )
{
  return clasp_ffi::ForeignData_O::create(source->rowMajorAddressOfElement_(0));
}

CL_DOCSTRING("Pin the objects in the list in memory and then call the thunk");
CL_DEFUN T_mv ext__pinned_objects_funcall(List_sp objects, T_sp thunk)
{
  size_t num = cl__length(objects);
  T_O* pointerArray[num];
  size_t idx = 0;
  for ( auto cur : objects ) {
    T_sp obj = CONS_CAR(cur);
    pointerArray[idx] = obj.raw_();
    idx++;
  }
  return eval::funcall(thunk);
}
  
  
}; /* core */
