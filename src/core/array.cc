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

#include <stdio.h>
#include <string.h>
#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/bformat.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/numbers.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/primitives.h>
#include <clasp/core/designators.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/array.h>
#include <clasp/core/character.h>
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

extern "C" {
using namespace core;
Character_sp SimpleBaseString_get(SimpleBaseString_sp array, Fixnum_sp idx) {
  return clasp_make_character((*array)[idx.unsafe_fixnum()]);
}
void SimpleBaseString_set(SimpleBaseString_sp array, Fixnum_sp idx, Character_sp c) {
  (*array)[idx.unsafe_fixnum()] = c.unsafe_character();
}
};

namespace core {
void bitVectorDoesntSupportError() {
  SIMPLE_ERROR(BF("You tried to invoke a method that bit-vector doesn't support on a bit-vector"));
}
void missingValueListError(List_sp indices) {
  SIMPLE_ERROR(BF("The value was missing after the indices %s") % _rep_(indices));
}
void tooManyIndicesListError(List_sp indices) {
  SIMPLE_ERROR(BF("Too many indices %s") % _rep_(indices));
}
void badAxisNumberError(Symbol_sp fn_name, size_t rank, size_t axisNumber) {
  SIMPLE_ERROR(BF("In %s illegal axis number %d must be less than rank %d") % _rep_(fn_name) % axisNumber % rank );
}
void badIndexError(size_t oneIndex, size_t curDimension) {
  SIMPLE_ERROR(BF("The index %d must be less than %d") % oneIndex % curDimension );
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
void vectorNotArrayError(Symbol_sp fn_name, T_sp array) {
  SIMPLE_ERROR(BF("In %s - vector is not an array") % _rep_(fn_name));
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

// -----------------------------------------------------------
//
// Utility functions
//

bool ranged_bit_vector_EQ_(const SimpleBitVector_O& bvx,const SimpleBitVector_O& bvy, size_t startx, size_t endx, size_t starty, size_t endy) {
  size_t lenx = endx - startx;
  size_t leny = endy - starty;
  if (lenx!=leny) return false;
  for (size_t ix(startx), iy(starty); ix<endx; ++ix, ++iy ) {
    if (bvx.testBit(ix) != bvy.testBit(iy)) return false;
  }
  return true;
}

Array_sp ranged_bit_vector_reverse(SimpleBitVector_sp sv, size_t start, size_t end) {
  gctools::smart_ptr<SimpleBitVector_O> rev = SimpleBitVector_O::make(end-start);
  for (size_t i(0),iEnd(end-start); i<iEnd; ++i ) {
    rev->setBit(i,sv->testBit(end-1-i));
  }
  return rev;
}

void ranged_bit_vector_nreverse(SimpleBitVector_sp sv, size_t start, size_t end) {
  size_t middle = (end-start)/2;
  for (size_t i(start),iEnd(start+end-1),iOff(0); i<middle; ++i,--iEnd ) {
    uint x = sv->testBit(i);
    uint y = sv->testBit(iEnd);
    sv->setBit(iEnd,x);
    sv->setBit(i,y);
  }
}

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
        size_t oneIndex = index.unsafe_fixnum();
        unlikely_if (oneIndex < 0 || oneIndex >= curDimension) {
          badIndexError(oneIndex, curDimension);
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
      size_t oneIndex = one.unsafe_fixnum();
      unlikely_if (oneIndex < 0 || oneIndex >= curDimension) {
        badIndexError(oneIndex, curDimension);
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
  if (idx >= array->rank()) {
    SIMPLE_ERROR(BF("array-dimension index %d is out of bounds - must be less than %d") % idx % array->rank());
  }
  return array->arrayDimension(idx);
}

CL_LISPIFY_NAME("core:data-vector-p");
CL_DEFUN bool core__data_vector_p(T_sp obj)
{
  return gc::IsA<AbstractSimpleVector_sp>(obj);
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
  return array->arrayElementType();
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

CL_LISPIFY_NAME("core:array-fill");
CL_DEFUN void core__arrayFill(Array_sp array, T_sp val)
{
  array->unsafe_fillArrayWithElt(val,0,array->arrayTotalSize());
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



// ----------------------------------------------------------------------
//
//  String functions
//


namespace core {

static bool member_charbag(claspCharacter c, SEQUENCE_sp char_bag) {
  if (char_bag.nilp())
    return false;
  if (Cons_sp clcur = char_bag.asOrNull<Cons_O>()) {
    List_sp lcur = clcur;
    for (; lcur.notnilp(); lcur = oCdr(lcur)) {
      if (cl__eql(oCar(lcur), clasp_make_character(c)))
        return true;
    }
  } else if (Vector_sp vcur = char_bag.asOrNull<Vector_O>()) {
    for (size_t i = 0, iEnd(vcur->length()); i < iEnd; ++i) {
      if (cl__eql(vcur->rowMajorAref(i),clasp_make_character(c)))
        return true;
    }
  }
  return false;
}

static String_sp string_trim0(bool left_trim, bool right_trim, T_sp char_bag, T_sp tstrng) {
  cl_index i, j;
  String_sp strng = coerce::stringDesignator(tstrng);
  i = 0;
  j = cl__length(strng);
  if (left_trim) {
    for (; i < j; i++) {
      claspCharacter c = cl__char(strng,i).unsafe_character();
      if (!member_charbag(c, char_bag))
        break;
    }
  }
  if (right_trim) {
    for (; j > i; j--) {
      claspCharacter c = cl__char(strng,j - 1).unsafe_character();
      if (!member_charbag(c, char_bag)) {
        break;
      }
    }
  }
  return strng->unsafe_subseq(i, j);
}

CL_LAMBDA(charbag str);
CL_DECLARE();
CL_DOCSTRING("string_trim");
CL_DEFUN String_sp cl__string_trim(T_sp charbag, T_sp str) {
  return string_trim0(true, true, charbag, str);
};

CL_LAMBDA(charbag str);
CL_DECLARE();
CL_DOCSTRING("string_left_trim");
CL_DEFUN String_sp cl__string_left_trim(T_sp charbag, T_sp str) {
  return string_trim0(true, false, charbag, str);
};

CL_LAMBDA(charbag str);
CL_DECLARE();
CL_DOCSTRING("string_right_trim");
CL_DEFUN String_sp cl__string_right_trim(T_sp charbag, T_sp str) {
  return string_trim0(false, true, charbag, str);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("string");
CL_DEFUN String_sp cl__string(T_sp arg) {
  String_sp result = coerce::stringDesignator(arg);
  return (result);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("string_upcase");
CL_DEFUN SimpleString_sp cl__string_upcase(T_sp arg) {
  String_sp str = coerce::stringDesignator(arg);
  SimpleString_sp result = gc::As_unsafe<SimpleString_sp>(core__make_vector(str->arrayElementType(),str->length(),false));
  for ( size_t i(0), iEnd(str->length()); i<iEnd; ++i ) {
    T_sp cc = str->rowMajorAref(i);
    claspCharacter c = cc.unsafe_character();
    claspCharacter u = claspCharacter_upcase(c);
    Character_sp cu = clasp_make_character(u);
    result->rowMajorAset(i,clasp_make_character(u));
  }
  return (result);
};


CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("string_downcase");
CL_DEFUN SimpleString_sp cl__string_downcase(T_sp arg) {
  String_sp str = coerce::stringDesignator(arg);
  SimpleString_sp result = gc::As_unsafe<SimpleString_sp>(core__make_vector(str->arrayElementType(),str->length(),false));
  for ( size_t i(0), iEnd(str->length()); i<iEnd; ++i ) {
    claspCharacter c = str->rowMajorAref(i).unsafe_character();
    claspCharacter u = claspCharacter_downcase(c);
    result->rowMajorAset(i,clasp_make_character(u));
  }
  return (result);
};


CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("nstring_upcase");
CL_DEFUN String_sp cl__nstring_upcase(String_sp arg) {
  for ( cl_index i(0), iEnd(arg->length()); i<iEnd; ++i ) {
    arg->rowMajorAset(i,clasp_make_character(claspCharacter_upcase(arg->rowMajorAref(i).unsafe_character())));
  }
  return arg;
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("nstring_downcase");
CL_DEFUN String_sp cl__nstring_downcase(String_sp arg) {
  for ( cl_index i(0), iEnd(arg->length()); i<iEnd; ++i ) {
    arg->rowMajorAset(i,clasp_make_character(claspCharacter_downcase(arg->rowMajorAref(i).unsafe_character())));
  }
  return arg;
};



bool clasp_memberChar(claspChar c, String_sp charBag) {
  for ( cl_index i(0), iEnd(charBag->length()); i<iEnd; ++i ) {
    if (charBag->rowMajorAref(i).unsafe_character() == c) return true;
  }
  return false;
}

// ----------------------------------------------------------------------
//


SYMBOL_EXPORT_SC_(ClPkg, string);
SYMBOL_EXPORT_SC_(ClPkg, string_upcase);
SYMBOL_EXPORT_SC_(ClPkg, string_downcase);
SYMBOL_EXPORT_SC_(ClPkg, nstring_upcase);
SYMBOL_EXPORT_SC_(ClPkg, nstring_downcase);
SYMBOL_EXPORT_SC_(ClPkg, stringTrim);
SYMBOL_EXPORT_SC_(ClPkg, stringLeftTrim);
SYMBOL_EXPORT_SC_(ClPkg, stringRightTrim);
SYMBOL_EXPORT_SC_(ClPkg, char);

template <typename T>
struct StringCharPointer {
  const T* _stringPtr;
  size_t _pos;
  size_t _start;
  typedef typename T::simple_element_type CharacterType;
  StringCharPointer(const T* strP, size_t start) : _stringPtr(strP), _start(start), _pos(start) {}
  inline size_t offset() { return this->_pos - this->_start;};
  CharacterType operator*() {
    CharacterType c = (*this->_stringPtr)[this->_pos];
    return c;
  }
void* address() { return (void*)&((*this->_stringPtr)[this->_pos]);}
StringCharPointer& operator++() {
  ++this->_pos;
  return *this;
}
};


template <typename T1, typename T2>
bool template_string_equalp_bool(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2) {
  StringCharPointer<T1> cp1(&string1,start1);
  StringCharPointer<T2> cp2(&string2,start2);
  size_t num1 = end1 - start1;
  size_t num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    if ((toupper(static_cast<claspCharacter>(*cp1)) != toupper(static_cast<claspCharacter>(*cp2))))
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




/*! bounding index designator range from 0 to the end of each string */
template <typename T1,typename T2>
T_sp template_string_EQ_(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2)
{
  StringCharPointer<T1> cp1(&string1,start1);
  StringCharPointer<T2> cp2(&string2,start2);
  size_t num1 = end1 - start1;
  size_t num2 = end2 - start2;
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
  return _Nil<T_O>();
 RETURN_TRUE:
  return _lisp->_true();
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_NE_(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2)
{
  StringCharPointer<T1> cp1(&string1,start1);
  StringCharPointer<T2> cp2(&string2,start2);
  size_t num1 = end1 - start1;
  size_t num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    if ((static_cast<claspCharacter>(*cp1) != static_cast<claspCharacter>(*cp2)))
      goto RETURN_TRUE;
    --num1;
    --num2;
    ++cp1;
    ++cp2;
  }
 END_STRING1:
  if (num2 != 0)
    goto RETURN_TRUE;
  goto RETURN_FALSE;
 END_STRING2: // Did not hit end of string 1 at this point
 RETURN_TRUE: // strings are not equal
  return make_fixnum((int)(cp1.offset() + start1));
 RETURN_FALSE:
  return _Nil<T_O>();
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_LT_(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2) {
  StringCharPointer<T1> cp1(&string1,start1);
  StringCharPointer<T2> cp2(&string2,start2);
  size_t num1 = end1 - start1;
  size_t num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    if ((static_cast<claspCharacter>(*cp1) != static_cast<claspCharacter>(*cp2))) {
      if (static_cast<claspCharacter>(*cp1) < static_cast<claspCharacter>(*cp2))
        goto RETURN_TRUE;
      goto RETURN_FALSE;
    }
    --num1;
    --num2;
    ++cp1;
    ++cp2;
  }
 END_STRING1:
  if (num2 == 0)
    goto RETURN_FALSE;
 RETURN_TRUE:
  return make_fixnum((int)(cp1.offset() + start1));
 END_STRING2:
 RETURN_FALSE:
  return _Nil<T_O>();
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_GT_(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2) {
  StringCharPointer<T1> cp1(&string1,start1);
  StringCharPointer<T2> cp2(&string2,start2);
  size_t num1 = end1 - start1;
  size_t num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    if ((static_cast<claspCharacter>(*cp1) != static_cast<claspCharacter>(*cp2))) {
      if (static_cast<claspCharacter>(*cp1) > static_cast<claspCharacter>(*cp2))
        goto RETURN_TRUE;
      goto RETURN_FALSE;
    }
    --num1;
    --num2;
    ++cp1;
    ++cp2;
  }
 END_STRING1:
 RETURN_FALSE:
  return _Nil<T_O>();
 END_STRING2:
 RETURN_TRUE:
  return make_fixnum((int)(cp1.offset() + start1));
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_LE_(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2) {
  StringCharPointer<T1> cp1(&string1,start1);
  StringCharPointer<T2> cp2(&string2,start2);
  size_t num1 = end1 - start1;
  size_t num2 = end2 - start2;
  // the empty string is le any other string
  if (num1 == 0) goto RETURN_TRUE;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    if ((static_cast<claspCharacter>(*cp1) != static_cast<claspCharacter>(*cp2))) {
      if (static_cast<claspCharacter>(*cp1) < static_cast<claspCharacter>(*cp2))
        goto RETURN_TRUE;
      goto RETURN_FALSE;
    }
    --num1;
    --num2;
    ++cp1;
    ++cp2;
  }
 END_STRING1:
    goto RETURN_TRUE;
 END_STRING2:
 RETURN_FALSE:
  return _Nil<T_O>();
 RETURN_TRUE:
  return make_fixnum((int)(cp1.offset() + start1));
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_GE_(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2) {
  StringCharPointer<T1> cp1(&string1,start1);
  StringCharPointer<T2> cp2(&string2,start2);
  size_t num1 = end1 - start1;
  size_t num2 = end2 - start2;
  // Any String is ge the empty string
  if (num2 == 0) goto RETURN_TRUE;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    if ((static_cast<claspCharacter>(*cp1) != static_cast<claspCharacter>(*cp2))) {
      if (static_cast<claspCharacter>(*cp1) > static_cast<claspCharacter>(*cp2))
        goto RETURN_TRUE;
      goto RETURN_FALSE;
    }
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
  // String1 still has chars, string2 not
  goto RETURN_TRUE;
 RETURN_FALSE:
  return _Nil<T_O>();
 RETURN_TRUE:
  return make_fixnum((int)(cp1.offset() + start1));
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_equal(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2) {
  StringCharPointer<T1> cp1(&string1,start1);
  StringCharPointer<T2> cp2(&string2,start2);
  size_t num1 = end1 - start1;
  size_t num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    if ((toupper(static_cast<claspCharacter>(*cp1)) != toupper(static_cast<claspCharacter>(*cp2))))
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
  return _Nil<T_O>();
 RETURN_TRUE:
  return _lisp->_true();
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_not_equal(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2) {
  StringCharPointer<T1> cp1(&string1,start1);
  StringCharPointer<T2> cp2(&string2,start2);
  size_t num1 = end1 - start1;
  size_t num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    if ((toupper(static_cast<claspCharacter>(*cp1)) != toupper(static_cast<claspCharacter>(*cp2))))
      goto RETURN_TRUE;
    --num1;
    --num2;
    ++cp1;
    ++cp2;
  }
 END_STRING1:
  if (num2 != 0)
    goto RETURN_TRUE;
  goto RETURN_FALSE;
 END_STRING2: // Did not hit end of string 1 at this point
 RETURN_TRUE: // strings are not equal
  return make_fixnum((int)(cp1.offset() + start1)); //_lisp->_true();
 RETURN_FALSE:
  return _Nil<T_O>();
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_lessp(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2) {
  StringCharPointer<T1> cp1(&string1,start1);
  StringCharPointer<T2> cp2(&string2,start2);
  size_t num1 = end1 - start1;
  size_t num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    claspCharacter ucp1 = toupper(static_cast<claspCharacter>(*cp1));
    claspCharacter ucp2 = toupper(static_cast<claspCharacter>(*cp2));
    if (ucp1 != ucp2) {
      if (ucp1 < ucp2)
        goto RETURN_TRUE;
      goto RETURN_FALSE;
    }
    --num1;
    --num2;
    ++cp1;
    ++cp2;
  }
 END_STRING1:
  if (num2 == 0)
    goto RETURN_FALSE;
 RETURN_TRUE:
  return make_fixnum((int)(cp1.offset() + start1));
 END_STRING2:
 RETURN_FALSE:
  return _Nil<T_O>();
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_greaterp(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2) {
  StringCharPointer<T1> cp1(&string1,start1);
  StringCharPointer<T2> cp2(&string2,start2);
  size_t num1 = end1 - start1;
  size_t num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    claspCharacter ucp1 = toupper(static_cast<claspCharacter>(*cp1));
    claspCharacter ucp2 = toupper(static_cast<claspCharacter>(*cp2));
    if ((ucp1 != ucp2)) {
      if (ucp1 > ucp2)
        goto RETURN_TRUE;
      goto RETURN_FALSE;
    }
    --num1;
    --num2;
    ++cp1;
    ++cp2;
  }
 END_STRING1:
 RETURN_FALSE:
  return _Nil<T_O>();
 END_STRING2:
 RETURN_TRUE:
  return make_fixnum((int)(cp1.offset() + start1));
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_not_greaterp(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2) {
  StringCharPointer<T1> cp1(&string1,start1);
  StringCharPointer<T2> cp2(&string2,start2);
  size_t num1 = end1 - start1;
  size_t num2 = end2 - start2;
  // The empty String is not greater than any other string, even another empty string
  if (num1 == 0) goto RETURN_TRUE;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    claspCharacter ucp1 = toupper(static_cast<claspCharacter>(*cp1));
    claspCharacter ucp2 = toupper(static_cast<claspCharacter>(*cp2));
    if ((ucp1 != ucp2)) {
      if (ucp1 < ucp2)
        goto RETURN_TRUE;
      goto RETURN_FALSE;
    }
    --num1;
    --num2;
    ++cp1;
    ++cp2;
  }
 END_STRING1:
    goto RETURN_TRUE;
 END_STRING2:
 RETURN_FALSE:
  return _Nil<T_O>();
 RETURN_TRUE:
  return make_fixnum((int)(cp1.offset() + start1));
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_not_lessp(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2) {
  StringCharPointer<T1> cp1(&string1,start1);
  StringCharPointer<T2> cp2(&string2,start2);
  size_t num1 = end1 - start1;
  size_t num2 = end2 - start2;
  // No String is lessp the empty string
  // So every String is not-lessp the empty string
  if (num2 == 0) goto RETURN_TRUE;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    claspCharacter ucp1 = toupper(static_cast<claspCharacter>(*cp1));
    claspCharacter ucp2 = toupper(static_cast<claspCharacter>(*cp2));
    if ((ucp1 != ucp2)) {
      if (ucp1 > ucp2)
        goto RETURN_TRUE;
      goto RETURN_FALSE;
    }
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
  //String2 is consumed, String1 not yet
  goto RETURN_TRUE;
 RETURN_FALSE:
  return _Nil<T_O>();
 RETURN_TRUE:
  return make_fixnum((int)(cp1.offset() + start1));
}

inline void setup_string_op_arguments(T_sp string1_desig, T_sp string2_desig,
                                      String_sp &string1, String_sp &string2,
                                      Fixnum_sp start1, T_sp end1,
                                      Fixnum_sp start2, T_sp end2,
                                      size_t &istart1, size_t &iend1,
                                      size_t &istart2, size_t &iend2) {
  string1 = coerce::stringDesignator(string1_desig);
  string2 = coerce::stringDesignator(string2_desig);
  Fixnum fstart1 = unbox_fixnum(start1);
  if (fstart1 < 0)
      // a negative start1 should error
    SIMPLE_ERROR(BF("start1 %d out of bounds for string %s") % fstart1 % string1);
  istart1 = MAX(fstart1, 0);
  if (istart1 > cl__length(string1)) {
    SIMPLE_ERROR(BF("start1 %d out of bounds for string %s") % istart1 % string1);
  }
  iend1 = MIN(end1.nilp() ? cl__length(string1) : unbox_fixnum(gc::As<Fixnum_sp>(end1)), cl__length(string1));
  Fixnum fstart2 = unbox_fixnum(start2);
  if (fstart2 <0)
    // a negative start2 should error
    SIMPLE_ERROR(BF("start2 %d out of bounds for string %s") % fstart2 % string2);
  istart2 = MAX(fstart2, 0);
  if (istart2 > cl__length(string2)) {
    SIMPLE_ERROR(BF("start2 %d out of bounds for string %s") % istart2 % string2);
  }
  iend2 = MIN(end2.nilp() ? cl__length(string2) : unbox_fixnum(gc::As<Fixnum_sp>(end2)), cl__length(string2));
}

#define TEMPLATE_SINGLE_STRING_DISPATCHER(_string_,_function_,istart,iend) \
  if (gc::IsA<SimpleString_sp>(_string_)) {				\
    if (gc::IsA<SimpleBaseString_sp>(_string_)) { \
      auto sbcs2 = gc::As_unsafe<SimpleBaseString_sp>(_string_); \
      return _function_(*sbcs2,istart,iend); \
    } else {							\
      auto scs2 = gc::As_unsafe<SimpleCharacterString_sp>(_string_); \
      return _function_(*scs2,istart,iend); \
    } \
  } else { \
    if (gc::IsA<Str8Ns_sp>(_string_)) { \
      auto ns82 = gc::As_unsafe<Str8Ns_sp>(_string_); \
      return _function_(*ns82,istart,iend); \
    } else { \
      auto nsw2 = gc::As_unsafe<StrWNs_sp>(_string_); \
      return _function_(*nsw2,istart,iend); \
    } \
  }

#define TEMPLATE_HALF_STRING_DISPATCHER(_this_,_string2_,_function_,istart1,iend1,istart2,iend2) \
  if (gc::IsA<SimpleString_sp>(_string2_)) {				\
    if (gc::IsA<SimpleBaseString_sp>(_string2_)) { \
      auto sbcs2 = gc::As_unsafe<SimpleBaseString_sp>(_string2_); \
      return _function_(*_this_,*sbcs2,istart1,iend1,istart2,iend2); \
    } else {							\
      auto scs2 = gc::As_unsafe<SimpleCharacterString_sp>(_string2_); \
      return _function_(*_this_,*scs2,istart1,iend1,istart2,iend2); \
    } \
  } else { \
    if (gc::IsA<Str8Ns_sp>(_string2_)) { \
      auto ns82 = gc::As_unsafe<Str8Ns_sp>(_string2_); \
      return _function_(*_this_,*ns82,istart1,iend1,istart2,iend2); \
    } else { \
      auto nsw2 = gc::As_unsafe<StrWNs_sp>(_string2_); \
      return _function_(*_this_,*nsw2,istart1,iend1,istart2,iend2); \
    } \
  }

#define TEMPLATE_STRING_DISPATCHER(_string1_,_string2_,_function_,istart1,iend1,istart2,iend2) \
if (gc::IsA<SimpleString_sp>(_string1_) ) {			    \
  if (gc::IsA<SimpleString_sp>(_string2_)) {				\
    if (gc::IsA<SimpleBaseString_sp>(_string1_)) { \
      auto sbcs1 =  gc::As_unsafe<SimpleBaseString_sp>(_string1_); \
      if (gc::IsA<SimpleBaseString_sp>(_string2_)) { \
        auto sbcs2 = gc::As_unsafe<SimpleBaseString_sp>(_string2_); \
        return _function_(*sbcs1,*sbcs2,istart1,iend1,istart2,iend2); \
      } else {							\
        auto scs2 = gc::As_unsafe<SimpleCharacterString_sp>(_string2_); \
        return _function_(*sbcs1,*scs2,istart1,iend1,istart2,iend2); \
      } \
    } else { \
      auto scs1 = gc::As_unsafe<SimpleCharacterString_sp>(_string1_); \
      if (gc::IsA<SimpleBaseString_sp>(_string2_)) { \
        auto sbcs2 = gc::As_unsafe<SimpleBaseString_sp>(_string2_); \
        return _function_(*scs1,*sbcs2,istart1,iend1,istart2,iend2); \
      } else { \
        auto scs2 = gc::As_unsafe<SimpleCharacterString_sp>(_string2_); \
        return _function_(*scs1,*scs2,istart1,iend1,istart2,iend2); \
      } \
    } \
  } else { \
    if (gc::IsA<SimpleBaseString_sp>(_string1_)) { \
      auto sbcs1 = gc::As_unsafe<SimpleBaseString_sp>(_string1_); \
      if (gc::IsA<Str8Ns_sp>(_string2_)) { \
        auto ns82 = gc::As_unsafe<Str8Ns_sp>(_string2_); \
        return _function_(*sbcs1,*ns82,istart1,iend1,istart2,iend2);	\
      } else {							\
        auto nsw2 = gc::As_unsafe<StrWNs_sp>(_string2_); \
        return _function_(*sbcs1,*nsw2,istart1,iend1,istart2,iend2);	\
      }								\
    } else {								\
      auto scs1 = gc::As_unsafe<SimpleCharacterString_sp>(_string1_); \
      if (gc::IsA<Str8Ns_sp>(_string2_)) { \
        auto ns82 = gc::As_unsafe<Str8Ns_sp>(_string2_); \
        return _function_(*scs1,*ns82,istart1,iend1,istart2,iend2);	\
      } else {							\
        auto nsw2 = gc::As_unsafe<StrWNs_sp>(_string2_); \
        return _function_(*scs1,*nsw2,istart1,iend1,istart2,iend2);	\
      }								\
    }									\
  }									\
 } else { /* _string1_ is a StrNs_sp */  				    \
  if (gc::IsA<SimpleString_sp>(_string2_)) {				\
    /* _string2_ is a SimpleString_sp */ \
    if (gc::IsA<Str8Ns_sp>(_string1_)) { \
      auto ns81 = gc::As_unsafe<Str8Ns_sp>(_string1_); \
      if (gc::IsA<SimpleBaseString_sp>(_string2_)) { \
        auto sbcs2 = gc::As_unsafe<SimpleBaseString_sp>(_string2_); \
        return _function_(*ns81,*sbcs2,istart1,iend1,istart2,iend2); \
      } else { \
        auto scs2 = gc::As_unsafe<SimpleCharacterString_sp>(_string2_); \
        return _function_(*ns81,*scs2,istart1,iend1,istart2,iend2); \
      } \
    } else { \
      auto nsw1 = gc::As_unsafe<StrWNs_sp>(_string1_); \
      if (gc::IsA<SimpleBaseString_sp>(_string2_)) { \
        auto sbcs2 = gc::As_unsafe<SimpleBaseString_sp>(_string2_); \
        return _function_(*nsw1,*sbcs2,istart1,iend1,istart2,iend2); \
      } else { \
        auto scs2 = gc::As_unsafe<SimpleCharacterString_sp>(_string2_); \
        return _function_(*nsw1,*scs2,istart1,iend1,istart2,iend2); \
      } \
    } \
  } else { \
    if (gc::IsA<Str8Ns_sp>(_string1_)) { \
      auto ns81 = gc::As_unsafe<Str8Ns_sp>(_string1_); \
      if (gc::IsA<Str8Ns_sp>(_string2_)) { \
        auto ns82 = gc::As_unsafe<Str8Ns_sp>(_string2_); \
        return _function_(*ns81,*ns82,istart1,iend1,istart2,iend2); \
      } else { \
        auto nsw2 = gc::As_unsafe<StrWNs_sp>(_string2_); \
        return _function_(*ns81,*nsw2,istart1,iend1,istart2,iend2); \
      } \
    } else { \
      auto nsw1 = gc::As_unsafe<StrWNs_sp>(_string1_); \
      if (gc::IsA<Str8Ns_sp>(_string2_)) { \
        auto ns82 = gc::As_unsafe<Str8Ns_sp>(_string2_); \
        return _function_(*nsw1,*ns82,istart1,iend1,istart2,iend2); \
      } else { \
        auto nsw2 = gc::As_unsafe<StrWNs_sp>(_string2_); \
        return _function_(*nsw1,*nsw2,istart1,iend1,istart2,iend2); \
      } \
    } \
  } \
 } \
 SIMPLE_ERROR(BF("Illegal combination of string arguments in TEMPLATE_STRING_DISPATCHER"));





CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_EQ_");
CL_DEFUN T_sp cl__string_EQ_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  size_t istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_DISPATCHER(string1,string2,template_string_EQ_,istart1,iend1,istart2,iend2);
}

CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_NE_");
CL_DEFUN T_mv cl__string_NE_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  size_t istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_DISPATCHER(string1,string2,template_string_NE_, istart1, iend1, istart2, iend2);
}

CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_LT_");
CL_DEFUN T_mv cl__string_LT_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  size_t istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_DISPATCHER(string1,string2,template_string_LT_, istart1, iend1, istart2, iend2);
}

CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_GT_");
CL_DEFUN T_mv cl__string_GT_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  size_t istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_DISPATCHER(string1,string2,template_string_GT_, istart1, iend1, istart2, iend2);
}

CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_LE_");
CL_DEFUN T_mv cl__string_LE_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  size_t istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_DISPATCHER(string1,string2,template_string_LE_, istart1, iend1, istart2, iend2);
}

CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_GE_");
CL_DEFUN T_mv cl__string_GE_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  size_t istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_DISPATCHER(string1,string2,template_string_GE_, istart1, iend1, istart2, iend2);
}

CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_equal");
CL_DEFUN T_sp cl__string_equal(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  size_t istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_DISPATCHER(string1,string2,template_string_equal, istart1, iend1, istart2, iend2);
}

CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_not_equal");
CL_DEFUN T_mv cl__string_not_equal(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  size_t istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_DISPATCHER(string1,string2,template_string_not_equal, istart1, iend1, istart2, iend2);
}
CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_lessp");
CL_DEFUN T_mv cl__string_lessp(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  size_t istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_DISPATCHER(string1,string2,template_string_lessp, istart1, iend1, istart2, iend2);
}
CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_greaterp");
CL_DEFUN T_mv cl__string_greaterp(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  size_t istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_DISPATCHER(string1,string2,template_string_greaterp, istart1, iend1, istart2, iend2);
}
CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_not_greaterp");
CL_DEFUN T_mv cl__string_not_greaterp(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  size_t istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_DISPATCHER(string1,string2,template_string_not_greaterp, istart1, iend1, istart2, iend2);
}
CL_LAMBDA(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2);
CL_DECLARE();
CL_DOCSTRING("string_not_lessp");
CL_DEFUN T_mv cl__string_not_lessp(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2) {
  size_t istart1, iend1, istart2, iend2;
  String_sp string1;
  String_sp string2;
  setup_string_op_arguments(strdes1, strdes2, string1, string2, start1, end1, start2, end2, istart1, iend1, istart2, iend2);
  TEMPLATE_STRING_DISPATCHER(string1,string2,template_string_not_lessp, istart1, iend1, istart2, iend2);
}


// ------------------------------------------------------------
//
//



CL_LAMBDA("size &key initial-element (element-type 'character)");
CL_DECLARE();
CL_DOCSTRING("See CLHS: make_string");
CL_DEFUN T_sp cl__make_string(Fixnum_sp size, T_sp initial_element, T_sp element_type) {
  if (initial_element.nilp()) {
    initial_element = clasp_make_character(' ');
  }
  if (!initial_element.characterp()) {
    TYPE_ERROR(initial_element,cl::_sym_character);
  }
  if (!size.fixnump()) {
    TYPE_ERROR(size,cl::_sym_fixnum);
  }
  Fixnum sz = size.unsafe_fixnum();
  if (sz < 0 ) {
    SIMPLE_ERROR(BF("Size must be >= 0"));
  }
  claspCharacter initial_element_cc = initial_element.unsafe_character();
  if ( element_type == cl::_sym_base_char ) {
    if (!clasp_base_char_p(initial_element_cc)) {
      TYPE_ERROR(initial_element,cl::_sym_base_char);
    }
    SimpleBaseString_sp s = SimpleBaseString_O::make(size.unsafe_fixnum(),initial_element.unsafe_character(), true);
    return s;
  }
  SimpleCharacterString_sp s = SimpleCharacterString_O::make(size.unsafe_fixnum(),initial_element.unsafe_character(), true);
  return s;
}

SYMBOL_EXPORT_SC_(ClPkg,simple_string);

CL_LAMBDA(str index);
CL_DECLARE();
CL_DOCSTRING("CLHS schar");
CL_DEFUN Character_sp cl__schar(AbstractSimpleVector_sp str, size_t idx) {
  if (SimpleBaseString_sp sb = str.asOrNull<SimpleBaseString_O>()) {
    return clasp_make_character((*sb)[idx]);
  } else if (SimpleCharacterString_sp sc = str.asOrNull<SimpleCharacterString_O>()) {
    return clasp_make_character((*sc)[idx]);
  }
  TYPE_ERROR(str,cl::_sym_simple_string);
}


CL_LAMBDA(str idx);
CL_DOCSTRING("Common lisp char");
CL_DEFUN Character_sp cl__char(String_sp str, size_t idx) {
/* Return the character at idx - ignore fill pointers */
  if ( SimpleBaseString_sp sb = str.asOrNull<SimpleBaseString_O>() ) {
    return clasp_make_character((*sb)[idx]);
  } else if (Str8Ns_sp s8 = str.asOrNull<Str8Ns_O>() ) {
    return clasp_make_character((*s8)[idx]);
  } else if (SimpleCharacterString_sp sc = str.asOrNull<SimpleCharacterString_O>() ) {
    return clasp_make_character((*sc)[idx]);
  } else if (StrWNs_sp sw = str.asOrNull<StrWNs_O>() ) {
    return clasp_make_character((*sw)[idx]);
  }
  TYPE_ERROR(str,cl::_sym_string);
};

CL_LISPIFY_NAME("cl:char")
CL_LAMBDA(c str index);
CL_DECLARE();
CL_DOCSTRING("CLHS (setf char)");
CL_DEFUN_SETF Character_sp core__char_set(Character_sp c, String_sp str, size_t idx) {
  if ( SimpleBaseString_sp sb = str.asOrNull<SimpleBaseString_O>() ) {
    (*sb)[idx] = c.unsafe_character();
  } else if (Str8Ns_sp s8 = str.asOrNull<Str8Ns_O>() ) {
    (*s8)[idx] = c.unsafe_character();
  } else if (SimpleCharacterString_sp sc = str.asOrNull<SimpleCharacterString_O>() ) {
    (*sc)[idx] = c.unsafe_character();
  } else if (StrWNs_sp sw = str.asOrNull<StrWNs_O>() ) {
    (*sw)[idx] = c.unsafe_character();
  } else {
    TYPE_ERROR(str,cl::_sym_string);
  }
  return c;
};

CL_LISPIFY_NAME("cl:schar");
CL_LAMBDA(str index c);
CL_DECLARE();
CL_DOCSTRING("CLHS (setf schar)");
CL_DEFUN_SETF Character_sp core__schar_set(Character_sp c, String_sp str, size_t idx) {
  str->rowMajorAset(idx,c);
  return c;
};

typedef enum { iinit,
               iwhite,
               inum,
               itrailspace,
               ijunk,
               idone } IntegerFSMState;

/*! Digits are 0-9 or a-z/A-Z.
      If digit >= radix then return -1.
     */
cl_index fsmIntegerDigit(char c, cl_index radix) {
  cl_index idigit = -1;
  if (isdigit(c)) {
    idigit = c - '0';
  } else if (isalpha(c)) {
    idigit = -1;
    if (c >= 'A' && c <= 'Z')
      idigit = c - 'A' + 10;
    else if (c >= 'a' && c <= 'z')
      idigit = c - 'a' + 10;
  }
  if (idigit < 0)
    return idigit;
  if (idigit >= radix)
    return -1;
  return idigit;
}

cl_index fsmInteger(mpz_class &result, cl_index &numDigits, bool &sawJunk, String_sp str, cl_index istart, cl_index iend, bool junkAllowed, cl_index radix) {
  IntegerFSMState state = iinit;
  cl_index sign = 1;
  result = 0;
  numDigits = 0;
  cl_index cur = istart;
  while (1) {
    claspCharacter c = clasp_as_claspCharacter(gc::As_unsafe<Character_sp>(str->rowMajorAref(cur)));
    LOG(BF("fsmInteger str[%d] -> c = [%d/%c]") % cur  % c % c );
    switch (state) {
      LOG(BF("  top state = %d") % state);
    case iinit:
    case iwhite: {
      if (isspace(c)) {
        state = iwhite;
        break;
      } else if (c == '-') {
        state = inum;
        sign = -1;
        break;
      } else if (c == '+') {
        state = inum;
        break;
      } else if (isalnum(c)) {
        cl_index idigit = fsmIntegerDigit(c, radix);
        if (idigit < 0 || idigit >= radix) {
          LOG(BF("Hit junk at %d\n") % cur);
          state = ijunk;
          break;
        }
        result = result * GMP_LONG(radix) + GMP_LONG(idigit);
        ++numDigits;
        state = inum;
        break;
      }
      state = ijunk;
      break;
    }
    case inum: {
      if (isspace(c)) {
        // Optional leading and trailing whitespace[1] is ignored.
        // don't understand this but sbcl and ccl seem to agree, that if junkAllowed, than the parsing stop
        // once a space after the number is read
        if (junkAllowed) {
          state = ijunk; // itrailspace;
          break;
        }
        state = itrailspace;
        break;
      } else if (isalnum(c)) {
        cl_index idigit = fsmIntegerDigit(c, radix);
        if (idigit < 0 || idigit >= radix) {
          state = ijunk;
          break;
        }
        result = result * GMP_LONG(radix) + GMP_LONG(idigit);
        ++numDigits;
        state = inum;
        break;
      }
      state = ijunk;
      break;
    }
    case itrailspace: {
      if (!isspace(c)) {
        state = ijunk;
        break;
      }
    }
    case ijunk:
        break;
    case idone:
        break;
    }
    LOG(BF("  bottom state = %d") % state);
    if (state == idone)
      break;
    if (state == ijunk)
      break;
    ++cur;
    if (cur >= iend)
      break;
  }
  sawJunk = (state == ijunk);
  if (sign < 0) {
    mpz_class nresult;
    mpz_neg(nresult.get_mpz_t(), result.get_mpz_t());
    mpz_swap(nresult.get_mpz_t(), result.get_mpz_t());
  }
  LOG(BF("Returning with cur=%d") % cur);
  return cur;
};


CL_LAMBDA(string &key (start 0) end (radix 10) junk-allowed);
CL_DECLARE();
CL_DOCSTRING("parseInteger");
CL_DEFUN T_mv cl__parse_integer(String_sp str, Fixnum start, T_sp end, uint radix, T_sp junkAllowed) {
  Fixnum istart = std::max((Fixnum)0, start);
  Fixnum iend = cl__length(str);
  if (end.notnilp()) {
    iend = std::min(iend, unbox_fixnum(gc::As<Fixnum_sp>(end)));
  }
  mpz_class result;
  bool sawJunk = false;
  cl_index numDigits = 0;
  cl_index cur = fsmInteger(result, numDigits, sawJunk, str, istart, iend, junkAllowed.isTrue(), radix);
  if (junkAllowed.notnilp() || (cur >= iend) || !sawJunk) {
    // normal exit
    if (numDigits > 0) {
      Integer_sp iresult = Integer_O::create(result);
      LOG(BF("Returning parse-integer with result = %s  cur = %d") % _rep_(iresult) % cur );
      return (Values(iresult, make_fixnum(cur)));
    } else {
      //If junk-allowed is false, an error of type parse-error is signaled if substring does not consist entirely of the representation
      // of a signed integer, possibly surrounded on either side by whitespace[1] characters.
      // The first value returned is either the integer that was parsed, or else nil if no syntactically correct integer was seen but junk-allowed was true.
      if (junkAllowed.notnilp())
        return (Values(_Nil<T_O>(), make_fixnum(cur)));
      else PARSE_ERROR(SimpleBaseString_O::make("Could not parse integer from ~S"), Cons_O::create(str,_Nil<T_O>()));
    }
  }
  PARSE_ERROR(SimpleBaseString_O::make("Could not parse integer from ~S"), Cons_O::create(str,_Nil<T_O>()));
  UNREACHABLE();
};


SYMBOL_EXPORT_SC_(ClPkg, string_EQ_);
SYMBOL_EXPORT_SC_(ClPkg, string_NE_);
SYMBOL_EXPORT_SC_(ClPkg, string_LT_);
SYMBOL_EXPORT_SC_(ClPkg, string_GT_);
SYMBOL_EXPORT_SC_(ClPkg, string_LE_);
SYMBOL_EXPORT_SC_(ClPkg, string_GE_);
SYMBOL_EXPORT_SC_(ClPkg, string_equal);
SYMBOL_EXPORT_SC_(ClPkg, string_not_equal);
SYMBOL_EXPORT_SC_(ClPkg, string_lessp);
SYMBOL_EXPORT_SC_(ClPkg, string_greaterp);
SYMBOL_EXPORT_SC_(ClPkg, string_not_greaterp);
SYMBOL_EXPORT_SC_(ClPkg, string_not_lessp);
SYMBOL_EXPORT_SC_(ClPkg, make_string);
SYMBOL_EXPORT_SC_(ClPkg, parseInteger);

};



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

// ------------------------------------------------------------
//
// Class SimpleBaseString_O
//


    // (element_type == cl::_sym_base_char)
typename SimpleBaseString_O::value_type SimpleBaseString_O::initial_element_from_object(T_sp obj, bool initialElementSuppliedP) {
  typename SimpleBaseString_O::value_type initialBaseChar = '\0';
  if (initialElementSuppliedP) {
    if (obj.characterp() ) {
      Character_sp initCharacter = gc::As_unsafe<Character_sp>(obj);
      if (clasp_base_char_p(initCharacter)) {
        return (typename SimpleBaseString_O::value_type)initCharacter.unsafe_character();
      }
    }
    TYPE_ERROR(obj,cl::_sym_base_char);
  }
  return '\0';
}

bool SimpleBaseString_O::equal(T_sp other) const {
  if (&*other==this) return true;
  if (!other.generalp()) return false;
  if (!cl__stringp(other)) return false;
  String_sp sother = gc::As_unsafe<String_sp>(other);
  TEMPLATE_HALF_STRING_DISPATCHER(this,sother,template_string_EQ_equal,0,this->length(),0,sother->length());
};

bool SimpleBaseString_O::equalp(T_sp other) const {
  if (&*other==this) return true;
  if (!other.generalp()) return false;
  if (cl__stringp(other)) {
    String_sp sother = gc::As_unsafe<String_sp>(other);
    TEMPLATE_HALF_STRING_DISPATCHER(this,sother,template_string_equalp_bool,0,this->length(),0,sother->length());
  } else {
    return this->AbstractSimpleVector_O::equalp(other);
  }
}


// ------------------------------------------------------------
//
// Class SimpleCharacterString_O
//

typename SimpleCharacterString_O::value_type SimpleCharacterString_O::initial_element_from_object(T_sp obj, bool initialElementSuppliedP) {
  typename SimpleCharacterString_O::value_type initialCharacter = '\0';
  if (initialElementSuppliedP) {
    if (obj.characterp() ) {
      Character_sp initCharacter = gc::As_unsafe<Character_sp>(obj);
      return (typename SimpleCharacterString_O::value_type)initCharacter.unsafe_character();
    }
    TYPE_ERROR(obj,cl::_sym_character);
  }
  return '\0';
}

bool SimpleCharacterString_O::equal(T_sp other) const {
  if (&*other==this) return true;
  if (!other.generalp()) return false;
  if (gc::IsA<SimpleString_sp>(other)) {
    if (gc::IsA<SimpleBaseString_sp>(other)) {
      auto so = gc::As_unsafe<SimpleBaseString_sp>(other);
      return template_string_EQ_equal(*this,*so,0,this->length(),0,so->length());
    } else {
      auto so = gc::As_unsafe<SimpleCharacterString_sp>(other);
      return template_string_EQ_equal(*this,*so,0,this->length(),0,so->length());
    }
  } else {
    if (gc::IsA<Str8Ns_sp>(other)) {
      auto so = gc::As_unsafe<Str8Ns_sp>(other);
      return template_string_EQ_equal(*this,*so,0,this->length(),0,so->length());
    } else if (gc::IsA<StrWNs_sp>(other)) {
      auto so = gc::As_unsafe<StrWNs_sp>(other);
      return template_string_EQ_equal(*this,*so,0,this->length(),0,so->length());
    }
  }
  return false;
};

bool SimpleCharacterString_O::equalp(T_sp other) const {
  if (&*other==this) return true;
  if (!other.generalp()) return false;
  if (cl__stringp(other)) {
    String_sp sother = gc::As_unsafe<String_sp>(other);
    TEMPLATE_HALF_STRING_DISPATCHER(this,sother,template_string_equalp_bool,0,this->length(),0,sother->length());
  } else {
    return this->AbstractSimpleVector_O::equalp(other);
  }
}

std::string SimpleCharacterString_O::get_std_string() const {
  std::string sout(this->length(),' ');
  for (size_t i(0),iEnd(this->length()); i<iEnd; ++i ) sout[i] = (*this)[i];
  return sout;
}

std::string SimpleCharacterString_O::__repr__() const {
  return this->get_std_string();
}

// ----------------------------------------------------------------------
//
// Class SimpleBitVector
//

SimpleBitVector_sp SimpleBitVector_O::make(const string& bv) {
  size_t dim = bv.size()-2;
  SimpleBitVector_sp x = SimpleBitVector_O::make(dim);
  for (int i = 0; i<dim; i++) {
    char elt = bv[i+2];
    x->setBit(i,elt-'0');
  }
  return x;
}

// Restored by drmeister because Cando uses this function
//  we can remove it if there is a problem with it or it is redundant.
SimpleBitVector_sp SimpleBitVector_copy(SimpleBitVector_sp orig_sbv)
{
  size_t value_type_size = core::SimpleBitVector_O::bitunit_array_type::sizeof_for_length(orig_sbv->length())/sizeof(core::SimpleBitVector_O::value_type);
//  printf("%s:%d Copy SimpleBitVector length = %" PRu "   value_type_size = %lu\n", __FILE__, __LINE__, orig_sbv->length(), value_type_size );
//  fflush(stdout);
  core::SimpleBitVector_sp sbv = core::SimpleBitVector_O::make(orig_sbv->length(),0,true,value_type_size,&orig_sbv->_Data[0]);
  return sbv;
}

Array_sp SimpleBitVector_O::unsafe_subseq(size_t start, size_t end) const {
  BOUNDS_ASSERT(0<=start&&start<end&&end<=this->length());
  SimpleBitVector_sp sbv = SimpleBitVector_O::make(end-start);
  for (size_t i(0),iEnd(end-start);i<iEnd;++i) {
    sbv->setBit(i,this->testBit(start+i));
  }
  return sbv;
}

Array_sp SimpleBitVector_O::unsafe_setf_subseq(size_t start, size_t end, Array_sp other) {
  BOUNDS_ASSERT(0<=start&&start<end&&end<=this->length());
  if (SimpleBitVector_sp sbv = other.asOrNull<SimpleBitVector_O>()) {
      // TODO: Write specialized versions of this to speed it up
    for ( size_t i(start),ni(0); i<end; ++i,++ni ) {
      this->setBit(i,sbv->testBit(ni));
    }
    return this->asSmartPtr();
  } else if (BitVectorNs_sp bv = other.asOrNull<BitVectorNs_O>()) {
    AbstractSimpleVector_sp bsv;
    size_t ostart, oend;
    bv->asAbstractSimpleVectorRange(bsv,ostart,oend);
    SimpleBitVector_sp sbv = gc::As_unsafe<SimpleBitVector_sp>(bsv);
    for ( size_t i(start),io(ostart); i<end; ++i, ++io) {
      this->setBit(i,sbv->testBit(io));
    }
    return this->asSmartPtr();
  }
  TYPE_ERROR(other,cl::_sym_bit_vector);
}

// This does not work properly
/*
void SimpleBitVector_O::unsafe_fillArrayWithElt(T_sp initialElement, size_t start, size_t end)
{
  value_type initBlockValue = (initialElement.nilp()) ? 0 : ~0;
  // round up start and round down end
  size_t blockStart = ((start+BitWidth-1)/BitWidth);
  size_t blockEnd = (end+BitWidth-1)/BitWidth;
  for (size_t i(start), iEnd(blockStart*BitWidth); i<iEnd; ++i ) {
    this->setBit(i,initBlockValue);
  }
  for (size_t i(blockStart),iEnd(blockEnd); i<iEnd; ++i ) {
    this->_Data[i] = initBlockValue;
  }
  for (size_t i(blockEnd*BitWidth), iEnd(end); i<iEnd; ++i ) {
    this->setBit(i,initBlockValue);
  }
};
*/

void SimpleBitVector_O::unsafe_fillArrayWithElt(T_sp initialElement, size_t start, size_t end) {
  if (CLASP_FIXNUMP (initialElement))
  {
    Fixnum zero_or_one = gc::As<core::Fixnum_sp>(initialElement).unsafe_fixnum();
    if ((zero_or_one == 0) || (zero_or_one == 1)) {
      for (size_t i(start),iEnd(end); i<iEnd; ++i) {
        this->setBit(i, zero_or_one);
      }
    }
    else TYPE_ERROR(initialElement, cl::_sym_bit);
  }
  else TYPE_ERROR(initialElement, cl::_sym_bit);
}


bool SimpleBitVector_O::equal(T_sp other) const {
  if (this == &*other) return true;
  if (SimpleBitVector_sp sbv = other.asOrNull<SimpleBitVector_O>()) {
    if (this->length()!=sbv->length()) return false;
    return ranged_bit_vector_EQ_(*this,*sbv,0,this->length(),0,sbv->length());
  } else if (BitVectorNs_sp bvns = other.asOrNull<BitVectorNs_O>()) {
    if (this->length()!=bvns->length()) return false;
    AbstractSimpleVector_sp sv;
    size_t start, end;
    bvns->asAbstractSimpleVectorRange(sv,start,end);
    SimpleBitVector_O& sbv = *gc::As<SimpleBitVector_sp>(sv);
    return ranged_bit_vector_EQ_(*this,sbv,0,this->length(),start,end);
  }
  return false;
}

Array_sp SimpleBitVector_O::reverse() const {
  return ranged_bit_vector_reverse(this->asSmartPtr(),0,this->length());
}

Array_sp SimpleBitVector_O::nreverse() {
  ranged_bit_vector_nreverse(this->asSmartPtr(),0,this->length());
  return this->asSmartPtr();
}

void SimpleBitVector_inPlaceOr(SimpleBitVector_sp x, SimpleBitVector_sp y) {
  size_t i;
  if (x->length() != y->length()) SIMPLE_ERROR(BF("BitVectors aren't the same length for in place or - lengths are %d and %d") % x->length() % y->length());
  for (size_t i = 0; i<x->_Data.number_of_words(); ++i ) {
    (*x)._Data[i] |= (*y)._Data[i];
  }
}

void SimpleBitVector_inPlaceAnd(SimpleBitVector_sp x, SimpleBitVector_sp y) {
  size_t i;
  if (x->length() != y->length()) SIMPLE_ERROR(BF("BitVectors aren't the same length for operation"));
  for (size_t i = 0; i<x->_Data.number_of_words(); ++i ) {
    (*x)._Data[i] &= (*y)._Data[i];
  }
}

void SimpleBitVector_inPlaceXor(SimpleBitVector_sp x, SimpleBitVector_sp y) {
  size_t i;
  if (x->length() != y->length()) SIMPLE_ERROR(BF("BitVectors aren't the same length for operation"));
  for (size_t i = 0; i<x->_Data.number_of_words(); ++i ) {
    (*x)._Data[i] ^= (*y)._Data[i];
  }
}

size_t SimpleBitVector_lowestIndex(SimpleBitVector_sp x) {
  size_t word_length = x->length()/SimpleBitVector_O::BitWidth;
  size_t ib = 0;
  size_t iw = 0;
  while (iw<word_length && x->_Data[iw]==0) {
    ++iw;
    ib = ib + SimpleBitVector_O::BitWidth;
  }
  for ( ; ib < x->length(); ib++) {
    if (x->testBit(ib)) {
      return ib;
    }
  }
  return ib;
}

void SimpleBitVector_getOnIndices(SimpleBitVector_sp x, vector<size_t> &res) {
  size_t i;
  res.clear();
  i = SimpleBitVector_lowestIndex(x);
  for (; i != x->length(); i++) {
    if (x->testBit(i)) {
      res.push_back(i);
    }
  }
}

bool SimpleBitVector_isZero(SimpleBitVector_sp x) {
  return (SimpleBitVector_lowestIndex(x) == x->length());
}
// ------------------------------------------------------------
//
// Class VectorNs
//

namespace core {

};


// ------------------------------------------------------------
//
// Class Str8Ns
//

bool Str8Ns_O::equal(T_sp other) const {
  if (&*other==this) return true;
  if (!other.generalp()) return false;
  if (gc::IsA<SimpleString_sp>(other)) {
    if (gc::IsA<SimpleBaseString_sp>(other)) {
      auto so = gc::As_unsafe<SimpleBaseString_sp>(other);
      return template_string_EQ_equal(*this,*so,0,this->length(),0,so->length());
    } else {
      auto so = gc::As_unsafe<SimpleCharacterString_sp>(other);
      return template_string_EQ_equal(*this,*so,0,this->length(),0,so->length());
    }
  } else {
    if (gc::IsA<Str8Ns_sp>(other)) {
      auto so = gc::As_unsafe<Str8Ns_sp>(other);
      return template_string_EQ_equal(*this,*so,0,this->length(),0,so->length());
    } else if (gc::IsA<StrWNs_sp>(other)){
      auto so = gc::As_unsafe<StrWNs_sp>(other);
      return template_string_EQ_equal(*this,*so,0,this->length(),0,so->length());
    }
  }
  return false;
};

// Creators - depreciate these once the new array stuff is working better
Str8Ns_sp Str8Ns_O::create(const string& nm) {
  auto ss = SimpleBaseString_O::make(nm.size(),'\0',true,nm.size(),(const claspChar*)nm.c_str());
  return Str8Ns_O::make(nm.size(),'\0',false,_Nil<T_O>(),ss,false,0);
}

Str8Ns_sp Str8Ns_O::create(const char* nm,size_t len) {
  SimpleBaseString_sp ss = SimpleBaseString_O::make(len,'\0',true,len,(const claspChar*)nm);
  return Str8Ns_O::make(len,'\0',false,_Nil<T_O>(),ss,false,0);
}

Str8Ns_sp Str8Ns_O::create(const char* nm) {
  size_t len = strlen(nm);
  return Str8Ns_O::create(nm,len);
}

Str8Ns_sp Str8Ns_O::create(size_t len) {
  return Str8Ns_O::make(len,'\0',true,_Nil<T_O>(),_Nil<T_O>(),false,0);
}

Str8Ns_sp Str8Ns_O::create(Str8Ns_sp other) {
  size_t len = other->length();
  SimpleBaseString_sp ss = SimpleBaseString_O::make(len,'\0',true,len,&(*other)[0]);
  return Str8Ns_O::make(len,'\0',false,_Nil<T_O>(),ss,false,0);
}

SYMBOL_EXPORT_SC_(CorePkg,vectorPushExtend_claspCharacter);
void Str8Ns_O::vectorPushExtend_claspChar(claspChar newElement, size_t extension) {
  unlikely_if (!this->_Flags.fillPointerP()) noFillPointerError(_sym_vectorPushExtend_claspCharacter,this->asSmartPtr());
  cl_index idx = this->_FillPointerOrLengthOrDummy;
  unlikely_if (idx >= this->_ArrayTotalSize) {
    if (extension <= 0) extension = calculate_extension(this->_ArrayTotalSize);
    cl_index new_size = this->_ArrayTotalSize+extension;
    this->internalAdjustSize_(new_size);
  }
  (*this)[idx] = newElement;
  ++this->_FillPointerOrLengthOrDummy;
}


SimpleString_sp Str8Ns_O::asMinimalSimpleString() const {
  SimpleBaseString_sp str8 = SimpleBaseString_O::make(this->length());
  str8->unsafe_setf_subseq(0,this->length(),this->asSmartPtr());
  return str8;
}



// ------------------------------------------------------------
//
// Class StrWNs
//

bool StrWNs_O::all_base_char_p() const {
  bool extended=false;
  for (size_t i(0),iEnd(this->length()); i<iEnd;++i) {
    if (!clasp_base_char_p((*this)[i])) extended = true;
  }
  return !extended;
}

bool StrWNs_O::equal(T_sp other) const {
  if (&*other==this) return true;
  if (!other.generalp()) return false;
  if (gc::IsA<SimpleString_sp>(other)) {
    if (gc::IsA<SimpleBaseString_sp>(other)) {
      auto so = gc::As_unsafe<SimpleBaseString_sp>(other);
      return template_string_EQ_equal(*this,*so,0,this->length(),0,so->length());
    } else {
      auto so = gc::As_unsafe<SimpleCharacterString_sp>(other);
      return template_string_EQ_equal(*this,*so,0,this->length(),0,so->length());
    }
  } else {
    if (gc::IsA<Str8Ns_sp>(other)) {
      auto so = gc::As_unsafe<Str8Ns_sp>(other);
      return template_string_EQ_equal(*this,*so,0,this->length(),0,so->length());
    } else if (gc::IsA<StrWNs_sp>(other)){
      auto so = gc::As_unsafe<StrWNs_sp>(other);
      return template_string_EQ_equal(*this,*so,0,this->length(),0,so->length());
    }
  }
  return false;
};

std::string StrWNs_O::get_std_string() const {
  std::string sout(this->length(),' ');
  for (size_t i(0),iEnd(this->length()); i<iEnd; ++i ) sout[i] = (*this)[i];
  return sout;
}

std::string StrWNs_O::__repr__() const {
  return this->get_std_string();
}

SYMBOL_EXPORT_SC_(CorePkg,vectorPushExtend_claspCharacter);
void StrWNs_O::vectorPushExtend_claspCharacter(claspCharacter newElement, size_t extension) {
  unlikely_if (!this->_Flags.fillPointerP()) noFillPointerError(_sym_vectorPushExtend_claspCharacter,this->asSmartPtr());
  cl_index idx = this->_FillPointerOrLengthOrDummy;
  unlikely_if (idx >= this->_ArrayTotalSize) {
    if (extension <= 0) extension = calculate_extension(this->_ArrayTotalSize);
    cl_index new_size = this->_ArrayTotalSize+extension;
    this->internalAdjustSize_(new_size);
  }
  (*this)[idx] = newElement;
  ++this->_FillPointerOrLengthOrDummy;
}

SimpleString_sp StrWNs_O::asMinimalSimpleString() const {
  if (this->all_base_char_p()) {
    SimpleBaseString_sp str8 = SimpleBaseString_O::make(this->length());
    str8->unsafe_setf_subseq(0,this->length(),this->asSmartPtr());
    return str8;
  } else {
    SimpleCharacterString_sp strw = SimpleCharacterString_O::make(this->length());
    strw->unsafe_setf_subseq(0,this->length(),this->asSmartPtr());
    return strw;
  }
}

// ------------------------------------------------------------
//
// Class BitVectorNs
//
//

SYMBOL_EXPORT_SC_(ClPkg,vectorPushExtend);
Fixnum_sp BitVectorNs_O::vectorPushExtend(T_sp newElement, size_t extension) {
  unlikely_if (!this->_Flags.fillPointerP()) noFillPointerError(cl::_sym_vectorPushExtend,this->asSmartPtr());
  cl_index idx = this->_FillPointerOrLengthOrDummy;
//  printf("%s:%d  idx = %lld  this->_ArrayTotalSize = %zu\n", __FILE__, __LINE__, idx, this->_ArrayTotalSize );
  unlikely_if (idx >= this->_ArrayTotalSize) {
    unlikely_if (this->displacedToP()) {
    // The array needs to be resized because it's displaced
      if (extension <= 0) extension = calculate_extension(this->_ArrayTotalSize);
      cl_index new_size = this->_ArrayTotalSize+extension;
//      printf("%s:%d About to adjust displaced BitVectorNs_O size to %lld bits\n", __FILE__, __LINE__, new_size);
      this->internalAdjustSize_(new_size);
    } else {
      size_t bytes_for_ArrayTotalSizeP1 = SimpleBitVector_O::bitunit_array_type::sizeof_for_length(this->_ArrayTotalSize+1);
      size_t bytes_for_ArrayTotalSize = SimpleBitVector_O::bitunit_array_type::sizeof_for_length(this->_ArrayTotalSize);
//      printf("%s:%d bytes_for_ArrayTotalSizeP1 = %zu\n", __FILE__, __LINE__, bytes_for_ArrayTotalSizeP1);
//      printf("%s:%d bytes_for_ArrayTotalSize = %zu\n", __FILE__, __LINE__, bytes_for_ArrayTotalSize);
      if (bytes_for_ArrayTotalSizeP1 > bytes_for_ArrayTotalSize) { // or it needs more words to store the bits
    // The array needs to be resized because there aren't enough bits to hold the next bit
        if (extension <= 0) extension = calculate_extension(this->_ArrayTotalSize);
        cl_index new_size = this->_ArrayTotalSize+extension;
//        printf("%s:%d About to adjust BitVectorNs_O size to %lld bits\n", __FILE__, __LINE__, new_size);
        this->internalAdjustSize_(new_size);
      } else {
    // There were enough bits to handle the extend
        this->_ArrayTotalSize = idx+1;
        this->_Dimensions[0] = idx+1;
      }
    }
  }
  this->_Data->rowMajorAset(idx+this->_DisplacedIndexOffset,newElement);
  ++this->_FillPointerOrLengthOrDummy;
  return make_fixnum(idx);
}

void BitVectorNs_O::internalAdjustSize_(size_t size, T_sp initElement, bool initElementSupplied) {
//  printf("%s:%d:%s    size = %zu\n", __FILE__, __LINE__, __FUNCTION__, size);
  if (size == this->_ArrayTotalSize) return;
  AbstractSimpleVector_sp basesv;
  size_t start, end;
  this->asAbstractSimpleVectorRange(basesv,start,end);
  gctools::smart_ptr<simple_type> sv = gc::As_unsafe<gctools::smart_ptr<simple_type>>(basesv);
  size_t initialContentsSize = MIN(this->length(),size);
  gc::smart_ptr<simple_type> newData = simple_type::make(size,0,true);
//  printf("%s:%d  class-of newData -> %s   newData.raw_() -> %p\n", __FILE__, __LINE__, _rep_(cl__class_of(newData)).c_str(), (void*)newData.raw_());
  for (size_t i(0),iEnd(initialContentsSize); i<iEnd; ++i ) {
//    printf("%s:%d   Reading bit at i+start->%zu  value-> %u    writing to index: %zu\n", __FILE__, __LINE__, i+start, sv->testBit(i+start), i);
    newData->setBit(i,sv->testBit(i+start));
  }
  this->set_data(newData);
  this->_ArrayTotalSize = size;
  this->_Dimensions[0] = size;
  if (!this->_Flags.fillPointerP()) this->_FillPointerOrLengthOrDummy = size;
  this->_DisplacedIndexOffset = 0;
  this->_Flags.set_displacedToP(false);
}


Array_sp BitVectorNs_O::reverse() const {
  AbstractSimpleVector_sp basesv;
  size_t start, end;
  this->asAbstractSimpleVectorRange(basesv,start,end);
  gctools::smart_ptr<simple_type> sv = gc::As_unsafe<gctools::smart_ptr<simple_type>>(basesv);
  return ranged_bit_vector_reverse(sv,start,end);
}

Array_sp BitVectorNs_O::nreverse() {
  AbstractSimpleVector_sp basesv;
  size_t start, end;
  this->asAbstractSimpleVectorRange(basesv,start,end);
  gctools::smart_ptr<simple_type> sv = gc::As_unsafe<gctools::smart_ptr<simple_type>>(basesv);
  ranged_bit_vector_nreverse(sv,start,end);
  return this->asSmartPtr();
}

bool BitVectorNs_O::equal(T_sp other) const {
  if (&*other==this) return true;
  if (!other.generalp()) return false;
  if (BitVectorNs_sp strns = other.asOrNull<BitVectorNs_O>()) {
    AbstractSimpleVector_sp bme;
    size_t mstart, mend;
    this->asAbstractSimpleVectorRange(bme,mstart,mend);
    simple_type* me = reinterpret_cast<simple_type*>(&*bme);
    AbstractSimpleVector_sp bso;
    size_t ostart, oend;
    strns->asAbstractSimpleVectorRange(bso,ostart,oend);
    simple_type* so = reinterpret_cast<simple_type*>(&*bso);
    return ranged_bit_vector_EQ_(*me,*so,mstart,mend,ostart,oend);
  } else if (SimpleBitVector_sp ss = other.asOrNull<SimpleBitVector_O>()) {
    AbstractSimpleVector_sp bme;
    size_t mstart, mend;
    this->asAbstractSimpleVectorRange(bme,mstart,mend);
    simple_type* me = reinterpret_cast<simple_type*>(&*bme);
    return ranged_bit_vector_EQ_(*me,*ss,mstart,mend,0,ss->length());
  }
  return false;
};

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



namespace core {
void StringPushSubString(String_sp buffer, String_sp str, size_t start, size_t end) {
  for ( ; start<end; ++start ) {
    buffer->vectorPushExtend(cl__char(str, start));
  }
}

void StringPushString(String_sp buffer, String_sp other) {
  StringPushSubString(buffer, other, 0, cl__length(other));
}

void StringPushStringCharStar(String_sp buffer, const char *cPtr) {
  while (*cPtr) {
    buffer->vectorPushExtend(clasp_make_character(*cPtr), 64);
    ++cPtr;
  }
}




string string_get_std_string(String_sp str) { return str->get_std_string(); };
string string_get_std_string(T_sp str) {
  if (str.nilp()) {
    SIMPLE_ERROR(BF("Could not convert nil to Str"));
  };
  return gc::As<String_sp>(str)->get_std_string();
};
SimpleBaseString_sp str_create(const string &str) { return SimpleBaseString_O::make(str); };
SimpleBaseString_sp str_create(const char *str) { return SimpleBaseString_O::make(std::string(str)); };


};


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




CL_LAMBDA(element_type);
CL_DECLARE();
CL_DOCSTRING("Returns the element used as an initial-element if none is supplied, for the given array element type.");
CL_DEFUN T_sp core__default_initial_element(T_sp element_type) {
  if (element_type == cl::_sym_bit)
    return SimpleBitVector_O::to_object(SimpleBitVector_O::default_initial_element());
  else if (element_type == cl::_sym_base_char)
    return SimpleBaseString_O::to_object(SimpleBaseString_O::default_initial_element());
  else if (element_type == cl::_sym_character)
    return SimpleCharacterString_O::to_object(SimpleCharacterString_O::default_initial_element());
  else if (element_type == cl::_sym_T_O)
    return SimpleVector_O::to_object(SimpleVector_O::default_initial_element());
  else if (element_type == cl::_sym_double_float)
    return SimpleVector_double_O::to_object(SimpleVector_double_O::default_initial_element());
  else if (element_type == cl::_sym_single_float)
    return SimpleVector_float_O::to_object(SimpleVector_float_O::default_initial_element());
  else if (element_type == ext::_sym_integer8)
    return SimpleVector_int8_t_O::to_object(SimpleVector_int8_t_O::default_initial_element());
  else if (element_type == ext::_sym_byte8)
    return SimpleVector_byte8_t_O::to_object(SimpleVector_byte8_t_O::default_initial_element());
  else if (element_type == ext::_sym_integer16)
    return SimpleVector_int16_t_O::to_object(SimpleVector_int16_t_O::default_initial_element());
  else if (element_type == ext::_sym_byte16)
    return SimpleVector_byte16_t_O::to_object(SimpleVector_byte16_t_O::default_initial_element());
  else if (element_type == ext::_sym_integer32)
    return SimpleVector_int32_t_O::to_object(SimpleVector_int32_t_O::default_initial_element());
  else if (element_type == ext::_sym_byte32)
    return SimpleVector_byte32_t_O::to_object(SimpleVector_byte32_t_O::default_initial_element());
  else if (element_type == ext::_sym_integer64)
    return SimpleVector_int64_t_O::to_object(SimpleVector_int64_t_O::default_initial_element());
  else if (element_type == ext::_sym_byte64)
    return SimpleVector_byte64_t_O::to_object(SimpleVector_byte64_t_O::default_initial_element());
  else if (element_type == _sym_size_t)
    return SimpleVector_size_t_O::to_object(SimpleVector_size_t_O::default_initial_element());
  else if (element_type == cl::_sym_fixnum)
    return SimpleVector_fixnum_O::to_object(SimpleVector_fixnum_O::default_initial_element());
  else SIMPLE_ERROR(BF("Unknown element type %s") % _rep_(element_type));
}

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
    return SimpleBaseString_O::make(dimension,initialChar,initialElementSuppliedP,true);
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
  if (element_type == cl::_sym_T_O) {
    if (adjustable) return MDArrayT_O::make_multi_dimensional(dimensions,initialElement,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    else return SimpleMDArrayT_O::make_multi_dimensional(dimensions,initialElement,_Nil<T_O>());
  } else if (element_type == cl::_sym_double_float) {
    double initialValue = SimpleVector_double_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return MDArray_double_O::make_multi_dimensional(dimensions,initialValue,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    else return SimpleMDArray_double_O::make_multi_dimensional(dimensions,initialValue,_Nil<T_O>());
  } else if (element_type == cl::_sym_single_float) {
    float initialValue = SimpleVector_float_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return MDArray_float_O::make_multi_dimensional(dimensions,initialValue,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    else return SimpleMDArray_float_O::make_multi_dimensional(dimensions,initialValue,_Nil<T_O>());
  } else if (element_type == cl::_sym_bit) {
    uint initialValue = SimpleBitVector_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return MDArrayBit_O::make_multi_dimensional(dimensions,initialValue,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    else return SimpleMDArrayBit_O::make_multi_dimensional(dimensions,initialValue,_Nil<T_O>());
  } else if (element_type == cl::_sym_base_char) {
    claspChar initialValue = SimpleBaseString_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return MDArrayBaseChar_O::make_multi_dimensional(dimensions,initialValue,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    else return SimpleMDArrayBaseChar_O::make_multi_dimensional(dimensions,initialValue,_Nil<T_O>());
  } else if (element_type == cl::_sym_character) {
    claspCharacter initialValue = SimpleCharacterString_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return MDArrayCharacter_O::make_multi_dimensional(dimensions,initialValue,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    else return SimpleMDArrayCharacter_O::make_multi_dimensional(dimensions,initialValue,_Nil<T_O>());

  } else if (element_type == ext::_sym_integer8) {
    int8_t initialValue = SimpleVector_int8_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return MDArray_int8_t_O::make_multi_dimensional(dimensions,initialValue,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    else return SimpleMDArray_int8_t_O::make_multi_dimensional(dimensions,initialValue,_Nil<T_O>());
  } else if (element_type == ext::_sym_byte8) {
    byte8_t initialValue = SimpleVector_byte8_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return MDArray_byte8_t_O::make_multi_dimensional(dimensions,initialValue,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    else return SimpleMDArray_byte8_t_O::make_multi_dimensional(dimensions,initialValue,_Nil<T_O>());

  } else if (element_type == ext::_sym_integer16) {
    int16_t initialValue = SimpleVector_int16_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return MDArray_int16_t_O::make_multi_dimensional(dimensions,initialValue,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    else return SimpleMDArray_int16_t_O::make_multi_dimensional(dimensions,initialValue,_Nil<T_O>());
  } else if (element_type == ext::_sym_byte16) {
    byte16_t initialValue = SimpleVector_byte16_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return MDArray_byte16_t_O::make_multi_dimensional(dimensions,initialValue,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    else return SimpleMDArray_byte16_t_O::make_multi_dimensional(dimensions,initialValue,_Nil<T_O>());

  } else if (element_type == ext::_sym_integer32) {
    int32_t initialValue = SimpleVector_int32_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return MDArray_int32_t_O::make_multi_dimensional(dimensions,initialValue,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    else return SimpleMDArray_int32_t_O::make_multi_dimensional(dimensions,initialValue,_Nil<T_O>());
  } else if (element_type == ext::_sym_byte32) {
    byte32_t initialValue = SimpleVector_byte32_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return MDArray_byte32_t_O::make_multi_dimensional(dimensions,initialValue,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    else return SimpleMDArray_byte32_t_O::make_multi_dimensional(dimensions,initialValue,_Nil<T_O>());

  } else if (element_type == ext::_sym_integer64) {
    int64_t initialValue = SimpleVector_int64_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return MDArray_int64_t_O::make_multi_dimensional(dimensions,initialValue,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    else return SimpleMDArray_int64_t_O::make_multi_dimensional(dimensions,initialValue,_Nil<T_O>());
  } else if (element_type == ext::_sym_byte64) {
    byte64_t initialValue = SimpleVector_byte64_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return MDArray_byte64_t_O::make_multi_dimensional(dimensions,initialValue,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    else return SimpleMDArray_byte64_t_O::make_multi_dimensional(dimensions,initialValue,_Nil<T_O>());

  } else if (element_type == _sym_size_t) {
    size_t initialValue = SimpleVector_size_t_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return MDArray_size_t_O::make_multi_dimensional(dimensions,initialValue,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    else return SimpleMDArray_size_t_O::make_multi_dimensional(dimensions,initialValue,_Nil<T_O>());

  } else if (element_type == cl::_sym_fixnum) {
    Fixnum initialValue = SimpleVector_fixnum_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return MDArray_fixnum_O::make_multi_dimensional(dimensions,initialValue,displacedTo,displacedTo.notnilp(),displacedIndexOffset);
    else return SimpleMDArray_fixnum_O::make_multi_dimensional(dimensions,initialValue,_Nil<T_O>());

  }
  SIMPLE_ERROR(BF("Handle creation of multi-dimensional array of type %s") % _rep_(element_type));
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


CL_DEFUN void core__verify_mdarray_layout(size_t FillPointerOfLengthOfDummy_offset, size_t ArrayTotalSize_offset, size_t Data_offset, size_t DisplacedIndexOffset_offset, size_t Flags_offset, size_t rank_offset, size_t dimensions_offset)
{
  size_t cxx_FillPointerOfLengthOfDummy_offset = offsetof(MDArray_O,_FillPointerOrLengthOrDummy);
  size_t cxx_ArrayTotalSize_offset = offsetof(MDArray_O,_ArrayTotalSize);
  size_t cxx_Data_offset = offsetof(MDArray_O,_Data);
  size_t cxx_DisplacedIndexOffset_offset = offsetof(MDArray_O,_DisplacedIndexOffset);
  size_t cxx_Flags_offset = offsetof(MDArray_O,_Flags);
  size_t cxx_rank_offset = offsetof(MDArray_O,_Dimensions._Length);
  size_t cxx_dimensions_offset = offsetof(MDArray_O,_Dimensions._Data);
  if (FillPointerOfLengthOfDummy_offset!=cxx_FillPointerOfLengthOfDummy_offset)
    SIMPLE_ERROR(BF("FillPointerOfLengthOfDummy_offset %lu does not match cxx_FillPointerOfLengthOfDummy_offset %lu") % FillPointerOfLengthOfDummy_offset % cxx_FillPointerOfLengthOfDummy_offset );
  if (ArrayTotalSize_offset!=cxx_ArrayTotalSize_offset)
    SIMPLE_ERROR(BF("ArrayTotalSize_offset %lu does not match cxx_ArrayTotalSize_offset %lu") % ArrayTotalSize_offset % cxx_ArrayTotalSize_offset );
  if (Data_offset!=cxx_Data_offset)
    SIMPLE_ERROR(BF("Data_offset %lu does not match cxx_Data_offset %lu") % Data_offset % cxx_Data_offset );
  if (DisplacedIndexOffset_offset!=cxx_DisplacedIndexOffset_offset)
    SIMPLE_ERROR(BF("DisplacedIndexOffset_offset %lu does not match cxx_DisplacedIndexOffset_offset %lu") % DisplacedIndexOffset_offset % cxx_DisplacedIndexOffset_offset );
  if (Flags_offset!=cxx_Flags_offset)
    SIMPLE_ERROR(BF("Flags_offset %lu does not match cxx_Flags_offset %lu") % Flags_offset % cxx_Flags_offset );
  if (rank_offset!=cxx_rank_offset)
    SIMPLE_ERROR(BF("rank_offset %lu does not match cxx_rank_offset %lu") % rank_offset % cxx_rank_offset );
  if (dimensions_offset!=cxx_dimensions_offset)
    SIMPLE_ERROR(BF("dimensions_offset %lu does not match cxx_dimensions_offset %lu") % dimensions_offset % cxx_dimensions_offset );
}



SYMBOL_SC_(CorePkg, make_vector);
SYMBOL_EXPORT_SC_(CorePkg, adjustVector);
SYMBOL_EXPORT_SC_(ClPkg, vectorPush);
SYMBOL_EXPORT_SC_(ClPkg, vectorPushExtend);



CL_LAMBDA(core:&va-rest args);
CL_LISPIFY_NAME(base_string_concatenate);
CL_DEFUN T_sp core__base_string_concatenate(VaList_sp vargs) {
  size_t nargs = vargs->remaining_nargs();
  stringstream ss;
  for (size_t i(0); i < nargs; ++i) {
    T_sp csp = vargs->next_arg();
    String_sp ssp = coerce::stringDesignator(csp);
    ss << ssp->get_std_string();
  }
  return SimpleBaseString_O::make(ss.str());
};



template <typename T1,typename T2>
T_sp template_search_string(const T1& sub, const T2& outer, size_t sub_start, size_t sub_end, size_t outer_start, size_t outer_end)
{
  // The std::search convention is reversed -->  std::search(outer,sub,...)
  const typename T2::simple_element_type* startp = &outer[0];
  const typename T2::simple_element_type* cps = &outer[outer_start];
  const typename T2::simple_element_type* cpe = &outer[outer_end];
  const typename T1::simple_element_type* s_cps = &sub[sub_start];
  const typename T1::simple_element_type* s_cpe = &sub[sub_end];
  const typename T2::simple_element_type* pos = std::search(cps,cpe,s_cps,s_cpe);
  if (pos == cpe ) return _Nil<T_O>();
  // this should return the absolute position starting from 0, not relative to outer_start
  //now that I understood this in pointer arithmethic, compare to the beginning of the string, e.g. index 0
  return clasp_make_fixnum(pos-startp);
}

SYMBOL_EXPORT_SC_(CorePkg,search_string);
CL_LAMBDA(sub sub_start sub_end outer outer_start outer_end);
CL_DOCSTRING("search for the first occurance of sub in outer");
CL_DEFUN T_sp core__search_string(String_sp sub, size_t sub_start, T_sp sub_end, String_sp outer, size_t outer_start, T_sp outer_end) {
  size_t_pair psub = sequenceStartEnd(_sym_search_string,sub->length(),sub_start,sub_end);
  size_t_pair pouter = sequenceStartEnd(_sym_search_string,outer->length(),outer_start,outer_end);
  TEMPLATE_STRING_DISPATCHER(sub,outer,template_search_string,psub.start,psub.end,pouter.start,pouter.end);
};


CL_LISPIFY_NAME("core:split");
CL_DEFUN List_sp core__split(const string& all, const string &chars) {
  vector<string> parts = split(all, chars);
  T_sp first = _Nil<T_O>();
  T_sp* cur = &first;
  for (vector<string>::iterator it = parts.begin(); it != parts.end(); it++) {
    Cons_sp cons = Cons_O::create(Str_O::create(*it), _Nil<T_O>());
    *cur = cons;
    cur = &(cons->_Cdr);
  }
  return first;
}


clasp_elttype clasp_array_elttype(T_sp a)
{
  return gc::As<Array_sp>(a)->elttype();
}




CL_DEFUN T_sp core__copy_to_simple_base_string(T_sp x)
{
 AGAIN:
  if (x.characterp()) {
    x = cl__string(x);
    goto AGAIN;
  } else if (gc::IsA<Symbol_sp>(x)) {
    x = gc::As_unsafe<Symbol_sp>(x)->symbolName();
    goto AGAIN;
  }
#ifdef CLASP_UNICODE
  unlikely_if (gc::IsA<StrWNs_sp>(x)) {
    StrWNs_sp wx = gc::As_unsafe<StrWNs_sp>(x);
    AbstractSimpleVector_sp bsv;
    size_t start, end;
    wx->asAbstractSimpleVectorRange(bsv,start,end);
    SimpleCharacterString_sp swx = gc::As_unsafe<SimpleCharacterString_sp>(bsv);
    SimpleBaseString_sp y = SimpleBaseString_O::make(wx->length());
    for (size_t index(0); index < wx->length(); ++index ) {
      claspCharacter c = (*swx)[index+start];
      if (!clasp_base_char_p(c)) {
        SIMPLE_ERROR(BF("Cannot coerce string %s to a base-string") % _rep_(x));
      }
      (*y)[index] = c;
    }
    return y;
  }
  unlikely_if (gc::IsA<SimpleCharacterString_sp>(x)) {
    SimpleCharacterString_sp sx = gc::As_unsafe<SimpleCharacterString_sp>(x);
    SimpleBaseString_sp y = SimpleBaseString_O::make(sx->length());
    for (size_t index(0); index < sx->length(); ++index ) {
      claspCharacter c = (*sx)[index];
      if (!clasp_base_char_p(c)) {
        SIMPLE_ERROR(BF("Cannot coerce string %s to a base-string") % _rep_(x));
      }
      (*y)[index] = c;
    }
    return y;
  }
#endif
  if (core__base_string_p(x)) {
    String_sp sx = gc::As_unsafe<String_sp>(x);
    AbstractSimpleVector_sp bsv;
    size_t start, end;
    sx->asAbstractSimpleVectorRange(bsv,start,end);
    SimpleBaseString_sp swx = gc::As_unsafe<SimpleBaseString_sp>(bsv);
    SimpleBaseString_sp y = SimpleBaseString_O::make(sx->length());
    memcpy(&(*y)[0],&(*swx)[start],sx->length());
    return y;
  }
  SIMPLE_ERROR(BF("Could not copy %s to simple-base-string") % _rep_(x));
}


template <typename T1>
bool template_fits_in_base_string(const T1& sub, size_t start, size_t end)
{
  // The std::search convention is reversed -->  std::search(outer,sub,...)
  const typename T1::simple_element_type* s_cps = (typename T1::simple_element_type*)sub.rowMajorAddressOfElement_(start); // &sub[start];
  const typename T1::simple_element_type* s_cpe = (typename T1::simple_element_type*)sub.rowMajorAddressOfElement_(end); // &sub[end];
  for ( ; s_cps != s_cpe; ++s_cps ) {
    if ( !clasp_base_char_p(*s_cps) ) {
      return false;
    }
  }
  return true;
}

CL_DEFUN bool core__fits_in_base_string(T_sp tstr) {
  String_sp str = gc::As<String_sp>(tstr);
  TEMPLATE_SINGLE_STRING_DISPATCHER(str,template_fits_in_base_string,0,str->length());
}

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
