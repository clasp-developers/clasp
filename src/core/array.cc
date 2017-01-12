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

#include <clasp/core/common.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/numbers.h>
#include <clasp/core/primitives.h>
#include <clasp/core/designators.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/vectorObjects.h>
#include <clasp/core/array.h>
#include <clasp/core/character.h>
#include <clasp/core/wrappers.h>


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
  SIMPLE_ERROR(BF("The index %lz must be less than %d") % oneIndex % curDimension );
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
        core::lisp_createList(kw::_sym_formatControl, core::lisp_createStr("When calling ~S the argument ~S is not an array with a fill pointer."),
                              kw::_sym_formatArguments, core::lisp_createList(fn_name, thing),
                              kw::_sym_expectedType, core::lisp_createList(cl::_sym_and,cl::_sym_vector,core::lisp_createList(cl::_sym_satisfies,cl::_sym_array_has_fill_pointer_p)),
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


void Array_O::fillInitialContents(T_sp ic) {
  unlikely_if (gc::IsA<MDArrayNs_sp>(ic)) {
    notVectorError(ic);
    UNREACHABLE();
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
  } else if (Vector_sp vic = ic.asOrNull<Vector_O>()) {
    for (size_t i = 0; i < vic->length(); ++i) {
      T_sp obj = vic->rowMajorAref(i);
      this->rowMajorAset(i, obj);
    }
  } else {
    SIMPLE_ERROR(BF("Illegal :INITIAL-CONTENTS"));
  }
}

Symbol_sp Array_O::elementTypeAsSymbol() const {
  // If this fails we need a different way of doing this
  Symbol_sp elementType = this->arrayElementType();
  if ( cl__symbolp(elementType) ) {
    return elementType;
  }
  if (elementType == _lisp->_true()) {
    return cl::_sym_T;
  }
  SIMPLE_ERROR(BF("Handle more array types - the current array type is: %s") % _rep_(elementType));
}

size_t Array_O::index_vector_int(const vector<int> &indices) const {
  size_t offset = 0;
  size_t oneIndex = 0;
  Cons_sp cur;
  size_t idx = 0;
  for (idx = 0; idx < this->rank(); ++idx) {
    if (idx > 0)
      offset *= this->arrayDimension(idx);
    oneIndex = indices[idx];
    offset += oneIndex;
  }
  return offset;
}

size_t Array_O::index_val_(List_sp indices, bool last_value_is_val, T_sp &last_val) const {
  size_t rank = this->rank(); 
  size_t offset = 0;
  size_t idx = 0;
  List_sp cur = indices;;
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
      insufficientIndexListError(indices);
    }
    cur = oCdr(cur);
  }
  if (last_value_is_val) {
    LIKELY_if (cur.consp()) {
      last_val = oCar(cur);
      cur = oCdr(cur);
    } else {
      missingValueListError(indices);
    }
  }
  unlikely_if (cur.consp()) {
    tooManyIndicesListError(indices);
  }
  return offset;
}

size_t Array_O::index_val_(VaList_sp indices, bool last_value_is_val, T_sp &last_val) const {
  size_t indices_passed = indices->remaining_nargs() - (last_value_is_val ? 1 : 0);
  size_t rank = this->rank();
  unlikely_if (indices_passed!=rank) insufficientIndexListError(core__list_from_va_list(indices));
  if (last_value_is_val) {
    unlikely_if (indices->remaining_nargs()!=(rank+1)) {
      if (indices->remaining_nargs()<(rank+1)) {
        missingValueListError(core__list_from_va_list(indices));
      }
      tooManyIndicesListError(core__list_from_va_list(indices));
    }
  }
  size_t offset = 0;
  size_t idx = 0;
  size_t idxEnd(indices_passed);
  for ( ; idx < idxEnd; ++idx) {
    core::T_sp one = indices->next_arg();
    size_t curDimension = this->arrayDimension(idx);
    LIKELY_if (one.fixnump()) {
      size_t oneIndex = one.unsafe_fixnum();
      unlikely_if (oneIndex < 0 || oneIndex >= curDimension) {
        badIndexError(oneIndex,curDimension);
      }
      offset = offset * curDimension + oneIndex;
    }
  }
  if (last_value_is_val) {
    last_val = indices->next_arg();
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
  return (gc::IsA<MDArray_sp>(array));
}


CL_LISPIFY_NAME("cl:array-dimension");
CL_DEFUN size_t cl__arrayDimension(Array_sp array, size_t idx)
{
  if (idx >= array->rank()) {
    SIMPLE_ERROR(BF("array-dimension index %lu is out of bounds - must be less than %lu") % idx % array->rank());
  }
  return array->arrayDimension(idx);
}

// ------------------------------------------------------------
//
// MDArray_O
//
//


// Constructor
MDArray_O::MDArray_O(size_t rank,
                     List_sp dimensions,
                     T_sp fillPointer,
                     T_sp displacedTo,
                     size_t displacedIndexOffset )
  : _FillPointerOrLengthOrDummy(0xDEADBEEF01234567),
    _DisplacedIndexOffset(displacedIndexOffset),
    _DisplacedToP(displacedTo.notnilp()),
    _FillPointerP(fillPointer.fixnump()),
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
  this->_ArrayTotalSize = arrayTotalSize;
  if ( rank == 1 ) {
    if ( fillPointer.fixnump() ) {
      this->_FillPointerOrLengthOrDummy = fillPointer.unsafe_fixnum();
    } else {
      this->_FillPointerOrLengthOrDummy = arrayTotalSize;
    }
  } else {
     // For MDArray leave dummy val
  }
  if ( displacedTo.notnilp() ) {
    this->set_data(gc::As<Array_sp>(displacedTo));
    // _DisplacedIndexOffset was set above
  } else {
     /* this->_Data must be initialized by caller = core__make_vector(elementType, arrayTotalSize,false,_Nil<T_O>(),_Nil<T_O>(),0,initial_element,true); */
    this->_DisplacedIndexOffset = 0;
    // this->_DisplacedToP was set above
  }
}


void MDArray_O::set_data(Array_sp a) {
  this->_Data = a;
}




// ----------------------------------------------------------------------
//

CL_LISPIFY_NAME("cl:array-element-type");
CL_DEFUN  T_sp cl__arrayElementType(Array_sp array)
{
  return array->arrayElementType();
}

CL_LAMBDA(array &va-rest core::indices);
CL_LISPIFY_NAME("cl:arrayRowMajorIndex");
CL_DEFUN size_t cl__arrayRowMajorIndex(Array_sp array, VaList_sp indices) {
  return array->arrayRowMajorIndex(indices);
}


CL_LISPIFY_NAME("core:rowMajorAset");
CL_DEFUN void cl__rowMajorAset(Array_sp array, size_t idx, T_sp value)
{
  array->rowMajorAset(idx,value);
}

CL_LISPIFY_NAME("cl:rowMajorAref");
CL_DEFUN  T_sp cl__rowMajorAref(Array_sp array, size_t idx)
{
  return array->rowMajorAref(idx);
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

CL_LAMBDA(dest destStart orig origStart len);
CL_DECLARE();
CL_DOCSTRING("copy_subarray");
CL_DEFUN void core__copy_subarray(Array_sp dest, Fixnum_sp destStart, Array_sp orig, Fixnum_sp origStart, Fixnum_sp len) {
  // TODO: THIS NEEDS TO BE OPTIMIZED FOR DIFFERENT TYPES OF ARRAYS!!!!!!!
  //       Currently this is very inefficient
  intptr_t iLen = unbox_fixnum(len);
  if (iLen == 0)
    return;
  ASSERTF(dest->rank() == 1, BF("dest array must be rank 1 - instead it is %d") % dest->rank());
  ASSERTF(orig->rank() == 1, BF("orig array must be rank 1 - instead it is %d") % orig->rank());
  intptr_t iDestStart = unbox_fixnum(destStart);
  intptr_t iOrigStart = unbox_fixnum(origStart);
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


CL_LAMBDA(array &va-rest indices-value);
CL_DECLARE();
CL_DOCSTRING("aset");
CL_DEFUN T_sp core__aset(Array_sp array, VaList_sp vargs) {
  T_sp last_val;
  cl_index rowMajorIndex = array->index_val_(vargs,true,last_val);
  array->rowMajorAset(rowMajorIndex,last_val);
  return last_val;
};

CL_LISPIFY_NAME("cl:aref");
CL_LAMBDA(array &va-rest core::indices);
CL_DEFUN T_sp cl__aref(Array_sp array, VaList_sp vargs)
{
  cl_index rowMajorIndex = array->arrayRowMajorIndex(vargs);
  return array->rowMajorAref(rowMajorIndex);
}

CL_LAMBDA(array &va-rest indices);
CL_LISPIFY_NAME("core:index");
CL_DEFUN gc::Fixnum core__index(Array_sp array, VaList_sp indices) {
  T_sp dummy;
  return array->index_val_(indices, false, dummy);
}


CL_LAMBDA(array &rest core::indices-val);
CL_DOCSTRING("Setter for aref");
CL_LISPIFY_NAME("core:array-setf-aref");
CL_DEFUN T_sp core__setf_aref(Array_sp array, List_sp indices_val) {
  T_sp last_val;
  cl_index idx = array->index_val_(indices_val,true,last_val);
  array->rowMajorAset(idx,last_val);
  return last_val;
};



CL_LISPIFY_NAME("cl:svref");
CL_DEFUN T_sp cl__svref(SimpleVector_sp simple_vector, size_t idx)
{
  return simple_vector->rowMajorAref(idx);
}

CL_LISPIFY_NAME("core:setf-svref");
CL_DEFUN T_sp core__setf_svref(SimpleVector_sp simple_vector, cl_index idx, T_sp val)
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
  SimpleString_sp result = core__make_vector(str->arrayElementType(),str->length(),false);
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
  SimpleString_sp result = core__make_vector(str->arrayElementType(),str->length(),false);
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
  typedef typename T::value_type CharacterType;
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
  return _lisp->_true();
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
  return make_fixnum((int)(cp1.offset()));
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
  return make_fixnum((int)(cp1.offset()));
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_LE_(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2) {
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
    goto RETURN_TRUE;
  goto RETURN_FALSE;
 END_STRING2:
 RETURN_FALSE:
  return _Nil<T_O>();
 RETURN_TRUE:
  return make_fixnum((int)(cp1.offset()));
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_GE_(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2) {
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
  if (num2 == 0)
    goto RETURN_TRUE;
  goto RETURN_FALSE;
 END_STRING2:
 RETURN_FALSE:
  return _Nil<T_O>();
 RETURN_TRUE:
  return make_fixnum((int)(cp1.offset()));
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
  return _lisp->_true();
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
    char ucp1 = toupper(static_cast<claspCharacter>(*cp1));
    char ucp2 = toupper(static_cast<claspCharacter>(*cp2));
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
  return make_fixnum((int)(cp1.offset()));
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
    char ucp1 = toupper(static_cast<claspCharacter>(*cp1));
    char ucp2 = toupper(static_cast<claspCharacter>(*cp2));
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
  return make_fixnum((int)(cp1.offset()));
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_not_greaterp(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2) {
  StringCharPointer<T1> cp1(&string1,start1);
  StringCharPointer<T2> cp2(&string2,start2);
  size_t num1 = end1 - start1;
  size_t num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    char ucp1 = toupper(static_cast<claspCharacter>(*cp1));
    char ucp2 = toupper(static_cast<claspCharacter>(*cp2));
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
  if (num2 == 0)
    goto RETURN_TRUE;
  goto RETURN_FALSE;
 END_STRING2:
 RETURN_FALSE:
  return _Nil<T_O>();
 RETURN_TRUE:
  return make_fixnum((int)(cp1.offset()));
}

/*! bounding index designator range from 0 to the end of each string */
template <typename T1, typename T2>
T_sp template_string_not_lessp(const T1& string1, const T2& string2, size_t start1, size_t end1, size_t start2, size_t end2) {
  StringCharPointer<T1> cp1(&string1,start1);
  StringCharPointer<T2> cp2(&string2,start2);
  size_t num1 = end1 - start1;
  size_t num2 = end2 - start2;
  while (1) {
    if (num1 == 0)
      goto END_STRING1;
    if (num2 == 0)
      goto END_STRING2;
    char ucp1 = toupper(static_cast<claspCharacter>(*cp1));
    char ucp2 = toupper(static_cast<claspCharacter>(*cp2));
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
 RETURN_FALSE:
  return _Nil<T_O>();
 RETURN_TRUE:
  return make_fixnum((int)(cp1.offset()));
}

inline void setup_string_op_arguments(T_sp string1_desig, T_sp string2_desig,
                                      String_sp &string1, String_sp &string2,
                                      Fixnum_sp start1, T_sp end1,
                                      Fixnum_sp start2, T_sp end2,
                                      size_t &istart1, size_t &iend1,
                                      size_t &istart2, size_t &iend2) {
  string1 = coerce::stringDesignator(string1_desig);
  string2 = coerce::stringDesignator(string2_desig);
  istart1 = MAX(unbox_fixnum(start1), 0);
  iend1 = MIN(end1.nilp() ? cl__length(string1) : unbox_fixnum(gc::As<Fixnum_sp>(end1)), cl__length(string1));
  istart2 = MAX(unbox_fixnum(start2), 0);
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
  if ( element_type == cl::_sym_base_char ) {
    if (!clasp_base_char_p(initial_element)) {
      TYPE_ERROR(initial_element,cl::_sym_base_char);
    }
    SimpleBaseString_sp s = SimpleBaseString_O::make(size.unsafe_fixnum(),initial_element.unsafe_character());
    return s;
  }
  SimpleCharacterString_sp s = SimpleCharacterString_O::make(size.unsafe_fixnum(),initial_element.unsafe_character());
  return s;
}

SYMBOL_EXPORT_SC_(ClPkg,simple_string);

CL_LAMBDA(str index);
CL_DECLARE();
CL_DOCSTRING("CLHS schar");
CL_DEFUN Character_sp cl__schar(BaseSimpleVector_sp str, size_t idx) {
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

CL_LAMBDA(str index c);
CL_DECLARE();
CL_DOCSTRING("CLHS (setf char)");
CL_DEFUN Character_sp core__char_set(String_sp str, size_t idx, Character_sp c) {
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

CL_LAMBDA(str index c);
CL_DECLARE();
CL_DOCSTRING("CLHS schar");
CL_DEFUN Character_sp core__schar_set(String_sp str, size_t idx, Character_sp c) {
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
    claspCharacter c = clasp_as_claspCharacter(str->rowMajorAref(cur));
    LOG(BF("fsmInteger str[%ld] -> c = [%d/%c]") % cur  % c % c );
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
          LOG(BF("Hit junk at %ld\n") % cur);
          state = ijunk;
          break;
        }
        result = result * radix + idigit;
        ++numDigits;
        state = inum;
        break;
      }
      state = ijunk;
      break;
    }
    case inum: {
      if (isspace(c)) {
        if (junkAllowed) {
          state = ijunk; // itrailspace;
          break;
        }
        state = idone;
        break;
      } else if (isalnum(c)) {
        cl_index idigit = fsmIntegerDigit(c, radix);
        if (idigit < 0 || idigit >= radix) {
          state = ijunk;
          break;
        }
        result = result * radix + idigit;
        ++numDigits;
        state = inum;
        break;
      }
      state = ijunk;
      break;
    }
    case itrailspace: {
      if (junkAllowed) {
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
  LOG(BF("Returning with cur=%ld") % cur);
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
      LOG(BF("Returning parse-integer with result = %s  cur = %ld") % _rep_(iresult) % cur );
      return (Values(iresult, make_fixnum(cur)));
    } else {
      return (Values(_Nil<T_O>(), make_fixnum(cur)));
    }
  }
  PARSE_ERROR(SimpleBaseString_O::make("Could not parse integer from ~S"), Cons_O::create(str));
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
  } else if (gc::IsA<VectorNs_sp>(other)) {
    VectorNs_sp vother = gc::As_unsafe<VectorNs_sp>(other);
    if (vother->length()!=this->length()) return false;
    for (size_t i(0),iEnd(this->length()); i<iEnd; ++i ) {
      if (!cl__equalp(this->rowMajorAref(i),vother->rowMajorAref(i))) return false;
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
#if 0
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
    } else {
      auto so = gc::As_unsafe<StrWNs_sp>(other);
      return template_string_EQ_equal(*this,*so,0,this->length(),0,so->length());
    }
  }
  return false;
#endif
};

bool SimpleBaseString_O::equalp(T_sp other) const {
  if (&*other==this) return true;
  if (!other.generalp()) return false;
  if (!cl__stringp(other)) return false;
  String_sp sother = gc::As_unsafe<String_sp>(other);
  TEMPLATE_HALF_STRING_DISPATCHER(this,sother,template_string_equalp_bool,0,this->length(),0,sother->length());
}


#if 0
void SimpleString8_O::__write__(T_sp stream) const {
  cl_index ndx;
  if (!clasp_print_escape() && !clasp_print_readably()) {
    for (ndx = 0; ndx < this->length(); ndx++) {
      clasp_write_char((*this)[ndx], stream);
    }
  } else {
    clasp_write_char('"', stream);
    for (ndx = 0; ndx < this->length(); ndx++) {
      char c = (*this)[ndx];
      if (c == '"' || c == '\\')
        clasp_write_char('\\', stream);
      clasp_write_char(c, stream);
    }
    clasp_write_char('"', stream);
  }
}
#endif


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
    } else {
      auto so = gc::As_unsafe<StrWNs_sp>(other);
      return template_string_EQ_equal(*this,*so,0,this->length(),0,so->length());
    }
  }
  return false;
};

bool SimpleCharacterString_O::equalp(T_sp other) const {
  if (&*other==this) return true;
  if (!other.generalp()) return false;
  if (!cl__stringp(other)) return false;
  String_sp sother = gc::As_unsafe<String_sp>(other);
  TEMPLATE_HALF_STRING_DISPATCHER(this,sother,template_string_equalp_bool,0,this->length(),0,sother->length());
}

std::string SimpleCharacterString_O::get_std_string() const {
  T_sp stream = cl__make_string_output_stream(cl::_sym_base_char);
  for (size_t i(0),iEnd(this->length()); i<iEnd; ++i ) clasp_write_char((*this)[i],stream);
  String_sp s = gc::As_unsafe<String_sp>(cl__get_output_stream_string(stream));
  return s->get_std_string();
}

std::string SimpleCharacterString_O::__repr__() const {
  return this->get_std_string();
}

// ------------------------------------------------------------
//
// Class SimpleVector_O
//

bool SimpleVector_O::equalp(T_sp other) const {
  if (&*other==this) return true;
  if (!other.generalp()) return false;
  if (gc::IsA<SimpleVector_sp>(other)) {
    SimpleVector_sp svother = gc::As_unsafe<SimpleVector_sp>(other);
    if (svother->length()!=this->length()) return false;
    for (size_t i(0),iEnd(this->length()); i<iEnd; ++i ) {
      if (!cl__equalp((*this)[i],(*svother)[i])) return false;
    }
    return true;
  } else if (gc::IsA<VectorTNs_sp>(other)) {
    VectorTNs_sp vother = gc::As_unsafe<VectorTNs_sp>(other);
    if (vother->length()!=this->length()) return false;
    for (size_t i(0),iEnd(this->length()); i<iEnd; ++i ) {
      if (!cl__equalp((*this)[i],(*vother)[i])) return false;
    }
    return true;
  }
  return this->AbstractSimpleVector_O::equalp(other);
}


// ------------------------------------------------------------
//
// Class SimpleDoubleVector_O
//



// ----------------------------------------------------------------------
//
// Class SimpleBitVector
//

SimpleBitVector_sp SimpleBitVector_O::make(const string& bv) {
  size_t dim = bv.size();
  SimpleBitVector_sp x = SimpleBitVector_O::make(dim);
  for (int i = 2; i < dim; i++) {
    char elt = bv[i];
    x->setBit(i,elt-'0');
  }
  return x;
}

Array_sp SimpleBitVector_O::unsafe_subseq(size_t start, size_t end) const {
  SimpleBitVector_sp sbv = SimpleBitVector_O::make(end-start);
  for (size_t i(0),iEnd(end-start);i<iEnd;++i) {
    sbv->setBit(i,this->testBit(start+i));
  }
  return sbv;
}

Array_sp SimpleBitVector_O::unsafe_setf_subseq(size_t start, size_t end, Array_sp other) {
  if (SimpleBitVector_sp sbv = other.asOrNull<SimpleBitVector_O>()) {
      // TODO: Write specialized versions of this to speed it up
    for ( size_t i(start),ni(0); i<end; ++i,++ni ) {
      this->setBit(i,sbv->testBit(ni));
    }
    return this->asSmartPtr();
  } else if (BitVectorNs_sp bv = other.asOrNull<BitVectorNs_O>()) {
    BaseSimpleVector_sp bsv;
    size_t ostart, oend;
    bv->asBaseSimpleVectorRange(bsv,ostart,oend);
    SimpleBitVector_sp sbv = gc::As_unsafe<SimpleBitVector_sp>(bsv);
    for ( size_t i(start),io(ostart); i<end; ++i, ++io) {
      this->setBit(i,sbv->testBit(io));
    }
    return this->asSmartPtr();
  }
  TYPE_ERROR(other,cl::_sym_bit_vector);
}

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
bool SimpleBitVector_O::equal(T_sp other) const {
  if (this == &*other) return true;
  if (SimpleBitVector_sp sbv = other.asOrNull<SimpleBitVector_O>()) {
    if (this->length()!=sbv->length()) return false;
    return ranged_bit_vector_EQ_(*this,*sbv,0,this->length(),0,sbv->length());
  } else if (BitVectorNs_sp bvns = other.asOrNull<BitVectorNs_O>()) {
    if (this->length()!=sbv->length()) return false;
    BaseSimpleVector_sp sv;
    size_t start, end;
    bvns->asBaseSimpleVectorRange(sv,start,end);
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

// ------------------------------------------------------------
//
// Class VectorNs
//

namespace core {
void VectorNs_O::ensureSpaceAfterFillPointer(T_sp init_element, size_t size) {
  ASSERT(this->_FillPointerP);
  cl_index left = this->arrayTotalSize() - this->fillPointer();
  if (left < size) {
    this->internalAdjustSize_((size-left)+this->_ArrayTotalSize,init_element,true);
  }
}

bool VectorNs_O::equalp(T_sp other) const {
  if (&*other==this) return true;
  if (!other.generalp()) return false;
  if (gc::IsA<AbstractSimpleVector_sp>(other)) {
    AbstractSimpleVector_sp svother = gc::As_unsafe<AbstractSimpleVector_sp>(other);
    if (svother->length()!=this->length()) return false;
    for (size_t i(0),iEnd(this->length()); i<iEnd; ++i ) {
      if (!cl__equalp(this->rowMajorAref(i),svother->rowMajorAref(i))) return false;
    }
    return true;
  } else if (gc::IsA<VectorNs_sp>(other)) {
    VectorNs_sp vother = gc::As_unsafe<VectorNs_sp>(other);
    if (vother->length()!=this->length()) return false;
    for (size_t i(0),iEnd(this->length()); i<iEnd; ++i ) {
      if (!cl__equalp(this->rowMajorAref(i),vother->rowMajorAref(i))) return false;
    }
    return true;
  }
  return false;
}



T_sp VectorNs_O::vectorPush(T_sp newElement) {
  unlikely_if (!this->_FillPointerP) noFillPointerError(cl::_sym_vectorPush,this->asSmartPtr());
  cl_index idx = this->_FillPointerOrLengthOrDummy;
  unlikely_if (idx >= this->_ArrayTotalSize) {
    return _Nil<T_O>();
  }
  this->_Data->rowMajorAset(idx+this->_DisplacedIndexOffset,newElement);
  ++this->_FillPointerOrLengthOrDummy;
  return clasp_make_fixnum(idx);
}

SYMBOL_EXPORT_SC_(ClPkg,vectorPushExtend);
Fixnum_sp VectorNs_O::vectorPushExtend(T_sp newElement, size_t extension) {
  unlikely_if (!this->_FillPointerP) noFillPointerError(cl::_sym_vectorPushExtend,this->asSmartPtr());
  cl_index idx = this->_FillPointerOrLengthOrDummy;
  unlikely_if (idx >= this->_ArrayTotalSize) {
    if (extension <= 0) extension = 32;
    cl_index new_size = this->_ArrayTotalSize+extension;
    unlikely_if (!cl::_sym_adjust_array || !cl::_sym_adjust_array->boundP()) {
      this->internalAdjustSize_(new_size);
    } else {
      eval::funcall(cl::_sym_adjust_array,this->asSmartPtr(),clasp_make_fixnum(new_size),cl::_sym_fill_pointer,clasp_make_fixnum(this->_FillPointerOrLengthOrDummy));
    }
  }
  this->_Data->rowMajorAset(idx+this->_DisplacedIndexOffset,newElement);
  ++this->_FillPointerOrLengthOrDummy;
  return make_fixnum(idx);
}


};
// ------------------------------------------------------------
//
// Class VectorTNs
//

VectorTNs_sp VectorTNs_O::make(size_t dimension, T_sp initElement, T_sp fillPointer, T_sp displacedTo, size_t displacedIndexOffset ) {
  GC_ALLOCATE_VARIADIC(VectorTNs_O,s,dimension,fillPointer,displacedTo,displacedIndexOffset);
  LIKELY_if (displacedTo.nilp()) {
    SimpleVector_sp sb = SimpleVector_O::make(dimension,initElement,true);
    s->set_data(sb);
  }
  return s;
}

VectorTNs_sp VectorTNs_O::create(const gc::Vec0<T_sp>& objs) {
  size_t len = objs.size();
  SimpleVector_sp ss = SimpleVector_O::make(len,_Nil<T_O>(),true,len,&(objs[0]));
  VectorTNs_sp result = VectorTNs_O::make(len);
  result->set_data(ss);
  return result;
}

bool VectorTNs_O::equalp(T_sp other) const {
  if (&*other==this) return true;
  if (!other.generalp()) return false;
  if (gc::IsA<SimpleVector_sp>(other)) {
    SimpleVector_sp svother = gc::As_unsafe<SimpleVector_sp>(other);
    if (svother->length()!=this->length()) return false;
    for (size_t i(0),iEnd(this->length()); i<iEnd; ++i ) {
      if (!cl__equalp((*this)[i],(*svother)[i])) return false;
    }
    return true;
  } else if (gc::IsA<VectorTNs_sp>(other)) {
    VectorTNs_sp vother = gc::As_unsafe<VectorTNs_sp>(other);
    if (vother->length()!=this->length()) return false;
    for (size_t i(0),iEnd(this->length()); i<iEnd; ++i ) {
      if (!cl__equalp((*this)[i],(*vother)[i])) return false;
    }
    return true;
  }
  return false;
}
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
    } else {
      auto so = gc::As_unsafe<StrWNs_sp>(other);
      return template_string_EQ_equal(*this,*so,0,this->length(),0,so->length());
    }
  }
  return false;
};

// Creators - depreciate these once the new array stuff is working better
Str8Ns_sp Str8Ns_O::create(const string& nm) {
  auto ss = SimpleBaseString_O::make(nm.size(),'\0',true,nm.size(),(const claspChar*)nm.c_str());
  GC_ALLOCATE_VARIADIC(Str8Ns_O,result,nm.size(),_Nil<T_O>(),_Nil<T_O>(),0);
  result->set_data(ss);
  return result;
}

Str8Ns_sp Str8Ns_O::create(const char* nm,size_t len) {
  SimpleBaseString_sp ss = SimpleBaseString_O::make(len,'\0',true,len,(const claspChar*)nm);
  GC_ALLOCATE_VARIADIC(Str8Ns_O,result,len,_Nil<T_O>(),_Nil<T_O>(),0);
  result->set_data(ss);
  return result;
}

Str8Ns_sp Str8Ns_O::create(const char* nm) {
  size_t len = strlen(nm);
  return Str8Ns_O::create(nm,len);
}

Str8Ns_sp Str8Ns_O::create(size_t len) {
  SimpleBaseString_sp ss = SimpleBaseString_O::make(len,'\0',true);
  GC_ALLOCATE_VARIADIC(Str8Ns_O,result,len,_Nil<T_O>(),_Nil<T_O>(),0);
  result->set_data(ss);
  return result;
}

Str8Ns_sp Str8Ns_O::create(Str8Ns_sp other) {
  size_t len = other->length();
  SimpleBaseString_sp ss = SimpleBaseString_O::make(len,'\0',true,len,&(*other)[0]);
  GC_ALLOCATE_VARIADIC(Str8Ns_O,result,len,_Nil<T_O>(),_Nil<T_O>(),0);
  result->set_data(ss);
  return result;
}

SYMBOL_EXPORT_SC_(CorePkg,vectorPushExtend_claspCharacter);
void Str8Ns_O::vectorPushExtend_claspChar(claspChar newElement, size_t extension) {
  unlikely_if (!this->_FillPointerP) noFillPointerError(core::_sym_vectorPushExtend_claspCharacter,this->asSmartPtr());
  cl_index idx = this->_FillPointerOrLengthOrDummy;
  unlikely_if (idx >= this->_ArrayTotalSize) {
    if (extension <= 0) extension = 32;
    cl_index new_size = this->_ArrayTotalSize+extension;
    unlikely_if (!cl::_sym_adjust_array || !cl::_sym_adjust_array->boundP()) {
      this->internalAdjustSize_(new_size);
    } else {
      eval::funcall(cl::_sym_adjust_array,this->asSmartPtr(),clasp_make_fixnum(new_size),cl::_sym_fill_pointer,clasp_make_fixnum(this->_FillPointerOrLengthOrDummy));
    }
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
    } else {
      auto so = gc::As_unsafe<StrWNs_sp>(other);
      return template_string_EQ_equal(*this,*so,0,this->length(),0,so->length());
    }
  }
  return false;
};

std::string StrWNs_O::get_std_string() const {
  T_sp stream = cl__make_string_output_stream(cl::_sym_base_char);
  for (size_t i(0),iEnd(this->length()); i<iEnd; ++i ) clasp_write_char((*this)[i],stream);
  String_sp s = gc::As_unsafe<String_sp>(cl__get_output_stream_string(stream));
  return s->get_std_string();
}

std::string StrWNs_O::__repr__() const {
  return this->get_std_string();
}

SYMBOL_EXPORT_SC_(CorePkg,vectorPushExtend_claspCharacter);
void StrWNs_O::vectorPushExtend_claspCharacter(claspCharacter newElement, size_t extension) {
  unlikely_if (!this->_FillPointerP) noFillPointerError(core::_sym_vectorPushExtend_claspCharacter,this->asSmartPtr());
  cl_index idx = this->_FillPointerOrLengthOrDummy;
  unlikely_if (idx >= this->_ArrayTotalSize) {
    if (extension <= 0) extension = 32;
    cl_index new_size = this->_ArrayTotalSize+extension;
    unlikely_if (!cl::_sym_adjust_array || !cl::_sym_adjust_array->boundP()) {
      this->internalAdjustSize_(new_size);
    } else {
      eval::funcall(cl::_sym_adjust_array,this->asSmartPtr(),clasp_make_fixnum(new_size),cl::_sym_fill_pointer,clasp_make_fixnum(this->_FillPointerOrLengthOrDummy));
    }
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
// Class AbstractVectorNs
//

// ------------------------------------------------------------
//
// Class VectorTNs
//

// ------------------------------------------------------------
//
// Class BitVectorNs
//
//

void BitVectorNs_O::internalAdjustSize_(size_t size, T_sp initElement, bool initElementSupplied) {
  if (size == this->_ArrayTotalSize) return;
  BaseSimpleVector_sp basesv;
  size_t start, end;
  this->asBaseSimpleVectorRange(basesv,start,end);
  gctools::smart_ptr<simple_type> sv = gc::As_unsafe<gctools::smart_ptr<simple_type>>(basesv);
  size_t initialContentsSize = MIN(this->length(),size);
  smart_ptr_type newData = simple_type::make(size,0);
  for (size_t i(0),iEnd(initialContentsSize); i<iEnd; ++i ) {
    newData->setBit(i,this->testBit(i+start));
  }
  this->set_data(newData);
  this->_ArrayTotalSize = size;
  if (!this->_FillPointerP) this->_FillPointerOrLengthOrDummy = size;
  this->_DisplacedIndexOffset = 0;
  this->_DisplacedToP = false;
}


Array_sp BitVectorNs_O::reverse() const {
  BaseSimpleVector_sp basesv;
  size_t start, end;
  this->asBaseSimpleVectorRange(basesv,start,end);
  gctools::smart_ptr<simple_type> sv = gc::As_unsafe<gctools::smart_ptr<simple_type>>(basesv);
  return ranged_bit_vector_reverse(sv,start,end);
}

Array_sp BitVectorNs_O::nreverse() {
  BaseSimpleVector_sp basesv;
  size_t start, end;
  this->asBaseSimpleVectorRange(basesv,start,end);
  gctools::smart_ptr<simple_type> sv = gc::As_unsafe<gctools::smart_ptr<simple_type>>(basesv);
  ranged_bit_vector_nreverse(sv,start,end);
  return this->asSmartPtr();
}

bool BitVectorNs_O::equal(T_sp other) const {
  if (&*other==this) return true;
  if (!other.generalp()) return false;
  if (BitVectorNs_sp strns = other.asOrNull<BitVectorNs_O>()) {
    BaseSimpleVector_sp bme;
    size_t mstart, mend;
    this->asBaseSimpleVectorRange(bme,mstart,mend);
    simple_type* me = reinterpret_cast<simple_type*>(&*bme);
    BaseSimpleVector_sp bso;
    size_t ostart, oend;
    strns->asBaseSimpleVectorRange(bso,ostart,oend);
    simple_type* so = reinterpret_cast<simple_type*>(&*bso);
    return ranged_bit_vector_EQ_(*me,*so,mstart,mend,ostart,oend);
  } else if (SimpleBitVector_sp ss = other.asOrNull<SimpleBitVector_O>()) {
    BaseSimpleVector_sp bme;
    size_t mstart, mend;
    this->asBaseSimpleVectorRange(bme,mstart,mend);
    simple_type* me = reinterpret_cast<simple_type*>(&*bme);
    return ranged_bit_vector_EQ_(*me,*ss,mstart,mend,0,ss->length());
  }
  return false;
};


// ----------------------------------------------------------------------
//
// AbstractArrayNs
//


// ------------------------------------------------------------
//
// ArrayNs
namespace core {
ArrayTNs_sp ArrayTNs_O::make(T_sp dim_desig, T_sp initialElement, T_sp displacedTo, size_t displacedIndexOffset) {
  size_t rank = cl__length(dim_desig);
  List_sp dim;
  if (dim_desig.nilp()) {
    dim = dim_desig;
  } else if (cl__atom(dim_desig)) {
    int idim = clasp_to_int(gc::As<Integer_sp>(dim_desig));
    dim = Cons_O::create(make_fixnum(idim));
  } else {
    dim = dim_desig;
  }
  GC_ALLOCATE_VARIADIC(ArrayTNs_O, array, rank, dim, displacedTo, displacedIndexOffset);
  LIKELY_if (displacedTo.nilp()) {
    SimpleVector_sp sb = SimpleVector_O::make(array->arrayTotalSize(),initialElement,true);
    array->set_data(sb);
  }
  return array;
}



};


namespace core {


void StringPushSubString(String_sp buffer, String_sp str, size_t start, size_t end) {
  for ( ; start<end; ++start ) {
    buffer->vectorPushExtend(cl__char(str, start));
  }
}

#if 0
CL_LISPIFY_NAME(push-string);
CL_DEFMETHOD
#endif
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
T_sp str_create(const string &str) { return SimpleBaseString_O::make(str); };
T_sp str_create(const char *str) { return SimpleBaseString_O::make(std::string(str)); };


};



////////////////////////////////////////////////////////////


namespace core {

CL_LAMBDA(&rest args);
CL_DECLARE();
CL_DOCSTRING("vector");
CL_DEFUN Vector_sp cl__vector(List_sp args) {
  VectorObjects_sp vec = VectorObjects_O::make(cl__length(args), _Nil<T_O>());
  vec->fillInitialContents(args);
  return vec;
};
SYMBOL_EXPORT_SC_(ClPkg, subtypep);




CL_LAMBDA(element_type dimension &optional adjustable fill_pointer displaced_to displaced_index_offset initial_element initial_element_supplied_p);
CL_DECLARE();
CL_DOCSTRING("Makes a vector based on the arguments. See si_make_vector in ecl>>array.d");
CL_DEFUN Vector_sp core__make_vector(T_sp element_type,
                                     size_t dimension,
                                     bool adjustable,
                                     T_sp fillPointer,
                                     T_sp displacedTo,
                                     T_sp tdisplacedIndexOffset,
                                     T_sp initialElement,
                                     bool initialElementSuppliedP) {
  size_t displacedIndexOffset = 0;
  if (tdisplacedIndexOffset.fixnump()) {
    displacedIndexOffset = tdisplacedIndexOffset.unsafe_fixnum();
  } else {
    if (tdisplacedIndexOffset.notnilp()) {
      TYPE_ERROR(tdisplacedIndexOffset,Cons_O::createList(cl::_sym_or,cl::_sym_null,cl::_sym_integer));
    }
  }
  if (fillPointer == cl::_sym_T_O) fillPointer = clasp_make_fixnum(dimension);
  if ( fillPointer.notnilp() || displacedTo.notnilp()) adjustable = true;
  if (element_type == cl::_sym_bit) {
    SimpleBitVector_O::value_type init_bit = SimpleBitVector_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return BitVectorNs_O::make(dimension,init_bit,initialElementSuppliedP,fillPointer,displacedTo,displacedIndexOffset);
    return SimpleBitVector_O::make(dimension,init_bit,initialElementSuppliedP);
  } else if (element_type == cl::_sym_base_char
             || element_type == cl::_sym_character) {
    unlikely_if (element_type == cl::_sym_character) {
      claspCharacter initialCharacter = SimpleCharacterString_O::initial_element_from_object(initialElement,initialElementSuppliedP);
      if (adjustable) return StrWNs_O::make(dimension,initialCharacter,initialElementSuppliedP,fillPointer,displacedTo,displacedIndexOffset);
      else return SimpleCharacterString_O::make(dimension,initialCharacter,initialElementSuppliedP);
    }
    claspChar initialChar = SimpleBaseString_O::initial_element_from_object(initialElement,initialElementSuppliedP);
    if (adjustable) return Str8Ns_O::make(dimension,initialChar,initialElementSuppliedP,fillPointer,displacedTo,displacedIndexOffset);
    else return SimpleBaseString_O::make(dimension,initialChar,initialElementSuppliedP);
  } else if ( element_type == cl::_sym_T_O ) {
    if (adjustable) return VectorTNs_O::make(dimension,initialElement,fillPointer,displacedTo,displacedIndexOffset);
    return SimpleVector_O::make(dimension,initialElement,true);
  }
  SIMPLE_ERROR(BF("Handle make-vector :element-type %s") % _rep_(element_type));
};





CL_LAMBDA(dimensions element_type displaced_to displaced_index_offset initial_element initial_element_supplied_p);
CL_DECLARE();
CL_DOCSTRING("Makes a multidimensional array based on the arguments.");
CL_DEFUN MDArray_sp core__make_mdarray(List_sp dimensions,
                                       T_sp element_type,
                                       T_sp displacedTo,
                                       size_t displacedIndexOffset,
                                       T_sp initialElement,
                                       bool initialElementSuppliedP) {
  return ArrayTNs_O::make(dimensions,initialElement,displacedTo,displacedIndexOffset);
};




#if 0 // DEPRECIATED
CL_LAMBDA(array dimensions initial-element); //initial-contents);
CL_DECLARE();
CL_DOCSTRING("adjustVector");
CL_DEFUN T_sp core__adjust_vector(T_sp array, int new_dimensions, T_sp initialElement /*, List_sp initial_contents*/) {
  if (VectorObjects_sp vo = array.asOrNull<VectorObjects_O>()) {
    vo->adjust(initialElement,/*initial_contents,*/ new_dimensions);
    return vo;
  }
  IMPLEMENT_MEF(BF("Implement adjustVector for: %s") % _rep_(array));
};
#endif

  

// ------------------------------------------------------------
// ------------------------------------------------------------







CL_LAMBDA(newElement vector);
CL_DECLARE();
CL_DOCSTRING("vectorPush");
CL_DEFUN T_sp cl__vector_push(T_sp newElement, Vector_sp vec) {
  return vec->vectorPush(newElement);
};

CL_LAMBDA(newElement vector &optional (exension 16));
CL_DECLARE();
CL_DOCSTRING("vectorPushExtend");
CL_DEFUN Fixnum_sp cl__vector_push_extend(T_sp newElement, Vector_sp vec, size_t extension) {
  return vec->vectorPushExtend(newElement, extension);
}




SYMBOL_SC_(CorePkg, make_vector);
SYMBOL_EXPORT_SC_(CorePkg, adjustVector);
SYMBOL_EXPORT_SC_(ClPkg, vectorPush);
SYMBOL_EXPORT_SC_(ClPkg, vectorPushExtend);



CL_LAMBDA(&va-rest args);
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
  const typename T2::value_type* cps = &outer[outer_start];
  const typename T2::value_type* cpe = &outer[outer_end];
  const typename T1::value_type* s_cps = &sub[sub_start];
  const typename T1::value_type* s_cpe = &sub[sub_end];
  const typename T2::value_type* pos = std::search(cps,cpe,s_cps,s_cpe);
  if (pos == cpe ) return _Nil<T_O>();
  return clasp_make_fixnum(pos-cps);
}

SYMBOL_EXPORT_SC_(CorePkg,search_string);
CL_LAMBDA(sub sub_start sub_end outer outer_start outer_end);
CL_DOCSTRING("search for the first occurance of sub in outer");
CL_DEFUN T_sp core__search_string(String_sp sub, size_t sub_start, T_sp sub_end, String_sp outer, size_t outer_start, T_sp outer_end) {
  size_t_pair psub = sequenceStartEnd(core::_sym_search_string,sub->length(),sub_start,sub_end);
  size_t_pair pouter = sequenceStartEnd(core::_sym_search_string,outer->length(),outer_start,outer_end);
  TEMPLATE_STRING_DISPATCHER(sub,outer,template_search_string,psub.start,psub.end,pouter.start,pouter.end);
};


CL_LISPIFY_NAME("core:split");
CL_DEFUN List_sp core__split(const string& all, const string &chars) {
  vector<string> parts = core::split(all, chars);
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
    BaseSimpleVector_sp bsv;
    size_t start, end;
    wx->asBaseSimpleVectorRange(bsv,start,end);
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
    BaseSimpleVector_sp bsv;
    size_t start, end;
    sx->asBaseSimpleVectorRange(bsv,start,end);
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
  const typename T1::value_type* s_cps = &sub[start];
  const typename T1::value_type* s_cpe = &sub[end];
  for ( ; s_cps != s_cpe; ++s_cps ) {
    if ( !clasp_base_char_p(*s_cps) ) {
      return false;
    }
  }
  return true;
}

bool core__fits_in_base_string(T_sp tstr) {
  String_sp str = gc::As<String_sp>(tstr);
  TEMPLATE_SINGLE_STRING_DISPATCHER(str,template_fits_in_base_string,0,str->length());
}

}; /* core */
