/*
    File: lispVector.cc
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
#include <clasp/core/environment.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/lispVector.h>
#include <clasp/core/bitVector.h>
#include <clasp/core/print.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/str.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/vectorObjects.h>
#include <clasp/core/character.h>
#include <clasp/core/wrappers.h>
namespace core {

// ----------------------------------------------------------------------
//

void noFillPointerError() {
  SIMPLE_ERROR(BF("This vector does not have a fill pointer"));
}

CL_LAMBDA(&rest args);
CL_DECLARE();
CL_DOCSTRING("vector");
CL_DEFUN Vector_sp cl__vector(List_sp args) {
  VectorObjects_sp vec = VectorObjects_O::make(_Nil<T_O>(), cl__length(args), cl::_sym_T_O);
  vec->fillInitialContents(args);
  return vec;
};
SYMBOL_EXPORT_SC_(ClPkg, subtypep);

CL_LAMBDA(element-type dimension &optional adjustable (fill-pointer t) displaced-to displaced-index-offset initial-element initial-element-supplied-p);
CL_DECLARE();
CL_DOCSTRING("Makes a vector based on the arguments. See si_make_vector in ecl>>array.d");
CL_DEFUN Vector_sp core__make_vector(T_sp element_type,
                                     size_t dimension,
                                     bool adjustable,
                                     T_sp fill_pointer,
                                     T_sp displaced_to,
                                     cl_index displaced_index_offset,
                                     T_sp initial_element,
                                     bool initial_element_supplied_p) {
  // FIXME: make compatible with the new code.
  IMPLEMENT_ME(BF("This has to be brought up to speed with the new code"));
  ASSERTF(displaced_to.nilp(), BF("Add support for make-vector :displaced-to"));
  ASSERTF(displaced_index_offset.nilp() || unbox_fixnum(gc::As<Fixnum_sp>(displaced_index_offset)) == 0, BF("Add support for make-vector non-zero :displaced-index-offset "));
  if (fill_pointer == cl::_sym_T_O) fill_pointer = clasp_make_fixnum(dimension);
  if (element_type == cl::_sym_bit) {
    uint init_bit = 0;
    if (initial_element_supplied_p) {
      if (initial_element.fixnump() ) {
        if (initial_element.unsafe_fixnum() < 0 ||
            initial_element.unsafe_fixnum() > 1 ) {
            init_bit = initial_element.unsafe_fixnum();
          goto GOOD_BIT;
        }
      }
      TYPE_ERROR(initial_element,cl::_sym_bit);
    }
  GOOD_BIT:
    return make_bit_vector(init_bit, dimension,adjustable,fill_pointer, displaced_to, displaced_index_offset);
  } else if (element_type == cl::_sym_base_char
             || element_type == cl::_sym_character
             || element_type == cl::_sym_standard_char
             || element_type == cl::_sym_extended_char) {
    return make_string(element_type, dimension,adjustable,fill_pointer, displaced_to, displaced_index_offset, initial_element, initial_element_supplied_p);
  } else {
    if ((element_type).consp()) {
      // For type = '(unsigned-byte XXX) set initial_element if it hasn't been set
      Cons_sp cet = gc::As<Cons_sp>(element_type);
      if (oCar(cet) == cl::_sym_UnsignedByte && initial_element.nilp()) {
        initial_element = make_fixnum(0);
      }
    }
    return make_vector_objects(element_type, dimension, initial_element, initial_element_supplied_p.notnilp(), adjustable, fill_pointer, displaced_to, displaced_index_offset);
  }
  SIMPLE_ERROR(BF("Handle make-vector :element-type %s") % _rep_(element_type));
};

CL_LAMBDA(array dimensions initial-element); //initial-contents);
CL_DECLARE();
CL_DOCSTRING("adjustVector");
CL_DEFUN T_sp core__adjust_vector(T_sp array, int new_dimensions, T_sp initial_element /*, List_sp initial_contents*/) {
  if (VectorObjects_sp vo = array.asOrNull<VectorObjects_O>()) {
    vo->adjust(initial_element,/*initial_contents,*/ new_dimensions);
    return vo;
  }
  IMPLEMENT_MEF(BF("Implement adjustVector for: %s") % _rep_(array));
};


cl_index NonSimpleVector_O::checkBounds(cl_index start, T_sp end, size_t length) {
  coerce::inBoundsOrError(start,0,length);
  cl_index iend = coerce::coerceToEndInRangeOrError(end,start,length);
  return iend;
}

T_sp NonSimpleVector_O::subseq(cl_index start, T_sp end) const {
  cl_index length = this->length();
  coerce::inBoundsOrError(start,0,length);
  cl_index iend = coerce::coerceToEndInRangeOrError(end,start,length);
  return this->_Vec->subseq(start+this->_DisplacedIndexOffset,iend+this->_DisplacedIndexOffset);
}

T_sp NonSimpleVector_O::setf_subseq(cl_index start, T_sp end, T_sp new_subseq) const {
  size_t iend = checkBounds(start,end,this->length());
  return this->_Vec->setf_subseq(start+this->_DisplacedIndexOffset,iend+this->_DisplacedIndexOffset,new_subseq);
}

T_sp NonSimpleVector_O::vectorPush(T_sp newElement) {
  unlikely_if (!this->_FillPointerp) noFillPointerError();
  cl_index idx = this->_FillPointerOrLength;
  likely_if (idx < this->_TotalArraySize) {
    this->_Vec->rowMajorAset(idx+this->_DisplacedIndexOffset,newElement);
    ++this->_FillPointerOrLength;
    return clasp_make_fixnum(idx);
  }
  return _Nil<T_O>();
}

Fixnum_sp NonSimpleVector_O::vectorPushExtend(T_sp newElement, cl_index extension) {
  unlikely_if (!this->_FillPointerP) noFillPointerError();
  cl_index idx = this->_FillPointerOrLength;
  unlikely_if (idx >= this->_TotalArraySize) {
    if (extension <= 0) extension = 32;
  }
  cl_index new_size = this->_TotalArraySize+extension;
  unlikely_if (!cl::_sym_adjust_array->boundP()) {
    this->setSize(new_size);
  } else {
    eval::funcall(cl::_sym_adjust_array,this->asSmartPtr(),clasp_make_fixnum(new_size),cl::_sym_fill_pointer,this->_FillPointer);
  }
  this->_Vec->rowMajorAset(idx+this->_DisplacedIndexOffset,newElement);
  ++this->_FillPointerOrLength;
  return make_fixnum(idx);
}
























gc::Fixnum Vector_O::arrayDimension(gc::Fixnum axisNumber) const {
  ASSERTF(axisNumber == 0, BF("Illegal axis number %d for Vector") % axisNumber);
  return this->arrayTotalSize();
}

List_sp Vector_O::arrayDimensions() const {
  return Cons_O::create(make_fixnum(this->arrayTotalSize()), _Nil<T_O>());
}

bool Vector_O::equalp(T_sp o) const {
  if ( this->eq(o) ) return true;
  if (Array_sp other = o.asOrNull<Array_O>()) {
    if (other->rank() != 1)
      return false;
    size_t my_size = this->length();
    if (other->arrayDimension(0) != my_size)
      return false;
    for (size_t i(0); i < my_size; ++i) {
      if (!cl__equalp(this->svref(i), other->rowMajorAref(i)))
        return false;
    }
    return true;
  }
  if (Vector_sp vec = o.asOrNull<Vector_O>()) {
    size_t my_size = this->length();
    size_t other_size = vec->length();
    if (my_size != other_size)
      return false;
    for (int i(0); i < my_size; ++i) {
      if (!cl__equalp(this->aref_unsafe(i), vec->aref_unsafe(i)))
        return false;
    }
    return true;
  }
  return false;
}
CL_NAME("FILL-POINTER-SET");
CL_DEFMETHOD void Vector_O::fillPointerSet(T_sp idx)
{
  ERROR(cl::_sym_simpleTypeError,
        core::lisp_createList(kw::_sym_formatControl, core::lisp_createStr("~S is not an array with a fill pointer."),
                              kw::_sym_formatArguments, core::lisp_createList(this->asSmartPtr()),
                              kw::_sym_expectedType, core::lisp_createList(cl::_sym_and,cl::_sym_vector,core::lisp_createList(cl::_sym_satisfies,cl::_sym_array_has_fill_pointer_p)),
                              kw::_sym_datum, this->asSmartPtr()));
}

T_sp Vector_O::aref(VaList_sp args) const {
  cl_index idx = this->index_(args);
  return this->rowMajorAref(idx);
}

T_sp Vector_O::setf_aref(List_sp args_val) {
  T_sp val;
  cl_index idx = this->index_val_(args_val, true, val);
  this->rowMajorAset(idx, val);
  return val;
}

T_sp Vector_O::reverse() {
  _OF();
  int thisLength = this->length();
  int lastElement = thisLength - 1;
  Vector_sp newVec = gc::As<Vector_sp>(eval::funcall(_sym_make_vector, this->elementType(), make_fixnum(thisLength)));
  for (int i = 0; i < thisLength; i++) {
    int ri = lastElement - i;
    //    newVec->rowMajorAset(ri, this->rowMajorAref(i));
    newVec->aset_unsafe(ri, this->rowMajorAref(i));
  }
  return newVec;
}

/*! Return the reversed vector destructively modifying the current vector */
// Swaps at +
// Length = 6   halfLength = 3
//  0 1 2 3 4 5
//  + + +
//  5 4 3 2 1 0
// Length = 5   halfLength = 2
//  0 1 2 3 4
//  + +
//  4 3 2 1 0
T_sp Vector_O::nreverse() {
  _OF();
  int thisLength = this->length();
  int halfLength = thisLength / 2; // 5/2 = 2  0
  int lasti = thisLength - 1;
  for (int i = 0; i < halfLength; i++) {
    int ri = lasti - i;
    this->swapElements(i, ri);
  }
  return this->sharedThis<T_O>();
}


CL_DEFUN T_sp core__make_simple_string8(T_sp val)
{
  if (Str_sp sv = val.asOrNull<Str_O>() ) {
    GC_ALLOCATE_VARIADIC(SimpleString8_O,dv,' ',sv->length());
    memcpy(dv->begin(),sv->begin(),sv->length());
    return dv;
  }
  SIMPLE_ERROR(BF("Only argument must be a string"));
}
  
  

// ------------------------------------------------------------
//
// SimpleString8_O
//

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
CL_DEFUN Fixnum_sp cl__vector_push_extend(T_sp newElement, Vector_sp vec, int extension) {
  return vec->vectorPushExtend(newElement, extension);
}




SYMBOL_SC_(CorePkg, make_vector);
SYMBOL_EXPORT_SC_(CorePkg, adjustVector);
SYMBOL_EXPORT_SC_(ClPkg, vectorPush);
SYMBOL_EXPORT_SC_(ClPkg, vectorPushExtend);



}; /* core */
