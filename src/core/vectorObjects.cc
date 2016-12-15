/*
    File: vectorObjects.cc
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
#include <clasp/core/vectorObjects.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/serialize.h>
#include <clasp/core/wrappers.h>
namespace core {

// ----------------------------------------------------------------------
//



VectorObjects_sp VectorObjects_O::make(T_sp initialElement, size_t dimension, T_sp elementType, T_sp fillPointer, T_sp displacedTo, Fixnum displacedIndexOffset) {
  GC_ALLOCATE(VectorObjects_O, vo);
  vo->setup(initialElement, dimension, elementType, fillPointer, displacedTo, displacedIndexOffset );
  return vo;
}

VectorObjects_sp VectorObjects_O::create(T_sp initialElement, size_t dimension, T_sp elementType) {
  return VectorObjects_O::make(initialElement,dimension,elementType);
}

VectorObjects_sp VectorObjects_O::create(const gctools::Vec0<T_sp> &data) {
  VectorObjects_sp val = VectorObjects_O::make(_Nil<core::T_O>(),data.size(),cl::_sym_T_O,_Nil<core::T_O>());
  for (int i = 0, iEnd(data.size()); i < iEnd; ++i) {
    val->operator[](i) = data[i];
  }
  return val;
}

void VectorObjects_O::setup(T_sp initialElement, size_t dimension, T_sp elementType, T_sp fillPointer, T_sp displacedTo, Fixnum displacedIndexOffset ) {
  this->_Dimension = dimension;
  this->_FillPointer = fillPointer;
  this->_ElementType = elementType;
  if (displacedTo.nilp() ) {
    vector_type values;
    this->_Values = values;
    this->_Values.resize(dimension, initialElement);
    this->_DisplacedIndexOffset = 0;
    this->_DisplacedTo = _Nil<core::T_O>();
  } else {
    vector_type values = gc::As<VectorObjects_sp>(displacedTo)->_Values;
    this->_DisplacedTo = displacedTo;
    this->_DisplacedIndexOffset = displacedIndexOffset;
  }
}

void VectorObjects_O::fillInitialContents(T_sp ic) {
  if (cl__length(ic) != this->dimension())
    SIMPLE_ERROR(BF("The number of elements %d in :INITIAL-CONTENTS does not match the size of the vector %d") % cl__length(ic) % this->dimension());
  if (ic.nilp()) {
    // do nothing
  } else if (Cons_sp ccInitialContents = ic.asOrNull<Cons_O>()) {
    List_sp cInitialContents = ccInitialContents;
    size_t i = 0;
    for (auto cur : cInitialContents) {
      T_sp obj = oCar(cur);
      this->setf_elt(i, obj);
      ++i;
    }
  } else if (Vector_sp vic = ic.asOrNull<Vector_O>()) {
    for (size_t i = 0; i < vic->length(); ++i) {
      T_sp obj = vic->elt(i);
      this->setf_elt(i, obj);
    }
  } else {
    SIMPLE_ERROR(BF("Illegal :INITIAL-CONTENTS"));
  }
}

void VectorObjects_O::adjust(T_sp initialElement, size_t dimension) {
  this->_Values.resize(dimension, initialElement);
  this->_Dimension = dimension;
}

SYMBOL_EXPORT_SC_(KeywordPkg, elementType);
SYMBOL_EXPORT_SC_(KeywordPkg, adjustable);

void VectorObjects_O::fillArrayWithElt(T_sp element, Fixnum_sp start, T_sp end) {
  _OF();
  uint istart = unbox_fixnum(start);
  uint last = this->_Dimension;
  uint iend = last;
  if (end.notnilp())
    iend = unbox_fixnum(gc::As<Fixnum_sp>(end));
  ASSERTF(iend >= istart, BF("Illegal fill range istart=%d iend=%d") % istart % iend);
  ASSERTF(iend <= last, BF("Illegal value for end[%d] - must be between istart[%d] and less than %d") % iend % istart % last);
  ASSERTF(istart >= 0 <= iend, BF("Illegal value for start[%d] - must be between 0 and %d") % istart % iend);
  for (uint i = istart; i < iend; i++) {
    this->_Values[this->_DisplacedIndexOffset+i] = element;
  }
}

string VectorObjects_O::__repr__() const {
  _OF();
  stringstream ss;
  ss << "#( ";
  for (int i = 0; i < this->_Dimension; i++) {
    ss << _rep_((*this)[i]) << " ";
  }
  ss << ")";
  return ss.str();
}

cl_index VectorObjects_O::arrayRowMajorIndex(List_sp indices) const {
  ASSERTF(cl__length(indices) == 1, BF("Vectors have only one dimension - you passed indices %s") % _rep_(indices));
  return unbox_fixnum(gc::As<Fixnum_sp>(oCar(indices)));
}

T_sp VectorObjects_O::aref(VaList_sp indices) const {
  core::T_sp arg0 = indices->next_arg();
  cl_index index = arg0.unsafe_fixnum();
  return this->elt(index);
}

T_sp VectorObjects_O::setf_aref(List_sp indices_val) {
  ASSERTF(cl__length(indices_val) == 2, BF("Vectors only support one index followed by a value - passed: %s") % _rep_(indices_val));
  return this->setf_elt(clasp_to_int(gc::As<Integer_sp>(oCar(indices_val))), oCadr(indices_val));
}

void VectorObjects_O::swap(VectorObjects_sp other) {
  VectorObjects_O temp = *this;
  *this = *other;
  *other = temp;
};


T_sp VectorObjects_O::subseq(cl_index istart, T_sp end) const {
  cl_index iend = (end.nilp()) ? this->length() : unbox_fixnum(gc::As<Fixnum_sp>(end));
  if ( iend > this->length()) {
    SIMPLE_ERROR(BF("out of bounds for subseq"));
  }
  int isize = iend - istart;
  VectorObjects_sp result = VectorObjects_O::make(_Nil<core::T_O>(),isize,this->_ElementType,_Nil<core::T_O>());
  for (int i = 0; i < isize; ++i) {
      result->setf_elt(i,this->_Values[this->_DisplacedIndexOffset+istart]);
      ++istart;
  }
  return result;
}


T_sp VectorObjects_O::vectorPush(T_sp newElement) {
  if (!this->_FillPointer.fixnump()) {
    SIMPLE_ERROR(BF("This vector does not have a fill pointer"));
  }
  cl_index idx = this->_FillPointer.unsafe_fixnum();
  if (idx < this->_Dimension) {
    this->_Values[this->_DisplacedIndexOffset+idx] = newElement;
    this->_FillPointer = clasp_make_fixnum(idx+1);
    return clasp_make_fixnum(idx);
  }
  return _Nil<T_O>();
}

Fixnum_sp VectorObjects_O::vectorPushExtend(T_sp newElement, cl_index extension) {
  if (!this->_FillPointer.fixnump()) {
    SIMPLE_ERROR(BF("This vector does not have a fill pointer"));
  }
  cl_index idx = this->_FillPointer.unsafe_fixnum();
  if (idx >= this->_Dimension) {
    if (extension <= 0) extension = 256;
  }
  cl_index new_size = this->_Dimension+extension;
  unlikely_if (!cl::_sym_adjust_array->boundP()) {
    this->adjust(_Nil<core::T_O>(),new_size);
  } else {
    eval::funcall(cl::_sym_adjust_array,this->asSmartPtr(),clasp_make_fixnum(new_size),cl::_sym_fill_pointer, this->_FillPointer);
  }
  this->_Values[idx] = newElement;
  this->_FillPointer = clasp_make_fixnum(idx+1);
  return make_fixnum(idx);
}


}; /* core */


namespace core {
#if 0
#include <clasp/core/common.h>
#include <clasp/core/environment.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/serialize.h>
#include <clasp/core/wrappers.h>
namespace core {

// ----------------------------------------------------------------------
//

CL_LAMBDA(vec);
CL_DECLARE();
CL_DOCSTRING("Return the fill-pointer");
CL_DEFUN int cl__fill_pointer(Vector_sp vec) {
  return vec->fillPointer();
}



VectorObjects_sp VectorObjectsWithFillPtr_O::make(T_sp initialElement, int dimension, Fixnum fillPtr, T_sp elementType) {
  GC_ALLOCATE(VectorObjectsWithFillPtr_O, vo);
  if (fillPtr < 0)
    fillPtr = 0;
  if (fillPtr > dimension)
    fillPtr = dimension;
  vo->_FillPtr = fillPtr;
  vo->setup(initialElement, dimension, elementType, 0);
  return vo;
}

VectorObjectsWithFillPtr_O::VectorObjectsWithFillPtr_O() : Base(){};




void VectorObjectsWithFillPtr_O::archiveBase(::core::ArchiveP node) {
  this->Base::archiveBase(node);
  SYMBOL_EXPORT_SC_(KeywordPkg, fillPointer);
  node->attribute(kw::_sym_fillPointer, this->_FillPtr);
}

string VectorObjectsWithFillPtr_O::__repr__() const {
  _OF();
  stringstream ss;
  ss << "#" << this->_FillPtr << "( ";
  for (int i = 0; i < this->_FillPtr; i++) {
    ss << _rep_(this->elt(i)) << " ";
  }
  ss << ")";
  return ss.str();
}


T_sp VectorObjectsWithFillPtr_O::elt(int index) const {
  if (index >= this->_FillPtr) {
    //	    ERROR(make_condition(_sym_indexTooLargeError) << kw::_sym_datum << index << kw::_sym_expectedType << this->_FillPtr );
    SIMPLE_ERROR(BF("Index %d is too large - must be less than %d") % index % this->_FillPtr);
  }
  return this->Base::elt(index);
}

T_sp VectorObjectsWithFillPtr_O::setf_elt(int index, T_sp value) {
  if (index >= this->_FillPtr) {
    //	    ERROR(make_condition(_sym_indexTooLargeError) << kw::_sym_datum << index << kw::_sym_expectedType << this->_FillPtr );
    SIMPLE_ERROR(BF("Index %d is too large - must be less than %d") % index % this->_FillPtr);
  }
  return this->Base::setf_elt(index, value);
}

void VectorObjectsWithFillPtr_O::setFillPointer(cl_index fp) {
  if (fp < this->_Dimension) {
    this->_FillPtr = fp;
    return;
  }
  TYPE_ERROR_INDEX(this->asSmartPtr(),fp);
}
#endif

};
