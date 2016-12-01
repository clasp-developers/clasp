/*
    File: vectorObjectsWithFillPtr.cc
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
#include <clasp/core/vectorObjectsWithFillPtr.h>
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



VectorObjectsWithFillPtr_sp VectorObjectsWithFillPtr_O::make(T_sp initialElement, T_sp initialContents, int dimension, Fixnum fillPtr, bool adjustable, T_sp elementType) {
  GC_ALLOCATE(VectorObjectsWithFillPtr_O, vo);
  if (fillPtr < 0)
    fillPtr = 0;
  if (fillPtr > dimension)
    fillPtr = dimension;
  vo->_FillPtr = fillPtr;
  vo->setup(initialElement, initialContents, dimension, adjustable, elementType);
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

T_sp VectorObjectsWithFillPtr_O::vectorPush(T_sp newElement) {
  if (this->_FillPtr < this->_Values.size()) {
    int idx = this->_FillPtr;
    this->_Values[idx] = newElement;
    this->_FillPtr++;
    return make_fixnum(idx);
  }
  return _Nil<T_O>();
}

Fixnum_sp VectorObjectsWithFillPtr_O::vectorPushExtend(T_sp newElement, int extension) {
  if (this->_FillPtr >= this->_Values.size()) {
    if (extension <= 0)
      extension = this->_Values.size();
    this->_Values.resize(this->_Values.size() + extension, _Nil<T_O>());
  }
  int idx = this->_FillPtr;
  this->_Values[idx] = newElement;
  this->_FillPtr++;
  return make_fixnum(idx);
}

void VectorObjectsWithFillPtr_O::setFillPointer(size_t fp) {
  if (fp < this->_Values.size()) {
    this->_FillPtr = fp;
    return;
  }
  TYPE_ERROR_INDEX(this->asSmartPtr(),fp);
}

}; /* core */
