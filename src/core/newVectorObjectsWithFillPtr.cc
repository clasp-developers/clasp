/*
    File: newVectorObjectsWithFillPtr.cc
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
#define DEBUG_LEVEL_FULL

#include <clasp/core/common.h>
#include <clasp/core/environment.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/vectorObjectsWithFillPtr.h>
#include <clasp/core/serialize.h>
#include <clasp/core/wrappers.h>
namespace core {

// ----------------------------------------------------------------------
//

EXPOSE_CLASS(core, VectorObjectsWithFillPtr_O);

#define ARGS_VectorObjectsWithFillPtr_O_make "(initial-element initial-contents dimension fillptr adjustable)"
#define DECL_VectorObjectsWithFillPtr_O_make ""
#define DOCS_VectorObjectsWithFillPtr_O_make "make VectorObjectsWithFillPtr args: initial-element initial-contents dimension"
VectorObjectsWithFillPtr_sp VectorObjectsWithFillPtr_O::make(T_sp initialElement, T_sp initialContents, int dimension, int fillPtr, bool adjustable) {
  _G();
  GC_ALLOCATE(VectorObjectsWithFillPtr_O, vo);
  if (fillPtr < 0)
    fillPtr = 0;
  if (fillPtr > dimension)
    fillPtr = dimension;
  vo->_FillPtr = fillPtr;
  vo->setup(initialElement, initialContents, dimension, adjustable);
  return vo;
}

VectorObjectsWithFillPtr_O::VectorObjectsWithFillPtr_O() : Base(){};

void VectorObjectsWithFillPtr_O::exposeCando(::core::Lisp_sp lisp) {
  ::core::class_<VectorObjectsWithFillPtr_O>()
      .def("setf_fillPointer", &VectorObjectsWithFillPtr_O::setf_fillPointer);
}

void VectorObjectsWithFillPtr_O::exposePython(::core::Lisp_sp lisp) {
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(Pkg(), VectorObjectsWithFillPtr, "", "", _LISP)
      //	.initArgs("(self)")
      ;
#endif
}

void VectorObjectsWithFillPtr_O::archiveBase(::core::ArchiveP node) {
  this->Base::archiveBase(node);
  SYMBOL_EXPORT_SC_(KeywordPkg, fillPointer);
  node->attribute(kw::_sym_fillPointer, this->_FillPtr);
}

void VectorObjectsWithFillPtr_O::initialize() {
  _OF();
  this->Base::initialize();
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

T_sp &VectorObjectsWithFillPtr_O::operator[](uint index) {
  _G();
  return (*this->_Values)[index];
}

T_sp VectorObjectsWithFillPtr_O::elt(int index) const {
  _G();
  if (index >= this->_FillPtr) {
    //	    ERROR(make_condition(_sym_indexTooLargeError) << kw::_sym_datum << index << kw::_sym_expectedType << this->_FillPtr );
    SIMPLE_ERROR(BF("Index %d is too large - must be less than %d") % index % this->_FillPtr);
  }
  return this->Base::elt(index);
}

T_sp VectorObjectsWithFillPtr_O::setf_elt(int index, T_sp value) {
  _G();
  if (index >= this->_FillPtr) {
    //	    ERROR(make_condition(_sym_indexTooLargeError) << kw::_sym_datum << index << kw::_sym_expectedType << this->_FillPtr );
    SIMPLE_ERROR(BF("Index %d is too large - must be less than %d") % index % this->_FillPtr);
  }
  return this->Base::setf_elt(index, value);
}

Fixnum_sp VectorObjectsWithFillPtr_O::vectorPush(T_sp newElement) {
  _G();
  ANN(this->_Values);
  if (this->_FillPtr < (*this->_Values).size()) {
    int idx = this->_FillPtr;
    (*this->_Values)[idx] = newElement;
    this->_FillPtr++;
    return Fixnum_O::create(idx);
  }
  return _Nil<Fixnum_O>();
}

Fixnum_sp VectorObjectsWithFillPtr_O::vectorPushExtend(T_sp newElement, int extension) {
  _G();
  ANN(this->_Values);
  if (this->_FillPtr >= (*this->_Values).size()) {
    if (extension <= 0)
      extension = (*this->_Values).size();
    this->_Values = (*this->_Values).resize((*this->_Values).size() + extension, _Nil<T_O>());
  }
  int idx = this->_FillPtr;
  (*this->_Values)[idx] = newElement;
  this->_FillPtr++;
  return Fixnum_O::create(idx);
}

void VectorObjectsWithFillPtr_O::setf_fillPointer(int fp) {
  _G();
  ANN(this->_Values);
  if (fp >= (*this->_Values).size())
    fp = (*this->_Values).size();
  this->_FillPtr = fp;
}

}; /* core */
