/*
    File: newVectorObjects.cc
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
#include <clasp/core/vectorObjects.h>
#include <clasp/core/serialize.h>
#include <clasp/core/wrappers.h>
namespace core {

// ----------------------------------------------------------------------
//

EXPOSE_CLASS(core, VectorObjects_O);

#define ARGS_VectorObjects_O_make "(initial-element initial-contents dimension adjustable)"
#define DECL_VectorObjects_O_make ""
#define DOCS_VectorObjects_O_make "make VectorObjects args: initial-element initial-contents dimension"
VectorObjects_sp VectorObjects_O::make(T_sp initialElement, T_sp initialContents, int dimension, bool adjustable) {
  _G();
  GC_ALLOCATE(VectorObjects_O, vo);
  vo->setup(initialElement, initialContents, dimension, adjustable);
  return vo;
}

VectorObjects_O::VectorObjects_O() : Base(), _ElementType(cl::_sym_T_O), _Adjustable(true), _Values(NULL){};

void VectorObjects_O::exposeCando(::core::Lisp_sp lisp) {
  ::core::class_<VectorObjects_O>();
}

void VectorObjects_O::exposePython(::core::Lisp_sp lisp) {
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(Pkg(), VectorObjects, "", "", _LISP)
      //	.initArgs("(self)")
      ;
#endif
}

VectorObjects_sp VectorObjects_O::create(T_sp initial_element, int dimension, T_sp elementType) {
  _G();
  GC_ALLOCATE(VectorObjects_O, vo);
  vo->setElementType(elementType);
  vo->_Values = vec::MutableVector<T_sp>::create(dimension, initial_element);
  return vo;
}

void VectorObjects_O::setup(T_sp initialElement, T_sp initialContents, int dimension, bool adjustable) {
  _G();
  this->_Adjustable = adjustable;
  if (initialElement.notnilp() && initialContents.notnilp()) {
    SIMPLE_ERROR(BF("You can only specify one of initial-element or initialContents"));
  }
  this->_Values = vec::MutableVector<T_sp>::create(dimension, initialElement);
  if (initialContents.notnilp()) {
    this->fillInitialContents(initialContents);
  }
}

void VectorObjects_O::fillInitialContents(T_sp ic) {
  if (ic->length() != this->dimension())
    SIMPLE_ERROR(BF("The number of elements %d in :INITIAL-CONTENTS does not match the size of the vector %d") % ic->length() % this->dimension());
  if (Cons_sp cInitialContents = ic.asOrNull<Cons_O>()) {
    size_t i = 0;
    for (Cons_sp cur = cInitialContents; cur.notnilp(); cur = cCdr(cur)) {
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

void VectorObjects_O::adjust(T_sp initialElement, T_sp initialContents, int dimension) {
  _G();
  ANN(this->_Values);
  if (initialElement.notnilp() && initialContents.notnilp()) {
    SIMPLE_ERROR(BF("You can only specify one of initial-element or initialContents"));
  }
  if (initialContents.notnilp()) {
    this->_Values = this->_Values->resize(dimension, _Nil<T_O>());
    this->fillInitialContents(initialContents);
  } else {
    this->_Values = this->_Values->resize(dimension, initialElement);
  }
}

SYMBOL_EXPORT_SC_(KeywordPkg, elementType);
SYMBOL_EXPORT_SC_(KeywordPkg, adjustable);
void VectorObjects_O::archiveBase(::core::ArchiveP node) {
  this->Base::archiveBase(node);
  node->attribute(kw::_sym_elementType, this->_ElementType);
  node->attribute(kw::_sym_adjustable, this->_Adjustable);
  node->vector(this->_Values);
  // Archive other instance variables here
}

void VectorObjects_O::initialize() {
  _OF();
  this->Base::initialize();
}

void VectorObjects_O::fillArrayWithElt(T_sp element, Fixnum_sp start, T_sp end) {
  _OF();
  ANN(this->_Values);
  uint istart = start->get();
  uint last = (*this->_Values).size();
  uint iend = last;
  if (end.notnilp())
    iend = gc::As<Fixnum_sp>(end)->get();
  ASSERTF(iend >= istart, BF("Illegal fill range istart=%d iend=%d") % istart % iend);
  ASSERTF(iend < last, BF("Illegal value for end[%d] - must be between istart[%d] and less than %d") % iend % istart % last);
  ASSERTF(istart >= 0 <= iend, BF("Illegal value for start[%d] - must be between 0 and %d") % istart % iend);
  for (uint i = istart; i < iend; i++) {
    (*this->_Values)[i] = element;
  }
}

string VectorObjects_O::__repr__() const {
  _OF();
  ANN(this->_Values);
  stringstream ss;
  ss << "#( ";
  for (int i = 0; i < (*this->_Values).size(); ++i) {
    ss << _rep_((*this->_Values)[i]) << " ";
  }
  ss << ")";
  return ss.str();
}

void VectorObjects_O::rowMajorAset(int idx, T_sp value) {
  _G();
  ANN(this->_Values);
  ASSERTF(idx < this->length(), BF("Index %d is out of range (<%d)") % idx % this->length());
  (*this->_Values)[idx] = value;
}

T_sp VectorObjects_O::rowMajorAref(int idx) const {
  _G();
  ANN(this->_Values);
  ASSERTF(idx < this->length(), BF("Index %d is out of range (<%d)") % idx % this->length());
  return (*this->_Values)[idx];
}

int VectorObjects_O::arrayRowMajorIndex(List_sp indices) const {
  ASSERTF(cl_length(indices) == 1, BF("Vectors have only one dimension - you passed indices %s") % _rep_(indices));
  return gc::As<Fixnum_sp>(oCar(indices))->get();
}

T_sp VectorObjects_O::elt(int index) const {
  _G();
  ANN(this->_Values);
  if (index >= this->length()) {
    SIMPLE_ERROR(BF("Index too large %d must be less than %d") % index % this->length());
  }
  return (*this->_Values)[index];
}

T_sp VectorObjects_O::aref(List_sp indices) const {
  _G();
  ASSERTF(cl_length(indices) == 1, BF("Vectors only support one index - passed: %s") % _rep_(indices));
  return this->elt(oCar(indices).as<Integer_O>()->as_int());
}

T_sp VectorObjects_O::setf_elt(int index, T_sp obj) {
  _G();
  ANN(this->_Values);
  (*this->_Values)[index] = obj;
  return obj;
}

T_sp VectorObjects_O::setf_aref(List_sp indices_val) {
  _G();
  ASSERTF(cl_length(indices_val) == 2, BF("Vectors only support one index followed by a value - passed: %s") % _rep_(indices_val));
  return this->setf_elt(oCar(indices_val).as<Integer_O>()->as_int(), oCadr(indices_val));
}

void VectorObjects_O::swap(VectorObjects_sp other) {
  auto temp = this->_Values;
  this->_Values = other->_Values;
  other->_Values = temp;
  this->_ElementType.swap(other->_ElementType);
  bool thisAdjustable = this->_Adjustable;
  this->_Adjustable = other->_Adjustable;
  other->_Adjustable = thisAdjustable;
};

T_sp VectorObjects_O::subseq(int istart, T_sp end) const {
  _G();
  int iend = (end.nilp()) ? this->length() : gc::As<Fixnum_sp>(end)->get();
  if (istart < 0 || iend > this->length()) {
    SIMPLE_ERROR(BF("out of bounds for subseq"));
  }
  GC_ALLOCATE(VectorObjects_O, result);
  int isize = iend - istart;
  result->_Values = result->_Values->resize(isize);
  for (int i = 0; i < isize; ++i) {
    (*result->_Values)[i] = (*this->_Values)[istart];
    ++istart;
  }
  return result;
}

}; /* core */
