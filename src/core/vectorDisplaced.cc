/*
    File: vectorDisplaced.cc
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
#include <clasp/core/vectorDisplaced.h>
#include <clasp/core/serialize.h>
#include <clasp/core/wrappers.h>
namespace core {

// ----------------------------------------------------------------------
//

EXPOSE_CLASS(core, VectorDisplaced_O);

#define ARGS_core_make_vector_displaced "(dim element-type displaced-to displaced-offset)"
#define DECL_core_make_vector_displaced ""
#define DOCS_core_make_vector_displaced ""
VectorDisplaced_sp core_make_vector_displaced(T_sp dim, T_sp elementType, T_sp displacedTo, size_t displacedOffset) {
  GC_ALLOCATE(VectorDisplaced_O, vo);
  vo->_ElementType = elementType;
  vo->_Size = clasp_to_fixnum(dim);
  if ( vo->_Size >= displacedOffset+cl_length(displacedTo)) {
    vo->_Size = cl_length(displacedTo)-displacedOffset;
  }
  vo->_DisplacedIndexOffset = displacedOffset;
  vo->_Vector = gc::As<Vector_sp>(displacedTo);
  return vo;
}

void VectorDisplaced_O::exposeCando(::core::Lisp_sp lisp) {
  ::core::class_<VectorDisplaced_O>();

  CoreDefun(make_vector_displaced);
}

void VectorDisplaced_O::exposePython(::core::Lisp_sp lisp) {
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(Pkg(), VectorDisplaced, "", "", _LISP)
      //	.initArgs("(self)")
      ;
#endif
}

string VectorDisplaced_O::__repr__() const {
  _OF();
  stringstream ss;
  ss << "#( ";
  for (int i = 0; i < this->_Size; i++) {
    ss << _rep_((*this->_Vector)[i]) << " ";
  }
  ss << ")";
  return ss.str();
}

void VectorDisplaced_O::rowMajorAset(cl_index idx, T_sp value) {
  _G();
  ASSERTF(idx < this->length(), BF("Index %d is out of range (<%d)") % idx % this->length());
  (*this)[idx + this->_DisplacedIndexOffset] = value;
}

T_sp VectorDisplaced_O::rowMajorAref(cl_index idx) const {
  ASSERTF(idx < this->length(), BF("Index %d is out of range (<%d)") % idx % this->length());
  return this->operator[](idx);
}

T_sp VectorDisplaced_O::elt(int index) const {
  _G();
  if (index >= this->length()) {
    SIMPLE_ERROR(BF("Index too large %d must be less than %d") % index % this->length());
  }
  return (*this)[index];
}

T_sp VectorDisplaced_O::aref(List_sp indices) const {
  _G();
  ASSERTF(cl_length(indices) == 1, BF("Vectors only support one index - passed: %s") % _rep_(indices));
  return this->elt(clasp_to_int(gc::As<Integer_sp>(oCar(indices))));
}

T_sp VectorDisplaced_O::setf_elt(int index, T_sp obj) {
  _G();
  (*this)[index] = obj;
  return obj;
}

T_sp VectorDisplaced_O::setf_aref(List_sp indices_val) {
  _G();
  ASSERTF(cl_length(indices_val) == 2, BF("Vectors only support one index followed by a value - passed: %s") % _rep_(indices_val));
  return this->setf_elt(clasp_to_int(gc::As<Integer_sp>(oCar(indices_val))), oCadr(indices_val));
}

void VectorDisplaced_O::swap(VectorDisplaced_sp other) {
  this->_Vector.swap(other->_Vector);
  T_sp temp = this->_ElementType;
  this->_ElementType = other->_ElementType;
  other->_ElementType = temp;
  size_t t = this->_Size;
  this->_Size = other->_Size;
  other->_Size = t;
  t = this->_DisplacedIndexOffset;
  this->_DisplacedIndexOffset = other->_DisplacedIndexOffset;
  other->_DisplacedIndexOffset = t;
};

#if 0
T_sp VectorDisplaced_O::subseq(int istart, T_sp end) const {
  _G();
  int iend = (end.nilp()) ? this->length() : unbox_fixnum(gc::As<Fixnum_sp>(end));
  if (istart < 0 || iend > this->length()) {
    SIMPLE_ERROR(BF("out of bounds for subseq"));
  }
  GC_ALLOCATE(VectorDisplaced_O, result);
  int isize = iend - istart;
  result->_Values.resize(isize);
  for (int i = 0; i < isize; ++i) {
    result->_Values[i] = this->_Values[istart];
    ++istart;
  }
  return result;
}
#endif
}; /* core */
