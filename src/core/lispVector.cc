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
#define DEBUG_LEVEL_FULL

#include <clasp/core/common.h>
#include <clasp/core/environment.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/lispVector.h>
#include <clasp/core/bitVector.h>
#include <clasp/core/str.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/strWithFillPtr.h>
#include <clasp/core/vectorObjects.h>
#include <clasp/core/vectorObjectsWithFillPtr.h>
#include <clasp/core/character.h>
#include <clasp/core/wrappers.h>
namespace core {

// ----------------------------------------------------------------------
//

#define ARGS_cl_vector "(&rest args)"
#define DECL_cl_vector ""
#define DOCS_cl_vector "vector"
Vector_sp cl_vector(List_sp args) {
  _G();
  Vector_sp vec = VectorObjects_O::make(_Nil<T_O>(), args, cl_length(args), false, cl::_sym_T_O);
  return vec;
};

#define DOCS_core_make_vector "make_vector See si_make_vector in ecl>>array.d"
#define ARGS_core_make_vector "(element-type dimension &optional adjustable (fill-pointer t) displaced-to displaced-index-offset initial-element initial-contents)"
#define DECL_core_make_vector ""
SYMBOL_EXPORT_SC_(ClPkg, subtypep);
Vector_sp core_make_vector(T_sp element_type,
                           int dimension,
                           bool adjustable,
                           T_sp fill_pointer,
                           T_sp displaced_to,
                           T_sp displaced_index_offset,
                           T_sp initial_element,
                           T_sp initialContents) {
  _G();
  ASSERTF(displaced_to.nilp(), BF("Add support for make-vector :displaced-to"));
  ASSERTF(displaced_index_offset.nilp() || unbox_fixnum(gc::As<Fixnum_sp>(displaced_index_offset)) == 0, BF("Add support for make-vector non-zero :displaced-index-offset "));
  if (element_type == cl::_sym_bit) {
    if (adjustable || fill_pointer.notnilp()) {
      size_t s_fill_ptr = dimension;
      if (fill_pointer.notnilp()) {
        if (fill_pointer != cl::_sym_T_O)
          s_fill_ptr = MIN(dimension, std::abs(unbox_fixnum(gc::As<Fixnum_sp>(fill_pointer))));
      }
      return BitVectorWithFillPtr_O::create(dimension, s_fill_ptr, adjustable);
    }
    return SimpleBitVector_O::create(dimension);
  } else if (element_type == cl::_sym_base_char || element_type == cl::_sym_character || element_type == cl::_sym_standard_char || element_type == cl::_sym_extended_char) {
    // Currently any kind of Character vector is a Str or subclass
    // TODO: Maybe use other types of strings - unicode?
    char c = ' ';
    if (Character_sp cc = initial_element.asOrNull<Character_O>()) {
      c = clasp_as_char(cc);
    }
    if (fill_pointer.notnilp()) {
      int ifp = 0;
      if (fill_pointer == cl::_sym_T_O)
        ifp = dimension;
      else
        ifp = MIN(dimension, std::abs(unbox_fixnum(gc::As<Fixnum_sp>(fill_pointer))));
      return StrWithFillPtr_O::create(c, dimension, ifp, adjustable, initialContents);
    }
    return (Str_O::create(' ', dimension, initialContents));
  } else {
    if (cl_consp(element_type)) {
      // For type = '(unsigned-byte XXX) set initial_element if it hasn't been set
      Cons_sp cet = gc::As<Cons_sp>(element_type);
      if (oCar(cet) == cl::_sym_UnsignedByte && initial_element.nilp()) {
        initial_element = make_fixnum(0);
      }
    }
    if (fill_pointer.notnilp()) {
      int ifp = 0;
      if (fill_pointer == _lisp->_true())
        ifp = dimension;
      else
        ifp = unbox_fixnum(gc::As<Fixnum_sp>(fill_pointer));
      return VectorObjectsWithFillPtr_O::make(initial_element, initialContents, dimension, ifp, adjustable, element_type);
    } else {
      return VectorObjects_O::make(initial_element, initialContents, dimension, adjustable, element_type);
    }
  }
  SIMPLE_ERROR(BF("Handle make-vector :element-type %s") % _rep_(element_type));
};

#define ARGS_core_adjustVector "(array dimensions initial-element initial-contents)"
#define DECL_core_adjustVector ""
#define DOCS_core_adjustVector "adjustVector"
T_sp core_adjustVector(T_sp array, int new_dimensions, T_sp initial_element, List_sp initial_contents) {
  _G();
  if (VectorObjects_sp vo = array.asOrNull<VectorObjects_O>()) {
    vo->adjust(initial_element, initial_contents, new_dimensions);
    return vo;
  }
  IMPLEMENT_MEF(BF("Implement adjustVector for: %s") % _rep_(array));
};

void Vector_O::initialize() {
  _OF();
  this->Base::initialize();
}

void Vector_O::archiveBase(::core::ArchiveP node) {
  // Do nothing
}

gc::Fixnum Vector_O::arrayDimension(gc::Fixnum axisNumber) const {
  ASSERTF(axisNumber == 0, BF("Illegal axis number %d for Vector") % axisNumber);
  return this->dimension();
}

List_sp Vector_O::arrayDimensions() const {
  return Cons_O::create(make_fixnum(this->dimension()), _Nil<T_O>());
}

bool Vector_O::equalp(T_sp o) const {
  if (this->eq(o))
    return true;
  if (Array_sp other = o.asOrNull<Array_O>()) {
    if (other->rank() != 1)
      return false;
    size_t my_size = this->length();
    if (other->arrayDimension(0) != my_size)
      return false;
    for (size_t i(0); i < my_size; ++i) {
      if (!cl_equalp(this->svref(i), other->rowMajorAref(i)))
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
      if (!cl_equalp(this->aref_unsafe(i), vec->aref_unsafe(i)))
        return false;
    }
    return true;
  }
  return false;
}

T_sp Vector_O::aref(List_sp args) const {
  cl_index idx = this->index(args);
  return this->rowMajorAref(idx);
}

T_sp Vector_O::setf_aref(List_sp args_val) {
  List_sp cons_val;
  cl_index idx = this->index_val(args_val, true, cons_val);
  this->rowMajorAset(idx, oCar(cons_val));
  return oCar(cons_val);
}

T_sp Vector_O::reverse() {
  _OF();
  int thisLength = this->length();
  int lastElement = thisLength - 1;
  Vector_sp newVec = gc::As<Vector_sp>(eval::funcall(_sym_make_vector, this->elementType(), make_fixnum(thisLength)));
  for (int i = 0; i < thisLength; i++) {
    int ri = lastElement - i;
    //    newVec->setf_elt(ri, this->elt(i));
    newVec->aset_unsafe(ri, this->elt(i));
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

#define ARGS_cl_vectorPush "(newElement vector)"
#define DECL_cl_vectorPush ""
#define DOCS_cl_vectorPush "vectorPush"
T_sp cl_vectorPush(T_sp newElement, Vector_sp vec) {
  _G();
  return vec->vectorPush(newElement);
};

#define ARGS_cl_vectorPushExtend "(newElement vector &optional (exension 16))"
#define DECL_cl_vectorPushExtend ""
#define DOCS_cl_vectorPushExtend "vectorPushExtend"
Fixnum_sp cl_vectorPushExtend(T_sp newElement, Vector_sp vec, int extension) {
  _G();
  return vec->vectorPushExtend(newElement, extension);
}

EXPOSE_CLASS(core, Vector_O);

void Vector_O::exposeCando(::core::Lisp_sp lisp) {
  _G();
  ::core::class_<Vector_O>()
      .def("adjustableArrayP", &Vector_O::adjustableArrayP)
      //	.initArgs("(self)")
      ;
  SYMBOL_SC_(CorePkg, make_vector);
  CoreDefun(make_vector);
  SYMBOL_EXPORT_SC_(CorePkg, adjustVector);
  CoreDefun(adjustVector);
  SYMBOL_EXPORT_SC_(ClPkg, vectorPush);
  ClDefun(vectorPush);
  SYMBOL_EXPORT_SC_(ClPkg, vectorPushExtend);
  ClDefun(vectorPushExtend);
  ClDefun(vector);
}

void Vector_O::exposePython(::core::Lisp_sp lisp) {
//	PYTHON_CLASS_2BASES(Pkg(),Vector,"","",_LISP)
#ifdef USEBOOSTPYTHON
  boost::python::class_<Vector_O,
                        gctools::smart_ptr<Vector_O>,
                        boost::python::bases<Vector_O::Bases::Base1, Vector_O::Bases::Base2>,
                        boost::noncopyable>("Vector_O", boost::python::no_init)
      //	.initArgs("(self)")
      ;
#endif
}

}; /* core */
