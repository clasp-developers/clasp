/*
    File: arrayObjects.cc
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
#include <clasp/core/arrayObjects.h>
#include <clasp/core/wrappers.h>
namespace core {

// ----------------------------------------------------------------------
//

EXPOSE_CLASS(core, ArrayObjects_O);

#define ARGS_ArrayObjects_O_make "(dimensions element-type initial-element adjustable)"
#define DECL_ArrayObjects_O_make ""
#define DOCS_ArrayObjects_O_make "make ArrayObjects args: dimensions element-type initial-element"
ArrayObjects_sp ArrayObjects_O::make(T_sp dim_desig, T_sp elementType, T_sp initialElement, T_sp adjustable) {
  _G();
  GC_ALLOCATE(ArrayObjects_O, array);
  array->_ElementType = elementType;
  List_sp dim;
  if (dim_desig.nilp()) {
    dim = dim_desig;
  } else if (cl_atom(dim_desig)) {
    int idim = clasp_to_int(gc::As<Integer_sp>(dim_desig));
    dim = Cons_O::create(make_fixnum(idim));
  } else {
    dim = dim_desig;
  }
  /* LongLongInt elements = */ array->setDimensions(dim, initialElement);
  return ((array));
}

void ArrayObjects_O::exposeCando(::core::Lisp_sp lisp) {
  ::core::class_<ArrayObjects_O>();
  Defun_maker(CorePkg, ArrayObjects);
}

void ArrayObjects_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, ArrayObjects, "", "", _lisp);
#endif
}

#if defined(XML_ARCHIVE)
void ArrayObjects_O::archiveBase(::core::ArchiveP node) {
  this->Base::archiveBase(node);
  IMPLEMENT_ME();
#if 0
	node->attribute("elementType",this->_ElementType);
	node->archiveVector0<T_O>("contents",this->_Values);
#endif
  // Archive other instance variables here
}
#endif // defined(XML_ARCHIVE)
#if defined(OLD_SERIALIZE)
void ArrayObjects_O::serialize(serialize::SNode node) {
  this->Base::serialize(node);
  node->namedObject("elementType", this->_ElementType);
  node->namedVector0<T_O>("contents", this->_Values);
}
#endif

void ArrayObjects_O::initialize() {
  _OF();
  this->Base::initialize();
  this->_ElementType = cl::_sym_T_O;
}

void ArrayObjects_O::rowMajorAset(cl_index idx, T_sp value) {
  _G();
  ASSERTF(idx < this->_Values.size(), BF("Illegal row-major-aref index %d - must be less than %d") % idx % this->_Values.size());
  this->_Values[idx] = value;
}

T_sp ArrayObjects_O::aset_unsafe(int idx, T_sp value) {
  _G();
  this->_Values[idx] = value;
  return value;
}

bool ArrayObjects_O::equalp(T_sp o) const {
  if (this->eq(o))
    return true;
  if (ArrayObjects_sp other = o.asOrNull<ArrayObjects_O>()) {
    const std::vector<cl_index> &my_dimensions = this->_Dimensions;
    const std::vector<cl_index> &other_dimensions = other->_Dimensions;
    if (my_dimensions.size() != other_dimensions.size())
      return false;
    size_t size = 1;
    for (int i(0); i < my_dimensions.size(); ++i) {
      size *= my_dimensions[i];
      if (my_dimensions[i] != other_dimensions[i])
        return false;
    }
    for (size_t i(0); i < size; ++i) {
      if (!cl_equalp(this->rowMajorAref(i), other->rowMajorAref(i)))
        return false;
    }
    return true;
  }
  if (Vector_sp vec = o.asOrNull<Vector_O>()) {
    if (this->_Dimensions.size() != 1)
      return false;
    if (this->_Dimensions[0] != cl_length(vec))
      return false;
    size_t size = this->_Dimensions[0];
    for (int i(0); i < size; ++i) {
      if (!cl_equalp(this->rowMajorAref(i), vec->aref_unsafe(i)))
        return false;
    }
    return true;
  }
  return false;
}

T_sp ArrayObjects_O::rowMajorAref(cl_index idx) const {
  _G();
  ASSERTF(idx < this->_Values.size(), BF("Illegal row-major-aref index %d - must be less than %d") % idx % this->_Values.size());
  return ((this->_Values[idx]));
};

T_sp ArrayObjects_O::aref(List_sp indices) const {
  LOG(BF("indices[%s]") % _rep_(indices));
  int index = this->index(indices);
  return ((this->_Values[index]));
}

T_sp ArrayObjects_O::setf_aref(List_sp indices_val) {
  _OF();
  List_sp val_cons;
  LongLongInt index = this->index_val(indices_val, true, val_cons);
  this->_Values[index] = oCar(val_cons);
  return ((oCar(val_cons)));
}

T_sp ArrayObjects_O::shallowCopy() const {
  GC_ALLOCATE(ArrayObjects_O, array);
  array->_Dimensions = this->_Dimensions;
  array->_ElementType = this->_ElementType;
  array->_Values = this->_Values;
  return ((array));
}

void ArrayObjects_O::arrayFill(T_sp val) {
  _OF();
  for (int i = 0; i < (int)this->_Values.size(); i++) {
    this->_Values[i] = val;
  }
}

T_sp ArrayObjects_O::deepCopy() const {
  _OF();
  GC_ALLOCATE(ArrayObjects_O, narray);
  narray->_Dimensions = this->_Dimensions;
  narray->_ElementType = this->_ElementType; // Don't copy ElementType - it's an immutable Symbol representing a Class
  narray->_Values.resize(this->_Values.size());
  for (uint i = 0; i < this->_Values.size(); i++) {
    narray->_Values[i] = this->_Values[i]->deepCopy();
  }
  return ((narray));
}

T_sp ArrayObjects_O::svref(int index) const {
  _G();
  if (this->_Dimensions.size() == 1) {
    ASSERT(index >= 0 && index < this->_Dimensions[0]);
    return ((this->_Values[index]));
  }
  SIMPLE_ERROR(BF("ArrayObjects has more than one dimension - cannot use svref"));
}

T_sp ArrayObjects_O::setf_svref(int index, T_sp value) {
  _G();
  if (this->_Dimensions.size() == 1) {
    ASSERT(index >= 0 && index < this->_Dimensions[0]);
    this->_Values[index] = value;
    return ((value));
  }
  SIMPLE_ERROR(BF("ArrayObjects has more than one dimension - cannot use setf-svref"));
}

LongLongInt ArrayObjects_O::setDimensions(List_sp dim, T_sp initialElement) {
  _OF();
  LongLongInt elements = 1;
  int newRank = cl_length(dim);
  if (newRank >= CLASP_ARRAY_RANK_LIMIT) {
    SIMPLE_ERROR(BF("Maximum rank is %d") % CLASP_ARRAY_RANK_LIMIT);
  }
  this->_Dimensions.resize(newRank);
  int idx = 0;
  for (; dim.notnilp(); dim = oCdr(dim)) {
    int oneDim = clasp_to_int(gc::As<Rational_sp>(oCar(dim)));
    this->_Dimensions[idx] = oneDim;
    elements *= oneDim;
    idx++;
  }
  this->_Values.resize(elements, initialElement);
  return ((elements));
}

gc::Fixnum ArrayObjects_O::arrayDimension(gc::Fixnum axisNumber) const {
  ASSERTF(axisNumber >= 0, BF("Axis number must be >= 0"));
  ASSERTF(axisNumber < this->_Dimensions.size(), BF("There is no axis with number %d - must be less than %d") % axisNumber % this->_Dimensions.size());
  return ((this->_Dimensions[axisNumber]));
}

#if defined(XML_ARCHIVE)
void ArrayObjects_O::archiveBase(::core::ArchiveP node) {
  this->Base::archiveBase(node);
  node->archiveVectorInt("dims", this->_Dimensions);
  // Archive other instance variables here
}
#endif // defined(XML_ARCHIVE)
#if defined(OLD_SERIALIZE)
void ArrayObjects_O::serialize(serialize::SNode node) {
  node->namedPOD("dims", this->_Dimensions);
}
#endif

}; /* core */
