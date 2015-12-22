/*
    File: arrayDisplaced.cc
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
#include <clasp/core/arrayDisplaced.h>
#include <clasp/core/wrappers.h>
namespace core {

// ----------------------------------------------------------------------
//

EXPOSE_CLASS(core, ArrayDisplaced_O);

T_mv ArrayDisplaced_O::arrayDisplacement() const {
  return Values(this->_Array, make_fixnum(this->_DisplacedIndexOffset));
};

#define ARGS_ArrayDisplaced_O_make "(dimensions element-type displaced-to displaced-index-offset)"
#define DECL_ArrayDisplaced_O_make ""
#define DOCS_ArrayDisplaced_O_make "make ArrayDisplaced args: dimensions element-type displaced-to displaced-index-offset"
ArrayDisplaced_sp ArrayDisplaced_O::make(T_sp dim_desig, T_sp elementType, T_sp displacedTo, int displacedIndexOffset) {
  GC_ALLOCATE(ArrayDisplaced_O, array);
  array->_ElementType = elementType;
  array->_Array = displacedTo;
  array->_DisplacedIndexOffset = displacedIndexOffset;
  List_sp dim;
  if (cl__atom(dim_desig)) {
    int idim = clasp_to_int(gc::As<Integer_sp>(dim_desig));
    dim = Cons_O::create(make_fixnum(idim));
  } else {
    dim = dim_desig;
  }
  /* LongLongInt elements = */ array->setDimensions(dim, displacedTo);
  return array;
}

void ArrayDisplaced_O::exposeCando(::core::Lisp_sp lisp) {
  ::core::class_<ArrayDisplaced_O>();
  Defun_maker(CorePkg, ArrayDisplaced);
}

void ArrayDisplaced_O::exposePython(Lisp_sp lisp) {
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, ArrayDisplaced, "", "", _lisp);
#endif
}

#if defined(XML_ARCHIVE)
void ArrayDisplaced_O::archiveBase(::core::ArchiveP node) {
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
void ArrayDisplaced_O::serialize(serialize::SNode node) {
  this->Base::serialize(node);
  node->namedObject("elementType", this->_ElementType);
  node->namedVector0<T_O>("contents", this->_Values);
}
#endif

void ArrayDisplaced_O::initialize() {
  _OF();
  this->Base::initialize();
  this->_ElementType = cl::_sym_T_O;
}

void ArrayDisplaced_O::rowMajorAset(cl_index idx, T_sp value) {
  this->_Array->rowMajorAset(this->_DisplacedIndexOffset + idx, value);
}

T_sp ArrayDisplaced_O::aset_unsafe(int idx, T_sp value) {
  this->_Array->aset_unsafe(this->_DisplacedIndexOffset + idx, value);
  return value;
}

T_sp ArrayDisplaced_O::rowMajorAref(cl_index idx) const {
  return ((this->_Array->rowMajorAref(idx + this->_DisplacedIndexOffset)));
};

T_sp ArrayDisplaced_O::aref(List_sp indices) const {
  _OF();
  LOG(BF("indices[%s]") % _rep_(indices));
  int index = this->index(indices);
  return ((this->rowMajorAref(index)));
}

T_sp ArrayDisplaced_O::setf_aref(List_sp indices_val) {
  _OF();
  List_sp val_cons;
  LongLongInt index = this->index_val(indices_val, true, val_cons);
  this->aset_unsafe(index, oCar(val_cons));
  return ((oCar(val_cons)));
}

T_sp ArrayDisplaced_O::shallowCopy() const {
  _OF();
  GC_ALLOCATE(ArrayDisplaced_O, array);
  array->_Dimensions = this->_Dimensions;
  array->_ElementType = this->_ElementType;
  array->_Array = this->_Array;
  return ((array));
}

void ArrayDisplaced_O::arrayFill(T_sp val) {
  for (int i = 0, iEnd(this->arrayTotalSize()); i < iEnd; i++) {
    this->aset_unsafe(i, val);
  }
}

T_sp ArrayDisplaced_O::deepCopy() const {
  _OF();
  SIMPLE_ERROR(BF("deepCopy not supported for ArrayDisplaced_O"));
}

T_sp ArrayDisplaced_O::svref(int index) const {
  if (this->_Dimensions.size() == 1) {
    ASSERT(index >= 0 && index < this->_Dimensions[0]);
    return ((this->rowMajorAref(index)));
  }
  SIMPLE_ERROR(BF("ArrayDisplaced has more than one dimension - cannot use svref"));
}

T_sp ArrayDisplaced_O::setf_svref(int index, T_sp value) {
  if (this->_Dimensions.size() == 1) {
    ASSERT(index >= 0 && index < this->_Dimensions[0]);
    this->rowMajorAset(index, value);
    return ((value));
  }
  SIMPLE_ERROR(BF("ArrayDisplaced has more than one dimension - cannot use setf-svref"));
}

LongLongInt ArrayDisplaced_O::setDimensions(List_sp ldim, T_sp displacedTo) {
  _OF();
  LongLongInt elements = 1;
  this->_Dimensions.resize(cl__length(ldim));
  int idx = 0;
  for (auto dim : ldim) {
    int oneDim = clasp_to_int(gc::As<Rational_sp>(oCar(dim)));
    this->_Dimensions[idx] = oneDim;
    elements *= oneDim;
    idx++;
  }
  if (this->arrayTotalSize() < elements) {
    SIMPLE_ERROR(BF("The displaced to array is not large enough for this displaced array"));
  }
  return ((elements));
}

gc::Fixnum ArrayDisplaced_O::arrayDimension(gc::Fixnum axisNumber) const {
  ASSERTF(axisNumber >= 0, BF("Axis number must be >= 0"));
  ASSERTF(axisNumber < this->_Dimensions.size(), BF("There is no axis with number %d - must be less than %d") % axisNumber % this->_Dimensions.size());
  return ((this->_Dimensions[axisNumber]));
}

#if defined(XML_ARCHIVE)
void ArrayDisplaced_O::archiveBase(::core::ArchiveP node) {
  this->Base::archiveBase(node);
  node->archiveVectorInt("dims", this->_Dimensions);
  // Archive other instance variables here
}
#endif // defined(XML_ARCHIVE)
#if defined(OLD_SERIALIZE)
void ArrayDisplaced_O::serialize(serialize::SNode node) {
  node->namedPOD("dims", this->_Dimensions);
}
#endif

}; /* core */
