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
//#define DEBUG_LEVEL_FULL

#include <clasp/core/common.h>
#include <clasp/core/environment.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/arrayObjects.h>
#include <clasp/core/wrappers.h>
namespace core {

// ----------------------------------------------------------------------
//



CL_LISPIFY_NAME(make-array-objects);
CL_DEFUN ArrayObjects_sp ArrayObjects_O::make(T_sp dim_desig, T_sp elementType, T_sp initialElement, T_sp initialElementSuppliedP, T_sp displacedTo, Fixnum displacedIndexOffset ) {
  GC_ALLOCATE(ArrayObjects_O, array);
  array->_ElementType = elementType;
  List_sp dim;
  if (dim_desig.nilp()) {
    dim = dim_desig;
  } else if (cl__atom(dim_desig)) {
    int idim = clasp_to_int(gc::As<Integer_sp>(dim_desig));
    dim = Cons_O::create(make_fixnum(idim));
  } else {
    dim = dim_desig;
  }
  /* LongLongInt elements = */ array->setDimensions(dim, initialElement, displacedTo );
  if (displacedTo.notnilp()) {
    array->_DisplacedIndexOffset = displacedIndexOffset;
  } else {
    array->_DisplacedIndexOffset = 0;
  }
  return array;
}

void ArrayObjects_O::rowMajorAset(cl_index idx, T_sp value) {
  this->_Values[this->_DisplacedIndexOffset+idx] = value;
}

T_sp ArrayObjects_O::aset_unsafe(size_t idx, T_sp value) {
  this->_Values[this->_DisplacedIndexOffset+idx] = value;
  return value;
}

bool ArrayObjects_O::equalp(T_sp o) const {
  if (this->eq(o)) return true;
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
      if (!cl__equalp(this->rowMajorAref(i), other->rowMajorAref(i)))
        return false;
    }
    return true;
  }
  if (Vector_sp vec = o.asOrNull<Vector_O>()) {
    if (this->_Dimensions.size() != 1)
      return false;
    if (this->_Dimensions[0] != cl__length(vec))
      return false;
    size_t size = this->_Dimensions[0];
    for (int i(0); i < size; ++i) {
      if (!cl__equalp(this->rowMajorAref(i), vec->aref_unsafe(i)))
        return false;
    }
    return true;
  }
  return false;
}

T_sp ArrayObjects_O::rowMajorAref(cl_index idx) const {
  return ((this->_Values[this->_DisplacedIndexOffset+idx]));
};

T_sp ArrayObjects_O::aref(VaList_sp indices) const {
  LOG(BF("indices[%s]") % _rep_(indices));
  Fixnum index = this->index_(indices);
  return ((this->_Values[this->_DisplacedIndexOffset+index]));
}

T_sp ArrayObjects_O::setf_aref(List_sp indices_val) {
  T_sp val;
  LongLongInt index = this->index_val_(indices_val, true, val);
  this->_Values[this->_DisplacedIndexOffset+index] = val;
  return val;
}

T_sp ArrayObjects_O::shallowCopy() const {
  GC_ALLOCATE(ArrayObjects_O, array);
  *array = *this;
  return array;
}

void ArrayObjects_O::arrayFill(T_sp val) {
  for (int i = 0; i < (int)this->_Values.size(); i++) {
    this->_Values[this->_DisplacedIndexOffset+i] = val;
  }
}

T_sp ArrayObjects_O::svref(int index) const {
  if (this->rank() == 1) {
    return ((this->_Values[this->_DisplacedIndexOffset+index]));
  }
  SIMPLE_ERROR(BF("ArrayObjects has more than one dimension - cannot use svref"));
}

T_sp ArrayObjects_O::setf_svref(int index, T_sp value) {
  if (this->rank() == 1) {
    this->_Values[this->_DisplacedIndexOffset+index] = value;
    return value;
  }
  SIMPLE_ERROR(BF("ArrayObjects has more than one dimension - cannot use setf-svref"));
}

LongLongInt ArrayObjects_O::setDimensions(List_sp ldim, T_sp initialElement, T_sp displacedTo) {
  int newRank = cl__length(ldim);
  if (newRank > CLASP_ARRAY_RANK_LIMIT) {
    SIMPLE_ERROR(BF("Maximum rank is %d") % CLASP_ARRAY_RANK_LIMIT);
  }
  this->_Dimensions.resize(newRank);
  Fixnum elements = 1;
  Fixnum idx = 0;
  for ( auto dim : ldim ) {
    Fixnum oneDim = clasp_to_int(gc::As<Rational_sp>(oCar(dim)));
    this->_Dimensions[idx] = oneDim;
    elements *= oneDim;
    idx++;
  }
  this->_Dimension = elements;
  if (displacedTo.nilp()) {
    gc::Vec0<T_sp> values;
    this->_Values = values;
    this->_DisplacedTo = _Nil<core::T_O>();
    this->_Values.resize(elements, initialElement);
  } else {
    this->_DisplacedTo = displacedTo;
    this->_Values = gc::As<ArrayObjects_sp>(displacedTo)->_Values;
  }
  return elements;
}

T_sp ArrayObjects_O::replace_array(T_sp other) {
  ArrayObjects_sp aother = gctools::As<ArrayObjects_sp>(other);
  if (this->rank()!=aother->rank()) {
    SIMPLE_ERROR(BF("Cannot replace-array with mismatched rank this -> %d other -> %d") % this->rank() % aother->rank());
  }
  *this = *aother;
  return this->asSmartPtr();
}

gc::Fixnum ArrayObjects_O::arrayDimension(gc::Fixnum axisNumber) const {
  return this->_Dimensions[axisNumber];
}

}; /* core */
