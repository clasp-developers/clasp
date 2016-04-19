/*
    File: array.cc
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
#include <clasp/core/corePackage.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/environment.h>
#include <clasp/core/vectorObjects.h>
#include <clasp/core/array.h>
#include <clasp/core/arrayObjects.h>
#include <clasp/core/wrappers.h>
namespace core {

// ----------------------------------------------------------------------
//


CL_LAMBDA(core::array);
CL_DECLARE();
CL_DOCSTRING("arrayDisplacement");
CL_DEFUN T_mv cl__array_displacement(T_sp array) {
  if (array.notnilp()) {
    if (Array_sp arr = array.asOrNull<Array_O>()) {
      (void)arr;
      return Values(_Nil<T_O>(), make_fixnum(0));
    }
  }
  TYPE_ERROR(array, cl::_sym_array);
}

CL_LAMBDA(core::type &optional core::env);
CL_DECLARE();
CL_DOCSTRING("upgradedArrayElementType");
CL_DEFUN T_mv cl__upgraded_array_element_type(T_sp type) {
  return (Values(T_O::static_class));
};

CL_LAMBDA(out outStart in inStart len);
CL_DECLARE();
CL_DOCSTRING("copy_subarray");
CL_DEFUN void core__copy_subarray(Array_sp out, Fixnum_sp outStart, Array_sp in, Fixnum_sp inStart, Fixnum_sp len) {
  // TODO: THIS NEEDS TO BE OPTIMIZED FOR DIFFERENT TYPES OF ARRAYS!!!!!!!
  //       Currently this is very inefficient
  int iLen = unbox_fixnum(len);
  if (iLen == 0)
    return;
  ASSERTF(out->rank() == 1, BF("out array must be rank 1 - instead it is %d") % out->rank());
  ASSERTF(in->rank() == 1, BF("in array must be rank 1 - instead it is %d") % in->rank());
  int iOutStart = unbox_fixnum(outStart);
  int iInStart = unbox_fixnum(inStart);
  if ((iLen + iOutStart) >= out->arrayDimension(0))
    iLen = out->arrayDimension(0) - iOutStart;
  if ((iLen + iInStart) >= in->arrayDimension(0))
    iLen = in->arrayDimension(0) - iInStart;
  if (iOutStart < iInStart) {
    for (int i = 0; i < iLen; ++i) {
      out->aset_unsafe(iOutStart, in->aref_unsafe(iInStart));
      ++iOutStart;
      ++iInStart;
    }
  } else {
    iOutStart += iLen;
    iInStart += iLen;
    for (int i = 0; i < iLen; ++i) {
      --iOutStart;
      --iInStart;
      out->aset_unsafe(iOutStart, in->aref_unsafe(iInStart));
    }
  }
}

#if 0 // DEPRECIATED???
CL_LAMBDA(array &rest indices-value);
CL_DECLARE();
CL_DOCSTRING("aset");
CL_DEFUN T_sp core__aset(Array_sp array, List_sp indices_value) {
  int r = cl__length(indices_value) - 1;
  int j;
  if (Vector_sp vec = array.asOrNull<Vector_O>()) {
    if (r != 1) {
      SIMPLE_ERROR(BF("Wrong number of indices"));
    }
    T_sp ind0 = oCar(indices_value);
    indices_value = oCdr(indices_value);
    j = Array_O::checkedIndex(__FILE__, __LINE__, __FUNCTION__, array, 0, ind0, cl__length(vec));
    return vec->aset_unsafe(j, oCar(indices_value));
  } else {
    if (r != array->rank()) {
      SIMPLE_ERROR(BF("Wrong number of indices."));
    }
    int i;
    for (i = j = 0; i < r; i++) {
      T_sp index = oCar(indices_value);
      indices_value = oCdr(indices_value);
      int s = Array_O::checkedIndex(__FILE__, __LINE__, __FUNCTION__, array, i, index, array->arrayDimension(i));
      j = j * (array->arrayDimension(i)) + s;
    }
    return array->aset_unsafe(j, oCar(indices_value));
  }
  IMPLEMENT_MEF(BF("Implement aset"));
};
#endif



int Array_O::checkedIndex(const string &filename, int lineno, const string &function, Array_sp array, int which, T_sp index, int nonincl_index) {
  if (index.fixnump()) {
    int ifn = unbox_fixnum(gc::As<Fixnum_sp>(index));
    if (ifn < 0 || ifn >= nonincl_index) {
      core__wrong_index(filename, lineno, lisp_intern(function, CurrentPkg), array, which, index, nonincl_index);
    }
    return ifn;
  }
  core__wrong_index(filename, lineno, lisp_intern(function, CurrentPkg), array, which, index, nonincl_index);
  UNREACHABLE();
}

CL_LISPIFY_NAME("cl:arrayTotalSize");
CL_DEFMETHOD gc::Fixnum Array_O::arrayTotalSize() const {
  gc::Fixnum sz = 1;
  for (int i = 0; i < this->rank(); i++) {
    sz *= this->arrayDimension(i);
  }
  return sz;
}

void Array_O::initialize() {
  this->Base::initialize();
}

Symbol_sp Array_O::element_type_as_symbol() const {
  // If this fails we need a different way of doing this
  if ( cl__symbolp(this->elementType()) ) {
    return this->elementType();
  }
  if (this->elementType() == _lisp->_true()) {
    return cl::_sym_T;
  }
  if (this->elementType() == cl__find_class(cl::_sym_DoubleFloat_O) ) {
    return cl::_sym_DoubleFloat_O;
  }
  SIMPLE_ERROR(BF("Handle more array types - the current array type is: %s") % _rep_(this->elementType()));
}

CL_LISPIFY_NAME("cl:aref");
CL_LAMBDA((core::self cl:array) &va-rest core::indices);
CL_DEFMETHOD T_sp Array_O::aref(VaList_sp indices) const {
  SUBCLASS_MUST_IMPLEMENT();
}

cl_index Array_O::index_vector_int(const vector<int> &indices) const {
  cl_index offset = 0;
  cl_index oneIndex = 0;
  Cons_sp cur;
  cl_index idx = 0;
  for (idx = 0; idx < this->rank(); ++idx) {
    if (idx > 0)
      offset *= this->arrayDimension(idx);
    oneIndex = indices[idx];
    offset += oneIndex;
  }
  return ((offset));
}

cl_index Array_O::index_val_(List_sp indices, bool last_value_is_val, T_sp &last_val) const {
  int indices_passed = cl__length(indices) - (last_value_is_val ? 1 : 0);
#ifdef DEBUG_ON
  ASSERTF(indices_passed == (int)this->rank(),
          BF("Wrong number of indices[%d] must match rank[%d]") % indices_passed % this->rank());
#endif
  cl_index offset = 0;
  cl_index idx = 0;
  cl_index idxEnd(indices_passed);
  List_sp cur = indices;;
  for ( ; idx<idxEnd; ++idx ) {
    T_sp index = oCar(cur);
    cl_index curDimension = this->arrayDimension(idx);
    cl_index oneIndex = clasp_to_int(gc::As<Rational_sp>(index));
    if (oneIndex < 0 || oneIndex >= curDimension) {
      SIMPLE_ERROR(BF("Bad index %d - must be [0,%d)") % curDimension);
    }
    offset = offset * curDimension + oneIndex;
  }
  if (last_value_is_val) last_val = oCar(cur);
  return offset;
}

cl_index Array_O::index_val_(VaList_sp indices, bool last_value_is_val, T_sp &last_val) const {
  int indices_passed = LCC_VA_LIST_NUMBER_OF_ARGUMENTS(indices) - (last_value_is_val ? 1 : 0);
#ifdef DEBUG_ON
  ASSERTF(indices_passed == (int)this->rank(),
          BF("Wrong number of indices[%d] must match rank[%d]") % indices_passed % this->rank());
#endif
  cl_index offset = 0;
  cl_index idx = 0;
  cl_index idxEnd(indices_passed);
  for ( ; idx < idxEnd; ++idx) {
    core::T_sp cur = LCC_NEXT_ARG(indices,idx);
    cl_index curDimension = this->arrayDimension(idx);
    cl_index oneIndex = clasp_to_int(gc::As<Rational_sp>(cur));
    if (oneIndex < 0 || oneIndex >= curDimension) {
      SIMPLE_ERROR(BF("Bad index %d - must be [0,%d)") % curDimension);
    }
    offset = offset * curDimension + oneIndex;
  }
  if (last_value_is_val) {
    last_val = LCC_NEXT_ARG(indices,idx);
  }
  return offset;
}

CL_LAMBDA(array &va-rest indices);
CL_LISPIFY_NAME("core:index");
CL_DEFUN gc::Fixnum core__index(Array_sp array, VaList_sp indices) {
  T_sp dummy;
  return array->index_val_(indices, false, dummy);
}

CL_LAMBDA((core::self array) &va-rest core::indices);
CL_LISPIFY_NAME("cl:arrayRowMajorIndex");
CL_DEFMETHOD gc::Fixnum Array_O::arrayRowMajorIndex(VaList_sp indices) const {
  return this->index_(indices);
}

CL_LISPIFY_NAME("cl:array-dimensions");
CL_DEFMETHOD List_sp Array_O::arrayDimensions() const {
  _OF();
  List_sp indices = _Nil<T_O>();
  for (int i = this->rank() - 1; i >= 0; i--) {
    indices = Cons_O::create(make_fixnum(this->arrayDimension(i)), indices);
  }
  return ((indices));
}

CL_LAMBDA((core::self array) &rest core::indices-val);
CL_DOCSTRING("Setter for aref");
CL_LISPIFY_NAME("core:array-setf-aref");
CL_DEFMETHOD T_sp Array_O::setf_aref(List_sp indices_val) {
  SUBCLASS_MUST_IMPLEMENT();
};

struct RecursivePrint {
  Array_sp me;
  int depth;
  vector<int> indices;
  stringstream ss;

  RecursivePrint(const Array_sp &array) {
    this->me = array;
    this->indices.resize(array->rank(), 0);
    this->depth = array->rank() - 1;
  }
  void recurse(int level) {
    while (1) {
      if (level < depth) {
        ss << "(";
        recurse(level + 1);
        ss << ")";
      } else {
        ss << _rep_(this->me->rowMajorAref(this->me->index_vector_int(this->indices))) << " ";
      }
      if (!this->advanceIndices(level))
        break;
    }
  }

  bool advanceIndices(int level) {
    this->indices[level]++;
    if (this->indices[level] < this->me->arrayDimension(level))
      return ((true));
    this->indices[level] = 0;
    return ((false));
  }
};

string Array_O::__repr__() const {
  RecursivePrint rp(this->asSmartPtr());
  rp.ss << "#" << this->rank() << "A(";
  rp.recurse(0);
  rp.ss << ")";
  return ((rp.ss.str()));
}

SYMBOL_SC_(CorePkg, copy_subarray);
SYMBOL_SC_(CorePkg, aset);




}; /* core */
