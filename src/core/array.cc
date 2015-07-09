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
EXPOSE_CLASS(core, Array_O);

#define ARGS_cl_arrayDisplacement "(array)"
#define DECL_cl_arrayDisplacement ""
#define DOCS_cl_arrayDisplacement "arrayDisplacement"
T_mv cl_arrayDisplacement(T_sp array) {
  if (array.notnilp()) {
    if (Array_sp arr = array.asOrNull<Array_O>()) {
      return Values(_Nil<T_O>(), Fixnum_O::create(0));
    }
  }
  TYPE_ERROR(array, cl::_sym_array);
}

#define ARGS_af_upgradedArrayElementType "(type &optional env)"
#define DECL_af_upgradedArrayElementType ""
#define DOCS_af_upgradedArrayElementType "upgradedArrayElementType"
T_mv af_upgradedArrayElementType(T_sp type) {
  _G();
  return (Values(T_O::___staticClass));
};

#define ARGS_af_copy_subarray "(out outStart in inStart len)"
#define DECL_af_copy_subarray ""
#define DOCS_af_copy_subarray "copy_subarray"
void af_copy_subarray(Array_sp out, Fixnum_sp outStart, Array_sp in, Fixnum_sp inStart, Fixnum_sp len) {
  _G();
  int iLen = len->get();
  if (iLen == 0)
    return;
  ASSERTF(out->rank() == 1, BF("out array must be rank 1 - instead it is %d") % out->rank());
  ASSERTF(in->rank() == 1, BF("in array must be rank 1 - instead it is %d") % in->rank());
  int iOutStart = outStart->get();
  int iInStart = inStart->get();
  if ((iLen + iOutStart) >= out->arrayDimension(0))
    iLen = out->arrayDimension(0) - iOutStart;
  if ((iLen + iInStart) >= in->arrayDimension(0))
    iLen = in->arrayDimension(0) - iInStart;
  for (int i = 0; i < iLen; ++i) {
    out->setf_svref(iOutStart, in->svref(iInStart));
    ++iOutStart;
    ++iInStart;
  }
};

#define ARGS_af_aset "(array &rest indices-value)"
#define DECL_af_aset ""
#define DOCS_af_aset "aset"
T_sp af_aset(Array_sp array, Cons_sp indices_value) {
  _G();
  int r = cl_length(indices_value) - 1;
  int j;
  if (Vector_sp vec = array.asOrNull<Vector_O>()) {
    if (r != 1) {
      SIMPLE_ERROR(BF("Wrong number of indices"));
    }
    T_sp ind0 = oCar(indices_value);
    indices_value = cCdr(indices_value);
    j = Array_O::checkedIndex(__FILE__, __LINE__, __FUNCTION__, array, 0, ind0, cl_length(vec));
    return vec->asetUnsafe(j, oCar(indices_value));
  } else {
    if (r != array->rank()) {
      SIMPLE_ERROR(BF("Wrong number of indices."));
    }
    int i;
    for (i = j = 0; i < r; i++) {
      T_sp index = oCar(indices_value);
      indices_value = cCdr(indices_value);
      int s = Array_O::checkedIndex(__FILE__, __LINE__, __FUNCTION__, array, i, index, array->arrayDimension(i));
      j = j * (array->arrayDimension(i)) + s;
    }
    return array->asetUnsafe(j, oCar(indices_value));
  }
  IMPLEMENT_MEF(BF("Implement aset"));
};

int Array_O::checkedIndex(const string &filename, int lineno, const string &function, Array_sp array, int which, T_sp index, int nonincl_index) {
  if (Fixnum_sp fn = index.asOrNull<Fixnum_O>()) {
    int ifn = fn->get();
    if (ifn < 0 || ifn >= nonincl_index) {
      af_wrongIndex(filename, lineno, lisp_intern(function, CurrentPkg), array, which, index, nonincl_index);
    }
    return ifn;
  }
  af_wrongIndex(filename, lineno, lisp_intern(function, CurrentPkg), array, which, index, nonincl_index);
  UNREACHABLE();
}

int Array_O::arrayTotalSize() const {
  _G();
  int sz = 1;
  for (int i = 0; i < this->rank(); i++) {
    sz *= this->arrayDimension(i);
  }
  return ((sz));
}

void Array_O::initialize() {
  _OF();
  this->Base::initialize();
}

Symbol_sp Array_O::element_type_as_symbol() const {
  _G();
  IMPLEMENT_MEF(BF("Mimic ECL array.d>>ecl_elttype_to_symbol"));
}

#define ARGS_Array_O_aref "((self array) &rest indices)"
#define DECL_Array_O_aref ""
#define DOCS_Array_O_aref "See CLHS aref"
T_sp Array_O::aref(Cons_sp indices) const {
  _OF();
  SUBCLASS_MUST_IMPLEMENT();
}

int Array_O::index_vector_int(const vector<int> &indices) const {
  _G();
  int offset = 0;
  int oneIndex = 0;
  Cons_sp cur;
  int idx = 0;
  for (idx = 0; idx < this->rank(); ++idx) {
    if (idx > 0)
      offset *= this->arrayDimension(idx);
    oneIndex = indices[idx];
    offset += oneIndex;
  }
  return ((offset));
}

int Array_O::index_val(Cons_sp indices, bool last_value_is_val, Cons_sp &val_cons) const {
  _OF();
#ifdef DEBUG_ON
  int indices_passed = cl_length(indices) - (last_value_is_val ? 1 : 0);
  ASSERTF(indices_passed == (int)this->rank(),
          BF("Wrong number of indices[%d] must match rank[%d]") % indices_passed % this->rank());
#endif
  int offset = 0;
  int idx = -1;
  Cons_sp cur;
  for (cur = indices; cur.notnilp(); cur = cCdr(cur)) {
    int curDimension = this->arrayDimension(idx);
    if (cCdr(cur).nilp() && last_value_is_val)
      break;
    if (idx >= 0)
      offset *= curDimension;
    idx++;
    int oneIndex = oCar(cur).as<Rational_O>()->as_int();
    if (oneIndex >= curDimension) {
      SIMPLE_ERROR(BF("Bad index"));
    }
    offset += oneIndex;
  }
  val_cons = cur;
  return ((offset));
}

int Array_O::index(Cons_sp indices) const {
  Cons_sp dummy;
  return ((this->index_val(indices, false, dummy)));
}

int Array_O::arrayRowMajorIndex(Cons_sp indices) const {
  return ((this->index(indices)));
}

Cons_sp Array_O::arrayDimensions() const {
  _OF();
  Cons_sp indices = _Nil<Cons_O>();
  for (int i = this->rank() - 1; i >= 0; i--) {
    indices = Cons_O::create(Fixnum_O::create(this->arrayDimension(i)), indices);
  }
  return ((indices));
}

#define ARGS_Array_O_setf_aref "((self array) &rest indices-val)"
#define DECL_Array_O_setf_aref ""
#define DOCS_Array_O_setf_aref "CLHS: setter for aref"
T_sp Array_O::setf_aref(Cons_sp indices_val) {
  _G();
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

bool Array_O::equalp(T_sp other) const {
  IMPLEMENT_MEF(BF("Implement Array_O::equalp"));
}

string Array_O::__repr__() const {
  _G();
  RecursivePrint rp(this->const_sharedThis<Array_O>());
  rp.ss << "#" << this->rank() << "A(";
  rp.recurse(0);
  rp.ss << ")";
  return ((rp.ss.str()));
}

void Array_O::exposeCando(::core::Lisp_sp lisp) {
  _G();
  ::core::class_<Array_O>()
      .DEF(Array_O, aref)
      .def("core:array-setf-aref", &Array_O::setf_aref, ARGS_Array_O_setf_aref, DECL_Array_O_setf_aref, DOCS_Array_O_setf_aref)
      .def("core:index", &Array_O::index)
      .def("cl:arrayTotalSize", &Array_O::arrayTotalSize)
      .def("cl:array-dimension", &Array_O::arrayDimension)
      .def("cl:array-dimensions", &Array_O::arrayDimensions)
      .def("cl:array-elementType", &Array_O::elementType)
      .def("cl:array-rank", &Array_O::rank)
      .def("core:array-fill", &Array_O::arrayFill)
      .def("core:fill-array-with-elt", &Array_O::fillArrayWithElt)
      .def("cl:svref", &Array_O::svref)
      .def("core:setf-svref", &Array_O::setf_svref)
      .def("core:rowMajorAset", &Array_O::rowMajorAset)
      .def("cl:rowMajorAref", &Array_O::rowMajorAref)
      .def("cl:arrayRowMajorIndex", &Array_O::arrayRowMajorIndex)
      .def("cl:arrayHasFillPointerP", &Array_O::arrayHasFillPointerP)

      ;
  SYMBOL_SC_(CorePkg, copy_subarray);
  Defun(copy_subarray);
  SYMBOL_SC_(CorePkg, aset);
  Defun(aset);
  ClDefun(arrayDisplacement);
}

void Array_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, Array, "", "", _lisp)
      //	.initArgs("(self)")
      //	    .def_raw("aref",&Array_O::aref)
      .def("core:array-setf-aref", &Array_O::setf_aref)
      .def("core:index", &Array_O::index)
      .def("cl:array-dimension", &Array_O::arrayDimension)
      .def("cl:array-dimensions", &Array_O::arrayDimensions)
      .def("cl:array-rank", &Array_O::rank)
      .def("copy-array", &Array_O::shallowCopy)
      .def("array-fill", &Array_O::arrayFill);
#endif
}

}; /* core */
