/*
    File: loadTimeValues.cc
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

#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/environment.h>
#include <clasp/core/array.h>
#include <clasp/core/predicates.h>
#include <clasp/core/loadTimeValues.h>
#include <clasp/core/wrappers.h>

namespace core {

// ----------------------------------------------------------------------
//

#if 0
CL_LAMBDA(name);
CL_DECLARE();
CL_DOCSTRING("setRunTimeValuesVector - return true if its set and false if it was already set");
CL_DEFUN bool core__set_run_time_values_table(const string &name) {
  if (run_time_values_table != NULL) {
    return false;
  }
  /*! LoadTimeValues_O are allocated in non-moving pool so we can
          set a global pointer to one of them without working about it moving */
  LoadTimeValues_sp ltv = _lisp->getOrCreateLoadTimeValues(name);
  run_time_values_table = reinterpret_cast<LoadTimeValues_O *>(ltv.raw_());
  return true;
};

CL_LAMBDA(name &optional (data-size 0));
CL_DECLARE();
CL_DOCSTRING("loadTimeValueArray");
CL_DEFUN LoadTimeValues_sp core__load_time_value_array(const string &name, size_t dataSize) {
  LoadTimeValues_sp ltv = _lisp->getOrCreateLoadTimeValues(name, dataSize);
  return Values(ltv);
};

CL_LAMBDA(name idx);
CL_DECLARE();
CL_DOCSTRING("Return the load-time-value associated with array NAME and IDX");
CL_DEFUN T_sp core__lookup_load_time_value(const string &name, int idx) {
  int count = 0;
  LoadTimeValues_sp ltva = gc::As<LoadTimeValues_sp>(_lisp->findLoadTimeValuesWithNameContaining(name, count));
  if (count != 1) {
    SIMPLE_ERROR(BF("There is more than one load-time-values object with a name that contains: %s") % name);
  }
  if (idx < 0 || idx >= ltva->numberOfValues()) {
    SIMPLE_ERROR(BF("Illegal index %d for load-time-value") % idx);
  }
  return ltva->data_element(idx);
};
#endif

#if 0
CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("Return a cons of the load-time-values ids");
CL_DEFUN void core__load_time_values_ids() {
  List_sp names = _lisp->loadTimeValuesIds();
  for (auto cur : names) {
    SimpleBaseString_sp nm = gc::As<SimpleBaseString_sp>(oCar(cur));
    T_sp ltv = _lisp->findLoadTimeValues(nm->get());
    printf("%s:%d LTV[%s]@%p = %s\n", __FILE__, __LINE__, nm->get().c_str(), ltv.raw_(), _rep_(ltv).c_str());
  }
};

void parseIndices(vector<gctools::Fixnum> &vec_indices, T_sp indices) {
  if (indices.nilp()) {
    vec_indices.clear();
  } else if (indices.fixnump()) {
    vec_indices.push_back(indices.unsafe_fixnum());
  } else if (Cons_sp ccur = indices.asOrNull<Cons_O>()) {
    List_sp cur = ccur;
    for (; cur.notnilp(); cur = oCdr(cur)) {
      T_sp val = oCar(cur);
      if (val.notnilp()) {
        if (val.fixnump()) {
          vec_indices.push_back(val.unsafe_fixnum());
        } else {
          SIMPLE_ERROR(BF("Illegal index"));
        }
      } else
        SIMPLE_ERROR(BF("Illegal nil index"));
    }
  }
}

CL_LAMBDA(name-or-ltv &optional indices);
CL_DECLARE();
CL_DOCSTRING("Dump the load-time-values for the id _name_(string).");
CL_DEFUN void core__load_time_values_dump_values(T_sp nameOrLtv, T_sp indices) {
  LoadTimeValues_sp ltv;
  if (cl__stringp(nameOrLtv)) {
    int count = 0;
    ltv = _lisp->findLoadTimeValuesWithNameContaining(gc::As<String_sp>(nameOrLtv)->get(), count);
    if (count != 1) {
      SIMPLE_ERROR(BF("There is more than one load-time-values object with a name that contains: %s") % gc::As<String_sp>(nameOrLtv)->get());
    }
  } else {
    ltv = gc::As<LoadTimeValues_sp>(nameOrLtv);
  }
  vector<gctools::Fixnum> vi;
  parseIndices(vi, indices);
  ltv->dumpValues(vi);
};
#endif

#if 0
CL_LISPIFY_NAME(make-load-time-values);
CL_DEFUN LoadTimeValues_sp LoadTimeValues_O::make(size_t dataDimension) {
  GC_ALLOCATE(LoadTimeValues_O, vo);
  vo->_Objects.resize(dataDimension, _Nil<T_O>());
//  vo->_Symbols.resize(symbolsDimension, _Nil<Symbol_O>());
  return vo;
}
#endif



SYMBOL_SC_(CorePkg, loadTimeValuesIds);
SYMBOL_SC_(CorePkg, loadTimeValueArray);
SYMBOL_SC_(CorePkg, lookupLoadTimeValue);
//SYMBOL_SC_(CorePkg, lookupLoadTimeSymbol);
SYMBOL_EXPORT_SC_(CorePkg, setRunTimeValuesVector);




#if 0
void dumpOneValue(stringstream &ss, T_sp val) {
  if (val.nilp()) {
    ss << "NIL";
  } else if (val.fixnump()) {
    ss << val.unsafe_fixnum();
  } else if (val.objectp()) {
    ss << _rep_(val);
  } else {
    ss << "Unknown object!!!!";
  }
}

void LoadTimeValues_O::dumpValues(vector<gctools::Fixnum> &indices) {
  printf("%s:%d Dumping Values LTV@%p  size %" PRu "\n", __FILE__, __LINE__, this, this->_Objects.size());
  if (indices.size() == 0) {
    for (int i = 0, iEnd(this->_Objects.size()); i < iEnd; i++) {
      T_sp &obj = this->_Objects[i];
      stringstream ss;
      dumpOneValue(ss, obj);
      printf("LTV[%4d]@%p --> %s(base@%p)\n", i, (void *)(&this->_Objects[i]), ss.str().c_str(), obj.objectp() ? obj.raw_() : NULL);
    }
  } else {
    for (int i = 0, iEnd(indices.size()); i < iEnd; ++i) {
      int idx = indices[i];
      T_sp &obj = this->_Objects[idx];
      stringstream ss;
      dumpOneValue(ss, obj);
      printf("LTV[%4d]@%p --> %s(base@%p)\n", idx, (void *)(&this->_Objects[idx]), ss.str().c_str(), obj.objectp() ? obj.raw_() : NULL);
    }
  }
}
#endif


#if 0
/*! Ignore extension */
CL_LISPIFY_NAME("data_vectorPushExtend");
CL_DEFMETHOD size_t LoadTimeValues_O::data_vectorPushExtend(T_sp val, size_t extension) {
  size_t idx = this->_Objects.size();
  this->_Objects.push_back(val);
  return idx;
}
#endif



}; /* core */
