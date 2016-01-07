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
#define DEBUG_LEVEL_FULL

#include <clasp/core/common.h>
#include <clasp/core/environment.h>
#include <clasp/core/str.h>
#include <clasp/core/predicates.h>
#include <clasp/core/loadTimeValues.h>
#include <clasp/core/wrappers.h>

namespace core {

// ----------------------------------------------------------------------
//

CL_LAMBDA(name);
CL_DECLARE();
CL_DOCSTRING("setRunTimeValuesVector - return true if its set and false if it was already set");
CL_DEFUN bool core__set_run_time_values_vector(const string &name) {
  if (globalTaggedRunTimeValues != NULL) {
    return false;
  }
  /*! LoadTimeValues_O are allocated in non-moving pool so we can
          set a global pointer to one of them without working about it moving */
  LoadTimeValues_sp ltv = _lisp->getOrCreateLoadTimeValues(name);
  globalTaggedRunTimeValues = reinterpret_cast<LoadTimeValues_O *>(ltv.raw_());
  return true;
};

CL_LAMBDA(name &optional (data-size 0) (symbol-size 0));
CL_DECLARE();
CL_DOCSTRING("loadTimeValueArray");
CL_DEFUN LoadTimeValues_mv core__load_time_value_array(const string &name, int dataSize, int symbolSize) {
  LoadTimeValues_sp ltv = _lisp->getOrCreateLoadTimeValues(name, dataSize, symbolSize);
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

CL_LAMBDA(name idx);
CL_DECLARE();
CL_DOCSTRING("Return the load-time-value associated with array NAME and IDX");
CL_DEFUN Symbol_sp core__lookup_load_time_symbol(const string &name, int idx) {
  int count = 0;
  T_sp tltva = _lisp->findLoadTimeValuesWithNameContaining(name, count);
  if (tltva.nilp()) {
    SIMPLE_ERROR(BF("Could not find load-time-values %s") % name);
  }
  LoadTimeValues_sp ltva = gc::As<LoadTimeValues_sp>(tltva);
  if (count != 1) {
    SIMPLE_ERROR(BF("There is more than one load-time-values object with a name that contains: %s") % name);
  }
  if (idx < 0 || idx >= ltva->numberOfValues()) {
    SIMPLE_ERROR(BF("Illegal index %d for load-time-symbol") % idx);
  }
  return ltva->symbols_element(idx);
};

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING("Return a cons of the load-time-values ids");
CL_DEFUN void core__load_time_values_ids() {
  List_sp names = _lisp->loadTimeValuesIds();
  for (auto cur : names) {
    Str_sp nm = gc::As<Str_sp>(oCar(cur));
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
    ltv = _lisp->findLoadTimeValuesWithNameContaining(gc::As<Str_sp>(nameOrLtv)->get(), count);
    if (count != 1) {
      SIMPLE_ERROR(BF("There is more than one load-time-values object with a name that contains: %s") % gc::As<Str_sp>(nameOrLtv)->get());
    }
  } else {
    ltv = gc::As<LoadTimeValues_sp>(nameOrLtv);
  }
  vector<gctools::Fixnum> vi;
  parseIndices(vi, indices);
  ltv->dumpValues(vi);
};

CL_LAMBDA(name-or-ltv &optional indices);
CL_DECLARE();
CL_DOCSTRING("Dump the load-time-values for the id _name_(string).");
CL_DEFUN void core__load_time_values_dump_symbols(T_sp nameOrLtv, T_sp indices) {
  LoadTimeValues_sp ltv;
  if (cl__stringp(nameOrLtv)) {
    int count = 0;
    ltv = _lisp->findLoadTimeValuesWithNameContaining(gc::As<Str_sp>(nameOrLtv)->get(), count);
    if (count != 1) {
      SIMPLE_ERROR(BF("There is more than one load-time-values object with a name that contains: %s") % gc::As<Str_sp>(nameOrLtv)->get());
    }
  } else {
    ltv = gc::As<LoadTimeValues_sp>(nameOrLtv);
  }
  vector<gctools::Fixnum> vi;
  parseIndices(vi, indices);
  ltv->dumpSymbols(vi);
};

EXPOSE_CLASS(core, LoadTimeValues_O);

#define ARGS_LoadTimeValues_O_make "(dimension)"
#define DECL_LoadTimeValues_O_make ""
#define DOCS_LoadTimeValues_O_make "This is a thin wrapper around VectorObjectsWithFillPtr - it creates a place to store LoadTimeValues"
CL_LISPIFY_NAME(make-load-time-values);
CL_DEFUN LoadTimeValues_sp LoadTimeValues_O::make(int dataDimension, int symbolsDimension) {
  GC_ALLOCATE(LoadTimeValues_O, vo);
  vo->_Objects.resize(dataDimension, _Nil<T_O>());
  vo->_Symbols.resize(symbolsDimension, _Nil<Symbol_O>());
  return vo;
}

SYMBOL_SC_(CorePkg, loadTimeValuesIds);
SYMBOL_SC_(CorePkg, loadTimeValueArray);
SYMBOL_SC_(CorePkg, lookupLoadTimeValue);
SYMBOL_SC_(CorePkg, lookupLoadTimeSymbol);
SYMBOL_EXPORT_SC_(CorePkg, setRunTimeValuesVector);


void LoadTimeValues_O::exposeCando(::core::Lisp_sp lisp) {
  ::core::class_<LoadTimeValues_O>()
      .def("data_vectorPushExtend", &LoadTimeValues_O::data_vectorPushExtend)
      .def("symbols_vectorPushExtend", &LoadTimeValues_O::symbols_vectorPushExtend);
//  Defun_maker(CorePkg, LoadTimeValues);
}

void LoadTimeValues_O::exposePython(::core::Lisp_sp lisp) {
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(Pkg(), LoadTimeValues, "", "", _LISP)
      //	.initArgs("(self)")
      ;
#endif
}

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
  printf("%s:%d Dumping Values LTV@%p  size %lu  LTS size %lu\n", __FILE__, __LINE__, this, this->_Objects.size(), this->_Symbols.size());
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

void LoadTimeValues_O::dumpSymbols(vector<gctools::Fixnum> &indices) {
  printf("%s:%d  Dumping Symbols  LTV@%p  size %lu  LTS size %lu\n", __FILE__, __LINE__, this, this->_Objects.size(), this->_Symbols.size());
  if (indices.size() == 0) {
    for (int i = 0, iEnd(this->_Symbols.size()); i < iEnd; i++) {
      Symbol_sp &obj = this->_Symbols[i];
      stringstream ss;
      dumpOneValue(ss, obj);
      printf("LTS[%4d]@%p --> %s(base@%p)\n", i, (void *)(&this->_Symbols[i]), ss.str().c_str(), obj.objectp() ? obj.raw_() : NULL);
    }
  } else {
    for (int i = 0, iEnd(indices.size()); i < iEnd; ++i) {
      int idx = indices[i];
      Symbol_sp &obj = this->_Symbols[idx];
      stringstream ss;
      dumpOneValue(ss, obj);
      printf("LTS[%4d]@%p --> %s(base@%p)\n", idx, (void *)(&this->_Symbols[idx]), ss.str().c_str(), obj.objectp() ? obj.raw_() : NULL);
    }
  }
}

/*! Ignore extension */
CL_LISPIFY_NAME("data_vectorPushExtend");
CL_DEFMETHOD int LoadTimeValues_O::data_vectorPushExtend(T_sp val, int extension) {
  int idx = this->_Objects.size();
  this->_Objects.push_back(val);
  return idx;
}

void LoadTimeValues_O::symbols_setFillPointer(uint i) {
  ASSERT(i == 0);
  this->_Symbols.resize(i);
}

CL_LISPIFY_NAME("symbols_vectorPushExtend");
CL_DEFMETHOD int LoadTimeValues_O::symbols_vectorPushExtend(Symbol_sp val, int extension) {
  int i = this->_Symbols.size();
  this->_Symbols.push_back(val);
  return i;
}

#if 0 // Depreciated

    EXPOSE_CLASS(core,MemoryLockedLoadTimeValuesPointer_O);

    void MemoryLockedLoadTimeValuesPointer_O::exposeCando(::core::Lisp_sp lisp)
    {
	::core::class_<MemoryLockedLoadTimeValuesPointer_O>()
	    ;
    }

    void MemoryLockedLoadTimeValuesPointer_O::exposePython(::core::Lisp_sp lisp)
    {
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(Pkg(),MemoryLockedLoadTimeValuesPointer,"","",_LISP)
//	.initArgs("(self)")
	    ;
#endif
    }

#endif

}; /* core */
