/*
    File: primitives.cc
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

#include <unistd.h>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/cons.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/environment.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/bformat.h>
#include <clasp/core/bignum.h>
#include <clasp/core/character.h>
#include <clasp/core/executables.h>
#include <clasp/core/package.h>
#include <clasp/core/readtable.h>
#include <clasp/core/vectorObjectsWithFillPtr.h>
#include <clasp/core/instance.h>
#include <clasp/core/backquote.h>
#include <clasp/core/sequence.h>
#include <clasp/core/structureObject.h>
#include <clasp/core/wrappedPointer.h>
#include <clasp/core/bitVector.h>
#include <clasp/core/pathname.h>
#include <clasp/core/unixfsys.h>
#include <clasp/core/predicates.h>
#include <clasp/core/genericFunction.h>
#include <clasp/core/pointer.h>
#include <clasp/core/lispMath.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/null.h>
//#include "debugger.h"
#include <clasp/core/ql.h>
#include <clasp/core/str.h>
#include <clasp/core/numbers.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/compiler.h>
#include <clasp/core/print.h>
#include <clasp/core/standardClass.h>
#include <clasp/core/forwardReferencedClass.h>
#include <clasp/core/singleDispatchMethod.h>
#include <clasp/core/singleDispatchGenericFunction.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/designators.h>
#include <clasp/core/primitives.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/hashTableEql.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/structureObject.h>
#include <clasp/core/lispReader.h>
#include <clasp/core/designators.h>
#include <clasp/core/profile.h>
#include <clasp/core/wrappers.h>
CL_NAMESPACE namespace core {

#define ARGS_cl_sleep "(seconds)"
#define DECL_cl_sleep ""
#define DOCS_cl_sleep "sleep"
void cl_sleep(T_sp oseconds) {
  _G();
  SYMBOL_EXPORT_SC_(ClPkg, sleep);
  if (oseconds.nilp()) {
    ERROR_WRONG_TYPE_ONLY_ARG(cl::_sym_sleep, oseconds, cl::_sym_Number_O);
  }
  double dsec = clasp_to_double(gc::As<Real_sp>(oseconds));
  if (dsec < 0.0) {
    SIMPLE_ERROR(BF("You cannot sleep for < 0 seconds"));
  }
  double seconds = floor(dsec);
  double frac_seconds = dsec - seconds;
  double nanoseconds = (frac_seconds * 1000000000.0);
  timespec ts;
  ts.tv_sec = seconds;
  ts.tv_nsec = nanoseconds;
  nanosleep(&ts, NULL);
}

#define ARGS_cl_lispImplementationType "()"
#define DECL_cl_lispImplementationType ""
#define DOCS_cl_lispImplementationType "lispImplementationType"
T_sp cl_lispImplementationType() {
  _G();
  return Str_O::create("Clasp");
};

#define ARGS_cl_lispImplementationVersion "()"
#define DECL_cl_lispImplementationVersion ""
#define DOCS_cl_lispImplementationVersion "lispImplementationVersion"
T_sp cl_lispImplementationVersion() {
  _G();
  stringstream ss;
  List_sp cleavir = gc::As<Cons_sp>(cl::_sym_STARfeaturesSTAR->symbolValue())->memberEq(kw::_sym_cclasp);
  if (cleavir.notnilp()) {
    ss << "cclasp-";
  }
#ifdef USE_MPS
  ss << "mps-";
#endif
#ifdef USE_BOEHM
  ss << "boehm-";
#endif
  ss << CLASP_VERSION;
  return Str_O::create(ss.str());
};

#define ARGS_core_method_cache_resize "(pow)"
#define DECL_core_method_cache_resize ""
#define DOCS_core_method_cache_resize "cache_resize - Resize the cache to 2^pow"
void core_method_cache_resize(Fixnum_sp pow) {
  if (pow < 2 || pow > 64) {
    SIMPLE_ERROR(BF("Cache power must be in the range of 2...64"));
  }
  size_t size = 1 << pow;
  return _lisp->_Roots._MethodCachePtr->setup(Lisp_O::MaxFunctionArguments, size);
}

#define ARGS_core_slot_cache_resize "(pow)"
#define DECL_core_slot_cache_resize ""
#define DOCS_core_slot_cache_resize "cache_resize - Resize the cache to 2^pow"
void core_slot_cache_resize(Fixnum_sp pow) {
  if (pow < 2 || pow > 64) {
    SIMPLE_ERROR(BF("Cache power must be in the range of 2...64"));
  }
  size_t size = 1 << pow;
  return _lisp->_Roots._SlotCachePtr->setup(Lisp_O::MaxClosSlots, size);
}

#define ARGS_core_single_dispatch_method_cache_resize "(pow)"
#define DECL_core_single_dispatch_method_cache_resize ""
#define DOCS_core_single_dispatch_method_cache_resize "cache_resize - Resize the cache to 2^pow"
void core_single_dispatch_method_cache_resize(Fixnum_sp pow) {
  if (pow < 2 || pow > 64) {
    SIMPLE_ERROR(BF("Cache power must be in the range of 2...64"));
  }
  size_t size = 1 << pow;
  return _lisp->_Roots._SingleDispatchMethodCachePtr->setup(2, size);
}

#define ARGS_core_method_cache_status "()"
#define DECL_core_method_cache_status ""
#define DOCS_core_method_cache_status "cache_status - (values searches misses total-depth)"
T_mv core_method_cache_status() {
  return Values(clasp_make_fixnum(_lisp->_Roots._MethodCachePtr->_searches),
                clasp_make_fixnum(_lisp->_Roots._MethodCachePtr->_misses),
                clasp_make_fixnum(_lisp->_Roots._MethodCachePtr->_total_depth));
}
#define ARGS_core_slot_cache_status "()"
#define DECL_core_slot_cache_status ""
#define DOCS_core_slot_cache_status "cache_status - (values searches misses total-depth)"
T_mv core_slot_cache_status() {
  return Values(clasp_make_fixnum(_lisp->_Roots._SlotCachePtr->_searches),
                clasp_make_fixnum(_lisp->_Roots._SlotCachePtr->_misses),
                clasp_make_fixnum(_lisp->_Roots._SlotCachePtr->_total_depth));
}

#define ARGS_core_single_dispatch_method_cache_status "()"
#define DECL_core_single_dispatch_method_cache_status ""
#define DOCS_core_single_dispatch_method_cache_status "cache_status - (values searches misses total-depth)"
T_mv core_single_dispatch_method_cache_status() {
  return Values(clasp_make_fixnum(_lisp->_Roots._SingleDispatchMethodCachePtr->_searches),
                clasp_make_fixnum(_lisp->_Roots._SingleDispatchMethodCachePtr->_misses),
                clasp_make_fixnum(_lisp->_Roots._SingleDispatchMethodCachePtr->_total_depth));
}

#define ARGS_core_lispImplementationId "()"
#define DECL_core_lispImplementationId ""
#define DOCS_core_lispImplementationId "lispImplementationId - the git commit sha1 code"
T_sp core_lispImplementationId() {
  _G();
  string all = CLASP_GIT_COMMIT;
#define RIGHT_CHARS 8
  string rightChars;
  if (all.size() > RIGHT_CHARS) {
    rightChars = all.substr(all.size() - RIGHT_CHARS);
  } else {
    rightChars = all;
  }
  return Str_O::create(rightChars);
};

#define ARGS_core_create_tagged_immediate_value_or_nil "(obj)"
#define DECL_core_create_tagged_immediate_value_or_nil ""
#define DOCS_core_create_tagged_immediate_value_or_nil "Convert an object, either a fixnum, character or single float into an tagged version and return as an integer (either Fixnum or Bignum) or return NIL"
T_sp core_create_tagged_immediate_value_or_nil(T_sp object) {
  if (object.fixnump() || object.characterp() || object.single_floatp()) {
    return Integer_O::create((gc::Fixnum)object.raw_());
  }
  return _Nil<T_O>();
};

#define ARGS_cl_softwareType "()"
#define DECL_cl_softwareType ""
#define DOCS_cl_softwareType "softwareType"
T_sp cl_softwareType() {
  return _Nil<T_O>();
};

#define ARGS_cl_softwareVersion "()"
#define DECL_cl_softwareVersion ""
#define DOCS_cl_softwareVersion "softwareVersion"
T_sp cl_softwareVersion() {
  _G();
  string all = CLASP_VERSION;
  return Str_O::create(all);
};

#define ARGS_cl_machineType "()"
#define DECL_cl_machineType ""
#define DOCS_cl_machineType "machineType"
T_sp cl_machineType() {
  _G();
  return _Nil<T_O>();
};

#define ARGS_cl_machineVersion "()"
#define DECL_cl_machineVersion ""
#define DOCS_cl_machineVersion "machineVersion"
T_sp cl_machineVersion() {
  _G();
  return _Nil<T_O>();
};

#define ARGS_cl_machineInstance "()"
#define DECL_cl_machineInstance ""
#define DOCS_cl_machineInstance "machineInstance"
T_sp cl_machineInstance() {
  _G();
  return _Nil<T_O>();
};

#define ARGS_af_argc "()"
#define DECL_af_argc ""
#define DOCS_af_argc "argc"
int af_argc() {
  _G();
  return _lisp->_Argc;
};

#define ARGS_af_argv "(idx)"
#define DECL_af_argv ""
#define DOCS_af_argv "argv"
Str_sp af_argv(int idx) {
  _G();
  return Str_O::create(_lisp->_Argv[idx]);
};

#define ARGS_cl_set "(sym value)"
#define DECL_cl_set ""
#define DOCS_cl_set "set"
T_sp cl_set(Symbol_sp sym, T_sp val) {
  _G();
  if (sym.nilp()) {
    SIMPLE_ERROR(BF("You cannot assign to the constant NIL"));
  }
  sym->setf_symbolValue(val);
  return val;
};

#define ARGS_af_dumpAddressOf "(arg)"
#define DECL_af_dumpAddressOf ""
#define DOCS_af_dumpAddressOf "dumpAddressOf"
void af_dumpAddressOf(T_sp arg) {
  _G();
  ASSERT(arg.objectp());
  void *ptr = &(*arg);
  printf("%s:%d  AddressOf = %p\n", __FILE__, __LINE__, ptr);
};

#define ARGS_af_incompleteNextHigherPowerOf_2 "(arg)"
#define DECL_af_incompleteNextHigherPowerOf_2 ""
#define DOCS_af_incompleteNextHigherPowerOf_2 "incompleteNextHigherPowerOf_2 - see the incompleteNextHigherPowerOf_2 builtin - only works for Fixnums and not the full range; just for testing"
int af_incompleteNextHigherPowerOf_2(Fixnum_sp fn) {
  _G();
  unsigned int f = unbox_fixnum(fn);
  return 1 << ((sizeof(f) * 8) - __builtin_clz(f));
};

#define ARGS_af_allRegisteredClassNames "()"
#define DECL_af_allRegisteredClassNames ""
#define DOCS_af_allRegisteredClassNames "allRegisteredClassNames"
Vector_sp af_allRegisteredClassNames() {
  _G();
  VectorObjects_sp vo = VectorObjects_O::make(_Nil<T_O>(), _Nil<T_O>(), _lisp->classSymbolsHolder().size(), false, cl::_sym_T_O);
  for (int i(0), iEnd(_lisp->classSymbolsHolder().size()); i < iEnd; ++i) {
    vo->setf_elt(i, _lisp->classSymbolsHolder()[i]);
  }
  return vo;
};

#define ARGS_af_toTaggedFixnum "(arg)"
#define DECL_af_toTaggedFixnum ""
#define DOCS_af_toTaggedFixnum "toTaggedFixnum"
T_sp af_toTaggedFixnum(int val) {
  _G();
  return gctools::smart_ptr<T_O>(val);
};

#define ARGS_af_fromTaggedFixnum "(val)"
#define DECL_af_fromTaggedFixnum ""
#define DOCS_af_fromTaggedFixnum "fromTaggedFixnum"
gctools::Fixnum af_fromTaggedFixnum(T_sp val) {
  _G();
  if (val.fixnump()) {
    return val.unsafe_fixnum();
  }
  SIMPLE_ERROR(BF("Not a fixnum"));
};

#define ARGS_af_dumpTaggedFixnum "(arg)"
#define DECL_af_dumpTaggedFixnum ""
#define DOCS_af_dumpTaggedFixnum "dumpTaggedFixnum"
void af_dumpTaggedFixnum(T_sp val) {
  _G();
  if (val.fixnump()) {
    printf("%s:%d Raw TaggedFixnum %p   Untagged %ld\n",
           __FILE__, __LINE__, val.raw_(), val.unsafe_fixnum());
  } else
    printf("%s:%d Not a tagged fixnum\n", __FILE__, __LINE__);
}

#define ARGS_af_getEnv "(arg)"
#define DECL_af_getEnv ""
#define DOCS_af_getEnv "getEnv"
T_sp af_getEnv(Str_sp arg) {
  _G();
  char *sres = getenv(arg->c_str());
  if (sres == NULL) {
    return _Nil<T_O>();
  }
  return Str_O::create(sres);
};


LAMBDA();
DECLARE();
DOCSTRING(R"doc(Return a string representing the llvm version (eg: 3.6.0))doc")
CL_DEFUN T_sp ext_llvm_version() {
  return core::Str_O::create(LLVM_VERSION);
}


#define ARGS_core_describe_cxx_object "(name &optional stream)"
#define DECL_core_describe_cxx_object ""
#define DOCS_core_describe_cxx_object "Describe a C++ object as CL:DESCRIBE"
LAMBDA(name &optional stream)
DECLARE()
DOCSTRING(R"doc(Describe a
C++ object
like CL:DESCRIBE)doc")
CL_DEFUN void core_describe_cxx_object(T_sp obj, T_sp stream)
{
  if (obj.generalp()) {
    obj->describe(stream);
  } else if (obj.consp()) {
    obj->describe(stream);
  }
  SIMPLE_ERROR(BF("Use the CL facilities to describe this object"));
};

#define ARGS_core_setenv "(name arg overwrite)"
#define DECL_core_setenv ""
#define DOCS_core_setenv "getEnv"
LAMBDA(name arg overwrite)
DECLARE()
DOCSTRING(R"doc(Set an environment variable)doc")
CL_DEFUN void core_setenv(Str_sp name, Str_sp arg, bool overwrite) {
  setenv(name->c_str(), arg->c_str(), overwrite);
};

#define ARGS_core_pointer "(arg)"
#define DECL_core_pointer ""
#define DOCS_core_pointer "Return the value of the pointer - used by conditions.lsp"
LAMBDA(arg)
DECLARE()
DOCSTRING(R"doc(Return the value of a pointer - used by conditions.lsp - not useful in MPS)doc")
CL_DEFUN int core_pointer(T_sp obj) {
  _G();
  return obj.intptr();
};

#define ARGS_af_isTrue "(arg)"
#define DECL_af_isTrue ""
#define DOCS_af_isTrue "isTrue"
bool af_isTrue(T_sp arg) {
  _G();
  return arg.isTrue();
};

#define ARGS_af_substitute "(arg)"
#define DECL_af_substitute ""
#define DOCS_af_substitute "substitute"
T_mv af_substitute() {
  _G();
  IMPLEMENT_MEF(BF("Implement substitute"));
};

#define ARGS_af_unbound "()"
#define DECL_af_unbound ""
#define DOCS_af_unbound "Return the UNBOUND value"
T_sp af_unbound() {
  _G();
  return _Unbound<T_O>();
};

#define ARGS_af_smartPointerDetails "()"
#define DECL_af_smartPointerDetails ""
#define DOCS_af_smartPointerDetails "smartPointerDetails - returns (values ptr-type px-offset px-size). The ptr-type is the type of pointer used to pass objects - either MPS-GARBAGE-COLLECTION or INTRUSIVE-REFERENCE-COUNTED-POINTER. The px-offset is the number of bytes offset of the smart_ptr data pointer from the start of the smart_ptr and px-size is the size of the data pointer"
T_mv af_smartPointerDetails() {
  _G();
  SYMBOL_SC_(CorePkg, intrusiveReferenceCountedPointer);
  SYMBOL_SC_(CorePkg, sharedReferenceCountedPointer);
  SYMBOL_SC_(CorePkg, mpsGarbageCollection);
#if defined(USE_MPS)
  Symbol_sp ptrType = _sym_mpsGarbageCollection;
#else
  Symbol_sp ptrType = _sym_intrusiveReferenceCountedPointer;
#endif
  T_sp dummy;
  Fixnum_sp pxOffset = make_fixnum(0);
  Fixnum_sp pxSize = make_fixnum((gctools::Fixnum)gctools::pointer_size);
  return Values(ptrType, pxOffset, pxSize);
}

#define ARGS_cl_values "(&rest args)"
#define DECL_cl_values ""
#define DOCS_cl_values "values"
T_mv cl_values(List_sp args) {
  _G();
  // returns multiple values
  T_mv result = ValuesFromCons(args);
  return result;
}

#define ARGS_core_valuesTesting "(&rest args)"
#define DECL_core_valuesTesting ""
#define DOCS_core_valuesTesting "values"
T_mv core_valuesTesting(List_sp args) {
  _G();
  // returns multiple values
  T_mv result = ValuesFromCons(args);
  printf("%s:%d core_valuesTesting: %s\n", __FILE__, __LINE__, _rep_(args).c_str());
  return result;
}

#define ARGS_af_values_list "(list)"
#define DECL_af_values_list ""
#define DOCS_af_values_list "values_list"
T_mv af_values_list(List_sp list) {
  _G();
  return ValuesFromCons(list);
}

Symbol_sp functionBlockName(T_sp functionName) {
  _G();
  if (cl_symbolp(functionName))
    return gc::As<Symbol_sp>(functionName);
  if (cl_consp(functionName)) {
    List_sp cfn = functionName;
    if (oCar(cfn) == cl::_sym_setf && cl_symbolp(oCadr(cfn)) & oCadr(cfn).notnilp()) {
      return gc::As<Symbol_sp>(oCadr(cfn));
    }
  }
  return _Nil<Symbol_O>();
}

#define ARGS_af_functionBlockName "(functionName)"
#define DECL_af_functionBlockName ""
#define DOCS_af_functionBlockName "See CLHS glossary 'function block name'. If the functionName is a symbol return it.  If the functionName is a cons of the form (setf xxxx) return xxxx"
Symbol_mv af_functionBlockName(T_sp functionName) {
  _G();
  Symbol_sp output = functionBlockName(functionName);
  if (output.nilp()) {
    SIMPLE_ERROR(BF("Invalid function name: %s") % _rep_(functionName));
  }
  return (Values(output));
}

#define ARGS_af_validFunctionNameP "(arg)"
#define DECL_af_validFunctionNameP ""
#define DOCS_af_validFunctionNameP "validFunctionNameP"
T_mv af_validFunctionNameP(T_sp arg) {
  _G();
  T_sp name = functionBlockName(arg);
  if (name.nilp())
    return (Values(_Nil<T_O>()));
  return (Values(_lisp->_true()));
};

#define ARGS_af_makeStringOutputStream "(&key (elementType 'character))"
#define DECL_af_makeStringOutputStream ""
#define DOCS_af_makeStringOutputStream "makeStringOutputStream"
T_mv af_makeStringOutputStream(T_sp elementType) {
  _G();
  if (elementType != cl::_sym_character) {
    SIMPLE_ERROR(BF("Add support for non character string output streams - you asked for %s") % _rep_(elementType));
  }
  T_sp ss = clasp_make_string_output_stream();
  return (Values(ss));
};

#define ARGS_af_testMemoryError "()"
#define DECL_af_testMemoryError ""
#define DOCS_af_testMemoryError "testMemoryError"
void af_testMemoryError() {
  _G();
  int *h = (int *)malloc(sizeof(int));
  *h = 1;
  free(h);
  *h = 2;
};

#define ARGS_af_separatePairList "(listOfPairs)"
#define DECL_af_separatePairList ""
#define DOCS_af_separatePairList "Split a list of pairs into a pair of lists returned as MultipleValues. The first list is each first element and the second list is each second element or nil if there was no second element"
T_mv af_separatePairList(List_sp listOfPairs) {
  _G();
  ql::list firsts(_lisp);
  ql::list seconds(_lisp);
  for (auto cur : listOfPairs) {
    T_sp element = oCar(cur);
    if (cl_atom(element)) {
      firsts << element;
      seconds << _Nil<T_O>();
    } else if (cl_consp(element)) {
      List_sp pair = element;
      size_t pairlen = cl_length(pair);
      if (pairlen == 2 || pairlen == 1) {
        firsts << oCar(pair);
        seconds << oCadr(pair);
      } else {
        SIMPLE_ERROR(BF("Expected one or two element list got: %s") % _rep_(pair));
      }
    } else {
      SIMPLE_ERROR(BF("Expected single object or 2-element list - got: %s") % _rep_(element));
    }
  }
  T_sp tfirsts = firsts.cons();
  return (Values(tfirsts, seconds.cons()));
}

#if DEPRECIATED_C_FUNCTION
#define ARGS_af_c_function "(sym)"
#define DECL_af_c_function ""
#define DOCS_af_c_function "c_function"
Pointer_mv af_c_function(Symbol_sp sym) {
  _G();
  return (Values(_lisp->lookup_c_function_ptr(sym)));
};
#endif

#define ARGS_cl_macroFunction "(symbol &optional env)"
#define DECL_cl_macroFunction ""
#define DOCS_cl_macroFunction "See CLHS: macroFunction"
T_sp cl_macroFunction(Symbol_sp symbol, T_sp env) {
  _G();
  T_sp func = _Nil<T_O>();
  if (env.nilp()) {
    func = af_interpreter_lookup_macro(symbol, env);
  } else if (Environment_sp eenv = env.asOrNull<Environment_O>()) {
    func = af_interpreter_lookup_macro(symbol, eenv);
  } else {
    if (cleavirEnv::_sym_macroFunction->fboundp()) {
      func = eval::funcall(cleavirEnv::_sym_macroFunction, symbol, env);
    } else {
      printf("%s:%d Unexpected environment for MACRO-FUNCTION before Cleavir is available - using toplevel environment\n", __FILE__, __LINE__);
      func = af_interpreter_lookup_macro(symbol, _Nil<T_O>());
    }
  }
  return func;
}

#define ARGS_cl_specialOperatorP "(symbol)"
#define DECL_cl_specialOperatorP ""
#define DOCS_cl_specialOperatorP "See CLHS: special-operator-p"
T_mv cl_specialOperatorP(T_sp sym) {
  _G();
  SYMBOL_EXPORT_SC_(ClPkg, let);
  SYMBOL_EXPORT_SC_(ClPkg, letSTAR);
  SYMBOL_EXPORT_SC_(ClPkg, return_from);
  SYMBOL_EXPORT_SC_(ClPkg, catch);
  SYMBOL_EXPORT_SC_(ClPkg, load_time_value);
  SYMBOL_EXPORT_SC_(ClPkg, setq);
  SYMBOL_EXPORT_SC_(ClPkg, eval_when);
  SYMBOL_EXPORT_SC_(ClPkg, locally);
  SYMBOL_EXPORT_SC_(ClPkg, symbol_macrolet);
  SYMBOL_EXPORT_SC_(ClPkg, flet);
  SYMBOL_EXPORT_SC_(ClPkg, macrolet);
  SYMBOL_EXPORT_SC_(ClPkg, tagbody);
  SYMBOL_EXPORT_SC_(ClPkg, multiple_value_call);
  SYMBOL_EXPORT_SC_(ClPkg, the);
  SYMBOL_EXPORT_SC_(ClPkg, go);
  SYMBOL_EXPORT_SC_(ClPkg, multiple_value_prog1);
  SYMBOL_EXPORT_SC_(ClPkg, if);
  SYMBOL_EXPORT_SC_(ClPkg, unwind_protect);
  SYMBOL_EXPORT_SC_(ClPkg, labels);
  SYMBOL_EXPORT_SC_(ClPkg, progv);
  if ((sym == cl::_sym_block) ||
      (sym == cl::_sym_let) ||
      (sym == cl::_sym_letSTAR) ||
      (sym == cl::_sym_return_from) ||
      (sym == cl::_sym_load_time_value) ||
      (sym == cl::_sym_setq) ||
      (sym == cl::_sym_eval_when) ||
      (sym == cl::_sym_locally) ||
      (sym == cl::_sym_symbol_macrolet) ||
      (sym == cl::_sym_flet) ||
      (sym == cl::_sym_macrolet) ||
      (sym == cl::_sym_tagbody) ||
      (sym == cl::_sym_function) ||
      (sym == cl::_sym_multiple_value_call) ||
      (sym == cl::_sym_the) ||
      (sym == cl::_sym_go) ||
      (sym == cl::_sym_multiple_value_prog1) ||
      (sym == cl::_sym_if) ||
      (sym == cl::_sym_progn) ||
      (sym == cl::_sym_labels) ||
      (sym == cl::_sym_unwind_protect) ||
      (sym == cl::_sym_catch) ||
      (sym == cl::_sym_throw) ||
      (sym == cl::_sym_progv) ||
      (sym == cl::_sym_quote)) {
    return (Values(_lisp->_true()));
  }
  return (Values(_Nil<T_O>()));
};

#if 0
#define ARGS_core_treatAsSpecialOperatorP "(symbol)"
#define DECL_core_treatAsSpecialOperatorP ""
#define DOCS_core_treatAsSpecialOperatorP "See CLHS: special-operator-p"
T_sp core_treatAsSpecialOperatorP(T_sp sym) {
  _G();
  SYMBOL_EXPORT_SC_(CorePkg, debug_message);
  if (sym == cl::_sym_unwind_protect)
    return _Nil<T_O>(); // All handled in macros
  if (sym == core::_sym_debug_message)
    return _lisp->_true();
  return cl_specialOperatorP(sym);
};
#endif

#define ARGS_cl_ash "(integer count)"
#define DECL_cl_ash ""
#define DOCS_cl_ash "CLHS: ash"
Integer_sp cl_ash(Integer_sp integer, Integer_sp count) {
  int cnt = clasp_to_int(count);
  return clasp_shift(integer, cnt);
}

#define ARGS_af_break "(&optional fmt-control &rest args)"
#define DECL_af_break ""
#define DOCS_af_break "Built in implementation of break - that calls the internal debugger - replace this with a CL implemented version"
void af_break(T_sp fmt, List_sp args) {
  _G();
  if (fmt.notnilp()) {
    af_format(_lisp->_true(), gc::As<Str_sp>(fmt), args);
  }
  dbg_hook("built in break");
  af_invokeInternalDebugger(_Nil<core::T_O>());
};

#define ARGS_af_gdb "(&optional msg)"
#define DECL_af_gdb ""
#define DOCS_af_gdb "hook to invoke gdb"
void af_gdb(T_sp msg) {
  _G();
  T_sp obj = msg;
  string smsg = "No msg";
  if (obj.notnilp()) {
    smsg = _rep_(obj);
  }
  dbg_hook(smsg.c_str());
  af_invokeInternalDebugger(_Nil<core::T_O>());
};

#define ARGS_core_cxx_lambda_list_handler_create_bindings_calls "()"
#define DECL_core_cxx_lambda_list_handler_create_bindings_calls ""
#define DOCS_core_cxx_lambda_list_handler_create_bindings_calls "Return the number of times lambdaListHandler_createBindings"
Integer_sp core_cxx_lambda_list_handler_create_bindings_calls() {
  size_t calls = threadLocalInfoPtr->_lambda_list_handler_create_bindings_count;
  return Integer_O::create((Fixnum)calls);
};

#define ARGS_core_trapExecution "(&optional msg)"
#define DECL_core_trapExecution ""
#define DOCS_core_trapExecution "hook to invoke gdb"
void core_trapExecution(T_sp msg) {
  _G();
  T_sp obj = msg;
  string smsg = "No msg";
  if (obj.notnilp()) {
    smsg = _rep_(obj);
  }
  printf("%s:%d In core_trapExecution: %s \n", __FILE__, __LINE__, smsg.c_str());
  fflush(stdout);
};

#define ARGS_af_gdbInspect "(msg o)"
#define DECL_af_gdbInspect ""
#define DOCS_af_gdbInspect "hook to invoke gdb"
void af_gdbInspect(Str_sp msg, T_sp o) {
  _G();
  printf("gdbInspect object: %s\n", _rep_(o).c_str());
  dbg_hook(msg->get().c_str());
  af_invokeInternalDebugger(_Nil<core::T_O>());
};

#define ARGS_af_constantp "(obj &optional env)"
#define DECL_af_constantp ""
#define DOCS_af_constantp "constantp"
bool af_constantp(T_sp obj, T_sp env) {
  _G();
  // ignore env
  if (cl_numberp(obj))
    return true;
  if (af_characterP(obj))
    return true;
  if (af_arrayP(obj))
    return true;
  // TODO add various kinds of array
  if (cl_consp(obj) && oCar(obj) == cl::_sym_quote)
    return true;
  if (obj.nilp())
    return true;
  if (cl_symbolp(obj)) {
    if (af_keywordP(obj))
      return true;
    return gc::As<Symbol_sp>(obj)->isConstant();
  }
  return false;
};

#define ARGS_af_identity "(arg)"
#define DECL_af_identity ""
#define DOCS_af_identity "identity"
T_mv af_identity(T_sp arg) {
  _G();
  return (Values(arg));
};

#define ARGS_af_macroexpand_default "(macro_function form macro_env)"
#define DECL_af_macroexpand_default ""
#define DOCS_af_macroexpand_default "macroexpand_default Default value of *macroexpand-hook*"
T_mv af_macroexpand_default(Function_sp macro_function, T_sp form, T_sp macro_env) {
  _G();
  Function_sp debugMacroFunction = macro_function;
  T_sp debugForm = form;
  T_sp debugEnvironment = macro_env;
  T_sp tllh = macro_function->functionLambdaListHandler();
  if (LambdaListHandler_sp llh = tllh.asOrNull<LambdaListHandler_O>()) {
    if (llh->numberOfRequiredArguments() != 2) {
      stringstream err;
      err << __FILE__ << ":" << __LINE__ << " Caught a problem in af_macroexpand_default - the macro_function requires " << llh->numberOfRequiredArguments() << " arguments but I'm only going to pass 2!!!" << std::endl;
      err << "lambda_list: " << _rep_(llh) << std::endl;
      err << "Passing argument 1: " << _rep_(form) << std::endl;
      err << "Passing argument 2: " << _rep_(macro_env) << std::endl;
      gctools::tagged_pointer<Closure> closure = macro_function->closure;
      err << "macro_function[" << _rep_(macro_function->functionName()) << std::endl;
      if (auto ic = closure.as<InterpretedClosure>()) {
        err << "code: " << _rep_(ic->code());
      } else {
        err << "macro_function is not an interpreted function";
      }
      SIMPLE_ERROR(BF("Wrong number of arguments %d within macroexpand_default when trying to invoke macro %s\nMore detail: %s") % llh->numberOfRequiredArguments() % _rep_(macro_function->functionName()) % err.str());
    }
  }
  T_sp result = eval::funcall(macro_function, form, macro_env);
  return (Values(result));
};

#define ARGS_af_null "(obj)"
#define DECL_af_null ""
#define DOCS_af_null "null test - return true if the object is the empty list otherwise return nil"
T_mv af_null(T_sp obj) {
  _G();
  if (obj.nilp())
    return (Values(_lisp->_true()));
  return (Values(_Nil<T_O>()));
};

#define ARGS_af_classOf "(obj)"
#define DECL_af_classOf ""
#define DOCS_af_classOf "return class of object - see CLHS"
Class_sp af_classOf(T_sp obj) {
  _G();
  Class_sp result = lisp_instance_class(obj);
#if DEBUG_CLOS >= 3
  printf("\nMLOG classOf %s ---> %s\n", obj->__repr__().c_str(), result->__repr__().c_str());
#endif
  return (result);
}

#define ARGS_af_STARfset "(function-name fn &optional macro)"
#define DECL_af_STARfset ""
#define DOCS_af_STARfset "fset - bind a function to its name - handles symbol function-name and (SETF XXXX) names. (macro) defines if the function is a macro or not."
T_sp af_STARfset(T_sp functionName, Function_sp functionObject, T_sp macro) {
  ASSERTF(functionObject, BF("function is undefined\n"));
  if (macro.isTrue()) {
    functionObject->setKind(kw::_sym_macro);
  } else {
    functionObject->setKind(kw::_sym_function);
  }
  if (comp::_sym_STARall_functions_for_one_compileSTAR->boundP()) {
    functionObject->closure->setAssociatedFunctions(comp::_sym_STARall_functions_for_one_compileSTAR->symbolValue());
  }
  if (cl_symbolp(functionName)) {
    Symbol_sp symbol = gc::As<Symbol_sp>(functionName);
    symbol->setf_symbolFunction(functionObject);
    return functionObject;
  } else if (cl_consp(functionName)) {
    SYMBOL_EXPORT_SC_(ClPkg, setf);
    List_sp cur = functionName;
    if (oCar(cur) == cl::_sym_setf) {
      Symbol_sp symbol = gc::As<Symbol_sp>(oCadr(cur));
      symbol->setSetfFdefinition(functionObject);
      return functionObject;
    }
  }
  SIMPLE_ERROR(BF("Illegal name for function[%s]") % _rep_(functionName));
};

#define ARGS_af_fdefinition "(function-name)"
#define DECL_af_fdefinition ""
#define DOCS_af_fdefinition "fdefinition"
T_sp af_fdefinition(T_sp functionName) {
  if (cl_symbolp(functionName)) {
    Symbol_sp sym = gc::As<Symbol_sp>(functionName);
    return sym->symbolFunction();
  } else if (cl_consp(functionName)) {
    List_sp cname = functionName;
    if (oCar(cname) == cl::_sym_setf) {
      Symbol_sp name = gc::As<Symbol_sp>(oCadr(cname));
      if (name.notnilp()) {
        return name->getSetfFdefinition();
      }
    }
  }
  SIMPLE_ERROR(BF("Illegal function-name[%s]") % _rep_(functionName));
}

#define ARGS_af_fboundp "(function-name)"
#define DECL_af_fboundp ""
#define DOCS_af_fboundp "fboundp"
bool af_fboundp(T_sp functionName) {
  _G();
  if (functionName.nilp()) {
    return false;
  }
  if (cl_symbolp(functionName)) {
    Symbol_sp sym = gc::As<Symbol_sp>(functionName);
    return sym->fboundp();
  } else if (cl_consp(functionName)) {
    List_sp cname = functionName;
    if (oCar(cname) == cl::_sym_setf) {
      Symbol_sp name = gc::As<Symbol_sp>(oCadr(cname));
      if (name.notnilp())
        return name->setf_fboundp();
    }
  }
  SIMPLE_ERROR(BF("Illegal function-name[%s]") % _rep_(functionName));
}

#define ARGS_af_fmakunbound "(function-name)"
#define DECL_af_fmakunbound ""
#define DOCS_af_fmakunbound "fmakunbound"
T_mv af_fmakunbound(T_sp functionName) {
  _G();
  if (cl_symbolp(functionName)) {
    Symbol_sp sym = gc::As<Symbol_sp>(functionName);
    sym->setf_symbolFunction(_Unbound<Function_O>());
    return (Values(sym));
  } else if (cl_consp(functionName)) {
    List_sp cname = functionName;
    if (oCar(cname) == cl::_sym_setf) {
      Symbol_sp name = gc::As<Symbol_sp>(oCadr(cname));
      if (name.notnilp()) {
        name->resetSetfFdefinition(); //_lisp->remove_setfDefinition(name);
        return (Values(functionName));
      }
    }
  }
  SIMPLE_ERROR(BF("Illegal function-name[%s]") % _rep_(functionName));
}

#define ARGS_af_read_delimited_list "(char &optional input-stream-designator recursive-p)"
#define DECL_af_read_delimited_list ""
#define DOCS_af_read_delimited_list "read a list up to a specific character - see CLHS"
T_mv af_read_delimited_list(Character_sp chr, T_sp input_stream_designator, T_sp recursive_p) {
  _G();
  T_sp sin = coerce::inputStreamDesignator(input_stream_designator);
#if 0
	// I think it is safe to ignore recursive_p
  if ( recursive_p.isTrue() )
  {
    SIMPLE_ERROR(BF("Currently I don't handle recursive-p[true] for read_delimited_list"));
  }
#endif
  T_sp result = read_list(sin, clasp_as_char(chr), true);
  if (cl::_sym_STARread_suppressSTAR->symbolValue().isTrue()) {
    return (Values(_Nil<T_O>()));
  }
  return (Values(result));
}

#define ARGS_cl_read "(&optional input-stream-designator (eof-error-p t) eof-value recursive-p)"
#define DECL_cl_read ""
#define DOCS_cl_read "read an object from a stream - see CLHS"
T_sp cl_read(T_sp input_stream_designator, T_sp eof_error_p, T_sp eof_value, T_sp recursive_p) {
  bool preserve_whitespace = true;
  if ( recursive_p.isTrue() ) {
    preserve_whitespace = _sym_STARpreserve_whitespace_pSTAR->symbolValue().isTrue();
  } else {
    preserve_whitespace = false;
  }
  DynamicScopeManager scope(_sym_STARpreserve_whitespace_pSTAR, _lisp->_boolean(preserve_whitespace));
  T_sp sin = coerce::inputStreamDesignator(input_stream_designator);
  return read_lisp_object(sin, eof_error_p.isTrue(), eof_value, recursive_p.notnilp());
}

#define ARGS_cl_read_preserving_whitespace "(&optional input-stream-designator (eof-error-p t) eof-value recursive-p)"
#define DECL_cl_read_preserving_whitespace ""
#define DOCS_cl_read_preserving_whitespace "read an object from a stream while preserving whitespace - see CLHS"
T_sp cl_read_preserving_whitespace(T_sp input_stream_designator, T_sp eof_error_p, T_sp eof_value, T_sp recursive_p) {
  bool preserve_whitespace = true;
  if ( recursive_p.isTrue() ) {
    preserve_whitespace = _sym_STARpreserve_whitespace_pSTAR->symbolValue().isTrue();
  } else {
    preserve_whitespace = true;
  }
  DynamicScopeManager scope(_sym_STARpreserve_whitespace_pSTAR, _lisp->_boolean(preserve_whitespace));
  T_sp sin = coerce::inputStreamDesignator(input_stream_designator);
  return read_lisp_object(sin, eof_error_p.isTrue(), eof_value, recursive_p.isTrue());
}

/* -------------------------------------------------------- */
/*     Sequence primitives                                  */

#if 0
GC_RESULT VectorStepper::onHeapScanGCRoots(GC_SCAN_ARGS_PROTOTYPE)
{
#ifdef USE_MPS
  MPS_SCAN_BEGIN(GC_SCAN_STATE) {
    SMART_PTR_FIX(this->_Domain);
  } MPS_SCAN_END(GC_SCAN_STATE);
#endif
  return GC_RES_OK;
}
#endif

#if 0
GC_RESULT ConsStepper::onHeapScanGCRoots(GC_SCAN_ARGS_PROTOTYPE)
{
#ifdef USE_MPS
  MPS_SCAN_BEGIN(GC_SCAN_STATE) {
    SMART_PTR_FIX(this->_Cur);
  } MPS_SCAN_END(GC_SCAN_STATE);
#endif
  return GC_RES_OK;
}
#endif

ListOfSequenceSteppers::ListOfSequenceSteppers(List_sp sequences) {
  _G();
  this->_AtEnd = false;
  for (auto cur : sequences) {
    T_sp obj = oCar(cur);
    if (Vector_sp vobj = obj.asOrNull<Vector_O>()) {
      if (cl_length(vobj) == 0)
        goto EMPTY;
      gctools::tagged_pointer<VectorStepper> vP(gctools::ClassAllocator<VectorStepper>::allocateClass(vobj));
      this->_Steppers.push_back(vP);
    } else if (Cons_sp cobj = obj.asOrNull<Cons_O>()) {
      gctools::tagged_pointer<ConsStepper> cP(gctools::ClassAllocator<ConsStepper>::allocateClass(cobj));
      this->_Steppers.push_back(cP);
    } else if (obj.nilp()) {
      goto EMPTY;
    } else {
      SIMPLE_ERROR(BF("Illegal object for stepper[%s] class[%s]") % _rep_(obj) % obj->_instanceClass()->classNameAsString());
    }
  }
  this->_AtEnd = false;
  return;
 EMPTY:
  this->_AtEnd = true;
}

void ListOfSequenceSteppers::fillValueFrameUsingCurrentSteppers(ActivationFrame_sp frame) const {
  _G();
  if (this->_AtEnd)
    SIMPLE_ERROR(BF("Tried to make list of ended stepper"));
  int idx = 0;
  for (auto rit = this->_Steppers.begin(); rit != this->_Steppers.end(); rit++) {
    frame->set_entry(idx, (*rit)->element());
    ++idx;
  }
}

bool ListOfSequenceSteppers::advanceSteppers() {
  _OF();
  if (this->_AtEnd)
    SIMPLE_ERROR(BF("Tried to advance ended stepper"));
  for (auto it = this->_Steppers.begin(); it != this->_Steppers.end(); it++) {
    this->_AtEnd |= (*it)->advance();
  }
  return !this->_AtEnd;
}

class ListOfListSteppers : public ListOfSequenceSteppers {
public:
  ListOfListSteppers(List_sp lists);
  virtual ~ListOfListSteppers(){};
};

ListOfListSteppers::ListOfListSteppers(List_sp sequences) {
  _G();
  for (auto cur : sequences) {
    T_sp obj = oCar(cur);
    if (Cons_sp cobj = obj.asOrNull<Cons_O>()) {
      gctools::tagged_pointer<ConsStepper> cP(gctools::ClassAllocator<ConsStepper>::allocateClass(cobj));
      this->_Steppers.push_back(cP);
    } else {
      goto EMPTY;
    }
  }
  this->_AtEnd = false;
  return;
 EMPTY:
  this->_AtEnd = true;
  return;
}

bool test_every_some_notevery_notany(Function_sp predicate, List_sp sequences, bool elementTest, bool elementReturn, bool fallThroughReturn, T_sp &retVal) {
  _G();
  ListOfSequenceSteppers steppers(sequences);
  ValueFrame_sp frame(ValueFrame_O::create(steppers.size(), _Nil<ActivationFrame_O>()));
  if (steppers.atEnd())
    goto FALLTHROUGH; // return elementReturn;
  while (!steppers.atEnd()) {
    steppers.fillValueFrameUsingCurrentSteppers(frame);
    LOG(BF("Applying predicate to elements[%s]") % frame->asString());
    retVal = eval::applyToActivationFrame(predicate, frame);
    bool test = retVal.isTrue();
    if (test == elementTest) {
      LOG(BF("element test was %d - returning %d") % elementTest % elementReturn);
      return elementReturn;
    }
    steppers.advanceSteppers();
  }
  LOG(BF("passed-through - returning %d") % fallThroughReturn);
 FALLTHROUGH:
  return fallThroughReturn;
}

#define ARGS_af_every "(predicate &rest sequences)"
#define DECL_af_every ""
#define DOCS_af_every "See CLHS for every"
T_sp af_every(T_sp predicate, List_sp sequences) {
  _G();
  Function_sp op = coerce::functionDesignator(predicate);
  T_sp dummy;
  bool result = test_every_some_notevery_notany(op, sequences, false, false, true, dummy);
  return _lisp->_boolean(result);
}

#define ARGS_af_some "(predicate &rest sequences)"
#define DECL_af_some ""
#define DOCS_af_some "See CLHS for some"
T_sp af_some(T_sp predicate, List_sp sequences) {
  _G();
  Function_sp op = coerce::functionDesignator(predicate);
  T_sp retVal;
  bool result = test_every_some_notevery_notany(op, sequences, true, true, false, retVal);
  if (result)
    return retVal;
  return _Nil<T_O>();
}

#define ARGS_af_notany "(predicate &rest sequences)"
#define DECL_af_notany ""
#define DOCS_af_notany "See CLHS for notany"
T_sp af_notany(T_sp predicate, List_sp sequences) {
  _G();
  Function_sp op = coerce::functionDesignator(predicate);
  T_sp dummy;
  bool result = test_every_some_notevery_notany(op, sequences, true, false, true, dummy);
  return _lisp->_boolean(result);
}

#define ARGS_af_notevery "(predicate &rest sequences)"
#define DECL_af_notevery ""
#define DOCS_af_notevery "See CLHS for notevery"
T_sp af_notevery(T_sp predicate, List_sp sequences) {
  _G();
  Function_sp op = coerce::functionDesignator(predicate);
  T_sp dummy;
  bool result = test_every_some_notevery_notany(op, sequences, false, true, false, dummy);
  return _lisp->_boolean(result);
}

/*
  __BEGIN_DOC(candoScript.general.mapcar)
  __END_DOC
*/
#define ARGS_cl_mapcar "(func_desig &rest lists)"
#define DECL_cl_mapcar ""
#define DOCS_cl_mapcar "See CLHS for mapcar"
SYMBOL_EXPORT_SC_(ClPkg, mapcar);
T_sp cl_mapcar(T_sp func_desig, List_sp lists) {
  _G();
  Function_sp func = coerce::functionDesignator(func_desig);
  ListOfListSteppers steppers(lists);
  ValueFrame_sp frame(ValueFrame_O::create(steppers.size(), _Nil<ActivationFrame_O>()));
  ql::list result(_lisp);
  while (!steppers.atEnd()) {
    steppers.fillValueFrameUsingCurrentSteppers(frame);
    T_sp res = eval::applyToActivationFrame(func, frame);
    result << res;
    steppers.advanceSteppers();
  }
  return result.cons();
}

/*
  __BEGIN_DOC(candoScript.general.mapcar)
  __END_DOC
*/
#define ARGS_cl_mapc "(op &rest lists)"
#define DECL_cl_mapc ""
#define DOCS_cl_mapc "See CLHS mapc"
T_sp cl_mapc(T_sp top, List_sp lists) {
  _G();
  Function_sp op = coerce::functionDesignator(top);
  VectorObjectsWithFillPtr_sp argumentLists(VectorObjectsWithFillPtr_O::make(_Nil<T_O>(), _Nil<T_O>(), 8, 0, true, cl::_sym_T_O));
  // Copy the arguments into argumentLists
  for (auto carg : lists) {
    argumentLists->vectorPushExtend(oCar(carg), 8);
  }
  List_sp result, curResult;
  ValueFrame_sp frame(ValueFrame_O::create(cl_length(argumentLists), _Nil<ActivationFrame_O>()));
  while (1) {
    int idx = 0;
    for (size_t it(0), itEnd(cl_length(argumentLists)); it < itEnd; ++it) {
      if (argumentLists->operator[](it).nilp()) {
        // We hit a nil - jump to the end
        goto RETURN;
      }
      frame->set_entry(idx, oCar(argumentLists->operator[](it)));
      argumentLists->operator[](it) = oCdr(argumentLists->operator[](it));
      ++idx;
    }
    LOG(BF("About to evaluate map op[%s] on arguments[%s]") % _rep_(op) % _rep_(frame));
    T_sp res = eval::applyToActivationFrame(op, frame);
  }
 RETURN:
  return oCar(lists);
}

#define ARGS_cl_maplist "(func_desig &rest lists)"
#define DECL_cl_maplist ""
#define DOCS_cl_maplist "See CLHS maplist"
T_sp cl_maplist(T_sp func_desig, List_sp lists) {
  _G();
  //        printf("%s:%d maplist func_desig=%s   lists=%s\n", __FILE__, __LINE__, _rep_(func_desig).c_str(), _rep_(lists).c_str() );
  Function_sp op = coerce::functionDesignator(func_desig);
  VectorObjectsWithFillPtr_sp argumentLists(VectorObjectsWithFillPtr_O::make(_Nil<T_O>(), _Nil<T_O>(), 16, 0, true, cl::_sym_T_O));
  //	vector<List_sp> argumentLists;
  // Copy the arguments into argumentLists
  for (auto carg : lists) {
    argumentLists->vectorPushExtend(oCar(carg), 8);
    //	    argumentLists.push_back(oCar(carg));
  }
  //        printf("%s:%d  argumentLists = %s\n", __FILE__, __LINE__, _rep_(argumentLists).c_str() );
  List_sp result, curResult;
  result = Cons_O::create(_Nil<T_O>(), _Nil<T_O>());
  ValueFrame_sp frame(ValueFrame_O::create(cl_length(argumentLists), _Nil<ActivationFrame_O>()));
  curResult = result;
  while (1) {
    int idx = 0;
    //            printf("%s:%d  length(argumentLists) = %d   argumentLists->fillPointer()=%d\n", __FILE__, __LINE__, cl_length(argumentLists), argumentLists->fillPointer() );
    for (int it(0), itEnd(cl_length(argumentLists)); it < itEnd; ++it) {
      T_sp val = (*argumentLists)[it];
      if (val.nilp())
        goto RETURN; // hit nil in arguments - exit
      frame->set_entry(idx, val);
      idx++;
    }
    //            printf("%s:%d op %s on frame: %s\n", __FILE__, __LINE__, _rep_(op).c_str(), _rep_(frame).c_str() );
    LOG(BF("About to evaluate map op[%s] on arguments[%s]") % _rep_(op) % _rep_(frame));
    T_sp res = eval::applyToActivationFrame(op, frame);
    Cons_sp one = Cons_O::create(res);
    curResult.asCons()->setCdr(one);
    curResult = one;
    // Advance to the next element
    for (int it(0), itEnd(cl_length(argumentLists)); it < itEnd; ++it) {
      argumentLists->operator[](it) = oCdr(argumentLists->operator[](it));
      //		*it = cCdr((*it));
    }
  }
 RETURN:
  return oCdr(result);
}

#define ARGS_cl_mapl "(op &rest lists)"
#define DECL_cl_mapl ""
#define DOCS_cl_mapl "See CLHS maplist"
T_sp cl_mapl(T_sp top, List_sp lists) {
  _G();
  Function_sp op = coerce::functionDesignator(top);
  cl_maplist(op, lists);
  return oCar(lists);
}

#define ARGS_af_mapappend "(fun &rest cargs)"
#define DECL_af_mapappend ""
#define DOCS_af_mapappend "mapappend is like mapcar except that the results are appended together - see AMOP 280"
T_mv af_mapappend(Function_sp fun, List_sp cargs) {
  _G();
  IMPLEMENT_MEF(BF("Fix me - I think I'm broken"));
  T_sp testNull = eval::funcall(cl::_sym_some, cl::_sym_null->symbolFunction(), cargs);
  if (testNull.nilp())
    return (Values(_Nil<T_O>()));
  T_sp arg0 = eval::funcall(cl::_sym_mapcar, cl::_sym_car->symbolFunction(), cargs);
  T_sp appendHead = eval::funcall(fun, arg0);
  T_sp arg2 = eval::funcall(cl::_sym_mapcar, cl::_sym_cdr->symbolFunction(), cargs);
  T_sp appendTail = eval::funcall(_sym_mapappend, fun, arg2);
  return eval::funcall(cl::_sym_append, appendHead, appendTail);
};

#define ARGS_cl_mapcon "(op &rest lists)"
#define DECL_cl_mapcon ""
#define DOCS_cl_mapcon "mapcon"
T_mv cl_mapcon(T_sp op, List_sp lists) {
  _G();
  List_sp parts = cl_maplist(op, lists);
  T_sp result = cl_nconc(parts);
  return (Values(result));
};

#define ARGS_cl_mapcan "(op &rest lists)"
#define DECL_cl_mapcan ""
#define DOCS_cl_mapcan "mapcan"
T_mv cl_mapcan(T_sp op, List_sp lists) {
  _G();
  List_sp parts = cl_mapcar(op, lists);
  T_sp result = cl_nconc(parts);
#if 0
  ValueFrame_sp frame(ValueFrame_O::create(parts,_Nil<ActivationFrame_O>()));
  T_sp result = eval::applyToActivationFrame(cl::_sym_nconc,frame);
#endif
  return (Values(result));
};

#define ARGS_macro_backquote "(form env)"
#define DECL_macro_backquote ""
#define DOCS_macro_backquote "backquote"
T_mv macro_backquote(List_sp form, T_sp env) {
  _G();
  T_sp arg = oCadr(form);
  LOG(BF("Expanding backquote going in: %s") % _rep_(arg));
  T_mv result = af_backquote_completely_process(arg);
  LOG(BF("Expanded backquote result: %s") % _rep_(result));
  return (result);
}

/*!
  Equivalent to Common Lisps append function
  (append a b c)
  It recreates the list structures of the first arguments a and b and strings
  them together into one list and then points the cdr of the last element of this new list
  to c.
*/
#define ARGS_af_append "(&rest lists)"
#define DECL_af_append ""
#define DOCS_af_append "append as in clhs"
List_sp af_append(List_sp lists) {
  _G();
  ql::list list;
  LOG(BF("Carrying out append with arguments: %s") % _rep_(lists));
  auto it = lists.begin();
  auto end = lists.end();
  T_sp curit = *it;
  while (it != end) {
    curit = *it;
    it++;
    if (it == end)
      break;
    for (auto inner : (List_sp)oCar(curit)) {
      list << oCar(inner);
    }
  }
  /* Now append the last argument by setting the new lists last element cdr
       to the last argument of append */
  T_sp last = oCar(curit);
  list.dot(last);
  T_sp res = list.cons();
  return res;
}

#define ARGS_af_sequence_start_end "(func sequence start end)"
#define DECL_af_sequence_start_end ""
#define DOCS_af_sequence_start_end "Copied from ecl::sequence.d::sequence_start_end - throws errors if start/end are out of range for the sequence." \
  " I'm not sure what the func argument is for. If end is nil then it is set to the end of the sequence.  Return MultipleValues(start,end,length)."
T_mv af_sequence_start_end(T_sp func, T_sp sequence, Fixnum_sp start, T_sp end) {
  _G();
  uint len = cl_length(sequence);
  if (end.nilp())
    end = make_fixnum(len);
  Fixnum_sp fnend = gc::As<Fixnum_sp>(end);
  if (unbox_fixnum(start) < 0) {
    SIMPLE_ERROR(BF("start[%d] must be greater than zero") % _rep_(start));
  }
  if (unbox_fixnum(fnend) > len) {
    SIMPLE_ERROR(BF("end[%d] must be <= length of sequence[%d]") % _rep_(end) % len);
  }
  Fixnum_sp length = make_fixnum(len);
  if (unbox_fixnum(fnend) < unbox_fixnum(start)) {
    SIMPLE_ERROR(BF("end[%d] is less than start[%d]") % _rep_(end) % _rep_(start));
  }
  return (Values(start, fnend, length));
};

#if 0
#define ARGS_af_open "(filespec_desig &key (direction :input) element-type if-exists if-does-not-exist (external-format :default))"
#define DECL_af_open ""
#define DOCS_af_open ""
Stream_mv af_open(T_sp filespec_desig, Symbol_sp direction, T_sp element_type, T_sp if_exists, T_sp if_does_not_exist, T_sp external_format )
{_G();
  LOG(BF("filespec_desig = %s") % _rep_(filespec_desig) );
  LOG(BF("direction[%s]") % _rep_(direction) );
  LOG(BF("if_exists[%s]") % _rep_(if_exists));
  LOG(BF("if_does_not_exist[%s]") % _rep_(if_does_not_exist));
  LOG(BF("external_format[%s]") % _rep_(external_format));
  Pathname_sp filespec = cl_pathname(filespec_desig);
  if ( direction == kw::_sym_input )
  {
    LOG(BF("status"));
    if ( af_file_kind(filespec) != kw::_sym_file )
    {
      FILE_ERROR(filespec);
    }
    if ( external_format == kw::_sym_default )
    {
      LOG(BF("status"));
      FDInStream_sp fin = FDInStream_O::create(filespec);
      return(Values(fin));
      SYMBOL_SC_(KeywordPkg,gzip);
    }
    SIMPLE_ERROR(BF("For file[%s] Bad external-format option: %s") % filespec % _rep_(external_format) );
  } else if ( direction == kw::_sym_output )
  {
    LOG(BF("direction was :output"));
    if ( cl_probe_file(filespec).notnilp() )
    {
      Str_sp truename = cl_namestring(cl_truename(filespec));
      LOG(BF("The file[%s] already exists")% truename->get() );
      SYMBOL_SC_(KeywordPkg,supersede);
      if ( if_exists.nilp() || if_exists == kw::_sym_supersede)
      {
        LOG(BF("supersede"));
		// First write output to a temporary file and then rename it to the original on close
        Pathname_sp temporaryFileSpec = cl_make_pathname(_Nil<T_O>(), // host
                                                        false, // hostp
                                                        _Nil<T_O>(), // device
                                                        false, // devicep
                                                        _Nil<T_O>(), // directory
                                                        false, // directoryp
                                                        _Nil<T_O>(), // name
                                                        false, // namep
                                                        Str_O::create(filespec->_Type.as<Str_O>()->get()+"Temp"), //type
                                                        true, // typep
                                                        _Nil<T_O>(), // version
                                                        false, // versionp
                                                        kw::_sym_local, // scase
                                                        filespec // defaults
                                                        );
        if ( external_format == kw::_sym_default)
        {
          LOG(BF("external_format is :default"));
          FDOutStream_sp fout = FDOutStream_O::createTemporary(temporaryFileSpec,filespec,std::ios_base::out|std::ios_base::trunc);
          return(Values(fout));
        }
        SYMBOL_SC_(KeywordPkg,error);
      } else if ( if_exists == kw::_sym_error )
      {
        FILE_ERROR(filespec);
        SYMBOL_SC_(KeywordPkg,append);
      } else if ( if_exists == kw::_sym_append )
      {
        LOG(BF("if_exists is :append"));
        if ( external_format != kw::_sym_default)
        {
          SIMPLE_ERROR(BF("You cannot append to a file of external_format[%s]") % _rep_(external_format) );
        }
        LOG(BF("Setting up FDOutStream with append"));
        FDOutStream_sp fout = FDOutStream_O::create(filespec,std::ios_base::ate|std::ios_base::app);
        return(Values(fout));
      }
      SIMPLE_ERROR(BF("unknown option[%s] for if-exists") % _rep_(if_exists) );
    } else
    {
      LOG(BF("File does not exist"));
      SYMBOL_SC_(KeywordPkg,create);
      if ( if_does_not_exist.nilp() || if_does_not_exist == kw::_sym_create )
      {
        LOG(BF("if_does_not_exist is :create"));
        if ( external_format == kw::_sym_default)
        {
          LOG(BF("external_format is :default"));
          FDOutStream_sp fout = FDOutStream_O::create(filespec,std::ios_base::out|std::ios_base::trunc);
          return(Values(fout));
#if 0
        } else if ( external_format == kw::_sym_gzip )
        {
          LOG(BF("external_format is :gzip"));
          FileOutCompressedStream_sp fout = FileOutCompressedStream_O::createGzip(filespec);
          return(Values(fout));
#endif
        }
      } else if ( if_does_not_exist == kw::_sym_error )
      {
        FILE_ERROR(filespec);
      }
      LOG(BF("falling through to unimplemented"));
      IMPLEMENT_ME();
    }
  }
  LOG(BF("status"));
  IMPLEMENT_ME();
}
#endif

#define ARGS_af_gensym "(&optional x)"
#define DECL_af_gensym ""
#define DOCS_af_gensym "See CLHS gensym"
Symbol_mv af_gensym(T_sp x) {
  _G();
  stringstream ss;
  if (x.nilp()) {
    int counter = unbox_fixnum(gc::As<Fixnum_sp>(cl::_sym_STARgensym_counterSTAR->symbolValue()));
    cl::_sym_STARgensym_counterSTAR->setf_symbolValue(make_fixnum(counter + 1));
    ss << "G";
    ss << counter;
  } else if (af_stringP(x)) {
    int counter = unbox_fixnum(gc::As<Fixnum_sp>(cl::_sym_STARgensym_counterSTAR->symbolValue()));
    cl::_sym_STARgensym_counterSTAR->setf_symbolValue(make_fixnum(counter + 1));
    ss << gc::As<Str_sp>(x)->get();
    ss << counter;
  } else if (af_integerP(x)) {
    int counter = clasp_to_int(gc::As<Integer_sp>(x));
    ASSERTF(counter >= 0, BF("gensym argument %d must be >= 0") % counter);
    ss << "G";
    ss << counter;
  } else {
    SIMPLE_ERROR(BF("Illegal argument for gensym[%s]") % _rep_(x));
  }
  Symbol_sp sym = Symbol_O::create(ss.str());
  sym->setPackage(_Nil<Package_O>());
  return (Values(sym));
}

#define ARGS_af_type_to_symbol "(x)"
#define DECL_af_type_to_symbol ""
#define DOCS_af_type_to_symbol "type_to_symbol"
Symbol_mv af_type_to_symbol(T_sp x) {
  _G();
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-variable"
  if (x.fixnump())
    return (Values(cl::_sym_fixnum));
  else if (Character_sp ccx = x.asOrNull<Character_O>())
    return (Values(cl::_sym_character));
  else if (SingleFloat_sp sfx = x.asOrNull<SingleFloat_O>())
    return (Values(cl::_sym_single_float));
  else if (x.nilp())
    return (Values(cl::_sym_Symbol_O)); // Return _sym_null??
  else if (Symbol_sp sx = x.asOrNull<Symbol_O>())
    return (Values(cl::_sym_Symbol_O));
  else if (x.consp())
    return (Values(cl::_sym_list));
  else if (DoubleFloat_sp dfx = x.asOrNull<DoubleFloat_O>())
    return (Values(cl::_sym_DoubleFloat_O));
  else if (Bignum_sp bnx = x.asOrNull<Bignum_O>())
    return (Values(cl::_sym_Bignum_O));
  else if (Ratio_sp rx = x.asOrNull<Ratio_O>())
    return (Values(cl::_sym_Ratio_O));
#ifdef CLASP_LONG_FLOAT
  else if (LongFloat_sp lfx = x.asOrNull<LongFloat_O>())
    return (Values(cl::_sym_LongFloat_O));
#endif
  else if (Complex_sp cx = x.asOrNull<Complex_O>())
    return (Values(cl::_sym_Complex_O));
  else if (Package_sp px = x.asOrNull<Package_O>())
    return (Values(cl::_sym_Package_O));
  else if (HashTable_sp htx = x.asOrNull<HashTable_O>())
    return (Values(cl::_sym_HashTable_O));
  else if (Vector_sp vx = x.asOrNull<Vector_O>())
    return (Values(cl::_sym_Vector_O));
  else if (BitVector_sp bvx = x.asOrNull<BitVector_O>())
    return (Values(cl::_sym_BitVector_O));
  else if (Array_sp ax = x.asOrNull<Array_O>())
    return (Values(cl::_sym_Array_O));
  else if (Str_sp strx = x.asOrNull<Str_O>())
    return (Values(cl::_sym_String_O));
  //    else if ( x.isA<BaseString_O>() ) return(Values(_sym_BaseString_O));
  else if (Stream_sp streamx = x.asOrNull<Stream_O>())
    return (Values(cl::_sym_Stream_O));
  else if (ReadTable_sp rtx = x.asOrNull<ReadTable_O>())
    return (Values(cl::_sym_ReadTable_O));
  return Values(x->__class()->className());
  SIMPLE_ERROR(BF("Add af_type_to_symbol support for type: %s") % x->_instanceClass()->classNameAsString());
#pragma clang diagnostic pop
}

T_sp type_of(T_sp x) {
  _G();
  if (x.nilp())
    return cl::_sym_null;
#ifdef CLOS
  if (Instance_sp instance = x.asOrNull<Instance_O>()) {
    T_sp cl = lisp_instance_class(instance);
    T_sp t;
    if (Class_sp mcl = cl.asOrNull<Class_O>()) {
      t = mcl->className();
    } else if (Instance_sp icl = cl.asOrNull<Instance_O>()) {
      (void)icl;
      DEPRECIATEDP("Classes of instances should always be of Class_O type, not Instance_O");
      //	    t = icl->_CLASS_NAME();
    } else {
      SIMPLE_ERROR(BF("Illegal class %s for instance class of %s") % _rep_(cl) % _rep_(instance));
    }
    Symbol_sp st = gc::As<Symbol_sp>(t);
    if (t.nilp() || cl != T_sp(eval::funcall(cl::_sym_findClass, st, _Nil<T_O>()))) {
      t = cl;
    }
    return t;
  } else if (Class_sp mc = x.asOrNull<Class_O>()) {
    Class_sp mcc = lisp_static_class(mc);
    return mcc->className();
  } else
#endif
    if (x.fixnump()) {
      ql::list res(_lisp);
      res << cl::_sym_integer << x << x;
      return res.cons();
    } else if (Integer_sp ix = x.asOrNull<Integer_O>()) {
      ql::list res(_lisp);
      res << cl::_sym_integer << ix << ix;
      return res.cons();
    } else if (af_characterP(x)) {
      if (af_standard_char_p(gc::As<Character_sp>(x)))
        return cl::_sym_standard_char;
      return cl::_sym_character;
    } else if (Symbol_sp symx = x.asOrNull<Symbol_O>()) {
      if (x == _lisp->_true())
        return cl::_sym_boolean;
      if (af_keywordP(symx))
        return cl::_sym_keyword;
      return cl::_sym_symbol;
    } else if (String_sp sx = x.asOrNull<String_O>()) {
      Symbol_sp t;
      if (sx->adjustable_array_p() || sx->array_has_fill_pointer_p() || sx->_displaced_array_p()) {
        t = cl::_sym_array;
      } else
        t = cl::_sym_simple_array;
      return (ql::list(_lisp) << t << cl::_sym_base_char << Cons_O::createList(make_fixnum(1), make_fixnum(cl_length(sx)))).cons();
    } else if (Vector_sp vx = x.asOrNull<Vector_O>()) {
      if (vx->adjustable_array_p() || vx->_displaced_array_p()) {
        return (ql::list(_lisp) << cl::_sym_vector << vx->element_type_as_symbol() << vx->arrayDimensions()).cons();
      } else if (vx->array_has_fill_pointer_p() /* || (cl_elttype)x->vector.elttype != aet_object) */) {
        return (ql::list(_lisp) << cl::_sym_simple_array << vx->element_type_as_symbol() << vx->arrayDimensions()).cons();
      } else {
        return (ql::list(_lisp) << cl::_sym_simple_vector << make_fixnum(cl_length(vx))).cons();
      }
    } else if (Array_sp ax = x.asOrNull<Array_O>()) {
      Symbol_sp t;
      if (ax->adjustable_array_p() || ax->_displaced_array_p()) {
        t = cl::_sym_array;
      } else
        t = cl::_sym_simple_array;
      return (ql::list(_lisp) << t << ax->element_type_as_symbol() << ax->arrayDimensions()).cons();
    } else if (BitVector_sp bx = x.asOrNull<BitVector_O>()) {
      Symbol_sp t;
      if (bx->adjustable_array_p() || bx->array_has_fill_pointer_p() || bx->_displaced_array_p()) {
        t = cl::_sym_array;
      } else
        t = cl::_sym_simple_array;
      return (ql::list(_lisp) << t << cl::_sym_bit << Cons_O::createList(make_fixnum(1), make_fixnum(cl_length(bx)))).cons();
    } else if (WrappedPointer_sp pp = x.asOrNull<WrappedPointer_O>()) {
      return pp->_instanceClass()->className();
    } else if (af_structurep(x)) {
      return gc::As<StructureObject_sp>(x)->structureType();
    } else if (Stream_sp stx = x.asOrNull<Stream_O>()) {
      if (gc::IsA<SynonymStream_sp>(stx))
        return cl::_sym_SynonymStream_O;
      else if (gc::IsA<BroadcastStream_sp>(stx))
        return cl::_sym_BroadcastStream_O;
      else if (gc::IsA<ConcatenatedStream_sp>(stx))
        return cl::_sym_ConcatenatedStream_O;
      else if (gc::IsA<TwoWayStream_sp>(stx))
        return cl::_sym_TwoWayStream_O;
      else if (gc::IsA<StringInputStream_sp>(stx))
        return _sym_StringInputStream_O;
      else if (gc::IsA<StringOutputStream_sp>(stx))
        return _sym_StringOutputStream_O;
      else if (gc::IsA<EchoStream_sp>(stx))
        return cl::_sym_EchoStream_O;
      else
        return cl::_sym_FileStream_O;
    } else if (x.consp()) {
      return cl::_sym_cons;
    } else if (Pathname_sp px = x.asOrNull<Pathname_O>()) {
      if (af_logicalPathnameP(px)) {
        return cl::_sym_logical_pathname;
      } else {
        return cl::_sym_pathname;
      }
    }
  return af_type_to_symbol(x);
}

#define ARGS_af_type_of "(obj)"
#define DECL_af_type_of ""
#define DOCS_af_type_of "type_of"
T_sp af_type_of(T_sp x) {
  _G();
  return type_of(x);
}

#define ARGS_cl_sxhash "(obj)"
#define DECL_cl_sxhash ""
#define DOCS_cl_sxhash "sxhash"
Integer_sp cl_sxhash(T_sp obj) {
  _G();
  if (obj.nilp())
    return make_fixnum(1);
  HashGenerator hg;
  clasp_sxhash(obj, hg);
  return Integer_O::create(hg.hash());
}

// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------

}; /* core */

CL_NAMESPACE namespace core {

EXPOSE_CLASS(core, InvocationHistoryFrameIterator_O);

bool satisfiesTest(InvocationHistoryFrameIterator_sp iterator, T_sp test) {
  if (!iterator->isValid()) {
    SIMPLE_ERROR(BF("Invalid InvocationHistoryFrameIterator"));
  }
  if (test.nilp())
    return true;
  T_sp res = eval::funcall(test, iterator);
  return res.isTrue();
}

void nextInvocationHistoryFrameIteratorThatSatisfiesTest(Fixnum num, InvocationHistoryFrameIterator_sp iterator, T_sp test) {
  do {
    if (!iterator->isValid())
      return;
    if (satisfiesTest(iterator, test)) {
      if (num == 0)
        return;
      --num;
    }
    iterator->setFrame(iterator->frame()->previous());
  } while (num >= 0);
}

#define ARGS_InvocationHistoryFrameIterator_O_make ""
#define DECL_InvocationHistoryFrameIterator_O_make ""
#define DOCS_InvocationHistoryFrameIterator_O_make "Return the InvocationHistoryFrameIterator for the frame that is the (first) that satisfies (test)"
InvocationHistoryFrameIterator_sp InvocationHistoryFrameIterator_O::make(Fixnum first, T_sp test) {
  InvocationHistoryFrame *cur = _lisp->invocationHistoryStack().top();
  InvocationHistoryFrameIterator_sp iterator = InvocationHistoryFrameIterator_O::create();
  iterator->setFrame(cur);
  nextInvocationHistoryFrameIteratorThatSatisfiesTest(first, iterator, test);
  return iterator;
}

InvocationHistoryFrameIterator_sp InvocationHistoryFrameIterator_O::prev(T_sp test) {
  nextInvocationHistoryFrameIteratorThatSatisfiesTest(1, this->asSmartPtr(), test);
  return this->asSmartPtr();
}

T_sp InvocationHistoryFrameIterator_O::functionName() {
  if (!this->isValid()) {
    SIMPLE_ERROR(BF("Invalid InvocationHistoryFrameIterator"));
  }
  gctools::tagged_pointer<Closure> closure = this->_Frame->closure;
  if (!closure) {
    SIMPLE_ERROR(BF("Could not access closure of InvocationHistoryFrame"));
  }
  return closure->name;
}

T_sp InvocationHistoryFrameIterator_O::environment() {
  if (!this->isValid()) {
    SIMPLE_ERROR(BF("Invalid InvocationHistoryFrameIterator"));
  }
  gctools::tagged_pointer<Closure> closure = this->_Frame->closure;
  if (!closure) {
    SIMPLE_ERROR(BF("Could not access closure of InvocationHistoryFrame"));
  }
  return closure->closedEnvironment;
}

int InvocationHistoryFrameIterator_O::index() {
  if (!this->isValid()) {
    SIMPLE_ERROR(BF("Invalid InvocationHistoryFrameIterator"));
  }
  return this->_Frame->index();
}

Function_sp InvocationHistoryFrameIterator_O::function() {
  if (!this->isValid()) {
    SIMPLE_ERROR(BF("Invalid InvocationHistoryFrameIterator"));
  }
  gctools::tagged_pointer<Closure> closure = this->_Frame->closure;
  if (!closure) {
    SIMPLE_ERROR(BF("Could not access closure of InvocationHistoryFrame"));
  }
  return Function_O::make(closure); // Should I really be creating a new Function object every time???
}

Vector_sp InvocationHistoryFrameIterator_O::arguments() {
  if (!this->isValid()) {
    SIMPLE_ERROR(BF("Invalid InvocationHistoryFrameIterator"));
  }
  InvocationHistoryFrame *frame = this->_Frame;
  return frame->arguments();
}

void InvocationHistoryFrameIterator_O::exposeCando(::core::Lisp_sp lisp) {
  ::core::class_<InvocationHistoryFrameIterator_O>()
      .def("frameIteratorFunctionName", &InvocationHistoryFrameIterator_O::functionName)
      .def("frameIteratorArguments", &InvocationHistoryFrameIterator_O::arguments)
      .def("frameIteratorEnvironment", &InvocationHistoryFrameIterator_O::environment)
      .def("frameIteratorIsValid", &InvocationHistoryFrameIterator_O::isValid)
      .def("frameIteratorPreviousFrame", &InvocationHistoryFrameIterator_O::prev)
      //	.initArgs("(self)")
      ;
  SYMBOL_SC_(CorePkg, makeInvocationHistoryFrameIterator);
  Defun_maker(CorePkg, InvocationHistoryFrameIterator);
};

void InvocationHistoryFrameIterator_O::exposePython(::core::Lisp_sp lisp) {
//	PYTHON_CLASS_2BASES(Pkg(),Vector,"","",_LISP)
#ifdef USEBOOSTPYTHON
#endif
}

#define ARGS_core_getInvocationHistoryFrameSearch "(idx direction)"
#define DECL_core_getInvocationHistoryFrameSearch ""
#define DOCS_core_getInvocationHistoryFrameSearch "getInvocationHistoryFrameSearch - Return an InvocationHistoryFrame as an iterator."                                                               \
                                                  "If idx == NIL return the top frame. "                                                                                                             \
                                                  "If idx>=0 return the frame that satisfies *backtrace-frame-selector-hook* that has the index idx if direction==NIL, "                             \
                                                  "or if direction==:PREV return the frame previous to it (away from the top) or if direction==:NEXT the next frame (towards the top). "             \
                                                  "*backtrace-frame-selector-hook* is a function that takes an invocation-history-frame-iterator and returns true if it should be in the backtrace." \
                                                  "Test the result to make sure it is valid."
InvocationHistoryFrameIterator_sp core_getInvocationHistoryFrameSearch(T_sp idx, Symbol_sp direction) {
  SYMBOL_EXPORT_SC_(CorePkg, STARbacktraceFrameSelectorHookSTAR);
  SYMBOL_EXPORT_SC_(KeywordPkg, next);
  SYMBOL_EXPORT_SC_(KeywordPkg, prev);
  T_sp backtraceFrameSelectorHook = core::_sym_STARbacktraceFrameSelectorHookSTAR->symbolValue();
  InvocationHistoryFrameIterator_sp top = InvocationHistoryFrameIterator_O::make(0, backtraceFrameSelectorHook);
  if (idx.nilp())
    return top;
  if (!idx.fixnump()) {
    SIMPLE_ERROR(BF("The first argument must be nil (for top) or a fixnum >= 0 - was given: %s") % _rep_(idx));
  }
  Fixnum fidx = idx.unsafe_fixnum();
  InvocationHistoryFrameIterator_sp lastSelectedFrame = top;
  InvocationHistoryFrameIterator_sp cur = top;
  while (cur->isValid() && fidx != cur->index()) {
    lastSelectedFrame = cur;
    cur = cur->prev(backtraceFrameSelectorHook);
  }
  if (direction.nilp())
    return cur;
  if (direction == kw::_sym_next)
    return lastSelectedFrame;
  if (direction == kw::_sym_prev)
    return cur->prev(backtraceFrameSelectorHook);
  SIMPLE_ERROR(BF("Direction argument must be one of NIL, :NEXT, :PREV - received %s") % _rep_(direction));
}

#define ARGS_core_getInvocationHistoryFrameTop ""
#define DECL_core_getInvocationHistoryFrameTop ""
#define DOCS_core_getInvocationHistoryFrameTop "getInvocationHistoryFrameSearch - Return an top InvocationHistoryFrame as an iterator."
InvocationHistoryFrameIterator_sp core_getInvocationHistoryFrameTop() {
  return core_getInvocationHistoryFrameSearch(_Nil<T_O>(), _Nil<T_O>());
}

#define ARGS_core_getInvocationHistoryFrame ""
#define DECL_core_getInvocationHistoryFrame ""
#define DOCS_core_getInvocationHistoryFrame "getInvocationHistoryFrame - Return an indexed InvocationHistoryFrame as an iterator."
InvocationHistoryFrameIterator_sp core_getInvocationHistoryFrame(int idx) {
  Fixnum_sp fnidx = clasp_make_fixnum(idx);
  return core_getInvocationHistoryFrameSearch(fnidx, _Nil<T_O>());
}

#define ARGS_core_getInvocationHistoryFramePrev ""
#define DECL_core_getInvocationHistoryFramePrev ""
#define DOCS_core_getInvocationHistoryFramePrev "getInvocationHistoryFramePrev - Return the prev InvocationHistoryFrame before index as an iterator."
InvocationHistoryFrameIterator_sp core_getInvocationHistoryFramePrev(int idx) {
  Fixnum_sp fnidx = clasp_make_fixnum(idx);
  return core_getInvocationHistoryFrameSearch(fnidx, kw::_sym_prev);
}

#define ARGS_core_getInvocationHistoryFrameNext ""
#define DECL_core_getInvocationHistoryFrameNext ""
#define DOCS_core_getInvocationHistoryFrameNext "getInvocationHistoryFrameNext - Return the next InvocationHistoryFrame after index as an iterator."
InvocationHistoryFrameIterator_sp core_getInvocationHistoryFrameNext(int idx) {
  Fixnum_sp fnidx = clasp_make_fixnum(idx);
  return core_getInvocationHistoryFrameSearch(fnidx, kw::_sym_next);
}
};

extern "C" {
using namespace core;
#define ARGS_af_ihsBacktraceNoArgs "()"
#define DECL_af_ihsBacktraceNoArgs ""
#define DOCS_af_ihsBacktraceNoArgs "ihsBacktraceNoArgs"
void af_ihsBacktraceNoArgs() {
  _G();
  af_ihsBacktrace(_lisp->_true(), _Nil<core::T_O>());
};

#define ARGS_af_ihsTop "()"
#define DECL_af_ihsTop ""
#define DOCS_af_ihsTop "ihsTop"
int af_ihsTop() {
  InvocationHistoryFrameIterator_sp top = core_getInvocationHistoryFrameTop();
  if (!top->isValid())
    return 0;
  return top->index();
};

#define ARGS_af_ihsPrev "(cur)"
#define DECL_af_ihsPrev ""
#define DOCS_af_ihsPrev "ihsPrev"
int af_ihsPrev(int idx) {
  InvocationHistoryFrameIterator_sp prev = core_getInvocationHistoryFramePrev(idx);
  if (!prev->isValid())
    return 0;
  return prev->index();
};

#define ARGS_af_ihsNext "(cur)"
#define DECL_af_ihsNext ""
#define DOCS_af_ihsNext "ihsNext"
int af_ihsNext(int idx) {
  InvocationHistoryFrameIterator_sp next = core_getInvocationHistoryFrameNext(idx);
  if (!next->isValid())
    return 0;
  return next->index();
}

#define ARGS_af_ihsFun "(arg)"
#define DECL_af_ihsFun ""
#define DOCS_af_ihsFun "ihsFun: return the function in the invocation history stack at i"
T_sp af_ihsFun(int idx) {
  InvocationHistoryFrameIterator_sp cur = core_getInvocationHistoryFrame(idx);
  if (!cur->isValid())
    return _Nil<T_O>();
  return cur->function();
};

#define ARGS_af_ihsArguments "(arg)"
#define DECL_af_ihsArguments ""
#define DOCS_af_ihsArguments "ihsArguments: return the arguments to the function in the invocation history stack at i"
T_sp af_ihsArguments(int idx) {
  InvocationHistoryFrameIterator_sp cur = core_getInvocationHistoryFrame(idx);
  if (!cur->isValid())
    return _Nil<T_O>();
  return cur->arguments();
};

#define ARGS_af_ihsEnv "(cur)"
#define DECL_af_ihsEnv ""
#define DOCS_af_ihsEnv "ihsEnv"
T_sp af_ihsEnv(int idx) {
  InvocationHistoryFrameIterator_sp cur = core_getInvocationHistoryFrame(idx);
  if (!cur->isValid())
    return _Nil<T_O>();
  return cur->environment();
};

#define ARGS_af_ihsBds "(cur)"
#define DECL_af_ihsBds ""
#define DOCS_af_ihsBds "ihsBds"
int af_ihsBds(int idx) {
  InvocationHistoryFrameIterator_sp cur = core_getInvocationHistoryFrame(idx);
  if (!cur->isValid())
    return 0;
  return cur->frame()->bds();
};

#define ARGS_af_ihsCurrentFrame "()"
#define DECL_af_ihsCurrentFrame ""
#define DOCS_af_ihsCurrentFrame "ihsCurrentFrame"
int af_ihsCurrentFrame() {
  _G();
  T_sp cf = _sym_STARihsCurrentSTAR->symbolValue();
  if (cf.nilp()) {
    int icf = af_ihsTop();
    return af_setIhsCurrentFrame(icf);
  }
  int icf = unbox_fixnum(gc::As<Fixnum_sp>(cf));
  if (icf < 0) {
    _sym_STARihsCurrentSTAR->setf_symbolValue(make_fixnum(icf));
    return 0;
  }
  if (icf >= af_ihsTop()) {
    _sym_STARihsCurrentSTAR->setf_symbolValue(make_fixnum(af_ihsTop()));
    return af_ihsTop();
  }
  return icf;
}

#define ARGS_af_setIhsCurrentFrame "()"
#define DECL_af_setIhsCurrentFrame ""
#define DOCS_af_setIhsCurrentFrame "setIhsCurrentFrame"
int af_setIhsCurrentFrame(int icf) {
  _G();
  if (icf < 0)
    icf = 0;
  else if (icf >= af_ihsTop())
    icf = af_ihsTop();
  _sym_STARihsCurrentSTAR->setf_symbolValue(make_fixnum(icf));
  return icf;
}

#define ARGS_af_bdsTop "()"
#define DECL_af_bdsTop ""
#define DOCS_af_bdsTop "bdsTop"
int af_bdsTop() {
  return _lisp->bindings().top();
};

#define ARGS_af_bdsVar "(idx)"
#define DECL_af_bdsVar ""
#define DOCS_af_bdsVar "bdsVar"
Symbol_sp af_bdsVar(int idx) {
  return _lisp->bindings().var(idx);
};

#define ARGS_af_bdsVal "(idx)"
#define DECL_af_bdsVal ""
#define DOCS_af_bdsVal "bdsVal"
T_sp af_bdsVal(int idx) {
  return _lisp->bindings().val(idx);
};

#define ARGS_core_exceptionStack "()"
#define DECL_core_exceptionStack ""
#define DOCS_core_exceptionStack "exceptionStack"
Vector_sp core_exceptionStack() {
  return _lisp->exceptionStack().backtrace();
}

#define ARGS_core_exceptionStackDump "()"
#define DECL_core_exceptionStackDump ""
#define DOCS_core_exceptionStackDump "exceptionStackDump"
void core_exceptionStackDump() {
  _G();
  ExceptionStack &stack = _lisp->exceptionStack();
  printf("Exception stack size: %zu members\n", stack.size());
  for (int i(0); i < stack.size(); ++i) {
    string kind;
    switch (stack[i]._FrameKind) {
    case CatchFrame:
      kind = "catch";
      break;
    case BlockFrame:
      kind = "block";
      break;
    case TagbodyFrame:
      kind = "tagbody";
      break;
    default:
      kind = "unknown";
      break;
    };
    printf("Exception exceptionstack[%2d] = %8s %s@%p\n", i, kind.c_str(), _rep_(stack[i]._Key).c_str(), stack[i]._Key.raw_());
  }
  printf("----Done----\n");
};

#define ARGS_core_dynamicBindingStackDump "()"
#define DECL_core_dynamicBindingStackDump ""
#define DOCS_core_dynamicBindingStackDump "dynamicBindingStackDump"
void core_dynamicBindingStackDump(std::ostream &out) {
  DynamicBindingStack &bd = _lisp->bindings();
  for (int i(0), iEnd(bd.size()); i < iEnd; ++i) {
    out << "  dbstack[" << i << " --> " << _rep_(bd.var(i)) << std::endl;
  };
}

#define ARGS_af_ihsBacktrace "(&optional (out t) msg)"
#define DECL_af_ihsBacktrace ""
#define DOCS_af_ihsBacktrace "ihsBacktrace"
T_sp af_ihsBacktrace(T_sp outputDesignator, T_sp msg) {
  _G();
  T_sp ss;
  if (outputDesignator.nilp()) {
    ss = clasp_make_string_output_stream();
  } else {
    ss = coerce::outputStreamDesignator(outputDesignator);
  }
  if (!msg.nilp()) {
    clasp_writeln_string(((BF("\n%s") % _rep_(msg)).str()), ss);
  }
  clasp_writeln_string(((BF("%s") % _lisp->invocationHistoryStack().asString()).str()), ss);
  if (outputDesignator.nilp()) {
    return cl_get_output_stream_string(ss);
  }
  return _Nil<T_O>();
};
};

CL_NAMESPACE namespace core {
void initialize_primitives() {
  _G();
  //
  // Define functions first because generics and methods depend on some of them
  //
  Defun(allRegisteredClassNames);

  SYMBOL_SC_(CorePkg, smartPointerDetails);
  Defun(smartPointerDetails);
  SYMBOL_EXPORT_SC_(ClPkg, null);
  Defun(null);

  SYMBOL_SC_(CorePkg, STARfset);
  Defun(STARfset);

  SYMBOL_SC_(CorePkg, unbound);
  Defun(unbound);

  SYMBOL_EXPORT_SC_(ClPkg, read);
  ClDefun(read);

  SYMBOL_EXPORT_SC_(ClPkg, read_preserving_whitespace);
  ClDefun(read_preserving_whitespace);

  SYMBOL_EXPORT_SC_(ClPkg, read_delimited_list);
  Defun(read_delimited_list);

  SYMBOL_EXPORT_SC_(ClPkg, every);
  Defun(every);

  SYMBOL_EXPORT_SC_(ClPkg, some);
  Defun(some);

  SYMBOL_EXPORT_SC_(ClPkg, notevery);
  Defun(notevery);

  SYMBOL_EXPORT_SC_(ClPkg, notany);
  Defun(notany);

  SYMBOL_EXPORT_SC_(ClPkg, mapcar);
  ClDefun(mapcar);

  SYMBOL_EXPORT_SC_(ClPkg, mapc);
  ClDefun(mapc);

  SYMBOL_EXPORT_SC_(ClPkg, maplist);
  ClDefun(maplist);

  SYMBOL_EXPORT_SC_(ClPkg, mapl);
  ClDefun(mapl);

  SYMBOL_SC_(CorePkg, mapappend);
  Defun(mapappend);

  SYMBOL_EXPORT_SC_(ClPkg, mapcan);
  ClDefun(mapcan);

  SYMBOL_EXPORT_SC_(ClPkg, mapcon);
  ClDefun(mapcon);

  SYMBOL_SC_(CorePkg, macroexpand_default);
  Defun(macroexpand_default);

  SYMBOL_EXPORT_SC_(ClPkg, append);
  Defun(append);

  SYMBOL_EXPORT_SC_(ClPkg, classOf);
  Defun(classOf);

  SYMBOL_EXPORT_SC_(ClPkg, identity);
  Defun(identity);

  SYMBOL_EXPORT_SC_(ClPkg, constantp);
  Defun(constantp);

  SYMBOL_SC_(CorePkg, sequence_start_end);
  Defun(sequence_start_end);

  //	SYMBOL_EXPORT_SC_(ClPkg,open);
  //	Defun(open);

  SYMBOL_EXPORT_SC_(ClPkg, ash);
  ClDefun(ash);

  SYMBOL_SC_(CorePkg, type_to_symbol);
  Defun(type_to_symbol);

  SYMBOL_SC_(CorePkg, gdb);
  Defun(gdb);
  Defun(break);
  SYMBOL_SC_(CorePkg, gdbInspect);
  Defun(gdbInspect);

  defmacro(CorePkg, "backquote", &macro_backquote, ARGS_macro_backquote, DECL_macro_backquote, DOCS_macro_backquote, __FILE__, __LINE__);

  SYMBOL_EXPORT_SC_(ClPkg, gensym);
  Defun(gensym);

  SYMBOL_EXPORT_SC_(ClPkg, type_of);
  Defun(type_of);

  SYMBOL_EXPORT_SC_(ClPkg, specialOperatorP);
  ClDefun(specialOperatorP);

  SYMBOL_EXPORT_SC_(ClPkg, macroFunction);
  ClDefun(macroFunction);

  SYMBOL_SC_(CorePkg, separatePairList);
  Defun(separatePairList);

  SYMBOL_EXPORT_SC_(ClPkg, makeStringOutputStream);
  Defun(makeStringOutputStream);

  SYMBOL_EXPORT_SC_(ClPkg, set);

  SYMBOL_EXPORT_SC_(ClPkg, gensym);
  Defun(gensym);

  SYMBOL_EXPORT_SC_(ClPkg, type_of);
  Defun(type_of);

  SYMBOL_SC_(CorePkg, separatePairList);
  Defun(separatePairList);

  SYMBOL_EXPORT_SC_(ClPkg, makeStringOutputStream);
  Defun(makeStringOutputStream);

  SYMBOL_EXPORT_SC_(ClPkg, gensym);
  Defun(gensym);

  SYMBOL_EXPORT_SC_(ClPkg, type_of);
  Defun(type_of);

  //  CoreDefun(treatAsSpecialOperatorP);

  SYMBOL_SC_(CorePkg, separatePairList);
  Defun(separatePairList);

  SYMBOL_EXPORT_SC_(ClPkg, makeStringOutputStream);
  Defun(makeStringOutputStream);

  ClDefun(set);

  SYMBOL_SC_(CorePkg, testMemoryError);
  Defun(testMemoryError);

  SYMBOL_SC_(CorePkg, functionBlockName);
  Defun(functionBlockName);

  SYMBOL_SC_(CorePkg, validFunctionNameP);
  Defun(validFunctionNameP);

  SYMBOL_EXPORT_SC_(ClPkg, fdefinition);
  Defun(fdefinition);

  SYMBOL_EXPORT_SC_(ClPkg, fboundp);
  Defun(fboundp);

  SYMBOL_EXPORT_SC_(ClPkg, fmakunbound);
  Defun(fmakunbound);

  SYMBOL_EXPORT_SC_(ClPkg, values);
  ClDefun(values);
  CoreDefun(valuesTesting);

  SYMBOL_EXPORT_SC_(ClPkg, values_list);
  Defun(values_list);

  Defun(isTrue);


  SYMBOL_EXPORT_SC_(ExtPkg, getEnv);
  Defun(getEnv);
  SYMBOL_EXPORT_SC_(CorePkg, pointer);

#if 0
  CoreDefun(describe_cxx_object);
  CoreDefun(setenv);
  CoreDefun(pointer);
#endif
  SYMBOL_EXPORT_SC_(CorePkg, toTaggedFixnum);
  SYMBOL_EXPORT_SC_(CorePkg, fromTaggedFixnum);
  SYMBOL_EXPORT_SC_(CorePkg, dumpTaggedFixnum);
  Defun(toTaggedFixnum);
  Defun(fromTaggedFixnum);
  Defun(dumpTaggedFixnum);
  Defun(dumpAddressOf);
  Defun(incompleteNextHigherPowerOf_2);
  Defun(argc);
  Defun(argv);
  ClDefun(lispImplementationVersion);
  ClDefun(lispImplementationType);
  CoreDefun(lispImplementationId);
  ClDefun(softwareVersion);
  ClDefun(softwareType);
  ClDefun(machineVersion);
  ClDefun(machineType);
  ClDefun(machineInstance);
  ClDefun(sxhash);
  ClDefun(sleep);
  CoreDefun(create_tagged_immediate_value_or_nil);

  SYMBOL_SC_(CorePkg, ihsBacktrace);
  Defun(ihsBacktrace);
  SYMBOL_SC_(CorePkg, ihsTop);
  Defun(ihsTop);
  SYMBOL_SC_(CorePkg, ihsPrev);
  Defun(ihsPrev);
  SYMBOL_SC_(CorePkg, ihsNext);
  Defun(ihsNext);
  SYMBOL_SC_(CorePkg, ihsFun);
  Defun(ihsFun);
  Defun(ihsArguments);
  SYMBOL_SC_(CorePkg, ihsEnv);
  Defun(ihsEnv);
  SYMBOL_SC_(CorePkg, bdsTop);
  Defun(bdsTop);
  SYMBOL_SC_(CorePkg, bdsVar);
  Defun(bdsVar);
  SYMBOL_SC_(CorePkg, bdsVal);
  Defun(bdsVal);
  CoreDefun(exceptionStack);
  CoreDefun(exceptionStackDump);
  CoreDefun(trapExecution);

  CoreDefun(method_cache_status);
  CoreDefun(slot_cache_status);
  CoreDefun(single_dispatch_method_cache_status);
  CoreDefun(method_cache_resize);
  CoreDefun(slot_cache_resize);
  CoreDefun(single_dispatch_method_cache_resize);
  CoreDefun(cxx_lambda_list_handler_create_bindings_calls);
}

void initializePythonPrimitives(Lisp_sp lisp) {
  _G();
#if 0
	using namespace boost::python;
	def_raw(CorePkg,"read",&fn_read,ARGS_fn_read,DOCS_fn_read,_LISP);
#if 0
	def_raw(CorePkg,"readDelimitedList",&fn_read_delimited_list,ARGS_fn_read_delimited_list,DOCS_fn_read_delimited_list,_LISP);
#endif
#endif
}
};
