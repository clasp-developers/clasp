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
//#define DEBUG_LEVEL_FULL

#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <pthread.h> // TODO: PORTING - frgo, 2017-08-04
#include <signal.h>  // TODO: PORTING - frgo, 2017-08-04
#include <sys/utsname.h>
#include <clasp/external/PicoSHA2/picosha2.h>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/cons.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/bytecode_compiler.h> // Lexenv
#include <clasp/core/fileSystem.h>
#include <clasp/core/bformat.h>
#include <clasp/core/bignum.h>
#include <clasp/core/sysprop.h>
#include <clasp/core/character.h>
#include <clasp/core/array.h>
#include <clasp/core/package.h>
#include <clasp/core/readtable.h>
#include <clasp/core/instance.h>
#include <clasp/core/funcallableInstance.h>
#include <clasp/core/backquote.h>
#include <clasp/core/sequence.h>
#include <clasp/core/wrappedPointer.h>
#include <clasp/core/pathname.h>
#include <clasp/core/unixfsys.h>
#include <clasp/core/predicates.h>
#include <clasp/core/pointer.h>
#include <clasp/core/symbolTable.h>
//#include <clasp/core/clcenv.h>
#include <clasp/core/null.h>
#include <clasp/core/debugger.h>
#include <clasp/core/ql.h>
#include <clasp/core/numbers.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/compiler.h>
#include <clasp/core/print.h>
#include <clasp/core/singleDispatchMethod.h>
#include <clasp/core/singleDispatchGenericFunction.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/designators.h>
#include <clasp/core/primitives.h>
#include <clasp/core/numberToString.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/hashTableEql.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/lispReader.h>
#include <clasp/core/designators.h>
#include <clasp/core/wrappers.h>
#include <version.h>

#define DEBUG_LEVEL_NONE


namespace core {

std::string global_startupSourceName = "";
StartupEnum global_startupEnum = undefined;


int clasp_musleep(double dsec, bool alertable) {
  double seconds = floor(dsec);
  double frac_seconds = dsec - seconds;
  double nanoseconds = (frac_seconds * 1000000000.0);
  timespec ts;
  ts.tv_sec = seconds;
  ts.tv_nsec = nanoseconds;
  int code;
 AGAIN:
  code = nanosleep(&ts, &ts);
  int old_errno = errno;
  gctools::handle_all_queued_interrupts();
  {
    if (code < 0 && old_errno == EINTR && !alertable) {
      goto AGAIN;
    } else if (code<0) {
      printf("%s:%d nanosleep returned code = %d  errno = %d\n", __FILE__, __LINE__, code, errno);
    }
  }
  return code;
}

CL_LAMBDA(seconds);
CL_DECLARE();
CL_DOCSTRING(R"dx(sleep)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__sleep(Real_sp oseconds) {
  // seconds - a non-negative real.
  SYMBOL_EXPORT_SC_(ClPkg, sleep);
  double dsec = clasp_to_double(oseconds);
  if (dsec < 0.0) {
    TYPE_ERROR(oseconds,Cons_O::createList(cl::_sym_float,clasp_make_single_float(0.0)));
  }
  int retval = clasp_musleep(dsec,false);
  return nil<T_O>();
}


CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(A list of all symbols defined in C++)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__interpreter_symbols() {
  List_sp ls = nil<T_O>();

#ifndef SCRAPING
#define DO_SYMBOL(package, name, _a, _b, _c, _d) ls = Cons_O::create(package::name, ls);

#define ClPkg_SYMBOLS
#define AstToolingPkg_SYMBOLS
#define CorePkg_SYMBOLS
#define ExtPkg_SYMBOLS
#define LlvmoPkg_SYMBOLS
#define ClbindPkg_SYMBOLS
#define MpPkg_SYMBOLS
#define KeywordPkg_SYMBOLS
#define Clasp_ffi_pkg_SYMBOLS
#define SocketsPkg_SYMBOLS
#define ServeEventPkg_SYMBOLS
#define CompPkg_SYMBOLS
#define CleavirEnvPkg_SYMBOLS
#define CleavirPrimopPkg_SYMBOLS
#define ClosPkg_SYMBOLS
#define GrayPkg_SYMBOLS
#define ClcenvPkg_SYMBOLS
#define GcToolsPkg_SYMBOLS
  
#include SYMBOLS_SCRAPED_INC_H
  
#undef ClPkg_SYMBOLS
#undef AstToolingPkg_SYMBOLS
#undef CorePkg_SYMBOLS
#undef ExtPkg_SYMBOLS
#undef LlvmoPkg_SYMBOLS
#undef ClbindPkg_SYMBOLS
#undef MpPkg_SYMBOLS
#undef KeywordPkg_SYMBOLS
#undef Clasp_ffi_pkg_SYMBOLS
#undef SocketsPkg_SYMBOLS
#undef ServeEventPkg_SYMBOLS
#undef CompPkg_SYMBOLS
#undef CleavirEnvPkg_SYMBOLS
#undef CleavirPrimopPkg_SYMBOLS
#undef ClosPkg_SYMBOLS
#undef GrayPkg_SYMBOLS
#undef ClcenvPkg_SYMBOLS
#undef GcToolsPkg_SYMBOLS

#undef DO_SYMBOL
#endif // #ifndef SCRAPING
  return ls;
}

std::atomic<int64_t> global_next_number;

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(Return the next number.  An internal counter is incremented every time this function is called.)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__next_number() {
  int64_t num, next_num;
  do {
    num = global_next_number;
    next_num = num + 1;
    if ((next_num<<gctools::fixnum_shift)<=0) { // if it wraps it won't fit in a positive fixnum
      next_num = 1;
    }
  } while (!global_next_number.compare_exchange_weak(num,next_num));
  return core::clasp_make_fixnum(next_num);
};

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(lispImplementationType)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__lisp_implementation_type() {
  return SimpleBaseString_O::make(gctools::program_name());
};


SYMBOL_EXPORT_SC_(KeywordPkg,eclasp);

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(lisp-implementation-version)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__lisp_implementation_version() {
  stringstream ss;
  List_sp eclasp = gc::As<Cons_sp>(cl::_sym_STARfeaturesSTAR->symbolValue())->memberEq(kw::_sym_eclasp);
  List_sp cclasp = gc::As<Cons_sp>(cl::_sym_STARfeaturesSTAR->symbolValue())->memberEq(kw::_sym_cclasp);
  List_sp cst = gc::As<Cons_sp>(cl::_sym_STARfeaturesSTAR->symbolValue())->memberEq(kw::_sym_cst);
  if (eclasp.notnilp()) {
    ss << "e";
  } else if (cclasp.notnilp()) {
    ss << "c";
  }
  ss << gctools::program_name();
  ss << "-";
#if defined(USE_MPS)
  ss << "mps-";
#elif defined(USE_BOEHM)
# ifdef USE_PRECISE_GC
  ss << "boehmprecise-";
# else
  ss << "boehm-";
# endif
#elif defined(USE_MMTK)
# ifdef USE_PRECISE_GC
  ss << "mmtkprecise-";
# else
  ss << "mmtk-";
# endif
#endif
#ifdef RUNNING_PRECISEPREP
  ss << "mpsprep-";
#endif
  ss << CLASP_VERSION;
  if (cst.notnilp())
    ss << "-cst";
  else
    ss << "-non-cst";
  return SimpleBaseString_O::make(ss.str());
};


CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(lispImplementationId - a fragment of the git commit hash code)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__lisp_implementation_id() {
#ifdef CLASP_GIT_COMMIT
  string all = CLASP_GIT_COMMIT;
#define RIGHT_CHARS 9
  string rightChars;
  if (all.size() > RIGHT_CHARS) {
    rightChars = all.substr(all.size() - RIGHT_CHARS);
  } else {
    rightChars = all;
  }
  return SimpleBaseString_O::make(rightChars);
#else
  return nil<T_O>();
#endif
};

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(clasp-git-full-commit - the full git commit hash code)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__clasp_git_full_commit() {
#ifdef CLASP_GIT_FULL_COMMIT
  string all = CLASP_GIT_FULL_COMMIT;
  return SimpleBaseString_O::make(all);
#else
  return nil<T_O>();
#endif
};

CL_LAMBDA(obj);
CL_DECLARE();
CL_DOCSTRING(R"dx(Convert an object, either a fixnum, character or single float into an tagged version and return as an integer (either Fixnum or Bignum) or return NIL)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__create_tagged_immediate_value_or_nil(T_sp object) {
  if (object.fixnump() || object.characterp() || object.single_floatp()) {
    return Integer_O::create((Fixnum)object.raw_());
  }
  return nil<T_O>();
};

CL_LAMBDA(obj);
CL_DECLARE();
CL_DOCSTRING(R"dx(Convert a fixnum value that represents an immediate back into an immediate value)dx");
CL_DOCSTRING_LONG(R"dx(either a fixnum, character or single float into an tagged version and return as an integer (either Fixnum or Bignum) or return NIL)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__value_from_tagged_immediate(T_sp object) {
  if (object.fixnump()) {
    T_sp value((gctools::Tagged)object.unsafe_fixnum());
    return value;
  } if (gc::IsA<Bignum_sp>(object)) {
    size_t val = clasp_to_size_t(object);
    T_sp value((gctools::Tagged)val);
    return value;
  }
  SIMPLE_ERROR(("Value must fit in fixnum"));
}

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(softwareType)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__software_type() {
  struct utsname aux;
  if (uname(&aux) < 0)
    return nil<T_O>();
  else
    return SimpleBaseString_O::make(aux.sysname);
};

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(softwareVersion)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__software_version() {
  string all = CLASP_VERSION;
  return SimpleBaseString_O::make(all);
};

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(machineType)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__machine_type() {
  struct utsname aux;
  if (uname(&aux) < 0)
    return nil<T_O>();
  else
    return SimpleBaseString_O::make(aux.machine);
};

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(machineVersion)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__machine_version() {
  struct utsname aux;
  if (uname(&aux) < 0)
    return nil<T_O>();
  else
    return SimpleBaseString_O::make(aux.version);
};

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(machineInstance)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__machine_instance() {
  struct utsname aux;
   if (uname(&aux) < 0)
     return nil<T_O>();
   else
     return SimpleBaseString_O::make(aux.nodename);
};

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(argc)dx");
DOCGROUP(clasp);
CL_DEFUN int core__argc() {
  return global_options->_RawArguments.size();
};

CL_LAMBDA(idx);
CL_DECLARE();
CL_DOCSTRING(R"dx(argv)dx");
DOCGROUP(clasp);
CL_DEFUN SimpleBaseString_sp core__argv(int idx) {
  if ( idx < global_options->_RawArguments.size() ) return SimpleBaseString_O::make(global_options->_RawArguments[idx]);
  return SimpleBaseString_O::make("");
};

CL_LAMBDA(sym value);
CL_DECLARE();
CL_DOCSTRING(R"dx(set)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__set(Symbol_sp sym, T_sp val) {
  if (sym->getReadOnly())
    SIMPLE_ERROR(("Cannot modify value of constant %s") , _rep_(sym));
  sym->setf_symbolValue(val);
  return val;
};

CL_LAMBDA(sym value cell);
CL_DECLARE();
CL_DOCSTRING(R"dx(Set TLS symbol value, or if unbound there, the cell)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__setf_symbol_value_from_cell(Symbol_sp sym, T_sp val, Cons_sp cell) {
  sym->setf_symbolValueFromCell(val, cell);
  return val;
}

CL_LAMBDA(arg &optional msg);
CL_DECLARE();
DOCGROUP(clasp);
CL_DEFUN T_sp core__print_address_of(T_sp arg, T_sp msg) {
  ASSERT(arg.objectp());
  void *ptr = &(*arg);
  printf("%s:%d  AddressOf = %p msg: %s\n", __FILE__, __LINE__, ptr, _rep_(msg).c_str());
  return arg;
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(Ssee the incomplete-next-higher-power-of-2 builtin - only works for Fixnums and not the full range; just for testing)dx");
DOCGROUP(clasp);
CL_DEFUN int core__incomplete_next_higher_power_of_2(Fixnum_sp fn) {
  unsigned int f = unbox_fixnum(fn);
  return 1 << ((sizeof(f) * 8) - __builtin_clz(f));
};

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(allRegisteredClassNames)dx");
DOCGROUP(clasp);
CL_DEFUN Vector_sp core__all_registered_class_names() {
  ComplexVector_T_sp vo = ComplexVector_T_O::make( _lisp->classSymbolsHolder().size(), nil<T_O>());
  for (int i(0), iEnd(_lisp->classSymbolsHolder().size()); i < iEnd; ++i) {
    vo->rowMajorAset(i, _lisp->classSymbolsHolder()[i]);
  }
  return vo;
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(toTaggedFixnum)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__to_tagged_fixnum(int val) {
  return gctools::smart_ptr<T_O>(val);
};

CL_LAMBDA(val);
CL_DECLARE();
CL_DOCSTRING(R"dx(fromTaggedFixnum)dx");
DOCGROUP(clasp);
CL_DEFUN gctools::Fixnum core__from_tagged_fixnum(T_sp val) {
  if (val.fixnump()) {
    return val.unsafe_fixnum();
  }
  SIMPLE_ERROR(("Not a fixnum"));
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(dumpTaggedFixnum)dx");
DOCGROUP(clasp);
CL_DEFUN void core__dump_tagged_fixnum(T_sp val) {
  if (val.fixnump()) {
    printf("%s:%d Raw TaggedFixnum %p   Untagged %" PFixnum "\n",
           __FILE__, __LINE__, val.raw_(), val.unsafe_fixnum());
  } else
    printf("%s:%d Not a tagged fixnum\n", __FILE__, __LINE__);
}

CL_DOCSTRING(R"dx(Return a string representing the llvm version (eg: 3.6.0))dx");
DOCGROUP(clasp);
CL_DEFUN T_mv ext__llvm_version() {
  return Values(core::SimpleBaseString_O::make(CXX_MACRO_STRING(LLVM_VERSION)),clasp_make_single_float(LLVM_VERSION));
}


CL_LAMBDA(name &optional stream);
CL_DECLARE();
CL_DOCSTRING(R"dx(Describe a
C++ object
like CL:DESCRIBE)dx")
DOCGROUP(clasp);
CL_DEFUN void core__describe_cxx_object(T_sp obj, T_sp stream)
{
  if (obj.generalp()) {
    obj.unsafe_general()->describe(stream);
  } else if (obj.consp()) {
    obj.unsafe_cons()->describe(stream);
  }
  SIMPLE_ERROR(("Use the CL facilities to describe this object"));
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(isTrue)dx");
DOCGROUP(clasp);
CL_DEFUN bool core__is_true(T_sp arg) {
  return arg.isTrue();
};

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(Return the UNBOUND value)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__unbound() {
  return unbound<T_O>();
};

CL_LAMBDA();
CL_DECLARE();
CL_DOCSTRING(R"dx(smartPointerDetails - returns (values ptr-type px-offset px-size))dx");
CL_DOCSTRING_LONG(R"dx(The ptr-type is the type of pointer used to pass objects - either MPS-GARBAGE-COLLECTION or INTRUSIVE-REFERENCE-COUNTED-POINTER. The px-offset is the number of bytes offset of the smart_ptr data pointer from the start of the smart_ptr and px-size is the size of the data pointer)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__smart_pointer_details() {
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

CL_LAMBDA(core:&va-rest args);
CL_DECLARE();
CL_DOCSTRING(R"dx(values)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv cl__values(Vaslist_sp vargs) {
  // returns multiple values
  size_t nargs = vargs->nargs();
  SUPPRESS_GC();
#ifdef DEBUG_VALUES
  if (nargs >= core::MultipleValues::MultipleValuesLimit) {
    SIMPLE_ERROR(("Too many arguments to values - only %d are supported and you tried to return %d values") , core::MultipleValues::MultipleValuesLimit , nargs );
  }
  if (_sym_STARdebug_valuesSTAR &&
      _sym_STARdebug_valuesSTAR->boundP() &&
      _sym_STARdebug_valuesSTAR->symbolValue().notnilp()) {
    for (size_t di(0); di<nargs; ++di) {
      T_sp dsp((gctools::Tagged)va_arg(debugl,T_O*));
      printf("%s:%d   VALUES[%lu] -> %s\n", __FILE__, __LINE__, di, _rep_(dsp).c_str());
    }
    va_end(debugl);
  }
#endif
  core::MultipleValues &me = (core::lisp_multipleValues());
  me.setSize(0);
  core::T_sp first(nil<core::T_O>());
  if (nargs > 0) {
    first = vargs->next_arg();
    for (size_t i(1); i< nargs; ++i ) {
      T_O* tcsp = ENSURE_VALID_OBJECT(vargs->next_arg().raw_());
      T_sp csp((gctools::Tagged)tcsp);
      me.valueSet(i, csp);
    }
    me.setSize(nargs);
  }
  ENABLE_GC();
  core::T_mv mv = gctools::multiple_values<core::T_O>(first,nargs);
  return mv;
}

CL_LAMBDA(list);
CL_DECLARE();
CL_DOCSTRING(R"dx(values_list)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv cl__values_list(T_sp list) {
  if (list.consp()) {
    return ValuesFromCons(list);
  } else if (list.nilp()) {
    return Values0<T_O>();
  } else if (gc::IsA<Vaslist_sp>(list)) {
    Vaslist_sp vorig = gc::As_unsafe<Vaslist_sp>(list);
    Vaslist valist_copy(*vorig);
    Vaslist_sp valist(&valist_copy);
    return cl__values(valist);
  } else
    // FIXME: Incorrect as vaslists are also allowed.
    TYPE_ERROR(list, cl::_sym_list);
}

// Need to distinguish between nil as invalid-input and nil as the symbol found
// correctp indicates correctness of input
Symbol_sp functionBlockName(T_sp functionName, bool * correctp) {
  *correctp = true;
  if (cl__symbolp(functionName))
    return gc::As<Symbol_sp>(functionName);
  if (functionName.consp()) {
    List_sp cfn = functionName;
    if (oCar(cfn) == cl::_sym_setf && oCdr(cfn).consp() && cl__symbolp(oCadr(cfn)) && oCddr(cfn).nilp())  {
      return gc::As<Symbol_sp>(oCadr(cfn));
    }
  }
  *correctp = false;
  return nil<Symbol_O>();
}

CL_LAMBDA(functionName);
CL_DECLARE();
CL_DOCSTRING(R"dx(Return the symbol part of the name)dx");
CL_DOCSTRING_LONG(R"dx(If the functionName is a symbol return it.  If the functionName is a cons of the form (setf xxxx) return xxxx)dx");
DOCGROUP(clasp);
CL_DEFUN Symbol_sp core__function_block_name(T_sp functionName) {
  bool correct;
  Symbol_sp output = functionBlockName(functionName, &correct);
  if (!correct) {
    SIMPLE_ERROR(("Invalid function name: %s") , _rep_(functionName));
  }
  return output;
}

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(validFunctionNameP)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__valid_function_name_p(T_sp arg) {
  bool correct;
  Symbol_sp name = functionBlockName(arg, &correct);
  if (!correct)
    return nil<T_O>();
  return _lisp->_true();
};

CL_LAMBDA(listOfPairs);
CL_DECLARE();
CL_DOCSTRING(R"dx(Split a list of pairs into a pair of lists returned as MultipleValues)dx");
CL_DOCSTRING_LONG(R"dx(The first list is each first element and the second list is each second element or nil if there was no second element)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__separate_pair_list(List_sp listOfPairs) {
  ql::list firsts;
  ql::list seconds;
  for (auto cur : listOfPairs) {
    T_sp element = oCar(cur);
    if (cl__atom(element)) {
      firsts << element;
      seconds << nil<T_O>();
    } else if (element.consp()) {
      List_sp pair = element;
      size_t pairlen = cl__length(pair);
      if (pairlen == 2 || pairlen == 1) {
        firsts << oCar(pair);
        seconds << oCadr(pair);
      } else {
        SIMPLE_ERROR(("Expected one or two element list got: %s") , _rep_(pair));
      }
    } else {
      SIMPLE_ERROR(("Expected single object or 2-element list - got: %s") , _rep_(element));
    }
  }
  T_sp tfirsts = firsts.cons();
  return (Values(tfirsts, seconds.cons()));
}

// ignore env
CL_LAMBDA(name &optional env);
DOCGROUP(clasp);
CL_DEFUN T_sp core__get_global_inline_status(core::T_sp name, core::T_sp env)
{
  return core__get_sysprop(name,cl::_sym_inline);
}

CL_LAMBDA(name status &optional env);
DOCGROUP(clasp);
CL_DEFUN void core__setf_global_inline_status(core::T_sp name, bool status, core::T_sp env)
{
  core__put_sysprop(name,cl::_sym_inline,_lisp->_boolean(status));
}


// ignore env
CL_LAMBDA(name &optional env);
DOCGROUP(clasp);
CL_DEFUN T_sp core__function_type(T_sp name, T_sp env) {
  return core__get_sysprop(name, cl::_sym_ftype);
}

CL_LISPIFY_NAME("CORE:function-type");
CL_LAMBDA(type name &optional env);
DOCGROUP(clasp);
CL_DEFUN_SETF T_sp core__setf_function_type(T_sp type, T_sp name, T_sp env) {
  core__put_sysprop(name, cl::_sym_ftype, type);
  return type;
}

// Return whether this thing is shadowed in the function namespace.
// It's shadowed if there's an flet or labels or macrolet for the name.
// Used by get-setf-expansion and a few other places.
// Note that this will return T even if there is no global definition to shadow.
DOCGROUP(clasp);
CL_DEFUN bool core__operator_shadowed_p(T_sp name, T_sp env) {
  if (env.nilp()) {
    // No lexical environment.
    return false;
  } else if (comp::Lexenv_sp bce = env.asOrNull<comp::Lexenv_O>()) {
    return bce->functionInfo(name).notnilp();
  } else { // Cleavir, maybe
    SYMBOL_EXPORT_SC_(CorePkg, cleavir_operator_shadowed_p);
    T_sp lbool = eval::funcall(core::_sym_cleavir_operator_shadowed_p,
                               name, env);
    return lbool.notnilp();
  }
}

CL_LAMBDA(symbol &optional env);
CL_DECLARE();
CL_DOCSTRING(R"dx(See CLHS: macro-function)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__macro_function(Symbol_sp symbol, T_sp env) {
  if (env.nilp()) {
    if (symbol->fboundp() && symbol->macroP()) return symbol->symbolFunction();
    else return nil<T_O>();
  } else if (gc::IsA<comp::Lexenv_sp>(env)) {
    T_sp func = gc::As_unsafe<comp::Lexenv_sp>(env)->lookupMacro(symbol);
    if (func.nilp() // not bound locally, try global
        && symbol->fboundp() && symbol->macroP())
      return symbol->symbolFunction();
    else return func;
  } else {
    if (cleavirEnv::_sym_macroFunction->fboundp()) {
      return eval::funcall(cleavirEnv::_sym_macroFunction, symbol, env);
    } else {
      printf("%s:%d Unexpected environment for MACRO-FUNCTION before Cleavir is available - using toplevel environment\n", __FILE__, __LINE__);
      if (symbol->fboundp() && symbol->macroP()) return symbol->symbolFunction();
      else return nil<T_O>();
    }
  }
}

CL_LISPIFY_NAME("cl:macro-function");
CL_LAMBDA(function symbol &optional env);
CL_DECLARE();
CL_DOCSTRING(R"dx((setf macro-function))dx");
DOCGROUP(clasp);
CL_DEFUN_SETF T_sp setf_macro_function(Function_sp function, Symbol_sp symbol, T_sp env) {
  Function_sp namedFunction;
  (void)env; // ignore
  symbol->setf_macroP(true);
  symbol->setf_symbolFunction(function);
  return function;
}

CL_LAMBDA(symbol);
CL_DECLARE();
CL_DOCSTRING(R"dx(See CLHS: special-operator-p)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__special_operator_p(Symbol_sp sym) {
  // should signal type-error if its argument is not a symbol.
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
      (sym == cl::_sym_progn) ||
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
      (sym == cl::_sym_labels) ||
      (sym == cl::_sym_unwind_protect) ||
      (sym == cl::_sym_catch) ||
      (sym == cl::_sym_throw) ||
      (sym == cl::_sym_progv) ||
      (sym == cl::_sym_quote)) {
    return _lisp->_true();
  } else return nil<T_O>();
};

CL_DEFUN Integer_sp core__ash_left(Integer_sp integer, Integer_sp count) {
  if (count.fixnump())
    return clasp_shift_left(integer, count.unsafe_fixnum());
  else if (clasp_zerop(integer))
    return integer;
  else SIMPLE_ERROR(("ash for bignum count not implemented"));
}

CL_DEFUN Integer_sp core__ash_right(Integer_sp integer, Integer_sp count) {
  if (count.fixnump())
    return clasp_shift_right(integer, count.unsafe_fixnum());
  // bignum zero is impossible, so: all digits gone.
  else if (clasp_minusp(integer))
    return clasp_make_fixnum(-1);
  else return clasp_make_fixnum(0);
}

CL_DECLARE();
CL_DOCSTRING(R"dx(CLHS: ash)dx");
DOCGROUP(clasp);
CL_DEFUN Integer_sp cl__ash(Integer_sp integer, Integer_sp count) {
  if (count.fixnump()) {
    Fixnum c = count.unsafe_fixnum();
    if (c > 0) return clasp_shift_left(integer, c);
    else if (c < 0) return clasp_shift_right(integer, -c);
    else return integer;
  } else {
    // count is bignum
    // We don't have integers with more than most-positive-fixnum digits,
    // so this operation is now pretty trivial.
    if (clasp_plusp(count)) {
      if (clasp_zerop (integer))
        return integer;
        // result will not fit in memory, giveup (FIXME: storage-condition?)
      else SIMPLE_ERROR(("ash for bignum count not implemented"));
    } else if (clasp_minusp (count)) {
        // Count is a negative bignum, so all digits are gone.
      if (clasp_minusp(integer))
        return clasp_make_fixnum(-1);
      else return clasp_make_fixnum(0);
    } else return integer; // zero bignum, should be impossible
  }
}

CL_LAMBDA(&optional fmt-control &rest args);
CL_DECLARE();
CL_DOCSTRING(R"dx(Built in implementation of break - that calls the internal debugger - replace this with a CL implemented version)dx");
DOCGROUP(clasp);
CL_DEFUN void core__break_low_level(T_sp fmt, List_sp args) {
  if (fmt.notnilp()) {
    cl__format(_lisp->_true(), gc::As<String_sp>(fmt), args);
  }
  dbg_hook("built in break");
  core__invoke_internal_debugger(nil<core::T_O>());
};

CL_LAMBDA(&optional msg);
CL_DECLARE();
CL_DOCSTRING(R"dx(hook to invoke gdb)dx");
DOCGROUP(clasp);
NEVER_OPTIMIZE CL_DEFUN void core__gdb(T_sp msg) {
  T_sp obj = msg;
  string smsg = "No msg";
  if (obj.notnilp()) {
    smsg = _rep_(obj);
  }
  dbg_hook(smsg.c_str());
//  core__invoke_internal_debugger(nil<core::T_O>());
};


CL_LAMBDA(&optional msg);
CL_DECLARE();
CL_DOCSTRING(R"dx(hook to invoke gdb)dx");
DOCGROUP(clasp);
CL_DEFUN void core__trap_execution(T_sp msg) {
  T_sp obj = msg;
  string smsg = "No msg";
  if (obj.notnilp()) {
    smsg = _rep_(obj);
  }
  printf("%s:%d In core__trap_execution: %s \n", __FILE__, __LINE__, smsg.c_str());
  fflush(stdout);
};

CL_LAMBDA(msg o);
CL_DECLARE();
CL_DOCSTRING(R"dx(hook to invoke gdb)dx");
DOCGROUP(clasp);
CL_DEFUN void core__gdb_inspect(String_sp msg, T_sp o) {
  ASSERT(cl__stringp(msg));
  printf("gdbInspect object: %s\n", _rep_(o).c_str());
  dbg_hook(msg->get_std_string().c_str());
  core__invoke_internal_debugger(nil<core::T_O>());
};

CL_LISPIFY_NAME("EXT:specialp");
CL_LAMBDA(specialp symbol);
CL_DECLARE();
CL_DOCSTRING(R"dx(Set whether SYMBOL is globally known to be special. Use cautiously.)dx");
DOCGROUP(clasp);
CL_DEFUN_SETF bool setf_symbol_specialp(bool specialp, Symbol_sp symbol) {
  symbol->setf_specialP(specialp);
  return specialp;
}

CL_LAMBDA(symbol);
CL_DECLARE();
CL_DOCSTRING(R"dx(Returns whether SYMBOL is known to be a constant (i.e. from DEFCONSTANT).)dx");
DOCGROUP(clasp);
CL_DEFUN bool core__symbol_constantp(Symbol_sp symbol) {
  return symbol->getReadOnly();
}

CL_LISPIFY_NAME("CORE:symbol-constantp");
CL_LAMBDA(value symbol);
CL_DECLARE();
CL_DOCSTRING(R"dx(Set whether SYMBOL is known to be a constant. Use cautiously.)dx");
DOCGROUP(clasp);
CL_DEFUN_SETF T_sp setf_symbol_constantp(T_sp value, Symbol_sp symbol) {
  symbol->setReadOnly(value.notnilp());
  return value;
}

// Must be synced with constant-form-value in source-transformations.lisp
CL_LAMBDA(obj &optional env);
CL_DECLARE();
CL_DOCSTRING(R"dx(constantp)dx");
DOCGROUP(clasp);
CL_DEFUN bool cl__constantp(T_sp obj, T_sp env) {
  // ignore env
  if (cl__symbolp(obj)) {
    if (cl__keywordp(obj)) return true;
    return gc::As<Symbol_sp>(obj)->getReadOnly();
  }
  if (obj.consp()) {
    if (oCar(obj) == cl::_sym_quote)
      // more analysis could be done here.
      return true;
    else return false;
  }
  return true;
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(identity)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__identity(T_sp arg) {
  return arg;
};

CL_LAMBDA(obj);
CL_DECLARE();
CL_DOCSTRING(R"dx(null test - return true if the object is the empty list otherwise return nil)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__null(T_sp obj) {
  if (obj.nilp())
    return _lisp->_true();
  return nil<T_O>();
};

CL_LAMBDA(obj);
CL_DECLARE();
CL_DOCSTRING(R"dx(return class of object - see CLHS)dx");
DOCGROUP(clasp);
CL_DEFUN Instance_sp cl__class_of(T_sp obj) {
  Instance_sp result = lisp_instance_class(obj);
  return (result);
}

SYMBOL_EXPORT_SC_(CorePkg,STARdebug_fsetSTAR);
CL_LAMBDA(function-name fn &optional is-macro (lambda-list nil lambda-list-p));
CL_DECLARE();
CL_DOCSTRING(R"dx(Primitive to setup a function/macro)dx");
CL_DOCSTRING_LONG(R"dx(* Arguments
- function-name :: The name of the function to bind.
- fn :: The function object.
- is-macro :: A boolean.
- lambda-list : A lambda-list or nil.
- lambda-list-p : T if lambda-list is passed
* Description
Bind a function to the function slot of a symbol
- handles symbol function-name and (SETF XXXX) names.
IS-MACRO defines if the function is a macro or not.
LAMBDA-LIST passes the lambda-list.)dx")
DOCGROUP(clasp);
CL_DEFUN T_sp core__fset(T_sp functionName, Function_sp functor, T_sp is_macro, T_sp lambda_list, T_sp lambda_list_p) {
  if ( Function_sp functionObject = functor.asOrNull<Function_O>() ) {
    if ( lambda_list_p.notnilp() ) {
      functionObject->setf_lambdaList(lambda_list);
    }
  }
  if (cl__symbolp(functionName)) {
    Symbol_sp symbol = gc::As<Symbol_sp>(functionName);
    symbol->setf_macroP(is_macro.isTrue());
    symbol->setf_symbolFunction(functor);
    return functor;
  } else if (functionName.consp()) {
    SYMBOL_EXPORT_SC_(ClPkg, setf);
    List_sp cur = functionName;
    if (oCar(cur) == cl::_sym_setf) {
      Symbol_sp symbol = gc::As<Symbol_sp>(oCadr(cur));
      symbol->setSetfFdefinition(functor);
      return functor;
    }
  }
  TYPE_ERROR(functionName, Cons_O::createList(cl::_sym_satisfies, core::_sym_validFunctionNameP));
};

CL_LAMBDA(function-name);
CL_DECLARE();
CL_DOCSTRING(R"dx(fdefinition)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__fdefinition(T_sp functionName) {
  if (functionName.consp()) {
    List_sp cname = functionName;
    if (oCar(cname) == cl::_sym_setf) {
     // take care of (setf . bar) or (setf bar foo) or (setf bar .foo)
     // so don't go directly for the cadr
      T_sp dname = oCdr(cname);
      if (dname.consp()) {
        Symbol_sp name = gc::As<Symbol_sp>(oCar(dname));
        if (name.notnilp() && oCdr(dname).nilp()) {
          if (!name->fboundp_setf()) {
            ERROR_UNDEFINED_FUNCTION(functionName);
          }
          return name->getSetfFdefinition();
        }
      }
    }
  } else if (Symbol_sp sym = functionName.asOrNull<Symbol_O>() ) {
    if (!sym->fboundp()) {
      ERROR_UNDEFINED_FUNCTION(functionName);
    }
    return sym->symbolFunction();
  }
  TYPE_ERROR(functionName, Cons_O::createList(cl::_sym_satisfies, core::_sym_validFunctionNameP));
}

CL_LISPIFY_NAME("cl:fdefinition");
CL_LAMBDA(function name);
CL_DECLARE();
CL_DOCSTRING(R"dx((setf fdefinition))dx");
DOCGROUP(clasp);
CL_DEFUN_SETF T_sp setf_fdefinition(Function_sp function, T_sp name) {
  Symbol_sp symbol;
  Function_sp functionObject;
  if ((symbol = name.asOrNull<Symbol_O>())) {
    symbol->setf_macroP(false);
    symbol->setf_symbolFunction(function);
    return function;
  } else if (name.consp()) {
    List_sp cur = name;
    if (oCar(cur) == cl::_sym_setf) {
      T_sp cur2 = oCdr(cur);
      if (cur2.consp()) {
        symbol = gc::As<Symbol_sp>(oCar(cur2));
        if (symbol.notnilp() && oCdr(cur2).nilp()) {
          symbol->setSetfFdefinition(function);
          return function;
        }
      }
    }
  }
  TYPE_ERROR(name, Cons_O::createList(cl::_sym_satisfies, core::_sym_validFunctionNameP));
}

// reader in symbol.cc; this additionally involves function properties, so it's here
CL_LISPIFY_NAME("cl:symbol-function");
CL_LAMBDA(function symbol);
CL_DECLARE();
CL_DOCSTRING(R"dx((setf symbol-function))dx");
DOCGROUP(clasp);
CL_DEFUN_SETF T_sp setf_symbol_function(Function_sp function, Symbol_sp name) {
  Function_sp functionObject;
  name->setf_macroP(false);
  name->setf_symbolFunction(function);
  return function;
}

CL_LAMBDA(function-name);
CL_DECLARE();
CL_DOCSTRING(R"dx(fboundp)dx");
DOCGROUP(clasp);
CL_DEFUN bool cl__fboundp(T_sp functionName) {
  if (functionName.consp()) {
    List_sp cname = functionName;
    if (oCar(cname) == cl::_sym_setf) {
      T_sp dname = oCdr(cname);
      if (dname.consp()) {
        Symbol_sp name = gc::As<Symbol_sp>(oCar(dname));
        // (setf function <whatever>) is also a type error
        // (setf nil) is ok (setf) and (setf nil nil) not
        if (oCdr(dname).nilp())
          return name->fboundp_setf();
      // else is a type_error, so continue execution
      }
    }
  } else if (Symbol_sp sym = functionName.asOrNull<Symbol_O>() ) {
    return sym->fboundp();
  } else if (functionName.nilp()) {
    return false;
  }
  TYPE_ERROR(functionName, Cons_O::createList(cl::_sym_satisfies, core::_sym_validFunctionNameP));
}

CL_LAMBDA(function-name);
CL_DECLARE();
CL_DOCSTRING(R"dx(fmakunbound)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__fmakunbound(T_sp functionName) {
  if (functionName.consp()) {
    List_sp cname = functionName;
    if (oCar(cname) == cl::_sym_setf) {
      T_sp dname = oCdr(cname);
      if (dname.consp()) {
        Symbol_sp name = gc::As<Symbol_sp>(oCar(dname));
        if (name.notnilp()  && oCdr(dname).nilp()){
          name->fmakunbound_setf();
          return functionName;
        }
      }
    }
  } else if (Symbol_sp sym = functionName.asOrNull<Symbol_O>() ) {
    sym->fmakunbound();
    return sym;
  }
  TYPE_ERROR(functionName, Cons_O::createList(cl::_sym_satisfies, core::_sym_validFunctionNameP));
}

CL_LAMBDA(char &optional input-stream-designator recursive-p);
CL_DECLARE();
CL_DOCSTRING(R"dx(read a list up to a specific character - see CLHS)dx");
DOCGROUP(clasp);
CL_DEFUN List_sp cl__read_delimited_list(Character_sp chr, T_sp input_stream_designator, T_sp recursive_p) {
  T_sp sin = coerce::inputStreamDesignator(input_stream_designator);
#if 0
	// I think it is safe to ignore recursive_p
  if ( recursive_p.isTrue() )
  {
    SIMPLE_ERROR(("Currently I don't handle recursive-p[true] for read_delimited_list"));
  }
#endif
  List_sp result = read_list(sin, clasp_as_claspCharacter(chr), true);
  if (cl::_sym_STARread_suppressSTAR->symbolValue().isTrue()) {
    return nil<T_O>();
  }
  return result;
}

SYMBOL_EXPORT_SC_(CorePkg, STARread_hookSTAR);

CL_LAMBDA(&optional input-stream-designator (eof-error-p t) eof-value recursive-p);
CL_DECLARE();
CL_DOCSTRING(R"dx(read an object from a stream - see CLHS)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__read(T_sp input_stream_designator, T_sp eof_error_p, T_sp eof_value, T_sp recursive_p) {
  bool preserve_whitespace = true;
  if ( recursive_p.isTrue() ) {
    preserve_whitespace = _sym_STARpreserve_whitespace_pSTAR->symbolValue().isTrue();
  } else {
    preserve_whitespace = false;
  }
  DynamicScopeManager scope(_sym_STARpreserve_whitespace_pSTAR, _lisp->_boolean(preserve_whitespace));
  T_sp sin = coerce::inputStreamDesignator(input_stream_designator);
  if (_sym_STARread_hookSTAR->boundP() &&
      _sym_STARread_hookSTAR->symbolValue().notnilp())
    return eval::funcall(_sym_STARread_hookSTAR->symbolValue(), sin ,eof_error_p, eof_value, recursive_p);
  else
    return read_lisp_object(sin, eof_error_p.isTrue(), eof_value, recursive_p.notnilp());
}


CL_LAMBDA(&optional input-stream-designator (eof-error-p t) eof-value recursive-p);
CL_DECLARE();
CL_DOCSTRING(R"dx(read an object from a stream - see CLHS)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__fast_read(T_sp input_stream_designator, T_sp eof_error_p, T_sp eof_value, T_sp recursive_p) {
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

SYMBOL_EXPORT_SC_(CorePkg, STARread_preserving_whitespace_hookSTAR);
CL_LAMBDA(&optional input-stream-designator (eof-error-p t) eof-value recursive-p);
CL_DECLARE();
CL_DOCSTRING(R"dx(read an object from a stream while preserving whitespace - see CLHS)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__read_preserving_whitespace(T_sp input_stream_designator, T_sp eof_error_p, T_sp eof_value, T_sp recursive_p) {
  bool preserve_whitespace = true;
  if ( recursive_p.isTrue() ) {
    preserve_whitespace = _sym_STARpreserve_whitespace_pSTAR->symbolValue().isTrue();
  } else {
    preserve_whitespace = true;
  }
  DynamicScopeManager scope(_sym_STARpreserve_whitespace_pSTAR, _lisp->_boolean(preserve_whitespace));
  T_sp sin = coerce::inputStreamDesignator(input_stream_designator);
  if (_sym_STARread_preserving_whitespace_hookSTAR->boundP() &&
      _sym_STARread_preserving_whitespace_hookSTAR->symbolValue().notnilp())
    return eval::funcall(_sym_STARread_preserving_whitespace_hookSTAR->symbolValue(), sin ,eof_error_p, eof_value, recursive_p);
  else
    return read_lisp_object(sin, eof_error_p.isTrue(), eof_value, recursive_p.isTrue());
}

/* -------------------------------------------------------- */
/*     Sequence primitives                                  */

/* Only works on lists of lists - used to support macroexpansion backquote */
bool test_every_some_notevery_notany(Function_sp predicate, List_sp sequences, bool elementTest, bool elementReturn, bool fallThroughReturn, T_sp &retVal) {
  if (!sequences.consp()) goto FALLTHROUGH;
  {
    size_t nargs = sequences.unsafe_cons()->proper_list_length();
    MAKE_STACK_FRAME(frame,nargs);
    bool atend = false;
    while (!atend) {
      atend = false;
      size_t idx = 0;
      for ( auto cur : sequences ) {
        List_sp top = CONS_CAR(cur);
        gctools::fill_frame_one( frame, idx, oCar(top).raw_() );
        if (top.consp()) {
          cur->rplaca(CONS_CDR(top));
        } else atend = true;
      }
      if (!atend) {
        Vaslist valist_struct(nargs,frame);
        Vaslist_sp valist(&valist_struct);
        retVal = funcall_general<core::Function_O>(predicate.tagged_(),nargs,frame->arguments());
        if (retVal.isTrue() == elementTest) {
          return elementReturn;
        }
      }
    }
  }
 FALLTHROUGH:
  return fallThroughReturn;
}

CL_LAMBDA(predicate &rest sequences);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(See CLHS for every)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__every_list(T_sp predicate, List_sp sequences) {
  Function_sp op = coerce::functionDesignator(predicate);
  T_sp dummy;
  bool result = test_every_some_notevery_notany(op, sequences, false, false, true, dummy);
  return _lisp->_boolean(result);
}

CL_LAMBDA(predicate &rest sequences);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(See CLHS for some)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__some_list(T_sp predicate, List_sp sequences) {
  Function_sp op = coerce::functionDesignator(predicate);
  T_sp retVal;
  bool result = test_every_some_notevery_notany(op, sequences, true, true, false, retVal);
  if (result)
    return retVal;
  return nil<T_O>();
}

CL_LAMBDA(predicate &rest sequences);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(See CLHS for notany)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__notany_list(T_sp predicate, List_sp sequences) {
  Function_sp op = coerce::functionDesignator(predicate);
  T_sp dummy;
  bool result = test_every_some_notevery_notany(op, sequences, true, false, true, dummy);
  return _lisp->_boolean(result);
}

/*
  __BEGIN_DOC(candoScript.general.mapcar)
  __END_DOC
*/
SYMBOL_EXPORT_SC_(ClPkg, mapcar);

CL_LAMBDA(func-desig &rest lists);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(See CLHS for mapcar)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__mapcar(T_sp func_desig, List_sp lists) {
  Function_sp func = coerce::functionDesignator(func_desig);
  if (lists.consp()) {
    ql::list result;
    size_t nargs = lists.unsafe_cons()->proper_list_length();
    MAKE_STACK_FRAME(frame,nargs);
    bool atend = false;
    while (!atend) {
      atend = false;
      size_t idx = 0;
      for ( auto cur : lists ) {
        List_sp top = CONS_CAR(cur);
        gctools::fill_frame_one( frame, idx, oCar(top).raw_() );
        if (top.consp()) {
          cur->rplaca(CONS_CDR(top));
        } else atend = true;
      }
      if (!atend) {
        result << funcall_general<core::Function_O>(func.tagged_(),nargs,frame->arguments(0));
      }
    }
    return result.cons();
  }
  SIMPLE_PROGRAM_ERROR("Mapcar second argument can't be empty", lists);
}

/*
  __BEGIN_DOC(candoScript.general.mapcar)
  __END_DOC
*/
CL_LAMBDA(op &rest lists);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(See CLHS mapc)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__mapc(T_sp func_desig, List_sp lists) {
  // mapc function &rest lists+    list-1
  Function_sp func = coerce::functionDesignator(func_desig);
  if (lists.consp()) {
    List_sp result = CONS_CAR(lists);
    size_t nargs = lists.unsafe_cons()->proper_list_length();
    MAKE_STACK_FRAME(frame, nargs );
    bool atend = false;
    while (!atend) {
      atend = false;
      size_t idx = 0;
      for ( auto cur : lists ) {
        List_sp top = CONS_CAR(cur);
        gctools::fill_frame_one( frame, idx, oCar(top).raw_() );
        if (top.consp()) {
          cur->rplaca(CONS_CDR(top));
        } else atend = true;
      }
      if (!atend) {
        funcall_general<core::Function_O>(func.tagged_(), nargs, frame->arguments(0));
      }
    }
    return result;
  }
  SIMPLE_PROGRAM_ERROR("Mapc second argument can't be empty", lists);
}

CL_LAMBDA(func-desig &rest lists);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(See CLHS maplist)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__maplist(T_sp func_desig, List_sp lists) {
  Function_sp func = coerce::functionDesignator(func_desig);
  if (lists.consp()) {
    ql::list result;
    size_t nargs = lists.unsafe_cons()->proper_list_length();
    MAKE_STACK_FRAME(frame,nargs);
    bool atend = false;
    while (!atend) {
      atend = false;
      size_t idx = 0;
      for ( auto cur : lists ) {
        List_sp top = CONS_CAR(cur);
        gctools::fill_frame_one( frame, idx, top.raw_() );
        if (top.consp()) {
          cur->rplaca(CONS_CDR(top));
        } else atend = true;
      }
      if (!atend) {
        result << funcall_general<core::Function_O>(func.tagged_(), nargs, frame->arguments() );
      }
    }
    return result.cons();
  }
  SIMPLE_PROGRAM_ERROR("Maplist second argument can't be empty", lists);
}

CL_LAMBDA(op &rest lists);
CL_DECLARE();
CL_UNWIND_COOP(true);
CL_DOCSTRING(R"dx(See CLHS maplist)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__mapl(T_sp func_desig, List_sp lists) {
  Function_sp func = coerce::functionDesignator(func_desig);
  if (lists.consp()) {
    List_sp result = CONS_CAR(lists);
    size_t nargs = lists.unsafe_cons()->proper_list_length();
    MAKE_STACK_FRAME(frame, nargs );
    bool atend = false;
    while (!atend) {
      atend = false;
      size_t idx = 0;
      for ( auto cur : lists ) {
        List_sp top = CONS_CAR(cur);
        gctools::fill_frame_one( frame, idx, top.raw_() );
        if (top.consp()) {
          cur->rplaca(CONS_CDR(top));
        } else atend = true;
      }
      if (!atend) {
        funcall_general<core::Function_O>(func.tagged_(), nargs, frame->arguments(0));
      }
    }
    return result;
  }
  SIMPLE_PROGRAM_ERROR("Mapl second argument can't be empty", lists);
}

CL_LAMBDA(op &rest lists);
CL_DECLARE();
CL_DOCSTRING(R"dx(mapcon)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__mapcon(T_sp op, List_sp lists) {
  List_sp parts = cl__maplist(op, lists);
  T_sp result = cl__nconc(parts);
  return result;
};

CL_LAMBDA(op &rest lists);
CL_DECLARE();
CL_DOCSTRING(R"dx(mapcan)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__mapcan(T_sp op, List_sp lists) {
  List_sp parts = cl__mapcar(op, lists);
  T_sp result = cl__nconc(parts);
  return result;
};



/*!
  Equivalent to Common Lisps append function
  (append a b c)
  It recreates the list structures of the first arguments a and b and strings
  them together into one list and then points the cdr of the last element of this new list
  to c.
*/
CL_LAMBDA(core:&va-rest lists);
CL_DECLARE();
CL_DOCSTRING(R"dx(append as in clhs)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__append(Vaslist_sp args) {
  ql::list list;
  LOG("Carrying out append with arguments: %s" , _rep_(lists));
  size_t lenArgs = args->nargs();
  unlikely_if (lenArgs==0) return nil<T_O>();
  T_O* lastArg = (*args)[lenArgs-1];
  for ( int i(0),iEnd(lenArgs-1);i<iEnd; ++i ) {
    T_sp curit = args->next_arg();
    LIKELY_if (curit.consp()) {
      for (auto inner : (List_sp)curit) {
        list << CONS_CAR(inner);
      }
    } else if (!curit.nilp()) {
      TYPE_ERROR(curit,cl::_sym_list);
    }
  }
  /* Now append the last argument by setting the new lists last element cdr
       to the last argument of append */
  T_sp last((gctools::Tagged)lastArg);
  list.dot(last);
  T_sp res = list.cons();
  return res;
}

CL_LAMBDA(sequence start end);
CL_DECLARE();
CL_DOCSTRING(R"dx(Throws errors if start/end are out of range for the sequence.)dx");
CL_DOCSTRING_LONG(R"dx(I'm not sure what the func argument is for. If end is nil then it is set to the end of the sequence.  Return MultipleValues(start,end,length).)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__sequence_start_end(T_sp sequence, Fixnum_sp start, T_sp end) {
  // the func argument is useless, drop it here and in the caller with-start-end
  uint len = cl__length(sequence);
  if (end.nilp())
    end = make_fixnum(len);
  Fixnum_sp fnend = gc::As<Fixnum_sp>(end);
  if (unbox_fixnum(start) < 0) {
    TYPE_ERROR_INDEX_VARIABLE("start[~a] must be greater than zero for sequence ~a", sequence, start, len);
  }
  if (unbox_fixnum(start) > len) {
    TYPE_ERROR_INDEX_VARIABLE("start[~a] must be <= length of sequence[~a]", sequence, start, len);
  }
  if (unbox_fixnum(fnend) < 0) {
    TYPE_ERROR_INDEX_VARIABLE("end[~a] must be greater than zero for sequence ~a", sequence, end, len);
  }
  if (unbox_fixnum(fnend) > len) {
    TYPE_ERROR_INDEX_VARIABLE("end[~a] must be <= length of sequence[~a]", sequence, end, len);
  }
  if (unbox_fixnum(fnend) < unbox_fixnum(start)) {
    SIMPLE_PROGRAM_ERROR_2_ARGS ("end[~d] is less than start[~d]", end, start);
  }
  return (Values(start, fnend, make_fixnum(len)));
};

CL_DEFUN Symbol_sp core__gensym_quick(SimpleBaseString_sp prefix,
                                      size_t suffix) {
  size_t prefixlen = prefix->length();
  size_t suffixlen = (suffix < 2) ? 1 : std::ceil(std::log10(suffix));
  auto name = SimpleBaseString_O::make(prefixlen + suffixlen);
  for (size_t i = 0; i < prefixlen; ++i)
    (*name)[i] = (*prefix)[i];
  for (size_t j = prefixlen + suffixlen - 1; j >= prefixlen; --j) {
    auto div = std::div(suffix, 10);
    (*name)[j] = div.rem + '0';
    suffix = div.quot;
  }
  return Symbol_O::create(name);
}

CL_DEFUN Symbol_sp core__gensym_quick_char(SimpleCharacterString_sp prefix,
                                           size_t suffix) {
  size_t prefixlen = prefix->length();
  size_t suffixlen = (suffix < 2) ? 1 : std::ceil(std::log10(suffix));
  auto name = SimpleCharacterString_O::make(prefixlen + suffixlen);
  for (size_t i = 0; i < prefixlen; ++i)
    (*name)[i] = (*prefix)[i];
  for (size_t j = prefixlen + suffixlen - 1; j >= prefixlen; --j) {
    auto div = std::div(suffix, 10);
    (*name)[j] = div.rem + '0';
    suffix = div.quot;
  }
  return Symbol_O::create(name);

}

CL_LAMBDA(&optional (x "G"));
CL_DECLARE();
CL_DOCSTRING(R"dx(See CLHS gensym)dx");
DOCGROUP(clasp);
CL_DEFUN Symbol_sp cl__gensym(T_sp x) {
  if (cl__stringp(x)) {
    Integer_sp counter = gc::As<Integer_sp>(cl::_sym_STARgensym_counterSTAR->symbolValue());
    if (gc::IsA<SimpleBaseString_sp>(x)
        && counter.fixnump() && counter.unsafe_fixnum() >= 0) {
      // fast path
      // (GENSYM is actually very common in compile time due to
      //  macroexpansion, at least with the otherwise-quick
      //  bytecode-compiler. Profiling finds unexpected bottlenecks.)
      gctools::Fixnum fcounter = counter.unsafe_fixnum();
      Symbol_sp result = core__gensym_quick(gc::As_unsafe<SimpleBaseString_sp>(x), fcounter);
      cl::_sym_STARgensym_counterSTAR->setf_symbolValue(Integer_O::create(1 + fcounter));
      return result;
    } else if (gc::IsA<SimpleCharacterString_sp>(x)
               && counter.fixnump() && counter.unsafe_fixnum() >= 0) {
      // other fast path
      gctools::Fixnum fcounter = counter.unsafe_fixnum();
      Symbol_sp result = core__gensym_quick_char(gc::As_unsafe<SimpleCharacterString_sp>(x), fcounter);
      cl::_sym_STARgensym_counterSTAR->setf_symbolValue(Integer_O::create(1 + fcounter));
      return result;
    }
    String_sp sx = gc::As_unsafe<String_sp>(x);
    StrNs_sp ss = gc::As_unsafe<StrNs_sp>(core__make_vector(sx->element_type(),16,true,clasp_make_fixnum(0)));
    StringPushString(ss,sx);
    core__integer_to_string(ss,gc::As<Integer_sp>(cl::_sym_STARgensym_counterSTAR->symbolValue()),clasp_make_fixnum(10));
    // If and only if no explicit suffix is supplied, *gensym-counter* is incremented after it is used.
    if (clasp_minusp(counter))
      TYPE_ERROR(counter,cl::_sym_UnsignedByte);
    if (counter.fixnump()) {
      Fixnum gensymCounter = counter.unsafe_fixnum() +1;
      if (gensymCounter == (MOST_POSITIVE_FIXNUM + 1)) {
        Integer_sp gensymCounterBignum = Integer_O::create(gensymCounter);
        cl::_sym_STARgensym_counterSTAR->setf_symbolValue(gensymCounterBignum);
      } else cl::_sym_STARgensym_counterSTAR->setf_symbolValue(make_fixnum(gensymCounter));
    } else {
      // counter must be a bignum, positive
      // still need to increase the bignum by 1
      counter = gc::As_unsafe<Integer_sp>(clasp_one_plus(counter));
      cl::_sym_STARgensym_counterSTAR->setf_symbolValue(counter);
    }
    return Symbol_O::create(ss->asMinimalSimpleString());
  }
  if ((x.fixnump() || gc::IsA<Integer_sp>(x)) && (!(clasp_minusp(gc::As_unsafe<Integer_sp>(x))))) {
    SafeBufferStr8Ns ss;
    ss.string()->vectorPushExtend('G');
    core__integer_to_string(ss.string(),gc::As_unsafe<Integer_sp>(x),clasp_make_fixnum(10));
    return Symbol_O::create(ss.string()->asMinimalSimpleString());
  } else {
    TYPE_ERROR(x,Cons_O::createList(cl::_sym_or,cl::_sym_string,cl::_sym_UnsignedByte));
  }
}

CL_LAMBDA(x);
CL_DECLARE();
CL_DOCSTRING(R"dx(type_to_symbol)dx");
DOCGROUP(clasp);
CL_DEFUN Symbol_mv core__type_to_symbol(T_sp x) {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-variable"
  if (x.fixnump())
    return (Values(cl::_sym_fixnum));
  else if ( x.characterp() )
    return (Values(cl::_sym_character));
  else if ( x.single_floatp() )
    return (Values(cl::_sym_single_float));
  else if (x.consp())
    return (Values(cl::_sym_list));
  else if (x.generalp()) {
    General_sp gx(x.unsafe_general());
    if (gc::IsA<DoubleFloat_sp>(gx))
      return (Values(cl::_sym_DoubleFloat_O));
    else if (gc::IsA<Symbol_sp>(gx))
      return (Values(cl::_sym_Symbol_O));
    else if (gx.nilp())
      return (Values(cl::_sym_Symbol_O)); // Return _sym_null??
    else if (gc::IsA<Bignum_sp>(gx))
      return (Values(cl::_sym_Bignum_O));
    else if (gc::IsA<Ratio_sp>(gx))
      return (Values(cl::_sym_Ratio_O));
#ifdef CLASP_LONG_FLOAT
    else if (gc::IsA<LongFloat_sp>(gx))
      return (Values(cl::_sym_LongFloat_O));
#endif
    else if (gc::IsA<Complex_sp>(gx))
      return (Values(cl::_sym_Complex_O));
    else if (gc::IsA<Package_sp>(gx))
      return (Values(cl::_sym_Package_O));
    else if (gc::IsA<HashTable_sp>(gx))
      return (Values(cl::_sym_HashTable_O));
    else if (Array_sp ax = gx.asOrNull<Array_O>())
      // Handle all of the array subclasses using type_as_symbol()
      return Values(ax->array_type());
  //    else if ( x.isA<BaseString_sp>() ) return(Values(_sym_BaseString_O));
    else if (gc::IsA<Stream_sp>(gx))
      return (Values(cl::_sym_Stream_O));
    else if (gc::IsA<Readtable_sp>(gx))
      return (Values(cl::_sym_Readtable_O));
    return Values(gx->__class()->_className());
  }
  SIMPLE_ERROR(("Add core__type_to_symbol support for type: %s") , cl__class_of(x)->_classNameAsString());
#pragma clang diagnostic pop
}

T_sp type_of_decide_class(T_sp cl) {
  // Return the name of the class if it's a proper name, or else the class.
  Instance_sp mcl = gc::As<Instance_sp>(cl);
  T_sp type = mcl->_className();
  Symbol_sp st = gc::As<Symbol_sp>(type);
  // Only use the class-name as a type if it's the proper name of the class.
  if (type.nilp() || cl != T_sp(eval::funcall(cl::_sym_findClass, st, nil<T_O>()))) {
    type = cl;
  }
  return type;
}

T_sp type_of(T_sp x) {
  if (x.fixnump()) {
    ql::list res;
    res << cl::_sym_integer << x << x;
    return res.cons();
  } else if (x.consp()) {
    return cl::_sym_cons;
  } else if (x.single_floatp()) {
    return cl::_sym_single_float;
  } else if (x.valistp() ) {
    return core::_sym_valist;
  } else if (x.characterp()) {
    Character_sp character = gc::As_unsafe<Character_sp>(x);
    if (cl__standard_char_p(character))
      return cl::_sym_standard_char;
    else if (clasp_base_char_p(character))
      return cl::_sym_base_char;
    else return cl::_sym_character;
  } else if (Integer_sp ix = x.asOrNull<Integer_O>()) {
    ql::list res;
    res << cl::_sym_integer << ix << ix;
    return res.cons();
  }
#ifdef CLOS
  else if (Instance_sp instance = x.asOrNull<Instance_O>()) {
    return type_of_decide_class(lisp_instance_class(instance));
  } else if (FuncallableInstance_sp instance = x.asOrNull<FuncallableInstance_O>()) {
    return type_of_decide_class(lisp_instance_class(instance));
  }
#endif
  else if (Symbol_sp symx = x.asOrNull<Symbol_O>()) {
    if (x.nilp()) return cl::_sym_null;
    if (x == _lisp->_true())
      return cl::_sym_boolean;
    if (cl__keywordp(symx))
      return cl::_sym_keyword;
    return cl::_sym_symbol;
  } else if (gc::IsA<Array_sp>(x)) {
    Array_sp ax = gc::As_unsafe<Array_sp>(x);
    return ax->type_of();
  } else if (WrappedPointer_sp pp = x.asOrNull<WrappedPointer_O>()) {
    return pp->_instanceClass()->_className();
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
  } else if (Pathname_sp px = x.asOrNull<Pathname_O>()) {
    if (core__logical_pathname_p(px)) {
      return cl::_sym_logical_pathname;
    } else {
      return cl::_sym_pathname;
    }
  }
  return core__type_to_symbol(x);
}

CL_LAMBDA(obj);
CL_DECLARE();
CL_DOCSTRING(R"dx(type_of)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__type_of(T_sp x) {
  return type_of(x);
}

CL_LAMBDA(obj);
CL_DECLARE();
CL_DOCSTRING(R"dx(sxhash)dx");
DOCGROUP(clasp);
CL_DEFUN Fixnum_sp cl__sxhash(T_sp obj) {
  if (obj.nilp())
    return make_fixnum(1);
  HashGenerator hg;
  clasp_sxhash(obj, hg);
  gc::Fixnum hash = MOST_POSITIVE_FIXNUM&hg.rawhash();
  return clasp_make_fixnum(hash);
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

namespace core {

SYMBOL_EXPORT_SC_(KeywordPkg, next);
SYMBOL_EXPORT_SC_(KeywordPkg, prev);
};


namespace core {

CL_LISPIFY_NAME("ext:function-lambda-list");
CL_LAMBDA(function);
CL_DECLARE();
CL_DOCSTRING(R"dx(Return the lambda-list of a function designator. Note that
this is intended for human consumption and so may not
literally describe the function; e.g. macro and type expander
functions will have the defmacro/deftype lambda list.)dx")
DOCGROUP(clasp);
CL_DEFUN T_mv ext__function_lambda_list(T_sp obj) {
  if (obj.nilp()) {
    return Values(nil<T_O>(),nil<T_O>());
  } else if (Symbol_sp sym = obj.asOrNull<Symbol_O>()) {
    if (!sym->fboundp()) {
      return Values(nil<T_O>(),nil<T_O>());
    }
    Function_sp fn = sym->symbolFunction();
    return Values(ext__function_lambda_list(fn),_lisp->_true());
  } else if (Function_sp func = obj.asOrNull<Function_O>()) {
    return Values(func->lambdaList(), _lisp->_true());
  }
  return Values(nil<T_O>(),nil<T_O>());
}

CL_LAMBDA(function);
CL_DECLARE();
CL_DOCSTRING(R"dx(functionSourcePosInfo)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__function_source_pos_info(T_sp functionDesignator) {
  Function_sp closure = coerce::closureDesignator(functionDesignator);
  return closure->sourcePosInfo();
}


};

namespace core {
DOCGROUP(clasp);
CL_DEFUN T_sp core__hash256_hex_string(T_sp string)
{
  String_sp sarg = gc::As<String_sp>(string);
  std::string raw = sarg->get_std_string();
  std::string result;
  picosha2::hash256_hex_string(raw.begin(),raw.end(),result);
  return SimpleBaseString_O::make(result);
};


#include <clasp/external/hash-library/sha1.cpp>
#include <clasp/external/hash-library/md5.cpp>
#include <clasp/external/hash-library/sha256.cpp>
#include <clasp/external/hash-library/sha1.h>
#include <clasp/external/hash-library/md5.h>
#include <clasp/external/hash-library/hmac.h>

DOCGROUP(clasp);
CL_DEFUN T_sp core__hmac_sha256(SimpleVector_byte8_t_sp data, SimpleVector_byte8_t_sp key)
{
  std::string hash = hmac<SHA256>(&(*data)[0],data->length(),&(*key)[0],key->length());
  return SimpleBaseString_O::make(hash);
}

/*! Provide a list of SimpleVector_byte8_t_sp objects */
DOCGROUP(clasp);
CL_DEFUN T_sp core__digest_sha1(List_sp data)
{
  SHA1 digestSha1;
  for (auto part : data ) {
    SimpleVector_byte8_t_sp bytes = gc::As<SimpleVector_byte8_t_sp>(CONS_CAR(part));
    digestSha1.add(&((*bytes)[0]),bytes->length());
  }
  std::string result = digestSha1.getHash();
  return SimpleBaseString_O::make(result);
}

/*! Provide a list of SimpleVector_byte8_t_sp objects */
DOCGROUP(clasp);
CL_DEFUN T_sp core__digest_md5(List_sp data)
{
  MD5 digest;
  for (auto part : data ) {
    SimpleVector_byte8_t_sp bytes = gc::As<SimpleVector_byte8_t_sp>(CONS_CAR(part));
    digest.add(&((*bytes)[0]),bytes->length());
  }
  std::string result = digest.getHash();
  return SimpleBaseString_O::make(result);
}

/*! Provide a list of SimpleVector_byte8_t_sp objects */
DOCGROUP(clasp);
CL_DEFUN T_sp core__digest_sha256(List_sp data)
{
  SHA256 digest;
  for (auto part : data ) {
    SimpleVector_byte8_t_sp bytes = gc::As<SimpleVector_byte8_t_sp>(CONS_CAR(part));
    digest.add(&((*bytes)[0]),bytes->length());
  }
  std::string result = digest.getHash();
  return SimpleBaseString_O::make(result);
}

DOCGROUP(clasp);
CL_DEFUN SimpleVector_byte8_t_sp core__base_string_to_octets(T_sp tarray)
{
  if (!core__base_string_p(tarray)) {
    TYPE_ERROR(tarray,cl::_sym_base_string);
  }
  if (gc::IsA<SimpleBaseString_sp>(tarray)) {
    SimpleBaseString_sp sarray = gc::As_unsafe<SimpleBaseString_sp>(tarray);
    SimpleVector_byte8_t_sp result = SimpleVector_byte8_t_O::make(sarray->length(),0,false,sarray->length(),&(*sarray)[0]);
    return result;
  } else if (gc::IsA<Str8Ns_sp>(tarray)) {
    Str8Ns_sp sarray = gc::As_unsafe<Str8Ns_sp>(tarray);
    AbstractSimpleVector_sp basesv;
    size_t start, end;
    sarray->asAbstractSimpleVectorRange(basesv,start,end);
    SimpleBaseString_sp sbs = gc::As_unsafe<SimpleBaseString_sp>(basesv);
    SimpleVector_byte8_t_sp result = SimpleVector_byte8_t_O::make((end-start),0,false,(end-start),&(*sbs)[start]);
    return result;
  }
  SIMPLE_ERROR(("Don't get here"));
}

DOCGROUP(clasp);
CL_DEFUN SimpleVector_byte8_t_sp core__character_string_that_fits_in_base_string_to_octets(T_sp tarray)
{
  if (gc::IsA<SimpleCharacterString_sp>(tarray)) {
    SimpleCharacterString_sp sarray = gc::As_unsafe<SimpleCharacterString_sp>(tarray);
    SimpleVector_byte8_t_sp result = SimpleVector_byte8_t_O::make(sarray->length(),0,false);
    for ( int i=0; i<sarray->length(); ++i ) {
      int c = (*sarray)[i];
      (*result)[i] = c;
    }
    return result;
  } else if (gc::IsA<StrWNs_sp>(tarray)) {
    StrWNs_sp sarray = gc::As_unsafe<StrWNs_sp>(tarray);
    AbstractSimpleVector_sp basesv;
    size_t start, end;
    sarray->asAbstractSimpleVectorRange(basesv,start,end);
    SimpleCharacterString_sp sbs = gc::As_unsafe<SimpleCharacterString_sp>(basesv);
    SimpleVector_byte8_t_sp result = SimpleVector_byte8_t_O::make((end-start),0,false);
    for ( int i=0; i<sarray->length(); ++i ) {
      int c = (*sarray)[i];
      (*result)[i] = c;
    }
    return result;
  }
  SIMPLE_ERROR(("Handle Don't get here"));
}



CL_DEFUN void core__test_write_stream()
{
  write_bf_stream(fmt::sprintf("This is a test %d %d", 1, 2, T_sp() ));
}

CL_LAMBDA(filename &optional (max-lines 0) approach);
CL_DOCSTRING(R"dx(Count number of lines in text file up to max-lines if max-lines is not 0)dx");
CL_DOCSTRING_LONG(R"dx(Return (valus number-of-lines file-position size-of-file).)dx");
DOCGROUP(clasp);
CL_DEFUN T_mv core__countLinesInFile(const std::string& filename, size_t maxLines, bool approach) {
  int numberOfLines = 0;
  int charsSinceLastEol = 0;
  std::string line;
    // Use get() - character at a time
  std::ifstream myfile(filename);
  char c;
  if (myfile.is_open()) {
    do {
      c = myfile.get();
      if (c == EOF) {
        numberOfLines += ((charsSinceLastEol>0) ? 1 : 0);
        break;
      }
      if (maxLines && (numberOfLines>=maxLines)) break;
      if (c == '\n') {
        ++numberOfLines;
        charsSinceLastEol = 0;
      } else {
        ++charsSinceLastEol;
      }
      if ((numberOfLines & 0xffff) == 0) {
        gctools::handle_all_queued_interrupts();
      }
    } while (true);
    size_t currentFilePos;
    size_t sizeOfFile;
  // Get current position in file
    struct stat stat_buf;
    int rc = stat(filename.c_str(), &stat_buf);
    sizeOfFile = rc == 0 ? stat_buf.st_size : -1;
    if (c != EOF) {
      currentFilePos = myfile.tellg();
    } else {
      currentFilePos = sizeOfFile;
    }
    return Values(make_fixnum(numberOfLines),make_fixnum(currentFilePos),make_fixnum(sizeOfFile));
  }
  SIMPLE_ERROR(("Could not open file %s") , filename);
}
};

extern "C" {
int add_two_numbers(int x, int y) {
  return x + y;
}

void print_add_two_numbers(int x, int y) {
  printf("%s:%d %d + %d -> %d\n", __FILE__, __LINE__, x, y,  x + y );
}
};

  SYMBOL_SC_(CorePkg, smartPointerDetails);
  SYMBOL_EXPORT_SC_(ClPkg, null);
  SYMBOL_SC_(CorePkg, unbound);
  SYMBOL_EXPORT_SC_(ClPkg, read);
  SYMBOL_EXPORT_SC_(ClPkg, read_preserving_whitespace);
  SYMBOL_EXPORT_SC_(ClPkg, read_delimited_list);
  SYMBOL_EXPORT_SC_(ClPkg, every);
  SYMBOL_EXPORT_SC_(ClPkg, some);
  SYMBOL_EXPORT_SC_(ClPkg, notevery);
  SYMBOL_EXPORT_SC_(ClPkg, notany);
  SYMBOL_EXPORT_SC_(ClPkg, mapcar);
  SYMBOL_EXPORT_SC_(ClPkg, mapc);
  SYMBOL_EXPORT_SC_(ClPkg, maplist);
  SYMBOL_EXPORT_SC_(ClPkg, mapl);
  SYMBOL_SC_(CorePkg, mapappend);
  SYMBOL_EXPORT_SC_(ClPkg, mapcan);
  SYMBOL_EXPORT_SC_(ClPkg, mapcon);
  SYMBOL_EXPORT_SC_(ClPkg, append);
  SYMBOL_EXPORT_SC_(ClPkg, classOf);
  SYMBOL_EXPORT_SC_(ClPkg, identity);
  SYMBOL_EXPORT_SC_(ClPkg, constantp);
  SYMBOL_SC_(CorePkg, sequence_start_end);
  SYMBOL_EXPORT_SC_(ClPkg, ash);
  SYMBOL_SC_(CorePkg, type_to_symbol);
  SYMBOL_SC_(CorePkg, gdb);
  SYMBOL_SC_(CorePkg, gdbInspect);
  SYMBOL_EXPORT_SC_(ClPkg, gensym);
  SYMBOL_EXPORT_SC_(ClPkg, type_of);
  SYMBOL_EXPORT_SC_(ClPkg, specialOperatorP);
  SYMBOL_EXPORT_SC_(ClPkg, macroFunction);
  SYMBOL_SC_(CorePkg, separatePairList);
  SYMBOL_EXPORT_SC_(ClPkg, set);
  SYMBOL_EXPORT_SC_(ClPkg, gensym);
  SYMBOL_SC_(CorePkg, separatePairList);
  SYMBOL_EXPORT_SC_(ClPkg, gensym);
  SYMBOL_SC_(CorePkg, separatePairList);
  SYMBOL_SC_(CorePkg, testMemoryError);
  SYMBOL_SC_(CorePkg, functionBlockName);
  SYMBOL_SC_(CorePkg, validFunctionNameP);
  SYMBOL_EXPORT_SC_(ClPkg, fdefinition);
  SYMBOL_EXPORT_SC_(ClPkg, fboundp);
  SYMBOL_EXPORT_SC_(ClPkg, fmakunbound);
  SYMBOL_EXPORT_SC_(ClPkg, values);
  SYMBOL_EXPORT_SC_(ClPkg, values_list);
  SYMBOL_EXPORT_SC_(CorePkg, pointer);
  SYMBOL_EXPORT_SC_(CorePkg, toTaggedFixnum);
  SYMBOL_EXPORT_SC_(CorePkg, fromTaggedFixnum);
  SYMBOL_EXPORT_SC_(CorePkg, dumpTaggedFixnum);
  SYMBOL_SC_(CorePkg, ihsBacktrace);
  SYMBOL_SC_(CorePkg, ihsTop);
  SYMBOL_SC_(CorePkg, ihsPrev);
  SYMBOL_SC_(CorePkg, ihsNext);
  SYMBOL_SC_(CorePkg, ihsFun);
  SYMBOL_SC_(CorePkg, ihsEnv);
  SYMBOL_SC_(CorePkg, bdsTop);
  SYMBOL_SC_(CorePkg, bdsVar);
  SYMBOL_SC_(CorePkg, bdsVal);




namespace core {




int tak_aux(int x, int y, int z, bool allocate)
{
  if (y < x) {
    return tak_aux(tak_aux(x-1,y,z,allocate),tak_aux(y-1,z,x,allocate),tak_aux(z-1,x,y,allocate),allocate);
  } else {
    if (allocate) {
#ifdef USE_BOEHM      
      GC_MALLOC(128);
#endif
    }
    return z;
  }
}

int tak(int x, int y, int z, bool allocate, int times) {
  int ret;
  for ( int ii=0; ii<times; ++ii ) {
    ret = tak_aux(x,y,z,allocate);
  }
  return ret;
}

struct Ctak {
  int val;
  Ctak(int v) : val(v) {};
};

int ctak_aux(int x, int y, int z, bool allocate)
{
  if (!(y < x)) {
    Ctak ret(z);
    if (allocate) {
#ifdef USE_BOEHM
      GC_MALLOC(128);
#endif
    }
    throw ret;
  } else {
    int rx;
    try {
      ctak_aux(x-1,y,z,allocate);
    } catch (Ctak& val) {
      rx = val.val;
    }
    int ry;
    try {
      ctak_aux(y-1,z,x,allocate);
    } catch (Ctak& val) {
      ry = val.val;
    }
    int rz;
    try {
      ctak_aux(z-1,x,y,allocate);
    } catch (Ctak& val) {
      rz = val.val;
    }
    return ctak_aux(rx,ry,rz,allocate);
  }
}

int ctak(int x, int y, int z, bool allocate,int times) {
  int ret;
  for (int ii=0; ii<times; ++ii) {
    try {
      ctak_aux(x,y,z,allocate);
    } catch (Ctak& val) {
      ret = val.val;
    }
  }
  return ret;
}


CL_DOCSTRING(R"dx(Run the ctak test function (google 'tak function' - this is a try/catch/throw version))dx");
CL_LAMBDA(x y z &key allocate (times 1));
DOCGROUP(clasp);
CL_DEFUN void core__ctak(int x, int y, int z, bool allocate, int times)
{
  ctak(x,y,z,allocate,times);
}

CL_DOCSTRING(R"dx(Run the tak test function (google 'tak function'))dx");
CL_LAMBDA(x y z &key allocate (times 1));
DOCGROUP(clasp);
CL_DEFUN void core__tak(int x, int y, int z, bool allocate,int times)
{
  tak(x,y,z,allocate,times);
}

};


namespace core {

uint32_t crc32_for_byte(uint32_t r) {
  for(int j = 0; j < 8; ++j)
    r = (r & 1? 0: (uint32_t)0xEDB88320L) ^ r >> 1;
  return r ^ (uint32_t)0xFF000000L;
}

void crc32(const void *data, size_t n_bytes, uint32_t* crc) {
  static uint32_t table[0x100];
  if(!*table)
    for(size_t i = 0; i < 0x100; ++i)
      table[i] = crc32_for_byte(i);
  for(size_t i = 0; i < n_bytes; ++i)
    *crc = table[(uint8_t)*crc ^ ((uint8_t*)data)[i]] ^ *crc >> 8;
}



};



namespace core {

CL_DEFUN void core__withStackCons(T_sp car, T_sp cdr, T_sp fn) {
  gc::StackAllocate<Cons_O> cons(car,cdr);
  printf("%s:%d:%s The cons size is %lu\n", __FILE__, __LINE__, __FUNCTION__, sizeof(cons));
  printf("%s:%d:%s The ConsSizeCalculator<Cons_O> size is %lu\n", __FILE__, __LINE__, __FUNCTION__, gctools::ConsSizeCalculator<gctools::RuntimeStage,Cons_O>::value());
  eval::funcall(fn,cons.asSmartPtr());
}
#if 0
DOCGROUP(clasp);
CL_DEFUN core::Test_sp core__makeTest() {
  auto tt = new Test();
  auto t = gctools::GC<Test_O>::allocate_with_default_constructor();
  t->set_wrapped(tt);
  return t;
}
#endif

void Test::setMultiplier(int m) {
  this->multiplier = m;
}

void Test::set2(int n0, int n1) {
  this->numbers.clear();
  printf("%s:%d In set2 n0-> %d n1-> %d\n", __FILE__, __LINE__, n0, n1);
  this->numbers.push_back(n0);
  this->numbers.push_back(n1);
}

void Test::set3(int n0, int n1, int n2) {
  this->numbers.clear();
  this->numbers.push_back(n0);
  this->numbers.push_back(n1);
  this->numbers.push_back(n2);
}


void Test::print_numbers() {
  int idx=0;
  for (auto n : this->numbers) {
    printf("%s:%d number[%d] -> %d\n", __FILE__, __LINE__, idx, n*this->multiplier);
    ++idx;
  }
}

CL_EXTERN_DEFMETHOD(Test_O,&Test::setMultiplier);
CL_EXTERN_DEFMETHOD(Test_O,&Test::set2);
CL_EXTERN_DEFMETHOD(Test_O,&Test::set3);
CL_EXTERN_DEFMETHOD(Test_O,&Test::print_numbers);

};



namespace core {
void initialize_primitives() {
  //
  // Define functions first because generics and methods depend on some of them
  //

}

};

