/*
    File: llvmoPackage.cc
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

#include <clasp/core/foundation.h>
#include <stdint.h>

#include <llvm/Support/raw_ostream.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/builtInClass.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/lispStream.fwd.h>
#include <clasp/core/designators.h>
#include <clasp/llvmo/llvmoPackage.h>
//#include "llvmoExpose.generated.h"
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/llvmo/insertPoint.h>
#include <clasp/llvmo/debugLoc.h>
#include <clasp/llvmo/llvmoDwarf.h>
#include <clasp/llvmo/clbindLlvmExpose.h>
#include <clasp/llvmo/debugInfoExpose.h>
#include <clasp/llvmo/intrinsics.h>
#include <clasp/llvmo/claspLinkPass.h>
#include <clasp/core/loadTimeValues.h>
#include <clasp/core/unixfsys.h>
#include <clasp/core/environment.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/str.h>
#include <clasp/core/wrappers.h>

using namespace core;

namespace llvmo {
#define EXPOSE_TO_CANDO
#define Use_LlvmoPkg
#define EXTERN_REGISTER
//#include <clasp/core/initClasses.h>
#undef EXTERN_REGISTER
#undef Use_LlvmoPkg
#undef EXPOSE_TO_CANDO
};

//
// Load the gctools::GcInfo<core-classes>::Kind specializers
//
#define NAMESPACE_llvmo
#include <clasp/gctools/gc_interface.h>
#undef NAMESPACE_llvmo

namespace llvmo {

SYMBOL_EXPORT_SC_(LlvmoPkg, STARrunTimeExecutionEngineSTAR);

void redirect_llvm_interface_addSymbol() {
  //	llvm_interface::addSymbol = &addSymbolAsGlobal;
}

CL_LAMBDA(filename &optional verbose print external_format);
CL_DEFUN bool llvm_sys__load_bitcode(core::Pathname_sp filename, bool verbose, bool print, core::T_sp externalFormat )
{
  core::DynamicScopeManager scope(cl::_sym_STARpackageSTAR, cl::_sym_STARpackageSTAR->symbolValue());
  T_sp tn = cl__truename(filename);
  if ( tn.nilp() ) {
    SIMPLE_ERROR(BF("Could not get truename for %s") % _rep_(filename));
  }
  core::T_sp tnamestring = cl__namestring(filename);
  if ( tnamestring.nilp() ) {
    SIMPLE_ERROR(BF("Could not create namestring for %s") % _rep_(filename));
  }
  if (comp::_sym_STARllvm_contextSTAR->symbolValue().nilp()) {
    SIMPLE_ERROR(BF("The cmp:*llvm-context* is NIL"));
  }
  core::Str_sp namestring = gctools::As<core::Str_sp>(tnamestring);
  Module_sp m = llvm_sys__parseBitcodeFile(namestring,comp::_sym_STARllvm_contextSTAR->symbolValue());
  EngineBuilder_sp engineBuilder = EngineBuilder_O::make(m);
  TargetOptions_sp targetOptions = TargetOptions_O::make();
  engineBuilder->setTargetOptions(targetOptions);
  ExecutionEngine_sp executionEngine = engineBuilder->createExecutionEngine();
  if (comp::_sym_STARload_time_value_holder_nameSTAR->symbolValue().nilp() ) {
    SIMPLE_ERROR(BF("The cmp:*load-time-value-holder-name* is nil"));
  }
  finalizeEngineAndRegisterWithGcAndRunMainFunctions(executionEngine,
                                                     comp::_sym_STARload_time_value_holder_nameSTAR->symbolValue(),
                                                     namestring);
  return true;
}

CL_DEFUN core::Str_sp llvm_sys__mangleSymbolName(core::Str_sp name) {
  stringstream sout;
  const char *cur = name->get().c_str();
  bool first = true;
  while (*cur) {
    if (((*cur) >= 'a' && (*cur) <= 'z') || ((*cur) >= 'A' && (*cur) <= 'Z') || ((*cur) == '_') || (!first && ((*cur) >= '0' && (*cur) <= '9'))) {
      sout << (*cur);
    } else {
      sout << "_";
      sout << std::hex << std::uppercase << (int)(*cur) << std::dec;
      sout << "_";
    }
    first = false;
    ++cur;
  }
  return Str_O::create(sout.str());
};

/*! Return an a-list containing lots of values that define C++ objects that Clasp needs to know about */
CL_DEFUN core::T_sp llvm_sys__cxxDataStructuresInfo() {
  List_sp list = _Nil<T_O>();
  list = Cons_O::create(Cons_O::create(_sym_tsp, make_fixnum((int)sizeof(T_sp))), _Nil<T_O>());
  list = Cons_O::create(Cons_O::create(_sym_tmv, make_fixnum((int)sizeof(T_mv))), list);
  list = Cons_O::create(Cons_O::create(_sym_invocationHistoryFrame, make_fixnum((int)sizeof(InvocationHistoryFrame))), list);
  list = Cons_O::create(Cons_O::create(_sym_size_t, make_fixnum((int)sizeof(size_t))), list);
  list = Cons_O::create(Cons_O::create(_sym_threadInfo, make_fixnum((int)sizeof(ThreadLocalState))), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("LCC-ARGS-IN-REGISTERS"), make_fixnum((int)sizeof(LCC_ARGS_IN_REGISTERS))), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("FIXNUM-MASK"), make_fixnum((int)gctools::fixnum_mask)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("TAG-MASK"), make_fixnum((int)gctools::tag_mask)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("IMMEDIATE-MASK"), make_fixnum((int)gctools::immediate_mask)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("GENERAL-TAG"), make_fixnum((int)gctools::general_tag)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("FIXNUM-TAG"), make_fixnum((int)gctools::fixnum_tag)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("CONS-TAG"), make_fixnum((int)gctools::cons_tag)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("CHARACTER-TAG"), make_fixnum((int)gctools::character_tag)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("SINGLE-FLOAT-TAG"), make_fixnum((int)gctools::single_float_tag)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("MULTIPLE-VALUES-LIMIT"), make_fixnum((int)MultipleValues::MultipleValuesLimit)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("MULTIPLE-VALUES-SIZEOF"), make_fixnum((int)sizeof(MultipleValues))), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("CONS-CAR-OFFSET"), make_fixnum(core::Cons_O::car_offset())), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("CONS-CDR-OFFSET"), make_fixnum(core::Cons_O::cdr_offset())), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("UINTPTR_T-SIZE"), make_fixnum(sizeof(uintptr_t))), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("VALIST_S-SIZE"), make_fixnum(sizeof(VaList_S))), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("REGISTER-SAVE-AREA-SIZE"), make_fixnum(LCC_TOTAL_REGISTERS*sizeof(void*))), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("ALIGNMENT"),make_fixnum(gctools::Alignment())),list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("VOID*-SIZE"),make_fixnum(sizeof(void*))),list);
#define ENTRY(list, name, code) list = Cons_O::create(Cons_O::create(lisp_internKeyword(#name), code), list)
  LoadTimeValues_O tempLtv;
  ENTRY(list, LOAD - TIME - VALUES - OBJECTS - OFFSET, make_fixnum((char *)&tempLtv._Objects - (char *)&tempLtv));
  ENTRY(list, LOAD - TIME - VALUES - SYMBOLS - OFFSET, make_fixnum((char *)&tempLtv._Symbols - (char *)&tempLtv));
  gc::Vec0<T_sp> tempVec0Tsp;
  ENTRY(list, VEC0 - VECTOR - OFFSET, make_fixnum((char *)&tempVec0Tsp._Vector - (char *)&tempVec0Tsp));
  gc::GCVector_moveable<T_O *> tempGCVector(1, 0);
  ENTRY(list, GCVECTOR - CAPACITY - OFFSET, make_fixnum((char *)&tempGCVector._Capacity - (char *)&tempGCVector));
  ENTRY(list, GCVECTOR - END - OFFSET, make_fixnum((char *)&tempGCVector._End - (char *)&tempGCVector));
  ENTRY(list, GCVECTOR - DATA0 - OFFSET, make_fixnum((char *)&tempGCVector._Data[0] - (char *)&tempGCVector));
  return list;
}

CL_LAMBDA(&key tsp tmv ihf);
CL_DEFUN void llvm_sys__throwIfMismatchedStructureSizes(core::Fixnum_sp tspSize, core::Fixnum_sp tmvSize, gc::Nilable<core::Fixnum_sp> givenIhfSize) {
  int T_sp_size = sizeof(core::T_sp);
  if (unbox_fixnum(tspSize) != T_sp_size) {
    SIMPLE_ERROR(BF("Mismatch between tsp size[%d] and core::T_sp size[%d]") % unbox_fixnum(tspSize) % T_sp_size);
  }
  int T_mv_size = sizeof(core::T_mv);
  if (unbox_fixnum(tmvSize) != T_mv_size) {
    SIMPLE_ERROR(BF("Mismatch between tmv size[%d] and core::T_mv size[%d]") % unbox_fixnum(tmvSize) % T_mv_size);
  }
  int InvocationHistoryFrame_size = sizeof(core::InvocationHistoryFrame);
  if (givenIhfSize.notnilp() && unbox_fixnum(givenIhfSize) != InvocationHistoryFrame_size) {
    SIMPLE_ERROR(BF("Mismatch between IR lisp-compiled-function-ihf size[%d]"
                    " and sizeof(LispCompiledFunctionIHF)=[%d]") %
                 _rep_(givenIhfSize) % InvocationHistoryFrame_size);
  }
};

#if 0
    core::Symbol_sp* getOrCreateMemoryLockedSymbolForLlvm(core::Symbol_sp sym)
    {
	STATIC_ROOT_FRAME_BEGIN(MemoryLockedSymbols) {
	    map<string,core::Symbol_sp*>	_Map;
	    MemoryLockedSymbols() {this->attachToGCRoot();} // attach to MPS
	    GC_SCANNER() {
		GC_SCANNER_BEGIN() {
		for ( map<string,core::Symbol_sp*>::iterator it=this->_Map.begin(); it!=this->_Map.end(); ++it )
		{
		    SMART_PTR_FIX(*(it->second));
		}
		} GC_SCANNER_END();
		return GC_RES_OK;
	    }
	} STATIC_ROOT_FRAME_END(MemoryLockedSymbols,static_lockedSymbols);

	string symbolFullName = sym->fullName();
	map<string,core::Symbol_sp*>::iterator it = static_lockedSymbols->_Map.find(symbolFullName);
	core::Symbol_sp* address = NULL;
	if (it == static_lockedSymbols->_Map.end() )
	{
	    address = new core::Symbol_sp(sym);
	    static_lockedSymbols->_Map[symbolFullName] = address;
	} else
	{
	    address = it->second;
	}
	return address;
    };
#endif

CL_DEFUN llvmo::GlobalVariable_sp llvm_sys__getOrCreateExternalGlobal(llvmo::Module_sp module, const string &name, llvmo::Type_sp data_type) {
  llvm::Module *llvm_module = module->wrappedPtr();
  llvm::Type *llvm_data_type = data_type->wrappedPtr();
  ASSERT(llvm_module != NULL);
  ASSERT(llvm_data_type != NULL);
  llvm::GlobalVariable *global = NULL;
  global = llvm_module->getNamedGlobal(name);
  if (global == NULL) {
    global = new llvm::GlobalVariable(*llvm_module,
                                      llvm_data_type,
                                      true, // isConstant
                                      llvm::GlobalValue::ExternalLinkage,
                                      0, // Initializer
                                      name);
  }
  GlobalVariable_sp gv = RP_Create_wrapped<GlobalVariable_O, llvm::GlobalVariable *>(global);
  return gv;
}

void dump_funcs(core::Function_sp compiledFunction) {
  core::T_sp funcs = compiledFunction->associatedFunctions();
  if (funcs.notnilp()) {
    string outstr;
    llvm::raw_string_ostream sout(outstr);
    if ((funcs).consp()) {
      core::List_sp cfuncs = funcs;
      for (auto cur : cfuncs) {
        core::T_sp func = oCar(cur);
        if (llvmo::Function_sp f = gc::As<llvmo::Function_sp>(func)) {
          f->wrappedPtr()->print(sout);
        } else {
          printf("llvm_sys__disassemble -> %s\n", _rep_(func).c_str());
        }
      }
      core::clasp_write_string(outstr);
      return;
    }
  } else {
    STDOUT_BFORMAT(BF("There were no associated functions available for disassembly\n"));
  }
}

CL_DEFUN void llvm_sys__disassembleSTAR(core::Function_sp cf) {
  dump_funcs(cf);
}

CL_LAMBDA(fn &optional only);
CL_DEFUN void llvm_sys__viewCFG(core::T_sp funcDes, core::T_sp only) {
  core::Function_sp compiledFunction = core::coerce::functionDesignator(funcDes);
  if (auto cl = compiledFunction.asOrNull<core::CompiledClosure_O>()) {
    core::T_sp funcs = cl->associatedFunctions();
    if ((funcs).consp()) {
      core::List_sp cfuncs = funcs;
      for (auto cur : cfuncs) {
        core::T_sp func = oCar(cur);
        if (llvmo::Function_sp f = gc::As<llvmo::Function_sp>(func)) {
          if (only.notnilp()) {
            f->wrappedPtr()->viewCFGOnly();
          } else {
            f->wrappedPtr()->viewCFG();
          }
        }
      }
    }
  } else {
    SIMPLE_ERROR(BF("The function is not a compiled function"));
  }
}

;

void LlvmoExposer_O::expose(core::Lisp_sp lisp, core::Exposer_O::WhatToExpose what) const {
  //
  // Initialize the intrinsic functions in intrinsics.cc
  //

  switch (what) {
  case candoClasses: {
#if 0
#define ALL_STAGES
#define Use_LlvmoPkg
#define INVOKE_REGISTER
#define LOOKUP_SYMBOL(pkg, name) _lisp->internUniqueWithPackageName(pkg, name)
//#include <clasp/core/initClasses.h>
#undef LOOKUP_SYMBOL
#undef INVOKE_REGISTER
#undef Use_LlvmoPkg
#undef ALL_STAGES
#endif
  } break;
  case candoFunctions: {
    SYMBOL_EXPORT_SC_(LlvmoPkg, getOrCreateExternalGlobal);
//    Defun(getOrCreateExternalGlobal);
    SYMBOL_EXPORT_SC_(LlvmoPkg, disassembleSTAR);
//    Defun(disassembleSTAR);
    SYMBOL_EXPORT_SC_(LlvmoPkg, throwIfMismatchedStructureSizes);
//    Defun(throwIfMismatchedStructureSizes);
//    Defun(cxxDataStructuresInfo);
//    Defun(mangleSymbolName);
//    Defun(viewCFG);
    //nothing
  };
      break;
  case candoGlobals: {
    initialize_intrinsics(); //<< comment this out - symbols disappear
    initialize_link_intrinsics();
    initialize_llvmo_expose();
    initialize_clbind_llvm_expose();
    initialize_dwarf_constants();
    SYMBOL_EXPORT_SC_(LlvmoPkg, _PLUS_globalBootFunctionsName_PLUS_);
    SYMBOL_EXPORT_SC_(LlvmoPkg, _PLUS_globalEpilogueName_PLUS_);
  };
      break;
  case pythonClasses:
  case pythonFunctions:
  case pythonGlobals: {
    IMPLEMENT_ME();
  } break;
  }
}
};

#ifdef USE_MPS
//
// Include the Kinds
//
#ifndef RUNNING_GC_BUILDER
#define NAMESPACE_llvmo
#include "clasp_gc.cc"
#undef NAMESPACE_llvmo
#endif
#endif

