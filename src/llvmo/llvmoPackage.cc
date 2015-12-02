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

#include <stdint.h>

#include <clasp/core/foundation.h>
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
#include <clasp/core/environment.h>
#include <clasp/core/str.h>
#include <clasp/core/wrappers.h>

using namespace core;
#if 0
namespace kw {
#pragma GCC visibility push(default)
#define KeywordPkg_SYMBOLS
#define DO_SYMBOL(cname, idx, pkgName, lispName, export) core::Symbol_sp cname;
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
  #endif
#undef DO_SYMBOL
#undef KeywordPkg_SYMBOLS
#pragma GCC visibility pop
};
#endif
namespace llvmo {

#pragma GCC visibility push(default)
#define LlvmoPkg_SYMBOLS
#define DO_SYMBOL(cname, idx, pkgName, lispName, export) core::Symbol_sp cname;
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
  #endif
#undef DO_SYMBOL
#undef LlvmoPkg_SYMBOLS
#pragma GCC visibility pop
};

namespace llvmo {
#define EXPOSE_TO_CANDO
#define Use_LlvmoPkg
#define EXTERN_REGISTER
#include INIT_CLASSES_INC_H
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

CL_NAMESPACE namespace llvmo {

SYMBOL_EXPORT_SC_(LlvmoPkg, STARrunTimeExecutionEngineSTAR);

void redirect_llvm_interface_addSymbol() {
  //	llvm_interface::addSymbol = &addSymbolAsGlobal;
}

#define ARGS_af_mangleSymbolName "(arg)"
#define DECL_af_mangleSymbolName ""
#define DOCS_af_mangleSymbolName "Mangle the LLVM symbol name so that it will be a legal symbol for ld"
Str_sp af_mangleSymbolName(Str_sp name) {
  _G();
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


#define ARGS_af_cxxDataStructuresInfo "()"
#define DECL_af_cxxDataStructuresInfo ""
#define DOCS_af_cxxDataStructuresInfo "cxxDataStructuresInfo: Return an alist of C++ data structure sizes ((name . size-of-in-bytes))"
T_sp af_cxxDataStructuresInfo() {
  List_sp list = _Nil<T_O>();
  list = Cons_O::create(Cons_O::create(_sym_tsp, make_fixnum((int)sizeof(T_sp))), _Nil<T_O>());
  list = Cons_O::create(Cons_O::create(_sym_tmv, make_fixnum((int)sizeof(T_mv))), list);
  list = Cons_O::create(Cons_O::create(_sym_invocationHistoryFrame, make_fixnum((int)sizeof(InvocationHistoryFrame))), list);
  list = Cons_O::create(Cons_O::create(_sym_size_t, make_fixnum((int)sizeof(size_t))), list);
  list = Cons_O::create(Cons_O::create(_sym_threadInfo, make_fixnum((int)sizeof(ThreadInfo))), list);
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

#define ARGS_af_throwIfMismatchedStructureSizes "(&key tsp tmv ihf)"
#define DECL_af_throwIfMismatchedStructureSizes ""
#define DOCS_af_throwIfMismatchedStructureSizes "throwIfMismatchedStructureSizes"
void af_throwIfMismatchedStructureSizes(Fixnum_sp tspSize, Fixnum_sp tmvSize, gc::Nilable<Fixnum_sp> givenIhfSize) {
  _G();
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
#define ARGS_af_memoryLockedSymbolForLlvm "(symbol)"
#define DECL_af_memoryLockedSymbolForLlvm ""
#define DOCS_af_memoryLockedSymbolForLlvm "Lookup or create a boost::shared_ptr<Symbol_O> for a Symbol and return the pointer to it"
    core::Symbol_sp* getOrCreateMemoryLockedSymbolForLlvm(core::Symbol_sp sym)
    {_G();
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

#define ARGS_af_getOrCreateExternalGlobal "(symbol resname shared-ptr-type)"
#define DECL_af_getOrCreateExternalGlobal ""
#define DOCS_af_getOrCreateExternalGlobal "getOrCreateExternalGlobal"
llvmo::GlobalVariable_sp af_getOrCreateExternalGlobal(llvmo::Module_sp module, const string &name, llvmo::Type_sp data_type) {
  _G();
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
  auto cb = compiledFunction->closure;
  if (!cb->compiledP()) {
    SIMPLE_ERROR(BF("You can only disassemble compiled functions"));
  }
  auto cc = cb.as<llvmo::CompiledClosure>();
  core::T_sp funcs = cc->associatedFunctions;
  if (cl_consp(funcs)) {
    core::List_sp cfuncs = funcs;
    for (auto cur : cfuncs) {
      core::T_sp func = oCar(cur);
      if (llvmo::Function_sp f = gc::As<llvmo::Function_sp>(func)) {
        f->dump();
      } else {
        printf("af_disassemble -> %s\n", _rep_(func).c_str());
      }
    }
    return;
  }
  STDOUT_BFORMAT(BF("There were no associated functions available for disassembly\n"));
}

#define ARGS_af_disassembleSTAR "(fn)"
#define DECL_af_disassembleSTAR ""
#define DOCS_af_disassembleSTAR "disassembleSTAR"
void af_disassembleSTAR(core::Function_sp cf) {
  _G();
  dump_funcs(cf);
}

#define ARGS_af_viewCFG "(fn &optional only)"
#define DECL_af_viewCFG ""
#define DOCS_af_viewCFG "viewCFG (view-cfg fn &optional only)"
void af_viewCFG(core::T_sp funcDes, core::T_sp only) {
  core::Function_sp compiledFunction = core::coerce::functionDesignator(funcDes);
  if (auto cl = compiledFunction->closure.as<CompiledClosure>()) {
    core::T_sp funcs = cl->associatedFunctions;
    if (cl_consp(funcs)) {
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
      return;
    }
  }
}

;

void LlvmoExposer::expose(core::Lisp_sp lisp, core::Exposer::WhatToExpose what) const {
  _G();
  //
  // Initialize the intrinsic functions in intrinsics.cc
  //

  switch (what) {
  case candoClasses: {
#define LlvmoPkg_SYMBOLS
#define DO_SYMBOL(cname, idx, pkg, lispname, exportp)          \
  {                                                            \
    cname = _lisp->internUniqueWithPackageName(pkg, core::lispify_symbol_name(lispname)); \
    cname->exportYourself(exportp);                            \
  }
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
  #endif
#undef DO_SYMBOL
#undef LlvmoPkg_SYMBOLS

#define ALL_STAGES
#define Use_LlvmoPkg
#define INVOKE_REGISTER
#define LOOKUP_SYMBOL(pkg, name) _lisp->internUniqueWithPackageName(pkg, name)
#include INIT_CLASSES_INC_H
#undef LOOKUP_SYMBOL
#undef INVOKE_REGISTER
#undef Use_LlvmoPkg
#undef ALL_STAGES
  } break;
  case candoFunctions: {
    SYMBOL_EXPORT_SC_(LlvmoPkg, getOrCreateExternalGlobal);
    Defun(getOrCreateExternalGlobal);
    SYMBOL_EXPORT_SC_(LlvmoPkg, disassembleSTAR);
    Defun(disassembleSTAR);
    SYMBOL_EXPORT_SC_(LlvmoPkg, throwIfMismatchedStructureSizes);
    Defun(throwIfMismatchedStructureSizes);
    Defun(cxxDataStructuresInfo);
    Defun(mangleSymbolName);
    Defun(viewCFG);
    //nothing
  };
      break;
  case candoGlobals: {
    initialize_intrinsics(); //<< comment this out - symbols disappear
    initialize_link_intrinsics();
    initialize_llvmo_expose();
    initialize_clbind_llvm_expose();
    initialize_dwarf_constants();
    initialize_claspLinkPass();
    SYMBOL_EXPORT_SC_(LlvmoPkg, _PLUS_ClaspMainFunctionName_PLUS_);
    SYMBOL_EXPORT_SC_(LlvmoPkg, _PLUS_globalBootFunctionsName_PLUS_);
    SYMBOL_EXPORT_SC_(LlvmoPkg, _PLUS_globalBootFunctionsNameSize_PLUS_);
    _sym__PLUS_ClaspMainFunctionName_PLUS_->defconstant(core::Str_O::create(CLASP_MAIN_FUNCTION_NAME));

    _sym__PLUS_globalBootFunctionsName_PLUS_->defconstant(core::Str_O::create(GLOBAL_BOOT_FUNCTIONS_NAME));
    _sym__PLUS_globalBootFunctionsNameSize_PLUS_->defconstant(core::Str_O::create(GLOBAL_BOOT_FUNCTIONS_SIZE_NAME));

    //	initializeLlvmConstants(_lisp);
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
#include STATIC_ANALYZER_PRODUCT
#undef NAMESPACE_llvmo
#endif
#endif

#if USE_INTRUSIVE_SMART_PTR == 1
#define EXPAND_CLASS_MACROS
#if defined(USE_MPS) // MPS doesn't need INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS
#define _CLASS_MACRO(_U_) \
  STATIC_CLASS_INFO(_U_);
#else
#define _CLASS_MACRO(_U_) \
  STATIC_CLASS_INFO(_U_); \
  INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(_U_)
#endif
#include INIT_CLASSES_INC_H
#undef _CLASS_MACRO
#undef EXPAND_CLASS_MACROS
#endif
