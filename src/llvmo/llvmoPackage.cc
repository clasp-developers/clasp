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
#include <csetjmp>
#include <regex>

#include <llvm/Support/raw_ostream.h>
#include "llvm/Support/InitLLVM.h"
#include "llvm/InitializePasses.h"
#if 0
#undef NDEBUG
#include "llvm/Support/Debug.h"
#endif

#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/lispStream.fwd.h>
#include <clasp/core/designators.h>
#include <clasp/llvmo/llvmoPackage.h>
// #include "llvmoExpose.generated.h"
#include <clasp/gctools/gcFunctions.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/llvmo/jit.h>
#include <clasp/llvmo/insertPoint.h>
#include <clasp/llvmo/debugLoc.h>
#include <clasp/llvmo/llvmoDwarf.h>
#include <clasp/llvmo/clbindLlvmExpose.h>
#include <clasp/llvmo/debugInfoExpose.h>
#include <clasp/llvmo/intrinsics.h>
#include <clasp/llvmo/claspLinkPass.h>
#include <clasp/core/bytecode.h>
#include <clasp/core/instance.h>
#include <clasp/core/funcallableInstance.h>
#include <clasp/core/pathname.h>
#include <clasp/core/compiler.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/loadTimeValues.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/unixfsys.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/array.h>
#include <clasp/core/wrappers.h>
#include <clasp/core/unwind.h> // sizeof dynenvs

using namespace core;

//
// Load the gctools::GcInfo<core-classes>::Kind specializers
//
#define NAMESPACE_llvmo
#include <clasp/gctools/gc_interface.h>
#undef NAMESPACE_llvmo

namespace llvmo {

SYMBOL_EXPORT_SC_(LlvmoPkg, STARdebugObjectFilesSTAR);
SYMBOL_EXPORT_SC_(LlvmoPkg, STARdefault_code_modelSTAR);
SYMBOL_EXPORT_SC_(LlvmoPkg, STARdumpObjectFilesSTAR);
SYMBOL_EXPORT_SC_(LlvmoPkg, STARrunTimeExecutionEngineSTAR);
SYMBOL_EXPORT_SC_(LlvmoPkg, load_bc);
SYMBOL_EXPORT_SC_(LlvmoPkg, load_ll);
SYMBOL_SHADOW_EXPORT_SC_(LlvmoPkg, function);
SYMBOL_SHADOW_EXPORT_SC_(LlvmoPkg, type);
SYMBOL_SHADOW_EXPORT_SC_(LlvmoPkg, min);
SYMBOL_SHADOW_EXPORT_SC_(LlvmoPkg, max);
SYMBOL_SHADOW_EXPORT_SC_(LlvmoPkg, and);
SYMBOL_SHADOW_EXPORT_SC_(LlvmoPkg, or);

void redirect_llvm_interface_addSymbol() {
  //	llvm_interface::addSymbol = &addSymbolAsGlobal;
}

CL_DOCSTRING(R"dx(Load an llvm-ir file with either a bc extension or ll extension.)dx");
CL_LAMBDA(pathname &optional verbose print external_format (startup-id 0));
DOCGROUP(clasp);
CL_DEFUN bool llvm_sys__load_ir(core::T_sp filename, bool verbose, bool print, core::T_sp externalFormat, size_t startup_name) {
  core::Pathname_sp pfilename = core::cl__pathname(filename);
  core::Pathname_sp ll_file = core::Pathname_O::makePathname(nil<core::T_O>(), nil<core::T_O>(), nil<core::T_O>(), nil<core::T_O>(),
                                                             core::SimpleBaseString_O::make("ll"));
  ll_file = cl__merge_pathnames(ll_file, pfilename);
  T_sp found = cl__probe_file(ll_file);
  if (found.notnilp()) {
    core::clasp_write_string(fmt::format("Loading ll file {}\n", _rep_(ll_file)));
    return llvm_sys__load_ll(ll_file, verbose, print, externalFormat, startup_name);
  }
  core::Pathname_sp bc_file = core::Pathname_O::makePathname(nil<core::T_O>(), nil<core::T_O>(), nil<core::T_O>(), nil<core::T_O>(),
                                                             core::SimpleBaseString_O::make("bc"));
  bc_file = cl__merge_pathnames(bc_file, pfilename);
  found = cl__probe_file(bc_file);
  if (found.notnilp()) {
    core::clasp_write_string(fmt::format("Loading bc file {}\n", _rep_(bc_file)));
    return llvm_sys__load_bc(bc_file, verbose, print, externalFormat, startup_name);
  }
  SIMPLE_ERROR("Could not find llvm-ir file {} with .bc or .ll extension", _rep_(filename));
}

JITDylib_sp loadModule(llvmo::Module_sp module, size_t startupID, const std::string& libname) {
  ClaspJIT_sp jit = llvm_sys__clasp_jit();
  JITDylib_sp jitDylib = jit->createAndRegisterJITDylib(libname);
  //  printf("%s:%d:%s jit = %p  jitDylib = %p\n", __FILE__, __LINE__, __FUNCTION__, jit.raw_(), jitDylib.raw_() );
  ThreadSafeContext_sp tsc = gc::As<ThreadSafeContext_sp>(comp::_sym_STARthread_safe_contextSTAR->symbolValue());
  std::vector<std::string> startup_functions;
  for (auto& F : *module->wrappedPtr()) {
    std::string function_name = F.getName().str();
    // printf("%s:%d Function: %s looking for %s\n", __FILE__, __LINE__, function_name.c_str(), clasp_startup_FUNCTION_NAME);
    if (function_name.find(clasp_startup_FUNCTION_NAME) != std::string::npos) {
      // printf("%s:%d !!!!!        Function: %s found %s\n", __FILE__, __LINE__, function_name.c_str(),
      // clasp_startup_FUNCTION_NAME);
      startup_functions.push_back(function_name);
    }
  }
  jit->addIRModule(jitDylib, module, tsc, startupID);
  //
  //
  for (auto name : startup_functions) {
    //    printf("%s:%d Startup function: %s\n", __FILE__, __LINE__, name.c_str());
    core::Pointer_sp ptr = jit->lookup(jitDylib, name);
    voidStartUp startup = (voidStartUp)ptr->ptr();
    //    printf("%s:%d      ptr->%p\n", __FILE__, __LINE__, startup);
    (startup)();
  }
  [[maybe_unused]] size_t num = core::startup_functions_are_waiting();
  //  printf("%s:%d There are %lu startup functions waiting to be evaluated\n", __FILE__, __LINE__, num);
  core::startup_functions_invoke(NULL);
  //  printf("%s:%d Invoked startup functions - continuing\n", __FILE__, __LINE__ );
  return jitDylib;
}

CL_LAMBDA(filename &optional verbose print external_format (startup-id 0));
DOCGROUP(clasp);
CL_DEFUN bool llvm_sys__load_ll(core::Pathname_sp filename, bool verbose, bool print, core::T_sp externalFormat, size_t startupID) {
  core::DynamicScopeManager scope(::cl::_sym_STARpackageSTAR, ::cl::_sym_STARpackageSTAR->symbolValue());
  T_sp tn = cl__truename(filename);
  if (tn.nilp()) {
    SIMPLE_ERROR("Could not get truename for {}", _rep_(filename));
  }
  core::T_sp tnamestring = cl__namestring(filename);
  if (tnamestring.nilp()) {
    SIMPLE_ERROR("Could not create namestring for {}", _rep_(filename));
  }
  core::String_sp namestring = gctools::As<core::String_sp>(tnamestring);
  LLVMContext_sp context = llvm_sys__thread_local_llvm_context();
  Module_sp m = llvm_sys__parseIRFile(namestring, context);
  loadModule(m, startupID, namestring->get_std_string());
  return true;
}

CL_LAMBDA(filename &optional verbose print external_format (startup-id 0));
DOCGROUP(clasp);
CL_DEFUN bool llvm_sys__load_bc(core::Pathname_sp filename, bool verbose, bool print, core::T_sp externalFormat, size_t startupID) {
  core::DynamicScopeManager scope(::cl::_sym_STARpackageSTAR, ::cl::_sym_STARpackageSTAR->symbolValue());
  T_sp tn = cl__truename(filename);
  if (tn.nilp()) {
    SIMPLE_ERROR("Could not get truename for {}", _rep_(filename));
  }
  core::T_sp tnamestring = cl__namestring(filename);
  if (tnamestring.nilp()) {
    SIMPLE_ERROR("Could not create namestring for {}", _rep_(filename));
  }
  core::String_sp namestring = gctools::As<core::String_sp>(tnamestring);
  LLVMContext_sp context = llvm_sys__thread_local_llvm_context();
  Module_sp m = llvm_sys__parseBitcodeFile(namestring, context);
  loadModule(m, startupID, namestring->get_std_string());
  return true;
}

CL_DOCSTRING(R"dx(Load a module into the Common Lisp environment as if it were loaded from a bitcode file)dx");

DOCGROUP(clasp);
CL_DEFUN core::SimpleBaseString_sp llvm_sys__mangleSymbolName(core::String_sp name) {
  ASSERT(cl__stringp(name));
  stringstream sout;
  std::string ssout = sout.str();
  const char* cur = ssout.c_str();
  bool first = true;
  while (*cur) {
    if (((*cur) >= 'a' && (*cur) <= 'z') || ((*cur) >= 'A' && (*cur) <= 'Z') || ((*cur) == '_') ||
        (!first && ((*cur) >= '0' && (*cur) <= '9'))) {
      sout << (*cur);
    } else {
      sout << "_";
      sout << std::hex << std::uppercase << (int)(*cur) << std::dec;
      sout << "_";
    }
    first = false;
    ++cur;
  }
  return SimpleBaseString_O::make(sout.str());
};

SYMBOL_EXPORT_SC_(KeywordPkg, fixnum_tag);
SYMBOL_EXPORT_SC_(KeywordPkg, character_tag);
SYMBOL_EXPORT_SC_(KeywordPkg, single_float_tag);
SYMBOL_EXPORT_SC_(KeywordPkg, cons_tag);

DOCGROUP(clasp);
CL_DEFUN core::T_sp llvm_sys__tag_tests() {
  ql::list l;
  l << core::Cons_O::createList(
      kw::_sym_fixnum_tag,
      core::make_fixnum(gctools::STAMPWTAG_FIXNUM << (gctools::BaseHeader_s::general_mtag_shift - gctools::fixnum_shift)),
      core::make_fixnum(FIXNUM_TEST), core::_sym_fixnump);
  l << core::Cons_O::createList(
      kw::_sym_single_float_tag,
      core::make_fixnum(gctools::STAMPWTAG_SINGLE_FLOAT << (gctools::BaseHeader_s::general_mtag_shift - gctools::fixnum_shift)),
      core::make_fixnum(SINGLE_FLOAT_TEST), core::_sym_single_float_p);
  l << core::Cons_O::createList(
      kw::_sym_character_tag,
      core::make_fixnum(gctools::STAMPWTAG_CHARACTER << (gctools::BaseHeader_s::general_mtag_shift - gctools::fixnum_shift)),
      core::make_fixnum(CHARACTER_TEST), ::cl::_sym_characterp);
  l << core::Cons_O::createList(
      kw::_sym_cons_tag,
      core::make_fixnum(gctools::STAMPWTAG_CONS << (gctools::BaseHeader_s::general_mtag_shift - gctools::fixnum_shift)),
      core::make_fixnum(CONS_TEST), ::cl::_sym_consp);
  return l.cons();
}

/*! Return an a-list containing lots of values that define C++ objects that Clasp needs to know about */
DOCGROUP(clasp);
CL_DEFUN core::T_sp llvm_sys__cxxDataStructuresInfo() {
  List_sp list = nil<T_O>();
  list = Cons_O::create(Cons_O::create(_sym_tsp, make_fixnum((int)sizeof(T_sp))), nil<T_O>());
  list = Cons_O::create(Cons_O::create(_sym_tmv, make_fixnum((int)sizeof(T_mv))), list);
  list = Cons_O::create(Cons_O::create(_sym_size_t, make_fixnum((int)sizeof(size_t))), list);
  list = Cons_O::create(Cons_O::create(_sym_threadInfo, make_fixnum((int)sizeof(ThreadLocalState))), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("ALIGNMENT"), make_fixnum((int)gctools::Alignment())), list);
  //  list = Cons_O::create(Cons_O::create(lisp_internKeyword("VALUE-FRAME-PARENT-OFFSET"),
  //  make_fixnum((int)offsetof(core::ValueFrame_O,_Parent))),list); list =
  //  Cons_O::create(Cons_O::create(lisp_internKeyword("VALUE-FRAME-ELEMENT0-OFFSET"),
  //  make_fixnum((int)offsetof(core::ValueFrame_O,_Objects._Data[0]))),list); list =
  //  Cons_O::create(Cons_O::create(lisp_internKeyword("VALUE-FRAME-ELEMENT-SIZE"),
  //  make_fixnum((int)sizeof(core::ValueFrame_O::value_type))),list);
  list = Cons_O::create(
      Cons_O::create(lisp_internKeyword("LCC-ARGS-IN-REGISTERS"), make_fixnum((int)sizeof(LCC_ARGS_IN_REGISTERS))), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("PTAG-MASK"), make_fixnum((int)gctools::ptag_mask)), list);

  list = Cons_O::create(Cons_O::create(lisp_internKeyword("MTAG-MASK"), make_fixnum((int)gctools::Header_s::mtag_mask)), list);
  list = Cons_O::create(
      Cons_O::create(lisp_internKeyword("DERIVABLE-WTAG"), make_fixnum((int)gctools::Header_s::Header_s::derivable_wtag)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("RACK-WTAG"), make_fixnum((int)gctools::Header_s::Header_s::rack_wtag)),
                        list);
  list = Cons_O::create(
      Cons_O::create(lisp_internKeyword("WRAPPED-WTAG"), make_fixnum((int)gctools::Header_s::Header_s::wrapped_wtag)), list);
  list = Cons_O::create(
      Cons_O::create(lisp_internKeyword("HEADER-WTAG"), make_fixnum((int)gctools::Header_s::Header_s::header_wtag)), list);
  list =
      Cons_O::create(Cons_O::create(lisp_internKeyword("MAX-WTAG"), make_fixnum((int)gctools::Header_s::Header_s::max_wtag)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("WTAG-WIDTH"), make_fixnum((int)gctools::Header_s::Header_s::wtag_width)),
                        list);
  list = Cons_O::create(
      Cons_O::create(lisp_internKeyword("GENERAL-MTAG-WIDTH"), make_fixnum((int)gctools::Header_s::Header_s::general_mtag_width)),
      list);

  list = Cons_O::create(Cons_O::create(lisp_internKeyword("IMMEDIATE-MASK"), make_fixnum((int)gctools::immediate_mask)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("GENERAL-TAG"), make_fixnum((int)gctools::general_tag)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("FIXNUM-MASK"), make_fixnum((int)gctools::fixnum_mask)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("TAG-BITS"), make_fixnum((int)TAG_BITS)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("FIXNUM00-TAG"), make_fixnum((int)gctools::fixnum00_tag)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("FIXNUM01-TAG"), make_fixnum((int)gctools::fixnum01_tag)), list);
#if TAG_BITS == 4
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("FIXNUM10-TAG"), make_fixnum((int)gctools::fixnum10_tag)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("FIXNUM11-TAG"), make_fixnum((int)gctools::fixnum11_tag)), list);
#endif
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("CONS-TAG"), make_fixnum((int)gctools::cons_tag)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("VASLIST0-TAG"), make_fixnum((int)gctools::vaslist0_tag)), list);
#if TAG_BITS == 4
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("VASLIST1-TAG"), make_fixnum((int)gctools::vaslist1_tag)), list);
  list =
      Cons_O::create(Cons_O::create(lisp_internKeyword("VASLIST-PTAG-MASK"), make_fixnum((int)gctools::vaslist_ptag_mask)), list);
#endif
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("CHARACTER-TAG"), make_fixnum((int)gctools::character_tag)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("SINGLE-FLOAT-TAG"), make_fixnum((int)gctools::single_float_tag)), list);
  list =
      Cons_O::create(Cons_O::create(lisp_internKeyword("SINGLE-FLOAT-SHIFT"), make_fixnum((int)gctools::single_float_shift)), list);
  list = Cons_O::create(
      Cons_O::create(lisp_internKeyword("MULTIPLE-VALUES-LIMIT"), make_fixnum((int)MultipleValues::MultipleValuesLimit)), list);
  list =
      Cons_O::create(Cons_O::create(lisp_internKeyword("MULTIPLE-VALUES-SIZEOF"), make_fixnum((int)sizeof(MultipleValues))), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("CONS-CAR-OFFSET"), make_fixnum(core::Cons_O::car_offset())), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("CONS-CDR-OFFSET"), make_fixnum(core::Cons_O::cdr_offset())), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("UINTPTR_T-SIZE"), make_fixnum(sizeof(uintptr_t))), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("VASLIST-SIZE"), make_fixnum(sizeof(Vaslist))), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("VASLIST-ALIGNMENT"), make_fixnum(VASLIST_ALIGNMENT)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("VASLIST-ARGS-OFFSET"), make_fixnum((int)Vaslist::args_offset())), list);
  list =
      Cons_O::create(Cons_O::create(lisp_internKeyword("VASLIST-NARGS-OFFSET"), make_fixnum((int)Vaslist::nargs_offset())), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("VASLIST-NARGS-DECREMENT"), make_fixnum((int)Vaslist::NargsDecrement)),
                        list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("VASLIST-NARGS-MASK"), make_fixnum((int)Vaslist::NargsMask)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("VASLIST-NARGS-SHIFT"), make_fixnum((int)Vaslist::NargsShift)), list);

  list = Cons_O::create(Cons_O::create(lisp_internKeyword("HEADER-SIZE"), make_fixnum(sizeof(gctools::Header_s))), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("HEADER-STAMP-OFFSET"),
                                       make_fixnum(offsetof(gctools::Header_s, _badge_stamp_wtag_mtag._value))),
                        list);
  list =
      Cons_O::create(Cons_O::create(lisp_internKeyword("HEADER-STAMP-SIZE"), make_fixnum(sizeof(gctools::tagged_stamp_t))), list);
  list = Cons_O::create(
      Cons_O::create(lisp_internKeyword("REGISTER-SAVE-AREA-SIZE"), make_fixnum(LCC_TOTAL_REGISTERS * sizeof(void*))), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("ALIGNMENT"), make_fixnum(gctools::Alignment())), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("VOID*-SIZE"), make_fixnum(sizeof(void*))), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("JMP-BUF-SIZE"), make_fixnum(sizeof(jmp_buf))), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("UNWIND-PROTECT-DYNENV-SIZE"),
                                       make_fixnum(gctools::sizeof_with_header<UnwindProtectDynEnv_O>())),
                        list);
  list = Cons_O::create(
      Cons_O::create(lisp_internKeyword("BINDING-DYNENV-SIZE"), make_fixnum(gctools::sizeof_with_header<BindingDynEnv_O>())), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("CONS-SIZE"), make_fixnum(gctools::sizeof_with_header<Cons_O>())), list);
  list = Cons_O::create(
      Cons_O::create(lisp_internKeyword("CLOSURE-ENTRY-POINT-OFFSET"), make_fixnum(offsetof(core::Function_O, _TheSimpleFun))),
      list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("SIMPLE-FUN-ENTRY-POINTS-OFFSET"),
                                       make_fixnum(offsetof(core::SimpleFun_O, _EntryPoints))),
                        list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("SIZE_T-BITS"), make_fixnum(sizeof(size_t) * 8)), list);
#define ENTRY(list, name, code) list = Cons_O::create(Cons_O::create(lisp_internKeyword(name), code), list)
  LoadTimeValues_O tempLtv;
  ENTRY(list, "LOAD-TIME-VALUES-OBJECTS-OFFSET", make_fixnum((char*)&tempLtv._Objects - (char*)&tempLtv));
  gc::Vec0<T_sp> tempVec0Tsp;
  ENTRY(list, "VEC0-VECTOR-OFFSET", make_fixnum((char*)&tempVec0Tsp._Vector - (char*)&tempVec0Tsp));
  gc::GCVector_moveable<T_O*> tempGCVector(1, 0);
  ENTRY(list, "GCVECTOR-CAPACITY-OFFSET", make_fixnum((char*)&tempGCVector._Capacity - (char*)&tempGCVector));
  ENTRY(list, "GCVECTOR-END-OFFSET", make_fixnum((char*)&tempGCVector._End - (char*)&tempGCVector));
  ENTRY(list, "GCVECTOR-DATA0-OFFSET", make_fixnum((char*)&tempGCVector._Data[0] - (char*)&tempGCVector));
  ENTRY(list, "OPTIMIZED-SLOT-INDEX-INDEX", make_fixnum(OPTIMIZED_SLOT_INDEX_INDEX));
  ENTRY(list, "CLASS-REP-STAMP", make_fixnum(gctools::STAMPWTAG_CLASS_REP));
  ENTRY(list, "UNUSED-STAMP", make_fixnum(gctools::STAMPWTAG_UNUSED));
  ENTRY(list, "FIXNUM-STAMP", make_fixnum(gctools::STAMPWTAG_FIXNUM));
  ENTRY(list, "FIXNUM-SHIFT", make_fixnum(gctools::fixnum_shift));
  ENTRY(list, "CHARACTER-SHIFT", make_fixnum(gctools::character_shift));
  ENTRY(list, "CHARACTER-TAG", make_fixnum(gctools::character_tag));
  ENTRY(list, "C++-STAMP-MAX", make_fixnum(gctools::STAMPWTAG_max));
  ENTRY(list, "WHERE-TAG-MASK", make_fixnum(gctools::Header_s::where_mask));
  ENTRY(list, "DERIVABLE-WHERE-TAG", make_fixnum(gctools::Header_s::derivable_wtag));
  ENTRY(list, "RACK-WHERE-TAG", make_fixnum(gctools::Header_s::rack_wtag));
  ENTRY(list, "WRAPPED-WHERE-TAG", make_fixnum(gctools::Header_s::wrapped_wtag));
  ENTRY(list, "HEADER-WHERE-TAG", make_fixnum(gctools::Header_s::header_wtag));
  ENTRY(list, "WHERE-TAG-WIDTH", make_fixnum(gctools::Header_s::where_tag_width));
  ENTRY(list, "C++-STAMP-MAX",
        make_fixnum(gctools::STAMPWTAG_max << (gctools::BaseHeader_s::general_mtag_shift - gctools::fixnum_shift)));
  ENTRY(list, "CONS-STAMP", make_fixnum(gctools::STAMPWTAG_CONS));
  ENTRY(list, "VASLIST_S-STAMP", make_fixnum(gctools::STAMPWTAG_VASLIST_S));
  ENTRY(list, "CHARACTER-STAMP", make_fixnum(gctools::STAMPWTAG_CHARACTER));
  ENTRY(list, "SINGLE-FLOAT-STAMP", make_fixnum(gctools::STAMPWTAG_SINGLE_FLOAT));
  ENTRY(list, "INSTANCE-RACK-OFFSET", make_fixnum(offsetof(Instance_O, _Rack)));
  ENTRY(list, "INSTANCE-RACK-STAMP-OFFSET", make_fixnum(Instance_O::rack_stamp_offset()));
  ENTRY(list, "INSTANCE-STAMP", make_fixnum(static_cast<Fixnum>(gctools::STAMPWTAG_INSTANCE)));
  ENTRY(list, "WRAPPED-POINTER-STAMP", make_fixnum(static_cast<Fixnum>(gctools::STAMPWTAG_WRAPPED_POINTER)));
  ENTRY(list, "DERIVABLE-STAMP", make_fixnum(static_cast<Fixnum>(gctools::STAMPWTAG_DERIVABLE)));
  ENTRY(list, "FUNCALLABLE-INSTANCE-STAMP", make_fixnum(static_cast<Fixnum>(gctools::STAMPWTAG_FUNCALLABLE_INSTANCE)));
  ENTRY(list, "LITERAL-TAG-CHAR-CODE", make_fixnum(static_cast<Fixnum>(LITERAL_TAG_CHAR)));
  //  ENTRY(list, "CLASS-KIND", make_fixnum(static_cast<Fixnum>(gctools::STAMPWTAG_CLASS)));
  ENTRY(list, "SIMPLE-VECTOR._DATA-OFFSET",
        make_fixnum(offsetof(SimpleVector_O, _Data) + offsetof(SimpleVector_O::vector_type, _Data)));
  ENTRY(list, "SIMPLE-VECTOR._LENGTH-OFFSET",
        make_fixnum(offsetof(SimpleVector_O, _Data) + offsetof(SimpleVector_O::vector_type, _MaybeSignedLength)));
  ENTRY(list, "ENTRY-POINT-ARITY-BEGIN", make_fixnum(ENTRY_POINT_ARITY_BEGIN));
  ENTRY(list, "ENTRY-POINT-ARITY-END", make_fixnum(ENTRY_POINT_ARITY_END));
  ENTRY(list, "NUMBER-OF-ENTRY-POINTS", make_fixnum(NUMBER_OF_ENTRY_POINTS));
  ENTRY(list, "ENDIAN-LSB-OFFSET", make_fixnum(ENDIAN_LSB_OFFSET));
  ENTRY(list, "UNBOUND-MASK", make_fixnum(UNBOUND_MASK));
  ENTRY(list, "UNBOUND-BYTE", make_fixnum(UNBOUND_BYTE));
  return list;
}

CL_LAMBDA(&key tsp tmv symbol symbol-function-offset symbol-setf-function-offset function function-description-offset gcroots-in-module vaslist function-description);
DOCGROUP(clasp);
CL_DEFUN void llvm_sys__throwIfMismatchedStructureSizes(core::Fixnum_sp tspSize, core::Fixnum_sp tmvSize,
                                                        core::Fixnum_sp symbolSize, core::Fixnum_sp symbol_function_offset,
                                                        core::Fixnum_sp symbol_setf_function_offset, core::Fixnum_sp functionSize,
                                                        core::Fixnum_sp function_description_offset, core::T_sp gcRootsInModuleSize,
                                                        core::T_sp tvaslistsize, core::T_sp tFunctionDescriptionSize) {
  int T_sp_size = sizeof(core::T_sp);
  if (unbox_fixnum(tspSize) != T_sp_size) {
    SIMPLE_ERROR("Mismatch between tsp size[{}] and core::T_sp size[{}]", unbox_fixnum(tspSize), T_sp_size);
  }
  int T_mv_size = sizeof(core::T_mv);
  if (unbox_fixnum(tmvSize) != T_mv_size) {
    SIMPLE_ERROR("Mismatch between tmv size[{}] and core::T_mv size[{}]", unbox_fixnum(tmvSize), T_mv_size);
  }
  int Symbol_O_size = sizeof(core::Symbol_O);
  if (unbox_fixnum(symbolSize) != Symbol_O_size) {
    SIMPLE_ERROR("Mismatch between symbol size[{}] and core::Symbol_O size[{}]", unbox_fixnum(symbolSize), Symbol_O_size);
  }
  if (symbol_function_offset.unsafe_fixnum() != offsetof(core::Symbol_O, _Function)) {
    SIMPLE_ERROR("Mismatch between symbol function offset[{}] and core::Symbol_O._Function offset[{}]",
                 symbol_function_offset.unsafe_fixnum(), offsetof(core::Symbol_O, _Function));
  }
  if (symbol_setf_function_offset.unsafe_fixnum() != offsetof(core::Symbol_O, _SetfFunction)) {
    SIMPLE_ERROR("Mismatch between symbol setf function offset[{}] and core::Symbol_O._SetfFunction offset[{}]",
                 symbol_setf_function_offset.unsafe_fixnum(), offsetof(core::Symbol_O, _SetfFunction));
  }

  int Function_O_size = sizeof(core::Function_O);
  if (unbox_fixnum(functionSize) != Function_O_size) {
    SIMPLE_ERROR("Mismatch between function size[{}] and core::Function_O size[{}]", unbox_fixnum(functionSize), Function_O_size);
  }
  if (function_description_offset.unsafe_fixnum() != offsetof(core::SimpleFun_O, _FunctionDescription)) {
    SIMPLE_ERROR("Mismatch between function description offset[{}] and core::SimpleFun_O._FunctionDescription offset[{}]",
                 function_description_offset.unsafe_fixnum(), offsetof(core::SimpleFun_O, _FunctionDescription));
  }
  if (gcRootsInModuleSize.notnilp()) {
    int gcRootsInModule_size = sizeof(gctools::GCRootsInModule);
    if (gcRootsInModuleSize.fixnump()) {
      if (gcRootsInModule_size != gcRootsInModuleSize.unsafe_fixnum()) {
        SIMPLE_ERROR("GCRootsInModule size {} mismatch with Common Lisp code {}", gcRootsInModule_size,
                     gcRootsInModuleSize.unsafe_fixnum());
      }
    } else {
      SIMPLE_ERROR("gcRootsInModule keyword argument expects a fixnum");
    }
  }
  if (tvaslistsize.fixnump()) {
    size_t vaslistsize = tvaslistsize.unsafe_fixnum();
    if (vaslistsize != sizeof(Vaslist)) {
      SIMPLE_ERROR("Vaslist size {} mismatch with Common Lisp code {}", sizeof(Vaslist), vaslistsize);
    }
  }
  if (tFunctionDescriptionSize.fixnump()) {
    size_t functionDescriptionSize = tFunctionDescriptionSize.unsafe_fixnum();
    //    printf("%s:%d:%s Checked function-description size\n", __FILE__, __LINE__, __FUNCTION__ );
    if (functionDescriptionSize != sizeof(core::FunctionDescription_O)) {
      SIMPLE_ERROR("function-description size {} mismatch with Common Lisp code {}", sizeof(core::FunctionDescription_O),
                   functionDescriptionSize);
    }
  }
}

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

DOCGROUP(clasp);
CL_DEFUN llvmo::GlobalVariable_sp llvm_sys__getOrCreateExternalGlobal(llvmo::Module_sp module, const string& name,
                                                                      llvmo::Type_sp data_type,
                                                                      llvm::GlobalValue::LinkageTypes linkage) {
  llvm::Module* llvm_module = module->wrappedPtr();
  llvm::Type* llvm_data_type = data_type->wrappedPtr();
  ASSERT(llvm_module != NULL);
  ASSERT(llvm_data_type != NULL);
  llvm::GlobalVariable* global = NULL;
  global = llvm_module->getNamedGlobal(name);
  if (global == NULL) {
    global = new llvm::GlobalVariable(*llvm_module, llvm_data_type,
                                      true, // isConstant
                                      linkage,
                                      0, // Initializer
                                      name);
  }
  GlobalVariable_sp gv = RP_Create_wrapped<GlobalVariable_O, llvm::GlobalVariable*>(global);
  return gv;
}

#if 0
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
#endif

CL_LAMBDA(module &optional (stream t));
DOCGROUP(clasp);
CL_DEFUN void dump_module(Module_sp module, core::T_sp tstream) {
  core::T_sp stream = core::coerce::outputStreamDesignator(tstream);
  string outstr;
  llvm::raw_string_ostream sout(outstr);
  module->wrappedPtr()->print(sout, NULL);
  core::clasp_write_string(sout.str(), stream);
}

CL_LAMBDA(func &optional (stream t));
DOCGROUP(clasp);
CL_DEFUN void dump_function(Function_sp function, core::T_sp tstream) {
  core::T_sp stream = core::coerce::outputStreamDesignator(tstream);
  string outstr;
  llvm::raw_string_ostream sout(outstr);
  function->wrappedPtr()->print(sout, NULL);
  core::clasp_write_string(outstr, stream);
}

CL_LAMBDA(fn &optional only);
DOCGROUP(clasp);
CL_DEFUN void llvm_sys__viewCFG(core::T_sp funcs, core::T_sp only) {
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

DOCGROUP(clasp);
CL_DEFUN ClaspJIT_sp llvm_sys__clasp_jit() {
  Lisp::GCRoots* roots = &_lisp->_Roots;
  //  printf("%s:%d:%s Getting _ClaspJIT roots %p  roots->_ClaspJIT %p nilp %d\n", __FILE__, __LINE__, __FUNCTION__, roots,
  //  roots->_ClaspJIT.raw_(), roots->_ClaspJIT.nilp());
  return gc::As_unsafe<ClaspJIT_sp>(roots->_ClaspJIT);
}

void initialize_llvm() {
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();
  llvm::initializeScalarOpts(*llvm::PassRegistry::getPassRegistry());
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllDisassemblers();
};

void initialize_ClaspJIT() {
  // printf("%s:%d:%s About to set _ClaspJIT\n", __FILE__, __LINE__, __FUNCTION__ );
  auto jit_engine = gctools::GC<ClaspJIT_O>::allocate();
  jit_engine->installMainJITDylib();
  _lisp->_Roots._ClaspJIT = jit_engine;
}

void LlvmoExposer_O::expose(core::LispPtr lisp, core::Exposer_O::WhatToExpose what) const {
  //
  // Initialize the intrinsic functions in intrinsics.cc
  //

  switch (what) {
  case candoClasses: {
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
    // nothing
  }; break;
  case candoGlobals: {
    initialize_llvm();
    initialize_ClaspJIT();
    initialize_llvmo_expose();
    initialize_clbind_llvm_expose();
    initialize_dwarf_constants();
#ifdef USE_JITLINKER
#error "Define a different code-model"
#else
#ifdef _TARGET_OS_DARWIN
    // This is from Lang Hames who said on Discord #llvm channel:
    // @drmeister Regarding code models: I would switch your code model and custom linking layer together:
    // If Darwin then use ObjectLinkingLayer and Small, otherwise RTDyldObjectLinkingLayer and Large.
    // Also see llvmoExpose.cc
    //     llvmo::_sym_STARdefault_code_modelSTAR->defparameter(llvmo::_sym_CodeModel_Large);
    llvmo::_sym_STARdefault_code_modelSTAR->defparameter(llvmo::_sym_CodeModel_Small);
#else
    llvmo::_sym_STARdefault_code_modelSTAR->defparameter(llvmo::_sym_CodeModel_Large);
#endif
#endif
    llvmo::_sym_STARdebugObjectFilesSTAR->defparameter(
        gc::As<core::Cons_sp>(::cl::_sym_STARfeaturesSTAR->symbolValue())->memberEq(kw::_sym_debugObjectFiles));
    llvmo::_sym_STARdumpObjectFilesSTAR->defparameter(
        gc::As<core::Cons_sp>(::cl::_sym_STARfeaturesSTAR->symbolValue())->memberEq(kw::_sym_dumpObjectFiles));
    if (llvmo::_sym_STARdebugObjectFilesSTAR->symbolValue().notnilp()) {
      printf("%s:%d:%s llvm-sys:*debugObjectFiles* is true\n", __FILE__, __LINE__, __FUNCTION__);
    }
    if (llvmo::_sym_STARdumpObjectFilesSTAR->symbolValue().notnilp()) {
      printf("%s:%d:%s llvm-sys:*dumpObjectFiles* is true\n", __FILE__, __LINE__, __FUNCTION__);
    }
    SYMBOL_EXPORT_SC_(LlvmoPkg, _PLUS_globalBootFunctionsName_PLUS_);
    SYMBOL_EXPORT_SC_(LlvmoPkg, _PLUS_globalEpilogueName_PLUS_);
  }; break;
  case pythonClasses:
  case pythonFunctions:
  case pythonGlobals: {
    IMPLEMENT_ME();
  } break;
  }
}

/*!
   Install a trampoline that spills registers onto the stack

   The bytecode trampoline passes a PC.
*/

}; // namespace llvmo

extern "C" {
NEVER_OPTIMIZE
gctools::return_type default_bytecode_trampoline(unsigned char* pc, core::T_O* closure, uint64_t nargs, core::T_O** args) {
  return bytecode_call(pc, closure, nargs, args);
}

NEVER_OPTIMIZE
gctools::return_type unknown_bytecode_trampoline(unsigned char* pc, core::T_O* closure, uint64_t nargs, core::T_O** args) {
  return bytecode_call(pc, closure, nargs, args);
}

NEVER_OPTIMIZE
gctools::return_type lambda_nil(unsigned char* pc, core::T_O* closure, uint64_t nargs, core::T_O** args) {
  return bytecode_call(pc, closure, nargs, args);
}
};

namespace llvmo {
#include <trampoline.h>

std::atomic<size_t> global_trampoline_counter;
#ifdef CLASP_THREADS
mp::Mutex* global_trampoline_mutex = NULL;
#endif

string escapeNameForLlvm(const string& inp) {
  stringstream sout;
  stringstream sin(inp);
  char c;
  while (1) {
    sin.get(c);
    if (!sin.good())
      break;
    switch (c) {
    case '"':
      sout << "_";
      break;
    default:
      sout << c;
    }
  };
  return sout.str();
}

CL_DEFUN core::Pointer_mv cmp__compile_trampoline(core::T_sp tname) {
#if !defined(DEFAULT_OUTPUT_TYPE_BYTECODE)
  return Values(Pointer_O::create((void*)bytecode_call), SimpleBaseString_O::make("bytecode_call"));
#endif
  if (global_trampoline_mutex == NULL) {
    global_trampoline_mutex = new mp::Mutex(DISSASSM_NAMEWORD);
  }
  WITH_READ_WRITE_LOCK(*global_trampoline_mutex);
  ClaspJIT_sp jit = llvm_sys__clasp_jit();
  if (!global_options->_GenerateTrampolines) {
    if (!getenv("CLASP_ENABLE_TRAMPOLINES")) {
      // If the JIT isn't ready then use the default trampoline
      return Values(Pointer_O::create((void*)bytecode_call), SimpleBaseString_O::make("bytecode_call"));
    }
  }
  if (jit.nilp()) {
    // If the JIT isn't ready then use the default trampoline
    return Values(Pointer_O::create((void*)default_bytecode_trampoline), SimpleBaseString_O::make("default_bytecode_trampoline"));
  }
  if (tname.consp() && CONS_CAR(tname) == ::cl::_sym_lambda && CONS_CDR(tname).consp() && CONS_CAR(CONS_CDR(tname)).nilp()) {
    return Values(Pointer_O::create((void*)lambda_nil), SimpleBaseString_O::make("lambda_nil"));
  }

  std::string name;
  if (gc::IsA<core::Symbol_sp>(tname)) {
    name = gc::As_unsafe<core::Symbol_sp>(tname)->fullName();
  } else {
    name = _rep_(tname);
    // printf("%s:%d:%s trampoline name = |%s|\n", __FILE__, __LINE__, __FUNCTION__, name.c_str());
    // fflush();
    if (name[0] == '"' && name[name.size() - 1] == '"') {
      if (name.size() < 3) { // matches ""
        return Values(Pointer_O::create((void*)unknown_bytecode_trampoline),
                      SimpleBaseString_O::make("unknown_bytecode_trampoline"));
      }
    }
    name = name.substr(1, name.size() - 2); // Strip double quotes
  }
  name = escapeNameForLlvm(name) + "_bct" + std::to_string(global_trampoline_counter++);
  LLVMContext_sp context = llvm_sys__thread_local_llvm_context();
  std::string trampoline = core::searchAndReplaceString(global_trampoline, "wrapper:name", name);
  Module_sp module = llvm_sys__parseIRString(trampoline, context, "backtrace_trampoline");
  JITDylib_sp jitDylib = loadModule(module, 0, "trampoline");
  core::Pointer_sp bytecode_ptr = jit->lookup(jitDylib, name);
  return Values(bytecode_ptr, SimpleBaseString_O::make(name));
}
}; // namespace llvmo

namespace llvmo {

// Compile callbacks for FFI.
CL_DEFUN JITDylib_sp jit_module_to_dylib(Module_sp module, const std::string& libname) { return loadModule(module, 0, libname); }

CL_DEFUN core::Pointer_sp jit_lookup(JITDylib_sp dylib, const std::string& name) {
  return llvm_sys__clasp_jit()->lookup(dylib, name);
}

// Access a T_sp in a variable.
CL_DEFUN core::T_sp jit_lookup_t(JITDylib_sp dylib, const std::string& name) {
  void* ptr;
  bool found = llvm_sys__clasp_jit()->do_lookup(dylib, name, ptr);
  if (!found)
    SIMPLE_ERROR("Could not find pointer for name |{}|", name);
  core::T_O** tptr = (core::T_O**)ptr;
  T_sp ret((gctools::Tagged)(*tptr));
  return ret;
}

CL_LISPIFY_NAME("llvmo:jit-lookup-t");
CL_DEFUN_SETF core::T_sp setf_jit_lookup_t(core::T_sp value, JITDylib_sp dylib, const std::string& name) {
  void* ptr;
  bool found = llvm_sys__clasp_jit()->do_lookup(dylib, name, ptr);
  if (!found)
    SIMPLE_ERROR("Could not find pointer for name |{}|", name);
  core::T_O** tptr = (core::T_O**)ptr;
  *tptr = value.raw_();
  return value;
}

}; // namespace llvmo

#if defined(USE_MPS) || defined(USE_PRECISE_GC)
//
// Include the Kinds
//
#ifndef RUNNING_PRECISEPREP
#define NAMESPACE_llvmo
#ifndef SCRAPING
#include CLASP_GC_CC
#endif
#undef NAMESPACE_llvmo
#endif
#endif
