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
//#include "llvmoExpose.generated.h"
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
#include <clasp/core/instance.h>
#include <clasp/core/funcallableInstance.h>
#include <clasp/core/pathname.h>
#include <clasp/core/compiler.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/loadTimeValues.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/unixfsys.h>
#include <clasp/core/environment.h>
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

SYMBOL_SHADOW_EXPORT_SC_(LlvmoPkg, function);
SYMBOL_EXPORT_SC_(LlvmoPkg, STARrunTimeExecutionEngineSTAR);
SYMBOL_EXPORT_SC_(LlvmoPkg, STARdebugObjectFilesSTAR);
SYMBOL_EXPORT_SC_(LlvmoPkg, STARdumpObjectFilesSTAR);
SYMBOL_EXPORT_SC_(KeywordPkg, dumpObjectFiles );
SYMBOL_EXPORT_SC_(KeywordPkg, debugObjectFiles );
SYMBOL_EXPORT_SC_(LlvmoPkg, STARdefault_code_modelSTAR);

SYMBOL_EXPORT_SC_(CompPkg, thread_local_llvm_context);

void redirect_llvm_interface_addSymbol() {
  //	llvm_interface::addSymbol = &addSymbolAsGlobal;
}


CL_DOCSTRING(R"dx(Load an llvm-ir file with either a bc extension or ll extension.)dx")
CL_LAMBDA(pathname &optional verbose print external_format (startup-id 0))
DOCGROUP(clasp)
CL_DEFUN bool llvm_sys__load_ir(core::T_sp filename, bool verbose, bool print, core::T_sp externalFormat, size_t startup_name )
{
  core::Pathname_sp pfilename = core::cl__pathname(filename);
  core::Pathname_sp ll_file = core::Pathname_O::makePathname(nil<core::T_O>(),nil<core::T_O>(),nil<core::T_O>(),
                                                             nil<core::T_O>(),core::SimpleBaseString_O::make("ll"));
  ll_file = cl__merge_pathnames(ll_file,pfilename);
  T_sp found = cl__probe_file(ll_file);
  if (found.notnilp()) {
    core::write_bf_stream(fmt::sprintf("Loading ll file %s\n" , _rep_(ll_file)));
    return llvm_sys__load_ll(ll_file,verbose,print,externalFormat,startup_name);
  }
  core::Pathname_sp bc_file = core::Pathname_O::makePathname(nil<core::T_O>(),nil<core::T_O>(),nil<core::T_O>(),
                                                             nil<core::T_O>(),core::SimpleBaseString_O::make("bc"));
  bc_file = cl__merge_pathnames(bc_file,pfilename);
  found = cl__probe_file(bc_file);
  if (found.notnilp()) {
    core::write_bf_stream(fmt::sprintf("Loading bc file %s\n" , _rep_(bc_file)));
    return llvm_sys__load_bc(bc_file,verbose,print,externalFormat,startup_name);
  }
  SIMPLE_ERROR(("Could not find llvm-ir file %s with .bc or .ll extension") , _rep_(filename));
}

JITDylib_sp loadModule(llvmo::Module_sp module, size_t startupID, const std::string& libname )
{
  ClaspJIT_sp jit = llvm_sys__clasp_jit();
  JITDylib_sp jitDylib = jit->createAndRegisterJITDylib(libname);
//  printf("%s:%d:%s jit = %p  jitDylib = %p\n", __FILE__, __LINE__, __FUNCTION__, jit.raw_(), jitDylib.raw_() );
  ThreadSafeContext_sp tsc = gc::As<ThreadSafeContext_sp>(comp::_sym_STARthread_safe_contextSTAR->symbolValue());
  std::vector<std::string> startup_functions;
  for (auto &F : *module->wrappedPtr()) {
    std::string function_name = F.getName().str();
//    printf("%s:%d Function: %s\n", __FILE__, __LINE__, function_name.c_str());
    if (function_name.find(MODULE_STARTUP_FUNCTION_NAME) != std::string::npos) {
      startup_functions.push_back(function_name);
    }
  }
  jit->addIRModule( jitDylib, module, tsc, startupID );
  //
  //
  for ( auto name : startup_functions ) {
//    printf("%s:%d Startup function: %s\n", __FILE__, __LINE__, name.c_str());
    core::Pointer_sp ptr = jit->lookup(jitDylib,name);
    voidStartUp startup = (voidStartUp)ptr->ptr();
//    printf("%s:%d      ptr->%p\n", __FILE__, __LINE__, startup);
    (startup)();
  }
  size_t num = core::startup_functions_are_waiting();
//  printf("%s:%d There are %lu startup functions waiting to be evaluated\n", __FILE__, __LINE__, num);
  core::startup_functions_invoke(NULL);
//  printf("%s:%d Invoked startup functions - continuing\n", __FILE__, __LINE__ );
  return jitDylib;
}
  
CL_LAMBDA(filename &optional verbose print external_format (startup-id 0))
DOCGROUP(clasp)
CL_DEFUN bool llvm_sys__load_ll(core::Pathname_sp filename, bool verbose, bool print, core::T_sp externalFormat, size_t startupID )
{
  core::DynamicScopeManager scope(::cl::_sym_STARpackageSTAR, ::cl::_sym_STARpackageSTAR->symbolValue());
  T_sp tn = cl__truename(filename);
  if ( tn.nilp() ) {
    SIMPLE_ERROR(("Could not get truename for %s") , _rep_(filename));
  }
  core::T_sp tnamestring = cl__namestring(filename);
  if ( tnamestring.nilp() ) {
    SIMPLE_ERROR(("Could not create namestring for %s") , _rep_(filename));
  }
  core::String_sp namestring = gctools::As<core::String_sp>(tnamestring);
  LLVMContext_sp context = llvm_sys__thread_local_llvm_context();
  Module_sp m = llvm_sys__parseIRFile(namestring,context);
  loadModule(m,startupID,namestring->get_std_string());
  return true;
}


CL_LAMBDA(filename &optional verbose print external_format (startup-id 0))
DOCGROUP(clasp)
CL_DEFUN bool llvm_sys__load_bc(core::Pathname_sp filename, bool verbose, bool print, core::T_sp externalFormat, size_t startupID )
{
  core::DynamicScopeManager scope(::cl::_sym_STARpackageSTAR, ::cl::_sym_STARpackageSTAR->symbolValue());
  T_sp tn = cl__truename(filename);
  if ( tn.nilp() ) {
    SIMPLE_ERROR(("Could not get truename for %s") , _rep_(filename));
  }
  core::T_sp tnamestring = cl__namestring(filename);
  if ( tnamestring.nilp() ) {
    SIMPLE_ERROR(("Could not create namestring for %s") , _rep_(filename));
  }
  core::String_sp namestring = gctools::As<core::String_sp>(tnamestring);
  LLVMContext_sp context = llvm_sys__thread_local_llvm_context();
  Module_sp m = llvm_sys__parseBitcodeFile(namestring,context);
  loadModule(m,startupID,namestring->get_std_string());
  return true;
}

CL_DOCSTRING(R"dx(Load a module into the Common Lisp environment as if it were loaded from a bitcode file)dx")

DOCGROUP(clasp)
CL_DEFUN core::SimpleBaseString_sp llvm_sys__mangleSymbolName(core::String_sp name) {
  ASSERT(cl__stringp(name));
  stringstream sout;
  std::string ssout = sout.str();
  const char *cur = ssout.c_str();
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
  return SimpleBaseString_O::make(sout.str());
};

SYMBOL_EXPORT_SC_(KeywordPkg, fixnum_tag);
SYMBOL_EXPORT_SC_(KeywordPkg, character_tag);
SYMBOL_EXPORT_SC_(KeywordPkg, single_float_tag);
SYMBOL_EXPORT_SC_(KeywordPkg, cons_tag);

DOCGROUP(clasp)
CL_DEFUN core::T_sp llvm_sys__tag_tests() {
  ql::list l;
  l << core::Cons_O::createList(kw::_sym_fixnum_tag,core::make_fixnum(gctools::STAMPWTAG_FIXNUM),core::make_fixnum(FIXNUM_TEST),core::_sym_fixnump);
  l << core::Cons_O::createList(kw::_sym_single_float_tag,core::make_fixnum(gctools::STAMPWTAG_SINGLE_FLOAT),core::make_fixnum(SINGLE_FLOAT_TEST), core::_sym_single_float_p);
  l << core::Cons_O::createList(kw::_sym_character_tag,core::make_fixnum(gctools::STAMPWTAG_CHARACTER),core::make_fixnum(CHARACTER_TEST), ::cl::_sym_characterp);
  l << core::Cons_O::createList(kw::_sym_cons_tag,core::make_fixnum(gctools::STAMPWTAG_CONS),core::make_fixnum(CONS_TEST), ::cl::_sym_consp);
  return l.cons();
}

/*! Return an a-list containing lots of values that define C++ objects that Clasp needs to know about */
DOCGROUP(clasp)
CL_DEFUN core::T_sp llvm_sys__cxxDataStructuresInfo() {
  List_sp list = nil<T_O>();
  list = Cons_O::create(Cons_O::create(_sym_tsp, make_fixnum((int)sizeof(T_sp))), nil<T_O>());
  list = Cons_O::create(Cons_O::create(_sym_tmv, make_fixnum((int)sizeof(T_mv))), list);
  list = Cons_O::create(Cons_O::create(_sym_size_t, make_fixnum((int)sizeof(size_t))), list);
  list = Cons_O::create(Cons_O::create(_sym_threadInfo, make_fixnum((int)sizeof(ThreadLocalState))), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("ALIGNMENT"), make_fixnum((int)gctools::Alignment())),list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("VALUE-FRAME-PARENT-OFFSET"), make_fixnum((int)offsetof(core::ValueFrame_O,_Parent))),list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("VALUE-FRAME-ELEMENT0-OFFSET"), make_fixnum((int)offsetof(core::ValueFrame_O,_Objects._Data[0]))),list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("VALUE-FRAME-ELEMENT-SIZE"), make_fixnum((int)sizeof(core::ValueFrame_O::value_type))),list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("LCC-ARGS-IN-REGISTERS"), make_fixnum((int)sizeof(LCC_ARGS_IN_REGISTERS))), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("PTAG-MASK"), make_fixnum((int)gctools::ptag_mask)), list);

  list = Cons_O::create(Cons_O::create(lisp_internKeyword("MTAG-MASK"), make_fixnum((int)gctools::Header_s::mtag_mask)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("DERIVABLE-WTAG"), make_fixnum((int)gctools::Header_s::Header_s::derivable_wtag)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("RACK-WTAG"), make_fixnum((int)gctools::Header_s::Header_s::rack_wtag)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("WRAPPED-WTAG"), make_fixnum((int)gctools::Header_s::Header_s::wrapped_wtag)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("HEADER-WTAG"), make_fixnum((int)gctools::Header_s::Header_s::header_wtag)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("MAX-WTAG"), make_fixnum((int)gctools::Header_s::Header_s::max_wtag)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("WTAG-WIDTH"), make_fixnum((int)gctools::Header_s::Header_s::wtag_width)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("GENERAL-MTAG-WIDTH"), make_fixnum((int)gctools::Header_s::Header_s::general_mtag_width)), list);
  
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("IMMEDIATE-MASK"), make_fixnum((int)gctools::immediate_mask)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("GENERAL-TAG"), make_fixnum((int)gctools::general_tag)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("FIXNUM-MASK"), make_fixnum((int)gctools::fixnum_mask)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("TAG-BITS"), make_fixnum((int)TAG_BITS)), list );
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("FIXNUM00-TAG"), make_fixnum((int)gctools::fixnum00_tag)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("FIXNUM01-TAG"), make_fixnum((int)gctools::fixnum01_tag)), list);
#if TAG_BITS==4
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("FIXNUM10-TAG"), make_fixnum((int)gctools::fixnum10_tag)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("FIXNUM11-TAG"), make_fixnum((int)gctools::fixnum11_tag)), list);
#endif
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("CONS-TAG"), make_fixnum((int)gctools::cons_tag)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("VASLIST0-TAG"), make_fixnum((int)gctools::vaslist0_tag)), list);
#if TAG_BITS==4
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("VASLIST1-TAG"), make_fixnum((int)gctools::vaslist1_tag)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("VASLIST-PTAG-MASK"), make_fixnum((int)gctools::vaslist_ptag_mask)), list);
#endif
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("CHARACTER-TAG"), make_fixnum((int)gctools::character_tag)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("SINGLE-FLOAT-TAG"), make_fixnum((int)gctools::single_float_tag)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("SINGLE-FLOAT-SHIFT"), make_fixnum((int)gctools::single_float_shift)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("MULTIPLE-VALUES-LIMIT"), make_fixnum((int)MultipleValues::MultipleValuesLimit)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("MULTIPLE-VALUES-SIZEOF"), make_fixnum((int)sizeof(MultipleValues))), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("CONS-CAR-OFFSET"), make_fixnum(core::Cons_O::car_offset())), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("CONS-CDR-OFFSET"), make_fixnum(core::Cons_O::cdr_offset())), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("UINTPTR_T-SIZE"), make_fixnum(sizeof(uintptr_t))), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("VASLIST-SIZE"), make_fixnum(sizeof(Vaslist))), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("VASLIST-ALIGNMENT"), make_fixnum(VASLIST_ALIGNMENT)), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("VASLIST-ARGS-OFFSET"), make_fixnum((int)Vaslist::args_offset())),list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("VASLIST-NARGS-OFFSET"), make_fixnum((int)Vaslist::nargs_offset())),list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("VASLIST-NARGS-DECREMENT"), make_fixnum((int)Vaslist::NargsDecrement)),list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("VASLIST-NARGS-MASK"), make_fixnum((int)Vaslist::NargsMask)),list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("VASLIST-NARGS-SHIFT"), make_fixnum((int)Vaslist::NargsShift)),list);
  
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("HEADER-SIZE"), make_fixnum(sizeof(gctools::Header_s))), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("HEADER-STAMP-OFFSET"), make_fixnum(offsetof(gctools::Header_s,_badge_stamp_wtag_mtag._value))), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("HEADER-STAMP-SIZE"), make_fixnum(sizeof(gctools::tagged_stamp_t))), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("REGISTER-SAVE-AREA-SIZE"), make_fixnum(LCC_TOTAL_REGISTERS*sizeof(void*))), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("ALIGNMENT"),make_fixnum(gctools::Alignment())),list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("VOID*-SIZE"),make_fixnum(sizeof(void*))),list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("JMP-BUF-SIZE"),make_fixnum(sizeof(jmp_buf))), list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("UNWIND-PROTECT-DYNENV-SIZE"),make_fixnum(gctools::sizeof_with_header<UnwindProtectDynEnv_O>())),list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("BINDING-DYNENV-SIZE"),make_fixnum(gctools::sizeof_with_header<BindingDynEnv_O>())),list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("CONS-SIZE"),make_fixnum(gctools::sizeof_with_header<Cons_O>())),list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("CLOSURE-ENTRY-POINT-OFFSET"),make_fixnum(offsetof(core::Function_O,_TheEntryPoint))),list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("GLOBAL-ENTRY-POINT-ENTRY-POINTS-OFFSET"),make_fixnum(offsetof(core::GlobalEntryPoint_O, _EntryPoints))),list);
  list = Cons_O::create(Cons_O::create(lisp_internKeyword("SIZE_T-BITS"),make_fixnum(sizeof(size_t)*8)),list);
#define ENTRY(list, name, code) list = Cons_O::create(Cons_O::create(lisp_internKeyword(name), code), list)
  LoadTimeValues_O tempLtv;
  ENTRY(list, "LOAD-TIME-VALUES-OBJECTS-OFFSET", make_fixnum((char *)&tempLtv._Objects - (char *)&tempLtv));
  gc::Vec0<T_sp> tempVec0Tsp;
  ENTRY(list, "VEC0-VECTOR-OFFSET", make_fixnum((char *)&tempVec0Tsp._Vector - (char *)&tempVec0Tsp));
  gc::GCVector_moveable<T_O *> tempGCVector(1, 0);
  ENTRY(list, "GCVECTOR-CAPACITY-OFFSET", make_fixnum((char *)&tempGCVector._Capacity - (char *)&tempGCVector));
  ENTRY(list, "GCVECTOR-END-OFFSET", make_fixnum((char *)&tempGCVector._End - (char *)&tempGCVector));
  ENTRY(list, "GCVECTOR-DATA0-OFFSET", make_fixnum((char *)&tempGCVector._Data[0] - (char *)&tempGCVector));
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
  ENTRY(list, "C++-STAMP-MAX", make_fixnum(gctools::STAMPWTAG_max));
  ENTRY(list, "CONS-STAMP", make_fixnum(gctools::STAMPWTAG_CONS));
  ENTRY(list, "VASLIST_S-STAMP", make_fixnum(gctools::STAMPWTAG_VASLIST_S));
  ENTRY(list, "CHARACTER-STAMP", make_fixnum(gctools::STAMPWTAG_CHARACTER));
  ENTRY(list, "SINGLE-FLOAT-STAMP", make_fixnum(gctools::STAMPWTAG_SINGLE_FLOAT)); 
  ENTRY(list, "INSTANCE-RACK-OFFSET", make_fixnum(offsetof(Instance_O,_Rack)));
  ENTRY(list, "INSTANCE-RACK-STAMP-OFFSET", make_fixnum(Instance_O::rack_stamp_offset()));
  ENTRY(list, "INSTANCE-STAMP", make_fixnum(static_cast<Fixnum>(gctools::STAMPWTAG_INSTANCE)));
  ENTRY(list, "WRAPPED-POINTER-STAMP", make_fixnum(static_cast<Fixnum>(gctools::STAMPWTAG_WRAPPED_POINTER)));
  ENTRY(list, "DERIVABLE-STAMP", make_fixnum(static_cast<Fixnum>(gctools::STAMPWTAG_DERIVABLE)));
  ENTRY(list, "FUNCALLABLE-INSTANCE-STAMP", make_fixnum(static_cast<Fixnum>(gctools::STAMPWTAG_FUNCALLABLE_INSTANCE)));
  ENTRY(list, "LITERAL-TAG-CHAR-CODE", make_fixnum(static_cast<Fixnum>(LITERAL_TAG_CHAR)));
//  ENTRY(list, "CLASS-KIND", make_fixnum(static_cast<Fixnum>(gctools::STAMPWTAG_CLASS)));
  ENTRY(list, "SIMPLE-VECTOR._DATA-OFFSET",make_fixnum(offsetof(SimpleVector_O,_Data)+offsetof(SimpleVector_O::vector_type,_Data)));
  ENTRY(list, "SIMPLE-VECTOR._LENGTH-OFFSET",make_fixnum(offsetof(SimpleVector_O,_Data)+offsetof(SimpleVector_O::vector_type,_MaybeSignedLength)));
  ENTRY(list, "ENTRY-POINT-ARITY-BEGIN",make_fixnum(ENTRY_POINT_ARITY_BEGIN));
  ENTRY(list, "ENTRY-POINT-ARITY-END",make_fixnum(ENTRY_POINT_ARITY_END));
  ENTRY(list, "NUMBER-OF-ENTRY-POINTS",make_fixnum(NUMBER_OF_ENTRY_POINTS));
  ENTRY(list, "ENDIAN-LSB-OFFSET", make_fixnum(ENDIAN_LSB_OFFSET) );
  ENTRY(list, "UNBOUND-MASK", make_fixnum(UNBOUND_MASK) );
  ENTRY(list, "UNBOUND-BYTE", make_fixnum(UNBOUND_BYTE) );
  return list;
}

CL_LAMBDA(&key tsp tmv symbol symbol-function-offset symbol-setf-function-offset function function-description-offset gcroots-in-module vaslist function-description)
DOCGROUP(clasp)
CL_DEFUN void llvm_sys__throwIfMismatchedStructureSizes(core::Fixnum_sp tspSize, core::Fixnum_sp tmvSize,
                                                        core::Fixnum_sp symbolSize, core::Fixnum_sp symbol_function_offset, core::Fixnum_sp symbol_setf_function_offset,
                                                        core::Fixnum_sp functionSize,
                                                        core::Fixnum_sp function_description_offset,
                                                        core::T_sp gcRootsInModuleSize,
                                                        core::T_sp tvaslistsize,
                                                        core::T_sp tFunctionDescriptionSize ) {
  int T_sp_size = sizeof(core::T_sp);
  if (unbox_fixnum(tspSize) != T_sp_size) {
    SIMPLE_ERROR(("Mismatch between tsp size[%d] and core::T_sp size[%d]") , unbox_fixnum(tspSize) , T_sp_size);
  }
  int T_mv_size = sizeof(core::T_mv);
  if (unbox_fixnum(tmvSize) != T_mv_size) {
    SIMPLE_ERROR(("Mismatch between tmv size[%d] and core::T_mv size[%d]") , unbox_fixnum(tmvSize) , T_mv_size);
  }
  int Symbol_O_size = sizeof(core::Symbol_O);
  if (unbox_fixnum(symbolSize) != Symbol_O_size) {
    SIMPLE_ERROR(("Mismatch between symbol size[%d] and core::Symbol_O size[%d]") , unbox_fixnum(symbolSize) , Symbol_O_size);
  }
  if (symbol_function_offset.unsafe_fixnum()!=offsetof(core::Symbol_O,_Function)) {
    SIMPLE_ERROR(("Mismatch between symbol function offset[%d] and core::Symbol_O._Function offset[%d]") , symbol_function_offset.unsafe_fixnum() , offsetof(core::Symbol_O,_Function));
  }
  if (symbol_setf_function_offset.unsafe_fixnum()!=offsetof(core::Symbol_O,_SetfFunction)) {
    SIMPLE_ERROR(("Mismatch between symbol setf function offset[%d] and core::Symbol_O._SetfFunction offset[%d]") , symbol_setf_function_offset.unsafe_fixnum() , offsetof(core::Symbol_O,_SetfFunction));
  }
  
  int Function_O_size = sizeof(core::Function_O);
  if (unbox_fixnum(functionSize) != Function_O_size) {
    SIMPLE_ERROR(("Mismatch between function size[%d] and core::Function_O size[%d]") , unbox_fixnum(functionSize) , Function_O_size);
  }
  if (function_description_offset.unsafe_fixnum()!=offsetof(core::GlobalEntryPoint_O,_FunctionDescription)) {
    SIMPLE_ERROR(("Mismatch between function description offset[%d] and core::GlobalEntryPoint_O._FunctionDescription offset[%d]") , function_description_offset.unsafe_fixnum() , offsetof(core::GlobalEntryPoint_O,_FunctionDescription));
  }
  if ( gcRootsInModuleSize.notnilp() ) {
    int gcRootsInModule_size = sizeof(gctools::GCRootsInModule);
    if (gcRootsInModuleSize.fixnump()) {
      if (gcRootsInModule_size != gcRootsInModuleSize.unsafe_fixnum()) {
        SIMPLE_ERROR(("GCRootsInModule size %d mismatch with Common Lisp code %d") , gcRootsInModule_size , gcRootsInModuleSize.unsafe_fixnum());
      }
    } else {
      SIMPLE_ERROR(("gcRootsInModule keyword argument expects a fixnum"));
    }
  }
  if (tvaslistsize.fixnump()) {
    size_t vaslistsize = tvaslistsize.unsafe_fixnum();
    if (vaslistsize != sizeof(Vaslist)) {
      SIMPLE_ERROR(("Vaslist size %d mismatch with Common Lisp code %d") , sizeof(Vaslist) , vaslistsize);
    }
  }
  if (tFunctionDescriptionSize.fixnump()) {
    size_t functionDescriptionSize = tFunctionDescriptionSize.unsafe_fixnum();
//    printf("%s:%d:%s Checked function-description size\n", __FILE__, __LINE__, __FUNCTION__ );
    if (functionDescriptionSize != sizeof(core::FunctionDescription_O)) {
      SIMPLE_ERROR(("function-description size %lu mismatch with Common Lisp code %lu") , sizeof(core::FunctionDescription_O) , functionDescriptionSize );
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

DOCGROUP(clasp)
CL_DEFUN llvmo::GlobalVariable_sp llvm_sys__getOrCreateExternalGlobal(llvmo::Module_sp module, const string &name, llvmo::Type_sp data_type, llvm::GlobalValue::LinkageTypes linkage) {
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
                                      linkage,
                                      0, // Initializer
                                      name);
  }
  GlobalVariable_sp gv = RP_Create_wrapped<GlobalVariable_O, llvm::GlobalVariable *>(global);
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

CL_LAMBDA(module &optional (stream t))
DOCGROUP(clasp)
CL_DEFUN void dump_module(Module_sp module, core::T_sp tstream) {
  core::T_sp stream = core::coerce::outputStreamDesignator(tstream);
  string outstr;
  llvm::raw_string_ostream sout(outstr);
  module->wrappedPtr()->print(sout,NULL);
  core::clasp_write_string(sout.str(),stream);
}

CL_LAMBDA(func &optional (stream t))
DOCGROUP(clasp)
CL_DEFUN void dump_function(Function_sp function, core::T_sp tstream) {
  core::T_sp stream = core::coerce::outputStreamDesignator(tstream);
  string outstr;
  llvm::raw_string_ostream sout(outstr);
  function->wrappedPtr()->print(sout,NULL);
  core::clasp_write_string(outstr,stream);
}

CL_LAMBDA(fn &optional only)
DOCGROUP(clasp)
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


DOCGROUP(clasp)
CL_DEFUN ClaspJIT_sp llvm_sys__clasp_jit() {
  return gc::As<ClaspJIT_sp>(_lisp->_Roots._ClaspJIT);
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
    //nothing
  };
      break;
  case candoGlobals: {
    initialize_llvm();
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
    auto jit_engine = gctools::GC<ClaspJIT_O>::allocate( false, (llvmo::JITDylib_O*)NULL );
    //llvm_sys__create_lljit_thread_pool();
    _lisp->_Roots._ClaspJIT = jit_engine;
    llvmo::_sym_STARdebugObjectFilesSTAR->defparameter(gc::As<core::Cons_sp>(::cl::_sym_STARfeaturesSTAR->symbolValue())->memberEq(kw::_sym_debugObjectFiles));
    llvmo::_sym_STARdumpObjectFilesSTAR->defparameter(gc::As<core::Cons_sp>(::cl::_sym_STARfeaturesSTAR->symbolValue())->memberEq(kw::_sym_dumpObjectFiles));
    if (llvmo::_sym_STARdebugObjectFilesSTAR->symbolValue().notnilp()) {
      printf("%s:%d:%s llvm-sys:*debugObjectFiles* is true\n", __FILE__, __LINE__, __FUNCTION__ );
    }
    if (llvmo::_sym_STARdumpObjectFilesSTAR->symbolValue().notnilp()) {
      printf("%s:%d:%s llvm-sys:*dumpObjectFiles* is true\n", __FILE__, __LINE__, __FUNCTION__ );
    }
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


void LlvmoExposer_O::shutdown() {
  _lisp->_Roots._ClaspJIT = nil<core::T_O>();
  gctools::gctools__garbage_collect();
};



/*!
   Install a trampoline that spills registers onto the stack

   This does both an interpeter trampoline and a bytecode trampoline.
   The bytecode trampoline also passes a PC.
   We could recover the bytecode PC if we spill it into the stack here as well.
   We could register another stackmap entry that stores the PC for backtraces and debugging.
   Just add another slot to %Registers and register it with a new stackmap value.

*/

CL_DEFUN core::T_mv llvm_sys__installBacktraceTrampoline() {
  std::string trampoline = R"trampoline(





; ModuleID = 'trampoline.bc'
source_filename = "trampoline.cc"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@__clasp_gcroots_in_module_trampoline = internal global { i64, i8*, i64, i64, i8**, i64 } zeroinitializer
@__clasp_literals_trampoline = internal global [0 x i8*] zeroinitializer
@_ZL16global_save_args = internal unnamed_addr global i64* null, align 8, !dbg !0

; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: argmemonly nofree nosync nounwind willreturn
declare void @llvm.lifetime.start.p0i8(i64 immarg, i8* nocapture) #2

; Function Attrs: argmemonly nofree nosync nounwind willreturn
declare void @llvm.lifetime.end.p0i8(i64 immarg, i8* nocapture) #2

; Function Attrs: mustprogress uwtable
define dso_local { i8*, i64 } @bytecode_trampoline_with_stackmap(i64 noundef %0, i8* noundef %1, i64 noundef %2, i8** noundef %3) local_unnamed_addr #0 !dbg !145 {
  %5 = alloca [3 x i64], align 16
  call void (i64, i32, ...) @llvm.experimental.stackmap(i64 3735879680, i32 0, [3 x i64]* nonnull %5)
  call void @llvm.dbg.value(metadata i64 %0, metadata !149, metadata !DIExpression()), !dbg !154
  call void @llvm.dbg.value(metadata i8* %1, metadata !150, metadata !DIExpression()), !dbg !154
  call void @llvm.dbg.value(metadata i64 %2, metadata !151, metadata !DIExpression()), !dbg !154
  call void @llvm.dbg.value(metadata i8** %3, metadata !152, metadata !DIExpression()), !dbg !154
  %6 = bitcast [3 x i64]* %5 to i8*, !dbg !155
  call void @llvm.lifetime.start.p0i8(i64 24, i8* nonnull %6) #5, !dbg !155
  call void @llvm.dbg.declare(metadata [3 x i64]* %5, metadata !153, metadata !DIExpression()), !dbg !156
  %7 = getelementptr inbounds [3 x i64], [3 x i64]* %5, i64 0, i64 0, !dbg !157
  %8 = ptrtoint i8* %1 to i64, !dbg !159
  store i64 %8, i64* %7, align 16, !dbg !160, !tbaa !136
  %9 = getelementptr inbounds [3 x i64], [3 x i64]* %5, i64 0, i64 1, !dbg !161
  store i64 %2, i64* %9, align 8, !dbg !162, !tbaa !136
  %10 = ptrtoint i8** %3 to i64, !dbg !163
  %11 = getelementptr inbounds [3 x i64], [3 x i64]* %5, i64 0, i64 2, !dbg !164
  store i64 %10, i64* %11, align 16, !dbg !165, !tbaa !136
  %12 = call { i8*, i64 } @bytecode_call(i64 noundef %0, i8* noundef %1, i64 noundef %2, i8** noundef %3), !dbg !166
  call void @llvm.lifetime.end.p0i8(i64 24, i8* nonnull %6) #5, !dbg !167
  ret { i8*, i64 } %12, !dbg !167
}

declare !dbg !168 { i8*, i64 } @bytecode_call(i64 noundef, i8* noundef, i64 noundef, i8** noundef) local_unnamed_addr #3

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone uwtable willreturn
define dso_local void @"CLASP_STARTUP_trampoline"() local_unnamed_addr #4 !dbg !170 {
  ret void, !dbg !173
}

declare void @llvm.experimental.stackmap(i64, i32, ...) #6

; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

attributes #0 = { mustprogress uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nofree nosync nounwind readnone speculatable willreturn }
attributes #2 = { argmemonly nofree nosync nounwind willreturn }
attributes #3 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #4 = { mustprogress nofree norecurse nosync nounwind readnone uwtable willreturn "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #5 = { nounwind }
attributes #6 = { nofree nosync willreturn }

!llvm.dbg.cu = !{!2}
!llvm.module.flags = !{!96, !97, !98, !99, !100, !101, !102}
!llvm.ident = !{!103}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(name: "global_save_args", linkageName: "_ZL16global_save_args", scope: !2, file: !3, line: 104, type: !95, isLocal: true, isDefinition: true)
!2 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus_14, file: !3, producer: "Debian clang version 14.0.6-2", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !4, globals: !8, imports: !9, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "trampoline.cc", directory: "/home/meister/Development/clasp-vm/src/core/trampoline", checksumkind: CSK_MD5, checksum: "f8b494ea176a734cff65e2a2727ff77f")
!4 = !{!5}
!5 = !DIDerivedType(tag: DW_TAG_typedef, name: "uintptr_t", file: !6, line: 90, baseType: !7)
!6 = !DIFile(filename: "/usr/include/stdint.h", directory: "", checksumkind: CSK_MD5, checksum: "8e56ab3ccd56760d8ae9848ebf326071")
!7 = !DIBasicType(name: "unsigned long", size: 64, encoding: DW_ATE_unsigned)
!8 = !{!0}
!9 = !{!10, !18, !22, !26, !30, !32, !34, !36, !38, !41, !44, !47, !50, !53, !55, !60, !64, !68, !71, !73, !75, !77, !79, !82, !85, !88, !91, !94}
!10 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !11, entity: !12, file: !17, line: 47)
!11 = !DINamespace(name: "std", scope: null)
!12 = !DIDerivedType(tag: DW_TAG_typedef, name: "int8_t", file: !13, line: 24, baseType: !14)
!13 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-intn.h", directory: "", checksumkind: CSK_MD5, checksum: "b26974ec56196748bbc399ee826d2a0e")
!14 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int8_t", file: !15, line: 37, baseType: !16)
!15 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/types.h", directory: "", checksumkind: CSK_MD5, checksum: "58b79843d97f4309eefa4aa722dac91e")
!16 = !DIBasicType(name: "signed char", size: 8, encoding: DW_ATE_signed_char)
!17 = !DIFile(filename: "/usr/bin/../lib/gcc/x86_64-linux-gnu/12/../../../../include/c++/12/cstdint", directory: "")
!18 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !11, entity: !19, file: !17, line: 48)
!19 = !DIDerivedType(tag: DW_TAG_typedef, name: "int16_t", file: !13, line: 25, baseType: !20)
!20 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int16_t", file: !15, line: 39, baseType: !21)
!21 = !DIBasicType(name: "short", size: 16, encoding: DW_ATE_signed)
!22 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !11, entity: !23, file: !17, line: 49)
!23 = !DIDerivedType(tag: DW_TAG_typedef, name: "int32_t", file: !13, line: 26, baseType: !24)
!24 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int32_t", file: !15, line: 41, baseType: !25)
!25 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!26 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !11, entity: !27, file: !17, line: 50)
!27 = !DIDerivedType(tag: DW_TAG_typedef, name: "int64_t", file: !13, line: 27, baseType: !28)
!28 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int64_t", file: !15, line: 44, baseType: !29)
!29 = !DIBasicType(name: "long", size: 64, encoding: DW_ATE_signed)
!30 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !11, entity: !31, file: !17, line: 52)
!31 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast8_t", file: !6, line: 58, baseType: !16)
!32 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !11, entity: !33, file: !17, line: 53)
!33 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast16_t", file: !6, line: 60, baseType: !29)
!34 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !11, entity: !35, file: !17, line: 54)
!35 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast32_t", file: !6, line: 61, baseType: !29)
!36 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !11, entity: !37, file: !17, line: 55)
!37 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast64_t", file: !6, line: 62, baseType: !29)
!38 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !11, entity: !39, file: !17, line: 57)
!39 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least8_t", file: !6, line: 43, baseType: !40)
!40 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least8_t", file: !15, line: 52, baseType: !14)
!41 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !11, entity: !42, file: !17, line: 58)
!42 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least16_t", file: !6, line: 44, baseType: !43)
!43 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least16_t", file: !15, line: 54, baseType: !20)
!44 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !11, entity: !45, file: !17, line: 59)
!45 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least32_t", file: !6, line: 45, baseType: !46)
!46 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least32_t", file: !15, line: 56, baseType: !24)
!47 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !11, entity: !48, file: !17, line: 60)
!48 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least64_t", file: !6, line: 46, baseType: !49)
!49 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least64_t", file: !15, line: 58, baseType: !28)
!50 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !11, entity: !51, file: !17, line: 62)
!51 = !DIDerivedType(tag: DW_TAG_typedef, name: "intmax_t", file: !6, line: 101, baseType: !52)
!52 = !DIDerivedType(tag: DW_TAG_typedef, name: "__intmax_t", file: !15, line: 72, baseType: !29)
!53 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !11, entity: !54, file: !17, line: 63)
!54 = !DIDerivedType(tag: DW_TAG_typedef, name: "intptr_t", file: !6, line: 87, baseType: !29)
!55 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !11, entity: !56, file: !17, line: 65)
!56 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint8_t", file: !57, line: 24, baseType: !58)
!57 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-uintn.h", directory: "", checksumkind: CSK_MD5, checksum: "3d2fbc5d847dd222c2fbd70457568436")
!58 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint8_t", file: !15, line: 38, baseType: !59)
!59 = !DIBasicType(name: "unsigned char", size: 8, encoding: DW_ATE_unsigned_char)
!60 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !11, entity: !61, file: !17, line: 66)
!61 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint16_t", file: !57, line: 25, baseType: !62)
!62 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint16_t", file: !15, line: 40, baseType: !63)
!63 = !DIBasicType(name: "unsigned short", size: 16, encoding: DW_ATE_unsigned)
!64 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !11, entity: !65, file: !17, line: 67)
!65 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint32_t", file: !57, line: 26, baseType: !66)
!66 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint32_t", file: !15, line: 42, baseType: !67)
!67 = !DIBasicType(name: "unsigned int", size: 32, encoding: DW_ATE_unsigned)
!68 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !11, entity: !69, file: !17, line: 68)
!69 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint64_t", file: !57, line: 27, baseType: !70)
!70 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint64_t", file: !15, line: 45, baseType: !7)
!71 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !11, entity: !72, file: !17, line: 70)
!72 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast8_t", file: !6, line: 71, baseType: !59)
!73 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !11, entity: !74, file: !17, line: 71)
!74 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast16_t", file: !6, line: 73, baseType: !7)
!75 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !11, entity: !76, file: !17, line: 72)
!76 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast32_t", file: !6, line: 74, baseType: !7)
!77 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !11, entity: !78, file: !17, line: 73)
!78 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast64_t", file: !6, line: 75, baseType: !7)
!79 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !11, entity: !80, file: !17, line: 75)
!80 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least8_t", file: !6, line: 49, baseType: !81)
!81 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least8_t", file: !15, line: 53, baseType: !58)
!82 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !11, entity: !83, file: !17, line: 76)
!83 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least16_t", file: !6, line: 50, baseType: !84)
!84 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least16_t", file: !15, line: 55, baseType: !62)
!85 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !11, entity: !86, file: !17, line: 77)
!86 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least32_t", file: !6, line: 51, baseType: !87)
!87 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least32_t", file: !15, line: 57, baseType: !66)
!88 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !11, entity: !89, file: !17, line: 78)
!89 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least64_t", file: !6, line: 52, baseType: !90)
!90 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least64_t", file: !15, line: 59, baseType: !70)
!91 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !11, entity: !92, file: !17, line: 80)
!92 = !DIDerivedType(tag: DW_TAG_typedef, name: "uintmax_t", file: !6, line: 102, baseType: !93)
!93 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uintmax_t", file: !15, line: 73, baseType: !7)
!94 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !11, entity: !5, file: !17, line: 81)
!95 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !69, size: 64)
!96 = !{i32 7, !"Dwarf Version", i32 5}
!97 = !{i32 2, !"Debug Info Version", i32 3}
!98 = !{i32 1, !"wchar_size", i32 4}
!99 = !{i32 7, !"PIC Level", i32 2}
!100 = !{i32 7, !"PIE Level", i32 2}
!101 = !{i32 7, !"uwtable", i32 1}
!102 = !{i32 7, !"frame-pointer", i32 2}
!103 = !{!"Debian clang version 14.0.6-2"}
!104 = distinct !DISubprogram(name: "interpreter_trampoline_with_stackmap", scope: !3, file: !3, line: 109, type: !105, scopeLine: 109, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !116)
!105 = !DISubroutineType(types: !106)
!106 = !{!107, !108, !112, !113, !115}
!107 = !DICompositeType(tag: DW_TAG_structure_type, name: "return_type", file: !3, line: 93, size: 128, flags: DIFlagFwdDecl | DIFlagNonTrivial, identifier: "_ZTS11return_type")
!108 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !109, size: 64)
!109 = !DIDerivedType(tag: DW_TAG_typedef, name: "interpreter_trampoline_type", file: !3, line: 101, baseType: !110)
!110 = !DISubroutineType(types: !111)
!111 = !{!107, !112, !113, !115}
!112 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: null, size: 64)
!113 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_t", scope: !11, file: !114, line: 298, baseType: !7)
!114 = !DIFile(filename: "/usr/bin/../lib/gcc/x86_64-linux-gnu/12/../../../../include/x86_64-linux-gnu/c++/12/bits/c++config.h", directory: "", checksumkind: CSK_MD5, checksum: "e442060cb5d8ef14615f2a1f144d5d0b")
!115 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !112, size: 64)
!116 = !{!117, !118, !119, !120, !121}
!117 = !DILocalVariable(name: "fn", arg: 1, scope: !104, file: !3, line: 109, type: !108)
!118 = !DILocalVariable(name: "closure", arg: 2, scope: !104, file: !3, line: 109, type: !112)
!119 = !DILocalVariable(name: "nargs", arg: 3, scope: !104, file: !3, line: 109, type: !113)
!120 = !DILocalVariable(name: "args", arg: 4, scope: !104, file: !3, line: 109, type: !115)
!121 = !DILocalVariable(name: "trampoline_save_args", scope: !104, file: !3, line: 110, type: !122)
!122 = !DICompositeType(tag: DW_TAG_array_type, baseType: !69, size: 192, elements: !123)
!123 = !{!124}
!124 = !DISubrange(count: 3)
!125 = !DILocation(line: 0, scope: !104)
!126 = !DILocation(line: 110, column: 5, scope: !104)
!127 = !DILocation(line: 110, column: 14, scope: !104)
!128 = !DILocation(line: 111, column: 24, scope: !104)
!129 = !DILocation(line: 111, column: 22, scope: !104)
!130 = !{!131, !131, i64 0}
!131 = !{!"any pointer", !132, i64 0}
!132 = !{!"omnipotent char", !133, i64 0}
!133 = !{!"Simple C++ TBAA"}
!134 = !DILocation(line: 112, column: 31, scope: !104)
!135 = !DILocation(line: 112, column: 29, scope: !104)
!136 = !{!137, !137, i64 0}
!137 = !{!"long", !132, i64 0}
!138 = !DILocation(line: 113, column: 5, scope: !104)
!139 = !DILocation(line: 113, column: 29, scope: !104)
!140 = !DILocation(line: 114, column: 31, scope: !104)
!141 = !DILocation(line: 114, column: 5, scope: !104)
!142 = !DILocation(line: 114, column: 29, scope: !104)
!143 = !DILocation(line: 115, column: 12, scope: !104)
!144 = !DILocation(line: 116, column: 3, scope: !104)
!145 = distinct !DISubprogram(name: "bytecode_trampoline_with_stackmap", scope: !3, file: !3, line: 120, type: !146, scopeLine: 120, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !148)
!146 = !DISubroutineType(types: !147)
!147 = !{!107, !69, !112, !113, !115}
!148 = !{!149, !150, !151, !152, !153}
!149 = !DILocalVariable(name: "pc", arg: 1, scope: !145, file: !3, line: 120, type: !69)
!150 = !DILocalVariable(name: "closure", arg: 2, scope: !145, file: !3, line: 120, type: !112)
!151 = !DILocalVariable(name: "nargs", arg: 3, scope: !145, file: !3, line: 120, type: !113)
!152 = !DILocalVariable(name: "args", arg: 4, scope: !145, file: !3, line: 120, type: !115)
!153 = !DILocalVariable(name: "trampoline_save_args", scope: !145, file: !3, line: 121, type: !122)
!154 = !DILocation(line: 0, scope: !145)
!155 = !DILocation(line: 121, column: 5, scope: !145)
!156 = !DILocation(line: 121, column: 14, scope: !145)
!157 = !DILocation(line: 122, column: 24, scope: !145)
!158 = !DILocation(line: 122, column: 22, scope: !145)
!159 = !DILocation(line: 123, column: 31, scope: !145)
!160 = !DILocation(line: 123, column: 29, scope: !145)
!161 = !DILocation(line: 124, column: 5, scope: !145)
!162 = !DILocation(line: 124, column: 29, scope: !145)
!163 = !DILocation(line: 125, column: 31, scope: !145)
!164 = !DILocation(line: 125, column: 5, scope: !145)
!165 = !DILocation(line: 125, column: 29, scope: !145)
!166 = !DILocation(line: 126, column: 12, scope: !145)
!167 = !DILocation(line: 127, column: 3, scope: !145)
!168 = !DISubprogram(name: "bytecode_call", scope: !3, file: !3, line: 118, type: !146, flags: DIFlagPrototyped, spFlags: DISPFlagOptimized, retainedNodes: !169)
!169 = !{}
!170 = distinct !DISubprogram(name: "CLASP_STARTUP_trampoline", scope: !3, file: !3, line: 130, type: !171, scopeLine: 131, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !169)
!171 = !DISubroutineType(types: !172)
!172 = !{null}
!173 = !DILocation(line: 132, column: 3, scope: !170)





)trampoline";

  LLVMContext_sp context = llvm_sys__thread_local_llvm_context();
  trampoline = std::regex_replace( trampoline, std::regex("CLASP_STARTUP"), std::string(MODULE_STARTUP_FUNCTION_NAME) );
  Module_sp module = llvm_sys__parseIRString(trampoline, context, "backtrace_trampoline" );
//  printf("%s:%d:%s About to call loadModule with module = %p\n", __FILE__, __LINE__, __FUNCTION__, module.raw_() );
  JITDylib_sp jitDylib = loadModule( module, 0, "trampoline" );
  ClaspJIT_sp jit = llvm_sys__clasp_jit();
  core::Pointer_sp bytecode_ptr = jit->lookup(jitDylib,"bytecode_trampoline_with_stackmap");
//  printf("%s:%d:%s before interpreter_trampoline = %p\n", __FILE__, __LINE__, __FUNCTION__, core::interpreter_trampoline );
  if (getenv("CLASP_DISABLE_TRAMPOLINES")) {
    printf("%s:%d:%s CLASP_DISABLE_TRAMPOLINES is set so disabling trampolines\n bytecode_trampoline at %p", __FILE__, __LINE__, __FUNCTION__, (void*)bytecode_ptr->ptr());
    return Values0<core::T_O>();
  }
  core::bytecode_trampoline = (bytecode_trampoline_function)bytecode_ptr->ptr();
//  printf("%s:%d:%s after interpreter_t
return Values(bytecode_ptr);
}




void initialize_llvm(int argc, char **argv) {
//  InitLLVM X(argc,argv);
  printf("%s:%d:%s\n", __FILE__, __LINE__, __FUNCTION__ );
}

};

#if defined(USE_MPS)||defined(USE_PRECISE_GC)
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
