/*
    File: debugger.cc
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

#include <csignal>
#include <iomanip>
#include <clasp/core/foundation.h>
#include <dlfcn.h>
#ifdef _TARGET_OS_DARWIN
#include <sys/mman.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/stat.h>
#endif
#if defined(_TARGET_OS_DARWIN) || defined(_TARGET_OS_FREEBSD)
#include <err.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#define _GNU_SOURCE
#endif

#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/arguments.h>
#include <clasp/core/myReadLine.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/pathname.h>
#include <clasp/core/debugger.h>
#include <clasp/core/funcallableInstance.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/primitives.h>
#include <clasp/core/array.h>
#include <clasp/core/bformat.h>
#include <clasp/core/write_ugly.h>
#include <clasp/core/sort.h>
#include <clasp/core/package.h>
#include <clasp/core/symbol.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/designators.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/llvmo/debugInfoExpose.h>
#include <clasp/llvmo/code.h>
#include <clasp/llvmo/jit.h>
#include <clasp/core/wrappers.h>
#include <clasp/core/stackmap.h>

#if defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_FREEBSD)
namespace core {
void* find_base_of_loaded_object(const char* name);
core::SymbolTable load_linux_symbol_table(const char* filename, uintptr_t start, uintptr_t& stackmap_start, size_t& stackmap_size);
}; // namespace core
#endif

namespace core {

DOCGROUP(clasp);
CL_DEFUN Vaslist_sp core__vaslist_rewind(Vaslist_sp v) {
  Vaslist* vaslist0 = &*v;
  Vaslist* vaslist1 = &vaslist0[1];
  memcpy(vaslist0, vaslist1, sizeof(Vaslist));
  return v;
}

DOCGROUP(clasp);
CL_DEFUN Vaslist_sp core__do_validate_vaslist(Vaslist_sp v) {
  if (!gctools::tagged_vaslistp<T_O*>(v.raw_())) {
    printf("%s:%d:%s vaslist is not tagged properly %p\n", __FILE__, __LINE__, __FUNCTION__, v.raw_());
    abort();
  }
  if (v->nargs() > 2048) {
    printf("%s:%d:%s vaslist nargs = %lu\n", __FILE__, __LINE__, __FUNCTION__, v->nargs());
    abort();
  }
  if (((uintptr_t)v->args()) & 0x7) {
    printf("%s:%d:%s vaslist args is not aligned %p\n", __FILE__, __LINE__, __FUNCTION__, v->args());
    abort();
  }
  return v;
}

DOCGROUP(clasp);
CL_DEFUN size_t core__vaslist_length(Vaslist_sp v) {
  //  printf("%s:%d vaslist length %" PRu "\n", __FILE__, __LINE__, v->nargs());
  return v->nargs();
}

DOCGROUP(clasp);
CL_DEFUN T_sp core__vaslist_pop(Vaslist_sp v) {
#ifdef DEBUG_VASLIST
  if (_sym_STARdebugVaslistSTAR && _sym_STARdebugVaslistSTAR->symbolValue().notnilp()) {
    printf("%s:%d:%s nargs: %lu  args: %p\n", __FILE__, __LINE__, __FUNCTION__, v->_nargs, v->_args);
  }
#endif
  T_sp val = v->next_arg();
#ifdef DEBUG_VASLIST
  if (_sym_STARdebugVaslistSTAR && _sym_STARdebugVaslistSTAR->symbolValue().notnilp()) {
    printf("%s:%d:%s Returning vaslist_pop-> @%p\n", __FILE__, __LINE__, __FUNCTION__, val.raw_());
    printf("%s:%d:%s Returning vaslist_pop->%s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(val).c_str());
  }
#endif
  return val;
}

DOCGROUP(clasp);
CL_DEFUN bool core__vaslistp(T_sp o) { return o.valistp(); }

DOCGROUP(clasp);
CL_DEFUN List_sp core__list_from_vaslist(Vaslist_sp vorig) {
  Vaslist valist_copy(*vorig);
  Vaslist_sp valist(&valist_copy);
  ql::list l;
  size_t nargs = valist->nargs();
  //  printf("%s:%d in %s  nargs=%zu\n", __FILE__, __LINE__, __FUNCTION__, nargs);
  for (size_t i = 0; i < nargs; ++i) {
    T_sp one = valist->next_arg_indexed(i);
    l << one;
  }
  T_sp result = l.cons();
  return result;
}
}; // namespace core

namespace core {

DebugInfo* global_DebugInfo = NULL;

DebugInfo& debugInfo() {
  if (!global_DebugInfo) {
    global_DebugInfo = new DebugInfo();
  }
  return *global_DebugInfo;
}

void add_dynamic_library_using_origin(add_dynamic_library* adder, bool is_executable, const std::string& libraryName,
                                      uintptr_t origin, gctools::clasp_ptr_t text_start, gctools::clasp_ptr_t text_end,
                                      bool hasDataConst, gctools::clasp_ptr_t dataConstStart, gctools::clasp_ptr_t dataConstEnd) {
  // printf("%s:%d:%s  About to call add_dynamic_library_using_origin libname = %s   is_executable = %d\n", __FILE__, __LINE__,
  // __FUNCTION__, libraryName.c_str(), is_executable );
  add_dynamic_library_impl(adder, is_executable, libraryName, true, origin, NULL, text_start, text_end, hasDataConst,
                           dataConstStart, dataConstEnd);
}

bool if_dynamic_library_loaded_remove(const std::string& libraryName) {
#ifdef CLASP_THREADS
  WITH_READ_WRITE_LOCK(debugInfo()._OpenDynamicLibraryMutex);
#endif
  map<string, OpenDynamicLibraryInfo*>::iterator fi = debugInfo()._OpenDynamicLibraryHandles.find(libraryName);
  bool exists = (fi != debugInfo()._OpenDynamicLibraryHandles.end());
  if (exists) {
    //    if (fi->second._SymbolTable._SymbolNames) free((void*)(fi->second._SymbolTable._SymbolNames));
    BT_LOG(("What about the stackmaps for this library - you need to remove them as well - I should probably NOT store stackmaps "
            "for libraries - but fetch them every time we need a backtrace!\n"));
    if (fi->second->_Handle == 0) {
      printf("%s:%d:%s You cannot remove the library %s\n", __FILE__, __LINE__, __FUNCTION__, libraryName.c_str());
    } else {
      dlclose(fi->second->_Handle);
      debugInfo()._OpenDynamicLibraryHandles.erase(libraryName);
    }
  }
  return exists;
}

void executablePath(std::string& name) {
  WITH_READ_LOCK(debugInfo()._OpenDynamicLibraryMutex);
  for (auto& entry : debugInfo()._OpenDynamicLibraryHandles) {
    if (entry.second->loadableKind() == Executable) {
      name = entry.second->_Filename;
      return;
    }
  }
  SIMPLE_ERROR("Could not find the executablePath");
}
void executableVtableSectionRange(gctools::clasp_ptr_t& start, gctools::clasp_ptr_t& end) {
  WITH_READ_LOCK(debugInfo()._OpenDynamicLibraryMutex);
  for (auto& entry : debugInfo()._OpenDynamicLibraryHandles) {
    if (entry.second->loadableKind() == Executable) {
      ExecutableLibraryInfo* eli = dynamic_cast<ExecutableLibraryInfo*>(entry.second);
      start = eli->_VtableSectionStart;
      end = eli->_VtableSectionEnd;
      // printf("%s:%d:%s start %p   end %p\n", __FILE__, __LINE__, __FUNCTION__, start, end );
      return;
    }
  }
  SIMPLE_ERROR("Could not find the executableVtableSectionRange");
}

void executableTextSectionRange(gctools::clasp_ptr_t& start, gctools::clasp_ptr_t& end) {
  WITH_READ_LOCK(debugInfo()._OpenDynamicLibraryMutex);
  for (auto& entry : debugInfo()._OpenDynamicLibraryHandles) {
    if (entry.second->loadableKind() == Executable) {
      start = entry.second->_TextStart;
      end = entry.second->_TextEnd;
      return;
    }
  }
  SIMPLE_ERROR("Could not find the executableVtableSectionRange");
}

bool lookup_address_in_library(gctools::clasp_ptr_t address, gctools::clasp_ptr_t& start, gctools::clasp_ptr_t& end,
                               std::string& libraryName, bool& executable, uintptr_t& vtableStart, uintptr_t& vtableEnd) {
  WITH_READ_LOCK(debugInfo()._OpenDynamicLibraryMutex);
  for (auto entry : debugInfo()._OpenDynamicLibraryHandles) {
    //    printf("%s:%d:%s Looking at entry: %s start: %p end: %p\n", __FILE__, __LINE__, __FUNCTION__,
    //    entry.second._Filename.c_str(), entry.second._LibraryStart, entry.second._LibraryEnd );
    if (ExecutableLibraryInfo* eli = dynamic_cast<ExecutableLibraryInfo*>(entry.second)) {
      if (eli->_TextStart <= address && address < eli->_TextEnd ||
          (eli->_VtableSectionStart <= address && address < eli->_VtableSectionEnd)) {
        libraryName = eli->_Filename;
        start = eli->_TextStart;
        end = eli->_TextEnd;
        executable = true;
        vtableStart = (uintptr_t)eli->_VtableSectionStart;
        vtableEnd = (uintptr_t)eli->_VtableSectionEnd;
        return true;
      }
    } else if (entry.second->_TextStart <= address && address < entry.second->_TextEnd) {
      libraryName = entry.second->_Filename;
      start = entry.second->_TextStart;
      end = entry.second->_TextEnd;
      executable = false;
      vtableStart = 0; // libraries don't have vtables we care about
      vtableEnd = 0;
      return true;
    }
  }
  return false;
}

bool library_with_name(const std::string& name, bool isExecutable, std::string& libraryName, uintptr_t& start, uintptr_t& end,
                       uintptr_t& vtableStart, uintptr_t& vtableEnd) {
  WITH_READ_LOCK(debugInfo()._OpenDynamicLibraryMutex);
  for (auto entry : debugInfo()._OpenDynamicLibraryHandles) {
    std::string libName = entry.second->_Filename;
    if (ExecutableLibraryInfo* eli = dynamic_cast<ExecutableLibraryInfo*>(entry.second)) {
      if (isExecutable || (name.size() <= libName.size() && name == libName.substr(libName.size() - name.size()))) {
        libraryName = eli->_Filename;
        start = (uintptr_t)(eli->_TextStart);
        end = (uintptr_t)(eli->_TextEnd);
        vtableStart = (uintptr_t)eli->_VtableSectionStart;
        vtableEnd = (uintptr_t)eli->_VtableSectionEnd;
        //      printf("%s:%d:%s isExecutable = %d name = %s  libraryName = %s\n", __FILE__, __LINE__, __FUNCTION__, isExecutable,
        //      name.c_str(), libraryName.c_str());
        return true;
      }
    } else {
      if (name.size() <= libName.size() && name == libName.substr(libName.size() - name.size())) {
        if (isExecutable) {
          printf("%s:%d:%s THERE IS A PROBLEM - The library %s is being searched for as Executable but it is not\n", __FILE__,
                 __LINE__, __FUNCTION__, name.c_str());
        }
        libraryName = entry.second->_Filename;
        start = (uintptr_t)(entry.second->_TextStart);
        end = (uintptr_t)(entry.second->_TextEnd);
        vtableStart = 0; // (uintptr_t)entry.second->_VtableSectionStart;
        vtableEnd = 0;   // (uintptr_t)entry.second->_VtableSectionEnd;
        //      printf("%s:%d:%s isExecutable = %d name = %s  libraryName = %s\n", __FILE__, __LINE__, __FUNCTION__, isExecutable,
        //      name.c_str(), libraryName.c_str());
        return true;
      }
    }
  }
  return false;
}

bool lookup_address(uintptr_t address, const char*& symbol, uintptr_t& start, uintptr_t& end) {
  void* ip = (void*)address;
  T_sp of = llvmo::only_object_file_for_instruction_pointer(ip);
  if (!gc::IsA<llvmo::ObjectFile_sp>(of))
    return false; // no ofi found

  // Get DWARF stuff set up
  llvmo::ObjectFile_sp ofi = gc::As_unsafe<llvmo::ObjectFile_sp>(of);
  llvmo::SectionedAddress_sp sa = object_file_sectioned_address(ip, ofi, false);
  llvmo::DWARFContext_sp dcontext = llvmo::DWARFContext_O::createDWARFContext(ofi);

  symbol = llvmo::getFunctionNameForAddress(dcontext, sa);

  // Get the address ranges
  uintptr_t code_start = ofi->codeStart();
  auto eranges = llvmo::getAddressRangesForAddressInner(dcontext, sa);
  if (eranges) {
    auto ranges = eranges.get();
    // NOTE: We assume the first range is the one we got. Practically speaking
    // I think this is okay?
    if (ranges.empty())
      return false;
    start = code_start + ranges[0].LowPC;
    end = code_start + ranges[0].HighPC;
    return true; // success!
  } else
    return false; // no ranges available
}

SYMBOL_EXPORT_SC_(KeywordPkg, function_name);
SYMBOL_EXPORT_SC_(KeywordPkg, arguments);
SYMBOL_EXPORT_SC_(KeywordPkg, closure);

}; // namespace core

namespace core {

DOCGROUP(clasp);
CL_DEFUN void core__lowLevelDescribe(T_sp obj) { dbg_lowLevelDescribe(obj); }

DOCGROUP(clasp);
CL_DEFUN core::T_mv core__lookup_address(core::Pointer_sp address) {
  const char* symbol;
  uintptr_t start;
  uintptr_t end;
  if (lookup_address((uintptr_t)(address->ptr()), symbol, start, end))
    return Values(SimpleBaseString_O::make(symbol), Pointer_O::create((void*)start), Pointer_O::create((void*)end));
  else
    return Values(nil<core::T_O>(), nil<core::T_O>(), nil<core::T_O>());
}

void dbg_Vaslist_sp_describe(T_sp obj) {
  // Convert the T_sp object into a Vaslist_sp object
  Vaslist_sp vl = Vaslist_sp((gc::Tagged)obj.raw_());
  printf("Original vaslist at: %p\n", &*((Vaslist*)gc::untag_vaslist(reinterpret_cast<Vaslist*>(obj.raw_()))));
  // Create a copy of the Vaslist with a va_copy of the vaslist
  Vaslist vlcopy_s(*vl);
  Vaslist_sp vlcopy(&vlcopy_s);
  printf("Calling dump_Vaslist_ptr\n");
  bool atHead = dump_Vaslist_ptr(stdout, &vlcopy_s);
  if (atHead) {
    for (size_t i(0), iEnd(vlcopy->nargs()); i < iEnd; ++i) {
      T_sp v = vlcopy->next_arg_indexed(i);
      printf("entry@%p %3zu --> %s\n", v.raw_(), i, _rep_(v).c_str());
    }
  }
}

void dbg_lowLevelDescribe(T_sp obj) {
  if (obj.valistp()) {
    dbg_Vaslist_sp_describe(obj);
  } else if (obj.fixnump()) {
    printf("fixnum_tag: %" PFixnum "\n", obj.unsafe_fixnum());
  } else if (obj.single_floatp()) {
    printf("single-float: %f\n", obj.unsafe_single_float());
  } else if (obj.characterp()) {
    printf("character: %d #\\%c\n", obj.unsafe_character(), obj.unsafe_character());
  } else if (obj.generalp()) {
    printf("    vtable-ptr: %p  typeid: %s\n", &*obj, typeid(obj.unsafe_general()).name());
    printf("className-> %s\n", obj.unsafe_general()->className().c_str());
    printf("contents-> [%s]\n", _rep_(obj).c_str());
    if (Function_sp closure = obj.asOrNull<Function_O>()) {
      core__closure_slots_dump(closure);
    }
  } else if (obj.consp()) {
    printf("cons_tag: %p  typeid: %s\n", &*obj, typeid(obj).name());
    printf("List:  \n");
    for (auto c : coerce_to_list(obj)) {
      printf("@%p > car@%p  cdr@%p : %s\n", c.raw_(), oCar(c).raw_(), oCdr(c).raw_(), _rep_(oCar(c)).c_str());
    }
    return;
  } else {
    printf("lowLevelDescribe handle: %p\n", obj.raw_());
  }
  fflush(stdout);
}

void dbg_describe_tagged_T_Optr(T_O* p) { client_describe(p); }

void dbg_describe_tagged_T_Optr_header(T_O* p) { client_describe(p); }

extern void dbg_describe(T_sp obj);
void dbg_describe(T_sp obj) {
  DynamicScopeManager scope(_sym_STARenablePrintPrettySTAR, nil<T_O>());
  stringstream ss;
  printf("dbg_describe object class--> %s\n", _rep_(cl__class_of(obj)->_className()).c_str());
  ss << _rep_(obj);
  printf("dbg_describe: %s\n", ss.str().c_str());
  fflush(stdout);
}

void dbg_describe_cons(Cons_sp obj) {
  DynamicScopeManager scope(_sym_STARenablePrintPrettySTAR, nil<T_O>());
  stringstream ss;
  printf("dbg_describe object class--> CONS\n");
  ss << _rep_(obj);
  printf("dbg_describe: %s\n", ss.str().c_str());
}

void dbg_describe_symbol(Symbol_sp obj) {
  DynamicScopeManager scope(_sym_STARenablePrintPrettySTAR, nil<T_O>());
  stringstream ss;
  printf("dbg_describe object class--> %s\n", _rep_(obj->__class()->_className()).c_str());
  ss << _rep_(obj);
  printf("dbg_describe: %s\n", ss.str().c_str());
}

void dbg_describeTPtr(uintptr_t raw) {
  if (raw == 0) {
    printf("dbg_describe: NULL\n");
    return;
  }
  T_sp obj = gctools::smart_ptr<T_O>(raw);
  printf("dbg_describeTPtr Raw pointer value: %p\n", obj.raw_());
  DynamicScopeManager scope(_sym_STARenablePrintPrettySTAR, nil<T_O>());
  stringstream ss;
  printf("dbg_describe object class--> %s\n", _rep_(lisp_instance_class(obj)->_className()).c_str());
  ss << _rep_(obj);
  printf("dbg_describe: %s\n", ss.str().c_str());
  fflush(stdout);
}

void dbg_printTPtr(uintptr_t raw, bool print_pretty) {
  core::T_sp sout = cl::_sym_STARstandard_outputSTAR->symbolValue();
  T_sp obj = gctools::smart_ptr<T_O>((gc::Tagged)raw);
  clasp_write_string(fmt::format("dbg_printTPtr Raw pointer value: {}\n", (void*)obj.raw_()));
  DynamicScopeManager scope(_sym_STARenablePrintPrettySTAR, nil<T_O>());
  DynamicScopeManager scope2(cl::_sym_STARprint_readablySTAR, _lisp->_boolean(print_pretty));
  clasp_write_string(fmt::format("dbg_printTPtr object class --> {}\n", _rep_(lisp_instance_class(obj)->_className())), sout);
  fflush(stdout);
  write_ugly_object(obj, sout);
  stream_force_output(sout);
}

} // namespace core

extern "C" {
#define REPR_ADDR(addr) << "@" << (void*)addr
// #define REPR_ADDR(addr)

/*! Generate text representation of a objects without using the lisp printer!
This code MUST be bulletproof!  It must work under the most memory corrupted conditions */
std::string dbg_safe_repr(uintptr_t raw) {
  stringstream ss;
  core::T_sp obj((gc::Tagged)raw);
  if (gc::tagged_generalp((gc::Tagged)raw) || gc::tagged_consp((gc::Tagged)raw)) {
    // protect us from bad pointers
    if (raw < 0x1000) {
      ss << "BAD-TAGGED-POINTER(" REPR_ADDR(raw) << ")";
      return ss.str();
    }
  }
  if (obj.generalp()) {
    if (gc::IsA<core::Symbol_sp>(obj)) {
      core::Symbol_sp sym = gc::As_unsafe<core::Symbol_sp>(obj);
      core::Package_sp pkg = gc::As_unsafe<core::Package_sp>(sym->_HomePackage.load());
      if (pkg.generalp() && gc::IsA<core::Package_sp>(pkg)) {
        if (pkg->isKeywordPackage()) {
          ss << ":";
        } else {
          ss << pkg->_Name->get_std_string() << "::";
        }
      }
      if (sym->_Name.generalp() && gc::IsA<core::String_sp>(sym->_Name)) {
        ss << sym->_Name->get_std_string();
      }
    } else if (gc::IsA<core::SimpleBaseString_sp>(obj)) {
      core::SimpleBaseString_sp sobj = gc::As_unsafe<core::SimpleBaseString_sp>(obj);
      ss << "\"" << sobj->get_std_string() << "\"";
    } else if (gc::IsA<core::SimpleCharacterString_sp>(obj)) {
      core::SimpleCharacterString_sp sobj = gc::As_unsafe<core::SimpleCharacterString_sp>(obj);
      ss << "\"" << sobj->get_std_string() << "\"";
    } else if (gc::IsA<core::Str8Ns_sp>(obj)) {
      core::Str8Ns_sp sobj = gc::As_unsafe<core::Str8Ns_sp>(obj);
      ss << "\"" << sobj->get_std_string() << "\"";
    } else if (gc::IsA<core::SimpleVector_sp>(obj)) {
      core::SimpleVector_sp svobj = gc::As_unsafe<core::SimpleVector_sp>(obj);
      ss << "#(";
      for (size_t i = 0, iEnd(svobj->length()); i < iEnd; ++i) {
        ss << dbg_safe_repr((uintptr_t)(svobj[i]).raw_()) << " ";
      }
      ss << ")" REPR_ADDR(raw);
    } else if (gc::IsA<core::FuncallableInstance_sp>(obj)) {
      core::FuncallableInstance_sp fi = gc::As_unsafe<core::FuncallableInstance_sp>(obj);
      ss << "#<$FUNCALLABLE-INSTANCE ";
      ss << _safe_rep_(fi->functionName());
      ss << " :class " << _safe_rep_(fi->_Class->_className());
      ss << "$>" REPR_ADDR(raw);
    } else if (gc::IsA<llvmo::SectionedAddress_sp>(obj)) {
      llvmo::SectionedAddress_sp sa = gc::As_unsafe<llvmo::SectionedAddress_sp>(obj);
      ss << "#<$SECTIONED-ADDRESS :address " << (void*)sa->_value.Address;
      ss << " :section-index " << sa->_value.SectionIndex;
      ss << " " REPR_ADDR(raw) << "$>";
    } else if (gc::IsA<llvmo::ObjectFile_sp>(obj)) {
      llvmo::ObjectFile_sp of = gc::As_unsafe<llvmo::ObjectFile_sp>(obj);
      ss << "#<$OBJECT-FILE :code-start " << (void*)of->codeStart();
      ss << " :code-end " << (void*)of->codeEnd();
      ss << " " REPR_ADDR(raw) << "$>";
    } else if (gc::IsA<core::Instance_sp>(obj)) {
      core::Instance_sp ii = gc::As_unsafe<core::Instance_sp>(obj);
      ss << "#<$INSTANCE :class ";
      ss << _safe_rep_(ii->_Class->_className());
      ss << " " REPR_ADDR(raw) << "$>";
    } else if (gc::IsA<core::Pathname_sp>(obj)) {
      core::Pathname_sp ii = gc::As_unsafe<core::Pathname_sp>(obj);
      ss << "#<$PATHNAME ";
      if (CONS_CAR(ii->_Directory) == kw::_sym_absolute) {
        ss << "/";
      }
      for (auto x : gc::As_unsafe<core::List_sp>(CONS_CDR(ii->_Directory))) {
        ss << _safe_rep_(CONS_CAR(x));
        ss << "/";
      }
      ss << _safe_rep_(ii->_Name);
      ss << ".";
      ss << _safe_rep_(ii->_Type);
      ss << " " REPR_ADDR(raw) << "$>";
    } else {
      core::General_sp gen = gc::As_unsafe<core::General_sp>(obj);
      ss << "#<$" << gen->className() << " " REPR_ADDR(gen.raw_()) << "$>";
    }
  } else if (obj.consp()) {
    ss << "(";
    while (obj.consp()) {
      ss << dbg_safe_repr((uintptr_t)CONS_CAR(obj).raw_()) << " ";
      obj = CONS_CDR(obj);
    }
    if (obj.notnilp()) {
      ss << ". ";
      ss << dbg_safe_repr((uintptr_t)obj.raw_());
    }
    ss << ")";
  } else if (obj.fixnump()) {
    ss << (gc::Fixnum)obj.unsafe_fixnum();
  } else if (obj.nilp()) {
    ss << "NIL";
  } else if (obj.valistp()) {
    core::Vaslist_sp vaslist = gc::As_unsafe<core::Vaslist_sp>(obj);
    ss << "#<$VASLIST ";
    ;
#if 0
    // difference during diff
    ss << (void*)obj.raw_();
#endif
#if 0
    ss  << " :args @" << (void*)vaslist->_args;
    ;
#endif
    ss << ":nargs " << vaslist->nargs();
    ss << " :contents (";
    for (size_t ii = 0; ii < vaslist->nargs(); ii++) {
      ss << dbg_safe_repr((uintptr_t)vaslist->relative_indexed_arg(ii)) << " ";
    }
    ss << ")$>";
  } else if (obj.unboundp()) {
    ss << "#:UNBOUND";
  } else if (obj.characterp()) {
    ss << "#\\" << (char)obj.unsafe_character() << "[" << (int)obj.unsafe_character() << "]";
  } else if (obj.single_floatp()) {
    ss << (float)obj.unsafe_single_float();
  } else {
    ss << " #<$RAW" REPR_ADDR(obj.raw_()) << "$>";
  }
  if (ss.str().size() > 2048) {
    return ss.str().substr(0, 2048);
  }
  return ss.str();
}
};

string _safe_rep_(core::T_sp obj) { return dbg_safe_repr((uintptr_t)obj.raw_()); }

extern "C" {

void dbg_safe_print(uintptr_t raw) { printf(" %s", dbg_safe_repr(raw).c_str()); }

void dbg_safe_println(uintptr_t raw) { printf(" %s\n", dbg_safe_repr(raw).c_str()); }
};

extern "C" {

void tprint(void* ptr) { core::dbg_printTPtr((uintptr_t)ptr, false); }

void tsymbol(void* ptr) {
  printf("%s:%d Looking up symbol at ptr->%p\n", __FILE__, __LINE__, ptr);
  core::T_sp result = llvmo::llvm_sys__lookup_jit_symbol_info(ptr);
  printf("      Result -> %s\n", _rep_(result).c_str());
}

SYMBOL_EXPORT_SC_(CorePkg, primitive_print_backtrace);
void dbg_primitive_print_backtrace() { core::eval::funcall(core::_sym_primitive_print_backtrace); }

void dbg_safe_backtrace() {
  printf("%s:%d:%s This is where we would have clasp dump a backtrace that doesn't use the printer or allocate memory\n", __FILE__,
         __LINE__, __FUNCTION__);
  printf(
      "          We don't currently have that functionality - but if we did it would use backtrace(), backtrace_symbols()\n"
      "          and scrape the DWARF accessible through _lisp->_Roots._AllObjectFiles to build a backtrace with JIT function "
      "names\n"
      "          we can use the ObjectFile_O and Code_O objects to figure out what return addresses are JITted code and what "
      "DWARF\n"
      "          belongs to each return address.\n"
      "          It might be better to use the gdb JIT API (https://sourceware.org/gdb/current/onlinedocs/gdb/JIT-Interface.html)\n"
      "          What I'm missing to make the gdb JIT API work is how do we know where the code lives for the object "
      "file/symfile?\n"
      "          It would be useful to examine __jit_debug_descriptor and look at the symfile_addr/symfile_size entries to figure "
      "out\n"
      "          where they point relative to what we have in _lisp->_Roots._AllObjectFiles.\n"
      "          As of May 2, 2021 only ELF files work with the gdb JIT API - so this wouldn't work on macOS\n");
  abort();
};
};
namespace core {

DOCGROUP(clasp);
CL_DEFUN std::string core__safe_repr(core::T_sp obj) {
  std::string result = dbg_safe_repr((uintptr_t)obj.raw_());
  return result;
}

DOCGROUP(clasp);
CL_DEFUN Pointer_sp core__objectAddress(core::T_sp obj) {
  Pointer_sp result = Pointer_O::create(&*obj);
  return result;
}

}; // namespace core
