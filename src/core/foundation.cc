/*
    File: foundation.cc
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
// #define DEBUG_LEVEL_FULL

//
// (C) 2004 Christian E. Schafmeister
//

#include <csignal>
#include <cstdarg>
#include <typeindex> // allocate_class_id
#include <dlfcn.h>

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/symbolToEnumConverter.h>
#include <clasp/core/cons.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/lispList.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/ql.h>
#include <clasp/core/fli.h>
#include <clasp/core/lispStream.h>
#include <clasp/gctools/gctoolsPackage.h>
#include <clasp/gctools/snapshotSaveLoad.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/function.h>
#include <clasp/clbind/class_rep.h>
#include <clasp/core/pointer.h>
#include <clasp/core/singleDispatchGenericFunction.h>
#include <clasp/core/singleDispatchMethod.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/write_object.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/designators.h>
#include <clasp/core/instance.h>
#include <clasp/core/documentation.h>
#include <clasp/core/array.h>
#include <clasp/core/bytecode_compiler.h>
#include <clasp/core/pointer.h>
#include <clasp/core/lispReader.h>
#include <clasp/core/wrappedPointer.h>
#include <clasp/core/debugger.h>
#include <clasp/core/debugger2.h>
// #i n c l u d e "setfExpander.h"
#include <clasp/core/primitives.h>

#ifdef _TARGET_OS_LINUX
#include <elf.h>
#endif
#ifdef darwin
#include <stdint.h>
#include <mach/mach_time.h>
#else
#include <time.h>
#endif

#include <clasp/core/object.h>
#include <clasp/core/package.h>
#include <clasp/core/null.h>
#include <clasp/core/wrappers.h>

namespace reg {

typedef std::map<std::type_index, class_id> map_type;
map_type* global_registered_ids_ptr = NULL;
class_id global_next_id = 0;

void dump_class_ids() {
  if (global_registered_ids_ptr == NULL) {
    printf("The global_registered_ids_ptr is NULL\n");
    return;
  }
  for (auto it : (*global_registered_ids_ptr)) {
    const char* fnName = it.first.name();
    size_t length;
    int status;
    char* ret = abi::__cxa_demangle(fnName, NULL, &length, &status);
    if (status == 0) {
      printf("  %s --> %lu : %s\n", ret, it.second, _rep_(lisp_classSymbolFromClassId(it.second)).c_str());
      delete ret;
    } else {
      // demangling failed. Output function name as a C function with
      // no arguments.
      printf("  %s --> %lu : %s\n", it.first.name(), it.second, _rep_(lisp_classSymbolFromClassId(it.second)).c_str());
    }
  }
}

class_id allocate_class_id(const std::type_info& cls) {
  //  printf("%s:%d:%s\n", __FILE__, __LINE__, __FUNCTION__ );
  if (global_registered_ids_ptr == NULL) {
    global_registered_ids_ptr = new map_type();
  }
  std::pair<map_type::iterator, bool> inserted = global_registered_ids_ptr->insert(std::make_pair(std::type_index(cls), global_next_id));

  if (inserted.second) {
    //            printf("%s:%d allocate_class_id for %40s %ld\n", __FILE__, __LINE__, cls.name(), id );
    ++global_next_id;
  }

  return inserted.first->second;
}

void lisp_associateClassIdWithClassSymbol(class_id cid, core::Symbol_sp sym) {
  ASSERT(_lisp);
  // I'm clobbering the memory - add this assert
  ASSERT(_lisp->classSymbolsHolder().size() < 4096 && _lisp->classSymbolsHolder().size() >= 0);
  if (cid >= _lisp->classSymbolsHolder().size()) {
    _lisp->classSymbolsHolder().resize(cid + 1, core::lisp_symbolNil());
  }
  _lisp->classSymbolsHolder()[cid] = sym;
}

core::Symbol_sp lisp_classSymbolFromClassId(class_id cid) {
  core::Symbol_sp sym = _lisp->classSymbolsHolder()[cid];
  if (sym.nilp()) {
    return unbound<core::Symbol_O>();
  }
  return sym;
}
}; // namespace reg

namespace core {

union NW {
  uint64_t word;
  char name[8];
};

uint64_t lisp_nameword(core::T_sp name) {
  NW nw;
  if (gc::IsA<String_sp>(name)) {
    String_sp sname = gc::As_unsafe<String_sp>(name);
    size_t i = 0;
    size_t iEnd(std::min((size_t)sname->length(), (size_t)8));
    for (; i < iEnd; ++i) {
      nw.name[i] = sname->rowMajorAref(i).unsafe_character();
    }
    for (size_t is = i; is < 7; ++is)
      nw.name[is] = ' ';
    nw.name[7] = '\0';
    return nw.word;
  } else if (gc::IsA<Symbol_sp>(name)) {
    String_sp sname = gc::As_unsafe<Symbol_sp>(name)->symbolName();
    size_t i = 0;
    size_t iEnd(std::min((size_t)sname->length(), (size_t)8));
    for (; i < iEnd; ++i) {
      nw.name[i] = sname->rowMajorAref(i).unsafe_character();
    }
    for (size_t is = i; is < 7; ++is)
      nw.name[is] = ' ';
    nw.name[7] = '\0';
    return nw.word;
  }
  SIMPLE_ERROR("The name {} must be a string or a symbol", _rep_(name));
}

Instance_sp lisp_built_in_class() {
  return _lisp->_Roots._TheBuiltInClass;
  //  return cl__find_class(clbind::_sym_built_in_class,false,nil<T_O>());
}
Instance_sp lisp_standard_class() {
  return _lisp->_Roots._TheStandardClass;
  // return cl__find_class(cl::_sym_standard_class,false,nil<T_O>());
}

Instance_sp lisp_derivable_cxx_class() { return _lisp->_Roots._TheDerivableCxxClass; }
Instance_sp lisp_clbind_cxx_class() { return _lisp->_Roots._TheClbindCxxClass; }

/*! Convert valid objects to void*/
void* lisp_to_void_ptr(T_sp o) {
  if (gc::IsA<clasp_ffi::ForeignData_sp>(o)) {
    return gc::As_unsafe<clasp_ffi::ForeignData_sp>(o)->ptr();
  } else if (gc::IsA<Pointer_sp>(o)) {
    return gc::As_unsafe<Pointer_sp>(o)->ptr();
  }
  SIMPLE_ERROR("The object {} cannot be converted to a void*", _rep_(o));
}

/*! Convert void* to a ForeignData_O object */
T_sp lisp_from_void_ptr(void* p) { return clasp_ffi::ForeignData_O::create(p); }
}; // namespace core

namespace core {

DOCGROUP(clasp);
CL_DEFUN void core__dump_class_ids() { reg::dump_class_ids(); }

SYMBOL_EXPORT_SC_(ClPkg,floating_point_invalid_operation);

[[noreturn]] void lisp_errorExpectedList(core::T_O* v) {
  T_sp tv((gctools::Tagged)v);
  TYPE_ERROR(tv, cl::_sym_list);
}

[[noreturn]] DONT_OPTIMIZE_ALWAYS void lisp_errorUnexpectedTypeStampWtag(size_t to, core::T_O* objP) {
  size_t expectedStamp = STAMP_UNSHIFT_WTAG(to);
#ifdef DEBUG_RUNTIME
  // This is a really low level debug code appropriate when debugging the runtime
  const char* expectedName = obj_name(expectedStamp);
  printf("%s:%d:%s ----- Unexpected type error! ------\n", __FILE__, __LINE__, __FUNCTION__);
  printf(" EXPECTED stamp_wtag = %4lu name: %s\n", (size_t)to, expectedName);
  printf(" the actual object with tagged pointer %p has the header:\n", objP);
  client_describe(objP);
#if 0
  for ( size_t ii=0; ii<_lisp->_Roots.staticClassSymbolsUnshiftedNowhere.size(); ii++ ) {
    Symbol_sp ssym = _lisp->_Roots.staticClassSymbolsUnshiftedNowhere[ii];
    printf("%s:%d:%s _lisp->_Roots.staticClassSymbolsUnshiftedNowhere[%lu] -> %s\n", __FILE__, __LINE__, __FUNCTION__, ii, _rep_(ssym).c_str() );
  }
#endif
#endif
  Symbol_sp expectedClassSymbol = _lisp->_Roots.staticClassSymbolsUnshiftedNowhere[expectedStamp];
  T_sp obj((gctools::Tagged)objP);
  TYPE_ERROR(obj, expectedClassSymbol);
}

[[noreturn]] DONT_OPTIMIZE_ALWAYS void lisp_errorUnexpectedType(class_id expected_class_id, class_id given_class_id, core::T_O* objP) {
  if (expected_class_id >= _lisp->classSymbolsHolder().size()) {
    core::lisp_error_simple(
        __FUNCTION__, __FILE__, __LINE__,
        fmt::format("expected class_id {} out of range max[{}]", expected_class_id, _lisp->classSymbolsHolder().size()));
  }
  core::Symbol_sp expectedSym = _lisp->classSymbolsHolder()[expected_class_id];
  if (expectedSym.nilp()) {
    core::lisp_error_simple(__FUNCTION__, __FILE__, __LINE__,
                            fmt::format("unexpected class_id for object {} (given_class_id {} / expected_class_id {}) could not "
                                        "lookup expected_class_id symbol",
                                        (void*)objP, given_class_id, expected_class_id));
  }

  if (given_class_id >= _lisp->classSymbolsHolder().size()) {
    core::lisp_error_simple(
        __FUNCTION__, __FILE__, __LINE__,
        fmt::format("given class_id {} out of range max[{}]", given_class_id, _lisp->classSymbolsHolder().size()));
  }
  core::Symbol_sp givenSym = _lisp->classSymbolsHolder()[given_class_id];
  if (givenSym.nilp()) {
    core::lisp_error_simple(__FUNCTION__, __FILE__, __LINE__,
                            fmt::format("given class_id {} symbol was not defined", given_class_id));
  }

  gctools::smart_ptr<core::T_O> obj((gc::Tagged)objP);
  TYPE_ERROR(obj, expectedSym);
}

[[noreturn]] DONT_OPTIMIZE_ALWAYS void lisp_errorBadCastToFixnum(class_id from_typ, core::T_O* objP) {
  class_id to_typ = reg::registered_class<core::Fixnum_I>::id;
  core::lisp_errorUnexpectedType(to_typ, from_typ, objP);
}

[[noreturn]] DONT_OPTIMIZE_ALWAYS void lisp_errorBadCast(class_id toType, class_id fromType, core::T_O* objP) {
  lisp_errorUnexpectedType(toType, fromType, objP);
}

[[noreturn]] DONT_OPTIMIZE_ALWAYS void lisp_errorBadCastStampWtag(size_t to, core::T_O* objP) {
  lisp_errorUnexpectedTypeStampWtag((size_t)to, objP);
}

[[noreturn]] DONT_OPTIMIZE_ALWAYS void lisp_errorBadCastFromT_O(class_id toType, core::T_O* objP) {
  class_id from_typ = reg::registered_class<core::T_O>::id;
  lisp_errorUnexpectedType(toType, from_typ, objP);
}

[[noreturn]] DONT_OPTIMIZE_ALWAYS void lisp_errorBadCastFromT_OToCons_O(core::T_O* objP) {
  class_id to_typ = reg::registered_class<core::Cons_O>::id;
  class_id from_typ = reg::registered_class<core::T_O>::id;
  lisp_errorUnexpectedType(to_typ, from_typ, objP);
}

[[noreturn]] DONT_OPTIMIZE_ALWAYS void lisp_errorBadCastFromSymbol_O(class_id toType, core::Symbol_O* objP) {
  class_id from_typ = reg::registered_class<core::Symbol_O>::id;
  lisp_errorUnexpectedType(toType, from_typ, reinterpret_cast<core::T_O*>(objP));
}

[[noreturn]] DONT_OPTIMIZE_ALWAYS void lisp_errorUnexpectedNil(class_id expectedTyp) {
  if (expectedTyp >= _lisp->classSymbolsHolder().size()) {
    core::lisp_error_simple(
        __FUNCTION__, __FILE__, __LINE__,
        fmt::format("expected class_id {} out of range max[%zu]", expectedTyp, _lisp->classSymbolsHolder().size()));
  }
  core::Symbol_sp expectedSym = _lisp->classSymbolsHolder()[expectedTyp];
  if (expectedSym.nilp()) {
    core::lisp_error_simple(__FUNCTION__, __FILE__, __LINE__,
                            fmt::format("expected class_id {} symbol was not defined", expectedTyp));
  }
  TYPE_ERROR(nil<core::T_O>(), expectedSym);
}

}; // namespace core
namespace boost {
using namespace core;
void assertion_failed(char const* expr, char const* function, char const* file, long line) {
  THROW_HARD_ERROR("A BOOST assertion failed");
}
}; // namespace boost

NOINLINE void dbg_hook(const char* error) {
  // Do nothing
  // set a break point here to catch every error
  //
  printf("\n\n%s\n%s:%d dbg_hook(...) was called\n", error, __FILE__, __LINE__);
  fflush(stdout);
  //	asm("int $3");

  //	core__invoke_internal_debugger(nil<core::T_O>());
}

namespace core {

void lisp_vectorPushExtend(T_sp vec, T_sp obj) {
  ComplexVector_T_sp vvec = gc::As<ComplexVector_T_sp>(vec);
  vvec->vectorPushExtend(obj);
}
}; // namespace core

namespace core {

void lisp_pushClassSymbolOntoSTARallCxxClassesSTAR(Symbol_sp classSymbol) {
  if (_sym_STARallCxxClassesSTAR->boundP()) {
    _sym_STARallCxxClassesSTAR->setf_symbolValue(Cons_O::create(classSymbol, _sym_STARallCxxClassesSTAR->symbolValue()));
  }
};

void lisp_defparameter(Symbol_sp sym, T_sp val) { sym->defparameter(val); }

void lisp_write(const string& fmt, T_sp strm) { clasp_write_string(fmt, strm); }

Symbol_sp lisp_symbolNil() { return nil<Symbol_O>(); }

bool lisp_boundp(Symbol_sp s) { return s->boundP(); }

T_sp lisp_adjust_array(T_sp array, T_sp new_size, T_sp fill_pointer) {
  return eval::funcall(cl::_sym_adjust_array, array, new_size, kw::_sym_fill_pointer, fill_pointer);
}

List_sp lisp_copy_default_special_bindings() { return _lisp->copy_default_special_bindings(); }

CL_LAMBDA(name);
CL_DECLARE();
CL_DOCSTRING(R"dx(lispifyName)dx");
DOCGROUP(clasp);
CL_DEFUN String_sp core__lispify_name(String_sp name) {
  ASSERT(name.notnilp());
  string lispified = lispify_symbol_name(name->get_std_string());
  return SimpleBaseString_O::make(lispified);
};

/*!
* Arguments
- name :: A string.
- package_str :: Return the package part.
- symbol_str :: Return the symbol part.
* Description
Convert strings that have the form pkg:name or pkg__name into a package name string and a symbol name string, run them through
lispify_symbol_name and then recombine them as pkg:name.
*/
void colon_split(const string& name, string& package_str, string& symbol_str) {
  std::size_t found = name.find(":");
  if (found != std::string::npos) {
    package_str = name.substr(0, found);
    transform(package_str.begin(), package_str.end(), package_str.begin(), ::toupper);
    symbol_str = name.substr(found + 1, std::string::npos);
    size_t first = 0;
    for (first = 0; first < symbol_str.size(); first++) {
      if (!isspace(symbol_str[first]))
        break;
    }
    size_t last;
    for (last = symbol_str.size() - 1; last >= 0; last--) {
      if (!isspace(symbol_str[last]))
        break;
    }
    symbol_str = symbol_str.substr(first, last - first + 1);
    if (symbol_str[0] == '|' && symbol_str[symbol_str.size() - 1] == '|') {
      symbol_str = symbol_str.substr(1, symbol_str.size() - 2);
    }
    return;
  }
  SIMPLE_ERROR("Could not convert {} into package:symbol_name", name);
}

CL_LAMBDA("name &optional (package \"\")");
CL_DOCSTRING(R"(Intern the package:name or name/package combination)");
DOCGROUP(clasp);
CL_DEFUN Symbol_sp core__magic_intern(const string& name, const string& package) {
  std::string pkg_sym = magic_name(name, package);
  std::string sym;
  std::string pkg;
  colon_split(pkg_sym, pkg, sym);
  Package_sp p = gc::As<Package_sp>(_lisp->findPackage(pkg));
  return p->intern(SimpleBaseString_O::make(sym));
}

CL_LAMBDA("name");
CL_DOCSTRING(R"(Disassemble and intern the package:name)");
DOCGROUP(clasp);
CL_DEFUN T_mv core__magic_disassemble_and_intern(const string& name) {
  std::string sym;
  std::string pkg;
  colon_split(name, pkg, sym);
  Package_sp p = gc::As<Package_sp>(_lisp->findPackage(pkg));
  p->intern(SimpleBaseString_O::make(sym));
  return Values(SimpleBaseString_O::make(pkg), SimpleBaseString_O::make(sym));
}

/*!
* Arguments
- name :: A string.
- package_name :: A string
* Description
Convert strings that have the form pkg:name or pkg__name into a package name string and a symbol name string, run them through
lispify_symbol_name and then recombine them as pkg:name. If package_name is not the empty string in either of the above cases,
signal an error. If the name has neither of the above forms then use the package_name to construct package_name:name after
lispifying name and return that.
*/
std::string magic_name(const std::string& name, const std::string& package_name) {
  // printf("%s:%d magic_name -> %s\n", __FILE__, __LINE__, name.c_str());
  std::size_t found = name.find(":");
  if (found != std::string::npos) {
    if (package_name != "") {
      SIMPLE_ERROR("Cannot convert {} into a symbol name because package_name {} was provided", name, package_name);
    }
    std::string pkg_str = name.substr(0, found);
    std::string symbol_str = name.substr(found + 1, std::string::npos);
    pkg_str = lispify_symbol_name(pkg_str);
    symbol_str = lispify_symbol_name(symbol_str);
    stringstream ss;
    ss << pkg_str << ":" << symbol_str;
    return ss.str();
  }
  std::size_t found2 = name.find("__");
  if (found2 != std::string::npos) {
    if (package_name != "") {
      SIMPLE_ERROR("Cannot convert {} into a symbol name because package_name {} was provided", name, package_name);
    }
    std::string pkg_str = name.substr(0, found2);
    std::string symbol_str = name.substr(found2 + 2, std::string::npos);
    pkg_str = lispify_symbol_name(pkg_str);
    symbol_str = lispify_symbol_name(symbol_str);
    stringstream ss;
    ss << pkg_str << ":" << symbol_str;
    return ss.str();
  }
  if (package_name != "") {
    std::string symbol_str = lispify_symbol_name(name);
    stringstream ss;
    ss << package_name << ":" << symbol_str;
    return ss.str();
  }
  SIMPLE_ERROR("Cannot convert {} into a package:name form because no package_name was provided", name);
}

CL_LAMBDA("name &optional (package \"\")");
CL_DECLARE();
CL_DOCSTRING(R"dx(* Arguments
- name :: A string.
- package :: A string
* Description
Convert strings that have the form pkg:name or pkg__name into a package name string and a symbol name string, 
run them through lispify_symbol_name and then recombine them as pkg:name.
Then split them again (sorry) and return (values pkg:sym pkg sym).)dx")
DOCGROUP(clasp);
CL_DEFUN T_mv core__magic_name(const std::string& name, const std::string& package) {
  std::string pkg_sym = magic_name(name, package);
  std::string sym;
  std::string pkg;
  colon_split(pkg_sym, pkg, sym);
  return Values(SimpleBaseString_O::make(pkg_sym), SimpleBaseString_O::make(pkg), SimpleBaseString_O::make(sym));
};

MultipleValues& lisp_multipleValues() {
  //	return &(_lisp->multipleValues());
  return my_thread->_MultipleValues;
}

[[noreturn]] void errorFormatted(const string& msg) {
  dbg_hook(msg.c_str());
  core__invoke_internal_debugger(nil<core::T_O>());
}

[[noreturn]] void errorFormatted(const char* msg) {
  dbg_hook(msg);
  core__invoke_internal_debugger(nil<core::T_O>());
}

string lisp_currentPackageName() {
  string pkg = _lisp->getCurrentPackage()->packageName();
  return pkg;
}

string lisp_packageName(T_sp tpkg) {
  Package_sp pkg = gc::As<Package_sp>(tpkg);
  return pkg->packageName();
}

Symbol_sp lispify_intern_keyword(string const& name) {
  string lispName = lispify_symbol_name(name);
  return _lisp->internKeyword(lispName);
}

Symbol_sp lisp_upcase_intern(string const& name, string const& packageName) {
  string lispName = stringUpper(name);
  return _lisp->intern(lispName, packageName);
}

Symbol_sp lisp_upcase_intern_export(string const& name, string const& packageName) {
  Symbol_sp sym = lisp_upcase_intern(name, packageName);
  sym->exportYourself();
  return sym;
}

typedef enum { ignore, upperCaseAlpha } NextCharTest;

}; // namespace core

template <typename Char> struct fmt::formatter<core::NextCharTest, Char> : fmt::formatter<fmt::basic_string_view<Char>> {
  template <typename FormatContext>
  auto format(const core::NextCharTest& o, FormatContext& ctx) const -> typename FormatContext::iterator {
    return fmt::formatter<fmt::basic_string_view<Char>>::format(o == core::ignore ? "ignore" : "upperCaseAlpha", ctx);
  }
};

namespace core {

bool lispify_match(const char*& cur, const char* match, NextCharTest nextCharTest = ignore) {
  const char* ccur = cur;
  while (*match) {
    if (*ccur == '\0')
      return false;
    if (*ccur != *match)
      return false;
    ++ccur;
    ++match;
  }
  if (nextCharTest == ignore) {
    cur = ccur;
    return true;
  }
  if (nextCharTest == upperCaseAlpha) {
    if (*match && upper_case_p(*match)) {
      cur = ccur;
      return true;
    }
    return false;
  }
  SIMPLE_ERROR("Unknown nextCharTest({})", nextCharTest);
}

string lispify_symbol_name(const string& s) {
  bool new_rule_change = false;
  LOG("lispify_symbol_name pass1 source[{}]", s);
  /*! Temporarily store the string in a buffer */
#define LISPIFY_BUFFER_SIZE 1024
  char buffer[LISPIFY_BUFFER_SIZE];
  int name_length = s.size();
  if ((name_length + 2) >= LISPIFY_BUFFER_SIZE) {
    printf("%s:%d The C++ string is too large to lispify - increase LISPIFY_BUFFER_SIZE to at least %d\n", __FILE__, __LINE__,
           (name_length + 2));
    abort();
  }
  memset(buffer, 0, name_length + 2);
  strncpy(buffer, s.c_str(), name_length);
  stringstream stream_pass1;
  const char* cur = buffer;
  while (*cur) {
    if (lispify_match(cur, "_SHARP_")) {
      stream_pass1 << "#";
      continue;
    }
    if (lispify_match(cur, "_BANG_")) {
      stream_pass1 << "!";
      continue;
    }
    if (lispify_match(cur, "_ATSIGN_")) {
      stream_pass1 << "@";
      continue;
    }
    if (lispify_match(cur, "_COMMA_")) {
      stream_pass1 << ",";
      continue;
    }
    if (lispify_match(cur, "_DIVIDE_")) {
      stream_pass1 << "/";
      continue;
    }
    if (lispify_match(cur, "_MINUS_")) {
      stream_pass1 << "-";
      continue;
    }
    if (lispify_match(cur, "_TIMES_")) {
      stream_pass1 << "*";
      continue;
    }
    if (lispify_match(cur, "_SLASH_")) {
      stream_pass1 << "/";
      continue;
    }
    if (lispify_match(cur, "PERCENT")) {
      stream_pass1 << "%";
      continue;
    }
    if (lispify_match(cur, "_PLUS_")) {
      stream_pass1 << "+";
      continue;
    }
    if (lispify_match(cur, "_DOT_")) {
      stream_pass1 << ".";
      continue;
    }
    if (lispify_match(cur, "_EQ_")) {
      stream_pass1 << "=";
      continue;
    }
    if (lispify_match(cur, "_NE_")) {
      stream_pass1 << "/=";
      continue;
    }
    if (lispify_match(cur, "_LT_")) {
      stream_pass1 << "<";
      continue;
    }
    if (lispify_match(cur, "_GT_")) {
      stream_pass1 << ">";
      continue;
    }
    if (lispify_match(cur, "_LE_")) {
      stream_pass1 << "<=";
      continue;
    }
    if (lispify_match(cur, "_GE_")) {
      stream_pass1 << ">=";
      continue;
    }
    if (lispify_match(cur, "_UNDERSCORE_")) {
      stream_pass1 << "_";
      continue;
    }
    if (lispify_match(cur, "STAR")) {
      stream_pass1 << "*";
      continue;
    }
    if (lispify_match(cur, "AMP")) {
      stream_pass1 << "&";
      continue;
    }
    if (lispify_match(cur, "_")) {
      stream_pass1 << "-";
      continue;
    }
    stream_pass1 << *cur;
    ++cur;
  }
  stringstream stream_pass2;
  string str_pass2 = stream_pass1.str();
  LOG("lispify_symbol_name pass2 source[{}]", str_pass2);
  const char* start_pass2 = str_pass2.c_str();
  cur = start_pass2;
  while (*cur) {
    if (lower_case_p(*cur) && upper_case_p(*(cur + 1))) {
      char lcur = *cur;
      char ucur = char_upcase(lcur);
      stream_pass2 << ucur << "-";
      ++cur;
      continue;
    }
    stream_pass2 << (char)(char_upcase(*cur));
    ++cur;
  }
  LOG("lispify_symbol_name output[{}]", stream_pass2.str());
  if (new_rule_change) {
    printf("%s:%d The symbol |%s| changed according to the new rule ([A-Z]+)([A-Z][a-z]) -> \\1-\\2-\n", __FILE__, __LINE__,
           stream_pass2.str().c_str());
  }
  return stream_pass2.str();
}

string symbol_symbolName(Symbol_sp sym) { return sym->symbolNameAsString(); }

string symbol_packageName(Symbol_sp sym) {
  T_sp p = sym->homePackage();
  if (p.nilp()) {
    return "";
  }
  return gc::As<Package_sp>(p)->packageName();
}

/* If o is an instance of Instance_O or Instance_O then it returns o->_instanceClass()
       Otherwise it returns lisp_static_class(o)
    */
Instance_sp lisp_instance_class(T_sp o) {
  T_sp tc;
  if (o.fixnump()) {
    tc = core::Fixnum_dummy_O::staticClass();
  } else if (o.characterp()) {
    tc = core::Character_dummy_O::staticClass();
  } else if (o.single_floatp()) {
    tc = core::SingleFloat_dummy_O::staticClass();
  } else if (o.consp()) {
    tc = core::Cons_O::staticClass();
  } else if (o.generalp()) {
    General_sp go(o.unsafe_general());
    tc = go->_instanceClass();
  } else if (o.valistp()) {
    tc = core::Vaslist_dummy_O::staticClass();
  } else if (o.unboundp()) {
    SIMPLE_ERROR("lisp_instance_class for called on #<UNBOUND>");
  } else {
    SIMPLE_ERROR("Add support for unknown (immediate?) object to lisp_instance_class obj = {}", (void*)(o.raw_()));
  }
  return gc::As_unsafe<Instance_sp>(tc);
}

Instance_sp lisp_static_class(T_sp o) {
  if (o.generalp()) {
    General_sp go(o.unsafe_general());
    if (go.nilp()) {
      return core::Null_O::staticClass();
    }
    return go->__class();
  }
  SIMPLE_ERROR("Add support for more classes to lisp_static_class");
}

string _rep_(T_sp obj) {
// #define USE_WRITE_OBJECT
#if defined(USE_WRITE_OBJECT)
  StringOutputStream_sp sout = clasp_make_string_output_stream();
  write_object(obj, sout);
  return sout->get_string()->get();
#else
  if (obj.fixnump()) {
    stringstream ss;
    ss << obj.unsafe_fixnum();
    return ss.str();
  } else if (obj.characterp()) {
    stringstream ss;
    ss << "#\\" << (char)obj.unsafe_character();
    return ss.str();
  } else if (obj.single_floatp()) {
    stringstream ss;
    ss << obj.unsafe_single_float();
    return ss.str();
  } else if (obj.consp()) {
    Cons_sp cobj(obj.unsafe_cons());
    return cobj->__repr__();
  } else if (obj.generalp()) {
    General_sp gobj(obj.unsafe_general());
    return gobj->__repr__(); // This is the only place where obj->__repr__() is allowed
  } else if (obj.valistp()) {
    Vaslist_sp vobj((gctools::Tagged)obj.raw_());
    List_sp l = core__list_from_vaslist(vobj);
    return _rep_(l);
  } else if (obj.unboundp()) {
    return "#<UNBOUND>";
  }
  stringstream ss;
  ss << "WTF-object@" << (void*)obj.raw_();
  return ss.str();
#endif
}

void lisp_throwUnexpectedType(T_sp offendingObject, Symbol_sp expectedTypeId) {
  Symbol_sp offendingTypeId = cl__class_of(offendingObject)->_className();
  SIMPLE_ERROR("Expected {} of class[{}] to be subclass of class[{}]", _rep_(offendingObject), _rep_(offendingTypeId),
               _rep_(expectedTypeId));
}

string lisp_classNameAsString(Instance_sp c) { return c->_classNameAsString(); }

void lisp_throwLispError(const string& str) { SIMPLE_ERROR("{}", str); }

bool lisp_debugIsOn(const char* fileName, uint debugFlags) {
  bool ret = _lisp->debugLog().isOn(fileName, debugFlags);
  return ret;
}

DebugStream* lisp_debugLog() { return &(_lisp->debugLog()); }

Fixnum lisp_hash(uintptr_t x) {
  HashGenerator hg;
  hg.addValue(x);
  return hg.rawhash();
}

T_sp lisp_true() { return _lisp->_true(); }

T_sp lisp_false() { return _lisp->_false(); }

bool lisp_CoreBuiltInClassesInitialized() { return _lisp->CoreBuiltInClassesInitialized(); }

bool lisp_BuiltInClassesInitialized() { return _lisp->BuiltInClassesInitialized(); }

T_sp lisp_boot_findClassBySymbolOrNil(Symbol_sp classSymbol) {
  Instance_sp mc = gc::As<Instance_sp>(eval::funcall(cl::_sym_findClass, classSymbol, _lisp->_true()));
  return mc;
}

List_sp lisp_parse_arguments(const string& packageName, const string& args, int number_of_required_arguments,
                             const std::set<int> skip_indices) {
  if (args == "") {
    // If args is "" then cook up arguments using number_of_required_arguments and skip_indices
    ql::list args;
    int num = 0;
    for (int idx = 0; idx < number_of_required_arguments; idx++) {
      if (skip_indices.count(idx) == 0) {
        stringstream ss;
        ss << "ARG" << num;
        num++;
        Symbol_sp arg_sym = _lisp->intern(ss.str(), packageName);
        args << arg_sym;
      }
    }
    return args.cons();
  }
  Package_sp pkg = gc::As<Package_sp>(_lisp->findPackage(packageName, true));
  ChangePackage changePackage(pkg);
  SimpleBaseString_sp ss = SimpleBaseString_O::make(args);
  Stream_sp str = gc::As_unsafe<Stream_sp>(cl__make_string_input_stream(ss, 0, nil<T_O>()));
#if 0  
  Reader_sp reader = Reader_O::create(str);
  T_sp osscons = reader->primitive_read(true, nil<T_O>(), false);
#else
  T_sp osscons = read_lisp_object(str, true, nil<T_O>(), false);
#endif
  List_sp sscons = osscons;
  return sscons;
}

List_sp lisp_lexical_variable_names(List_sp lambda_list, bool& trivial_wrapper) {
  gctools::Vec0<RequiredArgument> reqs;
  gctools::Vec0<OptionalArgument> optionals;
  gctools::Vec0<KeywordArgument> keys;
  gctools::Vec0<AuxArgument> auxs;
  RestArgument restarg;
  T_sp key_flag;
  T_sp allow_other_keys;
  parse_lambda_list(lambda_list, cl::_sym_function, reqs, optionals, restarg, key_flag, keys, allow_other_keys, auxs);
  //
  // trivial_wrapper is true if only required arguments are in lambda_list
  //
  trivial_wrapper = true;
  if (optionals.size() > 0)
    trivial_wrapper = false;
  if (keys.size() > 0)
    trivial_wrapper = false;
  if (auxs.size() > 0)
    trivial_wrapper = false;
  if (restarg.isDefined())
    trivial_wrapper = false;
  if (allow_other_keys.notnilp())
    trivial_wrapper = false;
  if (key_flag.notnilp())
    trivial_wrapper = false;
  T_sp vars = lexical_variable_names(reqs, optionals, restarg, keys, auxs);
  return vars;
}

List_sp lisp_parse_declares(const string& packageName, const string& declarestring) {
  if (declarestring == "")
    return nil<T_O>();
  Package_sp pkg = gc::As<Package_sp>(_lisp->findPackage(packageName, true));
  ChangePackage changePackage(pkg);
  SimpleBaseString_sp ss = SimpleBaseString_O::make(declarestring);
  Stream_sp str = gc::As_unsafe<Stream_sp>(cl__make_string_input_stream(ss, 0, nil<T_O>()));
#if 0
  Reader_sp reader = Reader_O::create(str);
  List_sp sscons = reader->primitive_read(true, nil<T_O>(), false);
#else
  List_sp sscons = read_lisp_object(str, true, nil<T_O>(), false);
#endif
  return sscons;
}

/*! Insert the package qualified class_symbol into the lambda list wherever a ! is seen */
string fix_method_lambda(core::Symbol_sp class_symbol, const string& lambda) {
  stringstream new_lambda;
  for (auto c : lambda) {
    if (c == '!') {
      new_lambda << class_symbol->formattedName(true);
    } else {
      new_lambda << c;
    }
  }
  return new_lambda.str();
}

SYMBOL_EXPORT_SC_(KeywordPkg, body);
SYMBOL_EXPORT_SC_(KeywordPkg, docstring);

static Function_sp bytecompile_wrapper(Function_sp entry, List_sp vars, Symbol_sp name, List_sp lambda_list) {
  /*
  // Add the name to the list so we know to compile it later when the
  // native compiler is up.
  List_sp names = core::_sym_STARbytecode_wrappersSTAR->symbolValue();
  names = Cons_O::create(name, names);
  core::_sym_STARbytecode_wrappersSTAR->setf_symbolValue(names);
*/
  // Make and return the wrapper.
  /*
`(lambda ,lambda-list
   (declare (core:lambda-name ,name))
   (cleavir-primop:funcall ,entry ,@vars))
*/
  List_sp funcall_form = Cons_O::create(cleavirPrimop::_sym_funcall, Cons_O::create(entry, vars));
  List_sp declare_form = Cons_O::createList(cl::_sym_declare, Cons_O::createList(core::_sym_lambdaName, name));
  List_sp form = Cons_O::createList(cl::_sym_lambda, lambda_list, declare_form, funcall_form);
  return comp::bytecompile(form, nil<T_O>());
}

void lisp_defineSingleDispatchMethod(T_sp name, Symbol_sp classSymbol,
                                     Function_sp method_body, size_t TemplateDispatchOn, bool useTemplateDispatchOn,
                                     const string& raw_arguments, const string& declares, const string& docstring, bool autoExport,
                                     int number_of_required_arguments, const std::set<int> pureOutIndices) {
  string arguments = fix_method_lambda(classSymbol, raw_arguments);
  Instance_sp receiver_class = gc::As<Instance_sp>(eval::funcall(cl::_sym_findClass, classSymbol, _lisp->_true()));
  Symbol_sp className = receiver_class->_className();
  List_sp ldeclares = lisp_parse_declares(gc::As<Package_sp>(className->getPackage())->packageName(), declares);
  // NOTE: We are compiling the llhandler in the package of the class - not the package of the
  // method name  -- sometimes the method name will belong to another class (ie: core:--init--)
  List_sp lambda_list;
  size_t single_dispatch_argument_index;
  if (useTemplateDispatchOn)
    single_dispatch_argument_index = TemplateDispatchOn;
  if (arguments == "" && number_of_required_arguments >= 0) {
    lambda_list = lisp_parse_arguments(gc::As<Package_sp>(className->getPackage())->packageName(), arguments,
                                       number_of_required_arguments, pureOutIndices);
  } else if (arguments != "") {
    List_sp llraw = lisp_parse_arguments(gc::As<Package_sp>(className->getPackage())->packageName(), arguments);
    T_mv mv_llprocessed = process_single_dispatch_lambda_list(llraw, true);
    T_sp tllproc = coerce_to_list(mv_llprocessed); // slice
    lambda_list = coerce_to_list(tllproc);
    MultipleValues& mvn = core::lisp_multipleValues();
    Symbol_sp sd_symbol = gc::As<Symbol_sp>(mvn.valueGet(1, mv_llprocessed.number_of_values()));
    Symbol_sp specializer_symbol = gc::As<Symbol_sp>(mvn.valueGet(2, mv_llprocessed.number_of_values()));
    T_sp dispatchOn = mvn.valueGet(3, mv_llprocessed.number_of_values());
    if (dispatchOn.unsafe_fixnum() != 0) {
      printf("%s:%d:%s bad dispatchOn lambda_list = %s sd_symbol = %s  specializer_symbol = %s dispatchOn = %s\n", __FILE__,
             __LINE__, __FUNCTION__, _rep_(lambda_list).c_str(), _rep_(sd_symbol).c_str(), _rep_(specializer_symbol).c_str(),
             _rep_(dispatchOn).c_str());
      abort();
    }
    single_dispatch_argument_index = dispatchOn.unsafe_fixnum();
  }
  if (TemplateDispatchOn != single_dispatch_argument_index) {
    SIMPLE_ERROR("Mismatch between single_dispatch_argument_index[{}] from lambda_list and TemplateDispatchOn[{}] for class {}  "
                 "method: {}  lambda_list: {}",
                 single_dispatch_argument_index, TemplateDispatchOn, _rep_(classSymbol), _rep_(name), arguments);
  }
  LOG("Interned method in class[{}]@{} with symbol[{}] arguments[{}] - autoexport[{}]", receiver_class->instanceClassName(),
      (receiver_class.get()), sym->fullName(), arguments, autoExport);

  T_sp docStr = nil<T_O>();
  if (docstring != "")
    docStr = SimpleBaseString_O::make(docstring);
  SingleDispatchGenericFunction_sp gfn = core__ensure_single_dispatch_generic_function(
      name, autoExport, single_dispatch_argument_index, lambda_list); // Ensure the single dispatch generic function exists
  //
  //
  bool trivial_wrapper;
  List_sp vars = lisp_lexical_variable_names(lambda_list, trivial_wrapper);
  Function_sp func;
  if (trivial_wrapper)
    func = method_body;
  else
    func = bytecompile_wrapper(method_body, vars, name, lambda_list);

  func->setf_sourcePathname(nil<T_O>());
  func->setf_lambdaList(lambda_list); // use this for lambda wrapper
  func->setf_docstring(docStr);
  List_sp names = core::_sym_STARbuiltin_single_dispatch_method_namesSTAR->symbolValue();
  names = Cons_O::create(name, names);
  core::_sym_STARbuiltin_single_dispatch_method_namesSTAR->setf_symbolValue(names);
  core__ensure_single_dispatch_method(gfn, name, receiver_class, ldeclares, docStr, func);
}

void lisp_throwIfBuiltInClassesNotInitialized() { _lisp->throwIfBuiltInClassesNotInitialized(); }

Instance_sp lisp_classFromClassSymbol(Symbol_sp classSymbol) {
  return gc::As<Instance_sp>(eval::funcall(cl::_sym_findClass, classSymbol, _lisp->_true()));
}

/*! If the name has the structure XXX:YYY or XXX::YYY then intern YYY in package XXX either
      exported or not respectively.   If there is no package prefix then use the defaultPackageName */
Symbol_sp lispify_intern(const string& name, const string& defaultPackageName, bool exportSymbol) {
  string lispName = lispify_symbol_name(name);
  string packageName = lispify_symbol_name(defaultPackageName);
  Symbol_sp sym = _lisp->internWithDefaultPackageName(packageName, lispName);
  if (exportSymbol) {
    sym->exportYourself();
  }
  return sym;
}

SYMBOL_EXPORT_SC_(CorePkg, bytecode_wrapper);

void lisp_bytecode_defun(SymbolFunctionEnum kind, Symbol_sp sym, const string& packageName,
                         Function_sp entry, const string& arguments, const string& declares, const string& docstring,
                         const string& sourceFile, int lineNumber, int numberOfRequiredArguments, bool autoExport,
                         const std::set<int>& skipIndices) {
  List_sp lambda_list = lisp_parse_arguments(packageName, arguments, numberOfRequiredArguments, skipIndices);
  bool trivial_wrapper;
  List_sp vars = lisp_lexical_variable_names(lambda_list, trivial_wrapper);
  Function_sp func;
  if (trivial_wrapper)
    func = entry;
  else
    func = bytecompile_wrapper(entry, vars, sym, lambda_list);
  func->setSourcePosInfo(SimpleBaseString_O::make(sourceFile), 0, lineNumber, 0);
  if (kind == symbol_function) {
    sym->setf_symbolFunction(func);
    List_sp names = core::_sym_STARbuiltin_function_namesSTAR->symbolValue();
    names = Cons_O::create(sym, names);
    core::_sym_STARbuiltin_function_namesSTAR->setf_symbolValue(names);
  } else if (kind == symbol_function_macro) {
    sym->setf_symbolFunction(func);
    sym->setf_macroP(true);
    List_sp names = core::_sym_STARbuiltin_macro_function_namesSTAR->symbolValue();
    names = Cons_O::create(sym, names);
    core::_sym_STARbuiltin_macro_function_namesSTAR->setf_symbolValue(names);
  } else if (kind == symbol_function_setf) {
    sym->setSetfFdefinition(func);
    List_sp names = core::_sym_STARbuiltin_setf_function_namesSTAR->symbolValue();
    names = Cons_O::create(sym, names);
    core::_sym_STARbuiltin_setf_function_namesSTAR->setf_symbolValue(names);
  }
  if (autoExport)
    sym->exportYourself();
  T_sp tdocstring = nil<T_O>();
  if (docstring != "")
    tdocstring = core::SimpleBaseString_O::make(docstring);
  func->setf_lambdaList(lambda_list);
  func->setf_docstring(tdocstring);
}

Symbol_sp lisp_internKeyword(const string& name) {
  if (name == "")
    return nil<Symbol_O>();
  return _lisp->internKeyword(name);
}

Symbol_sp lisp_intern(const string& name) {
  if (name == "")
    return nil<Symbol_O>();
  return _lisp->intern(name);
}

Symbol_sp lisp_intern(const string& name, const string& pkg) {
  if (name == "")
    return nil<Symbol_O>();
  return _lisp->internWithPackageName(pkg, name);
}

string symbol_fullName(Symbol_sp s) { return s->fullName(); }

void lisp_installGlobalInitializationCallback(InitializationCallback initGlobals) {
  _lisp->installGlobalInitializationCallback(initGlobals);
}

int lisp_lookupEnumForSymbol(Symbol_sp predefSymId, T_sp symbol) {
  SymbolToEnumConverter_sp converter = gc::As<SymbolToEnumConverter_sp>(predefSymId->symbolValue());
  return converter->enumIndexForSymbol(gc::As<Symbol_sp>(symbol));
}

Symbol_sp lisp_lookupSymbolForEnum(Symbol_sp predefSymId, int enumVal) {
  SymbolToEnumConverter_sp converter = gc::As<SymbolToEnumConverter_sp>(predefSymId->symbolValue());
  return converter->symbolForEnumIndex(enumVal);
}

void lisp_extendSymbolToEnumConverter(SymbolToEnumConverter_sp conv, Symbol_sp const& name, Symbol_sp const& archiveName,
                                      int value) {
  conv->addSymbolEnumPair(name, archiveName, value);
}

void lisp_debugLogWrite(char const* fileName, const char* functionName, uint lineNumber, uint col, const string& fmt_str,
                        uint debugFlags) {
  if (debugFlags == DEBUG_SCRIPT) {
    _lisp->debugLog().beginNode(DEBUG_TOPLEVEL, fileName, functionName, lineNumber, 0, fmt_str);
    _lisp->debugLog().endNode(DEBUG_TOPLEVEL);
    return;
  }
  if (debugFlags == DEBUG_SHOUT) {
    _lisp->debugLog().beginNode(DEBUG_SHOUT, fileName, functionName, lineNumber, 0, fmt_str);
    _lisp->debugLog().endNode(DEBUG_SHOUT);
    return;
  }
  _lisp->debugLog().beginNode(DEBUG_LOG, fileName, functionName, lineNumber, 0, fmt_str);
  _lisp->debugLog().writeRaw("~~~");
  _lisp->debugLog().endNode(DEBUG_LOG);
}

string concatenateVectorStrings(VectorStrings strs) {
  string conc;
  VectorStrings::iterator vs;
  conc = "";
  for (vs = strs.begin(); vs != strs.end(); vs++) {
    conc = conc + (*vs) + " ";
  }
  return conc;
}

void appendVectorStrings(VectorStrings& app, VectorStrings strs) {
  VectorStrings::iterator vs;
  for (vs = strs.begin(); vs != strs.end(); vs++) {
    app.push_back(*vs);
  }
}

string stripCharacters(const string& orig, const string& stripSet) {
  stringstream sout;
  stringstream sin;
  sin.str(orig);
  char c;
  while (sin.good()) {
    sin.get(c);
    if (stripSet.find(c) != string::npos)
      continue;
    if (!sin.good())
      continue;
    sout << c;
  }
  return sout.str();
}

string escapeWhiteSpace(const string& inp) {
  stringstream sout;
  stringstream sin(inp);
  char c;
  while (1) {
    sin.get(c);
    if (!sin.good())
      break;
    switch (c) {
    case ' ':
      sout << "\\s";
      break;
    case '\n':
      sout << "\\n";
      break;
    case '\t':
      sout << "\\t";
      break;
    case '!':
      sout << "\\\\";
      break;
    default:
      sout << c;
    }
  };
  return sout.str();
}

string unEscapeWhiteSpace(string const& inp) {
  stringstream sout;
  stringstream sin(inp);
  char c;
  while (1) {
    sin.get(c);
    if (!sin.good())
      break;
    if (c == '\\') {
      sin.get(c);
      switch (c) {
      case 's':
        sout << " ";
        break;
      case 'n':
        sout << std::endl;
        break;
      case 't':
        sout << "\t";
        break;
      case '!':
        sout << "\\";
        break;
      default:
        THROW_HARD_ERROR("Illegal escaped char[{}]", c);
        break;
      }
    } else {
      sout << c;
    }
  }
  return sout.str();
}

// Trim Both leading and trailing spaces
string trimWhiteSpace(const string& str) {
  // Find the first character position after excluding leading blank spaces
  size_t startpos = str.find_first_not_of(" \t");
  // Find the first character position from reverse af
  size_t endpos = str.find_last_not_of(" \t");
  // if all spaces or empty return an empty string
  if ((string::npos == startpos) || (string::npos == endpos)) {
    return "";
  }
  return str.substr(startpos, endpos - startpos + 1);
}

Function_sp lisp_symbolFunction(Symbol_sp sym) { return sym->symbolFunction(); }

T_sp lisp_symbolValue(Symbol_sp sym) { return sym->symbolValue(); }

string lisp_symbolNameAsString(Symbol_sp sym) {
  if (sym.nilp())
    return "NIL";
  return sym->symbolNameAsString();
}

T_sp lisp_createStr(const string& s) { return SimpleBaseString_O::make(s); }

T_sp lisp_createFixnum(int fn) { return make_fixnum(fn); }

SourcePosInfo_sp lisp_createSourcePosInfo(const string& fileName, size_t filePos, int lineno) {
  SimpleBaseString_sp fn = SimpleBaseString_O::make(fileName);
  T_mv sfi_mv = core__file_scope(fn);
  MultipleValues& mvn = core::lisp_multipleValues();
  Fixnum_sp handle = gc::As<Fixnum_sp>(mvn.valueGet(1, sfi_mv.number_of_values()));
  int sfindex = unbox_fixnum(handle);
  return SourcePosInfo_O::create(sfindex, filePos, lineno, 0);
}

/*! Create a core:source-pos-info object on the fly */
SourcePosInfo_sp core__createSourcePosInfo(const string& filename, size_t filePos, int lineno) {
  return lisp_createSourcePosInfo(filename, filePos, lineno);
}

T_sp lisp_createList() { return nil<T_O>(); }
T_sp lisp_createList(T_sp a1) { return Cons_O::create(a1, nil<T_O>()); }
T_sp lisp_createList(T_sp a1, T_sp a2) { return Cons_O::createList(a1, a2); };
T_sp lisp_createList(T_sp a1, T_sp a2, T_sp a3) { return Cons_O::createList(a1, a2, a3); };
T_sp lisp_createList(T_sp a1, T_sp a2, T_sp a3, T_sp a4) { return Cons_O::createList(a1, a2, a3, a4); };
T_sp lisp_createList(T_sp a1, T_sp a2, T_sp a3, T_sp a4, T_sp a5) { return Cons_O::createList(a1, a2, a3, a4, a5); };
T_sp lisp_createList(T_sp a1, T_sp a2, T_sp a3, T_sp a4, T_sp a5, T_sp a6) { return Cons_O::createList(a1, a2, a3, a4, a5, a6); };
T_sp lisp_createList(T_sp a1, T_sp a2, T_sp a3, T_sp a4, T_sp a5, T_sp a6, T_sp a7) {
  return Cons_O::createList(a1, a2, a3, a4, a5, a6, a7);
}
T_sp lisp_createList(T_sp a1, T_sp a2, T_sp a3, T_sp a4, T_sp a5, T_sp a6, T_sp a7, T_sp a8) {
  return Cons_O::createList(a1, a2, a3, a4, a5, a6, a7, a8);
}

[[noreturn]] void lisp_error_no_stamp(void* ptr) {
  gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(gctools::GeneralPtrToHeaderPtr(ptr));
  SIMPLE_ERROR("This General_O object {} does not return a stamp because its subclass should overload get_stamp_() and return one "
               " - the subclass header stamp value is {}",
               ((void*)ptr), header->_badge_stamp_wtag_mtag.stamp_());
}

void lisp_errorCannotAllocateInstanceWithMissingDefaultConstructor(T_sp aclass_symbol) {
  ASSERT(aclass_symbol);
  Symbol_sp cclassSymbol = aclass_symbol.as<Symbol_O>();
  SIMPLE_ERROR("You cannot allocate a {} with no arguments because it is missing a default constructor", _rep_(cclassSymbol));
}
void lisp_errorExpectedTypeSymbol(Symbol_sp typeSym, T_sp datum) { TYPE_ERROR(datum, typeSym); }

#define MAX_SPRINTF_BUFFER 1024
[[noreturn]] void lisp_error_sprintf(const char* fileName, int lineNumber, const char* functionName, const char* fmt, ...) {
  va_list args;
  va_start(args, fmt);
  char buffer[MAX_SPRINTF_BUFFER];
  vsnprintf(buffer, MAX_SPRINTF_BUFFER, fmt, args);
  va_end(args);
  stringstream ss;
  ss << "In " << functionName << " " << fileName << " line " << lineNumber << std::endl;
  ss << buffer;
  if (!_sym_signalSimpleError) {
    printf("%s:%d %s\n", __FILE__, __LINE__, ss.str().c_str());
    throw(HardError("System starting up - debugger not available yet: " + ss.str()));
  }
  if (!_sym_signalSimpleError->fboundp()) {
    printf("%s:%d %s\n", __FILE__, __LINE__, ss.str().c_str());
    dbg_hook(ss.str().c_str());
    early_debug(nil<T_O>(), false);
  }
  SYMBOL_EXPORT_SC_(ClPkg, programError);
  eval::funcall(_sym_signalSimpleError,
                core::_sym_simpleProgramError,    // arg0
                nil<T_O>(),                       // arg1
                SimpleBaseString_O::make(buffer), // arg2
                nil<T_O>());
  UNREACHABLE();
}

DONT_OPTIMIZE_ALWAYS
NOINLINE void lisp_error_simple(const char* functionName, const char* fileName, int lineNumber, const string& fmt) {
  stringstream ss;
  ss << "In " << functionName << " " << fileName << " line " << lineNumber << std::endl;
  ss << fmt;
  if (!_sym_signalSimpleError) {
    printf("%s:%d %s\n", __FILE__, __LINE__, ss.str().c_str());
    throw(HardError("System starting up - debugger not available yet: " + ss.str()));
  }
  if (!_sym_signalSimpleError->fboundp()) {
    printf("%s:%d %s\n", __FILE__, __LINE__, ss.str().c_str());
    dbg_hook(ss.str().c_str());
    early_debug(nil<T_O>(), false);
  }
  SYMBOL_EXPORT_SC_(ClPkg, programError);
  eval::funcall(_sym_signalSimpleError,
                core::_sym_simpleProgramError, // arg0
                nil<T_O>(),                    // arg1
                SimpleBaseString_O::make("~a"), core::Cons_O::createList(SimpleBaseString_O::make(fmt)));
  UNREACHABLE();
}

[[noreturn]] void lisp_error(T_sp datum, T_sp arguments) {
  if (!cl::_sym_error->fboundp()) {
    stringstream ss;
    ss << "Error " << _rep_(datum) << " initializers: " << _rep_(arguments) << std::endl;
    printf("%s:%d lisp_error ->\n %s\n", __FILE__, __LINE__, ss.str().c_str());
    early_debug(nil<T_O>(), false);
  }
  core__apply1(coerce::calledFunctionDesignator(cl::_sym_error), arguments, datum);
  UNREACHABLE();
}

string stringUpper(const string& s) {
  LOG("Converting string({}) to uppercase", s);
  stringstream ss;
  for (uint si = 0; si < s.length(); si++) {
    ss << (char)(char_upcase(s[si]));
  }
  LOG("Returning stringUpper({})", ss.str());
  return ss.str();
}

string stringUpper(const char* s) {
  LOG("Converting const char*({}) to uppercase", s);
  stringstream ss;
  for (; *s; s++) {
    ss << (char)(char_upcase(*s));
  }
  LOG("Returning stringUpper({})", ss.str());
  return ss.str();
}

vector<string> split(const string& str, const string& delimiters) {
  vector<string> parts;
  tokenize(str, parts, delimiters);
  return parts;
}

/*! DONT PUT DEBUGGING CODE IN TOKENIZE!!!!  IT IS USED BY DebugStream
 */
void tokenize(const string& str, vector<string>& tokens, const string& delimiters) {
  tokens.erase(tokens.begin(), tokens.end());
  // Skip delimiters at beginning.
  string::size_type lastPos = str.find_first_not_of(delimiters, 0);
  // Find first "non-delimiter".
  string::size_type pos = str.find_first_of(delimiters, lastPos);
  while (string::npos != pos || string::npos != lastPos) {
    // Found a token, add it to the vector.
    tokens.push_back(str.substr(lastPos, pos - lastPos));
    // Skip delimiters.  Note the "not_of"
    lastPos = str.find_first_not_of(delimiters, pos);
    // Find next "non-delimiter"
    pos = str.find_first_of(delimiters, lastPos);
  }
}

string searchAndReplaceString(const string& str, const string& search, const string& replace) {
  string result;
  string::size_type pos = 0;
  result = str;
  while ((pos = result.find(search, pos)) != string::npos) {
    result.replace(pos, search.size(), replace);
    pos++;
  }
  return result;
}

void queueSplitString(const string& str, std::queue<string>& tokens, const string& delimiters) {
  //    LOG("foundation.cc::tokenize-- Entered" );
  // Skip delimiters at beginning.
  string::size_type lastPos = str.find_first_not_of(delimiters, 0);
  // Find first "non-delimiter".
  string::size_type pos = str.find_first_of(delimiters, lastPos);
  while (string::npos != pos || string::npos != lastPos) {
    // Found a token, add it to the vector.
    tokens.push(str.substr(lastPos, pos - lastPos));
    // Skip delimiters.  Note the "not_of"
    lastPos = str.find_first_not_of(delimiters, pos);
    // Find next "non-delimiter"
    pos = str.find_first_of(delimiters, lastPos);
  }
}

//
//	Compare a wildcard containing string to a regular string
//
//	Wildcard strings can contain '*' or '?'
//
//	Return true if it matches and false if it doesn't
//
bool wildcmp(string const& sWild, string const& sRegular) {
  const char *wild, *mp, *cp;
  const char* regular;

  cp = NULL;
  mp = NULL;
  wild = sWild.c_str();
  regular = sRegular.c_str();
  while ((*regular) && (*wild != '*')) {
    if ((*wild != *regular) && (*wild != '?'))
      return false;
    wild++;
    regular++;
  }

  while (*regular) {
    if (*wild == '*') {
      if (!*++wild)
        return true;
      mp = wild;
      cp = regular + 1;
    } else if ((*wild == *regular) || (*wild == '?')) {
      wild++;
      regular++;
    } else {
      wild = mp;
      regular = cp++;
    }
  }

  while (*wild == '*') {
    wild++;
  }
  return !*wild;
}

const char* trimSourceFilePathName(const char* longName) {
  if (longName == NULL)
    return NULL;
  const char* cp = longName + strlen(longName);
  while (cp > longName && *cp != '/')
    --cp;
  if (*cp == '/')
    ++cp;
  return cp;
}

bool _ClassesAreInitialized = false;

void throwIfClassesNotInitialized(const LispPtr& lisp) {
  if (!_ClassesAreInitialized) {
    fmt::print("Debug information was being written when classes have not yet been initialized");
    fmt::print("This should never happen.");
    fmt::print("Run with the debugger and use the following commands:");
    fmt::print("l foundation.cc:1");
    fmt::print("search throwIfClassesNotInitialized");
    fmt::print("--> set a breakpoint in the if block");
    fmt::print("Then backtrace to find the offending initialization routine.");
    abort();
  }
}

}; // namespace core

extern "C" {
size_t global_drag_native_calls_delay = 0;
size_t global_drag_cxx_calls_delay = 0;
size_t global_drag_interpret_dtree_delay = 0;
size_t global_drag_cons_allocation_delay = 0;
size_t global_drag_general_allocation_delay = 0;

void drag_native_calls() {
  for (size_t ii = 0; ii < global_drag_native_calls_delay; ii++) {
    drag_delay();
  }
};

void drag_cxx_calls() {
  for (size_t ii = 0; ii < global_drag_cxx_calls_delay; ii++) {
    drag_delay();
  }
};

void drag_interpret_dtree() {
  for (size_t ii = 0; ii < global_drag_interpret_dtree_delay; ii++) {
    drag_delay();
  }
};

void drag_cons_allocation() {
  for (size_t ii = 0; ii < global_drag_cons_allocation_delay; ii++) {
    drag_delay();
  }
};

void drag_general_allocation() {
  for (size_t ii = 0; ii < global_drag_general_allocation_delay; ii++) {
    drag_delay();
  }
};
};

namespace core {

CL_DEFUN void core__set_drag_cxx_calls_delay(size_t num) {
  global_drag_cxx_calls_delay = num;
#ifndef DEBUG_DRAG_CXX_CALLS
  SIMPLE_ERROR("Does nothing - DEBUG_DRAG_CXX_CALLS is off");
#endif
};

CL_DEFUN void core__set_drag_native_calls_delay(size_t num) {
  global_drag_native_calls_delay = num;
#ifndef DEBUG_DRAG_NATIVE_CALLS
  SIMPLE_ERROR("Does nothing - DEBUG_DRAG_NATIVE_CALLS is off");
#endif
};

CL_DEFUN void core__set_drag_interpret_dtree_delay(size_t num) {
  global_drag_interpret_dtree_delay = num;
#ifndef DEBUG_DRAG_INTERPRET_DTREE
  SIMPLE_ERROR("Does nothing - DEBUG_DRAG_INTERPRET_DTREE is off");
#endif
};

CL_DEFUN void core__set_drag_cons_allocation_delay(size_t num) {
  global_drag_cons_allocation_delay = num;
#ifndef DEBUG_DRAG_CONS_ALLOCATION
  SIMPLE_ERROR("Does nothing - DEBUG_DRAG_CONS_ALLOCATION is off");
#endif
};

CL_DEFUN void core__set_drag_general_allocation_delay(size_t num) {
  global_drag_general_allocation_delay = num;
#ifndef DEBUG_DRAG_GENERAL_ALLOCATION
  SIMPLE_ERROR("Does nothing - DEBUG_DRAG_GENERAL_ALLOCATION is off");
#endif
};

}; // namespace core

namespace core {

DOCGROUP(clasp);
CL_DEFUN size_t core__get_badge(T_sp object) { return gctools::lisp_badge(object); }

DOCGROUP(clasp);
CL_DEFUN void core__debug_only_set_badge(T_sp object, size_t badge) {
  if (object.consp()) {
    gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(gctools::ConsPtrToHeaderPtr(object.unsafe_cons()));
    header->_badge_stamp_wtag_mtag._header_badge = badge;
    return;
  } else if (object.generalp()) {
    gctools::Header_s* header = const_cast<gctools::Header_s*>(gctools::header_pointer(object.unsafe_general()));
    header->_badge_stamp_wtag_mtag._header_badge = badge;
  }
}

}; // namespace core

size_t global_pointerCount = 0;
size_t global_goodPointerCount = 0;
std::set<std::string> global_mangledSymbols;
std::set<uintptr_t> global_addresses;
snapshotSaveLoad::SymbolLookup* global_SymbolLookup = NULL;

void maybe_register_symbol_using_dladdr_ep(void* functionPointer, size_t size, const std::string& name, size_t arityCode) {
  if (core::global_options->_ExportedSymbolsCheck) {
    if (!global_SymbolLookup) {
      global_SymbolLookup = new snapshotSaveLoad::SymbolLookup();
      global_SymbolLookup->addAllLibraries();
    }
    if (global_addresses.count((uintptr_t)functionPointer) == 0) {
      if ((uintptr_t)functionPointer < 1024)
        return; // This means it's a virtual method.
      global_pointerCount++;
      Dl_info info;
#ifdef RTLD_DL_SYMENT
      const Elf64_Sym* extra_info;
      int ret = dladdr1(functionPointer, &info, (void**)&extra_info, RTLD_DL_SYMENT);
#else
      int ret = dladdr(functionPointer, &info);
#endif
      bool system_dladdr_had_problem = false;
      if (ret == 0) {
        printf("%s:%d Out of %lu pointers, #%lu FAIL - system dladdr returned 0x0 for %s\n", __FILE__, __LINE__,
               global_pointerCount, (global_pointerCount - global_goodPointerCount), name.c_str());
        system_dladdr_had_problem = true;
      } else if (!info.dli_sname) {
        printf("%s:%d Out of %lu pointers, #%lu FAIL - system dladdr could not find a symbol to match %p of %s : dli_fname = %p "
               "dli_fbase = %p dli_saddr = %p\n",
               __FILE__, __LINE__, global_pointerCount, (global_pointerCount - global_goodPointerCount), functionPointer,
               name.c_str(), info.dli_fname, info.dli_fbase, info.dli_saddr);
        system_dladdr_had_problem = true;
      } else if (info.dli_saddr != functionPointer) {
        printf("%s:%d Out of %lu pointers, #%lu FAIL - system dladdr could not find exact match to %p - found %p of %s\n", __FILE__,
               __LINE__, global_pointerCount, (global_pointerCount - global_goodPointerCount), functionPointer, info.dli_saddr,
               name.c_str());
        system_dladdr_had_problem = true;
      } else if (dlsym(RTLD_DEFAULT, info.dli_sname) == 0) {
        printf("%s:%d %lu/%lu WARNING system dlsym could not find name %s - I'm going to add it anyway - if this works - remove "
               "this message\n",
               __FILE__, __LINE__, (global_pointerCount - global_goodPointerCount), global_pointerCount, info.dli_sname);
      }
      std::string claspDladdrName;
      if (!global_SymbolLookup->lookupAddr((uintptr_t)functionPointer, claspDladdrName)) {
        printf("%s:%d:%s FAIL Clasp's dladdr could not find a symbol for the address %p for name %s - THIS WILL BREAK "
               "save-lisp-and-die\n",
               __FILE__, __LINE__, __FUNCTION__, (void*)functionPointer, name.c_str());
      } else {
        if (system_dladdr_had_problem) {
          printf("%s:%d:%s   The system dladdr had a problem but clasp's dladdr found the symbol - the clasp dladdr name is: %s\n",
                 __FILE__, __LINE__, __FUNCTION__, claspDladdrName.c_str());
        }
      }
      if (system_dladdr_had_problem)
        return;
      global_mangledSymbols.insert(info.dli_sname);
      global_goodPointerCount++;
      global_addresses.insert((uintptr_t)functionPointer);
      return;
    }
  }
}

void maybe_register_symbol_using_dladdr(void* functionPointer, size_t size, const std::string& name, size_t arityCode) {
  maybe_register_symbol_using_dladdr_ep(functionPointer, size, name, arityCode);
}

namespace core {
CL_LAMBDA(&optional (stream-designator t));
DOCGROUP(clasp);
CL_DEFUN void core__mangledSymbols(T_sp stream_designator) {
  T_sp stream = coerce::outputStreamDesignator(stream_designator);
  clasp_write_string(fmt::format("# Dumping {} mangled function names\n", global_mangledSymbols.size()), stream);
  for (auto entry : ::global_mangledSymbols) {
#ifdef _TARGET_OS_DARWIN
    stream_write_char(stream, '_');
#endif
    clasp_write_string(entry, stream);
    stream_terpri(stream);
  }
  if (comp::_sym_STARprimitivesSTAR.boundp() && comp::_sym_STARprimitivesSTAR->symbolValue().notnilp()) {
    List_sp keys = gc::As<HashTable_sp>(comp::_sym_STARprimitivesSTAR->symbolValue())->keysAsCons();
    for (auto cur : keys) {
      T_sp key = CONS_CAR(cur);
      stream_write_char(stream, '_');
      clasp_write_string(gc::As<String_sp>(key)->get_std_string(), stream);
      stream_terpri(stream);
    }
  }
  clasp_write_string("__mh_execute_header", stream);
  stream_terpri(stream);
  clasp_write_string("_cc_throw", stream);
  stream_terpri(stream);
  clasp_write_string("_llvm_orc_registerJITLoaderGDBWrapper", stream);
  stream_terpri(stream);
  clasp_write_string("___jit_debug_descriptor", stream);
  stream_terpri(stream);
  clasp_write_string("___jit_debug_register_code", stream);
  stream_terpri(stream);
  //  clasp_write_string("_start_of_snapshot",stream); stream_terpri(stream);
  //  clasp_write_string("_end_of_snapshot",stream); stream_terpri(stream);
  clasp_write_string("__ZTIN4core6UnwindE", stream);
  stream_terpri(stream);
};

}; // namespace core
