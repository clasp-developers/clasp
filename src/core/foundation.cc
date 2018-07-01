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
//#define DEBUG_LEVEL_FULL

//
// (C) 2004 Christian E. Schafmeister
//

#include <csignal>

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/symbolToEnumConverter.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/lispList.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/ql.h>
#include <clasp/core/fli.h>
#include <clasp/core/lispStream.h>
//#i n c l u d e "genericFunction.h"
#include <clasp/gctools/gctoolsPackage.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/functor.h>
#include <clasp/clbind/class_rep.h>
#include <clasp/core/reader.h>
#include <clasp/core/pointer.h>
#include <clasp/core/singleDispatchGenericFunction.h>
#include <clasp/core/singleDispatchMethod.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/write_object.h>
#include <clasp/core/designators.h>
#include <clasp/core/instance.h>
#include <clasp/core/documentation.h>
#include <clasp/core/array.h>
#include <clasp/core/pointer.h>
#include <clasp/core/wrappedPointer.h>
#include <clasp/core/debugger.h>
//#i n c l u d e "setfExpander.h"
#include <clasp/core/environment.h>
#include <clasp/core/primitives.h>

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

typedef std::map<type_id, class_id> map_type;
map_type* global_registered_ids_ptr = NULL;
class_id global_next_id = 0;

void dump_class_ids() {
  if ( global_registered_ids_ptr == NULL ) {
    printf("The global_registered_ids_ptr is NULL\n");
    return;
  }
  char buffer[1024];
  for ( auto it : (*global_registered_ids_ptr) ) {
    const char* fnName = it.first.name();
    size_t length;
    int status;
    char *ret = abi::__cxa_demangle(fnName, NULL, &length, &status);
    if (status == 0) {
      printf("  %s --> %lu : %s\n", ret, it.second, _rep_(lisp_classSymbolFromClassId(it.second)).c_str() );
      delete ret;
    } else {
        // demangling failed. Output function name as a C function with
        // no arguments.
      printf("  %s --> %lu : %s\n", it.first.name(), it.second, _rep_(lisp_classSymbolFromClassId(it.second)).c_str() );
    }
  }
}

class_id allocate_class_id(type_id const &cls) {
  if ( global_registered_ids_ptr == NULL ) {
    global_registered_ids_ptr = new map_type();
  }
  std::pair<map_type::iterator, bool> inserted = global_registered_ids_ptr->insert(std::make_pair(cls, global_next_id));

  if (inserted.second) {
    //            printf("%s:%d allocate_class_id for %40s %ld\n", __FILE__, __LINE__, cls.name(), id );
    ++global_next_id;
  }

  return inserted.first->second;
}


void lisp_associateClassIdWithClassSymbol(class_id cid, core::Symbol_sp sym) {
  ASSERT(_lisp);
  // I'm clobbering the memory - add this assert
  ASSERT(_lisp->classSymbolsHolder().size() < 4096 && _lisp->classSymbolsHolder().size()>=0 );
  if (cid >= _lisp->classSymbolsHolder().size()) {
    _lisp->classSymbolsHolder().resize(cid + 1, core::lisp_symbolNil());
  }
  _lisp->classSymbolsHolder()[cid] = sym;
}

core::Symbol_sp lisp_classSymbolFromClassId(class_id cid) {
  core::Symbol_sp sym = _lisp->classSymbolsHolder()[cid];
  if (sym.nilp()) {
    return _Unbound<core::Symbol_O>();
  }
  return sym;
}
};


namespace core {

Instance_sp lisp_built_in_class() {
  return _lisp->_Roots._TheBuiltInClass;
//  return cl__find_class(clbind::_sym_built_in_class,false,_Nil<T_O>());
}
Instance_sp lisp_standard_class() {
  return _lisp->_Roots._TheStandardClass;
  // return cl__find_class(cl::_sym_standard_class,false,_Nil<T_O>());
}

Instance_sp lisp_derivable_cxx_class() {
  return _lisp->_Roots._TheDerivableCxxClass;
}
Instance_sp lisp_clbind_cxx_class() {
  return _lisp->_Roots._TheClbindCxxClass;
}


/*! Convert valid objects to void*/
void* lisp_to_void_ptr(T_sp o) {
  if (gc::IsA<clasp_ffi::ForeignData_sp>(o)) {
    return gc::As_unsafe<clasp_ffi::ForeignData_sp>(o)->ptr();
  } else if (gc::IsA<Pointer_sp>(o)) {
    return gc::As_unsafe<Pointer_sp>(o)->ptr();
  }
  SIMPLE_ERROR(BF("The object %s cannot be converted to a void*") % _rep_(o));
}

/*! Convert void* to a ForeignData_O object */
T_sp lisp_from_void_ptr(void* p) {
  return clasp_ffi::ForeignData_O::create(p);
}
};

namespace core {

CL_DEFUN void core__dump_class_ids()
{
  reg::dump_class_ids();
}


void lisp_errorExpectedList(core::T_O* v) {
  T_sp tv((gctools::Tagged)v);
  TYPE_ERROR(tv,cl::_sym_list);
}

void lisp_errorIllegalDereference(void *v) {
  SIMPLE_ERROR(BF("Tried to dereference px=%p") % v);
}

void lisp_errorDereferencedNonPointer(core::T_O *v) {
  SIMPLE_ERROR(BF("Tried to dereference immediate value: %p") % v);
}

void lisp_errorDereferencedNil() {
  SIMPLE_ERROR(BF("Tried to dereference nil"));
}

void lisp_errorDereferencedUnbound() {
  SIMPLE_ERROR(BF("Tried to dereference unbound"));
}

DONT_OPTIMIZE_ALWAYS void lisp_errorUnexpectedType(class_id expectedTyp, class_id givenTyp, core::T_O *objP) {
  if (expectedTyp >= _lisp->classSymbolsHolder().size()) {
    core::lisp_error_simple(__FUNCTION__, __FILE__, __LINE__, boost::format("expected class_id %d out of range max[%d]") % expectedTyp % _lisp->classSymbolsHolder().size());
  }
  core::Symbol_sp expectedSym = _lisp->classSymbolsHolder()[expectedTyp];
  if (expectedSym.nilp()) {
    core::lisp_error_simple(__FUNCTION__, __FILE__, __LINE__, boost::format("expected class_id %d symbol was not defined") % expectedTyp);
  }

  if (givenTyp >= _lisp->classSymbolsHolder().size()) {
    core::lisp_error_simple(__FUNCTION__, __FILE__, __LINE__, boost::format("given class_id %d out of range max[%d]") % givenTyp % _lisp->classSymbolsHolder().size());
  }
  core::Symbol_sp givenSym = _lisp->classSymbolsHolder()[givenTyp];
  if (givenSym.nilp()) {
    core::lisp_error_simple(__FUNCTION__, __FILE__, __LINE__, boost::format("given class_id %d symbol was not defined") % givenTyp);
  }

  gctools::smart_ptr<core::T_O> obj((gc::Tagged)objP);
  TYPE_ERROR(obj, expectedSym);
}

DONT_OPTIMIZE_ALWAYS void lisp_errorBadCastToFixnum(class_id from_typ, core::T_O *objP) {
  class_id to_typ = reg::registered_class<core::Fixnum_I>::id;
  core::lisp_errorUnexpectedType(to_typ, from_typ, objP);
}

DONT_OPTIMIZE_ALWAYS void lisp_errorBadCast(class_id toType, class_id fromType, core::T_O *objP) {
  lisp_errorUnexpectedType(toType, fromType, objP);
}

DONT_OPTIMIZE_ALWAYS void lisp_errorBadCastFromT_O(class_id toType, core::T_O *objP) {
  class_id from_typ = reg::registered_class<core::T_O>::id;
  lisp_errorUnexpectedType(toType, from_typ, objP);
}

DONT_OPTIMIZE_ALWAYS void lisp_errorBadCastFromT_OToCons_O(core::T_O *objP) {
  class_id to_typ = reg::registered_class<core::Cons_O>::id;
  class_id from_typ = reg::registered_class<core::T_O>::id;
  lisp_errorUnexpectedType(to_typ, from_typ, objP);
}

DONT_OPTIMIZE_ALWAYS void lisp_errorBadCastFromSymbol_O(class_id toType, core::Symbol_O *objP) {
  class_id from_typ = reg::registered_class<core::Symbol_O>::id;
  lisp_errorUnexpectedType(toType, from_typ, reinterpret_cast<core::T_O *>(objP));
}

DONT_OPTIMIZE_ALWAYS void lisp_errorUnexpectedNil(class_id expectedTyp) {
  if (expectedTyp >= _lisp->classSymbolsHolder().size()) {
    core::lisp_error_simple(__FUNCTION__, __FILE__, __LINE__, boost::format("expected class_id %" PRu " out of range max[%zu]") % expectedTyp % _lisp->classSymbolsHolder().size());
  }
  core::Symbol_sp expectedSym = _lisp->classSymbolsHolder()[expectedTyp];
  if (expectedSym.nilp()) {
    core::lisp_error_simple(__FUNCTION__, __FILE__, __LINE__, boost::format("expected class_id %" PRu " symbol was not defined") % expectedTyp);
  }
  TYPE_ERROR(_Nil<core::T_O>(), expectedSym);
}

};
namespace boost {
using namespace core;
void assertion_failed(char const *expr, char const *function, char const *file, long line) {
  THROW_HARD_ERROR(BF("A BOOST assertion failed"));
}
};

namespace llvm_interface {

::llvm_interface::llvmAddSymbolCallbackType addSymbol = NULL;
};

NOINLINE void dbg_hook(const char *error) {
  // Do nothing
  // set a break point here to catch every error
  //
  printf("\n\n%s\n%s:%d dbg_hook(...) was called\n", error, __FILE__, __LINE__);
  fflush(stdout);
  //	asm("int $3");

  //	core__invoke_internal_debugger(_Nil<core::T_O>());
}

namespace core {

void lisp_vectorPushExtend(T_sp vec, T_sp obj) {
  VectorObjects_sp vvec = gc::As<VectorObjects_sp>(vec);
  vvec->vectorPushExtend(obj);
}
};

namespace core {

List_sp clasp_grab_rest_args(va_list args, int nargs) {
  ql::list l;
  while (nargs) {
    T_sp arg = gctools::smart_ptr<T_O>((gc::Tagged)va_arg(args, T_O *));
    l << arg;
    --nargs;
  }
  return l.cons();
}

void lisp_pushClassSymbolOntoSTARallCxxClassesSTAR(Symbol_sp classSymbol) {
  if (_sym_STARallCxxClassesSTAR->symbolValueUnsafe()) {
    _sym_STARallCxxClassesSTAR->setf_symbolValue(Cons_O::create(classSymbol, _sym_STARallCxxClassesSTAR->symbolValue()));
  }
};

void lisp_defparameter(Symbol_sp sym, T_sp val) {
  sym->defparameter(val);
}

void lisp_write(const boost::format &fmt, T_sp strm) {
  clasp_write_string(fmt.str(), strm);
}

Symbol_sp lisp_symbolNil() {
  return _Nil<Symbol_O>();
}

bool lisp_boundp(Symbol_sp s) {
  return s->boundP();
}

T_sp lisp_adjust_array(T_sp array, T_sp new_size, T_sp fill_pointer) {
  return eval::funcall(cl::_sym_adjust_array,array,new_size,kw::_sym_fill_pointer,fill_pointer);
}


List_sp lisp_copy_default_special_bindings() {
  return _lisp->copy_default_special_bindings();
}

CL_LAMBDA(name);
CL_DECLARE();
CL_DOCSTRING("lispifyName");
CL_DEFUN String_sp core__lispify_name(String_sp name) {
  ASSERT(name.notnilp());
  string lispified = lispify_symbol_name(name->get());
  return SimpleBaseString_O::make(lispified);
};

/*!
* Arguments
- name :: A string.
- package_str :: Return the package part.
- symbol_str :: Return the symbol part.
* Description
Convert strings that have the form pkg:name or pkg__name into a package name string and a symbol name string, run them through lispify_symbol_name and then recombine them as pkg:name.
*/
void colon_split(const string& name, string& package_str, string& symbol_str)
{
  std::size_t found = name.find(":");
  if ( found != std::string::npos ) {
    package_str = name.substr(0,found);
    symbol_str = name.substr(found+1,std::string::npos);
    return;
  }
  SIMPLE_ERROR(BF("Could not convert %s into package:symbol_name") % name);
}

CL_LAMBDA("name &optional (package \"\")");
CL_DOCSTRING(R"(Intern the package:name or name/package combination)");
CL_DEFUN Symbol_sp core__magic_intern(const string& name, const string& package)
{
  std::string pkg_sym = magic_name(name,package);
  std::string sym;
  std::string pkg;
  colon_split(pkg_sym,pkg,sym);
  Package_sp p = _lisp->findPackage(pkg);
  return p->intern(SimpleBaseString_O::make(sym));
}


/*!
* Arguments
- name :: A string.
- package_name :: A string
* Description
Convert strings that have the form pkg:name or pkg__name into a package name string and a symbol name string, run them through lispify_symbol_name and then recombine them as pkg:name. If
package_name is not the empty string in either of the above cases, signal an error.
If the name has neither of the above forms then use the package_name to construct package_name:name after lispifying name and return that.
*/
std::string magic_name(const std::string& name,const std::string& package_name)
{
  std::size_t found = name.find(":");
  if ( found != std::string::npos ) {
    if ( package_name != "" ) {
      SIMPLE_ERROR(BF("Cannot convert %s into a symbol name because package_name %s was provided") % name % package_name);
    }
    std::string pkg_str = name.substr(0,found);
    std::string symbol_str = name.substr(found+1,std::string::npos);
    pkg_str = lispify_symbol_name(pkg_str);
    symbol_str = lispify_symbol_name(symbol_str);
    stringstream ss;
    ss << pkg_str << ":" << symbol_str;
    return ss.str();
  }
  std::size_t found2 = name.find("__");
  if ( found2 != std::string::npos ) {
    if ( package_name != "" ) {
      SIMPLE_ERROR(BF("Cannot convert %s into a symbol name because package_name %s was provided") % name % package_name);
    }
    std::string pkg_str = name.substr(0,found2);
    std::string symbol_str = name.substr(found2+2,std::string::npos);
    pkg_str = lispify_symbol_name(pkg_str);
    symbol_str = lispify_symbol_name(symbol_str);
    stringstream ss;
    ss << pkg_str << ":" << symbol_str;
    return ss.str();
  }
  if ( package_name != "" ) {
    std::string symbol_str = lispify_symbol_name(name);
    stringstream ss;
    ss << package_name << ":" << symbol_str;
    return ss.str();
  }
  SIMPLE_ERROR(BF("Cannot convert %s into a package:name form because no package_name was provided") % name);
}


CL_LAMBDA("name &optional (package \"\")");
CL_DECLARE();
CL_DOCSTRING(R"doc(* Arguments
- name :: A string.
- package :: A string
* Description
Convert strings that have the form pkg:name or pkg__name into a package name string and a symbol name string, 
run them through lispify_symbol_name and then recombine them as pkg:name.
Then split them again (sorry) and return (values pkg:sym pkg sym).)doc");
CL_DEFUN T_mv core__magic_name(const std::string& name, const std::string& package) {
  std::string pkg_sym = magic_name(name,package);
  std::string sym;
  std::string pkg;
  colon_split(pkg_sym,pkg,sym);
  return Values(SimpleBaseString_O::make(pkg_sym),SimpleBaseString_O::make(pkg),SimpleBaseString_O::make(sym));
};


MultipleValues &lisp_multipleValues() {
  //	return &(_lisp->multipleValues());
  return my_thread->_MultipleValues;
}

[[noreturn]]void errorFormatted(boost::format fmt) {
  TRY_BOOST_FORMAT_STRING(fmt, fmt_str);
  dbg_hook(fmt_str.c_str());
  core__invoke_internal_debugger(_Nil<core::T_O>());
}

[[noreturn]]void errorFormatted(const string &msg) {
  dbg_hook(msg.c_str());
  core__invoke_internal_debugger(_Nil<core::T_O>());
}

[[noreturn]]void errorFormatted(const char *msg) {
  dbg_hook(msg);
  core__invoke_internal_debugger(_Nil<core::T_O>());
}

string lisp_currentPackageName() {
  string pkg = _lisp->getCurrentPackage()->packageName();
  return pkg;
}

Symbol_sp lispify_intern_keyword(string const &name) {
  string lispName = lispify_symbol_name(name);
  return _lisp->internKeyword(lispName);
}

Symbol_sp lisp_upcase_intern(string const &name, string const &packageName) {
  string lispName = stringUpper(name);
  return _lisp->intern(lispName, packageName);
}

Symbol_sp lisp_upcase_intern_export(string const &name, string const &packageName) {
  Symbol_sp sym = lisp_upcase_intern(name, packageName);
  sym->exportYourself();
  return sym;
}

typedef enum { ignore, upperCaseAlpha } NextCharTest;
bool lispify_match(const char *&cur, const char *match, NextCharTest nextCharTest = ignore) {
  const char *ccur = cur;
  while (*match) {
    if (*ccur == '\0')
      return false;
    if (*ccur != *match)
      return false;
    ++ccur;
    ++match;
  }
  if ( nextCharTest == ignore ) {
    cur = ccur;
    return true;
  }
  if ( nextCharTest == upperCaseAlpha ) {
    if (*match && isalpha(*match) && isupper(*match)) {
      cur = ccur;
      return true;
    }
    return false;
  }
  SIMPLE_ERROR(BF("Unknown nextCharTest(%d)") % nextCharTest );
}

/*! This checks for names like CXXObject and will convert them to CXX-OBJECT.
It looks for [A-Z]+[A-Z][a-z] - a run of more than 
two upper case characters followed by a lower case
alpha character. If it sees this it returns true and the first N-1 characters of the
upper case sequence and it advances cur to the last upper case character */

 bool lispify_more_than_two_upper_case_followed_by_lower_case(const char*&cur, stringstream& accumulate)
{
  const char *ccur = cur;
  stringstream ss_acc;
  while (*ccur) {
    if (isalpha(*ccur)) {
      if (isupper(*ccur)) {
        if (!*(ccur+1)) return false;
        if (isalpha(*(ccur+1))) {
          if (isupper(*(ccur+1))) {
            if (!*(ccur+2) ) return false;
            if (isalpha(*(ccur+2))) {
              if ( islower(*(ccur+2))) {
                accumulate << ss_acc.str() << '-';
                cur = ccur;
                return true;
              } else {
                ss_acc << *ccur;
                ++ccur;
                continue;
              }
            } else return false;
          } else return false;
        } else return false;
      } else return false;
    } else return false;
  }
  return false;
}

string lispify_symbol_name(const string &s) {
  bool new_rule_change = false;
  LOG(BF("lispify_symbol_name pass1 source[%s]") % s);
  /*! Temporarily store the string in a buffer */
#define LISPIFY_BUFFER_SIZE 1024
  char buffer[LISPIFY_BUFFER_SIZE];
  int name_length = s.size();
  if ((name_length+2)>=LISPIFY_BUFFER_SIZE) {
    printf("%s:%d The C++ string is too large to lispify - increase LISPIFY_BUFFER_SIZE to at least %d\n", __FILE__, __LINE__, (name_length+2));
    abort();
  }
  memset(buffer,0,name_length+2);
  strncpy(buffer,s.c_str(),name_length);
  stringstream stream_pass1;
  const char *cur = buffer;
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
  LOG(BF("lispify_symbol_name pass2 source[%s]") % str_pass2);
  const char *start_pass2 = str_pass2.c_str();
  cur = start_pass2;
  while (*cur) {
    if (islower(*cur) && isalpha(*(cur + 1)) && isupper(*(cur + 1))) {
      char lcur = *cur;
      char ucur = toupper(lcur);
      stream_pass2 << ucur << "-";
      ++cur;
      continue;
    }
    stream_pass2 << (char)(toupper(*cur));
    ++cur;
  }
  LOG(BF("lispify_symbol_name output[%s]") % stream_pass2.str());
  if ( new_rule_change ) {
    printf("%s:%d The symbol |%s| changed according to the new rule ([A-Z]+)([A-Z][a-z]) -> \\1-\\2-\n", __FILE__, __LINE__, stream_pass2.str().c_str() );
  }
  return stream_pass2.str();
}

string symbol_symbolName(Symbol_sp sym) {
  return sym->symbolNameAsString();
}

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
    tc = core::Fixnum_dummy_O::static_class;
    //	    return core::Fixnum_O::___staticClass;
  } else if (o.characterp()) {
    tc = core::Character_dummy_O::static_class;
  } else if (o.single_floatp()) {
    tc = core::SingleFloat_dummy_O::static_class;
  } else if (o.consp()) {
    tc = core::Cons_O::static_class;
  } else if (o.generalp()) {
    General_sp go(o.unsafe_general());
    unlikely_if ( go.nilp() ) {
      tc = core::Null_O::static_class;
    }
    tc = go->_instanceClass();
  } else if (o.valistp()) {
    // What do I tc = for this?
    tc = core::VaList_dummy_O::static_class;
  } else {
    SIMPLE_ERROR(BF("Add support for unknown (immediate?) object to lisp_instance_class obj = %p") % (void*)(o.raw_()));
  }
  return gc::As_unsafe<Instance_sp>(tc);
}

Instance_sp lisp_static_class(T_sp o) {
  if ( o.generalp() ) {
    General_sp go(o.unsafe_general());
    if (go.nilp()) {
      return core::Null_O::static_class;
    }
    return go->__class();
  }
  SIMPLE_ERROR(BF("Add support for more classes to lisp_static_class"));
}

string _rep_(T_sp obj) {
//#define USE_WRITE_OBJECT
#if defined(USE_WRITE_OBJECT)
  T_sp sout = clasp_make_string_output_stream();
  write_object(obj, sout);
  return cl__get_output_stream_string(sout).as<String_O>()->get();
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
    VaList_sp vobj((gctools::Tagged)obj.raw_());
    List_sp l = core__list_from_va_list(vobj);
    return _rep_(l);
  } else if (obj.unboundp()) {
    return "!UNBOUND!";
  }
  stringstream ss;
  ss << "WTF-object@" << (void*)obj.raw_();
  return ss.str();
#endif
}

void lisp_throwUnexpectedType(T_sp offendingObject, Symbol_sp expectedTypeId) {
  Symbol_sp offendingTypeId = cl__class_of(offendingObject)->_className();
  SIMPLE_ERROR(BF("Expected %s of class[%s] to be subclass of class[%s]") % _rep_(offendingObject) % _rep_(offendingTypeId) % _rep_(expectedTypeId));
}

string lisp_classNameAsString(Instance_sp c) {
  return c->_classNameAsString();
}

void lisp_throwLispError(const string &str) {
  SIMPLE_ERROR(BF("%s") % str);
}

void lisp_throwLispError(const boost::format &fmt) {
  TRY_BOOST_FORMAT_STRING(fmt, fmt_str);
  SIMPLE_ERROR(BF(fmt_str));
}

bool lisp_debugIsOn(const char *fileName, uint debugFlags) {
  bool ret = _lisp->debugLog().isOn(fileName, debugFlags);
  return ret;
}

DebugStream *lisp_debugLog() {
  return &(_lisp->debugLog());
}

uint lisp_hash(uintptr_clasp_t x) {
  HashGenerator hg;
  hg.addPart(x);
  return hg.hash();
}

T_sp lisp_true() {
  return _lisp->_true();
}

T_sp lisp_false() {
  return _lisp->_false();
}

string lisp_rep(T_sp obj) {
  return _rep_(obj);
}

bool lisp_CoreBuiltInClassesInitialized() {
  return _lisp->CoreBuiltInClassesInitialized();
}

bool lisp_BuiltInClassesInitialized() {
  return _lisp->BuiltInClassesInitialized();
}

T_sp lisp_boot_findClassBySymbolOrNil(Symbol_sp classSymbol) {
  Instance_sp mc = gc::As<Instance_sp>(eval::funcall(cl::_sym_findClass, classSymbol, _lisp->_true()));
  return mc;
}

// void lisp_defineInitializationArgumentsForClassSymbol(Lisp_sp lisp, const string& argumentString, uint classSymbol)
// {
//     Instance_sp mc = lisp->classFromClassSymbol(classSymbol);
//     mc->__setLambdaListHandlerString(argumentString);
// }

void lisp_addClassSymbol(Symbol_sp classSymbol,
                   gctools::smart_ptr<Creator_O> cb,
                   Symbol_sp base1ClassSymbol)
{
  DEPRECATED();
  printf("%s:%d:%s    lisp_addClass   %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(classSymbol).c_str());
  _lisp->addClassSymbol(classSymbol, cb, base1ClassSymbol); //, base2ClassSymbol);
}
void lisp_addClass(Symbol_sp classSymbol) {
  DEPRECATED();
  //	_lisp->addClass(classSymbol);
}

List_sp lisp_parse_arguments(const string &packageName, const string &args) {
  if (args == "")
    return _Nil<T_O>();
  Package_sp pkg = gc::As<Package_sp>(_lisp->findPackage(packageName, true));
  ChangePackage changePackage(pkg);
  SimpleBaseString_sp ss = SimpleBaseString_O::make(args);
  Stream_sp str = cl__make_string_input_stream(ss, make_fixnum(0), _Nil<T_O>());
  Reader_sp reader = Reader_O::create(str);
  T_sp osscons = reader->primitive_read(true, _Nil<T_O>(), false);
  List_sp sscons = osscons;
  return sscons;
}

List_sp lisp_parse_declares(const string &packageName, const string &declarestring) {
  if (declarestring == "")
    return _Nil<T_O>();
  Package_sp pkg = gc::As<Package_sp>(_lisp->findPackage(packageName, true));
  ChangePackage changePackage(pkg);
  SimpleBaseString_sp ss = SimpleBaseString_O::make(declarestring);
  Stream_sp str = cl__make_string_input_stream(ss, make_fixnum(0), _Nil<T_O>());
  Reader_sp reader = Reader_O::create(str);
  List_sp sscons = reader->primitive_read(true, _Nil<T_O>(), false);
  return sscons;
}

LambdaListHandler_sp lisp_function_lambda_list_handler(List_sp lambda_list, List_sp declares, std::set<int> pureOutValues) {
  LambdaListHandler_sp llh = LambdaListHandler_O::create(lambda_list, declares, cl::_sym_function, pureOutValues);
  return llh;
}


/*! Insert the package qualified class_symbol into the lambda list wherever a ! is seen */
string fix_method_lambda(core::Symbol_sp class_symbol, const string& lambda)
{
  stringstream new_lambda;
  for ( auto c : lambda ) {
    if ( c == '!' ) {
      new_lambda << class_symbol->formattedName(true);
    } else {
      new_lambda << c;
    }
  }
  return new_lambda.str();
}


SYMBOL_EXPORT_SC_(KeywordPkg, body);
SYMBOL_EXPORT_SC_(KeywordPkg, lambda_list_handler);
SYMBOL_EXPORT_SC_(KeywordPkg, docstring);
void lisp_defineSingleDispatchMethod(T_sp name,
                                     Symbol_sp classSymbol,
                                     BuiltinClosure_sp method_body,
                                     size_t TemplateDispatchOn,
                                     bool useTemplateDispatchOn,
                                     const string &raw_arguments,
                                     const string &declares,
                                     const string &docstring,
                                     bool autoExport,
                                     int number_of_required_arguments,
                                     const std::set<int> pureOutIndices) {
  string arguments = fix_method_lambda(classSymbol,raw_arguments);
  Instance_sp receiver_class = gc::As<Instance_sp>(eval::funcall(cl::_sym_findClass, classSymbol, _lisp->_true()));
  Symbol_sp className = receiver_class->_className();
  List_sp ldeclares = lisp_parse_declares(gc::As<Package_sp>(className->getPackage())->getName(), declares);
  // NOTE: We are compiling the llhandler in the package of the class - not the package of the
  // method name  -- sometimes the method name will belong to another class (ie: core:--init--)
  LambdaListHandler_sp llhandler;
  size_t single_dispatch_argument_index;
  if (useTemplateDispatchOn) single_dispatch_argument_index = TemplateDispatchOn;
  if (arguments == "" && number_of_required_arguments >= 0) {
    // If the arguments string is empty and number_of_required_arguments is >= 0 then create
    // a LambdaListHandler that supports that number of required arguments
    llhandler = LambdaListHandler_O::create(number_of_required_arguments, pureOutIndices);
  } else if (arguments != "") {
    List_sp llraw = lisp_parse_arguments(gc::As<Package_sp>(className->getPackage())->getName(), arguments);
    T_mv mv_llprocessed = LambdaListHandler_O::process_single_dispatch_lambda_list(llraw, true);
    T_sp tllproc = coerce_to_list(mv_llprocessed); // slice
    Symbol_sp sd_symbol = gc::As<Symbol_sp>(mv_llprocessed.valueGet_(1));
    Symbol_sp specializer_symbol = gc::As<Symbol_sp>(mv_llprocessed.valueGet_(2));
    List_sp llproc = coerce_to_list(tllproc);
    if (specializer_symbol.notnilp() && specializer_symbol != classSymbol) {
      SIMPLE_ERROR(BF("Mismatch between hard coded class[%s] and"
                      " specializer_symbol[%s] for function %s with argument list: %s") %
                   classSymbol->fullName() % specializer_symbol->fullName() % _rep_(name) % _rep_(llraw));
    }
    llhandler = lisp_function_lambda_list_handler(llproc, ldeclares, pureOutIndices);
    if (sd_symbol.notnilp()) {
      single_dispatch_argument_index = llhandler->single_dispatch_on_argument(sd_symbol);
      if (useTemplateDispatchOn) {
        if (single_dispatch_argument_index != TemplateDispatchOn) {
          SIMPLE_ERROR(BF("There is a mismatch between the single_dispatch_argument_index %d and TemplateDispatchOn %d") % single_dispatch_argument_index % TemplateDispatchOn);
        }
      }
      // if dispatching off of something other than the first argument and this isn't a (setf xxx) function - then error
      if (single_dispatch_argument_index != 0 && !(name.consp() && CONS_CAR(name)==cl::_sym_setf)) {
        SIMPLE_ERROR(BF("There is no support for dispatching on anything but the first argument -"
                        " wrap this virtual function in a regular function and do the dispatch yourself  %s::%s") %
                     _rep_(className) % _rep_(name));
      }
    }
  } else {
    SIMPLE_ERROR(BF("No arguments were provided and number_of_required_arguments is %d") % number_of_required_arguments);
  }
  if (TemplateDispatchOn != single_dispatch_argument_index) {
    SIMPLE_ERROR(BF("Mismatch between single_dispatch_argument_index[%d] from lambda_list and TemplateDispatchOn[%d] for class %s  method: %s  lambda_list: %s") % single_dispatch_argument_index % TemplateDispatchOn % _rep_(classSymbol) % _rep_(name) % arguments);
  }
  LOG(BF("Interned method in class[%s]@%p with symbol[%s] arguments[%s] - autoexport[%d]") % receiver_class->instanceClassName() % (receiver_class.get()) % sym->fullName() % arguments % autoExport);
  
  T_sp docStr = _Nil<T_O>();
  if (docstring!="") docStr = SimpleBaseString_O::make(docstring);
  T_sp gfn = core__ensure_single_dispatch_generic_function(name, llhandler,autoExport,single_dispatch_argument_index); // Ensure the single dispatch generic function exists
  (void)gfn;                                                         // silence compiler warning
  method_body->finishSetup(llhandler);
  method_body->setf_sourcePathname(_Nil<T_O>());
  method_body->setf_lambdaList(llhandler->lambdaList());
  method_body->setf_docstring(docStr);
  method_body->setf_declares(ldeclares);
  ASSERT(llhandler || llhandler.notnilp());
#ifdef DEBUG_PROGRESS
  printf("%s:%d lisp_defineSingleDispatchMethod sym: %s\n", __FILE__, __LINE__, _rep_(sym).c_str());
#endif
  core__ensure_single_dispatch_method(gfn, name, receiver_class, llhandler, ldeclares, docStr, method_body);
}

void lisp_throwIfBuiltInClassesNotInitialized() {
  _lisp->throwIfBuiltInClassesNotInitialized();
}

Instance_sp lisp_classFromClassSymbol(Symbol_sp classSymbol) {
  return gc::As<Instance_sp>(eval::funcall(cl::_sym_findClass, classSymbol, _lisp->_true()));
}


  


/*! If the name has the structure XXX:YYY or XXX::YYY then intern YYY in package XXX either
      exported or not respectively.   If there is no package prefix then use the defaultPackageName */
Symbol_sp lispify_intern(const string &name, const string &defaultPackageName, bool exportSymbol) {
  string lispName = lispify_symbol_name(name);
  string packageName = lispify_symbol_name(defaultPackageName);
  Symbol_sp sym = _lisp->internWithDefaultPackageName(packageName, lispName);
  if (exportSymbol) {
    sym->exportYourself();
  }
  return sym;
}

void lisp_defun(Symbol_sp sym,
                const string &packageName,
                BuiltinClosure_sp fc,
                const string &arguments,
                const string &declarestring,
                const string &docstring,
                const string &sourceFile,
                int lineNumber,
                int number_of_required_arguments,
                const std::set<int> &skipIndices) {
  List_sp ldeclares = lisp_parse_declares(packageName, declarestring); // get the declares but ignore them for now
  (void)ldeclares;                                                     // suppress warning
  LambdaListHandler_sp llh;
  if ((arguments == "" || arguments == "()") && number_of_required_arguments >= 0) {
    llh = LambdaListHandler_O::create(number_of_required_arguments, skipIndices);
  } else {
    List_sp ll = lisp_parse_arguments(packageName, arguments);
    llh = lisp_function_lambda_list_handler(ll, _Nil<T_O>(), skipIndices);
  }
  fc->finishSetup(llh);
  fc->setSourcePosInfo(SimpleBaseString_O::make(sourceFile), 0, lineNumber, 0);
  Function_sp func = fc;
  sym->setf_symbolFunction(func);
  sym->exportYourself();
  T_sp tdocstring = _Nil<T_O>();
  if (docstring!="") tdocstring = core::SimpleBaseString_O::make(docstring);
  fc->setf_lambdaList(llh->lambdaList());
  fc->setf_declares(ldeclares);
  fc->setf_docstring(tdocstring);
  core::ext__annotate(sym,cl::_sym_documentation,cl::_sym_function, tdocstring);
  core::ext__annotate(func,cl::_sym_documentation,cl::_sym_function, tdocstring);
}

// identical to above except for using setSetfFdefinition.
void lisp_defun_setf(Symbol_sp sym,
                     const string &packageName,
                     BuiltinClosure_sp fc,
                     const string &arguments,
                     const string &declarestring,
                     const string &docstring,
                     const string &sourceFile,
                     int lineNumber,
                     int number_of_required_arguments,
                     const std::set<int> &skipIndices) {
  List_sp ldeclares = lisp_parse_declares(packageName, declarestring); // get the declares but ignore them for now
  (void)ldeclares;                                                     // suppress warning
  LambdaListHandler_sp llh;
  if ((arguments == "" || arguments == "()") && number_of_required_arguments >= 0) {
    llh = LambdaListHandler_O::create(number_of_required_arguments, skipIndices);
  } else {
    List_sp ll = lisp_parse_arguments(packageName, arguments);
    llh = lisp_function_lambda_list_handler(ll, _Nil<T_O>(), skipIndices);
  }
  fc->finishSetup(llh);
  fc->setSourcePosInfo(SimpleBaseString_O::make(sourceFile), 0, lineNumber, 0);
  T_sp tdocstring = _Nil<T_O>();
  if (docstring!="") tdocstring = core::SimpleBaseString_O::make(docstring);
  fc->setf_sourcePathname(_Nil<T_O>());
  fc->setf_lambdaList(llh->lambdaList());
  fc->setf_declares(ldeclares);
  fc->setf_docstring(tdocstring);
  Function_sp func = fc;
  sym->setSetfFdefinition(func);
  sym->exportYourself();
  core::ext__annotate(sym,cl::_sym_documentation,cl::_sym_function, tdocstring);
  core::ext__annotate(func,cl::_sym_documentation,cl::_sym_function, tdocstring);
}

void lisp_defmacro(Symbol_sp sym,
                   const string &packageName,
                   BuiltinClosure_sp f,
                   const string &arguments,
                   const string &declarestring,
                   const string &docstring) {
  LOG(BF("Adding form[%s] with arguments[%s]") % name % arguments);
  List_sp ll = lisp_parse_arguments(packageName, arguments);
  List_sp ldeclares = lisp_parse_declares(packageName, declarestring);
  (void)ldeclares;
  LambdaListHandler_sp llh = lisp_function_lambda_list_handler(ll, _Nil<T_O>());
  f->finishSetup(llh);
  sym->setf_macroP(true);
  f->setf_sourcePathname(_Nil<T_O>());
  f->setf_lambdaList(llh->lambdaList());
  f->setf_declares(ldeclares);
  T_sp tdocstring = _Nil<T_O>();
  if (docstring!="") tdocstring = core::SimpleBaseString_O::make(docstring);
  f->setf_docstring(tdocstring);
  Function_sp func = f;
  //    Package_sp package = lisp->getPackage(packageName);
  //    package->addFunctionForLambdaListHandlerCreation(func);
  sym->setf_symbolFunction(func);
  sym->exportYourself();
}


Symbol_sp lisp_internKeyword(const string &name) {
  if (name == "")
    return _Nil<Symbol_O>();
  return _lisp->internKeyword(name);
}

Symbol_sp lisp_intern(const string &name) {
  if (name == "")
    return _Nil<Symbol_O>();
  return _lisp->intern(name);
}

Symbol_sp lisp_intern(const string &name, const string &pkg) {
  if (name == "")
    return _Nil<Symbol_O>();
  return _lisp->internWithPackageName(pkg, name);
}

string symbol_fullName(Symbol_sp s) {
  return s->fullName();
}

core::T_sp lisp_registerSourceInfo(T_sp obj, SourceFileInfo_sp sfo, size_t filePos, int lineno, int column) {
  T_sp tdb = _lisp->sourceDatabase();
  if (SourceManager_sp db = tdb.asOrNull<SourceManager_O>()) {
    return db->registerSourceInfo(obj, sfo, filePos, lineno, column);
  }
  return _Nil<T_O>();
}

core::T_sp lisp_registerSourcePosInfo(T_sp obj, SourcePosInfo_sp spi) {
  T_sp tdb = _lisp->sourceDatabase();
  if (SourceManager_sp db = tdb.asOrNull<SourceManager_O>()) {
    return db->registerSourcePosInfo(obj, spi);
  }
  return _Nil<T_O>();
}

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

void lisp_extendSymbolToEnumConverter(SymbolToEnumConverter_sp conv, Symbol_sp const &name, Symbol_sp const &archiveName, int value) {
  conv->addSymbolEnumPair(name, archiveName, value);
}

bool lisp_isGlobalInitializationAllowed(Lisp_sp lisp) {
  return lisp->isGlobalInitializationAllowed();
}

void lisp_debugLogWrite(char const *fileName, const char *functionName, uint lineNumber, uint col, const boost::format &fmt, uint debugFlags) {
  TRY_BOOST_FORMAT_STRING(fmt, fmt_str);
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

void appendVectorStrings(VectorStrings &app, VectorStrings strs) {
  VectorStrings::iterator vs;
  for (vs = strs.begin(); vs != strs.end(); vs++) {
    app.push_back(*vs);
  }
}

string stripCharacters(const string &orig, const string &stripSet) {
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

string escapeWhiteSpace(const string &inp) {
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

string unEscapeWhiteSpace(string const &inp) {
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
        THROW_HARD_ERROR(BF("Illegal escaped char[%s]") % c);
        break;
      }
    } else {
      sout << c;
    }
  }
  return sout.str();
}

// Trim Both leading and trailing spaces
string trimWhiteSpace(const string &str) {
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

Function_sp lisp_symbolFunction(Symbol_sp sym) {
  return sym->symbolFunction();
}

T_sp lisp_symbolValue(Symbol_sp sym) {
  return sym->symbolValue();
}

string lisp_symbolNameAsString(Symbol_sp sym) {
  if (sym.nilp())
    return "NIL";
  return sym->symbolNameAsString();
}

T_sp lisp_createStr(const string &s) {
  return SimpleBaseString_O::make(s);
}

T_sp lisp_createFixnum(int fn) {
  return make_fixnum(fn);
}

SourcePosInfo_sp lisp_createSourcePosInfo(const string &fileName, size_t filePos, int lineno) {
  SimpleBaseString_sp fn = SimpleBaseString_O::make(fileName);
  SourceFileInfo_mv sfi_mv = core__source_file_info(fn);
  SourceFileInfo_sp sfi = sfi_mv;
  Fixnum_sp handle = gc::As<Fixnum_sp>(sfi_mv.valueGet_(1));
  int sfindex = unbox_fixnum(handle);
  return SourcePosInfo_O::create(sfindex, filePos, lineno, 0);
}

T_sp lisp_createList(T_sp a1) { return Cons_O::create(a1, _Nil<T_O>()); }
T_sp lisp_createList(T_sp a1, T_sp a2) { return Cons_O::createList(a1, a2); };
T_sp lisp_createList(T_sp a1, T_sp a2, T_sp a3) { return Cons_O::createList(a1, a2, a3); };
T_sp lisp_createList(T_sp a1, T_sp a2, T_sp a3, T_sp a4) { return Cons_O::createList(a1, a2, a3, a4); };
T_sp lisp_createList(T_sp a1, T_sp a2, T_sp a3, T_sp a4, T_sp a5) { return Cons_O::createList(a1, a2, a3, a4, a5); };
T_sp lisp_createList(T_sp a1, T_sp a2, T_sp a3, T_sp a4, T_sp a5, T_sp a6) { return Cons_O::createList(a1, a2, a3, a4, a5, a6); };
T_sp lisp_createList(T_sp a1, T_sp a2, T_sp a3, T_sp a4, T_sp a5, T_sp a6, T_sp a7) { return Cons_O::createList(a1, a2, a3, a4, a5, a6, a7); }
T_sp lisp_createList(T_sp a1, T_sp a2, T_sp a3, T_sp a4, T_sp a5, T_sp a6, T_sp a7, T_sp a8) { return Cons_O::createList(a1, a2, a3, a4, a5, a6, a7, a8); }

[[noreturn]] void lisp_error_no_stamp(void* ptr)
{
  gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(gctools::ClientPtrToBasePtr(ptr));
  SIMPLE_ERROR(BF("This General_O object %p does not return a stamp because its subclass should overload get_stamp_() and return one  - the subclass header stamp value is %lu") % ((void*)ptr) % header->stamp());
}

void lisp_errorCannotAllocateInstanceWithMissingDefaultConstructor(T_sp aclass_symbol)
{
  ASSERT(aclass_symbol);
  Symbol_sp cclassSymbol = aclass_symbol.as<Symbol_O>();
  SIMPLE_ERROR(BF("You cannot allocate a %s with no arguments because it is missing a default constructor") % _rep_(cclassSymbol));
}
void lisp_errorExpectedTypeSymbol(Symbol_sp typeSym, T_sp datum) {
  TYPE_ERROR(datum, typeSym);
}

size_t global_error_simple_depth = 0;
struct ErrorSimpleDepthCounter {
  ErrorSimpleDepthCounter(const std::string msg) {
    ++global_error_simple_depth;
    if ( global_error_simple_depth > 20 ) {
      printf("%s:%d %s", __FILE__, __LINE__, msg.c_str());
    }
  }
  ~ErrorSimpleDepthCounter() {
    --global_error_simple_depth;
  }
};


#define MAX_SPRINTF_BUFFER 1024
[[noreturn]]void lisp_error_sprintf(const char* fileName, int lineNumber, const char* functionName, const char* fmt, ... ) {
  va_list args;
  va_start(args,fmt);
  char buffer[MAX_SPRINTF_BUFFER];
  vsnprintf(buffer,MAX_SPRINTF_BUFFER,fmt,args);
  va_end(args);
  stringstream ss;
  ss << "In " << functionName << " " << fileName << " line " << lineNumber << std::endl;
  ss << buffer;
  ErrorSimpleDepthCounter counter(ss.str());
  if (!_sym_signalSimpleError) {
    printf("%s:%d %s\n", __FILE__, __LINE__, ss.str().c_str());
    throw(HardError("System starting up - debugger not available yet: "+ss.str()));
  }
  if (!_sym_signalSimpleError->fboundp()) {
    printf("%s:%d %s\n", __FILE__, __LINE__, ss.str().c_str());
    dbg_hook(ss.str().c_str());
    if (my_thread->_InvocationHistoryStackTop == NULL) {
      throw(HardError("System starting up - debugger not available yet: "+ ss.str()));
    }
    LispDebugger dbg;
    dbg.invoke();
  }
  SYMBOL_EXPORT_SC_(ClPkg, programError);
  eval::funcall(_sym_signalSimpleError,
                core::_sym_simpleProgramError,    //arg0
                _Nil<T_O>(),              // arg1
                SimpleBaseString_O::make(buffer), // arg2
                _Nil<T_O>());
  UNREACHABLE();
}



NOINLINE void lisp_error_simple(const char *functionName, const char *fileName, int lineNumber, const boost::format &fmt) {
  stringstream ss;
  ss << "In " << functionName << " " << fileName << " line " << lineNumber << std::endl;
  ss << fmt.str();
  ErrorSimpleDepthCounter counter(ss.str());
  if (!_sym_signalSimpleError) {
    printf("%s:%d %s\n", __FILE__, __LINE__, ss.str().c_str());
    throw(HardError("System starting up - debugger not available yet: "+ss.str()));
  }
  if (!_sym_signalSimpleError->fboundp()) {
    printf("%s:%d %s\n", __FILE__, __LINE__, ss.str().c_str());
    dbg_hook(ss.str().c_str());
    if (my_thread->_InvocationHistoryStackTop == NULL) {
      throw(HardError("System starting up - debugger not available yet: "+ ss.str()));
    }
    LispDebugger dbg;
    dbg.invoke();
  }
  SYMBOL_EXPORT_SC_(ClPkg, programError);
  eval::funcall(_sym_signalSimpleError,
                core::_sym_simpleProgramError,    //arg0
                _Nil<T_O>(),              // arg1
                SimpleBaseString_O::make(fmt.str()), // arg2
                _Nil<T_O>());
  UNREACHABLE();
}

[[noreturn]] void lisp_error(T_sp datum, T_sp arguments) {
  if (!cl::_sym_error->fboundp()) {
    stringstream ss;
    ss << "Error " << _rep_(datum) << " initializers: " << _rep_(arguments) << std::endl;
    printf("%s:%d lisp_error ->\n %s\n", __FILE__, __LINE__, ss.str().c_str());
    LispDebugger dbg;
    dbg.invoke();
  }
  eval::applyLastArgsPLUSFirst(cl::_sym_error, arguments, datum);
  UNREACHABLE();
}

string stringUpper(const string &s) {
  LOG(BF("Converting string(%s) to uppercase") % s);
  stringstream ss;
  for (uint si = 0; si < s.length(); si++) {
    ss << (char)(toupper(s[si]));
  }
  LOG(BF("Returning stringUpper(%s)") % ss.str());
  return ss.str();
}

string stringUpper(const char *s) {
  LOG(BF("Converting const char*(%s) to uppercase") % s);
  stringstream ss;
  for (; *s; s++) {
    ss << (char)(toupper(*s));
  }
  LOG(BF("Returning stringUpper(%s)") % ss.str());
  return ss.str();
}

T_sp lisp_ArgArrayToCons(int nargs, ArgArray args) {
  Cons_O::CdrType_sp first = _Nil<Cons_O::CdrType_O>();
  Cons_O::CdrType_sp *curP = &first;
  //        gctools::StackRootedPointerToSmartPtr<Cons_O::CdrType_O> cur(&first);
  for (int i(0); i < nargs; ++i) {
    Cons_sp one = Cons_O::create(gctools::smart_ptr<core::T_O>((gctools::Tagged)(args[i])));
    *curP = one;          // cur.setPointee(one); //*cur = one;
    curP = one->cdrPtr(); // cur.setPointer(one->cdrPtr()); // cur = one->cdrPtr();
  }
  return first;
}

vector<string> split(const string &str, const string &delimiters) {
  vector<string> parts;
  tokenize(str, parts, delimiters);
  return parts;
}

/*! DONT PUT DEBUGGING CODE IN TOKENIZE!!!!  IT IS USED BY DebugStream
     */
void tokenize(const string &str,
              vector<string> &tokens,
              const string &delimiters) {
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

string searchAndReplaceString(const string &str, const string &search, const string &replace, Lisp_sp lisp) {
  string result;
  string::size_type pos = 0;
  result = str;
  while ((pos = result.find(search, pos)) != string::npos) {
    result.replace(pos, search.size(), replace);
    pos++;
  }
  return result;
}

void queueSplitString(const string &str,
                      std::queue<string> &tokens,
                      const string &delimiters) {
  //    LOG(BF("foundation.cc::tokenize-- Entered") );
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
bool wildcmp(string const &sWild,
             string const &sRegular) {
  const char *wild, *mp, *cp;
  const char *regular;

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

void StringStack::pop() {
  HARD_ASSERT(this->parts.size() > 0);
  this->parts.pop_back();
};

string StringStack::all(const string &separator) {
  stringstream ss;
  ss.str("");
  if (this->parts.size() > 0) {
    ss << this->parts[0];
  }
  for (uint i = 1; i < this->parts.size(); i++) {
    ss << separator;
    ss << this->parts[i];
  }
  return ss.str();
};

const char *trimSourceFilePathName(const char *longName) {
  if (longName == NULL)
    return NULL;
  const char *cp = longName + strlen(longName);
  while (cp > longName && *cp != '/')
    --cp;
  if (*cp == '/')
    ++cp;
  return cp;
}

bool _ClassesAreInitialized = false;

void throwIfClassesNotInitialized(const Lisp_sp &lisp) {
  if (!_ClassesAreInitialized) {
    lisp->print(BF("Debug information was being written when classes have not yet been initialized"));
    lisp->print(BF("This should never happen."));
    lisp->print(BF("Run with the debugger and use the following commands:"));
    lisp->print(BF("l foundation.cc:1"));
    lisp->print(BF("search throwIfClassesNotInitialized"));
    lisp->print(BF("--> set a breakpoint in the if block"));
    lisp->print(BF("Then backtrace to find the offending initialization routine."));
    abort();
  }
}

};

namespace core {
size_t  debug_InvocationHistoryFrame = 0;
const char* debug_InvocationHistoryFrame_name = "";

CL_DEFUN void core__debug_invocation_history_frame(size_t v) {
  debug_InvocationHistoryFrame = v;
  const char* ihf = getenv("IHS");
  if (ihf==NULL) {
    ihf = "ESTIMATE-CODE-SIZE-1";
  }
  debug_InvocationHistoryFrame_name = ihf;
  printf("%s:%d     Will trap InvocationHistoryFrame at |%s|\n",__FILE__,__LINE__,debug_InvocationHistoryFrame_name); 
  printf("   ---------------   InvocationHistoryStack:\n");
  const InvocationHistoryFrame* cur = my_thread->_InvocationHistoryStackTop;
  size_t count= 0;
  while (cur!=NULL) {
    T_sp tfun = cur->function();
    if (gc::IsA<Function_sp>(tfun)) {
      Function_sp fun = gc::As_unsafe<Function_sp>(tfun);
      printf("    frame[%lu] @%p  function: %s\n", count, cur, _rep_(fun->functionName()).c_str());
    } else {
      printf("    frame[%lu] @%p  function: %s\n", count, cur, _rep_(tfun).c_str());
    }
    cur = cur->previous();
    ++count;
  }
}
  
};



