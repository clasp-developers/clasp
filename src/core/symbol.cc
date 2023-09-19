/*
    File: symbol.cc
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

#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/symbol.h>
#include <clasp/core/array.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/function.h>
#include <clasp/core/numbers.h>
#include <clasp/core/compiler.h>
#include <clasp/core/lispList.h>
#include <clasp/core/instance.h>
#include <clasp/core/package.h>
#include <clasp/core/lisp.h>
#include <clasp/core/wrappers.h>
#include <clasp/core/designators.h>

// Print more information about the symbol
#define VERBOSE_SYMBOLS 0

namespace core {

//    Symbol_sp 	_sym_nil;	// equivalent to nil<T_O>()
//    Symbol_sp 	_sym_t;		// equivalent to _lisp->_true()

CL_LAMBDA(sym);
CL_DECLARE();
CL_DOCSTRING(R"dx(Return the symbol plist)dx");
DOCGROUP(clasp);
CL_DEFUN List_sp cl__symbol_plist(Symbol_sp sym) {
  return sym->plist();
}

CL_LISPIFY_NAME("cl:symbol-plist");
CL_LAMBDA(plist sym);
CL_DECLARE();
CL_DOCSTRING(R"dx(Set the symbol plist)dx");
DOCGROUP(clasp);
CL_DEFUN_SETF List_sp core__set_symbol_plist(List_sp plist, Symbol_sp sym) {
  sym->setf_plist(plist);
  return plist;
}

CL_LAMBDA(sym);
CL_DECLARE();
CL_DOCSTRING(R"dx(Sequentially consistent atomic load of the symbol-plist)dx");
DOCGROUP(clasp);
CL_DEFUN List_sp core__atomic_symbol_plist(Symbol_sp sym) {
  return sym->atomic_plist();
}

CL_LAMBDA(nv symbol);
CL_DECLARE();
CL_DOCSTRING(R"dx(Sequentially consistent atomic store of the symbol-plist)dx");
DOCGROUP(clasp);
CL_DEFUN void core__atomic_set_symbol_plist(List_sp nv, Symbol_sp symbol) {
  symbol->atomic_setf_plist(nv);
}

CL_LAMBDA(cmp newv sym);
CL_DECLARE();
CL_DOCSTRING(R"dx(Compare-and-swap the symbol-plist)dx");
DOCGROUP(clasp);
CL_DEFUN List_sp core__cas_symbol_plist(List_sp cmp, List_sp newv, Symbol_sp sym) {
  return sym->cas_plist(cmp, newv);
}

CL_LAMBDA(sym indicator &optional default);
CL_DECLARE();
CL_DOCSTRING(R"dx(Return the value of a plist property)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__get(Symbol_sp sym, T_sp indicator, T_sp defval) {
  return cl__getf(sym->plist(), indicator, defval);
}

CL_LISPIFY_NAME("cl:get");
CL_LAMBDA(val sym indicator &optional default);
CL_DECLARE();
CL_DOCSTRING(R"dx(Set the value of a plist property)dx");
DOCGROUP(clasp);
CL_DEFUN_SETF T_sp core__putprop(T_sp val, Symbol_sp sym, T_sp indicator, T_sp defval) {
  (void)(defval); // unused
  sym->setf_plist(core__put_f(sym->plist(), val, indicator));
  return val;
}

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(boundp)dx");
DOCGROUP(clasp);
CL_DEFUN bool cl__boundp(Symbol_sp arg) {
  if (arg.nilp())
    return true;
  return arg->boundP();
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(symbolPackage)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__symbol_package(Symbol_sp arg) {
  return arg->homePackage();
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(symbolFunction)dx");
DOCGROUP(clasp);
CL_DEFUN Function_sp cl__symbol_function(Symbol_sp sym) {
  return sym->symbolFunction();
};


CL_LAMBDA(sym);
CL_DECLARE();
CL_DOCSTRING(R"dx(Return true if the symbol is dynamic/special)dx");
DOCGROUP(clasp);
CL_DEFUN bool ext__specialp(Symbol_sp sym) {
  return sym->specialP();
}

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(symbolName)dx");
DOCGROUP(clasp);
CL_DEFUN SimpleString_sp cl__symbol_name(Symbol_sp arg) {
  return arg->symbolName();
}

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(symbolValue)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__symbol_value(Symbol_sp arg) {
  return arg->symbolValue();
};

CL_LAMBDA(symbol);
CL_DECLARE();
CL_DOCSTRING(R"dx(Sequentially-consistent atomic read of SYMBOL-VALUE.)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__atomic_symbol_value(Symbol_sp arg) {
  return arg->atomicSymbolValue();
}

CL_LAMBDA(nv symbol);
CL_DECLARE();
CL_DOCSTRING(R"dx(Sequentially-consistent atomic write of SYMBOL-VALUE.)dx");
DOCGROUP(clasp);
CL_DEFUN void core__atomic_set_symbol_value(T_sp nv, Symbol_sp arg) {
  arg->set_atomicSymbolValue(nv);
}

CL_LAMBDA(cmp new-value symbol);
CL_DECLARE();
CL_DOCSTRING(R"dx(Compare-and-swap of SYMBOL-VALUE.)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__cas_symbol_value(T_sp cmp, T_sp new_value, Symbol_sp sym) {
  return sym->casSymbolValue(cmp, new_value);
}

CL_LAMBDA(symbol cell unbound);
CL_DECLARE();
CL_DOCSTRING(R"dx(Get the value of a symbol from TLS or from the given CELL)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__symbol_value_from_cell(Symbol_sp symbol, Cons_sp cell, T_sp unbound_marker) {
  return symbol->symbolValueFromCell(cell, unbound_marker);
}

CL_LAMBDA(name);
CL_DECLARE();
CL_DOCSTRING(R"dx(make_symbol)dx");
DOCGROUP(clasp);
CL_DEFUN Symbol_sp cl__make_symbol(String_sp tstrng) {
  SimpleString_sp name = coerce::simple_string(tstrng);
  Symbol_sp sym = Symbol_O::create(name);
  return sym;
};
};

namespace core {


struct UnboundFunctionEntryPoint {
  static inline LCC_RETURN LISP_CALLING_CONVENTION() {
    Closure_O* closure = gctools::untag_general<Closure_O*>((Closure_O*)lcc_closure);
    Symbol_sp symbol = gc::As<Symbol_sp>((*closure)[0]);
    ERROR_UNDEFINED_FUNCTION(symbol);
  }
    static inline LISP_ENTRY_0() {
    return entry_point_n(lcc_closure,0,NULL);
  }
  static inline LISP_ENTRY_1() {
    core::T_O* args[1] = {lcc_farg0};
    return entry_point_n(lcc_closure,1,args);
  }
  static inline LISP_ENTRY_2() {
    core::T_O* args[2] = {lcc_farg0,lcc_farg1};
    return entry_point_n(lcc_closure,2,args);
  }
  static inline LISP_ENTRY_3() {
    core::T_O* args[3] = {lcc_farg0,lcc_farg1,lcc_farg2};
    return entry_point_n(lcc_closure,3,args);
  }
  static inline LISP_ENTRY_4() {
    core::T_O* args[4] = {lcc_farg0,lcc_farg1,lcc_farg2,lcc_farg3};
    return entry_point_n(lcc_closure,4,args);
  }
  static inline LISP_ENTRY_5() {
    core::T_O* args[5] = {lcc_farg0,lcc_farg1,lcc_farg2,lcc_farg3,lcc_farg4};
    return entry_point_n(lcc_closure,5,args);
  }

};

struct UnboundSetfFunctionEntryPoint {
  static inline LCC_RETURN LISP_CALLING_CONVENTION() {
    Closure_O* closure = gctools::untag_general<Closure_O*>((Closure_O*)lcc_closure);
    Symbol_sp symbol = gc::As<Symbol_sp>((*closure)[0]);
    List_sp name = Cons_O::createList(cl::_sym_setf,symbol);
    ERROR_UNDEFINED_FUNCTION(name);
  }
  static inline LISP_ENTRY_0() {
    return entry_point_n(lcc_closure,0,NULL);
  }
  static inline LISP_ENTRY_1() {
    core::T_O* args[1] = {lcc_farg0};
    return entry_point_n(lcc_closure,1,args);
  }
  static inline LISP_ENTRY_2() {
    core::T_O* args[2] = {lcc_farg0,lcc_farg1};
    return entry_point_n(lcc_closure,2,args);
  }
  static inline LISP_ENTRY_3() {
    core::T_O* args[3] = {lcc_farg0,lcc_farg1,lcc_farg2};
    return entry_point_n(lcc_closure,3,args);
  }
  static inline LISP_ENTRY_4() {
    core::T_O* args[4] = {lcc_farg0,lcc_farg1,lcc_farg2,lcc_farg3};
    return entry_point_n(lcc_closure,4,args);
  }
  static inline LISP_ENTRY_5() {
    core::T_O* args[5] = {lcc_farg0,lcc_farg1,lcc_farg2,lcc_farg3,lcc_farg4};
    return entry_point_n(lcc_closure,5,args);
  }

};


Closure_sp make_unbound_symbol_function(Symbol_sp name)
{
  if (_lisp->_Roots._UnboundSymbolFunctionEntryPoint.unboundp()) {
    _lisp->_Roots._UnboundSymbolFunctionEntryPoint = makeGlobalSimpleFunAndFunctionDescription<UnboundFunctionEntryPoint>(name,nil<T_O>());
  }
  Closure_sp closure = 
      gctools::GC<core::Closure_O>::allocate_container<gctools::RuntimeStage>(false,1,
                                                                                       _lisp->_Roots._UnboundSymbolFunctionEntryPoint);
  (*closure)[0] = name;
  return closure;
}



Closure_sp make_unbound_setf_symbol_function(Symbol_sp name)
{
  if (_lisp->_Roots._UnboundSetfSymbolFunctionEntryPoint.unboundp()) {
    List_sp sname = Cons_O::createList(cl::_sym_setf,name);
    _lisp->_Roots._UnboundSetfSymbolFunctionEntryPoint = makeGlobalSimpleFunAndFunctionDescription<UnboundSetfFunctionEntryPoint>(sname,nil<T_O>());
  }
  Closure_sp closure = 
      gctools::GC<core::Closure_O>::allocate_container<gctools::RuntimeStage>(false, 1,
                                                                                       _lisp->_Roots._UnboundSetfSymbolFunctionEntryPoint);
  (*closure)[0] = name;
  return closure;
}


/*! Construct a symbol that is incomplete, it has no Class or Package */
Symbol_O::Symbol_O(const only_at_startup& dummy) : _HomePackage(nil<T_O>()),
#ifdef SYMBOL_CLASS
                                                   _Class(nil<T_O>()),
#endif
                                                   _GlobalValue(unbound<T_O>()),
                                                   _Function(unbound<Function_O>()),
                                                   _SetfFunction(unbound<Function_O>()),
                                                   _BindingIdx(NO_THREAD_LOCAL_BINDINGS),
                                                   _Flags(0),
                                                   _PropertyList(nil<List_V>()) {};

Symbol_O::Symbol_O() : Base(),
                       _BindingIdx(NO_THREAD_LOCAL_BINDINGS),
                       _Flags(0),
                       _PropertyList(nil<List_V>()) {};


void Symbol_O::finish_setup(Package_sp pkg, bool exportp, bool shadowp) {
  ASSERTF(pkg, "The package is UNDEFINED");
  this->_HomePackage = pkg;
  if (pkg->actsLikeKeywordPackage())
    this->set_globalValue(this->asSmartPtr());
  else
    this->set_globalValue(unbound<T_O>());
  this->fmakunbound();
  this->fmakunbound_setf();
  pkg->bootstrap_add_symbol_to_package(this->symbolName()->get_std_string().c_str(),
                                       this->sharedThis<Symbol_O>(), exportp, shadowp);
  this->setf_plist(nil<T_O>());
}

Symbol_sp Symbol_O::create_from_string(const string &nm) {
  // This is used to allocate roots that are pointed
  // to by global variable _sym_XXX  and will never be collected
  only_at_startup dummy;
  Symbol_sp n = gctools::GC<Symbol_O>::allocate(dummy); // root_allocate(true)
  SimpleString_sp snm = SimpleBaseString_O::make(nm);
  n->setf_name(snm);
  // The following are done in finish_setup
  //  n->fmakunbound();
  //  n->fmakunbound_setf();
  ASSERTF(nm != "", "You cannot create a symbol without a name");
  return n;
};

Symbol_sp Symbol_O::makunbound() {
  if (this->getReadOnly())
    // would be a nice extension to make this a continuable error
    SIMPLE_ERROR("Cannot make constant {} unbound", this->__repr__());
  setf_symbolValue(unbound<T_O>());
  return this->asSmartPtr();
}

CL_LAMBDA(function-name);
CL_DECLARE();
CL_DOCSTRING(R"dx(makunbound)dx");
DOCGROUP(clasp);
CL_DEFUN Symbol_sp cl__makunbound(Symbol_sp functionName) {
  return functionName->makunbound();
}

Function_sp Symbol_O::symbolFunction() const {
  Function_sp cell = functionCell();
  if (cell->entry() != UnboundFunctionEntryPoint::entry_point_n)
    return cell;
  else ERROR_UNDEFINED_FUNCTION(this->asSmartPtr());
}

Function_sp Symbol_O::getSetfFdefinition() const {
  Function_sp cell = setfFunctionCell();
  if (cell->entry() != UnboundSetfFunctionEntryPoint::entry_point_n)
    return cell;
  else ERROR_UNDEFINED_FUNCTION(this->asSmartPtr());
}

bool Symbol_O::fboundp() const {
  return functionCell()->entry() != UnboundFunctionEntryPoint::entry_point_n;
};

void Symbol_O::fmakunbound()
{
  functionCellSet(make_unbound_symbol_function(this->asSmartPtr()));
}

bool Symbol_O::fboundp_setf() const {
  return setfFunctionCell()->entry() != UnboundSetfFunctionEntryPoint::entry_point_n;
};

void Symbol_O::fmakunbound_setf()
{
  setfFunctionCellSet(make_unbound_setf_symbol_function(this->asSmartPtr()));
}


NEVER_OPTIMIZE void Symbol_O::symbolUnboundError() const {
  UNBOUND_VARIABLE_ERROR(this->asSmartPtr());
}

void Symbol_O::sxhash_(HashGenerator &hg) const {
  // clhs 18.2.14 sxhash
  // Although similarity is defined for symbols in terms of both the symbol's name and the packages
  // in which the symbol is accessible, item 3 disallows using package information to compute the hash
  // code, since changes to the package status of a symbol are not visible to equal.
  // if (hg.isFilling()) this->getPackage().unsafe_general()->sxhash_(hg);
  if (hg.isFilling()) this->_Name->sxhash_(hg);
}

void Symbol_O::sxhash_equal(HashGenerator &hg) const
{
  if (hg.isFilling()) HashTable_O::sxhash_equal(hg,this->getPackage());
  if (hg.isFilling()) HashTable_O::sxhash_equal(hg,this->_Name);
}


Symbol_sp Symbol_O::copy_symbol(T_sp copy_properties) const {
  Symbol_sp new_symbol = Symbol_O::create(this->_Name);
  if (copy_properties.isTrue()) {
    if (this->boundP())
      new_symbol->set_globalValue(this->symbolValue());
    new_symbol->setReadOnly(this->getReadOnly());
    new_symbol->setf_plist(cl__copy_list(this->plist()));
    if (this->fboundp()) new_symbol->setf_symbolFunction(this->symbolFunction());
    else new_symbol->fmakunbound();
    if (this->fboundp_setf()) new_symbol->setSetfFdefinition(this->getSetfFdefinition());
    else new_symbol->fmakunbound_setf();
  }
  return new_symbol;
};


CL_LISPIFY_NAME("cl:copy_symbol");
CL_LAMBDA(symbol &optional copy-properties);
DOCGROUP(clasp);
CL_DEFUN Symbol_sp cl__copy_symbol(Symbol_sp symbol, T_sp copy_properties)
{
  return symbol->copy_symbol(copy_properties);
}


bool Symbol_O::isKeywordSymbol() {
  if (this->homePackage().nilp()) return false;
  Package_sp pkg = gc::As<Package_sp>(this->getPackage());
  return pkg->isKeywordPackage();
};

Symbol_sp Symbol_O::asKeywordSymbol() {
  if (this->getPackage().notnilp()) {
    Package_sp pkg = gc::As<Package_sp>(this->getPackage());
    if (pkg->isKeywordPackage())
      return this->asSmartPtr();
  }
  Symbol_sp kwSymbol = _lisp->internKeyword(this->symbolNameAsString());
  return kwSymbol;
};

CL_LISPIFY_NAME("core:asKeywordSymbol");
CL_LAMBDA(symbol);
DOCGROUP(clasp);
CL_DEFUN Symbol_sp core__asKeywordSymbol(Symbol_sp symbol) {
  return symbol->asKeywordSymbol();
}

#ifdef SYMBOL_CLASS
T_sp Symbol_O::find_class() {
  T_sp tholder = this->_Class.load();
  if (tholder.nilp()) return tholder;
  if (gc::As_unsafe<ClassHolder_sp>(tholder)->class_unboundp()) {
    return unbound<T_O>();
  }
  return gc::As_unsafe<ClassHolder_sp>(tholder)->class_();
}

void Symbol_O::setf_find_class(T_sp class_) {
  T_sp tholder = this->_Class.load();
  if (tholder.nilp()) {
    tholder = ClassHolder_O::create(class_);
  }
  if (class_.notnilp()) {
    gc::As_unsafe<ClassHolder_sp>(tholder)->class_set(class_);
  } else {
    gc::As_unsafe<ClassHolder_sp>(tholder)->class_mkunbound();
  }
}

ClassHolder_sp Symbol_O::find_class_holder() {
  T_sp tholder = this->_Class.load();
  if (tholder.nilp()) {
    ClassHolder_sp holder = ClassHolder_O::create();
    this->_Class.set(holder);
  }
  return gc::As_unsafe<ClassHolder_sp>(tholder);
}
#endif

void Symbol_O::makeSpecial() {
  this->setf_specialP(true);
}

CL_LISPIFY_NAME("core:STARmakeSpecial");
CL_LAMBDA(symbol);
DOCGROUP(clasp);
CL_DEFUN void core__STARmakeSpecial(Symbol_sp symbol) {
  symbol->makeSpecial();
}

T_sp Symbol_O::defconstant(T_sp val) {
  
  T_sp result = this->setf_symbolValue(val);
  this->setf_specialP(true);
  this->setReadOnly(true);
  return result;
}

T_sp Symbol_O::defparameter(T_sp val) {
  
  T_sp result = this->setf_symbolValue(val);
  this->setf_specialP(true);
  return result;
}

string Symbol_O::symbolNameAsString() const {
  return this->_Name->get_std_string();
}


string Symbol_O::safeFormattedName() const { //no guard
  stringstream ss;
  Package_sp pkg = gc::As_unsafe<Package_sp>(this->_HomePackage.load());
  if (pkg.generalp() && gc::IsA<Package_sp>(pkg)) {
    ss << pkg->_Name->get_std_string();
  }
  ss << "::";
  if (this->_Name.generalp() && gc::IsA<String_sp>(this->_Name)) {
    ss << this->_Name->get_std_string();
  }
  return ss.str();
};

string Symbol_O::formattedName(bool prefixAlways) const { //no guard
  stringstream ss;
  if (this->getPackage().nilp()) {
    ss << "#:";
    ss << this->_Name->get_std_string();
  } else {
    T_sp tmyPackage = this->getPackage();
    if (!tmyPackage) {
      ss << "<PKG-NULL>:" << this->_Name->get_std_string();
      return ss.str();
    }
    if (gc::IsA<Package_sp>(tmyPackage)) {
      Package_sp myPackage = gc::As_unsafe<Package_sp>(tmyPackage);
      if (myPackage->isKeywordPackage()) {
        ss << ":" << this->_Name->get_std_string();
      } else {
        Package_sp currentPackage = _lisp->getCurrentPackage();
        if (prefixAlways) {
          ss << myPackage->getName() << "::" << this->_Name->get_std_string();
        } else {
          ss << this->_Name->get_std_string();
        }
      }
    } else {
      ss << "BAD_PACKAGE::" << this->_Name->get_std_string();
    }
  }
// Sometimes its useful to add the address of the symbol to
// the name for debugging - uncomment the following line if you want that
#if 0
  ss << "@" << (void*)(this);
#endif

#if VERBOSE_SYMBOLS
  if (this->specialP()) {
    ss << "/special";
  } else {
    ss << "/lexical";
  }
#endif
  return ss.str();
};

bool Symbol_O::isExported() {
  //#error "Why is this assignment possible?  Translating constructor?????"
  T_sp myPackage = this->getPackage();
  if (myPackage.nilp())
    return false;
  return gc::As<Package_sp>(myPackage)->isExported(this->sharedThis<Symbol_O>());
}

Symbol_sp Symbol_O::exportYourself(bool doit) {
  if (doit) {
    if (!this->isExported()) {
      if (this->getPackage().nilp())
        SIMPLE_ERROR("Cannot export - no package");
      Package_sp pkg = gc::As<Package_sp>(this->getPackage());
      if (!pkg->isKeywordPackage()) {
        pkg->_export2(this->asSmartPtr());
      }
    }
  }
  return this->asSmartPtr();
}

string Symbol_O::__repr__() const {
  unlikely_if(!this->_Name) return "UNITIALIZED-SYMBOL";
  return this->formattedName(false);
};

string Symbol_O::currentName() const {
  string formattedName = this->formattedName(false);
  return formattedName;
}

string Symbol_O::fullName() const {
  string formattedName = this->formattedName(true);
  return formattedName;
}

CL_LAMBDA(symbol);
DOCGROUP(clasp);
CL_DEFUN string core__fullName(Symbol_sp symbol){
  return symbol->fullName();
}

T_sp Symbol_O::getPackage() const {
  T_sp pkg = this->_HomePackage.load(std::memory_order_relaxed);
  if (!pkg) return nil<T_O>();
  return pkg;
}

void Symbol_O::setPackage(T_sp p) {
  ASSERTF(p, "The package is UNDEFINED");
  ASSERT(p.nilp() || gc::IsA<Package_sp>(p));
  this->_HomePackage.store(p, std::memory_order_relaxed);
}

SYMBOL_EXPORT_SC_(ClPkg, make_symbol);
SYMBOL_EXPORT_SC_(ClPkg, symbolName);
SYMBOL_EXPORT_SC_(ClPkg, symbolValue);
SYMBOL_EXPORT_SC_(ClPkg, symbolPackage);
SYMBOL_EXPORT_SC_(ClPkg, symbolFunction);
SYMBOL_EXPORT_SC_(ClPkg, boundp);




void Symbol_O::dump() {
  stringstream ss;
  ss << "Symbol @" << (void *)this << " --->" << std::endl;
  {
    ss << "Name: " << this->_Name->get_std_string() << std::endl;
    if (!this->getPackage()) {
      ss << "Package: UNDEFINED" << std::endl;
    } else {
      ss << "Package: ";
      ss << _rep_(this->getPackage()) << std::endl;
    }
    T_sp val = this->symbolValueUnsafe();
    if (!val) {
      ss << "VALUE: NULL" << std::endl;
    } else if (val.unboundp()) {
      ss << "Value: UNBOUND" << std::endl;
    } else if (val.nilp()) {
      ss << "Value: nil" << std::endl;
    } else {
      ss << "Value: " << _rep_(val) << std::endl;
    }
    if (this->fboundp()) {
      ss << "Function: " << _rep_(this->symbolFunction()) << std::endl;
    } else {
      ss << "Function: UNBOUND" << std::endl;
    }
    ss << "IsSpecial: " << this->specialP() << std::endl;
    ss << "IsConstant: " << this->getReadOnly() << std::endl;
    ss << "PropertyList: " << _rep_(this->plist()) << std::endl;
  }
  printf("%s", ss.str().c_str());
}

// This should not ignore pkg, nor should it unintern shadowed symbols
// what is done with T_sp tpkg(pkg)
void Symbol_O::remove_package(Package_sp pkg)
{
  // can't even understand the syntax of this, cast of package to T_sp?
  // T_sp tpkg(pkg);
  if ((pkg->getSystemLockedP()) || (pkg->getUserLockedP()))
    return;
  T_sp home = this->getPackage();
  if (!home.nilp()) {
     Package_sp home_package = coerce::packageDesignator(home);
    if ((home_package == pkg) &&
        !home_package->getSystemLockedP() &&
        !home_package->getUserLockedP()) {
      this->setPackage(nil<T_O>());
    }
  }
};

DOCGROUP(clasp);
CL_DEFUN T_sp core__symbol_global_value(Symbol_sp s) {
  return s->globalValue();
}

CL_DOCSTRING(R"(Set the value slot of the symbol to the value.
This bypasses thread local storage of symbol value slots and any threads that start
after this has been set will start with the value set here.)")
DOCGROUP(clasp);
CL_DEFUN void core__symbol_global_value_set(Symbol_sp symbol, T_sp value) {
  symbol->set_globalValue(value);
}

DOCGROUP(clasp);
CL_DEFUN T_sp core__symbol_thread_local_value(Symbol_sp s) {
  return s->threadLocalSymbolValue();
}

DOCGROUP(clasp);
CL_DEFUN bool core__no_thread_local_bindingp(T_sp object) {
  return gctools::tagged_no_thread_local_bindingp(object.raw_());
}

/*! For debugging only - return the address of whatever word contains the
    symbol-value for this symbol.  It's either &_GlobalValue or the address
    of the thread local vector slot at _BindingIdx */
DOCGROUP(clasp);
CL_DEFUN Pointer_sp core__symbol_value_address(Symbol_sp s) {
  if (s->_BindingIdx == NO_THREAD_LOCAL_BINDINGS) {
    return Pointer_O::create((void*)&s->_GlobalValue);
  }
  return Pointer_O::create((void*)my_thread->bindings().thread_local_reference(s->_BindingIdx));
}


SYMBOL_EXPORT_SC_(KeywordPkg,vtable);
SYMBOL_EXPORT_SC_(KeywordPkg,name);
SYMBOL_EXPORT_SC_(KeywordPkg,home_package);
SYMBOL_EXPORT_SC_(KeywordPkg,global_value);
SYMBOL_EXPORT_SC_(KeywordPkg,function);
SYMBOL_EXPORT_SC_(KeywordPkg,setf_function);
SYMBOL_EXPORT_SC_(KeywordPkg,binding_idx);
SYMBOL_EXPORT_SC_(KeywordPkg,flags);
SYMBOL_EXPORT_SC_(KeywordPkg,property_list);

DOCGROUP(clasp);
CL_DEFUN void core__verify_symbol_layout(T_sp alist)
{
  expect_offset(kw::_sym_name,alist,offsetof(Symbol_O,_Name)-gctools::general_tag);
  expect_offset(kw::_sym_home_package,alist,offsetof(Symbol_O,_HomePackage)-gctools::general_tag);
  expect_offset(kw::_sym_global_value,alist,offsetof(Symbol_O,_GlobalValue)-gctools::general_tag);
  expect_offset(kw::_sym_function,alist,offsetof(Symbol_O,_Function)-gctools::general_tag);
  expect_offset(kw::_sym_setf_function,alist,offsetof(Symbol_O,_SetfFunction)-gctools::general_tag);
  expect_offset(kw::_sym_binding_idx,alist,offsetof(Symbol_O,_BindingIdx)-gctools::general_tag);
  expect_offset(kw::_sym_flags,alist,offsetof(Symbol_O,_Flags)-gctools::general_tag);
  expect_offset(kw::_sym_property_list,alist,offsetof(Symbol_O,_PropertyList)-gctools::general_tag);
}

};
