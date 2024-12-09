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
// #define DEBUG_LEVEL_FULL

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
CL_DEFUN List_sp cl__symbol_plist(Symbol_sp sym) { return sym->plist(); }

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
CL_DEFUN List_sp core__atomic_symbol_plist(Symbol_sp sym) { return sym->atomic_plist(); }

CL_LAMBDA(nv symbol);
CL_DECLARE();
CL_DOCSTRING(R"dx(Sequentially consistent atomic store of the symbol-plist)dx");
DOCGROUP(clasp);
CL_DEFUN void core__atomic_set_symbol_plist(List_sp nv, Symbol_sp symbol) { symbol->atomic_setf_plist(nv); }

CL_LAMBDA(cmp newv sym);
CL_DECLARE();
CL_DOCSTRING(R"dx(Compare-and-swap the symbol-plist)dx");
DOCGROUP(clasp);
CL_DEFUN List_sp core__cas_symbol_plist(List_sp cmp, List_sp newv, Symbol_sp sym) { return sym->cas_plist(cmp, newv); }

CL_LAMBDA(sym indicator &optional default);
CL_DECLARE();
CL_DOCSTRING(R"dx(Return the value of a plist property)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__get(Symbol_sp sym, T_sp indicator, T_sp defval) { return cl__getf(sym->plist(), indicator, defval); }

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
CL_DEFUN T_sp cl__symbol_package(Symbol_sp arg) { return arg->homePackage(); };

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(symbolFunction)dx");
DOCGROUP(clasp);
CL_DEFUN Function_sp cl__symbol_function(Symbol_sp sym) { return sym->symbolFunction(); };

CL_LAMBDA(sym);
CL_DECLARE();
CL_DOCSTRING(R"dx(Return true if the symbol is dynamic/special)dx");
DOCGROUP(clasp);
CL_DEFUN bool ext__specialp(Symbol_sp sym) { return sym->specialP(); }

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(symbolName)dx");
DOCGROUP(clasp);
CL_DEFUN SimpleString_sp cl__symbol_name(Symbol_sp arg) { return arg->symbolName(); }

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(symbolValue)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__symbol_value(Symbol_sp arg) { return arg->symbolValue(); };

DOCGROUP(clasp);
CL_DEFUN T_sp core__symbol_global_value(Symbol_sp s) { return s->globalSymbolValue(); }

CL_DOCSTRING(R"(Set the value slot of the symbol to the value.
This bypasses thread local storage of symbol value slots and any threads that start
after this has been set will start with the value set here.)")
DOCGROUP(clasp);
CL_DEFUN void core__symbol_global_value_set(Symbol_sp sym, T_sp nv) { sym->set_globalSymbolValue(nv); }

CL_LAMBDA(symbol);
CL_DECLARE();
CL_DOCSTRING(R"dx(Sequentially-consistent atomic read of SYMBOL-VALUE.)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__atomic_symbol_value(Symbol_sp arg) { return arg->atomicSymbolValue(); }

CL_LAMBDA(nv symbol);
CL_DECLARE();
CL_DOCSTRING(R"dx(Sequentially-consistent atomic write of SYMBOL-VALUE.)dx");
DOCGROUP(clasp);
CL_DEFUN void core__atomic_set_symbol_value(T_sp nv, Symbol_sp arg) { arg->set_atomicSymbolValue(nv); }

CL_LAMBDA(cmp new-value symbol);
CL_DECLARE();
CL_DOCSTRING(R"dx(Compare-and-swap of SYMBOL-VALUE.)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__cas_symbol_value(T_sp cmp, T_sp new_value, Symbol_sp sym) { return sym->casSymbolValue(cmp, new_value); }

CL_LAMBDA(name);
CL_DECLARE();
CL_DOCSTRING(R"dx(make_symbol)dx");
DOCGROUP(clasp);
CL_DEFUN Symbol_sp cl__make_symbol(String_sp tstrng) {
  SimpleString_sp name = coerce::simple_string(tstrng);
  Symbol_sp sym = Symbol_O::create(name);
  return sym;
}

/*! Construct a symbol that is incomplete, it has no Class or Package */
Symbol_O::Symbol_O(const only_at_startup& dummy)
    : _HomePackage(nil<T_O>()),
#ifdef SYMBOL_CLASS
      _Class(nil<T_O>()),
#endif
      _Value(unbound<VariableCell_O>()), _Function(unbound<FunctionCell_O>()), _SetfFunction(unbound<FunctionCell_O>()), _Flags(0),
      _PropertyList(nil<List_V>()){};

Symbol_O::Symbol_O() : Base(), _Flags(0), _PropertyList(nil<List_V>()){};

void Symbol_O::finish_setup(Package_sp pkg, bool exportp, bool shadowp) {
  ASSERTF(pkg, "The package is UNDEFINED");
  this->_HomePackage = pkg;
  if (pkg->actsLikeKeywordPackage())
    this->setf_symbolValue(this->asSmartPtr());
  pkg->bootstrap_add_symbol_to_package(this->symbolName()->get_std_string().c_str(), this->sharedThis<Symbol_O>(), exportp,
                                       shadowp);
  this->setf_plist(nil<T_O>());
}

Symbol_sp Symbol_O::create_from_string(const string& nm) {
  // This is used to allocate roots that are pointed
  // to by global variable _sym_XXX  and will never be collected
  only_at_startup dummy;
  Symbol_sp n = gctools::GC<Symbol_O>::allocate(dummy); // root_allocate(true)
  SimpleString_sp snm = SimpleBaseString_O::make(nm);
  n->setf_name(snm);
  // The following are done in finish_setup
  ASSERTF(nm != "", "You cannot create a symbol without a name");
  return n;
};

VariableCell_sp VariableCell_O::make(T_sp name) { return gctools::GC<VariableCell_O>::allocate(name); }

uint32_t VariableCell_O::ensureBindingIndex() const {
  uint32_t no_binding = NO_THREAD_LOCAL_BINDINGS;
  uint32_t binding_index = _BindingIdx.load(std::memory_order_relaxed);
  if (binding_index == no_binding) {
    // Get a new index and try to exchange it in.
    auto& bindings = my_thread->_Bindings;
    uint32_t new_index = bindings.new_binding_index();
    // NOTE: We can use memory_order_relaxed because (a) nothing outside
    // of VariableCell_O deals with the _BindingIdx, and (b) the only guarantee we
    // should need for this structure is modification order consistency.
    if (!(_BindingIdx.compare_exchange_strong(no_binding, new_index, std::memory_order_relaxed))) {
      // Some other thread has beat us. That's fine - just use theirs
      // (which is now in no_binding) and release the index we just
      // grabbed.
      bindings.release_binding_index(new_index);
      return no_binding;
    } else
      return new_index;
  } else
    return binding_index;
}

void VariableCell_O::unboundError() const { UNBOUND_VARIABLE_ERROR(name()); }

VariableCell_sp Symbol_O::ensureVariableCell() {
  VariableCell_sp vcell = variableCell();
  if (vcell.unboundp()) {
    VariableCell_sp n = VariableCell_O::make(this->asSmartPtr());
    if (_Value.compare_exchange_strong(vcell, n, std::memory_order_relaxed))
      return n;
    else
      return vcell;
  } else
    return vcell;
}

CL_DEFUN VariableCell_sp core__ensure_variable_cell(Symbol_sp name) { return name->ensureVariableCell(); }

CL_DEFUN T_sp core__variable_cell(Symbol_sp name) {
  VariableCell_sp vcell = name->variableCell();
  if (vcell.unboundp())
    return nil<T_O>();
  else
    return vcell;
}

CL_LISPIFY_NAME(variable-cell/name);
CL_DEFUN T_sp core__variable_cell_name(VariableCell_sp vcell) { return vcell->name(); }

bool Symbol_O::boundP() const {
  VariableCell_sp vcell = variableCell();
  if (vcell.unboundp())
    return false;
  else
    return vcell->boundP();
}

T_sp Symbol_O::symbolValue() const {
  VariableCell_sp vcell = variableCell();
  if (vcell.unboundp())
    UNBOUND_VARIABLE_ERROR(this->asSmartPtr());
  else
    return vcell->value();
}

T_sp Symbol_O::atomicSymbolValue() const {
  VariableCell_sp vcell = variableCell();
  if (vcell.unboundp())
    UNBOUND_VARIABLE_ERROR(this->asSmartPtr());
  else
    return vcell->value();
}

T_sp Symbol_O::globalSymbolValue() const {
  VariableCell_sp vcell = variableCell();
  if (vcell.unboundp())
    UNBOUND_VARIABLE_ERROR(this->asSmartPtr());
  else
    return vcell->globalValue();
}

void Symbol_O::setf_symbolValue(T_sp nv) { ensureVariableCell()->set_value(nv); }
void Symbol_O::set_atomicSymbolValue(T_sp nv) { ensureVariableCell()->set_valueSeqCst(nv); }
void Symbol_O::set_globalSymbolValue(T_sp nv) { ensureVariableCell()->set_globalValue(nv); }

T_sp Symbol_O::casSymbolValue(T_sp cmp, T_sp nv) { return ensureVariableCell()->cas_globalValueSeqCst(cmp, nv); }

void Symbol_O::makunbound() {
  if (this->getReadOnly())
    // would be a nice extension to make this a continuable error
    SIMPLE_ERROR("Cannot make constant {} unbound", this->__repr__());
  setf_symbolValue(unbound<T_O>());
}

CL_LAMBDA(function-name);
CL_DECLARE();
CL_DOCSTRING(R"dx(makunbound)dx");
DOCGROUP(clasp);
CL_DEFUN Symbol_sp cl__makunbound(Symbol_sp functionName) {
  functionName->makunbound();
  return functionName;
}

FunctionCell_sp Symbol_O::ensureFunctionCell() {
  FunctionCell_sp existing = functionCell();
  if (existing.unboundp()) {
    FunctionCell_sp n = FunctionCell_O::make(this->asSmartPtr());
    if (this->_Function.compare_exchange_strong(existing, n, std::memory_order_relaxed))
      return n;
    else
      return existing;
  } else
    return existing;
}

FunctionCell_sp Symbol_O::ensureFunctionCell(Function_sp init) {
  FunctionCell_sp existing = functionCell();
  if (existing.unboundp()) {
    FunctionCell_sp n = FunctionCell_O::make(this->asSmartPtr(), init);
    if (this->_Function.compare_exchange_strong(existing, n, std::memory_order_relaxed))
      return n;
    else
      // Some other thread installed a cell with presumably some other
      // function, but we don't have to bother putting in our function,
      // since we can just act like that other thread acted after us.
      return existing;
  } else
    return existing;
}

FunctionCell_sp Symbol_O::ensureSetfFunctionCell() {
  FunctionCell_sp existing = setfFunctionCell();
  if (existing.unboundp()) {
    FunctionCell_sp n = FunctionCell_O::make(Cons_O::createList(cl::_sym_setf, this->asSmartPtr()));
    if (this->_SetfFunction.compare_exchange_strong(existing, n, std::memory_order_relaxed))
      return n;
    else
      return existing;
  } else
    return existing;
}

FunctionCell_sp Symbol_O::ensureSetfFunctionCell(Function_sp init) {
  FunctionCell_sp existing = setfFunctionCell();
  if (existing.unboundp()) {
    FunctionCell_sp n = FunctionCell_O::make(Cons_O::createList(cl::_sym_setf, this->asSmartPtr()), init);
    if (this->_SetfFunction.compare_exchange_strong(existing, n, std::memory_order_relaxed))
      return n;
    else
      return existing;
  } else
    return existing;
}

Function_sp Symbol_O::symbolFunction() const {
  FunctionCell_sp cell = functionCell();
  if (!cell.unboundp()) {
    if (cell->fboundp())
      return cell->real_function();
  }
  ERROR_UNDEFINED_FUNCTION(this->asSmartPtr());
}

Function_sp Symbol_O::getSetfFdefinition() const {
  FunctionCell_sp cell = setfFunctionCell();
  if (!cell.unboundp()) {
    if (cell->fboundp())
      return cell->real_function();
  }
  ERROR_UNDEFINED_FUNCTION(this->asSmartPtr());
}

bool Symbol_O::fboundp() const {
  FunctionCell_sp fcell = functionCell();
  return !(fcell.unboundp()) && fcell->fboundp();
};

void Symbol_O::fmakunbound() {
  FunctionCell_sp fcell = functionCell();
  if (!fcell.unboundp())
    fcell->fmakunbound(this->asSmartPtr());
}

void Symbol_O::setSetfFdefinition(Function_sp fn) { ensureSetfFunctionCell(fn)->real_function_set(fn); }

bool Symbol_O::fboundp_setf() const {
  FunctionCell_sp fcell = setfFunctionCell();
  return !(fcell.unboundp()) && fcell->fboundp();
};

void Symbol_O::fmakunbound_setf() {
  FunctionCell_sp fcell = setfFunctionCell();
  if (!fcell.unboundp())
    gc::As_assert<FunctionCell_sp>(fcell)->fmakunbound(Cons_O::createList(cl::_sym_setf, this->asSmartPtr()));
}

void Symbol_O::setf_symbolFunction(Function_sp fn) { ensureFunctionCell(fn)->real_function_set(fn); }

void Symbol_O::sxhash_equal(HashGenerator& hg) const {
  // clhs 18.2.14 sxhash
  // Although similarity is defined for symbols in terms of both the symbol's name and the packages
  // in which the symbol is accessible, item 3 disallows using package information to compute the hash
  // code, since changes to the package status of a symbol are not visible to equal.
  // if (hg.isFilling()) this->getPackage().unsafe_general()->sxhash_equal(hg);
  if (hg.isFilling()) this->_Name->sxhash_equal(hg);
}

Symbol_sp Symbol_O::copy_symbol(T_sp copy_properties) const {
  Symbol_sp new_symbol = Symbol_O::create(this->_Name);
  if (copy_properties.isTrue()) {
    if (this->boundP())
      new_symbol->setf_symbolValue(this->symbolValue());
    new_symbol->setReadOnly(this->getReadOnly());
    new_symbol->setf_plist(cl__copy_list(this->plist()));
    if (this->fboundp())
      new_symbol->setf_symbolFunction(this->symbolFunction());
    else
      new_symbol->fmakunbound();
    if (this->fboundp_setf())
      new_symbol->setSetfFdefinition(this->getSetfFdefinition());
    else
      new_symbol->fmakunbound_setf();
  }
  return new_symbol;
};

CL_LISPIFY_NAME("cl:copy_symbol");
CL_LAMBDA(symbol &optional copy-properties);
DOCGROUP(clasp);
CL_DEFUN Symbol_sp cl__copy_symbol(Symbol_sp symbol, T_sp copy_properties) { return symbol->copy_symbol(copy_properties); }

bool Symbol_O::isKeywordSymbol() {
  if (this->homePackage().nilp())
    return false;
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
CL_DEFUN Symbol_sp core__asKeywordSymbol(Symbol_sp symbol) { return symbol->asKeywordSymbol(); }

#ifdef SYMBOL_CLASS
T_sp Symbol_O::find_class() {
  T_sp tholder = this->_Class.load();
  if (tholder.nilp())
    return tholder;
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
  this->check_package_lock("proclaiming ~s special");
  this->setf_specialP(true);
}

CL_LISPIFY_NAME("core:STARmakeSpecial");
CL_LAMBDA(symbol);
DOCGROUP(clasp);
CL_DEFUN void core__STARmakeSpecial(Symbol_sp symbol) { symbol->makeSpecial(); }

T_sp Symbol_O::defconstant(T_sp val) {

  this->setf_symbolValue(val);
  this->setf_specialP(true);
  this->setReadOnly(true);
  return val;
}

T_sp Symbol_O::defparameter(T_sp val) {

  this->setf_symbolValue(val);
  this->setf_specialP(true);
  return val;
}

string Symbol_O::symbolNameAsString() const { return this->_Name->get_std_string(); }

string Symbol_O::safeFormattedName() const { // no guard
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

string Symbol_O::formattedName(bool prefixAlways) const { // no guard
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
        if (prefixAlways || myPackage != currentPackage) {
          ss << myPackage->packageName() << "::" << this->_Name->get_std_string();
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
  // #error "Why is this assignment possible?  Translating constructor?????"
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
CL_DEFUN string core__fullName(Symbol_sp symbol) { return symbol->fullName(); }

T_sp Symbol_O::getPackage() const {
  T_sp pkg = this->_HomePackage.load(std::memory_order_relaxed);
  if (!pkg)
    return nil<T_O>();
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
  ss << "Symbol @" << (void*)this << " --->" << std::endl;
  {
    ss << "Name: " << this->_Name->get_std_string() << std::endl;
    if (!this->getPackage()) {
      ss << "Package: UNDEFINED" << std::endl;
    } else {
      ss << "Package: ";
      ss << _rep_(this->getPackage()) << std::endl;
    }
    if (this->boundP()) {
      T_sp val = this->symbolValue();
      if (val.unboundp()) {
        ss << "Value: UNBOUND" << std::endl;
      } else if (val.nilp()) {
        ss << "Value: nil" << std::endl;
      } else {
        ss << "Value: " << _rep_(val) << std::endl;
      }
    } else {
      ss << "No Value" << std::endl;
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
void Symbol_O::remove_package(Package_sp pkg) {
  // can't even understand the syntax of this, cast of package to T_sp?
  // T_sp tpkg(pkg);
  if (pkg->lockedP())
    return;
  T_sp home = this->getPackage();
  if (!home.nilp()) {
    Package_sp home_package = coerce::packageDesignator(home);
    if ((home_package == pkg) && !home_package->lockedP()) {
      this->setPackage(nil<T_O>());
    }
  }
};

// Signal an error if this is a symbol in a locked package.
void Symbol_O::check_package_lock(const char* fmt) {
  T_sp p = this->homePackage();
  if (p.isA<Package_O>()) {
    Package_sp pkg = p.as_unsafe<Package_O>();
    if (pkg->lockedP())
      CEpackage_lock_violation(pkg, fmt, 1, this->asSmartPtr());
  }
}

DOCGROUP(clasp);
CL_DEFUN bool core__no_thread_local_bindingp(T_sp object) { return gctools::tagged_no_thread_local_bindingp(object.raw_()); }

SYMBOL_EXPORT_SC_(KeywordPkg, vtable);
SYMBOL_EXPORT_SC_(KeywordPkg, name);
SYMBOL_EXPORT_SC_(KeywordPkg, home_package);
SYMBOL_EXPORT_SC_(KeywordPkg, value);
SYMBOL_EXPORT_SC_(KeywordPkg, function);
SYMBOL_EXPORT_SC_(KeywordPkg, setf_function);
SYMBOL_EXPORT_SC_(KeywordPkg, binding_idx);
SYMBOL_EXPORT_SC_(KeywordPkg, flags);
SYMBOL_EXPORT_SC_(KeywordPkg, property_list);

DOCGROUP(clasp);
CL_DEFUN void core__verify_symbol_layout(T_sp alist) {
  expect_offset(kw::_sym_name, alist, offsetof(Symbol_O, _Name) - gctools::general_tag);
  expect_offset(kw::_sym_home_package, alist, offsetof(Symbol_O, _HomePackage) - gctools::general_tag);
  expect_offset(kw::_sym_value, alist, offsetof(Symbol_O, _Value) - gctools::general_tag);
  expect_offset(kw::_sym_function, alist, offsetof(Symbol_O, _Function) - gctools::general_tag);
  expect_offset(kw::_sym_setf_function, alist, offsetof(Symbol_O, _SetfFunction) - gctools::general_tag);
  expect_offset(kw::_sym_flags, alist, offsetof(Symbol_O, _Flags) - gctools::general_tag);
  expect_offset(kw::_sym_property_list, alist, offsetof(Symbol_O, _PropertyList) - gctools::general_tag);
}

}; // namespace core
