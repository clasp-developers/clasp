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
#include <clasp/core/functor.h>
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

//    Symbol_sp 	_sym_nil;	// equivalent to _Nil<T_O>()
//    Symbol_sp 	_sym_t;		// equivalent to _lisp->_true()

CL_LAMBDA(sym);
CL_DECLARE();
CL_DOCSTRING("Return the symbol plist");
CL_DEFUN List_sp cl__symbol_plist(Symbol_sp sym) {
  return sym->plist();
}

CL_LISPIFY_NAME("cl:symbol-plist");
CL_LAMBDA(plist sym);
CL_DECLARE();
CL_DOCSTRING("Set the symbol plist");
CL_DEFUN_SETF List_sp core__set_symbol_plist(List_sp plist, Symbol_sp sym) {
  sym->setf_plist(plist);
  return plist;
}

CL_LAMBDA(sym indicator &optional default);
CL_DECLARE();
CL_DOCSTRING("Return the value of a plist property");
CL_DEFUN T_sp cl__get(Symbol_sp sym, T_sp indicator, T_sp defval) {
  return cl__getf(sym->plist(), indicator, defval);
}

CL_LISPIFY_NAME("cl:get")
CL_LAMBDA(val sym indicator &optional default);
CL_DECLARE();
CL_DOCSTRING("Set the value of a plist property");
CL_DEFUN_SETF T_sp core__putprop(T_sp val, Symbol_sp sym, T_sp indicator, T_sp defval) {
  (void)(defval); // unused
  sym->setf_plist(core__put_f(sym->plist(), val, indicator));
  return val;
}

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("boundp");
CL_DEFUN bool cl__boundp(Symbol_sp arg) {
  if (arg.nilp())
    return true;
  return arg->boundP();
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("symbolPackage");
CL_DEFUN T_sp cl__symbol_package(Symbol_sp arg) {
  return arg->homePackage();
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("symbolFunction");
CL_DEFUN Function_sp cl__symbol_function(Symbol_sp sym) {
  if (!sym->fboundp()) {
    ERROR_UNDEFINED_FUNCTION(sym);
  }
  return sym->symbolFunction();
};


CL_LAMBDA(sym);
CL_DECLARE();
CL_DOCSTRING("Return true if the symbol is dynamic/special");
CL_DEFUN bool ext__specialp(Symbol_sp sym) {
  return sym->specialP();
}

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("symbolName");
CL_DEFUN SimpleString_sp cl__symbol_name(Symbol_sp arg) {
  return arg->symbolName();
}

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("symbolValue");
CL_DEFUN T_sp cl__symbol_value(Symbol_sp arg) {
  if (!arg->boundP()) arg->symbolUnboundError();
  return arg->symbolValue();
};

CL_LAMBDA(symbol cell unbound);
CL_DECLARE();
CL_DOCSTRING("Get the value of a symbol from TLS or from the given CELL");
CL_DEFUN T_sp core__symbol_value_from_cell(Symbol_sp symbol, Cons_sp cell, T_sp unbound_marker) {
  return symbol->symbolValueFromCell(cell, unbound_marker);
}

CL_LAMBDA(name);
CL_DECLARE();
CL_DOCSTRING("make_symbol");
CL_DEFUN Symbol_sp cl__make_symbol(String_sp tstrng) {
  SimpleString_sp name = coerce::simple_string(tstrng);
  Symbol_sp sym = Symbol_O::create(name);
  return sym;
};
};

namespace core {

core::FunctionDescription* global_unboundSymbolFunctionFunctionDescription = NULL;
ClosureWithSlots_sp make_unbound_symbol_function(Symbol_sp name)
{
  if (global_unboundSymbolFunctionFunctionDescription == NULL) {
    global_unboundSymbolFunctionFunctionDescription = makeFunctionDescription(name,_Nil<T_O>());
  }
  ClosureWithSlots_sp closure = 
    gctools::GC<core::ClosureWithSlots_O>::allocate_container(false,1,
                                                              unboundFunctionEntryPoint,
                                                              global_unboundSymbolFunctionFunctionDescription,
                                                              ClosureWithSlots_O::cclaspClosure);
  (*closure)[0] = name;
  return closure;
}

core::FunctionDescription* global_unboundSetfSymbolFunctionFunctionDescription = NULL;
ClosureWithSlots_sp make_unbound_setf_symbol_function(Symbol_sp name)
{
  if (global_unboundSetfSymbolFunctionFunctionDescription == NULL) {
    List_sp sname = Cons_O::createList(cl::_sym_setf,name);
    global_unboundSetfSymbolFunctionFunctionDescription = makeFunctionDescription(sname,_Nil<T_O>());
  }
  ClosureWithSlots_sp closure = 
    gctools::GC<core::ClosureWithSlots_O>::allocate_container(false, 1,
                                                              unboundSetfFunctionEntryPoint,
                                                              global_unboundSetfSymbolFunctionFunctionDescription,
                                                              ClosureWithSlots_O::cclaspClosure);
  (*closure)[0] = name;
  return closure;
}


/*! Construct a symbol that is incomplete, it has no Class or Package */
Symbol_O::Symbol_O(bool dummy) : _HomePackage(_Nil<T_O>()),
#ifdef SYMBOL_CLASS
                                 _Class(_Nil<T_O>()),
#endif
                                 _GlobalValue(_Unbound<T_O>()),
                                 _Function(_Unbound<Function_O>()),
                                 _SetfFunction(_Unbound<Function_O>()),
                                 _BindingIdx(NO_THREAD_LOCAL_BINDINGS),
                                 _Flags(0),
                                 _PropertyList(_Nil<List_V>()) {};

Symbol_O::Symbol_O() : Base(),
                       _BindingIdx(NO_THREAD_LOCAL_BINDINGS),
                       _Flags(0),
                       _PropertyList(_Nil<List_V>()) {};


void Symbol_O::finish_setup(Package_sp pkg, bool exportp, bool shadowp) {
  ASSERTF(pkg, BF("The package is UNDEFINED"));
  this->_HomePackage = pkg;
  if (pkg->actsLikeKeywordPackage())
    this->_GlobalValue = this->asSmartPtr();
  else
    this->_GlobalValue = _Unbound<T_O>();
  this->fmakunbound();
  this->fmakunbound_setf();
  pkg->bootstrap_add_symbol_to_package(this->symbolName()->get_std_string().c_str(),
                                       this->sharedThis<Symbol_O>(), exportp, shadowp);
  this->_PropertyList = _Nil<T_O>();
}

Symbol_sp Symbol_O::create_at_boot(const string &nm) {
  // This is used to allocate roots that are pointed
  // to by global variable _sym_XXX  and will never be collected
  Symbol_sp n = gctools::GC<Symbol_O>::root_allocate();
  ASSERTF(nm != "", BF("You cannot create a symbol without a name"));
#if VERBOSE_SYMBOLS
  if (nm.find("/dyn") != string::npos) {
    THROW_HARD_ERROR(BF("Illegal name for symbol[%s]") % nm);
  }
#endif
  n->_Name = SimpleBaseString_O::make(nm.size(),'\0',true,nm.size(),(const claspChar*)nm.c_str());
  return n;
};

Symbol_sp Symbol_O::create_from_string(const string &nm) {
  // This is used to allocate roots that are pointed
  // to by global variable _sym_XXX  and will never be collected
  Symbol_sp n = gctools::GC<Symbol_O>::root_allocate(true);
  SimpleString_sp snm = SimpleBaseString_O::make(nm);
  n->setf_name(snm);
  // The following are done in finish_setup
//  n->fmakunbound();
//  n->fmakunbound_setf();
  ASSERTF(nm != "", BF("You cannot create a symbol without a name"));
#if VERBOSE_SYMBOLS
  if (nm.find("/dyn") != string::npos) {
    THROW_HARD_ERROR(BF("Illegal name for symbol[%s]") % nm);
  }
#endif
  return n;
};
   
Symbol_sp Symbol_O::makunbound() {
  if (this->getReadOnly())
    // would be a nice extension to make this a continuable error
    SIMPLE_ERROR(BF("Cannot make constant %s unbound") % this->__repr__());
  setf_symbolValue(_Unbound<T_O>());
  return this->asSmartPtr();
}

CL_LAMBDA(function-name);
CL_DECLARE();
CL_DOCSTRING("makunbound");
CL_DEFUN Symbol_sp cl__makunbound(Symbol_sp functionName) {
  return functionName->makunbound();
}

bool Symbol_O::fboundp() const {
  return this->_Function->entry.load() != unboundFunctionEntryPoint;
};

void Symbol_O::fmakunbound()
{
  this->_Function = make_unbound_symbol_function(this->asSmartPtr());
}

bool Symbol_O::fboundp_setf() const {
  return this->_SetfFunction->entry.load() != unboundSetfFunctionEntryPoint;
};

void Symbol_O::fmakunbound_setf()
{
  this->_SetfFunction = make_unbound_setf_symbol_function(this->asSmartPtr());
}


__attribute__((optnone)) void Symbol_O::symbolUnboundError() const {
  UNBOUND_VARIABLE_ERROR(this->asSmartPtr());
}

void Symbol_O::sxhash_(HashGenerator &hg) const {
  if (hg.isFilling()) this->_HomePackage.load().unsafe_general()->sxhash_(hg);
  if (hg.isFilling()) this->_Name->sxhash_(hg);
}

void Symbol_O::sxhash_equal(HashGenerator &hg) const
{
  if (hg.isFilling()) HashTable_O::sxhash_equal(hg,this->_HomePackage);
  if (hg.isFilling()) HashTable_O::sxhash_equal(hg,this->_Name);
}


CL_LISPIFY_NAME("cl:copy_symbol");
CL_LAMBDA(symbol &optional copy-properties);
CL_DEFMETHOD Symbol_sp Symbol_O::copy_symbol(T_sp copy_properties) const {
  Symbol_sp new_symbol = Symbol_O::create(this->_Name);
  if (copy_properties.isTrue()) {
    if (this->boundP())
      new_symbol->_GlobalValue = this->symbolValue();
    new_symbol->setReadOnly(this->getReadOnly());
    new_symbol->_PropertyList = cl__copy_list(this->_PropertyList);
    if (this->fboundp()) new_symbol->_Function = this->_Function;
    else new_symbol->fmakunbound();
    if (this->fboundp_setf()) new_symbol->_SetfFunction = this->_SetfFunction;
    else new_symbol->fmakunbound_setf();
  }
  return new_symbol;
};


bool Symbol_O::isKeywordSymbol() {
  if (this->homePackage().nilp()) return false;
  Package_sp pkg = gc::As<Package_sp>(this->_HomePackage.load()); //
  return pkg->isKeywordPackage();
};

#if defined(OLD_SERIALIZE)
void Symbol_O::serialize(serialize::SNode node) {
  _OF();
  if (node->loading()) {
    SIMPLE_ERROR(BF("You can't load symbols with serialize!!"));
  } else {
    SIMPLE_ERROR(BF("You can't save symbols with serialize!!!"));
  }
}
#endif

#if defined(XML_ARCHIVE)
void Symbol_O::archiveBase(ArchiveP node) {
  if (node->loading()) {
    SIMPLE_ERROR(BF("You can't load symbols with archiveBase!! See Dumb_Node::createYourSymbol"));
  } else {
    string name = this->formattedName(true);
    node->attribute("_sym", name);
  }
}
#endif // defined(XML_ARCHIVE)

CL_LISPIFY_NAME("core:asKeywordSymbol");
CL_DEFMETHOD Symbol_sp Symbol_O::asKeywordSymbol() {
  if (this->_HomePackage.load().notnilp()) {
    Package_sp pkg = gc::As<Package_sp>(this->_HomePackage.load());
    if (pkg->isKeywordPackage())
      return this->asSmartPtr();
  }
  Symbol_sp kwSymbol = _lisp->internKeyword(this->symbolNameAsString());
  return kwSymbol;
};

#ifdef SYMBOL_CLASS
T_sp Symbol_O::find_class() {
  T_sp tholder = this->_Class.load();
  if (tholder.nilp()) return tholder;
  if (gc::As_unsafe<ClassHolder_sp>(tholder)->class_unboundp()) {
    return _Unbound<T_O>();
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

CL_LISPIFY_NAME("core:STARmakeSpecial");
CL_DEFMETHOD void Symbol_O::makeSpecial() {
  this->setf_specialP(true);
}

T_sp Symbol_O::defconstant(T_sp val) {
  _OF();
  T_sp result = this->setf_symbolValue(val);
  this->setf_specialP(true);
  this->setReadOnly(true);
  return result;
}

T_sp Symbol_O::defparameter(T_sp val) {
  _OF();
  T_sp result = this->setf_symbolValue(val);
  this->setf_specialP(true);
  return result;
}

CL_LISPIFY_NAME("core:setf_symbolFunction");
CL_DEFMETHOD void Symbol_O::setf_symbolFunction(Function_sp exec) {
  this->_Function = exec;
}

string Symbol_O::symbolNameAsString() const {
  return this->_Name->get_std_string();
}

string Symbol_O::formattedName(bool prefixAlways) const { //no guard
  stringstream ss;
  if (this->_HomePackage.load().nilp()) {
    ss << "#:";
    ss << this->_Name->get_std_string();
  } else {
    T_sp tmyPackage = this->_HomePackage;
    if (!tmyPackage) {
      ss << "<PKG-NULL>:" << this->_Name->get_std_string();
      return ss.str();
    }
    Package_sp myPackage = gc::As<Package_sp>(tmyPackage);
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
      if (this->_HomePackage.load().nilp())
        SIMPLE_ERROR(BF("Cannot export - no package"));
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

CL_LISPIFY_NAME("core:fullName");
CL_DEFMETHOD string Symbol_O::fullName() const {
  string formattedName = this->formattedName(true);
  return formattedName;
}

T_sp Symbol_O::getPackage() const {
  T_sp pkg = this->_HomePackage.load();
  if (!pkg) return _Nil<T_O>();
  return pkg;
}

void Symbol_O::setPackage(T_sp p) {
  ASSERTF(p, BF("The package is UNDEFINED"));
  ASSERT(p.nilp() || gc::IsA<Package_sp>(p));
  this->_HomePackage = p;
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
    if (!this->_HomePackage.load()) {
      ss << "Package: UNDEFINED" << std::endl;
    } else {
      ss << "Package: ";
      ss << _rep_(this->_HomePackage) << std::endl;
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
      ss << "Function: " << _rep_(this->_Function) << std::endl;
    } else {
      ss << "Function: UNBOUND" << std::endl;
    }
    ss << "IsSpecial: " << this->specialP() << std::endl;
    ss << "IsConstant: " << this->getReadOnly() << std::endl;
    ss << "PropertyList: ";
    if (this->_PropertyList) {
      ss << _rep_(this->_PropertyList) << std::endl;
    } else {
      ss << "UNDEFINED" << std::endl;
    }
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
      this->_HomePackage = _Nil<T_O>();
    }
  }
};

CL_DEFUN T_sp core__symbol_global_value(Symbol_sp s) {
  return s->globalValue();
}

CL_DOCSTRING(R"(Set the value slot of the symbol to the value.
This bypasses thread local storage of symbol value slots and any threads that start
after this has been set will start with the value set here.)");
CL_DEFUN void core__symbol_global_value_set(Symbol_sp symbol, T_sp value) {
  symbol->set_globalValue(value);
}

CL_DEFUN T_sp core__symbol_thread_local_value(Symbol_sp s) {
  return s->threadLocalSymbolValue();
}

CL_DEFUN bool core__no_thread_local_bindingp(T_sp object) {
  return gctools::tagged_no_thread_local_bindingp(object.raw_());
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
