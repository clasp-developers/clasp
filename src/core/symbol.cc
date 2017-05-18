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

#include <clasp/core/useBoostPython.h>
#include <clasp/core/common.h>
#include <clasp/core/symbol.h>
#include <clasp/core/array.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/numbers.h>
#include <clasp/core/lispList.h>
#include <clasp/core/package.h>
#include <clasp/core/lisp.h>
#include <clasp/core/wrappers.h>

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

CL_LAMBDA(sym indicator &optional default);
CL_DECLARE();
CL_DOCSTRING("Return the symbol plist");
CL_DEFUN T_sp cl__get(Symbol_sp sym, T_sp indicator, T_sp defval) {
#if 0
  if (sym.nilp()) {
    return cl__getf(coerce_to_list(cl::_sym_nil), indicator, defval);
  }
#endif
  return cl__getf(sym->_PropertyList, indicator, defval);
}

CL_LAMBDA(sym plist);
CL_DECLARE();
CL_DOCSTRING("Set the symbol plist");
CL_DEFUN void core__set_symbol_plist(Symbol_sp sym, List_sp plist) {
  if (sym.nilp()) {
    SIMPLE_ERROR(BF("You cannot set the plist of nil"));
  };
  sym->setf_plist(plist);
}

CL_LAMBDA(sym val indicator);
CL_DECLARE();
CL_DOCSTRING("Set the symbol plist");
CL_DEFUN T_sp core__putprop(Symbol_sp sym, T_sp val, T_sp indicator) {
  if (sym.nilp()) {
    SIMPLE_ERROR(BF("You cannot set the plist of nil"));
  };
  sym->_PropertyList = core__put_f(sym->_PropertyList, val, indicator);
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
    SIMPLE_ERROR(BF("No function bound to %s") % _rep_(sym));
  }
  return sym->symbolFunction();
};

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

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("symbolValueAddress");
CL_DEFUN T_sp core__symbol_value_address(Symbol_sp arg) {
  return Pointer_O::create(&arg->symbolValueRef());
};

CL_LAMBDA(name);
CL_DECLARE();
CL_DOCSTRING("make_symbol");
CL_DEFUN Symbol_sp cl__make_symbol(SimpleString_sp name) {
  Symbol_sp sym = Symbol_O::create(name);
  return sym;
};
};

namespace core {

/*! Construct a symbol that is incomplete, it has no Class or Package */
Symbol_O::Symbol_O(bool dummy) : _HomePackage(_Nil<T_O>()),
                                 _GlobalValue(_Unbound<T_O>()),
                                 _Function(_Unbound<T_O>()),
                                 _SetfFunction(_Unbound<T_O>()),
                                 _Binding(NO_THREAD_LOCAL_BINDINGS),
                                 _IsSpecial(false),
                                 _IsConstant(false),
                                 _ReadOnlyFunction(false),
                                 _PropertyList(_Nil<List_V>()) {};

Symbol_O::Symbol_O() : Base(),
                       _Binding(NO_THREAD_LOCAL_BINDINGS),
                       _IsSpecial(false),
                       _IsConstant(false),
                       _ReadOnlyFunction(false),
                       _PropertyList(_Nil<List_V>()) {};

void Symbol_O::finish_setup(Package_sp pkg, bool exportp, bool shadowp) {
  ASSERTF(pkg, BF("The package is UNDEFINED"));
  this->_HomePackage = pkg;
  this->_GlobalValue = _Unbound<T_O>();
  this->_Function = _Unbound<T_O>();
  this->_SetfFunction = _Unbound<T_O>();
  pkg->bootstrap_add_symbol_to_package(this->symbolName()->get().c_str(), this->sharedThis<Symbol_O>(), exportp, shadowp);
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
  ASSERTF(nm != "", BF("You cannot create a symbol without a name"));
#if VERBOSE_SYMBOLS
  if (nm.find("/dyn") != string::npos) {
    THROW_HARD_ERROR(BF("Illegal name for symbol[%s]") % nm);
  }
#endif
  return n;
};


CL_LISPIFY_NAME("makunbound");
CL_DEFMETHOD Symbol_sp Symbol_O::makunbound() {
  *my_thread->_Bindings.reference_raw(this) = _Unbound<T_O>();
  return this->asSmartPtr();
}

void Symbol_O::symbolUnboundError() const {
  UNBOUND_VARIABLE_ERROR(this->_Name);
}

void Symbol_O::setf_plist(List_sp plist) {
  this->_PropertyList = plist;
}

void Symbol_O::sxhash_(HashGenerator &hg) const {
  if (hg.isFilling())
    this->_Name->sxhash_(hg);
}

CL_LISPIFY_NAME("cl:copy_symbol");
CL_LAMBDA(symbol &optional copy-properties);
CL_DEFMETHOD Symbol_sp Symbol_O::copy_symbol(T_sp copy_properties) const {
  Symbol_sp new_symbol = Symbol_O::create(this->_Name);
  if (copy_properties.isTrue()) {
    ASSERT(this->_Function);
    new_symbol->_GlobalValue = this->symbolValue();
    new_symbol->_Function = this->_Function;
    new_symbol->_IsConstant = this->_IsConstant;
    new_symbol->_ReadOnlyFunction = this->_ReadOnlyFunction;
    new_symbol->_PropertyList = cl__copy_list(this->_PropertyList);
  }
  return new_symbol;
};

bool Symbol_O::isKeywordSymbol() {
  if (this->homePackage().nilp()) return false;
  Package_sp pkg = gc::As<Package_sp>(this->_HomePackage); //
  return pkg->isKeywordPackage();
};

bool Symbol_O::amp_symbol_p() const {
  return (this->_Name->get()[0] == '&');
}

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
  if (this->_HomePackage.notnilp()) {
    Package_sp pkg = gc::As<Package_sp>(this->_HomePackage);
    if (pkg->isKeywordPackage())
      return this->asSmartPtr();
  }
  Symbol_sp kwSymbol = _lisp->internKeyword(this->symbolNameAsString());
  return kwSymbol;
};


CL_LISPIFY_NAME("core:STARmakeSpecial");
CL_DEFMETHOD void Symbol_O::makeSpecial() {
  this->_IsSpecial = true;
}

CL_LISPIFY_NAME("core:STARmakeConstant");
CL_DEFMETHOD void Symbol_O::makeConstant(T_sp val) {
  this->setf_symbolValue(val);
  this->_IsSpecial = true;
  this->_IsConstant = true;
}

T_sp Symbol_O::defconstant(T_sp val) {
  _OF();
  T_sp result = this->setf_symbolValue(val);
  this->_IsSpecial = true;
  this->setReadOnly(true);
  return result;
}

T_sp Symbol_O::defparameter(T_sp val) {
  _OF();
  T_sp result = this->setf_symbolValue(val);
  this->_IsSpecial = true;
  return result;
}

CL_LISPIFY_NAME("core:setf_symbolFunction");
CL_DEFMETHOD void Symbol_O::setf_symbolFunction(T_sp exec) {
  _OF();
  this->_Function = exec;
}

string Symbol_O::symbolNameAsString() const {
  return this->_Name->get();
}

string Symbol_O::formattedName(bool prefixAlways) const { //no guard
  stringstream ss;
  if (this->_HomePackage.nilp()) {
    ss << "#:";
    ss << this->_Name->get();
  } else {
    T_sp tmyPackage = this->_HomePackage;
    if (!tmyPackage) {
      ss << "<PKG-NULL>:" << this->_Name->get();
      return ss.str();
    }
    Package_sp myPackage = gc::As<Package_sp>(tmyPackage);
    if (myPackage->isKeywordPackage()) {
      ss << ":" << this->_Name->get();
    } else {
      Package_sp currentPackage = _lisp->getCurrentPackage();
      if (prefixAlways) {
        ss << myPackage->getName() << "::" << this->_Name->get();
      } else {
        ss << this->_Name->get();
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
      if (this->_HomePackage.nilp())
        SIMPLE_ERROR(BF("Cannot export - no package"));
      Package_sp pkg = gc::As<Package_sp>(this->getPackage());
      if (!pkg->isKeywordPackage()) {
        pkg->_export2(this->asSmartPtr());
      }
    }
  }
  return this->asSmartPtr();
}

#if 0
T_sp Symbol_O::apply() {
  ValueFrame_sp frame = ValueFrame_O::create(0, _Nil<ActivationFrame_O>());
  T_sp result = lisp_apply(this->sharedThis<Symbol_O>(), frame);
  return result;
}
T_sp Symbol_O::funcall() {
  _OF();
  ValueFrame_sp frame(ValueFrame_O::create(0, _Nil<ActivationFrame_O>()));
  T_sp result = lisp_apply(this->sharedThis<Symbol_O>(), frame);
  return result;
}
#endif

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
  if (!this->_HomePackage) return _Nil<T_O>();
  return this->_HomePackage;
}

void Symbol_O::setPackage(T_sp p) {
  ASSERTF(p, BF("The package is UNDEFINED"));
  ASSERT(p.nilp() || p.asOrNull<Package_O>());
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
    ss << "Name: " << this->_Name->get() << std::endl;
    if (!this->_HomePackage) {
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
    if (!this->_Function) {
      ss << "Function: NULL" << std::endl;
    } else if (this->_Function.unboundp()) {
      ss << "Function: UNBOUND" << std::endl;
    } else if (this->_Function.nilp()) {
      ss << "Function: nil" << std::endl;
    } else {
      ss << "Function: " << _rep_(this->_Function) << std::endl;
    }
    ss << "IsSpecial: " << this->_IsSpecial << std::endl;
    ss << "IsConstant: " << this->_IsConstant << std::endl;
    ss << "ReadOnlyFunction: " << this->_ReadOnlyFunction << std::endl;
    ss << "PropertyList: ";
    if (this->_PropertyList) {
      ss << _rep_(this->_PropertyList) << std::endl;
    } else {
      ss << "UNDEFINED" << std::endl;
    }
  }
  printf("%s", ss.str().c_str());
}

#if 0
void Symbol_O::remove_package(Package_sp pkg)
{
  if (this->_HomePackage == pkg) {
    this->_HomePackage = _Nil<T_O>();
  }
}
#endif

void Symbol_O::remove_package(Package_sp pkg)
{
  T_sp tpkg(pkg);
  this->_HomePackage = _Nil<T_O>();
};

CL_DEFUN T_sp core__symbol_global_value(Symbol_sp s) {
  return s->_GlobalValue;
}

CL_DEFUN void core__symbol_global_value_set(Symbol_sp s, T_sp v) {
  s->_GlobalValue = v;
}

};
