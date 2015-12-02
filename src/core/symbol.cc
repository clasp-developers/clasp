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
#define DEBUG_LEVEL_FULL

#include <clasp/core/useBoostPython.h>
#include <clasp/core/common.h>
#include <clasp/core/symbol.h>
#include <clasp/core/str.h>
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

#define ARGS_cl_symbolPlist "(sym)"
#define DECL_cl_symbolPlist ""
#define DOCS_cl_symbolPlist "Return the symbol plist"
List_sp cl_symbolPlist(Symbol_sp sym) {
  if (sym.nilp()) {
    return _Nil<List_V>();
  };
  return sym->plist();
}

#define ARGS_cl_get "(sym indicator &optional default)"
#define DECL_cl_get ""
#define DOCS_cl_get "Return the symbol plist"
T_sp cl_get(Symbol_sp sym, T_sp indicator, T_sp defval) {
  if (sym.nilp()) {
    return cl_getf(coerce_to_list(cl::_sym_nil), indicator, defval);
  }
  return cl_getf(sym->_PropertyList, indicator, defval);
}

#define ARGS_core_setfSymbolPlist "(sym plist)"
#define DECL_core_setfSymbolPlist ""
#define DOCS_core_setfSymbolPlist "Set the symbol plist"
void core_setfSymbolPlist(Symbol_sp sym, List_sp plist) {
  if (sym.nilp()) {
    SIMPLE_ERROR(BF("You cannot set the plist of nil"));
  };
  sym->setf_plist(plist);
}

#define ARGS_core_putprop "(sym val indicator)"
#define DECL_core_putprop ""
#define DOCS_core_putprop "Set the symbol plist"
T_sp core_putprop(Symbol_sp sym, T_sp val, T_sp indicator) {
  if (sym.nilp()) {
    SIMPLE_ERROR(BF("You cannot set the plist of nil"));
  };
  sym->_PropertyList = core_put_f(sym->_PropertyList, val, indicator);
  return val;
}

#define ARGS_af_boundp "(arg)"
#define DECL_af_boundp ""
#define DOCS_af_boundp "boundp"
bool af_boundp(Symbol_sp arg) {
  _G();
  if (arg.nilp())
    return true;
  return arg->boundP();
};

#define ARGS_af_symbolPackage "(arg)"
#define DECL_af_symbolPackage ""
#define DOCS_af_symbolPackage "symbolPackage"
T_sp af_symbolPackage(Symbol_sp arg) {
  _G();
  return arg->homePackage();
};

#define ARGS_af_symbolFunction "(arg)"
#define DECL_af_symbolFunction ""
#define DOCS_af_symbolFunction "symbolFunction"
Function_sp af_symbolFunction(Symbol_sp sym) {
  _G();
  if (!sym->fboundp()) {
    SIMPLE_ERROR(BF("No function bound to %s") % _rep_(sym));
  }
  return sym->symbolFunction();
};

#define ARGS_af_symbolName "(arg)"
#define DECL_af_symbolName ""
#define DOCS_af_symbolName "symbolName"
Str_sp af_symbolName(Symbol_sp arg) {
  _G();
  return arg->symbolName();
}

#define ARGS_af_symbolValue "(arg)"
#define DECL_af_symbolValue ""
#define DOCS_af_symbolValue "symbolValue"
T_sp af_symbolValue(const Symbol_sp arg) {
  if (!arg->boundP()) {
    SIMPLE_ERROR(BF("Symbol %s@%p is unbound") % _rep_(arg) % (void *)arg.raw_());
  }
  return arg->symbolValue();
};

#define ARGS_af_symbolValueAddress "(arg)"
#define DECL_af_symbolValueAddress ""
#define DOCS_af_symbolValueAddress "symbolValueAddress"
T_sp af_symbolValueAddress(const Symbol_sp arg) {
  _G();
  return Pointer_O::create(&arg->symbolValueRef());
};

#define ARGS_af_make_symbol "(name)"
#define DECL_af_make_symbol ""
#define DOCS_af_make_symbol "make_symbol"
Symbol_mv af_make_symbol(Str_sp name) {
  _G();
  Symbol_sp sym = Symbol_O::create(name->get());
  return (Values(sym));
};
};

namespace core {

/*! Construct a symbol that is incomplete, it has no Class or Package */
Symbol_O::Symbol_O(bool dummy) : T_O(),
                                 _HomePackage(_Nil<T_O>()),
                                 _Value(_Unbound<T_O>()),
                                 _Function(_Unbound<T_O>()),
                                 _SetfFunction(_Unbound<T_O>()),
                                 _IsSpecial(false),
                                 _IsConstant(false),
                                 _ReadOnlyFunction(false),
                                 _PropertyList(_Nil<List_V>()) { //no guard
  //  this->_Name = Str_O::create(name);
}

Symbol_O::Symbol_O() : Base(),
                       _Name(gctools::smart_ptr<Str_O>()),
                       _HomePackage(gctools::smart_ptr<Package_O>()),
                       _Value(gctools::smart_ptr<T_O>()),
                       _Function(gctools::smart_ptr<Function_O>()),
                       _SetfFunction(gctools::smart_ptr<Function_O>()),
                       _IsSpecial(false),
                       _IsConstant(false),
                       _ReadOnlyFunction(false)
                       // ,_PropertyList(gctools::smart_ptr<Cons_O>())
                       {
                        // nothing
                       };

void Symbol_O::finish_setup(Package_sp pkg, bool exportp) {
  ASSERTF(pkg, BF("The package is UNDEFINED"));
  //	printf("%s:%d finish_setup of symbol: %s:%s\n", __FILE__, __LINE__, pkg->getName().c_str(), this->_Name->get().c_str());
  this->_HomePackage = pkg;
  this->_Value = _Unbound<T_O>();
  this->_Function = _Unbound<Function_O>();
  this->_SetfFunction = _Unbound<Function_O>();
  pkg->add_symbol_to_package(this->symbolName()->get().c_str(), this->sharedThis<Symbol_O>(), exportp);
  this->_PropertyList = _Nil<T_O>();
}

Symbol_sp Symbol_O::create_at_boot(const string &nm) {
  _G();
  // This is used to allocate roots that are pointed
  // to by global variable _sym_XXX  and will never be collected
  Symbol_sp n = gctools::GCObjectAllocator<Symbol_O>::rootAllocate();
  ASSERTF(nm != "", BF("You cannot create a symbol without a name"));
#if VERBOSE_SYMBOLS
  if (nm.find("/dyn") != string::npos) {
    THROW_HARD_ERROR(BF("Illegal name for symbol[%s]") % nm);
  }
#endif
  n->_Name = Str_O::create(nm);
  return n;
};

Symbol_sp Symbol_O::create(const string &nm) {
  _G();
  // This is used to allocate roots that are pointed
  // to by global variable _sym_XXX  and will never be collected
  Symbol_sp n = gctools::GCObjectAllocator<Symbol_O>::rootAllocate(true);
  Str_sp snm = Str_O::create(nm);
  n->setf_name(snm);
  ASSERTF(nm != "", BF("You cannot create a symbol without a name"));
#if VERBOSE_SYMBOLS
  if (nm.find("/dyn") != string::npos) {
    THROW_HARD_ERROR(BF("Illegal name for symbol[%s]") % nm);
  }
#endif
#if 0
	n->_Name = Str_O::create(nm);
#endif
  return n;
};

bool Symbol_O::boundP() const {
  return !this->_Value.unboundp();
}

void Symbol_O::makunbound() {
  this->_Value = _Unbound<T_O>();
}

List_sp Symbol_O::plist() const {
  return this->_PropertyList;
}

void Symbol_O::setf_plist(List_sp plist) {
  this->_PropertyList = plist;
}

void Symbol_O::sxhash_(HashGenerator &hg) const {
  if (hg.isFilling())
    this->_Name->sxhash_(hg);
}

#define ARGS_Symbol_O_copy_symbol "(symbol &optional copy-properties)"
#define DECL_Symbol_O_copy_symbol ""
#define DOCS_Symbol_O_copy_symbol "copy_symbol"
Symbol_sp Symbol_O::copy_symbol(T_sp copy_properties) const {
  _G();
  Symbol_sp new_symbol = Symbol_O::create(this->_Name->get());
  if (copy_properties.isTrue()) {
    ASSERT(this->_Function);
    new_symbol->_Value = this->_Value;
    new_symbol->_Function = this->_Function;
    new_symbol->_IsConstant = this->_IsConstant;
    new_symbol->_ReadOnlyFunction = this->_ReadOnlyFunction;
    new_symbol->_PropertyList = cl_copyList(this->_PropertyList);
  }
  return new_symbol;
};

bool Symbol_O::isKeywordSymbol() {
  if (this->_HomePackage.nilp())
    return false;
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
  _G();
  if (node->loading()) {
    SIMPLE_ERROR(BF("You can't load symbols with archiveBase!! See Dumb_Node::createYourSymbol"));
  } else {
    string name = this->formattedName(true);
    node->attribute("_sym", name);
  }
}
#endif // defined(XML_ARCHIVE)

Symbol_sp Symbol_O::asKeywordSymbol() {
  _OF();
  if (this->_HomePackage.notnilp()) {
    Package_sp pkg = gc::As<Package_sp>(this->_HomePackage);
    if (pkg->isKeywordPackage())
      return this->asSmartPtr();
  }
  Symbol_sp kwSymbol = _lisp->internKeyword(this->symbolNameAsString());
  return kwSymbol;
};

T_sp Symbol_O::setf_symbolValue(T_sp val) {
  _OF();
  ASSERT(!this->_IsConstant);
#if 0
	// trap a change in a dynamic variable
	if ( this->_Name.as<Str_O>()->get() == "*THE-MODULE*")
	{
	    printf("%s:%d Changing value of a symbol named *THE-MODULE* to %s\n", __FILE__, __LINE__, _rep_(val).c_str());
	    if ( val.nilp() ) {
		printf("%s:%d Trap here - it changed to nil\n", __FILE__, __LINE__ );
	    }
	}
#endif
  this->_Value = val;
  return val;
}

void Symbol_O::makeSpecial() {
  this->_IsSpecial = true;
}

void Symbol_O::makeConstant(T_sp val) {
  _G();
  this->_Value = val;
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

void Symbol_O::setf_symbolValueReadOnlyOverRide(T_sp val) {
  _OF();
  this->_IsConstant = true;
  this->_Value = val;
}

T_sp Symbol_O::symbolValue() const {
  if (this->_Value.unboundp()) {
    SIMPLE_ERROR(BF("Unbound symbol-value for %s@@%p") % this->_Name->c_str() % this);
  }
  return this->_Value;
}

T_sp Symbol_O::symbolValueUnsafe() const {
  return this->_Value;
}

void Symbol_O::setf_symbolFunction(T_sp exec) {
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
      if ((currentPackage == myPackage) && !prefixAlways) {
        ss << this->_Name->get();
      } else {
        if (myPackage->isExported(this->const_sharedThis<Symbol_O>())) {
          ss << myPackage->getName() << ":" << this->_Name->get();
        } else {
          ss << myPackage->getName() << "::" << this->_Name->get();
        }
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
  _G();
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
  _G();
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

T_sp Symbol_O::getPackage() const {
  _G();
  if (!this->_HomePackage)
    return _Nil<T_O>();
  return this->_HomePackage;
}

void Symbol_O::setPackage(T_sp p) {
  _G();
  ASSERTF(p, BF("The package is UNDEFINED"));
  ASSERT(p.nilp() || p.asOrNull<Package_O>());
  this->_HomePackage = p;
}

void Symbol_O::exposeCando(Lisp_sp lisp) {
  _G();
  // TODO: By default these symbols like SPECIALP are being dumped into the COMMON-LISP package - don't do that.
  class_<Symbol_O>()
      .def("core:specialp", &Symbol_O::specialP)
      .def("core:STARmakeSpecial", &Symbol_O::makeSpecial)
      .def("core:STARmakeConstant", &Symbol_O::makeConstant)
      .def("core:fullName", &Symbol_O::fullName)
      .def("core:asKeywordSymbol", &Symbol_O::asKeywordSymbol)
      .def("core:setf_symbolFunction", &Symbol_O::setf_symbolFunction)
      .def("makunbound", &Symbol_O::makunbound)
      .def("cl:copy_symbol", &Symbol_O::copy_symbol,
           ARGS_Symbol_O_copy_symbol,
           DECL_Symbol_O_copy_symbol,
           DOCS_Symbol_O_copy_symbol)
      SYMBOL_EXPORT_SC_(ClPkg, make_symbol);
  Defun(make_symbol);
  SYMBOL_EXPORT_SC_(ClPkg, symbolName);
  Defun(symbolName);
  SYMBOL_EXPORT_SC_(ClPkg, symbolValue);
  Defun(symbolValueAddress);
  Defun(symbolValue);
  SYMBOL_EXPORT_SC_(ClPkg, symbolPackage);
  Defun(symbolPackage);
  SYMBOL_EXPORT_SC_(ClPkg, symbolFunction);
  Defun(symbolFunction);
  SYMBOL_EXPORT_SC_(ClPkg, boundp);
  Defun(boundp);
  ClDefun(symbolPlist);
  ClDefun(get);
  CoreDefun(setfSymbolPlist);
  CoreDefun(putprop);
}

void Symbol_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, Symbol, "", "", _lisp)
      .def("fullName", &Symbol_O::fullName)
      //	    .def("namestring",&Symbol_O::fullName)
      .def("asKeywordSymbol", &Symbol_O::asKeywordSymbol);
#endif
}

void Symbol_O::dump() {
  _G();
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
    if (!this->_Value) {
      ss << "VALUE: NULL" << std::endl;
    } else if (this->_Value.unboundp()) {
      ss << "Value: UNBOUND" << std::endl;
    } else if (this->_Value.nilp()) {
      ss << "Value: nil" << std::endl;
    } else {
      ss << "Value: " << _rep_(this->_Value) << std::endl;
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

EXPOSE_CLASS(core, Symbol_O);
};
