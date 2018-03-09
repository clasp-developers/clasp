/*
    File: derived_wrappers.h
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
#ifndef core_wrappers_H
#define core_wrappers_H

#include <functional>
#include <clasp/core/lispDefinitions.h>
#include <clasp/core/symbolToEnumConverter.h>
#include <clasp/core/activationFrame.h>

//#define DEBUG_METHOIDS 1

namespace core {

template <int DispatchOn, typename FN>
class VariadicMethoid : public Functoid {};

#ifdef BUILDING_CLASP
#include <wrappers_methoids.h>
#else
#ifdef USE_CLASP_DEBUG
#ifdef USE_CLASP_BOEHM
#include <clasp/core/debug/boehm/wrappers_methoids.h>
#else
#include <clasp/core/debug/mps/wrappers_methoids.h>
#endif
#else
#ifdef USE_CLASP_BOEHM
#include <clasp/core/release/boehm/wrappers_methoids.h>
#else
#include <clasp/core/release/mps/wrappers_methoids.h>
#endif
#endif
#endif
};

namespace core {

#define DEF(ClassName, FunctionName) def(#FunctionName, &ClassName::FunctionName, ARGS_##ClassName##_##FunctionName, DECL_##ClassName##_##FunctionName, DOCS_##ClassName##_##FunctionName, true)
};

namespace core {

class Function_O;

template <int N>
struct DispatchOn {
  enum { value = N };
};

// ----------------------------------------
// ----------------------------------------
// ----------------------------------------
// ----------------------------------------

extern Symbol_sp _sym_STARallCxxClassesSTAR;

template <typename OT>
class derived_class_ {
public:
  typedef OT wrapped_type;

private:
  Symbol_sp _ClassSymbol;

public:
  void setup_class(const string &makerName = "") {
    _G();
    if (IS_SYMBOL_UNDEFINED(OT::static_classSymbol())) {
      SIMPLE_ERROR_SPRINTF("Attempting to add methods for class that isn't defined yet");
    }

    this->_ClassSymbol = OT::static_classSymbol();
    reg::lisp_registerClassSymbol<OT>(this->_ClassSymbol);

    /*! Accumulate all of the classes in reverse order of how they were initialized
	      in the core::*all-cxx-classes* variable */
    if (_sym_STARallCxxClassesSTAR->symbolValueUnsafe()) {
      _sym_STARallCxxClassesSTAR->setf_symbolValue(Cons_O::create(OT::static_classSymbol(), _sym_STARallCxxClassesSTAR->symbolValue()));
    }
    //
    // If the class isn't in the class table then add it
    //
    if (lisp_boot_findClassBySymbolOrNil(OT::static_classSymbol()).nilp()) {
      DEPRECATED();
      LOG(BF("Adding class(%s) to environment") % OT::static_className());
      lisp_addClass(/*_lisp,OT::static_packageName(),
				OT::static_className(), */
                    OT::static_classSymbol(),
                    OT::static_allocator,
                    OT::Bases::baseClass1Id(),
                    OT::Bases::baseClass2Id());
    }
    if (makerName != "") {
      // use make-<className>
      af_def(OT::static_packageName(), makerName, &new_LispObject<OT>);
    }
  }

  //
  //
  // ctor
  //
  //

  class_() {
    _G();
    this->setup_class("");
  }

  class_(const string &makerName) {
    _G();
    this->setup_class(makerName);
  }

  class_(const string &packageName, const string &className) {
    _G();
    this->setup_class(packageName, className);
  }
};

template <typename oclass>
void defaultExposeCando(Lisp_sp lisp) {
  _G();
  // Only expose the class, don't create any methods
  // By default put class in the Cl package
  class_<oclass>();
}

struct EnumValueDefinition {
  string _Name;
  int _Value;
};

template <typename X>
class enum_ {
private:
  SymbolToEnumConverter_sp _Converter;
  Symbol_sp _PredefinedConverterSymbolId;

public:
  enum_(Symbol_sp symbol, const string &title) {
    _G();
    this->_PredefinedConverterSymbolId = symbol;
    this->_Converter = SymbolToEnumConverter_O::create(title);
    symbol->defparameter(this->_Converter);
  }

  enum_ &value(Symbol_sp const &sym, X value) {
    _G();
    lisp_extendSymbolToEnumConverter(this->_Converter, sym, sym, value);
    return *this;
  }
  enum_ &value(Symbol_sp const &name, Symbol_sp const &archiveName, X value) {
    _G();
    lisp_extendSymbolToEnumConverter(this->_Converter, name, archiveName, value);
    return *this;
  }
  Symbol_sp symbolFromEnum(int value) {
    _G();
    return lisp_lookupSymbolForEnum(this->_PredefinedConverterSymbolId, (int)(value));
  }
};
};

//ostream& operator<<(ostream& out, gctools::smart_ptr<core::T_O> obj);

#include <clasp/core/python_wrappers.h>

#endif // wrappers_h
