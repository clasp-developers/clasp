/*
    File: wrappers.h
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

  class TranslationFunctor_O : public BuiltinClosure_O {
    LISP_CLASS(core,CorePkg,TranslationFunctor_O,"TranslationFunctor",BuiltinClosure_O);
  public:
    typedef core::T_O* (*Type)(core::T_O* arg);
    Type fptr;
  public:
  TranslationFunctor_O(FunctionDescription* fdesc, Type ptr) : BuiltinClosure_O(TranslationFunctor_O::entry_point,fdesc), fptr(ptr) {};
  public:
    typedef BuiltinClosure_O TemplatedBase;
    virtual size_t templatedSizeof() const { return sizeof(TranslationFunctor_O); };
    static inline LCC_RETURN LISP_CALLING_CONVENTION() {
      TranslationFunctor_O* closure = gctools::untag_general<TranslationFunctor_O*>((TranslationFunctor_O*)lcc_closure);
      return gctools::return_type((closure->fptr)(lcc_fixed_arg0),1);
    }
  };

  template <typename FN>
    class VariadicFunctor : public BuiltinClosure_O {
  public:
    typedef BuiltinClosure_O TemplatedBase;
    virtual size_t templatedSizeof() const { return sizeof(VariadicFunctor<FN>); };
  };
};


/*! Make every templated VariadicFunctor KIND the same as the VariadicFunctor<T>::TemplatedBase */
template <typename T>
class gctools::GCStamp<core::VariadicFunctor<T>> {
public:
  static gctools::GCStampEnum const Stamp = gctools::GCStamp<typename core::VariadicFunctor<T>::TemplatedBase>::Stamp;
};


#include <clasp/core/arguments.h>
#include <clasp/core/lambdaListHandler.fwd.h>
namespace core {
#include <clasp/core/wrappers_functoids.h>
};

namespace core {
template <int DispatchOn, typename FN>
class VariadicMethoid : public BuiltinClosure_O {
public:
  typedef BuiltinClosure_O TemplatedBase;
  size_t templatedSizeof() const { return sizeof(VariadicMethoid<DispatchOn, FN>); };
};

#include <clasp/core/wrappers_methoids.h>
};

namespace core {

  inline void wrap_translator(const string &packageName, const string &name, core::T_O* (*fp)(core::T_O*), const string& filename,  const string &arguments = "", const string &declares = "", const string &docstring = "", const string &sourceFile = "", int sourceLine = 0) {
    Symbol_sp symbol = lispify_intern(name, packageName);
    FunctionDescription* fdesc = makeFunctionDescription(symbol);
    BuiltinClosure_sp f = gctools::GC<TranslationFunctor_O>::allocate(fdesc,fp);
    lisp_defun(symbol, packageName, f, arguments, declares, docstring, sourceFile, sourceLine, 1 );
    validateFunctionDescription(__FILE__,__LINE__,f);
  }



// this is used in gc_interface.cc expose_function
 template <typename RT, typename... ARGS>
void wrap_function(const string &packageName, const string &name, RT (*fp)(ARGS...), const string &arguments = "", const string &declares = "", const string &docstring = "", const string &sourceFile = "", int sourceLine = 0) {
   Symbol_sp symbol = _lisp->intern(name, packageName);
   FunctionDescription* fdesc = makeFunctionDescription(symbol);
   BuiltinClosure_sp f = gctools::GC<VariadicFunctor<RT(ARGS...)>>::allocate(fdesc,fp);
   lisp_defun(symbol, packageName, f, arguments, declares, docstring, sourceFile, sourceLine, sizeof...(ARGS));
   validateFunctionDescription(__FILE__,__LINE__,f);
 }

// this is used in gc_interface.cc expose_function_setf
  template <typename RT, typename... ARGS>
void wrap_function_setf(const string &packageName, const string &name, RT (*fp)(ARGS...), const string &arguments = "", const string &declares = "", const string &docstring = "", const string &sourceFile = "", int sourceLine = 0) {
  Symbol_sp symbol = _lisp->intern(name, packageName);
  FunctionDescription* fdesc = makeFunctionDescription(symbol);
  BuiltinClosure_sp f = gctools::GC<VariadicFunctor<RT(ARGS...)>>::allocate(fdesc,fp);
  lisp_defun_setf(symbol, packageName, f, arguments, declares, docstring, sourceFile, sourceLine, sizeof...(ARGS));
  validateFunctionDescription(__FILE__,__LINE__,f);
  }



};

template <int DispatchOn, typename T>
class gctools::GCStamp<core::VariadicMethoid<DispatchOn, T>> {
public:
  static gctools::GCStampEnum const Stamp = gctools::GCStamp<typename core::VariadicMethoid<DispatchOn, T>::TemplatedBase>::Stamp;
};

namespace core {

class Function_O;

// -----------------------------------
//
// Macro stuff
//
//
// Wrapper for ActivationFrameMacroPtr

// basically like wrap_function.
inline void defmacro(const string &packageName, const string &name, T_mv (*mp)(List_sp, T_sp env), const string &arguments, const string &declares, const string &docstring, const string &sourcePathname, int lineno) {
  Symbol_sp symbol = lispify_intern(name, packageName);
  FunctionDescription* fdesc = makeFunctionDescription(symbol);
  BuiltinClosure_sp f = gc::GC<VariadicFunctor<T_mv(List_sp, T_sp)>>::allocate(fdesc,mp);
  lisp_defmacro(symbol, packageName, f, arguments, declares, docstring);
  validateFunctionDescription(__FILE__,__LINE__,f);
}

 
template <int N>
struct DispatchOn {
  enum { value = N };
};

// ----------------------------------------
// ----------------------------------------
// ----------------------------------------
// ----------------------------------------


extern Symbol_sp& _sym_STARallCxxClassesSTAR;


template <typename OT>
class class_ {
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
    lisp_pushClassSymbolOntoSTARallCxxClassesSTAR(OT::static_classSymbol());

//
// If the class isn't in the class table then add it
//
    if (makerName != "") {
      // use make-<className>
      std::string magic_maker_name = core::magic_name(makerName,OT::static_packageName());
      std::string pkg_part;
      std::string symbol_part;
      core::colon_split( magic_maker_name, pkg_part, symbol_part);
      wrap_function(pkg_part,symbol_part, &new_LispObject<OT>);
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
 
  // non-const function dispatch on parameter 0
  template <typename RT, class... ARGS>
  class_ &def(string const &name, RT (OT::*mp)(ARGS...),
              string const &lambda_list = "", const string &declares = "", const string &docstring = "", bool autoExport = true)
  {
    std::string pkgName;
    std::string symbolName;
    core::colon_split(name,pkgName,symbolName);
    if (pkgName == "") {
      pkgName = symbol_packageName(this->_ClassSymbol);
    }
    Symbol_sp symbol = _lisp->intern(symbolName,pkgName);
    FunctionDescription* fdesc = makeFunctionDescription(symbol);
    BuiltinClosure_sp m = gc::GC<VariadicMethoid<0, RT (OT::*)(ARGS...)>>::allocate(fdesc,mp);
    lisp_defineSingleDispatchMethod(symbol, this->_ClassSymbol, m, 0, true, lambda_list, declares, docstring, autoExport, sizeof...(ARGS)+1);
    validateFunctionDescription(__FILE__,__LINE__,m);
    return *this;
  }
 
  // const function dispatch on parameter 0
  template <typename RT, class... ARGS>
  class_ &def(string const &name, RT (OT::*mp)(ARGS...) const,
              string const &lambda_list = "", const string &declares = "", const string &docstring = "", bool autoExport = true)
  {
    std::string pkgName;
    std::string symbolName;
    core::colon_split(name,pkgName,symbolName);
    if (pkgName == "") {
      pkgName = symbol_packageName(this->_ClassSymbol);
    }
    Symbol_sp symbol = _lisp->intern(symbolName,pkgName);
    FunctionDescription* fdesc = makeFunctionDescription(symbol);
    BuiltinClosure_sp m = gc::GC<VariadicMethoid<0, RT (OT::*)(ARGS...) const>>::allocate(fdesc,mp);
    lisp_defineSingleDispatchMethod(symbol, this->_ClassSymbol, m, 0, true, lambda_list, declares, docstring, autoExport, sizeof...(ARGS)+1);
    validateFunctionDescription(__FILE__,__LINE__,m);
    return *this;
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
    lisp_defparameter(symbol, this->_Converter);
  }

  enum_ &value(Symbol_sp const &sym, X value) {
    _G();
    lisp_extendSymbolToEnumConverter(this->_Converter, sym, sym, static_cast<int>(value));
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

//ostream& operator<<(ostream& out, gctools::smart_ptr<core::T_O>

#endif // wrappers_h
