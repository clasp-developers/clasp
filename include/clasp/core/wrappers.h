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

  class TranslationFunctor : public BuiltinClosure_O {
  public:
    typedef core::T_O* (*Type)(core::T_O* arg);
    Type fptr;
  public:
  TranslationFunctor(T_sp name, Symbol_sp funcType, Type ptr, SOURCE_INFO) : BuiltinClosure_O(TranslationFunctor::entry_point,name,funcType,SOURCE_INFO_PASS), fptr(ptr) {};
  public:
    typedef BuiltinClosure_O TemplatedBase;
    virtual size_t templatedSizeof() const { return sizeof(TranslationFunctor); };
    static inline LCC_RETURN LISP_CALLING_CONVENTION() {
      TranslationFunctor* closure = gctools::untag_general<TranslationFunctor*>((TranslationFunctor*)lcc_closure);
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
class gctools::GCKind<core::VariadicFunctor<T>> {
public:
  static gctools::GCKindEnum const Kind = gctools::GCKind<typename core::VariadicFunctor<T>::TemplatedBase>::Kind;
};


#include <clasp/core/arguments.h>
#include <clasp/core/lambdaListHandler.fwd.h>
namespace core {
#include <clasp/core/generated/wrappers_functoids.h>
};

namespace core {
template <int DispatchOn, typename FN>
class VariadicMethoid : public BuiltinClosure_O {
public:
  typedef BuiltinClosure_O TemplatedBase;
  size_t templatedSizeof() const { return sizeof(VariadicMethoid<DispatchOn, FN>); };
};

#include <clasp/core/generated/wrappers_methoids.h>
};

namespace core {

  inline void wrap_translator(const string &packageName, const string &name, core::T_O* (*fp)(core::T_O*), const string &arguments = "", const string &declares = "", const string &docstring = "", const string &sourceFile = "", int sourceLine = 0) {
  Symbol_sp symbol = lispify_intern(name, packageName);
  SourcePosInfo_sp spi = lisp_createSourcePosInfo(sourceFile, 0, sourceLine);
  BuiltinClosure_sp f = gctools::GC<TranslationFunctor>::allocate(symbol, kw::_sym_function, fp, SOURCE_POS_INFO_FIELDS(spi));
  lisp_defun(symbol, packageName, f, arguments, declares, docstring, sourceFile, sourceLine, true, 1);
}



// this is used in gc_interface.cc expose_function
 template <typename RT, typename... ARGS>
void wrap_function(const string &packageName, const string &name, RT (*fp)(ARGS...), const string &arguments = "", const string &declares = "", const string &docstring = "", const string &sourceFile = "", int sourceLine = 0) {
  Symbol_sp symbol = _lisp->intern(name, packageName);
  SourcePosInfo_sp spi = lisp_createSourcePosInfo(sourceFile, 0, sourceLine);
  BuiltinClosure_sp f = gctools::GC<VariadicFunctor<RT(ARGS...)>>::allocate(symbol, kw::_sym_function, fp, SOURCE_POS_INFO_FIELDS(spi));
  lisp_defun(symbol, packageName, f, arguments, declares, docstring, sourceFile, sourceLine, true, sizeof...(ARGS));
}

};

template <int DispatchOn, typename T>
class gctools::GCKind<core::VariadicMethoid<DispatchOn, T>> {
public:
  static gctools::GCKindEnum const Kind = gctools::GCKind<typename core::VariadicMethoid<DispatchOn, T>::TemplatedBase>::Kind;
};

namespace core {
};

namespace core {

class Function_O;

// -----------------------------------
//
// Macro stuff
//
//
// Wrapper for ActivationFrameMacroPtr
   

inline void defmacro(const string &packageName, const string &name, T_mv (*mp)(List_sp, T_sp env), const string &arguments, const string &declares, const string &docstring, const string &sourceFileName, int lineno, bool autoExport = true) {
  _G();
  Symbol_sp symbol = lispify_intern(name, packageName);
  SourcePosInfo_sp spi = lisp_createSourcePosInfo(sourceFileName, 0, lineno);
  BuiltinClosure_sp f = gc::GC<MacroClosure_O>::allocate(symbol, mp, SOURCE_POS_INFO_FIELDS(spi));
  lisp_defmacro(symbol, packageName, f, arguments, declares, docstring, autoExport);
}

 
template <int N>
struct DispatchOn {
  enum { value = N };
};

// ----------------------------------------
// ----------------------------------------
// ----------------------------------------
// ----------------------------------------

#if 0
 struct MethodDefinition {
  string _Name;
  int _ClassSymbol;
  Function_sp _Methoid;
};
#endif
//    typedef	enum { no_init,class_name_init, make_class_name_init } maker_enum;

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
      SIMPLE_ERROR(BF("Attempting to add methods for "
                      "class that isn't defined yet"));
    }

    this->_ClassSymbol = OT::static_classSymbol();

#if 0
            OT dummy;
            size_t offsetT = (size_t)((char*)(dynamic_cast<T_O*>(&dummy)) - (char*)(&dummy));
            size_t offsetGCO = (size_t)((char*)(dynamic_cast<gctools::GCObject*>(&dummy)) - (char*)(&dummy));
            printf("%s:%d %50s offsetof(T_O) = %3lu  offsetof(gctools::GCObject) = %3lu\n", __FILE__, __LINE__, typeid(OT).name(),offsetT, offsetGCO);
#endif

    reg::lisp_registerClassSymbol<OT>(this->_ClassSymbol);

    /*! Accumulate all of the classes in reverse order of how they were initialized
	      in the core::*all-cxx-classes* variable */
    lisp_pushClassSymbolOntoSTARallCxxClassesSTAR(OT::static_classSymbol());

//
// If the class isn't in the class table then add it
//
#if 0
	    if ( lisp_boot_findClassBySymbolOrNil(OT::static_classSymbol()).nilp())
	    {
                DEPRECATED();
		LOG(BF("Adding class(%s) to environment")% OT::static_className() );
		lisp_addClass(/*_lisp,OT::static_packageName(),
				OT::static_className(), */
		    OT::static_classSymbol(),
		    OT::static_creator,
		    OT::Bases::baseClass1Id(),
		    OT::Bases::baseClass2Id() );
	    }
#endif
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
    BuiltinClosure_sp m = gc::GC<VariadicMethoid<0, RT (OT::*)(ARGS...)>>::allocate(symbol, mp);
    lisp_defineSingleDispatchMethod(symbol, this->_ClassSymbol, m, 0, lambda_list, declares, docstring, autoExport, sizeof...(ARGS)+1);
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
    BuiltinClosure_sp m = gc::GC<VariadicMethoid<0, RT (OT::*)(ARGS...) const>>::allocate(symbol, mp);
    lisp_defineSingleDispatchMethod(symbol, this->_ClassSymbol, m, 0, lambda_list, declares, docstring, autoExport, sizeof...(ARGS)+1);
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

//ostream& operator<<(ostream& out, gctools::smart_ptr<core::T_O> obj);

#include <clasp/core/python_wrappers.h>

#endif // wrappers_h
