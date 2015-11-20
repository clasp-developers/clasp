/*
    File: external_wrappers.h
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
#ifndef external_wrappers_H
#define external_wrappers_H

#include <clasp/core/lispDefinitions.h>

#include <clasp/core/wrappers.h>
//#include "clbind.h"

namespace core {

//    using namespace clbind::policies;

#include <clasp/core/external_policies.h>
using namespace policies;

template <typename Policies, typename OT, typename Method>
class IndirectVariadicMethoid : public BuiltinClosure {
  typedef BuiltinClosure TemplatedBase;
  virtual size_t templatedSizeof() const { return sizeof(*this); };
  virtual const char *describe() const { return "IndirectVariadicMethoid"; };
};

#include <clasp/core/generated/external_wrappers_indirect_methoids.h>
};

namespace core {
template <class D, class C>
class GetterMethoid : public BuiltinClosure {
public:
  typedef BuiltinClosure TemplatedBase;

public:
  //        typedef std::function<void (OT& ,)> Type;
  typedef D(C::*MemPtr);
  MemPtr mptr;
  GetterMethoid(core::T_sp name, MemPtr ptr) : BuiltinClosure(name), mptr(ptr){};
  DISABLE_NEW();
  virtual size_t templatedSizeof() const { return sizeof(*this); };
};
};

template <typename Policies, typename OT, typename Method>
class gctools::GCKind<core::IndirectVariadicMethoid<Policies, OT, Method>> {
public:
  static gctools::GCKindEnum const Kind = gctools::GCKind<typename core::IndirectVariadicMethoid<Policies, OT, Method>::TemplatedBase>::Kind;
};

namespace core {

// ----------------------------------------
// ----------------------------------------
// ----------------------------------------
// ----------------------------------------
extern Symbol_sp _sym_STARallCxxClassesSTAR;

template <typename OT>
class externalClass_ {
private:
  Symbol_sp _ClassSymbol;

public:
  void setup_class(const string &makerName) {
    _G();
    if (IS_SYMBOL_UNDEFINED(OT::static_classSymbol())) {
      SIMPLE_ERROR(BF("Attempting to add methods for "
                      "class that isn't defined yet"));
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
    T_sp theClass = lisp_boot_findClassBySymbolOrNil(OT::static_classSymbol());
    if (theClass.nilp()) {
      LOG(BF("Adding class(%s) to environment") % OT::static_className());
      lisp_addClass(OT::static_classSymbol(),
                    OT::static_creator,
                    OT::Bases::baseClass1Id(),
                    OT::Bases::baseClass2Id());
    }
    if (makerName != "") {
      // use make-<className>
      DEPRECIATED();
      af_def(OT::static_packageName(), makerName, &new_LispObject<OT>);
    }
  }

  //
  //
  // ctor
  //
  //

  externalClass_() {
    _G();
    this->setup_class("");
  }

  externalClass_(const string &makerName) {
    _G();
    this->setup_class(makerName);
  }

  template <typename RT, class... ARGS>
  externalClass_ &def(string const &name, RT (OT::*mp)(ARGS...), string const &lambda_list = "", const string &declares = "", const string &docstring = "", bool autoExport = true) {
    _G();
    Symbol_sp symbol = lispify_intern(name, symbol_packageName(this->_ClassSymbol));
    gctools::tagged_pointer<BuiltinClosure> m = gctools::ClassAllocator<VariadicMethoid<0, RT (OT::*)(ARGS...)>>::allocateClass(symbol, mp);
    lisp_defineSingleDispatchMethod(symbol, this->_ClassSymbol, m, 0, lambda_list, declares, docstring, autoExport, sizeof...(ARGS)+1);
    return *this;
  }

  template <typename RT, class... ARGS>
  externalClass_ &def(string const &name, RT (OT::*mp)(ARGS...) const,
                      string const &lambda_list = "", const string &declares = "", const string &docstring = "", bool autoExport = true) {
    _G();
    Symbol_sp symbol = lispify_intern(name, symbol_packageName(this->_ClassSymbol));
    gc::tagged_pointer<BuiltinClosure> m = gctools::ClassAllocator<VariadicMethoid<0, RT (OT::*)(ARGS...) const>>::allocateClass(symbol, mp);
    lisp_defineSingleDispatchMethod(symbol, this->_ClassSymbol, m, 0, lambda_list, declares, docstring, autoExport, sizeof...(ARGS)+1);
    return *this;
  }

  template <typename RT, class... ARGS>
  externalClass_ &def(const string &name, RT (OT::ExternalType::*mp)(ARGS...),
                      const string &lambda_list = "", const string &declares = "", const string &docstring = "", bool autoExport = true) {
    _G();
    Symbol_sp symbol = lispify_intern(name, symbol_packageName(this->_ClassSymbol));
    gc::tagged_pointer<BuiltinClosure> m = gctools::ClassAllocator<IndirectVariadicMethoid<policies_<>, OT, RT (OT::ExternalType::*)(ARGS...)>>::allocateClass(symbol, mp);
    lisp_defineSingleDispatchMethod(symbol, this->_ClassSymbol, m, 0, lambda_list, declares, docstring, autoExport, sizeof...(ARGS)+1);
    return *this;
  }

  template <typename RT, class... ARGS>
  externalClass_ &def(const string &name, RT (OT::ExternalType::*mp)(ARGS...) const,
                      const string &lambda_list = "", const string &declares = "", const string &docstring = "", bool autoExport = true) {
    _G();
    Symbol_sp symbol = lispify_intern(name, symbol_packageName(this->_ClassSymbol));
    gc::tagged_pointer<BuiltinClosure> m = gctools::ClassAllocator<IndirectVariadicMethoid<policies_<>, OT, RT (OT::ExternalType::*)(ARGS...) const>>::allocateClass(symbol, mp);
    lisp_defineSingleDispatchMethod(symbol, this->_ClassSymbol, m, 0, lambda_list, declares, docstring, autoExport, sizeof...(ARGS)+1);
    return *this;
  }

  template <class C, class D>
  externalClass_ &def_readonly(string const &name, D C::*mem_ptr) {
#if 0
    Symbol_sp symbol = lispify_intern(name, symbol_packageName(this->_ClassSymbol));
    gc::tagged_pointer<BuiltinClosure> m = gctools::ClassAllocator<GetterMethoid<D, C>>::allocateClass(symbol, mem_ptr);
            lisp_defineSingleDispatchMethod(symbol
                                            ,this->_ClassSymbol
                                            ,m
                                            ,0
                                            ,lambda_list
                                            ,declares
                                            ,docstring
                                            ,autoExport
                                            ,sizeof...(ARGS)+1);
#endif
    return *this;
  }
};
};

#endif // external_wrappers_h
