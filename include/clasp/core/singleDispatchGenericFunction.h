/*
    File: singleDispatchGenericFunction.h
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
#ifndef _SINGLEDISPATCHGENERICFUNCTION_H_
#define _SINGLEDISPATCHGENERICFUNCTION_H_

#include <clasp/core/foundation.h>
#include <clasp/core/hashTable.fwd.h>
#include <clasp/core/singleDispatchGenericFunction.fwd.h>
#include <clasp/core/singleDispatchMethod.fwd.h>
#include <clasp/core/singleDispatchEffectiveMethodFunction.fwd.h>

namespace core {

class SingleDispatchGenericFunctionClosure : public FunctionClosure {
public:
  /*! Store the methods here */
  List_sp _Methods;
  LambdaListHandler_sp _lambdaListHandler;
  /*! Store the method functions hashed on the receiver class */
  //	HashTable_sp	classesToClosures;
public:
  DISABLE_NEW();
  SingleDispatchGenericFunctionClosure(T_sp name, Symbol_sp k, SOURCE_INFO)
      : FunctionClosure(name, k, _Nil<T_O>() /*Env*/, SOURCE_INFO_PASS), _Methods(_Nil<T_O>()), _lambdaListHandler(_Nil<LambdaListHandler_O>()){};
  SingleDispatchGenericFunctionClosure(T_sp name)
      : FunctionClosure(name), _Methods(_Nil<T_O>()), _lambdaListHandler(_Nil<LambdaListHandler_O>()){};
  void finishSetup(LambdaListHandler_sp llh, Symbol_sp k) {
    this->_lambdaListHandler = llh;
    this->kind = k;
  }
  T_sp lambdaList() const;
  virtual size_t templatedSizeof() const { return sizeof(*this); };
  virtual const char *describe() const { return "SingleDispatchGenericFunctionClosure"; };
  LCC_VIRTUAL LCC_RETURN LISP_CALLING_CONVENTION();
  bool singleDispatchGenericP() const { return true; };

  /*! Define a method to this SingleDispatchGenericFunction
	  If there is already a method with the same receiver then replace it
	  unless it's locked. Whenever a method is defined the method combination table
	  is wiped out */
  void addMethod(SingleDispatchMethod_sp method);
  LambdaListHandler_sp lambdaListHandler() const { return this->_lambdaListHandler; };

  Function_sp slowMethodLookup(Class_sp mc);
  Function_sp computeEffectiveMethodFunction(gctools::Vec0<SingleDispatchMethod_sp> const &applicableMethods);
};

class SingleDispatchGenericFunction_O : public Function_O {
  LISP_BASE1(Function_O);
  LISP_CLASS(core, CorePkg, SingleDispatchGenericFunction_O, "single-dispatch-generic-function");
  DECLARE_INIT();
  //    DECLARE_ARCHIVE();
  friend class SingleDispatchGenericFunctoid;

public: // Simple default ctor/dtor
  DEFAULT_CTOR_DTOR(SingleDispatchGenericFunction_O);

public: // ctor/dtor for classes with shared virtual base
        //    explicit SingleDispatchGenericFunction_O(core::Class_sp const& mc) : T_O(mc), Function(mc) {};
        //    virtual ~SingleDispatchGenericFunction_O() {};
public:
  static SingleDispatchGenericFunction_sp create(T_sp functionName, LambdaListHandler_sp llhandler);

public: // Functions here
  /*! Return the Cons of methods attached to this SingleDispatchGenericFunction */
  List_sp methods() const {
    gctools::tagged_pointer<SingleDispatchGenericFunctionClosure> cl = this->closure.as<SingleDispatchGenericFunctionClosure>();
    return cl->_Methods;
  };

}; // SingleDispatchGenericFunction class

}; // core namespace
TRANSLATE(core::SingleDispatchGenericFunction_O);

namespace core {
class SingleDispatchGenericFunctoid : public Closure {
private:
  SingleDispatchGenericFunction_sp _sdgf;

public:
  DISABLE_NEW();
  virtual const char *describe() const { return "SingleDispatchGenericFunctoid"; };
  LCC_VIRTUAL LCC_RETURN LISP_CALLING_CONVENTION() {
    IMPLEMENT_MEF(BF("Handle single dispatch"));
#if 0            
	    *lcc_resultP = this->_sdgf->INVOKE(lcc_nargs, nargs,args);
#endif
  }
};

class Lambda_emf : public FunctionClosure {
  FRIEND_GC_SCANNER(core::Lambda_emf);

private:
  /*! Store the method_function that this emf invokes.
	  This function takes two arguments: (args next-emfun) */
  Function_sp _method_function;

public:
  virtual const char *describe() const { return "Lambda_emf"; };
  bool requires_activation_frame() const { return true; };
  virtual size_t templatedSizeof() const { return sizeof(*this); };

public:
  Lambda_emf(T_sp name,
             SingleDispatchGenericFunction_sp gf,
             SingleDispatchMethod_sp cur_method);
  DISABLE_NEW();

  LCC_VIRTUAL LCC_RETURN LISP_CALLING_CONVENTION() {
    IMPLEMENT_ME();
#if 0
            // The closedEnv 
            ASSERTF(closedEnv.nilp(),BF("Since I don't pass the closedEnv forward it I expect that it should always be nil - this time it wasn't - figure out what is up with that"));
            return this->_method_function->INVOKE(nargs,args);
#endif
  }
};

T_sp af_ensureSingleDispatchGenericFunction(Symbol_sp gfname, LambdaListHandler_sp llhandler);
};

#endif /* _SINGLEDISPATCHGENERICFUNCTION_H_ */
