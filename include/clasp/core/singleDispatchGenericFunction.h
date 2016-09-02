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
//#include <clasp/core/singleDispatchEffectiveMethodFunction.fwd.h>



namespace core {

  FORWARD(SingleDispatchCxxEffectiveMethodFunction);  
  class SingleDispatchCxxEffectiveMethodFunction_O : public FunctionClosure_O {
    LISP_CLASS(core,CorePkg,SingleDispatchCxxEffectiveMethodFunction_O,"SingleDispatchCxxEffectiveMethodFunction",FunctionClosure_O);
  public:
    const char *describe() const { return "SingleDispatchCxxEffectiveMethodFunction"; };
  public:
    CxxMethodFunction_sp _onlyCxxMethodFunction;
  public:
    T_sp lambda_list() const { return _Nil<T_O>();};
  SingleDispatchCxxEffectiveMethodFunction_O(T_sp name, CxxMethodFunction_sp mf) : Base(name), _onlyCxxMethodFunction(mf) {};
    LCC_RETURN LISP_CALLING_CONVENTION();
  };

  FORWARD(SingleDispatchEffectiveMethodFunction);  
  class SingleDispatchEffectiveMethodFunction_O : public FunctionClosure_O {
    LISP_CLASS(core,CorePkg,SingleDispatchEffectiveMethodFunction_O,"SingleDispatchEffectiveMethodFunction",FunctionClosure_O);
  public:
    const char *describe() const { return "SingleDispatchEffectiveMethodFunction"; };
  public:
    List_sp _Befores;
    List_sp _Primaries;
    List_sp _Afters;
  public:
    T_sp lambda_list() const { return _Nil<T_O>();};
  SingleDispatchEffectiveMethodFunction_O(T_sp name, List_sp befores, List_sp primaries, List_sp afters) : Base(name), _Befores(befores),_Primaries(primaries),_Afters(afters) {};
    LCC_RETURN LISP_CALLING_CONVENTION();
  };
};


template <>
struct gctools::GCInfo<core::SingleDispatchGenericFunctionClosure_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};
namespace core {
  FORWARD(SingleDispatchMethod);
  class SingleDispatchGenericFunctionClosure_O : public FunctionClosure_O {
    LISP_CLASS(core,CorePkg,SingleDispatchGenericFunctionClosure_O,"SingleDispatchGenericFunctionClosure",FunctionClosure_O);
  public:
  /*! Store the methods here */
    List_sp _Methods;
    LambdaListHandler_sp _lambdaListHandler;
  /*! Store the method functions hashed on the receiver class */
  //	HashTable_sp	classesToClosures;
  public:
      static SingleDispatchGenericFunctionClosure_sp create(T_sp functionName, LambdaListHandler_sp llhandler);
public:
    DISABLE_NEW();
  SingleDispatchGenericFunctionClosure_O(T_sp name, Symbol_sp k, SOURCE_INFO)
    : Base(name, k, SOURCE_INFO_PASS), _Methods(_Nil<T_O>()), _lambdaListHandler(_Nil<LambdaListHandler_O>()){};
  SingleDispatchGenericFunctionClosure_O(T_sp name)
    : Base(name), _Methods(_Nil<T_O>()), _lambdaListHandler(_Nil<LambdaListHandler_O>()){};
    void finishSetup(LambdaListHandler_sp llh, Symbol_sp k) {
      this->_lambdaListHandler = llh;
      this->kind = k;
    }
    T_sp lambda_list() const;
    void setf_lambda_list(List_sp lambda_list);
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
    CL_LISPIFY_NAME("SingleDispatchGenericFunction-methods");
    CL_DEFMETHOD   List_sp methods() const {
      return this->_Methods;
    };
    virtual List_sp declares() const {NOT_APPLICABLE();};
    virtual T_sp docstring() const {NOT_APPLICABLE();};

    Function_sp slowMethodLookup(Class_sp mc);
    Function_sp computeEffectiveMethodFunction(List_sp applicableMethodList);
  };

};

#if 0  
namespace core {
  FORWARD(SingleDispatchGenericFunctionClosure);
  class SingleDispatchGenericFunction_O : public Function_O {
    LISP_CLASS(core, CorePkg, SingleDispatchGenericFunction_O, "single-dispatch-generic-function",Function_O);
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
  }; // SingleDispatchGenericFunction class
}; // core namespace
#endif

#if 0
namespace core {
class SingleDispatchGenericFunctoid : public Closure_O {
private:
  SingleDispatchGenericFunction_sp _sdgf;
public:
  DISABLE_NEW();
  virtual const char *describe() const { return "SingleDispatchGenericFunctoid"; };
  LCC_VIRTUAL LCC_RETURN LISP_CALLING_CONVENTION() {
    INCREMENT_FUNCTION_CALL_COUNTER(this);
    ASSERT_LCC_VA_LIST_CLOSURE_DEFINED();
    IMPLEMENT_MEF(BF("Handle single dispatch"));
#if 0            
	    *lcc_resultP = this->_sdgf->INVOKE(lcc_nargs, nargs,args);
#endif
  }
};
};
#endif


namespace core {
 
class Lambda_emf : public FunctionClosure_O {
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
             SingleDispatchGenericFunctionClosure_sp gf,
             SingleDispatchMethod_sp cur_method);
  DISABLE_NEW();

  LCC_VIRTUAL LCC_RETURN LISP_CALLING_CONVENTION() {
    INCREMENT_FUNCTION_CALL_COUNTER(this);
    IMPLEMENT_ME();
#if 0
            // The closedEnv 
            ASSERTF(closedEnv.nilp(),BF("Since I don't pass the closedEnv forward it I expect that it should always be nil - this time it wasn't - figure out what is up with that"));
            return this->_method_function->INVOKE(nargs,args);
#endif
  }
};

T_sp core__ensure_single_dispatch_generic_function(Symbol_sp gfname, LambdaListHandler_sp llhandler);
};


#endif /* _SINGLEDISPATCHGENERICFUNCTION_H_ */
