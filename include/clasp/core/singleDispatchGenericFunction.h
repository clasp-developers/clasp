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

#include <clasp/core/hashTable.fwd.h>
#include <clasp/core/singleDispatchGenericFunction.fwd.h>
#include <clasp/core/singleDispatchMethod.fwd.h>


namespace core {

  FORWARD(SingleDispatchCxxEffectiveMethodFunction);  
  class SingleDispatchCxxEffectiveMethodFunction_O : public Closure_O {
    LISP_CLASS(core,CorePkg,SingleDispatchCxxEffectiveMethodFunction_O,"SingleDispatchCxxEffectiveMethodFunction",Closure_O);
  public:
    const char *describe() const { return "SingleDispatchCxxEffectiveMethodFunction"; };
  public:
    CxxMethodFunction_sp _onlyCxxMethodFunction;
  public:
    T_sp lambda_list() const { return _Nil<T_O>();};
  SingleDispatchCxxEffectiveMethodFunction_O(FunctionDescription* fdesc, CxxMethodFunction_sp mf) : Base(SingleDispatchCxxEffectiveMethodFunction_O::entry_point,fdesc), _onlyCxxMethodFunction(mf) {};
    static LCC_RETURN LISP_CALLING_CONVENTION();
  };

  FORWARD(SingleDispatchEffectiveMethodFunction);  
  class SingleDispatchEffectiveMethodFunction_O : public Closure_O {
    LISP_CLASS(core,CorePkg,SingleDispatchEffectiveMethodFunction_O,"SingleDispatchEffectiveMethodFunction",Closure_O);
  public:
    const char *describe() const { return "SingleDispatchEffectiveMethodFunction"; };
  public:
    List_sp _Befores;
    List_sp _Primaries;
    List_sp _Afters;
  public:
    T_sp lambda_list() const { return _Nil<T_O>();};
  SingleDispatchEffectiveMethodFunction_O(FunctionDescription* fdesc, List_sp befores, List_sp primaries, List_sp afters) : Base(entry_point,fdesc), _Befores(befores),_Primaries(primaries),_Afters(afters) {};
    static LCC_RETURN LISP_CALLING_CONVENTION();
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
  class SingleDispatchGenericFunctionClosure_O : public Closure_O {
    LISP_CLASS(core,CorePkg,SingleDispatchGenericFunctionClosure_O,"SingleDispatchGenericFunctionClosure",Closure_O);
  public:
  /*! Store the methods here */
    List_sp _Methods;
    LambdaListHandler_sp _lambdaListHandler;
    size_t _SingleDispatchArgumentIndex;
  /*! Store the method functions hashed on the receiver class */
  //	HashTable_sp	classesToClosures;
  public:
    static SingleDispatchGenericFunctionClosure_sp create(T_sp functionName, LambdaListHandler_sp llhandler, size_t singleDispatchArgumentIndex);
public:
  SingleDispatchGenericFunctionClosure_O(FunctionDescription* fdesc, size_t sdai)
    : Base(entry_point,fdesc), _Methods(_Nil<T_O>()), _lambdaListHandler(_Unbound<LambdaListHandler_O>()), _SingleDispatchArgumentIndex(sdai) {};
    T_sp lambdaList() const;
    void finishSetup(LambdaListHandler_sp llh) {
      this->_lambdaListHandler = llh;
    }
    virtual size_t templatedSizeof() const { return sizeof(*this); };
    virtual const char *describe() const { return "SingleDispatchGenericFunctionClosure"; };
    static LCC_RETURN LISP_CALLING_CONVENTION();
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
    Function_sp slowMethodLookup(Instance_sp mc);
    Function_sp computeEffectiveMethodFunction(List_sp applicableMethodList);
  };

};


namespace core {
 
class Lambda_emf : public Closure_O {
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

  LCC_VIRTUAL LCC_RETURN LISP_CALLING_CONVENTION() {
    INCREMENT_FUNCTION_CALL_COUNTER(this);
    HARD_IMPLEMENT_ME();
#if 0
            // The closedEnv 
            ASSERTF(closedEnv.nilp(),BF("Since I don't pass the closedEnv forward it I expect that it should always be nil - this time it wasn't - figure out what is up with that"));
            return this->_method_function->INVOKE(nargs,args);
#endif
  }
};

 SingleDispatchGenericFunctionClosure_sp core__ensure_single_dispatch_generic_function(T_sp gfname, LambdaListHandler_sp llhandler, bool autoExport, size_t singleDispatchArgumentIndex);
};


#endif /* _SINGLEDISPATCHGENERICFUNCTION_H_ */
