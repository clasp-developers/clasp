/*
    File: singleDispatchMethod.h
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
#ifndef _singleDispatchMethod_H_
#define _singleDispatchMethod_H_

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/singleDispatchMethod.fwd.h>

namespace core {
class SingleDispatchMethod_O : public T_O {
  friend class SingleDispatchGenericFunctionClosure;
  LISP_BASE1(T_O);
  LISP_CLASS(core, CorePkg, SingleDispatchMethod_O, "SingleDispatchMethod");
  DECLARE_INIT();
  //    DECLARE_ARCHIVE();
public:
  friend class SingleDispatchMethodPrimitive_O;
  friend class SingleDispatchGeneralFunction_O;
  friend class Lambda_emf;
  friend class Lambda_method_function;

public: // Simple default ctor/dtor
  DEFAULT_CTOR_DTOR(SingleDispatchMethod_O);
GCPRIVATE: // instance variables here
           /*! Store the generic function name */
  Symbol_sp _name;
  /*! Store the receiver class for this method */
  Class_sp _receiver_class;
  /*! Store the body of the method */
  Function_sp code;
  //        CompiledBody_sp		_body;
  //	BuiltIn_sp	_method_builtin;
  /*! This is the LambdaListHandler for the Builtin method */
  LambdaListHandler_sp _argument_handler;
  List_sp _declares;
  /*! Store the docstring */
  T_sp _docstring;

public: // creation function
  // The creates above are depreciated
  static SingleDispatchMethod_sp create(Symbol_sp name,
                                        Class_sp receiver,
                                        LambdaListHandler_sp lambda_list_handler,
                                        List_sp declares, gc::Nilable<Str_sp> docstr,
                                        Function_sp body);

public: // Functions here
  Class_sp receiver_class() const { return this->_receiver_class; };
  LambdaListHandler_sp method_lambda_list_handler() const { return this->_argument_handler; };
  string __repr__() const;

  Symbol_sp singleDispatchMethodName() const { return this->_name; };
  Class_sp singleDispatchMethodReceiverClass() const { return this->_receiver_class; };
  Function_sp singleDispatchMethodCode() const { return this->code; };
  LambdaListHandler_sp singleDispatchMethodLambdaListHandler() const { return this->_argument_handler; };
  List_sp singleDispatchMethodDeclares() const { return this->_declares; };
  T_sp singleDispatchMethodDocstring() const { return this->_docstring; };

}; // SingleDispatchMethod class

}; // core namespace
template <>
struct gctools::GCInfo<core::SingleDispatchMethod_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};
TRANSLATE(core::SingleDispatchMethod_O);

namespace core {

/*! A method function when invoked is given two arguments: (args next-emfun)
      It creates a FunctionValueEnvironment that defines call-next-method and next-method-p 
      with the method environment as its parent and then invokes the method-function
      with (args next-emfun) */
class Lambda_method_function : public BuiltinClosure {
  FRIEND_GC_SCANNER(core::Lambda_method_function);

private:
  SingleDispatchMethod_sp _method;
  Function_sp _temporary_function;

public:
  const char *describe() const { return "Lambda_method_function"; };

public:
  Lambda_method_function(T_sp name, SingleDispatchMethod_sp method)
      : BuiltinClosure(name) {
    _G();
    this->_method = method;
    this->_temporary_function = _Nil<Function_O>();
  }

  DISABLE_NEW();
  virtual size_t templatedSizeof() const { return sizeof(*this); };
  bool requires_activation_frame() const { return true; };

  /*! The argument list is: (args next-emfun)
	  Use next-emfun to set up a FunctionValueEnvironment that defines call-next-method and next-method-p */
  void LISP_INVOKE();
};
};

#endif /* _singleDispatchMethod_H_ */
