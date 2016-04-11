/*
    File: singleDispatchMethod.cc
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
#include <clasp/core/common.h>
#include <clasp/core/str.h>
#include <clasp/core/environment.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/singleDispatchMethod.h>
#include <clasp/core/wrappers.h>

namespace core {

// ----------------------------------------------------------------------
//






SingleDispatchMethod_sp SingleDispatchMethod_O::create(Symbol_sp name,
                                                       Class_sp receiverClass,
                                                       LambdaListHandler_sp llh,
                                                       List_sp declares,
                                                       gc::Nilable<Str_sp> docstr,
                                                       Function_sp body) {
  GC_ALLOCATE(SingleDispatchMethod_O, method);
  method->_name = name;
  method->_receiver_class = receiverClass;
  ASSERTF(body.notnilp(), BF("The body of a method should never be nil"));
  if ( BuiltinClosure_sp bif = body.asOrNull<BuiltinClosure_O>() ) {
    method->_body = gctools::GC<CxxMethodFunction_O>::allocate(name,body);
  } else {
    method->_body = gctools::GC<SingleDispatchMethodFunction_O>::allocate(name,body);
  }
  method->_argument_handler = llh;
  method->_declares = declares;
  method->_docstring = docstr;
// method->_Function this is what we need to set up NOW.
// -- this function has to accept two arguments: (args next-emfun)
// So it's a chainable methoid, it can be called with a next-emfun argument
// which can be called by applying arguments to the local function "call-next-method"
#if 0
	CompiledBody_sp cb_method_function_primitive = CompiledBody_O::create(method_functoid,_Nil<T_O>());
	LambdaListHandler_sp llh_pass_arguments_through(_Nil<LambdaListHandler_O>());
	BuiltinClosure* method_functoid = gctools::ClassAllocator<Lambda_method_function>::allocate_class(name,method);
	method->_method_builtin = BuiltIn_O::make(name,llh_pass_arguments_through,cb_method_function_primitive);
#endif
  return method;
}

string SingleDispatchMethod_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->classNameAsString()
     << " :name " << _rep_(this->_name)
     << " :receiver-class " << _rep_(this->_receiver_class)
     << " :code " << _rep_(this->_body)
     //	   << " :method_builtin " << _rep_(this->_method_builtin)
     << " >";
  return ss.str();
}

}; /* core */
