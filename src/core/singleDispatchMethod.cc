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

//#define DEBUG_LEVEL_FULL
#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/array.h>
#include <clasp/core/environment.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/singleDispatchMethod.h>
#include <clasp/core/wrappers.h>

namespace core {

// ----------------------------------------------------------------------
//






SingleDispatchMethod_sp SingleDispatchMethod_O::create(T_sp name,
                                                       Instance_sp receiverClass,
                                                       LambdaListHandler_sp llh,
                                                       List_sp declares,
                                                       gc::Nilable<String_sp> docstr,
                                                       Function_sp body) {
  SingleDispatchMethodFunction_sp method_body;
  FunctionDescription* fdesc = makeFunctionDescription(name,llh->lambdaList(),docstr);
  if ( BuiltinClosure_sp bif = body.asOrNull<BuiltinClosure_O>() ) {
    method_body = gctools::GC<CxxMethodFunction_O>::allocate(fdesc,body);
  } else {
    method_body = gctools::GC<SingleDispatchMethodFunction_O>::allocate(fdesc,body);
  }
  GC_ALLOCATE_VARIADIC(SingleDispatchMethod_O, method, name,receiverClass,llh,declares,docstr,method_body);
  // method->_name = name;
  // method->_receiver_class = receiverClass;
  // ASSERTF(body.notnilp(), BF("The body of a method should never be nil"));
  // method->_argument_handler = llh;
  // method->_declares = declares;
  // method->_docstring = docstr;
  return method;
}

string SingleDispatchMethod_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString()
     << " :name " << _rep_(this->_name)
     << " :receiver-class " << _rep_(this->_receiver_class)
     << " :code " << _rep_(this->_body)
     //	   << " :method_builtin " << _rep_(this->_method_builtin)
     << " >";
  return ss.str();
}

}; /* core */
