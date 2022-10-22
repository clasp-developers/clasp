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

#include <clasp/core/object.h>
#include <clasp/core/singleDispatchMethod.fwd.h>


template <>
struct gctools::GCInfo<core::SingleDispatchMethod_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {
class SingleDispatchMethod_O : public General_O {
  LISP_CLASS(core, CorePkg, SingleDispatchMethod_O, "SingleDispatchMethod",General_O);
  //    DECLARE_ARCHIVE();
public:
  friend class SingleDispatchGeneralFunction_O;
public: // Simple default ctor/dtor
 SingleDispatchMethod_O(T_sp name, Instance_sp receiverClass, List_sp declares, T_sp docstring, Function_sp body) : _name(name), _receiver_class(receiverClass), _declares(declares), _docstring(docstring), _function(body) {};
 public:
           /*! Store the generic function name */
  T_sp _name;
  /*! Store the receiver class for this method */
  Instance_sp _receiver_class;
  //	BuiltIn_sp	_method_builtin;
  List_sp _declares;
  /*! Store the docstring */
  T_sp _docstring;
  /*! Store the body of the method */
  Function_sp       _function;
public: // creation function
  // The creates above are depreciated
  static SingleDispatchMethod_sp create(T_sp name,
                                        Instance_sp receiver,
                                        List_sp declares,
                                        gc::Nilable<String_sp> docstr,
                                        Function_sp body);

public: // Functions here
  Instance_sp receiver_class() const { return this->_receiver_class; };
  string __repr__() const;

CL_LISPIFY_NAME("singleDispatchMethodName");
CL_DEFMETHOD   T_sp singleDispatchMethodName() const { return this->_name; };
CL_LISPIFY_NAME("singleDispatchMethodReceiverClass");
CL_DEFMETHOD   Instance_sp singleDispatchMethodReceiverClass() const { return this->_receiver_class; };
 
//CL_LISPIFY_NAME("singleDispatchMethodCode");
//CL_DEFMETHOD   Function_sp singleDispatchMethodCode() const { return this->code; };
CL_LISPIFY_NAME("singleDispatchMethodDeclares");
CL_DEFMETHOD   List_sp singleDispatchMethodDeclares() const { return this->_declares; };
CL_LISPIFY_NAME("singleDispatchMethodDocstring");
CL_DEFMETHOD   T_sp singleDispatchMethodDocstring() const { return this->_docstring; };

}; // SingleDispatchMethod class

}; // core namespace

namespace core {
/*! A method function when invoked is given two arguments: (args next-emfun)
      It creates a FunctionValueEnvironment that defines call-next-method and next-method-p 
      with the method environment as its parent and then invokes the method-function
      with (args next-emfun) */
void core__ensure_single_dispatch_method(SingleDispatchGenericFunction_sp gfunction, T_sp gfname, Instance_sp receiver_class, List_sp declares, core::T_sp docstring, Function_sp body);


};

#endif /* _singleDispatchMethod_H_ */
