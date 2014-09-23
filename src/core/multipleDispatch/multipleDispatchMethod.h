/*
    File: multipleDispatchMethod.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See file 'clasp/Copyright' for full details.
 
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
#ifndef _multipleDispatchMethod_H_
#define _multipleDispatchMethod_H_

#include "core/foundation.h"
#include "core/object.h"
#include "core/multipleDispatchMethod.fwd.h"


namespace core
{
    class MultipleDispatchMethod_O : public T_O
    {
	LISP_BASE1(T_O);
	LISP_CLASS(CorePkg,MultipleDispatchMethod_O,"MultipleDispatchMethod");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public:
	friend class MultipleDispatchMethodPrimitive_O;
	friend class MultipleDispatchGeneralFunction_O;
	friend class Lambda_emf;
	friend class Lambda_method_function;
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(MultipleDispatchMethod_O);
    public:
	void initialize();
	
    private: // instance variables here
		/*! Store the generic function name */
	Symbol_sp	_name;
	/*! Store the receiver class for this method */
	MetaClass_sp	_receiver_class;
	/*! Store the method environment */
	Environment_sp	_environment;
	/*! Store the body of the method */
	Cons_sp		_body;
	/*! Store the computed function for this method - this
	  takes two arguments: (args next-emfun)
	  (args) is the list of arguments passed to the method (first argument is the receiver)
	  (next-emfun) is the next effective method function called with call-next-method.
	*/
	Function_sp	_method_function;
	LambdaListHandler_sp	_argument_handler;
	Cons_sp 	_declares;
	/*! Store the docstring */
	Str_sp		_docstring;
	/*! Store whether the method can be redefined */
	bool		_can_be_redefined;

    public: // creation function
	// The creates above are depreciated
	static MultipleDispatchMethod_sp create(Symbol_sp name, MetaClass_sp receiver, LambdaListHandler_sp lambda_list_handler, Cons_sp declares, Str_sp docstr, Cons_sp body, Lisp_sp lisp);


    public: // Functions here

	MetaClass_sp receiver_class() const { return this->_receiver_class; };
	bool can_be_redefined() const { return this->_can_be_redefined;};

	string __repr__() const;


    }; // MultipleDispatchMethod class
    
}; // core namespace
TRANSLATE(core::MultipleDispatchMethod_O);




#endif /* _multipleDispatchMethod_H_ */
