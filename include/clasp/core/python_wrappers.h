/*
    File: python_wrappers.h
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

#ifndef PYTHON_WRAPPERS_H
#define PYTHON_WRAPPERS_H

#ifdef USEBOOSTPYTHON
#include <boost/python.hpp>
#include <boost/python/detail/prefix.hpp>
#include <boost/python/tuple.hpp>
#include <boost/python/dict.hpp>
#include <boost/python/object/py_function.hpp>
#include <boost/mpl/vector/vector10.hpp>

#include <boost/limits.hpp>
#include <cstddef>
#include <clasp/core/array.h>
#include <clasp/core/activationFrame.h>
#include <clasp/core/evaluator.h>

namespace core {

extern List_sp python_convertArgumentsToCons(PyObject *args, PyObject *keywords, Lisp_sp lisp);
extern PyObject *python_convertObject(core::T_sp obj, Lisp_sp lisp);
};

namespace kw {
extern core::Symbol_sp& _sym_function;
};

namespace boost {
namespace python {
namespace detail {
template <class F>
struct wrapped_dispatcher {
  wrapped_dispatcher(F f, core::Lisp_sp l) : f(f), lisp(l) {}

  PyObject *operator()(PyObject *args, PyObject *keywords) {
    {
      _G();
      core::List_sp cargs = core::python_convertArgumentsToCons(args, keywords, _lisp);
      HARD_IMPLEMENT_MEF(BF("Handle new ActivationFrame/Environment stuff"));
#if 0
			    LOG(BF("dispatching function with arguments: %s") % cargs->__repr__() );
			    core::ValueFrame_sp frame(core::ValueFrame_O::create(cargs));
			    core::T_sp result = core::eval::apply_function(this->f,frame);
			    return core::python_convertObject(result,lisp);
#endif
    }
  }

private:
  F f;
  core::Lisp_sp lisp;
};

object BOOST_PYTHON_DECL make_raw_function(objects::py_function);
}

#if 0
	template <class F>
	    object def_raw(string const& packageName, string const& functionName, F f, const string& args, const string& docs,core::Lisp_sp lisp)
	{_G()
	    core::Symbol_sp funcSymbol = lisp->internWithPackageName(packageName,functionName);
	    core::FunctionPtr* func = new core::FunctionPtr(f);
	    core::List_sp ll = lisp_parse_arguments(lisp,packageName,args);
	    core::LambdaListHandler_sp llh = lisp_function_lambda_list_handler(_lisp,ll,_Nil<T_O>(), _Nil<T_O>());
#if 0
	    core::FunctionPrimitive_sp fp = core::FunctionPrimitive_O::create(funcSymbol,func,
									      llh,
									      docs,
									      kw::_sym_function,
									      _lisp);
#else
	    core::CompiledBody_sp cbfunc = core::CompiledBody_O::create(func,_lisp);
	    core::Function_sp fp = core::NamedFunction_O::create(funcSymbol,
							    llh,
							    _lisp->cnil(),
							    core::lisp_create_str(docs),
							    cbfunc,
							    _Nil<T_O>(),
							    kw::_sym_function,
							    _lisp);
#endif
	    object ofn = detail::make_raw_function(
		objects::py_function(
//		    detail::wrapped_dispatcher<core::FunctionPrimitive_sp>(fp,lisp)
		    detail::wrapped_dispatcher<core::Function_sp>(fp,lisp)
		    , mpl::vector1<PyObject*>()
		    , 0
		    , (std::numeric_limits<unsigned>::max)()
		    )
		);
	    boost::python::def(cstr(functionName),ofn);
	    return ofn;
	}
#endif
//
// This is used to wrap a Function_sp object as a python function
//
template <class F>
object def_executable(string const &packageName, string const &functionName, F fp, const string &args, const string &docs, core::Lisp_sp lisp) {
  core::Symbol_sp funcSymbol = lisp->internWithPackageName(packageName, functionName);
  object ofn = detail::make_raw_function(
      objects::py_function(
          detail::wrapped_dispatcher<core::Function_sp>(fp, lisp), mpl::vector1<PyObject *>(), 0, (std::numeric_limits<unsigned>::max)()));
  boost::python::def(functionName.c_str(), ofn);
  return ofn;
}

/*! This is used to wrap raw methods written in C++ with
	  translation of python function calls of arbitrary arity 
	  to lisp raw functions.
	  It's a bit tedious because you have
	  to specify the static_classSymbol of the class and the name of 
	  the function twice and the lisp interpreter.
	  This has to be this way because the method needs to know its symbol (does it????)
	  This is so that we can work with boost::python's .def function
	  example:
	  boost::python::class_<ObjectList_O>
	  .def("-name-",
	           raw_method<ObjectList_O>("-name-",
		                            &ObjectList::func,
					    _lisp,
					    "argumentList",  / * Optional default="" * /
					    "docstring"      / * Optional default="" * /
					    ))
	*/
}
} // namespace boost::python

#endif // USEBOOSTPYTHON
#endif //
