/*
    File: python_wrappers.cc
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

#include <Python.h>
#include <clasp/core/common.h>
#include <clasp/core/lisp.h>
#include <clasp/core/str.h>
#include <clasp/core/bignum.h>
#include <clasp/core/cons.h>
#include <clasp/core/wrappers.h>

#ifdef USEBOOSTPYTHON
namespace core
{

    extern T_sp python_convertObject( PyObject* value, core::Lisp_sp lisp);

	Cons_sp python_convertTupleToCons(PyObject* pargs,Lisp_sp lisp)
	{_G();
	    core::Cons_sp first = core::Cons_O::create(_Nil<T_O>(),_Nil<T_O>(),lisp);
	    if ( pargs!=NULL)
	    {
		core::Cons_sp cur = first;
		ASSERTF(PyTuple_Check(pargs),BF("Arguments aren't a tuple"));
		LOG(BF("Parsing arguments and keywords from python"));
		int tupleSize = PyTuple_Size(pargs);
		for ( int i=0; i<tupleSize; i++ )
		{
		    PyObject* value = PyTuple_GetItem(pargs,i);
#ifdef	DEBUG_ON
		    PyObject* repr = PyObject_Repr(value);
		    string srepr = PyString_AsString(repr);
		    Py_XDECREF(repr);
		    LOG(BF("Parsing one argument[%d] --> %s") % i % srepr );
#endif
		    T_sp obj = python_convertObject(value,lisp);
		    core::Cons_sp one = core::Cons_O::create(obj,lisp);
		    cur->setCdr(one);
		    cur = one;
		}
	    }
	    return first->cdr();
	}


	Cons_sp python_convertKeywordDictToAssocList(PyObject* pargs,Lisp_sp lisp)
	{_G();
	    core::Cons_sp first = core::Cons_O::create(_Nil<T_O>(),_Nil<T_O>(),lisp);
	    if ( pargs != NULL )
	    {
		LOG(BF("Parsing dict"));
		core::Cons_sp cur = first;
		ASSERTF(PyDict_Check(pargs),BF("Argument isn't a dict"));
		Py_ssize_t it = 0;
		PyObject* pyokey;
		PyObject* pyoval;
		while (PyDict_Next(pargs,&it,&pyokey,&pyoval)!=0)
		{
		    LOG(BF("Got pair"));
		    if ( pyokey == NULL ) continue;
		    if ( pyoval == NULL ) continue;
#ifdef	DEBUG_ON
		    PyObject* pyreprKey = PyObject_Repr(pyokey);
		    LOG(BF("Got repr key"));
		    PyObject* pyreprVal = PyObject_Repr(pyoval);
		    LOG(BF("Got repr val"));
		    LOG(BF("Parsing one dict item [%s -> %s]")
			% PyString_AsString(pyreprKey) % PyString_AsString(pyreprVal));
		    Py_XDECREF(pyreprKey);
		    Py_XDECREF(pyreprVal);
#endif
		    Symbol_sp keywordSymbol = lisp->internKeyword(PyString_AsString(pyokey));
		    LOG(BF("Got symbol[%s]") % keywordSymbol->__repr__() );
		    T_sp value = python_convertObject(pyoval,lisp);
		    LOG(BF("Got value[%s]") % value->__repr__() );
		    core::Cons_sp oneAssoc = core::Cons_O::create(keywordSymbol,value,lisp);
		    core::Cons_sp one = core::Cons_O::create(oneAssoc,lisp);
		    cur->setCdr(one);
		    cur = one;
		    LOG(BF("Created acons: %s") % one->__repr__());
		}
	    }
	    return first->cdr();
	}



	Cons_sp python_convertDictToAssocList(PyObject* pargs,Lisp_sp lisp)
	{_G();
	    core::Cons_sp first = core::Cons_O::create(_Nil<T_O>(),_Nil<T_O>(),lisp);
	    core::Cons_sp cur = first;
	    ASSERTF(PyDict_Check(pargs),BF("Argument isn't a dict"));
	    PyObject* itemList = PyDict_Items(pargs);
	    int listSize = PyList_Size(itemList);
	    for ( int i=0; i<listSize; i++ )
	    {
		PyObject* item = PyList_GetItem(pargs,i);
		PyObject* pyokey = PyTuple_GetItem(item,0);
		PyObject* pyoval = PyTuple_GetItem(item,1);
#ifdef	DEBUG_ON
		PyObject* pyreprKey = PyObject_Repr(pyokey);
		PyObject* pyreprVal = PyObject_Repr(pyoval);
		LOG(BF("Parsing one dict item [%s -> %s]")
		    % PyString_AsString(pyreprKey) % PyString_AsString(pyreprVal));
		Py_XDECREF(pyreprKey);
		Py_XDECREF(pyreprVal);
#endif
		T_sp key = python_convertObject(pyokey,lisp);
		T_sp value = python_convertObject(pyoval,lisp);
		core::Cons_sp oneAssoc = core::Cons_O::create(key,value,lisp);
		core::Cons_sp one = core::Cons_O::create(oneAssoc,lisp);
		cur->setCdr(one);
		cur = one;
	    }
	    Py_XDECREF(itemList);
	    return first->cdr();
	}




	Cons_sp python_convertListToCons(PyObject* pargs,Lisp_sp lisp)
	{_G();
	    core::Cons_sp first;
	    first = core::Cons_O::create(_Nil<T_O>(),_Nil<T_O>(),lisp);
	    core::Cons_sp cur = first;
	    ASSERTF(PyList_Check(pargs),BF("Arguments aren't a tuple"));
	    LOG(BF("Parsing arguments and keywords from python"));
	    int listSize = PyList_Size(pargs);
	    for ( int i=0; i<listSize; i++ )
	    {
		PyObject* value = PyList_GetItem(pargs,i);
#ifdef	DEBUG_ON
		PyObject* repr = PyObject_Repr(value);
		string srepr = PyString_AsString(repr);
		Py_XDECREF(repr);
		LOG(BF("Parsing one entry[%d] --> %s") % i % srepr );
#endif
		T_sp obj = python_convertObject(value,lisp);
		core::Cons_sp one = core::Cons_O::create(obj,lisp);
		cur->setCdr(one);
		cur = one;
	    }
	    return first->cdr();
	}

    T_sp python_convertObject( PyObject* value, core::Lisp_sp lisp)
    {_G();
	T_sp result = _Nil<T_O>();
	if (PyString_Check(value)) {
	    result = core::Str_O::create(PyString_AsString(value));
	}
	else if (value == Py_None) {
	    result = _Nil<T_O>();
	}
	else if (PyBool_Check(value)) {
	    if ( value == Py_True ) result = lisp->_true();
	    else result = _Nil<T_O>();
	}
	else if (PyInt_Check(value)) {
	    result = make_fixnum((LongLongInt)PyInt_AsLong(value));
	    LOG(BF("Argument --> Fixnum"));
	}
	else if (PyLong_Check(value)) {
	    int overflow = 0;
	    LongLongInt lli = PyLong_AsLongLongAndOverflow(value,&overflow);
	    result = Bignum_O::create(lli);
	    LOG(BF("Argument --> Bignum"));
	}
	else if (PyFloat_Check(value)) {
	    result = DoubleFloat_O::create(PyFloat_AsDouble(value));
	    LOG(BF("Argument --> Float"));
	}
	else if (PyTuple_Check(value)) {
	    result = python_convertTupleToCons(value,lisp);
	    LOG(BF("Argument --> tuple"));
	}
	else if (PyList_Check(value)) {
	    result = python_convertListToCons(value,lisp);
	    LOG(BF("Argument --> list"));
	}
	else if (PyDict_Check(value)) {
	    result = python_convertDictToAssocList(value,lisp);
	    LOG(BF("Argument --> dict"));
	}
	else {
	    result = boost::python::extract<gctools::smart_ptr<T_O> >(value);
	    LOG(BF("Extracted the object[%s]") % result->__repr__() );
	}
	return result;
    }


    core::Cons_sp python_convertArgumentsToCons(PyObject* argTuple, PyObject* keywordDict, Lisp_sp lisp)
    {_G();
	core::Cons_sp fixedArgs = python_convertTupleToCons(argTuple,lisp);
	core::Cons_sp keywordAList = python_convertKeywordDictToAssocList(keywordDict,lisp);
	core::Cons_sp first = core::Cons_O::create(_Nil<T_O>(),lisp);
	first->setCdr(fixedArgs);
	core::List_sp cur = first->last();
	for ( Cons_sp acur = keywordAList; acur.notnilp(); acur = cCdr(acur))
	{
	    core::Cons_sp assoc = acur->ocar().as<core::Cons_O>();
	    core::Symbol_sp keyword = assoc->ocar().as<core::Symbol_O>();
	    T_sp value = assoc->ocdr();
	    core::Cons_sp oneKeyword = core::Cons_O::create(keyword,lisp);
	    cur->setCdr(oneKeyword);
	    cur = oneKeyword;
	    core::Cons_sp oneValue = core::Cons_O::create(value,lisp);
	    cur->setCdr(oneValue);
	    cur = oneValue;
	}
	return first->cdr();
    }



    PyObject* python_convertObject(T_sp obj,Lisp_sp lisp)
    {_G();
	if ( obj == lisp->_true() )
	{
	    Py_RETURN_TRUE;
	} else if ( obj.nilp() )
	{
	    Py_INCREF(Py_None);
	    return Py_None;
	} else if ( obj.isA<Str_O>() )
	{
//	    printf("Returning a string value[%s]\n", obj.as<Str_O>()->get().c_str() );
	    return Py_BuildValue("s",obj.as<Str_O>()->get().c_str());
	} else if ( obj.isA<Fixnum_O>() )
	{
	    return PyInt_FromLong(obj.as<Fixnum_O>()->get());
	} else if ( obj.isA<Bignum_O>())
	{
	    Bignum_sp bn = obj.as<Bignum_O>();
	    if (bn->as_mpz().fits_sint_p())
	    {
		return PyInt_FromLong(bn->as_mpz().get_si());
	    }
	    SIMPLE_ERROR(BF("Cannot convert Bignum %s to python int")%bn->valueAsString() );
	} else if ( obj.isA<DoubleFloat_O>())
	{
	    return PyFloat_FromDouble(obj.as<DoubleFloat_O>()->get());
	}
	boost::python::object o(obj);
	PyObject* res = o.ptr();
	Py_IncRef(res);
	return res;
    }
};
#endif // USEBOOSTPYTHON
