       
//
// (C) 2004 Christian E. Schafmeister
//



#include "foundation.h"

#include "pythonCallback.h"

using namespace core;


#ifdef	USEBOOSTPYTHON
void	Dumb_PythonCallback::setCallback( PyObject* callback ) {
    LOG(BF("Dumb_PythonCallback::setCallback setting callback to 0x%08x") % (callback ) );
    this->callbackFn = callback;
}
#endif

void	Dumb_PythonCallback::callCallback()
{

    LOG(BF("Dumb_PythonCallback::callCallback") );
#ifdef	USEBOOSTPYTHON
PyObject	*arglist;
PyObject	*result;
long		res;
    if ( this->callbackFn != NULL ) {
        LOG(BF("Dumb_PythonCallback::callCallback setting up argument list") );
		    // Time to call the callback
	arglist = Py_BuildValue("(d,d,i,i)",
				this->doubleVal0, this->doubleVal1,
				this->longVal0, this->longVal1 );
        LOG(BF("Dumb_PythonCallback::callCallback calling callback") );
	result = PyEval_CallObject(this->callbackFn, arglist);
	Py_DECREF(arglist);
	if (result == NULL) {
	    LOG(BF("Dumb_PythonCallback::callCallback returned NULL") );
	    this->longReturn = 0;
	    return;
	}
	res = PyInt_AsLong(result);
	LOG(BF("Dumb_PythonCallback::callCallback returned res=%d") % (res ) );
	Py_DECREF(result);
	this->longReturn = res;
    }
#else
    this->longReturn = 0;
#endif
}


