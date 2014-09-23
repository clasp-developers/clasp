/*
    File: pythonCallback.cc
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
