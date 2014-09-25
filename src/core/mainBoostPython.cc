/*
    File: mainBoostPython.cc
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
#define	DEBUG_LEVEL_FULL
//
// (C) 2004 Christian E. Schafmeister
//



#include "foundation.h"
#include <iostream>
#include "object.h"
#include "lisp.h"


#ifdef	USEBOOSTPYTHON

namespace core {



void	cPrint(string s)
{
char	*cSrc;
char	*cSrcArray;
char	*cDestArray;
char	*cDest;
int	destSize;
    cSrcArray = (char*)malloc(s.size()+1);
    destSize = s.size()+2048;
    cDestArray = (char*)malloc(destSize);
    strncpy( cSrcArray, s.c_str(), s.size() );
    cSrcArray[s.size()] = '\0';
    cSrc = cSrcArray;
    cDest = cDestArray;
    while ( *cSrc != '\0' ) {
	switch (*cSrc) {
	    case '<':
		if ( cDest-cDestArray > destSize-3 ) {
		    SIMPLE_ERROR(BF("cPrint OVERFLOW"));
		}
		*cDest++ = '&';
		*cDest++ = 'l';
		*cDest++ = 't';
		break;
	    case '>':
		if ( cDest-cDestArray > destSize-3 ) {
		    SIMPLE_ERROR(BF("cPrint OVERFLOW"));
		}
		*cDest++ = '&';
		*cDest++ = 'g';
		*cDest++ = 't';
		break;
	    case '&':
		if ( cDest-cDestArray > destSize-4 ) {
		    SIMPLE_ERROR(BF("cPrint OVERFLOW"));
		}
		*cDest++ = '&';
		*cDest++ = 'a';
		*cDest++ = 'm';
		*cDest++ = 'p';
		break;
	    case '"':
		if ( cDest-cDestArray > destSize-5 ) {
		    SIMPLE_ERROR(BF("cPrint OVERFLOW"));
		}
		*cDest++ = '&';
		*cDest++ = 'q';
		*cDest++ = 'u';
		*cDest++ = 'o';
		*cDest++ = 't';
		break;
	    case '\'':
		if ( cDest-cDestArray > destSize-4 ) {
		    SIMPLE_ERROR(BF("cPrint OVERFLOW"));
		}
		*cDest++ = '&';
		*cDest++ = '#';
		*cDest++ = '3';
		*cDest++ = '9';
		break;
	    default:
		*cDest++ = *cSrc;
	}
	cSrc++;
    }
    *cDest = '\0';
    debugLog().write(cDestArray);
    free(cSrcArray);
    free(cDestArray);
}


void	cDebugOpen(string s)
{
    debugLog().write("<DBG>");
    debugLog().write(s);
}

void	cDebugClose()
{
    debugLog().write("</DBG>\n");
}


bool	boost_debugOn()
{
    return !debugLog().getSuppressMessages();
}


void	boost_FunctionTraceOpen(string func,string file, int ln)
{
stringstream	serr;
    serr << "<PYFUNC";
    serr << " l=\"" << ln << "\" i=\"" << func << "\" f=\"" << file << "\">{"<<std::endl;
    debugLog().write(serr.str());
}

void	boost_FunctionTraceClose()
{
    debugLog().write("}</PYFUNC>\n");
}

void	boost_BlockTraceOpen(string func,string file, int ln)
{
stringstream	serr;
    serr << "<PYBLOCK";
    serr << " l=\"" << ln << "\" i=\"" << func << "\" f=\"" << file << "\">{"<<std::endl;
    debugLog().write(serr.str());
}

void	boost_BlockTraceClose()
{
    debugLog().write("}</PYBLOCK>\n");
}


void	boost_VP0(string s,string fn, int ln)
{
stringstream	ss;
    ss.str("");
    ss << "*" << fn << "_" << ln << " " << s;
    cDebugOpen(ss.str());
    cDebugClose();
}

void	errPrint(string s)
{
    debugLog().write(s);
}


void	dbgPython( const string& ns, const string& code )
{
stringstream ss;
string	nodeName = "Python";
    ss.str("");
    ss << "<" << nodeName << " ";
    ss << "_namespace=\"" <<ns<<"\" ";
    ss << "_p=\"" << debugLog().nextPosition() << "\"";
    ss << ">" << code << "</"<<nodeName<<">"<<std::endl;
    debugLog().write(ss.str());
}



void	dbgPythonAssign(const string& ns, const string& var, T_sp obj)
{
stringstream ss;
string nodeName="PythonAssign";
    ss << "_namespace=\"" <<ns<<"\" ";
    ss << "_p=\"" <<debugLog().nextPosition()<<"\" ";
    ss << "_variable=\"" <<var<<"\"";
    debugLog().write(obj->asXmlStringWrap(nodeName,ss.str()));
    debugLog().write("\n");
}


Lisp_sp	environment()
{_errorF();
    static Lisp_sp _BoostPythonLispObject;
    if ( _BoostPythonLispObject.pointerp() )
    {
	LOG(BF("Defining Lisp environment@%X") % &_BoostPythonLispObject );
	_BoostPythonLispObject = Lisp_O::create();
	ASSERTNOTNULL(_BoostPythonLispObject);
	LOG(BF("Lisp object@%X") % _BoostPythonLispObject.get() );
    }

    LOG(BF("Looking up Lisp environment@%X") % &_BoostPythonLispObject );
    return _BoostPythonLispObject;
}

};


using namespace core;

__INITIALIZE_PYTHON(InitPython_BoostStuff);
void InitPython_BoostStuff() 
{_G();
    boost::python::def("environment",environment);
    boost::python::def("cPrint",cPrint);
    boost::python::def("dbgPython",&dbgPython);
    boost::python::def("dbgPythonAssign",&dbgPythonAssign);
    boost::python::def("errPrint",errPrint);
    boost::python::def("boost_VP0",boost_VP0);
    boost::python::def("boost_debugOn",boost_debugOn);
    boost::python::def("cDebugOpen",cDebugOpen);
    boost::python::def("cDebugClose",cDebugClose);
    boost::python::def("boost_FunctionTraceOpen",boost_FunctionTraceOpen);
    boost::python::def("boost_FunctionTraceClose",boost_FunctionTraceClose);
    boost::python::def("boost_BlockTraceOpen",boost_BlockTraceOpen);
    boost::python::def("boost_BlockTraceClose",boost_BlockTraceClose);
}







BOOST_PYTHON_MODULE(initmbbCxx)
{_G();
     
    ios::sync_with_stdio();
#ifdef	EMBEDED_IN_PYTHON
    printf("EMBEDED IN PYTHON\n");
#endif
  
#ifdef	_DEBUG
    printf( "Turning on memory overwrite checking\n" );
    ASSERT(_CrtCheckMemory());
    _crtDbgFlag |= _CRTDBG_CHECK_ALWAYS_DF;
#endif

}


#endif
