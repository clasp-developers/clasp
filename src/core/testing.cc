/*
    File: testing.cc
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

#include <clasp/core/common.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/environment.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/bignum.h>
#include <clasp/core/character.h>
#include <clasp/core/executables.h>
#include <clasp/core/package.h>
#include <clasp/core/readtable.h>
#include <clasp/core/instance.h>
//#include "core/structureObject.h"
#include <clasp/core/bitVector.h>
#include <clasp/core/genericFunction.h>
#include <clasp/core/pointer.h>
#include <clasp/core/lispMath.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/null.h>
//#include "debugger.h"
#include <clasp/core/ql.h>
#include <clasp/core/str.h>
#include <clasp/core/numbers.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/compiler.h>
#include <clasp/core/backquote.h>
#include <clasp/core/bformat.h>
#include <clasp/core/standardClass.h>
#include <clasp/core/forwardReferencedClass.h>
#include <clasp/core/singleDispatchMethod.h>
#include <clasp/core/singleDispatchGenericFunction.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/designators.h>
#include <clasp/core/primitives.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/hashTableEql.h>
#include <clasp/core/sysprop.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/documentation.h>
#include <clasp/core/lispReader.h>
#include <clasp/core/designators.h>
#include <clasp/core/profile.h>
#include <clasp/core/wrappers.h>
namespace core
{

    
    
#define ARGS_af_isString "(arg)"
#define DECL_af_isString ""
#define DOCS_af_isString "isString"
    void af_isString(T_sp arg)
    {_G();
	if ( Str_sp s = arg.asOrNull<Str_O>() )
	{
	    printf("The object |%s| is a string\n", s->get().c_str() );
	} else
	{
	    printf("The object is not a string\n" );
	}
    };


    
#define ARGS_af_isArray "(arg)"
#define DECL_af_isArray ""
#define DOCS_af_isArray "isArray"
    void af_isArray(T_sp arg)
    {_G();
	if ( Array_sp s = arg.asOrNull<Array_O>() )
	{
	    printf("The object |%s| is an array\n", _rep_(s).c_str() );
	} else
	{
	    printf("The object is not an array\n" );
	}
    };


    
    
    
#define ARGS_af_testVal "(arg)"
#define DECL_af_testVal ""
#define DOCS_af_testVal "testVal"
    T_sp af_testVal(T_sp v)
    {_G();
	if ( Fixnum_sp fn = v.asOrNull<Fixnum_O>() ) {
	    return Str_O::create("val is fixnum");
	} else if ( Symbol_sp sym = v.asOrNull<Symbol_O>() ) {
	    return Str_O::create("arg is symbol");
	}
	return Str_O::create("arg didn't match");
    };

    


    void initialize_testing()
    {
	SYMBOL_EXPORT_SC_(CorePkg,isString);
	Defun(isString);
	SYMBOL_EXPORT_SC_(CorePkg,isArray);
	Defun(isArray);
	SYMBOL_EXPORT_SC_(CorePkg,testVal);
	Defun(testVal);
    }


}; /* core */
