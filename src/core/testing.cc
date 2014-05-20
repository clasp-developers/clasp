#define	DEBUG_LEVEL_FULL

#include "core/common.h"
#include "core/corePackage.h"
#include "core/environment.h"
#include "core/fileSystem.h"
#include "core/bignum.h"
#include "core/character.h"
#include "core/executables.h"
#include "core/package.h"
#include "core/readtable.h"
#include "core/instance.h"
//#include "core/structureObject.h"
#include "core/bitVector.h"
#include "core/genericFunction.h"
#include "core/pointer.h"
#include "core/lispMath.h"
#include "core/symbolTable.h"
#include "core/null.h"
//#include "debugger.h"
#include "ql.h"
#include "str.h"
#include "numbers.h"
#include "evaluator.h"
#include "compiler.h"
#include "backquote.h"
#include "bformat.h"
#include "standardClass.h"
#include "forwardReferencedClass.h"
#include "singleDispatchMethod.h"
#include "singleDispatchGenericFunction.h"
#include "lambdaListHandler.h"
#include "designators.h"
#include "primitives.h"
#include "hashTable.h"
#include "hashTableEql.h"
#include "sysprop.h"
#include "multipleValues.h"
#include "lispStream.h"
#include "documentation.h"
#include "lispReader.h"
#include "designators.h"
#include "profile.h"
#include "core/wrappers.h"
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
