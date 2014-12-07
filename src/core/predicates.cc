/*
    File: predicates.cc
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

#include "core/foundation.h"
#include "core/object.h"
#include "core/lispList.h"
#include "core/metaClass.h"
#include "core/package.h"
#include "core/bignum.h"
#include "core/closPackage.h"
#include "core/bitVector.h"
#include "core/pathname.h"
#include "core/hashTable.h"
#include "core/vectorObjects.h"
//#ifndef CLOS
#include "core/structureObject.h"
//#else
#include "core/instance.h"
//#endif
#include "core/readtable.h"
#include "core/lambdaListHandler.h"
#include "core/singleDispatchGenericFunction.h"
#include "core/numbers.h"
#include "core/externalObject.h"
#include "core/lispStream.h"
#include "core/fileSystem.h"
#include "core/str.h"
#include "core/wrappers.h"


namespace core
{


    
    
#define ARGS_af_atom "(arg)"
#define DECL_af_atom ""
#define DOCS_af_atom "atom"
    bool af_atom(T_sp obj)
    {_G();
	if (obj.nilp()) return true;
	if (Cons_sp c = obj.asOrNull<Cons_O>() )
	{
	    return false;
	}
	return true;
    };


        
    
#define ARGS_core_baseCharP "(arg)"
#define DECL_core_baseCharP ""
#define DOCS_core_baseCharP "endp"
    bool core_baseCharP(T_sp arg)
    {_G();
	if ( arg.nilp() ) return false;
	if ( Character_sp c = arg.asOrNull<Character_O>() ) {
	    return true;
	}
	return false;
    };

    
    
#define ARGS_af_endp "(arg)"
#define DECL_af_endp ""
#define DOCS_af_endp "endp"
    bool af_endp(T_sp arg)
    {_G();
	return arg.nilp();
    };


#define ARGS_af_listp "(arg)"
#define DECL_af_listp ""
#define DOCS_af_listp "listp"
    bool af_listp(T_sp arg)
    {_G();
	if ( arg.nilp() ) return true;
        return arg.isA<Cons_O>();
    };



    
    
#define ARGS_af_bignumP "(arg)"
#define DECL_af_bignumP ""
#define DOCS_af_bignumP "bignumP"
    bool af_bignumP(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	return obj.isA<Bignum_O>();
    };

#define ARGS_af_fixnumP "(arg)"
#define DECL_af_fixnumP ""
#define DOCS_af_fixnumP "fixnumP"
    bool af_fixnumP(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	if ( obj.pointerp() ) {
	    if ( Fixnum_sp fn = obj.asOrNull<Fixnum_O>() ) {
		return true;
	    }
	    return false;
	}
	return false;
    }




#define ARGS_af_stringP "(arg)"
#define DECL_af_stringP ""
#define DOCS_af_stringP "stringP"
    bool af_stringP(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	return obj.isA<String_O>();
    };

#define ARGS_af_strP "(arg)"
#define DECL_af_strP ""
#define DOCS_af_strP "strP"
    bool af_strP(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	return obj.isA<Str_O>();
    };



#define ARGS_af_doubleFloatP "(arg)"
#define DECL_af_doubleFloatP ""
#define DOCS_af_doubleFloatP "doubleFloatP"
    bool af_doubleFloatP(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	return obj.isA<DoubleFloat_O>();
    };


#define ARGS_af_functionP "(arg)"
#define DECL_af_functionP ""
#define DOCS_af_functionP "functionP"
    bool af_functionP(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	return obj.isA<Function_O>();
    };




    
    
#define ARGS_af_interpretedFunctionP "(arg)"
#define DECL_af_interpretedFunctionP ""
#define DOCS_af_interpretedFunctionP "interpretedFunctionP"
    bool af_interpretedFunctionP(T_sp arg)
    {_G();
	if ( arg.nilp() ) return false;
        if ( arg.isA<Function_O>() ) {
            if ( dynamic_cast<InterpretedClosure*>(arg.as<Function_O>()->closure) ) {
                return true;
            }
        }
        return false;
    };


#define ARGS_af_consP "(arg)"
#define DECL_af_consP ""
#define DOCS_af_consP "consP"
    bool af_consP(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	return obj.isA<Cons_O>();
    };

#define ARGS_af_symbolp "(arg)"
#define DECL_af_symbolp ""
#define DOCS_af_symbolp "symbolP"
    bool af_symbolp(T_sp obj)
    {_G();
	if (obj.nilp()) return true; // nil is a symbol
	return obj.isA<Symbol_O>();
    };


#define ARGS_cl_packagep "(arg)"
#define DECL_cl_packagep ""
#define DOCS_cl_packagep "See CLHS packagep"
    bool cl_packagep(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	return obj.isA<Package_O>();
    };

#define ARGS_af_classp "(arg)"
#define DECL_af_classp ""
#define DOCS_af_classp "classp"
    bool af_classp(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	return obj.isA<Class_O>();
    };


#define ARGS_af_lambda_list_handler_p "(arg)"
#define DECL_af_lambda_list_handler_p ""
#define DOCS_af_lambda_list_handler_p "lambda_list_handler_p"
    bool af_lambda_list_handler_p(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	return obj.isA<LambdaListHandler_O>();
    };



#define ARGS_af_numberP "(arg)"
#define DECL_af_numberP ""
#define DOCS_af_numberP "numberP"
    bool af_numberP(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	return obj.isA<Number_O>();
    };

#define ARGS_af_complexP "(arg)"
#define DECL_af_complexP ""
#define DOCS_af_complexP "complexP"
    bool af_complexP(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	return obj.isA<Complex_O>();
    };

#define ARGS_af_ratioP "(arg)"
#define DECL_af_ratioP ""
#define DOCS_af_ratioP "ratioP"
    bool af_ratioP(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	return obj.isA<Ratio_O>();
    };


#define ARGS_af_rationalP "(arg)"
#define DECL_af_rationalP ""
#define DOCS_af_rationalP "rationalP"
    bool af_rationalP(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	return obj.isA<Rational_O>();
    };

#ifdef CLASP_LONG_FLOAT
#define ARGS_af_longFloatP "(arg)"
#define DECL_af_longFloatP ""
#define DOCS_af_longFloatP "longFloatP"
    bool af_longFloatP(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	return obj.isA<LongFloat_O>();
    };
#endif

#define ARGS_af_shortFloatP "(arg)"
#define DECL_af_shortFloatP ""
#define DOCS_af_shortFloatP "shortFloatP"
    bool af_shortFloatP(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	return obj.isA<ShortFloat_O>();
    };


#define ARGS_af_singleFloatP "(arg)"
#define DECL_af_singleFloatP ""
#define DOCS_af_singleFloatP "singleFloatP"
    bool af_singleFloatP(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	return obj.isA<SingleFloat_O>();
    };


#define ARGS_af_realP "(arg)"
#define DECL_af_realP ""
#define DOCS_af_realP "realP"
    bool af_realP(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	return obj.isA<Real_O>();
    };


#define ARGS_af_floatP "(arg)"
#define DECL_af_floatP ""
#define DOCS_af_floatP "floatP"
    bool af_floatP(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	return obj.isA<Float_O>();
    };


#define ARGS_af_characterP "(arg)"
#define DECL_af_characterP ""
#define DOCS_af_characterP "characterP"
    bool af_characterP(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	return obj.isA<Character_O>();
    };

#define ARGS_af_vectorP "(arg)"
#define DECL_af_vectorP ""
#define DOCS_af_vectorP "vectorP"
    bool af_vectorP(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	return obj.isA<Vector_O>();
    };

#define ARGS_af_integerP "(arg)"
#define DECL_af_integerP ""
#define DOCS_af_integerP "integerP"
    bool af_integerP(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	return obj.isA<Integer_O>();
    };

#define ARGS_af_keywordP "(arg)"
#define DECL_af_keywordP ""
#define DOCS_af_keywordP "keywordP"
    bool af_keywordP(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	if ( Symbol_sp s = obj.asOrNull<Symbol_O>() )
	{
	    return s->isKeywordSymbol();
	}
	return false;
    };



#define ARGS_af_pathP "(arg)"
#define DECL_af_pathP ""
#define DOCS_af_pathP "pathP"
    bool af_pathP(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	return obj.isA<Path_O>();
    };


#define ARGS_af_bitVectorP "(arg)"
#define DECL_af_bitVectorP ""
#define DOCS_af_bitVectorP "bitVectorP"
    bool af_bitVectorP(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	return obj.isA<BitVector_O>();
    };


#define ARGS_af_hashTableP "(arg)"
#define DECL_af_hashTableP ""
#define DOCS_af_hashTableP "hashTableP"
    bool af_hashTableP(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
        if ( HashTable_sp ht = obj.asOrNull<HashTable_O>() ) {
            return true;
        }
        return false;
    };


#define ARGS_af_readtableP "(arg)"
#define DECL_af_readtableP ""
#define DOCS_af_readtableP "readtableP"
    bool af_readtableP(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	return obj.isA<ReadTable_O>();
    };

#define ARGS_af_structureObjectP "(arg)"
#define DECL_af_structureObjectP ""
#define DOCS_af_structureObjectP "structureObjectP"
    bool af_structureObjectP(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	if (obj.isA<StructureObject_O>()) return true;
	return obj.isA<Instance_O>();
    };

#define ARGS_af_arrayP "(arg)"
#define DECL_af_arrayP ""
#define DOCS_af_arrayP "arrayP"
    bool af_arrayP(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	return obj.isA<Array_O>();
    };


#define ARGS_af_singleDispatchGenericFunctionP "(arg)"
#define DECL_af_singleDispatchGenericFunctionP ""
#define DOCS_af_singleDispatchGenericFunctionP "singleDispatchGenericFunctionP"
    bool af_singleDispatchGenericFunctionP(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	return obj.isA<SingleDispatchGenericFunction_O>();
    };


#define ARGS_af_activation_frame_p "(arg)"
#define DECL_af_activation_frame_p ""
#define DOCS_af_activation_frame_p "activation_frame_p"
    bool af_activation_frame_p(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	return obj.isA<ActivationFrame_O>();
    };

#define ARGS_af_externalObjectP "(arg)"
#define DECL_af_externalObjectP ""
#define DOCS_af_externalObjectP "externalObjectP"
    bool af_externalObjectP(T_sp obj)
    {_G();
	if (obj.nilp()) return false;
	return obj.isA<ExternalObject_O>();
    };



    
    
#define ARGS_af_simple_bit_vector_p "(arg)"
#define DECL_af_simple_bit_vector_p ""
#define DOCS_af_simple_bit_vector_p "simple_bit_vector_p"
    bool af_simple_bit_vector_p(T_sp o)
    {_G();
	if ( o.nilp() ) return false;
	if ( SimpleBitVector_sp sbv = o.asOrNull<SimpleBitVector_O>() )
	{
	    return true;
	}
	return false;
    };



    
    
#define ARGS_af_simple_vector_p "(arg)"
#define DECL_af_simple_vector_p ""
#define DOCS_af_simple_vector_p "simple_vector_p"
    bool af_simple_vector_p(T_sp o)
    {_G();
	if ( o.nilp() ) return false;
	if ( VectorObjects_sp vo=o.asOrNull<VectorObjects_O>() )
	{
	    return true;
	}
	return false;
    };



    
    
#define ARGS_af_compiled_function_p "(arg)"
#define DECL_af_compiled_function_p ""
#define DOCS_af_compiled_function_p "compiled_function_p"
    bool af_compiled_function_p(T_sp o)
    {_G();
	if ( o.nilp() ) return false;
        if ( Function_sp fn = o.asOrNull<Function_O>() ) {
            return fn->closure->compiledP();
        }
        return false;
    };

#define ARGS_af_genericFunctionP "(arg)"
#define DECL_af_genericFunctionP ""
#define DOCS_af_genericFunctionP "genericFunctionP"
    bool af_genericFunctionP(T_sp o)
    {_G();
	if ( o.nilp() ) return false;
	if ( Function_sp cf=o.asOrNull<Function_O>() )
	{
            IMPLEMENT_MEF(BF("I should have a more sophisticated test here"));
	    return true;
	}
	return false;
    };




    


    
#if 0    
#define ARGS_af_sourceCodeConsP "(arg)"
#define DECL_af_sourceCodeConsP ""
#define DOCS_af_sourceCodeConsP "sourceCodeConsP"
    bool af_sourceCodeConsP(T_sp o)
    {_G();
	if ( o.nilp() ) return false;
	if ( Cons_sp scc = o.asOrNull<ourceCodeCons_O>() )
	{
	    return true;
	}
	return false;
    };
#endif



    
    

    
    

    
    
#define ARGS_af_properListP "(arg)"
#define DECL_af_properListP ""
#define DOCS_af_properListP "Return true if arg is a proper list"
    bool af_properListP(T_sp arg)
    {_G();
	T_sp fast, slow;
	bool test = true;
	fast = slow = arg;
	for ( int n=0; !fast.nilp(); n++, fast = oCdr(fast) )
	{
	    if ( !af_listp(fast) ) {
		test = false;
		break;
	    }
	    if ( n&1 ){ 
		/* Circular list? */
		if ( slow == fast) {
		    test = false;
		    break;
		}
		slow = oCdr(slow);
	    }
	}
	return test;
    };




    
    
#define ARGS_af_pathnamep "(arg)"
#define DECL_af_pathnamep ""
#define DOCS_af_pathnamep "pathnamep"
    bool af_pathnamep(T_sp obj) {
	if ( obj.nilp() ) return false;
	return obj.isA<Pathname_O>();
    };


    
    
#define ARGS_af_logicalPathnameP "(arg)"
#define DECL_af_logicalPathnameP ""
#define DOCS_af_logicalPathnameP "logicalPathnameP"
    bool af_logicalPathnameP(T_sp obj) {
	if ( obj.nilp() ) return false;
	return obj.isA<LogicalPathname_O>();
    };






    void initialize_predicates()
    {
	Defun(endp);
#define newNameDefun(pkg,myname,lispname) af_def(pkg,#lispname,&af_##myname,ARGS_af_##myname,DECL_af_##myname,DOCS_af_##myname)
	newNameDefun(ClPkg,symbolp,symbolp);
	newNameDefun(ClPkg,consP,consp);
	newNameDefun(ClPkg,listp,listp);
	newNameDefun(ClPkg,numberP,numberp);
	newNameDefun(ClPkg,integerP,integerp);
	newNameDefun(ClPkg,rationalP,rationalp);
	newNameDefun(ClPkg,floatP,floatp);
	newNameDefun(ClPkg,realP,realp);
	newNameDefun(ClPkg,complexP,complexp);
	newNameDefun(ClPkg,characterP,characterp);
	newNameDefun(ClPkg,stringP,stringp);
	newNameDefun(ClPkg,bitVectorP,bit_vector_p);
	newNameDefun(ClPkg,vectorP,vectorp);
	newNameDefun(ClPkg,simple_vector_p,simple_vector_p);
	newNameDefun(ClPkg,strP,simple_string_p);
	newNameDefun(ClPkg,simple_bit_vector_p,simple_bit_vector_p);
	newNameDefun(ClPkg,arrayP,arrayp);
	ClDefun(packagep);
	newNameDefun(ClPkg,functionP,functionp);
	newNameDefun(ClPkg,compiled_function_p,compiled_function_p);
	newNameDefun(CorePkg,genericFunctionP,genericFunctionP);
	newNameDefun(ClPkg,keywordP,keywordp);
	SYMBOL_EXPORT_SC_(ClPkg,atom);
	Defun(atom);
	CoreDefun(baseCharP);
	newNameDefun(CorePkg,fixnumP,fixnump);
	newNameDefun(CorePkg,bignumP,bignump);
	Defun(strP);
	Defun(doubleFloatP);
	SYMBOL_EXPORT_SC_(ClosPkg,classp);
	af_def(ClosPkg,"classp",&af_classp,ARGS_af_classp,DECL_af_classp,DOCS_af_classp);
	Defun(lambda_list_handler_p);
	Defun(ratioP);
#ifdef CLASP_LONG_FLOAT
	Defun(longFloatP);
#endif
	Defun(shortFloatP);
	Defun(singleFloatP);
	Defun(pathP);
	Defun(hashTableP);
	Defun(readtableP);
	Defun(structureObjectP);
	Defun(singleDispatchGenericFunctionP);
	Defun(activation_frame_p);
	Defun(externalObjectP);
//	Defun(sourceCodeConsP);
	Defun(interpretedFunctionP);
	Defun(properListP);
	Defun(pathnamep);
	Defun(logicalPathnameP);
    };

};
