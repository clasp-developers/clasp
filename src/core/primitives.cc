#define	DEBUG_LEVEL_FULL

#include "core/foundation.h"
#include "core/object.h"
#include "core/cons.h"
#include "core/corePackage.h"
#include "core/environment.h"
#include "core/fileSystem.h"
#include "core/bformat.h"
#include "core/bignum.h"
#include "core/character.h"
#include "core/executables.h"
#include "core/package.h"
#include "core/readtable.h"
#include "core/vectorObjectsWithFillPtr.h"
#include "core/instance.h"
#include "core/backquote.h"
#include "core/sequence.h"
#include "core/structureObject.h"
#include "core/wrappedPointer.h"
#include "core/bitVector.h"
#include "core/pathname.h"
#include "core/unixfsys.h"
#include "core/predicates.h"
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
#include "print.h"
#include "standardClass.h"
#include "forwardReferencedClass.h"
#include "singleDispatchMethod.h"
#include "singleDispatchGenericFunction.h"
#include "lambdaListHandler.h"
#include "designators.h"
#include "primitives.h"
#include "hashTable.h"
#include "hashTableEql.h"
#include "multipleValues.h"
#include "lispStream.h"
#include "structureObject.h"
#include "lispReader.h"
#include "designators.h"
#include "profile.h"
#include "core/wrappers.h"
namespace core
{





    
    
#define ARGS_af_argc "()"
#define DECL_af_argc ""
#define DOCS_af_argc "argc"
    int af_argc()
    {_G();
        return _lisp->_Argc;
    };


    
    
#define ARGS_af_argv "(idx)"
#define DECL_af_argv ""
#define DOCS_af_argv "argv"
    Str_sp af_argv(int idx)
    {_G();
        return Str_O::create(_lisp->_Argv[idx]);
    };
    
    
#define ARGS_cl_set "(sym value)"
#define DECL_cl_set ""
#define DOCS_cl_set "set"
    T_sp cl_set(Symbol_sp sym, T_sp val)
    {_G();
        if ( sym.nilp() ) {
            SIMPLE_ERROR(BF("You cannot assign to the constant NIL"));
        }
        sym->setf_symbolValue(val);
        return val;
    };

    
    
#define ARGS_af_dumpAddressOf "(arg)"
#define DECL_af_dumpAddressOf ""
#define DOCS_af_dumpAddressOf "dumpAddressOf"
    void af_dumpAddressOf(T_sp arg)
    {_G();
        void* ptr = arg.pbase();
        printf("%s:%d  AddressOf = %p\n", __FILE__, __LINE__, ptr );
    };
    
    
#define ARGS_af_incompleteNextHigherPowerOf_2 "(arg)"
#define DECL_af_incompleteNextHigherPowerOf_2 ""
#define DOCS_af_incompleteNextHigherPowerOf_2 "incompleteNextHigherPowerOf_2 - see the incompleteNextHigherPowerOf_2 builtin - only works for Fixnums and not the full range; just for testing"
    int af_incompleteNextHigherPowerOf_2(Fixnum_sp fn)
    {_G();
        unsigned int f = fn->get();
        return 1<< ((sizeof(f)*8) - __builtin_clz(f));
    };

    
    
#define ARGS_af_testBasePointerConversion "(arg)"
#define DECL_af_testBasePointerConversion ""
#define DOCS_af_testBasePointerConversion "testBasePointerConversion"
    void af_testBasePointerConversion(T_sp p)
    {_G();
        printf("original px_ref = %p\n", p.px_ref());
        gctools::tagged_base_ptr base(p);
        printf("base = %p\n", base.base.px_ref());
        gctools::tagged_backcastable_base_ptr<T_O> backcastable(p);
        printf("Backcastable base = %p  offset(Fixnum) = %d\n", backcastable.base.px_ref(), backcastable.offset.fixnum());
        T_sp back = backcastable.backcast();
        printf("After backcasting back.px_ref() = %p\n", back.px_ref() );
    };


    
    
#define ARGS_af_exceptionStackDump "()"
#define DECL_af_exceptionStackDump ""
#define DOCS_af_exceptionStackDump "exceptionStackDump"
    void af_exceptionStackDump()
    {_G();
        ExceptionStack& stack = _lisp->exceptionStack();
        printf("Exception stack size: %zu members\n", stack.size());
        for ( int i(0); i<stack.size(); ++i ) {
            string kind;
            switch (stack[i]._FrameKind) {
            case CatchFrame:
                kind = "catch";
                break;
            case BlockFrame:
                kind = "block";
                break;
            case TagbodyFrame:
                kind = "tagbody";
                break;
            default:
                kind = "unknown";
                break;
            };
            printf("Exception stack[%2d] = %8s %s@%p\n", i, kind.c_str(), _rep_(stack[i]._Key).c_str(), stack[i]._Key.px_ref());
        }
        printf("----Done----\n");
    };


    
    
#define ARGS_af_allRegisteredClassNames "()"
#define DECL_af_allRegisteredClassNames ""
#define DOCS_af_allRegisteredClassNames "allRegisteredClassNames"
    Vector_sp af_allRegisteredClassNames()
    {_G();
        VectorObjects_sp vo=VectorObjects_O::make(_Nil<T_O>(),_Nil<Cons_O>(),_lisp->classSymbolsHolder().size(),false);
        for ( int i(0),iEnd(_lisp->classSymbolsHolder().size());i<iEnd;++i) {
            vo->setf_elt(i,_lisp->classSymbolsHolder()[i]);
        }
        return vo;
    };
    
    
#define ARGS_af_toTaggedFixnum "(arg)"
#define DECL_af_toTaggedFixnum ""
#define DOCS_af_toTaggedFixnum "toTaggedFixnum"
    T_sp af_toTaggedFixnum(int val)
    {_G();
	return gctools::smart_ptr<T_O>(val);
    };



    
    
#define ARGS_af_fromTaggedFixnum "(val)"
#define DECL_af_fromTaggedFixnum ""
#define DOCS_af_fromTaggedFixnum "fromTaggedFixnum"
    int af_fromTaggedFixnum(T_sp val)
    {_G();
	if ( val.BaseType::fixnump() )
	{
	    return val.fixnum();
	}
	SIMPLE_ERROR(BF("Not a fixnum"));
    };

#define ARGS_af_dumpTaggedFixnum "(arg)"
#define DECL_af_dumpTaggedFixnum ""
#define DOCS_af_dumpTaggedFixnum "dumpTaggedFixnum"
    void af_dumpTaggedFixnum(T_sp val)
    {_G();
	if ( val.BaseType::fixnump() ) {
	    printf("%s:%d Raw TaggedFixnum %p   Untagged %d\n",
		   __FILE__,__LINE__, val.pxget(), val.fixnum() );
	} else
	    printf("%s:%d Not a tagged fixnum\n", __FILE__, __LINE__ );
    }


    
    
#define ARGS_af_getEnv "(arg)"
#define DECL_af_getEnv ""
#define DOCS_af_getEnv "getEnv"
    Str_sp af_getEnv(Str_sp arg)
    {_G();
	char* sres = getenv(arg->c_str());
	if ( sres == NULL ) {
	    return _Nil<Str_O>();
	}
	return Str_O::create(sres);
    };

    

    
#define ARGS_af_pointer "(arg)"
#define DECL_af_pointer ""
#define DOCS_af_pointer "Return the value of the pointer - used by conditions.lsp"
    int af_pointer(T_sp obj)
    {_G();
	return obj.intptr();
    };

    
    
#define ARGS_af_isTrue "(arg)"
#define DECL_af_isTrue ""
#define DOCS_af_isTrue "isTrue"
    bool af_isTrue(T_sp arg)
    {_G();
	return arg.isTrue();
    };


    
    
#define ARGS_af_substitute "(arg)"
#define DECL_af_substitute ""
#define DOCS_af_substitute "substitute"
    T_mv af_substitute()
    {_G();
	IMPLEMENT_MEF(BF("Implement substitute"));
    };


    
    
#define ARGS_af_unbound "()"
#define DECL_af_unbound ""
#define DOCS_af_unbound "Return the UNBOUND value"
    T_sp af_unbound()
    {_G();
	return _Unbound<T_O>();
    };

    
    
#define ARGS_af_smartPointerDetails "()"
#define DECL_af_smartPointerDetails ""
#define DOCS_af_smartPointerDetails "smartPointerDetails - returns (values ptr-type px-offset px-size). The ptr-type is the type of pointer used to pass objects - either MPS-GARBAGE-COLLECTION or INTRUSIVE-REFERENCE-COUNTED-POINTER. The px-offset is the number of bytes offset of the smart_ptr data pointer from the start of the smart_ptr and px-size is the size of the data pointer"
    T_mv af_smartPointerDetails()
    {_G();
	SYMBOL_SC_(CorePkg,intrusiveReferenceCountedPointer);
	SYMBOL_SC_(CorePkg,sharedReferenceCountedPointer);
	SYMBOL_SC_(CorePkg,mpsGarbageCollection);
#if defined(USE_MPS)
	Symbol_sp ptrType = _sym_mpsGarbageCollection;
#else
	Symbol_sp ptrType = _sym_intrusiveReferenceCountedPointer;
#endif
	T_sp dummy;
	Fixnum_sp pxOffset = Fixnum_O::create(dummy.offset_of_px_from_this());
	Fixnum_sp pxSize = Fixnum_O::create(dummy.size_of_px());
	return Values(ptrType,pxOffset,pxSize);
    }

    
    
#define ARGS_af_values "(&rest args)"
#define DECL_af_values ""
#define DOCS_af_values "values"
    T_mv af_values(Cons_sp args)
    {_G();
	// returns multiple values
	T_mv result = ValuesFromCons(args);
	return result;
    }




    
    
#define ARGS_af_values_list "(list)"
#define DECL_af_values_list ""
#define DOCS_af_values_list "values_list"
    T_mv af_values_list(Cons_sp list)
    {_G();
	return ValuesFromCons(list);
    }




    Symbol_sp functionBlockName(T_sp functionName)
    {_G();
	if ( af_symbolp(functionName) ) return functionName.as<Symbol_O>();
	if ( af_consP(functionName) )
	{
	    Cons_sp cfn = functionName.as_or_nil<Cons_O>();
	    if ( oCar(cfn) == cl::_sym_setf && af_symbolp(oCadr(cfn))& oCadr(cfn).notnilp() )
	    {
		return oCadr(cfn).as<Symbol_O>();
	    }
	}
	return _Nil<Symbol_O>();
    }

    
    
#define ARGS_af_functionBlockName "(functionName)"
#define DECL_af_functionBlockName ""
#define DOCS_af_functionBlockName "See CLHS glossary 'function block name'. If the functionName is a symbol return it.  If the functionName is a cons of the form (setf xxxx) return xxxx"
    Symbol_mv af_functionBlockName(T_sp functionName)
    {_G();
	Symbol_sp output = functionBlockName(functionName);
	if ( output.nilp() ) 
	{
	    SIMPLE_ERROR(BF("Invalid function name: %s") % _rep_(functionName) );
	}
	return(Values(output));
    }




    
    
#define ARGS_af_validFunctionNameP "(arg)"
#define DECL_af_validFunctionNameP ""
#define DOCS_af_validFunctionNameP "validFunctionNameP"
    T_mv af_validFunctionNameP(T_sp arg)
    {_G();
	T_sp name = functionBlockName(arg);
	if ( name.nilp() ) return(Values(_Nil<T_O>()));
	return(Values(_lisp->_true()));
    };
    
    
    

#define ARGS_af_makeStringOutputStream "(&key (elementType 'character))"
#define DECL_af_makeStringOutputStream ""
#define DOCS_af_makeStringOutputStream "makeStringOutputStream"
    T_mv af_makeStringOutputStream(T_sp elementType)
    {_G();
	if ( elementType != cl::_sym_Character_O )
	{
	    SIMPLE_ERROR(BF("Add support for non character string output streams - you asked for %s") % _rep_(elementType) );
	}
	StringOutStream_sp ss = StringOutStream_O::make();
	return(Values(ss));
    };



    
    
#define ARGS_af_getOutputStreamString "(stringOutputStream)"
#define DECL_af_getOutputStreamString ""
#define DOCS_af_getOutputStreamString "getOutputStreamString"
    string af_getOutputStreamString(StringOutStream_sp stringOutputStream)
    {_G();
	return stringOutputStream->str();
    };



    
    
#define ARGS_af_testMemoryError "()"
#define DECL_af_testMemoryError ""
#define DOCS_af_testMemoryError "testMemoryError"
    void af_testMemoryError()
    {_G();
	int* h = (int*)malloc(sizeof(int));
	*h = 1;
	free(h);
	*h = 2;
    };


    
    
#define ARGS_af_separatePairList "(listOfPairs)"
#define DECL_af_separatePairList ""
#define DOCS_af_separatePairList "Split a list of pairs into a pair of lists returned as MultipleValues. The first list is each first element and the second list is each second element or nil if there was no second element"
    Cons_mv af_separatePairList(Cons_sp listOfPairs)
    {_G();
	ql::list firsts(_lisp);
	ql::list seconds(_lisp);
	for ( Cons_sp cur = listOfPairs; cur.notnilp(); cur=cCdr(cur) )
	{
	    T_sp element = oCar(cur);
	    if ( af_atom(element) )
	    {
		firsts << element;
		seconds << _Nil<T_O>();
	    } else if ( af_consP(element) )
	    {
		Cons_sp pair = element.as_or_nil<Cons_O>();
		size_t pairlen = af_length(pair);
		if ( pairlen == 2 || pairlen == 1 )
		{
		    firsts << oCar(pair);
		    seconds << oCadr(pair);
		} else
		{
		    SIMPLE_ERROR(BF("Expected one or two element list got: %s") % _rep_(pair) );
		}
	    } else
	    {
		SIMPLE_ERROR(BF("Expected single object or 2-element list - got: %s") % _rep_(element) );
	    }
	}
	return(Values(firsts.cons(),seconds.cons()));
    }



    
#if DEPRECIATED_C_FUNCTION    
#define ARGS_af_c_function "(sym)"
#define DECL_af_c_function ""
#define DOCS_af_c_function "c_function"
    Pointer_mv af_c_function(Symbol_sp sym)
    {_G();
	return(Values(_lisp->lookup_c_function_ptr(sym)));
    };
#endif
    
    
#define ARGS_af_macroFunction "(symbol &optional env)"
#define DECL_af_macroFunction ""
#define DOCS_af_macroFunction "See CLHS: macroFunction"
    T_sp af_macroFunction(Symbol_sp symbol, Environment_sp env)
    {_G();
	Function_sp func = af_interpreter_lookup_macro(symbol,env);
	if ( func.nilp() ) return _Nil<T_O>();
	if ( func->macroP() ) return func;
	return _Nil<T_O>();
    }
    
    
#define ARGS_af_specialOperatorP "(symbol)"
#define DECL_af_specialOperatorP ""
#define DOCS_af_specialOperatorP "See CLHS: special-operator-p"
    T_mv af_specialOperatorP(T_sp sym)
    {_G();
	SYMBOL_EXPORT_SC_(ClPkg,let);
	SYMBOL_EXPORT_SC_(ClPkg,letSTAR);
	SYMBOL_EXPORT_SC_(ClPkg,return_from);
	SYMBOL_EXPORT_SC_(ClPkg,catch);
	SYMBOL_EXPORT_SC_(ClPkg,load_time_value);
	SYMBOL_EXPORT_SC_(ClPkg,setq);
	SYMBOL_EXPORT_SC_(ClPkg,eval_when);
	SYMBOL_EXPORT_SC_(ClPkg,locally);
	SYMBOL_EXPORT_SC_(ClPkg,symbol_macrolet);
	SYMBOL_EXPORT_SC_(ClPkg,flet);
	SYMBOL_EXPORT_SC_(ClPkg,macrolet);
	SYMBOL_EXPORT_SC_(ClPkg,tagbody);
	SYMBOL_EXPORT_SC_(ClPkg,multiple_value_call);
	SYMBOL_EXPORT_SC_(ClPkg,the);
	SYMBOL_EXPORT_SC_(ClPkg,go);
	SYMBOL_EXPORT_SC_(ClPkg,multiple_value_prog1);
	SYMBOL_EXPORT_SC_(ClPkg,if);
	SYMBOL_EXPORT_SC_(ClPkg,unwind_protect);
	SYMBOL_EXPORT_SC_(ClPkg,labels);
	SYMBOL_EXPORT_SC_(ClPkg,progv);
	if ( ( sym == cl::_sym_block) ||
	     ( sym == cl::_sym_let) ||
	     ( sym == cl::_sym_letSTAR) ||
	     ( sym == cl::_sym_return_from) ||
	     ( sym == cl::_sym_catch) ||
	     ( sym == cl::_sym_load_time_value) ||
	     ( sym == cl::_sym_setq) ||
	     ( sym == cl::_sym_eval_when) ||
	     ( sym == cl::_sym_locally) ||
	     ( sym == cl::_sym_symbol_macrolet) ||
	     ( sym == cl::_sym_flet) ||
	     ( sym == cl::_sym_macrolet) ||
	     ( sym == cl::_sym_tagbody) ||
	     ( sym == cl::_sym_function) ||
	     ( sym == cl::_sym_multiple_value_call) ||
	     ( sym == cl::_sym_the) ||
	     ( sym == cl::_sym_go) ||
	     ( sym == cl::_sym_multiple_value_prog1) ||
	     ( sym == cl::_sym_throw) ||
	     ( sym == cl::_sym_if) ||
	     ( sym == cl::_sym_progn) ||
	     ( sym == cl::_sym_unwind_protect) ||
	     ( sym == cl::_sym_labels) ||
	     ( sym == cl::_sym_progv) ||
	     ( sym == cl::_sym_quote ) )
	{
	    return(Values(_lisp->_true()));
	}
	return(Values(_Nil<T_O>()));
    };


    
    
#define ARGS_af_ash "(integer count)"
#define DECL_af_ash ""
#define DOCS_af_ash "CLHS: ash"
    int af_ash(Integer_sp integer, Integer_sp count)
    {_G();
	int cnt = count->as_int();
	int res;
	if ( cnt > 0 )
	{
	    res = integer->as_int() << cnt;
	} else
	{
	    res = integer->as_int() >> (-cnt);
	}
	return res;
    };


    
    

#define ARGS_af_break "(&optional fmt-control &rest args)"
#define DECL_af_break ""
#define DOCS_af_break "Built in implementation of break - that calls the internal debugger - replace this with a CL implemented version"
    void af_break(Str_sp fmt, Cons_sp args)
    {_G();
	if ( fmt.notnilp() ) {
	    af_format(_lisp->_true(),fmt,args);
	}
	dbg_hook("built in break");
    };

    
    
    
#define ARGS_af_gdb "(&optional msg)"
#define DECL_af_gdb ""
#define DOCS_af_gdb "hook to invoke gdb"
    void af_gdb(T_sp msg)
    {_G();
        T_sp obj = msg;
	string smsg = "No msg";
	if ( obj.notnilp() ) {
	    smsg = _rep_(obj);
	}
        dbg_hook(smsg.c_str());
    };


#define ARGS_af_gdbInspect "(msg o)"
#define DECL_af_gdbInspect ""
#define DOCS_af_gdbInspect "hook to invoke gdb"
    void af_gdbInspect(Str_sp msg, T_sp o)
    {_G();
	printf("gdbInspect object: %s\n", _rep_(o).c_str());
	dbg_hook(msg->get().c_str());
    };




    
    
#define ARGS_af_constantp "(obj &optional env)"
#define DECL_af_constantp ""
#define DOCS_af_constantp "constantp"
    bool af_constantp(T_sp obj, T_sp env)
    {_G();
	// ignore env
	if ( af_numberP(obj) ) return true;
	if ( af_characterP(obj) ) return true;
	if ( af_arrayP(obj) ) return true;
	// TODO add various kinds of array
	if ( af_consP(obj) && oCar(obj) == cl::_sym_quote) return true;
	if ( obj.nilp() ) return true;
	if ( af_symbolp(obj) )
	{
	    if ( af_keywordP(obj) ) return true;
	    return obj.as<Symbol_O>()->isConstant();
	}
	return false;
    };

    
    
#define ARGS_af_identity "(arg)"
#define DECL_af_identity ""
#define DOCS_af_identity "identity"
    T_mv af_identity(T_sp arg)
    {_G();
	return(Values(arg));
    };



#define DOCS_af_macroexpand_default "macroexpand_default Default value of *macroexpand-hook*"
#define LOCK_af_macroexpand_default 1
#define ARGS_af_macroexpand_default "(macro_function form macro_env)"
#define DECL_af_macroexpand_default ""
    T_mv af_macroexpand_default(Function_sp macro_function, T_sp form, Environment_sp macro_env)
    {_G();
	T_sp result = eval::funcall(macro_function,form,macro_env);
	return(Values(result));
    };







#define ARGS_af_null "(obj)"
#define DECL_af_null ""    
#define DOCS_af_null "null test - return true if the object is the empty list otherwise return nil"
#define LOCK_af_null 1
    T_mv af_null(T_sp obj)
    {_G();
	if (obj.nilp() ) return(Values(_lisp->_true()));
	return(Values(_Nil<T_O>()));
    };


#define LOCK_af_classOf 1
#define DOCS_af_classOf "return class of object - see CLHS"
#define ARGS_af_classOf "(obj)"
#define DECL_af_classOf ""    
    Class_sp af_classOf(T_sp obj)
    {_G();
	Class_sp result = lisp_instance_class(obj);
#if DEBUG_CLOS >= 3
	printf("\nMLOG classOf %s ---> %s\n", obj->__repr__().c_str(), result->__repr__().c_str() );
#endif
	return(result);
    }



#define LOCK_af_STARfset 1
#define DOCS_af_STARfset "fset - bind a function to its name - handles symbol function-name and (SETF XXXX) names. (macro) defines if the function is a macro or not."
#define ARGS_af_STARfset "(function-name fn &optional macro)"
#define DECL_af_STARfset ""    
    T_mv af_STARfset(T_sp functionName, Function_sp fn, T_sp macro )
    {_G();
	if ( macro.isTrue() )
	{
	    if ( !af_symbolp(functionName) )
	    {
		SIMPLE_ERROR(BF("You cannot define a macro with the name[%s]") % _rep_(functionName) );
	    }
	    fn->set_kind(kw::_sym_macro);
	} else
	{
	    fn->set_kind(kw::_sym_function);
	}
	if ( af_symbolp(functionName) )
	{
	    Symbol_sp symbol = functionName.as<Symbol_O>();
	    symbol->setf_symbolFunction(fn);
	    return Values(fn);
	} else if (af_consP(functionName))
	{
	    SYMBOL_EXPORT_SC_(ClPkg,setf);
	    Cons_sp cur = functionName.as_or_nil<Cons_O>();
	    if ( oCar(cur) == cl::_sym_setf )
	    {
		Symbol_sp symbol = oCadr(cur).as<Symbol_O>();
		_lisp->set_setfDefinition(symbol,fn);
		return Values(fn);
	    }
	}
	SIMPLE_ERROR(BF("Illegal name for function[%s]") % _rep_(functionName) );
    };





    
    
#define ARGS_af_fdefinition "(function-name)"
#define DECL_af_fdefinition ""
#define DOCS_af_fdefinition "fdefinition"
    Function_mv af_fdefinition(T_sp functionName)
    {_G();
	if ( af_symbolp(functionName) )
	{
	    Symbol_sp sym = functionName.as<Symbol_O>();
	    return(Values(sym->symbolFunction()));
	} else if ( af_consP(functionName) )
	{
	    Cons_sp cname = functionName.as_or_nil<Cons_O>();
	    if ( oCar(cname) == cl::_sym_setf )
	    {
		Symbol_sp name = oCadr(cname).as<Symbol_O>();
		if ( name.notnilp() )
		{
		    return(Values(_lisp->get_setfDefinition(name)));
		}
	    }
	}
	SIMPLE_ERROR(BF("Illegal function-name[%s]") % _rep_(functionName) );
    }



    
#define ARGS_af_fboundp "(function-name)"
#define DECL_af_fboundp ""
#define DOCS_af_fboundp "fboundp"
    bool af_fboundp(T_sp functionName)
    {_G();
        if (functionName.nilp() ) {
            TYPE_ERROR(functionName,cl::_sym_Symbol_O);
        }
	if ( af_symbolp(functionName) )
	{
	    Symbol_sp sym = functionName.as<Symbol_O>();
	    return sym->symbolFunction().pointerp();
	} else if ( af_consP(functionName) )
	{
	    Cons_sp cname = functionName.as_or_nil<Cons_O>();
	    if ( oCar(cname) == cl::_sym_setf )
	    {
		Symbol_sp name = oCadr(cname).as<Symbol_O>();
		if ( name.notnilp() )
		{
		    return _lisp->get_setfDefinition(name).notnilp();
		}
	    }
	}
	SIMPLE_ERROR(BF("Illegal function-name[%s]") % _rep_(functionName) );
    }




    
#define ARGS_af_fmakunbound "(function-name)"
#define DECL_af_fmakunbound ""
#define DOCS_af_fmakunbound "fmakunbound"
    T_mv af_fmakunbound(T_sp functionName)
    {_G();
	if ( af_symbolp(functionName) )
	{
	    Symbol_sp sym = functionName.as<Symbol_O>();
	    sym->setf_symbolFunction(_Nil<Function_O>());
	    return(Values(sym));
	} else if ( af_consP(functionName) )
	{
	    Cons_sp cname = functionName.as_or_nil<Cons_O>();
	    if ( oCar(cname) == cl::_sym_setf )
	    {
		Symbol_sp name = oCadr(cname).as<Symbol_O>();
		if ( name.notnilp() )
		{
		    _lisp->remove_setfDefinition(name);
		    return(Values(functionName));
		}
	    }
	}
	SIMPLE_ERROR(BF("Illegal function-name[%s]") % _rep_(functionName) );
    }








#define LOCK_af_read_delimited_list 1
#define DOCS_af_read_delimited_list "read a list up to a specific character - see CLHS"
#define ARGS_af_read_delimited_list "(char &optional input-stream-designator recursive-p)"
#define DECL_af_read_delimited_list ""    
    T_mv af_read_delimited_list(Character_sp chr, T_sp input_stream_designator, T_sp recursive_p)
    {_G();
	Stream_sp sin = coerce::inputStreamDesignator(input_stream_designator);
#if 0
	// I think it is safe to ignore recursive_p
	if ( recursive_p.isTrue() )
	{
	    SIMPLE_ERROR(BF("Currently I don't handle recursive-p[true] for read_delimited_list"));
	}
#endif
	T_sp result = read_list(sin,chr->asChar(),true);
	if ( cl::_sym_STARread_suppressSTAR->symbolValue().isTrue() )
	{
	    return(Values(_Nil<T_O>()));
	}
	return(Values(result));
    }





#define LOCK_af_read 1
#define DOCS_af_read "read an object from a stream - see CLHS"
#define ARGS_af_read "(&optional input-stream-designator (eof-error-p t) eof-value recursive-p)"
#define DECL_af_read ""    
    T_sp af_read(T_sp input_stream_designator, T_sp eof_error_p, T_sp eof_value, T_sp recursive_p)
    {_G();
	Stream_sp sin = coerce::inputStreamDesignator(input_stream_designator);
	return(read_lisp_object(sin,eof_error_p.isTrue(),eof_value,recursive_p.notnilp()));
    }


#define DOCS_af_read_preserving_whitespace "read an object from a stream while preserving whitespace - see CLHS"
#define ARGS_af_read_preserving_whitespace "(&optional input-stream-designator (eof-error-p t) eof-value recursive-p)"
#define DECL_af_read_preserving_whitespace ""    
    T_sp af_read_preserving_whitespace(T_sp input_stream_designator, T_sp eof_error_p, T_sp eof_value, T_sp recursive_p)
    {_G();
	DynamicScopeManager scope(_sym_STARpreserve_whitespace_pSTAR,BRCL_T);
	Stream_sp sin = coerce::inputStreamDesignator(input_stream_designator);
	return(read_lisp_object(sin,eof_error_p.isTrue(),eof_value,recursive_p));
    }






/* -------------------------------------------------------- */
/*     Sequence primitives                                  */



#if 0 
    GC_RESULT VectorStepper::onHeapScanGCRoots(GC_SCAN_ARGS_PROTOTYPE)
    {
#ifdef USE_MPS
        MPS_SCAN_BEGIN(GC_SCAN_STATE) {
            SMART_PTR_FIX(this->_Domain);
        } MPS_SCAN_END(GC_SCAN_STATE);
#endif
        return GC_RES_OK;
    }
#endif

#if 0
    GC_RESULT ConsStepper::onHeapScanGCRoots(GC_SCAN_ARGS_PROTOTYPE)
    {
#ifdef USE_MPS
        MPS_SCAN_BEGIN(GC_SCAN_STATE) {
            SMART_PTR_FIX(this->_Cur);
        } MPS_SCAN_END(GC_SCAN_STATE);
#endif
        return GC_RES_OK;
    }
#endif



    ListOfSequenceSteppers::ListOfSequenceSteppers(Cons_sp sequences)
    {_G();
	this->_AtEnd = false;
	for ( Cons_sp cur = sequences; cur.notnilp(); cur=cCdr(cur) )
	{
	    T_sp obj = oCar(cur);
	    if ( af_vectorP(obj) )
	    {
		if (af_length(obj.as<Vector_O>()) == 0 ) goto EMPTY;
                VectorStepper* vP(gctools::ClassAllocator<VectorStepper>::allocateClass(obj.as<Vector_O>()));
                this->_Steppers.push_back(vP);
	    } else if ( af_consP(obj) )
	    {
		if (obj.as_or_nil<Cons_O>().nilp()) goto EMPTY;
                ConsStepper* cP(gctools::ClassAllocator<ConsStepper>::allocateClass(obj.as_or_nil<Cons_O>()));
                this->_Steppers.push_back(cP);
	    } else if ( obj.nilp() ) 
	    {
		goto EMPTY;
	    } else
	    {
		SIMPLE_ERROR(BF("Illegal object for stepper[%s] class[%s]") % _rep_(obj) % obj->_instanceClass()->classNameAsString() );
	    }
	}
	this->_AtEnd = false;
	return;
    EMPTY:
	this->_AtEnd = true;
    }

    ListOfSequenceSteppers::~ListOfSequenceSteppers()
    {
	for ( auto rit=this->_Steppers.begin();
	      rit!=this->_Steppers.end(); rit++ )
	{
            gctools::ClassAllocator<SequenceStepper>::deallocateClass(*rit);
	}
    }



    void ListOfSequenceSteppers::fillValueFrameUsingCurrentSteppers(ActivationFrame_sp frame) const
    {_G();
	if ( this->_AtEnd) SIMPLE_ERROR(BF("Tried to make list of ended stepper"));
	Cons_sp res = _Nil<Cons_O>();
	int idx=0;
	for (auto rit=this->_Steppers.begin(); rit!=this->_Steppers.end(); rit++ )
	{
	    frame->set_entry(idx,(*rit)->element());
	    ++idx;
	}
    }


    bool ListOfSequenceSteppers::advanceSteppers()
    {_OF();
	if ( this->_AtEnd) SIMPLE_ERROR(BF("Tried to advance ended stepper"));
	for ( auto it=this->_Steppers.begin(); it!=this->_Steppers.end(); it++ )
	{
	    this->_AtEnd |= (*it)->advance();
	}
	return !this->_AtEnd;
    }



    class ListOfListSteppers : public ListOfSequenceSteppers
    {
    public:
	ListOfListSteppers(Cons_sp lists);
	virtual ~ListOfListSteppers() {};
    };

    ListOfListSteppers::ListOfListSteppers(Cons_sp sequences)
    {_G();
	for ( Cons_sp cur = sequences; cur.notnilp(); cur=cCdr(cur) )
	{
	    T_sp obj = oCar(cur);
	    if ( af_consP(obj) )
	    {
		if ( obj.as_or_nil<Cons_O>().nilp() ) goto EMPTY;
                ConsStepper* cP(gctools::ClassAllocator<ConsStepper>::allocateClass(obj.as_or_nil<Cons_O>()));
                this->_Steppers.push_back(cP);
	    } else
	    {
		goto EMPTY;
	    }
	}
	this->_AtEnd = false;
	return;
    EMPTY:
	this->_AtEnd = true;
	return;
    }


    bool test_every_some_notevery_notany(Function_sp predicate, Cons_sp sequences, bool elementTest, bool elementReturn, bool fallThroughReturn )
    {_G();
	ListOfSequenceSteppers steppers(sequences);
	ValueFrame_sp frame(ValueFrame_O::create(steppers.size(),_Nil<ActivationFrame_O>()));
	if ( steppers.atEnd() ) goto FALLTHROUGH; // return elementReturn;
	while (!steppers.atEnd())
	{
	    steppers.fillValueFrameUsingCurrentSteppers(frame);
	    LOG(BF("Applying predicate to elements[%s]") % frame->asString());
	    bool test = eval::applyToActivationFrame(predicate,frame).isTrue();
	    if (test==elementTest)
	    {
		LOG(BF("element test was %d - returning %d") % elementTest % elementReturn );
		return elementReturn;
	    }
	    steppers.advanceSteppers();
	}
	LOG(BF("passed-through - returning %d") % fallThroughReturn );
    FALLTHROUGH:
	return fallThroughReturn;
    }

#define LOCK_af_every 1
#define DOCS_af_every "See CLHS for every"
#define ARGS_af_every "(predicate &rest sequences)"
#define DECL_af_every ""    
    T_mv af_every(T_sp predicate, Cons_sp sequences)
    {_G();
	Function_sp op = coerce::functionDesignator(predicate);
	bool result = test_every_some_notevery_notany(op,sequences,false,false,true);
	return(Values(_lisp->_boolean(result)));
    }


#define LOCK_af_some 1
#define DOCS_af_some "See CLHS for some"
#define ARGS_af_some "(predicate &rest sequences)"
#define DECL_af_some ""    
    T_mv af_some(T_sp predicate, Cons_sp sequences)
    {_G();
	Function_sp op = coerce::functionDesignator(predicate);
	bool result = test_every_some_notevery_notany(op,sequences,true,true,false);
	return(Values(_lisp->_boolean(result)));
    }




#define LOCK_af_notany 1
#define DOCS_af_notany "See CLHS for notany"
#define ARGS_af_notany "(predicate &rest sequences)"
#define DECL_af_notany ""    
    T_mv af_notany(T_sp predicate, Cons_sp sequences)
    {_G();
	Function_sp op = coerce::functionDesignator(predicate);
	bool result = test_every_some_notevery_notany(op,sequences,true,false,true);
	return(Values(_lisp->_boolean(result)));
    }



#define LOCK_af_notevery 1
#define DOCS_af_notevery "See CLHS for notevery"
#define ARGS_af_notevery "(predicate &rest sequences)"
#define DECL_af_notevery ""    
    T_mv af_notevery(T_sp predicate, Cons_sp sequences)
    {_G();
	Function_sp op = coerce::functionDesignator(predicate);
	bool result = test_every_some_notevery_notany(op,sequences,false,true,false);
	return(Values(_lisp->_boolean(result)));
    }





/*
  __BEGIN_DOC(candoScript.general.mapcar)
  __END_DOC
*/
#define LOCK_af_mapcar 1
#define DOCS_af_mapcar "See CLHS for mapcar"
#define ARGS_af_mapcar "(func_desig &rest lists)"
#define DECL_af_mapcar ""    
    SYMBOL_EXPORT_SC_(ClPkg,mapcar);
    List_mv af_mapcar(T_sp func_desig, Cons_sp lists)
    {_G();
	Function_sp func = coerce::functionDesignator(func_desig);
	ListOfListSteppers steppers(lists);
	ValueFrame_sp frame(ValueFrame_O::create(steppers.size(),_Nil<ActivationFrame_O>()));
	ql::list result(_lisp);
	while (!steppers.atEnd())
	{
	    steppers.fillValueFrameUsingCurrentSteppers(frame);
	    T_sp res = eval::applyToActivationFrame(func,frame);
	    result << res;
	    steppers.advanceSteppers();
	}
	return(Values(result.cons()));
    }



/*
  __BEGIN_DOC(candoScript.general.mapcar)
  __END_DOC
*/
#define LOCK_af_mapc 1
#define	DOCS_af_mapc "See CLHS mapc"
#define	ARGS_af_mapc "(op &rest lists)"
#define DECL_af_mapc ""    
    T_mv af_mapc(Function_sp op, Cons_sp lists)
    {_G();
	VectorObjectsWithFillPtr_sp argumentLists(VectorObjectsWithFillPtr_O::make(_Nil<T_O>(),_Nil<Cons_O>(),8,0,true));
	// Copy the arguments into argumentLists
	for ( Cons_sp carg = lists; carg.notnilp(); carg = cCdr(carg))
	{
	    argumentLists->vectorPushExtend(oCar(carg),8);
	}
	Cons_sp result, curResult;
	ValueFrame_sp frame(ValueFrame_O::create(af_length(argumentLists),_Nil<ActivationFrame_O>()));
	while (1)
	{
	    int idx = 0;
	    for ( size_t it(0), itEnd(af_length(argumentLists)); it<itEnd; ++it )
	    {
		if ( argumentLists->operator[](it).nilp() )
		{
		    // We hit a nil - jump to the end
		    goto RETURN;
		}
		frame->set_entry(idx,oCar(argumentLists->operator[](it)));
		argumentLists->operator[](it) = cCdr(argumentLists->operator[](it));
		++idx;
	    }
	    LOG(BF("About to evaluate map op[%s] on arguments[%s]") % _rep_(op) % _rep_(frame) );
	    T_sp res = op->INVOKE(frame->length(),frame->argArray()); // T_sp res = eval::applyFunctionToActivationFrame(op,frame);
	}
    RETURN:
	return(Values(oCar(lists)));
    }




#define	DOCS_af_maplist "See CLHS maplist"
#define ARGS_af_maplist "(func_desig &rest lists)"
#define DECL_af_maplist ""    
    T_mv af_maplist(T_sp func_desig, Cons_sp lists)
    {_G();
	Function_sp op = coerce::functionDesignator(func_desig);
	VectorObjectsWithFillPtr_sp argumentLists(VectorObjectsWithFillPtr_O::make(_Nil<Cons_O>(),_Nil<Cons_O>(),16,0,true));
//	vector<Cons_sp> argumentLists;
	// Copy the arguments into argumentLists
	for ( Cons_sp carg = lists; carg.notnilp(); carg = cCdr(carg))
	{
	    argumentLists->vectorPushExtend(oCar(carg),8);
//	    argumentLists.push_back(oCar(carg).as_or_nil<Cons_O>());
	}
	Cons_sp result, curResult;
	result = Cons_O::create(_Nil<T_O>(),_Nil<Cons_O>());
	ValueFrame_sp frame(ValueFrame_O::create(af_length(argumentLists),_Nil<ActivationFrame_O>()));
	curResult = result;
	while (1)
	{
	    int idx = 0;
	    for ( int it(0), itEnd(af_length(argumentLists)); it<itEnd; ++it )
	    {
		T_sp val = argumentLists->operator[](it);
		if ( val.nilp() ) goto RETURN; // hit nil in arguments - exit
		frame->set_entry(idx,val);
		idx++;
	    }
	    LOG(BF("About to evaluate map op[%s] on arguments[%s]")
		% _rep_(op) % _rep_(frame) );
	    T_sp res = op->INVOKE(frame->length(),frame->argArray()); // T_sp res = eval::applyFunctionToActivationFrame(op,frame);
	    Cons_sp one = Cons_O::create(res);
	    curResult->setCdr(one);
	    curResult = one;
	    // Advance to the next element
	    for ( int it(0), itEnd(af_length(argumentLists)); it<itEnd; ++it )
	    {
		argumentLists->operator[](it) = cCdr(argumentLists->operator[](it));
//		*it = cCdr((*it));
	    }
	}
    RETURN:
	return(Values(cCdr(result)));
    }


#define LOCK_af_mapl 1
#define	DOCS_af_mapl "See CLHS maplist"
#define ARGS_af_mapl "(op &rest lists)"
#define DECL_af_mapl ""    
    T_mv af_mapl(Function_sp op, Cons_sp lists)
    {_G();
	af_maplist(op,lists);
	return(Values(oCar(lists)));
    }





#define LOCK_af_mapappend 1
#define DOCS_af_mapappend "mapappend is like mapcar except that the results are appended together - see AMOP 280"
#define ARGS_af_mapappend "(fun &rest cargs)"
#define DECL_af_mapappend ""    
    T_mv af_mapappend(Function_sp fun, Cons_sp cargs)
    {_G();
	IMPLEMENT_MEF(BF("Fix me - I think I'm broken"));
	T_sp testNull = eval::funcall(cl::_sym_some,cl::_sym_null->symbolFunction(),cargs);
	if ( testNull.nilp() ) return(Values(_Nil<T_O>()));
	T_sp appendHead = eval::funcall(fun,eval::funcall(cl::_sym_mapcar,cl::_sym_car->symbolFunction(),cargs).as_or_nil<Cons_O>());
	T_sp appendTail = eval::funcall(_sym_mapappend,fun,eval::funcall(cl::_sym_mapcar,cl::_sym_cdr->symbolFunction(),cargs).as_or_nil<Cons_O>());
	return eval::funcall(cl::_sym_append,appendHead,appendTail);
    };




    
    
#define ARGS_af_mapcon "(op &rest lists)"
#define DECL_af_mapcon ""
#define DOCS_af_mapcon "mapcon"
    T_mv af_mapcon(T_sp op, Cons_sp lists)
    {_G();
	Cons_sp parts = af_maplist(op,lists).as_or_nil<Cons_O>();
	ValueFrame_sp frame(ValueFrame_O::create(parts,_Nil<ActivationFrame_O>()));
	T_sp result = eval::applyToActivationFrame(cl::_sym_nconc,frame);
	return(Values(result));
    };


#define ARGS_af_mapcan "(op &rest lists)"
#define DECL_af_mapcan ""
#define DOCS_af_mapcan "mapcan"
    T_mv af_mapcan(T_sp op, Cons_sp lists)
    {_G();
	Cons_sp parts = af_mapcar(op,lists).as_or_nil<Cons_O>();
	ValueFrame_sp frame(ValueFrame_O::create(parts,_Nil<ActivationFrame_O>()));
	T_sp result = eval::applyToActivationFrame(cl::_sym_nconc,frame);
	return(Values(result));
    };








#define ARGS_macro_backquote "(form env)"
#define DECL_macro_backquote ""    
#define DOCS_macro_backquote "backquote"
    T_mv macro_backquote(Cons_sp form, Environment_sp env)
    {_G();
	T_sp arg = oCadr(form);
	LOG(BF("Expanding backquote going in: %s") % _rep_(arg) );
	T_mv result = af_backquote_completely_process(arg);
	LOG(BF("Expanded backquote result: %s") % _rep_(result) );
	return(result);
    }




/*!
  Equivalent to Common Lisps append function
  (append a b c)
  It recreates the list structures of the first arguments a and b and strings
  them together into one list and then points the cdr of the last element of this new list
  to c.
*/
#define	ARGS_af_append "(&rest lists)"
#define	DECL_af_append ""
#define DOCS_af_append "append as in clhs"
T_sp af_append(Cons_sp lists)
{_G();
    ql::list list;
    LOG(BF("Carrying out append with arguments: %s") % _rep_(lists) );
    Cons_sp appendArg = lists;
    for ( ; cCdr(appendArg).notnilp(); appendArg = cCdr(appendArg) )
    {
	Cons_sp oneList = oCar(appendArg).as_or_nil<Cons_O>();
	for ( Cons_sp element=oneList; element.notnilp(); element = cCdr(element) )
	{
	    list << oCar(element);
	}
    }
    /* Now append the last argument by setting the new lists last element cdr
       to the last argument of append */
    return(list.dot(oCar(appendArg)).cons());
}





    
    
#define ARGS_af_coerce_to_function "(arg)"
#define DECL_af_coerce_to_function ""
#define DOCS_af_coerce_to_function "coerce_to_function"
    Function_mv af_coerce_to_function(T_sp arg)
    {_G();
	return(Values(coerce::functionDesignator(arg)));
    };



    
    
#define ARGS_af_sequence_start_end "(func sequence start end)"
#define DECL_af_sequence_start_end ""
#define DOCS_af_sequence_start_end "Copied from ecl::sequence.d::sequence_start_end - throws errors if start/end are out of range for the sequence."\
    " I'm not sure what the func argument is for. If end is nil then it is set to the end of the sequence.  Return MultipleValues(start,end,length)."
    T_mv af_sequence_start_end(T_sp func, Sequence_sp sequence, Fixnum_sp start, Fixnum_sp end)
    {_G();
	uint len = af_length(sequence);
	if ( end.nilp() ) end = Fixnum_O::create(len);
	if ( start->get() < 0 )
	{
	    SIMPLE_ERROR(BF("start[%d] must be greater than zero") % _rep_(start));
	}
	if ( end->get() > len )
	{
	    SIMPLE_ERROR(BF("end[%d] must be <= length of sequence[%d]") % _rep_(end) % len );
	}
	Fixnum_sp length = Fixnum_O::create(len);
	if ( end->get() < start->get() )
	{
	    SIMPLE_ERROR(BF("end[%d] is less than start[%d]") % _rep_(end) % _rep_(start) );
	}
	return(Values(start,end,length));
    };




#define ARGS_af_open "(filespec_desig &key (direction :input) element-type if-exists if-does-not-exist (external-format :default))"
#define DECL_af_open ""
#define DOCS_af_open ""
Stream_mv af_open(T_sp filespec_desig, Symbol_sp direction, T_sp element_type, T_sp if_exists, T_sp if_does_not_exist, T_sp external_format )
{_G();
    LOG(BF("filespec_desig = %s") % _rep_(filespec_desig) );
    LOG(BF("direction[%s]") % _rep_(direction) );
    LOG(BF("if_exists[%s]") % _rep_(if_exists));
    LOG(BF("if_does_not_exist[%s]") % _rep_(if_does_not_exist));
    LOG(BF("external_format[%s]") % _rep_(external_format));
    Pathname_sp filespec = af_pathname(filespec_desig);
    if ( direction == kw::_sym_input )
    {
	LOG(BF("status"));
	if ( af_file_kind(filespec) != kw::_sym_file )
	{
	    FILE_ERROR(filespec);
	}
	if ( external_format == kw::_sym_default )
	{
	    LOG(BF("status"));
	    FDInStream_sp fin = FDInStream_O::create(filespec);
	    return(Values(fin));
	    SYMBOL_SC_(KeywordPkg,gzip);
	}
	SIMPLE_ERROR(BF("For file[%s] Bad external-format option: %s") % filespec % _rep_(external_format) );
    } else if ( direction == kw::_sym_output )
    {
	LOG(BF("direction was :output"));
	if ( af_probe_file(filespec).notnilp() )
	{
	    Str_sp truename = af_namestring(af_truename(filespec));
	    LOG(BF("The file[%s] already exists")% truename->get() );
	    SYMBOL_SC_(KeywordPkg,supersede);
	    if ( if_exists.nilp() || if_exists == kw::_sym_supersede)
	    {
		LOG(BF("supersede"));
		// First write output to a temporary file and then rename it to the original on close
		Pathname_sp temporaryFileSpec = af_makePathname(_Nil<T_O>(), // host 
								false, // hostp 
								_Nil<T_O>(), // device 
								false, // devicep 
								_Nil<T_O>(), // directory 
								false, // directoryp 
								_Nil<T_O>(), // name 
								false, // namep 
								Str_O::create(filespec->_Type.as<Str_O>()->get()+"Temp"), //type
								true, // typep 
								_Nil<T_O>(), // version 
								false, // versionp 
								kw::_sym_local, // scase 
								filespec // defaults 
		    );
		if ( external_format == kw::_sym_default)
		{
		    LOG(BF("external_format is :default"));
		    FDOutStream_sp fout = FDOutStream_O::createTemporary(temporaryFileSpec,filespec,std::ios_base::out|std::ios_base::trunc);
		    return(Values(fout));
		}
		SYMBOL_SC_(KeywordPkg,error);
	    } else if ( if_exists == kw::_sym_error )
	    {
		FILE_ERROR(filespec);
		SYMBOL_SC_(KeywordPkg,append);
	    } else if ( if_exists == kw::_sym_append )
	    {
		LOG(BF("if_exists is :append"));
		if ( external_format != kw::_sym_default)
		{
		    SIMPLE_ERROR(BF("You cannot append to a file of external_format[%s]") % _rep_(external_format) );
		}
		LOG(BF("Setting up FDOutStream with append"));
		FDOutStream_sp fout = FDOutStream_O::create(filespec,std::ios_base::ate|std::ios_base::app);
		return(Values(fout));
	    }
	    SIMPLE_ERROR(BF("unknown option[%s] for if-exists") % _rep_(if_exists) );
	} else
	{
	    LOG(BF("File does not exist"));
	    SYMBOL_SC_(KeywordPkg,create);
	    if ( if_does_not_exist.nilp() || if_does_not_exist == kw::_sym_create )
	    {
		LOG(BF("if_does_not_exist is :create"));
		if ( external_format == kw::_sym_default)
		{
		    LOG(BF("external_format is :default"));
		    FDOutStream_sp fout = FDOutStream_O::create(filespec,std::ios_base::out|std::ios_base::trunc);
		    return(Values(fout));
#if 0
		} else if ( external_format == kw::_sym_gzip )
		{
		    LOG(BF("external_format is :gzip"));
		    FileOutCompressedStream_sp fout = FileOutCompressedStream_O::createGzip(filespec);
		    return(Values(fout));
#endif
		}
	    } else if ( if_does_not_exist == kw::_sym_error )
	    {
		FILE_ERROR(filespec);
	    }
	    LOG(BF("falling through to unimplemented"));
	    IMPLEMENT_ME();
	}
    }
    LOG(BF("status"));
    IMPLEMENT_ME();
}


#define ARGS_af_gensym "(&optional x)"
#define DECL_af_gensym ""
#define DOCS_af_gensym "See CLHS gensym"
Symbol_mv af_gensym(T_sp x)
{_G();
    stringstream ss;
    if ( x.nilp() )
    {
	int counter = cl::_sym_STARgensym_counterSTAR->symbolValue().as<Fixnum_O>()->get();
	cl::_sym_STARgensym_counterSTAR->setf_symbolValue(Fixnum_O::create(counter+1));
	ss << "G";
	ss << counter;
    } else if ( af_stringP(x) )
    {
	int counter = cl::_sym_STARgensym_counterSTAR->symbolValue().as<Fixnum_O>()->get();
	cl::_sym_STARgensym_counterSTAR->setf_symbolValue(Fixnum_O::create(counter+1));
	ss << x.as<Str_O>()->get();
	ss << counter;
    } else if ( af_integerP(x) )
    {
	int counter = x.as<Integer_O>()->as_int();
	ASSERTF(counter >=0, BF("gensym argument %d must be >= 0") % counter );
	ss << "G";
	ss << counter;
    } else
    {
	SIMPLE_ERROR(BF("Illegal argument for gensym[%s]") % _rep_(x) );
    }
    Symbol_sp sym = Symbol_O::create(ss.str());
    sym->setPackage(_Nil<Package_O>());
    return(Values(sym));
}









#define ARGS_af_type_to_symbol "(x)"
#define DECL_af_type_to_symbol ""
#define DOCS_af_type_to_symbol "type_to_symbol"
Symbol_mv af_type_to_symbol(T_sp x)
{_G();
    if ( af_characterP(x) ) return(Values(cl::_sym_Character_O));
    else if ( af_fixnumP(x) ) return(Values(cl::_sym_Fixnum_O));
    else if ( af_bignumP(x) ) return(Values(cl::_sym_Bignum_O));
    else if ( af_ratioP(x) ) return(Values(cl::_sym_Ratio_O));
    else if ( af_singleFloatP(x) ) return(Values(cl::_sym_SingleFloat_O));
    else if ( af_doubleFloatP(x) ) return(Values(cl::_sym_DoubleFloat_O));
    else if ( af_longFloatP(x) ) return(Values(cl::_sym_LongFloat_O));
    else if ( af_complexP(x) ) return(Values(cl::_sym_Complex_O));
    else if ( af_symbolp(x) ) return(Values(cl::_sym_Symbol_O));
    else if ( af_packageP(x) ) return(Values(cl::_sym_Package_O));
    else if ( af_listp(x) ) return(Values(cl::_sym_List_O));
    else if ( af_hashTableP(x) ) return(Values(cl::_sym_HashTable_O));
    else if ( af_vectorP(x) ) return(Values(cl::_sym_Vector_O));
    else if ( af_bitVectorP(x) ) return(Values(cl::_sym_BitVector_O));
    else if ( af_arrayP(x) ) return(Values(cl::_sym_Array_O));
    else if ( af_stringP(x) ) return(Values(cl::_sym_String_O));
//    else if ( x.isA<BaseString_O>() ) return(Values(_sym_BaseString_O));
    else if ( cl_streamp(x) ) return(Values(cl::_sym_Stream_O));
    else if ( af_readtableP(x) ) return(Values(cl::_sym_ReadTable_O));
    else if ( CandoException_sp ce = x.asOrNull<CandoException_O>() )
    {
	return(Values(_sym_CandoException_O));
    }
    return Values(x->__class()->className());
    SIMPLE_ERROR(BF("Add af_type_to_symbol support for type: %s") % x->_instanceClass()->classNameAsString() );
}





T_sp type_of(T_sp x)
{_G();
    if ( x.nilp() ) return cl::_sym_null;
#ifdef CLOS
    if ( Instance_sp instance = x.asOrNull<Instance_O>() )
    {
	T_sp cl = lisp_instance_class(instance);
	T_sp t;
        if (Class_sp mcl = cl.asOrNull<Class_O>() )
	{
	    t = mcl->className();
	} else if ( Instance_sp icl = cl.asOrNull<Instance_O>() )
	{
            DEPRECIATEDP("Classes of instances should always be of Class_O type, not Instance_O");
//	    t = icl->_CLASS_NAME();
	} else
	{
	    SIMPLE_ERROR(BF("Illegal class %s for instance class of %s") % _rep_(cl) % _rep_(instance) );
	}
	Symbol_sp st = t.as<Symbol_O>();
	if ( t.nilp() || cl != eval::funcall(cl::_sym_findClass,st,_Nil<T_O>()))
	{
	    t = cl;
	}
	return t;
    } else if ( Class_sp mc = x.asOrNull<Class_O>() )
    {
	Class_sp mcc = lisp_static_class(mc);
	return mcc->className();
    }
    else
#endif
    if ( af_integerP(x) )
    {
	ql::list res(_lisp);
	res << cl::_sym_integer << x << x;
	return res.cons();
    } else if ( af_characterP(x) )
    {
	if ( af_standard_char_p(x.as<Character_O>()) ) return cl::_sym_standard_char;
	return cl::_sym_Character_O;
    } else if ( af_symbolp(x) )
    {
	if ( x == _lisp->_true() ) return cl::_sym_boolean;
	if ( af_keywordP(x.as<Symbol_O>()) ) return cl::_sym_keyword;
	return cl::_sym_symbol;
    } else if ( af_stringP(x) )
    {
	String_sp sx = x.as<String_O>();
	Symbol_sp t;
	if ( sx->adjustable_array_p() || sx->array_has_fill_pointer_p() || sx->_displaced_array_p() )
	{
	    t = cl::_sym_array;
	} else t = cl::_sym_simple_array;
	return (ql::list(_lisp) << t << cl::_sym_base_char << Cons_O::createList(Fixnum_O::create(1),Fixnum_O::create(af_length(sx)))).cons();
    } else if ( af_vectorP(x) )
    {
	Vector_sp vx = x.as<Vector_O>();
	if ( vx->adjustable_array_p() || vx->_displaced_array_p() )
	{
	    return (ql::list(_lisp) << cl::_sym_vector << vx->element_type_as_symbol() << vx->arrayDimensions() ).cons();
	} else if ( vx->array_has_fill_pointer_p() /* || (cl_elttype)x->vector.elttype != aet_object) */ )
	{
	    return (ql::list(_lisp) << cl::_sym_simple_array << vx->element_type_as_symbol() << vx->arrayDimensions() ).cons();
	} else
	{
	    return (ql::list(_lisp) << cl::_sym_simple_vector << Fixnum_O::create(af_length(vx))).cons();
	}
    } else if (af_arrayP(x))
    {
	Symbol_sp t;
	Array_sp ax = x.as<Array_O>();
	if ( ax->adjustable_array_p() || ax->_displaced_array_p() )
	{
	    t = cl::_sym_array;
	} else t = cl::_sym_simple_array;
	return (ql::list(_lisp) << t << ax->element_type_as_symbol() << ax->arrayDimensions() ).cons();
    } else if ( af_bitVectorP(x) )
    {
	BitVector_sp bx = x.as<BitVector_O>();
	Symbol_sp t;
	if ( bx->adjustable_array_p() || bx->array_has_fill_pointer_p() || bx->_displaced_array_p() )
	{
	    t = cl::_sym_array;
	} else t = cl::_sym_simple_array;
	return (ql::list(_lisp) << t << cl::_sym_bit << Cons_O::createList(Fixnum_O::create(1),Fixnum_O::create(af_length(bx)))).cons();
    } else if ( WrappedPointer_sp pp = x.asOrNull<WrappedPointer_O>() ) {
        return pp->_instanceClass()->className();
    } else if ( af_structurep(x) )
    {
	return x.as<StructureObject_O>()->structureType();
    } else if ( cl_streamp(x) )
    {
	if ( x.isA<SynonymStream_O>() ) 		return cl::_sym_SynonymStream_O;
	else if (x.isA<BroadcastStream_O>() ) 		return cl::_sym_BroadcastStream_O;
	else if (x.isA<ConcatenatedStream_O>() ) 	return cl::_sym_ConcatenatedStream_O;
	else if (x.isA<TwoWayStream_O>() ) 		return cl::_sym_TwoWayStream_O;
	else if (x.isA<StringInputStream_O>() ) 	return _sym_StringInputStream_O;
	else if (x.isA<StringOutStream_O>() ) 	return _sym_StringOutStream_O;
	else if (x.isA<EchoStream_O>() ) 		return cl::_sym_EchoStream_O;
	else return cl::_sym_FileStream_O;
    } else if ( af_listp(x) )
    {
	return cl::_sym_cons;
    } else if ( af_pathnamep(x) )
    {
	IMPLEMENT_ME();
#if 0
    case t_pathname:
	t = x->pathname.logical? @'logical-pathname' : @'pathname';
	break;
#endif
    }
    return af_type_to_symbol(x);
}


#define ARGS_af_type_of "(obj)"
#define DECL_af_type_of ""
#define DOCS_af_type_of "type_of"
T_sp af_type_of(T_sp x)
{_G();
    return type_of(x);
}






#define ARGS_af_rem_f "(plist indicator)"
#define DECL_af_rem_f ""
#define DOCS_af_rem_f "Removes the property with the indicator from the property list in place if present and returns MultipleValues with the new property list and T if the property was found"
T_mv af_rem_f(Cons_sp plist, Symbol_sp indicator)
{_G();
    if ( oCar(plist) == indicator )
    {
	plist = cCddr(plist);
	return(Values(plist,_lisp->_true()));
    }
    for ( Cons_sp cur = plist; cCddr(cur).notnilp(); cur = cCddr(cur) )
    {
	T_sp k = oCaddr(cur);
	if ( k == indicator )
	{
	    cCdr(cur)->setCdr(cCddr(cCddr(cur)));
	    return(Values(plist,_lisp->_true()));
	}
    }
    return(Values(plist,_lisp->_false()));
};






#define ARGS_cl_sxhash "(obj)"
#define DECL_cl_sxhash ""
#define DOCS_cl_sxhash "sxhash"
Integer_sp cl_sxhash(T_sp obj)
{_G();
    if ( obj.nilp() ) return Fixnum_O::create(1);

    IMPLEMENT_MEF(BF("Implement sxhash"));
};







// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------




void initialize_primitives()
    {_G();
	//
	// Define functions first because generics and methods depend on some of them
	//
        Defun(allRegisteredClassNames);

	SYMBOL_SC_(CorePkg,smartPointerDetails);
	Defun(smartPointerDetails);
	SYMBOL_EXPORT_SC_(ClPkg,null);
	Defun(null);

	SYMBOL_SC_(CorePkg,STARfset);
	Defun(STARfset);

	SYMBOL_SC_(CorePkg,unbound);
	Defun(unbound);

	SYMBOL_EXPORT_SC_(ClPkg,read);
	Defun(read);

	SYMBOL_EXPORT_SC_(ClPkg,read_preserving_whitespace);
	Defun(read_preserving_whitespace);


	SYMBOL_EXPORT_SC_(ClPkg,read_delimited_list);
	Defun(read_delimited_list);


	SYMBOL_EXPORT_SC_(ClPkg,every);
	Defun(every);


	SYMBOL_EXPORT_SC_(ClPkg,some);
	Defun(some);

	SYMBOL_EXPORT_SC_(ClPkg,notevery);
	Defun(notevery);

	SYMBOL_EXPORT_SC_(ClPkg,notany);
	Defun(notany);


	SYMBOL_EXPORT_SC_(ClPkg,mapcar);
	Defun(mapcar);

	SYMBOL_EXPORT_SC_(ClPkg,mapc);
	Defun(mapc);

	SYMBOL_EXPORT_SC_(ClPkg,maplist);
	Defun(maplist);

	SYMBOL_EXPORT_SC_(ClPkg,mapl);
	Defun(mapl);

	SYMBOL_SC_(CorePkg,mapappend);
	Defun(mapappend);

	SYMBOL_EXPORT_SC_(ClPkg,mapcan);
	Defun(mapcan);

	SYMBOL_EXPORT_SC_(ClPkg,mapcon);
	Defun(mapcon);

	SYMBOL_SC_(CorePkg,macroexpand_default);
	Defun(macroexpand_default);

	SYMBOL_EXPORT_SC_(ClPkg,append);
	Defun(append);


	SYMBOL_EXPORT_SC_(ClPkg,classOf);
	Defun(classOf);

	SYMBOL_SC_(CorePkg,coerce_to_function);
	Defun(coerce_to_function);

	SYMBOL_EXPORT_SC_(ClPkg,identity);
	Defun(identity);

	SYMBOL_EXPORT_SC_(ClPkg,constantp);
	Defun(constantp);

	SYMBOL_SC_(CorePkg,sequence_start_end);
	Defun(sequence_start_end);

	SYMBOL_EXPORT_SC_(ClPkg,open);
	Defun(open);

	SYMBOL_EXPORT_SC_(ClPkg,ash);
	Defun(ash);

	SYMBOL_SC_(CorePkg,type_to_symbol);
	Defun(type_to_symbol);

	SYMBOL_SC_(CorePkg,gdb);
	Defun(gdb);
	Defun(break);
	SYMBOL_SC_(CorePkg,gdbInspect);
	Defun(gdbInspect);

	defmacro(CorePkg,"backquote",&macro_backquote,ARGS_macro_backquote,DECL_macro_backquote,DOCS_macro_backquote);

	SYMBOL_EXPORT_SC_(ClPkg,gensym);
	Defun(gensym);

	SYMBOL_EXPORT_SC_(ClPkg,type_of);
	Defun(type_of);

	SYMBOL_SC_(CorePkg,rem_f);
	Defun(rem_f);

	SYMBOL_EXPORT_SC_(ClPkg,specialOperatorP);
	Defun(specialOperatorP);

	SYMBOL_EXPORT_SC_(ClPkg,macroFunction);
	Defun(macroFunction);

	SYMBOL_SC_(CorePkg,separatePairList);
	Defun(separatePairList);

	SYMBOL_EXPORT_SC_(ClPkg,makeStringOutputStream);
	Defun(makeStringOutputStream);

	SYMBOL_EXPORT_SC_(ClPkg,getOutputStreamString);
	Defun(getOutputStreamString);

        SYMBOL_EXPORT_SC_(ClPkg,set);
        ClDefun(set);

	SYMBOL_SC_(CorePkg,testMemoryError);
	Defun(testMemoryError);

	SYMBOL_SC_(CorePkg,functionBlockName);
	Defun(functionBlockName);

	SYMBOL_SC_(CorePkg,validFunctionNameP);
	Defun(validFunctionNameP);

	SYMBOL_EXPORT_SC_(ClPkg,fdefinition);
	Defun(fdefinition);

	SYMBOL_EXPORT_SC_(ClPkg,fboundp);
	Defun(fboundp);

	SYMBOL_EXPORT_SC_(ClPkg,fmakunbound);
	Defun(fmakunbound);

	SYMBOL_EXPORT_SC_(ClPkg,values);
	Defun(values);
	SYMBOL_EXPORT_SC_(ClPkg,values_list);
	Defun(values_list);

	Defun(isTrue);

	SYMBOL_EXPORT_SC_(CorePkg,pointer);
	Defun(pointer);

	SYMBOL_EXPORT_SC_(ExtPkg,getEnv);
	Defun(getEnv);

        Defun(exceptionStackDump);
	SYMBOL_EXPORT_SC_(CorePkg,toTaggedFixnum);
	SYMBOL_EXPORT_SC_(CorePkg,fromTaggedFixnum);
	SYMBOL_EXPORT_SC_(CorePkg,dumpTaggedFixnum);
	Defun(toTaggedFixnum);
	Defun(fromTaggedFixnum);
	Defun(dumpTaggedFixnum);
        Defun(testBasePointerConversion);
        Defun(dumpAddressOf);
        Defun(incompleteNextHigherPowerOf_2);
        Defun(argc);
        Defun(argv);
    }


    void initializePythonPrimitives(Lisp_sp lisp)
    {_G();
#if 0
	using namespace boost::python;
	def_raw(CorePkg,"read",&fn_read,ARGS_fn_read,DOCS_fn_read,_LISP);
#if 0
	def_raw(CorePkg,"readDelimitedList",&fn_read_delimited_list,ARGS_fn_read_delimited_list,DOCS_fn_read_delimited_list,_LISP);
#endif
#endif
    }



}; /* core */
