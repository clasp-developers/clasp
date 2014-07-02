#define	DEBUG_LEVEL_FULL
#include "foundation.h"
#include "executables.h"
#include "lisp.h"
#include "str.h"
#include "symbolTable.h"
#include "standardObject.h"
#include "package.h"
//#include "debugger.h"
#include "iterator.h"
#include "primitives.h"
#include "vectorObjects.h"
#include "sourceFileInfo.h"
#include "activationFrame.h"
#include "lambdaListHandler.h"
//#i n c l u d e "environmentDependent.h"
#include "environment.h"
#include "evaluator.h"
// to avoid Generic to_object include headers here
#include "wrappers.h"



namespace core
{




    EXPOSE_CLASS(core,Function_O);
//    EXPOSE_CLASS(core,PrimitiveWithArguments_O);
//    EXPOSE_CLASS(core,FunctionPrimitive_O);
//    REGISTER_CLASS(core,MacroPrimitive_O);
//    REGISTER_CLASS(core,MethodPrimitive_O);
    EXPOSE_CLASS(core,Interpreted_O);
//    REGISTER_CLASS(core,MethodO_O);

    SYMBOL_EXPORT_SC_(KeywordPkg,calledFunction);
    SYMBOL_EXPORT_SC_(KeywordPkg,givenNumberOfArguments);
    SYMBOL_EXPORT_SC_(KeywordPkg,requiredNumberOfArguments);
    SYMBOL_EXPORT_SC_(KeywordPkg,unrecognizedKeyword);


    void handleArgumentHandlingExceptions(Function_sp func)
    {
        Function_sp localFunc = func;
        try {
            throw;
        } catch (TooManyArgumentsError& error) {
            lisp_error(core::_sym_tooManyArgumentsError
                       , lisp_createList(kw::_sym_calledFunction,func
                                         , kw::_sym_givenNumberOfArguments,Fixnum_O::create(error.givenNumberOfArguments)
                                         , kw::_sym_requiredNumberOfArguments,Fixnum_O::create(error.requiredNumberOfArguments)
                           ));
        } catch (TooFewArgumentsError& error) {
            lisp_error(core::_sym_tooFewArgumentsError
                       , lisp_createList(kw::_sym_calledFunction,func
                                         , kw::_sym_givenNumberOfArguments,Fixnum_O::create(error.givenNumberOfArguments)
                                         , kw::_sym_requiredNumberOfArguments,Fixnum_O::create(error.requiredNumberOfArguments)
                           ));
        } catch (UnrecognizedKeywordArgumentError& error ) {
            lisp_error(core::_sym_unrecognizedKeywordArgumentError
                       , lisp_createList(kw::_sym_calledFunction, func
                                         , kw::_sym_unrecognizedKeyword, error.argument));
        }
    }


    
    
#define ARGS_cl_functionLambdaExpression "(fn)"
#define DECL_cl_functionLambdaExpression ""
#define DOCS_cl_functionLambdaExpression "functionLambdaExpression"
    T_mv cl_functionLambdaExpression(Function_sp fn)
    {_G();
	if ( fn.nilp() ) {
	    WRONG_TYPE_ARG(fn,cl::_sym_Function_O);
	}
	Cons_sp code = _Nil<Cons_O>();
	if ( Interpreted_sp ifn = fn.asOrNull<Interpreted_O>() ) {
	    code = ifn->getCode();
	}
	bool closedp = fn->closedEnvironment().notnilp();
	T_sp name = fn->functionName();
	return Values(code,_lisp->_boolean(closedp),name);
    };





    Environment_sp Function_O::closedEnvironment() const
    {
	SUBIMP();
    }





    T_mv Function_O::functionFile() const
    {
	string pathName = "-no-file-";
	if ( this->_SourceFileInfo.notnilp() ) {
	    pathName =  this->_SourceFileInfo->namestring();
	}
	return Values(Str_O::create(pathName),Fixnum_O::create(this->_LineNumber));
    }


    SourceFileInfo_sp Function_O::sourceFileInfo() const
    {
	return this->_SourceFileInfo;
    }


    bool Function_O::macroP() const
    {
	return (this->_Kind == kw::_sym_macro );
    }




    void Function_O::set_kind(Symbol_sp kind)
    {_OF();
	ASSERTF(kind==kw::_sym_function || kind==kw::_sym_macro,BF("Only :function or :macro are allowed"));
	this->_Kind = kind;
    }





    void Function_O::exposeCando(Lisp_sp lisp)
    {
	class_<Function_O>()
	    .def("getLambdaListHandler",&Function_O::getLambdaListHandler)
	    .def("macrop",&Function_O::macroP)
	    .def("set-kind",&Function_O::set_kind)
	    .def("functionKind",&Function_O::functionKind)
	    .def("closedEnvironment",&Function_O::closedEnvironment)
	    .def("functionName",&Function_O::getFunctionName)
	    .def("functionFile",&Function_O::functionFile)
	    ;
	ClDefun(functionLambdaExpression);
    }

    void Function_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,Function,"","",_lisp)
	    ;
#endif
    }



    string Function_O::__repr__() const
    {_G();
	ASSERTNOTNULL(this->_Name);
	T_sp name = this->_Name;
	stringstream ss;
	ss << "#<" << this->_instanceClass()->classNameAsString() << " " << _rep_(name) << ">";
	return ss.str();
    }



#if defined(XML_ARCHIVE)
    void Function_O::archiveBase(ArchiveP node)
    {
	this->Base::archiveBase(node);
	node->archiveWeakPointer("weakName",this->_WeakName);
    }
#endif // defined(XML_ARCHIVE)

    void Function_O::setFunctionName(T_sp s)
    {
	this->_Name = s;
    }

    T_sp Function_O::getFunctionName() const
    {_OF();
	ASSERTNOTNULL(this->_Name);
	if ( this->_Name.nilp() ) return _Nil<T_O>();
	return this->_Name;
    }





    void Interpreted_O::exposeCando(Lisp_sp lisp)
    {
	class_<Interpreted_O>()
	    .def("getCode",&Interpreted_O::getCode)
	    ;
	af_def(CorePkg,"makeInterpreted",&Interpreted_O::make);
    }

    void Interpreted_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,Interpreted,"","",_lisp)
	    ;
#endif
    }



    Interpreted_O::Interpreted_O() : Base()
                                   , _ClosedEnvironment(_Nil<Environment_O>())
                                   , _DocString(_Nil<Str_O>())
                                   , _Declares(_Nil<Cons_O>())
                                   , _Code(_Nil<Cons_O>()) {};


    Interpreted_sp	Interpreted_O::create( T_sp name,
					       LambdaListHandler_sp lambda_list_handler,
					       Cons_sp declares,
					       Str_sp docString,
					       Cons_sp code,
					       Environment_sp environ,
					       Symbol_sp kind)
    {_G();
	Interpreted_sp	proc;
	LOG(BF("Creating a function named: %s")% _rep_(name));
	proc = Interpreted_O::create();
	proc->_Name = name;
	proc->_LambdaListHandler = lambda_list_handler;
	proc->_Declares = declares;
	proc->_DocString = docString;
	proc->_Declares = declares;
	proc->_Code = code;
	proc->_ClosedEnvironment = environ;
	SourceFileInfo_mv sfi = af_walkToFindSourceInfo(code);
	if ( sfi.number_of_values() > 3 ) {
	    proc->_SourceFileInfo = sfi;
	    proc->_LineNumber = sfi.valueGet(1).as<Fixnum_O>()->get();
	    proc->_Column = sfi.valueGet(2).as<Fixnum_O>()->get();
	} else {
//	    IMPLEMENT_MEF(BF("Why don't I have source code????"));
	    proc->_SourceFileInfo = _Nil<SourceFileInfo_O>();
	    proc->_LineNumber = 0;
	    proc->_Column = 0;
	}
	proc->_Kind = kind;
	LOG(BF("Created procedure with name: %s")% _rep_(proc->getFunctionName()) );
	LOG(BF("  Its position in memory: %lX")% proc.get() );
	LOG(BF("  Its className() = %s")% proc->__class()->className() );
	return proc;
    }




    Interpreted_sp	Interpreted_O::make( T_sp functionName,
				  LambdaListHandler_sp llh,
					     Cons_sp code)
    {_G();
	Interpreted_sp proc = Interpreted_O::create(functionName,
						    llh,
						    _Nil<Cons_O>(),
						    _Nil<Str_O>(),
						    code,
						    _Nil<Environment_O>(),
						    kw::_sym_function );
	return proc;
    }









#if defined(XML_ARCHIVE)
    void Interpreted_O::archiveBase(ArchiveP node)
    {
	this->Base::archiveBase(node);
	node->archiveObject("lambdaListHandler",this->_LambdaListHandler);
	node->attributeIfNotNil("docString",this->_DocString);
	node->attributeIfNotNil("declares",this->_Declares);
	node->attributeIfNotNil("code",this->_Code);
	IMPLEMENT_MEF(BF("Handle kind better"));
	node->attribute("kind",this->_Kind);
    }
#endif // defined(XML_ARCHIVE)


    string Interpreted_O::__repr__() const
    {
	stringstream ss;
	ss << "#<" << this->_instanceClass()->classNameAsString() << " :name " << _rep_(this->getFunctionName());
	ss << " :llh " << _rep_(this->_LambdaListHandler);
#if 0
	if ( !this->_ClosedEnvironment.nilp() )
	{
	    ss << " :environment " << _rep_(this->_ClosedEnvironment);
	} else
	{
	    ss << " :environment nil ";
	}
#else
        ss << ":environment <MAY-OVERLOAD-STACK>";
#endif
	ss << " :declares " << _rep_(this->_Declares);
	ss << " :docstring \"" << _rep_(this->_DocString) << "\" ";
#if 1
        ss << ":code <NOT-PRINTING-COULD-OVERLOAD-STACK>";
#else
	ss << " :code " << _rep_(this->_Code);
#endif
	ss << " :sourceFileInfo " << _rep_(this->_SourceFileInfo);
	ss << " :lineNumber " << this->_LineNumber;
	ss << " :column " << this->_Column;
	ss << " >";
	return ss.str();
    }


    Environment_sp Interpreted_O::closedEnvironment() const
    {_OF();
	ASSERTNOTNULL(this->_ClosedEnvironment);
	return this->_ClosedEnvironment;
    }





    void Interpreted_O::closeOverEnvironment(Environment_sp environ)
    {_G();
	ASSERTNOTNULL(environ);
	this->_ClosedEnvironment = environ;
    }



    // This is the old way of doing things using createBindingsInEnvironment

    T_mv Interpreted_O::INVOKE(int nargs, ArgArray args )
    {_G();
        if ( _sym_STARdebugInterpretedFunctionsSTAR && _sym_STARdebugInterpretedFunctionsSTAR->symbolValue().notnilp() ) {
//            printf("%s:%d   _sym_STARdebugInterpretedFunctionsSTAR->symbolValue().px_ref() = %p\n", __FILE__, __LINE__, _sym_STARdebugInterpretedFunctionsSTAR->symbolValue().px_ref() );
            printf("%s:%d  Trapped an InterpretedFunction call: %s  code:\n%s\n", __FILE__, __LINE__, _rep_(this->_Name).c_str(), _rep_(this->_Code).c_str());
        }
	T_mv result;
	LambdaListHandler_sp lambdaListHandler = this->getLambdaListHandler();
	ValueEnvironment_sp newValueEnvironment = ValueEnvironment_O::createForLambdaListHandler(lambdaListHandler,this->closedEnvironment());
	ValueEnvironmentDynamicScopeManager scope(newValueEnvironment);
	// I used to use createBindingsInEnvironment but I switched to createBindingInActivationFrame
        try {
            lambdaListHandler->createBindingsInScope_argArray(nargs,args,scope);
        } catch (...) {
            handleArgumentHandlingExceptions(this->asSmartPtr());
        }
	LOG(BF("About to evaluate the code bound to symbol[%s] in the environment->\n%s")
	    % _rep_(this->getFunctionName()) % _rep_(newValueEnvironment) );
	LOG(BF("About to evaluate code: %s")%_rep_(this->_Code) );
	if ( af_consP(this->_Code) )
	{
	    ValueFrame_sp newActivationFrame = newValueEnvironment->getActivationFrame().as<ValueFrame_O>();
	    VectorObjects_sp debuggingInfo = lambdaListHandler->namesOfLexicalVariablesForDebugging();
	    newActivationFrame->attachDebuggingInfo(debuggingInfo);
	    LispInterpretedFunctionIHF _frame(_lisp->invocationHistoryStack(),this->sharedThis<Interpreted_O>(),newActivationFrame);
            result = eval::sp_progn(this->_Code,newValueEnvironment);
	} else
	{
	    SIMPLE_ERROR(BF("What do we do now, this->_Code = %s") % _rep_(this->_Code) );
	}
	LOG(BF("Returning result: %s") % _rep_(result) );
	return(result);
    }












//------------------------------------------------------------
//
// BuiltIn
//
//




    BuiltIn_sp	BuiltIn_O::create( T_sp name,
				    LambdaListHandler_sp lambda_list_handler,
				    CompiledBody_sp code,
				    ActivationFrame_sp environment,
				    Symbol_sp kind )
    {_G();
	BuiltIn_sp proc(BuiltIn_O::create());
	LOG(BF("Creating a function named: %s")% _rep_(name) );
	proc->_Name = name;
	proc->_LambdaListHandler = lambda_list_handler;
	proc->_Body = code;
	proc->_ClosedEnvironment = environment;
	proc->_Kind = kind;
	LOG(BF("Created procedure with name: %s")% _rep_(proc->getFunctionName()) );
	LOG(BF("  Its position in memory: %lX")% proc.get() );
	LOG(BF("  Its className() = %s")% proc->__class()->className() );
	return proc;
    }







    BuiltIn_sp BuiltIn_O::create_single_dispatch_function( T_sp functionName,
							   LambdaListHandler_sp lambdaList,
							   CompiledBody_sp code )
    {_G();
	LOG(BF("Creating a function named: %s")% _rep_(functionName) );
	BuiltIn_sp proc = BuiltIn_O::create();
	if ( lambdaList.nilp() ) {
	    IMPLEMENT_MEF(BF("LambdaListHandler should never be nil"));
	}
	proc->set_kind(kw::_sym_function);
	proc->setFunctionName(functionName);
	proc->setLambdaListHandler(lambdaList);
	proc->_Body = code;
	proc->_ClosedEnvironment = _Nil<ActivationFrame_O>();
	if ( code.nilp() )
	{
	    SIMPLE_ERROR(BF("The procedure code is Nil!!!!"));
	}
	LOG(BF("Created procedure with name: %s")% _rep_(proc->getFunctionName()) );
	LOG(BF("  Its position in memory: %lX")% proc.get() );
	LOG(BF("  Its className() = %s")% af_classOf(proc)->className() );
	return proc;
    }










    BuiltIn_sp BuiltIn_O::make(T_sp functionName, LambdaListHandler_sp llh, CompiledBody_sp body)
    {_G();
	BuiltIn_sp cf(BuiltIn_O::create(functionName,
					llh,
					body,
					_Nil<ActivationFrame_O>(),
					kw::_sym_function));
	return cf;
    }


#if 0
    // What do I do here - if I have a LambdaListHandler or I don't
    // I still have to do argument conversions - WHAT DO I DO????????
    T_mv BuiltIn_O::FUNCALL(int n_args, ... )
    {_G();
	T_mv result;
	Functoid* functoid = this->_Body->functoid();
	// This is where we take the evaluated arguments that we were passed and
	// give them to the LambdaListHandler to fill in to another ActivationFrame with
	// optional defaults, key argument defaults and auxs 
	va_list ap;
	va_start(ap,n_args);
	if ( !this->_LambdaListHandler.nilp() )
	{
	    ValueFrame_sp activationFrame(ValueFrame_O::createForLambdaListHandler(this->_LambdaListHandler,_Nil<ActivationFrame_O>()));
	    ActivationFrameDynamicScopeManager scope(activationFrame);
	    this->_LambdaListHandler->createBindingsInScope(ap,scope);
	    activationFrame->attachDebuggingInfo(this->_LambdaListHandler->namesOfLexicalVariablesForDebugging());
	    LOG(BF("Populated activation_frame: %s") % _rep_(activationFrame) );
	    result = functoid->activate(activationFrame);
	} else
	{
	    // If there is no LambdaListHandler then pass the given activation frame on to the functoid
	    result = functoid->activate(n_args,ap);
	}
	va_end(ap);
	LOG(BF("Returning result: %s") % _rep_(result) );
	if ( !result )
	{
	    SIMPLE_ERROR(BF("Error: UNDEFINED result from INVOKE of function[%s] on arguments[%s]") % _rep_(this->getFunctionName()) %  _rep_(args) );
	}
	LOG(BF("Returning result: %s") % _rep_(result) );
	return(result);
    }
#endif



//#define DEBUG_BUILTIN_INVOKE

    T_mv BuiltIn_O::INVOKE(int nargs, ArgArray argArray)
    {_G();
	T_mv result;
	LOG(BF("The body is a CompiledBody"));
	Functoid* functoid = this->_Body->functoid();
	// This is where we take the evaluated arguments that we were passed and
	// give them to the LambdaListHandler to fill in to another ActivationFrame with
	// optional defaults, key argument defaults and auxs 
	if ( !this->_LambdaListHandler.nilp() )
	{
	    LambdaListHandler_sp llh = this->_LambdaListHandler;
#ifdef DEBUG_BUILTIN_INVOKE
            printf("%s:%d Builtin::INVOKE fn[%s]  ----------------------    incoming nargs=%d\n", __FILE__, __LINE__, _rep_(this->_Name).c_str(), nargs);
            for ( int zzz(0); zzz<nargs; ++zzz ) {
                printf("%s:%d    [%d] --> %s\n", __FILE__, __LINE__,
                       zzz, _rep_(argArray[zzz]).c_str() );
            }
            printf("%s:%d BuiltIn_O::INVOKE  lambdaListHandler = %s\n", __FILE__, __LINE__, _rep_(llh).c_str());
#endif
	    ValueFrame_sp expandedArgs(ValueFrame_O::createForLambdaListHandler(this->_LambdaListHandler,_Nil<ActivationFrame_O>()));
	    expandedArgs->attachDebuggingInfo(this->_LambdaListHandler->namesOfLexicalVariablesForDebugging());
	    ActivationFrameDynamicScopeManager scope(expandedArgs);
            try {
                this->_LambdaListHandler->createBindingsInScope_argArray(nargs,argArray,scope);
            } catch (...) {
                handleArgumentHandlingExceptions(this->asSmartPtr());
            }
	    LOG(BF("Populated activation_frame: %s") % _rep_(expandedArgs) );
#if 1
	    CxxFunctionIHF _frame(_lisp->invocationHistoryStack(),this->sharedThis<BuiltIn_O>());
#else
	    CxxFunctionIHF _frame(_lisp->invocationHistoryStack(),this->sharedThis<BuiltIn_O>(),expandedArgs->length(),expandedArgs->argArray());
#endif

	    // BuiltIn functions run in the nil lexical environment
#ifdef DEBUG_BUILTIN_INVOKE
            printf("%s:%d  functoid->activate fn[%s]  %d\n", __FILE__, __LINE__,
                   _rep_(this->_Name).c_str(),
                   expandedArgs->length() );
            for ( int zzz(0); zzz<expandedArgs->length(); ++zzz ) {
                printf("%s:%d    [%d] --> %s\n", __FILE__, __LINE__,
                       zzz, _rep_(expandedArgs->argArray()[zzz]).c_str() );
            }
#endif
	    result = functoid->activate(_Nil<ActivationFrame_O>(),expandedArgs->length(),expandedArgs->argArray());
	} else
	{
#if 1
	    CxxFunctionIHF _frame(_lisp->invocationHistoryStack(),this->sharedThis<BuiltIn_O>());
#else
	    CxxFunctionIHF _frame(_lisp->invocationHistoryStack(),this->sharedThis<BuiltIn_O>(),nargs,argArray);
#endif
	    // If there is no LambdaListHandler then pass the given activation frame on to the functoid
	    /* TODO: Create an ActivationFrame that is completely stored on the stack and use it here */
            try {
                result = functoid->activate(_Nil<ActivationFrame_O>(),nargs,argArray);
            } catch (...) {
                handleArgumentHandlingExceptions(this->asSmartPtr());
            }
	}
	LOG(BF("Returning result: %s") % _rep_(result) );
	if ( !result )
	{
	    SIMPLE_ERROR(BF("Error: UNDEFINED result from INVOKE of function[%s]") % _rep_(this->getFunctionName()) );
	}
	LOG(BF("Returning result: %s") % _rep_(result) );
	return(result);
    }



    Environment_sp BuiltIn_O::closedEnvironment() const
    {
	return this->_ClosedEnvironment;
    }



    string BuiltIn_O::__repr__() const
    {_G();
	stringstream ss;
	ss << "#<" << this->_instanceClass()->classNameAsString() << " ";
	ss << _rep_(this->getFunctionName()) << " ";
	ss << ">";
	return ss.str();
    }

    EXPOSE_CLASS(core,BuiltIn_O);

    void BuiltIn_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<BuiltIn_O>()
	    ;
	af_def(CorePkg,"makeBuiltIn",&BuiltIn_O::make);
    }

    void BuiltIn_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,BuiltIn,"","",_lisp)
	    ;
#endif
    }






    CompiledFunction_sp CompiledFunction_O::makeCompiledFunction(T_sp functionName,
								 CompiledBody_sp code,
								 ActivationFrame_sp closedEnvironment,
								 SourceFileInfo_sp sourceFileInfo,
								 uint lineNumber,
								 uint column,
								 Symbol_sp functionKind )
    {_G();
	CompiledFunction_sp cl(CompiledFunction_O::create());
	cl->_Name = functionName;
	cl->_ClosedEnvironment = closedEnvironment;
	cl->_Body = code;
	ASSERTF(af_keywordP(functionKind),BF("The function kind must be a keyword symbol! - you passed %s") % _rep_(functionKind) );
	cl->_Kind = functionKind;
	cl->_SourceFileInfo = sourceFileInfo;
	cl->_LineNumber = lineNumber;
	cl->_Column = column;
	cl->_LambdaListHandler = _Nil<LambdaListHandler_O>();
	return cl;
    }


#if 0
    T_mv CompiledFunction_O::FUNCALL(int n_args, ... )
    {_G();
	af_stackMonitor();
	T_mv result;
	LispCompiledFunctionIHF _frame(_lisp->invocationHistoryStack(),this->sharedThis<CompiledFunction_O>());
	LOG(BF("The body is a CompiledBody"));
	Functoid* functoid = this->_Body->functoid();
	IMPLEMENT_MEF(BF("What do I do here???"));
#if FUNCALL
	args->setParentFrame(this->_ClosedEnvironment);
#endif
	IMPLEMENT_MEF(BF("WHAT DO I DO HERE???"));
#if FUNCALL
	result = functoid->activate(args);
#endif
	ASSERTF(result,BF("Error: UNDEFINED result from INVOKE of function[%s]") % _rep_(this->getFunctionName()) );
	return(result);
    }
#endif



    T_mv CompiledFunction_O::INVOKE(int nargs, ArgArray args)
    {_G();
	af_stackMonitor();
	T_mv result;
	LispCompiledFunctionIHF _frame(_lisp->invocationHistoryStack(),this->sharedThis<CompiledFunction_O>());
	LOG(BF("The body is a CompiledBody"));
	Functoid* functoid = this->_Body->functoid();
//	args->setParentFrame(this->_ClosedEnvironment);
        try {
            result = functoid->activate(this->_ClosedEnvironment,nargs,args);
        } catch (...) {
            handleArgumentHandlingExceptions(this->asSmartPtr());
        }
	ASSERTF(result,BF("Error: UNDEFINED result from INVOKE of function[%s]") % _rep_(this->getFunctionName()));
	return(result);
    }


    string CompiledFunction_O::__repr__() const
    {_G();
	stringstream ss;
	ss << "#<" << this->_instanceClass()->classNameAsString() << " ";
	ss << ":name " << _rep_(this->_Name) << " ";
	ss << ":body " << _rep_(this->_Body) << " ";
        ss << ":source-file-info " << _rep_(this->_SourceFileInfo) << " ";
        ss << ":lineno " << this->_LineNumber << " ";
        ss << ":column " << this->_Column << " >";
	return ss.str();
    }


    Environment_sp CompiledFunction_O::closedEnvironment() const
    {
	return this->_ClosedEnvironment;
    }




    
    EXPOSE_CLASS(core,CompiledFunction_O);
    
    void CompiledFunction_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<CompiledFunction_O>()
	    .def("getBody",&CompiledFunction_O::getBody)
	    ;
    }
    
    void CompiledFunction_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,CompiledFunction,"","",_lisp)
	    ;
#endif
    }



};




