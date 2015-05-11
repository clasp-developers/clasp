/*
    File: executables.cc
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
#include <clasp/core/foundation.h>
#include <clasp/core/executables.h>
#include <clasp/core/lisp.h>
#include <clasp/core/str.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/standardObject.h>
#include <clasp/core/package.h>
//#include "debugger.h"
#include <clasp/core/iterator.h>
#include <clasp/core/designators.h>
#include <clasp/core/primitives.h>
#include <clasp/core/vectorObjects.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/activationFrame.h>
#include <clasp/core/lambdaListHandler.h>
//#i n c l u d e "environmentDependent.h"
#include <clasp/core/environment.h>
#include <clasp/core/evaluator.h>
// to avoid Generic to_object include headers here
#include <clasp/core/wrappers.h>



namespace core
{


    
#define ARGS_core_functionLambdaList "(function)"
#define DECL_core_functionLambdaList ""
#define DOCS_core_functionLambdaList "functionLambdaList"
    T_mv core_functionLambdaList(T_sp obj)
    {
	if ( obj.nilp() ) {
	    return Values(_Nil<T_O>(),_Nil<T_O>());
	} else if ( Symbol_sp sym = obj.asOrNull<Symbol_O>() ) {
	    if (!sym->fboundp()) {
		return Values(_Nil<T_O>(),_Nil<T_O>());
	    }
	    Function_sp fn = sym->symbolFunction();
	    return Values(fn->lambdaList(),_lisp->_true());
	} else if ( Function_sp func = obj.asOrNull<Function_O>() ) {
	    return Values(func->lambdaList(),_lisp->_true());
	}
	return Values(_Nil<T_O>(),_Nil<T_O>());
    }


    
#define ARGS_core_functionSourcePosInfo "(function)"
#define DECL_core_functionSourcePosInfo ""
#define DOCS_core_functionSourcePosInfo "functionSourcePosInfo"
    gc::Nilable<SourcePosInfo_sp> core_functionSourcePosInfo(T_sp functionDesignator)
    {
	Function_sp func = coerce::functionDesignator(functionDesignator);
	gctools::tagged_functor<Closure> closure = func->closure;
	gc::Nilable<SourcePosInfo_sp> sourcePosInfo = closure->sourcePosInfo();
	return sourcePosInfo;
    }


    
    
#define ARGS_core_setKind "(fn kind)"
#define DECL_core_setKind ""
#define DOCS_core_setKind "setKind"
#define FILE_core_setKind __FILE__
#define LINE_core_setKind __LINE__
    void core_setKind(Function_sp fn, Symbol_sp kind)
    {
        fn->closure->setKind(kind);
    };


    bool FunctionClosure::macroP() const
    {
        return this->kind == kw::_sym_macro;
    }
    int FunctionClosure::sourceFileInfoHandle() const {
	if ( this->_SourcePosInfo.notnilp() ) {
	    return this->_SourcePosInfo.as<SourcePosInfo_O>()->fileHandle();
	}
	return 0;
    };

    size_t FunctionClosure::filePos() const { return this->_SourcePosInfo.notnilp() ? this->_SourcePosInfo.as<SourcePosInfo_O>()->filepos() : 0; };

    int FunctionClosure::lineNumber() const { return this->_SourcePosInfo.notnilp() ? this->_SourcePosInfo.as<SourcePosInfo_O>()->lineno() : 0; };
    int FunctionClosure::column() const { return this->_SourcePosInfo.notnilp() ? this->_SourcePosInfo.as<SourcePosInfo_O>()->column() : 0; };


T_sp FunctionClosure::setSourcePosInfo(T_sp sourceFile, size_t filePos, int lineno, int column )
    {
        SourceFileInfo_mv sfi = core_sourceFileInfo(sourceFile);
        Fixnum_sp fileId = sfi.valueGet(1).as<Fixnum_O>();
        SourcePosInfo_sp spi = SourcePosInfo_O::create(fileId->get(),filePos,lineno,column);
        this->_SourcePosInfo = spi;
        return spi;
    }


    T_sp BuiltinClosure::lambdaList() const
    {
	return this->_lambdaListHandler->lambdaList();
    }
    
    void BuiltinClosure::LISP_CALLING_CONVENTION()
    {
        IMPLEMENT_MEF(BF("Handle call to BuiltinClosure"));
    };

    
    InterpretedClosure::InterpretedClosure(T_sp fn, T_sp sp, Symbol_sp k
					   , LambdaListHandler_sp llh, List_sp dec, T_sp doc
					   , T_sp e, List_sp c)
            : FunctionClosure(fn,sp,k,e)
            , _lambdaListHandler(llh)
            , _declares(dec)
            , _docstring(doc)
            , _code(c)
    {
	if ( sp.nilp() ) {
	    sp = core::_sym_STARcurrentSourcePosInfoSTAR->symbolValue().as<SourcePosInfo_O>();
	    if ( sp.nilp() ) {
		printf("%s:%d Caught creation of InterpretedClosure %s with nil SourcePosInfo\n", __FILE__,__LINE__,_rep_(fn).c_str());
	    } else {
		this->_SourcePosInfo = sp;
	    }
	}
    };


    T_sp InterpretedClosure::lambdaList() const
    {
	return this->lambdaListHandler()->lambdaList();
    }

    void InterpretedClosure::LISP_CALLING_CONVENTION()
    {
        ValueEnvironment_sp newValueEnvironment = ValueEnvironment_O::createForLambdaListHandler(this->_lambdaListHandler,this->closedEnvironment);
        ValueEnvironmentDynamicScopeManager scope(newValueEnvironment);
	InvocationHistoryFrame _frame(this);
        lambdaListHandler_createBindings(this,this->_lambdaListHandler,scope,LCC_PASS_ARGS);
        ValueFrame_sp newActivationFrame = newValueEnvironment->getActivationFrame().as<ValueFrame_O>();
        VectorObjects_sp debuggingInfo = _lambdaListHandler->namesOfLexicalVariablesForDebugging();
        newActivationFrame->attachDebuggingInfo(debuggingInfo);
	//        InvocationHistoryFrame _frame(this,newActivationFrame);
	_frame.setActivationFrame(newActivationFrame);
#if 0
	if (_sym_STARdebugInterpretedClosureSTAR->symbolValue().notnilp()) {
	    printf("%s:%d Entering InterpretedClosure   source file = %s  lineno=%d\n", __FILE__, __LINE__, _frame.sourcePathName().c_str(), _frame.lineno());
	}
#endif
        *lcc_resultP = eval::sp_progn(this->_code,newValueEnvironment);
    };




    T_mv Function_O::lambdaList() {
        ASSERTF(this->closure,BF("The Function closure is NULL"));
        return Values(this->closure->lambdaList(),_lisp->_true());
    }

    T_sp Function_O::functionLambdaListHandler() const {
        ASSERTF(this->closure,BF("The Function closure is NULL"));
        return this->closure->lambdaListHandler();
    };
    bool Function_O::macroP() const {
        ASSERTF(this->closure,BF("The Function closure is NULL"));
        return this->closure->macroP();
    }

    void Function_O::setKind(Symbol_sp k) {
        ASSERTF(this->closure,BF("The Function closure is NULL"));
        this->closure->setKind(k);
    }
    Symbol_sp Function_O::functionKind() const {
        ASSERTF(this->closure,BF("The Function closure is NULL"));
        return this->closure->getKind();
    };

    T_sp Function_O::docstring() const {
        ASSERTF(this->closure,BF("The Function closure is NULL"));
        return this->closure->docstring();
    };
	
    List_sp Function_O::declares() const {
        ASSERTF(this->closure,BF("The Function closure is NULL"));
        return this->closure->declares();
    };
    T_sp Function_O::closedEnvironment() const
    {
        ASSERTF(this->closure,BF("The Function closure is NULL"));
        return this->closure->closedEnvironment;
    };
    

    
    T_sp Function_O::functionName() const {
        ASSERTF(this->closure,BF("The Function closure is NULL"));
        return this->closure->name;
    }

    T_mv Function_O::functionSourcePos() const {
        ASSERTF(this->closure,BF("The Function closure is NULL"));
        T_sp spi = this->closure->sourcePosInfo();
        T_sp sfi = core_sourceFileInfo(spi);
	if ( sfi.nilp() || spi.nilp() ) {
	    return Values(sfi,Integer_O::create(0),make_fixnum(0));
	}
        return Values(sfi,Integer_O::create((size_t)spi.as<SourcePosInfo_O>()->filepos()), make_fixnum(spi.as<SourcePosInfo_O>()->lineno()));
    }



    EXPOSE_CLASS(core,Function_O);

    SYMBOL_EXPORT_SC_(KeywordPkg,calledFunction);
    SYMBOL_EXPORT_SC_(KeywordPkg,givenNumberOfArguments);
    SYMBOL_EXPORT_SC_(KeywordPkg,requiredNumberOfArguments);
    SYMBOL_EXPORT_SC_(KeywordPkg,unrecognizedKeyword);


    void handleArgumentHandlingExceptions(gctools::tagged_functor<FunctionClosure> closure)
    {
        Function_sp func = Function_O::make(closure);
        try {
            throw;
        } catch (TooManyArgumentsError& error) {
            lisp_error(core::_sym_tooManyArgumentsError
                       , lisp_createList(kw::_sym_calledFunction,func
                                         , kw::_sym_givenNumberOfArguments,make_fixnum(error.givenNumberOfArguments)
                                         , kw::_sym_requiredNumberOfArguments,make_fixnum(error.requiredNumberOfArguments)
                           ));
        } catch (TooFewArgumentsError& error) {
            lisp_error(core::_sym_tooFewArgumentsError
                       , lisp_createList(kw::_sym_calledFunction,func
                                         , kw::_sym_givenNumberOfArguments,make_fixnum(error.givenNumberOfArguments)
                                         , kw::_sym_requiredNumberOfArguments,make_fixnum(error.requiredNumberOfArguments)
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
	List_sp code = _Nil<List_V>();
	if ( gctools::tagged_functor<InterpretedClosure> ic = fn->closure.as<InterpretedClosure>() ) {
	    code = ic->_code;
	}
	bool closedp = true; // fn->closedEnvironment().notnilp();
	T_sp name = fn->closure->name;
	T_sp tcode = code;
	return Values(tcode,_lisp->_boolean(closedp),name);
    };


    #define ARGS_core_functionSourceCode "(fn)"
#define DECL_core_functionSourceCode ""
#define DOCS_core_functionSourceCode "functionSourceCode"
    T_sp core_functionSourceCode(Function_sp fn)
    {
	gctools::tagged_functor<Closure> closure = fn->closure;
	if ( auto ic = closure.as<InterpretedClosure>() ) {
	    return ic->code();
	}
	return _Nil<T_O>();
    }







    void Function_O::exposeCando(Lisp_sp lisp)
    {
	class_<Function_O>()
	    .def("core:macrop",&Function_O::macroP)
	    .def("core:setFunctionKind",&Function_O::setKind)
	    .def("core:functionKind",&Function_O::functionKind)
	    .def("core:closedEnvironment",&Function_O::closedEnvironment)
	    .def("core:functionName",&Function_O::functionName)
	    .def("core:functionSourcePos",&Function_O::functionSourcePos)
            .def("core:functionLambdaListHandler",&Function_O::functionLambdaListHandler)
            .def("core:function_declares",&Function_O::declares)
            .def("core:function_docstring",&Function_O::docstring)
	    ;
	ClDefun(functionLambdaExpression);
	CoreDefun(functionSourcePosInfo);
        CoreDefun(setKind);
	CoreDefun(functionLambdaList);
	CoreDefun(functionSourceCode);
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
	if ( !(this->closure) ) {
	    return "Function_O::__repr__ NULL closure";
	}
	T_sp name = this->closure->name;
	stringstream ss;
	ss << "#<" << this->_instanceClass()->classNameAsString();
	ss << " " << _rep_(name);
	ss << " " << this->closure->describe();
#if 0
	auto closure = this->closure;
	void* fptr = closure->functionAddress();
	if ( fptr!=NULL ) {
	    ss << " :address " << fptr;
	}
#endif
	ss << ">";
	return ss.str();
    }



#if defined(XML_ARCHIVE)
    void Function_O::archiveBase(ArchiveP node)
    {
	this->Base::archiveBase(node);
	node->archiveWeakPointer("weakName",this->_WeakName);
    }
#endif // defined(XML_ARCHIVE)





    EXPOSE_CLASS(core,CompiledFunction_O);
    
    void CompiledFunction_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<CompiledFunction_O>()
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
