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
    int FunctionClosure::sourceFileInfoHandle() const { return this->_SourcePosInfo.notnilp() ? this->_SourcePosInfo->fileHandle() : 0; };

    int FunctionClosure::lineNumber() const { return this->_SourcePosInfo.notnilp() ? this->_SourcePosInfo->lineno() : 0; };
    int FunctionClosure::column() const { return this->_SourcePosInfo.notnilp() ? this->_SourcePosInfo->column() : 0; };


    SourcePosInfo_sp FunctionClosure::setSourcePosInfo(T_sp sourceFile, int lineno, int column )
    {
        SourceFileInfo_mv sfi = af_sourceFileInfo(sourceFile);
        Fixnum_sp fileId = sfi.valueGet(1).as<Fixnum_O>();
        SourcePosInfo_sp spi = SourcePosInfo_O::create(fileId->get(),lineno,column);
        this->_SourcePosInfo = spi;
        return spi;
    }

    void BuiltinClosure::LISP_CALLING_CONVENTION()
    {
        IMPLEMENT_MEF(BF("Handle call to BuiltinClosure"));
    };


    void InterpretedClosure::LISP_CALLING_CONVENTION()
    {
        ValueEnvironment_sp newValueEnvironment = ValueEnvironment_O::createForLambdaListHandler(this->_lambdaListHandler,this->closedEnvironment);
        ValueEnvironmentDynamicScopeManager scope(newValueEnvironment);
//        printf("%s:%d About to invoke %s with llh: %s\n", __FILE__, __LINE__, _rep_(this->code).c_str(), _rep_(this->lambdaListHandler).c_str());
        lambdaListHandler_createBindings(this,this->_lambdaListHandler,scope,LCC_PASS_ARGS);
        ValueFrame_sp newActivationFrame = newValueEnvironment->getActivationFrame().as<ValueFrame_O>();
        VectorObjects_sp debuggingInfo = _lambdaListHandler->namesOfLexicalVariablesForDebugging();
        newActivationFrame->attachDebuggingInfo(debuggingInfo);
        InvocationHistoryFrame _frame(this,newActivationFrame);
        *lcc_resultP = eval::sp_progn(this->code,newValueEnvironment);
    };





    LambdaListHandler_sp Function_O::functionLambdaListHandler() const {
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
    Environment_sp Function_O::closedEnvironment() const{IMPLEMENT_ME();};
    T_sp Function_O::functionName() const {
        ASSERTF(this->closure,BF("The Function closure is NULL"));
        return this->closure->name;
    }

    T_mv Function_O::functionSourcePos() const {
        ASSERTF(this->closure,BF("The Function closure is NULL"));
        SourcePosInfo_sp spi = this->closure->sourcePosInfo();
        SourceFileInfo_sp sfi = af_sourceFileInfo(spi);
        return Values(sfi,Fixnum_O::create(spi->lineno()));
    }



    EXPOSE_CLASS(core,Function_O);

    SYMBOL_EXPORT_SC_(KeywordPkg,calledFunction);
    SYMBOL_EXPORT_SC_(KeywordPkg,givenNumberOfArguments);
    SYMBOL_EXPORT_SC_(KeywordPkg,requiredNumberOfArguments);
    SYMBOL_EXPORT_SC_(KeywordPkg,unrecognizedKeyword);


    void handleArgumentHandlingExceptions(FunctionClosure* closure)
    {
        Function_sp func = Function_O::make(closure);
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
	if ( InterpretedClosure* ic = dynamic_cast<InterpretedClosure*>(fn->closure) ) {
	    code = ic->code;
	}
	bool closedp = true; // fn->closedEnvironment().notnilp();
	T_sp name = fn->closure->name;
	return Values(code,_lisp->_boolean(closedp),name);
    };








    void Function_O::exposeCando(Lisp_sp lisp)
    {
	class_<Function_O>()
	    .def("core:macrop",&Function_O::macroP)
	    .def("core:setFunctionKind",&Function_O::setKind)
	    .def("core:functionKind",&Function_O::functionKind)
	    .def("core:closedEnvironment",&Function_O::closedEnvironment)
	    .def("functionName",&Function_O::functionName)
	    .def("core:functionSourcePos",&Function_O::functionSourcePos)
            .def("core:functionLambdaListHandler",&Function_O::functionLambdaListHandler)
	    ;
	ClDefun(functionLambdaExpression);
        CoreDefun(setKind);
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
	if ( this->closure==NULL ) {
	    return "Function_O::__repr__ NULL closure";
	}
	T_sp name = this->closure->name;
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
