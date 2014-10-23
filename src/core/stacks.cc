/*
    File: stacks.cc
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
#include "foundation.h"
#include "stacks.h"
#include "object.h"
#include "cons.h"
#include "symbolTable.h"
#include "lispStream.h"
#include "evaluator.h"
#include "designators.h"
#include "str.h"
#include "sourceFileInfo.h"
#include "symbol.h"
#include "wrappers.h"

namespace core
{



    /*! Return the index of the stack entry with the matching key.
      If return -1 then the key wasn't found */
    int ExceptionStack::findKey(FrameKind kind, T_sp key) {
        for ( int i(this->_Stack.size()-1); i>=0; --i ) {
            if (this->_Stack[i]._FrameKind == kind && this->_Stack[i]._Key == key) return i;
        }
        return -1;
    }

    InvocationHistoryFrame::InvocationHistoryFrame(Closure* c, ActivationFrame_sp env)
        :closure(c), environment(env)
        , runningSourceFileInfoHandle(c->sourceFileInfoHandle())
        , runningLineNumber(c->lineNumber())
        , runningColumn(c->column())
    {
	this->_Stack = &_lisp->invocationHistoryStack();
	this->_Next = this->_Stack->top();
	if (this->_Next == NULL)
	{
	    this->_Index = 0;
	} else
	{
	    this->_Index = this->_Next->_Index+1;
	}
	this->_Stack->push(this);
	this->_Bds = _lisp->bindings().size();
    }



    InvocationHistoryFrame::InvocationHistoryFrame(int sourceFileInfoHandle,
                                                   int lineno,
                                                   int column,
                                                   ActivationFrame_sp env)
        : closure(NULL), environment(env)
        , runningSourceFileInfoHandle(sourceFileInfoHandle)
        , runningLineNumber(lineno)
        , runningColumn(column)
    {
	this->_Stack = &_lisp->invocationHistoryStack();
	this->_Next = this->_Stack->top();
	if (this->_Next == NULL)
	{
	    this->_Index = 0;
	} else
	{
	    this->_Index = this->_Next->_Index+1;
	}
	this->_Stack->push(this);
	this->_Bds = _lisp->bindings().size();
    }






    InvocationHistoryFrame::~InvocationHistoryFrame()
    {
	this->_Stack->pop();
    }


    void InvocationHistoryStack::setExpressionForTop(T_sp expression)
    {
        uint ilineno(0), icolumn(0);
        if ( _lisp->sourceDatabase().notnilp() ) {
            SourceFileInfo_mv info = _lisp->sourceDatabase()->lookupSourceInfo(expression);
            if ( info.notnilp() ) {
                Fixnum_sp lineno = info.valueGet(1).as<Fixnum_O>();
                Fixnum_sp column = info.valueGet(2).as<Fixnum_O>();
                ilineno = lineno.nilp() ? 0 : lineno->get();
                icolumn = column.nilp() ? 0 : column->get();
                this->_Top->setSourcePos(info->fileHandle(),ilineno,icolumn);
            } else {
//                this->_Top->setSourcePos(0,0,0);
            }
        }
    }


    string InvocationHistoryFrame::asStringLowLevel(Closure* closure,
                                                    const string& funcName,
						    const string& sourceFileName,
						    uint lineNumber,
						    uint column) const
    {
	stringstream ss;
        if ( lineNumber == UNDEF_UINT ) lineNumber = 0;
        if ( column == UNDEF_UINT ) column = 0;
        string closureType = "/?";
        if (closure) {
        if ( closure->interpretedP() ) {
            closureType = "/i";
        } else if ( closure->compiledP() ) {
            closureType = "/c";
        } else if ( closure->builtinP() ) {
            closureType = "/b";
        }
        } else closureType = "toplevel";
	ss << (BF("#%3d%2s %20s %5d/%-3d %s") % this->_Index % closureType % sourceFileName % lineNumber % column  % funcName ).str();
//	ss << std::endl;
//	ss << (BF("     activationFrame->%p") % this->activationFrame().get()).str();
	return ss.str();
    }



    string InvocationHistoryFrame::sourcePathName() const
    {
        return af_sourceFileInfo(Fixnum_O::create(this->runningSourceFileInfoHandle))->namestring();
    }



    string InvocationHistoryFrame::asString()
    {
        string name;
        SourceFileInfo_sp sfi = af_sourceFileInfo(Fixnum_O::create(this->runningSourceFileInfoHandle));
        if ( this->closure ) {
            name = _rep_(this->closure->name);
        } else {
            name = "REPL";
        }
	return this->asStringLowLevel(this->closure,
                                      name,
				      sfi->fileName(),
				      this->runningLineNumber,
                                      this->runningColumn);
    }



















#if 0
    //! Define this to ensure that TopLevelIHF vtable is weak symbol
    void TopLevelIHF::keyFunctionForVtable() {};




    TopLevelIHF::TopLevelIHF(InvocationHistoryStack& stack, T_sp expression, FunctionClosure* fc )
	: InvocationHistoryFrame(TopLevel,stack, fc),
	  _SourceFileInfo(_Nil<SourceFileInfo_O>()),
	  _LineNumber(0),
	  _Column(0),
	  _FilePos(0)
    {
	if ( expression.notnilp() ) {
            if ( _lisp->sourceDatabase().notnilp() ) {
                SourceFileInfo_mv sfi = _lisp->sourceDatabase()->lookupSourceInfo(expression);
                this->_SourceFileInfo = sfi;
                if ( sfi.number_of_values()>=3 ) {
                    this->_LineNumber = sfi.valueGet(1).as<Fixnum_O>()->get();
                    this->_Column = sfi.valueGet(2).as<Fixnum_O>()->get();
                    this->_FilePos = sfi.valueGet(3).as<Fixnum_O>()->get();
                }
            }
	}
    }

    string TopLevelIHF::asString() const
    {
	return this->asStringLowLevel(this->typeName(),"anon",
				      this->_SourceFileInfo.nilp() ? "" : this->_SourceFileInfo->fileName(),
				      this->_LineNumber,this->_Column);
    }

    string TopLevelIHF::sourcePathName() const
    {
	return this->_SourceFileInfo->namestring();
    }




    string DebuggerIHF::asString() const
    {
	return this->asStringLowLevel(this->typeName(),"anon", "", NoLine, NoColumn);
    }


    void DebuggerIHF::setLineNumberColumn(uint lineNumber, uint column)
    {
	printf("%s:%d Something is trying to setLineNumberColumn of a DebuggerIHF\n", __FILE__, __LINE__ );
    }
    void DebuggerIHF::setActivationFrame(ActivationFrame_sp af)
    {
	printf("%s:%d Something is trying to setActivationFrame of a DebuggerIHF\n", __FILE__, __LINE__ );
    }


    CxxFunctionIHF::~CxxFunctionIHF() {};


    string CxxFunctionIHF::sourcePathName() const
    {
	return af_sourceFileInfo(this->_Function)->namestring();
    }


    string CxxFunctionIHF::asString() const
    {
	return this->asStringLowLevel(this->typeName(),_rep_(this->_Function->closure->name),af_sourceFileInfo(this->_Function)->fileName(),this->_LineNumber,0);
    }


    void CxxFunctionIHF::setActivationFrame(ActivationFrame_sp af)
    {_G();
	// Do nothing
    }

    ActivationFrame_sp CxxFunctionIHF::activationFrame() const {
	return _Nil<ActivationFrame_O>();
//	IMPLEMENT_MEF(BF("Switch to ArgArray"));
    }

    LispFunctionIHF::LispFunctionIHF(IHFLeafKind kind, InvocationHistoryStack& stack, FunctionClosure* fc)
	: InvocationHistoryFrame(kind,stack,fc),
	  _LineNumber(0),
	  _Column(0)
    {};
    LispFunctionIHF::LispFunctionIHF(IHFLeafKind kind, InvocationHistoryStack& stack, FunctionClosure* fc, ActivationFrame_sp af)
	: InvocationHistoryFrame(kind,stack,fc),
	  _ActivationFrame(af),
	  _LineNumber(0),
	  _Column(0)
    {};


    string LispFunctionIHF::sourcePathName() const
    {
	return this->closure->sourcePosInfo->sourceFileInfo->namestring();
    }



    ActivationFrame_sp LispFunctionIHF::activationFrame() const
    {
	return this->_ActivationFrame;
    };


    void LispFunctionIHF::setLineNumberColumn(uint lineNumber, uint column)
    {
	this->_LineNumber = lineNumber;
	this->_Column = column;
    };


    void LispFunctionIHF::setActivationFrame(ActivationFrame_sp af)
    {
	this->_ActivationFrame = af;
    };




    string LispInterpretedFunctionIHF::asString() const
    {
	return this->asStringLowLevel(this->typeName(),_rep_(this->closure->name),
                                      this->closure->sourceFileInfo->fileName(),
				      this->_LineNumber,this->_Column);
    }

    string LispCompiledFunctionIHF::asString() const
    {
	return this->asStringLowLevel(this->typeName(),
				      _rep_(this->closure->name),
				      this->closure->sourceFileInfo->fileName(),
				      this->_LineNumber,
				      this->_Column);
    }




    string MacroExpansionIHF::asString() const
    {
	return this->asStringLowLevel(this->typeName(),
				      _rep_(this->closure->name),
				      this->closure->sourceFileInfo->fileName(),
				      this->_LineNumber,this->_Column);
    }


#endif

    


    vector<InvocationHistoryFrame*> InvocationHistoryStack::asVectorFrames()
    {
	vector<InvocationHistoryFrame*> frames;
	frames.resize(this->_Top->index()+1);
	for ( InvocationHistoryFrame* cur = _lisp->invocationHistoryStack().top();
	      cur!=NULL; cur=cur->next() )
	{
	    frames[cur->index()] = cur;
	}
	return frames;
    }




    string InvocationHistoryStack::asString() const
    {
	stringstream	ss;
	ss.str("");
	ss << std::endl;
	vector<InvocationHistoryFrame*> frames = _lisp->invocationHistoryStack().asVectorFrames();
	ss << "--------STACK TRACE--------" << std::endl;
	int ihsCur = af_ihsCurrentFrame();
	for ( int i=0; i<frames.size(); ++i )
	{
	    InvocationHistoryFrame* cur = frames[i];
	    if ( i == ihsCur ) {
		ss << "-->";
	    } else {
		ss << "   ";
	    }
	    ss << "frame";
            ss << cur->asString() << std::endl;
	}
	return ss.str();
    }




    SYMBOL_EXPORT_SC_(CorePkg,STARwatchDynamicBindingStackSTAR);
    void DynamicBindingStack::push(Symbol_sp var)
    {
        if ( _sym_STARwatchDynamicBindingStackSTAR->symbolValueUnsafe().notnilp() ) {
            Cons_sp assoc = af_assoc(var,_sym_STARwatchDynamicBindingStackSTAR->symbolValue(),_Nil<T_O>());
            if ( assoc.notnilp() ) {
                T_sp funcDesig = oCdr(assoc);
                if ( funcDesig.notnilp() ) {
                    eval::funcall(funcDesig,var,_lisp->_true());
                } else {
                    printf("%s:%d  *watch-dynamic-binding-stack* caught push[%zu] of %s  value = %s\n", __FILE__, __LINE__, this->_Bindings.size(), _rep_(var).c_str(), _rep_(var->symbolValue()).c_str() );
                }
            }
        }
	T_sp val = var->symbolValueUnsafe();
	DynamicBinding bind(var,var->symbolValueUnsafe());
	this->_Bindings.push_back(bind);
    }


    void DynamicBindingStack::pop()
    {
	DynamicBinding& bind = this->_Bindings.back();
        if ( _sym_STARwatchDynamicBindingStackSTAR->symbolValue().notnilp() ) {
            Cons_sp assoc = af_assoc(bind._Var,_sym_STARwatchDynamicBindingStackSTAR->symbolValue(),_Nil<T_O>());
            if ( assoc.notnilp() ) {
                T_sp funcDesig = oCdr(assoc);
                if ( funcDesig.notnilp() ) {
                    eval::funcall(funcDesig,bind._Var,_Nil<T_O>());
                } else {
                    printf("%s:%d  *watch-dynamic-binding-stack* caught pop[%zu] of %s  overwriting value = %s\n", __FILE__, __LINE__, this->_Bindings.size()-1, _rep_(bind._Var).c_str(), _rep_(bind._Var->symbolValue()).c_str() );
                }
            }
        }
	bind._Var->setf_symbolValue(bind._Val);
	this->_Bindings.pop_back();
    }

#ifdef OLD_MPS
    GC_RESULT DynamicBindingStack::scanGCRoots(GC_SCAN_ARGS_PROTOTYPE)
    {
	GC_SCANNER_BEGIN() {
	    for ( vector<DynamicBinding>::iterator it=this->_Bindings.begin(); it<this->_Bindings.end(); ++it)
	    {
		SMART_PTR_FIX(it->_Var);
		SMART_PTR_FIX(it->_Val);
	    }
	} GC_SCANNER_END();
	return GC_RES_OK;
    }
#endif





    InvocationHistoryFrame* get_ihs_ptr(int idx)
    {
	InvocationHistoryFrame* cur = _lisp->invocationHistoryStack().top();
	for ( ; cur!=NULL; cur = cur->next() )
	{
	    if (cur->index() == idx ) return cur;
	}
	return NULL;
    }




#ifdef OLD_MPS
    GC_RESULT InvocationHistoryStack::scanGCRoots(GC_SCAN_ARGS_PROTOTYPE)
    {
	InvocationHistoryStack& ihs = _lisp->invocationHistoryStack(); // in multithreaded code there is one for every thread
	InvocationHistoryFrame* cur = ihs.top();
	GC_SCANNER_BEGIN() {
	    while (cur) {
		switch (cur->_IHSFrameKind) {
		case TopLevel: {
                    TopLevelIHF* tcur = dynamic_cast<TopLevelIHF*>(cur);
                    tcur->scanGCRoots(GC_SCAN_ARGS_PASS);
		    break;}
		case Debugger: {
		    DebuggerIHF* dcur = dynamic_cast<DebuggerIHF*>(cur);
                    dcur->scanGCRoots(GC_SCAN_ARGS_PASS);
		    break;}
		case CxxFunction: {
		    CxxFunctionIHF* ccur = dynamic_cast<CxxFunctionIHF*>(cur);
                    ccur->scanGCRoots(GC_SCAN_ARGS_PASS);
		    break;}
		case LispInterpretedFunction: {
		    LispInterpretedFunctionIHF* lcur = dynamic_cast<LispInterpretedFunctionIHF*>(cur);
                    lcur->scanGCRoots(GC_SCAN_ARGS_PASS);
		    break; }
		case LispCompiledFunction: {
		    LispCompiledFunctionIHF* mcur = dynamic_cast<LispCompiledFunctionIHF*>(cur);
                    mcur->scanGCRoots(GC_SCAN_ARGS_PASS);
		    break; }
		case MacroExpansionFunction: {
		    MacroExpansionIHF* mcur = dynamic_cast<MacroExpansionIHF*>(cur);
                    mcur->scanGCRoots(GC_SCAN_ARGS_PASS);
		    break; }
		};
		cur = cur->_Next;
	    }
	} GC_SCANNER_END();
	return GC_RES_OK;
    }
#endif
    




    
    
#define ARGS_af_ihsBacktrace "(&optional (out t) msg)"
#define DECL_af_ihsBacktrace ""
#define DOCS_af_ihsBacktrace "ihsBacktrace"
    T_sp af_ihsBacktrace(T_sp outputDesignator, T_sp msg)
    {_G();
	T_sp ss;
	if ( outputDesignator.nilp() ) {
	    ss = clasp_make_string_output_stream();
	} else {
	    ss = coerce::outputStreamDesignator(outputDesignator);
	}
	if ( !msg.nilp() )
	{
	    clasp_writeln_string(((BF("\n%s")%_rep_(msg)).str()),ss);
	}
	clasp_writeln_string(((BF("%s") % _lisp->invocationHistoryStack().asString()).str()),ss);
	if (outputDesignator.nilp() )
	{
	    return cl_get_output_stream_string(ss);
	}
	return _Nil<T_O>();
    };



    extern "C" {    
    
#define ARGS_af_ihsBacktraceNoArgs "()"
#define DECL_af_ihsBacktraceNoArgs ""
#define DOCS_af_ihsBacktraceNoArgs "ihsBacktraceNoArgs"
	void af_ihsBacktraceNoArgs()
	{_G();
            af_ihsBacktrace(_lisp->_true(),_Nil<T_O>());
        };

    };


    
    
#define ARGS_af_ihsTop "()"
#define DECL_af_ihsTop ""
#define DOCS_af_ihsTop "ihsTop"
  int af_ihsTop()
  {_G();
    if ( _lisp->invocationHistoryStack().top() == NULL ) {
      return 0;
    }
    return _lisp->invocationHistoryStack().top()->index();
  };



    
    
#define ARGS_af_ihsPrev "(cur)"
#define DECL_af_ihsPrev ""
#define DOCS_af_ihsPrev "ihsPrev"
    int af_ihsPrev(int idx)
    {_G();
	idx--;
	if ( idx<0 ) idx = 0;
	return idx;
    };



    
    
#define ARGS_af_ihsNext "(cur)"
#define DECL_af_ihsNext ""
#define DOCS_af_ihsNext "ihsNext"
    int af_ihsNext(int idx)
    {_G();
	idx++;
	if ( idx > _lisp->invocationHistoryStack().top()->index() ) idx = _lisp->invocationHistoryStack().top()->index();
	return idx;
    };



    
    
#define ARGS_af_ihsFun "(arg)"
#define DECL_af_ihsFun ""
#define DOCS_af_ihsFun "ihsFunc"
    Function_sp af_ihsFun(int idx)
    {_G();
	InvocationHistoryFrame* cur = get_ihs_ptr(idx);
	if (cur)
	{
            Function_sp func = Function_O::make(cur->closure); //QUESTION: Should I be returning a new function every time?  This will cause problems when comparing functions - but I don't think I do that anywhere.
            if ( func.pointerp() ) return func;
        }
	return _Nil<Function_O>();
    };



    
    
#define ARGS_af_ihsEnv "(cur)"
#define DECL_af_ihsEnv ""
#define DOCS_af_ihsEnv "ihsEnv"
    Environment_sp af_ihsEnv(int idx)
    {_G();
	InvocationHistoryFrame* cur = get_ihs_ptr(idx);
	if (cur)
	{
            Environment_sp env = cur->activationFrame();
            if ( env.pointerp() ) return env;
        }
	return _Nil<Environment_O>();
    };


#define ARGS_af_ihsBds "(cur)"
#define DECL_af_ihsBds ""
#define DOCS_af_ihsBds "ihsBds"
    int af_ihsBds(int idx)
    {_G();
	InvocationHistoryFrame* cur = get_ihs_ptr(idx);
	if (cur) return cur->bds();
	return 0;
    };



	
	
#define ARGS_af_ihsCurrentFrame "()"
#define DECL_af_ihsCurrentFrame ""
#define DOCS_af_ihsCurrentFrame "ihsCurrentFrame"
    int af_ihsCurrentFrame()
    {_G();
        T_sp cf = _sym_STARihsCurrentSTAR->symbolValue();
        if ( cf.nilp() ) {
            int icf = af_ihsTop();
            return af_setIhsCurrentFrame(icf);
        }
        int icf = cf.as<Fixnum_O>()->get();
        if (icf < 0) {	
            _sym_STARihsCurrentSTAR->setf_symbolValue(Fixnum_O::create(icf));
            return 0;
        }
        if (icf >= af_ihsTop() ) {
            _sym_STARihsCurrentSTAR->setf_symbolValue(Fixnum_O::create(af_ihsTop()));
            return af_ihsTop();
        }
        return icf;
    }


#define ARGS_af_setIhsCurrentFrame "()"
#define DECL_af_setIhsCurrentFrame ""
#define DOCS_af_setIhsCurrentFrame "setIhsCurrentFrame"
    int af_setIhsCurrentFrame(int icf)
    {_G();
        if (icf < 0) icf = 0;
        else if (icf >= af_ihsTop() ) icf = af_ihsTop();
        _sym_STARihsCurrentSTAR->setf_symbolValue(Fixnum_O::create(icf));
        return icf;
    }



    
    
    
#define ARGS_af_bdsTop "()"
#define DECL_af_bdsTop ""
#define DOCS_af_bdsTop "bdsTop"
    int af_bdsTop()
    {_G();
	return _lisp->bindings().top();
    };


    
    
#define ARGS_af_bdsVar "(idx)"
#define DECL_af_bdsVar ""
#define DOCS_af_bdsVar "bdsVar"
    Symbol_sp af_bdsVar(int idx)
    {_G();
	return _lisp->bindings().var(idx);
    };



    
    
#define ARGS_af_bdsVal "(idx)"
#define DECL_af_bdsVal ""
#define DOCS_af_bdsVal "bdsVal"
    T_sp af_bdsVal(int idx)
    {_G();
	return _lisp->bindings().val(idx);
    };







    void initialize_stacks()
    {
	SYMBOL_SC_(CorePkg,ihsBacktrace);
	Defun(ihsBacktrace);
	SYMBOL_SC_(CorePkg,ihsTop);
	Defun(ihsTop);
	SYMBOL_SC_(CorePkg,ihsPrev);
	Defun(ihsPrev);
	SYMBOL_SC_(CorePkg,ihsNext);
	Defun(ihsNext);
	SYMBOL_SC_(CorePkg,ihsFun);
	Defun(ihsFun);
	SYMBOL_SC_(CorePkg,ihsEnv);
	Defun(ihsEnv);
	SYMBOL_SC_(CorePkg,bdsTop);
	Defun(bdsTop);
	SYMBOL_SC_(CorePkg,bdsVar);
	Defun(bdsVar);
	SYMBOL_SC_(CorePkg,bdsVal);
	Defun(bdsVal);
    }





};
