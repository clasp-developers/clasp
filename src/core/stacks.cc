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
#include <clasp/core/foundation.h>
#include <clasp/core/stacks.h>
#include <clasp/core/object.h>
#include <clasp/core/cons.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/designators.h>
#include <clasp/core/str.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/symbol.h>
#include <clasp/core/wrappers.h>

namespace core
{



    /*! Return the index of the stack entry with the matching key.
      If return -1 then the key wasn't found */
    int ExceptionStack::findKey(FrameKind kind, T_sp key) {
        for (  int i(this->_Stack.size()-1); i>=0; --i ) {
            if (this->_Stack[i]._FrameKind == kind && this->_Stack[i]._Key == key) return i;
        }
        return -1;
    }

    Vector_sp ExceptionStack::backtrace() {
	if ( this->_Stack.size() == 0 ) {
	    return _Nil<Vector_O>();
	}
	printf("%s:%d ExceptionStack::backtrace stack size = %lu\n", __FILE__, __LINE__, this->_Stack.size());
	Vector_sp result = af_make_vector(_Nil<T_O>()
					  ,this->_Stack.size()
					  ,false
					  ,make_fixnum((int)(this->_Stack.size()))
					 );
	for ( int i(0), iEnd(this->_Stack.size()); i<iEnd; ++i ) {
	    Symbol_sp kind;
	    SYMBOL_EXPORT_SC_(KeywordPkg,catchFrame);
	    SYMBOL_EXPORT_SC_(KeywordPkg,blockFrame);
	    SYMBOL_EXPORT_SC_(KeywordPkg,tagbodyFrame);
	    SYMBOL_EXPORT_SC_(KeywordPkg,landingPadFrame);
	    switch (this->_Stack[i]._FrameKind) {
	    case NullFrame:
		kind = _Nil<Symbol_O>();
		break;
	    case CatchFrame:
		kind = kw::_sym_catchFrame;
		break;
	    case BlockFrame:
		kind = kw::_sym_blockFrame;
		break;
	    case TagbodyFrame:
		kind = kw::_sym_tagbodyFrame;
		break;
	    case LandingPadFrame:
		kind = kw::_sym_landingPadFrame;
		break;
	    };
	    result->setf_elt(i,Cons_O::create(kind,this->_Stack[i]._Key));
	}
	return result;
    }
	    

    InvocationHistoryFrame::InvocationHistoryFrame(Closure* c, T_sp env)
        :closure(c), environment(env)
        , runningSourceFileInfoHandle(c->sourceFileInfoHandle())
	, runningFilePos(c->filePos())
        , runningLineNumber(c->lineNumber())
        , runningColumn(c->column())
    {
	if ( c->name.nilp() ) {
	    SIMPLE_ERROR(BF("The InvocationHistoryFrame closure has nil name"));
	}
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
        uint ifilepos(0), ilineno(0), icolumn(0);
        if ( _lisp->sourceDatabase().notnilp() ) {
            T_sp info = _lisp->sourceDatabase().as<SourceManager_O>()->lookupSourcePosInfo(expression);
            if ( info.notnilp() ) {
                this->_Top->setSourcePos(info);
            } else {
//                this->_Top->setSourcePos(0,0,0);
            }
        }
    }


    string InvocationHistoryFrame::asStringLowLevel(Closure* closure,
						    uint lineNumber,
						    uint column) const
    {
	if ( closure==NULL ) {return "InvocationHistoryFrame::asStringLowLevel NULL closure";};
	T_sp funcNameObj = closure->name;
	string funcName = _rep_(funcNameObj);
	int sourceFileInfoHandle = this->runningSourceFileInfoHandle;
	SourceFileInfo_sp sfi = core_sourceFileInfo(make_fixnum(sourceFileInfoHandle));
	string sourceFileName = sfi->fileName();
	stringstream ss;
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
	ss << (BF("#%3d%2s@%p %20s %5d/%-3d %s") % this->_Index % closureType % (void*)closure % sourceFileName % lineNumber % column  % funcName ).str();
//	ss << std::endl;
//	ss << (BF("     activationFrame->%p") % this->activationFrame().get()).str();
	return ss.str();
    }



    string InvocationHistoryFrame::sourcePathName() const
    {
        return core_sourceFileInfo(make_fixnum(this->runningSourceFileInfoHandle)).as<SourceFileInfo_O>()->namestring();
    }



    string InvocationHistoryFrame::asString()
    {
        string name;
	return this->asStringLowLevel(this->closure,
				      this->runningLineNumber,
                                      this->runningColumn);
    }


    


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
#if 0 // debugging
        if ( _sym_STARwatchDynamicBindingStackSTAR->symbolValueUnsafe().notnilp() ) {
            List_sp assoc = cl_assoc(var,_sym_STARwatchDynamicBindingStackSTAR->symbolValue(),_Nil<T_O>());
            if ( assoc.notnilp() ) {
                T_sp funcDesig = oCdr(assoc);
                if ( funcDesig.notnilp() ) {
                    eval::funcall(funcDesig,var,_lisp->_true());
                } else {
                    printf("%s:%d  *watch-dynamic-binding-stack* caught push[%zu] of %s\n", __FILE__, __LINE__, this->_Bindings.size(), var->formattedName(true).c_str());
                }
            }
        }
#endif
#if 0 // debugging
        if ( _sym_STARwatchDynamicBindingStackSTAR->symbolValueUnsafe().notnilp() ) {
	    //	    printf("%s:%d  *watch-dynamic-binding-stack* caught push[%zu] of %s\n", __FILE__, __LINE__, this->_Bindings.size(), var->formattedName(true).c_str());
	    printf("%s:%d  *watch-dynamic-binding-stack* caught push[%zu] of %s\n", __FILE__, __LINE__, this->_Bindings.size(), var->symbolNameAsString().c_str());
	    //printf("%s:%d  *watch-dynamic-binding-stack* caught push[%zu]\n", __FILE__, __LINE__, this->_Bindings.size() );
	};
#endif
	T_sp val = var->symbolValueUnsafe();
	DynamicBinding bind(var,var->symbolValueUnsafe());
	this->_Bindings.push_back(bind);
    }


    void DynamicBindingStack::pop()
    {
	DynamicBinding& bind = this->_Bindings.back();
#if 0 // debugging
        if ( _sym_STARwatchDynamicBindingStackSTAR->symbolValue().notnilp() ) {
            List_sp assoc = cl_assoc(bind._Var,_sym_STARwatchDynamicBindingStackSTAR->symbolValue(),_Nil<T_O>());
            if ( assoc.notnilp() ) {
                T_sp funcDesig = oCdr(assoc);
                if ( funcDesig.notnilp() ) {
                    eval::funcall(funcDesig,bind._Var,_Nil<T_O>());
                } else {
                    printf("%s:%d  *watch-dynamic-binding-stack* caught pop[%zu] of %s  overwriting value = %s\n", __FILE__, __LINE__, this->_Bindings.size()-1, _rep_(bind._Var).c_str(), _rep_(bind._Var->symbolValue()).c_str() );
                }
            }
        }
#endif
#if 0 // debugging
	if ( _sym_STARwatchDynamicBindingStackSTAR->symbolValueUnsafe().notnilp() ) {
	    //	    printf("%s:%d  *watch-dynamic-binding-stack* caught pop[%zu] of %s\n", __FILE__, __LINE__, this->_Bindings.size()-1, bind._Var->formattedName(true).c_str());
	    printf("%s:%d  *watch-dynamic-binding-stack* caught pop[%zu] of %s\n", __FILE__, __LINE__, this->_Bindings.size(), bind._Var->symbolNameAsString().c_str());
	    //printf("%s:%d  *watch-dynamic-binding-stack* caught pop[%zu]\n", __FILE__, __LINE__, this->_Bindings.size() );
	}	    
#endif
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
    

#define ARGS_core_lowLevelBacktrace "()"
#define DECL_core_lowLevelBacktrace ""
#define DOCS_core_lowLevelBacktrace "lowLevelBacktrace"
void    core_lowLevelBacktrace()
    {
	InvocationHistoryStack& ihs = _lisp->invocationHistoryStack();
	InvocationHistoryFrame* top = ihs.top();
	if ( top == NULL ) {
	    printf("Empty InvocationHistoryStack\n");
	    return;
	}
	printf("From bottom to top invocation-history-stack frames = %d\n", top->_Index+1);
	for ( InvocationHistoryFrame* cur = top; cur != NULL; cur=cur->_Next ) {
	    string name = "-no-name-";
	    Closure* closure = cur->closure;
	    if ( closure == NULL ) {
		name = "-NO-CLOSURE-";
	    } else {
		if ( closure->name.notnilp() ) {
		    try {
			name = _rep_(closure->name);
		    } catch (...) {
			name = "-BAD-NAME-";
		    }
		}
	    }
	    /*Nilable?*/T_sp sfi = core_sourceFileInfo(make_fixnum(closure->sourceFileInfoHandle()));
	    string sourceName = "cannot-determine";
	    if ( sfi.notnilp() ) {
		sourceName = sfi.as<SourceFileInfo_O>()->fileName();
	    }
	    printf("_Index: %4d  Frame@%p(next=%p)  closure@%p  closure->name[%40s]  line: %3d  file: %s\n", cur->_Index, cur, cur->_Next, closure, name.c_str(), closure->lineNumber(), sourceName.c_str() );
	}
	printf("----Done\n");
    }



    
    
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
#define DOCS_af_ihsFun "ihsFun: return the function in the invocation history stack at i"
    T_sp af_ihsFun(int idx)
    {_G();
	InvocationHistoryFrame* cur = get_ihs_ptr(idx);
	if (cur)
	{
            Function_sp func = Function_O::make(cur->closure); //QUESTION: Should I be returning a new function every time?  This will cause problems when comparing functions - but I don't think I do that anywhere.
            if ( func.objectp() ) return func;
        }
	printf("%s:%d   There is a problem - ihsFun is returning NIL as the function at idx: %d\n", __FILE__, __LINE__, idx );
	return _Nil<T_O>();
    };



    
    
#define ARGS_af_ihsEnv "(cur)"
#define DECL_af_ihsEnv ""
#define DOCS_af_ihsEnv "ihsEnv"
    T_sp af_ihsEnv(int idx)
    {_G();
	InvocationHistoryFrame* cur = get_ihs_ptr(idx);
	if (cur)
	{
            T_sp env = cur->activationFrame();
            return env;
        }
	return _Nil<T_O>();
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
        int icf = unbox_fixnum(cf.as<Fixnum_O>());
        if (icf < 0) {	
            _sym_STARihsCurrentSTAR->setf_symbolValue(make_fixnum(icf));
            return 0;
        }
        if (icf >= af_ihsTop() ) {
            _sym_STARihsCurrentSTAR->setf_symbolValue(make_fixnum(af_ihsTop()));
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
        _sym_STARihsCurrentSTAR->setf_symbolValue(make_fixnum(icf));
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



#define ARGS_core_exceptionStack "()"
#define DECL_core_exceptionStack ""
#define DOCS_core_exceptionStack "exceptionStack"
    Vector_sp core_exceptionStack()
    {
	return _lisp->exceptionStack().backtrace();
    }


    
#define ARGS_core_exceptionStackDump "()"
#define DECL_core_exceptionStackDump ""
#define DOCS_core_exceptionStackDump "exceptionStackDump"
    void core_exceptionStackDump()
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
            printf("Exception exceptionstack[%2d] = %8s %s@%p\n", i, kind.c_str(), _rep_(stack[i]._Key).c_str(), stack[i]._Key.raw_());
        }
        printf("----Done----\n");
    };


#define ARGS_core_dynamicBindingStackDump "()"
#define DECL_core_dynamicBindingStackDump ""
#define DOCS_core_dynamicBindingStackDump "dynamicBindingStackDump"
    void core_dynamicBindingStackDump(std::ostream& out)
    {
	DynamicBindingStack& bd = _lisp->bindings();
	for ( int i(0), iEnd(bd.size()); i<iEnd; ++i ) {
	    out << "  dbstack[" << i << " --> " << _rep_(bd.var(i)) << std::endl;
	};
    }

    
};



namespace core {
    
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
	CoreDefun(lowLevelBacktrace);
	CoreDefun(exceptionStack);
	CoreDefun(exceptionStackDump);
	//	CoreDefun(dynamicBindingStackDump);

    }





};
