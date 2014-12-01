/*
    File: intrinsics.cc
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
#define DEBUG_LEVEL_FULL
#ifdef USE_MPS
extern "C" {
#include "mps/code/mps.h"
};
#endif
#include "core/foundation.h"
#include "core/common.h"
//#include "core/debugger.h"
#include "core/bignum.h"
#include "core/character.h"
#include "core/symbolTable.h"
#include "core/arrayObjects.h"
#include "core/vectorObjects.h"
#include "core/arguments.h"
#include "core/designators.h"
#include "core/compPackage.h"
#include "core/package.h"
#include "core/hashTable.h"
#include "core/evaluator.h"
#include "core/sourceFileInfo.h"
#include "core/loadTimeValues.h"
#include "core/multipleValues.h"
#include "core/activationFrame.h"
#include "core/vectorObjectsWithFillPtr.h"
#include "core/str.h"
#include "symbolTable.h"
#include "llvmoExpose.h"
#include "intrinsics.h"

//#define	DEBUG_FLOW_CONTROL	1

using namespace core;


#pragma GCC visibility push(default)


extern "C"
{

    LispCallingConventionPtr lccGlobalFunction( core::Symbol_sp sym)
    {
        printf("%s:%d lccSymbolFunction for %s returning NULL for now\n", __FILE__, __LINE__, _rep_(sym).c_str() );
        return NULL;
    }


#if 0
    void varargs_lexicalFunction( core::Function_sp* resultP, int depth, int index, core::ActivationFrame_sp* evaluateFrameP )
    {_G();
	LOG(BF("About to invoke lexicalFunction depth[%d] index[%d]") % depth % index );
	(*resultP) = (*evaluateFrameP)->lookupFunction(depth,index).as<core::Function_O>();
	ASSERTF((*resultP).pointerp(), BF("UNDEFINED lexicalFunctionRead!! value depth[%d] index[%d] activationFrame: %s")
		% depth % index % _rep_(*evaluateFrameP) );
    }

#endif



    int testVarargs(int numargs, ...)
    {
//        printf("%s:%d Entered testVarargs with numargs=%d: ", __FILE__, __LINE__, numargs );
        int i(0);
        va_list argp;
        va_start(argp,numargs);
        for ( ; numargs; --numargs ) {
            i = va_arg(argp,int);
//            printf("%d ", i );
        }
        va_end(argp);
//        printf("\n");
        return i;
    }


};
extern "C"
{

    const std::type_info& typeidCoreCatchThrow = typeid(core::CatchThrow);
    const std::type_info& typeidCoreLexicalGo =  typeid(core::LexicalGo);
    const std::type_info& typeidCoreDynamicGo =  typeid(core::DynamicGo);
    const std::type_info& typeidCoreReturnFrom = typeid(core::ReturnFrom);


#define ALWAYS_INLINE __attribute__((always_inline))

#define LOW_LEVEL_TRACE_QUEUE_SIZE	1024
    uint _LLVMLowLevelTraceQueueIn = 0;
    uint _LLVMLowLevelTraceQueueWrapped = false;
    uint _LLVMLowLevelTraceQueue[LOW_LEVEL_TRACE_QUEUE_SIZE];

    void lowLevelTrace(uint traceid)
    {
	if ( comp::_sym_STARlowLevelTracePrintSTAR->symbolValue().isTrue() )
	{
	    printf("+++ lowLevelTrace[%d]\n", traceid);
	}
	_LLVMLowLevelTraceQueue[_LLVMLowLevelTraceQueueIn] = traceid;
	++_LLVMLowLevelTraceQueueIn;
	if ( _LLVMLowLevelTraceQueueIn >= LOW_LEVEL_TRACE_QUEUE_SIZE )
	{
	    _LLVMLowLevelTraceQueueIn = 0;
	    _LLVMLowLevelTraceQueueWrapped = true;
	}
    }


    void unreachableError()
    {
	printf("%s:%d In unreachableError -  Hit an unreachable block\n",
	       __FILE__,__LINE__);
    }

    void dumpLowLevelTrace(int numLowLevels)
    {
	int cur = _LLVMLowLevelTraceQueueIn;
	for ( int i=0; i<numLowLevels; i++)
	{
	    --cur;
	    if ( cur < 0 )
	    {
		if (_LLVMLowLevelTraceQueueWrapped)
		{
		    cur = LOW_LEVEL_TRACE_QUEUE_SIZE-1;
		} else {
		    printf("-----Ran out of block trace entries----\n");
		    break;
		}
	    }
	    printf( "LowLevel-trace#%d -> %u\n", -i, _LLVMLowLevelTraceQueue[cur]);
	}
    }


};

extern "C" {
#if 0
    TAGGED_PTR va_vararg(va_list ap)
    {
	return va_arg(ap,TAGGED_PTR);
    }

    void generateTspFromTAGGED_PTR(core::T_sp* resultP, TAGGED_PTR x)
    {
	(*resultP) = gctools::smart_ptr<T_O>(x);
    }
#endif


    void va_throwTooManyArgumentsException( const char* funcName, int givenNumberOfArguments, int requiredNumberOfArguments)
    {_G();
        core::throwTooManyArgumentsError(givenNumberOfArguments,requiredNumberOfArguments);
//	SIMPLE_ERROR(BF("In %s too many arguments passed - got %d and expected %d") % funcName % givenNumberOfArguments % requiredNumberOfArguments  );
    }

    void va_throwNotEnoughArgumentsException( const char* funcName,  int givenNumberOfArguments, int requiredNumberOfArguments)
    {_G();
        core::throwTooFewArgumentsError(givenNumberOfArguments,requiredNumberOfArguments);
//	SIMPLE_ERROR(BF("In %s not enough arguments passed - got %d and expected %d") % funcName % givenNumberOfArguments % requiredNumberOfArguments );
    }

    extern void va_throwIfExcessKeywordArguments( char* fnName, int nargs, core::T_sp* argArray, int argIdx)
    {_G();
	if ( argIdx >= nargs ) return;
        stringstream ss;
        for ( int i(0); i<nargs; ++i ) {
            ss << _rep_(argArray[i]) << " ";
        }
        SIMPLE_ERROR(BF("va_throwIfExcessKeywordArguments>> Excess keyword arguments fnName: %s argIdx: %d  args: %s") % fnName % argIdx % ss.str() );
//        core::throwUnrecognizedKeywordArgumentError(argArray[argIdx]);
#if 0
	stringstream ss;
	for ( int ii = argIdx; ii < nargs; ii+=2 )
	{
	    core::T_sp& keyRef = argArray[ii];
	    if ( keyRef == kw::_sym_allow_other_keys ) continue;
	    ss << _rep_(keyRef) << " ";
	}
	SIMPLE_ERROR(BF("In %s extraneous keyword arguments: %s") % fnName % ss.str() );
#endif
    }




    void va_fillActivationFrameWithRequiredVarargs(core::ActivationFrame_sp* afP, int nargs, core::T_sp* ap)
    {
	for ( int i=0;i<nargs;++i) {
//	    core::T_sp p = ap[i];
	    (*afP)->operator[](i) = ap[i];
	}
    }


    core::Closure* va_coerceToClosure( core::T_sp* argP )
    {_G();
	if ( !(*argP).pointerp() ) {
	    SIMPLE_ERROR(BF(" symbol %s") % _rep_((*argP)));
	}
        core::Function_sp func = core::coerce::functionDesignator((*argP));
	if ( func.nilp() ) {
	    SIMPLE_ERROR(BF("Could not coerce %s to function") % _rep_((*argP)));
	}
        return func->closure;
    }

    core::Closure* va_symbolFunction( core::Symbol_sp* symP )
    {_G();
#ifdef RUN_SAFE
	if ( !(*symP) || !(*symP).pointerp() ) {
	    SIMPLE_ERROR(BF("The head of the form %s is not a function designator") % _rep_((*symP)));
	}
#endif
        core::Function_sp func = (*symP)->_Function;
	if ( !func.pointerp() ) {
	    SIMPLE_ERROR(BF("There is no function bound to the symbol %s") % _rep_((*symP)));
	}
        return func->closure;
    }



    core::Closure* va_lexicalFunction( int depth, int index, core::T_sp* evaluateFrameP )
    {_G();
	LOG(BF("About to invoke lexicalFunction depth[%d] index[%d]") % depth % index );
        core::Function_sp func = core::Environment_O::clasp_lookupFunction(*evaluateFrameP,depth,index);
	ASSERTF(func.pointerp(), BF("UNDEFINED lexicalFunctionRead!! value depth[%d] index[%d] activationFrame: %s")
		% depth % index % _rep_(*evaluateFrameP) );
        return func->closure;
    }

    void mv_FUNCALL( core::T_mv* resultP, core::Closure* closure, LCC_ARGS )
    {
        ASSERTF(resultP,BF("mv_FUNCALL resultP is NULL!!!"));
        closure->invoke(resultP,LCC_PASS_ARGS);
    }

    void sp_FUNCALL( core::T_sp* resultP, core::Closure* closure, LCC_ARGS_BASE, ... )
    {
        ASSERTF(resultP,BF("sp_FUNCALL resultP is NULL!!!"));
        core::T_mv result_mv;
        closure->invoke(&result_mv,LCC_PASS_ARGS);
        (*resultP) = result_mv;
    }


    void mv_FUNCALL_activationFrame(core::T_mv* resultP, core::Closure* closure, core::ActivationFrame_sp af)
    {
        (*resultP) = core::eval::applyClosureToActivationFrame(closure,af);
    }

    void sp_FUNCALL_activationFrame(core::T_sp* resultP, core::Closure* closure, core::ActivationFrame_sp af)
    {
        (*resultP) = core::eval::applyClosureToActivationFrame(closure,af);
    }



    __attribute__((visibility("default")))  void va_fillRestTarget( core::T_sp* restP, int nargs, core::T_sp* argArray, int startRest, char* fnName)
    {_G();
	core::Cons_sp result = _Nil<core::Cons_O>();
	for ( int i=nargs-1; i>=startRest; i-- )
	{
	    result = core::Cons_O::create(argArray[i],result);
	}
	(*restP) = result;
	ASSERTNOTNULL(*restP);
    }


    extern int va_allowOtherKeywords(int saw_aok, int nargs, core::T_sp* argArray, int argIdx)
    {
	if (saw_aok) return saw_aok;
	bool aokTrue = argArray[argIdx+1].isTrue();
	return aokTrue ? 2 : 1;
    }


    void va_throwIfBadKeywordArgument(int allowOtherKeys, int badKwIdx, int nargs,  core::T_sp* argArray )
    {
	if ( allowOtherKeys == 2 ) return;
	if ( badKwIdx >= 0 )
	{
	    SIMPLE_ERROR(BF("Bad keyword argument %s") % _rep_(argArray[badKwIdx]) );
	}
    }
};

extern "C" {

    void newFunction_sp(core::Function_sp* sharedP)
    {_G();
	ASSERT(sharedP!=NULL);
	new(sharedP) core::Function_sp();

    }
#pragma clang diagnostic push
//#pragma clang diagnostic ignored "-Wunused-local-typedef"
    void destructFunction_sp(core::Function_sp* sharedP)
    {_G();
	ASSERT(sharedP!=NULL);
	if ( (*sharedP).pointerp() ) {
	  typedef core::Function_sp dummy;
	  (*sharedP).~dummy();
	}
    }
    void destructTsp(core::T_sp* sharedP)
    {_G();
	ASSERT(sharedP!=NULL);
	if ( (*sharedP).pointerp() ) {
	  typedef core::T_sp dummy;
	  (*sharedP).~dummy();
	}
    }
    void destructTmv(core::T_mv* sharedP)
    {_G();
	ASSERT(sharedP!=NULL);
	if ( (*sharedP).pointerp() ) {
	  typedef core::T_mv dummy;
	  (*sharedP).~dummy();
	}
    }
    void destructAFsp(core::ActivationFrame_sp* frameP)
    {_G();
	ASSERT(frameP!=NULL);
	if ( (*frameP).pointerp() ) {
	    typedef core::ActivationFrame_sp dummy;
	    (*frameP).~dummy();
	}
    }
#pragma clang diagnostic pop

    void newTsp(core::T_sp* sharedP)
    {_G();
	ASSERT(sharedP!=NULL);
	new(sharedP) core::T_sp();

    }


    void resetTsp(core::T_sp* sharedP)
    {_G();
	ASSERT(sharedP!=NULL);
	(*sharedP).reset();
    }

    void makeUnboundTsp(core::T_sp* sharedP)
    {_G();
	ASSERT(sharedP!=NULL);
	(*sharedP) = _Unbound<T_O>();
    }

    extern int compareTsp(core::T_sp* xP, core::T_sp* yP)
    {
//	ASSERT(xP!=NULL);
//	ASSERT(yP!=NULL);
	return ((*xP)==(*yP)) ? 1 : 0;
    }



    extern void copyArgs(core::T_sp* destP, int nargs, core::T_O* arg0, core::T_O* arg1, core::T_O* arg2, va_list args )
    {_G();
//        printf("%s:%d copyArgs destP=%p  nargs=%d\n", __FILE__, __LINE__, destP, nargs);
        switch (nargs) {
        case 0:
            return;
        case 1:
            destP[0] = gctools::smart_ptr<core::T_O>(arg0);
            return;
        case 2:
            destP[0] = gctools::smart_ptr<core::T_O>(arg0);
            destP[1] = gctools::smart_ptr<core::T_O>(arg1);
            return;
        case 3:
            destP[0] = gctools::smart_ptr<core::T_O>(arg0);
            destP[1] = gctools::smart_ptr<core::T_O>(arg1);
            destP[2] = gctools::smart_ptr<core::T_O>(arg2);
            return;
        default:
            destP[0] = gctools::smart_ptr<core::T_O>(arg0);
            destP[1] = gctools::smart_ptr<core::T_O>(arg1);
            destP[2] = gctools::smart_ptr<core::T_O>(arg2);
            int idx = 3;
            while (idx <nargs ) {
                destP[idx] = gctools::smart_ptr<core::T_O>(va_arg(args,core::T_O*));
                ++idx;
            }
            return;
        }
    }

    extern void sp_copyTsp(core::T_sp* destP, core::T_sp* sourceP)
    {_G();
//	ASSERT(sourceP!=NULL);
//	ASSERT(destP!=NULL);
	*destP = *sourceP;
    }


    extern void mv_copyTsp(core::T_mv* destP, core::T_sp* sourceP)
    {_G();
	ASSERT(sourceP!=NULL);
	ASSERT(destP!=NULL);
	*destP = Values(*sourceP);
    }







    void newTmv(core::T_mv* sharedP)
    {_G();
	ASSERT(sharedP!=NULL);
	new(sharedP) core::T_mv();

    }

    void resetTmv(core::T_mv* sharedP)
    {_G();
	ASSERT(sharedP!=NULL);
	(*sharedP).reset();
    }

    extern void mv_copyTmv(core::T_mv* destP, core::T_mv* sourceP)
    {_G();
	ASSERT(sourceP!=NULL);
	ASSERT(destP!=NULL);
	*destP = *sourceP;
    }

    /* This function slices a T_mv down to a T_sp */
    extern void sp_copyTmv(core::T_sp* destP, core::T_mv* sourceP)
    {_G();
	ASSERT(sourceP!=NULL);
	ASSERT(destP!=NULL);
	*destP = *sourceP;
    }






};


extern "C"
{

    /*! This copies a T_mv from source to dest */
    void mv_copyTmvOrSlice(core::T_mv* destP, core::T_mv* sourceP)
    {
//	printf("intrinsics.cc mv_copyTmvOrSlice copying %d values\n", (*sourceP).number_of_values());
	(*destP) = (*sourceP);
    }

    /*! This slices a T_mv in source down to a T_sp in dest */
    void sp_copyTmvOrSlice(core::T_sp* destP, core::T_mv* sourceP)
    {
	if ( (*sourceP).number_of_values() == 0 ) {
//	    printf("intrinsics.cc sp_copyTmvOrSlice slicing number_of_values=0\n");
	    (*destP) = _Nil<T_O>();
	} else (*destP) = (*sourceP);
    }

};

extern "C"
{

    void newAFsp(core::ActivationFrame_sp* frameP)
    {_G();
	ASSERT(frameP!=NULL);
	new(frameP) core::ActivationFrame_sp();

    }

    void newAFsp_ValueFrameOfSize(core::ActivationFrame_sp* frameP, int size)
    {_G();
	ASSERT(frameP!=NULL);
	new(frameP) core::ActivationFrame_sp();
	core::ValueFrame_sp valueFrame(core::ValueFrame_O::create(size,_Nil<core::ActivationFrame_O>()));
	(*frameP) = valueFrame;

    }


    void resetAFsp(core::ActivationFrame_sp* frameP)
    {_G();
	ASSERT(frameP!=NULL);
	(*frameP).reset();
    }

    extern void copyAFsp( core::ActivationFrame_sp* destP, core::ActivationFrame_sp* sourceP)
    {_G();
	ASSERT(sourceP!=NULL);
	ASSERT(destP!=NULL);
	*destP = *sourceP;
    }




    void sp_makeNil(core::T_sp* result)
    {_G();
	(*result) = _Nil<core::T_O>();
    }

    void mv_makeNil(core::T_mv* result)
    {_G();
	(*result) = Values(_Nil<core::T_O>());
    }


    void makeT(core::T_sp* result)
    {_G();
	(*result) = _lisp->_true();
    }


    void makeCons(core::T_sp* resultConsP, core::T_sp* carP, core::T_sp* cdrP)
    {_G();
	(*resultConsP) = core::Cons_O::create(*carP,*cdrP);
    }



    core::T_sp* getMultipleValues(int offset) 
    {
	return &_lisp->multipleValues().callingArgsStart()[offset];
    }

    /*! Return i32 1 if (valP) is != unbound 0 if it is */
    int isBound(core::T_sp* valP)
    {
	ASSERT(valP!=NULL);
	return (*valP).unboundp() ? 0 : 1;
	//return (gctools::tagged_ptr<core::T_O>::tagged_unboundp(valP)) ? 0 : 1;
    }

    /*! Return i32 1 if (valP) is != nil 0 if it is */
    int isTrue( core::T_sp* valP)
    {_G();
	ASSERT(valP!=NULL);
	return (*valP).nilp() ? 0 : 1;
//	return (gctools::tagged_ptr<core::T_O>::tagged_nilp(valP)) ? 0 : 1;
    }



    /*! Return i32 1 if (*valP) is Nil or 0 if not */
    int isNilTsp( core::T_sp* valP)
    {_G();
	ASSERT(valP!=NULL);
	return (*valP).nilp();
    }



    void internSymbol_tsp( core::T_sp* resultP, const char* symbolNameP, const char* packageNameP )
    {_G();
	core::Symbol_sp newSym = _lisp->internWithPackageName(packageNameP,symbolNameP);
#ifdef DEBUG_LOAD_TIME_VALUES
//        printf("%s:%d  internSymbol_tsp(%s::%s)  newSym.px_ref() = %p   cl::destructuring-bind.px_ref()=%p\n", __FILE__, __LINE__, packageNameP, symbolNameP, newSym.px_ref(), cl::_sym_destructuring_bind.px_ref());
#endif
	ASSERTNOTNULL(newSym);
	(*resultP) = newSym;
    }

    void makeSymbol_tsp( core::T_sp* resultP, const char* symbolNameP)
    {_G();
	core::Symbol_sp newSym = core::Symbol_O::create(symbolNameP);
	ASSERTNOTNULL(newSym);
	(*resultP) = newSym;
    }


    void internSymbol_symsp( core::Symbol_sp* resultP, const char* symbolNameP, const char* packageNameP )
    {_G();
	core::Symbol_sp newSym = _lisp->internWithPackageName(packageNameP,symbolNameP);
	ASSERTNOTNULL(newSym);
	(*resultP) = newSym;
    }

    void makeSymbol_symsp( core::Symbol_sp* resultP, const char* symbolNameP)
    {_G();
	core::Symbol_sp newSym = core::Symbol_O::create(symbolNameP);
	ASSERTNOTNULL(newSym);
	(*resultP) = newSym;
    }




    void makeFixnum( core::T_sp* fnP, int s)
    {_G();
	ASSERT(fnP!=NULL);
	(*fnP) = core::Fixnum_sp(core::Fixnum_O::create(s));
    }

    void makeCharacter( core::T_sp* fnP, int s)
    {_G();
	ASSERT(fnP!=NULL);
	(*fnP) = core::StandardChar_O::create((char)s);
    }

    void makeBignum( core::T_sp* fnP, const char* cP)
    {_G();
	ASSERT(fnP!=NULL);
	string str = cP;
	core::Bignum_sp ns = core::Bignum_O::make(str);
	(*fnP) = ns;
    }




    void makeString( core::T_sp* fnP, const char* str)
    {_G();
// placement new into memory passed into this function
	ASSERT(fnP!=NULL);
	core::Str_sp ns = core::Str_O::create(str);
	(*fnP) = ns;
    }

    void makePathname( core::T_sp* fnP, const char* cstr)
    {_G();
// placement new into memory passed into this function
	ASSERT(fnP!=NULL);

        core::Str_sp str = core::Str_O::create(cstr);
	core::Pathname_sp ns = core::cl_pathname(str);
	(*fnP) = ns;
    }


    void makeShortFloat( core::T_sp* fnP, double s)
    {_G();
	ASSERT(fnP!=NULL);
	(*fnP) = core::ShortFloat_sp(core::ShortFloat_O::create(s));
    }

    void makeSingleFloat( core::T_sp* fnP, float s)
    {_G();
	ASSERT(fnP!=NULL);
	(*fnP) = core::SingleFloat_sp(core::SingleFloat_O::create(s));
    }

    void makeDoubleFloat( core::T_sp* fnP, double s)
    {_G();
	ASSERT(fnP!=NULL);
	(*fnP) = core::DoubleFloat_sp(core::DoubleFloat_O::create(s));
    }

    void makeComplex( core::T_sp* fnP, double r, double i)
    {_G();
	ASSERT(fnP!=NULL);
	(*fnP) = core::Complex_sp(core::Complex_O::create(r,i));
    }

#ifdef CLASP_LONG_FLOAT
    void makeLongFloat( core::T_sp* fnP, LongFloat s)
    {_G();
	ASSERT(fnP!=NULL);
	(*fnP) = core::LongFloat_sp(core::LongFloat_O::create(s));
    }
#endif

};



core::T_sp proto_makeCompiledFunction(fnLispCallingConvention funcPtr, char* sourceName, int lineno, int column, core::T_sp* functionNameP, core::T_sp* compiledFuncsP, core::ActivationFrame_sp* frameP )
{_G();
    // TODO: If a pointer to an integer was passed here we could write the sourceName SourceFileInfo_sp index into it for source line debugging
    core::Str_sp sourceStr = core::Str_O::create(sourceName);
    core::SourceFileInfo_mv sfi = core::af_sourceFileInfo(sourceStr);
    int sfindex = sfi.valueGet(1).as<core::Fixnum_O>()->get();   // sfindex could be written into the Module global for debugging
    core::SourcePosInfo_sp spi = core::SourcePosInfo_O::create(sfindex,lineno,column);
    core::FunctionClosure* closure = gctools::ClassAllocator<llvmo::CompiledClosure>::allocateClass(*functionNameP,spi,kw::_sym_function,funcPtr,_Nil<llvmo::Function_O>(),*frameP,*compiledFuncsP);
    core::CompiledFunction_sp compiledFunction = core::CompiledFunction_O::make(closure);
    return compiledFunction;
};
extern "C"
{
    void sp_makeCompiledFunction( core::T_sp* resultCompiledFunctionP, fnLispCallingConvention funcPtr, char* sourceName, int lineno, int column, core::T_sp* functionNameP, core::T_sp* compiledFuncsP, core::ActivationFrame_sp* frameP )
    {
	(*resultCompiledFunctionP) = proto_makeCompiledFunction(funcPtr,sourceName,lineno,column,functionNameP,compiledFuncsP,frameP);
    }

    void mv_makeCompiledFunction( core::T_mv* resultCompiledFunctionP, fnLispCallingConvention funcPtr, char* sourceName, int lineno, int column, core::T_sp* functionNameP, core::T_sp* compiledFuncsP, core::ActivationFrame_sp* frameP )
    {
	(*resultCompiledFunctionP) = Values(proto_makeCompiledFunction(funcPtr,sourceName,lineno,column,functionNameP,compiledFuncsP,frameP));
    }

};



extern "C"
{
    void invokeLlvmFunction( core::T_mv* resultP,
			     fnLispCallingConvention fptr,
			     core::ActivationFrame_sp* frameP,
                             int* sourceFileInfoHandleP,
                             int lineno,
                             int column )
    {
	core::T_sp closedEnv = _Nil<T_O>();
	ActivationFrame_sp frame = (*frameP);
        core::InvocationHistoryFrame invFrame(*sourceFileInfoHandleP,lineno,column);
	if ( frame.nilp() ) {
	    fptr(resultP,LCC_FROM_SMART_PTR(closedEnv),LCC_PASS_ARGS0());
	} else {
            DEPRECIATED();
	    //fptr(resultP,closedEnv,frame->length(),frame->argArray());
	}
	ASSERTNOTNULL(*resultP);
    };


    void invokeMainFunctions( fnVoidType fptr[], int* numfunP)
    {
        int numfun = *numfunP;
//        printf("%s:%d invokeMainFunctions(%d)\n", __FILE__, __LINE__, numfun);
        for ( int i=0; i<numfun; ++i ) {
            (fptr[i])();
        }
    }


    void invokeLlvmFunctionVoid( fnVoidType fptr)
    {
	fptr();
    };


    __attribute__((visibility("default"))) void invokeFASLLlvmFunctionVoid( fnVoidType fptr, char* fileName )
    {_G();
//	IMPLEMENT_MEF(BF("Figure out what to do in this case"));
//	core::TopLevelIHF frame(_lisp->invocationHistoryStack(),_Nil<T_O>());
	fptr();
    };



    extern void sp_symbolValueReadOrUnbound(core::T_sp* resultP, const core::Symbol_sp* symP)
    {_G();
	ASSERTF(symP!=NULL,BF("passed symbol is NULL"));
	*resultP = (*symP)->symbolValueUnsafe();
	ASSERTNOTNULL(*resultP);
    }
    extern void mv_symbolValueReadOrUnbound(core::T_mv* resultP, const core::Symbol_sp* symP)
    {_G();
	ASSERTF(symP!=NULL,BF("passed symbol is NULL"));
	*resultP = Values((*symP)->symbolValueUnsafe());
	ASSERTNOTNULL(*resultP);
    }





    extern void sp_symbolValueRead(core::T_sp* resultP, const core::Symbol_sp* symP)
    {_G();
	ASSERTF(symP!=NULL,BF("passed symbol is NULL"));
	*resultP = (*symP)->symbolValue();
	ASSERTNOTNULL(*resultP);
    }
    extern void mv_symbolValueRead(core::T_mv* resultP, const core::Symbol_sp* symP)
    {_G();
	ASSERTF(symP!=NULL,BF("passed symbol is NULL"));
	*resultP = Values((*symP)->symbolValue());
	ASSERTNOTNULL(*resultP);
    }

    core::T_sp*  symbolValueReference(core::Symbol_sp* symbolP)
    {_G();
	ASSERT(symbolP!=NULL);
	return ((*symbolP)->valueReference());
    }

    extern core::T_sp* lexicalValueReference(int depth, int index, core::ActivationFrame_sp* frameP)
    {_G();
	ASSERT(frameP!=NULL);
	return const_cast<core::T_sp*>(&((*frameP)->lookupValueReference(depth,index)));
    }


};

core::T_sp proto_lexicalValueRead( int depth, int index, core::ActivationFrame_sp* renvP)
{_G();
    ASSERT(renvP!=NULL);
    LOG(BF("About to lookupValue depth[%d] index[%d]") % depth % index );
    LOG(BF("(*renvP) --> %s") % (*renvP)->__repr__() );
    ActivationFrame_sp renv = *renvP;
    ASSERTF(renv,BF("lexicalValueRead is NULL depth[%d] index[%d] activationFrame: %s") % depth % index % _rep_((*renvP)));
    core::T_sp res = core::Environment_O::clasp_lookupValue(renv,depth,index); //renv->lookupValue(depth,index);
    return res;
}

extern "C"
{
    void sp_lexicalValueRead( core::T_sp* resultP, int depth, int index, core::ActivationFrame_sp* renvP)
    {
	(*resultP) = proto_lexicalValueRead(depth,index,renvP);
	ASSERTNOTNULL(*resultP);
    }
    void mv_lexicalValueRead( core::T_mv* resultP, int depth, int index, core::ActivationFrame_sp* renvP)
    {
	(*resultP) = Values(proto_lexicalValueRead(depth,index,renvP));
	ASSERTNOTNULL(*resultP);
    }
};


extern "C"
{

    extern void makeValueFrame(core::T_sp* resultActivationFrameP, int numargs, int id)
    // was ActivationFrame_sp
    {_G();
	ASSERT(resultActivationFrameP!=NULL);
	core::ValueFrame_sp valueFrame(core::ValueFrame_O::create(numargs,_Nil<core::ActivationFrame_O>()));
	valueFrame->setEnvironmentId(id);
	(*resultActivationFrameP) = valueFrame;
//        printf("%s:%d makeValueFrame address &result->%p (&result)->px->%p valueFrame->%p\n", __FILE__, __LINE__, resultActivationFrameP, resultActivationFrameP->px_ref(), valueFrame.px_ref() );
    }

    extern void makeTagbodyFrame(core::ActivationFrame_sp* resultP)
    // was ActivationFrame_sp
    {_G();
	ASSERT(resultP!=NULL);
	core::TagbodyFrame_sp tagbodyFrame(core::TagbodyFrame_O::create(_Nil<core::ActivationFrame_O>()));
	(*resultP) = tagbodyFrame;
	ASSERTNOTNULL(*resultP);
    }



    extern void makeValueFrameFromReversedCons(core::ActivationFrame_sp* afP, core::T_sp* consP, uint id )
    {_G();
	core::Cons_sp cons = (*consP).as<core::Cons_O>();
	core::ValueFrame_sp vf = core::ValueFrame_O::createFromReversedCons(cons,_Nil<core::ActivationFrame_O>());
	vf->setEnvironmentId(id);
	(*afP) = vf;
	ASSERTNOTNULL(*afP);
    }


    extern void setParentOfActivationFrameTPtr( core::T_sp* resultP, core::T_O* parentP)
    {
        if ( resultP->framep() ) {
            frame::SetParentFrame(*resultP,parentP);
            return;
        } else if (ActivationFrame_sp af = (*resultP).as<ActivationFrame_O>() ) {
            af->setParentFrame(parentP);
            return;
        }
        SIMPLE_ERROR(BF("Destination was not ActivationFrame"));
    }

    extern void setParentOfActivationFrame( core::T_sp* resultP, core::T_sp* parentsp)
    {
	T_O* parentP = parentsp->px_ref();
        if ( resultP->framep() ) {
            frame::SetParentFrame(*resultP,parentP);
            return;
        } else if (ActivationFrame_sp af = (*resultP).as<ActivationFrame_O>() ) {
            af->setParentFrame(parentP);
            return;
        }
        SIMPLE_ERROR(BF("Destination was not ActivationFrame"));
    }







    extern void attachDebuggingInfoToValueFrame(core::ActivationFrame_sp* resultP,
						core::T_sp* debuggingInfoP )
    {_G();
	ASSERT(resultP!=NULL);
	ASSERT(debuggingInfoP!=NULL);
	ASSERT((*resultP));
	core::ValueFrame_sp vf = (*resultP).as<core::ValueFrame_O>();
	core::VectorObjects_sp vo = (*debuggingInfoP).as<core::VectorObjects_O>();
	ASSERTF(vf->length()==vo->length(),BF("There is a mismatch between the size of the ValueFrame[%d] and the number of Symbols[%d] attaching to it") % vf->length() % vo->length() );
	vf->attachDebuggingInfo(vo);
    }


    ALWAYS_INLINE extern core::T_sp* valueFrameReference(core::ActivationFrame_sp* frameP, int idx)
    {_G();
	ASSERT(frameP!=NULL);
	ASSERT((*frameP));
	ASSERTF(idx>=0 && idx<((*frameP)->length()),BF("Illegal value of idx[%d] must be in range [0<=idx<%d]") % idx % (*frameP)->length());
	core::ValueFrame_sp frame = (*frameP).pointerAsUnsafe<core::ValueFrame_O>();
        core::T_sp* pos_gc_safe = const_cast<core::T_sp*>(&frame->entryReference(idx));
	return pos_gc_safe;
    }



    ALWAYS_INLINE extern core::T_sp* valueFrameReferenceWithOffset(core::ActivationFrame_sp* frameP, int idx, int offset)
    {_G();
	int ridx = idx+offset;
	ASSERT(frameP!=NULL);
	ASSERT(*frameP);
	ASSERT(ridx>=0 && ridx<(*frameP)->length());
	core::ValueFrame_sp frame = (*frameP).pointerAsUnsafe<core::ValueFrame_O>();
	core::T_sp* pos_gc_safe = const_cast<core::T_sp*>(&frame->entryReference(ridx));
	return pos_gc_safe;
    }


    extern void makeFunctionFrame(core::ActivationFrame_sp* resultP, int numargs, core::ActivationFrame_sp* parentP)
    // was ActivationFrame_sp
    {_G();
	ASSERT(resultP!=NULL);
	ASSERT(parentP!=NULL);
	(*resultP) = core::FunctionFrame_sp(core::FunctionFrame_O::create(numargs,(*parentP)));
	ASSERTNOTNULL(*resultP);
    }


    extern core::T_sp* functionFrameReference(core::ActivationFrame_sp* frameP, int idx)
    {_G();
	ASSERT(frameP!=NULL);
	ASSERT(frameP->pointerp());
	core::FunctionFrame_sp frame = (*frameP).as<core::FunctionFrame_O>();
	if ( idx < 0 || idx >= frame->length())
	{
	    SIMPLE_ERROR(BF("Invalid index[%d] for FunctionFrame(size=%d)") % idx % frame->length());
	}
	core::T_sp* pos_gc_safe = const_cast<core::T_sp*>(&frame->entryReference(idx));
	return pos_gc_safe;
    }








    extern void fillRestTarget( core::T_sp* restP, core::ActivationFrame_sp* frameP, int startRest, char* fnName)
    {_G();
	ASSERT(frameP!=NULL);
	ASSERT(frameP->pointerp());
	core::ValueFrame_sp frame = (*frameP).as<core::ValueFrame_O>();
	core::Cons_sp result = _Nil<core::Cons_O>();
	for ( int i=frame->length()-1; i>=startRest; i-- )
	{
	    result = core::Cons_O::create(frame->entry(i),result);
	}
	(*restP) = result;
	ASSERTNOTNULL(*restP);
    }


    /*! Look for the :allow-other-keywords XX keyword argument and
      calculate (or (*ampAllowOtherKeywordsP) XX) return 1 if result is true otherwise 0 */
    extern int checkForAllowOtherKeywords(int ampAllowOtherKeywords, core::ActivationFrame_sp* frameP, int argIdx)
    {_G();
	ASSERT(frameP!=NULL);
	ASSERT(frameP->pointerp());
	ASSERT(argIdx>=0);
	core::ValueFrame_sp vf = (*frameP).as<core::ValueFrame_O>();
	if ( argIdx >= vf->length() ) return 0;
	int argsLeft = vf->length()-argIdx;
	if ( (argsLeft % 2) != 0 )
	{
	    stringstream serr;
	    serr << "There must be an even number of keyword arguments - you passed: ";
	    for ( int ei = argIdx; ei<vf->length(); ei++ )
	    {
		serr << _rep_(vf->entry(ei)) << " ";
	    }
	    SIMPLE_ERROR(BF("%s") % serr.str() );
	}
	if (ampAllowOtherKeywords) return 1;
	for ( int ii = argIdx; ii < vf->length(); ii+=2 )
	{
	    if ( vf->entry(ii) == kw::_sym_allow_other_keys )
	    {
		core::T_sp val = vf->entry(ii+1);
		if ( val.isTrue() ) return 1;
		// TODO: Handle :allow-other-keys nil :allow-other-keys t
		// In safe mode this should throw an exceptions
		// (see 3.4.1.4.1.1 Examples of Suppressing Keyword Argument Checking)
	    }
	}
	return 0;
    }




/*! Look for the keyword in (*frameP) after argIdx.
 */
    extern void throwIfExcessKeywordArguments( char* fnName, core::ActivationFrame_sp* frameP, int argIdx)
    {_G();
	ASSERT(frameP!=NULL);
	ASSERT(frameP->pointerp());
	ASSERT(argIdx>=0);
	if ( argIdx >= (*frameP)->length() ) return;
        stringstream ss;
        for ( int i(0); i<(*frameP)->length(); ++i ) {
            ss << _rep_((*frameP)->entry(i)) << " ";
        }
        SIMPLE_ERROR(BF("Excess keyword arguments fnName: %s argIdx: %d  args: %s") % fnName % argIdx % ss.str() );
//      core::throwUnrecognizedKeywordArgumentError((*frameP)->argArray()[argIdx]);
#if 0
	core::ActivationFrame_sp frame = (*frameP).as<core::ActivationFrame_O>();
	if ( argIdx >= frame->length() ) return;
	stringstream ss;
	for ( int ii = argIdx; ii < frame->length(); ii+=2 )
	{
	    core::T_sp& keyRef = frame->entryReference(ii);
	    if ( keyRef == kw::_sym_allow_other_keys ) continue;
	    ss << _rep_(keyRef) << " ";
	}
	SIMPLE_ERROR(BF("In %s extraneous keyword arguments: %s") % fnName % ss.str() );
#endif
    }


    extern void throwIfExcessArguments( core::T_sp* frameP, int argIdx)
    {_G();
	ASSERT(frameP!=NULL);
	ASSERT(frameP->pointerp());
	ASSERT(argIdx>=0);
	ASSERT(argIdx<(*frameP).as<core::Cons_O>()->length());
	core::ActivationFrame_sp frame = (*frameP).as<core::ActivationFrame_O>();
	if ( argIdx < frame->length() )
	{
	    stringstream serr;
	    for ( int i=argIdx; i<frame->length(); i++ )
	    {
		serr << _rep_(frame->entry(i)) << " ";
	    }
	    SIMPLE_ERROR(BF("extraneous arguments: %s") % serr.str() );
	}
    }

};



inline core::T_sp prependMultipleValues(core::T_mv* multipleValuesP)
{_G();
    core::Cons_sp result = _Nil<core::Cons_O>();
    core::T_mv& mv = (*multipleValuesP);
    if ( mv.number_of_values() > 0 )
    {
	result = core::Cons_O::create(mv,result);
	for ( int i=1; i<mv.number_of_values(); i++ )
	{
	    result = core::Cons_O::create(mv.valueGet(i),result);
	}
    }
    return result;
}

extern "C"
{
    void sp_prependMultipleValues(core::T_sp* resultP, core::T_mv* multipleValuesP)
    {
	(*resultP) = prependMultipleValues(multipleValuesP);
	ASSERTNOTNULL(*resultP);
    }
    void mv_prependMultipleValues(core::T_mv* resultP, core::T_mv* multipleValuesP)
    {
	(*resultP) = Values(prependMultipleValues(multipleValuesP));
	ASSERTNOTNULL(*resultP);
    }
};



extern "C"
{

/*! Invoke a symbol function with the given arguments and put the result in (*resultP) */
    void sp_symbolFunctionRead(core::T_sp* resultP, const core::Symbol_sp* symP)
    {_G();
	ASSERT(resultP!=NULL);
	ASSERTF(symP!=NULL,BF("passed symbol is NULL"));
	ASSERTF((*symP)->fboundp(),BF("There is no function bound to symbol[%s]") % _rep_((*symP)));
	(*resultP) = (*symP)->symbolFunction();
	ASSERTNOTNULL(*resultP);
    }
    void mv_symbolFunctionRead(core::T_mv* resultP, const core::Symbol_sp* symP)
    {_G();
	ASSERT(resultP!=NULL);
	ASSERTF(symP!=NULL,BF("passed symbol is NULL"));
	ASSERTF((*symP)->fboundp(),BF("There is no function bound to symbol[%s]") % _rep_((*symP)));
	(*resultP) = Values((*symP)->symbolFunction());
	ASSERTNOTNULL(*resultP);
    }



/*! Invoke a symbol function with the given arguments and put the result in (*resultP) */
    extern void setfSymbolFunctionRead(core::T_sp* resultP, const core::Symbol_sp* symP)
    {_G();
	ASSERT(resultP!=NULL);
	ASSERTF(symP!=NULL,BF("passed symbol is NULL"));
	core::Function_sp setfFunc = _lisp->get_setfDefinition(*symP);
	ASSERTF(setfFunc,BF("There is no setf function bound to symbol[%s]") % _rep_((*symP)));
	(*resultP) = setfFunc;
	ASSERTNOTNULL(*resultP);
    }



};

core::T_sp proto_lexicalFunctionRead(int depth, int index, core::ActivationFrame_sp* renvP)
{
    ASSERT(renvP!=NULL);
    LOG(BF("About to lexicalFunction depth[%d] index[%d]") % depth % index );
    LOG(BF("(*renvP) --> %s") % (*renvP)->__repr__() );
    core::Function_sp res = core::Environment_O::clasp_lookupFunction((*renvP),depth,index);
    if ( !res.pointerp() )
    {
	SIMPLE_ERROR(BF("UNDEFINED lexicalFunctionRead!! value depth[%d] index[%d] activationFrame: %s")
			   % depth % index % _rep_((*renvP)) );
    }
    return res;
}

extern "C"
{
    void sp_lexicalFunctionRead(core::T_sp* resultP, int depth, int index, core::ActivationFrame_sp* renvP)
    {
	(*resultP) = proto_lexicalFunctionRead(depth,index,renvP);
	ASSERTNOTNULL(*resultP);
    }
    void mv_lexicalFunctionRead(core::T_mv* resultP, int depth, int index, core::ActivationFrame_sp* renvP)
    {
	(*resultP) = Values(proto_lexicalFunctionRead(depth,index,renvP));
	ASSERTNOTNULL(*resultP);
    }
};


extern "C"
{
    int activationFrameSize(core::ActivationFrame_sp* activationFrameP)
    {_G();
	ASSERT(activationFrameP!=NULL);
	return (*activationFrameP)->length();
    }

#if 0
/*! Return a pointer to the shared_ptr of the activation frames parent */
    extern core::ActivationFrame_sp* activationFrameParentRef(core::T_sp* frameP)
    {_G();
	ASSERT(frameP!=NULL);
	if ( (*frameP).nilp() ) return &(*frameP); // _Nil<core::ActivationFrame_O>();
	return &((*frameP)->parentFrameRef());
    }

/*! Return a pointer to the shared_ptr of the activation frames parent */
    extern core::ActivationFrame_sp* activationFrameNil()
    {_G();
	return &_lisp->activationFrameNil();
    }
#endif


    void throwTooManyArgumentsException( const char* funcName, core::ActivationFrame_sp* afP, int givenNumberOfArguments, int requiredNumberOfArguments)
    {_G();
        core::throwTooManyArgumentsError(givenNumberOfArguments,requiredNumberOfArguments);
//	SIMPLE_ERROR(BF("In %s too many arguments passed - got %d and expected %d - args: %s") % funcName % givenNumberOfArguments % requiredNumberOfArguments % _rep_((*(afP))) );
    }


    void throwNotEnoughArgumentsException( const char* funcName, core::ActivationFrame_sp* afP, int givenNumberOfArguments, int requiredNumberOfArguments)
    {_G();
        core::throwTooFewArgumentsError(givenNumberOfArguments,requiredNumberOfArguments);
	SIMPLE_ERROR(BF("In %s not enough arguments passed - got %d and expected %d - args: %s") % funcName % givenNumberOfArguments % requiredNumberOfArguments % _rep_((*(afP))) );
    }



    void gdb()
    {_G();
	printf("%s:%d Set a breakpoint here to invoke gdb\n",__FILE__,__LINE__);
    }



    void debugInspectActivationFrame(core::ActivationFrame_sp* afP)
    {_G();
	core::ActivationFrame_sp af = (*afP);
	printf("debugInspectActivationFrame: %s\n", _rep_(af).c_str() );
	printf("%s:%d Insert breakpoint here if you want to inspect af\n", __FILE__,__LINE__);
    }

    void debugInspectT_sp(core::T_sp* objP)
    {_G();
	core::T_sp obj = (*objP);
	printf("debugInspectObject@%p  obj.px_ref()=%p: %s\n", (void*)objP, (*objP).px_ref(), _rep_(obj).c_str() );
	printf("%s:%d Insert breakpoint here if you want to inspect object\n", __FILE__,__LINE__);
    }

    void debugInspectTPtr(core::T_O* tP)
    {
        core::T_sp obj = gctools::smart_ptr<core::T_O>(tP);
	printf("debugInspectTPtr@%p  obj.px_ref()=%p: %s\n", (void*)tP, obj.px_ref(), _rep_(obj).c_str() );
	printf("%s:%d Insert breakpoint here if you want to inspect object\n", __FILE__,__LINE__);
    }

    void debugInspectT_mv(core::T_mv* objP)
    {_G();
	printf("debugInspectT_mv@%p\n", (void*)objP);
        printf("   debugInspectT_mv  obj= %s\n", _rep_(*objP).c_str() );
	printf("%s:%d Insert breakpoint here if you want to inspect object\n", __FILE__,__LINE__);
    }


    void debugMsg( const char* msg)
    {_G();
	printf("++++++ JIT-TRACE: %s \n", msg );
    }

    void debugPointer( const unsigned char* ptr)
    {_G();
	printf("++++++ debugPointer: %p \n", ptr );
    }

    void debugSymbolPointer( core::Symbol_sp* ptr)
    {_G();
	printf("++++++ debugSymbolPointer: %s\n", _rep_(*ptr).c_str());
    }

    void debugSymbolValue( core::Symbol_sp sym)
    {_G();
	printf("+++++ debugSymbolValue: %s\n", _rep_(core::af_symbolValue(sym)).c_str() );
    }


    void debugPrintObject( const char* msg, core::T_sp* objP )
    {_G();
	std::cout << "++++++ JIT-PRINT-OBJECT: ";
	std::cout << msg << " --> ";
	if ( objP==NULL )
	{
	    std::cout << "objP is UNDEFINED";
	} else if ( !(*objP).pointerp() )
	{
	    std::cout << "(*objP) is UNDEFINED";
	} else
	{
	    std::cout << _rep_((*objP));
	}
	std::cout << std::endl;
    }


    void debugPrintI32( int i32 )
    {_G();
	printf( "+++DBG-I32[%d]\n", i32);
    }


    void singleStepCallback()
    {
    }


    void throwCatchThrow(core::T_sp* tagP)
    {
	ASSERT(tagP!=NULL);
	core::T_sp tag = *tagP;
	int frame = _lisp->exceptionStack().findKey(CatchFrame,tag);
	if ( frame < 0 )
	{
	    CONTROL_ERROR();
	} else
	{
	    core::CatchThrow catchThrow(frame);
#ifdef DEBUG_FLOW_CONTROL
	    printf("Throwing core::CatchThrow exception tag[%s]\n", (*tagP)->__repr__().c_str());
#endif
	    throw catchThrow;
	}
	SIMPLE_ERROR(BF("This should never happen"));
    }


    void throwReturnFrom(core::Symbol_sp* blockSymbolP)
    {
        ASSERT(blockSymbolP!=NULL);
	core::T_sp blockSymbol = *blockSymbolP;
	int frame = _lisp->exceptionStack().findKey(BlockFrame,blockSymbol);
	if ( frame < 0 ) {CONTROL_ERROR();}
#ifdef DEBUG_FLOW_CONTROL
	printf("Throwing core::ReturnFrom exception frame[%d]\n", frame);
#endif
	core::ReturnFrom returnFrom(frame);
	throw returnFrom;
    }


};


core::T_mv  proto_blockHandleReturnFrom( unsigned char* exceptionP, int frame)
{
    core::ReturnFrom& returnFrom = (core::ReturnFrom&)*((core::ReturnFrom*)(exceptionP));
    if ( returnFrom.getFrame() == frame )
    {
	return gctools::multiple_values<T_O>::createFromValues();
    }
#ifdef DEBUG_FLOW_CONTROL
    printf("Re-throwing core::ReturnFrom exception frame[%d]\n", returnFrom.getFrame());
#endif
    throw returnFrom;
}

extern "C"
{

    void sp_blockHandleReturnFrom( core::T_sp* resultP, unsigned char* exceptionP, int frame)
    {
	(*resultP) = proto_blockHandleReturnFrom(exceptionP,frame);
	ASSERTNOTNULL(*resultP);
    }
    void mv_blockHandleReturnFrom( core::T_mv* resultP, unsigned char* exceptionP, int frame)
    {
	(*resultP) = proto_blockHandleReturnFrom(exceptionP,frame);
	ASSERTNOTNULL(*resultP);
    }

};


extern "C"
{


// Return 1 if exception depth is zero				- blockTestDepth0
// Set the result from the ReturnFrom exception return value   	- blockStoreResult
//

    void cando_terminate(char* fileName, int lineno, int col, char* fnName)
    {
	printf("%s:%d - terminate called from file %s, lineno %d, col %d, function %s\n", __FILE__, __LINE__, fileName, lineno, col, fnName);
//	throw core::ExitProgram(0);
	std::terminate();
    }




    int pushCatchFrame(core::T_sp* tagP)
    {_G();
	ASSERT(tagP!=NULL);
        return _lisp->exceptionStack().push(CatchFrame, *tagP);
    }

    int pushBlockFrame(core::Symbol_sp* tagP)
    {_G();
	ASSERT(tagP!=NULL);
        core::T_sp osym = *tagP;
        return _lisp->exceptionStack().push(BlockFrame, osym);
    }

    int pushTagbodyFrame(core::ActivationFrame_sp* afP)
    {_G();
	ASSERT(afP!=NULL);
        core::T_sp tagbodyId = (*afP);
        return _lisp->exceptionStack().push(TagbodyFrame, tagbodyId);
    }



};



core::T_mv proto_ifCatchFrameMatchesStoreResultElseRethrow(int catchFrame, unsigned char* exceptionP)
{_G();
    core::CatchThrow* ctExceptionP = reinterpret_cast<core::CatchThrow*>(exceptionP);
    if ( catchFrame == ctExceptionP->getFrame() )
    {
	return gctools::multiple_values<core::T_O>::createFromValues(); // ctExceptionP->getReturnedObject();
    }
    // rethrow the exception
#ifdef	DEBUG_FLOW_CONTROL
    printf("Re-throwing CatchThrow tag[%s]\n", ctExceptionP->getThrownTag()->__repr__().c_str() );
#endif
    throw *ctExceptionP;
}


extern "C"
{
    void sp_ifCatchFrameMatchesStoreResultElseRethrow(core::T_sp* resultP, int catchFrame, unsigned char* exceptionP)
    {
	(*resultP) = proto_ifCatchFrameMatchesStoreResultElseRethrow(catchFrame,exceptionP);
	ASSERTNOTNULL(*resultP);
    }
    void mv_ifCatchFrameMatchesStoreResultElseRethrow(core::T_mv* resultP, int catchFrame, unsigned char* exceptionP)
    {
	(*resultP) = proto_ifCatchFrameMatchesStoreResultElseRethrow(catchFrame,exceptionP);
	ASSERTNOTNULL(*resultP);
    }
};



extern "C"
{
    void exceptionStackUnwind(int frame)
    {_G();
	_lisp->exceptionStack().unwind(frame);
    }


    void throwIllegalSwitchValue(int val, int max)
    {_G();
	SIMPLE_ERROR(BF("Illegal switch value %d - max value is %d") % val % max );
    }


    void throw_LexicalGo(int depth, int index)
    {_G();
	core::LexicalGo lgo(depth,index);
#ifdef DEBUG_FLOW_CONTROL
	printf("Throwing core::Go depth[%d] index[%d]\n", depth, index);
#endif
	throw lgo;
    }

    void throwDynamicGo(int depth, int index, core::ActivationFrame_sp* afP)
    {_G();
	T_sp tagbodyId = core::Environment_O::clasp_lookupTagbodyId((*afP),depth,index);
        int frame = _lisp->exceptionStack().findKey(TagbodyFrame,tagbodyId);
	core::DynamicGo dgo(frame,index);
#ifdef DEBUG_FLOW_CONTROL
	printf("Throwing core::DynamicGo tagbodyIdP[%p] index[%d]\n", (void*)((*tagbodyIdP).get()), index);
#endif
	throw dgo;
    }


    int tagbodyLexicalGoIndexElseRethrow(char* exceptionP)
    {
        IMPLEMENT_MEF(BF("Update me"));
#if 0
	core::LexicalGo* goExceptionP = (core::LexicalGo*)(exceptionP);
	if ( goExceptionP->depth() == 0 )
	{
	    return goExceptionP->index();
	}
	goExceptionP->decrementDepth();
#ifdef DEBUG_FLOW_CONTROL
	printf("Re-throwing core::Go depth[%d] index[%d]\n", goExceptionP->depth(), goExceptionP->index());
#endif
	throw *goExceptionP;
#endif
    }


    int tagbodyDynamicGoIndexElseRethrow(char* exceptionP, int frame)
    {
        core::DynamicGo* goExceptionP = reinterpret_cast<core::DynamicGo*>(exceptionP);
	if ( goExceptionP->getFrame() == frame )
	{
	    return goExceptionP->index();
	}
#ifdef DEBUG_FLOW_CONTROL
	printf("Re-throwing core::DynamicGo tagbodyId[%p] index[%d]\n", goExceptionP.getPointer()->tagbodyId().get(), goExceptionP.getPointer()->index());
#endif
	throw *goExceptionP;
    }


    void getOrCreateLoadTimeValueArray(core::LoadTimeValues_O** ltvPP, const char* moduleName, int numberOfLoadTimeValues, int numberOfLoadTimeSymbols )
    {
	core::LoadTimeValues_sp loadTimeValues = _lisp->getOrCreateLoadTimeValues(moduleName,numberOfLoadTimeValues, numberOfLoadTimeSymbols);
	*ltvPP = reinterpret_cast<core::LoadTimeValues_O*>(loadTimeValues.pbase());
    }


    void dumpLoadTimeValues(core::LoadTimeValues_O* ltvP)
    {
	ltvP->dump();
    }


    void assignSourceFileInfoHandle(const char* moduleName, int* sourceFileInfoHandleP)
    {
        core::Str_sp mname = core::Str_O::create(moduleName);
        SourceFileInfo_mv sfi_mv = core::af_sourceFileInfo(mname);
        int sfindex = sfi_mv.valueGet(1).as<core::Fixnum_O>()->get();
        *sourceFileInfoHandleP = sfindex;
    }

    void debugSourceFileInfoHandle(int* sourceFileInfoHandleP)
    {
        int sfindex = *sourceFileInfoHandleP;
        core::Fixnum_sp fn = core::Fixnum_O::create(sfindex);
        SourceFileInfo_sp sfi = core::af_sourceFileInfo(fn);
        printf("%s:%d debugSourceFileInfoHandle[%d] --> %s\n", __FILE__, __LINE__, sfindex, _rep_(sfi).c_str());
    }
};


inline core::T_sp proto_copyLoadTimeValue(core::LoadTimeValues_O** ltvPP, int index)
{
    core::LoadTimeValues_O& ltv = **ltvPP;
    return ltv.data_element(index);
}
extern "C"
{
    void sp_copyLoadTimeValue(core::T_sp* resultP, core::LoadTimeValues_O** ltvPP, int index)
    {
	ASSERT(resultP!=NULL);
	ASSERT(ltvPP!=NULL);
	ASSERT((*ltvPP)!=NULL);
	core::T_sp val = proto_copyLoadTimeValue(ltvPP,index);
#if defined(USE_MPS) && defined(DEBUG_LOAD_TIME_VALUES)
        if (core::_sym_STARdebugLoadTimeValuesSTAR && core::_sym_STARdebugLoadTimeValuesSTAR->symbolValue().notnilp() ) {
            stringstream ss;
            ss << (BF("%s:%d sp_copyLoadTimeValue@%p  _Objects@%p  index[%d]  result client@%p  value: %s  cl::_sym_destructuring_bind@%p") % __FILE__ % __LINE__ % *ltvPP % (*ltvPP)->_Objects._Vector._Contents % index % val.pbase() % _rep_(val).c_str() % cl::_sym_destructuring_bind.pbase()).str();
            printf("%s\n", ss.str().c_str());
        }
#endif
	(*resultP) = val;
	ASSERTNOTNULL(*resultP);
    }
    void mv_copyLoadTimeValue(core::T_mv* resultP, core::LoadTimeValues_O** ltvPP, int index)
    {
	ASSERT(resultP!=NULL);
	ASSERT(ltvPP!=NULL);
	ASSERT((*ltvPP)!=NULL);
	(*resultP) = Values(proto_copyLoadTimeValue(ltvPP,index));
#ifdef DEBUG_LOAD_TIME_VALUES
//        printf("%s:%d mv_copyTimeValue@%p  index[%d]  result client@%p  value: %s\n", __FILE__, __LINE__, *ltvPP, index, (*resultP).pbase(), _rep_(*resultP).c_str());
#endif
	ASSERTNOTNULL(*resultP);
    }
};


extern "C"
{


    core::T_sp* loadTimeValueReference(core::LoadTimeValues_O** ltvPP, int index)
    {
	ASSERT(ltvPP!=NULL);
	ASSERT(*ltvPP!=NULL);
	core::LoadTimeValues_O& ltv = **ltvPP;
	core::T_sp& result = ltv.data_element(index);
#ifdef DEBUG_LOAD_TIME_VALUES
//        printf("%s:%d loadTimeValueReference@%p  index[%d]  result client@%p  value: %s\n", __FILE__, __LINE__, *ltvPP, index, result.pbase(), _rep_(result).c_str());
#endif
	return &result;
    }

    core::Symbol_sp* loadTimeSymbolReference(core::LoadTimeValues_O** ltvPP, int index)
    {
	ASSERT(ltvPP!=NULL);
	ASSERT(*ltvPP!=NULL);
	core::Symbol_sp& result = (*ltvPP)->symbols_element(index);
#ifdef DEBUG_LOAD_TIME_VALUES
//        printf("%s:%d loadTimeSymbolReference@%p  index[%d]  result client@%p  value: %s\n", __FILE__, __LINE__, (*ltvPP), index, result.pbase(), _rep_(result).c_str());
#endif
	return &result;
    }




};

inline core::T_sp proto_getLoadTimeValue(core::LoadTimeValues_O** ltvPP, int index)
{
    return (*ltvPP)->data_element(index);
}

extern "C"
{
    void sp_getLoadTimeValue(core::T_sp* resultP, core::LoadTimeValues_O** ltvPP, int index)
    {
	(*resultP) = proto_getLoadTimeValue(ltvPP,index);
	ASSERTNOTNULL(*resultP);
    }
    void mv_getLoadTimeValue(core::T_mv* resultP, core::LoadTimeValues_O** ltvPP, int index)
    {
	(*resultP) = Values(proto_getLoadTimeValue(ltvPP,index));
	ASSERTNOTNULL(*resultP);
    }

};

extern "C"
{

    void ltv_makeCons(core::T_sp* resultP)
    {
	ASSERT(resultP!=NULL);
	(*resultP) = core::Cons_O::create();
	ASSERTNOTNULL(*resultP);
    }
#if 0
    void ltv_makeSourceCodeCons(core::T_sp* resultP, const char* sourceFileNameP, int lineNo, int column)
    {_G();
	ASSERT(resultP!=NULL);
	(*resultP) = core::Cons_O::create(lineNo,column,core::SourceFileInfo_O::getOrCreate(sourceFileNameP),_lisp);
	ASSERTNOTNULL(*resultP);
    }
#endif
    void ltv_makeArrayObjects(core::T_sp* resultP, core::T_sp* elementTypeP, int rank, int dimensions[])
    {
	ASSERT(resultP!=NULL);
	ASSERT(elementTypeP!=NULL);
	if ( rank == 1 ) // vector
	{
	    *resultP = core::VectorObjects_O::create(_Nil<core::T_O>(),dimensions[0],*elementTypeP);
	} else
	{
	    core::Cons_sp dims = _Nil<core::Cons_O>();
	    for ( int i= rank-1; i>= 0; i-- )
	    {
		dims = core::Cons_O::create(core::Fixnum_O::create(dimensions[i]),dims);
	    }
	    *resultP = core::ArrayObjects_O::make(dims,cl::_sym_T_O,*elementTypeP);
	}
	ASSERTNOTNULL(*resultP);
    }

    void ltv_makeHashTable(core::T_sp* resultP, core::T_sp* testP)
    {
	ASSERT(resultP!=NULL);
	(*resultP) = core::HashTable_O::create(*testP);
	ASSERTNOTNULL(*resultP);
    }


    void rplaca(core::T_sp* resultP, core::T_sp* carP)
    {
	ASSERT(resultP!=NULL);
	(*resultP).as<core::Cons_O>()->setCar(*carP);
    }

    void rplacd(core::T_sp* resultP, core::T_sp* carP)
    {
	ASSERT(resultP!=NULL);
	(*resultP).as<core::Cons_O>()->setOCdr(*carP);
    }



    void ltv_initializeArrayObjectsRowMajorArefOrder(core::T_sp* arrayP, core::LoadTimeValues_O** ltvPP, int* indices )
    {
	core::Array_sp array = (*arrayP).as<core::Array_O>();
	int arrayTotalSize = array->arrayTotalSize();
	for (int i=0; i<arrayTotalSize; i++ )
	{
	    array->rowMajorAset(i,(*ltvPP)->data_element(indices[i]));
	}
    }

    void ltv_initializeHashTable(core::T_sp* hashTableP, int numEntries, core::LoadTimeValues_O** ltvPP, int* indices )
    {
	core::HashTable_sp hashTable = (*hashTableP).as<core::HashTable_O>();
	int j = 0;
	for (int i=0; i<numEntries; i++ )
	{
	    int ikey = indices[j];
	    int ival = indices[j+1];
	    core::T_sp key = (*ltvPP)->data_element(ikey);
	    core::T_sp val = (*ltvPP)->data_element(ival);
	    hashTable->hash_table_setf_gethash(key,val);
	    j += 2;
	}
    }


    void saveToMultipleValue0(core::T_mv* mvP)
    {
        MultipleValues* mv = lisp_multipleValues();
        mv->valueSet(0,*mvP);
    }


/*! Copy the current MultipleValues in _lisp->values() into a VectorObjects */
    extern void saveValues(core::T_sp* resultP, core::T_mv* mvP)
    {_G();
	ASSERT(resultP!=NULL);
	ASSERT(mvP!=NULL);
	int numValues = (*mvP).number_of_values();
	core::VectorObjects_sp vo = core::VectorObjects_O::create(_Nil<core::T_O>(),numValues,core::T_O::___staticClass);
//	printf("intrinsics.cc saveValues numValues = %d\n", numValues );
	if ( numValues > 0 ) {
	    vo->setf_elt(0,(*mvP));
	}
	for ( int i(1); i<(*mvP).number_of_values(); ++i ) {
	    vo->setf_elt(i,(*mvP).valueGet(i));
	}
	(*resultP) = vo;
	ASSERTNOTNULL(*resultP);
    }

/*! Copy the current MultipleValues in _lisp->values() into a VectorObjects */
    extern void loadValues(core::T_mv* resultP, core::T_sp* vectorObjectsP)
    {_G();
	ASSERT(resultP!=NULL);
	ASSERT(vectorObjectsP!=NULL);
        if ( !(*vectorObjectsP) ) {
            // If there was a non-local exit then *vectorObjectP will be NULL
            // check for that here and if so set the result to gctools::multiple_values<core::T_O>()
            (*resultP) = gctools::multiple_values<core::T_O>();
            return;
        }
        ASSERTF(*vectorObjectsP,BF("*vectorObjectsP is UNDEFINED"));
	core::VectorObjects_sp vo = (*vectorObjectsP).as<core::VectorObjects_O>();
//	printf("intrinsics.cc loadValues vo->length() = %d\n", vo->length() );
	if ( vo->length() == 0 )
	{
	    (*resultP) = gctools::multiple_values<core::T_O>();
	    return;
	}
	(*resultP) = gctools::multiple_values<core::T_O>(vo->elt(0),vo->length());
	for ( int i(1); i<vo->length(); ++i )
	{
	    (*resultP).valueSet(i,vo->elt(i));
	}
    }



    /*! If saw_aok > 0 then return that.
      Otherwise check the following argument - if true then return 2 --> :a-o-k t
      Otherwise return 1 --> :a-o-k nil
    */
    int kw_allowOtherKeywords(int saw_aok, core::ActivationFrame_sp* afP, int argIdx)
    {
	if (saw_aok) return saw_aok;
	ASSERTNOTNULL(*afP);
	core::ValueFrame_sp valueFrame = (*afP).as<core::ValueFrame_O>();
	bool aokTrue = valueFrame->entryReference(argIdx+1).isTrue();
	return aokTrue ? 2 : 1;
    }

    void kw_throwIfNotKeyword(core::T_sp* objP)
    {
	ASSERT(objP!=NULL);
	if ( !af_keywordP((*objP)) )
	{
            SIMPLE_ERROR(BF("Not keyword %s")% _rep_(*objP));
//            core::throwUnrecognizedKeywordArgumentError(*objP);
	}
    }


    int kw_trackFirstUnexpectedKeyword(int badKwIdx, int newBadKwIdx)
    {
	if ( badKwIdx>0 ) return badKwIdx;
	return newBadKwIdx;
    }

    void kw_throwIfBadKeywordArgument(int allowOtherKeys, int badKwIdx, core::ActivationFrame_sp* afP )
    {
	if ( allowOtherKeys == 2 ) return;
	ASSERTNOTNULL(*afP);
	if ( badKwIdx >= 0 )
	{
	    core::ValueFrame_sp valueFrame = (*afP).as<core::ValueFrame_O>();
	    SIMPLE_ERROR(BF("Bad keyword argument %s in args: %s") % _rep_(valueFrame->entry(badKwIdx)) % _rep_(valueFrame) );
	}
    }

};


extern "C"
{
    union SetjmpUserTy
    {
	T_mv* 	tmvP;
	int	ival;
    };

    struct SetjmpBufTy
    {
	void*	lowLevelFrame;
	void*	lowLevelLongJumpAddress;
	SetjmpUserTy	user0;
	SetjmpUserTy	user1;
	SetjmpUserTy 	user2;
    };

    void setjmp_set_jump_address( SetjmpBufTy* bufP, void* jumpAddress)
    {
	bufP->lowLevelLongJumpAddress = jumpAddress;
    }

    void setjmp_user0_set_i32( SetjmpBufTy* bufP, int val)
    {
	bufP->user0.ival = val;
    }

    int setjmp_user0_get_i32( SetjmpBufTy* bufP )
    {
	return bufP->user0.ival;
    }

    void setjmp_user0_allocate_set_tmv( SetjmpBufTy* bufP, T_mv* valP )
    {
	bufP->user0.tmvP = new T_mv(*valP);
    }

    void setjmp_user0_get_tmv( T_mv* resultP, SetjmpBufTy* bufP )
    {
	*resultP = *(bufP->user0.tmvP);
    }

    void setjmp_user0_delete_tmv( SetjmpBufTy* bufP )
    {
	bufP->user0.tmvP->reset();
	delete bufP->user0.tmvP;
    }


};


extern "C"
{

    void progvSaveSpecials( void** saveSpecialsP, core::T_sp* symbolsP, core::T_sp* valuesP)
    {_G();
	core::DynamicScopeManager* managerP = new core::DynamicScopeManager();
	(*saveSpecialsP) = (void*)managerP;
	core::Cons_sp symbols = (*symbolsP).as<Cons_O>();
	core::Cons_sp values = (*valuesP).as<Cons_O>();
	for ( ; symbols.notnilp(); symbols=cCdr(symbols), values=cCdr(values) )
	{
	    core::Symbol_sp symbol = oCar(symbols).as<Symbol_O>();
	    core::T_sp value = oCar(values);
	    managerP->pushSpecialVariableAndSet(symbol,value);
	}
    }

    void progvRestoreSpecials( void** saveSpecialsP)
    {_G();
	core::DynamicScopeManager* managerP = (core::DynamicScopeManager*)(*saveSpecialsP);
	delete (managerP);
    }

};



extern "C"
{

    void pushDynamicBinding(core::Symbol_sp* symbolP)
    {
	core::Symbol_sp sym = *symbolP;
	_lisp->bindings().push(sym);
//	printf("%s:%d - pushDynamicBinding symbol: %s  value: %s\n", __FILE__, __LINE__, sym->__repr__().c_str(), sym->symbolValueOrUnbound()->__repr__().c_str() );
    }

    void popDynamicBinding(core::Symbol_sp* symbolP)
    {
	core::Symbol_sp sym = *symbolP;
	ASSERTF(sym == _lisp->bindings().topSymbol(),BF("Mismatch in popDynamicBinding"));
	_lisp->bindings().pop();
//	printf("%s:%d - popDynamicBinding symbol: %s  restored value: %s\n", __FILE__, __LINE__, sym->__repr__().c_str(), sym->symbolValueOrUnbound()->__repr__().c_str() );
    }

};



extern "C"
{

    void trace_setLineNumberColumnForIHSTop( int* sourceFileInfoHandleP, int ln, int col )
    {
	_lisp->invocationHistoryStack().setSourcePosForTop(*sourceFileInfoHandleP,ln,col);
    }

    void trace_setActivationFrameForIHSTop(core::ActivationFrame_sp* afP)
    {_G();
	_lisp->invocationHistoryStack().setActivationFrameForTop(*afP);
    }





    extern int matchKeywordOnce(core::T_sp* xP, core::T_sp* yP, unsigned char* sawKeyAlreadyP)
    {
        if ( (*xP)!=(*yP) ) return 0;
        if (*sawKeyAlreadyP) return 2;
        return 1;
    }


};


#pragma GCC visibility pop


namespace llvmo {
    // We must link one symbol to the executable or none of this file will be inserted
    void initialize_intrinsics()
    {
    }

};
