#ifndef	_core_executables_H //(
#define _core_executables_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"
//#i n c l u d e "macro.h"
#include "cons.h"
#include "symbol.h"
#include "str.fwd.h"
#include "activationFrame.fwd.h"
#include "package.fwd.h"
#include "evaluator.fwd.h"
#include "lambdaListHandler.fwd.h"
#include "lispDefinitions.h"

namespace kw {
    extern core::Symbol_sp _sym_function;
};


namespace core {

    class FunctionClosure : public Closure
    {
    public:
        SourcePosInfo_sp        sourcePosInfo;
        Symbol_sp               kind;
    public:
        DISABLE_NEW();
        FunctionClosure(T_sp fn, SourcePosInfo_sp spo, Symbol_sp k )
            : Closure(fn)
            , sourcePosInfo(spo)
            , kind(k){};
        FunctionClosure(T_sp fn)
            : Closure(fn)
            , sourcePosInfo(_Nil<SourcePosInfo_O>())
            , kind(kw::_sym_function){};
        virtual size_t templatedSizeof() const { return sizeof(*this); };

	virtual string describe() const {return "SingleDispatchGenericFunctoid";};
	virtual void LISP_CALLING_CONVENTION() =0;
        void setKind(Symbol_sp k) { this->kind = k; };
        bool macroP() const;
        virtual bool builtinP() const { return false; }
        virtual bool compiledP() const { return false; }
        virtual bool interpretedP() const { return false; }
    };


    extern void handleArgumentHandlingExceptions(FunctionClosure*);

};


namespace core {
    SMART(LambdaListHandler);
    SMART(Function);
    class Function_O : public T_O
    {
        LISP_BASE1(T_O);
        LISP_CLASS(core,ClPkg,Function_O,"Function");

#if defined(XML_ARCHIVE)
        void archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
    public:  // was protected:
        FunctionClosure*        closure;
    public:
        Function_O() : Base()
                     , closure(NULL)
        {};
        virtual ~Function_O() {};
    public:
        static Function_sp make(FunctionClosure* c) {
            GC_ALLOCATE(Function_O,f);
            f->closure = c;
            return f;
        }
    public:
        string __repr__() const;
        string description() const { return "Function::description"; };

        LambdaListHandler_sp getLambdaListHandler() const;
        bool macroP() const;
        void setKind(Symbol_sp k);
        Symbol_sp functionKind() const;
        Environment_sp closedEnvironment() const;
        T_sp functionName() const;
        T_sp functionFile() const;

    };
};
template<> struct gctools::GCInfo<core::Function_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = false;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};
TRANSLATE(core::Function_O);






namespace core
{
    class InterpretedClosure : public FunctionClosure
    {
    public:
        LambdaListHandler_sp    lambdaListHandler;
        Cons_sp                 declares;
        Str_sp                  docstring;
        Environment_sp          closedEnv;
	Cons_sp                 code;
    public:
        DISABLE_NEW();
        InterpretedClosure(T_sp fn, SourcePosInfo_sp sp, Symbol_sp k
                           , LambdaListHandler_sp llh, Cons_sp dec, Str_sp doc
                           , Environment_sp e, Cons_sp c)
            : FunctionClosure(fn,sp,k)
            , lambdaListHandler(llh)
            , declares(dec)
            , docstring(doc)
            , closedEnv(e)
            , code(c) {};
        virtual size_t templatedSizeof() const { return sizeof(*this); };
	virtual string describe() const {return "InterpretedClosure";};
	virtual void LISP_CALLING_CONVENTION();
        bool interpretedP() const { return true; };
    };



    class BuiltinClosure : public FunctionClosure
    {
    public:
        LambdaListHandler_sp    lambdaListHandler;
    public:
        DISABLE_NEW();
        BuiltinClosure(T_sp name, SourcePosInfo_sp sp, Symbol_sp k)
            : FunctionClosure(name,sp,k)
            , lambdaListHandler(_Nil<LambdaListHandler_O>()) {};
        BuiltinClosure(T_sp name)
            : FunctionClosure(name)
            , lambdaListHandler(_Nil<LambdaListHandler_O>()) {};
        void finishSetup(LambdaListHandler_sp llh, Symbol_sp k) {
            this->lambdaListHandler = llh;
            this->kind = k;
        }
        virtual size_t templatedSizeof() const { return sizeof(*this); };
	virtual string describe() const {return "BuiltinClosure";};
	virtual void LISP_CALLING_CONVENTION();
        bool builtinP() const { return true; };
    };



};




namespace core {
    SMART(LambdaListHandler);
    SMART(Function);
    class CompiledFunction_O : public Function_O
    {
        LISP_BASE1(Function_O);
        LISP_CLASS(core,ClPkg,CompiledFunction_O,"CompiledFunction");

#if defined(XML_ARCHIVE)
        void archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
    public:
        CompiledFunction_O() : Base() {};
        virtual ~CompiledFunction_O() {};
    public:
        static Function_sp make(FunctionClosure* c) {
            GC_ALLOCATE(Function_O,f);
            f->closure = c;
            return f;
        }
    public:
    };
};
template<> struct gctools::GCInfo<core::CompiledFunction_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = false;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};
TRANSLATE(core::CompiledFunction_O);





#endif //)
