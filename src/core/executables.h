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
        T_sp			_Name;
        Symbol_sp			_Kind;
        LambdaListHandler_sp	_LambdaListHandler;
        SourceFileInfo_sp 		_SourceFileInfo;
        uint			_LineNumber;
        uint			_Column;
        uint 			_FilePos;
    public:
        Function_O() : Base(), _Name(_Nil<T_O>())
                     , _Kind(kw::_sym_function)
                     , _LambdaListHandler(_Nil<LambdaListHandler_O>())
                     , _SourceFileInfo(_Nil<SourceFileInfo_O>())
                     , _LineNumber(0)
                     , _Column(0)
        {};   
        virtual ~Function_O() {};
    public:
        /*! Variadic funcall invoker */
        virtual T_mv INVOKE(int nargs, ArgArray args) {_OF(); SUBCLASS_MUST_IMPLEMENT();};
    public:



        virtual Environment_sp closedEnvironment() const;

        Symbol_sp functionKind() const { return this->_Kind;};
        T_sp functionName() const { return this->_Name;};
        void setFunctionName(T_sp symbol);
        T_sp getFunctionName() const;

        T_mv functionFile() const;

        bool macroP() const;


        /*! Set the kind of the function (function) or (macro) */
        void set_kind(Symbol_sp function_kind);

        SourceFileInfo_sp sourceFileInfo() const;
        int lineNumber() const { return this->_LineNumber;};
        int column() const { return this->_Column;};
    public:


        string __repr__() const;

        string description() const { return _rep_(this->getFunctionName());};

        void setLambdaListHandler(LambdaListHandler_sp llh) { this->_LambdaListHandler=llh;};
        LambdaListHandler_sp getLambdaListHandler() {return this->_LambdaListHandler;};


    };
};
template<> struct gctools::GCAllocatorInfo<core::Function_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = false;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};
TRANSLATE(core::Function_O);


namespace core {
    SMART(Interpreted);
    class Interpreted_O : public Function_O
    {
        LISP_BASE1(Function_O);
        LISP_CLASS(core,CorePkg,Interpreted_O,"interpreted");
        friend class SingleDispatchGenericFunction_O;
        friend class Lambda_emf;
        friend class Lambda_call_next_method;
//    friend T_mv core::eval::applyFunctionToActivationFrame(Function_sp head, ActivationFrame_sp args);
#if defined(XML_ARCHIVE)
        void archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
    protected:
        /*! Closed over environment */
        Environment_sp		_ClosedEnvironment;
        // Docstring
        Str_sp			_DocString;
        // Declares
        Cons_sp			_Declares;
        // Code
        Cons_sp			_Code;
    private:
        /*! Variadic funcall invoker */
        virtual T_mv INVOKE(int nargs, ArgArray args );
    public:

        static Interpreted_sp make( T_sp functionName,
                                    LambdaListHandler_sp llh,
                                    Cons_sp code );
        /*! Creates functions.  kind can be :macro or :function */
        static Interpreted_sp create( T_sp functionName,
                                      LambdaListHandler_sp lambda_list_handler,
                                      Cons_sp declares,
                                      Str_sp docString,
                                      Cons_sp code,
                                      Environment_sp environment,
                                      Symbol_sp kind );



    public:
        virtual Environment_sp closedEnvironment() const;
        void closeOverEnvironment(Environment_sp environment);

        Cons_sp getCode() { return this->_Code;};

        void setDocStr(Str_sp str);
        Str_sp getDocString();


//    virtual void setupArgumentHandling( Cons_sp arguments, Lisp_sp);
    public:

        virtual bool can_be_redefined() const { return true;};

        string __repr__() const;
        string __str__() { return _rep_(this); };

        explicit Interpreted_O();
        virtual ~Interpreted_O() {};
    };
};
template<> struct gctools::GCAllocatorInfo<core::Interpreted_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = false;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};
TRANSLATE(core::Interpreted_O);





    
namespace core
{
    class BuiltIn_O : public Function_O
    {
	LISP_BASE1(Function_O);
	LISP_CLASS(core,CorePkg,BuiltIn_O,"BuiltIn");
	DECLARE_INIT();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(BuiltIn_O);
    public: // ctor/dtor for classes with shared virtual base
//    explicit BuiltIn_O(core::Class_sp const& mc) : T_O(mc), Function(mc) {};
//    virtual ~BuiltIn_O() {};
    private: // instance variables here
	ActivationFrame_sp 		_ClosedEnvironment;
	CompiledBody_sp		_Body;
    private:
	virtual T_mv INVOKE(int nargs, ArgArray args);
	    
    public:
	/*! Used to create temporary functions in singleDispatchMethod */
	static BuiltIn_sp create_single_dispatch_function( T_sp functionName,
							   LambdaListHandler_sp args,
							   CompiledBody_sp code );
	static BuiltIn_sp make( T_sp functionName,
				LambdaListHandler_sp llh,
				CompiledBody_sp code );

	static BuiltIn_sp create( T_sp functionName,
				  LambdaListHandler_sp lambda_list_handler,
				  CompiledBody_sp code,
				  ActivationFrame_sp environment,
				  Symbol_sp kind );

	string __repr__() const;

	virtual Environment_sp closedEnvironment() const;
	    
    public: // Functions here
    }; // BuiltIn class
	
}; // core namespace
template<> struct gctools::GCAllocatorInfo<core::BuiltIn_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = false;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};

    TRANSLATE(core::BuiltIn_O);
    



namespace core
{
    class CompiledFunction_O : public Function_O
    {
	LISP_BASE1(Function_O);
	LISP_CLASS(core,ClPkg,CompiledFunction_O,"CompiledFunction");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(CompiledFunction_O);
    public: // ctor/dtor for classes with shared virtual base
//    explicit CompiledFunction_O(core::Class_sp const& mc) : T_O(mc), Function(mc) {};
//    virtual ~CompiledFunction_O() {};
    private: // instance variables here
	ActivationFrame_sp 	_ClosedEnvironment;
	CompiledBody_sp 	_Body;

    public:
	static CompiledFunction_sp makeCompiledFunction(T_sp functionName,
							CompiledBody_sp code,
							ActivationFrame_sp closedEnvironment,
							SourceFileInfo_sp sourceFileInfo,
							uint lineNumber,
							uint column,
							Symbol_sp functionKind );
    public:
    /*! Variadic funcall invoker */
	virtual T_mv INVOKE(int nargs, ArgArray args);
    public:

    virtual Environment_sp closedEnvironment() const;

	CompiledBody_sp getBody() const { return this->_Body;};

	string __repr__() const;

    }; // CompiledFunction class
    
}; // core namespace
template<> struct gctools::GCAllocatorInfo<core::CompiledFunction_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = false;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};

TRANSLATE(core::CompiledFunction_O);


    




#endif //)
