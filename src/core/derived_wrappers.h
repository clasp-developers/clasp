#ifndef	core_wrappers_H
#define core_wrappers_H

#include <functional>
#include "lispDefinitions.h"
#include "symbolToEnumConverter.h"
#include "activationFrame.h"

//#define DEBUG_METHOIDS 1


namespace core
{


    template <int DispatchOn, typename FN>
    class VariadicMethoid : public Functoid {};

#include "wrappers_methoids.h"


};













namespace core {

#define	DEF(ClassName,FunctionName) def(#FunctionName,&ClassName::FunctionName,ARGS_##ClassName##_##FunctionName,DECL_##ClassName##_##FunctionName,DOCS_##ClassName##_##FunctionName,true)
#define DEF_DONT_EXPORT(ClassName,FunctionName) def(#FunctionName,&ClassName::FunctionName,ARGS_##ClassName##_##FunctionName,DECL_##ClassName##_##FunctionName,DOCS_##ClassName##_##FunctionName,false)

};



namespace core {

    class Function_O;    


// -----------------------------------
//
// Macro stuff
//
//
// Wrapper for ActivationFrameMacroPtr
    class ActivationFrameMacroWrapPtr : public Functoid {
    private:
	typedef	T_mv (*MacroPtr)(Cons_sp,Environment_sp);
	MacroPtr	mptr;
    public:
	virtual string describe() const {return "ActivationFrameMacroWrapPtr";};
// constructor
	ActivationFrameMacroWrapPtr(const string& name, MacroPtr ptr) : Functoid(name),mptr(ptr) {}

	T_mv activate( ActivationFrame_sp closedEnv, int nargs, ArgArray argArray)
	{_G();
	    Cons_sp form = argArray[0].as_or_nil<Cons_O>();
	    Environment_sp env = argArray[1].as_or_nil<Environment_O>();
	    T_mv retval = (this->mptr)(form,env);
	    return retval;
	};
    };


    inline void defmacro(const string& packageName, const string& name, T_mv (*mp)(Cons_sp,Environment_sp env),const string& arguments="", const string& declares="", const string& docstring="", bool autoExport=true)
    {_G();
	Functoid* f = new ActivationFrameMacroWrapPtr("macro->"+packageName+"::"+name,mp);
	lisp_defmacro(packageName,name,f,arguments,declares,docstring,autoExport);
    }










    template <int N>
    struct DispatchOn {
        enum {value = N};
    };




// ----------------------------------------
// ----------------------------------------
// ----------------------------------------
// ----------------------------------------

    struct	MethodDefinition {
	string		_Name;
	int		_ClassSymbol;
	Functoid*	_Methoid;
    };

//    typedef	enum { no_init,class_name_init, make_class_name_init } maker_enum;


    extern Symbol_sp _sym_STARallCxxClassesSTAR;

    template < typename OT>
    class derived_class_
    {
    public:
	typedef OT wrapped_type;
    private:
	Symbol_sp	_ClassSymbol;
    public:

	void setup_class(const string& makerName = "")
	{_G();
	    if ( IS_SYMBOL_UNDEFINED(OT::static_classSymbol()) )
	    {
		SIMPLE_ERROR(BF("Attempting to add methods for "
				"class that isn't defined yet"));
	    }

	    this->_ClassSymbol = OT::static_classSymbol();
            reg::lisp_registerClassSymbol<OT>(this->_ClassSymbol);

	    /*! Accumulate all of the classes in reverse order of how they were initialized
	      in the core::*all-cxx-classes* variable */
	    if ( _sym_STARallCxxClassesSTAR->symbolValueUnsafe() ) {
		_sym_STARallCxxClassesSTAR->setf_symbolValue(Cons_O::create(OT::static_classSymbol(),_sym_STARallCxxClassesSTAR->symbolValue()));
	    }
	    // 
	    // If the class isn't in the class table then add it
	    //
	    if ( lisp_boot_findClassBySymbolOrNil(OT::static_classSymbol()).nilp())
	    {
                DEPRECIATED();
		LOG(BF("Adding class(%s) to environment")% OT::static_className() );
		lisp_addClass(/*_lisp,OT::static_packageName(),
				OT::static_className(), */
		    OT::static_classSymbol(),
		    OT::static_allocator,
		    OT::Bases::baseClass1Id(),
		    OT::Bases::baseClass2Id() );
	    }
	    if (makerName != "")
	    {
		// use make-<className>
		af_def(OT::static_packageName(),makerName,&new_LispObject<OT>);
	    }
	}


	//
	//
	// ctor
	//
	//

	class_()
	{_G();
	    this->setup_class("");
	}

	class_(const string& makerName)
	{_G();
	    this->setup_class(makerName);
	}


        class_(const string& packageName, const string& className )
        {_G();
            this->setup_class(packageName,className);
        }



        // non-const function dispatch on parameter 0
	template <typename RT,class... ARGS>
	class_& def( string const& name, RT (OT::*mp)(ARGS...),
                     string const& lambda_list="", const string& declares="", const string& docstring="",bool autoExport=true)
	{_G();
	    Functoid* m = new VariadicMethoid<0,RT(OT::*)(ARGS...)>(name,mp);
	    lisp_defineSingleDispatchMethod(name,this->_ClassSymbol,m,0,lambda_list,declares,docstring,autoExport,sizeof...(ARGS)+1);
	    return *this;
	}


        // const function dispatch on parameter 0
	template <typename RT,class... ARGS>
	class_& def( string const& name, RT (OT::*mp)(ARGS...) const,
		     string const& lambda_list="", const string& declares="", const string& docstring="",bool autoExport=true)
	{_G();
//	    ConstVariadicMethoid<RT,OT,ARGS...>* m = new ConstVariadicMethoid<RT,OT,ARGS...>(name,mp);
	    Functoid* m = new VariadicMethoid<0,RT(OT::*)(ARGS...) const>(name,mp);
	    lisp_defineSingleDispatchMethod(name,this->_ClassSymbol,m,0,lambda_list,declares,docstring,autoExport,sizeof...(ARGS)+1);
	    return *this;
	}


    };




    template <typename oclass>
    void defaultExposeCando(Lisp_sp lisp)
    {_G();
	// Only expose the class, don't create any methods
	// By default put class in the Cl package
	class_<oclass>();
    }


    struct	EnumValueDefinition {
	string		_Name;
	int		_Value;
    };


    template <typename X>
    class enum_
    {
    private:
	SymbolToEnumConverter_sp	_Converter;
	Symbol_sp		_PredefinedConverterSymbolId;
    public:
	enum_(Symbol_sp symbol, const string& title)
	{_G();
	    this->_PredefinedConverterSymbolId = symbol;
	    this->_Converter = SymbolToEnumConverter_O::create(title);
	    symbol->setf_symbolValue(this->_Converter);
	}

	enum_& value(Symbol_sp const& sym, X value )
	{_G();
	    lisp_extendSymbolToEnumConverter(this->_Converter,sym,sym,value);
	    return *this;
	}
	enum_& value(Symbol_sp const& name, Symbol_sp const& archiveName, X value )
	{_G();
	    lisp_extendSymbolToEnumConverter(this->_Converter,name,archiveName,value);
	    return *this;
	}
	Symbol_sp symbolFromEnum(int value)
	{_G();
	    return lisp_lookupSymbolForEnum(this->_PredefinedConverterSymbolId,(int)(value));
	}
    };

};












//ostream& operator<<(ostream& out, mem::smart_ptr<core::T_O> obj);









#include "python_wrappers.h"

#endif // wrappers_h
