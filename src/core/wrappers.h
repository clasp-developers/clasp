#ifndef	core_wrappers_H
#define core_wrappers_H

#include <functional>
#include "lispDefinitions.h"
#include "symbolToEnumConverter.h"
#include "activationFrame.h"

//#define DEBUG_METHOIDS 1


namespace core
{

    template <typename FN>
    class VariadicFunctoid : public Functoid {
    public:
        typedef Functoid TemplatedBase;
        virtual size_t templatedSizeof() const { return sizeof(VariadicFunctoid<FN>);};
    };
};

template <typename T>
class gctools::GCKind<core::VariadicFunctoid<T>> {
public:
    static gctools::GCKindEnum const Kind = gctools::GCKind<typename core::VariadicFunctoid<T>::TemplatedBase>::Kind;
};

namespace core {
#include "wrappers_functoids.h"
};




namespace core {
    template <int DispatchOn, typename FN>
    class VariadicMethoid : public Functoid {
    public:
        typedef Functoid TemplatedBase;
        size_t templatedSizeof() const { return sizeof(VariadicMethoid<DispatchOn,FN>);};
    };

#include "wrappers_methoids.h"

};


namespace core{


    template <typename RT,typename... ARGS>
    void af_def(const string& packageName, const string& name, RT (*fp)(ARGS...) , const string& arguments="", const string& declares="", const string& docstring="", int locked=1 )
    {_G();
        Functoid* f = gctools::ClassAllocator<VariadicFunctoid<RT(ARGS...)> >::allocateClass(packageName+"::"+name,fp);
        lisp_defun_lispify_name(packageName,name,f,arguments,declares,docstring,locked,true,sizeof...(ARGS));
    }
};


template <int DispatchOn, typename T>
class gctools::GCKind<core::VariadicMethoid<DispatchOn,T>> {
public:
    static gctools::GCKindEnum const Kind = gctools::GCKind<typename core::VariadicMethoid<DispatchOn,T>::TemplatedBase>::Kind;
};













namespace core {

#define	DEF(ClassName,FunctionName) def(#FunctionName,&ClassName::FunctionName,ARGS_##ClassName##_##FunctionName,DECL_##ClassName##_##FunctionName,DOCS_##ClassName##_##FunctionName,true)
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
        DISABLE_NEW();
        size_t templatedSizeof() const { return sizeof(ActivationFrameMacroWrapPtr);};
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
	Functoid* f = gctools::ClassAllocator<ActivationFrameMacroWrapPtr>::allocateClass("macro->"+packageName+"::"+name,mp);
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
    class class_
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

#if 0
            OT dummy;
            size_t offsetT = (size_t)((char*)(dynamic_cast<T_O*>(&dummy)) - (char*)(&dummy));
            size_t offsetGCO = (size_t)((char*)(dynamic_cast<gctools::GCObject*>(&dummy)) - (char*)(&dummy));
            printf("%s:%d %50s offsetof(T_O) = %3lu  offsetof(gctools::GCObject) = %3lu\n", __FILE__, __LINE__, typeid(OT).name(),offsetT, offsetGCO);
#endif            

            reg::lisp_registerClassSymbol<OT>(this->_ClassSymbol);

	    /*! Accumulate all of the classes in reverse order of how they were initialized
	      in the core::*all-cxx-classes* variable */
            lisp_pushClassSymbolOntoSTARallCxxClassesSTAR(OT::static_classSymbol());

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
		    OT::static_creator,
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




        // non-const function dispatch on parameter 0
	template <typename RT,class... ARGS>
	class_& def( string const& name, RT (OT::*mp)(ARGS...),
                     string const& lambda_list="", const string& declares="", const string& docstring="",bool autoExport=true)
	{_G();
	    Functoid* m = gctools::ClassAllocator<VariadicMethoid<0,RT(OT::*)(ARGS...)>>::allocateClass(name,mp);
	    lisp_defineSingleDispatchMethod(name,this->_ClassSymbol,m,0,lambda_list,declares,docstring,autoExport,sizeof...(ARGS)+1);
	    return *this;
	}


        // const function dispatch on parameter 0
	template <typename RT,class... ARGS>
	class_& def( string const& name, RT (OT::*mp)(ARGS...) const,
		     string const& lambda_list="", const string& declares="", const string& docstring="",bool autoExport=true)
	{_G();
	    Functoid* m = gctools::ClassAllocator<VariadicMethoid<0,RT(OT::*)(ARGS...) const>>::allocateClass(name,mp);
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
	    lisp_symbolSetSymbolValue(symbol,this->_Converter);
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












//ostream& operator<<(ostream& out, gctools::smart_ptr<core::T_O> obj);









#include "python_wrappers.h"

#endif // wrappers_h
