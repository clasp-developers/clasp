#ifndef	_core_funcallableStandardClass_H
#define _core_funcallableStandardClass_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"
#include "stdClass.h"
#include "holder.h"

namespace core {

    SMART(FuncallableStandardClass);


    SMART(StringSet);






    SMART(FuncallableStandardClass );
    class FuncallableStandardClass_O : public StdClass_O
    {
        LISP_META_CLASS(StandardClass);
        LISP_BASE1(StdClass_O);
        LISP_CLASS(clos,ClosPkg,FuncallableStandardClass_O,"FuncallableStandardClass");
    public:
#if defined(XML_ARCHIVE)
	void	archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
	void	initialize();
    protected:
	Class_sp			_InstanceCoreClass;
    public:
	/*! Special creator used when starting up lisp environment */
	static FuncallableStandardClass_sp create(Class_sp mc);

#if 0 // for now comment out all functions
	static FuncallableStandardClass_sp create(Lisp_sp e,Symbol_sp name /* , Symbol_sp instanceClassSymbol */ );

	/*! ensure-class-using-class - see AOMOP-183 */
	static T_sp create_ensureClassUsingClass( Function_sp exec,
						  Cons_sp args,
						  Environment_sp env,
						  Lisp_sp lisp);

							      

    public:


    public:

//    virtual T_sp instanceSig() const;

	
    public:

	void appendInstanceVariablesFromFuncallableStandardClass(FuncallableStandardClass_sp cc);
	void appendInstanceVariablesFromListOfSymbols(Cons_sp variableNames);

        virtual void describe();
        virtual string dumpInfo();

#endif

        DEFAULT_CTOR_DTOR(FuncallableStandardClass_O);
    };

};

template<> struct gctools::GCInfo<core::FuncallableStandardClass_O> {
    static bool constexpr NeedsInitialization = true;
    static bool constexpr NeedsFinalization = false;
    static bool constexpr Moveable = true; // old=false
    static bool constexpr Atomic = false;
};






namespace core {


    class	FuncallableStandardClassInitializationFunctoid : public Functoid
    {
    private:
	FuncallableStandardClass_sp	_FuncallableStandardClass;	
    public:
        DISABLE_NEW();

        virtual string describe() const {return "FuncallableStandardClassInitializationFunctoid";};
        FuncallableStandardClassInitializationFunctoid(const string& name, FuncallableStandardClass_sp c) : Functoid(name)
        { this->_FuncallableStandardClass = c;};
        virtual ~FuncallableStandardClassInitializationFunctoid() {};
    };

};
TRANSLATE(core::FuncallableStandardClass_O);
#endif //]
