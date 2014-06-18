#ifndef	_core_structureClass_H
#define _core_structureClass_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"
#include "holder.h"

namespace core {

    SMART(StructureClass);


    SMART(StringSet);






    SMART(StructureClass );
    class StructureClass_O : public Class_O
    {
        LISP_META_CLASS(StandardClass);
        LISP_BASE1(Class_O);
        LISP_CLASS(core,ClPkg,StructureClass_O,"structure-class");
    public:
#if defined(XML_ARCHIVE)
	void	archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
	void	initialize();
    protected:
	Class_sp			_InstanceCoreClass;
    public:
	/*! Special creator used when starting up lisp environment, the object returned will be a root */
	static StructureClass_sp createUncollectable();
#if 0
	/*! Special creator used when starting up lisp environment */
	static StructureClass_sp create(Class_sp mc);
#endif

#if 0 // for now comment out all functions
	static StructureClass_sp create(Lisp_sp e,Symbol_sp name /* , uint instanceClassSymbol */ );

	/*! ensure-class-using-class - see AOMOP-183 */
	static T_sp create_ensureClassUsingClass( Function_sp exec,
						  Cons_sp args,
						  Environment_sp env,
						  Lisp_sp lisp);

							      
        /*! Convert a Class list designator to a Cons of classes.
          Accept the following designators:
          nil - Convert to a Cons containing StructureClass.
          A single class - Convert to a Cons containing that Class.
          A cons of classes - return it.
        */
//    static Cons_sp classListDesignator(T_sp baseClassesDesignator, Lisp_sp lisp);


    public:
//	void defineYourSlotsFromBinderArchiveNode(ArchiveP node);
//	uint numberOfSlots();
        /*! Look for the symbol and return an iterator for the slot
         * otherwise return end()
         */
//	slotIterator find(Symbol_sp sym);
    public:

        /*! Reset the slots */
//	void resetSlots();

	void appendInstanceVariablesFromStructureClass(StructureClass_sp cc);
	void appendInstanceVariablesFromListOfSymbols(Cons_sp variableNames);

        virtual void describe();
        virtual string dumpInfo();


#endif

        StructureClass_O();
        virtual ~StructureClass_O() {};
    };

};

template<> struct gctools::GCInfo<core::StructureClass_O> {
    static bool constexpr NeedsInitialization = true;
    static bool constexpr NeedsFinalization = false;
    static bool constexpr Moveable = false;
    static bool constexpr Atomic = false;
};



namespace core {


    class	StructureClassInitializationFunctoid : public Functoid
    {
    private:
	StructureClass_sp	_StructureClass;	
    public:
        DISABLE_NEW();

        virtual string describe() const {return "StructureClassInitializationFunctoid";};
        StructureClassInitializationFunctoid(const string& name, StructureClass_sp c) : Functoid("StructureClassInitializationFunctoid->"+name) { this->_StructureClass = c;};
        virtual ~StructureClassInitializationFunctoid() {};
    };

};
TRANSLATE(core::StructureClass_O);
#endif //]
