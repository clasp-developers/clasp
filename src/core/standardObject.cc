#define	DEBUG_LEVEL_FULL

#include "lisp.h"
#include "standardObject.h"
#include "symbolTable.h"
#include "evaluator.h"
#include "builtInClass.h"
#include "standardClass.h"
#include "wrappers.h"


namespace core
{





    StandardObject_O::StandardObject_O() : StandardObject_O::Base()  { /*this->_InstanceClass.reset();*/ };
    StandardObject_O::~StandardObject_O() {};
#if 0
    StandardObject_sp StandardObject_O::create(StandardClass_sp instanceClass, Lisp_sp lisp)
    {_G();
	StandardObject_sp co = lisp->create<StandardObject_O>();
	co->_InstanceClass = instanceClass;
	return co;
    }
#endif

    void	StandardObject_O::initialize()
    {_G();
	LOG(BF("Initializing StandardObject"));
	this->Base::initialize();
//	Class_mv sc = af_findClass(cl::_sym_StandardClass_O,true,_Nil<Environment_O>());
//	this->_InstanceClass = sc.as<StandardClass_O>();
    }


    T_sp StandardObject_O::__init__(Function_sp exec, Cons_sp args, Environment_sp bargs, Lisp_sp env)
    {_G();
	return _Nil<T_O>();
    }



#if defined(OLD_SERIALIZE)
    void	StandardObject_O::serialize(serialize::SNode node)
    {
	IMPLEMENT_ME(); // handle slots properly so they are indexed by name
#if 0
	this->Base::serialize(node); 
	node->archiveObject("slots",this->_SlotBinder);
#endif
    }
#endif




#if defined(XML_ARCHIVE)
    void	StandardObject_O::archiveBase(ArchiveP node)
    {
	IMPLEMENT_ME(); // handle slots properly so that they are indexed by name
#if 0
	this->Base::archiveBase(node); 
	node->archiveObject("slots",this->_SlotBinder);
#endif
    }
#endif // defined(XML_ARCHIVE)


    string StandardObject_O::__repr__() const
    {
	stringstream ss;
	ss << this->_instanceClass()->classNameAsString() << " ";
#if 0
	ASSERT(this->_InstanceClass);
	if ( this->_InstanceClass.unboundp() )
	{
	    ss << "InstanceClass - UNBOUND -" << std::endl;
	} else
	{
	    ss << "InstanceClass: " << this->_InstanceClass->className() << std::endl;
	    ss << "[slots ";
	    ASSERT(this->_InstanceClass->_EffectiveSlotDefinitions);
	    if ( this->_InstanceClass->_EffectiveSlotDefinitions.unboundp() )
	    {
		ss << "_EffectiveSlotDefinitions are UNDEFINED";
	    } else
	    { 
		for ( Cons_sp cur = this->_InstanceClass->_EffectiveSlotDefinitions; cur.notnilp(); cur=cCdr(cur) )
		{
		    EffectiveSlotDefinition_sp slotDef = cur->ocar().as<EffectiveSlotDefinition_O>();
		    ss << slotDef->_SlotName->__repr__();
		}
	    }
	    ss << "]" << std::endl;
	}
#endif
	return ss.str();
    }


#if 0
    T_sp& StandardObject_O::slot_ref(Symbol_sp slot_name) throw(SlotRefFailed)
    {_OF();
	IMPLEMENT_ME();
	StandardClass_sp cc = this->__class().as<StandardClass_O>();
	ASSERT(cc.notnilp());
	// TODO: What about class slots? slot_location returns the index of an instance slot
	// but where do we store class slots and how do we return a reference to one of them
	LOG(BF("This object is of class: %s") % cc->__repr__() );
	uint location = cc->slot_location(slot_name); // Can throw SlotRefFailed if slot_name not found
	LOG(BF("Found the slot with name: %s")% slot_name->__repr__() );
	LOG(BF("   the number of slots in this object are: %d")% this->_Slots.size());
	return this->_Slots[location];
    }
#endif


#if 0
    void StandardObject_O::allocate_slot_storage(uint numSlots, T_sp initialValue )
    {_G();
	this->_Slots.resize(numSlots,initialValue);
    }
#endif


    void StandardObject_O::exposeCando(Lisp_sp lisp)
	{
	    class_<StandardObject_O>()
		;
	}

    void StandardObject_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,StandardObject,"","",_lisp)
		;
#endif
	}



    EXPOSE_CLASS(core,StandardObject_O);
};
