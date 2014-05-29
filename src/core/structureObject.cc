#define	DEBUG_LEVEL_FULL

#include "lisp.h"
#include "structureObject.h"
#include "symbolTable.h"
#include "numbers.h"
#include "evaluator.h"
#include "str.h"
#include "builtInClass.h"
#include "sysprop.h"
#include "instance.h"
#include "structureClass.h"
#include "primitives.h"
#include "wrappers.h"


namespace core
{



#define USE_INSTANCES_FOR_STRUCTURES
    
    
#define ARGS_af_makeStructure "(type &rest slot_values)"
#define DECL_af_makeStructure ""
#define DOCS_af_makeStructure "makeStructure"
    T_sp af_makeStructure(T_sp type, Cons_sp slot_values)  
    {_G();
	if ( type.nilp() )
	{
	    SIMPLE_ERROR(BF("You cannot makeStructure of type nil"));
	}
#ifdef CLOS
	if ( Class_sp ctype = type.asOrNull<Class_O>() ) {
	    ASSERTF(!type.nilp(), BF("Tried to make-structure with type = nil"));
//	printf("%s:%d  af_makeStructure of %s  slot_values: %s\n",
//	       __FILE__, __LINE__, _rep_(type).c_str(), _rep_(slot_values).c_str());
	    Instance_sp so = Instance_O::allocateInstance(ctype,af_length(slot_values)).as<Instance_O>();
	    int idx = 0;
	    for ( Cons_sp slot=slot_values; !slot.nilp(); slot=cCdr(slot) )
	    {
		so->instanceSet(idx,oCar(slot));
		++idx;
	    }
	    return so;
	}
#endif // CLOS
	StructureObject_sp so = StructureObject_O::create(type,slot_values);
	return so;
    };



    
    
#define ARGS_af_copyStructure "(arg)"
#define DECL_af_copyStructure ""
#define DOCS_af_copyStructure "copyStructure"
    T_sp af_copyStructure(T_sp arg)
    {_G();
	if ( arg.nilp() )
	{
	    SIMPLE_ERROR(BF("You cannot copyStructure nil"));
	}
#ifdef CLOS
	if ( Instance_sp iarg=arg.asOrNull<Instance_O>() )
	{
	    return iarg->copyInstance();
	}
#endif
	if ( StructureObject_sp so = arg.asOrNull<StructureObject_O>() )
	{
	    return so->copyStructure();
	}
	SIMPLE_ERROR(BF("You cannot copy-structure a %s") % _rep_(arg));
    };


    
    
#define ARGS_af_structureRef "(obj name idx)"
#define DECL_af_structureRef ""
#define DOCS_af_structureRef "structureRef"
    T_sp af_structureRef(T_sp obj, Symbol_sp type, int idx)
    {_G();
	if ( obj.nilp() )
	{
	    TYPE_ERROR(obj,type);
	}
#ifdef CLOS
	if ( Instance_sp so = obj.asOrNull<Instance_O>() )
	{
	    if ( !af_structureSubtypep(af_classOf(so),type) )
	    {
		WRONG_TYPE_NTH_ARG(1,obj,type);
	    }
	    return so->instanceRef(idx);
	}
#endif
	if ( StructureObject_sp so = obj.asOrNull<StructureObject_O>() )
	{
	    T_sp soclass = af_type_of(so);
	    if ( !af_structureSubtypep(soclass,type) )
	    {
		WRONG_TYPE_NTH_ARG(1,obj,type);
	    }
	    return so->structureRef(idx);
	}
	TYPE_ERROR(obj,type);
    };


    
#define ARGS_af_structureSet "(struct type idx val)"
#define DECL_af_structureSet ""
#define DOCS_af_structureSet "structureSet"
    T_sp af_structureSet(T_sp obj, Symbol_sp type, int idx, T_sp val)
    {_G();
	if ( obj.nilp() )
	{
	    TYPE_ERROR(obj,type);
	}
#ifdef CLOS
	if ( Instance_sp so = obj.asOrNull<Instance_O>() )
	{
	    if ( !af_structureSubtypep(af_classOf(so),type) )
	    {
		WRONG_TYPE_NTH_ARG(1,obj,type);
	    }
	    return so->instanceSet(idx,val);
	}
#endif
	if ( StructureObject_sp so = obj.asOrNull<StructureObject_O>() )
	{
	    T_sp sotype = af_type_of(so);
	    if ( !af_structureSubtypep(sotype,type) )
	    {
		WRONG_TYPE_NTH_ARG(1,obj,type);
	    }
	    return so->structureSet(idx,val);
	}
	TYPE_ERROR(obj,type);
    };





    
    
#define ARGS_af_structurep "(arg)"
#define DECL_af_structurep ""
#define DOCS_af_structurep "structurep"
    bool af_structurep(T_sp arg)
    {_G();
	if ( arg.nilp() )
	{
	    return false;
	}
#ifdef CLOS
	if ( Instance_sp io = arg.asOrNull<Instance_O>() )
	{
	    if ( af_structureSubtypep(io->_instanceClass(),cl::_sym_StructureObject_O) ) return true;
	}
#endif
	if ( StructureObject_sp so = arg.asOrNull<StructureObject_O>() )
	{
	    return true;
	}
	return false;
    };




    
    
#define ARGS_af_structureSubtypep "(x y)"
#define DECL_af_structureSubtypep ""
#define DOCS_af_structureSubtypep "structureSubtypep checks if the structure type Y is a subtype of X"
    bool af_structureSubtypep(T_sp x, Symbol_sp y)
    {_G();
	if ( x.nilp() ) return false;
#ifdef CLOS
	if ( Class_sp cx = x.asOrNull<Class_O>() ) {
	    if (cx->className() == y) {
		return true;
	    } else {
		Cons_sp superiors = cx->directSuperclasses();
		for ( Cons_sp sup = superiors; !sup.nilp(); sup=cCdr(sup))
		{
		    if (af_structureSubtypep(oCar(sup).as<Class_O>(),y) ) return true;
		}
		return false;
	    }
	}
#endif
	if ( Symbol_sp sx = x.asOrNull<Symbol_O>() ) {
	    do {
		if (sx == y) return true;
		SYMBOL_EXPORT_SC_(CorePkg,structure_include);
		sx = af_get_sysprop(sx,_sym_structure_include).as<Symbol_O>();
	    } while (!sx.nilp());
	    return false;
	}
	return false;
    }







    StructureObject_O::StructureObject_O() : StructureObject_O::Base() {};

    StructureObject_O::~StructureObject_O() {};

    StructureObject_sp StructureObject_O::create(T_sp type, Cons_sp slot_values)
    {_G();
	StructureObject_sp co = StructureObject_O::create();
	co->_Type = type;
	co->_Slots.resize(af_length(slot_values));
	int i=0;
	for ( Cons_sp cur=slot_values; cur.notnilp(); cur = cCdr(cur), i++ )
	{
	    T_sp val = oCar(cur);
	    co->_Slots[i] = val;
	}
	return co;
    }


    void	StructureObject_O::initialize()
    {_G();
	LOG(BF("Initializing StructureObject"));
	this->Base::initialize();
	this->_Type = _Nil<T_O>();
	this->_Slots.clear();
    }


    T_sp StructureObject_O::oinstancepSTAR() const
    {
	return Fixnum_O::create((int)(this->_Slots.size()));
    }


    


#if defined(OLD_SERIALIZE)
    void	StructureObject_O::serialize(serialize::SNode node)
    {
	IMPLEMENT_ME(); // handle slots properly so they are indexed by name
#if 0
	this->Base::serialize(node); 
	node->archiveObject("slots",this->_SlotBinder);
#endif
    }
#endif




    T_sp StructureObject_O::structureAsList() const
    {
	Cons_O::CdrType_sp first(_Nil<Cons_O::CdrType_O>());
        Cons_O::CdrType_sp* curP = &first;
//        mem::StackRootedPointerToSmartPtr<Cons_O::CdrType_O> cur(&first);
	Cons_sp head = Cons_O::create(this->_Type);
	*curP = head; // cur.setPointee(head); // *cur = head;
	curP = head->cdrPtr(); // cur.setPointer(head->cdrPtr()); // cur = head->cdrPtr();
	SYMBOL_EXPORT_SC_(CorePkg,structure_slot_descriptions);
	Cons_sp slots = af_get_sysprop(this->_Type,_sym_structure_slot_descriptions).as<Cons_O>();
	for ( ; slots.notnilp(); slots = cCdr(slots) ) {
	    Cons_sp slotDesc = oCar(slots).as<Cons_O>();
	    Cons_sp slotNameCons = Cons_O::create(_lisp->internKeyword(oCar(slotDesc).as<Symbol_O>()->symbolName()->get()));
	    *curP = slotNameCons; // cur.setPointee(slotNameCons); // *cur = slotNameCons;
	    curP = slotNameCons->cdrPtr(); // cur.setPointer(slotNameCons->cdrPtr()); // cur = slotNameCons->cdrPtr();
	    int idx = oFifth(slotDesc).as<Fixnum_O>()->get();
	    T_sp val = this->structureRef(idx);
	    Cons_sp slotValueCons = Cons_O::create(val);
	    *curP = slotValueCons; // cur.setPointee(slotValueCons); // *cur = slotValueCons;
	    curP = slotValueCons->cdrPtr(); // cur.setPointer(slotValueCons->cdrPtr()); // cur = slotValueCons->cdrPtr();
	}
	return first;
    }



    T_sp StructureObject_O::structureRef( int index) const
    {_OF();
	ASSERTF(index >=0 && index < this->_Slots.size(),BF("Illegal slot index[%d] - must be less than %d") % index % this->_Slots.size() );
	return this->_Slots[index];
    }

    T_sp StructureObject_O::structureSet(int index, T_sp value)
    {_OF();
	ASSERTF(index >=0 && index < this->_Slots.size(),BF("Illegal slot index[%d] - must be less than %d") % index % this->_Slots.size() );
	this->_Slots[index] = value;
	return value;
    }



    void	StructureObject_O::archiveBase(ArchiveP node)
    {
	// Call out to core:serialize
	IMPLEMENT_MEF(BF("Call out to core::serialize me node")); // handle slots properly so that they are indexed by name
    }



    T_sp StructureObject_O::copyStructure() const
    {_G();
        StructureObject_sp copy = gctools::GCObjectAllocator<StructureObject_O>::copy(*this);
//GC_COPY(StructureObject_O,copy,*this);
	return copy;
    }


    string StructureObject_O::__repr__() const
    {
	stringstream ss;
	ss << "#< ";
	ss << this->_instanceClass()->classNameAsString() << " ";
	ASSERT(this->_Type);
	if ( this->_Type.unboundp() )
	{
	    ss << ":type - UNBOUND -" << std::endl;
	} else
	{
	    ss << ":type " << _rep_(this->_Type) << std::endl;
	}
	ss << "[slots ";
	for ( int i=0; i<this->_Slots.size(); i++ )
	{
	    if ( this->_Slots[i].nilp() ) {
		ss << "NIL ";
	    } else if ( this->_Slots[i].get() == this )
	    {
		ss << "SELF_REFERENCE!!!! ";
	    } else
	    {
		ss << _rep_(this->_Slots[i]) << " ";
	    }
	}
	ss << "]";
	ss << " >";
	return ss.str();
    }


#if 0
    T_sp& StructureObject_O::slot_ref(Symbol_sp slot_name) throw(SlotRefFailed)
    {_OF();
	StructureClass_sp cc = this->_Type;
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
    void StructureObject_O::allocate_slot_storage(uint numSlots, T_sp initialValue )
    {_G();
	this->_Slots.resize(numSlots,initialValue);
    }
#endif


    void StructureObject_O::exposeCando(Lisp_sp lisp)
	{
	    class_<StructureObject_O>()
//		.def("copy-structure",&StructureObject_O::copyStructure) // moved to primitives.cc
		;
#if 0
	    SYMBOL_SC_(CorePkg,make_structure);
	    Defun(make_structure);
#endif
	    SYMBOL_EXPORT_SC_(CorePkg,structureRef);
	    Defun(structureRef);
	    SYMBOL_EXPORT_SC_(CorePkg,structureSet);
	    Defun(structureSet);
	    SYMBOL_EXPORT_SC_(CorePkg,makeStructure);
	    Defun(makeStructure);

	    SYMBOL_EXPORT_SC_(ClPkg,copyStructure);
	    Defun(copyStructure);

	    SYMBOL_EXPORT_SC_(CorePkg,structurep);
	    Defun(structurep);

	    SYMBOL_EXPORT_SC_(CorePkg,structureSubtypep);
	    Defun(structureSubtypep);
	}

    void StructureObject_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,StructureObject,"","",_lisp)
		;
#endif
	}



    EXPOSE_CLASS(core,StructureObject_O);
};
