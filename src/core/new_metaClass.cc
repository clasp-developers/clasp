/*
    File: new_metaClass.cc
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
#define	DEBUG_LEVEL_FULL

//#i n c l u d e <boost/graph/properties.hpp>
#pragma clang diagnostic push
#pragma GCC diagnostic ignored "-Warray-bounds"
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunneeded-internal-declaration"
#include <boost/graph/vector_as_graph.hpp>
#include <boost/graph/topological_sort.hpp>
#pragma clang diagnostic pop
#include <clasp/core/foundation.h>
#include <clasp/core/lisp.h>
#include <clasp/core/metaClass.h>
#include <clasp/core/package.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/cons.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/instance.h>
#include <clasp/core/vectorObjectsWithFillPtr.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/builtInClass.h>
#include <clasp/core/standardClass.h>
#include <clasp/core/funcallableStandardClass.h>
#include <clasp/core/structureClass.h>
#include <clasp/core/wrappers.h>

namespace core
{



    List_sp global_closClassSlots = _Nil<T_O>();
    const int Class_O::NumberOfClassSlots;




    
    
#define ARGS_af_allocateRawClass "(original meta-class slots)"
#define DECL_af_allocateRawClass ""
#define DOCS_af_allocateRawClass "allocateRawClass - behaves like ECL instance::allocate_raw_instance"
    T_sp af_allocateRawClass(Class_sp orig, Class_sp metaClass, int slots)
    {_G();
	if ( orig.notnilp() )
	{
	    SIMPLE_ERROR(BF("Deal with non-nil orig in allocateRawClass"));
	    // Check out ecl/src/c/instance.d/si_allocate_raw_instance
	}
	Class_sp newClass = metaClass->allocate_newNil().as<Class_O>();
	newClass->initialize();
	newClass->initializeSlots(slots);
#if DEBUG_CLOS >= 2
	printf("\nMLOG allocate-raw-CLASS    %p number_of_slots[%d]\n", (void*)(newClass.get()), slots);
#endif
	return newClass;
    };


    
    
#define ARGS_af_makeSureClosClassSlotsMatchClass "(class-slots)"
#define DECL_af_makeSureClosClassSlotsMatchClass ""
#define DOCS_af_makeSureClosClassSlotsMatchClass "makeSureClosClassSlotsMatchClass"
    void af_makeSureClosClassSlotsMatchClass(Cons_sp classSlots)
    {_G();
	global_closClassSlots = classSlots;
    };


    T_sp closClassSlotInfo(int idx)
    {_G();
	if ( global_closClassSlots )
	{
	    Cons_sp cur = global_closClassSlots;
	    for ( ; idx>0; cur=cCdr(cur), --idx ) {}
	    return oCar(cur);
	}
	return _Nil<T_O>();
    }
	    


    Class_O::Class_O() : Class_O::Base() ,
			 _Signature_ClassSlots(_Unbound<T_O>()),
			 _allocator(NULL)
    {
    };

    Class_O::~Class_O()
    {
#if ENABLE_PROFILING
	printf("+PROFILE-FIND-CLASS+ %s %d\n", this->_Name->__repr__().c_str(), this->_FindClassCount);
#endif

    }


    void Class_O::initializeSlots(int slots)
    {_G();
	if ( slots<Class_O::NumberOfClassSlots )
	{
	    SIMPLE_ERROR(BF("Classes need at least %d slots - you asked for %d") % Class_O::NumberOfClassSlots % slots);
	}
	this->_MetaClassSlots.resize(slots,_Unbound<T_O>());
	this->instanceSet(REF_DIRECT_SUPERCLASSES,_Nil<T_O>());
    }


    Symbol_sp Class_O::className() const
    {
	return this->name();
//    SIMPLE_ERROR(BF("You should use instanceClassName rather than className for classes"));
    }

    string Class_O::classNameAsString() const
    {
	return _rep_(this->name());
//    SIMPLE_ERROR(BF("You should use instanceClassName rather than className for classes"));
    }


    T_sp Class_O::allocate_newNil()
    {_G();
	ASSERTP(this->_allocator != NULL, "_allocator is NULL!!");
	T_sp newObject = (*(this->_allocator))();
	return newObject;
    }

#if 0
    bool Class_O::isOfClass(Class_sp mc) const
    {
	return this == mc.get();
    };
#endif
    bool Class_O::isSubClassOf(Class_sp ancestor) const
    {_G();
	if ( this == ancestor.get() ) return true;
	// TODO: I need to memoize this somehow so that I'm not constantly searching a list in
	// linear time
	List_sp cpl = this->instanceRef(Instance_O::REF_CLASS_PRECEDENCE_LIST);
	ASSERTF(!cpl.unboundp(),BF("You tried to use isSubClassOf when the ClassPrecedenceList had not been initialized"))
	    for ( Cons_sp cur=cpl; cur.notnilp(); cur=cCdr(cur) )
	    {
		if ( oCar(cur) == ancestor ) return true;
	    }
	return false;
    }


#if defined(XML_ARCHIVE)
    void Class_O::archive(ArchiveP node)
    {
	IMPLEMENT_ME();
    }
#endif // defined(XML_ARCHIVE)

    void	Class_O::initialize()
    {
	this->Base::initialize();
    }





    string Class_O::__repr__() const
    {
	if ( this == _lisp->_true().get() )
	{
	    return "#<built-in-class t>";
	}
	stringstream ss;
	ss << "#<" << this->__class()->className() << " " << this->instanceClassName() << " " << (void*)(this) << ">";

	return ss.str();
    }

    string Class_O::getPackageName() const
    {
	return this->name()->getPackage()->getName();
    }



// LambdaListHandler_sp Class_O::__init__lambdaListHandler()
// {_G();
//     return _Nil<LambdaListHandler_O>();
// }


    string Class_O::dumpInfo()
    {
	stringstream ss;
	ss << (boost::format("this.instanceClassName: %s @ %X") % this->instanceClassName() % this) << std::endl;
	ss << "_FullName[" << this->name()->fullName() << "]" <<std::endl;
	ss << boost::format("    _Class = %X  this._Class.instanceClassName()=%s\n") % this->__class().get() % this->__class()->instanceClassName();
	for ( Cons_sp cc = this->directSuperclasses(); cc.notnilp(); cc=cCdr(cc) )
	{
	    ss << "Base class: " << oCar(cc).as<Class_O>()->instanceClassName() << std::endl;
	}
	ss << boost::format("this._allocator* = %p") % (void*)(this->getA!llocator()) << std::endl;
	return ss.str();
    }


    string Class_O::getPackagedName() const
    {
	return this->name()->formattedName(false);
    }

    void Class_O::accumulateSuperClasses(HashTable_sp supers, VectorObjectsWithFillPtr_sp arrayedSupers, Class_sp mc)
    {_G();
	if ( IS_SYMBOL_UNDEFINED(mc->className()) ) return;
	if ( !supers->gethash(mc,_Unbound<T_O>()).unboundp() ) return;
	supers->hash_table_setf_gethash(mc,Fixnum_O::create(cl_length(arrayedSupers)));
	arrayedSupers->vectorPushExtend(mc,8);
	for ( Cons_sp cur=mc->directSuperclasses(); cur.notnilp(); cur=cCdr(cur) )
	{
	    accumulateSuperClasses(supers,arrayedSupers,oCar(cur).as<Class_O>());
	}
    }


    void Class_O::lowLevel_calculateClassPrecedenceList()
    {_G();
	using namespace boost;
	HashTable_sp supers(af_make_hash_table(cl::_sym_eq,Fixnum_O::create(8),DoubleFloat_O::create(1.5),DoubleFloat_O::create(2.0)));
	VectorObjectsWithFillPtr_sp arrayedSupers(VectorObjectsWithFillPtr_O::make(_Nil<T_O>(),_Nil<T_O>(),16,0,true));
	this->accumulateSuperClasses(supers,arrayedSupers,this->sharedThis<Class_O>());
	vector<list<int> > graph(cl_length(arrayedSupers));

	class TopoSortSetup : public KeyValueMapper {
	private:
	    HashTable_sp 	supers;
	    vector<list<int> >* graphP;
	public:
	    TopoSortSetup(HashTable_sp asupers,vector<list<int> >* gP) : supers(asupers), graphP(gP) {};
	    virtual bool mapKeyValue(T_sp key, T_sp value) {
		int mcIndex = value.as<Fixnum_O>()->get();
		Class_sp mc = key.as<Class_O>();
		for ( Cons_sp mit=mc->directSuperclasses(); mit.notnilp(); mit=cCdr(mit) )
		{
		    int aSuperIndex = this->supers->gethash(oCar(mit),_Unbound<T_O>()).as<Fixnum_O>()->get();
		    (*this->graphP)[mcIndex].push_front(aSuperIndex);
		}
		return true;
	    }
	};
	TopoSortSetup topoSortSetup(supers,&graph);
	supers->lowLevelMapHash(&topoSortSetup);
#ifdef DEBUG_ON
	{
	    for ( size_t zi(0),ziEnd(cl_length(arrayedSupers)); zi<ziEnd; ++zi )
	    {
		stringstream ss;
		ss << (BF("graph[%d/name=%s] = ") % zi % arrayedSupers->operator[](zi).as<Class_O>()->instanceClassName() ).str();
		for ( list<int>::const_iterator it=graph[zi].begin(); it!=graph[zi].end(); it++ )
		{
		    ss << *it << "-> ";
		}
		ss << ";";
		LOG(BF("%s") % ss.str() );
	    }
	}
#endif
	deque<int> topo_order;
	topological_sort(graph,front_inserter(topo_order),vertex_index_map(identity_property_map()));
#ifdef DEBUG_ON
	{
	    stringstream ss;
	    ss << "Topologically sorted superclasses ";
	    for ( deque<int>::const_reverse_iterator it=topo_order.rbegin(); it!=topo_order.rend(); it++ )
	    {
		Class_sp mc = arrayedSupers->operator[](*it).as<Class_O>();
		ss << "-> " << mc->className() << "/" <<mc->instanceClassName();
	    }
	    LOG(BF("%s") % ss.str() );
	}
#endif
	List_sp cpl = _Nil<T_O>();
	for ( deque<int>::const_reverse_iterator it=topo_order.rbegin(); it!=topo_order.rend(); it++ )
	{
	    Class_sp mc = arrayedSupers->operator[](*it).as<Class_O>();
	    LOG(BF("pushing superclass[%s] to front of ClassPrecedenceList") % mc->instanceClassName() );
	    cpl = Cons_O::create(mc,cpl);
	}
	this->instanceSet(REF_CLASS_PRECEDENCE_LIST,cpl);
    }



    void Class_O::addInstanceBaseClassDoNotCalculateClassPrecedenceList(Symbol_sp className)
    {_OF();
	Class_sp cl = eval::funcall(cl::_sym_findClass,className,_lisp->_true()).as<Class_O>();
	// When booting _DirectSuperClasses may be undefined
	ASSERT(this->directSuperclasses());
	List_sp dsc = _Nil<T_O>();
	if ( !this->directSuperclasses().unboundp() )
	{
	    dsc = this->directSuperclasses();
	}
	this->instanceSet(REF_DIRECT_SUPERCLASSES,Cons_O::create(cl,dsc));
    }


    void Class_O::addInstanceBaseClass(Symbol_sp className)
    {_OF();
	this->addInstanceBaseClassDoNotCalculateClassPrecedenceList(className);
	this->lowLevel_calculateClassPrecedenceList();
    }


    void Class_O::setInstanceBaseClasses(Cons_sp classes)
    {_OF();
	this->instanceSet(REF_DIRECT_SUPERCLASSES,classes->copyList());
	this->lowLevel_calculateClassPrecedenceList();
    }




    Cons_sp Class_O::directSuperclasses() const
    {_OF();
	return this->instanceRef(REF_DIRECT_SUPERCLASSES);
    }


/*
  __BEGIN_DOC(classes.classMethods.describe,describe)
  \scriptCmd{describe}{classObject}

  Dumps a description of the class to stdout.
  __END_DOC
*/
    void	Class_O::describe()
    {_G();
	_lisp->print(BF("%s") % this->dumpInfo().c_str() );
    }


    T_sp Class_O::make_instance()
    {_G();
	T_sp instance = this->allocate_newNil();
	instance->initialize();
	return instance;
    }


    T_sp Class_O::instanceRef(int idx) const
    {
	ASSERTF(idx>=0 && idx<this->_MetaClassSlots.size(),BF("Out of range index %d for instanceRef(%d)") % idx % this->_MetaClassSlots.size());
	return this->_MetaClassSlots[idx];
    }

    T_sp Class_O::instanceSet(int idx,T_sp val)
    {
	ASSERTF(idx>=0 && idx<this->_MetaClassSlots.size(),BF("Out of range index %d for instanceRef(%d)") % idx % this->_MetaClassSlots.size());
	this->_MetaClassSlots[idx] = val;
	return val;
    }



/*! Return true if every member of subset is in superset */
    bool subsetp(Cons_sp subset, Cons_sp superset)
    {
	ASSERT(subset);
	ASSERT(superset);
	if ( subset.nilp() && superset.nilp()) return true;
	if ( superset.nilp() ) return false;
	if ( subset.nilp() ) return true;
	for ( ; subset.notnilp(); subset=cCdr(subset) )
	{
	    T_sp o = oCar(subset);
	    if (!superset->memberEq(o)) return false;
	}
	return true;
    }





    T_sp Class_O::instanceSigSet()
    {
	// Do nothing
	Class_sp mc = this->_instanceClass().as<Class_O>();
	ASSERTNOTNULL(mc);
	T_sp sig = mc->slots();
	ASSERTNOTNULL(sig);
	this->_Signature_ClassSlots = sig;
#if DEBUG_CLOS >= 2
	printf("\nMLOG instance_set_sig object %p\n", (void*)(this) );
#endif
	return sig;
    }

    T_sp Class_O::instanceSig() const
    {
	ASSERTNOTNULL(this->_Signature_ClassSlots);
#if DEBUG_CLOS >= 2
	printf("\nMLOG instance_sig object %p\n", (void*)(this) );
#endif
	return this->_Signature_ClassSlots;
    }


    Class_sp Class_O::_instanceClass() const
    {
	return this->__class();
    }


    T_sp Class_O::instanceClassSet(Class_sp mc)
    {
	if (mc.get() == this) return mc;
	SIMPLE_ERROR(BF("You cannot change the meta-class of a class object" ));
    }


    void Class_O::__setupStage3NameAndCalculateClassPrecedenceList(Symbol_sp className)
    {_G();
	this->setName(className);
	T_sp tmc = this->_instanceClass();
	ASSERTNOTNULL(tmc);
	Class_sp mc = tmc.as<Class_O>();
	this->lowLevel_calculateClassPrecedenceList();
    }





#define ARGS_af_subclassp "(low high)"
#define DECL_af_subclassp ""
#define DOCS_af_subclassp "subclassp"
    bool af_subclassp(T_sp low, T_sp high)
    {_G();
	if ( low == high ) return true;
	if ( Class_sp lowmc = low.asOrNull<Class_O>() )
	{
	    List_sp lowClassPrecedenceList = lowmc->instanceRef(Instance_O::REF_CLASS_PRECEDENCE_LIST);// classPrecedenceList();
	    return lowClassPrecedenceList->memberEq(high).notnilp();
	} else if (Instance_sp inst = low.asOrNull<Instance_O>() )
	{
	    IMPLEMENT_MEF(BF("Run some other tests to make sure that instance is a Class: %s") % _rep_(low) );
	}
	SIMPLE_ERROR(BF("Illegal argument for subclassp: %s") % _rep_(low) );
    };




    
    


    void Class_O::exposeCando(Lisp_sp lisp)
    {
	class_<Class_O>()
	    .def("core:className",&Class_O::className)
	    .def("direct-superclasses",&Class_O::directSuperclasses)
	    ;
	SYMBOL_SC_(CorePkg,makeSureClosClassSlotsMatchClass);
	Defun(makeSureClosClassSlotsMatchClass);
	SYMBOL_SC_(CorePkg,subclassp);
	Defun(subclassp);
	SYMBOL_SC_(CorePkg,allocateRawClass);
	Defun(allocateRawClass);
    }
    void Class_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,Class,"","",_lisp)
	    ;
#endif
    }


    EXPOSE_CLASS(core,Class_O);
};
