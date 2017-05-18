/*
    File: metaClass.cc
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
//#define DEBUG_LEVEL_FULL

//#i n c l u d e <boost/graph/properties.hpp>
#pragma clang diagnostic push
#pragma GCC diagnostic ignored "-Warray-bounds"
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunneeded-internal-declaration"
//#pragma GCC diagnostic ignored "-Wunused-local-typedef"
#include <boost/graph/vector_as_graph.hpp>
#include <boost/graph/topological_sort.hpp>
#pragma clang diagnostic pop
#include <clasp/core/foundation.h>
#include <clasp/core/lisp.h>
#include <clasp/core/metaClass.h>
#include <clasp/core/package.h>
#include <clasp/core/pointer.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/cons.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/lispList.h>
#include <clasp/core/array.h>
#include <clasp/core/instance.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/builtInClass.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/standardClass.h>
#include <clasp/core/funcallableStandardClass.h>
#include <clasp/core/structureClass.h>
#include <clasp/core/wrappers.h>

#define NAMESPACE_gctools_mem
#include <clasp/gctools/gc_interface.h>
#undef NAMESPACE_gctools_mem

#if 0
namespace translate {
template <>
struct to_object<core::Creator_sp> {
  typedef core::Creator *GivenType;
  static core::T_sp convert(GivenType v) {
    if (v)
      return core::Pointer_O::create(v);
    return _Nil<core::T_O>();
  }
};
};
#endif


namespace core {
Class_sp Class_O::create(Symbol_sp symbol, Class_sp metaClass ) {
  DEPRECATED();
};

Class_sp Class_O::createUncollectable(gctools::Stamp is, Class_sp metaClass, size_t number_of_slots) {
  GC_ALLOCATE_UNCOLLECTABLE(Class_O,oclass,is,metaClass, number_of_slots);
  return oclass;
};

};




namespace core {

SYMBOL_EXPORT_SC_(ClosPkg,forward_referenced_class);
SYMBOL_EXPORT_SC_(ClPkg,built_in_class);
SYMBOL_EXPORT_SC_(ClPkg,standard_class);
SYMBOL_EXPORT_SC_(CorePkg,std_class);
SYMBOL_EXPORT_SC_(ClPkg,structure_class);
SYMBOL_EXPORT_SC_(CorePkg,cxx_class);
      
CL_LAMBDA(class theDirectSuperclasses);
CL_DECLARE();
CL_DOCSTRING("inheritDefaultAllocator - make this a regular function so that there are no dispatching problems at boot time");
CL_DEFUN void core__inherit_default_allocator(Class_sp cl, T_sp directSuperclasses) {
  //        printf("%s:%d In core__inherit_default_allocator for class: %s direct-superclasses: %s\n",__FILE__,__LINE__, _rep_(cl).c_str(), _rep_(directSuperclasses).c_str());
  cl->inheritDefaultAllocator(directSuperclasses);
};

CL_LAMBDA(original meta-class slots &optional creates-classes);
CL_DECLARE();
CL_DOCSTRING(R"doc(allocate-raw-class - behaves like ECL instance::allocate_raw_instance)doc");
CL_DEFUN T_sp core__allocate_raw_class(T_sp orig, T_sp tMetaClass, int slots, bool creates_classes) {
  if ( Class_sp cMetaClass = tMetaClass.asOrNull<Class_O>() ) {
#if 0
    if (slots == 28 ) {
//      printf("%s:%d  Creating the class %s with 28 slots\n", __FILE__, __LINE__, cMetaClass->classNameAsString().c_str());
    }
#endif
    T_sp tNewClass = cMetaClass->allocate_newClass(cMetaClass,slots);
    if ( Class_sp newClass = tNewClass.asOrNull<Class_O>() ) {
      newClass->initialize();
      newClass->_MetaClass = cMetaClass;
      newClass->_NumberOfSlots = slots;
      //      newClass->initializeSlots(slots);
//      printf("%s:%d allocate-raw-class %p of metaclass %s number_of_slots[%d]\n", __FILE__, __LINE__, (void *)(newClass.get()), cMetaClass->classNameAsString().c_str(), slots);
      if (creates_classes) {
        auto cb = gctools::GC<core::BuiltInObjectCreator<Class_O>>::allocate();
        newClass->setCreator(cb);
      };
      if (orig.nilp()) {
        orig = newClass;
      } else if (Class_sp corig = orig.asOrNull<Class_O>()) {
        corig->_MetaClass = cMetaClass;
        corig->_MetaClassSlots = newClass->_MetaClassSlots;
        printf("%s:%d Changing the #slots to %d for metaclass %s\n", __FILE__, __LINE__, slots, cMetaClass->classNameAsString().c_str() );
      }
      return orig;
    } else if ( Instance_sp iNewClass = tNewClass.asOrNull<Instance_O>() ) {
      SIMPLE_ERROR(BF("Creating an instance of %s resulted in an instance of Instance_O") % _rep_(cMetaClass) );
    } else {
      SIMPLE_ERROR(BF("Creating an instance of %s resulted in something unexpected -> %s") % _rep_(cMetaClass) % _rep_(tNewClass) );
    }
  } else if (tMetaClass.nilp()) {
    printf("%s:%d   core::allocate-raw-class was invoked with NIL as the meta-class - check out why\n", __FILE__, __LINE__ );
    GC_ALLOCATE_UNCOLLECTABLE(Class_O,newClass,gctools::NextStamp(),lisp_StandardClass(),REF_CLASS_NUMBER_OF_SLOTS_IN_STANDARD_CLASS);
    newClass->initialize();
    auto cb = gctools::GC<core::BuiltInObjectCreator<Class_O>>::allocate();
    newClass->setCreator(cb);
//    newClass->initializeSlots(slots);
    return newClass;
  }
  SIMPLE_ERROR(BF("I don't know how to make class with metaclass %s") % _rep_(tMetaClass));
};


void Class_O::initializeSlots(size_t slots) {
  if (slots==0) {
    printf("%s:%d initializeSlots slots = 0\n", __FILE__, __LINE__ );
  }
//  if ( _lisp->_PackagesInitialized ) printf("%s:%d Changing the #slots to %lu\n", __FILE__, __LINE__, slots );
  this->_MetaClassSlots.resize(slots, _Unbound<T_O>());
  this->instanceSet(REF_CLASS_DIRECT_SUPERCLASSES, _Nil<T_O>());
  this->instanceSet(REF_CLASS_DIRECT_DEFAULT_INITARGS, _Nil<T_O>());
  this->instanceSet(REF_CLASS_FINALIZED, _Nil<T_O>());
}

gc::Nilable<Class_sp> identifyCxxDerivableAncestorClass(Class_sp aClass) {
  if (aClass->cxxClassP()) {
    if (aClass->cxxDerivableClassP()) {
      return aClass;
    }
  }
  for (auto supers : aClass->directSuperclasses()) {
    Class_sp aSuperClass = gc::As<Class_sp>(oCar(supers));
    gc::Nilable<Class_sp> taPossibleCxxDerivableAncestorClass = identifyCxxDerivableAncestorClass(aSuperClass);
    if (taPossibleCxxDerivableAncestorClass.notnilp())
      return taPossibleCxxDerivableAncestorClass;
  }
  return _Nil<Class_O>();
}

void Class_O::inheritDefaultAllocator(List_sp superclasses) {
  // If this class already has an allocator then leave it alone
  if (this->has_creator()) return;
  Class_sp aCxxDerivableAncestorClass_unsafe; // Danger!  Unitialized!
  for (auto cur : superclasses) {
    T_sp tsuper = oCar(cur);
    if (Class_sp aSuperClass = tsuper.asOrNull<Class_O>() ) {
      if (aSuperClass->cxxClassP() && !aSuperClass->cxxDerivableClassP()) {
        SIMPLE_ERROR(BF("You cannot derive from the non-derivable C++ class %s\n"
                        "any C++ class you want to derive from must inherit from clbind::Adapter") %
                     _rep_(aSuperClass->className()));
      }
      gc::Nilable<Class_sp> aPossibleCxxDerivableAncestorClass = identifyCxxDerivableAncestorClass(aSuperClass);
      if (aPossibleCxxDerivableAncestorClass.notnilp()) {
        if (!aCxxDerivableAncestorClass_unsafe) {
          aCxxDerivableAncestorClass_unsafe = aPossibleCxxDerivableAncestorClass;
        } else {
          SIMPLE_ERROR(BF("Only one derivable C++ class is allowed to be"
                          " derived from at a time instead we have two %s and %s ") %
                       _rep_(aCxxDerivableAncestorClass_unsafe->className()) % _rep_(aPossibleCxxDerivableAncestorClass->className()));
        }
      }
    } else if ( Instance_sp iSuperClass = tsuper.asOrNull<Instance_O>() ) {
      SIMPLE_ERROR(BF("In Clasp, Instances are never Classes - so this error should never occur.  If it does - figure out why tsuper is an Instance"));
      // I don't think I do anything here
      // If aCxxDerivableAncestorClass_unsafe is left unchanged then
      // an InstanceCreator_O will be created for this class.
    }
  }
  if (aCxxDerivableAncestorClass_unsafe) {
    // Here aCxxDerivableAncestorClass_unsafe has a value - so it's ok to dereference it
    Creator_sp aCxxAllocator(gctools::As<Creator_sp>(aCxxDerivableAncestorClass_unsafe->class_creator()));
    Creator_sp dup = aCxxAllocator->duplicateForClassName(this->name());
    this->setCreator(dup); // this->setCreator(dup.get());
  } else {
    // I think this is the most common outcome -
//    printf("%s:%d   Creating an InstanceCreator_O for the class: %s\n", __FILE__, __LINE__, _rep_(this->name()).c_str());
    InstanceCreator_sp instanceAllocator = gc::GC<InstanceCreator_O>::allocate(this->asSmartPtr());
    //gctools::StackRootedPointer<InstanceCreator> instanceAllocator(new InstanceCreator(this->name()));
    this->setCreator(instanceAllocator); // this->setCreator(instanceAllocator.get());
  }
}

CL_LISPIFY_NAME("core:nameOfClass");
CL_DEFMETHOD Symbol_sp Class_O::className() const {
  return this->name();
  //    SIMPLE_ERROR(BF("You should use instanceClassName rather than className for classes"));
}

string Class_O::classNameAsString() const {
  return _rep_(this->name());
  //    SIMPLE_ERROR(BF("You should use instanceClassName rather than className for classes"));
}

#if 0
T_sp Class_O::allocate_newNil() {
  ASSERTF(this->_theCreator, BF("The class %s does not have a creator defined") % this->classNameAsString() );
  T_sp newObject = this->_theCreator->creator_allocate();
  return newObject;
}
#endif

T_sp Class_O::allocate_newClass(Class_sp metaClass, int slots) {
  ASSERTF(this->_theCreator, BF("The class %s does not have a creator defined") % this->classNameAsString() );
  Class_sp newClass = gc::As<Class_sp>(this->_theCreator->creator_allocate());
  newClass->_MetaClass = metaClass;
  newClass->_NumberOfSlots = slots;
//  printf("%s:%d  Initialize class slots here?????\n", __FILE__, __LINE__);
  return newClass;
}

T_sp Class_O::make_instance() {
  T_sp instance = this->_theCreator->creator_allocate(); //this->allocate_newNil();
  if (instance.generalp()) {
    instance.unsafe_general()->initialize();
  } else {
    SIMPLE_ERROR(BF("Add support to make_instance of non general objects"));
  }
  return instance;
}

bool Class_O::isSubClassOf(Class_sp ancestor) const {
  if (this == ancestor.get())
    return true;
  // TODO: I need to memoize this somehow so that I'm not constantly searching a list in
  // linear time
  List_sp cpl = this->instanceRef(Class_O::REF_CLASS_CLASS_PRECEDENCE_LIST);
  ASSERTF(!cpl.unboundp(), BF("You tried to use isSubClassOf when the ClassPrecedenceList had not been initialized"))
  for (auto cur : cpl) {
    if (oCar(cur) == ancestor)
      return true;
  }
  return false;
}

#if defined(XML_ARCHIVE)
void Class_O::archive(ArchiveP node) {
  IMPLEMENT_ME();
}
#endif // defined(XML_ARCHIVE)

void Class_O::initialize() {
  this->Base::initialize();
  this->initializeSlots(this->_NumberOfSlots);
}

string Class_O::__repr__() const {
  if (this == _lisp->_true().get()) {
    return "#<built-in-class t>";
  }
  stringstream ss;
  ss << "#<" << _rep_(this->_MetaClass->name()) << " " << this->instanceClassName() << ">";

  return ss.str();
}

string Class_O::getPackageName() const {
  return gc::As<Package_sp>(this->name()->getPackage())->getName();
}

// LambdaListHandler_sp Class_O::__init__lambdaListHandler()
// {
//     return _Nil<LambdaListHandler_O>();
// }

string Class_O::dumpInfo() {
  stringstream ss;
  ss << (boost::format("this.instanceClassName: %s @ %X") % this->instanceClassName() % this) << std::endl;
  ss << "_FullName[" << this->name()->fullName() << "]" << std::endl;
  ss << boost::format("    _Class = %X  this._Class.instanceClassName()=%s\n") % this->__class().get() % this->__class()->instanceClassName();
  for (auto cc : this->directSuperclasses()) {
    ss << "Base class: " << gc::As<Class_sp>(oCar(cc))->instanceClassName() << std::endl;
  }
  ss << boost::format("this.instanceCreator* = %p") % (void *)(&*this->class_creator()) << std::endl;
  return ss.str();
}

string Class_O::getPackagedName() const {
  return this->name()->formattedName(false);
}

void Class_O::accumulateSuperClasses(HashTableEq_sp supers, VectorObjects_sp arrayedSupers, Class_sp mc) {
  if (IS_SYMBOL_UNDEFINED(mc->className()))
    return;
  //	printf("%s:%d accumulateSuperClasses of: %s\n", __FILE__, __LINE__, _rep_(mc->className()).c_str() );
  if (supers->contains(mc))
    return;
  Fixnum arraySuperLength = arrayedSupers->length();
  supers->setf_gethash(mc, clasp_make_fixnum(arraySuperLength));
  arrayedSupers->vectorPushExtend(mc);
  List_sp directSuperclasses = mc->directSuperclasses();
  //	printf("%s:%d accumulateSuperClasses arraySuperLength = %d\n", __FILE__, __LINE__, arraySuperLength->get());
  for (auto cur : directSuperclasses) // ; cur.notnilp(); cur=cCdr(cur) )
  {
    T_sp one = oCar(cur);
    Class_sp oneClass = gc::As<Class_sp>(one);
    accumulateSuperClasses(supers, arrayedSupers, oneClass);
  }
}
};

namespace gcroots {

#if 0
    GC_RESULT stl_onHeapScanGCRoots(map<core::Class_sp,int>::iterator& it, GC_SCAN_ARGS_PROTOTYPE)
    {
        GC_SCANNER_BEGIN() {
            SMART_PTR_FIX(it->first);
        } GC_SCANNER_END();
        return GC_RES_OK;
    }
#endif
};

namespace core {

void Class_O::lowLevel_calculateClassPrecedenceList() {
  using namespace boost;
  HashTableEq_sp supers = HashTableEq_O::create_default();
  VectorObjects_sp arrayedSupers(VectorObjects_O::make(16, _Nil<T_O>(), clasp_make_fixnum(0)));
  this->accumulateSuperClasses(supers, arrayedSupers, this->sharedThis<Class_O>());
  vector<list<int>> graph(cl__length(arrayedSupers));

  class TopoSortSetup : public KeyValueMapper {
  private:
    HashTable_sp supers;
    vector<list<int>> *graphP;

  public:
    TopoSortSetup(HashTable_sp asupers, vector<list<int>> *gP) : supers(asupers), graphP(gP){};
    virtual bool mapKeyValue(T_sp key, T_sp value) {
      Fixnum_sp fnValue(gc::As<Fixnum_sp>(value));
      int mcIndex = unbox_fixnum(fnValue);
      Class_sp mc = gc::As<Class_sp>(key);
      for (auto mit : (List_sp)(mc->directSuperclasses())) {
        T_sp val = this->supers->gethash(oCar(mit));
        ASSERT(val.notnilp());
        Fixnum_sp fnval = gc::As<Fixnum_sp>(val);
        int aSuperIndex = unbox_fixnum(fnval);
        (*this->graphP)[mcIndex].push_front(aSuperIndex);
      }
      return true;
    }
  };
  TopoSortSetup topoSortSetup(supers, &graph);
  supers->lowLevelMapHash(&topoSortSetup);
#ifdef DEBUG_ON
  {
    for (size_t zi(0), ziEnd(cl__length(arrayedSupers)); zi < ziEnd; ++zi) {
      stringstream ss;
      ss << (BF("graph[%d/name=%s] = ") % zi % arrayedSupers->operator[](zi).as<Class_O>()->instanceClassName()).str();
      for (list<int>::const_iterator it = graph[zi].begin(); it != graph[zi].end(); it++) {
        ss << *it << "-> ";
      }
      ss << ";";
      LOG(BF("%s") % ss.str());
    }
  }
#endif
  deque<int> topo_order;
  topological_sort(graph, front_inserter(topo_order), vertex_index_map(identity_property_map()));
#ifdef DEBUG_ON
  {
    stringstream ss;
    ss << "Topologically sorted superclasses ";
    for (deque<int>::const_reverse_iterator it = topo_order.rbegin(); it != topo_order.rend(); it++) {
      Class_sp mc = arrayedSupers->operator[](*it).as<Class_O>();
      ss << "-> " << mc->className() << "/" << mc->instanceClassName();
    }
    LOG(BF("%s") % ss.str());
  }
#endif
  List_sp cpl = _Nil<T_O>();
  for (deque<int>::const_reverse_iterator it = topo_order.rbegin(); it != topo_order.rend(); it++) {
    Class_sp mc = gc::As<Class_sp>(arrayedSupers->operator[](*it));
    LOG(BF("pushing superclass[%s] to front of ClassPrecedenceList") % mc->instanceClassName());
    cpl = Cons_O::create(mc, cpl);
  }
  this->instanceSet(REF_CLASS_CLASS_PRECEDENCE_LIST, cpl);
}

#if 0
    void Class_O::lowLevel_calculateClassPrecedenceList()
    {_OF();
	using namespace boost;
        gctools::StackRootedStlContainer<map<Class_sp,int> > supers;
        gctools::StackRootedStlContainer<vector<Class_sp> > arrayedSupers;
	this->accumulateSuperClasses(supers.get(),arrayedSupers.get(),this->sharedThis<Class_O>());
	vector< list<int> > graph(arrayedSupers.get().size());
	for ( auto it_gc_safe : supers.get() )
	{
	    int mcIndex = it_gc_safe.second;
	    for ( Cons_sp mit=it_gc_safe.first->directSuperclasses(); mit.notnilp(); mit=cCdr(mit) )
	    {
		if ( IS_SYMBOL_DEFINED((oCar(mit).as<Class_O>())->className()) )
		{
		    graph[mcIndex].push_front(supers.get()[oCar(mit).as<Class_O>()]);
		}
	    }
	}
#ifdef DEBUG_ON
	{
	    for ( uint zi=0; zi<arrayedSupers.get().size(); zi++ )
	    {
		stringstream ss;
		ss << (BF("graph[%d/name=%s] = ") % zi % arrayedSupers.get()[zi]->instanceClassName() ).str();
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
		Class_sp mc = arrayedSupers.get()[*it];
		ss << "-> " << mc->className() << "/" <<mc->instanceClassName();
	    }
	    LOG(BF("%s") % ss.str() );
	}
#endif
	List_sp cpl = _Nil<T_O>();
	for ( deque<int>::const_reverse_iterator it=topo_order.rbegin(); it!=topo_order.rend(); it++ )
	{
	    Class_sp mc = arrayedSupers.get()[*it];
	    LOG(BF("pushing superclass[%s] to front of ClassPrecedenceList") % mc->instanceClassName() );
	    cpl = Cons_O::create(mc,cpl);
	}
	this->instanceSet(REF_CLASS_CLASS_PRECEDENCE_LIST,cpl);
    }
#endif

void Class_O::addInstanceBaseClassDoNotCalculateClassPrecedenceList(Symbol_sp className) {
  _OF();
  Class_sp cl = gc::As<Class_sp>(eval::funcall(cl::_sym_findClass, className, _lisp->_true()));
  // When booting _DirectSuperClasses may be undefined
  ASSERT(this->directSuperclasses());
  List_sp dsc = _Nil<List_V>();
  if (!this->directSuperclasses().unboundp()) {
    dsc = this->directSuperclasses();
  }
  this->instanceSet(REF_CLASS_DIRECT_SUPERCLASSES, Cons_O::create(cl, dsc));
}

void Class_O::addInstanceBaseClass(Symbol_sp className) {
  this->addInstanceBaseClassDoNotCalculateClassPrecedenceList(className);
  this->lowLevel_calculateClassPrecedenceList();
}

void Class_O::setInstanceBaseClasses(List_sp classes) {
  this->instanceSet(REF_CLASS_DIRECT_SUPERCLASSES, cl__copy_list(classes));
  this->lowLevel_calculateClassPrecedenceList();
}

CL_LISPIFY_NAME("clos:direct-superclasses");
CL_DEFMETHOD List_sp Class_O::directSuperclasses() const {
  return coerce_to_list(this->instanceRef(REF_CLASS_DIRECT_SUPERCLASSES));
}

void Class_O::appendDirectSuperclassAndResetClassPrecedenceList(Class_sp superClass) {
  List_sp directSuperclasses = this->directSuperclasses();
  directSuperclasses = Cons_O::create(superClass, directSuperclasses);
  this->instanceSet(REF_CLASS_DIRECT_SUPERCLASSES, directSuperclasses);
  this->instanceSet(REF_CLASS_CLASS_PRECEDENCE_LIST, _Nil<T_O>());
}

/*
  __BEGIN_DOC(classes.classMethods.describe,describe)
  \scriptCmd{describe}{classObject}

  Dumps a description of the class to stdout.
  __END_DOC
*/
void Class_O::describe(T_sp stream) {
  stringstream ss;
  ss << (BF("Class instanceClassName %s\n") % this->instanceClassName().c_str()).str();
  ss << (BF("FullName %s\n") % this->name()->fullName().c_str()).str();
  if (this->directSuperclasses().nilp()) {
    ss << (BF("There are no super-classes!!!!!!\n")).str();
  } else {
    for (Cons_sp cc : this->directSuperclasses()) {
      ss << (BF("directSuperclasses: %s\n") % gc::As<Class_sp>(oCar(cc))->instanceClassName().c_str()).str();
    }
  }
  ss << (BF(" this.instanceCreator* = %p\n") % (void *)(this->class_creator().raw_())).str();
  ss << (BF("cxxClassP[%d]  cxxDerivableClassP[%d]   primaryCxxDerivableClassP[%d]\n") % this->cxxClassP() % this->cxxDerivableClassP() % this->primaryCxxDerivableClassP()).str();
  clasp_write_string(ss.str(), stream);
}

T_sp Class_O::instanceRef(size_t idx) const {
  if (idx>= this->_MetaClassSlots.size()) {
    SIMPLE_ERROR(BF("Class slot %d is out of bounds only %d slots available with metaclass %s") % idx % this->_MetaClassSlots.size() % this->_MetaClass->classNameAsString().c_str());
  }
  ASSERT(this->_MetaClassSlots[idx]);
  T_sp val = this->_MetaClassSlots[idx];
  return val;
}

T_sp Class_O::instanceSet(size_t idx, T_sp val) {
  if (idx>= this->_MetaClassSlots.size()) {
    SIMPLE_ERROR(BF("Class slot %d is out of bounds only %d slots available with metaclass %s") % idx % this->_MetaClassSlots.size()  % this->_MetaClass->classNameAsString().c_str());
  }
  this->_MetaClassSlots[idx] = val;
  return val;
}

T_sp Class_O::copyInstance() const {
  GC_COPY(Class_O,c,*this);
  return c;
};
/*! Return true if every member of subset is in superset */
bool subsetp(List_sp subset, List_sp superset) {
  ASSERT(subset);
  ASSERT(superset);
  if (subset.nilp() && superset.nilp())
    return true;
  if (superset.nilp())
    return false;
  if (subset.nilp())
    return true;
  for (; subset.notnilp(); subset = oCdr(subset)) {
    T_sp o = oCar(subset);
    if (!superset.asCons()->memberEq(o))
      return false;
  }
  return true;
}

T_sp Class_O::instanceSigSet() {
  // Do nothing
  Class_sp mc = gc::As<Class_sp>(this->_instanceClass());
  ASSERTNOTNULL(mc);
  T_sp sig = mc->slots();
  ASSERTNOTNULL(sig);
  this->_Signature_ClassSlots = sig;
#if DEBUG_CLOS >= 2
  printf("\nMLOG instance_set_sig object %p\n", (void *)(this));
#endif
  return sig;
}

T_sp Class_O::instanceSig() const {
  ASSERTNOTNULL(this->_Signature_ClassSlots);
#if DEBUG_CLOS >= 2
  printf("\nMLOG instance_sig object %p\n", (void *)(this));
#endif
  return this->_Signature_ClassSlots;
}

Class_sp Class_O::_instanceClass() const {
  return this->_MetaClass;
}

T_sp Class_O::instanceClassSet(Class_sp mc) {
  if (mc.get() == this)
    return mc;
  SIMPLE_ERROR(BF("You cannot change the meta-class of a class object"));
}

void Class_O::__setupStage3NameAndCalculateClassPrecedenceList(Symbol_sp className) {
  this->setName(className);
  // Initialize some of the class slots
  this->instanceSet(REF_CLASS_DIRECT_SLOTS,_Nil<T_O>());
  this->instanceSet(REF_CLASS_DEFAULT_INITARGS,_Nil<T_O>());
  T_sp tmc = this->_instanceClass();
  ASSERTNOTNULL(tmc);
  Class_sp mc = gc::As<Class_sp>(tmc);
  (void)mc;
  this->lowLevel_calculateClassPrecedenceList();
}

CL_DEFUN List_sp core__class_slot_sanity_check()
{
  List_sp sanity = _Nil<T_O>();
  sanity = Cons_O::create(Cons_O::create(clos::_sym_name, core::clasp_make_fixnum(Class_O::REF_CLASS_CLASS_NAME)),sanity);
  sanity = Cons_O::create(Cons_O::create(clos::_sym_DIRECT_SUPERCLASSES, core::clasp_make_fixnum(Class_O::REF_CLASS_DIRECT_SUPERCLASSES)),sanity);
  sanity = Cons_O::create(Cons_O::create(clos::_sym_SLOTS, core::clasp_make_fixnum(Class_O::REF_CLASS_SLOTS)),sanity);
  sanity = Cons_O::create(Cons_O::create(clos::_sym_DIRECT_DEFAULT_INITARGS, core::clasp_make_fixnum(Class_O::REF_CLASS_DIRECT_DEFAULT_INITARGS)),sanity);
  sanity = Cons_O::create(Cons_O::create(clos::_sym_FINALIZED, core::clasp_make_fixnum(Class_O::REF_CLASS_FINALIZED)),sanity);
  sanity = Cons_O::create(Cons_O::create(clos::_sym_PRECEDENCE_LIST, core::clasp_make_fixnum(Class_O::REF_CLASS_CLASS_PRECEDENCE_LIST)),sanity);
  sanity = Cons_O::create(Cons_O::create(clos::_sym_DIRECT_SLOTS, core::clasp_make_fixnum(Class_O::REF_CLASS_DIRECT_SLOTS)),sanity);
  sanity = Cons_O::create(Cons_O::create(clos::_sym_DEFAULT_INITARGS, core::clasp_make_fixnum(Class_O::REF_CLASS_DEFAULT_INITARGS)),sanity);
  sanity = Cons_O::create(Cons_O::create(clos::_sym_NUMBER_OF_SLOTS_IN_STANDARD_CLASS, core::clasp_make_fixnum(REF_CLASS_NUMBER_OF_SLOTS_IN_STANDARD_CLASS)),sanity);
  sanity = Cons_O::create(Cons_O::create(clos::_sym_NUMBER_OF_SLOTS_IN_STRUCTURE_CLASS, core::clasp_make_fixnum(REF_CLASS_NUMBER_OF_SLOTS_IN_STRUCTURE_CLASS)),sanity);
  return sanity;
}

  
CL_LAMBDA(low high);
CL_DECLARE();
CL_DOCSTRING("subclassp");
CL_DEFUN bool core__subclassp(T_sp low, T_sp high) {
  if (low == high)
    return true;
  if (Class_sp lowmc = low.asOrNull<Class_O>()) {
    List_sp lowClassPrecedenceList = lowmc->instanceRef(Class_O::REF_CLASS_CLASS_PRECEDENCE_LIST); // classPrecedenceList();
    return lowClassPrecedenceList.asCons()->memberEq(high).notnilp();
  } else if (Instance_sp inst = low.asOrNull<Instance_O>()) {
    (void)inst;
    IMPLEMENT_MEF(BF("Run some other tests to make sure that instance is a Class: %s") % _rep_(low));
  }
  SIMPLE_ERROR(BF("Illegal argument for subclassp: %s") % _rep_(low));
};

SYMBOL_SC_(CorePkg, subclassp);
SYMBOL_SC_(CorePkg, allocateRawClass);
SYMBOL_EXPORT_SC_(CorePkg, inheritDefaultAllocator);




};
