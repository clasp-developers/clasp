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
#define DEBUG_LEVEL_FULL

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
#include <clasp/core/vectorObjectsWithFillPtr.h>
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

namespace translate {

template <>
struct to_object<core::Creator *> {
  typedef core::Creator *GivenType;
  static core::T_sp convert(GivenType v) {
    _G();
    if (v)
      return core::Pointer_O::create(v);
    return _Nil<core::T_O>();
  }
};
};

namespace core {

const int Class_O::NumberOfClassSlots;

#define ARGS_af_inheritDefaultAllocator "(class directSuperclasses)"
#define DECL_af_inheritDefaultAllocator ""
#define DOCS_af_inheritDefaultAllocator "inheritDefaultAllocator - make this a regular function so that there are no dispatching problems at boot time"
void af_inheritDefaultAllocator(Class_sp cl, T_sp directSuperclasses) {
  _G();
  //        printf("%s:%d In af_inheritDefaultAllocator for class: %s direct-superclasses: %s\n",__FILE__,__LINE__, _rep_(cl).c_str(), _rep_(directSuperclasses).c_str());
  cl->inheritDefaultAllocator(directSuperclasses);
};

#define ARGS_af_allocateRawClass "(original meta-class slots &optional name)"
#define DECL_af_allocateRawClass ""
#define DOCS_af_allocateRawClass "allocateRawClass - behaves like ECL instance::allocate_raw_instance, The allocator for the new class is taken from (allocatorPrototype).  If (allocatorPrototype) is nil then use the allocator for Instance_O."
T_sp af_allocateRawClass(T_sp orig, Class_sp metaClass, int slots, T_sp className) {
  _G();
  if (orig.notnilp()) {
    SIMPLE_ERROR(BF("Deal with non-nil orig class in allocateRawClass"));
    // Check out ecl/src/c/instance.d/si_allocate_raw_instance
  }
  ASSERTF(metaClass->hasCreator(), BF("The metaClass allocator should always be defined - for class %s it hasn't been") % _rep_(metaClass));
  Class_sp newClass = gc::As<Class_sp>(metaClass->allocate_newNil());
  newClass->initialize();
  newClass->initializeSlots(slots);
#if DEBUG_CLOS >= 2
  printf("\nMLOG allocate-raw-CLASS    %p number_of_slots[%d]\n", (void *)(newClass.get()), slots);
#endif
  return newClass;
};

Class_O::Class_O() : Class_O::Base(), _Signature_ClassSlots(_Unbound<T_O>()), _theCreator(){};

void Class_O::initializeSlots(int slots) {
  if (slots < Class_O::NumberOfClassSlots) {
    SIMPLE_ERROR(BF("Classes need at least %d slots - you asked for %d") % Class_O::NumberOfClassSlots % slots);
  }
  this->_MetaClassSlots.resize(slots, _Unbound<T_O>());
  this->instanceSet(REF_DIRECT_SUPERCLASSES, _Nil<T_O>());
  this->instanceSet(REF_FINALIZED, cl::_sym_T_O);
}

T_sp InstanceCreator::allocate() {
  GC_ALLOCATE(Instance_O, output);
  return output;
};

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
  if (this->hasCreator())
    return;
  Class_sp aCxxDerivableAncestorClass_unsafe; // Danger!  Unitialized!
  for (auto cur : superclasses) {
    Class_sp aSuperClass = gc::As<Class_sp>(oCar(cur));
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
  }
  if (aCxxDerivableAncestorClass_unsafe) {
    // Here aCxxDerivableAncestorClass_unsafe has a value - so it's ok to dereference it
    gc::tagged_pointer<Creator> aCxxAllocator(aCxxDerivableAncestorClass_unsafe->getCreator());
    // gctools::StackRootedPointer<Creator> aCxxAllocator(aCxxDerivableAncestorClass_unsafe->getCreator());
    gc::tagged_pointer<Creator> dup(aCxxAllocator->duplicateForClassName(this->name()));
    //gctools::StackRootedPointer<Creator> dup(aCxxAllocator->duplicateForClassName(this->name()));
    this->setCreator(dup); // this->setCreator(dup.get());
  } else {
    gc::tagged_pointer<InstanceCreator> instanceAllocator = gctools::ClassAllocator<InstanceCreator>::allocateClass(this->name());
    //gctools::StackRootedPointer<InstanceCreator> instanceAllocator(new InstanceCreator(this->name()));
    this->setCreator(instanceAllocator); // this->setCreator(instanceAllocator.get());
  }
}

Symbol_sp Class_O::className() const {
  return this->name();
  //    SIMPLE_ERROR(BF("You should use instanceClassName rather than className for classes"));
}

string Class_O::classNameAsString() const {
  return _rep_(this->name());
  //    SIMPLE_ERROR(BF("You should use instanceClassName rather than className for classes"));
}

T_sp Class_O::allocate_newNil() {
  _G();
  if (!this->_theCreator) {
    IMPLEMENT_MEF(BF("All allocation should be done through _creator"));
    // if the newNil_callback is NULL then allocate an instance
    int slots = unbox_fixnum(gc::As<Fixnum_sp>(this->_MetaClassSlots[REF_SIZE]));
    printf("%s:%d:%s  Allocating new instance of %s with %d slots\n", __FILE__, __LINE__, __FUNCTION__, _rep_(this->asSmartPtr()).c_str(), slots);
    return Instance_O::allocateInstance(this->asSmartPtr(), slots);
    //	    SIMPLE_ERROR(BF("_creator for %s is NULL!!") % _rep_(this->asSmartPtr()) );
  }
  T_sp newObject = this->_theCreator->allocate();
  return newObject;
}

T_sp Class_O::make_instance() {
  _G();
  T_sp instance = this->allocate_newNil();
  instance->initialize();
  return instance;
}

bool Class_O::isSubClassOf(Class_sp ancestor) const {
  _G();
  if (this == ancestor.get())
    return true;
  // TODO: I need to memoize this somehow so that I'm not constantly searching a list in
  // linear time
  List_sp cpl = this->instanceRef(Class_O::REF_CLASS_PRECEDENCE_LIST);
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
}

string Class_O::__repr__() const {
  if (this == _lisp->_true().get()) {
    return "#<built-in-class t>";
  }
  stringstream ss;
  ss << "#<" << _rep_(this->__class()->className()) << " " << this->instanceClassName() << " @" << (void *)(this) << ">";

  return ss.str();
}

string Class_O::getPackageName() const {
  return gc::As<Package_sp>(this->name()->getPackage())->getName();
}

// LambdaListHandler_sp Class_O::__init__lambdaListHandler()
// {_G();
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
  ss << boost::format("this.instanceCreator* = %p") % (void *)(&*this->getCreator()) << std::endl;
  return ss.str();
}

string Class_O::getPackagedName() const {
  return this->name()->formattedName(false);
}

void Class_O::accumulateSuperClasses(HashTableEq_sp supers, VectorObjectsWithFillPtr_sp arrayedSupers, Class_sp mc) {
  _G();
  if (IS_SYMBOL_UNDEFINED(mc->className()))
    return;
  //	printf("%s:%d accumulateSuperClasses of: %s\n", __FILE__, __LINE__, _rep_(mc->className()).c_str() );
  if (supers->contains(mc))
    return;
  Fixnum_sp arraySuperLength = make_fixnum(arrayedSupers->length());
  supers->setf_gethash(mc, arraySuperLength);
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
  _G();
  using namespace boost;
  HashTableEq_sp supers = HashTableEq_O::create_default();
  VectorObjectsWithFillPtr_sp arrayedSupers(VectorObjectsWithFillPtr_O::make(_Nil<T_O>(), _Nil<T_O>(), 16, 0, true, cl::_sym_T_O));
  this->accumulateSuperClasses(supers, arrayedSupers, this->sharedThis<Class_O>());
  vector<list<int>> graph(cl_length(arrayedSupers));

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
        ASSERTF(val.notnilp(), BF("val.notnilp() failed"));
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
    for (size_t zi(0), ziEnd(cl_length(arrayedSupers)); zi < ziEnd; ++zi) {
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
  this->instanceSet(REF_CLASS_PRECEDENCE_LIST, cpl);
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
	this->instanceSet(REF_CLASS_PRECEDENCE_LIST,cpl);
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
  this->instanceSet(REF_DIRECT_SUPERCLASSES, Cons_O::create(cl, dsc));
}

void Class_O::addInstanceBaseClass(Symbol_sp className) {
  _OF();
  this->addInstanceBaseClassDoNotCalculateClassPrecedenceList(className);
  this->lowLevel_calculateClassPrecedenceList();
}

void Class_O::setInstanceBaseClasses(List_sp classes) {
  _OF();
  this->instanceSet(REF_DIRECT_SUPERCLASSES, cl_copyList(classes));
  this->lowLevel_calculateClassPrecedenceList();
}

List_sp Class_O::directSuperclasses() const {
  _OF();
  return coerce_to_list(this->instanceRef(REF_DIRECT_SUPERCLASSES));
}

void Class_O::appendDirectSuperclassAndResetClassPrecedenceList(Class_sp superClass) {
  _G();
  List_sp directSuperclasses = this->directSuperclasses();
  directSuperclasses = Cons_O::create(superClass, directSuperclasses);
  this->instanceSet(REF_DIRECT_SUPERCLASSES, directSuperclasses);
  this->instanceSet(REF_CLASS_PRECEDENCE_LIST, _Nil<T_O>());
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
  ss << (BF(" this.instanceCreator* = %p\n") % (void *)(this->getCreator().raw_())).str();
  ss << (BF("cxxClassP[%d]  cxxDerivableClassP[%d]   primaryCxxDerivableClassP[%d]\n") % this->cxxClassP() % this->cxxDerivableClassP() % this->primaryCxxDerivableClassP()).str();
  clasp_write_string(ss.str(), stream);
}

T_sp Class_O::instanceRef(int idx) const {
  ASSERTF(idx >= 0 && idx < this->_MetaClassSlots.size(), BF("Out of range index %d for instanceRef(%d)") % idx % this->_MetaClassSlots.size());
  ASSERT(this->_MetaClassSlots[idx]);
  return this->_MetaClassSlots[idx];
}

T_sp Class_O::instanceSet(int idx, T_sp val) {
  ASSERTF(idx >= 0 && idx < this->_MetaClassSlots.size(), BF("Out of range index %d for instanceRef(%d)") % idx % this->_MetaClassSlots.size());
  this->_MetaClassSlots[idx] = val;
  return val;
}

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
  return this->__class();
}

T_sp Class_O::instanceClassSet(Class_sp mc) {
  if (mc.get() == this)
    return mc;
  SIMPLE_ERROR(BF("You cannot change the meta-class of a class object"));
}

void Class_O::__setupStage3NameAndCalculateClassPrecedenceList(Symbol_sp className) {
  _G();
  this->setName(className);
  T_sp tmc = this->_instanceClass();
  ASSERTNOTNULL(tmc);
  Class_sp mc = gc::As<Class_sp>(tmc);
  (void)mc;
  this->lowLevel_calculateClassPrecedenceList();
}

#define ARGS_af_subclassp "(low high)"
#define DECL_af_subclassp ""
#define DOCS_af_subclassp "subclassp"
bool af_subclassp(T_sp low, T_sp high) {
  _G();
  if (low == high)
    return true;
  if (Class_sp lowmc = low.asOrNull<Class_O>()) {
    List_sp lowClassPrecedenceList = lowmc->instanceRef(Class_O::REF_CLASS_PRECEDENCE_LIST); // classPrecedenceList();
    return lowClassPrecedenceList.asCons()->memberEq(high).notnilp();
  } else if (Instance_sp inst = low.asOrNull<Instance_O>()) {
    (void)inst;
    IMPLEMENT_MEF(BF("Run some other tests to make sure that instance is a Class: %s") % _rep_(low));
  }
  SIMPLE_ERROR(BF("Illegal argument for subclassp: %s") % _rep_(low));
};

void Class_O::exposeCando(Lisp_sp lisp) {
  class_<Class_O>()
      .def("core:nameOfClass", &Class_O::className)
      .def("core:direct-superclasses", &Class_O::directSuperclasses)
      .def("core:hasCreator", &Class_O::hasCreator)
      //      .def("core:getCreator", &Class_O::getCreator);
      //	SYMBOL_SC_(CorePkg,makeSureClosClassSlotsMatchClass);
      //	Defun(makeSureClosClassSlotsMatchClass);
      SYMBOL_SC_(CorePkg, subclassp);
  Defun(subclassp);
  SYMBOL_SC_(CorePkg, allocateRawClass);
  Defun(allocateRawClass);
  SYMBOL_EXPORT_SC_(CorePkg, inheritDefaultAllocator);
  Defun(inheritDefaultAllocator);
}
void Class_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, Class, "", "", _lisp);
#endif
}

EXPOSE_CLASS(core, Class_O);
};
