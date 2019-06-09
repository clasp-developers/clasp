/*
    File: instance.cc
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

#pragma clang diagnostic push
#pragma GCC diagnostic ignored "-Warray-bounds"
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunneeded-internal-declaration"
//#pragma GCC diagnostic ignored "-Wunused-local-typedef"
#include <boost/graph/vector_as_graph.hpp>
#include <boost/graph/topological_sort.hpp>
#pragma clang diagnostic pop

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/symbolTable.h>
#include <clasp/gctools/gcFunctions.h>
#include <clasp/core/serialize.h>
#include <clasp/core/lispList.h>
#include <clasp/core/mpPackage.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/genericFunction.h>
#include <clasp/core/instance.h>
#include <clasp/core/funcallableInstance.h>
#include <clasp/core/wrappers.h>

namespace core {

Rack_sp Rack_O::make(size_t numSlots, T_sp initialValue )
{
  auto bs = gctools::GC<Rack_O>::allocate_container(false,numSlots,initialValue,true);
  return bs;
}

CL_LAMBDA(instance func);
CL_DECLARE();
CL_DOCSTRING("instanceClassSet");
CL_DEFUN T_sp core__instance_class_set(T_sp obj, Instance_sp mc) {
  if (Instance_sp iobj = obj.asOrNull<Instance_O>()) {
    return iobj->instanceClassSet(mc);
  }
  SIMPLE_ERROR(BF("You can only instanceClassSet on Instance_O or Instance_O - you tried to set it on a: %s") % _rep_(mc));
};

CL_LAMBDA(obj);
CL_DECLARE();
CL_DOCSTRING("copy-instance returns a shallow copy of the instance");
CL_DEFUN T_sp core__copy_instance(T_sp obj) {
  if (gc::IsA<Instance_sp>(obj)) {
    Instance_sp iobj = gc::As_unsafe<Instance_sp>(obj);
    Instance_sp cp = gc::As_unsafe<Instance_sp>(iobj->copyInstance());
    return cp;
  }
  SIMPLE_ERROR(BF("copy-instance doesn't support copying %s") % _rep_(obj));
};

void Instance_O::initializeSlots(gctools::ShiftedStamp stamp, size_t numberOfSlots) {
  ASSERT(stamp==0||gctools::Header_s::Value::is_rack_shifted_stamp(stamp));
  this->_Rack = Rack_O::make(numberOfSlots,_Unbound<T_O>());
  this->stamp_set(stamp);
#ifdef DEBUG_GUARD_VALIDATE
  client_validate(this->_Rack);
#endif
//  printf("%s:%d  Make sure you initialize slots for classes this->_Class -> %s\n", __FILE__, __LINE__, _rep_(this->_Class).c_str());
}

CL_DEFUN void core__specializer_call_history_generic_functions_push_new(T_sp tclass_, T_sp generic_function)
{
  Instance_sp class_ = gc::As<Instance_sp>(tclass_);
  class_->CLASS_call_history_generic_functions_push_new(generic_function);
}

void Instance_O::CLASS_call_history_generic_functions_push_new(T_sp generic_function) {
  if (this->instanceRef(REF_SPECIALIZER_MUTEX).unboundp()) {
    SimpleBaseString_sp sbsr = SimpleBaseString_O::make("CALHISR");
    SimpleBaseString_sp sbsw = SimpleBaseString_O::make("CALHISW");
    this->instanceSet(REF_SPECIALIZER_MUTEX,mp::SharedMutex_O::make_shared_mutex(sbsr,sbsw));
  }
  ClassWriteLock(gc::As<mp::SharedMutex_sp>(this->instanceRef(REF_SPECIALIZER_MUTEX)));
  List_sp gflist = gc::As<List_sp>(this->instanceRef(REF_SPECIALIZER_CALL_HISTORY_GENERIC_FUNCTIONS));
  if (gflist.consp()) {
    if (gflist.unsafe_cons()->memberEq(generic_function).notnilp()) return;
  }
  this->instanceSet(REF_SPECIALIZER_CALL_HISTORY_GENERIC_FUNCTIONS,Cons_O::create(generic_function,gflist));
}

void Instance_O::CLASS_set_stamp_for_instances(gctools::ShiftedStamp s) {
  ASSERT(gctools::Header_s::Value::is_shifted_stamp(s));
  T_sp stamp((gctools::Tagged)s);
  this->instanceSet(REF_CLASS_STAMP_FOR_INSTANCES_,stamp); // write shifted stamp - it's automatically a fixnum
};

// NOT called by regular CL allocate instance. FIXME, find a way to remove this if possible.
void Instance_O::initializeClassSlots(Creator_sp creator, gctools::ShiftedStamp stamp) {
  // Should match clos/hierarchy.lsp
  ASSERT(gctools::Header_s::Value::is_shifted_stamp(stamp));
  this->instanceSet(REF_SPECIALIZER_CALL_HISTORY_GENERIC_FUNCTIONS, _Nil<T_O>());
  this->instanceSet(REF_CLASS_DIRECT_SUBCLASSES, _Nil<T_O>());
  this->instanceSet(REF_CLASS_DIRECT_SUPERCLASSES, _Nil<T_O>());
  this->instanceSet(REF_CLASS_DIRECT_DEFAULT_INITARGS, _Nil<T_O>());
  this->instanceSet(REF_CLASS_FINALIZED, _Nil<T_O>());
  this->instanceSet(REF_CLASS_DEPENDENTS, _Nil<T_O>());
  this->instanceSet(REF_CLASS_LOCATION_TABLE, _Nil<T_O>());
  this->CLASS_set_stamp_for_instances(stamp);
  this->instanceSet(REF_CLASS_CREATOR, creator);
}


CL_DEFUN List_sp core__class_slot_sanity_check()
{
  List_sp sanity = _Nil<T_O>();
  sanity = Cons_O::create(Cons_O::create(clos::_sym_NUMBER_OF_SLOTS_IN_STANDARD_CLASS, core::clasp_make_fixnum(REF_CLASS_NUMBER_OF_SLOTS_IN_STANDARD_CLASS)),sanity);
  sanity = Cons_O::create(Cons_O::create(clos::_sym_NUMBER_OF_SLOTS_IN_STRUCTURE_CLASS, core::clasp_make_fixnum(REF_CLASS_NUMBER_OF_SLOTS_IN_STRUCTURE_CLASS)),sanity);
#define ADD_SANITY_CHECK_SIMPLE(slot_name,enum_name)   sanity = Cons_O::create(Cons_O::create(clos::_sym_##slot_name, core::clasp_make_fixnum(Instance_O::REF_##enum_name)),sanity);
  ADD_SANITY_CHECK_SIMPLE(NAME,CLASS_CLASS_NAME);
  ADD_SANITY_CHECK_SIMPLE(DIRECT_SUPERCLASSES,CLASS_DIRECT_SUPERCLASSES);
  ADD_SANITY_CHECK_SIMPLE(SLOTS,CLASS_SLOTS);
  ADD_SANITY_CHECK_SIMPLE(DIRECT_DEFAULT_INITARGS,CLASS_DIRECT_DEFAULT_INITARGS);
  ADD_SANITY_CHECK_SIMPLE(FINALIZED,CLASS_FINALIZED);
  ADD_SANITY_CHECK_SIMPLE(PRECEDENCE_LIST,CLASS_CLASS_PRECEDENCE_LIST);
  ADD_SANITY_CHECK_SIMPLE(DIRECT_SLOTS,CLASS_DIRECT_SLOTS);
  ADD_SANITY_CHECK_SIMPLE(DEFAULT_INITARGS,CLASS_DEFAULT_INITARGS);
  ADD_SANITY_CHECK_SIMPLE(DIRECT_SUBCLASSES,CLASS_DIRECT_SUBCLASSES);
  ADD_SANITY_CHECK_SIMPLE(DEPENDENTS,CLASS_DEPENDENTS);
  ADD_SANITY_CHECK_SIMPLE(LOCATION_TABLE,CLASS_LOCATION_TABLE);
  ADD_SANITY_CHECK_SIMPLE(CALL_HISTORY_GENERIC_FUNCTIONS,SPECIALIZER_CALL_HISTORY_GENERIC_FUNCTIONS);
  ADD_SANITY_CHECK_SIMPLE(SPECIALIZER_MUTEX,SPECIALIZER_MUTEX);
  return sanity;
}

CL_LAMBDA(class slot-count);
CL_DEFUN T_sp core__allocate_new_instance(Instance_sp cl, size_t slot_count) {
  // cl is known to be a standard-class.
  ASSERT(cl->CLASS_has_creator());
  Creator_sp creator = gctools::As<Creator_sp>(cl->CLASS_get_creator());
  Instance_sp obj = gc::As_unsafe<Instance_sp>(creator->creator_allocate());
  obj->_Class = cl;
  /* Unlike other slots, the stamp must be initialized in the allocator,
   * as it's required for dispatch. */
  obj->initializeSlots(cl->CLASS_stamp_for_instances(), slot_count);
#ifdef DEBUG_COUNT_ALLOCATIONS
  gctools::count_allocation(cl->CLASS_stamp_for_instances());
#endif
  obj->_Sig = cl->slots();
  return obj;
}

// Give an instance a new class and rack. Used by change-class.
CL_DEFUN Instance_sp core__reallocate_instance(Instance_sp instance, Instance_sp new_class, size_t new_size) {
  instance->_Class = new_class;
  instance->initializeSlots(new_class->CLASS_stamp_for_instances(), new_size); // set the rack
#ifdef DEBUG_COUNT_ALLOCATIONS
  gctools::count_allocation(new_class->CLASS_stamp_for_instances());
#endif
  instance->_Sig = new_class->slots();
  return instance;
}

SYMBOL_EXPORT_SC_(ExtPkg,fieldsp);
SYMBOL_EXPORT_SC_(ExtPkg,fields);


bool Instance_O::fieldsp() const {
  if (ext::_sym_fieldsp->fboundp()) {
    T_sp result = eval::funcall(ext::_sym_fieldsp,this->asSmartPtr());
    return result.notnilp();
  }
  return false;
}

void Instance_O::fields(Record_sp node) {
  eval::funcall(ext::_sym_fields,this->asSmartPtr(),node);
}



size_t Instance_O::rack_stamp_offset() {
  return offsetof(Rack_O,_ShiftedStamp);
}

Fixnum Instance_O::stamp() const {
  return this->_Rack->stamp_get();
};

void Instance_O::stamp_set(gctools::ShiftedStamp s) {
  ASSERT(s==0||gctools::Header_s::Value::is_rack_shifted_stamp(s));
  this->_Rack->stamp_set(s);
};

size_t Instance_O::numberOfSlots() const {
  return this->_Rack->length();
};


T_sp Instance_O::instanceSigSet() {
  T_sp classSlots(_Nil<T_O>());
  Instance_sp mc = this->_instanceClass();
  classSlots = mc->slots();
  this->_Sig = classSlots;
  return ((classSlots));
}

T_sp Instance_O::instanceSig() const {
#if DEBUG_CLOS >= 2
  stringstream ssig;
  if (this->_Sig) {
    ssig << this->_Sig->__repr__();
  } else {
    ssig << "UNDEFINED ";
  }
  printf("\nMLOG INSTANCE-SIG of Instance %p \n", (void *)(this));
#endif
  return ((this->_Sig));
}

SYMBOL_EXPORT_SC_(CorePkg, instanceClassSet);

T_sp Instance_O::instanceClassSet(Instance_sp mc) {
  this->_Class = mc;
  return (this->sharedThis<Instance_O>());
#ifdef DEBUG_GUARD_VALIDATE
  client_validate(this->_Rack);
#endif
}

T_sp Instance_O::instanceRef(size_t idx) const {
#ifdef DEBUG_GUARD_VALIDATE
  client_validate(this->_Rack);
#endif
#if DEBUG_CLOS >= 2
  printf("\nMLOG INSTANCE-REF[%d] of Instance %p --->%s\n", idx, (void *)(this), this->_Rack[idx]->__repr__().c_str());
#endif
  T_sp val = low_level_instanceRef(this->_Rack,idx);
#if 0
  if (idx==5) {
    printf("%s:%d Read slot 5 with %s\n", __FILE__, __LINE__, _safe_rep_(val).c_str());
  }
#endif
  return val;

//  return ((*this->_Rack)[idx+RACK_SLOT_START]);
}
T_sp Instance_O::instanceSet(size_t idx, T_sp val) {
  if (idx==5) {
    if (val.fixnump() && val.unsafe_fixnum()>65536) {
      printf("%s:%d Setting slot 5 rack@%p with %s\n", __FILE__, __LINE__, this, _safe_rep_(val).c_str());
    }
  }
#if DEBUG_CLOS >= 2
  printf("\nMLOG SI-INSTANCE-SET[%d] of Instance %p to val: %s\n", idx, (void *)(this), val->__repr__().c_str());
#endif
  low_level_instanceSet(this->_Rack,idx,val);
  // (*this->_Rack)[idx+RACK_SLOT_START] = val;
#ifdef DEBUG_GUARD_VALIDATE
  client_validate(this->_Rack);
#endif
  return val;
}

string Instance_O::__repr__() const {
  stringstream ss;
  ss << "#S(";
  if (Instance_sp mc = this->_Class.asOrNull<Instance_O>()) {
    ss << mc->_classNameAsString() << " ";
  } else {
    ss << "<ADD SUPPORT FOR INSTANCE _CLASS=" << _rep_(this->_Class) << " >";
  }
  if (clos__classp(this->asSmartPtr())) {
    ss << _rep_(this->instanceRef(REF_CLASS_CLASS_NAME)) << " ";
  }
  {
    //    ss << " #slots[" << this->numberOfSlots() << "]";
#if 0
    for (size_t i(1); i < this->numberOfSlots(); ++i) {
      T_sp obj = this->_Rack[i];
      ss << "        :slot" << i << " ";
      if (obj) {
        stringstream sslot;
        if ((obj).consp()) {
          sslot << "CONS...";
          ss << sslot.str() << std::endl;
        } else if (Instance_sp inst = obj.asOrNull<Instance_O>()) {
          (void)inst; // turn off warning
          sslot << "INSTANCE...";
          ss << sslot.str() << std::endl;
        } else {
          sslot << _rep_(obj);
          if (sslot.str().size() > 80) {
            ss << sslot.str().substr(0, 80) << "...";
          } else {
            ss << sslot.str();
          }
          ss << " " << std::endl;
        }
      } else {
        ss << "UNDEFINED " << std::endl;
      }
    }
#endif
  }
  ss << ")" ;
  return ss.str();
}

T_sp Instance_O::copyInstance() const {
  Instance_sp cl = this->_Class;
  Instance_sp copy = gc::As_unsafe<Instance_sp>(cl->CLASS_get_creator()->creator_allocate());
  copy->_Class = cl;
  copy->_Rack = this->_Rack;
  copy->_Sig = this->_Sig;
  return copy;
}

SYMBOL_SC_(ClosPkg, standardOptimizedReaderMethod);
SYMBOL_SC_(ClosPkg, standardOptimizedWriterMethod);

// equalp method for structure-objects, which are instances too.
bool Instance_O::equalp(T_sp obj) const {
  if (!obj.generalp()) return false;
  if (this->_Class->_Class != _lisp->_Roots._TheStructureClass ) {
    return this == &*obj;
  }
  if (this == obj.unsafe_general()) return true;
  if (Instance_sp iobj = obj.asOrNull<Instance_O>()) {
    if (this->_Class != iobj->_Class) return false;
    if (this->stamp() != iobj->stamp()) return false;
    for (size_t i(0), iEnd(this->_Rack->length()); i < iEnd; ++i) {
      if (!cl__equalp((*this->_Rack)[i], (*iobj->_Rack)[i])) return false;
    }
    return true;
  }
  return false;
}

// also only for structure-objects.
void Instance_O::sxhash_equalp(HashGenerator &hg, LocationDependencyPtrT ld) const {
  if (this->_Class->_Class != _lisp->_Roots._TheStructureClass ) {
    HashTable_O::sxhash_eq(hg,this->asSmartPtr(),ld);
    return;
  }
  if (hg.isFilling()) HashTable_O::sxhash_equalp(hg, this->_Class->_className(), ld);
  for (size_t i(0), iEnd(this->numberOfSlots()); i < iEnd; ++i) {
    if (!this->instanceRef(i).unboundp() && hg.isFilling())
      HashTable_O::sxhash_equalp(hg, this->instanceRef(i), ld);
  }
}

  
void Instance_O::describe(T_sp stream) {
  stringstream ss;
  ss << (BF("Instance\n")).str();
  ss << (BF("_Class: %s\n") % _rep_(this->_Class).c_str()).str();
  for (int i(0); i < this->_Rack->length(); ++i) {
    ss << (BF("_Rack[%d]: %s\n") % i % _rep_((*this->_Rack)[i]).c_str()).str();
  }
  clasp_write_string(ss.str(), stream);
}


Instance_sp Instance_O::create(Symbol_sp symbol, Instance_sp metaClass, Creator_sp creator ) {
  DEPRECATED();
};

Instance_sp Instance_O::createClassUncollectable(gctools::ShiftedStamp stamp, Instance_sp metaClass, size_t number_of_slots, Creator_sp creator ) {
  ASSERT(gctools::Header_s::Value::is_shifted_stamp(stamp));
#if 0  
  printf("%s:%d:%s stamp -> %llu\n", __FILE__, __LINE__, __FUNCTION__, stamp);
  if (!metaClass.unboundp()) {
    printf("       metaClass->CLASS_stamp_for_instances() -> %llu\n", metaClass->CLASS_stamp_for_instances());
  } else {
    printf("       The metaClass was UNBOUND !!!!! I need a stamp for the class slots!!!!!!\n");
  }
#endif
  GC_ALLOCATE_UNCOLLECTABLE(Instance_O, oclass, metaClass /*, number_of_slots*/);
  oclass->_Class = metaClass;
  gctools::ShiftedStamp class_stamp = 0;
  if (!metaClass.unboundp()) {
    class_stamp = metaClass->CLASS_stamp_for_instances();
    ASSERT(gctools::Header_s::Value::is_shifted_stamp(class_stamp));
  }
  // class_stamp may be 0 if metaClass.unboundp();
  oclass->initializeSlots(class_stamp,number_of_slots);
  oclass->initializeClassSlots(creator,stamp);
  return oclass;
};

void Instance_O::CLASS_set_creator(Creator_sp cb) {
#ifdef DEBUG_CLASS_INSTANCE
  printf("%s:%d    setCreator for %s @%p -> @%p\n", __FILE__, __LINE__, _rep_(this->name()).c_str(), this, cb.raw_());
#endif
  this->instanceSet(REF_CLASS_CREATOR,cb);
}

void Instance_O::accumulateSuperClasses(HashTableEq_sp supers, ComplexVector_T_sp arrayedSupers, Instance_sp mc) {
  if (IS_SYMBOL_UNDEFINED(mc->_className()))
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
    Instance_sp oneClass = gc::As<Instance_sp>(one);
    accumulateSuperClasses(supers, arrayedSupers, oneClass);
  }
}

void Instance_O::lowLevel_calculateClassPrecedenceList() {
  using namespace boost;
  HashTableEq_sp supers = HashTableEq_O::create_default();
  ComplexVector_T_sp arrayedSupers(ComplexVector_T_O::make(16, _Nil<T_O>(), clasp_make_fixnum(0)));
  this->accumulateSuperClasses(supers, arrayedSupers, this->sharedThis<Instance_O>());
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
      Instance_sp mc = gc::As<Instance_sp>(key);
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
#if 0
#ifdef DEBUG_ON
  {
    for (size_t zi(0), ziEnd(cl__length(arrayedSupers)); zi < ziEnd; ++zi) {
      stringstream ss;
      ss << (BF("graph[%d/name=%s] = ") % zi % arrayedSupers->operator[](zi).as<Instance_O>()->instanceClassName()).str();
      for (list<int>::const_iterator it = graph[zi].begin(); it != graph[zi].end(); it++) {
        ss << *it << "-> ";
      }
      ss << ";";
      LOG(BF("%s") % ss.str());
    }
  }
#endif
#endif
  deque<int> topo_order;
  topological_sort(graph, front_inserter(topo_order), vertex_index_map(identity_property_map()));
#if 0
#ifdef DEBUG_ON
  {
    stringstream ss;
    ss << "Topologically sorted superclasses ";
    for (deque<int>::const_reverse_iterator it = topo_order.rbegin(); it != topo_order.rend(); it++) {
      Instance_sp mc = arrayedSupers->operator[](*it).as<Instance_O>();
      ss << "-> " << mc->className() << "/" << mc->instanceClassName();
    }
    LOG(BF("%s") % ss.str());
  }
#endif
#endif
  List_sp cpl = _Nil<T_O>();
  for (deque<int>::const_reverse_iterator it = topo_order.rbegin(); it != topo_order.rend(); it++) {
    Instance_sp mc = gc::As<Instance_sp>(arrayedSupers->operator[](*it));
    LOG(BF("pushing superclass[%s] to front of ClassPrecedenceList") % mc->instanceClassName());
    cpl = Cons_O::create(mc, cpl);
  }
  this->instanceSet(REF_CLASS_CLASS_PRECEDENCE_LIST, cpl);
}

void Instance_O::addInstanceBaseClass(Symbol_sp className) {
  this->addInstanceBaseClassDoNotCalculateClassPrecedenceList(className);
  this->lowLevel_calculateClassPrecedenceList();
}

void Instance_O::setInstanceBaseClasses(List_sp classes) {
  this->instanceSet(REF_CLASS_DIRECT_SUPERCLASSES, cl__copy_list(classes));
  this->lowLevel_calculateClassPrecedenceList();
}


bool Instance_O::isSubClassOf(Instance_sp ancestor) const {
#if 0
  printf("%s:%d   Checking if this[%s] isSubClassOf[%s]\n", __FILE__, __LINE__, _rep_(this->asSmartPtr()).c_str(), _rep_(ancestor).c_str());
  Instance_sp find_theClass = cl__find_class(cl::_sym_class,true,_Nil<T_O>());
  if (_lisp->_Roots._TheClass != find_theClass) {
    printf("%s:%d   Instance_O::isSubClassOf  find_theClass(%p) and _lisp->_Root._TheClass(%p) don't match anymore\n", __FILE__, __LINE__, find_theClass.raw_(), _lisp->_Roots._TheClass.raw_() );
  }
#endif
  Instance_sp this_class = ENSURE_VALID_OBJECT(this->_Class);
  if (this_class==_lisp->_Roots._TheClass
      || this_class==_lisp->_Roots._TheBuiltInClass
      || this_class==_lisp->_Roots._TheStandardClass
      || this_class==_lisp->_Roots._TheStructureClass
      || this_class->isSubClassOf(_lisp->_Roots._TheClass)) {
    if (this == &*ancestor) return true;
  // TODO: I need to memoize this somehow so that I'm not constantly searching a list in
  // linear time
    List_sp cpl = this->instanceRef(Instance_O::REF_CLASS_CLASS_PRECEDENCE_LIST);
    ASSERTF(!cpl.unboundp(), BF("You tried to use isSubClassOf when the ClassPrecedenceList had not been initialized"));
    for (auto cur : cpl) {
      if (CONS_CAR(cur) == ancestor) return true;
    }
    return false;
  }
  printf("%s:%d FAILED   this_class->isSubClassOf(_lisp->_Roots._TheClass) ->%d\n", __FILE__, __LINE__, this_class->isSubClassOf(_lisp->_Roots._TheClass));
  {
    printf("%s:%d   this->className() -> %s  checking if subclass of %s\n", __FILE__, __LINE__, _rep_(this->_className()).c_str(), _rep_(ancestor->_className()).c_str());
    List_sp cpl = this->instanceRef(Instance_O::REF_CLASS_CLASS_PRECEDENCE_LIST);
    for ( auto cur: cpl ) {
      printf("%s:%d  cpl -> %s\n", __FILE__, __LINE__, _rep_(gc::As<Instance_sp>(CONS_CAR(cur))->_className()).c_str());
    }
  }
  {
    printf("%s:%d   this->_Class->className() -> %s  its cpl ->>>\n", __FILE__, __LINE__, _rep_(this->_Class->_className()).c_str());
    List_sp cpl = this_class->instanceRef(Instance_O::REF_CLASS_CLASS_PRECEDENCE_LIST);
    for ( auto cur: cpl ) {
      Instance_sp cc = gc::As<Instance_sp>(CONS_CAR(cur));
      printf("%s:%d  cpl -> %s == _lisp->_Roots._TheClass -> %d\n", __FILE__, __LINE__, _rep_(cc->_className()).c_str(), cc == _lisp->_Roots._TheClass);
    }
  }
  TYPE_ERROR(this->asSmartPtr(),cl::_sym_class);
}


void Instance_O::addInstanceBaseClassDoNotCalculateClassPrecedenceList(Symbol_sp className) {
  Instance_sp cl;
  if (!(cl::_sym_findClass) || !cl::_sym_findClass->fboundp()) {
    cl = gc::As<Instance_sp>(cl__find_class(className));
  } else {
    cl = gc::As<Instance_sp>(eval::funcall(cl::_sym_findClass, className, _lisp->_true()));
  }
  // When booting _DirectSuperClasses may be undefined
  List_sp dsc = this->directSuperclasses();
  this->instanceSet(REF_CLASS_DIRECT_SUPERCLASSES, Cons_O::create(cl, dsc));
}



void Instance_O::__setupStage3NameAndCalculateClassPrecedenceList(Symbol_sp className) {
  this->_setClassName(className);
  // Initialize some of the class slots
  this->instanceSet(REF_CLASS_DIRECT_SLOTS,_Nil<T_O>());
  this->instanceSet(REF_CLASS_DEFAULT_INITARGS,_Nil<T_O>());
  T_sp tmc = this->_instanceClass();
  ASSERTNOTNULL(tmc);
  Instance_sp mc = gc::As<Instance_sp>(tmc);
  (void)mc;
  this->lowLevel_calculateClassPrecedenceList();
}


string Instance_O::dumpInfo() {
  stringstream ss;
  ss << (boost::format("this.instanceClassName: %s @ %X") % this->instanceClassName() % this) << std::endl;
  ss << "_FullName[" << this->_className()->fullName() << "]" << std::endl;
  ss << boost::format("    _Class = %X  this._Class.instanceClassName()=%s\n") % this->__class().get() % this->__class()->instanceClassName();
  for (auto cc : this->directSuperclasses()) {
    ss << "Base class: " << gc::As<Instance_sp>(oCar(cc))->instanceClassName() << std::endl;
  }
  ss << boost::format("this.instanceCreator* = %p") % (void *)(&*this->CLASS_get_creator()) << std::endl;
  return ss.str();
}

string Instance_O::getPackagedName() const {
  return this->_className()->formattedName(false);
}

string Instance_O::_classNameAsString() const {
  return _rep_(this->_className());
}


T_sp Instance_O::make_instance() {
  T_sp instance = this->CLASS_get_creator()->creator_allocate(); //this->allocate_newNil();
  if (instance.generalp()) {
    instance.unsafe_general()->initialize();
  } else {
    SIMPLE_ERROR(BF("Add support to make_instance of non general objects"));
  }
  return instance;
}

List_sp Instance_O::directSuperclasses() const {
  T_sp obj = this->instanceRef(REF_CLASS_DIRECT_SUPERCLASSES);
  ASSERT(obj);
  ASSERT(!obj.unboundp());
  if (obj.unboundp()) {
    printf("%s:%d  The REF_CLASS_DIRECT_SUPERCLASSES[%u] field is unbound\n", __FILE__, __LINE__, REF_CLASS_DIRECT_SUPERCLASSES);
  }
  return coerce_to_list(obj);
}
};


/* The following functions should only work for Classes */
namespace core {
CL_DEFUN List_sp clos__direct_superclasses(Instance_sp c) {
  if (c->_Class->isSubClassOf(_lisp->_Roots._TheClass)) {
    return c->directSuperclasses();
  }
  TYPE_ERROR(c,cl::_sym_class);
}

// FIXME: Perhaps gctools::NextStamp could be exported and used as the stamp slot's initform.
CL_LAMBDA(class_ &optional (name nil name-p));
CL_DEFUN void core__class_new_stamp(Instance_sp c, T_sp name, T_sp namep) {
//  printf("%s:%d Something is whacked here - I'm calling NextStamp twice for class %s!!!!\n", __FILE__, __LINE__, _safe_rep_(name).c_str() );
  gctools::ShiftedStamp stamp = gctools::NextShiftedStampMergeWhere(gctools::Header_s::rack_wtag);
  c->CLASS_set_stamp_for_instances(stamp); // Was gctools::NextStamp());
  std::string sname;
  if (namep.notnilp()) {
    if (gc::IsA<Symbol_sp>(name)) {
      sname = gc::As_unsafe<Symbol_sp>(name)->formattedName(true);
    } else {
      printf("%s:%d Name %s must be a symbol\n", __FILE__, __LINE__, _rep_(name).c_str());
      sname = "UNKNOWN";
    }
  } else {
    sname = c->_className()->formattedName(true);
  }
//  printf("%s:%d Registering %s with stamp %lld\n", __FILE__, __LINE__, sname.c_str(), stamp);
  register_stamp_name(sname,gctools::Header_s::Value::unshift_shifted_stamp(stamp));
}

CL_DEFUN T_sp core__class_stamp_for_instances(Instance_sp c) {
  if (c->_Class->isSubClassOf(_lisp->_Roots._TheClass)) {
    // DONT convert this to an integer - it is already a shifted stamp and
    // it can be treated like a fixnum
    ASSERT(gctools::Header_s::Value::is_shifted_stamp(c->CLASS_stamp_for_instances()));
    T_sp stamp((gctools::Tagged)c->CLASS_stamp_for_instances());
    return stamp;
  }
  TYPE_ERROR(c,cl::_sym_class);
};

CL_DEFUN T_sp core__name_of_class(Instance_sp c) {
  if (c->_Class->isSubClassOf(_lisp->_Roots._TheClass)) {
    return c->_className();
  }
  TYPE_ERROR(c,cl::_sym_class);
};

CL_DEFUN T_sp core__class_creator(Instance_sp c) {
  if (c->_Class->isSubClassOf(_lisp->_Roots._TheClass)) {
    return c->CLASS_get_creator();
  }
  TYPE_ERROR(c,cl::_sym_class);
};

CL_DEFUN bool core__has_creator(Instance_sp c) {
  if (c->_Class->isSubClassOf(_lisp->_Roots._TheClass)) {
    return c->CLASS_has_creator();
  }
  TYPE_ERROR(c,cl::_sym_class);
};





};


namespace core {
ClassReadLock::ClassReadLock(mp::SharedMutex_sp lock) : _Lock(lock) {
  this->_Lock->shared_lock();
}
ClassReadLock::~ClassReadLock() {
  this->_Lock->shared_unlock();
}

ClassWriteLock::ClassWriteLock(mp::SharedMutex_sp lock) : _Lock(lock) {
  this->_Lock->write_lock();
}
ClassWriteLock::~ClassWriteLock() {
  this->_Lock->write_unlock();
}


CL_DEFMETHOD Instance_sp ClassHolder_O::class_get() const { return this->_Class.load(); };
CL_DEFMETHOD void ClassHolder_O::class_set(Instance_sp cl) { this->_Class.store(cl); };
void ClassHolder_O::class_mkunbound() { this->_Class.store(_Unbound<Instance_O>()); };
bool ClassHolder_O::class_unboundp() const { return this->_Class.load().unboundp();};

CL_DEFUN Instance_sp ext__class_get(ClassHolder_sp holder) {
  return holder->class_get();
}

CL_DEFUN bool ext__class_unboundp(ClassHolder_sp holder) {
  return holder->class_unboundp();
}


CL_DEFUN void core__verify_instance_layout(size_t instance_size, size_t instance_rack_offset)
{
  if (instance_size!=sizeof(Instance_O)) SIMPLE_ERROR(BF("The cmpintrinsics.lsp instance_size %lu does not match sizeof(Instance_O)") % instance_size % sizeof(Instance_O));
  if (instance_rack_offset!=offsetof(Instance_O,_Rack))
    SIMPLE_ERROR(BF("instance_rack_offset %lu does not match offsetof(_Rack,Instance_O) %lu") % instance_rack_offset % offsetof(Instance_O,_Rack));
}

CL_DEFUN void core__verify_rack_layout(size_t stamp_offset, size_t data_offset)
{
  size_t cxx_stamp_offset = offsetof(Rack_O,_ShiftedStamp);
  size_t cxx_data_offset = offsetof(Rack_O,_Slots._Data);
  if (stamp_offset!=cxx_stamp_offset)
    SIMPLE_ERROR(BF("stamp_offset %lu does not match cxx_stamp_offset %lu") % stamp_offset % cxx_stamp_offset);
  if (data_offset!=cxx_data_offset)
    SIMPLE_ERROR(BF("data_offset %lu does not match cxx_data_offset %lu") % data_offset % cxx_data_offset);
}




};


