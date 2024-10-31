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
// #define DEBUG_LEVEL_FULL

#pragma clang diagnostic push
#pragma GCC diagnostic ignored "-Warray-bounds"
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunneeded-internal-declaration"
// #pragma GCC diagnostic ignored "-Wunused-local-typedef"
#include <boost/graph/vector_as_graph.hpp>
#include <boost/graph/topological_sort.hpp>
#pragma clang diagnostic pop

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/symbolTable.h>
#include <clasp/gctools/gcFunctions.h>
#include <clasp/core/lispList.h>
#include <clasp/core/mpPackage.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/instance.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/funcallableInstance.h>
#include <clasp/core/wrappers.h>

namespace core {

Rack_sp Rack_O::make(size_t numSlots, T_sp sig, T_sp initialValue) {
  auto bs = gctools::GC<Rack_O>::allocate_container<gctools::RuntimeStage>(false, numSlots, sig, initialValue, true);
  return bs;
}

DOCGROUP(clasp);
CL_DEFUN T_sp core__make_rack(size_t numSlots, T_sp sig, T_sp stamp, T_sp initialValue) {
  Rack_sp r = Rack_O::make(numSlots, sig, initialValue);
  r->stamp_set((gctools::ShiftedStamp)(stamp.raw_()));
  return r;
}

DOCGROUP(clasp);
CL_DEFUN T_sp core__stamp_of_rack(Rack_sp rack) {
  core::T_sp stamp((gctools::Tagged)(rack->_ShiftedStamp));
  return stamp;
}

DOCGROUP(clasp);
CL_DEFUN T_sp core__rack_sig(Rack_sp rack) { return rack->_Sig; }

DOCGROUP(clasp);
CL_DEFUN size_t core__rack_size(Rack_sp rack) { return rack->length(); }

DOCGROUP(clasp);
CL_DEFUN T_sp core__rack_ref(Rack_sp rack, size_t i) { return rack->low_level_rackRef(i); }

CL_NAME("RACK-REF");
DOCGROUP(clasp);
CL_DEFUN_SETF T_sp core__rack_set(T_sp val, Rack_sp rack, size_t i) {
  rack->low_level_rackSet(i, val);
  return val;
}

DOCGROUP(clasp);
CL_LAMBDA(order rack index);
CL_DEFUN T_sp core__atomic_rack_read(T_sp order, Rack_sp rack, size_t i) {
  T_sp val = rack->low_level_rackRef(i);
  return val;
}

DOCGROUP(clasp);
CL_LAMBDA(order value rack index);
CL_DEFUN void core__atomic_rack_write(T_sp order, T_sp val, Rack_sp rack, size_t i) { rack->low_level_rackSet(i, val); }

SYMBOL_EXPORT_SC_(KeywordPkg, SequentiallyConsistent);

DOCGROUP(clasp);
CL_LAMBDA(order old nv rack index);
CL_DEFUN T_sp core__cas_rack(T_sp order, T_sp old, T_sp newval, Rack_sp rack, size_t index) {
  if (order != kw::_sym_SequentiallyConsistent) {
    SIMPLE_ERROR("Add support for order {}", _rep_(order).c_str());
  }
  [[maybe_unused]] bool result = rack->low_level_rack_compare_exchange_strong(index, old, newval);
#if 0
  printf("%s:%d:%s order = %s old = %p newval = %p rack = %p index = %zu result = %d\n",
         __FILE__, __LINE__, __FUNCTION__, _rep_(order).c_str(), old.raw_(), newval.raw_(), rack.raw_(), index, result );
#endif
  return old;
}

CL_NAME("INSTANCE-CLASS");
CL_LAMBDA(class instance);
CL_DECLARE();
CL_DOCSTRING(R"dx(instanceClassSet)dx");
DOCGROUP(clasp);
CL_DEFUN_SETF T_sp core__instance_class_set(Instance_sp mc, T_sp obj) {
  if (Instance_sp iobj = obj.asOrNull<Instance_O>()) {
    return iobj->instanceClassSet(mc);
  }
  SIMPLE_ERROR("You can only instanceClassSet on Instance_O or Instance_O - you tried to set it on a: {}", _rep_(mc));
};

void Instance_O::initializeSlots(gctools::ShiftedStamp stamp, T_sp sig, size_t numberOfSlots) {
  ASSERT(stamp == 0 || gctools::Header_s::StampWtagMtag::is_rack_shifted_stamp(stamp));
  this->_Rack = Rack_O::make(numberOfSlots, sig, unbound<T_O>());
  this->stamp_set(stamp);
#ifdef DEBUG_GUARD_VALIDATE
  client_validate(rack());
#endif
  //  printf("%s:%d  Make sure you initialize slots for classes this->_Class -> %s\n", __FILE__, __LINE__,
  //  _rep_(this->_Class).c_str());
}

void Instance_O::CLASS_set_stamp_for_instances(gctools::BaseHeader_s::StampWtagMtag s) {
  T_sp stamp((gctools::Tagged)s.as_fixnum());
  this->instanceSet(REF_CLASS_STAMP_FOR_INSTANCES_, stamp); // write shifted stamp - it's automatically a fixnum
};

// NOT called by regular CL allocate instance. FIXME, find a way to remove this if possible.
void Instance_O::initializeClassSlots(Creator_sp creator, gctools::BaseHeader_s::StampWtagMtag stamp) {
  // Should match clos/hierarchy.lisp
  //  ASSERT(gctools::Header_s::StampWtagMtag::is_shifted_stamp(stamp));
  SimpleBaseString_sp sbsr = SimpleBaseString_O::make("CALHISR");
  SimpleBaseString_sp sbsw = SimpleBaseString_O::make("CALHISW");
  this->instanceSet(REF_SPECIALIZER_MUTEX, mp::SharedMutex_O::make_shared_mutex(sbsr, sbsw));
  this->instanceSet(REF_SPECIALIZER_CALL_HISTORY_GENERIC_FUNCTIONS, nil<T_O>());
  this->instanceSet(REF_CLASS_DIRECT_SUBCLASSES, nil<T_O>());
  this->instanceSet(REF_CLASS_DIRECT_SUPERCLASSES, nil<T_O>());
  this->instanceSet(REF_CLASS_DIRECT_DEFAULT_INITARGS, nil<T_O>());
  this->instanceSet(REF_CLASS_FINALIZED, nil<T_O>());
  this->instanceSet(REF_CLASS_DEPENDENTS, nil<T_O>());
  this->instanceSet(REF_CLASS_LOCATION_TABLE, nil<T_O>());
  this->CLASS_set_stamp_for_instances(stamp);
  this->instanceSet(REF_CLASS_CREATOR, creator);
}

DOCGROUP(clasp);
CL_DEFUN List_sp core__class_slot_sanity_check() {
  List_sp sanity = nil<T_O>();
  sanity = Cons_O::create(Cons_O::create(clos::_sym_NUMBER_OF_SLOTS_IN_STANDARD_CLASS,
                                         core::clasp_make_fixnum(REF_CLASS_NUMBER_OF_SLOTS_IN_STANDARD_CLASS)),
                          sanity);
  sanity = Cons_O::create(Cons_O::create(clos::_sym_NUMBER_OF_SLOTS_IN_STRUCTURE_CLASS,
                                         core::clasp_make_fixnum(REF_CLASS_NUMBER_OF_SLOTS_IN_STRUCTURE_CLASS)),
                          sanity);
#define ADD_SANITY_CHECK_SIMPLE(slot_name, enum_name)                                                                              \
  sanity = Cons_O::create(Cons_O::create(clos::_sym_##slot_name, core::clasp_make_fixnum(Instance_O::REF_##enum_name)), sanity);
  ADD_SANITY_CHECK_SIMPLE(NAME, CLASS_CLASS_NAME);
  ADD_SANITY_CHECK_SIMPLE(DIRECT_SUPERCLASSES, CLASS_DIRECT_SUPERCLASSES);
  ADD_SANITY_CHECK_SIMPLE(SLOTS, CLASS_SLOTS);
  ADD_SANITY_CHECK_SIMPLE(DIRECT_DEFAULT_INITARGS, CLASS_DIRECT_DEFAULT_INITARGS);
  ADD_SANITY_CHECK_SIMPLE(FINALIZED, CLASS_FINALIZED);
  ADD_SANITY_CHECK_SIMPLE(PRECEDENCE_LIST, CLASS_CLASS_PRECEDENCE_LIST);
  ADD_SANITY_CHECK_SIMPLE(DIRECT_SLOTS, CLASS_DIRECT_SLOTS);
  ADD_SANITY_CHECK_SIMPLE(DEFAULT_INITARGS, CLASS_DEFAULT_INITARGS);
  ADD_SANITY_CHECK_SIMPLE(DIRECT_SUBCLASSES, CLASS_DIRECT_SUBCLASSES);
  ADD_SANITY_CHECK_SIMPLE(DEPENDENTS, CLASS_DEPENDENTS);
  ADD_SANITY_CHECK_SIMPLE(LOCATION_TABLE, CLASS_LOCATION_TABLE);
  ADD_SANITY_CHECK_SIMPLE(CALL_HISTORY_GENERIC_FUNCTIONS, SPECIALIZER_CALL_HISTORY_GENERIC_FUNCTIONS);
  ADD_SANITY_CHECK_SIMPLE(SPECIALIZER_MUTEX, SPECIALIZER_MUTEX);
  return sanity;
}

// FIXME: Exists solely for cases where the list of slotds is hard to get.
CL_LAMBDA(class slot-count);
DOCGROUP(clasp);
CL_DEFUN T_sp core__allocate_standard_instance(Instance_sp cl, size_t slot_count) {
  auto obj = gctools::GC<Instance_O>::allocate(cl);
  obj->initializeSlots(cl->CLASS_stamp_for_instances(), cl->slots(), slot_count);
  return obj;
}

CL_LAMBDA(class rack);
DOCGROUP(clasp);
CL_DEFUN Instance_sp core__allocate_raw_instance(Instance_sp cl, Rack_sp rack) {
  auto obj = gctools::GC<Instance_O>::allocate(cl, rack);
  return obj;
}

CL_LAMBDA(class rack);
DOCGROUP(clasp);
CL_DEFUN Instance_sp core__allocate_raw_general_instance(Instance_sp cl, Rack_sp rack) {
  // This function allocates using the creator.
  ASSERT(cl->CLASS_has_creator());
  Creator_sp creator = gctools::As<Creator_sp>(cl->CLASS_get_creator());
  Instance_sp obj = gc::As_unsafe<Instance_sp>(creator->creator_allocate());
  obj->_Class = cl;
  obj->_Rack = rack;
  return obj;
}

SYMBOL_EXPORT_SC_(CorePkg, fieldsp);

bool Instance_O::fieldsp() const {
  if (core::_sym_fieldsp->fboundp()) {
    T_sp result = eval::funcall(core::_sym_fieldsp, this->asSmartPtr());
    return result.notnilp();
  }
  return false;
}

// funcalling ext::fields can't work, since ext::fields is unbound
// so at least let protect it
// and export it from core to be consistent
SYMBOL_EXPORT_SC_(CorePkg, fields);
void Instance_O::fields(Record_sp node) {
  if (core::_sym_fields->fboundp()) {
    eval::funcall(core::_sym_fields, this->asSmartPtr(), node);
  }
}

DOCGROUP(clasp);
CL_DEFUN Rack_sp core__instance_rack(T_sp instance) {
  if (gc::IsA<Instance_sp>(instance)) {
    return gc::As_unsafe<Instance_sp>(instance)->rack();
  } else if (gc::IsA<FuncallableInstance_sp>(instance)) {
    return gc::As_unsafe<FuncallableInstance_sp>(instance)->rack();
  }
  TYPE_ERROR(instance, Cons_O::createList(core::_sym_Instance_O, core::_sym_FuncallableInstance_O));
}

CL_NAME("INSTANCE-RACK");
DOCGROUP(clasp);
CL_DEFUN_SETF void core__instance_rack_set(Rack_sp rack, T_sp instance) {
  if (gc::IsA<Instance_sp>(instance)) {
    gc::As_unsafe<Instance_sp>(instance)->_Rack = rack;
    return;
  } else if (gc::IsA<FuncallableInstance_sp>(instance)) {
    gc::As_unsafe<FuncallableInstance_sp>(instance)->_Rack = rack;
    return;
  }
  TYPE_ERROR(instance, Cons_O::createList(core::_sym_Instance_O, core::_sym_FuncallableInstance_O));
}

DOCGROUP(clasp);
CL_LAMBDA(old nv instance index);
CL_DEFUN T_sp core__instance_cas(T_sp old, T_sp newval, T_sp instance, size_t index) {
  Rack_sp rack = core__instance_rack(instance);
  return core__cas_rack(kw::_sym_SequentiallyConsistent, old, newval, rack, index);
}

size_t Instance_O::rack_stamp_offset() { return offsetof(Rack_O, _ShiftedStamp); }

Fixnum Instance_O::stamp() const { return rack()->stamp_get(); };

void Instance_O::stamp_set(gctools::ShiftedStamp s) {
  ASSERT(s == 0 || gctools::Header_s::StampWtagMtag::is_rack_shifted_stamp(s));
  rack()->stamp_set(s);
};

size_t Instance_O::numberOfSlots() const { return rack()->length(); };

T_sp Instance_O::instanceSigSet() {
  Instance_sp mc = this->_instanceClass();
  T_sp classSlots = mc->slots();
  rack()->_Sig = classSlots;
  return ((classSlots));
}

T_sp Instance_O::instanceSig() const {
#if DEBUG_CLOS >= 2
  stringstream ssig;
  if (rack()->_Sig) {
    ssig << rack()->_Sig->__repr__();
  } else {
    ssig << "UNDEFINED ";
  }
  printf("\nMLOG INSTANCE-SIG of Instance %p \n", (void*)(this));
#endif
  return ((rack()->_Sig));
}

SYMBOL_EXPORT_SC_(CorePkg, instanceClassSet);

T_sp Instance_O::instanceClassSet(Instance_sp mc) {
  this->_Class = mc;
  return (this->sharedThis<Instance_O>());
#ifdef DEBUG_GUARD_VALIDATE
  client_validate(rack());
#endif
}

T_sp Instance_O::instanceRef(size_t idx) const {
#ifdef DEBUG_GUARD_VALIDATE
  client_validate(rack());
#endif
#if DEBUG_CLOS >= 2
  printf("\nMLOG INSTANCE-REF[%d] of Instance %p --->%s\n", idx, (void*)(this), rack()[idx]->__repr__().c_str());
#endif
  T_sp val = low_level_instanceRef(rack(), idx);
#if 0
  if (idx==5) {
    printf("%s:%d Read slot 5 with %s\n", __FILE__, __LINE__, _safe_rep_(val).c_str());
  }
#endif
  return val;
}
T_sp Instance_O::instanceSet(size_t idx, T_sp val) {
#if DEBUG_CLOS >= 2
  printf("\nMLOG SI-INSTANCE-SET[%d] of Instance %p to val: %s\n", idx, (void*)(this), val->__repr__().c_str());
#endif
  low_level_instanceSet(rack(), idx, val);
#ifdef DEBUG_GUARD_VALIDATE
  client_validate(rack());
#endif
  return val;
}

string Instance_O::__repr__() const {
  // This should match print-unreadable-object.
  // The only reason we don't just fall back to General_O::__repr__
  // is that it doesn't display the pointer.
  stringstream ss;
  ss << "#<";
  if (Instance_sp mc = this->_Class.asOrNull<Instance_O>()) {
    ss << mc->_classNameAsString();
    ss << " ";
    ss << this->_classNameAsString();
  } else {
    ss << "<ADD SUPPORT FOR INSTANCE _CLASS=" << _rep_(this->_Class);
  }
  ss << " @" << this << ">";
  return ss.str();
}

SYMBOL_SC_(ClosPkg, standardOptimizedReaderMethod);
SYMBOL_SC_(ClosPkg, standardOptimizedWriterMethod);

// equalp method for structure-objects, which are instances too.
bool Instance_O::equalp(T_sp obj) const {
  if (!obj.generalp())
    return false;
  if (this->_Class->_Class != _lisp->_Roots._TheStructureClass) {
    return this == &*obj;
  }
  if (this == obj.unsafe_general())
    return true;
  if (Instance_sp iobj = obj.asOrNull<Instance_O>()) {
    if (this->_Class != iobj->_Class)
      return false;
    if (this->stamp() != iobj->stamp())
      return false;
    for (size_t i(0), iEnd(rack()->length()); i < iEnd; ++i) {
      if (!cl__equalp(low_level_instanceRef(rack(), i), low_level_instanceRef(iobj->rack(), i)))
        return false;
    }
    return true;
  }
  return false;
}

// also only for structure-objects.
void Instance_O::sxhash_equalp(HashGenerator& hg) const {
  if (this->_Class->_Class != _lisp->_Roots._TheStructureClass) {
    HashTable_O::sxhash_eq(hg, this->asSmartPtr());
    return;
  }
  if (hg.isFilling())
    HashTable_O::sxhash_equalp(hg, this->_Class->_className());
  for (size_t i(0), iEnd(this->numberOfSlots()); i < iEnd; ++i) {
    if (!this->instanceRef(i).unboundp() && hg.isFilling())
      HashTable_O::sxhash_equalp(hg, this->instanceRef(i));
  }
}

void Instance_O::describe(T_sp stream) {
  stringstream ss;
  ss << fmt::format("Instance\n");
  ss << fmt::format("_Class: {}\n", _rep_(this->_Class));
  for (int i(0); i < rack()->length(); ++i) {
    ss << fmt::format("_Rack[{}]: {}\n", i, _rep_(low_level_instanceRef(rack(), i)));
  }
  clasp_write_string(ss.str(), stream);
}

Instance_sp Instance_O::create(Symbol_sp symbol, Instance_sp metaClass, Creator_sp creator) { DEPRECATED(); };

Instance_sp Instance_O::createClassUncollectable(gctools::BaseHeader_s::StampWtagMtag stamp, Instance_sp metaClass,
                                                 size_t number_of_slots, Creator_sp creator) {
#if 0  
  printf("%s:%d:%s stamp -> %zu\n", __FILE__, __LINE__, __FUNCTION__, stamp);
  if (!metaClass.unboundp()) {
    printf("       metaClass->CLASS_stamp_for_instances() -> %zu\n", metaClass->CLASS_stamp_for_instances());
  } else {
    printf("       The metaClass was UNBOUND !!!!! I need a stamp for the class slots!!!!!!\n");
  }
#endif
  auto oclass = gctools::GC<Instance_O>::allocate(metaClass /*, number_of_slots*/);
  oclass->_Class = metaClass;
  gctools::ShiftedStamp class_stamp = 0;
  T_sp sig = unbound<T_O>();
  if (!metaClass.unboundp()) {
    class_stamp = metaClass->CLASS_stamp_for_instances();
    sig = metaClass->slots();
    ASSERT(gctools::Header_s::StampWtagMtag::is_shifted_stamp(class_stamp));
  }
  // class_stamp may be 0 if metaClass.unboundp();
  oclass->initializeSlots(class_stamp, sig, number_of_slots);
  oclass->initializeClassSlots(creator, stamp);
  return oclass;
};

void Instance_O::CLASS_set_creator(Creator_sp cb) {
#ifdef DEBUG_CLASS_INSTANCE
  printf("%s:%d    setCreator for %s @%p -> @%p\n", __FILE__, __LINE__, _rep_(this->name()).c_str(), this, cb.raw_());
#endif
  this->instanceSet(REF_CLASS_CREATOR, cb);
}

void Instance_O::accumulateSuperClasses(HashTableEq_sp supers, ComplexVector_T_sp arrayedSupers, Instance_sp mc) {
  if (IS_SYMBOL_UNDEFINED(mc->_className()))
    return;
  //	printf("%s:%d accumulateSuperClasses of: %s\n", __FILE__, __LINE__, _rep_(mc->className()).c_str() );
  if (supers->contains(mc))
    return;
  Fixnum arraySuperLength = arrayedSupers->length();
  //  printf("%s:%d arraySuperLength = %ld\n", __FILE__, __LINE__, arraySuperLength);
  T_sp index = clasp_make_fixnum(arraySuperLength);
  supers->setf_gethash(mc, index);
  //  printf("%s:%d dumping supers hash-table\n", __FILE__, __LINE__ );
  // supers->hash_table_early_dump();
  //  printf("%s:%d associating %s with %s\n", __FILE__, __LINE__, _rep_(mc).c_str(), _rep_(index).c_str());
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
  ComplexVector_T_sp arrayedSupers(ComplexVector_T_O::make(16, nil<T_O>(), clasp_make_fixnum(0)));
  if (!gc::IsA<ComplexVector_T_sp>(arrayedSupers)) {
    printf("%s:%d:%s The object must be a ComplexVector_T_sp but failed gc::IsA<ComplexVector_T_sp>()\n", __FILE__, __LINE__,
           __FUNCTION__);
    abort();
  }
  this->accumulateSuperClasses(supers, arrayedSupers, this->sharedThis<Instance_O>());
  vector<list<int>> graph(cl__length(arrayedSupers));

  class TopoSortSetup : public KeyValueMapper {
  private:
    HashTable_sp supers;
    vector<list<int>>* graphP;

  public:
    TopoSortSetup(HashTable_sp asupers, vector<list<int>>* gP) : supers(asupers), graphP(gP){};
    virtual bool mapKeyValue(T_sp key, T_sp value) {
      Fixnum_sp fnValue(gc::As<Fixnum_sp>(value));
      int mcIndex = unbox_fixnum(fnValue);
      Instance_sp mc = gc::As<Instance_sp>(key);
      //      printf("%s:%d Superclasses -> %s\n", __FILE__, __LINE__, _rep_(mc->directSuperclasses()).c_str());
      for (auto mit : (List_sp)(mc->directSuperclasses())) {
        T_sp key = oCar(mit);
        T_sp val = this->supers->gethash(key);
        //        printf("%s:%d lookup key@%p -> %s  val -> %s\n", __FILE__, __LINE__, key.raw_(), _rep_(key).c_str(),
        //        _rep_(val).c_str());
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
  {
    printf("%s:%d About to do topological_sort\n", __FILE__, __LINE__);
    for (size_t zi(0), ziEnd(cl__length(arrayedSupers)); zi < ziEnd; ++zi) {
      stringstream ss;
      ss << fmt::format("graph[{}/name={}] = ", zi , arrayedSupers[zi].as<Instance_O>()->instanceClassName());
      for (list<int>::const_iterator it = graph[zi].begin(); it != graph[zi].end(); it++) {
        ss << *it << "-> ";
      }
      ss << ";";
      printf("%s \n", ss.str().c_str());
    }
  }
#endif
  deque<int> topo_order;
  topological_sort(graph, front_inserter(topo_order), vertex_index_map(identity_property_map()));
#if 0
  {
    stringstream ss;
    ss << "Topologically sorted superclasses ";
    for (deque<int>::const_reverse_iterator it = topo_order.rbegin(); it != topo_order.rend(); it++) {
      Instance_sp mc = arrayedSupers[*it].as<Instance_O>();
      ss << "-> " << mc->className() << "/" << mc->instanceClassName();
    }
    LOG("{}" , ss.str());
  }
#endif
  List_sp cpl = nil<T_O>();
  for (deque<int>::const_reverse_iterator it = topo_order.rbegin(); it != topo_order.rend(); it++) {
    Instance_sp mc = gc::As<Instance_sp>(arrayedSupers[*it]);
    LOG("pushing superclass[{}] to front of ClassPrecedenceList", mc->instanceClassName());
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
  Instance_sp find_theClass = cl__find_class(cl::_sym_class,true,nil<T_O>());
  if (_lisp->_Roots._TheClass != find_theClass) {
    printf("%s:%d   Instance_O::isSubClassOf  find_theClass(%p) and _lisp->_Root._TheClass(%p) don't match anymore\n", __FILE__, __LINE__, find_theClass.raw_(), _lisp->_Roots._TheClass.raw_() );
  }
#endif
  Instance_sp this_class = ENSURE_VALID_OBJECT(this->_Class);
  if (this_class == _lisp->_Roots._TheClass || this_class == _lisp->_Roots._TheBuiltInClass ||
      this_class == _lisp->_Roots._TheStandardClass || this_class == _lisp->_Roots._TheStructureClass ||
      this_class->isSubClassOf(_lisp->_Roots._TheClass)) {
    if (this == &*ancestor)
      return true;
    // TODO: I need to memoize this somehow so that I'm not constantly searching a list in
    // linear time
    List_sp cpl = this->instanceRef(Instance_O::REF_CLASS_CLASS_PRECEDENCE_LIST);
    ASSERTF(!cpl.unboundp(), "You tried to use isSubClassOf when the ClassPrecedenceList had not been initialized");
    for (auto cur : cpl) {
      if (CONS_CAR(cur) == ancestor)
        return true;
    }
    return false;
  }
  printf("%s:%d FAILED   this_class->isSubClassOf(_lisp->_Roots._TheClass) ->%d\n", __FILE__, __LINE__,
         this_class->isSubClassOf(_lisp->_Roots._TheClass));
  {
    printf("%s:%d   this->className() -> %s  checking if subclass of %s\n", __FILE__, __LINE__, _rep_(this->_className()).c_str(),
           _rep_(ancestor->_className()).c_str());
    List_sp cpl = this->instanceRef(Instance_O::REF_CLASS_CLASS_PRECEDENCE_LIST);
    for (auto cur : cpl) {
      printf("%s:%d  cpl -> %s\n", __FILE__, __LINE__, _rep_(gc::As<Instance_sp>(CONS_CAR(cur))->_className()).c_str());
    }
  }
  {
    printf("%s:%d   this->_Class->className() -> %s  its cpl ->>>\n", __FILE__, __LINE__,
           _rep_(this->_Class->_className()).c_str());
    List_sp cpl = this_class->instanceRef(Instance_O::REF_CLASS_CLASS_PRECEDENCE_LIST);
    for (auto cur : cpl) {
      Instance_sp cc = gc::As<Instance_sp>(CONS_CAR(cur));
      printf("%s:%d  cpl -> %s == _lisp->_Roots._TheClass -> %d\n", __FILE__, __LINE__, _rep_(cc->_className()).c_str(),
             cc == _lisp->_Roots._TheClass);
    }
  }
  TYPE_ERROR(this->asSmartPtr(), cl::_sym_class);
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

/*! Add this class as a direct subclass of the class named by className */
void Instance_O::addInstanceAsSubClass(Symbol_sp className) {
  //  printf("%s:%d This is where I would add the Instance SubClass for builtin cclasses\n", __FILE__, __LINE__ );
  Instance_sp cl;
  if (!(cl::_sym_findClass) || !cl::_sym_findClass->fboundp()) {
    cl = gc::As<Instance_sp>(cl__find_class(className));
  } else {
    cl = gc::As<Instance_sp>(eval::funcall(cl::_sym_findClass, className, _lisp->_true()));
  }
  List_sp dsc = cl->instanceRef(REF_CLASS_DIRECT_SUBCLASSES);
  cl->instanceSet(REF_CLASS_DIRECT_SUBCLASSES, Cons_O::create(this->asSmartPtr(), dsc));

#if 0
  Instance_sp cl;
  if (!(cl::_sym_findClass) || !cl::_sym_findClass->fboundp()) {
    cl = gc::As<Instance_sp>(cl__find_class(className));
  } else {
    cl = gc::As<Instance_sp>(eval::funcall(cl::_sym_findClass, className, _lisp->_true()));
  }
  // When booting _DirectSuperClasses may be undefined
  List_sp dsc = this->directSuperclasses();
  this->instanceSet(REF_CLASS_DIRECT_SUPERCLASSES, Cons_O::create(cl, dsc));
#endif
}

void Instance_O::__setupStage3NameAndCalculateClassPrecedenceList(Symbol_sp className) {
  this->_setClassName(className);
  // Initialize some of the class slots
  this->instanceSet(REF_CLASS_DIRECT_SLOTS, nil<T_O>());
  this->instanceSet(REF_CLASS_DEFAULT_INITARGS, nil<T_O>());
  T_sp tmc = this->_instanceClass();
  ASSERTNOTNULL(tmc);
  Instance_sp mc = gc::As<Instance_sp>(tmc);
  (void)mc;
  this->lowLevel_calculateClassPrecedenceList();
}

string Instance_O::dumpInfo() {
  stringstream ss;
  ss << fmt::format("this.instanceClassName: {} @ {}", this->instanceClassName(), (void*)this) << std::endl;
  ss << "_FullName[" << this->_className()->fullName() << "]" << std::endl;
  ss << fmt::format("    _Class = {}  this._Class.instanceClassName()={}\n", (void*)&*(this->__class()),
                    this->__class()->instanceClassName());
  for (auto cc : this->directSuperclasses()) {
    ss << "Base class: " << gc::As<Instance_sp>(oCar(cc))->instanceClassName() << std::endl;
  }
  ss << fmt::format("this.instanceCreator* = {}\n", (void*)(&*this->CLASS_get_creator()));
  return ss.str();
}

string Instance_O::getPackagedName() const { return this->_className()->formattedName(false); }

string Instance_O::_classNameAsString() const { return _rep_(this->_className()); }

T_sp Instance_O::make_instance() {
  T_sp instance = this->CLASS_get_creator()->creator_allocate(); // this->allocate_newNil();
  if (instance.generalp()) {
    instance.unsafe_general()->initialize();
  } else {
    SIMPLE_ERROR("Add support to make_instance of non general objects");
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
}; // namespace core

/* The following functions should only work for Classes */
namespace core {
DOCGROUP(clasp);
CL_DEFUN List_sp clos__direct_superclasses(Instance_sp c) {
  if (c->_Class->isSubClassOf(_lisp->_Roots._TheClass)) {
    return c->directSuperclasses();
  }
  TYPE_ERROR(c, cl::_sym_class);
}

// FIXME: Perhaps gctools::NextStamp could be exported and used as the stamp slot's initform.
CL_LAMBDA(class_ &optional (name nil name-p));
DOCGROUP(clasp);
CL_DEFUN void core__class_new_stamp(Instance_sp c, T_sp name, T_sp namep) {
  //  printf("%s:%d Something is whacked here - I'm calling NextStamp twice for class %s!!!!\n", __FILE__, __LINE__,
  //  _safe_rep_(name).c_str() );
  gctools::ShiftedStamp stamp = gctools::NextStampWtag(gctools::Header_s::rack_wtag);
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
  register_stamp_name(sname, gctools::Header_s::StampWtagMtag::unshift_shifted_stamp(stamp));
}

DOCGROUP(clasp);
CL_DEFUN T_sp core__class_stamp_for_instances(Instance_sp c) {
  if (c->_Class->isSubClassOf(_lisp->_Roots._TheClass)) {
    // DONT convert this to an integer - it is already a shifted stamp and
    // it can be treated like a fixnum
    ASSERT(gctools::Header_s::StampWtagMtag::is_shifted_stamp(c->CLASS_stamp_for_instances()));
    T_sp stamp((gctools::Tagged)c->CLASS_stamp_for_instances());
    return stamp;
  }
  TYPE_ERROR(c, cl::_sym_class);
};
}; // namespace core

#define READ_RACK_STAMP
#include <clasp/llvmo/read-stamp.cc>
#undef READ_RACK_STAMP

namespace core {
CL_DEFUN T_sp core__rack_stamp(core::T_sp obj) {
  General_O* client_ptr = gctools::untag_general<General_O*>((General_O*)obj.raw_());
  uintptr_t stamp = (uintptr_t)(llvmo::template_read_rack_stamp(client_ptr));
  // core::clasp_write_string(fmt::format("{}:{}:{} stamp = {}u\n", __FILE__, __LINE__, __FUNCTION__, stamp ));
  T_sp result((gctools::Tagged)stamp);
  return result;
}

DOCGROUP(clasp);
CL_DEFUN T_sp core__name_of_class(Instance_sp c) {
  if (c->_Class->isSubClassOf(_lisp->_Roots._TheClass)) {
    return c->_className();
  }
  TYPE_ERROR(c, cl::_sym_class);
};

DOCGROUP(clasp);
CL_DEFUN T_sp core__class_creator(Instance_sp c) {
  if (c->_Class->isSubClassOf(_lisp->_Roots._TheClass)) {
    return c->CLASS_get_creator();
  }
  TYPE_ERROR(c, cl::_sym_class);
};

DOCGROUP(clasp);
CL_DEFUN bool core__has_creator(Instance_sp c) {
  if (c->_Class->isSubClassOf(_lisp->_Roots._TheClass)) {
    return c->CLASS_has_creator();
  }
  TYPE_ERROR(c, cl::_sym_class);
};

}; // namespace core

namespace core {

CL_DEFMETHOD Instance_sp ClassHolder_O::class_get() const { return this->_Class.load(std::memory_order_relaxed); }
CL_DEFMETHOD void ClassHolder_O::class_set(Instance_sp cl) { this->_Class.store(cl, std::memory_order_relaxed); }
void ClassHolder_O::class_mkunbound() { this->_Class.store(unbound<Instance_O>(), std::memory_order_relaxed); }
bool ClassHolder_O::class_unboundp() const { return this->_Class.load(std::memory_order_relaxed).unboundp(); }

DOCGROUP(clasp);
CL_DEFUN Instance_sp ext__class_get(ClassHolder_sp holder) { return holder->class_get(); }

DOCGROUP(clasp);
CL_DEFUN bool ext__class_unboundp(ClassHolder_sp holder) { return holder->class_unboundp(); }

DOCGROUP(clasp);
CL_DEFUN void core__verify_instance_layout(size_t instance_size, size_t instance_rack_offset) {
  if (instance_size != sizeof(Instance_O))
    SIMPLE_ERROR("The cmpintrinsics.lisp instance_size {} does not match sizeof(Instance_O) {}", instance_size, sizeof(Instance_O));
  if (instance_rack_offset != offsetof(Instance_O, _Rack))
    SIMPLE_ERROR("instance_rack_offset {} does not match offsetof(_Rack,Instance_O) {}", instance_rack_offset,
                 offsetof(Instance_O, _Rack));
}

DOCGROUP(clasp);
CL_DEFUN void core__verify_rack_layout(size_t stamp_offset, size_t data_offset) {
  size_t cxx_stamp_offset = offsetof(Rack_O, _ShiftedStamp);
  size_t cxx_data_offset = offsetof(Rack_O, _Slots._Data);
  if (stamp_offset != cxx_stamp_offset)
    SIMPLE_ERROR("stamp_offset {} does not match cxx_stamp_offset {}", stamp_offset, cxx_stamp_offset);
  if (data_offset != cxx_data_offset)
    SIMPLE_ERROR("data_offset {} does not match cxx_data_offset {}", data_offset, cxx_data_offset);
}

}; // namespace core
