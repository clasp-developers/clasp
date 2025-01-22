#pragma once
/*
    File: instance.h
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

#include <clasp/core/object.h>
#include <clasp/core/array.h>
#include <clasp/core/hashTable.fwd.h>
#include <clasp/core/instance.fwd.h>
// may need more later
#include <clasp/gctools/gc_interface.h>

template <> struct gctools::GCInfo<core::Instance_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {
FORWARD(Rack);
class Rack_O final : public General_O {
  LISP_CLASS(core, CorePkg, Rack_O, "Rack", core::General_O);

public:
  gctools::ShiftedStamp _ShiftedStamp;
  T_sp _Sig; // A list of slotds, used in change-class.
  typedef core::T_sp value_type;
  gctools::GCArray_atomic<value_type> _Slots;

public:
  Rack_O(size_t length, T_sp sig, value_type initialElement = T_sp(), bool initialElementSupplied = true)
      : _Sig(sig), _Slots(length, initialElement, initialElementSupplied){};

  static Rack_sp make(size_t numberOfSlots, T_sp sig, value_type value);
  size_t length() const { return this->_Slots.length(); };
  inline value_type low_level_rackRef(size_t i) { return _Slots.load(i); }
  inline void low_level_rackSet(size_t i, value_type value) { _Slots.store(i, value); }
  inline bool low_level_rack_compare_exchange_weak(size_t i, value_type& expected, value_type desired,
                                                   std::memory_order sync = std::memory_order_seq_cst) {
    return this->_Slots[i].compare_exchange_weak(expected, desired, sync);
  }
  inline bool low_level_rack_compare_exchange_strong(size_t i, value_type& expected, value_type desired,
                                                     std::memory_order sync = std::memory_order_seq_cst) {
    return this->_Slots[i].compare_exchange_strong(expected, desired, sync);
  }
  inline void stamp_set(gctools::ShiftedStamp stamp) {
    ASSERT(stamp == 0 || gctools::Header_s::StampWtagMtag::is_rack_shifted_stamp(stamp) ||
           gctools::Header_s::StampWtagMtag::is_derivable_shifted_stamp(stamp));
    this->_ShiftedStamp = stamp;
  };
  inline gctools::ShiftedStamp stamp_get() {
    ASSERT(gctools::Header_s::StampWtagMtag::is_rack_shifted_stamp(this->_ShiftedStamp) ||
           gctools::Header_s::StampWtagMtag::is_derivable_shifted_stamp(this->_ShiftedStamp));
    return this->_ShiftedStamp;
  };
};
}; // namespace core

namespace core {

class Instance_O : public General_O {
  LISP_CLASS(core, CorePkg, Instance_O, "Instance", General_O);
  // Store the stamp in slot 0 - so offset all the other slots
  // These must be exposed in core__class_slot_sanity_check()
#define NUMBER_OF_SPECIALIZER_SLOTS 3
#define CLASS_SLOT_OFFSET NUMBER_OF_SPECIALIZER_SLOTS
  typedef enum {
    REF_SPECIALIZER_CALL_HISTORY_GENERIC_FUNCTIONS = 1,
    REF_SPECIALIZER_MUTEX = 2,
    REF_CLASS_CLASS_NAME = (0 + CLASS_SLOT_OFFSET),
    REF_CLASS_DIRECT_SUPERCLASSES = (1 + CLASS_SLOT_OFFSET),
    REF_CLASS_DIRECT_SUBCLASSES = (2 + CLASS_SLOT_OFFSET),
    REF_CLASS_SLOTS = (3 + CLASS_SLOT_OFFSET),
    REF_CLASS_CLASS_PRECEDENCE_LIST = (4 + CLASS_SLOT_OFFSET),
    REF_CLASS_DIRECT_SLOTS = (5 + CLASS_SLOT_OFFSET),
    REF_CLASS_DIRECT_DEFAULT_INITARGS = (6 + CLASS_SLOT_OFFSET),
    REF_CLASS_DEFAULT_INITARGS = (7 + CLASS_SLOT_OFFSET),
    REF_CLASS_FINALIZED = (8 + CLASS_SLOT_OFFSET),
    REF_CLASS_DOCSTRING = (9 + CLASS_SLOT_OFFSET),
    REF_CLASS_DEPENDENTS = (12 + CLASS_SLOT_OFFSET),
    REF_CLASS_LOCATION_TABLE = (14 + CLASS_SLOT_OFFSET),
    REF_CLASS_STAMP_FOR_INSTANCES_ = (15 + CLASS_SLOT_OFFSET),
    REF_CLASS_CREATOR = (16 + CLASS_SLOT_OFFSET)
  } Slots;

public:
  bool fieldsp() const override;
  void fields(Record_sp node) override;

public: // ctor/dtor for classes with shared virtual base
  Instance_O() : _Class(nil<Instance_O>()), _Rack(unbound<Rack_O>()){};
  explicit Instance_O(Instance_sp metaClass) : _Class(metaClass), _Rack(unbound<Rack_O>()){};
  Instance_O(Instance_sp cl, Rack_sp rack) : _Class(cl), _Rack(rack){};

public:
  // The order MUST be:
  // _Class (as large as FuncallableInstance_O's entry slot)
  // _Rack  (matches offset in FuncallableInstance_O)
  Instance_sp _Class;
  Rack_sp _Rack;

public:
  static Instance_sp createClassUncollectable(gctools::BaseHeader_s::StampWtagMtag is, Instance_sp metaClass,
                                              size_t number_of_slots, Creator_sp creator);
  static Instance_sp create(Symbol_sp symbol, Instance_sp metaClass, Creator_sp creator);

  /*! Setup the instance nil value */
  //	void setupInstanceNil();

public:
  // Functions from Instance_O
  string _classNameAsString() const;
  void _setClassName(Symbol_sp id) { this->instanceSet(REF_CLASS_CLASS_NAME, id); };
  Symbol_sp _className() const { return gc::As<Symbol_sp>(this->instanceRef(REF_CLASS_CLASS_NAME)); }

  void CLASS_set_creator(Creator_sp cb);
  Creator_sp CLASS_get_creator() const { return gc::As_unsafe<Creator_sp>(this->instanceRef(REF_CLASS_CREATOR)); };
  bool CLASS_has_creator() const { return (bool)(!this->instanceRef(REF_CLASS_CREATOR).unboundp()); };
  gctools::ShiftedStamp CLASS_stamp_for_instances() const {
    gctools::ShiftedStamp result = (gctools::ShiftedStamp)this->instanceRef(REF_CLASS_STAMP_FOR_INSTANCES_).raw_();
    ASSERT(gctools::Header_s::StampWtagMtag::is_shifted_stamp(result));
    return result;
  };
  void CLASS_set_stamp_for_instances(gctools::BaseHeader_s::StampWtagMtag s);

  string dumpInfo();

  /*! Return the direct superclasses */
  List_sp directSuperclasses() const;

  void addInstanceBaseClass(Symbol_sp cl);
  void addInstanceAsSubClass(Symbol_sp parent);
  T_sp slots() const { return this->instanceRef(REF_CLASS_SLOTS); };

  template <typename oclass> bool isSubClassOf() const {
    return this->isSubClassOf(lisp_classFromClassSymbol(oclass::static_classSymbol()));
  }

  void accumulateSuperClasses(HashTable_sp supers, ComplexVector_T_sp arrayedSupers, Instance_sp mc);
  void lowLevel_calculateClassPrecedenceList();

  virtual bool isSubClassOf(Instance_sp mc) const;

  string getPackagedName() const;
  string instanceClassName() { return this->getPackagedName(); };
  string instanceClassName() const { return this->getPackagedName(); };

  T_sp make_instance();

  /*! predicate if this is a raw C++ class that is wrapped with clbind
          - it can only be used to derive other classes if cxxDerivableClassP is true */
  virtual bool cxxClassP() const { return false; };

  /*! cxxDerivableClass is a class that inherits from a raw C++ class and
          the clbind::Adapter class - this allows it to be derived from */
  virtual bool cxxDerivableClassP() const { return false; };

  void setInstanceBaseClasses(List_sp classes);
  void __setup_stage1_with_sharedPtr_lisp_sid(T_sp theThis, Symbol_sp instanceClassSymbol) {
    this->instanceSet(REF_CLASS_CLASS_NAME, instanceClassSymbol);
  }

  void __setup_stage2_with_classSymbol(Symbol_sp csid) { _OF(); }

  void __setupStage3NameAndCalculateClassPrecedenceList(Symbol_sp isid);

  void addInstanceBaseClassDoNotCalculateClassPrecedenceList(Symbol_sp cl);

public: // The hard-coded indexes above are defined below to be used by Class
  void initializeSlots(gctools::ShiftedStamp is, T_sp sig, size_t numberOfSlots);
  // Used by clbind
  void initializeSlots(gctools::ShiftedStamp is, size_t numberOfSlots) { initializeSlots(is, unbound<T_O>(), numberOfSlots); }
  void initializeClassSlots(Creator_sp creator, gctools::BaseHeader_s::StampWtagMtag class_stamp);

public:
  static size_t rack_stamp_offset();

public: // Functions here
  Rack_sp rack() const { return this->_Rack; };
  Fixnum stamp() const;
  void stamp_set(gctools::ShiftedStamp s);
  size_t numberOfSlots() const;
  /*! Return number of slots if not nil otherwise nil */

  Instance_sp _instanceClass() const override { return this->_Class; };

  T_sp instanceClassSet(Instance_sp mc) override;

  virtual T_sp instanceSigSet() override;
  virtual T_sp instanceSig() const override;

  virtual bool equalp(T_sp obj) const override;
  virtual void sxhash_equalp(HashGenerator& hg) const override;

  /*! Return the value of a slot */
  T_sp instanceRef(size_t idx) const override;
  /*! Set the value of a slot and return the new value */
  T_sp instanceSet(size_t idx, T_sp val) override;

  string __repr__() const override;

  virtual void describe(T_sp stream) override;

  void __write__(T_sp sout) const override; // Look in write_ugly.cc

}; // Instance class

#define OPTIMIZED_SLOT_INDEX_INDEX 1

template <class RackType_sp> inline T_sp low_level_instanceRef(RackType_sp rack, size_t index) {
  return rack->low_level_rackRef(index);
}
template <class RackType_sp> inline void low_level_instanceSet(RackType_sp rack, size_t index, T_sp value) {
  rack->low_level_rackSet(index, value);
}

}; // namespace core

namespace gctools {
/*! Specialize TaggedCast for Instance_O - always use dynamic_cast */
template <typename FROM> struct TaggedCast<core::Instance_O*, FROM> {
  typedef core::Instance_O* ToType;
  typedef FROM FromType;
  inline static bool isA(FromType ptr) {
    if (tagged_generalp(ptr)) {
      // Maybe
      core::General_O* raw_client = (core::General_O*)untag_general<FromType>(ptr);
      if (cast::Cast<core::Instance_O*, core::General_O*>::isA(raw_client))
        return true;
      core::Instance_O* iptr = dynamic_cast<core::Instance_O*>(raw_client);
      return iptr != NULL;
    }
    return false;
  }
  inline static core::Instance_O* castOrNULL(FromType client) {
    if (tagged_generalp(client)) {
      // maybe
      core::General_O* raw_client = (core::General_O*)untag_general<FromType>(client);
      if (cast::Cast<core::Instance_O*, core::General_O*>::isA(raw_client)) {
        return tag_general<core::Instance_O*>(reinterpret_cast<core::Instance_O*>(raw_client));
      }
      core::Instance_O* iclient = dynamic_cast<core::Instance_O*>(raw_client);
      if (iclient)
        return tag_general<ToType>(iclient);
      return NULL;
    }
    return NULL;
  }
};
}; // namespace gctools

namespace core {
SMART(ClassHolder);
class ClassHolder_O : public core::General_O {
  LISP_CLASS(core, CorePkg, ClassHolder_O, "ClassHolder", core::General_O);

public:
  static ClassHolder_sp create(Instance_sp cl) {
    auto ch = gctools::GC<ClassHolder_O>::allocate(cl);
    return ch;
  }
  std::atomic<Instance_sp> _Class;

public:
  bool class_unboundp() const;
  Instance_sp class_get() const;
  void class_set(Instance_sp cl);
  void class_mkunbound();
  explicit ClassHolder_O(Instance_sp c) : _Class(c){};
};
}; // namespace core
