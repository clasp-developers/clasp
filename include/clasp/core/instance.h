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
#ifndef _core_instance_H_
#define _core_instance_H_

#include <clasp/core/object.h>
#include <clasp/core/array.h>
#include <clasp/core/hashTable.fwd.h>
#include <clasp/core/mpPackage.fwd.h>
#include <clasp/core/instance.fwd.h>
// may need more later
#include <clasp/gctools/gc_interface.h>

template <>
struct gctools::GCInfo<core::Instance_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};



namespace core {
FORWARD(Rack);
  class Rack_O final : public General_O {
    LISP_CLASS(core,CorePkg,Rack_O,"Rack",core::General_O);
  public:
    gctools::ShiftedStamp     _ShiftedStamp;
    typedef core::T_sp value_type;
    gctools::GCArray_moveable<value_type> _Slots;
  public:
    Rack_O(size_t length, value_type initialElement=T_sp(), bool initialElementSupplied=true) : _Slots(length,initialElement,initialElementSupplied) {};

    static Rack_sp make(size_t numberOfSlots, T_sp value);
    size_t length() const { return this->_Slots._Length; };
        inline T_sp &operator[](size_t idx) {
      BOUNDS_ASSERT(idx<this->_Slots._Length);
      return this->_Slots[idx];
    };
    inline const T_sp &operator[](size_t idx) const {
      BOUNDS_ASSERT(idx<this->_Slots._Length);
      return this->_Slots[idx];
    };
    inline void stamp_set(gctools::ShiftedStamp stamp) {
      ASSERT(stamp==0||gctools::Header_s::Value::is_rack_shifted_stamp(stamp));
      this->_ShiftedStamp = stamp;
    };
    inline gctools::ShiftedStamp stamp_get() {
      ASSERT(gctools::Header_s::Value::is_rack_shifted_stamp(this->_ShiftedStamp));
      return this->_ShiftedStamp;
    };
  };
};

namespace core {

  class Instance_O : public General_O {
    LISP_CLASS(core, CorePkg, Instance_O, "Instance",General_O);
    // Store the stamp in slot 0 - so offset all the other slots
  // These must be exposed in core__class_slot_sanity_check()
#define NUMBER_OF_SPECIALIZER_SLOTS 5
#define CLASS_SLOT_OFFSET NUMBER_OF_SPECIALIZER_SLOTS
    typedef enum {
        REF_SPECIALIZER_CALL_HISTORY_GENERIC_FUNCTIONS = 3,
        REF_SPECIALIZER_MUTEX = 4,
        REF_CLASS_CLASS_NAME = (0 + CLASS_SLOT_OFFSET),
        REF_CLASS_DIRECT_SUPERCLASSES = (1+CLASS_SLOT_OFFSET),
        REF_CLASS_DIRECT_SUBCLASSES = (2+CLASS_SLOT_OFFSET),
        REF_CLASS_SLOTS = (3+CLASS_SLOT_OFFSET),
        REF_CLASS_CLASS_PRECEDENCE_LIST = (4+CLASS_SLOT_OFFSET),
        REF_CLASS_DIRECT_SLOTS = (5+CLASS_SLOT_OFFSET),
        REF_CLASS_DIRECT_DEFAULT_INITARGS = (6+CLASS_SLOT_OFFSET),
        REF_CLASS_DEFAULT_INITARGS = (7+CLASS_SLOT_OFFSET),
        REF_CLASS_FINALIZED = (8+CLASS_SLOT_OFFSET),
        REF_CLASS_DOCSTRING = (9+CLASS_SLOT_OFFSET),
        REF_CLASS_DEPENDENTS = (12+CLASS_SLOT_OFFSET),
        REF_CLASS_LOCATION_TABLE = (15+CLASS_SLOT_OFFSET),
        REF_CLASS_STAMP_FOR_INSTANCES_ = (16+CLASS_SLOT_OFFSET),
        REF_CLASS_CREATOR = (17+CLASS_SLOT_OFFSET)
    } Slots;

  public:
    bool fieldsp() const;
    void fields(Record_sp node);
  public: // ctor/dtor for classes with shared virtual base
  Instance_O() : _Sig(_Unbound<T_O>()), _Class(_Nil<Instance_O>()), _Rack(_Unbound<Rack_O>()) {};
    explicit Instance_O(Instance_sp metaClass) :
      _Sig(_Unbound<T_O>())
      ,_Class(metaClass)
        ,_Rack(_Unbound<Rack_O>())
      
//    ,_NumberOfSlots(slots)
    {};
    virtual ~Instance_O(){};
  public:
    // The order MUST be:
    // _Sig
    // _Class (matches offset of FuncallableInstance_O)
    // _Rack  (matches offset of FuncallableInstance_O)
    T_sp _Sig;
    Instance_sp _Class;
    Rack_sp _Rack;
  /*! Mimicking ECL instance->sig generation signature
        This is pointed to the class slots in case they change 
        - then the instances can be updated*/
  public:
    static Instance_sp createClassUncollectable(gctools::ShiftedStamp is,Instance_sp metaClass, size_t number_of_slots, Creator_sp creator);
    static Instance_sp create(Symbol_sp symbol,Instance_sp metaClass,Creator_sp creator);
  
  /*! Setup the instance nil value */
  //	void setupInstanceNil();

  public:
    virtual bool isCallable() const { return false; };
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
      ASSERT(gctools::Header_s::Value::is_shifted_stamp(result));
      return result;
    };
    void CLASS_set_stamp_for_instances(gctools::UnshiftedStamp s);

    void CLASS_call_history_generic_functions_push_new(T_sp generic_function);
    void CLASS_call_history_generic_functions_remove(T_sp list_ofgeneric_functions);
    
    string dumpInfo();

  /*! Return the direct superclasses */
    List_sp directSuperclasses() const;

    void addInstanceBaseClass(Symbol_sp cl);

    T_sp slots() const { return this->instanceRef(REF_CLASS_SLOTS); };

    template <typename oclass>
      bool isSubClassOf() const {
    return this->isSubClassOf(lisp_classFromClassSymbol(oclass::static_classSymbol()));
  }

    void accumulateSuperClasses(HashTableEq_sp supers, ComplexVector_T_sp arrayedSupers, Instance_sp mc);
    void lowLevel_calculateClassPrecedenceList();

    virtual bool isSubClassOf(Instance_sp mc) const;

    string getPackagedName() const;
    string instanceClassName() { return this->getPackagedName(); };
    string instanceClassName() const { return this->getPackagedName(); };

    T_sp make_instance();
  /*! predicate if this is a BuiltInClass class */
    virtual bool builtInClassP() const { return this == &*core::lisp_built_in_class(); };

  /*! predicate if this is a raw C++ class that is wrapped with clbind
          - it can only be used to derive other classes if cxxDerivableClassP is true */
    virtual bool cxxClassP() const { return false; };

  /*! cxxDerivableClass is a class that inherits from a raw C++ class and
          the clbind::Adapter class - this allows it to be derived from */
    virtual bool cxxDerivableClassP() const { return false; };

  /*! primaryCxxDerivableClassP is a predicate that returns true if
          this class is the primary derivable C++ class */
    virtual bool primaryCxxDerivableClassP() const { return false; };

    void setInstanceBaseClasses(List_sp classes);
    void __setup_stage1_with_sharedPtr_lisp_sid(T_sp theThis, Symbol_sp instanceClassSymbol) {
    this->instanceSet(REF_CLASS_CLASS_NAME, instanceClassSymbol);
  }

    void __setup_stage2_with_classSymbol(Symbol_sp csid) {
    _OF();
  }

    void __setupStage3NameAndCalculateClassPrecedenceList(Symbol_sp isid);

    void addInstanceBaseClassDoNotCalculateClassPrecedenceList(Symbol_sp cl);
  public: // The hard-coded indexes above are defined below to be used by Class
    void initializeSlots(gctools::ShiftedStamp is, size_t numberOfSlots);
    void initializeClassSlots(Creator_sp creator, gctools::ShiftedStamp class_stamp);
  public:
    static size_t rack_stamp_offset();
  public: // Functions here
    Fixnum stamp() const;
    void stamp_set(gctools::ShiftedStamp s);
    size_t numberOfSlots() const;
  /*! Return number of slots if not nil otherwise nil */

    Instance_sp _instanceClass() const { return this->_Class; };

    T_sp instanceClassSet(Instance_sp mc);

    virtual T_sp instanceSigSet();
    virtual T_sp instanceSig() const;


    virtual bool equalp(T_sp obj) const;
    virtual void sxhash_equalp(HashGenerator &hg,LocationDependencyPtrT ptr) const;

  /*! Return the value of a slot */
    T_sp instanceRef(size_t idx) const;
  /*! Set the value of a slot and return the new value */
    T_sp instanceSet(size_t idx, T_sp val);

    string __repr__() const;

    virtual T_sp copyInstance() const;

    virtual void describe(T_sp stream);

    void __write__(T_sp sout) const; // Look in write_ugly.cc



  }; // Instance class

  #define OPTIMIZED_SLOT_INDEX_INDEX 1

    template <class RackType_sp>
    inline T_sp low_level_instanceRef(RackType_sp rack, size_t index) { return (*rack)[index]; }
  template <class RackType_sp>
    inline void low_level_instanceSet(RackType_sp rack, size_t index, T_sp value) { (*rack)[index] = value; }

}; // core namespace


namespace gctools {
 /*! Specialize TaggedCast for Instance_O - always use dynamic_cast */
  template <typename FROM>
    struct TaggedCast<core::Instance_O *, FROM> {
    typedef core::Instance_O *ToType;
    typedef FROM FromType;
    inline static bool isA(FromType ptr) {
      if (tagged_generalp(ptr)) {
      // Maybe
        core::General_O* raw_client = (core::General_O*)untag_general<FromType>(ptr);
        core::Instance_O* iptr = dynamic_cast<core::Instance_O*>(raw_client);
        return iptr!=NULL;
      }
      return false;
    }
    inline static core::Instance_O* castOrNULL(FromType client) {
      if ( tagged_generalp(client) ) {
      // maybe
        core::General_O* raw_client = (core::General_O*)untag_general<FromType>(client);
        core::Instance_O* iclient = dynamic_cast<core::Instance_O*>(raw_client);
        if ( iclient ) return tag_general<ToType>(iclient);
        return NULL;
      }
      return NULL;
    }
  };
};

namespace core {
  T_sp core__allocate_new_instance(Instance_sp theClass, size_t numberOfSlots);
};

namespace core {
  struct ClassReadLock {
    mp::SharedMutex_sp _Lock;
    ClassReadLock(mp::SharedMutex_sp lock);
    ~ClassReadLock();
  };

  struct ClassWriteLock {
    mp::SharedMutex_sp _Lock;
    ClassWriteLock(mp::SharedMutex_sp lock);
    ~ClassWriteLock();
  };

};


namespace core {
SMART(ClassHolder);
class ClassHolder_O : public core::General_O {
  LISP_CLASS(core, CorePkg, ClassHolder_O, "ClassHolder",core::General_O);
public:
  static ClassHolder_sp create(Instance_sp cl) {
    GC_ALLOCATE_VARIADIC(ClassHolder_O,ch,cl);
    return ch;
  }
  std::atomic<Instance_sp> _Class;
public:
  bool class_unboundp() const;
  Instance_sp class_get() const;
  void class_set(Instance_sp cl);
  void class_mkunbound();
  explicit ClassHolder_O(Instance_sp c) : _Class(c) {};
};
};

#endif /* _core_instance_H_ */
