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
#ifndef _core_funcallable_instance_H_
#define _core_funcallable_instance_H_

#include <clasp/core/object.h>
#include <clasp/core/array.h>
#include <clasp/core/hashTable.fwd.h>
#include <clasp/core/mpPackage.h>

// may need more later
#include <clasp/gctools/gc_interface.h>

namespace core {
  FORWARD(FuncallableInstance);
};

template <>
struct gctools::GCInfo<core::FuncallableInstance_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {

  class FuncallableInstance_O : public Function_O {
    LISP_CLASS(core, CorePkg, FuncallableInstance_O, "FuncallableInstance",Function_O);
// These indices MUST match the order and positions of slots in +standard-generic-function-slots+
    typedef enum { REF_GFUN_NAME = 0,
                   REF_GFUN_SPECIALIZERS = 1,  // lock
                   REF_GFUN_LAMBDA_LIST = 3,   // lock
                 } GenericFunctionSlots;
  public: // ctor/dtor for classes with shared virtual base
    // entry_point is the LISP_CALLING_CONVENTION() macro
  FuncallableInstance_O(FunctionDescription* fdesc) :
    Base(funcallable_entry_point)
      , _Class(_Nil<Instance_O>())
      , _Sig(_Nil<T_O>())
      , _FunctionDescription(fdesc)
      , _CallHistory(_Nil<T_O>())
      , _SpecializerProfile(_Nil<T_O>())
//      _Lock(mp::SharedMutex_O::make_shared_mutex(_Nil<T_O>())),
      , _CompiledDispatchFunction(_Nil<T_O>()) {};
    explicit FuncallableInstance_O(FunctionDescription* fdesc,Instance_sp metaClass, size_t slots) :
    Base(funcallable_entry_point),
      _Class(metaClass)
      ,_Sig(_Unbound<T_O>())
      ,_FunctionDescription(fdesc)
      , _CallHistory(_Nil<T_O>())
      ,_SpecializerProfile(_Nil<T_O>())
//      ,_Lock(mp::SharedMutex_O::make_shared_mutex(_Nil<T_O>()))
      , _CompiledDispatchFunction(_Nil<T_O>())
    {};
    virtual ~FuncallableInstance_O(){};
  public:
    // The order MUST be
    // entry (inherited from Function_O)
    // _Class   (matches offset of Instance_O)
    // _Rack    (matches offset of Instance_O)
    Instance_sp _Class;
    Rack_sp _Rack;
    T_sp   _Sig;
    FunctionDescription* _FunctionDescription;
    std::atomic<size_t>         _Compilations;
    gc::atomic_wrapper<T_sp>   _CallHistory;
    gc::atomic_wrapper<T_sp>   _SpecializerProfile;
//    T_sp   _Lock;
    gc::atomic_wrapper<T_sp>   _CompiledDispatchFunction;
  public:
    T_sp GFUN_NAME() const { return this->instanceRef(REF_GFUN_NAME); };
    T_sp GFUN_SPECIALIZERS() const { return this->instanceRef(REF_GFUN_SPECIALIZERS); };
    T_sp GFUN_LAMBDA_LIST() const { return this->instanceRef(REF_GFUN_LAMBDA_LIST);};
    void GFUN_LAMBDA_LIST_set(T_sp lambda_list)
    {
      if (this->instanceRef(REF_GFUN_LAMBDA_LIST).unboundp() && lambda_list.nilp()) {
        printf("%s:%d Ignoring GFUN_LAMBDA_LIST_SET - returning\n", __FILE__, __LINE__ );
        return;
      }
      this->instanceSet(REF_GFUN_LAMBDA_LIST,lambda_list);
    };
    T_sp GFUN_SPECIALIZER_PROFILE() const { return this->_SpecializerProfile.load(); };
    T_sp GFUN_SPECIALIZER_PROFILE_compare_exchange(T_sp expected, T_sp new_value);
    T_sp GFUN_CALL_HISTORY() const { return this->_CallHistory.load(); };
    T_sp GFUN_CALL_HISTORY_compare_exchange(T_sp expected, T_sp new_value);

//    mp::SharedMutex_sp GFUN_LOCK() const { return gc::As<mp::SharedMutex_sp>(this->_Lock);};
//    void GFUN_LOCK_set(T_sp l) { this->_Lock = l; };
    T_sp GFUN_DISPATCHER() const { return this->_CompiledDispatchFunction.load(); };
    void GFUN_DISPATCHER_set(T_sp val) { this->_CompiledDispatchFunction.store(val); };
  public:

    void accumulateSuperClasses(HashTableEq_sp supers, ComplexVector_T_sp arrayedSupers, Instance_sp mc);
    void lowLevel_calculateClassPrecedenceList();

//    virtual bool isSubClassOf(Instance_sp mc) const;

    FunctionDescription* fdesc() const { return this->_FunctionDescription; }
    virtual void set_fdesc(FunctionDescription* address) { this->_FunctionDescription = address; };
    
    T_sp make_instance();
  public:
  // Add support for Function_O methods
    T_sp functionName() const { return this->GFUN_NAME(); };
    virtual T_sp closedEnvironment() const { HARD_IMPLEMENT_ME(); };
    virtual T_sp setSourcePosInfo(T_sp sourceFile, size_t filePos, int lineno, int column) { HARD_IMPLEMENT_ME(); };
//  virtual T_mv functionSourcePos() const { HARD_IMPLEMENT_ME();;
    virtual List_sp declares() const { HARD_IMPLEMENT_ME(); };
    virtual T_sp docstring() const { HARD_IMPLEMENT_ME(); };
    virtual void *functionAddress() const { HARD_IMPLEMENT_ME(); };
    virtual bool macroP() const { return false; };
    virtual T_sp getKind() const { return kw::_sym_function; };
    virtual int sourceFileInfoHandle() const { HARD_IMPLEMENT_ME(); };
    virtual size_t filePos() const { return 0; }
    virtual int lineNumber() const { return 0; }
    virtual int column() const { return 0; };
    virtual T_sp lambdaListHandler() const { HARD_IMPLEMENT_ME(); };
  public: // The hard-coded indexes above are defined below to be used by Class
    void initializeSlots(gctools::ShiftedStamp is, size_t numberOfSlots);
    void initializeClassSlots(Creator_sp creator, gctools::ShiftedStamp class_stamp);
    virtual void setf_lambda_list(List_sp lambda_list) { this->GFUN_LAMBDA_LIST_set(lambda_list); };
    virtual T_sp lambda_list() const { return this->GFUN_LAMBDA_LIST(); };
  public:
    static size_t rack_stamp_offset();
  public:
    virtual void LISP_INVOKE();

  public: // Functions here
    Fixnum stamp() const;
    void stamp_set(Fixnum s);
    size_t numberOfSlots() const;

    Instance_sp _instanceClass() const { return this->_Class; };

    T_sp instanceClassSet(Instance_sp mc);

    virtual T_sp instanceSig() const;

  /*! Return the value of a slot */
    T_sp instanceRef(size_t idx) const;
  /*! Set the value of a slot and return the new value */
    T_sp instanceSet(size_t idx, T_sp val);

    string __repr__() const;

    T_sp copyInstance() const;

    T_sp setFuncallableInstanceFunction(T_sp functionOrT);

    void increment_compilations() { this->_Compilations++; };
    size_t compilations() const { return this->_Compilations.load(); };
    
    void describe(T_sp stream);

    void __write__(T_sp sout) const; // Look in write_ugly.cc

    static LCC_RETURN funcallable_entry_point(LCC_ARGS_ELLIPSIS);
  }; // FuncallableInstance class

}; // core namespace


namespace gctools {
 /*! Specialize TaggedCast for FuncallableInstance_O - always use dynamic_cast */
  template <typename FROM>
    struct TaggedCast<core::FuncallableInstance_O *, FROM> {
    typedef core::FuncallableInstance_O *ToType;
    typedef FROM FromType;
    inline static bool isA(FromType ptr) {
      if (tagged_generalp(ptr)) {
      // Maybe
        core::General_O* raw_client = (core::General_O*)untag_general<FromType>(ptr);
        core::FuncallableInstance_O* iptr = dynamic_cast<core::FuncallableInstance_O*>(raw_client);
        return iptr!=NULL;
      }
      return false;
    }
    inline static core::FuncallableInstance_O* castOrNULL(FromType client) {
      if ( tagged_generalp(client) ) {
      // maybe
        core::General_O* raw_client = (core::General_O*)untag_general<FromType>(client);
        core::FuncallableInstance_O* iclient = dynamic_cast<core::FuncallableInstance_O*>(raw_client);
        if ( iclient ) return tag_general<ToType>(iclient);
        return NULL;
      }
      return NULL;
    }
  };
};


template <>
struct gctools::GCInfo<core::DtreeInterpreter_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {

FORWARD(DtreeInterpreter);
  class DtreeInterpreter_O : public Closure_O {
    LISP_CLASS(core, CorePkg, DtreeInterpreter_O, "DtreeInterpreter",Closure_O);
  public:
    static const size_t dtree_eql_test_step = 2; // size of the step between dtree eql_tests
    static const size_t dtree_eql_test_outcome_index = 0;
    static const size_t dtree_eql_test_spec_index = 1;
    static const size_t dtree_range_step = 3; // size of the step between dtree ranges
    static const size_t dtree_range_outcome_index = 0;
    static const size_t dtree_range_low_index = 1;
    static const size_t dtree_range_high_index = 2;
    inline static size_t dtree_eql_test_step_fn() { return dtree_eql_test_step; };
    inline static size_t dtree_range_step_fn() { return dtree_range_step; };

    typedef enum { REF_TYPE = 0 } NamedStructSlots;
    typedef enum { REF_DTREE_TYPE = 0,
                   REF_DTREE_NODE = 1,
                   REF_DTREE_END = 2 } DtreeSlots;
    typedef enum { REF_NODE_TYPE = 0,
                   REF_NODE_EQL_SPECIALIZERS = 1,
                   REF_NODE_SKIP = 2,
                   REF_NODE_TAG_TESTS = 3,
                   REF_NODE_CXX_CLASS_SPECIALIZERS = 4,
                   REF_NODE_CLASS_SPECIALIZERS = 5,
                   REF_NODE_NODE = 6,
                   REF_NODE_END = 7} NodeSlots;
    typedef enum { REF_RANGE_TYPE = 0,
                   REF_RANGE_OUTCOME = 1,
                   REF_RANGE_FIRST_STAMP = 2,
                   REF_RANGE_LAST_STAMP = 3,
                   REF_RANGE_REVERSED_CLASSES = 4,
                   REF_RANGE_END = 5} RangeSlots;
    typedef enum { REF_SKIP_TYPE = 0,
                   REF_SKIP_OUTCOME = 1,
                   REF_SKIP_END = 2 } SkipSlots;
    typedef enum { REF_OUTCOME_TYPE = 0,
                   REF_OUTCOME_SUBTYPE = 1 } OutcomeSlots;
    typedef enum { REF_OPTIMIZED_SLOT_READER_TYPE = 0,
                   REF_OPTIMIZED_SLOT_READER_SUBTYPE = 1,
                   REF_OPTIMIZED_SLOT_READER_INDEX = 2,
                   REF_OPTIMIZED_SLOT_READER_SLOT_NAME = 3,
                   REF_OPTIMIZED_SLOT_READER_METHOD = 4,
                   REF_OPTIMIZED_SLOT_READER_CLASS = 5,
                   REF_OPTIMIZED_SLOT_READER_END = 6 } OptimizedSlotReaderSlots;
    typedef enum { REF_OPTIMIZED_SLOT_WRITER_TYPE = 0,
                   REF_OPTIMIZED_SLOT_WRITER_SUBTYPE = 1,
                   REF_OPTIMIZED_SLOT_WRITER_INDEX = 2,
                   REF_OPTIMIZED_SLOT_WRITER_SLOT_NAME = 3,
                   REF_OPTIMIZED_SLOT_WRITER_METHOD = 4,
                   REF_OPTIMIZED_SLOT_WRITER_CLASS = 5,
                   REF_OPTIMIZED_SLOT_WRITER_END = 6 } OptimizedSlotWriterSlots;
    typedef enum { REF_FAST_METHOD_CALL_TYPE = 0,
                   REF_FAST_METHOD_CALL_SUBTYPE = 1,
                   REF_FAST_METHOD_CALL_FUNCTION = 2,
                   REF_FAST_METHOD_CALL_END = 3 } FastMethodCallSlots;
    typedef enum { REF_EFFECTIVE_METHOD_OUTCOME_TYPE = 0,
                   REF_EFFECTIVE_METHOD_OUTCOME_SUBTYPE = 1,
                   REF_EFFECTIVE_METHOD_OUTCOME_APPLICABLE_METHODS = 2,
                   REF_EFFECTIVE_METHOD_OUTCOME_FORM = 3,
                   REF_EFFECTIVE_METHOD_OUTCOME_FUNCTION = 4,
                   REF_EFFECTIVE_METHOD_OUTCOME_END = 5 } EffectiveMethodOutcome;
  public:
    T_sp            _GenericFunction;
    core::T_sp      _DtreeRoot;
    size_t          _CallCount;
  public:
    static DtreeInterpreter_sp make_dtree_interpreter(T_sp generic_function, T_sp dtree);
  public:
    static LCC_RETURN LISP_CALLING_CONVENTION();
    DtreeInterpreter_O(FunctionDescription* fdesc, T_sp generic_function, T_sp dtreeRoot) : Closure_O(entry_point,fdesc), _GenericFunction(generic_function), _DtreeRoot(dtreeRoot), _CallCount(0) {};
  };

};

#endif /* _core_instance_H_ */
