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

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/array.h>
#include <clasp/core/hashTable.fwd.h>
#include <clasp/core/instance.fwd.h>
// may need more later
#include <clasp/gctools/gc_interface.h>

/*! Different values for Instance_O.isgf */
#define CLASP_NOT_FUNCALLABLE 0
#define CLASP_STANDARD_DISPATCH 1
#define CLASP_RESTRICTED_DISPATCH 2
#define CLASP_READER_DISPATCH 3
#define CLASP_WRITER_DISPATCH 4
#define CLASP_USER_DISPATCH 5
#define CLASP_STRANDH_DISPATCH 6
#define CLASP_INVALIDATED_DISPATCH 7


template <>
struct gctools::GCInfo<core::Instance_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {

  class Instance_O : public General_O {
    LISP_CLASS(core, CorePkg, Instance_O, "Instance",General_O);
  // These must be exposed in core__class_slot_sanity_check()
    typedef enum { REF_CLASS_CLASS_NAME = 3,
                   REF_CLASS_DIRECT_SUPERCLASSES = 4,
                   REF_CLASS_DIRECT_SUBCLASSES = 5,
                   REF_CLASS_SLOTS = 6,
                   REF_CLASS_CLASS_PRECEDENCE_LIST = 7,
                   REF_CLASS_DIRECT_SLOTS = 8,
                   REF_CLASS_DIRECT_DEFAULT_INITARGS = 9,
                   REF_CLASS_DEFAULT_INITARGS = 10,
                   REF_CLASS_FINALIZED = 11,
                   REF_CLASS_SEALEDP = 14,
                   REF_CLASS_DEPENDENTS = 16,
                   REF_CLASS_LOCATION_TABLE = 19,
                   REF_CLASS_INSTANCE_STAMP = 20,
                   REF_CLASS_CREATOR = 21
    } Slots;
    
  public: // ctor/dtor for classes with shared virtual base
  Instance_O() : _Class(_Nil<Class_O>()), _Sig(_Nil<T_O>()){};
    explicit Instance_O(Class_sp metaClass) :
      _Class(metaClass)
      ,_Sig(_Unbound<T_O>())
#ifdef METER_ALLOCATIONS
      ,_allocation_counter(0)
      ,_allocation_total_size(0)
#endif
      
//    ,_NumberOfSlots(slots)
    {};
    virtual ~Instance_O(){};
  public:
    // The order MUST be:
    // _Sig
    // _Class (matches offset of FuncallableInstance_O)
    // _Rack  (matches offset of FuncallableInstance_O)
    T_sp _Sig;
    Class_sp _Class;
    SimpleVector_sp _Rack;
  /*! Mimicking ECL instance->sig generation signature
        This is pointed to the class slots in case they change 
        - then the instances can be updated*/
#ifdef METER_ALLOCATIONS
  // Keep track of allocations
    size_t _allocation_counter;
    size_t _allocation_total_size;
#endif
  public:
    static Instance_sp createClassUncollectable(gctools::Stamp is,Class_sp metaClass, size_t number_of_slots, Creator_sp creator);
    static Class_sp create(Symbol_sp symbol,Class_sp metaClass,Creator_sp creator);
  
  /*! Setup the instance nil value */
  //	void setupInstanceNil();

  public:
    virtual bool isCallable() const { return false; };
  public:
    // Functions from Class_O
    string _classNameAsString() const;
    void _setClassName(Symbol_sp id) { this->instanceSet(REF_CLASS_CLASS_NAME, id); };
    Symbol_sp _className() const { return gc::As<Symbol_sp>(this->instanceRef(REF_CLASS_CLASS_NAME)); }

    void CLASS_set_creator(Creator_sp cb);
    Creator_sp CLASS_get_creator() const { return gc::As_unsafe<Creator_sp>(this->instanceRef(REF_CLASS_CREATOR)); };
    bool CLASS_has_creator() const { return (bool)(!this->instanceRef(REF_CLASS_CREATOR).unboundp()); };
    Fixnum _get_instance_stamp() const { return this->instanceRef(REF_CLASS_INSTANCE_STAMP).unsafe_fixnum(); };
    
    string dumpInfo();

    virtual T_sp allocate_class(Class_sp metaClass, int slots);

  /*! Return the direct superclasses */
    List_sp directSuperclasses() const;

    void addInstanceBaseClass(Symbol_sp cl);

    T_sp slots() const { return this->instanceRef(REF_CLASS_SLOTS); };

    template <typename oclass>
      bool isSubClassOf() const {
    return this->isSubClassOf(lisp_classFromClassSymbol(oclass::static_classSymbol()));
  }

    void accumulateSuperClasses(HashTableEq_sp supers, VectorObjects_sp arrayedSupers, Class_sp mc);
    void lowLevel_calculateClassPrecedenceList();

    virtual bool isSubClassOf(Class_sp mc) const;

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
    void initializeSlots(gctools::Stamp is, size_t numberOfSlots);
    void initializeClassSlots(Creator_sp creator, gctools::Stamp class_stamp);
  public:
    static size_t rack_stamp_offset();
  protected:
    void reshapeInstance(int delta);
  public: // Functions here
    Fixnum stamp() const;
    void stamp_set(Fixnum s);
    size_t numberOfSlots() const;
  /*! Return number of slots if not nil otherwise nil */
    T_sp oinstancepSTAR() const;
  /*! Return number of slots if not nil otherwise nil */
    T_sp oinstancep() const;

    Class_sp _instanceClass() const { return this->_Class; };

    T_sp instanceClassSet(Class_sp mc);

    virtual T_sp instanceSigSet();
    virtual T_sp instanceSig() const;


    virtual bool equalp(T_sp obj) const;
    virtual void sxhash_(HashGenerator &hg) const;
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

#if 0
namespace core {
  FORWARD(FuncallableInstance);
};
template <>
struct gctools::GCInfo<core::FuncallableInstance_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};
#endif



namespace core {
#if 0
  class FuncallableInstance_O : public Instance_O {
    LISP_CLASS(core, CorePkg, FuncallableInstance_O, "FuncallableInstance",Instance_O);
  // These must be exposed in core__class_slot_sanity_check()
  public: // ctor/dtor for classes with shared virtual base
  FuncallableInstance_O() : _isgf(CLASP_NOT_FUNCALLABLE), _entryPoint(NULL) {};
    explicit FuncallableInstance_O(Class_sp metaClass) : Instance_O(metaClass) {};
    virtual ~FuncallableInstance_O(){};
  public:
    DispatchFunction_fptr_type _entryPoint;
    int _isgf;
  public:
    static Instance_sp createClassUncollectable(gctools::Stamp is,Class_sp metaClass, size_t number_of_slots, Creator_sp creator);
    static Class_sp create(Symbol_sp symbol,Class_sp metaClass,Creator_sp creator);
  
  /*! Setup the instance nil value */
  //	void setupInstanceNil();

  public:
    bool isCallable() const { return (bool)(this->_entryPoint); };
  public: // These indices MUST match the order in +standard-generic-function-slots+
    T_sp GFUN_NAME() const { return this->instanceRef(0); };
    T_sp GFUN_SPECIALIZERS() const { return this->instanceRef(1); };
    T_sp GFUN_COMB() const { return this->instanceRef(2); };
    T_sp GFUN_DISPATCHER() const { return this->instanceRef(3);};
    void GFUN_DISPATCHER_set(T_sp f)  { this->instanceSet(3,f);};
    T_sp GFUN_CALL_HISTORY() const { return this->instanceRef(4);};
    void GFUN_CALL_HISTORY_set(T_sp h);
    T_sp GFUN_LAMBDA_LIST() const { return this->instanceRef(5);};
    void GFUN_LAMBDA_LIST_set(T_sp lambda_list) {
      if (this->instanceRef(5).unboundp() && lambda_list.nilp()) {
        printf("%s:%d Ignoring GFUN_LAMBDA_LIST_SET - returning\n", __FILE__, __LINE__ );
        return;
      }
      this->instanceSet(5,lambda_list);
    };
  public:
  public:
  // Add support for Function_O methods
    CL_DEFMETHOD int isgf() const { return this->_isgf; };
    T_sp functionName() const { ASSERT(this->isgf()); return this->GFUN_NAME(); };
    virtual Symbol_sp functionKind() const { IMPLEMENT_ME(); };
    virtual T_sp closedEnvironment() const { IMPLEMENT_ME(); };
    virtual T_sp setSourcePosInfo(T_sp sourceFile, size_t filePos, int lineno, int column) { IMPLEMENT_ME(); };
//  virtual T_mv functionSourcePos() const { IMPLEMENT_ME();;
    virtual T_sp cleavir_ast() const { return _Nil<T_O>(); };
    virtual void setf_cleavir_ast(T_sp ast) { SIMPLE_ERROR(BF("Generic functions cannot be inlined"));};
    virtual List_sp declares() const { IMPLEMENT_ME(); };
    virtual T_sp docstring() const { IMPLEMENT_ME(); };
    virtual void *functionAddress() const { IMPLEMENT_ME(); };
    virtual bool macroP() const { return false; };
    virtual void set_kind(Symbol_sp k);
    virtual Symbol_sp getKind() const { return kw::_sym_function; };
    virtual int sourceFileInfoHandle() const { IMPLEMENT_ME(); };
    virtual size_t filePos() const { return 0; }
    virtual int lineNumber() const { return 0; }
    virtual int column() const { return 0; };
    virtual LambdaListHandler_sp lambdaListHandler() const { IMPLEMENT_ME(); };
    virtual void setAssociatedFunctions(List_sp funcs) { NOT_APPLICABLE(); };
  public: // The hard-coded indexes above are defined below to be used by Class
    void initializeSlots(gctools::Stamp is, size_t numberOfSlots);
    void initializeClassSlots(Creator_sp creator, gctools::Stamp class_stamp);
    void ensureClosure(DispatchFunction_fptr_type entryPoint);
    virtual void setf_lambda_list(List_sp lambda_list) { if (!this->_isgf) {SIMPLE_ERROR(BF("Cannot set lambda list of non gf function ll->%s") % _rep_(lambda_list));} this->GFUN_LAMBDA_LIST_set(lambda_list); }; //{ this->_lambda_list = lambda_list; };
    virtual T_sp lambda_list() const { return this->GFUN_LAMBDA_LIST(); };
  public:
    virtual void LISP_INVOKE();
    T_sp setFuncallableInstanceFunction(T_sp functionOrT);
    T_sp userFuncallableInstanceFunction() const;
    bool genericFunctionP() const;
    static  LCC_RETURN LISP_CALLING_CONVENTION();
    string __repr__() const;
    virtual T_sp copyInstance() const;
    virtual void describe(T_sp stream);

  }; // FuncallableInstance class
#endif
}; // core namespace

















namespace core {
  T_sp allocate_instance(Class_sp theClass, size_t numberOfSlots);

  T_sp core__allocate_raw_class(T_sp orig, Class_sp tMetaClass, int slots, bool creates_classes=true);

};


#endif /* _core_instance_H_ */
