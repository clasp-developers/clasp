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

/*! Different values for Instance_O.isgf */
#define CLASP_NOT_FUNCALLABLE 0
#define CLASP_NORMAL_DISPATCH 1
#define CLASP_INVALIDATED_DISPATCH 3

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
                   REF_GFUN_COMB = 2,          
                   REF_GFUN_LAMBDA_LIST = 3,   // lock
                   MIN_GFUN_SLOTS = 4 } GenericFunctionSlots;
  public: // ctor/dtor for classes with shared virtual base
    // entry_point is the LISP_CALLING_CONVENTION() macro
  FuncallableInstance_O(FunctionDescription* fdesc) : Base(not_funcallable_entry_point)
      , _isgf(CLASP_NOT_FUNCALLABLE)
      , _DebugOn(false)
      , _Class(_Nil<Instance_O>())
      , _Sig(_Nil<T_O>())
      , _FunctionDescription(fdesc)
      , _CallHistory(_Nil<T_O>())
      , _SpecializerProfile(_Nil<T_O>())
//      _Lock(mp::SharedMutex_O::make_shared_mutex(_Nil<T_O>())),
      , _CompiledDispatchFunction(_Nil<T_O>()) {};
    explicit FuncallableInstance_O(FunctionDescription* fdesc,Instance_sp metaClass, size_t slots) :
    Base(not_funcallable_entry_point),
      _Class(metaClass)
      ,_DebugOn(false)
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
    SimpleVector_sp _Rack;
    T_sp   _Sig;
    FunctionDescription* _FunctionDescription;
    gc::atomic_wrapper<T_sp>   _CallHistory;
    gc::atomic_wrapper<T_sp>   _SpecializerProfile;
//    T_sp   _Lock;
    gc::atomic_wrapper<T_sp>   _CompiledDispatchFunction;
    int    _isgf;
    bool   _DebugOn;
  public:
//    static FuncallableInstance_sp createClassUncollectable(gctools::Stamp is,Instance_sp metaClass, size_t number_of_slots, Creator_sp creator);
    static Instance_sp create(Symbol_sp symbol,Instance_sp metaClass,Creator_sp creator);
  public:
    T_sp GFUN_NAME() const { return this->instanceRef(REF_GFUN_NAME); };
    T_sp GFUN_SPECIALIZERS() const { return this->instanceRef(REF_GFUN_SPECIALIZERS); };
    T_sp GFUN_COMB() const { return this->instanceRef(REF_GFUN_COMB); };
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

    void accumulateSuperClasses(HashTableEq_sp supers, VectorObjects_sp arrayedSupers, Instance_sp mc);
    void lowLevel_calculateClassPrecedenceList();

//    virtual bool isSubClassOf(Instance_sp mc) const;

    FunctionDescription* fdesc() const { return this->_FunctionDescription; }
    T_sp make_instance();
  public:
  // Add support for Function_O methods
    T_sp functionName() const { ASSERT(this->isgf()); return this->GFUN_NAME(); };
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
    virtual LambdaListHandler_sp lambdaListHandler() const { HARD_IMPLEMENT_ME(); };
  public: // The hard-coded indexes above are defined below to be used by Class
    void initializeSlots(gctools::Stamp is, size_t numberOfSlots);
    void initializeClassSlots(Creator_sp creator, gctools::Stamp class_stamp);
    virtual void setf_lambda_list(List_sp lambda_list) { if (!this->_isgf) {SIMPLE_ERROR_SPRINTF("Cannot set lambda list of non gf function ll->%s", _rep_(lambda_list).c_str());} this->GFUN_LAMBDA_LIST_set(lambda_list); }; //{ this->_lambda_list = lambda_list; };
    virtual T_sp lambda_list() const { return this->GFUN_LAMBDA_LIST(); };
  public:
    static size_t rack_stamp_offset();
  public:
    virtual void LISP_INVOKE();

  public: // Functions here
    Fixnum stamp() const;
    void stamp_set(Fixnum s);
    size_t numberOfSlots() const;

    CL_DEFMETHOD int isgf() const { return this->_isgf; };

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

    void describe(T_sp stream);

    void __write__(T_sp sout) const; // Look in write_ugly.cc

    static LCC_RETURN invalidated_entry_point(LCC_ARGS_ELLIPSIS);
    static LCC_RETURN not_funcallable_entry_point(LCC_ARGS_ELLIPSIS);
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


#endif /* _core_instance_H_ */
