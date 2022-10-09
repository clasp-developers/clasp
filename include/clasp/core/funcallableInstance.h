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
  FORWARD(SingleDispatchMethod);
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
    // This structure is known to the compiler,
    // in src/lisp/kernel/cmp/cmpintrinsics.lisp.
    // Changes to the structure here must be reflected there.
  public: // ctor/dtor for classes with shared virtual base
  FuncallableInstance_O(GlobalEntryPoint_sp ep) :
      Base(ep)
      , _Class(nil<Instance_O>())
    , _CompiledDispatchFunction(nil<T_O>()) {}
    explicit FuncallableInstance_O(GlobalEntryPoint_sp fdesc,Instance_sp metaClass, size_t slots) :
        Base(fdesc)
        , _Class(metaClass)
        , _CompiledDispatchFunction(nil<T_O>())
    {};
    FuncallableInstance_O(GlobalEntryPoint_sp fdesc, Instance_sp cl, Rack_sp rack)
        : Base(fdesc),
          _Class(cl),
          _Rack(rack),
          _CompiledDispatchFunction(nil<T_O>())
    {};
    virtual ~FuncallableInstance_O(){};
  public:
    // The order MUST be
    // entry (inherited from Function_O, matches offset)
    // _Rack (matches offset in Instance_O)
    Rack_sp _Rack;
    Instance_sp _Class;
    std::atomic<size_t> _InterpretedCalls;
    std::atomic<T_sp>   _CompiledDispatchFunction;
  public:

    T_sp GFUN_DISPATCHER() const { return this->_CompiledDispatchFunction.load(); };
    void GFUN_DISPATCHER_set(T_sp val) { this->_CompiledDispatchFunction.store(val); };
  public:

    virtual bool compiledP() const { return true; };
    void accumulateSuperClasses(HashTableEq_sp supers, ComplexVector_T_sp arrayedSupers, Instance_sp mc);
    void lowLevel_calculateClassPrecedenceList();

//    virtual bool isSubClassOf(Instance_sp mc) const;
    T_sp make_instance();
  public:
  // Add support for Function_O methods
//  virtual T_mv functionSourcePos() const { HARD_IMPLEMENT_ME();;
    virtual List_sp declares() const { HARD_IMPLEMENT_ME(); };
    virtual size_t filePos() const { return 0; }
    virtual int lineNumber() const { return 0; }
    virtual int column() const { return 0; };
  public: // The hard-coded indexes above are defined below to be used by Class
    void initializeSlots(gctools::BaseHeader_s::StampWtagMtag is, T_sp sig, size_t numberOfSlots);
    void initializeClassSlots(Creator_sp creator, gctools::BaseHeader_s::StampWtagMtag class_stamp);
  public:
    static size_t rack_stamp_offset();

  public: // Functions here
    Rack_sp rack() const { return this->_Rack; }
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

    T_sp setFuncallableInstanceFunction(T_sp functionOrT);

    size_t increment_calls () { return this->_InterpretedCalls++; }
    size_t interpreted_calls () { return this->_InterpretedCalls; }

    void describe(T_sp stream);

    void __write__(T_sp sout) const; // Look in write_ugly.cc

    static inline LCC_RETURN LISP_CALLING_CONVENTION() {
      SETUP_CLOSURE(FuncallableInstance_O,closure);
      INCREMENT_FUNCTION_CALL_COUNTER(closure);
      DO_DRAG_CXX_CALLS();
      // We need to be sure to load the GFUN_DISPATCHER only once.
      // We used to load it twice, which caused a race condition in that other threads
      // could call setFuncallableInstanceFunction between the loads, meaning we called
      // the code for one function but pass it the closure object for another.
      T_sp funcallable_closure = closure->GFUN_DISPATCHER();
      // This is where we could decide to compile the dtree and switch the GFUN_DISPATCHER() or not
      //  printf("%s:%d:%s About to call %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(closure->functionName()).c_str());
      return funcall_general<core::Function_O>( funcallable_closure.tagged_(), lcc_nargs, lcc_args );
    }
      static inline LISP_ENTRY_0() {
    return entry_point_n(lcc_closure,0,NULL);
  }
  static inline LISP_ENTRY_1() {
    core::T_O* args[1] = {lcc_farg0};
    return entry_point_n(lcc_closure,1,args);
  }
  static inline LISP_ENTRY_2() {
    core::T_O* args[2] = {lcc_farg0,lcc_farg1};
    return entry_point_n(lcc_closure,2,args);
  }
  static inline LISP_ENTRY_3() {
    core::T_O* args[3] = {lcc_farg0,lcc_farg1,lcc_farg2};
    return entry_point_n(lcc_closure,3,args);
  }
  static inline LISP_ENTRY_4() {
    core::T_O* args[4] = {lcc_farg0,lcc_farg1,lcc_farg2,lcc_farg3};
    return entry_point_n(lcc_closure,4,args);
  }
  static inline LISP_ENTRY_5() {
    core::T_O* args[5] = {lcc_farg0,lcc_farg1,lcc_farg2,lcc_farg3,lcc_farg4};
    return entry_point_n(lcc_closure,5,args);
  }

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

namespace core {
void registerOrDumpDtreeInfo(std::ostream& fout);
extern size_t global_compile_discriminating_function_trigger;

};

#endif /* _core_funcallable_instance_H_ */
