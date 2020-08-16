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
    // This structure is known to the compiler,
    // in src/lisp/kernel/cmp/cmpintrinsics.lsp.
    // Changes to the structure here must be reflected there.
  public: // ctor/dtor for classes with shared virtual base
    // entry_point is the LISP_CALLING_CONVENTION() macro
  FuncallableInstance_O(FunctionDescription* fdesc) :
    Base(funcallable_entry_point)
      , _Class(_Nil<Instance_O>())
      , _FunctionDescription(fdesc)
      , _CompiledDispatchFunction(_Nil<T_O>()) {};
    explicit FuncallableInstance_O(FunctionDescription* fdesc,Instance_sp metaClass, size_t slots) :
    Base(funcallable_entry_point),
      _Class(metaClass)
      , _FunctionDescription(fdesc)
      , _CompiledDispatchFunction(_Nil<T_O>())
    {};
    FuncallableInstance_O(FunctionDescription* fdesc, Instance_sp cl, Rack_sp rack)
      : Base(funcallable_entry_point),
        _Class(cl),
        _Rack(rack),
        _FunctionDescription(fdesc),
        _CompiledDispatchFunction(_Nil<T_O>())
    {};
    virtual ~FuncallableInstance_O(){};
  public:
    // The order MUST be
    // entry (inherited from Function_O, matches offset)
    // _Rack (matches offset in Instance_O)
    Rack_sp _Rack;
    Instance_sp _Class;
    FunctionDescription* _FunctionDescription;
    std::atomic<size_t>        _InterpretedCalls;
    std::atomic<T_sp>   _CompiledDispatchFunction;
  public:

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
    T_sp functionName() const { return this->fdesc()->functionName(); }
    virtual T_sp closedEnvironment() const { HARD_IMPLEMENT_ME(); };
//  virtual T_mv functionSourcePos() const { HARD_IMPLEMENT_ME();;
    virtual List_sp declares() const { HARD_IMPLEMENT_ME(); };
    virtual size_t filePos() const { return 0; }
    virtual int lineNumber() const { return 0; }
    virtual int column() const { return 0; };
    virtual T_sp lambdaListHandler() const { HARD_IMPLEMENT_ME(); };
  public: // The hard-coded indexes above are defined below to be used by Class
    void initializeSlots(gctools::ShiftedStamp is, T_sp sig, size_t numberOfSlots);
    void initializeClassSlots(Creator_sp creator, gctools::ShiftedStamp class_stamp);
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

#endif /* _core_funcallable_instance_H_ */
