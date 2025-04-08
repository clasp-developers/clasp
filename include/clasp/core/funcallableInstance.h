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
#include <clasp/core/mpPackage.h>

// may need more later
#include <clasp/gctools/gc_interface.h>

namespace core {
FORWARD(FuncallableInstance);
FORWARD(SingleDispatchMethod);
}; // namespace core

template <> struct gctools::GCInfo<core::FuncallableInstance_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

#define GF_SINGLE_DISPATCH_SPECIALIZER_CALL_HISTORY 0
#define GF_SINGLE_DISPATCH_SPECIALIZER_DISPATCH_ARGUMENT_INDEX 1
#define GF_SINGLE_DISPATCH_SPECIALIZER_METHODS 2
#define GF_SINGLE_DISPATCH_SPECIALIZER_SLOTS 3

namespace core {

class FuncallableInstance_O : public Function_O {
  LISP_CLASS(core, CorePkg, FuncallableInstance_O, "FuncallableInstance", Function_O);
  // This structure is known to the compiler,
  // in src/lisp/kernel/cmp/cmpintrinsics.lisp.
  // Changes to the structure here must be reflected there.
public: // ctor/dtor for classes with shared virtual base
  FuncallableInstance_O(SimpleFun_sp ep) : Base(ep), _Class(nil<Instance_O>()), _RealFunction(nil<Function_O>()) {}
  explicit FuncallableInstance_O(SimpleFun_sp ep, Instance_sp metaClass, size_t slots)
      : Base(ep), _Class(metaClass), _RealFunction(nil<Function_O>()){};
  FuncallableInstance_O(SimpleFun_sp ep, Instance_sp cl, Rack_sp rack)
      : Base(ep), _Rack(rack), _Class(cl), _RealFunction(nil<Function_O>()){};

public:
  // The order MUST be
  // _TheSimpleFun (inherited from Function_O, matches offset)
  // _Rack (matches offset in Instance_O)
  // <other stuff>
  Rack_sp _Rack;
  Instance_sp _Class;
  std::atomic<size_t> _InterpretedCalls;
  std::atomic<Function_sp> _RealFunction;

public:
  Function_sp REAL_FUNCTION() const { return this->_RealFunction.load(); };
  void REAL_FUNCTION_set(Function_sp val) { this->_RealFunction.store(val); };

public:
  virtual bool compiledP() const { return true; };
  void accumulateSuperClasses(HashTable_sp supers, ComplexVector_T_sp arrayedSupers, Instance_sp mc);
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

  size_t increment_calls() { return this->_InterpretedCalls++; }
  size_t interpreted_calls() { return this->_InterpretedCalls; }

  void describe(T_sp stream);

  void __write__(T_sp sout) const; // Look in write_ugly.cc

  static inline LCC_RETURN entry_point_n(core::T_O* lcc_closure, size_t lcc_nargs, core::T_O** lcc_args) {
    SETUP_CLOSURE(FuncallableInstance_O, closure);
    DO_DRAG_CXX_CALLS();
    // We need to be sure to load the REAL_FUNCTION only once to avoid race conditions.
    Function_sp funcallable_closure = closure->REAL_FUNCTION();
    // This is where we could decide to compile the dtree and switch the REAL_FUNCTION() or not
    //  printf("%s:%d:%s About to call %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(closure->functionName()).c_str());
    return funcallable_closure->apply_raw(lcc_nargs, lcc_args);
  }

  template <typename... Ts>
  static inline LCC_RETURN entry_point_fixed(core::T_O* lcc_closure, Ts... args) {
    SETUP_CLOSURE(FuncallableInstance_O, closure);
    DO_DRAG_CXX_CALLS();
    // We need to be sure to load the REAL_FUNCTION only once to avoid race conditions.
    Function_sp funcallable_closure = closure->REAL_FUNCTION();
    return funcallable_closure->funcall_raw(args...);
  }

}; // FuncallableInstance class

}; // namespace core

namespace gctools {
/*! Specialize TaggedCast for FuncallableInstance_O - always use dynamic_cast */
template <typename FROM> struct TaggedCast<core::FuncallableInstance_O*, FROM> {
  typedef core::FuncallableInstance_O* ToType;
  typedef FROM FromType;
  inline static bool isA(FromType ptr) {
    if (tagged_generalp(ptr)) {
      // Maybe
      core::General_O* raw_client = (core::General_O*)untag_general<FromType>(ptr);
      core::FuncallableInstance_O* iptr = dynamic_cast<core::FuncallableInstance_O*>(raw_client);
      return iptr != NULL;
    }
    return false;
  }
  inline static core::FuncallableInstance_O* castOrNULL(FromType client) {
    if (tagged_generalp(client)) {
      // maybe
      core::General_O* raw_client = (core::General_O*)untag_general<FromType>(client);
      core::FuncallableInstance_O* iclient = dynamic_cast<core::FuncallableInstance_O*>(raw_client);
      if (iclient)
        return tag_general<ToType>(iclient);
      return NULL;
    }
    return NULL;
  }
};
}; // namespace gctools

namespace core {

// Fulfill the role of bytecode_function
FORWARD(GFBytecodeSimpleFun);
class GFBytecodeSimpleFun_O : public SimpleFun_O {
  LISP_CLASS(core, CorePkg, GFBytecodeSimpleFun_O, "GFBytecodeSimpleFun", SimpleFun_O);

public:
  // Store the bytecode in the _Code slot
  // Entry point into the bytes vector in the containing module.
  // This is an offset instead of an interior pointer to make dumping/loading/GC considerations easier.
  unsigned int _EntryPcN;
  BytecodeTrampolineFunction _Trampoline;
  SimpleVector_byte8_t_sp _Bytecode;
  SimpleVector_sp _Literals;
  Function_sp _GenericFunction;
  // Number of specialized parameters. entry_point_n has to do an argcount
  // check with this at runtime.
  size_t _SpecializedLength;

public:
  // Accessors
  GFBytecodeSimpleFun_O(FunctionDescription_sp fdesc, unsigned int entryPcN, SimpleVector_byte8_t_sp bytecode,
                        SimpleVector_sp literals, Function_sp generic_function, size_t specialized_length);

public:
  static GFBytecodeSimpleFun_sp make(Function_sp generic_function);

public:
  virtual Pointer_sp defaultEntryAddress() const;
  CL_LISPIFY_NAME(GFBytecodeSimpleFun/bytecode)
  CL_DEFMETHOD SimpleVector_byte8_t_sp bytecode() const { return this->_Bytecode; }
  CL_LISPIFY_NAME(GFBytecodeSimpleFun/literals)
  CL_DEFMETHOD SimpleVector_sp literals() const { return this->_Literals; }
  string __repr__() const;

  virtual void fixupInternalsForSnapshotSaveLoad(snapshotSaveLoad::Fixup* fixup);
  size_t entryPcN() const;
  inline size_t specializedLength() const { return this->_SpecializedLength; }
};

}; // namespace core
