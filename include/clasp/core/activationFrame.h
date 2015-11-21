/*
    File: activationFrame.h
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
#ifndef core_ActivationFrame_H
#define core_ActivationFrame_H

//#define DEBUG_FRAME

#include <alloca.h>
#include <utility>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/activationFrame.fwd.h>
#include <clasp/core/loadTimeValues.fwd.h>
#include <clasp/core/environment.h>
#include <clasp/core/holder.h>

// may need more later
//#include GC_INTERFACE_HEADER

namespace core {

/*! Types of ActivationFrame */
//    typedef enum { variables_frame=0, functions_frame=1, block_frame=2, tagbody_frame=3 } ActivationFrameType;

// TODO: ActivationFrame should adopt the _findValue behaviors of RuntimeVisibleEnvironment
// TODO: and it should inherit from Environment_O not RuntimeVisibleEnvironment_O
class ActivationFrame_O : public Environment_O // RuntimeVisibleEnvironment_O
                          {
  LISP_BASE1(Environment_O); // RuntimeVisibleEnvironment_O);
  LISP_VIRTUAL_CLASS(core, CorePkg, ActivationFrame_O, "ActivationFrame");

protected:
public:
  static string clasp_asString(T_sp af);

public:
  ActivationFrame_O() : Base(){};
  virtual ~ActivationFrame_O(){};

  virtual T_sp *argArray() { SUBIMP(); };

  virtual T_sp &operator[](int idx) { SUBIMP(); };
  virtual const T_sp &operator[](int idx) const { SUBIMP(); };

  virtual T_sp currentVisibleEnvironment() const;
  virtual T_sp getActivationFrame() const;

  virtual T_sp _lookupValue(int depth, int index);
  virtual T_sp &lookupValueReference(int depth, int index);
  virtual Function_sp _lookupFunction(int depth, int index) const;
  virtual T_sp _lookupTagbodyId(int depth, int index) const;

  virtual bool _findTag(Symbol_sp tag, int &depth, int &index, bool &interFunction, T_sp &tagbodyEnv) const;
  virtual bool _findValue(T_sp sym, int &depth, int &index, ValueKind &valueKind, T_sp &value) const;
  virtual bool _findFunction(T_sp functionName, int &depth, int &index, Function_sp &value) const;

  virtual T_sp &parentFrameRef() { SUBIMP(); };
  virtual T_sp parentFrame() const { SUBIMP(); };

  /*! Methods for interogating ActivationFrames as Environments */
  T_sp getParentEnvironment() const { return this->parentFrame(); };
  /*! Method for interogating ActivationFrames as Environments */
  virtual string summaryOfContents() const;

  inline void setParentFrame(T_O *parent) {
    this->parentFrameRef().rawRef_() = parent;
  }
  inline void setParentFrame(T_sp parent) { this->parentFrameRef() = parent; };
  /*! Return the number of arguments */
  virtual uint length() const { SUBIMP(); };

  virtual bool boundp_entry(uint idx) const { SUBIMP(); }

  /*! Set one entry of the activation frame */
  virtual void set_entry(uint idx, T_sp obj) { SUBIMP(); }

  virtual T_sp entry(int idx) const { SUBIMP(); }
  virtual const T_sp &entryReference(int idx) const { SUBIMP(); }

private:
  virtual string asString() const;

public:
  virtual string __repr__() const { return this->asString(); };
  virtual void dump() {
    string ts = this->asString();
    printf("%s\n", ts.c_str());
  };

  /*! Access a function */
  virtual Function_sp function(int idx) const { THROW_HARD_ERROR(BF("Subclass must implement function(idx)")); };

  List_sp asCons(int start = 0) const {
    _G();
    Cons_sp dummy = Cons_O::create(_Nil<T_O>());
    Cons_sp cur = dummy;
    for (int i = start; i < (int)this->length(); i++) {
      Cons_sp one = Cons_O::create(this->entry(i));
      cur->setCdr(one);
      cur = one;
    }
    return coerce_to_list(oCdr(dummy));
  }

}; // ActivationFrame class
}; // core namespace

template <>
struct gctools::GCInfo<core::ValueFrame_O> {
  static bool const NeedsInitialization = false;
  static bool const NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
  //  static bool const InlineScan = true;
  //  static bool const Roo
};
TRANSLATE(core::ActivationFrame_O);

namespace core {
class ValueFrame_O : public ActivationFrame_O {
  LISP_BASE1(ActivationFrame_O);
  LISP_CLASS(core, CorePkg, ValueFrame_O, "ValueFrame");
GCPROTECTED:
  T_sp _ParentFrame;
  gctools::Frame0<T_sp> _Objects;
  //IndirectObjectArray             _Objects;
  core::T_sp _DebuggingInfo;

public:
  static ValueFrame_sp createForMultipleValues(const T_sp &parent) {
    _G();
    GC_ALLOCATE(ValueFrame_O, vf);
    MultipleValues &mv = core::lisp_callArgs();
    vf->allocate(mv.getSize());
    // TODO: This is used for all generic function calls - is there a better way than copying the ValueFrame??????
    for (int i(0); i < mv.getSize(); ++i) {
      vf->_Objects[i].setRaw_(reinterpret_cast<gc::Tagged>(mv[i]));
    }
    vf->_ParentFrame = parent;
#if 0
	    vf->_OwnArgs = false;
	    vf->_NumArgs = nargs;
	    vf->_Args = argArray;
#endif
    return vf;
  }
  static ValueFrame_sp create(T_sp parent) {
    _G();
    GC_ALLOCATE(ValueFrame_O, vf);
    vf->_ParentFrame = parent;
    return vf;
  }

  static ValueFrame_sp create(int numArgs, const T_sp &parent) {
    _G();
    GC_ALLOCATE(ValueFrame_O, vf);
    vf->_ParentFrame = parent;
    vf->allocate(numArgs);
    //	    vf->allocateStorage(numArgs);
    return vf;
  }

  static ValueFrame_sp create(List_sp values, T_sp parent);

  static ValueFrame_sp createFromReversedCons(List_sp values, T_sp parent);

  static ValueFrame_sp createForLambdaListHandler(LambdaListHandler_sp llh, T_sp parent);

  template <class... ARGS>
  static ValueFrame_sp create_fill_args(T_sp parent, ARGS &&... args) {
    _G();
    GC_ALLOCATE(ValueFrame_O, vf);
    vf->_ParentFrame = parent;
    vf->allocate(0, std::forward<ARGS>(args)...);
    return vf;
  }

  template <class... ARGS>
  static ValueFrame_sp create_fill_numExtraArgs(int numExtraArgs, T_sp parent, ARGS &&... args) {
    _G();
    GC_ALLOCATE(ValueFrame_O, vf);
    vf->_ParentFrame = parent;
    vf->allocate(numExtraArgs, std::forward<ARGS>(args)...);
    return vf;
  }

  ValueFrame_O() : Base(), _Objects(), _DebuggingInfo(_Nil<T_O>()){};

#if 0
        /*! ValueFrames must always be initialized with _Unbound !!!!! */
        template <class...ARGS>
        ValueFrame_O(size_t numExtraArgs, ARGS&&...args) : _Objects(numExtraArgs,_Unbound<T_O>(),std::forward<ARGS>(args)...), _DebuggingInfo(_Nil<T_O>())
        {
//            printf("%s::%d ctor ValueFrame@%p\n", __FILE__, __LINE__, this);
        };
#endif

  virtual ~ValueFrame_O(){
      //            printf("%s::%d dtor ValueFrame@%p\n", __FILE__, __LINE__, this);
  };

private:
  template <class... ARGS>
  void allocate(size_t numExtraArgs, ARGS &&... args) {
    this->_Objects.allocate(numExtraArgs, _Unbound<T_O>(), std::forward<ARGS>(args)...);
  };

public:
  virtual T_sp &parentFrameRef() { return this->_ParentFrame; };
  virtual T_sp parentFrame() const { return this->_ParentFrame; };

  inline void attachDebuggingInfo(core::T_sp debuggingInfo) {
    this->_DebuggingInfo = debuggingInfo;
  }

  core::T_sp debuggingInfo() const {
    return this->_DebuggingInfo;
  }

public:
  virtual T_sp &operator[](int idx);
  virtual const T_sp &operator[](int idx) const;

  T_sp *argArray() { return this->_Objects.data(); };

  /*! Return the number of arguments */
  virtual uint length() const { return this->_Objects.capacity(); };

  T_sp _lookupValue(int depth, int index);
  T_sp &lookupValueReference(int depth, int index);

  virtual bool _updateValue(Symbol_sp sym, T_sp obj);
  virtual bool _findValue(T_sp sym, int &depth, int &index, ValueKind &valueKind, T_sp &value) const;

  bool boundp_entry(uint idx) const {
    return !this->_Objects[idx].unboundp();
  }

  /*! Set one entry of the activation frame */
  void set_entry(uint idx, T_sp obj) {
    _G();
    this->_Objects[idx] = obj;
  }

  T_sp entry(int idx) const {
    _G();
    return this->_Objects[idx];
  }

#if 0
	virtual T_sp& entryReference(int idx) const
	{_G();
            return this->_Objects.entryReference(idx);
	}
#endif
  /*! Fill the activation frame starting at entry istart with values.
	  DO NOT OVERFLOW THE ValueFrame!!!! */
  void fillRestOfEntries(int istart, List_sp values);

  /*! Method for interogating ActivationFrames as Environments */
  virtual string summaryOfContents() const;

  string asString() const;
};
};

template <>
struct gctools::GCInfo<core::FunctionFrame_O> {
  static bool const NeedsInitialization = false;
  static bool const NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {
class FunctionFrame_O : public ActivationFrame_O {
  LISP_BASE1(ActivationFrame_O);
  LISP_CLASS(core, CorePkg, FunctionFrame_O, "FunctionFrame");
GCPRIVATE:
  T_sp _ParentFrame;
  gctools::Frame0<T_sp> _Objects;
  //        IndirectObjectArray     _Objects;
public:
#if 0
	inline void allocateStorage(int numArgs)
	{_G();
	    this->_Objects.allocateStorage(numArgs);
	}
#endif
private:
  template <class... ARGS>
  void allocate(size_t numExtraArgs, ARGS &&... args) {
    this->_Objects.allocate(numExtraArgs, _Unbound<T_O>(), std::forward<ARGS>(args)...);
  };

public:
  static FunctionFrame_sp create(T_sp parent) {
    _G();
    GC_ALLOCATE(FunctionFrame_O, vf);
    vf->_ParentFrame = parent;
    return vf;
  }

  static FunctionFrame_sp create(int numArgs, T_sp parent) {
    _G();
    GC_ALLOCATE(FunctionFrame_O, vf);
    vf->_ParentFrame = parent;
    vf->allocate(numArgs);
    return vf;
  }

  static FunctionFrame_sp create(List_sp args, T_sp parent) {
    _G();
    FunctionFrame_sp vf(FunctionFrame_O::create(cl_length(args), parent));
    //	    vf->allocateStorage(args->length());
    int idx = 0;
    for (auto cur : args) {
      vf->operator[](idx) = oCar(cur); // n e w (&(vf->_Args[idx])) T_sp(oCar(cur));
      ++idx;
    }
    return vf;
  }

  template <class... ARGS>
  static FunctionFrame_sp create_fill(T_sp parent, ARGS &&... args) {
    _G();
    GC_ALLOCATE(FunctionFrame_O, vf);
    vf->_ParentFrame = parent;
    vf->allocate(0, std::forward<ARGS>(args)...);
    return vf;
  }

  FunctionFrame_O() : Base(), _Objects(){};

/*! FunctionFrames must always be initialized with _Unbound !!!!! */
#if 0
        template <typename...ARGS>
	FunctionFrame_O(size_t numExtraArgs, ARGS...args)
            : _Objects(numExtraArgs,_Unbound<T_O>(),args...), Base() {};
#endif
  virtual ~FunctionFrame_O() {}

public:
  virtual T_sp &parentFrameRef() { return this->_ParentFrame; };
  virtual T_sp parentFrame() const { return this->_ParentFrame; };

  /*! Return the number of arguments */
  virtual uint length() const { return this->_Objects.capacity(); };
  //	T_sp* argArray() { return this->_Objects.argArray(); };

  bool boundp_entry(uint idx) const {
    _G();
    return !this->_Objects[idx].unboundp();
  }

  /*! Set one entry of the activation frame */
  void set_entry(uint idx, T_sp obj) {
    _G();
    this->_Objects[idx] = obj;
  }
  T_sp entry(int idx) const;                 // Return by reference for efficiency?
  const T_sp &entryReference(int idx) const; // Return by reference for efficiency?

  string asString() const;

  /*! Method for interogating ActivationFrames as Environments */
  virtual string summaryOfContents() const;

  virtual Function_sp _lookupFunction(int depth, int index) const;
};
};

namespace core {
class TagbodyFrame_O : public ActivationFrame_O {
  LISP_BASE1(ActivationFrame_O);
  LISP_CLASS(core, CorePkg, TagbodyFrame_O, "TagbodyFrame");
GCPRIVATE:
  T_sp _ParentFrame;

public:
  static TagbodyFrame_sp create(T_sp parent);

  virtual T_sp &parentFrameRef() { return this->_ParentFrame; };
  virtual T_sp parentFrame() const { return this->_ParentFrame; };
  virtual string summaryOfContents() const;

  T_sp _lookupTagbodyId(int depth, int index) const;

  TagbodyFrame_O() : Base(){};
  virtual ~TagbodyFrame_O(){};

public:
  string asString() const;
};
};
template <>
struct gctools::GCInfo<core::TagbodyFrame_O> {
  static bool const NeedsInitialization = false;
  static bool const NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

#if 0
namespace frame {
  typedef core::T_O *ElementType;
  typedef gctools::smart_ptr<core::STACK_FRAME> FrameType_sp;

  static const size_t IdxRegisterSaveArea = 0;
  static const size_t IdxNumElements = IdxRegisterSaveArea; // Where the num arguments (RAW - do not fix!!!)
  static const size_t IdxOverflowArgs = 6; // IdxOverflowArgs-IdxRegisterSaveArea == Number of arguments passed in registers
  static const size_t IdxRegisterArgumentsStart = IdxOverflowArgs-LCC_ARGS_IN_REGISTERS; // Where the register arguments start
  static const size_t IdxValuesArray = IdxRegisterArgumentsStart;            // where the stack based arguments start

  struct Frame {
    ElementType* _frameImpl;
    
    static
    /*! Calculate the number of elements required to represent the frame.
     It's IdxValuesArray+#elements */
    static inline size_t FrameSize(size_t elements) {
      return (elements+IdxOverflowArgs) - LCC_ARGS_IN_REGISTERS;
    }

    Frame(size_t numArguments,core::T_sp parent = _Nil<core::T_O>()) {
      size_t sz = FrameSize(numArguments)*sizeof(ElementType);
      this->_frameImpl = reinterpret_cast<ElementType*>(core::lisp_threadLocalStack().pushFrame(sz));
      this->_frameImpl[IdxNumElements] = reinterpret_cast<core::T_O*>(numArguments);
      for (size_t i(IdxValuesArray), iEnd(IdxValuesArray + numArguments); i < iEnd; ++i) {
        this->_frameImpl[i] = gctools::tag_unbound<core::T_O *>();
      }
    }

    ~Frame() {
      core::lisp_threadLocalStack().popFrame(reinterpret_cast<void*>(this->_frameImpl));
    }

    ElementType& operator[](size_t idx) {
      return this->_frameImpl[IdxValuesArray+idx];
    }

    core::T_sp arg(size_t idx) { return core::T_sp((gc::Tagged)this->operator[](idx));}
    
    inline core::VaList_sp setupVaList(core::VaList_S& args) {
      args._Args[0].gp_offset = (frame::IdxRegisterArgumentsStart-frame::IdxRegisterSaveArea)*sizeof(frame::ElementType);
      args._Args[0].fp_offset = 304;
      args._Args[0].reg_save_area = &this->_frameImpl[frame::IdxRegisterSaveArea];
      args._Args[0].overflow_arg_area = &this->_frameImpl[frame::IdxOverflowArgs];
      return core::VaList_sp(&args);
    }
    
  };
};
#endif

// Return the next argument of the frame
//#define FRAME_ARG(frame) va_arg(frame.unsafe_frame()->args,ElementType)
//#define FRAME_END(frame) 0 // use va_end!!!
#endif
