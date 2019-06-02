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
#include <clasp/core/object.h>
#include <clasp/core/activationFrame.fwd.h>
#include <clasp/core/loadTimeValues.fwd.h>
#include <clasp/core/environment.h>
#include <clasp/core/sequence.h>

// may need more later
//#include GC_INTERFACE_HEADER

namespace core {

/*! Types of ActivationFrame */
//    typedef enum { variables_frame=0, functions_frame=1, block_frame=2, tagbody_frame=3 } ActivationFrameType;

// TODO: ActivationFrame should adopt the _findValue behaviors of RuntimeVisibleEnvironment
// TODO: and it should inherit from Environment_O not RuntimeVisibleEnvironment_O
class ActivationFrame_O : public Environment_O {
  LISP_ABSTRACT_CLASS(core, CorePkg, ActivationFrame_O, "ActivationFrame",Environment_O);
 public:
  T_sp _Parent;
public:
  static string clasp_asString(T_sp af);
  T_sp &parentFrameRef_() { return this->_Parent; };
  T_sp parentFrame() const { return this->_Parent; };
public:
 ActivationFrame_O() : Base(), _Parent(_Nil<T_O>())
  {};
 ActivationFrame_O(T_sp p) : Base(), _Parent(p) {};
  virtual ~ActivationFrame_O(){};

  virtual T_sp *argArray() { SUBIMP(); };


  virtual size_t length() const { SUBIMP(); };
virtual T_sp currentVisibleEnvironment() const;
  virtual T_sp getActivationFrame() const;

//  virtual T_sp _lookupValue(int depth, int index);
//  virtual T_sp &lookupValueReference(int depth, int index);
//  virtual Function_sp _lookupFunction(int depth, int index) const;
//  virtual T_sp _lookupTagbodyId(int depth, int index) const;

  virtual bool _findTag(Symbol_sp tag, int &depth, int &index, bool &interFunction, T_sp &tagbodyEnv) const;
  virtual bool _findValue(T_sp sym, int &depth, int &index, bool& crossesFunction, ValueKind &valueKind, T_sp &value, T_sp& env) const;
  virtual bool _findFunction(T_sp functionName, int &depth, int &index, Function_sp &value, T_sp& functionEnv) const;

  /*! Methods for interogating ActivationFrames as Environments */
  T_sp getParentEnvironment() const { return this->parentFrame(); };
  /*! Method for interogating ActivationFrames as Environments */
  virtual string summaryOfContents() const;

  inline void setParentFrame(T_O *parent) {
    this->parentFrameRef_().rawRef_() = parent;
#ifdef DEBUG_ASSERT
    T_sp p((gctools::Tagged)parent);
    if (!(p.nilp() || p.asOrNull<Environment_O>()) ) {
      SIMPLE_ERROR_SPRINTF("Activation frame is not an activation frame - it is a %s", _rep_(p).c_str());
    }
#endif
  }
  inline void setParentFrame(T_sp p) {
    this->parentFrameRef_() = p;
  };
private:
  virtual string asString() const;

public:
  virtual string __repr__() const { return this->asString(); };
  virtual void dump() {
    string ts = this->asString();
    printf("%s\n", ts.c_str());
  };

  /*! Access a function */
  Function_sp function(int idx) const { throw_hard_error("Subclass must implement function(idx)"); };


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

namespace core {
class ValueFrame_O : public ActivationFrame_O {
  LISP_CLASS(core, CorePkg, ValueFrame_O, "ValueFrame",ActivationFrame_O);
  typedef T_sp value_type;
public:
  gctools::GCArray_moveable<value_type> _Objects;
public:
  template <class... ARGS>
  static ValueFrame_sp create_fill_capacity(int capacity, T_sp parent, ARGS &&... args) {
    ASSERT(sizeof...(ARGS) <= capacity);
    ValueFrame_sp vf = gc::GC<ValueFrame_O>::allocate_container(false,capacity,parent,std::forward<ARGS>(args)...);
    return vf;
  }

  static ValueFrame_sp create(int numArgs, const T_sp &parent) {
    ValueFrame_sp vf = create_fill_capacity(numArgs,parent);
    return vf;
  }

    static ValueFrame_sp createForMultipleValues(const T_sp &parent) {
    MultipleValues &mv = core::lisp_multipleValues();
    ValueFrame_sp vf = ValueFrame_O::create(mv.getSize(),parent);
    // TODO: This is used for all generic function calls - is there a better way than copying the ValueFrame??????
    for (int i(0); i < mv.getSize(); ++i) {
      (*vf)[i].setRaw_(reinterpret_cast<gc::Tagged>(mv[i]));
    }
    return vf;
  }

  static ValueFrame_sp create(List_sp values, T_sp parent);

  static ValueFrame_sp createFromReversedCons(List_sp values, T_sp parent);

  static ValueFrame_sp createForLambdaListHandler(LambdaListHandler_sp llh, T_sp parent);


 private:
  ValueFrame_O() = delete;
 public:
  template <typename...ARGS>
    ValueFrame_O(size_t capacity, /*const T_sp& initial_element,*/ T_sp parent, size_t initialContentsSize=0, T_sp* initialContents=NULL)
    : Base(parent)
    ,_Objects(capacity,_Unbound<T_O>(),true,initialContentsSize,initialContents) /*GCArray_moveable ctor*/ {};
  virtual ~ValueFrame_O(){
      //            printf("%s::%d dtor ValueFrame@%p\n", __FILE__, __LINE__, this);
  };
public:
public:
  inline T_sp &operator[](size_t idx) {
    BOUNDS_ASSERT(idx<this->_Objects._Length);
    return this->_Objects[idx];
  };
  inline const T_sp &operator[](size_t idx) const {
    BOUNDS_ASSERT(idx<this->_Objects._Length);
    return this->_Objects[idx];
  };

//  T_sp *argArray() { return this->_Objects.data(); };

  /*! Return the number of arguments */
  size_t length() const { return this->_Objects.length(); };

//  T_sp _lookupValue(int depth, int index);
//  T_sp &lookupValueReference(int depth, int index);

  virtual bool _updateValue(Symbol_sp sym, T_sp obj);
  virtual bool _findValue(T_sp sym, int &depth, int &index, bool& crossesFunction, ValueKind &valueKind, T_sp &value, T_sp& env) const;

  inline bool boundp_entry(uint idx) const {
    return !(*this)[idx].unboundp();
  }

  /*! Set one entry of the activation frame */
  void set_entry(uint idx, T_sp obj) {
    (*this)[idx] = obj;
  }

  inline T_sp entry(int idx) const {
    return (*this)[idx];
  }
  inline const T_sp &entryReference(int idx) const {
    return (*this)[idx];
  };

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
  LISP_CLASS(core, CorePkg, FunctionFrame_O, "FunctionFrame",ActivationFrame_O);
GCPRIVATE:
  typedef T_sp value_type;
  gctools::GCArray_moveable<value_type> _Objects;
public:
  static FunctionFrame_sp create(int numArgs, T_sp parent) {
    FunctionFrame_sp vf = gc::GC<FunctionFrame_O>::allocate_container(false,numArgs,parent);
    return vf;
  }

  static FunctionFrame_sp create(List_sp args, T_sp parent) {
    FunctionFrame_sp vf(FunctionFrame_O::create(cl__length(args), parent));
    //	    vf->allocateStorage(args->length());
    int idx = 0;
    for (auto cur : args) {
      (*vf)[idx] = oCar(cur);
      ++idx;
    }
    return vf;
  }

  template <class... ARGS>
  static FunctionFrame_sp create_fill(T_sp parent, ARGS &&... args) {
    FunctionFrame_sp vf = gc::GC<FunctionFrame_O>::allocate_container(false,sizeof...(ARGS),parent,std::forward<ARGS>(args)...);
    return vf;
  }
 private:
  FunctionFrame_O() = delete;
 public:
  template <typename...ARGS>
    FunctionFrame_O(size_t size, T_sp parent, ARGS && ...args) : Base(parent),_Objects(size,_Unbound<T_O>(),true,std::forward<ARGS>(args)...) {};
  /*! FunctionFrames must always be initialized with _Unbound !!!!! */
  virtual ~FunctionFrame_O() {}
public:
  /*! Return the number of arguments */
  size_t length() const { return this->_Objects.length(); };
  //	T_sp* argArray() { return this->_Objects.argArray(); };


  inline T_sp &operator[](size_t idx) {
    BOUNDS_ASSERT(idx<this->_Objects._Length);
    return this->_Objects[idx];
  };
  inline const T_sp &operator[](size_t idx) const {
    BOUNDS_ASSERT(idx<this->_Objects._Length);
    return this->_Objects[idx];
  };

  bool boundp_entry(uint idx) const {
    return !(*this)[idx].unboundp();
  }

  /*! Set one entry of the activation frame */
  void set_entry(uint idx, T_sp obj) {
    (*this)[idx] = obj;
  }
  T_sp entry(int idx) const { return (*this)[idx];};
  const T_sp &entryReference(int idx) const { return (*this)[idx];};
  T_sp &entryReference(int idx) { return (*this)[idx];};

  string asString() const;

  /*! Method for interogating ActivationFrames as Environments */
  virtual string summaryOfContents() const;

//  virtual Function_sp _lookupFunction(int depth, int index) const;
};
};

namespace core {
  void error_frame_range(const char* type, int index, int capacity );
  void error_end_of_frame_list(const char* message);

  inline ActivationFrame_sp value_frame_lookup(ActivationFrame_sp af, int depth)
  {
    while (true) {
      if (depth == 0 ) return af;
      --depth;
      af = gc::As<ActivationFrame_sp>(af->_Parent);
    }
  };
  
  inline T_sp& value_frame_lookup_reference(ActivationFrame_sp activationFrame, int depth, int index )
  {
    while (true) {
      if ( depth == 0 ) {
        ASSERT(activationFrame.isA<ValueFrame_O>());
        ValueFrame_sp vf = gc::reinterpret_cast_smart_ptr<ValueFrame_O,T_O>(activationFrame);
#ifdef DEBUG_ASSERT
        if ( index >= vf->_Objects.length() )
          error_frame_range("ValueFrame",index,vf->_Objects.length());
#endif
        return vf->_Objects[index];
      }
      --depth;
      activationFrame = gc::As<ActivationFrame_sp>(activationFrame->_Parent);
    }
  };

  inline T_sp& function_frame_lookup(ActivationFrame_sp activationFrame, int depth, int index )
  {
    while (true) {
      if ( depth == 0 ) {
        if (activationFrame.isA<FunctionFrame_O>()) {
          FunctionFrame_sp ff = gc::reinterpret_cast_smart_ptr<FunctionFrame_O,T_O>(activationFrame);
#ifdef DEBUG_ASSERT
          if ( index >= ff->_Objects.length() )
            error_frame_range("ValueFrame",index,ff->_Objects.length());
#endif
          return ff->_Objects[index];
        }
        error_end_of_frame_list("FunctionFrame");
      }
      --depth;
      activationFrame = gc::As<ActivationFrame_sp>(activationFrame->_Parent);
    }
  };

  DONT_OPTIMIZE_WHEN_DEBUG_RELEASE inline T_sp tagbody_frame_lookup(ActivationFrame_sp activationFrame, int depth, int index )
  {
    while (true) {
      if ( depth == 0 ) {
        if (activationFrame.isA<ValueFrame_O>()) {
          ValueFrame_sp tf = gc::reinterpret_cast_smart_ptr<ValueFrame_O,T_O>(activationFrame);
          return tf;
        }
        error_end_of_frame_list("TagbodyFrame");
      }
      --depth;
      activationFrame = gc::As<ActivationFrame_sp>(activationFrame->_Parent);
    }
  };

};

#endif
