#ifndef gctools_threadLocalStacks_H
#define gctools_threadLocalStacks_H

#include <clasp/core/clasp_gmpxx.h>
//#include <clasp/core/mpPackage.fwd.h>
namespace core {
  struct BignumExportBuffer {
  BignumExportBuffer() : buffer(NULL), bufferSize(0){};
    ~BignumExportBuffer() {
      if (this->buffer)
        free(this->buffer);
    };
    unsigned int *buffer = NULL;
    size_t bufferSize = 0;
    unsigned int *getOrAllocate(const mpz_class &bignum, int nail);
  };
};




namespace core {
  class DynamicBinding {
  public:
    Symbol_sp _Var;
    T_sp _Val;
  DynamicBinding(Symbol_sp sym, T_sp val) : _Var(sym), _Val(val){};
  };
#pragma GCC visibility push(default)
  class DynamicBindingStack {
  public:
    gctools::Vec0<DynamicBinding> _Bindings;
    mutable gctools::Vec0<T_sp>           _ThreadLocalBindings;
  public:
    size_t new_binding_index();
    void release_binding_index(size_t index);
    inline size_t top() const { return this->_Bindings.size() - 1; }
    Symbol_sp topSymbol() const { return this->_Bindings.back()._Var; };
    Symbol_sp var(size_t i) const { return this->_Bindings[i]._Var; };
    T_sp val(size_t i) const { return this->_Bindings[i]._Val; };
    ATTR_WEAK void push_with_value_coming(Symbol_sp var,T_sp* globalValuePtr);
    ATTR_WEAK void push_binding(Symbol_sp var, T_sp* globalValuePtr, T_sp value=_Unbound<T_O>());
    // Push the current value of the symbol onto the DynamicBindingStack
    //   The new value will follow immediately
    ATTR_WEAK void pop_binding();
    void reserve(size_t x) { this->_Bindings.reserve(x); };
    size_t size() const { return this->_Bindings.size(); };
    void expandThreadLocalBindings(size_t index);
    // Dynamic symbol access
    /*! Return a pointer to the value slot for the symbol.  
        USE THIS IMMEDIATELY AND THEN DISCARD.
        DO NOT DO STORE THIS OR KEEP THIS FOR ANY LENGTH OF TIME. */
    T_sp* reference_raw_(Symbol_O* varP, T_sp* globalValuePtr);
    const T_sp* reference_raw_(const Symbol_O* varP, const T_sp* globalValuePtr) const;
    const T_sp* reference_raw(const Symbol_O* varP, const T_sp* globalValuePtr) const { return const_cast<const T_sp*>(this->reference_raw_(const_cast<Symbol_O*>(varP),globalValuePtr));};
    T_sp* reference_raw(Symbol_O* varP,T_sp* globalValuePtr) { return this->reference_raw_(varP,globalValuePtr);};
    T_sp* reference(Symbol_sp var,T_sp* globalValuePtr) { return const_cast<T_sp*>(this->reference_raw(&*var,globalValuePtr));};
    const T_sp* reference(Symbol_sp var, T_sp* globalValuePtr) const { return this->reference_raw(&*var, globalValuePtr);};
    T_sp  value(Symbol_sp var, T_sp* globalValuePtr) const { return *this->reference(var,globalValuePtr);};
//    void  setf_value(Symbol_sp var, T_sp value) { *this->reference(var) = value;};
  };
#pragma GCC visibility pop
};


namespace core {
/*! Exception stack information */

  typedef enum { NullFrame,
                 CatchFrame,
                 BlockFrame,
                 TagbodyFrame,
                 LandingPadFrame } FrameKind;
/*! Store the information for the exception 
      For CatchThrow:   _Obj1
    */
  class ExceptionEntry {
  public:
  ExceptionEntry() : _FrameKind(NullFrame), _Key(_Nil<T_O>()){};
  ExceptionEntry(FrameKind k, T_sp key) : _FrameKind(k), _Key(key){};
    FrameKind _FrameKind;
    T_sp _Key;
  };
  class ExceptionStack {
  public:
    gctools::Vec0<ExceptionEntry> _Stack;
  public:
    ExceptionEntry &operator[](int i) { return this->_Stack[i]; };
    size_t size() const { return this->_Stack.size(); };
    string summary() {
      std::stringstream ss;
      ss << "ExceptionStackSummary: depth[" << this->size() << "] ";
      for (int idx = this->size() - 1; idx >= 0; --idx) {
        FrameKind fk = this->_Stack[idx]._FrameKind;
        char frameChar;
        switch (fk) {
        case NullFrame:
            frameChar = 'N';
            break;
        case CatchFrame:
            frameChar = 'C';
            break;
        case BlockFrame:
            frameChar = 'B';
            break;
        case TagbodyFrame:
            frameChar = 'T';
            break;
        case LandingPadFrame:
            frameChar = 'L';
            break;
        default:
            frameChar = 'u';
            break;
        }
        ss << frameChar << idx;
        if (this->_Stack[idx]._Key.notnilp()) {
          ss << "{" << _rep_(this->_Stack[idx]._Key) << "@" << (void *)this->_Stack[idx]._Key.raw_() << "}";
        }
        ss << " ";
      };
      return ss.str();
    };
    void validateFrame(size_t frame) {
      if (frame >= this->_Stack.size()) {
        printf("%s:%d A request to unwind to frame %lu has been made but there are only %lu frames on the exception stack - the frame won't be found and a crash will occur - aborting now.  Trap abort() in the debugger to investigate\n", __FILE__, __LINE__, frame, this->_Stack.size());
        abort();
      }
    }
    inline size_t push(FrameKind kind, T_sp key) {
      size_t frame = this->_Stack.size();
      this->_Stack.emplace_back(kind, key);
      return frame;
    }
    inline void pop() {
      this->_Stack.pop_back();
    };
  /*! Return the index of the stack entry with the matching key.
          If return -1 then the key wasn't found */
    int findKey(FrameKind kind, T_sp key);
    T_sp backKey() const { return this->_Stack.back()._Key; };
    void unwind(size_t newTop) { this->_Stack.resize(newTop); };
    Vector_sp backtrace();
  };
};



#endif
