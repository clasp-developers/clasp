/*
    File: gcStack.h
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
#ifndef gc_gcStack_H
#define gc_gcStack_H

namespace gctools {
#if 0
static const size_t IdxRegisterSaveArea = 0;
static const size_t IdxNumElements = LCC_NARGS_REGISTER;                                 // Where the num arguments (RAW - do not fix!!!)
static const size_t IdxOverflowArgs = LCC_TOTAL_REGISTERS;                               // IdxOverflowArgs-IdxRegisterSaveArea == Number of arguments passed in registers
static const size_t IdxRegisterArgumentsStart = IdxOverflowArgs - LCC_ARGS_IN_REGISTERS; // Where the register arguments start
static const size_t IdxValuesArray = IdxRegisterArgumentsStart;                          // where the stack based arguments start
 #endif
/*! Frame: A class that maintains an array of T_O* pointers on a thread-local stack for setting up calls.
This class always needs to be allocated on the stack.
It uses RAII to pop its array of pointers from the stack when the Frame goes out of scope.
*/

struct Frame {
  typedef core::T_O *ElementType;
  ElementType _register_save_area[LCC_ABI_ARGS_IN_REGISTERS];
  ElementType _overflow_area[];
  /*! Calculate the number of elements required to represent the frame.
     It's IdxValuesArray+#elements */
  static inline size_t FrameElements_(size_t frame_elements) {
    return std::max(LCC_ABI_ARGS_IN_REGISTERS,(int)frame_elements-LCC_ARGS_IN_REGISTERS+LCC_ABI_ARGS_IN_REGISTERS);
  }
  static inline size_t FrameBytes_(size_t elements) {
    return FrameElements_(elements) * sizeof(ElementType);
  }
  void* reg_save_area_ptr() const { return const_cast<void*>(reinterpret_cast<const void*>(&this->_register_save_area[0])); };
  void* overflow_arg_area_ptr() const { return const_cast<void*>(reinterpret_cast<const void*>(&this->_overflow_area[0])); };
  Frame(core::T_O* closure, size_t numArguments) {
    this->_register_save_area[LCC_CLOSURE_REGISTER] = closure;
//    this->_register_save_area[LCC_OVERFLOW_SAVE_REGISTER] = reinterpret_cast<core::T_O*>(&this->_overflow_area[0]);
    this->_register_save_area[LCC_NARGS_REGISTER] = reinterpret_cast<core::T_O*>(numArguments);
    for ( int i=(LCC_ABI_ARGS_IN_REGISTERS-LCC_ARGS_IN_REGISTERS); i<LCC_ABI_ARGS_IN_REGISTERS; ++i ) {
      this->_register_save_area[i] = gctools::tag_unbound<core::T_O*>();
    }
    int num_overflow_args = (int)numArguments - LCC_ARGS_IN_REGISTERS;
    for ( int j=0; j<num_overflow_args; ++j ) {
      this->_overflow_area[j] = gctools::tag_unbound<core::T_O*>();
    }
  }

  inline size_t number_of_arguments() const {
    return reinterpret_cast<size_t>(this->_register_save_area[LCC_NARGS_REGISTER]);
  }

  inline void set_number_of_arguments(size_t num) {
    this->_register_save_area[LCC_NARGS_REGISTER] = reinterpret_cast<core::T_O*>(num);
  }

  inline ElementType& operator[](size_t idx) {
    GCTOOLS_ASSERTF(idx >= 0 && idx < this->number_of_arguments(),"idx out of bounds");
    // This works because the overflow area follows the register save area
    return this->_register_save_area[idx+LCC_ARG0_REGISTER];
  }
  inline const ElementType& operator[](size_t idx) const {
    GCTOOLS_ASSERTF(idx >= 0 && idx < this->number_of_arguments(),"idx out of bounds");
    // This works because the overflow area follows the register save area
    return this->_register_save_area[idx+LCC_ARG0_REGISTER];
  }
  //! Describe the Frame
  void dump() const;
  inline core::T_sp arg(size_t idx) { return core::T_sp((gc::Tagged) this->operator[](idx)); }
};

#define MAKE_STACK_FRAME( framename, closure, num_elements)                                                                                                        \
  gctools::Frame *framename = reinterpret_cast<gctools::Frame *>(__builtin_alloca(gctools::Frame::FrameBytes_(num_elements))); \
  new(framename) gctools::Frame(closure, num_elements);

} // namespace gctools

namespace core {

// A struct that wraps va_list and behaves like a Common Lisp LIST
typedef gctools::smart_ptr<Vaslist> VaList_sp;
/*! Vaslist: A class that maintains a C va_list and allows the programmer to
iterate over a list of arguments.  It uses a lot of trickery to let it iterate over
a list of arguments passed to a function or a list of arguments in a Frame.
It must always be allocated on the Stack.
*/
struct Vaslist {
  /* WARNING WARNING WARNING WARNING
DO NOT CHANGE THE ORDER OF THESE OBJECTS WITHOUT UPDATING THE DEFINITION OF +va_list+ in cmpintrinsics.lsp
*/
  mutable va_list _Args;
  mutable size_t  _remaining_nargs;

#ifdef _DEBUG_BUILD
  inline void check_remaining_nargs() const {
    if (this->_remaining_nargs >CALL_ARGUMENTS_LIMIT) {
      printf("%s:%d  this->_remaining_nargs has bad value %lu\n", __FILE__, __LINE__, this->_remaining_nargs);
    }
  }
#else
  inline void check_remaining_nargs() const {};
#endif

  inline core::T_O *asTaggedPtr() {
    return gctools::tag_vaslist<core::T_O *>(this);
  }
  Vaslist(gc::Frame* frame) {
    LCC_SETUP_VA_LIST_FROM_FRAME(this->_Args, *frame);
    this->_remaining_nargs = frame->number_of_arguments();
    this->check_remaining_nargs();
  };

  Vaslist(const gc::Frame& frame) {
    LCC_SETUP_VA_LIST_FROM_FRAME(this->_Args, frame);
    this->_remaining_nargs = frame.number_of_arguments();
    this->check_remaining_nargs();
  };

  Vaslist(size_t nargs, va_list vargs) {
    va_copy(this->_Args, vargs);
    this->_remaining_nargs = nargs;
    this->check_remaining_nargs();
  };
  // The Vaslist._Args must be initialized immediately after this
  //    using va_start(xxxx._Args,FIRST_ARG)
  //    See lispCallingConvention.h INITIALIZE_VA_LIST
  Vaslist(size_t nargs) {
    this->_remaining_nargs = nargs;
    this->check_remaining_nargs();
  };
  Vaslist(const Vaslist &other) {
    va_copy(this->_Args, other._Args);
    this->_remaining_nargs = other._remaining_nargs;
    this->check_remaining_nargs();
  }

  Vaslist(){};

  ~Vaslist() {
    va_end(this->_Args);
  }

#if 0  
  void set_from_other_Vaslist_change_nargs(Vaslist *other, size_t nargs_remaining) {
    LCC_SETUP_VA_LIST_FROM_VA_LIST_CHANGE_NARGS(this->_Args, other->_Args, nargs_remaining);
  }
#endif
  void set_from_other_Vaslist(Vaslist *other) {
    LCC_SETUP_VA_LIST_FROM_VA_LIST(this->_Args, other->_Args);
    this->_remaining_nargs = other->_remaining_nargs;
    this->check_remaining_nargs();
  }

  inline size_t total_nargs() const {
    size_t n = LCC_VA_LIST_TOTAL_NUMBER_OF_ARGUMENTS(this);
    return n;
  }



  inline const size_t& remaining_nargs() const {
    return this->_remaining_nargs;
  }
  inline size_t& remaining_nargs() {
    return this->_remaining_nargs;
  }
#if 0
  inline size_t current_index() const {
    printf("%s:%d  implement-me\n", __FILE__, __LINE__ );
    size_t idx;
    LCC_VA_LIST_CURRENT_INDEX(idx,this);
    return idx;
  }
#endif
  inline core::T_O* next_arg_raw() {
    --this->_remaining_nargs;
 #ifdef _DEBUG_BUILD
    this->check_remaining_nargs();
 #endif
    return LCC_NEXT_ARG_RAW_AND_ADVANCE(this);
  }

  inline core::T_sp next_arg() {
    --this->_remaining_nargs;
#ifdef _DEBUG_BUILD
    this->check_remaining_nargs();
#endif
    T_O* ptr = LCC_NEXT_ARG_RAW_AND_ADVANCE(this);
    return T_sp(reinterpret_cast<gctools::Tagged>(ptr));
  }

  inline core::T_O *relative_indexed_arg(size_t idx) const {
    core::T_O *res;
    LCC_VA_LIST_RELATIVE_INDEXED_ARG(res, this, idx);
    return res;
  }
};
};

namespace gctools {
/*! Specialization of smart_ptr<T> on core::Vaslist
*/
template <>
  class smart_ptr<core::Vaslist> { // : public tagged_ptr<core::Vaslist> {
public:
  typedef core::Vaslist Type;
  Type* theObject;
public:
  //Default constructor, set theObject to NULL
 smart_ptr() : theObject((Type*)NULL){};
  explicit inline smart_ptr(core::Vaslist *ptr) : theObject((Type*)gctools::tag_vaslist<Type *>(ptr)) {
//    GCTOOLS_ASSERT(this->valistp());
  };
  /*! Create a smart pointer from an existing tagged pointer */
  explicit inline smart_ptr(Tagged ptr) : theObject((Type*)ptr) {
//    GCTOOLS_ASSERT(this->theObject == NULL || this->valistp());
  };

  inline Type *operator->() {
//    GCTOOLS_ASSERT(this->valistp());
    return reinterpret_cast<Type *>(this->unsafe_valist());
  };

  inline const Type *operator->() const {
//    GCTOOLS_ASSERT(this->valistp());
    return reinterpret_cast<Type *>(this->unsafe_valist());
  };

  inline Type &operator*() {
//    GCTOOLS_ASSERT(this->valistp());
    return *reinterpret_cast<Type *>(this->unsafe_valist());
  };

  public:
  inline operator bool() { return this->theObject != NULL; };
public:
  inline bool nilp() const { return tagged_nilp(this->theObject); }
  inline bool notnilp() const { return (!this->nilp()); };
  inline bool fixnump() const { return tagged_fixnump(this->theObject); };
  inline bool generalp() const { return tagged_generalp(this->theObject); };
  inline bool consp() const { return tagged_consp(this->theObject); };
  inline bool objectp() const { return this->generalp() || this->consp(); };
  inline Type* unsafe_valist() const { return reinterpret_cast<Type*>(untag_vaslist(this->theObject)); };
  inline core::T_O *raw_() const { return reinterpret_cast<core::T_O *>(this->theObject); };
  inline gctools::Tagged tagged_() const { return reinterpret_cast<gctools::Tagged>(this->theObject); }

};
};

#endif
