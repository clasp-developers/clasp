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
    this->_register_save_area[LCC_OVERFLOW_SAVE_REGISTER] = reinterpret_cast<core::T_O*>(&this->_overflow_area[0]);
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
    GCTOOLS_ASSERTF(idx >= 0 && idx < this->number_of_arguments(),BF("idx = %d  number_of_arguments=%d") % idx % this->number_of_arguments());
    // This works because the overflow area follows the register save area
    return this->_register_save_area[idx+LCC_ARG0_REGISTER];
  }
  inline const ElementType& operator[](size_t idx) const {
    GCTOOLS_ASSERTF(idx >= 0 && idx < this->number_of_arguments(),BF("idx = %d  number_of_arguments=%d") % idx % this->number_of_arguments());
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
typedef gctools::smart_ptr<VaList_S> VaList_sp;
/*! VaList_S: A class that maintains a C va_list and allows the programmer to
iterate over a list of arguments.  It uses a lot of trickery to let it iterate over
a list of arguments passed to a function or a list of arguments in a Frame.
It must always be allocated on the Stack.
*/
struct VaList_S {
  /* WARNING WARNING WARNING WARNING
DO NOT CHANGE THE ORDER OF THESE OBJECTS WITHOUT UPDATING THE DEFINITION OF +va_list+ in cmpintrinsics.lsp
*/
  mutable va_list _Args;
  inline core::T_O *asTaggedPtr() {
    return gctools::tag_valist<core::T_O *>(this);
  }
  VaList_S(gc::Frame* frame) {
    LCC_SETUP_VA_LIST_FROM_FRAME(this->_Args, *frame);
  };

  VaList_S(const gc::Frame& frame) {
    LCC_SETUP_VA_LIST_FROM_FRAME(this->_Args, frame);
  };

  VaList_S(int nargs, va_list vargs) {
    va_copy(this->_Args, vargs);
  };
  VaList_S(const VaList_S &other) {
    va_copy(this->_Args, other._Args);
  }

  VaList_S(){};

#if 0  
  void set_from_other_VaList_S_change_nargs(VaList_S *other, size_t nargs_remaining) {
    LCC_SETUP_VA_LIST_FROM_VA_LIST_CHANGE_NARGS(this->_Args, other->_Args, nargs_remaining);
  }
#endif
  void set_from_other_VaList_S(VaList_S *other) {
    LCC_SETUP_VA_LIST_FROM_VA_LIST(this->_Args, other->_Args);
  }

  virtual ~VaList_S(){}; // Make it polymorphic
  inline size_t total_nargs() const {
    size_t n = LCC_VA_LIST_TOTAL_NUMBER_OF_ARGUMENTS(this);
    return n;
  }
  inline size_t remaining_nargs() const {
    size_t n;
    LCC_VA_LIST_REMAINING_NUMBER_OF_ARGUMENTS(n,this);
    return n;
  }
  inline size_t current_index() const {
    size_t idx;
    LCC_VA_LIST_CURRENT_INDEX(idx,this);
    return idx;
  }
  inline core::T_O* next_arg_raw() {
    return LCC_NEXT_ARG_RAW_AND_ADVANCE(this);
  }

  inline core::T_sp next_arg() {
    T_O* ptr = LCC_NEXT_ARG_RAW_AND_ADVANCE(this);
    return T_sp(reinterpret_cast<gctools::Tagged>(ptr));
  }

  inline core::T_O *absolute_indexed_arg(size_t idx) const {
    core::T_O *res;
    LCC_VA_LIST_ABSOLUTE_INDEXED_ARG(res, this, idx);
    return res;
  }
};
};

namespace gctools {
/*! Specialization of smart_ptr<T> on core::VaList_S
*/
template <>
class smart_ptr<core::VaList_S> : public tagged_ptr<core::VaList_S> {
public:
  typedef core::VaList_S Type;

public:
  //Default constructor, set theObject to NULL
  smart_ptr() : tagged_ptr<Type>(){};
  explicit inline smart_ptr(core::VaList_S *ptr) : tagged_ptr<Type>((Tagged)gctools::tag_valist<Type *>(ptr)) {
    GCTOOLS_ASSERT(this->valistp());
  };
  /*! Create a smart pointer from an existing tagged pointer */
  explicit inline smart_ptr(Tagged ptr) : tagged_ptr<Type>((Tagged)ptr) {
    GCTOOLS_ASSERT(this->theObject == NULL || this->valistp());
  };

  inline Type *operator->() {
    GCTOOLS_ASSERT(this->valistp());
    return reinterpret_cast<Type *>(this->unsafe_valist());
  };

  inline const Type *operator->() const {
    GCTOOLS_ASSERT(this->valistp());
    return reinterpret_cast<Type *>(this->unsafe_valist());
  };

  inline Type &operator*() {
    GCTOOLS_ASSERT(this->valistp());
    return *reinterpret_cast<Type *>(this->unsafe_valist());
  };
};
};

#endif
