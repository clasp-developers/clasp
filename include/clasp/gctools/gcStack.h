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
namespace frame {
typedef core::T_O *ElementType;
static const size_t IdxRegisterSaveArea = 0;
static const size_t IdxNumElements = LCC_NARGS_REGISTER;                                 // Where the num arguments (RAW - do not fix!!!)
static const size_t IdxOverflowArgs = LCC_TOTAL_REGISTERS;                               // IdxOverflowArgs-IdxRegisterSaveArea == Number of arguments passed in registers
static const size_t IdxRegisterArgumentsStart = IdxOverflowArgs - LCC_ARGS_IN_REGISTERS; // Where the register arguments start
static const size_t IdxValuesArray = IdxRegisterArgumentsStart;                          // where the stack based arguments start
/*! Frame: A class that maintains an array of T_O* pointers on a thread-local stack for setting up calls.
This class always needs to be allocated on the stack.
It uses RAII to pop its array of pointers from the stack when the Frame goes out of scope.
*/

struct Frame {
  size_t _ArrayLength;
  size_t _ElementCapacity; // May be larger than length
  ElementType *_frameBlock;
  /*! Calculate the number of elements required to represent the frame.
     It's IdxValuesArray+#elements */
  static inline size_t FrameElements(size_t frame_elements) {
    return std::max((frame_elements + IdxOverflowArgs) - LCC_ARGS_IN_REGISTERS, IdxOverflowArgs + 1);
  }
  static inline size_t FrameBytes(size_t elements) {
    return FrameElements(elements) * sizeof(ElementType);
  }
#ifdef USE_ALLOCA_FOR_FRAME
  Frame(ElementType *buffer, size_t numArguments);
#else
  Frame(size_t numArguments);
#endif

  inline ElementType &lowLevelElementRef(size_t idx) {
    GCTOOLS_ASSERT(idx >= 0 && idx < this->_ElementCapacity);
    return this->_frameBlock[idx];
  }
  inline const ElementType &lowLevelElementRef(size_t idx) const {
    GCTOOLS_ASSERT(idx >= 0 && idx < this->_ElementCapacity);
    return this->_frameBlock[idx];
  }
  inline void setLength(size_t l) { this->_ArrayLength = l; };
  inline size_t getLength() const { return this->_ArrayLength; };
  //! Describe the Frame
  void dump() const;
  ~Frame();
  inline ElementType &operator[](size_t idx) { return this->lowLevelElementRef(idx + IdxValuesArray); }
  inline core::T_sp arg(size_t idx) { return core::T_sp((gc::Tagged) this->lowLevelElementRef(idx + IdxValuesArray)); }
};
};

#ifdef USE_ALLOCA_FOR_FRAME
#define STACK_FRAME(buffername, framename, num_elements)                                                                                                        \
  gctools::frame::ElementType *buffername = reinterpret_cast<gctools::frame::ElementType *>(__builtin_alloca(gctools::frame::Frame::FrameBytes(num_elements))); \
  gctools::frame::Frame framename(buffername, num_elements);
#else
#define STACK_FRAME(buffername, framename, num_elements) gctools::frame::Frame framename(num_elements);
#endif

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
  core::T_O *asTaggedPtr() {
    return gctools::tag_valist<core::T_O *>(this);
  }
  VaList_S(gc::frame::Frame &frame) {
    LCC_SETUP_VA_LIST_FROM_FRAME(this->_Args, frame);
#if 0
      // This must match (and should be in) lispCallingConvention.h
      this->_Args[0].reg_save_area = &frame.lowLevelElementRef(gc::frame::IdxRegisterSaveArea);
      this->_Args[0].overflow_arg_area = &frame.lowLevelElementRef(gc::frame::IdxOverflowArgs);
      // This is where the number of arguments remaining should be stored
      ((uintptr_t*)(this->_Args[0].reg_save_area))[LCC_NARGS_REGISTER] = frame._ArrayLength;
      ((uintptr_t*)(this->_Args[0].reg_save_area))[LCC_OVERFLOW_SAVE_REGISTER] = (uintptr_t)(this->_Args[0].overflow_arg_area);
      this->_Args[0].gp_offset = (gc::frame::IdxRegisterArgumentsStart-gc::frame::IdxRegisterSaveArea)*sizeof(gc::frame::ElementType);
      this->_Args[0].fp_offset = 304;
#endif
  };

  VaList_S(int nargs, va_list vargs) {
    va_copy(this->_Args, vargs);
  };
  VaList_S(const VaList_S &other) {
    va_copy(this->_Args, other._Args);
  }

  VaList_S(){};

  void set(VaList_S *other, size_t nargs_remaining) {
    LCC_SETUP_VA_LIST_FROM_VA_LIST(this->_Args, other->_Args, nargs_remaining);
  }
  virtual ~VaList_S(){}; // Make it polymorphic
  inline size_t nargs() const { return LCC_raw_VA_LIST_NUMBER_OF_ARGUMENTS(this->_Args); };
  inline core::T_O *indexed_arg(size_t idx) const {
    core::T_O *res;
    LCC_VA_LIST_INDEXED_ARG(res, *this, idx);
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
