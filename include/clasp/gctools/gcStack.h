#pragma once

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

namespace gctools {

/*! Frame: A class that maintains an array of T_O* pointers on a thread-local stack for setting up calls.
This class always needs to be allocated on the stack.
It uses RAII to pop its array of pointers from the stack when the Frame goes out of scope.
*/
struct Frame {

  typedef core::T_O* ElementType;
  ElementType _args[];

  /* Calculate size of frame in bytes for __builtin_alloca
   */
  static inline size_t FrameBytes_(size_t elements) { return elements * sizeof(ElementType); }

  Frame(size_t numArguments) { this->debugEmptyFrame(numArguments); }

  ElementType* arguments(size_t start = 0) const { return ((ElementType*)&_args[0]) + start; }

  inline void mkunboundValue_(size_t idx) { this->_args[idx] = gctools::tag_unbound<core::T_O*>(); }

  inline ElementType value_(size_t idx) const { return this->_args[idx]; }

  //! Describe the Frame
  void dumpFrame(size_t nargs) const;
  void checkFrame(size_t idx, size_t nargs) const;
  void debugEmptyFrame(size_t nargs);
  inline core::T_sp arg(size_t idx) { return core::T_sp((gc::Tagged)this->_args[idx]); }
};

template <typename... Args> inline void fill_frame_templated(Frame* frame, size_t& idx, Args... args) {
  using InitialContents = core::T_O * [sizeof...(Args)];
  InitialContents* initialContents((InitialContents*)frame->arguments(idx));
  new (initialContents) InitialContents{args.raw_()...};
  idx += sizeof...(Args);
};

inline void fill_frame_one_indexed(Frame* frame, size_t idx, core::T_O* val) { frame->_args[idx] = val; }

inline void fill_frame_one(Frame* frame, size_t& idx, core::T_O* val) {
  frame->_args[idx] = val;
  idx++;
}

inline void fill_frame_nargs_args(Frame* frame, size_t& idx, size_t nargs, core::T_O** args) {
  memcpy((void*)frame->arguments(idx), (void*)args, sizeof(core::T_O*) * nargs);
  idx += nargs;
}

#define MAKE_STACK_FRAME(framename, num_elements)                                                                                  \
  gctools::Frame* framename = reinterpret_cast<gctools::Frame*>(__builtin_alloca(gctools::Frame::FrameBytes_(num_elements)));      \
  new (framename) gctools::Frame(num_elements);

#if 0
#define DEBUG_DUMP_FRAME(frame, nargs) frame->dump(nargs);
#else
#define DEBUG_DUMP_FRAME(frame, nargs)
#endif

#if DEBUG_FRAME() == 1
#define CHECK_FRAME(framename, _idx_, _nargs_)                                                                                     \
  if (_idx_ != _nargs_) {                                                                                                          \
    printf("%s:%d:%s FRAME not filled %lu should be %lu\n", __FILE__, __LINE__, __FUNCTION__, _idx_, _nargs_);                     \
    abort();                                                                                                                       \
  }
#elif DEBUG_FRAME() == 2
#define CHECK_FRAME(framename, _idx_, _nargs_) framename->checkFrame(_idx_, _nargs_)
#elif DEBUG_FRAME() == 0
#define CHECK_FRAME(framename, _idx_, _nargs_)
#else
#error "Bad DEBUG_FRAME() value"
#endif

} // namespace gctools

namespace core {

typedef gctools::smart_ptr<Vaslist> Vaslist_sp;
/*! Vaslist: A class that maintains a pointer to a vector of arguments and a number of arguments
to allow iteration over a list of arguments.
It must always be allocated on the Stack.
*/
struct Vaslist {
  /* WARNING WARNING WARNING WARNING
DO NOT CHANGE THE ORDER OF THESE OBJECTS WITHOUT UPDATING THE DEFINITION OF +vaslist+ in cmpintrinsics.lisp
*/
  mutable T_O** _Args;          // MUST be first slot
  mutable size_t _ShiftedNargs; // MUST be second slot

  // fixme2022 - make NargsShift = 2
  static constexpr size_t NargsShift = 2;
  static constexpr size_t NargsDecrement = 1 << NargsShift;
  static constexpr size_t NargsMask = 0;

#ifdef _DEBUG_BUILD
  inline void check_ShiftedNargs() const {
    if ((this->_ShiftedNargs & NargsMask) != 0 || this->_ShiftedNargs > (CALL_ARGUMENTS_LIMIT << NargsShift)) {
      printf("%s:%d  this->_ShiftedNargs has bad value %lu\n", __FILE__, __LINE__, this->_ShiftedNargs);
    }
  }
#else
  inline void check_nargs() const {};
#endif

  inline size_t nargs() const { return this->_ShiftedNargs >> NargsShift; };
  inline bool nargs_zero() const { return this->_ShiftedNargs == 0; };

  inline core::T_O* asTaggedPtr() { return gctools::tag_vaslist<core::T_O*>(this); }
  Vaslist(size_t nargs, gc::Frame* frame) : _Args(frame->arguments(0)), _ShiftedNargs(nargs << NargsShift) { this->check_nargs(); };

  Vaslist(size_t nargs, T_O** args) : _Args(args), _ShiftedNargs(nargs << NargsShift) { this->check_nargs(); };
  // The Vaslist._Args must be initialized immediately after this
  //    using va_start(xxxx._Args,FIRST_ARG)
  //    See lispCallingConvention.h INITIALIZE_VASLIST
  Vaslist(size_t nargs) : _ShiftedNargs(nargs << NargsShift) { this->check_nargs(); };
  Vaslist(const Vaslist& other) : _Args(other._Args), _ShiftedNargs(other._ShiftedNargs) { this->check_nargs(); }

  Vaslist(){};
  ~Vaslist() {}

  T_O* operator[](size_t index) { return this->_Args[index]; };

  inline core::T_sp next_arg() {
    core::T_sp obj((gctools::Tagged)(*this->_Args));
    this->_Args++;
    this->_ShiftedNargs -= NargsDecrement;
    return obj;
  }

  inline core::T_O* next_arg_raw() {
    core::T_O* obj = *this->_Args;
    this->_Args++;
    this->_ShiftedNargs -= NargsDecrement;
    return obj;
  }

  inline core::T_sp next_arg_indexed(size_t idx) {
    core::T_sp obj((gctools::Tagged)this->operator[](idx));
    return obj;
  }

  inline core::T_O** args() const { return this->_Args; }

  static T_O* make_shifted_nargs(size_t nargs) { return (T_O*)(nargs << NargsShift); }
  static size_t nargs_offset() { return offsetof(Vaslist, _ShiftedNargs); }
  static size_t args_offset() { return offsetof(Vaslist, _Args); }

  inline core::T_O* relative_indexed_arg(size_t idx) const { return this->_Args[idx]; }

  inline core::T_sp iarg(size_t idx) const {
    core::T_sp tsp((gctools::Tagged)this->_Args[idx]);
    return tsp;
  }
};
}; // namespace core

namespace gctools {
/*! Specialization of smart_ptr<T> on core::Vaslist
 */
template <> class smart_ptr<core::Vaslist> {
public:
  typedef core::Vaslist Type;
  Type* theObject;

public:
  // Default constructor, set theObject to NULL
  smart_ptr() : theObject((Type*)NULL){};
  explicit inline smart_ptr(core::Vaslist* ptr)
      : theObject((Type*)gctools::tag_vaslist<Type*>(ptr)){};
  /*! Create a smart pointer from an existing tagged pointer */
  explicit inline smart_ptr(Tagged ptr)
      : theObject((Type*)ptr){};

  inline Type* operator->() { return unsafe_valist(); };

  inline const Type* operator->() const { return unsafe_valist(); }

  inline Type& operator*() { return *unsafe_valist(); }

public:
  inline operator bool() { return this->theObject != NULL; };

public:
  inline bool nilp() const { return false; }
  inline bool notnilp() const { return true; };
  inline bool fixnump() const { return false; };
  inline bool generalp() const { return tagged_generalp(this->theObject); };
  inline bool consp() const { return false; };
  inline bool objectp() const { return this->generalp(); };
  inline Type* unsafe_valist() const { return untag_vaslist(this->theObject); };
  inline core::T_O* raw_() const { return reinterpret_cast<core::T_O*>(this->theObject); };
  inline gctools::Tagged tagged_() const { return reinterpret_cast<gctools::Tagged>(this->theObject); }
};

inline void fill_frame_vaslist(Frame* frame, size_t& idx, const core::Vaslist_sp vaslist) {
  core::Vaslist* vas = vaslist.unsafe_valist();
  fill_frame_nargs_args(frame, idx, vas->nargs(), vas->args());
}

}; // namespace gctools
