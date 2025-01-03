/*
    File: lispCallingConvention.h
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

// FIXME: This file is pretty much three separate headers,
// conditionalized by LCC_MACROS, LCC_PROTOTYPES, and LCC_FUNCALL.
// This file is #included in three places with the three different
// conditions defined.

#ifdef LCC_MACROS

namespace gctools {
struct return_type {
#if (LCC_RETURN_VALUES_IN_REGISTERS() != 1)
#error "The number of return values in registers does not match core::return_type"
#endif
  void* ret0[LCC_RETURN_VALUES_IN_REGISTERS()]; // One for every LCC_RETURN_VALUES_IN_REGISTERS
  size_t nvals;

  return_type() : ret0{NULL}, nvals(0){};
  return_type(core::T_O* r0, size_t nv) : ret0{r0}, nvals(nv){};
  template <typename T> return_type(T* r0, size_t nv) : ret0{reinterpret_cast<void*>(r0)}, nvals(nv){};
};

}; // namespace gctools
// Return raw values that can be used to construct a core::T_mv
#define LCC_RETURN_RAW gctools::return_type
#define LCC_RETURN gctools::return_type

extern "C" {

LCC_RETURN_RAW general_entry_point_redirect_0(core::T_O* closure);

LCC_RETURN_RAW general_entry_point_redirect_1(core::T_O* closure, core::T_O* farg0);

LCC_RETURN_RAW general_entry_point_redirect_2(core::T_O* closure, core::T_O* farg0, core::T_O* farg1);

LCC_RETURN_RAW general_entry_point_redirect_3(core::T_O* closure, core::T_O* farg0, core::T_O* farg1, core::T_O* farg2);

LCC_RETURN_RAW general_entry_point_redirect_4(core::T_O* closure, core::T_O* farg0, core::T_O* farg1, core::T_O* farg2,
                                              core::T_O* farg3);

LCC_RETURN_RAW general_entry_point_redirect_5(core::T_O* closure, core::T_O* farg0, core::T_O* farg1, core::T_O* farg2,
                                              core::T_O* farg3, core::T_O* farg4);

LCC_RETURN_RAW general_entry_point_redirect_6(core::T_O* closure, core::T_O* farg0, core::T_O* farg1, core::T_O* farg2,
                                              core::T_O* farg3, core::T_O* farg4, core::T_O* farg5);

LCC_RETURN_RAW general_entry_point_redirect_7(core::T_O* closure, core::T_O* farg0, core::T_O* farg1, core::T_O* farg2,
                                              core::T_O* farg3, core::T_O* farg4, core::T_O* farg5, core::T_O* farg6);
};

#define MULTIPLE_VALUES_ARRAY core::lisp_multipleValues()

/*! This is X86_64 dependent code */
#if defined(__x86_64__) || defined(__aarch64__)

// This is VERY HACKISH
// it's based on the System V Application Binary Interface for X86_64
// I'm writing the register arguments into the reg_save_area and then
// resetting the gp_offset to point to the first register argument lcc_fixed_arg0
#define LCC_ABI_ARGS_IN_REGISTERS 6

#define ASSERT_LCC_VASLIST_AT_START(_valist_s_)                                                                                    \
  ASSERT((_valist_s_)._Args->gp_offset == sizeof(uintptr_t) * (LCC_ABI_ARGS_IN_REGISTERS - LCC_ARGS_IN_REGISTERS));

// Registers are %rdi, %rsi, %rdx, %rcx, %r8, %r9
#define LCC_CLOSURE_REGISTER 0
#define LCC_NARGS_REGISTER 1
#define LCC_ARGS_PTR_REGISTER 2
#define LCC_ARGS_PASSED_IN_REGISTERS LCC_ARGS_IN_REGISTERS
#define LCC_TOTAL_REGISTERS LCC_ABI_ARGS_IN_REGISTERS
#define LCC_ARG0_REGISTER 2
#define LCC_SPILL_NUMBER_ARGUMENTS_TO_VASLIST(_valist_s_, _num_)                                                                   \
  { ((uintptr_t*)(_valist_s_)._Args->reg_save_area)[LCC_NARGS_REGISTER] = (uintptr_t)(_num_); }

// Create a Vaslist_sp from lcc_vargs (a vaslist)
// #define LCC_MAKE_VASLIST_SP(valist_sp) Vaslist_sp valist_sp(lcc_nargs,lcc_vargs);
#endif
#endif // #ifdef LCC_MACROS

#define SETUP_CLOSURE_(Type, var) Type* var = gctools::untag_general<Type*>((Type*)lcc_closure);
#ifdef _DEBUG_BUILD
#define SETUP_CLOSURE(Type, var)                                                                                                   \
  if (!gc::TaggedCast<Type*, core::T_O*>::isA(lcc_closure)) {                                                                      \
    SIMPLE_ERROR("Bad cast of closure {} to type: {}", (void*)lcc_closure, #Type);                                                 \
  }                                                                                                                                \
  SETUP_CLOSURE_(Type, var);
#else
#define SETUP_CLOSURE(Type, var) SETUP_CLOSURE_(Type, var)
#endif

#ifdef LCC_PROTOTYPES

#include <atomic>

// This function type is basically a lie - core functions can have any
// function type.
typedef LCC_RETURN_RAW (*ClaspCoreFunction)();
// This type is also a lie - we use this to hold ClaspXepGeneralFunction,
// ClaspXep0Function, etc. below.
// As long as we know what we're doing and don't try to actually call a
// function while treating it as this type we're ok (see C++ standard on
// reinterpret_cast, I think). Ditto for ClaspCoreFunction.
typedef LCC_RETURN_RAW (*ClaspXepAnonymousFunction)();
#define LISP_CALLING_CONVENTION() entry_point_n(core::T_O* lcc_closure, size_t lcc_nargs, core::T_O** lcc_args)
typedef LCC_RETURN_RAW (*BytecodeTrampolineFunction)(unsigned char* pc, core::T_O* lcc_closure, size_t lcc_nargs,
                                                     core::T_O** lcc_args);
typedef LCC_RETURN_RAW (*ClaspXepGeneralFunction)(core::T_O* lcc_closure, size_t lcc_nargs, core::T_O** lcc_args);
typedef LCC_RETURN_RAW (*ClaspXep0Function)(core::T_O* lcc_closure);
typedef LCC_RETURN_RAW (*ClaspXep1Function)(core::T_O* lcc_closure, core::T_O* farg0);
typedef LCC_RETURN_RAW (*ClaspXep2Function)(core::T_O* lcc_closure, core::T_O* farg0, core::T_O* farg1);
typedef LCC_RETURN_RAW (*ClaspXep3Function)(core::T_O* lcc_closure, core::T_O* farg0, core::T_O* farg1, core::T_O* farg2);
typedef LCC_RETURN_RAW (*ClaspXep4Function)(core::T_O* lcc_closure, core::T_O* farg0, core::T_O* farg1, core::T_O* farg2,
                                            core::T_O* farg3);
typedef LCC_RETURN_RAW (*ClaspXep5Function)(core::T_O* lcc_closure, core::T_O* farg0, core::T_O* farg1, core::T_O* farg2,
                                            core::T_O* farg3, core::T_O* farg4);

// A struct that a ClaspXepFunction is instantiated from.
struct ClaspXepTemplate {
  ClaspXepAnonymousFunction _EntryPoints[NUMBER_OF_ENTRY_POINTS];
};

// This is a template where the entry point functions are static.
// They are expected to be available as
// LCC_RETURN entry_point_n(T_O* closure, size_t nargs, T_O** args)
// LCC_RETURN entry_point_fixed(T_O* closure, ...)
// where entry_point_fixed is not variadic, but rather an infinite
// collection of overloaded functions accepting T_O*s, via templating.
template <typename Wrapper>
struct XepStereotype : public ClaspXepTemplate {
  XepStereotype() {
    _EntryPoints[0] = (ClaspXepAnonymousFunction)&Wrapper::entry_point_n;
    // C++ selects the correct overload of entry_point_fixed based on
    // the type we're assigning it to. A little magical.
    // See https://en.cppreference.com/w/cpp/language/overloaded_address
    // Doing it this way instead of putting in template parameters lets
    // us define some overloads without templates.
    // See e.g. clbind/iteratorMemberFunction.h
    ClaspXep0Function ep0 = Wrapper::entry_point_fixed;
    _EntryPoints[1] = (ClaspXepAnonymousFunction)ep0;
    ClaspXep1Function ep1 = Wrapper::entry_point_fixed;
    _EntryPoints[2] = (ClaspXepAnonymousFunction)ep1;
    ClaspXep2Function ep2 = Wrapper::entry_point_fixed;
    _EntryPoints[3] = (ClaspXepAnonymousFunction)ep2;
    ClaspXep3Function ep3 = Wrapper::entry_point_fixed;
    _EntryPoints[4] = (ClaspXepAnonymousFunction)ep3;
    ClaspXep4Function ep4 = Wrapper::entry_point_fixed;
    _EntryPoints[5] = (ClaspXepAnonymousFunction)ep4;
    ClaspXep5Function ep5 = Wrapper::entry_point_fixed;
    _EntryPoints[6] = (ClaspXepAnonymousFunction)ep5;
  }
  // Used for GFBytecodeSimpleFun_O
  XepStereotype(size_t specializer_length) {
    _EntryPoints[0] = (ClaspXepAnonymousFunction)&Wrapper::entry_point_n;

    if (0 < specializer_length) {
      ClaspXep0Function ep0 = Wrapper::error_entry_point_fixed;
      _EntryPoints[1] = (ClaspXepAnonymousFunction)ep0;
    } else {
      ClaspXep0Function ep0 = Wrapper::entry_point_fixed;
      _EntryPoints[1] = (ClaspXepAnonymousFunction)ep0;
    }
    if (1 < specializer_length) {
      ClaspXep1Function ep1 = Wrapper::error_entry_point_fixed;
      _EntryPoints[2] = (ClaspXepAnonymousFunction)ep1;
    } else {
      ClaspXep1Function ep1 = Wrapper::entry_point_fixed;
      _EntryPoints[2] = (ClaspXepAnonymousFunction)ep1;
    }
    if (2 < specializer_length) {
      ClaspXep2Function ep2 = Wrapper::error_entry_point_fixed;
      _EntryPoints[3] = (ClaspXepAnonymousFunction)ep2;
    } else {
      ClaspXep2Function ep2 = Wrapper::entry_point_fixed;
      _EntryPoints[3] = (ClaspXepAnonymousFunction)ep2;
    }
    if (3 < specializer_length) {
      ClaspXep3Function ep3 = Wrapper::error_entry_point_fixed;
      _EntryPoints[4] = (ClaspXepAnonymousFunction)ep3;
    } else {
      ClaspXep3Function ep3 = Wrapper::entry_point_fixed;
      _EntryPoints[4] = (ClaspXepAnonymousFunction)ep3;
    }
    if (4 < specializer_length) {
      ClaspXep4Function ep4 = Wrapper::error_entry_point_fixed;
      _EntryPoints[5] = (ClaspXepAnonymousFunction)ep4;
    } else {
      ClaspXep4Function ep4 = Wrapper::entry_point_fixed;
      _EntryPoints[5] = (ClaspXepAnonymousFunction)ep4;
    }
    if (5 < specializer_length) {
      ClaspXep5Function ep5 = Wrapper::error_entry_point_fixed;
      _EntryPoints[6] = (ClaspXepAnonymousFunction)ep5;
    } else {
      ClaspXep5Function ep5 = Wrapper::entry_point_fixed;
      _EntryPoints[6] = (ClaspXepAnonymousFunction)ep5;
    }
  }
};

struct ClaspXepFunction {
  static const int Entries = NUMBER_OF_ENTRY_POINTS;
  std::atomic<ClaspXepAnonymousFunction> _EntryPoints[NUMBER_OF_ENTRY_POINTS];
  // We need a default constructor since SimpleFun is
  // default constructible. I'm not sure WHY it is - something funky
  // about the LISP_CLASS macro I think. FIXME?
  ClaspXepFunction() = default;
  ClaspXepFunction(const ClaspXepTemplate& tl) {
    for (size_t i = 0; i < Entries; ++i)
      _EntryPoints[i].store(tl._EntryPoints[i], std::memory_order_relaxed);
  }
  void fixupInternalsForSnapshotSaveLoad(SimpleFun_O* cep, snapshotSaveLoad::Fixup* fixup) {
    printf("%s:%d:%s See function.h/clbind/line136\n", __FILE__, __LINE__, __FUNCTION__);
  }
  inline ClaspXepAnonymousFunction operator[](int index) const { return this->_EntryPoints[index].load(std::memory_order_relaxed); };
  inline void store(size_t index, ClaspXepAnonymousFunction fun, std::memory_order order = std::memory_order_relaxed) {
    this->_EntryPoints[index].store(fun, order);
  }

  inline LCC_RETURN invoke_n(T_O* closure, size_t lcc_nargs, T_O** lcc_args) const {
    return ((ClaspXepGeneralFunction)((*this)[0]))(closure, lcc_nargs, lcc_args);
  }

  inline LCC_RETURN invoke_0(T_O* closure) const { return ((ClaspXep0Function)((*this)[1]))(closure); }

  inline LCC_RETURN invoke_1(T_O* closure, T_O* farg0) const {
    return ((ClaspXep1Function)((*this)[2]))(closure, farg0);
  }

  inline LCC_RETURN invoke_2(T_O* closure, T_O* farg0, T_O* farg1) const {
    return ((ClaspXep2Function)((*this)[3]))(closure, farg0, farg1);
  }

  inline LCC_RETURN invoke_3(T_O* closure, T_O* farg0, T_O* farg1, T_O* farg2) const {
    return ((ClaspXep3Function)((*this)[4]))(closure, farg0, farg1, farg2);
  }

  inline LCC_RETURN invoke_4(T_O* closure, T_O* farg0, T_O* farg1, T_O* farg2, T_O* farg3) const {
    return ((ClaspXep4Function)((*this)[5]))(closure, farg0, farg1, farg2, farg3);
  }

  inline LCC_RETURN invoke_5(T_O* closure, T_O* farg0, T_O* farg1, T_O* farg2, T_O* farg3, T_O* farg4) const {
    return ((ClaspXep5Function)((*this)[6]))(closure, farg0, farg1, farg2, farg3, farg4);
  }

  // Call the XEP and return its results.
  // Fancy template version of the above invokes.
  // Arguments are raw pointers (T_O*) or it won't typecheck.
  template <typename... Ts>
  inline LCC_RETURN call(T_O* closure, Ts... args) {
    constexpr size_t nargs = sizeof...(Ts);
    // We have to use if constexpr so that the discarded branches
    // won't compile. switch constexpr would be nice but apparently
    // does not exist. Womp womp.
    if constexpr(nargs == 0) return invoke_0(closure);
    else if constexpr(nargs == 1) return invoke_1(closure, args...);
    else if constexpr(nargs == 2) return invoke_2(closure, args...);
    else if constexpr(nargs == 3) return invoke_3(closure, args...);
    else if constexpr(nargs == 4) return invoke_4(closure, args...);
    else if constexpr(nargs == 5) return invoke_5(closure, args...);
    else {
      // Collect args into an array, then invoke the general.
      T_O* lcc_args[nargs] = {args...};
      return invoke_n(closure, nargs, lcc_args);
    }
  }
};

#endif

#ifdef LCC_FUNCALL

extern "C" {

std::string dbg_safe_repr(void* raw);

// Return true if the Vaslist is at the head of the list and false if it is used up
inline bool dump_Vaslist_ptr(FILE* fout, Vaslist* args) {
  fprintf(fout, "Vaslist dump @%p\n", (void*)args);
  fprintf(fout, "Vaslist nargs = %lu\n", args->nargs());
  int iEnd = args->nargs();
  if (iEnd > CALL_ARGUMENTS_LIMIT) {
    iEnd = 0;
    fprintf(fout, "%s:%d      args->_nargs -> %lu !!!!  A BAD VALUE\n", __FILE__, __LINE__, args->nargs());
  }
  for (int i = 0; i < iEnd; ++i) {
    T_O* arg = (*args)[i];
    fprintf(fout, "     [%d] --> %p %s\n", i, arg, dbg_safe_repr((void*)arg).c_str());
  }
  return true;
};

inline void dump_lcc_args(FILE* fout, size_t lcc_nargs, T_O** lcc_args) {
  fprintf(fout, "lcc_args dump @%p\n", (void*)lcc_args);
  fprintf(fout, "lcc_nargs = %zu", lcc_nargs);
  if (lcc_nargs > CALL_ARGUMENTS_LIMIT) {
    fprintf(fout, "%s:%d      args->_nargs -> %zu !!!!  A BAD VALUE\n", __FILE__, __LINE__, lcc_nargs);
  } else {
    for (size_t i = 0; i < lcc_nargs; ++i) {
      T_O* arg = lcc_args[i];
      fprintf(fout, "     [%zu] --> %p %s\n", i, arg, dbg_safe_repr((void*)arg).c_str());
    }
  }
}

}; // extern "C"

#endif // LCC_FUNCALL
