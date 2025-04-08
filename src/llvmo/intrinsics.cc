/*
    File: intrinsics.cc
    Small functions used by the runtime that may be inlined at the
    compiler's discretion.
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
// #define DEBUG_LEVEL_FULL
#ifdef USE_MPS
extern "C" {
#include <clasp/mps/code/mps.h>
};
#endif

#include <cctype>
#include <cstdint>
#include <typeinfo>

#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/numbers.h>
#include <clasp/core/bignum.h>
#include <clasp/core/character.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/arguments.h>
#include <clasp/core/lisp.h>
#include <clasp/core/designators.h>
#include <clasp/core/compPackage.h>
#include <clasp/core/package.h>
#include <clasp/core/fli.h>
#include <clasp/core/instance.h>
#include <clasp/core/funcallableInstance.h>
#include <clasp/core/wrappedPointer.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/derivableCxxObject.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/loadTimeValues.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/numbers.h>
#include <clasp/core/array.h>
#include <clasp/core/fli.h>
#include <clasp/core/debugger.h>
#include <clasp/core/symbolTable.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/llvmo/intrinsics.h>

using namespace core;

#pragma GCC visibility push(default)

extern "C" {
void invalid_index_error(void* fixnum_index, void* fixnum_max, void* fixnum_axis) {
  SIMPLE_ERROR("Invalid index {} for axis {} of array: expected 0-{}", gctools::untag_fixnum((core::T_O*)fixnum_index),
               gctools::untag_fixnum((core::T_O*)fixnum_axis), gctools::untag_fixnum((core::T_O*)fixnum_max));
}
};

extern "C" {

ALWAYS_INLINE core::T_O* cc_ensure_valid_object(core::T_O* tagged_object) {
  NO_UNWIND_BEGIN();
  return gctools::ensure_valid_object(tagged_object);
  NO_UNWIND_END();
}

ALWAYS_INLINE core::T_O* cc_gatherVaRestArguments(Vaslist* vaslist, std::size_t nargs, Vaslist untagged_vargs_rest[2]) {
  NO_UNWIND_BEGIN();
  new (&untagged_vargs_rest[0]) Vaslist(nargs, vaslist->args());
  new (&untagged_vargs_rest[1]) Vaslist(nargs, vaslist->args());
  T_O* result = untagged_vargs_rest->asTaggedPtr();
#ifdef DEBUG_VASLIST
  if (_sym_STARdebugVaslistSTAR && _sym_STARdebugVaslistSTAR->symbolValue().notnilp()) {
    printf("%s:%d:%s nargs = %lu\n", __FILE__, __LINE__, __FUNCTION__, nargs);
    for (size_t ii = 0; ii < nargs; ++ii) {
      printf("     vaslist[%lu] = %s\n", ii, _rep_(core::T_sp((gctools::Tagged)untagged_vargs_rest[0][ii])).c_str());
    }
  }
#endif
  return result;
  NO_UNWIND_END();
}

ALWAYS_INLINE core::T_O* cc_makeCell() {
  core::Cons_sp res = core::Cons_O::create(nil<core::T_O>(), nil<core::T_O>());
#ifdef DEBUG_CC
  printf("%s:%d makeCell res.px[%p]\n", __FILE__, __LINE__, res.px);
#endif
  return res.raw_();
}

ALWAYS_INLINE char* cc_getPointer(core::T_O* pointer_object) {
  NO_UNWIND_BEGIN();
  core::Pointer_O* po = reinterpret_cast<core::Pointer_O*>(gctools::untag_general(pointer_object));
  char* ptr = reinterpret_cast<char*>(po->ptr());
  return ptr;
  NO_UNWIND_END();
}
};

extern "C" {

ALWAYS_INLINE core::T_O* cc_stack_enclose(void* closure_address, core::T_O* entryPointInfo, std::size_t numCells) {
  NO_UNWIND_BEGIN();
  ASSERT(((uintptr_t)(closure_address)&0x7) == 0); //
  gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(closure_address);
  const gctools::Header_s::BadgeStampWtagMtag closure_header = gctools::Header_s::BadgeStampWtagMtag::make<core::Closure_O>();
//  gctools::global_stack_closure_bytes_allocated += size;
#ifdef DEBUG_GUARD
  size_t size = gctools::sizeof_container_with_header<core::Closure_O>(numCells);
  new (header) gctools::GCHeader<core::Closure_O>::HeaderType(closure_header, size, 0, size);
#else
  new (header) gctools::GCHeader<core::Closure_O>::HeaderType(closure_header);
#endif
  core::T_sp tentryPoint((gctools::Tagged)entryPointInfo);
  core::SimpleCoreFun_sp entryPoint = gc::As<SimpleCoreFun_sp>(tentryPoint);
  auto obj = gctools::HeaderPtrToGeneralPtr<typename gctools::smart_ptr<core::Closure_O>::Type>(closure_address);
  new (obj)(typename gctools::smart_ptr<core::Closure_O>::Type)(numCells, entryPoint);
  gctools::smart_ptr<core::Closure_O> functoid = gctools::smart_ptr<core::Closure_O>(obj);
  //  printf("%s:%d  Allocating closure on stack at %p  stack_closure_p()->%d\n", __FILE__, __LINE__, functoid.raw_(),
  //  functoid->stack_closure_p());
  return functoid.raw_();
  NO_UNWIND_END();
}
};

extern "C" {
/* These functions are called by the runtime when the compiler wants to unbox
 * things. Unlike the FLI translators below, they are not permissive, in that
 * they will signal a type error if given anything but the particular type that's
 * supposed to be unboxed.
 * FIXME: Would be cleaner if we only had one set of translators, if they
 * actually have the same requirements (I don't know if this is the case).
 */

float cc_unbox_single_float(core::T_O* box) {
  T_sp tbox((gctools::Tagged)box);
  return gc::As<SingleFloat_sp>(tbox).unsafe_single_float();
}
double cc_unbox_double_float(core::T_O* box) {
  T_sp tbox((gctools::Tagged)box);
  return gc::As<DoubleFloat_sp>(tbox)->get();
}

}; // extern "C"

extern "C" {

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// M K -FUNCTIONS
// Helpers to create Lisp objects from C++ typed vars / values

// These functions are part of the Foreign Language Interface and are
// referenced from the FLI functions in fli.cc.

ALWAYS_INLINE core::T_sp mk_fixnum_short(short v) { return core::make_fixnum(v); }

ALWAYS_INLINE core::T_sp mk_fixnum_ushort(unsigned short v) { return core::make_fixnum(v); }

ALWAYS_INLINE core::T_sp mk_fixnum_int(int v) { return core::make_fixnum(v); }

ALWAYS_INLINE core::T_sp mk_fixnum_uint(unsigned int v) { return core::make_fixnum(v); }

ALWAYS_INLINE core::T_sp mk_fixnum_int8(int8_t v) { return core::make_fixnum(v); }

ALWAYS_INLINE core::T_sp mk_fixnum_uint8(uint8_t v) { return core::make_fixnum(v); }

ALWAYS_INLINE core::T_sp mk_fixnum_int16(int16_t v) { return core::make_fixnum(v); }

ALWAYS_INLINE core::T_sp mk_fixnum_uint16(uint16_t v) { return core::make_fixnum(v); }

ALWAYS_INLINE core::T_sp mk_fixnum_int32(int32_t v) { return core::make_fixnum(v); }

ALWAYS_INLINE core::T_sp mk_fixnum_uint32(uint32_t v) { return core::make_fixnum(v); }

ALWAYS_INLINE core::T_sp mk_integer_int64(int64_t v) { return core::Integer_O::create(v); }

ALWAYS_INLINE core::T_sp mk_integer_uint64(uint64_t v) { return core::Integer_O::create(v); }

ALWAYS_INLINE core::T_sp mk_integer_long(long v) { return core::Integer_O::create((Fixnum)v); }

ALWAYS_INLINE core::T_sp mk_integer_ulong(unsigned long v) { return core::Integer_O::create(static_cast<Fixnum>(v)); }

ALWAYS_INLINE core::T_sp mk_integer_longlong(long long v) { return core::Integer_O::create((long long)v); }

ALWAYS_INLINE core::T_sp mk_integer_ulonglong(unsigned long long v) { return core::Integer_O::create((unsigned long long)v); }

ALWAYS_INLINE core::T_sp mk_double_float(double v) { return core::DoubleFloat_O::create(v); }

ALWAYS_INLINE core::T_sp mk_single_float(float v) { return core::make_single_float(v); }

ALWAYS_INLINE core::T_sp mk_long_double(long double v) { return core::LongFloat_O::create(v); }

ALWAYS_INLINE core::T_sp mk_time(time_t v) {
  size_t size = sizeof(time_t);
  auto self = gctools::GC<clasp_ffi::ForeignData_O>::allocate_with_default_constructor();
  self->allocate(kw::_sym_clasp_foreign_data_kind_time, core::DeleteOnDtor, size);
  memmove(self->raw_data(), &v, size);
  return self;
}

ALWAYS_INLINE core::T_sp mk_pointer(void* v) {
  clasp_ffi::ForeignData_sp ptr = clasp_ffi::ForeignData_O::create(reinterpret_cast<uintptr_t>(v));
  ptr->set_kind(kw::_sym_clasp_foreign_data_kind_pointer);
  return ptr;
}

ALWAYS_INLINE core::T_sp mk_size(size_t v) { return core::Integer_O::create(static_cast<Fixnum>(v)); }

ALWAYS_INLINE core::T_sp mk_ssize(ssize_t v) { return core::Integer_O::create(static_cast<Fixnum>(v)); }

ALWAYS_INLINE core::T_sp mk_ptrdiff(ptrdiff_t v) { return core::Integer_O::create(static_cast<Fixnum>(v)); }

ALWAYS_INLINE core::T_sp mk_char(char v) { return core::clasp_make_character(v); }

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// T R A N S L A T O R S

// These functions are part of the Foreign Language Interface and are
// referenced from the FLI functions in fli.cc.

// ----------------------------------------------------------------------------
// FIXNUM
// ----------------------------------------------------------------------------

ALWAYS_INLINE gctools::Fixnum from_object_fixnum(core::T_O* obj) {
  gctools::Fixnum x = gctools::untag_fixnum<T_O*>(obj);
  return x;
}

ALWAYS_INLINE core::T_O* to_object_fixnum(gctools::Fixnum x) { return core::make_fixnum(x).raw_(); }

// ----------------------------------------------------------------------------
// SHORT
// ----------------------------------------------------------------------------

ALWAYS_INLINE short from_object_short(core::T_O* obj) { return (short)from_object_fixnum(obj); }

ALWAYS_INLINE core::T_O* to_object_short(short x) { return to_object_fixnum((gctools::Fixnum)x); }

// ----------------------------------------------------------------------------
// UNSIGNED SHORT
// ----------------------------------------------------------------------------

ALWAYS_INLINE unsigned short from_object_unsigned_short(core::T_O* obj) { return (unsigned short)from_object_fixnum(obj); }

ALWAYS_INLINE core::T_O* to_object_unsigned_short(short x) { return to_object_fixnum((gctools::Fixnum)x); }

// ----------------------------------------------------------------------------
// INT
// ----------------------------------------------------------------------------

ALWAYS_INLINE int from_object_int(core::T_O* obj) { return (int)from_object_fixnum(obj); }

ALWAYS_INLINE core::T_O* to_object_int(int x) { return to_object_fixnum((gctools::Fixnum)x); }

// ----------------------------------------------------------------------------
// UNSIGNED INT
// ----------------------------------------------------------------------------

ALWAYS_INLINE unsigned int from_object_unsigned_int(core::T_O* obj) { return (unsigned int)from_object_fixnum(obj); }

ALWAYS_INLINE core::T_O* to_object_unsigned_int(unsigned int x) { return to_object_fixnum((gctools::Fixnum)x); }

// ----------------------------------------------------------------------------
// LONG
// ----------------------------------------------------------------------------

ALWAYS_INLINE long from_object_long(core::T_O* obj) {
  long x = translate::make_from_object<long>(gctools::smart_ptr<core::T_O>((gctools::Tagged)obj));
  return x;
}

ALWAYS_INLINE core::T_O* to_object_long(long x) { return translate::to_object<long>::convert(x).raw_(); }

// ----------------------------------------------------------------------------
// UNSIGNED LONG
// ----------------------------------------------------------------------------

ALWAYS_INLINE unsigned long from_object_unsigned_long(core::T_O* obj) {
  unsigned long x = translate::make_from_object<unsigned long>(gctools::smart_ptr<core::T_O>((gctools::Tagged)obj));
  return x;
}

ALWAYS_INLINE core::T_O* to_object_unsigned_long(unsigned long x) { return translate::to_object<unsigned long>::convert(x).raw_(); }

// ----------------------------------------------------------------------------
// INT8
// ----------------------------------------------------------------------------

ALWAYS_INLINE int8_t from_object_int8(core::T_O* obj) { return (int8_t)from_object_fixnum(obj); }

ALWAYS_INLINE core::T_O* to_object_int8(int8_t x) { return to_object_fixnum((gctools::Fixnum)x); }

// ----------------------------------------------------------------------------
// Uint8
// ----------------------------------------------------------------------------

ALWAYS_INLINE uint8_t from_object_uint8(core::T_O* obj) { return (uint8_t)from_object_fixnum(obj); }

ALWAYS_INLINE core::T_O* to_object_uint8(uint8_t x) { return to_object_fixnum((gctools::Fixnum)x); }

// ----------------------------------------------------------------------------
// INT16
// ----------------------------------------------------------------------------

ALWAYS_INLINE int16_t from_object_int16(core::T_O* obj) { return (int16_t)from_object_fixnum(obj); }

ALWAYS_INLINE core::T_O* to_object_int16(int16_t x) { return to_object_fixnum((gctools::Fixnum)x); }

// ----------------------------------------------------------------------------
// UINT16
// ----------------------------------------------------------------------------

ALWAYS_INLINE uint16_t from_object_uint16(core::T_O* obj) { return (uint16_t)from_object_fixnum(obj); }

ALWAYS_INLINE core::T_O* to_object_uint16(uint16_t x) { return to_object_fixnum((gctools::Fixnum)x); }

// ----------------------------------------------------------------------------
// INT32
// ----------------------------------------------------------------------------

ALWAYS_INLINE int32_t from_object_int32(core::T_O* obj) { return (int32_t)from_object_fixnum(obj); }

ALWAYS_INLINE core::T_O* to_object_int32(int32_t x) { return to_object_fixnum((gctools::Fixnum)x); }

// ----------------------------------------------------------------------------
// UINT32
// ----------------------------------------------------------------------------

ALWAYS_INLINE uint32_t from_object_uint32(core::T_O* obj) { return (uint32_t)from_object_fixnum(obj); }

ALWAYS_INLINE core::T_O* to_object_uint32(uint32_t x) { return to_object_fixnum((gctools::Fixnum)x); }

// ----------------------------------------------------------------------------
// INT64
// ----------------------------------------------------------------------------

ALWAYS_INLINE int64_t from_object_int64(core::T_O* obj) {
  int64_t x = translate::make_from_object<int64_t>(gctools::smart_ptr<core::T_O>((gctools::Tagged)obj));
  return x;
}

ALWAYS_INLINE core::T_O* to_object_int64(int64_t x) { return translate::to_object<int64_t>::convert(x).raw_(); }

// ----------------------------------------------------------------------------
// UINT64
// ----------------------------------------------------------------------------

ALWAYS_INLINE uint64_t from_object_uint64(core::T_O* obj) {
  uint64_t x = translate::make_from_object<uint64_t>(gctools::smart_ptr<core::T_O>((gctools::Tagged)obj));
  return x;
}

ALWAYS_INLINE core::T_O* to_object_uint64(uint64_t x) { return translate::to_object<uint64_t>::convert(x).raw_(); }

// ----------------------------------------------------------------------------
// LONG LONG
// ----------------------------------------------------------------------------

ALWAYS_INLINE long long from_object_long_long(core::T_O* obj) {
  core::T_sp tobj((gctools::Tagged)obj);
  long long x = translate::make_from_object<long long>(tobj);
  return x;
}

ALWAYS_INLINE core::T_O* to_object_long_long(long long x) { return translate::to_object<long long>::convert(x).raw_(); }

// ----------------------------------------------------------------------------
// UNSIGNED LONG LONG
// ----------------------------------------------------------------------------

ALWAYS_INLINE unsigned long long from_object_unsigned_long_long(core::T_O* obj) {
  unsigned long long x = translate::make_from_object<long long>(gctools::smart_ptr<core::T_O>((gctools::Tagged)obj));
  return x;
}

ALWAYS_INLINE core::T_O* to_object_unsigned_long_long(unsigned long long x) {
  return translate::to_object<unsigned long long>::convert(x).raw_();
}

// ----------------------------------------------------------------------------
// SIZE_T
// ----------------------------------------------------------------------------

ALWAYS_INLINE size_t from_object_size(core::T_O* obj) {
  size_t x = translate::make_from_object<size_t>(gctools::smart_ptr<core::T_O>((gctools::Tagged)obj));
  return x;
}

ALWAYS_INLINE core::T_O* to_object_size(size_t x) { return translate::to_object<size_t>::convert(x).raw_(); }

// ----------------------------------------------------------------------------
// SSIZE_T
// ----------------------------------------------------------------------------

ALWAYS_INLINE size_t from_object_ssize(core::T_O* obj) {
  ssize_t x = translate::make_from_object<ssize_t>(gctools::smart_ptr<core::T_O>((gctools::Tagged)obj));
  return x;
}

ALWAYS_INLINE core::T_O* to_object_ssize(ssize_t x) { return translate::to_object<ssize_t>::convert(x).raw_(); }

// ----------------------------------------------------------------------------
// PTRDIFF_T
// ----------------------------------------------------------------------------

ALWAYS_INLINE ptrdiff_t from_object_ptrdiff(core::T_O* obj) {
  ptrdiff_t x = translate::make_from_object<ptrdiff_t>(gctools::smart_ptr<core::T_O>((gctools::Tagged)obj));
  return x;
}

ALWAYS_INLINE core::T_O* to_object_ptrdiff(ptrdiff_t x) { return translate::to_object<ptrdiff_t>::convert(x).raw_(); }

// ----------------------------------------------------------------------------
// TIME_T
// ----------------------------------------------------------------------------

ALWAYS_INLINE time_t from_object_time(core::T_O* obj) {
  time_t x = translate::make_from_object<time_t>(gctools::smart_ptr<core::T_O>((gctools::Tagged)obj));
  return x;
}

ALWAYS_INLINE core::T_O* to_object_time(time_t x) { return translate::to_object<time_t>::convert(x).raw_(); }

// ----------------------------------------------------------------------------
// CHAR
// ----------------------------------------------------------------------------

ALWAYS_INLINE char from_object_char(core::T_O* obj) {
  char x = translate::make_from_object<char>(gctools::smart_ptr<core::T_O>((gctools::Tagged)obj));
  return x;
}

ALWAYS_INLINE core::T_O* to_object_char(char x) { return translate::to_object<char>::convert(x).raw_(); }

// ----------------------------------------------------------------------------
// UNSIGNED CHAR
// ----------------------------------------------------------------------------

ALWAYS_INLINE unsigned char from_object_unsigned_char(core::T_O* obj) {
  unsigned char x = translate::make_from_object<unsigned char>(gctools::smart_ptr<core::T_O>((gctools::Tagged)obj));
  return x;
}

ALWAYS_INLINE core::T_O* to_object_unsigned_char(unsigned char x) { return translate::to_object<unsigned char>::convert(x).raw_(); }

// ----------------------------------------------------------------------------
// claspCharacter
// ----------------------------------------------------------------------------

ALWAYS_INLINE claspCharacter from_object_claspCharacter(core::T_O* obj) {
  return reinterpret_cast<claspCharacter>(gctools::untag_character<core::T_O*>(obj));
}

ALWAYS_INLINE core::T_O* to_object_claspCharacter(claspCharacter x) { return gctools::tag_character<core::T_O*>(x); }

// ----------------------------------------------------------------------------
// claspChar
// ----------------------------------------------------------------------------

ALWAYS_INLINE claspChar from_object_claspChar(core::T_O* obj) {
  return static_cast<claspChar>(gctools::untag_character<core::T_O*>(obj));
}

ALWAYS_INLINE core::T_O* to_object_claspChar(claspChar x) { return gctools::tag_character<core::T_O*>(x); }

// ----------------------------------------------------------------------------
// FLOAT
// ----------------------------------------------------------------------------

ALWAYS_INLINE float from_object_float(core::T_O* obj) {
  // Christian - Feb 12, 2017
  //
  // this is correct but it's not the absolute best solution.
  // The best solution would be a converter from every type to
  // float. As in:  from_single_float_to_float, from_double_float_to_float, from_fixnum_to_float
  // and then have type checks in Common Lisp.
  T_sp tobj((gctools::Tagged)obj);
  if (gc::IsA<Number_sp>(tobj)) {
    Number_sp nobj = gc::As_unsafe<Number_sp>(tobj);
    float x = clasp_to_float(nobj);
    return x;
  }
  TYPE_ERROR(tobj, cl::_sym_Number_O);
}

ALWAYS_INLINE core::T_O* to_object_float(float x) { return translate::to_object<float>::convert(x).raw_(); }

// ----------------------------------------------------------------------------
// DOUBLE
// ----------------------------------------------------------------------------

ALWAYS_INLINE double from_object_double(core::T_O* obj) {
  double x = translate::make_from_object<double>(gctools::smart_ptr<core::T_O>((gctools::Tagged)obj));
  return x;
}

ALWAYS_INLINE core::T_O* to_object_double(double x) { return translate::to_object<double>::convert(x).raw_(); }

// ----------------------------------------------------------------------------
// LONG DOUBLE
// ----------------------------------------------------------------------------

ALWAYS_INLINE long double from_object_long_double(core::T_O* obj) {
  long double x = translate::make_from_object<long double>(gctools::smart_ptr<core::T_O>((gctools::Tagged)obj));
  return x;
}

ALWAYS_INLINE core::T_O* to_object_long_double(long double x) { return translate::to_object<long double>::convert(x).raw_(); }

ALWAYS_INLINE core::T_O* to_object_void(void) { return nil<core::T_O>().raw_(); }

// ----------------------------------------------------------------------------
// POINTER
// ----------------------------------------------------------------------------

ALWAYS_INLINE void* from_object_pointer(core::T_O* obj) {
  T_sp tobj((gctools::Tagged)obj);
  if (gctools::IsA<clasp_ffi::ForeignData_sp>(tobj)) {
    return gctools::As_unsafe<clasp_ffi::ForeignData_sp>(tobj)->ptr();
  }
  if (gctools::IsA<core::Pointer_sp>(tobj)) {
    return gctools::As_unsafe<core::Pointer_sp>(tobj)->ptr();
  }
  SIMPLE_ERROR("Handle from_object_pointer for value: {}", _rep_(tobj));
}

ALWAYS_INLINE core::T_O* to_object_pointer(void* x) { return clasp_ffi::ForeignData_O::create(x).raw_(); }

// === END OF CORE TRANSLATORS ===

}; // extern "C"

////////////////////////////////////////////////////////////
//
// builtins.cc moved here.
//

#define TEMPLATE_READ_STAMP
#include <clasp/llvmo/read-stamp.cc>
#undef TEMPLATE_READ_STAMP

extern "C" {
gctools::Tagged cx_read_stamp(core::T_O* obj) {
  gctools::Tagged old_stamp = (gctools::Tagged)llvmo::template_read_stamp<core::T_O>(obj);
  return old_stamp;
}
};

extern "C" {

gctools::ShiftedStamp cc_read_derivable_cxx_stamp_untagged_object(core::T_O* untagged_object) {
  core::DerivableCxxObject_O* derivable_cxx_object_ptr = reinterpret_cast<core::DerivableCxxObject_O*>(untagged_object);
  gctools::ShiftedStamp stamp = (gctools::ShiftedStamp)derivable_cxx_object_ptr->get_stamp_();
  ASSERT(gctools::Header_s::BadgeStampWtagMtag::is_derivable_shifted_stamp(stamp));
  //  printf("%s:%d:%s returning stamp %lu - check if it is correct\n", __FILE__, __LINE__, __FUNCTION__, stamp);
  return stamp;
}

#if 0
void cc_rewind_vaslist(vaslist va_args, void** register_save_areaP)
{
  LCC_REWIND_VASLIST(va_args,register_save_areaP);
}
#endif

size_t cc_checkBound(core::T_O* array, size_t bound, core::T_O* index) {
  (void)array; // FIXME
  core::T_sp tindex((gctools::Tagged)index);
  if (tindex.fixnump()) {
    core::Fixnum findex = tindex.unsafe_fixnum();
    if ((0 <= findex) && (findex < bound))
      return findex;
    else {
      // FIXME: use core::badIndexError instead
      TYPE_ERROR(tindex, Cons_O::createList(cl::_sym_integer, core::make_fixnum(0), Cons_O::createList(core::make_fixnum(bound))));
    }
  } else
    TYPE_ERROR(tindex, cl::_sym_fixnum);
}

unsigned char cc_simpleBitVectorAref(core::T_O* tarray, size_t index) {
  core::SimpleBitVector_O* array = reinterpret_cast<core::SimpleBitVector_O*>(gctools::untag_general<core::T_O*>(tarray));
  return (*array)[index];
}

void cc_simpleBitVectorAset(core::T_O* tarray, size_t index, unsigned char v) {
  core::SimpleBitVector_O* array = reinterpret_cast<core::SimpleBitVector_O*>(gctools::untag_general<core::T_O*>(tarray));
  (*array)[index] = v;
}

#if 0
void cc_validate_tagged_pointer(core::T_O* ptr)
{
  unlikely_if ((((uintptr_t)ptr)&gctools::gc_tag)==gctools::gc_tag) {
    uintptr_t testval = ((uintptr_t)ptr)&gctools::gc_tag;
    uintptr_t gctag = gctools::gc_tag;
    printf("%s:%d Invalid tagged pointer %p testval(%lu)==gctag(%lu)\n", __FILE__, __LINE__, ptr, testval,gctag );
    abort();
  }
}
#endif
};

namespace llvmo {

void initialize_raw_translators(void) {
  // Nothing to do

  return;

} // initialize_raw_translators

}; // namespace llvmo

#pragma GCC visibility pop
