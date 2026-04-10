#pragma once

/*
  File: smart_pointers.h
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

//
// (C) 2004 Christian E. Schafmeister
//

#include <iostream>
#include <cstring>

#include <clasp/gctools/globals.h>

extern void lisp_errorUnexpectedNil(type_info const& toType);
extern void lisp_errorBadCast(type_info const& toType, type_info const& fromType, core::T_O* objP);
extern void lisp_errorBadCastFromT_O(type_info const& toType, core::T_O* objP);
extern void lisp_errorBadCastToFixnum_O(type_info const& fromType, core::T_O* objP);
extern void lisp_errorBadCastFromT_OToCons_O(core::T_O* objP);
extern void lisp_errorBadCastFromSymbol_O(type_info const& toType, core::Symbol_O* objP);

typedef uint8_t byte8_t;
typedef uint16_t byte16_t;
typedef uint32_t byte32_t;
typedef uint64_t byte64_t;
typedef int8_t integer8_t;
typedef int16_t integer16_t;
typedef int32_t integer32_t;
typedef int64_t integer64_t;

namespace core {
struct Vaslist;
}; // namespace core

namespace gctools {

//    typedef core::T_O Fixnum_ty;

template <class T> class smart_ptr;

#ifdef _ADDRESS_MODEL_64

static const uintptr_t alignment = 8;    // 16 byte alignment for all pointers
static const uintptr_t pointer_size = 8; // 8 byte words 64-bits
#if defined(CLASP_MS_WINDOWS_HOST)
#error "Define a 64bit Fixnum for windows"
#else
                                         //! Fixnum definition for 64 bit system
typedef ::Fixnum Fixnum;
#endif

typedef Fixnum cl_fixnum;
};

typedef gctools::Fixnum cl_index;

namespace gctools {
/*! A pointer that is already tagged can be passed to smart_ptr constructors
      by first reinterpret_casting it to Tagged */
typedef uintptr_t Tagged;
typedef uintptr_t TaggedVaslist; // Used in situations where only a tagged Vaslist ptr is accepted
static const int tag_shift = TAG_BITS;
static const int fixnum_bits = 64 - FIXNUM_SHIFT;
static const int fixnum_shift = FIXNUM_SHIFT;

static const long int most_positive_fixnum = 2305843009213693951;
static const long int most_negative_fixnum = -2305843009213693952;
static const size_t thread_local_cl_stack_min_size = THREAD_LOCAL_CL_STACK_MIN_SIZE;

#define MOST_POSITIVE_FIXNUM gctools::most_positive_fixnum
#define MOST_NEGATIVE_FIXNUM gctools::most_negative_fixnum
#define FIXNUM_BITS gctools::fixnum_bits

#endif // _ADDRESS_MODEL_64

#ifdef _ADDRESS_MODEL_32
#error "Add support for 32 bits - squeeze, Squeeze, SQUEEZE!"
#endif
/*! Pointer and immediate value tagging is set up here */
/* FIXNUM's have the lsb x00 set to zero - this allows addition and comparison to be fast */
/* The rest of the bits are the fixnum */
static const uintptr_t ptag_mask = ZERO_TAG_MASK;  // #b111;
static const uintptr_t fixnum00_tag = FIXNUM0_TAG; // x0000 means fixnum
static const uintptr_t fixnum01_tag = FIXNUM1_TAG; // x0100 means fixnum
#if TAG_BITS == 4
static const uintptr_t fixnum10_tag = FIXNUM2_TAG; // x1000 means fixnum
static const uintptr_t fixnum11_tag = FIXNUM3_TAG; // x1100 means fixnum
#endif
static const uintptr_t fixnum_mask = FIXNUM_MASK;
/*! The pointer tags, that point to objects that the GC manages are general_tag and cons_tag
Robert Strandh suggested a separate tag for CONS cells so that there would be a quick CONSP test
for a CONS cell*/
static const uintptr_t ptr_mask = ~(uintptr_t)ZERO_TAG_MASK;
static const uintptr_t general_tag = GENERAL_TAG; // means a GENERAL pointer
static const uintptr_t cons_tag = CONS_TAG;       // means a CONS cell pointer
/*! A test for pointers has the form (potential_ptr&POINTER_TAG_MASK)==POINTER_TAG_EQ) */
static const uintptr_t pointer_tag_mask = POINTER_TAG_MASK;
static const uintptr_t pointer_tag_eq = POINTER_TAG_EQ;

/*! gc_tag is used for headerless objects to indicate that this word is
used by the garbage collector */
static const uintptr_t gc_tag = GC_TAG; // 0b1111;

/*! valist_tag is a tag for va_list(s) on the stack, it is used by Clasp to
iterate over variable numbers of arguments passed to functions.
Pointers with this tag are NOT moved in memory, the objects valist_tag'd pointers
point to are only ever on the stack.
I hack the vaslist structure in X86_64 ABI dependent ways and I will abstract all of the
ABI dependent behavior into a single header file so that it can be implemented for other
ABI's  */
static const uintptr_t vaslist0_tag = VASLIST0_TAG; // means a valist
#if TAG_BITS == 4
static const uintptr_t vaslist1_tag = VASLIST1_TAG;          // means a valist that is unaligned
static const uintptr_t vaslist_ptag_mask = VASLIST_TAG_MASK; // #b111
#endif
/*! Immediate value tags */
static const uintptr_t immediate_mask = IMMEDIATE_MASK;
static const uintptr_t character_tag = CHARACTER_TAG;
static const uintptr_t character_shift = CHARACTER_SHIFT;
static const uintptr_t single_float_tag = SINGLE_FLOAT_TAG;
static const uintptr_t single_float_shift = SINGLE_FLOAT_SHIFT;
static const uintptr_t single_float_mask = 0x1FFFFFFFFF; // single-floats are in these 32+5bits
#ifdef CLASP_SHORT_FLOAT
static const uintptr_t short_float_tag = SHORT_FLOAT_TAG;
static const uintptr_t short_float_shift = SHORT_FLOAT_SHIFT;
static const uintptr_t short_float_mask = 0x1FFFFFFFFF; // single-floats are in these 32+5bits
#endif

template <class T> T ptag(T ptr) { return reinterpret_cast<T>(reinterpret_cast<uintptr_t>(ptr) & ptag_mask); };

template <class T> inline bool tagged_consp(T ptr) { return (reinterpret_cast<uintptr_t>(ptag(ptr)) == cons_tag); };

template <class T> inline T tag_cons(T p) {
  GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(p) & ptag_mask) == 0);
  return reinterpret_cast<T>(reinterpret_cast<uintptr_t>(p) + cons_tag);
}

template <class T> inline T untag_cons(T ptr) {
  GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr) & ptag_mask) == cons_tag);
  return reinterpret_cast<T>(reinterpret_cast<uintptr_t>(ptr) - cons_tag);
}

template <class T> inline T tag_general(T p) {
  GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(p) & ptag_mask) == 0);
  return reinterpret_cast<T>(reinterpret_cast<uintptr_t>(p) + general_tag);
}

template <class T> inline T tag_object(T ptr) { return tag_general<T>(ptr); }
template <> inline core::Cons_O* tag_object<core::Cons_O*>(core::Cons_O* ptr) { return tag_cons<core::Cons_O*>(ptr); }

template <class T> inline bool tagged_nilp(T ptr) {
  bool res = (reinterpret_cast<void*>(ptr) == global_tagged_Symbol_OP_nil);
  return res;
}
template <class T> inline bool tagged_unboundp(T ptr) {
  return ((reinterpret_cast<uintptr_t>(ptr) & UNBOUND_MASK) == UNBOUND_BYTE);
}
template <class T> inline bool tagged_no_thread_local_bindingp(T ptr) {
  return ((reinterpret_cast<uintptr_t>(ptr) & UNBOUND_MASK) == NO_THREAD_LOCAL_BINDING_UNBOUND_BYTE);
}
template <class T> inline bool tagged_deletedp(T ptr) {
  return ((reinterpret_cast<uintptr_t>(ptr) & UNBOUND_MASK) == DELETED_UNBOUND_BYTE);
}
template <class T> inline bool tagged_no_keyp(T ptr) {
  return ((reinterpret_cast<uintptr_t>(ptr) & UNBOUND_MASK) == NO_KEY_UNBOUND_BYTE);
}
template <class T> inline bool tagged_same_as_keyp(T ptr) {
  return ((reinterpret_cast<uintptr_t>(ptr) & UNBOUND_MASK) == SAME_AS_KEY_UNBOUND_BYTE);
}

template <class T> inline T tag_nil() {
  GCTOOLS_ASSERT(tagged_nilp(global_tagged_Symbol_OP_nil));
  return reinterpret_cast<T>(global_tagged_Symbol_OP_nil);
}
template <class T> inline T tag_unbound() { return reinterpret_cast<T>((unsigned long)UNBOUND_BYTE); }
template <class T> inline T tag_no_thread_local_binding() {
  return reinterpret_cast<T>((unsigned long)NO_THREAD_LOCAL_BINDING_UNBOUND_BYTE);
}
template <class T> inline T tag_no_key() { return reinterpret_cast<T>((unsigned long)NO_KEY_UNBOUND_BYTE); }
template <class T> inline T tag_deleted() { return reinterpret_cast<T>((unsigned long)DELETED_UNBOUND_BYTE); }
template <class T> inline T tag_same_as_key() { return reinterpret_cast<T>((unsigned long)SAME_AS_KEY_UNBOUND_BYTE); }
template <class T> inline T tag_vaslist(core::Vaslist* p) {
#if TAG_BITS == 4
  GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(p) & vaslist_ptag_mask) == 0);
#error "Handle TAG_BITS==4"
#endif
  return reinterpret_cast<T>(reinterpret_cast<uintptr_t>(p) + vaslist0_tag);
}

template <class T> inline T untag_general(T ptr) {
  GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr) & ptag_mask) == general_tag);
  return reinterpret_cast<T>(reinterpret_cast<uintptr_t>(ptr) - general_tag);
}

template <class T> inline core::Vaslist* untag_vaslist(T ptr) {
#if TAG_BITS == 4
  GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr) & vaslist_ptag_mask) == vaslist0_tag);
#error "Handle TAG_BITS==4"
#endif
  return reinterpret_cast<core::Vaslist*>(reinterpret_cast<uintptr_t>(ptr) - vaslist0_tag);
}

template <class T> inline T tag_fixnum(Fixnum fn) { return reinterpret_cast<T>((fn << fixnum_shift)); }
template <class T> inline Fixnum untag_fixnum(T const ptr) {
  GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr) & fixnum_mask) == 0);
  return (Fixnum)(reinterpret_cast<Fixnum>(ptr) >> fixnum_shift);
}
template <class T> inline bool tagged_fixnump(T ptr) { return ((reinterpret_cast<uintptr_t>(ptr) & fixnum_mask) == fixnum00_tag); };
template <class T> inline T tag_character(int ch) { return reinterpret_cast<T>((ch << character_shift) | character_tag); }
template <class T> inline claspCharacter untag_character(T ptr) {
  GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr) & immediate_mask) == character_tag);
  return (claspCharacter)(reinterpret_cast<uintptr_t>(ptr) >> character_shift);
}
template <class T> inline bool tagged_characterp(T ptr) {
  return ((reinterpret_cast<uintptr_t>(ptr) & immediate_mask) == character_tag);
};

template <class T> inline T tag_single_float(float fn) {
  GCTOOLS_ASSERT(sizeof(uintptr_t) == 8);
  GCTOOLS_ASSERT(sizeof(float) == 4);
  uintptr_t val;
  memcpy(&val, &fn, sizeof(fn));
  return reinterpret_cast<T>((val << single_float_shift) + single_float_tag);
}
template <class T> inline uintptr_t tagged_single_float_masked(T const ptr) {
  return reinterpret_cast<uintptr_t>(reinterpret_cast<uintptr_t>(ptr) & single_float_mask);
}
template <class T> inline float untag_single_float(T const ptr) {
  GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr) & immediate_mask) == single_float_tag);
  GCTOOLS_ASSERT(sizeof(uintptr_t) == 8);
  GCTOOLS_ASSERT(sizeof(float) == 4);
  uintptr_t val(reinterpret_cast<uintptr_t>(ptr));
  float result;
  val >>= single_float_shift;
  memcpy(&result, &val, sizeof(result));
  return result;
}
template <class T> inline bool tagged_single_floatp(T ptr) {
  return ((reinterpret_cast<uintptr_t>(ptr) & immediate_mask) == single_float_tag);
};

#ifdef CLASP_SHORT_FLOAT
template <class T> inline T tag_short_float(float fn) {
  GCTOOLS_ASSERT(sizeof(uintptr_t) == 8);
  GCTOOLS_ASSERT(sizeof(float) == 4);
  uintptr_t val;
  memcpy(&val, &fn, sizeof(fn));
  return reinterpret_cast<T>((val << short_float_shift) + short_float_tag);
}
template <class T> inline uintptr_t tagged_short_float_masked(T const ptr) {
  return reinterpret_cast<uintptr_t>(reinterpret_cast<uintptr_t>(ptr) & short_float_mask);
}
template <class T> inline float untag_short_float(T const ptr) {
  GCTOOLS_ASSERT((reinterpret_cast<uintptr_t>(ptr) & immediate_mask) == short_float_tag);
  GCTOOLS_ASSERT(sizeof(uintptr_t) == 8);
  GCTOOLS_ASSERT(sizeof(float) == 4);
  uintptr_t val(reinterpret_cast<uintptr_t>(ptr));
  float result;
  val >>= short_float_shift;
  memcpy(&result, &val, sizeof(result));
  return result;
}
template <class T> inline bool tagged_short_floatp(T ptr) {
  return ((reinterpret_cast<uintptr_t>(ptr) & immediate_mask) == short_float_tag);
};
#endif

template <class T> inline bool tagged_generalp(T ptr) { return ((uintptr_t)(ptr)&ptag_mask) == general_tag; }

template <class T> inline bool tagged_vaslistp(T ptr) {
#if TAG_BITS == 3
  return ((reinterpret_cast<uintptr_t>(ptr) & ptag_mask) == vaslist0_tag);
#else
#error "Handle TAG_BITS==4"
  return ((reinterpret_cast<uintptr_t>(ptr) & vaslist_ptag_mask) == vaslist0_tag);
#endif
};

template <class T> inline bool tagged_objectp(T ptr) {
  return (reinterpret_cast<uintptr_t>(ptr) & pointer_tag_mask) == pointer_tag_eq;
}

template <class Type> inline Type untag_object(Type tagged_obj) {
  GCTOOLS_ASSERT(tagged_objectp(tagged_obj));
  return reinterpret_cast<Type>((uintptr_t)tagged_obj & ptr_mask);
}

}; // namespace gctools
