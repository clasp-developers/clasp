#pragma once

/*
    File: iteratorMemberFunction.h
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
#include <clasp/clbind/clbind_wrappers.h>
#include <clasp/clbind/policies.h>
#include <clasp/clbind/details.h>
#include <clasp/clbind/wrapped_iterator.h>
namespace clbind {

template <typename T, typename FN> struct BeginReturnType {};

template <typename T, typename RT> struct BeginReturnType<T, RT (T::*)()> {
  typedef RT type;
};

template <typename Pols, typename OT, typename Begin, typename End> class WRAPPER_Iterator : public core::GlobalSimpleFunBase_O {
public:
  typedef WRAPPER_Iterator MyType;
  typedef core::GlobalSimpleFunBase_O TemplatedBase;

public:
  WRAPPER_Iterator(core::FunctionDescription_sp fdesc, core::T_sp code, Begin begin, End end)
      : core::GlobalSimpleFunBase_O(fdesc, core::XepStereotype<MyType>(), code), _begin(begin), _end(end){};

private:
  typedef typename BeginReturnType<OT, Begin>::type IteratorType;
  typedef Iterator<IteratorType, Pols> WrappedIteratorType;
  Begin _begin;
  End _end;

public:
  virtual size_t templatedSizeof() const { return sizeof(*this); };

  virtual void fixupInternalsForSnapshotSaveLoad(snapshotSaveLoad::Fixup* fixup) {
    // printf("%s:%d:%s Shouldn't need to fixup internals\n", __FILE__, __LINE__, __FUNCTION__ );
  }

public:
  static LCC_RETURN LISP_CALLING_CONVENTION() {
    WRAPPER_Iterator* closure = gctools::untag_general<WRAPPER_Iterator*>((WRAPPER_Iterator*)lcc_closure);
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    DO_DRAG_CXX_CALLS();
    if (lcc_nargs != 1)
      core::wrongNumberOfArguments(core::T_sp((gctools::Tagged)lcc_closure), lcc_nargs, 1);
    core::T_sp arg0((gctools::Tagged)lcc_args[0]);
    OT* objPtr = gc::As<core::WrappedPointer_sp>(arg0)->cast<OT>();
    IteratorType itBegin = ((*objPtr).*(closure->_begin))();
    IteratorType itEnd = ((*objPtr).*(closure->_end))();
    auto smart_itBegin = gctools::GC<WrappedIteratorType>::allocate(itBegin);
    auto smart_itEnd = gctools::GC<WrappedIteratorType>::allocate(itEnd);
    return Values(smart_itBegin, smart_itEnd);
  }
  static inline LISP_ENTRY_0() { return entry_point_n(lcc_closure, 0, NULL); }
  static inline LISP_ENTRY_1() {
    core::T_O* args[1] = {lcc_farg0};
    return entry_point_n(lcc_closure, 1, args);
  }
  static inline LISP_ENTRY_2() {
    core::T_O* args[2] = {lcc_farg0, lcc_farg1};
    return entry_point_n(lcc_closure, 2, args);
  }
  static inline LISP_ENTRY_3() {
    core::T_O* args[3] = {lcc_farg0, lcc_farg1, lcc_farg2};
    return entry_point_n(lcc_closure, 3, args);
  }
  static inline LISP_ENTRY_4() {
    core::T_O* args[4] = {lcc_farg0, lcc_farg1, lcc_farg2, lcc_farg3};
    return entry_point_n(lcc_closure, 4, args);
  }
  static inline LISP_ENTRY_5() {
    core::T_O* args[5] = {lcc_farg0, lcc_farg1, lcc_farg2, lcc_farg3, lcc_farg4};
    return entry_point_n(lcc_closure, 5, args);
  }
};
}; // namespace clbind

template <typename Policies, typename OT, typename Begin, typename End>
class gctools::GCStamp<clbind::WRAPPER_Iterator<Policies, OT, Begin, End>> {
public:
  static gctools::GCStampEnum const StampWtag =
      gctools::GCStamp<typename clbind::WRAPPER_Iterator<Policies, OT, Begin, End>::TemplatedBase>::StampWtag;
};

template <typename Policies, typename OT, typename Begin, typename End>
struct gctools::Inherits<typename clbind::WRAPPER_Iterator<Policies, OT, Begin, End>::TemplatedBase,
                         clbind::WRAPPER_Iterator<Policies, OT, Begin, End>> : public std::true_type {};
