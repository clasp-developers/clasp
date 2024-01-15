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

template <typename Pols, typename OT, typename Begin, typename End> class WRAPPER_Iterator : public core::SimpleFun_O {
public:
  typedef WRAPPER_Iterator MyType;
  typedef core::SimpleFun_O TemplatedBase;

public:
  WRAPPER_Iterator(core::FunctionDescription_sp fdesc, core::T_sp code, Begin begin, End end)
    : core::SimpleFun_O(fdesc, code, core::XepStereotype<MyType>()), _begin(begin), _end(end){};

private:
  typedef std::invoke_result_t<Begin, OT> IteratorType;
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
  static inline LCC_RETURN entry_point_fixed(core::T_O* lcc_closure) {
    DO_DRAG_CXX_CALLS();
    core::wrongNumberOfArguments(core::T_sp((gctools::Tagged)lcc_closure), 0, 1);
  }
  template <typename... Ts>
  static inline LCC_RETURN entry_point_fixed(core::T_O* lcc_closure,
                                             core::T_O* a0, Ts... as) {
    WRAPPER_Iterator* closure = gctools::untag_general<WRAPPER_Iterator*>((WRAPPER_Iterator*)lcc_closure);
    DO_DRAG_CXX_CALLS();
    if constexpr(sizeof...(Ts) == 0) { // correct argcount
      core::T_sp arg0((gctools::Tagged)a0);
      OT* objPtr = gc::As<core::WrappedPointer_sp>(arg0)->cast<OT>();
      IteratorType itBegin = ((*objPtr).*(closure->_begin))();
      IteratorType itEnd = ((*objPtr).*(closure->_end))();
      auto smart_itBegin = gctools::GC<WrappedIteratorType>::allocate(itBegin);
      auto smart_itEnd = gctools::GC<WrappedIteratorType>::allocate(itEnd);
      return Values(smart_itBegin, smart_itEnd);
    } else { // too many arguments
      core::wrongNumberOfArguments(core::T_sp((gctools::Tagged)lcc_closure), 1+sizeof...(Ts), 1);
    }
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
