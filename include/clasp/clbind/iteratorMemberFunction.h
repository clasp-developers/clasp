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
#ifndef clbind_iteratorMemberFunction_H
#define clbind_iteratorMemberFunction_H

#include <clasp/clbind/clbind_wrappers.h>
#include <clasp/clbind/policies.h>
#include <clasp/clbind/details.h>
#include <clasp/clbind/wrapped_iterator.h>
namespace clbind {

template <typename T, typename FN>
struct BeginReturnType {};

template <typename T, typename RT>
struct BeginReturnType<T, RT (T::*)()> {
  typedef RT type;
};

template <typename Pols, typename OT, typename Begin, typename End>
class IteratorMethoid : public core::BuiltinClosure_O {
public:
  typedef core::BuiltinClosure_O TemplatedBase;

public:
 IteratorMethoid(core::FunctionDescription* fdesc, Begin begin, End end) : core::BuiltinClosure_O(entry_point,fdesc), _begin(begin), _end(end){};

private:
  typedef typename BeginReturnType<OT, Begin>::type IteratorType;
  typedef Iterator<IteratorType, Pols> WrappedIteratorType;
  Begin _begin;
  End _end;

public:
  virtual size_t templatedSizeof() const { return sizeof(*this); };

public:
  static LCC_RETURN LISP_CALLING_CONVENTION() {
    IteratorMethoid* closure = gctools::untag_general<IteratorMethoid*>((IteratorMethoid*)lcc_closure);
    INCREMENT_FUNCTION_CALL_COUNTER(closure);
    if (lcc_nargs != 1)
      core::wrongNumberOfArguments(lcc_nargs, 1);
    OT *objPtr = gc::As<core::WrappedPointer_sp>((LCC_ARG0()))->cast<OT>();
    IteratorType itBegin = ((*objPtr).*(closure->_begin))();
    IteratorType itEnd = ((*objPtr).*(closure->_end))();
    GC_ALLOCATE_VARIADIC(WrappedIteratorType, smart_itBegin, itBegin);
    GC_ALLOCATE_VARIADIC(WrappedIteratorType, smart_itEnd, itEnd);
    return Values(smart_itBegin, smart_itEnd);
  }
};
};

template <typename Pols, typename OT, typename Begin, typename End>
class gctools::GCStamp<clbind::IteratorMethoid<Pols, OT, Begin, End>> {
public:
  static gctools::GCStampEnum const Stamp = gctools::GCStamp<typename clbind::IteratorMethoid<Pols, OT, Begin, End>::TemplatedBase>::Stamp;
};

#endif
