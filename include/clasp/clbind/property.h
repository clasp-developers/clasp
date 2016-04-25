/*
    File: property.h
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
#ifndef clbind_property_H
#define clbind_property_H

#include <clasp/core/translators.h>

#include <clasp/clbind/clbind_wrappers.h>
#include <clasp/clbind/policies.h>
#include <clasp/clbind/details.h>

namespace clbind {

template <typename T>
struct memberpointertraits {};

template <typename M, typename C>
struct memberpointertraits<M C::*> {
  typedef M member_type;
  typedef C class_type;
};

template <typename GetterPolicies, typename OT, typename VariablePtrType>
class GetterMethoid : public core::BuiltinClosure_O {
public:
  typedef core::BuiltinClosure_O TemplatedBase;

private:
  typedef typename memberpointertraits<VariablePtrType>::member_type MemberType;
  typedef clbind::Wrapper<MemberType> WrapperType;
  VariablePtrType _MemberPtr;

public:
  virtual size_t templatedSizeof() const { return sizeof(*this); };

public:
  GetterMethoid(core::T_sp name, VariablePtrType p) : core::BuiltinClosure_O(name), _MemberPtr(p){};
  DISABLE_NEW();
  LCC_RETURN LISP_CALLING_CONVENTION() {
    ASSERT_LCC_VA_LIST_CLOSURE_DEFINED(lcc_arglist);
    OT *objPtr = gc::As<core::WrappedPointer_sp>((LCC_ARG0()))->cast<OT>();
    MemberType &orig = (*objPtr).*(this->_MemberPtr);
    return Values(translate::to_object<MemberType, translate::dont_adopt_pointer>::convert(orig));
  }
};
};

namespace clbind {
template <typename GetterPolicies, typename OT, typename MemberType>
class GetterMethoid<GetterPolicies, OT, MemberType *const(OT::*)> : public core::BuiltinClosure_O {
  typedef core::BuiltinClosure_O TemplatedBase;

private:
  typedef clbind::Wrapper<MemberType> WrapperType;
  string _Name;
  typedef MemberType *const(OT::*VariablePtrType);
  VariablePtrType _MemberPtr;

public:
  GetterMethoid(core::T_sp name, VariablePtrType p) : BuiltinClosure_O(name), _MemberPtr(p){};
  DISABLE_NEW();
  LCC_RETURN LISP_CALLING_CONVENTION() {
    ASSERT_LCC_VA_LIST_CLOSURE_DEFINED(lcc_arglist);
    OT *objPtr = gc::As<core::WrappedPointer_sp>((LCC_ARG0()))->cast<OT>();
    MemberType *ptr = (*objPtr).*(this->_MemberPtr);
    return translate::to_object<MemberType *, translate::dont_adopt_pointer>::convert(ptr);
  }
};
};

template <typename GetterPolicies, typename OT, typename VariablePtrType>
class gctools::GCKind<clbind::GetterMethoid<GetterPolicies, OT, VariablePtrType>> {
public:
  static gctools::GCKindEnum const Kind = gctools::GCKind<typename clbind::GetterMethoid<GetterPolicies, OT, VariablePtrType>::TemplatedBase>::Kind;
};

#endif
