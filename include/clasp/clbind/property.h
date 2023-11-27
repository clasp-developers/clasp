#pragma once

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

#include <clasp/core/translators.h>

#include <clasp/clbind/clbind_wrappers.h>
#include <clasp/clbind/policies.h>
#include <clasp/clbind/details.h>

namespace clbind {

template <typename T> struct memberpointertraits {};

template <typename M, typename C> struct memberpointertraits<M C::*> {
  typedef M member_type;
  typedef C class_type;
};

extern void trapGetterMethoid();

}; // namespace clbind

namespace clbind {

template <typename GetterPolicies, typename OT, typename VariablePtrType> class WRAPPER_Getter;
#include <clasp/clbind/getterTop.h>
#include <clasp/clbind/getterBot.h>

template <typename SetterPolicies, typename OT, typename VariablePtrType> class WRAPPER_Setter;
#include <clasp/clbind/setterTop.h>
#include <clasp/clbind/setterBot.h>
}; // namespace clbind

template <typename Policies, typename OT, typename VariablePtrType>
class gctools::GCStamp<clbind::WRAPPER_Getter<Policies, OT, VariablePtrType>> {
public:
  static gctools::GCStampEnum const StampWtag =
      gctools::GCStamp<typename clbind::WRAPPER_Getter<Policies, OT, VariablePtrType>::TemplatedBase>::StampWtag;
};

template <typename Policies, typename OT, typename VariablePtrType>
struct gctools::Inherits<typename clbind::WRAPPER_Getter<Policies, OT, VariablePtrType>::TemplatedBase,
                         clbind::WRAPPER_Getter<Policies, OT, VariablePtrType>> : public std::true_type {};

template <typename Policies, typename OT, typename VariablePtrType>
class gctools::GCStamp<clbind::WRAPPER_Setter<Policies, OT, VariablePtrType>> {
public:
  static gctools::GCStampEnum const StampWtag =
      gctools::GCStamp<typename clbind::WRAPPER_Setter<Policies, OT, VariablePtrType>::TemplatedBase>::StampWtag;
};

template <typename Policies, typename OT, typename VariablePtrType>
struct gctools::Inherits<typename clbind::WRAPPER_Setter<Policies, OT, VariablePtrType>::TemplatedBase,
                         clbind::WRAPPER_Setter<Policies, OT, VariablePtrType>> : public std::true_type {};
