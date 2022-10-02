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

  extern void trapGetterMethoid();

};

namespace clbind {

template <typename GetterPolicies, typename OT, typename VariablePtrType>
class TEMPLATED_FUNCTION_GetterMethoid;
#include <clasp/clbind/getterTop.h>
#include <clasp/clbind/getterBot.h>

template <typename SetterPolicies, typename OT, typename VariablePtrType>
class TEMPLATED_FUNCTION_SetterMethoid;
#include <clasp/clbind/setterTop.h>
#include <clasp/clbind/setterBot.h>
};

template <typename GetterPolicies, typename OT, typename VariablePtrType >
class gctools::GCStamp<clbind::TEMPLATED_FUNCTION_GetterMethoid<GetterPolicies, OT, VariablePtrType >> {
public:
  virtual size_t templatedSizeof() const { return sizeof(*this); };
  
public:
  static gctools::GCStampEnum const StampWtag = gctools::GCStamp<typename clbind::TEMPLATED_FUNCTION_GetterMethoid<GetterPolicies, OT, VariablePtrType >::TemplatedBase>::StampWtag;
};

template <typename SetterPolicies, typename OT, typename VariablePtrType>
class gctools::GCStamp<clbind::TEMPLATED_FUNCTION_SetterMethoid<SetterPolicies, OT, VariablePtrType>> {
public:
  virtual size_t templatedSizeof() const { return sizeof(*this); };

public:
  static gctools::GCStampEnum const StampWtag = gctools::GCStamp<typename clbind::TEMPLATED_FUNCTION_SetterMethoid<SetterPolicies, OT, VariablePtrType>::TemplatedBase>::StampWtag;
};

#endif
