/*
    File: memberFunction.h
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
#ifndef clbind_memberFunction_H
#define clbind_memberFunction_H

#include <clasp/clbind/clbind_wrappers.h>
#include <clasp/clbind/policies.h>
#include <clasp/clbind/details.h>

namespace clbind {

template <typename Pols, typename OT, typename MethodPtrType>
class IndirectVariadicMethoid : public core::Functoid {
public:
  typedef Functoid TemplatedBase;
  virtual size_t templatedSizeof() const { return sizeof(*this); };
};

#ifdef BUILDING_CLASP
#include <clbind_methoids.h>
#else
#ifdef USE_CLASP_DEBUG
#ifdef USE_CLASP_BOEHM
#include <clasp/clbind/generated/debug/boehm/clbind_methoids.h>
#else
#include <clasp/clbind/generated/debug/mps/clbind_methoids.h>
#endif
#else
#ifdef USE_CLASP_BOEHM
#include <clasp/clbind/generated/release/boehm/clbind_methoids.h>
#else
#include <clasp/clbind/generated/release/mps/clbind_methoids.h>
#endif
#endif
#endif

#ifdef BUILDING_CLASP
#include <clbind_static_members.h>
#else
#ifdef USE_CLASP_DEBUG
#ifdef USE_CLASP_BOEHM
#include <clasp/clbind/generated/debug/boehm/clbind_static_members.h>
#else
#include <clasp/clbind/generated/debug/mps/clbind_static_members.h>
#endif
#else
#ifdef USE_CLASP_BOEHM
#include <clasp/clbind/generated/release/boehm/clbind_static_members.h>
#else
#include <clasp/clbind/generated/release/mps/clbind_static_members.h>
#endif
#endif
#endif
};

template <typename Pols, typename OT, typename MethodPtrType>
class gctools::GCKind<clbind::IndirectVariadicMethoid<Pols, OT, MethodPtrType>> {
public:
  static gctools::GCKindEnum const Kind = gctools::GCKind<typename clbind::IndirectVariadicMethoid<Pols, OT, MethodPtrType>::TemplatedBase>::Kind;
};

#endif
