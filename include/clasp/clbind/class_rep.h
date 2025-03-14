#pragma once

/*
    File: class_rep.h
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
// Copyright (c) 2003 Daniel Wallin and Arvid Norberg

// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
// ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
// TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
// PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
// SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
// ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
// OR OTHER DEALINGS IN THE SOFTWARE.

#include <boost/limits.hpp>

#include <string>
#include <utility>
#include <vector>

#include <clasp/clbind/config.h>
#include <clasp/core/object.h>
#include <clasp/core/instance.h>
#include <clasp/core/numbers.h>
#include <clasp/core/symbol.h>
#include <clasp/clbind/class_registry.h>
#include <clasp/clbind/primitives.h>
#include <clasp/clbind/typeid.h>

namespace clbind {
namespace detail {
class cast_graph;
class class_id_map;
}; // namespace detail
}; // namespace clbind

namespace clbind {
class ClassRep_O;
};

template <> struct gctools::GCInfo<clbind::ClassRep_O> {
  static bool constexpr NeedsInitialization = true;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};
namespace clbind {

CLBIND_API std::string stack_content_by_name(core::LispPtr L, int start_index);

struct class_registration;

struct conversion_storage;

class ClassRep_O : public core::Instance_O {
  LISP_CLASS(clbind, ClbindPkg, ClassRep_O, "ClassRep", core::Instance_O);
  // I may want to change this back to have the metaclass standard-class
  friend struct class_registration;

public:
  bool cxxClassP() const override { return true; };
  bool cxxDerivableClassP() const override { return this->m_derivable; };

#if 0
 ClassRep_O() : Instance_O(core::lisp_class_rep_class()/*,REF_CLASS_NUMBER_OF_SLOTS_IN_STANDARD_CLASS*/) {
    printf("%s:%d:%s  create class\n", __FILE__, __LINE__, __FUNCTION__ );
  };
#endif

  ClassRep_O(core::Instance_sp c) : Instance_O(c) { printf("%s:%d:%s  create class\n", __FILE__, __LINE__, __FUNCTION__); };

  ClassRep_O(core::Instance_sp class_, type_id const& type, core::Symbol_sp name, bool derivable);

#if 0
  ClassRep_O(const std::string &name, bool derivable);
#endif

public:
  static ClassRep_sp create(core::Instance_sp class_, type_id const& mtype, core::Symbol_sp name, bool derivable) {
    auto val = gctools::GC<ClassRep_O>::allocate(class_, mtype, name, derivable);
    return val;
  }
  void add_base_class(core::Fixnum_sp pointer_offset, ClassRep_sp base);

  const gctools::Vec0<core::Cons_sp>& bases() const throw() { return m_bases; }

  void set_type(type_id const& t) { m_type = t; }
  type_id const& type() const throw() { return m_type._value; }

  std::string name_() const throw() { return m_name->symbolNameAsString(); }

  detail::cast_graph const& casts() const { return *m_casts._value; }

  detail::class_id_map const& classes() const { return *m_classes._value; }

public:
  // this is a pointer to the type_info structure for
  // this type
  // warning: this may be a problem when using dll:s, since
  // typeid() may actually return different pointers for the same
  // type.
  dont_expose<type_id> m_type;

  // a list of info for every class this class derives from
  // the information stored here is sufficient to do
  // type casts to the base classes
  gctools::Vec0<core::Cons_sp> m_bases;

  // the class' name (as given when registered to lua with class_)
  core::Symbol_sp m_name;

  dont_expose<detail::cast_graph*> m_casts;
  /* What does this store???? */
  dont_expose<detail::class_id_map*> m_classes;
  bool m_derivable;
};

bool is_class_rep(core::LispPtr L, int index);
} // namespace clbind

// #include <clasp/clbind/detail/overload_rep_impl.hpp>
