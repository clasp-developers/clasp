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

#ifndef CLBIND_CLASS_REP_HPP_INCLUDED
#define CLBIND_CLASS_REP_HPP_INCLUDED

#include <boost/limits.hpp>
#include <boost/preprocessor/repetition/enum_params_with_a_default.hpp>

#include <string>
#include <utility>
#include <vector>

#include <clasp/clbind/config.h>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/builtInClass.h>
#include <clasp/core/numbers.h>
//#include <clasp/clbind/lua_include.hpp>
//#include <clasp/clbind/detail/object_rep.hpp>
//#include <clasp/clbind/detail/garbage_collector.hpp>
//#include <clasp/clbind/detail/operator_id.hpp>
#include <clasp/clbind/class_registry.h>
//#include <clasp/clbind/error.hpp>
//#include <clasp/clbind/handle.hpp>
#include <clasp/clbind/primitives.h>
#include <clasp/clbind/typeid.h>
//#include <clasp/clbind/detail/ref.hpp>

namespace clbind {
namespace detail {
class cast_graph;
class class_id_map;
};
};

namespace clbind {

CLBIND_API std::string stack_content_by_name(core::Lisp_sp L, int start_index);

struct class_registration;

struct conversion_storage;

class ClassRep_O : public core::BuiltInClass_O {
  LISP_META_CLASS(StandardClass);
  LISP_BASE1(core::BuiltInClass_O);
  LISP_CLASS(clbind, ClbindPkg, ClassRep_O, "ClassRep");

  friend struct class_registration;

public:
  bool cxxClassP() const { return true; };
  bool cxxDerivableClassP() const { return this->m_derivable; };
  bool primaryCxxDerivableClassP() const { return this->getCreator()->duplicationLevel() == 0; };

  ClassRep_O() : m_derivable(false){};

  ClassRep_O(type_id const &type, const std::string &name, bool derivable);

  ClassRep_O(const std::string &name, bool derivable);

  virtual ~ClassRep_O();

public:
  static ClassRep_sp create(type_id const &mtype, const std::string &name, bool derivable) {
    GC_ALLOCATE_VARIADIC(ClassRep_O, val, mtype, name, derivable);
    return val;
  }

#if 0
        std::pair<void*,void*> allocate() const;

        // this is called as metamethod __call on the ClassRep_O.
        static int constructor_dispatcher();
        struct base_info
        {
            core::Fixnum_sp pointer_offset; // the offset added to the pointer to obtain a basepointer (due to multiple-inheritance)
            ClassRep_sp base;
        };

#endif
  void add_base_class(core::Fixnum_sp pointer_offset, ClassRep_sp base);

  const gctools::Vec0<core::Cons_sp> &bases() const throw() { return m_bases; }

  void set_type(type_id const &t) { m_type = t; }
  type_id const &type() const throw() { return m_type; }

  std::string name() const throw() { return m_name; }

#if 0 // begin_meister_disabled
        // the lua reference to the metatable for this class' instances
        int metatable_ref() const throw() { return m_instance_metatable; }

        void get_table() const { m_table.push(); }
        void get_default_table(core::Lisp_sp L) const { m_default_table.push(L); }

        class_type get_class_type() const { return m_class_type; }

        void add_static_constant(const char* name, int val);

        static int super_callback(core::Lisp_sp L);

        static int lua_settable_dispatcher(core::Lisp_sp L);

        // called from the metamethod for __index
        // obj is the object pointer
        static int static_class_gettable(core::Lisp_sp L);

        bool has_operator_in_lua(core::Lisp_sp, int id);
#endif

  detail::cast_graph const &casts() const {
    return *m_casts;
  }

  detail::class_id_map const &classes() const {
    return *m_classes;
  }

GCPRIVATE:

#if 0
        void cache_operators(core::Lisp_sp);
#endif
  // this is a pointer to the type_info structure for
  // this type
  // warning: this may be a problem when using dll:s, since
  // typeid() may actually return different pointers for the same
  // type.
  type_id m_type;

  // a list of info for every class this class derives from
  // the information stored here is sufficient to do
  // type casts to the base classes
  gctools::Vec0<core::Cons_sp> m_bases;

  // the class' name (as given when registered to lua with class_)
  std::string m_name;

  detail::cast_graph *m_casts;
  /* What does this store???? */
  detail::class_id_map *m_classes;
  bool m_derivable;
};

bool is_class_rep(core::Lisp_sp L, int index);
}
template <>
struct gctools::GCInfo<clbind::ClassRep_O> {
  static bool constexpr NeedsInitialization = true;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

//#include <clasp/clbind/detail/overload_rep_impl.hpp>

#endif // CLBIND_CLASS_REP_HPP_INCLUDED
