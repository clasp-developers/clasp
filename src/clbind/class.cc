/*
    File: class.cc
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
// Copyright (c) 2004 Daniel Wallin and Arvid Norberg

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

#define CLBIND_BUILDING

#include <boost/foreach.hpp>

#include <clasp/core/foundation.h>
#include <clasp/core/package.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/str.h>
#include <clasp/clbind/config.h>
#include <clasp/clbind/scope.h>
#include <clasp/clbind/clbind_wrappers.h>
#include <clasp/clbind/class.h>
#include <clasp/clbind/primitives.h>
#include <clasp/clbind/class_registry.h>
#include <clasp/clbind/class_rep.h>
#include <clasp/clbind/nil.h>

#include <cstring>
#include <iostream>

namespace clbind {
CLBIND_API detail::nil_type nil;
default_constructor globalDefaultConstructorSignature;
}

namespace clbind {
namespace detail {

class_registration::class_registration(const std::string &name) : m_default_constructor(NULL) {
  m_name = name;
}

void class_registration::register_() const {
  ClassRegistry_sp registry = ClassRegistry_O::get_registry();
  clbind::ClassRep_sp crep = clbind::ClassRep_O::create(this->m_type, this->m_name, this->m_derivable);
  std::string classNameString(this->m_name);
  core::Symbol_sp className = core::lispify_intern(classNameString, _lisp->getCurrentPackage()->packageName());
  className->exportYourself();
  crep->setName(className);
  reg::lisp_associateClassIdWithClassSymbol(m_id, className); // TODO: Or do I want m_wrapper_id????
  if (core::_sym_STARallCxxClassesSTAR->symbolValueUnsafe()) {
    core::_sym_STARallCxxClassesSTAR->setf_symbolValue(
        core::Cons_O::create(className, core::_sym_STARallCxxClassesSTAR->symbolValue()));
  }
  gctools::tagged_pointer<core::Creator> allocator;
  if (m_default_constructor != NULL) {
    allocator = m_default_constructor->registerDefaultConstructor_();
  } else {
    allocator = gctools::ClassAllocator<DummyCreator>::allocateClass(classNameString);
  }
  _lisp->addClass(className, crep, allocator);
  registry->add_class(m_type, crep);

  detail::class_map &classes = *globalClassMap;
  classes.put(m_id, crep);

  bool const has_wrapper = m_wrapper_id != reg::registered_class<reg::null_type>::id;
#if 0
            if (has_wrapper) {
                printf("%s:%d:%s   class[%s] has wrapper\n", __FILE__,__LINE__,__FUNCTION__,m_name);
            } else {
                printf("%s:%d:%s   class[%s] does not have wrapper\n", __FILE__,__LINE__,__FUNCTION__,m_name);
            }
#endif
  classes.put(m_wrapper_id, crep);

  m_members.register_();

  cast_graph *const casts = globalCastGraph;
  class_id_map *const class_ids = globalClassIdMap;

  class_ids->put(m_id, m_type);
  if (has_wrapper)
    class_ids->put(m_wrapper_id, m_wrapper_type);

  BOOST_FOREACH (cast_entry const &e, m_casts) {
    casts->insert(e.src, e.target, e.cast);
  }

  if (m_bases.size() == 0) {
    // If no base classes are specified then make T a base class from Common Lisp's point of view
    //
    crep->addInstanceBaseClass(cl::_sym_T_O);
  } else {
    for (std::vector<base_desc>::iterator i = m_bases.begin();
         i != m_bases.end(); ++i) {
      //            CLBIND_CHECK_STACK(L);

      // the baseclass' class_rep structure
      ClassRep_sp bcrep = registry->find_class(i->first);
      ASSERTF(bcrep.notnilp(), BF("Could not find base class %s") % i->first.name());
      // Add it to the DirectSuperClass list
      crep->addInstanceBaseClass(bcrep->className());
      crep->add_base_class(core::make_fixnum(0), bcrep);
    }
  }
}

// -- interface ---------------------------------------------------------

class_base::class_base(const string &name)
    : scope(std::auto_ptr<registration>(
          m_registration = new class_registration(name))) {
}

void class_base::init(
    type_id const &type_id_, class_id id, type_id const &wrapper_type, class_id wrapper_id, bool derivable) {
  m_registration->m_type = type_id_;
  m_registration->m_id = id;
  m_registration->m_wrapper_type = wrapper_type;
  m_registration->m_wrapper_id = wrapper_id;
  m_registration->m_derivable = derivable;
}

void class_base::add_base(type_id const &base, cast_function cast) {
  m_registration->m_bases.push_back(std::make_pair(base, cast));
}

void class_base::set_default_constructor(registration *member) {
  //            std::auto_ptr<registration> ptr(member);
  m_registration->m_default_constructor = member;
}

void class_base::add_member(registration *member) {
  std::auto_ptr<registration> ptr(member);
  m_registration->m_members.operator, (scope(ptr));
}

void class_base::add_default_member(registration *member) {
  std::auto_ptr<registration> ptr(member);
  m_registration->m_default_members.operator, (scope(ptr));
}

string class_base::name() const {
  return m_registration->m_name;
}

void class_base::add_static_constant(const char *name, int val) {
  m_registration->m_static_constants[name] = val;
}

void class_base::add_inner_scope(scope &s) {
  m_registration->m_scope.operator, (s);
}

void class_base::add_cast(
    class_id src, class_id target, cast_function cast) {
  //            printf("%s:%d:%s   src[%lu] target[%lu]\n", __FILE__,__LINE__,__FUNCTION__,src,target);
  m_registration->m_casts.push_back(cast_entry(src, target, cast));
}

void add_custom_name(type_id const &i, std::string &s) {
  s += " [";
  s += i.name();
  s += "]";
}

std::string get_class_name(core::Lisp_sp L, type_id const &i) {
  IMPLEMENT_MEF(BF("get_class_name"));
#if 0  // start_meister_disabled
            std::string ret;

            assert(L);

            class_registry* r = class_registry::get_registry(L);
            class_rep* crep = r->find_class(i);

            if (crep == 0)
            {
                ret = "custom";
                add_custom_name(i, ret);
            }
            else
            {
                /* TODO reimplement this?
                   if (i == crep->holder_type())
                   {
                   ret += "smart_ptr<";
                   ret += crep->name();
                   ret += ">";
                   }
                   else if (i == crep->const_holder_type())
                   {
                   ret += "smart_ptr<const ";
                   ret += crep->name();
                   ret += ">";
                   }
                   else*/
                {
                    ret += crep->name();
                }
            }
            return ret;
#endif // end_meister_disabled
}
}

} // namespace clbind::detail
