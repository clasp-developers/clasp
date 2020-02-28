/*
    File: derivable_class.cc
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
#include <clasp/core/array.h>
//#include <clasp/clbind/lua_include.hpp>
#include <clasp/clbind/config.h>
#include <clasp/clbind/scope.h>
#include <clasp/clbind/clbind_wrappers.h>
#include <clasp/clbind/class.h>
#include <clasp/clbind/derivable_class.h>
#include <clasp/clbind/primitives.h>
#include <clasp/clbind/class_registry.h>
#include <clasp/clbind/class_rep.h>
#include <clasp/clbind/nil.h>

#include <cstring>
#include <iostream>

namespace clbind {

void validateRackOffset(size_t wrapped_type_offset) {
    /*
     * printf("%s:%d The Derivable class _Rack offset (%lu) must be at the same offset as Instance_O::_Rack (%lu)\n",
             __FILE__, __LINE__, wrapped_type_offset, offsetof(core::Instance_O,_Rack));
     */
    if (wrapped_type_offset != offsetof(core::Instance_O,_Rack)) {
      printf("The Derivable class _Rack offset (%lu) must be at the same offset as Instance_O::_Rack (%lu) - but it is not\n",
             wrapped_type_offset, offsetof(core::Instance_O,_Rack));
      abort();
    };
  };

};

namespace clbind {
namespace detail {

derivable_class_registration::derivable_class_registration(char const *name) : m_default_constructor(NULL) {
  m_name = name;
}

void derivable_class_registration::register_() const {
  ClassRegistry_sp registry = ClassRegistry_O::get_registry();
  clbind::ClassRep_sp crep = clbind::ClassRep_O::create(core::lisp_derivable_cxx_class(), this->m_type, this->m_name, this->m_derivable);
#ifdef DEBUG_CLASS_INSTANCE
  printf("%s:%d:%s   Registering clbind class\n", __FILE__, __LINE__, __FUNCTION__ );
#endif
  crep->initializeSlots(crep->_Class->CLASS_stamp_for_instances(),REF_CLASS_NUMBER_OF_SLOTS_IN_DERIVABLE_CXX_CLASS);
  std::string classNameString(this->m_name);
  gctools::smart_ptr<core::Creator_O> creator;
  if (m_default_constructor != NULL) {
    creator = m_default_constructor->registerDefaultConstructor_();
  } else {
    creator = gctools::GC<DummyCreator_O>::allocate(classNameString);
  }
  crep->initializeClassSlots(creator,gctools::NextStampWtag(gctools::Header_s::derivable_wtag));
  core::Symbol_sp className = core::lispify_intern(classNameString, _lisp->getCurrentPackage()->packageName());
  className->exportYourself();
  crep->_setClassName(className);
  reg::lisp_associateClassIdWithClassSymbol(m_id, className); // TODO: Or do I want m_wrapper_id????
  lisp_pushClassSymbolOntoSTARallCxxClassesSTAR(className);
  core__setf_find_class(crep, className);
  registry->add_class(m_type, crep);

  class_map_put(m_id, crep);

  bool const has_wrapper = m_wrapper_id != reg::registered_class<reg::null_type>::id;
#if 0
            if (has_wrapper) {
                printf("%s:%d:%s   class[%s] has wrapper\n", __FILE__,__LINE__,__FUNCTION__,m_name);
            } else {
                printf("%s:%d:%s   class[%s] does not have wrapper\n", __FILE__,__LINE__,__FUNCTION__,m_name);
            }
#endif
  class_map_put(m_wrapper_id, crep);

  m_members.register_();

  cast_graph *const casts = globalCastGraph;
  class_id_map *const class_ids = globalClassIdMap;

  class_ids->put(m_id, m_type);
  if (has_wrapper)
    class_ids->put(m_wrapper_id, m_wrapper_type);

  BOOST_FOREACH (cast_entry const &e, m_casts) {
    casts->insert(e.src, e.target, e.cast);
  }

//  printf("%s:%d Registering Derivable class %s\n", __FILE__, __LINE__, _rep_(className).c_str());
  if (m_bases.size() == 0) {
    // If no base classes are specified then make T a base class from Common Lisp's point of view
    //
    printf("%s:%d           %s inherits from T\n", __FILE__, __LINE__, _rep_(className).c_str());
    crep->addInstanceBaseClass(core::_sym_derivable_cxx_object);
  } else {
    for (std::vector<base_desc>::iterator i = m_bases.begin();
         i != m_bases.end(); ++i) {
      //            CLBIND_CHECK_STACK(L);

      // the baseclass' class_rep structure
      ClassRep_sp bcrep = registry->find_class(i->first);
//      printf("%s:%d         %s inherits from %s\n", __FILE__, __LINE__, _rep_(className).c_str(), _rep_(bcrep).c_str());
      ASSERTF(bcrep.notnilp(), BF("Could not find base class %s") % i->first.name());
      // Add it to the DirectSuperClass list
      crep->addInstanceBaseClass(bcrep->_className());
      crep->add_base_class(core::make_fixnum(0), bcrep);
    }
    crep->addInstanceBaseClass(core::_sym_derivable_cxx_object);
  }
}

// -- interface ---------------------------------------------------------

derivable_class_base::derivable_class_base(char const *name)
    : scope(std::unique_ptr<registration>(
          m_registration = new derivable_class_registration(name))) {
}

void derivable_class_base::init(
    type_id const &type_id_, class_id id, type_id const &wrapper_type, class_id wrapper_id, bool derivable) {
  m_registration->m_type = type_id_;
  m_registration->m_id = id;
  m_registration->m_wrapper_type = wrapper_type;
  m_registration->m_wrapper_id = wrapper_id;
  m_registration->m_derivable = derivable;
}

void derivable_class_base::add_base(type_id const &base, cast_function cast) {
  m_registration->m_bases.push_back(std::make_pair(base, cast));
}

void derivable_class_base::set_default_constructor(registration *member) {
  //            std::auto_ptr<registration> ptr(member);
  m_registration->m_default_constructor = member;
}

void derivable_class_base::add_member(registration *member) {
  std::unique_ptr<registration> ptr(member);
  m_registration->m_members.operator, (scope(std::move(ptr)));
}

void derivable_class_base::add_default_member(registration *member) {
  std::unique_ptr<registration> ptr(member);
  m_registration->m_default_members.operator, (scope(std::move(ptr)));
}

const char *derivable_class_base::name() const {
  return m_registration->m_name;
}

void derivable_class_base::add_static_constant(const char *name, int val) {
  m_registration->m_static_constants[name] = val;
}

void derivable_class_base::add_inner_scope(scope &s) {
  m_registration->m_scope.operator, (s);
}

void derivable_class_base::add_cast(
    class_id src, class_id target, cast_function cast) {
  //            printf("%s:%d:%s   src[%" PRu "] target[%lu]\n", __FILE__,__LINE__,__FUNCTION__,src,target);
  m_registration->m_casts.push_back(cast_entry(src, target, cast));
}
}

} // namespace clbind::detail
