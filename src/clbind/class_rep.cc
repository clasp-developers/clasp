/*
    File: class_rep.cc
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

#define CLBIND_BUILDING

#include <clasp/core/foundation.h>
#include <clasp/core/package.h>
#include <clasp/clbind/cl_include.h>

// #include <clasp/clbind/detail/stack_utils.hpp>
// #include <clasp/clbind/detail/conversion_storage.hpp>
#include <clasp/clbind/clbind.h>
// #include <clasp/clbind/exception_handler.hpp>
// #include <clasp/clbind/get_main_thread.hpp>
// #include <utility>
#include <clasp/clbind/class_rep.h>
#include <clasp/core/wrappers.h>

using namespace clbind::detail;

namespace clbind {

ClassRep_O::ClassRep_O(core::Instance_sp class_, core::Symbol_sp name, bool derivable)
    : Instance_O(class_ /*,REF_CLASS_NUMBER_OF_SLOTS_IN_STANDARD_CLASS*/), m_name(name)
      //	, m_class_type(cpp_class)
      //	, m_operator_cache(0)
      ,
      m_casts(globalCastGraph) // Meister - luabind did this
      ,
      m_classes(globalClassIdMap) // Meister - luabind did this
      ,
      m_derivable(derivable) {
}

void ClassRep_O::add_base_class(core::Fixnum_sp pointer_offset, ClassRep_sp base)
// const ClassRep_O::base_info& binfo)
{
  // If you hit this assert you are deriving from a type that is not registered
  // in cl. That is, in the class_<> you are giving a baseclass that isn't registered.
  // Please note that if you don't need to have access to the base class or the
  // conversion from the derived class to the base class, you don't need
  // to tell clbind that it derives.
  ASSERTF(base.objectp(), "You cannot derive from an unregistered type");

  // also, save the baseclass info to be used for typecasts
  core::Cons_sp binfo = core::Cons_O::create(pointer_offset, base);
  m_bases.push_back(binfo);
}
}; // namespace clbind
