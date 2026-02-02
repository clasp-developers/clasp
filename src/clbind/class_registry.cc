/*
    File: class_registry.cc
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
// Copyright (c) 2004 Daniel Wallin

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
#include <clasp/core/object.h>
#include <clasp/core/hashTableEql.h>

#include <clasp/core/lisp.h>

#include <clasp/clbind/clbind.h>
#include <clasp/core/symbolTable.h>
#include <clasp/clbind/class_registry.h>
#include <clasp/clbind/class_rep.h>
// #include <clasp/clbind/detail/operator_id.h>
#include <clasp/core/wrappers.h>
namespace clbind {

void ClassRegistry_O::initialize() {
  this->Base::initialize();
  this->m_classes = core::HashTable_O::createEql();
}

ClassRegistry_sp ClassRegistry_O::get_registry() {
  SYMBOL_EXPORT_SC_(ClbindPkg, STARtheClassRegistrySTAR);
  return gc::As<ClassRegistry_sp>(clbind::_sym_STARtheClassRegistrySTAR->symbolValue());
}

core::Integer_sp type_id_toClassRegistryKey(std::type_index const& info) {
  core::Integer_sp p = core::Integer_O::create(info.hash_code());
  return p;
}

void ClassRegistry_O::add_class(std::type_index const& info, ClassRep_sp crep) {
  core::Integer_sp key = type_id_toClassRegistryKey(info);
  // ASSERTF(!this->m_classes->contains(key), "You are trying to register the class {} twice", info.name());
  this->m_classes->setf_gethash(key, crep);
}

ClassRep_sp ClassRegistry_O::find_class(std::type_index const& info) const {
  core::Integer_sp key = type_id_toClassRegistryKey(info);
  core::T_sp value = this->m_classes->gethash(key, nil<core::T_O>());
  if (value.nilp()) {
    SIMPLE_ERROR("Could not find class for typeid: {} name: {}", _rep_(key), info.name());
  }
  return gc::As<ClassRep_sp>(value);
}

} // namespace clbind
