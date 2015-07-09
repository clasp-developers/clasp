/*
    File: scope.cc
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

#include <clasp/clbind/cl_include.h>

#include <clasp/clbind/scope.h>
//#include <clasp/clbind/detail/debug.hpp>
//#include <clasp/clbind/detail/stack_utils.hpp>
#include <cassert>
#include <clasp/core/foundation.h>
#include <clasp/core/arguments.h>
#include <clasp/core/package.h>
#include <clasp/core/symbolTable.h>

namespace clbind {
namespace detail {

registration::registration()
    : m_next(0) {
}

registration::~registration() {
  delete m_next;
}

} // namespace detail

scope::scope()
    : m_chain(0) {
}

scope::scope(std::auto_ptr<detail::registration> reg)
    : m_chain(reg.release()) {
}

scope::scope(scope const &other)
    : m_chain(other.m_chain) {
  const_cast<scope &>(other).m_chain = 0;
}

scope &scope::operator=(scope const &other_) {
  delete m_chain;
  m_chain = other_.m_chain;
  const_cast<scope &>(other_).m_chain = 0;
  return *this;
}

scope::~scope() {
  delete m_chain;
}

scope &scope::operator, (scope s) {
  if (!m_chain) {
    m_chain = s.m_chain;
    s.m_chain = 0;
    return *this;
  }

  for (detail::registration *c = m_chain;; c = c->m_next) {
    if (!c->m_next) {
      c->m_next = s.m_chain;
      s.m_chain = 0;
      break;
    }
  }

  return *this;
}

void scope::register_() const {
  for (detail::registration *r = m_chain; r != 0; r = r->m_next) {
    r->register_();
  }
}

} // namespace clbind

namespace clbind {

package_::package_(string const &name, std::list<std::string> nicknames, std::list<string> usePackageNames)
    : m_name(name), m_nicknames(nicknames), m_usePackageNames(usePackageNames) {
}

void package_::operator[](scope s) {
  string packageName = m_name;
  core::Package_sp pkg = _lisp->findPackage(packageName);
  if (pkg.nilp()) {
    pkg = _lisp->makePackage(packageName, m_nicknames, m_usePackageNames);
  }
  core::DynamicScopeManager lispScope(cl::_sym_STARpackageSTAR, pkg);
  s.register_();
}

} // namespace clbind
