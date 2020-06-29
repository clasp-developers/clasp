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

//#include <clasp/clbind/detail/debug.hpp>
//#include <clasp/clbind/detail/stack_utils.hpp>
#include <cassert>
#include <clasp/core/foundation.h>
#include <clasp/clbind/function.h>
#include <clasp/clbind/scope.h>
#include <clasp/clbind/cl_include.h>
#include <clasp/core/arguments.h>
#include <clasp/core/package.h>
#include <clasp/core/ql.h>
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

scope_::scope_()
    : m_chain(0) {
  LOG_SCOPE(("%s:%d scope_::scope_ %p m_chain-> %p\n", __FILE__, __LINE__, this, m_chain));
}

scope_::scope_(std::unique_ptr<detail::registration> reg)
    : m_chain(reg.release()) {
  LOG_SCOPE(("%s:%d scope_::scope_ %p m_chain-> %p\n", __FILE__, __LINE__, this, m_chain));
}

scope_::scope_(scope_ const &other)
    : m_chain(other.m_chain) {
  LOG_SCOPE(("%s:%d copy ctor scope_::scope_ %p m_chain-> %p  other= %p  other.m_chain(was %p) set to 0\n", __FILE__, __LINE__, this, m_chain, &other, other.m_chain));
  const_cast<scope_ &>(other).m_chain = 0;
}

scope_ &scope_::operator=(scope_ const &other_) {
  delete m_chain;
  m_chain = other_.m_chain;
  LOG_SCOPE(("%s:%d operator= scope_::scope_ %p m_chain-> %p  other= %p  other.m_chain set to 0\n", __FILE__, __LINE__, this, m_chain, &other_));
  const_cast<scope_ &>(other_).m_chain = 0;
  return *this;
}

scope_::~scope_() {
  delete m_chain;
}


scope_ &scope_::operator, (scope_ s) {
  if (!m_chain) {
    m_chain = s.m_chain;
#ifdef DEBUG_SCOPE
    if (s.m_chain) {
      printf("%s:%d   Setting scope %p %s/%s  to chain of %p\n", __FILE__, __LINE__, s.m_chain, s.m_chain->kind().c_str(), s.m_chain->name().c_str(), this);
    } else {
      printf("%s:%d   Appending NULL scope to chain of %p\n",  __FILE__, __LINE__, this);
    }
#endif
    s.m_chain = 0;
    return *this;
  }

  LOG_SCOPE(("%s:%d   Head of list registration entry %p to chain of %p\n",  __FILE__, __LINE__, m_chain, this));
  for (detail::registration *c = m_chain;; c = c->m_next) {
    if (!c->m_next) {
#ifdef DEBUG_SCOPE      
      if (s.m_chain) {
        printf("%s:%d   Appending scope %p %s/%s  to chain of %p\n", __FILE__, __LINE__, s.m_chain, s.m_chain->kind().c_str(), s.m_chain->name().c_str(), this);
      } else {
        printf("%s:%d   Appending NULL scope to chain of %p\n",  __FILE__, __LINE__, this);
      }
#endif
      c->m_next = s.m_chain;
      s.m_chain = 0;
      break;
    } else {
      LOG_SCOPE(("%s:%d   Skipping registration entry %p %s/%s to chain of %p\n",  __FILE__, __LINE__, c, c->kind().c_str(), c->name().c_str(), this));
    }
  }

  return *this;
}


void scope_::register_() const {
  LOG_SCOPE(("%s:%d  register_ scope -> %p\n", __FILE__, __LINE__, this ));
  for (detail::registration *r = m_chain; r != 0; r = r->m_next) {
    LOG_SCOPE(("%s:%d register_ r-> %p  %s/%s\n", __FILE__, __LINE__, r, r->kind().c_str(), r->name().c_str()));
    r->register_();
  }
}

} // namespace clbind

namespace clbind {

package_::package_(string const &name, std::list<std::string> nicknames, std::list<string> usePackageNames)
    : m_name(name), m_nicknames(nicknames), m_usePackageNames(usePackageNames) {
  string packageName = m_name;
  core::T_sp pkg = _lisp->findPackage(packageName);
  if (pkg.nilp()) {
    pkg = _lisp->makePackage(packageName, m_nicknames, m_usePackageNames);
  } else {
    core::Package_sp ppkg = gc::As_unsafe<core::Package_sp>(pkg);
    ql::list l;
    if (m_nicknames.size()!=0) {
      for ( auto nick : m_nicknames) {
        l << _lisp->findPackage(nick);
      }
      ppkg->setNicknames(l.cons());
    }
    if (m_usePackageNames.size()!=0) {
      for ( auto usen : m_usePackageNames) {
        core::Package_sp other = gc::As<core::Package_sp>(_lisp->findPackage(usen));
        bool used = ppkg->usePackage(other);
        if (!used) {
          printf("There was a problem (conflicting symbols?) using package %s in package %s\n", usen.c_str(), packageName.c_str());
        }
      }
    }
  }
  this->_PackageDynamicVariable = new core::DynamicScopeManager(cl::_sym_STARpackageSTAR, pkg);
  LOG_SCOPE(("%s:%d package scope_ -> %p\n", __FILE__, __LINE__, &this->_Scope));
}

package_::~package_() {
  _Scope.register_();
  delete this->_PackageDynamicVariable;
};

} // namespace clbind
