#pragma once

/*
    File: inheritance.h
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
// Copyright Daniel Wallin 2009. Use, modification and distribution is
// subject to the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include <cassert>
#include <limits>
#include <map>
#include <memory>
#include <vector>
#include <typeindex>
#include <clasp/clbind/class_rep.h>
#include <clasp/clbind/inheritance.fwd.h>

namespace clbind {
namespace detail {

class cast_graph {
public:
  cast_graph();

  /*!
     src and p here describe the *most derived* object. This means that
     for a polymorphic type, the pointer must be cast with
     dynamic_cast<void*> before being passed in here, and src has to
     match typeid(*p).
  */
  std::pair<void*, int> cast(void* p, class_id src, class_id target, class_id dynamic_id, void const* dynamic_ptr) const;
  void insert(class_id src, class_id target, cast_function cast);
  void dump(FILE* fout);

private:
  // Indirection to hide the details of impl. Pointless otherwise. FIXME?
  class impl;
  std::unique_ptr<impl> m_impl;
};

inline ClassRep_sp class_map_get(class_id id) {
  //  printf("%s:%d:%s\n", __FILE__, __LINE__, __FUNCTION__);
  if (id >= _lisp->_Roots._ClassMap.size())
    return nil<ClassRep_O>();
  return _lisp->_Roots._ClassMap[id];
}

inline void class_map_put(class_id id, ClassRep_sp cls) {
  //  printf("%s:%d:%s\n", __FILE__, __LINE__, __FUNCTION__);
  if (id >= _lisp->_Roots._ClassMap.size())
    _lisp->_Roots._ClassMap.resize(id + 1);
  _lisp->_Roots._ClassMap[id] = cls;
}

template <class S, class T> struct static_cast_ {
  static void* execute(void* p) { return static_cast<T*>(static_cast<S*>(p)); }
};

template <class S, class T> struct dynamic_cast_ {
  static void* execute(void* p) { return dynamic_cast<T*>(static_cast<S*>(p)); }
};

// Thread safe class_id allocation (in core/foundation.cc).
class_id allocate_class_id(const std::type_info& cls);

template <class T> struct registered_class {
  static class_id const id;
};

template <class T> class_id const registered_class<T>::id = allocate_class_id(typeid(T));

template <class T> struct registered_class<T const> : registered_class<T> {};
} // namespace detail
} // namespace clbind
