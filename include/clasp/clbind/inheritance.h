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

#ifndef CLBIND_INHERITANCE_090217_HPP
#define CLBIND_INHERITANCE_090217_HPP

#include <cassert>
#include <limits>
#include <map>
#include <memory>
#include <vector>
#include <clasp/clbind/typeid.h>
#include <clasp/clbind/class_rep.h>
#include <boost/scoped_ptr.hpp>

namespace clbind {
namespace detail {

typedef void *(*cast_function)(void *);

class cast_graph {
public:
  cast_graph();
  ~cast_graph();

  // `src` and `p` here describe the *most derived* object. This means that
  // for a polymorphic type, the pointer must be cast with
  // dynamic_cast<void*> before being passed in here, and `src` has to
  // match typeid(*p).
  std::pair<void *, int> cast(
      void *p, class_id src, class_id target, class_id dynamic_id, void const *dynamic_ptr) const;
  void insert(class_id src, class_id target, cast_function cast);

private:
  class impl;
  boost::scoped_ptr<impl> m_impl;
};

// Maps a type_id to a class_id. Note that this actually partitions the
// id-space into two, using one half for "local" ids; ids that are used only as
// keys into the conversion cache. This is needed because we need a shared key
// even for types that hasn't been registered explicitly.
class class_id_map {
public:
  class_id_map();

  class_id get(type_id type) const;
  class_id get_local(type_id type);
  void put(class_id id, type_id type);

private:
  typedef std::map<type_id, class_id> map_type;
  map_type m_classes;
  class_id m_local_id;

  static class_id const local_id_base;
};

inline class_id_map::class_id_map()
    : m_local_id(local_id_base) {}

inline class_id class_id_map::get(type_id type) const {
  map_type::const_iterator i = m_classes.find(type);
  if (i == m_classes.end() || i->second >= local_id_base)
    return reg::unknown_class;
  return i->second;
}

inline class_id class_id_map::get_local(type_id type) {
  std::pair<map_type::iterator, bool> result = m_classes.insert(
      std::make_pair(type, 0));

  if (result.second)
    result.first->second = m_local_id++;

  assert(m_local_id >= local_id_base);

  return result.first->second;
}

inline void class_id_map::put(class_id id, type_id type) {
  assert(id < local_id_base);

  std::pair<map_type::iterator, bool> result = m_classes.insert(
      std::make_pair(type, 0));

  assert(
      result.second || result.first->second == id || result.first->second >= local_id_base);

  result.first->second = id;
}

inline ClassRep_sp class_map_get(class_id id)  {
  if (id >= _lisp->_Roots._ClassMap.size())
    return _Nil<ClassRep_O>();
  return _lisp->_Roots._ClassMap[id];
}

inline void class_map_put(class_id id, ClassRep_sp cls) {
  if (id >= _lisp->_Roots._ClassMap.size())
    _lisp->_Roots._ClassMap.resize(id + 1);
  _lisp->_Roots._ClassMap[id] = cls;
}

template <class S, class T>
struct static_cast_ {
  static void *execute(void *p) {
    return static_cast<T *>(static_cast<S *>(p));
  }
};

template <class S, class T>
struct dynamic_cast_ {
  static void *execute(void *p) {
    return dynamic_cast<T *>(static_cast<S *>(p));
  }
};


// Thread safe class_id allocation.
        class_id allocate_class_id(type_id const& cls);

        template <class T>
        struct registered_class
        {
            static class_id const id;
        };

        template <class T>
        class_id const registered_class<T>::id = allocate_class_id(typeid(T));

        template <class T>
        struct registered_class<T const>
            : registered_class<T>
        {};
}
} // namespace clbind::detail

#endif // CLBIND_INHERITANCE_090217_HPP
