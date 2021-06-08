/*
    File: inheritance.cc
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

#define CLBIND_BUILDING

#include <limits>
#include <map>
#include <vector>
#include <queue>
#pragma clang diagnostic push
//#pragma clang diagnostic ignored "-Wunused-local-typedef"
#include <boost/dynamic_bitset.hpp>
#include <boost/foreach.hpp>
#include <boost/tuple/tuple.hpp>
#include <boost/tuple/tuple_comparison.hpp>
#pragma clang diagnostic pop
#include <clasp/core/foundation.h>
#include <clasp/clbind/inheritance.h>

namespace clbind {
namespace detail {

class_id const class_id_map::local_id_base =
    std::numeric_limits<class_id>::max() / 2;

namespace {


typedef std::pair<std::ptrdiff_t, int> cache_entry;

class cache {
public:
  static std::ptrdiff_t const unknown;
  static std::ptrdiff_t const invalid;

  cache_entry get(
      class_id src, class_id target, class_id dynamic_id, std::ptrdiff_t object_offset) const;

  void put(
      class_id src, class_id target, class_id dynamic_id, std::ptrdiff_t object_offset, std::size_t distance, std::ptrdiff_t offset);

  void invalidate();

private:
  typedef boost::tuple<
      class_id, class_id, class_id, std::ptrdiff_t> key_type;
  typedef std::map<key_type, cache_entry> map_type;
  map_type m_cache;
};

std::ptrdiff_t const cache::unknown =
    std::numeric_limits<std::ptrdiff_t>::max();
std::ptrdiff_t const cache::invalid = cache::unknown - 1;

cache_entry cache::get(
    class_id src, class_id target, class_id dynamic_id, std::ptrdiff_t object_offset) const {
  map_type::const_iterator i = m_cache.find(
      key_type(src, target, dynamic_id, object_offset));
  return i != m_cache.end() ? i->second : cache_entry(unknown, -1);
}

void cache::put(
    class_id src, class_id target, class_id dynamic_id, std::ptrdiff_t object_offset, std::size_t distance, std::ptrdiff_t offset) {
  m_cache.insert(std::make_pair(
      key_type(src, target, dynamic_id, object_offset), cache_entry(offset, distance)));
}

void cache::invalidate() {
  m_cache.clear();
}

} // namespace unnamed

class cast_graph::impl {
public:
  std::pair<void *, int> cast(
      void *p, class_id src, class_id target, class_id dynamic_id, void const *dynamic_ptr) const;
  void insert_impl(class_id src, class_id target, cast_function cast);
  void dump_impl();
private:
//  std::vector<vertex> m_vertices;
  mutable cache m_cache;
};

namespace {

struct queue_entry {
  queue_entry(void *p, class_id vertex_id, int distance)
      : p(p), vertex_id(vertex_id), distance(distance) {}

  void *p;
  class_id vertex_id;
  int distance;
};

} // namespace unnamed

DONT_OPTIMIZE_WHEN_DEBUG_RELEASE
std::pair<void *, int> cast_graph::impl::cast(
    void *const p, class_id src, class_id target, class_id dynamic_id, void const *dynamic_ptr) const {
#if 0
  printf("%s:%d:%s p=%p src=%lu target=%lu dynamic_id=%lu dynamic_ptr = %p\n",
         __FILE__, __LINE__, __FUNCTION__, p, src, target, dynamic_id, dynamic_ptr);
#endif
  if (src == target)
    return std::make_pair(p, 0);

  if (src >= _lisp->_Roots._CastGraph.size() || target >= _lisp->_Roots._CastGraph.size())
    return std::pair<void *, int>((void *)0, -1);

  std::ptrdiff_t const object_offset =
      (char const *)dynamic_ptr - (char const *)p;

  cache_entry cached = m_cache.get(src, target, dynamic_id, object_offset);

  if (cached.first != cache::unknown) {
    if (cached.first == cache::invalid)
      return std::pair<void *, int>((void *)0, -1);
    return std::make_pair((char *)p + cached.first, cached.second);
  }

  std::queue<queue_entry> q;
  q.push(queue_entry(p, src, 0));

  boost::dynamic_bitset<> visited(_lisp->_Roots._CastGraph.size());

  while (!q.empty()) {
    queue_entry const qe = q.front();
    q.pop();

    visited[qe.vertex_id] = true;
    vertex const &v = _lisp->_Roots._CastGraph[qe.vertex_id];

    if (v.id == target) {
      m_cache.put(
          src, target, dynamic_id, object_offset, qe.distance, (char *)qe.p - (char *)p);

      return std::make_pair(qe.p, qe.distance);
    }

    for ( edge const &e : v.edges) {
      if (visited[e.target])
        continue;
      if (void *casted = e.cast(qe.p))
        q.push(queue_entry(casted, e.target, qe.distance + 1));
    }
  }
  m_cache.put(src, target, dynamic_id, object_offset, cache::invalid, -1);
  return std::pair<void *, int>((void *)0, -1);
}

void cast_graph::impl::insert_impl(
    class_id src, class_id target, cast_function cast) {
  printf("%s:%d:%s src=%lu target=%lu cast=%p\n", __FILE__, __LINE__, __FUNCTION__, src, target, (void*)cast);
  class_id const max_id = std::max(src, target);

  if (max_id >= _lisp->_Roots._CastGraph.size()) {
    _lisp->_Roots._CastGraph.reserve(max_id + 1);
    for (class_id i = _lisp->_Roots._CastGraph.size(); i < max_id + 1; ++i)
      _lisp->_Roots._CastGraph.push_back(vertex(i));
  }

  gctools::Vec0<edge> &edges = _lisp->_Roots._CastGraph[src].edges;

  auto ii = std::lower_bound(edges.begin(), edges.end(), edge(target, 0));
  if (ii == edges.end() || ii->target != target) {
    edges.insert(ii, edge(target, cast));
    m_cache.invalidate();
  }
}
void cast_graph::impl::dump_impl() {
  for ( class_id ii = 0; ii < _lisp->_Roots._CastGraph.size(); ++ ii ) {
    gctools::Vec0<edge>& edges = _lisp->_Roots._CastGraph[ii].edges;
    if (edges.size()>0) {
      printf("%s:%d:%s class_id: %lu has %lu edges\n", __FILE__, __LINE__, __FUNCTION__, ii, edges.size() );
    }
  }
}

std::pair<void *, int> cast_graph::cast(
    void *p, class_id src, class_id target, class_id dynamic_id, void const *dynamic_ptr) const {
#if 0
  printf("%s:%d:%s p=%p src=%lu target=%lu dynamic_id=%lu dynamic_ptr = %p\n",
         __FILE__, __LINE__, __FUNCTION__, p, src, target, dynamic_id, dynamic_ptr);
#endif
  return m_impl->cast(p, src, target, dynamic_id, dynamic_ptr);
}

void cast_graph::insert(class_id src, class_id target, cast_function cast) {
  printf("%s:%d:%s src=%lu target=%lu cast=%p\n", __FILE__, __LINE__, __FUNCTION__, src, target, (void*)cast);
  m_impl->insert_impl(src, target, cast);
}

void cast_graph::dump() {
  m_impl->dump_impl();
}

cast_graph::cast_graph()
    : m_impl(new impl) {
  printf("%s:%d:%s\n", __FILE__, __LINE__, __FUNCTION__ );
}

cast_graph::~cast_graph() {
  printf("%s:%d:%s\n", __FILE__, __LINE__, __FUNCTION__ );
}
}
} // namespace clbind::detail
