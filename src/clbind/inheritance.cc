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
// #pragma clang diagnostic ignored "-Wunused-local-typedef"
#include <boost/dynamic_bitset.hpp>
#include <boost/foreach.hpp>
#include <boost/tuple/tuple.hpp>
#include <boost/tuple/tuple_comparison.hpp>
#pragma clang diagnostic pop
#include <clasp/core/foundation.h>
#include <clasp/clbind/inheritance.h>

namespace clbind {
namespace detail {

class_id const class_id_map::local_id_base = std::numeric_limits<class_id>::max() / 2;

namespace {

typedef std::pair<std::ptrdiff_t, int> cache_entry;

class cache {
public:
  static std::ptrdiff_t const unknown;
  static std::ptrdiff_t const invalid;

  cache_entry get(class_id src, class_id target, class_id dynamic_id, std::ptrdiff_t object_offset) const;

  void put_entry(class_id src, class_id target, class_id dynamic_id, std::ptrdiff_t object_offset, std::ptrdiff_t offset,
                 int distance);

  void invalidate();

private:
  typedef boost::tuple<class_id, class_id, class_id, std::ptrdiff_t> key_type;
  typedef std::map<key_type, cache_entry> map_type;
  map_type m_cache;
};

std::ptrdiff_t const cache::unknown = std::numeric_limits<std::ptrdiff_t>::max();
std::ptrdiff_t const cache::invalid = cache::unknown - 1;

cache_entry cache::get(class_id src, class_id target, class_id dynamic_id, std::ptrdiff_t object_offset) const {
  map_type::const_iterator i = m_cache.find(key_type(src, target, dynamic_id, object_offset));
  if (i != m_cache.end()) {
    //    printf("%s:%d:%s Returning cache key_type(%lu,%lu,%lu,%ld) -> cache_entry(%ld,%d)\n",
    //           __FILE__, __LINE__, __FUNCTION__, src, target, dynamic_id, object_offset, i->second.first, i->second.second );
    return i->second;
  } else {
    //    printf("%s:%d:%s Returning cache key_type(%lu,%lu,%lu,%ld) -> cache_entry(unknown,-1)\n",
    //           __FILE__, __LINE__, __FUNCTION__, src, target, dynamic_id, object_offset );
    return cache_entry(unknown, -1);
  }
}

void cache::put_entry(class_id src, class_id target, class_id dynamic_id, std::ptrdiff_t object_offset, std::ptrdiff_t offset,
                      int distance) {
#if 0
  printf("%s:%d:%s Adding to cache key_type(%lu,%lu,%lu,%ld) -> cache_entry(%ld,%d)\n",
         __FILE__, __LINE__, __FUNCTION__, src, target, dynamic_id, object_offset, offset, distance );
#endif
  m_cache.insert(std::make_pair(key_type(src, target, dynamic_id, object_offset), cache_entry(offset, distance)));
}

void cache::invalidate() { m_cache.clear(); }

} // namespace

class cast_graph::impl {
public:
  std::pair<void*, int> cast(void* p, class_id src, class_id target, class_id dynamic_id, void const* dynamic_ptr) const;
  void insert_impl(class_id src, class_id target, cast_function cast);
  void dump_impl(FILE* fout);

private:
  //  std::vector<vertex> m_vertices;
  mutable cache m_cache;
};

namespace {

struct queue_entry {
  queue_entry(void* p, class_id vertex_id, int distance) : p(p), vertex_id(vertex_id), distance(distance) {}

  void* p;
  class_id vertex_id;
  int distance;
};

} // namespace

// #define DEBUG_CAST_GRAPH 1

std::pair<void*, int> cast_graph::impl::cast(void* const p, class_id src, class_id target, class_id dynamic_id,
                                             void const* dynamic_ptr) const {
#ifdef DEBUG_CAST_GRAPH
  if (src != target) {
    // only print when non-trivial cases
    //    printf("%s:%d:%s p=%p src=%lu target=%lu dynamic_id=%lu dynamic_ptr = %p\n",
    //           __FILE__, __LINE__, __FUNCTION__, p, src, target, dynamic_id, dynamic_ptr);
  }
#endif
  if (src == target) {
    return std::make_pair(p, 0);
  }

  if (src >= _lisp->_Roots._CastGraph.size() || target >= _lisp->_Roots._CastGraph.size()) {
#ifdef DEBUG_CAST_GRAPH
    printf("%s:%d:%s returning B pair(%p, %d)\n", __FILE__, __LINE__, __FUNCTION__, (void*)0, -1);
#endif
    return std::pair<void*, int>((void*)0, -1);
  }
  std::ptrdiff_t const object_offset = (char const*)dynamic_ptr - (char const*)p;

  cache_entry cached = m_cache.get(src, target, dynamic_id, object_offset);

  if (cached.first != cache::unknown) {
    if (cached.first == cache::invalid) {
#ifdef DEBUG_CAST_GRAPH
      printf("%s:%d:%s returning C pair(%p, %d)\n", __FILE__, __LINE__, __FUNCTION__, (void*)0, -1);
#endif
      return std::pair<void*, int>((void*)0, -1);
    }
#ifdef DEBUG_CAST_GRAPH
    char* ptr = (char*)p + cached.first;
    printf("%s:%d:%s returning cached D pair(%p, %d)\n", __FILE__, __LINE__, __FUNCTION__, ptr, cached.second);
    if (((uintptr_t)ptr) & 0x7) {
      printf("%s:%d:%s THERE IS A PROBLEM - RETURNED PTR %p IS NOT QWORD ALIGNED p -> %p  cached.first -> %lu cache::unknown -> "
             "%lu class_id_map::local_id_base -> %lu\n",
             __FILE__, __LINE__, __FUNCTION__, ptr, (char*)p, cached.first, cache::unknown, class_id_map::local_id_base);
    }
#endif
    return std::make_pair((char*)p + cached.first, cached.second);
  }

  std::queue<queue_entry> q;
  q.push(queue_entry(p, src, 0));

  boost::dynamic_bitset<> visited(_lisp->_Roots._CastGraph.size());

  while (!q.empty()) {
    queue_entry const qe = q.front();
    q.pop();

    visited[qe.vertex_id] = true;
    vertex const& v = _lisp->_Roots._CastGraph[qe.vertex_id];

    if (v.id == target) {
      m_cache.put_entry(src, target, dynamic_id, object_offset, (char*)qe.p - (char*)p, qe.distance);
#ifdef DEBUG_CAST_GRAPH
      printf("%s:%d:%s returning E pair(%p, %d)\n", __FILE__, __LINE__, __FUNCTION__, qe.p, qe.distance);
#endif
      return std::make_pair(qe.p, qe.distance);
    }

    for (edge const& e : v.edges) {
      if (visited[e.target])
        continue;
      if (void* casted = e.cast(qe.p))
        q.push(queue_entry(casted, e.target, qe.distance + 1));
    }
  }
  m_cache.put_entry(src, target, dynamic_id, object_offset, cache::invalid, -1);
#ifdef DEBUG_CAST_GRAPH
  printf("%s:%d:%s returning F pair(%p, %d)\n", __FILE__, __LINE__, __FUNCTION__, (void*)0, -1);
#endif
  return std::pair<void*, int>((void*)0, -1);
}

void cast_graph::impl::insert_impl(class_id src, class_id target, cast_function cast) {
  //  printf("%s:%d:%s src=%lu target=%lu cast=%p\n", __FILE__, __LINE__, __FUNCTION__, src, target, (void*)cast);
  class_id const max_id = std::max(src, target);

  if (max_id >= _lisp->_Roots._CastGraph.size()) {
    _lisp->_Roots._CastGraph.reserve(max_id + 1);
    for (class_id i = _lisp->_Roots._CastGraph.size(); i < max_id + 1; ++i)
      _lisp->_Roots._CastGraph.push_back(vertex(i));
  }

  gctools::Vec0<edge>& edges = _lisp->_Roots._CastGraph[src].edges;

  auto ii = std::lower_bound(edges.begin(), edges.end(), edge(target, 0));
  if (ii == edges.end() || ii->target != target) {
    edges.insert(ii, edge(target, cast));
    m_cache.invalidate();
  }
}
void cast_graph::impl::dump_impl(FILE* fout) {
  for (class_id ii = 0; ii < _lisp->_Roots._CastGraph.size(); ++ii) {
    gctools::Vec0<edge>& edges = _lisp->_Roots._CastGraph[ii].edges;
    if (edges.size() > 0) {
      core::Symbol_sp sym = reg::lisp_classSymbolFromClassId(ii);
      printf("%s:%d:%s class_id: %lu/%s has %lu edges\n", __FILE__, __LINE__, __FUNCTION__, ii, _rep_(sym).c_str(), edges.size());
      if (fout) {
        for (auto edge : edges) {
          if (ii < edge.target) {
            core::Symbol_sp targetSym = reg::lisp_classSymbolFromClassId(edge.target);
            fprintf(fout, "\"%s_%lu\" -> \"%s_%lu\"\n", _rep_(sym).c_str(), ii, _rep_(targetSym).c_str(), edge.target);
          }
        }
      }
    }
  }
}

std::pair<void*, int> cast_graph::cast(void* p, class_id src, class_id target, class_id dynamic_id, void const* dynamic_ptr) const {
  return m_impl->cast(p, src, target, dynamic_id, dynamic_ptr);
}

void cast_graph::insert(class_id src, class_id target, cast_function cast) {
  //  printf("%s:%d:%s src=%lu target=%lu cast=%p\n", __FILE__, __LINE__, __FUNCTION__, src, target, (void*)cast);
  m_impl->insert_impl(src, target, cast);
}

void cast_graph::dump(FILE* fout) { m_impl->dump_impl(fout); }

cast_graph::cast_graph() : m_impl(new impl) {
  //  printf("%s:%d:%s\n", __FILE__, __LINE__, __FUNCTION__ );
}

cast_graph::~cast_graph() {
  //  printf("%s:%d:%s\n", __FILE__, __LINE__, __FUNCTION__ );
}
} // namespace detail
} // namespace clbind

namespace clbind {

DOCGROUP(clasp);
CL_DEFUN void clbind__dump_class_id_map() {
  printf("%s:%d:%s\n", __FILE__, __LINE__, __FUNCTION__);
  printf("local_id_base = %lu\n", globalClassIdMap->local_id_base);
  printf("m_local_id = %lu\n", globalClassIdMap->m_local_id);
  printf("Dump of m_classes\n");
  for (auto entry : globalClassIdMap->m_type_id_to_class_id) {
    printf(" type_id @%p (%s) -> class_id(%lu)\n", (void*)entry.first.get_type_info(), entry.first.name(), entry.second);
  }
  printf("------\n");
};

CL_LAMBDA(&optional filename);
DOCGROUP(clasp);
CL_DEFUN void clbind__dump_cast_graph(core::T_sp filename) {
  printf("%s:%d:%s\n", __FILE__, __LINE__, __FUNCTION__);
  printf("Dump of castGraph\n");
  if (gc::IsA<core::String_sp>(filename)) {
    std::string fn = gc::As_unsafe<core::String_sp>(filename)->get_std_string();
    FILE* fout = fopen(fn.c_str(), "w");
    fprintf(fout, "digraph {\n");
    globalCastGraph->dump(fout);
    fprintf(fout, "}\n");
    fclose(fout);
  } else {
    globalCastGraph->dump(NULL);
  }
  printf("------\n");
};

}; // namespace clbind
