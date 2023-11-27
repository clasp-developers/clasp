#pragma once

namespace clbind {
namespace detail {

typedef void *(*cast_function)(void *);

class edge {
public:
  edge(class_id target, cast_function cast) : target(target), cast(cast) {}
  edge(){};
  class_id target;
  cast_function cast;
};

inline bool operator<(edge const &x, edge const &y) { return x.target < y.target; }

class vertex {
public:
  vertex(class_id id) : id(id) {}

  class_id id;
  gctools::Vec0<edge> edges;
};

}; // namespace detail
}; // namespace clbind
