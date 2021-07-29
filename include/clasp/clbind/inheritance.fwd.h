#ifndef INHERITANCE_FWD_H
#define INHERITANCE_FWD_H

namespace clbind {
namespace detail {

typedef void *(*cast_function)(void *);

struct edge {
  edge(class_id target, cast_function cast)
      : target(target), cast(cast) {}
  edge() {};
  class_id target;
  cast_function cast;
};

inline bool operator<(edge const &x, edge const &y) {
  return x.target < y.target;
}

struct vertex {
  vertex(class_id id)
    : id(id) {}

  class_id id;
  gctools::Vec0<edge> edges;
};

};
};

#endif
